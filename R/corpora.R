# corpora.R -- access to the many-language corpus registry. The R package reads
# the bundled, pre-derived lexica for the demonstrations and can download
# CSV-format corpora (Connector A) on demand into a user cache. The wordfreq
# connector (Connector B, ~40 languages) is provided by the Python package; its
# pre-derived outputs are read here as ordinary lexica.

#' Per-user cache directory for fetched corpora
#'
#' The directory `tools::R_user_dir("lexsync", "cache")` names for this package.
#' It is where [fetch_corpus()] puts a download unless told otherwise, and it is
#' the only place the package writes to without being handed a path. This
#' function only reports the path. The directory is created by the first
#' download into it, so until then it does not exist.
#'
#' The cache persists between sessions. A registered corpus is a delimited word
#' list, and a download is refused above 200 MB, so a cache holding several
#' large corpora can reach a few hundred megabytes. Each download into the cache
#' first deletes any partial download that an interrupted transfer left there
#' more than a day earlier, and fetching a corpus again replaces the earlier
#' copy. Downloaded corpora otherwise stay until [lexsync_cache_clear()] removes
#' them, one corpus at a time or all together. Nothing kept there is
#' irreplaceable, so the next call downloads afresh.
#'
#' @return The cache directory path, which need not exist yet.
#' @seealso [lexsync_cache_clear()] to empty the cache.
#' @examples
#' lexsync_cache_dir()
#' @importFrom tools R_user_dir
#' @export
lexsync_cache_dir <- function() {
  tools::R_user_dir("lexsync", "cache")
}

#' Remove fetched corpora from the cache
#'
#' Deletes what [fetch_corpus()] put in [lexsync_cache_dir()]. Given a `name`,
#' that is the corpus's `<name>.csv` and any `<name>.csv.part` that an
#' interrupted download left behind. With `name` left `NULL` it is the whole
#' cache directory. A corpus needed again is downloaded afresh by the next
#' [fetch_corpus()] call. A file that [fetch_corpus()] wrote to a `dest` of the
#' caller's choosing is not in the cache, and is never touched.
#'
#' @param name A corpus name as given to [fetch_corpus()], or `NULL` (the
#'   default) to remove the whole cache.
#' @return The paths of the files removed, invisibly. The vector is empty when
#'   there was nothing to remove.
#' @seealso [lexsync_cache_dir()] for where the cache lives.
#' @examples
#' # A throwaway cache stands in for yours, so running this leaves your own alone.
#' local({
#'   tmp <- tempfile("lexsync-")
#'   old <- Sys.getenv("R_USER_CACHE_DIR", unset = NA)
#'   on.exit({
#'     if (is.na(old)) Sys.unsetenv("R_USER_CACHE_DIR") else
#'       Sys.setenv(R_USER_CACHE_DIR = old)
#'     unlink(tmp, recursive = TRUE)
#'   })
#'   Sys.setenv(R_USER_CACHE_DIR = tmp)
#'   # Two corpora and an interrupted download
#'   dir.create(lexsync_cache_dir(), recursive = TRUE)
#'   invisible(file.create(file.path(lexsync_cache_dir(),
#'     c("subtlex_nl.csv", "subtlex_nl.csv.part", "lexique_fr.csv"))))
#'   # One corpus, with its partial download
#'   print(basename(lexsync_cache_clear("subtlex_nl")))
#'   # Everything else, and the directory itself
#'   print(basename(lexsync_cache_clear()))
#'   dir.exists(lexsync_cache_dir())
#' })
#' @export
lexsync_cache_clear <- function(name = NULL) {
  # Mirrors cache_clear() in python_workflow/src/lexsync/corpora.py, which also
  # removes the <name>_wordfreq.csv that only that engine builds. The tilde is
  # expanded here because unlink() below expands nothing, and R_user_dir() keeps a
  # leading '~' from R_USER_CACHE_DIR as it is.
  dir <- path.expand(lexsync_cache_dir())
  if (is.null(name)) {
    removed <- list.files(dir, recursive = TRUE, full.names = TRUE, all.files = TRUE)
    targets <- dir
  } else {
    # The name becomes a file name inside the cache, so a path separator, or the
    # colon of a Windows drive or stream name, would let it reach a file other than
    # the corpus's own. The Python twin refuses the same three characters.
    if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name) ||
        grepl("[/\\\\:]", name)) {
      stop("lexsync: 'name' must be a single corpus name, not a path.", call. = FALSE)
    }
    targets <- file.path(dir, paste0(name, c(".csv", ".csv.part")))
    # Files only, as the twin's os.path.isfile() has it.
    targets <- targets[file.exists(targets) & !dir.exists(targets)]
    removed <- targets
  }
  # With expand = FALSE a name or cache path holding *, ? or [ ] names itself and
  # nothing else; with the default, "*" would empty the cache. unlink() reports a
  # file it could not delete (one held open on Windows, say) only through its
  # return code, so what survives is checked for directly.
  unlink(targets, recursive = is.null(name), expand = FALSE)
  left <- targets[file.exists(targets)]
  if (length(left)) {
    stop(sprintf("lexsync: could not remove %s from the cache; check that no other process is using it.",
                 paste(left, collapse = ", ")), call. = FALSE)
  }
  invisible(removed)
}

# Readies the cache for a download into it. Mirrors _prepare_cache() in
# python_workflow/src/lexsync/corpora.py. A '.part' sidecar outlives its fetch
# only when the transfer was cut short in a way fetch_corpus()'s handlers never
# see: an interrupt reaches neither the error nor the warning branch, and a
# process that dies runs no handler at all. A sidecar can also belong to a
# download still running in another session, which writes to it as it goes and
# gives up on a stalled server after options(timeout). A day without a write
# therefore marks a sidecar as abandoned, and only those are deleted. A younger
# one is left to its download, and a retry of the same corpus truncates its own.
.prepare_cache <- function() {
  dir <- path.expand(lexsync_cache_dir())
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  parts <- list.files(dir, pattern = "\\.part$", full.names = TRUE, all.files = TRUE)
  stale <- difftime(Sys.time(), file.mtime(parts), units = "secs") > .stale_part_seconds()
  unlink(parts[!is.na(stale) & stale], expand = FALSE)
  dir
}

# How long a sidecar may go unwritten before the sweep treats it as abandoned.
# Mirrors _STALE_PART_SECONDS in python_workflow/src/lexsync/corpora.py.
.stale_part_seconds <- function() 24 * 60 * 60

#' Locate the corpus registry
#' @keywords internal
default_registry_path <- function() {
  cand <- c(
    getOption("lexsync.registry", ""),
    file.path("corpora", "registry.yaml"),
    file.path("..", "corpora", "registry.yaml"),
    system.file("extdata", "registry.yaml", package = "lexsync")
  )
  cand <- cand[nzchar(cand) & file.exists(cand)]
  if (!length(cand)) {
    stop("lexsync: could not locate 'registry.yaml'; set options(lexsync.registry = '...').",
         call. = FALSE)
  }
  cand[1]
}

#' List the corpora known to the registry
#'
#' @param registry_path Optional path to `registry.yaml`.
#' @return A data frame describing each registered corpus.
#' @importFrom yaml read_yaml
#' @export
list_corpora <- function(registry_path = NULL) {
  reg <- yaml::read_yaml(registry_path %||% default_registry_path())
  corp <- reg$corpora
  data.frame(
    name = names(corp),
    language = vapply(corp, function(x) x$language$name %||% NA_character_, character(1)),
    iso = vapply(corp, function(x) x$language$iso %||% NA_character_, character(1)),
    status = vapply(corp, function(x) x$status %||% NA_character_, character(1)),
    connector = vapply(corp, function(x) x$connector %||% "openlexicon", character(1)),
    citation = vapply(corp, function(x) x$citation %||% NA_character_, character(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

# Does the file open with '<', i.e. an HTML or XML document? Mirrors
# _starts_with_markup() in python_workflow/src/lexsync/corpora.py. A delimited
# file opens on data; a login wall, a redirect stub or a 404 page served with a
# 200 status opens on a tag, and would otherwise be cached as <name>.csv and
# resurface much later as an unintelligible schema error.
.starts_with_markup <- function(path) {
  bytes <- readBin(path, "raw", n = 512L)
  bom <- as.raw(c(0xEF, 0xBB, 0xBF))
  if (length(bytes) >= 3L && identical(bytes[1:3], bom)) bytes <- bytes[-(1:3)]
  ws <- as.raw(c(0x20, 0x09, 0x0A, 0x0D, 0x0B, 0x0C))
  first <- which(!(bytes %in% ws))
  length(first) > 0L && identical(bytes[first[1]], as.raw(0x3C))
}

# Hard cap on a corpus download, in bytes. Mirrors _max_download_bytes() in
# python_workflow/src/lexsync/corpora.py. A function rather than a bare constant
# so the twin tests can lower it without writing 200 MB to disk; the message
# below names the real limit either way.
.max_download_bytes <- function() 200 * 1024^2

.stop_download <- function(name, url, detail) {
  stop(sprintf(paste0("lexsync: could not download corpus '%s' from %s (%s). Check the URL ",
                      "in registry.yaml, or download the file manually and pass it to ",
                      "load_lexicon()."),
               name, url, detail), call. = FALSE)
}

#' Download a CSV-format registered corpus into the cache
#'
#' Suitable for Connector A corpora that expose a delimited file. The URL's
#' scheme is checked first; the transfer then lands in a sidecar file that is
#' renamed into place only after the size cap, the markup sniff and any
#' `sha256` the registry entry carries have all passed. The download is
#' recorded so it can be cited; consult [list_corpora()] for the citation.
#'
#' The file lands in [lexsync_cache_dir()] unless `dest` names somewhere else.
#' The cache directory is created only once the registry entry and its URL have
#' been accepted, so a refused call writes nothing. A download to the default
#' destination begins by deleting any `.part` sidecar in the cache that an
#' interrupted transfer left there more than a day earlier. The cache persists
#' between sessions, and one corpus may reach the 200 MB download cap, so several
#' of them add up. [lexsync_cache_clear()] removes one corpus or the whole cache,
#' and the next call downloads the corpus again. A `dest` the caller names, inside
#' the cache or not, is used as given. Its directory must already exist, no
#' sidecar is swept, and the only file beside it that lexsync writes or deletes is
#' the `<dest>.part` sidecar of the download itself.
#'
#' @param name A corpus name present in the registry.
#' @param registry_path Optional path to `registry.yaml`.
#' @param dest Optional destination file; defaults to `<name>.csv` in
#'   [lexsync_cache_dir()].
#' @return The path to the downloaded file, invisibly.
#' @importFrom yaml read_yaml
#' @importFrom utils download.file
#' @export
fetch_corpus <- function(name, registry_path = NULL, dest = NULL) {
  reg <- yaml::read_yaml(registry_path %||% default_registry_path())
  entry <- reg$corpora[[name]]
  if (is.null(entry)) {
    stop(sprintf("lexsync: corpus '%s' is not in the registry.", name), call. = FALSE)
  }
  # Only 'openlexicon' names a delimited file; 'url' is the landing page, and
  # downloading that would silently cache an HTML document as <name>.csv.
  url <- entry$openlexicon
  if (is.null(url)) {
    stop(sprintf(paste0("lexsync: corpus '%s' registers only a landing page (%s); lexsync ",
                        "cannot download it automatically. Retrieve the delimited file ",
                        "manually and pass it to load_lexicon()."),
                 name, entry$url %||% "see corpora/registry.yaml"), call. = FALSE)
  }
  # A registry is editable, and fetch_corpus() writes wherever it points, so a
  # 'file://' or 'ftp://' entry would read a local path under the guise of a
  # download. Only the two schemes a corpus is published over are honoured.
  if (!grepl("^https?://", url, ignore.case = TRUE)) {
    stop(sprintf("lexsync: corpus '%s' registers a non-http(s) URL (%s); refusing to download.",
                 name, url), call. = FALSE)
  }
  # Only now, with every refusal behind it, does the call touch the disk, and only
  # the cache is swept: a caller's `dest` may sit beside files lexsync never wrote.
  if (is.null(dest)) dest <- file.path(.prepare_cache(), paste0(name, ".csv"))
  # The transfer lands in a sidecar and is renamed over `dest` only after every
  # check below has passed, so a truncated or unverified body can never sit at
  # the cache path, where a later run would trust it.
  part <- paste0(dest, ".part")
  # download.file() reports a failed transfer as a warning under some methods and
  # as an error under others, so both are branded. Called through the import
  # rather than utils:: so the tests can substitute an offline transport; it
  # honours options(timeout), where the Python engine passes its own.
  tryCatch(
    download.file(url, part, mode = "wb", quiet = TRUE),
    error = function(e) {
      unlink(part)
      .stop_download(name, url, conditionMessage(e))
    },
    warning = function(w) {
      unlink(part)
      .stop_download(name, url, conditionMessage(w))
    }
  )
  # download.file() cannot stop a transfer mid-stream as the Python engine's
  # chunked reader does, so the cap is enforced on the landed sidecar instead.
  if (file.size(part) > .max_download_bytes()) {
    unlink(part)
    stop(paste0("lexsync: corpus download exceeded the 200 MB size limit. Retrieve the ",
                "delimited file manually and pass it to load_lexicon()."),
         call. = FALSE)
  }
  if (.starts_with_markup(part)) {
    unlink(part)
    stop(sprintf(paste0("lexsync: corpus '%s' returned an HTML page, not a delimited file ",
                        "(%s); the registry URL may have rotted. Retrieve the delimited file ",
                        "manually and pass it to load_lexicon()."),
                 name, url), call. = FALSE)
  }
  # 'sha256' is optional per registry entry; when present the download must
  # match it before it may enter the cache.
  if (!is.null(entry$sha256) && !identical(sha256_file(part), entry$sha256)) {
    unlink(part)
    stop(sprintf(paste0("lexsync: checksum mismatch for corpus '%s'; the download does not ",
                        "match the registry's sha256. Retry the download, or verify the ",
                        "sha256 recorded in registry.yaml."),
                 name), call. = FALSE)
  }
  # file.rename() will not overwrite an existing file on Windows.
  if (file.exists(dest)) unlink(dest)
  file.rename(part, dest)
  message(sprintf("lexsync: downloaded '%s'. Please cite: %s", name, entry$citation %||% "(see registry)"))
  invisible(dest)
}
