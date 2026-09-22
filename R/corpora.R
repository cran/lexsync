# corpora.R -- access to the many-language corpus registry. The R package reads
# the bundled, pre-derived lexica for the demonstrations and can download
# CSV-format corpora (Connector A) on demand into a user cache. The wordfreq
# connector (Connector B, ~40 languages) is provided by the Python package; its
# pre-derived outputs are read here as ordinary lexica.

#' Per-user cache directory for fetched corpora
#'
#' The directory `tools::R_user_dir("lexsync", "cache")` names for this package,
#' created on first use. It is where [fetch_corpus()] puts a download unless told
#' otherwise, and it is the only place the package writes to without being handed
#' a path.
#'
#' The cache persists between sessions and lexsync never prunes it. A registered
#' corpus is a delimited word list, and a download is refused above 200 MB, so a
#' cache holding several large corpora can reach a few hundred megabytes. It holds
#' nothing that cannot be fetched again, so it may be deleted at any time, whole or
#' file by file, and the next call downloads afresh.
#'
#' @return A writable cache directory path (created if absent).
#' @importFrom tools R_user_dir
#' @export
lexsync_cache_dir <- function() {
  d <- tools::R_user_dir("lexsync", "cache")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

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
#' That cache persists between sessions and the package never prunes it; one
#' corpus may reach the 200 MB download cap, so several of them add up. Nothing
#' kept there is irreplaceable, so the directory may be deleted at any time and
#' the next call downloads the corpus again.
#'
#' @param name A corpus name present in the registry.
#' @param registry_path Optional path to `registry.yaml`.
#' @param dest Optional destination path; defaults to the cache.
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
  dest <- dest %||% file.path(lexsync_cache_dir(), paste0(name, ".csv"))
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
