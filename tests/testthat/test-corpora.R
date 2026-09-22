registry_path <- function() system.file("extdata", "registry.yaml", package = "lexsync")

# The vocabulary registry.yaml's header defines and list_corpora() surfaces.
STATUSES <- c("validated", "supported", "manual", "listed")

# A one-entry registry pointing at `url`, so no test touches the network.
temp_registry <- function(url, sha256 = NULL) {
  path <- tempfile(fileext = ".yaml")
  entry <- list(
    language = list(name = "Test", iso = "xx"), connector = "openlexicon",
    status = "supported", openlexicon = url, citation = "Test (2026)."
  )
  if (!is.null(sha256)) entry$sha256 <- sha256
  yaml::write_yaml(list(corpora = list(fake = entry)), path)
  path
}

# Serves `bytes` in place of the network, through the download.file import seam.
mock_download <- function(bytes) {
  function(url, destfile, ...) {
    writeBin(bytes, destfile)
    0L
  }
}

# A directory the test owns outright, so leftovers are provable by listing it.
temp_cache <- function() {
  dir <- tempfile()
  dir.create(dir)
  dir
}

CSV_BODY <- charToRaw("word,freq_zipf\ndog,4.5\n")

write_head <- function(bytes) {
  path <- tempfile()
  writeBin(bytes, path)
  path
}

BOM <- as.raw(c(0xEF, 0xBB, 0xBF))

# Pins the same contract as test_fetch_corpus_refuses_landing_page_only_entry in
# the Python engine's test_corpora.py: 'url' is the human-facing page and
# 'openlexicon' the delimited file, so falling back to 'url' would cache an HTML
# document as <name>.csv and fail later as a confusing schema error.
test_that("fetch_corpus refuses an entry that registers only a landing page", {
  expect_error(fetch_corpus("subtlex_esp", registry_path = registry_path()),
               "landing page")
})

test_that("fetch_corpus rejects a corpus absent from the registry", {
  expect_error(fetch_corpus("subtlex_klingon", registry_path = registry_path()),
               "not in the registry")
})

# The registry's own header defines 'validated' as a bundled example slice
# demonstrated end to end. Every bundled lexicon is wordfreq-derived, so no
# SUBTLEX entry may claim it; list_corpora() shows 'status' to users.
test_that("registry status reflects what is actually shipped", {
  reg <- yaml::read_yaml(registry_path())
  expect_identical(reg$corpora$subtlex_uk$status, "manual")
  expect_identical(reg$corpora$subtlex_esp$status, "listed")
  expect_false(any(vapply(reg$corpora, function(x) identical(x$status, "validated"), logical(1))))
  expect_false(any(vapply(reg$corpora, function(x) "bundled" %in% names(x), logical(1))))
})

test_that("registry statuses come from the documented vocabulary", {
  reg <- yaml::read_yaml(registry_path())
  for (name in names(reg$corpora)) {
    expect_true(reg$corpora[[name]]$status %in% STATUSES, info = name)
  }
})

# Pins the same contract as
# test_subtlex_uk_advertises_a_landing_page_not_a_dead_download in the Python
# engine's test_corpora.py. SUBTLEX-UK's openlexicon path 404s, and openlexicon
# has never hosted the corpus; van Heuven's own distribution publishes it only as
# zip archives, which the delimited-file connector cannot ingest. The entry must
# therefore send a human to the landing page rather than advertise a dead download.
test_that("subtlex_uk advertises a landing page, not a dead download", {
  reg <- yaml::read_yaml(registry_path())
  entry <- reg$corpora$subtlex_uk
  expect_null(entry$openlexicon)
  expect_match(entry$url, "^https://")
  expect_error(fetch_corpus("subtlex_uk", registry_path = registry_path()),
               "landing page")
})

# 'supported' means fetchable into the user cache, which fetch_corpus() can only
# honour through an 'openlexicon' key; conversely an entry carrying that key
# advertises a download, so it may not claim a status that denies one. Pinning the
# equivalence keeps a rotted URL from being demoted in status alone.
test_that("the openlexicon key and 'supported' status agree", {
  reg <- yaml::read_yaml(registry_path())
  for (name in names(reg$corpora)) {
    entry <- reg$corpora[[name]]
    has_file <- !is.null(entry$openlexicon) && nzchar(entry$openlexicon)
    expect_identical(has_file, identical(entry$status, "supported"), info = name)
  }
})

# Pins the same contract as test_fetch_corpus_refuses_a_non_http_url in the Python
# engine's test_corpora.py. A registry is editable and fetch_corpus() writes
# wherever it points, so 'file://' must not be read under the guise of a download.
test_that("fetch_corpus refuses a non-http(s) URL", {
  for (url in c("file:///etc/passwd", "ftp://example.invalid/x.csv", "corpora/local.csv")) {
    expect_error(fetch_corpus("fake", registry_path = temp_registry(url)),
                 "non-http", info = url)
  }
})

# Pins the same decision table as test_starts_with_markup_decision_table in the
# Python engine's test_corpora.py: identical bytes must give an identical verdict
# in both engines, so a rotted URL is caught on the same byte by each.
test_that("a body opening on a tag is recognised as markup", {
  expect_true(.starts_with_markup(write_head(charToRaw("<!DOCTYPE html>\n<html>404</html>"))))
  expect_true(.starts_with_markup(write_head(charToRaw("\n\r\t <html>"))))
  expect_true(.starts_with_markup(write_head(c(BOM, charToRaw("<html>")))))
})

test_that("a delimited body is not mistaken for markup", {
  expect_false(.starts_with_markup(write_head(charToRaw("word,freq_zipf\ndog,4.5\n"))))
  expect_false(.starts_with_markup(write_head(c(BOM, charToRaw("word,freq_zipf\n")))))
  expect_false(.starts_with_markup(write_head(raw(0))))
  expect_false(.starts_with_markup(write_head(charToRaw("   "))))
})

# A URL answering 200 with an HTML page (a login wall, or a 404 page served with
# the wrong status) must not be cached as <name>.csv, where it would resurface as
# an unintelligible schema error. Pins the same contract as
# test_fetch_corpus_rejects_an_html_body_and_caches_nothing in the Python
# engine's test_corpora.py: the sniff now runs on the sidecar, so not even a
# '.part' file may remain.
test_that("an HTML body is refused and leaves nothing behind", {
  dir <- temp_cache()
  local_mocked_bindings(
    download.file = mock_download(charToRaw("<!DOCTYPE html>\n<html><body>Not Found</body></html>\n"))
  )
  expect_error(
    fetch_corpus("fake", registry_path = temp_registry("https://example.invalid/rotted.csv"),
                 dest = file.path(dir, "fake.csv")),
    "HTML page, not a delimited file"
  )
  expect_identical(list.files(dir), character(0))
})

# Pins the same contract as test_fetch_corpus_removes_the_sidecar_when_the_transfer_dies
# in the Python engine's test_corpora.py: the pre-sidecar implementation cached
# exactly such truncated bodies, to resurface later as schema errors.
test_that("a transfer that dies mid-stream leaves nothing behind", {
  dir <- temp_cache()
  local_mocked_bindings(download.file = function(url, destfile, ...) {
    writeBin(charToRaw("word,freq_zipf\ndog,4"), destfile)
    warning("connection reset")
  })
  expect_error(
    fetch_corpus("fake", registry_path = temp_registry("https://example.invalid/flaky.csv"),
                 dest = file.path(dir, "fake.csv")),
    "could not download corpus 'fake'"
  )
  expect_identical(list.files(dir), character(0))
})

# Pins the same contract as test_fetch_corpus_promotes_a_verified_download in the
# Python engine's test_corpora.py: the transfer lands in '<dest>.part' and is
# renamed into place only after every check has passed.
test_that("fetch_corpus promotes a verified download and removes the sidecar", {
  dir <- temp_cache()
  dest <- file.path(dir, "fake.csv")
  local_mocked_bindings(download.file = mock_download(CSV_BODY))
  expect_message(
    out <- fetch_corpus("fake", registry_path = temp_registry("https://example.invalid/good.csv"),
                        dest = dest),
    "downloaded 'fake'"
  )
  expect_identical(out, dest)
  expect_identical(list.files(dir), "fake.csv")
  expect_identical(readBin(dest, "raw", n = file.size(dest)), CSV_BODY)
})

# Pins the same contract as test_fetch_corpus_aborts_an_oversized_download in the
# Python engine's test_corpora.py. The cap is lowered through its seam because a
# genuine 200 MB fixture has no place in a test suite; the message names the real
# limit regardless.
test_that("an oversized download is aborted and leaves nothing behind", {
  dir <- temp_cache()
  local_mocked_bindings(
    download.file = mock_download(charToRaw(strrep("x", 64))),
    .max_download_bytes = function() 16
  )
  expect_error(
    fetch_corpus("fake", registry_path = temp_registry("https://example.invalid/big.csv"),
                 dest = file.path(dir, "fake.csv")),
    "exceeded the 200 MB size limit"
  )
  expect_identical(list.files(dir), character(0))
})

# Pins the same contract as test_fetch_corpus_refuses_a_checksum_mismatch in the
# Python engine's test_corpora.py. The field is optional and no shipped registry
# entry carries one yet, so the contract is exercised through the temporary
# registry alone.
test_that("a checksum mismatch is refused and leaves nothing behind", {
  dir <- temp_cache()
  local_mocked_bindings(download.file = mock_download(CSV_BODY))
  expect_error(
    fetch_corpus("fake",
                 registry_path = temp_registry("https://example.invalid/good.csv",
                                               sha256 = strrep("0", 64)),
                 dest = file.path(dir, "fake.csv")),
    "checksum mismatch for corpus 'fake'"
  )
  expect_identical(list.files(dir), character(0))
})

# Pins the same contract as test_fetch_corpus_accepts_a_matching_checksum in the
# Python engine's test_corpora.py: both engines must derive the same digest from
# the same bytes.
test_that("a matching checksum is accepted", {
  dir <- temp_cache()
  dest <- file.path(dir, "fake.csv")
  local_mocked_bindings(download.file = mock_download(CSV_BODY))
  sha <- digest::digest(CSV_BODY, algo = "sha256", serialize = FALSE)
  expect_message(
    fetch_corpus("fake",
                 registry_path = temp_registry("https://example.invalid/good.csv", sha256 = sha),
                 dest = dest),
    "downloaded 'fake'"
  )
  expect_identical(list.files(dir), "fake.csv")
})

test_that("list_corpora surfaces the registry status", {
  frame <- list_corpora(registry_path())
  status <- stats::setNames(frame$status, frame$name)
  expect_identical(unname(status[["subtlex_uk"]]), "manual")
  expect_identical(unname(status[["subtlex_esp"]]), "listed")
  expect_false("validated" %in% status)
})
