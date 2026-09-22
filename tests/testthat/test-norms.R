# The design's `norms:` block: a semantic dimension joined before the pool is built.
#
# The join itself is merge_norms, tested in test-dimensions.R. What is tested here is
# the part that makes it usable and honest: the orchestrator applies the block before
# build_pool, so a norm column can be filtered on, matched on and spanned; and every
# table it joined is recorded in the materials datasheet with its checksum and its
# per-column coverage.
#
# That record is not decoration. A norm table can supply the very variable a design
# manipulates, so a run whose datasheet did not name the file would describe a
# selection over columns of unstated origin -- unreproducible from the record that
# exists to make it reproducible. Coverage is recorded for the same reason: a word the
# table does not cover gets an NA, and the tolerance windows then drop it from the pool
# without saying so.
#
# No norm data is bundled. These tests write their own tiny lexicon and norm table, so
# the feature is covered end to end without shipping a norm dataset.
#
# python_workflow/tests/test_norms.py asserts the same properties.

NORM_WORDS <- c("cat", "dog", "car", "cap", "bat", "bag", "cot", "cog", "rat", "rag",
                "hat", "hag", "pot", "peg", "man", "map")

norm_schema <- function() {
  yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
}

# A tiny lexicon. `upper` writes the forms in capitals, which load_lexicon folds.
norm_lexicon <- function(dir, upper = FALSE) {
  path <- file.path(dir, "lex.csv")
  lexsync:::write_csv_utf8(
    data.frame(word = if (upper) toupper(NORM_WORDS) else NORM_WORDS,
               freq_zipf = 3.0 + 0.2 * seq_along(NORM_WORDS) - 0.2,
               stringsAsFactors = FALSE), path)
  path
}

# A norm table covering `covered` of the lexicon (all of it by default).
norm_table <- function(dir, covered = NULL, name = "conc.csv") {
  words <- if (is.null(covered)) NORM_WORDS else NORM_WORDS[seq_len(covered)]
  path <- file.path(dir, name)
  lexsync:::write_csv_utf8(
    # Deliberately not monotonic in frequency, so a filter on concreteness selects a
    # set that a filter on frequency could not have produced by accident.
    data.frame(word = words,
               concreteness = 2.0 + ((seq_along(words) - 1L) %% 4L),
               stringsAsFactors = FALSE), path)
  path
}

test_that(".apply_norms joins and records provenance", {
  dir <- tempfile("norms"); dir.create(dir)
  schema <- norm_schema()
  lex <- load_lexicon(norm_lexicon(dir), schema)
  path <- norm_table(dir)
  out <- lexsync:::.apply_norms(lex, list(norms = list(list(path = path))))
  expect_true("concreteness" %in% names(out$lexicon))
  rec <- out$provenance[[1]]
  expect_identical(rec$path, path)
  expect_identical(nchar(rec$sha256), 64L)
  expect_identical(rec$on, "word")
  expect_identical(rec$columns,
                   list(list(column = "concreteness", n_matched = length(NORM_WORDS),
                             n_total = length(NORM_WORDS))))
})

test_that(".apply_norms records partial coverage", {
  # The uncovered rows carry NA, which the tolerance windows drop from the pool, so
  # coverage is part of how the pool was defined and belongs in the record.
  dir <- tempfile("norms"); dir.create(dir)
  schema <- norm_schema()
  lex <- load_lexicon(norm_lexicon(dir), schema)
  out <- lexsync:::.apply_norms(
    lex, list(norms = list(list(path = norm_table(dir, covered = 10)))))
  expect_identical(out$provenance[[1]]$columns[[1]],
                   list(column = "concreteness", n_matched = 10L,
                        n_total = length(NORM_WORDS)))
})

test_that(".apply_norms accepts a single bare mapping", {
  # Writing one table as a bare mapping rather than a one-element list is the obvious
  # thing to do in YAML, so it must not be silently ignored.
  dir <- tempfile("norms"); dir.create(dir)
  lex <- load_lexicon(norm_lexicon(dir), norm_schema())
  out <- lexsync:::.apply_norms(lex, list(norms = list(path = norm_table(dir))))
  expect_true("concreteness" %in% names(out$lexicon))
  expect_length(out$provenance, 1L)
})

test_that(".apply_norms is a no-op without the block", {
  # Every shipped design lacks a `norms:` block, so this path must not touch the
  # lexicon at all -- otherwise adding the feature would move existing artefacts.
  dir <- tempfile("norms"); dir.create(dir)
  lex <- load_lexicon(norm_lexicon(dir), norm_schema())
  out <- lexsync:::.apply_norms(lex, list(name = "x"))
  expect_length(out$provenance, 0L)
  expect_identical(out$lexicon, lex)
})

test_that(".apply_norms rejects a traversing path or a missing one", {
  dir <- tempfile("norms"); dir.create(dir)
  lex <- load_lexicon(norm_lexicon(dir), norm_schema())
  expect_error(lexsync:::.apply_norms(lex, list(norms = list(list(path = "../secrets/x.csv")))),
               "must not contain '..'", fixed = TRUE)
  expect_error(lexsync:::.apply_norms(lex, list(norms = list(list(columns = list("concreteness"))))),
               "needs a `path`", fixed = TRUE)
})

test_that(".apply_norms folds the lexicon key", {
  # load_lexicon case-folds `word`, so an upper-case source still joins. This is the
  # regression guard for the half-folded key: only the norm table's side used to be
  # normalised, and both engines then agreed on an all-NA dimension.
  dir <- tempfile("norms"); dir.create(dir)
  lex <- load_lexicon(norm_lexicon(dir, upper = TRUE), norm_schema())
  out <- lexsync:::.apply_norms(lex, list(norms = list(list(path = norm_table(dir)))))
  expect_false(any(is.na(out$lexicon$concreteness)))
})

# End to end: the block is applied before the pool, so a norm column is matchable.
# This is what the wiring buys. Before it, merge_norms existed but nothing called it,
# so a semantic dimension could not reach `match_on` at all.
test_that("the pipeline matches on a norm column and records it", {
  dir <- tempfile("norms"); dir.create(dir)
  lexicon <- norm_lexicon(dir)
  norms_path <- norm_table(dir)
  design <- list(
    name = "normtest", language = "english", lexicon = lexicon,
    description = "A design whose control dimension comes from a norm table.",
    n_per_condition = 3L,
    norms = list(list(path = norms_path, columns = list("concreteness"))),
    pool_filters = list(concreteness = c(2.0, 5.0)),
    conditions = list(list(name = "high", define_by = list(frequency = c(4.2, 7.0))),
                      list(name = "low", define_by = list(frequency = c(3.0, 3.8)))),
    match_on = list("length", "concreteness"),
    counterbalance = list(lists = 1L),
    timing = list(fixation_ms = 500L, word_ms = 500L, isi_ms = 250L))
  design_path <- file.path(dir, "design_normtest.yaml")
  yaml::write_yaml(design, design_path)

  outdir <- file.path(dir, "out")
  run_pipeline(design_path, system.file("extdata", "schema.yaml", package = "lexsync"),
               outdir, verbose = FALSE)

  stim <- as.data.frame(lexsync:::read_csv_utf8(
    file.path(outdir, "stimuli", "normtest_english_stimuli_R.csv")))
  # The norm column reached the stimuli table, and the pool filter held.
  expect_true("concreteness" %in% names(stim))
  expect_false(any(is.na(stim$concreteness)))
  expect_true(all(stim$concreteness >= 2.0 & stim$concreteness <= 5.0))

  ds <- jsonlite::fromJSON(file.path(outdir, "reports", "normtest_english_datasheet_R.json"),
                           simplifyVector = FALSE)
  recorded <- ds$materials_source$norms
  expect_length(recorded, 1L)
  expect_identical(recorded[[1]]$path, norms_path)
  expect_identical(recorded[[1]]$columns[[1]]$column, "concreteness")
  # The matched dimension is named as controlled, and the norm file is named in the
  # Methods prose a user pastes into a paper.
  expect_true("concreteness" %in% unlist(ds$selection$match_on))
  md <- paste(readLines(file.path(outdir, "reports", "normtest_english_datasheet_R.md"),
                        warn = FALSE), collapse = "\n")
  expect_true(grepl("## Joined norms", md, fixed = TRUE))
  expect_true(grepl("Norm dimensions were joined from conc.csv", md, fixed = TRUE))
})
