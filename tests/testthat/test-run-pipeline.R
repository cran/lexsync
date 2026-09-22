extdata <- function(f) system.file("extdata", f, package = "lexsync")

# Everything is written under tempdir(), so R CMD check leaves no artefacts and
# the committed output/ tree that the parity suite compares against is untouched.
tmp_out <- function() {
  d <- tempfile("lexsync-")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

# One quiet call, so the suite is not narrated. run_pipeline() restores the
# verbosity option itself, so nothing has to be put back here.
run_quiet <- function(...) run_pipeline(..., verbose = FALSE)

write_design <- function(dir, text) {
  p <- file.path(dir, "design.yaml")
  writeLines(text, p, useBytes = TRUE)
  p
}

as_yaml_path <- function(p) gsub("\\\\", "/", p)

test_that("pipeline functions require an explicit output directory", {
  expect_error(run_pipeline("not-used.yaml", verbose = FALSE),
               "outdir must be supplied")
  expect_error(run_all(config_dir = "not-used", verbose = FALSE),
               "outdir must be supplied")
})

test_that("run_pipeline exports every artefact for a corpus design", {
  # Mirrors test_cli.py::test_run_one_design, but asserts on the whole artefact
  # set: nothing else in R CMD check executes the orchestrator.
  out <- tmp_out()
  design <- write_design(out, c(
    "name: cli_test",
    "language: english",
    paste0("lexicon: ", as_yaml_path(extdata("en_example.csv"))),
    "n_per_condition: 5",
    "pool_filters: {length: [3, 7], frequency: [3.8, 7]}",
    "conditions:",
    "  - {name: high, define_by: {frequency: [5.0, 7.0]}}",
    "  - {name: low, define_by: {frequency: [3.8, 4.4]}}",
    "match_on: [length]"
  ))

  res <- run_quiet(design, extdata("schema.yaml"), outdir = out)

  for (f in c(file.path("stimuli", "cli_test_english_stimuli_R.csv"),
              file.path("reports", "cli_test_english_descriptives_R.csv"),
              file.path("reports", "cli_test_english_comparisons_R.csv"),
              file.path("reports", "cli_test_english_datasheet_R.json"),
              file.path("reports", "cli_test_english_datasheet_R.md"),
              file.path("reports", "cli_test_english_run_log_R.md"))) {
    expect_true(file.exists(file.path(out, f)), info = f)
  }
  expect_named(res, c("stimuli", "descriptives", "comparisons", "experiments", "log"))

  stim <- read_csv_utf8(res$stimuli)
  expect_equal(as.integer(table(stim$condition)), c(5L, 5L))
  expect_equal(sort(unique(stim$set)), 1:5)
  expect_true(all(c("word", "condition", "set", "trial") %in% names(stim)))
})

test_that("run_pipeline generates pseudowords for a generate design", {
  out <- tmp_out()
  design <- write_design(out, c(
    "name: gen_test",
    "language: english",
    "paradigm: lexical_decision",
    "items:",
    "  source: generate",
    paste0("  lexicon: ", as_yaml_path(extdata("en_example.csv"))),
    "n_per_condition: 5",
    "pool_filters: {length: [4, 7], frequency: [3.5, 6.0]}"
  ))

  res <- run_quiet(design, extdata("schema.yaml"), outdir = out)

  stim <- read_csv_utf8(res$stimuli)
  expect_setequal(unique(stim$condition), c("word", "pseudoword"))
  expect_equal(as.integer(table(stim$condition)), c(5L, 5L))
})

test_that("run_pipeline reads a table design from its items file", {
  out <- tmp_out()
  items <- file.path(out, "pairs.csv")
  writeLines(c("item,condition,prime,target",
               "1,related,nurse,doctor",
               "1,unrelated,window,doctor",
               "2,related,dog,cat",
               "2,unrelated,table,cat"), items, useBytes = TRUE)
  design <- write_design(out, c(
    "name: tab_test",
    "language: english",
    "paradigm: priming",
    "items:",
    "  source: table",
    paste0("  path: ", as_yaml_path(items)),
    "counterbalance:",
    "  lists: 2"
  ))

  res <- run_quiet(design, extdata("schema.yaml"), outdir = out)

  stim <- read_csv_utf8(res$stimuli)
  expect_setequal(unique(stim$target), c("doctor", "cat"))
  # A table design matches nothing, so no descriptives report is produced.
  expect_null(res$descriptives)
  expect_null(res$comparisons)
})

test_that("an unknown item source is an error, not a silent fallback", {
  out <- tmp_out()
  design <- write_design(out, c(
    "name: bad_test",
    "language: english",
    "items:",
    "  source: telepathy"
  ))
  expect_error(run_quiet(design, extdata("schema.yaml"), outdir = out),
               "unknown item source 'telepathy'")
})

test_that("run_all errors when the configuration directory holds no designs", {
  out <- tmp_out()
  expect_error(run_all(config_dir = out, schema_path = extdata("schema.yaml"),
                       outdir = out, verbose = FALSE),
               "no design_.*yaml files")
})

# ---- pipeline-level guards --------------------------------------------------
# Twinned with test_run_pipeline.py: every fixture, expectation and message
# below must stay in step with the Python suite.

test_that("a misspelt pool filter is an error, not an unfiltered pool", {
  out <- tmp_out()
  design <- write_design(out, c(
    "name: guard_test",
    "language: english",
    paste0("lexicon: ", as_yaml_path(extdata("en_example.csv"))),
    "n_per_condition: 5",
    "pool_filters: {frequncy: [3.8, 7]}",
    "conditions:",
    "  - {name: high, define_by: {frequency: [5.0, 7.0]}}",
    "  - {name: low, define_by: {frequency: [3.8, 4.4]}}",
    "match_on: [length]"
  ))
  expect_error(run_quiet(design, extdata("schema.yaml"), outdir = out),
               "pool_filters name column")
})

test_that("a non-integer n_per_condition is an error", {
  out <- tmp_out()
  design <- write_design(out, c(
    "name: guard_test",
    "language: english",
    paste0("lexicon: ", as_yaml_path(extdata("en_example.csv"))),
    "n_per_condition: 2.5",
    "conditions:",
    "  - {name: high, define_by: {frequency: [5.0, 7.0]}}",
    "  - {name: low, define_by: {frequency: [3.8, 4.4]}}",
    "match_on: [length]"
  ))
  expect_error(run_quiet(design, extdata("schema.yaml"), outdir = out),
               "positive whole number")
})

# Both values used to slip past this guard in one engine only. A configured 0 is
# falsy in Python, so `or` read it as absent, fell through to the matcher's default
# of twenty and ran; `%||%` here falls back on NULL alone and refused. An infinite
# request went the other way: trunc(Inf) is Inf, so this engine took it as a whole
# number and selected the whole pool, while Python's int() raised OverflowError.
# Pinned identically in tests/test_run_pipeline.py.
test_that("a zero or infinite n_per_condition is an error", {
  for (n in c("0", ".inf")) {
    out <- tmp_out()
    design <- write_design(out, c(
      "name: guard_test",
      "language: english",
      paste0("lexicon: ", as_yaml_path(extdata("en_example.csv"))),
      paste0("n_per_condition: ", n),
      "conditions:",
      "  - {name: high, define_by: {frequency: [5.0, 7.0]}}",
      "  - {name: low, define_by: {frequency: [3.8, 4.4]}}",
      "match_on: [length]"
    ))
    expect_error(run_quiet(design, extdata("schema.yaml"), outdir = out),
                 "positive whole number", info = n)
  }
})

# A design with one condition has nothing to compare against the anchor, and the
# engines used to part company there: this one died inside the reporting loop on
# seq_len(nrow(NULL)), while the Python engine finished and wrote a comparisons CSV
# with no header. Both now write this header and no rows. Pinned identically in
# tests/test_run_pipeline.py.
test_that("a single-condition design completes and writes a header-only comparisons file", {
  out <- tmp_out()
  design <- write_design(out, c(
    "name: onecond",
    "language: english",
    paste0("lexicon: ", as_yaml_path(extdata("en_example.csv"))),
    "n_per_condition: 5",
    "pool_filters: {length: [3, 7], frequency: [3.8, 7]}",
    "conditions:",
    "  - {name: only, define_by: {frequency: [4.0, 7.0]}}",
    "match_on: [length]"
  ))

  res <- run_quiet(design, extdata("schema.yaml"), outdir = out)

  expect_true(file.exists(res$comparisons))
  expect_identical(
    readLines(res$comparisons, warn = FALSE),
    paste0("condition,reference,dimension,cohens_d,d_ci_low,d_ci_high,",
           "var_ratio,tost_p,equivalent"))
})

test_that("a continuous table without members is an error", {
  out <- tmp_out()
  items <- file.path(out, "pairs.csv")
  writeLines(c("item,condition,prime,target", "1,related,nurse,doctor"),
             items, useBytes = TRUE)
  design <- write_design(out, c(
    "name: guard_test",
    "language: english",
    "paradigm: priming",
    "items:",
    "  source: table",
    paste0("  path: ", as_yaml_path(items)),
    "continuous:",
    "  predictor: target.frequency",
    "  controls: [target.length]",
    "match_on: [target.length]"
  ))
  expect_error(run_quiet(design, extdata("schema.yaml"), outdir = out),
               "requires items.members")
})

test_that("pool_filters on a plain table are an error", {
  out <- tmp_out()
  items <- file.path(out, "pairs.csv")
  writeLines(c("item,condition,prime,target",
               "1,related,nurse,doctor",
               "1,unrelated,window,doctor"), items, useBytes = TRUE)
  design <- write_design(out, c(
    "name: guard_test",
    "language: english",
    "paradigm: priming",
    "items:",
    "  source: table",
    paste0("  path: ", as_yaml_path(items)),
    "pool_filters: {length: [3, 7]}"
  ))
  expect_error(run_quiet(design, extdata("schema.yaml"), outdir = out),
               "pool_filters have no effect")
})

test_that("a generate shortfall errors by default and allow accepts it", {
  out <- tmp_out()
  base <- c(
    "name: guard_gen",
    "language: english",
    "paradigm: lexical_decision",
    "items:",
    "  source: generate",
    paste0("  lexicon: ", as_yaml_path(extdata("en_example.csv"))),
    "n_per_condition: 2000",
    "pool_filters: {length: [4, 7], frequency: [3.5, 6.0]}"
  )
  expect_error(run_quiet(write_design(out, base), extdata("schema.yaml"), outdir = out),
               "could be generated")
  out2 <- tmp_out()
  design <- write_design(out2, c(base, "matching: {shortfall: allow}"))
  res <- suppressMessages(run_quiet(design, extdata("schema.yaml"), outdir = out2))
  expect_true(file.exists(res$stimuli))
})

test_that("a condition without define_by reaches the datasheet", {
  # This engine's NULL define_by always fell back to the whole pool; the Python
  # engine used to crash with a bare KeyError on the same design, so this run is
  # pinned in both suites.
  out <- tmp_out()
  design <- write_design(out, c(
    "name: guard_bare",
    "language: english",
    paste0("lexicon: ", as_yaml_path(extdata("en_example.csv"))),
    "n_per_condition: 3",
    "pool_filters: {length: [3, 7], frequency: [3.8, 7]}",
    "conditions:",
    "  - {name: high, define_by: {frequency: [5.0, 7.0]}}",
    "  - {name: rest}",
    "match_on: [length]"
  ))
  res <- run_quiet(design, extdata("schema.yaml"), outdir = out)
  stim <- read_csv_utf8(res$stimuli)
  expect_setequal(unique(stim$condition), c("high", "rest"))
  ds <- jsonlite::fromJSON(file.path(out, "reports", "guard_bare_english_datasheet_R.json"),
                           simplifyVector = FALSE)
  entries <- ds$selection$candidate_pool
  conds <- vapply(entries, function(e) e$condition, character(1))
  counts <- vapply(entries, function(e) as.numeric(e$n_candidates), numeric(1))
  expect_setequal(conds, c("high", "rest"))
  # The bare condition's candidates are the whole filtered pool.
  expect_gte(counts[conds == "rest"], counts[conds == "high"])
  expect_gt(counts[conds == "high"], 0)
})

test_that("a pair design's window relaxation reaches the datasheet", {
  # The pair path dropped the selector's audit on re-expansion, so a tolerance
  # relaxation never reached the run log or the datasheet. tolerance_k 0 pins a
  # zero-width window that no pair satisfies, forcing the relaxation.
  out <- tmp_out()
  items <- file.path(out, "pairs.csv")
  writeLines(c("item,condition,prime,target",
               "1,related,aaa,flat", "1,unrelated,abba,flat",
               "2,related,acne,glass", "2,unrelated,aaron,glass",
               "3,related,alarm,across", "3,unrelated,abrams,across",
               "4,related,aha,house", "4,unrelated,abdel,house"),
             items, useBytes = TRUE)
  design <- write_design(out, c(
    "name: guard_pair_relax",
    "language: english",
    "paradigm: priming",
    "items:",
    "  source: table",
    paste0("  path: ", as_yaml_path(items)),
    "  members: [prime, target]",
    paste0("  lexicon: ", as_yaml_path(extdata("en_example.csv"))),
    "  anchor_condition: related",
    "n_per_condition: 3",
    "continuous:",
    "  predictor: target.frequency",
    "  controls: [target.length]",
    "match_on: [target.length]",
    "matching: {tolerance_k: {target.length: 0}}"
  ))
  res <- run_quiet(design, extdata("schema.yaml"), outdir = out)
  ds <- jsonlite::fromJSON(file.path(out, "reports",
                                     "guard_pair_relax_english_datasheet_R.json"),
                           simplifyVector = FALSE)
  rx <- ds$selection$window_relaxations
  expect_length(rx, 1)
  expect_identical(rx[[1]]$condition, "continuous")
  expect_identical(as.integer(rx[[1]]$n_needed), 3L)
  log <- paste(readLines(res$log, warn = FALSE), collapse = "\n")
  expect_true(grepl("tolerance window relaxed", log, fixed = TRUE))
})

test_that("a continuous design over a supplied pool selects continuously", {
  # The predicate that gates the continuous selector must treat a supplied
  # pool like a corpus; before the fix this design fell through to the
  # conditions matcher and crashed differently in each engine.
  out <- tmp_out()
  lex <- read_csv_utf8(extdata("en_example.csv"))
  pool_path <- file.path(out, "pool.csv")
  write_csv_utf8(data.frame(word = head(lex$word, 60)), pool_path)
  design <- write_design(out, c(
    "name: guard_pool",
    "language: english",
    "items:",
    "  source: pool",
    paste0("  path: ", as_yaml_path(pool_path)),
    paste0("  lexicon: ", as_yaml_path(extdata("en_example.csv"))),
    "n_per_condition: 10",
    "continuous:",
    "  predictor: frequency",
    "  controls: [length]",
    "match_on: [length]"
  ))
  res <- run_quiet(design, extdata("schema.yaml"), outdir = out)
  stim <- read_csv_utf8(res$stimuli)
  expect_true(all(stim$condition == "continuous"))
  expect_equal(sort(unique(stim$set)), 1:10)
})

test_that("run_pipeline leaves the verbosity option as it found it", {
  # An exported function that set an option and walked away would silence, or
  # unsilence, the rest of the caller's session. Both exit paths are covered,
  # since a design that fails validation must restore it too.
  # test_run_pipeline.py::test_verbose_false_keeps_the_console_silent asserts the
  # same property on the Python engine's module-level gate.
  out <- tmp_out()
  design <- write_design(out, c(
    "name: opt_test",
    "language: english",
    paste0("lexicon: ", as_yaml_path(extdata("en_example.csv"))),
    "n_per_condition: 5",
    "pool_filters: {length: [3, 7], frequency: [3.8, 7]}",
    "conditions:",
    "  - {name: high, define_by: {frequency: [5.0, 7.0]}}",
    "  - {name: low, define_by: {frequency: [3.8, 4.4]}}",
    "match_on: [length]"
  ))

  before <- getOption("lexsync.verbose")
  invisible(run_quiet(design, extdata("schema.yaml"), outdir = out))
  expect_identical(getOption("lexsync.verbose"), before)

  bad <- write_design(out, c(
    "name: opt_test_bad",
    "language: english",
    paste0("lexicon: ", as_yaml_path(extdata("en_example.csv"))),
    "n_per_condition: 0",
    "conditions:",
    "  - {name: high, define_by: {frequency: [5.0, 7.0]}}"
  ))
  expect_error(run_quiet(bad, extdata("schema.yaml"), outdir = out),
               "positive whole number")
  expect_identical(getOption("lexsync.verbose"), before)
})
