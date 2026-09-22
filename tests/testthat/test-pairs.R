# The pair-keyed item model: member norms, relational dimensions, pair selection.
#
# A relational design's predictor is a property of the PAIR while its controls are
# properties of each MEMBER, so it needs an item table and selection at the same
# time. Until this existed a design could have one or the other.
#
# Three properties have to hold, and each has a test here.
#
# The member prefix leads (`prime.frequency`, never `frequency.prime`). R's `$`
# partial-matches on a data frame, so a joined `word.frequency` with no bare `word`
# would make the selector's tie-break silently sort by it here while the Python
# engine raised KeyError. The reserved-name check prevents that class of divergence.
#
# Selection runs on one row per pair and re-expands. A filter on `target.frequency`
# applied row-wise would keep a pair's related row and drop its unrelated one,
# leaving a set the Latin-square counterbalancer cannot complete.
#
# `pair.overlap` is computed in-engine, which is only safe because the core is an
# integer edit distance and this path's arithmetic reaches nothing beyond -, / and
# the shared rounder. That is a property of this path, not of the package: the
# Student t functions behind the equivalence report do reach an artefact, and
# validation.R records what is guaranteed there instead.
# python_workflow/tests/test_pairs.py asserts the same values.

pairs_df <- data.frame(
  item = c(1L, 1L, 2L, 2L, 3L, 3L),
  set = c(1L, 1L, 2L, 2L, 3L, 3L),
  condition = rep(c("related", "unrelated"), 3),
  prime = c("nurse", "window", "dog", "table", "queen", "pencil"),
  target = c("doctor", "doctor", "cat", "cat", "king", "king"),
  stringsAsFactors = FALSE
)
pairs_df[["target.frequency"]] <- c(5.0, 5.0, 4.8, 4.8, 5.2, 5.2)
pairs_df[["target.length"]] <- c(6L, 6L, 3L, 3L, 4L, 4L)

test_that("relational overlap matches the Python engine", {
  out <- add_pair_overlap(pairs_df)
  expect_equal(out[["pair.lev"]], c(6L, 5L, 3L, 4L, 5L, 5L))
  expect_equal(out[["pair.overlap"]],
               c(0, 0.166666667, 0, 0.2, 0, 0.166666667))
})

test_that("overlap handles non-ASCII and the degenerate pair", {
  df <- data.frame(prime = c("café", "", "北京"),
                   target = c("cafe", "", "北方"), stringsAsFactors = FALSE)
  out <- add_pair_overlap(df)
  expect_equal(out[["pair.lev"]], c(1L, 0L, 1L))
  # Two empty forms give 0, never 0/0: a NaN would be sorted and compared, and would
  # drop the row from one engine's control window but not the other's.
  expect_equal(out[["pair.overlap"]], c(0.75, 0, 0.5))
})

test_that("a reserved member name is rejected", {
  for (nm in c("word", "set", "condition", "item")) {
    expect_true(nm %in% lexsync:::.RESERVED_MEMBER_NAMES)
    expect_error(lexsync:::.check_members(nm, pairs_df), "reserved name")
  }
})

test_that("a member column that is absent is rejected", {
  expect_error(lexsync:::.check_members(c("prime", "nope"), pairs_df), "does not have")
})

test_that("selection keeps every condition row of every chosen pair", {
  design <- list(n_per_condition = 2L,
                 continuous = list(predictor = "target.frequency",
                                   controls = list("target.length")),
                 match_on = list("target.length"))
  res <- lexsync:::.select_continuous_pairs(
    pairs_df, list(anchor_condition = "related"), design,
    list(matching = list(tolerance_k = list(target.length = 2))))
  out <- res$stim
  expect_equal(length(unique(out$set)), 2L)
  expect_equal(nrow(out), 4L)
  expect_true(all(tapply(out$condition, out$set, function(x) length(unique(x))) == 2L))
})

test_that("a pair failing a filter on any row is excluded whole", {
  design <- list(n_per_condition = 3L,
                 pool_filters = list(target.length = c(4L, 6L)),   # excludes set 2
                 continuous = list(predictor = "target.frequency",
                                   controls = list("target.length")),
                 match_on = list("target.length"),
                 # The exclusion leaves 2 of the 3 requested pairs; accepting that
                 # shrink is the point of the test, so the shortfall policy is
                 # opted out of here.
                 matching = list(shortfall = "allow"))
  res <- lexsync:::.select_continuous_pairs(
    pairs_df, list(anchor_condition = "related"), design,
    list(matching = list(tolerance_k = list(target.length = 2))))
  expect_equal(res$n_eligible, 2L)
  expect_false(2L %in% res$stim$set)
})

test_that("a mistyped pool filter is rejected rather than skipped", {
  design <- list(n_per_condition = 2L,
                 pool_filters = list(target.lenght = c(3L, 6L)),
                 continuous = list(predictor = "target.frequency",
                                   controls = list("target.length")),
                 match_on = list("target.length"))
  # build_pool silently skips an unknown column, which would quietly widen the
  # selection rather than failing.
  expect_error(lexsync:::.select_continuous_pairs(
    pairs_df, list(), design, list(matching = list(tolerance_k = list()))),
    "does not have")
})

test_that("the anchor condition defaults to the byte-first one", {
  design <- list(n_per_condition = 3L,
                 continuous = list(predictor = "target.frequency",
                                   controls = list("target.length")),
                 match_on = list("target.length"))
  schema <- list(matching = list(tolerance_k = list()))
  explicit <- lexsync:::.select_continuous_pairs(
    pairs_df, list(anchor_condition = "related"), design, schema)
  default <- lexsync:::.select_continuous_pairs(pairs_df, list(), design, schema)
  expect_equal(default$stim$set, explicit$stim$set)
})

test_that("a window relaxation is returned on the re-expanded frame", {
  # The audit rides on the collapsed selection as an attribute, which the
  # re-expansion used to drop, so a relaxed window on a pair design left no
  # trace for the run log or the datasheet. tolerance_k 0 pins a zero-width
  # window no pair satisfies, forcing the relaxation. Twinned with
  # test_pairs.py.
  design <- list(n_per_condition = 2L,
                 continuous = list(predictor = "target.frequency",
                                   controls = list("target.length")),
                 match_on = list("target.length"))
  res <- lexsync:::.select_continuous_pairs(
    pairs_df, list(anchor_condition = "related"), design,
    list(matching = list(tolerance_k = list(target.length = 0))))
  audit <- attr(res$stim, "audit")
  expect_false(is.null(audit))
  rx <- audit$window_relaxations
  expect_length(rx, 1)
  expect_identical(rx[[1]]$condition, "continuous")
  expect_identical(as.integer(rx[[1]]$n_needed), 2L)
})

test_that("a pair missing its anchor row is an error", {
  partial <- pairs_df[pairs_df$condition == "unrelated", , drop = FALSE]
  rownames(partial) <- NULL
  design <- list(n_per_condition = 2L,
                 continuous = list(predictor = "target.frequency",
                                   controls = list("target.length")),
                 match_on = list("target.length"))
  expect_error(lexsync:::.select_continuous_pairs(
    partial, list(anchor_condition = "related"), design,
    list(matching = list(tolerance_k = list()))),
    "no 'related' row")
})
