make_design <- function() {
  list(
    name = "t", language = "english", n_per_condition = 10,
    pool_filters = list(length = c(3, 7), frequency = c(3.8, 7)),
    conditions = list(
      list(name = "high", define_by = list(frequency = c(5.2, 7.0))),
      list(name = "low",  define_by = list(frequency = c(3.8, 4.4)))
    ),
    match_on = list("length", "n_density", "old20")
  )
}

test_that("match_stimuli is deterministic", {
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  lex <- load_lexicon(system.file("extdata", "en_example.csv", package = "lexsync"), schema, "english")
  s1 <- match_stimuli(lex, make_design(), schema)
  s2 <- match_stimuli(lex, make_design(), schema)
  expect_identical(s1$word, s2$word)
})

test_that("match_stimuli balances conditions and isolates the manipulated dimension", {
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  lex <- load_lexicon(system.file("extdata", "en_example.csv", package = "lexsync"), schema, "english")
  s <- match_stimuli(lex, make_design(), schema)
  expect_equal(as.integer(table(s$condition)), c(10L, 10L))
  hi <- s$frequency[s$condition == "high"]
  lo <- s$frequency[s$condition == "low"]
  expect_gt(mean(hi), mean(lo))                              # frequency manipulated
  expect_lt(abs(cohens_d(s$length[s$condition == "high"],
                         s$length[s$condition == "low"])), 0.3)  # length controlled
})

make_confounded_design <- function(method = NULL) {
  # Neighbourhood density is intrinsically confounded with length and
  # frequency, so the per-anchor matcher leaves a residual on the controls.
  d <- list(
    name = "tj", language = "english", n_per_condition = 20,
    pool_filters = list(length = c(3, 7), frequency = c(3.8, 7)),
    conditions = list(
      list(name = "dense",  define_by = list(n_density = c(4, 100))),
      list(name = "sparse", define_by = list(n_density = c(0, 1)))
    ),
    match_on = list("length", "frequency")
  )
  if (!is.null(method)) d$matching <- list(method = method)
  d
}

test_that("joint matching cancels the control-dimension confound", {
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  lex <- load_lexicon(system.file("extdata", "en_example.csv", package = "lexsync"), schema, "english")
  s <- match_stimuli(lex, make_confounded_design(method = "joint"), schema)
  expect_equal(as.integer(table(s$condition)), c(20L, 20L))
  expect_gt(mean(s$n_density[s$condition == "dense"]),
            mean(s$n_density[s$condition == "sparse"]))      # density manipulated
  for (dim in c("length", "frequency")) {                    # controls equated tightly
    expect_lt(abs(cohens_d(s[[dim]][s$condition == "dense"],
                           s[[dim]][s$condition == "sparse"])), 0.1)
  }
  # Joint matching is at least as tight as the per-anchor matcher on the confound.
  s_std <- match_stimuli(lex, make_confounded_design(), schema)
  d_joint <- abs(cohens_d(s$length[s$condition == "dense"],
                          s$length[s$condition == "sparse"]))
  d_std <- abs(cohens_d(s_std$length[s_std$condition == "dense"],
                        s_std$length[s_std$condition == "sparse"]))
  expect_lte(d_joint, d_std + 1e-9)
})

test_that("mahalanobis matching balances and is deterministic within an engine", {
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  lex <- load_lexicon(system.file("extdata", "en_example.csv", package = "lexsync"), schema, "english")
  d <- make_design(); d$matching <- list(method = "mahalanobis")
  s <- match_stimuli(lex, d, schema)
  expect_equal(as.integer(table(s$condition)), c(10L, 10L))
  expect_gt(mean(s$frequency[s$condition == "high"]), mean(s$frequency[s$condition == "low"]))
  for (dim in c("length", "n_density", "old20")) {           # correlated controls balanced
    expect_lt(abs(cohens_d(s[[dim]][s$condition == "high"],
                           s[[dim]][s$condition == "low"])), 0.5)
  }
  # deterministic within an engine (cross-engine identity is not guaranteed here)
  expect_identical(s$word, match_stimuli(lex, d, schema)$word)
})

test_that("a missing value on a matched dimension reduces the mahalanobis metric to identity", {
  # cor() under its default use = "everything" returns NA for any pair involving a
  # column that holds one, and .maha_metric reads NA as no correlation, so the metric
  # collapses and mahalanobis matching becomes the standardised Euclidean matching it
  # was chosen over. Nothing upstream prevents it, which is what the comment there now
  # says; this pins the behaviour so a change of estimator cannot pass unnoticed.
  # Pinned identically in tests/test_matching.py.
  z <- cbind(c(1, 2, 3, 4), c(4, 3, 2, 1))
  informative <- .maha_metric(z)
  expect_gt(abs(informative[1, 2]), 0.5)                 # the correlation is used

  z[2, 1] <- NA_real_
  collapsed <- .maha_metric(z)
  expect_equal(collapsed[1, 2], 0)
  expect_equal(collapsed, diag(2) / (1 + 1e-6), tolerance = 1e-12)
})

test_that("optimal matching equates the confound and is deterministic within an engine", {
  skip_if_not_installed("clue")
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  lex <- load_lexicon(system.file("extdata", "en_example.csv", package = "lexsync"), schema, "english")
  s <- match_stimuli(lex, make_confounded_design(method = "optimal"), schema)
  expect_equal(as.integer(table(s$condition)), c(20L, 20L))
  expect_gt(mean(s$n_density[s$condition == "dense"]),
            mean(s$n_density[s$condition == "sparse"]))
  for (dim in c("length", "frequency")) {
    expect_lt(abs(cohens_d(s[[dim]][s$condition == "dense"],
                           s[[dim]][s$condition == "sparse"])), 0.15)
  }
  expect_identical(s$word,
                   match_stimuli(lex, make_confounded_design(method = "optimal"), schema)$word)
})

make_continuous_design <- function() {
  list(name = "cont", language = "english", n_per_condition = 40,
    pool_filters = list(length = c(3, 8), frequency = c(3.8, 7.0)),
    continuous = list(predictor = "frequency",
                      controls = list("length", "n_density", "old20")),
    match_on = list("length", "n_density", "old20"),
    matching = list(tolerance_k = list(length = 1.5, n_density = 1.5, old20 = 1.5)))
}

test_that("select_continuous_stimuli spans the predictor and decorrelates controls", {
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  lex <- load_lexicon(system.file("extdata", "en_example.csv", package = "lexsync"), schema, "english")
  pool <- build_pool(lex, make_continuous_design()$pool_filters)
  s <- select_continuous_stimuli(pool, make_continuous_design(), schema)
  expect_true(all(s$condition == "continuous"))
  expect_equal(s$set, seq_len(nrow(s)))
  for (cc in c("length", "n_density", "old20")) {
    expect_lt(abs(suppressWarnings(stats::cor(s$frequency, s[[cc]]))), 0.6)
  }
  # deterministic within an engine
  expect_identical(s$word, select_continuous_stimuli(pool, make_continuous_design(), schema)$word)
})

test_that("select_continuous_stimuli is driven by byte order, not row order", {
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  lex <- load_lexicon(system.file("extdata", "en_example.csv", package = "lexsync"), schema, "english")
  pool <- build_pool(lex, make_continuous_design()$pool_filters)
  a <- select_continuous_stimuli(pool, make_continuous_design(), schema)
  b <- select_continuous_stimuli(pool[rev(seq_len(nrow(pool))), , drop = FALSE],
                                 make_continuous_design(), schema)
  expect_setequal(a$word, b$word)
})

test_that("select_continuous_stimuli rejects the predictor appearing in controls", {
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  lex <- load_lexicon(system.file("extdata", "en_example.csv", package = "lexsync"), schema, "english")
  pool <- build_pool(lex, make_continuous_design()$pool_filters)
  d <- make_continuous_design()
  d$continuous$controls <- list("frequency", "length")
  d$match_on <- list("frequency", "length")
  expect_error(select_continuous_stimuli(pool, d, schema), "must not also appear")
})

test_that("select_continuous_stimuli requires match_on to equal the controls", {
  # Twin of test_select_continuous_requires_match_on_equals_controls, which this suite
  # was missing. The two keys state one contract twice, so a design where they disagree
  # is ambiguous about which dimensions are actually held constant, and the datasheet
  # would record whichever list it happened to read.
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  lex <- load_lexicon(system.file("extdata", "en_example.csv", package = "lexsync"), schema, "english")
  pool <- build_pool(lex, make_continuous_design()$pool_filters)
  d <- make_continuous_design()
  d$match_on <- list("length", "n_density")   # short of continuous.controls
  expect_error(select_continuous_stimuli(pool, d, schema), "match_on must equal")
})

test_that("select_continuous_stimuli requires at least one control", {
  # With no control there is nothing held constant, and an even spread over the
  # predictor alone would still be recorded as a controlled selection.
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  lex <- load_lexicon(system.file("extdata", "en_example.csv", package = "lexsync"), schema, "english")
  pool <- build_pool(lex, make_continuous_design()$pool_filters)
  d <- make_continuous_design()
  d$continuous$controls <- list()
  d$match_on <- list()
  expect_error(select_continuous_stimuli(pool, d, schema), "at least one control")
})

tiny_schema <- function() {
  list(matching = list(method = "standardised_euclidean", tolerance_k = list()))
}

na_pool <- function() {
  # The two missing-concreteness rows lead the low subpool deliberately: a matcher
  # that ranks an NA distance by row order rather than last would pick them first.
  data.frame(
    id = 1:8,
    word = c("lac", "lad", "laa", "lab", "laz", "hab", "hac", "had"),
    frequency = c(2.0, 2.5, 1.0, 1.5, 3.0, 5.0, 6.0, 7.0),
    concreteness = c(NA, NA, 3.0, 2.95, 9.0, 3.0, 3.1, 2.9),
    stringsAsFactors = FALSE
  )
}

na_design <- function(n = 3L) {
  list(name = "na", language = "english", n_per_condition = n,
       conditions = list(
         list(name = "high", define_by = list(frequency = c(5.0, 7.0))),
         list(name = "low",  define_by = list(frequency = c(1.0, 3.0)))
       ),
       match_on = list("concreteness"))
}

test_that("rows missing a matched dimension are dropped and ranked last", {
  # Pins the same contract as test_matching.py: only two low candidates fall inside
  # the anchor window, so the window relaxes, and the NA rows must never be chosen
  # ahead of a real one. The R and Python engines must agree word for word.
  s <- match_stimuli(na_pool(), na_design(), tiny_schema())
  expect_identical(s$word[s$condition == "high"], c("hab", "hac", "had"))
  expect_identical(s$word[s$condition == "low"], c("laa", "lab", "laz"))
  expect_false(any(is.na(s$concreteness)))
})

test_that("an undersized condition raises rather than duplicating words", {
  # Five anchors but only two low candidates: the greedy assignment would exhaust
  # the low pool and re-pick its first word for sets 3 to 5.
  pool <- data.frame(
    id = 1:7,
    word = c("laa", "lab", "hab", "hac", "had", "hae", "haf"),
    frequency = c(1.0, 1.5, 5.0, 5.5, 6.0, 6.5, 7.0),
    concreteness = rep(3.0, 7),
    stringsAsFactors = FALSE
  )
  expect_error(match_stimuli(pool, na_design(5L), tiny_schema()),
               "condition 'low' has only 2 candidate")
})

test_that("an NA-depleted condition raises rather than duplicating words", {
  # The raw row count clears n_per_condition, but only two rows carry the matched
  # dimension. The rest rank last on an NA distance and are never assigned, so the
  # greedy pass would exhaust the usable rows and re-pick them. Pinned identically in
  # test_matching.py.
  pool <- data.frame(
    id = 1:9,
    word = c("laa", "lab", "lac", "lad", "lae", "hab", "hac", "had", "hae"),
    frequency = c(1.0, 1.5, 2.0, 2.5, 3.0, 5.0, 5.5, 6.0, 6.5),
    concreteness = c(3.0, 3.1, NA, NA, NA, 3.0, 3.1, 2.9, 3.2),
    stringsAsFactors = FALSE
  )
  expect_error(match_stimuli(pool, na_design(4L), tiny_schema()),
               "condition 'low' has only 2 usable candidate")
})

test_that("a single-item anchor relaxes the window rather than selecting NA", {
  # An anchor of one item gives sd = NA, so every tolerance bound is NA. Without
  # keep[is.na(keep)] <- FALSE the whole vector goes NA and cand[keep, ] indexes
  # all-NA filler rows, which clear both count guards and are then selected. Python
  # compares False against NaN and relaxes; test_matching.py pins the same word.
  pool <- data.frame(
    id = 1:5,
    word = c("aaa", "bbb", "ccc", "ddd", "eee"),
    frequency = c(1.0, 2.0, 3.0, 4.0, 5.0),
    concreteness = c(3.0, 3.1, 3.2, 3.3, 3.4),
    stringsAsFactors = FALSE
  )
  design <- list(name = "one", language = "english", n_per_condition = 1L,
                 conditions = list(
                   list(name = "high", define_by = list(frequency = c(1.0, 1.0))),
                   list(name = "low",  define_by = list(frequency = c(2.0, 5.0)))
                 ),
                 match_on = list("concreteness"))
  s <- match_stimuli(pool, design, tiny_schema())
  expect_identical(s$word[s$condition == "low"], "bbb")
  expect_false(any(is.na(s$word)))
})

test_that("an unknown matching method is an error, not a silent fallback", {
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  lex <- load_lexicon(system.file("extdata", "en_example.csv", package = "lexsync"), schema, "english")
  d <- make_design(); d$matching <- list(method = "jiont")
  expect_error(match_stimuli(lex, d, schema), "unknown matching method 'jiont'")
})

test_that("joint matching rejects a design without exactly two conditions", {
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  lex <- load_lexicon(system.file("extdata", "en_example.csv", package = "lexsync"), schema, "english")
  d <- make_design()
  d$matching <- list(method = "joint")
  d$conditions[[3]] <- list(name = "mid", define_by = list(frequency = c(4.4, 5.2)))
  expect_error(match_stimuli(lex, d, schema), "requires exactly two conditions")
})

test_that("resample_stimuli produces disjoint matched sets", {
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  lex <- load_lexicon(system.file("extdata", "en_example.csv", package = "lexsync"), schema, "english")
  design <- list(name = "r", language = "english", n_per_condition = 5,
    pool_filters = list(length = c(3, 7), frequency = c(3.8, 7)),
    conditions = list(list(name = "high", define_by = list(frequency = c(5.0, 7.0))),
                      list(name = "low", define_by = list(frequency = c(3.8, 4.4)))),
    match_on = list("length"))
  pool <- build_pool(lex, design$pool_filters)
  s <- resample_stimuli(pool, design, schema, 3L)
  expect_setequal(unique(s$replicate), c(1L, 2L, 3L))
  for (r in unique(s$replicate)) {
    expect_equal(sort(as.integer(table(s$condition[s$replicate == r]))), c(5L, 5L))
  }
  w <- split(s$word, s$replicate)
  expect_length(intersect(w[[1]], w[[2]]), 0)
  expect_length(intersect(w[[1]], w[[3]]), 0)
  expect_length(intersect(w[[2]], w[[3]]), 0)
})

# ---- shortfall and tolerance policies, guards -------------------------------
# Twinned with test_matching.py: every fixture, expectation and message below
# must stay in step with the Python suite.

small_pool <- function() {
  data.frame(
    id = 1:8,
    word = c("haa", "hab", "hac", "laa", "lab", "lac", "lad", "lae"),
    frequency = c(5.0, 5.5, 6.0, 1.0, 1.5, 2.0, 2.5, 3.0),
    concreteness = c(3.0, 3.1, 2.9, 3.0, 3.1, 3.2, 2.8, 3.3),
    stringsAsFactors = FALSE
  )
}

small_design <- function(n, matching = NULL) {
  d <- list(
    name = "sf", language = "english", n_per_condition = n,
    conditions = list(
      list(name = "high", define_by = list(frequency = c(5.0, 7.0))),
      list(name = "low",  define_by = list(frequency = c(1.0, 3.0)))
    ),
    match_on = list("concreteness")
  )
  if (!is.null(matching)) d$matching <- matching
  d
}

test_that("a shortfall is an error by default", {
  # Three anchor candidates cannot honour n = 5; a silent shrink would leave
  # the datasheet stating the requested n over a smaller realised set.
  expect_error(match_stimuli(small_pool(), small_design(5L), tiny_schema()),
               "5 sets per condition were requested but only 3")
})

test_that("shortfall: allow accepts the shrink", {
  s <- match_stimuli(small_pool(), small_design(5L, matching = list(shortfall = "allow")),
                     tiny_schema())
  expect_equal(sort(as.integer(table(s$condition))), c(3L, 3L))
})

test_that("an unknown shortfall policy is an error", {
  expect_error(match_stimuli(small_pool(), small_design(3L, matching = list(shortfall = "maybe")),
                             tiny_schema()),
               "unknown shortfall policy 'maybe'")
})

test_that("joint matching errors on a shortfall by default", {
  # At most three disjoint pairs exist, so n = 5 cannot be met.
  pool <- small_pool()[1:6, , drop = FALSE]
  expect_error(match_stimuli(pool, small_design(5L, matching = list(method = "joint")),
                             tiny_schema()),
               "5 sets per condition were requested but only 3")
})

test_that("on_insufficient_tolerance: error refuses to relax", {
  # In the na_pool scenario the low condition has too few candidates inside
  # the anchor window, which the default policy silently relaxes.
  d <- na_design()
  d$matching <- list(on_insufficient_tolerance = "error")
  expect_error(match_stimuli(na_pool(), d, tiny_schema()),
               "within tolerance but 3 are needed")
})

test_that("a window relaxation is recorded in the audit attribute", {
  s <- match_stimuli(na_pool(), na_design(), tiny_schema())
  audit <- attr(s, "audit")
  expect_false(is.null(audit))
  rx <- audit$window_relaxations
  expect_length(rx, 1)
  expect_identical(rx[[1]]$condition, "low")
  expect_identical(rx[[1]]$n_needed, 3L)
})

overlapping_design <- function(n, method) {
  # Both conditions admit the whole pool, so every word sits in both subpools
  # and its self-pair costs exactly zero.
  list(
    name = "ov", language = "english", n_per_condition = n,
    conditions = list(
      list(name = "a", define_by = list(frequency = c(1.0, 7.0))),
      list(name = "b", define_by = list(frequency = c(1.0, 7.0)))
    ),
    match_on = list("concreteness"),
    matching = list(method = method, shortfall = "allow")
  )
}

test_that("joint matching never pairs a word with itself", {
  s <- match_stimuli(small_pool(), overlapping_design(3L, "joint"), tiny_schema())
  expect_false(any(duplicated(s$word)))
  for (st in split(s$word, s$set)) expect_length(unique(st), 2)
})

test_that("optimal matching never pairs a word with itself", {
  skip_if_not_installed("clue")
  s <- match_stimuli(small_pool(), overlapping_design(3L, "optimal"), tiny_schema())
  expect_false(any(duplicated(s$word)))
  for (st in split(s$word, s$set)) expect_length(unique(st), 2)
})

test_that("a misspelt define_by dimension is an error, not a widened condition", {
  d <- small_design(3L)
  d$conditions[[2]]$define_by <- list(frequnecy = c(1.0, 3.0))
  expect_error(match_stimuli(small_pool(), d, tiny_schema()),
               "dimension 'frequnecy' in condition 'low'")
})

test_that("a condition without define_by draws on the whole pool", {
  # NULL define_by falls back to the whole pool (and, for the anchor, to
  # ordering by 'frequency'); the Python engine used to crash with a bare
  # KeyError on a design this engine accepted. The words are pinned identically
  # in test_matching.py, so the two engines select the same stimuli.
  d <- small_design(3L)
  d$conditions[[2]] <- list(name = "rest")
  s <- match_stimuli(small_pool(), d, tiny_schema())
  expect_identical(s$word[s$condition == "high"], c("haa", "hab", "hac"))
  expect_identical(s$word[s$condition == "rest"], c("laa", "lab", "lad"))
  # An anchor without define_by orders by the fallback dimension.
  d <- small_design(3L)
  d$conditions[[1]] <- list(name = "any")
  d$conditions[[2]] <- list(name = "low", define_by = list(frequency = c(1.0, 3.0)))
  s <- match_stimuli(small_pool(), d, tiny_schema())
  expect_identical(s$word[s$condition == "any"], c("laa", "lad", "hac"))
  expect_identical(s$word[s$condition == "low"], c("lab", "lac", "lae"))
})

test_that("duplicate condition names are an error", {
  d <- small_design(3L)
  d$conditions[[2]]$name <- "high"
  expect_error(match_stimuli(small_pool(), d, tiny_schema()),
               "condition name 'high' appears more than once")
})

test_that("a negative tolerance is an error", {
  expect_error(match_stimuli(small_pool(),
                             small_design(3L, matching = list(tolerance_k = list(concreteness = -1))),
                             tiny_schema()),
               "tolerance_k for dimension 'concreteness' is negative")
})

test_that("a design without conditions is an error", {
  d <- small_design(3L)
  d$conditions <- NULL
  expect_error(match_stimuli(small_pool(), d, tiny_schema()),
               "the design has no conditions")
})

test_that("selection ignores unused columns and the seed", {
  # Metamorphic invariants: an inert extra column must not steer selection, and
  # the schema seed only ever reaches the shuffle keys, never the matcher.
  base <- match_stimuli(small_pool(), small_design(3L), tiny_schema())
  with_extra <- small_pool()
  with_extra$unused_norm <- c(9.9, 1.2, 5.5, 0.1, 7.7, 3.3, 2.2, 8.8)
  seeded <- tiny_schema()
  seeded$seed <- 99L
  expect_identical(match_stimuli(with_extra, small_design(3L), tiny_schema())$word, base$word)
  expect_identical(match_stimuli(small_pool(), small_design(3L), seeded)$word, base$word)
})

test_that("the matching module uses the shared rounder on distance paths", {
  # The 9-dp distance and cost roundings must go through .round_dp: native
  # round() pairs with numpy's different scale-rint-unscale algorithm. The two
  # remaining native calls are the 0-dp even-spread index sites, which are
  # half-even on exactly representable halves in both engines and pinned by the
  # committed goldens -- converting them would change selections.
  # Reads the package source, which only a source checkout has: R CMD check runs
  # the tests from lexsync.Rcheck/tests/testthat, where the installed copy holds a
  # lazy-load database and no R/ directory. Skip there rather than fail, the same
  # way test-config.R skips its repository-level checks.
  path <- testthat::test_path("..", "..", "R", "matching.R")
  skip_if_not(file.exists(path), "package sources not available")
  src <- readLines(path)
  src <- sub("#.*$", "", src)   # comments legitimately mention round()
  hits <- sum(lengths(regmatches(src, gregexpr("(?<![._[:alnum:]])round\\(", src, perl = TRUE))))
  expect_identical(hits, 2L)
})
