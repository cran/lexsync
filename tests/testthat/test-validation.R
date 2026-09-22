test_that("cohens_d is zero for identical samples and large for separated ones", {
  expect_equal(cohens_d(c(1, 2, 3, 4), c(1, 2, 3, 4)), 0)
  expect_gt(abs(cohens_d(c(5, 6, 7, 8), c(1, 2, 3, 4))), 1)
})

test_that("tost_equiv flags closely matched samples as equivalent", {
  set.seed(1)
  x <- rep(c(4, 5, 6), 20)
  y <- rep(c(4, 5, 6), 20)
  tt <- tost_equiv(x, y, bound_d = 0.5)
  expect_true(isTRUE(tt$equivalent))
})

test_that("tost_equiv defaults to the schema's 0.5 bound", {
  # The bound is the schema's smallest effect size of interest (Lakens, 2017);
  # pinned here because every other test passes bound_d explicitly. The p-value is
  # the same literal asserted in test_validation.py, so the engines cannot drift apart.
  expect_identical(formals(tost_equiv)$bound_d, 0.5)
  expect_equal(round(tost_equiv(10:19, 11:20)$p, 9), 0.354383811)
})

test_that("cohens_d_ci brackets the point estimate and matches cohens_d", {
  x <- rep(c(5, 6, 7, 8), 10); y <- rep(c(1, 2, 3, 4), 10)
  ci <- cohens_d_ci(x, y)
  expect_lte(ci$ci_low, ci$d)
  expect_lte(ci$d, ci$ci_high)
  expect_equal(ci$d, cohens_d(x, y), tolerance = 1e-9)
})

test_that("cohens_d_ci width shrinks as the number of items grows", {
  small <- cohens_d_ci(rep(c(4, 5, 6), 2),  rep(c(4, 5, 7), 2))
  large <- cohens_d_ci(rep(c(4, 5, 6), 40), rep(c(4, 5, 7), 40))
  expect_gt(small$ci_high - small$ci_low, large$ci_high - large$ci_low)
})

test_that("cohens_d_ci within the bound coheres with the TOST verdict", {
  x <- rep(c(4, 5, 6), 20); y <- rep(c(4, 5, 6), 20)
  ci <- cohens_d_ci(x, y, alpha = 0.05)
  within <- ci$ci_low > -0.5 && ci$ci_high < 0.5
  expect_identical(within, isTRUE(tost_equiv(x, y, bound_d = 0.5, alpha = 0.05)$equivalent))
})

test_that("cohens_d_ci is a point for a constant dimension", {
  ci <- cohens_d_ci(c(2, 2, 2, 2), c(2, 2, 2, 2))
  expect_equal(c(ci$d, ci$ci_low, ci$ci_high), c(0, 0, 0))
})

test_that("a zero pooled SD with unequal means is undefined, not zero", {
  # Two conditions each constant at a different value differ by infinitely many
  # SDs: reporting d = 0 (perfect balance) inverted the truth. The equal-constant
  # case is pinned by the zh_freqcontrast golden and must stay exactly zero.
  x <- c(2, 2, 2, 2); y <- c(3, 3, 3, 3)
  expect_true(is.na(cohens_d(x, y)))
  ci <- cohens_d_ci(x, y)
  expect_true(all(is.na(c(ci$d, ci$ci_low, ci$ci_high))))
  expect_false(tost_equiv(x, y)$equivalent)  # already correct on this branch
  expect_equal(variance_ratio(y, x), 1)      # unchanged: both spreads are zero
  expect_equal(cohens_d(x, x), 0)            # equal constants stay exactly zero
})

test_that("match_report carries an undefined d as a missing cell", {
  # Serialised as an empty CSV cell and a null JSON value, exactly like the other
  # missing statistics, and identically in both engines.
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  stim <- data.frame(word = c("aa", "bb", "cc", "dd"),
                     condition = c("a", "a", "b", "b"),
                     length = c(2, 2, 3, 3), stringsAsFactors = FALSE)
  cmp <- match_report(stim, "length", schema)$comparisons
  expect_true(is.na(cmp$cohens_d))
  expect_true(is.na(cmp$d_ci_low))
  expect_true(is.na(cmp$d_ci_high))
  expect_false(cmp$equivalent)
  expect_equal(cmp$var_ratio, 1)
})

test_that("match_report gives a single-condition design an empty comparisons frame", {
  # There is nothing to compare against the anchor, so the frame comes back with its
  # columns and no rows. rbind() over an empty list used to return NULL, which
  # run_pipeline's seq_len(nrow(...)) loop died on with "argument must be coercible
  # to non-negative integer", naming neither the design nor the cause, while the
  # Python engine carried on and wrote a comparisons CSV with no header at all.
  # Pinned identically in tests/test_validation.py.
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  stim <- data.frame(word = c("aa", "bb"), condition = c("only", "only"),
                     length = c(2, 3), stringsAsFactors = FALSE)
  cmp <- match_report(stim, "length", schema)$comparisons
  expect_s3_class(cmp, "data.frame")
  expect_equal(nrow(cmp), 0L)
  expect_identical(names(cmp),
                   c("condition", "reference", "dimension", "cohens_d", "d_ci_low",
                     "d_ci_high", "var_ratio", "tost_p", "equivalent"))
  # The loop that used to die on NULL now runs zero times.
  expect_length(seq_len(nrow(cmp)), 0L)
})

test_that("variance_ratio flags unequal spread", {
  expect_equal(variance_ratio(c(1, 2, 3, 4), c(1, 2, 3, 4)), 1)       # equal spread
  expect_gt(variance_ratio(c(0, 5, 10, 15), c(4, 5, 6, 7)), 1)        # condition wider
  expect_lt(variance_ratio(c(4, 5, 6, 7), c(0, 5, 10, 15)), 1)        # condition narrower
  expect_true(is.na(variance_ratio(c(1, 2, 3), c(5, 5, 5))))          # constant reference
  expect_equal(variance_ratio(c(5, 5, 5), c(2, 2, 2)), 1)             # both constant
})

test_that("match_report_continuous returns predictor and control rows", {
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  stim <- data.frame(word = c("a", "b", "c", "d"), condition = "continuous",
                     frequency = c(2, 3, 4, 5), length = c(3, 4, 3, 4),
                     n_density = c(1, 2, 1, 2), old20 = c(1.5, 1.6, 1.5, 1.6),
                     stringsAsFactors = FALSE)
  rep <- match_report_continuous(stim, "frequency", c("length", "n_density", "old20"), schema)
  expect_true(all(c("descriptives", "comparisons") %in% names(rep)))
  cmp <- rep$comparisons
  expect_identical(names(cmp), c("dimension", "role", "pearson_r", "predictor_span"))
  expect_identical(cmp$role, c("predictor", "control", "control", "control"))
  expect_equal(cmp$predictor_span[1], 3)
})

test_that("describe_stimuli summarises per group", {
  df <- data.frame(condition = c("a", "a", "b", "b"), x = c(1, 2, 3, 4), stringsAsFactors = FALSE)
  d <- describe_stimuli(df, "x")
  expect_true(all(c("group", "dimension", "n", "mean", "sd") %in% names(d)))
  expect_equal(nrow(d), 2)
})

test_that("describe_stimuli groups in order of first appearance", {
  # Not locale-collated order: this is the order validation.py's groupby(sort = FALSE)
  # yields, and the order in which match_report() takes its anchor from unique().
  df <- data.frame(condition = c("b", "b", "a", "a"), x = c(1, 2, 3, 4), stringsAsFactors = FALSE)
  expect_identical(describe_stimuli(df, "x")$group, c("b", "a"))
})

test_that("describe_stimuli reports an all-NA dimension as missing, not infinite", {
  df <- data.frame(condition = c("a", "a"), x = c(NA_real_, NA_real_))
  d <- describe_stimuli(df, "x")
  expect_identical(d$n, 0L)
  expect_true(is.na(d$min))
  expect_true(is.na(d$max))
  expect_false(is.infinite(d$min))
  expect_false(is.infinite(d$max))
})

test_that("balance_check detects imbalance", {
  balanced <- data.frame(condition = c("a", "a", "b", "b"))
  unbalanced <- data.frame(condition = c("a", "a", "a", "b"))
  expect_length(balance_check(balanced, "condition"), 0)
  expect_length(balance_check(unbalanced, "condition"), 1)
})
