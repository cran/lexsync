# The reproducible reductions.
#
# These exist because the package's byte-identity claim was failing in a way nothing
# caught. Two designs' reported means differed between the engines in the last decimal
# the descriptives publish -- 1.448 against 1.447 -- because R's mean() uses a two-pass
# long-double algorithm while numpy sums pairwise, and the true value sat on a rounding
# boundary. Every reduction in validation.R now goes through these instead.
#
# The algorithm is Neumaier compensated summation, written out in plain double
# arithmetic in both engines: every operation is +, -, abs or a comparison, and IEEE-754
# requires + and - to be correctly rounded, so the two engines cannot disagree. That is
# an argument rather than a measurement, which is what relying on R's long-double
# accumulator amounted to.
#
# python_workflow/tests/test_exact_reductions.py asserts the same properties, and
# test_byte_parity.py checks the end result on the real artefacts.

test_that("the sum is exact where a naive loop is not", {
  # The classic compensated-summation case: a large value followed by many small ones
  # that a naive accumulator loses entirely.
  xs <- c(1e16, rep(1, 100))
  naive <- 0
  for (v in xs) naive <- naive + v
  expect_identical(naive, 1e16)                          # every 1 was swallowed
  expect_identical(lexsync:::.exact_sum(xs), 1e16 + 100) # compensation recovers them
})

test_that("the sum handles catastrophic cancellation", {
  expect_identical(lexsync:::.exact_sum(c(1e-20, 1e20, -1e20, 1e-20)), 2e-20)
  expect_identical(lexsync:::.exact_sum(c(1e100, 1, -1e100)), 1)
})

test_that("order does not change the sum for these cases", {
  # Not a general guarantee -- compensated summation is not order-independent in
  # principle -- but the engines iterate a column in the same order, and this pins that
  # the algorithm is stable for realistic data rather than luckily so.
  xs <- c(3.7, 1.2, 9.9, 0.001, 5.5, 2.25, 8.125)
  expect_identical(lexsync:::.exact_sum(xs), lexsync:::.exact_sum(rev(xs)))
})

test_that("mean and variance on a known case", {
  xs <- c(2, 4, 4, 4, 5, 5, 7, 9)
  expect_identical(lexsync:::.exact_mean(xs), 5)
  expect_equal(lexsync:::.exact_var(xs), 4.571428571428571, tolerance = 0)
  expect_identical(lexsync:::.exact_sd(xs), sqrt(lexsync:::.exact_var(xs)))
})

test_that("degenerate inputs are not silently zero", {
  expect_true(is.na(lexsync:::.exact_mean(numeric(0))))
  # A variance needs two values; returning 0 would claim a spread of nothing.
  expect_true(is.na(lexsync:::.exact_var(1)))
  expect_identical(lexsync:::.exact_var(c(2, 2)), 0)
  expect_identical(lexsync:::.exact_sd(c(2, 2)), 0)
})

test_that("variance survives data far from zero", {
  # The one-pass "sum of squares minus n times squared mean" form cancels
  # catastrophically here and can even return a negative variance; the two-pass form
  # gives the right answer.
  xs <- 1e9 + c(4, 7, 13, 16)
  expect_equal(lexsync:::.exact_var(xs), 30, tolerance = 1e-9)
  expect_gte(lexsync:::.exact_var(xs), 0)
})

test_that("the reductions agree with R's own on well-conditioned data", {
  # Where the built-ins are reliable the replacements must not have changed the answer;
  # the point of the change is the boundary cases, not a different definition.
  set_free <- c(3.65, 5.4, 4.2, 5.3, 2.96, 4.07, 3.715, 6.1)
  expect_equal(lexsync:::.exact_mean(set_free), mean(set_free), tolerance = 1e-15)
  expect_equal(lexsync:::.exact_var(set_free), stats::var(set_free), tolerance = 1e-15)
  expect_equal(lexsync:::.exact_sd(set_free), stats::sd(set_free), tolerance = 1e-15)
})

test_that("the median is the sorted middle, in plain double arithmetic", {
  # stats::median averages the two middle values through mean()'s long-double
  # accumulator, the one published reduction that bypassed the exact primitives;
  # (a + b) / 2 is one correctly-rounded addition and one exact halving, so both
  # engines compute the same double. test_exact_reductions.py pins these values.
  expect_identical(lexsync:::.exact_median(c(3, 1, 2)), 2)
  expect_identical(lexsync:::.exact_median(c(0.1, 0.2)), (0.1 + 0.2) / 2)
  expect_identical(lexsync:::.exact_median(c(4, 1, 3, 2)), 2.5)
  # Missing values are dropped, and an empty vector is NA rather than an error.
  expect_identical(lexsync:::.exact_median(c(1, NA, 3)), 2)
  expect_true(is.na(lexsync:::.exact_median(numeric(0))))
  # Where stats::median is reliable the replacement must agree with it.
  set_free <- c(3.65, 5.4, 4.2, 5.3, 2.96, 4.07, 3.715, 6.1)
  expect_equal(lexsync:::.exact_median(set_free), stats::median(set_free),
               tolerance = 1e-15)
  expect_equal(lexsync:::.exact_median(set_free[-1]), stats::median(set_free[-1]),
               tolerance = 1e-15)
})


test_that("the shared decimal rounder is pinned", {
  # No pairing of built-ins agrees. Measured over 210,000 values including every 3-dp
  # halfway case in range: R's round() disagrees with Python's builtin round(), Python's
  # builtin disagrees with numpy's, and even R's sprintf("%.3f") disagrees with Python's
  # "%.3f" on 274 of them. The rounder is defined by its arithmetic instead, and both
  # engines compute the same double by construction. test_exact_reductions.py asserts
  # these same values.
  expect_identical(lexsync:::.round_dp(7.8125, 3), 7.813)   # half AWAY from zero
  expect_identical(lexsync:::.round_dp(-7.8125, 3), -7.813) # and away for negatives
  expect_identical(lexsync:::.round_dp(2.5, 0), 3)
  expect_identical(lexsync:::.round_dp(-2.5, 0), -3)
  expect_identical(lexsync:::.round_dp(1.0005, 3), 1.001)
  expect_identical(lexsync:::.round_dp(4.2505, 3), 4.251)
  expect_identical(lexsync:::.round_dp(0, 3), 0)
  # R's own round() would give the half-to-even answer on the first of these, which is
  # exactly the divergence this replaces.
  expect_false(identical(round(7.8125, 3), lexsync:::.round_dp(7.8125, 3)))
  # Vectorised, and non-finite values pass through rather than becoming nonsense.
  expect_identical(lexsync:::.round_dp(c(1.0005, 2.0005), 3), c(1.001, 2.001))
  expect_true(is.na(lexsync:::.round_dp(NA_real_, 3)))
  expect_identical(lexsync:::.round_dp(Inf, 3), Inf)
  # The Python engine now carries a vectorised twin, _round_dp_vec, and
  # test_the_vectorised_rounder_matches_the_scalar_elementwise asserts these same
  # values elementwise; here the vectorised call is the function itself, so one
  # mixed vector pins the pinned cases and the non-finite pass-through together.
  expect_identical(
    lexsync:::.round_dp(c(7.8125, -7.8125, 1.0005, 4.2505, 0, NA_real_, Inf, -Inf), 3),
    c(7.813, -7.813, 1.001, 4.251, 0, NA_real_, Inf, -Inf))
  # A finite input whose scaled value overflows passes through unchanged; the
  # Python scalar _round_dp pins the same values.
  expect_identical(lexsync:::.round_dp(1e306, 3), 1e306)
  expect_identical(lexsync:::.round_dp(-1e306, 3), -1e306)
})

# ---- The CSV writer's magnitude limits -------------------------------------

test_that("the writer accepts what readr and Python render identically", {
  # Each expected string is readr's own output, asserted so that a readr change breaks
  # this suite as well as the Python one. test_exact_reductions.py pins the same values.
  path <- file.path(tempdir(), "ok.csv")
  x <- data.frame(v = c(5e14, 999999999999999, 100000000000000.5, 562949953421312.5))
  expect_silent(write_csv_utf8(x, path))
  expect_equal(readLines(path)[-1],
               c("500000000000000", "999999999999999",
                 "100000000000000.5", "562949953421312.5"))
})

test_that("a magnitude the two engines cannot agree on is refused", {
  # readr writes 1.5e16 as "15e15" with an integer mantissa, the largest double as
  # "17976931348623157e292", and the double nearest 5e22 as "4.9999999999999996e+22".
  # No rule fits all three, so Python cannot reproduce the layout and the value is
  # refused in BOTH engines rather than accepted by one and rejected by the other.
  path <- file.path(tempdir(), "big.csv")
  for (v in c(1e15, 1.5e16, 5e22, -1e15)) {
    expect_error(write_csv_utf8(data.frame(norm = c(1, v)), path),
                 "too large to write identically", fixed = TRUE)
  }
})

test_that("a value with two shortest decimal forms is refused", {
  # 1000000000000000.25 round-trips from both "...0.3" (readr's choice) and "...0.2"
  # (Python's). The digits differ, so no reformatting reconciles them.
  expect_error(
    write_csv_utf8(data.frame(v = 1000000000000000.25),
                   file.path(tempdir(), "tie.csv")),
    "too large to write identically", fixed = TRUE)
})

test_that("the ambiguity guard bites below 1e15 too, exactly where Python's does", {
  # The literal 844424930131968.2 parses to the double ...968.25, whose rounding
  # interval holds both "...968.2" and "...968.3": two shortest forms, of which
  # readr prints one and Python's repr the other. The old test derived the digit
  # count from format(t, digits = 15), which never shows a fractional digit in
  # [2^49, 1e15), so R accepted every value Python's _readr_cell refused here.
  # test_exact_reductions.py refuses and accepts these same values.
  path <- file.path(tempdir(), "tie_band.csv")
  expect_error(write_csv_utf8(data.frame(v = 844424930131968.2), path),
               "more than one shortest decimal form", fixed = TRUE)
  expect_error(write_csv_utf8(data.frame(v = -844424930131968.2), path),
               "more than one shortest decimal form", fixed = TRUE)
  # Unambiguous fractions in the same band still pass, exactly as before.
  expect_silent(write_csv_utf8(data.frame(v = 844424930131968.5), path))
  expect_silent(write_csv_utf8(data.frame(v = 562949953421312.5), path))
})
