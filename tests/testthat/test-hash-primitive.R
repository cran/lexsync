# The keyed-hash primitive, which every deterministic-but-varying value rests on.
#
# lexsync draws no random numbers. Anything that looks stochastic -- a jittered
# duration, and any future search needing a candidate order -- is a pure function
# of a SHA-256 digest of a key string. `hash_unit` turns that digest into a
# uniform variate in [0, 1), and it has to give the *same bits* in R and Python or
# the two engines will select different stimuli.
#
# The golden digests below were frozen only after both engines were compared
# bit-for-bit over 20,005 keys, including accented Spanish and CJK strings and the
# empty key. python_workflow/tests/test_hash_primitive.py computes the same
# digests independently, so the two suites fail together if either engine drifts.

two52 <- 4503599627370496

# The IEEE-754 bit pattern, so a one-bit difference cannot hide in rounding.
bits_of <- function(x) {
  vapply(x, function(v) paste(rev(sprintf("%02x", as.integer(writeBin(v, raw(), size = 8)))),
                              collapse = ""), character(1), USE.NAMES = FALSE)
}

test_that("the variate lies strictly inside the unit interval", {
  u <- lexsync:::hash_unit(sprintf("sweep|%d", 0:4999))
  expect_true(all(u >= 0 & u < 1))
  # 13 hex digits cap the variate one ulp below 1, which is what stops
  # lo + floor(u * n) from ever returning hi + 1.
  expect_lte(max(u), 1 - 1 / two52)
})

test_that("known keys, including non-ASCII, match the Python engine", {
  expect_equal(bits_of(lexsync:::hash_unit("sweep|0")), "3fcd8a2fd4c97bd0")
  expect_equal(lexsync:::hash_int_range("42|0|A|1|niño_corazón", 200L, 800L), 302L)
  expect_equal(lexsync:::hash_int_range("42|0|A|1|北京大学汉字", 200L, 800L), 209L)
})

test_that("the integer mapping respects both bounds", {
  for (rng in list(c(0L, 1L), c(1L, 3L), c(200L, 800L), c(0L, 0L), c(-5L, 5L))) {
    v <- lexsync:::hash_int_range(sprintf("k|%d", 0:2999), rng[1], rng[2])
    expect_gte(min(v), rng[1])
    expect_lte(max(v), rng[2])
  }
  # A degenerate range is not an error; it is a constant.
  expect_equal(lexsync:::hash_int_range("anything", 7L, 7L), 7L)
  expect_error(lexsync:::hash_int_range("k", 10L, 2L), "hi >= lo")
})

test_that("the variate stream matches the Python engine", {
  stream <- paste(bits_of(lexsync:::hash_unit(sprintf("sweep|%d", 0:19999))), collapse = "|")
  expect_equal(digest::digest(stream, algo = "sha256", serialize = FALSE),
               "8c1dabf515645b54c85e10529275bf0920ba965708f0d115a9c5a3b810d800f3")
})

test_that("the integer stream matches the Python engine", {
  v <- lexsync:::hash_int_range(sprintf("sweep|%d", 0:19999), 200L, 800L)
  stream <- paste(v, collapse = "|")
  expect_equal(digest::digest(stream, algo = "sha256", serialize = FALSE),
               "170c356c58314548ca0d2bffc31ac4da56f960d5037dc05ca78234dcb045ad97")
})

test_that("distinct keys do not collide in practice", {
  # Not a uniformity proof, only a guard against a broken derivation: a truncated
  # or constant digest would collapse the range, which is worth catching cheaply.
  u <- lexsync:::hash_unit(sprintf("sweep|%d", 0:19999))
  expect_equal(length(unique(u)), 20000L)
})
