# Balance-aware list assignment (`counterbalance.optimise`).
#
# The factorial recipe deals item sets to lists by rank, which balances nothing: any
# dimension that varies smoothly across set ids is dealt out unevenly, and where each
# list goes to a different group of participants that unevenness is confounded with the
# group. balance_lists() searches for an assignment whose lists are equated instead.
#
# Two properties have to hold, and each is pinned here.
#
# The search must be identical in the two engines, so the objective is all-integer.
# Cohen's d was measured to differ between R and Python by around 3e-16, which at nine
# decimal places leaves roughly a one-in-three chance over a full run of a candidate
# comparison resolving differently -- and the failure would be silent and total,
# different words rather than different last bits. The assignment asserted below is
# pinned character for character against test_balance.py.
#
# It must be off by default. Switching it on changes which items a participant sees, so
# a design that does not ask for it has to be unaffected.
#
# python_workflow/tests/test_balance.py asserts the same properties.

bal_schema <- list(seed = 2026)

# Sets whose properties vary smoothly with set id, which is the case the rank deal
# handles worst: it hands every Nth set to the same list.
bal_mk <- function(n_sets = 16L) {
  s <- rep(seq_len(n_sets), each = 2L)
  data.frame(
    word = sprintf("w%03d%s", s, rep(c("a", "b"), n_sets)),
    set = s,
    condition = rep(c("hi", "lo"), n_sets),
    frequency = 2.0 + 0.31 * s,
    length = 12L - (s %% 7L),
    old20 = 1.0 + 0.07 * ((s * 5L) %% 11L),
    stringsAsFactors = FALSE
  )
}

bal_design <- function() {
  list(name = "b", language = "english",
       match_on = list("frequency", "length", "old20"),
       counterbalance = list(lists = 4L, optimise = TRUE))
}

bal_spread <- function(stim, assign, dim) {
  t <- tapply(stim[[dim]], assign, sum)
  max(t) - min(t)
}

test_that("the assignment is pinned across engines", {
  res <- balance_lists(bal_mk(), bal_design(), bal_schema)
  expect_identical(unname(res$list_of_set),
                   c(1L, 2L, 3L, 4L, 3L, 2L, 1L, 4L, 4L, 3L, 2L, 4L, 1L, 2L, 3L, 1L))
  expect_equal(res$report$cost_before, 88128)
  expect_equal(res$report$cost_after, 33440)
  expect_identical(res$report$n_swaps, 3L)
  expect_false(res$report$max_passes_reached)
  # Whole numbers, not fractions: a float cost is what would let the engines diverge.
  expect_identical(res$report$cost_before, trunc(res$report$cost_before))
  expect_identical(res$report$cost_after, trunc(res$report$cost_after))
})

test_that("every balanced dimension improves on the rank deal", {
  stim <- bal_mk()
  res <- balance_lists(stim, bal_design(), bal_schema)
  sets <- sort(unique(stim$set))
  before <- ((match(stim$set, sets) - 1L) %% 4L) + 1L
  after <- as.integer(res$list_of_set[as.character(stim$set)])
  for (dim in c("frequency", "length", "old20")) {
    expect_lt(bal_spread(stim, after, dim), bal_spread(stim, before, dim))
  }
})

test_that("the search is repeatable and preserves list sizes", {
  a <- balance_lists(bal_mk(), bal_design(), bal_schema)$list_of_set
  b <- balance_lists(bal_mk(), bal_design(), bal_schema)$list_of_set
  expect_identical(a, b)
  # A swap exchanges one set for another, so the rank deal's sizes are invariant.
  expect_identical(sort(as.integer(table(a))), c(4L, 4L, 4L, 4L))
})

test_that("the cost never increases", {
  res <- balance_lists(bal_mk(), bal_design(), bal_schema)
  expect_lte(res$report$cost_after, res$report$cost_before)
})

test_that("optimise is refused where it does not apply", {
  # A Latin square already puts every item in every list, so there is nothing to
  # balance; saying so beats silently doing nothing.
  expect_error(balance_lists(bal_mk(), list(paradigm = "priming",
                                            match_on = list("frequency"),
                                            counterbalance = list(lists = 2L)),
                             bal_schema), "Latin-square")
  expect_error(balance_lists(bal_mk(), list(match_on = list("frequency"),
                                            counterbalance = list(lists = 1L)),
                             bal_schema), "2 or more")
  expect_error(balance_lists(bal_mk(), list(counterbalance = list(lists = 2L)),
                             bal_schema), "no dimensions to balance")
  expect_error(balance_lists(bal_mk(), list(match_on = list("nope"),
                                            counterbalance = list(lists = 2L)),
                             bal_schema), "do not have: 'nope'")
})

test_that("a missing dimension value is refused", {
  # Treating a missing value as zero would bias the objective silently.
  bad <- bal_mk(); bad$frequency[3] <- NA
  expect_error(balance_lists(bad, bal_design(), bal_schema), "missing values")
})

test_that("balance_on defaults to match_on", {
  explicit <- bal_design()
  explicit$counterbalance$balance_on <- list("frequency", "length", "old20")
  expect_identical(balance_lists(bal_mk(), explicit, bal_schema)$list_of_set,
                   balance_lists(bal_mk(), bal_design(), bal_schema)$list_of_set)
})

test_that("counterbalance uses the supplied assignment", {
  stim <- bal_mk()
  res <- balance_lists(stim, bal_design(), bal_schema)
  out <- counterbalance(stim, bal_design(), bal_schema, res$list_of_set)
  expect_identical(out$list, as.integer(res$list_of_set[as.character(out$set)]))
})

test_that("an assignment that misses a set is an error", {
  # Silently defaulting the uncovered sets to list 1 would unbalance the very thing
  # the caller asked to balance.
  stim <- bal_mk()
  sets <- sort(unique(stim$set))
  partial <- stats::setNames(rep(1L, length(sets) - 1L),
                             as.character(sets[-length(sets)]))
  expect_error(lexsync:::counterbalance_factorial(stim, bal_design(), bal_schema, partial),
               "does not cover set")
})

test_that("a design without optimise is dealt by rank", {
  # The default must be untouched: enabling the optimiser changes which items a
  # participant sees, so it cannot happen to a design on upgrade.
  stim <- bal_mk()
  plain <- list(name = "b", language = "english", match_on = list("frequency"),
                counterbalance = list(lists = 4L))
  out <- counterbalance(stim, plain, bal_schema)
  sets <- sort(unique(stim$set))
  deal <- ((match(out$set, sets) - 1L) %% 4L) + 1L
  expect_identical(out$list, deal)
})
