# Per-trial event durations: jittered, or read from an item column.
#
# A fixed duration is a property of the design; one that varies from trial to
# trial is a property of the trial, and therefore a variable the analysis needs.
# Both forms are resolved into the stimuli table before anything is generated, so
# the realised milliseconds travel with the stimuli rather than living only inside
# the presentation script.
#
# The jitter draws no random number. It is a uniform integer keyed on the seed,
# the column name, the list, the set and the condition, so the two engines realise
# the same milliseconds and a rerun reproduces them. Naming the column in the key
# is what makes two jittered events in one design draw independently instead of
# sharing a value.
#
# python_workflow/tests/test_trial_timing.py asserts the same values.

schema <- list(seed = 42L)

events <- list(
  list(type = "fixation", content = "+", duration_ms = 500L),
  list(type = "text", content = "{prime}", duration = list(from_column = "soa_ms"), trigger = 20L),
  list(type = "text", content = "{target}", duration_ms = 800L, trigger = "condition"),
  list(type = "blank", duration = list(jitter = c(400L, 800L), as = "isi_ms"))
)

stim <- data.frame(
  set = 1:4,
  condition = c("related", "unrelated", "related", "unrelated"),
  list = c(1L, 1L, 2L, 2L),
  prime = c("cat", "sun", "dog", "car"),
  target = c("pet", "sky", "pet", "van"),
  soa_ms = c(60L, 250L, 1200L, 60L),
  trial = 1:4,
  stringsAsFactors = FALSE
)

test_that("a jittered duration lands in the stimuli table within its range", {
  out <- resolve_trial_timing(stim, list(events = events), schema)
  expect_true("isi_ms" %in% names(out))
  expect_true(all(out$isi_ms >= 400 & out$isi_ms <= 800))
  # Deterministic: the same inputs give the same milliseconds every time.
  expect_equal(out$isi_ms, resolve_trial_timing(stim, list(events = events), schema)$isi_ms)
})

test_that("the realised values match the Python engine", {
  out <- resolve_trial_timing(stim, list(events = events), schema)
  expect_equal(out$isi_ms, c(537L, 583L, 764L, 594L))
})

test_that("the seed changes the draw", {
  a <- resolve_trial_timing(stim, list(events = events), list(seed = 42L))$isi_ms
  b <- resolve_trial_timing(stim, list(events = events), list(seed = 43L))$isi_ms
  expect_false(identical(a, b))
})

test_that("two jittered events draw independently", {
  ev <- list(
    list(type = "blank", duration = list(jitter = c(0L, 1000L), as = "gap_a_ms")),
    list(type = "blank", duration = list(jitter = c(0L, 1000L), as = "gap_b_ms"))
  )
  out <- resolve_trial_timing(stim, list(events = ev), schema)
  # Sharing a key would make these identical; the column name is in the key.
  expect_false(identical(out$gap_a_ms, out$gap_b_ms))
  expect_equal(out$gap_a_ms, c(76L, 51L, 593L, 690L))
  expect_equal(out$gap_b_ms, c(460L, 499L, 442L, 125L))
})

test_that("a column duration is referenced rather than baked in", {
  rendered <- render_events(events, list(), 60)
  expect_equal(rendered[[1]]$ms, 500L)              # fixed, resolved at build time
  expect_equal(rendered[[2]]$ms_column, "soa_ms")   # per trial, resolved at run time
  # [[ ]], not $: `$` would partial-match `ms_column` and return it.
  expect_null(rendered[[2]][["ms"]])
  expect_equal(rendered[[4]]$ms_column, "isi_ms")
})

test_that("the loop table carries every per-trial duration column", {
  out <- resolve_trial_timing(stim, list(events = events), schema)
  tab <- lexsync:::loop_table(out, events)
  expect_true(all(c("soa_ms", "isi_ms") %in% names(tab)))
})

test_that("a missing source column is an error, not a silent default", {
  ev <- list(list(type = "blank", duration = list(from_column = "not_there")))
  expect_error(resolve_trial_timing(stim, list(events = ev), schema),
               "which the items do not have")
})

test_that("a malformed duration block is rejected", {
  expect_error(render_events(list(list(type = "blank", duration = list(jitter = 400L))), list(), 60),
               "two-element range")
  expect_error(render_events(list(list(type = "blank", duration = list(nonsense = 1L))), list(), 60),
               "from_column' or 'jitter'")
})

test_that("an unnamed jitter column falls back to the event index", {
  ev <- list(type = "blank", duration = list(jitter = c(10L, 20L)))
  expect_equal(lexsync:::.duration_spec(ev, 2L)$column, "event2_ms")
})

test_that("key parts render integral doubles the way Python does", {
  # The hazard this guards: a pandas column promoted to float64 by one missing
  # value would otherwise key on "3.0" in Python and "3" in R.
  expect_equal(lexsync:::.key_part(3L), "3")
  expect_equal(lexsync:::.key_part(3.0), "3")
  expect_equal(lexsync:::.key_part(-7.0), "-7")
  expect_equal(lexsync:::.key_part("related"), "related")
  expect_equal(lexsync:::.key_part(2.5), "2.5")
})


test_that("a hash-key component that cannot be rendered identically is refused", {
  # Measured divergence before this: a missing value rendered "NA" here and "nan" in
  # Python, TRUE/FALSE against True/False, Inf against inf. A blank `condition` cell is a
  # routine data error that neither reader rejects, and it produced a DIFFERENT trial
  # order in each engine -- reproducibly, with nothing to signal it.
  expect_error(lexsync:::.key_part(NA), "missing")
  expect_error(lexsync:::.key_part(c(1, NA)), "missing")
  expect_error(lexsync:::.key_part(NA_character_), "missing")
  expect_error(lexsync:::.key_part(Inf), "not finite")
  # Booleans are legitimate, so they get a pinned spelling rather than a refusal.
  # test_trial_timing.py asserts the same two strings.
  expect_identical(lexsync:::.key_part(TRUE), "TRUE")
  expect_identical(lexsync:::.key_part(FALSE), "FALSE")
  # A value past the integer range would make as.integer() return NA, which would put
  # an empty component into the key.
  expect_identical(lexsync:::.key_part(3e9), "3000000000")
})
