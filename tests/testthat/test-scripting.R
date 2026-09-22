make_stim <- function() {
  stim <- data.frame(
    word = c("cat", "dog", "car", "cap"), condition = c("a", "a", "b", "b"),
    set = c(1, 2, 1, 2), trial = 1:4, length = 3L, frequency = 5,
    n_density = 2L, old20 = 1.5, stringsAsFactors = FALSE
  )
  assign_triggers(stim)
}

test_that("assign_triggers produces valid EEG codes", {
  stim <- make_stim()
  expect_true(all(stim$item_trigger >= 40 & stim$item_trigger <= 239))
  expect_setequal(unique(stim$condition_trigger), c(101, 102))
})

test_that("more than 200 item sets discloses the trigger wrap", {
  stim <- data.frame(condition = "a", set = seq_len(201), stringsAsFactors = FALSE)
  expect_message(
    stim <- assign_triggers(stim),
    "lexsync: 201 item sets exceed the 200-code trigger range; item codes wrap and repeat.",
    fixed = TRUE
  )
  expect_true(all(stim$item_trigger >= 40 & stim$item_trigger <= 239))
  # 201 sets into 200 codes: exactly one code is reused, and it is the first.
  expect_identical(sum(duplicated(stim$item_trigger)), 1L)
  expect_identical(stim$item_trigger[duplicated(stim$item_trigger)], 40L)
})

test_that("exactly 200 item sets stays silent", {
  stim <- data.frame(condition = "a", set = seq_len(200), stringsAsFactors = FALSE)
  expect_silent(stim <- assign_triggers(stim))
  expect_identical(sum(duplicated(stim$item_trigger)), 0L)
})

test_that("response and question timeouts round through the shared rule", {
  # An integer timeout_ms divides to at most three decimals, so rounding is the
  # identity and no committed experiment byte moves.
  r <- render_events(list(list(type = "response", timeout_ms = 1500L)), list(), 60)
  expect_identical(r[[1]]$timeout, 1.5)
  # A fractional timeout goes through .round_dp, the rounder both engines share;
  # 7812.5 ms lands on a 3-dp halfway case where the engines' own rounders differ.
  r <- render_events(list(list(type = "response", timeout_ms = 1500.0005)), list(), 60)
  expect_identical(r[[1]]$timeout, .round_dp(1500.0005 / 1000, 3))
  r <- render_events(list(list(type = "question", timeout_ms = 7812.5)), list(), 60)
  expect_identical(r[[1]]$timeout, .round_dp(7812.5 / 1000, 3))
  expect_false(identical(r[[1]]$timeout, round(7812.5 / 1000, 3)))
})

test_that("PsychoPy export is frame-locked and OpenSesame export is internally consistent", {
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  stim <- make_stim()
  design <- list(name = "t", language = "english", timing = list())
  out <- tempfile("lexsync"); dir.create(out)

  py <- export_psychopy(stim, design, schema, out)
  pytxt <- paste(readLines(py, warn = FALSE), collapse = "\n")
  expect_true(grepl("win.callOnFlip(port.setData", pytxt, fixed = TRUE))
  expect_false(grepl("{{", pytxt, fixed = TRUE))   # all placeholders substituted

  osexp <- export_opensesame(stim, design, schema, out)
  os <- readLines(osexp, warn = FALSE)
  expect_true(any(grepl("set start lexsync_experiment", os, fixed = TRUE)))
  runs <- unique(sub("^\\s*run\\s+(\\S+).*$", "\\1", grep("^\\s*run\\s", os, value = TRUE)))
  defs <- sub("^define\\s+\\S+\\s+(\\S+).*$", "\\1", grep("^define\\s", os, value = TRUE))
  expect_true(all(runs %in% defs))                 # every run resolves to a definition
})

test_that("INTER_TRIGGER_S goes through %.17g like its neighbouring substitutions", {
  # as.character(16.65 / 1000) gives "0.01665" while Python's str() gives
  # "0.016649999999999998", so the substitution is pinned through %.17g in both
  # engines. test_scripting.py asserts these same generated lines.
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  design <- list(name = "t", language = "english", timing = list())
  out <- tempfile("lexsync"); dir.create(out)
  schema$triggers$inter_trigger_ms <- 16.65
  pytxt <- paste(readLines(export_psychopy(make_stim(), design, schema, out), warn = FALSE),
                 collapse = "\n")
  expect_true(grepl("INTER_TRIGGER_S = 0.016649999999999998\n", pytxt, fixed = TRUE))
  # The shipped default still renders as "0.01", so no committed experiment
  # byte moves.
  schema$triggers$inter_trigger_ms <- NULL
  pytxt <- paste(readLines(export_psychopy(make_stim(), design, schema, out), warn = FALSE),
                 collapse = "\n")
  expect_true(grepl("INTER_TRIGGER_S = 0.01\n", pytxt, fixed = TRUE))
})

test_that("the OpenSesame export carries both trigger backends and a test-mode fallback", {
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  os <- paste(readLines(export_opensesame(make_stim(),
                                          list(name = "t", language = "english"),
                                          schema, tempdir()), warn = FALSE), collapse = "\n")
  expect_true(grepl("dlportio", os, fixed = TRUE))     # parallel-port backend
  expect_true(grepl("serial.Serial", os, fixed = TRUE)) # serial backend
  expect_true(grepl("test_mode", os, fixed = TRUE))     # no-hardware fallback
})

test_that("the OpenSesame loop follows the computed trial order", {
  # The loop table is sorted on the seeded `trial` column, and all three targets
  # must present that order; OpenSesame's default (`order random`) would reshuffle.
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  out <- tempfile("lexsync"); dir.create(out)
  os <- readLines(export_opensesame(make_stim(), list(name = "t", language = "english"),
                                    schema, out), warn = FALSE)
  expect_true("\tset order sequential" %in% os)
  expect_false("\tset order random" %in% os)
})

test_that("all targets offset the stimulus before the response window", {
  # The stimulus offsets at its own duration, not at the participant's keypress:
  # PsychoPy flips an empty window and OpenSesame shows an empty canvas, both
  # before the response is collected, matching jsPsych's empty `response` stimulus.
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  design <- list(name = "t", language = "english", timing = list())
  out <- tempfile("lexsync"); dir.create(out)

  pytxt <- paste(readLines(export_psychopy(make_stim(), design, schema, out), warn = FALSE),
                 collapse = "\n")
  expect_true(grepl("        win.flip()\n        keys = event.waitKeys(", pytxt, fixed = TRUE))

  os <- readLines(export_opensesame(make_stim(), design, schema, out), warn = FALSE)
  expect_true("define inline_script lexsync_e2_blank" %in% os)
  expect_true("define keyboard_response lexsync_e2" %in% os)
  expect_identical(grep("^\trun lexsync_e", os, value = TRUE),
                   c("\trun lexsync_e0 always", "\trun lexsync_e1 always",
                     "\trun lexsync_e2_blank always", "\trun lexsync_e2 always",
                     "\trun lexsync_e3 always"))
})

test_that("a per-design font overrides the Latin default for logographic scripts", {
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  stim <- assign_triggers(data.frame(
    word = c("自然", "回来", "三亚", "东盟"),
    condition = c("a", "a", "b", "b"), set = c(1, 2, 1, 2), trial = 1:4,
    length = 2L, frequency = 5, n_density = 80L, old20 = 1.0, stringsAsFactors = FALSE
  ))
  design <- list(name = "zh", language = "chinese", font = "SimHei", timing = list())
  out <- tempfile("lexsync"); dir.create(out)

  pytxt <- paste(readLines(export_psychopy(stim, design, schema, out), warn = FALSE), collapse = "\n")
  expect_true(grepl('WORD_FONT = "SimHei"', pytxt, fixed = TRUE))
  expect_false(grepl("{{", pytxt, fixed = TRUE))

  os <- readLines(export_opensesame(stim, design, schema, out), warn = FALSE)
  expect_true(any(os == "set font_family SimHei"))

  # The default remains a Latin font when no per-design font is given.
  pytxt2 <- paste(readLines(export_psychopy(make_stim(),
                                            list(name = "t", language = "english", timing = list()),
                                            schema, out), warn = FALSE), collapse = "\n")
  expect_true(grepl('WORD_FONT = "Courier New"', pytxt2, fixed = TRUE))
})

# Pins the same verdict as _language_tag in the Python engine's scripting.py on a
# label whose folding the two case mappings disagree about. Base R's tolower()
# applies only the simple mapping, folding U+0130 to a bare "i" so the lookup hits
# and the tag becomes "it"; Python's str.lower() applies the Unicode default full
# mapping, giving i + U+0307, which misses and yields "und". Routing the lookup
# through .lower_invariant() (ICU, root locale) reproduces Python here, and under a
# Turkish or Azeri locale, where tolower("I") would otherwise give a dotless "i"
# and lose "ITALIAN" too. The label is built with intToUtf8 so the source stays
# ASCII (CRAN).
test_that("the BCP 47 lookup folds case as the Python engine does", {
  expect_identical(.language_tag(list(language = "ITALIAN")), "it")
  expect_identical(.language_tag(list(language = paste0(intToUtf8(0x130), "TALIAN"))), "und")
})
