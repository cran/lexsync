# Practice and filler blocks, the feedback event, and the per-block event filter.
#
# The property that matters is the split: the experiment runs more trials than the study
# analyses. The stimuli file and the reports are written from the main rows; the generated
# experiments are written from every presented trial. Getting that backwards would either
# put practice items into the realised-control statistics or leave them out of the
# experiment entirely.
#
# Three placements are asserted rather than assumed, because each is a methodological
# choice and not a convenience:
#
# Practice comes strictly first.
#
# Fillers are INTERLEAVED, not appended. A block of fillers at the end is not a filler --
# it is a second block the participant can tell apart. That is why they are merged in
# before the order is drawn.
#
# Feedback is restricted to practice, via `blocks:` on the event, because feedback in the
# task itself would contaminate the reaction times it is measuring.
#
# A design that declares neither block must be untouched, down to not gaining a `block`
# column, so adding this feature moved no existing artefact.
#
# python_workflow/tests/test_blocks.py asserts the same properties.

# Resolved ONCE, at load, because blk_in_repo() changes the working directory: a helper
# that walked up from getwd() would then start from the repository root and go too far.
BLK_REPO <- normalizePath(file.path(getwd(), "..", "..", ".."), mustWork = FALSE)
blk_repo <- function(...) file.path(BLK_REPO, ...)
blk_schema <- function() yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
blk_design_path <- function() blk_repo("config", "design_en_lexdec_blocks.yaml")

blk_main <- function(n = 8L) {
  data.frame(item = seq_len(n), set = seq_len(n),
             condition = rep(c("word", "pseudoword"), n / 2L),
             target = sprintf("w%02d", seq_len(n)),
             answer = rep(c("f", "j"), n / 2L),
             list = 1L, trial = seq_len(n), stringsAsFactors = FALSE)
}

# The design names its item tables relative to the repository root, which is where the
# pipeline runs; the tests run from the package directory, so those that load the tables
# need the same working directory.
blk_in_repo <- function(expr) {
  old <- setwd(BLK_REPO); on.exit(setwd(old), add = TRUE)
  force(expr)
}

blk_feedback_events <- function() {
  list(
    list(type = "text", content = "{target}", duration_ms = 800L),
    list(type = "response", keys = c("f", "j"), timeout_ms = 2000L),
    list(type = "feedback", answer = "answer", duration_ms = 600L,
         blocks = list("practice"))
  )
}

test_that("a design without blocks is untouched", {
  stim <- blk_main()
  out <- lexsync:::.add_blocks(stim, list(name = "x"), blk_schema())
  expect_null(out$report)
  # Not even a block column: adding this feature must move no existing artefact.
  expect_false("block" %in% names(out$presented))
  expect_identical(out$presented, stim)
})

test_that("practice comes first and fillers interleave", {
  skip_if_not(file.exists(blk_design_path()), "repository design absent")
  out <- blk_in_repo(lexsync:::.add_blocks(blk_main(), yaml::read_yaml(blk_design_path()),
                                           blk_schema()))
  p <- out$presented[order(out$presented$trial), , drop = FALSE]
  order_v <- p$block
  first_main <- which(order_v == "main")[1]
  expect_identical(unique(order_v[seq_len(first_main - 1L)]), "practice")
  filler <- which(order_v == "filler"); main <- which(order_v == "main")
  # Mixed through the main trials, not left as a trailing run.
  expect_lt(min(filler), max(main))
  expect_gt(max(filler), min(main))
})

test_that("block sets do not collide with the main ones", {
  # `set` is part of the key the trial-order shuffle hashes, so two rows sharing one
  # would be ordered by a coin the package does not own.
  skip_if_not(file.exists(blk_design_path()), "repository design absent")
  out <- blk_in_repo(lexsync:::.add_blocks(blk_main(), yaml::read_yaml(blk_design_path()),
                                           blk_schema()))
  by <- split(out$presented$set, out$presented$block)
  expect_length(intersect(by$practice, by$main), 0L)
  expect_length(intersect(by$filler, by$main), 0L)
  expect_length(intersect(by$practice, by$filler), 0L)
})

test_that("every block appears in every list", {
  skip_if_not(file.exists(blk_design_path()), "repository design absent")
  stim <- blk_main(); stim$list <- c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L)
  out <- blk_in_repo(lexsync:::.add_blocks(stim, yaml::read_yaml(blk_design_path()),
                                           blk_schema()))
  for (li in unique(out$presented$list)) {
    expect_setequal(unique(out$presented$block[out$presented$list == li]),
                    c("practice", "filler", "main"))
  }
})

test_that("the report names the tables and their checksums", {
  skip_if_not(file.exists(blk_design_path()), "repository design absent")
  out <- blk_in_repo(lexsync:::.add_blocks(blk_main(), yaml::read_yaml(blk_design_path()),
                                           blk_schema()))
  expect_identical(out$report$analysed, "main")
  by <- stats::setNames(out$report$blocks, vapply(out$report$blocks, function(b) b$block,
                                                 character(1)))
  expect_setequal(names(by), c("main", "practice", "filler"))
  for (nm in c("practice", "filler")) {
    expect_identical(nchar(by[[nm]]$sha256), 64L)
    expect_identical(by[[nm]]$n_per_list, 8L)
  }
  expect_true(grepl("interleaved", by$filler$placement, fixed = TRUE))
  expect_true(grepl("before", by$practice$placement, fixed = TRUE))
})

test_that("a block without a path is refused", {
  expect_error(lexsync:::.add_blocks(blk_main(), list(practice = list(n = 4)), blk_schema()),
               "needs a `path`", fixed = TRUE)
})

test_that("the feedback event renders with its texts and restriction", {
  rendered <- lexsync:::render_events(blk_feedback_events(), list(), 60)
  fb <- rendered[[length(rendered)]]
  expect_identical(fb$type, "feedback")
  # `answer` names a loop-table column holding a KEY, so scoring needs no mapping.
  expect_identical(fb$answer, "answer")
  expect_identical(fb$ms, 600L)
  # A timeout is reported separately from a wrong key: on a timed task they mean
  # different things to a participant.
  expect_true(nzchar(fb$correct) && nzchar(fb$incorrect) && nzchar(fb$no_response))
  expect_identical(unlist(fb$blocks), "practice")
  # An unrestricted event carries no filter at all, so nothing changes for designs that
  # do not use blocks.
  expect_null(rendered[[1]]$blocks)
})

test_that("block reaches the loop table", {
  # The runners match an event's restriction against it and watch it for a block
  # boundary, so it has to travel with the trials rather than stay in the stimuli file.
  stim <- blk_main(); stim$block <- "main"
  tab <- lexsync:::loop_table(stim, blk_feedback_events())
  expect_true("block" %in% names(tab))
  # And it is absent when the design has no blocks, so no existing loop table changes.
  expect_false("block" %in% names(lexsync:::loop_table(blk_main(), blk_feedback_events())))
})

test_that("the OpenSesame emitter guards a restricted event and refuses an impossible one", {
  ev <- lexsync:::render_events(blk_feedback_events(), list(), 60)
  blockdef <- lexsync:::.osexp_event_block("ev_fb", ev[[length(ev)]])
  txt <- paste(blockdef$block, collapse = "\n")
  # str, not unicode: OpenSesame 3.3+ runs inline scripts in a Python 3 workspace and
  # does not inject the Python 2 builtin, so the earlier spelling died with NameError on
  # the first trial of any design using a feedback event or a blocks restriction.
  expect_true(grepl("if str(var.get(u'block', u'main')) in [u'practice']:",
                    txt, fixed = TRUE))
  expect_false(grepl("unicode(", txt, fixed = TRUE))
  # var.get(name, None) is indistinguishable from no default in OpenSesame's var_store
  # and RAISES; only a non-None sentinel yields a value.
  expect_false(grepl("var.get(u'response', None)", txt, fixed = TRUE))
  # A keyboard_response is not an inline script, so the guard cannot reach it; saying so
  # beats emitting an experiment that ignores the restriction.
  resp <- ev[[2]]; resp$blocks <- list("practice")
  expect_error(lexsync:::.osexp_event_block("ev_r", resp), "cannot be restricted to blocks")
})

test_that("the jsPsych export carries the filter and the break", {
  stim <- blk_main(); stim$block <- c(rep("practice", 4), rep("main", 4))
  design <- list(name = "f", language = "english", events = blk_feedback_events())
  out <- tempfile("js"); dir.create(out)
  path <- export_jspsych(stim, design, blk_schema(), out, "fb")
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_true(grepl('"blocks":["practice"]', html, fixed = TRUE))
  expect_true(grepl("eventApplies", html, fixed = TRUE))   # the per-trial filter
  expect_true(grepl("block_break", html, fixed = TRUE))    # the boundary screen
  expect_true(grepl("lexsync_scored", html, fixed = TRUE)) # feedback finds ITS response
  expect_false(grepl("{{", html, fixed = TRUE))
})

test_that("a missing value drops the key rather than emitting NaN", {
  # The regression that made the two engines' generated experiments differ. A design
  # whose practice items carry an `answer` but whose main items do not leaves the main
  # rows missing that value. The Python engine embedded a bare NaN into the JSON --
  # which is not valid JSON at all -- while jsonlite dropped the key. Both now drop it,
  # which is also the honest rendering: a trial with no correct answer has none.
  # test_blocks.py pins the same behaviour on the Python side.
  j <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, dataframe = "rows"))
  df <- data.frame(a = c(1, 2), b = c(NA, 5))
  expect_identical(j(df), '[{"a":1},{"a":2,"b":5}]')
  # A whole-number double loses its fractional part, which is why the Python engine has
  # to conform rather than the other way round.
  expect_identical(j(list(a = 2, b = 2.5)), '{"a":2,"b":2.5}')
})


test_that("a feedback event with nothing to score is refused", {
  # With no preceding response or question the OpenSesame runner raises on the unset
  # variable, PsychoPy compares against None and jsPsych finds no scored row: three
  # different confusing run-time failures for one design error that is obvious at
  # generation time.
  ev <- list(list(type = "text", content = "{target}", duration_ms = 800L),
             list(type = "feedback", answer = "answer", duration_ms = 600L))
  expect_error(lexsync:::render_events(ev, list(), 60), "no response or question event")
  # With one, it renders.
  ok <- append(ev, list(list(type = "response", keys = c("f", "j"), timeout_ms = 2000L)),
               after = 1L)
  expect_length(lexsync:::render_events(ok, list(), 60), 3L)
})
