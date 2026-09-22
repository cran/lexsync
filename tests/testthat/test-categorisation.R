# The cued-categorisation paradigm, and the type trap it exposed.
#
# Two things are pinned here.
#
# The paradigm rotates rather than crossing. Each item carries a narrow cue and a broad
# one, so the factorial recipe would hand a participant the same target twice and the
# second presentation would be a repetition-priming trial rather than a categorisation
# trial. That was the first version's actual behaviour, caught by counting targets per
# list, so it is asserted rather than assumed.
#
# `answer` survives as text. It holds the response KEY, and the natural coding for a
# two-choice task is `f` and `j` -- which readr reads as the LOGICAL value FALSE while
# pandas keeps the string. A design's correct answer was therefore silently turned into
# FALSE in this engine. Measured on readr 2.2.0: f, t, T and F infer as logical; j, y and
# n do not, which is the worst possible split because f/j and t/f are the two commonest
# key pairs in the field.
#
# python_workflow/tests/test_categorisation.py asserts the same properties.

cat_repo <- function(...) {
  # The tests run with the installed package, so the repository is two levels up from
  # the R package directory rather than reachable from the library path.
  file.path(normalizePath(file.path(getwd(), "..", "..", "..")), ...)
}
cat_items <- function() cat_repo("items", "categorisation_en.csv")
cat_design_path <- function() cat_repo("config", "design_en_categorisation.yaml")
cat_schema <- function() yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))

test_that("the paradigm is registered with the fields it needs", {
  p <- PARADIGMS[["categorisation"]]
  expect_false(is.null(p))
  # `answer` is required, so a categorisation experiment cannot be generated without a
  # way to score it.
  expect_identical(p$stimulus_fields, c("target", "category", "answer"))
  types <- vapply(p$events, function(e) e$type, character(1))
  # The cue is a trial event, not one-off instructions: the category varies by trial.
  expect_identical(types, c("fixation", "text", "text", "response", "blank"))
  expect_identical(p$events[[2]]$content, "{category}")
  expect_identical(p$events[[3]]$content, "{target}")
  # Only the target onset carries the condition marker; the cue is not the stimulus.
  expect_identical(p$events[[3]]$trigger, "condition")
  expect_null(p$events[[2]]$trigger)
})

test_that("the paradigm rotates rather than crossing", {
  # With the factorial recipe a participant sees each target under both cues, and the
  # second is a repetition trial. Asserted because the first version got it wrong.
  expect_identical(PARADIGMS[["categorisation"]]$counterbalance, "latin_square_target")
})

test_that("required_fields include the cue and the answer", {
  design <- list(paradigm = "categorisation", name = "c", language = "english")
  expect_true(all(c("target", "category", "answer") %in% required_fields(design)))
})

test_that("the answer key is read as text, not a boolean", {
  skip_if_not(file.exists(cat_items()), "repository item table absent")
  design <- yaml::read_yaml(cat_design_path())
  items <- load_items(cat_items(), required_fields(design))
  expect_identical(unique(items$answer), "f")
  expect_type(items$answer, "character")
  # The whole point: not "FALSE" and not the logical FALSE.
  expect_false("FALSE" %in% items$answer)
})

test_that("each target appears once per list", {
  skip_if_not(file.exists(cat_items()), "repository item table absent")
  design <- yaml::read_yaml(cat_design_path())
  schema <- cat_schema()
  items <- load_items(cat_items(), required_fields(design))
  out <- counterbalance(items, design, schema)
  per <- table(out$list, out$target)
  expect_true(all(per <= 1L))
  # And the two cues stay balanced within each list.
  expect_equal(length(unique(as.integer(table(out$list, out$condition)))), 1L)
})

test_that("the jsPsych export carries the cue and the answer", {
  # jsPsych is generated from the same event list as the other two targets, so the new
  # paradigm needs no browser-specific code -- but "needs none" is a claim, and this is
  # what checks it.
  skip_if_not(file.exists(cat_items()), "repository item table absent")
  design <- yaml::read_yaml(cat_design_path())
  schema <- cat_schema()
  items <- load_items(cat_items(), required_fields(design))
  stim <- counterbalance(items, design, schema)
  out <- tempfile("js"); dir.create(out)
  path <- export_jspsych(stim, design, schema, out, "cat")
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_true(grepl('"category"', html, fixed = TRUE))
  expect_true(grepl('"answer"', html, fixed = TRUE))
  expect_true(grepl("A BIRD?", html, fixed = TRUE))
  expect_true(grepl('"answer":"f"', html, fixed = TRUE))
  # Two text events, so the cue is presented as well as the target.
  expect_equal(lengths(regmatches(html, gregexpr('"type":"text"', html, fixed = TRUE))), 2L)
  # And nothing was left unsubstituted.
  expect_false(grepl("{{", html, fixed = TRUE))
})

test_that("the rendered events use milliseconds", {
  d <- list(paradigm = "categorisation", name = "c", language = "english")
  rendered <- lexsync:::render_events(resolve_events(d), list(), 60)
  expect_identical(vapply(rendered, function(r) r$type, character(1)),
                   c("fixation", "text", "text", "response", "blank"))
  expect_identical(rendered[[1]]$ms, 500L)
  expect_identical(rendered[[2]]$ms, 750L)
  expect_identical(rendered[[2]]$field, "category")
  expect_identical(rendered[[3]]$ms, 800L)
  expect_identical(rendered[[3]]$field, "target")
  expect_equal(rendered[[4]]$timeout, 2.5)
  expect_identical(rendered[[5]]$ms, 250L)
})

test_that("readr's logical inference is neutralised", {
  # The general form of the bug, on the tokens that trigger it.
  p <- tempfile(fileext = ".csv")
  writeLines(c("item,condition,target,category,answer",
               "1,a,dog,AN ANIMAL?,f", "2,b,cat,AN ANIMAL?,t"), p)
  items <- load_items(p, c("target", "category", "answer"))
  expect_identical(items$answer, c("f", "t"))
  expect_identical(items$condition, c("a", "b"))
})
