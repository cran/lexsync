## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "",
  R.options = list(
    cli.num_colors = 1,
    cli.hyperlink = FALSE,
    crayon.enabled = FALSE,
    width = 80
  )
)
# Console colour carries no meaning on a rendered page. pkgdown turns it on for
# its own build, and the escape sequences then reach the reader as literal text,
# so colour is switched off here for a plain vignette render and a site build
# alike. The fixed width keeps printed output inside the documentation column.

## ----setup--------------------------------------------------------------------
library(lexsync)
schema <- yaml::read_yaml(
  system.file("extdata", "schema.yaml", package = "lexsync")
)
lex <- load_lexicon(
  system.file("extdata", "en_example.csv", package = "lexsync"),
  schema, language = "english"
)
pool <- build_pool(lex, list(length = c(3, 7), frequency = c(3.8, 7.0)))

design <- list(
  name = "vignette_experiment", language = "english", n_per_condition = 12,
  paradigm = "factorial",
  conditions = list(
    list(name = "high", define_by = list(frequency = c(5.2, 7.0))),
    list(name = "low",  define_by = list(frequency = c(3.8, 4.4)))
  ),
  match_on = list("length", "n_density", "old20"),
  counterbalance = list(lists = 1)
)
stim <- match_stimuli(pool, design, schema)
stim <- counterbalance(stim, design, schema)
head(stim[, c("trial", "list", "set", "condition", "word")], 3)

## ----events-------------------------------------------------------------------
events <- resolve_events(design)
str(events)

## ----content------------------------------------------------------------------
lapply(events[1:2], function(ev) ev$content)

## ----required-fields----------------------------------------------------------
required_fields(design)
required_fields(list(paradigm = "priming"))
required_fields(list(paradigm = "self_paced_reading"))

## ----triggers-in-events-------------------------------------------------------
priming_events <- PARADIGMS$priming$events
or_dash <- function(x) if (is.null(x)) "-" else as.character(x)
data.frame(
  type = vapply(priming_events, function(e) e$type, character(1)),
  content = vapply(
    priming_events, function(e) or_dash(e$content), character(1)
  ),
  trigger = vapply(priming_events, function(e) or_dash(e$trigger), character(1))
)

## ----assign-triggers----------------------------------------------------------
trig <- assign_triggers(stim)
unique(trig[, c("condition", "condition_trigger")])
range(trig$item_trigger)

## ----assign-triggers-order----------------------------------------------------
unique(assign_triggers(stim, conditions = c("low", "high"))[
  , c("condition", "condition_trigger")])

## ----registry-----------------------------------------------------------------
names(PARADIGMS)
data.frame(
  paradigm = names(PARADIGMS),
  fields = vapply(
    PARADIGMS,
    function(p) paste(p$stimulus_fields, collapse = ", "),
    character(1)
  ),
  counterbalance = vapply(
    PARADIGMS, function(p) p$counterbalance, character(1)
  ),
  n_events = vapply(PARADIGMS, function(p) length(p$events), integer(1)),
  row.names = NULL
)

## ----counterbalance-recipes---------------------------------------------------
table(list = stim$list, condition = stim$condition)

## ----export-------------------------------------------------------------------
out <- file.path(tempdir(), "lexsync_experiment")
dir.create(out, showWarnings = FALSE)
files <- export_experiments(stim, design, schema, out)
basename(unlist(files))

## ----export-dir---------------------------------------------------------------
list.files(out)

## ----loop-table---------------------------------------------------------------
psychopy_csv <- file.path(out, "vignette_experiment_english_psychopy.csv")
names(read.csv(psychopy_csv))

## ----trigger-settings---------------------------------------------------------
str(schema$triggers)

## ----custom-events------------------------------------------------------------
custom <- design
custom$name <- "vignette_custom"
custom$events <- list(
  list(type = "fixation", content = "+", duration_frames = 30L),
  list(type = "text", content = "{word}", duration_frames = 12L,
       trigger = 30L, onset_locked = TRUE),
  list(type = "mask", content = "#####", duration_frames = 6L),
  list(type = "text", content = "{word}", duration_frames = 48L,
       trigger = "condition", onset_locked = TRUE),
  list(type = "response", keys = c("left", "right"), timeout_ms = 2000L),
  list(type = "blank", duration_frames = 15L)
)
required_fields(custom)
custom_files <- export_experiments(stim, custom, schema, out)
basename(unlist(custom_files))

