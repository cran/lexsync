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

## ----install, eval = FALSE----------------------------------------------------
# install.packages("lexsync")

## ----setup--------------------------------------------------------------------
library(lexsync)
schema <- yaml::read_yaml(
  system.file("extdata", "schema.yaml", package = "lexsync")
)
lex <- load_lexicon(
  system.file("extdata", "en_example.csv", package = "lexsync"),
  schema, language = "english"
)
nrow(lex)

## ----design-------------------------------------------------------------------
design <- list(
  name = "vignette_demo", language = "english", n_per_condition = 15,
  pool_filters = list(length = c(3, 7), frequency = c(3.8, 7)),
  conditions = list(
    list(name = "high", define_by = list(frequency = c(5.2, 7.0))),
    list(name = "low",  define_by = list(frequency = c(3.8, 4.4)))
  ),
  match_on = list("length", "n_density", "old20")
)
pool <- build_pool(lex, design$pool_filters)
stim <- match_stimuli(pool, design, schema)
head(stim[, c(
  "word", "condition", "length", "frequency", "n_density", "old20"
)])

## ----report-------------------------------------------------------------------
report <- match_report(
  stim, c("length", "frequency", "n_density", "old20"), schema
)
knitr::kable(
  report$descriptives,
  caption = "Descriptive statistics per condition"
)
knitr::kable(
  report$comparisons,
  caption = paste(
    "Standardised mean differences with 90% confidence intervals, plus the",
    "complementary TOST equivalence test"
  )
)

## ----export-------------------------------------------------------------------
out <- file.path(tempdir(), "lexsync_demo")
dir.create(out, showWarnings = FALSE)
files <- export_experiments(stim, design, schema, out)
basename(unlist(files))

