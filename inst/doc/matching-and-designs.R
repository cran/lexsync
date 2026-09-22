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
head(
  lex[, c("word", "frequency", "length", "n_syllables", "n_density", "old20")],
  4
)

## ----dimensions---------------------------------------------------------------
dims <- schema$dimensions
knitr::kable(data.frame(
  dimension = names(dims),
  type = vapply(dims, function(d) d$type, character(1)),
  unit = vapply(dims, function(d) d$unit, character(1)),
  row.names = NULL
), caption = "The dimensions declared in schema.yaml")

## ----syllables----------------------------------------------------------------
count_syllables(c("cat", "table", "rhythm", "being", "chocolate"))

## ----neighbourhood------------------------------------------------------------
sample_words <- lex[lex$word %in% c("cat", "dog", "house", "rhythm"), ]
add_neighbourhood(
  sample_words, reference = lex$word
)[, c("word", "n_density", "old20")]

## ----norms--------------------------------------------------------------------
aoa <- data.frame(word = c("cat", "dog", "house"), aoa = c(3.1, 3.0, 3.4))
merged <- merge_norms(lex, aoa)
merged[merged$word %in% aoa$word, c("word", "frequency", "aoa")]

## ----pool---------------------------------------------------------------------
pool_filters <- list(length = c(3, 7), frequency = c(3.8, 7.0))
pool <- build_pool(lex, pool_filters)
c(lexicon = nrow(lex), pool = nrow(pool))

## ----design-------------------------------------------------------------------
design <- list(
  name = "vignette_matching", language = "english", n_per_condition = 20,
  pool_filters = pool_filters,
  conditions = list(
    list(name = "high", define_by = list(frequency = c(5.2, 7.0))),
    list(name = "low",  define_by = list(frequency = c(3.8, 4.4)))
  ),
  match_on = list("length", "n_density", "old20")
)
stim <- match_stimuli(pool, design, schema)
head(
  stim[
    stim$set %in% 1:3,
    c("set", "condition", "word", "frequency", "length", "old20")
  ],
  6
)

## ----correlations-------------------------------------------------------------
round(cor(pool[, c("length", "n_density", "old20")]), 2)

## ----methods------------------------------------------------------------------
methods <- c("standardised_euclidean", "joint", "mahalanobis")
if (requireNamespace("clue", quietly = TRUE)) methods <- c(methods, "optimal")

summarise_method <- function(method) {
  d <- design
  d$matching <- list(method = method)
  s <- match_stimuli(pool, d, schema)
  cmp <- match_report(
    s, c("frequency", "length", "n_density", "old20"), schema
  )$comparisons
  controls <- cmp[cmp$dimension != "frequency", ]
  high <- s$frequency[s$condition == "high"]
  low  <- s$frequency[s$condition == "low"]
  data.frame(
    method = method,
    worst_control_d = round(max(abs(controls$cohens_d)), 3),
    freq_d = round(cmp$cohens_d[cmp$dimension == "frequency"], 2),
    freq_raw_diff = round(mean(high) - mean(low), 2),
    freq_sd_high = round(sd(high), 2)
  )
}
do.call(rbind, lapply(methods, summarise_method))

## ----tolerance----------------------------------------------------------------
str(schema$matching$tolerance_k)

## ----tolerance-override-------------------------------------------------------
tight <- design
tight$matching <- list(tolerance_k = list(old20 = 0.25))
tight_stim <- match_stimuli(pool, tight, schema, verbose = TRUE)
round(match_report(tight_stim, "old20", schema)$comparisons$cohens_d, 3)

## ----tolerance-relaxed--------------------------------------------------------
too_tight <- design
too_tight$matching <- list(tolerance_k = list(old20 = 0.05))
relaxed_stim <- match_stimuli(pool, too_tight, schema, verbose = TRUE)

## ----continuous---------------------------------------------------------------
cont_design <- list(
  name = "vignette_continuous", language = "english", n_per_condition = 60,
  pool_filters = pool_filters,
  continuous = list(
    predictor = "frequency",
    controls = c("length", "n_density", "old20")
  ),
  match_on = list("length", "n_density", "old20"),
  matching = list(
    tolerance_k = list(length = 1.5, n_density = 1.5, old20 = 1.5)
  )
)
cont <- select_continuous_stimuli(pool, cont_design, schema)
nrow(cont)
range(cont$frequency)

## ----continuous-report--------------------------------------------------------
cont_report <- match_report_continuous(
  cont, "frequency",
  c("length", "n_density", "old20"), schema
)
cont_report$comparisons

## ----report-desc--------------------------------------------------------------
report <- match_report(
  stim, c("frequency", "length", "n_density", "old20"), schema
)
knitr::kable(
  report$descriptives,
  caption = "Descriptive statistics per condition"
)

## ----report-comp--------------------------------------------------------------
knitr::kable(
  report$comparisons,
  caption = paste(
    "Realised control: standardised differences with 90% intervals,",
    "variance ratios and TOST verdicts"
  )
)

## ----equivalence-settings-----------------------------------------------------
str(schema$equivalence)

## ----direct-stats-------------------------------------------------------------
high <- stim$old20[stim$condition == "high"]
low  <- stim$old20[stim$condition == "low"]
str(cohens_d_ci(high, low))
str(tost_equiv(high, low, bound_d = 0.5))
round(variance_ratio(low, high), 3)

## ----balance------------------------------------------------------------------
balance_check(stim, "condition")
balance_check(rbind(stim, stim[1, ]), "condition")

## ----lexdec-------------------------------------------------------------------
lexdec_pool <- build_pool(lex, list(length = c(4, 7), frequency = c(3.5, 6.0)))
stim_lexdec <- build_lexdec_stimuli(
  lexdec_pool, n = 8, reference_words = lex$word
)
head(stim_lexdec[, c("target", "condition", "length", "set")], 4)

## ----subsyllabic--------------------------------------------------------------
sub_stim <- build_lexdec_stimuli(
  lexdec_pool, n = 8, reference_words = lex$word, method = "subsyllabic"
)
# The real words come first in the frame, so pair each with its twin by `set`.
merge(
  sub_stim[sub_stim$condition == "word", c("set", "target")],
  sub_stim[sub_stim$condition == "pseudoword", c("set", "target")],
  by = "set", suffixes = c("_word", "_pseudoword")
)[1:4, ]

## ----pseudowords--------------------------------------------------------------
generate_pseudowords(c("house", "table"), lex$word)

## ----resample-----------------------------------------------------------------
reps <- resample_stimuli(pool, design, schema, n_sets = 2)
table(reps$replicate, reps$condition)
length(intersect(
  reps$word[reps$replicate == 1],
  reps$word[reps$replicate == 2]
))

