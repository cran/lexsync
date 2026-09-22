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
  name = "vignette_repro", language = "english", n_per_condition = 15,
  conditions = list(
    list(name = "high", define_by = list(frequency = c(5.2, 7.0))),
    list(name = "low",  define_by = list(frequency = c(3.8, 4.4)))
  ),
  match_on = list("length", "n_density", "old20"),
  counterbalance = list(lists = 1)
)

## ----determinism--------------------------------------------------------------
a <- match_stimuli(pool, design, schema)
b <- match_stimuli(pool, design, schema)
identical(a, b)

## ----even-spread--------------------------------------------------------------
anchor_pool <- pool[pool$frequency >= 5.2 & pool$frequency <= 7.0, ]
anchor_pool <- anchor_pool[
  order(anchor_pool$frequency, anchor_pool$word, method = "radix"),
]
idx <- unique(round(seq(1, nrow(anchor_pool), length.out = 15)))
idx

## ----radix--------------------------------------------------------------------
words <- c("zebra", "Apple", "apple", "Zebra")
sort(words, method = "radix")

## ----tie-break----------------------------------------------------------------
distance <- c(0.5, 0.5, 0.2)
word     <- c("beta", "alpha", "gamma")
id       <- c(1L, 2L, 3L)
word[order(distance, word, id, method = "radix")]

## ----trial-order--------------------------------------------------------------
stim <- counterbalance(match_stimuli(pool, design, schema), design, schema)
head(stim[, c("trial", "condition", "word")], 4)
same <- counterbalance(match_stimuli(pool, design, schema), design, schema)
identical(stim$trial, same$trial)

## ----cross-engine-------------------------------------------------------------
maha <- design
maha$matching <- list(method = "mahalanobis")
maha_stim <- match_stimuli(pool, maha, schema)
maha_ds <- build_datasheet(
  maha, schema, NULL, maha_stim,
  system.file("extdata", "en_example.csv", package = "lexsync"),
  list(stimuli = NA_character_), schema$seed
)
maha_ds$selection$cross_engine

## ----datasheet----------------------------------------------------------------
out <- file.path(tempdir(), "lexsync_repro")
dir.create(out, showWarnings = FALSE)
report <- match_report(
  stim, c("frequency", "length", "n_density", "old20"), schema
)
stim_path <- file.path(out, "stimuli.csv")
write.csv(stim, stim_path, row.names = FALSE)

ds <- build_datasheet(
  design, schema, report, stim,
  source_path = system.file("extdata", "en_example.csv", package = "lexsync"),
  artifacts = list(stimuli = stim_path),
  seed = schema$seed,
  candidate_pool = lapply(design$conditions, function(cnd)
    list(
      condition = cnd$name,
      n_candidates = nrow(build_pool(pool, cnd$define_by))
    ))
)
names(ds)

## ----checksums----------------------------------------------------------------
ds$materials_source$sha256
str(ds$selection$tolerance_k)
do.call(rbind, lapply(ds$selection$candidate_pool, as.data.frame))

## ----versions-----------------------------------------------------------------
str(ds$reproducibility)

## ----methods-paragraph--------------------------------------------------------
cat(strwrap(methods_paragraph(ds), width = 76), sep = "\n")

## ----write-datasheet----------------------------------------------------------
paths <- write_datasheet(
  ds, file.path(out, "datasheet.json"), file.path(out, "datasheet.md")
)
md <- readLines(file.path(out, "datasheet.md"))
grep("^#", md, value = TRUE)

## ----analysis-----------------------------------------------------------------
ds$analysis$suggested_model

## ----run-log------------------------------------------------------------------
log <- new_run_log(
  "vignette_repro",
  meta = list(seed = schema$seed, language = "english")
)
log <- log_step(
  log,
  sprintf("lexicon loaded: %d words", nrow(lex)),
  list(words = nrow(lex))
)
log <- log_step(log, sprintf("pool after filters: %d words", nrow(pool)))
log <- log_artefact(log, stim_path, rows = nrow(stim))
log_md <- write_run_log(log, file.path(out, "run_log.md"),
                        file.path(out, "run_log.jsonl"))
cat(readLines(log_md), sep = "\n")

