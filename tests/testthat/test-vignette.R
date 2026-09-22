# The vignette's design chunk declares pool_filters, which match_stimuli never
# reads: the filter takes effect only through the explicit build_pool step the
# vignette demonstrates, mirroring run_pipeline. This pins that the demonstrated
# code applies the filter and that skipping the step would change the selection.
test_that("vignette design chunk applies pool_filters through build_pool", {
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  lex <- load_lexicon(system.file("extdata", "en_example.csv", package = "lexsync"),
                      schema, language = "english")
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
  expect_true(all(pool$length >= 3 & pool$length <= 7))
  expect_true(all(pool$frequency >= 3.8 & pool$frequency <= 7))
  stim <- match_stimuli(pool, design, schema)
  expect_equal(nrow(stim), 30L)
  expect_true(all(stim$length <= 7))
  # Passing the raw lexicon silently drops the filter: longer words become
  # eligible and the selection changes.
  unfiltered <- match_stimuli(lex, design, schema)
  expect_false(identical(sort(unfiltered$word), sort(stim$word)))
})
