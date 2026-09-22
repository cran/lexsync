# Contracts for the shipped configuration files. The bundled schema copy must
# stay byte-identical to config/schema.yaml, and the schema must not advertise
# keys that no code reads: a key that parses but is never consulted invites
# users to edit the one setting (the tie-break) that underpins the
# byte-identical R<->Python parity guarantee. Repository-level checks skip
# gracefully when the package is checked in isolation.

bundled_schema <- function() system.file("extdata", "schema.yaml", package = "lexsync")

repo_file <- function(...) {
  path <- testthat::test_path("..", "..", "..", ...)
  testthat::skip_if_not(file.exists(path), "repository configuration not available")
  path
}

test_that("bundled schema is byte-identical to config/schema.yaml", {
  repo_schema <- repo_file("config", "schema.yaml")
  expect_identical(readBin(repo_schema, "raw", file.size(repo_schema)),
                   readBin(bundled_schema(), "raw", file.size(bundled_schema())))
})

# Every key in the schema must be read by the code; the tie-break order, the
# TOST choice and the trigger reset value are fixed in matching/validation/
# scripting, so they live in comments, not as (inert) configurable keys.
test_that("schema carries no inert keys", {
  schema <- yaml::read_yaml(bundled_schema())
  expect_false("tie_break" %in% names(schema$matching))
  expect_false("test" %in% names(schema$equivalence))
  expect_false("reset_value" %in% names(schema$triggers))
})

# Pins the same contract as
# test_es_gender_repro_frequency_tolerance_is_exactly_one_ninth in the Python
# engine's test_config.py. The truncated 0.111 gave a window 0.1% narrower than
# the criterion the design claims to reproduce.
test_that("the reproduction design encodes the origin study's SD/9 window exactly", {
  design <- yaml::read_yaml(repo_file("config", "design_es_gender_repro.yaml"))
  expect_identical(design$matching$tolerance_k$frequency, 1 / 9)
})
