# Tests for the Shiny front-end in apps/r_shiny. The app is not part of the
# package, so these skip wherever it or its interface dependencies are absent
# (a package check run from a source tarball, for instance).

app_env <- function() {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  app <- NULL
  for (cand in c("apps/r_shiny/app.R", "../../../apps/r_shiny/app.R",
                 "../../../../apps/r_shiny/app.R")) {
    if (file.exists(cand)) { app <- cand; break }
  }
  if (is.null(app)) skip("The Shiny app source is not in this tree.")
  e <- new.env(parent = globalenv())
  ok <- tryCatch({ source(app, local = e); TRUE },
                 error = function(err) conditionMessage(err))
  if (!isTRUE(ok)) skip(paste("The Shiny app could not be sourced:", ok))
  e
}

test_that("positive_tolerances keeps only the dimensions given a positive k", {
  e <- app_env()
  # A k of zero must not reach the design: it means "use the schema default", not
  # "match to a zero-width window". Pinned identically in tests/test_apps.py.
  expect_equal(
    e$positive_tolerances(list(length = 0, frequency = 0.111, n_density = 2)),
    list(frequency = 0.111, n_density = 2)
  )
  # Empty, so build_design writes no tolerance_k key at all and the schema stands.
  expect_length(e$positive_tolerances(list(length = 0, old20 = 0)), 0)
  expect_length(e$positive_tolerances(list()), 0)
  expect_equal(e$positive_tolerances(list(length = NA_real_, frequency = 1)),
               list(frequency = 1))
})

test_that("positive_tolerances preserves the order the dimensions are matched on", {
  e <- app_env()
  expect_equal(names(e$positive_tolerances(list(old20 = 2, length = 1, frequency = 0.5))),
               c("old20", "length", "frequency"))
})

test_that("the bundle zip holds the design and every artefact at outdir-relative paths", {
  e <- app_env()
  skip_if_not_installed("zip")
  outdir <- file.path(tempfile("lexsync_zip_"), "output")
  dir.create(file.path(outdir, "reports"), recursive = TRUE)
  writeLines("name: my_design", file.path(outdir, "my_design.yaml"))
  writeLines("word\ncat", file.path(outdir, "stimuli.csv"))
  writeLines("# datasheet", file.path(outdir, "reports", "datasheet.md"))

  zf <- tempfile(fileext = ".zip")
  e$write_bundle_zip(zf, outdir)

  # The same three entries the Streamlit app's make_zip produces (test_apps.py):
  # names are relative to outdir and subdirectories survive.
  expect_equal(sort(zip::zip_list(zf)$filename),
               c("my_design.yaml", "reports/datasheet.md", "stimuli.csv"))
})

test_that("the bundle zip needs no external zip executable", {
  e <- app_env()
  skip_if_not_installed("zip")
  outdir <- tempfile("lexsync_zip_"); dir.create(outdir)
  writeLines("word\ncat", file.path(outdir, "stimuli.csv"))
  zf <- tempfile(fileext = ".zip")

  # utils::zip shells out to R_ZIPCMD, which a stock R for Windows does not ship.
  # Pointing it at a name that cannot resolve proves the export no longer uses it.
  old <- Sys.getenv("R_ZIPCMD", unset = NA)
  on.exit(if (is.na(old)) Sys.unsetenv("R_ZIPCMD") else Sys.setenv(R_ZIPCMD = old))
  Sys.setenv(R_ZIPCMD = "lexsync-no-such-zip-tool")
  expect_no_warning(e$write_bundle_zip(zf, outdir))
  expect_true(file.exists(zf))
  expect_equal(zip::zip_list(zf)$filename, "stimuli.csv")
})

test_that("the app offers exactly the paradigms the engine implements", {
  e <- app_env()
  expect_equal(sort(unname(e$PARADIGMS)),
               c("factorial", "lexical_decision", "priming", "self_paced_reading"))
})

test_that("the app offers exactly the pseudoword generators the engine implements", {
  e <- app_env()
  # The engine default first: the design only names a method when the other is
  # chosen. Pinned identically in tests/test_apps.py.
  expect_equal(e$GENERATION_METHODS, c("letter_substitution", "subsyllabic"))
})

test_that("the design YAML is written with LF endings on every platform", {
  e <- app_env()
  # The pipeline hashes the design file it ran into the datasheet's
  # design_sha256, so the bytes must not record which operating system wrote
  # them; yaml::write_yaml and writeLines both open text-mode connections,
  # which on Windows turn every newline into CRLF. tests/test_apps.py pins the
  # same property for the Streamlit app's _write_design_yaml.
  path <- tempfile(fileext = ".yaml")
  e$write_yaml_lf(list(name = "t", n_per_condition = 10L), path)
  raw <- readBin(path, "raw", file.info(path)$size)
  expect_false(any(raw == as.raw(0x0d)))
  expect_identical(raw[length(raw)], as.raw(0x0a))
  expect_identical(yaml::read_yaml(path)$name, "t")
})
