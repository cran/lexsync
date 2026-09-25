# Locating the lexsync repository around the package, for the tests that hold the
# package to files kept only at the repository root: templates/, config/, items/,
# apps/ and README.md.
#
# From a source checkout the tests run in R_workflow/tests/testthat, three levels
# below the root; a local R CMD check started in R_workflow/ runs them from
# R_workflow/lexsync.Rcheck/tests/testthat, four levels below it, and reaches the
# repository only with NOT_CRAN=true, as devtools::check() sets. On CRAN the same
# walk ends wherever the check was started, and on its Linux hosts that directory
# holds the unpacked sources of other CRAN packages. A bare name can therefore be
# somebody else's package: the 'templates' package once passed a dir.exists() guard
# here and failed the check. A candidate root counts only if it carries this
# package's own DESCRIPTION under R_workflow/ and the Python twin beside it, and on
# CRAN every test that needs it skips, since no repository can be present there.

# The repository root, or NULL when the package is not inside its repository.
repo_root <- function() {
  for (depth in 3:4) {
    root <- normalizePath(do.call(testthat::test_path, as.list(rep("..", depth))),
                          winslash = "/", mustWork = FALSE)
    desc <- file.path(root, "R_workflow", "DESCRIPTION")
    if (!file.exists(desc) || !dir.exists(file.path(root, "python_workflow"))) next
    pkg <- tryCatch(read.dcf(desc, fields = "Package")[1, 1], error = function(e) NA)
    if (identical(unname(pkg), "lexsync")) return(root)
  }
  NULL
}

# A path under the repository root, skipping the calling test with `message` on
# CRAN or when the package is not inside its repository.
repo_path <- function(..., message = "the lexsync repository is not around this package") {
  testthat::skip_on_cran()
  root <- repo_root()
  if (is.null(root)) testthat::skip(message)
  file.path(root, ...)
}
