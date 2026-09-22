# The experiment templates are mirrored into both packages; the mirrors must match.
#
# templates/ at the repository root is the canonical copy. Both packages carry
# their own so an installed copy can reach one without the repository: the R
# package under inst/templates/, the Python package under src/lexsync/templates/,
# and each resolves its own through .lexsync_template() / find_template().
#
# Nothing enforced that the three agree, and no functional test can. A stale
# mirror is still a perfectly valid template: it renders, it runs, and the
# experiment it produces is simply not the one the repository documents. For the
# trigger templates that is worse than ordinary staleness, because what drifts is
# the code time-locking EEG markers to stimulus onset, and an experiment that
# records the wrong onset does not fail -- it quietly collects unusable data.
#
# Comparing the bytes is the check that catches it. Twinned with
# python_workflow/tests/test_templates.py.
# Repository-level checks skip gracefully when the package is checked in
# isolation, as test-config.R already does.

repo_templates_dir <- function() {
  path <- testthat::test_path("..", "..", "..", "templates")
  testthat::skip_if_not(dir.exists(path), "repository templates not available")
  path
}

packaged_templates_dir <- function() {
  path <- system.file("templates", package = "lexsync")
  testthat::skip_if_not(nzchar(path) && dir.exists(path), "packaged templates not available")
  path
}

relative_tree <- function(root) {
  sort(list.files(root, recursive = TRUE, all.files = FALSE))
}

read_raw <- function(path) readBin(path, "raw", file.size(path))

test_that("the packaged mirror holds the same files as the repository", {
  canonical <- relative_tree(repo_templates_dir())
  expect_gt(length(canonical), 0)
  expect_identical(relative_tree(packaged_templates_dir()), canonical)
})

test_that("each packaged template is byte-identical to the repository copy", {
  canonical_dir <- repo_templates_dir()
  packaged_dir <- packaged_templates_dir()
  for (rel in relative_tree(canonical_dir)) {
    expect_identical(read_raw(file.path(packaged_dir, rel)),
                     read_raw(file.path(canonical_dir, rel)),
                     info = rel)
  }
})
