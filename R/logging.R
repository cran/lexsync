# logging.R -- an automated, comprehensive run log. Records each pipeline stage
# with its parameters, the seed, package and corpus provenance, row counts and
# file fingerprints, and writes both a human-readable Markdown log and a
# machine-readable JSON Lines log.

#' Start a new run log
#'
#' @param name A label for the run.
#' @param meta A named list of run-level metadata (seed, versions, ...).
#' @return A run-log object (a list).
#' @export
new_run_log <- function(name, meta = list()) {
  list(
    name = name,
    started = as.character(Sys.time()),
    engine = sprintf("R %s", getRversion()),
    meta = meta,
    steps = list()
  )
}

#' Append a step to a run log
#'
#' @param log A run-log object.
#' @param message A short description of the step.
#' @param data An optional named list of step details.
#' @return The updated run-log object.
#' @export
log_step <- function(log, message, data = NULL) {
  log$steps[[length(log$steps) + 1]] <- list(
    time = as.character(Sys.time()), message = message, data = data
  )
  if (isTRUE(getOption("lexsync.verbose", TRUE))) {
    cat(sprintf("[lexsync] %s\n", message))
  }
  log
}

#' Record a written artefact (path, rows, fingerprint) in the log
#'
#' @param log A run-log object.
#' @param path A file path that has just been written.
#' @param rows Optional row count.
#' @return The updated run-log object.
#' @export
log_artefact <- function(log, path, rows = NA_integer_) {
  log_step(log, sprintf("wrote '%s'", basename(path)),
           list(path = path, rows = rows, md5 = hash_file(path)))
}

#' Write the run log to Markdown (and optionally JSON Lines)
#'
#' @param log A run-log object.
#' @param md_path Output path for the Markdown log.
#' @param jsonl_path Optional output path for the JSON Lines log.
#' @return `md_path`, invisibly.
#' @importFrom jsonlite toJSON
#' @export
write_run_log <- function(log, md_path, jsonl_path = NULL) {
  dir.create(dirname(md_path), recursive = TRUE, showWarnings = FALSE)
  lines <- c(
    sprintf("# lexsync run log: %s", log$name), "",
    sprintf("- Engine: %s", log$engine),
    sprintf("- Started: %s", log$started),
    sprintf("- Finished: %s", as.character(Sys.time()))
  )
  if (length(log$meta)) {
    lines <- c(lines, "", "## Run metadata", "")
    for (k in names(log$meta)) {
      lines <- c(lines, sprintf("- %s: %s", k, paste(log$meta[[k]], collapse = ", ")))
    }
  }
  lines <- c(lines, "", "## Steps", "")
  for (s in log$steps) {
    lines <- c(lines, sprintf("- **%s**: %s", s$time, s$message))
    if (!is.null(s$data)) {
      for (k in names(s$data)) {
        lines <- c(lines, sprintf("    - %s: %s", k, paste(s$data[[k]], collapse = ", ")))
      }
    }
  }
  # write_lines_lf, not writeLines: a text-mode connection turns every newline into
  # CRLF on Windows, which made the R run log disagree byte for byte with the Python
  # engine's for the same run. The .jsonl below already used a binary connection.
  write_lines_lf(lines, md_path)

  if (!is.null(jsonl_path)) {
    con <- file(jsonl_path, open = "wb", encoding = "UTF-8")
    on.exit(close(con))
    for (s in log$steps) {
      writeLines(jsonlite::toJSON(s, auto_unbox = TRUE, null = "null"), con, useBytes = TRUE)
    }
  }
  invisible(md_path)
}
