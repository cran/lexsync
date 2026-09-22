# io_utils.R -- robust UTF-8 input/output and provenance helpers.
# Centralising these guards against the encoding pitfalls that arise with
# multilingual stimuli on Windows, and provides the hashing used by the run log.

#' Null-coalescing operator
#'
#' Returns `a` unless it is `NULL`, in which case it returns `b`.
#' @param a,b Values; `a` is returned unless `NULL`.
#' @return `a` or `b`.
#' @name grapes-or-or-grapes
#' @keywords internal
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Read a UTF-8 CSV file
#'
#' @param path Path to a CSV file.
#' @param as_character Character vector of column names whose type must not be guessed,
#'   read as text instead. A name the file's header does not carry is ignored, since
#'   readr warns about a parser for a column that is not there.
#' @return A data frame (a tibble), as returned by [readr::read_csv()].
#' @importFrom readr read_csv locale
#' @keywords internal
read_csv_utf8 <- function(path, as_character = character(0)) {
  if (!file.exists(path)) {
    stop(sprintf("lexsync: file not found: '%s'", path), call. = FALSE)
  }
  # `as_character` names columns whose type must NOT be guessed. readr reads a column
  # whose values are all `f`, `t`, `T` or `F` as LOGICAL, so an item table coding its
  # two response keys as f and j had `answer` turned into FALSE, while pandas kept the
  # string "f". Measured, not supposed: readr 2.2.0 reads f and t as logical and j, y
  # and n as character, which is the worst possible split because f/j and t/f are the
  # two commonest key pairs in a two-choice task. The paradigm's presented fields are
  # therefore read as text; every other column keeps its inferred type, so a numeric
  # column a design filters on still arrives numeric.
  col_types <- NULL
  if (length(as_character)) {
    # Only name columns the file actually has: readr warns about a parser for a column
    # that is not there, and a caller naming a paradigm's required fields cannot know
    # whether the table supplies them. Reporting the missing column is load_items's
    # job, and its message says which ones. Reading the header alone costs one line.
    header <- names(readr::read_csv(path, n_max = 0, show_col_types = FALSE,
                                    progress = FALSE,
                                    locale = readr::locale(encoding = "UTF-8")))
    want <- intersect(as_character, header)
    if (length(want)) {
      col_types <- do.call(readr::cols, c(stats::setNames(
        replicate(length(want), readr::col_character(), simplify = FALSE),
        want), list(.default = readr::col_guess())))
    }
  }
  readr::read_csv(
    path,
    col_types = col_types,
    show_col_types = FALSE,
    progress = FALSE,
    locale = readr::locale(encoding = "UTF-8")
  )
}

# Where readr leaves fixed notation. Beyond it readr's layout could not be reproduced
# in Python: it writes 1.5e16 as "15e15", the largest double as "17976931348623157e292",
# and the double nearest 5e22 as "4.9999999999999996e+22", and no single rule fits all
# three. Above 2^49 consecutive doubles are more than 0.1 apart, so two different
# one-decimal strings can round-trip to the same double and the engines print different
# ones: readr writes 1000000000000000.25 as "...0.3" where Python gives "...0.2".
.READR_BIG <- 1e15
.READR_TIE <- 2^49

#' Refuse a value the two engines could not write identically
#'
#' Nothing lexsync computes reaches these magnitudes (frequencies are Zipf values under
#' 8, counts and durations under 1e6), but a joined norm table, a supplied pool
#' or an item table may carry any column the user likes, and those columns go straight
#' into the stimuli CSV. The guard lives in both engines so that each refuses the same
#' design; one engine accepting what the other rejects is a difference of its own.
#'
#' @param x A data frame about to be written.
#' @return `x`, invisibly, or an error.
#' @keywords internal
.check_csv_writable <- function(x) {
  for (nm in names(x)) {
    col <- x[[nm]]
    if (!is.numeric(col) || is.integer(col)) next
    v <- col[is.finite(col)]
    if (!length(v)) next
    big <- v[abs(v) >= .READR_BIG]
    if (length(big)) {
      stop(sprintf(paste("lexsync: column '%s' holds %s, which is too large to write",
                         "identically from both engines. Above 1e15 readr's number",
                         "format could not be reproduced in Python, so the R and Python",
                         "CSVs would differ with nothing to signal it. Scale the column,",
                         "or carry it as text."), nm, format(big[1], digits = 17)),
           call. = FALSE)
    }
    # The shortest decimal is unique below 2^49, so only this band needs the check.
    tie <- v[abs(v) >= .READR_TIE & v != trunc(v)]
    for (t in tie) {
      a <- abs(t)
      # Between 2^49 and 1e15 consecutive doubles are exactly 0.125 apart, so a
      # non-integral double's shortest round-tripping decimal has exactly one
      # fractional digit, and the form is ambiguous precisely when two neighbouring
      # one-decimal strings parse back to the same double. `ip + d/10` IS that
      # parse: ip is an exact double (below 2^53), the error of d/10 is under
      # 2^-56, and no candidate sits nearer a rounding boundary than 1/80, so the
      # sum rounds to the same double the decimal string does. Must refuse exactly
      # what _shortest_digits_ambiguous refuses in io_utils.py.
      ip <- trunc(a)
      if (sum(ip + (1:9) / 10 == a) >= 2L) {
        stop(sprintf(paste("lexsync: column '%s' holds %s, which has more than one",
                           "shortest decimal form. The R and Python engines print",
                           "different ones, so the two CSVs would differ with nothing to",
                           "signal it. Round the column, or carry it as text."),
                     nm, format(t, digits = 16)), call. = FALSE)
      }
    }
  }
  invisible(x)
}

#' Write a data frame to a BOM-free UTF-8 CSV file
#'
#' @param x A data frame.
#' @param path Output path; parent directories are created as needed.
#' @return `path`, invisibly.
#' @details A value the two engines cannot render alike is refused, naming the column:
#'   a magnitude at or above 1e15, where readr has three incompatible layouts and no
#'   rule fits all of them, and a value with two equally short decimal forms, where the
#'   two writers pick opposite ones.
#' @importFrom readr write_csv
#' @keywords internal
write_csv_utf8 <- function(x, path) {
  .check_csv_writable(x)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(x, path, na = "")
  invisible(path)
}

#' Write text to a file with LF line endings on every platform
#'
#' `writeLines(x, path)` opens the path in text mode, so on Windows R turns every
#' newline into CRLF. The generated experiment scripts are compared against the
#' Python engine's byte for byte, and their checksums are published in the
#' materials datasheet, so their bytes must not record which operating system
#' produced them. A binary connection writes the string as given.
#'
#' @param x A character vector of lines.
#' @param path Output path; parent directories are created as needed.
#' @return `path`, invisibly.
#' @keywords internal
write_lines_lf <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(x, con, useBytes = TRUE, sep = "\n")
  invisible(path)
}

# ---- Reproducible reductions -----------------------------------------------
# A sum, mean, variance and median that give the same bits in the R and Python
# engines.
#
# This is not pedantry; it was a live bug. Two designs' reported means differed between
# the engines in the last decimal place the descriptives publish, 1.448 from R against
# 1.447 from numpy, because R's mean() uses a two-pass long-double algorithm while
# numpy sums pairwise, and the true value happened to sit on a rounding boundary.
# Summing 20000 identical doubles was measured to give three different answers across
# R's sum(), math.fsum, numpy's pairwise sum and a naive loop, so no language's
# built-in reduction can be relied on for a cross-engine artefact.
#
# Neumaier compensated summation is used instead, written out in plain double
# arithmetic in both engines. Every operation is +, -, abs or a comparison, and
# IEEE-754 requires + and - to be correctly rounded, so the two engines execute the
# same sequence of exactly-specified operations and cannot disagree. That is an
# argument. Relying on R's long-double accumulator amounted only to a measurement.
# Mirrors io_utils.py.

#' @keywords internal
.exact_sum <- function(x) {
  s <- 0; comp <- 0
  for (v in as.numeric(x)) {
    t <- s + v
    # The larger magnitude keeps its low bits; the smaller one's are what get lost, so
    # the correction is computed from whichever term is smaller.
    comp <- comp + if (abs(s) >= abs(v)) ((s - t) + v) else ((v - t) + s)
    s <- t
  }
  s + comp
}

#' @keywords internal
.exact_mean <- function(x) {
  x <- as.numeric(x)
  if (!length(x)) return(NA_real_)
  .exact_sum(x) / length(x)
}

# Two-pass variance: the mean first, then the compensated sum of squared deviations.
# The textbook one-pass form (sum of squares minus n times the squared mean) is
# catastrophically cancelling for data far from zero and would differ between engines
# by far more than a last bit.
#' @keywords internal
.exact_var <- function(x) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 2L) return(NA_real_)
  d <- x - .exact_mean(x)
  .exact_sum(d * d) / (n - 1L)
}

#' @keywords internal
.exact_sd <- function(x) {
  v <- .exact_var(x)
  # sqrt is correctly rounded under IEEE-754, so it adds no divergence.
  if (is.na(v)) NA_real_ else sqrt(v)
}

# Median via a sort and the exact middle. stats::median averages the two middle
# values through mean(), whose long-double accumulator is platform-dependent;
# (a + b) / 2 in plain double arithmetic is one correctly-rounded IEEE addition
# and one exact halving, so both engines compute the same double from the same
# input. Must stay identical to _exact_median in
# python_workflow/src/lexsync/io_utils.py.
#' @keywords internal
.exact_median <- function(x) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  n <- length(x)
  if (!n) return(NA_real_)
  x <- sort(x, method = "radix")
  if (n %% 2L == 1L) return(x[(n + 1L) %/% 2L])
  (x[n %/% 2L] + x[n %/% 2L + 1L]) / 2
}

# ---- One decimal rounder, shared by both engines ---------------------------
#
# No pairing of built-ins works. Measured over 210,000 values including every 3-dp
# halfway case in range: R's round() disagrees with Python's builtin round(), Python's
# builtin disagrees with numpy's round(), and even R's sprintf("%.3f") disagrees with
# Python's "%.3f" on 274 of them, because R's delegates to the platform C library while
# Python's is correctly rounded. No value rounded for an artefact can safely be handed
# to any language's own rounder.
#
# This one is defined by its arithmetic instead: scale, truncate toward zero, then step
# away from zero when the remainder reaches a half. Every operation is *, -, /, trunc,
# abs or a comparison, all of which IEEE-754 either mandates correctly rounded or makes
# exact, so both engines compute the same double from the same input by construction.
#
# It rounds the SCALED double. For a tie that is not exactly representable, the true
# decimal value would give a different answer, so this is a choice the package makes
# and arithmetic does not force. It is the same trade-off the balance optimiser's
# quantisation already makes, and it is the right way
# round: reproducible across engines matters more here than agreeing with what a
# calculator would say about a value that was never exactly a half.
# Must stay identical to _round_dp in python_workflow/src/lexsync/io_utils.py. This
# function is already vectorised, so it is also the twin of the Python engine's
# _round_dp_vec, which applies the same arithmetic over a whole numpy array; a
# change here must land in both of them.
#' @keywords internal
.round_dp <- function(x, dp) {
  p <- 10^dp
  y <- as.numeric(x) * p
  t <- trunc(y)
  r <- y - t
  # sign(y), because a negative value must step away from zero too.
  adj <- ifelse(is.finite(r) & abs(r) >= 0.5, sign(y), 0)
  out <- (t + adj) / p
  out[!is.finite(y)] <- as.numeric(x)[!is.finite(y)]
  out
}

#' MD5 digest of a file, for provenance logging
#'
#' MD5 (from base \pkg{tools}) is used as a lightweight content fingerprint; it
#' is a provenance aid, not a security measure. The Python package uses the same
#' algorithm so that run logs are comparable across engines.
#'
#' @param path File path.
#' @return A hex digest string, or `NA` when the file is absent.
#' @importFrom tools md5sum
#' @keywords internal
hash_file <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  unname(tools::md5sum(path))
}

#' SHA-256 digest of a file, the stronger fingerprint used by the datasheet
#'
#' @param path File path.
#' @return A hex digest string, or `NA` when the file is absent.
#' @importFrom digest digest
#' @keywords internal
sha256_file <- function(path) {
  if (is.null(path) || is.na(path) || !file.exists(path)) return(NA_character_)
  digest::digest(file = path, algo = "sha256")
}

# Is this design a continuous (non-dichotomised) selection?
#
# One predicate, where there were four copies of the same expression. It was repeated
# verbatim in run_pipeline.R, datasheet.R and both Python twins, which is how a
# `continuous:` block under `items.source: table` came to be silently inert in all
# four places at once. `pool` belongs in the allowed set because run_pipeline's
# corpus/pool branch handles continuous selection generically; leaving it out sent a
# continuous design over a supplied pool to the conditions matcher, which then failed
# with a different obscure error in each engine. `generate` stays excluded
# deliberately: a continuous block there would push a word/pseudoword frame into the
# selector.
# Must stay identical to _is_continuous in python_workflow/src/lexsync/io_utils.py.
#' @keywords internal
.is_continuous <- function(design) {
  src <- (design$items$source %||% "corpus")
  if (!is.null(design$continuous) && identical(src, "generate")) {
    stop("lexsync: a 'continuous' block cannot be combined with items.source 'generate'.",
         call. = FALSE)
  }
  !is.null(design$continuous) && src %in% c("corpus", "pool", "table")
}

# Render one component of a hash key. Never interpolate a number directly: R
# prints 42.0 as "42" and Python as "42.0", and a pandas column silently promoted
# to float64 by a single missing value would otherwise change every digest and so
# every realised duration. Integral values go through %d in both engines.
# Must stay identical to _key_part in python_workflow/src/lexsync/io_utils.py.
#' @keywords internal
.key_part <- function(x) {
  # A component that cannot be rendered identically in both engines must never be
  # silently hashed. Measured: a missing value rendered "NA" here and "nan" in Python,
  # TRUE/FALSE against True/False, Inf against inf. A blank `condition` cell is a
  # routine data error that neither reader rejects, and it produced a DIFFERENT trial
  # order in each engine, reproducibly and with nothing to signal it.
  #
  # Raising beats picking a spelling. A missing condition, set or list is always a data
  # error, and a reproducible order computed over a meaningless key is worse than a
  # stop. Booleans do get a pinned spelling, because they are legitimate: R's
  # as.character gives "TRUE" and Python's str gives "True", so the spelling is fixed
  # here, so neither language chooses it.
  # Must stay identical to _key_part in python_workflow/src/lexsync/io_utils.py.
  if (anyNA(x)) {
    stop(paste("lexsync: a hash-key component is missing, so the trial order cannot be",
               "made identical across engines. Check the items table for a blank",
               "condition, set or list cell."), call. = FALSE)
  }
  if (is.logical(x)) return(ifelse(x, "TRUE", "FALSE"))
  if (is.numeric(x)) {
    if (any(!is.finite(x))) {
      stop("lexsync: a hash-key component is not finite, so it cannot be keyed.",
           call. = FALSE)
    }
    # The integer bound matters: as.integer() beyond it yields NA with a warning, which
    # would put an empty component into the key.
    ok <- x == floor(x) & abs(x) <= .Machine$integer.max
    out <- character(length(x))
    out[ok] <- sprintf("%d", as.integer(x[ok]))
    out[!ok] <- sprintf("%.17g", x[!ok])
    return(out)
  }
  as.character(x)
}

# A uniform variate in [0, 1) derived from a keyed SHA-256 digest.
#
# This is how lexsync gets anything that looks stochastic without a generator:
# jittered durations, and any future search that needs a candidate order. The
# scheme is chosen for exact reproducibility across the two engines, and every
# step of it is there for a reason.
#
# Thirteen hex digits give a 52-bit integer, which a double represents exactly;
# dividing by 2^52 is exact because the divisor is a power of two. The result is
# therefore identical bits in R and Python, to the last one. Fourteen
# digits or more would round up to exactly 1.0, and `lo + floor(u * n)` would
# then silently return `hi + 1`. R needs two chunks because `strtoi` returns NA
# above 2^31 - 1.
#
# Only +, -, * and / are used downstream. IEEE-754 mandates those to be
# correctly rounded, so they agree on any conforming platform; `exp`, `log` and
# `^` do not, and were measured to differ between R and Python by one unit in
# the last place. Must stay identical to hash_unit() in
# python_workflow/src/lexsync/io_utils.py.
#' @importFrom digest digest
#' @keywords internal
hash_unit <- function(key) {
  # enc2utf8 because digest(serialize = FALSE) hashes the stored bytes: a
  # latin1-marked string read from a user's CSV would otherwise give a different
  # digest from the same characters in Python.
  h <- vapply(enc2utf8(as.character(key)),
              function(k) digest::digest(k, algo = "sha256", serialize = FALSE),
              character(1), USE.NAMES = FALSE)
  (strtoi(substr(h, 1L, 7L), 16L) * 16777216 + strtoi(substr(h, 8L, 13L), 16L)) /
    4503599627370496
}

# A uniform integer in [lo, hi], both ends included, from a keyed digest.
#' @keywords internal
hash_int_range <- function(key, lo, hi) {
  lo <- as.integer(lo); hi <- as.integer(hi)
  if (any(hi < lo)) stop("lexsync: jitter range must have hi >= lo.", call. = FALSE)
  as.integer(lo + floor(hash_unit(key) * (hi - lo + 1)))
}

#' Build a short, filesystem-safe slug
#'
#' Keeps generated file names short and space-free, which avoids the Windows
#' `MAX_PATH` limit inside deeply nested, cloud-synced directories.
#'
#' @param ... Character fragments to join.
#' @return A lower-case, underscore-separated slug.
#' @keywords internal
slugify <- function(...) {
  s <- paste(c(...), collapse = "_")
  s <- gsub("[^A-Za-z0-9]+", "_", s)
  s <- gsub("_+", "_", s)
  # Reducing to ASCII first does not make base `tolower()` safe here: under a
  # Turkish or Azeri locale it maps "I" to the dotless i (U+0131), so the slug
  # would leave ASCII and this design's artefacts would be written under a name
  # the Python engine never produces. Case-fold as [.lower_invariant()] does.
  .lower_invariant(gsub("^_|_$", "", s))
}

#' Read a YAML configuration file
#'
#' @param path Path to a YAML file.
#' @return A named list.
#' @importFrom yaml read_yaml
#' @keywords internal
read_config <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("lexsync: configuration not found: '%s'", path), call. = FALSE)
  }
  yaml::read_yaml(path)
}

#' Validate a single stimulus value for safe inclusion in generated files
#'
#' Rejects control characters (including tab/newline) and over-long strings, so a
#' crafted item cannot corrupt the generated loop table or experiment scripts.
#' Commas and quotation marks are allowed: presented strings are written as data
#' into a properly quoted CSV the experiment reads at run time, never interpolated
#' into generated code. Mirrors the Python `clean_field`.
#'
#' @param value A value coerced to a single string.
#' @param field Field name, for error messages.
#' @param max_len Maximum permitted length in characters.
#' @return The value as a plain string.
#' @keywords internal
clean_field <- function(value, field = "field", max_len = 1000L) {
  s <- as.character(value)
  # R character strings cannot themselves contain a nul byte, so guarding the
  # remaining C0 controls and DEL covers every reachable case.
  if (grepl("[\x01-\x1f\x7f]", s, perl = TRUE)) {
    stop(sprintf("lexsync: stimulus '%s' contains control characters: %s", field, s),
         call. = FALSE)
  }
  if (nchar(s) > max_len) {
    stop(sprintf("lexsync: stimulus '%s' exceeds %d characters.", field, max_len), call. = FALSE)
  }
  s
}

# Characters that can leave a data position and start a code one. A stimulus may hold
# any of these, because a stimulus is written to a CSV the experiment reads at run time;
# a value INTERPOLATED INTO the generated script or markup may not.
#   ' " \ backtick  end a Python or JavaScript string literal
#   < > &           open a tag or entity in the generated HTML
#   { } ;           end a CSS declaration, or a template placeholder
#   $               begins a JavaScript template substitution
.UNSAFE_META <- "['\"\\\\`<>&{};$]"
# \z and not $: in PCRE (and in Python's re) `$` also matches just BEFORE a final
# newline, so a value ending in one satisfied a `$`-anchored shape check and carried
# that newline into a line-oriented .osexp, splitting the inline script it landed in.
# Anchoring at end-of-string is the whole guard here.
.PORT_SHAPE <- "^(0[xX][0-9A-Fa-f]{1,8}|[0-9]{1,10})\\z"
.COLUMN_SHAPE <- "^[A-Za-z_][A-Za-z0-9_]*\\z"
# A response key is written into OpenSesame's `set allowed_responses "a;b"`, into a
# PsychoPy key list and into a jsPsych `choices` array. Key names are short tokens.
.KEY_SHAPE <- "^[A-Za-z0-9_ +]{1,20}\\z"

#' Validate a metadata value interpolated into generated code or markup
#'
#' A design's name, language label and font are not stimuli. They do not travel in the
#' loop table the experiment reads at run time; they are substituted straight into the
#' PsychoPy script, the OpenSesame inline Python and the jsPsych HTML, so a quote or an
#' angle bracket there stops being text and becomes syntax. A design file is meant to be
#' shared and re-run by someone else, which is what makes an unvalidated one an
#' executable payload as much as a configuration.
#'
#' Refusing beats escaping. Escaping correctly would mean three different escapes for
#' three targets in two engines, six places to get subtly wrong, and it would change the
#' bytes the two engines write; refusing is one rule that leaves every legitimate value
#' ("en_lexdec", "english", "Courier New", "SimHei") byte-identical. Mirrors the Python
#' `clean_meta`.
#'
#' @param value A value coerced to a single string.
#' @param field Field name, for error messages.
#' @param max_len Maximum permitted length in characters.
#' @return The value as a plain string.
#' @keywords internal
clean_meta <- function(value, field = "value", max_len = 200L) {
  s <- as.character(value)
  if (grepl("[\x01-\x1f\x7f]", s, perl = TRUE)) {
    stop(sprintf(paste("lexsync: %s contains control characters, and it is written into",
                       "the generated experiment scripts: %s"), field, s), call. = FALSE)
  }
  if (nchar(s) > max_len) {
    stop(sprintf("lexsync: %s exceeds %d characters.", field, max_len), call. = FALSE)
  }
  if (grepl(.UNSAFE_META, s, perl = TRUE)) {
    bad <- regmatches(s, regexpr(.UNSAFE_META, s, perl = TRUE))
    stop(sprintf(paste("lexsync: %s contains '%s', which cannot be written safely into",
                       "the generated PsychoPy, OpenSesame and jsPsych files, because it",
                       "would end a string literal or open a tag there. Use letters,",
                       "digits, spaces and -_.() in a design's name, language and font.",
                       "Offending value: %s"), field, bad, s), call. = FALSE)
  }
  s
}

#' Validate a parallel-port address, which is written into the script unquoted
#'
#' @param value A value coerced to a single string.
#' @param field Field name, for error messages.
#' @return The value as a plain string.
#' @keywords internal
clean_port <- function(value, field = "triggers.parallel_address") {
  s <- as.character(value)
  if (!grepl(.PORT_SHAPE, s, perl = TRUE)) {
    stop(sprintf(paste("lexsync: %s must be a port address such as 0x0378 or 888, not",
                       "'%s'. It is written into the generated experiment as a bare",
                       "number."), field, s), call. = FALSE)
  }
  s
}

#' Validate one response key, which is written into the generated experiments
#'
#' OpenSesame takes the keys as `set allowed_responses "a;b"` on one line of a
#' line-oriented format, so a key containing a quote closed the string and a newline
#' ended the line, and the rest of the value became new top-level items in the
#' experiment, including an inline_script whose body runs.
#'
#' @param value A value coerced to a single string.
#' @param field Field name, for error messages.
#' @return The value as a plain string.
#' @keywords internal
clean_key <- function(value, field = "an event's `keys`") {
  s <- as.character(value)
  if (!grepl(.KEY_SHAPE, s, perl = TRUE)) {
    stop(sprintf(paste("lexsync: %s must be a key name such as 'f', 'space' or 'left',",
                       "not '%s'. Keys are written into the generated experiments as an",
                       "allowed-response list."), field, s), call. = FALSE)
  }
  s
}

#' Validate a loop-table column name, which is written into generated code
#'
#' @param value A value coerced to a single string.
#' @param field Field name, for error messages.
#' @return The value as a plain string.
#' @keywords internal
clean_column <- function(value, field = "column") {
  s <- as.character(value)
  if (!grepl(.COLUMN_SHAPE, s, perl = TRUE)) {
    stop(sprintf(paste("lexsync: %s must be a plain column name (letters, digits and",
                       "underscore, not starting with a digit), not '%s'. It is written",
                       "into the generated experiment as a variable reference."),
                 field, s), call. = FALSE)
  }
  s
}
