# validation.R -- match-quality reporting: per-condition descriptive statistics,
# standardised mean differences and equivalence tests. This elevates the
# original workflow's equal-frequency warning loop into an inspectable report.
#
# The one place a non-elementary function reaches a compared artefact. Everything
# else on the path to a stimuli, descriptives or comparisons file is built from
# operations IEEE-754 either mandates correctly rounded or makes exact, so the two
# engines agree by construction. The Student t quantile and distribution behind
# the confidence interval and the TOST p-value are not among those: stats::qt and
# stats::pt here, scipy.stats.t there, two independent implementations. Measured
# on R 4.6.1 against SciPy 1.17.1, they disagree by at most about 1e-12 over the
# range these reports use, while the published values are rounded to three and
# four decimal places, which leaves 5e-4 and 5e-5 of headroom before a byte could
# move. The comparisons artefacts are therefore byte-identical on the evidence of
# a margin of some seven orders of magnitude, not on the by-construction argument
# that carries the rest. Mirrors the note in validation.py.

#' Per-group descriptive statistics for several dimensions
#'
#' @param stimuli A stimuli data frame.
#' @param dims Character vector of dimension columns.
#' @param by Grouping column (default `"condition"`).
#' @return A long data frame with n, mean, sd, min, median and max per group.
#' @export
describe_stimuli <- function(stimuli, dims, by = "condition") {
  # Group in order of first appearance: split() alone coerces to a factor whose
  # levels are locale-collated sort(unique(x)), which diverges from validation.py's
  # groupby(sort = FALSE) and from the anchor match_report() takes from unique().
  by_vals <- as.character(stimuli[[by]])
  groups <- split(stimuli, factor(by_vals, levels = unique(by_vals)))
  rows <- list()
  for (g in names(groups)) {
    d <- groups[[g]]
    for (dim in dims) {
      x <- suppressWarnings(as.numeric(d[[dim]]))
      xv <- x[!is.na(x)]
      rows[[length(rows) + 1]] <- data.frame(
        group = g, dimension = dim, n = length(xv),
        mean = .exact_mean(xv), sd = .exact_sd(xv),
        # NA (not Inf/-Inf, which is what min/max of an empty vector give) when a
        # dimension is entirely missing, so both engines agree.
        min = if (length(xv)) min(xv) else NA_real_,
        # .exact_median, not stats::median: the latter averages the two middle
        # values through mean()'s long-double accumulator, the one reduction
        # here that would bypass the shared exact primitives.
        median = .exact_median(xv),
        max = if (length(xv)) max(xv) else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }
  out <- do.call(rbind, rows)
  numcols <- c("mean", "sd", "min", "median", "max")
  out[numcols] <- lapply(out[numcols], function(v) .round_dp(v, 3))
  out
}

#' Cohen's d (pooled-SD standardised mean difference)
#'
#' @param x,y Numeric vectors.
#' @return The standardised mean difference; 0 when either sample is too small or
#'   both share one constant, `NA` when the pooled SD is zero but the means
#'   differ (the standardised difference is then unbounded, not zero).
#' @examples
#' cohens_d(c(5, 6, 7, 8), c(5, 6, 7, 9))
#' @export
cohens_d <- function(x, y) {
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  nx <- length(x); ny <- length(y)
  if (nx < 2 || ny < 2) return(0)
  sp <- sqrt(((nx - 1) * .exact_var(x) + (ny - 1) * .exact_var(y)) / (nx + ny - 2))
  if (is.na(sp) || sp == 0) {
    # Two constants: an exactly-zero difference is exactly zero SDs apart, but
    # unequal constants are infinitely many. That is undefined, not perfect balance.
    if (.exact_mean(x) - .exact_mean(y) == 0) return(0)
    return(NA_real_)
  }
  (.exact_mean(x) - .exact_mean(y)) / sp
}

#' Cohen's d with a confidence interval, complementing the TOST verdict
#'
#' The interval is the `(1 - 2 * alpha)` confidence interval for the standardised
#' mean difference; for `alpha = 0.05` this is the 90% interval that corresponds
#' exactly to a TOST decision at the .05 level (Lakens, 2017). Reporting the
#' interval, rather than only a binary verdict, makes the realised imbalance and
#' its sampling uncertainty explicit, and keeps the dependence on the number of
#' items visible. With few items the interval is wide, so a small point estimate
#' cannot be over-read as evidence of a small true difference (Sassenhagen &
#' Alday, 2016).
#'
#' @param x,y Numeric vectors.
#' @param alpha Significance level matching the TOST (default 0.05).
#' @return A list with `d`, `ci_low` and `ci_high`.
#' @importFrom stats qt
#' @export
cohens_d_ci <- function(x, y, alpha = 0.05) {
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  nx <- length(x); ny <- length(y)
  if (nx < 2 || ny < 2) return(list(d = 0, ci_low = NA_real_, ci_high = NA_real_))
  sp <- sqrt(((nx - 1) * .exact_var(x) + (ny - 1) * .exact_var(y)) / (nx + ny - 2))
  diff <- .exact_mean(x) - .exact_mean(y)
  if (is.na(sp) || sp == 0) {
    # Equal constants carry no sampling uncertainty: a point at zero. Unequal
    # constants are infinitely many SDs apart, so the estimate and its interval
    # are undefined, not a perfect [0, 0].
    if (diff == 0) return(list(d = 0, ci_low = 0, ci_high = 0))
    return(list(d = NA_real_, ci_low = NA_real_, ci_high = NA_real_))
  }
  d <- diff / sp
  margin <- stats::qt(1 - alpha, nx + ny - 2) * sqrt(1 / nx + 1 / ny)
  list(d = d, ci_low = d - margin, ci_high = d + margin)
}

#' Two one-sided tests (TOST) of equivalence on a Cohen's d bound
#'
#' Reports the larger of the two one-sided p-values; a value below `alpha`
#' supports equivalence within +/- `bound_d` standard deviations. A
#' non-significant difference test is not itself evidence of equivalence, hence
#' TOST is reported alongside the standardised mean difference (Lakens, 2017).
#'
#' @param x,y Numeric vectors.
#' @param bound_d Smallest effect size of interest (Cohen's d); defaults to the
#'   schema value of 0.5 (Lakens, 2017).
#' @param alpha Significance level.
#' @return A list with `p` and logical `equivalent`.
#' @importFrom stats pt
#' @export
tost_equiv <- function(x, y, bound_d = 0.5, alpha = 0.05) {
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  nx <- length(x); ny <- length(y)
  if (nx < 2 || ny < 2) return(list(p = NA_real_, equivalent = NA))
  sp <- sqrt(((nx - 1) * .exact_var(x) + (ny - 1) * .exact_var(y)) / (nx + ny - 2))
  if (is.na(sp) || sp == 0) {
    # Both conditions are constants (e.g. a dimension fixed by the pool, such as
    # two-character Chinese words). They are equivalent iff they share that
    # constant; the standardised difference is then exactly zero.
    if (.exact_mean(x) - .exact_mean(y) == 0) return(list(p = 0, equivalent = TRUE))
    return(list(p = 1, equivalent = FALSE))
  }
  se <- sp * sqrt(1 / nx + 1 / ny)
  if (is.na(se) || se == 0) return(list(p = NA_real_, equivalent = NA))
  bound <- bound_d * sp
  dfree <- nx + ny - 2
  diff <- .exact_mean(x) - .exact_mean(y)
  t_low <- (diff + bound) / se
  t_high <- (diff - bound) / se
  p <- max(stats::pt(t_low, dfree, lower.tail = FALSE),
           stats::pt(t_high, dfree, lower.tail = TRUE))
  list(p = p, equivalent = isTRUE(p < alpha))
}

#' Variance ratio: a distributional balance check
#'
#' The ratio of a condition's variance to the reference's, complementing the
#' mean-based Cohen's d and TOST. Two conditions can share a mean yet differ in
#' spread and still confound, which a mean-based statistic misses
#' (Armstrong et al., 2012; Austin, 2009). A ratio near 1 is balanced; a common
#' heuristic flags ratios outside roughly 0.5 to 2.
#'
#' @param cond,ref Numeric vectors (condition and reference).
#' @return The variance ratio, or `NA` when a variance is undefined.
#' @examples
#' variance_ratio(c(1, 2, 3, 4), c(1, 2, 3, 8))
#' @export
variance_ratio <- function(cond, ref) {
  cond <- cond[!is.na(cond)]; ref <- ref[!is.na(ref)]
  if (length(cond) < 2 || length(ref) < 2) return(NA_real_)
  v_ref <- .exact_var(ref)
  if (v_ref == 0) return(if (.exact_var(cond) == 0) 1 else NA_real_)
  as.numeric(.exact_var(cond) / v_ref)
}

#' Check that the levels of given columns occur equally often
#'
#' @param stimuli A stimuli data frame.
#' @param columns Columns whose level counts should be equal.
#' @return A character vector of human-readable balance warnings (empty if none).
#' @export
balance_check <- function(stimuli, columns) {
  issues <- character(0)
  for (col in columns) {
    if (!col %in% names(stimuli)) next
    tab <- table(stimuli[[col]])
    if (length(unique(as.integer(tab))) > 1) {
      issues <- c(issues, sprintf(
        "Column '%s' is unbalanced: %s", col,
        paste(sprintf("%s=%d", names(tab), as.integer(tab)), collapse = ", ")
      ))
    }
  }
  issues
}

#' Build the full match-quality report
#'
#' @param stimuli A matched-stimuli data frame (must contain `condition`).
#' @param dims Dimensions to summarise and compare.
#' @param schema The parsed global schema (equivalence settings).
#' @return A list with `descriptives` and `comparisons` data frames. Every
#'   comparison is against the first condition in order of appearance, so a design
#'   with a single condition has nothing to compare and `comparisons` comes back
#'   with its columns and no rows.
#' @export
match_report <- function(stimuli, dims, schema) {
  conds <- unique(stimuli$condition)
  anchor <- conds[1]
  desc <- describe_stimuli(stimuli, dims, by = "condition")
  bound <- schema$equivalence$bound_d %||% 0.5
  alpha <- schema$equivalence$alpha %||% 0.05
  comp <- list()
  for (cc in conds[-1]) {
    for (dim in dims) {
      x <- suppressWarnings(as.numeric(stimuli[stimuli$condition == anchor, dim, drop = TRUE]))
      y <- suppressWarnings(as.numeric(stimuli[stimuli$condition == cc, dim, drop = TRUE]))
      tt <- tost_equiv(x, y, bound_d = bound, alpha = alpha)
      ci <- cohens_d_ci(x, y, alpha = alpha)
      vr <- variance_ratio(y, x)
      comp[[length(comp) + 1]] <- data.frame(
        condition = cc, reference = anchor, dimension = dim,
        cohens_d = .round_dp(cohens_d(x, y), 3),
        d_ci_low = .round_dp(ci$ci_low, 3), d_ci_high = .round_dp(ci$ci_high, 3),
        var_ratio = if (is.na(vr)) NA_real_ else .round_dp(vr, 3),
        tost_p = .round_dp(tt$p, 4), equivalent = tt$equivalent,
        stringsAsFactors = FALSE
      )
    }
  }
  list(descriptives = desc,
       comparisons = if (length(comp)) do.call(rbind, comp) else .empty_comparisons())
}

# The comparisons frame a single-condition design produces: the columns, no rows.
# rbind() of an empty list returns NULL, which run_pipeline's seq_len(nrow(...)) loop
# then died on with "argument must be coercible to non-negative integer", naming
# neither the design nor the cause, while the Python engine went on and wrote a
# comparisons CSV with no header at all. Naming the columns here gives both engines
# the same header-only file and the same empty loop.
# Must list the same columns, in the same order, as _COMPARISON_COLUMNS in
# python_workflow/src/lexsync/validation.py.
#' @keywords internal
.empty_comparisons <- function() {
  data.frame(condition = character(0), reference = character(0),
             dimension = character(0), cohens_d = numeric(0),
             d_ci_low = numeric(0), d_ci_high = numeric(0),
             var_ratio = numeric(0), tost_p = numeric(0),
             equivalent = logical(0), stringsAsFactors = FALSE)
}

# Pearson correlation from raw sums, rounded to 9 dp so it is byte-comparable across
# the R and Python engines (not stats::cor). Mirrors validation.py _pearson.
.pearson <- function(x, y) {
  ok <- !(is.na(x) | is.na(y)); x <- x[ok]; y <- y[ok]
  if (length(x) < 2) return(NA_real_)
  dx <- x - .exact_mean(x); dy <- y - .exact_mean(y)
  denom <- sqrt(.exact_sum(dx * dx) * .exact_sum(dy * dy))
  if (denom == 0) return(0)
  .round_dp(.exact_sum(dx * dy) / denom, 9)
}

#' Realised-control report for a continuous design
#'
#' Returns the same list shape as [match_report()] (`descriptives` + `comparisons`),
#' but the comparisons describe a continuous predictor: its realised span and, for
#' each control, the Pearson correlation with the predictor (near zero when the
#' control is held constant). Mirrors match_report_continuous in validation.py.
#'
#' @param stimuli A stimuli data frame (a single "continuous" group).
#' @param predictor The spanned predictor dimension.
#' @param controls Character vector of control dimensions.
#' @param schema The parsed global schema.
#' @return A list with `descriptives` and `comparisons` data frames.
#' @export
match_report_continuous <- function(stimuli, predictor, controls, schema) {
  desc <- describe_stimuli(stimuli, c(predictor, controls), by = "condition")
  pv <- suppressWarnings(as.numeric(stimuli[[predictor]]))
  valid <- pv[!is.na(pv)]
  # NA (not -Inf) when the predictor has no span, so both engines agree.
  span <- if (length(valid) >= 2) .round_dp(max(valid) - min(valid), 3) else NA_real_
  rows <- list(data.frame(dimension = predictor, role = "predictor",
                          pearson_r = NA_real_, predictor_span = span,
                          stringsAsFactors = FALSE))
  for (cc in controls) {
    cv <- suppressWarnings(as.numeric(stimuli[[cc]]))
    r <- .pearson(pv, cv)
    rows[[length(rows) + 1L]] <- data.frame(
      dimension = cc, role = "control",
      pearson_r = if (is.na(r)) NA_real_ else .round_dp(r, 3),
      predictor_span = span, stringsAsFactors = FALSE)
  }
  list(descriptives = desc, comparisons = do.call(rbind, rows))
}
