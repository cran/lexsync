# matching.R -- the multidimensional constraint-matching engine. A deterministic
# two-stage procedure: a per-dimension tolerance pre-filter (generalising the
# original workflow's mean +/- k*SD windows) followed by standardised
# nearest-neighbour assignment with a stable tie-break. Because no random number
# generator is used, the R and Python engines return identical selections from
# identical input.
#
# Cross-engine determinism: the default methods (standardised_euclidean, joint) are
# byte-identical across the R and Python engines. The optional mahalanobis and
# optimal methods are the exception, because they use a covariance-matrix inverse and
# a linear-assignment solver whose last bits differ between the two linear-algebra
# backends, so those two agree closely but not byte-for-byte.

# Candidate cap for the pairwise (joint, optimal) matchers. The datasheet reports
# whether the cap fired, so the literal lives in one place per engine.
# Must stay identical to PAIRWISE_CAP in python_workflow/src/lexsync/matching.py.
.PAIRWISE_CAP <- 1200L

# Resolve a matching policy key with a design-over-schema-over-default cascade and
# a closed vocabulary, mirroring how tolerance_k is resolved. Must stay identical
# to _resolve_policy in python_workflow/src/lexsync/matching.py.
.resolve_policy <- function(design, schema, key, default, known) {
  val <- design$matching[[key]] %||% schema$matching[[key]] %||% default
  if (!val %in% known) {
    stop(sprintf("lexsync: unknown %s policy '%s'. Known policies: %s.",
                 key, val, paste(known, collapse = ", ")), call. = FALSE)
  }
  val
}

# The shortfall contract: a selection that returns fewer sets than the design asked
# for silently invalidates the datasheet and the generated Methods text, which state
# the requested n. Under the default policy that is an error; a design may opt into
# the shrink with matching: shortfall: allow.
# Must stay identical to _check_shortfall in python_workflow/src/lexsync/matching.py.
.check_shortfall <- function(realised, n, policy, continuous = FALSE) {
  if (realised >= n || identical(policy, "allow")) return(invisible(NULL))
  if (continuous) {
    stop(sprintf(paste0("lexsync: %d items were requested but only %d could be selected; ",
                        "widen the pool or lower n_per_condition, or set matching: ",
                        "shortfall: allow to accept a smaller set."),
                 n, realised), call. = FALSE)
  }
  stop(sprintf(paste0("lexsync: %d sets per condition were requested but only %d could be ",
                      "selected; widen pool_filters/define_by or lower n_per_condition, ",
                      "or set matching: shortfall: allow to accept a smaller set."),
               n, realised), call. = FALSE)
}

# A word selected for two conditions is a confound, not a match. The anchored and
# joint paths prevent it structurally; the optimal path can only be steered away by
# a finite penalty, so the invariant is asserted on every path's output.
# Must stay identical to _assert_distinct_words in python_workflow/src/lexsync/matching.py.
.assert_distinct_words <- function(out) {
  dup <- out$word[duplicated(out$word)]
  if (length(dup)) {
    stop(sprintf(paste0("lexsync: overlapping conditions selected the word '%s' in more ",
                        "than one condition; make the conditions disjoint or lower ",
                        "n_per_condition."), dup[1]), call. = FALSE)
  }
  invisible(NULL)
}

#' Match stimuli across conditions on several lexical dimensions
#'
#' The first condition is the anchor; its items are chosen by an even spread
#' across the sorted candidate subpool. Every other condition is then matched to
#' the anchor item by item, on the `match_on` dimensions, using standardised
#' Euclidean distance under a tolerance window derived from the anchor.
#'
#' Two policies govern degraded selections, each read from the design's
#' `matching` block with the schema as fallback: `shortfall` (`"error"`, the
#' default, refuses to return fewer sets than requested; `"allow"` accepts the
#' shrink) and `on_insufficient_tolerance` (`"relax"`, the default, widens an
#' undersupplied tolerance window to the full condition subpool and records the
#' relaxation in an `"audit"` attribute; `"error"` refuses instead).
#'
#' @param pool A lexicon/pool with all `match_on` dimensions present (see
#'   [add_neighbourhood()]).
#' @param design A parsed design configuration (conditions, `match_on`,
#'   `n_per_condition`/`n_per_cell`).
#' @param schema The parsed global schema (tolerances live here).
#' @param verbose Logical; report tolerance relaxations and a shrunk anchor.
#' @return A data frame of selected stimuli with a `condition` label and a `set`
#'   index pairing matched items across conditions.
#' @export
match_stimuli <- function(pool, design, schema, verbose = FALSE) {
  conditions <- design$conditions
  if (is.null(conditions) || length(conditions) == 0) {
    # Without this, the anchor lookup below dies with a bare subscript error in R
    # and a KeyError in Python, two different messages for the same mistake.
    stop("lexsync: the design has no conditions; a matched design needs a conditions list.",
         call. = FALSE)
  }
  match_on <- unlist(design$match_on, use.names = FALSE)
  n <- design$n_per_condition %||% design$n_per_cell %||% 20L
  # Tolerance window k per dimension (window = anchor mean +/- k * SD). A design
  # may override the schema defaults per dimension, e.g. to reproduce a published
  # study's exact windows (Gonzalez Alonso et al. used SD/9 for frequency).
  tol_k <- schema$matching$tolerance_k
  if (!is.null(design$matching$tolerance_k)) {
    tol_k <- utils::modifyList(tol_k, design$matching$tolerance_k)
  }
  shortfall <- .resolve_policy(design, schema, "shortfall", "error", c("error", "allow"))
  on_tol <- .resolve_policy(design, schema, "on_insufficient_tolerance", "relax",
                            c("relax", "error"))

  for (d in match_on) {
    if (!d %in% names(pool)) {
      stop(sprintf("lexsync: dimension '%s' is absent from the pool.", d), call. = FALSE)
    }
    k <- tol_k[[d]] %||% 2
    if (is.numeric(k) && k < 0) {
      # A negative k inverts the window (upper bound below the lower), which empties
      # the candidate set and silently relaxes to the full pool, which is never
      # intended.
      stop(sprintf("lexsync: tolerance_k for dimension '%s' is negative; tolerances must be zero or positive.",
                   d), call. = FALSE)
    }
  }
  cnames <- vapply(conditions, function(cd) cd$name, character(1))
  dup_name <- cnames[duplicated(cnames)]
  if (length(dup_name)) {
    stop(sprintf("lexsync: condition name '%s' appears more than once; condition names must be unique.",
                 dup_name[1]), call. = FALSE)
  }
  for (cd in conditions) {
    for (d in names(cd$define_by)) {
      # build_pool skips a column it does not recognise, so a misspelt define_by
      # key would silently hand the condition the whole pool as its subpool and
      # the manipulated contrast would vanish while matching proceeds.
      if (!d %in% names(pool)) {
        stop(sprintf("lexsync: dimension '%s' in condition '%s' is absent from the pool.",
                     d, cd$name), call. = FALSE)
      }
    }
  }

  # Standardisation statistics taken from the whole pool, so scaling is stable
  # and identical across conditions.
  center <- vapply(match_on, function(d) .exact_mean(pool[[d]][!is.na(pool[[d]])]), numeric(1))
  scale_ <- vapply(match_on, function(d) .exact_sd(pool[[d]][!is.na(pool[[d]])]), numeric(1))
  scale_[is.na(scale_) | scale_ == 0] <- 1
  zmat <- function(df) {
    m <- as.matrix(df[, match_on, drop = FALSE])
    sweep(sweep(m, 2, center, "-"), 2, scale_, "/")
  }

  subpools <- lapply(conditions, function(cd) build_pool(pool, cd$define_by))
  cond_names <- vapply(conditions, function(cd) cd$name, character(1))

  method <- design$matching$method %||% schema$matching$method %||% "standardised_euclidean"
  # Fixed order, as documented in schema.yaml; not sort(), which is locale-collated
  # on character vectors and would drift from the Python engine's message.
  known_methods <- c("standardised_euclidean", "joint", "mahalanobis", "optimal")
  if (!method %in% known_methods) {
    stop(sprintf("lexsync: unknown matching method '%s'. Known methods: %s.",
                 method, paste(known_methods, collapse = ", ")), call. = FALSE)
  }
  if (method %in% c("joint", "optimal") && length(conditions) != 2L) {
    # Both are pairwise matchers; falling back to the anchor matcher here would make
    # the datasheet's recorded method differ from the one actually used.
    stop(sprintf("lexsync: matching method '%s' requires exactly two conditions, got %d.",
                 method, length(conditions)), call. = FALSE)
  }
  if (identical(method, "joint") && length(conditions) == 2L) {
    out <- match_joint(subpools, cond_names, match_on, center, scale_, n)
    .check_shortfall(length(unique(out$set)), n, shortfall)
    .assert_distinct_words(out)
    return(out)
  }
  if (identical(method, "optimal") && length(conditions) == 2L) {
    out <- match_optimal(subpools, cond_names, match_on, center, scale_, n)
    .check_shortfall(length(unique(out$set)), n, shortfall)
    .assert_distinct_words(out)
    return(out)
  }
  # A covariance-aware metric for Mahalanobis matching (NULL -> plain Euclidean).
  metric <- if (identical(method, "mahalanobis")) .maha_metric(zmat(pool)) else NULL

  # --- Anchor condition: deterministic even spread -------------------------
  anchor_pool <- subpools[[1]]
  if (nrow(anchor_pool) == 0) {
    stop(sprintf("lexsync: anchor condition '%s' has no candidates.", cond_names[1]), call. = FALSE)
  }
  ord_dim <- names(conditions[[1]]$define_by)[1]
  if (is.null(ord_dim) || !ord_dim %in% names(anchor_pool)) ord_dim <- "frequency"
  anchor_pool <- anchor_pool[order(anchor_pool[[ord_dim]], anchor_pool$word, method = "radix"), , drop = FALSE]
  n_take <- min(n, nrow(anchor_pool))
  idx <- unique(round(seq(1, nrow(anchor_pool), length.out = n_take)))
  anchor <- anchor_pool[idx, , drop = FALSE]
  anchor$condition <- cond_names[1]
  n_take <- nrow(anchor)
  .check_shortfall(n_take, n, shortfall)
  if (verbose && n_take < n) {
    message(sprintf("lexsync: anchor condition '%s' yields only %d items; n_per_condition is %d.",
                    cond_names[1], n_take, n))
  }
  z_anchor <- zmat(anchor)

  # Tolerance windows from the anchor distribution (Stage 1 pre-filter).
  win <- lapply(match_on, function(d) {
    m <- .exact_mean(anchor[[d]][!is.na(anchor[[d]])])
    s <- .exact_sd(anchor[[d]][!is.na(anchor[[d]])])
    k <- tol_k[[d]] %||% 2
    c(m - k * s, m + k * s)
  })
  names(win) <- match_on

  selected <- list(anchor)
  used_words <- anchor$word
  relaxations <- list()

  for (ci in seq_along(conditions)[-1]) {
    cname <- cond_names[ci]
    cand <- subpools[[ci]]
    cand <- cand[!cand$word %in% used_words, , drop = FALSE]
    if (nrow(cand) == 0) {
      stop(sprintf("lexsync: condition '%s' has no candidates left to match.", cname), call. = FALSE)
    }
    # Stage 1: tolerance pre-filter.
    keep <- rep(TRUE, nrow(cand))
    # Drop a row missing a matched dimension, as the Python engine does: NA >= x is
    # NA, and cand[NA, ] injects an all-NA filler row that would inflate the count
    # the relaxation guard below tests. The window itself can also be NA (an anchor
    # of one item has sd = NA), which makes `keep` NA even where the candidate is
    # present, so resolve NA to FALSE. The !is.na() conjunct alone does not cover it.
    for (d in match_on) keep <- keep & !is.na(cand[[d]]) & cand[[d]] >= win[[d]][1] & cand[[d]] <= win[[d]][2]
    keep[is.na(keep)] <- FALSE            # NaN comparisons are FALSE in the Python engine
    cand_f <- cand[keep, , drop = FALSE]
    if (nrow(cand_f) < n_take) {
      if (identical(on_tol, "error")) {
        stop(sprintf(paste0("lexsync: condition '%s' has %d candidates within tolerance but %d ",
                            "are needed; raise tolerance_k or set matching: ",
                            "on_insufficient_tolerance: relax to widen the window."),
                     cname, nrow(cand_f), n_take), call. = FALSE)
      }
      if (verbose) {
        message(sprintf("lexsync: condition '%s' has %d candidates within tolerance (< %d needed); relaxing the window.",
                        cname, nrow(cand_f), n_take))
      }
      # The relaxation changes what "matched" means for this condition, so it is
      # recorded on the result for the run log and datasheet, where a console message
      # would leave no trace.
      relaxations[[length(relaxations) + 1L]] <-
        list(condition = cname, n_within_tolerance = nrow(cand_f), n_needed = n_take)
      cand_f <- cand
    }
    if (nrow(cand_f) < n_take) {
      # The assignment below would otherwise re-pick an exhausted pool's first item
      # (every remaining distance is Inf, so the tie-break decides), and emit the
      # same word in several sets.
      stop(sprintf(paste0("lexsync: condition '%s' has only %d candidate(s) but %d are needed; ",
                          "widen pool_filters/define_by or lower n_per_condition."),
                   cname, nrow(cand_f), n_take), call. = FALSE)
    }
    # Relaxing the window re-admits rows missing a matched dimension. Their distance
    # is NA and they rank last, so they are never assigned; counting them would let an
    # NA-depleted pool past the guard above and back into re-picking used rows.
    usable <- sum(stats::complete.cases(cand_f[, match_on, drop = FALSE]))
    if (usable < n_take) {
      stop(sprintf(paste0("lexsync: condition '%s' has only %d usable candidate(s) complete on the ",
                          "matched dimensions but %d are needed; widen pool_filters/define_by or ",
                          "lower n_per_condition."),
                   cname, usable, n_take), call. = FALSE)
    }
    # Stage 2: standardised nearest-neighbour assignment, greedy, no replacement.
    z_cand <- zmat(cand_f)
    used <- rep(FALSE, nrow(cand_f))
    pick <- integer(n_take)
    for (a in seq_len(n_take)) {
      # Rounded to 9 dp through the shared rule so the stable tie-break below is
      # itself reproducible across R and Python. Native round() would pair R's
      # decimal algorithm with numpy's scale-rint-unscale, a pairing io_utils.R
      # documents as disagreeing at boundaries. On the mahalanobis path the
      # inputs already differ in their last bits, so the absorber must be the
      # same function in both engines.
      delta <- sweep(z_cand, 2, z_anchor[a, ], "-")
      if (is.null(metric)) {
        dvec <- .round_dp(sqrt(rowSums(delta^2)), 9)
      } else {
        dvec <- .round_dp(sqrt(pmax(rowSums((delta %*% metric) * delta), 0)), 9)
      }
      dvec[used] <- Inf
      best <- order(dvec, cand_f$word, cand_f$id, method = "radix")[1]  # stable, locale-independent tie-break
      pick[a] <- best
      used[best] <- TRUE
    }
    sel <- cand_f[pick, , drop = FALSE]
    sel$condition <- cname
    used_words <- c(used_words, sel$word)
    selected[[length(selected) + 1]] <- sel
  }

  common <- Reduce(intersect, lapply(selected, names))
  out <- do.call(rbind, lapply(selected, function(x) x[, common, drop = FALSE]))
  out$set <- rep(seq_len(n_take), times = length(conditions))
  rownames(out) <- NULL
  .assert_distinct_words(out)
  if (length(relaxations)) {
    # rbind and pd.concat both drop attributes, so the pipeline reads this
    # immediately after the call, before any reshaping.
    attr(out, "audit") <- list(window_relaxations = relaxations)
  }
  out
}

# Column means for the overlap-cap centroid, through the compensated reduction rather
# than colMeans. The cap decides which candidates survive into matching, and it fires
# for the shipped en_ndensity and es_ndensity designs, so a centroid that differs in the
# last bits between engines is a selection difference waiting to happen, not a rounding
# curiosity. Mirrors _exact_colmeans in matching.py.
.exact_colmeans <- function(z) {
  vapply(seq_len(ncol(z)), function(j) .exact_mean(z[, j]), numeric(1))
}

#' Joint nearest-pair matching for a two-condition design
#'
#' Selects the `n` best-matched pairs across the two conditions, keeping only
#' items that have a good counterpart. This equates the control dimensions more
#' tightly than per-anchor matching when the manipulation is confounded with them
#' (for example neighbourhood density with word length). Deterministic and
#' identical to the Python engine (rounded costs; byte-rank tie-breaks).
#'
#' @keywords internal
match_joint <- function(subpools, cond_names, match_on, center, scale_, n, cap = .PAIRWISE_CAP) {
  s0 <- subpools[[1]]
  s1 <- subpools[[2]]
  if (nrow(s0) == 0 || nrow(s1) == 0) {
    stop("lexsync: a condition has no candidates for joint matching.", call. = FALSE)
  }
  zof <- function(df) {
    m <- as.matrix(df[, match_on, drop = FALSE])
    sweep(sweep(m, 2, center, "-"), 2, scale_, "/")
  }
  cap_overlap <- function(df, z, centroid) {
    if (nrow(df) <= cap) return(list(df = df, z = z))
    d <- .round_dp(sqrt(rowSums(sweep(z, 2, centroid, "-")^2)), 9)
    ord <- order(d, seq_len(nrow(df)), method = "radix")[seq_len(cap)]
    keep <- sort(ord)
    list(df = df[keep, , drop = FALSE], z = z[keep, , drop = FALSE])
  }
  z0 <- zof(s0); z1 <- zof(s1)
  o0 <- cap_overlap(s0, z0, .exact_colmeans(z1)); s0 <- o0$df; z0 <- o0$z
  o1 <- cap_overlap(s1, z1, .exact_colmeans(z0)); s1 <- o1$df; z1 <- o1$z
  m0 <- nrow(z0); m1 <- nrow(z1)
  cost <- matrix(0, m0, m1)
  for (d in seq_len(ncol(z0))) cost <- cost + outer(z0[, d], z1[, d], "-")^2
  # .round_dp drops dims (as.numeric traverses column-major), so rebuild the matrix.
  cost <- matrix(.round_dp(sqrt(cost), 9), m0, m1)
  rows <- as.vector(row(cost)); cols <- as.vector(col(cost)); vals <- as.vector(cost)
  ord <- order(vals, rows, cols, method = "radix")
  used0 <- logical(m0); used1 <- logical(m1)
  pick0 <- integer(0); pick1 <- integer(0)
  # Word-level tracking, as in the anchored matcher: overlapping condition windows
  # can put one word in both subpools, where its self-pair costs exactly 0 and a
  # mirrored re-pick (x with y, then y with x) would reuse both words. For
  # disjoint conditions no word appears twice, so these skips never fire and the
  # selection is unchanged.
  used_words <- character(0)
  for (t in ord) {
    i <- rows[t]; j <- cols[t]
    if (used0[i] || used1[j]) next
    if (s0$word[i] == s1$word[j]) next
    if (s0$word[i] %in% used_words || s1$word[j] %in% used_words) next
    used0[i] <- TRUE; used1[j] <- TRUE
    used_words <- c(used_words, s0$word[i], s1$word[j])
    pick0 <- c(pick0, i); pick1 <- c(pick1, j)
    if (length(pick0) >= n) break
  }
  a <- s0[pick0, , drop = FALSE]; a$condition <- cond_names[1]
  b <- s1[pick1, , drop = FALSE]; b$condition <- cond_names[2]
  common <- intersect(names(a), names(b))
  out <- rbind(a[, common, drop = FALSE], b[, common, drop = FALSE])
  out$set <- rep(seq_len(length(pick0)), times = 2)
  rownames(out) <- NULL
  out
}

# The metric matrix for Mahalanobis distance in standardised space: the inverse of
# the pool's correlation matrix, so the distance down-weights correlated dimensions
# instead of double-counting their shared variance (Rubin, 1980; Stuart, 2010). A
# small ridge keeps it invertible under near collinearity. Uses a matrix inverse,
# so not byte-identical to the Python engine (see the file header).
.maha_metric <- function(z_pool, ridge = 1e-6) {
  if (ncol(z_pool) == 1L) return(matrix(1, 1, 1))
  cmat <- stats::cor(z_pool)
  # An undefined correlation is treated as no correlation, which covers two cases
  # worth telling apart. A constant dimension has no correlation to estimate, and
  # zero is the right reading. A dimension carrying even one missing value also
  # gives NA here, under cor()'s default use = "everything", and zeroing it drops
  # that dimension's whole row and column; if every dimension is affected the metric
  # is the identity and Mahalanobis matching becomes the standardised Euclidean
  # matching it was chosen over. Nothing upstream prevents that: the completeness
  # guards in match_stimuli count usable candidates per condition and pass easily on
  # a large pool with a handful of missing values. It is the one respect in which
  # this method is quietly sensitive to pool quality, so a design choosing it should
  # match on dimensions the lexicon covers completely.
  # Must stay identical to _maha_metric in python_workflow/src/lexsync/matching.py.
  cmat[is.na(cmat)] <- 0
  diag(cmat) <- 1
  solve(cmat + ridge * diag(ncol(cmat)))
}

#' Optimal (minimum-total-distance) pairing for a two-condition design
#'
#' Solves the linear-assignment problem globally rather than greedily, so it
#' minimises the summed pair distance and leaves fewer poorly matched pairs (Gu and
#' Rosenbaum, 1993; Hansen & Klopfer, 2006). Needs the 'clue' package. The
#' solver's tie handling differs from the Python engine's, so the two agree closely
#' but not byte-for-byte.
#'
#' @keywords internal
match_optimal <- function(subpools, cond_names, match_on, center, scale_, n, cap = .PAIRWISE_CAP) {
  if (!requireNamespace("clue", quietly = TRUE)) {
    stop("lexsync: matching method 'optimal' needs the 'clue' package (install.packages('clue')).",
         call. = FALSE)
  }
  s0 <- subpools[[1]]; s1 <- subpools[[2]]
  if (nrow(s0) == 0 || nrow(s1) == 0) {
    stop("lexsync: a condition has no candidates for optimal matching.", call. = FALSE)
  }
  zof <- function(df) {
    m <- as.matrix(df[, match_on, drop = FALSE])
    sweep(sweep(m, 2, center, "-"), 2, scale_, "/")
  }
  cap_overlap <- function(df, z, centroid) {
    if (nrow(df) <= cap) return(list(df = df, z = z))
    d <- .round_dp(sqrt(rowSums(sweep(z, 2, centroid, "-")^2)), 9)
    ord <- order(d, seq_len(nrow(df)), method = "radix")[seq_len(cap)]
    keep <- sort(ord)
    list(df = df[keep, , drop = FALSE], z = z[keep, , drop = FALSE])
  }
  z0 <- zof(s0); z1 <- zof(s1)
  o0 <- cap_overlap(s0, z0, .exact_colmeans(z1)); s0 <- o0$df; z0 <- o0$z
  o1 <- cap_overlap(s1, z1, .exact_colmeans(z0)); s1 <- o1$df; z1 <- o1$z
  m0 <- nrow(z0); m1 <- nrow(z1)
  cost <- matrix(0, m0, m1)
  for (d in seq_len(ncol(z0))) cost <- cost + outer(z0[, d], z1[, d], "-")^2
  cost <- matrix(.round_dp(sqrt(cost), 9), m0, m1)
  # A word present in both subpools (overlapping condition windows) must not be
  # paired with itself. The penalty is a large FINITE constant because both
  # solve_LSAP and scipy's linear_sum_assignment reject Inf, and their failure
  # modes differ across engines; 1e9 dominates any real distance, and the
  # selection is asserted same-word-free afterwards in match_stimuli.
  # Must stay identical to the constant in _match_optimal (matching.py).
  same <- outer(s0$word, s1$word, "==")
  cost[same] <- 1e9
  # clue::solve_LSAP needs nrow <= ncol; solve on the transpose otherwise.
  if (m0 <= m1) {
    asg <- as.integer(clue::solve_LSAP(cost)); ri <- seq_len(m0); ci <- asg
  } else {
    asg <- as.integer(clue::solve_LSAP(t(cost))); ci <- seq_len(m1); ri <- asg
  }
  pair_cost <- cost[cbind(ri, ci)]
  ord <- order(pair_cost, ri, ci, method = "radix")
  # The penalty steers the assignment away from self-pairs, but an overlapping
  # design can still mirror a pair (x with y, then y with x), reusing both words.
  # Greedy word-level dedup over the cost-ordered assignment keeps the selection
  # identical for disjoint designs, where every assigned word is distinct.
  used_words <- character(0)
  pi <- integer(0); pj <- integer(0)
  for (t in ord) {
    wi <- s0$word[ri[t]]; wj <- s1$word[ci[t]]
    if (wi == wj || wi %in% used_words || wj %in% used_words) next
    used_words <- c(used_words, wi, wj)
    pi <- c(pi, ri[t]); pj <- c(pj, ci[t])
    if (length(pi) >= n) break
  }
  a <- s0[pi, , drop = FALSE]; a$condition <- cond_names[1]
  b <- s1[pj, , drop = FALSE]; b$condition <- cond_names[2]
  common <- intersect(names(a), names(b))
  out <- rbind(a[, common, drop = FALSE], b[, common, drop = FALSE])
  out$set <- rep(seq_len(length(pi)), times = 2)
  rownames(out) <- NULL
  out
}

#' Select a set spanning a continuous predictor, holding controls constant
#'
#' Instead of dichotomising the predictor into conditions and matching, items are
#' chosen to cover the predictor's range evenly while the control dimensions are
#' held within a tolerance band, so they stay near-constant and near-uncorrelated
#' with the predictor. The set is analysed by regression / mixed models rather than
#' between-condition contrasts (Kuperman, 2015; Liben-Nowell et al., 2019). Two
#' deterministic even-spread passes make the R and Python engines select
#' byte-identical stimuli. Mirrors select_continuous_stimuli in matching.py.
#'
#' The design is checked before anything is selected, so a design that cannot be
#' honoured is refused outright. `continuous.controls` must be non-empty and must not
#' name the predictor, `match_on` must name exactly the same dimensions as
#' `continuous.controls`, every dimension named and the `key` column must be present
#' in the pool, no `tolerance_k` may be negative, and the pool must not be empty.
#'
#' @param pool A candidate pool with the predictor and control dimensions present.
#' @param design A parsed design configuration carrying a `continuous` block.
#' @param schema The parsed global schema (tolerance windows).
#' @param verbose Logical; report a window relaxation.
#' @param key Column used as the selection unit and the byte-order tie-break, by
#'   default `"word"`. The pair-keyed path passes `"set"`: after a pair table is
#'   collapsed to one row per item set there is no `word` column, and `set` is unique
#'   per row, integer, and already derived deterministically.
#' @param label Value written into the result's `condition` column, or `NULL` to leave
#'   the existing conditions alone. The pair path passes `NULL`, because its rows
#'   already carry the design's own conditions and overwriting them would destroy the
#'   contrast the design exists to test.
#' @param renumber_sets Logical; renumber the selected rows `1..n`. The pair path
#'   passes `FALSE`, because its `set` ids have to survive selection for the result to
#'   be re-expanded back to the full pair table.
#' @return A data frame of the selected stimuli. Unless `label` is `NULL` the
#'   `condition` column is set to it, `"continuous"` by default, and unless
#'   `renumber_sets` is `FALSE` the `set` column is renumbered `1..n`.
#' @export
select_continuous_stimuli <- function(pool, design, schema, verbose = FALSE,
                                      key = "word", label = "continuous",
                                      renumber_sets = TRUE) {
  cfg <- design$continuous
  predictor <- cfg$predictor
  controls <- unlist(cfg$controls, use.names = FALSE)
  match_on <- unlist(design$match_on, use.names = FALSE)
  if (length(controls) == 0) {
    stop("lexsync: a continuous design needs at least one control dimension (continuous.controls must be non-empty).",
         call. = FALSE)
  }
  if (predictor %in% controls) {
    stop(sprintf("lexsync: the continuous predictor '%s' must not also appear in continuous.controls.",
                 predictor), call. = FALSE)
  }
  if (!identical(sort(match_on), sort(controls))) {
    stop("lexsync: for a continuous design, match_on must equal continuous.controls.",
         call. = FALSE)
  }
  for (d in c(predictor, controls)) {
    if (!d %in% names(pool)) {
      stop(sprintf("lexsync: dimension '%s' is absent from the pool.", d), call. = FALSE)
    }
  }
  n <- design$n_per_condition %||% design$n_per_cell %||% 60L
  tol_k <- schema$matching$tolerance_k
  if (!is.null(design$matching$tolerance_k)) {
    tol_k <- utils::modifyList(tol_k, design$matching$tolerance_k)
  }
  shortfall <- .resolve_policy(design, schema, "shortfall", "error", c("error", "allow"))
  on_tol <- .resolve_policy(design, schema, "on_insufficient_tolerance", "relax",
                            c("relax", "error"))
  for (d in controls) {
    k <- tol_k[[d]] %||% 2
    if (is.numeric(k) && k < 0) {
      stop(sprintf("lexsync: tolerance_k for dimension '%s' is negative; tolerances must be zero or positive.",
                   d), call. = FALSE)
    }
  }
  if (!(key %in% names(pool))) {
    stop(sprintf("lexsync: the continuous tie-break column '%s' is absent from the pool.", key),
         call. = FALSE)
  }
  even_spread <- function(df) {
    if (nrow(df) == 0) return(df)
    # `key` is the deterministic tie-break when two rows share a predictor value.
    # It is `word` for a corpus pool and `set` for a collapsed pair table, where no
    # `word` column exists. Note `df[[key]]` rather than `df$word`: `$`
    # partial-matches on a data frame, so a joined `word.frequency` with no bare
    # `word` would silently sort by it in R while Python raised KeyError.
    df <- df[order(df[[predictor]], df[[key]], method = "radix"), , drop = FALSE]
    n_take <- min(n, nrow(df))
    idx <- unique(round(seq(1, nrow(df), length.out = n_take)))
    df[idx, , drop = FALSE]
  }
  # Pass 1: an even spread over the whole pool defines the control windows.
  spread <- even_spread(pool)
  if (nrow(spread) == 0) stop("lexsync: the pool is empty for the continuous design.", call. = FALSE)
  win <- lapply(controls, function(d) {
    m <- .exact_mean(spread[[d]][!is.na(spread[[d]])])
    s <- .exact_sd(spread[[d]][!is.na(spread[[d]])])
    k <- tol_k[[d]] %||% 2
    c(m - k * s, m + k * s)
  })
  names(win) <- controls
  keep <- rep(TRUE, nrow(pool))
  # As in match_stimuli: an NA on a control must drop the row rather than index an
  # all-NA filler row into `filtered` and inflate the count tested just below. A
  # one-item spread also gives sd = NA, making `keep` NA through the window bounds.
  for (d in controls) keep <- keep & !is.na(pool[[d]]) & pool[[d]] >= win[[d]][1] & pool[[d]] <= win[[d]][2]
  keep[is.na(keep)] <- FALSE            # NaN comparisons are FALSE in the Python engine
  filtered <- pool[keep, , drop = FALSE]
  relaxations <- list()
  if (nrow(filtered) < n) {
    if (identical(on_tol, "error")) {
      stop(sprintf(paste0("lexsync: %d items lie within the control windows but %d are ",
                          "needed; raise tolerance_k or set matching: ",
                          "on_insufficient_tolerance: relax to widen the window."),
                   nrow(filtered), n), call. = FALSE)
    }
    if (verbose) {
      message(sprintf("lexsync: %d items within the control windows (< %d needed); relaxing to the full pool.",
                      nrow(filtered), n))
    }
    relaxations[[1L]] <- list(condition = "continuous",
                              n_within_tolerance = nrow(filtered), n_needed = n)
    filtered <- pool
  }
  # Pass 2: an even spread over the filtered pool is the selection.
  sel <- even_spread(filtered)
  .check_shortfall(nrow(sel), n, shortfall, continuous = TRUE)
  # A pair table already carries its own `condition` and `set`, which the Latin
  # square and the trial-order digest depend on, so the pair path passes
  # label = NULL and renumber_sets = FALSE to leave both alone.
  if (!is.null(label)) sel$condition <- label
  if (isTRUE(renumber_sets)) sel$set <- seq_len(nrow(sel))
  rownames(sel) <- NULL
  if (length(relaxations)) {
    attr(sel, "audit") <- list(window_relaxations = relaxations)
  }
  sel
}

#' Produce several disjoint matched item sets (items as a random factor)
#'
#' Each replicate is an independent, fully matched set drawn from the pool with the
#' items of earlier replicates removed, so no item is reused. This lets a study
#' treat its items as a random factor (running different item samples across
#' participant groups, or showing an effect holds across samples) instead of treating
#' them as a fixed set (Clark, 1973; Yarkoni, 2022). Deterministic and identical to the
#' Python engine.
#'
#' @param pool A candidate pool with the `match_on` dimensions present.
#' @param design A parsed design configuration.
#' @param schema The parsed global schema.
#' @param n_sets Number of disjoint matched sets to draw.
#' @param verbose Logical; passed to [match_stimuli()].
#' @return A data frame of matched stimuli with an added `replicate` column. The
#'   replicates are bound together, which drops the `"audit"` attribute
#'   [match_stimuli()] uses to report a relaxed tolerance window, so a relaxation
#'   inside a replicate reaches the console under `verbose` but not the run log or
#'   the datasheet.
#' @export
resample_stimuli <- function(pool, design, schema, n_sets, verbose = FALSE) {
  used <- character(0)
  parts <- list()
  for (k in seq_len(as.integer(n_sets))) {
    pk <- pool[!(pool$word %in% used), , drop = FALSE]
    sk <- match_stimuli(pk, design, schema, verbose = verbose)
    sk$replicate <- k
    used <- c(used, sk$word)
    parts[[k]] <- sk
  }
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out
}
