# counterbalancing.R -- assign stimuli to presentation lists and orders, and build
# participant counterbalancing tables. Two recipes: the original `factorial` one
# (every matched item shown, lists split the matched sets) and
# `latin_square_target` for paired/sentence paradigms (each item appears once per
# list in one rotated condition, so a target is never repeated within a list).
# Trial order within a list comes from a seeded, keyed-hash shuffle, so the R and
# Python engines produce the same order byte for byte. Mirrors
# python_workflow/src/lexsync/counterbalancing.py.

.cb_recipe <- function(design) {
  if (!is.null(design$events) && length(design$events)) return("factorial")
  name <- design$paradigm %||% "factorial"
  p <- tryCatch(get_paradigm(name), error = function(e) NULL)
  if (is.null(p)) "factorial" else (p$counterbalance %||% "factorial")
}

# A trial's position within its list is decided by a keyed hash, not a random
# number generator: each row is ranked by the SHA-256 digest of
# "seed|replicate|list|set|condition", a tuple that identifies the trial uniquely
# under either recipe. Distinct inputs to SHA-256 behave as independent uniform
# draws, so ordering by the digest realises a seeded random permutation, but as a
# pure function of the design: the same bytes from the R and Python engines on any
# platform, no generator state to save or restore, and a different order for every
# seed. R's own sample() and numpy's PCG64 could never agree on a permutation,
# which used to be the one engine-specific artefact in an otherwise byte-identical
# pipeline. The replicate term keeps independently counterbalanced item sets from
# sharing one permutation pattern.
.shuffle_deterministic <- function(df, seed) {
  rep_id <- if ("replicate" %in% names(df)) df$replicate else 0L
  # Every component is formatted explicitly rather than interpolated. Default
  # stringification is where the two engines silently part company: R renders the
  # double 42 as "42" and Python as "42.0", and a pandas column is promoted to
  # float64 by a single missing value, which would change every digest, every
  # trial order and every generated artefact with nothing to signal it. The
  # values are integers today, so this changes no output; it removes a trap.
  # enc2utf8 because digest(serialize = FALSE) hashes the stored bytes, as
  # hash_unit already does: a latin1-marked condition read from a user's CSV
  # would otherwise give a different digest from the same characters in Python.
  key <- enc2utf8(paste(.key_part(seed), .key_part(rep_id), .key_part(df$list),
                        .key_part(df$set), .key_part(df$condition), sep = "|"))
  rank <- vapply(key, function(k) digest::digest(k, algo = "sha256", serialize = FALSE),
                 character(1), USE.NAMES = FALSE)
  df <- df[order(rank, method = "radix"), , drop = FALSE]
  df$trial <- seq_len(nrow(df))
  df
}

# ---- Balance-aware list assignment (counterbalance.optimise) ----------------
#
# The factorial recipe deals item sets to lists by rank: set 1 to list 1, set 2 to
# list 2, and so on. That is reproducible but arbitrary with respect to the items
# themselves, so one list can end up holding systematically shorter or more frequent
# words than another. Where lists are given to different participants, that
# difference is confounded with the between-subjects factor.
#
# `counterbalance.optimise: true` searches for an assignment whose lists match on the
# declared dimensions instead. It is off by default: switching it on changes which
# items a participant sees, so it must be an explicit choice rather than something
# that happens to a design on upgrade.
#
# The search obeys the package's two hard rules, and both shape the code below more
# than is obvious.
#
# No RNG. The order of the search and every tie in it is decided by the seeded keyed
# hash already used for trial order, so the result is a pure function of the design.
#
# An ALL-INTEGER objective. This is not fastidiousness. Cohen's d computed in the two
# engines was measured to differ by about 3e-16 on average, which at nine decimal
# places leaves roughly a one-in-three chance over a full run that a comparison of
# two candidate swaps resolves differently in R than in Python -- and the failure is
# silent and total, different words rather than different last bits. The values are
# quantised to integers ONCE, and the search then uses only +, -, * and
# comparison, all of which are exact on integers held in doubles below 2^53. There is
# no division anywhere in the objective.

# Milli-units of a dimension's own mean, the scale every dimension is quantised to.
# Expressing all dimensions in one unit is what lets their imbalances be added
# without a per-dimension float weight: a word's length and its Zipf frequency differ
# by orders of magnitude in raw units, and summing raw deviations would silently
# optimise almost entirely for whichever dimension has the larger numbers.
#
# The constant is 1000 times the 100 that `unit` already carries (see .quantise_dim),
# so a value at the dimension's mean quantises to 1000. That factor is not cosmetic:
# at 1000 here, a value at the mean quantised to 10, which left only about ten
# distinguishable levels across a dimension's whole range, and imbalances smaller than
# a tenth of the mean were invisible to the objective. The search then traded away a
# dimension it could not see in order to improve one it could.
.BALANCE_UNIT_SCALE <- 100000L

# Guard on the objective's magnitude. The arithmetic is exact only while every
# intermediate stays below 2^53; past that a sum would round and the two engines
# could disagree. Nothing near this is reachable with realistic designs (the bound is
# some four orders of magnitude above a 200-set, six-dimension design), so this is a
# tripwire rather than a limit, and it fails loudly where quiet rounding would hide
# the problem.
.BALANCE_MAX_MAGNITUDE <- 2^50

.balance_dims <- function(design) {
  cb <- design[["counterbalance"]] %||% list()
  dims <- unlist(cb[["balance_on"]] %||% list(), use.names = FALSE)
  if (!length(dims)) dims <- unlist(design[["match_on"]] %||% list(), use.names = FALSE)
  if (!length(dims) && !is.null(design[["continuous"]])) {
    dims <- c(design$continuous$predictor,
              unlist(design$continuous$controls %||% list(), use.names = FALSE))
  }
  unique(as.character(dims))
}

# Quantise one dimension to integers in milli-units of its own mean.
#
# The only floating-point steps in the whole optimiser are here, and each is a single
# IEEE operation that the standard requires to be correctly rounded, so both engines
# hold the same double before it is truncated. Truncation of a double is exact, so no
# rounding mode is involved. `round()` would have agreed on every case measured, but
# truncation removes the question rather than answering it.
#
# The mean is computed from INTEGER counts, never as a float sum: summing 20000
# doubles was measured to give three different answers across R's sum(), math.fsum,
# numpy's pairwise sum and a naive loop, so a float mean here could put the two
# engines on different search paths from the first step.
.quantise_dim <- function(values, name) {
  if (any(is.na(values))) {
    stop(sprintf(paste("lexsync: dimension '%s' has missing values, so it cannot be",
                       "balanced across lists. Fill or drop those items."), name),
         call. = FALSE)
  }
  # Integer magnitudes first, so the unit is reproducible without a float reduction.
  # sum() over an integer vector accumulates in a 64-bit integer and returns a double
  # if the total leaves int range (measured on R 4.6.1: no warning, no NA), so the
  # total is exact either way while it stays under 2^53, which the per-value bound
  # below and any realistic number of items keep it well inside.
  scaled <- trunc(abs(as.numeric(values)) * 100)
  if (max(scaled, 0) > .Machine$integer.max) {
    stop(sprintf("lexsync: dimension '%s' has values too large to balance on.", name),
         call. = FALSE)
  }
  unit <- max(1L, sum(as.integer(scaled)) %/% length(scaled))
  trunc(as.numeric(values) * .BALANCE_UNIT_SCALE / unit)
}

# One integer per set per dimension: the set's total, because a list receives whole
# sets. Rows are grouped by set in byte order so the two engines build the same
# matrix from the same frame.
.balance_values <- function(stimuli, dims) {
  sets <- sort(unique(stimuli$set))
  V <- matrix(0, nrow = length(dims), ncol = length(sets),
              dimnames = list(dims, as.character(sets)))
  for (i in seq_along(dims)) {
    q <- .quantise_dim(stimuli[[dims[i]]], dims[i])
    # A plain integer sum per set: exact, so grouping order cannot matter.
    for (j in seq_along(sets)) V[i, j] <- sum(q[stimuli$set == sets[j]])
  }
  V
}

# Total absolute deviation of each list's dimension total from its fair share,
# scaled by the number of sets so the fair share stays an integer. Every term is an
# exact integer; there is no division.
.balance_cost <- function(V, assign, n_lists) {
  n_sets <- ncol(V)
  cost <- 0
  for (i in seq_len(nrow(V))) {
    total <- sum(V[i, ])
    for (l in seq_len(n_lists)) {
      in_l <- assign == l
      cost <- cost + abs(sum(V[i, in_l]) * n_sets - total * sum(in_l))
    }
  }
  cost
}

#' Assign item sets to counterbalancing lists so the lists match on the item dimensions
#'
#' The factorial recipe's default deal is by set rank, which balances nothing. This
#' searches instead for an assignment whose lists have near-equal totals on each
#' declared dimension, by steepest-descent pairwise swaps between lists. List sizes
#' are preserved, because a swap exchanges one set for another.
#'
#' The search is deterministic and identical in the R and Python engines: the
#' objective is all-integer (see the notes in this file), the descent takes the single
#' best swap each pass, and ties are broken by the seeded keyed hash rather than by
#' position, so no list is favoured by being numbered first. Because the cost is a
#' non-negative integer that strictly decreases, the search terminates; `max_passes`
#' bounds it anyway and the report says whether the bound was reached.
#'
#' Five situations are refused rather than answered with an assignment that would
#' mislead. A Latin-square design is refused because every item already appears in
#' every list there, so there is nothing left to equate, and fewer than two lists
#' leaves no pair of lists to exchange sets between. A design with no resolvable
#' balance dimension is refused, as is one naming a dimension the stimuli do not
#' carry, and the message names the columns. The last refusal is arithmetic: the
#' search stops if the integer objective would leave the range a double represents
#' exactly, since past that point the two engines could disagree.
#'
#' @param stimuli A stimuli data frame with a `set` column and the balance dimensions.
#' @param design A parsed design configuration. Reads `counterbalance.lists`,
#'   `counterbalance.balance_on` (defaulting to `match_on`, then to the continuous
#'   predictor and controls) and `counterbalance.max_passes`.
#' @param schema The parsed global schema (provides the seed).
#' @return A list with `list_of_set` (a named integer vector mapping each set to a
#'   list) and `report` (the dimensions, the integer cost before and after, the number
#'   of swaps taken, and whether the pass bound was reached).
#' @export
balance_lists <- function(stimuli, design, schema) {
  n_lists <- as.integer(design$counterbalance$lists %||% 1L)
  seed <- schema$seed %||% 1L
  if (identical(.cb_recipe(design), "latin_square_target")) {
    stop(paste("lexsync: counterbalance.optimise does not apply to a Latin-square",
               "design. Every item already appears in every list there, so the lists",
               "are balanced on the items by construction; the rotation decides only",
               "which condition each item takes."), call. = FALSE)
  }
  if (n_lists < 2L) {
    stop("lexsync: counterbalance.optimise needs counterbalance.lists to be 2 or more.",
         call. = FALSE)
  }
  dims <- .balance_dims(design)
  if (!length(dims)) {
    stop(paste("lexsync: counterbalance.optimise has no dimensions to balance. Name",
               "them in counterbalance.balance_on, or give the design a match_on."),
         call. = FALSE)
  }
  absent <- setdiff(dims, names(stimuli))
  if (length(absent)) {
    stop(sprintf("lexsync: cannot balance on column(s) the stimuli do not have: %s.",
                 paste(sprintf("'%s'", sort(absent, method = "radix")), collapse = ", ")),
         call. = FALSE)
  }

  sets <- sort(unique(stimuli$set))
  n_sets <- length(sets)
  V <- .balance_values(stimuli, dims)
  totals <- rowSums(V)
  if (max(abs(totals)) * n_sets > .BALANCE_MAX_MAGNITUDE) {
    stop("lexsync: the balance objective would exceed the exact-integer range.",
         call. = FALSE)
  }

  # Start from the default deal, so the search improves on the shipped behaviour
  # rather than starting somewhere unrelated to it.
  assign <- ((seq_len(n_sets) - 1L) %% n_lists) + 1L
  cost0 <- .balance_cost(V, assign, n_lists)

  # Every unordered pair of set POSITIONS, built once. The per-pass work is then
  # vectorised over pairs; a scalar loop over 5000 pairs for a few hundred passes is
  # minutes of R rather than milliseconds.
  if (n_sets > 1L) {
    pairs_i <- rep(seq_len(n_sets - 1L), times = rev(seq_len(n_sets - 1L)))
    pairs_j <- unlist(lapply(seq_len(n_sets - 1L), function(i) (i + 1L):n_sets),
                      use.names = FALSE)
  } else {
    pairs_i <- integer(0); pairs_j <- integer(0)
  }

  max_passes <- as.integer(design$counterbalance$max_passes %||% 500L)
  n_swaps <- 0L
  cost <- cost0
  hit_bound <- FALSE
  for (pass in seq_len(max(0L, max_passes))) {
    li <- assign[pairs_i]; lj <- assign[pairs_j]
    keep <- which(li != lj)
    if (!length(keep)) break
    a <- pairs_i[keep]; b <- pairs_j[keep]
    la <- li[keep]; lb <- lj[keep]
    # Per-list totals and fair shares, recomputed each pass on integers.
    n_in <- tabulate(assign, nbins = n_lists)
    delta <- numeric(length(keep))
    for (i in seq_len(nrow(V))) {
      Sl <- vapply(seq_len(n_lists), function(l) sum(V[i, assign == l]), numeric(1))
      share <- totals[i] * n_in
      va <- V[i, a]; vb <- V[i, b]
      Sa <- Sl[la]; Sb <- Sl[lb]
      delta <- delta +
        abs((Sa - va + vb) * n_sets - share[la]) +
        abs((Sb - vb + va) * n_sets - share[lb]) -
        abs(Sa * n_sets - share[la]) -
        abs(Sb * n_sets - share[lb])
    }
    best <- min(delta)
    if (best >= 0) break
    tied <- which(delta == best)
    if (length(tied) > 1L) {
      # Hash tie-break, not position: taking the first tied pair would
      # systematically prefer low-numbered sets, and the digest is the package's
      # established way of choosing without a generator. enc2utf8 for the same
      # reason as the shuffle key: digest hashes the stored bytes.
      key <- enc2utf8(paste(.key_part(seed), "balance", .key_part(sets[a[tied]]),
                            .key_part(sets[b[tied]]), sep = "|"))
      h <- vapply(key, function(k) digest::digest(k, algo = "sha256", serialize = FALSE),
                  character(1), USE.NAMES = FALSE)
      pick <- tied[order(h, method = "radix")[1]]
    } else {
      pick <- tied[1]
    }
    swap_a <- a[pick]; swap_b <- b[pick]
    tmp <- assign[swap_a]; assign[swap_a] <- assign[swap_b]; assign[swap_b] <- tmp
    cost <- cost + best
    n_swaps <- n_swaps + 1L
    if (pass == max_passes) hit_bound <- TRUE
  }

  list(list_of_set = stats::setNames(as.integer(assign), as.character(sets)),
       report = list(dimensions = as.list(dims), cost_before = cost0, cost_after = cost,
                     n_swaps = n_swaps, max_passes_reached = hit_bound,
                     cost_unit = paste("summed absolute deviation of each list's",
                                       "dimension total from its fair share, in",
                                       "milli-units of the dimension's mean, scaled",
                                       "by the number of item sets")))
}

#' Assign stimuli to lists and a randomised, reproducible trial order
#'
#' Dispatches on the design's paradigm: the factorial recipe for matched word
#' lists, or a Latin square over conditions for paired/sentence paradigms.
#'
#' @param stimuli A stimuli data frame (matched set or loaded item table).
#' @param design A parsed design configuration.
#' @param schema The parsed global schema (provides the seed).
#' @param list_of_set Optional named integer vector mapping each `set` to a list,
#'   from [balance_lists()]. Supplied by the pipeline when
#'   `counterbalance.optimise` is on; when `NULL` the factorial recipe deals sets to
#'   lists by rank as before.
#' @return `stimuli` with added `list` and `trial` columns.
#' @export
counterbalance <- function(stimuli, design, schema, list_of_set = NULL) {
  # Resampled designs counterbalance each replicate (an independent item set) on
  # its own, so trial order is numbered within each replicate.
  if ("replicate" %in% names(stimuli) && length(unique(stimuli$replicate)) > 1L) {
    parts <- lapply(split(stimuli, stimuli$replicate), function(g) {
      rownames(g) <- NULL
      .counterbalance_one(g, design, schema, list_of_set)
    })
    out <- do.call(rbind, parts); rownames(out) <- NULL
    return(out)
  }
  .counterbalance_one(stimuli, design, schema, list_of_set)
}

.counterbalance_one <- function(stimuli, design, schema, list_of_set = NULL) {
  if (identical(.cb_recipe(design), "latin_square_target")) {
    return(counterbalance_latin_square(stimuli, design, schema))
  }
  counterbalance_factorial(stimuli, design, schema, list_of_set)
}

#' @keywords internal
counterbalance_factorial <- function(stimuli, design, schema, list_of_set = NULL) {
  n_lists <- design$counterbalance$lists %||% 1L
  seed <- schema$seed %||% 1L

  stimuli$list <- 1L
  if (n_lists > 1) {
    sets <- sort(unique(stimuli$set))
    if (!is.null(list_of_set)) {
      # A balanced assignment from balance_lists(). Looked up by set VALUE, since the
      # map is keyed by it; a set the map does not name is a caller error rather than
      # a silent fall-back to list 1.
      key <- as.character(stimuli$set)
      missing <- setdiff(unique(key), names(list_of_set))
      if (length(missing)) {
        stop(sprintf("lexsync: the balanced list assignment does not cover set(s) %s.",
                     paste(sort(missing, method = "radix"), collapse = ", ")),
             call. = FALSE)
      }
      stimuli$list <- as.integer(list_of_set[key])
    } else {
      # Deal by set RANK, not by set value, so the deal does not depend on `set`
      # being contiguous and starting at 1 (Python's enumerate() does the same).
      deal <- ((seq_along(sets) - 1L) %% n_lists) + 1L
      stimuli$list <- deal[match(stimuli$set, sets)]
    }
  }

  parts <- split(stimuli, stimuli$list)
  stimuli <- do.call(rbind, lapply(parts, .shuffle_deterministic, seed = seed))
  rownames(stimuli) <- NULL
  stimuli
}

#' One row per item per list, condition rotated across lists (Latin square)
#'
#' Each item (`set`) contributes exactly one trial to a list, so its target is
#' never repeated within a list; conditions are balanced because items rotate
#' through them. With `lists` unset the number of lists equals the number of
#' conditions. Mirrors the Python recipe (byte-order condition list, zero-based
#' rotation), so the two engines assign the same condition to each item per list.
#' @keywords internal
counterbalance_latin_square <- function(stimuli, design, schema) {
  seed <- schema$seed %||% 1L
  conds <- sort(unique(as.character(stimuli$condition)), method = "radix")
  n_cond <- length(conds)
  n_lists <- design$counterbalance$lists %||% n_cond
  sets <- sort(unique(stimuli$set))

  parts <- list()
  for (li in seq_len(n_lists) - 1L) {
    rows <- vector("list", length(sets))
    for (si in seq_along(sets) - 1L) {
      s <- sets[si + 1L]
      cond <- conds[((si + li) %% n_cond) + 1L]
      sel <- stimuli[stimuli$set == s & stimuli$condition == cond, , drop = FALSE]
      if (nrow(sel) == 0L) {
        stop(sprintf("lexsync: item set %s has no row for condition '%s'.", s, cond), call. = FALSE)
      }
      rows[[si + 1L]] <- sel[1, , drop = FALSE]
    }
    df <- do.call(rbind, rows)
    df$list <- li + 1L
    df <- .shuffle_deterministic(df, seed)
    parts[[length(parts) + 1L]] <- df
  }
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out
}

#' Build a participant counterbalancing table
#'
#' Crosses the supplied counterbalancing factors and replicates the cells to
#' cover `n_participants`, generalising the `expand.grid()` + replication pattern
#' of the original workflow's `participant_parameters.R`.
#'
#' @param factors A named list of factors, each a vector of levels.
#' @param n_participants Number of participants to allocate.
#' @return A data frame with one row per participant.
#' @examples
#' participant_table(list(list = 1:2, order = c("a", "b")), 6)
#' @export
participant_table <- function(factors, n_participants) {
  grid <- expand.grid(factors, stringsAsFactors = FALSE)
  reps <- ceiling(n_participants / nrow(grid))
  idx <- rep(seq_len(nrow(grid)), reps)[seq_len(n_participants)]
  tab <- grid[idx, , drop = FALSE]
  tab$participant <- seq_len(n_participants)
  rownames(tab) <- NULL
  tab
}
