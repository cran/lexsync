# pairs.R -- the pair-keyed item model: word-level norms joined onto each member of
# a prime-target pair, and continuous selection over pairs rather than words.
#
# Before this, a design could have matching or its own item table but never both:
# the continuous selector consumed a word pool while `items.source: table` bypassed
# selection entirely. A relational design needs both at once, because its predictor
# (distributional similarity, associative strength) is a property of the pair while
# its controls (frequency, length) are properties of each member.
#
# Mirrors python_workflow/src/lexsync/pairs.py.

# Column names a member may not take. The engines read all of these with `$`, and
# R's `$` partial-matches on a data frame, so a joined `word.frequency` with no bare
# `word` present would make the selector's tie-break silently sort by it while the
# Python engine raised KeyError. Rejecting the name is cheaper than defending every
# read site. Must list the same names as pairs.py.
.RESERVED_MEMBER_NAMES <- c("word", "id", "set", "list", "trial", "condition",
                            "replicate", "item")

#' @keywords internal
.check_members <- function(members, stim) {
  bad <- intersect(members, .RESERVED_MEMBER_NAMES)
  if (length(bad)) {
    stop(sprintf("lexsync: items.members may not use the reserved name(s) %s; the engines read those columns directly.",
                 paste(sprintf("'%s'", sort(bad, method = "radix")), collapse = ", ")),
         call. = FALSE)
  }
  missing <- setdiff(members, names(stim))
  if (length(missing)) {
    stop(sprintf("lexsync: items.members name column(s) the item table does not have: %s.",
                 paste(sprintf("'%s'", sort(missing, method = "radix")), collapse = ", ")),
         call. = FALSE)
  }
  invisible(TRUE)
}

# Where a pair design's member norms come from. Resolved here rather than inline so
# that run_pipeline, which loads the lexicon itself in order to apply the design's
# `norms:` block to it, asks the same question and reports the same error.
#' @keywords internal
.member_lexicon_path <- function(items_cfg, design) {
  path <- items_cfg[["lexicon"]] %||% design[["lexicon"]]
  if (is.null(path)) {
    stop("lexsync: items.members needs a lexicon (items.lexicon or the design's lexicon) to draw norms from.",
         call. = FALSE)
  }
  path
}

# Join the lexicon's dimensions onto each member, prefixed `<member>.<dimension>`.
#
# The prefix leads deliberately. `prime.frequency` is safe because `df$prime` still
# exact-matches the bare `prime` column, whereas `frequency.prime` would be a
# partial-match hazard for any code reading `df$frequency`.
#
# A member form absent from the lexicon is a hard error rather than an NA. An NA
# norm would be dropped by the control-window filter, which removes ROWS from a
# set, and a set missing one of its condition rows detonates the Latin square's
# completeness guard. Naming the first few offenders in byte order keeps the message
# deterministic across engines.
#
# `lex` is the already-loaded lexicon when the caller has one. run_pipeline passes it
# because it must apply the design's `norms:` block to that lexicon first and record
# where the norms came from; loading it here as well would read the file twice and,
# worse, would join the un-normed copy, so a semantic predictor named in `norms:`
# would silently be missing from the members.
#' @keywords internal
.join_member_norms <- function(stim, members, items_cfg, design, schema, lex = NULL) {
  .check_members(members, stim)
  lexicon <- .member_lexicon_path(items_cfg, design)
  if (is.null(lex)) lex <- load_lexicon(lexicon, schema, language = design$language)
  # `id` is a row identifier rather than a dimension; joining it would put a
  # meaningless `prime.id` in the stimuli file and in the datasheet.
  dims <- setdiff(names(lex), c("word", "language", "source", "id"))

  forms <- unique(unlist(lapply(members, function(m)
    .lower_invariant(.trim_invariant(as.character(stim[[m]])))), use.names = FALSE))
  missing <- setdiff(forms, lex$word)
  if (length(missing)) {
    shown <- utils::head(sort(missing, method = "radix"), 5L)
    stop(sprintf("lexsync: %d member form(s) are absent from lexicon '%s': %s%s.",
                 length(missing), lexicon,
                 paste(sprintf("'%s'", shown), collapse = ", "),
                 if (length(missing) > length(shown)) ", ..." else ""),
         call. = FALSE)
  }
  for (m in members) {
    key <- .lower_invariant(.trim_invariant(as.character(stim[[m]])))
    idx <- match(key, lex$word)
    for (d in dims) stim[[paste0(m, ".", d)]] <- lex[[d]][idx]
  }
  stim
}

# Collapse a pair table to one row per set, select over it, then re-expand.
#
# The re-expansion is what keeps the Latin square valid. `build_pool` and the
# control windows filter ROWS, and a filter on `prime.frequency` would keep a
# pair's related row while dropping its unrelated one, leaving a set that has no row
# for one condition. Eligibility is therefore decided at set granularity, selection
# runs on one row per set, and the result is re-expanded as a pure row subset of the
# original frame: every condition row of every surviving set is present, and no norm
# or relational value is recomputed and so none can drift.
#' @keywords internal
.select_continuous_pairs <- function(stim, items_cfg, design, schema, verbose = FALSE) {
  cfg <- design$continuous
  predictor <- cfg$predictor
  controls <- unlist(cfg$controls, use.names = FALSE)

  # build_pool silently skips a column it does not recognise, which on this path
  # would mean a mistyped filter quietly widening the selection.
  unknown <- setdiff(names(design$pool_filters %||% list()), names(stim))
  if (length(unknown)) {
    stop(sprintf("lexsync: pool_filters name column(s) the item table does not have: %s.",
                 paste(sprintf("'%s'", sort(unknown, method = "radix")), collapse = ", ")),
         call. = FALSE)
  }

  # Step A: a set is eligible only if EVERY one of its rows passes the filters.
  stim[["..lexsync_pair_row"]] <- seq_len(nrow(stim))
  passed <- build_pool(stim, design$pool_filters)
  failed <- unique(stim$set[!(stim[["..lexsync_pair_row"]] %in% passed[["..lexsync_pair_row"]])])
  eligible <- setdiff(unique(stim$set), failed)
  stim[["..lexsync_pair_row"]] <- NULL
  if (!length(eligible)) {
    stop("lexsync: no item set passes the pool filters on every one of its rows.", call. = FALSE)
  }

  # Step B: collapse to the anchor condition. The default is the byte-first
  # condition, using the same sort the Latin square uses, so the two engines agree
  # without inventing a new convention.
  anchor_cond <- items_cfg$anchor_condition %||%
    sort(unique(as.character(stim$condition)), method = "radix")[1]
  anchor <- stim[stim$set %in% eligible & as.character(stim$condition) == anchor_cond, ,
                 drop = FALSE]
  dup <- anchor$set[duplicated(anchor$set)]
  if (length(dup)) {
    stop(sprintf("lexsync: item set(s) %s have more than one '%s' row.",
                 paste(sort(unique(dup)), collapse = ", "), anchor_cond), call. = FALSE)
  }
  if (nrow(anchor) != length(eligible)) {
    stop(sprintf("lexsync: %d eligible item set(s) have no '%s' row.",
                 length(eligible) - nrow(anchor), anchor_cond), call. = FALSE)
  }
  rownames(anchor) <- NULL

  # Step C: select over the collapsed frame. `set` is the tie-break: after the
  # collapse it is unique per row, it is an integer, and load_items already derived
  # it deterministically. No `word` column is needed anywhere on this path.
  sel <- select_continuous_stimuli(anchor, design, schema, verbose = verbose,
                                   key = "set", label = NULL, renumber_sets = FALSE)

  # Step D: re-expand as a pure row subset, preserving the item table's own order.
  out <- stim[stim$set %in% sel$set, , drop = FALSE]
  rownames(out) <- NULL
  # The selector's audit rides on `sel` as an attribute, which the row subset
  # above does not carry; without the copy a window relaxation on a pair design
  # would never reach the run log or the datasheet.
  attr(out, "audit") <- attr(sel, "audit")

  # Step E: report on the COLLAPSED frame. On the expanded one every target would
  # be counted once per condition, and the predictor-control correlations would be
  # computed over duplicated rows.
  report <- match_report_continuous(sel, predictor, controls, schema)
  list(stim = out, report = report, n_eligible = length(eligible))
}
