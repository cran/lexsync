# blocks.R -- practice and filler trials: rows that are PRESENTED but not ANALYSED.
#
# Everything else in the package treats one frame as both the materials record and the
# thing the experiment runs. That works only while the two are the same set of trials,
# and they usually are not. A practice block exists to settle the participant into the
# task and is discarded before analysis; fillers exist to dilute the manipulation so it
# is less guessable, and are likewise not analysed. Both have to reach the generated
# experiment and neither belongs in the stimuli file, the descriptives or the realised
# control.
#
# The pipeline therefore splits after counterbalancing: the analysis artefacts are
# written from the main rows, and the experiment is generated from the presented rows. A `block`
# column marks which is which, and it appears ONLY when a design declares one of these
# blocks -- a design without them keeps exactly the columns it had.
#
# Where each block goes in the sequence is a methodological choice, not a convenience:
#
# Practice comes first, as its own run of trials, because its purpose is to precede the
# task. It is shuffled within itself so participants do not all meet the practice items
# in one order.
#
# Fillers are INTERLEAVED with the main trials, not appended, because a block of fillers
# at the end is not a filler -- it is a second block that participants can tell apart.
# They are merged into each list before the order is drawn, so one deterministic shuffle
# mixes them through. That does renumber the main trials, which is correct: adding
# fillers changes the sequence, and the stimuli file records where each item actually
# appeared.
#
# Both blocks appear in EVERY list. They are not counterbalanced, because they carry no
# manipulation to rotate; every participant should get the same practice.
#
# Mirrors python_workflow/src/lexsync/blocks.py.

.BLOCK_MAIN <- "main"

# Bind two block frames that need not have the same columns. A filler table carries no
# `frequency` or `old20`, and the main rows carry no filler-specific field; the union is
# taken and the gaps left missing, which is honest -- a filler has no matched frequency
# because it was never matched.
#' @keywords internal
.rbind_blocks <- function(a, b) {
  cols <- union(names(a), names(b))
  for (cl in setdiff(cols, names(a))) a[[cl]] <- NA
  for (cl in setdiff(cols, names(b))) b[[cl]] <- NA
  out <- rbind(a[, cols, drop = FALSE], b[, cols, drop = FALSE])
  rownames(out) <- NULL
  out
}

# Read one block's item table and give it a block label and a non-colliding `set`.
#
# The offset matters more than it looks. `load_items` numbers sets from 1 within
# whatever table it is given, so practice item 1 and main item 1 would both be set 1 --
# and `set` is part of the key the trial-order shuffle hashes. Two rows sharing a key
# would be ordered by a coin the package does not own.
#' @keywords internal
.load_block <- function(cfg, design, label, set_offset) {
  path <- cfg[["path"]]
  if (is.null(path)) {
    stop(sprintf("lexsync: the `%s:` block needs a `path` to an item table.", label),
         call. = FALSE)
  }
  df <- load_items(path, required_fields(design))
  df$block <- label
  df$set <- as.integer(df$set) + as.integer(set_offset)
  rownames(df) <- NULL
  df
}

#' Assemble the presented trial sequence from the main, filler and practice blocks
#'
#' @param stimuli The counterbalanced main stimuli (with `list` and `trial`).
#' @param design A parsed design configuration; reads `practice` and `fillers`.
#' @param schema The parsed global schema (provides the seed).
#' @return A list with `presented` (every trial the experiment runs, in order, with a
#'   `block` column when more than one block exists) and `report` (per-block counts and
#'   the item tables' checksums, or `NULL` when the design declares no extra block).
#' @keywords internal
.add_blocks <- function(stimuli, design, schema) {
  practice_cfg <- design[["practice"]]
  filler_cfg <- design[["fillers"]]
  if (is.null(practice_cfg) && is.null(filler_cfg)) {
    # No block column at all: a design that declares none must keep the columns it had,
    # so adding this feature moves no existing artefact.
    return(list(presented = stimuli, report = NULL))
  }
  seed <- schema$seed %||% 1L
  stimuli$block <- .BLOCK_MAIN
  if (is.null(stimuli$list)) stimuli$list <- 1L
  lists <- sort(unique(stimuli$list))
  offset <- max(c(0L, as.integer(stimuli$set)))

  fillers <- NULL
  if (!is.null(filler_cfg)) {
    fillers <- .load_block(filler_cfg, design, "filler", offset)
    offset <- max(c(offset, as.integer(fillers$set)))
  }
  practice <- NULL
  if (!is.null(practice_cfg)) {
    practice <- .load_block(practice_cfg, design, "practice", offset)
  }

  parts <- list()
  for (li in lists) {
    body <- stimuli[stimuli$list == li, , drop = FALSE]
    if (!is.null(fillers)) {
      f <- fillers; f$list <- li
      body <- .rbind_blocks(body, f)
    }
    # One shuffle over main and fillers together is what interleaves them.
    body <- .shuffle_deterministic(body, seed)
    if (!is.null(practice)) {
      p <- practice; p$list <- li
      p <- .shuffle_deterministic(p, seed)
      body <- .rbind_blocks(p, body)
    }
    body$trial <- seq_len(nrow(body))
    parts[[length(parts) + 1L]] <- body
  }
  out <- do.call(rbind, parts)
  rownames(out) <- NULL

  report <- list(blocks = list(list(block = .BLOCK_MAIN,
                                    n_per_list = sum(stimuli$list == lists[1]))))
  if (!is.null(fillers)) {
    report$blocks[[length(report$blocks) + 1L]] <- list(
      block = "filler", n_per_list = nrow(fillers), path = filler_cfg[["path"]],
      sha256 = sha256_file(filler_cfg[["path"]]),
      placement = "interleaved with the main trials by the seeded order")
  }
  if (!is.null(practice)) {
    report$blocks[[length(report$blocks) + 1L]] <- list(
      block = "practice", n_per_list = nrow(practice), path = practice_cfg[["path"]],
      sha256 = sha256_file(practice_cfg[["path"]]),
      placement = "before the main trials")
  }
  report$analysed <- .BLOCK_MAIN
  list(presented = out, report = report)
}
