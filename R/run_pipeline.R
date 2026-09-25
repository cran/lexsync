# run_pipeline.R -- the orchestrator. For each design it loads a lexicon, builds
# the candidate pool, computes any missing dimensions, matches stimuli across
# conditions, counterbalances, writes the descriptive-statistics report and the
# run log, and exports the PsychoPy and OpenSesame scripts. run_all() loops over
# every design configuration, automating the work across languages and designs.

#' Run the lexsync pipeline for one design
#'
#' @param design_path Path to a design configuration (YAML).
#' @param schema_path Path to the global schema (YAML).
#' @param outdir Output directory supplied by the caller (subdirectories
#'   `stimuli`, `reports`, `experiments` are created). It must be supplied;
#'   lexsync does not choose a default output location.
#' @param reference_words Optional reference word list for neighbourhood
#'   computation; defaults to the whole lexicon.
#' @param verbose Logical; print progress.
#' @return A named list of output paths, invisibly.
#' @export
run_pipeline <- function(design_path, schema_path = "config/schema.yaml",
                         outdir = NULL, reference_words = NULL, verbose = TRUE) {
  if (is.null(outdir) || !is.character(outdir) || length(outdir) != 1L ||
      is.na(outdir) || !nzchar(outdir)) {
    stop("lexsync: outdir must be supplied; no output directory is chosen by default.",
         call. = FALSE)
  }
  # An exported function must not leave the session changed, so the narration gate
  # is restored on every exit path, error included. Mirrors the Python twin, where
  # the gate is module state restored by a decorator.
  old_opts <- options(lexsync.verbose = verbose)
  on.exit(options(old_opts), add = TRUE)
  schema <- read_config(schema_path)
  design <- read_config(design_path)
  n_req <- design$n_per_condition %||% design$n_per_cell
  # is.finite rather than is.na: trunc(Inf) is Inf, so an infinite request passed both
  # the integrality and the lower-bound test here and went on to select the whole pool,
  # while the Python engine's int() raised OverflowError on it. Both engines now refuse
  # it with this message.
  if (!is.null(n_req) &&
      (!is.numeric(n_req) || length(n_req) != 1L || !is.finite(n_req) ||
       n_req < 1 || n_req != trunc(n_req))) {
    stop("lexsync: n_per_condition must be a positive whole number.", call. = FALSE)
  }
  items_cfg <- design$items %||% list()
  source <- items_cfg$source %||% "corpus"
  paradigm <- design$paradigm %||% "factorial"
  is_continuous <- .is_continuous(design)
  if (is_continuous && identical(source, "table") &&
      !length(unlist(items_cfg$members %||% list(), use.names = FALSE))) {
    # Without members the table branch loads the rows and never selects, while the
    # log and datasheet would still record continuous mode, which is a provenance lie.
    stop("lexsync: a 'continuous' block with items.source 'table' requires items.members.",
         call. = FALSE)
  }
  if (identical(source, "table") && !is_continuous && !is.null(design$pool_filters)) {
    # Only the continuous pairs selector consumes pool_filters on the table path; a
    # curated item table is the researcher's selection, so a stray filter here is
    # always a mistake rather than a request to drop rows.
    stop(paste0("lexsync: pool_filters have no effect for items.source 'table' without ",
                "a 'continuous' block; remove them or use a continuous design."),
         call. = FALSE)
  }

  log <- new_run_log(design$name, meta = list(
    design = design$name, language = design$language,
    paradigm = paradigm, source = source, seed = schema$seed %||% NA,
    mode = if (is_continuous) "continuous" else "conditions"
  ))

  report <- NULL
  pair_eligible <- NULL
  norms <- list()
  selection_audit <- NULL

  # Read the matcher's audit attribute straight off the returned frame (rbind and
  # pd.concat drop attributes) and put any window relaxation on the run log; the
  # datasheet records it too. Returns the frame unchanged.
  take_audit <- function(stim) {
    selection_audit <<- attr(stim, "audit")
    for (rx in selection_audit$window_relaxations %||% list()) {
      log <<- log_step(log, sprintf("tolerance window relaxed for condition '%s' (%d within tolerance, %d needed)",
                                    rx$condition, rx$n_within_tolerance, rx$n_needed),
                       list(condition = rx$condition,
                            n_within_tolerance = rx$n_within_tolerance,
                            n_needed = rx$n_needed))
    }
    stim
  }

  # The design's `norms:` tables are joined onto the lexicon before the pool is
  # built, so a filter, a matched dimension or a continuous predictor may name a
  # semantic dimension lexsync does not compute. The records come back with the
  # lexicon and go into the datasheet: a norm table can carry the manipulated
  # variable itself, so the run is not reproducible from a record that does not name
  # the file and its checksum.
  join_norms <- function(lex, log) {
    joined <- .apply_norms(lex, design)
    norms <<- joined$provenance
    for (rec in joined$provenance) {
      log <- log_step(log, sprintf("joined %d norm column(s) from '%s'",
                                   length(rec$columns), rec$path),
                      list(norms = rec$path, sha256 = rec$sha256))
    }
    list(lexicon = joined$lexicon, log = log)
  }

  ref_words <- NULL
  if (source %in% c("corpus", "generate", "pool")) {
    if (identical(source, "pool")) {
      # A supplied word list, given the matcher's dimensions rather than dressed up as
      # a corpus lexicon. `reference` comes back separately because the neighbourhood
      # dimensions are properties of the language, not of the supplied list.
      pool_path <- items_cfg$path
      lexicon <- items_cfg$lexicon %||% design$lexicon
      log <- log_step(log, sprintf("loading supplied pool '%s'", pool_path))
      lp <- load_pool(pool_path, schema, lexicon = lexicon, language = design$language)
      lex <- lp$pool
      ref_words <- lp$reference
      log <- log_step(log, sprintf("supplied pool: %d words%s", nrow(lex),
                                   if (is.null(lexicon)) ""
                                   else sprintf(" (dimensions from '%s')", lexicon)),
                      list(words = nrow(lex), lexicon = lexicon %||% NA_character_))
    } else {
      lexicon <- items_cfg$lexicon %||% design$lexicon
      log <- log_step(log, sprintf("loading lexicon '%s'", lexicon))
      lex <- load_lexicon(lexicon, schema, language = design$language)
      log <- log_step(log, sprintf("lexicon loaded: %d words", nrow(lex)), list(words = nrow(lex)))
      ref_words <- lex$word
    }
    jn <- join_norms(lex, log); lex <- jn$lexicon; log <- jn$log
    # build_pool skips a filter column it does not recognise, so a misspelt key
    # would silently leave the pool unfiltered; the pair path carries the same
    # guard. Checked after the norms join, which legitimately adds filterable
    # columns.
    unknown <- setdiff(names(design$pool_filters %||% list()), names(lex))
    if (length(unknown)) {
      stop(sprintf("lexsync: pool_filters name column(s) the lexicon does not have: %s.",
                   paste(unknown, collapse = ", ")), call. = FALSE)
    }
    pool <- build_pool(lex, design$pool_filters)
    log <- log_step(log, sprintf("pool after filters: %d words", nrow(pool)), list(pool = nrow(pool)))
  }

  if (source %in% c("corpus", "pool")) {
    match_on <- unlist(design$match_on, use.names = FALSE)
    ref <- reference_words %||% ref_words
    needed <- intersect(c("n_density", "old20"), match_on)
    if (length(needed) && any(!needed %in% names(pool))) {
      log <- log_step(log, "computing orthographic neighbourhood (N, OLD20)")
      pool <- add_neighbourhood(pool, reference = ref)
    }
    if ("bigram_freq" %in% match_on && !("bigram_freq" %in% names(pool))) {
      log <- log_step(log, "computing bigram frequency (phonotactic-probability proxy)")
      pool <- add_bigram_frequency(pool, reference = ref)
    }
    if (is_continuous) {
      predictor <- design$continuous$predictor
      controls <- unlist(design$continuous$controls, use.names = FALSE)
      stim <- take_audit(select_continuous_stimuli(pool, design, schema, verbose = verbose))
      log <- log_step(log, sprintf("selected %d items spanning '%s' (continuous design)",
                                   nrow(stim), predictor), list(predictor = predictor))
      report <- match_report_continuous(stim, predictor, controls, schema)
    } else {
      if (!is.null(design$resample)) {
        # Per-replicate audits are dropped with the rbind inside resample_stimuli;
        # a relaxation there still reaches the console via verbose.
        stim <- resample_stimuli(pool, design, schema, design$resample$n_sets %||% 2L, verbose = verbose)
        log <- log_step(log, sprintf("resampled %d disjoint matched sets (%d items total)",
                                     length(unique(stim$replicate)), nrow(stim)),
                        list(conditions = paste(unique(stim$condition), collapse = ", ")))
      } else {
        stim <- take_audit(match_stimuli(pool, design, schema, verbose = verbose))
        log <- log_step(log, sprintf("matched %d items across %d conditions",
                                     nrow(stim), length(unique(stim$condition))),
                        list(conditions = paste(unique(stim$condition), collapse = ", ")))
      }
      std <- c("length", "frequency", "n_density", "old20")
      # First-occurrence-order union with match_on: sort() would give a
      # locale-collated order that drifts from the Python engine. A custom joined
      # norm the design matches on then reaches the descriptives, comparisons and
      # realised-control record, and not only the stimuli file.
      dims <- unique(c(std, match_on))
      dims <- dims[dims %in% names(stim)]
      report <- match_report(stim, dims, schema)
    }
  } else if (source == "generate") {
    n <- design$n_per_condition %||% design$n_per_cell %||% 40L
    gen_method <- items_cfg$generation$method %||% "letter_substitution"
    stim <- build_lexdec_stimuli(pool, n, reference_words = lex$word, method = gen_method)
    sf <- .resolve_policy(design, schema, "shortfall", "error", c("error", "allow"))
    realised <- length(unique(stim$set))
    if (realised < n && !identical(sf, "allow")) {
      # build_lexdec_stimuli filters the pool to a-z forms before selecting, so the
      # eligible pool can be smaller than the request without any pool_filters.
      stop(sprintf(paste0("lexsync: %d sets per condition were requested but only %d could ",
                          "be generated; the eligible a-z pool is smaller than the request, ",
                          "so lower n_per_condition or supply more words, or set matching: ",
                          "shortfall: allow to accept a smaller set."),
                   n, realised), call. = FALSE)
    }
    log <- log_step(log, sprintf("generated %d items (words + pseudowords, %s)", nrow(stim), gen_method),
                    list(conditions = paste(unique(stim$condition), collapse = ", ")))
    report <- match_report(stim, "length", schema)
  } else if (source == "table") {
    path <- items_cfg$path
    log <- log_step(log, sprintf("loading items '%s'", path))
    stim <- load_items(path, required_fields(design))
    log <- log_step(log, sprintf("loaded %d items across %d conditions",
                                 length(unique(stim$set)), length(unique(stim$condition))),
                    list(conditions = paste(unique(stim$condition), collapse = ", ")))
    members <- unlist(items_cfg$members %||% list(), use.names = FALSE)
    if (length(members)) {
      # Load the member lexicon here rather than inside .join_member_norms, so the
      # design's `norms:` block reaches the members too: a semantic predictor such as
      # `target.concreteness` needs the norm columns present before they are prefixed.
      mem_lexicon <- .member_lexicon_path(items_cfg, design)
      log <- log_step(log, sprintf("loading member lexicon '%s'", mem_lexicon))
      mem_lex <- load_lexicon(mem_lexicon, schema, language = design$language)
      jn <- join_norms(mem_lex, log); mem_lex <- jn$lexicon; log <- jn$log
      stim <- .join_member_norms(stim, members, items_cfg, design, schema, lex = mem_lex)
      log <- log_step(log, sprintf("joined word-level norms onto %s", paste(members, collapse = " and ")))
      if (all(c("prime", "target") %in% members)) {
        # The columns by name, not members[1]/members[2]: a member listed before
        # prime/target would silently redirect the overlap to the wrong pair.
        stim <- add_pair_overlap(stim, "prime", "target")
        log <- log_step(log, "computed relational dimensions (pair.lev, pair.overlap)")
      }
      if (is_continuous) {
        res <- .select_continuous_pairs(stim, items_cfg, design, schema, verbose)
        stim <- take_audit(res$stim); report <- res$report; pair_eligible <- res$n_eligible
        log <- log_step(log, sprintf("selected %d pairs spanning '%s' (%d eligible)",
                                     length(unique(stim$set)), design$continuous$predictor,
                                     res$n_eligible),
                        list(sets = length(unique(stim$set)), eligible = res$n_eligible))
      }
    }
  } else {
    stop(sprintf("lexsync: unknown item source '%s'. Known sources: corpus, pool, generate, table.",
                 source), call. = FALSE)
  }

  if (!is.null(report)) {
    for (msg in balance_check(stim, "condition")) log <- log_step(log, paste("balance:", msg))
    if (is_continuous) {
      for (i in seq_len(nrow(report$comparisons))) {
        cr <- report$comparisons[i, ]
        if (identical(cr$role, "control"))
          log <- log_step(log, sprintf("continuous: '%s' correlation with the predictor r = %s",
                                       cr$dimension, format(cr$pearson_r)))
      }
    } else {
      for (i in seq_len(nrow(report$comparisons))) {
        cr <- report$comparisons[i, ]
        ci <- if (!is.na(cr$d_ci_low) && !is.na(cr$d_ci_high))
          sprintf(" [%.2f, %.2f]", cr$d_ci_low, cr$d_ci_high) else ""
        log <- log_step(log, sprintf("equivalence %s vs %s on '%s': d = %.2f%s, TOST p = %.3f (%s)",
                                     cr$condition, cr$reference, cr$dimension, cr$cohens_d, ci, cr$tost_p,
                                     if (isTRUE(cr$equivalent)) "equivalent" else "not shown equivalent"))
      }
    }
  }

  # Balance-aware list assignment, when the design asks for it. Off by default,
  # because it changes which items a participant sees. The Latin-square recipe
  # rejects it: there every item is in every list already.
  list_of_set <- NULL
  balance <- NULL
  if (isTRUE(design$counterbalance$optimise)) {
    bl <- balance_lists(stim, design, schema)
    list_of_set <- bl$list_of_set
    balance <- bl$report
    log <- log_step(log, sprintf(
      "balanced %d item sets across %d lists on %s: cost %s -> %s in %d swap(s)",
      length(list_of_set), design$counterbalance$lists %||% 1L,
      paste(unlist(balance$dimensions), collapse = ", "),
      format(balance$cost_before, scientific = FALSE),
      format(balance$cost_after, scientific = FALSE), balance$n_swaps),
      list(cost_before = balance$cost_before, cost_after = balance$cost_after,
           swaps = balance$n_swaps))
    if (isTRUE(balance$max_passes_reached)) {
      log <- log_step(log, paste("balance: the pass bound was reached, so the search",
                                 "stopped before it ran out of improving swaps"))
    }
  }
  # The trigger codes follow the design's conditions, then any other condition in
  # the order the item source lists it (a generated lexical decision's words
  # before its pseudowords, an item table's own order). Taken here, before the
  # shuffle, because the order of first appearance after it depends on the seed.
  condition_order <- unique(c(.design_condition_names(design),
                              as.character(unique(stim$condition))))
  stim <- counterbalance(stim, design, schema, list_of_set)
  # Practice and filler trials are presented but not analysed, so the frame splits here:
  # the experiment is generated from every presented trial, the stimuli file and the
  # reports from the main ones. A design declaring neither block is unaffected, down to
  # not gaining a `block` column.
  blk <- .add_blocks(stim, design, schema)
  blocks <- blk$report
  # Realise any per-trial duration before the stimuli are written, so a jittered
  # or item-driven interval is recorded as a variable rather than living only
  # inside the generated script. Run on the presented set, so practice and filler
  # trials get their own realised durations too.
  presented <- resolve_trial_timing(blk$presented, design, schema)
  stim <- if (is.null(blocks)) presented
          else presented[presented$block == .BLOCK_MAIN, , drop = FALSE]
  rownames(stim) <- NULL
  if (!is.null(blocks)) {
    for (b in blocks$blocks) {
      log <- log_step(log, sprintf("block '%s': %d trial(s) per list%s", b$block,
                                   b$n_per_list,
                                   if (is.null(b$placement)) "" else paste0(", ", b$placement)),
                      list(block = b$block, n_per_list = b$n_per_list))
    }
    log <- log_step(log, sprintf("presented %d trial(s); %d analysed",
                                 nrow(presented), nrow(stim)))
  }

  base <- slugify(design$name, design$language)
  for (sub in c("stimuli", "reports", "experiments")) {
    dir.create(file.path(outdir, sub), recursive = TRUE, showWarnings = FALSE)
  }

  stim_path <- file.path(outdir, "stimuli", paste0(base, "_stimuli_R.csv"))
  write_csv_utf8(stim, stim_path); log <- log_artefact(log, stim_path, nrow(stim))

  desc_path <- comp_path <- NULL
  if (!is.null(report)) {
    desc_path <- file.path(outdir, "reports", paste0(base, "_descriptives_R.csv"))
    write_csv_utf8(report$descriptives, desc_path); log <- log_artefact(log, desc_path, nrow(report$descriptives))
    comp_path <- file.path(outdir, "reports", paste0(base, "_comparisons_R.csv"))
    write_csv_utf8(report$comparisons, comp_path); log <- log_artefact(log, comp_path, nrow(report$comparisons))
  }

  # Generated from the PRESENTED set: the experiment runs the practice and filler
  # trials too, even though they are absent from the stimuli file above.
  exps <- export_experiments(presented, design, schema, file.path(outdir, "experiments"), base,
                             conditions = condition_order)
  for (p in exps) log <- log_artefact(log, p)

  # A materials datasheet (machine + human readable) and a pre-registration
  # template: the shareable provenance record the reproducibility literature asks
  # for (Bochynska et al., 2023; Roettger, 2019).
  source_path <- items_cfg$path %||% items_cfg$lexicon %||% design$lexicon
  artifacts <- list(stimuli = stim_path, descriptives = desc_path,
                    comparisons = comp_path, experiments = exps)
  candidate_pool <- NULL
  if (is_continuous && identical(source, "table")) {
    # A pair design has no word pool. Its candidates are the item sets that passed
    # the filters on every one of their rows, counted at set granularity.
    candidate_pool <- list(list(condition = "eligible pairs",
                                n_candidates = pair_eligible %||% NA_integer_))
  } else if (is_continuous) {
    candidate_pool <- list(list(condition = "continuous", n_candidates = nrow(pool)))
  } else if (source %in% c("corpus", "pool")) {
    candidate_pool <- lapply(design$conditions, function(cnd)
      list(condition = cnd$name, n_candidates = nrow(build_pool(pool, cnd$define_by))))
  } else if (identical(source, "generate")) {
    candidate_pool <- list(list(condition = "words in band", n_candidates = nrow(pool)))
  }
  # The neighbourhood reference is provenance: overriding it changes n_density,
  # old20 and bigram_freq without touching any input file the datasheet hashes.
  nref <- NULL
  if (!is.null(reference_words)) {
    nref <- list(source = "user-supplied", n_words = length(reference_words),
                 sha256 = digest::digest(paste(enc2utf8(reference_words), collapse = "\n"),
                                         algo = "sha256", serialize = FALSE))
  }
  ds <- build_datasheet(design, schema, report, stim, source_path, artifacts,
                        schema$seed, engine = "R", candidate_pool = candidate_pool,
                        norms = norms, balance = balance, blocks = blocks,
                        design_path = design_path, schema_path = schema_path,
                        selection_audit = selection_audit,
                        neighbourhood_reference = nref)
  ds_json <- file.path(outdir, "reports", paste0(base, "_datasheet_R.json"))
  ds_md <- file.path(outdir, "reports", paste0(base, "_datasheet_R.md"))
  write_datasheet(ds, ds_json, ds_md)
  log <- log_artefact(log, ds_json); log <- log_artefact(log, ds_md)

  log_md <- file.path(outdir, "reports", paste0(base, "_run_log_R.md"))
  write_run_log(log, log_md, file.path(outdir, "reports", paste0(base, "_run_log_R.jsonl")))

  if (verbose) cat(sprintf("[lexsync] design '%s' complete.\n", base))
  invisible(list(stimuli = stim_path, descriptives = desc_path, comparisons = comp_path,
                 experiments = exps, log = log_md))
}

#' Run the lexsync pipeline for every design configuration
#'
#' @param config_dir Directory of `design_*.yaml` configurations.
#' @param schema_path Path to the global schema.
#' @param outdir Output directory supplied by the caller. It must be supplied;
#'   lexsync does not choose a default output location.
#' @param verbose Logical; print progress.
#' @return A named list of per-design results, invisibly.
#' @export
run_all <- function(config_dir = "config", schema_path = file.path(config_dir, "schema.yaml"),
                    outdir = NULL, verbose = TRUE) {
  if (is.null(outdir) || !is.character(outdir) || length(outdir) != 1L ||
      is.na(outdir) || !nzchar(outdir)) {
    stop("lexsync: outdir must be supplied; no output directory is chosen by default.",
         call. = FALSE)
  }
  designs <- list.files(config_dir, pattern = "^design_.*\\.ya?ml$", full.names = TRUE)
  if (!length(designs)) stop(sprintf("lexsync: no design_*.yaml files in '%s'.", config_dir), call. = FALSE)
  results <- list()
  for (d in designs) {
    if (verbose) cat(sprintf("\n=== lexsync: design '%s' ===\n", basename(d)))
    results[[basename(d)]] <- run_pipeline(d, schema_path, outdir, verbose = verbose)
  }
  if (verbose) cat(sprintf("\n[lexsync] all %d designs complete.\n", length(designs)))
  invisible(results)
}
