# datasheet.R -- materials datasheet and pre-registration template. A machine- and
# human-readable provenance record for a design: where the items came from, how
# they were selected and matched, the realised control, how they were
# counterbalanced, and the seeds, versions and checksums needed to reproduce them
# exactly. The shareable materials record whose scarcity motivates lexsync
# (Bochynska et al., 2023; Roettger, 2019). Mirrors datasheet.py.

# 1.1 added `materials_source$norms` (the design's joined norm tables, with their
# checksums and per-column coverage) and, for a pair-keyed design, a `relational`
# block plus an honest `selection$cross_engine`. Both were required by the rule that
# anything affecting item selection is recorded here: a norm table can carry the
# manipulated variable itself, and the pair path performs a real selection that the
# record used to describe as "n/a (user-supplied items)".
DATASHEET_VERSION <- "1.1"

# Datasheet labels for the pseudoword generators in generation.R, keyed by the
# items.generation.method token. Kept character-for-character identical to
# _GENERATION_LABELS in datasheet.py so the two engines' records stay comparable.
.GENERATION_LABELS <- list(
  letter_substitution = "constrained letter substitution (deterministic pseudowords)",
  subsyllabic = "subsyllabic constituent swap (Wuggy-style, deterministic pseudowords)"
)

.versions_R <- function(engine) {
  v <- list(engine = engine,
            lexsync = tryCatch(as.character(utils::packageVersion("lexsync")),
                               error = function(e) "0.1.0"),
            R = paste(R.version$major, R.version$minor, sep = "."))
  for (p in c("readr", "stringdist", "jsonlite", "digest", "yaml", "stringi")) {
    pv <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) NULL)
    if (!is.null(pv)) v[[p]] <- pv
  }
  # The OS completes the environment record; per-engine, like the rest of the block.
  v$os <- paste(Sys.info()[["sysname"]], Sys.info()[["machine"]])
  v
}

# Whether the R and Python engines select byte-identical materials. The
# deterministic methods are byte-identical; mahalanobis and optimal are the
# exception (covariance inverse / assignment solver; see matching.R). Mirrors
# datasheet.py.
#
# `selected` distinguishes the two things an item table can now mean. A plain table
# design does no selection, so there is nothing for the engines to agree on and "n/a"
# is the whole story. A pair-keyed continuous design selects over that table, and that
# selection was measured to be byte-identical, so answering "n/a" there understated
# the guarantee on the one path where a reader most needs it.
.cross_engine <- function(method, source, selected = FALSE) {
  if (identical(source, "table") && !isTRUE(selected)) return("n/a (user-supplied items)")
  if (!is.null(method) && method %in% c("mahalanobis", "optimal"))
    return("approximate (platform linear algebra)")
  "byte-identical"
}

# The pair-keyed part of the record, or NULL for a design that is not pair-keyed.
#
# Derived from the design and the realised stimuli rather than passed in, so both
# engines compute it from the same two objects and cannot disagree about it.
#
# Three of these fields answer questions the rest of the datasheet gets wrong for a
# pair design. `n_pairs` is stated because `items$n_total` counts ROWS, which is one
# per pair per condition, so a reader comparing it against the design's
# `n_per_condition` would find it doubled. The member lexicon is named and checksummed
# because it is where every member-level control came from, and nothing else in the
# record mentions it. `materials_source` names the item table. And the member
# dimensions are separated from the relational ones because they are different kinds
# of variable: `target.frequency` is a property of one word, `pair.overlap` is a
# property of the pair, and only the second is unavailable from any word-level norm
# database. Mirrors datasheet.py.
.relational_record <- function(design, stimuli) {
  items_cfg <- design$items %||% list()
  members <- unlist(items_cfg[["members"]] %||% list(), use.names = FALSE)
  if (!length(members)) return(NULL)
  cols <- names(stimuli)
  # The union over members, not the first member's alone. .join_member_norms gives
  # every member the same dimensions, so the sets coincide in anything the pipeline
  # produced; taking the union means a hand-built frame cannot report an empty list
  # merely because the first member happens to carry no prefixed column.
  # startsWith, not a regex: a member name is user-supplied and may contain a
  # character a regex would read as syntax.
  member_dims <- unique(unlist(lapply(members, function(m) {
    prefix <- paste0(m, ".")
    substring(cols[startsWith(cols, prefix)], nchar(prefix) + 1L)
  }), use.names = FALSE))
  lexicon <- items_cfg[["lexicon"]] %||% design[["lexicon"]]
  list(members = as.list(members),
       n_pairs = length(unique(stimuli$set)),
       member_lexicon = lexicon,
       member_lexicon_sha256 = sha256_file(lexicon),
       member_dimensions = as.list(sort(member_dims, method = "radix")),
       relational_dimensions = as.list(sort(cols[startsWith(cols, "pair.")], method = "radix")))
}

# The tolerance windows as match_stimuli resolves them (schema defaults, overridden
# per dimension by the design), the same resolution the matcher performs. The
# nearest-neighbour methods apply these windows; the pairwise joint/optimal methods
# never consult them, which is why the datasheet attaches this block only for the
# methods that do. Mirrors datasheet.py.
.resolve_tolerance_k <- function(design, schema) {
  tol_k <- schema$matching$tolerance_k %||% list()
  if (!is.null(design$matching$tolerance_k))
    tol_k <- utils::modifyList(tol_k, design$matching$tolerance_k)
  tol_k
}

.controlled_dims <- function(design, source) {
  # A supplied pool goes through the same matcher as a corpus, so it controls the same
  # dimensions; only the origin of the candidate words differs.
  if (source %in% c("corpus", "pool")) unlist(design$match_on, use.names = FALSE)
  else if (identical(source, "generate")) "length"
  else character(0)
}

.r4 <- function(v) if (is.null(v) || is.na(v)) NULL else .round_dp(as.numeric(v), 4)

# A suggested crossed mixed-model formula for the design. Handing the user an
# items-crossed model guards against the language-as-fixed-effect fallacy
# (Clark, 1973; Baayen et al., 2008): items are a random sample of the language,
# so an analysis that treats them as fixed over-generalises. Mirrors datasheet.py.
.analysis_R <- function(design, source) {
  cont <- design$continuous
  if (!is.null(cont)) {
    predictor <- cont$predictor
    controls <- unlist(cont$controls, use.names = FALSE)
    fixed <- paste(c(predictor, controls), collapse = " + ")
    note <- paste0("The predictor is kept continuous and analysed by regression or a ",
                   "mixed model rather than dichotomised (Kuperman, 2015; Liben-Nowell ",
                   "et al., 2019); the controls enter as covariates. Crossed random ",
                   "effects for subjects and items guard the language-as-fixed-effect ",
                   "fallacy (Clark, 1973; Baayen et al., 2008); reduce the structure if ",
                   "it does not converge (Matuschek et al., 2017).")
    # A pair design's member-prefixed terms carry a dot, which R formulas accept
    # but Patsy reads as syntax, so the Python analyst needs the quoting stated.
    if (any(grepl(".", c(predictor, controls), fixed = TRUE)))
      note <- paste0(note, " Dotted dimension names are valid in R formulas but must ",
                     "be quoted as Q(\"...\") in Patsy-style Python interfaces.")
    return(list(
      response = "the trial outcome (e.g. reaction time or accuracy)",
      suggested_model = sprintf("response ~ %s + (1 + %s | subject) + (1 | item)", fixed, predictor),
      note = note
    ))
  }
  paradigm <- design$paradigm %||% "factorial"
  if (identical(source, "generate") || identical(paradigm, "lexical_decision")) {
    factor <- "lexicality"; item_re <- "(1 | item)"
  } else if (paradigm %in% c("priming", "self_paced_reading")) {
    factor <- "condition"; item_re <- "(1 + condition | item)"
  } else {
    factor <- "condition"; item_re <- "(1 | item)"
  }
  list(
    response = "the trial outcome (e.g. reaction time or accuracy)",
    suggested_model = sprintf("response ~ %s + (1 + %s | subject) + %s", factor, factor, item_re),
    note = paste0("Crossed random effects for subjects and items guard against the ",
                  "language-as-fixed-effect fallacy (Clark, 1973; Baayen et al., 2008). ",
                  "Begin with this maximal structure (Barr et al., 2013) and reduce it ",
                  "if the model does not converge (Matuschek et al., 2017). The formula ",
                  "is lme4 syntax, for lme4 in R or pymer4 in Python; statsmodels ",
                  "MixedLM cannot take it directly and needs the random effects ",
                  "restated in its own arguments. The equivalence tests in the realised ",
                  "control are post-selection diagnostics on deterministically selected ",
                  "items, not inferential tests over a sample.")
  )
}

#' Assemble the materials datasheet for one design
#'
#' @param design A parsed design list.
#' @param schema The parsed global schema (`schema.yaml`).
#' @param report Match report data frame, from [match_report()] or
#'   [match_report_continuous()], or `NULL` for generated or tabled items.
#' @param stimuli The selected stimulus data frame.
#' @param source_path Path of the lexicon or item table the stimuli came from.
#' @param artifacts Named list of the artifact paths written for the design
#'   (stimuli, descriptives, comparisons, experiments).
#' @param seed The integer seed recorded for the counterbalanced trial order.
#' @param engine Engine label recorded in the record (default `"R"`).
#' @param candidate_pool Optional list of per-condition candidate-pool sizes
#'   (`list(condition, n_candidates)`) recording how many items satisfied each
#'   condition's window before matching; reported for selection transparency.
#' @param norms Optional list of norm-table provenance records, from the design's
#'   `norms:` block (see the pipeline). Each names a file, its sha256, the join key
#'   and the per-column coverage. Recorded because a norm table can supply the very
#'   variable a design manipulates, so a record that omitted it would describe a
#'   selection over columns of unstated origin.
#' @param balance Optional balance-optimiser report, from [balance_lists()].
#'   Recorded because it decides which items each participant sees.
#' @param blocks Optional practice/filler block report. Recorded because those
#'   trials are presented but not analysed, so the presented and analysed counts
#'   differ and the record must say why.
#' @param design_path,schema_path Optional paths of the design and schema files
#'   the run read; when given, their sha256 checksums complete the
#'   reproducibility record, because those two files decide everything the seed
#'   does not.
#' @param selection_audit Optional matcher audit record; its `window_relaxations`
#'   entries are recorded because a relaxed window changes what "matched" means
#'   for that condition.
#' @param neighbourhood_reference Optional record of the lexicon the
#'   neighbourhood dimensions were computed against
#'   (`list(source, n_words, sha256)`), recorded verbatim.
#' @return The datasheet as a nested list, ready for [write_datasheet()].
#' @export
build_datasheet <- function(design, schema, report, stimuli, source_path, artifacts,
                            seed, engine = "R", candidate_pool = NULL, norms = NULL,
                            balance = NULL, blocks = NULL, design_path = NULL,
                            schema_path = NULL, selection_audit = NULL,
                            neighbourhood_reference = NULL) {
  source <- design$items$source %||% "corpus"
  is_continuous <- .is_continuous(design)
  controlled <- .controlled_dims(design, source)
  relational <- .relational_record(design, stimuli)
  conditions <- unique(as.character(stimuli$condition))

  realised <- list()
  if (!is.null(report)) {
    cmp <- report$comparisons
    for (i in seq_len(nrow(cmp))) {
      if (is_continuous) {
        realised[[length(realised) + 1L]] <- list(
          dimension = cmp$dimension[i], role = cmp$role[i],
          pearson_r = .r4(cmp$pearson_r[i]),
          predictor_span = .r4(cmp$predictor_span[i])
        )
      } else {
        realised[[length(realised) + 1L]] <- list(
          dimension = cmp$dimension[i],
          role = if (cmp$dimension[i] %in% controlled) "controlled" else "manipulated/free",
          cohens_d = .r4(cmp$cohens_d[i]),
          ci_low = .r4(cmp$d_ci_low[i]), ci_high = .r4(cmp$d_ci_high[i]),
          var_ratio = .r4(cmp$var_ratio[i]),
          tost_p = .r4(cmp$tost_p[i]),
          equivalent = if (is.na(cmp$equivalent[i])) NULL else isTRUE(cmp$equivalent[i])
        )
      }
    }
  }

  selection <- if (is_continuous) {
    # The controls are banded by the same tolerance windows the matcher uses, so
    # the record states them here too; without them the banding is unreproducible.
    list(method = "continuous even-spread (predictor spanned, controls banded)",
         predictor = design$continuous$predictor,
         controls = as.list(unlist(design$continuous$controls, use.names = FALSE)),
         tolerance_k = .resolve_tolerance_k(design, schema))
  } else if (source %in% c("corpus", "pool")) {
    method <- design$matching$method %||% schema$matching$method %||% "standardised_euclidean"
    sel <- list(method = method, match_on = as.list(controlled))
    if (method %in% c("joint", "optimal")) {
      # The pairwise methods rank whole pairs and never consult the tolerance
      # windows; recording tolerance_k here would claim a filter that was not
      # applied. They get the cap they do apply instead, with a per-condition
      # verdict on whether it fired.
      entries <- Filter(function(x) !is.null(x$condition) && !is.null(x$n_candidates) &&
                          !is.na(x$n_candidates), candidate_pool %||% list())
      sel$candidate_cap <- list(
        cap = as.integer(.PAIRWISE_CAP),
        applied = stats::setNames(
          lapply(entries, function(x) isTRUE(x$n_candidates > .PAIRWISE_CAP)),
          vapply(entries, function(x) as.character(x$condition), character(1))))
    } else {
      sel$tolerance_k <- .resolve_tolerance_k(design, schema)
    }
    sel
  } else if (identical(source, "generate")) {
    gen_method <- design$items$generation$method %||% "letter_substitution"
    list(method = .GENERATION_LABELS[[gen_method]] %||%
           paste0(gen_method, " (deterministic pseudowords)"),
         generation_method = gen_method,
         matched_on = list("length"))
  } else {
    list(method = "item table (user-supplied)")
  }
  if (!is.null(candidate_pool) && source %in% c("corpus", "pool", "generate"))
    selection$candidate_pool <- candidate_pool
  # A pair-keyed continuous design does select, over the item table.
  selection$cross_engine <- .cross_engine(selection$method, source,
                                          selected = is_continuous && !is.null(relational))
  # A relaxed window changes what "matched" means for that condition, so the
  # matcher's audit trail belongs in the record, not only in the run narration.
  # Integers only, so both engines serialise the counts identically.
  relaxations <- selection_audit$window_relaxations %||% list()
  if (length(relaxations)) {
    selection$window_relaxations <- lapply(relaxations, function(r)
      list(condition = r$condition,
           n_within_tolerance = as.integer(r$n_within_tolerance),
           n_needed = as.integer(r$n_needed)))
  }
  if (!is.null(neighbourhood_reference))
    selection$neighbourhood_reference <- neighbourhood_reference

  materials_source <- list(
    type = source, path = source_path, sha256 = sha256_file(source_path),
    provenance = if (source %in% c("corpus", "generate"))
      paste0("wordfreq (Speer, 2022), data CC BY-SA 4.0; full corpus licence and ",
             "citation at https://github.com/pablobernabeu/lexsync/blob/main/corpora/",
             "ATTRIBUTION.md")
    else if (identical(source, "pool"))
      "user-supplied word pool, matched by lexsync"
    else "user-supplied item table")
  # A supplied pool usually draws its dimensions from a corpus lexicon, and that
  # lexicon is where every matched value came from, so it is named and checksummed
  # here: `path` above records only the word list itself.
  if (identical(source, "pool")) {
    dim_lex <- design$items$lexicon %||% design$lexicon
    materials_source$dimensions_from <- if (is.null(dim_lex))
      "the supplied pool's own columns (no lexicon given)" else dim_lex
    if (!is.null(dim_lex)) materials_source$dimensions_sha256 <- sha256_file(dim_lex)
  }
  # Assigned here, because assigning NULL to a list element removes it, so a design
  # with no `norms:` block gets no key at all rather than a "norms": null that every
  # datasheet would then carry.
  if (length(norms)) materials_source$norms <- norms

  # A corpus design draws on every schema dimension, and so does a pair-keyed design:
  # every lexicon dimension is joined onto each member. A generate or plain table
  # design reports only the ones it controlled, so the record does not claim
  # dimensions that played no part in the selection. Mirrors datasheet.py. The `[`
  # form keeps the names attribute, so an empty result is a named list() and
  # serialises as {} rather than [], matching Python.
  keep_dims <- if (source %in% c("corpus", "pool") || !is.null(relational))
    rep(TRUE, length(schema$dimensions))
  else names(schema$dimensions) %in% controlled

  # The balance report is added only when the optimiser ran, for the same reason the
  # norms record is: a key that is null on every design that does not use the feature
  # is noise in a research artefact.
  counterbalancing <- list(
    recipe = if (identical(source, "table")) "latin_square_target" else "factorial",
    lists = design$counterbalance$lists %||% 1L)
  if (!is.null(balance)) counterbalancing$optimise <- balance
  # Practice and filler trials change what a participant sees but not what is
  # analysed, so the record has to state both counts: a reader comparing the stimuli
  # file against the experiment would otherwise find them a different length with no
  # explanation.
  if (!is.null(blocks)) counterbalancing$blocks <- blocks

  # The equivalence settings the report's TOST verdicts were computed against,
  # recorded so the Methods prose can state the bound it actually ran with.
  equivalence <- list(bound_d = schema$equivalence$bound_d %||% 0.5,
                      alpha = schema$equivalence$alpha %||% 0.05)

  reproducibility <- list(seed = seed, versions = .versions_R(engine))
  # The design and schema decide everything the seed does not, so their checksums
  # complete the reproducibility record when the pipeline names them.
  if (!is.null(design_path)) reproducibility$design_sha256 <- sha256_file(design_path)
  if (!is.null(schema_path)) reproducibility$schema_sha256 <- sha256_file(schema_path)

  list(
    lexsync_datasheet_version = DATASHEET_VERSION,
    design = list(name = design$name, language = design$language,
                  paradigm = design$paradigm %||% "factorial", source = source,
                  description = design$description,
                  n_per_condition = design$n_per_condition %||% design$n_per_cell),
    materials_source = materials_source,
    dimensions = schema$dimensions[keep_dims],
    selection = selection,
    relational = relational,
    analysis = .analysis_R(design, source),
    equivalence = equivalence,
    realised_control = realised,
    counterbalancing = counterbalancing,
    resampling = if (!is.null(design$resample))
      list(n_sets = design$resample$n_sets, disjoint = TRUE) else NULL,
    items = list(n_total = nrow(stimuli), n_conditions = length(conditions),
                 conditions = as.list(conditions),
                 stimuli_file = artifacts$stimuli, stimuli_sha256 = sha256_file(artifacts$stimuli)),
    reproducibility = reproducibility,
    artifacts = lapply(Filter(Negate(is.null), .artifact_paths(artifacts)),
                       function(p) list(file = p, sha256 = sha256_file(p)))
  )
}

.artifact_paths <- function(artifacts) {
  c(artifacts$stimuli, artifacts$descriptives, artifacts$comparisons,
    unlist(artifacts$experiments, use.names = FALSE))
}

# The norm tables named in the Methods prose. Basenames only: the full paths and
# checksums are in the datasheet, and a paper's Methods section wants the source, not
# the directory it happened to sit in on one machine. Mirrors datasheet.py.
.norms_note <- function(ds) {
  norms <- ds$materials_source$norms
  if (is.null(norms) || !length(norms)) return("")
  files <- paste(vapply(norms, function(x) basename(x$path), character(1)), collapse = ", ")
  sprintf(paste0(" Norm dimensions were joined from %s, whose checksums and per-column ",
                 "coverage are recorded in the datasheet."), files)
}

#' A ready-to-adapt methods paragraph rendered from a datasheet
#'
#' @param ds A datasheet list, from [build_datasheet()].
#' @return A single character string describing the materials procedure.
#' @export
methods_paragraph <- function(ds) {
  d <- ds$design
  src <- ds$materials_source$type
  n <- d$n_per_condition
  lang <- paste0(toupper(substring(d$language, 1, 1)), substring(d$language, 2))
  sel <- ds$selection
  # What was selected. A pair design's unit is the pair, and its `n_per_condition`
  # counts pairs, so calling them "items" would misreport the size of the materials
  # by a factor of the number of conditions.
  unit <- if (is.null(ds$relational)) "items"
  else paste0(paste(unlist(ds$relational$members), collapse = "-"), " pairs")
  if (!is.null(sel$predictor)) {
    predictor <- sel$predictor
    controls <- paste(unlist(sel$controls), collapse = ", ")
    rc <- ds$realised_control
    span <- NULL; rs <- numeric(0)
    for (r in rc) {
      if (!is.null(r$predictor_span) && is.null(span)) span <- r$predictor_span
      if (!is.null(r$pearson_r)) rs <- c(rs, abs(r$pearson_r))
    }
    span_str <- if (!is.null(span)) sprintf(" (a span of %.2f)", span) else ""
    # Report |r| at 3 dp (its stored precision) so the text is identical across
    # engines; a 2-dp format of, say, 0.165 rounds to 0.16 in R and 0.17 in Python.
    corr_str <- if (length(rs))
      sprintf(", and the largest predictor-control correlation was |r| = %.3f", max(rs)) else ""
    cb <- ds$counterbalancing
    recipe_label <- switch(cb$recipe, latin_square_target = "a Latin-square rotation",
                           factorial = "a factorial split", cb$recipe)
    return(sprintf(paste0("%s %s %s were selected to span %s%s continuously while holding ",
                          "%s near-constant%s, for analysis by regression or a mixed model rather ",
                          "than a between-condition contrast (Kuperman, 2015; Liben-Nowell et al., ",
                          "2019).%s Materials were counterbalanced into %s list(s) (%s) and generated ",
                          "for PsychoPy, OpenSesame and jsPsych. The selection is deterministic and ",
                          "reproducible (seed %s; lexsync %s)."),
                   n, lang, unit, predictor, span_str, controls, corr_str, .norms_note(ds),
                   cb$lists, recipe_label,
                   ds$reproducibility$seed, ds$reproducibility$versions$lexsync))
  }
  if (identical(src, "corpus")) {
    ctrl <- paste(unlist(ds$selection$match_on), collapse = ", ")
    lead <- sprintf(paste0("%s items per condition were selected from the %s lexicon (%s) and ",
                           "matched item by item on %s using lexsync's %s matcher"),
                    n, lang, ds$materials_source$provenance, ctrl, ds$selection$method)
  } else if (identical(src, "pool")) {
    # A supplied pool is matched exactly as a corpus is; what differs, and what the
    # Methods section has to say, is that the candidate words were chosen by the
    # researcher rather than drawn from the whole lexicon.
    ctrl <- paste(unlist(ds$selection$match_on), collapse = ", ")
    lead <- sprintf(paste0("%s %s items per condition were selected from a supplied ",
                           "candidate pool and matched item by item on %s using ",
                           "lexsync's %s matcher, with the matched dimensions taken ",
                           "from %s"),
                    n, lang, ctrl, ds$selection$method,
                    ds$materials_source$dimensions_from)
  } else if (identical(src, "generate")) {
    lead <- sprintf(paste0("%s real %s words and %s length-matched pseudowords (generated by %s) ",
                           "were assembled for a lexical-decision contrast"),
                    n, lang, n, ds$selection$method)
  } else {
    per <- ds$items$n_total %/% max(1L, ds$items$n_conditions)
    lead <- sprintf("%s items were drawn from an item table for a %s design (%s)",
                    per, d$paradigm, lang)
  }
  ctrl_rows <- Filter(function(r) identical(r$role, "controlled"), ds$realised_control)
  defined <- Filter(function(r) !is.null(r$cohens_d), ctrl_rows)
  control <- ""
  if (length(ctrl_rows)) {
    # Affirmative only when the stored verdicts support it: every controlled row
    # has a defined d and every one of them passed the TOST. An undefined d
    # (constant dimensions at different constants) is the worst possible failure
    # of the matching, not an excludable row.
    all_equivalent <- length(defined) == length(ctrl_rows) &&
      all(vapply(defined, function(r) isTRUE(r$equivalent), logical(1)))
    if (all_equivalent && length(defined)) {
      worst <- defined[[which.max(vapply(defined, function(r) abs(r$cohens_d), numeric(1)))]]
      control <- sprintf(paste0(". The realised control was close. The largest standardised difference ",
                                "on any matched dimension was %.2f (90%% CI [%.2f, %.2f]), within the ",
                                "%s-SD equivalence bound"),
                         worst$cohens_d, worst$ci_low, worst$ci_high,
                         ds$equivalence$bound_d)
    } else {
      control <- paste0(". Equivalence was not confirmed on every matched dimension; ",
                        "the per-dimension differences are reported in the ",
                        "realised-control table")
    }
  }
  resamp <- if (!is.null(ds$resampling))
    sprintf(". %s disjoint matched item sets were drawn, so items can be treated as a random factor",
            ds$resampling$n_sets) else ""
  cb <- ds$counterbalancing
  recipe_label <- switch(cb$recipe,
                         latin_square_target = "a Latin-square rotation",
                         factorial = "a factorial split",
                         cb$recipe)
  tail <- sprintf(paste0(resamp, ". Materials were counterbalanced into %s list(s) (%s) and generated for ",
                         "PsychoPy, OpenSesame and jsPsych. The selection is deterministic and ",
                         "reproducible (seed %s; lexsync %s)."),
                  cb$lists, recipe_label, ds$reproducibility$seed, ds$reproducibility$versions$lexsync)
  bal <- ds$counterbalancing$optimise
  bal_note <- if (is.null(bal)) "" else sprintf(
    paste0(". Item sets were assigned to lists so as to equate the lists on %s rather ",
           "than by an arbitrary deal, by a deterministic integer search (%d swap(s); ",
           "imbalance reduced from %s to %s)"),
    paste(unlist(bal$dimensions), collapse = ", "), bal$n_swaps,
    format(bal$cost_before, scientific = FALSE),
    format(bal$cost_after, scientific = FALSE))
  cp <- ds$selection$candidate_pool
  pool_note <- ""
  if (!is.null(cp) && length(cp)) {
    sizes <- vapply(cp, function(x) if (is.null(x$n_candidates)) NA_real_ else as.numeric(x$n_candidates),
                    numeric(1))
    sizes <- sizes[!is.na(sizes)]
    if (length(sizes))
      pool_note <- sprintf(paste0(". The smallest condition was selected from %d eligible ",
                                  "candidates, and the selection was deterministic and blind ",
                                  "to any outcome measure"), as.integer(min(sizes)))
  }
  cap_rec <- ds$selection$candidate_cap
  cap_note <- ""
  if (!is.null(cap_rec) && any(vapply(cap_rec$applied, isTRUE, logical(1))))
    cap_note <- sprintf(paste0(". Each candidate pool exceeding the pairwise cap was reduced to ",
                               "the %d candidates nearest the other condition's centroid ",
                               "before pairing"), as.integer(cap_rec$cap))
  ce <- ds$selection$cross_engine %||% ""
  ce_note <- if (startsWith(ce, "approximate"))
    paste0(". This design's matching method uses a covariance inverse or an assignment ",
           "solver, so the R and Python engines select equivalent but not byte-identical ",
           "materials") else ""
  paste0(lead, control, pool_note, cap_note, ce_note, bal_note, tail, .norms_note(ds))
}

#' @keywords internal
prereg_template <- function(ds) {
  paste0(
    "## Pre-registration template\n\n",
    "*Auto-generated by lexsync. The Materials section is filled from the datasheet; ",
    "complete the remaining sections before data collection.*\n\n",
    "### Study information\n- Title:\n- Authors:\n- Research questions:\n\n",
    "### Hypotheses\n- H1:\n\n",
    "### Design\n- Manipulated variable(s):\n- Measured variable(s):\n",
    sprintf("- Paradigm: %s\n\n", ds$design$paradigm),
    "### Materials (from the lexsync datasheet)\n", methods_paragraph(ds), "\n\n",
    "### Sampling plan\n- Sample size and justification:\n- Stopping rule:\n\n",
    sprintf("### Analysis plan\n- Statistical model: %s\n  (%s)\n",
            ds$analysis$suggested_model, ds$analysis$note),
    "- Inference criteria:\n",
    "- Treatment of items (e.g. items as a random factor):\n")
}

#' @keywords internal
render_datasheet_md <- function(ds) {
  d <- ds$design
  versions <- paste(vapply(names(ds$reproducibility$versions),
                           function(k) paste(k, ds$reproducibility$versions[[k]]), character(1)),
                    collapse = ", ")
  lines <- c(
    sprintf("# Materials datasheet -- %s (%s)", d$name, d$language), "",
    sprintf("*lexsync datasheet v%s; %s engine.*", ds$lexsync_datasheet_version,
            ds$reproducibility$versions$engine), "",
    "## Provenance", "",
    sprintf("- **Paradigm:** %s  |  **Item source:** %s", d$paradigm, d$source),
    sprintf("- **Description:** %s", d$description %||% "--"),
    sprintf("- **Materials source:** `%s` (sha256 `%s...`)", ds$materials_source$path,
            substring(ds$materials_source$sha256 %||% "", 1, 16)),
    # Where a supplied pool's matched values came from. Without this the record names
    # only the word list, and the numbers every control rests on have no stated origin.
    if (!is.null(ds$materials_source$dimensions_from))
      sprintf("- **Dimensions from:** `%s`%s", ds$materials_source$dimensions_from,
              if (is.null(ds$materials_source$dimensions_sha256)) ""
              else sprintf(" (sha256 `%s...`)",
                           substring(ds$materials_source$dimensions_sha256, 1, 16)))
    else NULL,
    sprintf("- **Selection:** %s", ds$selection$method),
    sprintf("- **Cross-engine determinism:** %s", ds$selection$cross_engine %||% "byte-identical"),
    sprintf("- **Counterbalancing:** %s, %s list(s)", ds$counterbalancing$recipe,
            ds$counterbalancing$lists),
    sprintf("- **Items:** %s rows across %s conditions (%s)", ds$items$n_total,
            ds$items$n_conditions, paste(unlist(ds$items$conditions), collapse = ", ")),
    sprintf("- **Seed:** %s  |  **Versions:** %s", ds$reproducibility$seed, versions), "")
  # Norm tables, with their checksums and coverage. A dimension covering only part of
  # the lexicon matters to a reader: the uncovered rows carry NA and are dropped by
  # the tolerance windows, so coverage is part of how the pool was defined.
  nrm <- ds$materials_source$norms
  if (!is.null(nrm) && length(nrm)) {
    lines <- c(lines, "## Joined norms", "",
               "| File | Key | Column | Coverage | sha256 |", "|---|---|---|---|---|")
    for (t in nrm) {
      for (cl in t$columns) {
        lines <- c(lines, sprintf("| `%s` | %s | %s | %s / %s | `%s...` |", t$path, t$on,
                                  cl$column, cl$n_matched, cl$n_total,
                                  substring(t$sha256 %||% "", 1, 16)))
      }
    }
    lines <- c(lines, "")
  }
  # Balance-aware list assignment. Reported because it decides which items each
  # participant sees, and the before/after costs are what make the claim checkable
  # rather than a bare assertion that the lists are balanced.
  bal <- ds$counterbalancing$optimise
  if (!is.null(bal)) {
    lines <- c(lines, "## Balanced list assignment", "",
               sprintf("- **Balanced on:** %s", paste(unlist(bal$dimensions), collapse = ", ")),
               sprintf("- **Imbalance:** %s before, %s after, in %s swap(s)",
                       format(bal$cost_before, scientific = FALSE),
                       format(bal$cost_after, scientific = FALSE), bal$n_swaps),
               sprintf("- **Cost unit:** %s", bal$cost_unit),
               if (isTRUE(bal$max_passes_reached))
                 paste0("- The search stopped at its pass bound rather than at a local ",
                        "optimum, so a higher `counterbalance.max_passes` may balance ",
                        "the lists further.")
               else paste0("- The search ran to a local optimum: no single exchange of ",
                           "two item sets between lists would reduce the imbalance further."),
               paste0("- The search is a deterministic integer descent with a keyed-hash ",
                      "tie-break, so it uses no random number generator and the R and ",
                      "Python engines produce the same assignment."), "")
  }
  rel <- ds$relational
  if (!is.null(rel)) {
    lines <- c(lines, "## Pair-keyed items", "",
               sprintf("- **Members:** %s  |  **Pairs:** %s",
                       paste(unlist(rel$members), collapse = ", "), rel$n_pairs),
               sprintf("- **Member lexicon:** `%s` (sha256 `%s...`)", rel$member_lexicon,
                       substring(rel$member_lexicon_sha256 %||% "", 1, 16)),
               sprintf("- **Member-level dimensions** (one word): %s",
                       paste(unlist(rel$member_dimensions), collapse = ", ")),
               sprintf("- **Relational dimensions** (the pair): %s",
                       paste(unlist(rel$relational_dimensions), collapse = ", ")),
               paste0("- Selection ran on one row per pair and the result was re-expanded, ",
                      "so every condition row of every chosen pair is present and the ",
                      "Latin-square rotation is complete."), "")
  }
  cp <- ds$selection$candidate_pool
  if (!is.null(cp) && length(cp)) {
    parts <- paste(vapply(cp, function(x) sprintf("%s: %s", x$condition, x$n_candidates),
                          character(1)), collapse = ", ")
    lines <- c(lines, "## Selection transparency", "",
               sprintf(paste0("- **Candidate pool** (items satisfying each condition's window ",
                              "before matching): %s."), parts),
               paste0("- Selection is deterministic given the seed and blind to any outcome ",
                      "measure, so it is reproducible and free of item-selection bias ",
                      "(Forster, 2000; Simmons et al., 2011)."), "")
  }
  if (length(ds$realised_control) && !is.null(ds$selection$predictor)) {
    lines <- c(lines, "## Realised control (continuous predictor)", "",
               "| Dimension | Role | r with predictor | Predictor span |",
               "|---|---|---|---|")
    for (r in ds$realised_control) {
      rr <- if (is.null(r$pearson_r)) "--" else sprintf("%.3f", r$pearson_r)
      sp <- if (is.null(r$predictor_span)) "--" else sprintf("%.3f", r$predictor_span)
      lines <- c(lines, sprintf("| %s | %s | %s | %s |", r$dimension, r$role, rr, sp))
    }
    lines <- c(lines, "")
  } else if (length(ds$realised_control)) {
    lines <- c(lines, "## Realised control", "",
               "| Dimension | Role | Cohen's d | 90% CI | Var ratio | TOST p | Equivalent |",
               "|---|---|---|---|---|---|---|")
    for (r in ds$realised_control) {
      ci <- if (!is.null(r$ci_low)) sprintf("[%.2f, %.2f]", r$ci_low, r$ci_high) else "--"
      dstr <- if (is.null(r$cohens_d)) "--" else sprintf("%.2f", r$cohens_d)
      vr <- if (is.null(r$var_ratio)) "--" else sprintf("%.2f", r$var_ratio)
      lines <- c(lines, sprintf("| %s | %s | %s | %s | %s | %s | %s |", r$dimension, r$role,
                                dstr, ci, vr, r$tost_p %||% "--", r$equivalent %||% "--"))
    }
    lines <- c(lines, "")
  }
  a <- ds$analysis
  if (!is.null(a)) {
    lines <- c(lines, "## Suggested analysis", "",
               sprintf("- **Model:** `%s` -- where the response is %s.", a$suggested_model, a$response),
               paste0("- ", a$note), "")
  }
  lines <- c(lines, "## Methods paragraph", "", methods_paragraph(ds), "", prereg_template(ds))
  paste(lines, collapse = "\n")
}

#' Write a datasheet to a JSON record and a Markdown rendering
#'
#' @param ds A datasheet list, from [build_datasheet()].
#' @param json_path Output path for the machine-readable JSON record.
#' @param md_path Output path for the human-readable Markdown rendering.
#' @return Invisibly, the two paths written.
#' @export
write_datasheet <- function(ds, json_path, md_path) {
  # write_lines_lf, not writeLines: a text-mode connection turns every newline into
  # CRLF on Windows, so the datasheet's own bytes recorded which machine built it and
  # disagreed with the Python engine's record of the same design. The datasheet is the
  # provenance artefact, and the last file that should depend on the platform.
  # digits = NA, not jsonlite's default of 4. The default silently truncated every
  # value that had not already been rounded on the way in: a design declaring
  # `tolerance_k: 0.1111111111111111` had it recorded as 0.1111, which does not
  # reproduce the run the record exists to describe. Most fields survived only because
  # they are rounded to four places deliberately (see .r4). NA means R's full display
  # precision, 15 significant digits, which the Python engine also writes.
  write_lines_lf(jsonlite::toJSON(ds, auto_unbox = TRUE, pretty = TRUE,
                                  null = "null", na = "null", digits = NA), json_path)
  write_lines_lf(render_datasheet_md(ds), md_path)
  invisible(c(json_path, md_path))
}
