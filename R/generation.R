# generation.R -- deterministic, orthographically-controlled pseudoword generation.
# For each real base word a pseudoword is produced by constrained letter
# substitution: change the fewest letters so that every resulting bigram is
# attested in the corpus, the form is not a real word, and the length is
# preserved. Among the legal non-word neighbours the most bigram-plausible is
# chosen, with byte-order (radix) tie-breaks, so the R and Python engines generate
# the identical pseudoword from the identical corpus. A deterministic orthographic
# cousin of Wuggy (Keuleers & Brysbaert, 2010). Mirrors generation.py.

#' Integer counts of adjacent letter bigrams across a word list
#' @keywords internal
bigram_counts <- function(words) {
  words <- as.character(words)
  pairs <- unlist(lapply(words, function(w) {
    n <- nchar(w)
    if (n < 2L) character(0) else substring(w, 1:(n - 1L), 2:n)
  }), use.names = FALSE)
  tab <- table(pairs)
  out <- as.integer(tab)
  names(out) <- names(tab)
  out
}

.bg_of <- function(w) {
  n <- nchar(w)
  if (n < 2L) character(0) else substring(w, 1:(n - 1L), 2:n)
}

.bg_legal <- function(cand, bigrams) all(.bg_of(cand) %in% names(bigrams))

.bg_score <- function(cand, bigrams) {
  v <- bigrams[.bg_of(cand)]
  sum(ifelse(is.na(v), 0L, v))
}

#' The most bigram-plausible legal non-word at the smallest edit distance
#'
#' Searches single-letter substitutions first, then two-letter substitutions;
#' candidates are ranked by summed bigram frequency with a byte-order tie-break,
#' so the choice is deterministic and identical across engines.
#'
#' @param word The base word to derive the pseudoword from.
#' @param bigrams Named integer vector of bigram counts, from [bigram_counts()].
#' @param lexset Environment used as a set of the real word forms a pseudoword
#'   must avoid.
#' @param usedset Environment used as a set of the pseudowords already taken.
#' @return A single pseudoword string, or `NULL` if no legal candidate exists.
#' @export
make_pseudoword <- function(word, bigrams, lexset, usedset) {
  word <- as.character(word)
  L <- nchar(word)
  chars <- strsplit(word, "", fixed = TRUE)[[1]]
  cands <- character(0)
  for (pos in seq_len(L)) {
    for (ch in letters) {
      if (ch == chars[pos]) next
      cand <- word
      substr(cand, pos, pos) <- ch
      if (exists(cand, envir = lexset, inherits = FALSE)) next
      if (exists(cand, envir = usedset, inherits = FALSE)) next
      if (.bg_legal(cand, bigrams)) cands <- c(cands, cand)
    }
  }
  if (length(cands) == 0L) {
    for (i in seq_len(L)) for (j in seq_len(L)) {
      if (j <= i) next
      for (ci in letters) {
        if (ci == chars[i]) next
        for (cj in letters) {
          if (cj == chars[j]) next
          cand <- word
          substr(cand, i, i) <- ci
          substr(cand, j, j) <- cj
          if (exists(cand, envir = lexset, inherits = FALSE)) next
          if (exists(cand, envir = usedset, inherits = FALSE)) next
          if (.bg_legal(cand, bigrams)) cands <- c(cands, cand)
        }
      }
    }
  }
  if (length(cands) == 0L) return(NULL)
  scores <- vapply(cands, .bg_score, integer(1), bigrams = bigrams)
  cands[order(-scores, cands, method = "radix")[1]]
}

#' A length-matched pseudoword for each base word (byte-order processing)
#'
#' @param base_words Character vector of words to derive pseudowords from.
#' @param reference_words Character vector (typically the full lexicon) that
#'   supplies the bigram statistics and the real word forms to avoid.
#' @return A data frame with columns `base_word` and `pseudoword`.
#' @export
generate_pseudowords <- function(base_words, reference_words) {
  base <- as.character(base_words)
  bigrams <- bigram_counts(reference_words)
  lexset <- new.env(hash = TRUE, parent = emptyenv())
  for (w in unique(as.character(reference_words))) assign(w, TRUE, envir = lexset)
  usedset <- new.env(hash = TRUE, parent = emptyenv())
  pseudo <- character(length(base))
  for (i in order(base, method = "radix")) {
    pw <- make_pseudoword(base[i], bigrams, lexset, usedset)
    if (is.null(pw)) {
      stop(sprintf("lexsync: could not generate a pseudoword for '%s'.", base[i]), call. = FALSE)
    }
    assign(pw, TRUE, envir = usedset)
    pseudo[i] <- pw
  }
  data.frame(base_word = base, pseudoword = pseudo, stringsAsFactors = FALSE)
}

# Order in which subsyllabic constituents are considered for substitution: codas
# and nuclei vary more freely than onsets. Mirrors generation.py _ROLE_ORDER.
.ROLE_ORDER <- c(coda = 0L, nucleus = 1L, onset = 2L)

#' Split a word into ordered (role, text) subsyllabic constituents
#'
#' Nuclei are the maximal vowel runs; consonants before the first nucleus form the
#' first onset, those after the last nucleus the final coda, and a consonant run
#' between two nuclei is split at its midpoint (floor(m/2) to the left coda). An
#' orthographic model for Latin a-z words only: any word with a character outside
#' a-z (accented, hyphenated, digit) returns an empty list, as does a word with no
#' vowel, and the caller falls back to letter substitution. Mirrors
#' segment_subsyllabic in generation.py.
#' @keywords internal
segment_subsyllabic <- function(word) {
  w <- .lower_invariant(word)
  # Subsyllabic segmentation is an orthographic model for Latin a-z words; any word
  # with a character outside a-z returns an empty list so the caller falls back to
  # letter substitution (keeps the engines in step; avoids multi-byte edge cases).
  if (!grepl("^[a-z]+$", w, perl = TRUE)) return(list())   # perl: locale-independent ASCII a-z
  L <- nchar(w)
  m <- gregexpr(.VOWELS, w, perl = TRUE)[[1]]
  if (length(m) == 1L && m[1] == -1L) return(list())
  starts <- as.integer(m)
  ends <- starts + attr(m, "match.length") - 1L
  out <- list()
  add <- function(role, text) out[[length(out) + 1L]] <<- list(role = role, text = text)
  if (starts[1] > 1L) add("onset", substr(w, 1L, starts[1] - 1L))
  ns <- length(starts)
  for (si in seq_len(ns)) {
    add("nucleus", substr(w, starts[si], ends[si]))
    run_start <- ends[si] + 1L
    run_end <- if (si < ns) starts[si + 1L] - 1L else L
    run <- if (run_end >= run_start) substr(w, run_start, run_end) else ""
    mrun <- nchar(run)
    if (si < ns) {
      cut <- mrun %/% 2L
      if (cut > 0L) add("coda", substr(run, 1L, cut))
      if (mrun - cut > 0L) add("onset", substr(run, cut + 1L, mrun))
    } else if (mrun > 0L) {
      add("coda", run)
    }
  }
  out
}

#' Attested subsyllabic constituents keyed by "role|length" with integer counts
#' @keywords internal
build_constituent_inventory <- function(reference_words) {
  inv <- new.env(hash = TRUE, parent = emptyenv())
  for (w in as.character(reference_words)) {
    for (cst in segment_subsyllabic(w)) {
      key <- paste0(cst$role, "|", nchar(cst$text))
      bucket <- if (exists(key, envir = inv, inherits = FALSE)) get(key, envir = inv) else integer(0)
      bucket[cst$text] <- (if (cst$text %in% names(bucket)) bucket[[cst$text]] else 0L) + 1L
      assign(key, bucket, envir = inv)
    }
  }
  inv
}

#' A pseudoword built by swapping whole subsyllabic constituents
#'
#' Up to ceil(2k/3) constituents (codas and nuclei before onsets) are each replaced
#' by an attested constituent of the same role and length, keeping every bigram legal
#' and the form a novel non-word; length is preserved. Returns NULL if no legal swap
#' exists (the caller falls back to letter substitution). Mirrors
#' make_subsyllabic_pseudoword in generation.py.
#' @keywords internal
make_subsyllabic_pseudoword <- function(word, inv, bigrams, lexset, usedset) {
  segs <- segment_subsyllabic(word)
  roles <- vapply(segs, function(c) c$role, character(1))
  if (!("nucleus" %in% roles)) return(NULL)
  k <- length(segs)
  target <- as.integer(ceiling(2 * k / 3))
  texts <- vapply(segs, function(c) c$text, character(1))
  bucket_of <- function(i) {
    key <- paste0(segs[[i]]$role, "|", nchar(texts[i]))
    if (exists(key, envir = inv, inherits = FALSE)) get(key, envir = inv) else integer(0)
  }
  elig <- Filter(function(i) length(bucket_of(i)) >= 2L, seq_len(k))
  if (length(elig)) {
    elig <- elig[order(vapply(elig, function(i) .ROLE_ORDER[[segs[[i]]$role]], integer(1)),
                       elig, method = "radix")]
  }
  changed <- 0L
  for (i in elig[seq_len(min(length(elig), target))]) {
    alts <- names(bucket_of(i)); alts <- alts[alts != texts[i]]
    cands <- character(0); scores <- integer(0); trials <- list()
    for (alt in alts) {
      trial <- texts; trial[i] <- alt
      cand <- paste0(trial, collapse = "")
      if (exists(cand, envir = lexset, inherits = FALSE)) next
      if (exists(cand, envir = usedset, inherits = FALSE)) next
      if (!.bg_legal(cand, bigrams)) next
      cands <- c(cands, cand); scores <- c(scores, .bg_score(cand, bigrams))
      trials[[length(trials) + 1L]] <- trial
    }
    if (length(cands)) {
      best <- order(-scores, cands, method = "radix")[1]
      texts <- trials[[best]]; changed <- changed + 1L
    }
  }
  if (changed == 0L) return(NULL)
  final <- paste0(texts, collapse = "")
  if (exists(final, envir = lexset, inherits = FALSE)) return(NULL)
  if (exists(final, envir = usedset, inherits = FALSE)) return(NULL)
  if (!.bg_legal(final, bigrams)) return(NULL)
  final
}

#' A subsyllabic pseudoword for each base word (with letter-substitution fallback)
#' @keywords internal
generate_pseudowords_subsyllabic <- function(base_words, reference_words) {
  base <- as.character(base_words)
  bigrams <- bigram_counts(reference_words)
  lexset <- new.env(hash = TRUE, parent = emptyenv())
  for (w in unique(as.character(reference_words))) assign(w, TRUE, envir = lexset)
  inv <- build_constituent_inventory(reference_words)
  usedset <- new.env(hash = TRUE, parent = emptyenv())
  pseudo <- character(length(base))
  for (i in order(base, method = "radix")) {
    pw <- make_subsyllabic_pseudoword(base[i], inv, bigrams, lexset, usedset)
    if (is.null(pw)) pw <- make_pseudoword(base[i], bigrams, lexset, usedset)
    if (is.null(pw)) {
      stop(sprintf("lexsync: could not generate a pseudoword for '%s'.", base[i]), call. = FALSE)
    }
    assign(pw, TRUE, envir = usedset)
    pseudo[i] <- pw
  }
  data.frame(base_word = base, pseudoword = pseudo, stringsAsFactors = FALSE)
}

#' Assemble a word-vs-pseudoword lexical-decision set from a candidate pool
#'
#' Real words are drawn by an even spread across the byte-ordered pool, then a
#' length-matched pseudoword is generated for each. The pool is first filtered to
#' lower-case a-z forms, the only ones the pseudoword generators are defined for,
#' so the eligible pool can be smaller than the request; the pipeline's shortfall
#' policy then decides whether that errors. `reference_words` (the full
#' lexicon) supplies the bigram statistics and the real-word list a pseudoword
#' must avoid. The presented string is the `target` column; conditions are `word`
#' and `pseudoword` and `set` pairs them.
#'
#' @param pool Data frame of candidate words, e.g. from [build_pool()].
#' @param n Number of real words to select.
#' @param reference_words Character vector supplying the bigram statistics and
#'   the real word forms a pseudoword must avoid; defaults to the pool's words.
#' @param method Pseudoword generator: `"letter_substitution"` (default) or
#'   `"subsyllabic"` (Wuggy-style; Keuleers & Brysbaert, 2010).
#' @return A stimulus data frame with `target`, `condition` and `set` columns.
#' @export
build_lexdec_stimuli <- function(pool, n, reference_words = NULL, method = "letter_substitution") {
  pool <- pool[order(pool$word, method = "radix"), , drop = FALSE]
  # The pseudoword generators are defined for Latin a-z words; skip others.
  pool <- pool[grepl("^[a-z]+$", pool$word, perl = TRUE), , drop = FALSE]
  if (nrow(pool) == 0L) stop("lexsync: lexical-decision pool has no a-z words.", call. = FALSE)
  n_take <- min(n, nrow(pool))
  idx <- unique(round(seq(1, nrow(pool), length.out = n_take)))
  words <- pool[idx, , drop = FALSE]
  ref <- if (!is.null(reference_words)) as.character(reference_words) else pool$word
  gen <- if (identical(method, "subsyllabic")) {
    generate_pseudowords_subsyllabic(words$word, ref)
  } else if (identical(method, "letter_substitution")) {
    generate_pseudowords(words$word, ref)
  } else {
    stop(sprintf("lexsync: unknown pseudoword generation method '%s'.", method), call. = FALSE)
  }
  pw_map <- stats::setNames(gen$pseudoword, gen$base_word)
  real <- data.frame(target = words$word, word = words$word, condition = "word",
                     length = words$length, set = seq_len(nrow(words)), stringsAsFactors = FALSE)
  pseudo <- data.frame(target = unname(pw_map[words$word]), word = unname(pw_map[words$word]),
                       condition = "pseudoword", length = words$length,
                       set = seq_len(nrow(words)), stringsAsFactors = FALSE)
  out <- rbind(real, pseudo)
  rownames(out) <- NULL
  out
}
