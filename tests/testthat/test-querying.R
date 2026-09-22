# Every code point Python's str.strip() removes, i.e. every one whose
# str.isspace() is true: the Unicode White_Space property plus the C0
# information separators U+001C-U+001F. test_querying.py pins this same list
# against str.strip() itself, so the two engines are held to one definition.
STRIPPED <- c(0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x1C, 0x1D, 0x1E, 0x1F, 0x20,
              0x85, 0xA0, 0x1680, 0x2000:0x200A, 0x2028, 0x2029, 0x202F,
              0x205F, 0x3000)
# Format characters, not whitespace: a zero-width space, a Mongolian vowel
# separator and a word joiner must survive in both engines.
KEPT <- c(0x200B, 0x180E, 0x2060)

# The lexicon fixtures carry non-ASCII, so write the bytes rather than trusting
# the session's native encoding to render them (Windows).
write_utf8 <- function(text, path) {
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(enc2utf8(text)), con)
}

test_that(".trim_invariant strips exactly the whitespace Python strips", {
  padded <- vapply(STRIPPED, function(cp) paste0(intToUtf8(cp), "x", intToUtf8(cp)),
                   character(1))
  expect_identical(.trim_invariant(padded), rep("x", length(STRIPPED)))

  kept <- vapply(KEPT, function(cp) paste0(intToUtf8(cp), "x", intToUtf8(cp)),
                 character(1))
  expect_identical(.trim_invariant(kept), kept)

  expect_identical(.trim_invariant(NA_character_), NA_character_)
  expect_identical(.trim_invariant("  "), "")
})

test_that("load_lexicon validates and computes basic dimensions", {
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  path <- system.file("extdata", "en_example.csv", package = "lexsync")
  lex <- load_lexicon(path, schema, language = "english")
  expect_true(all(c("word", "freq_zipf", "length", "frequency", "id") %in% names(lex)))
  expect_equal(lex$length, nchar(lex$word))
  expect_false(any(duplicated(lex$word)))
})

test_that("add_neighbourhood computes Coltheart's N and OLD20", {
  df <- data.frame(word = c("cat", "car", "cap", "dog"), stringsAsFactors = FALSE)
  out <- add_neighbourhood(df, reference = df$word, n_old = 2)
  # 'cat' differs from 'car' and 'cap' by a single substitution -> N = 2
  expect_equal(out$n_density[out$word == "cat"], 2L)
  expect_true(all(out$old20 > 0))
})

test_that("build_pool filters by numeric range and membership", {
  df <- data.frame(word = letters[1:5], frequency = 1:5, pos = "n", stringsAsFactors = FALSE)
  expect_equal(nrow(build_pool(df, list(frequency = c(2, 4)))), 3)
  expect_equal(nrow(build_pool(df, list(pos = "n"))), 5)
})

# Pins the same contract as test_load_lexicon_drops_missing_words in the Python
# engine's test_querying.py: a missing word must be dropped, not coerced to a
# word-like string, or the two engines' ids diverge from this row on.
test_that("load_lexicon drops rows whose word is missing", {
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("word,freq_zipf", "cat,5.0", ",4.0", "dog,3.0", "NA,2.0", "cow,1.0"), path)
  lex <- load_lexicon(path, schema)
  expect_identical(lex$word, c("cat", "cow", "dog"))
  expect_identical(lex$id, 1:3)
})

# Pins the same contract as test_load_lexicon_strips_unicode_whitespace in the
# Python engine's test_querying.py. The word is the canonical key behind every
# byte-order tie-break, so base R's trimws() -- which leaves a no-break space or
# a form feed in place where Python's str.strip() removes them -- would key,
# sort and number such a lexicon differently from the Python engine.
test_that("load_lexicon strips the whitespace Python strips", {
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  # A no-break space, a form feed and an ideographic space: none is trimws()'s.
  pad <- intToUtf8(c(0xA0, 0x0C, 0x3000))
  write_utf8(paste0("word,freq_zipf\n",
                    "\"", pad, "dog", pad, "\",5.0\n",
                    "\" cat \",4.0\n"), path)
  lex <- load_lexicon(path, schema)
  expect_identical(lex$word, c("cat", "dog"))
  expect_identical(lex$length, c(3L, 3L))
  expect_identical(lex$id, 1:2)
})

# Pins the same contract as test_load_lexicon_keeps_zero_width_characters: the
# mirror of the above, stopping the Unicode trim from over-reaching.
test_that("load_lexicon keeps zero-width characters", {
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  zw <- intToUtf8(0x200B)
  write_utf8(paste0("word,freq_zipf\n\"", zw, "dog", zw, "\",5.0\n"), path)
  lex <- load_lexicon(path, schema)
  expect_identical(lex$word, paste0(zw, "dog", zw))
  expect_identical(lex$length, 5L)
})

# Pins the same contract as test_merge_norms_trims_the_join_key in the Python
# engine's test_querying.py: the join key gets the lexicon's own trim and
# case-fold, so a padded norm table still joins in both engines.
test_that("merge_norms joins on the same cleaned key", {
  nbsp <- intToUtf8(0xA0)
  lex <- data.frame(word = c("cat", "dog"), stringsAsFactors = FALSE)
  norms <- data.frame(word = c(paste0(nbsp, "CAT", nbsp), " dog"),
                      conc = c(1.0, 2.0), stringsAsFactors = FALSE)
  out <- merge_norms(lex, norms)
  expect_identical(out$conc, c(1.0, 2.0))
})

# Pins the same contract as test_load_lexicon_reports_an_empty_lexicon in the
# Python engine's test_querying.py. R used to die on base R's "replacement has 1
# row, data has 0" (from `df$language <- language`) while Python handed back an
# empty frame; both engines now raise the same message, naming the file.
test_that("load_lexicon reports a lexicon left with no rows", {
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  bodies <- list(
    "header-only"     = "word,freq_zipf",
    "no-words"        = c("word,freq_zipf", ",1.0", "NA,2.0"),
    "no-frequencies"  = c("word,freq_zipf", "cat,")
  )
  for (nm in names(bodies)) {
    path <- tempfile(fileext = ".csv")
    on.exit(unlink(path), add = TRUE)
    writeLines(bodies[[nm]], path)
    expect_error(load_lexicon(path, schema, language = "english"),
                 "has no usable rows", info = nm)
    # The crash was reachable only with `language`; the error must not be.
    expect_error(load_lexicon(path, schema), "has no usable rows", info = nm)
  }
})

test_that(".lower_invariant reproduces Unicode default casing whatever the locale", {
  # Written as code points so the source stays ASCII (CRAN), and so the expected
  # Greek final sigma (U+03C2) and dotted-I expansion are unambiguous.
  upper <- c(intToUtf8(c(0xC1, 0x52, 0x42, 0x4F, 0x4C)),   # ARBOL, A acute
             intToUtf8(c(0x39F, 0x394, 0x39F, 0x3A3)),     # ODOS, Greek
             intToUtf8(0x130))                             # I with dot above
  lower <- c(intToUtf8(c(0xE1, 0x72, 0x62, 0x6F, 0x6C)),
             intToUtf8(c(0x3BF, 0x3B4, 0x3BF, 0x3C2)),
             intToUtf8(c(0x69, 0x307)))
  expect_identical(.lower_invariant(upper), lower)
  expect_identical(.lower_invariant(NA_character_), NA_character_)

  old <- Sys.getlocale("LC_CTYPE")
  skip_if(!isTRUE(suppressWarnings(Sys.setlocale("LC_CTYPE", "C")) == "C"),
          "cannot switch to the C locale on this platform")
  on.exit(Sys.setlocale("LC_CTYPE", old), add = TRUE)
  expect_identical(.lower_invariant(upper), lower)
})

# Pins the same contract as test_load_items_trims_ascii_whitespace in the Python
# engine's test_querying.py. readr strips this padding before R ever sees it and
# pandas does not, so the Python engine trims to match; the assertion holds both
# to the trimmed value rather than to either reader's default.
test_that("load_items trims the ASCII whitespace readr trims", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("item,condition,target",
               "\"  i2  \",\"  related  \",\"  padded target  \"",
               "\"\ti1\t\",\"\tunrelated\t\",\"\ttab padded\t\""), path)
  items <- load_items(path, "target")
  expect_identical(items$target, c("padded target", "tab padded"))
  expect_identical(items$condition, c("related", "unrelated"))
  # The set id is byte order over the trimmed item, so the padding must not
  # decide it: trimmed, 'i1' sorts before 'i2'.
  expect_identical(items$set, c(2L, 1L))
})

# Pins the same contract as test_load_items_keeps_non_ascii_whitespace: a
# no-break space is not ASCII whitespace, neither reader touches it, so the
# engines agree without trimming it. This is the boundary of the trim above.
test_that("load_items keeps non-ASCII whitespace", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  nbsp <- intToUtf8(0xA0)
  write_utf8(paste0("item,condition,target\ni1,related,\"", nbsp, "cat", nbsp, "\"\n"), path)
  items <- load_items(path, "target")
  expect_identical(items$target, paste0(nbsp, "cat", nbsp))
})

test_that("missing required columns raise an informative error", {
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  bad <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(notword = "x"), bad, row.names = FALSE)
  expect_error(load_lexicon(bad, schema), "required column")
})

# Pins the same contract as test_load_items_refuses_missing_cells in the Python
# engine's test_querying.py. An NA used to flow into cryptic downstream failures
# here, while Python stringified it to the literal 'nan' and carried on. The two
# readers reach the refusal at different stages (readr reads a blank or 'NA' or
# all-whitespace cell as NA; pandas keeps a quoted whitespace cell as text until
# the trim), but the message is the same.
test_that("load_items refuses missing or blank cells", {
  cases <- list(
    "blank-item"           = list(body = c("item,condition,target", ",related,cat"),
                                  col = "item"),
    "blank-condition"      = list(body = c("item,condition,target", "i1,,cat"),
                                  col = "condition"),
    "blank-target"         = list(body = c("item,condition,target", "i1,related,"),
                                  col = "target"),
    "whitespace-condition" = list(body = c("item,condition,target", "i1,\"   \",cat"),
                                  col = "condition"),
    "literal-NA"           = list(body = c("item,condition,target", "i1,NA,cat"),
                                  col = "condition")
  )
  for (nm in names(cases)) {
    path <- tempfile(fileext = ".csv")
    on.exit(unlink(path), add = TRUE)
    writeLines(cases[[nm]]$body, path)
    expect_error(load_items(path, "target"),
                 sprintf(paste("lexsync: the items table has missing value(s) in column '%s';",
                               "every item, condition and presented field must be filled."),
                         cases[[nm]]$col),
                 fixed = TRUE, info = nm)
  }
})

# The boundary of the refusal above: both readers treat only '' and 'NA' as
# missing, so a literal 'nan' is a kept value and must stay one.
test_that("load_items keeps a literal 'nan' label", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("item,condition,target", "i1,nan,cat"), path)
  items <- load_items(path, "target")
  expect_identical(items$condition, "nan")
})

# Pins the same contract as test_load_items_refuses_a_duplicate_item_condition_pair
# in the Python engine's test_querying.py: it is the pair that must be unique, so
# the same item under another condition passes.
test_that("load_items refuses a duplicate item and condition pair", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("item,condition,target",
               "i1,related,cat", "i1,unrelated,dog", "i1,related,cow"), path)
  expect_error(load_items(path, "target"),
               paste("lexsync: the items table repeats item 'i1' for condition 'related';",
                     "each item and condition pair may appear once."),
               fixed = TRUE)
})

# Pins the same contract as test_load_items_keeps_numeric_ids_as_written in the
# Python engine's test_querying.py. `item` is now read as text: left to
# inference, both readers number-parsed '01' down to 1 (and pandas
# float-promoted the whole column to '1.0' ids whenever any cell was missing),
# so ids must survive exactly as written.
test_that("load_items keeps numeric ids as written", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("item,condition,target", "01,related,cat", "2,related,dog"), path)
  items <- load_items(path, "target")
  expect_identical(items$item, c("01", "2"))
  expect_identical(items$set, c(1L, 2L))
})

# Pins the same contract as test_build_pool_refuses_a_reversed_range in the
# Python engine's test_querying.py: c(7, 3) used to empty the pool without a word.
test_that("build_pool refuses a reversed range", {
  df <- data.frame(word = letters[1:5], frequency = 1:5, stringsAsFactors = FALSE)
  expect_error(build_pool(df, list(frequency = c(7, 3))),
               "lexsync: filter 'frequency' has a reversed range; give it as [low, high].",
               fixed = TRUE)
})

# Pins the same contract as test_build_pool_refuses_a_non_finite_bound in the
# Python engine's test_querying.py: YAML's .nan/.inf used to drop every row silently.
test_that("build_pool refuses a non-finite bound", {
  df <- data.frame(word = letters[1:5], frequency = 1:5, stringsAsFactors = FALSE)
  for (bad in list(c(NaN, 4), c(2, Inf))) {
    expect_error(build_pool(df, list(frequency = bad)),
                 "lexsync: filter 'frequency' has a non-finite bound; ranges need finite numbers.",
                 fixed = TRUE)
  }
})

# Equal bounds are a point, not a reversal: the zh design filters with [2, 2].
test_that("build_pool keeps a degenerate range", {
  df <- data.frame(word = letters[1:5], frequency = 1:5, stringsAsFactors = FALSE)
  expect_identical(build_pool(df, list(frequency = c(2, 2)))$word, "b")
})
