test_that("subsyllabic segmentation matches worked examples", {
  expect_equal(segment_subsyllabic("bridge"),
               list(list(role = "onset", text = "br"), list(role = "nucleus", text = "i"),
                    list(role = "coda", text = "d"), list(role = "onset", text = "g"),
                    list(role = "nucleus", text = "e")))
  expect_equal(segment_subsyllabic("strength"),
               list(list(role = "onset", text = "str"), list(role = "nucleus", text = "e"),
                    list(role = "coda", text = "ngth")))
  expect_length(segment_subsyllabic("crwth"), 0)          # no vowel run
})

test_that("subsyllabic generation yields legal non-words of preserved length", {
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  lex <- load_lexicon(system.file("extdata", "en_example.csv", package = "lexsync"), schema, "english")
  ref <- lex$word
  base <- head(build_pool(lex, list(length = c(4, 7)))$word, 30)
  gen <- generate_pseudowords_subsyllabic(base, ref)
  bg <- bigram_counts(ref)
  for (i in seq_len(nrow(gen))) {
    expect_equal(nchar(gen$pseudoword[i]), nchar(gen$base_word[i]))   # length preserved
    expect_false(gen$pseudoword[i] %in% ref)                          # a novel non-word
    expect_true(.bg_legal(gen$pseudoword[i], bg))                     # every bigram attested
  }
  expect_equal(length(unique(gen$pseudoword)), nrow(gen))
})

test_that("build_lexdec_stimuli rejects an unknown generation method", {
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  lex <- load_lexicon(system.file("extdata", "en_example.csv", package = "lexsync"), schema, "english")
  pool <- build_pool(lex, list(length = c(4, 6)))
  expect_error(build_lexdec_stimuli(pool, 10L, reference_words = lex$word, method = "invalid"),
               "unknown pseudoword generation method")
})
