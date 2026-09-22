LEX <- c("cat", "cap", "car", "can", "cab", "bat", "bad", "bag", "ban",
         "rat", "ran", "ram", "rag", "tan", "tap", "tar", "mat", "map", "man")

test_that("every paradigm resolves to typed events with its required fields", {
  for (name in names(PARADIGMS)) {
    events <- resolve_events(list(paradigm = name))
    expect_gt(length(events), 0)
    expect_true(all(vapply(events, function(e) !is.null(e$type), logical(1))))
    expect_true(all(PARADIGMS[[name]]$stimulus_fields %in% required_fields(list(paradigm = name))))
  }
})

test_that("content_field and referenced_fields read field references", {
  expect_identical(content_field("{target}"), "target")
  expect_null(content_field("+"))
  expect_identical(referenced_fields(resolve_events(list(paradigm = "priming"))),
                   c("prime", "target"))
})

test_that("explicit events override the paradigm default", {
  design <- list(events = list(list(type = "text", content = "{w}", duration_frames = 10L)))
  expect_identical(vapply(resolve_events(design), function(e) e$type, character(1)), "text")
})

test_that("pseudowords are deterministic", {
  a <- generate_pseudowords(c("cat", "bat", "rat"), LEX)
  b <- generate_pseudowords(c("cat", "bat", "rat"), LEX)
  expect_identical(a$pseudoword, b$pseudoword)
})

test_that("pseudowords are legal non-words of matched length", {
  gen <- generate_pseudowords(c("cat", "bat", "rat", "man"), LEX)
  bg <- names(bigram_counts(LEX))
  for (i in seq_len(nrow(gen))) {
    pw <- gen$pseudoword[i]
    expect_false(pw %in% LEX)
    expect_equal(nchar(pw), nchar(gen$base_word[i]))
    bgs <- substring(pw, 1:(nchar(pw) - 1L), 2:nchar(pw))
    expect_true(all(bgs %in% bg))
  }
  expect_equal(length(unique(gen$pseudoword)), nrow(gen))
})

test_that("build_lexdec_stimuli pairs words and pseudowords by length", {
  schema <- yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
  lex <- load_lexicon(system.file("extdata", "en_example.csv", package = "lexsync"), schema, "english")
  pool <- build_pool(lex, list(length = c(4, 6), frequency = c(3.0, 6.5)))
  stim <- build_lexdec_stimuli(pool, 15L, reference_words = lex$word)
  expect_setequal(unique(stim$condition), c("word", "pseudoword"))
  expect_equal(sum(stim$condition == "word"), sum(stim$condition == "pseudoword"))
  for (s in unique(stim$set)) expect_equal(length(unique(stim$length[stim$set == s])), 1L)
})

test_that("load_items validates columns and clean_field rejects control characters", {
  p <- tempfile(fileext = ".csv")
  writeLines(c("item,prime", "1,nurse"), p)
  expect_error(load_items(p, c("prime", "target")))
  expect_error(clean_field("ab\ncd", "target"))
  expect_error(clean_field("ab\tcd", "target"))
  expect_identical(clean_field("a clean, quoted 'item'", "x"), "a clean, quoted 'item'")
})

test_that("the Latin square shows each item once per list, balanced", {
  items <- data.frame(
    item = c(1, 1, 2, 2, 3, 3, 4, 4),
    condition = rep(c("related", "unrelated"), 4),
    prime = letters[1:8],
    target = c("x", "x", "y", "y", "z", "z", "w", "w"),
    stringsAsFactors = FALSE)
  items$set <- items$item
  out <- counterbalance(items, list(paradigm = "priming", counterbalance = list(lists = 2)),
                        list(seed = 1))
  for (l in unique(out$list)) {
    lst <- out[out$list == l, ]
    expect_equal(length(unique(lst$set)), nrow(lst))
    expect_equal(length(unique(lst$target)), nrow(lst))
    expect_setequal(unique(lst$condition), c("related", "unrelated"))
    expect_equal(sum(lst$condition == "related"), sum(lst$condition == "unrelated"))
  }
})
