# A design file must not be able to execute code on the machine that runs it.
#
# A design is meant to be shared: posted with a pre-registration, attached to a paper,
# handed to a collaborator running the other engine. The recipient runs it and opens the
# generated PsychoPy script, OpenSesame experiment or jsPsych page, which is the only
# thing those files are for. Every value the design controls therefore has to stay data.
#
# Stimulus text always did: it travels in the loop-table CSV the experiment reads at run
# time. Design METADATA did not. The name, language label, font, parallel-port address
# and the column names on jitter and feedback events were substituted straight into code
# and markup positions, so a quote or an angle bracket there stopped being text.
#
# Mirrors test_injection.py. Both suites assert refusal rather than escaping; see
# clean_meta() in io_utils.R for why one rule beats three escapes across two engines.

schema_for_injection <- function() {
  yaml::read_yaml(system.file("extdata", "schema.yaml", package = "lexsync"))
}

stim_for_injection <- function() {
  data.frame(word = c("alpha", "beta"), condition = c("a", "b"), item = 1:2,
             set = 1:2, list = c(1L, 1L), trial = 1:2, stringsAsFactors = FALSE)
}

# Payloads that ended a string literal, a tag or a CSS rule in the generated file.
INJECTION_PAYLOADS <- c(
  'x"""; __import__("os").system("calc"); """',              # PsychoPy docstring
  "x'; __import__('os').system('calc'); #",                  # single-quoted literal
  "</title><script>alert(1)</script>",                       # jsPsych title
  'Courier New"; } body { background: url(//e.invalid) } .z {',  # CSS rule
  'x`+fetch("//e.invalid")+`',                               # JS template literal
  "x<img src=x onerror=alert(1)>",                           # HTML attribute
  "x&amp;y"                                                  # entity
)

test_that("a design cannot inject through its name, language or font", {
  schema <- schema_for_injection()
  out <- tempfile(); dir.create(out)
  for (payload in INJECTION_PAYLOADS) {
    for (field in c("name", "language", "font")) {
      design <- list(name = "inj", language = "english", timing = list())
      design[[field]] <- payload
      expect_error(export_psychopy(stim_for_injection(), design, schema, out),
                   "cannot be written safely")
      expect_error(export_jspsych(stim_for_injection(), design, schema, out),
                   "cannot be written safely")
      expect_error(export_opensesame(stim_for_injection(), design, schema, out),
                   "cannot be written safely")
    }
  }
})

test_that("the parallel-port address must be an address", {
  # It is written into `TRIGGER_ADDRESS = {{...}}` and `var.parallel_port_address = ...`
  # with no quotes at all, so anything that is not a number is a statement.
  schema <- schema_for_injection()
  out <- tempfile(); dir.create(out)
  design <- list(name = "inj", language = "english", timing = list())
  bad <- schema
  bad$triggers$parallel_address <- '0x378; __import__("os").system("calc")'
  expect_error(export_psychopy(stim_for_injection(), design, bad, out),
               "must be a port address")
  for (good in c("0x0378", "0x378", "888")) {
    ok <- schema
    ok$triggers$parallel_address <- good
    expect_silent(export_psychopy(stim_for_injection(), design, ok, out))
  }
})

test_that("event column names must be identifiers", {
  # `as:` on a jittered duration and `answer:` on a feedback event both become a
  # variable reference in the emitted OpenSesame Python.
  schema <- schema_for_injection()
  out <- tempfile(); dir.create(out)
  design <- list(
    name = "inj", language = "english", timing = list(),
    events = list(
      list(type = "text", content = "{word}", duration_ms = 100L),
      list(type = "response", keys = c("f", "j"), timeout_ms = 500L),
      list(type = "feedback",
           answer = "ans'); __import__('os').system('calc'); ('",
           duration_ms = 100L)
    )
  )
  expect_error(export_opensesame(stim_for_injection(), design, schema, out),
               "must be a plain column name")
})

test_that("a stated language_tag is shape-checked", {
  # It used to be returned verbatim into the generated page's lang attribute.
  expect_equal(lexsync:::.language_tag(
    list(language_tag = 'en"><script>alert(1)</script>')), "und")
  expect_equal(lexsync:::.language_tag(list(language_tag = "en-GB")), "en-GB")
  expect_equal(lexsync:::.language_tag(list(language = "english")), "en")
})

test_that(".pyq escapes a newline", {
  # An .osexp is line-oriented: a raw newline closed the inline-script block and let
  # the rest of the value start a new top-level item in the emitted experiment.
  expect_false(grepl("\n", lexsync:::.pyq("a\nb"), fixed = TRUE))
  expect_equal(lexsync:::.pyq("a\nb"), "u'a\\nb'")
  expect_equal(lexsync:::.pyq("a\r\tb"), "u'a\\r\\tb'")
})

test_that("legitimate metadata still passes", {
  # The guard must not cost a real design anything: these are the values the shipped
  # designs actually use, including the Chinese font and an accented label.
  schema <- schema_for_injection()
  out <- tempfile(); dir.create(out)
  cases <- list(
    c("en_lexdec", "english", "Courier New"),
    c("zh_freqcontrast", "chinese", "SimHei"),
    c("es_gender_repro", "español", "Courier New"),
    c("a-design_1.0 (v2)", "British English", "DejaVu Sans Mono")
  )
  for (case in cases) {
    design <- list(name = case[1], language = case[2], font = case[3], timing = list())
    expect_silent(export_psychopy(stim_for_injection(), design, schema, out))
    expect_silent(export_jspsych(stim_for_injection(), design, schema, out))
  }
})

# ---- Bypasses found by adversarially attacking the first version of the guards ----

test_that("a response key cannot inject .osexp items", {
  # The guards' biggest miss, found independently by three reviewers. OpenSesame takes
  # the keys as `set allowed_responses "a;b"` on ONE line of a line-oriented format, so
  # a key holding a double quote closed that string and a newline ended the line: the
  # rest of the value became new top-level items, including an inline_script whose
  # ___run__ body OpenSesame executes.
  schema <- schema_for_injection()
  out <- tempfile(); dir.create(out)
  payload <- paste0('j"\n\ndefine inline_script lexsync_pwned\n\tset _prepare ""\n',
                    "\t___run__\n\t__import__('os').system('calc')\n\t__end__\n",
                    'define sequence dummy\n\tset x "')
  design <- list(
    name = "inj", language = "english", timing = list(),
    events = list(
      list(type = "text", content = "{word}", duration_ms = 100L),
      list(type = "response", keys = c("f", payload), timeout_ms = 500L)
    )
  )
  for (fn in list(export_psychopy, export_jspsych, export_opensesame)) {
    expect_error(fn(stim_for_injection(), design, schema, out), "must be a key name")
  }
})

test_that("key shapes are constrained", {
  for (bad in c('f"', "f\n", "f;j", "f'", "f<", "", strrep("x", 21))) {
    expect_error(lexsync:::clean_key(bad), "must be a key name")
  }
  for (good in c("f", "j", "space", "left", "arrowleft", "num_1", "a b")) {
    expect_equal(lexsync:::clean_key(good), good)
  }
})

test_that("a trailing newline does not slip past the shape guards", {
  # `$` also matches just BEFORE a final newline, in R's PCRE and Python's re alike, so
  # "888\n" satisfied a `$`-anchored check and carried a newline into a line-oriented
  # .osexp. The anchors are \z / \Z now.
  for (bad in c("888\n", "0x378\n")) {
    expect_error(lexsync:::clean_port(bad), "must be a port address")
  }
  for (bad in c("iti_ms\n", "answer\n")) {
    expect_error(lexsync:::clean_column(bad), "must be a plain column name")
  }
  expect_equal(lexsync:::clean_port("888"), "888")
  expect_equal(lexsync:::clean_column("iti_ms"), "iti_ms")
})

test_that("a scalar keys or blocks is kept whole", {
  # YAML allows `keys: space` and `blocks: practice` as scalars. Python's list() made
  # five one-character keys where R kept the string whole, so one design produced two
  # different allowed-response lists across the engines.
  design <- list(
    name = "inj", language = "english", timing = list(),
    events = list(
      list(type = "text", content = "{word}", duration_ms = 100L),
      list(type = "response", keys = "space", timeout_ms = 500L),
      list(type = "feedback", answer = "answer", blocks = "practice",
           duration_ms = 100L)
    )
  )
  rendered <- render_events(resolve_events(design), list(), 60)
  expect_equal(rendered[[2]]$keys, "space")
  expect_equal(unlist(rendered[[3]]$blocks), "practice")
})

test_that("the html escape covers the JavaScript line separators", {
  # U+2028 and U+2029 end a line in JavaScript but are not ASCII controls, so
  # clean_field passes them. Python escaped them and this engine did not, so the same
  # design produced different bytes and the R <script> was a SyntaxError before ES2019.
  sep <- paste0("a", "\u2028", "b", "\u2029", "c")
  out <- lexsync:::.json_html(sep)
  expect_false(grepl("\u2028", out, fixed = TRUE))
  expect_false(grepl("\u2029", out, fixed = TRUE))
  expect_true(grepl("\\u2028", out, fixed = TRUE))
  expect_true(grepl("\\u2029", out, fixed = TRUE))
})
