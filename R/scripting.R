# scripting.R -- export finalised, hardware-timed experiment scripts. Both targets
# render the same declarative trial-event sequence (see paradigms.R), so a new
# paradigm requires only a configuration change rather than new backend code. The PsychoPy script reads
# stimulus text as data from the conditions file and interprets an embedded EVENTS
# list; the OpenSesame .osexp is generated block by block. Generation imports
# neither psychopy nor pyserial: it only writes text. Mirrors scripting.py.

#' Locate a bundled template file
#' @keywords internal
find_template <- function(relpath) {
  opt <- getOption("lexsync.templates", "")
  cand <- c(
    if (nzchar(opt)) file.path(opt, relpath),
    system.file("templates", relpath, package = "lexsync"),
    file.path("templates", relpath),
    file.path("..", "templates", relpath)
  )
  cand <- cand[nzchar(cand) & file.exists(cand)]
  if (!length(cand)) stop(sprintf("lexsync: template '%s' not found.", relpath), call. = FALSE)
  cand[1]
}

#' Assign EEG trigger codes to stimuli
#'
#' Adds `condition_trigger` (101, 102, ... per condition) and `item_trigger`
#' (40-239 per item/`set`). Events reference these by the tokens "condition" and
#' "item", or carry their own integer codes. The item range holds 200 codes (an
#' 8-bit-port constraint), so past 200 sets the codes wrap and repeat, and a
#' runtime notice says so.
#'
#' @param stimuli A stimuli data frame.
#' @return `stimuli` with trigger columns added.
#' @export
assign_triggers <- function(stimuli) {
  conds <- unique(stimuli$condition)
  stimuli$condition_trigger <- 100L + match(stimuli$condition, conds)
  sets <- unique(stimuli$set)
  sets <- sets[order(as.character(sets), method = "radix")]
  # A wrapped code no longer identifies its item one to one, which the analyst
  # must hear about at generation time, not at decode time.
  if (length(sets) > 200L) {
    message(sprintf(
      "lexsync: %d item sets exceed the 200-code trigger range; item codes wrap and repeat.",
      length(sets)))
  }
  code <- 40L + ((seq_along(sets) - 1L) %% 200L)
  stimuli$item_trigger <- code[match(stimuli$set, sets)]
  stimuli
}

#' @keywords internal
trigger_spec <- function(value) {
  if (is.null(value)) return(NULL)
  if (identical(value, "condition")) return("@condition_trigger")
  if (identical(value, "item")) return("@item_trigger")
  as.integer(value)
}

# Frames to milliseconds at a given refresh rate. Multiplication comes first so
# both engines evaluate the same two IEEE-754 operations on the same doubles:
# `frames * 1000 / hz`, never `frames * (1000 / hz)`, which is a different
# computation and agrees with the first only by accident of the divisor.
# Must stay identical to .frames_to_ms in python_workflow/src/lexsync/scripting.py.
#' @keywords internal
frames_to_ms <- function(frames, hz) {
  as.integer(round(as.numeric(frames) * 1000 / as.numeric(hz)))
}

# The refresh rate a design's frame counts were authored for. Only ever used to
# convert `*_frames` to milliseconds; it is not the rate the experiment runs at.
#' @keywords internal
.refresh_hz <- function(schema) {
  hz <- (schema$presentation %||% list())$assumed_refresh_hz %||% 60
  hz <- as.numeric(hz)
  if (!is.finite(hz) || hz <= 0) {
    stop("lexsync: presentation.assumed_refresh_hz must be a positive number.", call. = FALSE)
  }
  hz
}

# How long the onset code is held before it is reset to 0, in milliseconds.
#
# `reset_after_frames: N` is still accepted, but converting it needs care: the old
# implementation queued the reset on flip N and callOnFlip runs on the FOLLOWING
# flip, so the code was actually held for N + 1 flip intervals. Converting N
# directly would silently shorten every trigger by one frame, so the +1 is part of
# the compatibility contract, not an off-by-one.
# Must stay identical to _trigger_hold_ms in python_workflow/src/lexsync/scripting.py.
#' @keywords internal
.trigger_hold_ms <- function(schema) {
  triggers <- schema$triggers %||% list()
  if (!is.null(triggers$trigger_hold_ms)) {
    ms <- as.numeric(triggers$trigger_hold_ms)
  } else if (!is.null(triggers$reset_after_frames)) {
    ms <- as.numeric(frames_to_ms(as.integer(triggers$reset_after_frames) + 1L,
                                  .refresh_hz(schema)))
  } else {
    ms <- 50
  }
  if (!is.finite(ms) || ms <= 0) {
    stop("lexsync: triggers.trigger_hold_ms must be a positive number.", call. = FALSE)
  }
  ms
}

# Resolve one event's duration to whole milliseconds. Milliseconds are canonical;
# a frame count is accepted for backward compatibility and converted at the
# design's assumed refresh rate. An explicit `duration_ms` wins over
# `duration_frames` if a design carries both.
#' @keywords internal
.event_ms <- function(ev, override_ms, override_frames, hz, default_frames = 1L) {
  if (!is.null(override_ms)) return(as.integer(override_ms))
  if (!is.null(override_frames)) return(frames_to_ms(override_frames, hz))
  if (!is.null(ev$duration_ms)) return(as.integer(ev$duration_ms))
  frames_to_ms(ev$duration_frames %||% default_frames, hz)
}


# A design's per-event `duration:` block, if it declares one.
#   duration: {from_column: soa_ms}                  read per trial from the items
#   duration: {jitter: [400, 800], as: isi_ms}       drawn per trial from the hash
#' @keywords internal
.duration_spec <- function(ev, index) {
  d <- ev$duration
  if (is.null(d) || !is.list(d)) return(NULL)
  if (!is.null(d$from_column)) {
    return(list(column = as.character(d$from_column), jitter = NULL))
  }
  if (!is.null(d$jitter)) {
    rng <- as.integer(unlist(d$jitter))
    if (length(rng) != 2L) {
      stop("lexsync: duration jitter must be a two-element range [lo, hi].", call. = FALSE)
    }
    return(list(column = as.character(d$as %||% sprintf("event%d_ms", index)),
                jitter = rng))
  }
  stop("lexsync: a duration block needs either 'from_column' or 'jitter'.", call. = FALSE)
}

# An event's response keys, validated. OpenSesame writes them into
# `set allowed_responses "a;b"`, one line of a line-oriented format, so a key holding a
# quote closed the string and a newline ended the line -- and the rest became new
# top-level items in the experiment, including an inline_script whose body runs.
#' @keywords internal
.keys_of <- function(ev, default) {
  keys <- ev$keys %||% default
  vapply(as.character(unlist(keys)), clean_key, character(1), USE.NAMES = FALSE)
}

#' Realise per-trial event durations onto the stimuli table
#'
#' An event may declare a duration that varies from trial to trial, either read
#' from an item column or drawn from a range. A drawn value is a pure function of
#' the keyed hash, so both engines realise the same milliseconds, and it is
#' written into the stimuli table as well as the generated script, because timing
#' that varies is a variable the analysis needs, not presentation detail.
#'
#' @param stimuli A counterbalanced stimuli data frame.
#' @param design A parsed design configuration.
#' @param schema The parsed global schema (provides the seed).
#' @return `stimuli` with one integer column per jittered event.
#' @export
resolve_trial_timing <- function(stimuli, design, schema) {
  events <- resolve_events(design)
  seed <- schema$seed %||% 1L
  for (i in seq_along(events)) {
    spec <- .duration_spec(events[[i]], i)
    if (is.null(spec) || is.null(spec$jitter)) {
      # from_column reads a column the items already carry; nothing to realise.
      if (!is.null(spec) && !(spec$column %in% names(stimuli))) {
        stop(sprintf("lexsync: event %d reads its duration from column '%s', which the items do not have.",
                     i, spec$column), call. = FALSE)
      }
      next
    }
    # The key names the column as well as the trial, so two jittered events in
    # one design draw independently, where a key naming only the trial would give
    # them one shared value.
    key <- paste(.key_part(seed), "jitter", spec$column,
                 .key_part(stimuli$list %||% 1L), .key_part(stimuli$set),
                 .key_part(stimuli$condition), sep = "|")
    stimuli[[spec$column]] <- hash_int_range(key, spec$jitter[1], spec$jitter[2])
  }
  stimuli
}

#' Translate paradigm events into backend-neutral rendering dictionaries
#'
#' Durations are emitted as whole milliseconds (`ms`), the unit every backend
#' consumes: OpenSesame and jsPsych schedule it directly, and the PsychoPy script
#' converts it back into whole flips against the refresh rate it measures at
#' start-up.
#' @keywords internal
render_events <- function(events, timing, hz = 60) {
  timing <- timing %||% list()
  # A design's timing block was previously read key by key, so a typo such as
  # `fixation_frame` was silently ignored and the event kept a default duration.
  # Reject an unknown key instead: a mistimed experiment is not recoverable after
  # the data are collected. Must list the same names as scripting.py.
  known <- c("fixation_ms", "word_ms", "isi_ms",
             "fixation_frames", "word_frames", "isi_frames")
  unknown <- setdiff(names(timing), known)
  if (length(unknown)) {
    stop(sprintf("lexsync: unknown timing key(s) %s. Known keys: %s.",
                 paste(sprintf("'%s'", sort(unknown)), collapse = ", "),
                 paste(known, collapse = ", ")), call. = FALSE)
  }
  fix_ms <- timing$fixation_ms;  fix <- timing$fixation_frames
  word_ms <- timing$word_ms;    word <- timing$word_frames
  isi_ms <- timing$isi_ms;      isi <- timing$isi_frames
  out <- list()
  # A feedback event scores the key the participant pressed, so something must have
  # collected one first. With no preceding response or question the OpenSesame runner
  # raises on the unset variable, PsychoPy compares against None and jsPsych finds no
  # scored row -- three different confusing failures at run time, for one design error
  # that is obvious here.
  seen_response <- FALSE
  for (i in seq_along(events)) {
    ty <- events[[i]]$type
    if (ty %in% c("response", "question")) seen_response <- TRUE
    if (identical(ty, "feedback") && !seen_response) {
      stop(sprintf(paste("lexsync: event %d is a feedback event, but no response or",
                         "question event precedes it, so there is no response to score."),
                   i), call. = FALSE)
    }
  }
  for (i in seq_along(events)) {
    ev <- events[[i]]
    t <- ev$type
    r <- list(type = if (identical(t, "region_by_region")) "region" else t)
    if (t %in% c("fixation", "text", "mask")) {
      f <- content_field(ev$content)
      if (!is.null(f)) r$field <- f else r$text <- as.character(ev$content %||% "")
      # The overrides are type-keyed, as before: `fixation_*` reaches every
      # fixation and `word_*` only a text event whose trigger is "condition",
      # which is why a priming prime (trigger 20) keeps its own duration.
      is_word <- t == "text" && identical(ev$trigger, "condition")
      spec <- .duration_spec(ev, i)
      if (!is.null(spec)) {
        r$ms_column <- spec$column
      } else {
        r$ms <- .event_ms(
          ev,
          if (t == "fixation") fix_ms else if (is_word) word_ms else NULL,
          if (t == "fixation") fix else if (is_word) word else NULL,
          hz
        )
      }
      spec <- trigger_spec(ev$trigger)
      if (!is.null(spec)) r$trigger <- spec
    } else if (t == "blank") {
      spec <- .duration_spec(ev, i)
      if (!is.null(spec)) r$ms_column <- spec$column else r$ms <- .event_ms(ev, isi_ms, isi, hz)
    } else if (t == "region_by_region") {
      r$field <- content_field(ev$content)
      r$sep <- ev$sep %||% "|"
      r$key <- ev$advance %||% "space"
      spec <- trigger_spec(ev$critical_region_trigger)
      if (!is.null(spec)) r$crit_trigger <- spec
    } else if (t == "response") {
      r$keys <- .keys_of(ev, c("left", "right"))
      # .round_dp, not round(): the timeout is written into experiment files both
      # engines emit as the same bytes, and the two languages' own rounders
      # disagree on 3-dp halfway cases (see .round_dp in io_utils.R).
      r$timeout <- .round_dp((ev$timeout_ms %||% 2000) / 1000, 3)
    } else if (t == "question") {
      r$field <- content_field(ev$content)
      r$keys <- .keys_of(ev, c("f", "j"))
      r$timeout <- .round_dp((ev$timeout_ms %||% 5000) / 1000, 3)
    } else if (t == "feedback") {
      # Feedback compares the key the participant pressed against the key the item says
      # is correct, so `answer` names a loop-table column holding a KEY, not a label:
      # the runner then needs no mapping table and no notion of what the keys mean.
      r$answer <- as.character(ev$answer %||% "answer")
      r$correct <- as.character(ev$correct %||% "Correct")
      r$incorrect <- as.character(ev$incorrect %||% "Incorrect")
      r$no_response <- as.character(ev$no_response %||% "Too slow")
      r$ms <- .event_ms(ev, NULL, NULL, hz, default_frames = 36L)
    } else {
      stop(sprintf("lexsync: unknown event type '%s'.", t), call. = FALSE)
    }
    # An event may be restricted to named blocks. This is what lets feedback run during
    # practice and nowhere else: the event list is global to the design, so the
    # restriction has to travel with the event and be applied per trial at run time.
    # Generating a second event list would be the alternative, and a worse one.
    if (!is.null(ev$blocks)) {
      r$blocks <- as.list(as.character(unlist(ev$blocks, use.names = FALSE)))
    }
    out[[length(out) + 1L]] <- r
  }
  out
}

#' Per-trial table carrying exactly the fields the events reference
#' @keywords internal
loop_table <- function(stimuli, events = NULL) {
  fields <- if (!is.null(events)) referenced_fields(events) else
    (if ("word" %in% names(stimuli)) "word" else character(0))
  # A per-trial duration is read from the loop table at run time, so its column
  # has to travel with the trials, and cannot stay behind in the stimuli file.
  ms_cols <- character(0)
  if (!is.null(events)) {
    for (i in seq_along(events)) {
      s <- .duration_spec(events[[i]], i)
      if (!is.null(s) && !(s$column %in% ms_cols)) ms_cols <- c(ms_cols, s$column)
    }
  }
  # `block` travels with the trials because the runners need it: it is what an
  # event's `blocks:` restriction is matched against, and what a runner watches to
  # know a block boundary has been reached. Absent unless the design declares a
  # practice or filler block, so no existing loop table gains a column.
  order_cols <- c("trial", "list", "block", "set", "condition", fields, ms_cols,
                  "critical_region", "answer", "condition_trigger", "item_trigger")
  cols <- character(0)
  for (c in order_cols) if (c %in% names(stimuli) && !(c %in% cols)) cols <- c(cols, c)
  tab <- stimuli[, cols, drop = FALSE]
  if ("trial" %in% names(tab)) tab <- tab[order(tab$trial), , drop = FALSE]
  rownames(tab) <- NULL
  tab
}

#' Export a runnable PsychoPy script that interprets the event sequence
#'
#' @param stimuli Stimuli with trigger columns (see [assign_triggers()]).
#' @param design A parsed design configuration.
#' @param schema The parsed global schema (trigger and presentation settings).
#' @param outdir Output directory.
#' @param base Optional file-name stem.
#' @return The path to the generated `.py`, invisibly.
#' @importFrom jsonlite toJSON
#' @export
export_psychopy <- function(stimuli, design, schema, outdir, base = NULL) {
  base <- base %||% slugify(design$name, design$language)
  events <- resolve_events(design)
  rendered <- render_events(events, design$timing %||% list(), .refresh_hz(schema))
  csv_name <- paste0(base, "_psychopy.csv")
  write_csv_utf8(loop_table(stimuli, events), file.path(outdir, csv_name))
  tmpl <- paste(readLines(find_template("psychopy/trial_runner_template.py"), warn = FALSE),
                collapse = "\n")
  triggers <- schema$triggers %||% list()
  presentation <- schema$presentation %||% list()
  subs <- list(
    DESIGN = clean_meta(design$name, "the design's `name`"),
    LANGUAGE = clean_meta(design$language, "the design's `language`"),
    CONDITIONS_FILE = csv_name,
    TRIGGER_ADDRESS = clean_port(triggers$parallel_address %||% "0x0378"),
    TRIGGER_HOLD_MS = sprintf("%.17g", .trigger_hold_ms(schema)),
    # Through %.17g like its neighbours, never default stringification: R's
    # as.character() and Python's str() part company on a non-integer quotient
    # (as.character(16.65 / 1000) gives "0.01665", str gives
    # "0.016649999999999998"), and this value lands in the generated script.
    INTER_TRIGGER_S = sprintf("%.17g", (triggers$inter_trigger_ms %||% 10) / 1000),
    WORD_FONT = clean_meta(design$font %||% presentation$font %||% "Courier New", "the font"),
    FULLSCREEN = "False",
    # The fallback used only when the script cannot measure the display.
    ASSUMED_REFRESH_HZ = sprintf("%.17g", .refresh_hz(schema)),
    EVENTS_JSON = as.character(jsonlite::toJSON(rendered, auto_unbox = TRUE))
  )
  for (k in names(subs)) {
    tmpl <- gsub(sprintf("{{%s}}", k), as.character(subs[[k]]), tmpl, fixed = TRUE)
  }
  out <- file.path(outdir, paste0(base, "_psychopy.py"))
  write_lines_lf(tmpl, out)
  invisible(out)
}

# ---- OpenSesame code generation ------------------------------------------

.pyq <- function(s) {
  # A newline had to be escaped as well as the backslash and the quote. An .osexp is a
  # line-oriented format whose inline scripts are delimited by their own lines, so a raw
  # newline in a value did not merely break the literal: it closed the script block and
  # let the rest of the value start a new top-level item in the emitted experiment.
  s <- gsub("\\", "\\\\", s, fixed = TRUE)
  s <- gsub("'", "\\'", s, fixed = TRUE)
  s <- gsub("\n", "\\n", s, fixed = TRUE)
  s <- gsub("\r", "\\r", s, fixed = TRUE)
  s <- gsub("\t", "\\t", s, fixed = TRUE)
  paste0("u'", s, "'")
}

.content_expr <- function(ev) if (!is.null(ev$field)) paste0("var.", ev$field) else .pyq(ev$text %||% "")

.trigger_expr <- function(spec) {
  if (is.null(spec)) return(NULL)
  if (is.character(spec) && startsWith(spec, "@")) return(paste0("var.", substring(spec, 2)))
  as.character(as.integer(spec))
}

.inline_block <- function(name, desc, body) {
  c(sprintf("define inline_script %s", name),
    sprintf('\tset description "%s"', desc),
    '\tset _prepare ""', "\t___run__", paste0("\t", body), "\t__end__", "")
}

# The Python that OpenSesame's inline_script actually runs.
#
# Two spellings here are forced by OpenSesame's runtime and were both wrong at first:
#
# `str`, never `unicode`. OpenSesame 3.3 and later run inline scripts in a Python 3
# workspace and inject only a fixed set of globals (exp, var, pool, items, clock, log
# and a small API); `unicode` is a Python 2 builtin and is not among them, so every
# emitted guard and feedback script died with NameError on the first trial.
#
# `var.get(name, u'')`, never `var.get(name, None)`. OpenSesame's var_store.get ends
# `elif default is not None: val = default / else: raise VariableDoesNotExist(var)`, so
# passing None explicitly is indistinguishable from passing no default and RAISES rather
# than yielding None. A non-None sentinel is the only default that works.

# Restrict an inline script's body to the named blocks.
#
# The guard goes INSIDE the script rather than on the sequence's `run` line. OpenSesame
# does support a run condition, but its syntax and quoting could not be verified here
# without the application, and an emitter that generates an experiment nobody can open is
# worse than one that generates a slightly longer script.
#' @keywords internal
.osexp_block_guard <- function(body, blocks) {
  if (is.null(blocks) || !length(blocks)) return(body)
  wanted <- paste(vapply(as.character(unlist(blocks)), .pyq, character(1)), collapse = ", ")
  c(sprintf("if str(var.get(u'block', u'main')) in [%s]:", wanted),
    paste0("    ", body))
}

.osexp_event_block <- function(name, ev) {
  t <- ev$type
  blocks <- ev[["blocks"]]
  if (!is.null(blocks) && length(blocks) && t == "response") {
    # A keyboard_response is not an inline script, so the guard above cannot reach it.
    # Saying so beats emitting an experiment that ignores the restriction.
    stop(paste("lexsync: a `response` event cannot be restricted to blocks on the",
               "OpenSesame target. Restrict a feedback or stimulus event instead."),
         call. = FALSE)
  }
  # A fixed duration is a literal; one that varies per trial reads the loop-table
  # column, which OpenSesame exposes on `var`. Must match _osexp_event_block in
  # scripting.py, since the two engines write the same .osexp.
  # [[ ]], not $: `$` partial-matches on a list, so `ev$ms` would return the value
  # of `ms_column` for a per-trial duration and emit the column name as a literal.
  sleep_arg <- if (!is.null(ev[["ms_column"]]))
    sprintf("int(var.%s)", clean_column(ev[["ms_column"]], "a jittered duration's `as`")) else
    sprintf("%d", as.integer(ev[["ms"]] %||% 0L))
  if (t %in% c("fixation", "text", "mask", "blank")) {
    body <- "c = Canvas()"
    if (t != "blank") body <- c(body, sprintf("c.text(%s)", .content_expr(ev)))
    body <- c(body, "var.onset_time = c.show()")
    trig <- .trigger_expr(ev$trigger)
    if (!is.null(trig)) body <- c(body, sprintf("send_trigger(%s)", trig))
    body <- c(body, sprintf("clock.sleep(%s)", sleep_arg))
    return(list(block = .inline_block(name, "Show stimulus and send onset-aligned trigger",
                                      .osexp_block_guard(body, blocks)),
                run = name))
  }
  if (t == "region") {
    trig <- .trigger_expr(ev$crit_trigger)
    body <- c(
      sprintf("_regions = [r for r in var.%s.split(%s) if r != u'']", ev$field, .pyq(ev$sep %||% "|")),
      "_crit = int(var.get(u'critical_region', 0) or 0)",
      sprintf("_kb = Keyboard(keylist=[%s], timeout=None)", .pyq(ev$key %||% "space")),
      "for _i, _region in enumerate(_regions, start=1):",
      "    c = Canvas(); c.text(_region); c.show()")
    if (!is.null(trig)) body <- c(body, sprintf("    if _i == _crit: send_trigger(%s)", trig))
    body <- c(body, "    _kb.get_key()")
    return(list(block = .inline_block(name, "Self-paced reading region by region",
                                      .osexp_block_guard(body, blocks)), run = name))
  }
  if (t == "question") {
    keys_list <- paste(vapply(ev$keys %||% c("f", "j"), .pyq, character(1)), collapse = ", ")
    body <- c(
      sprintf("c = Canvas(); c.text(var.%s); c.show()", ev$field),
      sprintf("_kb = Keyboard(keylist=[%s], timeout=%d)", keys_list,
              as.integer((ev$timeout %||% 5) * 1000)),
      "var.response, var.response_time = _kb.get_key()")
    return(list(block = .inline_block(name, "Comprehension question",
                                      .osexp_block_guard(body, blocks)), run = name))
  }
  if (t == "feedback") {
    # `var.response` is set by the keyboard_response above; `var.answer` (or whichever
    # column the event names) holds the KEY that is correct, so scoring is a string
    # comparison. A timeout leaves the response None, which is reported separately
    # because on a timed task it means something different from a wrong key.
    body <- c(
      sprintf("_want = str(var.get(u'%s', u'')).strip()",
              clean_column(ev[["answer"]] %||% "answer", "a feedback event's `answer`")),
      "_got = var.get(u'response', u'')",
      sprintf("if _got is None or _got == u'': _msg = %s", .pyq(ev[["no_response"]] %||% "Too slow")),
      sprintf("elif str(_got).strip() == _want: _msg = %s",
              .pyq(ev[["correct"]] %||% "Correct")),
      sprintf("else: _msg = %s", .pyq(ev[["incorrect"]] %||% "Incorrect")),
      "c = Canvas(); c.text(_msg); c.show()",
      sprintf("clock.sleep(%s)", sleep_arg))
    return(list(block = .inline_block(name, "Practice feedback",
                                      .osexp_block_guard(body, blocks)), run = name))
  }
  if (t == "response") {
    keys <- paste(ev$keys %||% c("left", "right"), collapse = ";")
    # A keyboard_response draws nothing, so the preceding canvas would stay up for
    # the whole response window. Blank it first, so the stimulus offsets at its own
    # duration as it does in the PsychoPy and jsPsych targets.
    block <- c(
      .inline_block(sprintf("%s_blank", name), "Clear the screen for the response window",
                    c("c = Canvas()", "c.show()")),
      sprintf("define keyboard_response %s", name),
      paste0("\tset timeout ", as.integer((ev$timeout %||% 2) * 1000)),
      "\tset flush yes", "\tset duration keypress",
      '\tset description "Collect a response"',
      sprintf('\tset allowed_responses "%s"', keys), "")
    return(list(block = block, run = c(sprintf("%s_blank", name), name)))
  }
  stop(sprintf("lexsync: unknown event type '%s'.", t), call. = FALSE)
}

#' Build a complete plain-text OpenSesame experiment from rendered events
#' @keywords internal
build_osexp <- function(design, conditions_file, schema, rendered, font = "mono") {
  addr <- clean_port(schema$triggers$parallel_address %||% "0x378")
  design_name <- clean_meta(design$name, "the design's `name`")
  language <- clean_meta(design$language, "the design's `language`")
  # `set font_family <font>` takes the value unquoted on its own line, so it is guarded
  # here rather than at the caller: an .osexp is line-oriented and a newline would add
  # items to the experiment.
  font <- clean_meta(font, "the font")
  setup <- c(
    "var.trigger_backend = u'parallel'",
    sprintf("var.parallel_port_address = %s", addr),
    "var.test_mode = u'no'",
    "import time",
    "def _printer(code):",
    "    print(u'[lexsync test trigger] %d' % int(code))",
    "    time.sleep(0.01)",
    "    print(u'[lexsync test trigger] 0')",
    "send_trigger = _printer",
    "try:",
    "    if var.trigger_backend == u'serial':",
    "        import serial",
    "        import serial.tools.list_ports",
    "        _ports = serial.tools.list_ports.comports()",
    "        if _ports:",
    "            _sp = serial.Serial(_ports[0].device)",
    "            def send_trigger(code):",
    "                _sp.write(int(code).to_bytes(1, 'big'))",
    "                time.sleep(0.01)",
    "                _sp.write((0).to_bytes(1, 'big'))",
    "        else:",
    "            var.test_mode = u'yes'",
    "    else:",
    "        from ctypes import windll",
    "        _io = windll.dlportio",
    "        _addr = int(var.parallel_port_address)",
    "        def send_trigger(code):",
    "            _io.DlPortWritePortUchar(_addr, int(code))",
    "            time.sleep(0.01)",
    "            _io.DlPortWritePortUchar(_addr, 0)",
    "except Exception as _exc:",
    "    var.test_mode = u'yes'",
    "    print(u'lexsync: trigger device unavailable; test mode.')",
    "send_trigger(0)"
  )

  event_blocks <- character(0); run_names <- character(0)
  for (i in seq_along(rendered)) {
    eb <- .osexp_event_block(sprintf("lexsync_e%d", i - 1L), rendered[[i]])
    event_blocks <- c(event_blocks, eb$block)
    run_names <- c(run_names, eb$run)
  }

  header <- c(
    "---", "API: 2.1", "OpenSesame: 3.3.14", "Platform: nt", "---",
    "set width 1024", "set uniform_coordinates yes",
    sprintf('set title "lexsync: %s (%s)"', design_name, language),
    "set subject_nr 0", "set start lexsync_experiment",
    "set sound_sample_size -16", "set sound_freq 48000", "set sound_channels 2",
    "set sound_buf_size 1024", "set sampler_backend legacy", "set round_decimals 2",
    "set mouse_backend legacy", "set keyboard_backend legacy", "set height 768",
    "set fullscreen no", "set foreground white", "set font_size 32",
    sprintf("set font_family %s", font), 'set description "Generated by lexsync"',
    "set coordinates uniform", "set compensation 0", "set color_backend legacy",
    "set clock_backend legacy", "set canvas_backend xpyriment", "set background black", "",
    "define inline_script lexsync_trigger_setup",
    '\tset description "Open the trigger device with a test-mode fallback"',
    '\tset _run ""', "\t___prepare__", paste0("\t", setup), "\t__end__", ""
  )
  trial <- c(
    "define sequence lexsync_trial",
    "\tset flush_keyboard yes",
    '\tset description "One trial: the paradigm event sequence"',
    sprintf("\trun %s always", run_names), "",
    "define loop lexsync_loop",
    sprintf('\tset source_file "%s"', conditions_file),
    # Sequential, so the loop presents the seeded trial order the CSV is sorted
    # by; OpenSesame's default (random) would discard it and diverge from the
    # PsychoPy and jsPsych targets.
    "\tset source file", "\tset repeat 1", "\tset order sequential",
    '\tset description "Present each item once"', "\trun lexsync_trial", "",
    "define sequence lexsync_experiment",
    "\tset flush_keyboard yes",
    '\tset description "Top-level experiment sequence"',
    "\trun lexsync_trigger_setup always", "\trun lexsync_loop always", ""
  )
  paste(c(header, event_blocks, trial), collapse = "\n")
}

#' Export a complete plain-text OpenSesame experiment
#'
#' @inheritParams export_psychopy
#' @return The path to the generated `.osexp`, invisibly.
#' @export
export_opensesame <- function(stimuli, design, schema, outdir, base = NULL) {
  base <- base %||% slugify(design$name, design$language)
  events <- resolve_events(design)
  rendered <- render_events(events, design$timing %||% list(), .refresh_hz(schema))
  csv_name <- paste0(base, "_opensesame.csv")
  write_csv_utf8(loop_table(stimuli, events), file.path(outdir, csv_name))
  presentation <- schema$presentation %||% list()
  font <- design$font %||% presentation$opensesame_font %||% "mono"
  txt <- build_osexp(design, csv_name, schema, rendered, font = font)
  out <- file.path(outdir, paste0(base, ".osexp"))
  write_lines_lf(txt, out)
  invisible(out)
}

# BCP 47 tags for the human-readable language labels the designs carry, covering the
# languages the corpus connectors derive lexica for (corpora/fetch_corpora.py).
.BCP47_TAGS <- c(english = "en", spanish = "es", french = "fr", german = "de",
                 dutch = "nl", italian = "it", portuguese = "pt",
                 chinese = "zh", `chinese (mandarin)` = "zh")

# The design's BCP 47 tag for the generated HTML lang attribute. `language` is a
# free-text label ("english"), which is not a valid tag, so it is mapped. A design
# may state `language_tag` outright, and a label that is already tag-shaped ("en",
# "en-GB") is taken as given. Anything else becomes "und" (BCP 47 "undetermined"):
# registered, and unlike lang="english" resolvable by a validator or a screen reader.
.language_tag <- function(design) {
  # The shape check applies to a STATED tag too. It used to be returned verbatim, so
  # `language_tag: 'en"><script>...'` reached the lang attribute of the generated HTML
  # intact -- the one input to this function that an attacker would actually choose.
  tag <- trimws(as.character(design$language_tag %||% ""))
  if (nzchar(tag)) {
    return(if (grepl("^[A-Za-z]{2,3}(-[A-Za-z0-9]{1,8})*$", tag, perl = TRUE)) tag else "und")
  }
  label <- trimws(as.character(design$language %||% ""))
  if (grepl("^[A-Za-z]{2,3}(-[A-Za-z0-9]{1,8})*$", label, perl = TRUE)) return(label)
  m <- .BCP47_TAGS[.lower_invariant(label)]
  if (is.na(m)) "und" else unname(m)
}

# Event-model key names (PsychoPy style) mapped to browser KeyboardEvent keys.
.JSPSYCH_KEYS <- c(left = "arrowleft", right = "arrowright", up = "arrowup",
                   down = "arrowdown", space = " ", "return" = "enter")

.map_keys_jspsych <- function(rendered) {
  mapk <- function(k) { m <- .JSPSYCH_KEYS[k]; unname(ifelse(is.na(m), k, m)) }
  lapply(rendered, function(e) {
    # [[ ]], not $: `$` partial-matches on a list, so `e$key` returns the value of
    # `keys` and the assignment then adds a `key` the Python engine never emits.
    if (!is.null(e[["keys"]])) e[["keys"]] <- mapk(e[["keys"]])
    if (!is.null(e[["key"]])) e[["key"]] <- mapk(e[["key"]])
    e
  })
}

.json_html <- function(x) {
  s <- as.character(x)
  s <- gsub("<", "\\u003c", s, fixed = TRUE)
  s <- gsub(">", "\\u003e", s, fixed = TRUE)
  s <- gsub("&", "\\u0026", s, fixed = TRUE)
  # U+2028 and U+2029 terminate a line in JavaScript but are not ASCII controls, so
  # clean_field passes them, and a raw one inside a <script> string literal is a
  # SyntaxError before ES2019. The Python twin escaped them and this did not, so the
  # same design produced different bytes from the two engines. Written as escape
  # sequences rather than literal characters so this file stays pure ASCII.
  s <- gsub("\u2028", "\\u2028", s, fixed = TRUE)
  gsub("\u2029", "\\u2029", s, fixed = TRUE)
}

#' Export a browser-runnable jsPsych experiment
#'
#' The rendered events and the trial data are embedded in one HTML file, so anyone
#' can reproduce the procedure online from the same materials. The jsPsych library
#' and stylesheet are loaded from a CDN, so the machine running the file needs an
#' internet connection; the trial data are embedded and the responses are saved
#' locally, so no server is required either to run it or to collect them. Onset
#' triggers are recorded in each trial's data (a browser cannot drive a parallel
#' port).
#'
#' @inheritParams export_psychopy
#' @return The path to the generated `.html`, invisibly.
#' @importFrom jsonlite toJSON
#' @export
export_jspsych <- function(stimuli, design, schema, outdir, base = NULL) {
  base <- base %||% slugify(design$name, design$language)
  events <- resolve_events(design)
  rendered <- .map_keys_jspsych(
    render_events(events, design$timing %||% list(), .refresh_hz(schema)))
  trials <- loop_table(stimuli, events)
  presentation <- schema$presentation %||% list()
  font <- design$font %||% presentation$font %||% "Courier New"
  tmpl <- paste(readLines(find_template("jspsych/experiment_template.html"), warn = FALSE),
                collapse = "\n")
  subs <- list(
    DESIGN = clean_meta(design$name, "the design's `name`"),
    LANGUAGE = clean_meta(design$language, "the design's `language`"),
    LANGUAGE_TAG = .language_tag(design), WORD_FONT = clean_meta(font, "the font"),
    EVENTS_JSON = .json_html(jsonlite::toJSON(rendered, auto_unbox = TRUE)),
    TRIALS_JSON = .json_html(jsonlite::toJSON(trials, dataframe = "rows"))
  )
  for (k in names(subs)) {
    tmpl <- gsub(sprintf("{{%s}}", k), as.character(subs[[k]]), tmpl, fixed = TRUE)
  }
  out <- file.path(outdir, paste0(base, ".html"))
  write_lines_lf(tmpl, out)
  invisible(out)
}

#' Export all presentation targets (PsychoPy, OpenSesame, jsPsych)
#'
#' @inheritParams export_psychopy
#' @return A named list of generated file paths.
#' @export
export_experiments <- function(stimuli, design, schema, outdir, base = NULL) {
  stimuli <- assign_triggers(stimuli)
  list(
    psychopy = export_psychopy(stimuli, design, schema, outdir, base),
    opensesame = export_opensesame(stimuli, design, schema, outdir, base),
    jspsych = export_jspsych(stimuli, design, schema, outdir, base)
  )
}
