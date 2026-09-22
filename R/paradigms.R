# paradigms.R -- the paradigm registry and the trial-event model. A paradigm is a
# named default sequence of trial events plus the fields it requires and a
# counterbalancing recipe. A design names a paradigm (and inherits its events) or
# supplies an explicit `events` list; every presentation backend renders the same
# event list, so adding a paradigm is purely a matter of configuration.
# Identical in structure to python_workflow/src/lexsync/paradigms.py.

#' The paradigm registry: default event sequences and required fields
#'
#' Each event is a list with `type` (fixation | text | mask | blank |
#' region_by_region | response | question | feedback), `content` (a literal or a
#' `{field}` reference), an optional `trigger` (an integer EEG code or the token
#' "condition"/"item"), `onset_locked`, response `keys`/`timeout_ms`, and an
#' optional `blocks` restricting the event to named blocks.
#'
#' @format A named list with one entry per paradigm (`factorial`,
#'   `lexical_decision`, `priming`, `categorisation`, `self_paced_reading`), each
#'   holding
#'   `stimulus_fields`, a `counterbalance` recipe and an `events` list.
#' @return A plain named list (class `"list"`; no package-specific class) of
#'   built-in paradigm specifications. Each element contains `stimulus_fields`,
#'   a character vector naming required item-table fields; `counterbalance`, a
#'   character scalar naming the counterbalancing recipe; and `events`, an
#'   ordered list of event-specification lists. The selected entry supplies the
#'   default trial sequence, required item fields, and counterbalancing rule
#'   inherited by a design that names that paradigm.
#' @export
PARADIGMS <- list(
  factorial = list(
    stimulus_fields = c("word"),
    counterbalance = "factorial",
    events = list(
      list(type = "fixation", content = "+", duration_frames = 30L),
      list(type = "text", content = "{word}", duration_frames = 48L,
           trigger = "condition", onset_locked = TRUE),
      list(type = "response", keys = c("left", "right"), timeout_ms = 2000L),
      list(type = "blank", duration_frames = 15L)
    )
  ),
  lexical_decision = list(
    stimulus_fields = c("target"),
    counterbalance = "factorial",
    events = list(
      list(type = "fixation", content = "+", duration_frames = 30L),
      list(type = "text", content = "{target}", duration_frames = 48L,
           trigger = "condition", onset_locked = TRUE),
      list(type = "response", keys = c("left", "right"), timeout_ms = 2000L),
      list(type = "blank", duration_frames = 15L)
    )
  ),
  priming = list(
    stimulus_fields = c("prime", "target"),
    counterbalance = "latin_square_target",
    events = list(
      list(type = "fixation", content = "+", duration_frames = 30L),
      list(type = "text", content = "{prime}", duration_frames = 3L,
           trigger = 20L, onset_locked = TRUE),
      list(type = "mask", content = "#####", duration_frames = 2L),
      list(type = "text", content = "{target}", duration_frames = 48L,
           trigger = "condition", onset_locked = TRUE),
      list(type = "response", keys = c("left", "right"), timeout_ms = 2000L),
      list(type = "blank", duration_frames = 15L)
    )
  ),
  # Cued semantic categorisation: a category question, then the word to judge against
  # it. The cue is what distinguishes this from lexical decision, and it is a separate
  # event rather than instructions shown once, because a category that varies by trial
  # is what the paradigm is for. Crossing the same words with different cues is
  # how a categorisation study separates a property of the word from the demands of the
  # task (a robin is a bird quickly and an animal slowly).
  #
  # `answer` holds the KEY that is correct for the trial, not a label, so scoring is a
  # string comparison against the recorded response with nothing to look up. It is a
  # field of the item table like any other; the paradigm requires it so that a design
  # cannot generate an unscoreable categorisation experiment.
  categorisation = list(
    stimulus_fields = c("target", "category", "answer"),
    # latin_square_target, not factorial. Each item carries both cues, so the factorial
    # recipe would give a participant the same target twice -- and the second
    # presentation would be a repetition-priming trial, not a categorisation trial. The
    # rotation gives each target once per list, under one cue.
    counterbalance = "latin_square_target",
    events = list(
      list(type = "fixation", content = "+", duration_ms = 500L),
      list(type = "text", content = "{category}", duration_ms = 750L),
      list(type = "text", content = "{target}", duration_ms = 800L,
           trigger = "condition", onset_locked = TRUE),
      list(type = "response", keys = c("f", "j"), timeout_ms = 2500L),
      list(type = "blank", duration_ms = 250L)
    )
  ),
  self_paced_reading = list(
    stimulus_fields = c("sentence", "question"),
    counterbalance = "latin_square_target",
    events = list(
      list(type = "fixation", content = "+", duration_frames = 30L),
      list(type = "region_by_region", content = "{sentence}", advance = "space",
           critical_region_trigger = "condition"),
      list(type = "question", content = "{question}", keys = c("f", "j"),
           timeout_ms = 5000L),
      list(type = "blank", duration_frames = 15L)
    )
  )
)

#' @keywords internal
get_paradigm <- function(name) {
  if (is.null(PARADIGMS[[name]])) {
    stop(sprintf("lexsync: unknown paradigm '%s'. Known paradigms: %s.",
                 name, paste(sort(names(PARADIGMS)), collapse = ", ")), call. = FALSE)
  }
  PARADIGMS[[name]]
}

#' The design's trial event list: its own `events`, else its paradigm's
#'
#' @param design A parsed design list.
#' @return The list of trial events the design presents.
#' @examples
#' vapply(resolve_events(list(paradigm = "lexical_decision")),
#'        function(e) e$type, character(1))
#' @export
resolve_events <- function(design) {
  if (!is.null(design$events) && length(design$events)) return(design$events)
  get_paradigm(design$paradigm %||% "factorial")$events
}

#' If `content` is a single braced field reference, return the bare field name
#' @keywords internal
content_field <- function(content) {
  if (!is.character(content) || length(content) != 1L) return(NULL)
  m <- regmatches(content, regexec("^\\s*\\{([A-Za-z_][A-Za-z0-9_]*)\\}\\s*$", content))[[1]]
  if (length(m) == 2L) m[2] else NULL
}

#' The ordered, unique trial fields referenced by an event list's content
#' @keywords internal
referenced_fields <- function(events) {
  fields <- character(0)
  for (ev in events) {
    f <- content_field(ev$content)
    if (!is.null(f) && !(f %in% fields)) fields <- c(fields, f)
  }
  fields
}

#' Trial fields a design needs present in its items (paradigm + events)
#'
#' @param design A parsed design list.
#' @return Character vector of the item fields the design's trials reference.
#' @examples
#' required_fields(list(paradigm = "categorisation"))
#' @export
required_fields <- function(design) {
  name <- design$paradigm %||% "factorial"
  base <- if (!is.null(PARADIGMS[[name]])) PARADIGMS[[name]]$stimulus_fields else character(0)
  for (f in referenced_fields(resolve_events(design))) {
    if (!(f %in% base)) base <- c(base, f)
  }
  base
}
