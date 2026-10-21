# =====================================================================
# RECONCILE_COUNTS.R   revision 6
#
# Prints every count the manuscript still has two values for, on one
# frame and under one convention, and matches each manuscript figure to
# the cell that produced it.
#
# WHAT REVISION 1 GOT WRONG, both worth stating because both are the same
# mistake in different places.
#
#   IT TRUSTED A NAME. Section A read `ev` from the session and reported
#   6,960 person-waves, 4,293 persons and 0 employed against a roster of
#   66,719, 10,650 and 56,309. The designs module rebinds `ev` after the
#   T0 blocks, so a warm session holds a later, narrower object under that
#   name, and Section C then failed on three absent columns. This is the
#   argument verify_manuscript_numbers.R makes about OUTDIR at its line 57:
#   a value inherited by name was diagnosed as a wrong moment rather than a
#   wrong place.
#
#   REVISION 3 ASKS THE REGISTRY FIRST. The contract persists the roster at
#   handoff/ev.rds and td_get() is the resolver, so the frame has a stable
#   address that survives a rebinding and a process boundary both. Content
#   resolution is kept as the fallback for a session where the contract has
#   not loaded, and every candidate is printed either way.
#
#   THE LADDER OMITTED A RESTRICTION THE MANUSCRIPT NAMES. Section 4.5
#   says "restricting to heterosexual couples where both partners appear
#   in the panel" and revision 1 applied only the second half. On the
#   first run the manifest sat 1.95 per cent below the ladder on
#   couple-waves and 1.94 per cent below it on person-waves, the same
#   ratio to three decimals, which is a restriction on the frame rather
#   than a counting difference. The ladder is now crossed with it so the
#   difference is measured instead of inferred.
#
# RUN IT AFTER 03a_design_audit.R, in the same session, or in any session
# where 00_contract.R has loaded and the registry carries `frame` and `ev`,
# OR IN A FRESH PROCESS, as RUN_ALL.R runs it (revision 6, below). It writes
# nothing unless WRITE_REPORT is TRUE and modifies nothing.
#
# WHAT REVISION 6 CHANGES, and why.
#
#   A FRESH PROCESS FOUND NOTHING. RUN_ALL.R runs every stage in its own
#   Rscript child, so the session had no contract, no `panel` and no `ev`.
#   On 5 October Section A fell back to content and found no roster, B and
#   C could not run, and the script ended "No section produced verdicts".
#   It still said "Done.", so the run looked finished while every assertion
#   in it had gone unchecked. The script now sources 00_contract.R when the
#   session lacks it, and takes the analysis sample from td_frame() when no
#   `panel` is in scope. A session after 03a is unchanged: what is already
#   in scope is used as before.
#
#   IT WRITES ITS OWN TRANSCRIPT. Revision 5 opened no sink, so RUN_ALL.R,
#   which checks each stage's newest log, reported "no new log" and could
#   not read the verdicts. The pattern is REFEREE_RUN.R's: open a log only
#   when nothing else is recording, and close only what was opened, on
#   every exit.
#
#   THE ROSTER IT EXPECTS IS v21.49's: 67,783 person-waves, 10,838 persons
#   and 57,182 employed, against 66,719, 10,650 and 56,309 before.
#
# BLOCKS ARE NAMED, NOT NUMBERED, because line numbers into the 03 stage
# went stale when it became five modules. The blocks this script reconciles
# against are: cov_wave and the T0 coverage table in 03_common.R, the T0b
# missingness table and nonemp_zero in 03a_design_audit.R, and the
# dual-earner stacked panel in 03c_experience_units.R.
# =====================================================================

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select <- dplyr::select;       filter    <- dplyr::filter
mutate <- dplyr::mutate;       slice     <- dplyr::slice
recode <- dplyr::recode;       rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange <- dplyr::arrange;     count     <- dplyr::count;  lag <- dplyr::lag

suppressPackageStartupMessages(library(dplyr))

.fmt <- function(x) format(x, big.mark = ",")
.hdr <- function(s) cat("\n", strrep("=", 70), "\n", s, "\n",
                        strrep("=", 70), "\n", sep = "")

# ---------------------------------------------------------------------
# Resolve a frame by the columns it carries, not by the name it holds.
# Every candidate is printed with its size, so a frame picked here can be
# overruled by eye. Returns NULL rather than guessing when nothing fits.
# ---------------------------------------------------------------------
.by_content <- function(need, extra = character(0), label = "") {
  nms <- ls(envir = .GlobalEnv)
  hits <- Filter(function(nm) {
    o <- tryCatch(get(nm, envir = .GlobalEnv), error = function(e) NULL)
    is.data.frame(o) && all(need %in% names(o)) &&
      (!length(extra) || any(extra %in% names(o)))
  }, nms)
  if (!length(hits)) {
    cat(sprintf("  no object carries %s%s\n",
                paste(need, collapse = ", "),
                if (length(extra))
                  paste0(" plus one of ", paste(extra, collapse = "/")) else ""))
    return(NULL)
  }
  tab <- do.call(rbind, lapply(hits, function(nm) {
    o <- get(nm, envir = .GlobalEnv)
    data.frame(object = nm, rows = nrow(o),
               persons = if ("person_id" %in% names(o))
                 dplyr::n_distinct(o$person_id) else NA_integer_,
               stringsAsFactors = FALSE)
  }))
  tab <- tab[order(-tab$rows), ]
  cat("  candidates for ", label, ":\n", sep = "")
  print(tab, row.names = FALSE)
  get(tab$object[1], envir = .GlobalEnv)
}

# The registry is the right answer when it is there: a key survives both a
# rebinding in a warm session and the process boundary RUN_ORDER puts between
# scripts, which is the failure the contract was written for.
.from_registry <- function(key) {
  if (!exists("td_has") || !exists("td_get")) return(NULL)
  ok <- tryCatch(isTRUE(td_has(key)), error = function(e) FALSE)
  if (!ok) return(NULL)
  o <- tryCatch(td_get(key), error = function(e) NULL)
  if (is.data.frame(o)) {
    cat(sprintf("  registry key '%s': %s rows\n", key, format(nrow(o), big.mark = ",")))
    o
  } else NULL
}

# ---------------------------------------------------------------------
# REVISION 6. THE TRANSCRIPT, opened only if nothing else is recording.
# The body below runs inside tryCatch(finally = ...), so an error anywhere
# in it cannot leave this file's sink open. tryCatch evaluates the body in
# the calling environment, which for a sourced file is the global one, so
# reconcile_report and reconcile_ok still land in the session.
# ---------------------------------------------------------------------
.rcl_sink0 <- sink.number()
.rcl_owns  <- (.rcl_sink0 == 0L)
.rcl_log   <- NA_character_
if (.rcl_owns) {
  .rcl_dir <- if (exists("LOG_DIR")) LOG_DIR else
    file.path(if (exists("PROJ_ROOT")) PROJ_ROOT else getwd(), "run_logs")
  dir.create(.rcl_dir, showWarnings = FALSE, recursive = TRUE)
  .rcl_log <- file.path(.rcl_dir, paste0(
    "RECONCILE_COUNTS_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
  sink(.rcl_log, split = TRUE)
  cat("Log: ", .rcl_log, "\n", sep = "")
  rm(.rcl_dir)
} else {
  cat("Log: inheriting a sink already open (", .rcl_sink0, "), so no ",
      "separate transcript is made.\n", sep = "")
}

tryCatch({

# THE CONTRACT, when the session lacks it: the registry that resolves `ev`
# and td_frame() that resolves the analysis sample both live there. Not
# found is not fatal, because Section A falls back to content.
if (!(exists("td_get", mode = "function") && exists("td_frame", mode = "function"))) {
  .c <- c("00_contract.R", file.path("..", "00_contract.R"))
  .c <- .c[file.exists(.c)]
  if (length(.c)) {
    cat("Contract: not loaded in this session; sourcing ",
        normalizePath(.c[1], winslash = "/"), "\n", sep = "")
    source(.c[1])
  } else {
    cat("Contract: 00_contract.R not found from ", getwd(),
        ", so the registry is unavailable.\n", sep = "")
  }
  rm(.c)
}

# =====================================================================
# A. WHICH FRAME IS WHICH
# =====================================================================
.hdr("A. Frames, resolved by content")

.panel <- if (exists("panel") && is.data.frame(panel)) panel else NULL
# A FRESH PROCESS HAS NO `panel`. The analysis sample 03a works on is the
# registry's frame, so take it from td_frame(), as paper_tables.R does. It
# is held as .panel only, so nothing named `panel` is left in a session
# that did not have one.
if (is.null(.panel) && exists("td_frame", mode = "function")) {
  .panel <- tryCatch(td_frame(context = "RECONCILE_COUNTS"),
                     error = function(e) {
                       cat("  td_frame() failed: ", conditionMessage(e), "\n", sep = "")
                       NULL
                     })
  if (!is.data.frame(.panel)) .panel <- NULL
}
if (!is.null(.panel))
  cat(sprintf("panel (analysis sample)   %s person-waves, %s persons\n",
              .fmt(nrow(.panel)), .fmt(dplyr::n_distinct(.panel$person_id))))

cat("\nEvent-study roster. The registry key first; failing that, the columns.\n")
.rost <- .from_registry("ev")
if (is.null(.rost)) {
  cat("  registry unavailable, falling back to content. Needs the gate columns\n")
  cat("  AND an event-time column, which is what distinguishes the roster from\n")
  cat("  the analysis sample.\n")
  .rost <- .by_content(need  = c("person_id", "wave", "employed",
                                 "training_module_asked", "jttpewt_obs"),
                       extra = c("k", "cohort"),
                       label = "the roster")
}
if (!is.null(.rost))
  cat(sprintf("\n  taking the largest: %s person-waves, %s persons, %s employed\n",
              .fmt(nrow(.rost)), .fmt(dplyr::n_distinct(.rost$person_id)),
              .fmt(sum(.rost$employed == 1, na.rm = TRUE))))
# v21.49, 5 October: the roster moved with the parenthood rule.
# v-screen, 14 October: no screen by default, so the roster is larger.
if (isTRUE(get0("TD_SCREEN_ON", ifnotfound = FALSE))) {
  cat("\n  Expected 67,783 / 10,838 / 57,182, the screened roster (TD_SCREEN=on).
  A smaller object means the registry was bypassed and a rebound `ev` was
  picked up instead, which is the Stage 3 symptom the runbook describes;
  134,131 instead of 129,544 on the analysis sample means the screen did
  not reach the frame.\n")
} else {
  cat("\n  Expected 68,361 / 10,902 / 57,271, the roster without the screen,
  the default since 14 October (67,783 / 10,838 / 57,182 with it). A
  smaller object means the registry was bypassed and a rebound `ev` was
  picked up instead, which is the Stage 3 symptom the runbook describes.
  The analysis sample is the whole extract, 134,131 person-waves.\n")
}
cat("\n  Table 1 (coverage) is cov_wave in 03_common.R, on the roster.\n")
cat("  Table 2 (missingness) is T0b in 03a_design_audit.R, on the analysis\n")
cat("  sample. Neither note should call its frame the estimation sample.\n")

# =====================================================================
# B. THE COUPLE LADDER, CROSSED WITH THE HETEROSEXUAL RESTRICTION
#
# A COUPLE-WAVE is a distinct couple_id-by-wave cell and a PERSON-WAVE is
# a row. pw_per_cw is the diagnostic: near 2 on a reciprocal frame, and 1
# wherever a person-wave count has been given a couple-wave label.
# =====================================================================
.hdr("B. Couple counts, one convention, both sex rules")

if (is.null(.panel) || !"partner_id" %in% names(.panel)) {
  cat("partner_id absent from panel; Section B cannot run.\n")
} else {

  .has_sex <- "female" %in% names(.panel)
  if (!.has_sex)
    cat("NOTE: no `female` column, so the heterosexual arm is skipped and\n",
        "      the ladder below is the unrestricted one only.\n", sep = "")

  .cnt <- function(d, label, rule) {
    cw <- dplyr::n_distinct(paste0(d$couple_id, "_", d$wave))
    data.frame(rule = rule, step = label,
               persons = dplyr::n_distinct(d$person_id),
               couples = dplyr::n_distinct(d$couple_id),
               couple_waves = cw, person_waves = nrow(d),
               pw_per_cw = round(nrow(d) / max(1L, cw), 2),
               stringsAsFactors = FALSE)
  }

  .base <- .panel %>%
    filter(!is.na(partner_id), partner_id != "") %>%
    mutate(couple_id = paste0(pmin(person_id, partner_id), "_",
                              pmax(person_id, partner_id)))

  .ids <- unique(.panel$person_id)
  .key <- paste0(.panel$person_id, "_", .panel$wave)

  # Partner sex, looked up on the person-wave the partner actually
  # occupies, so a couple whose partner is absent that wave is not
  # silently classed either way.
  if (.has_sex) {
    .sex <- .panel %>% select(person_id, wave, female) %>%
      rename(partner_id = person_id, p_female = female)
    .base <- .base %>% left_join(.sex, by = c("partner_id", "wave"))
  }

  .ladder <- function(d, rule) {
    s1 <- d
    s2 <- s1 %>% filter(partner_id %in% .ids)
    s3 <- s2 %>% filter(paste0(partner_id, "_", wave) %in% .key)
    # Both employed is mutual, so the couple-wave has to carry two
    # employed rows rather than one employed person with a partner.
    e  <- s3 %>% filter(employed == 1)
    s4 <- e %>% semi_join(e %>% count(couple_id, wave) %>% filter(n == 2) %>%
                            select(couple_id, wave), by = c("couple_id", "wave"))
    out <- rbind(.cnt(s1, "1. partner named", rule),
                 .cnt(s2, "2. partner a respondent somewhere", rule),
                 .cnt(s3, "3. partner a respondent same wave", rule),
                 .cnt(s4, "4. both partners employed", rule))
    # 3w EXISTS FOR ONE FIGURE. Appendix A.4 says "of 76,220 partner-matched
    # person-waves, 59,312 carry an observed wage", which is the same-wave
    # frame filtered on the index person's wage WITHOUT requiring either
    # partner employed. Revision 2's ladder had no such cell, so that figure
    # matched step 4 at 3.19 per cent and looked like the one A.4 number that
    # would not reconcile. It was the ladder that was missing a rung.
    .w3 <- intersect(c("hourly_wage_clean", "hourly_wage", "ln_wage"), names(s3))[1]
    if (!is.na(.w3))
      out <- rbind(out, .cnt(s3 %>% filter(!is.na(.data[[.w3]])),
                             "3w. partner same wave, index waged", rule))
    # TWO DUAL-EARNER READINGS, because the manuscript and the code do not
    # agree on which one it is. Section 4.5 says "both partners appear in
    # the wage sample", which is 5b. The stacked construction in
    # 03c_experience_units.R filters employed == 1, p_employed == 1 and
    # !is.na(ln_wage), where the wage test applies to the index person
    # only, which is 5a. Both are printed rather than one chosen, and on
    # the first run neither reproduced the manuscript's pair: every cell
    # built on the analysis sample sits below it, so the stacked panel's
    # base is wider than `panel` and that is the thing to identify first.
    .wcol <- intersect(c("hourly_wage_clean", "hourly_wage", "ln_wage"),
                       names(s4))[1]
    if (!is.na(.wcol)) {
      s5a <- s4 %>% filter(!is.na(.data[[.wcol]]))
      out <- rbind(out, .cnt(s5a, "5a. both employed, index waged", rule))
      s5b <- s5a %>% semi_join(s5a %>% count(couple_id, wave) %>%
                                 filter(n == 2) %>% select(couple_id, wave),
                               by = c("couple_id", "wave"))
      out <- rbind(out, .cnt(s5b, "5b. both employed, both waged", rule))
    } else {
      cat("  no wage column resolved, so steps 5a and 5b are absent.\n")
    }
    out
  }

  .all <- .ladder(.base, "any couple")
  if (.has_sex)
    .all <- rbind(.all, .ladder(.base %>%
                                  filter(!is.na(p_female), female != p_female),
                                "heterosexual"))
  print(.all, row.names = FALSE)

  # ---- the manuscript's counts, and the cell each is asserted against --
  #
  # REVISION 4 ASSERTS RATHER THAN SUGGESTS. Revisions 2 and 3 printed the
  # nearest cell to each figure, which found the couple triple and the
  # dual-earner error but left a reader to judge what "1.29 per cent off"
  # meant. Every figure now names the cell it should come from, so the run
  # returns PASS or FAIL and a changed sample shows up as a failure rather
  # than as a slightly worse nearest neighbour.
  #
  # TOLERANCES DIFFER BY SECTION AND THAT IS THE POINT. Section 4.5 is
  # computed on this panel and must be exact. Appendix A.4 is built from
  # the full-time-equivalent addendum rather than the panel, so it sits a
  # consistent tenth to a third of a per cent below every cell here; 0.5
  # per cent admits that and nothing more.
  .tex <- tibble::tribble(
    ~where,         ~quantity,                     ~value, ~rule,          ~step, ~field,          ~tol,
    # v-unscreened-values-2, 18 October: the unscreened sample. 76,220, 51,877 and 5,554 are no
    # longer in the manuscript, so their rows are gone; 59,312 is now 60,932 in E.1.
    "Section 4.5",  "couples",                       6762, "heterosexual", "3.",  "couples",       0,
    "Section 4.5",  "couple-waves",                 39150, "heterosexual", "3.",  "couple_waves",  0,
    "Section 4.5",  "person-waves",                 78299, "heterosexual", "3.",  "person_waves",  0,
    "Section 4.5",  "partner present somewhere",    85021, "any couple",   "2.",  "person_waves",  0,
    "Section 4.5",  "dual-earner couples",           5140, "heterosexual", "5b",  "couples",       0,
    "Section 4.5",  "dual-earner couple-waves",     23849, "heterosexual", "5b",  "couple_waves",  0,
    "Section 4.5",  "dual-earner person-waves",     47698, "heterosexual", "5b",  "person_waves",  0,
    "Appendix A.4", "couples",                       6931, "any couple",   "3.",  "couples",       0.5,
    "Appendix A.4", "couple-waves",                 39922, "any couple",   "3.",  "couple_waves",  0.5,
    "Appendix E.1", "with an observed wage",        60932, "any couple",   "3w",  "person_waves",  0.5)

  .cell <- function(rule, step, field) {
    r <- .all[.all$rule == rule & substr(.all$step, 1, 2) == substr(step, 1, 2), ]
    if (!nrow(r)) return(NA_real_)
    as.numeric(r[[field]][1])
  }
  .chk <- do.call(rbind, lapply(seq_len(nrow(.tex)), function(i) {
    x <- .cell(.tex$rule[i], .tex$step[i], .tex$field[i])
    pct <- if (is.na(x)) NA_real_ else 100 * abs(x - .tex$value[i]) / .tex$value[i]
    data.frame(where = .tex$where[i], quantity = .tex$quantity[i],
               tex = .tex$value[i], run = x, pct = round(pct, 3),
               verdict = if (is.na(x)) "NO CELL" else
                 if (pct <= .tex$tol[i]) "pass" else "FAIL",
               expected = paste(.tex$rule[i], .tex$step[i], .tex$field[i]),
               stringsAsFactors = FALSE)
  }))
  cat("\nEach manuscript count against the cell it is asserted to come from.\n")
  print(.chk, row.names = FALSE)
  .bad <- sum(.chk$verdict != "pass")
  cat(sprintf("\n  %d of %d pass.\n", nrow(.chk) - .bad, nrow(.chk)))
  if (.bad)
    cat("  *** A FAIL is one of three things and the ratios tell them apart.\n",
        "      A constant ratio across couple-waves and person-waves within a\n",
        "      section is a restriction on the frame. A ratio that differs\n",
        "      between them is a counting convention, which is how 74,989 came\n",
        "      to be labelled couple-waves. A figure matching no cell at all is\n",
        "      usually another design's number quoted here, which is what the\n",
        "      old dual-earner pair turned out to be.\n", sep = "")


  .r3 <- .all[.all$step == "3. partner a respondent same wave", ]
  for (i in seq_len(nrow(.r3)))
    cat(sprintf("\nRECIPROCITY, %s, same-wave frame: 2 x couple-waves - person-waves = %s\n",
                .r3$rule[i], .fmt(2L * .r3$couple_waves[i] - .r3$person_waves[i])))
  cat("  Zero means every couple-wave carries both partners' rows. A small\n")
  cat("  positive number is the count that carry only one, so the two\n")
  cat("  quantities cannot be converted into each other by halving.\n")

  .rc_b <- .chk   # kept for the combined report at the foot
  rm(.cnt, .base, .ids, .key, .ladder, .all, .tex, .cell, .chk, .bad, .r3, .has_sex)
  if (exists(".sex")) rm(.sex)
}

# =====================================================================
# C. THE ASKED-POPULATION TRAINING RATE
#
# 7.9 per cent in Section 4.3 against 0.087 in Section 5.4. The quantity
# is rate_obs in the nonemp_zero block: the training rate among
# NON-EMPLOYED person-waves that were nonetheless asked the gate item.
# Both counts that depend on it are recomputed, because Section 4.3
# quotes all three and they have to multiply.
# =====================================================================
.hdr("C. The asked population among the non-employed")

# The two values were never rivals. Both are the training rate among
# NON-EMPLOYED person-waves that were nonetheless asked the gate item, and
# each is right on the frame its own section describes: 0.087 on the
# unconditional arm's roster and 0.080 on the wider analysis sample. What was
# stale was the triple in Section 4.3, which multiplied on a frame no current
# object builds. Both sites are now asserted, so a repeat of that cannot pass.
.rate_block <- function(d, nm) {
  ne <- d %>% filter(employed == 0)
  n_asked <- sum(ne$training_module_asked == 1, na.rm = TRUE)
  n_never <- nrow(ne) - n_asked
  rate <- mean(ne$jttpewt_obs[ne$training_module_asked == 1], na.rm = TRUE)
  data.frame(frame = nm, non_employed_pw = nrow(ne), asked = n_asked,
             never_asked = n_never, rate_among_asked = round(rate, 4),
             implied_understatement = round(n_never * rate),
             stringsAsFactors = FALSE)
}
.need <- c("employed", "training_module_asked", "jttpewt_obs")
.out <- list()
if (!is.null(.rost) && all(.need %in% names(.rost)))
  .out[[length(.out) + 1L]] <- .rate_block(.rost, "roster")
if (!is.null(.panel) && all(.need %in% names(.panel)))
  .out[[length(.out) + 1L]] <- .rate_block(.panel, "analysis sample")

if (!length(.out)) {
  cat("Neither frame carries all of: ", paste(.need, collapse = ", "),
      "\nSection C cannot run. If the roster candidates in Section A were\n",
      "all small, `ev` has been reassigned and the columns are gone with\n",
      "it; re-source 03 up to the T0 blocks.\n", sep = "")
} else {
  .rt <- do.call(rbind, .out)
  print(.rt, row.names = FALSE)

  # TOLERANCE FROM THE QUOTED PRECISION, which is the rule
  # verify_manuscript_numbers.R applies at its .tol_for(): a figure written to
  # three decimals is asserted to three, and one written as an integer has to
  # be the integer. Comparing 0.0801 against a manuscript's 0.080 at machine
  # precision would fail every rounded number in the paper.
  .tol_for <- function(v) {
    txt <- format(v, scientific = FALSE, trim = TRUE)
    d <- if (grepl("[.]", txt)) nchar(sub("^.*[.]", "", txt)) else 0
    0.5 * 10^(-d)
  }
  .tex <- tibble::tribble(
    ~where,        ~quantity,                    ~value, ~frame,            ~field,
    # v-unscreened-values-2, 18 October: 17,514 and 1,390, which the text prints as \NeverAsked
    # and \ImputationUnderstates. With the screen they were 14,466 and 1,159.
    "Section 4.3", "routed-out person-waves",     17514, "analysis sample", "never_asked",
    "Section 4.3", "rate among those asked",      0.080, "analysis sample", "rate_among_asked",
    "Section 4.3", "implied understatement",       1390, "analysis sample", "implied_understatement",
    "Section 5.4", "rate on the arm's own frame", 0.087, "roster",          "rate_among_asked",
    "Section 5.4", "rate on the analysis sample", 0.080, "analysis sample", "rate_among_asked")

  .chk_c <- do.call(rbind, lapply(seq_len(nrow(.tex)), function(i) {
    r <- .rt[.rt$frame == .tex$frame[i], ]
    x <- if (nrow(r)) as.numeric(r[[.tex$field[i]]][1]) else NA_real_
    tol <- .tol_for(.tex$value[i])
    data.frame(where = .tex$where[i], quantity = .tex$quantity[i],
               tex = .tex$value[i], run = x, frame = .tex$frame[i],
               verdict = if (is.na(x)) "NO FRAME" else
                 if (abs(x - .tex$value[i]) <= tol) "pass" else "FAIL",
               stringsAsFactors = FALSE)
  }))
  cat("\nEach manuscript figure against the frame it names.\n")
  print(.chk_c, row.names = FALSE)
  .bad_c <- sum(.chk_c$verdict != "pass")
  cat(sprintf("\n  %d of %d pass.\n", nrow(.chk_c) - .bad_c, nrow(.chk_c)))

  # THE TRIPLE HAS TO MULTIPLY, and this is the check that caught it being
  # stale. The superseded 17,514 x 0.079 = 1,384 against a quoted 1,390 was
  # internally consistent, which is why it survived four passes: a coherent
  # set from a build nothing else in the paper still used.
  .a <- .rt[.rt$frame == "analysis sample", ]
  if (nrow(.a)) {
    .prod <- .a$never_asked[1] * .a$rate_among_asked[1]
    cat(sprintf("\n  Consistency: %s never asked x %.4f = %s against the %s\n",
                .fmt(.a$never_asked[1]), .a$rate_among_asked[1],
                .fmt(round(.prod)), .fmt(.a$implied_understatement[1])))
    cat("  reported. A triple that multiplies is necessary and not sufficient:\n")
    cat("  the superseded one multiplied too, on a frame no object here builds.\n")
    cat("  Check the routed-out count against a frame as well as the product.\n")
  }
  if (.bad_c)
    cat("\n  *** A FAIL here is a frame or a vintage. Read the two rows of the\n",
        "      table above: if the manuscript figure matches the OTHER frame,\n",
        "      the section names the wrong one; if it matches neither, it is\n",
        "      from a build these objects no longer reproduce.\n", sep = "")
  .rc_c <- .chk_c  # kept for the combined report at the foot
  rm(.rt, .tol_for, .tex, .chk_c, .bad_c, .a)
  if (exists(".prod")) rm(.prod)
}

# =====================================================================
# THE VERDICTS, IN ONE OBJECT
#
# The script printed and returned nothing, which is fine for a one-off and
# wrong for a check. A FAIL was visible only to whoever read the transcript,
# and the runbook's own history is a list of things that were in a transcript
# and not noticed. `reconcile_report` is left in the session so a caller can
# test it, and WRITE_REPORT writes it beside the other artefacts, which is
# the convention verify_manuscript_numbers.R uses for its own output.
# =====================================================================
.hdr("Verdicts")

.rc <- function(d, sect, ctx) if (is.null(d)) NULL else
  data.frame(section = sect, where = d$where, quantity = d$quantity,
             tex = d$tex, run = d$run, context = as.character(d[[ctx]]),
             verdict = d$verdict, stringsAsFactors = FALSE)

reconcile_report <- do.call(rbind, Filter(Negate(is.null), list(
  if (exists(".rc_b")) .rc(.rc_b, "B. couple counts", "expected"),
  if (exists(".rc_c")) .rc(.rc_c, "C. asked population", "frame"))))

if (is.null(reconcile_report)) {
  cat("No section produced verdicts; nothing to report.\n")
  reconcile_ok <- NA
} else {
  .n  <- nrow(reconcile_report)
  .np <- sum(reconcile_report$verdict == "pass")
  reconcile_ok <- (.np == .n)
  cat(sprintf("%d of %d assertions pass.\n", .np, .n))
  if (!reconcile_ok) {
    cat("\nFailures:\n")
    print(reconcile_report[reconcile_report$verdict != "pass", ], row.names = FALSE)
  }
  cat("\n  reconcile_report and reconcile_ok are in the session.\n")
  if (isTRUE(get0("WRITE_REPORT", ifnotfound = FALSE))) {
    .f <- if (exists("DIR_DESIGNS")) file.path(DIR_DESIGNS, "reconcile_counts.csv")
          else "reconcile_counts.csv"
    utils::write.csv(reconcile_report, .f, row.names = FALSE)
    cat("  written to ", .f, "\n", sep = "")
    rm(.f)
  } else {
    cat("  Set WRITE_REPORT <- TRUE to write reconcile_counts.csv beside the\n")
    cat("  other artefacts, so a later pass can diff against this one.\n")
  }
  rm(.n, .np)
}

rm(.rate_block, .need, .out, .by_content, .from_registry, .rc, .fmt, .hdr)
if (exists(".rc_b")) rm(.rc_b)
if (exists(".rc_c")) rm(.rc_c)
cat("\nDone.\n")

}, finally = {
  # CLOSE ONLY WHAT THIS FILE OPENED, on success and on error alike. A run
  # that inherited a sink leaves it exactly as it found it.
  if (isTRUE(get0(".rcl_owns", ifnotfound = FALSE)))
    while (sink.number() > get0(".rcl_sink0", ifnotfound = 0L)) sink()
  if (!is.na(get0(".rcl_log", ifnotfound = NA_character_)))
    cat("Log written to ", .rcl_log, "\n", sep = "")
  rm(list = intersect(c(".rcl_owns", ".rcl_sink0", ".rcl_log"),
                      ls(globalenv(), all.names = TRUE)), envir = globalenv())
})
