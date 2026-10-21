# =====================================================================
# REFEREE_RUN.R   revision 4
#
# Produces the numbers the referee response needs and cannot supply from
# the existing artefacts. Nothing here is a rewrite; each block answers
# one numbered concern and writes one small csv, so the response can be
# drafted against output rather than against intent.
#
#   A. concern 7   standard errors for the cross-partial profile
#   B. concern 5   the headline on validated births beside all transitions
#   C. concern 8   cohort composition of the two Sun-Abraham arms
#   D. concern 8   the static and the cohort-robust estimate side by side
#   E. concern 3   what job or employer identifier this panel actually has
#   F. concern 3   the stable-employer test block E showed was possible
#   G. terminology the fee decomposition read against its own base
#
# ORDER MATTERS AND A IS FIRST. It is the only block whose result can
# turn a caveat back into a conclusion: Section 7.3 currently reports a
# profile with no uncertainty attached, so it cannot say more than "no
# evidence of a difference". With standard errors it either can or it
# demonstrably cannot, and both are worth knowing before the terminology
# decision, because a section that survives is a section worth renaming
# carefully.
#
# RUN IT AFTER 03a_design_audit.R, in the same session, so `panel` and
# the registry are present, OR IN A FRESH PROCESS, as RUN_ALL.R runs it:
# revision 4 then loads `panel` from the registry (see the note above the
# panel check). Every block is guarded: a missing column skips its block
# and says which column and which concern goes unanswered rather than
# failing the run.
#
# ---------------------------------------------------------------------
# WHAT REVISION 2 CHANGES, and why each change exists
#
# 1. WRITE_REPORT DEFAULTS TO TRUE. Revision 1 honoured it and never set
#    it, so the run printed everything and wrote nothing. Block G's
#    bootstrap ran for two minutes and its interval survived only in the
#    console, which is how the response came to quote an interval of
#    [1.00, 1.30] for a statistic the run put at [0.990, 1.296]. A number
#    that exists only in a transcript is a number that gets transcribed.
#
# 2. BLOCK G WRITES THE STATE TABLE AS WELL AS THE BOOTSTRAP. The
#    provenance row names two artefacts and revision 1 wrote one.
#
# 3. BLOCK G COMPUTES ITS VERDICTS INSTEAD OF PREDICTING THEM. Revision 1
#    closed with a paragraph on what a trial on simulated cells produced,
#    which was then read as if it described this run. It did not: the
#    trial's index excluded one and this one's does not. The paragraph is
#    replaced by three verdicts the block derives from its own replicate
#    distribution, so the reading is in the output rather than in the
#    reader.
#
# 4. INTERVALS ARE WRITTEN AT FOUR DECIMALS WITH EXPLICIT FLAGS. The
#    concentration verdict turns on whether a bound is 0.99 or 1.00.
#    Rounding to two decimals before a human reads it decides the
#    question by typography.
#
# 5. BOOT_R DEFAULTS TO 1000. At 400 replicates the 2.5 per cent endpoint
#    is the tenth order statistic, and the tenth order statistic is
#    exactly where the concentration verdict is decided.
#
# 6. BLOCK A ALSO EMITS ITS TWO COEFFICIENTS PER SEX. The six evaluated
#    horizons are one linear function of experience, not six estimates,
#    and paper_tables.R needs the coefficients and their covariance to
#    print the delta-method column that app_b2_profile.tex still lacks.
#
# 7. BLOCK B PRINTS ITS OWN VALIDATED SHARE. Revision 1 reported three
#    regressions and left the share to be backed out of person counts.
#    The block's comment about the referee's 22.3 per cent was also wrong
#    and is corrected below.
#
# 8. BLOCK D COMPUTES THE AGREEMENT INSTEAD OF ASSERTING IT. Revision 1
#    printed two hard-coded numbers and a recommendation.
# =====================================================================

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select <- dplyr::select;       filter    <- dplyr::filter
mutate <- dplyr::mutate;       slice     <- dplyr::slice
recode <- dplyr::recode;       rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange <- dplyr::arrange;     count     <- dplyr::count;  lag <- dplyr::lag

suppressPackageStartupMessages({ library(dplyr) })

# =====================================================================
# LOG  (revision 3)
#
# WHY THIS FILE NEEDED ONE AT ALL. Every block here prints a table and
# then a paragraph saying how to read it, and the reading is the point:
# "the interval INCLUDES one" is a verdict, not a number, and it existed
# only on a console. That is how Section 6.1 came to publish [1.00, 1.30]
# for a statistic this file has twice put at [0.99, 1.28]. A transcript is
# the cheapest half of the fix and WRITE_REPORT is the other half.
#
# IT OWNS THE SINK ONLY IF NOTHING ELSE DOES. This file expects to run in
# 03a's session, and a module that errored before its finisher leaves a
# sink open; opening another on top writes this run into that module's
# transcript and names it for a script that was not running. The pattern
# is 04financing_space.R's: record the incoming depth, open only at zero,
# unwind only to what was recorded.
#
# rr_log_close() IS EXPOSED because a block that stops mid-run leaves the
# console captured, and hunting for `sink()` while output goes nowhere is
# a bad five minutes.
# =====================================================================
.rr_sink_out_before <- sink.number()
.rr_sink_msg_before <- sink.number(type = "message")
.rr_owns_sink <- (.rr_sink_out_before == 0L)
.rr_log <- NA_character_

if (.rr_owns_sink) {
  .rr_dir <- if (exists("LOG_DIR")) LOG_DIR else
    file.path(if (exists("PROJ_ROOT")) PROJ_ROOT else getwd(), "run_logs")
  dir.create(.rr_dir, showWarnings = FALSE, recursive = TRUE)
  .rr_log <- file.path(.rr_dir, paste0(
    "REFEREE_RUN_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
  sink(.rr_log, split = TRUE)
  cat("Log: ", .rr_log, "\n", sep = "")
  cat("  (standalone run; this file owns the sink and will close it)\n")
  cat("  If a block aborts, restore the console with: rr_log_close()\n")
} else {
  cat(sprintf(paste0("Log: inheriting a sink already open (%d output). This ",
                     "run is written\n  into whatever is already recording ",
                     "and no separate transcript is made.\n  If that was not ",
                     "intended, a script errored before closing its own:\n  ",
                     "run sink() until sink.number() reaches 0, then source ",
                     "this file again.\n"), .rr_sink_out_before))
}

rr_log_close <- function() {
  if (!isTRUE(get0(".rr_owns_sink", ifnotfound = FALSE))) {
    cat("rr_log_close(): this file does not own a sink; nothing to do.\n")
    return(invisible(NULL))
  }
  while (sink.number(type = "message") > .rr_sink_msg_before)
    sink(type = "message")
  while (sink.number() > .rr_sink_out_before) sink()
  cat("Log written to ", .rr_log, "\n", sep = "")
  invisible(.rr_log)
}
.have_fixest <- requireNamespace("fixest", quietly = TRUE)
.have_tidyr  <- requireNamespace("tidyr",  quietly = TRUE)

# WRITE_REPORT. Set here rather than inherited, and TRUE rather than
# FALSE. A block whose only expensive output is a bootstrap interval
# should not have "wrote nothing" as its default outcome. Set it to FALSE
# before sourcing if a dry run is what is wanted.
if (!exists("WRITE_REPORT")) WRITE_REPORT <- TRUE

# .fmt lives in RECONCILE_COUNTS.R and this file used it without defining
# it, which the bootstrap only reached after the point estimates printed.
# Helpers are defined where they are used rather than inherited from
# whatever else happens to have been sourced into the session.
.fmt <- function(x) format(x, big.mark = ",")
.hdr <- function(s) cat("\n", strrep("=", 70), "\n", s, "\n",
                        strrep("=", 70), "\n", sep = "")
.skip <- function(concern, why)
  cat("  SKIPPED. ", why, "\n  Concern ", concern,
      " goes unanswered and the response must say so.\n", sep = "")
.outdir <- function() if (exists("DIR_DESIGNS")) DIR_DESIGNS else "."
.out <- function(x, f) {
  if (!isTRUE(get0("WRITE_REPORT", ifnotfound = FALSE))) {
    cat("  (not written; WRITE_REPORT is FALSE)\n"); return(invisible(NULL))
  }
  p <- file.path(.outdir(), f)
  utils::write.csv(x, p, row.names = FALSE)
  cat("  wrote ", p, "\n", sep = "")
}

# Artefact lookup for block D. The directory layout has moved twice, so
# the file is searched for rather than assumed, and the path that hit is
# printed so the reader knows which copy was read.
.find_art <- function(f) {
  d <- unique(c(if (exists("DIR_DESIGNS")) DIR_DESIGNS,
                if (exists("DIR_RESULTS")) DIR_RESULTS,
                if (exists("PROJ_ROOT")) PROJ_ROOT,
                if (exists("project_dir")) project_dir,
                getwd(), file.path(getwd(), "output")))
  h <- file.path(d, f); h <- h[file.exists(h)]
  if (length(h)) h[1] else NA_character_
}

# REVISION 4. A FRESH PROCESS HAS NO `panel`, AND RUN_ALL.R STARTS ONE.
#
# RUN_ALL.R runs every stage in its own Rscript child, so "after 03a, in
# the same session" can never hold there. On 5 October the referee stage
# died on the check below and wrote nothing. A session after 03a is
# unchanged: what is already in scope is used as before. A fresh process
# now loads what 03a would have left, from where 03a gets it:
#   00_contract.R  td_frame() and the registry, and DIR_DESIGNS, so the
#                  csvs land in output/designs, not the working directory;
#   00compat.R     event_col(), without which block B skips;
#   td_frame()     the registry's frame as `panel`, the one analysis
#                  sample every script reads (129,544 rows on 5 October).
# The event frame for B and F already falls back to td_require("ev").
if (!(exists("panel") && is.data.frame(panel))) {
  if (!exists("td_frame", mode = "function") && file.exists("00_contract.R")) {
    cat("`panel` is not in the session: sourcing 00_contract.R for td_frame()\n")
    source("00_contract.R")
  }
  if (!exists("event_col", mode = "function") && file.exists("00compat.R")) {
    cat("sourcing 00compat.R for event_col()\n")
    source("00compat.R")
  }
  if (!exists("event_col", mode = "function"))
    cat("NOTE: event_col() is still not in scope, so block B will skip.\n")
  if (exists("td_frame", mode = "function"))
    panel <- td_frame(need_cols = c("person_id", "wave", "female", "ever_parent"),
                      context = "REFEREE_RUN")
}
.panel <- if (exists("panel") && is.data.frame(panel)) panel else NULL
if (is.null(.panel))
  stop("`panel` is not in the session and td_frame() did not supply it. ",
       "Run 03a first, in this session.", call. = FALSE)
if (!.have_fixest)
  cat("NOTE: fixest is absent. Blocks A, B, F and G need it and will skip.\n",
      "     02fte_experience.R already depends on it, so this is a session\n",
      "     problem rather than a missing dependency.\n", sep = "")
if (!.have_tidyr)
  cat("NOTE: tidyr is absent. Blocks A and C reshape with it and will skip.\n")
cat("Output directory: ", .outdir(), "   WRITE_REPORT = ",
    isTRUE(WRITE_REPORT), "\n", sep = "")

# =====================================================================
# THE EVENT FRAME, so B and F can be fitted on the paper's own sample.
#
# Both blocks build their own transition indicator on `panel` and gate it
# on employment. That is a wider sample than the one the headline is
# estimated on: 103,397 person-waves across 17,570 persons on the
# 21 September run, against the event sample's 66,719 and 10,650, of
# which the employed-only arm keeps 54,202. Both blocks say in their
# console text that the level does not travel and only the ratio does.
# The manuscript quotes the level anyway, at lines 483 and 859, and calls
# -0.124 and -0.116 alike "the employed frame".
#
# Rather than argue with the prose, fit the same arms on `ev` as well.
# The wide-frame rows stay exactly what they were; the event-sample rows
# give a reference that belongs beside the headline. Every output row now
# names its frame and its treated-person count, so a level can be quoted
# with the sample it came from.
# =====================================================================
.evframe <- local({
  e <- NULL
  if (exists("ev") && is.data.frame(ev)) e <- ev
  if (is.null(e) && exists("td_require", mode = "function"))
    tryCatch({
      r <- td_require("ev")
      e <- if (is.data.frame(r)) r else get0("ev", ifnotfound = NULL)
    }, error = function(err) NULL)
  if (is.null(e)) {
    p <- .find_art(file.path("handoff", "ev.rds"))
    if (is.na(p)) p <- .find_art("ev.rds")
    if (!is.na(p)) e <- tryCatch(readRDS(p), error = function(err) NULL)
  }
  if (!is.data.frame(e)) return(NULL)
  if (!"person_id" %in% names(e) && "id" %in% names(e)) e$person_id <- e$id
  e
})
.ev_ok <- function(cols)
  is.data.frame(.evframe) && all(cols %in% names(.evframe))
# The birth wave, however the frame carries it: the panel records
# ever_parent turning on, the event frame usually records the cohort.
.bw_col <- function(df) {
  if ("cohort" %in% names(df))
    return(ifelse(df$cohort < 10000, as.numeric(df$cohort), NA_real_))
  if (all(c("ever_parent", "person_id", "wave") %in% names(df))) {
    lk <- df %>% filter(!is.na(ever_parent)) %>% arrange(person_id, wave) %>%
      group_by(person_id) %>%
      summarise(.bw = { dd <- c(NA, diff(ever_parent))
                        i <- which(!is.na(dd) & dd > 0)
                        if (length(i)) wave[i[1]] else NA_real_ },
                .groups = "drop")
    return(lk$.bw[match(df$person_id, lk$person_id)])
  }
  if (all(c("k", "wave") %in% names(df)))
    return(ifelse(is.na(df$k), NA_real_, df$wave - df$k))
  rep(NA_real_, nrow(df))
}
if (is.null(.evframe)) {
  cat("NOTE: the event frame did not resolve, from `ev` in the session, from\n")
  cat("      td_require(\"ev\") or from handoff/ev.rds. Blocks B and F report\n")
  cat("      their own frame only, and their levels do not belong beside the\n")
  cat("      paper's headline arm. Quote the ratio.\n")
} else {
  cat(sprintf("Event frame: %s rows, %s persons\n",
              .fmt(nrow(.evframe)),
              .fmt(dplyr::n_distinct(.evframe$person_id))))
}

# =====================================================================
# A. STANDARD ERRORS FOR THE CROSS-PARTIAL PROFILE          concern 7
#
# Appendix A.1 fits, separately by sex on the wage sample,
#
#   ln w = g1 (educ_c * exp_c) + g2 (educ_c * exp_c^2) + G'X + a_i + l_t
#
# and reports the cross-partial at experience e as g1 + 2 g2 (e - ebar_s).
# That has a closed form, so its variance has one too, and the table's
# missing column is a delta method away rather than a new design:
#
#   Var = V11 + 4 (e-ebar)^2 V22 + 4 (e-ebar) V12
#
# THE DIFFERENCE SUMS THE TWO VARIANCES, which is valid here for the
# reason Section 5.5 gives about the sex-specific arms generally: they
# are fitted on disjoint persons. That is the one place this block
# assumes rather than computes, and it is the paper's own convention.
#
# SIX ROWS ARE TWO COEFFICIENTS. The profile is linear in e by
# construction, so the six evaluated horizons are g1 and g2 read at six
# points and not six estimates. Printing six independent-looking rows
# over-counts the evidence by three, which is why revision 2 writes the
# coefficients and their covariance as well as the evaluated table.
# paper_tables.R needs them either way: app_b2_profile.tex currently
# carries three numeric columns and no uncertainty at all, which is the
# gap this block exists to close and did not, because nothing was
# written.
#
# WHAT A NULL RESULT HERE WOULD MEAN. If the profile's difference is
# indistinguishable from zero at every horizon, Section 7.3 keeps the
# wording it now has and the referee is right that it cannot carry a
# principal conclusion. If the early horizons are signed, the section can
# say women's complementarity EXCEEDS men's early rather than only that
# there is no evidence against it. Either way the section stops resting
# on a table with no uncertainty in it.
# =====================================================================
.hdr("A. The cross-partial, with standard errors      [concern 7]")

.need_a <- c("ln_hourly_wage", "educ_years", "experience_years", "female",
             "person_id", "wave")
if (!.have_fixest) .skip(7, "fixest unavailable") else
if (!.have_tidyr) .skip(7, "tidyr unavailable") else
if (!all(.need_a %in% names(.panel))) {
  .skip(7, paste("panel lacks:", paste(setdiff(.need_a, names(.panel)),
                                       collapse = ", ")))
} else {
  .w <- .panel %>%
    filter(!is.na(ln_hourly_wage), !is.na(educ_years), !is.na(experience_years),
           !is.na(female))
  if ("in_wage_sample" %in% names(.w)) .w <- .w %>% filter(in_wage_sample == 1)

  # The controls Appendix A.1 names. Anything absent is dropped and named,
  # because a profile fitted on a different control set is a different
  # profile and the table note says which one it is.
  .want <- c("experience_years", "age_sq", "married")
  .alt  <- list(age_sq = c("age_sq", "age2"), married = c("married", "partnered"))
  .ctrl <- unlist(lapply(.want, function(v) {
    cand <- if (!is.null(.alt[[v]])) .alt[[v]] else v
    hit <- intersect(cand, names(.w)); if (length(hit)) hit[1] else NA_character_
  }))
  .missing_ctrl <- .want[is.na(.ctrl)]
  if (length(.missing_ctrl))
    cat("  controls unavailable and omitted: ",
        paste(.missing_ctrl, collapse = ", "),
        ". The profile is not the appendix's until they are restored.\n", sep = "")
  .ctrl <- .ctrl[!is.na(.ctrl)]

  # One fit per sex. The evaluated profile and the coefficients behind it
  # come back together so the second cannot be lost while the first is
  # printed, which is what happened in revision 1.
  .one <- function(s) {
    dd <- .w %>% filter(female == s)
    eb <- mean(dd$experience_years, na.rm = TRUE)
    db <- mean(dd$educ_years, na.rm = TRUE)
    dd <- dd %>% mutate(.ec = educ_years - db, .xc = experience_years - eb)
    f <- stats::as.formula(paste0(
      "ln_hourly_wage ~ I(.ec * .xc) + I(.ec * .xc^2)",
      if (length(.ctrl)) paste0(" + ", paste(.ctrl, collapse = " + ")) else "",
      " | person_id + wave"))
    m <- fixest::feols(f, data = dd, cluster = ~person_id)
    b <- stats::coef(m); V <- stats::vcov(m)
    i1 <- grep("\\.ec \\* \\.xc\\)", names(b))[1]
    i2 <- grep("\\.xc\\^2", names(b))[1]
    g1 <- unname(b[i1]); g2 <- unname(b[i2])
    V11 <- V[i1, i1]; V22 <- V[i2, i2]; V12 <- V[i1, i2]
    sex <- if (s == 1) "women" else "men"
    list(
      prof = data.frame(sex = sex, ebar = eb, e = c(5, 10, 15, 20, 25, 30)) %>%
        mutate(d = e - ebar,
               est = 100 * (g1 + 2 * g2 * d),
               se  = 100 * sqrt(pmax(0, V11 + 4 * d^2 * V22 + 4 * d * V12)),
               n = fixest::fitstat(m, "n")$n),
      coef = data.frame(
        sex = sex, ebar = eb, educ_bar = db,
        g1 = g1, g2 = g2,
        se_g1 = sqrt(V11), se_g2 = sqrt(V22), cov_g1g2 = V12,
        slope_per_year = 100 * 2 * g2,
        n = fixest::fitstat(m, "n")$n,
        controls = paste(.ctrl, collapse = "+"),
        stringsAsFactors = FALSE))
  }
  .fits <- lapply(c(0, 1), .one)
  .prof <- do.call(rbind, lapply(.fits, function(z) z$prof))
  .co   <- do.call(rbind, lapply(.fits, function(z) z$coef))

  .d <- .prof %>% select(sex, e, est, se) %>%
    tidyr::pivot_wider(names_from = sex, values_from = c(est, se)) %>%
    mutate(diff = est_women - est_men,
           # DISJOINT PERSONS, so the variances add. Section 5.5's rule.
           se_diff = sqrt(se_women^2 + se_men^2),
           t = diff / se_diff,
           sig = ifelse(abs(t) >= 1.96, "*", ""))
  print(as.data.frame(.d), row.names = FALSE, digits = 3)
  cat("\n  Units are percentage points of log wage per year of education\n",
      "  per year of experience, matching the appendix profile table.\n", sep = "")
  cat("  A column of blanks under `sig` means Section 7.3 keeps its present\n")
  cat("  wording. Any signed early horizon lets it say women's return\n")
  cat("  exceeds men's there rather than only that nothing runs against it.\n")
  cat(sprintf("  Largest |t| across the six horizons: %.3f at e = %d.\n",
              max(abs(.d$t)), as.integer(.d$e[which.max(abs(.d$t))])))

  cat("\n  The two coefficients the six rows are read from:\n")
  print(as.data.frame(.co), row.names = FALSE, digits = 4)
  cat("  slope_per_year is the change in the printed cross-partial per year\n")
  cat("  of experience. It is constant, which is the point: the table is a\n")
  cat("  line evaluated at six places and should be presented as one.\n")

  .out(.d,  "referee_A_crosspartial_se.csv")
  .out(.co, "referee_A_crosspartial_coefs.csv")
  rm(.w, .want, .ctrl, .alt, .missing_ctrl, .one, .fits, .prof, .co, .d)
}

# =====================================================================
# B. THE HEADLINE ON VALIDATED BIRTHS                       concern 5
#
# THE REFEREE'S 22.3 PER CENT IS RIGHT, and revision 1's comment here was
# wrong to call the manuscript figure superseded. The two numbers are one
# quantity on two denominators. Pre_analysis_diagnostics2_v7.R ITEM 12
# computes the match rate over the 2,388 transitions where the life-event
# item is OBSERVED and gets 88.0 per cent same-wave, so 12 unmatched. The
# manuscript computes it over all 2,704 transitions, counting an
# unobserved item as unmatched, and gets 77.7, so 22.3 unmatched. Both
# gaps are 10.3 points, which is one denominator change and not two
# computations. The unconditional figure is the conservative one and the
# right one for a claim about all transitions: a transition whose item is
# missing is not a verified birth.
#
# The sensitivity analysis is worth running on either reading. Twelve per
# cent is not nothing and twenty-two is less, and household-composition
# changes are exactly the kind of event that moves employment and
# training too.
#
# IT IS A MEASUREMENT CHECK AND NOT THE PREFERRED ESTIMAND, which is the
# paper's own position and remains right: restricting to matched
# transitions conditions treatment on a second, separately measured item,
# so the restricted sample is defined by agreement between two variables
# rather than by the event. Reporting it beside the full sample costs
# nothing and declining to report it looks like avoidance.
#
# THE SHARE IS NOW PRINTED. Revision 1 reported three regressions and
# left the validated share to be reconstructed from person counts, which
# is how a reader ends up solving three simultaneous equations to recover
# a percentage the block already knew.
# =====================================================================
.hdr("B. Validated births beside all transitions        [concern 5]")

.bcol <- tryCatch(
  if (exists("event_col", mode = "function"))
    event_col("had_birth_this_wave", site = "REFEREE_RUN.R", want = "birth")
  else NA_character_, error = function(e) NA_character_)
.need_b <- c("person_id", "wave", "female", "ever_parent", "employed")
.ycol <- intersect(c("jttpewt_any", "jttpewt_obs"), names(.panel))[1]

if (!.have_fixest) .skip(5, "fixest unavailable") else
if (is.na(.ycol) || !all(.need_b %in% names(.panel))) {
  .skip(5, paste("panel lacks:",
                 paste(c(setdiff(.need_b, names(.panel)),
                         if (is.na(.ycol)) "a jttpewt outcome"), collapse = ", ")))
} else if (is.na(.bcol) || !.bcol %in% names(.panel)) {
  .skip(5, "the birth event column did not resolve via event_col()")
} else {
  .b <- .panel %>% arrange(person_id, wave) %>% group_by(person_id) %>%
    mutate(.bw = { dd <- c(NA, diff(ever_parent))
                   i <- which(!is.na(dd) & dd > 0)
                   if (length(i)) wave[i[1]] else NA_real_ }) %>%
    ungroup() %>%
    mutate(post = as.integer(!is.na(.bw) & wave >= .bw))

  # A transition is validated when the birth item fires in the transition
  # wave itself. The wave before is deliberately NOT accepted: on the birth
  # item widening buys 0.4 points, and on the pregnancy item it buys 16,
  # which is the contrast footnote 5 rests on.
  .hit <- .panel %>% select(person_id, wave, .bi = dplyr::all_of(.bcol)) %>%
    filter(!is.na(.bi), .bi == 1) %>% mutate(.key = paste0(person_id, "_", wave))
  # Whether the item was OBSERVED at the transition wave, which is the
  # denominator the diagnostics script uses and this one did not report.
  .seen <- .panel %>% select(person_id, wave, .bi = dplyr::all_of(.bcol)) %>%
    filter(!is.na(.bi)) %>% mutate(.key = paste0(person_id, "_", wave))
  .b <- .b %>% mutate(
    validated = !is.na(.bw) & paste0(person_id, "_", .bw) %in% .hit$.key,
    item_seen = !is.na(.bw) & paste0(person_id, "_", .bw) %in% .seen$.key)

  # ---- the share, on both denominators, before any regression ---------
  .tr <- .b %>% filter(!is.na(.bw)) %>%
    group_by(person_id) %>%
    summarise(validated = dplyr::first(validated),
              item_seen = dplyr::first(item_seen), .groups = "drop")
  .share <- data.frame(
    denominator = c("all transitions", "transitions with the item observed"),
    n_transitions = c(nrow(.tr), sum(.tr$item_seen)),
    n_validated = c(sum(.tr$validated), sum(.tr$validated & .tr$item_seen)),
    stringsAsFactors = FALSE)
  .share$pct_validated <- round(100 * .share$n_validated /
                                  pmax(1, .share$n_transitions), 2)
  .share$pct_unmatched <- round(100 - .share$pct_validated, 2)
  cat("  Validation share on this frame, both denominators:\n")
  print(.share, row.names = FALSE)
  cat("  The first row is the manuscript's convention and the referee's\n")
  cat("  22.3 per cent; the second is the diagnostics script's 88.0. This\n")
  cat("  frame is employed-gated and uses its own cohort rule, so neither\n")
  cat("  row need equal ITEM 12 exactly. A large divergence on either row\n")
  cat("  is a frame problem worth chasing; a point or two is not.\n")
  .out(.share, "referee_B_validation_share.csv")

  .fit <- function(dd, lab) {
    m <- fixest::feols(
      stats::as.formula(paste0(.ycol, " ~ post + post:female | person_id + female^wave")),
      data = dd, cluster = ~person_id)
    b <- stats::coef(m); v <- sqrt(diag(stats::vcov(m)))
    i <- grep("post:female", names(b))[1]
    data.frame(sample = lab, est = unname(b[i]), se = unname(v[i]),
               n = fixest::fitstat(m, "n")$n,
               persons = dplyr::n_distinct(dd$person_id),
               # the treated count, so a row can be told from another row
               # fitted on a different sample at a glance
               treated = if (".bw" %in% names(dd))
                 dplyr::n_distinct(dd$person_id[!is.na(dd$.bw)]) else NA_integer_,
               stringsAsFactors = FALSE)
  }
  .e <- .b %>% filter(employed == 1, !is.na(.data[[.ycol]]))
  .res <- rbind(
    .fit(.e, "all transitions (reference)"),
    .fit(.e %>% filter(is.na(.bw) | validated), "validated transitions only"),
    .fit(.e %>% filter(is.na(.bw) | !validated), "unmatched transitions only"))
  .res$t <- .res$est / .res$se
  .res$pct_of_reference <- round(100 * .res$est / .res$est[1], 1)
  cat("\n")
  print(.res, row.names = FALSE, digits = 4)
  cat("\n  The third row is the diagnostic, not a result. If the unmatched\n",
      "  transitions carry a similar estimate, misclassification attenuates\n",
      "  and does not drive; if they carry none of it, the reference is a\n",
      "  mixture and the validated row is the one to quote.\n", sep = "")
  cat("  The reference here is this block's own frame and is not the\n")
  cat("  paper's headline arm. Quote the ratio, not the level, or say\n")
  cat("  which reference the level belongs to.\n")
  .res$frame <- "panel, employed-gated"

  # ---- the same three arms on the event frame -------------------------
  # The validated flag is a property of a person and a birth wave, so it
  # carries over from .hit unchanged; only the sample changes.
  .evb <- NULL
  if (.ev_ok(c("person_id", "wave", "female", "employed", .ycol))) {
    .eb <- .evframe
    .eb$.bw <- .bw_col(.eb)
    if (all(is.na(.eb$.bw))) {
      cat("\n  The event frame carries no cohort, ever_parent or k column, so\n")
      cat("  the event-sample arms are not fitted.\n")
    } else {
      .eb$post <- as.integer(!is.na(.eb$.bw) & .eb$wave >= .eb$.bw)
      .eb$validated <- !is.na(.eb$.bw) &
        paste0(.eb$person_id, "_", .eb$.bw) %in% .hit$.key
      .ee <- .eb[.eb$employed == 1 & !is.na(.eb[[.ycol]]), , drop = FALSE]
      .evb <- tryCatch(rbind(
        .fit(.ee, "all transitions (reference)"),
        .fit(.ee[is.na(.ee$.bw) | .ee$validated, , drop = FALSE],
             "validated transitions only"),
        .fit(.ee[is.na(.ee$.bw) | !.ee$validated, , drop = FALSE],
             "unmatched transitions only")), error = function(e) NULL)
      if (is.null(.evb)) {
        cat("\n  The event-sample arms did not fit; the frame is probably\n")
        cat("  missing a fixed effect the specification needs.\n")
      } else {
        .evb$t <- .evb$est / .evb$se
        .evb$pct_of_reference <- round(100 * .evb$est / .evb$est[1], 1)
        .evb$frame <- "event sample"
        cat("\n  The same three arms on the event frame:\n")
        print(.evb, row.names = FALSE, digits = 4)
        cat("\n  QUOTE A LEVEL FROM THESE ROWS, NOT THE ONES ABOVE. The\n")
        cat("  reference here is the sample the headline is estimated on, so\n")
        cat("  its level sits beside the headline and the wide-frame rows do\n")
        cat("  not. If the two references differ by more than rounding, that\n")
        cat("  difference is the screen, and the prose has to name which\n")
        cat("  sample its number belongs to.\n")
      }
    }
  } else if (!is.null(.evframe)) {
    cat("\n  The event frame resolved but lacks one of person_id, wave,\n")
    cat("  female, employed or the outcome, so only the wide frame is fitted.\n")
  }
  .res <- rbind(.res, .evb)
  .out(.res, "referee_B_validated_births.csv")
  rm(.b, .hit, .seen, .tr, .share, .fit, .e, .res)
  rm(list = intersect(c(".evb", ".eb", ".ee"), ls(all.names = TRUE)))
}

# =====================================================================
# C. COHORT COMPOSITION OF THE TWO ARMS                     concern 8
#
# The arms are fitted separately by sex and differenced, so if the two
# hold different distributions of birth waves, part of the sex difference
# is an aggregation difference rather than a treatment difference. The
# referee is right that the paper never checks this. It is a table, not an
# estimator: the birth-wave shares by sex, and the effect-weight each
# cohort receives.
# =====================================================================
.hdr("C. Cohort composition, women against men            [concern 8]")

if (!.have_tidyr) .skip(8, "tidyr unavailable") else
if (!all(c("person_id", "wave", "female", "ever_parent") %in% names(.panel))) {
  .skip(8, "panel lacks person_id, wave, female or ever_parent")
} else {
  .tr <- .panel %>% arrange(person_id, wave) %>% group_by(person_id) %>%
    summarise(female = dplyr::first(female),
              bw = { dd <- c(NA, diff(ever_parent))
                     i <- which(!is.na(dd) & dd > 0)
                     if (length(i)) wave[i[1]] else NA_real_ },
              .groups = "drop") %>% filter(!is.na(bw))
  .cw <- .tr %>% count(female, bw) %>% group_by(female) %>%
    mutate(share = 100 * n / sum(n)) %>% ungroup() %>%
    tidyr::pivot_wider(names_from = female, values_from = c(n, share),
                       names_prefix = "sex") %>% arrange(bw)
  print(as.data.frame(.cw), row.names = FALSE, digits = 3)
  .ks <- suppressWarnings(stats::ks.test(.tr$bw[.tr$female == 1],
                                         .tr$bw[.tr$female == 0]))
  cat(sprintf("\n  Kolmogorov-Smirnov on the two birth-wave distributions: D = %.4f, p = %.3f\n",
              unname(.ks$statistic), .ks$p.value))
  cat("  A small D means the two arms aggregate over nearly the same cohorts\n")
  cat("  and the difference is a treatment difference. A large one means the\n")
  cat("  paper has to say which cohorts each arm is weighted toward.\n")
  .out(.cw, "referee_C_cohort_weights.csv")
  rm(.tr, .cw, .ks)
}

# =====================================================================
# D. STATIC AGAINST COHORT-ROBUST                           concern 8
#
# Section 7.2 already reports that the two agree at the birth wave to four
# decimals. The referee's point is not that they disagree but that the
# static one carries the headline, and agreement at one horizon is not a
# reason to lead with the estimator that is vulnerable rather than the one
# that is not.
#
# REVISION 2 COMPUTES THE AGREEMENT. Revision 1 printed two hard-coded
# numbers and a recommendation, which is a table asserting its own result.
# The two estimates already exist as artefacts, so the block reads them:
# the static coefficient from the unsealed blinded estimates, and the
# cohort-robust one as the women-minus-men difference at k = 0 from the
# event-study object. If either file is absent the block says so and falls
# back to the typed statement, marked as typed.
# =====================================================================
.hdr("D. Which estimator carries the headline             [concern 8]")

.f_blind <- .find_art("BLIND_training_estimates.rds")
.f_es    <- .find_art("training_event_study_results.rds")
.static  <- NA_real_; .sa <- NA_real_

if (!is.na(.f_blind)) {
  .bl <- tryCatch(readRDS(.f_blind), error = function(e) NULL)
  if (!is.null(.bl) && "T1_jttpewt_any_employed" %in% names(.bl)) {
    .static <- as.numeric(.bl[["T1_jttpewt_any_employed"]])[1]
    cat("  static, T1_jttpewt_any_employed, from ", .f_blind, "\n", sep = "")
  }
  rm(.bl)
}
if (!is.na(.f_es)) {
  .es <- tryCatch(readRDS(.f_es), error = function(e) NULL)
  .kw <- "employed_only women jttpewt_any"; .km <- "employed_only men jttpewt_any"
  if (!is.null(.es) && all(c(.kw, .km) %in% names(.es))) {
    .g0 <- function(x) {
      x <- as.data.frame(x)
      x$att[which(abs(x$event_time - 0) < 1e-8)][1]
    }
    .sa <- .g0(.es[[.kw]]) - .g0(.es[[.km]])
    cat("  cohort-robust, women minus men at k = 0, from ", .f_es, "\n", sep = "")
    rm(.g0)
  }
  rm(.es, .kw, .km)
}

if (is.na(.static) || is.na(.sa)) {
  cat("  ONE OR BOTH ARTEFACTS NOT FOUND. The statement below is TYPED and\n")
  cat("  has not been recomputed in this session.\n")
  cat("    static -0.1163, cohort-robust -0.1163 at the birth wave.\n")
} else {
  .cmp <- data.frame(
    estimator = c("static two-way fixed effects", "Sun-Abraham at k = 0"),
    estimate  = c(.static, .sa), stringsAsFactors = FALSE)
  print(.cmp, row.names = FALSE, digits = 6)
  cat(sprintf("\n  absolute difference: %.6f\n", abs(.static - .sa)))
  cat(sprintf("  they agree to %d decimal places\n",
              as.integer(max(0, floor(-log10(max(abs(.static - .sa), 1e-12)))))))
  .out(.cmp, "referee_D_estimator_comparison.csv")
  rm(.cmp)
}
cat("\n  The recommendation stands on its own: lead with the cohort-robust\n")
cat("  estimate and report the static one as a summary check. Agreement at\n")
cat("  k = 0 is a reason the swap is cheap, not a reason to skip it.\n")
rm(.f_blind, .f_es, .static, .sa)

# =====================================================================
# E. IS THERE ANY EMPLOYER OR JOB IDENTIFIER                concern 3
#
# The sorting paragraph should say what cannot be done and why, and at the
# moment it says nothing. If a job-spell marker exists, a stable-employer
# subsample is the one exercise that speaks to sorting directly. If none
# does, the paper should say that HILDA carries no employer identifier
# rather than leaving a reader to wonder whether it was tried.
#
# WATCH THE ZERO-COVERAGE ROWS. firm_size_comparable came back with no
# non-missing values at all, which is not a small-coverage problem but an
# empty column: it was built to bridge the wave 21 to 22 firm-size break,
# where the missing share falls from 31.5 to 0.9 per cent and the
# small-firm share rises from 3.1 to 20.9, and it was never populated.
# That is why this panel has no firm-size measure comparable across the
# break, and it belongs in the sorting paragraph as a stated limit.
# =====================================================================
.hdr("E. Employer and job identifiers in this panel        [concern 3]")

.pat <- "employer|jbmo|job_?spell|job_?change|tenure|jbempt|same_?job|firm"
.cand <- grep(.pat, names(.panel), value = TRUE, ignore.case = TRUE)
if (!length(.cand)) {
  cat("  No column matches ", .pat, ".\n", sep = "")
  cat("  Say plainly in the sorting paragraph that the panel carries no\n")
  cat("  employer identifier, so a stable-employer subsample is unavailable.\n")
} else {
  .tab <- do.call(rbind, lapply(.cand, function(v) {
    x <- .panel[[v]]
    data.frame(column = v, class = class(x)[1],
               non_missing = sum(!is.na(x)),
               distinct = dplyr::n_distinct(x[!is.na(x)]),
               stringsAsFactors = FALSE)
  }))
  .tab <- .tab[order(-.tab$non_missing), ]
  print(.tab, row.names = FALSE)
  .empty <- .tab$column[.tab$non_missing == 0]
  if (length(.empty))
    cat("\n  EMPTY COLUMNS, zero non-missing values: ",
        paste(.empty, collapse = ", "),
        ".\n  These are declared and never filled. Anything the paper says a\n",
        "  column like this would have supported has to be said as a limit.\n",
        sep = "")
  cat("\n  A column with many distinct values and wide coverage would support\n")
  cat("  a stable-employer subsample. A tenure or job-change FLAG supports a\n")
  cat("  weaker version: restrict to person-waves with no job change since\n")
  cat("  the wave before the birth, which holds the employer fixed without\n")
  cat("  identifying it. Either is more than the paper currently offers.\n")
  .out(.tab, "referee_E_job_identifiers.csv")
  rm(.tab, .empty)
}
rm(.pat, .cand)

# =====================================================================
# F. THE STABLE-EMPLOYER TEST                               concern 3
# v-jobchange, 22 October 2026. event_job_change is coded 1 = no, 2 = yes, as the
# loader stores it. The flag below summed the raw value, so every answered
# wave after the birth counted as a job change and the arm kept only waves
# with the item missing. Only a 2 counts now; a missing answer still counts
# as no change.
#
# BLOCK E SAID THIS WAS POSSIBLE, so it is run rather than described.
# The probe found event_job_change on 114,695 person-waves and
# tenure_employer on 107,046, which is wide coverage on both. Neither
# identifies a firm, so no employer fixed effect is available and the
# sorting concern cannot be answered in the form the referee frames it.
# What they do support is the weaker test that answers the same worry: if
# the decline is mothers moving to employers who train less, it should be
# smaller among mothers who did not move.
#
# TWO RULES, because they fail differently. The job-change flag is an
# event and can be cumulated from the wave before the birth, which is what
# "did not move" means; tenure rising by roughly one per wave says the
# same thing from the other side and catches a move the flag misses. They
# are reported together and a disagreement between them is informative
# about the flag rather than about mothers.
#
# THIS IS A POST-HOC ARM. It was designed after the referee report and
# after the estimates were seen, so it carries no pre-analysis status and
# must be marked post-hoc at its own site, as Section 5.4 requires of the
# pre-birth full-time arm.
#
# QUOTE THE RATIO, NOT THE LEVEL. The reference row is this block's frame
# and not the paper's headline arm, so a percentage of reference travels
# and a coefficient does not.
# =====================================================================
.hdr("F. Restricting to women who did not change employer   [concern 3]")

.jc <- intersect(c("event_job_change"), names(.panel))[1]
.tn <- intersect(c("tenure_employer"), names(.panel))[1]
if (!.have_fixest) .skip(3, "fixest unavailable") else
if (is.na(.jc) && is.na(.tn)) .skip(3, "neither event_job_change nor tenure_employer") else
if (!exists(".ycol") || is.na(.ycol)) .skip(3, "no jttpewt outcome resolved") else {
  .g <- .panel %>% arrange(person_id, wave) %>% group_by(person_id) %>%
    mutate(.bw = { dd <- c(NA, diff(ever_parent))
                   i <- which(!is.na(dd) & dd > 0)
                   if (length(i)) wave[i[1]] else NA_real_ },
           post = as.integer(!is.na(.bw) & wave >= .bw),
           # moves since the wave BEFORE the birth, so the baseline job is
           # the one held going in
           .moved_flag = if (!is.na(.jc))
             cumsum(as.integer(.data[[.jc]] %in% 2) *
                    as.integer(!is.na(.bw) & wave >= .bw)) else NA_real_,
           .tn_drop = if (!is.na(.tn))
             cumsum(as.integer(!is.na(dplyr::lag(.data[[.tn]])) &
                               .data[[.tn]] < dplyr::lag(.data[[.tn]]) &
                               !is.na(.bw) & wave >= .bw)) else NA_real_) %>%
    ungroup() %>% filter(employed == 1, !is.na(.data[[.ycol]]))

  .fit2 <- function(dd, lab) {
    .tr_n <- if (".bw" %in% names(dd))
      dplyr::n_distinct(dd$person_id[!is.na(dd$.bw)]) else NA_integer_
    if (nrow(dd) < 500 || dplyr::n_distinct(dd$person_id) < 200)
      return(data.frame(sample = lab, est = NA, se = NA, n = nrow(dd),
                        persons = dplyr::n_distinct(dd$person_id),
                        treated = .tr_n,
                        stringsAsFactors = FALSE))
    m <- fixest::feols(
      stats::as.formula(paste0(.ycol, " ~ post + post:female | person_id + female^wave")),
      data = dd, cluster = ~person_id)
    b <- stats::coef(m); v <- sqrt(diag(stats::vcov(m)))
    i <- grep("post:female", names(b))[1]
    data.frame(sample = lab, est = unname(b[i]), se = unname(v[i]),
               n = fixest::fitstat(m, "n")$n,
               persons = dplyr::n_distinct(dd$person_id),
               treated = .tr_n, stringsAsFactors = FALSE)
  }
  .rows <- list(.fit2(.g, "all employed (reference)"))
  if (!is.na(.jc))
    .rows <- c(.rows, list(.fit2(filter(.g, .moved_flag == 0),
                                 "no job change since the birth")))
  if (!is.na(.tn))
    .rows <- c(.rows, list(.fit2(filter(.g, .tn_drop == 0),
                                 "employer tenure never resets")))
  .r <- do.call(rbind, .rows); .r$t <- .r$est / .r$se
  .r$pct_of_reference <- round(100 * .r$est / .r$est[1], 1)
  print(.r, row.names = FALSE, digits = 4)
  cat("\n  READ IT AS A BOUND ON SORTING, not as its elimination. Holding the\n",
      "  job fixed removes movers, who are the channel sorting would work\n",
      "  through, so a gap that survives at close to its reference magnitude\n",
      "  is one that operates within continuing employment relationships. A\n",
      "  gap that collapses says the opposite. Neither rules out a firm that\n",
      "  trains less for the same worker without her moving, which is what an\n",
      "  employer identifier would be needed for and this panel has none.\n",
      "  The restriction is also selective: staying is an outcome of the\n",
      "  birth, so this conditions on a post-treatment variable and cannot\n",
      "  carry a causal reading on its own.\n", sep = "")
  .r$frame <- "panel, employed-gated"

  # ---- the same arms on the event frame -------------------------------
  # Section 8.4 quotes this block's reference as "-0.124 on the employed
  # frame" and then compares the tenure arm to it. Both rows come from
  # this frame, so the comparison is internally sound, but the level is
  # not the headline's and the percentage of reference is the only figure
  # that travels between them. These rows give the paper a level it can
  # quote beside the headline instead.
  .rev <- NULL
  .jc_ev <- !is.na(.jc) && .ev_ok(.jc)
  .tn_ev <- !is.na(.tn) && .ev_ok(.tn)
  if (.ev_ok(c("person_id", "wave", "female", "employed", .ycol)) &&
      (.jc_ev || .tn_ev)) {
    .gev <- .evframe
    .gev$.bw <- .bw_col(.gev)
    .gev <- .gev %>% arrange(person_id, wave) %>% group_by(person_id) %>%
      mutate(post = as.integer(!is.na(.bw) & wave >= .bw),
             .moved_flag = if (.jc_ev)
               cumsum(as.integer(.data[[.jc]] %in% 2) *
                      as.integer(!is.na(.bw) & wave >= .bw)) else NA_real_,
             .tn_drop = if (.tn_ev)
               cumsum(as.integer(!is.na(dplyr::lag(.data[[.tn]])) &
                                 .data[[.tn]] < dplyr::lag(.data[[.tn]]) &
                                 !is.na(.bw) & wave >= .bw)) else NA_real_) %>%
      ungroup() %>% filter(employed == 1, !is.na(.data[[.ycol]]))
    .rows_e <- list(.fit2(.gev, "all employed (reference)"))
    if (.jc_ev)
      .rows_e <- c(.rows_e, list(.fit2(filter(.gev, .moved_flag == 0),
                                       "no job change since the birth")))
    if (.tn_ev)
      .rows_e <- c(.rows_e, list(.fit2(filter(.gev, .tn_drop == 0),
                                       "employer tenure never resets")))
    .rev <- tryCatch({
      x <- do.call(rbind, .rows_e); x$t <- x$est / x$se
      x$pct_of_reference <- round(100 * x$est / x$est[1], 1)
      x$frame <- "event sample"; x }, error = function(e) NULL)
    if (is.null(.rev)) {
      cat("\n  The event-sample arms did not fit.\n")
    } else {
      cat("\n  The same arms on the event frame:\n")
      print(.rev, row.names = FALSE, digits = 4)
      cat("  The percentage of reference is the figure the paper should\n")
      cat("  quote, and at one decimal it settles whether the tenure arm is\n")
      cat("  95 or 96 per cent of its reference, which rounding the levels\n")
      cat("  to three decimals does not.\n")
    }
  } else if (!is.null(.evframe)) {
    cat("\n  The event frame lacks the job-change and tenure columns, so the\n")
    cat("  restriction cannot be repeated on it. The rows above are this\n")
    cat("  block's own frame and only their ratio belongs in the paper.\n")
  }
  .r <- rbind(.r, .rev)
  .out(.r, "referee_F_stable_employer.csv")
  rm(.g, .fit2, .rows, .r)
  rm(list = intersect(c(".rev", ".gev", ".rows_e", ".jc_ev", ".tn_ev"),
                      ls(all.names = TRUE)))
}
rm(.jc, .tn)

# =====================================================================
# G. THE FEE DECOMPOSITION, AGAINST ITS OWN BASE
#
# NOT A REFEREE CONCERN. This one comes out of the terminology decision:
# under a two-rung scheme the interpretive claim, that what moves is an
# investment the employer bears, is earned in exactly two places, and this
# is one of them. A block that carried a share of the argument can carry a
# larger share once "employer-financed" stops being a label, so it has to
# be able to.
#
# WHAT IT IS MISSING IS A DENOMINATOR. Section 6.1 reports each state's
# share of the birth-wave FALL and no state's share of the pre-birth BASE.
# "Concentrated" is a claim about one against the other: a state holding
# 72.6 per cent of the fall and 72.6 per cent of the base is declining
# exactly proportionately and is not concentrated at all.
#
# AND THE SECOND CONCLUSION MAY BE BACKWARDS. Section 6.1 says there is
# no shift toward cost-sharing, because the fee-bearing cells fall
# alongside the no-fee cells rather than rising. Under an unconditional
# construction every state is bounded above by training incidence, which
# falls by eleven points, so all of them falling together is close to
# mechanical and rules out only the pure form of cost-shifting in which
# total training is held constant. A PARTIAL shift shows up as the
# fee-bearing state falling by less than its share of the base.
#
# The concentration index below is the share of the fall over the share
# of the base. Above one is concentration, one is proportionate decline,
# below one is a state the withdrawal spared.
#
# ---------------------------------------------------------------------
# THE FRAME, said once. Rows are employed person-waves with both training
# items observed, and the SELF-EMPLOYED ARE RETAINED, which matches
# 03_common.R and the headline arm and differs from
# training_funding_divergence.R, whose make_arm() drops them. That is a
# contract difference and not an error, but it is the reason the base
# here will not equal the base there, and it should be stated wherever
# these shares are quoted. The pre-birth window is k in [-3, -1] on
# women, which is 03_common.R's PRE_LO to PRE_HI.
# =====================================================================
.hdr("G. Fee states: share of the fall against share of the base")

.fee <- intersect(c("training_self_funded_z", "training_self_funded"),
                  names(.panel))[1]
.rel <- intersect(c("jttpewt_any", "jttpewt_obs"), names(.panel))[1]
.own <- intersect(c("jttpeot_any", "jttpeot_obs"), names(.panel))[1]

if (!.have_fixest) .skip("terminology", "fixest unavailable") else
if (any(is.na(c(.fee, .rel, .own)))) {
  .skip("terminology",
        paste("panel lacks:",
              paste(c("a fee item", "a released-time item", "an own-time item")[
                is.na(c(.fee, .rel, .own))], collapse = ", ")))
} else {
  if (.fee == "training_self_funded")
    cat("  *** using the UNGATED fee item. It is observed only among\n",
        "      trainees, and trainee status moves at the birth, so the\n",
        "      states below condition on the outcome. Rerun on the _z twin.\n",
        sep = "")

  .s <- .panel %>% arrange(person_id, wave) %>% group_by(person_id) %>%
    mutate(.bw = { dd <- c(NA, diff(ever_parent))
                   i <- which(!is.na(dd) & dd > 0)
                   if (length(i)) wave[i[1]] else NA_real_ },
           post = as.integer(!is.na(.bw) & wave >= .bw)) %>%
    ungroup() %>%
    filter(employed == 1, !is.na(.data[[.rel]]), !is.na(.data[[.own]])) %>%
    mutate(.r = .data[[.rel]] == 1, .o = .data[[.own]] == 1,
           .f = dplyr::coalesce(.data[[.fee]], 0) == 1)

  if ("is_self_employed" %in% names(.s))
    cat(sprintf("  frame retains %s self-employed person-waves, by contract.\n",
                .fmt(sum(!is.na(.s$is_self_employed) &
                         .s$is_self_employed == 1))))

  # Six states, mutually exclusive and jointly exhaustive over the trained.
  .st <- list(
    released_nofee = quote( .r & !.o & !.f),
    released_fee   = quote( .r & !.o &  .f),
    both_nofee     = quote( .r &  .o & !.f),
    both_fee       = quote( .r &  .o &  .f),
    owntime_nofee  = quote(!.r &  .o & !.f),
    owntime_fee    = quote(!.r &  .o &  .f))

  # -------------------------------------------------------------------
  # THE PRIMARY STATISTIC IS A SHARE SHIFT, NOT THE RATIO INDEX.
  #
  # Both carry the same information. Each state's share of workplace
  # training before the birth against its share after it, differenced, is
  # zero when the state declines in proportion, negative where the
  # withdrawal concentrated, positive where it was spared. The index says
  # the same thing as a ratio.
  #
  # The difference is what survives a bootstrap. The index divides a
  # state's share of the FALL by its share of the base, and the share of
  # the fall divides by the sum of four estimates. In a resample where
  # that sum wanders toward zero the index explodes. The share shift
  # divides by a sum of LEVELS, which is bounded away from zero by
  # construction, so it is stable in exactly the replicates where the
  # index is not. Both are reported; the shift is the one to quote.
  # -------------------------------------------------------------------
  .wk <- c("released_nofee", "released_fee", "both_nofee", "both_fee")

  .core <- function(dd, pid) {
    M <- cbind(as.matrix(dd[, paste0(".y", seq_along(.wk))]),
               post = dd$post, pf = dd$post * dd$female)
    fw <- paste0(dd$female, "_", dd$wave)
    Md <- if (exists("demean", where = asNamespace("fixest")))
      fixest::demean(M, f = list(factor(pid), factor(fw))) else {
        # alternating projections, for a fixest without demean()
        for (it in 1:20) for (f in list(factor(pid), factor(fw)))
          M <- M - rowsum(M, f)[as.integer(f), , drop = FALSE] /
            as.vector(tabulate(f))[as.integer(f)]
        M
      }
    Md <- as.matrix(Md)
    if (!all(c("post", "pf") %in% colnames(Md)))
      colnames(Md) <- c(paste0(".y", seq_along(.wk)), "post", "pf")
    X <- Md[, c("post", "pf"), drop = FALSE]
    B <- solve(crossprod(X), crossprod(X, Md[, seq_along(.wk), drop = FALSE]))
    list(est = as.numeric(B["pf", ]),
         base = colMeans(as.matrix(dd[dd$.pre, paste0(".y", seq_along(.wk)),
                                      drop = FALSE])))
  }

  # The statistics, from one (est, base) pair. Named so a percentile table
  # reads without a legend.
  .stat <- function(o) {
    pre  <- o$base / sum(o$base)
    post <- (o$base + o$est) / sum(o$base + o$est)
    conc <- (o$est / sum(o$est)) / pre
    fee  <- c(2, 4)
    c(stats::setNames(100 * (post - pre), paste0("dshare_", .wk)),
      stats::setNames(conc,               paste0("conc_",   .wk)),
      fee_share_shift = 100 * sum(post[fee]) - 100 * sum(pre[fee]),
      conc_gap = unname(conc[1] - conc[2]))
  }

  .d <- .s
  for (k in seq_along(.wk))
    .d[[paste0(".y", k)]] <- as.integer(eval(.st[[.wk[k]]], envir = .d))
  .d$.pre <- .d$female == 1 & !is.na(.d$.bw) &
    .d$wave >= .d$.bw - 3 & .d$wave < .d$.bw
  .d <- .d[stats::complete.cases(.d[, c(paste0(".y", seq_along(.wk)),
                                        "post", "female", "wave", "person_id")]), ]

  .o0 <- .core(.d, .d$person_id)
  .pt <- .stat(.o0)

  # ---- the state table, which revision 1 printed and did not write ----
  # The provenance row names two artefacts. This is the other one, and
  # without it the point estimates live only in a console transcript.
  .states <- data.frame(
    state = .wk,
    base_rate = as.numeric(.o0$base),
    share_of_base = 100 * as.numeric(.o0$base) / sum(.o0$base),
    est_post_f = as.numeric(.o0$est),
    share_of_fall = 100 * as.numeric(.o0$est) / sum(.o0$est),
    conc_index = as.numeric(.o0$est / sum(.o0$est)) /
      (as.numeric(.o0$base) / sum(.o0$base)),
    stringsAsFactors = FALSE)
  .states$dshare <- unname(.pt[paste0("dshare_", .wk)])
  cat("\n  States, base against fall:\n")
  print(.states, row.names = FALSE, digits = 4)
  cat("\n  point estimates:\n")
  print(round(.pt, 3))
  .out(.states, "referee_G_fee_states.csv")

  # -------------------------------------------------------------------
  # CLUSTER BOOTSTRAP ON PERSONS. The four estimates come off the same
  # rows and the four base rates off the same people, so nothing here is
  # independent of anything else and a delta method would need a
  # covariance this block does not have. Resampling persons and
  # recomputing all eight quantities carries every correlation for free.
  # A resampled person is given a fresh id, or the fixed effect would
  # pool their two copies and the within variation would be wrong.
  #
  # 1000 REPLICATES, NOT 400. The concentration verdict is decided by
  # whether one interval endpoint sits above or below one, and at 400 the
  # 2.5 per cent endpoint is the tenth order statistic. Spending four
  # minutes rather than two on the quantity the section turns on is the
  # cheapest precision in the file.
  # -------------------------------------------------------------------
  # v2. A SEEDED DRAW, BECAUSE THE PROSE QUOTES THE ENDPOINTS.
  #
  # Nothing here set a seed, so every run drew a new interval. The no-fee
  # released state's share loss came out at [-14.81, +0.51] on 21 September
  # and at [-14.42, +1.09] on 23 September, with the point estimate unmoved
  # at -6.48 because only the interval is simulated. Online Appendix
  # \ref{sec:oa_states} had quoted the first pair. The prose check caught
  # one of the four stale endpoints; the other three passed because their
  # digits exist in unrelated artefacts, which is not the same as being
  # right.
  #
  # THE SEED IS AN OUTPUT, NOT A SETTING. It is written into the interval
  # file beside boot_R, so the draw the manuscript quotes can be
  # reproduced rather than approximately re-drawn. Change BOOT_SEED only
  # with the prose in front of you.
  #
  # The RNG state is restored afterwards, so seeding here cannot reach any
  # other block.
  .B <- as.integer(get0("BOOT_R", ifnotfound = 1000L))
  if (.B <= 0) cat("\n  BOOT_R is 0, so no interval is computed.\n") else {
    .seed_g <- as.integer(get0("BOOT_SEED", ifnotfound = 20260923L))
    .rng_old <- if (exists(".Random.seed", envir = globalenv()))
      get(".Random.seed", envir = globalenv()) else NULL
    set.seed(.seed_g)
    cat(sprintf("\n  bootstrap seed %d, RNG %s\n", .seed_g,
                paste(RNGkind()[1:2], collapse = " / ")))
    .ids  <- unique(.d$person_id)
    .rowi <- split(seq_len(nrow(.d)), factor(.d$person_id, levels = .ids))
    .len  <- lengths(.rowi)
    cat(sprintf("\n  bootstrapping %d replicates over %s persons\n",
                .B, .fmt(length(.ids))))
    .t0 <- Sys.time()
    .bs <- t(vapply(seq_len(.B), function(bb) {
      p  <- sample(.ids, replace = TRUE)
      ri <- unlist(.rowi[match(p, .ids)], use.names = FALSE)
      pid <- rep(seq_along(p), .len[match(p, .ids)])
      tryCatch(.stat(.core(.d[ri, ], pid)),
               error = function(e) rep(NA_real_, length(.pt)))
    }, numeric(length(.pt))))
    .ok <- stats::complete.cases(.bs)
    # Hand the stream back exactly as it was found. This block runs at top
    # level, where on.exit() is inert, so the restore is explicit.
    if (is.null(.rng_old)) {
      if (exists(".Random.seed", envir = globalenv()))
        rm(".Random.seed", envir = globalenv())
    } else assign(".Random.seed", .rng_old, envir = globalenv())
    cat(sprintf("  %d of %d replicates usable, %.0f seconds\n", sum(.ok), .B,
                as.numeric(difftime(Sys.time(), .t0, units = "secs"))))

    # FOUR DECIMALS AND AN EXPLICIT FLAG. Two decimals turn 0.990 into
    # 1.00, and 1.00 is the value at which the concentration claim changes
    # from a direction into a finding. The flag is written so the reading
    # does not depend on how the number is later rounded.
    .lo <- apply(.bs, 2, stats::quantile, 0.025, na.rm = TRUE)
    .hi <- apply(.bs, 2, stats::quantile, 0.975, na.rm = TRUE)
    .ci <- data.frame(
      statistic = names(.pt), point = round(.pt, 4),
      lo = round(.lo, 4), hi = round(.hi, 4),
      excludes_zero = !(.lo <= 0 & .hi >= 0),
      excludes_one  = !(.lo <= 1 & .hi >= 1),
      boot_R = .B, n_usable = sum(.ok), boot_seed = .seed_g,
      stringsAsFactors = FALSE)
    print(.ci, row.names = FALSE)
    cat("  For a dshare or the fee shift read excludes_zero; for a conc\n")
    cat("  index read excludes_one. The other column is not meaningful.\n")
    cat(sprintf("  Endpoints are one draw of %d replicates at seed %d. Between\n",
                .B, .seed_g))
    cat("  the two unseeded draws this block made, the share endpoints moved\n")
    cat("  by 0.39 and 0.58 points, so their second decimal is simulation\n")
    cat("  noise. Quote them to one decimal, and quote an index endpoint to\n")
    cat("  three only because the verdict turns on its distance from one.\n")

    # ---- directional probabilities, now written as well as printed ----
    .p_ord <- mean(apply(.bs[, paste0("dshare_", .wk), drop = FALSE], 1,
                         function(r) !anyNA(r) && !is.unsorted(r)), na.rm = TRUE)
    .probs <- data.frame(
      statement = c("the no-fee released state loses share",
                    "the fee-bearing released state gains share",
                    "the fee-bearing share of workplace training rises",
                    "the four states preserve the point ordering"),
      P = c(mean(.bs[, "dshare_released_nofee"] < 0, na.rm = TRUE),
            mean(.bs[, "dshare_released_fee"]   > 0, na.rm = TRUE),
            mean(.bs[, "fee_share_shift"]       > 0, na.rm = TRUE),
            .p_ord),
      benchmark = c(0.5, 0.5, 0.5, 1/24),
      stringsAsFactors = FALSE)
    .probs$P <- round(.probs$P, 3)
    cat("\n  Directional probabilities, from the replicate distribution:\n")
    print(.probs, row.names = FALSE)

    # -----------------------------------------------------------------
    # THE VERDICTS, COMPUTED. Revision 1 closed with a paragraph
    # describing what a trial on simulated cells had produced, and that
    # paragraph was then read as if it described this run. It did not:
    # the trial's index excluded one and the real one does not. A block
    # that can derive its own reading should not leave it to prose.
    # -----------------------------------------------------------------
    .g1 <- function(s) .ci[.ci$statistic == s, , drop = FALSE][1, ]
    .v_conc  <- .g1("conc_released_nofee")
    .v_share <- .g1("dshare_released_nofee")
    .v_fee   <- .g1("fee_share_shift")

    cat("\n  ", strrep("-", 66), "\n", sep = "")
    cat("  VERDICTS, derived from the table above and not from expectation\n")
    cat("  ", strrep("-", 66), "\n", sep = "")

    cat(sprintf("  1. Concentration. Index %.3f, interval [%.3f, %.3f].\n",
                .v_conc$point, .v_conc$lo, .v_conc$hi))
    if (isTRUE(.v_conc$excludes_one) && .v_conc$lo > 1)
      cat("     The interval excludes one. The concentration finding can be\n",
          "     stated at full strength.\n", sep = "")
    else
      cat("     The interval INCLUDES one. Report concentration as a\n",
          "     direction, not a magnitude, and do not say the index\n",
          "     excludes proportionate decline.\n", sep = "")

    cat(sprintf("  2. Share loss. %.3f points, interval [%.3f, %.3f].\n",
                .v_share$point, .v_share$lo, .v_share$hi))
    if (isTRUE(.v_share$excludes_zero))
      cat("     The interval excludes zero. The no-fee released state loses\n",
          "     share, and that is a finding rather than a sign.\n", sep = "")
    else
      cat(sprintf(paste0("     The interval INCLUDES zero, so this is a direction",
                         " at P = %.3f\n     and not a significant fall. Do not",
                         " print an interval that\n     excludes zero.\n"),
                  .probs$P[1]))

    cat(sprintf("  3. Cost sharing. Fee share %+.3f points, interval [%.3f, %.3f].\n",
                .v_fee$point, .v_fee$lo, .v_fee$hi))
    if (isTRUE(.v_fee$excludes_zero))
      cat("     The interval excludes zero. The reversal is a magnitude.\n")
    else
      cat(sprintf(paste0("     The interval INCLUDES zero. Report the reversal",
                         " as a direction\n     at P = %.3f.\n"), .probs$P[3]))

    cat(sprintf("  4. Ordering. Preserved in %.1f per cent of replicates against\n",
                100 * .p_ord))
    cat("     4.2 for four random draws.\n")
    if (.p_ord >= 0.5)
      cat("     The ordering replicates in most resamples and can be stated.\n")
    else
      cat("     The ordering fails to replicate in most resamples. Section\n",
          "     6.1 keeps the two-state contrast and says the rest is not\n",
          "     resolved.\n", sep = "")

    .verdict <- data.frame(
      claim = c("concentration index excludes one",
                "no-fee released state's share loss excludes zero",
                "fee-share shift excludes zero",
                "four-state ordering replicates in most resamples"),
      holds = c(isTRUE(.v_conc$excludes_one) && .v_conc$lo > 1,
                isTRUE(.v_share$excludes_zero),
                isTRUE(.v_fee$excludes_zero),
                .p_ord >= 0.5),
      stringsAsFactors = FALSE)
    .out(.ci,      "referee_G_fee_states_boot.csv")
    .out(.probs,   "referee_G_fee_states_probs.csv")
    .out(.verdict, "referee_G_fee_states_verdict.csv")
    rm(.ids, .rowi, .len, .bs, .ok, .ci, .lo, .hi, .t0, .p_ord, .probs,
       .g1, .v_conc, .v_share, .v_fee, .verdict)
  }
  # ===================================================================
  # v-G-roster, 18 October. THE SAME DECOMPOSITION ON THE EVENT-STUDY
  # ROSTER, which is the arm Online Appendix B.6 quotes.
  #
  # WHY. The arm above runs on the analysis sample and takes `post` from
  # ever_parent turning on inside the panel. A person who is already a
  # parent at entry never turns on, so all of their waves carry post = 0,
  # and they sit among the controls that identify the sex-by-wave
  # effects. The paper's design keeps parents out of the control group
  # (Sections 3.2 and 4.1). On the roster the four released-time states
  # sum to the headline employed-only estimate, and the own-time states
  # sum to the own-time arm, so the split decomposes the published
  # numbers rather than a neighbouring sample. The wider arm stays as the
  # check B.6 mentions in one sentence.
  #
  # WHAT IS THE SAME. States, fixed effects (person, sex by wave), the
  # pre-birth window, the statistics, the seed and the replicate count.
  #
  # WHAT IS ADDED. Clustered standard errors for every state, the
  # own-time and other-place states, the direct estimates on jttpewt_any
  # and jttpeot_any, the frame counts, and the number of parents the wider
  # arm uses as controls. B.6 quoted all of these and the arm above
  # computed none of them.
  #
  # REGISTERED with paper_numbers.R when WRITE_REPORT is TRUE: prefix Gr
  # for the roster states, Grboot and Grprob for the bootstrap, Gw for the
  # wider arm and Gframe for the frame counts. Run paper_tables.R
  # afterwards so paper_numbers.tex picks them up.
  # ===================================================================
  .hdr("G2. The same fee states on the event-study roster")
  .gr_need <- c("person_id", "wave", "female", "employed", "post",
                .rel, .own, .fee)
  # v-G-roster-2, 18 October. G2 FINDS ITS OWN FRAME. The run of 18 October
  # 20:49 found an `ev` of 7,018 rows in the session, left by another
  # script, and skipped. The registry's `ev` is the roster 03 and 04 use,
  # so it is tried first, then the session's event frame, then
  # handoff/ev.rds. The first that carries every column G2 needs is used
  # and named; the reason each earlier one was passed over is kept.
  .gr_why <- character(0)
  .gr_src <- local({
    cands <- list()
    if (exists("td_has", mode = "function") && exists("td_get", mode = "function"))
      cands$registry <- tryCatch(if (isTRUE(td_has("ev"))) td_get("ev") else NULL,
                                 error = function(e) NULL)
    cands$session <- .evframe
    p <- .find_art(file.path("handoff", "ev.rds"))
    if (is.na(p)) p <- .find_art("ev.rds")
    if (!is.na(p)) cands$handoff <- tryCatch(readRDS(p), error = function(e) NULL)
    for (nm in names(cands)) {
      e <- cands[[nm]]
      if (!is.data.frame(e)) { .gr_why <<- c(.gr_why, paste(nm, "not available")); next }
      if (!"person_id" %in% names(e) && "id" %in% names(e)) e$person_id <- e$id
      miss <- setdiff(.gr_need, names(e))
      if (length(miss)) {
        .gr_why <<- c(.gr_why, sprintf("%s (%s rows) lacks %s", nm, .fmt(nrow(e)),
                                       paste(miss, collapse = ", ")))
        next
      }
      if (length(.gr_why)) cat("  passed over: ", paste(.gr_why, collapse = "; "), "\n", sep = "")
      cat(sprintf("  G2 frame: %s, %s rows, %s persons\n", nm, .fmt(nrow(e)),
                  .fmt(dplyr::n_distinct(e$person_id))))
      return(e)
    }
    NULL
  })
  if (is.null(.gr_src)) {
    cat("  SKIPPED. No event frame carries every column G2 needs: ",
        paste(.gr_why, collapse = "; "),
        "\n  B.6 cannot be quoted from this run.\n", sep = "")
  } else {
    .gr <- .gr_src
    .gr$.bw <- .bw_col(.gr)
    .gr <- .gr %>%
      filter(employed == 1, !is.na(.data[[.rel]]), !is.na(.data[[.own]])) %>%
      mutate(.r = .data[[.rel]] == 1, .o = .data[[.own]] == 1,
             .f = dplyr::coalesce(.data[[.fee]], 0) == 1,
             post = as.integer(post))
    # The frame's own post is the paper's. Say whether it agrees with the
    # birth wave this block reads, rather than assume it.
    .gr_post_bw <- as.integer(!is.na(.gr$.bw) & .gr$wave >= .gr$.bw)
    cat(sprintf("  post from the frame against post from the birth wave: %s of %s rows differ\n",
                .fmt(sum(.gr_post_bw != .gr$post, na.rm = TRUE)), .fmt(nrow(.gr))))

    # Training at another place only, from the pooled items, where the
    # frame carries them. Outside the paper's measure by construction, and
    # reported so its exclusion is visible.
    .gr_has_el <- all(c("training_paid_time", "training_own_time") %in% names(.gr))
    .gr$.e <- if (.gr_has_el)
      dplyr::coalesce(.gr$training_paid_time, 0) == 1 |
      dplyr::coalesce(.gr$training_own_time, 0) == 1 else FALSE
    .gr_st <- c(.st, if (.gr_has_el) list(
      elsewhere_nofee = quote(!.r & !.o & .e & !.f),
      elsewhere_fee   = quote(!.r & !.o & .e &  .f)))
    if (!.gr_has_el)
      cat("  training_paid_time or training_own_time is absent, so the other-place\n",
          "  states are not fitted.\n", sep = "")

    for (k in seq_along(.wk))
      .gr[[paste0(".y", k)]] <- as.integer(eval(.st[[.wk[k]]], envir = .gr))
    for (nm in names(.gr_st))
      .gr[[paste0("s_", nm)]] <- as.integer(eval(.gr_st[[nm]], envir = .gr))
    .gr$s_direct_jttpewt     <- as.integer(.gr$.r)
    .gr$s_owntime_arm_jttpeot <- as.integer(.gr$.o)
    .gr$.pre <- .gr$female == 1 & !is.na(.gr$.bw) &
      .gr$wave >= .gr$.bw - 3 & .gr$wave < .gr$.bw
    .gr <- .gr[stats::complete.cases(.gr[, c(paste0(".y", seq_along(.wk)),
                                             "post", "female", "wave", "person_id")]), ]
    .gr$pf <- .gr$post * .gr$female
    .gr$gr_fw <- paste0(.gr$female, "_", .gr$wave)

    .o0r <- .core(.gr, .gr$person_id)
    .ptr <- .stat(.o0r)

    # ---- every state with a clustered standard error ------------------
    # fixef.rm = "none" keeps every row, so these fits use exactly the rows
    # the bootstrap core uses. A state with no variation (a cell nobody is
    # in) returns NA instead of stopping the block.
    .gr_fit1 <- function(d, y) tryCatch({
      m <- fixest::feols(stats::as.formula(paste0(y, " ~ post + pf | person_id + gr_fw")),
                         data = d, cluster = ~person_id, fixef.rm = "none",
                         notes = FALSE, warn = FALSE)
      c(est = unname(stats::coef(m)[["pf"]]), se = unname(fixest::se(m)[["pf"]]),
        n = stats::nobs(m))
    }, error = function(e) {
      cat("  [skip] ", y, ": ", conditionMessage(e), "\n", sep = "")
      c(est = NA_real_, se = NA_real_, n = NA_real_)
    })
    .gr_ys <- c(names(.gr_st), "direct_jttpewt", "owntime_arm_jttpeot")
    .gr_fit <- do.call(rbind, lapply(.gr_ys, function(nm) {
      z <- .gr_fit1(.gr, paste0("s_", nm))
      data.frame(state = nm, est = z[["est"]], se = z[["se"]], n = z[["n"]],
                 base_rate = mean(.gr[[paste0("s_", nm)]][.gr$.pre]),
                 stringsAsFactors = FALSE)
    }))
    .gr_fit$ci_lo <- .gr_fit$est - stats::qnorm(0.975) * .gr_fit$se
    .gr_fit$ci_hi <- .gr_fit$est + stats::qnorm(0.975) * .gr_fit$se
    cat("\n  Every state, mothers minus fathers, clustered on the person:\n")
    print(transform(.gr_fit, excludes_zero = ci_lo > 0 | ci_hi < 0),
          row.names = FALSE, digits = 4)

    # The checks that license calling this a decomposition of the paper's
    # estimates. A fit on the same rows is linear, so the sums are exact.
    .gr_e <- stats::setNames(.gr_fit$est, .gr_fit$state)
    .gr_dev <- max(abs(.gr_e[.wk] - .o0r$est))
    cat(sprintf("\n  fitted states against the bootstrap core: largest difference %.2e%s\n",
                .gr_dev, if (.gr_dev > 1e-4) "  *** CHECK: the two fits are not on the same rows" else ""))
    cat(sprintf("  released-time states sum to %+.4f; jttpewt_any fitted directly gives %+.4f on %s rows.\n",
                sum(.gr_e[.wk]), .gr_e[["direct_jttpewt"]], .fmt(nrow(.gr))))
    cat(sprintf("  own-time states plus mixed states sum to %+.4f; jttpeot_any gives %+.4f.\n",
                sum(.gr_e[c("both_nofee", "both_fee", "owntime_nofee", "owntime_fee")]),
                .gr_e[["owntime_arm_jttpeot"]]))
    cat("  Compare the direct estimates with R1R4.all.kge0.E1 and .O1 (05) and with\n",
        "  T1_jttpewt_any_employed in the ledger. If they differ, this arm is on\n",
        "  different rows from the headline and B.6 must not call it a decomposition.\n",
        sep = "")

    # ---- base against fall, as the arm above writes it ---------------
    .states_r <- data.frame(
      state = .wk,
      base_rate = as.numeric(.o0r$base),
      share_of_base = 100 * as.numeric(.o0r$base) / sum(.o0r$base),
      est_post_f = as.numeric(.o0r$est),
      share_of_fall = 100 * as.numeric(.o0r$est) / sum(.o0r$est),
      conc_index = as.numeric(.o0r$est / sum(.o0r$est)) /
        (as.numeric(.o0r$base) / sum(.o0r$base)),
      stringsAsFactors = FALSE)
    .states_r$dshare <- unname(.ptr[paste0("dshare_", .wk)])
    cat("\n  Roster states, base against fall:\n")
    print(.states_r, row.names = FALSE, digits = 4)
    .out(.states_r, "referee_G_fee_states_roster.csv")
    .out(.gr_fit,   "referee_G_fee_states_roster_fit.csv")

    # ---- the frames, counted -----------------------------------------
    # Parents whose birth the wider arm never sees, so post stays 0 for
    # them there: the number this sentence of B.6 needs.
    .gw_par <- if ("ever_parent" %in% names(.d))
      dplyr::n_distinct(.d$person_id[is.na(.d$.bw) & .d$ever_parent %in% 1]) else NA_real_
    .d$pf <- .d$post * .d$female
    .d$gr_fw <- paste0(.d$female, "_", .d$wave)
    .d$s_direct_jttpewt <- as.integer(.d$.r)
    .gw_dir <- .gr_fit1(.d, "s_direct_jttpewt")
    .frames <- data.frame(
      frame = c("roster", "wide"),
      rows = c(nrow(.gr), nrow(.d)),
      persons = c(dplyr::n_distinct(.gr$person_id), dplyr::n_distinct(.d$person_id)),
      n_treated = c(dplyr::n_distinct(.gr$person_id[!is.na(.gr$.bw)]),
                    dplyr::n_distinct(.d$person_id[!is.na(.d$.bw)])),
      n_parent_controls = c(0, .gw_par),
      direct = c(.gr_e[["direct_jttpewt"]], .gw_dir[["est"]]),
      direct_se = c(.gr_fit$se[.gr_fit$state == "direct_jttpewt"], .gw_dir[["se"]]),
      four_state_sum = c(sum(.gr_e[.wk]), sum(.o0$est)),
      stringsAsFactors = FALSE)
    cat("\n  The two frames:\n")
    print(.frames, row.names = FALSE, digits = 4)
    cat("  n_parent_controls counts persons who are parents in the wider arm but\n",
        "  whose birth it never sees, so it treats all their waves as untreated.\n",
        sep = "")
    .out(.frames, "referee_G_fee_states_frames.csv")

    # ---- the bootstrap, as above, on the roster ----------------------
    if (.B > 0) {
      .seed_g <- as.integer(get0("BOOT_SEED", ifnotfound = 20260923L))
      .rng_old <- if (exists(".Random.seed", envir = globalenv()))
        get(".Random.seed", envir = globalenv()) else NULL
      set.seed(.seed_g)
      .ids  <- unique(.gr$person_id)
      .rowi <- split(seq_len(nrow(.gr)), factor(.gr$person_id, levels = .ids))
      .len  <- lengths(.rowi)
      cat(sprintf("\n  bootstrapping %d replicates over %s roster persons, seed %d\n",
                  .B, .fmt(length(.ids)), .seed_g))
      .t0 <- Sys.time()
      .bs <- t(vapply(seq_len(.B), function(bb) {
        p  <- sample(.ids, replace = TRUE)
        ri <- unlist(.rowi[match(p, .ids)], use.names = FALSE)
        pid <- rep(seq_along(p), .len[match(p, .ids)])
        tryCatch(.stat(.core(.gr[ri, ], pid)),
                 error = function(e) rep(NA_real_, length(.ptr)))
      }, numeric(length(.ptr))))
      .ok <- stats::complete.cases(.bs)
      if (is.null(.rng_old)) {
        if (exists(".Random.seed", envir = globalenv()))
          rm(".Random.seed", envir = globalenv())
      } else assign(".Random.seed", .rng_old, envir = globalenv())
      cat(sprintf("  %d of %d replicates usable, %.0f seconds\n", sum(.ok), .B,
                  as.numeric(difftime(Sys.time(), .t0, units = "secs"))))
      .lo <- apply(.bs, 2, stats::quantile, 0.025, na.rm = TRUE)
      .hi <- apply(.bs, 2, stats::quantile, 0.975, na.rm = TRUE)
      .ci_r <- data.frame(
        statistic = names(.ptr), point = round(.ptr, 4),
        lo = round(.lo, 4), hi = round(.hi, 4),
        excludes_zero = !(.lo <= 0 & .hi >= 0),
        excludes_one  = !(.lo <= 1 & .hi >= 1),
        boot_R = .B, n_usable = sum(.ok), boot_seed = .seed_g,
        stringsAsFactors = FALSE)
      print(.ci_r, row.names = FALSE)
      .p_ord <- mean(apply(.bs[, paste0("dshare_", .wk), drop = FALSE], 1,
                           function(r) !anyNA(r) && !is.unsorted(r)), na.rm = TRUE)
      .probs_r <- data.frame(
        what = c("nofee_loses_share", "fee_released_gains_share",
                 "fee_share_rises", "ordering_preserved"),
        statement = c("the no-fee released state loses share",
                      "the fee-bearing released state gains share",
                      "the fee-bearing share of workplace training rises",
                      "the four states preserve the point ordering"),
        P = round(c(mean(.bs[, "dshare_released_nofee"] < 0, na.rm = TRUE),
                    mean(.bs[, "dshare_released_fee"]   > 0, na.rm = TRUE),
                    mean(.bs[, "fee_share_shift"]       > 0, na.rm = TRUE),
                    .p_ord), 3),
        benchmark = c(0.5, 0.5, 0.5, 1/24),
        stringsAsFactors = FALSE)
      cat("\n  Directional probabilities on the roster:\n")
      print(.probs_r[, c("statement", "P", "benchmark")], row.names = FALSE)
      .vc <- .ci_r[.ci_r$statistic == "conc_released_nofee", ]
      cat(sprintf("\n  Roster verdict: index %.3f [%.3f, %.3f], %s one.\n",
                  .vc$point, .vc$lo, .vc$hi,
                  if (.vc$lo > 1) "EXCLUDES" else "includes"))
      .out(.ci_r,    "referee_G_fee_states_roster_boot.csv")
      .out(.probs_r, "referee_G_fee_states_roster_probs.csv")
    }

    # ---- v-G-roster-4, 18 October: the own-time arm, split on these rows ----
    # B.6 and Sections 6.1 and 9 split the own-time arm into own time alone
    # and own time with released time, and set the joint states' share of
    # the decline against their share of the pre-birth base. Those numbers
    # came from 04, whose base is every pre-birth wave; the fee states above
    # use k = -3 to -1. Computed here, both use the same rows and the same
    # base window. The standard errors of the two groups add the states'
    # variances as if independent, as the text says.
    .gse <- stats::setNames(.gr_fit$se, .gr_fit$state)
    .gb  <- stats::setNames(.gr_fit$base_rate, .gr_fit$state)
    .grp <- list(pure_own       = c("owntime_nofee", "owntime_fee"),
                 mixed          = c("both_nofee", "both_fee"),
                 released_alone = c("released_nofee", "released_fee"))
    .own_split <- data.frame(
      part = names(.grp),
      est = vapply(.grp, function(s) sum(.gr_e[s]), numeric(1)),
      se_approx = vapply(.grp, function(s) sqrt(sum(.gse[s]^2)), numeric(1)),
      base_rate = vapply(.grp, function(s) sum(.gb[s]), numeric(1)),
      stringsAsFactors = FALSE)
    .own_split$est_pp       <- 100 * .own_split$est
    .own_split$se_pp_approx <- 100 * .own_split$se_approx
    .own_split$base_pct     <- 100 * .own_split$base_rate
    .ow_tot <- .gr_e[["owntime_arm_jttpeot"]]
    .ow_pm  <- .own_split$est[1] + .own_split$est[2]
    .ow_bsh <- .own_split$base_rate[2] / (.own_split$base_rate[1] + .own_split$base_rate[2])
    .own_sum <- data.frame(
      item = "owntime_arm",
      total = .ow_tot,
      mixed_decline_pct = 100 * .own_split$est[2] / .ow_tot,
      mixed_decline_pct_whole = 100 * .own_split$est[2] / .ow_tot,
      mixed_base_pct = 100 * .ow_bsh,
      concentration_index = (.own_split$est[2] / .ow_tot) / .ow_bsh,
      released_over_pure = .own_split$est[3] / .own_split$est[1],
      stringsAsFactors = FALSE)
    cat("\n  The own-time arm, split (approximate SEs treat states as independent):\n")
    print(.own_split, row.names = FALSE, digits = 4)
    print(.own_sum, row.names = FALSE, digits = 4)
    cat(sprintf("  own time alone plus mixed: %+.4f against the arm's %+.4f%s\n", .ow_pm, .ow_tot,
                if (abs(.ow_pm - .ow_tot) > 1e-8) "  *** CHECK: they should be equal" else ""))
    .out(.own_split, "referee_G_owntime_split.csv")
    .out(.own_sum,   "referee_G_owntime_summary.csv")
    rm(.gse, .gb, .grp, .ow_tot, .ow_pm, .ow_bsh)

    # ---- the paper's registry ----------------------------------------
    if (isTRUE(get0("WRITE_REPORT", ifnotfound = FALSE))) {
      if (!exists("pn_register_csv", mode = "function") && file.exists("paper_numbers.R"))
        source("paper_numbers.R")
      if (!exists("pn_register_csv", mode = "function")) {
        cat("  *** paper_numbers.R is not on the working directory, so nothing was\n",
            "      registered. B.6 cannot use \\pn{} keys from this run.\n", sep = "")
      } else {
        .bt <- attr(.panel, "build_tag")
        if (is.null(.bt)) .bt <- attr(.evframe, "build_tag")
        if (is.null(.bt)) .bt <- NA_character_
        .reg <- function(df, prefix, part, digits = NULL)
          pn_register_csv(df, prefix = prefix, part = part, script = "REFEREE_RUN.R",
                          build_tag = .bt, id = names(df)[1], digits = digits)
        .reg(.gr_fit[, c("state", "est", "se", "ci_lo", "ci_hi", "base_rate", "n")],
             "Gr", "G_roster_fit", digits = c(n = 0))
        .reg(.states_r[, c("state", "share_of_base", "share_of_fall", "conc_index", "dshare")],
             "Gr", "G_roster_shares",
             digits = c(share_of_base = 1, share_of_fall = 1, conc_index = 2, dshare = 1))
        .reg(.states[, c("state", "base_rate", "est_post_f", "share_of_base", "share_of_fall",
                         "conc_index", "dshare")],
             "Gw", "G_wide_states",
             digits = c(base_rate = 3, est_post_f = 3, share_of_base = 1, share_of_fall = 1,
                        conc_index = 2, dshare = 1))
        .reg(.frames, "Gframe", "G_frames",
             digits = c(rows = 0, persons = 0, n_treated = 0, n_parent_controls = 0,
                        direct = 3, direct_se = 3, four_state_sum = 3))
        .reg(.own_split[, c("part", "est", "se_approx", "base_rate", "est_pp", "se_pp_approx", "base_pct")],
             "Grown", "G_roster_owntime_split",
             digits = c(est = 4, se_approx = 3, base_rate = 3, est_pp = 2, se_pp_approx = 1, base_pct = 0))
        .reg(.own_sum, "Grownsum", "G_roster_owntime_summary",
             digits = c(total = 4, mixed_decline_pct = 1, mixed_decline_pct_whole = 0,
                        mixed_base_pct = 1, concentration_index = 2, released_over_pure = 0))
        if (exists(".ci_r")) {
          .is_conc <- grepl("^conc", .ci_r$statistic)
          .reg(.ci_r[!.is_conc, c("statistic", "point", "lo", "hi")],
               "Grboot", "G_roster_boot_shares", digits = c(point = 1, lo = 1, hi = 1))
          .reg(.ci_r[.is_conc, c("statistic", "point", "lo", "hi")],
               "Grboot", "G_roster_boot_conc", digits = c(point = 3, lo = 3, hi = 3))
          .reg(data.frame(what = .probs_r$what, pct = 100 * .probs_r$P,
                          benchmark_pct = 100 * .probs_r$benchmark),
               "Grprob", "G_roster_probs", digits = c(pct = 1, benchmark_pct = 1))
          rm(.is_conc)
        }
        rm(.bt, .reg)
      }
    }

    rm(list = intersect(c(".gr", ".gr_post_bw", ".gr_has_el", ".gr_st", ".o0r", ".ptr",
                          ".gr_fit1", ".gr_ys", ".gr_fit", ".gr_e", ".gr_dev", ".states_r",
                          ".gw_par", ".gw_dir", ".frames", ".seed_g", ".rng_old", ".ids",
                          ".rowi", ".len", ".t0", ".bs", ".ok", ".lo", ".hi", ".ci_r",
                          ".p_ord", ".probs_r", ".vc", ".own_split", ".own_sum", "k", "nm"),
                        ls(all.names = TRUE)))
  }
  rm(.gr_need, .gr_src, .gr_why)
  rm(.core, .stat, .d, .pt, .B, .o0, .states)
  rm(.s, .st, .wk)
}
rm(.fee, .rel, .own)

.hdr("Done")
cat("A and B are the two that changed what the paper claims, and G is the\n")
cat("third: read against its own base the fee decomposition reverses its\n")
cat("second conclusion. C, E and F change what the paper admits. D is a\n")
cat("decision already made and only needs carrying out.\n")
cat("\nWhat this run wrote, if WRITE_REPORT was TRUE:\n")
cat("  referee_A_crosspartial_se.csv        the six evaluated horizons\n")
cat("  referee_A_crosspartial_coefs.csv     the two coefficients behind them\n")
cat("  referee_B_validation_share.csv       both denominators\n")
cat("  referee_B_validated_births.csv       three arms\n")
cat("  referee_C_cohort_weights.csv         birth-wave shares by sex\n")
cat("  referee_D_estimator_comparison.csv   static against cohort-robust\n")
cat("  referee_E_job_identifiers.csv        the column probe\n")
cat("  referee_F_stable_employer.csv        three arms\n")
cat("  referee_G_fee_states.csv             base, fall and index by state\n")
cat("  referee_G_fee_states_boot.csv        intervals with explicit flags\n")
cat("  referee_G_fee_states_probs.csv       directional probabilities\n")
cat("  referee_G_fee_states_verdict.csv     what the intervals license\n")
cat("  referee_G_fee_states_roster.csv      the same, on the event-study roster\n")
cat("  referee_G_fee_states_roster_fit.csv  every roster state, with clustered SEs\n")
cat("  referee_G_fee_states_roster_boot.csv roster intervals\n")
cat("  referee_G_fee_states_roster_probs.csv roster directional probabilities\n")
cat("  referee_G_fee_states_frames.csv      the two frames, counted\n")
cat("  referee_G_owntime_split.csv          the own-time arm split, roster\n")
cat("  referee_G_owntime_summary.csv        its shares of base and decline\n")
rm(.fmt, .hdr, .skip, .out, .outdir, .find_art, .panel, .need_a, .need_b,
   .bcol, .have_fixest, .have_tidyr)
if (exists(".ycol")) rm(.ycol)

# CLOSE ONLY WHAT WAS OPENED, then say where it went. A run that inherited
# a sink leaves it exactly as it found it.
rr_log_close()
rm(.rr_owns_sink, .rr_sink_out_before, .rr_sink_msg_before, .rr_log)
if (exists(".rr_dir")) rm(.rr_dir)
