# =====================================================================
# cs_event_study.R
# Heterogeneity-robust staggered event study of the child penalty on
# WAGES and EXPERIENCE, via Sun & Abraham (2021), fixest::sunab.
# (Used in place of the Callaway-Sant'Anna `did` engine, which fails in
#  its internal standardization on this extract; same design and
#  identifying assumption, robust to heterogeneous timing.)
#
# Run after within_couples_MASTER_hh.R with the broad person-wave frame
# `panel` in the workspace. Treatment timing = first birth, defined as in
# the master's section 6.1 (the wave the absorbing ever_parent goes 0->1).
# Never-treated = always-childless; left-censored parents are dropped.
# Endpoints are BINNED at |event time| = K (default 8); the binned cells
# pool the thinning long-horizon observations.
#
# -----------------------------------------------------------------------------
# ROSTER EXPORT (added 2026-06).  08_gelbach_decomposition.R must run on the
# SAME persons this event study estimates on, or its focal coefficient (and
# hence the experience share) is computed on the wrong population. This script
# is the single source of truth for that sample. After `cs` is finalised we
# export:
#   cs_event_study_sample_ids.rds   -- full event-study roster (treated +
#                                      never-treated), the literal persons the
#                                      Sun-Abraham models below estimate on.
#   cs_event_study_wageobs_ids.rds  -- the WAGE-OBSERVED subset (persons with at
#                                      least one valid hourly wage), to support
#                                      the wage-sample-selection sensitivity in
#                                      the Gelbach decomposition.
#   cs_event_study_sample_manifest.rds -- counts for both, so downstream audits
#                                      read the realised target rather than a
#                                      hard-coded number.
# The two rosters exist because the pooled experience share is sensitive to
# wage-sample selection: conditioning on being employed shrinks the focal
# women-minus-men wage coefficient and therefore raises the experience share.
# Exporting both lets the decomposition report the full-sample share and the
# wage-observed share side by side rather than silently picking one.
# -----------------------------------------------------------------------------
# =====================================================================
stopifnot(requireNamespace("fixest", quietly = TRUE))
suppressPackageStartupMessages({ library(dplyr); library(fixest) })

# ---------------------------------------------------------------------------
# WHERE THIS SCRIPT WRITES, said once. Every saveRDS and write.csv below used
# a bare filename, so the destination was getwd(): the same call put the file
# in WG when run from the project root and in WG/TG once the scripts moved
# into a subdirectory. That is how two copies of the panel and two of
# train_ledger.csv came to exist a directory apart, neither obviously current,
# and why paper_tables.R needs a four-path search to find one .rds file.
#
# project_dir is where these landed when the working directory WAS the root,
# so naming it changes nothing about the layout and makes the layout explicit.
# v2. PROJ_ROOT FIRST. 00_contract.R seals PROJ_ROOT and leaves project_dir
# unsealed as a back-compat alias, so preferring project_dir meant every
# output followed the one any script can reassign without anything noticing.
# The two agree today; the day they do not, five files move in silence.
# v-route. THE LOCAL DEFINITION WAS SHADOWING THE CONTRACT'S AND POINTING
# SOMEWHERE ELSE. 00_contract.R defines .out_path() as td_out("results", f)
# and seals DIR_RESULTS at PROJ_ROOT/output/results. This file redefined
# the same name to file.path(PROJ_ROOT, f), and because it is sourced AFTER
# the contract its version won: every saveRDS below read as routed and
# landed in the project root instead. On 9 September six .rds files sat
# beside the scripts at 11:01 while the master's own eight csv files, using
# the contract's helper, went to output/results in the same run.
#
# The contract's own note says it: use td_out() or a local NAME, do not
# reassign these. So the contract's helper is preferred when it is loaded,
# and the fallback below routes to the same place rather than to the root,
# so a standalone run puts files where a driven one does.
.out_path <- if (exists("td_out", mode = "function")) {
  function(f) td_out("results", f)
} else {
  function(f) {
    .root <- if (exists("PROJ_ROOT", envir = globalenv()))
      get("PROJ_ROOT", envir = globalenv())
    else if (exists("project_dir", envir = globalenv()))
      get("project_dir", envir = globalenv()) else getwd()
    .d <- file.path(.root, "output", "results")
    dir.create(.d, showWarnings = FALSE, recursive = TRUE)
    file.path(.d, f)
  }
}

# Figures follow the same rule. A bare "figures/..." resolves against
# whatever the session's working directory happens to be, which is the
# defect one line up in a different costume.
.fig_path <- if (exists("td_out", mode = "function")) {
  function(f) td_out("figures", f)
} else {
  function(f) {
    .root <- if (exists("PROJ_ROOT", envir = globalenv()))
      get("PROJ_ROOT", envir = globalenv()) else getwd()
    .d <- file.path(.root, "figures")
    dir.create(.d, showWarnings = FALSE, recursive = TRUE)
    file.path(.d, f)
  }
}

# v2. READS AND DELETES GO THROUGH THE SAME HELPER AS WRITES.
#
# This script wrote five files through .out_path() and then read two of them
# back, and deleted all five, with bare filenames that resolve to getwd(). On
# 31 August the root was WG and the working directory TG, so
# cs_event_study_results.rds was reported ABSENT by the master in the same run
# that created it, and the wage path fell back to a transcribed copy.
#
# THE DELETE LOOP WAS THE WORSE HALF and it is easy to miss. It cleared bare
# names from the working directory while the writes went to the root, so it
# removed whatever happened to be beside the script and left the genuinely
# stale file exactly where the next reader would find it. A stale-output guard
# that clears the wrong directory is worse than none: it prints "Removed
# stale" and leaves the staleness in place.
#
# .in_path() prefers the root and falls back to the working directory, because
# a file left beside the script by an older build is still the file the user
# meant. It says so when it does, rather than preferring it silently.
.in_path <- function(f) {
  .p <- .out_path(f)
  if (file.exists(.p)) return(.p)
  if (file.exists(f) && !identical(normalizePath(f, mustWork = FALSE),
                                   normalizePath(.p, mustWork = FALSE))) {
    message("NOTE: ", f, " found beside the script rather than at ", .p,
            "; reading it, but the two directories disagree.")
    return(f)
  }
  .p
}

# --- Namespace protection (avoid MASS/stats masking dplyr verbs) ----
select <- dplyr::select;       filter    <- dplyr::filter
mutate <- dplyr::mutate;       slice     <- dplyr::slice
recode <- dplyr::recode;       rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange <- dplyr::arrange;     count     <- dplyr::count;  lag <- dplyr::lag

K <- 8L   # bin |event time| >= K into the endpoints; widen/narrow as needed

# --- panel loading -------------------------------------------------
# Default input is the shared slim panel hilda_panel_data_W12_W24_slim.rds
# (~18,600 persons, 175 columns), which the master/training/IV scripts also
# read. Override with options(cs_panel_file=...) to point at the full
# hilda_panel_data_W12_W24.rds (16,598 cols) if the slim file is unavailable.
PANEL_FILE  <- getOption("cs_panel_file", "hilda_panel_data_W12_W24_slim.rds")
MIN_PERSONS <- 15000L   # real data has ~18,600; abort below this as implausible

# NB: exists("panel") is TRUE even when `panel` is only the base function
# (the name resolves to it), which silently skips the load. Test for an
# actual data frame instead, so a missing/clobbered panel always reloads.
# v13. THE CALLER'S `panel` IS SAVED AND PUT BACK AT THE FOOT.
#
# WHY IT MATTERS. This script either uses an ambient `panel` or loads and
# SCREENS one into that name. MASTER sources it mid-run, and
# RUN_FIGURES_PAPER.R and paper_tables.R both prefer an in-memory `panel`
# over resolving their own. So a run of this file could hand the two
# deliverable-building scripts a different frame from the one they would
# otherwise have used, and on 10 September that is exactly what happened:
# they built three stale figures, twelve \tofill macros and an empty
# app_b2_profile off a narrow frame left in the session.
#
# WHAT IS RESTORED. If the caller had a `panel`, the same object goes back
# unchanged. If it did not, the name is removed rather than left holding this
# script's screened frame, because a name that appears from nowhere is worse
# than one that is absent.
.cs_panel_in  <- if (exists("panel", inherits = TRUE) &&
                     is.data.frame(get0("panel", ifnotfound = NULL)))
  get("panel", inherits = TRUE) else NULL
.cs_panel_had <- !is.null(.cs_panel_in)

.have_panel <- .cs_panel_had
if (!.have_panel) {
  # v2. THE PANEL IS RESOLVED, NOT ASSUMED, AND THE FRAME IS CHECKED.
  #
  # PANEL_FILE was a bare name resolved against getwd(). Two copies of
  # hilda_panel_data_W12_W24_slim.rds exist, one beside the loader and one in
  # the script folder, and a bare name takes whichever is nearer. That is the
  # same resolution Pre_analysis_diagnostics2 had, and it is how a script
  # comes to run on a different file from the one the contract resolved.
  #
  # THE SECOND HALF MATTERS MORE THAN THE PATH. Loading the panel from disk
  # gives the RAW extract, 134,131 rows; the master hands in the SCREENED
  # frame, 129,544, because the contract applies the age-minus-experience
  # rule on resolve. So this branch and the normal path produce event studies
  # on different samples, and nothing said so. td_frame() is used when the
  # contract is loaded, and the explicit rule is the fallback.
  #
  # An absolute path in options(cs_panel_file=) is honoured as given: a
  # caller who names a file has answered the question this block asks.
  .cands <- if (grepl("^([A-Za-z]:)?[/\\\\]", PANEL_FILE)) PANEL_FILE else {
    .roots <- unique(as.character(unlist(list(
      if (exists("PROJ_ROOT")) PROJ_ROOT,
      if (exists("DIR_DATA"))  DIR_DATA,
      if (exists("project_dir")) project_dir,
      getwd(),
      normalizePath(file.path(getwd(), ".."), mustWork = FALSE)))))
    unique(file.path(.roots, PANEL_FILE))
  }
  .found <- .cands[file.exists(.cands)]
  if (!length(.found))
    stop("`panel` not in memory and '", PANEL_FILE, "' not found in:\n  ",
         paste(.cands, collapse = "\n  "),
         "\nRun the master first, or set options(cs_panel_file=) to a full path.",
         call. = FALSE)
  if (length(.found) > 1L) {
    message("NOTE: more than one panel found. Reading the first; ",
            "check they agree:")
    for (.p in .found)
      message("        ", .p, "  ",
              format(file.info(.p)$mtime, "%Y-%m-%d %H:%M"), ", ",
              round(file.info(.p)$size / 1048576, 2), " MB")
  }
  PANEL_FILE <- .found[1]
  message("Loading panel from ", PANEL_FILE)
  panel <- readRDS(PANEL_FILE)

  # The screen, so a stand-alone run estimates on the same rows the master
  # would have handed in. Reported either way: a count that does not change
  # means this ran on the raw extract and its event studies are not
  # comparable with anything else in the pipeline.
  # v13. td_frame(panel) WAS A MISCALL AND HID BEHIND ITS OWN FALLBACK.
  # td_frame's first argument is need_cols, not a frame, so passing the panel
  # errored on every run; the tryCatch swallowed it and the hard-coded 10-to-35
  # filter below ran instead. The result matched the contract by coincidence,
  # and only while the contract's bounds were 10 and 35. Now that
  # TD_SAMPLE_HI is a setting, a sensitivity run would have screened at the
  # default here while every other stage screened at the new bound, and
  # nothing would have said so.
  #
  # td_apply_sample_rule() IS THE RIGHT CALL. It takes a frame, applies the
  # contract's bounds, stamps the rule and reports the removals by group. An
  # error from it now stops the run: a silent fallback is what produced this.
  .n_raw <- nrow(panel)
  # v15, 14 October. NO SCREEN BY DEFAULT. The contract's rule is "none"
  # unless TD_SCREEN=on, and the hand rule below follows the same setting.
  .cs_scr_on <- if (exists("TD_SCREEN_ON")) isTRUE(TD_SCREEN_ON) else
    identical(tolower(trimws(Sys.getenv("TD_SCREEN", "off"))), "on")
  if (exists("td_apply_sample_rule", mode = "function")) {
    panel <- td_apply_sample_rule(panel)
  } else if (.cs_scr_on && all(c("age", "experience_years") %in% names(panel))) {
    .lo <- suppressWarnings(as.numeric(Sys.getenv("TD_SAMPLE_LO", "")))
    .hi <- suppressWarnings(as.numeric(Sys.getenv("TD_SAMPLE_HI", "")))
    if (is.na(.lo)) .lo <- 10
    if (is.na(.hi)) .hi <- 35
    .gap  <- panel$age - panel$experience_years
    panel <- panel[!is.na(.gap) & .gap >= .lo & .gap <= .hi, , drop = FALSE]
    message(sprintf("Contract not loaded: screen applied by hand, %g to %g.",
                    .lo, .hi))
    rm(.gap, .lo, .hi)
  } else if (!.cs_scr_on) {
    message("Contract not loaded, and no screen: TD_SCREEN is off.")
  }
  message(sprintf("Estimation screen: %s of %s rows retained (%s removed).",
                  format(nrow(panel), big.mark = ","),
                  format(.n_raw,      big.mark = ","),
                  format(.n_raw - nrow(panel), big.mark = ",")))
  if (.cs_scr_on && nrow(panel) == .n_raw)
    message("*** NOTHING WAS REMOVED. Neither td_frame() nor age and ",
            "experience_years\n    were available, so this run is on the raw ",
            "extract and its event\n    studies are on a different sample ",
            "from the rest of the pipeline.")
  rm(.n_raw, .cands, .found, .cs_scr_on)
  if (exists(".roots")) rm(.roots)
  if (exists(".p")) rm(.p)
}
stopifnot(is.data.frame(panel))
stopifnot(all(c("person_id","wave","female","ever_parent",
                "ln_hourly_wage_real","experience_years") %in% names(panel)))

# --- resolve weight column; subset only if handed a very wide frame -----
# The slim file is already lean, so normally no subsetting happens. If the
# caller hands in the full 16,598-column frame instead, we subset to the
# columns this script needs so it runs memory-light. We never write back to
# the shared slim file: the master owns that artifact.
.wt_cands <- c(getOption("cs_weight_col", NA_character_),
               "longitudinal_weight","lnwtrp","wlr","hhwtrp","wtrp","fwtrp")
.wt_col   <- .wt_cands[!is.na(.wt_cands) & .wt_cands %in% names(panel)][1]

if (ncol(panel) > 400L) {            # only the full frame is this wide
  # v12. jttpewt_any IS CARRIED THROUGH. Without it the training outcome
  # cannot be fitted here, and the only heterogeneity-robust aggregate the
  # paper can offer for its headline would have to come from a second
  # script fitting a second Sun-Abraham model on a second frame.
  .need <- c("person_id","wave","female","ever_parent",
             "ln_hourly_wage_real","experience_years",
             intersect("employed", names(panel)),
             intersect("jttpewt_any", names(panel)),
             intersect("age",      names(panel)),
             if (!is.na(.wt_col)) .wt_col)
  message(sprintf("Wide frame detected; subsetting %d -> %d columns%s",
                  ncol(panel), length(.need),
                  if (!is.na(.wt_col)) paste0(" (weight: '", .wt_col, "')")
                  else " (no weight column found)"))
  panel <- panel[, .need, drop = FALSE]
}
# standardise the weight column name the attrition block expects
if (!is.na(.wt_col) && .wt_col != "longitudinal_weight" &&
    .wt_col %in% names(panel))
  names(panel)[names(panel) == .wt_col] <- "longitudinal_weight"

# refuse to run on an implausibly small panel: abort loudly rather than produce
# 9,999-person results that could be mistaken for the real estimates.
.n_persons <- dplyr::n_distinct(panel$person_id)
# v-replication, 21 October 2026. The replication package's synthetic panel
# (00b_make_synthetic_hilda.R) is smaller than the extract and says so:
# attr(panel, "synthetic") is TRUE, and run_all.R sets SYNTHETIC_DATA. Its
# results cannot be mistaken for the real estimates, so the floor drops
# to 1,000 persons for that panel and for nothing else.
if (isTRUE(attr(panel, "synthetic")) ||
    isTRUE(get0("SYNTHETIC_DATA", ifnotfound = FALSE))) {
  MIN_PERSONS <- 1000L
  cat("  SYNTHETIC panel (", .n_persons, " persons): the wage path is ",
      "illustrative, not the paper's.\n", sep = "")
}
if (.n_persons < MIN_PERSONS)
  stop(sprintf(paste0("panel has only %d persons (< %d): implausibly small for ",
                      "the HILDA extract. Check that PANEL_FILE points at the ",
                      "real slim panel (hilda_panel_data_W12_W24_slim.rds)."),
               .n_persons, MIN_PERSONS))

# v14. A SECOND HARD-CODED SCREEN, AND IT WOULD HAVE UNDONE THE FIRST.
#
# The block near line 200 now calls td_apply_sample_rule(), which applies the
# contract's bounds and stamps the rule. This one then applied 10 to 35
# again, by hand. While the contract's bounds WERE 10 and 35 that was a
# harmless second pass over rows already removed. They are settings now, so
# under TD_SAMPLE_HI = 60 the first block would keep everything and this one
# would cut it straight back to 35, leaving the wage arm on the default
# sample while every other stage ran on the relaxed one. Nothing would have
# said so: the sensitivity banner prints at contract load, not here.
#
# THE GUARD MADE IT WORSE RATHER THAN SAFER. It skips when age_exp_diff is
# already present, and td_apply_sample_rule() does not create that column,
# so the guard always passed and the filter always ran.
#
# age_exp_diff IS STILL BUILT, because later blocks read it. What is removed
# is the second filtering.
if (!"age_exp_diff" %in% names(panel)) {
  if (!"age" %in% names(panel))
    stop("`age` column needed for the experience-age diagnostic is missing.")
  .n0 <- nrow(panel)
  panel <- panel %>% mutate(age_exp_diff = age - experience_years)
  .out <- sum(!dplyr::between(panel$age_exp_diff,
                              if (exists("TD_SAMPLE_LO")) TD_SAMPLE_LO else 10,
                              if (exists("TD_SAMPLE_HI")) TD_SAMPLE_HI else 35),
              na.rm = TRUE)
  .cs_on <- isTRUE(get0("TD_SCREEN_ON", ifnotfound = FALSE))
  message(sprintf(paste0("Experience-age: %d rows, %d persons; %d outside the",
                         " screen's bounds and NOT removed here%s."),
                  nrow(panel), dplyr::n_distinct(panel$person_id), .out,
                  if (.cs_on) "" else " (no screen applies: TD_SCREEN is off)"))
  if (.out > 0L && .cs_on)
    message("  The screen is td_apply_sample_rule()'s job and it has already",
            " run.\n  A non-zero count here means this frame reached the",
            " script unscreened.")
  rm(.n0, .out, .cs_on)
}

# clear stale event-study / Lee-bound outputs from any prior (e.g. synthetic)
# run so real and test outputs are never confused on disk.
# Both locations are cleared, and each is named as it goes. The write target
# is the one that matters; a copy beside the script is cleared too, because
# leaving it there is how .in_path()'s fallback finds a stale file later.
for (f in c("cs_event_study_results.rds", "cs_event_study_lee_bounds.rds",
            "cs_event_study_sample_ids.rds", "cs_event_study_wageobs_ids.rds",
            "cs_event_study_sample_manifest.rds", "cs_event_study_vcov.rds",
            "wage_path_tests.csv", "open_items_queries.csv"))
  for (.p in unique(c(.out_path(f), f)))
    if (file.exists(.p)) { file.remove(.p); message("Removed stale ", .p) }

has_lw  <- "longitudinal_weight" %in% names(panel)
has_emp <- "employed" %in% names(panel)   # extensive-margin (employment) outcome
if (!has_emp)
  warning("No `employed` column: Lee selection bounds will be SKIPPED.")

# --- first-birth timing: wave ever_parent first turns 0 -> 1 --------
g_tbl <- panel %>% group_by(person_id) %>% arrange(wave) %>%
  summarise(ep_first = first(ever_parent), ep_last = last(ever_parent),
            trans = { d_ep <- diff(ever_parent)            # NA-safe 0->1 jump
            idx  <- which(!is.na(d_ep) & d_ep > 0)
            if (length(idx)) wave[idx[1] + 1] else NA_integer_ },
            .groups = "drop") %>%
  mutate(cohort = case_when(
    !is.na(trans)                ~ as.double(trans),
    ep_first == 0 & ep_last == 0 ~ 10000,
    TRUE                         ~ NA_real_))

cs <- panel %>% inner_join(select(g_tbl, person_id, cohort), by = "person_id") %>%
  filter(!is.na(cohort))

clean_num <- function(x) { x <- unclass(x); attributes(x) <- NULL
if (is.numeric(x)) x else suppressWarnings(as.numeric(as.character(x))) }
has_train <- "jttpewt_any" %in% names(cs)
has_age <- "age" %in% names(cs)
keep_num <- c("wave","female","ln_hourly_wage_real","experience_years",
              if (has_emp) "employed",
              if (has_train) "jttpewt_any",
              if (has_age) "age",
              if (has_lw) "longitudinal_weight")
for (v in keep_num) cs[[v]] <- clean_num(cs[[v]])
cs <- cs %>% mutate(id = as.integer(factor(person_id)),
                    wave = as.double(wave), cohort = as.double(cohort))
cs <- as.data.frame(cs)
cat(sprintf("Sample: %d persons (%d never-treated, %d treated); waves %d-%d; K=%d\n",
            n_distinct(cs$id), n_distinct(cs$id[cs$cohort == 10000]),
            n_distinct(cs$id[cs$cohort < 10000]),
            as.integer(min(cs$wave)), as.integer(max(cs$wave)), K))

# =====================================================================
# TWO CLAIMS THE PAPER MAKES THAT NOTHING ON DISK COMPUTED
# ---------------------------------------------------------------------
# 1. NEVER-TREATED BIRTH REPORTS. Online Appendix E.1 says 3.7 percent of
#    never-treated men and 5.1 percent of never-treated women report a
#    birth. These people never meet the parenthood rule, so either the child
#    is never recorded as theirs or the panel ends before it could be. The
#    rows below separate the two: a report in the person's last observed
#    wave cannot be followed up, and a report followed by later waves with no
#    child recorded is the case that needs explaining.
# 2. THE 30-49 WINDOW. Section 4 says the conventional 30-49 restriction
#    "recovers nothing beyond four years after first birth". The rows below
#    count how far into the post-birth years treated persons aged 30-49 are
#    observed, per person-wave and for persons inside the band at every wave.
# Printed, and written to open_items_queries.csv. Nothing downstream reads
# it; the paper takes its numbers from the log.
# =====================================================================
.oi_env <- new.env()
.oi_env$rows <- list()
.oi_add <- function(q, sex, value, note = "") {
  .oi_env$rows[[length(.oi_env$rows) + 1L]] <- data.frame(
    query = q, sex = sex, value = value, note = note, stringsAsFactors = FALSE)
}
if ("had_child_this_wave" %in% names(cs)) {
  .has_part <- "partnered" %in% names(cs)
  .nt <- cs[cs$cohort == 10000,
            c("person_id", "female", "wave", "had_child_this_wave",
              if (has_age) "age", if (.has_part) "partnered"), drop = FALSE]
  .nt$bx <- clean_num(.nt$had_child_this_wave)
  if (.has_part) .nt$partnered <- clean_num(.nt$partnered)
  .nt <- .nt[order(.nt$person_id, .nt$wave), , drop = FALSE]
  .pp <- .nt %>% group_by(person_id, female) %>%
    summarise(asked     = any(!is.na(bx)),
              reported  = any(bx == 1, na.rm = TRUE),
              first_rep = if (any(bx == 1, na.rm = TRUE)) min(wave[which(bx == 1)]) else NA_real_,
              last_wave = max(wave),
              age_rep   = if (has_age && any(bx == 1, na.rm = TRUE)) age[which(bx == 1)[1]] else NA_real_,
              part_rep  = if (.has_part && any(bx == 1, na.rm = TRUE)) partnered[which(bx == 1)[1]] else NA_real_,
              pw_asked  = sum(!is.na(bx)),
              pw_rep    = sum(bx == 1, na.rm = TRUE),
              .groups = "drop")
  for (.s in c(0, 1)) {
    .sx <- if (.s == 1) "women" else "men"
    .d  <- .pp[.pp$female == .s, , drop = FALSE]
    .na <- sum(.d$asked); .nr <- sum(.d$reported)
    .oi_add("never-treated persons", .sx, nrow(.d))
    .oi_add("  with the birth item observed", .sx, .na)
    .oi_add("  reporting a birth", .sx, .nr,
            sprintf("%.1f%% of persons observed; %.2f%% of person-waves asked",
                    100 * .nr / max(.na, 1), 100 * sum(.d$pw_rep) / max(sum(.d$pw_asked), 1)))
    if (.nr) {
      .r <- .d[.d$reported, , drop = FALSE]
      .oi_add("    report in their last observed wave", .sx,
              sum(.r$first_rep == .r$last_wave), "the panel cannot follow these up")
      .oi_add("    report followed by later waves", .sx,
              sum(.r$first_rep < .r$last_wave), "no child is ever recorded for these")
      .oi_add("    reporting in more than one wave", .sx, sum(.r$pw_rep > 1))
      if (has_age)
        .oi_add("    median age at the first report", .sx, stats::median(.r$age_rep, na.rm = TRUE))
      if (.has_part)
        .oi_add("    partnered at the first report", .sx, sum(.r$part_rep == 1, na.rm = TRUE))
    }
  }
  rm(.nt, .pp, .d, .s, .sx, .na, .nr, .has_part)
  if (exists(".r")) rm(.r)
} else {
  cat("  had_child_this_wave is not on this frame; the birth-report query is skipped.\n")
}
if (has_age) {
  .tr <- cs[cs$cohort < 10000 & !is.na(cs$age),
            c("person_id", "female", "wave", "cohort", "age"), drop = FALSE]
  .tr$k    <- .tr$wave - .tr$cohort
  .tr$band <- .tr$age >= 30 & .tr$age <= 49
  .all_in  <- tapply(.tr$band, as.character(.tr$person_id), all)
  .tr$all_in <- unname(.all_in[as.character(.tr$person_id)])
  for (.s in c(0, 1)) {
    .sx <- if (.s == 1) "women" else "men"
    .a  <- .tr[.tr$female == .s & .tr$band, , drop = FALSE]
    .b  <- .tr[.tr$female == .s & .tr$all_in, , drop = FALSE]
    .oi_add("30-49 by person-wave: latest event time observed", .sx,
            if (nrow(.a)) max(.a$k) else NA_real_)
    .oi_add("30-49 by person-wave: person-waves at k >= 5", .sx, sum(.a$k >= 5),
            sprintf("%d persons", length(unique(.a$person_id[.a$k >= 5]))))
    .oi_add("30-49 at every wave: latest event time observed", .sx,
            if (nrow(.b)) max(.b$k) else NA_real_)
    .oi_add("30-49 at every wave: person-waves at k >= 5", .sx, sum(.b$k >= 5),
            sprintf("%d persons", length(unique(.b$person_id[.b$k >= 5]))))
  }
  rm(.tr, .all_in, .a, .b, .s, .sx)
} else {
  cat("  no `age` column on cs; the 30-49 query is skipped.\n")
}
if (length(.oi_env$rows)) {
  .oi <- do.call(rbind, .oi_env$rows)
  cat("\n#### Open items: never-treated birth reports and the 30-49 window ####\n")
  print(.oi, row.names = FALSE)
  utils::write.csv(.oi, .out_path("open_items_queries.csv"), row.names = FALSE)
  cat("  wrote open_items_queries.csv\n")
  rm(.oi)
}
rm(.oi_env, .oi_add)

# =====================================================================
# EXPORT THE EVENT-STUDY ROSTER(S) FOR 08_gelbach_decomposition.R
# ---------------------------------------------------------------------
# `cs` is the literal estimation frame for every Sun-Abraham fit below, so its
# distinct person_ids ARE the event-study population. We export that roster, and
# separately the wage-observed subset, plus a manifest of counts. The Gelbach
# script reads cs_event_study_sample_ids.rds and matches it exactly; the
# wage-observed roster supports the wage-selection sensitivity (the focal
# coefficient, and thus the experience share, depends on whether the sample is
# conditioned on observed employment).
# =====================================================================
es_person_ids_export <- sort(unique(cs$person_id))

# wage-observed subset: persons with at least one non-missing hourly wage in the
# event-study frame. This is the population a wage-conditioned decomposition
# would use; it is a STRICT SUBSET of the full roster.
es_wageobs_ids_export <- cs %>%
  filter(!is.na(ln_hourly_wage_real)) %>%
  distinct(person_id) %>%
  arrange(person_id) %>%
  pull(person_id)

# treated / never-treated splits, for the manifest (diagnostic only)
.n_treated      <- dplyr::n_distinct(cs$person_id[cs$cohort <  10000])
.n_nevertreated <- dplyr::n_distinct(cs$person_id[cs$cohort == 10000])

es_manifest <- list(
  n_persons_full        = length(es_person_ids_export),
  n_persons_wageobs     = length(es_wageobs_ids_export),
  n_treated             = .n_treated,
  n_never_treated       = .n_nevertreated,
  n_obs_full            = nrow(cs),
  n_obs_wageobs         = sum(!is.na(cs$ln_hourly_wage_real)),
  panel_file            = PANEL_FILE,
  K                     = K,
  built_at              = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)

saveRDS(es_person_ids_export, .out_path("cs_event_study_sample_ids.rds"))
saveRDS(es_wageobs_ids_export, .out_path("cs_event_study_wageobs_ids.rds"))
saveRDS(es_manifest, .out_path("cs_event_study_sample_manifest.rds"))

cat(strrep("-", 72), "\n", sep = "")
cat("Exported event-study rosters for the Gelbach decomposition:\n")
cat(sprintf("  full event-study roster : %s persons  -> cs_event_study_sample_ids.rds\n",
            format(es_manifest$n_persons_full, big.mark = ",")))
cat(sprintf("    of which treated      : %s\n",
            format(es_manifest$n_treated, big.mark = ",")))
cat(sprintf("    of which never-treated: %s\n",
            format(es_manifest$n_never_treated, big.mark = ",")))
cat(sprintf("  wage-observed subset    : %s persons  -> cs_event_study_wageobs_ids.rds\n",
            format(es_manifest$n_persons_wageobs, big.mark = ",")))
cat(sprintf("  manifest                : cs_event_study_sample_manifest.rds\n"))
cat("  NOTE: the full roster is what the Sun-Abraham models below estimate on,\n",
    "  and is the correct Gelbach focal-coefficient sample. The wage-observed\n",
    "  subset is for the wage-selection sensitivity only. Whichever the paper\n",
    "  reports, the prose and Table D1 must state the same population: the full\n",
    "  event-study roster is NOT 4,761 persons, that table cell must be checked.\n",
    sep = "")
cat(strrep("-", 72), "\n", sep = "")

# bin relative time >= K and <= -K into the endpoints (pools thinning cells)
BIN <- stats::setNames(list((-200):(-K), K:200), c(as.character(-K), as.character(K)))

# --- estimator: Sun-Abraham event study (binned endpoints) ----------
# v14. fe TAKES AN EXTRA TERM, so an age-absorbed arm can be fitted beside
# the main one instead of replacing it. Default unchanged, so every existing
# call returns exactly what it did.
fit <- function(d, y, w = NULL, fe = "id + wave") {
  rhs <- "sunab(cohort, wave, bin.rel = BIN)"
  f <- as.formula(paste0(y, " ~ ", rhs, " | ", fe))
  m <- tryCatch(feols(f, data = d, weights = w, cluster = ~ id),
                error = function(e) NULL)
  if (is.null(m))                                  # fallback: no binning
    m <- feols(as.formula(paste0(y, " ~ sunab(cohort, wave) | ", fe)),
               data = d, weights = w, cluster = ~ id)
  m
}
es_tab <- function(m) {
  ct <- coef(m); s <- se(m); nm <- names(ct)
  rt <- suppressWarnings(as.numeric(sub(".*::(-?\\d+).*", "\\1", nm)))
  k <- !is.na(rt) & grepl("::", nm)
  # v25. SIX DECIMALS, NOT FOUR. Every downstream table is built from this
  # one, and the prose quotes three. A coefficient stored as -0.0245 sits
  # exactly on the three-decimal boundary, so the printed figure depended on
  # which rounding ran second and nothing on disk could settle it. Six
  # decimals decide the third; the console still shows four.
  data.frame(event_time = rt[k], att = round(unname(ct[k]), 6),
             se = round(unname(s[k]), 6))[order(rt[k]), ]
}
att_overall <- function(m) tryCatch({
  a <- summary(m, agg = "att")$coeftable
  a[grep("ATT|att", rownames(a))[1], 1:2] }, error = function(e) c(NA, NA))
# v-cw. COMMON COHORT WEIGHTS. sa_common_weights.R, beside this script, forms
# each women-minus-men contrast inside the cohort-by-event-time cells and
# averages it on one set of weights. sa_try() returns one fit's cells, or
# NULL with a note when it cannot read them.
if (!exists("sa_common_gap", mode = "function")) {
  .sa_f <- c("sa_common_weights.R",
             file.path(if (exists("PROJ_ROOT")) PROJ_ROOT else ".", "sa_common_weights.R"))
  .sa_f <- .sa_f[file.exists(.sa_f)]
  if (!length(.sa_f)) stop("sa_common_weights.R is not beside this script.", call. = FALSE)
  source(.sa_f[1]); rm(.sa_f)
}
sa_try <- function(m, label) tryCatch(sa_cells(m), error = function(e) {
  cat("  *** sa_cells() failed for ", label, ": ", conditionMessage(e), "\n", sep = "")
  NULL })

# THE FULL COVARIANCE OF AN EVENT-TIME PATH. es_tab() keeps each horizon's
# standard error, which is enough to draw the path but not to test it: a
# joint test of the leads, a pooled window and a sensitivity bound all need
# the covariances between horizons. This keeps them, and refuses a matrix
# that is not the one behind the standard errors es_tab() reports, because
# fixest can return the covariance of the cohort-by-period coefficients
# rather than of the aggregated path.
# v15. For a Sun-Abraham fit, coef() and se() report the aggregated event-time
# path, but vcov() and summary()$cov.scaled keep the cohort-by-period
# coefficients, so none of the first three candidates in .event_vcov() can
# match. .sunab_agg_vcov() rebuilds the covariance of the aggregated path with
# the shares fixest's own aggregate() uses: the weighted column sums of the
# sunab dummies. Its diagonal then reproduces se(m), and pick() still checks
# that before the matrix is used.
.sunab_agg_vcov <- function(m, agg_names) {
  V <- tryCatch(stats::vcov(m), error = function(e) NULL)
  if (is.null(V) || is.null(rownames(V))) return(NULL)
  rn  <- rownames(V)
  sa  <- grepl(":cohort::", rn, fixed = TRUE)
  per <- sub(":cohort::.*$", "", rn)
  mm  <- tryCatch(stats::model.matrix(m), error = function(e) NULL)
  if (is.null(mm)) return(NULL)
  W <- matrix(0, length(agg_names), length(rn), dimnames = list(agg_names, rn))
  for (a in agg_names) {
    cols <- rn[sa & per == a]
    if (!length(cols) || !all(cols %in% colnames(mm))) return(NULL)
    x  <- sign(mm[, cols, drop = FALSE])
    sh <- if (!is.null(m$weights)) colSums(m$weights * x) else colSums(x)
    W[a, cols] <- sh / sum(sh)
  }
  W %*% V %*% t(W)
}

.event_vcov <- function(m) {
  b  <- stats::coef(m)
  nm <- names(b)
  rt <- suppressWarnings(as.numeric(sub(".*::(-?\\d+).*", "\\1", nm)))
  ok <- !is.na(rt) & grepl("::", nm)
  if (!any(ok)) return(NULL)
  s  <- unname(fixest::se(m))[ok]     # by position, as es_tab() reads it
  pick <- function(V) {
    if (is.null(V) || !all(nm[ok] %in% rownames(V))) return(NULL)
    V <- V[nm[ok], nm[ok], drop = FALSE]
    if (max(abs(sqrt(pmax(diag(V), 0)) - s)) > 1e-6 * max(1, max(s))) return(NULL)
    V
  }
  V <- pick(tryCatch(stats::vcov(m), error = function(e) NULL))
  if (is.null(V)) V <- pick(tryCatch(stats::vcov(summary(m)), error = function(e) NULL))
  if (is.null(V)) V <- pick(tryCatch(summary(m)$cov.scaled, error = function(e) NULL))
  if (is.null(V)) V <- pick(.sunab_agg_vcov(m, nm[ok]))
  if (is.null(V)) {
    cat("  .event_vcov(): no covariance matrix matches the reported standard\n",
        "  errors, so this path cannot be tested jointly.\n", sep = "")
    return(NULL)
  }
  o <- order(rt[ok])
  list(k = rt[ok][o], b = unname(b[ok])[o], V = unname(V)[o, o, drop = FALSE],
       binned = isTRUE(tryCatch(
         grepl("bin.rel", paste(deparse(stats::formula(m)), collapse = "")),
         error = function(e) FALSE)))
}

# ---- headline: UNWEIGHTED -----------------------------------------
res <- list()
.atts <- list()
.cells <- list()         # cohort-by-event cells of every fit, for common weights
.wage_vc <- list()       # per-sex covariance of the wage path, for the tests
.wage_vc_age <- list()   # the same for the age-absorbed arm
# v12. THE TRAINING OUTCOME IS FITTED HERE, and the reason is a specification
# objection the paper could not previously answer.
#
# WHAT TABLE 1 ACTUALLY REPORTS. Its headline comes from
# feols(jttpewt_any ~ post + post_f | person_id + wave^female) in
# 03a_design_audit.R, with post = 1[k >= 0]. So post_f is a STATIC two-way
# fixed-effects difference-in-differences: an average over every post-birth
# horizon in the window, not an effect at the birth wave. Methods says in the
# same breath that heterogeneous effects make two-way fixed effects
# unreliable when treatment timing varies, and that timing varies here by
# construction.
#
# WHY THE EXISTING DEFENCE DOES NOT WORK. Section 7.2 reports that the two
# estimators agree, -0.1163 against -0.1163. They are not estimating the same
# thing: the first is the Sun-Abraham coefficient at k = 0 and the second is
# the static post-period average. Two different estimands agreeing to four
# decimals is a coincidence, not a robustness result, and it leaves the
# staggered-timing objection unanswered.
#
# WHAT THIS PRODUCES. att_overall() below already aggregates a sunab fit to a
# post-period ATT and is applied to every outcome in this vector. Adding the
# training item yields, for each sex, the heterogeneity-robust average over
# k >= 0 that post_f is trying to estimate, and gap() differences the two.
# THAT is the like-for-like comparison, and it is the number that belongs
# beside the static estimate in Table 1.
#
# THE FRAME IS ALREADY RIGHT. `cs` is not conditioned on an observed wage:
# the filter at the roster export is for a separate object. feols drops rows
# missing the outcome per fit, so the training arm is fitted on the
# person-waves where the item was answered, which is the frame Table 1 uses.
# THE TRAINING ITEM IS FITTED ON TWO FRAMES, and the reason is what the
# column means off the employed rows. The panel codes a never-asked
# non-employed person-wave as zero on jttpewt_any, so an unconditional fit
# mixes a measured no with an imputed one. Table 1's headline is the
# EMPLOYED-ONLY arm at -0.1163, so that is the arm the post-period ATT has to
# be read against; the unconditional arm answers a different question and is
# kept because T1_uncond at -0.120 is the thing to compare it with.
if (has_train && "employed" %in% names(cs))
  cs$jttpewt_emp <- ifelse(cs$employed == 1, cs$jttpewt_any, NA_real_)
outcomes <- c("ln_hourly_wage_real", "experience_years",
              if (has_emp) "employed",
              if (has_train) "jttpewt_any",
              if ("jttpewt_emp" %in% names(cs)) "jttpewt_emp")
for (sx in c("women","men")) {
  d <- filter(cs, female == ifelse(sx == "women", 1, 0))
  for (y in outcomes) {
    m <- fit(d, y); tag <- paste(sx, y); a <- att_overall(m)
    .cells[[tag]] <- sa_try(m, tag)
    cat("\n== ", tag, " ==\n", sep = "")
    cat(sprintf("overall post-birth ATT = %.4f (SE %.4f)\n", a[1], a[2]))
    et <- es_tab(m); print(et); res[[tag]] <- et
    if (y == "ln_hourly_wage_real") .wage_vc[[sx]] <- .event_vcov(m)
    # v12. THE AGGREGATE IS KEPT. att_overall() has been computed and
    # printed on every run since this file was written, and discarded on
    # every one. A number that exists only in a console line is a number
    # the manuscript has to be told rather than shown, which is the whole
    # class of fault the verification pass has been closing.
    # v13. <<- WAS WRONG AND CRASHED THE MASTER. `.atts[[tag]] <- x` is a
    # replacement call: it must GET .atts before modifying it, and under <<-
    # that lookup skips the current environment and searches the parents.
    # Standalone the current environment IS globalenv() so it worked;
    # MASTER sources this file into a local environment, where .atts lives in
    # the local frame and the parent chain does not hold it, giving
    # "object '.atts' not found". The loop is not inside a function, and
    # res[[tag]] <- et four lines above already uses <-.
    .atts[[tag]] <- data.frame(sex = sx, outcome = y,
                                att = unname(a[1]), se = unname(a[2]),
                                stringsAsFactors = FALSE)
  }
}
gap <- function(a, b) { e <- intersect(a$event_time, b$event_time)
data.frame(event_time = e,
           gap = round(a$att[match(e, a$event_time)] - b$att[match(e, b$event_time)], 6),
           se  = round(sqrt(a$se[match(e, a$event_time)]^2 +
                              b$se[match(e, b$event_time)]^2), 4)) }
cat("\n== child penalty (women - men), ln hourly wage ==\n")
print(gap(res[["women ln_hourly_wage_real"]], res[["men ln_hourly_wage_real"]]))
cat("\n== differential experience (women - men) ==\n")
print(gap(res[["women experience_years"]], res[["men experience_years"]]))
if (has_emp) {
  cat("\n== child penalty (women - men), employment (extensive margin) ==\n")
  print(gap(res[["women employed"]], res[["men employed"]]))
}
# ---- persist the GAPS, not just the per-sex series -------------------------
# WHY THIS EXISTS. Until now this file saved `res` alone: six per-sex event
# studies keyed "women ln_hourly_wage_real", "men ln_hourly_wage_real" and so
# on. The gaps were computed by gap() immediately above, printed, and
# discarded. So the object every downstream consumer reads has never
# contained the quantity the paper actually reports, and each consumer was
# left to rebuild it from parts.
#
# MASTER_training.R did rebuild it, and got it wrong: its probe took the
# FIRST data frame in the list carrying an event-time and an estimate column,
# which is the women's series, and labelled it the gap between mothers and
# fathers. Mothers against never-mothers carries a large pre-birth level
# difference that differencing removes, so section 8.2 reported a spurious
# pre-trend, "8 of 11 pre-birth horizons reach |t| >= 1.96", against an
# argument that requires the path to be flat before the birth. Nothing
# errored. The figure drew, the lag computed, the run continued.
#
# The per-sex series stay, because the Lee bounds and the weighted checks
# below use them. What changes is that the gap no longer has to be inferred.
# --- THE POST-PERIOD AGGREGATES, WRITTEN DOWN ------------------------------
#
# WHAT THIS TABLE ANSWERS. Table 1's headline is post_f from a static
# two-way fixed-effects regression: an average over k >= 0. The rows below
# are the Sun-Abraham average over the same horizons, so the two are
# comparable in a way that a k = 0 coefficient is not. Where they agree the
# staggered-timing objection is answered on its own terms; where they differ
# the difference is the size of the bias the objection predicts, and the
# paper should report it rather than the agreement it currently claims.
#
# SEs ADD IN QUADRATURE, same convention as gap() above and same caveat: the
# two arms are fitted on disjoint individuals, so the sum is right for
# independence and the paper notes where that assumption bites.
# --- THE AGE-ABSORBED WAGE ARM --------------------------------------------
#
# WHAT THE REFEREE ASKED. Both estimators omit nonlinear age against a
# never-treated pool running 20 to 55, and both parents drift down against
# those controls at long lags. The abstract's ten-year figure comes from this
# arm, so it is the one the objection actually reaches; the training margin
# was answered separately in 03a and moved by five ten-thousandths.
#
# WHAT IS AND IS NOT IDENTIFIED. age is wave minus year of birth, and id
# absorbs year of birth, so the LINEAR component of the age profile is not
# separately identified from id and wave. What the dummies add is the
# nonlinear part, which varies across cohorts observed at the same wave.
# That is the classic age-period-cohort restriction and it is the reason the
# age effects themselves are absorbed and never read. fixest drops whatever
# levels are collinear and says so in the log.
#
# BESIDE, NOT INSTEAD. The main fits are untouched, so cs_event_study_results
# .rds and everything Check 7b reads from it are unchanged. This adds rows to
# cs_post_period_att.csv under an outcome name of its own.
if (exists("has_age") && isTRUE(has_age)) {
  for (sx in c("women", "men")) {
    d <- cs[cs$female == (sx == "women"), , drop = FALSE]
    d <- d[!is.na(d$age), , drop = FALSE]
    if (!nrow(d)) next
    m <- tryCatch(fit(d, "ln_hourly_wage_real", fe = "id + wave + age"),
                  error = function(e) NULL)
    if (is.null(m)) {
      cat("  age-absorbed wage arm failed for ", sx, "; skipped.\n", sep = "")
      next
    }
    a <- att_overall(m)
    cat(sprintf("  %s, wage, age absorbed: ATT %+.4f (SE %.4f)\n",
                sx, a[1], a[2]))
    .atts[[paste(sx, "ln_hourly_wage_real_agefe")]] <-
      data.frame(sex = sx, outcome = "ln_hourly_wage_real_agefe",
                 att = unname(a[1]), se = unname(a[2]),
                 stringsAsFactors = FALSE)
    # THE PATH, NOT ONLY THE AGGREGATE. att_overall() averages over k >= 0,
    # and the abstract's figure is the coefficient at k = +10. Those are
    # different numbers from different parts of one path, and the drift the
    # referee describes is at LONG LAGS, so the average is the wrong place to
    # look for it. Storing es_tab() puts the age-absorbed path beside the
    # plain one in the same object, so the ten-year point can be read and
    # gap() can difference the two arms the way it does for every other
    # outcome.
    res[[paste(sx, "ln_hourly_wage_real_agefe")]] <- es_tab(m)
    .cells[[paste(sx, "ln_hourly_wage_real_agefe")]] <- sa_try(m, paste(sx, "agefe"))
    .wage_vc_age[[sx]] <- .event_vcov(m)
  }
  cat("  Read the gap row against the plain ln_hourly_wage_real gap. A move\n",
      "  is age composition in the comparison pool; no move is the referee's\n",
      "  concern answered on the arm the abstract quotes.\n", sep = "")
} else {
  cat("  no `age` column on cs, so the age-absorbed wage arm is skipped.\n")
}

.att_tab <- do.call(rbind, .atts)
if (!is.null(.att_tab) && nrow(.att_tab)) {
  .w <- .att_tab[.att_tab$sex == "women", ]
  .m <- .att_tab[.att_tab$sex == "men", ]
  .g <- merge(.w[, c("outcome", "att", "se")], .m[, c("outcome", "att", "se")],
              by = "outcome", suffixes = c("_w", "_m"))
  .g$sex <- "gap"
  .g$att <- round(.g$att_w - .g$att_m, 6)
  .g$se  <- round(sqrt(.g$se_w^2 + .g$se_m^2), 6)
  # v-cw. COMMON COHORT WEIGHTS. The gap row is now the women-minus-men
  # difference formed inside each cohort-by-event cell and averaged on pooled
  # cell counts over common support. The estimator it replaces is kept as
  # gap_separate, with gap_average, gap_minimum, the composition term and the
  # weight outside common support beside it; MASTER registers them all as
  # csatt.<row>.<outcome>.att.
  .sch <- do.call(rbind, lapply(.g$outcome, function(o) {
    cw <- .cells[[paste("women", o)]]; cm <- .cells[[paste("men", o)]]
    if (is.null(cw) || is.null(cm)) return(NULL)
    s <- sa_schemes(cw, cm); s$outcome <- o; s
  }))
  .nocw <- setdiff(.g$outcome, if (is.null(.sch)) character(0) else .sch$outcome)
  if (length(.nocw))
    cat("  *** no cells for ", paste(.nocw, collapse = ", "),
        ", so their gap rows keep separate weights.\n", sep = "")
  if (!is.null(.sch)) {
    .chk <- merge(.sch[.sch$sex == "gap_separate", c("outcome", "att")],
                  .g[, c("outcome", "att")], by = "outcome")
    .bad <- .chk$outcome[abs(.chk$att.x - .chk$att.y) > 1e-5]
    if (length(.bad))
      stop("the separate-weight gap rebuilt from the cells differs from fixest's for ",
           paste(.bad, collapse = ", "), call. = FALSE)
    rm(.chk, .bad)
  }
  .att_tab <- rbind(.att_tab, .g[.g$outcome %in% .nocw, c("sex", "outcome", "att", "se")],
                    if (!is.null(.sch)) .sch[, c("sex", "outcome", "att", "se")])
  rm(.sch, .nocw)
  .att_tab$att <- round(.att_tab$att, 6)
  .att_tab$se  <- round(.att_tab$se,  6)
  cat("\n== post-period ATT, Sun-Abraham, averaged over k >= 0 ==\n")
  print(.att_tab, row.names = FALSE)
  if ("jttpewt_any" %in% .att_tab$outcome) {
    .tg <- .att_tab$att[.att_tab$sex == "gap" & .att_tab$outcome == "jttpewt_any"]
    cat(sprintf(paste0("  the training gap over k >= 0 is %+.4f. Table 1's",
                       " static post_f is the\n  quantity to read this",
                       " against, NOT the k = 0 coefficient.\n"), .tg))
    rm(.tg)
  }
  utils::write.csv(.att_tab, .out_path("cs_post_period_att.csv"),
                   row.names = FALSE)
  cat("  wrote cs_post_period_att.csv\n")
  rm(.w, .m, .g)
}
rm(.atts)

res[["gap ln_hourly_wage_real"]] <- gap(res[["women ln_hourly_wage_real"]],
                                        res[["men ln_hourly_wage_real"]])
# GUARDED, because the age-absorbed arm is conditional on `age` being on the
# frame and on both sexes fitting. A missing element here would take out the
# whole object, and this file's one job is to produce it.
if (all(c("women ln_hourly_wage_real_agefe",
          "men ln_hourly_wage_real_agefe") %in% names(res))) {
  res[["gap ln_hourly_wage_real_agefe"]] <-
    gap(res[["women ln_hourly_wage_real_agefe"]],
        res[["men ln_hourly_wage_real_agefe"]])
  # Printed at the horizons the paper actually quotes, so the comparison is
  # on the page rather than in the object. k = +10 is the abstract's figure.
  .pl <- res[["gap ln_hourly_wage_real"]]
  .ag <- res[["gap ln_hourly_wage_real_agefe"]]
  .kk <- intersect(intersect(.pl$event_time, .ag$event_time), c(0, 4, 6, 8, 10))
  if (length(.kk)) {
    cat("\n  wage gap, plain against age-absorbed, at the quoted horizons:\n")
    for (.k in sort(.kk))
      # gap() returns event_time, gap, se. NOT att: that is es_tab()'s column
      # name and reading it here would give NULL, sprintf would receive a
      # zero-length argument, and the loop would fail on a line that looks
      # like arithmetic.
      cat(sprintf("    k = %+3d   %+.4f   %+.4f   (%+.4f)\n", .k,
                  .pl$gap[.pl$event_time == .k][1],
                  .ag$gap[.ag$event_time == .k][1],
                  .ag$gap[.ag$event_time == .k][1] -
                    .pl$gap[.pl$event_time == .k][1]))
    cat("    A widening at long lags is the referee's drift removed, and it\n",
        "    makes the gap larger rather than smaller.\n", sep = "")
  }
  rm(.pl, .ag, .kk)
  if (exists(".k")) rm(.k)
}
res[["gap experience_years"]]    <- gap(res[["women experience_years"]],
                                        res[["men experience_years"]])
if (has_emp)
  res[["gap employed"]] <- gap(res[["women employed"]], res[["men employed"]])

# Provenance travels with the object. A consumer that reads this file can
# state where its numbers came from without being told separately, and a
# consumer reading an older file finds the attribute absent and knows it is
# holding the pre-gap vintage.
# v-cw. THE GAP PATHS ON COMMON WEIGHTS. Every "gap <outcome>" element is
# rebuilt from the cells, so MASTER's 8.2, the wage ledger and 05's R6 read
# the common-weight path unchanged. The gap() prints above still difference
# the two arms on their own weights; they are log lines only. The cells are
# saved, so the weights behind any gap can be inspected.
for (.o in sub("^women ", "", grep("^women ", names(.cells), value = TRUE))) {
  .cg <- tryCatch(sa_common_gap(.cells[[paste("women", .o)]], .cells[[paste("men", .o)]]),
                  error = function(e) NULL)
  if (!is.null(.cg) && !is.null(.cg$V))
    res[[paste("gap", .o)]] <- data.frame(event_time = .cg$k, gap = round(.cg$g, 6),
                                          se = round(.cg$path$se, 6))
}
rm(list = intersect(c(".o", ".cg"), ls(all.names = TRUE)))
saveRDS(.cells, .out_path("cs_event_study_cells.rds"))
attr(res, "gap_definition") <- paste(
  "gap rows are women minus men formed within each cohort-by-event-time cell and",
  "averaged with common weights, pooled cell counts on common support",
  "(sa_common_weights.R); se from the two arms' covariances summed.",
  "k = -1 is omitted and absent from every series.")
attr(res, "built_at")   <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
attr(res, "build_tag")  <- if (exists("bt")) bt else NA_character_

# =====================================================================
# WAGE-PATH TESTS
# ---------------------------------------------------------------------
# WHAT THE PAPER COULD NOT SAY. The wage leads fall from about +0.07 eight
# years before the birth to about zero, and each lead is inside its own
# interval. That is a statement about each lead alone. Whether they are
# jointly zero, how steep a drift they imply, and what is left of the
# post-birth gap once that drift is carried forward all need the
# covariances between horizons, which gap() does not keep. .event_vcov()
# kept them for the wage arms, and this block uses them.
#
# WHAT IS COMPUTED, for the plain path and the age-absorbed one:
#   1. a Wald test that every lead in WT_LEADS is zero;
#   2. the drift per year implied by a line through the leads that passes
#      through the reference year, and the post-birth gap net of that line;
#   3. the mean gap over WT_EARLY and over WT_LATE, and late minus early,
#      which tests the ordering claim directly;
#   4. Rambachan-Roth bounds from HonestDiD, under smoothness and under
#      relative magnitudes, for the late-window mean and for k = +10, when
#      the package is installed. options(cs_honestdid = FALSE) skips them.
# THE SEXES ARE INDEPENDENT. They are fitted on disjoint persons, so the
# gap's covariance is the sum of the two, the rule gap() already applies to
# the diagonal. A binned endpoint pools a tail and is kept out of every
# window.
# =====================================================================
WT_LEADS <- getOption("cs_wt_leads", -8:-2)
WT_EARLY <- getOption("cs_wt_early", 0:4)
WT_LATE  <- getOption("cs_wt_late",  5:10)
WT_POST  <- getOption("cs_wt_post",  0:10)
HD_M     <- getOption("cs_hd_m",     c(0, 0.005, 0.01, 0.02))
HD_MBAR  <- getOption("cs_hd_mbar",  c(0.5, 1, 1.5, 2))
# v-hd. THE RELATIVE-MAGNITUDE GRID. HonestDiD finds this interval by testing
# each point of a grid, which by default runs from -20 to +20 times the
# target's SE, and reports the lowest and highest points it accepts. A bound on
# the edge of the grid is the grid, not the interval. .hd_rm() runs each Mbar
# on +/-HD_GRID_MULT[1] SE, the default, reruns any whose bound touches the
# edge on the next width, and labels a bound still on the edge in
# bound_status. The smoothness bounds use the FLCI, which has no grid.
HD_GRID_MULT <- getOption("cs_hd_grid_mult", c(20, 100))
HD_GRID_PTS  <- getOption("cs_hd_grid_pts",  c(1000, 2000))
.hd_rm <- function(bh, sg, nPre, nPost, lv) {
  f    <- HonestDiD::createSensitivityResults_relativeMagnitudes
  post <- (nPre + 1L):(nPre + nPost)
  sdT  <- sqrt(max(drop(t(lv) %*% sg[post, post, drop = FALSE] %*% lv), 0))
  can  <- all(c("grid.lb", "grid.ub", "gridPoints") %in% names(formals(f)))
  mult <- if (can) HD_GRID_MULT else 20
  pts  <- if (can) HD_GRID_PTS else 1000
  n    <- min(length(mult), length(pts))
  if (!can)
    cat("  this HonestDiD takes no grid, so the relative-magnitude bounds use its\n",
        "  default of +/-20 SE and cannot be widened.\n", sep = "")
  one <- function(mbar, j) {
    hw   <- mult[j] * sdT
    args <- list(betahat = bh, sigma = sg, numPrePeriods = nPre,
                 numPostPeriods = nPost, Mbarvec = mbar, l_vec = lv)
    if (can) args <- c(args, list(grid.lb = -hw, grid.ub = hw, gridPoints = pts[j]))
    r <- tryCatch(do.call(f, args), error = function(e) {
      cat("  HonestDiD relative-magnitude bounds failed: ", conditionMessage(e), "\n", sep = "")
      NULL })
    if (is.null(r) || !nrow(r)) return(NULL)
    tol <- hw / (pts[j] - 1)       # half a grid step
    data.frame(Mbar = r$Mbar, lb = r$lb, ub = r$ub, mult = mult[j], hw = hw,
               edge = is.finite(r$lb) & is.finite(r$ub) &
                 (r$lb <= -hw + tol | r$ub >= hw - tol))
  }
  out <- one(HD_MBAR, 1L)
  if (is.null(out)) return(NULL)
  for (j in seq_len(n)[-1L]) {
    w <- which(out$edge)
    if (!length(w)) break
    for (i in w) {
      r <- one(out$Mbar[i], j)
      if (!is.null(r)) out[i, ] <- r
    }
  }
  out$status <- ifelse(!is.finite(out$lb) | !is.finite(out$ub),
                       "empty: no grid point accepted",
                ifelse(out$edge,
                       sprintf("at the grid edge, +/-%g SE: no informative bound", out$mult),
                ifelse(out$mult == mult[1L], "inside the grid",
                       sprintf("inside the grid after widening to +/-%g SE", out$mult))))
  out
}

.wage_path_tests <- function(vw, vm, label) {
  k  <- sort(intersect(vw$k, vm$k))
  iw <- match(k, vw$k); im <- match(k, vm$k)
  g  <- vw$b[iw] - vm$b[im]
  V  <- vw$V[iw, iw, drop = FALSE] + vm$V[im, im, drop = FALSE]
  tails <- if (isTRUE(vw$binned) || isTRUE(vm$binned)) c(-K, K) else numeric(0)
  at   <- function(h) which(k %in% setdiff(h, tails))
  iL <- at(WT_LEADS); iE <- at(WT_EARLY); iT <- at(WT_LATE); iP <- at(WT_POST)
  span <- function(ix) if (length(ix)) paste(min(k[ix]), "to", max(k[ix])) else ""
  sdv  <- function(a, ix) sqrt(max(drop(t(a) %*% V[ix, ix, drop = FALSE] %*% a), 0))
  z2p  <- function(z) 2 * stats::pnorm(-abs(z))
  acc  <- new.env(); acc$rows <- list()
  # v-hd. grid_halfwidth and bound_status are set only on the
  # relative-magnitude rows; see .hd_rm().
  add  <- function(quantity, horizons, estimate = NA_real_, se = NA_real_,
                   stat = NA_real_, df = NA_real_, p = NA_real_,
                   lower = NA_real_, upper = NA_real_,
                   grid_halfwidth = NA_real_, bound_status = "") {
    acc$rows[[length(acc$rows) + 1L]] <- data.frame(
      path = label, quantity = quantity, horizons = horizons,
      estimate = estimate, se = se, stat = stat, df = df, p = p,
      lower = lower, upper = upper, grid_halfwidth = grid_halfwidth,
      bound_status = bound_status, stringsAsFactors = FALSE)
  }

  # 1. Every lead zero, jointly.
  if (length(iL)) {
    gL <- g[iL]
    W  <- tryCatch(drop(t(gL) %*% solve(V[iL, iL, drop = FALSE], gL)),
                   error = function(e) NA_real_)
    add("joint test, all leads zero", span(iL), stat = W, df = length(iL),
        p = stats::pchisq(W, df = length(iL), lower.tail = FALSE))
  }

  # 2. The drift the leads imply, as a line through the reference year, and
  #    the gap left once that line is carried past the birth.
  VLi <- if (length(iL) >= 2)
    tryCatch(solve(V[iL, iL, drop = FALSE]), error = function(e) NULL) else NULL
  if (!is.null(VLi)) {
    x   <- k[iL] + 1
    vb  <- 1 / drop(t(x) %*% VLi %*% x)
    bt  <- vb * drop(t(x) %*% VLi %*% g[iL])
    add("lead drift per year, line through k = -1", span(iL),
        bt, sqrt(vb), bt / sqrt(vb), p = z2p(bt / sqrt(vb)))
    A   <- vb * drop(VLi %*% x)                 # bt = sum(A * g[iL])
    cgb <- drop(V[, iL, drop = FALSE] %*% A)    # covariance of each g with bt
    for (j in iP) {
      h  <- k[j] + 1
      dt <- g[j] - bt * h
      sd <- sqrt(max(V[j, j] + h^2 * vb - 2 * h * cgb[j], 0))
      add("gap net of the lead drift", as.character(k[j]), dt, sd, dt / sd,
          p = z2p(dt / sd))
    }
  }

  # 3. The early and late windows, and late minus early.
  win <- function(ix, name) {
    a <- rep(1 / length(ix), length(ix)); est <- sum(a * g[ix]); s <- sdv(a, ix)
    add(name, span(ix), est, s, est / s, p = z2p(est / s))
  }
  if (length(iE)) win(iE, "mean gap, early window")
  if (length(iT)) win(iT, "mean gap, late window")
  if (length(iE) && length(iT)) {
    ix  <- c(iE, iT)
    a   <- c(rep(-1 / length(iE), length(iE)), rep(1 / length(iT), length(iT)))
    est <- sum(a * g[ix]); s <- sdv(a, ix)
    add("late window minus early window", paste(span(iT), "less", span(iE)),
        est, s, est / s, p = z2p(est / s))
  }

  # 4. Rambachan-Roth bounds. HonestDiD wants consecutive leads ending at
  #    k = -2 and consecutive lags starting at k = 0, the reference omitted.
  want_hd <- isTRUE(getOption("cs_honestdid", TRUE))
  if (want_hd && !requireNamespace("HonestDiD", quietly = TRUE)) {
    cat("  HonestDiD is not installed, so the Rambachan-Roth bounds for the ",
        label, " path are skipped.\n", sep = "")
  } else if (want_hd && length(iL) >= 2 && length(iP) >= 1 &&
             all(diff(k[iL]) == 1) && max(k[iL]) == -2 &&
             all(diff(k[iP]) == 1) && min(k[iP]) == 0) {
    ix <- c(iL, iP); bh <- g[ix]; sg <- V[ix, ix, drop = FALSE]
    nPre <- length(iL); nPost <- length(iP)
    tg <- list()
    wl <- as.numeric(k[iP] %in% WT_LATE)
    if (sum(wl)) tg[["late-window mean"]] <- wl / sum(wl)
    if (10 %in% k[iP]) tg[["k = +10"]] <- as.numeric(k[iP] == 10)
    for (tn in names(tg)) {
      lv <- matrix(tg[[tn]], ncol = 1)
      oc <- tryCatch(HonestDiD::constructOriginalCS(
              betahat = bh, sigma = sg, numPrePeriods = nPre,
              numPostPeriods = nPost, l_vec = lv),
              error = function(e) NULL)
      if (!is.null(oc))
        add(paste0("95% interval under parallel trends, ", tn), "",
            lower = oc$lb[1], upper = oc$ub[1])
      sm <- tryCatch(HonestDiD::createSensitivityResults(
              betahat = bh, sigma = sg, numPrePeriods = nPre,
              numPostPeriods = nPost, Mvec = HD_M, l_vec = lv),
              error = function(e) {
                cat("  HonestDiD smoothness bounds failed: ", conditionMessage(e), "\n", sep = "")
                NULL })
      if (!is.null(sm)) for (r in seq_len(nrow(sm)))
        add(sprintf("Rambachan-Roth, smoothness M = %.3f, %s", sm$M[r], tn), "",
            lower = sm$lb[r], upper = sm$ub[r])
      # v-hd. On an explicit grid, widened where a bound touches its edge.
      rr <- .hd_rm(bh, sg, nPre, nPost, lv)
      if (!is.null(rr)) for (r in seq_len(nrow(rr)))
        add(sprintf("Rambachan-Roth, relative magnitudes Mbar = %.1f, %s", rr$Mbar[r], tn), "",
            lower = rr$lb[r], upper = rr$ub[r],
            grid_halfwidth = rr$hw[r], bound_status = rr$status[r])
      if (!is.null(rr) && any(rr$edge))
        cat("  ", label, ", ", tn, ": the relative-magnitude bound for Mbar = ",
            paste(rr$Mbar[rr$edge], collapse = ", "),
            " is still at the grid edge,\n  so it is uninformative; do not quote it as an interval.\n",
            sep = "")
    }
  } else if (want_hd) {
    cat("  the ", label, " path's leads or lags are not consecutive around the\n",
        "  reference year, so the Rambachan-Roth bounds are skipped.\n", sep = "")
  }
  do.call(rbind, acc$rows)
}

.wt_rows <- list()
for (.arm in list(list(vc = .wage_vc, label = "plain"),
                  list(vc = .wage_vc_age, label = "age absorbed"))) {
  if (!is.null(.arm$vc[["women"]]) && !is.null(.arm$vc[["men"]])) {
    # v-cw. The tests run on the common-weight gap, passed as one arm against
    # a zero arm, so .wage_path_tests() needs no change.
    .oc <- if (.arm$label == "plain") "ln_hourly_wage_real" else "ln_hourly_wage_real_agefe"
    .cg <- tryCatch(sa_common_gap(.cells[[paste("women", .oc)]], .cells[[paste("men", .oc)]]),
                    error = function(e) NULL)
    .wt_rows[[.arm$label]] <- if (!is.null(.cg) && !is.null(.cg$V)) {
      .wage_path_tests(sa_as_arm(.cg, binned = isTRUE(.arm$vc[["women"]]$binned)),
                       sa_zero_arm(.cg$k), .arm$label)
    } else {
      cat("  *** no common-weight wage gap for the ", .arm$label,
          " path; its tests use separate weights.\n", sep = "")
      .wage_path_tests(.arm$vc[["women"]], .arm$vc[["men"]], .arm$label)
    }
    rm(.oc, .cg)
  } else {
    cat("  wage-path tests skipped for the ", .arm$label,
        " path: its covariance was not kept (see .event_vcov()).\n", sep = "")
  }
}
if (length(.wt_rows)) {
  .wt <- do.call(rbind, .wt_rows)
  rownames(.wt) <- NULL
  .wt_print <- .wt
  .num <- c("estimate", "se", "stat", "p", "lower", "upper")
  .wt_print[.num] <- lapply(.wt_print[.num], function(x) round(x, 4))
  cat("\n#### Wage-path tests, women minus men, log hourly wage ####\n")
  print(.wt_print, row.names = FALSE)
  utils::write.csv(.wt, .out_path("wage_path_tests.csv"), row.names = FALSE)
  cat("  wrote wage_path_tests.csv\n")
  rm(.wt, .wt_print, .num)
}
# v15. Written only when a covariance was kept, and the message says how many.
.kept <- c(plain = length(.wage_vc), age_absorbed = length(.wage_vc_age))
if (sum(.kept)) {
  saveRDS(list(plain = .wage_vc, age_absorbed = .wage_vc_age),
          .out_path("cs_event_study_vcov.rds"))
  cat("  wrote cs_event_study_vcov.rds: event-time coefficients and covariances\n",
      "  for ", .kept[["plain"]], " plain and ", .kept[["age_absorbed"]],
      " age-absorbed fits, so these tests can be rerun without refitting.\n", sep = "")
} else {
  cat("  cs_event_study_vcov.rds not written: no covariance was kept.\n")
}
rm(.wt_rows, .arm, .kept)

saveRDS(res, .out_path("cs_event_study_results.rds"))
cat("\n  cs_event_study_results.rds: ", length(res), " elements (",
    sum(grepl("^gap ", names(res))), " gap series). Downstream consumers\n",
    "  should read the 'gap ' elements and must not difference the per-sex\n",
    "  series themselves.\n", sep = "")

# =====================================================================
# LEE (2009) TRIMMING BOUNDS ON THE EVENT-STUDY WAGE PATH
# ---------------------------------------------------------------------
# Wages are observed only for the EMPLOYED. Childbirth pushes women out
# of employment far more than men, so the post-birth wage sample is
# differentially selected by sex. Lee (2009, ReStud) bounds the wage ATT
# under one monotonicity assumption: the treatment (here, the extra
# post-birth employment exit borne by women relative to men) only ever
# moves people OUT of the wage sample, never in. The differential exit
# is the "trimming proportion" p: we trim that fraction from the relevant
# tail of the better-off group's observed wage distribution to construct
# the worst-/best-case bound.
#
# Implementation, per horizon k and pooled, in the women-minus-men metric
# the paper reports:
#   1. p_k = differential employment drop (men's employment ATT minus
#      women's employment ATT at k, relative to the always-childless),
#      i.e. the share of would-be-employed women selected out by birth in
#      EXCESS of the male counterfactual. p_k <= 0 contributes no trimming.
#   2. Normalise by the employed share of the treated-women cell at k to
#      get the trimming fraction q_k applied to that cell's observed wage
#      distribution.
#   3. LOWER bound on the women-minus-men wage gap: trim q_k from the TOP
#      of treated women's wages (assume the marginal stayers were high-wage
#      => observed gap understates the penalty => trimming the top widens
#      it). UPPER bound: trim q_k from the BOTTOM.
#   4. Re-form women-minus-men using the (untrimmed) men's path, which the
#      monotonic differential exit leaves as the reference.
#
# This uses ONLY objects already in `cs`: ln_hourly_wage_real, employed,
# female, cohort, wave. No re-estimation of the SA model is required for
# the bound itself; we reuse the employment-ATT gaps computed above.
# =====================================================================
if (has_emp) {
  cat("\n#### Lee (2009) trimming bounds on the women-minus-men wage path ####\n")
  
  # relative event time on the treated; never-treated have none
  cs$rel <- ifelse(cs$cohort < 10000, cs$wave - cs$cohort, NA_real_)
  
  # women-minus-men EMPLOYMENT gap by horizon (selection wedge), from res
  emp_gap <- if (is.data.frame(res[["gap employed"]])) res[["gap employed"]] else
    gap(res[["women employed"]], res[["men employed"]])
  # men lose less employment than women => (women - men) employment gap < 0;
  # the differential female exit is its negative.
  # v25. THE FLOOR STAYS, THE SIGN IS NO LONGER LOST. pmax() at zero is
  # right for the bound: a horizon where women lose LESS employment than men
  # has no differential female exit to trim. But the floored series alone
  # cannot say whether a zero trimming share means "selection is balanced
  # here" or "selection runs the other way", and the set collapses to the
  # point estimate in both cases. Both series are carried and every horizon
  # is labelled, so the paper can report which sets are degenerate.
  emp_gap$p_raw  <- -emp_gap$gap
  emp_gap$p_diff <- pmax(emp_gap$p_raw, 0)   # >=0 fraction selected out, women>men
  
  # observed women-minus-men wage gap by horizon (the headline path)
  wage_gap <- if (is.data.frame(res[["gap ln_hourly_wage_real"]]))
    res[["gap ln_hourly_wage_real"]] else
    gap(res[["women ln_hourly_wage_real"]], res[["men ln_hourly_wage_real"]])
  
  # employed share of the treated-women cell at each horizon (denominator
  # that turns an ATT-on-employment into a within-cell trimming fraction)
  emp_share_w <- cs %>%
    filter(female == 1, cohort < 10000, !is.na(rel)) %>%
    group_by(rel) %>%
    summarise(emp_share = mean(employed == 1, na.rm = TRUE), .groups = "drop")
  
  # per-horizon observed treated-women wage distribution, for trimmed means
  lee_one <- function(k) {
    praw <- emp_gap$p_raw [match(k, emp_gap$event_time)]
    pr   <- emp_gap$p_diff[match(k, emp_gap$event_time)]
    es   <- emp_share_w$emp_share[match(k, emp_share_w$rel)]
    obs  <- wage_gap$gap[match(k, wage_gap$event_time)]
    # one row per horizon, carrying WHY the bounds are what they are
    .row <- function(lo, hi, q, n_cell, status)
      data.frame(event_time = k, observed = obs, lo = lo, hi = hi, q = q,
                 p_raw = praw, emp_share = es, n_cell = n_cell,
                 status = status, stringsAsFactors = FALSE)
    if (is.na(pr) || is.na(es) || es <= 0)
      return(.row(obs, obs, 0, NA_integer_, "no_selection_estimate"))
    if (pr <= 0)
      return(.row(obs, obs, 0, NA_integer_, "no_differential_exit"))
    q <- min(pr / es, 0.999)                      # trimming fraction in-cell
    w_emp <- cs %>%
      filter(female == 1, cohort < 10000, employed == 1,
             !is.na(rel), rel == k, !is.na(ln_hourly_wage_real)) %>%
      pull(ln_hourly_wage_real)
    if (length(w_emp) < 20)
      return(.row(obs, obs, q, length(w_emp), "thin_cell"))
    mean_full <- mean(w_emp)
    lo_cut <- quantile(w_emp, 1 - q, names = FALSE, type = 7)  # keep bottom (1-q)
    hi_cut <- quantile(w_emp, q,     names = FALSE, type = 7)  # keep top    (1-q)
    mean_trim_top <- mean(w_emp[w_emp <= lo_cut])   # drop high earners
    mean_trim_bot <- mean(w_emp[w_emp >= hi_cut])   # drop low earners
    # shift the observed women-minus-men gap by the within-cell trimming shift
    .row(obs + (mean_trim_top - mean_full),   # widen penalty (lower bound)
         obs + (mean_trim_bot - mean_full),   # narrow penalty (upper bound)
         q, length(w_emp), "trimmed")
  }
  
  post_k <- sort(wage_gap$event_time[wage_gap$event_time >= 0])
  lee_tab <- do.call(rbind, lapply(post_k, lee_one))
  rownames(lee_tab) <- NULL
  # v25. ROUNDED FOR THE CONSOLE, NOT FOR THE FILE. This table used to be
  # rounded to four decimals BEFORE saveRDS, so a bound of -0.0245 reached
  # the manuscript as -0.025 or -0.024 depending on which rounding ran
  # second, and the artefact could not settle it. Full precision is stored.
  .lee_print <- lee_tab
  .lee_print[] <- lapply(.lee_print,
                         function(x) if (is.numeric(x)) round(x, 4) else x)
  cat("\nPer-horizon Lee bounds (women - men, log hourly wage):\n")
  print(.lee_print, row.names = FALSE)
  if (any(lee_tab$status != "trimmed")) {
    cat("\n  STATUS OTHER THAN 'trimmed' MARKS A DEGENERATE SET. There the\n")
    cat("  bounds equal the point estimate by construction, not by data:\n")
    cat("  no differential exit to trim (p_raw <= 0) or a cell too thin to\n")
    cat("  trim. Those horizons are not identified sets and must not be\n")
    cat("  reported as ones that exclude zero:\n")
    print(.lee_print[.lee_print$status != "trimmed",
                     c("event_time", "observed", "p_raw", "q", "status")],
          row.names = FALSE)
  }
  
  # pooled post-birth bound: trim the pooled treated-women employed wages by
  # the employment-weighted average differential exit over post-birth horizons
  .w_pool <- pmax(table(cs$rel[cs$female==1 & cs$cohort<10000 &
                                 cs$rel>=0])[as.character(
                                   emp_gap$event_time[emp_gap$event_time>=0])], 1,
                  na.rm = TRUE)
  p_pool     <- weighted.mean(emp_gap$p_diff[emp_gap$event_time >= 0],
                              w = .w_pool, na.rm = TRUE)
  # the same average WITHOUT the floor, so a pooled q of zero can be read
  p_pool_raw <- weighted.mean(emp_gap$p_raw[emp_gap$event_time >= 0],
                              w = .w_pool, na.rm = TRUE)
  es_pool <- cs %>% filter(female==1, cohort<10000, !is.na(rel), rel>=0) %>%
    summarise(s = mean(employed==1, na.rm=TRUE)) %>% pull(s)
  # v-cw. The pooled wage gap on common weights, from the cells already kept;
  # the two refits below run only if the cells are missing.
  obs_pool <- tryCatch(sa_common_gap(.cells[["women ln_hourly_wage_real"]],
                                     .cells[["men ln_hourly_wage_real"]])$post[["est"]],
                       error = function(e) NULL)
  if (!length(obs_pool))
    obs_pool <- att_overall(fit(filter(cs, female==1),"ln_hourly_wage_real"))[1] -
      att_overall(fit(filter(cs, female==0),"ln_hourly_wage_real"))[1]
  q_pool <- if (is.finite(p_pool) && es_pool>0) min(p_pool/es_pool, 0.999) else 0
  wp <- cs %>% filter(female==1, cohort<10000, employed==1, !is.na(rel), rel>=0,
                      !is.na(ln_hourly_wage_real)) %>% pull(ln_hourly_wage_real)
  if (length(wp) >= 50 && q_pool > 0) {
    mf <- mean(wp)
    lo_pool <- obs_pool + (mean(wp[wp <= quantile(wp,1-q_pool,names=FALSE)]) - mf)
    hi_pool <- obs_pool + (mean(wp[wp >= quantile(wp,  q_pool,names=FALSE)]) - mf)
  } else { lo_pool <- hi_pool <- obs_pool }
  cat(sprintf(
    "\nPooled post-birth women-minus-men wage gap:\n  observed = %.4f | Lee bounds = [%.4f, %.4f] | trim q = %.3f\n",
    obs_pool, lo_pool, hi_pool, q_pool))
  
  saveRDS(list(per_horizon = lee_tab,
               pooled = c(observed = obs_pool, lo = lo_pool, hi = hi_pool,
                          q = q_pool,
                          q_raw = if (is.finite(es_pool) && es_pool > 0)
                            p_pool_raw / es_pool else NA_real_)),
          .out_path("cs_event_study_lee_bounds.rds"))
  # v25. A CSV BESIDE THE RDS. The manuscript quotes six figures from this
  # table and a reader checking them should not have to open an R object.
  utils::write.csv(lee_tab, .out_path("cs_event_study_lee_bounds.csv"),
                   row.names = FALSE)
  cat("\n  Written: cs_event_study_lee_bounds.rds and .csv, full precision,\n",
      "  with p_raw, the cell size and the trimming status of every horizon.\n",
      sep = "")
  
  cat("\nReading: the LOWER bound trims the highest-wage marginal stayers from\n",
      "the treated-women cell (positive-selection-into-employment case the\n",
      "Online Appendix documents), so it is the conservative 'true penalty is\n",
      "at least this large' figure. The observed path sits inside [lo, hi].\n",
      sep = "")
}

# ---- attrition robustness: LONGITUDINAL-WEIGHTED wage path ---------
if (has_lw) {
  cat("\n#### attrition robustness: longitudinal-weighted (lnwtrp>0) wage ATT ####\n")
  for (sx in c("women","men")) {
    d <- filter(cs, female == ifelse(sx == "women", 1, 0),
                longitudinal_weight > 0, !is.na(longitudinal_weight))
    a <- att_overall(fit(d, "ln_hourly_wage_real", w = ~longitudinal_weight))
    cat(sprintf("  %-6s weighted wage ATT = %.4f (SE %.4f)\n", sx, a[1], a[2]))
  }
  cat("  (compare with the unweighted ATTs above; large divergence flags attrition.)\n")
} else cat("\n(no longitudinal_weight column found; skipping attrition robustness)\n")

cat("\nNotes:\n",
    sprintf(" * endpoints binned at |event time| >= %d; those bins pool the thinning\n", K),
    "   long-horizon cells, whose per-period counts and precision fall off.\n",
    " * event_time < 0 coefficients test parallel pre-trends; read them in the\n",
    "   women-minus-men gap, where life-cycle trends cancel and the near-birth\n",
    "   pre-period is flat.\n",
    " * wages are observed only for the employed, so the post-birth wage path is a\n",
    "   lower bound; the employment (extensive-margin) event study above bounds the\n",
    "   selection that drives this, women's employment falls sharply at first birth.\n",
    " * the Lee (2009) trimming bounds block formalises this: it trims the treated-\n",
    "   women wage cell by the differential (women-minus-men) employment exit and\n",
    "   reports [lower, upper] bounds on the wage gap at each horizon and pooled.\n",
    "   The lower bound is the conservative 'penalty is at least this large' figure.\n",
    " * headline is unweighted (a causal ATT); the longitudinal-weighted rerun is an\n",
    "   attrition check only, not the main estimate.\n",
    " * the event-study roster is exported (cs_event_study_sample_ids.rds) so the\n",
    "   Gelbach decomposition runs on EXACTLY this population; the wage-observed\n",
    "   subset is exported alongside for the wage-selection sensitivity.\n")

# =====================================================================
# When sourced from MASTER_hh.R with options(cs_estimation_only = TRUE),
# estimation stops above; the figure below is built only on a standalone
# run (or by RUN_FIGURES_hh.R).
# =====================================================================
if (!isTRUE(getOption("cs_estimation_only"))) {
  
  # =====================================================================
  # fig_event_study.R
  # Builds figures/fig_event_study.pdf, the women-minus-men event-study
  # paths for log wage and experience, from cs_event_study_results.rds
  # (saved by cs_event_study.R). Endpoints binned at |event time| = K to
  # match Table~\ref{tab:event_study}.
  # =====================================================================
  suppressPackageStartupMessages({ library(dplyr); library(ggplot2); library(tidyr) })
  # --- Namespace protection (avoid MASS/stats masking dplyr verbs) ----
  select <- dplyr::select;       filter    <- dplyr::filter
  mutate <- dplyr::mutate;       slice     <- dplyr::slice
  recode <- dplyr::recode;       rename    <- dplyr::rename
  summarise <- dplyr::summarise; summarize <- dplyr::summarize
  arrange <- dplyr::arrange;     count     <- dplyr::count;  lag <- dplyr::lag
  
  K <- 8L
  res <- readRDS(.in_path("cs_event_study_results.rds"))
  
  
  gap <- function(w, m) {                      # women minus men, with binning at +/-K
    e <- intersect(w$event_time, m$event_time)
    g  <- w$att[match(e, w$event_time)] - m$att[match(e, m$event_time)]
    v  <- w$se[match(e, w$event_time)]^2 + m$se[match(e, m$event_time)]^2
    eb <- pmax(pmin(e, K), -K)                 # bin tails into +/-K
    tapply(seq_along(eb), eb, function(ix) {   # inverse-variance pool within bin
      wgt <- 1 / v[ix]
      c(att = sum(g[ix] * wgt) / sum(wgt), se = sqrt(1 / sum(wgt)))
    }) -> agg
    data.frame(event_time = as.numeric(names(agg)),
               att = sapply(agg, `[`, "att"), se = sapply(agg, `[`, "se"))
  }
  
  dat <- bind_rows(
    transform(gap(res[["women ln_hourly_wage_real"]], res[["men ln_hourly_wage_real"]]),
              outcome = "Log hourly wage"),
    transform(gap(res[["women experience_years"]],    res[["men experience_years"]]),
              outcome = "Experience (years)"),
    if (all(c("women employed","men employed") %in% names(res)))
      transform(gap(res[["women employed"]], res[["men employed"]]),
                outcome = "Employment (pp)")) %>%
    mutate(lo = att - 1.96 * se, hi = att + 1.96 * se,
           outcome = factor(outcome, levels = c("Log hourly wage", "Experience (years)",
                                                "Employment (pp)")))
  
  p <- ggplot(dat, aes(event_time, att)) +
    facet_wrap(~outcome, scales = "free_y") +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
    geom_vline(xintercept = -0.5, linetype = 2, linewidth = 0.3, colour = "grey60") +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15) +
    geom_line() + geom_point(size = 1.4) +
    labs(x = "Years from first birth (women minus men)", y = "ATT") +
    theme_minimal(base_size = 11)
  
  # --- overlay Lee (2009) selection bounds on the wage panel ----------
  # The bounds are per-horizon post-birth (event times >= 0). We draw them
  # only on the "Log hourly wage" facet, as a shaded band between the
  # lower (conservative) and upper bound plus dashed boundary lines, so the
  # band visibly collapses onto the point estimate where differential
  # employment selection is exhausted (q -> 0 at the long horizons).
  if (file.exists(.in_path("cs_event_study_lee_bounds.rds"))) {
    lee <- readRDS(.in_path("cs_event_study_lee_bounds.rds"))$per_horizon
    lee_dat <- data.frame(
      event_time = pmin(lee$event_time, K),       # fold tails into +K to match
      lo = lee$lo, hi = lee$hi,
      outcome = factor("Log hourly wage",
                       levels = levels(dat$outcome)))
    # if folding created duplicate +K rows, keep the widest (most conservative)
    lee_dat <- lee_dat %>% group_by(event_time, outcome) %>%
      summarise(lo = min(lo), hi = max(hi), .groups = "drop") %>%
      as.data.frame()
    p <- p +
      geom_ribbon(data = lee_dat, aes(x = event_time, ymin = lo, ymax = hi),
                  inherit.aes = FALSE, fill = "#c1666b", alpha = 0.12) +
      geom_line(data = lee_dat, aes(x = event_time, y = lo),
                inherit.aes = FALSE, linetype = 3, linewidth = 0.35,
                colour = "#9a3b40") +
      geom_line(data = lee_dat, aes(x = event_time, y = hi),
                inherit.aes = FALSE, linetype = 3, linewidth = 0.35,
                colour = "#9a3b40")
    cat("overlaid Lee selection bounds on the wage panel\n")
  } else {
    cat("(no cs_event_study_lee_bounds.rds found; figure without Lee band)\n")
  }
  
  .f <- .fig_path("fig_event_study.pdf")
  ggsave(.f, p, width = 12, height = 3.6)
  cat("wrote ", .f, "\n", sep = "")
  rm(.f)
  
}  # end if (!isTRUE(getOption("cs_estimation_only")))

# --- put the caller's panel back ------------------------------------------
# Paired with the save above. Unconditional, so an early return or an error
# further up is the only way to skip it, and in that case the run has already
# stopped and the session is being inspected by hand.
if (isTRUE(.cs_panel_had)) {
  panel <- .cs_panel_in
  message("CS_event_study: caller's `panel` restored (",
          format(nrow(panel), big.mark = ","), " rows).")
} else if (exists("panel", inherits = FALSE)) {
  rm(panel)
  message("CS_event_study: `panel` removed; the caller did not have one.")
}
rm(.cs_panel_in, .cs_panel_had)
if (exists(".have_panel")) rm(.have_panel)
