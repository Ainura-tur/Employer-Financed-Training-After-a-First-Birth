# =====================================================================
# 03c_experience_units.R
# ---------------------------------------------------------------------
# The return to experience under alternative units. The PRICE of human capital, not the quantity.
#
# Designs: T8, the DML swap: 20 arms over 5 experience constructions and 4 identifying strategies
# Paper:   Section 7.3, not a gendered price of human capital
# Source:  03training_designs.R lines 3779-5767, extracted verbatim.
#
# THE REASON THE SPLIT EXISTS. 1,989 lines, 34 per cent of the old file, and the only slow block. It is not a training design at all: it estimates the gender difference in the return to experience, which is a different question from whether employers finance training. While it sat in 03 an iteration run could avoid it only through RUN_T8, a flag that then had to be threaded into the ledger, the skip audit and the master run record. See RESTRUCTURE_PLAN.md section 4 for which arms need to run: five are structurally unidentified and four suffice while iterating.
#
# HOW THIS FILE RELATES TO THE OTHER TWO. 03training_designs.R was 5,884
# lines doing three jobs. 03_common.R holds the prelude all three need:
# the contract guard, the configuration, report()/note()/fe_block(), the
# panel, the event frame and the benchmarks. 03_finish.R holds the ledger
# merge and the close. This file holds only its own designs, so it can be
# read, edited and re-run without the other two.
#
# THE LEDGER IS MERGED, NOT OVERWRITTEN. Re-running this module alone
# refreshes its own rows through the registry and leaves the other
# modules' rows in place. 03_finish.R explains why that is not the
# default-clobber behaviour the single file had.
# =====================================================================

TD03_MODULE <- "03c_experience_units.R"
# 03_logging.R is sourced UNCONDITIONALLY and BEFORE the guard, so the
# transcript is named for the module producing the output rather than
# for whichever module happened to trigger the prelude. In a warm
# session the guard below skips 03_common.R, and when the logging lived
# inside it that skip took the log name with it.
source("03_logging.R")
if (!isTRUE(get0(".TD03_COMMON_DONE", ifnotfound = FALSE))) {
  source("03_common.R")
  .TD03_COMMON_DONE <- TRUE
} else {
  cat("  03_common.R already loaded this session; reusing ev and the",
      "helpers.\n")
}

# =====================================================================
# T8. THE DML SWAP: THE RETURN TO EXPERIENCE IN FTE UNITS
#
# v21.31. TWO STANDING INSTRUCTIONS FROM PAPER_ATTENTION.md.
#
# ONE, section 2: "Do not draw OLS and DML as a robustness pair on beta2."
# They disagree in SIGN on two of four cells, with LASSO siding with DML,
# and both are precisely estimated. Two estimators of one parameter on one
# sample cannot both be right. The _wp and _mk arms below exist to test
# whether that is person-level confounding; until they have run, report
# the disagreement as UNRESOLVED rather than presenting either as the
# answer.
#
# TWO, this block issues NO UNITS VERDICT while its two preconditions
# fail independently: no arm has a gap distinguishable from zero, so
# there is nothing for a re-measurement to shrink; and no gap-consistent
# contrast clears the divergence gate, because the FTE weight reaches
# only about a third of the stock. Write it as two statements and fix the
# first before testing the second.
#
# WHY THIS IS HERE AND NOT IN 02. 02 established that the accumulation
# gap is roughly twice as large in FTE years as in calendar years, and
# it estimated the gender gap in the return to experience in both units.
# That estimate could not do the job: its MDE was 0.0038 against a pooled
# gap of 0.0026, so a two-way fixed-effects wage regression cannot see the
# object at all. The published pooled gap comes from DML-PLR on the
# couple-stacked dual-earner panel, and the only way to ask whether FTE
# re-measurement moves it is to put FTE experience into that estimator.
# This section does exactly that and nothing else.
#
# WHAT IS BEING TESTED. The published Panel A of the Mincer-gap table is
#   pooled        his +1.193%, hers +1.453%, gap -0.260%, p = 0.016
#   within-couple his +2.176%, hers +2.248%, gap -0.072%, p = 0.675
# and P3 reads the collapse from the first row to the second as evidence
# of a common experience-return technology within households. A referee
# can reply that the pooled gap is a units artefact: a calendar year of
# a mother's experience contains fewer hours than a calendar year of a
# father's, so the two coefficients are not per-unit comparable to begin
# with. If that is right, the pooled gap should move towards zero once
# experience is measured in hours.
#
# NOTE THE SIGN BEFORE READING THE RESULT. The published gap is negative:
# women's measured return per calendar year is HIGHER than men's. Naive
# mismeasurement predicts the opposite. If a woman's calendar year holds
# fewer hours, then dividing the same wage growth by more calendar years
# should depress her per-calendar-year return, making the gap positive.
# So the units story does not have a free hand here: it predicts a sign
# it does not observe, and this is a sharp test rather than a confirmation
# exercise. The interesting outcomes are that the gap moves towards zero
# (units mattered), that it moves further negative (units were masking a
# larger difference), or that it does not move (the pooled gap is about
# something other than hours).
#
# UNITS AND THE LEVEL PROBLEM. DML-PLR has no person fixed effect, so it
# compares experience levels across people. The in-panel FTE stock is not
# a level: it starts at zero at the person's first observed wave, so it
# would confound the FTE re-measurement with when someone entered the
# panel. The hybrid stock from 02, the calendar base at panel entry plus
# in-panel FTE accumulation, is the level analogue and is what goes in
# here. The calendar hybrid is run alongside it so that the two arms
# differ ONLY in the weight, and the published experience variable is run
# as a third arm so the reproduction can be checked separately from the
# swap.
# =====================================================================
cat("\n--- T8. DML swap: the return to experience in FTE units ---\n")

dml_pkgs <- c("DoubleML", "mlr3", "mlr3learners", "ranger")
have_dml <- all(vapply(dml_pkgs, function(p)
  requireNamespace(p, quietly = TRUE), logical(1)))
# v26. THE CANDIDATE LIST WAS BUILT FROM project_dir AND OUTDIR, AND BOTH
# MOVED. project_dir is one of the three back-compat aliases the contract
# leaves unsealed, and on the 1 September run it resolved to the script folder
# rather than the project root, so every candidate pointed under TG while 02
# had written the addendum under WG/output. The section reported "not found"
# for a file that exists, T8 was skipped, and the frame check below never ran.
#
# THE CAUSE IS UPSTREAM OF BOTH SCRIPTS: two copies of
# hilda_panel_data_W12_W24_slim.rds exist, one beside the loader and one in
# the script folder, so the contract resolved PROJ_ROOT to the second and
# DIR_DESIGNS with it. Deleting the duplicate fixes the root. This block makes
# the addendum findable either way, which is worth having because a path that
# works from only one root is how this started.
#
# RESOLVED FROM THE CONTRACT FIRST, then from the aliases, then from the
# working directory, with every candidate printed on a miss. A file found
# outside the contract's own tree is named as such, because that is the
# diagnosis rather than a detail.
.add_roots <- unique(as.character(unlist(list(
  if (exists("PROJ_ROOT")) PROJ_ROOT,
  if (exists("DIR_DATA")) dirname(DIR_DATA),
  if (exists("OUTDIR")) dirname(OUTDIR),
  if (exists("project_dir")) project_dir,
  getwd(),
  normalizePath(file.path(getwd(), ".."), mustWork = FALSE)))))
.add_try <- unique(as.character(unlist(lapply(.add_roots, function(r)
  c(file.path(r, "output", "fte_experience", "panel_fte_addendum.rds"),
    file.path(r, "fte_experience", "panel_fte_addendum.rds"))))))
# NEWEST, NOT FIRST IN ROOT ORDER. This took .hit[1], the first candidate
# path that happened to exist, and root order is about where the project is
# rather than which file is current. On 5 September it read an addendum from
# 1 September while a second copy sat in another root, and it said so in a
# warning nobody has to act on. Ordering by mtime makes the choice the
# defensible one and leaves the warning to report the rest.
.hit <- which(file.exists(.add_try))
if (length(.hit) > 1L)
  .hit <- .hit[order(file.info(.add_try[.hit])$mtime, decreasing = TRUE)]
add_path <- if (length(.hit)) .add_try[.hit[1]] else .add_try[1]

# AND CHECK IT AGAINST THE PANEL, which is the question that actually
# matters. Which of two addenda is newer is a tie-break; whether the one
# chosen predates the panel it describes is a correctness problem, because
# 02fte_experience.R builds the addendum FROM the panel. An addendum older
# than the panel was built from different data, and the row-overlap check
# below will not catch it whenever the panel changed by columns rather than
# rows, which is the common case.
#
# THE FIRST VERSION OF THIS CHECK MADE THE MISTAKE IT WAS WRITTEN TO CATCH.
# It built its own list of candidate panel paths and took the first that
# existed, which is precedence order and not date, so on 5 September it
# compared a 1 September addendum against a 25 August panel copy and
# reported "at least as new as the panel" while the live panel was dated the
# 5th. A check that can be wrong in the reassuring direction is worse than
# no check.
#
# Two changes. TD_FRAME_FILES is the contract's own ordered list of where a
# panel may live, so it is used when the contract is loaded rather than a
# list invented here. And every candidate is listed with its date, newest
# first, so a wrong pick is visible instead of silent.
.pan_try <- unique(as.character(c(
  if (exists("TD_FRAME_FILES")) TD_FRAME_FILES,
  unlist(lapply(.add_roots, function(r)
    c(file.path(r, "hilda_panel_data_W12_W24_slim.rds"),
      file.path(r, "output", "hilda_panel_data_W12_W24_slim.rds")))))))
.pan_hit <- which(file.exists(.pan_try))
# v27. THE SAME FILE REACHED BY TWO PATHS IS ONE PANEL, NOT TWO. The
# candidate list holds absolute paths and "./" forms, so a single file
# matched twice and the alarm below reported MORE THAN ONE PANEL ON DISK on
# every run. An alarm that fires when nothing is wrong is one people learn to
# scroll past, and this one guards a real hazard: two genuinely different
# panels, chosen between on mtime.
.pan_hit <- .pan_hit[!duplicated(normalizePath(.pan_try[.pan_hit],
                                               winslash = "/",
                                               mustWork = FALSE))]
if (length(.pan_hit) > 1L)
  .pan_hit <- .pan_hit[order(file.info(.pan_try[.pan_hit])$mtime, decreasing = TRUE)]
.pan_file <- if (length(.pan_hit)) .pan_try[.pan_hit[1]] else NA_character_

if (!is.na(.pan_file) && file.exists(add_path)) {
  if (length(.pan_hit) > 1L) {
    cat("  *** MORE THAN ONE PANEL ON DISK. Comparing against the NEWEST:\n")
    for (.q in .pan_try[.pan_hit])
      cat(sprintf("      %s  %s\n", .q,
                  format(file.info(.q)$mtime, "%Y-%m-%d %H:%M")))
    rm(.q)
  }
  .t_add <- file.info(add_path)$mtime
  .t_pan <- file.info(.pan_file)$mtime
  cat(sprintf("  addendum %s\n  panel    %s  (%s)\n",
              format(.t_add, "%Y-%m-%d %H:%M"),
              format(.t_pan, "%Y-%m-%d %H:%M"), .pan_file))
  if (isTRUE(.t_add < .t_pan))
    cat("  *** THE ADDENDUM PREDATES THE PANEL. 02fte_experience.R builds it\n",
        "      from the panel, so this one was built from different data. If\n",
        "      the panel changed only by adding columns the row-overlap check\n",
        "      below still passes and T8 is unaffected; if any filter, screen\n",
        "      or derived stock moved, it is not. Re-run 02fte_experience.R\n",
        "      unless you know which of those it was.\n", sep = "")
  else
    cat("  the addendum is at least as new as the newest panel on disk.\n")
  rm(.t_add, .t_pan)
}
rm(.pan_try, .pan_hit, .pan_file)
if (length(.hit) && exists("PROJ_ROOT") &&
    !startsWith(normalizePath(add_path, winslash = "/", mustWork = FALSE),
                normalizePath(PROJ_ROOT, winslash = "/", mustWork = FALSE)))
  cat("  *** The addendum was found OUTSIDE the contract's root. 02 and this\n",
      "      module disagree about where the project is, which is usually a\n",
      "      second copy of the panel moving PROJ_ROOT.\n", sep = "")
if (length(.hit) > 1L) {
  cat("  *** MORE THAN ONE ADDENDUM. Reading the NEWEST, listed first:\n")
  for (.p in .add_try[.hit])
    cat(sprintf("      %s  %s\n", .p,
                format(file.info(.p)$mtime, "%Y-%m-%d %H:%M")))
  rm(.p)
}
have_add <- file.exists(add_path)

if (!have_dml)
  cat("Missing packages:",
      paste(dml_pkgs[!vapply(dml_pkgs, function(p)
        requireNamespace(p, quietly = TRUE), logical(1))], collapse = ", "),
      "\nInstall them, or run source(\"PACKAGES.R\"), then re-run.\n")
if (!have_add) {
  cat("panel_fte_addendum.rds not found. Run 02_fte_experience.R first;\n",
      "  it writes the addendum this section reads.\n")
  cat("  paths tried:\n"); cat(paste0("    ", .add_try, collapse = "\n"), "\n")
  cat("  If the file exists somewhere else, set add_path before sourcing\n")
  cat("  rather than moving it, so 02 stays the one thing that decides\n")
  cat("  where it lives.\n")
} else {
  # The vintage matters as much as the presence: an addendum built on the
  # superseded 134,131-row extract reads fine and carries the wrong sample.
  cat("Addendum found at ", add_path, "\n  modified ",
      format(file.info(add_path)$mtime), ", ",
      format(file.info(add_path)$size / 1048576, digits = 3), " MB\n", sep = "")
}

if (!RUN_T8)
  cat("RUN_T8 is FALSE, so the DML swap is skipped. Everything above is\n",
      " unaffected; set RUN_T8 <- TRUE for the run that gets reported.\n")
if (RUN_T8 && have_dml && have_add) {
  suppressPackageStartupMessages({
    library(DoubleML); library(mlr3); library(mlr3learners)
  })
  # Quieten the mlr3 logger. The accessor moved out of mlr3's exports between
  # versions (mlr3 1.x expects lgr::get_logger), and this is cosmetic, so it
  # is wrapped: a logging call must never be able to stop an estimation run.
  try(suppressWarnings({
    if (requireNamespace("lgr", quietly = TRUE))
      lgr::get_logger("mlr3")$set_threshold("warn")
  }), silent = TRUE)
  
  n_threads <- suppressWarnings(as.integer(
    Sys.getenv("MASTER_HH_THREADS", unset = NA)))
  if (is.na(n_threads))
    n_threads <- min(4L, max(1L, parallel::detectCores() - 1L))
  cat(sprintf("Threads: %d | folds: %d | repetitions: %d | learner: %s\n",
              n_threads, DML_FOLDS, DML_REPS,
              if (DML_FAST) "glmnet (fast pass)" else "ranger"))
  if (!DML_FAST)
    cat("Random-forest nuisance on this many fits is slow. Expect tens of\n",
        "  minutes. Set DML_FAST <- TRUE for a lasso pass first.\n")
  
  # --- v26.1. CHOOSE THE ADDENDUM, THEN REFUSE A BAD ONE ---------------
  #
  # WHY THIS STOPS RATHER THAN WARNS. The previous revision printed a
  # warning and carried on. That is the right behaviour when someone is
  # watching and the wrong behaviour here: the DML below runs random
  # forests over five folds and three repetitions and takes hours, so an
  # unattended run that warns produces twenty arms nobody should use,
  # overwrites T8_dml_swap.csv with them, and leaves them looking
  # authoritative in the morning. A check that fires into an empty room
  # is not a check.
  #
  # WHAT MAKES AN ADDENDUM UNUSABLE, and why 03c cannot repair it. The
  # stocks are cumulative over each person's observed span, so a row the
  # estimation screen removes still contributed to the stock every later
  # row carries. The inner_join below drops the surplus row; it cannot
  # withdraw its contribution. Recomputing here is not an option either,
  # because 02 is deliberately the only place a stock is defined (F2).
  # So the only correct responses are to use a frame-consistent addendum
  # or to stop, and stopping is what an unattended run needs.
  #
  # ALL CANDIDATES ARE TESTED, not just the first that exists. On 1
  # September two addenda were on disk: a 26 August build of 129,325 rows
  # accumulated on the screened frame, and a 1 September build of 133,911
  # rows accumulated on the raw extract, the second sitting in the
  # directory this module resolves first. Preferring by path would have
  # taken the wrong one. Preferring by frame takes the right one whichever
  # directory it is in, and says which it took.
  .pan_key <- paste0(panel$person_id, "_", panel$wave)
  .cands <- lapply(.add_try[.hit], function(p) {
    .a <- readRDS(p)
    .k <- paste0(.a$person_id, "_", .a$wave)
    list(path = p, obj = .a, n = nrow(.a),
         outside = sum(!(.k %in% .pan_key)),
         matched = sum(.pan_key %in% .k))
  })

  cat("\nAddendum candidates, tested against this module's frame:\n")
  for (.c in .cands)
    cat(sprintf("  %-7s %s rows, %s outside the frame, %s of %s panel rows matched\n      %s\n      %s\n",
                if (.c$outside == 0L) "USABLE" else "REJECT",
                format(.c$n, big.mark = ","),
                format(.c$outside, big.mark = ","),
                format(.c$matched, big.mark = ","),
                format(nrow(panel), big.mark = ","),
                .c$path,
                format(file.info(.c$path)$mtime, "%Y-%m-%d %H:%M")))

  .good <- Filter(function(z) z$outside == 0L, .cands)

  if (!length(.good)) {
    cat("\n*** NO FRAME-CONSISTENT ADDENDUM. Every candidate carries rows\n")
    cat("    this module's frame excludes, which means its stocks were\n")
    cat("    accumulated over person-waves the estimation sample removes.\n")
    cat("    The join below would drop those rows and keep their\n")
    cat("    contribution, so every arm would be fitted on the right rows\n")
    cat("    using the wrong stocks.\n\n")
    cat("    THE FIX IS IN 02, NOT HERE. 02fte_experience.R opens with a\n")
    cat("    bare readRDS(PANEL); resolving through td_frame() there makes\n")
    cat("    the addendum match this frame. Rebuilding is the only thing\n")
    cat("    that changes the stocks rather than the sample.\n\n")
    cat("    To run anyway, knowing the arms are not comparable with the\n")
    cat("    published ones, set T8_ALLOW_FRAME_MISMATCH <- TRUE before\n")
    cat("    sourcing. Nothing below silently does that for you.\n")
    if (!isTRUE(get0("T8_ALLOW_FRAME_MISMATCH", ifnotfound = FALSE)))
      stop("03c: no frame-consistent addendum; refusing to spend hours ",
           "fitting arms that cannot be reported. See the note above.",
           call. = FALSE)
    cat("\n    T8_ALLOW_FRAME_MISMATCH is set. Proceeding under protest with\n")
    cat("    the first candidate. Label every arm below accordingly.\n")
    .pick <- .cands[[1]]
  } else {
    .pick <- .good[[1]]
    if (length(.cands) > length(.good))
      cat("\n  Chose the frame-consistent candidate over ",
          length(.cands) - length(.good),
          " that would have been rejected.\n", sep = "")
  }

  add      <- .pick$obj
  add_path <- .pick$path
  cat(sprintf("\nAddendum: %s rows, %d columns\n", 
              format(nrow(add), big.mark = ","), ncol(add)))
  cat(sprintf("  frame overlap: %s of %s panel rows matched, %s unmatched\n",
              format(.pick$matched,  big.mark = ","),
              format(nrow(panel),    big.mark = ","),
              format(nrow(panel) - .pick$matched, big.mark = ",")))
  if (nrow(panel) - .pick$matched > 0L)
    cat("  Panel rows with no addendum row are 02's weight filter, which it\n",
        "    reports as its own restriction; they complete-case out below.\n",
        sep = "")
  rm(.pan_key, .cands, .good, .pick)
  if (exists(".c")) rm(.c)
  
  # -------------------------------------------------------------------
  # Variable resolution. The slim panel's names are not guaranteed, so
  # each control is looked up rather than assumed, and the control set is
  # built from what is actually present. Anything missing is named in the
  # log rather than silently dropped.
  # -------------------------------------------------------------------
  # On a miss, print the columns that look close rather than leaving the
  # caller to search 234 names by hand. A resolver that fails silently is
  # worse than no resolver.
  pick <- function(cands, pattern = NULL) {
    hit <- intersect(cands, names(panel))
    if (length(hit)) return(hit[1])
    if (!is.null(pattern)) {
      near <- grep(pattern, names(panel), value = TRUE, ignore.case = TRUE)
      if (length(near))
        cat("  no match for", cands[1], "| columns that look close:",
            paste(utils::head(near, 15), collapse = ", "), "\n")
    }
    NA_character_
  }
  # educ_years is the name MASTER_hh.R uses, so it goes first.
  v_wage <- pick(c("ln_hourly_wage_real", "ln_wage"), "wage")
  v_educ <- pick(c("educ_years", "educ", "years_education", "edu_years",
                   "education_years", "schooling_years", "years_educ",
                   "years_of_education", "yrs_educ"), "educ|school|qual")
  v_hrs  <- pick(c("hours_worked_clean", "hours"), "hour")
  v_age  <- pick(c("age"), "^age")
  v_mar  <- pick(c("married", "married_num", "partnered", "is_married"),
                 "marri|partner")
  v_exp  <- pick(c("experience_years", "actual_experience", "exp"), "experi")
  
  cat("Resolved variables:\n")
  print(data.frame(role = c("wage", "schooling", "hours", "age", "married",
                            "published experience"),
                   column = c(v_wage, v_educ, v_hrs, v_age, v_mar, v_exp),
                   stringsAsFactors = FALSE), row.names = FALSE)
  
  if (is.na(v_wage) || is.na(v_educ) || is.na(v_exp)) {
    cat("\nThe wage, schooling or experience variable could not be resolved,\n")
    cat("so the swap cannot be run. Name them in the pick() lists above.\n")
    note("T8: DML swap not run; wage, schooling or experience column unresolved.")
  } else {
    
    # exp_fte_obs, base_exp_cal, waves_in and waves_seen are read here rather
    # than later, because the gap treatment cannot be reconstructed without
    # them and every contrast in T8 turns on it (F2).
    add_cols <- intersect(c("person_id", "wave", "exp_hybrid_cal",
                            "exp_hybrid_fte", "exp_hybrid_fte_obs",
                            "exp_hybrid_fte_int", "exp_hybrid_one_obs",
                            "exp_hybrid_one_int",
                            "exp_fte_obs", "exp_cal_obs", "base_exp_cal",
                            "in_panel", "weight_obs", "waves_in", "waves_seen"),
                          names(add))
    cat("Addendum columns read:",
        paste(setdiff(add_cols, c("person_id", "wave")), collapse = ", "), "\n")
    base <- panel %>%
      inner_join(add %>% select(all_of(add_cols)),
                 by = c("person_id", "wave")) %>%
      mutate(ln_wage      = .data[[v_wage]],
             educ         = .data[[v_educ]],
             exp_pub      = .data[[v_exp]],
             hours        = if (!is.na(v_hrs)) .data[[v_hrs]] else NA_real_,
             age_sq       = if (!is.na(v_age)) (.data[[v_age]] / 10)^2 else NA_real_,
             married      = if (!is.na(v_mar)) as.numeric(.data[[v_mar]]) else NA_real_,
             # A CONTROL AT A WAVE, NOT A CUT, so ever_parent is the right
             # column and the lifetime one would be wrong here. Appendix
             # A.2 lists this among the pooled controls as "an indicator
             # for having children": whether she has them NOW, which is
             # what belongs on the right-hand side of a wage equation.
             # Substituting the lifetime version would credit a woman with
             # children years before she had any.
             has_children = as.numeric(ever_parent),
             is_female    = as.numeric(female),
             fulltime     = as.numeric(fulltime))
    
    # ---------------------------------------------------------------
    # Couple stacking. Each person-wave carries the partner's variables
    # for the same wave, and the sample is restricted to couple-waves in
    # which both partners are employed with an observed wage, which is
    # the dual-earner definition the published DML uses.
    # ---------------------------------------------------------------
    prt <- base %>%
      select(person_id, wave, p_educ = educ, p_exp_pub = exp_pub,
             p_exp_cal = exp_hybrid_cal, p_exp_fte = exp_hybrid_fte,
             p_hours = hours, p_fulltime = fulltime, p_employed = employed,
             p_wage = ln_wage)
    
    # Dual-earner means both partners employed. The partner's WAGE is not a
    # control in any arm, so requiring it to be observed would select the
    # sample on both partners having a measured wage, which is not the
    # published restriction and is not innocuous: it drops couple-waves
    # differentially by whichever partner is harder to price, and both
    # members of the couple go with it.
    stacked <- base %>%
      filter(!is.na(partner_id), partner_id != "") %>%
      inner_join(prt, by = c("partner_id" = "person_id", "wave" = "wave")) %>%
      filter(employed == 1, p_employed == 1, !is.na(ln_wage)) %>%
      mutate(couple_id = paste0(pmin(person_id, partner_id), "_",
                                pmax(person_id, partner_id)))
    
    cat(sprintf("\nDual-earner stacked sample: %s person-waves, %s couples\n",
                format(nrow(stacked), big.mark = ","),
                format(dplyr::n_distinct(stacked$couple_id), big.mark = ",")))
    cat(sprintf("Published sample for comparison: %s person-waves, %s couples\n",
                format(MS_DML_OBS, big.mark = ","),
                format(MS_DML_COUPLES, big.mark = ",")))
    if (abs(nrow(stacked) - MS_DML_OBS) / MS_DML_OBS > 0.02)
      cat("These differ by more than two percent, so the calendar arm below is\n",
          "  a reconstruction rather than a reproduction. Read the swap as the\n",
          "  difference between the two arms, not as a check on the published\n",
          "  point estimates.\n")
    
    # -----------------------------------------------------------------
    # WHERE THE ROWS GO (R22)
    #
    # The first run returned 52,649 person-waves across 5,668 couples
    # against the published 84,292 across 4,661. FEWER waves and MORE
    # couples is a signature, not noise: the reconstruction identifies a
    # broader set of couples and then keeps far fewer of each couple's
    # waves, which is what a CONTEMPORANEOUS restriction does when the
    # published one is imposed at the couple level instead. Flagging the
    # discrepancy without attributing it leaves the reader to guess, so
    # each restriction is now counted in turn and the relaxed variant is
    # reported beside it.
    # -----------------------------------------------------------------
    wf_partnered <- base %>% filter(!is.na(partner_id), partner_id != "")
    wf_matched   <- wf_partnered %>%
      inner_join(prt, by = c("partner_id" = "person_id", "wave" = "wave"))
    wf_selfemp   <- wf_matched %>% filter(employed == 1)
    wf_bothemp   <- wf_selfemp %>% filter(p_employed == 1)
    wf_wage      <- wf_bothemp %>% filter(!is.na(ln_wage))
    cid <- function(d) if (!nrow(d)) 0L else dplyr::n_distinct(
      paste0(pmin(d$person_id, d$partner_id), "_",
             pmax(d$person_id, d$partner_id)))
    # v27. COUPLE-WAVES, COUNTED RATHER THAN INFERRED FROM person_waves.
    #
    # The appendix states a couple-wave count at the matched step and the
    # waterfall did not carry one, so the only route to it was to halve
    # person_waves. That route is wrong and the final step shows why:
    # 51,877 person-waves is an ODD number, and T8_stacked_cc.rds holds
    # 27,839 couple-waves against it, a ratio of 1.86. Of those, 3,801
    # carry one partner rather than two, because the restrictions are
    # applied per person-wave and can drop one side of a couple-wave while
    # keeping the other. Halving gives a number that looks plausible and
    # is not the quantity.
    #
    # SAME KEY AS cid(), ONE FIELD WIDER. The unordered person pair
    # identifies the couple; the pair plus the wave identifies the
    # couple-wave. Reusing the construction rather than writing a second
    # one means the two columns cannot come to disagree about what a
    # couple is.
    cwv <- function(d) if (!nrow(d)) 0L else dplyr::n_distinct(
      paste0(pmin(d$person_id, d$partner_id), "_",
             pmax(d$person_id, d$partner_id), "_", d$wave))
    waterfall <- data.frame(
      step = c("in the addendum", "has a partner id", "partner matched in wave",
               "self employed", "both employed", "own wage observed"),
      person_waves = c(nrow(base), nrow(wf_partnered), nrow(wf_matched),
                       nrow(wf_selfemp), nrow(wf_bothemp), nrow(wf_wage)),
      couples = c(NA_integer_, cid(wf_partnered), cid(wf_matched),
                  cid(wf_selfemp), cid(wf_bothemp), cid(wf_wage)),
      couple_waves = c(NA_integer_, cwv(wf_partnered), cwv(wf_matched),
                       cwv(wf_selfemp), cwv(wf_bothemp), cwv(wf_wage)),
      stringsAsFactors = FALSE)
    waterfall$lost <- c(NA_integer_, -diff(waterfall$person_waves))
    # ONE PARTNER OR TWO, per couple-wave. The share is not a curiosity:
    # a couple-wave holding one side is a couple-wave the within-couple
    # arms cannot difference, which is part of why five of the twenty arms
    # come back not identified.
    waterfall$singleton_cw <- waterfall$couple_waves * 2 - waterfall$person_waves
    cat("\nRow accounting, in the order the restrictions were applied:\n")
    print(waterfall, row.names = FALSE)
    write.csv(waterfall, file.path(OUTDIR, "T8_sample_waterfall.csv"),
              row.names = FALSE)
    
    # -----------------------------------------------------------------
    # THE SAME ACCOUNTING WITH THE FORCED CONSTRAINT FIRST (R13)
    #
    # A waterfall attributes each loss to whichever step happens to come
    # first, so its story depends on the ordering. Here the ordering is
    # actively misleading: an observed own wage is NOT a discretionary
    # restriction, it is the outcome variable, so it cannot be relaxed. Put
    # last, it appears to cost a few thousand rows because the employment
    # filters have already absorbed most of it. Put first, where it belongs,
    # it is the largest single constraint by a factor of three. Both orders
    # are printed so the attribution cannot be read off one of them alone.
    # -----------------------------------------------------------------
    n_matched   <- nrow(wf_matched)
    n_wage_m    <- sum(!is.na(wf_matched$ln_wage))
    n_partnered <- nrow(wf_partnered)
    wage_rate   <- if (n_matched > 0) n_wage_m / n_matched else NA_real_
    wf_w        <- wf_matched %>% filter(!is.na(ln_wage))
    wf_w_self   <- wf_w %>% filter(employed == 1)
    wf_w_both   <- wf_w_self %>% filter(p_employed == 1)
    waterfall2 <- data.frame(
      step = c("partner matched in wave", "own wage observed (FORCED)",
               "self employed", "both employed"),
      person_waves = c(n_matched, nrow(wf_w), nrow(wf_w_self), nrow(wf_w_both)),
      couples = c(cid(wf_matched), cid(wf_w), cid(wf_w_self), cid(wf_w_both)),
      couple_waves = c(cwv(wf_matched), cwv(wf_w), cwv(wf_w_self),
                       cwv(wf_w_both)),
      stringsAsFactors = FALSE)
    waterfall2$lost <- c(NA_integer_, -diff(waterfall2$person_waves))
    cat("\nThe same accounting with the forced constraint first:\n")
    print(waterfall2, row.names = FALSE)
    
    write.csv(waterfall2, file.path(OUTDIR, "T8_sample_waterfall_wage_first.csv"),
              row.names = FALSE)
    w_last  <- waterfall$lost[waterfall$step == "own wage observed"]
    w_first <- waterfall2$lost[2]
    if (length(w_last) == 1 && is.finite(w_last) && w_last > 0)
      cat(sprintf(paste("The wage requirement appears to cost %s rows when it",
                        "is applied last\nand %s when it is applied first, a",
                        "factor of %.1f. Read the second\ntable, not the",
                        "first, for what is available.\n"),
                  format(w_last, big.mark = ","),
                  format(w_first, big.mark = ","), w_first / w_last))
    if (nrow(wf_w) == nrow(wf_w_self))
      cat("Note that the wage filter SUBSUMES the employment filter: every\n",
          " partner-matched person-wave with an observed wage is employed, so\n",
          " the self-employed step in the first table removes nothing that the\n",
          " wage step had not already removed.\n")
    
    # -----------------------------------------------------------------
    # THE COUPLE-WAVE SUPPLY, WHICH SETTLES IT (R14)
    #
    # Everything above counts person-waves, and person-waves can be argued
    # about: whether each couple-wave contributes one row or two, whether
    # the wage is required of one partner or both. Couple-waves cannot. A
    # couple-wave exists only if BOTH partners are matched respondents in
    # that wave, and the published sample needs a definite number of them.
    # Comparing that against the panel's total supply, before any wage or
    # employment filter, decides between the two candidates rather than
    # leaving both open.
    # -----------------------------------------------------------------
    cpl_base <- wf_matched %>%
      mutate(couple_id = paste0(pmin(person_id, partner_id), "_",
                                pmax(person_id, partner_id)))
    # n_cw counts distinct COUPLE-waves, not rows (R15). The first version
    # used n(), which counts person-waves, so it was double for every
    # reciprocal couple. The tell was in its own output: the counts at nine
    # and ten waves came out identical, which can only happen if the
    # quantity is even for almost every couple. That made the wave-minimum
    # axis in the grid half as demanding as its label said, and it compared
    # a couple-wave requirement against a person-wave count.
    cpl_flags <- cpl_base %>%
      group_by(couple_id) %>%
      summarise(n_cw = dplyr::n_distinct(wave),
                n_rows = n(),
                both_ever = as.integer(any(employed == 1, na.rm = TRUE) &
                                         any(p_employed == 1, na.rm = TRUE)),
                both_share = mean(employed == 1 & p_employed == 1, na.rm = TRUE),
                .groups = "drop")
    cw_n <- dplyr::n_distinct(paste0(cpl_base$couple_id, "_", cpl_base$wave))
    cw_per <- cw_n / max(1L, dplyr::n_distinct(cpl_base$couple_id))
    pub_cw <- MS_DML_OBS / 2
    pub_cw_per <- pub_cw / MS_DML_COUPLES
    cat(sprintf(paste("\nCouple-waves: this panel matches %s, across %s couples,",
                      "%.2f each.\nThe published sample needs %s couple-waves",
                      "across %s couples, %.2f each,\nif each couple-wave",
                      "contributes one row per partner.\n"),
                format(cw_n, big.mark = ","),
                format(dplyr::n_distinct(cpl_base$couple_id), big.mark = ","),
                cw_per, format(round(pub_cw), big.mark = ","),
                format(MS_DML_COUPLES, big.mark = ","), pub_cw_per))
    asym <- 2L * cw_n - nrow(cpl_base)
    if (asym != 0L)
      cat(sprintf(paste("  %s couple-waves carry only one partner's row, so",
                        "partner_id is not\n  perfectly reciprocal. Small,",
                        "but it means a couple-wave is not always\n  two",
                        "person-waves.\n"), format(abs(asym), big.mark = ",")))
    if (pub_cw > cw_n) {
      cat(sprintf(paste("That is %.0f%% of EVERY couple-wave in the panel,",
                        "before any wage or\nemployment restriction is",
                        "applied. So the shortfall cannot be the wage\n",
                        "variable and cannot be a restriction: it is the",
                        "PARTNER LINK. The\npublished sample cannot be",
                        "requiring both partners to be matched\nrespondents",
                        "in the same wave.\n"),
                  100 * pub_cw / cw_n))
      note(paste("T8: the published sample needs more couple-waves than the",
                 "panel contains, so it cannot require both partners to be",
                 "matched respondents. The difference is the partner link,",
                 "not the wage variable or any sample rule."))
    }
    # The same conclusion from the other direction, which is independent of
    # how rows are counted: are there even enough long-observed couples?
    thr <- c(3L, 5L, 7L, 9L, 11L)
    wave_dist <- data.frame(
      couple_waves_at_least = thr,
      couples = vapply(thr, function(t) sum(cpl_flags$n_cw >= t), integer(1)))
    cat("\nCouples matched together for at least this many COUPLE-waves:\n")
    print(wave_dist, row.names = FALSE)
    need_at <- ceiling(pub_cw_per)
    have_at <- sum(cpl_flags$n_cw >= need_at)
    if (is.finite(have_at) && have_at < MS_DML_COUPLES)
      cat(sprintf(paste("The published average of %.2f waves per couple would",
                        "need most of its %s\ncouples at %d COUPLE-waves or more,",
                        "and only %s couples reach that. The couple\ncount",
                        "and the waves per couple cannot both be met at",
                        "once.\n"),
                  pub_cw_per, format(MS_DML_COUPLES, big.mark = ","),
                  need_at, format(have_at, big.mark = ",")))
    
    # -----------------------------------------------------------------
    # WHICH SAMPLE DEFINITION LANDS ON THE PUBLISHED PAIR (R25)
    #
    # The row accounting narrows this a long way. The published 84,292 sits
    # BETWEEN "has a partner id" and "partner matched in wave", and its
    # 4,661 couples are far fewer than either step gives, at 18.1 rows per
    # couple against 11.5 at the matched step. Fewer couples with more waves
    # each is the signature of a couple-level classification applied to
    # long-observed couples, not of a wave-level restriction. So the search
    # is over two axes: whether the dual-earner test is contemporaneous or
    # a property of the couple, and how many matched waves a couple needs.
    # Neither axis is guesswork once both are printed against the target.
    # -----------------------------------------------------------------
    # v21.11: THE SAMPLE SEARCH IS RETIRED. What stood here searched
    # fifteen sample rules for the one reproducing 84,292 person-waves
    # and 4,661 couples, reported pct_off_rows, pct_off_couples and
    # pct_off_rpc against those targets, derived a row ceiling to prove
    # no rule could reach them, and closed with a verdict about which
    # axis the published definition sat on.
    #
    # There is no published pair to reach. Without one, every column in
    # that table measured distance from a target nobody is pursuing, and
    # the verdict framed absence of agreement as failure. The sample rule
    # is now chosen on its merits.
    #
    # WHAT WAS WORTH KEEPING IS KEPT, above and elsewhere. The couple-wave
    # supply and the rows-per-couple arithmetic are printed by the
    # diagnostic that precedes this, and they are the power constraint
    # that actually binds. Pre_analysis_diagnostics2.R ITEM 2 reports the
    # same supply from an independent code path: 6,762 couples and 39,150
    # couple-waves on 20-55, of which 5,098 and 23,529 are dual-earner.
    # And the finding that partner_id is not perfectly reciprocal on four
    # couple-waves is a fact about the release, recorded in
    # PAPER_ATTENTION.md section 7 rather than lost with the search.
    #
    # Nothing downstream depended on it: gr was read only by its own
    # closing report, and thr and asym are both redefined later.
    # -----------------------------------------------------------------
    # THE ROW CEILING SURVIVES THE RETIREMENT, because it is not a
    # comparison against anything. Every sample rule also requires an
    # observed own wage, since the outcome IS the wage, and that is forced
    # rather than chosen. So there is a hard ceiling no restriction can
    # lift, and it is the power constraint that binds whatever rule is
    # picked. n_matched, n_wage_m, n_partnered and wage_rate come from the
    # wage-first waterfall above; only the loosened-link figure is new.
    ceil_loose <- if (is.finite(wage_rate)) round(n_partnered * wage_rate) else NA
    cat(sprintf(paste("\nRow ceiling: %s of %s partner-matched person-waves",
                      "carry an observed\n  wage (%.1f%%), so no restriction",
                      "of this panel can exceed %s rows.\n"),
                format(n_wage_m, big.mark = ","),
                format(n_matched, big.mark = ","), 100 * wage_rate,
                format(n_wage_m, big.mark = ",")))
    cat(sprintf(paste("  Loosening the partner link to any partner id at the",
                      "same wage rate\n  would still only supply about %s.\n"),
                format(ceil_loose, big.mark = ",")))
    cat("\nSample definition: chosen on its merits. The ceiling above and the\n")
    cat("couple-wave supply before it are the binding constraints; report the\n")
    cat("rule, the resulting count and the MDE, and do not rank candidate\n")
    cat("rules on distance from any external figure.\n")
    note(sprintf(paste("T8: the dual-earner row ceiling is %s person-waves,",
                       "forced by the wage requirement. Choose the sample rule",
                       "on its merits and report the MDE."),
                 format(n_wage_m, big.mark = ",")))
    # -----------------------------------------------------------------
    # T8a. A ONE-SIDED STOCK, AND DOES THE SWAP MOVE THE REGRESSOR? (R9)
    #
    # Two problems with the swap as first run, both visible in its own
    # output rather than in anything external.
    #
    # THE WEIGHT IS TWO-SIDED. 02 builds the FTE weight as h / 38 subject
    # to a ceiling of 1.25, so it does not only discount a part-time year,
    # it CREDITS an overtime year at up to 1.25. The ceiling binds on 27.9
    # percent of male employed person-waves against 10.0 percent of female
    # ones, so in the hybrid stock the male overtime credit and the female
    # part-time discount both operate and their net effect on the GAP is
    # ambiguous by construction. The hypothesis the section exists to test
    # is one-sided: that a mother's calendar year holds fewer hours than a
    # father's. A weight that also inflates his years tests something else.
    # So a third stock is built here with the weight capped at one, which
    # discounts part-time and credits nothing, and it is the arm that
    # answers the stated question. The two-sided arm stays as a secondary.
    # This belongs in 02 beside the other stocks; it is built here so the
    # fix does not require a rebuild.
    #
    # THE SWAP MAY NOT MOVE THE TREATMENT ENOUGH TO MOVE ANYTHING. Every
    # training design in this script carries an MDE against a substantive
    # benchmark and is dropped when the MDE exceeds it. T8 had no analogous
    # gate, and the quantity it turns on is not the sample size but the
    # DISTANCE BETWEEN THE TWO TREATMENTS. The hybrid stock is the
    # pre-panel calendar base plus in-panel accumulation, and the weight
    # touches only the in-panel part, so if the base dominates then the two
    # treatments are near-identical, the two gaps are near-identical by
    # construction, and the movement between them is noise. The first run
    # showed the treatment's spread moving from 9.56 to 9.82 for men and
    # from 8.55 to 8.58 for women: the group whose experience the weight is
    # meant to discount saw its spread move by a third of one percent.
    # -----------------------------------------------------------------
    cat("\n--- T8a. Treatment construction and divergence ---\n")
    
    # -----------------------------------------------------------------
    # THE GAP TREATMENT, WHICH REVISION 2 CONFOUNDED WITH THE WEIGHT (F2)
    #
    # 02 accumulates in-panel experience over a FILLED person-by-wave span
    # grid, from each person's first observed wave to their last, and it
    # carries two accumulations of the same weight:
    #   exp_fte_obs  gap waves contribute nothing   coalesce(fte_w, 0)
    #   exp_fte_int  interior gaps interpolated     approx(..., rule = 2)
    # and one calendar accumulation, exp_cal_obs = cumsum(coalesce(employed,
    # 0)), which is gap-blind like the first. It then defines
    #   exp_hybrid_cal = base_exp_cal + exp_cal_obs   GAP-BLIND
    #   exp_hybrid_fte = base_exp_cal + exp_fte_int   GAP-INTERPOLATED
    # so those two differ in the WEIGHT and in the GAP TREATMENT at once.
    # Revision 2 read the difference between them as the units swap, and it
    # built its one-sided stock by accumulating over observed panel rows,
    # which makes that stock the analogue of exp_fte_obs and not of
    # exp_fte_int. Its comment claimed the only difference was the ceiling.
    #
    # Whether that mattered is an empirical question. Interior wave gaps
    # could be correlated with motherhood, which would make the credit
    # larger for women and inflate the contrast; they could equally be
    # correlated with job mobility, which runs the other way. So the size
    # and the sign are measured and printed rather than assumed, here and in
    # 02's Section B. Revision 3 builds the one-sided stock in both gap
    # treatments, reads the stocks from the addendum, and pairs every
    # reported contrast so both sides share a gap treatment. exp_hybrid_fte minus exp_hybrid_fte_obs isolates the gap
    # credit with the weight held fixed, and it is printed by sex below: if
    # it differs by sex, the revision-2 contrast was partly a gap-treatment
    # contrast acting differently on mothers.
    #
    # THE WEIGHT IS TWO-SIDED. 02 builds the weight as h / FT_HOURS subject
    # to WEIGHT_RULE; under the "cap" rule in force that is
    # pmin(h / 38, 1.25), so it does not only discount a part-time year, it
    # CREDITS an overtime year at up to 1.25. The hypothesis this section
    # exists to test is one-sided: that a mother's calendar year holds fewer
    # hours than a father's. A weight that also inflates his years tests
    # something else. So the one-sided stock caps the weight at one,
    # discounting part-time and crediting nothing, and it is the arm that
    # answers the stated question. The two-sided arms stay secondary.
    #
    # THE SWAP MAY NOT MOVE THE TREATMENT ENOUGH TO MOVE ANYTHING. Every
    # training design here carries an MDE against a substantive benchmark
    # and is dropped when the MDE exceeds it. The quantity T8 turns on is
    # not the sample size but the DISTANCE BETWEEN THE TWO TREATMENTS: the
    # hybrid stock is the pre-panel calendar base plus in-panel
    # accumulation, the weight touches only the in-panel part, so if the
    # base dominates then the two treatments are near-identical and the
    # movement between them is noise. Revision 2 had the gate but computed
    # it on the two-sided arm and then issued the verdict on the one-sided
    # one, which is a different number (F1). The gate is now computed per
    # arm and each contrast is gated on its own arm.
    # -----------------------------------------------------------------
    
    # -----------------------------------------------------------------
    # THE STOCKS COME FROM THE ADDENDUM WHERE 02 SUPPLIES THEM (F2)
    #
    # 02 revision 3 builds all three weights in both gap treatments on one
    # span grid and ships them. This block therefore does no construction at
    # all when it is reading a current addendum; it checks what arrived,
    # names what is missing, and falls back to a local build only for an
    # addendum written by 02 revision 2. The fallback is announced, because
    # a stock built here from `panel` rows cannot be guaranteed to match
    # 02's grid and that mismatch is the whole subject of F2.
    # -----------------------------------------------------------------
    want <- c(cal        = "exp_hybrid_cal",
              one_obs    = "exp_hybrid_one_obs",
              one_int    = "exp_hybrid_one_int",
              fte_obs    = "exp_hybrid_fte_obs",
              fte_int    = "exp_hybrid_fte")
    have <- vapply(want, function(v) v %in% names(base), logical(1))
    cat("\nStocks read from the addendum:\n")
    print(data.frame(role = names(want), column = unname(want),
                     present = unname(have), stringsAsFactors = FALSE),
          row.names = FALSE)
    
    if (all(have)) {
      cat("All of them arrived, so nothing is rebuilt here and there is one\n")
      cat("definition of each stock, in 02 (F2).\n")
    } else {
      cat("\n*** FALLBACK. The addendum is missing", sum(!have), "of the five\n")
      cat("    stocks, so it was written by 02 revision 2 or earlier. The\n")
      cat("    one-sided stocks are rebuilt below from panel rows, which is\n")
      cat("    NOT guaranteed to match 02's span grid: 02 accumulates over a\n")
      cat("    filled person-by-wave span and this rebuild accumulates over\n")
      cat("    observed rows only, so the two agree on the gap-blind stock and\n")
      cat("    cannot agree on the interpolated one. Re-run 02 revision 3 and\n")
      cat("    re-run this script before reporting any T8 number.\n")
      note(paste("T8: the addendum predates 02 revision 3, so the one-sided",
                 "stocks were rebuilt locally. Re-run 02 and then 03 before",
                 "quoting any arm."))
      
      # A local copy of 02's interpolation, so the fallback is as close to
      # 02 as a fallback can be.
      interp_one <- function(w, y) {
        ok <- !is.na(y)
        if (!any(ok)) return(y)
        if (sum(ok) == 1L) return(rep(y[ok][1], length(y)))
        stats::approx(w[ok], y[ok], xout = w, rule = 2)$y
      }
      if (all(c("hours_worked_clean", "employed") %in% names(panel)) &&
          "base_exp_cal" %in% names(base)) {
        w_one_vec <- ifelse(is.na(panel$employed), NA_real_,
                            ifelse(panel$employed == 0, 0,
                                   ifelse(is.na(panel$hours_worked_clean),
                                          NA_real_,
                                          pmin(panel$hours_worked_clean /
                                                 FT_HOURS, 1))))
        one_span <- panel %>%
          group_by(person_id) %>%
          summarise(wv_lo = min(wave), wv_hi = max(wave), .groups = "drop") %>%
          mutate(n_span = wv_hi - wv_lo + 1L)
        one_grid <- data.frame(
          person_id = rep(one_span$person_id, one_span$n_span),
          wave      = sequence(one_span$n_span, from = one_span$wv_lo),
          stringsAsFactors = FALSE) %>%
          left_join(panel %>% select(person_id, wave) %>%
                      mutate(w_one = w_one_vec),
                    by = c("person_id", "wave")) %>%
          arrange(person_id, wave) %>%
          group_by(person_id) %>%
          mutate(exp_one_obs = cumsum(dplyr::coalesce(w_one, 0)),
                 exp_one_int = cumsum(dplyr::coalesce(interp_one(wave, w_one),
                                                      0))) %>%
          ungroup() %>%
          select(person_id, wave, exp_one_obs, exp_one_int)
        base <- base %>%
          left_join(one_grid, by = c("person_id", "wave")) %>%
          mutate(exp_hybrid_one_obs = base_exp_cal + exp_one_obs,
                 exp_hybrid_one_int = base_exp_cal + exp_one_int)
        if (!("exp_hybrid_fte_obs" %in% names(base)) &&
            "exp_fte_obs" %in% names(base))
          base <- base %>%
          mutate(exp_hybrid_fte_obs = base_exp_cal + exp_fte_obs)
        cat("    Rebuilt exp_hybrid_one_obs and exp_hybrid_one_int locally.\n")
      } else {
        cat("    The rebuild is not possible either: hours, employment or\n")
        cat("    base_exp_cal is absent. Only the arms whose stocks arrived\n")
        cat("    can run.\n")
      }
    }
    
    # Construction checks, run on whatever arrived or was rebuilt (R26). The
    # pairing matters: the one-sided stock cannot exceed the two-sided one at
    # the same gap treatment, and it cannot exceed the calendar stock when
    # both are gap-blind. An interpolated stock CAN exceed a gap-blind
    # calendar stock, which is exactly the confound, so that comparison is
    # measured below rather than treated as a fault.
    chk <- function(a, b, la, lb) {
      if (!all(c(a, b) %in% names(base))) return(invisible(NULL))
      v <- sum(base[[a]] > base[[b]] + 1e-8, na.rm = TRUE)
      m <- suppressWarnings(max(base[[a]] - base[[b]], na.rm = TRUE))
      if (v > 0) {
        cat(sprintf(paste("  *** %s exceeds %s on %s person-waves, by up to",
                          "%.4f years.\n  That is impossible by",
                          "construction. Do not report this arm.\n"),
                    la, lb, format(v, big.mark = ","), m))
        note(sprintf(paste("T8a: %s exceeds %s, which cannot happen. Fix the",
                           "weight or the accumulation grid before reporting",
                           "the arm."), la, lb))
      } else {
        cat(sprintf("  check passed: %s never exceeds %s (max %+.4f years).\n",
                    la, lb, m))
      }
    }
    cat("\nConstruction checks:\n")
    chk("exp_hybrid_one_obs", "exp_hybrid_fte_obs", "one-sided", "two-sided")
    chk("exp_hybrid_one_obs", "exp_hybrid_cal", "one-sided gap-blind",
        "calendar")
    chk("exp_hybrid_fte_obs", "exp_hybrid_fte", "two-sided gap-blind",
        "two-sided interpolated")
    
    # Rebuild the partner frame and the stacked sample so the new columns are
    # carried through both.
    prt <- base %>%
      select(person_id, wave, p_educ = educ, p_exp_pub = exp_pub,
             p_exp_cal = exp_hybrid_cal, p_exp_fte = exp_hybrid_fte,
             p_exp_one_obs  = dplyr::any_of("exp_hybrid_one_obs"),
             p_exp_one_int  = dplyr::any_of("exp_hybrid_one_int"),
             p_exp_fte_obs = dplyr::any_of("exp_hybrid_fte_obs"),
             p_hours = hours, p_fulltime = fulltime, p_employed = employed,
             p_wage = ln_wage)
    stacked <- base %>%
      filter(!is.na(partner_id), partner_id != "") %>%
      inner_join(prt, by = c("partner_id" = "person_id", "wave" = "wave")) %>%
      filter(employed == 1, p_employed == 1, !is.na(ln_wage)) %>%
      mutate(couple_id = paste0(pmin(person_id, partner_id), "_",
                                pmax(person_id, partner_id)))
    
    # How large the gap credit is with the weight held fixed. This is the
    # quantity revision 2's calendar-against-FTE contrast absorbed.
    #
    # ON THE STACKED ROWS, WHICH IS WHAT THE COMMENT USED TO CLAIM (F15).
    # Revision 3 computed this over every row of the addendum join, 133,911
    # of them, while saying it was on the T8 rows, of which there are
    # 52,649. 02 reports the same quantity on terminal stocks, so there were
    # three figures in circulation and none of them was on the sample the
    # arms are fitted to. The sample is named in the print.
    if (all(c("exp_hybrid_fte", "exp_hybrid_fte_obs") %in% names(stacked))) {
      # p95 among the rows that CARRY a credit (F16). Taken over everyone it
      # is zero whenever fewer than five percent have one, which repeats
      # pct_any and adds nothing.
      q95_pos <- function(x) {
        x <- x[!is.na(x) & x > 1e-8]
        if (!length(x)) NA_real_ else unname(stats::quantile(x, .95))
      }
      gapc <- stacked %>%
        filter(!is.na(exp_hybrid_fte), !is.na(exp_hybrid_fte_obs)) %>%
        mutate(.cr = exp_hybrid_fte - exp_hybrid_fte_obs) %>%
        group_by(female) %>%
        summarise(n = n(),
                  mean_gap_credit = round(mean(.cr), 4),
                  pct_any = round(100 * mean(.cr > 1e-8), 1),
                  mean_if_any = round(
                    if (any(.cr > 1e-8)) mean(.cr[.cr > 1e-8]) else 0, 4),
                  p95_if_any = round(q95_pos(.cr), 4),
                  .groups = "drop")
      cat(sprintf(paste("\nThe gap credit, weight held fixed, exp_hybrid_fte",
                        "less exp_hybrid_fte_obs,\nby sex, on the %s stacked",
                        "person-waves the arms are fitted to:\n"),
                  format(nrow(stacked), big.mark = ",")))
      print(as.data.frame(gapc), row.names = FALSE)
      write.csv(gapc, file.path(OUTDIR, "T8a_gap_credit.csv"),
                row.names = FALSE)
      cat("mean_gap_credit is over everyone; the last two columns are over the\n")
      cat("rows that carry a credit, which is where the magnitude lives.\n")
      gc_m <- gapc$mean_gap_credit[gapc$female == 0]
      gc_f <- gapc$mean_gap_credit[gapc$female == 1]
      if (length(gc_m) == 1 && length(gc_f) == 1) {
        cat(sprintf("Mean credit: men %+.4f years, women %+.4f, difference %+.4f.\n",
                    gc_m, gc_f, gc_f - gc_m))
        cat("Any contrast between a gap-blind stock and an interpolated one\n")
        cat("carries that difference as well as the weight, which is why the\n")
        cat("reported contrasts below declare what each of them varies.\n")
        note(sprintf(paste("T8a: the interpolated gap credit differs by %+.4f",
                           "years between the sexes on the T8 sample, so a",
                           "calendar-against-interpolated contrast mixes the",
                           "weight with the gap treatment. Report",
                           "gap-consistent pairs only."), gc_f - gc_m))
      }
    }
    
    # -----------------------------------------------------------------
    # DIVERGENCE, PER ARM AND PER SEX, AND THE GATE (F1)
    #
    # The gap can only move if the treatments diverge, and it can only move
    # by sex if they diverge differently by sex, so the ratio is computed
    # for each candidate treatment against the calendar baseline within sex,
    # and the binding value is the smaller of the two sexes. Each contrast
    # below is then gated on the divergence of ITS OWN arm.
    # -----------------------------------------------------------------
    div_cand <- c(one_obs = "exp_hybrid_one_obs",
                  one_int = "exp_hybrid_one_int",
                  fte_obs = "exp_hybrid_fte_obs",
                  fte_int = "exp_hybrid_fte")
    div_cand <- div_cand[div_cand %in% names(stacked)]
    if (!length(div_cand))
      cat("\nNo alternative experience stock is available, so there is no\n",
          "  divergence to measure and no swap to gate. T8 below reduces to\n",
          "  the reproduction check.\n")
    div_tab <- if (!length(div_cand)) data.frame(
      arm = character(0), is_female = numeric(0), div_ratio = numeric(0)
    ) else bind_rows(lapply(names(div_cand), function(a) {
      cc <- unname(div_cand[a])
      stacked %>%
        mutate(.arm = .data[[cc]],
               .div = .data[[cc]] - exp_hybrid_cal) %>%
        group_by(is_female) %>%
        summarise(arm = a, n = n(),
                  sd_cal      = round(stats::sd(exp_hybrid_cal, na.rm = TRUE), 3),
                  sd_arm      = round(stats::sd(.arm, na.rm = TRUE), 3),
                  mean_div    = round(mean(.div, na.rm = TRUE), 3),
                  sd_div      = round(stats::sd(.div, na.rm = TRUE), 3),
                  pct_div_pos = round(100 * mean(.div > 1e-8, na.rm = TRUE), 1),
                  r_cal_arm   = round(suppressWarnings(stats::cor(
                    exp_hybrid_cal, .arm, use = "complete.obs")), 5),
                  .groups = "drop") %>%
        mutate(div_ratio = round(sd_div / sd_cal, 4))
    })) %>% select(arm, is_female, dplyr::everything())
    cat("\nDivergence from the calendar hybrid, by arm and sex:\n")
    if (nrow(div_tab)) print(as.data.frame(div_tab), row.names = FALSE) else
      cat("  (no arm available)\n")
    write.csv(div_tab, file.path(OUTDIR, "T8a_treatment_divergence.csv"),
              row.names = FALSE)
    cat("pct_div_pos is the share of person-waves whose arm stock EXCEEDS the\n")
    cat("calendar one. For a gap-blind one-sided arm that can only be zero;\n")
    cat("anything above zero is the overtime credit or the interpolated gap\n")
    cat("credit, and the arm name says which.\n")
    
    inp <- stacked %>%
      mutate(inpanel = ifelse(exp_hybrid_cal > 0,
                              (exp_hybrid_cal - base_exp_cal) / exp_hybrid_cal,
                              NA_real_)) %>%
      group_by(is_female) %>%
      summarise(n = n(),
                mean_inpanel = round(100 * mean(inpanel, na.rm = TRUE), 1),
                .groups = "drop")
    cat("\nShare of the calendar stock accumulated inside the panel, which is\n")
    cat("the only part any weight can touch:\n")
    print(as.data.frame(inp), row.names = FALSE)
    
    # The gate, per arm.
    div_min <- if (!nrow(div_tab)) data.frame(
      arm = character(0), div_ratio_min = numeric(0), passes = logical(0)
    ) else div_tab %>% group_by(arm) %>%
      summarise(div_ratio_min = min(div_ratio, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(passes = is.finite(div_ratio_min) & div_ratio_min >= DML_DIV_MIN)
    cat(sprintf(paste("\nGate: the binding divergence-to-spread ratio must",
                      "reach %.2f, fixed\nbefore any of these numbers was",
                      "seen.\n"), DML_DIV_MIN))
    print(as.data.frame(div_min), row.names = FALSE)
    gate_ok <- function(a) {
      v <- div_min$passes[div_min$arm == a]
      length(v) == 1 && isTRUE(v)
    }
    gate_val <- function(a) {
      v <- div_min$div_ratio_min[div_min$arm == a]
      if (length(v) == 1) v else NA_real_
    }
    # Computed here rather than after the primary block, so the primary
    # note can stand down when the broader no-gap-consistent-arm note will
    # cover the same fact (F21).
    gap_consistent <- intersect(c("one_obs", "fte_obs"), div_min$arm)
    gc_pass <- if (length(gap_consistent))
      div_min$passes[div_min$arm %in% gap_consistent] else logical(0)
    gate_blocked <- length(gc_pass) && !any(gc_pass)
    
    prim_arm <- if ("one_obs" %in% div_min$arm) "one_obs" else
      if ("fte_obs" %in% div_min$arm) "fte_obs" else NA_character_
    swap_informative <- !is.na(prim_arm) && gate_ok(prim_arm)
    if (!is.na(prim_arm)) {
      cat(sprintf("\nThe PRIMARY arm is %s; its binding ratio is %.4f.\n",
                  prim_arm, gate_val(prim_arm)))
      if (swap_informative) {
        cat("Above the gate: the primary treatments diverge enough for the\n")
        cat("swap to be read as a between-arm contrast.\n")
      } else {
        cat("BELOW THE GATE ON THE PRIMARY ARM. The one-sided stock sits too\n")
        cat("close to the calendar stock for the swap to relocate the gap by\n")
        cat("an interpretable amount, so no units-artefact verdict is issued\n")
        cat("from it, on the same logic that drops an underpowered training\n")
        cat("design rather than reporting its null. Revision 2 cleared the\n")
        cat("gate on the two-sided arm and then issued the verdict on this\n")
        cat("one (F1); that is the error this per-arm gate prevents.\n")
        cat("The binding constraint is the pre-panel base, which no weight\n")
        cat("can touch. Two ways forward, neither a rebuild: restrict to\n")
        cat("person-waves whose in-panel share is high, where the weighting\n")
        cat("bites, and report the swap there; or report the in-panel stock\n")
        cat("alone with a panel-entry control and accept a narrower question.\n")
        # Only when the broader note below will NOT fire, so the decision
        # list does not carry the same fact twice (F21).
        if (!gate_blocked)
          note(sprintf(paste("T8: the PRIMARY one-sided arm diverges from the",
                             "calendar treatment by only %.3f of its spread,",
                             "under the pre-set %.2f, so no units-artefact",
                             "verdict is issued from it."),
                       gate_val(prim_arm), DML_DIV_MIN))
      }
    }
    # -----------------------------------------------------------------
    # WHAT THE GATE TABLE SAYS TAKEN TOGETHER (F14)
    #
    # Only two arms are gap-consistent with the calendar baseline, one_obs
    # and fte_obs, and a units contrast has to be gap-consistent to be a
    # contrast of weights. If neither of those clears the gate then no
    # units verdict is available on this panel at all, whatever the
    # interpolated arms do, and that is a statement about the design rather
    # than a failure of the run. It is printed as one sentence rather than
    # left for the reader to assemble from the table above.
    # -----------------------------------------------------------------
    if (gate_blocked) {
      cat("\nNO GAP-CONSISTENT CONTRAST CLEARS THE GATE. The only arms whose\n")
      # sprintf, not cat's argument list, so no separator space appears
      # before the comma (F23). And "on the full sample", because the remedy
      # test below may find a subsample where the gate clears (F21).
      cat(sprintf(paste("weights are %s, and both are below the pre-set %.2f.",
                        "So T8 cannot\nanswer the units question on the full",
                        "sample.\n"),
                  paste(gap_consistent, collapse = " and "), DML_DIV_MIN))
      if (exists("inp") && nrow(inp))
        cat(sprintf(paste("The binding constraint is stated two blocks above:",
                          "the weight touches only\nthe in-panel part of the",
                          "stock, and that is %.0f%% of it for men and %.0f%%",
                          "for\nwomen. A weight cannot move what it cannot",
                          "reach.\n"),
                    inp$mean_inpanel[inp$is_female == 0],
                    inp$mean_inpanel[inp$is_female == 1]))
      cat("Report this as a design limit with the two numbers, not as a null.\n")
      note(paste("T8: no gap-consistent contrast clears the divergence gate",
                 "on the full sample, so the swap cannot relocate the gap by a",
                 "readable amount. The weight reaches only about a third of",
                 "the stock. Report the limit and the in-panel share, not a",
                 "null."))
    } else if (length(gc_pass) && any(gc_pass)) {
      cat("\nAt least one gap-consistent arm clears the gate, so a units\n")
      cat("contrast is available. It is the one to lead with.\n")
    }
    
    # -----------------------------------------------------------------
    # IS THE REMEDY THE SCRIPT SUGGESTS ACTUALLY AVAILABLE? (F14)
    #
    # The obvious response to a weight that cannot reach most of the stock
    # is to restrict to person-waves where it can. Revision 3 suggested
    # that and left it as advice. Advice that is one group_by away from
    # being a result should be the result: the divergence ratio is
    # recomputed inside the high in-panel-share subgroup, so the suggestion
    # either rescues T8 on a stated subsample or is closed.
    #
    # Note what the restriction costs, beyond sample size. In-panel share
    # rises with time observed and falls with age at entry, so the retained
    # subsample is younger at entry and longer observed, and its return to
    # experience need not be the population's. That is a caveat on the
    # subsample estimate rather than a reason not to compute it.
    # -----------------------------------------------------------------
    REMEDY_GATE_OK <- FALSE; REMEDY_N <- NA_integer_
    if (all(c("base_exp_cal", "exp_hybrid_cal") %in% names(stacked)) &&
        length(div_cand)) {
      st_ip <- stacked %>%
        mutate(.ip = ifelse(exp_hybrid_cal > 0,
                            (exp_hybrid_cal - base_exp_cal) / exp_hybrid_cal,
                            NA_real_))
      thr <- suppressWarnings(unname(stats::quantile(st_ip$.ip, IP_KEEP_Q,
                                                     na.rm = TRUE)))
      if (is.finite(thr)) {
        st_hi <- st_ip %>% filter(!is.na(.ip), .ip >= thr)
        cat(sprintf(paste("\nRemedy test: restricting to the top %.0f%% on",
                          "in-panel share, a\nthreshold of %.2f, keeps %s of",
                          "%s person-waves.\n"),
                    100 * (1 - IP_KEEP_Q), thr,
                    format(nrow(st_hi), big.mark = ","),
                    format(nrow(st_ip), big.mark = ",")))
        if (nrow(st_hi) >= 500) {
          # slice to the BINDING sex rather than summarising, so sd_cal and
          # sd_div survive into the table (F19). The ratio is sd_div / sd_cal
          # and the subsample is selected on having little pre-panel base, so
          # some of any improvement is sd_cal falling rather than the
          # treatments moving apart, and sd_cal falling is less treatment
          # variation, which is precision. Both components and their change
          # against the full sample are reported so the reader can tell which.
          div_hi <- bind_rows(lapply(names(div_cand), function(a) {
            cc <- unname(div_cand[a])
            st_hi %>%
              mutate(.arm = .data[[cc]],
                     .div = .data[[cc]] - exp_hybrid_cal) %>%
              group_by(is_female) %>%
              summarise(arm = a, n = n(),
                        sd_cal = stats::sd(exp_hybrid_cal, na.rm = TRUE),
                        sd_div = stats::sd(.div, na.rm = TRUE),
                        .groups = "drop") %>%
              mutate(div_ratio = sd_div / sd_cal)
          })) %>%
            group_by(arm) %>%
            slice(which.min(div_ratio)) %>%
            ungroup() %>%
            left_join(div_tab %>% group_by(arm) %>%
                        slice(which.min(div_ratio)) %>% ungroup() %>%
                        select(arm, sd_cal_full = sd_cal,
                               sd_div_full = sd_div,
                               div_ratio_full = div_ratio),
                      by = "arm") %>%
            mutate(sd_cal_x = round(sd_cal / sd_cal_full, 3),
                   sd_div_x = round(sd_div / sd_div_full, 3),
                   div_ratio_min = round(div_ratio, 4),
                   passes = is.finite(div_ratio) & div_ratio >= DML_DIV_MIN) %>%
            select(arm, n, div_ratio_full, div_ratio_min, sd_cal_x, sd_div_x,
                   passes) %>%
            arrange(dplyr::desc(div_ratio_min))
          cat("Binding divergence ratio inside that subgroup:\n")
          print(as.data.frame(div_hi), row.names = FALSE)
          write.csv(div_hi, file.path(OUTDIR, "T8a_remedy_high_inpanel.csv"),
                    row.names = FALSE)
          cat("sd_cal_x and sd_div_x are the subgroup values divided by the\n")
          cat("full-sample ones. The ratio improves if sd_div_x exceeds\n")
          cat("sd_cal_x, and where that happens because sd_cal_x is well below\n")
          cat("one the gate is being cleared by removing calendar variation,\n")
          cat("which is the same variation the coefficient is identified from.\n")
          if (any(div_hi$sd_cal_x < 0.9, na.rm = TRUE) &&
              all(div_hi$sd_div_x > 0.9, na.rm = TRUE))
            cat("On this run sd_cal falls while sd_div holds, so the improvement\n",
                "  is mostly the denominator. Expect wider intervals there for\n",
                "  the same reason the ratio improves.\n")
          gc_hi <- div_hi$passes[div_hi$arm %in% gap_consistent]
          # THE VERDICT IS DEFERRED, NOT ISSUED HERE (F19). Clearing the gate
          # is one of the two preconditions; the other is that the baseline
          # gap be distinguishable from zero, and that cannot be known until
          # the arms are fitted below. Revision 3.1 wrote a decision note
          # here telling the reader to re-run the arms on this subsample, and
          # on this panel that re-run would have been refused by
          # swap_verdict for want of a baseline gap.
          REMEDY_GATE_OK <- length(gc_hi) && any(gc_hi)
          REMEDY_N <- nrow(st_hi)
          if (REMEDY_GATE_OK) {
            cat("A gap-consistent arm DOES clear the gate on this subsample, so\n")
            cat("the first of the two preconditions for a units verdict is met\n")
            cat("there. Whether the second is met, a baseline gap to relocate,\n")
            cat("is settled after the arms are fitted and is reported in the\n")
            cat("two-blocker block below. Do not act on this table alone.\n")
          } else {
            cat("No gap-consistent arm clears the gate even there, so the\n")
            cat("remedy is closed rather than untried. Do not suggest it in\n")
            cat("the text: the pre-panel base dominates the stock even among\n")
            cat("the person-waves where it dominates least, and the swap needs\n")
            cat("a panel that observes more of each working life, not a\n")
            cat("different restriction on this one.\n")
            note(paste("T8: restricting to high in-panel share does not rescue",
                       "the units contrast either, so the remedy is closed.",
                       "The swap needs longer panel coverage, not another",
                       "sample rule."))
          }
        } else {
          cat("That subgroup is too thin to recompute the divergence on.\n")
        }
      }
    }
    
    if (exists("n_wage_m") && is.finite(n_wage_m) && MS_DML_OBS > n_wage_m) {
      cat("  Separately from the gate: no arm can be read as a check on the\n")
      cat("  published gap. The sample reconciliation above closed that off,\n")
      cat("  because the published row count exceeds this panel's supply. The\n")
      cat("  between-arm contrasts are fitted on one common sample, enforced\n")
      cat("  below, and they stand; the level of any single arm does not.\n")
    }
    
    # Renamed from dm, which collided with the men's end-to-end drift movement
    # in the T2 placebo block (F12).
    demean_by_couple <- function(d, vars) {
      d %>% group_by(couple_id) %>%
        mutate(across(all_of(vars), ~ .x - mean(.x, na.rm = TRUE),
                      .names = "{.col}_w")) %>%
        ungroup()
    }
    # v21.20. WITHIN-PERSON, which the within-COUPLE arms do not deliver.
    #
    # C5 puts OLS and DML side by side and on the v21.14g run they
    # disagreed in SIGN on two of four cells: childless men +0.182 under
    # OLS against -0.133 under DML, childless women +0.139 against -0.306.
    # The first candidate named was that the DML carries no person effect
    # while experience_years holds the entry-cohort recall vintage as a
    # PERSON-CONSTANT term.
    #
    # WHY THE EXISTING _w ARMS DO NOT ANSWER IT. Couple demeaning removes
    # what is constant WITHIN A COUPLE. A vintage attached to each
    # partner's own panel entry is not: partners who joined in different
    # waves keep a within-couple difference in it, and the loader measures
    # that gradient at one to two years at the same age. So the couple
    # arms leave exactly the term in question.
    #
    # AND THE MASTER HAS ALREADY SHOWN THIS MATTERS ON THIS DATA. Its
    # Eq. 4 pair, pooled against person fixed effects, flips the sign of
    # the men's parenthood interaction: -0.0887 significant under OLS,
    # +0.0097 with p = 0.75 under FE. Pooled and within are measuring
    # different things here, which makes the C5 disagreement plausibly the
    # same phenomenon rather than an estimator failure. This tests it.
    #
    # TWO TRANSFORMATIONS, because they answer slightly different
    # questions and the pair is more informative than either.
    #
    #   _wp  WITHIN-PERSON. Demean y, D and the continuous controls by
    #        person. Removes every person-constant term including the
    #        vintage. Costs the between-person variation entirely, and
    #        single-observation persons contribute nothing and are dropped.
    #
    #   _mk  MUNDLAK. Keep the levels and ADD each person's own mean of D
    #        and of the controls as extra regressors. Controls the person
    #        effect without discarding between variation, and it suits DML
    #        better than the within transform does, because the ML learner
    #        sees the means as ordinary features rather than having the
    #        transformation imposed on the nuisance functions.
    #
    # If theta moves toward the OLS sign under EITHER, the disagreement is
    # person-level confounding. If it moves under NEITHER, the person
    # effect is not the explanation and the sensitivity route from
    # Ichimura's 2025 lecture is the next step.
    demean_by_person <- function(d, vars) {
      key <- if ("person_id" %in% names(d)) "person_id" else "person_couple"
      d %>% group_by(.data[[key]]) %>%
        mutate(n_obs_person = dplyr::n(),
               across(all_of(vars), ~ .x - mean(.x, na.rm = TRUE),
                      .names = "{.col}_wp")) %>%
        ungroup()
    }
    mundlak_means <- function(d, vars) {
      key <- if ("person_id" %in% names(d)) "person_id" else "person_couple"
      d %>% group_by(.data[[key]]) %>%
        mutate(across(all_of(vars), ~ mean(.x, na.rm = TRUE),
                      .names = "{.col}_mk")) %>%
        ungroup()
    }
    
    # married is not demeaned: both partners of a couple are married, so the
    # demeaned column would be identically zero (F11).
    # v21.17a FIX. ctrl_w asks for experience_is_proxy_w and nothing
    # builds it: the _w columns come from demean_by_couple(stacked_cc,
    # w_vars), and experience_is_proxy was never in w_vars. intersect()
    # then drops the name silently, which is why the run's within-couple
    # control list prints without it while the pooled one has the
    # undemeaned version.
    #
    # Same defect as promotion_training in the loader's select: a name
    # requested in one place, constructed in none, and dropped by a
    # tolerant intersect rather than raised as an error. G4 specified
    # this control for BOTH arms, and it reached one.
    w_vars <- intersect(c("ln_wage", "educ", "exp_pub", "exp_hybrid_cal",
                          "experience_is_proxy",
                          "exp_hybrid_fte", "exp_hybrid_fte_obs",
                          "exp_hybrid_one_obs", "exp_hybrid_one_int",
                          "hours", "age_sq", "fulltime",
                          "p_exp_pub", "p_exp_cal", "p_exp_fte",
                          "p_exp_fte_obs", "p_exp_one_obs", "p_exp_one_int",
                          "p_hours", "p_fulltime"), names(stacked))
    
    # -----------------------------------------------------------------
    # ARMS, AND ONE COMMON SAMPLE FOR ALL OF THEM (F3)
    #
    # Revision 2 asserted in prose that the between-arm contrasts were
    # "computed on identical rows" while run_dml applied complete.cases
    # separately inside each arm over that arm's own variable set. Whether
    # the claim held was left to whichever missingness patterns happened to
    # coincide. It is now constructed: the complete-case mask is taken once
    # over the union of every arm's variables, before any fit, and the
    # within-couple demeaning is applied AFTER that restriction so the
    # couple means belong to the estimation sample and not to a superset.
    # -----------------------------------------------------------------
    arm_spec <- list(
      pub     = list(col = "exp_pub",            p = "p_exp_pub",
                     tag = "betaX published units"),
      cal     = list(col = "exp_hybrid_cal",     p = "p_exp_cal",
                     tag = "betaX calendar hybrid, gap-blind"),
      one_obs = list(col = "exp_hybrid_one_obs",  p = "p_exp_one_obs",
                     tag = "betaX one-sided FTE, gap-blind"),
      one_int = list(col = "exp_hybrid_one_int",  p = "p_exp_one_int",
                     tag = "betaX one-sided FTE, interpolated"),
      fte_obs = list(col = "exp_hybrid_fte_obs", p = "p_exp_fte_obs",
                     tag = "betaX two-sided FTE, gap-blind"),
      fte_int = list(col = "exp_hybrid_fte",     p = "p_exp_fte",
                     tag = "betaX two-sided FTE, interpolated"))
    arms <- intersect(T8_ARMS, names(arm_spec))
    arms <- arms[vapply(arms, function(a)
      arm_spec[[a]]$col %in% names(stacked), logical(1))]
    dropped_arms <- setdiff(T8_ARMS, arms)
    if (length(dropped_arms))
      cat("\nArms requested but unavailable in this build:",
          paste(dropped_arms, collapse = ", "), "\n")
    
    # v21.17. THE PROVENANCE CONTROL, specified in the project's own
    # analysis notes (G4) and never added.
    #
    # G4: "DML-PLR has no person fixed effect, and the hybrid stock's base
    # term is experience_years at the person's first observed wave, which
    # is exactly the pre-panel history the fallback imputes. The proxy
    # therefore enters a level comparison across people with nothing
    # absorbing it, and IF THE PROXY SHARE DIFFERS BY SEX the swap is
    # contaminated on the arm that matters."
    #
    # THAT CONDITION IS NOW MEASURED AND MET. The v21.11 loader run puts
    # the Mincer proxy on 5.2% of men's person-waves and 4.4% of women's,
    # and the two provenances have visibly different experience densities:
    # the proxy peaks near ten years with almost no mass below five, where
    # ehtjb has its heaviest mass at nought to five and a long right tail.
    # Potential experience assumes continuous work, so it cannot represent
    # an interruption at all.
    #
    # Ichimura's 2025 Frisch lecture frames why this has to be done by
    # hand: "Introducing conditioning variables involve decisions about
    # which variables to include and how. Some of the ML methods solve
    # both questions at the same time." DML solves the HOW. It does not
    # solve the WHICH, and a control absent from this vector is absent
    # from the estimate however flexible the learner is.
    #
    # This is also the cheapest available test of the C5 sign
    # disagreement. If adding one indicator moves theta materially, the
    # disagreement is confounding rather than an estimator failure.
    `%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a
    ctrl_pool <- intersect(c("educ", "hours", "age_sq", "married", "fulltime",
                             "has_children", "wave", "p_educ", "p_hours",
                             "p_fulltime",
                             "experience_is_proxy", "p_experience_is_proxy"),
                           names(stacked))
    if (!"experience_is_proxy" %in% names(stacked))
      cat("\n  NOTE: experience_is_proxy is not on the stacked frame, so the\n",
          "  provenance control G4 specifies is NOT in the DML control set.\n",
          "  Add it to the his_data/her_data transmute() to enable it; the\n",
          "  proxy share differs by sex (5.2% against 4.4%) and the proxy\n",
          "  cannot represent an interruption, which is the quantity these\n",
          "  arms are about.\n", sep = "")
    arm_cols <- unlist(lapply(arms, function(a)
      c(arm_spec[[a]]$col, arm_spec[[a]]$p)))
    need_cc <- intersect(unique(c("ln_wage", "is_female", "couple_id",
                                  ctrl_pool, arm_cols)), names(stacked))
    # v21.32. REPORT THE SEX ASYMMETRY IN COMPLETE CASES.
    #
    # stacked is bind_rows(his_data, her_data), each a transmute of the
    # SAME dual_earner frame, so it starts with exactly equal halves and
    # an EVEN total. The v21.31 run reported his n = 25,870 against her
    # n = 26,779, a total of 52,649 -- an ODD number, so 909 more men than
    # women are being dropped by the control set.
    #
    # That means the his and her arms are estimated on DIFFERENT couple
    # sets, and the gap carries a composition difference on top of the
    # price difference it is meant to measure. It also undercuts the
    # independence note attached to every gap SE: the two arms are not
    # merely correlated, they are not the same couples.
    #
    # Reported rather than repaired, because the fix depends on the
    # cause. If one control drives it, drop that control. If it is spread
    # across many, complete-case on the COUPLE (drop both partners when
    # either is incomplete), which restores symmetry at the cost of rows.
    .cc_all <- stats::complete.cases(stacked[, need_cc, drop = FALSE])
    .by_sex <- table(stacked$is_female[!.cc_all])
    cat(sprintf(paste("\n  Complete cases: %s of %s rows. Dropped by sex:",
                      "men %s, women %s.\n"),
                format(sum(.cc_all), big.mark = ","),
                format(nrow(stacked), big.mark = ","),
                format(unname(.by_sex["0"]) %||% 0L, big.mark = ","),
                format(unname(.by_sex["1"]) %||% 0L, big.mark = ",")))
    # v21.34: the v21.32 run returned "52,649 of 52,649, dropped men 0,
    # women 0", so complete-casing is NOT the source of the 25,870/26,779
    # split. The stacked frame is unbalanced BEFORE any filtering, which
    # points at the partner link: 11.64% of partnered person-waves name a
    # partner who does not appear in the same wave, so a person-wave can
    # enter without its counterpart. Hypothesis tested and disproved;
    # the check stays because it is what disproved it.
    if (length(.by_sex) == 2L && min(.by_sex) > 0 &&
        abs(diff(as.numeric(.by_sex))) > 0.05 * min(as.numeric(.by_sex))) {
      cat("  *** ASYMMETRIC. The stacked frame starts with equal halves, so\n")
      cat("      the his and her arms below are on DIFFERENT couple sets.\n")
      cat("      Per-control missingness among the dropped rows:\n")
      .inc <- stacked[!.cc_all, need_cc, drop = FALSE]
      .miss <- sort(vapply(.inc, function(z) sum(is.na(z)), integer(1)),
                    decreasing = TRUE)
      .miss <- .miss[.miss > 0]
      for (nm in names(utils::head(.miss, 5)))
        cat(sprintf("        %-24s %s\n", nm,
                    format(.miss[[nm]], big.mark = ",")))
      cat("      If ONE control dominates, drop it. If the loss is spread,\n")
      cat("      complete-case on the COUPLE so both partners go together.\n")
      rm(.inc, .miss)
    }
    rm(.cc_all, .by_sex)
    keep_cc <- stats::complete.cases(
      as.data.frame(stacked)[, need_cc, drop = FALSE])
    stacked_cc <- stacked[keep_cc, , drop = FALSE]
    cat(sprintf(paste("\nCommon complete-case sample across all %d arms: %s of",
                      "%s\nperson-waves (%.1f%%), %s couples. Every arm below",
                      "is fitted on exactly\nthese rows.\n"),
                length(arms), format(nrow(stacked_cc), big.mark = ","),
                format(nrow(stacked), big.mark = ","),
                100 * nrow(stacked_cc) / max(1L, nrow(stacked)),
                format(dplyr::n_distinct(stacked_cc$couple_id), big.mark = ",")))
    if (nrow(stacked_cc) < nrow(stacked))
      cat("  The restriction is the union over arms, so it binds on the\n",
          "  baseline arm too. That is the price of a like-for-like contrast.\n")
    
    # v21.45. The frame every arm is fitted on, persisted BEFORE the first
    # fit. Without this it exists only in the session that built it, so a
    # screen or a diagnostic in another session has to re-run the arms to
    # get at it. res at 1549 is the results list and is no substitute.
    #
    # v21.46. STAMPED, for the same reason td_frame() stamps the panel. This
    # frame is built by joining the addendum onto the panel, so a 02 re-run
    # makes it stale while leaving it readable, and a reader in another
    # session gets pre-rebuild numbers with nothing to warn them. The
    # addendum's path and mtime ride along and the consumer checks them.
    if (!isTRUE(get0("TD_NO_WRITE", ifnotfound = FALSE))) {
      .cc_out <- stacked_cc
      attr(.cc_out, "addendum_path")  <- add_path
      attr(.cc_out, "addendum_mtime") <- if (file.exists(add_path))
        file.info(add_path)$mtime else NA
      attr(.cc_out, "panel_build_tag") <- attr(panel, "build_tag")
      attr(.cc_out, "written_at")     <- Sys.time()
      saveRDS(.cc_out, file.path(OUTDIR, "T8_stacked_cc.rds"))
      rm(.cc_out)
    }
    
    stacked_w <- demean_by_couple(stacked_cc, w_vars)
    # The within-couple arm demeans by couple, which absorbs anything
    # constant within a couple. The proxy indicator is NOT: it is a
    # property of each partner's own entry and history, so it varies
    # within couple and belongs here too.
    ctrl_w <- intersect(c("experience_is_proxy_w", "educ_w", "hours_w",
                          "age_sq_w", "fulltime_w",
                          "has_children", "wave", "p_hours_w", "p_fulltime_w"),
                        names(stacked_w))
    
    # ---------------------------------------------------------------
    # DML-PLR, matching the published settings: random-forest nuisance
    # with 500 trees, five folds, three repetitions, partialling-out
    # score, seed 42. The learner and repetition count are configurable
    # only so a fast lasso pass can be run first; the reported numbers
    # should come from the random-forest settings.
    # ---------------------------------------------------------------
    run_dml <- function(data, label, treatment, outcome, controls) {
      all_vars <- unique(c(outcome, treatment, controls))
      all_vars <- intersect(all_vars, names(data))
      dat <- data %>% select(all_of(all_vars)) %>% filter(complete.cases(.))
      if (nrow(dat) < 300 || !(treatment %in% names(dat))) {
        cat(sprintf("  %-34s N=%s too small or treatment absent, skipped\n",
                    label, format(nrow(dat), big.mark = ",")))
        return(NULL)
      }
      ctrl <- setdiff(all_vars, c(outcome, treatment))
      if (stats::sd(dat[[treatment]], na.rm = TRUE) == 0) {
        cat(sprintf("  %-34s treatment is constant, skipped\n", label))
        return(NULL)
      }
      dml_data <- DoubleML::DoubleMLData$new(as.data.frame(dat),
                                             y_col = outcome,
                                             d_cols = treatment,
                                             x_cols = ctrl)
      if (DML_FAST) {
        ml_l <- mlr3::lrn("regr.cv_glmnet", s = "lambda.min", alpha = 1)
        ml_m <- mlr3::lrn("regr.cv_glmnet", s = "lambda.min", alpha = 1)
      } else {
        ml_l <- mlr3::lrn("regr.ranger", num.trees = 500, min.node.size = 5,
                          seed = 42, num.threads = n_threads)
        ml_m <- mlr3::lrn("regr.ranger", num.trees = 500, min.node.size = 5,
                          seed = 43, num.threads = n_threads)
      }
      fit <- tryCatch({
        set.seed(42)
        m <- DoubleML::DoubleMLPLR$new(dml_data, ml_l, ml_m,
                                       n_folds = DML_FOLDS, n_rep = DML_REPS,
                                       score = "partialling out")
        m$fit(); m
      }, error = function(e) {
        cat(sprintf("  %-34s ERROR: %s\n", label, conditionMessage(e))); NULL
      })
      if (is.null(fit)) return(NULL)
      sd_d <- stats::sd(dat[[treatment]], na.rm = TRUE)
      cat(sprintf("  %-34s theta=%+.4f%%  SE=%.4f  n=%s  sd(D)=%.2f\n",
                  label, fit$coef * 100, fit$se * 100,
                  format(nrow(dat), big.mark = ","), sd_d))
      list(theta = unname(fit$coef), se = unname(fit$se), n = nrow(dat),
           sd_d = sd_d)
    }
    
    gender_gap <- function(frame, treat, outcome, ctrls, tag) {
      his <- run_dml(frame %>% filter(is_female == 0), paste("His", tag),
                     treat, outcome, ctrls)
      her <- run_dml(frame %>% filter(is_female == 1), paste("Her", tag),
                     treat, outcome, ctrls)
      if (is.null(his) || is.null(her)) return(NULL)
      gap <- his$theta - her$theta
      # TWO ASSUMPTIONS IN THIS STANDARD ERROR, BOTH STATED (F8).
      #
      # First, sqrt(se_his^2 + se_her^2) treats the two arms as independent.
      # They are estimated on disjoint sexes, but a man and his partner are
      # the same couple, so the two estimates are correlated through the
      # household and the true standard error on the difference is not this
      # one. The sign of the error depends on the sign of that correlation,
      # so it cannot even be signed as conservative.
      #
      # Second, DoubleMLData is constructed without a cluster column, so
      # neither arm's own standard error accounts for repeated person-waves
      # on the same person. Both are understated by whatever the
      # within-person correlation is.
      #
      # Revision 2 printed the p-value with neither caveat attached, next to
      # a published p-value computed some other way. Read the gap and the
      # movement between arms; do not read this p-value as a test.
      se  <- sqrt(his$se^2 + her$se^2)
      z   <- gap / se
      p   <- 2 * stats::pnorm(-abs(z))
      # The same gap expressed per standard deviation of the treatment. A
      # coefficient in log points per calendar year and one in log points per
      # FTE year are not directly comparable, because the unit itself has
      # changed. The scaling uses the mean of the two sexes' spreads, so it
      # is a common rescaling of the gap rather than a per-sex one, and when
      # sd(D) barely moves across arms it multiplies every arm by nearly the
      # same constant and adds nothing (F9). Whether it moves is checked
      # after the arms are in, not here.
      sdm <- mean(c(his$sd_d, her$sd_d))
      cat(sprintf("    %-26s gap=%+.4f%% (SE %.4f, p=%.4f) | per sd: %+.4f%%\n",
                  tag, gap * 100, se * 100, p, gap * sdm * 100))
      cat("      SE assumes the two sexes are independent, which couples are\n")
      cat("      not, and no arm is clustered on the person. See F8.\n")
      list(tag = tag, his = his$theta * 100, her = her$theta * 100,
           gap = gap * 100, se = se * 100, p = p,
           gap_per_sd = gap * sdm * 100, sd_d = sdm,
           sd_his = his$sd_d, sd_her = her$sd_d,
           n_his = his$n, n_her = her$n)
    }
    
    cat("\nControls, pooled:", paste(ctrl_pool, collapse = ", "), "\n")
    cat("Controls, within :", paste(ctrl_w, collapse = ", "), "\n")
    cat("The partner experience control is added per arm in its own units, so\n")
    cat("no arm carries a partner control measured in another arm's units.\n")
    
    res <- list()
    cat("\nPooled:\n")
    for (a in arms) {
      sp <- arm_spec[[a]]
      res[[paste0(a, "_pool")]] <- gender_gap(
        stacked_cc, sp$col, "ln_wage",
        c(ctrl_pool, if (sp$p %in% names(stacked_cc)) sp$p else NULL), sp$tag)
    }
    if (DML_WITHIN) {
      cat("\nWithin-couple:\n")
      for (a in arms) {
        sp <- arm_spec[[a]]
        tc <- paste0(sp$col, "_w"); pc <- paste0(sp$p, "_w")
        if (!(tc %in% names(stacked_w))) next
        # v21.20: the two person-level arms, run on the same rows with the
        # same learners and folds so the ONLY difference is the transform.
        # v21.34. THE WITHIN-PERSON ARM IS NOT IDENTIFIED, and the run
        # that dropped the collinear stocks proves it rather than fixing
        # it. Reported and NOT used.
        #
        # After dropping the 13 other experience stocks the two person-
        # level arms still disagree by a factor of eight:
        #   within person  gap +2.3205  p 0.0000  sd(D) 2.82  theta 6.12%
        #   Mundlak        gap +0.2784  p 0.3350  sd(D) 9.53  theta 2.16%
        # In a linear model these estimate the SAME coefficient. A 6.12%
        # annual return to experience is also three to five times the
        # Mincer literature, which is a magnitude check failing on its
        # own.
        #
        # THE CAUSE IS STRUCTURAL. Within a person, experience and age
        # advance together by construction: for anyone employed
        # throughout, exp(t) - mean(exp) equals age(t) - mean(age)
        # exactly. Simulated on a panel with 12% interruption:
        # corr(exp, age) is 0.11 in LEVELS and 0.98 WITHIN PERSON, with
        # corr(exp, age_sq) at 0.97. And age_sq is one of the six
        # surviving controls.
        #
        # So the demeaned treatment is nearly collinear with a control,
        # DML partials that control out of it, and what remains is the
        # residual deviation from lockstep growth - which IS the
        # interruption pattern. sd(D) collapsing 9.53 to 2.82, a 70%
        # loss, is that happening; the simulation reproduces 68%.
        #
        # The arm therefore estimates the wage return to INTERRUPTION,
        # not to experience, on a sample where interruption is what
        # distinguishes the sexes. That is the quantity the paper is
        # about measured as though it were a control, and it is why the
        # gap is large and significant here and nowhere else.
        #
        # This is not fixable by respecification: it is the age-period
        # problem, and a person effect cannot separate experience from
        # age when one is a deterministic function of the other. Mundlak
        # keeps the between variation and CAN separate them, which is why
        # the two disagree and why Mundlak is the one to read.
        .wp_frame <- demean_by_person(stacked, w_vars) %>%
          filter(n_obs_person > 1L)
        .mk_frame <- mundlak_means(stacked, w_vars)
        .n_drop <- nrow(stacked) - nrow(.wp_frame)
        if (.n_drop > 0)
          cat(sprintf(paste("  Within-person: %s of %s rows are",
                            "single-observation persons and\n  contribute",
                            "nothing after demeaning; dropped.\n"),
                      format(.n_drop, big.mark = ","),
                      format(nrow(stacked), big.mark = ",")))
        # v21.20a. THE CONTROL SET CANNOT CARRY FIVE COPIES OF THE
        # TREATMENT ONCE IT IS DEMEANED.
        #
        # The first run returned a significant gap on the published-units
        # arm, +2.7042 with p = 0.0016, where every level arm returns
        # nothing. It also returned Mundlak +0.3070 on the same rows and
        # the same question, and in a linear model those two estimate the
        # SAME coefficient. A factor of nine between them, and a sign flip
        # on the calendar arm, says the transform is doing something the
        # theory does not sanction.
        #
        # It is the control set. w_vars carries SIX experience stocks
        # (exp_pub, exp_hybrid_cal, exp_hybrid_fte, exp_hybrid_fte_obs,
        # exp_hybrid_one_obs, exp_hybrid_one_int), their partner
        # equivalents, and age_sq. Every one increments by about 1 a wave
        # for anyone employed throughout, so within a person they are the
        # same time index. Measured on simulated panels: two stocks
        # correlate 0.999 in levels and 1.000 after demeaning, and
        # exp_pub against age_sq goes to 0.87.
        #
        # So the partialling-out step residualises the treatment against
        # five other measures of itself. What survives is the DEVIATION
        # from lockstep growth, which is the interruption pattern -
        # common for women, rare for men - and the arm then reports a
        # large "gender gap in the return to experience" that is a gender
        # difference in interruption. The standard errors show it: his
        # 0.116 in levels against 0.693 within, hers 0.116 against 0.502.
        #
        # The other stocks are dropped from the within control set. They
        # were there to let the level arms adjust for alternative
        # measures; within a person they are the treatment.
        .exp_like <- grep("^(p_)?exp", w_vars, value = TRUE)
        .ctrl_wp <- intersect(paste0(setdiff(w_vars,
                                             c("ln_wage", .exp_like)), "_wp"),
                              names(.wp_frame))
        .ctrl_wp <- setdiff(.ctrl_wp, paste0(arm_spec[[a]]$col, "_wp"))
        cat(sprintf(paste("  Within-person controls: %d (the %d other",
                          "experience stocks are dropped;\n  demeaned they",
                          "are the treatment, not a control).\n"),
                    length(.ctrl_wp), length(.exp_like)))
        # Fitted so the diagnosis is on the record, and labelled so it
        # cannot be quoted as a result. See the note above.
        res[[paste0(a, "_wp")]] <- gender_gap(
          .wp_frame, paste0(arm_spec[[a]]$col, "_wp"), "ln_wage_wp",
          .ctrl_wp,
          paste(arm_spec[[a]]$tag, "[within person: NOT IDENTIFIED]"))
        .sd_lev <- stats::sd(stacked[[arm_spec[[a]]$col]], na.rm = TRUE)
        .sd_wp  <- stats::sd(.wp_frame[[paste0(arm_spec[[a]]$col, "_wp")]],
                             na.rm = TRUE)
        if (is.finite(.sd_lev) && .sd_lev > 0)
          cat(sprintf(paste("    treatment sd %.2f in levels to %.2f",
                            "demeaned, a %.0f%% loss. What\n    survives is",
                            "the deviation from lockstep experience growth,",
                            "which\n    is the interruption. Do not report",
                            "this arm.\n"),
                      .sd_lev, .sd_wp, 100 * (1 - .sd_wp / .sd_lev)))
        rm(.sd_lev, .sd_wp)
        .ctrl_mk <- intersect(c(ctrl_pool,
                                paste0(setdiff(w_vars, "ln_wage"), "_mk")),
                              names(.mk_frame))
        res[[paste0(a, "_mk")]] <- gender_gap(
          .mk_frame, arm_spec[[a]]$col, "ln_wage", .ctrl_mk,
          paste(arm_spec[[a]]$tag, "[Mundlak]"))
        
        res[[paste0(a, "_w")]] <- gender_gap(
          stacked_w, tc, "ln_wage_w",
          c(ctrl_w, if (pc %in% names(stacked_w)) pc else NULL), sp$tag)
      }
    }
    
    got <- Filter(Negate(is.null), res)
    if (length(got)) {
      dml_tbl <- do.call(rbind, lapply(names(got), function(nm) data.frame(
        arm = nm, unit = got[[nm]]$tag,
        his_pct = round(got[[nm]]$his, 4), her_pct = round(got[[nm]]$her, 4),
        gap_pct = round(got[[nm]]$gap, 4), se_pct = round(got[[nm]]$se, 4),
        p_value = round(got[[nm]]$p, 4),
        gap_per_sd_pct = round(got[[nm]]$gap_per_sd, 4),
        sd_treatment = round(got[[nm]]$sd_d, 3),
        n_his = got[[nm]]$n_his, n_her = got[[nm]]$n_her,
        stringsAsFactors = FALSE)))
      cat("\nGender gap in the return to experience, by unit:\n")
      # panel is assigned before the write, so the csv carries it and a
      # reader of the file cannot compare sd(D) across the two designs by
      # accident (F20).
      dml_tbl$panel <- ifelse(grepl("_w$", dml_tbl$arm), "within-couple",
                              "pooled")
      print(dml_tbl, row.names = FALSE)
      write.csv(dml_tbl, file.path(OUTDIR, "T8_dml_swap.csv"), row.names = FALSE)
      saveRDS(res, file.path(OUTDIR, "T8_dml_swap.rds"))

      # v27. THE ARMS ENTER THE LEDGER. Until now this module contributed
      # zero rows to it, not by choice but because report() opens with
      # fixest::coeftable() and these are DoubleML fits, so there was no
      # route in. Section 5 tells the reader the ledger file carries every
      # specification with its status; these twenty were the ones it did
      # not carry, and a thirteen-hour run left no n, SE or MDE anywhere a
      # verifier could check.
      #
      # BENCHMARK IS NA AND THAT IS DELIBERATE. These arms measure the
      # gender difference in the return to experience, in percentage
      # points, so a quarter of a training base rate would grade them
      # against a quantity they do not estimate. The MDE stands on its
      # own; Section 7.3 compares it to the pooled difference in prose.
      #
      # THE FIVE _wp ARMS ARE MARKED, not omitted. sig_tab$identified
      # already excludes them from n_sig because within-person is
      # structurally unidentified here, and a row sitting unlabelled
      # beside the identified arms with p = 0.0000 is exactly the reading
      # that block was written to prevent.
      if (exists("report_manual", mode = "function")) {
        TD03_LEDGER_MODULE <- "03c_experience_units"
        for (.i in seq_len(nrow(dml_tbl))) {
          .a <- dml_tbl$arm[.i]
          .wp <- grepl("_wp$", .a)
          # THE fe COLUMN RECORDS THE SPECIFICATION, NOT THE SAMPLE, and
          # dml_tbl$panel does not. It reads "pooled" for the Mundlak and
          # within-person arms as well as the pooled ones, because all
          # three use the full sample; only the within-couple arms differ
          # on that field. Writing panel into fe would put "pooled"
          # against ten rows whose whole point is that they are not, and
          # the ledger exists to make exactly that distinction checkable.
          # The suffix on the tag is the reliable key.
          .fe <- if (.wp) "within-person (not identified)" else
                 if (grepl("_mk$", .a)) "Mundlak" else
                 if (grepl("_w$", .a))  "within-couple" else "pooled"
          report_manual(
            tag    = paste0("T8_", .a),
            label  = dml_tbl$unit[.i],
            n      = dml_tbl$n_his[.i] + dml_tbl$n_her[.i],
            se     = dml_tbl$se_pct[.i],
            unit   = "pp",
            fe     = .fe,
            # BOTH STRINGS CARRY "post-hoc", and that is load-bearing
            # rather than decorative. Table 6 is the pre-commitment ledger
            # and build_ledger_table() holds a row back by matching
            # "post-hoc" in this field. The first version of this line read
            # "diagnostic (not identified)" for the _wp arms, so the
            # fifteen IDENTIFIED arms were held back and the five
            # unidentified ones were the only T8 rows that reached the
            # page: the exact inverse of what belongs there, and worse
            # than printing none. Section app_arms names all twenty as
            # post-hoc and not tabulated in Table 6; these statuses are
            # what make that true.
            status = if (.wp) "diagnostic (post-hoc, not identified)" else
              "descriptive (post-hoc)",
            quiet  = TRUE)
        }
        cat(sprintf("\n  %d arms added to the ledger (%d marked not identified).\n",
                    nrow(dml_tbl), sum(grepl("_wp$", dml_tbl$arm))))
        rm(.i, .a, .wp, .fe)
      } else {
        cat("\n  report_manual() is absent, so the arms above reach no ledger\n")
        cat("  row and the verifier cannot fingerprint them. Update\n")
        cat("  03_common.R before the run that gets reported.\n")
      }
      # Is gap_per_sd carrying information, or is it the gap times a constant
      # (F9)? If sd(D) hardly moves across arms the column looks like a second
      # piece of evidence and is a rescaling of the first.
      #
      # PER PANEL (F20). Revision 3.1 ran this over every arm at once, so the
      # range ran from the couple-demeaned arms to the pooled ones and
      # reported a spread of more than half. That is the demeaning, which
      # removes between-couple variation, and not the units. Within a panel
      # the arms differ only in the weight and the gap treatment, which is
      # what the check is about.
      for (pn in unique(dml_tbl$panel)) {
        sdv <- dml_tbl$sd_treatment[dml_tbl$panel == pn &
                                      is.finite(dml_tbl$sd_treatment)]
        if (length(sdv) < 2 || max(sdv) <= 0) next
        spread <- (max(sdv) - min(sdv)) / max(sdv)
        cat(sprintf(paste("\nsd(D) across the %s arms: %.2f to %.2f, a spread",
                          "of %.1f%%.\n"), pn, min(sdv), max(sdv),
                    100 * spread))
        if (spread < 0.05) {
          cat("  Under five percent, so gap_per_sd is the gap times a near-\n")
          cat("  constant on this panel and carries nothing the gap column does\n")
          cat("  not. Do not present it as a second result here.\n")
        } else {
          cat("  Large enough that the two columns can rank arms differently,\n")
          cat("  so report gap_per_sd on this panel where units are the point.\n")
        }
      }
      cat("Never compare sd(D) across the two panels: the within-couple arms\n")
      cat("are demeaned, so their spread is smaller for a reason that has\n")
      cat("nothing to do with the weight (F20).\n")
    }
    
    # ---------------------------------------------------------------
    # The comparison the section exists for. The two arms are estimated
    # on the same rows with treatments that are strongly correlated, so
    # their sampling errors are positively correlated and the naive
    # standard error on the difference of the two gaps is too large.
    # That makes the test conservative: a rejection is safe, a failure to
    # reject is not informative on its own, and the size of the movement
    # is the thing to read.
    # ---------------------------------------------------------------
    # varies says what the two sides of the contrast differ in, and the
    # conclusion follows from it (F13):
    #   "weight"  both sides share a gap treatment, so the movement is a
    #             units change and the units verdict is permitted
    #   "gap"     both sides share a weight, so the movement is a gap
    #             treatment; the size is reported and no units verdict is
    #             issued
    #   "both"    the two sides differ in weight AND gap treatment, so the
    #             movement is unattributable and is refused outright
    # Revision 3 had no such argument, and because the only arm clearing the
    # divergence gate on this panel is the interpolated one, the only two
    # contrasts that would have printed a verdict were a "both" pair and a
    # "gap" pair. Both would have been given units language.
    swap_verdict <- function(a, b, label, what = "pooled gap",
                             varies = "weight") {
      if (is.null(a) || is.null(b)) return(invisible(NULL))
      d  <- a$gap - b$gap
      sd_ <- sqrt(a$se^2 + b$se^2)
      cat(sprintf("\n%s\n", label))
      cat(sprintf("  baseline gap %+.4f%% (p=%.3f) -> other arm %+.4f%% (p=%.3f)\n",
                  a$gap, a$p, b$gap, b$p))
      cat(sprintf("  movement %+.4f%%, conservative SE %.4f, |z| <= %.2f\n",
                  d, sd_, abs(d / sd_)))
      # WHAT THIS PAIR CAN SUPPORT, BEFORE ANY NUMBER IS INTERPRETED (F13).
      if (identical(varies, "both")) {
        cat("  The two sides differ in the WEIGHT and in the GAP TREATMENT at\n")
        cat("  once, so no movement between them is attributable to either.\n")
        cat("  REFUSED, and not on the divergence gate: a gate cannot make a\n")
        cat("  two-way movement readable. The pair is here because it is the\n")
        cat("  revision-2 pairing and its size belongs on the record.\n")
        return(invisible(NULL))
      }
      if (identical(varies, "gap")) {
        cat("  Both sides carry the same WEIGHT and differ only in whether\n")
        cat("  interior wave gaps are credited, so this movement is a gap\n")
        cat("  treatment and not a unit. No units-artefact verdict is issued\n")
        cat("  from it. Read it as the size of the confound that sits inside\n")
        cat("  any pairing of a gap-blind stock with an interpolated one, in\n")
        cat("  the units of the gap itself.\n")
        if (abs(a$gap) > 1e-8)
          cat(sprintf("  As a share of the baseline gap: %.0f%%.\n",
                      100 * abs(d / a$gap)))
        return(invisible(NULL))
      }
      if (abs(a$gap) < 1e-8) return(invisible(NULL))
      # A proportional-shrinkage rule is only meaningful if the baseline
      # gap is distinguishable from zero to begin with. Two estimates that
      # are both indistinguishable from zero can differ by any percentage
      # at all, and reading that percentage as evidence would be reading
      # noise. This precondition was missing from the first version of the
      # rule and it is the more important half of it.
      # The criterion is significance, NOT the MDE (R14). An MDE bar asks
      # whether the design could have detected an effect of a given size; it
      # is the wrong question for "is this estimate distinguishable from
      # zero". The two differ by a factor of 1.43, and the difference is not
      # academic here: the PUBLISHED pooled gap of -0.260 has p = 0.016 and
      # an implied SE near 0.108, so its own MDE is about 0.303 and the gap
      # is below it. An MDE gate would therefore refuse to issue a verdict
      # even on a perfect reproduction of the published result.
      if (!is.finite(a$p) || a$p >= ALPHA) {
        cat(sprintf("  The calendar gap is not distinguishable from zero (p = %.3f,\n",
                    a$p))
        cat(sprintf("  MDE %.4f%% for context). No shrinkage\n", mde(a$se)))
        cat("  verdict is issued: a percentage change between two estimates\n")
        cat("  that are both indistinguishable from zero carries no evidence.\n")
        cat("  Reconcile the sample against the published one first; the\n")
        cat("  comparison is only interpretable once the calendar arm\n")
        cat("  reproduces a gap the published specification detects.\n")
        return(invisible(NULL))
      }
      shrink <- 1 - abs(b$gap) / abs(a$gap)
      cat(sprintf("  the gap is %.0f%% %s in FTE units\n", 100 * abs(shrink),
                  if (shrink > 0) "smaller" else "larger"))
      if (sign(a$gap) != sign(b$gap))
        cat("  and it changes sign, which no units argument predicts. Treat\n",
            "  that as a signal to check the treatment construction before\n",
            "  interpreting anything.\n")
      else if (shrink >= DML_SHRINK_MATERIAL)
        cat(sprintf("  which is at or beyond the %.0f%% set in advance, so the\n",
                    100 * DML_SHRINK_MATERIAL))
      else
        cat(sprintf("  which is short of the %.0f%% set in advance, so the\n",
                    100 * DML_SHRINK_MATERIAL))
      if (sign(a$gap) == sign(b$gap)) {
        if (shrink >= DML_SHRINK_MATERIAL)
          cat(sprintf("  %s is substantially an artefact of measuring experience\n  in calendar years, and P3 should say so.\n", what))
        else
          cat(sprintf("  %s is not mainly a units artefact. That strengthens P3\n  rather than weakening it: the gap is not explained by a\n  mother's calendar year holding fewer hours.\n", what))
      }
      invisible(NULL)
    }
    
    # -----------------------------------------------------------------
    # WHICH CONTRASTS ARE READ, AND ON WHOSE DIVERGENCE (F1, F2)
    #
    # Every reported contrast is gap-consistent: both sides share a gap
    # treatment, so the difference between them is the weight and nothing
    # else. Each is gated on the divergence of its own non-baseline arm, so
    # an arm cannot be read on another arm's divergence, which is what
    # revision 2 did.
    #
    # The fte_int against fte_obs row is not a units contrast at all. Both
    # sides carry the same two-sided weight and differ only in whether
    # interior wave gaps are interpolated, so it measures the gap treatment
    # on its own. It is here because it is the size of the confound that was
    # inside revision 2's headline contrast.
    # -----------------------------------------------------------------
    contrast_list <- list(
      list(a = "cal_pool",     b = "one_obs_pool",  arm = "one_obs",
           varies = "weight",
           lab = "POOLED, PRIMARY: calendar against one-sided FTE, both gap-blind",
           what = "pooled gap"),
      list(a = "cal_pool",     b = "fte_obs_pool", arm = "fte_obs",
           varies = "weight",
           lab = "POOLED, secondary: calendar against two-sided FTE, both gap-blind",
           what = "pooled gap"),
      list(a = "cal_pool",     b = "fte_int_pool", arm = "fte_int",
           varies = "both",
           lab = "POOLED, revision-2 pairing: calendar against interpolated FTE. NOT gap-consistent",
           what = "pooled gap"),
      list(a = "one_obs_pool", b = "fte_obs_pool", arm = "fte_obs",
           varies = "weight",
           lab = "POOLED, the overtime credit alone: one-sided against two-sided, both gap-blind",
           what = "pooled gap"),
      list(a = "fte_obs_pool", b = "fte_int_pool", arm = "fte_int",
           varies = "gap",
           lab = "POOLED, gap treatment alone: two-sided gap-blind against two-sided interpolated",
           what = "pooled gap"))
    if (DML_WITHIN)
      contrast_list <- c(contrast_list, list(
        list(a = "cal_w",     b = "one_obs_w",  arm = "one_obs",
             varies = "weight",
             lab = "WITHIN-COUPLE, PRIMARY: calendar against one-sided FTE, both gap-blind",
             what = "within-couple gap"),
        list(a = "cal_w",     b = "fte_obs_w", arm = "fte_obs",
             varies = "weight",
             lab = "WITHIN-COUPLE, secondary: calendar against two-sided FTE, both gap-blind",
             what = "within-couple gap"),
        list(a = "one_obs_w", b = "fte_obs_w", arm = "fte_obs",
             varies = "weight",
             lab = "WITHIN-COUPLE, the overtime credit alone",
             what = "within-couple gap"),
        list(a = "fte_obs_w", b = "fte_int_w", arm = "fte_int",
             varies = "gap",
             lab = "WITHIN-COUPLE, gap treatment alone",
             what = "within-couple gap")))
    
    for (cn in contrast_list) {
      A <- res[[cn$a]]; B <- res[[cn$b]]
      if (is.null(A) || is.null(B)) next
      vr <- if (is.null(cn$varies)) "weight" else cn$varies
      # A pair that varies two things at once is refused on identification
      # grounds and the gate is not consulted, because passing it would not
      # make the movement attributable (F13).
      if (identical(vr, "both")) {
        swap_verdict(A, B, cn$lab, what = cn$what, varies = vr)
        next
      }
      if (!gate_ok(cn$arm)) {
        cat(sprintf("\n%s\n", cn$lab))
        cat(sprintf(paste("  WITHHELD. The %s arm's binding divergence ratio is",
                          "%.4f, under the\n  pre-set %.2f, so a movement",
                          "between these two treatments is not\n  readable.",
                          "Gaps: %+.4f%% and %+.4f%%, on the record.\n"),
                    cn$arm, gate_val(cn$arm), DML_DIV_MIN, A$gap, B$gap))
        next
      }
      swap_verdict(A, B, cn$lab, what = cn$what, varies = vr)
    }
    # Which contrasts were eligible to carry a units verdict at all, stated
    # after the loop so the reader is not left counting (F13).
    elig <- vapply(contrast_list, function(cn) {
      vr <- if (is.null(cn$varies)) "weight" else cn$varies
      identical(vr, "weight") && gate_ok(cn$arm) &&
        !is.null(res[[cn$a]]) && !is.null(res[[cn$b]])
    }, logical(1))
    cat(sprintf(paste("\n%d of %d contrasts were eligible to carry a units",
                      "verdict: a pair has to\nvary the weight alone AND",
                      "clear the divergence gate.\n"),
                sum(elig), length(contrast_list)))
    if (!any(elig))
      cat("None were, so T8 issues no units-artefact verdict this run. The\n",
          "  arms and the movements are on the record; the interpretation is\n",
          "  not available on this panel.\n")
    # -----------------------------------------------------------------
    # THE TWO PRECONDITIONS, AGGREGATED (F18)
    #
    # A units verdict needs both of these and the script tested them in two
    # places without putting them together:
    #   DIVERGENCE  the treatments must be far enough apart for the swap to
    #               relocate the gap. Gated in T8a, per arm.
    #   A GAP       the baseline arm's gender gap must be distinguishable
    #               from zero, because a swap can only shrink something that
    #               is there. Checked inside swap_verdict, per pair, and so
    #               never summarised.
    # They point at different fixes. A divergence failure is about the FTE
    # construction and the panel's coverage of a working life. A missing gap
    # is about the SAMPLE: if the reconstruction does not recover the
    # published gap, there is nothing for any re-measurement to act on, and
    # no weight will create one. Reporting only the first sends the reader to
    # the wrong place.
    # -----------------------------------------------------------------
    sig_tab <- bind_rows(lapply(names(res), function(nm) {
      r <- res[[nm]]
      if (is.null(r)) return(NULL)
      data.frame(arm = nm, gap = round(r$gap, 4), se = round(r$se, 4),
                 p = round(r$p, 4),
                 sig = isTRUE(is.finite(r$p) && r$p < ALPHA),
                 stringsAsFactors = FALSE)
    }))
    if (nrow(sig_tab)) {
      # v21.46. IDENTIFICATION IS PART OF THE PRECONDITION, not a caption.
      # The `_wp` arms are marked NOT IDENTIFIED at L1453: within a person,
      # experience and age advance together by construction, so demeaning
      # leaves a treatment that is age in disguise. They were still counted
      # toward "is there a gap for the swap to act on", and on the 18:21 run
      # they were the ONLY arms counted: five of twenty significant, all
      # five `_wp`. The remedy verdict then read a baseline gap that no
      # identified arm carries, and told the reader to re-run the arms on a
      # subsample to relocate it. n_sig now excludes them and n_sig_all
      # keeps the old count so the difference is visible rather than
      # silently corrected.
      sig_tab$identified <- !grepl("_wp$", sig_tab$arm)
      cat("\nIs there a gap for a swap to act on? Every fitted arm:\n")
      print(as.data.frame(sig_tab), row.names = FALSE)
      write.csv(sig_tab, file.path(OUTDIR, "T8_arm_significance.csv"),
                row.names = FALSE)
      n_sig_all <- sum(sig_tab$sig)
      n_sig     <- sum(sig_tab$sig & sig_tab$identified)
      cat(sprintf(paste("%d of %d arms have a gap distinguishable from zero at",
                        "alpha = %.2f;\np runs %.2f to %.2f.\n"),
                  n_sig_all, nrow(sig_tab), ALPHA, min(sig_tab$p, na.rm = TRUE),
                  max(sig_tab$p, na.rm = TRUE)))
      cat(sprintf(paste("Of those, %d are on IDENTIFIED arms. The within-person",
                        "arms are excluded\nhere for the reason given at their",
                        "construction: the transform makes\nthe treatment",
                        "collinear with age, so a gap they show is not a gap\n",
                        "the swap could relocate.\n"), n_sig))
      if (n_sig_all > 0 && n_sig == 0)
        cat("EVERY significant arm is unidentified. Treat the units question",
            "as\nhaving no gap to act on, whatever the raw count says.\n")
      
      if (n_sig == 0) {
        cat("\nNO ARM HAS A GAP TO RELOCATE. That is a SECOND and INDEPENDENT\n")
        cat("blocker on the units question, and it is the one that points at\n")
        cat("the sample rather than at the FTE construction: the published gap\n")
        cat("is significant and this reconstruction does not recover it, so\n")
        cat("there is nothing for a re-measurement to shrink. No weight can\n")
        cat("create a gap that the sample does not contain.\n")
        if (gate_blocked) {
          cat("BOTH blockers bind on this run. Write the T8 section as two\n")
          cat("statements and not one: the dual-earner reconstruction does not\n")
          cat("recover the published gap, AND separately the weight reaches too\n")
          cat("little of the stock for a swap to move anything. The first has to\n")
          cat("be fixed before the second can be tested.\n")
          note(paste("T8: BOTH preconditions fail. No arm has a gap",
                     "distinguishable from zero, so there is nothing for the",
                     "swap to relocate, and separately no gap-consistent arm",
                     "clears the divergence gate. The missing gap is a sample",
                     "problem and has to be fixed first."))
        } else {
          cat("The divergence gate is satisfied somewhere, so the binding\n")
          cat("constraint is the missing gap and therefore the sample.\n")
          note(paste("T8: no arm has a gap distinguishable from zero, so there",
                     "is nothing for the swap to relocate. The divergence gate",
                     "is not what binds; the sample is."))
        }
      } else if (gate_blocked) {
        cat("\nA gap exists in at least one arm, so the binding constraint is\n")
        cat("divergence alone, and that is about the FTE construction and the\n")
        cat("panel's coverage rather than the sample.\n")
        note(paste("T8: a baseline gap exists but no gap-consistent arm clears",
                   "the divergence gate, so the binding constraint is the FTE",
                   "construction and the panel's in-panel coverage."))
      } else {
        cat("\nBoth preconditions are met somewhere, so a units verdict is\n")
        cat("available. It is in the contrast list above.\n")
      }
      
      # THE REMEDY VERDICT, NOW THAT BOTH PRECONDITIONS CAN BE CHECKED (F19).
      if (exists("REMEDY_GATE_OK")) {
        if (isTRUE(REMEDY_GATE_OK) && n_sig == 0) {
          cat("\nThe remedy test in T8a found a gap-consistent arm clearing the\n")
          cat(sprintf(paste("gate on the top %.0f%% by in-panel share (%s rows).",
                            "That is one\nprecondition, not both.\n"),
                      100 * (1 - IP_KEEP_Q),
                      format(REMEDY_N, big.mark = ",")))
          cat("The baseline gap is not distinguishable from zero on the FULL\n")
          cat("sample, and a subsample can only widen the interval, so re-running\n")
          cat("the arms there would be refused for want of a gap. The remedy is\n")
          cat("NOT available on this run. Do not write that a subsample rescues\n")
          cat("the swap.\n")
          note(paste("T8: the high in-panel-share subsample clears the",
                     "divergence gate but the baseline gap is not",
                     "distinguishable from zero on the full sample, so a",
                     "re-run there cannot produce a verdict either. Reconcile",
                     "the sample first."))
        } else if (isTRUE(REMEDY_GATE_OK)) {
          # v21.46. SIZE IT BEFORE RECOMMENDING IT. The restriction costs
          # rows AND treatment dispersion, and the second is the one the
          # remedy test already warns about: the gate clears partly because
          # sd_cal falls, and sd_cal is the variation the coefficient is
          # identified from. Both together scale the SE by roughly
          # sqrt(n_full / n_sub) / sd_cal_x. Telling someone to re-run for
          # seven hours without that number is advice, not a verdict.
          .se0 <- suppressWarnings(min(sig_tab$se[sig_tab$identified &
                                                    grepl("_pool$", sig_tab$arm)],
                                       na.rm = TRUE))
          .gap0 <- suppressWarnings(max(abs(sig_tab$gap[sig_tab$identified &
                                                          grepl("_pool$", sig_tab$arm)]),
                                        na.rm = TRUE))
          .sdx <- if (exists("div_hi") && "sd_cal_x" %in% names(div_hi))
            stats::median(div_hi$sd_cal_x, na.rm = TRUE) else NA_real_
          cat("\nThe subsample clears the gate and an identified arm carries a\n")
          cat("gap, so the remedy is available in principle. Its precision:\n")
          if (is.finite(.se0) && is.finite(.sdx) && .sdx > 0 &&
              is.finite(REMEDY_N) && REMEDY_N > 0) {
            .infl <- sqrt(nrow(stacked_cc) / REMEDY_N) / .sdx
            .mde_sub <- (stats::qnorm(1 - ALPHA / 2) + stats::qnorm(0.80)) *
              .se0 * .infl
            cat(sprintf(paste("  %s of %s rows and treatment dispersion at %.3f",
                              "of full, so the SE\n  scales by about %.2f: %.4f",
                              "becomes about %.4f, an MDE of %.3f\n  against a",
                              "largest identified pooled gap of %.3f.\n"),
                        format(REMEDY_N, big.mark = ","),
                        format(nrow(stacked_cc), big.mark = ","),
                        .sdx, .infl, .se0, .se0 * .infl, .mde_sub, .gap0))
            if (.mde_sub > .gap0) {
              cat("  The MDE EXCEEDS the gap, so the re-run would come back\n")
              cat("  inconclusive by construction. Report the limit and the\n")
              cat("  two numbers rather than spending the wall time.\n")
              note(paste("T8: the high in-panel-share subsample clears the gate",
                         "and an identified arm carries a gap, but restricting",
                         sprintf("costs a factor of %.1f in SE, giving an MDE of",
                                 .infl),
                         sprintf("%.3f against a gap of %.3f. The re-run cannot",
                                 .mde_sub, .gap0),
                         "produce a verdict. Report the limit."))
            } else {
              cat("  The MDE sits below the gap, so the re-run can produce a\n")
              cat("  verdict. State the selection on time observed and entry age.\n")
              note(paste("T8: re-run the arms on the high in-panel-share",
                         "subsample. It clears the divergence gate, an",
                         "identified arm carries a gap, and the restricted MDE",
                         sprintf("of %.3f sits below it. State the selection on",
                                 .mde_sub),
                         "time observed and entry age."))
            }
          } else {
            cat("  Precision could not be sized here, so the recommendation is\n")
            cat("  withheld: size it before spending the run.\n")
          }
        }
      }
    }
    
    if (!swap_informative)
      cat(paste("\nThe PRIMARY contrast was withheld on the gate. Note that a",
                "secondary\ncontrast clearing the gate does not substitute for",
                "it: the secondary arms\neither vary a different weight or",
                "vary the gap treatment, and only the\nfirst kind can carry a",
                "units claim at all.\n"))
    
    # -----------------------------------------------------------------
    # REPRODUCTION, JUDGED ON THE PUBLISHED-UNITS ARM (F6)
    #
    # Revision 2 compared the CALENDAR HYBRID arm against the manuscript and
    # called the result a sample diagnostic. That mixes two differences: the
    # sample and the treatment construction. The arm that shares the
    # published treatment variable is the one whose miss is diagnostic of the
    # sample alone, and it is the one used here. The calendar-hybrid arm is
    # printed beside it so the construction difference is visible as the
    # difference between the two columns.
    # -----------------------------------------------------------------
    if (!is.null(res$pub_pool)) {
      cat("\nReproduction check, by sex. The gap is a difference of two\n")
      cat("numbers, so a gap that misses can miss because one arm is off or\n")
      cat("because both are. Which it is says where to look.\n")
      rep_tbl <- data.frame(
        arm = c("his", "hers", "gap"),
        published_units = round(c(res$pub_pool$his, res$pub_pool$her,
                                  res$pub_pool$gap), 4),
        calendar_hybrid = if (!is.null(res$cal_pool))
          round(c(res$cal_pool$his, res$cal_pool$her, res$cal_pool$gap), 4)
        else rep(NA_real_, 3),
        manuscript = c(MS_DML_HIS_POOLED, MS_DML_HER_POOLED, MS_DML_GAP_POOLED),
        stringsAsFactors = FALSE)
      print(rep_tbl, row.names = FALSE)
      write.csv(rep_tbl, file.path(OUTDIR, "T8_reproduction_pooled.csv"),
                row.names = FALSE)
      
      # Both panels, not just the pooled one (R16). The informative pattern is
      # that the two arms can miss by equal amounts, leaving the gap intact,
      # or by unequal amounts, moving it.
      if (!is.null(res$pub_w) && is.finite(MS_DML_GAP_WITHIN)) {
        cat("\nWithin-couple, same comparison. Only the gap is published for\n")
        cat("this panel, so the arms are shown without a target.\n")
        print(data.frame(
          arm = c("his", "hers", "gap"),
          published_units = round(c(res$pub_w$his, res$pub_w$her,
                                    res$pub_w$gap), 4),
          calendar_hybrid = if (!is.null(res$cal_w))
            round(c(res$cal_w$his, res$cal_w$her, res$cal_w$gap), 4)
          else rep(NA_real_, 3),
          manuscript = c(NA, NA, MS_DML_GAP_WITHIN),
          stringsAsFactors = FALSE), row.names = FALSE)
        cat(sprintf("Gap miss, published units: %+.4f.\n",
                    res$pub_w$gap - MS_DML_GAP_WITHIN))
        cat("  A gap that reproduces under one treatment construction and not\n")
        cat("  another is reproducing because its two arms happen to be\n")
        cat("  displaced equally, not because the design is robust to the\n")
        cat("  sample. Do not read a close within-couple match as evidence\n")
        cat("  that the sample question is confined to the pooled panel.\n")
      }
      
      # The asymmetry test, on the published-units arm and with a middle band
      # (F6). Revision 2 used the calendar arm and a bare max > 3 * min rule,
      # which reports "similar" for a threefold asymmetry the moment the ratio
      # sits at 2.9. Three bands, on the same ground as R17.
      # SIGNED, and the direction computed rather than delegated (F22).
      # Revision 3.1 printed absolute values and then told the reader to note
      # which way each miss moved the gap, which is the whole story when the
      # signs differ.
      sh <- res$pub_pool$his - MS_DML_HIS_POOLED
      sf <- res$pub_pool$her - MS_DML_HER_POOLED
      dh <- abs(sh); df_her <- abs(sf)
      cat(sprintf("\nMiss on the published-units arm: his %+.4f, hers %+.4f.\n",
                  sh, sf))
      if (is.finite(sh) && is.finite(sf)) {
        if (sign(sh) == sign(sf)) {
          cat("  SAME sign, so both arms are displaced in the same direction\n")
          cat("  and the gap between them survives most of the displacement.\n")
          cat("  A common displacement points at the sample as a whole.\n")
        } else {
          cat("  OPPOSITE signs, so the two arms move TOWARDS each other and\n")
          cat("  the gap collapses by more than either miss. That is what makes\n")
          cat(sprintf(paste("  the gap %+.4f against a published %+.4f: his is",
                            "too high and hers\n  too low at once. A",
                            "sex-asymmetric displacement points at what the\n",
                            " dual-earner filter drops, not at the",
                            "specification.\n"),
                      res$pub_pool$gap, MS_DML_GAP_POOLED))
        }
      }
      asym <- if (min(dh, df_her) > 0) max(dh, df_her) / min(dh, df_her) else Inf
      cat(sprintf("  Ratio of the larger miss to the smaller: %.1f.\n", asym))
      if (!is.finite(asym) || asym >= 3) {
        cat("  One arm reproduces and the other does not, which is a sample\n")
        cat("  restriction acting on one sex rather than a construction error\n")
        cat("  acting on both. Check what the dual-earner filter drops before\n")
        cat("  changing anything in the specification.\n")
        note(paste("T8: the published-units arm misses by very unequal amounts",
                   "on the two sexes, so the dual-earner reconstruction is",
                   "acting differently on men and women. Read the sample, not",
                   "the specification."))
      } else if (asym <= 1.5) {
        cat("  Both arms miss by a similar amount, which points at the sample\n")
        cat("  or the panel rather than at either arm's construction.\n")
      } else {
        cat("  The two misses differ appreciably but not by a factor of three.\n")
        cat("  This test cannot separate a one-sex sample restriction from a\n")
        cat("  common displacement at that ratio; report both misses and do\n")
        cat("  not attribute. Note which direction each moves the gap: misses\n")
        cat("  of the same sign leave it intact, opposite signs move it.\n")
        note(sprintf(paste("T8: the published-units arm misses his by %.3f and",
                           "hers by %.3f, a ratio of %.1f. Report both; the",
                           "asymmetry test cannot attribute at that ratio."),
                     dh, df_her, asym))
      }
      if (is.finite(MS_DML_GAP_POOLED) && MS_DML_GAP_POOLED != 0)
        cat(sprintf(paste("  The published-units gap is %.0f%% of the published",
                          "gap (%+.4f against %+.4f),\n  on the published",
                          "treatment variable, so nothing about the FTE",
                          "weight or\n  the gap treatment is implicated in",
                          "this miss.\n"),
                    100 * res$pub_pool$gap / MS_DML_GAP_POOLED,
                    res$pub_pool$gap, MS_DML_GAP_POOLED))
    }
    
    # GATED, on the PRIMARY arm, and only when the baseline gap is
    # distinguishable from zero (R14). This note is what gets read as the
    # actionable summary, so a claim that survives only here is worse than one
    # that appears nowhere.
    prim <- res[[paste0(prim_arm, "_pool")]]
    if (!is.null(res$cal_pool) && !is.null(prim) && !is.na(prim_arm)) {
      # The divergence sentence is written ONCE, by the gate block in T8a
      # (F21). Revision 3.1 wrote it here as well, so the decision list
      # carried the same fact twice in different words. What this block adds
      # when the gate fails is the movement it withheld, which is a number
      # the gate block does not have, so it prints rather than notes.
      if (!gate_ok(prim_arm))
        cat(sprintf(paste("\nFor the record, the movement the PRIMARY contrast",
                          "withheld: %+.3f%% to\n%+.3f%%, a change of %+.3f%%.",
                          "It is not evidence about units and it is not\nnoted",
                          "again; see the gate and two-blocker blocks above.\n"),
                    res$cal_pool$gap, prim$gap, prim$gap - res$cal_pool$gap))
      else if (is.finite(res$cal_pool$p) && res$cal_pool$p < ALPHA)
        note(sprintf(paste("T8: the pooled gender gap in the return to",
                           "experience moves from %+.3f%% in calendar years to",
                           "%+.3f%% in one-sided FTE years, a change of %.0f%%,",
                           "both arms gap-blind."),
                     res$cal_pool$gap, prim$gap,
                     100 * abs(1 - abs(prim$gap) /
                                 max(abs(res$cal_pool$gap), 1e-8))))
      else
        note(sprintf(paste("T8: no shrinkage claim. The calendar-arm gap is",
                           "%+.3f%% at p = %.3f, indistinguishable from zero on",
                           "this sample, so the movement to %+.3f%% in FTE units",
                           "carries no evidence either way. Reconcile the sample",
                           "before reading the swap."),
                     res$cal_pool$gap, res$cal_pool$p, prim$gap))
    }
  }
}


source("03_finish.R")
