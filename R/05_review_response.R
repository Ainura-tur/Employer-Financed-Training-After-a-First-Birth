# =============================================================================
# v-usspell, 22 October 2026: US spelling in the R16 table notes (fix_us_spelling_05.R).
# v-posthoc-ledger, 23 October 2026: Appendix I describes post-hoc analyses as the paper does (fix_posthoc_wording_05.R).
# v-notes-trwin13, 19 October: R16 notes for Tables I.1 and I.3 (apply_notes_trwin13.R).
# 05_review_response.R  -  estimates requested by the pre-submission review
#
# WHAT THIS ADDS. One block per item in the review-response plan:
#   R1  the paid-time margin among full-time women with k = 0 and 1 dropped,
#       the Sun-Abraham path for full-time returners, and a recovery test
#   R2  the financing contrast in proportions on the same samples, with a
#       person bootstrap that carries the uncertainty in the pre-birth bases
#   R3  exposure: the calendar's share of the last financial year in jobs
#   R4  the worker margin at any place (own time anywhere, own time alone),
#       with formal study beside it
#   R5  an earnings event study with non-employed person-waves kept as zeros
#   R6  the pre-specified and the age-absorbed wage paths, drawn together
#   R6b the wage window contrast and lead test, from the full covariance
#   R7  hours accounting: the fall that the change in hours would predict,
#       removed from each margin (transport and sequential g-estimation)
#   R8  two clocks: the change at the birth and at the switch to part time
#   R9  the proportional gap year by year, beside mothers' part-time share
#   R10 quick checks: weights, Wave 20, never-mothers as the only comparison,
#       the interview clock where it exists, and the experience screen
#   R11 exit expectations: whether mothers' stated chance of leaving moves,
#       and whether the fall depends on her own pre-birth chance
#   R12 birth-wave exposure: the fall by the share of the window after birth
#   R13 part time by choice or constraint: the hours preference split
#   R14 where hours did not change, against fathers: R8 on the identified
#       comparison
#   R15 days of leave in the last twelve months: paid and unpaid leave as the
#       exposure measure the calendar cannot give
#   R16 appendix tables: LaTeX tables and a section file for the analyses the
#       revised text quotes, built from every result in the handoff
#
# EVERYTHING HERE IS POST-HOC. It was designed after unblinding, in response
# to a review. It writes to output/diagnostics/review_response/ and touches
# neither the ledger, output/results/ nor the manuscript's tables folder, so
# no number reaches the paper by accident. Move one in deliberately, marked
# post-hoc. The one exception is R16, which writes the adopted appendix
# tables to the build folder, output/tables/, beside every other fragment;
# paper_tables.R then syncs them to the manuscript through its \tofill gate.
# Run R16 before paper_tables.R.
#
# INPUTS
#   handoff/ev.rds                  03a_design_audit.R, for R1 to R5
#   the analysis frame              td_frame(), only for columns ev lacks
#   cs_event_study_results.rds      CS_event_study.R, for R6
# A block that lacks its input says so and skips; the others still run.
#
# SETTINGS are environment variables, so RUN_ALL.R's child processes see them:
#   RR_BOOT_B        bootstrap replicates for R2 (default 199; 0 skips)
#   RR_BOOT_CELLS    which sample:window cells R2 bootstraps. Default: all
#                    eight, so every row of Table I1 uses one method
#                    (k0 and k2 are accepted for k>=0 and k>=2). Each cell
#                    takes about three minutes at 199 replicates.
#   RR_BLOCKS        which blocks to run: all (default), or a list such as
#                    R2 or R7,R8,R9,R10. The reproduction check always runs.
#                    R7's bootstrap uses RR_BOOT_B and takes about eight
#                    minutes at 199 replicates.
#   RR_EXPOSURE_COL  the exposure column for R3. Unset, R3 reads the
#                    loader's fy_share_employed (the calendar's capj, per
#                    cent of the last financial year spent in jobs)
#   RR_STUDY_COL     formal-study indicator for R4. Unset, it is resolved as
#                    T9 resolves it (full_time_student, from edfts, in the
#                    current loader), never from the entrant-only edcqen
#                    columns (enrolled_in_course*)
#   RR_EARN_COL      annual earnings for R5. Unset, the loader's
#                    real_annual_income is used: coalesce(fy_wages_inc_ss,
#                    fy_wages), i.e. wsfes, deflated to Wave 24 dollars
#
# RUN AFTER 03a and CS_event_study.R. To add it to RUN_ALL.R:
#   list(id = "05", script = "05_review_response.R", mins = 15,
#        log = "^05_review_response_",
#        expect = list("reproduction" = "REPRODUCTION: 5 of 5"))
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
})

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

# ---- the contract ------------------------------------------------------------
# Sourced here only when this file runs on its own. Under RUN_ALL.R or the
# master it is already loaded, and sourcing it again would rebuild the
# registry mid-run.
if (!exists("td_get", mode = "function")) {
  rr_c <- c(file.path(Sys.getenv("HILDA_ROOT", ""), "R", "00_contract.R"),
            file.path(Sys.getenv("HILDA_ROOT", ""), "00_contract.R"),
            "00_contract.R", file.path("R", "00_contract.R"))
  rr_c <- rr_c[nzchar(Sys.getenv("HILDA_ROOT", "")) | !startsWith(rr_c, "/")]
  rr_c <- rr_c[file.exists(rr_c)]
  if (!length(rr_c))
    stop("05_review_response.R needs 00_contract.R. Source it first, or ",
         "set HILDA_ROOT.", call. = FALSE)
  source(rr_c[1])
  rm(rr_c)
}
if (exists("td_log_open", mode = "function"))
  try(td_log_open("05_review_response.R"), silent = TRUE)

# ---- settings ----------------------------------------------------------------
RR_BOOT_B <- suppressWarnings(as.integer(Sys.getenv("RR_BOOT_B", "199")))
if (is.na(RR_BOOT_B) || RR_BOOT_B < 0L) RR_BOOT_B <- 199L
RR_SEED         <- 20260927L
RR_BOOT_CELLS   <- Sys.getenv("RR_BOOT_CELLS",
  "all:k>=0,all:k>=2,ftret:k>=0,ftret:k>=2,preft:k>=0,preft:k>=2,ftnow:k>=0,ftnow:k>=2")
RR_BLOCKS       <- toupper(trimws(strsplit(Sys.getenv("RR_BLOCKS", "all"), ",")[[1]]))
rr_run <- function(b) "ALL" %in% RR_BLOCKS || any(toupper(b) %in% RR_BLOCKS)
RR_EXPOSURE_COL <- Sys.getenv("RR_EXPOSURE_COL", "")
RR_STUDY_COL    <- Sys.getenv("RR_STUDY_COL", "")
RR_EARN_COL     <- Sys.getenv("RR_EARN_COL", "")

# The static design is 03a's: person and sex-by-wave effects, clustered on
# the person. fe_block() is 03_common.R's name for it when that is loaded.
RR_FE <- if (exists("fe_block", mode = "function")) fe_block() else
  "person_id + wave^female"

# Windows. The training series use the figures' window, -8 to +9; the
# earnings series use the reported wage window sealed in the contract.
# Periods beyond a window are binned so that every reported horizon is
# estimated on its own.
RR_TRAIN_K <- c(-8L, 9L)
RR_EARN_K  <- c(if (exists("WAGE_K_LO")) WAGE_K_LO else -8L,
                if (exists("WAGE_K_HI")) WAGE_K_HI else 10L)
RR_EARLY   <- 2:4    # R1 recovery test: after leave, before any recovery
RR_LATE    <- 5:8    # R1 recovery test: the later horizons
RR_W_EARLY <- 0:4    # R5 and R6b windows, the paper's wage windows
RR_W_LATE  <- 5:10
RR_LEADS   <- -8:-2  # pre-trend joint test

RR_DIR <- td_out("diagnostics", "review_response")
dir.create(RR_DIR, showWarnings = FALSE, recursive = TRUE)
rr_path <- function(f) file.path(RR_DIR, f)

# State lives in an environment, not in globals assigned with <<-. The
# master sources scripts into local frames, where <<- skips the frame the
# object lives in (CS_event_study.R, v13). Each block's body runs in
# local(), so its working objects do not leak into the session, and a
# block that fails is recorded and skipped rather than ending the run.
rr <- new.env()
rr$fail <- character(0)
rr$skip <- character(0)
rr$tabs <- list()
rr_block <- function(label, expr) {
  cat("\n", strrep("=", 76), "\n", label, "\n", strrep("=", 76), "\n", sep = "")
  tryCatch(expr, error = function(e) {
    rr$fail <- c(rr$fail, paste0(label, ": ", conditionMessage(e)))
    cat("  *** BLOCK FAILED: ", conditionMessage(e), "\n", sep = "")
    invisible(NULL)
  })
}
rr_unselected <- function(label) {
  rr$skip <- c(rr$skip, paste0(label, ": not selected in RR_BLOCKS"))
  invisible(NULL)
}
# ---- paper numbers ------------------------------------------------------------
# Every table rr_write() writes is also registered, cell by cell, as its own
# part of the paper's number registry (output/numbers/05_review_response__
# <table>.csv), so a run that selects some blocks refreshes only their tables.
# A key is <prefix>.<row labels>.<column>, with operators spelled out, e.g.
# R2.all.kge0.E1_O2.proportional.est; key.abs prints the magnitude. The text
# uses \pn{key}; paper_tables.R writes the macro file. The descriptive tables
# (R3_exposure_means to R15_leave_means) hold the means and counts the text
# quotes beside the estimates. A form no table holds, such as a share as a
# percentage, is declared in paper_numbers_derived.R.
source("paper_numbers.R")
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
RR_PN_PREFIX <- c(R0_reproduction = "R0", R1_R4_did_by_sample = "R1R4", R4_study_by_sample = "R4",
                  R2_financing_contrast = "R2", R1_event_paths = "R1path", R1_recovery_test = "R1rec",
                  R3_exposure = "R3", R5_earnings_paths = "R5path", R5_earnings_windows = "R5win",
                  R6_wage_paths = "R6", R6b_wage_windows = "R6b", R7_hours_single = "R7single",
                  R7_hours_contrast = "R7contrast", R7_hours_first_stage = "R7stage1", R8_two_clocks = "R8",
                  R9_proportional_path = "R9path", R9_proportional_growth = "R9growth",
                  R10_quick_checks = "R10", R11_exit_expectations = "R11", R11_exit_by_group = "R11group",
                  R12_window_share = "R12", R13_part_time_choice = "R13", R14_hours_fixed_fathers = "R14",
                  R15_leave_days = "R15",
                  R3_exposure_means = "R3mean", R7_hours_did = "R7hours", R11_prebirth_means = "R11pre",
                  R12_mothers_by_share = "R12n", R13_part_time_shares = "R13share", R14_groups = "R14n",
                  R15_leave_means = "R15mean")
# Digits where the text prints a column other than to three decimals.
RR_PN_DIGITS <- list(R2_financing_contrast = c(mde = 2, boot_mde = 2),
                     R3_exposure_means = c(EXPO = 2),
                     R7_hours_did = c(est = 1, se = 1, mde = 1),
                     R15_leave_means = c(LEAVE_mothers = 0, LEAVE_fathers = 0),
                     R1_recovery_test = c(leads_p_women = 2, leads_p_men = 2))
rr_build_tag <- function() {
  if (!is.null(rr$build_tag)) return(rr$build_tag)
  bt <- if (exists("rr_ev")) attr(rr_ev, "build_tag") else NULL
  if (is.null(bt))
    bt <- tryCatch(attr(td_frame(need_cols = c("person_id", "wave"), context = "05 paper numbers"), "build_tag"),
                   error = function(e) NULL)
  rr$build_tag <- if (is.null(bt)) NA_character_ else as.character(bt)
  rr$build_tag
}
rr_write <- function(x, f) {
  utils::write.csv(x, rr_path(f), row.names = FALSE)
  cat("  wrote ", f, "\n", sep = "")
  stem <- sub("\\.csv$", "", f)
  tryCatch(pn_register_csv(x, prefix = if (stem %in% names(RR_PN_PREFIX)) RR_PN_PREFIX[[stem]] else stem,
                           part = stem, script = "05_review_response.R",
                           build_tag = rr_build_tag(), digits = RR_PN_DIGITS[[stem]]),
           error = function(e) cat("  *** paper numbers not registered for ", f, ": ",
                                   conditionMessage(e), "\n", sep = ""))
}
rr_num <- function(x) round(x, 4)

# What the manuscript reports, so the first thing this file does is show it
# is estimating on the paper's frames. Tolerance is half a unit in the third
# decimal plus rounding in the fourth.
RR_TARGETS <- data.frame(
  target = c("T1 employed-only, paid time at the workplace",
             "T1 employed-only, own time at the workplace",
             "Triple difference, sex-specific source-by-wave path",
             "T4_any, full-time returners",
             "T4c, full time before the birth"),
  sample = c("all", "all", "all", "ftret", "preft"),
  item   = c("E1", "O1", "E1-O1", "E1", "E1"),
  # v-unscreened-values-2, 18 October: the unscreened sample. With the screen, v21.49:
  # -0.1106, -0.0189, -0.0918, -0.1056, -0.1226 on 55039, 55039, 110078, 47312, 47240.
  paper  = c(-0.1101, -0.0187, -0.0913, -0.1047, -0.1225),
  paper_n = c(55121, 55121, 110242, 47394, 47302),
  stringsAsFactors = FALSE)
RR_TOL <- 0.0015

# =============================================================================
# 0. THE FRAME
# =============================================================================
td_require("ev", context = "05_review_response.R")
rr_ev <- as.data.frame(td_get("ev"))
rr_need <- c("person_id", "wave", "k", "female", "treated", "post",
             "employed", "jttpewt_any", "jttpeot_any")
rr_miss <- setdiff(rr_need, names(rr_ev))
if (length(rr_miss))
  stop("ev lacks ", paste(rr_miss, collapse = ", "), ". Rebuild it with ",
       "03a_design_audit.R before running this file.", call. = FALSE)

rr_to01 <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  if (is.factor(x) || is.character(x)) {
    x <- tolower(trimws(as.character(x)))
    return(ifelse(is.na(x), NA_real_,
                  as.numeric(x %in% c("1", "true", "yes", "full-time",
                                      "full time", "ft"))))
  }
  as.numeric(x)
}

# Full-time status. MASTER's hours arms read fulltime_num; the frame carries
# fulltime. Either is accepted, and the coding is checked rather than assumed.
rr_ft_col <- intersect(c("fulltime_num", "fulltime"), names(rr_ev))[1]
rr_ev$ft <- if (!is.na(rr_ft_col)) rr_to01(rr_ev[[rr_ft_col]]) else NA_real_
if (!is.na(rr_ft_col) &&
    !all(stats::na.omit(unique(rr_ev$ft)) %in% c(0, 1)))
  stop("full-time column ", rr_ft_col, " is not 0/1 after conversion. ",
       "Values: ", paste(utils::head(unique(rr_ev[[rr_ft_col]]), 6),
                         collapse = ", "), call. = FALSE)

rr_ev <- rr_ev %>%
  mutate(female   = rr_to01(female),
         treated  = rr_to01(treated),
         post     = rr_to01(post),
         employed = rr_to01(employed),
         post_f   = post * female,
         cohort   = ifelse(treated == 1, wave - k, 10000))

# One cohort per treated person, or the event time is not an event time.
rr_bad <- rr_ev %>% filter(treated == 1) %>% group_by(person_id) %>%
  summarise(nc = dplyr::n_distinct(cohort), .groups = "drop") %>%
  filter(nc > 1)
if (nrow(rr_bad))
  stop(nrow(rr_bad), " treated persons carry more than one first-birth wave ",
       "(wave - k varies within person). Check k in ev.", call. = FALSE)
rm(rr_bad)

# ---- the employed frame and the outcomes ----------------------------------
# The employed frame is T1's employed-only arm: employed person-waves with
# the workplace paid-time item observed (54,202 in the paper).
#
# The any-place items are coded missing when the respondent reported no
# training, because the place-and-time questions are put only to trainees.
# 04financing_space.R reads them the same way, as zero within the asked
# population, and so does this file. Employed person-waves are asked.
rr_has_any <- all(c("training_paid_time", "training_own_time") %in% names(rr_ev))
rr_emp <- rr_ev %>% filter(employed == 1, !is.na(jttpewt_any))
rr_emp$E1 <- as.numeric(rr_emp$jttpewt_any)   # paid time, at the workplace
rr_emp$O1 <- as.numeric(rr_emp$jttpeot_any)   # own time, at the workplace
if (rr_has_any) {
  rr_emp$E2 <- dplyr::coalesce(as.numeric(rr_emp$training_paid_time), 0)
  rr_emp$O2 <- dplyr::coalesce(as.numeric(rr_emp$training_own_time), 0)
  # The any-place items include the workplace ones by construction; if a
  # workplace yes sits beside an any-place no, the pooled column is wrong.
  rr_viol <- sum(rr_emp$E1 == 1 & rr_emp$E2 == 0, na.rm = TRUE) +
             sum(rr_emp$O1 == 1 & rr_emp$O2 == 0, na.rm = TRUE)
  if (rr_viol > 0)
    cat("  *** ", rr_viol, " person-waves have a workplace yes and an any-place\n",
        "      no. training_paid_time/own_time should pool across place; check\n",
        "      the loader before reading R4.\n", sep = "")
  rm(rr_viol)
  # Own time ALONE at any place: own time somewhere and paid time nowhere.
  rr_emp$O3 <- as.numeric(rr_emp$O2 == 1 & rr_emp$E2 == 0)
} else {
  rr_emp$E2 <- NA_real_; rr_emp$O2 <- NA_real_; rr_emp$O3 <- NA_real_
  cat("  training_paid_time / training_own_time absent from ev: the\n",
      "  any-place outcomes (E2, O2, O3) are skipped.\n", sep = "")
}
# Own time at OTHER places (O4), the clearest worker margin: training in the
# respondent's own time somewhere other than the workplace, so the after-hours
# workplace training the review worries about is excluded. It is read from the
# raw other-place item (jttopot_raw), coded 1 when mentioned and 0 otherwise
# within the asked population, as the _any items are. Unlike own time alone,
# it is not defined by the absence of paid time, so a fall in paid time does
# not move it mechanically.
# ev may not carry the raw item (04financing_space.R recovers the place
# dimension by the residual rule instead), so the frame is tried as well.
# A derived 0/1 twin (jttopot_any) is used if the loader writes one; the raw
# code otherwise, read on the same == 1 rule.
rr_o4_cand <- c("jttopot_any", "jttopot_raw")
rr_o4_col  <- intersect(rr_o4_cand, names(rr_emp))[1]
if (is.na(rr_o4_col)) {
  rr_fr <- tryCatch(td_frame(need_cols = c("person_id", "wave"), context = "05 O4"),
                    error = function(e) NULL)
  rr_o4_col <- if (!is.null(rr_fr)) intersect(rr_o4_cand, names(rr_fr))[1] else NA_character_
  if (!is.na(rr_o4_col)) {
    rr_emp <- rr_emp %>% left_join(rr_fr[, c("person_id", "wave", rr_o4_col)],
                                   by = c("person_id", "wave"))
    cat("  ", rr_o4_col, " joined from the frame.\n", sep = "")
  }
  rm(rr_fr)
}
rr_o4_note <- NULL
if (!is.na(rr_o4_col)) {
  rr_x <- rr_emp[[rr_o4_col]]
  rr_x <- if (is.factor(rr_x) || is.character(rr_x)) rr_to01(rr_x) else as.numeric(rr_x)
  rr_emp$O4 <- as.numeric(!is.na(rr_x) & rr_x == 1)
  if (rr_has_any) {
    rr_v <- sum(rr_emp$O4 == 1 & rr_emp$O2 == 0, na.rm = TRUE)
    if (rr_v > 0.01 * max(1, sum(rr_emp$O4 == 1)))
      cat("  *** O4: ", rr_v, " person-waves report own time elsewhere but no own time\n",
          "      at any place. Check ", rr_o4_col, "'s coding before reading O4.\n", sep = "")
    rm(rr_v)
  }
  rr_o4_note <- paste0(rr_o4_col, " == 1")
  rm(rr_x)
} else if (rr_has_any) {
  rr_emp$O4 <- as.numeric(rr_emp$O2 == 1 & rr_emp$O1 == 0)
  rr_o4_note <- "own time at any place but not at the workplace (jttopot_raw absent)"
  cat("  O4 is built by the residual rule, as 04 builds the place dimension: own\n",
      "  time at any place but not at the workplace. It misses people with own\n",
      "  time at both, and it rises mechanically when workplace own time falls,\n",
      "  which makes its contrast with paid time look larger. Keeping jttopot in\n",
      "  the loader as jttopot_raw would fix both.\n", sep = "")
} else rr_emp$O4 <- NA_real_
if (!is.null(rr_o4_note))
  cat("  O4, own time at other places: ", rr_o4_note, " | mean ",
      sprintf("%.4f", mean(rr_emp$O4, na.rm = TRUE)), "\n", sep = "")

RR_OUTCOMES <- c(E1 = "paid time, workplace (the paper's measure)",
                 E2 = "paid time, any place",
                 O1 = "own time, workplace (the paper's worker margin)",
                 O2 = "own time, any place",
                 O3 = "own time alone, any place",
                 O4 = "own time, other places (the clearest worker margin)")

# ---- the samples ----------------------------------------------------------
# ftret reproduces T4_any: treated persons full time at ANY wave in
# k = +1..+5, with every never-treated person (MASTER section 9.3 header).
# preft reproduces T4c and MASTER's arm E: treated persons full time at every
# observed wave in k = -3..-1, with at least two such waves.
# ftnow keeps full-time person-waves only, treated or not. It conditions on
# current full-time status, an outcome of the birth, so it bounds the
# exposure channel rather than estimating a causal effect.
rr_samples <- list(all = rr_emp)
if (!is.na(rr_ft_col)) {
  rr_ftret_ids <- rr_emp %>%
    filter(treated == 1, !is.na(k), k >= 1, k <= 5, ft == 1) %>%
    distinct(person_id) %>% pull(person_id)
  rr_preft_ids <- rr_emp %>%
    filter(treated == 1, !is.na(k), k >= -3, k <= -1, !is.na(ft)) %>%
    group_by(person_id) %>%
    summarise(all_ft = all(ft == 1), nk = dplyr::n(), .groups = "drop") %>%
    filter(all_ft, nk >= 2) %>% pull(person_id)
  rr_samples$ftret <- rr_emp %>% filter(treated == 0 | person_id %in% rr_ftret_ids)
  rr_samples$preft <- rr_emp %>% filter(treated == 0 | person_id %in% rr_preft_ids)
  rr_samples$ftnow <- rr_emp %>% filter(ft == 1)
} else {
  cat("  No full-time column on ev: R1 and R2 run on all employed only.\n")
}
RR_SAMPLE_LABEL <- c(all   = "all employed",
                     ftret = "full-time returners (T4_any)",
                     preft = "full time before the birth (T4c)",
                     ftnow = "full-time person-waves only")
RR_WINDOWS <- list(
  "k>=0" = function(d) d,
  # k = 0 and 1 are the waves whose twelve-month reference period overlaps
  # the birth and most leave. Dropping them removes the mechanical part of
  # the fall; post then means k >= 2.
  "k>=2" = function(d) d[!(d$treated == 1 & !is.na(d$k) & d$k %in% c(0, 1)), ,
                         drop = FALSE])

cat(sprintf("\n  ev: %s person-waves, %s persons | employed frame: %s person-waves\n",
            format(nrow(rr_ev), big.mark = ","),
            format(dplyr::n_distinct(rr_ev$person_id), big.mark = ","),
            format(nrow(rr_emp), big.mark = ",")))
for (s in names(rr_samples))
  cat(sprintf("  %-6s %-34s %s person-waves, %s treated persons\n", s,
              RR_SAMPLE_LABEL[[s]],
              format(nrow(rr_samples[[s]]), big.mark = ","),
              format(dplyr::n_distinct(rr_samples[[s]]$person_id[
                rr_samples[[s]]$treated == 1]), big.mark = ",")))

# =============================================================================
# ESTIMATORS
# =============================================================================
# Women's own pre-birth base, the paper's denominator: treated women over the
# three waves before the birth, in whatever sample is being estimated.
rr_base <- function(d, y)
  mean(d[[y]][d$treated == 1 & d$female == 1 & !is.na(d$k) &
                d$k >= -3 & d$k <= -1], na.rm = TRUE)

rr_fml <- function(y) stats::as.formula(paste0(y, " ~ post + post_f | ", RR_FE))

# Static difference-in-differences, mothers minus fathers: 03a's T1.
rr_did <- function(d, y, sample, window) {
  d <- d[!is.na(d[[y]]), , drop = FALSE]
  m <- tryCatch(feols(rr_fml(y), data = d, cluster = ~person_id,
                      notes = FALSE, warn = FALSE), error = function(e) NULL)
  ok <- !is.null(m) && "post_f" %in% names(stats::coef(m))
  est <- if (ok) unname(stats::coef(m)["post_f"]) else NA_real_
  se  <- if (ok) unname(fixest::se(m)["post_f"]) else NA_real_
  b   <- rr_base(d, y)
  data.frame(sample = sample, window = window, outcome = y,
             est = est, se = se, mde = mde(se),
             n = if (ok) stats::nobs(m) else nrow(d),
             persons = dplyr::n_distinct(d$person_id),
             base = b, prop = est / b, prop_se_base_fixed = se / b,
             stringsAsFactors = FALSE)
}

# Point estimate only, for the bootstrap. No vcov argument, so the call works
# on fixest versions older than 0.10 as well.
rr_coef <- function(d, y) {
  m <- tryCatch(feols(rr_fml(y), data = d, notes = FALSE, warn = FALSE),
                error = function(e) NULL)
  if (is.null(m) || !"post_f" %in% names(stats::coef(m))) NA_real_ else
    unname(stats::coef(m)["post_f"])
}

# The financing contrast as a stacked triple difference, the paper's
# sex-specific version: person-by-source and wave-by-sex-by-source effects,
# every two-way interaction of post, female and source. Fully interacted, it
# equals the difference of the two separate DiDs, on the same rows.
#
# prop = TRUE divides each source by its own pre-birth base first, so the
# coefficient is the DIFFERENCE IN PROPORTIONAL FALLS and every row stays in.
# Fixed-effects Poisson is not used: it drops each person-source with no
# training at all, and own-time training is rare within person in any
# sample, full time or not, so the separation the paper reports would stay.
rr_ddd <- function(d, e, o, prop, sample, window) {
  d  <- d[!is.na(d[[e]]) & !is.na(d[[o]]), , drop = FALSE]
  be <- rr_base(d, e); bo <- rr_base(d, o)
  if (prop && (!is.finite(be) || !is.finite(bo) || be <= 0 || bo <= 0))
    return(data.frame(sample = sample, window = window,
                      pair = paste(e, o, sep = "-"),
                      scale = "proportional", est = NA_real_, se = NA_real_,
                      mde = NA_real_, n = 2L * nrow(d), base_e = be, base_o = bo,
                      stringsAsFactors = FALSE))
  keep <- c("person_id", "wave", "female", "post", "post_f")
  s <- rbind(data.frame(d[keep], y = d[[e]] / (if (prop) be else 1), src = 1L),
             data.frame(d[keep], y = d[[o]] / (if (prop) bo else 1), src = 0L))
  s$post_src   <- s$post   * s$src
  s$post_f_src <- s$post_f * s$src
  m <- tryCatch(feols(y ~ post + post_f + post_src + post_f_src |
                        person_id^src + wave^female^src,
                      data = s, cluster = ~person_id, notes = FALSE, warn = FALSE),
                error = function(e) NULL)
  ok <- !is.null(m) && "post_f_src" %in% names(stats::coef(m))
  se <- if (ok) unname(fixest::se(m)["post_f_src"]) else NA_real_
  data.frame(sample = sample, window = window, pair = paste(e, o, sep = "-"),
             scale = if (prop) "proportional" else "level",
             est = if (ok) unname(stats::coef(m)["post_f_src"]) else NA_real_,
             se = se, mde = mde(se),
             n = if (ok) stats::nobs(m) else nrow(s), base_e = be, base_o = bo,
             stringsAsFactors = FALSE)
}

# Person bootstrap for the contrasts. Resampling persons carries both the
# clustering and the sampling error of the two bases, which the stacked
# regression's standard error treats as known. Each drawn person is a new
# cluster, so a person drawn twice counts twice.
rr_boot <- function(d, pairs, B, seed) {
  if (B < 1L) return(NULL)
  set.seed(seed)
  by_p <- split(seq_len(nrow(d)), as.character(d$person_id))
  ids  <- names(by_p)
  out  <- matrix(NA_real_, B, 2L * length(pairs),
                 dimnames = list(NULL, as.vector(t(outer(
                   names(pairs), c("level", "proportional"), paste, sep = "|")))))
  t0 <- Sys.time()
  for (b in seq_len(B)) {
    draw <- sample(ids, length(ids), replace = TRUE)
    rows <- by_p[draw]
    db <- d[unlist(rows, use.names = FALSE), , drop = FALSE]
    db$person_id <- rep.int(seq_along(draw), lengths(rows))
    for (p in names(pairs)) {
      e <- pairs[[p]][1]; o <- pairs[[p]][2]
      dd <- db[!is.na(db[[e]]) & !is.na(db[[o]]), , drop = FALSE]
      ce <- rr_coef(dd, e); co <- rr_coef(dd, o)
      out[b, paste0(p, "|level")]        <- ce - co
      out[b, paste0(p, "|proportional")] <- ce / rr_base(dd, e) - co / rr_base(dd, o)
    }
    if (b %% 50L == 0L || b == B)
      cat(sprintf("    bootstrap %d of %d (%.1f min)\n", b, B,
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  }
  out
}

# Sun-Abraham, fitted as CS_event_study.R fits it: sunab() on the first-birth
# cohort, never-treated as 10000, person and wave effects, clustered on the
# person. Periods outside [k_lo, k_hi] are binned at each end.
#
# THE BINS ARE BUILT FROM THE PERIODS PRESENT, and written into the formula.
# Two things differ from CS_event_study.R here. A bin object defined inside
# a function is not found by feols, which reads it as a missing variable;
# and fixest 0.14 rejects bin values that match no relative period, so
# CS's (-200):(-K) stops with "the value -200 does not match any value".
rr_sa <- function(d, y, k_lo, k_hi, fe = "person_id + wave") {
  rel <- d$wave[d$cohort < 10000] - d$cohort[d$cohort < 10000]
  rel <- sort(unique(rel[is.finite(rel)]))
  lo <- rel[rel < k_lo]; hi <- rel[rel > k_hi]
  bins <- c(if (length(lo) > 1L)
              sprintf("`%d` = c(%s)", k_lo - 1L, paste(lo, collapse = ", ")),
            if (length(hi) > 1L)
              sprintf("`%d` = c(%s)", k_hi + 1L, paste(hi, collapse = ", ")))
  rhs <- if (length(bins))
    sprintf("sunab(cohort, wave, bin.rel = list(%s))", paste(bins, collapse = ", ")) else
    "sunab(cohort, wave)"
  f <- stats::as.formula(paste0(y, " ~ ", rhs, " | ", fe))
  tryCatch(feols(f, data = d, cluster = ~person_id, notes = FALSE, warn = FALSE),
           error = function(e) { cat("    sunab failed: ", conditionMessage(e),
                                     "\n", sep = ""); NULL })
}

# The period coefficients AND THEIR COVARIANCE, inside the window.
#
# coef() and se() on a sunab fit return the aggregated period estimates,
# but vcov() returns the covariance of the cohort-by-period coefficients,
# so the two do not line up and a window contrast cannot be read off them.
# The aggregation is rebuilt here as fixest's aggregate() does it: each
# period's estimate is the cohort coefficients weighted by their number of
# observations in the estimation sample. The covariance is then A V A'.
# The rebuilt estimates and standard errors are checked against coef() and
# se(), so a fixest version that aggregates differently stops here rather
# than returning wrong intervals.
rr_es <- function(m, k_lo, k_hi) {
  if (is.null(m)) return(NULL)
  cf  <- m$coefficients; cn <- names(cf)
  pat <- m$model_matrix_info$sunab$agg_period
  if (is.null(pat)) pat <- "(wave)::(-?[[:digit:]]+):cohort"
  sel <- which(grepl(pat, cn, perl = TRUE))
  per <- as.numeric(gsub(paste0(".*", pat, ".*"), "\\2", cn[sel], perl = TRUE))
  mm  <- stats::model.matrix(m)
  cnt <- colSums(sign(mm[, cn[sel], drop = FALSE]))
  pk  <- sort(unique(per[per >= k_lo & per <= k_hi]))
  A   <- matrix(0, length(pk), length(cn), dimnames = list(pk, cn))
  for (j in seq_along(pk)) {
    w <- cnt[per == pk[j]]
    A[j, sel[per == pk[j]]] <- w / sum(w)
  }
  b  <- drop(A %*% cf)
  V  <- A %*% stats::vcov(m)[cn, cn] %*% t(A)
  ag <- stats::coef(m)
  if (!any(grepl("cohort", names(ag)))) {
    ak <- suppressWarnings(as.numeric(sub(".*::(-?[0-9]+)$", "\\1", names(ag))))
    i  <- match(pk, ak)
    dc <- suppressWarnings(max(abs(ag[i] - b), na.rm = TRUE))
    ds <- suppressWarnings(max(abs(fixest::se(m)[i] - sqrt(diag(V))), na.rm = TRUE))
    if (is.finite(dc) && (dc > 1e-8 || ds > 1e-8))
      stop("rebuilt sunab aggregation differs from fixest's by ",
           signif(max(dc, ds), 3), ". This fixest version aggregates ",
           "differently; check rr_es().")
  }
  list(rt = pk, b = unname(b), V = unname(V))
}

# Women minus men. The two sexes are fitted on disjoint persons, so their
# covariances add, as in the contract's gap().
rr_gap_es <- function(w, m) {
  if (is.null(w) || is.null(m)) return(NULL)
  e  <- intersect(w$rt, m$rt)
  iw <- match(e, w$rt); im <- match(e, m$rt)
  V  <- w$V[iw, iw, drop = FALSE] + m$V[im, im, drop = FALSE]
  list(rt = e, g = w$b[iw] - m$b[im], V = V,
       tab = data.frame(event_time = e, gap = w$b[iw] - m$b[im],
                        se = sqrt(diag(V))))
}

# Mean over one set of horizons less the mean over another, with the full
# covariance of the gap path.
rr_window <- function(gp, late, early) {
  il <- gp$rt %in% late; ie <- gp$rt %in% early
  if (!any(il) || !any(ie)) return(c(est = NA_real_, se = NA_real_))
  cv <- il / sum(il) - ie / sum(ie)
  c(est = sum(cv * gp$g), se = sqrt(drop(t(cv) %*% gp$V %*% cv)))
}

rr_leads <- function(gp, leads) {
  i <- which(gp$rt %in% leads)
  if (!length(i)) return(c(chi2 = NA_real_, df = 0, p = NA_real_))
  st <- tryCatch(drop(t(gp$g[i]) %*% solve(gp$V[i, i, drop = FALSE]) %*% gp$g[i]),
                 error = function(e) NA_real_)
  c(chi2 = st, df = length(i), p = stats::pchisq(st, length(i), lower.tail = FALSE))
}

rr_path_by_sex <- function(d, y, k_lo, k_hi, fe = "person_id + wave") {
  fw <- rr_sa(d[d$female == 1, , drop = FALSE], y, k_lo, k_hi, fe)
  fm <- rr_sa(d[d$female == 0, , drop = FALSE], y, k_lo, k_hi, fe)
  w <- rr_es(fw, k_lo, k_hi); m <- rr_es(fm, k_lo, k_hi)
  # v-cw. The gap on common weights, inside the window so the binned ends
  # stay out. The separate-weight gap is kept as gap_separate.
  cg <- if (is.null(fw) || is.null(fm)) NULL else
    tryCatch(sa_gap_fits(fw, fm, k_range = c(k_lo, k_hi)), error = function(e) {
      cat("    common-weight gap failed for ", y, ": ", conditionMessage(e), "\n", sep = "")
      NULL })
  gap <- if (is.null(cg)) rr_gap_es(w, m) else
    list(rt = cg$k, g = cg$g, V = cg$V,
         tab = data.frame(event_time = cg$k, gap = cg$g, se = sqrt(diag(cg$V))))
  list(women = w, men = m, gap = gap, gap_separate = rr_gap_es(w, m))
}

rr_path_table <- function(p, label) {
  if (is.null(p$gap)) return(NULL)
  data.frame(series = label, event_time = p$gap$rt,
             women = p$women$b[match(p$gap$rt, p$women$rt)],
             women_se = sqrt(diag(p$women$V))[match(p$gap$rt, p$women$rt)],
             men = p$men$b[match(p$gap$rt, p$men$rt)],
             men_se = sqrt(diag(p$men$V))[match(p$gap$rt, p$men$rt)],
             gap = p$gap$g, gap_se = sqrt(diag(p$gap$V)),
             stringsAsFactors = FALSE)
}

rr_plot_gaps <- function(tab, file, ylab, title) {
  if (!requireNamespace("ggplot2", quietly = TRUE) || is.null(tab) || !nrow(tab))
    return(invisible(NULL))
  g <- ggplot2::ggplot(tab, ggplot2::aes(x = event_time, y = gap,
                                         colour = series, fill = series)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50") +
    ggplot2::geom_vline(xintercept = -0.5, linetype = "dotted") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = gap - 1.96 * gap_se,
                                      ymax = gap + 1.96 * gap_se),
                         alpha = 0.15, colour = NA) +
    ggplot2::geom_line() + ggplot2::geom_point(ggplot2::aes(shape = series)) +
    ggplot2::labs(x = "Years from first birth", y = ylab, title = title,
                  colour = NULL, fill = NULL, shape = NULL) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(legend.position = "bottom")
  ggplot2::ggsave(rr_path(file), g, width = 7, height = 4.2)
  cat("  wrote ", file, "\n", sep = "")
}

# =============================================================================
# HELPERS FOR R3 AND R7 TO R14
# =============================================================================
# A contrast as ONE outcome per person-wave: paid time less own time, in
# levels (CL_) and with each divided by its own pre-birth base (CP_). The
# estimators are linear, so a regression on CP_ returns exactly the
# difference of the two proportional estimates on the same rows, and its
# standard error carries their within-person correlation. The bases are
# treated as known here; R2's bootstrap is the check on that.
rr_pair_id <- function(p) gsub("-", "_", p, fixed = TRUE)
rr_add_cp <- function(d, pairs, w = NULL) {
  sel <- d$treated == 1 & d$female == 1 & !is.na(d$k) & d$k >= -3 & d$k <= -1
  wt  <- if (is.null(w)) rep(1, nrow(d)) else d[[w]]
  for (p in names(pairs)) {
    e <- pairs[[p]][1]; o <- pairs[[p]][2]
    ok <- sel & !is.na(d[[e]]) & !is.na(d[[o]]) & !is.na(wt)
    be <- stats::weighted.mean(d[[e]][ok], wt[ok])
    bo <- stats::weighted.mean(d[[o]][ok], wt[ok])
    d[[paste0("CL_", rr_pair_id(p))]] <- d[[e]] - d[[o]]
    d[[paste0("CP_", rr_pair_id(p))]] <- d[[e]] / be - d[[o]] / bo
  }
  d
}

# One regression; the named coefficients come back as rows.
rr_fit <- function(d, y, rhs = "post + post_f", coefs = "post_f", fe = RR_FE,
                   w = NULL, label = "") {
  keep <- !is.na(d[[y]])
  if (!is.null(w)) keep <- keep & !is.na(d[[w]]) & d[[w]] > 0
  d <- d[keep, , drop = FALSE]
  f <- stats::as.formula(paste0(y, " ~ ", rhs, " | ", fe))
  m <- tryCatch(
    if (is.null(w)) feols(f, data = d, cluster = ~person_id, notes = FALSE, warn = FALSE)
    else feols(f, data = d, weights = stats::as.formula(paste0("~", w)),
               cluster = ~person_id, notes = FALSE, warn = FALSE),
    error = function(e) NULL)
  do.call(rbind, lapply(coefs, function(cf) {
    ok <- !is.null(m) && cf %in% names(stats::coef(m))
    se <- if (ok) unname(fixest::se(m)[cf]) else NA_real_
    data.frame(label = label, outcome = y, term = cf,
               est = if (ok) unname(stats::coef(m)[cf]) else NA_real_,
               se = se, mde = mde(se), n = if (ok) stats::nobs(m) else nrow(d),
               stringsAsFactors = FALSE)
  }))
}
rr_print <- function(x) {
  op <- options(width = 250); on.exit(options(op))
  print(data.frame(lapply(x, function(v) if (is.numeric(v)) round(v, 4) else v),
                   check.names = FALSE), row.names = FALSE)
}
rr_nonempty <- function(d, ys) ys[ys %in% names(d) &
                                    !vapply(ys, function(y) all(is.na(d[[y]])), logical(1))]

# =============================================================================
# COLUMNS THAT ev MAY NOT CARRY (R3, R11 TO R14)
# =============================================================================
# The exit-chance items, the birth-wave window share and the hours
# preference are in the loader's panel, so they are read from the frame and
# joined on person and wave when ev lacks them.
rr_from_frame <- function(d, cols, context) {
  need <- setdiff(cols, names(d))
  if (!length(need)) return(d)
  fr <- tryCatch(td_frame(need_cols = c("person_id", "wave"), context = context),
                 error = function(e) NULL)
  if (is.null(fr)) return(d)
  have <- intersect(need, names(fr))
  if (length(have))
    d <- d %>% left_join(fr[, c("person_id", "wave", have), drop = FALSE],
                         by = c("person_id", "wave"))
  d
}
rr_all_na <- function(d, cols) !length(intersect(cols, names(d))) ||
  all(vapply(intersect(cols, names(d)), function(v) all(is.na(d[[v]])), logical(1)))

# =============================================================================
# REPRODUCTION. Nothing below means anything until this matches the paper.
# =============================================================================
rr_block("REPRODUCTION: the paper's frames", local({
  rr_rep <- RR_TARGETS
  rr_rep$here <- NA_real_; rr_rep$here_n <- NA_real_
  for (i in seq_len(nrow(rr_rep))) {
    d <- rr_samples[[rr_rep$sample[i]]]
    if (is.null(d)) next
    if (rr_rep$item[i] == "E1-O1") {
      r <- rr_ddd(d, "E1", "O1", FALSE, rr_rep$sample[i], "k>=0")
    } else {
      r <- rr_did(d, rr_rep$item[i], rr_rep$sample[i], "k>=0")
    }
    rr_rep$here[i] <- r$est; rr_rep$here_n[i] <- r$n
  }
  rr_rep$match <- abs(rr_rep$here - rr_rep$paper) <= RR_TOL
  print(transform(rr_rep, here = rr_num(here)), row.names = FALSE)
  cat(sprintf("\n  REPRODUCTION: %d of %d targets within %.4f of the manuscript.\n",
              sum(rr_rep$match, na.rm = TRUE), nrow(rr_rep), RR_TOL))
  if (any(!rr_rep$match | is.na(rr_rep$match)))
    cat("  *** A target misses. Resolve it before reading anything below:\n",
        "      a different frame here means different numbers everywhere.\n",
        "      For T4_any, compare here_n with paper_n; the returner rule in\n",
        "      03a may differ from MASTER's description of it.\n", sep = "")
  rr$tabs$reproduction <- rr_rep
  rr_write(rr_rep, "R0_reproduction.csv")
}))

# =============================================================================
# R1 and R4. SINGLE-MARGIN DIFFERENCES-IN-DIFFERENCES, every sample and window
# =============================================================================
if (!rr_run(c("R1", "R4"))) rr_unselected("R1/R4 differences-in-differences") else
rr_block("R1/R4: difference-in-differences by sample, window and outcome", local({
  rr_out <- list()
  for (s in names(rr_samples)) for (w in names(RR_WINDOWS)) {
    d <- RR_WINDOWS[[w]](rr_samples[[s]])
    for (y in names(RR_OUTCOMES)) {
      if (all(is.na(d[[y]]))) next
      rr_out[[length(rr_out) + 1L]] <- rr_did(d, y, s, w)
    }
  }
  rr_d <- do.call(rbind, rr_out)
  rr_d$outcome_label <- RR_OUTCOMES[rr_d$outcome]
  print(transform(rr_d[, c("sample", "window", "outcome", "est", "se", "mde",
                           "n", "base", "prop", "prop_se_base_fixed")],
                  est = rr_num(est), se = rr_num(se), mde = rr_num(mde),
                  base = rr_num(base), prop = rr_num(prop),
                  prop_se_base_fixed = rr_num(prop_se_base_fixed)),
        row.names = FALSE)
  # The exposure reading predicts that the paid-time fall shrinks where
  # exposure is held fixed. Say what the table shows, for the one row that
  # carries the argument.
  g <- function(s, w, y) rr_d$est[rr_d$sample == s & rr_d$window == w &
                                    rr_d$outcome == y][1]
  gs <- function(s, w, y) rr_d$se[rr_d$sample == s & rr_d$window == w &
                                    rr_d$outcome == y][1]
  for (s in intersect(c("preft", "ftret", "ftnow"), names(rr_samples))) {
    e2 <- g(s, "k>=2", "E1"); s2 <- gs(s, "k>=2", "E1"); e0 <- g("all", "k>=0", "E1")
    cat(sprintf("\n  %s, k >= 2: paid time %+.4f (SE %.4f), %.0f%% of the headline %+.4f%s\n",
                RR_SAMPLE_LABEL[[s]], e2, s2, 100 * e2 / e0, e0,
                if (is.finite(e2 / s2) && abs(e2 / s2) >= 1.96) "" else
                  "  [not distinguishable from zero]"))
  }
  rr$tabs$did <- rr_d
  rr_write(rr_d, "R1_R4_did_by_sample.csv")
}))

# ---- formal study beside the worker margin (R4) --------------------------
if (!rr_run("R4")) rr_unselected("R4 formal study") else
rr_block("R4: formal study enrolment", local({
  # T9's own resolution order, less edcqen: that item goes only to new-person
  # respondents who are not current students, so it is an entrant snapshot
  # rather than a series (ENTRANT_ONLY_VARS in 00compat.R). In the current
  # loader T9 resolves to full_time_student, built from edfts.
  t9_cand <- c("studying_now", "currently_studying", "enrolled_study",
               "full_time_student", "student", "edfts")
  sc <- RR_STUDY_COL
  src <- rr_samples
  if (!nzchar(sc)) sc <- intersect(t9_cand, names(rr_ev))[1]
  if (is.na(sc) || !nzchar(sc) || !sc %in% names(rr_ev)) {
    fr <- td_frame(need_cols = c("person_id", "wave"), context = "05 R4 study")
    sc2 <- if (nzchar(RR_STUDY_COL)) RR_STUDY_COL else intersect(t9_cand, names(fr))[1]
    if (is.na(sc2) || !sc2 %in% names(fr)) {
      cat("  No study column in ev or the frame. Looked for: ",
          paste(t9_cand, collapse = ", "), ". Set RR_STUDY_COL.\n", sep = "")
      rr$skip <- c(rr$skip, "R4 formal study: no study column found")
      return(invisible(NULL))
    }
    sc <- sc2
    src <- lapply(rr_samples, function(d)
      d %>% left_join(fr[, c("person_id", "wave", sc)], by = c("person_id", "wave"),
                      suffix = c(".ev", "")))
  }
  if (sc %in% c("enrolled_in_course", "enrolled_in_course_yes",
                "enrolled_in_course_comparable", "edcqen"))
    cat("  *** ", sc, " is entrant-only; the estimates below describe new\n",
        "      entrants, not a panel series.\n", sep = "")
  cat("  study column: ", sc, " (T9's coding: 1 is studying, anything else 0)\n", sep = "")
  # T9's coding: missing counts as not studying.
  for (s in names(src)) {
    x <- src[[s]][[sc]]
    x <- if (is.factor(x) || is.character(x)) rr_to01(x) else as.numeric(x)
    src[[s]]$STUDY <- as.integer(!is.na(x) & x == 1)
  }
  rr_s <- do.call(rbind, lapply(names(src), function(s)
    do.call(rbind, lapply(names(RR_WINDOWS), function(w)
      rr_did(RR_WINDOWS[[w]](src[[s]]), "STUDY", s, w)))))
  rr_s$column <- sc
  print(transform(rr_s[, c("sample", "window", "est", "se", "mde", "n")],
                  est = rr_num(est), se = rr_num(se), mde = rr_num(mde)),
        row.names = FALSE)
  # The paper's T9 arm: +0.0185 (SE 0.0098) on 54,202 person-waves. The
  # 03-series T9 was fitted with person and wave effects, not sex-by-wave,
  # so both are shown when the design's own effects miss it.
  here <- rr_s$est[rr_s$sample == "all" & rr_s$window == "k>=0"][1]
  cat(sprintf("\n  T9 check (all employed, k >= 0): %+.4f against the paper's +0.0185%s\n",
              here, if (is.finite(here) && abs(here - 0.0185) <= RR_TOL) "  MATCH" else ""))
  if (!is.finite(here) || abs(here - 0.0185) > RR_TOL) {
    m0 <- tryCatch(feols(STUDY ~ post + post_f | person_id + wave, data = src$all,
                         cluster = ~person_id, notes = FALSE, warn = FALSE),
                   error = function(e) NULL)
    if (!is.null(m0)) {
      b0 <- unname(stats::coef(m0)["post_f"])
      cat(sprintf("  with person and wave effects only: %+.4f%s\n", b0,
                  if (abs(b0 - 0.0185) <= RR_TOL) "  MATCH: T9 used these effects" else ""))
    }
  }
  rr$tabs$study <- rr_s
  rr_write(rr_s, "R4_study_by_sample.csv")
}))

# =============================================================================
# R2. THE FINANCING CONTRAST, IN LEVELS AND IN PROPORTIONS
# =============================================================================
# Own time ALONE (O3) is not contrasted. It counts own-time training with no
# paid-time training beside it, so when paid time falls, people who did both
# move into it mechanically, and a contrast against paid time would mix that
# accounting with behaviour. Its single-margin estimate stays in R1/R4.
RR_PAIRS <- list("E1-O1" = c("E1", "O1"),   # the paper's pair
                 "E1-O2" = c("E1", "O2"),   # the review's worker margin
                 "E2-O2" = c("E2", "O2"),   # both at any place
                 "E1-O4" = c("E1", "O4"))   # own time outside the workplace
RR_PAIRS <- RR_PAIRS[vapply(RR_PAIRS, function(p) all(p %in% names(rr_emp)) &&
                              !all(is.na(rr_emp[[p[1]]])) && !all(is.na(rr_emp[[p[2]]])),
                            logical(1))]

if (!rr_run("R2")) rr_unselected("R2 financing contrast") else
rr_block("R2: financing contrast, level and proportional", local({
  rr_out <- list()
  for (s in names(rr_samples)) for (w in names(RR_WINDOWS)) {
    d <- RR_WINDOWS[[w]](rr_samples[[s]])
    for (p in names(RR_PAIRS)) for (pr in c(FALSE, TRUE))
      rr_out[[length(rr_out) + 1L]] <-
        rr_ddd(d, RR_PAIRS[[p]][1], RR_PAIRS[[p]][2], pr, s, w)
  }
  rr_c <- do.call(rbind, rr_out)
  rr_c$boot_se <- NA_real_; rr_c$boot_lo <- NA_real_; rr_c$boot_hi <- NA_real_
  rr_c$boot_B  <- NA_integer_

  # The bootstrap runs on the cells in RR_BOOT_CELLS: by default all eight,
  # every sample in both windows. v-boot. The default once skipped the three
  # full-time samples at k >= 0, so Table I1 mixed bootstrap and known-base
  # standard errors across rows; a review queried it, and the three cells
  # cost about ten minutes. RR_BOOT_B = 0 skips it.
  cells <- trimws(strsplit(RR_BOOT_CELLS, ",")[[1]])
  cells <- cells[nzchar(cells)]
  alias <- c(k0 = "k>=0", k2 = "k>=2")
  cells <- do.call(rbind, lapply(cells, function(x) {
    pr <- trimws(strsplit(x, ":", fixed = TRUE)[[1]])
    w  <- if (length(pr) > 1L) pr[2] else NA_character_
    if (!is.na(w) && w %in% names(alias)) w <- unname(alias[w])
    data.frame(sample = pr[1], window = w, stringsAsFactors = FALSE)
  }))
  if (!is.null(cells)) {
    bad <- !(cells$sample %in% names(rr_samples)) | !(cells$window %in% names(RR_WINDOWS))
    if (any(bad))
      cat("  RR_BOOT_CELLS: ignoring ", paste(paste(cells$sample[bad], cells$window[bad],
                                                    sep = ":"), collapse = ", "),
          " (samples: ", paste(names(rr_samples), collapse = ", "),
          "; windows: ", paste(names(RR_WINDOWS), collapse = ", "), ")\n", sep = "")
    cells <- unique(cells[!bad, , drop = FALSE])
  }
  if (RR_BOOT_B > 0L && !is.null(cells) && nrow(cells)) {
    cat(sprintf("\n  person bootstrap, %d replicates, in %d cells\n", RR_BOOT_B, nrow(cells)))
    for (ci in seq_len(nrow(cells))) {
      s <- cells$sample[ci]; w <- cells$window[ci]
      cat(sprintf("  %s, %s\n", s, w))
      d  <- RR_WINDOWS[[w]](rr_samples[[s]])
      bt <- rr_boot(d, RR_PAIRS, RR_BOOT_B, RR_SEED)
      for (cn in colnames(bt)) {
        p <- sub("\\|.*$", "", cn); sc <- sub("^.*\\|", "", cn)
        j <- which(rr_c$sample == s & rr_c$window == w & rr_c$pair == p &
                     rr_c$scale == sc)
        v <- bt[, cn][is.finite(bt[, cn])]
        # Filled only if at least nine in ten replicates produced an estimate;
        # fewer means the resampled fits are failing and the SE is not usable.
        if (length(v) < max(2L, floor(0.9 * RR_BOOT_B)))
          cat(sprintf("    *** %s: only %d of %d replicates usable; SE left empty\n",
                      cn, length(v), RR_BOOT_B))
        if (length(j) && length(v) >= max(2L, floor(0.9 * RR_BOOT_B))) {
          rr_c$boot_se[j] <- stats::sd(v)
          rr_c$boot_lo[j] <- unname(stats::quantile(v, 0.025))
          rr_c$boot_hi[j] <- unname(stats::quantile(v, 0.975))
          rr_c$boot_B[j]  <- length(v)
        }
      }
    }
  }
  rr_c$boot_mde <- mde(rr_c$boot_se)
  print(transform(rr_c[, c("sample", "window", "pair", "scale", "est", "se",
                           "mde", "boot_se", "boot_lo", "boot_hi")],
                  est = rr_num(est), se = rr_num(se), mde = rr_num(mde),
                  boot_se = rr_num(boot_se), boot_lo = rr_num(boot_lo),
                  boot_hi = rr_num(boot_hi)), row.names = FALSE)
  cat("\n  Read the proportional rows. A level contrast is negative whenever\n",
      "  the two margins fall by the same proportion from different bases, so\n",
      "  only the proportional contrast separates the employer account from\n",
      "  time at work. Where its interval covers zero, equal proportional\n",
      "  falls are not rejected; compare the estimate with its MDE before\n",
      "  calling that a null.\n", sep = "")
  rr$tabs$contrast <- rr_c
  rr_write(rr_c, "R2_financing_contrast.csv")
}))

# =============================================================================
# R1. THE PATH FOR FULL-TIME RETURNERS, AND WHETHER IT RECOVERS
# =============================================================================
if (!rr_run("R1")) rr_unselected("R1 event paths") else
rr_block("R1: Sun-Abraham path, full-time returners, with a recovery test", local({
  rr_p <- list(); rr_rec <- list()
  for (s in intersect(c("all", "ftret", "preft"), names(rr_samples))) {
    for (y in c("E1", "E2")) {
      d <- rr_samples[[s]]
      if (all(is.na(d[[y]]))) next
      d <- d[!is.na(d[[y]]), , drop = FALSE]
      p <- rr_path_by_sex(d, y, RR_TRAIN_K[1], RR_TRAIN_K[2])
      tb <- rr_path_table(p, paste(s, y))
      if (is.null(tb)) next
      rr_p[[length(rr_p) + 1L]] <- tb
      # Individual learning predicts recovery: a mother back at full time
      # reveals her attachment, and the employer's expectation, and its
      # training, move back up. Late minus early is then positive.
      rc <- rr_window(p$gap, RR_LATE, RR_EARLY)
      lt <- rr_leads(p$gap, RR_LEADS)
      # v-armleads, 19 October: each arm's own leads, on the fits that give the
      # difference's test: mothers against never-mothers, fathers against
      # childless men. Section 7 quotes both beside Table I.9's leads_p.
      arm_lt <- function(a) if (is.null(a)) c(chi2 = NA_real_, df = 0, p = NA_real_) else
        rr_leads(list(rt = a$rt, g = a$b, V = a$V), RR_LEADS)
      ltw <- arm_lt(p$women); ltm <- arm_lt(p$men)
      rr_rec[[length(rr_rec) + 1L]] <- data.frame(
        sample = s, outcome = y,
        early = paste(range(RR_EARLY), collapse = ".."),
        late = paste(range(RR_LATE), collapse = ".."),
        recovery = rc[["est"]], recovery_se = rc[["se"]],
        recovery_mde = mde(rc[["se"]]),
        leads_chi2 = lt[["chi2"]], leads_df = lt[["df"]], leads_p = lt[["p"]],
        leads_chi2_women = ltw[["chi2"]], leads_df_women = ltw[["df"]], leads_p_women = ltw[["p"]],
        leads_chi2_men = ltm[["chi2"]], leads_df_men = ltm[["df"]], leads_p_men = ltm[["p"]],
        stringsAsFactors = FALSE)
    }
  }
  rr_pt <- do.call(rbind, rr_p); rr_rc <- do.call(rbind, rr_rec)
  if (!is.null(rr_rc)) {
    print(transform(rr_rc, recovery = rr_num(recovery),
                    recovery_se = rr_num(recovery_se),
                    recovery_mde = rr_num(recovery_mde),
                    leads_chi2 = round(leads_chi2, 2), leads_p = round(leads_p, 3)),
          row.names = FALSE)
    cat("\n  recovery = mean gap over the late horizons less the mean over the\n",
        "  early ones. Positive is recovery. A value near zero with a small\n",
        "  MDE is evidence against individual learning; a value near zero\n",
        "  with a large one is not evidence of anything.\n", sep = "")
  }
  rr$tabs$ft_paths <- rr_pt; rr$tabs$recovery <- rr_rc
  if (!is.null(rr_pt)) rr_write(rr_pt, "R1_event_paths.csv")
  if (!is.null(rr_rc)) rr_write(rr_rc, "R1_recovery_test.csv")
  if (!is.null(rr_pt))
    rr_plot_gaps(rr_pt[rr_pt$series %in% c("all E1", "ftret E1", "preft E1"), ],
                 "R1_paid_time_paths.pdf", "Women minus men",
                 "Paid-time training at the workplace, by sample")
}))

# =============================================================================
# R3. EXPOSURE IN THE REFERENCE YEAR
# =============================================================================
# HILDA's annual activity calendar records each respondent's labour-force
# activity since the start of the previous financial year, and the release
# derives from it the per cent of the last financial year spent in jobs
# (capj). The loader keeps it as fy_share_employed. It covers July to June,
# so it overlaps most, not all, of the training items' twelve months before
# the interview. RR_EXPOSURE_COL names another column if you build one from
# the raw calendar, aligned to the interview.
#
# The column may be a share (0-1), months (0-12) or a percentage (0-100);
# it is converted to a share either way. Two variants, mothers minus
# fathers, every sample and both windows, on the paid-time margin, own time
# at any place and the proportional contrasts:
#   (a) only person-waves employed for the whole period;
#   (b) exposure as a control. It is an outcome of the birth, so this is the
#       arithmetic of the channel, not an identified effect.
if (!rr_run("R3")) rr_unselected("R3 exposure") else
rr_block("R3: exposure in the reference year", local({
  xc <- RR_EXPOSURE_COL
  if (!nzchar(xc)) xc <- "fy_share_employed"
  d0 <- rr_from_frame(rr_samples$all, xc, "05 R3 exposure")
  if (rr_all_na(d0, xc)) {
    cat("  ", xc, " is on neither ev nor the frame, so this block is skipped.\n",
        "  Add the calendar's per cent of the last financial year spent in jobs\n",
        "  (capj) to the loader as fy_share_employed, or set RR_EXPOSURE_COL.\n", sep = "")
    rr$skip <- c(rr$skip, paste0("R3 exposure: no ", xc))
    return(invisible(NULL))
  }
  x  <- suppressWarnings(as.numeric(d0[[xc]]))
  mx <- max(x, na.rm = TRUE)
  scale <- if (mx > 12.5) 100 else if (mx > 1.5) 12 else 1
  cat(sprintf("  exposure: %s, read as %s | answered on %.1f%% of employed person-waves\n",
              xc, c(`1` = "a share", `12` = "months", `100` = "a percentage")[[as.character(scale)]],
              100 * mean(!is.na(x))))
  expo <- d0[, c("person_id", "wave")]; expo$EXPO <- pmin(pmax(x / scale, 0), 1)
  pairs <- RR_PAIRS[intersect(c("E1-O2", "E1-O1"), names(RR_PAIRS))]
  ys <- c("E1", "O2", paste0("CP_", rr_pair_id(names(pairs))))
  out <- list()
  for (s in names(rr_samples)) {
    d <- rr_add_cp(rr_samples[[s]], pairs)
    d <- d %>% left_join(expo, by = c("person_id", "wave"))
    for (w in names(RR_WINDOWS)) {
      dw <- RR_WINDOWS[[w]](d)
      dw <- dw[!is.na(dw$EXPO), , drop = FALSE]
      for (y in rr_nonempty(dw, ys)) {
        base <- rr_fit(dw, y, label = "exposure observed")
        a <- rr_fit(dw[dw$EXPO >= 0.999, , drop = FALSE], y,
                    label = "(a) employed the whole period")
        b <- rr_fit(dw, y, "post + post_f + EXPO", "post_f",
                    label = "(b) exposure as a control")
        r <- rbind(base, a, b); r$sample <- s; r$window <- w
        out[[length(out) + 1L]] <- r
      }
    }
  }
  tab <- do.call(rbind, out); tab$column <- xc
  mm <- rr_samples$all %>% left_join(expo, by = c("person_id", "wave"))
  mm <- mm[mm$treated == 1 & mm$female == 1 & !is.na(mm$k) & mm$k >= -3 &
             mm$k <= 4 & !is.na(mm$EXPO), , drop = FALSE]
  cat("\n  mothers' mean share of the period employed, by event time:\n")
  em <- stats::aggregate(EXPO ~ k, data = mm, FUN = mean)
  rr_print(em)
  # The text quotes the share before the birth (k = -3 to -1 pooled), at the
  # birth wave and a year later.
  rr_write(data.frame(period = c("before", ifelse(em$k < 0, paste0("km", -em$k), paste0("k", em$k))),
                      EXPO = c(mean(mm$EXPO[mm$k < 0]), em$EXPO),
                      n = c(sum(mm$k < 0), as.vector(table(factor(mm$k, levels = em$k)))),
                      stringsAsFactors = FALSE), "R3_exposure_means.csv")
  key <- tab[tab$sample %in% c("all", "ftret") , ]
  cat("\n")
  rr_print(key[, c("sample", "window", "label", "outcome", "est", "se", "mde", "n")])
  cat("\n  All samples are in R3_exposure.csv. CP_ rows are proportional contrasts,\n",
      "  bases as known.\n", sep = "")
  rr$tabs$exposure <- tab
  rr_write(tab, "R3_exposure.csv")
}))

# =============================================================================
# R5. EARNINGS, WITH THE NON-EMPLOYED KEPT AS ZEROS
# =============================================================================
if (!rr_run("R5")) rr_unselected("R5 earnings") else
rr_block("R5: earnings event study, non-employed as zeros", local({
  # The loader's own columns first. real_annual_income is
  # coalesce(fy_wages_inc_ss, fy_wages), which is wsfes where observed,
  # deflated to Wave 24 dollars. The nominal columns are deflated here with
  # the Wage Price Index series MASTER_training_y.R uses.
  ec <- RR_EARN_COL
  cand <- c("real_annual_income", "annual_income", "fy_wages_inc_ss",
            "earnings_real", "wsfes")
  src <- rr_ev
  if (!nzchar(ec)) ec <- intersect(cand, names(src))[1]
  if (is.na(ec) || !nzchar(ec) || !ec %in% names(src)) {
    fr <- td_frame(need_cols = c("person_id", "wave"), context = "05 R5 earnings")
    ec2 <- if (nzchar(RR_EARN_COL)) RR_EARN_COL else intersect(cand, names(fr))[1]
    if (is.na(ec2) || !ec2 %in% names(fr))
      stop("no earnings column in ev or the frame. Looked for: ",
           paste(cand, collapse = ", "), ". Set RR_EARN_COL.")
    ec <- ec2
    src <- rr_ev %>% left_join(fr[, c("person_id", "wave", ec)],
                               by = c("person_id", "wave"),
                               suffix = c("", ".frame"))
  }
  if (!"age" %in% names(src)) {
    fr <- td_frame(need_cols = c("person_id", "wave", "age"), context = "05 R5 age")
    src <- src %>% left_join(fr[, c("person_id", "wave", "age")],
                             by = c("person_id", "wave"))
  }
  earn <- as.numeric(src[[ec]])
  if (ec %in% c("annual_income", "fy_wages_inc_ss", "fy_wages", "wsfes")) {
    wpi <- data.frame(wave = 12:24,
                      deflator = 149.6 / c(110.9, 114.6, 117.6, 120.4, 123.0,
                                           125.4, 127.9, 130.9, 133.7, 135.7,
                                           138.9, 143.7, 149.6))
    earn <- earn * wpi$deflator[match(src$wave, wpi$wave)]
    cat("  ", ec, " is nominal: deflated by the WPI to Wave 24 dollars.\n", sep = "")
  }
  earn[which(earn < 0)] <- NA_real_
  # Zero for the non-employed, as the earnings sample in Section IV defines.
  earn[which(is.na(earn) & src$employed == 0)] <- 0
  src$ASINH_EARN <- asinh(earn)
  src$EARN_K     <- earn / 1000
  cat(sprintf("  earnings column: %s | %s person-waves with earnings, %.1f%% zeros\n",
              ec, format(sum(!is.na(earn)), big.mark = ","),
              100 * mean(earn == 0, na.rm = TRUE)))
  rr_e <- list(); rr_w <- list()
  for (y in c("ASINH_EARN", "EARN_K")) for (fe in c("person_id + wave",
                                                    "person_id + wave + age")) {
    d <- src[!is.na(src[[y]]) & (fe == "person_id + wave" | !is.na(src$age)), ,
             drop = FALSE]
    p <- rr_path_by_sex(d, y, RR_EARN_K[1], RR_EARN_K[2], fe)
    lab <- paste(y, if (grepl("age", fe)) "age absorbed" else "pre-specified form")
    tb <- rr_path_table(p, lab)
    if (is.null(tb)) next
    rr_e[[length(rr_e) + 1L]] <- tb
    wc <- rr_window(p$gap, RR_W_LATE, RR_W_EARLY)
    lt <- rr_leads(p$gap, RR_LEADS)
    rr_w[[length(rr_w) + 1L]] <- data.frame(
      series = lab, k0 = p$gap$g[p$gap$rt == 0][1],
      k0_se = sqrt(diag(p$gap$V))[p$gap$rt == 0][1],
      late_minus_early = wc[["est"]], late_minus_early_se = wc[["se"]],
      leads_chi2 = lt[["chi2"]], leads_df = lt[["df"]], leads_p = lt[["p"]],
      stringsAsFactors = FALSE)
  }
  rr_et <- do.call(rbind, rr_e); rr_wt <- do.call(rbind, rr_w)
  if (!is.null(rr_wt))
    print(transform(rr_wt, k0 = rr_num(k0), k0_se = rr_num(k0_se),
                    late_minus_early = rr_num(late_minus_early),
                    late_minus_early_se = rr_num(late_minus_early_se),
                    leads_chi2 = round(leads_chi2, 2), leads_p = round(leads_p, 3)),
          row.names = FALSE)
  cat("\n  Earnings with zeros compound participation, hours and the wage, so\n",
      "  they fall at the birth by construction. The path bounds the wage\n",
      "  sample's selection; it does not test whether the WAGE response waits.\n",
      sep = "")
  rr$tabs$earnings_paths <- rr_et; rr$tabs$earnings_windows <- rr_wt
  if (!is.null(rr_et)) rr_write(rr_et, "R5_earnings_paths.csv")
  if (!is.null(rr_wt)) rr_write(rr_wt, "R5_earnings_windows.csv")
  if (!is.null(rr_et))
    rr_plot_gaps(rr_et[grepl("^ASINH_EARN", rr_et$series), ],
                 "R5_earnings_path.pdf", "Women minus men, asinh earnings",
                 "Earnings around first birth, non-employed as zeros")
}))

# =============================================================================
# R6. THE TWO WAGE PATHS, SIDE BY SIDE
# =============================================================================
if (!rr_run("R6")) rr_unselected("R6 wage paths") else
rr_block("R6: pre-specified and age-absorbed wage paths", local({
  f <- td_out("results", "cs_event_study_results.rds")
  if (!file.exists(f))
    stop("cs_event_study_results.rds not found at ", f,
         ". Run CS_event_study.R first.")
  cs <- readRDS(f)
  want <- c("gap ln_hourly_wage_real", "gap ln_hourly_wage_real_agefe")
  have <- intersect(want, names(cs))
  if (length(have) < 2L)
    stop("the results file lacks ", paste(setdiff(want, have), collapse = ", "),
         ". CS_event_study.R writes the age arm only when `age` is on its frame.")
  wt <- do.call(rbind, lapply(have, function(nm) {
    x <- cs[[nm]]
    data.frame(series = if (grepl("agefe", nm)) "age absorbed" else "pre-specified",
               event_time = x$event_time, gap = x$gap, gap_se = x$se,
               stringsAsFactors = FALSE)
  }))
  both <- merge(wt[wt$series == "pre-specified", c("event_time", "gap", "gap_se")],
                wt[wt$series == "age absorbed",  c("event_time", "gap", "gap_se")],
                by = "event_time", suffixes = c("_prespec", "_age"))
  both$difference <- both$gap_age - both$gap_prespec
  print(data.frame(lapply(both, function(v) if (is.numeric(v)) rr_num(v) else v)),
        row.names = FALSE)
  rr$tabs$wage_paths <- wt
  rr_write(wt, "R6_wage_paths.csv")
  rr_plot_gaps(wt, "R6_wage_paths.pdf", "Women minus men, log hourly wage",
               "Wage gap around first birth: pre-specified and age-absorbed")
  cat("  To draw both in Figure 2, add the age-absorbed gap to the wage panel\n",
      "  that RUN_FIGURES_PAPER.R builds from timing_paths.csv.\n", sep = "")
}))

# =============================================================================
# R6b. THE WAGE WINDOW CONTRAST AND LEAD TEST, WITH THE FULL COVARIANCE
# =============================================================================
# The paper reports late minus early as -0.058 (SE 0.025) and the joint lead
# test as chi-squared(7) = 7.92. On a sunab fit, vcov() is the cohort-by-period
# covariance while coef() is aggregated, so a contrast built from the two
# directly is misaligned. This re-estimates the wage path and computes both
# statistics from the rebuilt covariance. Matching point estimates with
# different standard errors would say the published SE came from the wrong
# matrix. Periods beyond the window are binned here; if CS_event_study.R
# bins at |k| >= 8, the late horizons differ for that reason too.
if (!rr_run(c("R6", "R6B"))) rr_unselected("R6b wage windows") else
rr_block("R6b: wage window contrast and lead test, full covariance", local({
  if (!"ln_hourly_wage_real" %in% names(rr_ev))
    stop("ln_hourly_wage_real is not on ev.")
  src <- rr_ev[!is.na(rr_ev$ln_hourly_wage_real), , drop = FALSE]
  if (!"age" %in% names(src)) {
    fr <- td_frame(need_cols = c("person_id", "wave", "age"), context = "05 R6b age")
    src <- src %>% left_join(fr[, c("person_id", "wave", "age")],
                             by = c("person_id", "wave"))
  }
  # v-cw. THE PAPER'S VALUES ARE READ, NOT TYPED. The literals -0.058, 0.025
  # and 7.92 were the separate-weight results. CS_event_study.R now writes
  # the common-weight ones to wage_path_tests.csv, which is where the paper's
  # wptest.* numbers come from, so the comparison reads that file. A missing
  # file or row gives NA.
  .wpt <- tryCatch(utils::read.csv(td_out("results", "wage_path_tests.csv"),
                                   stringsAsFactors = FALSE), error = function(e) NULL)
  .pub <- function(fe, q, col) {
    path <- if (grepl("age", fe)) "age absorbed" else "plain"
    if (is.null(.wpt) || !all(c("path", "quantity", col) %in% names(.wpt))) return(NA_real_)
    v <- .wpt[[col]][.wpt$path == path & .wpt$quantity == q]
    if (length(v)) as.numeric(v[1]) else NA_real_
  }
  rr_ww <- list()
  for (fe in c("person_id + wave", "person_id + wave + age")) {
    d <- if (grepl("age", fe)) src[!is.na(src$age), , drop = FALSE] else src
    p <- rr_path_by_sex(d, "ln_hourly_wage_real", RR_EARN_K[1], RR_EARN_K[2], fe)
    if (is.null(p$gap)) next
    wc <- rr_window(p$gap, RR_W_LATE, RR_W_EARLY)
    lt <- rr_leads(p$gap, RR_LEADS)
    rr_ww[[length(rr_ww) + 1L]] <- data.frame(
      series = if (grepl("age", fe)) "age absorbed" else "pre-specified",
      late_minus_early = wc[["est"]], late_minus_early_se = wc[["se"]],
      paper_late_minus_early = .pub(fe, "late window minus early window", "estimate"),
      paper_se = .pub(fe, "late window minus early window", "se"),
      leads_chi2 = lt[["chi2"]], leads_df = lt[["df"]], leads_p = lt[["p"]],
      paper_chi2 = .pub(fe, "joint test, all leads zero", "stat"),
      stringsAsFactors = FALSE)
  }
  rr_wwt <- do.call(rbind, rr_ww)
  if (!is.null(rr_wwt)) {
    print(data.frame(lapply(rr_wwt, function(v) if (is.numeric(v)) round(v, 4) else v)),
          row.names = FALSE)
    rr$tabs$wage_windows <- rr_wwt
    rr_write(rr_wwt, "R6b_wage_windows.csv")
  }
}))

# =============================================================================
# R7. HOURS ACCOUNTING
# =============================================================================
# How much of each fall would the change in hours predict?
#
# Step one estimates how each training margin moves with usual weekly hours,
# within person, among people with no birth: never-treated men and women, by
# sex, with hours in five bands and 35 to 44 as the reference. Step two
# subtracts that response at every person-wave's own hours. Step three
# re-runs the design on what is left. What disappears is the part hours
# account for. What survives is the part they do not, PROVIDED hours move
# training for mothers as they do for people without a birth.
#
# The second version estimates step one on everyone, mothers included, with
# the birth indicator in the first stage: sequential g-estimation (Acharya,
# Blackwell and Sen 2016). It drops the transport assumption, but counts any
# response specific to mothers' hours as "hours".
#
# Hours are an outcome of the birth, which is why they are removed this way
# rather than added as a control, which would be the bad control MASTER's
# 9.3 arms warn about.
#
# The person bootstrap re-estimates step one in every replicate, so the
# intervals carry its uncertainty. It runs on the transport version for
# E1, O2 and O4 and their contrasts; RR_BOOT_B = 0 skips it.
RR_HBANDS <- c("1-15", "16-24", "25-34", "35-44", "45+")
if (!rr_run("R7")) rr_unselected("R7 hours accounting") else
rr_block("R7: hours accounting", local({
  hc <- "hours_worked_clean"
  d  <- rr_samples$all
  if (!hc %in% names(d)) {
    fr <- tryCatch(td_frame(need_cols = c("person_id", "wave", hc), context = "05 R7 hours"),
                   error = function(e) NULL)
    if (is.null(fr)) {
      cat("  ", hc, " is on neither ev nor the frame; block skipped.\n", sep = "")
      rr$skip <- c(rr$skip, paste0("R7 hours accounting: no ", hc))
      return(invisible(NULL))
    }
    d  <- d %>% left_join(fr[, c("person_id", "wave", hc)], by = c("person_id", "wave"))
  }
  d$H <- suppressWarnings(as.numeric(d[[hc]]))
  n0  <- nrow(d)
  d   <- d[is.finite(d$H) & d$H > 0, , drop = FALSE]
  cat(sprintf("  hours: %s | %s of %s employed person-waves carry hours\n", hc,
              format(nrow(d), big.mark = ","), format(n0, big.mark = ",")))
  d$HB <- factor(as.character(cut(d$H, c(-Inf, 15, 24, 34, 44, Inf),
                                  labels = RR_HBANDS)), levels = RR_HBANDS)
  d$HB <- stats::relevel(d$HB, ref = "35-44")

  ys    <- rr_nonempty(d, c("E1", "E2", "O1", "O2", "O4"))
  pairs <- RR_PAIRS[intersect(c("E1-O2", "E1-O4", "E1-O1"), names(RR_PAIRS))]
  ys_b  <- intersect(c("E1", "O2", "O4"), ys)
  pr_b  <- pairs[names(pairs) %in% c("E1-O2", "E1-O4")]

  # How far hours move, mothers minus fathers.
  hh <- rbind(rr_fit(d, "H", label = "k>=0"),
              rr_fit(RR_WINDOWS[["k>=2"]](d), "H", label = "k>=2"))
  cat("\n  usual weekly hours, mothers minus fathers:\n")
  rr_print(hh[, c("label", "est", "se", "n")])
  rr_write(hh, "R7_hours_did.csv")

  gam <- function(dd, y, sex, version) {
    s <- dd[dd$female == sex & !is.na(dd[[y]]), , drop = FALSE]
    if (version == "transport") s <- s[s$treated == 0, , drop = FALSE]
    f <- stats::as.formula(paste0(y, " ~ ",
                                  if (version == "transport") "" else "post + ",
                                  "i(HB, ref = '35-44') | person_id + wave"))
    m <- tryCatch(feols(f, data = s, notes = FALSE, warn = FALSE),
                  error = function(e) NULL)
    g <- stats::setNames(rep(0, length(RR_HBANDS)), RR_HBANDS)
    if (!is.null(m)) {
      cf <- stats::coef(m); nm <- sub("^HB::", "", names(cf))
      k  <- nm %in% RR_HBANDS
      g[nm[k]] <- cf[k]
    }
    g
  }
  # One pass: first stage, demediated outcomes, both windows, the contrasts.
  # Returns a named vector, window|item|statistic, so the bootstrap reuses it.
  one <- function(dd, version, ys, pairs, keep_gamma = FALSE) {
    out <- c(); gt <- list()
    for (y in ys) {
      gw <- gam(dd, y, 1, version); gm <- gam(dd, y, 0, version)
      gt[[y]] <- rbind(women = gw, men = gm)
      hb <- as.character(dd$HB)
      dd[[paste0(y, "_net")]] <- dd[[y]] - ifelse(dd$female == 1, gw[hb], gm[hb])
    }
    for (w in names(RR_WINDOWS)) {
      dw <- RR_WINDOWS[[w]](dd)
      for (y in ys) {
        b  <- rr_base(dw, y)
        e0 <- rr_coef(dw, y); e1 <- rr_coef(dw, paste0(y, "_net"))
        out[paste(w, y, "orig", sep = "|")]      <- e0
        out[paste(w, y, "net", sep = "|")]       <- e1
        out[paste(w, y, "orig_prop", sep = "|")] <- e0 / b
        out[paste(w, y, "net_prop", sep = "|")]  <- e1 / b
      }
      for (p in names(pairs)) {
        e <- pairs[[p]][1]; o <- pairs[[p]][2]
        for (v in c("orig_prop", "net_prop"))
          out[paste(w, p, v, sep = "|")] <-
            out[paste(w, e, v, sep = "|")] - out[paste(w, o, v, sep = "|")]
      }
    }
    if (keep_gamma) attr(out, "gamma") <- gt
    out
  }

  res <- list(transport      = one(d, "transport", ys, pairs, keep_gamma = TRUE),
              `g-estimation` = one(d, "all", ys, pairs))

  cat("\n  How training moves with hours among people with no birth (transport\n",
      "  first stage; difference from 35-44 hours):\n", sep = "")
  gtab <- do.call(rbind, lapply(names(attr(res$transport, "gamma")), function(y) {
    g <- attr(res$transport, "gamma")[[y]]
    data.frame(outcome = y, sex = rownames(g), g[, setdiff(RR_HBANDS, "35-44"), drop = FALSE],
               check.names = FALSE, stringsAsFactors = FALSE)
  }))
  rr_print(gtab)

  long <- do.call(rbind, lapply(names(res), function(v) {
    o <- res[[v]]; pp <- do.call(rbind, strsplit(names(o), "|", fixed = TRUE))
    data.frame(version = v, window = pp[, 1], item = pp[, 2], stat = pp[, 3],
               est = unname(o), stringsAsFactors = FALSE)
  }))

  # ---- bootstrap, transport version ------------------------------------
  bse <- NULL
  if (RR_BOOT_B > 0L && length(ys_b)) {
    cat(sprintf("\n  person bootstrap, %d replicates, first stage re-estimated each time\n",
                RR_BOOT_B))
    set.seed(RR_SEED)
    by_p <- split(seq_len(nrow(d)), as.character(d$person_id)); ids <- names(by_p)
    bt <- NULL; t0 <- Sys.time()
    for (b in seq_len(RR_BOOT_B)) {
      draw <- sample(ids, length(ids), replace = TRUE); rows <- by_p[draw]
      db <- d[unlist(rows, use.names = FALSE), , drop = FALSE]
      db$person_id <- rep.int(seq_along(draw), lengths(rows))
      ob <- one(db, "transport", ys_b, pr_b)
      if (is.null(bt)) bt <- matrix(NA_real_, RR_BOOT_B, length(ob),
                                    dimnames = list(NULL, names(ob)))
      bt[b, names(ob)] <- ob
      if (b %% 50L == 0L || b == RR_BOOT_B)
        cat(sprintf("    bootstrap %d of %d (%.1f min)\n", b, RR_BOOT_B,
                    as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    }
    sdv <- function(v) { v <- v[is.finite(v)]
      if (length(v) >= max(2L, floor(0.9 * RR_BOOT_B))) stats::sd(v) else NA_real_ }
    cn <- colnames(bt)
    bse <- data.frame(key = cn, boot_se = apply(bt, 2, sdv), stringsAsFactors = FALSE)
    # The part hours explain, orig less net, with its own bootstrap SE.
    base_keys <- unique(sub("\\|(orig|net)(_prop)?$", "", cn))
    for (kk in base_keys) for (sfx in c("", "_prop")) {
      a <- paste0(kk, "|orig", sfx); z <- paste0(kk, "|net", sfx)
      if (all(c(a, z) %in% cn))
        bse <- rbind(bse, data.frame(key = paste0(kk, "|explained", sfx),
                                     boot_se = sdv(bt[, a] - bt[, z]),
                                     stringsAsFactors = FALSE))
    }
  }

  # ---- readable tables --------------------------------------------------
  get <- function(v, w, i, s) { x <- long$est[long$version == v & long$window == w &
                                                 long$item == i & long$stat == s]
                                if (length(x)) x[1] else NA_real_ }
  gse <- function(w, i, s) { if (is.null(bse)) return(NA_real_)
                             x <- bse$boot_se[bse$key == paste(w, i, s, sep = "|")]
                             if (length(x)) x[1] else NA_real_ }
  single <- do.call(rbind, lapply(names(res), function(v)
    do.call(rbind, lapply(names(RR_WINDOWS), function(w)
      do.call(rbind, lapply(ys, function(y) {
        o <- get(v, w, y, "orig"); n <- get(v, w, y, "net")
        data.frame(version = v, window = w, outcome = y, orig = o, net_of_hours = n,
                   explained = o - n,
                   share_explained = if (y %in% c("E1", "E2")) (o - n) / o else NA_real_,
                   net_boot_se = if (v == "transport") gse(w, y, "net") else NA_real_,
                   explained_boot_se = if (v == "transport") gse(w, y, "explained") else NA_real_,
                   stringsAsFactors = FALSE)
      }))))))
  contr <- do.call(rbind, lapply(names(res), function(v)
    do.call(rbind, lapply(names(RR_WINDOWS), function(w)
      do.call(rbind, lapply(names(pairs), function(p) {
        data.frame(version = v, window = w, pair = p,
                   orig_prop = get(v, w, p, "orig_prop"),
                   net_of_hours_prop = get(v, w, p, "net_prop"),
                   net_boot_se = if (v == "transport") gse(w, p, "net_prop") else NA_real_,
                   explained_boot_se = if (v == "transport") gse(w, p, "explained_prop") else NA_real_,
                   stringsAsFactors = FALSE)
      }))))))
  cat("\n  Each margin, before and after removing what hours predict:\n")
  rr_print(single)
  cat("\n  The proportional contrast, before and after:\n")
  rr_print(contr)
  cat("\n  net_of_hours is the birth effect with hours held where they were, IF\n",
      "  hours move training for mothers as for others (transport) or as fitted\n",
      "  on everyone (g-estimation). A proportional contrast that survives the\n",
      "  removal is not hours; one that disappears is.\n", sep = "")
  rr$tabs$hours <- list(single = single, contrast = contr, gamma = gtab,
                        hours_change = hh, long = long, boot = bse)
  rr_write(single, "R7_hours_single.csv")
  rr_write(contr, "R7_hours_contrast.csv")
  rr_write(gtab, "R7_hours_first_stage.csv")
}))

# =============================================================================
# R8. TWO CLOCKS: THE BIRTH AND THE SWITCH TO PART TIME
# =============================================================================
# Women only, mothers against never-mothers, person and wave effects, the
# employed frame.
#
# (a) Every mother. post is the change after the birth in full-time waves;
#     pt is the part-time difference among women with no birth; post_pt is
#     any EXTRA part-time difference for mothers after the birth. A post that
#     keeps most of the fall points to the birth; a post near zero with a
#     large post_pt points to hours.
# (b) Late switchers: mothers full time at k = 1 whose first part-time wave
#     comes at k = 2 or later. at_birth is the birth wave, k1 and k2_plus the
#     change at k = 1 and from k = 2, and after_switch the EXTRA change once
#     they go part time. If paid time falls at the birth and not again at the
#     switch, the response is to the birth; if it falls at the switch, it is
#     hours. k = 1 is kept apart because its twelve-month reference period
#     can still overlap leave.
# (c) Mothers full time at every observed wave from k = 1: the fall among
#     mothers who never work part time after the birth, at k = 1 and from
#     k = 2.
# All three condition on hours after the birth, so read them as descriptions
# of where the fall sits, not as causal splits.
if (!rr_run("R8")) rr_unselected("R8 two clocks") else
rr_block("R8: two clocks, the birth and the switch to part time", local({
  if (is.na(rr_ft_col)) {
    cat("  No full-time column on ev; block skipped.\n")
    rr$skip <- c(rr$skip, "R8 two clocks: no full-time column")
    return(invisible(NULL))
  }
  pairs <- RR_PAIRS[intersect(c("E1-O2", "E1-O4", "E1-O1"), names(RR_PAIRS))]
  d  <- rr_samples$all
  d  <- rr_add_cp(d[d$female == 1 & !is.na(d$ft), , drop = FALSE], pairs)
  # v-levelcontrast, 19 October: the level contrasts too, so the difference has its own SE.
  ys <- rr_nonempty(d, c("E1", "O2", "O4", paste0("CL_", rr_pair_id(names(pairs))),
                        paste0("CP_", rr_pair_id(names(pairs)))))
  fe <- "person_id + wave"
  d$pt <- 1 - d$ft
  d$post_pt <- d$post * d$pt
  a <- do.call(rbind, lapply(names(RR_WINDOWS), function(w)
    do.call(rbind, lapply(ys, function(y)
      rr_fit(RR_WINDOWS[[w]](d), y, "post + post_pt + pt", c("post", "post_pt", "pt"), fe,
             label = paste0("(a) all mothers, ", w))))))

  m  <- d[d$treated == 1 & !is.na(d$k), , drop = FALSE]
  k1 <- unique(m[m$k == 1, c("person_id", "ft")]); names(k1)[2] <- "ft1"
  k1 <- k1[!duplicated(k1$person_id), , drop = FALSE]
  sw <- m[m$k >= 1 & m$ft == 0, c("person_id", "k"), drop = FALSE]
  sw <- if (nrow(sw)) stats::aggregate(k ~ person_id, data = sw, FUN = min) else
    data.frame(person_id = m$person_id[0], k = numeric(0))
  names(sw)[2] <- "s"
  cl <- merge(merge(data.frame(person_id = unique(m$person_id)), k1, by = "person_id",
                    all.x = TRUE), sw, by = "person_id", all.x = TRUE)
  cl$group <- ifelse(!is.na(cl$ft1) & cl$ft1 == 1 & !is.na(cl$s) & cl$s >= 2, "late switcher",
              ifelse(!is.na(cl$s) & cl$s == 1, "part time at k = 1",
              ifelse(!is.na(cl$ft1) & cl$ft1 == 1 & is.na(cl$s), "full time from k = 1",
                     "other (not employed at k = 1, or unobserved)")))
  cat("  mothers by post-birth hours path:\n")
  print(table(cl$group))

  late <- cl[cl$group == "late switcher", c("person_id", "s")]
  b <- NULL
  if (nrow(late) >= 30) {
    dl <- merge(d[d$treated == 0 | d$person_id %in% late$person_id, , drop = FALSE],
                late, by = "person_id", all.x = TRUE)
    dl$at_birth     <- as.numeric(dl$treated == 1 & dl$k == 0)
    dl$k1           <- as.numeric(dl$treated == 1 & dl$k == 1)
    dl$k2_plus      <- as.numeric(dl$treated == 1 & dl$k >= 2)
    dl$after_switch <- as.numeric(dl$treated == 1 & !is.na(dl$s) & dl$k >= dl$s)
    b <- do.call(rbind, lapply(ys, function(y)
      rr_fit(dl, y, "at_birth + k1 + k2_plus + after_switch",
             c("at_birth", "k1", "k2_plus", "after_switch"), fe, label = "(b) late switchers")))
  } else cat("  fewer than 30 late switchers; (b) skipped.\n")

  ftid <- cl$person_id[cl$group == "full time from k = 1"]
  cc <- NULL
  if (length(ftid) >= 30) {
    dc <- d[d$treated == 0 | d$person_id %in% ftid, , drop = FALSE]
    dc$at_birth <- as.numeric(dc$treated == 1 & dc$k == 0)
    dc$k1       <- as.numeric(dc$treated == 1 & dc$k == 1)
    dc$k2_plus  <- as.numeric(dc$treated == 1 & dc$k >= 2)
    cc <- do.call(rbind, lapply(ys, function(y)
      rr_fit(dc, y, "at_birth + k1 + k2_plus", c("at_birth", "k1", "k2_plus"), fe,
             label = "(c) full time from k = 1")))
  }
  tab <- rbind(a, b, cc)
  rr_print(tab[, c("label", "outcome", "term", "est", "se", "mde", "n")])
  cat("\n  CP_ rows are the proportional contrast, paid time less the worker\n",
      "  margin, each divided by its own base (bases treated as known).\n", sep = "")
  rr$tabs$two_clocks <- tab
  rr$tabs$two_clocks_groups <- as.data.frame(table(cl$group))
  rr_write(tab, "R8_two_clocks.csv")
}))

# =============================================================================
# R9. THE PROPORTIONAL GAP, YEAR BY YEAR
# =============================================================================
# The Sun-Abraham path of the proportional contrast itself: paid time less
# the worker margin, each divided by its own pre-birth base, women minus
# men, by year since the birth. An asymmetry that opens at the birth and
# stays flat points to the event; one that grows over the years in which
# mothers move to part time points to hours. Mothers' part-time share is
# printed beside it for that comparison.
if (!rr_run("R9")) rr_unselected("R9 proportional gap by year") else
rr_block("R9: the proportional gap, year by year", local({
  pairs <- RR_PAIRS[intersect(c("E1-O2", "E1-O4", "E1-O1"), names(RR_PAIRS))]
  d <- rr_add_cp(rr_samples$all, pairs)
  pts <- if (!is.na(rr_ft_col))
    d %>% filter(treated == 1, female == 1, !is.na(k), k >= RR_TRAIN_K[1],
                 k <= RR_TRAIN_K[2], !is.na(ft)) %>%
      group_by(k) %>% summarise(mothers_pt_share = mean(ft == 0), n_mothers = dplyr::n(),
                                .groups = "drop") %>% as.data.frame() else NULL
  tabs <- list(); wins <- list()
  for (p in names(pairs)) {
    y  <- paste0("CP_", rr_pair_id(p))
    dd <- d[!is.na(d[[y]]), , drop = FALSE]
    pth <- rr_path_by_sex(dd, y, RR_TRAIN_K[1], RR_TRAIN_K[2])
    tb <- rr_path_table(pth, p)
    if (is.null(tb)) next
    tabs[[p]] <- tb
    g1 <- rr_window(pth$gap, 2:4, 0:1); g2 <- rr_window(pth$gap, 5:8, 0:1)
    lt <- rr_leads(pth$gap, RR_LEADS)
    wins[[p]] <- data.frame(pair = p,
                            years_2_4_minus_0_1 = g1[["est"]], se_2_4 = g1[["se"]],
                            years_5_8_minus_0_1 = g2[["est"]], se_5_8 = g2[["se"]],
                            leads_chi2 = lt[["chi2"]], leads_p = lt[["p"]],
                            stringsAsFactors = FALSE)
  }
  pt_all <- do.call(rbind, tabs); wt <- do.call(rbind, wins)
  if (!is.null(pt_all)) {
    first <- names(tabs)[1]
    show  <- tabs[[first]][, c("event_time", "gap", "gap_se")]
    names(show) <- c("event_time", paste0("prop_gap_", rr_pair_id(first)), "se")
    if (!is.null(pts)) show <- merge(show, pts, by.x = "event_time", by.y = "k", all.x = TRUE)
    cat("  proportional contrast by year (", first, "), with mothers' part-time share:\n",
        sep = "")
    rr_print(show)
    cat("\n  growth of the proportional contrast after the birth years:\n")
    rr_print(wt)
    cat("\n  Negative growth means the asymmetry widens after years 0 and 1.\n")
    rr$tabs$prop_path <- pt_all; rr$tabs$prop_path_growth <- wt
    rr$tabs$mothers_pt_share <- pts
    rr_write(pt_all, "R9_proportional_path.csv")
    rr_write(wt, "R9_proportional_growth.csv")
    rr_plot_gaps(pt_all, "R9_proportional_path.pdf",
                 "Paid time less worker margin, each over its base",
                 "The proportional contrast by year since the birth")
  }
}))

# =============================================================================
# R10. QUICK ROBUSTNESS CHECKS
# =============================================================================
# The headline margins and the proportional contrasts, all employed, both
# windows, under five variations: HILDA's longitudinal weights; Wave 20
# dropped, since the training questions moved in the questionnaire that year
# and it was the pandemic wave; Waves 12 to 19 only, the old questionnaire;
# never-mothers as the only comparison, so fathers' small rise cannot drive
# the contrast; and the interview-anchored clock where ev carries one. The
# experience screen needs the frame rebuilt, so this block states how.
if (!rr_run("R10")) rr_unselected("R10 quick checks") else
rr_block("R10: quick robustness checks", local({
  pairs <- RR_PAIRS[intersect(c("E1-O2", "E1-O4", "E1-O1"), names(RR_PAIRS))]
  base  <- rr_add_cp(rr_samples$all, pairs)
  ys    <- rr_nonempty(base, c("E1", "O2", "O4", paste0("CP_", rr_pair_id(names(pairs)))))
  run <- function(d, label, rhs = "post + post_f", coef = "post_f", fe = RR_FE, w = NULL)
    do.call(rbind, lapply(names(RR_WINDOWS), function(wn) {
      dw <- RR_WINDOWS[[wn]](d)
      r  <- do.call(rbind, lapply(ys, function(y) rr_fit(dw, y, rhs, coef, fe, w, label)))
      r$window <- wn; r
    }))
  out <- list(run(base, "baseline"))

  # The cross-sectional responding-person weight comes first, from ev or the
  # frame: it is positive for every respondent in every wave. A longitudinal
  # weight is positive only for the balanced panel it was built for, so it
  # re-weights a subset, and is used only when no cross-sectional weight exists.
  # The loader writes hhwtrps as analysis_weight and hhwtrp as
  # population_weight; lnwtrp is longitudinal_weight.
  xs_cand <- c("analysis_weight", "population_weight", "hhwtrps", "hhwtrp", "wtrp")
  lg_cand <- c("longitudinal_weight", "lnwtrp", "wlr", "fwtrp")
  fr <- NULL; bw <- base
  wc <- intersect(xs_cand, names(base))[1]
  if (is.na(wc)) {
    fr <- tryCatch(td_frame(need_cols = c("person_id", "wave"), context = "05 R10 weights"),
                   error = function(e) NULL)
    if (!is.null(fr)) {
      wc <- intersect(xs_cand, names(fr))[1]
      if (!is.na(wc))
        bw <- bw %>% left_join(fr[, c("person_id", "wave", wc)], by = c("person_id", "wave"))
    }
  }
  if (is.na(wc)) {
    wc <- intersect(lg_cand, names(base))[1]
    if (is.na(wc) && !is.null(fr)) {
      wc <- intersect(lg_cand, names(fr))[1]
      if (!is.na(wc))
        bw <- bw %>% left_join(fr[, c("person_id", "wave", wc)], by = c("person_id", "wave"))
    }
    if (!is.na(wc))
      cat("  no cross-sectional weight (analysis_weight, population_weight) on ev or\n",
          "  the frame; falling back\n",
          "  to ", wc, ".\n", sep = "")
  }
  if (!is.na(wc)) {
    bw$W <- suppressWarnings(as.numeric(bw[[wc]]))
    cat(sprintf("  weights: %s | %s of %s person-waves carry a positive weight\n", wc,
                format(sum(bw$W > 0, na.rm = TRUE), big.mark = ","),
                format(nrow(bw), big.mark = ",")))
    if (mean(bw$W > 0, na.rm = TRUE) < 0.5)
      cat("  *** fewer than half the person-waves carry a positive weight, so this\n",
          "      looks like a balanced-panel weight; the weighted rows describe that\n",
          "      subset only. analysis_weight (hhwtrps), the cross-sectional person\n",
          "      weight, avoids this.\n",
          sep = "")
    bw <- rr_add_cp(bw, pairs, w = "W")
    out[[length(out) + 1L]] <- run(bw, paste0("weighted (", wc, ")"), w = "W")
  } else cat("  no weight column found; the weighted check is skipped.\n")

  out[[length(out) + 1L]] <- run(base[base$wave != 20, , drop = FALSE], "Wave 20 dropped")
  out[[length(out) + 1L]] <- run(base[base$wave <= 19, , drop = FALSE], "Waves 12 to 19 only")
  out[[length(out) + 1L]] <- run(base[base$female == 1, , drop = FALSE],
                                 "never-mothers as the comparison", rhs = "post",
                                 coef = "post", fe = "person_id + wave")
  kc <- intersect(c("k_int", "k_interview", "k_iv", "event_time_int", "k_anchor"),
                  names(base))[1]
  if (!is.na(kc)) {
    bi <- base
    bi$k <- suppressWarnings(as.numeric(bi[[kc]]))
    bi$post <- as.numeric(bi$treated == 1 & !is.na(bi$k) & bi$k >= 0)
    bi$post_f <- bi$post * bi$female
    out[[length(out) + 1L]] <- run(bi, paste0("interview clock (", kc, ")"))
  } else cat("  no interview-anchored event time on ev (looked for k_int, k_interview,\n",
             "  k_iv, event_time_int, k_anchor); that check is skipped.\n", sep = "")

  tab <- do.call(rbind, out)
  wide <- stats::reshape(
    data.frame(check = tab$label, window = tab$window, outcome = tab$outcome,
               cell = ifelse(is.na(tab$est), "", sprintf("%+.4f (%.4f)", tab$est, tab$se)),
               stringsAsFactors = FALSE),
    idvar = c("check", "window"), timevar = "outcome", direction = "wide")
  names(wide) <- sub("^cell\\.", "", names(wide))
  rownames(wide) <- NULL
  cat("\n  estimate (SE); CP_ columns are proportional contrasts, bases as known:\n")
  rr_print(wide)
  cat("\n  Experience screen: ",
      if (isTRUE(get0("TD_SCREEN_ON", ifnotfound = FALSE)))
        paste0("ON, ", get0("TD_SAMPLE_RULE", ifnotfound = "?"),
               ". This is the sensitivity run.\n")
      else "off, the default since 14 October.\n",
      "  For the screened sensitivity run, set TD_SCREEN=on, delete\n",
      "  handoff/frame.rds and handoff/ev.rds, re-run 03a_design_audit.R, then\n",
      "  this file. The screen removes mothers' person-waves at about eight\n",
      "  times the controls' rate.\n", sep = "")
  rr$tabs$quick_checks <- tab
  rr_write(tab, "R10_quick_checks.csv")
}))

# =============================================================================
# R11. EXIT EXPECTATIONS: THE TENURE CHANNEL
# =============================================================================
# HILDA asks employees three per cent chances about the next twelve months:
# leaving the job voluntarily (C25, jbmplej), losing it (C26, jbmploj), and
# finding and accepting a job at least as good (C27, jbmpgj). The first is
# the worker's own expected attachment, the quantity the firm's condition
# turns on. All three are rescaled to probabilities here.
#
# (a) Does the stated chance move at the birth? If the employer's lower
#     expectation mirrors mothers' own, the chance of leaving should rise.
# (b) Does the paid-time fall depend on the mother's OWN pre-birth chance
#     of leaving? Mothers are split at the median of their average over
#     k = -3..-1, which is fixed before the birth. post_f is the fall for
#     mothers who expected to stay; post_f_hi is the EXTRA fall for those
#     who expected to leave. A fall of the same size in both groups points
#     to an employer acting on a view of mothers as a group rather than on
#     what this mother intends.
# (c) Mothers' post-birth person-waves kept only where she says her chance
#     of leaving is 10 per cent or less. This selects on an answer given
#     after the birth, so it is a description, not a causal split.
if (!rr_run("R11")) rr_unselected("R11 exit expectations") else
rr_block("R11: exit expectations, the tenure channel", local({
  cols <- c("chance_leave_job", "chance_lose_job", "chance_find_job")
  d <- rr_from_frame(rr_samples$all, cols, "05 R11 exit chances")
  if (rr_all_na(d, cols)) {
    cat("  The exit-chance columns are on neither ev nor the frame; block skipped.\n")
    rr$skip <- c(rr$skip, "R11 exit expectations: no chance_* columns")
    return(invisible(NULL))
  }
  cols <- intersect(cols, names(d))
  for (v in cols) d[[v]] <- suppressWarnings(as.numeric(d[[v]])) / 100
  pairs <- RR_PAIRS[intersect(c("E1-O2", "E1-O4"), names(RR_PAIRS))]
  d <- rr_add_cp(d, pairs)
  cps <- paste0("CP_", rr_pair_id(names(pairs)))

  cat("  mean stated chance among employed person-waves, before the birth:\n")
  pre <- d[d$treated == 1 & !is.na(d$k) & d$k >= -3 & d$k <= -1, , drop = FALSE]
  pre_means <- do.call(rbind, lapply(cols, function(v)
    data.frame(item = v,
               mothers_to_be = mean(pre[[v]][pre$female == 1], na.rm = TRUE),
               fathers_to_be = mean(pre[[v]][pre$female == 0], na.rm = TRUE),
               share_answered = mean(!is.na(d[[v]])))))
  rr_print(pre_means)
  rr_write(pre_means, "R11_prebirth_means.csv")

  # (a)
  a <- do.call(rbind, lapply(names(RR_WINDOWS), function(w) {
    dw <- RR_WINDOWS[[w]](d)
    rbind(do.call(rbind, lapply(cols, function(v)
            rr_fit(dw, v, label = paste0("(a) mothers minus fathers, ", w)))),
          do.call(rbind, lapply(cols, function(v)
            rr_fit(dw[dw$female == 1, , drop = FALSE], v, "post", "post",
                   "person_id + wave", label = paste0("(a) mothers minus never-mothers, ", w)))))
  }))

  # (b)
  b <- NULL; grp <- NULL
  if ("chance_leave_job" %in% cols) {
    pm <- pre[pre$female == 1 & !is.na(pre$chance_leave_job), , drop = FALSE]
    if (nrow(pm)) {
      pm  <- stats::aggregate(chance_leave_job ~ person_id, data = pm, FUN = mean)
      thr <- stats::median(pm$chance_leave_job)
      hi  <- pm$person_id[pm$chance_leave_job > thr]
      lo  <- pm$person_id[pm$chance_leave_job <= thr]
      grp <- data.frame(threshold = thr, mothers_expecting_to_stay = length(lo),
                        mothers_expecting_to_leave = length(hi))
      cat(sprintf(paste0("\n  (b) split at the median pre-birth chance of leaving, %.2f:\n",
                         "      %d mothers at or below, %d above\n"), thr, length(lo), length(hi)))
      db <- d[!(d$treated == 1 & d$female == 1 & !(d$person_id %in% c(hi, lo))), , drop = FALSE]
      db$post_f_hi <- db$post_f * as.numeric(db$person_id %in% hi)
      ys <- rr_nonempty(db, c("E1", "O2", "O4", cps))
      b <- do.call(rbind, lapply(names(RR_WINDOWS), function(w)
        do.call(rbind, lapply(ys, function(y)
          rr_fit(RR_WINDOWS[[w]](db), y, "post + post_f + post_f_hi",
                 c("post_f", "post_f_hi"), label = paste0("(b) by pre-birth chance, ", w))))))
      # Each group on its own, against its own pre-birth rate. A group that
      # trained more before the birth has more room to fall, so the levels
      # alone can mislead; the proportional fall uses the group's own base.
      gset <- list(`expect to stay` = lo, `expect to leave` = hi)
      bg <- do.call(rbind, lapply(names(gset), function(g) {
        other <- setdiff(c(hi, lo), gset[[g]])
        dg <- db[!(db$treated == 1 & db$female == 1 & db$person_id %in% other), , drop = FALSE]
        do.call(rbind, lapply(names(RR_WINDOWS), function(w)
          do.call(rbind, lapply(rr_nonempty(dg, c("E1", "O2", "O4")), function(y) {
            r <- rr_fit(RR_WINDOWS[[w]](dg), y, label = g)
            sel <- dg$treated == 1 & dg$female == 1 & !is.na(dg$k) & dg$k >= -3 & dg$k <= -1
            r$window <- w; r$group_base <- mean(dg[[y]][sel], na.rm = TRUE)
            r$prop <- r$est / r$group_base; r$prop_se <- r$se / r$group_base
            r
          }))))
      }))
      cat("\n  (b) each group against its own pre-birth rate:\n")
      rr_print(bg[, c("label", "window", "outcome", "group_base", "est", "se", "prop", "prop_se")])
      rr$tabs$exit_by_group <- bg
      rr_write(bg, "R11_exit_by_group.csv")
    }
  }

  # (c)
  cc <- NULL
  if ("chance_leave_job" %in% cols) {
    keep <- !(d$treated == 1 & d$female == 1 & d$post == 1 &
                !(!is.na(d$chance_leave_job) & d$chance_leave_job <= 0.10))
    dc <- d[keep, , drop = FALSE]
    ys <- rr_nonempty(dc, c("E1", "O2", "O4", cps))
    cc <- do.call(rbind, lapply(names(RR_WINDOWS), function(w)
      do.call(rbind, lapply(ys, function(y)
        rr_fit(RR_WINDOWS[[w]](dc), y,
               label = paste0("(c) mothers saying 10% or less after the birth, ", w))))))
  }
  tab <- rbind(a, b, cc)
  cat("\n"); rr_print(tab[, c("label", "outcome", "term", "est", "se", "mde", "n")])
  cat("\n  The chances are probabilities: +0.030 is three percentage points.\n")
  rr$tabs$exit <- tab; rr$tabs$exit_groups <- grp
  rr_write(tab, "R11_exit_expectations.csv")
}))

# =============================================================================
# R12. BIRTH-WAVE EXPOSURE: THE SHARE OF THE WINDOW AFTER THE BIRTH
# =============================================================================
# The training items cover the twelve months before the interview. For the
# birth wave the loader records how much of that window fell after the
# birth (post_birth_window_share: 0.125, 0.375, 0.625 or 0.875, from the
# birth quarter). Interview timing relative to the birth is close to
# arbitrary, so this is the cleanest test of time away at the start.
#
# Time away makes a two-sided prediction. At k = 0 the fall should GROW with
# the share, since more of the window was spent after the birth. At k = 1 it
# should SHRINK with the share: a birth just before the k = 0 interview puts
# most of the leave inside the k = 1 window. Falls that are flat across the
# share are not time away. Women only, mothers with a dated birth against
# never-mothers, person and wave effects.
if (!rr_run("R12")) rr_unselected("R12 birth-wave exposure") else
rr_block("R12: birth-wave exposure, the window share", local({
  d <- rr_from_frame(rr_samples$all, "post_birth_window_share", "05 R12 window share")
  if (rr_all_na(d, "post_birth_window_share")) {
    cat("  post_birth_window_share is on neither ev nor the frame; block skipped.\n")
    rr$skip <- c(rr$skip, "R12 birth-wave exposure: no post_birth_window_share")
    return(invisible(NULL))
  }
  pairs <- RR_PAIRS[intersect(c("E1-O2", "E1-O4"), names(RR_PAIRS))]
  d  <- rr_add_cp(d[d$female == 1, , drop = FALSE], pairs)
  sh <- d[d$treated == 1 & !is.na(d$k) & d$k == 0 & !is.na(d$post_birth_window_share),
          c("person_id", "post_birth_window_share")]
  sh <- sh[!duplicated(sh$person_id), , drop = FALSE]
  names(sh)[2] <- "share"
  d  <- merge(d, sh, by = "person_id", all.x = TRUE)
  d  <- d[d$treated == 0 | !is.na(d$share), , drop = FALSE]
  vals <- sort(unique(stats::na.omit(sh$share)))
  cat("  mothers with a dated birth, by share of the birth-wave window after it:\n")
  share_counts <- table(sh$share)
  print(share_counts)
  rr_write(data.frame(share = c(names(share_counts), "all"),
                      persons = c(as.vector(share_counts), sum(share_counts)),
                      stringsAsFactors = FALSE), "R12_mothers_by_share.csv")
  rhs <- character(0)
  for (i in seq_along(vals)) {
    d[[paste0("k0_s", i)]] <- as.numeric(d$treated == 1 & d$k == 0 & d$share == vals[i])
    d[[paste0("k1_s", i)]] <- as.numeric(d$treated == 1 & d$k == 1 & d$share == vals[i])
    rhs <- c(rhs, paste0("k0_s", i), paste0("k1_s", i))
  }
  d$k2_plus <- as.numeric(d$treated == 1 & d$k >= 2)
  d$share_c <- ifelse(is.na(d$share), 0, d$share - 0.5)
  d$k0 <- as.numeric(d$treated == 1 & d$k == 0); d$k1 <- as.numeric(d$treated == 1 & d$k == 1)
  d$k0_share <- d$k0 * d$share_c; d$k1_share <- d$k1 * d$share_c
  ys <- rr_nonempty(d, c("E1", "O2", "O4", paste0("CP_", rr_pair_id(names(pairs)))))
  bins <- do.call(rbind, lapply(ys, function(y)
    rr_fit(d, y, paste(c(rhs, "k2_plus"), collapse = " + "), c(rhs, "k2_plus"),
           "person_id + wave", label = "by share")))
  bins$share <- NA_real_
  for (i in seq_along(vals)) bins$share[bins$term %in% paste0(c("k0_s", "k1_s"), i)] <- vals[i]
  lin <- do.call(rbind, lapply(ys, function(y)
    rr_fit(d, y, "k0 + k0_share + k1 + k1_share + k2_plus",
           c("k0", "k0_share", "k1", "k1_share", "k2_plus"), "person_id + wave",
           label = "linear in the share (centred at 0.5)")))
  cat("\n  birth-wave (k0_s) and next-wave (k1_s) changes by share, and the later level:\n")
  rr_print(bins[, c("outcome", "term", "share", "est", "se", "n")])
  cat("\n  the same, linear in the share. Time away predicts k0_share < 0 and k1_share > 0:\n")
  rr_print(lin[, c("outcome", "term", "est", "se", "mde")])
  lin$share <- NA_real_
  rr$tabs$window_share <- rbind(bins[, names(lin)], lin)
  rr_write(rbind(bins[, names(lin)], lin), "R12_window_share.csv")
}))

# =============================================================================
# R13. PART TIME BY CHOICE OR BY CONSTRAINT
# =============================================================================
# R8 found that part-time work carries much of the proportional asymmetry.
# HILDA asks whether the respondent would prefer more, fewer or the same
# hours; the loader's underemployed flag marks those who want more. Women
# only, mothers against never-mothers, person and wave effects. post is the
# change after the birth in full-time waves; post_pt_vol and post_pt_und are
# the EXTRA change in mothers' part-time waves where she does not, or does,
# want more hours; pt and pt_und are the same part-time differences among
# women with no birth. If the asymmetry sits in part time she chose, the
# hours channel is her choice; if it sits in part time she would rather not
# work, it is a constraint.
if (!rr_run("R13")) rr_unselected("R13 part time by choice") else
rr_block("R13: part time by choice or constraint", local({
  if (is.na(rr_ft_col)) {
    cat("  No full-time column on ev; block skipped.\n")
    rr$skip <- c(rr$skip, "R13 part time by choice: no full-time column")
    return(invisible(NULL))
  }
  d <- rr_from_frame(rr_samples$all, "underemployed", "05 R13 hours preference")
  if (rr_all_na(d, "underemployed")) {
    cat("  underemployed is on neither ev nor the frame; block skipped.\n")
    rr$skip <- c(rr$skip, "R13 part time by choice: no underemployed")
    return(invisible(NULL))
  }
  pairs <- RR_PAIRS[intersect(c("E1-O2", "E1-O4"), names(RR_PAIRS))]
  d <- rr_add_cp(d[d$female == 1 & !is.na(d$ft), , drop = FALSE], pairs)
  d$pt  <- 1 - d$ft
  d$und <- as.numeric(!is.na(d$underemployed) & d$underemployed == 1)
  d$pt_und      <- d$pt * d$und
  d$post_pt_vol <- d$post * d$pt * (1 - d$und)
  d$post_pt_und <- d$post * d$pt * d$und
  sh <- data.frame(
    group = c("mothers' part-time waves after the birth", "never-mothers' part-time waves"),
    share_wanting_more_hours = c(
      mean(d$und[d$treated == 1 & d$post == 1 & d$pt == 1]),
      mean(d$und[d$treated == 0 & d$pt == 1])),
    n = c(sum(d$treated == 1 & d$post == 1 & d$pt == 1), sum(d$treated == 0 & d$pt == 1)))
  rr_print(sh)
  rr_write(sh, "R13_part_time_shares.csv")
  ys <- rr_nonempty(d, c("E1", "O2", "O4", paste0("CP_", rr_pair_id(names(pairs)))))
  terms <- c("post", "post_pt_vol", "post_pt_und", "pt", "pt_und")
  tab <- do.call(rbind, lapply(names(RR_WINDOWS), function(w)
    do.call(rbind, lapply(ys, function(y)
      rr_fit(RR_WINDOWS[[w]](d), y, paste(terms, collapse = " + "), terms,
             "person_id + wave", label = paste0("women, ", w))))))
  cat("\n"); rr_print(tab[, c("label", "outcome", "term", "est", "se", "mde", "n")])
  rr$tabs$part_time_choice <- tab; rr$tabs$part_time_choice_shares <- sh
  rr_write(tab, "R13_part_time_choice.csv")
}))

# =============================================================================
# R14. WHERE HOURS DID NOT CHANGE, AGAINST FATHERS
# =============================================================================
# R8 compares mothers with never-mothers, and that comparison trends before
# the birth (Section VI.A). This block repeats R8 against fathers, the
# identified comparison: person and sex-by-wave effects, so never-treated
# men and women carry each sex's calendar path, and every term is a mother
# minus a father.
#   (a) All parents. post_f is mothers minus fathers in full-time waves;
#       post_pt_f the extra difference in part-time waves; pt_f women's part-
#       time difference less men's, before any birth.
#   (b) Late switchers, mothers full time at k = 1 whose first part-time wave
#       comes at k = 2 or later, against all fathers. k2_plus_f is the gap
#       from k = 2 while still full time; after_switch the extra change at
#       the switch.
#   (c) Mothers full time at every observed wave from k = 1, against all
#       fathers, and again against fathers full time at every wave from
#       k = 1 (c'), so that both sides hold hours.
# All three select on hours after the birth, so they locate the fall rather
# than separate its causes.
if (!rr_run("R14")) rr_unselected("R14 hours fixed, against fathers") else
rr_block("R14: where hours did not change, against fathers", local({
  if (is.na(rr_ft_col)) {
    cat("  No full-time column on ev; block skipped.\n")
    rr$skip <- c(rr$skip, "R14 hours fixed, against fathers: no full-time column")
    return(invisible(NULL))
  }
  pairs <- RR_PAIRS[intersect(c("E1-O2", "E1-O4", "E1-O1"), names(RR_PAIRS))]
  d  <- rr_add_cp(rr_samples$all[!is.na(rr_samples$all$ft), , drop = FALSE], pairs)
  # v-levelcontrast, 19 October: the level contrasts too, so the difference has its own SE.
  ys <- rr_nonempty(d, c("E1", "O2", "O4", paste0("CL_", rr_pair_id(names(pairs))),
                        paste0("CP_", rr_pair_id(names(pairs)))))
  d$pt        <- 1 - d$ft
  d$pt_f      <- d$pt * d$female
  d$post_pt   <- d$post * d$pt
  d$post_pt_f <- d$post_pt * d$female
  a <- do.call(rbind, lapply(names(RR_WINDOWS), function(w)
    do.call(rbind, lapply(ys, function(y)
      rr_fit(RR_WINDOWS[[w]](d), y, "post + post_f + pt + pt_f + post_pt + post_pt_f",
             c("post_f", "post_pt_f", "pt_f"), label = paste0("(a) all parents, ", w))))))

  # Hours paths after the birth, as R8 builds them, for each sex.
  paths <- function(sex) {
    m  <- d[d$treated == 1 & d$female == sex & !is.na(d$k), , drop = FALSE]
    k1 <- m[m$k == 1, c("person_id", "ft")]; names(k1)[2] <- "ft1"
    k1 <- k1[!duplicated(k1$person_id), , drop = FALSE]
    sw <- m[m$k >= 1 & m$ft == 0, c("person_id", "k"), drop = FALSE]
    sw <- if (nrow(sw)) stats::aggregate(k ~ person_id, data = sw, FUN = min) else
      data.frame(person_id = m$person_id[0], k = numeric(0))
    names(sw)[2] <- "s"
    cl <- merge(merge(data.frame(person_id = unique(m$person_id)), k1, by = "person_id",
                      all.x = TRUE), sw, by = "person_id", all.x = TRUE)
    cl$late <- !is.na(cl$ft1) & cl$ft1 == 1 & !is.na(cl$s) & cl$s >= 2
    cl$ftk1 <- !is.na(cl$ft1) & cl$ft1 == 1 & is.na(cl$s)
    cl
  }
  mo <- paths(1); fa <- paths(0)
  cat(sprintf("  mothers: %d late switchers, %d full time from k = 1 | fathers: %d full time from k = 1\n",
              sum(mo$late), sum(mo$ftk1), sum(fa$ftk1)))
  rr_write(data.frame(group = c("mothers_late_switchers", "mothers_full_time_from_k1", "fathers_full_time_from_k1"),
                      persons = c(sum(mo$late), sum(mo$ftk1), sum(fa$ftk1)), stringsAsFactors = FALSE),
           "R14_groups.csv")
  terms_common <- function(z) {
    z$at_birth <- as.numeric(z$treated == 1 & z$k %in% 0)
    z$k1       <- as.numeric(z$treated == 1 & z$k %in% 1)
    z$k2_plus  <- as.numeric(z$treated == 1 & !is.na(z$k) & z$k >= 2)
    z$at_birth_f <- z$at_birth * z$female
    z$k1_f       <- z$k1 * z$female
    z$k2_plus_f  <- z$k2_plus * z$female
    z
  }
  b <- NULL
  late <- mo[mo$late, c("person_id", "s")]
  if (nrow(late) >= 30) {
    db <- d[d$treated == 0 | d$female == 0 | d$person_id %in% late$person_id, , drop = FALSE]
    db <- merge(db, late, by = "person_id", all.x = TRUE)
    db <- terms_common(db)
    db$after_switch <- as.numeric(db$female == 1 & db$treated == 1 & !is.na(db$s) & db$k >= db$s)
    b <- do.call(rbind, lapply(ys, function(y)
      rr_fit(db, y, "at_birth + k1 + k2_plus + at_birth_f + k1_f + k2_plus_f + after_switch",
             c("k1_f", "k2_plus_f", "after_switch"),
             label = "(b) late switchers, against all fathers")))
  }
  cc <- NULL
  ftm <- mo$person_id[mo$ftk1]; ftf <- fa$person_id[fa$ftk1]
  if (length(ftm) >= 30) {
    rhs <- "at_birth + k1 + k2_plus + at_birth_f + k1_f + k2_plus_f"
    dc  <- terms_common(d[d$treated == 0 | d$female == 0 | d$person_id %in% ftm, , drop = FALSE])
    dc2 <- terms_common(d[d$treated == 0 | d$person_id %in% c(ftm, ftf), , drop = FALSE])
    cc <- rbind(
      do.call(rbind, lapply(ys, function(y)
        rr_fit(dc, y, rhs, c("k1_f", "k2_plus_f"),
               label = "(c) mothers full time from k = 1, against all fathers"))),
      do.call(rbind, lapply(ys, function(y)
        rr_fit(dc2, y, rhs, c("k1_f", "k2_plus_f"),
               label = "(c') the same, against fathers full time from k = 1"))))
  }
  tab <- rbind(a, b, cc)
  rr_print(tab[, c("label", "outcome", "term", "est", "se", "mde", "n")])
  cat("\n  CP_ rows are proportional contrasts, paid time less the worker margin,\n",
      "  each over its own pre-birth base (bases as known). Compare with R8, which\n",
      "  uses never-mothers.\n", sep = "")
  rr$tabs$hours_fathers <- tab
  rr_write(tab, "R14_hours_fixed_fathers.csv")
}))

# =============================================================================
# R15. DAYS OF LEAVE IN THE LAST TWELVE MONTHS
# =============================================================================
# The calendar share in R3 appears to count parental leave as time in a job,
# so it cannot measure leave. HILDA asks employees, from Wave 5, how many
# days of leave they took in the last 12 months, the training items' own
# window: paid maternity, paternity, bereavement, family and carer's leave
# (alop, the loader's paid_nonannual_leave_days) and unpaid leave (alup;
# add it to the loader as unpaid_leave_days). Their sum measures time away
# on leave directly. Mothers minus fathers, on the paid-time margin, own time
# at any place and the proportional contrast:
#   (a) only person-waves with no such leave in the last 12 months;
#   (b) days of leave as a control, in hundreds of days. Leave is an outcome
#       of the birth, so this is the arithmetic of the channel.
# Without unpaid_leave_days the block runs on paid leave alone and says so.
if (!rr_run("R15")) rr_unselected("R15 days of leave") else
rr_block("R15: days of leave in the last twelve months", local({
  cols <- c("paid_nonannual_leave_days", "unpaid_leave_days")
  d0 <- rr_from_frame(rr_samples$all, cols, "05 R15 leave days")
  if (rr_all_na(d0, "paid_nonannual_leave_days")) {
    cat("  paid_nonannual_leave_days is on neither ev nor the frame; block skipped.\n")
    rr$skip <- c(rr$skip, "R15 days of leave: no paid_nonannual_leave_days")
    return(invisible(NULL))
  }
  has_unpaid <- !rr_all_na(d0, "unpaid_leave_days")
  if (!has_unpaid)
    cat("  unpaid_leave_days is missing, so leave means paid leave only. Add alup\n",
        "  to the loader as unpaid_leave_days to count unpaid parental leave too.\n", sep = "")
  lv <- d0[, c("person_id", "wave")]
  pl <- suppressWarnings(as.numeric(d0$paid_nonannual_leave_days))
  ul <- if (has_unpaid) suppressWarnings(as.numeric(d0$unpaid_leave_days)) else 0
  # A person-wave counts only if the paid item was answered; a missing unpaid
  # item alongside it is read as none, since the paid item shows the leave
  # questions were put.
  lv$LEAVE <- ifelse(is.na(pl), NA_real_, pl + ifelse(is.na(ul), 0, ul))
  cat(sprintf("  leave days answered on %.1f%% of employed person-waves\n",
              100 * mean(!is.na(lv$LEAVE))))
  mm <- rr_samples$all %>% left_join(lv, by = c("person_id", "wave"))
  mm <- mm[mm$treated == 1 & !is.na(mm$k) & mm$k >= -3 & mm$k <= 4 & !is.na(mm$LEAVE), ]
  cat("\n  mean days of leave in the last 12 months, by event time:\n")
  leave_means <- merge(stats::aggregate(LEAVE ~ k, data = mm[mm$female == 1, ], FUN = mean),
                       stats::aggregate(LEAVE ~ k, data = mm[mm$female == 0, ], FUN = mean),
                       by = "k", suffixes = c("_mothers", "_fathers"))
  rr_print(leave_means)
  rr_write(leave_means, "R15_leave_means.csv")
  pairs <- RR_PAIRS[intersect(c("E1-O2", "E1-O1"), names(RR_PAIRS))]
  ys <- c("E1", "O2", paste0("CP_", rr_pair_id(names(pairs))))
  out <- list()
  for (s in intersect(c("all", "ftret"), names(rr_samples))) {
    d <- rr_add_cp(rr_samples[[s]], pairs) %>% left_join(lv, by = c("person_id", "wave"))
    d$LEAVE100 <- d$LEAVE / 100
    for (w in names(RR_WINDOWS)) {
      dw <- RR_WINDOWS[[w]](d)
      dw <- dw[!is.na(dw$LEAVE), , drop = FALSE]
      for (y in rr_nonempty(dw, ys)) {
        r <- rbind(rr_fit(dw, y, label = "leave answered"),
                   rr_fit(dw[dw$LEAVE == 0, , drop = FALSE], y, label = "(a) no leave in the year"),
                   rr_fit(dw, y, "post + post_f + LEAVE100", "post_f",
                          label = "(b) days of leave as a control"))
        r$sample <- s; r$window <- w
        out[[length(out) + 1L]] <- r
      }
    }
  }
  tab <- do.call(rbind, out); tab$unpaid_included <- has_unpaid
  cat("\n"); rr_print(tab[, c("sample", "window", "label", "outcome", "est", "se", "mde", "n")])
  rr$tabs$leave_days <- tab
  rr_write(tab, "R15_leave_days.csv")
}))

# =============================================================================
# R16. APPENDIX TABLES FOR THE ANALYSES ADDED AFTER UNBLINDING
# =============================================================================
# Writes one LaTeX table per result that the revised text quotes, plus a
# section file that \inputs them, into the build folder output/tables/.
# It reads every result this and earlier runs left in the handoff, so it can
# run on its own (RR_BLOCKS = "R16") after the blocks it draws on. Tables whose
# results are missing are skipped and listed. The manuscript inputs
# tables/PH_appendix_section at the end of the online appendix, and
# paper_tables.R copies these files there with the rest of the build.
#
# v-sync. THE BUILD FOLDER, NOT DIAGNOSTICS. The tables went to
# diagnostics/review_response/tables/ and were copied to the page by hand, so
# paper_tables.R's gate listed all ten as fragments with no generator, and a
# rerun of R16 could leave the page holding the previous copies. Written
# beside the other fragments, they travel with the sync and the gate compares
# them like any other table.
#
# v-posthoc. POST HOC, NOT "ADDED IN REVISION". The review was informal, so the
# notes and the section title use the paper's own term for analyses added after
# the designations were fixed. "Added in revision" read as a formal round.
# Table I7 is titled by whether more hours are wanted, as Section 8.3 describes
# it, not by choice or constraint, which the hours answer cannot establish.
#
# v-pos. THE CLASS'S PLACEMENT KEY. cas-sc takes float placement through its pos
# key, as the manuscript's figures do. Under a bare [htbp] every table in the
# paper sat at the top of a page, so under the appendix's section barrier the
# Appendix I heading and introduction stood alone on their page. [pos=!htbp]
# lets the first tables sit under the introduction.
if (!rr_run("R16")) rr_unselected("R16 appendix tables") else
rr_block("R16: appendix tables for the post-hoc analyses", local({
  prev <- if (exists("td_has", mode = "function") && isTRUE(td_has("review_response")))
    tryCatch(td_get("review_response"), error = function(e) list()) else list()
  if (!is.list(prev)) prev <- list()
  tabs <- prev; tabs[names(rr$tabs)] <- rr$tabs
  # The resolution paper_tables.R uses for its build folder, TEXDIR.
  tdir <- if (exists("TEXDIR") && is.character(TEXDIR) && nzchar(TEXDIR)) TEXDIR else
    if (exists("DIR_TABLES") && is.character(DIR_TABLES) && nzchar(DIR_TABLES)) DIR_TABLES else
      file.path(if (exists("PROJ_ROOT")) PROJ_ROOT else getwd(), "output", "tables")
  dir.create(tdir, showWarnings = FALSE, recursive = TRUE)

  num  <- function(x, d = 3) ifelse(is.na(x), "",
                                    gsub("-", "$-$", sprintf(paste0("%.", d, "f"), x), fixed = TRUE))
  cell <- function(e, s, d = 3) ifelse(is.na(e), "", paste0(num(e, d), " (", num(s, d), ")"))
  pick <- function(df, ...) {
    cond <- list(...); keep <- rep(TRUE, nrow(df))
    for (nm in names(cond)) keep <- keep & !is.na(df[[nm]]) & df[[nm]] == cond[[nm]]
    df[keep, , drop = FALSE][1, , drop = FALSE]
  }
  get_cell <- function(df, ...) { r <- pick(df, ...); if (!nrow(r) || is.na(r$est)) "" else cell(r$est, r$se) }
  SAMPLE <- c(all = "All employed", ftret = "Full-time returners",
              preft = "Full time before the birth", ftnow = "Full-time person-waves only")
  YEARS  <- c("k>=0" = "All years", "k>=2" = "From year 2")
  written <- character(0); missing <- character(0)

  # v-pagefill-appI, 20 October: every Appendix I table is set in \footnotesize by
  # default (Tables I.2, I.4, I.6 and I.9 were \small), and every one opens with
  # [pos=!htbp]. With the tighter float spacing the manuscript sets before
  # Appendix I, Tables I.1 and I.2 then fit on the page where the appendix
  # starts. The "h" lets them sit under its introduction; the "!" lets them
  # go to the top of that page rather than the next if they cannot.
  write_table <- function(file, caption, label, header, rows, notes, align, size = "\\footnotesize") {
    body <- vapply(seq_len(nrow(rows)), function(i) {
      r <- unlist(rows[i, ], use.names = FALSE)
      if (startsWith(r[1], "\\multicolumn") || startsWith(r[1], "\\addlinespace")) r[1] else
        paste0(paste(r, collapse = " & "), " \\\\")
    }, character(1))
    lines <- c("\\begin{table}[!htbp]" , "\\centering", size, "\\begin{threeparttable}",
               paste0("\\caption{", caption, "}"), paste0("\\label{", label, "}"),
               paste0("\\begin{tabular}{", align, "}"), "\\toprule", header, "\\midrule",
               body, "\\bottomrule", "\\end{tabular}", "\\begin{tablenotes}[flushleft]",
               "\\footnotesize", paste0("\\item \\textit{Notes}: ", notes),
               "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
    writeLines(lines, file.path(tdir, file))
    written <<- c(written, file)
  }
  panel <- function(text, ncol) c(paste0("\\multicolumn{", ncol, "}{l}{\\textit{", text, "}} \\\\"),
                                  rep("", ncol - 1))
  space <- function(ncol) c("\\addlinespace", rep("", ncol - 1))

  # ---- A. The financing contrast (R2) ------------------------------------
  ct <- tabs[["contrast"]]
  if (!is.null(ct) && nrow(ct)) {
    ct$se_use <- ifelse(is.finite(ct$boot_se), ct$boot_se, ct$se)
    rows <- list()
    for (s in names(SAMPLE)) for (w in names(YEARS)) {
      cellp <- function(pair, sc) { r <- ct[ct$sample == s & ct$window == w & ct$pair == pair &
                                               ct$scale == sc, , drop = FALSE]
                                    if (!nrow(r)) "" else cell(r$est[1], r$se_use[1]) }
      v <- c(SAMPLE[[s]], YEARS[[w]], cellp("E1-O1", "level"), cellp("E1-O1", "proportional"),
             cellp("E1-O2", "level"), cellp("E1-O2", "proportional"))
      if (any(nzchar(v[3:6]))) rows[[length(rows) + 1L]] <- v
    }
    rows <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    bB <- suppressWarnings(max(ct$boot_B, na.rm = TRUE))
    nb  <- unique(ct[!is.finite(ct$boot_se), c("sample", "window")])
    nob <- if (nrow(nb)) vapply(split(nb$sample, nb$window), function(s)
      tolower(paste(SAMPLE[s], collapse = ", ")), character(1)) else character(0)
    nob <- if (length(nob)) paste0(tolower(YEARS[names(nob)]), " rows of ", nob) else character(0)
    write_table("PH_R2_contrast.tex",
      "Employer-Financed Training Less Own-Time Training, in Levels and in Proportions",
      "tab:ph_contrast",
      c(" & & \\multicolumn{2}{c}{Own time at the workplace} & \\multicolumn{2}{c}{Own time at any place} \\\\",
        "\\cmidrule(lr){3-4} \\cmidrule(lr){5-6}",
        "Sample & Years & Level & Proportional & Level & Proportional \\\\"),
      rows,
      paste0("Stacked triple differences, mothers minus fathers, with person-by-source and ",
             "wave-by-sex-by-source effects. Level: employer-financed training less the worker's ",
             "margin, in probability units. Proportional: each margin divided by its own pre-birth ",
             "base among mothers before the difference is taken. ",
             if (is.finite(bB)) paste0("Standard errors come from a person bootstrap of ", bB,
                                       " replicates, which carries the uncertainty in the bases",
                                       if (length(nob)) paste0(", except in the ",
                                         paste(nob, collapse = " and the "),
                                         ", where the bases are treated as known") else "", ". ")
             else "Standard errors treat the bases as known. ",
             "Full-time person-waves only keeps, for every person, only the waves worked full ",
             "time, before and after the birth, so its comparison group and pre-birth bases differ ",
             "from those of Table~\\ref{tab:ph_hours_fixed}, which uses every employed wave. ",
             "From year 2 drops the birth wave and the next, whose reference periods overlap most ",
             "leave. Table~\\ref{tab:ph_hours_fixed} fits each margin in its own regression and ",
             "treats the bases as known, so its proportional standard errors are smaller by ",
             "construction and not comparable with these. Post hoc."),
      "llcccc", size = "\\footnotesize")
  } else missing <- c(missing, "R2 financing contrast")

  # ---- A2. Quick checks (R10) ---------------------------------------------
  # v-se. ONE CONTRAST, TWO STANDARD ERRORS, SAID WHERE BOTH APPEAR. Tables
  # I2 and I5 print the headline proportional contrast with its bases treated
  # as known; Table I1 bootstraps them. The text explains the difference, and
  # a review asked that the tables say it too.
  se_of <- function(df, ...) {
    r <- pick(df, ...); if (!nrow(r) || is.na(r$se[1])) NA_real_ else r$se[1]
  }
  .hb <- tabs[["contrast"]]
  .se_boot_head <- if (!is.null(.hb) && nrow(.hb)) {
    r <- .hb[.hb$sample == "all" & .hb$window == "k>=0" & .hb$pair == "E1-O2" &
               .hb$scale == "proportional", , drop = FALSE]
    if (nrow(r) && is.finite(r$boot_se[1])) r$boot_se[1] else NA_real_
  } else NA_real_
  se_note <- function(se_known) {
    if (!is.finite(.se_boot_head) || !is.finite(se_known)) return("")
    paste0("The baseline proportional contrast is the headline estimate of Table~",
           "\\ref{tab:ph_contrast}, where a person bootstrap that carries the uncertainty ",
           "in the bases gives a standard error of ", num(.se_boot_head), " against ",
           num(se_known), " here. ")
  }

  qc <- tabs[["quick_checks"]]
  if (!is.null(qc) && nrow(qc)) {
    labs <- unique(qc$label)
    # A weighted row counts only if it used a cross-sectional weight; the
    # longitudinal weight covers a balanced-panel subset.
    labs <- labs[!grepl("^weighted", labs) |
                   grepl("analysis_weight|population_weight|hhwtrp", labs)]
    pretty <- function(l) {
      if (l == "baseline") return("Baseline")
      if (l == "never-mothers as the comparison") return("Never-mothers as the only comparison")
      if (grepl("^weighted", l)) return("Cross-sectional weights")
      if (grepl("^interview clock", l)) return("Interview-anchored clock")
      l
    }
    rows <- lapply(labs, function(l) c(pretty(l),
      get_cell(qc, label = l, window = "k>=0", outcome = "E1"),
      get_cell(qc, label = l, window = "k>=2", outcome = "E1"),
      get_cell(qc, label = l, window = "k>=0", outcome = "CP_E1_O2"),
      get_cell(qc, label = l, window = "k>=2", outcome = "CP_E1_O2")))
    rows <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    # v-wt, 19 October: the note names the weight the weighted row used, read
    # from its label, "weighted (<column>)". R10 prefers analysis_weight (hhwtrps),
    # then population_weight (hhwtrp); both are HILDA's cross-sectional
    # responding-person weight. A longitudinal-weight row is dropped above.
    .wl <- grep("^weighted", labs, value = TRUE)[1]
    .wc <- if (is.na(.wl)) NA_character_ else sub("^weighted \\((.*)\\)$", "\\1", .wl)
    .wv <- unname(c(analysis_weight = "hhwtrps", hhwtrps = "hhwtrps",
                    population_weight = "hhwtrp", hhwtrp = "hhwtrp")[.wc])
    .wt_note <- if (is.na(.wc)) "" else paste0(
      "The cross-sectional weights row weights each person-wave by HILDA's cross-sectional ",
      "responding-person weight for its wave (\\texttt{", if (is.na(.wv)) .wc else .wv, "}). ")
    write_table("PH_R10_checks.tex",
      "Robustness of the Fall and of the Proportional Contrast", "tab:ph_checks",
      c(" & \\multicolumn{2}{c}{Employer-financed} & \\multicolumn{2}{c}{Proportional contrast} \\\\",
        "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
        " & All years & From year 2 & All years & From year 2 \\\\"),
      rows,
      paste0("All employed person-waves. Mothers minus fathers with person and wave-by-sex effects, ",
             "except the never-mothers row, which compares mothers with never-mothers with person and ",
             "wave effects. Wave~20 is the wave in which the training questions moved in the ",
             "questionnaire; Waves 12 to 19 precede the move. ",
             .wt_note,
             "Proportional contrast against own time ",
             "at any place, bases treated as known. ",
             se_note(se_of(qc, label = "baseline", window = "k>=0", outcome = "CP_E1_O2")),
             "Standard errors clustered by person. Post hoc."),
      "lcccc")
  } else missing <- c(missing, "R10 quick checks")

  # ---- B. Where hours did not change (R14, R8) ---------------------------
  hf <- tabs[["hours_fathers"]]; tc <- tabs[["two_clocks"]]
  if (!is.null(hf) && nrow(hf)) {
    outs <- c("E1", "O2", "CL_E1_O2", "CP_E1_O2", "CP_E1_O1")
    rowdef <- list(
      list("Full-time waves", hf, "(a) all parents, k>=2", "post_f"),
      list("Late switchers, before the switch", hf, "(b) late switchers, against all fathers", "k2_plus_f"),
      list("Late switchers, extra at the switch", hf, "(b) late switchers, against all fathers", "after_switch"),
      list("Full time from year 1", hf, "(c) mothers full time from k = 1, against all fathers", "k2_plus_f"),
      list("Full time from year 1, fathers also full time", hf,
           "(c') the same, against fathers full time from k = 1", "k2_plus_f"))
    mk <- function(def) c(def[[1]], vapply(outs, function(o)
      get_cell(def[[2]], label = def[[3]], term = def[[4]], outcome = o), character(1)))
    rowdef[[length(rowdef) + 1L]] <- list("Part time, women less men", hf, "(a) all parents, k>=2", "pt_f")
    rows <- list(panel("Panel A: mothers against fathers", 6))
    for (d in rowdef) rows[[length(rows) + 1L]] <- mk(d)
    if (!is.null(tc) && nrow(tc)) {
      rows[[length(rows) + 1L]] <- space(6)
      rows[[length(rows) + 1L]] <- panel("Panel B: mothers against never-mothers", 6)
      for (d in list(list("Full-time waves", tc, "(a) all mothers, k>=2", "post"),
                     list("Late switchers, before the switch", tc, "(b) late switchers", "k2_plus"),
                     list("Late switchers, extra at the switch", tc, "(b) late switchers", "after_switch"),
                     list("Full time from year 1", tc, "(c) full time from k = 1", "k2_plus"),
                     list("Part time, women before any birth", tc, "(a) all mothers, k>=2", "pt")))
        rows[[length(rows) + 1L]] <- mk(d)
    }
    rows <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    grp <- tabs[["two_clocks_groups"]]
    ng  <- function(g) if (is.null(grp)) NA else suppressWarnings(as.integer(grp$Freq[grp$Var1 == g][1]))
    write_table("PH_R14_hours_fixed.tex",
      "Where Hours Did Not Change: Changes from the Second Year after the Birth",
      "tab:ph_hours_fixed",
      c(" & Employer- & Own time & Level & \\multicolumn{2}{c}{Proportional contrast} \\\\",
        "\\cmidrule(lr){5-6}",
        " & financed & at any place & contrast & Any place & Workplace \\\\"),
      rows,
      paste0("Employed person-waves. Panel A compares mothers with fathers, with person and ",
             "wave-by-sex effects; Panel B compares mothers with never-mothers, with person and ",
             "wave effects. Every row uses all employed person-waves, full and part time, of ",
             "mothers, fathers and the never-treated, before and after the birth. Full-time waves: ",
             "the change after the birth in waves in which the mother works full time, with ",
             "part-time waves allowed their own level and their own change. Late switchers",
             if (is.finite(ng("late switcher"))) paste0(" (", ng("late switcher"), " mothers)") else "",
             ": full time in the first year after the birth, part time from a later wave. ",
             "Full time from year 1",
             if (is.finite(ng("full time from k = 1"))) paste0(" (", ng("full time from k = 1"), " mothers)") else "",
             ": full time at every observed wave from the first year. Part time: the difference ",
             "between part-time and full-time waves among women before any birth (Panel B) and its ",
             "difference between women and men (Panel A). All rows select on hours ",
             "after the birth. Level contrast: employer-financed training less own time at any ",
             "place, in probability units, fitted as one outcome so that its standard error is ",
             "that of the difference. Proportional contrast: employer-financed training less the worker's ",
             "margin, each over its own pre-birth base, bases treated as known. Standard errors ",
             "clustered by person. Unlike Table~\\ref{tab:ph_contrast}, which stacks the two ",
             "margins in one triple difference and bootstraps the bases, each margin here has its ",
             "own regression, so the proportional standard errors leave out the uncertainty in the ",
             "bases. The extra change at the switch moves only within late-switcher mothers and is ",
             "measured against women's calendar path, which the two panels share, so it is the ",
             "same in both. Post hoc."),
      "lccccc", size = "\\footnotesize\\setlength{\\tabcolsep}{3pt}")  # v-r14width, 19 October: six columns fit the text width at 3 pt
  } else missing <- c(missing, "R14 hours fixed")

  # ---- C. Hours accounting (R7) -------------------------------------------
  hr <- tabs[["hours"]]
  if (!is.null(hr) && !is.null(hr$single)) {
    g <- hr$gamma
    rows <- list(panel("Panel A: training by weekly hours among people with no birth, against 35 to 44", 5))
    if (!is.null(g)) for (o in intersect(c("E1", "O2"), g$outcome)) for (sx in c("women", "men")) {
      r <- g[g$outcome == o & g$sex == sx, , drop = FALSE]
      if (nrow(r)) rows[[length(rows) + 1L]] <- c(
        paste0(if (o == "E1") "Employer-financed" else "Own time at any place", ", ", sx),
        num(r[["1-15"]]), num(r[["16-24"]]), num(r[["25-34"]]), num(r[["45+"]]))
    }
    rows[[length(rows) + 1L]] <- space(5)
    rows[[length(rows) + 1L]] <- panel("Panel B: the fall before and after removing what hours predict", 5)
    rows[[length(rows) + 1L]] <- c("Version, years", "Original", "Net of hours", "Share explained",
                                   "Proportional contrast, net")
    s1 <- hr$single; c1 <- hr$contrast
    for (v in c("transport", "g-estimation")) for (w in names(YEARS)) {
      r  <- s1[s1$version == v & s1$window == w & s1$outcome == "E1", , drop = FALSE]
      rc <- c1[c1$version == v & c1$window == w & c1$pair == "E1-O2", , drop = FALSE]
      if (!nrow(r)) next
      rows[[length(rows) + 1L]] <- c(
        paste0(if (v == "transport") "Transport" else "Sequential g-estimation", ", ", tolower(YEARS[[w]])),
        num(r$orig[1]),
        if (is.finite(r$net_boot_se[1])) cell(r$net_of_hours[1], r$net_boot_se[1]) else num(r$net_of_hours[1]),
        paste0(gsub("-", "$-$", sprintf("%.0f", 100 * r$share_explained[1]), fixed = TRUE), "\\%"),
        if (nrow(rc) && is.finite(rc$net_boot_se[1])) cell(rc$net_of_hours_prop[1], rc$net_boot_se[1])
        else if (nrow(rc)) num(rc$net_of_hours_prop[1]) else "")
    }
    rows <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    hh <- hr$hours_change
    write_table("PH_R7_hours_accounting.tex",
      "How Much of the Fall Hours Account For", "tab:ph_hours_accounting",
      " & 1--15 & 16--24 & 25--34 & 45 or more \\\\",
      rows,
      paste0("Panel A: within-person differences in the probability of training by band of usual ",
             "weekly hours, estimated on never-treated employed person-waves with person and wave ",
             "effects, against 35 to 44 hours. Panel B removes from each margin the change these ",
             "gradients predict from mothers' and fathers' own hours and re-estimates the design. ",
             "Transport uses the gradients of people with no birth; sequential g-estimation ",
             "\\citep{AcharyaBlackwellSen2016} estimates them on everyone with the birth indicator ",
             "in the first stage. ",
             if (!is.null(hh) && nrow(hh)) paste0("Mothers' usual weekly hours change by ",
               num(hh$est[hh$label == "k>=0"][1], 1), " relative to fathers'. ") else "",
             "Standard errors from a person bootstrap that re-estimates the gradients in each ",
             "replicate (transport rows). Post hoc."),
      "lcccc")
  } else missing <- c(missing, "R7 hours accounting")

  # ---- D. Exposure: calendar share and days of leave (R3, R15) ------------
  ex <- tabs[["exposure"]]; lv <- tabs[["leave_days"]]
  if ((!is.null(ex) && nrow(ex)) || (!is.null(lv) && nrow(lv))) {
    cols <- list(c("E1", "k>=0"), c("E1", "k>=2"), c("O2", "k>=0"), c("CP_E1_O2", "k>=0"), c("CP_E1_O2", "k>=2"))
    mkrow <- function(df, lab, name) c(name, vapply(cols, function(cc)
      get_cell(df, sample = "all", label = lab, outcome = cc[1], window = cc[2]), character(1)))
    rows <- list()
    if (!is.null(ex) && nrow(ex)) {
      rows[[length(rows) + 1L]] <- panel("Panel A: the calendar's share of the financial year in a job", 6)
      rows[[length(rows) + 1L]] <- mkrow(ex, "exposure observed", "Share observed")
      rows[[length(rows) + 1L]] <- mkrow(ex, "(a) employed the whole period", "Employed the whole year")
      rows[[length(rows) + 1L]] <- mkrow(ex, "(b) exposure as a control", "Share as a control")
    }
    if (!is.null(lv) && nrow(lv)) {
      if (length(rows)) rows[[length(rows) + 1L]] <- space(6)
      rows[[length(rows) + 1L]] <- panel(paste0("Panel B: days of paid parental",
        if (isTRUE(any(lv$unpaid_included))) " and unpaid" else "", " leave in the last twelve months"), 6)
      rows[[length(rows) + 1L]] <- mkrow(lv, "leave answered", "Leave answered")
      rows[[length(rows) + 1L]] <- mkrow(lv, "(a) no leave in the year", "No leave in the year")
      rows[[length(rows) + 1L]] <- mkrow(lv, "(b) days of leave as a control", "Days of leave as a control")
    }
    rows <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    write_table("PH_R3_R15_exposure.tex",
      "Exposure in the Reference Year: Calendar Share and Days of Leave", "tab:ph_exposure",
      c(" & \\multicolumn{2}{c}{Employer-financed} & Own time & \\multicolumn{2}{c}{Proportional contrast} \\\\",
        "\\cmidrule(lr){2-3} \\cmidrule(lr){5-6}",
        " & All years & From year 2 & All years & All years & From year 2 \\\\"),
      rows,
      paste0("All employed person-waves, mothers minus fathers. Panel A uses the calendar's percent ",
             "of the last financial year spent in a job (capj), which counts leave as time in a job. ",
             "Panel B uses days of paid parental, family and bereavement leave and of unpaid leave in ",
             "the last twelve months, the training items' window. The restricted rows select on an ",
             "outcome of the birth, and the control rows show the arithmetic of the channel rather ",
             "than an identified effect. Proportional contrast against own time at any place, bases ",
             "treated as known. ",
             se_note(if (!is.null(ex) && nrow(ex)) se_of(ex, sample = "all", label = "exposure observed",
                                                         outcome = "CP_E1_O2", window = "k>=0")
                     else NA_real_),
             "Standard errors clustered by person. Post hoc."),
      "lccccc", size = "\\footnotesize")
  } else missing <- c(missing, "R3/R15 exposure")

  # ---- E. Exit expectations (R11) ----------------------------------------
  xt <- tabs[["exit"]]; xg <- tabs[["exit_by_group"]]
  if (!is.null(xt) && nrow(xt)) {
    items <- c(chance_leave_job = "Chance of leaving the job voluntarily",
               chance_lose_job = "Chance of losing the job",
               chance_find_job = "Chance of an equally good job")
    rows <- list(panel("Panel A: change in the stated chance after the birth", 5))
    for (it in names(items)) rows[[length(rows) + 1L]] <- c(items[[it]],
      get_cell(xt, label = "(a) mothers minus fathers, k>=0", outcome = it),
      get_cell(xt, label = "(a) mothers minus fathers, k>=2", outcome = it),
      get_cell(xt, label = "(a) mothers minus never-mothers, k>=0", outcome = it),
      get_cell(xt, label = "(a) mothers minus never-mothers, k>=2", outcome = it))
    rows[[length(rows) + 1L]] <- space(5)
    rows[[length(rows) + 1L]] <- panel("Panel B: employer-financed training, mothers minus fathers", 5)
    rows[[length(rows) + 1L]] <- c("", "Pre-birth rate", "All years", "From year 2", "Proportional")
    if (!is.null(xg) && nrow(xg)) for (gname in c("expect to stay", "expect to leave")) {
      r0 <- xg[xg$label == gname & xg$outcome == "E1" & xg$window == "k>=0", , drop = FALSE]
      r2 <- xg[xg$label == gname & xg$outcome == "E1" & xg$window == "k>=2", , drop = FALSE]
      if (nrow(r0) && nrow(r2)) rows[[length(rows) + 1L]] <- c(
        paste0("Expected to ", sub("expect to ", "", gname)),
        num(r2$group_base[1]), cell(r0$est[1], r0$se[1]), cell(r2$est[1], r2$se[1]),
        cell(r2$prop[1], r2$prop_se[1]))
    }
    rows[[length(rows) + 1L]] <- c("Saying 10\\% or less after the birth", "",
      get_cell(xt, label = "(c) mothers saying 10% or less after the birth, k>=0", outcome = "E1"),
      get_cell(xt, label = "(c) mothers saying 10% or less after the birth, k>=2", outcome = "E1"), "")
    rows <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    thr <- tabs[["exit_groups"]]
    write_table("PH_R11_exit.tex",
      "What Mothers Expect: Stated Chances of Leaving, Losing and Replacing the Job", "tab:ph_exit",
      c(" & \\multicolumn{2}{c}{Against fathers} & \\multicolumn{2}{c}{Against never-mothers} \\\\",
        "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
        " & All years & From year 2 & All years & From year 2 \\\\"),
      rows,
      paste0("Employed person-waves. HILDA asks employees the percent chance, over the next twelve ",
             "months, of leaving the job voluntarily (C25), losing it (C26) and finding and accepting ",
             "a job at least as good (C27); rescaled to probabilities. Panel B splits mothers at the ",
             "median of their average pre-birth chance of leaving",
             if (!is.null(thr) && nrow(thr)) paste0(" (", num(thr$threshold[1], 2), "; ",
               thr$mothers_expecting_to_stay[1], " and ", thr$mothers_expecting_to_leave[1], " mothers)") else "",
             "; each group is compared with all fathers, and the proportional fall uses the group's own ",
             "pre-birth rate; the proportional column is from year 2. The last row keeps mothers' post-birth waves only where they put their ",
             "chance of leaving at 10 percent or less, which selects on an answer given after the ",
             "birth. Stated chances describe mothers who remained employed. Standard errors clustered ",
             "by person. Post hoc."),
      "lcccc", size = "\\footnotesize")
  } else missing <- c(missing, "R11 exit expectations")

  # v-dash, 19 October: the note prints the windows with an en dash (5--8, 2--4).
  # The early and late columns keep "..": the registry keys are built from them.
  # ---- E2. No recovery (R1) -----------------------------------------------
  rc <- tabs[["recovery"]]
  if (!is.null(rc) && nrow(rc)) {
    rows <- lapply(seq_len(nrow(rc)), function(i) c(
      SAMPLE[[rc$sample[i]]],
      if (rc$outcome[i] == "E1") "At the workplace" else "At any place",
      cell(rc$recovery[i], rc$recovery_se[i]), num(rc$recovery_mde[i]),
      num(rc$leads_p[i], 2)))
    rows <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    write_table("PH_R1_recovery.tex",
      "Does the Fall Recover? Later Years against Earlier Years", "tab:ph_recovery",
      "Sample & Employer-financed training & Recovery & MDE & Pre-trend $p$ \\\\",
      rows,
      paste0("Sun--Abraham paths of employer-financed training, women minus men, estimated ",
             "separately by sex on employed person-waves. Recovery: the mean gap over years ",
             gsub("..", "--", unique(rc$late)[1], fixed = TRUE), " after the birth less the mean over years ",
             gsub("..", "--", unique(rc$early)[1], fixed = TRUE),
             ", from the full covariance of the gap path; a positive value means the gap closes. ",
             "MDE at 80 percent power and 5 percent size. Pre-trend: joint test that the gaps at years ",
             "$-8$ to $-2$ are zero. Post hoc."),
      "llccc")
  } else missing <- c(missing, "R1 recovery")

  # ---- F. The window share (R12) ------------------------------------------
  ws <- tabs[["window_share"]]
  if (!is.null(ws) && nrow(ws)) {
    bins <- ws[ws$label == "by share", , drop = FALSE]
    lin  <- ws[ws$label != "by share", , drop = FALSE]
    # Rows are indexed by the k0_s<i> terms. Tables saved before the share
    # column was kept fall back to the loader's four quarter midpoints.
    idx <- sort(unique(suppressWarnings(as.integer(sub("^k0_s", "",
             grep("^k0_s[0-9]+$", bins$term, value = TRUE))))))
    std <- c(0.125, 0.375, 0.625, 0.875)
    shv <- vapply(idx, function(i) {
      v <- if ("share" %in% names(bins)) suppressWarnings(as.numeric(
             bins$share[bins$term == paste0("k0_s", i)][1])) else NA_real_
      if (is.na(v)) std[i] else v }, numeric(1))
    rows <- list()
    for (j in seq_along(idx)) { i <- idx[j]; rows[[length(rows) + 1L]] <- c(num(shv[j]),
      get_cell(bins, term = paste0("k0_s", i), outcome = "E1"),
      get_cell(bins, term = paste0("k1_s", i), outcome = "E1"),
      get_cell(bins, term = paste0("k0_s", i), outcome = "O2"),
      get_cell(bins, term = paste0("k1_s", i), outcome = "O2")) }
    rows[[length(rows) + 1L]] <- space(5)
    rows[[length(rows) + 1L]] <- c("Slope in the share",
      get_cell(lin, term = "k0_share", outcome = "E1"), get_cell(lin, term = "k1_share", outcome = "E1"),
      get_cell(lin, term = "k0_share", outcome = "O2"), get_cell(lin, term = "k1_share", outcome = "O2"))
    rows[[length(rows) + 1L]] <- c("From year 2", get_cell(lin, term = "k2_plus", outcome = "E1"), "",
                                   get_cell(lin, term = "k2_plus", outcome = "O2"), "")
    rows <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    write_table("PH_R12_window_share.tex",
      "The Birth Wave by Share of the Reference Period after the Birth", "tab:ph_window_share",
      c("Share of the & \\multicolumn{2}{c}{Employer-financed} & \\multicolumn{2}{c}{Own time at any place} \\\\",
        "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
        "period after birth & Birth wave & Next wave & Birth wave & Next wave \\\\"),
      rows,
      paste0("Women only: mothers with a dated birth against never-mothers, person and wave effects. ",
             "The share is the part of the birth wave's twelve-month reference period that followed ",
             "the birth, from the birth quarter. Time away predicts a birth-wave fall that grows with ",
             "the share and a next-wave fall that shrinks with it. The slope row comes from a model ",
             "linear in the share, centered at 0.5. The slopes are contrasts among mothers, so a ",
             "pre-birth trend common to all mothers in the comparison with never-mothers leaves ",
             "them unchanged; the levels carry it. Standard errors clustered by person. Post hoc."),
      "lcccc")
  } else missing <- c(missing, "R12 window share")

  # ---- G. Part time by choice (R13) --------------------------------------
  pc <- tabs[["part_time_choice"]]; psh <- tabs[["part_time_choice_shares"]]
  if (!is.null(pc) && nrow(pc)) {
    terms <- c(post = "After the birth, full-time waves",
               post_pt_vol = "Extra, part time not wanting more hours",
               post_pt_und = "Extra, part time wanting more hours",
               pt = "Part time, women before any birth",
               pt_und = "Extra, wanting more hours, before any birth")
    rows <- lapply(names(terms), function(tm) c(terms[[tm]],
      get_cell(pc, label = "women, k>=2", term = tm, outcome = "E1"),
      get_cell(pc, label = "women, k>=2", term = tm, outcome = "O2"),
      get_cell(pc, label = "women, k>=2", term = tm, outcome = "CP_E1_O2")))
    rows <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    write_table("PH_R13_part_time_choice.tex",
      "Part Time by Whether More Hours Are Wanted, from the Second Year after the Birth", "tab:ph_part_time",
      c(" & Employer- & Own time & Proportional \\\\", " & financed & at any place & contrast \\\\"),
      rows,
      paste0("Women only, mothers against never-mothers, person and wave effects, employed ",
             "person-waves. Wanting more hours is the survey's preferred-hours question. ",
             if (!is.null(psh) && nrow(psh)) paste0(sprintf("%.0f", 100 * psh$share_wanting_more_hours[1]),
               " percent of mothers' part-time waves after the birth want more hours, against ",
               sprintf("%.0f", 100 * psh$share_wanting_more_hours[2]),
               " percent of never-mothers' part-time waves. ") else "",
             "The extra rows are contrasts among mothers' own waves, so a pre-birth trend common ",
             "to all mothers in the comparison with never-mothers leaves them unchanged. ",
             "Proportional contrast against own time at any place, bases treated as known. ",
             "Standard errors clustered by person. Post hoc."),
      "lccc", size = "\\footnotesize")
  } else missing <- c(missing, "R13 part time by choice")

  # ---- H. Earnings with the non-employed kept at zero (R5) ---------------
  # v-R5. Section VII.A quotes the birth-wave gap and the late-minus-early
  # widening in dollars, so the dollar columns print to the nearest hundred,
  # the precision the text uses, and asinh earnings print to three decimals.
  ew <- tabs[["earnings_windows"]]
  if (!is.null(ew) && nrow(ew)) {
    usd <- function(x) ifelse(is.na(x), "", gsub("-", "$-$",
      formatC(round(1000 * x, -2), format = "d", big.mark = ","), fixed = TRUE))
    one <- function(s, est, se) {
      r <- ew[!is.na(ew$series) & ew$series == s, , drop = FALSE]
      if (!nrow(r) || is.na(r[[est]][1])) return("")
      if (startsWith(s, "EARN_K")) paste0(usd(r[[est]][1]), " (", usd(r[[se]][1]), ")")
      else cell(r[[est]][1], r[[se]][1])
    }
    lp <- function(s) {
      r <- ew[!is.na(ew$series) & ew$series == s, , drop = FALSE]
      if (!nrow(r) || is.na(r$leads_p[1])) "" else num(r$leads_p[1])
    }
    ser <- c("EARN_K pre-specified form", "EARN_K age absorbed",
             "ASINH_EARN pre-specified form", "ASINH_EARN age absorbed")
    late <- range(RR_W_LATE); early <- range(RR_W_EARLY); lead <- range(-RR_LEADS)
    rows <- list(
      c("At the birth", vapply(ser, one, character(1), est = "k0", se = "k0_se")),
      c(sprintf("Years %d--%d less years %d--%d", late[1], late[2], early[1], early[2]),
        vapply(ser, one, character(1), est = "late_minus_early", se = "late_minus_early_se")),
      c("Pre-birth leads, $p$-value", vapply(ser, lp, character(1))))
    rows <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    # v-i10header, 19 October: the columns read "Without age effects" and "Age effects";
    # "Pre-specified" implied a timing the ledger does not record.
    write_table("PH_R5_earnings.tex",
      "Earnings around the First Birth, with the Non-Employed Kept at Zero", "tab:ph_earnings",
      c(" & \\multicolumn{2}{c}{Earnings, dollars} & \\multicolumn{2}{c}{asinh earnings} \\\\",
        "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
        " & Without age effects & Age effects & Without age effects & Age effects \\\\"),
      rows,
      paste0("Sun--Abraham event-study estimates, women minus men, with person and wave effects; ",
             "the age-effects columns add age fixed effects. All person-waves in the event sample, ",
             "with the non-employed kept at zero earnings. Earnings are financial-year wages and ",
             "salary, including salary sacrifice, in Wave~24 dollars; dollar estimates are rounded ",
             "to the nearest hundred. ",
             sprintf("The second row is the mean gap in years %d to %d after the birth less the mean in years %d to %d. ",
                     late[1], late[2], early[1], early[2]),
             sprintf("The leads row is the $p$-value of a joint test that the gaps %d to %d years before the birth are zero. ",
                     lead[1], lead[2]),
             "Earnings combine participation, hours and the wage, so they fall at the birth by ",
             "construction; the path bounds the wage sample's selection rather than testing when ",
             "the wage responds. Standard errors clustered by person. Post hoc."),
      "lcccc", size = "\\footnotesize")
  } else missing <- c(missing, "R5 earnings")

  # ---- the section file ---------------------------------------------------
  order <- c("PH_R2_contrast.tex", "PH_R10_checks.tex", "PH_R14_hours_fixed.tex",
             "PH_R7_hours_accounting.tex", "PH_R3_R15_exposure.tex", "PH_R12_window_share.tex",
             "PH_R13_part_time_choice.tex", "PH_R11_exit.tex", "PH_R1_recovery.tex",
             "PH_R5_earnings.tex")
  written <- c(intersect(order, written), setdiff(written, order))
  if (length(written)) {
    sec <- c("\\section{I. Post-Hoc Analyses}", "\\label{sec:app_posthoc}",
             # v-posthoc-ledger, 23 October 2026. The paper describes every post-hoc
             # analysis one way: it carries no designation. Most, these among them,
             # are not in the ledger file, so the sentence no longer says where they are.
             paste0("The tables in this appendix report post-hoc analyses, which carry no designation. ",
                    "Section~\\ref{sec:disc_limits} states their limits."), "",
             paste0("\\input{tables/", sub("\\.tex$", "", written), "}"))
    writeLines(sec, file.path(tdir, "PH_appendix_section.tex"))
  }
  cat("  tables written to ", tdir, ":\n", sep = "")
  for (f in written) cat("    ", f, "\n", sep = "")
  if (length(missing)) cat("  not written, results missing: ", paste(missing, collapse = "; "), "\n", sep = "")
  rr$tabs$appendix_tables <- data.frame(file = written, stringsAsFactors = FALSE)
}))

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n", strrep("=", 76), "\nSUMMARY\n", strrep("=", 76), "\n", sep = "")
if (!is.null(rr$tabs[["reproduction"]]))
  cat(sprintf("  reproduction: %d of %d manuscript targets matched\n",
              sum(rr$tabs[["reproduction"]]$match, na.rm = TRUE),
              nrow(rr$tabs[["reproduction"]])))
if (!is.null(rr$tabs[["contrast"]])) {
  key <- rr$tabs[["contrast"]][rr$tabs[["contrast"]]$scale == "proportional" &
                            (rr$tabs[["contrast"]]$window == "k>=2" |
                               rr$tabs[["contrast"]]$sample == "all"), ]
  for (i in seq_len(nrow(key))) {
    bt  <- is.finite(key$boot_se[i])
    sev <- if (bt) key$boot_se[i] else key$se[i]
    cat(sprintf("  %-6s %-5s %-6s proportional contrast %+.3f (%s %.3f)%s\n",
                key$sample[i], key$window[i], key$pair[i], key$est[i],
                if (bt) "boot SE" else "SE, bases fixed", sev,
                if (is.finite(key$est[i] / sev) && abs(key$est[i] / sev) >= 1.96)
                  "" else "  equal proportional falls not rejected"))
  }
}
if (!is.null(rr$tabs[["hours"]])) {
  s1 <- rr$tabs[["hours"]]$single
  s1 <- s1[s1$version == "transport" & s1$outcome == "E1", ]
  for (i in seq_len(nrow(s1)))
    cat(sprintf("  R7 %-5s hours explain %3.0f%% of the paid-time fall (%+.3f of %+.3f)\n",
                s1$window[i], 100 * s1$share_explained[i], s1$orig[i] - s1$net_of_hours[i],
                s1$orig[i]))
  c1 <- rr$tabs[["hours"]]$contrast
  c1 <- c1[c1$version == "transport", ]
  for (i in seq_len(nrow(c1)))
    cat(sprintf("  R7 %-5s %-6s proportional contrast net of hours %+.3f (boot SE %s), was %+.3f\n",
                c1$window[i], c1$pair[i], c1$net_of_hours_prop[i],
                if (is.finite(c1$net_boot_se[i])) sprintf("%.3f", c1$net_boot_se[i]) else "not bootstrapped",
                c1$orig_prop[i]))
}
if (!is.null(rr$tabs[["two_clocks"]])) {
  t8 <- rr$tabs[["two_clocks"]]
  tb8 <- t8[t8$outcome == "E1" & t8$label == "(b) late switchers", ]
  if (nrow(tb8))
    cat(sprintf("  R8 late switchers, paid time: from k = 2 %+.3f, extra at the switch %+.3f (SE %.3f)\n",
                tb8$est[tb8$term == "k2_plus"], tb8$est[tb8$term == "after_switch"],
                tb8$se[tb8$term == "after_switch"]))
  tc8 <- t8[t8$outcome == "E1" & t8$label == "(c) full time from k = 1", ]
  if (nrow(tc8))
    cat(sprintf("  R8 full time from k = 1, paid time from k = 2: %+.3f (SE %.3f)\n",
                tc8$est[tc8$term == "k2_plus"], tc8$se[tc8$term == "k2_plus"]))
}
if (!is.null(rr$tabs[["exit"]])) {
  x <- rr$tabs[["exit"]]
  x1 <- x[x$label == "(a) mothers minus fathers, k>=0" & x$outcome == "chance_leave_job", ]
  if (nrow(x1)) cat(sprintf("  R11 mothers' chance of leaving after the birth: %+.3f (SE %.3f)\n",
                            x1$est, x1$se))
  x2 <- x[x$label == "(b) by pre-birth chance, k>=2" & x$outcome == "E1", ]
  if (nrow(x2) == 2)
    cat(sprintf("  R11 paid time from k = 2: mothers expecting to stay %+.3f (SE %.3f), extra if expecting to leave %+.3f (SE %.3f)\n",
                x2$est[x2$term == "post_f"], x2$se[x2$term == "post_f"],
                x2$est[x2$term == "post_f_hi"], x2$se[x2$term == "post_f_hi"]))
}
if (!is.null(rr$tabs[["window_share"]])) {
  x <- rr$tabs[["window_share"]]
  x <- x[x$outcome == "E1" & x$term %in% c("k0_share", "k1_share"), ]
  if (nrow(x) == 2)
    cat(sprintf("  R12 paid time, slope in the window share: k = 0 %+.3f (SE %.3f), k = 1 %+.3f (SE %.3f)\n",
                x$est[x$term == "k0_share"], x$se[x$term == "k0_share"],
                x$est[x$term == "k1_share"], x$se[x$term == "k1_share"]))
}
if (!is.null(rr$tabs[["part_time_choice"]])) {
  x <- rr$tabs[["part_time_choice"]]
  x <- x[x$label == "women, k>=2" & x$outcome == "E1", ]
  if (nrow(x))
    cat(sprintf("  R13 paid time from k = 2: full-time waves %+.3f, extra in chosen part time %+.3f, extra in part time wanting more hours %+.3f\n",
                x$est[x$term == "post"], x$est[x$term == "post_pt_vol"],
                x$est[x$term == "post_pt_und"]))
}
if (!is.null(rr$tabs[["hours_fathers"]])) {
  x <- rr$tabs[["hours_fathers"]]
  x1 <- x[x$outcome == "E1" & x$term == "post_f" & x$label == "(a) all parents, k>=2", ]
  x2 <- x[x$outcome == "E1" & x$term == "k2_plus_f" &
            x$label == "(c) mothers full time from k = 1, against all fathers", ]
  if (nrow(x1)) cat(sprintf("  R14 paid time from k = 2, full-time waves, mothers minus fathers: %+.3f (SE %.3f)\n",
                            x1$est, x1$se))
  if (nrow(x2)) cat(sprintf("  R14 mothers full time from k = 1, from k = 2, against fathers: %+.3f (SE %.3f)\n",
                            x2$est, x2$se))
}
if (!is.null(rr$tabs[["leave_days"]])) {
  x <- rr$tabs[["leave_days"]]
  x <- x[x$sample == "all" & x$window == "k>=0" & x$outcome == "E1", ]
  for (i in seq_len(nrow(x)))
    cat(sprintf("  R15 paid time, %s: %+.3f (SE %.3f)%s\n", x$label[i], x$est[i], x$se[i],
                if (isTRUE(x$unpaid_included[i])) "" else "  [paid leave only]"))
}
if (!is.null(rr$tabs[["exposure"]])) {
  x <- rr$tabs[["exposure"]]
  x <- x[x$sample == "all" & x$window == "k>=0" & x$outcome == "E1", ]
  for (i in seq_len(nrow(x)))
    cat(sprintf("  R3  paid time, %s: %+.3f (SE %.3f)\n", x$label[i], x$est[i], x$se[i]))
}
if (!is.null(rr$tabs[["prop_path_growth"]])) {
  g9 <- rr$tabs[["prop_path_growth"]]
  for (i in seq_len(nrow(g9)))
    cat(sprintf("  R9 %-6s proportional contrast, years 5-8 less years 0-1: %+.3f (SE %.3f)\n",
                g9$pair[i], g9$years_5_8_minus_0_1[i], g9$se_5_8[i]))
}
if (length(rr$fail)) {
  cat("\n  BLOCKS THAT FAILED:\n")
  for (x in rr$fail) cat("    ", x, "\n", sep = "")
} else cat("\n  no block failed.\n")
if (length(rr$skip)) {
  cat("  skipped for want of an input:\n")
  for (x in rr$skip) cat("    ", x, "\n", sep = "")
}
cat("  outputs: ", RR_DIR, "\n", sep = "")
cat("  All of it is post-hoc. Nothing here enters the ledger or the paper\n",
    "  until you move it there and mark it.\n", sep = "")

# Merged into what an earlier run left, so a run of selected blocks updates
# its own tables and keeps the rest.
rr_prev <- if (exists("td_has", mode = "function") && isTRUE(td_has("review_response")))
  tryCatch(td_get("review_response"), error = function(e) list()) else list()
if (!is.list(rr_prev)) rr_prev <- list()
rr_prev[names(rr$tabs)] <- rr$tabs
td_put("review_response", rr_prev)
rm(rr_prev)

# Each block runs in local(), so its working objects are gone already.
# These few are the top-level loop variables.
rm(list = intersect(c("s", "i", "x", "key", "sev", "s1", "c1", "t8", "tb8", "tc8", "g9",
                     "rr_o4_cand", "rr_o4_col", "x", "x1", "x2"), ls()))
if (exists("td_check_seals", mode = "function"))
  td_check_seals("05_review_response.R")
if (exists("td_log_close", mode = "function"))
  try(td_log_close(), silent = TRUE)
