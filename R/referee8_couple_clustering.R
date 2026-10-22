# =============================================================================
# referee8_couple_clustering.R
# -----------------------------------------------------------------------------
# REFEREE POINT 8. The two sex arms are fitted separately and their variances
# are added:
#
#     gap <- function(a, b) ... se = sqrt(a$se^2 + b$se^2)
#
# That is right only if the two estimators are independent. The individuals
# are disjoint, which is what the manuscript says, but the BIRTHS are not:
# many mothers and fathers in this sample are partners in the same birth, so
# a shock to one couple enters both arms.
#
# WHAT THE SIGN IMPLIES, AND WHY IT IS WORTH KNOWING FIRST.
#
#     Var(w - m) = Var(w) + Var(m) - 2 Cov(w, m)
#
# The current formula sets Cov to zero. If partners' residuals are POSITIVELY
# correlated, which assortative matching and a shared local labour market both
# predict, then Cov > 0 and the sum OVERSTATES the variance: the reported
# standard errors are conservative and the referee's objection, though correct
# in principle, runs in the paper's favour. If negative, they are too small
# and the timing claims that turn on significance need re-reading.
#
# SO THIS RUNS IN TWO STAGES, CHEAP FIRST.
#
#   Stage 1, seconds: how much overlap is there at all, and what is the sign
#   of the within-couple residual correlation. If the overlap is small the
#   whole question is moot and stage 2 is not worth running.
#
#   Stage 2, minutes: a block bootstrap over couples. Resample couples with
#   replacement, refit both arms, recompute the gap at each horizon, and take
#   the standard deviation across replicates. That estimates Var(gap) WITH the
#   covariance in it, and preserves the estimator exactly, which a joint fit
#   with sunab() interacted by sex would not.
#
# WHY NOT THE JOINT FIT. sunab() generates cohort-by-relative-time
# interactions and does not interact cleanly with a further factor in fixest.
# A stacked model with plain event-time dummies would interact, but it is a
# different estimator, so a difference in the gap would confound the change
# of estimator with the change of variance formula. The bootstrap changes
# only the variance.
#
# NOT YET RUN. Written against the structures CS_event_study.R uses and
# checked for syntax, not for behaviour. Read stage 1's output before
# trusting stage 2, and treat the first bootstrap as a pilot: if the
# replicate SDs are wild, B is too small or a cluster is dominating.
# =============================================================================

# LIBRARIES FIRST, THEN THE PROTECTION BLOCK. This file used %>% without
# attaching dplyr and got away with it only because MASTER and the stage
# scripts attach it earlier in any session that runs them. In a fresh
# session with just the contract loaded it failed at the first pipe. The
# namespace block below assigns dplyr's verbs but does not attach the
# package, so it cannot supply the pipe either.
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

# THE CONTRACT IS LOADED HERE WHEN THE SESSION LACKS IT, as robust_30_49.R
# does. This line used to stop instead, which kept the file out of RUN_ALL.R:
# every stage there runs in a fresh process, with no contract in it. On
# 5 October that left referee8_summary.csv from 25 September, older than
# the panel, feeding three macros.
if (!(exists("PROJ_ROOT") && exists("td_frame", mode = "function"))) {
  .c <- c("00_contract.R", file.path("..", "00_contract.R"))
  .c <- .c[file.exists(.c)]
  if (!length(.c))
    stop("00_contract.R not found from ", getwd(), ". setwd() to the ",
         "project root and source this file again.", call. = FALSE)
  cat("Contract: not loaded in this session; sourcing ",
      normalizePath(.c[1], winslash = "/"), "\n", sep = "")
  source(.c[1])
  rm(.c)
}
stopifnot(exists("PROJ_ROOT"), exists("td_frame"))

# --- logging ---------------------------------------------------------------
# WHY THIS FILE NEEDS ONE. It prints the overlap share, the residual
# correlation and nineteen bootstrap standard errors, and the paper will
# quote three of those. A number that exists only in a console buffer is a
# number nobody can check later, which is the whole class of fault this
# project has been closing. The two csvs it writes carry the values; the log
# carries how they were arrived at, including the roster assertion.
#
# GUARDED AND NESTED-SAFE, the way 04financing_space.R does it. If a caller
# already owns a sink, td_log_open() reports that it inherits and opens no
# second file; if the contract is not loaded at all, this is a no-op and the
# output goes to the console as before.
# silent = TRUE HIDES THE ONE THING WORTH KNOWING. 04financing_space.R wraps
# this call the same way, and on 19 September both this file and
# paper_tables.R produced no log with no explanation: a sink was already open
# in the session, so td_log_open() inherited it and wrote nothing of its own.
# That is correct behaviour and it should not be silent, because "no log" and
# "log inherited by another file" look identical afterwards and only one of
# them means something was left open.
.r8_log <- exists("td_log_open", mode = "function")
if (.r8_log) {
  .r8_sink0 <- sink.number()
  .r8_try <- try(td_log_open("referee8_couple_clustering.R"), silent = TRUE)
  if (inherits(.r8_try, "try-error"))
    cat("  td_log_open() failed: ", conditionMessage(attr(.r8_try, "condition")),
        "\n  Output goes to the console only.\n", sep = "")
  else {
    # BY FILE, NOT BY SINK COUNT. The first version compared sink.number()
    # before and after, and said "inherited" when it had not changed. But
    # when an ORPHAN sink is open, td_log_open() unwinds it and opens a
    # file of its own: one closed, one opened, count unchanged. On 21
    # September that printed "writes into the caller's log" above the
    # header of this script's own log file. The registry names the file
    # actually being written, which is the thing worth knowing.
    .r8_f <- if (exists(".TD_LOG") && exists("file", envir = .TD_LOG))
      get("file", envir = .TD_LOG) else NA_character_
    if (is.na(.r8_f))
      cat("  no registry log is open; output is console only.\n")
    # NO CLAIM ABOUT WHAT td_log_open() DID TO AN EARLIER SINK. The first
    # version said "an orphan sink was open and was closed first", and on
    # 21 September td_log_open() had NESTED into a dead run of this same
    # script instead, closing nothing. The contract prints its own line
    # saying which branch it took; this only reports that a sink predated
    # the script, which is the fact the sink count can actually support.
    else if (grepl("referee8", basename(.r8_f)))
      cat("  log: ", .r8_f,
          if (.r8_sink0 > 0)
            paste0("\n  (", .r8_sink0, " sink(s) were already open when this",
                   " started; the td_log_open line above says what it did",
                   " with them)") else "",
          "\n", sep = "")
    else
      cat("  writing into the caller's log: ", .r8_f, "\n", sep = "")
    rm(.r8_f)
  }
  rm(.r8_sink0, .r8_try)
} else {
  cat("  no contract loaded, so there is no log. Output is console only.\n")
}

# --- THE BODY RUNS INSIDE tryCatch(finally = close) ----------------------
#
# WHY. source() has no exit hook of its own, so an error anywhere below used
# to skip the td_log_close() at the foot and leave this file's sink open. On
# 21 September the first run died at the first pipe: its sink stayed open,
# every command typed afterwards was written into this file's log, the
# pre-flight stopifnot(sink.number() == 0L) failed twice, and the re-run
# nested into the dead run's registry entry and left one level open when it
# finished. Every orphan sink met in this project had this shape: a script
# that errored after opening its log.
#
# finally RUNS ON SUCCESS AND ON ERROR, and the error still propagates after
# it, so a failure is reported exactly as before and the log is closed
# either way. The body keeps its top-level assignments: tryCatch evaluates
# its expression in the calling environment, which for a sourced file is
# the global one. It is not re-indented, to keep the diff to the lines that
# changed.
.r8_ok <- FALSE
tryCatch({

R8_B      <- if (exists("R8_B")) R8_B else 200L   # bootstrap replicates
R8_SEED   <- 20260919L
# READ FROM results, WRITE TO diagnostics. This wrote into output/results,
# beside timing_paths.csv, and Check 7b compares each arm's file against the
# newest artefact in the same folder: on 21 September it reported both arms
# "0.2 days behind", which was this script's own csvs at 19:12 against
# MASTER's at 13:31. A warning that fires on every correctly ordered pass
# stops being read, so the outputs of a robustness diagnostic go where the
# diagnostics go and leave MASTER's folder to MASTER.
R8_IN     <- if (exists("DIR_RESULTS")) DIR_RESULTS else getwd()
R8_OUT    <- if (exists("DIR_DIAG"))    DIR_DIAG    else R8_IN

# THE ROSTER MUST NOT OUTLIVE THIS FILE UNDER THE NAME `panel`. Below,
# `panel` is narrowed to the event-study roster, 66,719 rows. Left in the
# session, it was picked up by paper_tables.R, which prefers an in-memory
# `panel`, and on 22 September and again on 25 September every sample macro
# was built from the roster: \PanelRows 66,719, \LeftCensored 0. So the file
# notes whether the session already had a `panel`, and on every exit puts
# that one back or removes its own.
.r8_env <- environment()
.r8_had_panel <- exists("panel", envir = .r8_env, inherits = FALSE)
if (.r8_had_panel) .r8_panel_before <- get("panel", envir = .r8_env)

panel <- td_frame(context = "referee8")

# --- the couple key ---------------------------------------------------------
# THE UNORDERED PAIR, not partner_id itself. partner_id varies by wave when a
# partnership changes, and a cluster has to be time-invariant for the person
# or the block is not a block. The FIRST observed partner fixes it; a person
# never observed with a partner clusters on themselves, which is the correct
# degenerate case and not a dropped observation.
if (!"partner_id" %in% names(panel))
  stop("panel has no partner_id, so no couple key can be built.", call. = FALSE)

.first_p <- panel %>%
  filter(!is.na(partner_id), partner_id != "") %>%
  arrange(person_id, wave) %>%
  group_by(person_id) %>%
  summarise(p1 = dplyr::first(partner_id), .groups = "drop")

.key <- .first_p %>%
  mutate(couple = paste0(pmin(person_id, p1), "_", pmax(person_id, p1))) %>%
  select(person_id, couple)

panel <- panel %>%
  dplyr::left_join(.key, by = "person_id") %>%
  mutate(couple = ifelse(is.na(couple), person_id, couple))

# --- event time, built here ------------------------------------------------
# k IS NOT ON THE PANEL. It is created in 03_common.R from the cohort join,
# so a script that reads the frame directly has to build it. The rule is the
# one 03_common.R uses: a person enters the roster if ever_parent is 0 in
# their first observed wave, and their cohort is the first wave in which it
# turns on. birth_w is computed only where a treated wave exists, because
# min() over an empty set returns Inf and as.integer(Inf) warns.
coh <- panel %>%
  arrange(person_id, wave) %>%
  group_by(person_id) %>%
  summarise(first_ep = dplyr::first(ever_parent),
            ever_ep  = if (any(ever_parent == 1, na.rm = TRUE)) 1L else 0L,
            birth_w  = if (any(ever_parent == 1, na.rm = TRUE))
              as.integer(min(wave[ever_parent == 1], na.rm = TRUE)) else
                NA_integer_,
            .groups  = "drop") %>%
  filter(!is.na(first_ep), first_ep == 0) %>%
  mutate(cohort = ifelse(ever_ep == 1L & !is.na(birth_w), birth_w, 10000L)) %>%
  select(person_id, cohort)

panel <- panel %>%
  dplyr::inner_join(coh, by = "person_id") %>%
  mutate(k = ifelse(cohort < 10000L, wave - cohort, NA_integer_))

cat(sprintf("\n  roster: %s persons, %s treated, %s person-waves\n",
            format(dplyr::n_distinct(panel$person_id), big.mark = ","),
            format(sum(coh$cohort < 10000L), big.mark = ","),
            format(nrow(panel), big.mark = ",")))

# THE COHORT RULE IS REBUILT HERE, SO IT CAN DRIFT FROM 03_common.R.
#
# This file reads the frame directly and reconstructs event time rather than
# taking it from the estimation stack, which is what lets it run outside a
# pass. The cost is a SECOND DEFINITION of who is on the roster, and two
# definitions of one thing is how \RosterPersons came to have two values
# earlier in this work.
#
# SO IT ASSERTS. 03a reports the roster on every run; if this file disagrees
# with it, nothing below is comparable to the estimates the paper quotes and
# the right response is to stop rather than to publish a bootstrap of a
# different sample. Override R8_ROSTER if the roster legitimately changes.
#
# A VALUE LEFT IN THE SESSION FAILS EVERY LATER RUN. On 22 September the
# guard was tested by setting R8_ROSTER to 1, 1, 1 before sourcing this file.
# A session that still holds a test value stops here with a roster that is
# in fact correct, so the file says which value it checks against.
# v21.49, 5 OCTOBER. LOADING_ND3_v3.R v21.49 counts own children only, so
# 03a's roster moved from 10,650 persons, 2,704 treated and 66,719
# person-waves to 10,838, 2,703 and 67,783. The default follows 03a.
# v-screen, 14 OCTOBER. 00_contract.R v28 applies no screen by default, so
# 03a's roster is 10,902 persons, 2,708 treated and 68,361 person-waves
# (screen_audit.R). Under TD_SCREEN=on it is the screened roster above.
.r8_default <- if (isTRUE(get0("TD_SCREEN_ON", ifnotfound = FALSE)))
  c(persons = 10838L, treated = 2703L, rows = 67783L) else
  c(persons = 10902L, treated = 2708L, rows = 68361L)
R8_ROSTER <- if (exists("R8_ROSTER")) R8_ROSTER else .r8_default
.r8_override <- !identical(as.integer(R8_ROSTER), as.integer(.r8_default))
if (.r8_override)
  cat("  R8_ROSTER was set in this session to a value other than the\n",
      "  default; checking against the session's value.\n", sep = "")
.got <- c(persons = dplyr::n_distinct(panel$person_id),
          treated = sum(coh$cohort < 10000L),
          rows    = nrow(panel))
if (!all(is.na(R8_ROSTER)) && !identical(as.integer(.got), as.integer(R8_ROSTER))) {
  cat("\n  *** ROSTER DISAGREES WITH 03a.\n")
  print(rbind(expected = R8_ROSTER, got = .got))
  if (.r8_override)
    cat("  The expected row came from the session, not from this file. If it\n",
        "  was set for a test, run rm(R8_ROSTER) and source the file again.\n",
        sep = "")
  stop("referee8: the cohort rule here no longer matches the one the ",
       "estimates use. Reconcile against 03_common.R before reading ",
       "anything below.", call. = FALSE)
}
cat("  matches 03a's roster.\n")
rm(.got, .r8_override, .r8_default)

# --- STAGE 1. How much overlap, and which way does it point? ----------------
.ev <- panel %>% filter(!is.na(k), employed == 1)
.both <- .ev %>%
  group_by(couple) %>%
  summarise(sexes = dplyr::n_distinct(female), .groups = "drop") %>%
  filter(sexes == 2)

cat("\n=== referee point 8: overlap between the two arms ===\n")
cat(sprintf("  estimation person-waves        %s\n",
            format(nrow(.ev), big.mark = ",")))
cat(sprintf("  distinct clusters              %s\n",
            format(dplyr::n_distinct(.ev$couple), big.mark = ",")))
cat(sprintf("  clusters with BOTH sexes       %s (%.1f%% of clusters)\n",
            format(nrow(.both), big.mark = ","),
            100 * nrow(.both) / dplyr::n_distinct(.ev$couple)))
# CAPTURED HERE, BECAUSE STAGE 2 REDEFINES .ev. The bootstrap frame adds the
# never-treated with k set to the reference horizon, which enlarges the
# denominator: computed there against the treated-only .both, the same
# expression returned 30.2 per cent where this one returns 72.7. Two numbers
# for one quantity, and the smaller one would have gone into the paper.
.ov8 <- 100 * mean(.ev$couple %in% .both$couple)
cat(sprintf("  person-waves in those clusters %s (%.1f%% of the sample)\n",
            format(sum(.ev$couple %in% .both$couple), big.mark = ","), .ov8))
cat("\n  A small share here means the two arms are nearly independent and the\n",
    "  summed variance is nearly right, whatever the correlation.\n", sep = "")

# THE SIGN. Residuals from the same specification the arms use, correlated
# between the two partners of a couple within a wave. This is not Cov(w, m)
# itself, which is a covariance between two ESTIMATORS, but it carries the
# sign and the rough magnitude, and it costs one regression.
if (nrow(.both)) {
  .r <- .ev %>% filter(couple %in% .both$couple)
  .fit <- tryCatch(
    feols(ln_hourly_wage_real ~ 1 | person_id + wave, data = .r,
          notes = FALSE, warn = FALSE), error = function(e) NULL)
  if (!is.null(.fit)) {
    # RESIDUALS COME BACK SHORTER THAN THE FRAME. feols drops any row with a
    # missing outcome, and it also drops singleton fixed-effect groups, so
    # the vector cannot be assigned wholesale: on this sample 15,991
    # residuals came back for 17,295 rows. fixest::obs() gives the indices
    # of the rows actually used, which is robust to every reason a row might
    # have gone rather than just the one we know about.
    .r$resid <- NA_real_
    .used <- tryCatch(fixest::obs(.fit), error = function(e) NULL)
    if (is.null(.used) || length(.used) != length(residuals(.fit)))
      .used <- which(!is.na(.r$ln_hourly_wage_real))
    if (length(.used) == length(residuals(.fit)))
      .r$resid[.used] <- as.numeric(residuals(.fit)) else
        cat("  could not align residuals to rows; the correlation is skipped.\n")
    .w <- .r %>% filter(female == 1, !is.na(resid)) %>%
      group_by(couple, wave) %>%
      summarise(rw = mean(resid), .groups = "drop")
    .m <- .r %>% filter(female == 0, !is.na(resid)) %>%
      group_by(couple, wave) %>%
      summarise(rm = mean(resid), .groups = "drop")
    .p <- dplyr::inner_join(.w, .m, by = c("couple", "wave"))
    if (nrow(.p) > 30) {
      .rho <- suppressWarnings(stats::cor(.p$rw, .p$rm, use = "complete.obs"))
      cat(sprintf("\n  within-couple residual correlation %+.3f on %s",
                  .rho, format(nrow(.p), big.mark = ",")),
          " couple-waves\n", sep = "")
      cat(if (is.finite(.rho) && .rho > 0)
        paste0("  POSITIVE, so Cov(w, m) > 0 and sqrt(se_w^2 + se_m^2)",
               " OVERSTATES\n  the gap's standard error. The reported",
               " intervals are conservative.\n")
        else if (is.finite(.rho))
          paste0("  NEGATIVE, so the summed variance UNDERSTATES the gap's",
                 " standard\n  error. Every significance claim on the gap",
                 " needs re-reading.\n")
        else "  not estimable on these rows.\n")
    } else cat("\n  too few matched couple-waves to correlate.\n")
    rm(.w, .m, .p)
    if (exists(".used")) rm(.used)
    # .rho is NOT removed: the summary below reports it.
  }
  rm(.r)
  if (exists(".fit")) rm(.fit)
}

# --- STAGE 2. Block bootstrap over couples ----------------------------------
# NEW IDS FOR DUPLICATED DRAWS. A cluster drawn twice must enter as two
# distinct people, or the person fixed effect pools the copies and the
# resample is not a resample. This is the step that is easy to omit and
# impossible to see afterwards: the estimates would look fine and the
# standard errors would be too small.
if (isTRUE(R8_B > 0)) {
  cat(sprintf("\n=== block bootstrap over couples, B = %d ===\n", R8_B))
  set.seed(R8_SEED)
  # NEVER-TREATED GO IN THE REFERENCE BIN, NOT OUT OF THE SAMPLE. i(k) drops
  # rows whose k is NA, which would take every control with it and change
  # the estimator rather than the variance. Coding them to the reference
  # horizon keeps them contributing through the wave effects, which is what
  # they do in the sunab fit this is estimating the variance of.
  .ev <- panel %>% filter(employed == 1) %>%
    mutate(k = ifelse(is.na(k), -1L, k)) %>%
    filter(k >= -8, k <= 10)
  .cl  <- unique(.ev$couple)
  .ks  <- sort(unique(.ev$k[.ev$k >= -8 & .ev$k <= 10]))
  .out <- matrix(NA_real_, nrow = R8_B, ncol = length(.ks),
                 dimnames = list(NULL, as.character(.ks)))

  .arm <- function(d, fem) {
    dd <- d[d$female == fem, , drop = FALSE]
    if (!nrow(dd)) return(setNames(rep(NA_real_, length(.ks)), .ks))
    m <- tryCatch(feols(ln_hourly_wage_real ~ i(k, ref = -1) | bid + wave,
                        data = dd, notes = FALSE, warn = FALSE),
                  error = function(e) NULL)
    if (is.null(m)) return(setNames(rep(NA_real_, length(.ks)), .ks))
    ct <- coef(m); nm <- sub("^k::", "", names(ct))
    setNames(ct[match(as.character(.ks), nm)], .ks)
  }

  for (b in seq_len(R8_B)) {
    draw <- sample(.cl, length(.cl), replace = TRUE)
    idx  <- data.frame(couple = draw, rep = seq_along(draw),
                       stringsAsFactors = FALSE)
    d <- dplyr::inner_join(.ev, idx, by = "couple", relationship = "many-to-many")
    d$bid <- paste0(d$person_id, "#", d$rep)     # the new-id step
    .out[b, ] <- .arm(d, 1) - .arm(d, 0)
    if (b %% 25 == 0) cat("  ", b, " of ", R8_B, "\n", sep = "")
  }

  .bse <- apply(.out, 2, stats::sd, na.rm = TRUE)
  .res <- data.frame(event_time = as.integer(.ks),
                     boot_se = round(unname(.bse), 4),
                     n_ok = colSums(!is.na(.out)))
  cat("\n  bootstrap standard errors on the gap, by horizon:\n")
  print(.res, row.names = FALSE)
  cat("\n  Compare each with sqrt(se_w^2 + se_m^2) from the main run. A\n",
      "  bootstrap SE BELOW the summed one confirms a positive covariance\n",
      "  and a conservative reported interval; above it is the case that\n",
      "  would change the paper.\n", sep = "")
  utils::write.csv(.res, file.path(R8_OUT, "referee8_gap_bootstrap_se.csv"),
                   row.names = FALSE)
  cat("  wrote referee8_gap_bootstrap_se.csv\n")

  # --- THE HEADLINE NUMBERS, WRITTEN WHERE A MACRO CAN REACH THEM -------
  #
  # The per-horizon table above is the evidence; the paper will quote three
  # summary figures from it. Typed, those three drift the moment B or the
  # sample changes, which is the failure this project has spent its time
  # removing. Written here, paper_tables.R can put() them and Check 9 can
  # trace them.
  #
  # THE RATIO IS THE POST-BIRTH MEDIAN, not the whole path: the pre-birth
  # horizons are not what any claim rests on, and including them moves the
  # figure without meaning anything.
  .an <- tryCatch({
    .r0 <- readRDS(file.path(R8_IN, "cs_event_study_results.rds"))
    .g0 <- .r0[["gap ln_hourly_wage_real"]]
    merge(.g0[, c("event_time", "se")], .res, by = "event_time")
  }, error = function(e) NULL)
  .sum8 <- data.frame(
    quantity = c("overlap_share", "resid_corr", "boot_se_ratio_post"),
    value = c(round(.ov8, 1),
              round(if (exists(".rho") && is.finite(.rho)) .rho else NA_real_, 3),
              if (is.null(.an)) NA_real_ else
                round(stats::median((.an$boot_se / .an$se)[.an$event_time >= 0],
                                    na.rm = TRUE), 3)),
    basis = c("per cent of estimation person-waves in clusters holding both sexes",
              "within-couple correlation of partners' wage residuals",
              "median bootstrap SE over analytic SE, post-birth horizons"),
    stringsAsFactors = FALSE)
  print(.sum8, row.names = FALSE)
  utils::write.csv(.sum8, file.path(R8_OUT, "referee8_summary.csv"),
                   row.names = FALSE)
  cat("  wrote referee8_summary.csv\n")
  rm(.an, .sum8)
  if (exists(".r0")) rm(.r0)
  if (exists(".g0")) rm(.g0)
  rm(.cl, .ks, .out, .arm, .bse, .res)
}

rm(.ev, .both, .first_p, .key)
if (exists(".ov8")) rm(.ov8)

.r8_ok <- TRUE          # the last line of the body: reaching it IS success
}, finally = {
  # CLOSE ONLY WHAT THIS FILE OPENED, ON EVERY PATH. td_log_close() is a
  # no-op on an inherited log and unwinds only its own depth, so a caller
  # that had a sink open gets it back exactly as it was.
  #
  # THE BANNER SAYS WHICH PATH. finally runs on both, and td_log_close()
  # defaults to "completed". On 22 September a deliberate roster failure
  # closed its log correctly and stamped it "completed in 1s", which a
  # reader scanning run_logs would take for success. .r8_ok is set on the
  # body's last line, so it is TRUE only if nothing above it stopped, and
  # the status is the same word the contract's own exit finalizer uses.
  if (isTRUE(.r8_log) && exists("td_log_close", mode = "function"))
    try(td_log_close(if (isTRUE(.r8_ok)) "completed" else "stopped"),
        silent = TRUE)
  rm(.r8_log, .r8_ok)
  # PUT BACK THE CALLER'S `panel`, OR REMOVE THIS FILE'S ROSTER.
  if (exists(".r8_had_panel")) {
    if (isTRUE(.r8_had_panel)) {
      assign("panel", .r8_panel_before, envir = .r8_env)
    } else if (exists("panel", envir = .r8_env, inherits = FALSE)) {
      rm("panel", envir = .r8_env)
    }
    rm(.r8_had_panel)
    if (exists(".r8_panel_before")) rm(.r8_panel_before)
  }
  if (exists(".r8_env")) rm(.r8_env)
})
