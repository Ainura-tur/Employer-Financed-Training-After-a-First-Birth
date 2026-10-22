# =============================================================================
# DATA AVAILABILITY
# Raw data: HILDA Survey (Waves 12-24), accessed via the Australian Data Archive
# (ADA). Researchers must apply via: https://dataverse.ada.edu.au/dataverse/hilda
# Data inputs (all built by LOADING_ND3_v3.R in a single pass):
#   hilda_panel_data_W12_W24_slim.rds  (read below)
#   hilda_panel_data_extended.rds      (read by the sourced modules)
#   hilda_panel_data_training.rds      (training event study fallback)
# The RDS files themselves cannot be shared.
# =============================================================================

# =============================================================================
# MASTER_train.R  -  v23. REBUILT AROUND THE TRAINING CLAIM
#
# THE CLAIM, in one sentence, from TRAINING_PAPER_OUTLINE.md section 1:
#
#   Employers withdraw training investment from mothers, and mothers
#   substitute their own time.
#
# That is not a child-penalty estimate. The desk rejection said that
# literature is crowded and it was right, so this script no longer produces
# an entry in it.
#
# WHY THE SCRIPT HAD TO BE INVERTED, not edited. Until v22 the training
# evidence was sourced at line 5,374 of 6,047: it ran last, after five parts
# of specialisation machinery, and read as an appendix to a wage paper. The
# paper it now supports has training as the spine and the wage penalty as
# two paragraphs of setting. A script whose execution order contradicts its
# paper's argument will eventually be read as the argument, and the ordering
# here is the argument: setting, then investment, then mechanism, then what
# the evidence rules out.
#
# THE PARTS, matching TRAINING_PAPER_OUTLINE.md section 4:
#
#   PART 0  Infrastructure. Logging, deflators, namespace protection, the
#           results registry, the single panel read, the sample map.
#   PART 1  SETTING (paper section 5). The child-penalty event study and
#           the interruption table. NOT results of this paper: they
#           establish that the sample behaves like the samples in the
#           literature, which the reader needs before believing anything
#           about training.
#   PART 2  THE INVESTMENT MARGIN (paper section 6). T1 to T6 and the
#           funding contrast. This is the paper.
#   PART 3  SUBSTITUTION (paper section 7). T9.
#   PART 4  WHAT THIS IS NOT (paper section 8). The A5 beta2 profile
#           demoted to an alternative ruled out, T7 anticipation, T3b
#           training purpose.
#   PART 5  The ledger check and the run record.
#
# -----------------------------------------------------------------------------
# REMOVED AT v23, and where each one went. This follows the v22 precedent
# for Parts G and H: retired to disk with the reason recorded, not deleted,
# because a removal whose reason is not written down gets reinstated.
#
#   All of it is in MASTER_SPECIALISATION_RETIRED.R, 4,598 lines.
#
#   A2 (part), A3, A4    The pre/post comparison, the wage-growth event
#                        study and the breadwinner lock-in. A4 is the
#                        static Becker channel at the person level. A3 is
#                        superseded by the Sun-Abraham event study in
#                        PART 1, which is the one the paper cites.
#
#   PART B (both)        Couple-level analysis, then housework mediation
#                        and the time-budget test. This IS the static
#                        Becker channel: S_ct, theta_1, the
#                        first-difference specification, P1. It answers a
#                        question this paper does not ask.
#
#   PART C               Panel IV on the parental-education instrument.
#                        Diagnostic only even in the wage paper.
#
#   PART D               Couple-level DML and the causal forest, including
#                        the within-couple DML for assortative matching and
#                        the dynamic Becker test. The within-couple work
#                        this paper needs is T6_couple_x_wave, on the
#                        training outcome, and it does it better.
#
#   PART E, PART F       The beta2 robustness battery, Heckman selection,
#                        Oster bounds, age-at-first-birth heterogeneity,
#                        wave-by-wave beta2. All of it prices beta2, which
#                        is now two paragraphs in PART 4.
#
#   SECTION V.E          The within-couple beta2 pre-birth gap, the
#                        Chiappori check. It consumed the Part D couple
#                        objects, so it could not survive Part D leaving,
#                        and T6 covers the matching critique on the
#                        outcome this paper reports.
#
#   SAVE DML RESULTS     Serialised Part D. Nothing to serialise.
#
#   PARTS G and H        Already removed at v22. The FTB-B Reform 1 IV: the
#                        bundled first stage was 1.93 against a
#                        falsification F of 44.79, so the placebo ran the
#                        WRONG WAY. Recorded here because the outline still
#                        lists it as a cut and a reader of this file should
#                        not have to check whether it was ever reinstated.
#
# WHAT IS KEPT AND DEMOTED, rather than cut. A1 and the transition ids
# survive only because A5 and A6 need them; they produce no reported
# number. A5's beta2 profile and A6's interruption table are retained at
# their original code but moved to PART 4 and PART 1 respectively, which is
# where the paper uses them.
#
# THE WAGE PENALTY IS IN THIS SCRIPT ONCE, as setting, and is NOT
# decomposed. The outline's section 7 decides this explicitly: state that
# mothers' wages diverge and that the paper asks what happens to the
# investment behind that divergence, and do not claim the training result
# explains the wage result. The mediation that would establish it has not
# been run and running it reintroduces the Gelbach machinery this rebuild
# removes.
# =============================================================================

# =============================================================================
# PART 0: INFRASTRUCTURE
# =============================================================================

# =============================================================================
# DATA AVAILABILITY
# Raw data: HILDA Survey (Waves 12–24), accessed via the Australian Data Archive
# (ADA). Researchers must apply via: https://dataverse.ada.edu.au/dataverse/hilda
# Data inputs (all built by HILDA_LOADER_UNIFIED.R in a single pass):
#   hilda_panel_data_W12_W24_slim.rds  (Parts A-F; read below)
#   hilda_panel_data_extended.rds      (Part G/H; read by the sourced scripts)
#   hilda_panel_data_training.rds      (training event study fallback)
# (also in this repository). The RDS file itself cannot be shared.
# =============================================================================



# Set working directory 

#setwd("C:/Users/tur277/OneDrive - CSIRO/Desktop/WG")

# ── Run logging: tee all console output to a timestamped file ────────────────
# stdout is split (console + file); messages/warnings (incl. fixest NOTEs)
# go to the file only. Sinks are closed at the very end of this script.
log_dir <- file.path(getwd(), "run_logs")
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
# The name was MASTER_hh_, inherited from within_couples_MASTER_ANALYSIS.R
# which this file was forked from. Every training run has been writing a
# household-named log, which is how a MASTER_training run comes to look like
# a MASTER_hh one when you go back through run_logs/, and is part of why two
# masters could sit side by side without it being obvious which had run.
log_file <- file.path(log_dir,
                      format(Sys.time(), "MASTER_training_%Y%m%d_%H%M%S.log"))
# clear any stale sinks left by a previous aborted run
while (sink.number() > 0) sink()
if (sink.number(type = "message") > 0) sink(type = "message")
.log_con <- file(log_file, open = "wt")
sink(.log_con, split = TRUE)
sink(.log_con, type = "message")
cat("Logging console output to:", log_file, "\n")

# ── source_logged(): source a module so EVERY top-level expression and its
#    auto-printed value reaches the (split) sink, not only its cat() lines.
#    Plain source() does not echo, so sourced modules (Part G, the IV
#    robustness battery, Part H, the event studies) previously wrote only
#    their explicit cat() output to the log and silently dropped any
#    auto-printed tables/summaries. echo + print.eval fixes that; spaced=FALSE
#    keeps the echo compact. Errors are reported but do not abort the run.
source_logged <- function(path, label = basename(path)) {
  cat(sprintf("\n>>> sourcing %s (echoed to log) <<<\n", label))
  # v23. THE SINK STACK IS RESTORED AFTER EVERY MODULE, unconditionally.
  #
  # 03training_designs.R used to open with `while (sink.number() > 0)
  # sink()`, which unwound THIS master's sinks to zero, put its own on, and
  # removed those on exit. Control came back with no sinks at all, so every
  # line from PART 3 onward went nowhere the master's log could see. The
  # master's log file ended at PART 2, at the exact point the module was
  # sourced, and nothing errored - which is why it read as the run having
  # stopped rather than as a logging fault.
  #
  # That module is fixed. This guard is here because the fix lives in a file
  # this one does not control, and because 01discovery_extended.R and
  # 02fte_experience.R still carry the same pattern. A wrapper that restores
  # what it owns is the only place the property can be guaranteed.
  .sk_out <- sink.number(); .sk_msg <- sink.number(type = "message")
  # v24.7. SNAPSHOT THE SEALED HELPERS, and restore them after.
  #
  # CS_event_study.R defines its own gap(w, m) at top level, and it is NOT a
  # duplicate of the contract's gap(a, b, keys): the module's version bins
  # the tails at +/-K and inverse-variance pools within bin, which the
  # contract's does not. Neither can be deleted without changing a result,
  # so neither is wrong - what is wrong is that the module's definition
  # SURVIVES its own source() and shadows the contract's for every later
  # caller. td_check_seals() then stops the run at the very end, after
  # every estimate has been computed.
  #
  # The fix is the same one already applied to `panel` twenty lines below:
  # a module is entitled to use a name while it runs; it is not entitled to
  # change what the caller means by that name afterwards. Restoring keeps
  # both definitions working, each in its own scope.
  .seal_snap <- NULL
  if (exists(".TD_SEALED")) {
    # all.names, because ls() omits names beginning with a dot and .out_path
    # is one. Without it the snapshot silently skips the helper this whole
    # mechanism exists to protect.
    .seal_snap <- mget(intersect(.TD_SEALED,
                                 ls(envir = globalenv(), all.names = TRUE)),
                       envir = globalenv(), ifnotfound = list(NULL))
  }
  tryCatch(
    source(path, echo = TRUE, print.eval = TRUE, max.deparse.length = Inf,
           spaced = FALSE, keep.source = TRUE),
    error = function(e)
      cat(sprintf("  NOTE: %s did not complete (%s); run it standalone.\n",
                  label, conditionMessage(e)))
  )
  # Drop anything the module left ON, then report anything it took OFF. The
  # second case cannot be repaired from here - the connection is the
  # master's and re-sinking it is done by the caller below - so it is
  # surfaced rather than patched over.
  while (sink.number() > .sk_out) sink()
  if (sink.number() < .sk_out) {
    cat(sprintf(paste0("\n  *** %s REMOVED %d of this master's output ",
                       "sink(s).\n      The log is dead from here. Restoring.",
                       "\n"), label, .sk_out - sink.number()))
    while (sink.number() < .sk_out && exists(".log_con")) {
      sink(.log_con, split = TRUE)
      if (sink.number(type = "message") == 0) sink(.log_con, type = "message")
    }
  }
  if (length(.seal_snap)) {
    .chg <- names(.seal_snap)[!vapply(names(.seal_snap), function(n)
      identical(get0(n, envir = globalenv()), .seal_snap[[n]]), logical(1))]
    if (length(.chg)) {
      cat(sprintf(paste0("  %s redefined the shared helper(s): %s. Restoring ",
                         "the\n  contract's version; the module's own ",
                         "definition applied while it ran.\n"),
                  label, paste(.chg, collapse = ", ")))
      for (.n in .chg) assign(.n, .seal_snap[[.n]], envir = globalenv())
      rm(.n)
    }
    rm(.chg)
  }
  rm(.seal_snap)
  cat(sprintf(">>> finished %s <<<\n", label))
  invisible(TRUE)
}

outdir <- file.path(getwd(), "output", "figures")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

suppressPackageStartupMessages({

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
# v26. THE DEFINITION MOVED TO 00_contract.R AND IS SEALED THERE.
#
# Correcting it here changed nothing. training_funding_divergence.R and
# CS_event_study.R each define their own .out_path at top level, both source()d
# above every write this file makes, and a module's top-level definition
# survives its source() and shadows the caller's. So the corrected helper was
# live for exactly the stretch of the run that writes nothing.
#
# One definition, in the file that seals names. Nothing is defined here; the
# contract is sourced below and brings it, and td_check_seals() reports any
# module that replaces it.

# v25. READS GO THROUGH THE SAME HELPER AS WRITES.
#
# The master wrote cs_event_study_results.rds through .out_path() and read it
# back with a bare filename, which resolves to getwd(). On 31 August those
# were WG and TG, so the file was reported ABSENT in the same run that wrote
# it and the wage path fell back to a transcribed copy. verify's check 7 then
# compared that transcription against six literals in its own source, so both
# sides were transcriptions of the same six numbers and the check could not
# fail. A helper that writes to one place and reads from another makes that
# class of bug possible; one helper for both makes it impossible.
#
# .in_path() is not .out_path() renamed. A read has a fallback a write does
# not: a file left beside the script by an older build is still the file the
# user meant, so the working directory is tried second rather than not at
# all. What it will not do is prefer it silently.
.in_path <- function(f) {
  .p <- .out_path(f)
  if (file.exists(.p)) return(.p)
  if (file.exists(f) && !identical(normalizePath(f, mustWork = FALSE),
                                   normalizePath(.p, mustWork = FALSE))) {
    cat("  NOTE:", f, "was found beside the script rather than at\n        ",
        .p, "\n        Reading it, but the two directories disagree.\n")
    return(f)
  }
  .p
}

  library(tidyverse)
  library(fixest)
  library(lmtest)
  library(sandwich)
  library(patchwork)
  library(ggplot2)
})

# Run PACKAGES.R first to install and version-check all dependencies.
for (pkg in c("DoubleML", "mlr3", "mlr3learners", "ranger", "glmnet", "grf")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop(sprintf("Package '%s' not found. Run PACKAGES.R first.", pkg))
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# Thread budget. On a SLURM batch job, use exactly the CPUs allocated
# (SLURM_CPUS_PER_TASK); otherwise (e.g. an interactive login-node session)
# cap at 4 so the script never monopolises a shared node. Override with
# the MASTER_HH_THREADS environment variable if needed.
n_threads <- suppressWarnings(as.integer(Sys.getenv("MASTER_HH_THREADS", "")))
if (is.na(n_threads))
  n_threads <- suppressWarnings(as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "")))
if (is.na(n_threads) || n_threads < 1L)
  n_threads <- min(4L, max(1L, parallel::detectCores(logical = TRUE) - 1L))
fixest::setFixest_nthreads(n_threads)
cat(sprintf("Using %d threads (fixest, ranger, grf)\n", n_threads))

set.seed(42)

# ── WPI deflators (base: Wave 24 = 2024) ─────────────────────────────────────
wpi_data <- data.frame( wave = 12:24, deflator = 149.6 / c(110.9, 114.6, 117.6, 120.4, 123.0, 125.4, 127.9, 130.9, 133.7, 135.7, 138.9, 143.7, 149.6) )


# ── Shared helpers & aesthetics ───────────────────────────────────────────────
stars_fn <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.001, "***", ifelse(p < 0.01, "**",
                                         ifelse(p < 0.05, "*", ifelse(p < 0.10, "\u2020", "")))))
}

label_group <- function(g) {
  case_when(
    g == "childless_men_ever" ~ "Childless men",
    g == "fathers_ever"       ~ "Fathers",
    g == "never_mothers"      ~ "Never-mothers",
    g == "mothers_ever"       ~ "Mothers (ever)",
    TRUE ~ g
  )
}

pal4       <- c("Childless men"="#2166AC","Fathers"="#92C5DE",
                "Never-mothers"="#B2182B","Mothers (ever)"="#F4A582")
pal3w      <- c("Never-mothers"="#B2182B","Active mothers"="#D6604D","Post-mothers"="#FDDBC7")
pal_gender <- c("Men"="#2166AC","Women"="#B2182B")
pal_est    <- c("OLS"="#4393C3","FE"="#D6604D")

theme_paper <- theme_minimal(base_size = 7) +
  theme(
    panel.grid.minor     = element_blank(),
    panel.grid.major.x   = element_blank(),
    legend.position      = "bottom",
    legend.title         = element_blank(),
    plot.title           = element_text(size = 7.5, face = "bold", hjust = 0),
    plot.subtitle        = element_text(size = 6,   colour = "grey40", hjust = 0),
    plot.caption         = element_text(size = 6,   colour = "grey50", hjust = 0),
    strip.text           = element_text(size = 6.5, face = "bold"),
    axis.title           = element_text(size = 6.5)
  )


save_fig <- function(p, name, w = 13/2.54, h = 8/2.54) {
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  ggsave(paste0(name, ".pdf"), p, width = w, height = h, units = "in")
  ggsave(paste0(name, ".png"), p, width = w, height = h, dpi = 500, units = "in")
  for (ext in c(".pdf", ".png")) {
    ff <- paste0(name, ext)
    if (file.exists(ff)) file.copy(ff, file.path(outdir, ff), overwrite = TRUE)
  }
  cat(sprintf("  \u2713 %s\n", name))
}

# ---- contract -------------------------------------------------------
# Namespace protection and the shared helpers (mde, gap, POWER, ALPHA)
# moved to 00_contract.R. Four identical alias blocks were harmless; a
# fifth definition that DIFFERED would not have been, and mde() proved
# it by killing a run 1,300 lines from either copy.
# v24.2. THE GUARD TESTS EVERYTHING IT NEEDS, not one function.
#
# It used to be `if (!exists("td_frame", mode = "function"))`, and that
# killed a run. A session where td_frame was present but mde() was not -
# a restored .RData from before mde moved into the contract is the easy
# way to get there - skipped the source() and then died 300 lines later
# on `could not find function "mde"`, after Block A had already computed
# and printed its whole event-time path.
#
# One name cannot stand for a file. The guard now names every object this
# script takes from the contract, sources when ANY is missing, and stops
# if any is still missing afterwards - which catches a contract that has
# been edited to no longer provide one of them, as opposed to one that
# simply was not loaded.
.TD_NEEDS <- c("td_frame", "td_log_open", "td_log_close", "td_check_seals",
               "td_put", "td_get", "td_has", "mde", "gap")
.td_missing <- .TD_NEEDS[!vapply(.TD_NEEDS, exists, logical(1),
                                 mode = "function")]
if (length(.td_missing) || !exists("POWER") || !exists("ALPHA")) {
  if (!file.exists("00_contract.R"))
    stop("00_contract.R not found. It defines the dplyr aliases, mde(), ",
         "gap(), the handoff registry and td_frame(). Nothing below is ",
         "safe without it.", call. = FALSE)
  source("00_contract.R")
}
.td_still <- .TD_NEEDS[!vapply(.TD_NEEDS, exists, logical(1),
                               mode = "function")]
if (length(.td_still) || !exists("POWER") || !exists("ALPHA"))
  stop("00_contract.R was loaded but does not provide: ",
       paste(c(.td_still, setdiff(c("POWER", "ALPHA"),
                                  ls(envir = globalenv()))),
             collapse = ", "),
       ". A partially-loaded contract fails far from here.", call. = FALSE)
rm(.td_missing, .td_still)

# --- ONE LOG PER RUN, NOT TWO ---------------------------------------------
#
# WHAT WAS HAPPENING. This script opens its own sink at line 150, long before
# the contract is sourced at 404. td_log_open() then found a sink it had no
# record of and read it as a leak: "found 1 open sink(s) above the caller's
# baseline of 0 ... a previous run died without closing its log." It unwound
# to the baseline, which CLOSED this script's own log eight seconds after it
# opened, and then opened a second file. Every run since produced a stub of
# a few hundred bytes and a real log beside it.
#
# WHY THE BASELINE IS 0 RATHER THAN 1. .TD_SINK_BASE is recorded the first
# time the contract loads in a session. Under FINAL_PASS.R that is step 1,
# when no sink is open, so the baseline is 0. This script then opens a sink
# and re-sources the contract, which leaves the baseline alone. So the sink
# is above the baseline and unaccounted for, which is exactly the shape of a
# genuine leak. The rescue was right about what it saw.
#
# THE FIX IS TO ACCOUNT FOR IT. Registering the sink before calling
# td_log_open() means `depth` exists, so the rescue does not fire and the
# nested branch runs instead: td_log_open reports that it inherits this log
# and opens no file. td_log_close() at the foot then takes its `inherited`
# branch, clearing the registry without touching a connection this script
# closes itself three lines earlier.
#
# `con` IS DELIBERATELY NOT REGISTERED. The contract's exit finalizer calls
# td_log_close("stopped") when it finds one, and this script closes
# .log_con itself. Handing the same connection to two owners is how the
# "stopped" banner came to appear on runs that completed.
if (exists(".TD_LOG")) {
  assign("depth",     1L,                    envir = .TD_LOG)
  assign("script",    "MASTER_training.R",   envir = .TD_LOG)
  assign("file",      log_file,              envir = .TD_LOG)
  assign("inherited", TRUE,                  envir = .TD_LOG)
  assign("t0",        Sys.time(),            envir = .TD_LOG)
}
td_log_open("MASTER_training.R")

# Inputs asserted HERE, not assumed 600 lines down. The failure this
# replaces is a NULL travelling until it surfaces as an unrelated error.
td_require("ev", context = "section 9.3 hours arms")

# =====================================================================
# WHERE 03training_designs.R PUT ITS ARTEFACTS
# =====================================================================
# v24.7. RESOLVED ONCE, HERE, because guessing it in two places failed
# twice in one run. The v24.5 skip audit and PART 6's ledger read both did
#
#     if (exists("OUTDIR")) OUTDIR else file.path(project_dir, "training_designs")
#
# and NEITHER variable is defined in this master. Both fell through to "."
# - the working directory, Desktop/WG - where training_designs/ does not
# exist. The audit reported all four artefacts ABSENT and PART 6 printed
# "NOTHING BELOW IS CHECKED", while the files sat intact under
# HILDA_project/training_designs the whole time.
#
# IT CANNOT BE DERIVED FROM THE PANEL PATH. The panel is read from
# Desktop/WG and the artefacts are written under HILDA_project, so the two
# trees are unrelated and no amount of file.path() on one finds the other.
# Hence a search list, and a loud failure when it comes up empty rather
# than a silent fallback that reports absence as fact.
#
# Set TD_OUTDIR before sourcing to override.
if (!exists("TD_OUTDIR")) {
  .od_try <- c(
    if (exists("OUTDIR")) OUTDIR,
    if (exists("project_dir")) file.path(project_dir, "training_designs"),
    file.path("..", "HILDA_project", "training_designs"),
    file.path(path.expand("~"), "..", "Documents - Copy", "Y2025",
              "HILDA_project", "training_designs"),
    file.path("training_designs"),
    file.path("..", "training_designs"))
  .od_hit <- .od_try[dir.exists(.od_try)]
  TD_OUTDIR <- if (length(.od_hit)) normalizePath(.od_hit[1], winslash = "/") else NA_character_
  rm(.od_try, .od_hit)
}
if (is.na(TD_OUTDIR)) {
  cat("\n*** COULD NOT LOCATE training_designs/. PART 2's artefacts cannot be\n")
  cat("    read, so the ledger check and the timing exhibit will report them\n")
  cat("    as absent. That is a PATH problem, not a missing-run problem:\n")
  cat("    set TD_OUTDIR to the directory holding training_ledger.csv and\n")
  cat("    re-run. Do not conclude that the 03 modules have not run.\n")
} else {
  cat(sprintf("\n  artefact directory: %s\n", TD_OUTDIR))
}

# fixest estimation accessors: pin to fixest so a later-attached package
# (DoubleML/mlr3/aod/etc.) cannot mask them. The 13 June crash was a masked
# wald() that dispatched round_any() on a fixest object.
wald <- fixest::wald;          pvalue    <- fixest::pvalue

# ── Results registry ────────────────────────────────────────────────────────
# Each section writes its live-computed headline value here via rec(); the
# FINAL VERIFICATION block prints them via pv() (registry) or gv() (persistent
# objects). Both fall back to "[not computed this run]" so a skipped or failed
# section never aborts the run and never prints a stale literal.
.PR <- new.env(parent = emptyenv())
rec <- function(key, val) { assign(key, val, envir = .PR); invisible(val) }
pv  <- function(key, fmt = "%.4f") {
  if (exists(key, envir = .PR, inherits = FALSE)) {
    v <- get(key, envir = .PR)
    if (length(v) == 0 || all(is.na(v))) return("[not computed this run]")
    if (is.numeric(v)) sprintf(fmt, v[1]) else as.character(v[1])
  } else "[not computed this run]"
}
gv  <- function(expr, fmt = "%.4f") {         # safe read of a persistent object
  v <- tryCatch(expr, error = function(e) NA)
  if (length(v) == 0 || all(is.na(v))) return("[not computed this run]")
  if (is.numeric(v)) sprintf(fmt, v[1]) else as.character(v[1])
}

# ── SAMPLE: Full panel ──────────────────────────────────────────────────────
# Paper §3 (Data): "91,279 person-wave observations across 16,276 individuals"
# Appendix Table: Full panel → counts re-verify against current extract; after
#   age_exp_diff filter (ages 30–49, experience plausibility check).
# ── LOAD DATA (single read) ───────────────────────────────────────────────────
# v21.18a. RESOLVE THE PANEL PATH RATHER THAN ASSUME THE WORKING DIRECTORY.
#
# This was a bare readRDS on a relative path, and it failed with "cannot
# open the connection" because the loader writes to the data directory and
# this script runs from the analysis one. The traceback names gzfile,
# which reads like a corrupt file rather than a missing one, and the
# recovery is to notice the working directory rather than the panel.
#
# Third time the layout has bitten. The deprecation scan missed this file
# for the same reason, and RUN_FIGURES_hh.R needed the same resolver for
# its diagnostics. So it is done here the same way: look in the obvious
# places, say WHICH one was used, and if none has it, list where it looked
# rather than report a connection error.
.panel_file <- "hilda_panel_data_W12_W24_slim.rds"
.panel_try <- c(.panel_file,
                file.path("..", .panel_file),
                file.path("data", .panel_file),
                file.path("..", "data", .panel_file),
                file.path(dirname(getwd()), .panel_file))
if (exists("PANEL_DIR") && nzchar(PANEL_DIR))
  .panel_try <- c(file.path(PANEL_DIR, .panel_file), .panel_try)
.panel_hit <- .panel_try[file.exists(.panel_try)]
# v24. THE FILE SEARCH IS NOW ADVISORY, not a precondition. td_frame()
# resolves from the registry first, so a run where handoff/frame.rds exists
# but the source RDS has moved is perfectly valid - and this block used to
# stop() it. Skip the stop when the registry can answer.
if (!length(.panel_hit) && exists("td_has", mode = "function") && td_has("frame")) {
  cat("Panel file not found on the search path, but handoff/frame.rds exists.\n")
  cat("Resolving from the registry; the source file is not needed.\n")
  .panel_hit <- NA_character_
} else if (!length(.panel_hit)) {
  cat("\n*** THE PANEL WAS NOT FOUND. Looked in:\n")
  for (.p in .panel_try) cat("      ", normalizePath(.p, mustWork = FALSE), "\n")
  cat("    Working directory is ", getwd(), ".\n", sep = "")
  cat("    Set PANEL_DIR before sourcing, or run from the directory the\n")
  cat("    loader writes to. This is a path problem, not a data problem.\n")
  stop("panel file not found; see the paths above.", call. = FALSE)
}
# v24. td_frame(), replacing this master's own readRDS and, below, its own
# copy of the sample rule.
#
# The master ran LAST under RUN_ORDER, so a restriction it applied after its
# own read could not reach the two scripts before it. They resolved from the
# file at 134,131 rows; the master worked on 129,544; and nothing compared
# them because each script agreed with itself. The rule now lives in
# 00_contract.R, is applied on the first resolve, and is persisted to
# handoff/frame.rds so every later process gets the same frame.
if (!is.na(.panel_hit[1]))
  cat(sprintf("Panel file found at: %s\n", normalizePath(.panel_hit[1])))
panel <- td_frame(
  need_cols = c("person_id", "wave", "female", "age", "experience_years"),
  context = "MASTER_training")

# ---------------------------------------------------------------------------
# v21.14h. SOURCE THE COMPAT LAYER AND CHECK WHAT WAS JUST READ.
#
# This script read an .rds and told you nothing about its provenance. That
# is how the figure scripts ended up plotting a workspace predating the v17
# rename for weeks without anyone noticing: nothing on the path from disk to
# estimate said which build wrote the file.
#
# compat_panel_check() states the build tag, stops if the panel carries a
# column name removed upstream, and stops if the tag is outside
# COMPAT_PANEL_TAGS. It costs one line and it is the only thing standing
# between a stale panel and a table of results that looks fine.
#
# Sourced defensively: if the compat file is not beside this one, the run
# continues without the guards rather than failing, and says so, because a
# missing guard should not stop an analysis that was working yesterday.
.compat_path <- c("00_compat_v21.R", "00compat.R",
                  file.path("..", "00_compat_v21.R"))
.compat_path <- .compat_path[file.exists(.compat_path)]
if (length(.compat_path)) {
  source(.compat_path[1])
  compat_panel_check(panel)
} else {
  cat("\nNOTE: no compat file found beside this script, so the restriction\n",
      "  guards are NOT active. The panel's build tag is unverified and\n",
      "  nothing below will stop if a restricted column is used the way\n",
      "  the loader's manifest says it must not be.\n", sep = "")
}
# ---------------------------------------------------------------------------
# PAPER NUMBERS. MASTER_paper_numbers.R registers the numbers this script
# produces or reads, block by block, so the manuscript can quote them as
# \pn{key} instead of typed copies. It records the build tag now, before
# anything below can drop the panel's attributes. .mpn() calls one of its
# functions if the file was found, and reports a failure without stopping
# the run; nothing it does changes an estimate.
.mpn <- function(f, ...) {
  if (!exists(f, mode = "function")) return(invisible(NULL))
  tryCatch(do.call(f, list(...)),
           error = function(e) cat("  *** paper numbers: ", f, " failed: ",
                                   conditionMessage(e), "\n", sep = ""))
}
if (file.exists("MASTER_paper_numbers.R")) {
  source("MASTER_paper_numbers.R")
  .mpn("mpn_begin", panel)
} else {
  cat("\nNOTE: MASTER_paper_numbers.R is not beside this script, so this run\n",
      "  registers no paper numbers.\n", sep = "")
}
# ---------------------------------------------------------------------------
num_rows    <- format(nrow(panel), big.mark = ",")
num_persons <- format(n_distinct(panel$person_id), big.mark = ",")
cat(paste("Loaded:", num_rows, "obs |", num_persons, "individuals\n"))

# Sanity check: xwaveid and person_id should map 1:1
cat("\nID consistency check:\n")
id_check <- panel %>%
  summarise(
    n_xwaveid_unique  = n_distinct(xwaveid),
    n_person_id_unique = n_distinct(person_id),
    n_pairs_unique    = n_distinct(paste(xwaveid, person_id, sep = "|"))
  )
print(id_check)
# All three numbers should be equal if xwaveid and person_id are 1:1.
# In within_couples_MASTER_ANALYSISpro.R, after the panel is loaded but
# BEFORE any analysis that uses group_ever, insert:

cat("\n--- Recomputing group_ever with full-window absorbing definition ---\n")
person_status_full <- panel %>%
  group_by(person_id) %>%
  summarise(
    is_female_p       = max(female == 1, na.rm = TRUE),
    has_child_anywave = max(
      pmax(coalesce(ever_parent, 0L),
           as.integer(coalesce(total_children_ever_had, 0L) > 0),
           na.rm = TRUE),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    is_female_p       = ifelse(is.infinite(is_female_p),       NA_integer_, is_female_p),
    has_child_anywave = ifelse(is.infinite(has_child_anywave), 0L,          has_child_anywave),
    group_ever_full = case_when(
      is_female_p == 0 & has_child_anywave == 0 ~ "childless_men_ever",
      is_female_p == 0 & has_child_anywave == 1 ~ "fathers_ever",
      is_female_p == 1 & has_child_anywave == 0 ~ "never_mothers",
      is_female_p == 1 & has_child_anywave == 1 ~ "mothers_ever",
      TRUE                                     ~ NA_character_
    )
  )

panel <- panel %>%
  select(-any_of("group_ever")) %>%
  left_join(person_status_full %>% select(person_id, group_ever = group_ever_full),
            by = "person_id")

cat("  group_ever coverage by wave (should be ~100% all 13 waves):\n")
panel %>%
  group_by(wave) %>%
  summarise(pct = round(100 * mean(!is.na(group_ever)), 1)) %>%
  print(n = Inf)


# ============================================================
#  recompute group_3way_women from new group_ever
# ============================================================
# THE RECOMPUTATION ABOVE IS RIGHT AND THIS SPLIT INHERITED A HOLE FROM IT.
# group_ever is now person-invariant, which is the fix: a woman who has a child
# anywhere in the window is mothers_ever at every wave, including her pre-birth
# ones. Good. But a pre-birth wave has num_children_under15 == 0, so the rule
# below sent it to post_mothers, and post-mothers are the oldest and most
# experienced women in the panel while she is among the youngest.
#
# The size of it is not small. On the 4 September panel 6,245 person-waves
# across 1,423 women sit before a first birth, against a post_mothers level of
# 13,733, so roughly three in ten of that column were women who had not yet had
# a child. It is also the column the paper leans on hardest: Section 4.5's
# whole argument is that a current-children measure misclassifies post-mothers
# as childless, and it is made by pointing at this category.
#
# So the split has four levels. ever_parent is the discriminator, because it is
# a status AT a wave and is 0 before a first birth and 1 after: within
# mothers_ever it separates pre from post exactly. Collapse pre_mothers into
# never_mothers and you get the old wave-varying column; collapse it into
# active_mothers and you get a lifetime binary; leave it out and it lands in
# whichever neighbour the case_when reaches first, which is what happened.
panel <- panel %>%
  mutate(
    group_3way_women = case_when(
      female != 1                                       ~ NA_character_,
      group_ever == "never_mothers"                     ~ "never_mothers",
      group_ever == "mothers_ever" &
        coalesce(ever_parent, 0L) == 0                  ~ "pre_mothers",
      group_ever == "mothers_ever" &
        coalesce(num_children_under15, 0L) > 0          ~ "active_mothers",
      group_ever == "mothers_ever" &
        coalesce(num_children_under15, 0L) == 0         ~ "post_mothers",
      TRUE                                              ~ NA_character_
    )
  )

cat("\n  group_3way_women distribution (women only, four levels):\n")
print(table(panel$group_3way_women, useNA = "ifany"))
cat("  never_mothers and pre_mothers are both women without a child at this\n")
cat("  wave; the first never has one and the second does later. A comparison\n")
cat("  group meant to exclude the treated wants never_mothers alone.\n")


panel <- panel %>%
  mutate(
    married_num       = as.numeric(married),
    fulltime_num      = as.numeric(fulltime),
    public_sector_num = as.numeric(public_sector),
    wave_num          = as.numeric(wave),
    year              = 2000 + wave,
    age_exp_diff      = age - experience_years
  )
# THE filter(between(age_exp_diff, 10, 35)) THAT WAS HERE HAS MOVED to
# TD_SAMPLE_RULE in 00_contract.R. td_frame() applied it above, before this
# mutate ran, so age_exp_diff is computed here only because downstream blocks
# report on it. Re-applying the filter would be a no-op today and a
# divergence the moment the rule changes in one place and not the other.

# =============================================================================
# THE SAMPLE CONTRACT
# =============================================================================
# THE FILTER ABOVE IS THE ONLY PLACE THIS RESTRICTION EXISTS. The loader
# applies no age or experience screen, so hilda_panel_data_W12_W24_slim.rds
# and hilda_panel_data_training.rds both carry the full 134,131 rows and
# 20,205 persons. This master alone cuts to 129,544 and 19,661.
#
# WHICH MEANS EVERY MODULE THAT READS A FILE INSTEAD OF `panel` IS ON A
# DIFFERENT SAMPLE, and the numbers still land in one ledger table.
#
# options(tfd_frame=) fixed that for one script, but it fixed it by asking
# the analyst to read a log line every run and confirm the option took. That
# is a vigilance tax and vigilance taxes are paid until the one run where
# they are not. The contract below removes the need to look:
#
#   - the master DECLARES its sample fingerprint here, once
#   - every module ASSERTS against it after resolving its frame
#   - a mismatch STOPS the run instead of printing a note
#
# A module that legitimately wants the unfiltered panel - the training event
# study reads the slim training panel by design - passes expect = "any" and
# says so in its own log. The point is not that one frame is correct. It is
# that the choice is DECLARED at the point of use rather than inferred from
# a row count three thousand lines away.
#
# WHY NOT JUST MOVE THE FILTER INTO THE LOADER, which would delete the
# problem rather than police it: because it is an analysis restriction and
# not a data-quality screen. age - experience_years outside 10 to 35 is an
# implausible pairing for THIS design's purposes; another design on the same
# panel may reasonably keep those rows. Pushing it upstream would silently
# impose one paper's sample on every consumer of the loader, which is the
# same class of error in the other direction. If it is ever agreed that all
# designs want it, move it and delete this block.
# v24. The fingerprint now RESTATES what td_frame() resolved rather than
# defining it. The filter string is read from the contract so the two cannot
# drift: a fingerprint that names a rule the resolver does not apply is worse
# than no fingerprint, because downstream asserts would pass against it.
SAMPLE_FINGERPRINT <- list(
  rows    = nrow(panel),
  persons = dplyr::n_distinct(panel$person_id),
  waves   = paste(range(panel$wave), collapse = "-"),
  filter  = if (exists("TD_SAMPLE_RULE")) TD_SAMPLE_RULE else "UNKNOWN")
options(analysis_sample_fingerprint = SAMPLE_FINGERPRINT)
cat(sprintf("\n  SAMPLE CONTRACT declared: %s rows, %s persons, waves %s.\n",
            format(SAMPLE_FINGERPRINT$rows, big.mark = ","),
            format(SAMPLE_FINGERPRINT$persons, big.mark = ","),
            SAMPLE_FINGERPRINT$waves))

# Guarded by exists() at every call site, so a module still runs standalone.
assert_analysis_sample <- function(df, context, expect = "match") {
  fp <- getOption("analysis_sample_fingerprint")
  if (is.null(fp)) {
    cat(sprintf("  [sample] %s: no fingerprint declared (standalone run).\n",
                context))
    return(invisible(FALSE))
  }
  n <- nrow(df); p <- dplyr::n_distinct(df$person_id)
  if (identical(expect, "any")) {
    cat(sprintf(paste0("  [sample] %s: %s rows / %s persons, DECLARED as not ",
                       "the analysis sample (%s / %s). Its estimates do not ",
                       "belong in a table with the rest.\n"),
                context, format(n, big.mark = ","), format(p, big.mark = ","),
                format(fp$rows, big.mark = ","),
                format(fp$persons, big.mark = ",")))
    return(invisible(TRUE))
  }
  if (n != fp$rows || p != fp$persons)
    stop(sprintf(paste0("SAMPLE CONTRACT VIOLATED in %s.\n",
                        "  this frame     : %s rows, %s persons\n",
                        "  analysis sample: %s rows, %s persons\n",
                        "  filter         : %s\n",
                        "  The module read a file instead of the master's ",
                        "`panel`, so its estimates are not on the run's ",
                        "sample. Hand it the frame, or pass expect = \"any\" ",
                        "and keep its numbers out of the shared tables."),
                 context, format(n, big.mark = ","), format(p, big.mark = ","),
                 format(fp$rows, big.mark = ","),
                 format(fp$persons, big.mark = ","), fp$filter),
         call. = FALSE)
  cat(sprintf("  [sample] %s: matches the analysis sample.\n", context))
  invisible(TRUE)
}

# Resolve housework column — check which name exists in the data
.hw_candidates <- c("housework_hours", "care_intensity", "hsjob", "jomfh", "hw_hours")
.hw_col <- .hw_candidates[.hw_candidates %in% names(panel)][1]
if (!is.na(.hw_col)) {
  panel$hw <- pmin(pmax(panel[[.hw_col]], 0), 100)
  cat(sprintf("  hw resolved from: '%s'  (non-NA: %d%%)\n",
              .hw_col, round(100 * mean(!is.na(panel$hw)))))
} else {
  panel$hw <- NA_real_
  cat("  WARNING: no housework column found — hw is NA throughout\n")
  cat(sprintf("  Available columns: %s\n", paste(names(panel)[1:20], collapse = ", ")))
}
panel$total_hours <- ifelse(
  is.na(panel$hours_worked_clean) | is.na(panel$hw), NA_real_,
  panel$hours_worked_clean + panel$hw
)

# partner_id_clean: clean HILDA partner ID, available globally for all couple sections.
# Two-step: check existence outside mutate, then run the clean mutate unconditionally.
if (!"partner_id" %in% names(panel)) panel$partner_id <- NA_character_
panel <- panel %>%
  mutate(
    partner_id_clean = dplyr::case_when(
      is.na(partner_id)       ~ NA_character_,
      partner_id == ""        ~ NA_character_,
      nchar(partner_id) < 5  ~ NA_character_,
      grepl("^-", partner_id) ~ NA_character_,
      TRUE                    ~ partner_id
    )
  )

# ── SAMPLE: Wage sample (main analysis) ────────────────────────────────────
# Paper §3: "wage sample (employed individuals with valid hourly wages),
#   comprising 87,229 observations across 15,668 individuals"
# Filter: in_wage_sample == 1 (employed, valid hourly wage, trimmed)
wage <- panel %>% filter(in_wage_sample == 1)

num_rows    <- format(nrow(wage), big.mark = ",")
num_persons <- format(n_distinct(wage$person_id), big.mark = ",")
cat(paste("Wage sample:", num_rows, "obs |", num_persons, "individuals\n"))

# ── SAMPLE MAP VERIFICATION ─────────────────────────────────────────────────
# Prints actual sample sizes vs the expected counts in Appendix Table
# "Sample Definitions by Analysis" (Table \ref{tab:sample_map}).
# This output is used to populate that table; differences indicate
# data-version or filter changes.

cat("\n")
cat(strrep("=", 80), "\n")
cat("  SAMPLE MAP VERIFICATION (Appendix Table: Sample Definitions)\n")
cat(strrep("=", 80), "\n\n")

cat(sprintf("  %-45s  %10s  %10s\n", "Sample", "Obs", "Persons"))
cat("  ", strrep("-", 70), "\n")

cat(sprintf("  %-45s  %10s  %10s\n",
            paste0("Full panel (rule: ",
                   get0("TD_SAMPLE_RULE", ifnotfound = "unknown"), ")"),
            format(nrow(panel), big.mark = ","),
            format(n_distinct(panel$person_id), big.mark = ",")))

cat(sprintf("  %-45s  %10s  %10s\n",
            "Wage sample (in_wage_sample == 1)",
            format(nrow(wage), big.mark = ","),
            format(n_distinct(wage$person_id), big.mark = ",")))

# Couple panel count (computed here, used later)
n_with_partner <- sum(!is.na(panel$partner_id_clean) &
                        panel$partner_id_clean %in% panel$person_id)
cat(sprintf("  %-45s  %10s  %10s\n",
            "Obs with valid partner in panel",
            format(n_with_partner, big.mark = ","),
            "(see Part B)"))

# Wage sample by group
for (g in c("childless_men_ever", "fathers_ever", "never_mothers", "mothers_ever")) {
  d <- wage %>% filter(group_ever == g)
  cat(sprintf("  %-45s  %10s  %10s\n",
              paste0("  Wage: ", label_group(g)),
              format(nrow(d), big.mark = ","),
              format(n_distinct(d$person_id), big.mark = ",")))
}

# Three-way women split
for (g in c("never_mothers", "pre_mothers", "active_mothers", "post_mothers")) {
  d <- wage %>% filter(group_3way_women == g)
  if (nrow(d) > 0)
    cat(sprintf("  %-45s  %10s  %10s\n",
                paste0("  Wage 3-way: ", g),
                format(nrow(d), big.mark = ","),
                format(n_distinct(d$person_id), big.mark = ",")))
}

# Transition counts
men_trans_n <- panel %>%
  filter(female == 0) %>%
  group_by(person_id) %>%
  summarise(trans = any(diff(ever_parent) > 0), .groups = "drop") %>%
  filter(trans) %>% nrow()
women_trans_n <- panel %>%
  filter(female == 1) %>%
  group_by(person_id) %>%
  summarise(trans = any(diff(ever_parent) > 0), .groups = "drop") %>%
  filter(trans) %>% nrow()
cat(sprintf("  %-45s  %10s  %10s\n",
            "Fatherhood transitions",
            "—", format(men_trans_n, big.mark = ",")))
cat(sprintf("  %-45s  %10s  %10s\n",
            "Motherhood transitions",
            "—", format(women_trans_n, big.mark = ",")))

# IV sample (if parental education available)
if ("father_educ_years" %in% names(panel) || "mother_educ_years" %in% names(panel)) {
  iv_samp <- wage %>%
    mutate(parent_educ_avg = case_when(
      !is.na(father_educ_years) & !is.na(mother_educ_years) ~
        (father_educ_years + mother_educ_years) / 2,
      !is.na(father_educ_years) ~ father_educ_years,
      !is.na(mother_educ_years) ~ mother_educ_years,
      TRUE ~ NA_real_
    )) %>%
    filter(!is.na(parent_educ_avg))
  cat(sprintf("  %-45s  %10s  %10s\n",
              "IV sample (parental educ available)",
              format(nrow(iv_samp), big.mark = ","),
              format(n_distinct(iv_samp$person_id), big.mark = ",")))
}

# Robustness samples
cat(sprintf("  %-45s  %10s\n", "Robustness: age 25–54",
            format(nrow(panel %>% filter(age >= 25, age <= 54)), big.mark = ",")))
cat(sprintf("  %-45s  %10s\n", "Robustness: full-time only",
            format(nrow(panel %>% filter(fulltime == 1)), big.mark = ",")))
cat(sprintf("  %-45s  %10s\n", "Robustness: part-time only",
            format(nrow(panel %>% filter(fulltime == 0, employed == 1)), big.mark = ",")))

if ("longitudinal_weight" %in% names(panel)) {
  lw <- panel %>% filter(!is.na(longitudinal_weight), longitudinal_weight > 0)
  cat(sprintf("  %-45s  %10s  %10s\n",
              "Robustness: longitudinal weights",
              format(nrow(lw), big.mark = ","),
              format(n_distinct(lw$person_id), big.mark = ",")))
}

# Trimming variants
trim19 <- wage %>%
  group_by(female) %>%
  mutate(p01 = quantile(hourly_wage_clean, 0.01, na.rm = TRUE),
         p99 = quantile(hourly_wage_clean, 0.99, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(hourly_wage_clean >= p01, hourly_wage_clean <= p99)
cat(sprintf("  %-45s  %10s\n", "Robustness: trim 1/99",
            format(nrow(trim19), big.mark = ",")))

notrim <- panel %>%
  filter(employed == 1, !is.na(hourly_wage_clean), hourly_wage_clean > 0)
cat(sprintf("  %-45s  %10s\n", "Robustness: no trim",
            format(nrow(notrim), big.mark = ",")))

cat("\n  NOTE: Use these counts to populate Appendix Table \\ref{tab:sample_map}.\n")
cat("  Differences from hardcoded table values indicate data-version changes.\n")
cat(strrep("=", 80), "\n\n")


################################################################################

# =============================================================================
# PART 1: WHAT HAPPENS AT FIRST BIRTH  (paper section 5)
# =============================================================================
# EMPLOYMENT, HOURS AND THE INTERRUPTION TABLE. What happens to mothers'
# employment, which is the background the training result sits against.
#
# THE WAGE PENALTY IS NO LONGER HERE. The updated outline promotes it to a
# result in PART 4, because the timing comparison needs the wage path and the
# training path on the same axis and the exhibit is section 8's, not section
# 5's. What stays in section 5 is the labour-supply background: hours, full-
# time status and the interruption around the transition.
#
# A1 and the transition ids below produce no reported number. They survive
# only because A6's interruption table and PART 4's beta2 profile need them,
# and they are here rather than in PART 4 so the objects exist before either
# consumer runs.


# ---- Panel size, echoed once ------------------------------------------------
# Carried out of the retired PART A banner. The banner itself announced
# 'FATHERHOOD TRANSITION - BREADWINNER LOCK-IN', which is the other paper's
# framing and would have been the first thing a reader of the log saw.
cat("Loading data...\n")
num_rows    <- format(nrow(panel), big.mark = ",")
num_persons <- format(n_distinct(panel$person_id), big.mark = ",")
cat(paste(num_rows, "obs |", num_persons, "individuals\n"))
# A1. IDENTIFY FATHERHOOD TRANSITIONS DURING PANEL

cat("\n--- A1. Fatherhood transitions during panel ---\n")

# Men who start as childless and become fathers during the panel
men_transitions <- panel %>%
  filter(female == 0) %>%
  group_by(person_id) %>%
  arrange(wave) %>%
  summarise(
    first_wave = first(wave),
    last_wave = last(wave),
    n_waves = n(),
    ever_parent_first = first(ever_parent),
    ever_parent_last = last(ever_parent),
    transition_wave = if (any(diff(ever_parent) > 0))
      wave[which(diff(ever_parent) > 0)[1] + 1] else NA_integer_,
    .groups = "drop"
  ) %>%
  mutate(
    transitioner = !is.na(transition_wave),
    always_childless = ever_parent_first == 0 & ever_parent_last == 0,
    always_father = ever_parent_first == 1 & ever_parent_last == 1
  )

cat(sprintf("  Total men: %s\n", format(nrow(men_transitions), big.mark = ",")))
cat(sprintf("  Transitioners (childless→father): %d (%.1f%%)\n",
            sum(men_transitions$transitioner),
            100 * mean(men_transitions$transitioner)))
cat(sprintf("  Always childless: %d\n", sum(men_transitions$always_childless)))
cat(sprintf("  Always father: %d\n", sum(men_transitions$always_father)))

# Same for women
women_transitions <- panel %>%
  filter(female == 1) %>%
  group_by(person_id) %>%
  arrange(wave) %>%
  summarise(
    first_wave = first(wave),
    last_wave = last(wave),
    n_waves = n(),
    ever_parent_first = first(ever_parent),
    ever_parent_last = last(ever_parent),
    transition_wave = if (any(diff(ever_parent) > 0))
      wave[which(diff(ever_parent) > 0)[1] + 1] else NA_integer_,
    .groups = "drop"
  ) %>%
  mutate(
    transitioner = !is.na(transition_wave),
    always_childless = ever_parent_first == 0 & ever_parent_last == 0,
    always_mother = ever_parent_first == 1 & ever_parent_last == 1
  )

cat(sprintf("\n  Total women: %s\n", format(nrow(women_transitions), big.mark = ",")))
cat(sprintf("  Transitioners (childless→mother): %d (%.1f%%)\n",
            sum(women_transitions$transitioner),
            100 * mean(women_transitions$transitioner)))

# v22 ledger. Written at the point of computation so the reconciliation
# cannot be defeated by a later reassignment of a common object name.
.tcol <- if ("is_transitioner" %in% names(men_transitions)) {
  "is_transitioner"
} else if ("transitioner" %in% names(men_transitions)) {
  "transitioner"
} else NA_character_
if (!is.na(.tcol)) {
  rec("fatherhood_n", sum(men_transitions[[.tcol]],   na.rm = TRUE))
  rec("motherhood_n", sum(women_transitions[[.tcol]], na.rm = TRUE))
}
rec("panel_rows",    nrow(panel))
rec("panel_persons", dplyr::n_distinct(panel$person_id))

# ---- The transition ids, carried out of the retired A2 ----------------------
# These two lines are ALL that A6 needs from A2. The rest of A2 - the
# pre/post wage comparison and the period-coded frames it builds - is
# retired, so taking a line range would have carried 190 lines of the
# specialisation paper to obtain two vectors, and an earlier attempt at
# exactly that cut mid-expression and would not parse.
men_trans_ids <- men_transitions %>% filter(transitioner) %>% pull(person_id)
women_trans_ids <- women_transitions %>% filter(transitioner) %>% pull(person_id)

# ---- The wage event study has MOVED to PART 4 -------------------------------
# It was here when the plan made it scene-setting. The updated outline makes
# it a RESULT: the timing comparison needs the wage path and the training path
# on one axis, so it has to run AFTER PART 2 produces the training path.
# Section 5 is now employment, hours and the interruption table only.


# ---- The interruption table (was A6) ----------------------------------------
# What happens to mothers' employment, which is the background the training
# result sits against. Same role as the event study above: setting.
# A6. HOURS AND FULLTIME STATUS AROUND TRANSITION

cat("\n\n--- A6. Hours and fulltime status around parenthood transition ---\n")

# Men: do hours increase after fatherhood?
cat("\n  MEN — Hours change around fatherhood (broad sample):\n")
panel %>%
  filter(female == 0, person_id %in% men_trans_ids) %>%
  left_join(men_transitions %>% select(person_id, transition_wave), by = "person_id") %>%
  mutate(period = ifelse(wave < transition_wave, "Pre", "Post")) %>%
  group_by(period) %>%
  summarise(
    N = n(),
    Mean_hours = round(mean(hours_worked_clean, na.rm = TRUE), 1),
    Pct_employed = round(100 * mean(employed == 1, na.rm = TRUE), 1),
    Pct_fulltime = round(100 * mean(fulltime == 1, na.rm = TRUE), 1),
    Mean_ln_wage = round(mean(ln_hourly_wage_real, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  print()

# Women: do hours decrease after motherhood?
cat("\n  WOMEN — Hours change around motherhood (broad sample):\n")
panel %>%
  filter(female == 1, person_id %in% women_trans_ids) %>%
  left_join(women_transitions %>% select(person_id, transition_wave), by = "person_id") %>%
  mutate(period = ifelse(wave < transition_wave, "Pre", "Post")) %>%
  group_by(period) %>%
  summarise(
    N = n(),
    Mean_hours = round(mean(hours_worked_clean, na.rm = TRUE), 1),
    Pct_employed = round(100 * mean(employed == 1, na.rm = TRUE), 1),
    Pct_fulltime = round(100 * mean(fulltime == 1, na.rm = TRUE), 1),
    Mean_ln_wage = round(mean(ln_hourly_wage_real, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  print()

# v30. THE PART A CHECKPOINT IS REMOVED.
#
# WHAT IT WAS PROTECTING. "PART A" is a banner that no longer exists: it was
# retired at the note above the panel-size echo, and what precedes this line
# is a descriptive summary table. A 170 MB write was insuring a group_by and
# a print.
#
# WHY IT WAS WORSE THAN NOTHING. save.image() writes the WHOLE global
# environment, `panel` included. A session restored from it brings back a
# frame of unknown width and provenance and walks straight past td_frame()'s
# guard, which is the guard that refused a narrow frame on 13 September and
# named the missing columns. On 10 September a ten-column `panel` left in a
# session produced three stale figures, twelve \tofill macros, a two-row
# descriptives table and an empty app_b2_profile, and every one of those read
# as a content fault. A .RData that restores that state on demand is the same
# hazard with a save button.
#
# THREE SMALLER COSTS. 170 MB rewritten into a synced folder on every run,
# when a OneDrive lock has already killed a stage mid-pass; a file outside
# every DIR_*, so Check 11 cannot see it and FINAL_PASS.R's vintage scan does
# not cover .RData; and no record of which scripts produced it.
#
# WHAT REPLACES IT. Nothing, because nothing read it. Where a later script
# needs an object, td_put() persists that object by name with provenance and
# td_require() asserts it, which is the mechanism the contract was built
# around. Delete master_state_after_partA.RData from the project root; it is
# not an input to anything.
cat("  PART A summary complete. No session checkpoint is written: see the\n")
cat("  note above. Objects a later script needs are handed over by\n")
cat("  td_put(), one at a time and with provenance.\n")

# #############################################################################

# =============================================================================
# PART 2: THE INVESTMENT MARGIN  (paper section 6)
# =============================================================================
# THIS IS THE PAPER. Everything above is setting and everything below is
# mechanism or elimination.
#
# WHY IT IS ANSWERABLE HERE AND RARELY ELSEWHERE. Most panels record whether
# training happened. HILDA records WHO PAID FOR IT: training in the
# respondent's own time is separated from training at the employer's place
# of work during working hours. That split is what distinguishes "mothers
# invest less" from "employers invest less in mothers", and without it the
# two are observationally identical.
#
# 03training_designs.R carries T1 to T7 and T9. The order inside it is T1-T7,
# then T9, then T8, because T9 can overturn a sentence the earlier designs
# would otherwise support. That ordering is deliberate and is not changed
# here.
#
# The modules read `panel` from memory and fall back to the RDS. They depend
# on NOTHING from the retired parts, which is what made this rebuild a
# reordering rather than a rewrite.


# ---- T8, and why it is gated -----------------------------------------------
# THE v21.39 RUN NEVER FINISHED. The log ends mid-T8a on "Growing trees..
# Progress: 85%". Nothing had failed: T8 is twelve cross-fitted random-forest
# fits and 03training_designs.R's own header calls it the only slow section.
# But the master is sequential, so PART 2 did not return, and FOUR THINGS
# THAT HAD NOTHING TO DO WITH T8 never ran: the training event study, the
# funding contrast and dosage, the wage event study, and both new blocks in
# PARTS 4 and 5.
#
# That is the wrong shape for this paper. The outline makes the timing figure
# step 3 and says to build it EARLY, because seeing both paths on one axis is
# how you find out whether the ordering is clean before writing section 8 to
# it. Burying it behind twelve forests inverts that.
#
# T8 FEEDS ONE THING ONLY: section 8.3's claim that the gender difference in
# the return to experience is not distinguishable from zero across ten arms
# and five treatment constructions. Nothing else in the paper depends on it,
# so it is the right thing to gate.
#
# SET MASTER_RUN_T8 <- TRUE FOR THE RUN THAT GETS REPORTED. FALSE is for
# iteration: everything except 8.3 completes in a couple of minutes.
if (!exists("MASTER_RUN_T8")) MASTER_RUN_T8 <- FALSE

# THE OVERRIDE IS LIVE as of the v23 edit to 03training_designs.R, which
# changed a bare `RUN_T8 <- TRUE` to
#
#     if (!exists("RUN_T8")) RUN_T8 <- TRUE
#
# leaving the default untouched: a plain source() of that file still gets
# TRUE. The check below is KEPT anyway. It costs nothing, and if the module
# is ever replaced by a copy without the patch the symptom is a forty-minute
# run that looks like a hang, which is the least diagnosable failure this
# pipeline can produce.
RUN_T8 <- MASTER_RUN_T8
.t8_requested <- MASTER_RUN_T8

# ---- unblinding, as an event rather than a default -------------------------
# 03training_designs.R is a DESIGN AUDIT and blinds by default: point
# estimates go to BLIND_training_estimates.rds and the log carries SE, MDE
# and a verdict. That is what makes its PREFERRED / secondary / diagnostic
# statuses worth anything, because they were fixed without anyone seeing a
# coefficient.
#
# The master needs the numbers, so it unblinds. That is legitimate and it is
# ALSO the moment the module stops being an audit and starts being an
# estimator, which is exactly the transition worth recording. Hence a stated
# flag and a printed line rather than a quiet default.
#
# DO NOT SET THIS TRUE AND THEN EDIT A DESIGN. Adding or changing an arm
# after seeing its coefficient is what the blind exists to prevent, and the
# module cannot tell the difference. T4c is the one arm added after
# unblinding and it is marked post-hoc at its own site for that reason.
if (!exists("MASTER_UNBLIND")) MASTER_UNBLIND <- TRUE
BLIND <- !MASTER_UNBLIND
if (MASTER_UNBLIND) {
  cat("  UNBLINDING the 03 modules (BLIND <- FALSE). Statuses in their\n")
  cat("  ledger were designated BEFORE any coefficient was seen and are not\n")
  cat("  revisable on the strength of what this run prints.\n")
}

# =====================================================================
# PART 2 IS SKIPPED BY DEFAULT AT v24.5
# =====================================================================
# WHY, and it is not a shortcut. 03training_designs.R completed a full pass
# on this panel on 18 August, 86,373 seconds, all twenty T8 combinations, no
# errors. Its artefacts are current and are what the paper now cites:
#
#   training_decisions.txt         24 decisions, seven of them T8
#   training_ledger.csv            33 tags with SE, MDE and status
#   BLIND_training_estimates.rds   the sealed point estimates
#   T2_profile_by_event_time.csv   the training path PART 4 reads
#   handoff/ev.rds                 the event frame section 9.3 needs
#
# RE-SOURCING IT WOULD DESTROY THREE OF THOSE. MASTER_RUN_T8 defaults to
# FALSE, so a re-run rewrites the decisions file with SEVENTEEN entries
# instead of twenty-four, drops the T8 rows from the ledger, and rewrites the
# estimates rds without them - while the T8_*.csv files survive untouched and
# silently stop matching the ledger beside them. Section 8.3 of the paper
# rests on the divergence-gate findings in exactly those seven decisions.
#
# NOTHING DOWNSTREAM BREAKS. Every consumer reads from disk or the registry
# rather than from memory:
#
#   td_require("ev") at the head of this file resolves handoff/ev.rds, which
#     03 persisted on its way out (td_put persists by default)
#   PART 4 reads T2_profile_by_event_time.csv
#   PART 6 reads training_ledger.csv and BLIND_training_estimates.rds
#
# The one exception is handled below: section 9.3's cross-check reads the
# in-memory .blind list, which will not exist. It falls back to reporting
# that T4c is unavailable, and T4c is in the ledger PART 6 prints anyway.
#
# SET MASTER_RUN_PART2 <- TRUE TO RE-RUN THE DESIGNS. Do that only when the
# panel has changed, and set MASTER_RUN_T8 <- TRUE at the same time, or the
# T8 material is lost for a saving of twenty-four hours you did not need.
if (!exists("MASTER_RUN_PART2")) MASTER_RUN_PART2 <- FALSE

if (!MASTER_RUN_PART2) {
  cat("\n  PART 2 SKIPPED. The 03 modules are not re-sourced.\n")
  cat("  Its artefacts from the completed run are used as they stand:\n")
  .p2_dir <- if (!is.na(TD_OUTDIR)) TD_OUTDIR else "."
  for (.f in c("training_decisions.txt", "training_ledger.csv",
               "BLIND_training_estimates.rds")) {
    .p <- file.path(.p2_dir, .f)
    cat(sprintf("    %-32s %s\n", .f,
                if (file.exists(.p))
                  format(file.info(.p)$mtime, "%Y-%m-%d %H:%M") else "*** ABSENT"))
  }
  # The decisions count is the fastest check that the artefacts are the T8
  # ones. Seventeen means a T8-less pass has overwritten them.
  .p2_dec <- file.path(.p2_dir, "training_decisions.txt")
  if (file.exists(.p2_dec)) {
    .n_dec <- length(readLines(.p2_dec, warn = FALSE))
    cat(sprintf("    decisions recorded: %d\n", .n_dec))
    if (.n_dec < 20)
      cat("    *** FEWER THAN TWENTY. The T8 decisions are missing, so these\n",
          "        artefacts are from a pass with T8 off and section 8.3 has\n",
          "        nothing behind it. Restore the backup or re-run 03 with\n",
          "        MASTER_RUN_T8 <- TRUE.\n", sep = "")
    rm(.n_dec)
  }
  rm(.p2_dir, .p2_dec, .f, .p)
} else if (any(file.exists(c("03a_design_audit.R", "03b_fee_descriptives.R",
                             "03c_experience_units.R")))) {
  
  # The order is fixed and is not alphabetical by accident: 03a pulls in
  # 03_common.R, which resolves the frame through td_frame() and computes
  # the pre-birth base rates every later module grades its MDE against.
  # 03c is last because it is the expensive one and because nothing else
  # depends on it.
  .td03 <- c("03a_design_audit.R"     = "T1-T7, T9, T12: the design audit",
             "03b_fee_descriptives.R" = "T3, T3b, T3c: the funding contrast",
             "03c_experience_units.R" = "T8, T8a: the return-to-experience arms")
  
  # T8 is a module now, so the gate decides whether it runs at all.
  if (!MASTER_RUN_T8) .td03 <- .td03[names(.td03) != "03c_experience_units.R"]
  
  .missing <- setdiff(names(.td03), list.files())
  if (length(.missing)) {
    cat("\n  *** MODULE(S) NOT IN ", getwd(), ":\n", sep = "")
    for (.m in .missing) cat("        ", .m, "\n", sep = "")
    cat("      They are skipped. Whatever they carry is NOT in this log.\n")
    .td03 <- .td03[!names(.td03) %in% .missing]
  }
  
  if (!length(.td03))
    cat("\n  *** NO MODULE LEFT TO SOURCE. Either every module is missing or\n",
        "      03c was the only one present and MASTER_RUN_T8 is FALSE.\n",
        "      T1-T9 are NOT in this log.\n", sep = "")
  cat(sprintf("\n  PART 2: sourcing %d module(s). T8 (DML swap): requested %s.\n",
              length(.td03), if (MASTER_RUN_T8) "ON" else "OFF (iteration pass)"))
  
  .blind_requested <- BLIND
  
  # HAND OVER THE FRAME, and keep a copy anyway.
  #
  # The module's own `panel <- readRDS(PANEL)` used to overwrite this
  # master's filtered panel unconditionally, so T1-T9 ran on 134,131 rows
  # while the master believed 129,544, AND every block after PART 2
  # inherited the replacement. options(td_frame = "panel") stops that at
  # the source: td_frame() in 00_contract.R reads the named object rather
  # than the file.
  #
  # The save and restore is belt and braces and it stays. A module is
  # entitled to use `panel` as a working name; it is not entitled to
  # change what the master means by it. Restoring afterwards makes that
  # true regardless of what any future revision does with the variable.
  # The option is set ONCE around the whole sequence rather than per
  # module, because a module that resets it would otherwise be masked by
  # the next module setting it again.
  options(td_frame = "panel")
  .panel_before <- panel
  .sink_before  <- sink.number()
  
  for (.m in names(.td03)) {
    .n_in    <- nrow(panel)
    .sink_in <- sink.number()
    
    source_logged(.m, .td03[[.m]])
    
    # Sink depth, checked per module. An unbalanced close does not error;
    # it silently redirects everything after it.
    .sink_out <- sink.number()
    if (!identical(.sink_out, .sink_in))
      cat(sprintf(paste0("\n  *** %s LEFT THE SINK UNBALANCED (%d in, %d ",
                         "out).\n      Output after this point may be going ",
                         "to that module's\n      transcript rather than to ",
                         "this one. Check its td_log_close().\n"),
                  .m, .sink_in, .sink_out))
    
    # The frame, checked per module, so a replacement names its culprit.
    if (!identical(nrow(panel), .n_in)) {
      cat(sprintf(paste0("\n  *** %s REPLACED `panel` (%s rows in, %s out). ",
                         "Restoring\n      the master's frame before the next ",
                         "module. Everything below\n      PART 2 would ",
                         "otherwise have been on a different sample than\n",
                         "      everything above it.\n"),
                  .m, format(.n_in, big.mark = ","),
                  format(nrow(panel), big.mark = ",")))
      panel <- .panel_before
    }
  }
  
  options(td_frame = NULL)
  
  # Belt and braces on the whole sequence, in case a module replaced the
  # frame with one of the same height.
  if (!identical(nrow(panel), nrow(.panel_before))) panel <- .panel_before
  if (!identical(sink.number(), .sink_before))
    cat(sprintf(paste0("\n  *** SINK DEPTH CHANGED ACROSS PART 2 (%d before, ",
                       "%d after).\n"), .sink_before, sink.number()))
  rm(.panel_before, .sink_before)
  
  # ---- the two overrides, verified rather than assumed ---------------
  if (exists("BLIND") && !identical(as.logical(BLIND), .blind_requested))
    cat("\n  *** THE BLIND OVERRIDE DID NOT TAKE. A module reset BLIND to ",
        BLIND, ".\n      Patch its declaration to if (!exists(\"BLIND\")) ",
        "BLIND <- TRUE.\n", sep = "")
  rm(.blind_requested)
  
  if (exists("RUN_T8") && !identical(as.logical(RUN_T8), .t8_requested)) {
    cat("\n  *** THE T8 GATE DID NOT TAKE. RUN_T8 was reset to ", RUN_T8,
        ",\n", sep = "")
    cat("      overwriting the master's request. Patch the declaration in\n")
    cat("      03c_experience_units.R to\n")
    cat("        if (!exists(\"RUN_T8\")) RUN_T8 <- TRUE\n")
    cat("      which changes no default and lets this master iterate without\n")
    cat("      paying for the forests.\n")
  } else if (!MASTER_RUN_T8) {
    cat("\n  T8 skipped: 03c_experience_units.R was not sourced. Appendix A's\n")
    cat("  return-to-experience arms are NOT in this log. Everything else is.\n")
    cat("  Re-run with MASTER_RUN_T8 <- TRUE before the version that gets read.\n")
  }
  
  # ---- did the ledger merge, and from which modules ------------------
  # 03_finish.R runs at the end of EVERY module and rewrites the merged
  # ledger, so the merge is only as current as the last module to finish.
  # A per-module copy newer than the merge is a module that ran alone.
  .ld_dir <- if (exists("TD_OUTDIR") && length(TD_OUTDIR) == 1L &&
                 !is.na(TD_OUTDIR)) TD_OUTDIR else
                   if (exists("DIR_DESIGNS")) DIR_DESIGNS else "."
  .ld     <- file.path(.ld_dir, "training_ledger.csv")
  if (file.exists(.ld)) {
    .n_ld <- nrow(utils::read.csv(.ld, stringsAsFactors = FALSE))
    cat(sprintf("\n  Ledger: %s rows in %s\n", .n_ld, basename(.ld)))
    for (.pm in list.files(.ld_dir, "^training_ledger_.*\\.csv$")) {
      .p  <- file.path(.ld_dir, .pm)
      .np <- nrow(utils::read.csv(.p, stringsAsFactors = FALSE))
      cat(sprintf("    %-44s %3d rows  %s%s\n", .pm, .np,
                  format(file.info(.p)$mtime, "%Y-%m-%d %H:%M"),
                  if (file.info(.p)$mtime > file.info(.ld)$mtime)
                    "  *** NEWER THAN THE MERGE" else ""))
      rm(.p, .np)
    }
    if (.n_ld != 38L)
      cat("    *** EXPECTED 38 (26 from 03a, 12 from 03b, none from 03c,\n",
          "        since no T8 result passes through report()). A different\n",
          "        count means a module did not finish or ran alone.\n", sep = "")
    rm(.n_ld)
  } else {
    cat("\n  *** training_ledger.csv ABSENT after PART 2. No module reached\n")
    cat("      03_finish.R, so Tables 5 and 6 have nothing behind them.\n")
  }
  
  # The decisions count is the fastest check that the artefacts are the
  # T8 ones, and it is the same test the skip branch above applies.
  .dec <- file.path(.ld_dir, "training_decisions.txt")
  if (file.exists(.dec)) {
    .n_dec <- length(readLines(.dec, warn = FALSE))
    cat(sprintf("  Decisions recorded: %d\n", .n_dec))
    if (.n_dec < 20)
      cat("    *** FEWER THAN TWENTY. The T8 decisions are missing, so\n",
          "        Appendix A has nothing behind it.\n", sep = "")
    rm(.n_dec)
  }
  # rm() on a name that was never bound is an error, and .m, .n_in,
  # .sink_in and .sink_out only exist if the loop ran at least once. It
  # does not run when MASTER_RUN_T8 is FALSE and 03c was the only module
  # present. Removing by intersection rather than by name keeps the
  # cleanup from being the thing that stops the master.
  rm(list = intersect(c(".ld_dir", ".ld", ".dec", ".td03", ".missing",
                        ".m", ".n_in", ".sink_in", ".sink_out"),
                      ls(all.names = TRUE)))
  
} else {
  cat("\n  NOTE: none of 03a_design_audit.R, 03b_fee_descriptives.R or\n")
  cat("  03c_experience_units.R is in ", getwd(), ".\n", sep = "")
  cat("  T1-T9 unavailable. Nothing below this point reports the paper's\n")
  cat("  main results.\n")
  cat("  With 03_common.R and 03_finish.R these replaced\n")
  cat("  03training_designs.R, which no longer exists. If this master was\n")
  cat("  moved, move them with it.\n")
}
rm(.t8_requested)
# THE TIMING BLOCK DOES NOT NEED THIS MODULE IN MEMORY. T2 writes
# T2_profile_by_event_time.csv before T8 starts, and PART 4 reads that file
# from disk, so 8.2 can be run standalone against an interrupted run's
# output. That is the fallback when a long pass is killed: the exhibit does
# not have to wait for the forests to finish.

# ---- Training event study (Sun-Abraham, training outcomes) ------------------
# THE ONE MODULE THAT LEGITIMATELY READS ANOTHER FRAME. It uses the slim
# training panel by design, which is unfiltered. That is allowed; what is
# not allowed is it being invisible. If the module gains a contract call it
# should pass expect = "any", which prints the two sample sizes and says
# plainly that its estimates do not belong in a table with the rest.
if (file.exists("training_event_study.R")) {
  options(training_estimation_only = TRUE)
  source_logged("training_event_study.R", "training event study")
  options(training_estimation_only = NULL)
  .mpn("mpn_module_csv", "trpost")
  .mpn("mpn_module_csv", "trwin")
} else {
  cat("  NOTE: training_event_study.R not found; skipped.\n")
}

# ---- The funding contrast and the dosage margin -----------------------------
# Section 6.3 and 6.5. Adds the employer-funded vs own-time contrast, the
# promotion vs maintenance aim contrast (which PART 4 reports as a null),
# and total_courses_z as the dosage margin. Loader v21.48 gates that count:
# NA if never asked, 0 if asked and did not train, NA if a trainee gave no
# count. On an older loader the module falls back to total_courses.
#
# Block D, the randomization-inference placebo, is 300 model fits and is
# skipped under the master via tfd_fast. Run the script standalone for it
# before submission: it is cheap insurance on the headline contrast.
# THE FRAME MUST BE HANDED OVER, and this is the one place in the pipeline
# where forgetting it changes published numbers silently.
#
# That script's default is file-first, deliberately: a stale in-memory
# `panel` once shadowed a rebuilt one and dropped a pair with no symptom.
# The default is right for standalone use and wrong here. It reaches for
# hilda_panel_data_training.rds, which is a COLUMN subset of
# analysis_sample carrying NO row filter - 134,131 rows, 20,205 persons -
# while this master applied age_exp_diff on load and is running everything
# else on 129,544 rows and 19,661 persons.
#
# 4,587 person-waves and 544 people apart. Without the option below, the
# funding contrast and T1 to T9 are on different samples and the ledger
# prints -0.117 beside -0.063 as though they were not.
#
# options(tfd_frame = "panel") is the v23 contract, matching the existing
# tfd_fast convention. The script STOPS if the named frame is absent rather
# than falling through to a file, because a caller that names a frame and
# does not supply it has a bug, not a fallback.
# tfd_frame is gone: td_frame() in 00_contract.R resolves the frame for
# every script, so the caller no longer has to name it. tfd_fast stays,
# because run LENGTH is a caller decision (Block D is 300 model fits).
if (file.exists("training_funding_divergence.R")) {
  options(tfd_fast = TRUE)
  source_logged("training_funding_divergence.R", "funding contrast + dosage")
  options(tfd_fast = NULL)
  .mpn("mpn_module_csv", "dose")
} else {
  cat("  NOTE: training_funding_divergence.R not found; skipped.\n")
}

# v25. THE DOSAGE DECOMPOSITION IS AN ARTEFACT, AND THIS NAMES IT WHEN IT IS
# NOT. Section 6.5 quotes part 2 of the two-part model and its Lee bounds as
# proportional effects. The module computed them in logs and converted only
# inside cat(), so the two bounds the paper prints existed on no file and the
# prose check reported them as numbers computed but never written. The v25
# module writes dosage_decomposition.csv with both units. A missing file here
# means the run used an older copy of the module, which is worth a line in the
# log rather than a surprise three checks later.
.dose_f <- .in_path("dosage_decomposition.csv")
if (file.exists(.dose_f)) {
  cat(sprintf("  dosage_decomposition.csv  %s\n",
              format(file.info(.dose_f)$mtime, "%Y-%m-%d %H:%M")))
} else {
  cat("  *** dosage_decomposition.csv ABSENT. Section 6.5's extensive and\n")
  cat("      intensive margins and their bounds are in the console and in\n")
  cat("      logs inside training_funding_divergence_results.rds, and in no\n")
  cat("      file in the units the manuscript quotes. Re-run the v25 module\n")
  cat("      before the next verification pass.\n")
}
rm(.dose_f)


# =============================================================================
# PART 3: SUBSTITUTION  (paper section 7)
# =============================================================================
# T9 is estimated inside 03training_designs.R above; this part exists to
# name it, because in the outline it is a section of its own and in the code
# it is one design among nine.
#
# IT IS THE MECHANISM AND THE BEST-IDENTIFIED QUANTITY among the mechanism
# tests. A mother who simply stopped investing would show both margins
# falling. Employer-funded down, own-time study UP (+0.038, SE 0.011), and
# the contrast between them the sharpest of the three, is what happens when
# the employer's willingness changes and the worker's does not.
cat("\n  PART 3: substitution (T9) is estimated within 03a_design_audit.R.\n")
cat("  See the T9 block in the log above. Ledger value +0.038 (SE 0.011),\n")
cat("  status preferred.\n")



# =============================================================================
# PART 4: THE WAGE PENALTY, AND WHEN IT ARRIVES  (paper section 8)
# =============================================================================
# THE WAGE PENALTY IS A RESULT NOW, not scene-setting. The updated outline
# moved it: the timing comparison needs both paths on the same axis, so the
# event study cannot sit in section 5 as background and then be the exhibit
# in section 8.
#
# THE CLAIM THIS PART CARRIES. Employers withdraw training at the birth,
# mothers substitute their own time, and wages diverge about five years
# later. The paper does not offer a better estimate of the penalty. It
# shows what moves first, and the ordering is what makes it more than a
# restatement of a crowded literature.
#
# WHY THE ORDERING REPLACES THE DECOMPOSITION. A mediation analysis asks
# how much of the wage gap runs through training, and needs the mediator
# correctly specified to answer. The timing asks which moves first, which
# is prior and better identified. Accumulation predicts investment then
# wages. A story in which mothers are simply worse-matched or
# lower-productivity from the birth onward predicts NO LAG AT ALL, so the
# lag is the discriminating fact and a magnitude is not needed to use it.
#
#   8.1  The wage path            <- CS_event_study.R below
#   8.2  Investment moves first   <- the timing block, the central exhibit
#   8.3  Not a gendered price     <- A5, promoted from two paragraphs

# ---- 8.1 The wage path (Sun-Abraham child-penalty event study) ---------------
# Reuses the `panel` already in memory. Writes cs_event_study_results.rds,
# which 8.2 reads back for the timing comparison.
if (file.exists("CS_event_study.R")) {
  options(cs_estimation_only = TRUE)
  source_logged("CS_event_study.R", "8.1 the wage path")
  options(cs_estimation_only = NULL)
  .mpn("mpn_module_csv", "csatt")
  .mpn("mpn_module_csv", "wptest")
} else {
  cat("  NOTE: CS_event_study.R not found; cs_event_study_results.rds not refreshed.\n")
  cat("  8.2 will fall back to the tabulated wage path. See the block below.\n")
}

# =============================================================================
# 8.2 INVESTMENT MOVES FIRST: THE TIMING BLOCK
# =============================================================================
# THE PAPER'S CENTRAL EXHIBIT. Wage path and employer-funded training path on
# one set of axes, event time on the other. Everything in section 8 is
# written to it.
#
# NOTHING IS RE-ESTIMATED HERE and that is deliberate. Both paths already
# exist: T2 in 03training_designs.R writes the training gap by event time to
# T2_profile_by_event_time.csv, and CS_event_study.R writes the wage path to
# cs_event_study_results.rds. This block ALIGNS them on a common k grid and
# tests the ordering. A block that re-estimated either path would be a
# second chance to get a different number, which is the opposite of what a
# pre-committed design wants.
#
# THE OUTLINE SAYS TO DO THIS EARLY even though it is step 3, because seeing
# both paths on one axis tells you quickly whether the ordering is as clean
# as the two coefficient tables suggest. It is not quite, and the block is
# written to say so rather than to smooth it. See the SIGNIFICANCE PATTERN
# warning below.

# ---- the training path ------------------------------------------------------
# T2's `gap` column is rate_1 - rate_0, mothers minus fathers, in the
# employer-funded outcome (jttpewt_any), employed only. It is a raw
# difference by event time and not a regression coefficient, which is the
# right object for a timing picture: it carries no reference-period
# normalisation to argue about.
# ---- the training path: SUN-ABRAHAM, to match the wage path -----------------
# THE ORDERING IS THE CONTRIBUTION, so the two series it rests on have to be
# estimated the same way. This read T2_profile_by_event_time.csv, which is
# 03's REFERENCE-PERIOD DIAGNOSTIC: a descriptive profile, not an estimator,
# defined only on k = -5..+8, running +0.0908, +0.0945, +0.1048 BEFORE the
# birth and +0.0047 AT it. The wage series beside it is Sun-Abraham with
# never-treated controls over 23 horizons. Setting one against the other and
# reading a lag off the pair compares two different objects, and the
# subtitle "training falls at the birth" was being carried by a series that
# is positive before the birth and near zero at it.
#
# training_event_study.R already estimates this correctly and nothing read
# it: sunab(cohort, wave) | id + wave on the SAME roster as
# CS_event_study.R, 10,650 persons of whom 7,946 never-treated, the same
# 23-horizon grid, same women-minus-men differencing with SE in quadrature.
#
# On employed_only jttpewt_any it gives:
#   k = 0   -0.1163 (t = -5.33)   training crosses here
#   k = +6  wages cross           (-0.0694, t = -2.25)
#   pre-birth horizons significant: ZERO on either series
# and the k = 0 estimate reproduces T1_jttpewt_any_employed (-0.116262) to
# four decimals. A staggered-robust estimator on never-treated controls
# recovering the two-way fixed-effects headline is the Goodman-Bacon
# objection answered with a number rather than an argument, and it is worth
# reporting rather than leaving implicit.
.tim_train <- NULL
# v-route. output/results FIRST, because that is where the file is now.
# training_event_study.R used to write a bare relative path, so the object
# landed in whatever the working directory was, and this reader looked in
# TD_OUTDIR then beside the script and found it. Routing the children
# through the contract moved it to output/results and left this list
# pointing at the two places it is not.
#
# THE FAILURE IS SILENT AND THAT IS THE COST. .te stays NULL, the timing
# block and the 8.2b decomposition both open with exists(".te"), and both
# skip without a word: on 9 September training_event_study_results.rds was
# written at 11:33 and timing_paths.csv kept its 11:01 build two minutes
# later, in the same pass. Three files a run behind while everything
# around them updated.
.te_path <- unique(c(
  file.path(if (exists("DIR_RESULTS")) DIR_RESULTS else
              if (exists("PROJ_ROOT")) file.path(PROJ_ROOT, "output", "results")
              else ".", "training_event_study_results.rds"),
  file.path(if (!is.na(TD_OUTDIR)) TD_OUTDIR else ".",
            "training_event_study_results.rds"),
  "training_event_study_results.rds"))
for (.p in .te_path) {
  if (is.null(.tim_train) && file.exists(.p)) {
    .te <- tryCatch(readRDS(.p), error = function(e) NULL)
    if (is.list(.te) && !is.null(names(.te))) {
      # BY NAME, NEVER BY POSITION. This object holds 24 series and the
      # wrong one looks exactly as plausible as the right one; picking the
      # first data frame is what put the women-only wage series into this
      # figure for a day.
      .arm <- if (exists("TIMING_ARM")) TIMING_ARM else "employed_only"
      # training_paid_time, NOT jttpewt_any. Figure~\ref{fig:ordering} and
      # the "training path" subsection of the manuscript quote this arm:
      # -0.0067 at k = -6, +0.0162 at -4, +0.0073 at -2, -0.1226 (SE
      # 0.0231) at the birth wave and -0.1527 at +1. All five match to the
      # quoted precision; jttpewt_any matches none of them. Defaulting to
      # the wrong outcome would have produced a timing block that disagreed
      # with the paper's own figure while looking entirely reasonable.
      .out <- if (exists("TIMING_OUTCOME")) TIMING_OUTCOME else "training_paid_time"
      .w <- paste(.arm, "women", .out); .m <- paste(.arm, "men", .out)
      if (all(c(.w, .m) %in% names(.te))) {
        .j <- merge(.te[[.w]][, c("event_time", "att", "se")],
                    .te[[.m]][, c("event_time", "att", "se")],
                    by = "event_time", suffixes = c("_w", "_m"))
        .tim_train <- data.frame(k   = as.integer(.j$event_time),
                                 gap = .j$att_w - .j$att_m,
                                 se  = sqrt(.j$se_w^2 + .j$se_m^2))
        # v-cw. THE GAP ON COMMON COHORT WEIGHTS. The difference above puts
        # each arm on its own cell weights, so it mixes the gender gap with
        # differences in cohort composition between the arms. Since the
        # revision training_event_study.R writes "<arm> gap <outcome>", the
        # difference formed inside each cohort-by-event cell and averaged on
        # pooled counts (sa_common_weights.R). That is the series drawn, and
        # the per-sex difference above is the fallback for an older file.
        .gk <- paste(.arm, "gap", .out)
        .cw_used <- is.data.frame(.te[[.gk]])
        if (.cw_used) {
          .tim_train <- data.frame(k   = as.integer(.te[[.gk]]$event_time),
                                   gap = .te[[.gk]]$gap,
                                   se  = .te[[.gk]]$se)
        } else {
          cat("  *** ", .gk, " is absent from ", basename(.p), ", so the training\n",
              "      path differences the two arms on their own weights. Rerun\n",
              "      training_event_study.R with the common-weight patch.\n", sep = "")
        }
        .tim_train <- .tim_train[order(.tim_train$k), ]
        .train_src <- sprintf(if (.cw_used)
          "Sun-Abraham, %s %s, women minus men on common cohort weights" else
          "Sun-Abraham, %s %s, women minus men, own weights per arm (SE in quadrature)",
                              .arm, .out)
        rm(.gk, .cw_used)
        cat("  training path read from:", .p, "\n")
        cat("   ", .train_src, "\n")
      } else {
        cat("  NOTE:", basename(.p), "lacks", .w, "or", .m, "\n")
        cat("        Has:", paste(head(names(.te), 3), collapse = " | "), "...\n")
      }
    }
  }
}
if (is.null(.tim_train)) {
  .train_src <- "ABSENT"
  cat("  *** NO TRAINING PATH. The timing comparison is SKIPPED rather than\n",
      "      drawn from T2's descriptive profile, which is not an estimator\n",
      "      and cannot be set beside a Sun-Abraham wage path.\n")
}

# ---- the wage path ----------------------------------------------------------
# TABULATED FALLBACK, and it is a fallback rather than a source. These are
# the outline's section 2 values. They are used only when
# cs_event_study_results.rds is absent, and the block SAYS SO in the log,
# because a timing figure drawn from hardcoded numbers that silently
# disagree with the rds is the worst outcome available here.
.wage_tab <- data.frame(
  k   = c(0, 2, 4, 6, 8, 10),
  est = c(-0.026, -0.022, -0.026, -0.069, -0.061, -0.119),
  se  = c( 0.019,  0.020,  0.024,  0.031,  0.039,  0.053))

.tim_wage <- NULL; .wage_src <- "tabulated (outline section 2)"
# v-age. The age-absorbed companion. It stays NULL without the RDS, so the
# tabulated fallback never acquires a second line it has no source for.
.tim_wage_age <- NULL
if (file.exists(.in_path("cs_event_study_results.rds"))) {
  .cs <- tryCatch(readRDS(.in_path("cs_event_study_results.rds")),
                  error = function(e) NULL)
  # The rds shape has changed across builds, so probe rather than assume.
  # Any data.frame carrying an event-time column and an estimate column will
  # do; anything else falls through to the table with a note.
  # v24.12. NAME THE ELEMENT. Filter() returns the list in ORDER, and
  # cs_event_study_results.rds stores "women ln_hourly_wage_real" before
  # "gap ln_hourly_wage_real", so the first data frame taken was the WOMEN'S
  # OWN PATH against never-treated women, not the mothers-minus-fathers gap.
  #
  # THAT IS WHY TWO WAGE PATHS DISAGREED. A women-only path carries a
  # pre-trend, because women who go on to have children differ from women who
  # do not: the run read eight of ten pre-birth horizons significant where the
  # gap reads zero of eleven. Differencing against men removes what is common
  # to both, which is the design. The paper's series was right; the reader was
  # picking a different object.
  #
  # CS_event_study.R says so inside the object it writes: "Downstream
  # consumers should read the 'gap ' elements and must not difference the
  # per-sex series themselves." The old selector did neither.
  .cand <- if (is.data.frame(.cs)) list(.cs) else if (is.list(.cs)) {
    .nm  <- names(.cs)
    .gp  <- if (!is.null(.nm)) which(grepl("^gap ", .nm)) else integer(0)
    if (length(.gp)) {
      .wg <- .gp[grepl("wage", .nm[.gp])]
      # v-age. THE PLAIN ARM FIRST, BY NAME. CS_event_study.R now stores
      # "gap ln_hourly_wage_real_agefe" beside "gap ln_hourly_wage_real",
      # and both match "wage". The loop below keeps the first frame it can
      # read, so the plain arm won only because it is written first. Sorting
      # on the exact name makes that a decision. order() is stable, so any
      # other wage element keeps its place behind it.
      .wg <- .wg[order(.nm[.wg] != "gap ln_hourly_wage_real")]
      Filter(is.data.frame, .cs[if (length(.wg)) .wg else .gp])
    } else {
      cat("  *** NO 'gap ' ELEMENT in cs_event_study_results.rds. Falling back\n")
      cat("      to the first data frame, which may be a SINGLE-SEX path and\n")
      cat("      would carry a pre-trend by construction.\n")
      cat("      Elements: ", paste(.nm, collapse = ", "), "\n", sep = "")
      Filter(is.data.frame, .cs)
    }
  } else list()
  for (.d in .cand) {
    .kc <- intersect(c("k", "rel_time", "event_time", "time", "period"), names(.d))
    # v24.14. "gap" ADDED TO THE ESTIMATE-COLUMN CANDIDATES, and without it
    # v24.12 was only half a fix. CS_event_study.R names the columns
    # differently in the two kinds of element:
    #
    #     per-sex frames   event_time, att, se
    #     gap frames       event_time, gap, se
    #
    # So selecting the "gap " element by name got the right data frame and
    # then failed the column test, because "gap" was not in this list. The
    # loop assigned nothing, and the block fell through to the tabulated
    # .wage_tab - which the 24 August log records as "tabulated (outline
    # section 2)". The figure was drawn from literals while the RDS sat
    # unread beside it.
    #
    # "gap" goes LAST so a frame carrying both att and gap still resolves to
    # att, which is the per-sex convention.
    .ec <- intersect(c("est", "estimate", "att", "coef", "coefficient", "gap"),
                     names(.d))
    .sc <- intersect(c("se", "std.error", "stderr", "se_att"), names(.d))
    if (length(.kc) && length(.ec) && is.null(.tim_wage)) {
      .tim_wage <- data.frame(k = .d[[.kc[1]]], est = .d[[.ec[1]]],
                              se = if (length(.sc)) .d[[.sc[1]]] else NA_real_)
      .wage_src <- sprintf("cs_event_study_results.rds (%s / %s)", .kc[1], .ec[1])
    }
  }
  # v-age. THE AGE-ABSORBED WAGE PATH, READ BY NAME AND KEPT APART.
  #
  # Figure 2 draws it as a second line in the wage panel. Nothing in the
  # timing test reads it: the crossings, the lag and the ledger below all
  # stay on the plain path. It travels to RUN_FIGURES_PAPER.R as two extra
  # columns of timing_paths.csv, wage_age_est and wage_age_se. Neither name
  # matches a Check 7b candidate in the verifier, exactly or by its regex
  # fallback, so that check reads what it read before.
  .ag <- if (is.list(.cs) && !is.data.frame(.cs))
    .cs[["gap ln_hourly_wage_real_agefe"]] else NULL
  if (is.data.frame(.ag) && all(c("event_time", "gap", "se") %in% names(.ag))) {
    .tim_wage_age <- data.frame(k            = as.integer(.ag$event_time),
                                wage_age_est = .ag$gap,
                                wage_age_se  = .ag$se)
    cat("  age-absorbed wage path read: gap ln_hourly_wage_real_agefe,",
        nrow(.tim_wage_age), "horizons\n")
  } else {
    cat("  NOTE: cs_event_study_results.rds has no usable\n",
        "        'gap ln_hourly_wage_real_agefe', so timing_paths.csv carries\n",
        "        no age-absorbed path and Figure 2 draws the plain one alone.\n",
        "        CS_event_study.R fits that arm only when `age` is on the frame.\n",
        sep = "")
  }
  rm(.ag)
  rm(.cs, .cand)
}
if (is.null(.tim_wage)) {
  .tim_wage <- .wage_tab
  cat("  *** WAGE PATH IS THE TABULATED FALLBACK, not the rds. Any figure\n")
  cat("      built from this run is provisional. Re-run with CS_event_study.R\n")
  cat("      present before the exhibit goes in the paper.\n")
} else {
  cat("  wage path read from:", .wage_src, "\n")
}

# ---- align and test ---------------------------------------------------------
if (!is.null(.tim_train)) {
  .tw <- .tim_train[, c("k", "gap", "se")]
  names(.tw) <- c("k", "train_gap", "train_se")
  .timing <- merge(.tw, .tim_wage, by = "k", all = TRUE)
  names(.timing)[names(.timing) == "est"] <- "wage_est"
  names(.timing)[names(.timing) == "se"]  <- "wage_se"
  # v-age. all.x, because the age arm is a companion to the plain path and
  # must not add horizons that the timing test would read as missing wages.
  if (!is.null(.tim_wage_age))
    .timing <- merge(.timing, .tim_wage_age, by = "k", all.x = TRUE)
  .timing <- .timing[order(.timing$k), ]
  .timing$wage_t  <- round(abs(.timing$wage_est  / .timing$wage_se),  2)
  .timing$train_t <- round(abs(.timing$train_gap / .timing$train_se), 2)
  if ("wage_age_est" %in% names(.timing))
    .timing$wage_age_t <- round(abs(.timing$wage_age_est / .timing$wage_age_se), 2)
  # The wage side is rounded to four decimals upstream and the training side
  # was not, so the printed table had train_se at eight decimals against
  # wage_se at four. The t is computed BEFORE this, off the full precision.
  .timing$train_gap <- round(.timing$train_gap, 4)
  .timing$train_se  <- round(.timing$train_se,  4)
  
  cat("\n", strrep("=", 78), "\n", sep = "")
  cat("  8.2  THE TIMING COMPARISON: training gap vs wage gap by event time\n")
  cat(strrep("=", 78), "\n", sep = "")
  print(.timing, row.names = FALSE)
  cat("  train_gap:", .train_src, "\n")
  cat("  wage_est:  event-study coefficient on ln hourly wage;", .wage_src, "\n")
  
  # WHEN DOES EACH PATH MOVE. Both sides now carry standard errors, so the
  # lag is the distance between two crossings tested the same way. It used
  # to be a level change on the training side against a t-test on the wage
  # side, because T2 had no SE, which made the headline claim of this paper
  # a comparison between two different kinds of evidence.
  .cross <- function(t, k) {
    ok <- !is.na(t) & k >= 0 & t >= 1.96
    if (any(ok)) min(k[ok]) else NA_integer_
  }
  .t_sig <- .cross(.timing$train_t, .timing$k)
  .w_sig <- .cross(.timing$wage_t,  .timing$k)
  
  if (!is.na(.t_sig))
    cat(sprintf("\n  Training: first |t| >= 1.96 at k = %+d (gap %+.4f).\n",
                as.integer(.t_sig),
                .timing$train_gap[.timing$k == .t_sig]))
  else
    cat("\n  Training: never reaches |t| >= 1.96 after the birth.\n")
  
  if (!is.na(.w_sig))
    cat(sprintf("  Wages:    first |t| >= 1.96 at k = %+d (gap %+.4f).\n",
                as.integer(.w_sig), .timing$wage_est[.timing$k == .w_sig]))
  else
    cat("  Wages:    never reach |t| >= 1.96 on this grid.\n")
  
  if (!is.na(.t_sig) && !is.na(.w_sig)) {
    if (.w_sig > .t_sig)
      cat(sprintf("  LAG: %d waves. Investment moves at k = %+d, price at k = %+d,\n",
                  as.integer(.w_sig - .t_sig), as.integer(.t_sig),
                  as.integer(.w_sig)),
          "       both Sun-Abraham on the same roster and control group.\n",
          sep = "")
    else
      cat(sprintf("  *** NO LAG in the claimed direction: wages cross at k = %+d,\n",
                  as.integer(.w_sig)),
          sprintf("      training at k = %+d. The ordering claim does not hold\n",
                  as.integer(.t_sig)),
          "      on these numbers. Do not write it.\n", sep = "")
  } else {
    cat("  The ordering claim cannot be made from these numbers; say so\n")
    cat("  rather than reading a lag off point estimates.\n")
  }
  
  # ---- THE PRE-TREND, TESTED BEFORE ANYTHING POST IS READ -----------------
  # v23. ADDED AFTER THE FIRST RUN, which produced a wage path with seven of
  # eight pre-birth horizons significant: -0.1325 at k = -11 (t 2.91),
  # -0.1112 at -10 (2.79), -0.0951 at -8 (3.56), -0.0662 at -7 (3.02), and
  # so on down to -0.0263 at -4 (2.07).
  #
  # A PRE-TREND THAT LARGE MAKES THE POST PATH UNREADABLE, and the ordering
  # argument is the first casualty. "Investment moves first" needs the wage
  # gap to be flat before the birth and to open after it. A gap already at
  # -0.13 eleven waves EARLIER is not a lag; it is a level difference
  # between women who go on to have children and women who do not, and the
  # event study is picking up selection rather than timing.
  #
  # This is reported BEFORE the lag, because a lag computed on a trending
  # series is a number without a claim attached.
  .pre <- .timing[!is.na(.timing$wage_t) & .timing$k < -1, ]
  if (nrow(.pre)) {
    .n_sig <- sum(.pre$wage_t >= 1.96)
    cat(sprintf("\n  PRE-TREND: %d of %d pre-birth horizons reach |t| >= 1.96.\n",
                .n_sig, nrow(.pre)))
    if (.n_sig > max(1L, ceiling(0.1 * nrow(.pre)))) {
      cat("  *** THE WAGE PATH IS NOT FLAT BEFORE THE BIRTH. The ordering\n")
      cat("      argument requires it to be. A gap that is already open at\n")
      cat("      k = -8 is a LEVEL difference between mothers and\n")
      cat("      never-mothers, not a lag, and section 8.2 cannot be written\n")
      cat("      from this series as it stands.\n")
      cat("      Check first: is this the right column of the rds, is the\n")
      cat("      reference period k = -1 or something else, and is the\n")
      cat("      comparison group never-treated or not-yet-treated. The\n")
      cat("      TRAINING path is flat pre-birth on the same axis, so the\n")
      cat("      problem is specific to the wage series.\n")
      warning("timing block: ", .n_sig, " of ", nrow(.pre),
              " pre-birth wage horizons are significant. The ordering claim ",
              "cannot rest on this series.", call. = FALSE)
    } else {
      cat("  Flat before the birth, so the post path is readable as timing.\n")
    }
    rm(.n_sig)
  }
  rm(.pre)
  
  # ---- THE HONESTY CHECK THAT THE OUTLINE'S TABLE HIDES --------------------
  # The wage path's significance is NOT monotone. On the tabulated values,
  # |t| runs 1.37, 1.10, 1.08, 2.23, 1.56, 2.25 across k = 0,2,4,6,8,10: it
  # crosses at +6, falls back BELOW the threshold at +8, and crosses again
  # at +10. "Indistinguishable from zero for four years then significant
  # thereafter" is not what the numbers say, and a referee reading the SE
  # column will see it immediately.
  #
  # The ordering claim survives this - a gap that is flat early and large
  # late is still an ordering - but it has to be stated as a rising path
  # with a noisy middle, not as a clean break. The alternative is to
  # aggregate the post-window, which buys power at the cost of the timing
  # resolution the whole argument depends on.
  # .first_sig and .w were the wage-only names this block used before the
  # training path acquired standard errors. Same test, current names.
  if (!is.na(.w_sig)) {
    .after <- .timing[!is.na(.timing$wage_t) & .timing$k > .w_sig, ]
    .gaps  <- .after$k[.after$wage_t < 1.96]
    if (length(.gaps)) {
      cat("\n  *** SIGNIFICANCE IS NOT MONOTONE. After first crossing at k = ",
          sprintf("%+d", as.integer(.w_sig)), ", |t| falls back below\n", sep = "")
      cat("      1.96 at k = ", paste(sprintf("%+d", as.integer(.gaps)),
                                      collapse = ", "), ".\n", sep = "")
      cat("      Do not write 'indistinguishable from zero for four years and\n")
      cat("      significant thereafter'. Write a rising path with a noisy\n")
      cat("      middle, and give the t-statistics. Aggregating the post\n")
      cat("      window would buy power by discarding the timing resolution\n")
      cat("      the argument rests on, so it is not the fix.\n")
    }
    rm(.after, .gaps)
  }
  
  # The discriminating logic, printed beside the numbers that carry it.
  cat("\n  WHAT THE LAG RULES OUT. Accumulation predicts investment then\n")
  cat("  wages. A lower-productivity or worse-match account predicts the wage\n")
  cat("  gap opening AT the birth, with no lag. The lag is therefore the\n")
  cat("  discriminating fact, and it does not require a magnitude - which is\n")
  cat("  why this replaces the Gelbach decomposition rather than deferring to\n")
  cat("  it.\n")
  
  write.csv(.timing, .out_path("timing_paths.csv"), row.names = FALSE)
  cat("\n  Written: timing_paths.csv  (the figure's data)\n")
  
  # ---- the exhibit ---------------------------------------------------------
  # Drawn here rather than left to RUN_FIGURES_hh.R because the outline makes
  # it step 3 and the reason is diagnostic: seeing both paths together is how
  # you find out whether the ordering is clean before writing to it.
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    # BOTH PATHS NOW CARRY A BAND. The training series came from T2, a
    # descriptive profile with no standard errors, so its lo/hi were NA and
    # only the wage path had a ribbon: the training line read as though it
    # were estimated precisely when it simply had nothing drawn. Both are
    # Sun-Abraham now, so both get an interval and the reader can see that
    # the training path is the tighter of the two, which is the point.
    #
    # "financed", not "funded": the manuscript standardised on
    # employer-financed, the term it defines twice, and a figure legend
    # disagreeing with every heading and table caption in the paper is the
    # kind of thing a referee notices before the argument.
    .pl <- rbind(
      data.frame(k = .timing$k, value = .timing$train_gap,
                 lo = .timing$train_gap - 1.96 * .timing$train_se,
                 hi = .timing$train_gap + 1.96 * .timing$train_se,
                 path = "Employer-financed training gap (pp)"),
      data.frame(k = .timing$k, value = .timing$wage_est,
                 lo = .timing$wage_est - 1.96 * .timing$wage_se,
                 hi = .timing$wage_est + 1.96 * .timing$wage_se,
                 path = "ln hourly wage gap"))
    .pl <- .pl[!is.na(.pl$value) & !is.na(.pl$k), ]
    .g <- ggplot2::ggplot(.pl, ggplot2::aes(k, value, colour = path)) +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
      ggplot2::geom_vline(xintercept = 0, linetype = 2, linewidth = 0.3) +
      # The subset stays, though both paths now have intervals: a horizon
      # with a missing SE would still take the layer down, and the filter
      # costs nothing.
      #
      # show.legend = FALSE on the ribbon. The fill scale used to emit its
      # own key, so the legend read "ln hourly wage gap" twice, once for
      # the line and once for the band. The colour legend already names
      # both series.
      ggplot2::geom_ribbon(
        data = .pl[!is.na(.pl$lo) & !is.na(.pl$hi), , drop = FALSE],
        ggplot2::aes(ymin = lo, ymax = hi, fill = path),
        alpha = 0.15, colour = NA, show.legend = FALSE) +
      ggplot2::geom_line(linewidth = 0.7) +
      ggplot2::geom_point(size = 1.6) +
      ggplot2::labs(x = "Waves from first birth",
                    y = "Mothers minus fathers",
                    colour = NULL, fill = NULL,
                    title = "Investment moves first",
                    # The subtitle is BUILT FROM THE SERIES, not asserted.
                    # It used to state that training falls at the birth
                    # while the line plotted was T2's profile, which is
                    # positive before the birth and +0.0047 at it. A caption
                    # that cannot be wrong is worth the four lines.
                    subtitle = local({
                      .f <- function(x, lab)
                        if (is.na(x)) paste(lab, "never significant")
                      else sprintf("%s significant from k = %+d",
                                   lab, as.integer(x))
                      paste0(.f(.t_sig, "Employer-financed training gap"),
                             "; ", .f(.w_sig, "wage gap"))
                    })) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(legend.position = "bottom")
    ggplot2::ggsave(.out_path("fig_timing_investment_before_wages.pdf"), .g,
                    width = 7, height = 4.6)
    cat("  Written: fig_timing_investment_before_wages.pdf\n")
    cat("  TWO SCALES ON ONE AXIS. The training path is in percentage points\n")
    cat("  and the wage path in log points, so the vertical distance between\n")
    cat("  them means nothing; only the TIMING of each does. Say that in the\n")
    cat("  note, or split into stacked panels with a shared x axis.\n")
    rm(.pl, .g)
  } else {
    cat("  NOTE: ggplot2 unavailable; timing_paths.csv written, no figure.\n")
  }
  # ==========================================================================
  # 8.2b  MOTHERS FALLING, OR FATHERS RISING
  #
  # T1 identifies mothers MINUS fathers, so both arms are treated and the
  # estimand is a gender difference among parents. It cannot say which side
  # moves, and "the gap widens" is compatible with mothers falling, fathers
  # rising, or both. A referee will ask, and until now the paper could not
  # answer.
  #
  # It does not need re-estimating. training_event_study.R already fits the
  # two sexes SEPARATELY against never-treated controls of the same sex -
  # mothers against never-mothers, fathers against childless men - and the
  # timing block above was reading only their difference. The levels are the
  # decomposition, and they were sitting in the same object.
  #
  # On employed_only jttpewt_any the answer is unambiguous: at k = 0 mothers
  # are -0.1017 (t = -6.02) and fathers +0.0146 (t = 1.06); eleven of twelve
  # post-birth horizons are significant for mothers and two of twelve for
  # fathers, one of those negative. Roughly seven eighths of the gap is the
  # mothers' side.
  #
  # IT DOES NOT HOLD FOR EVERY OUTCOME, which is why this is a table and not
  # a sentence. On total_training_days both sexes fall (-1.667 and -1.229),
  # so the dosage gap is a difference between two declines rather than a
  # decline against a flat line. Reporting the incidence result and letting
  # the reader assume it generalises would be the wrong summary.
  #
  # v21.48: the two days figures above are post-birth averages on employed
  # person-waves from the ungated column. On total_training_days_z, the 1
  # October run gives -1.67 and -1.21, so the reading stands.
  # ==========================================================================
  .decomp <- NULL
  # v-cw. The gap columns come from the common-weight series when
  # training_event_study.R wrote one; mothers and fathers stay as estimated,
  # each against its own never-treated controls. RUN_FIGURES_PAPER.R and
  # Check 7b read gap and gap_se from training_decomposition.csv, so this is
  # where the common-weight gap reaches the figures.
  .cw_col <- function(te, a, o, d, col, fallback) {
    cg <- te[[paste(a, "gap", o)]]
    if (!is.data.frame(cg) || !col %in% names(cg)) return(fallback)
    v <- cg[[col]][match(d$event_time, cg$event_time)]
    ifelse(is.na(v), fallback, v)
  }
  # AN ELSE, because exists(".te") failing quietly is how this block and the
  # timing one above fell a build behind without appearing in the log at all.
  # A guard that skips work should name the work it skipped and the file it
  # wanted, or the only place the gap shows is a folder listing.
  if (!exists(".te") || !is.list(.te) || is.null(names(.te)))
    cat("\n  .te is absent, so training_decomposition.csv and the timing\n",
        "  figure KEEP THEIR PREVIOUS BUILD. Looked for\n",
        "  training_event_study_results.rds in:\n",
        paste0("    ", .te_path, collapse = "\n"), "\n",
        "  Run training_event_study.R first, or check it is writing where\n",
        "  this reads.\n", sep = "")
  if (exists(".te") && is.list(.te) && !is.null(names(.te))) {
    # The "<arm> gap <outcome>" elements are not arm-by-sex series, so the
    # arms and outcomes are read off the per-sex names only.
    .ps   <- names(.te)[grepl(" (women|men) ", names(.te))]
    .arms <- unique(sub(" (women|men) .*$", "", .ps))
    .outs <- unique(sub("^\\S+ (women|men) ", "", .ps))
    rm(.ps)
    for (.a in .arms) for (.o in .outs) {
      .kw <- paste(.a, "women", .o); .km <- paste(.a, "men", .o)
      if (!all(c(.kw, .km) %in% names(.te))) next
      .d <- merge(.te[[.kw]][, c("event_time", "att", "se")],
                  .te[[.km]][, c("event_time", "att", "se")],
                  by = "event_time", suffixes = c("_mo", "_fa"))
      if (!nrow(.d)) next
      .decomp <- rbind(.decomp, data.frame(
        arm = .a, outcome = .o, k = as.integer(.d$event_time),
        mothers = round(.d$att_mo, 4), mothers_se = round(.d$se_mo, 4),
        fathers = round(.d$att_fa, 4), fathers_se = round(.d$se_fa, 4),
        gap     = round(.cw_col(.te, .a, .o, .d, "gap", .d$att_mo - .d$att_fa), 4),
        gap_se  = round(.cw_col(.te, .a, .o, .d, "se",
                                sqrt(.d$se_mo^2 + .d$se_fa^2)), 4),
        gap_separate = round(.d$att_mo - .d$att_fa, 4),
        stringsAsFactors = FALSE))
    }
    # Registered from .te itself, before the rounding above, so the training
    # path the paper quotes (employed_only training_paid_time) keeps its digits.
    .mpn("mpn_side", .te)
  }
  
  if (is.null(.decomp)) {
    cat("\n  NOTE: no decomposition; the event-study object is not in scope.\n")
  } else {
    .decomp <- .decomp[order(.decomp$arm, .decomp$outcome, .decomp$k), ]
    .decomp$mothers_t <- round(abs(.decomp$mothers / .decomp$mothers_se), 2)
    .decomp$fathers_t <- round(abs(.decomp$fathers / .decomp$fathers_se), 2)
    write.csv(.decomp, .out_path("training_decomposition.csv"), row.names = FALSE)
    
    # ==========================================================================
    # v25. THE WORKPLACE / ALL-PLACES DISTANCE, WITH ITS WINDOW NAMED.
    # Section 6.2, the figure note and the Online Appendix all quote "about
    # 13 percent" for the distance between the workplace pair and the pair
    # that also counts training elsewhere, and they quote it three ways: the
    # workplace item running smaller, pooling raising the decline, and a
    # property of the k = 0 table. Those are three different numbers on this
    # series: 5 percent at the birth wave, 14 percent averaged to k = +8 and
    # 29 percent over every post-birth horizon on the last build. The table
    # below writes each window out, and keeps the two percentage columns
    # apart, because x percent larger is not x percent smaller.
    # ==========================================================================
    .pl_pairs <- list(c(work = "jttpewt_any", all = "training_paid_time"),
                      c(work = "jttpeot_any", all = "training_own_time"))
    .pl_ratio <- NULL
    for (.pl_a in unique(.decomp$arm)) for (.pl_pr in .pl_pairs) {
      .pl_w <- .decomp[.decomp$arm == .pl_a &
                         .decomp$outcome == .pl_pr["work"], c("k", "gap")]
      .pl_all <- .decomp[.decomp$arm == .pl_a &
                           .decomp$outcome == .pl_pr["all"], c("k", "gap")]
      if (!nrow(.pl_w) || !nrow(.pl_all)) next
      .pl_m <- merge(.pl_w, .pl_all, by = "k", suffixes = c("_work", "_all"))
      .pl_m <- .pl_m[.pl_m$k >= 0, ]
      if (!nrow(.pl_m)) next
      for (.pl_hi in unique(c(0, min(8, max(.pl_m$k)), max(.pl_m$k)))) {
        .pl_x <- .pl_m[.pl_m$k <= .pl_hi, ]
        if (!nrow(.pl_x)) next
        .pl_mw <- mean(.pl_x$gap_work); .pl_ma <- mean(.pl_x$gap_all)
        # A RATIO OF TWO MEANS WITH OPPOSITE SIGNS IS NOT A DISTANCE. On the
        # own-time pair the average gap crosses zero at long horizons, and
        # the ratio there reads as -175 percent, which is arithmetic rather
        # than a finding. The window is still written, with the ratio blank.
        .pl_ok <- is.finite(.pl_mw) && is.finite(.pl_ma) && .pl_mw != 0 &&
          sign(.pl_mw) == sign(.pl_ma)
        .pl_r <- if (.pl_ok) .pl_ma / .pl_mw else NA_real_
        .pl_ratio <- rbind(.pl_ratio, data.frame(
          arm = .pl_a, pair = unname(.pl_pr["work"]),
          k_lo = 0L, k_hi = as.integer(.pl_hi), horizons = nrow(.pl_x),
          mean_gap_workplace  = .pl_mw,
          mean_gap_all_places = .pl_ma,
          all_over_workplace  = .pl_r,
          pooling_raises_pct    = if (.pl_ok) 100 * (.pl_r - 1) else NA_real_,
          workplace_smaller_pct = if (.pl_ok) 100 * (1 - 1 / .pl_r) else NA_real_,
          comparable = .pl_ok,
          stringsAsFactors = FALSE))
      }
    }
    if (!is.null(.pl_ratio)) {
      rownames(.pl_ratio) <- NULL
      write.csv(.pl_ratio, .out_path("training_place_ratio.csv"),
                row.names = FALSE)
      .mpn("mpn_place", .pl_ratio)
      .pl_print <- .pl_ratio
      .pl_print[] <- lapply(.pl_print,
                            function(z) if (is.numeric(z)) round(z, 3) else z)
      cat("\n  Workplace against all places, by window:\n")
      print(.pl_print, row.names = FALSE)
      cat("  Written: training_place_ratio.csv\n")
      cat("  NONE OF THESE ROWS IS THE STATIC CONTRAST. PART 6's run record\n")
      cat("  prints pe_gated against pooled_gated on identical rows, which is\n")
      cat("  a different estimand from an average of horizon coefficients and\n")
      cat("  is where the 13 percent came from. Quote one, and say which.\n")
      rm(.pl_print)
    }
    rm(list = intersect(c(".pl_pairs", ".pl_ratio", ".pl_a", ".pl_pr", ".pl_w",
                          ".pl_all", ".pl_m", ".pl_x", ".pl_r", ".pl_hi",
                          ".pl_mw", ".pl_ma", ".pl_ok"),
                        ls(all.names = TRUE)))
    
    cat("\n", strrep("=", 78), "\n", sep = "")
    cat("  8.2b  WHICH SIDE MOVES: mothers vs never-mothers, fathers vs\n")
    cat("        childless men, by outcome. Sun-Abraham, never-treated\n")
    cat("        controls of the same sex.\n")
    cat(strrep("=", 78), "\n", sep = "")
    
    .sh <- .decomp[.decomp$k == 0, ]
    cat("\n  AT THE BIRTH WAVE (k = 0):\n")
    print(.sh[, c("arm", "outcome", "mothers", "mothers_t",
                  "fathers", "fathers_t", "gap")], row.names = FALSE)
    
    # The share attributable to each side. Reported as a share of the SUM of
    # absolute movements, not of the gap: if the two move in opposite
    # directions a share of the gap can exceed one and reads as nonsense.
    cat("\n  Share of the k = 0 movement on each side:\n")
    for (.i in seq_len(nrow(.sh))) {
      .tot <- abs(.sh$mothers[.i]) + abs(.sh$fathers[.i])
      if (.tot > 0)
        cat(sprintf("    %-14s %-20s mothers %3.0f%%  fathers %3.0f%%\n",
                    .sh$arm[.i], .sh$outcome[.i],
                    100 * abs(.sh$mothers[.i]) / .tot,
                    100 * abs(.sh$fathers[.i]) / .tot))
    }
    
    # Post-birth horizons reaching significance, per side. A count is a
    # blunter instrument than the levels but it is the thing a reader
    # checks first, and it exposes the outcomes where BOTH sides move.
    cat("\n  Post-birth horizons with |t| >= 1.96, of those available:\n")
    for (.a in unique(.decomp$arm)) for (.o in unique(.decomp$outcome)) {
      .x <- .decomp[.decomp$arm == .a & .decomp$outcome == .o &
                      .decomp$k >= 0, ]
      if (!nrow(.x)) next
      cat(sprintf("    %-14s %-20s mothers %2d/%2d  fathers %2d/%2d%s\n",
                  .a, .o, sum(.x$mothers_t >= 1.96), nrow(.x),
                  sum(.x$fathers_t >= 1.96), nrow(.x),
                  if (sum(.x$fathers_t >= 1.96) >= 3)
                    "   <- BOTH sides move" else ""))
    }
    
    cat("\n  Written: training_decomposition.csv\n")
    cat("  THE AGGREGATE ATT IS NOT IN THE RDS. training_event_study.R\n")
    cat("  prints an 'overall post-birth ATT' with its SE for each of the 24\n")
    cat("  series and stores none of them; the object carries only\n")
    cat("  event_time/att/se. That aggregate is the natural headline under\n")
    cat("  Sun-Abraham and cannot be rebuilt from the horizons here, because\n")
    cat("  its SE needs the full covariance matrix. Store it there, the way\n")
    cat("  CS_event_study.R now stores its gap series.\n")
    rm(.sh)
  }
  
  rm(.tw, .cross, .t_sig, .w_sig)
  if (exists(".decomp")) rm(.decomp)
  if (exists(".cw_col")) rm(.cw_col)
  # .te is held from the reader above only so 8.2b can decompose the two
  # sexes. Removed here rather than left in the session, because a stale
  # event-study object surviving a run is how the run record came to print
  # five rows of "n/a" off a leftover `out`.
  if (exists(".te")) rm(.te)
} else {
  cat("\n  *** 8.2 CANNOT RUN. training_event_study_results.rds is absent or\n")
  cat("      lacks the named arm, so the training path has no source.\n")
  cat("      Section 8 has no exhibit and nothing in it should be drafted.\n")
  cat("      Run training_event_study.R first; this block reads its output,\n")
  cat("      it does not re-estimate the path. It will NOT fall back to\n")
  cat("      T2_profile_by_event_time.csv: that file is a reference-period\n")
  cat("      diagnostic, not an estimator, and cannot be set beside a\n")
  cat("      Sun-Abraham wage path.\n")
}
rm(.tim_train, .tim_wage, .wage_tab, .te_path)
if (exists(".tim_wage_age")) rm(.tim_wage_age)
if (exists(".timing")) rm(.timing)

# =============================================================================
# 8.3 NOT A GENDERED PRICE  (A5, promoted)
# =============================================================================
# PROMOTED, not demoted. The earlier plan had beta2 as two paragraphs ruling
# out an alternative. With the wage penalty back in the paper it does more
# work than that: it is what makes the penalty a QUANTITY story rather than a
# PRICE story, which is the premise the whole investment argument rests on.
# A section, with the sign-changing profile table in it.
#
# TWO FACTS CLOSE OFF THE PRICE INTERPRETATION. Across ten arms and five
# treatment constructions the gender difference in the return to experience
# is not distinguishable from zero, with MDEs stated. And at matched
# experience, mothers OUT-EARN never-mothers through most of the career:
# +0.21 log points at twelve years, still +0.09 at twenty-five, turning
# negative only in the top bin.
#
# The Oster bounds, the Heckman correction and the wave-by-wave series that
# used to surround this are still retired. A section is not a battery, and
# the quantity being defended here is a NULL with a stated MDE, which bounds
# do not help with.
# A5. INTERACTION: FATHERHOOD × β₂ IN POOLED MODEL

cat("\n\n--- A5. Pooled interaction: does fatherhood moderate β₂? ---\n")

# All men, pooled: educ_exp_c × ever_parent
# ---------------------------------------------------------------------------
# v21.33. THE TWO SEXES' beta2 ARE NOT COMPARABLE, AND CENTRING IS NOT
# WHY. I wrote the first version of this block believing the sex-specific
# centring of exp_c was the problem, and it is not.
#
# In a LINEAR-interaction model the cross-partial is a constant:
#   y = a + b*educ_c + c*exp_c + d*(educ_c * exp_c),  d2y/dEdX = d
# Shifting exp_c by any k changes educ_c*exp_c by k*educ_c, which the
# educ_c main effect absorbs, so d is invariant to the centre. Tested on
# a simulated surface with a genuinely varying cross-partial: re-centring
# moved the gap by 0.0%.
#
# WHERE THE CENTRE DOES MATTER is the quadratic in A5(ii):
#   ... + b1*(educ_c*exp_c) + b2*(educ_c*exp_c^2)
# whose cross-partial is b1 + 2*b2*(exp - centre), so b1 IS the value at
# the centre. That block is correct as written.
#
# SO THE REAL PROBLEM IS WEIGHTING. The linear beta2 is one constant
# forced on a surface where the cross-partial falls 3.3-fold across the
# experience range. Each sex's estimate is a different weighted average
# of that varying quantity, because the two have different experience
# DISTRIBUTIONS. No re-centring reaches that; only evaluating the
# quadratic at a COMMON experience level for both sexes does, which is
# what A5(ii) is extended to do below.
#
# The distribution difference is reported here because it is the size of
# the problem, and the common-centred columns are kept because they cost
# nothing and make the invariance checkable rather than asserted.
.exp_centre_pooled <- mean(wage$experience_years, na.rm = TRUE)
.exp_centre_m <- mean(wage$experience_years[wage$female == 0], na.rm = TRUE)
.exp_centre_w <- mean(wage$experience_years[wage$female == 1], na.rm = TRUE)
cat(sprintf(paste("\n  Experience centring: men %.2f years, women %.2f,",
                  "pooled %.2f.\n  The sex-specific beta2 below are the",
                  "cross-partial at %.2f and\n  %.2f years respectively,",
                  "which are NOT the same quantity.\n"),
            .exp_centre_m, .exp_centre_w, .exp_centre_pooled,
            .exp_centre_m, .exp_centre_w))
if (abs(.exp_centre_m - .exp_centre_w) > 1)
  cat(sprintf(paste("  *** %.2f YEARS APART. Quote the common-centred pair",
                    "for any\n      women-minus-men statement; the",
                    "sex-centred pair answers a\n      different question",
                    "for each sex.\n"),
              abs(.exp_centre_m - .exp_centre_w)))

men_all <- wage %>%
  filter(female == 0) %>%
  mutate(
    educ_c = educ_years - mean(educ_years, na.rm = TRUE),
    exp_c = experience_years - mean(experience_years, na.rm = TRUE),
    educ_exp_c = educ_c * exp_c,
    # v21.33: the common-centred twin, on the POOLED mean, so the two
    # sexes' interaction terms are evaluated at one experience level.
    exp_cc = experience_years - .exp_centre_pooled,
    educ_exp_cc = educ_c * exp_cc
  )

# OLS with interaction
# FIX: Removed state FE and weights to match baseline specification (Eq. 4).
# State × wave FE tested separately in robustness §5.13; weights in §5.2.
# v21.14h. THE ONE RESTRICTED COLUMN THIS SCRIPT USES, and the exact case
# the guard was written for.
#
# experience_years enters as a LEVEL here with wave fixed effects and no
# person effect. It is actual_experience wherever ehtjb resolves, so it
# carries the entry-cohort recall vintage as a person-constant term: the
# loader measures that gradient at one to two years at the same age, and
# 04 shows in_panel_at_w12 sitting above entry_w20_plus in every age band.
#
# The FE twin below (| person_id + wave) absorbs it and is fine. This one
# does not, so the vintage sits in the residual and is correlated with
# entry cohort, which is correlated with age, which is in the model.
#
# RESOLVED by controlling for entry_cohort, and here is why that and not
# one of the other two.
#
# This arm EXISTS to be the pooled counterpart to the FE arm twenty lines
# below. Adding person fixed effects would make the pair identical and
# destroy the comparison it is drawn for. Swapping to exp_cal_obs changes
# what the coefficient means, in-panel accumulation rather than a stock,
# and would need the FE twin changed to match or the pair stops being a
# pair.
#
# entry_cohort keeps the specification pooled and controls exactly the
# thing the guard objects to: the recall vintage is a property of the
# intake, 4.93 years out of the labour force for the original and 2011
# intakes against 1.72 for the 2019-plus intake. With wave fixed effects
# it is identified off within-wave variation in intake, which is what the
# refresher and the top-ups supply.
#
# The FE twin does NOT get it: entry_cohort is person-constant, so person
# fixed effects absorb it and adding it there would be collinear and
# dropped. The two arms therefore both control the vintage, one by
# covariate and one by fixed effect, which is the sense in which they
# remain comparable.
#
# THIS WILL MOVE THE COEFFICIENT. That is the point, and the size of the
# move is itself the finding: it is the first direct measurement of what
# the recall vintage was doing to beta2.
#
# ONE THING TO EXPECT IN THE OUTPUT. entry_cohort is not identified at
# wave 12, because every person observed there is in_panel_at_w12 by
# definition, so that wave contributes nothing to the cohort coefficients.
# Waves 13 to 19 carry two levels and 20 to 24 carry three. feols will
# report a rank deficiency or drop a level; that is the design, not a
# failure, and it means the cohort control is identified off waves 13
# onward. If the coefficient moves a lot, check whether it moves for that
# reason by re-running on waves 13-24 and comparing.
if (exists("assert_no_olf_level"))
  assert_no_olf_level(~ educ_years + educ_exp_c + experience_years,
                      context = "Eq. 4 baseline OLS, men",
                      allow_cohort_controlled = TRUE)

m_ols_int <- tryCatch(feols(
  ln_hourly_wage_real ~ educ_years + educ_exp_c * ever_parent +
    experience_years + age_sq + married + entry_cohort | wave,
  data = men_all, cluster = ~person_id,
  notes = FALSE
), error = function(e) NULL)

if (!is.null(m_ols_int) && "educ_exp_c:ever_parent" %in% names(coef(m_ols_int))) {
  s <- summary(m_ols_int)
  b_base <- coef(m_ols_int)["educ_exp_c"]
  b_int  <- coef(m_ols_int)["educ_exp_c:ever_parent"]
  p_int  <- s$coeftable["educ_exp_c:ever_parent", "Pr(>|t|)"]
  cat(sprintf("  OLS:  β₂(childless men) = %.4f%%\n", b_base*100))
  cat(sprintf("        β₂(fathers) = %.4f%% (= base %+.4f%%%s)\n",
              (b_base + b_int)*100, b_int*100, stars_fn(p_int)))
}

# FE with interaction (no weights — matches baseline spec)
# The FE twin. Person fixed effects DO absorb the person-constant recall
# vintage, so the declaration is true here and the guard acknowledges it
# rather than stopping. The acknowledgement now reaches the log, which it
# did not before v21.14h.
if (exists("assert_no_olf_level"))
  assert_no_olf_level(~ educ_exp_c + experience_years,
                      context = "Eq. 4 baseline FE, men",
                      allow_within_person = TRUE)

m_fe_int <- tryCatch(feols(
  ln_hourly_wage_real ~ educ_exp_c * ever_parent +
    experience_years + age_sq + married | person_id + wave,
  data = men_all, cluster = ~person_id,
  notes = FALSE
), error = function(e) NULL)

# v21.33: the same specification on the COMMON centre. Only the
# interaction's evaluation point differs, so a change between the two is
# the curvature and nothing else.
m_fe_cc <- tryCatch(feols(
  ln_hourly_wage_real ~ educ_exp_cc * ever_parent +
    experience_years + age_sq + married | person_id + wave,
  data = men_all, cluster = ~person_id, notes = FALSE
), error = function(e) NULL)

# ---------------------------------------------------------------------------
# v21.33 REPORT. The beta2 gender gap on both centrings.
#
# The two differ ONLY in where the interaction is evaluated, so the
# difference between the two gaps is curvature and nothing else. If they
# agree, the sex-specific centring was harmless here and the headline
# stands. If they differ materially, the published gap is partly a
# statement about where each sex sits on the experience distribution.
.b <- function(m, nm) tryCatch(unname(stats::coef(m)[[nm]]),
                               error = function(e) NA_real_)
.gap_own <- .b(w_fe_int, "educ_exp_c")  - .b(m_fe_int, "educ_exp_c")
.gap_cc  <- .b(w_fe_cc,  "educ_exp_cc") - .b(m_fe_cc,  "educ_exp_cc")
if (is.finite(.gap_own) || is.finite(.gap_cc)) {
  cat("\n--- beta2 gender gap, FE, on two centrings ---\n")
  cat(sprintf("  sex-specific centre : %+.4f%% (men at %.2f yrs, women at %.2f)\n",
              100 * .gap_own, .exp_centre_m, .exp_centre_w))
  cat(sprintf("  common centre       : %+.4f%% (both at %.2f yrs)\n",
              100 * .gap_cc, .exp_centre_pooled))
  if (is.finite(.gap_own) && is.finite(.gap_cc) && abs(.gap_own) > 1e-12) {
    .mv <- 100 * (.gap_cc - .gap_own) / abs(.gap_own)
    cat(sprintf("  difference: %+.2f%%.\n", .mv))
    if (abs(.mv) < 1) {
      cat("  As expected: in a linear-interaction model the cross-partial\n")
      cat("  is a constant and the centre cannot move it. This confirms\n")
      cat("  the invariance rather than testing the gap, and it means the\n")
      cat("  sex-specific centring is NOT the source of any comparability\n")
      cat("  problem. See A5(ii) for the one that is.\n")
    } else {
      cat("  *** UNEXPECTED. A linear interaction should be invariant to\n")
      cat("      the centre, so a difference here means the two fits are\n")
      cat("      not on the same rows: check complete cases on\n")
      cat("      educ_exp_c against educ_exp_cc before reading either.\n")
    }
  }
}

# ---------------------------------------------------------------------------
# v21.18. beta2 IS FORCED CONSTANT AND THE DATA SAY IT IS NOT.
#
# educ_exp_c is educ_c * exp_c, one product term, so the cross-partial
# d2(ln w)/d(educ)d(exp) is a single number by construction. The binned
# profiles in diag_wage_experience_by_educ.pdf show it is not: the
# education gap opens at roughly +0.027 log points per year of experience
# over the first decade, +0.016 to year 15, +0.003 to year 25, and turns
# slightly NEGATIVE after. All six education-by-sex cells are concave.
#
# So the reported beta2 is an average of a varying cross-partial, and the
# weights are the EXPERIENCE DISTRIBUTION. That distribution differs by
# sex, and section 2.9b of PAPER_ATTENTION.md puts 9 to 18 per cent of the
# women-minus-men experience gap at k = 0 down to reporting alone. A
# gender difference in a linear beta2 is therefore part price and part
# where each sex sits on a curve, and one term cannot separate them.
#
# THIS DOES NOT REPLACE THE LINEAR SPECIFICATION. The linear beta2 stays
# as the headline, because it is what the literature reports and what the
# ledger tracks. This runs beside it and answers one question: is the
# single number an adequate summary of the surface, or is it an average
# over a sign change?
#
# Reported as the cross-partial AT REPRESENTATIVE EXPERIENCE LEVELS, which
# is the only way a varying quantity can be read as numbers. Quadratic
# rather than splines so the term is interpretable and the marginal effect
# has a closed form: with educ_c*(exp_c + exp_c^2) the cross-partial at
# experience e is b1 + 2*b2*(e - exp_mean).
if (exists("assert_no_olf_level"))
  assert_no_olf_level(~ educ_exp_c + experience_years,
                      context = "Eq. 4 flexible beta2, men",
                      allow_within_person = TRUE)

.men_flex <- tryCatch({
  d <- men_all %>% mutate(exp_c2 = exp_c^2, educ_exp_c2 = educ_c * exp_c2)
  feols(ln_hourly_wage_real ~ educ_exp_c + educ_exp_c2 +
          experience_years + age_sq + married | person_id + wave,
        data = d, cluster = ~person_id, notes = FALSE)
}, error = function(e) {
  cat("  [A5(ii) men] flexible fit FAILED: ", conditionMessage(e), "\n",
      sep = "")
  NULL
})

if (!is.null(.men_flex) &&
    all(c("educ_exp_c", "educ_exp_c2") %in% names(coef(.men_flex)))) {
  .b1 <- coef(.men_flex)[["educ_exp_c"]]
  .b2 <- coef(.men_flex)[["educ_exp_c2"]]
  .em <- mean(men_all$experience_years, na.rm = TRUE)
  cat("\n--- A5(ii). beta2 allowed to vary with experience (men, FE) ---\n")
  cat(sprintf("  linear term %+.6f, quadratic term %+.6f, mean experience %.1f\n",
              .b1, .b2, .em))
  cat("  Cross-partial at representative experience levels, x100:\n")
  for (.e in c(5, 10, 15, 20, 25, 30))
    cat(sprintf("    %2d years: %+.4f\n", .e,
                100 * (.b1 + 2 * .b2 * (.e - .em))))
  .lo <- 100 * (.b1 + 2 * .b2 * (5  - .em))
  .hi <- 100 * (.b1 + 2 * .b2 * (30 - .em))
  cat(sprintf("\n  Range across 5 to 30 years: %+.4f to %+.4f.\n", .lo, .hi))
  # v21.18b CORRECTION. The first version compared the range against the
  # value at 5 YEARS, which is the largest value on a declining curve, so
  # a cross-partial falling to a third of its starting value printed as
  # "stable". On the v21.18 run it read +0.0950 at 5 years and +0.0287 at
  # 30, a 3.3-fold fall, and the block called that adequate.
  #
  # Third time in this project a verdict has been read off the wrong
  # denominator: the ratio band in 04's sex-split, the whole-window median
  # in Part 3's DiD, and this. A ratio between the extremes is the number,
  # and it needs no reference point at all.
  .ratio <- if (sign(.lo) == sign(.hi) && min(abs(c(.lo, .hi))) > 0)
    max(abs(c(.lo, .hi))) / min(abs(c(.lo, .hi))) else Inf
  cat(sprintf("  Ratio of largest to smallest across the range: %.1fx.\n",
              .ratio))
  if (sign(.lo) != sign(.hi)) {
    cat("  *** THE CROSS-PARTIAL CHANGES SIGN over the observed experience\n",
        "      range. A single linear beta2 is an average over a sign\n",
        "      change and its value depends entirely on the experience\n",
        "      distribution. Report the profile.\n", sep = "")
  } else if (.ratio > 1.5) {
    cat("  *** NOT ADEQUATELY SUMMARISED BY ONE NUMBER. The linear beta2\n",
        "      equals the cross-partial AT MEAN EXPERIENCE, because exp_c\n",
        "      is centred. So comparing beta2 across groups with different\n",
        "      mean experience compares different points on this curve,\n",
        "      and women hold less experience than men at every age.\n",
        "      Report beta2 at a COMMON experience level for both sexes,\n",
        "      or reweight to a common distribution, and say which.\n",
        sep = "")
    cat(sprintf(paste("      For scale: moving the evaluation point from",
                      "%.1f to %.1f years\n      changes the cross-partial",
                      "by %.0f%%.\n"), .em, .em - 3,
                100 * abs(2 * .b2 * 3 / (.b1 + 2 * .b2 * 0))))
  } else {
    cat("  Within 1.5x across the range, so the linear summary is adequate\n",
        "  and this block can be dropped.\n", sep = "")
  }
  rm(.ratio)
  rm(.b1, .b2, .em, .lo, .hi)
} else {
  cat("\n  [skip] A5(ii): the flexible fit did not converge or the terms\n")
  cat("  were dropped, most likely collinearity between exp_c and exp_c^2\n")
  cat("  after person demeaning. The linear beta2 above stands alone.\n")
}

if (!is.null(m_fe_int) && "educ_exp_c:ever_parent" %in% names(coef(m_fe_int))) {
  s <- summary(m_fe_int)
  b_base <- coef(m_fe_int)["educ_exp_c"]
  b_int  <- coef(m_fe_int)["educ_exp_c:ever_parent"]
  p_int  <- s$coeftable["educ_exp_c:ever_parent", "Pr(>|t|)"]
  cat(sprintf("  FE:   β₂(childless men) = %.4f%%\n", b_base*100))
  cat(sprintf("        β₂(fathers) = %.4f%% (= base %+.4f%%%s)\n",
              (b_base + b_int)*100, b_int*100, stars_fn(p_int)))
  cat(sprintf("        p-value for fatherhood × β₂: %.4f\n", p_int))
}

# Same for women
women_all <- wage %>%
  filter(female == 1) %>%
  mutate(
    educ_c = educ_years - mean(educ_years, na.rm = TRUE),
    exp_c = experience_years - mean(experience_years, na.rm = TRUE),
    educ_exp_c = educ_c * exp_c,
    exp_cc = experience_years - .exp_centre_pooled,   # v21.33
    educ_exp_cc = educ_c * exp_cc
  )

# ---------------------------------------------------------------------------
# v21.34. MOVED, and the move IS the fix.
#
# This block sat 62 lines ABOVE the line that builds women_all, so
# .women_flex was fitted against an object that did not yet exist.
# tryCatch returned NULL and A5(iii) reported "one of the two flexible
# fits is unavailable" — a message that reads as a data limitation and is
# in fact an ordering bug. The specification was never wrong.
#
# The tryCatch now REPORTS the condition rather than swallowing it. A
# silent NULL is what let a missing object look like a finding, and it
# cost a full master run to discover.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# v21.33b. THE SAME FLEXIBLE FIT FOR WOMEN, AND THE GAP AT A COMMON
# EXPERIENCE LEVEL. This is section 4.2's instruction implemented.
#
# The linear beta2 is one constant standing in for a cross-partial that
# falls 3.3-fold, and each sex's constant is a different weighted average
# of it because the two experience distributions differ. The quadratic
# gives the cross-partial as a function, so both sexes can be evaluated at
# ONE experience level and the difference is a price difference rather
# than a difference in where each sex sits.
.women_flex <- tryCatch({
  d <- women_all %>% mutate(exp_c2 = exp_c^2, educ_exp_c2 = educ_c * exp_c2)
  feols(ln_hourly_wage_real ~ educ_exp_c + educ_exp_c2 +
          experience_years + age_sq + married | person_id + wave,
        data = d, cluster = ~person_id, notes = FALSE)
}, error = function(e) {
  cat("  [A5(ii) women] flexible fit FAILED: ", conditionMessage(e), "\n",
      sep = "")
  NULL
})

# v28. THE PROFILE NOW CARRIES ITS UNCERTAINTY, and the reason it did not
# is that nothing here ever asked the fit for one.
#
# g(e) = b1 + 2*b2*(e - centre) is a linear function of two estimated
# coefficients, so its variance has a closed form and the fit already
# holds every term:
#
#   let d = 2*(e - centre)
#   Var(g) = Var(b1) + d^2 Var(b2) + 2 d Cov(b1, b2)
#
# WITHOUT IT THE TABLE CANNOT BE READ. app_b2_profile.tex prints six
# point estimates and no errors, and a reader has no way to tell a
# profile that falls 3.3-fold from one that is flat with wide bands. The
# referee response turns on exactly that distinction. On the runs to date
# the largest |t| on the gap is under 0.5, so the honest reading is that
# Section 7.3 keeps its present wording; that is a finding rather than a
# gap in the table, but only once the column exists.
#
# THE GAP ADDS THE VARIANCES because the two fits are on disjoint
# persons, which is this file's convention for every sex-specific
# contrast. It is the one step here that assumes rather than computes.
#
# SIX ROWS ARE TWO COEFFICIENTS. The profile is linear in e by
# construction, so the six evaluated horizons are b1 and b2 read at six
# places, not six estimates, and the standard errors are U-shaped with a
# minimum at the centre for the same reason. The coefficient frame
# written below is what paper_tables.R should print alongside the
# profile, so the table stops implying six independent numbers.
.xp2 <- function(m, centre, e) {
  b  <- tryCatch(stats::coef(m),  error = function(x) NULL)
  V  <- tryCatch(stats::vcov(m),  error = function(x) NULL)
  nm <- c("educ_exp_c", "educ_exp_c2")
  if (is.null(b) || is.null(V) || !all(nm %in% names(b)) ||
      !all(nm %in% rownames(V)))
    return(c(est = NA_real_, se = NA_real_))
  b1 <- unname(b[[nm[1]]]); b2 <- unname(b[[nm[2]]])
  if (!is.finite(b1) || !is.finite(b2))
    return(c(est = NA_real_, se = NA_real_))
  d  <- 2 * (e - centre)
  vv <- V[nm[1], nm[1]] + d^2 * V[nm[2], nm[2]] + 2 * d * V[nm[1], nm[2]]
  c(est = b1 + 2 * b2 * (e - centre),
    se  = if (is.finite(vv) && vv > 0) sqrt(vv) else NA_real_)
}
# Kept so the existing callers below read unchanged. A point estimate is
# still a point estimate; this is the same number .xp2 returns first.
.xp <- function(m, centre, e) unname(.xp2(m, centre, e)[["est"]])
if (!is.null(.men_flex) && !is.null(.women_flex)) {
  cat("\n--- A5(iii). beta2 gender gap AT A COMMON EXPERIENCE LEVEL ---\n")
  cat("  Each sex's cross-partial from its own quadratic, evaluated at the\n")
  cat("  same experience. The linear gap above is each sex's own weighted\n")
  cat("  average of these and is not a contrast at any single level.\n\n")
  cat("  Standard errors are delta-method on each sex's own quadratic;\n")
  cat("  the gap adds the two variances because the fits are on disjoint\n")
  cat("  persons. Units are percentage points of log wage per year of\n")
  cat("  education per year of experience, x100 as above.\n\n")
  cat(sprintf("  %5s  %9s %8s  %9s %8s  %9s %8s %7s %3s\n",
              "years", "men", "(se)", "women", "(se)", "gap", "(se)",
              "t", "sig"))
  .A5_PROFILE <- NULL
  for (.e in c(5, 10, 15, 20, 25, 30)) {
    .m2 <- .xp2(.men_flex,   .exp_centre_m, .e)
    .w2 <- .xp2(.women_flex, .exp_centre_w, .e)
    .hm <- unname(.m2[["est"]]); .sm <- unname(.m2[["se"]])
    .hw <- unname(.w2[["est"]]); .sw <- unname(.w2[["se"]])
    if (!is.finite(.hm) || !is.finite(.hw)) next
    .gp <- .hw - .hm
    .sg <- if (is.finite(.sm) && is.finite(.sw)) sqrt(.sm^2 + .sw^2) else NA_real_
    .tt <- if (is.finite(.sg) && .sg > 0) .gp / .sg else NA_real_
    cat(sprintf("  %5d  %+9.4f %8s  %+9.4f %8s  %+9.4f %8s %7s %3s\n", .e,
                100 * .hm, if (is.finite(.sm)) sprintf("(%.4f)", 100 * .sm) else "(NA)",
                100 * .hw, if (is.finite(.sw)) sprintf("(%.4f)", 100 * .sw) else "(NA)",
                100 * .gp, if (is.finite(.sg)) sprintf("(%.4f)", 100 * .sg) else "(NA)",
                if (is.finite(.tt)) sprintf("%.2f", .tt) else "NA",
                if (is.finite(.tt) && abs(.tt) >= 1.96) "*" else ""))
    .A5_PROFILE <- rbind(.A5_PROFILE, data.frame(
      years = .e,
      men = 100 * .hm, se_men = 100 * .sm,
      women = 100 * .hw, se_women = 100 * .sw,
      gap = 100 * .gp, se_gap = 100 * .sg, t_gap = .tt,
      stringsAsFactors = FALSE))
  }

  # WHAT THE COLUMN IS FOR, said once. A blank sig column means Section
  # 7.3 keeps its present wording: no horizon shows women's return
  # differing from men's, and the paper may say that nothing runs against
  # it but not that women's exceeds men's anywhere. A starred early
  # horizon licenses the stronger sentence.
  if (!is.null(.A5_PROFILE) && any(is.finite(.A5_PROFILE$t_gap))) {
    .imax <- which.max(abs(.A5_PROFILE$t_gap))
    cat(sprintf("\n  Largest |t| on the gap: %.2f at %d years.\n",
                abs(.A5_PROFILE$t_gap[.imax]), .A5_PROFILE$years[.imax]))
    if (all(abs(.A5_PROFILE$t_gap) < 1.96, na.rm = TRUE))
      cat("  No horizon is distinguishable. Section 7.3 keeps its wording:\n",
          "  nothing runs against the null, and that is not the same as\n",
          "  women's return exceeding men's anywhere.\n", sep = "")
    else
      cat("  At least one horizon is signed; Section 7.3 can say so, at the\n",
          "  experience level where it holds and not in general.\n", sep = "")
  }

  # THE TWO COEFFICIENTS BEHIND THE SIX ROWS. Written so paper_tables.R
  # can print them beside the profile; six rows generated by two
  # parameters per sex should not be presented as six estimates.
  .A5_COEFS <- NULL
  for (.sx in c("men", "women")) {
    .mm <- if (.sx == "men") .men_flex else .women_flex
    .cc <- tryCatch(stats::coef(.mm), error = function(e) NULL)
    .VV <- tryCatch(stats::vcov(.mm), error = function(e) NULL)
    if (is.null(.cc) || is.null(.VV)) next
    .A5_COEFS <- rbind(.A5_COEFS, data.frame(
      sex = .sx,
      centre = if (.sx == "men") .exp_centre_m else .exp_centre_w,
      b1 = unname(.cc[["educ_exp_c"]]),
      b2 = unname(.cc[["educ_exp_c2"]]),
      se_b1 = sqrt(.VV["educ_exp_c", "educ_exp_c"]),
      se_b2 = sqrt(.VV["educ_exp_c2", "educ_exp_c2"]),
      cov_b1b2 = .VV["educ_exp_c", "educ_exp_c2"],
      slope_per_year = 100 * 2 * unname(.cc[["educ_exp_c2"]]),
      n = tryCatch(fixest::fitstat(.mm, "n")$n, error = function(e) NA_real_),
      stringsAsFactors = FALSE))
  }
  if (!is.null(.A5_PROFILE)) {
    write.csv(.A5_PROFILE, .out_path("A5_crosspartial_profile.csv"),
              row.names = FALSE)
    cat("\n  Written: A5_crosspartial_profile.csv  (app_b2_profile.tex's data)\n")
  }
  if (!is.null(.A5_COEFS)) {
    write.csv(.A5_COEFS, .out_path("A5_crosspartial_coefs.csv"),
              row.names = FALSE)
    cat("  Written: A5_crosspartial_coefs.csv    (the two coefficients)\n")
  }
  .g5  <- 100 * (.xp(.women_flex, .exp_centre_w, 5)  -
                   .xp(.men_flex, .exp_centre_m, 5))
  .g30 <- 100 * (.xp(.women_flex, .exp_centre_w, 30) -
                   .xp(.men_flex, .exp_centre_m, 30))
  # v28b. THE TILT, TESTED RATHER THAN ASSERTED.
  #
  # The block used to print IT CHANGES SIGN whenever the two endpoints had
  # opposite signs, which is a statement about two point estimates and was
  # the only claim here not informed by an error once the profile gained
  # one. It also asserted the most: "report the profile" is an instruction
  # about what the paper prints.
  #
  # The gap is linear in e, so its whole tilt is one number: the
  # difference in the two quadratic terms. gap(e) = (b1w - b1m) +
  # 2(b2w - b2m)(e - c) up to the centring, so
  #
  #   d gap / d e = 2 (b2w - b2m),  Var = 4 (Var(b2w) + Var(b2m))
  #
  # disjoint persons again. A sign change with |t| below 2 on that slope
  # is two point estimates that could as easily have crossed the other
  # way, and the crossing point is then not identified either.
  .slope <- function(m) {
    b <- tryCatch(stats::coef(m), error = function(e) NULL)
    V <- tryCatch(stats::vcov(m), error = function(e) NULL)
    if (is.null(b) || is.null(V) || !"educ_exp_c2" %in% names(b))
      return(c(NA_real_, NA_real_))
    c(2 * unname(b[["educ_exp_c2"]]),
      V["educ_exp_c2", "educ_exp_c2"])
  }
  .sm2 <- .slope(.men_flex); .sw2 <- .slope(.women_flex)
  .d_slope <- 100 * (.sw2[1] - .sm2[1])
  .d_slope_se <- 100 * 2 * sqrt(.sw2[2] + .sm2[2])
  .d_t <- if (is.finite(.d_slope_se) && .d_slope_se > 0)
    .d_slope / .d_slope_se else NA_real_

  if (is.finite(.g5) && is.finite(.g30)) {
    cat(sprintf("\n  The gap itself runs %+.4f at 5 years to %+.4f at 30.\n",
                .g5, .g30))
    if (is.finite(.d_t)) {
      cat(sprintf(paste0("  Its tilt is one parameter: women's cross-partial",
                         " falls %+.5f per year\n  against men's %+.5f, a",
                         " difference of %+.5f (se %.5f, t %.2f).\n"),
                  100 * .sw2[1], 100 * .sm2[1], .d_slope, .d_slope_se, .d_t))
      cat(sprintf("  Across 5 to 30 years that is a change of %+.4f (se %.4f).\n",
                  25 * .d_slope, 25 * .d_slope_se))
    }
    if (sign(.g5) != sign(.g30) && is.finite(.d_t) && abs(.d_t) >= 1.96) {
      cat("  *** IT CHANGES SIGN, and the tilt that carries it is\n",
          "      distinguishable from zero. A single women-minus-men beta2\n",
          "      is then an average over a sign change and its value depends\n",
          "      on the experience distribution. Report the profile.\n",
          sep = "")
    } else if (sign(.g5) != sign(.g30)) {
      .cross <- if (is.finite(.d_slope) && .d_slope != 0)
        5 - .g5 / (.d_slope) else NA_real_
      cat("  The point estimates cross, and the tilt carrying them across is\n")
      cat(sprintf("  not distinguishable from zero (t %.2f). A flat gap fits\n",
                  .d_t))
      cat("  the profile as well as a crossing one, so the crossing point\n")
      if (is.finite(.cross))
        cat(sprintf("  near %.1f years is a property of the point estimates and\n",
                    .cross))
      cat("  not a horizon the design locates. Report the profile with its\n")
      cat("  errors and do not describe the sign change as a finding.\n")
      rm(.cross)
    } else {
      cat("  Same sign throughout, so the direction of the gap is not an\n",
          "  artefact of where it is evaluated. Report it at a stated\n",
          "  experience level and give the range.\n", sep = "")
    }
  }
  if (!is.null(.A5_COEFS) && is.finite(.d_t)) {
    .A5_COEFS$gap_slope_per_year <- .d_slope
    .A5_COEFS$gap_slope_se       <- .d_slope_se
    .A5_COEFS$gap_slope_t        <- .d_t
    write.csv(.A5_COEFS, .out_path("A5_crosspartial_coefs.csv"),
              row.names = FALSE)
  }
  rm(.slope, .sm2, .sw2, .d_slope, .d_slope_se, .d_t)
  rm(.g5, .g30)
  .mpn("mpn_A5", .A5_PROFILE, .A5_COEFS)
  rm(.A5_PROFILE, .A5_COEFS)
} else {
  cat("\n  [skip] A5(iii): ",
      if (is.null(.men_flex) && is.null(.women_flex)) "BOTH flexible fits are"
      else if (is.null(.men_flex)) "the MEN'S flexible fit is"
      else "the WOMEN'S flexible fit is",
      " unavailable, so the\n  gap cannot be evaluated at a common ",
      "experience level.\n", sep = "")
  cat("  Naming which one matters: a fit that failed for a DATA reason\n")
  cat("  and a fit that ran before its sample existed produce the same\n")
  cat("  NULL, and only the second is a bug in this file.\n")
}
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

cat("\n  WOMEN:\n")

w_ols_int <- tryCatch(feols(
  ln_hourly_wage_real ~ educ_years + educ_exp_c * ever_parent +
    experience_years + age_sq + married | wave,
  data = women_all, cluster = ~person_id,
  notes = FALSE
), error = function(e) NULL)

if (!is.null(w_ols_int) && "educ_exp_c:ever_parent" %in% names(coef(w_ols_int))) {
  s <- summary(w_ols_int)
  b_base <- coef(w_ols_int)["educ_exp_c"]
  b_int  <- coef(w_ols_int)["educ_exp_c:ever_parent"]
  p_int  <- s$coeftable["educ_exp_c:ever_parent", "Pr(>|t|)"]
  cat(sprintf("  OLS:  β₂(never-mothers) = %.4f%%\n", b_base*100))
  cat(sprintf("        β₂(mothers) = %.4f%% (= base %+.4f%%%s)\n",
              (b_base + b_int)*100, b_int*100, stars_fn(p_int)))
}

w_fe_cc <- tryCatch(feols(
  ln_hourly_wage_real ~ educ_exp_cc * ever_parent +
    experience_years + age_sq + married | person_id + wave,
  data = women_all, cluster = ~person_id, notes = FALSE
), error = function(e) NULL)

w_fe_int <- tryCatch(feols(
  ln_hourly_wage_real ~ educ_exp_c * ever_parent +
    experience_years + age_sq + married | person_id + wave,
  data = women_all, cluster = ~person_id,
  notes = FALSE
), error = function(e) NULL)

if (!is.null(w_fe_int) && "educ_exp_c:ever_parent" %in% names(coef(w_fe_int))) {
  s <- summary(w_fe_int)
  b_base <- coef(w_fe_int)["educ_exp_c"]
  b_int  <- coef(w_fe_int)["educ_exp_c:ever_parent"]
  p_int  <- s$coeftable["educ_exp_c:ever_parent", "Pr(>|t|)"]
  cat(sprintf("  FE:   β₂(never-mothers) = %.4f%%\n", b_base*100))
  cat(sprintf("        β₂(mothers) = %.4f%% (= base %+.4f%%%s)\n",
              (b_base + b_int)*100, b_int*100, stars_fn(p_int)))
  cat(sprintf("        p-value for motherhood × β₂: %.4f\n", p_int))
}


# =============================================================================
# PART 5: WHAT THIS IS NOT  (paper section 9)
# =============================================================================
#   9.1  Not anticipation                        <- T7 null, in PART 2's module
#   9.2  Not reallocation across training types  <- T3b, wrong sign
#   9.3  Not hours alone                         <- the open one, below
#
# 9.1 and 9.2 are estimated inside 03training_designs.R and
# training_funding_divergence.R and are named in the ledger at PART 6. Only
# 9.3 has code here.

# =============================================================================
# 9.3 NOT HOURS ALONE
# =============================================================================
# CORRECTION, AND IT NARROWS WHAT THIS BLOCK CLAIMS TO ADD. The outline says
# a specification holding hours fixed "has not been run", and an earlier
# version of this header repeated that. IT IS NOT TRUE. T4 in
# 03training_designs.R is exactly that specification and carries PREFERRED
# status:
#
#   T4_any   full time at ANY wave in k = +1..+5   n = 46,512  SE 0.0191
#            1,567 persons, 538 mothers
#   T4_all   full time at ALL waves in k = +1..+3  n = 40,333  SE 0.0282
#            894 persons, 155 mothers
#
# So the hours alternative was not unaddressed. What is missing is narrower
# and it is the part that decides the argument.
# THE ALTERNATIVE THE TIMING ARGUMENT CANNOT DISMISS. A mother working part
# time trains less AND accumulates less experience, so training and wages
# would move in exactly that order with no employer decision anywhere in the
# story. The ordering in 8.2 is consistent with the employer-decision reading
# and equally consistent with this one.
#
# The employed-only arm (-0.116) and the within-couple arm (-0.099) narrow it,
# since neither is driven by leaving work altogether, but neither conditions
# on full-time status at all.
#
# WHAT T4 DOES AND DOES NOT SETTLE. Both T4 rules select on full-time status
# AFTER the birth: k = +1..+5 and k = +1..+3. That is selection on an
# outcome. Mothers observed back on full-time hours are not a random
# subsample of mothers; they are the ones for whom returning was feasible,
# and whatever made it feasible can also bear on training. T4's own log line
# - "If the coefficient survives here, the part-time composition story is
# closed" - is therefore stronger than the design supports. Survival in T4 is
# evidence, not closure.
#
# THE MISSING ARM IS THE PRE-DETERMINED ONE. Full-time status BEFORE the
# birth is fixed before treatment, so restricting on it is conditioning
# rather than selection, and it answers the referee's question without the
# post-treatment problem. That is arm E below and it is what this block adds.
#
# -----------------------------------------------------------------------------
# A WARNING THE PLAN DOES NOT CARRY, and it changes which arm to believe.
#
# HOURS ARE AN OUTCOME OF THE BIRTH. Conditioning on a variable that the
# treatment moves is a bad control in the Angrist-Pischke sense: it blocks
# part of the causal path and can induce selection bias with no sign
# guarantee. So "add fulltime as a control" is NOT a clean test, and if arm B
# below attenuates the gap that is not evidence that hours explain it.
#
# The distinction matters enough to run five arms rather than three:
#
#   A  baseline, employed only            the benchmark, should land near -0.116
#   B  + fulltime_num                     BAD CONTROL. post-treatment.
#   C  + hours_worked_clean               BAD CONTROL. post-treatment.
#   D  continuously full time across      selected ON a post-treatment
#      the birth, k = -2..+2               outcome. This is T4's design on a
#                                          SYMMETRIC window, reported so the
#                                          T4 result has a like-for-like
#                                          neighbour rather than being
#                                          silently duplicated.
#   E  full time BEFORE the birth,        PRE-DETERMINED. NOW ALSO T4c.
#      k = -3..-1                          See the note below.
#
# ARM E AND T4c ARE THE SAME ESTIMATE, and that is a problem to resolve
# rather than a redundancy to tolerate. The v23 edit added T4c to
# 03training_designs.R with the identical rule - full time at every observed
# wave in k = -3..-1, at least two waves - because it belongs with the blind
# ledger: it gets report(), an MDE graded against the same benchmark, a
# status, and a line in the same log as the arms it has to be read beside.
#
# T4c IS AUTHORITATIVE. Arm E is retained as a CROSS-CHECK, because the two
# reach the same quantity through different code on possibly different
# frames, and a disagreement between them is worth catching. If they differ
# by more than rounding, the frames differ, and that is the finding. Report
# T4c.
#
# D IS NOT A NEW RESULT and should not be presented as one. T4_all already
# runs the post-birth restriction; D differs only in taking a window
# symmetric about the birth so that it and E use the same amount of
# information on either side. If D and T4_all disagree materially, the window
# is doing the work and both need reporting.
#
# ARM E IS THE ONE TO REPORT, and section 9.3 should cite T4 beside it rather
# than instead of it. The pair is the argument: T4 shows the gap survives
# among mothers who returned to full time, E shows it survives among mothers
# who were full time going in, and only the second is free of selection on an
# outcome. Two restrictions failing to kill the coefficient for different
# reasons is worth more than either alone.
#
# B and C are reported because the plan asks for them and because their
# distance from A measures how much of the raw gap runs through hours
# arithmetically. They are not the test.
#
# WHAT EACH ANSWER MEANS. If E survives, the employer-decision reading is
# much harder to dislodge. If E collapses, the honest claim narrows to
# "employers invest less in mothers, and the reduction in hours is why" -
# which is still a result, and still needs the funding split to establish,
# because without it you cannot tell whose decision the hours reduction
# reflects.
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 78), "\n", sep = "")
cat("  9.3  NOT HOURS ALONE: the training gap holding hours fixed\n")
cat(strrep("=", 78), "\n", sep = "")

.hrs_ok <- requireNamespace("fixest", quietly = TRUE)
if (!.hrs_ok) cat("  fixest unavailable; 9.3 skipped.\n")

# Reuse the event frame 03training_designs.R built if it is still in scope,
# so the arms below sit on the SAME sample as the published T-series. If it
# is not, rebuild it to the same recipe rather than silently using a
# different one: cohort = first birth wave, k = wave - cohort, post = k >= 0,
# never-treated coded 10000 so they contribute to the wave effects.
.ev <- NULL
# ev now comes from the registry, not from whatever a sourced script left
# in the global environment. td_require() at the head of this file has
# already failed if it is absent, so this cannot silently proceed on NULL.
.ev <- td_get("ev")
cat("  event frame: from the handoff registry (03_common.R)\n")
# THE FALLBACK IS GONE, DELIBERATELY. It rebuilt ev "to the T-series
# recipe" when ev was absent - a second implementation of the same frame,
# maintained separately, and used exactly when something upstream had
# already gone wrong. Two recipes that can drift is worse than one failure
# that stops. td_require() above now fails with the name of what is
# missing, before any of this runs.

.OUT <- "jttpewt_any"     # employer-funded: employer's place, working hours
if (.hrs_ok && !is.null(.ev) && .OUT %in% names(.ev)) {
  
  # Same FE block as the T-series so the arms are comparable to the published
  # numbers rather than to each other only.
  .fe <- if (exists("fe_block") && is.function(fe_block)) fe_block() else
    "person_id + wave^female"
  cat("  outcome:", .OUT, "| FE:", .fe, "| cluster: person_id\n")
  
  .fit <- function(dat, rhs = NULL, label = "") {
    .f <- stats::as.formula(paste0(.OUT, " ~ post + post_f",
                                   if (length(rhs)) paste0(" + ", paste(rhs, collapse = " + ")) else "",
                                   " | ", .fe))
    m <- tryCatch(fixest::feols(.f, data = dat, cluster = ~person_id,
                                notes = FALSE, warn = FALSE),
                  error = function(e) NULL)
    if (is.null(m)) return(data.frame(arm = label, est = NA_real_, se = NA_real_,
                                      n = nrow(dat), persons = NA_integer_))
    ct <- fixest::coeftable(m)
    if (!"post_f" %in% rownames(ct))
      return(data.frame(arm = label, est = NA_real_, se = NA_real_,
                        n = nrow(dat), persons = NA_integer_))
    data.frame(arm = label,
               est = unname(ct["post_f", 1]), se = unname(ct["post_f", 2]),
               # NOT fixest::nobs. fixest does not EXPORT nobs; it
               # registers an S3 METHOD for stats::nobs, so `fixest::nobs`
               # raises "'nobs' is not an exported object" and killed the
               # v23 master run at 9.3 after everything above it had
               # succeeded. Dispatch through the generic instead.
               n = stats::nobs(m),
               persons = dplyr::n_distinct(dat$person_id))
  }
  
  .emp <- .ev %>% dplyr::filter(employed == 1, !is.na(.data[[.OUT]]))
  
  # ---- D: continuously full time across the birth --------------------------
  # Full time at every observed wave in [-2, +2] for the treated. Selected on
  # a post-treatment outcome, so read it as "among women who did not reduce
  # hours" and not as a causal contrast.
  .cont_ft <- character(0)
  if ("fulltime_num" %in% names(.emp)) {
    .w <- .emp %>% dplyr::filter(treated == 1, !is.na(k), k >= -2, k <= 2,
                                 !is.na(fulltime_num))
    .cont_ft <- .w %>% dplyr::group_by(person_id) %>%
      dplyr::summarise(all_ft = all(fulltime_num == 1), nk = dplyr::n(),
                       .groups = "drop") %>%
      dplyr::filter(all_ft, nk >= 3) %>% dplyr::pull(person_id)
    rm(.w)
  }
  
  # ---- E: full time BEFORE the birth (pre-determined) ----------------------
  # Full time at every observed wave in [-3, -1]. Fixed before treatment, so
  # this is conditioning and not bad control. This is the reportable arm.
  .pre_ft <- character(0)
  if ("fulltime_num" %in% names(.emp)) {
    .w <- .emp %>% dplyr::filter(treated == 1, !is.na(k), k >= -3, k <= -1,
                                 !is.na(fulltime_num))
    .pre_ft <- .w %>% dplyr::group_by(person_id) %>%
      dplyr::summarise(all_ft = all(fulltime_num == 1), nk = dplyr::n(),
                       .groups = "drop") %>%
      dplyr::filter(all_ft, nk >= 2) %>% dplyr::pull(person_id)
    rm(.w)
  }
  
  # ---- what T4c said, if the designs module has run -----------------
  # Read from the blind ledger rather than re-derived, so the comparison is
  # against the number that will be reported and not a second estimate of it.
  # v24.5. FALL BACK TO THE FILE when .blind is not in memory, which is the
  # normal case now that PART 2 is skipped by default: 03training_designs.R
  # writes the same list to BLIND_training_estimates.rds on its way out, so
  # the cross-check does not have to be lost merely because the module was
  # not re-sourced.
  .t4c <- NULL
  .blind_src <- if (exists(".blind") && is.list(.blind)) .blind else {
    .bp <- file.path(if (!is.na(TD_OUTDIR)) TD_OUTDIR else ".",
                     "BLIND_training_estimates.rds")
    if (file.exists(.bp)) tryCatch(readRDS(.bp), error = function(e) NULL) else NULL
  }
  if (is.list(.blind_src) && "T4c_pre_birth_ft" %in% names(.blind_src)) {
    .ct <- .blind_src[["T4c_pre_birth_ft"]]
    if (!is.null(.ct) && "post_f" %in% rownames(.ct))
      .t4c <- c(est = unname(.ct["post_f", 1]), se = unname(.ct["post_f", 2]))
    rm(.ct)
  }
  rm(.blind_src)
  if (exists(".bp")) rm(.bp)
  if (!is.null(.t4c))
    cat(sprintf("  T4c (authoritative pre-birth arm): %+.4f (SE %.4f)\n",
                .t4c["est"], .t4c["se"]))
  else
    cat("  T4c not in the blind ledger; arm E below is the only pre-birth\n  estimate in this run.\n")
  
  .arms <- list()
  .arms[[1]] <- .fit(.emp, NULL, "A baseline (employed only)")
  if ("fulltime_num" %in% names(.emp))
    .arms[[2]] <- .fit(.emp, "fulltime_num", "B + fulltime  [BAD CONTROL]")
  if ("hours_worked_clean" %in% names(.emp))
    .arms[[3]] <- .fit(.emp, "hours_worked_clean", "C + hours     [BAD CONTROL]")
  if (length(.cont_ft) > 30)
    .arms[[4]] <- .fit(.emp %>% dplyr::filter(treated == 0 |
                                                person_id %in% .cont_ft),
                       NULL, "D continuously FT across birth")
  if (length(.pre_ft) > 30)
    .arms[[5]] <- .fit(.emp %>% dplyr::filter(treated == 0 |
                                                person_id %in% .pre_ft),
                       NULL, "E FT before birth [PRE-DETERMINED]")
  
  .hrs <- do.call(rbind, .arms)
  if (!is.null(.hrs) && nrow(.hrs)) {
    .hrs$t   <- round(.hrs$est / .hrs$se, 2)
    .hrs$mde <- if (exists("mde") && is.function(mde)) round(mde(.hrs$se), 4) else
      round(2.802 * .hrs$se, 4)     # 80% power, 5% two-sided
    # v26. THE STORED ESTIMATE IS UNROUNDED. It was rounded to four decimals
    # here, before write.csv, and that is why Check 6 reports arm B
    # UNDECIDABLE rather than reconciled: a stored -0.0775 stands for
    # anything in [-0.07755, -0.07745], and the two halves of that interval
    # do not round to the same third decimal. The check is right to refuse
    # to adjudicate, and it can only ever refuse while the file it reads has
    # already thrown the digit away.
    #
    # ROUNDING FOR THE CONSOLE, NOT FOR THE FILE. t and mde are computed
    # above on unrounded values already, so nothing downstream changes; the
    # printed frame keeps its four decimals because that is what makes the
    # log readable, and the csv keeps the estimate the fit produced because
    # that is what makes the .tex checkable.
    print(transform(.hrs, est = round(est, 4), se = round(se, 4)),
          row.names = FALSE)
    cat("  mde: smallest effect detectable at 80% power, 5% two-sided.\n")
    cat("  hours_robustness.csv stores est and se UNROUNDED, so Check 6 can\n",
        "  settle the last digit the .tex prints rather than reporting it\n",
        "  undecidable. The console above is rounded; the file is not.\n",
        sep = "")
    
    .gv <- function(a) { v <- .hrs$est[grepl(a, .hrs$arm)]; if (length(v)) v[1] else NA_real_ }
    .base <- .gv("^A "); .e <- .gv("^E ")
    
    # THE VERDICT IS READ OFF ARM E, and it is stated as a branch rather than
    # asserted, because both branches are publishable and the script does not
    # know which one the data give.
    if (!is.na(.base) && !is.na(.e)) {
      cat(sprintf("\n  Arm E is %.0f%% of the baseline gap (%.4f vs %.4f).\n",
                  100 * .e / .base, .e, .base))
      .es <- .hrs$se[grepl("^E ", .hrs$arm)][1]
      if (!is.na(.es) && abs(.e / .es) >= 1.96) {
        cat("  *** THE GAP SURVIVES among women full time BEFORE the birth.\n")
        cat("      Hours alone do not produce it: this subsample was equally\n")
        cat("      attached going in, and the restriction is on a\n")
        cat("      PRE-treatment variable, so it is not bad control.\n")
        cat("      REPORT THIS BESIDE T4, not instead of it. T4 shows the gap\n")
        cat("      surviving among mothers who RETURNED to full time; E shows\n")
        cat("      it surviving among mothers who WERE full time going in.\n")
        cat("      Only E is free of selection on an outcome, and two\n")
        cat("      restrictions failing to kill the coefficient for different\n")
        cat("      reasons is worth more than either alone.\n")
      } else {
        cat("  *** THE GAP DOES NOT SURVIVE at conventional significance in\n")
        cat("      arm E. Check the MDE column before calling it a null: a\n")
        cat("      pre-birth full-time restriction is a small subsample and\n")
        cat("      an underpowered arm is not evidence of absence.\n")
        cat("      If it is genuinely null, the claim narrows to 'employers\n")
        cat("      invest less in mothers, and the reduction in hours is\n")
        cat("      why'. That is still a result and still needs the funding\n")
        cat("      split, because without it you cannot tell whose decision\n")
        cat("      the hours reduction reflects.\n")
      }
      rm(.es)
    }
    if (!is.na(.gv("^B ")) || !is.na(.gv("^C ")))
      cat("\n  B and C are reported because the plan asks for them. Their\n",
          "  distance from A measures how much of the gap runs through hours\n",
          "  ARITHMETICALLY; it does not identify anything, because hours\n",
          "  respond to the birth. Do not report B or C as the test.\n", sep = "")
    
    # Cross-check E against T4c. Same rule, different code path; a gap
    # means the two frames differ and one of them is not the design's.
    if (!is.null(.t4c) && !is.na(.gv("^E "))) {
      .diff <- abs(.gv("^E ") - .t4c["est"])
      cat(sprintf("\n  E vs T4c: %+.4f vs %+.4f, difference %.4f.\n",
                  .gv("^E "), .t4c["est"], .diff))
      if (.diff > 0.005)
        cat("  *** THEY DISAGREE. Same sample rule, so the FRAMES differ.\n",
            "      03_common.R builds `ev` from the cohort join; this\n",
            "      block rebuilds it or reuses whatever was left in scope.\n",
            "      Find which frame the design uses before reporting either.\n",
            sep = "")
      else
        cat("  Agree to rounding, so the two frames are the same and T4c is\n",
            "  the one to report.\n", sep = "")
      rm(.diff)
    }
    write.csv(.hrs, .out_path("hours_robustness.csv"), row.names = FALSE)
    cat("\n  Written: hours_robustness.csv\n")
    .mpn("mpn_hours", .hrs)
    if (exists("REG") && is.function(REG))
      for (.i in seq_len(nrow(.hrs)))
        try(REG(paste("9.3 hours |", .hrs$arm[.i]), .hrs$est[.i],
                "employer-funded training gap, hours held fixed"), silent = TRUE)
    rm(.gv, .base, .e)
  }
  rm(.emp, .fit, .fe, .cont_ft, .pre_ft, .arms)
  if (exists(".t4c")) rm(.t4c)
  if (exists(".hrs")) rm(.hrs)
} else if (.hrs_ok) {
  cat("  9.3 SKIPPED: no event frame, or", .OUT, "absent from it.\n")
  cat("  T4 still stands - it is estimated inside 03a_design_audit.R and\n")
  cat("  does not depend on this block - so 9.3 is not empty. What is missing\n")
  cat("  without arm E is the PRE-BIRTH restriction, which is the only one\n")
  cat("  free of selection on a post-treatment outcome. Say that in the text\n")
  cat("  rather than presenting T4 as though it closed the question.\n")
}
rm(.hrs_ok, .OUT)
if (exists(".ev")) rm(.ev)

# =============================================================================
# PART 6: LEDGER, RUN RECORD, SESSION
# =============================================================================


# =============================================================================
# PART 6A: THE BLIND LEDGER CHECK
# =============================================================================
# Every estimate in TRAINING_PAPER_OUTLINE.md section 2 was designated
# preferred, secondary or bounded BEFORE any coefficient was seen. That is
# the paper's strongest procedural claim and the abstract's last sentence
# rests on it, so it is checked mechanically here rather than by eye.
#
# WHAT THIS BLOCK IS FOR, and what it is not. It does not re-estimate
# anything: the modules above did that. It reads the ledger values recorded
# in the outline and reports whether the objects now in memory still carry
# them. A drift here means either a module changed or the outline is stale,
# and either way the paper is quoting a number nothing produces.
#
# THE STATUS COLUMN IS THE POINT. A null in a PREFERRED specification is
# evidence; the same null in a specification chosen after seeing it is not.
# T7 anticipation is the case that matters: -0.012 against an MDE of 0.069
# is reportable only because the design was fixed in advance, and the
# paper's section 8 leans on exactly that.
# THE LEDGER IS READ, NOT TRANSCRIBED. This block used to hold nine rows
# typed out of TRAINING_PAPER_OUTLINE.md, checked against nothing. That is
# how "Any training, SE 0.019" survived: it named a design 03training_designs.R
# never ran, and no code could notice because the only copy of the number
# was the copy being checked.
#
# THREE LEDGERS BECAME ONE. The module writes training_ledger.csv (tags, n,
# SE, MDE, verdict, status) and BLIND_training_estimates.rds (the point
# estimates, released when the master unblinds). Those are the source. The
# outline's published values are kept below as a COMPARISON TARGET, which is
# the only honest role for a transcription: something to reconcile against,
# never something to report from.
.led_dir  <- if (!is.na(TD_OUTDIR)) TD_OUTDIR else "."
.led_path <- c(file.path(.led_dir, "training_ledger.csv"), "training_ledger.csv")
LEDGER <- NULL
for (.p in .led_path)
  if (is.null(LEDGER) && file.exists(.p))
    LEDGER <- tryCatch(read.csv(.p, stringsAsFactors = FALSE), error = function(e) NULL)

.est_path <- c(file.path(.led_dir, "BLIND_training_estimates.rds"),
               "BLIND_training_estimates.rds")
BLIND_EST <- NULL
for (.p in .est_path)
  if (is.null(BLIND_EST) && file.exists(.p))
    BLIND_EST <- tryCatch(readRDS(.p), error = function(e) NULL)

cat("\n", strrep("=", 78), "\n", sep = "")
cat("  THE BLIND LEDGER  (status designated before any coefficient was seen)\n")
cat(strrep("=", 78), "\n", sep = "")

if (is.null(LEDGER)) {
  cat("  training_ledger.csv not found. The 03 modules did not run, or\n")
  cat("  wrote to a different OUTDIR. NOTHING BELOW IS CHECKED and no number\n")
  cat("  in this run should reach the paper.\n")
} else {
  # Point estimates joined on where the module sealed them, so the ledger
  # prints one object rather than an estimate table and a status table that
  # a reader has to align by eye.
  if (!is.null(BLIND_EST) && length(BLIND_EST)) {
    .e <- vapply(LEDGER$tag, function(t)
      if (!is.null(BLIND_EST[[t]])) unname(BLIND_EST[[t]][1, 1]) else NA_real_,
      numeric(1))
    LEDGER$est <- round(.e, 4)
    rm(.e)
  } else {
    LEDGER$est <- NA_real_
    cat("  BLIND_training_estimates.rds absent: the module ran blinded, so\n")
    cat("  only SEs and verdicts are available. Set MASTER_UNBLIND <- TRUE.\n")
  }
  print(LEDGER[, intersect(c("tag", "n", "est", "se", "mde", "verdict",
                             "verdict_f", "status"), names(LEDGER))],
        row.names = FALSE)
  write.csv(LEDGER, .out_path("train_ledger.csv"), row.names = FALSE)
  cat("\n  Written: train_ledger.csv (from the module, not transcribed)\n")
}
# The ledger's estimates are registered from the sealed list, unrounded. A row
# is left out if the module that wrote it ran before the panel was built.
.mpn("mpn_ledger", LEDGER, BLIND_EST, .led_dir)

# ---- reconciliation against the outline's published values -----------------
# Each row names the TAG it claims to come from. A row whose tag is not in
# the module's ledger is a number with no design behind it, which is the
# failure this reconciliation exists to catch. A row whose SE disagrees is a
# stale transcription.
PUBLISHED <- data.frame(
  finding = c("Employer-financed training falls", "Own-time training does not",
              "The funding contrast, directly", "Survives within couples",
              "Study rises as training falls", "Any training",
              "Courses (dosage)", "Anticipation", "Purpose of training"),
  tag     = c("T1_jttpewt_any_employed", "T1_jttpeot_any_employed",
              "T3_ddd_saturated", "T6_couple_x_wave", "T9_study_vs_training",
              "T4_any", "T5b_courses", "T7_pregnancy_gap",
              "T3b_unconditional"),
  # THESE LITERALS ARE CORRECT AND MUST STAY. Unlike the wage path above,
  # se_pub is not a copy of a derived quantity: it is what the MANUSCRIPT
  # prints, and the block below reconciles it against what the module
  # computed. Deriving it would make the check compare a number with itself
  # and report agreement in every case, including the ones it exists to
  # catch. Update it when the paper's printed values change, not when the
  # estimates do.
  #
  # v21.48: courses 0.116 -> 0.115, the SE the revised text prints for
  # T5b_courses on total_courses_z (module 0.11549). Change it back if the
  # paper keeps 0.116.
  se_pub  = c(0.015, 0.0078, 0.014, 0.022, 0.011, 0.019, 0.115, 0.0246, 0.0149),
  stringsAsFactors = FALSE)
# THE "ANY TRAINING" ROW IS MISLABELLED AND THE v23 RUN PROVED IT.
#
# Adding any_training to T1 was supposed to supply the missing design. It
# supplied a design and RULED THE ROW OUT instead:
#
#   T4_any                    SE 0.0191   full-time RETURNERS, employer-funded
#   T1_any_training_employed  SE 0.0173   the actual any-training arm
#   T1_any_training_uncond    SE 0.0154
#
# 0.019 is T4_any to the digit. Neither any-training arm rounds to it. So
# the ledger row headed "Any training" has always been the FULL-TIME
# RETURNERS result, and the abstract would have reported an hours-restricted
# subsample as a headline incidence number - counting the hours robustness
# check twice, once as itself and once as the main effect.
#
# The tag above is corrected to T4_any so the reconciliation passes on what
# the number IS. THE LABEL IS NOW THE THING TO FIX, in the outline and the
# paper: that row is not "any training", it is "employer-funded training
# among mothers who returned to full time", and it carries T4's selection
# problem with it.
#
# AND THE REAL ANY-TRAINING NUMBER NOW EXISTS. T1_any_training_employed,
# n=54,280, SE 0.0173, verdict OK. If the paper wants an any-training
# incidence result it should quote that, not this.
PUBLISHED$note <- c(rep("", 5),
                    "MISLABELLED: this is T4_any (full-time returners), not any training",
                    rep("", 3))

if (!is.null(LEDGER)) {
  PUBLISHED$se_module <- LEDGER$se[match(PUBLISHED$tag, LEDGER$tag)]
  PUBLISHED$status    <- LEDGER$status[match(PUBLISHED$tag, LEDGER$tag)]
  PUBLISHED$ok <- ifelse(is.na(PUBLISHED$se_module), "TAG NOT IN LEDGER",
                         ifelse(abs(PUBLISHED$se_module - PUBLISHED$se_pub) <= 0.0015,
                                "ok", "SE DISAGREES"))
  cat("\n  Reconciliation against the outline's published SEs:\n")
  print(PUBLISHED[, c("finding", "tag", "se_pub", "se_module", "status", "ok")],
        row.names = FALSE)
  if (any(nzchar(PUBLISHED$note))) {
    cat("\n  Notes:\n")
    for (.i in which(nzchar(PUBLISHED$note)))
      cat("   ", PUBLISHED$finding[.i], "->", PUBLISHED$note[.i], "\n")
  }
  if (!is.null(LEDGER) && "T1_any_training_employed" %in% LEDGER$tag)
    cat(sprintf(paste0("\n  The genuine any-training arm exists: ",
                       "T1_any_training_employed, SE %.4f.\n  Quote that if ",
                       "the paper wants an any-training incidence result.\n"),
                LEDGER$se[LEDGER$tag == "T1_any_training_employed"]))
  .bad <- PUBLISHED[PUBLISHED$ok != "ok", ]
  if (nrow(.bad)) {
    cat("\n  *** ", nrow(.bad), " row(s) do not reconcile. Do not quote them.\n", sep = "")
    warning("train ledger: ", nrow(.bad),
            " published row(s) do not match the module's ledger (",
            paste(.bad$tag, collapse = ", "), ").", call. = FALSE)
  } else {
    cat("  All nine reconcile against the module's own ledger.\n")
  }
  rm(.bad)
}
rm(.led_dir, .led_path, .est_path)

# ---- the wage path, in the ledger because the claim now includes it ---------
# The updated outline keeps the wage penalty and makes the ORDERING the
# contribution, so these six coefficients are load-bearing and belong under
# the same audit as the training estimates rather than in prose.
#
# THE t COLUMN IS PRINTED ON PURPOSE. Read down it: 1.37, 1.10, 1.08, 2.23,
# 1.56, 2.25. The path crosses 1.96 at k = +6, falls back below at +8, and
# crosses again at +10. The clean sentence - flat for four years, significant
# thereafter - is not what these numbers say, and the SE column is the first
# thing a referee will read. 8.2 prints the same warning against the live
# series; this is the tabulated copy so the ledger is self-contained.
# v24.10. READ, NOT TYPED. These six coefficients were literals, and the
# comment above calls them load-bearing, which is exactly why they should not
# have been.
#
# THE COUNT OF VINTAGES IS THE ARGUMENT. The same six numbers existed in the
# manuscript prose, in Table 8 of the manuscript, in wage_path_ledger.csv
# written from here, and in these literals - four copies with one derivation
# among them, and the derivation was in none of the four. The manuscript also
# quotes pre-birth horizons, +0.006 down to -0.023, that appear in no copy at
# all, since the literals started at k = 0. Nothing was wrong; nothing was
# holding it in place either.
#
# THE READER ALREADY EXISTS IN THIS FILE. Section 8.2 resolves the same RDS
# at line ~1386, by pattern over candidate column names, because the schema
# is documented nowhere. Rather than write a second resolver that could drift
# from the first, this reuses .tim_wage when 8.2 has already built it and
# falls back to its own read when the block is skipped.
#
# IT REFUSES RATHER THAN WARNS. If the RDS and the literals disagree, the run
# STOPS. A warning would leave the paper quoting whichever number was typed
# most recently, which is the failure this replaces, and a divergence here is
# cheap to find and expensive to miss.
.wp_from_rds <- NULL
if (exists(".tim_wage") && is.data.frame(.tim_wage) &&
    all(c("k", "est") %in% names(.tim_wage))) {
  .wp_from_rds <- .tim_wage
  .wp_src <- if (exists(".wage_src")) .wage_src else "section 8.2's resolved frame"
} else if (file.exists(.in_path("cs_event_study_results.rds"))) {
  .cs <- tryCatch(readRDS(.in_path("cs_event_study_results.rds")),
                  error = function(e) NULL)
  # v24.12. Same correction as section 8.2: read the "gap " element by name
  # rather than the first data frame in the list. See the note there.
  .cand <- if (is.data.frame(.cs)) list(.cs) else if (is.list(.cs)) {
    .nm <- names(.cs)
    .gp <- if (!is.null(.nm)) which(grepl("^gap ", .nm)) else integer(0)
    .wg <- .gp[grepl("wage", .nm[.gp])]
    # v-age. Plain arm first by name, as in section 8.2. Without it the
    # ledger, and so the abstract's ten-year figure, would follow storage
    # order between the plain and the age-absorbed gap.
    .wg <- .wg[order(.nm[.wg] != "gap ln_hourly_wage_real")]
    if (length(.gp)) Filter(is.data.frame, .cs[if (length(.wg)) .wg else .gp])
    else Filter(is.data.frame, .cs)
  } else list()
  for (.dd in .cand) {
    .kc <- intersect(c("k", "rel_time", "event_time", "time", "period"), names(.dd))
    # v24.14. See the note in section 8.2: the gap frames call the estimate
    # column "gap", not "att".
    .ec <- intersect(c("est", "estimate", "att", "coef", "coefficient", "gap"),
                     names(.dd))
    .sc <- intersect(c("se", "std.error", "stderr", "se_att"), names(.dd))
    if (length(.kc) && length(.ec) && is.null(.wp_from_rds)) {
      .wp_from_rds <- data.frame(k = .dd[[.kc[1]]], est = .dd[[.ec[1]]],
                                 se = if (length(.sc)) .dd[[.sc[1]]] else NA_real_)
      .wp_src <- sprintf("cs_event_study_results.rds (%s / %s)", .kc[1], .ec[1])
    }
  }
  if (is.null(.wp_from_rds)) {
    cat("\n  cs_event_study_results.rds read, but no data frame in it carries a\n")
    cat("  recognisable event-time and estimate pair. Its structure:\n")
    utils::str(.cs, max.level = 2)
    cat("  Add the column names to the two intersect() calls above.\n")
  }
  rm(.cs, .cand)
}

# The horizons the manuscript tabulates. Kept as a request rather than as
# values: which k the paper shows is an editorial choice, what they equal is
# not.
WAGE_PATH_K <- c(0, 2, 4, 6, 8, 10)

if (!is.null(.wp_from_rds)) {
  WAGE_PATH_LEDGER <- .wp_from_rds[match(WAGE_PATH_K, .wp_from_rds$k), c("k","est","se")]
  rownames(WAGE_PATH_LEDGER) <- NULL
  cat(sprintf("\n  wage path read from %s\n", .wp_src))
  if (anyNA(WAGE_PATH_LEDGER$k)) {
    cat("  *** HORIZONS MISSING FROM THE SOURCE: ",
        paste(setdiff(WAGE_PATH_K, .wp_from_rds$k), collapse = ", "), "\n", sep = "")
    cat("      The manuscript tabulates them, so either the request above is\n")
    cat("      wrong or the event study did not reach that far.\n")
  }
} else {
  # LAST RESORT, AND IT SAYS SO. The literals are retained only so a run
  # without the RDS still produces a ledger, and every consumer downstream is
  # told the provenance. They are the values that stood on 18 August; if the
  # RDS ever disagrees with them the block below stops the run rather than
  # letting the two drift apart unnoticed.
  cat("\n  *** cs_event_study_results.rds ABSENT. The wage path below is the\n")
  cat("      TRANSCRIBED copy, not a derived one. Run CS_event_study.R and\n")
  cat("      re-run this before quoting any of it.\n")
  # ONE LITERAL IN THE FILE, NOT TWO. Section 8.2 already carries .wage_tab
  # as its own labelled fallback, so retyping the same six numbers here
  # would recreate in miniature the drift this whole block exists to stop:
  # two copies, one updated, no error.
  WAGE_PATH_LEDGER <- if (exists(".wage_tab") && is.data.frame(.wage_tab))
    .wage_tab[, c("k", "est", "se")] else data.frame(
      k   = c(0, 2, 4, 6, 8, 10),
      est = c(-0.026, -0.022, -0.026, -0.069, -0.061, -0.119),
      se  = c( 0.019,  0.020,  0.024,  0.031,  0.039,  0.053))
  .wp_src <- if (exists(".wage_tab")) "TRANSCRIPTION (section 8.2's .wage_tab)"
  else "TRANSCRIPTION (no RDS, no .wage_tab)"
}

# THE GUARANTEE THE OLD COMMENT CLAIMED AND DID NOT HAVE. If a previous run
# wrote wage_path_ledger.csv from a different source, the two must agree.
if (file.exists(.in_path("wage_path_ledger.csv")) && !is.null(.wp_from_rds)) {
  .prev <- tryCatch(utils::read.csv(.in_path("wage_path_ledger.csv")),
                    error = function(e) NULL)
  if (is.data.frame(.prev) && all(c("k","est") %in% names(.prev))) {
    .cmp <- merge(WAGE_PATH_LEDGER[, c("k","est")], .prev[, c("k","est")],
                  by = "k", suffixes = c("_new", "_old"))
    .off <- .cmp[abs(.cmp$est_new - .cmp$est_old) > 5e-4, ]
    if (nrow(.off)) {
      cat("\n", strrep("!", 74), "\n", sep = "")
      cat("WAGE PATH DISAGREES WITH THE CSV ON DISK\n")
      print(.off, row.names = FALSE)
      cat(strrep("!", 74), "\n", sep = "")
      stop("wage path: the RDS and wage_path_ledger.csv differ by more than ",
           "0.0005 at ", nrow(.off), " horizon(s). One of them is stale, and ",
           "the manuscript quotes these numbers. Resolve before continuing ",
           "rather than letting the run pick one.", call. = FALSE)
    }
    rm(.cmp, .off)
  }
  rm(.prev)
}
WAGE_PATH_LEDGER$t   <- round(WAGE_PATH_LEDGER$est / WAGE_PATH_LEDGER$se, 2)
WAGE_PATH_LEDGER$sig <- ifelse(abs(WAGE_PATH_LEDGER$t) >= 1.96, "*", "")
cat("\n  THE WAGE PATH (ln hourly wage, mothers minus fathers):\n")
print(WAGE_PATH_LEDGER, row.names = FALSE)
cat("  Training moves at k = 0. Wages first cross at k = +6, fall back at\n")
cat("  +8, cross again at +10. Report it as a rising path with a noisy\n")
cat("  middle and give the t-statistics; do not write a clean break.\n")
write.csv(WAGE_PATH_LEDGER, .out_path("wage_path_ledger.csv"), row.names = FALSE)

cat("\n  THE ORDERING IS THE CONTRIBUTION. This paper does not offer a better\n")
cat("  estimate of the penalty; it shows what moves first. Accumulation\n")
cat("  predicts investment then wages. A lower-productivity account predicts\n")
cat("  no lag. The lag discriminates without needing a magnitude, which is\n")
cat("  why it replaces the Gelbach decomposition rather than deferring to it.\n")

cat("\n  NOT A PRICE STORY, which is the premise the above rests on. No\n")
cat("  gender difference in the return to experience across ten arms and\n")
cat("  five treatment constructions, MDEs stated. And at MATCHED experience\n")
cat("  mothers out-earn never-mothers through most of the career: +0.21 log\n")
cat("  points at twelve years, +0.09 at twenty-five, negative only in the\n")
cat("  top bin. The penalty operates on the QUANTITY of human capital.\n")

# ---- what the funding module's standalone run established ------------------
# From training_funding_output.txt, the full standalone pass WITH Block D.
# Recorded here because the master runs that module under tfd_fast, which
# skips the placebo, so these facts are not in a master log.
cat("\n  THE FUNDING CONTRAST, from the standalone pass:\n")
cat("    pre-trend k<=-2   Holm min p 0.87, 0 of 11 raw p<.05.  PASSES.\n")
cat("    joint post k>=0   Holm min p 2.8e-06, 9 of 9 adjusted p<.05.\n")
cat("    T2 pooled post    -0.1145 (SE 0.0238), p 1.5e-06 [MDE 0.0667]\n")
cat("    early k0-1        -0.1129 (SE 0.0217)\n")
cat("    late  k4-8        -0.1037 (SE 0.0326)\n")
cat("    T3 decay          +0.0091 (SE 0.0392), p 0.82 [MDE 0.1098]\n")

# THE PRE-TREND MATTERS MOST AND IT PASSES CLEANLY. Zero of eleven pre-birth
# horizons reject at 5% raw, Holm min p 0.87. A funding contrast that trended
# before the birth would be uninterpretable whatever the post estimates did,
# so this is the precondition and not a footnote.
#
# THE PERSISTENCE CLAIM IS SUPPORTED, and it needs care because the module's
# own reading guide is easy to misread here. That guide says a significant
# NEGATIVE decay is the signature of the persistence claim, so a null decay
# looks like a failure. It is not. The paper claims the gap STAYS DOWN, not
# that it widens. Early -0.1129 against late -0.1037, both individually
# significant at p 2e-07 and p 0.0015, with a decay of +0.0091, is a flat and
# persistent gap. That is exactly the claim.
#
# BUT REPORT IT WITH THE MDE. The decay test's MDE is 0.1098, about the size
# of the effect itself, so "no decay" rules out the gap roughly halving and
# nothing finer. Write "indistinguishable from flat, MDE 0.11" rather than
# "constant". All eight decay tests across the pairs are null and every one
# carries the conservative-SE flag, so this is a bound and not a measurement.
cat("    PERSISTENCE: early and late are within 0.01 and both reject, so the\n")
cat("    gap is flat and persistent. Report the decay null WITH its MDE of\n")
cat("    0.11: it rules out the gap halving, not more.\n")

# THE STANDALONE PASS READ THE UNFILTERED FILE, 134,131 rows, because no
# fingerprint is declared outside a master. The contract behaved correctly -
# it is a no-op standalone - but it means these numbers are NOT on the
# analysis sample. Re-run under the master, or with the fingerprint set,
# before any of them is quoted beside a T-series estimate.
cat(sprintf(paste0("    SAMPLE: that pass predates the contract and ran on the panel as\n",
                   "    loaded, 134,131 rows; this run has %s. Re-run it under the\n",
                   "    master before quoting these beside T1-T9.\n"),
            format(nrow(panel), big.mark = ",")))

cat("\n  THE HOURS ALTERNATIVE is answered in two places, not one. T4 in\n")
cat("  03a_design_audit.R restricts to full-time RETURNERS (post-birth,\n")
cat("  so selected on an outcome); 9.3 arm E restricts to women full time\n")
cat("  BEFORE the birth (pre-determined). Cite both. Read\n")
cat("  hours_robustness.csv arm E before drafting section 9.3, and do not\n")
cat("  repeat T4's log line that the composition story is 'closed' - a\n")
cat("  post-birth restriction cannot close it.\n")

# =============================================================================
# PART 6B: WHAT THIS SCRIPT DELIBERATELY DOES NOT PRODUCE
# =============================================================================
# Printed at the end of every run so an absence is never mistaken for a
# module that failed silently. Each line is a decision from
# TRAINING_PAPER_OUTLINE.md, not an omission.
cat("\n", strrep("=", 78), "\n", sep = "")
cat("  NOT PRODUCED, BY DECISION\n")
cat(strrep("=", 78), "\n", sep = "")
cat("  - No Gelbach decomposition and no 29% share. It decomposes a WAGE\n")
cat("    penalty into experience and residual; this paper's outcome is\n")
cat("    training.\n")
cat("  - No specialisation regressor S_ct, no theta_1, no P1. The static\n")
cat("    Becker channel answers a question this paper does not ask.\n")
cat("  - No FTB-B instrument. First stage 1.93 against a falsification F of\n")
cat("    44.79: the placebo runs the wrong way.\n")
cat("  - No couple DML or causal forest. T6 does the within-couple work on\n")
cat("    the training outcome.\n")
cat("  - No mediation from training to wages. The timing establishes\n")
cat("    ORDERING, not magnitude, and that is the claim the data support. A\n")
cat("    share attributable to training needs the mediator correctly\n")
cat("    specified and reopens the machinery this rebuild cut.\n")
cat("  - No heterogeneity. PAPER_ATTENTION 2.22 closed that out of sample\n")
cat("    with a stated MDE. Reopening it with a different covariate set\n")
cat("    after a null is what the split-sample design exists to prevent.\n")
cat("  - No non-cash benefits RESULT. The level gap is real and reportable,\n")
cat("    but the birth-wave dynamic has an MDE about four times its base, so\n")
cat("    it belongs in the data section as a measurement point about what\n")
cat("    wages omit. The loader builds the columns; nothing here estimates\n")
cat("    an event study on them.\n")
cat("  - The wage event study is now a RESULT (PART 4, section 8), not\n")
cat("    setting. That changed with the updated outline. Section 5 is\n")
cat("    employment and hours only.\n")

# =============================================================================
# RUN RECORD: structured dump of every part's key result tables to the log
# =============================================================================
# source_logged() above already echoes each module's full output. This block
# additionally writes a clean, greppable copy of the headline result objects so
# the log is self-contained: a reader does not have to reopen the .rds files to
# confirm Parts G, G+, H, and the Heckman correction. Every print is guarded by
# exists()/is.null() so a skipped module degrades to a NOTE rather than erroring.

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

.print_tbl <- function(obj, header) {
  cat("\n", strrep("-", 78), "\n", sep = "")
  cat("  ", header, "\n", sep = "")
  cat(strrep("-", 78), "\n", sep = "")
  # Never let a print method abort the master run. Some objects (e.g. model
  # summaries, data frames with custom print methods) reject the n=/width=
  # arguments and raise "invalid 'na.print' specification"; fall back to a
  # plain print, then to str(), capturing any error as a NOTE.
  ok <- tryCatch({
    if (inherits(obj, "data.frame") || tibble::is_tibble(obj)) {
      print(as.data.frame(obj), row.names = FALSE)
    } else {
      print(obj)
    }
    TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) {
    tryCatch(utils::str(obj, max.level = 2, give.attr = FALSE),
             error = function(e)
               cat("  NOTE: could not print object (", conditionMessage(e), ")\n", sep = ""))
  }
  invisible(NULL)
}

cat("\n\n")
cat(strrep("#", 80), "\n")
cat("#  RUN RECORD: ALL PARTS (B-H) KEY TABLES\n")
cat(strrep("#", 80), "\n")

# Whole block guarded: the run record is diagnostic only and must never stop
# the script from reaching final verification, sessionInfo, and sink closure.
.run_record_ok <- tryCatch({
  
  # ── Heckman selection correction (Part 6B, paper Table 3) ──────────────────
  if (exists("heckman_results") && nrow(heckman_results) > 0) {
    .print_tbl(heckman_results,
               "HECKMAN SELECTION CORRECTION (Table 3): FE raw/corrected beta2 + IMR")
  } else {
    cat("\n  NOTE: heckman_results absent or empty; Heckman table not recorded.\n")
  }
  
  # ── Section V.E: pre-birth within-couple gap (P3 robustness vs Chiappori) ──
  if (exists("pre_birth_gap") && !is.null(pre_birth_gap)) {
    .print_tbl(pre_birth_gap,
               "SECTION V.E: PRE-BIRTH vs FULL within-couple beta2 gap (linear couple+wave FE)")
  } else {
    cat("\n  NOTE: pre_birth_gap absent; Section V.E robustness not recorded.\n")
  }
  
  # ── P3 robustness: within-couple gender gap in Mincer returns betaX, betaE ──
  if (exists("mincer_gap_tbl") && !is.null(mincer_gap_tbl)) {
    .print_tbl(mincer_gap_tbl,
               "P3 ROBUSTNESS: pooled vs within-couple gender gap in betaX (experience) and betaE (schooling)")
  } else {
    cat("\n  NOTE: mincer_gap_tbl absent; betaX/betaE gender gaps not recorded.\n")
  }
  
  # PART G, G+ and H tables removed at v22 with the modules that fed them.
  
  # ── Training funding contrast and dosage margin ────────────────────────────
  .tfd <- tryCatch(get("out", envir = globalenv()), error = function(e) NULL)
  if (is.null(.tfd) && file.exists(.in_path("training_funding_divergence_results.rds")))
    .tfd <- tryCatch(readRDS(.in_path("training_funding_divergence_results.rds")),
                     error = function(e) NULL)
  if (!is.null(.tfd)) {
    cat("\n", strrep("-", 78), "\n", sep = "")
    cat("  TRAINING: employed-only, mothers minus fathers\n")
    cat(strrep("-", 78), "\n", sep = "")
    .row <- function(lab, key, field, el) {
      v <- tryCatch({
        o <- .tfd[[key]]
        if (field == "triangle") unname(o$summary$iii[el]) else unname(o[[field]][el])
      }, error = function(e) NA_real_)
      s <- tryCatch({
        o <- .tfd[[key]]
        if (field == "triangle") unname(o$summary$iii["se"]) else unname(o[[field]]["se"])
      }, error = function(e) NA_real_)
      # v21.48. A key that is not in .tfd returns NULL rather than an error,
      # and if (is.finite(NULL)) stops the master. Print n/a instead.
      if (length(v) != 1L) v <- NA_real_
      if (length(s) != 1L) s <- NA_real_
      cat(sprintf("    %-38s %s (SE %s)\n", lab,
                  if (is.finite(v)) sprintf("%+.4f", v) else "n/a",
                  if (is.finite(s)) sprintf("%.4f", s) else "n/a"))
    }
    # v24.16. REKEYED FROM pooled_gated TO pe_gated, AND BOTH REPORTED.
    #
    # This block labelled a pooled_gated estimate "Employer-financed
    # incidence gap", which is not the quantity Table 7 heads. The pooled
    # outcomes are training_paid_time_obs = jttpewt | jttopwt, so they
    # count training on paid time AWAY from the workplace as
    # employer-financed; Table 7's headline is jttpewt_any, workplace
    # released time only.
    #
    # They are not the same number. On the asked population the pooled
    # difference-in-differences is -0.1433 against -0.1227 for
    # jttpewt_any on identical rows, and on the employed comparison
    # -0.1331 against -0.1162: the pooled measure runs fifteen to
    # seventeen per cent larger in magnitude. A run record carrying the
    # larger figure under the paper's label is how a reader comes to
    # quote it.
    #
    # BOTH ARE PRINTED rather than the wrong one swapped for the right
    # one, because the difference between them is the size of the
    # elsewhere category and is worth seeing on every run. pe_gated is
    # already in RUN_PAIRS, so this costs no estimation.
    .row("Employer-financed gap (jttpewt, matches Table 7)",
         "B | pe_gated | employed_only | jttpewt_obs", "triangle", "est")
    .row("  same, pooled across place (diagnostic)",
         "B | pooled_gated | employed_only | training_paid_time_obs",
         "triangle", "est")
    .row("Own-time gap (jttpeot, matches Table 7)",
         "B | pe_gated | employed_only | jttpeot_obs", "triangle", "est")
    .row("  same, pooled across place (diagnostic)",
         "B | pooled_gated | employed_only | training_own_time_obs",
         "triangle", "est")
    .row("Aim contrast (promotion - maintenance)",
         "B | aim | employed_only | d_rel", "triangle", "est")
    # v21.48. B2 runs on total_courses_z, the count gated in the loader,
    # and its key carries the column name. An older results file or an
    # older loader has only the ungated key, so read that one when it is
    # the only one there and say so. When neither is there, the rows print
    # n/a, which is the right signal.
    .crs_key <- "B2 | employed_only | total_courses_z"
    if (is.null(tryCatch(.tfd[[.crs_key]], error = function(e) NULL)) &&
        !is.null(tryCatch(.tfd[["B2 | employed_only | total_courses"]],
                          error = function(e) NULL))) {
      .crs_key <- "B2 | employed_only | total_courses"
      cat("    Courses rows read the ungated total_courses (pre-v21.48 run).\n")
    }
    .row("Courses gap (level)", .crs_key, "level", "est")
    .row("Courses gap (proportional)", .crs_key, "prop", "est")
    rm(.crs_key)
    cat("    total_training_hours_z omitted: MDE 15.1 h against a base of\n")
    cat("    17.9, so it bounds nothing. Report it as a limit of the measure.\n")
    
    # v24.17. THE OWN-TIME DECOMPOSITION MOVED OUT, to
    # report_own_time_decomposition.R, which runs after 04.
    #
    # It could not work here. It reads 04_financing_space_ledger.csv, and
    # 04 reads the handoff registry THIS file populates. The master runs
    # first and the ledger does not exist yet; 04 cannot run first because
    # it needs the frame. On 25 August the master ran at 13:50 and 04
    # wrote at 13:54, so the block found a 07:08 copy or nothing.
    #
    # The cycle is only there because the reporting sat inside the
    # estimation. Reading two ledgers and formatting them is not
    # estimation, so it belongs after both and the dependency dissolves.
    #
    # What it prints, for the record: Table 7's own-time arm is
    # jttpeot_any, which is 1 for anyone reporting workplace own-time
    # training including those who also had released time. 04 separates
    # them, -0.0026 pure against -0.0160 mixed on a total of -0.0186, so
    # about six sevenths of the arm is women who lost released time and
    # are counted for the own-time mention that came with it.
  } else {
    cat("\n  NOTE: training funding/dosage results absent; not recorded.\n")
  }
  
  TRUE
}, error = function(e) {  TRUE
}, error = function(e) {
  cat("\n  NOTE: RUN RECORD block stopped early (", conditionMessage(e),
      "); continuing to final verification.\n", sep = "")
  FALSE
})

cat("\n", strrep("#", 80), "\n", sep = "")
cat("#  END RUN RECORD\n")
cat(strrep("#", 80), "\n")

# Which paper numbers this run registered, and which it left as they were.
.mpn("mpn_end")

# ── Close log sinks (keep this as the last block of the script) ─────────────
cat(sprintf("\nRun complete: %s\nLog saved to: %s\n",
            format(Sys.time(), "%Y-%m-%d %H:%M:%S"), log_file))
sink(type = "message")
sink()
close(.log_con)

# ---- contract: hand off, verify, close -------------------------------
# td_put persists to handoff/, so the next script reads THIS object
# rather than whatever survived in the global environment.
# td_check_seals fails if a shared helper was redefined above: the
# last definition sourced wins and no call site says which is live.
td_check_seals("MASTER_training.R")
td_log_close()
