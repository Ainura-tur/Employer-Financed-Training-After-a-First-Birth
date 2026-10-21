# =============================================================================
# FINAL_PASS.R  -  one restarted session, one vintage
# -----------------------------------------------------------------------------
# WHAT THIS IS FOR. Every individual check in this project asserts something
# about one artefact. None of them can assert the property the submitted draft
# most needs, which is that the PDF and every estimate behind it came out of
# the same run. That property is not a check; it is a procedure, and this file
# is the procedure written down so it is followed the same way twice.
#
# RUN IT IN A RESTARTED SESSION. Not a cleared one. Half the faults this
# project has recorded were a name surviving from an earlier pass:
# RUN_STAGES set to three of the four stages, FIGDIR inherited from the
# exploratory driver, an orphaned sink that disabled logging for everything
# after it. rm(list = ls()) does not remove a sink, an attached package, or a
# masked dplyr verb. Restart, then source this.
#
# IT STOPS RATHER THAN WARNS. A warning in a log nobody reads afterwards is
# the failure mode this file exists to close, so each stage's precondition is
# an assertion. Set FP_STRICT <- FALSE to downgrade them to messages when you
# are deliberately running a partial pass; the default is on.
# =============================================================================

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select <- dplyr::select;       filter    <- dplyr::filter
mutate <- dplyr::mutate;       slice     <- dplyr::slice
recode <- dplyr::recode;       rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange <- dplyr::arrange;     count     <- dplyr::count;  lag <- dplyr::lag

FP_STRICT  <- if (exists("FP_STRICT"))  FP_STRICT  else TRUE
FP_CLEANUP <- if (exists("FP_CLEANUP")) FP_CLEANUP else FALSE
FP_T0      <- Sys.time()

.fp_say <- function(...) cat("\n[FINAL_PASS] ", ..., "\n", sep = "")
.fp_stop <- function(...) {
  if (isTRUE(FP_STRICT)) stop(paste0(...), call. = FALSE)
  cat("  *** ", ..., "\n      (FP_STRICT is FALSE, so continuing.)\n", sep = "")
}

# -----------------------------------------------------------------------------
# 0. PRE-FLIGHT: the session is empty, and the sink is at zero
# -----------------------------------------------------------------------------
# A BASELINE ABOVE ZERO POISONS EVERY LOG THAT FOLLOWS. 00_contract.R records
# the sink depth at load as .TD_SINK_BASE and every later script treats that
# depth as normal, so a contract sourced while a dead run's sink is still open
# teaches the whole session to ignore it. That is why this is checked before
# the contract rather than after.
.fp_say("0. pre-flight")
if (sink.number() > 0L)
  .fp_stop("a sink is open at depth ", sink.number(),
           " before anything has run.\n  Restart R. Do not source the contract",
           " on top of it: the baseline it records\n  will be wrong for the",
           " rest of the session.")

.fp_left <- intersect(c("panel", "ev", "RUN_STAGES", "FIGDIR", "OUTDIR",
                        "PUB", "HALF", "d"), ls(envir = globalenv()))
if (length(.fp_left))
  .fp_stop("this is not a restarted session. Still in scope: ",
           paste(.fp_left, collapse = ", "),
           "\n  Every one of these has decided a run's behaviour before.")
rm(.fp_left)

# -----------------------------------------------------------------------------
# 1. THE CONTRACT
# -----------------------------------------------------------------------------
.fp_say("1. contract")
source("00_contract.R")

if (!exists("PROJ_ROOT")) .fp_stop("00_contract.R did not define PROJ_ROOT.")

# THE WORKING DIRECTORY MUST BE PROJ_ROOT.
#
# On 11 September a run read its panel from TG/TG_updated and wrote every
# artefact into TG, because MASTER resolves its inputs relative to getwd()
# while the contract resolves outputs from PROJ_ROOT. Nothing downstream
# could see it: the two panels carried the same row count, the same person
# count and the same build_tag, so every check the project has came back
# clean on a run that had read one tree and written to another.
if (!identical(normalizePath(getwd(),   winslash = "/", mustWork = FALSE),
               normalizePath(PROJ_ROOT, winslash = "/", mustWork = FALSE)))
  .fp_stop("getwd() is\n    ", getwd(), "\n  and PROJ_ROOT is\n    ",
           PROJ_ROOT, "\n  Scripts resolve inputs from the first and outputs",
           " from the second.\n  setwd(PROJ_ROOT), or set HILDA_ROOT to the",
           " tree you mean, and restart.")

# A SENSITIVITY RUN MUST NOT BUILD THE PAPER.
#
# The figures, the table fragments and the macros all go to folders named
# without reference to the sample rule. So a pass under another rule would
# overwrite the manuscript's own figures/ and tables/ with estimates from a
# different sample, and the only trace would be a banner in a log.
#
# SINCE 14 OCTOBER THE DEFAULT RULE IS "none" (00_contract.R v28), and the
# screened run, TD_SCREEN=on, is the sensitivity run. It goes through RUN_ALL
# alone, not through this file.
# v-screen-guard, 16 October (apply_final_pass_screen_guard.R). A HARD
# STOP, WHATEVER FP_STRICT SAYS. This guard used .fp_stop(), and the run
# order sets FP_STRICT <- FALSE, so on 15 October a screened pass printed
# one line here and went on to write its estimates into the manuscript's
# folders. FP_STRICT downgrades the preconditions of a partial pass;
# building the paper from another sample is not one of those. TD_SCREEN
# is tested directly as well as through the rule it sets.
.fp_rule0 <- get0("TD_SAMPLE_RULE_DEFAULT", ifnotfound = "none")
.fp_scr   <- isTRUE(get0("TD_SCREEN_ON", ifnotfound = FALSE)) ||
  identical(tolower(trimws(Sys.getenv("TD_SCREEN", "off"))), "on")
if (.fp_scr ||
    (exists("TD_SAMPLE_RULE") && !identical(TD_SAMPLE_RULE, .fp_rule0)))
  stop("this is a sensitivity run. TD_SCREEN is \"",
       Sys.getenv("TD_SCREEN", ""), "\" and the sample rule is ",
       get0("TD_SAMPLE_RULE", ifnotfound = "unset"), ", not the default (",
       .fp_rule0, ").\n",
       "  This pass writes the manuscript's figures and table fragments,",
       " so it\n  would put another sample's estimates under the paper's",
       " captions.\n  Run Sys.unsetenv(c(\"TD_SCREEN\", \"TD_SAMPLE_HI\")),",
       " restart R, and\n  check that Sys.getenv(\"TD_SCREEN\") is empty",
       " before sourcing this file.\n  If it is still \"on\" after a",
       " restart, it is set in .Renviron or .Rprofile.\n  The screened",
       " pass goes through RUN_ALL alone.", call. = FALSE)
rm(.fp_rule0, .fp_scr)
if (sink.number() != 0L)
  .fp_stop("the contract loaded with a sink at depth ", sink.number(),
           ".\n  td_log_rescue(), restart, and source it again.")

# THE FOLDERS THAT HOLD THE SAME NAME TWICE, REPORTED BEFORE ANYTHING RUNS.
#
# Check 11 names these on every pass, and it names them because a duplicate
# is only ever resolved by deciding which copy is authoritative. That is a
# judgement, so this block reports and does not delete unless FP_CLEANUP is
# set deliberately. Deleting the copy no stage writes is right; deleting the
# older one is how a current artefact gets lost to a clock.
.fp_dups <- list(
  list(keep = if (exists("PAPER_FIGURES")) PAPER_FIGURES else NA_character_,
       drop = file.path(PROJ_ROOT, "figures"),
       why  = "\\includegraphics reads the manuscript's neighbour"),
  list(keep = if (exists("DIR_DESIGNS")) DIR_DESIGNS else NA_character_,
       drop = file.path(PROJ_ROOT, "training_designs"),
       why  = "the contract routes every design artefact to output/designs"),
  list(keep = if (exists("PAPER_FIGURES")) PAPER_FIGURES else NA_character_,
       drop = file.path(PROJ_ROOT, "els-cas-templates", "figs"),
       why  = "a sibling of figures/ that has held a duplicate manuscript"),
  list(keep = if (exists("DIR_FIGURES")) DIR_FIGURES else NA_character_,
       drop = file.path(PROJ_ROOT, "output", "figures"),
       why  = "nothing writes there and the contract does not name it"))

cat("\n  duplicate artefact locations:\n")
for (.p in .fp_dups) {
  if (is.na(.p$drop) || !dir.exists(.p$drop)) next
  .n <- length(list.files(.p$drop, recursive = TRUE))
  cat(sprintf("    %-46s %3d file(s)\n", .p$drop, .n))
  cat("      keep: ", .p$keep, "\n      because ", .p$why, "\n", sep = "")
  if (isTRUE(FP_CLEANUP) && .n > 0L && !is.na(.p$keep) && dir.exists(.p$keep)) {
    .dup <- intersect(list.files(.p$drop), list.files(.p$keep))
    cat("      FP_CLEANUP: removing ", length(.dup),
        " name(s) that exist in both.\n", sep = "")
    unlink(file.path(.p$drop, .dup), recursive = TRUE)
  }
}
if (!isTRUE(FP_CLEANUP))
  cat("  FP_CLEANUP is FALSE, so nothing was removed. Read the list, then\n",
      "  set FP_CLEANUP <- TRUE and source this file again, or remove them\n",
      "  by hand. A folder emptied here is recreated by the contract on the\n",
      "  next load unless the contract stops naming it.\n", sep = "")
rm(.fp_dups)
if (exists(".p")) rm(.p)
if (exists(".n")) rm(.n)
if (exists(".dup")) rm(.dup)

# THE FILES THAT MOVED, WHICH IS A DIFFERENT PROBLEM FROM THE FOLDERS THAT
# DID. Two artefacts changed destination and the old copies do not delete
# themselves. headline_figures.csv now goes to output/designs and used to go
# to output/tables; the six triage fragments now go to output/diagnostics and
# used to go to output/tables. Left behind, the csv is found in two folders
# by read_csv_if(), which takes the first in precedence order and says so in
# a line that scrolls past, and the fragments sit beside the ones the
# manuscript \input{}s, which is the collision the move was made to end.
#
# A MOVE IS NOT FINISHED UNTIL THE OLD COPY IS GONE. This is the one place
# in the pass that can see both, so it looks here rather than trusting that
# someone tidied up.
.fp_moved <- c("headline_figures.csv",
               outer(c("tab1_descriptives_", "tab2_couples_", "tab_samples_"),
                     c("20-55", "30-49"), paste0))
.fp_stale <- character(0)
for (.f in .fp_moved) {
  .where <- Filter(function(d) !is.na(d) && dir.exists(d) &&
                     length(list.files(d, pattern = paste0("^", .f))),
                   c(get0("DIR_TABLES",  ifnotfound = NA),
                     get0("DIR_DESIGNS", ifnotfound = NA),
                     get0("DIR_DIAG",    ifnotfound = NA)))
  if (length(.where) > 1L) {
    cat("\n  ", .f, " is in ", length(.where), " folders:\n", sep = "")
    for (.d in .where) cat("      ", .d, "\n", sep = "")
    .fp_stale <- c(.fp_stale, .f)
  }
}
if (length(.fp_stale))
  .fp_stop("the copies these artefacts were moved out of are still there.\n",
           "  Delete the ones under output/tables. paper_tables.R will",
           " otherwise read\n  whichever comes first in PT_CSV_DIRS and the",
           " precedence, not the vintage,\n  will decide which numbers reach",
           " the paper.")
rm(.fp_moved, .fp_stale)
if (exists(".where")) rm(.where)
if (exists(".f")) rm(.f)
if (exists(".d")) rm(.d)

# THE TWO TABLE FOLDERS, AND WHY ONE OF THEM BEING EMPTY MATTERS.
#
# DIR_TABLES is not an output directory the way its name suggests. It is
# TEXDIR, the folder paper_tables.R BUILDS into, and pt_sync_paper() then
# mirrors the fragments that carry no \tofill across to the manuscript's own
# tables/ folder. So there are two copies by design, and the build copy is
# the one that can be deleted safely.
#
# Safely, but not without effect. write_body() refuses to replace a real
# fragment with an all-placeholder one, and that refusal reads the file
# already on disk in TEXDIR. An empty build folder has nothing to refuse
# against, so on this pass any table whose upstream input did not run writes
# \tofill instead of being held back. The gate catches it downstream:
# pt_sync_paper() withholds the placeholder and pt_gate_check() reports the
# page as reading an older build. Nothing reaches the PDF that should not.
# What it does mean is that a pass starting from an empty build folder must
# genuinely run every stage, because "kept the good fragment" is no longer
# available as a silent fallback. For the final pass that is the behaviour
# you want, which is why this reports rather than stops.
#
# The manuscript's own folder is different. If that is empty the compile
# halts on missing \input, and no amount of rebuilding in this session helps
# until paper_tables.R has synced. That one is an assertion.
.fp_build <- get0("DIR_TABLES",   ifnotfound = NA_character_)
.fp_page  <- get0("PAPER_TABLES", ifnotfound = NA_character_)
.fp_nb <- if (!is.na(.fp_build) && dir.exists(.fp_build))
  length(list.files(.fp_build, pattern = "[.]tex$")) else 0L
.fp_np <- if (!is.na(.fp_page) && dir.exists(.fp_page))
  length(list.files(.fp_page, pattern = "[.]tex$")) else 0L
cat("\n  table fragments: ", .fp_nb, " in the build folder, ", .fp_np,
    " on the page.\n", sep = "")
if (.fp_nb == 0L)
  cat("    The build folder is empty, so write_body()'s refusal has nothing\n",
      "    to protect this pass. Every stage has to run for the gate to come\n",
      "    back clean; anything skipped will show as stale or orphaned.\n",
      sep = "")
if (.fp_np == 0L)
  .fp_stop("the manuscript's tables folder is empty:\n  ", .fp_page,
           "\n  Every \\input in the draft will fail. paper_tables.R has to",
           " build and sync\n  before the document can compile at all.")
rm(.fp_build, .fp_page, .fp_nb, .fp_np)

# headline_figures.csv IS AN INPUT TO THIS PASS AND IS WRITTEN OUTSIDE IT.
#
# Pre_analysis_diagnostics2.R writes it and nothing in the sequence below
# does. paper_tables.R reads it for the birth-item validation and degrades
# politely when it is absent, which is the problem: the validation simply
# goes unreported and the run still ends clean.
#
# IT MUST RUN IN ITS OWN SESSION, NOT THIS ONE. That script assigns a global
# `panel` at its line 174 from its own screen rule, and paper_tables.R
# prefers an in-memory `panel` over td_frame(). Sourced into this session it
# would hand the tables a differently screened frame, which is the fault the
# v35.1 note in paper_tables.R describes. Step 4d runs it in a child process
# instead, which is its own session, so this only reports.
.fp_hf <- get0("DIR_DESIGNS", ifnotfound = NA_character_)
if (!is.na(.fp_hf) &&
    !file.exists(file.path(.fp_hf, "headline_figures.csv")))
  cat("\n  headline_figures.csv is not in ", .fp_hf, ".\n",
      "    Step 4d writes it, in its own process, before the tables.\n", sep = "")
rm(.fp_hf)

# -----------------------------------------------------------------------------
# 2. FORGET, so nothing carries a vintage
# -----------------------------------------------------------------------------
.fp_say("2. forget")
rm(list = intersect("panel", ls(envir = globalenv())), envir = globalenv())
if (exists("td_forget", mode = "function")) td_forget() else
  cat("  td_forget() is not defined; the contract did not load its cache.\n")

# -----------------------------------------------------------------------------
# 3. THE STAGES, NAMED
# -----------------------------------------------------------------------------
# RUN_STAGES IS AN ORDINARY GLOBAL AND PERSISTS. RUN_ALL.R removes it once it
# has read it, so a pass that reaches that line leaves nothing behind; a pass
# that dies before it does. Naming all four contributors here is not belt and
# braces, it is the difference between the ledger being rebuilt from empty and
# being merged over whatever the last partial pass left.
#
# 02 IS IN THE LIST BECAUSE 03c CANNOT BE TRUSTED WITHOUT IT.
#
# It looks like a descriptive stage and it is not. 02fte_experience.R writes
# panel_fte_addendum.rds, and 03c reads all five hybrid experience stocks out
# of it: exp_hybrid_cal, exp_hybrid_one_obs, exp_hybrid_one_int,
# exp_hybrid_fte_obs and exp_hybrid_fte. When the addendum is absent 03c says
# so and skips; when it is an older revision 03c rebuilds the one-sided
# stocks locally and prints that the rebuild is NOT guaranteed to match 02's
# span grid, because 02 accumulates over a filled person-by-wave span and the
# fallback accumulates over observed rows only. Either way the twenty arms,
# the divergence gate and Appendix A would rest on a construction 03c itself
# declines to vouch for.
#
# THREE THINGS MAKE IT FREE. It is declared at three minutes against 03c's
# thousand. It writes an addendum and ten csvs and does not touch the panel,
# so no count in the manuscript can move because it ran. And it is not one of
# LEDGER_CONTRIBUTORS, which is the four 03 and 04 stages, so adding it
# leaves .full_ledger TRUE and the reset still fires.
#
# ORDER IS NOT DECIDED HERE. RUN_ALL.R filters its own STAGES list by name,
# so the stages run in its order, loader then 02 then 03a, whatever order
# they are written in below.
#
# THE LOADER IS THE ONE JUDGEMENT CALL LEFT. Adding "loader" costs a minute
# and its own expectation asserts the panel came back with 0 columns dropped,
# 0 added and 0 changed, which is the only direct evidence that the data
# under this pass did not move. It also rewrites the panel, so if it HAS
# moved, every count in the draft moves with it. Include it when the pass is
# the one the submitted PDF is built from; leave it out while iterating.
.fp_say("3. RUN_ALL, the addendum and all four contributors")
RUN_STAGES <- c("02", "03a", "03b", "03c", "04")
source("RUN_ALL.R")

# THE ASSERTION IS ON THE VARIABLE, NOT ON THE CONSOLE. RUN_ALL.R sets
# .full_ledger TRUE only when all four contributors are in the pass, and
# prints "MERGED, not rebuilt" otherwise. Reading the flag rather than the
# printed line means this cannot be satisfied by a message that happens to
# scroll past.
if (!isTRUE(get0(".full_ledger", ifnotfound = FALSE)))
  .fp_stop("RUN_ALL merged the ledger rather than rebuilding it, so at least\n",
           "  one module's rows carry a timestamp from an earlier pass. The\n",
           "  28 / 12 / 20 / 30 counts cannot all be this run's.")

# -----------------------------------------------------------------------------
# 4. THE MASTER, THE FIGURES, THE TABLES
# -----------------------------------------------------------------------------
.fp_say("4. master")
source("MASTER_training_y.R")

# 4a. THE REFEREE BOOTSTRAP, AFTER THE MASTER, IN ITS OWN PROCESS.
#
# REFEREE_RUN.R writes the referee_* files in output/designs/, and the text
# quotes five numbers from them, the fee-state shares among them. It needs
# the frame in memory as `panel` and the contract's folders in scope. On 2
# October this step handed it to RUN_ALL, whose child process carries
# neither, and it stopped at its first check. It now runs in a child process
# of its own that loads the contract and the frame first, the way step 4d
# runs the pre-analysis file. It takes about half an hour.
.fp_say("4a. referee bootstrap (REFEREE_RUN.R in its own process)")
.fp_rs <- file.path(R.home("bin"),
                    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
if (!file.exists(.fp_rs)) .fp_rs <- Sys.which("Rscript")
.fp_cap <- file.path(get0("LOG_DIR", ifnotfound = file.path(getwd(), "run_logs")),
                     sprintf("FINAL_PASS_referee_%s_console.log",
                             format(Sys.time(), "%Y%m%d_%H%M%S")))
.fp_code <- system2(.fp_rs, c("--no-save", "--no-restore", "-e",
  shQuote(sprintf(paste0('setwd("%s"); source("00_contract.R"); ',
                         'panel <- td_frame(context = "referee"); ',
                         'source("REFEREE_RUN.R", echo = TRUE)'), getwd()))),
  stdout = .fp_cap, stderr = .fp_cap)
.fp_rv <- file.path(get0("DIR_DESIGNS", ifnotfound = file.path(getwd(), "output", "designs")),
                    "referee_G_fee_states_verdict.csv")
if (!identical(as.integer(.fp_code), 0L) || !file.exists(.fp_rv) ||
    file.mtime(.fp_rv) < FP_T0)
  .fp_stop("REFEREE_RUN.R did not rewrite its verdicts.\n  Its console is in ",
           .fp_cap, ", and its own log is the newest REFEREE_RUN_ file in run_logs.")
cat("  referee verdicts rewritten: ", format(file.mtime(.fp_rv), "%H:%M"), "\n", sep = "")
rm(.fp_rs, .fp_cap, .fp_code, .fp_rv)

# 4b. THE REVIEW RESPONSE, AFTER THE MASTER AND BEFORE THE TABLES.
#
# 05_review_response.R reads ev from 03a and cs_event_study_results.rds,
# which the master rewrites through CS_event_study.R, so it cannot run
# earlier. Its block R16 writes the Appendix I tables into output/tables/,
# and step 6 syncs them to the manuscript, so it cannot run later: a pass
# that ran it after paper_tables.R would compile against the previous
# copies, and step 9 would name them. All blocks run, whatever RR_BLOCKS an
# earlier session left in the environment, so every appendix number is
# estimated this session rather than rewritten from an older handoff.
.fp_say("4b. review response (05, all blocks)")
Sys.setenv(RR_BLOCKS = "all")
source("05_review_response.R")
# THE REPRODUCTION BLOCK, ASSERTED. R0 re-estimates numbers the paper prints
# on 05's own frames; a miss means the appendix was estimated on something
# other than what the main text was.
.fp_rep <- tryCatch(rr$tabs$reproduction, error = function(e) NULL)
if (is.null(.fp_rep) || !nrow(.fp_rep) || any(is.na(.fp_rep$match)) ||
    !all(.fp_rep$match))
  .fp_stop("05's reproduction block did not match every number it checks.\n",
           "  Read its R0 table above before trusting any appendix table.")
# EVERY APPENDIX TABLE THE TEXT CITES, WRITTEN. A table R16 skips for want of
# results leaves its \ref unresolved, which LaTeX prints as ?? and which no
# number check notices.
.fp_tex  <- get0("MANUSCRIPT", ifnotfound = NA_character_)
.fp_tdir <- get0("DIR_TABLES", ifnotfound = file.path(
  get0("PROJ_ROOT", ifnotfound = getwd()), "output", "tables"))
if (length(.fp_tex) == 1L && !is.na(.fp_tex) && file.exists(.fp_tex)) {
  .fp_s <- paste(readLines(.fp_tex, warn = FALSE), collapse = "\n")
  .fp_cited <- unique(unlist(regmatches(.fp_s, gregexpr(
    "(?<=\\\\ref\\{)tab:ph_[^}]+", .fp_s, perl = TRUE))))
  .fp_have <- unique(unlist(lapply(
    list.files(.fp_tdir, pattern = "^PH_.*[.]tex$", full.names = TRUE),
    function(f) {
      x <- paste(readLines(f, warn = FALSE), collapse = "\n")
      regmatches(x, gregexpr("(?<=\\\\label\\{)tab:ph_[^}]+", x, perl = TRUE))[[1]]
    })))
  .fp_gap <- setdiff(.fp_cited, .fp_have)
  if (length(.fp_gap))
    .fp_stop("the text cites appendix tables R16 did not write: ",
             paste(.fp_gap, collapse = ", "), "\n  Its log lists the results",
             " it was missing.")
  cat("  appendix tables: ", length(.fp_cited), " cited, all written to ",
      .fp_tdir, "\n", sep = "")
  rm(.fp_s, .fp_cited, .fp_have, .fp_gap)
} else {
  cat("  MANUSCRIPT is not bound, so the appendix labels were not checked.\n")
}
rm(.fp_rep, .fp_tex, .fp_tdir)

# 4c. THE WAVE 24 CALENDAR CHECK. The paper quotes three numbers from it, and
# its csv sits in output/results/, so step 9 would stop on a copy left by an
# earlier session. It reads only the raw Wave 24 file, so its place in the
# pass is free; it sits beside the other post-hoc evidence. The comparison
# uses the script's own W24_QUOTED, so the paper's numbers live in one place.
.fp_say("4c. Wave 24 calendar check")
source("w24_calendar_check.R")
.fp_w24 <- tryCatch(utils::read.csv(td_out("results", "w24_calendar_check.csv"),
                                    stringsAsFactors = FALSE),
                    error = function(e) NULL)
.fp_q <- get0("W24_QUOTED", ifnotfound = c(persons = 528, median = 100,
                                            mean_180 = 96.5))
.fp_g <- if (is.null(.fp_w24)) rep(NA_real_, 3L) else c(
  .fp_w24$persons[.fp_w24$group == "took parental leave"][1],
  .fp_w24$median_capj[.fp_w24$group == "took parental leave"][1],
  .fp_w24$mean_capj[.fp_w24$group == "leave days (180,400]"][1])
if (anyNA(.fp_g) || any(abs(.fp_g - .fp_q) > c(0.5, 0.05, 0.05)))
  .fp_stop("the Wave 24 check does not give the paper's numbers: ",
           paste(sprintf("%s %s (paper %s)", names(.fp_q), format(.fp_g),
                         format(.fp_q)), collapse = "; "), ".")
rm(.fp_w24, .fp_q, .fp_g)

# 4d. headline_figures.csv, WRITTEN THIS PASS IN ITS OWN PROCESS.
#
# Pre_analysis_diagnostics2.R writes it, and paper_tables.R reads it for the
# birth-item validation. It cannot be sourced into this session, because it
# assigns its own global `panel` (see the note before step 3), so it used to
# run outside the pass and step 9 could never come back clean. A child
# Rscript process is its own session, the way RUN_ALL runs every stage, so
# the file is now written inside the pass from this pass's panel.
.fp_say("4d. Pre_analysis_diagnostics2.R in its own process (headline_figures.csv)")
.fp_rs <- file.path(R.home("bin"),
                    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
if (!file.exists(.fp_rs)) .fp_rs <- Sys.which("Rscript")
.fp_cap <- file.path(get0("LOG_DIR", ifnotfound = file.path(getwd(), "run_logs")),
                     sprintf("FINAL_PASS_pre_analysis_%s_console.log",
                             format(Sys.time(), "%Y%m%d_%H%M%S")))
.fp_code <- system2(.fp_rs, c("--no-save", "--no-restore", "-e",
  shQuote(sprintf('setwd("%s"); source("Pre_analysis_diagnostics2.R", echo = TRUE)',
                  getwd()))), stdout = .fp_cap, stderr = .fp_cap)
.fp_hf <- file.path(get0("DIR_DESIGNS", ifnotfound = file.path(getwd(), "output", "designs")),
                    "headline_figures.csv")
if (!identical(as.integer(.fp_code), 0L) || !file.exists(.fp_hf) ||
    file.mtime(.fp_hf) < FP_T0)
  .fp_stop("Pre_analysis_diagnostics2.R did not rewrite headline_figures.csv.\n",
           "  Its console is in ", .fp_cap, ".")
cat("  headline_figures.csv rewritten: ", format(file.mtime(.fp_hf), "%H:%M"), "\n", sep = "")
rm(.fp_rs, .fp_cap, .fp_code, .fp_hf)

# THE FRAME THE TWO BUILDERS GET, SET EXPLICITLY RATHER THAN INHERITED.
#
# RUN_FIGURES_PAPER.R and paper_tables.R both prefer an in-memory `panel`
# over resolving their own, and neither checks its width. On 10 September
# they were handed a ten-column frame left in the session and produced three
# stale figures, twelve \tofill macros, a two-row descriptives table and an
# empty app_b2_profile. Every one of those looked like a content failure and
# all of them were one frame.
#
# td_frame() REFUSES A NARROW FRAME, which is why it is the route here: its
# guard names the missing columns and stops rather than persisting them into
# handoff/. The frame also carries the pre-screen stamp, so paper_tables.R
# can write \ExtractRows, \ExtractPersons and \ScreenDropped.
#
# PUB AND HALF ARE SET, NOT DEFAULTED. Both have arrived INHERITED in every
# recent figures run. Setting them here means an INHERITED note in the log
# always signals a leftover rather than ambiguity about which default won.
.fp_frame <- function(stage) {
  rm(list = intersect("panel", ls(envir = globalenv())), envir = globalenv())
  assign("panel", td_frame(context = stage), envir = globalenv())
  cat("  frame for ", stage, ": ", format(nrow(panel), big.mark = ","),
      " rows x ", ncol(panel), " cols\n", sep = "")
  if (ncol(panel) < 100)
    .fp_stop("the frame has ", ncol(panel), " columns. The builders need the",
             " full panel;\n  a narrow one yields stale figures and \\tofill",
             " macros that read as content faults.")
}

.fp_say("5. figures")
.fp_frame("figures")
PUB <- TRUE; HALF <- "both"
source("RUN_FIGURES_PAPER.R")
# FIGDIR is resolved inside that file and is the one thing about it worth
# asserting from outside: a run that wrote to the wrong folder produces a
# complete, current set of figures that the manuscript does not read.
if (exists("FIGDIR") && exists("PAPER_FIGURES") &&
    !identical(normalizePath(FIGDIR, mustWork = FALSE),
               normalizePath(PAPER_FIGURES, mustWork = FALSE)))
  .fp_stop("FIGDIR is ", FIGDIR, "\n  and the manuscript reads ",
           PAPER_FIGURES, ".\n  The figures written this pass are not the",
           " ones that will compile.")

.fp_say("6. tables")
# AGAIN, because RUN_FIGURES_PAPER.R and anything it sources may have left a
# different object in `panel`. Cheap: td_frame() returns the registry copy.
.fp_frame("tables")
source("paper_tables.R")

# THE TWO GATES THE TODO LIST NAMES, ASSERTED RATHER THAN READ.
#
# paper_tables.R already calls both at its foot and prints their verdicts.
# Calling them again here costs a directory listing and turns two lines of
# console output into a stop, which is the difference between a check that
# ran and a check that was acted on. pt_gate_check() returns FALSE on a
# placeholder that reached the page, a fragment differing from this build,
# or one on the page with no generator; the last is the state an emptied
# build folder produces for anything this pass did not rebuild.
if (exists("pt_gate_check", mode = "function") && !isTRUE(pt_gate_check()))
  .fp_stop("the table gate did not come back clean. The page is compiling\n",
           "  against fragments this build did not produce, so the PDF and",
           " the\n  estimates behind it would not share a vintage.")
if (exists("pt_csv_report", mode = "function")) pt_csv_report()

# THE RECONCILIATION, LAST BEFORE THE VERIFIER AND FOR A REASON.
#
# WHAT IT IS FOR. reconcile_pre_birth_base.R writes two csvs into
# DIR_DESIGNS. The blocks one holds 130 men and 191 women asked without
# being flagged employed, at rates 0.0846 and 0.0995, which Section 4.4
# prints as 321 person-waves training at 8.5 and 10.0 per cent. Until it
# runs those are three literals with nothing behind them, so this is an
# input to Check 9 rather than a diagnostic beside it.
#
# WHY HERE RATHER THAN AFTER THE MASTER. It assigns d0, tab, add and steps
# at top level, and RUN_FIGURES_PAPER.R reads d0 sixteen times while
# paper_tables.R uses tab. Both assign their own first, so nothing would
# actually break, but a script whose leavings collide by name with two
# later stages should run after them when it costs nothing to do so. It
# prefers td_frame(), which is sealed and available at any point in the
# pass, so it needs nothing the earlier position would have given it.
#
# READ-ONLY. It creates no object any other script uses and writes only
# its two csvs, which is what makes the position a free choice.
if (file.exists("reconcile_pre_birth_base.R")) {
  .fp_say("7. pre-birth base reconciliation")
  source("reconcile_pre_birth_base.R")
} else {
  cat("\n  reconcile_pre_birth_base.R not found. Section 4.4's 321, 8.5 and\n",
      "  10.0 will have no artefact behind them and Check 9 will say so.\n",
      sep = "")
}
# THE COUNT RECONCILIATION, FOR THE SAME REASON AS THE BLOCK ABOVE.
#
# WHAT IT IS FOR. RECONCILE_COUNTS.R writes reconcile_counts.csv into
# DIR_DESIGNS, and that file is what puts an artefact behind the couple
# counts: 5,041 dual-earner couples, 23,508 couple-waves and 47,016
# person-waves on the heterosexual both-waged rung, 6,518 and 37,495 and
# 74,989 on the same-wave frame, and Section 4.3's 14,466 and 1,159. Until
# it runs those are literals Check 9 can only match by coincidence, which
# is why 23,508 sat on CHECK9_ACCEPTED with nothing behind it.
#
# WHY IT BELONGS IN THE PASS. It is a RUN_ALL stage, id "counts", and this
# file does not call RUN_ALL with it. So on 15 September the csv was
# written from the console and nothing tied its vintage to the ledger's. A
# file Check 9 traces the paper's counts to, which no pass rebuilds, will
# eventually describe an older build while reporting a clean match. That is
# the same shape as the T8 artefacts reconciling Table 3 against files two
# days older than the ledger.
#
# IT NEEDS `panel` AND DEGRADES RATHER THAN STOPS WITHOUT IT. From a cold
# session it reports 1 of 5 assertions passing: block B prints "partner_id
# absent from panel; Section B cannot run" and four of block C's five rows
# come back NO FRAME, and the summary still counts assertions and reads as
# a result. So the frame is bound here explicitly.
if (file.exists("RECONCILE_COUNTS.R")) {
  .fp_say("7b. count reconciliation")
  if (exists(".fp_frame", mode = "function")) {
    .fp_frame("count reconciliation")
  } else if (exists(".fp_reload_panel", mode = "function")) {
    panel <- .fp_reload_panel("count reconciliation")
  } else {
    rm(list = intersect("panel", ls(envir = globalenv())), envir = globalenv())
    assign("panel", td_frame(context = "count reconciliation"),
           envir = globalenv())
  }
  WRITE_REPORT <- TRUE
  source("RECONCILE_COUNTS.R")
  WRITE_REPORT <- FALSE
  if (exists("reconcile_ok") && !isTRUE(reconcile_ok))
    .fp_stop("RECONCILE_COUNTS.R reported a failing assertion. A manuscript\n",
             "  count no longer matches the cell it is asserted to come from,\n",
             "  and Check 9 below would still trace it to the stale csv.")
} else {
  cat("\n  RECONCILE_COUNTS.R not found. The couple counts and Section 4.3's\n",
      "  triple will have no artefact behind them; 23,508 and the rest fall\n",
      "  back to whatever else in the corpus happens to contain the string.\n",
      sep = "")
}


# -----------------------------------------------------------------------------
# 5. THE VERIFIER
# -----------------------------------------------------------------------------
.fp_say("8. verifier")
# v-settings. THE SETTINGS FIRST. vm_settings.R holds the accepted literals,
# the confirmed and rejected fractions and the LaTeX names. Without it the
# verifier falls back to its built-in lists and reports every settled entry
# as a failure, which is how the 1 October pass showed fifty in Check 9 and
# four in Check 10. It also sets WRITE_REPORT, so the line after it wins.
if (!file.exists("vm_settings.R"))
  .fp_stop("vm_settings.R is not in ", getwd(), "; the verifier needs it.")
source("vm_settings.R")
WRITE_REPORT <- TRUE
source("verify_manuscript_numbers.R")

# CHECK 10 IS FATAL HERE. It lists the macros the manuscript uses that fall
# back to \tofill, each of which renders as a red rule in the PDF. The
# control run reported sixteen and nothing stopped; a draft compiled from
# that pass carries sixteen visible gaps. The verifier reports, this file
# decides, which is the division everywhere else in this pass.
#
# READ THE VERIFIER'S OWN VERDICT, NOT THE FRAGMENT.
#
# THE FIRST VERSION OF THIS GUARD WAS WRONG AND SAID SO LOUDLY. It compared
# \providecommand names in data_section_numbers.tex against \newcommand
# names and called the difference a failure list. That file deliberately
# emits a fallback for anything macro-shaped, LaTeX's own \Delta,
# \FloatBarrier and \RaggedRight among them, because \providecommand is a
# no-op on a name already defined: paper_tables.R line 3676 puts it as "emit
# broadly; report narrowly". So the guard reported thirteen gaps on a run
# whose Check 10 said none, and one of the thirteen was a comment fragment,
# because sub() returns a non-matching line unchanged instead of dropping it.
#
# THE NARROW SET IS ALREADY COMPUTED TWICE, by paper_tables.R against
# .tex_own and by Check 10 in the verifier. Recomputing it here would be a
# third implementation of one rule, and the first attempt shows what that
# costs. The verifier's line is the thing to read.
#
# WHAT COUNTS AS CLEAN. Check 10 prints "none falling back to \tofill, no
# fragment carrying one" when every macro the manuscript uses resolved.
# Anything else is a draft that compiles with visible gaps.
.fp_vlog <- if (exists("VM_LOG") && file.exists(get0("VM_LOG", ifnotfound = "")))
  VM_LOG else {
    .l <- list.files(get0("LOG_DIR", ifnotfound = "."),
                     pattern = "^verify_manuscript_.*\\.log$", full.names = TRUE)
    if (length(.l)) .l[order(file.mtime(.l), decreasing = TRUE)][1] else NA_character_
  }
if (!is.na(.fp_vlog) && file.exists(.fp_vlog)) {
  # v-c10. THE VERDICT ROW, NOT A PHRASE. The verifier prints "none falling
  # back to \tofill" when clean and "N macro(s) fall back to \tofill" when
  # not, so the phrase this guard searched for matched only a clean run, and
  # the 1 October failure passed as "verdict line not found". The row in the
  # verdict table carries the count either way.
  .fp_vl  <- readLines(.fp_vlog, warn = FALSE)
  .fp_row <- grep("^\\s*10\\s+[0-9]+\\s+[0-9]+\\s+macro", .fp_vl, value = TRUE)
  .fp_c10 <- grep("fall(ing)? back to .tofill|never defined", .fp_vl, value = TRUE)
  .fp_bad <- if (length(.fp_row)) suppressWarnings(
    as.integer(strsplit(trimws(.fp_row[1]), "\\s+")[[1]][3])) else NA_integer_
  if (length(.fp_c10)) cat("  Check 10: ", trimws(.fp_c10[1]), "\n", sep = "")
  if (is.na(.fp_bad)) {
    cat("  Check 10's verdict row was not found in the verifier log.\n")
  } else if (.fp_bad > 0L) {
    .fp_stop("the manuscript would compile with visible gaps. Each is a value",
             " a\n  script owes you; the verifier log names them.")
  }
  rm(.fp_vl, .fp_row, .fp_c10, .fp_bad)
} else {
  cat("  no verifier log found, so Check 10 could not be read.\n")
}
rm(.fp_vlog)
if (exists(".l")) rm(.l)

# -----------------------------------------------------------------------------
# 6. ONE VINTAGE, ASSERTED
# -----------------------------------------------------------------------------
# THIS IS THE CHECK NO INDIVIDUAL CHECK CAN MAKE. Everything above verifies a
# value; this verifies that the values were produced together. Anything the
# manuscript reads that predates this session is named, whether or not its
# value happens to be right, because a correct number of the wrong vintage is
# the failure this whole procedure exists to prevent.
.fp_say("9. vintages")
# output/fte_experience IS RESOLVED BY HAND BECAUSE THE CONTRACT DOES NOT
# NAME IT. 02fte_experience.R sets FTE_DIR locally to
# PROJ_ROOT/output/fte_experience, 03c searches a list of candidates for the
# addendum, and verify_manuscript_numbers.R builds its own guess off
# dirname(OUTDIR). Three independent resolutions of one directory, and none
# of them is DIR_*, so a scan over the sealed names alone cannot see the
# addendum and a stale one would pass this assertion in silence. Reproduced
# here rather than left out; naming it in the contract is the real fix and
# needs the three readers changed with it.
.fp_fte <- file.path(get0("PROJ_ROOT", ifnotfound = getwd()),
                     "output", "fte_experience")
.fp_dirs <- Filter(dir.exists, c(get0("DIR_RESULTS", ifnotfound = NA),
                                 get0("DIR_DESIGNS", ifnotfound = NA),
                                 get0("DIR_TABLES",  ifnotfound = NA),
                                 get0("PAPER_FIGURES", ifnotfound = NA),
                                 .fp_fte))
.fp_f <- list.files(.fp_dirs, full.names = TRUE, recursive = TRUE,
                    pattern = "[.](csv|rds|tex|pdf|png)$")
.fp_old <- .fp_f[file.mtime(.fp_f) < FP_T0]
if (length(.fp_old)) {
  cat("\n  ", length(.fp_old), " of ", length(.fp_f),
      " artefacts predate this session:\n", sep = "")
  .fp_ord <- .fp_old[order(file.mtime(.fp_old))]
  for (.f in head(.fp_ord, 20L))
    cat(sprintf("    %s  %s\n",
                format(file.mtime(.f), "%Y-%m-%d %H:%M"), .f))
  if (length(.fp_ord) > 20L)
    cat("    ... and ", length(.fp_ord) - 20L, " more.\n", sep = "")
  .fp_stop("the pass did not rewrite every artefact the manuscript reads.")
} else {
  cat("\n  all ", length(.fp_f), " artefacts were written this session.\n",
      sep = "")
}
rm(.fp_dirs, .fp_f, .fp_old, .fp_fte)
if (exists(".fp_ord")) rm(.fp_ord)
if (exists(".f")) rm(.f)

.fp_say("done. Elapsed ",
        format(round(difftime(Sys.time(), FP_T0, units = "mins"), 1)))
cat("  Next: compile the manuscript, then source verify_manuscript_numbers.R\n",
    "  once more against the compiled draft. Keep this session's run_logs:\n",
    "  they are the only evidence the PDF and the estimates share a vintage.\n",
    sep = "")
