# =====================================================================
# RUN_ALL.R  -  one entry point, one report
#
# THE PROBLEM IS THE INTERFACE, NOT THE COUNT. Thirteen scripts is not
# many for a pipeline this size, and splitting 03 into common, a, b, c and
# finish was right. What is expensive is that running it correctly
# requires knowing which files are RUN and which are only ever SOURCED,
# in what order, which need a fresh session, and what each one's output
# should say. That knowledge lived in a plan document and in whoever ran
# it last, and every failure this month came from a gap in it: a module
# sourced directly, a warm session carrying a stale flag, a transcript
# left open, an addendum older than the panel it was joined to.
#
# So this file is not another script in the pipeline. It is the only one
# anybody needs to run, and it makes the checklist executable rather than
# readable.
#
# EACH STAGE RUNS IN ITS OWN PROCESS. That is not tidiness. A fresh
# process makes rm(list = ls()) irrelevant, gives every module its own
# transcript, and stops one script's error leaving a sink open for the
# next, which is three of this month's failures removed by construction
# rather than by guard. The handoff registry is designed for it; 04's own
# preflight says a healthy pipeline has no global `ev`.
#
# THE EXPECTATIONS ARE THE POINT. Every stage carries the values its log
# should contain, taken from runs already verified. A stage that completes
# with the wrong numbers is the failure mode that costs weeks, because
# nothing errors and the output looks like output. Checking a handful of
# invariants per stage catches it in the minute it happens.
#
#   Rscript RUN_ALL.R                 everything, ~14 hours (03c is 16)
#   Rscript RUN_ALL.R fast            everything except 03c and REFEREE
#   Rscript RUN_ALL.R 03a 03b 04      named stages only
#   Rscript RUN_ALL.R verify          the checking pass alone
#
# WHAT IT DELIBERATELY DOES NOT DO. It does not decide whether the panel
# needs rebuilding, and it does not skip a stage because its outputs look
# recent. Both are judgements, and a driver that makes them quietly is how
# a stale artefact survives a full pass.
# =====================================================================

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select <- dplyr::select;       filter    <- dplyr::filter
mutate <- dplyr::mutate;       slice     <- dplyr::slice
recode <- dplyr::recode;       rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange <- dplyr::arrange;     count     <- dplyr::count;  lag <- dplyr::lag

ROOT    <- if (nzchar(Sys.getenv("HILDA_ROOT"))) Sys.getenv("HILDA_ROOT") else getwd()
LOGS    <- file.path(ROOT, "run_logs")
# .exe on Windows, and check it exists before promising to run anything.
RSCRIPT <- file.path(R.home("bin"),
                     if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
if (!file.exists(RSCRIPT)) RSCRIPT <- Sys.which("Rscript")
if (!nzchar(RSCRIPT) || !file.exists(RSCRIPT))
  stop("Rscript not found at ", file.path(R.home("bin"), "Rscript"),
       ". Set RSCRIPT by hand.", call. = FALSE)
dir.create(LOGS, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------
# The version a script declares, so a check on it cannot go stale. Returns a
# regex matching that exact version, or one matching any version when the
# file cannot be read, because a check that cannot be evaluated must not
# fail a stage that ran.
.fs_declared_version <- function(
    path = file.path(if (exists("ROOT")) ROOT else ".", "04financing_space.R")) {
  if (!file.exists(path)) return("v[0-9]+\\.[0-9]+")
  ln <- grep("^FS_VERSION <-", readLines(path, warn = FALSE), value = TRUE)
  v  <- regmatches(ln, regexpr("v[0-9]+\\.[0-9]+", ln))
  if (!length(v)) "v[0-9]+\\.[0-9]+" else gsub("\\.", "\\\\.", v[1])
}

# The pipeline. `expect` is a named list of regexes the stage's own log
# must match; the name is what gets printed when it does not.
#
# SOURCED, NOT RUN, and therefore absent from this table: 00_contract.R,
# 03_logging.R, 03_common.R, 03_finish.R, training_event_study.R,
# training_funding_divergence.R, CS_event_study.R. Each is pulled in by
# the file that needs it. Running them by hand is what produced a
# transcript named 03_unnamed_caller and, later, one named 03c holding
# 04's output.
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# v-screen, 14 October. TWO SETS OF EXPECTATIONS, chosen by TD_SCREEN.
#
# 00_contract.R v28 applies no screen by default, so the default pass runs
# on all 134,131 person-waves and the roster grows from 10,838 persons to
# 10,902. TD_SCREEN=on brings the screen back for the sensitivity run. A
# check that depends on the sample carries both values, so either pass is
# checked against its own numbers, and the screened values stay on record
# as the test of the sensitivity run.
#
# The values without the screen come from screen_audit.R, which rebuilt the
# roster both ways and reproduced the screened one exactly. The checks it
# could not predict are listed in EXPECTED_TO_MOVE, below STAGES.
.SCREEN_ON <- identical(tolower(trimws(Sys.getenv("TD_SCREEN", "off"))), "on")
.by_screen <- function(on, off) if (.SCREEN_ON) on else off

STAGES <- list(
  # v21.49. ever_parent counts own children only and educ_years is a
  # running maximum, so the parenthood counts moved. These values come
  # from the verified 5 October run. Before v21.49 the cummax read
  # 82,034 -> 82,479 (gained 445) and the four-way split 6,245. The last
  # three checks confirm that the v21.49 block ran.
  #
  # halt_on: if any of these fails, the pass is not on the v21.49 panel,
  # so it stops here rather than run every later stage on the wrong one.
  # "panel unchanged" is left off, because a missing fingerprint baseline
  # fails it with nothing wrong in the panel.
  list(id = "loader", script = "LOADING_ND3_v3.R", mins = 1,
       log = "^LOADING_ND3_v3_",
       expect = list(
         "panel unchanged"   = "0 column\\(s\\) dropped, 0 added,\\s*\\n?\\s*0 changed",
         "row count"         = "134,131",
         "cummax gain"       = "81,275 -> 81,404 \\(gained 129",
         "four-way split"    = "moved out: 6,136 person-waves across 1,398 women",
         "firm_size filled"  = "firm_size_post22 non-missing: 23,844",
         "own-child rule"    = "only sign of a child on 759 person-waves",
         "education rules"   = "copied rules reproduce the wave mutate on all 124,132",
         "v21.49 roster"     = "2,704 treated \\+ 8,134 never-treated = 10,838; left-censored 8,823"),
       halt_on = c("row count", "cummax gain", "four-way split",
                   "firm_size filled", "own-child rule", "education rules",
                   "v21.49 roster")),

  list(id = "02", script = "02fte_experience.R", mins = 3,
       log = "^02fte_experience_",
       expect = list(
         "addendum rows"     = "129,325 rows, 26 columns",
         "dropped for weight"= "219 person-waves",
         "gap credit"        = "-0\\.0457")),

  list(id = "03a", script = "03a_design_audit.R", mins = 1,
       log = "^03a_design_audit_",
       expect = list(
         # v21.49, verified on the 5 October pass. Before v21.49 the event
         # sample was 66,719 person-waves and 10,650 persons, and women's
         # pre-birth base 0.2981. The persons match the loader's v21.49
         # roster of 10,838. halt_on: a different sample means 03a read a
         # stale frame or a stale handoff/ev.rds, so the pass stops here
         # rather than let 03c spend sixteen hours on it.
         # v-screen: without the screen, 68,361 and 10,902 (screen_audit.R).
         "event sample"      = .by_screen("67,783 person-waves, 10,838 persons",
                                          "68,361 person-waves, 10,902 persons"),
         "base rates"        = "0\\.2980",
         "T0b written"       = "T0b_missingness_by_group\\.csv",
         # NOT THE COUNT. 28 was August; 03a contributes 33 since the age
         # fixed-effects work. Confirms the stage printed its ledger line;
         # the census at the foot of this file does the counting, once.
         "ledger rows"       = "contributes [0-9]+ ledger rows"),
       halt_on = "event sample"),

  # v-runall-e1, 19 October. reconcile_pre_birth_base.R writes
  # T0n_never_treated_births.csv, the only source of E.1's birth reports (0.7
  # and 0.5 percent; 14 men and 10 women), and reconcile_pre_birth_base.csv,
  # the confirmed source of the 29.8 pre-birth base for Check 9. It was not a
  # stage, so a loader or sample change left E.1 checked against an old file.
  # It reads only the registry's frame, so it runs after 03a. It is in VERIFY
  # too, because the verifier and check9_sources.R read what it writes.
  #
  # The checks are anchored on output lines. Each "never-treated" check is the
  # first printed block of that sex's row: persons, asked, reporting and the
  # two shares, from the unscreened run of 16 October (8,194 never-treated
  # persons, as on the roster). The screened values were never recorded, so
  # the screened pattern checks only that the row printed.
  list(id = "reconcile", script = "reconcile_pre_birth_base.R", mins = 1,
       pre = 'source("00_contract.R"); ',
       log = "^reconcile_pre_birth_base_",
       expect = list(
         "base written"        = "(?m)^Wrote [^\\n]*reconcile_pre_birth_base\\.csv",
         "births written"      = "(?m)^Wrote [^\\n]*T0n_never_treated_births\\.csv",
         "never-treated men"   = .by_screen("(?m)^ +men +[0-9]+ +[0-9]+ ",
                                            "(?m)^ +men +4324 +3899 +26 +0\\.6 +0\\.7 "),
         "never-treated women" = .by_screen("(?m)^ +women +[0-9]+ +[0-9]+ ",
                                            "(?m)^ +women +3870 +3615 +17 +0\\.4 +0\\.5 "))),

  list(id = "03b", script = "03b_fee_descriptives.R", mins = 1,
       log = "^03b_fee_descriptives_",
       expect = list(
         # v21.49. Was 112,618 person-wave-sources and 9,756 persons.
         "stacked cell"      = "114,364 person-wave-sources, 9,925 persons",
         "ledger rows"       = "contributes [0-9]+ ledger rows")),

  list(id = "03c", script = "03c_experience_units.R", mins = 1000, slow = TRUE,
       log = "^03c_experience_units_",
       expect = list(
         "slow learner"      = "learner: ranger",
         "gap credit"        = "0\\.0764",
         "arms registered"   = "20 arms added to the ledger",
         "ledger rows"       = "contributes [0-9]+ ledger rows")),

  list(id = "04", script = "04financing_space.R", mins = 1,
       log = "^04financing_space_",
       expect = list(
         # READ FROM THE FILE, NOT TYPED HERE. This said v2\\.6 while
         # 04financing_space.R declared v2.8, so a current file failed its
         # own version check and the summary reported the stage broken when
         # it had worked. A hard-coded expectation goes stale on the next
         # bump by construction, and the thing it is checking is whether the
         # log matches the file, so the file is where to ask.
         "version"           = .fs_declared_version(),
         # v21.49. Was $478 (lower) to $682.
         # v-rebaseline, 19 October: $690 on the unscreened run (04 log of 16 October).
         "pre-birth base"    = .by_screen("\\$482 \\(lower\\) to \\$691",
                                          "\\$482 \\(lower\\) to \\$690"),
         "dose and dollars"  = "T11_investment_dollars\\.csv",
         # THE COUNT IS NOT CHECKED HERE. A typed total goes stale the next
         # time an arm is added, and it went stale: 30 against a ledger that
         # has been 90, 93 and 97. This confirms 04 got as far as printing
         # the line; the census at the foot of this file does the counting,
         # in one place.
         "ledger rows"       = "Ledger: [0-9]+ rows")),

  list(id = "master", script = "MASTER_training_y.R", mins = 5,
       log = "^MASTER_training_",
       expect = list(
         # v21.49, ANCHORED. The old "A5 errors" pattern, the bare number
         # 0.0342, still passed on the halted 5 October logs, because
         # 0.0342 turns up five times in unrelated event-study tables. A
         # check that a coincidence can satisfy does not check A5. "A5
         # errors" is now the 30-year row of the A5(iii) table: each sex's
         # cross-partial and the gap, with their standard errors. "the
         # tilt" is now its whole sentence; before v21.49 its t was -0.40.
         # v-rebaseline, 19 October: the unscreened values, MASTER log of 19 October 11:12.
         "A5 errors"         = .by_screen(
           "30 +\\+0\\.0388 \\(0\\.0358\\) +\\+0\\.0097 \\(0\\.0374\\) +-0\\.0291 \\(0\\.0518\\)",
           "30 +\\+0\\.0389 \\(0\\.0358\\) +\\+0\\.0100 \\(0\\.0373\\) +-0\\.0289 \\(0\\.0517\\)"),
         "the tilt"          = .by_screen(
           "a difference of -0\\.00157 \\(se 0\\.00334, t -0\\.47\\)",
           "a difference of -0\\.00142 \\(se 0\\.00333, t -0\\.43\\)"),
         "profile fragment"  = "A5_crosspartial_profile\\.csv",
         "run complete"      = "Run complete")),

  # REFEREE_RUN.R needs revision 4 or later. Revisions up to 3 required
  # `panel` from 03a's session, which a stage's own process never has, so
  # on 5 October the stage stopped on its first check. Revision 4 loads the
  # contract, the compat guards and the registry's frame itself.
  #
  # "verdicts written" needs the "wrote" line. The bare file name also
  # appears in the list the script prints at the end, whether or not the
  # file was written, so it passed even when nothing was written.
  list(id = "referee", script = "REFEREE_RUN.R", mins = 30, slow = TRUE,
       log = "^REFEREE_RUN_",
       expect = list(
         "bootstrap size"    = "1000 of 1000 replicates usable",
         "verdicts written"  = "wrote [^\\n]*referee_G_fee_states_verdict\\.csv",
         "index includes one"= "interval INCLUDES one")),

  # THE TWO DIAGNOSTICS paper_tables.R READS FROM output/diagnostics. Until
  # 5 October neither was a stage, so a full pass rebuilt everything else
  # and left robust_30_49.csv (27 September) and referee8_summary.csv
  # (25 September) behind: their macros described an older panel. Both read
  # MASTER's outputs, so they run after it and before the tables.
  #
  # Each check is anchored at the start of an output line, so code that
  # source(echo = TRUE) prints cannot satisfy it.
  list(id = "robust", script = "robust_30_49.R", mins = 5,
       log = "^robust_30_49_",
       expect = list(
         "reproduces"      = "(?m)^  all full-window fits reproduce the published values\\.",
         "csv written"     = "(?m)^  wrote [^\\n]*robust_30_49\\.csv")),

  # referee8 needs its revision of 5 October: the contract loaded when the
  # session lacks it, and the roster default moved to v21.49's 10,838.
  list(id = "referee8", script = "referee8_couple_clustering.R", mins = 10,
       log = "^referee8_couple_clustering_",
       expect = list(
         "roster"          = "(?m)^  matches 03a's roster\\.",
         "summary written" = "(?m)^  wrote referee8_summary\\.csv")),
  list(id = "06", script = "06_revision_inference.R", mins = 25,
       log = "^06_revision_inference_",
       expect = list("headline" = "(?m)^  REPRODUCED: the headline gap",
                     "wage"     = "(?m)^  REPRODUCED: the wage window",
                     "summary"  = "(?m)^  wrote [^\\n]*revision_inference_summary\\.csv")),

  # v-screen, 14 October. THE SCREEN AUDIT, after MASTER, because it first
  # reproduces CS_event_study.R's post-birth averages on the rule in force
  # and then fits the same models on the other sample. Its csvs in
  # output/diagnostics hold the numbers the appendix gives for the screen.
  list(id = "screen", script = "screen_audit.R", mins = 2,
       log = "^screen_audit_",
       expect = list(
         "reproduces" = "(?m)^  every fit on the rule in force reproduces the published value\\.",
         "written"    = "(?m)^  wrote screen_audit_groups\\.csv")),

  # v-unscreened-values, 18 October. THE CONTRACT FIRST. Without it the figures stage resolved
  # FIGDIR to the old figures/ folder on 16 October, so the paper kept its
  # previous figures. `pre` is code the child runs before the stage script.
  list(id = "figures", script = "RUN_FIGURES_PAPER.R", mins = 3,
       pre = 'source("00_contract.R"); ',
       log = "^RUN_FIGURES_PAPER_", expect = list()),

  list(id = "tables", script = "paper_tables.R", mins = 1,
       log = "^paper_tables_",
       expect = list(
         "label manifest"    = "Table label manifest",
         # Passes when no OUTPUT line says "post-hoc row(s) survive";
         # echoed code lines, which start "> " or "+ ", are ignored. The
         # pattern this replaces, ^(?!.*...).*$, could only pass on a
         # one-line log, so it failed whenever this stage ran.
         "one post-hoc row"  = "(?m)\\A(?![\\s\\S]*^(?![>+] ).*post-hoc row\\(s\\) survive)")),

  # The verifier needs the contract for its folders and vm_settings.R for the
  # accepted literals and confirmed fractions; on 16 October it ran without
  # both and scanned 2 folders of 5.
  list(id = "verify", script = "verify_manuscript_numbers.R", mins = 2,
       pre = 'source("00_contract.R"); source("vm_settings.R"); ',
       log = "^verify_manuscript_", expect = list()),

  # RECONCILE_COUNTS.R needs revision 6 or later. Revision 5 wrote no log
  # and, in a stage's own process, found no frame: on 5 October it ended
  # "No section produced verdicts" and still printed "Done.". Revision 6
  # loads the contract and the registry's frame, and writes its own log.
  # "all assertions pass" needs the two counts in its verdict line to be
  # equal, so a failed assertion, or a run with no verdicts, fails it.
  list(id = "counts", script = "RECONCILE_COUNTS.R", mins = 1,
       log = "^RECONCILE_COUNTS_",
       expect = list(
         # v-screen: without the screen, 68,361, 10,902 and 57,271.
         "roster"              = .by_screen(
           "taking the largest: 67,783 person-waves, 10,838 persons, 57,182 employed",
           "taking the largest: 68,361 person-waves, 10,902 persons, 57,271 employed"),
         "all assertions pass" = "(?m)^(\\d+) of \\1 assertions pass\\."))
)

# ---------------------------------------------------------------------
# THE RE-BASELINE LIST, EMPTY SINCE 5 OCTOBER. The first pass after
# LOADING_ND3_v3.R v21.49 moved 03a's event sample and base rate, 03b's
# stacked cell, 04's pre-birth base and MASTER's A5 row and tilt. Their
# new values are typed into STAGES above. The gap credits in 02 and 03c
# did not move.
#
# When a change upstream is expected to move a check, name it here as
# "stage: check". A listed check that fails is reported as MOVED, beside
# its old pattern, and does not fail its stage. The pass still ends in
# stop(), so nothing chained after RUN_ALL runs on values nobody has
# checked. Read each new value in its stage's log, check it, type it into
# STAGES and delete its name here. A stale entry hides a change.
#
# Never list referee's "index includes one". It is a verdict, not a
# level, and if it flips the conclusion has changed.
# ---------------------------------------------------------------------
# v-screen, 14 October. THE FIRST PASS WITHOUT THE SCREEN moves the checks
# below, and screen_audit.R could not predict them. Read each new value in its
# stage's log, type it into STAGES as .by_screen(old, new), and delete its
# name here; the screened value stays as the first argument. "counts: all
# assertions pass" is here because RECONCILE_COUNTS.R asserts the couple
# counts the manuscript prints, and those change with the sample until the
# text does.
# v-rebaseline, 19 October. 04's pre-birth base and MASTER's A5 row and tilt
# now carry their unscreened values in STAGES, and RECONCILE_COUNTS passes 15
# of 15 on the unscreened text, so those four names leave this list. The
# checks below still await a first unscreened log of their stage.
EXPECTED_TO_MOVE <- if (.SCREEN_ON) character(0) else c(
  "02: addendum rows", "02: dropped for weight", "02: gap credit",
  "03a: base rates", "03b: stacked cell", "03c: gap credit")

# A name that matches no check would excuse nothing, and a halt_on name
# that matches no check would never halt. Either is a typo, so stop.
.checks <- unlist(lapply(STAGES, function(s)
  if (length(s$expect)) paste0(s$id, ": ", names(s$expect))))
.halts  <- unlist(lapply(STAGES, function(s)
  if (length(s$halt_on)) paste0(s$id, ": ", s$halt_on)))
.typo   <- setdiff(c(EXPECTED_TO_MOVE, .halts), .checks)
if (length(.typo))
  stop("EXPECTED_TO_MOVE or halt_on names a check that does not exist: ",
       paste(.typo, collapse = ", "), call. = FALSE)
rm(.checks, .halts, .typo)

FAST <- setdiff(vapply(STAGES, `[[`, "", "id"),
                vapply(Filter(function(s) isTRUE(s$slow), STAGES), `[[`, "", "id"))
# v-runall-e1: reconcile writes the E.1 source the verifier reads, so it is
# part of the checking pass.
VERIFY <- c("reconcile", "tables", "verify", "counts")

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# TWO WAYS IN, because `Rscript RUN_ALL.R 03a` typed into the R console is
# a syntax error and the console is where this project actually lives.
#
#   from a terminal   Rscript RUN_ALL.R 03a 03b 04
#   from R            RUN_STAGES <- c("03a","03b","04"); source("RUN_ALL.R")
#   full pass from R  RUN_STAGES <- "all"; source("RUN_ALL.R")
#
# NO ARGUMENTS MEANS EVERYTHING ONLY FROM A TERMINAL. Sourced
# interactively with nothing set, this file used to read commandArgs() as
# empty and start the full pass, which includes a sixteen-hour 03c. A
# driver whose least deliberate invocation is also its most expensive is
# a trap, so the interactive path refuses and prints the options instead.
.args <- commandArgs(trailingOnly = TRUE)
.spec <- if (exists("RUN_STAGES")) RUN_STAGES else .args
if (!length(.spec) && interactive()) {
  cat("RUN_ALL.R sourced with no stages. Nothing has been run.\n\n")
  cat("  RUN_STAGES <- c(\"03a\", \"03b\", \"04\"); source(\"RUN_ALL.R\")\n")
  cat("  RUN_STAGES <- \"fast\"   everything except 03c and the referee bootstrap\n")
  cat("  RUN_STAGES <- \"verify\" the checking pass alone\n")
  cat("  RUN_STAGES <- \"all\"    the full pass, including 03c\n")
  cat("  RUN_STAGES <- ", deparse(vapply(STAGES, `[[`, "", "id")), "\n", sep = "")
  cat("\nThe full pass takes about sixteen hours, so it is not the default\n")
  cat("for a source() with nothing set.\n")
  stop("no stages selected", call. = FALSE)
}
# "all" is the console's way to ask for the full pass on purpose.
want <- if (!length(.spec) || identical(.spec, "all"))
  vapply(STAGES, `[[`, "", "id") else
  if (identical(.spec, "fast")) FAST else
    if (identical(.spec, "verify")) VERIFY else .spec
bad <- setdiff(want, vapply(STAGES, `[[`, "", "id"))
if (exists("RUN_STAGES")) rm(RUN_STAGES)
if (length(bad)) stop("unknown stage(s): ", paste(bad, collapse = ", "))

.newest <- function(pat) {
  f <- list.files(LOGS, pattern = pat, full.names = TRUE)
  if (!length(f)) return(NA_character_)
  f[which.max(file.mtime(f))]
}

# ---------------------------------------------------------------------
# THE LEDGER RESET, DECIDED HERE AND NOWHERE ELSE
#
# training_ledger.csv accumulates: 03_finish.R merges the running module's
# rows over whatever the registry already holds, so a tag that has since
# been renamed survives forever and a module that appends without
# resetting multiplies. On 7 September three runs of 04financing_space.R
# in one session left it holding 150 rows against 90, with T10 at 72 and
# T11 at 18.
#
# 03_finish.R ALREADY HAS THE HOOK and its comment names this file as the
# caller: "RUN.R sets TD03_LEDGER_RESET <- TRUE before the first module.
# Do not make this the default: a reset on every source is how running
# 03b alone silently deletes the other two thirds." That warning is the
# whole reason the flag cannot live in a module. Only the driver knows
# whether this pass will rebuild every contributor or just one of them.
#
# SO IT RESETS ONLY ON A COMPLETE PASS. All four contributors present,
# and the flag goes to the first of them; anything less and the ledger is
# left to merge, because a subset that reset would delete rows nothing in
# this run is going to rewrite. When it declines, it says so, since a
# stale ledger that nobody was told about is how sixty orphan rows
# reached a table.
LEDGER_CONTRIBUTORS <- c("03a", "03b", "03c", "04")
.full_ledger <- all(LEDGER_CONTRIBUTORS %in% want)
.reset_at <- if (.full_ledger)
  LEDGER_CONTRIBUTORS[LEDGER_CONTRIBUTORS %in% want][1] else NA_character_

cat(strrep("=", 70), "\n", sep = "")
cat("RUN_ALL  ", format(Sys.time()), "   root: ", ROOT, "\n", sep = "")
cat("stages: ", paste(want, collapse = " -> "), "\n", sep = "")
# Braced, because at top level R closes an `if` at the newline and a bare
# `else` on the next line is a parse error. Inside a function body it
# would have been fine, which is why this shape survives review.
if (!is.na(.reset_at)) {
  cat("ledger: rebuilt from empty at stage ", .reset_at,
      " (all four contributors present)\n", sep = "")
} else {
  cat("ledger: MERGED, not rebuilt. This pass runs ",
      sum(LEDGER_CONTRIBUTORS %in% want), " of 4 contributors, so rows from\n",
      "        the others carry forward from whenever they last ran.\n", sep = "")
}
cat(strrep("=", 70), "\n", sep = "")

res <- list()
for (st in Filter(function(s) s$id %in% want, STAGES)) {
  if (!file.exists(file.path(ROOT, st$script))) {
    cat(sprintf("\n[%s] SKIPPED, %s not found\n", st$id, st$script))
    res[[st$id]] <- data.frame(stage = st$id, status = "absent",
                               failed = "", moved = "",
                               not_checked = length(st$expect),
                               stringsAsFactors = FALSE)
    next
  }
  cat(sprintf("\n[%s] %s   (about %d min)\n", st$id, st$script, st$mins))
  before <- .newest(st$log); t0 <- Sys.time()
  # The flag has to be set INSIDE the child, because each stage runs in its
  # own process and nothing in this session reaches it.
  .pre <- if (identical(st$id, .reset_at)) "TD03_LEDGER_RESET <- TRUE; " else ""
  if (nzchar(.pre)) cat("      setting TD03_LEDGER_RESET for this stage\n")
  # v-unscreened-values, 18 October: a stage may name code to run first.
  if (!is.null(st$pre)) .pre <- paste0(st$pre, .pre)

  # NOT --vanilla. It implies --no-environ, so the child never reads
  # .Renviron and loses HILDA_ROOT and any library path an interactive
  # session has. The first attempt at this failed in 0.0 minutes with no
  # log, which is what a child dying before it reaches library(dplyr)
  # looks like from out here.
  #
  # AND THE OUTPUT IS KEPT. stdout = FALSE, stderr = FALSE discarded the
  # only description of the failure, so a stage that dies said "ERROR"
  # and nothing else. Each stage's console goes to its own file; the
  # stage's own transcript is still written by the stage, and this is the
  # net that catches a failure earlier than that.
  .cap <- file.path(LOGS, sprintf("RUN_ALL_%s_console.log", st$id))
  code <- system2(RSCRIPT, c("--no-save", "--no-restore", "-e",
                             shQuote(sprintf('setwd("%s"); %ssource("%s", echo = TRUE)',
                                             ROOT, .pre, st$script))),
                  stdout = .cap, stderr = .cap)
  mins <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  lg <- .newest(st$log)
  # A STAGE THAT WROTE NO NEW LOG DID NOT RUN, whatever its exit code says.
  fresh <- !is.na(lg) && (is.na(before) || !identical(lg, before))
  failed <- character(0); moved <- character(0)
  if (fresh && length(st$expect)) {
    txt <- paste(readLines(lg, warn = FALSE), collapse = "\n")
    for (nm in names(st$expect))
      if (!grepl(st$expect[[nm]], txt, perl = TRUE)) failed <- c(failed, nm)
    # A check named in EXPECTED_TO_MOVE that failed has moved, as expected.
    moved  <- failed[paste0(st$id, ": ", failed) %in% EXPECTED_TO_MOVE]
    failed <- setdiff(failed, moved)
  }
  halt <- intersect(failed, st$halt_on)
  status <- if (code != 0) "ERROR" else if (!fresh) "no new log" else
    if (length(failed)) "CHECK FAILED" else "ok"
  cat(sprintf("      %s in %.1f min%s\n", status, mins,
              if (fresh) paste0("  ->  ", basename(lg)) else ""))
  if (length(failed))
    for (f in failed) cat("      *** expected but not found: ", f, "\n", sep = "")
  if (length(moved))
    for (m in moved) cat("      moved, as expected: ", m,
                         "  (was /", st$expect[[m]], "/)\n", sep = "")
  if (status %in% c("ERROR", "no new log") && file.exists(.cap)) {
    .tail <- utils::tail(readLines(.cap, warn = FALSE), 15)
    if (length(.tail)) {
      cat("      last lines of the child's console (", basename(.cap), "):\n",
          sep = "")
      for (.l in .tail) cat("      | ", .l, "\n", sep = "")
      rm(.l)
    }
    rm(.tail)
  }
  # NOT CHECKED IS NOT PASSED. With no fresh log the expectations are never
  # evaluated, and a blank `failed` column reads exactly like a clean stage.
  res[[st$id]] <- data.frame(stage = st$id, status = status,
                             failed = paste(failed, collapse = "; "),
                             moved = paste(moved, collapse = "; "),
                             not_checked = if (fresh) 0L else length(st$expect),
                             stringsAsFactors = FALSE)
  rm(.pre, .cap)
  if (status == "ERROR") {
    cat("      stopping: a later stage would run on this stage's stale output.\n")
    break
  }
  if (length(halt)) {
    cat("      stopping: every later stage would run on the wrong input,\n",
        "      because these failed: ", paste(halt, collapse = ", "), "\n", sep = "")
    break
  }
}

out <- do.call(rbind, res)
cat("\n", strrep("=", 70), "\nSUMMARY\n", strrep("=", 70), "\n", sep = "")
print(out, row.names = FALSE)
utils::write.csv(out, file.path(LOGS, paste0("RUN_ALL_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")),
                 row.names = FALSE)
# What moved, in one place.
.moved <- out[nzchar(out$moved), c("stage", "moved"), drop = FALSE]
if (nrow(.moved)) {
  cat("\nRE-BASELINE. These checks moved, as expected. Read each new\n",
      "value in its stage's log and check it, then type it into STAGES and\n",
      "delete its name from EXPECTED_TO_MOVE:\n", sep = "")
  for (.i in seq_len(nrow(.moved)))
    cat("  ", .moved$stage[.i], ": ", .moved$moved[.i], "\n", sep = "")
  rm(.i)
}
.n_moved <- length(unlist(strsplit(.moved$moved, "; ", fixed = TRUE)))
# v-screen, 14 October. RUN_ALL_MOVED_OK <- TRUE lets a caller go on past
# checks that moved AS EXPECTED, and only those; a failed check still stops
# it. It is for the first FINAL_PASS.R without the screen, where 03c's new
# value cannot be known before the sixteen hours it takes to produce it.
if (.n_moved > 0L && isTRUE(get0("RUN_ALL_MOVED_OK", ifnotfound = FALSE)))
  cat("\nRUN_ALL_MOVED_OK is TRUE, so the ", .n_moved, " moved check(s) above\n",
      "do not stop the caller. Re-baseline them before the next pass.\n",
      sep = "")
# THE LEDGER, COUNTED RATHER THAN ASSUMED, ON EVERY PASS.
#
# THIS WAS GATED ON .full_ledger AND THAT IS BACKWARDS. On 7 September a
# three-of-four pass left 70 rows where 90 were expected: 03c's twenty had
# gone, the check did not run because the pass was partial, and the summary
# reported two stages ok and one version failure with no mention of the
# ledger at all.
#
# A full pass rewrites every module, so it is the LEAST likely to lose rows.
# A partial pass carries the absent modules forward from whenever they last
# ran, and carrying forward is exactly the operation that can silently drop
# them. The check belongs on the partial pass more than on the full one.
#
# So it always runs, and it distinguishes the two cases: a module in this
# pass should match its count because it just wrote; a module not in this
# pass should match because its rows survived. Both are failures and they
# have different causes, so they are reported differently.
.f_led <- Sys.glob(file.path(ROOT, "output", "designs", "training_ledger.csv"))
if (length(.f_led)) {
  .led <- utils::read.csv(.f_led[1], stringsAsFactors = FALSE)
  cat("\nLedger: ", nrow(.led), " rows\n", sep = "")
  print(table(.led$module))
  # MAINTAINED BY HAND, and it drifts every time an arm is added. 28 and 12
  # were the August counts; the age fixed-effects work of 13 September took
  # 03a to 33 and 03b to 15. If the total below does not come to nrow(.led),
  # the mismatch lines say which module is off and by how much, which is the
  # useful output rather than a failure to suppress.
  .exp <- c("03a_design_audit" = 33L, "03b_fee_descriptives" = 15L,
            "03c_experience_units" = 20L, "04financing_space" = 30L)
  # A MODULE WITH NO ROWS INDEXES TO NA, NOT TO ZERO, and NA propagates
  # through the comparison and prints as "NA rows". Coerced first, so a
  # module that vanished entirely reports 0 and reads as the loss it is.
  .got <- as.integer(table(.led$module)[names(.exp)])
  .got[is.na(.got)] <- 0L
  names(.got) <- names(.exp)
  .ran <- c("03a" = "03a_design_audit", "03b" = "03b_fee_descriptives",
            "03c" = "03c_experience_units", "04" = "04financing_space")
  .in_pass <- unname(.ran[intersect(names(.ran), want)])

  .bad <- names(.exp)[as.integer(.got) != as.integer(.exp)]
  if (length(.bad)) {
    for (.m in .bad) {
      .n <- as.integer(.got[.m]); .e <- as.integer(.exp[.m])
      if (.m %in% .in_pass)
        cat("  *** ", .m, ": ", .n, " rows, expected ", .e,
            ". It ran in this pass, so it did not finish or its tags\n",
            "      were renamed.\n", sep = "")
      else
        cat("  *** ", .m, ": ", .n, " rows, expected ", .e,
            ". It did NOT run in this pass, so these rows were carried\n",
            "      forward and the carry lost them. Whatever wrote the ledger\n",
            "      last did not preserve a module it was not rebuilding.\n",
            sep = "")
    }
    cat("      Total ", nrow(.led), " against ", sum(.exp),
        ". Re-run the short module, or\n",
        "      the full four, before reading anything downstream of the\n",
        "      ledger.\n", sep = "")
    rm(.m, .n, .e)
  } else {
    cat("  all four modules at their expected counts.\n")
  }
  # ZERO IS NOT DRIFT. A module at 0 rows means its stage never finished, and
  # on 13 September that produced a 48-row ledger which every downstream
  # script then ran on: MASTER, the figures, paper_tables and the verifier all
  # came back clean because each described its own denominator and none
  # described the ledger. This is the harder test and it is separate from the
  # counts above, because an expectation that has drifted is a nuisance and a
  # stage that contributed nothing is a wrong paper.
  .zero <- names(.exp)[as.integer(.got) == 0L]
  if (length(.zero)) {
    cat("  *** ", length(.zero), " module(s) contributed NOTHING: ",
        paste(.zero, collapse = ", "), "\n",
        "      This ledger is not a whole pass. Nothing downstream of it may\n",
        "      run: not MASTER, not the figures, not paper_tables, not the\n",
        "      verifier. A clean verdict from any of them would mean only\n",
        "      that what IS here reconciles.\n", sep = "")
    .not_whole <- TRUE
  } else .not_whole <- FALSE
  rm(.led, .exp, .got, .ran, .in_pass, .bad, .zero)
} else {
  .not_whole <- TRUE
  cat("\n  *** training_ledger.csv not found; the ledger check did not run.\n")
}
rm(.f_led)

if (any(out$status != "ok"))
  cat("\nNot clean. Read the named check in that stage's log before going on.\n")

# RETURNING IS NOT STOPPING. Until now a stage failure ended this file's own
# loop and handed control back to the caller, and a driver script that reads
#
#   source("RUN_ALL.R"); source("MASTER_training_y.R"); ...
#
# carried straight on. That is how a half-built ledger reached the figures,
# the tables and the verifier on 13 September. Set RUN_ALL_STOP_ON_FAIL to
# FALSE only when running this interactively to inspect a failure.
if (!exists("RUN_ALL_STOP_ON_FAIL")) RUN_ALL_STOP_ON_FAIL <- TRUE
if (isTRUE(RUN_ALL_STOP_ON_FAIL) &&
    (any(out$status != "ok") || sum(out$not_checked) > 0L ||
     (.n_moved > 0L && !isTRUE(get0("RUN_ALL_MOVED_OK", ifnotfound = FALSE))) ||
     isTRUE(get0(".not_whole", ifnotfound = FALSE)))) {
  .why <- c(if (any(out$status != "ok"))
              paste0(sum(out$status != "ok"), " stage(s) not ok: ",
                     paste(out$stage[out$status != "ok"], collapse = ", ")),
            if (sum(out$not_checked) > 0L)
              paste0(sum(out$not_checked), " expectation(s) never evaluated"),
            if (.n_moved > 0L)
              paste0(.n_moved, " moved check(s) await re-baseline"),
            if (isTRUE(get0(".not_whole", ifnotfound = FALSE)))
              "the ledger is not a whole pass")
  stop("RUN_ALL did not complete cleanly: ", paste(.why, collapse = "; "),
       ".\n  Nothing downstream of the ledger may run on this pass.",
       call. = FALSE)
}
