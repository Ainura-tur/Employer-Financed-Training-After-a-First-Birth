# =====================================================================
# 03_logging.R  -  opens the transcript for whichever module is running
# ---------------------------------------------------------------------
# SOURCED UNCONDITIONALLY by 03a, 03b and 03c, BEFORE the 03_common.R
# guard. That is the whole point of the file and the reason it exists as
# a file rather than as lines inside the prelude.
#
# THE FAILURE IT FIXES. Both logging calls used to live in 03_common.R,
# which each module sources behind
#
#     if (!isTRUE(.TD03_COMMON_DONE)) source("03_common.R")
#
# so in a warm session the second module to run skipped the prelude and
# therefore skipped the logging with it. The 25 August 14:17 run shows
# it: 03_common.R had been sourced on its own first, opening a transcript
# under the default name, and 03a then ran to completion inside it. The
# finisher's header said 03a_design_audit and the file was called
# 03training_designs, so a complete and correct run of one module was
# filed under another. Nothing errored, which is what makes it worth a
# guard: a mislabelled transcript is discovered when someone tries to
# reconstruct which run produced which ledger, by which time there are
# several candidates and no way to choose.
#
# ONE SINK, NOT TWO, and v2 of this file got that wrong. It opened the
# contract's archive and then opened a second sink for the companion
# transcript on top of it. The 14:24 run caught it: line 19 reads "Log:
# inheriting the caller's sink (1 output)", because the ownership test
# ran AFTER td_log_open had already taken the sink, so .t_owns_sink came
# out FALSE on a cold run and the companion file was never written.
#
# The right fix is not to reorder the test. It is not to open the second
# sink at all. The contract's own v24.4 note says why: with split = TRUE
# the enclosing sink is the second's "console", so every line lands in
# both files and neither is a record of one run. Two stacked sinks IS the
# failure, and a correctly ordered ownership test would only have made it
# happen reliably instead of by accident.
#
# So: ONE sink, held by the contract, and the companion file is a COPY
# taken by 03_finish.R after the sink closes. Same two files, no stack.
# =====================================================================

# THE NOTICE IS RECORDED, NOT JUST PRINTED. v3 warned here, before
# td_log_open() had opened anything, so the warning went to the console
# and NOT into the transcript. The 25 August 14:36 run is the result: an
# archive called 03training_designs_143622.log containing a complete 03a
# run, with nothing inside the file to say why it carries the wrong name.
# A caveat that is missing from the artefact it is a caveat about has not
# been recorded. The flag is set here and the notice is written after the
# sink opens.
.t_module_defaulted <- !exists("TD03_MODULE")
# v26. THE DEFAULT IS NO LONGER "03training_designs.R". That file was
# retired at the split into 03_common, 03a, 03b, 03c and 03_finish, and
# naming an unattributed transcript after it made a misfiled log
# indistinguishable from a genuine pre-split one in a directory listing.
# The warning below and the mismatch notice further down are both good
# and both stay, but they are messages inside a file that can run to
# hundreds of kilobytes, and the FILENAME is what anyone sees first.
# This name says what happened, sorts beside the real module logs, and
# cannot be mistaken for an archive entry.
# v27. THE NOTICE WAS NOT ENOUGH, AND THE 6 SEPTEMBER 07:44 RUN IS WHY.
# Every warning in this file fired exactly as written. 03_logging.R was
# sourced without a name, the transcript opened as 03_unnamed_caller, the
# defaulted notice printed at the head, 03a arrived two minutes later and
# the TRANSCRIPT NAME MISMATCH notice printed where it happened. A
# complete 03a pass, 28 ledger rows, ran to completion inside a file
# named for nobody. Two correct warnings did not prevent one misfiled
# archive, because by the time either could be read the sink was open and
# the name was fixed.
#
# THE COST IS NOT ONLY THE FILENAME. An inherited sink sets .t_companion
# to NA, correctly, because the caller owns the record. When the caller
# is an unnamed transcript there IS no owner, so 03_finish.R writes no
# companion at all and 03a_design_audit_log.txt in OUTDIR is never
# refreshed. That file is 04's OUTDIR marker, so a stale marker outlives
# the run that should have replaced it.
#
# SO IT STOPS. There is no caller that legitimately arrives here without
# a name: the three modules set it on line one and 03_common.R already
# refuses to run without this file. TD03_ALLOW_UNNAMED exists for a
# deliberate scratch session and keeps the old behaviour, notices and
# all, for anyone who wants a transcript of something that is not a
# module.
#
# NOT FIXED BY ROTATING THE LOG, and that argument in the mismatch branch
# below still holds: closing an open archive and opening a new one splits
# one session across two half-files. Renaming at close would be better
# than either, but the close lives in 03_finish.R and the archive cannot
# be renamed while the sink holds it open. Refusing at the door is the
# one fix that needs nothing this file cannot see.
.t_allow_unnamed <- isTRUE(get0("TD03_ALLOW_UNNAMED", ifnotfound = FALSE))
if (.t_module_defaulted && !.t_allow_unnamed)
  stop("03_logging.R was sourced without TD03_MODULE set.\n",
       "  Source a module rather than this file or 03_common.R directly:\n",
       "    03a_design_audit.R, 03b_fee_descriptives.R, 03c_experience_units.R\n",
       "  Each sets TD03_MODULE on its first line and sources this file on\n",
       "  the second, and each will rebuild the 03_common prelude by itself;\n",
       "  nothing needs warming by hand, and a loader run in this session\n",
       "  clears the flag that says otherwise.\n",
       "  For a deliberate scratch transcript: TD03_ALLOW_UNNAMED <- TRUE.",
       call. = FALSE)

if (.t_module_defaulted) TD03_MODULE <- "03_unnamed_caller.R"

# THE OWNERSHIP TEST COMES FIRST, before anything can open a sink. v23's
# two rules are unchanged and this is what they turn on:
#
#   If a caller already has a sink, LEAVE IT. The caller's log is the
#   record of the run and this module is part of that run. A second file
#   would split the record in half.
#
#   If it opens anything, it closes exactly that and nothing else. The
#   counts captured here are asserted on exit by 03_finish.R, so an
#   imbalance is a stated failure rather than a caller wondering where
#   its output went.
# =====================================================================
# BOOTSTRAP THE HELPERS THIS FILE USES
#
# v28b. td_log_open() is not defined here and was never sourced here
# either: this file has always run second, into a session where the
# bootstrap had already happened, so the dependency was satisfied by
# accident of ordering and nobody noticed it was unstated.
#
# It surfaced the moment a stage got its own process. RUN_ALL.R runs each
# script in a fresh R, so 03a set TD03_MODULE, sourced this file, and
# died on
#   Error in td_log_open(TD03_MODULE) : could not find function
# before writing a line. From outside that looks like a stage failing in
# zero seconds with no log, which is the least diagnosable failure this
# pipeline can produce.
#
# 03_common.R ALREADY DOES THIS, and the pattern is copied from it rather
# than invented: check for what is needed, source the contract if it is
# absent, and stop with the name of the missing function if it is still
# absent afterwards. A file that cannot run alone cannot be run by a
# driver, and "source something else first" is not a contract if it is
# written down nowhere.
if (!exists("td_log_open", mode = "function")) {
  for (.b in c("00_contract.R", "paths.R", "00compat.R")) {
    if (exists("td_log_open", mode = "function")) break
    if (file.exists(.b)) source(.b)
  }
  if (exists(".b")) rm(.b)
}
if (!exists("td_log_open", mode = "function"))
  stop("03_logging.R needs td_log_open() and could not find it.\n",
       "  Tried 00_contract.R, paths.R and 00compat.R in ", getwd(), ".\n",
       "  Source the file that defines it before the module, or run from\n",
       "  the project root where those live.", call. = FALSE)

.t_sink_out_before <- sink.number()
.t_sink_msg_before <- sink.number(type = "message")
.t_owns_sink <- (.t_sink_out_before == 0L)

if (.t_owns_sink) {
  td_log_open(TD03_MODULE)
  if (.t_module_defaulted) {
    cat("\n  TD03_MODULE WAS NOT SET when this transcript opened, so it is\n")
    cat("  named for the old single file rather than for the module that\n")
    cat("  produced the output below. That happens when 03_logging.R or\n")
    cat("  03_common.R is sourced directly. Whatever module runs after this\n")
    cat("  point writes into THIS file; check the finisher's header at the\n")
    cat("  foot for which one it was.\n")
    cat("  Source 03a_design_audit.R, 03b_fee_descriptives.R or\n")
    cat("  03c_experience_units.R instead; each sets the name on line one.\n\n")
  }
  # The contract records the path it chose; read it rather than
  # reconstructing the timestamp, which would race across a second
  # boundary and produce a name that does not exist.
  .t_log_file <- tryCatch(get("file", envir = .TD_LOG),
                          error = function(e) NA_character_)
  # Where 03_finish.R copies it once the sink is closed. The companion is
  # what a reader opens next to the ledger, and 04's OUTDIR validator
  # uses a *_log.txt in the designs directory as its marker, so this is
  # not decoration.
  .t_companion <- file.path(
    OUTDIR, paste0(tools::file_path_sans_ext(basename(TD03_MODULE)),
                   "_log.txt"))
  cat("  companion copy on close: ", basename(.t_companion), "\n", sep = "")
} else {
  .t_log_file  <- NA_character_
  .t_companion <- NA_character_
  cat(sprintf(paste0("Log: inheriting the caller's sink (%d output). This ",
                     "module writes no\n  separate log, so the run stays in ",
                     "one record.\n"), .t_sink_out_before))
  # NAME MISMATCH, ANNOUNCED WHERE IT HAPPENS. Inheriting a sink is
  # correct and is what running under MASTER_training.R looks like. What
  # is worth flagging is inheriting one opened under a DIFFERENT module's
  # name, because then the archive and its companion are both filed under
  # the earlier module while this one's output goes into them. The
  # 25 August 14:39 run is that case: 03_logging.R was sourced directly
  # first, so a full 03a pass landed in 03training_designs_143938.log.
  #
  # Not fixed by rotating the log. Closing the open archive and opening a
  # new one here would split a single session across two half-files, which
  # is harder to read than one file with a note in it. The fix is at the
  # call site and costs nothing: source the module, which sets the name on
  # its first line, rather than sourcing this file by hand.
  .t_open_as <- tryCatch(get("script", envir = .TD_LOG),
                         error = function(e) NA_character_)
  if (!is.na(.t_open_as) && !identical(.t_open_as, TD03_MODULE)) {
    cat("\n  TRANSCRIPT NAME MISMATCH. This module is ", TD03_MODULE,
        ",\n  but the open transcript was created for ", .t_open_as,
        ".\n", sep = "")
    cat("  Everything below is filed under the earlier name, in both the\n")
    cat("  archive and the companion copy in the designs directory.\n")
    # v27. The unnamed opener is the worse case and is worth separating.
    # A mismatch against a REAL module means the record is filed under
    # that module, which is at least a place someone looks. A mismatch
    # against the unnamed transcript means no module owns the record, so
    # 03_finish.R writes no companion and this module's *_log.txt marker
    # in OUTDIR is never refreshed. Reachable only under
    # TD03_ALLOW_UNNAMED, since the stop() above closes the accidental
    # route, but reachable.
    if (identical(.t_open_as, "03_unnamed_caller.R")) {
      cat("  THE OPEN TRANSCRIPT IS THE UNNAMED ONE, so no companion is\n")
      cat("  written for ", TD03_MODULE, " and its *_log.txt marker in\n",
          sep = "")
      cat("  the designs directory keeps whatever date it already had.\n")
      cat("  04's OUTDIR validator reads that marker. Restart in a fresh\n")
      cat("  session rather than trusting the run below.\n")
    }
    cat("  In a fresh session, source(\"", TD03_MODULE, "\") on its own:\n",
        sep = "")
    cat("  it sources this file on line one and the names then match.\n\n")
  }
  rm(.t_open_as)
}

# =====================================================================
# PER-MODULE ACCUMULATORS
# ---------------------------------------------------------------------
# A NOTE ON THE FILENAME, because it is now narrower than the contents.
# This file is per-module setup, of which the transcript is one part. It
# keeps the name only because three module headers already source it and
# a rename would leave a stale 03_logging.R on disk that still works.
# Read it as 03_module_init.R.
#
# THE FAULT THESE THREE LINES FIX. They were initialised in 03_common.R,
# which each module sources behind the .TD03_COMMON_DONE guard, so in a
# warm session the second module inherited the first module's rows. The
# 25 August 14:31 run: 03b reported "contributes 38 ledger rows" when
# T3, T3b and T3c produce 12, and the merged ledger labelled all 26 of
# 03a's rows 03b_fee_descriptives.
#
# WHY THAT IS WORSE THAN A WRONG TOTAL. The merged file was numerically
# correct: 38 unique tags, 26 from 03a and 12 from 03b, nothing lost or
# double-counted, because 03_finish.R dedupes on tag. Only the module
# column was wrong. A reader checking the ledger against the run logs
# would find every row present and every attribution false, and the
# module column exists precisely so that question can be answered.
#
# THE FRESH-PROCESS PATH WAS NEVER BROKEN, which is why this survived a
# working RUN_ORDER. Each Rscript process starts with an empty global
# environment, so 03_common.R always runs and these are always fresh.
# Only the interactive warm session, which is how a module actually gets
# iterated on, hits it.
# v28. TD03_LEDGER_MODULE IS CLEARED HERE FOR THE SAME REASON, and it is
# a risk the module column itself introduced. report() now stamps each
# ledger row with that variable at build time so a script which is not an
# 03 module can own its rows; 04financing_space.R sets it and, not being
# a module, has no finisher in which to unset it. A warm session that
# runs 04 and then a module would stamp the module's rows
# 04financing_space, which is the 25 August failure above running
# backwards. Every module opens a transcript, so this is the one place
# guaranteed to execute between them.
if (exists("TD03_LEDGER_MODULE", envir = globalenv()))
  rm("TD03_LEDGER_MODULE", envir = globalenv())

.blind  <- list()
.ledger <- list()
.decide <- character(0)
cat("  accumulators reset for ", TD03_MODULE, "\n", sep = "")