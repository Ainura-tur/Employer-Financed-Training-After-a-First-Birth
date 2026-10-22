# =====================================================================
# 04financing_space.R
# ---------------------------------------------------------------------
# T10. The financing state space: place-and-time crossed with the fee item.
# T11. Employer investment valued in dollars, with allocation bounds.
#
# SOURCE THIS AFTER THE 03 STAGE. It resolves `ev` through the
# handoff registry, and it uses 03's report(), note(), mde(), fe_block(),
# BM_PROP and OUTDIR where they exist.
#
# THE PACKAGE CHECK COMES FIRST, before the alias block, because the
# alias block is itself dplyr:: calls: a session without dplyr fails on
# `there is no package called 'dplyr'` at line one, which names the
# package but not the file or what it was for. Checking first buys a
# message that says which script wants it and why.
.FS_PKGS <- c("dplyr", "fixest")
.fs_nopkg <- .FS_PKGS[!vapply(.FS_PKGS, requireNamespace, logical(1),
                              quietly = TRUE)]
if (length(.fs_nopkg))
  stop("04financing_space.R needs ", paste(.fs_nopkg, collapse = " and "),
       ", which are not installed in this library. T10 needs dplyr for the ",
       "state construction and fixest for the stacked system; T11 needs ",
       "both. Nothing below can run.", call. = FALSE)
suppressPackageStartupMessages({
  
  # Defined here rather than at line 302, because the OUTDIR guard below
  # calls it and a definition after that guard is a definition after its
  # own first use.
  .note <- if (exists("note") && is.function(note)) note else
    function(x) cat("  [note] ", x, "\n", sep = "")
  
  library(dplyr); library(fixest)
})
rm(.FS_PKGS, .fs_nopkg)
#
# STATUS: POST-HOC THROUGHOUT, and more so than T3c. T3c was designated
# after the loader printed its proportions. This file was written after
# the paper's headline coefficients were in the text, so nothing here can
# carry a pre-committed status and nothing in it is graded preferred.
# What it can do is state a quantity the pre-committed designs do not
# reach, with its MDE printed the same way, and let the ledger record it
# as descriptive.
#
# WHY THIS FILE EXISTS. Section 4.3 sets the fee item aside because it is
# asked only of trainees while the time items are asked of everyone in
# work, so the two cannot sit on opposite sides of one difference. That
# is right about a DIFFERENCE and it does not follow for a JOINT
# DISTRIBUTION. Time and money are two dimensions of one investment
# decision, and the object the theory in Section 3 is about is their
# joint incidence: firm-sponsored is released time with no worker
# contribution, worker-sponsored is own time with one. Those are corners
# of a state space, not endpoints of a contrast, and a state space has a
# denominator that does not move with training incidence.
# =====================================================================

#Namespace protection (avoid MASS/stats masking dplyr verbs)
select <- dplyr::select;       filter    <- dplyr::filter
mutate <- dplyr::mutate;       slice     <- dplyr::slice
recode <- dplyr::recode;       rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange <- dplyr::arrange;     count     <- dplyr::count;  lag <- dplyr::lag

# THE PIPE IS NOT IN THE ALIAS BLOCK ABOVE, and that is what killed the
# first run of this file: every verb resolved through its dplyr:: prefix
# while `%>%` did not resolve at all, because it is an operator the block
# never names. 03training_designs.R gets away without it by calling
# library(dplyr) a few lines further down; this file is sourced into
# sessions where that has not happened. The attach at the head of the
# file fixes the run; this alias keeps the operator alive in a session
# where something later detaches the package.
`%>%` <- dplyr::`%>%`

# =====================================================================
# THE LOG
# ---------------------------------------------------------------------
# Copied from 03training_designs.R rather than reinvented, because the
# v23 comment there records what an invented version cost: a module that
# unwound its caller's sinks to zero, put its own on, and removed those
# on exit, handing control back with NO sinks at all. The master's log
# ended at the line where the module was sourced and nothing errored, so
# it read as the run having stopped rather than as a logging fault.
#
# TWO RULES, both taken verbatim from that file.
#
#   If a caller already has a sink, LEAVE IT. The caller's log is the
#   record of the run and this module is part of that run. A second file
#   would split the record in half.
#
#   If it opens anything, it closes exactly that and nothing else. The
#   depths are captured here and asserted on exit, so an imbalance is a
#   stated failure rather than a caller wondering where its output went.
#
# THE LOG IS TIMESTAMPED, unlike 03's fixed training_log.txt, and that is
# a deliberate difference rather than drift. 03 produces one archived
# record per pipeline run; this module is a diagnostic that gets re-run
# against the same panel while its own output is being read, and a fixed
# name overwrites the run you are still comparing against. The master's
# run_logs/ convention is followed, with the module's own name in it: the
# master's v-comment records what an inherited log name costs when a
# MASTER_training run writes a household-named file.
#
# OPENED HERE, before the version banner and the contract guard, so the
# banner and any contract failure land in the log rather than only on the
# console. The package guard above runs first and deliberately stays
# outside: a stop() there must not leave a sink open.
.fs_sink_out_before <- sink.number()
.fs_sink_msg_before <- sink.number(type = "message")
.fs_owns_sink <- (.fs_sink_out_before == 0L)
if (.fs_owns_sink) {
  FS_LOG_DIR <- if (exists("FS_LOG_DIR")) FS_LOG_DIR else
    file.path(getwd(), "run_logs")
  if (!dir.exists(FS_LOG_DIR)) dir.create(FS_LOG_DIR, recursive = TRUE)
  FS_LOG_FILE <- file.path(
    FS_LOG_DIR,
    format(Sys.time(), "04financing_space_%Y%m%d_%H%M%S.log"))
  .fs_log_con <- file(FS_LOG_FILE, open = "wt")
  sink(.fs_log_con, split = TRUE)          # console AND file
  sink(.fs_log_con, type = "message")      # warnings, fixest notes
  cat("Log: ", FS_LOG_FILE, "\n", sep = "")
  cat("  (standalone run; this module owns the sink and will close it)\n")
  # THE RECOVERY LINE, printed because this file contains stop() and a
  # top-level script cannot register on.exit. If a run aborts between
  # here and the closing block, the console is dead and nothing says so.
  cat("  If this run aborts, restore the console with: fs_log_close()\n")
} else {
  cat(sprintf(paste0("Log: inheriting the caller's sink (%d output). This ",
                     "module writes no\n  separate log, so the run stays ",
                     "in one record.\n"), .fs_sink_out_before))
}

# The closer, exposed as a function so it can be called by hand after an
# aborted run. The closing block at the foot of the file calls this same
# function, so there is one implementation rather than two that can drift.
fs_log_close <- function() {
  if (!isTRUE(get0(".fs_owns_sink", ifnotfound = FALSE))) {
    cat("fs_log_close(): this module does not own a sink; nothing to do.\n")
    return(invisible(FALSE))
  }
  # Ordering from 03: message sink, then output sink, then the connection.
  # Out of order leaves a sink writing to a closed file.
  #
  # THE TWO sink.number() CALLS DO NOT MEAN THE SAME THING, and the
  # difference looks like a bug in the log if it is not stated. The
  # output form returns a DEPTH, so 0 means no sink. The message form
  # returns the CONNECTION NUMBER carrying messages, and its default is
  # 2, stderr. So `sink.number(type = "message") > 0` is not a depth test
  # and is true even when no message sink was ever set; it is harmless
  # here because this branch only runs when this file opened one. What
  # actually verifies balance is the equality check at the foot of the
  # file, against the value captured on entry. A closing run that reports
  # "message 2 -> 2" is balanced, not broken.
  if (sink.number(type = "message") > 0) sink(type = "message")
  if (sink.number() > 0) sink()
  .c <- get0(".fs_log_con", ifnotfound = NULL)
  if (inherits(.c, "connection")) try(close(.c), silent = TRUE)
  assign(".fs_owns_sink", FALSE, envir = globalenv())
  cat("Log closed:", get0("FS_LOG_FILE", ifnotfound = "(unknown)"), "\n")
  invisible(TRUE)
}

# A VERSION BANNER, because a stale copy of this file is indistinguishable
# from a current one until three hundred lines of output have gone past.
# It has already happened once: a run reported "place dimension: NOT
# available" and the v1 state names long after the workplace rewrite, and
# nothing in the log said which build produced it. Print the version and
# the marker that identifies it, first thing.
FS_VERSION <- "v2.8 (T11 dollars; ledger reset; merges into training_ledger)"
cat("\n=== 04financing_space.R ", FS_VERSION, " ===\n", sep = "")
cat("  If the T10 output below says 'place dimension: NOT available' or\n")
cat("  names states 'released_nofee' rather than 'wp_released_nofee',\n")
cat("  the file that ran is NOT this one. Check with:\n")
cat("    any(grepl(\"wp_released\", readLines(\"04financing_space.R\")))\n")

# =====================================================================
# THE CONTRACT, BEFORE ANYTHING ELSE
# ---------------------------------------------------------------------
# 00_contract.R defines the dplyr aliases, mde(), gap(), td_frame() and
# the handoff registry. This file needs the registry above all, because
# the registry is where `ev` lives. The guard names every object it takes
# rather than testing one, on the reasoning 03 and the master both give:
# a session with td_frame present but mde absent skips the source() and
# dies hundreds of lines later.
# =====================================================================
.FS_NEEDS <- c("td_get", "td_has", "td_require", "mde")
.fs_missing <- .FS_NEEDS[!vapply(.FS_NEEDS, exists, logical(1),
                                 mode = "function")]
if (length(.fs_missing)) {
  if (file.exists("00_contract.R")) {
    cat("  contract: sourcing 00_contract.R for",
        paste(.fs_missing, collapse = ", "), "\n")
    source("00_contract.R")
  } else {
    cat("  contract: 00_contract.R not on the working directory, and",
        paste(.fs_missing, collapse = ", "), "are absent.\n")
    cat("  The registry is how this file finds ev. Without it the only\n")
    cat("  remaining route is a global ev or an explicit FS_FRAME.\n")
  }
}
.fs_still <- .FS_NEEDS[!vapply(.FS_NEEDS, exists, logical(1),
                               mode = "function")]
if (length(.fs_still))
  cat("  contract: still missing after sourcing:",
      paste(.fs_still, collapse = ", "),
      "\n  The registry route to ev is unavailable; the preflight will\n",
      "  fall back to handoff/ev.rds, a global ev, or FS_FRAME.\n")
rm(.fs_missing, .fs_still)

# THE CONTRACT'S OWN LOGGER, called the way 03 and the master call it, so
# this module appears in whatever run record the contract keeps. Guarded
# rather than required: td_log_open is not in .FS_NEEDS above, because a
# session that can supply ev through the registry but has no logger
# should still run rather than stop over a bookkeeping call.
#
# Note the ordering with the sink block above. If td_log_open itself
# opens a sink, the block above has ALREADY recorded a depth of zero and
# taken ownership, which would leave two logs. It is called here, after
# ownership is settled, so the sink depth this module reasons about is
# the one that existed when it arrived.
if (exists("td_log_open", mode = "function"))
  try(td_log_open("04financing_space.R"), silent = TRUE)

# ---- host-supplied helpers, with parse-safe fallbacks ---------------
if (!exists("BM_PROP"))     BM_PROP     <- 0.25
if (!exists("TD_NO_WRITE")) TD_NO_WRITE <- FALSE
if (!exists("FS_MIN_CELL")) FS_MIN_CELL <- 100L

# OUTDIR IS RESOLVED AND THEN VALIDATED, whoever supplied it. The
# master's v24.7 note records what a bare `if (exists("OUTDIR")) OUTDIR
# else "."` cost: two call sites fell through to the working directory,
# where training_designs/ does not exist, and the writes went somewhere
# nobody was reading.
#
# v2.3 ADDS THE SECOND HALF OF THAT LESSON. v2.2 resolved OUTDIR when it
# was ABSENT and trusted it when present, and a run then reported
# "OUTDIR from the caller: ...Desktop\WG" and wrote three CSVs there.
# That is the same wrong directory arriving by the other route: a caller
# whose own resolution already fell through hands the bad value on, and a
# module that only checks for absence cannot see it. Presence is not
# correctness, so the value is checked either way.
# v2.4. THE DIRECTORY IS CALLED output/designs, NOT training_designs.
#
# Both the validator and the fallback named training_designs, which is the
# LEGACY tree. 00_contract.R reports it as an artefact directory nothing reads
# any more and asks for its contents to be moved into output/designs, so this
# module was resolving to the folder the contract is trying to retire. Where a
# stale copy of that folder survives it also PASSES the validator, because it
# still carries training_decisions.txt from an older pass.
#
# DIR_DESIGNS is the sealed answer and is preferred over everything. The
# artefact test stays as the fallback, because a caller may legitimately hand
# over a directory the contract does not know about, but the retired name no
# longer counts as evidence on its own: presence of a stale folder is not
# correctness, which is the argument the original comment already makes.
fs_outdir_ok <- function(p) {
  if (!length(p) || is.na(p) || !dir.exists(p)) return(FALSE)
  .canon <- if (exists("DIR_DESIGNS") && !is.na(DIR_DESIGNS))
    normalizePath(DIR_DESIGNS, winslash = "/", mustWork = FALSE) else NA_character_
  if (!is.na(.canon) &&
      identical(normalizePath(p, winslash = "/", mustWork = FALSE), .canon))
    return(TRUE)
  basename(p) == "designs" ||
    any(file.exists(file.path(p, c("training_log.txt",
                                   "training_decisions.txt",
                                   "BLIND_training_estimates.rds"))))
}
.fs_outdir_src <- if (exists("OUTDIR")) "caller" else "resolved here"
if (!exists("OUTDIR")) {
  .fs_try <- c(if (exists("DIR_DESIGNS")) DIR_DESIGNS,
               if (exists("PROJ_ROOT"))  file.path(PROJ_ROOT, "output", "designs"),
               if (exists("project_dir")) file.path(project_dir, "output", "designs"),
               file.path("output", "designs"),
               file.path("..", "output", "designs"),
               # the retired tree, tried last and named so a run that lands
               # there says so rather than looking normal
               if (exists("project_dir")) file.path(project_dir, "training_designs"),
               "training_designs",
               file.path("..", "training_designs"))
  .fs_hit <- .fs_try[dir.exists(.fs_try)]
  OUTDIR <- if (length(.fs_hit)) .fs_hit[1] else "."
  if (length(.fs_hit) && grepl("training_designs$", OUTDIR))
    cat("  *** RESOLVED TO THE RETIRED training_designs TREE. Nothing else\n",
        "      reads it; the contract asks for its contents to be moved into\n",
        "      output/designs. Set FS_OUTDIR or run with the contract loaded.\n",
        sep = "")
  rm(.fs_try, .fs_hit)
}
cat(sprintf("  OUTDIR (%s): %s\n", .fs_outdir_src,
            normalizePath(OUTDIR, mustWork = FALSE)))
if (!fs_outdir_ok(OUTDIR)) {
  .fs_alt <- file.path(OUTDIR, "training_designs")
  cat("  THIS DOES NOT LOOK LIKE THE DESIGNS DIRECTORY. It is neither\n")
  cat("  named training_designs nor carrying 03's artefacts, so anything\n")
  cat("  written below goes somewhere nobody is reading. That is the\n")
  cat("  v24.7 failure arriving from the caller rather than from a\n")
  cat("  fallthrough here.\n")
  if (dir.exists(.fs_alt)) {
    OUTDIR <- .fs_alt
    cat(sprintf("  Redirected to: %s\n", normalizePath(OUTDIR, mustWork = FALSE)))
    .note(paste("04: the caller's OUTDIR was not the designs directory;",
                "redirected to", OUTDIR))
  } else {
    cat("  No training_designs/ underneath it either. Writing there\n")
    cat("  anyway; set FS_OUTDIR or OUTDIR and re-run to place the CSVs.\n")
    .note(paste("04: OUTDIR failed validation and no alternative was",
                "found. CSVs written to", OUTDIR))
  }
  rm(.fs_alt)
}
if (exists("FS_OUTDIR")) {
  OUTDIR <- FS_OUTDIR
  cat(sprintf("  OUTDIR overridden by FS_OUTDIR: %s\n",
              normalizePath(OUTDIR, mustWork = FALSE)))
}
rm(.fs_outdir_src)

.have_ledger <- exists("report") && is.function(report)
if (!.have_ledger)
  cat("\n  NOTE: 03's report() is not in scope. Rows are printed below and\n",
      "        collected into this module's own ledger CSV, so a\n",
      "        standalone run still leaves an artefact. They do NOT reach\n",
      "        03's training_ledger and carry no status there.\n",
      sep = "")

# THE MODULE'S OWN LEDGER. On the 25 August run report() was absent, so
# roughly twenty rows of estimates, standard errors and MDEs existed
# only as console text: printed, statused, and recorded nowhere. A run
# that cannot be re-read is a run that has to be repeated. Every .rep()
# call appends here whether or not 03's report() is available, and the
# frame is written at the foot of the file.
.fs_ledger <- list()

.mde <- if (exists("mde") && is.function(mde)) mde else
  function(se) 2.802 * se   # (qnorm(.975) + qnorm(.8)) * se

.fe <- if (exists("fe_block") && is.function(fe_block)) fe_block else
  function() "person_id + wave^female"

# .note is DEFINED ABOVE, before the OUTDIR guard that calls it. Left
# here it was a use-before-definition that only fired on the failure
# path: under the master OUTDIR is valid, the guard never runs, and the
# call is never reached. Standalone with a bad OUTDIR it dies with
# "could not find function '.note'" at the exact moment it was trying to
# tell you the directory was wrong. Same shape as the .src bug in
# training_funding_divergence.R, and invisible for the same reason.

# v2.5. OWN THE LEDGER ROWS THIS FILE PRODUCES. .rep() dispatches to
# 03_common.R's report(), so every T10 and T11 row lands in the shared
# .ledger accumulator. Without a name on them, whichever 03 module calls
# 03_finish.R next stamps them as its own, and on 6 September that put
# thirty of them into training_ledger.csv as 03a_design_audit.
#
# This is deliberately NOT TD03_MODULE. Setting that would tell
# 03_logging.R and 03_finish.R that this file is the module owning the
# transcript, which it is not: it opens no sink and calls no finisher.
# The two facts are separate and the accumulator only needs the second.
TD03_LEDGER_MODULE <- "04financing_space"

# v2.7. RESET THE SHARED ACCUMULATOR, because this file appends to one it
# does not own. report() writes into .ledger, a session-level list, and
# 03_logging.R empties it whenever a module opens its transcript. That is
# why 03a, 03b and 03c come back at exactly 28, 12 and 20 however often
# they run. This file calls report() through .rep(), sources no logging,
# and so never reset it: three runs on 7 September left 90 rows in
# training_ledger.csv where 30 belong, with T10 at 72 and T11 at 18. The
# module's own 04_financing_space_ledger.csv was right the whole time,
# because that uses .fs_ledger, which IS reset here.
#
# Guarded on existence rather than assigned unconditionally: in a cold
# session the name does not exist yet, and creating it here would hide a
# 03_common that failed to load.
if (exists(".ledger", envir = globalenv())) {
  .n_prev <- length(get(".ledger", envir = globalenv()))
  assign(".ledger", list(), envir = globalenv())
  if (.n_prev)
    cat("  cleared ", .n_prev, " row(s) left in the shared ledger by an\n",
        "  earlier run in this session. Without this they accumulate.\n",
        sep = "")
  rm(.n_prev)
}

.rep <- function(id, label, model, coefname, units, bm, bm_f, status) {
  # THE ROW IS CAPTURED ON BOTH PATHS. An earlier version returned early
  # when report() existed, so a run WITH the ledger recorded nothing
  # here and a run without it recorded nothing anywhere. The coefficient
  # is read from the model either way and appended before dispatching.
  .ct <- tryCatch(summary(model)$coeftable, error = function(e) NULL)
  if (!is.null(.ct) && coefname %in% rownames(.ct)) {
    .fs_ledger[[length(.fs_ledger) + 1L]] <<- data.frame(
      id = id, label = label, coef = coefname, units = units,
      estimate = unname(.ct[coefname, 1]), se = unname(.ct[coefname, 2]),
      mde = .mde(unname(.ct[coefname, 2])),
      benchmark = bm, benchmark_female = bm_f,
      n = tryCatch(fixest::nobs(model), error = function(e) NA_integer_),
      status = status, version = get0("FS_VERSION", ifnotfound = NA_character_),
      stringsAsFactors = FALSE)
  }
  if (.have_ledger)
    return(report(id, label, model, coefname, units, bm, bm_f,
                  status = status))
  if (is.null(.ct) || !coefname %in% rownames(.ct)) {
    cat(sprintf("  [skip] %s: %s not in the fitted coefficients.\n",
                id, coefname))
    return(invisible(NULL))
  }
  .se <- .ct[coefname, 2]
  cat(sprintf("  %-26s %+9.4f  (SE %.4f)  MDE %.4f  [%s]\n",
              id, .ct[coefname, 1], .se, .mde(.se), status))
  invisible(.se)
}

# =====================================================================
# PREFLIGHT. WHICH FRAME, AND DOES IT CARRY WHAT THE DESIGNS NEED
# ---------------------------------------------------------------------
# v1 of this file guarded both blocks with `if (exists("ev") && ...)` and
# followed them with `else if (exists("ev"))`. When `ev` was absent BOTH
# tests were false, so the file ran to completion and printed nothing but
# its own closing line. That is the one outcome a diagnostic module must
# never produce: a run that looks like a result and is an absence.
#
# Two things change here. The frame is RESOLVED rather than assumed, so a
# panel sitting under another name is found and named rather than missed.
# And every skip below is unconditional, so a block that does not run
# says why it did not.
# =====================================================================
fs_resolve_frame <- function() {
  .marks <- c("person_id", "wave", "treated", "post")
  
  # 1. THE HANDOFF REGISTRY. This is the route the pipeline actually
  # uses and v1 of this file did not test it, which is why a healthy
  # pipeline produced a silent run. 03training_designs.R ends with
  # td_put("ev", ev), which persists to handoff/ev.rds, and every
  # consumer since has read td_get("ev") rather than the global
  # environment. RUN_ORDER runs each script in its own process, so the
  # global environment does not survive between them and exists("ev")
  # is FALSE in a session where nothing whatever is wrong.
  if (exists("td_has", mode = "function") &&
      exists("td_get", mode = "function")) {
    if (isTRUE(tryCatch(td_has("ev"), error = function(e) FALSE))) {
      .f <- tryCatch(td_get("ev"), error = function(e) NULL)
      if (is.data.frame(.f))
        return(list(name = "ev", data = .f, how = "handoff registry"))
    }
  }
  
  # 2. explicit override, either a frame or the name of one
  if (exists("FS_FRAME", inherits = TRUE)) {
    .f <- get("FS_FRAME", inherits = TRUE)
    if (is.data.frame(.f))
      return(list(name = "FS_FRAME", data = .f, how = "override"))
    if (is.character(.f) && length(.f) == 1L && exists(.f, inherits = TRUE))
      return(list(name = .f, data = get(.f, inherits = TRUE),
                  how = "override"))
  }
  
  # 3. handoff/ev.rds read directly, for a session with no contract.
  #
  # v2.4. This was file.path("handoff", "ev.rds"), relative to getwd(). The
  # registry lives under the project root, so a run started from the script
  # folder looked for a handoff directory that is one level up and found
  # nothing. Since this branch exists precisely for a session with no
  # contract loaded, PROJ_ROOT may be absent too; every plausible root is
  # tried and the one that answered is named.
  .hs <- unique(as.character(unlist(list(
    if (exists("PROJ_ROOT")) file.path(PROJ_ROOT, "handoff", "ev.rds"),
    if (exists("project_dir")) file.path(project_dir, "handoff", "ev.rds"),
    file.path("handoff", "ev.rds"),
    file.path("..", "handoff", "ev.rds")))))
  .hh <- .hs[file.exists(.hs)]
  if (length(.hh)) {
    .f <- tryCatch(readRDS(.hh[1]), error = function(e) NULL)
    if (is.data.frame(.f))
      return(list(name = "ev", data = .f,
                  how = paste0(.hh[1], " read directly, no contract loaded")))
  }
  
  # 4. the global environment, which is where a same-session source() of
  # 03 leaves it
  if (exists("ev", inherits = TRUE)) {
    .f <- get("ev", inherits = TRUE)
    if (is.data.frame(.f))
      return(list(name = "ev", data = .f, how = "global environment"))
  }
  
  # 5. anything in the global environment shaped like the event frame.
  # A GUESS IS REPORTED AS A GUESS. If this branch fires, the run is
  # using a frame nobody named, and the log says so rather than
  # proceeding as though the name had been given.
  .cands <- Filter(function(nm) {
    .o <- tryCatch(get(nm, envir = globalenv()), error = function(e) NULL)
    is.data.frame(.o) && all(.marks %in% names(.o))
  }, ls(envir = globalenv()))
  if (length(.cands))
    return(list(name = .cands[1], data = get(.cands[1], envir = globalenv()),
                how = "GUESSED by column signature", others = .cands))
  
  # 6. THERE IS NO STEP SIX, and that is a decision rather than an
  # omission. ev could be rebuilt here from panel and the absorbing
  # ever_parent rule, and the master used to do exactly that. It was
  # removed deliberately: a second implementation of the same frame,
  # maintained separately, used precisely when something upstream has
  # already gone wrong, is worse than a failure that stops. This file
  # keeps that rule. If the registry cannot supply ev, the answer is to
  # run 03, not to reconstruct its output here.
  list(name = NA_character_, data = NULL, how = "not found")
}

.fs <- fs_resolve_frame()
cat("\n--- 04 preflight ---\n")
if (is.null(.fs$data)) {
  cat("  NO EVENT FRAME. Nothing below can run, and the reason is\n")
  cat("  upstream of this file rather than in it.\n\n")
  cat("  ev is built by 03_common.R and handed off with\n")
  cat("  td_put(\"ev\", ev), which persists to handoff/ev.rds. exists(\"ev\")\n")
  cat("  being FALSE is NOT the diagnostic: RUN_ORDER runs each script in\n")
  cat("  its own process, so a perfectly healthy pipeline has no global ev.\n")
  cat("  What matters is whether the registry can answer.\n\n")
  cat("  Checks, in the order worth running:\n")
  cat("    source(\"00_contract.R\"); td_has(\"ev\")\n")
  cat("    file.exists(\"handoff/ev.rds\")\n")
  cat("    getwd()\n\n")
  cat("  Causes, in order of how often each is the one:\n")
  cat("    1. The working directory is not the one the pipeline writes to,\n")
  cat("       so handoff/ is elsewhere. This is a path problem and it is\n")
  cat("       the most common of the four.\n")
  cat("    2. 03 has never completed on this panel, so nothing was put.\n")
  cat("       MASTER_RUN_PART2 defaults to FALSE, which is correct when\n")
  cat("       the artefacts are current and fatal when they were never\n")
  cat("       written. Set it TRUE, with MASTER_RUN_T8 TRUE alongside it.\n")
  cat("    3. 00_contract.R is not on the path, so the registry functions\n")
  cat("       do not exist and only a global ev could have been found.\n")
  cat("    4. The frame is in memory under another name. Set FS_FRAME.\n\n")
  cat("  THIS FILE WILL NOT REBUILD ev. The master removed its own\n")
  cat("  rebuild fallback deliberately, and a second recipe that can\n")
  cat("  drift from the first is worse than a run that stops.\n\n")
  cat(sprintf("  Working directory: %s\n", getwd()))
  # v2.4. The diagnostic reports every place the registry was looked for,
  # not just the one relative to the working directory. A message saying
  # "handoff/ exists: FALSE" while the registry sits one level up is how a
  # path problem gets read as a missing frame.
  cat("  Registry looked for at:\n")
  for (.d in unique(as.character(unlist(list(
        if (exists("PROJ_ROOT")) file.path(PROJ_ROOT, "handoff"),
        if (exists("project_dir")) file.path(project_dir, "handoff"),
        "handoff", file.path("..", "handoff"))))))
    cat(sprintf("    %-58s dir %s | ev.rds %s\n", .d, dir.exists(.d),
                file.exists(file.path(.d, "ev.rds"))))
  if (exists(".d")) rm(.d)
  if (dir.exists("handoff"))
    cat(sprintf("  handoff/ contains: %s\n",
                paste(utils::head(list.files("handoff"), 20), collapse = ", ")))
  .df_names <- Filter(function(nm)
    is.data.frame(tryCatch(get(nm, envir = globalenv()),
                           error = function(e) NULL)),
    ls(envir = globalenv()))
  cat(sprintf("  Data frames in the global environment: %s\n",
              if (length(.df_names))
                paste(utils::head(.df_names, 20), collapse = ", ")
              else "none"))
  rm(.df_names)
} else {
  # BIND UNLESS IT IS ALREADY BOUND. The test used to be
  # `if (!identical(.fs$name, "ev"))`, which skipped the assign whenever
  # the resolver returned the name ev - including the registry path,
  # where the frame is a return value and nothing of that name is in the
  # environment at all. The blocks below then failed on the same missing
  # object the resolver had just found. The condition is about WHERE the
  # frame came from, not what it is called.
  if (!identical(.fs$how, "global environment")) assign("ev", .fs$data)
  cat(sprintf("  frame: %s (%s), %s rows x %s columns.\n",
              .fs$name, .fs$how,
              format(nrow(.fs$data), big.mark = ","), ncol(.fs$data)))
  if (identical(.fs$how, "GUESSED by column signature")) {
    cat("  THIS NAME WAS GUESSED, not given. Candidates were: ",
        paste(.fs$others, collapse = ", "), "\n", sep = "")
    cat("  Set FS_FRAME explicitly before trusting anything below.\n")
    .note(paste("04: ran on a guessed frame,", .fs$name,
                "- set FS_FRAME and re-run before quoting."))
  }
  # THE AUDIT TESTS THE COLUMNS THE DESIGNS READ. v1 listed jttopwt_raw
  # and jttopot_raw, which the primary arm no longer touches: the place
  # dimension is recovered from jttpewt_any and jttpeot_any against the
  # pooled columns, by the residual rule in T10. Auditing the raw pair
  # reported the place dimension absent while the pair that matters was
  # present all along, which is exactly the wrong answer to print.
  .fs_want <- c("person_id", "wave", "k", "female", "treated", "post",
                "employed", "training_paid_time", "training_own_time",
                "training_self_funded_z", "training_module_asked",
                "total_training_hours_z", "total_training_days",
                "avg_training_hours", "real_hourly_wage",
                "ln_hourly_wage_real",
                "jttpewt_any", "jttpeot_any",
                "jttopwt_raw", "jttopot_raw")
  .fs_audit <- data.frame(column = .fs_want,
                          present = .fs_want %in% names(.fs$data))
  cat("\n  Column audit (absent columns downgrade a design, they do not\n")
  cat("  stop the file; each block states what it lost):\n")
  print(.fs_audit, row.names = FALSE)
  rm(.fs_want, .fs_audit)
}
cat("--- preflight complete ---\n")

# =====================================================================
# T10. THE FINANCING STATE SPACE
# ---------------------------------------------------------------------
# v2. TWO CORRECTIONS, and both change what the estimates mean.
#
# 1. THE OUTCOME NOW MATCHES THE PAPER'S. v1 built the time margin from
#    training_paid_time and training_own_time, which the loader pools
#    across place: training_paid_time is jttpewt | jttopwt. But
#    03training_designs.R estimates T1 on jttpewt_any, training at the
#    PLACE OF EMPLOYMENT during paid work time, and the loader's own
#    select comments label that pair "place-of-employment-only
#    indicators". The two are different outcomes, so v1's coefficients
#    could not be set beside Table 7 at all. The run that exposed it:
#    incidence on the pooled measure returned -0.1331 on 54,511 rows
#    against the paper's -0.116 on 54,202, a difference of 0.017 on
#    samples 309 rows apart. That is a measure gap, not a sample gap.
#
# 2. THE PLACE DIMENSION IS RECOVERABLE, contrary to v1's note. The full
#    2x2 cannot be inverted, because a person reporting both a workplace
#    and an other-place mention leaves the pooled column at 1 either way.
#    THE RESIDUAL CAN BE: where jttpewt_any is 0 and training_paid_time
#    is 1, the paid-time training happened somewhere other than the
#    workplace, and that is identified rather than inferred. Same on the
#    own-time margin. So the exhaustive, mutually exclusive taxonomy is
#
#       wp_released   jttpewt = 1, jttpeot = 0
#       wp_mixed      jttpewt = 1, jttpeot = 1
#       wp_own        jttpewt = 0, jttpeot = 1
#       elsewhere     neither, but training on one of the pooled margins
#       none          no training on any margin
#
#    crossed with the fee item: nine states, which is the space this
#    design was described as building before the loader was checked.
#
# THE ITEMS ARE MULTI-RESPONSE, so a cross of "mentioned" indicators is
# not a partition and a naive 2x2 double-counts. The taxonomy above is
# exhaustive and exclusive by construction, which is what lets the
# per-state coefficients be summed and checked. Verified across every
# logically consistent combination of the four inputs rather than
# assumed: nine reachable cells, no row unassigned, none double-counted.
#
# ONE CLASSIFICATION JUDGEMENT FALLS OUT OF THAT AND IS NOT NEUTRAL. A
# person with jttpewt = 1, jttpeot = 0 and own-time training ELSEWHERE
# lands in wp_released. The wp_ prefix means the state describes what
# happened AT THE WORKPLACE, and at the workplace this person trained
# only on paid time, so the label is accurate. But it is silent about
# her concurrent own-time training somewhere else, and the pooled arm
# below would call the same person "mixed". The two arms therefore
# disagree on these rows BY DESIGN and not by error; the disagreement is
# the price of an outcome that matches the paper's.
#
# THE FEE DIMENSION USES THE GATED COLUMN. training_self_funded is asked
# of trainees; training_self_funded_z carries the loader's four-state
# rule and is NA only where the module was never put. The gated twin is
# what makes the denominator the ASKED population rather than the
# trainee population.
# =====================================================================
.t10_need <- c("person_id", "wave", "k", "female", "treated", "post",
               "employed", "training_paid_time", "training_own_time",
               "training_self_funded_z")

if (exists("ev") && all(.t10_need %in% names(ev))) {
  cat("\n--- T10. The financing state space ---\n")
  
  .gate <- if ("training_module_asked" %in% names(ev)) {
    cat("  gate: training_module_asked (loader v21.6 or later).\n")
    ev$training_module_asked == 1L
  } else if ("training_paid_time_obs" %in% names(ev)) {
    cat("  gate: derived from training_paid_time_obs; the named gate is\n",
        "        absent, so this is the 00compat fallback.\n", sep = "")
    !is.na(ev$training_paid_time_obs)
  } else {
    cat("  GATE ABSENT. Falling back to employed == 1, which is NOT the\n",
        "  same population: the module is put to the employed, but a\n",
        "  person-wave can be employed with the module unresolved. The\n",
        "  shares below are then conditioned on employment rather than on\n",
        "  being asked, which is the defect this design exists to avoid.\n",
        sep = "")
    .note(paste("T10: ran on the employed fallback gate, not",
                "training_module_asked. Rebuild the panel before quoting."))
    ev$employed == 1
  }
  
  # DECLARE THROUGH THE TRAINING GATE. v1 read training_own_time and
  # training_paid_time, both of which sit in 00compat's
  # TRAINING_GATE_VARS, and declared nothing. The guard exists so a
  # design's treatment of the never-asked zero is visible in the log
  # rather than implicit in its subsetting; bypassing it is the same
  # class of omission the guard was written to catch. This design gates
  # explicitly, so the structural zero never enters and the declaration
  # says so.
  # DECLARED, NOT SUPPRESSED. This was wrapped in try(silent = TRUE) with
  # allow_structural_zero = FALSE, which is a contradiction: the call says
  # the structural zero is NOT allowed and then swallows the guard that
  # enforces it. Worse, .compat_stop() writes its banner with cat() before
  # raising, and silent = TRUE suppresses only the error message, so every
  # run printed "*** COMPAT GUARD STOPPED THE RUN ***" from a guard that
  # stopped nothing. A banner that lies about halting is worse than no
  # banner, because the reader stops believing the next one.
  #
  # employed_frame = TRUE is the true declaration and the guard offers it by
  # name: the frame here is already restricted to training_module_asked, so
  # no never-asked person-wave is in it and the _any columns cannot be
  # carrying a structural zero. allow_structural_zero = TRUE would be false,
  # since that asserts the unconditional estimand, which this is not.
  #
  # The guard now acknowledges rather than raising, so try() is gone and a
  # real objection would surface instead of being hidden.
  if (exists("assert_training_gate", mode = "function"))
    assert_training_gate(
      c("jttpewt_any", "jttpeot_any", "training_own_time",
        "training_paid_time")[c("jttpewt_any", "jttpeot_any",
                                "training_own_time",
                                "training_paid_time") %in% names(ev)],
      context = "T10 financing state space, gated to the asked population",
      employed_frame = TRUE, data = ev)
  # This one keeps its try(), and the difference is worth seeing: it passes
  # allow_pooled_regime = TRUE, a real declaration that the guard accepts, so
  # the wrapper catches an unexpected failure rather than a predictable
  # objection. The call above was the opposite, suppressing the very guard
  # its own argument invoked.
  if (exists("assert_training_gate_position", mode = "function"))
    try(assert_training_gate_position(
      c("jttpewt_any", "jttpeot_any")[c("jttpewt_any", "jttpeot_any")
                                      %in% names(ev)],
      context = "T10, waves 12-24", data = ev,
      allow_pooled_regime = TRUE), silent = TRUE)
  
  .has_pe <- all(c("jttpewt_any", "jttpeot_any") %in% names(ev))
  cat(sprintf("  place dimension: %s\n",
              if (.has_pe)
                "available via the residual rule; workplace states built"
              else "NOT available, pooled time margin only"))
  if (!.has_pe)
    .note(paste("T10: jttpewt_any/jttpeot_any absent, so the primary arm",
                "runs on the POOLED margin and is not comparable with",
                "Table 7. Rebuild on a loader that carries the pair."))
  
  .base <- ev[which(.gate), , drop = FALSE] %>%
    mutate(
      .any_wt = dplyr::coalesce(training_paid_time, 0),
      .any_ot = dplyr::coalesce(training_own_time, 0),
      .pe_wt  = if (.has_pe) dplyr::coalesce(jttpewt_any, 0) else NA_real_,
      .pe_ot  = if (.has_pe) dplyr::coalesce(jttpeot_any, 0) else NA_real_,
      .fee    = training_self_funded_z,
      post_f  = post * female)
  
  # ---- state construction -------------------------------------------
  fs_build_states <- function(d, place) {
    # THE BRANCH IS BOUND BEFORE THE PIPE, DELIBERATELY. Written as
    # `if (place) {A} else {B} %>% mutate(...)` the pipe attaches to the
    # ELSE BRANCH ONLY, so the workplace arm would return a frame with
    # time_state and no fin_state and everything downstream would fail on
    # a missing column. Tested, not assumed.
    d <- if (place) {
      d %>% mutate(time_state = dplyr::case_when(
        .any_wt == 0 & .any_ot == 0            ~ "none",
        .pe_wt == 1 & .pe_ot == 0              ~ "wp_released",
        .pe_wt == 1 & .pe_ot == 1              ~ "wp_mixed",
        .pe_wt == 0 & .pe_ot == 1              ~ "wp_own",
        TRUE                                   ~ "elsewhere"))
    } else {
      d %>% mutate(time_state = dplyr::case_when(
        .any_wt == 0 & .any_ot == 0            ~ "none",
        .any_wt == 1 & .any_ot == 0            ~ "released",
        .any_wt == 1 & .any_ot == 1            ~ "mixed",
        .any_wt == 0 & .any_ot == 1            ~ "own",
        TRUE                                   ~ NA_character_))
    }
    d %>% mutate(fin_state = dplyr::case_when(
      time_state == "none" ~ "none",
      is.na(.fee)          ~ "fee_unknown",
      .fee == 1            ~ paste0(time_state, "_fee"),
      TRUE                 ~ paste0(time_state, "_nofee")))
  }
  
  # ---- one arm: shares, stacked system, joint test, sum checks ------
  fs_fit_states <- function(d, tag, label, paper_cols = NULL,
                            min_state = 50L) {
    cat(sprintf("\n  === %s ===\n", label))
    cat(sprintf("  rows: %s, persons: %s\n",
                format(nrow(d), big.mark = ","),
                format(dplyr::n_distinct(d$person_id), big.mark = ",")))
    
    .tab <- d %>% count(fin_state) %>%
      mutate(share = round(n / sum(n), 4)) %>% arrange(dplyr::desc(n))
    print(as.data.frame(.tab), row.names = FALSE)
    
    # SMALL STATES ARE DROPPED FROM THE SYSTEM, NOT FOLDED INTO A
    # NEIGHBOUR. Folding fee_unknown into _nofee would assert that those
    # respondents met no fee, which is precisely what is unknown about
    # them. Dropping costs a few rows and asserts nothing; the sum check
    # below accounts for the shortfall rather than hiding it.
    .small <- .tab$fin_state[.tab$n < min_state & .tab$fin_state != "none"]
    if (length(.small)) {
      cat(sprintf("  dropped from the system (n < %d): %s\n", min_state,
                  paste(sprintf("%s (%d)", .small,
                                .tab$n[match(.small, .tab$fin_state)]),
                        collapse = ", ")))
      cat("  Dropped, not folded: folding fee_unknown into a _nofee state\n")
      cat("  would assert the fee answer that is missing.\n")
    }
    
    .states <- setdiff(.tab$fin_state, c("none", .small))
    if (!length(.states)) { cat("  no estimable states.\n"); return(invisible(NULL)) }
    
    # ---- BY-HORIZON SHARES, restored -----------------------------
    # The v2 rewrite dropped this and the CSV with it, which removed the
    # one output an event-time profile of a single state can be built
    # from. Keyed by tag so the workplace, employed-only and pooled arms
    # write three files rather than overwriting one.
    # Cells below FS_MIN_CELL are suppressed: a share computed on forty
    # people is a coincidence with a denominator, not a descriptive fact.
    .by_k <- d %>%
      filter(treated == 1, !is.na(k), k >= -4, k <= 6) %>%
      count(k, female, fin_state) %>%
      dplyr::group_by(k, female) %>%
      mutate(cell = sum(n), share = round(n / cell, 4)) %>%
      dplyr::ungroup() %>%
      filter(cell >= FS_MIN_CELL) %>%
      arrange(k, female, fin_state)
    if (nrow(.by_k) && !isTRUE(TD_NO_WRITE)) {
      .f <- file.path(OUTDIR, paste0("T10_state_shares_by_k_", tag, ".csv"))
      utils::write.csv(.by_k, .f, row.names = FALSE)
      cat(sprintf("  by-horizon shares written to %s\n", basename(.f)))
      rm(.f)
    }
    rm(.by_k)
    
    .long <- dplyr::bind_rows(lapply(.states, function(s)
      d %>% dplyr::transmute(person_id, wave, k, female, treated, post,
                             post_f, state = s,
                             y = as.integer(fin_state == s))))
    .m <- try(fixest::feols(
      y ~ i(state, post) + i(state, post_f) |
        person_id^state + wave^female^state,
      data = .long, cluster = ~person_id, notes = FALSE, warn = FALSE),
      silent = TRUE)
    if (inherits(.m, "try-error")) {
      cat("  [skip] stacked model failed to fit. A state below about half\n")
      cat("  a per cent will not identify once person^state is absorbed;\n")
      cat("  the shares above say which.\n")
      return(invisible(NULL))
    }
    
    .ct <- summary(.m)$coeftable
    .rows <- grep("post_f", rownames(.ct), value = TRUE)
    cat("\n  Per-state difference-in-differences (mothers minus fathers):\n")
    for (.r in .rows) {
      .lab <- sub(".*state::", "", sub(":post_f.*", "", .r))
      .b <- mean(d$fin_state[d$k < 0 & d$female == 1 &
                               d$treated == 1] == .lab, na.rm = TRUE)
      .rep(paste0("T10_", tag, "_", .lab), paste(label, "-", .lab),
           .m, .r, "pp", BM_PROP * .b, BM_PROP * .b,
           status = "descriptive (post-hoc)")
    }
    
    # ---- SUM CHECKS, FITTED RATHER THAN LEFT TO THE READER -----------
    # v1 printed the coefficient sum and told the reader to compare it
    # with T1 by hand. That is the check that never gets done. Both
    # comparisons are fitted here on the SAME ROWS, which is the only way
    # the comparison means anything: a taxonomy that partitions correctly
    # reproduces the direct estimate, and a gap is a leak.
    .fmlx <- function(y) stats::as.formula(paste0(y, " ~ post + post_f | ",
                                                  .fe()))
    d$.any_trained <- as.integer(d$fin_state != "none")
    .m_any <- try(fixest::feols(.fmlx(".any_trained"), data = d,
                                cluster = ~person_id, notes = FALSE,
                                warn = FALSE), silent = TRUE)
    if (!inherits(.m_any, "try-error")) {
      .direct <- fixest::coeftable(.m_any)["post_f", 1]
      cat(sprintf("\n  SUM CHECK. States sum to %+.4f; any-training fitted\n",
                  sum(.ct[.rows, 1])))
      cat(sprintf("  directly on the same rows is %+.4f. Gap %+.4f",
                  .direct, sum(.ct[.rows, 1]) - .direct))
      cat(if (length(.small)) ", which the dropped states explain.\n" else ".\n")
    }
    
    # THE COMPARABILITY CHECK. Only meaningful on the workplace arm: the
    # states involving paid time at the place of employment should sum to
    # the paper's own outcome, jttpewt_any, fitted on the same rows.
    if (!is.null(paper_cols) && all(paper_cols %in% names(d))) {
      d$.paper <- as.integer(dplyr::coalesce(d[[paper_cols[1]]], 0) == 1)
      .m_pap <- try(fixest::feols(.fmlx(".paper"), data = d,
                                  cluster = ~person_id, notes = FALSE,
                                  warn = FALSE), silent = TRUE)
      if (!inherits(.m_pap, "try-error")) {
        .pap <- fixest::coeftable(.m_pap)["post_f", 1]
        .wp <- .rows[grepl("wp_released|wp_mixed", .rows)]
        cat(sprintf("\n  COMPARABILITY. Workplace paid-time states sum to %+.4f;\n",
                    sum(.ct[.wp, 1])))
        cat(sprintf("  %s fitted directly on the same rows is %+.4f.\n",
                    paper_cols[1], .pap))
        cat("  THIS is the number that belongs beside Table 7. If it sits\n")
        cat("  near the paper's estimate the decomposition attaches to the\n")
        cat("  published result; if not, the sample is doing the work and\n")
        cat("  the decomposition attaches to nothing yet.\n")
        .rep(paste0("T10_", tag, "_paper_outcome"),
             paste(label, "- jttpewt_any, direct"), .m_pap, "post_f", "pp",
             BM_PROP * mean(d$.paper[d$k < 0 & d$female == 1 &
                                       d$treated == 1], na.rm = TRUE),
             BM_PROP * mean(d$.paper[d$k < 0 & d$female == 1 &
                                       d$treated == 1], na.rm = TRUE),
             status = "descriptive (post-hoc)")
      }
    }
    
    .w <- try(fixest::wald(.m, "post_f", print = FALSE), silent = TRUE)
    if (!inherits(.w, "try-error")) {
      cat(sprintf("\n  Joint test on the distribution: F = %.2f, p = %.3g, df1 = %d\n",
                  .w$stat, .w$p, .w$df1))
      cat("  NOTE ON WHAT THIS BUYS. On the v1 run the joint statistic was\n")
      cat("  the dominant state's t-ratio wearing a p-value, because one\n")
      cat("  state carried the movement. The joint test earns its place\n")
      cat("  only where no single state does; check the rows above before\n")
      cat("  quoting it as independent evidence.\n")
    }
    invisible(list(model = .m, states = .states))
  }
  
  # ---- the arms -----------------------------------------------------
  # PRIMARY is the workplace taxonomy on the asked population, because
  # that is the outcome the paper estimates. The employed-only twin is
  # what makes the coefficients comparable with Table 7's arms, which are
  # employed and employed-or-on-leave rather than asked. The pooled arm
  # is kept as a diagnostic so the cost of the place pooling is visible
  # rather than argued about.
  if (.has_pe) {
    .d_pe <- fs_build_states(.base, place = TRUE)
    fs_fit_states(.d_pe, "pe", "Workplace states, asked population",
                  paper_cols = "jttpewt_any")
    fs_fit_states(filter(.d_pe, employed == 1), "pe_emp",
                  "Workplace states, employed only",
                  paper_cols = "jttpewt_any")
    rm(.d_pe)
  }
  if (isTRUE(getOption("fs_run_pooled", TRUE))) {
    .d_pool <- fs_build_states(.base, place = FALSE)
    fs_fit_states(.d_pool, "pool",
                  "Pooled time margin (v1's outcome), asked population")
    cat("\n  The pooled arm is a DIAGNOSTIC. Its released states include\n")
    cat("  training on paid time away from the workplace, which the\n")
    cat("  paper's outcome excludes, so its coefficients answer a\n")
    cat("  different question and must not be quoted beside Table 7.\n")
    rm(.d_pool)
  }
  
  rm(.base, fs_build_states, fs_fit_states, .has_pe, .gate)
} else {
  cat("\n  [skip] T10.\n")
  if (!exists("ev"))
    cat("  Reason: no event frame in scope. The preflight above says why.\n")
  else
    cat(sprintf("  Reason: missing columns: %s\n",
                paste(setdiff(.t10_need, names(ev)), collapse = ", ")))
}

# =====================================================================
# T11. EMPLOYER INVESTMENT IN DOLLARS, WITH ALLOCATION BOUNDS
# ---------------------------------------------------------------------
# Equation (1) is about c, and nothing in the paper measures c. Released
# hours multiplied by the wage is the firm's forgone production, which is
# the employer's side of the investment condition in the units the
# condition is written in. The paper has hours, wages and the funding
# split on the same person-wave, so the quantity is constructible.
#
# THE ALLOCATION PROBLEM, AND WHY IT IS A BOUND RATHER THAN AN
# ASSUMPTION. Total training hours are days times hours per day, summed
# over all training in the year. They are NOT split by time-type. A
# person who reports both released and own-time training has some
# unobserved split of those hours between the two. Rather than assume a
# split, the block computes the two extremes:
#
#   lower   mixed person-waves contribute ZERO released hours
#   upper   mixed person-waves contribute ALL their hours as released
#
# The truth is inside. This is partial identification on a measurement
# problem rather than an identification one, and it needs no assumption
# the items do not already carry.
#
# WHY THE WAGE IS FIXED BEFORE THE BIRTH. Valuing released hours at the
# contemporaneous wage would make the dollar series move with the wage
# path, which is the OTHER outcome in the paper and the one Section 7.2
# is at pains to show moves later. A dollar measure built on a moving
# wage would fold the price response into the quantity response and
# destroy the ordering result it sits beside. The base wage is therefore
# the person's own pre-birth mean, so the series moves only with hours.
# The contemporaneous-wage version is fitted as a DIAGNOSTIC to show how
# large that contamination would have been.
#
# WHAT THE FIGURE IS AND IS NOT. It values released TIME. Fees the firm
# met are not observed and enter neither bound, so the dollar figure is a
# lower bound on total employer investment on that account, and it is a
# lower bound in the same direction as the paper's existing released-time
# convention rather than a new one.
# =====================================================================
.t11_need <- c("person_id", "wave", "k", "female", "treated", "post",
               "employed", "training_paid_time", "training_own_time")

if (exists("ev") && all(.t11_need %in% names(ev))) {
  cat("\n--- T11. Employer investment in dollars ---\n")
  
  # ---- hours ---------------------------------------------------------
  .hrs_col <- if ("total_training_hours_z" %in% names(ev)) {
    cat("  hours: total_training_hours_z (gated).\n"); "total_training_hours_z"
  } else if (all(c("total_training_days", "avg_training_hours") %in% names(ev))) {
    cat("  hours: rebuilt here as days x hours-per-day. The gated twin is\n",
        "         absent, so the zero mixes measured with imputed and the\n",
        "         employed-only arm is the one to read.\n", sep = "")
    ev$.t11_hours <- dplyr::coalesce(ev$total_training_days, 0) *
      dplyr::coalesce(ev$avg_training_hours, 0)
    ".t11_hours"
  } else {
    cat("  [skip] T11: no hours measure on the frame.\n"); NA_character_
  }
  
  # ---- wage ----------------------------------------------------------
  # THE LOADER'S LEVEL COLUMN IS real_hourly_wage, NOT hourly_wage_real.
  # LOADING_ND3_v3.R writes the prefix form for the level and the suffix
  # form for the log, ln_hourly_wage_real, and v1 of this file tested
  # only the suffix form for both. It found the log, missed the level,
  # and exponentiated something it did not need to.
  #
  # The gate the loader applies is an EXCLUSION, not a winsorisation:
  #   hourly_wage_clean = NA below $10 or above $500 nominal
  # so no value is compressed and exp(log(x)) is exact on every retained
  # row. What the gate does do is TRUNCATE THE SAMPLE, and the $500
  # ceiling has a consequence the tail diagnostic below turns on: if the
  # wage cannot exceed $500, a heavy tail in an hours-times-wage product
  # is coming from hours and not from wages.
  .wg_col <- if ("real_hourly_wage" %in% names(ev)) {
    cat("  wage: real_hourly_wage (the loader's own level column).\n")
    "real_hourly_wage"
  } else if ("hourly_wage_real" %in% names(ev)) {
    cat("  wage: hourly_wage_real.\n"); "hourly_wage_real"
  } else if ("ln_hourly_wage_real" %in% names(ev)) {
    cat("  wage: rebuilt as exp(ln_hourly_wage_real). The level column\n",
        "        real_hourly_wage is absent from this frame, which is\n",
        "        unexpected: the loader derives the log FROM it. The\n",
        "        exponential is exact, since the loader's $10 and $500\n",
        "        gate excludes rather than winsorises, but check why the\n",
        "        level was dropped from the panel.\n", sep = "")
    ev$.t11_wage <- exp(ev$ln_hourly_wage_real); ".t11_wage"
  } else NA_character_
  
  if (is.na(.hrs_col) || is.na(.wg_col))
    cat("  [skip] T11 estimation: ",
        paste(c(if (is.na(.hrs_col)) "no hours measure",
                if (is.na(.wg_col)) "no wage measure"), collapse = " and "),
        " on the frame.\n", sep = "")
  
  if (!is.na(.hrs_col) && !is.na(.wg_col)) {
    
    # THE BASE WAGE. Treated persons use their own mean over k in
    # [-3,-1], which is the window T4's pre-birth restriction already
    # uses. Never-treated persons have no birth, so they use the mean
    # over their first three observed waves. THE TWO RULES ARE NOT THE
    # SAME RULE and the asymmetry is stated rather than hidden: the
    # comparison group's base is anchored on panel entry while the
    # treated group's is anchored on the event. Both are pre-treatment
    # for the persons they apply to, which is what the design needs.
    .base_treat <- ev %>%
      filter(treated == 1, !is.na(k), k >= -3, k <= -1,
             !is.na(.data[[.wg_col]])) %>%
      dplyr::group_by(person_id) %>%
      summarise(w_base = mean(.data[[.wg_col]]), .groups = "drop")
    .base_never <- ev %>%
      filter(treated == 0, !is.na(.data[[.wg_col]])) %>%
      arrange(person_id, wave) %>%
      dplyr::group_by(person_id) %>%
      slice(1:3) %>%
      summarise(w_base = mean(.data[[.wg_col]]), .groups = "drop")
    .base_w <- dplyr::bind_rows(.base_treat, .base_never)
    cat(sprintf("  base wage recovered for %s of %s persons.\n",
                format(nrow(.base_w), big.mark = ","),
                format(dplyr::n_distinct(ev$person_id), big.mark = ",")))
    
    .d11 <- ev %>%
      dplyr::left_join(.base_w, by = "person_id") %>%
      mutate(
        .pt  = dplyr::coalesce(training_paid_time, 0),
        .ot  = dplyr::coalesce(training_own_time, 0),
        .h   = .data[[.hrs_col]],
        rel_hours_lo = ifelse(.pt == 1 & .ot == 0, .h, 0),
        rel_hours_hi = ifelse(.pt == 1,            .h, 0),
        inv_lo = rel_hours_lo * w_base,
        inv_hi = rel_hours_hi * w_base,
        inv_now = rel_hours_hi * .data[[.wg_col]],
        post_f = post * female) %>%
      filter(employed == 1, !is.na(inv_lo), !is.na(inv_hi))
    
    # THE BASE WAGE IS ITSELF A RESTRICTION and it selects in the
    # direction the paper already worries about. A person with no valid
    # pre-birth wage has no w_base and drops out, and Figure 1 says who
    # those people are: the wage sample's coverage gap opens at k = 0 and
    # falls hardest on mothers. The loss is printed rather than absorbed.
    .n_emp <- sum(ev$employed == 1, na.rm = TRUE)
    cat(sprintf("  rows lost to a missing base wage: %s of %s employed (%.1f%%).\n",
                format(.n_emp - nrow(.d11), big.mark = ","),
                format(.n_emp, big.mark = ","),
                100 * (.n_emp - nrow(.d11)) / max(.n_emp, 1)))
    cat(sprintf("  estimation rows: %s; mixed-state share %.3f.\n",
                format(nrow(.d11), big.mark = ","),
                mean(.d11$.pt == 1 & .d11$.ot == 1)))
    cat("  The mixed share is the width of the bound. A small share means\n")
    cat("  the two arms nearly coincide and the interval is tight; a large\n")
    cat("  one means the allocation problem is doing real work and the\n")
    cat("  interval should be quoted rather than either endpoint.\n")
    
    .pre <- .d11 %>% filter(treated == 1, female == 1, !is.na(k), k < 0)
    .b_lo <- mean(.pre$inv_lo, na.rm = TRUE)
    .b_hi <- mean(.pre$inv_hi, na.rm = TRUE)
    cat(sprintf("  Mothers' pre-birth base: $%.0f (lower) to $%.0f (upper)\n",
                .b_lo, .b_hi))
    cat("  per person-year of released-time employer investment.\n")

    # v2.5. THE THREE FIGURES SECTION 6.1 QUOTES, WRITTEN RATHER THAN READ
    # OFF A CONSOLE.
    #
    # T11_investment_by_k.csv carries bounds by horizon and sex and nothing
    # else, so two of the three dollar figures in that section could not be
    # recovered from any artefact: an audit that went looking for the median
    # dose and the birth-wave withdrawal found neither at cell level nor
    # under any aggregation of the by-k table, because a cell table cannot
    # hold a person-level median and does not carry a difference.
    #
    # THE PRE-BIRTH PAIR IS ALSO WRITTEN, and with the weighting named.
    # Reconstructing it from the by-k file came within about five dollars
    # under several defensible aggregations and matched none of them, which
    # is what happens when a mean over pooled person-waves is compared with
    # a mean of horizon means. This is the pooled one, over the rows below.
    #
    # THE MEDIAN IS OVER TRAINED PERSON-WAVES, not over all of them. A
    # median including the untrained is zero by construction, since most
    # person-waves carry no training at all, so the quantity Section 6.1
    # calls a dose is conditional on a dose existing. Both are written so
    # the text cannot quote one while meaning the other.
    .dose <- .pre %>% filter(rel_hours_hi > 0)
    .med_lo <- stats::median(.dose$inv_lo, na.rm = TRUE)
    .med_hi <- stats::median(.dose$inv_hi, na.rm = TRUE)

    # The birth-wave withdrawal: mothers at k = 0 against their own
    # pre-birth base, on the same rows and the same construction. It is a
    # descriptive difference and not the post_f coefficient below, which is
    # differenced against fathers as well; the two answer different
    # questions and should not be quoted interchangeably.
    .k0 <- .d11 %>% filter(treated == 1, female == 1, !is.na(k), k == 0)
    .w_lo <- .b_lo - mean(.k0$inv_lo, na.rm = TRUE)
    .w_hi <- .b_hi - mean(.k0$inv_hi, na.rm = TRUE)

    cat(sprintf(paste0("  Median dose among trained mother-waves before the",
                       " birth: $%.0f to $%.0f\n  over %s of %s pre-birth",
                       " rows (%.1f%% carry any released time).\n"),
                .med_lo, .med_hi, format(nrow(.dose), big.mark = ","),
                format(nrow(.pre), big.mark = ","),
                100 * nrow(.dose) / max(nrow(.pre), 1)))
    cat(sprintf(paste0("  Birth-wave withdrawal, mothers at k = 0 against",
                       " their own base:\n  $%.0f (lower) to $%.0f (upper),",
                       " on %s rows at k = 0.\n"),
                .w_lo, .w_hi, format(nrow(.k0), big.mark = ",")))

    # v2.6. THESE ARE COMPANIONS, NOT THE PAPER'S FIGURES, and the first
    # version of this block wrote them under the paper's names.
    #
    # The audit that prompted it found that the median dose and the
    # birth-wave withdrawal could not be recovered from
    # T11_investment_by_k.csv, which was true, and concluded they were
    # never computed, which was false. Both are computed several hundred
    # lines below, on different populations and different constructions:
    # the dose the paper quotes is dollars per TRAINEE across the sample,
    # not a median of the bounds among pre-birth mother-waves, and the
    # withdrawal it quotes is incidence times that dose, not a difference
    # of two means. Writing this pair as "median_dose_if_trained" and
    # "birth_wave_withdrawal" put three numbers in a file named for the
    # paper's dollars that were not the paper's dollars.
    #
    # So they keep names that say what they are, and the file itself is
    # assembled at the point where the reported figures exist. A companion
    # is worth having: the two constructions bracket each other, and a
    # reader who meets $918 and $574-$1047 in the same appendix should be
    # able to see which is which without re-deriving either.
    T11_PRE_LO <<- .b_lo; T11_PRE_HI <<- .b_hi
    T11_COMPANION <<- data.frame(
      quantity = c("median_bound_among_prebirth_trained_motherwaves",
                   "prebirth_base_less_k0_mean"),
      basis = c("median of the inv_lo/inv_hi bounds over mother-waves at k < 0 with released time",
                "descriptive difference, not the incidence-times-dose figure the paper quotes"),
      lower = c(.med_lo, .w_lo), upper = c(.med_hi, .w_hi),
      n_rows = c(nrow(.dose), nrow(.k0)), stringsAsFactors = FALSE)
    cat("  Both are COMPANIONS. The paper's dose and withdrawal are the\n")
    cat("  per-trainee median and incidence x dose, computed below and\n")
    cat("  written to T11_investment_dollars.csv there.\n")
    rm(.dose, .med_lo, .med_hi, .k0, .w_lo, .w_hi)
    
    .fml <- function(y) stats::as.formula(
      paste0(y, " ~ post + post_f | ", .fe()))
    
    for (.arm in c("inv_lo", "inv_hi")) {
      .m <- try(fixest::feols(.fml(.arm), data = .d11,
                              cluster = ~person_id, notes = FALSE,
                              warn = FALSE), silent = TRUE)
      if (!inherits(.m, "try-error")) {
        .bm <- if (.arm == "inv_lo") .b_lo else .b_hi
        .rep(paste0("T11_", .arm),
             paste("employer investment, $/yr,",
                   if (.arm == "inv_lo") "lower allocation" else "upper allocation"),
             .m, "post_f", "$", BM_PROP * .bm, BM_PROP * .bm,
             status = "descriptive (post-hoc)")
      } else cat(sprintf("  [skip] T11_%s: fit failed.\n", .arm))
    }
    
    # ---- the wage-contamination diagnostic ---------------------------
    # READ THE DISTANCE, NOT THE COEFFICIENT. inv_now values the same
    # hours at the contemporaneous wage, so the gap between it and inv_hi
    # is the part of a naive dollar estimate that is the wage path rather
    # than the training path. If that gap is large, a dollar measure
    # built on contemporaneous wages would have reported the ordering
    # result's two series as one.
    .mn <- try(fixest::feols(.fml("inv_now"), data = .d11,
                             cluster = ~person_id, notes = FALSE,
                             warn = FALSE), silent = TRUE)
    if (!inherits(.mn, "try-error"))
      .rep("T11_inv_now", "same hours at the contemporaneous wage",
           .mn, "post_f", "$", BM_PROP * .b_hi, BM_PROP * .b_hi,
           status = "diagnostic (contamination only)")
    
    # ---- the heavy tail ----------------------------------------------
    # Figure 3 shows training hours spanning three orders of magnitude,
    # and a dollar measure inherits every bit of that. A mean in levels
    # is therefore determined by which regime a person occupies, which is
    # the reason the paper reports course counts as its dosage measure.
    # Two responses, both reported: a 99th-percentile winsorisation in
    # levels, and the inverse hyperbolic sine the earnings measure
    # already uses. If the three disagree in sign, the levels arm is
    # being driven by its tail and should not be quoted.
    .cap <- stats::quantile(.d11$inv_hi, 0.99, na.rm = TRUE)
    .d11 <- .d11 %>% mutate(
      inv_hi_w  = pmin(inv_hi, .cap),
      inv_hi_ihs = asinh(inv_hi))
    for (.arm in c("inv_hi_w", "inv_hi_ihs")) {
      .m <- try(fixest::feols(.fml(.arm), data = .d11,
                              cluster = ~person_id, notes = FALSE,
                              warn = FALSE), silent = TRUE)
      if (!inherits(.m, "try-error"))
        .rep(paste0("T11_", .arm),
             paste("upper allocation,",
                   if (.arm == "inv_hi_w") "winsorised at p99" else "asinh"),
             .m, "post_f",
             if (.arm == "inv_hi_w") "$" else "asinh",
             BM_PROP * if (.arm == "inv_hi_w") .b_hi else asinh(.b_hi),
             BM_PROP * if (.arm == "inv_hi_w") .b_hi else asinh(.b_hi),
             status = "secondary (post-hoc)")
    }
    
    # ---- WHERE THE TAIL COMES FROM -----------------------------------
    # v2 OF THIS BLOCK. v1 compared the tail against every other row and
    # reported median hours of 320 against 0, which is not a comparison:
    # sixty-nine per cent of rows are non-trainees, so the body it
    # compared against was made of zeros. Its automated rule then tested
    # whether the tail exceeded three times a denominator of zero, which
    # is true for any data whatever. A diagnostic that cannot fail is not
    # a diagnostic. The contrast here is tail against TRAINEES.
    #
    # The loader settles one side of the question in advance:
    # hourly_wage_clean is NA above $500 nominal, so the wage cannot
    # carry a tail of its own and anything heavy in an hours-times-wage
    # product has to be hours. Figure 3 names them, the 60-to-250-day
    # programmes at the far end of a three-order-of-magnitude spread.
    .top <- .d11$inv_hi > .cap
    .pos <- .d11$inv_hi > 0
    if (sum(.top) > 20 && sum(.pos & !.top) > 20) {
      .body <- .pos & !.top          # trainees below the cap
      .rel <- function(x) stats::median(x[.top], na.rm = TRUE) /
        max(stats::median(x[.body], na.rm = TRUE), .Machine$double.eps)
      cat("\n  The p99 tail, against TRAINEES rather than all rows:\n")
      cat(sprintf("    tail n %s (%.2f%% of rows, %.1f%% of trainees)\n",
                  format(sum(.top), big.mark = ","),
                  100 * mean(.top), 100 * sum(.top) / sum(.pos)))
      cat(sprintf("    hours  median %.0f in the tail vs %.0f among other trainees (x%.1f)\n",
                  stats::median(.d11$.h[.top], na.rm = TRUE),
                  stats::median(.d11$.h[.body], na.rm = TRUE), .rel(.d11$.h)))
      cat(sprintf("    wage   median $%.0f in the tail vs $%.0f among other trainees (x%.2f)\n",
                  stats::median(.d11$w_base[.top], na.rm = TRUE),
                  stats::median(.d11$w_base[.body], na.rm = TRUE),
                  .rel(.d11$w_base)))
      # COMPOSITION, WITH THE LABELS THE COLUMNS ACTUALLY CARRY. v1 read
      # female == 1 and called it mothers, which inverted the reading:
      # thirty-one per cent WOMEN means a tail that is seventy per cent
      # men, and that is the opposite of what the label implied.
      cat(sprintf("    composition: %.0f%% women (sample %.0f%%), %.0f%% post-birth (sample %.0f%%)\n",
                  100 * mean(.d11$female[.top] == 1, na.rm = TRUE),
                  100 * mean(.d11$female == 1, na.rm = TRUE),
                  100 * mean(.d11$post[.top] == 1, na.rm = TRUE),
                  100 * mean(.d11$post == 1, na.rm = TRUE)))
      # THE CELL THE ESTIMATE TURNS ON. post_f is identified off
      # post-birth women, so if a handful of them sit in the tail with
      # 300-plus hours they can move the coefficient on their own. This
      # is the cross-tab, not a marginal.
      .pf <- .d11$post == 1 & .d11$female == 1
      cat(sprintf("    post-birth women in the tail: %s of %s in the sample (%.2f%% of that cell)\n",
                  format(sum(.top & .pf), big.mark = ","),
                  format(sum(.pf), big.mark = ","),
                  100 * sum(.top & .pf) / max(sum(.pf), 1)))
      if (sum(.top & .pf) > 0)
        cat(sprintf("    their median investment: $%.0f against $%.0f for the cell\n",
                    stats::median(.d11$inv_hi[.top & .pf], na.rm = TRUE),
                    stats::median(.d11$inv_hi[.pf & .pos], na.rm = TRUE)))
      # A RULE THAT CAN FAIL. Hours-driven requires the hours ratio to
      # dominate the wage ratio against the SAME denominator; if both are
      # elevated by similar factors the reading is not established and
      # the winsorised arm should not be quoted on the strength of it.
      .h_rel <- .rel(.d11$.h); .w_rel <- .rel(.d11$w_base)
      if (is.finite(.h_rel) && is.finite(.w_rel) && .h_rel > 3 * .w_rel) {
        .note(sprintf(paste("T11: p99 tail is hours-driven (hours x%.1f,",
                            "wage x%.2f against other trainees), so",
                            "winsorising trims long programmes."),
                      .h_rel, .w_rel))
      } else {
        cat("  THE HOURS READING IS NOT ESTABLISHED on this run: the wage\n")
        cat("  ratio is not small enough relative to the hours ratio to\n")
        cat("  attribute the tail to hours alone. Do not quote the\n")
        cat("  winsorised arm as a long-programme correction.\n")
        .note(sprintf(paste("T11: p99 tail NOT clearly hours-driven",
                            "(hours x%.1f, wage x%.2f). Winsorised arm",
                            "unexplained."), .h_rel, .w_rel))
      }
      rm(.body, .rel, .pf, .h_rel, .w_rel)
    }
    
    # ---- THE DOSE ARM: THE ONLY REPORTABLE DOLLAR FIGURE --------------
    # WHY THE LEVELS ARMS ABOVE ARE NOT IT. Section 4.3 already settled
    # this for hours and the same argument holds once hours are priced: a
    # mean over a distribution spanning three orders of magnitude is
    # determined by which part of it a person occupies. The paper reports
    # course counts for that reason and drops the conditional-hours
    # regression outright rather than report its null. T11's inv_lo and
    # inv_hi are that distribution multiplied by a wage, so they inherit
    # the problem rather than solving it, and their MDEs say so: both are
    # roughly twice their own point estimates.
    #
    # The winsorised arm is the paper's own logic applied, and it cannot
    # be adopted here. It was fitted after the untrimmed arms were seen,
    # it doubles the effect and halves the standard error, and choosing
    # it on that basis is the selection the blinding exists to prevent.
    #
    # WHAT SURVIVES is a figure built from the identified margin and a
    # dose that the tail cannot move:
    #
    #     withdrawal per mother-year = D(released-time incidence)
    #                                  x median dollars per trainee
    #
    # The incidence effect is the paper's own extensive margin, which is
    # identified and precise. The dose is a median rather than a mean, so
    # the 320-hour programmes enter as one observation each rather than
    # as a weight. The two multiply to a dollar figure whose sampling
    # uncertainty is the incidence coefficient's, the dose being treated
    # as a fixed descriptive constant. THAT SIMPLIFICATION IS STATED
    # RATHER THAN HIDDEN: a correct interval would widen it.
    .d11$inv_any <- as.integer(.d11$inv_hi > 0)
    .m_ext <- try(fixest::feols(.fml("inv_any"), data = .d11,
                                cluster = ~person_id, notes = FALSE,
                                warn = FALSE), silent = TRUE)
    if (!inherits(.m_ext, "try-error")) {
      .se_ext <- .rep("T11_inv_any", "released-time incidence, same rows",
                      .m_ext, "post_f", "pp",
                      BM_PROP * mean(.d11$inv_any[.d11$treated == 1 &
                                                    .d11$female == 1 &
                                                    .d11$k < 0], na.rm = TRUE),
                      BM_PROP * mean(.d11$inv_any[.d11$treated == 1 &
                                                    .d11$female == 1 &
                                                    .d11$k < 0], na.rm = TRUE),
                      status = "descriptive (post-hoc)")
      .ext <- tryCatch(fixest::coeftable(.m_ext)["post_f", 1],
                       error = function(e) NA_real_)
      .dose_med <- stats::median(.d11$inv_hi[.pos], na.rm = TRUE)
      .dose_mean <- mean(.d11$inv_hi[.pos], na.rm = TRUE)
      cat(sprintf("\n  Dollars per trainee: median $%.0f, mean $%.0f (ratio %.1f).\n",
                  .dose_med, .dose_mean, .dose_mean / max(.dose_med, 1)))
      cat("  THE RATIO IS THE WARNING. A mean several times its own median\n")
      cat("  is the Figure 3 spread arriving in dollars, and it is why the\n")
      cat("  median is the dose and the mean is shown beside it rather\n")
      cat("  than used.\n")
      if (is.finite(.ext) && !is.null(.se_ext) && is.finite(.se_ext)) {
        cat(sprintf("\n  IMPLIED WITHDRAWAL, at the median dose: $%.0f per mother-year\n",
                    .ext * .dose_med))
        cat(sprintf("    (incidence %+.4f x $%.0f; SE from incidence alone, +/-$%.0f)\n",
                    .ext, .dose_med, 1.96 * .se_ext * .dose_med))
        cat(sprintf("  At the mean dose the same calculation gives $%.0f, and the\n",
                    .ext * .dose_mean))
        cat("  distance between the two is the tail's influence rather than\n")
        cat("  a second estimate. Quote the median figure.\n")
        .note(sprintf(paste("T11: reportable dollar figure is $%.0f per",
                            "mother-year, incidence x median dose. The",
                            "levels arms are not reportable; their MDEs",
                            "exceed their point estimates."),
                      .ext * .dose_med))

        # v2.6. THE THREE FIGURES SECTION 6.1 QUOTES, IN A FILE. All three
        # were computed here all along and none reached an artefact, so an
        # audit looking for them in T11_investment_by_k.csv found a
        # horizon profile that cannot hold a person-level median or a
        # product of two scalars, and reasonably concluded they did not
        # exist. A console line is not a record.
        .t11 <- data.frame(
          quantity = c("prebirth_base_lower", "prebirth_base_upper",
                       "median_dose_per_trainee", "mean_dose_per_trainee",
                       "implied_withdrawal_per_mother_year",
                       "implied_withdrawal_ci_halfwidth"),
          value = c(get0("T11_PRE_LO", ifnotfound = NA_real_),
                    get0("T11_PRE_HI", ifnotfound = NA_real_),
                    .dose_med, .dose_mean,
                    .ext * .dose_med, 1.96 * .se_ext * .dose_med),
          basis = c("pooled mean of inv_lo over mother-waves at k < 0",
                    "pooled mean of inv_hi over the same rows",
                    "median dollars per trainee, all trainees",
                    "mean of the same; shown beside the median, not used",
                    "incidence x median dose",
                    "1.96 x SE from incidence alone; the dose carries none"),
          stringsAsFactors = FALSE)
        .f11 <- file.path(OUTDIR, "T11_investment_dollars.csv")
        utils::write.csv(.t11, .f11, row.names = FALSE)
        cat("  wrote ", .f11, "\n", sep = "")
        if (exists("T11_COMPANION")) {
          utils::write.csv(T11_COMPANION,
                           file.path(OUTDIR, "T11_investment_companions.csv"),
                           row.names = FALSE)
          cat("  wrote T11_investment_companions.csv (the two bracketing\n")
          cat("  constructions; NOT the figures the paper quotes)\n")
        }
        rm(.t11, .f11)
      }
      rm(.ext, .dose_med, .dose_mean)
      if (exists(".se_ext")) rm(.se_ext)
    }
    rm(.pos)
    
    cat("\n  WHAT T11 LICENSES, revised on this run. The dose arm above,\n")
    cat("  and not the levels arms. inv_lo and inv_hi both carry MDEs\n")
    cat("  roughly twice their own point estimates, so they are bounds\n")
    cat("  rather than estimates, and the allocation interval between\n")
    cat("  them is a dollar wide at a 6 per cent mixed share, so it was\n")
    cat("  never the binding constraint. Precision was.\n")
    cat("\n  WHAT IT DOES NOT LICENSE. The winsorised arm as a headline.\n")
    cat("  It was fitted after the untrimmed arms were seen and chosen on\n")
    cat("  the strength of what it returned. If a trimming rule is wanted\n")
    cat("  it has to be fixed in advance of the next run, and this run\n")
    cat("  cannot supply that pre-commitment for itself.\n")
    cat("\n  AND THE FIGURE REMAINS A LOWER BOUND. It values released time\n")
    cat("  only; fees the firm met are unobserved and enter nothing here.\n")
    cat("  It also inherits the reference-period spread in Section 4.6:\n")
    cat("  hours are reported over a twelve-month window that is not a\n")
    cat("  wave.\n")
    
    if (!isTRUE(TD_NO_WRITE))
      utils::write.csv(
        .d11 %>% filter(treated == 1, !is.na(k), k >= -4, k <= 6) %>%
          dplyr::group_by(k, female) %>%
          summarise(n = dplyr::n(),
                    inv_lo = round(mean(inv_lo, na.rm = TRUE), 1),
                    inv_hi = round(mean(inv_hi, na.rm = TRUE), 1),
                    .groups = "drop") %>%
          filter(n >= FS_MIN_CELL),
        file.path(OUTDIR, "T11_investment_by_k.csv"), row.names = FALSE)

    # --- THE FIVE COUNTS THIS BLOCK PRINTS AND HAS NEVER WRITTEN --------
    #
    # Three cat() calls above report the base-wage recovery, the rows it
    # costs and the mixed-state share, and the manuscript quotes all five
    # numbers in one paragraph: 9,315 of 10,650 persons, 1,798 of 56,309
    # employed person-waves lost, 54,511 left, 6.1 per cent mixed. Check 9
    # reported four of them as literals with no artefact behind them, and
    # it was right: a console line is not an artefact. The fifth escaped
    # notice only because 6.1 also appears in T7_indicator_coverage.csv on
    # an unrelated indicator, which is the coincidence that check warns
    # about.
    #
    # RECOMPUTED HERE, NOT CAPTURED FROM THE MESSAGES. The values are
    # derived from the same objects the cat() calls use rather than parsed
    # back out of formatted text, so a change to either the numbers or the
    # wording cannot make the two disagree silently.
    #
    # THE RESTRICTION IS SELECTIVE and the comment above says so: a person
    # with no valid pre-birth wage has no w_base and drops out, and the
    # coverage gap that produces falls hardest on mothers. Writing the
    # loss down is what lets the manuscript state its size rather than
    # describe it.
    if (!isTRUE(TD_NO_WRITE))
      utils::write.csv(
        data.frame(
          quantity = c("persons_with_base_wage", "roster_persons",
                       "employed_person_waves", "lost_to_missing_base_wage",
                       "estimation_rows", "mixed_state_share"),
          value = c(nrow(.base_w),
                    dplyr::n_distinct(ev$person_id),
                    .n_emp,
                    .n_emp - nrow(.d11),
                    nrow(.d11),
                    round(mean(.d11$.pt == 1 & .d11$.ot == 1), 4)),
          basis = c(
            "persons with a mean of up to three pre-birth wage observations",
            "distinct persons on the event-study roster",
            "employed person-waves on the roster",
            "employed rows dropped for want of a base wage",
            "rows the T11 bounds are estimated on",
            "share of estimation rows training on both released and own time"),
          stringsAsFactors = FALSE),
        file.path(OUTDIR, "T11_investment_waterfall.csv"), row.names = FALSE)

    rm(.d11, .base_w, .base_treat, .base_never, .pre, .cap)
    if (exists(".n_emp")) rm(.n_emp)
    if (exists(".top")) rm(.top)
    if (exists(".m_ext")) rm(.m_ext)
  }
} else {
  cat("\n  [skip] T11.\n")
  if (!exists("ev"))
    cat("  Reason: no event frame in scope. The preflight above says why.\n")
  else
    cat(sprintf("  Reason: missing columns: %s\n",
                paste(setdiff(.t11_need, names(ev)), collapse = ", ")))
}

rm(.t10_need, .t11_need)

# ---- write this module's ledger --------------------------------------
# Written before the log closes so the path appears in the log, and
# written unconditionally so a run with 03's report() present still
# leaves a self-contained record of what 04 estimated.
if (length(.fs_ledger)) {
  .fs_led <- dplyr::bind_rows(.fs_ledger)
  if (!isTRUE(TD_NO_WRITE)) {
    .f <- file.path(OUTDIR, "04_financing_space_ledger.csv")
    utils::write.csv(.fs_led, .f, row.names = FALSE)
    cat(sprintf("\nLedger: %s rows written to %s\n", nrow(.fs_led),
                normalizePath(.f, mustWork = FALSE)))

    # v2.8. MERGE INTO THE SHARED LEDGER HERE, because nothing else will.
    #
    # This file contributes thirty rows through report(), and every other
    # contributor writes them out in 03_finish.R. This one has no
    # finisher. Until now its rows reached training_ledger.csv only when a
    # 03 module happened to run afterwards in the same warm session, which
    # is not a mechanism: it is why three runs on 7 September left ninety
    # rows where thirty belong, and why a clean RUN_ALL pass would have
    # left none at all, since the driver runs 04 last and in its own
    # process.
    #
    # SAME SEMANTICS AS 03_finish.R, deliberately: this module's tags win
    # and every other tag carries forward. A module that rewrites its own
    # rows must not delete anyone else's.
    if (exists("td_put", mode = "function")) {
      .prev <- if (exists("td_has", mode = "function") && td_has("ledger"))
        td_get("ledger") else if (file.exists(file.path(OUTDIR, "training_ledger.csv")))
          utils::read.csv(file.path(OUTDIR, "training_ledger.csv"),
                          stringsAsFactors = FALSE) else NULL
      # AN UNTAGGED ROW CANNOT BE DE-DUPLICATED, SO IT CANNOT BE CARRIED.
      # The exclusion below is by tag, and NA %in% anything is FALSE, so a
      # row with no tag survives every pass and is re-bound on the next. It
      # is a pure accumulator, and it ran: the T10 workplace-state rows
      # reach the ledger with tag and n both NA, thirty of them became
      # sixty and then ninety, and the shared file went 70 -> 100 across
      # two passes of the same three stages. They look like data, with a
      # label, a standard error and a status, which is why nothing caught
      # it.
      #
      # Dropped and COUNTED. Keeping them doubles them; keeping the first
      # needs an identity they do not have. Sixty rows leaving in silence
      # would be the same failure in the other direction.
      .fs_drop_untagged <- function(x, where) {
        if (is.null(x) || !nrow(x)) return(x)
        if (!"tag" %in% names(x)) {
          cat("  *** ", where, ": no tag column; dropping ", nrow(x),
              " rows.\n", sep = "")
          return(x[0, , drop = FALSE])
        }
        .b <- is.na(x$tag) | !nzchar(trimws(as.character(x$tag)))
        if (any(.b)) {
          cat("  *** ", sum(.b), " untagged row(s) dropped from ", where,
              ".\n", sep = "")
          if ("label" %in% names(x))
            cat("      first: ",
                substr(as.character(x$label[which(.b)[1]]), 1, 58), "\n",
                sep = "")
          cat("      report() must supply a tag. An untagged estimate\n",
              "      cannot be graded, reconciled or excluded, and the T10\n",
              "      workplace-state block is where these come from.\n",
              sep = "")
        }
        x[!.b, , drop = FALSE]
      }
      # THE TWO LEDGERS HAVE DIFFERENT SCHEMAS, and that is what "no tag
      # column" meant. .fs_ledger builds rows with id, units and
      # benchmark_female; training_ledger.csv calls them tag, unit and
      # benchmark_f. Binding the two produced a frame where every 04 row
      # had tag = NA, which is why they could never be excluded and why
      # they accumulated. The names are mapped before anything else.
      #
      # AND THE EXTRA COLUMNS MUST NOT TRAVEL. .fs_ledger carries an
      # `estimate` column; training_ledger.csv has none, deliberately,
      # because it is written under blinding and a point estimate in it
      # would defeat the whole arrangement. So the merge keeps only the
      # columns the shared ledger already has, and says what it dropped.
      .fs_to_shared <- function(x) {
        if (is.null(x) || !nrow(x)) return(x)
        .ren <- c(id = "tag", units = "unit", benchmark_female = "benchmark_f")
        for (.a in names(.ren))
          if (.a %in% names(x) && !.ren[[.a]] %in% names(x))
            names(x)[names(x) == .a] <- .ren[[.a]]
        .keep <- if (!is.null(.prev)) intersect(names(x), names(.prev)) else
          setdiff(names(x), c("coef", "estimate", "version"))
        .lost <- setdiff(names(x), .keep)
        if (length(.lost))
          cat("  columns not carried into the shared ledger: ",
              paste(.lost, collapse = ", "),
              if ("estimate" %in% .lost)
                "  (estimate is withheld by design)" else "", "\n", sep = "")
        x[, .keep, drop = FALSE]
      }
      .mine <- .fs_drop_untagged(.fs_to_shared(.fs_led),
                                 "04financing_space this run")
      .prev <- .fs_drop_untagged(.prev,   "the carried ledger")
      if (!"module" %in% names(.mine)) .mine$module <- "04financing_space"
      .mine$module[is.na(.mine$module) | !nzchar(.mine$module)] <- "04financing_space"
      .all <- if (is.null(.prev)) .mine else
        dplyr::bind_rows(.mine, .prev[!(.prev$tag %in% .mine$tag), , drop = FALSE])
      .all <- .all[order(.all$tag), , drop = FALSE]
      td_put("ledger", .all)
      utils::write.csv(.all, file.path(OUTDIR, "training_ledger.csv"),
                       row.names = FALSE)
      cat(sprintf("  merged into training_ledger.csv: %d rows total\n",
                  nrow(.all)))
      if ("module" %in% names(.all)) print(table(.all$module))
      rm(.prev, .mine, .all)
    } else {
      cat("  00_contract.R is not loaded, so the shared ledger was not\n")
      cat("  updated. This module's rows exist only in its own csv.\n")
    }

    rm(.f)
  } else {
    cat(sprintf("\nLedger: %s rows held (TD_NO_WRITE is TRUE).\n",
                nrow(.fs_led)))
  }
  if (exists("td_put", mode = "function"))
    try(td_put("ledger_04", .fs_led), silent = TRUE)

  # =====================================================================
  # OWN-TIME DECOMPOSED: what Table 7's own-time arm actually contains
  # ---------------------------------------------------------------------
  # WAS Report_own_time_decomposition.R, folded in at v21.46. It read this
  # module's ledger back off disk, which meant it needed a directory, and
  # the directory it needed was not always the one 03's ledger was in. It
  # resolved one path by looking for training_ledger.csv and then expected
  # 04_financing_space_ledger.csv to be beside it; on the 27 August run 03
  # had written to output/designs and 04 had inherited
  # OUTDIR = figures_explore, so both files existed and the stage reported
  # the 04 ledger missing. That sent the reader to re-run a module whose
  # output was already on disk.
  #
  # Here .fs_led is in memory and there is no lookup to get wrong. Only
  # 03's ledger still has to be found, and it is optional: without it the
  # split still prints, it just cannot be reconciled against the published
  # arm.
  #
  # THE EMPLOYED ARM is the frame Table 7's own-time row uses. The asked
  # population is reported alongside rather than instead: the two differ by
  # the employment routing, which is what Section 4.3 spends a page on, so
  # showing one and not the other would hide it.
  .pull <- function(pre, st) {
    r <- .fs_led[.fs_led$id %in% paste0(pre, st), , drop = FALSE]
    if (!nrow(r)) return(NA_real_)
    sum(r$estimate, na.rm = TRUE)
  }
  .scale <- function(pre, st) {
    r <- .fs_led[.fs_led$id %in% paste0(pre, st), , drop = FALSE]
    if (!nrow(r)) return(NA_real_)
    # Components are fitted on the same rows, so their errors are not
    # independent and this is NOT a standard error for the sum. Printed as
    # a scale and labelled as one, because a quadrature figure here would
    # invent a precision nobody estimated.
    sqrt(sum(r$se^2, na.rm = TRUE))
  }

  cat("\n", strrep("-", 70), "\n", sep = "")
  cat("  OWN-TIME DECOMPOSED: what Table 7's own-time arm contains\n")
  cat(strrep("-", 70), "\n", sep = "")

  .otd <- NULL
  for (.arm in list(c(pre = "T10_pe_emp_", lab = "employed only"),
                    c(pre = "T10_pe_",     lab = "asked population"))) {
    .p <- .arm[["pre"]]
    .pure <- .pull(.p, c("wp_own_nofee", "wp_own_fee"))
    .mix  <- .pull(.p, c("wp_mixed_nofee", "wp_mixed_fee"))
    .rel  <- .pull(.p, c("wp_released_nofee", "wp_released_fee"))
    .els  <- .pull(.p, c("elsewhere_nofee", "elsewhere_fee"))
    if (!is.finite(.pure) || !is.finite(.mix)) next
    .tot <- .pure + .mix
    cat("\n  --- ", .arm[["lab"]], " ---\n", sep = "")
    cat(sprintf("    pure own time                    %+8.4f  (scale %.4f)\n",
                .pure, .scale(.p, c("wp_own_nofee", "wp_own_fee"))))
    cat(sprintf("    own time WITH released time      %+8.4f  (scale %.4f)\n",
                .mix, .scale(.p, c("wp_mixed_nofee", "wp_mixed_fee"))))
    cat(sprintf("    total, = Table 7's own-time arm  %+8.4f\n", .tot))
    if (abs(.tot) > 1e-8)
      cat(sprintf("    %.0f%% of the arm is the MIXED component.\n",
                  100 * abs(.mix) / abs(.tot)))
    cat(sprintf("    for context: released %+.4f, elsewhere %+.4f\n",
                .rel, .els))
    # THE CONCENTRATION, WRITTEN. Section 6.1 types that the joint states
    # hold 73.5 per cent of the pre-birth own-time base and 86.4 per cent of
    # the decline, an index of 1.17, and nothing wrote them. The base is the
    # benchmark each state's ledger row carries, the mothers' pre-birth
    # share; BM_PROP scales every row alike and cancels in the ratio.
    .bm <- function(pre, st) {
      r <- .fs_led[.fs_led$id %in% paste0(pre, st), , drop = FALSE]
      if (!nrow(r) || !"benchmark" %in% names(r)) return(NA_real_)
      sum(r$benchmark, na.rm = TRUE)
    }
    .b_mix  <- .bm(.p, c("wp_mixed_nofee", "wp_mixed_fee"))
    .b_pure <- .bm(.p, c("wp_own_nofee", "wp_own_fee"))
    .sh_base <- .b_mix / (.b_mix + .b_pure)
    .sh_decl <- if (abs(.tot) > 1e-8) .mix / .tot else NA_real_
    cat(sprintf("    joint states: %.1f%% of the pre-birth base, %.1f%% of the decline, index %.2f\n",
                100 * .sh_base, 100 * .sh_decl, .sh_decl / .sh_base))
    .otd <- rbind(.otd, data.frame(arm = .arm[["lab"]], pure_own = .pure,
                                   mixed = .mix, total = .tot,
                                   released = .rel, elsewhere = .els,
                                   mixed_share_of_base = .sh_base,
                                   mixed_share_of_decline = .sh_decl,
                                   concentration_index = .sh_decl / .sh_base,
                                   stringsAsFactors = FALSE))
    rm(.bm, .b_mix, .b_pure, .sh_base, .sh_decl)
  }

  # RECONCILE AGAINST 03. The split is worth nothing unless its total lands
  # on the number the paper reports; otherwise the two are fitted on
  # different rows and the decomposition attaches to a neighbouring sample.
  # That is the same warning this module prints about its own comparability
  # check, and it applies here too.
  .f_led <- local({
    cand <- unique(c(if (exists("TD_OUTDIR")) TD_OUTDIR, OUTDIR,
                     file.path(getwd(), "training_designs"),
                     file.path(getwd(), "output", "designs"), getwd()))
    hit <- cand[file.exists(file.path(cand, "training_ledger.csv"))]
    if (length(hit)) file.path(hit[1], "training_ledger.csv") else NA_character_
  })
  if (!is.na(.f_led) && !is.null(.otd)) {
    .led <- utils::read.csv(.f_led, stringsAsFactors = FALSE)
    .r <- .led[.led$tag == "T1_jttpeot_any_employed", , drop = FALSE]
    if (nrow(.r)) {
      .t <- .otd$total[.otd$arm == "employed only"][1]
      cat(sprintf("\n  03's own-time arm: n = %s, SE = %.4f (%s)\n",
                  format(.r$n[1], big.mark = ","), .r$se[1], basename(.f_led)))
      cat(sprintf("  04's states sum to %+.4f. Table 7 prints -0.018.\n", .t))
      cat("  Agreement is what licenses reading the split as a decomposition\n")
      cat("  of the published arm rather than of a neighbouring sample.\n")
    }
  } else if (is.na(.f_led)) {
    cat("\n  training_ledger.csv not found, so the split is printed but not\n")
    cat("  reconciled against the published arm. Run 03 first if that\n")
    cat("  check is wanted.\n")
  }

  if (!is.null(.otd) && !isTRUE(TD_NO_WRITE)) {
    .fo <- file.path(OUTDIR, "own_time_decomposition.csv")
    utils::write.csv(.otd, .fo, row.names = FALSE)
    cat("\n  Written: ", normalizePath(.fo, mustWork = FALSE), "\n", sep = "")
    rm(.fo)
  }
  if (!is.null(.otd)) {
    cat("\n  FOR SECTION 6.1. The paper reports own-time as bounded at\n")
    cat("  -0.018 with an MDE of 0.022 and says the margin does not move.\n")
    cat("  On this split the margin the WORKER controls is flat and the\n")
    cat("  movement in the arm is employer withdrawal arriving through a\n")
    cat("  definition that counts any own-time mention. That is the\n")
    cat("  stronger version of the paper's own argument. It is descriptive\n")
    cat("  and post-hoc in this ledger, so it carries no pre-analysis\n")
    cat("  status and must be reported as such.\n")
  }
  rm(.pull, .scale, .otd, .f_led)

  rm(.fs_led)
} else {
  cat("\nLedger: no rows collected, so nothing to write. If designs ran\n")
  cat("  above, .rep() failed to read their coefficients and that is the\n")
  cat("  thing to chase.\n")
}
rm(.fs_ledger)

cat("\n--- 04financing_space.R complete ---\n")

# =====================================================================
# CLOSE THE LOG
# ---------------------------------------------------------------------
# Close only what this file opened, then PROVE the stack is level. A
# module that quietly changes its caller's sink depth produces a caller
# whose output goes nowhere, which is the least diagnosable failure in
# this pipeline: nothing errors, the run continues, and the log simply
# stops. Warn loudly rather than let that recur.
# =====================================================================
if (isTRUE(.fs_owns_sink)) fs_log_close()

if (sink.number() != .fs_sink_out_before ||
    sink.number(type = "message") != .fs_sink_msg_before)
  warning("04financing_space.R left the sink stack unbalanced (output ",
          .fs_sink_out_before, " -> ", sink.number(), ", message ",
          .fs_sink_msg_before, " -> ", sink.number(type = "message"),
          "). The caller's log may be dead from here on.", call. = FALSE)

if (exists("td_log_close", mode = "function"))
  try(td_log_close(), silent = TRUE)

rm(.fs_sink_out_before, .fs_sink_msg_before)
