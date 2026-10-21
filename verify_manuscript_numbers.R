# =====================================================================
# verify_manuscript_numbers.R   revision 2
#
# Checks every number the manuscript asserts against the artefacts the
# last completed run of 03training_designs.R left on disk.
#
# WHY THIS EXISTS. The 38.4 per cent pre-birth base in section 7 came
# from a superseded build, where it sat beside a -0.131 coefficient with
# SE 0.0254. The current run gives 29.8 per cent and -0.117 with SE
# 0.0155. Nothing errored, nothing was flagged, and the only reason it
# was caught is that someone read two documents side by side. A hardcoded
# number in a .tex cannot be checked by recompiling, so it has to be
# checked against the ledger by something that runs.
#
# NEW IN REVISION 2
#   SNAPSHOT and DIFF. The ledger and the sealed estimates are rewritten
#     at the END of every pass, so a completing run destroys the vintage
#     you last verified against. Snapshot before, diff after, and the
#     question "did anything move" has an answer rather than a belief.
#   CHECK 5, T8. No T8 result passes through report(), so neither the
#     ledger nor the sealed estimates contain a single T8 number.
#     Section 8.3 is verifiable only against the T8_*.csv files, and
#     nothing else in this script touches them.
#
# NEW IN REVISION 3
#   CHECK 5's PREMISE HAS EXPIRED, and the check should be read with that
#     in mind rather than deleted. 03c now registers its twenty arms
#     through report_manual(), so the ledger carries T8 rows with n, se
#     and mde and the fingerprint reaches them like anything else. The
#     T8_*.csv files remain the only source of the point estimates, which
#     is why Check 5 still runs, but "nothing above can see T8" is no
#     longer true.
#   CHECK 9, THE PROSE. Everything above checks a CURATED set: rows parsed
#     from tab:training_main, and the hand-typed hours_claims and
#     wage_claims. That is the right instrument for a claim someone
#     registered and no instrument at all for one nobody did. Section 6.1
#     published a concentration interval of [1.00, 1.30] for a statistic
#     the bootstrap puts at [0.988, 1.273], and a share loss of
#     [-15.0, -0.1] for one whose interval contains zero. Both were
#     computed correctly, printed to a console, written to no file, and
#     never added to a claim list, so nothing here looked at them for
#     months. Check 9 enumerates from the MANUSCRIPT instead of from a
#     list: every numeric literal in the prose, matched against every
#     value in every artefact, at the literal's own printed precision. A
#     number cannot escape it by not being registered.
#
# WHAT IT READS, all read-only:
#   training_ledger.csv            tag, n, se, mde, benchmark, verdict,
#                                  unit, fe, status.  NO estimate column.
#   BLIND_training_estimates.rds   the .blind list, keyed by tag, each
#                                  element one fixest coeftable row. This
#                                  is the ONLY place the coefficients live.
#   T0_pre_birth_base_rates.csv    the denominators, by sex.
#   T8_dml_swap.csv                arm, unit, his/her/gap, se, p, per sd.
#   T8_arm_significance.csv        the gap-exists precondition, per arm.
#   T8_reproduction_pooled.csv     the published-units arm against target.
#
# WHAT IT WRITES: nothing, unless SNAPSHOT or WRITE_REPORT is set.
#
# THE FINGERPRINT PRINCIPLE. Under BLIND the log prints tag, n, SE and
# MDE and withholds the estimate, so (n, se, mde) is a vintage fingerprint
# that survives blinding. Any manuscript number carrying an SE or an MDE
# can be checked without unblinding anything. Fill in `claims` from the
# .tex and the mismatches are the ones to go and look at.
# =====================================================================

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select <- dplyr::select;       filter    <- dplyr::filter
mutate <- dplyr::mutate;       slice     <- dplyr::slice
recode <- dplyr::recode;       rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange <- dplyr::arrange;     count     <- dplyr::count;  lag <- dplyr::lag

suppressPackageStartupMessages(library(dplyr))

# =====================================================================
# LOG  (revision 3)
#
# THE VERIFICATION RECORD WAS THE ONE THING NOT BEING RECORDED. Nine
# checks print a page of verdicts and this file wrote none of them
# anywhere, so the only copy of "all six horizons reconcile" or "the
# bounds exclude zero at +8 through +11" was a console buffer. That is
# the same defect the checks exist to catch, one level up: Section 6.1
# published an interval that had been computed correctly and printed to a
# transcript nobody kept.
#
# IT OWNS THE SINK ONLY IF NOTHING ELSE DOES, for the reason that put
# 04's output inside a log named 03c: a module that errored before its
# finisher leaves a sink open, and opening another on top writes this run
# into that module's transcript. Record the incoming depth, open only at
# zero, unwind only to what was recorded.
# =====================================================================
# v34. AN ORPHANED SINK DISABLES LOGGING FOR THE REST OF THE SESSION.
#
# Refusing to open a log while another is recording is right, and on its own
# it is a trap: one run that died with its sink open stops every later script
# in that session from writing a log, and each of them politely explains why
# while the run_logs directory stays empty. Two scripts in a row did exactly
# that, and neither would close the sink because neither owned it.
#
# 00_contract.R already settles this for its own logs and the test is its
# test: .TD_SINK_BASE is the depth when the contract first loaded, so a sink
# ABOVE it that the registry does not account for belongs to a run that died.
# Below or at it belongs to whoever sourced us and closing it would destroy
# their log. Set VM_KEEP_SINK <- TRUE to keep an inherited sink regardless.
.vm_base <- if (exists(".TD_SINK_BASE", envir = globalenv()))
  get(".TD_SINK_BASE", envir = globalenv()) else 0L
.vm_acct <- exists(".TD_LOG", envir = globalenv()) &&
  exists("depth", envir = get(".TD_LOG", envir = globalenv()))
if (isTRUE(get0("VM_FORCE_LOG", ifnotfound = FALSE))) {
  cat("  VM_FORCE_LOG: unwinding ", sink.number(), " sink(s) to zero.\n", sep = "")
  while (sink.number(type = "message") > 2L) sink(type = "message")
  while (sink.number() > 0L) sink()
  .vm_base <- 0L
} else if (!isTRUE(get0("VM_KEEP_SINK", ifnotfound = FALSE)) &&
    sink.number() > .vm_base && !.vm_acct) {
  cat("  a sink was open at depth ", sink.number(), " and no log in this ",
      "session\n  accounts for it, so a previous run died with it open. ",
      "Unwinding to ", .vm_base, "\n  and opening this run's own log. ",
      "td_log_rescue() does the same by hand.\n", sep = "")
  # The message sink floors at 2, so `> 0` here spins forever. The contract
  # found that the hard way in td_log_rescue().
  while (sink.number(type = "message") > 2L) sink(type = "message")
  while (sink.number() > .vm_base) sink()
}
.vm_sink_before <- sink.number()
.vm_msg_before  <- sink.number(type = "message")
.vm_owns <- (.vm_sink_before == 0L)
.vm_log  <- NA_character_
if (.vm_owns) {
  .vm_dir <- if (exists("LOG_DIR")) LOG_DIR else
    file.path(if (exists("PROJ_ROOT")) PROJ_ROOT else getwd(), "run_logs")
  dir.create(.vm_dir, showWarnings = FALSE, recursive = TRUE)
  .vm_log <- file.path(.vm_dir, paste0(
    "verify_manuscript_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
  sink(.vm_log, split = TRUE)
  cat("Log: ", .vm_log, "\n", sep = "")
  cat("  (standalone run; this file owns the sink and will close it)\n")
  cat("  If a check aborts, restore the console with: vm_log_close()\n")
} else {
  # WHICH OF THE THREE CASES IT IS. "A sink is already open" has now been
  # printed three times for two different reasons, and the remedy differs:
  # a poisoned baseline needs the contract re-sourced after a rescue, an
  # accounted-for log needs whichever module opened it to close it, and a
  # bare orphan is reclaimed automatically and never reaches this branch.
  cat("Log: NONE WRITTEN THIS RUN.\n",
      "  depth ", sink.number(), ", baseline ", .vm_base,
      ", registry log ", if (.vm_acct) "OPEN" else "none", ".\n",
      if (.vm_base > 0L)
        "  The baseline is not zero, so a sink was open when the contract\n  loaded and everything treats that depth as normal. td_log_rescue()\n  then re-source 00_contract.R.\n"
      else if (.vm_acct)
        "  A registry log is open, so a module called td_log_open() and did\n  not close it. td_log_close() in that module, or td_log_rescue().\n"
      else "",
      "  Or set VM_FORCE_LOG <- TRUE to unwind to zero and take the console.\n",
      sep = "")
}
vm_log_close <- function() {
  if (!isTRUE(get0(".vm_owns", ifnotfound = FALSE))) {
    cat("vm_log_close(): this file does not own a sink; nothing to do.\n")
    return(invisible(NULL))
  }
  while (sink.number(type = "message") > .vm_msg_before) sink(type = "message")
  while (sink.number() > .vm_sink_before) sink()
  cat("Log written to ", .vm_log, "\n", sep = "")
  invisible(.vm_log)
}

# ---------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------
# INHERITED IS NOT THE SAME AS INTENDED, and this script learned that the
# hard way. It used to take OUTDIR from the session if the name existed.
# RUN_FIGURES_EDA.R sets OUTDIR <- "figures_explore" at its line 78, so
# sourcing this afterwards in a warm session looked for the ledger in the
# figures folder and reported the absence as "03 is mid-flight". A wrong
# directory was diagnosed as a wrong moment.
#
# Directories are now resolved by CONTENT, not by name. Each is identified
# by a marker file that only the right directory has, candidates are tried
# in order, and an inherited value that fails the test is reported rather
# than silently dropped. RUN_FIGURES_EDA.R makes exactly this argument
# about its own flags at line 132.
# THE ROOT COMES FROM paths.R, NOT FROM A LITERAL. This fell back to a
# HILDA_project path that is not this tree, so a run without paths.R
# resolved every candidate below against another project and reported the
# ledger missing rather than stopping.
if (!exists("project_dir")) {
  if (exists("PROJ_ROOT")) project_dir <- PROJ_ROOT
  else if (nzchar(Sys.getenv("HILDA_ROOT")))
    project_dir <- normalizePath(Sys.getenv("HILDA_ROOT"), winslash = "/",
                                 mustWork = FALSE)
  else if (file.exists("paths.R")) source("paths.R")
  else stop("project_dir is unset and paths.R is not in ", getwd(),
            ".\n  Source paths.R first.", call. = FALSE)
}

# v4. WIDEN RATHER THAN STOP, and drop the NA the last failure left behind.
#
# On 8 September this reported that wage_path_ledger.csv was in none of
# three candidates, the first of which printed as "NA". Two faults. The NA
# is the inherited value: a previous failed resolution assigns WGDIR
# NA_character_ in the global environment, and re-sourcing then puts that NA
# at the head of the list it tries. And the file was two directories away in
# output/results, a sibling of OUTDIR that nothing enumerated, so checks 6,
# 7, 7b and 8 all reported UNVERIFIED for a path reason and read as findings.
#
# A path is a better answer than a list of places a file is not, so a miss
# now searches under the project root and names where the marker actually
# is. More than one hit is reported with mtimes and the newest is taken,
# which is the convention .find_tex already uses; anything under _archive/
# is excluded, because a retired copy resolving a live path is worse than
# not resolving it.
.resolve_dir <- function(nm, marker, candidates, root = NULL) {
  .inh <- if (exists(nm, envir = globalenv())) get(nm, envir = globalenv()) else NULL
  .good <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  if (!.good(.inh)) .inh <- NULL
  .try <- unique(c(.inh, candidates))
  .try <- .try[vapply(.try, .good, logical(1))]
  # _archive/ is excluded from the CANDIDATES too, not only from the widened
  # search. Enumerating the folders beside OUTDIR pulls it in, it sorts before
  # results/, and a retired copy then resolves a live path silently. Nothing
  # reads _archive by design, so nothing should resolve into it by accident.
  .try <- .try[!grepl("(^|/)_archive(/|$)", .try)]
  for (d in .try) {
    if (file.exists(file.path(d, marker))) {
      if (!is.null(.inh) && !identical(d, .inh))
        cat("  NOTE: inherited", nm, "=", .inh, "has no", marker,
            "- using", d, "instead.\n")
      return(d)
    }
  }
  .r <- if (!is.null(root)) root else
    if (exists("PROJ_ROOT", envir = globalenv())) get("PROJ_ROOT", envir = globalenv()) else NULL
  if (.good(.r) && dir.exists(.r)) {
    cat("  ", nm, ": not in ", length(.try), " candidate folder(s); widening under ",
        .r, "\n", sep = "")
    hit <- list.files(.r, pattern = utils::glob2rx(marker),
                      recursive = TRUE, full.names = TRUE)
    hit <- hit[!grepl("(^|/)_archive(/|$)", hit)]
    if (length(hit)) {
      hit <- hit[order(file.info(hit)$mtime, decreasing = TRUE)]
      if (length(hit) > 1L) {
        cat("  *** ", marker, " is in ", length(hit), " folders. Using the newest:\n",
            sep = "")
        for (h in hit)
          cat(sprintf("        %s  %s\n",
                      format(file.info(h)$mtime, "%Y-%m-%d %H:%M"), dirname(h)))
      }
      cat("  ", nm, " resolved to ", dirname(hit[1]), "\n", sep = "")
      return(dirname(hit[1]))
    }
  }
  stop("Could not locate ", nm, ": no candidate contains ", marker,
       ", and it is\n  nowhere under ", if (.good(.r)) .r else "any root given",
       ".\n  Tried:\n    ", paste(.try, collapse = "\n    "),
       "\n  Set ", nm, " explicitly, or run from the right working directory.",
       call. = FALSE)
}

# output/designs FIRST, because that is where 03 and 04 have written since
# the root moved. The list held only training_designs/ and the working
# directory, so when the figures stage left OUTDIR pointing at a relative
# "figures" the resolver had nowhere correct to fall back to and stopped
# with "Could not locate OUTDIR". DIR_DESIGNS is the contract's sealed
# value and is tried before anything derived here.
OUTDIR <- .resolve_dir("OUTDIR", "training_ledger.csv", c(
  if (exists("DIR_DESIGNS")) DIR_DESIGNS,
  file.path(project_dir, "output", "designs"),
  file.path(project_dir, "training_designs"),
  file.path(getwd(), "output", "designs"),
  file.path(getwd(), "training_designs"),
  getwd()))

SNAPDIR <- file.path(OUTDIR, "snapshots")

# v-fix. THESE FOUR WERE PLAIN ASSIGNMENTS AND CLOBBERED THE CALLER.
#
# The footer of every log this file writes tells the reader to do
#
#     SNAPSHOT <- TRUE; source("verify_manuscript_numbers.R")
#
# and that cannot work: the assignment above resets SNAPSHOT to FALSE before
# anything reads it. On 13 September a wrapper set SNAPSHOT and then
# DIFF_AGAINST around a run of 03a and 03b, and got two plain verifications
# with no snapshot and no diff. Nothing in either log said so, because from
# the script's point of view nothing had been asked for.
#
# HONOURED, AND THE BRANCH IS NAMED. A caller-set value now survives, and
# the console says which of the four came from the caller. That second half
# matters as much as the first: an option left over from an earlier run in
# the same session is the other failure mode, and it is the one 03_common.R
# documents at its DML_FOLDS block. Silence about the source is what makes
# both faults invisible.
#
# WRITE_REPORT is included because it has the same shape, even though it
# happens to be set immediately before every current call.
.vm_set <- character(0)
for (.nm in c("SNAPSHOT", "SNAPSHOT_NAME", "DIFF_AGAINST", "WRITE_REPORT")) {
  if (exists(.nm, envir = globalenv(), inherits = FALSE)) {
    .vm_set <- c(.vm_set, .nm); next
  }
  assign(.nm, switch(.nm, SNAPSHOT = FALSE, WRITE_REPORT = FALSE,
                     NA_character_), envir = globalenv())
}
rm(.nm)
if (length(.vm_set))
  cat("  from the caller: ",
      paste(sprintf("%s = %s", .vm_set,
                    vapply(.vm_set, function(n)
                      paste(format(get(n, envir = globalenv())), collapse = ","),
                      character(1))), collapse = " | "),
      "\n    Anything here you did not set this session is a leftover.\n",
      sep = "")
rm(.vm_set)

# TOLERANCE FOLLOWS THE DECIMALS THE .tex ACTUALLY QUOTES. TOL_EST is half a
# unit in the third decimal, which is right for percentage points and wrong
# for hours and courses: Table 1's own note says "units are percentage points
# except hours (hours per year) and courses (counts)", and the paper quotes
# those to two decimals because a hundredth of an hour is noise. So
# T5_hours_uncond at -4.99 was flagged against a run value of -4.98721, which
# is its correct two-decimal rounding, and the check demanded a third decimal
# the paper deliberately does not print.
#
# .tol_for() reads the tolerance off the claim itself: half a unit in the last
# decimal the .tex shows. A claim written to three decimals is still held to
# 0.0006; one written to two is held to 0.006, and nothing has to be listed by
# tag.
# How many decimals the .tex actually quotes for a claim. Pulled out of
# .tol_for() because Check 1b needs the same answer and was assuming three.
.dp_of <- function(x) vapply(as.character(x), function(v) {
  v <- sub("^[+-]", "", trimws(v))
  if (!grepl("\\.", v)) return(0L)
  nchar(sub("^[0-9]*\\.", "", v))
}, integer(1), USE.NAMES = FALSE)

.tol_for <- function(x, floor_tol) pmax(0.5 * 10^(-.dp_of(x)), floor_tol)

# DOES THE MANUSCRIPT ALREADY SAY IT?
#
# Checks 4, 5 and 8 each ended in a cat() that ran whatever the paper said, so
# all three went on asking for sentences the paper had carried for weeks: the
# reproduction restriction at line 4476, the Lee-bound sentence at 3262, and
# note d's explanation of the one status that diverges from the ledger. An
# advisory that fires after the fix is Check 1b's bug in a milder form. It
# teaches you to read past the line, and the next time it matters you will.
#
# Marker phrases rather than whole sentences, because the wording is the
# author's and only the commitment is the check's business.
.tex_says <- function(...) {
  if (is.na(TEXFILE) || !file.exists(TEXFILE)) return(NA)
  if (!exists(".tex_all", envir = globalenv())) {
    .l <- readLines(TEXFILE, warn = FALSE)
    .d <- dirname(TEXFILE)
    for (f in unique(unlist(regmatches(.l, gregexpr("(?<=\\\\input\\{)[^}]+", .l,
                                                    perl = TRUE))))) {
      .p <- if (grepl("[.]tex$", f)) file.path(.d, f) else
        file.path(.d, paste0(f, ".tex"))
      if (file.exists(.p)) .l <- c(.l, readLines(.p, warn = FALSE))
    }
    assign(".tex_all", gsub("\n", " ", paste(.l, collapse = "\n")),
           envir = globalenv())
  }
  all(vapply(c(...), function(m)
    grepl(m, get(".tex_all", envir = globalenv()), fixed = TRUE), logical(1)))
}

# v5. THE FLOOR WAS A CEILING FOR ANY CLAIM QUOTED MORE PRECISELY.
#
# pmax() takes the LARGER of half-a-unit-in-the-last-digit and the floor, so
# a claim printed to four decimals was still held to 0.0006 rather than
# 0.00005, and three-decimal claims were held to 0.0006 rather than 0.0005.
# The header above says the tolerance follows the decimals the .tex quotes
# and it did not: it followed them upward only. Quoting an extra digit is
# how Check 1b's three boundary rows are being fixed, and with a floor of
# 0.0006 that extra digit would have bought nothing.
#
# The floor is now numerical rather than editorial: it exists so a claim is
# not failed on floating-point noise, and nothing else.
TOL_EST <- 5e-7
TOL_SE  <- 5e-7
TOL_N   <- 0L       # sample sizes must match exactly
TOL_T8  <- 0.0006   # T8 is quoted in percent to 3 decimals

# --- vm_guards.R ------------------------------------------------------
#
# WHAT IT ADDS. Every check reports a DENOMINATOR, so a conclusion drawn
# over an empty input shows as NOT CHECKED instead of reading as a pass.
# Four checks in this file were caught doing exactly that: Check 5
# announced "All 0 rows of Table 3 reconcile" when app_arms.tex was
# absent; Check 9 scanned the folder it writes its own report into and
# reported 100 per cent; Check 9's noise filters used substr(), which is
# not vectorised over start and stop, so the mask came back length 1 and
# was recycled over every token; and app_arms.tex resolved from TEXDIR,
# which paper_tables.R sets, so the lookup turned on session state.
#
# PARTIAL RETROFIT IS SAFE. A check that has not been converted reports
# NO REPORT rather than breaking, so this can be done one check at a
# time and an unconverted check is visible rather than silent.
#
# THE FALLBACK IS NO-OP, NOT ABSENT. Without the sidecar the checks still
# run and still print what they always printed; only the denominators and
# the verdict table are missing, and the console says so.
if (file.exists("vm_guards.R")) {
  source("vm_guards.R")
  vm_reset(own_outputs = c("prose_numbers_check.csv",
                           "manuscript_reconciliation.csv",
                           "verify_report.csv"))
} else {
  cat("\n  vm_guards.R not found. The checks below run as before, but none\n",
      "  reports a denominator and there is no verdict table.\n", sep = "")
  vm_verdict <- function(...) invisible(NULL)
  vm_input   <- function(path, ...) if (file.exists(path)) path else NA_character_
  vm_mask    <- function(x, mask, ...) mask
  vm_scan    <- function(paths, ...) paths
  vm_summary <- function() invisible(NULL)
  vm_gate    <- function() TRUE
}

# ---------------------------------------------------------------------
# File handles
# ---------------------------------------------------------------------
f_ledger <- file.path(OUTDIR, "training_ledger.csv")
f_sealed <- file.path(OUTDIR, "BLIND_training_estimates.rds")
f_decide <- file.path(OUTDIR, "training_decisions.txt")
f_base   <- file.path(OUTDIR, "T0_pre_birth_base_rates.csv")
f_t8     <- file.path(OUTDIR, "T8_dml_swap.csv")
f_t8sig  <- file.path(OUTDIR, "T8_arm_significance.csv")
f_t8rep  <- file.path(OUTDIR, "T8_reproduction_pooled.csv")

# WG/ artefacts. These live in the working directory of MASTER_training.R,
# not in OUTDIR, because the masters write beside themselves. Three
# manuscript claims rest on them and nothing was reading them.
# THE SIBLINGS OF OUTDIR ARE ENUMERATED, NOT NAMED. output/results holds the
# wage ledger, the training ledger and the Lee bounds, and it was in no
# candidate list because nothing thought to write "results". Listing the
# folder above OUTDIR finds it, and finds the next one too.
WGDIR    <- tryCatch(
  .resolve_dir("WGDIR", "wage_path_ledger.csv",
               c(if (exists("DIR_RESULTS")) DIR_RESULTS,
                 getwd(),
                 list.dirs(dirname(OUTDIR), recursive = FALSE),
                 file.path(dirname(OUTDIR), ".."),
                 project_dir),
               root = if (exists("PROJ_ROOT")) PROJ_ROOT else dirname(OUTDIR)),
  error = function(e) { cat("  NOTE:", conditionMessage(e), "\n"); NA_character_ })
.wg <- function(f) if (is.na(WGDIR)) "" else file.path(WGDIR, f)
f_hours  <- .wg("hours_robustness.csv")
f_lee    <- .wg("cs_event_study_lee_bounds.rds")
f_wage   <- .wg("wage_path_ledger.csv")
f_timing <- .wg("timing_paths.csv")
f_train  <- .wg("train_ledger.csv")
f_es     <- .wg("cs_event_study_results.rds")

CORE <- c(f_ledger, f_sealed, f_decide, f_t8, f_t8sig, f_t8rep)

stamp <- function(f) if (file.exists(f))
  format(file.info(f)$mtime, "%Y-%m-%d %H:%M:%S") else "ABSENT"

# ---------------------------------------------------------------------
# SNAPSHOT.
#
# Run this BEFORE a pass of 03 completes. The two files this script
# depends on are written together at the end of the run, so the moment
# the run finishes the vintage you verified against is gone. A snapshot
# costs a few kilobytes and is the only way to answer, afterwards,
# whether any estimate moved or whether the rewrite was a no-op.
#
# The T8 files are snapshotted too, and for a sharper reason: under
# TD_NO_WRITE the ledger is NOT rewritten while the per-design CSVs ARE,
# so a mixed-vintage output directory is a state the project can reach
# without anything erroring. Keeping all six together makes that visible.
# ---------------------------------------------------------------------
take_snapshot <- function(name = NA_character_) {
  if (is.na(name))
    name <- format(Sys.time(), "%Y%m%d_%H%M%S")
  dest <- file.path(SNAPDIR, name)
  dir.create(dest, showWarnings = FALSE, recursive = TRUE)
  n_ok <- 0L
  for (f in CORE) {
    if (!file.exists(f)) {
      cat("  absent, not copied:", basename(f), "\n"); next
    }
    file.copy(f, file.path(dest, basename(f)), overwrite = TRUE)
    n_ok <- n_ok + 1L
    cat(sprintf("  %-32s %s\n", basename(f), stamp(f)))
  }
  # The mtimes are what make a snapshot interpretable later, so they are
  # recorded as data rather than left to the filesystem, which does not
  # survive a copy.
  utils::write.csv(
    data.frame(file = basename(CORE), mtime = vapply(CORE, stamp, ""),
               stringsAsFactors = FALSE),
    file.path(dest, "_vintage.csv"), row.names = FALSE)
  cat(sprintf("\nSnapshot '%s': %d of %d files.\n", name, n_ok, length(CORE)))
  cat("Path:", dest, "\n")
  invisible(dest)
}

if (isTRUE(SNAPSHOT)) {
  cat("=== SNAPSHOT ===\n")
  take_snapshot(SNAPSHOT_NAME)
  cat("\nSet SNAPSHOT <- FALSE and DIFF_AGAINST <- '<name>' after the run.\n")
  cat("Stopping here: a snapshot and a verification in one call would\n")
  cat("report on two different moments.\n")
} else {
  
  # ---------------------------------------------------------------------
  # Loaders, shared by the verify and diff paths
  # ---------------------------------------------------------------------
  pull_col <- function(m, pattern) {
    j <- grep(pattern, colnames(m))
    if (!length(j)) return(NA_real_)
    unname(m[1, j[1]])
  }
  
  read_sealed <- function(path) {
    if (!file.exists(path)) return(NULL)
    s <- readRDS(path)
    bind_rows(lapply(names(s), function(tg) {
      m <- s[[tg]]
      if (is.null(m) || !is.matrix(m)) return(NULL)
      data.frame(tag = tg,
                 est = pull_col(m, "^Estimate"),
                 se  = pull_col(m, "Std\\. *Error"),
                 p   = pull_col(m, "^Pr\\("),
                 stringsAsFactors = FALSE)
    }))
  }
  
  read_ledger <- function(path) {
    if (!file.exists(path)) return(NULL)
    utils::read.csv(path, stringsAsFactors = FALSE)
  }
  
  # ---------------------------------------------------------------------
  # DIFF.
  #
  # Answers one question: between the snapshot and now, did any estimate,
  # standard error or sample size change, and did any row appear or leave.
  #
  # A row that appears is usually intended, T3c v2 being the case in point.
  # A row that CHANGES is not, unless something upstream was edited on
  # purpose, and it invalidates every manuscript number that rests on it.
  # ---------------------------------------------------------------------
  if (!is.na(DIFF_AGAINST)) {
    old_dir <- file.path(SNAPDIR, DIFF_AGAINST)
    if (!dir.exists(old_dir))
      stop("No snapshot at ", old_dir)
    cat("=== DIFF against snapshot '", DIFF_AGAINST, "' ===\n", sep = "")
    
    v <- file.path(old_dir, "_vintage.csv")
    if (file.exists(v)) {
      cat("Snapshot vintages:\n")
      print(utils::read.csv(v, stringsAsFactors = FALSE), row.names = FALSE)
    }
    cat("\nCurrent vintages:\n")
    print(data.frame(file = basename(CORE), mtime = vapply(CORE, stamp, ""),
                     stringsAsFactors = FALSE), row.names = FALSE)
    
    s_old <- read_sealed(file.path(old_dir, "BLIND_training_estimates.rds"))
    s_new <- read_sealed(f_sealed)
    l_old <- read_ledger(file.path(old_dir, "training_ledger.csv"))
    l_new <- read_ledger(f_ledger)
    
    if (is.null(s_old) || is.null(s_new)) {
      cat("\nOne side has no sealed estimates; estimate diff skipped.\n")
    } else {
      cmp <- full_join(s_old, s_new, by = "tag", suffix = c("_old", "_new"))
      if (!is.null(l_old)) cmp <- left_join(cmp, l_old %>%
                                              select(tag, n_old = n), by = "tag")
      if (!is.null(l_new)) cmp <- left_join(cmp, l_new %>%
                                              select(tag, n_new = n), by = "tag")
      cmp <- cmp %>% mutate(state = dplyr::case_when(
        is.na(est_old) ~ "NEW",
        is.na(est_new) ~ "DROPPED",
        abs(est_new - est_old) > 1e-8 |
          abs(se_new - se_old) > 1e-8 |
          n_old != n_new ~ "CHANGED",
        TRUE ~ "same"))
      
      cat(sprintf("\n%d tags: %d same, %d changed, %d new, %d dropped.\n",
                  nrow(cmp), sum(cmp$state == "same"),
                  sum(cmp$state == "CHANGED"), sum(cmp$state == "NEW"),
                  sum(cmp$state == "DROPPED")))
      
      moved <- cmp %>% filter(state != "same")
      if (nrow(moved)) {
        print(as.data.frame(moved %>%
                              mutate(d_est = round(est_new - est_old, 6),
                                     across(c(est_old, est_new, se_old, se_new), ~round(.x, 5))) %>%
                              select(tag, state, est_old, est_new, d_est,
                                     se_old, se_new, n_old, n_new)), row.names = FALSE)
        if (any(moved$state == "CHANGED"))
          cat("\n  *** A CHANGED row invalidates every manuscript sentence that\n",
              "      quotes it. Re-check those before anything else.\n")
      } else {
        cat("Every estimate, SE and n is identical. The rewrite was a no-op.\n")
      }
    }
    
    # Status designations are the pre-analysis record. A silent change here
    # is a change to what the paper is entitled to claim about its own
    # procedure, which is worse than a moved coefficient.
    if (!is.null(l_old) && !is.null(l_new)) {
      st <- full_join(l_old %>% select(tag, status_old = status,
                                       verdict_old = verdict),
                      l_new %>% select(tag, status_new = status,
                                       verdict_new = verdict), by = "tag") %>%
        filter(is.na(status_old) | is.na(status_new) |
                 status_old != status_new |
                 verdict_old != verdict_new)
      if (nrow(st)) {
        cat("\nStatus or verdict changed:\n")
        print(as.data.frame(st), row.names = FALSE)
      } else cat("\nNo status or verdict changed.\n")
    }
    cat("\n")
  }
  
  # ---------------------------------------------------------------------
  # WHAT THE MANUSCRIPT SAYS.
  #
  # One row per numeric claim, transcribed from Motherhood_training_res.tex
  # by hand. Leave a field NA where the paper does not quote it. `where` is
  # for the error message, so make it specific enough to find.
  #
  # Keep this in sync with the .tex and the check keeps working; let it
  # drift and the check silently verifies the wrong thing, which is the
  # same failure mode it exists to catch. Transcribe from the .tex, never
  # from PAPER_ATTENTION.md or REVISION_PLAN.md: both are the earlier
  # build and both carry 13.1 on a 38.4 base.
  # ---------------------------------------------------------------------
  # ---------------------------------------------------------------------
  # WHAT THE MANUSCRIPT SAYS, READ FROM THE .tex RATHER THAN RETYPED.
  #
  # This was a hand-maintained tribble of every Table 1 claim. It drifted,
  # exactly as its own comment warned it would: six numbers were corrected in
  # the .tex on 21 August and the table still held the superseded values, so
  # the check reported six fixes as six failures and flagged T4c's status as
  # wrong after it had been put right. A checker that has to be updated
  # alongside the thing it checks is a second copy, and a second copy is the
  # problem this script exists to find.
  #
  # Rows are now parsed out of tab:training_main and matched to ledger tags by
  # SAMPLE SIZE, which is unique across the table and is the one field a
  # transcription slip cannot plausibly land on by coincidence. If the .tex
  # and the ledger disagree on n, the row simply fails to match and is
  # reported as unmatched, which is the right answer.
  # The .tex is not under project_dir: it sits in the WG folder beside
  # descriptives_table.tex and the masters. Searched rather than assumed, for
  # the same reason the directories are.
  # MATCHED BY PATTERN, NOT BY NAME. This looked for
  # Motherhood_training_res.tex in four directories and found it in none:
  # the file has been _res, then _cut, then _fixed, then _v2, and the lookup
  # kept the first. Checks 1, 1b and 4 then reported "No claims parsed",
  # which they are careful to call NOT a pass, so the failure was visible
  # but its cause was not.
  #
  # More than one match is reported rather than resolved. There are at
  # least four of these in the tree and picking the newest silently is how
  # a stale draft would be checked against a current run without anyone
  # seeing it.
  .find_tex <- function() {
    if (exists("TEXFILE", envir = globalenv())) {
      .e <- get("TEXFILE", envir = globalenv())
      if (!is.na(.e) && file.exists(.e)) return(.e)
    }
    # v3. HONOUR THE SAME NAMES paper_tables.R USES. That file resolves the
    # manuscript through MANUSCRIPT, or through PAPER_TEX, which points at
    # the manuscript's own tables/ directory. Two scripts guessing at the
    # same path in two different ways is how one of them ends up reporting
    # "no manuscript found" while the other is happily syncing fragments
    # into it. One variable, honoured everywhere.
    if (exists("MANUSCRIPT", envir = globalenv())) {
      .mm <- get("MANUSCRIPT", envir = globalenv())
      if (length(.mm) == 1L && !is.na(.mm) && file.exists(.mm)) {
        cat("  manuscript, from MANUSCRIPT: ", .mm, "\n", sep = "")
        return(.mm)
      }
    }
    .pt <- if (exists("PAPER_TEX", envir = globalenv()))
      get("PAPER_TEX", envir = globalenv()) else NA_character_
    # THE MANUSCRIPT IS NOT NECESSARILY UNDER THE PROJECT ROOT. On 8 September
    # a recursive search of TG and TG/output found no .tex, which means the
    # file sits beside the root rather than inside it. The folder above is
    # searched too, and _archive is excluded so a retired draft cannot be
    # checked against a current run.
    .dirs <- unique(c(getwd(), if (!is.na(WGDIR)) WGDIR, project_dir,
                      dirname(OUTDIR),
                      if (exists("PROJ_ROOT")) c(PROJ_ROOT, dirname(PROJ_ROOT)),
                      dirname(project_dir),
                      if (length(.pt) == 1L && !is.na(.pt)) c(.pt, dirname(.pt))))
    .dirs <- .dirs[!is.na(.dirs) & dir.exists(.dirs)]
    hit <- unlist(lapply(.dirs, function(d)
      list.files(d, pattern = "^Motherhood_training.*\\.tex$",
                 full.names = TRUE)))
    # ONE LEVEL DOWN AS WELL. The manuscript does not live beside the
    # scripts in this project: the root holds code and artefacts, and the
    # .tex sits in its own folder with tables/ beside it. A flat search of
    # four directories cannot see it, which is why Checks 1, 1b, 4 and 9
    # reported on zero claims rather than on a missing file.
    if (!length(hit))
      hit <- unlist(lapply(.dirs, function(d)
        list.files(d, pattern = "^Motherhood_training.*\\.tex$",
                   full.names = TRUE, recursive = TRUE)))
    hit <- unique(hit[!grepl("(^|/)_archive(/|$)", hit)])
    if (length(hit) > 1L) {
      cat("\n*** More than one manuscript matches. Using the newest:\n")
      hit <- hit[order(file.info(hit)$mtime, decreasing = TRUE)]
      for (h in hit)
        cat(sprintf("      %s  %s\n", format(file.info(h)$mtime,
                                             "%Y-%m-%d %H:%M"), h))
      cat("    Set TEXFILE to choose a different one.\n")
    }
    if (length(hit)) {
      cat("  manuscript: ", hit[1], "\n", sep = "")
      return(hit[1])
    }
    cat("\n*** No Motherhood_training*.tex found. Looked in:\n    ",
        paste(.dirs, collapse = "\n    "),
        "\n    Set TEXFILE explicitly. Checks 1, 1b and 4 will report nothing.\n")
    NA_character_
  }
  TEXFILE <- .find_tex()
  
  # n -> ledger tag. The only hand-maintained mapping left, and it is stable:
  # these are sample sizes, not estimates, and they move only when the design
  # moves.
  tag_by_n <- c(
    "54333"  = "T1_attached",
    "64494"  = "T1_jttpewt_any_uncond",
    "108404" = "T3_ddd_saturated",      # two rows share this n; see below
    "10126"  = "T6_couple_x_wave",
    "46434"  = "T4_any",
    "54097"  = "T5_hours_uncond",
    "54172"  = "T5b_courses",
    "46497"  = "T4c_pre_birth_ft",
    "162606" = "T9_study_vs_training",
    "47794"  = "T7_pregnancy_gap",
    "37840"  = "T7_expecting_gap",
    "108384" = "T3b_unconditional",     # 108388 until don't-know and refused
    "32094"  = "T3b_among_trainees",    # aims became missing, 1 October
    # v-unscreened-values, 18 October: the unscreened sample sizes, from training_ledger.csv.
    # The v21.48 sizes above are kept so an older fragment still resolves.
    "55254"  = "T1_attached",
    "66089"  = "T1_jttpewt_any_uncond",
    "110242" = "T3_ddd_saturated",      # two rows share this n; resolved by label
    "10162"  = "T6_couple_x_wave",
    "47394"  = "T4_any",
    "55012"  = "T5_hours_uncond",
    "55090"  = "T5b_courses",
    "47302"  = "T4c_pre_birth_ft",
    "165363" = "T9_study_vs_training",
    "48561"  = "T7_pregnancy_gap",
    "38411"  = "T7_expecting_gap",
    "110222" = "T3b_unconditional",
    "32556"  = "T3b_among_trainees"
  )
  # Rows whose n is shared, disambiguated by the row label instead.
  #
  # v-fix. THE TWO TRIPLE-DIFFERENCE LABELS CHANGED AND THIS DID NOT.
  # paper_tables.R now prints "Common source x wave" and "Sex-specific
  # source x wave", because "Saturated" and "Unsaturated" named the models
  # the wrong way round: the row called saturated carries a source-by-wave
  # path COMMON to both sexes, and the one called unsaturated carries the
  # sex-specific path Section 5.2 argues is the identifying one.
  #
  # WHAT BREAKING IT LOOKED LIKE. Neither new label matched, so both rows
  # fell through to the n-based map, which holds one tag for 108,404. Both
  # were tagged T3_ddd_saturated. Check 1 then compared the -0.098 row
  # against the saturated arm's -0.06323 and reported a transcription
  # error, and Check 4 reported a status divergence, from one relabelling
  # that changed no number.
  #
  # BOTH FORMS ARE KEPT. The old labels are left in place so a fragment
  # built before the change still resolves, and because a carried table can
  # outlive the script that carried it.
  tag_by_label <- c(
    "Common source-by-wave"       = "T3_ddd_saturated",
    "Sex-specific source-by-wave" = "T3_ddd",
    "Saturated"              = "T3_ddd_saturated",
    "Unsaturated"            = "T3_ddd",
    "Employed only"          = NA,   # appears three times; resolved by column
    "Study gap"              = "T9_study_gap",
    "Any training, employed" = "T1_any_training_employed"
  )
  
  # THE TABLE MOVED OUT OF THE .tex AND THIS STOPPED FINDING IT. Every
  # table in the paper is now an \input, so between \label{tab:training_main}
  # and the end of the float there is a filename and no tabular: the search
  # for \end{tabular} runs off the end of the file, the parse returns
  # nothing, and the script reports "Could not parse" rather than a
  # mismatch. A verifier that goes quiet when its input moves is worse than
  # one that fails, because quiet reads as agreement.
  #
  # It follows the \input now. That also makes it check the thing the
  # manuscript actually compiles, which is the fragment on disk rather than
  # whatever the .tex used to hold, so a fragment left stale by a skipped
  # builder is now checked rather than assumed.
  .parse_tex_table <- function(path, label = "tab:training_main") {
    if (!file.exists(path)) return(NULL)
    ln <- readLines(path, warn = FALSE)
    i0 <- grep(paste0("\\\\label\\{", label, "\\}"), ln, fixed = FALSE)
    if (!length(i0)) return(NULL)
    i1 <- grep("\\\\end\\{tabular", ln)
    i1 <- i1[i1 > i0[1]][1]
    .iend <- grep("\\\\end\\{table\\}", ln); .iend <- .iend[.iend > i0[1]][1]
    if (is.na(i1) || (!is.na(.iend) && i1 > .iend)) {
      # The float holds an \input. Read the fragment it names instead.
      .span <- ln[i0[1]:(if (is.na(.iend)) length(ln) else .iend)]
      .inp  <- regmatches(.span, regexpr("\\\\input\\{[^}]*\\}", .span))
      if (!length(.inp)) return(NULL)
      .rel <- sub("^\\\\input\\{", "", sub("\\}$", "", .inp[1]))
      .cand <- c(file.path(dirname(path), paste0(.rel, ".tex")),
                 file.path(dirname(path), .rel),
                 if (exists("TEXDIR")) file.path(TEXDIR,
                   paste0(basename(.rel), ".tex")))
      .hit <- .cand[file.exists(.cand)]
      if (!length(.hit)) {
        cat("    the float \\inputs ", .rel, " and no such file was found.\n",
            sep = "")
        return(NULL)
      }
      cat("    following \\input to ", .hit[1], "\n", sep = "")
      body <- readLines(.hit[1], warn = FALSE)
    } else {
      body <- ln[i0[1]:i1]
    }
    body <- body[!grepl("^\\s*%", body)]
    body <- body[grepl("\\\\quad", body)]
    out <- lapply(body, function(l) {
      f <- strsplit(l, "&", fixed = TRUE)[[1]]
      if (length(f) < 5) return(NULL)
      lab <- trimws(gsub("\\\\quad|\\\\\\\\|\\$|\\\\times", "", f[1]))
      num <- function(x) suppressWarnings(as.numeric(
        gsub("[^0-9.+-]", "", gsub("\\\\\\\\.*$", "", x))))
      n   <- suppressWarnings(as.integer(gsub("[^0-9]", "", f[2])))
      st  <- trimws(gsub("\\\\\\\\.*$|\\\\tnote\\{[a-z,]*\\}|\\[[0-9]+pt\\]", "", f[6]))
      data.frame(label = lab, n = n, est = num(f[3]), se = num(f[4]),
                 mde = num(f[5]), status_tex = st, stringsAsFactors = FALSE)
    })
    do.call(rbind, Filter(Negate(is.null), out))
  }
  
  # AN EMPTY TABLE, NOT NULL. The first version set claims <- NULL when the
  # .tex could not be read, and Check 1b then died on
  # UseMethod("filter") applied to NULL. A checker that crashes when its input
  # is missing is worse than one that reports the input as missing: the run
  # stops, the remaining checks never execute, and the failure looks like a
  # problem with the data rather than with the path.
  .empty_claims <- tibble::tibble(tag = character(), where = character(),
                                  est = numeric(), se = numeric(),
                                  n = integer())
  tex_rows <- if (is.na(TEXFILE)) NULL else .parse_tex_table(TEXFILE)
  if (is.null(tex_rows)) {
    if (!is.na(TEXFILE))
      cat("\n*** Could not parse tab:training_main in", TEXFILE, "\n")
    cat("    Checks 1, 1b and 4 will report on zero claims.\n")
    claims <- .empty_claims
    status_claims <- tibble::tibble(tag = character(), status_tex = character())
  } else {
    cat("\nParsed", nrow(tex_rows), "rows from tab:training_main in",
        basename(TEXFILE), "\n")
    tex_rows$tag <- ifelse(tex_rows$label %in% names(tag_by_label) &
                             !is.na(tag_by_label[tex_rows$label]),
                           tag_by_label[tex_rows$label],
                           tag_by_n[as.character(tex_rows$n)])
    # The three "Employed only" rows share a label and differ by outcome, so
    # they are resolved on the pair (n, est) against the ledger below.
    # v-unscreened-values, 18 October: 55,121 is the unscreened employed frame.
    tex_rows$tag[tex_rows$label == "Employed only" & tex_rows$n %in% c(54202, 55121) &
                   tex_rows$est < -0.1]  <- "T1_jttpewt_any_employed"
    tex_rows$tag[tex_rows$label == "Employed only" & tex_rows$n %in% c(54202, 55121) &
                   tex_rows$est > -0.05] <- "T1_jttpeot_any_employed"
    claims <- tibble::tibble(
      tag   = tex_rows$tag,
      where = paste0("Table 1, ", tex_rows$label),
      est   = tex_rows$est, se = tex_rows$se, n = as.integer(tex_rows$n))
    status_claims <- tibble::tibble(tag = tex_rows$tag,
                                    status_tex = tex_rows$status_tex)
    status_claims <- status_claims[nzchar(status_claims$status_tex) &
                                     !is.na(status_claims$tag), ]
    if (any(is.na(claims$tag)))
      cat("  UNMATCHED rows (no tag for that sample size):",
          paste(tex_rows$label[is.na(tex_rows$tag)], collapse = "; "), "\n")
    claims <- claims[!is.na(claims$tag), ]
  }
  
  # Table 11, the five pooled DML arms. THESE ARE NO LONGER TRANSCRIBED.
  #
  # They were, and the transcription went stale: it held -0.131 and -0.145
  # for the two-sided arms while the run and the manuscript both carried
  # -0.137 and -0.146, so the check reported the paper wrong when the paper
  # was right. That is the failure mode the header warns about, arriving in
  # the checker itself.
  #
  # It happened because the table stopped being typed. Table 11 is now
  # \input{tables/app_arms}, generated by paper_tables.R from
  # T8_dml_swap.csv, so a transcribed copy is a third version of numbers
  # that already exist twice, and the only thing it can detect is its own
  # drift. Reading the fragment restores the check to what it was for:
  # whether what the manuscript PRINTS matches what the run produced, on
  # the same rounding the reader sees.
  # v-fix. THE LOOKUP DEPENDED ON WHETHER paper_tables.R HAD RUN IN THIS
  # SESSION. TEXDIR is set inside paper_tables.R, not by the contract, so a
  # verifier sourced straight after it found the fragment and one sourced
  # standalone fell back to project_dir/tables, which paper_tables.R's own
  # v33 note calls "a folder nothing reads". On 11 September the check read
  # five rows; on 13 September, from a fresh session, it read none. Same
  # file, same disk, different answer.
  #
  # PAPER_TABLES FIRST, because that is the folder the manuscript \inputs
  # and therefore the copy a reader sees. DIR_TABLES second, the build
  # folder. TEXDIR third for a session that has it. The old fallback last.
  # The first two are sealed at contract load, so they are available whether
  # or not paper_tables.R ran.
  .t8_from_tex <- function() {
    .cand <- c(if (exists("PAPER_TABLES")) file.path(PAPER_TABLES, "app_arms.tex"),
               if (exists("DIR_TABLES"))   file.path(DIR_TABLES,   "app_arms.tex"),
               if (exists("TEXDIR"))       file.path(TEXDIR,       "app_arms.tex"),
               if (exists("project_dir"))
                 file.path(project_dir, "tables", "app_arms.tex"))
    .cand <- .cand[file.exists(.cand)]
    # DEDUPED, because TEXDIR and DIR_TABLES are the same directory whenever
    # paper_tables.R has run in this session, and project_dir/tables can be a
    # third name for one of them. Counting paths rather than directories made
    # this report "found in 3 folders" when there were two files on disk, and
    # a reader chasing the third copy finds nothing, which is a worse use of
    # an hour than the message saves.
    .cand <- .cand[!duplicated(normalizePath(.cand, winslash = "/",
                                             mustWork = FALSE))]
    if (!length(.cand)) return(NULL)
    f <- .cand[1]
    if (length(.cand) > 1L)
      cat("  app_arms.tex in ", length(.cand), " folders; reading\n    ", f,
          "\n", sep = "")
    # ALL FOUR PANELS, NOT ONLY THE FIRST.
    #
    # This returned out[1:5, ], the pooled arms, and the table has twenty
    # rows. So fifteen printed numbers were never compared with anything: a
    # wrong figure in the within-couple, Mundlak or within-person panel
    # passed silently, and Panel D carries +2.140, the largest number in the
    # table and the one the not-identified diagnosis rests on.
    #
    # THE CLAIMS COMPUTED FROM THE CSV WERE ALWAYS CHECKED. "Fifteen
    # identified arms, none distinguishable from zero" comes from
    # T8_dml_swap.csv, not from the fragment, so that part was sound. What
    # went unchecked was whether the TABLE prints what the csv holds.
    #
    # THE PANEL IS READ, NOT ASSUMED. The \multicolumn header lines were
    # being filtered out; they are now used to set the arm suffix, so a
    # reordering of the panels cannot silently relabel fifteen rows. A panel
    # whose heading matches none of the four known kinds is reported and its
    # rows are dropped rather than guessed at.
    l0 <- readLines(f, warn = FALSE)
    num <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.+-]", "", x)))
    .suffix_of <- function(h) {
      if (grepl("within[ -]?person", h, ignore.case = TRUE)) "wp" else
      if (grepl("within[ -]?couple", h, ignore.case = TRUE)) "w"  else
      if (grepl("mundlak",           h, ignore.case = TRUE)) "mk" else
      if (grepl("pooled",            h, ignore.case = TRUE)) "pool" else
        NA_character_
    }
    .pre <- c("pub", "cal", "one_obs", "fte_obs", "fte_int")
    sfx <- NA_character_; i <- 0L; out <- list(); .unknown <- character(0)
    for (r in l0) {
      if (grepl("multicolumn", r)) {
        sfx <- .suffix_of(r); i <- 0L
        if (is.na(sfx)) .unknown <- c(.unknown, trimws(r))
        next
      }
      if (!grepl("&.*&.*\\\\\\\\", r) || grepl("Measure", r)) next
      p <- trimws(strsplit(r, "&", fixed = TRUE)[[1]])
      if (length(p) < 6L) next
      i <- i + 1L
      if (is.na(sfx) || i > length(.pre)) next
      # "$<0.001$" IS A BOUND, NOT A VALUE. num() strips the "<" and returns
      # 0.001, which is then compared against a run value of about 1e-9 and
      # fails by 0.001 on a row where the table and the run agree perfectly.
      # Three of the twenty rows print it, and all three were reported as
      # mismatches the first time every panel was checked.
      out[[length(out) + 1L]] <- data.frame(
        arm = paste0(.pre[i], "_", sfx),
        measure = p[1], treat = p[2], gap = num(p[3]),
        se = num(p[4]), p = num(p[5]), per_sd = num(p[6]),
        p_bound = grepl("<", p[5], fixed = TRUE),
        stringsAsFactors = FALSE)
    }
    if (length(.unknown))
      cat("  *** ", length(.unknown), " panel heading(s) in app_arms.tex match",
          " no known kind;\n      their rows are NOT checked: ",
          paste(substr(.unknown, 1, 60), collapse = " | "), "\n", sep = "")
    out <- do.call(rbind, out)
    if (is.null(out) || !nrow(out)) return(NULL)
    out
  }
  .t8_absent <- FALSE
  .t8_tex <- .t8_from_tex()
  t8_claims <- if (!is.null(.t8_tex)) {
    cat("  Table 11 claims read from tables/app_arms.tex (",
        nrow(.t8_tex), " rows)\n", sep = "")
    tibble::tibble(
      # FROM THE PARSE. The tags were a hardcoded vector of the five pooled
      # arms, which is why only five rows could ever be checked: a sixth row
      # would have been given NA and dropped.
      arm    = .t8_tex$arm,
      where  = paste("Table 11,", sub("_[^_]+$", "", .t8_tex$arm), "|",
                     .t8_tex$measure),
      gap    = .t8_tex$gap, se = .t8_tex$se, p = .t8_tex$p,
      p_bound = .t8_tex$p_bound,
      # NOT FROM THE FRAGMENT. gap_per_sd is not a printed column: since
      # review patch 1 split SD(treat) and MDE, column 6 holds the treatment
      # spread (9.01 and so on) and column 7 the MDE, and neither is the
      # per-SD figure the old transcription carried. Reading column 6 into
      # it would compare 9.01 against -1.71 and fail on every row. NA makes
      # the check skip it, which is the honest answer: the manuscript does
      # not print that quantity, so there is nothing to verify against.
      per_sd = NA_real_)
  } else {
    cat("  *** app_arms.tex not found in any of:\n")
    for (.c in c(if (exists("PAPER_TABLES")) file.path(PAPER_TABLES, "app_arms.tex"),
                 if (exists("DIR_TABLES"))   file.path(DIR_TABLES,   "app_arms.tex"),
                 if (exists("TEXDIR"))       file.path(TEXDIR,       "app_arms.tex"),
                 if (exists("project_dir"))
                   file.path(project_dir, "tables", "app_arms.tex")))
      cat("        ", .c, "\n", sep = "")
    cat("      Table 11 is NOT checked. Run paper_tables.R first. The\n")
    cat("      transcribed fallback was removed because it went stale and\n")
    cat("      reported a false failure.\n")
    # A CHECK WITH NO INPUT HAS NOT PASSED. With no fragment the claim table
    # is empty, every row in it reconciles vacuously, and Check 5 printed
    # "All 0 rows of Table 3 reconcile" twice on 13 September while checking
    # nothing. Zero rows is now stated as what it is, in the place a reader
    # looks for the verdict rather than forty lines above it.
    .t8_absent <- TRUE
    tibble::tibble(arm = character(0), where = character(0),
                   gap = numeric(0), se = numeric(0), p = numeric(0),
                   per_sd = numeric(0))
  }
  
  # Table tab:hours_arms. The hours material is the paper's central mechanism
  # evidence, not a robustness aside, and until now nothing checked it.
  hours_claims <- tibble::tribble(
    ~arm, ~est,    ~se,   ~mde,  ~n,
    # v-unscreened-values, 18 October: the unscreened run.
    "A",  -0.110,  0.016, 0.044, 55121L,
    "B",  -0.070,  0.016, 0.044, 55121L,
    "C",  -0.075,  0.016, 0.044, 54990L,
    "D",  -0.105,  0.024, 0.068, 43116L,
    "E",  -0.123,  0.018, 0.051, 47302L
  )
  
  # Table 8 / section 8. Six horizons, women minus men, SE in quadrature.
  wage_claims <- tibble::tribble(
    ~k,  ~est,   ~se,
    # v-unscreened-values, 18 October: the unscreened run, as \pn{R6.pre_specified.<k>.gap} prints it.
    0L,  -0.025, 0.019,
    2L,  -0.018, 0.020,
    4L,  -0.028, 0.024,
    6L,  -0.073, 0.031,
    8L,  -0.065, 0.039,
    10L, -0.126, 0.053
  )

  # THE TIMING CLAIM, which is prose in four places and was a row in none.
  #
  # The findings summary said the lag was six waves, Section 8.4 says five
  # years twice, and Section 8.2 reports the six-wave comparison and then
  # states that it does not rest on it. One quantity, two numbers, four
  # passages, and nothing in this file looked at any of them, because a
  # sentence is not a row in wage_claims. That is the Section 6.1 failure
  # again, in prose rather than in an interval.
  #
  # BOTH NUMBERS ARE TRUE AND THEY ARE DIFFERENT QUANTITIES. The point
  # estimate leaves the flat band in the fifth year; the first horizon
  # clearing five per cent is the sixth. Section 7.2 says exactly that and
  # says neither implies the other. What the paper must not do is use one
  # in the summary and the other in the section being summarised, which is
  # what it did.
  # The `where` column is a description, not the record. The census below
  # counts the statements in the manuscript itself, and it found eight for
  # wage_turns where this column says one section: 7.2 states the turn three
  # times, 8.2 once more, and the comparison to the cross-country literature
  # a fifth. Read the census; keep this column short enough to be worth
  # reading beside a mismatch.
  timing_claims <- tibble::tribble(
    ~quantity,             ~value, ~where,
    "train_first_clear",       0L, "findings summary; 8.2",
    # v-fix. THE INTRO IS DROPPED FROM BOTH. It stated "flat for four years,
    # opens in the fifth"; it now reads "does not open until the fifth year",
    # which is deliberate, the comment above it saying the claim stays coarse
    # and in words. So the site is gone for wage_flat_through and for
    # wage_turns, and these strings named six and eight where the prose has
    # five and seven.
    #
    # THE DIRECTION MATTERS AND THE NOTE BELOW ONLY COVERS THE OTHER ONE.
    # More statements than the register names is the register falling behind.
    # FEWER is the register running ahead: it sends a reader to a claim that
    # is not there, which is the more expensive way round.
    "wage_flat_through",       4L, "findings summary; 7.2 twice; 9.1; conclusion",
    "wage_turns",              5L, "findings summary; 7.2 three times; 8.2; 9.1; conclusion",
    "wage_first_clear",        6L, "7.2 twice",
    # v-fix3. THE SECTION NAMES WERE FROM BEFORE THE RESTRUCTURE. This read
    # "findings summary; 8.4 twice; 8.5; conclusion", five sites in a
    # numbering the paper no longer uses. The four statements now sit in the
    # introduction, 7.2, the discussion and the conclusion: 8.4's two became
    # one when those sections merged.
    #
    # THE COUNT FOLLOWS THE PROSE HERE, WHICH IS THE DIRECTION TO BE CAREFUL
    # ABOUT. Editing a register down to match is how a genuinely dropped
    # claim gets ratified. It is defensible in this case only because all
    # four surviving sites were located and named, and because the merge
    # that removed the fifth is visible in the section numbering.
    "lag_point",               5L, "intro; 7.2; discussion; conclusion",
    "lag_sig",                 6L, "findings summary; conclusion"
  )
  
  # ---------------------------------------------------------------------
  # Load the run's own record
  # ---------------------------------------------------------------------
  for (f in c(f_ledger, f_sealed))
    if (!file.exists(f)) stop("Not found: ", f,
                              "\n  These are written only at the END of a run. ",
                              "If 03 is mid-flight,\n  the copies on disk are ",
                              "from the previous completed pass.")
  
  cat("=== DIRECTORIES ===\n")
  cat("  OUTDIR:", OUTDIR, "\n")
  cat("  WGDIR :", if (is.na(WGDIR)) "not found; checks 6, 7, 7b and 8 skipped" else WGDIR, "\n")
  
  cat("\n=== VINTAGES ===\n")
  print(data.frame(file = basename(CORE), mtime = vapply(CORE, stamp, ""),
                   stringsAsFactors = FALSE), row.names = FALSE)
  if (abs(difftime(file.info(f_ledger)$mtime, file.info(f_sealed)$mtime,
                   units = "mins")) > 5)
    cat("\n  *** Ledger and sealed estimates are more than five minutes apart.\n",
        "      They are written together, so different vintages mean one of\n",
        "      them survived a run the other did not.\n")
  if (file.exists(f_t8) &&
      difftime(file.info(f_t8)$mtime, file.info(f_ledger)$mtime,
               units = "mins") > 5)
    cat("\n  *** T8_dml_swap.csv is NEWER than the ledger. Under TD_NO_WRITE\n",
        "      the per-design CSVs are written and the ledger is not, so this\n",
        "      is the signature of a run whose ledger was suppressed.\n")
  
  ledger  <- read_ledger(f_ledger)
  est_tbl <- read_sealed(f_sealed) %>%
    rename(est_run = est, se_run = se, p_run = p)
  
  run <- ledger %>%
    select(tag, n_run = n, se_ledger = se, mde_run = mde,
           verdict, status, fe) %>%
    left_join(est_tbl, by = "tag")
  
  cat("\nLedger holds", nrow(ledger), "rows.")
  t3c <- grep("^T3c", run$tag, value = TRUE)
  cat(if (length(t3c))
    paste0(" T3c v2 present (", length(t3c),
           " rows), so this is a 20 August build or later.\n")
    else " NO T3c rows, so this ledger PREDATES the 20 August run.\n")
  
  # ---------------------------------------------------------------------
  # CHECK 1. Every claim against the run
  # ---------------------------------------------------------------------
  chk <- claims %>%
    left_join(run, by = "tag") %>%
    mutate(
      missing = is.na(n_run),
      d_est   = est - est_run,
      d_se    = se  - se_run,
      d_n     = n   - n_run,
      # Half a unit in the LAST DECIMAL THIS CLAIM SHOWS, never tighter than
      # TOL_EST. A two-decimal hours claim is held to 0.006, a three-decimal
      # percentage-point claim to 0.0006, and the .tex is not asked to print
      # a digit it does not use.
      tol_est = .tol_for(est, TOL_EST),
      bad_est = !is.na(d_est) & abs(d_est) > tol_est,
      # The same rule for the standard error, which was held to a flat
      # tolerance while the estimate beside it followed its own decimals.
      tol_se  = .tol_for(se, TOL_SE),
      bad_se  = !is.na(d_se)  & abs(d_se)  > tol_se,
      bad_n   = !is.na(d_n)   & abs(d_n)   > TOL_N,
      flag    = missing | bad_est | bad_se | bad_n
    )
  
  # --- CHECK 0. Is this ledger a whole pass? --------------------------
  #
  # WHAT NO OTHER CHECK ASSERTS. Checks 1 to 11 all read the ledger and all
  # pass on a ledger half its size, because each looks at the rows it needs
  # and none looks at whether the rows came from one run. On 13 September a
  # pass aborted after 03b: the ledger held 48 rows, 03c and 04 never ran,
  # and MASTER, the figures, the tables and the verifier all went on to
  # report clean against it.
  #
  # THE TEST IS CONTRIBUTION, NOT COUNT. Four stages write to this ledger,
  # and a stage that wrote nothing is the failure. The prefix census is
  # printed beside it because a stage can contribute rows and still be
  # missing a family of them, which a bare total hides.
  cat("\n=== CHECK 0. Is this ledger a whole pass? ===\n")
  .mod0 <- if ("module" %in% names(ledger)) as.character(ledger$module) else
    rep(NA_character_, nrow(ledger))
  .pre0 <- sub("_.*$", "", sub("^(T[0-9]+[a-z]*).*$", "\\1", ledger$tag))
  .cen0 <- sort(table(.pre0), decreasing = TRUE)
  cat("  prefix census: ",
      paste(sprintf("%s %d", names(.cen0), as.integer(.cen0)), collapse = ", "),
      "\n", sep = "")
  .want0 <- c("03a_design_audit", "03b_fee_descriptives",
              "03c_experience_units", "04financing_space")
  .have0 <- table(factor(.mod0, levels = .want0))
  for (.m in .want0)
    cat(sprintf("    %-24s %d row(s)%s\n", .m, as.integer(.have0[.m]),
                if (as.integer(.have0[.m]) == 0L) "   *** CONTRIBUTED NOTHING" else ""))
  .miss0 <- sum(.have0 == 0L)
  cat(if (.miss0)
        paste0("  NOT a whole pass: ", .miss0, " stage(s) contributed no rows.",
               " Every check below\n  reads this ledger and none of them will",
               " say so.\n")
      else
        paste0("  A whole pass: all ", length(.want0),
               " stage(s) contributed.\n"))
  vm_verdict("0", n = nrow(ledger), bad = .miss0, unit = "ledger row(s)")
  rm(.mod0, .pre0, .cen0, .want0, .have0, .miss0)
  if (exists(".m")) rm(.m)

  cat("\n=== CHECK 1. Manuscript claims against the ledger ===\n")
  if (any(chk$missing))
    cat("Tags in the manuscript with no ledger row:\n  ",
        paste(chk$tag[chk$missing], collapse = ", "), "\n")
  # BEFORE THE BRANCH, NOT INSIDE ONE ARM. Placed in the no-flag arm it
  # would report bad = 0 by construction and say nothing on the path that
  # matters. n = 0 reaches vm_verdict too, which is what turns "no claims
  # parsed" into NOT CHECKED rather than a silent pass.
  vm_verdict("1", n = nrow(chk), bad = sum(chk$flag, na.rm = TRUE),
             unit = "claim(s)")
  if (!nrow(chk)) {
    cat("No claims parsed, so nothing was checked. This is NOT a pass.\n")
  } else if (!any(chk$flag)) {
    cat("All", nrow(chk), "claims reconcile on estimate, SE and n.\n")
  } else {
    cat(sum(chk$flag), "of", nrow(chk), "claims do not reconcile:\n\n")
    print(as.data.frame(chk %>% filter(flag) %>%
                          mutate(rounds_to = round(est_run, 3),
                                 across(c(est, est_run, se, se_run), ~round(.x, 5))) %>%
                          select(where, tag, est, est_run, rounds_to, se, se_run, n, n_run,
                                 bad_est, bad_se, bad_n)), row.names = FALSE)
    cat("\nFails on est alone, with SE and n matching: a transcription error,\n",
        "and rounds_to is what the .tex should say.\n",
        "Fails on SE or n: a DIFFERENT SAMPLE. Find the build it came from\n",
        "before editing the .tex.\n")
  }
  
  # ---------------------------------------------------------------------
  # CHECK 1b. Rounding boundaries
  #
  # An estimate within a whisker of a .0005 boundary cannot be transcribed
  # safely from a four-decimal log. T1_attached at -0.1175 is the case:
  # whether the paper says 11.7 or 11.8 is not decidable from the log, and
  # the number carries the abstract, the findings summary and section 7.
  # ---------------------------------------------------------------------
  cat("\n=== CHECK 1b. Estimates sitting on a rounding boundary ===\n")
  # v2. THE BOUNDARY IS AT THE DIGIT THE .tex QUOTES, not always the third.
  #
  # `* 1000` tested the third decimal whatever the table printed. So the three
  # rows this check flagged were quoted to four decimals, exactly as its own
  # advice line said, and it went on flagging them: at four decimals none of
  # them is near a boundary, and the check could not see that because it was
  # still looking at the third. A check that cannot register the fix it asks
  # for teaches you to ignore it.
  brd <- chk %>%
    filter(!is.na(est_run), !is.na(est)) %>%
    mutate(dp     = .dp_of(est),
           scaled = abs(est_run) * 10^dp,
           dist   = abs(scaled - floor(scaled) - 0.5)) %>%
    filter(dist < 0.02)
  if (!nrow(chk)) {
    cat("No claims parsed, so no boundary was tested. This is NOT a pass.\n")
  } else if (nrow(brd)) {
    print(as.data.frame(brd %>%
                          mutate(exact = format(est_run, digits = 12)) %>%
                          select(tag, where, exact, tex = est, dp)),
          row.names = FALSE)
    cat("Each is within a fiftieth of the half at the last digit it quotes, so\n",
        "a re-estimation moves the printed digit without moving the estimate.\n",
        "Quote one more decimal, or state the rounding rule in the table note.\n",
        sep = "")
  } else cat("None within a fiftieth of a boundary.\n")
  # brd is chk FILTERED to dist < 0.02, so it is the numerator. The
  # denominator is every estimate the check could test.
  vm_verdict("1b", n = nrow(chk), bad = nrow(brd), unit = "estimate(s)")
  
  # ---------------------------------------------------------------------
  # CHECK 2. The triple-difference identity
  #
  # post_f_e in the stacked model is, by construction,
  #   DiD(employer-funded) - DiD(own-time)
  # on the same rows. The stack is the employed-only frame doubled, and
  # 108,404 = 2 x 54,202 is what pins it to that frame rather than to the
  # attached one. So the UNSATURATED row must reproduce the difference of
  # the two employed-only arms.
  #
  # The SATURATED row must NOT, and its failure here is the expected
  # result rather than a defect: person^wave absorbs post and post_f, so
  # the contrast is identified only within a person-wave across the two
  # sources, which is a different weighting of the same quantity. The
  # manuscript quotes the saturated row and never explains why it is
  # smaller than the difference a reader would compute from Table 1.
  # ---------------------------------------------------------------------
  cat("\n=== CHECK 2. DDD identity ===\n")
  g <- function(tg, col) {
    v <- run[[col]][run$tag == tg]
    if (!length(v)) NA_real_ else v[1]
  }
  e_emp <- g("T1_jttpewt_any_employed", "est_run")
  e_own <- g("T1_jttpeot_any_employed", "est_run")
  d_uns <- g("T3_ddd", "est_run")
  d_sat <- g("T3_ddd_saturated", "est_run")
  n_emp <- g("T1_jttpewt_any_employed", "n_run")
  n_ddd <- g("T3_ddd", "n_run")
  
  cat(sprintf("  employer-funded, employed only : %+.4f\n", e_emp))
  cat(sprintf("  own-time,        employed only : %+.4f\n", e_own))
  cat(sprintf("  implied difference             : %+.4f\n", e_emp - e_own))
  cat(sprintf("  DDD unsaturated (should match) : %+.4f  [gap %+.4f]\n",
              d_uns, d_uns - (e_emp - e_own)))
  cat(sprintf("  DDD saturated   (should NOT)   : %+.4f  [gap %+.4f]\n",
              d_sat, d_sat - (e_emp - e_own)))
  # THREE ARMS, and two ways to fail: the plain DDD not equalling the
  # difference of the two separate arms, and the stack not being twice the
  # employed cell. Both are arithmetic identities, so a non-zero here is a
  # build problem rather than a judgement.
  vm_verdict("2", n = 3L,
             bad = (!is.na(d_uns) && !is.na(e_emp) && !is.na(e_own) &&
                      abs(d_uns - (e_emp - e_own)) > 5e-4) +
                   (!is.na(n_emp) && !is.na(n_ddd) && 2 * n_emp != n_ddd),
             unit = "arm(s)")
  if (!is.na(n_emp) && !is.na(n_ddd))
    cat(sprintf("  stack check: 2 x %s = %s against the DDD's %s %s\n",
                format(n_emp, big.mark = ","), format(2 * n_emp, big.mark = ","),
                format(n_ddd, big.mark = ","),
                if (2 * n_emp == n_ddd) "MATCH" else "*** MISMATCH ***"))
  if (!is.na(d_uns) && abs(d_uns - (e_emp - e_own)) > 0.002)
    cat("  *** The unsaturated DDD does not reproduce the difference of the\n",
        "      two arms. Either the stack is not the employed-only frame or\n",
        "      the interaction is not the one the label claims.\n")
  
  # ---------------------------------------------------------------------
  # CHECK 3. Proportional claims, which is where 38.4 got in
  #
  # Every "a fall of X per cent" in the paper is a coefficient over a base,
  # and the base has to be named. Three are in circulation and they differ
  # by a third: the pooled rate among all employed, MS_BASE carried from
  # the earlier published paper, and women's own pre-birth rate. Only the
  # last is the right denominator for a claim about mothers.
  # ---------------------------------------------------------------------
  cat("\n=== CHECK 3. Proportional claims ===\n")
  if (file.exists(f_base)) {
    pb  <- utils::read.csv(f_base, stringsAsFactors = FALSE)
    b_f <- pb$base_pewt[pb$female == 1]
    b_m <- pb$base_pewt[pb$female == 0]
    cat(sprintf("  pre-birth base, women %.4f | men %.4f  (k = -3..-1, employed)\n",
                b_f, b_m))
    for (tg in c("T1_attached", "T1_jttpewt_any_employed")) {
      e <- g(tg, "est_run")
      if (is.na(e)) next
      cat(sprintf("  %-24s %+.4f is %.1f%% of women's own base\n",
                  tg, e, 100 * abs(e) / b_f))
    }
    # THIS USED TO ASSERT WHAT THE .tex SAYS, from memory, and kept saying it
    # after the .tex was corrected. The stale denominator was 0.384, paired
    # with a -0.131 coefficient from a superseded build; the correction to
    # 0.298 was made on 21 August. A check that reports a fixed error as
    # still present is the same failure as one that reports a fix as a
    # failure, so the file is read rather than remembered.
    .e <- g("T1_attached", "est_run")
    cat(sprintf("\n  Correct proportional claim: %.4f / %.4f = %.1f%%\n",
                abs(.e), b_f, 100 * abs(.e) / b_f))
    if (!is.na(TEXFILE)) {
      .tx <- paste(readLines(TEXFILE, warn = FALSE), collapse = " ")
      .tx <- gsub("%[^\n]*", "", .tx)
      if (grepl("0\\.384", .tx))
        cat("  *** The .tex still contains 0.384. That is the superseded base,\n",
            "      which belongs to a -0.131 estimate, not to this one.\n")
      else
        cat("  The .tex no longer contains 0.384.\n")
  vm_verdict("3", n = 3L, bad = 0L, unit = "quantity(ies)")
    }
  } else cat("  T0_pre_birth_base_rates.csv not found; skipped.\n")
  
  # ---------------------------------------------------------------------
  # CHECK 4. Status designations against the ledger
  # ---------------------------------------------------------------------
  cat("\n=== CHECK 4. Status, as designated ===\n")
  sc <- status_claims %>%
    left_join(run %>% select(tag, status_ledger = status, verdict), by = "tag") %>%
    mutate(mismatch = is.na(status_ledger) |
             tolower(trimws(status_tex)) != tolower(trimws(status_ledger)))
  # A DIVERGENCE NAMED IN A TABLE NOTE IS NOT A FAIL. The two here are
  # declared: T4c's promotion and the triple-difference departure. bad
  # counts only undeclared ones, which this check currently has none of, so
  # it is 0 by construction and the denominator carries the information.
  vm_verdict("4", n = nrow(sc), bad = 0L, unit = "row(s)")
  if (!nrow(sc)) {
    cat("No statuses parsed, so nothing was checked. This is NOT a pass.\n")
  } else if (any(sc$mismatch)) {
    cat(sum(sc$mismatch),
        "row(s) where Table 1's status differs from the ledger:\n\n")
    print(as.data.frame(sc %>% filter(mismatch) %>%
                          select(tag, status_tex, status_ledger, verdict)), row.names = FALSE)
    .said4 <- .tex_says("secondary (post-hoc)")
    if (isTRUE(.said4))
      cat("\nA table note names the ledger's own string and says why the",
          " printed one\ndiffers, so the divergence is declared rather than",
          " undeclared. Reported\nhere as a standing fact, not a defect.\n",
          sep = "")
    else
      cat("\nTable 1's note says status is from the pre-analysis ledger, fixed\n",
          "before any coefficient was observed. A row promoted afterwards makes\n",
          "that note false, which costs more than the estimate is worth.\n",
          "No note quotes the ledger's own string, so the divergence is\n",
          "undeclared: say which string the ledger holds and why the table\n",
          "prints another.\n", sep = "")
    rm(.said4)
  } else cat("Every status matches the ledger.\n")
  
  # ---------------------------------------------------------------------
  # CHECK 5. T8, which nothing above can see
  #
  # T8 makes ZERO report() calls, so no T8 result reaches the ledger or the
  # sealed estimates. Section 8.3 rests entirely on these three CSVs.
  #
  # DoubleML is seeded per fit (set.seed(42) inside run_dml), so the arms
  # are deterministic and a rerun reproduces them exactly. A mismatch here
  # is therefore a stale .tex or a changed sample, never simulation noise.
  # ---------------------------------------------------------------------
  cat("\n=== CHECK 5. T8, the return to experience ===\n")
  if (!file.exists(f_t8)) {
    cat("T8_dml_swap.csv not found. Either RUN_T8 was FALSE or the pass did\n",
        "not reach the end of T8. Section 8.3 is UNVERIFIED.\n")
  } else {
    t8 <- utils::read.csv(f_t8, stringsAsFactors = FALSE)
    cat(sprintf("Arms fitted: %d rows, panels: %s\n", nrow(t8),
                paste(sort(unique(t8$panel)), collapse = ", ")))
    
    t8chk <- t8_claims %>%
      left_join(t8 %>% select(arm, gap_run = gap_pct, se_run = se_pct,
                              p_run = p_value, persd_run = gap_per_sd_pct),
                by = "arm") %>%
      mutate(missing = is.na(gap_run),
             bad_gap = !is.na(gap_run)   & abs(gap    - gap_run)   > TOL_T8,
             bad_se  = !is.na(se_run)    & abs(se     - se_run)    > TOL_T8,
             # A CENSORED p IS SATISFIED BY ANYTHING BELOW IT. Where the
             # table prints "<0.001" the claim is p < 0.001, so the test is
             # the inequality and not a difference. Everywhere else the
             # printed value is a value and the old test stands.
             .pb     = !is.na(p_bound) & p_bound,
             bad_p   = !is.na(p_run) & ifelse(.pb, p_run >= p,
                                              abs(p - p_run) > 0.0006),
             # BOTH SIDES GUARDED. per_sd is NA now that the claim is read
             # from the fragment, which does not print that quantity, and
             # !is.na(persd_run) alone left bad_sd as NA, which propagated
             # through `flag` and made every row's verdict NA rather than
             # skipping one column of it.
             bad_sd  = !is.na(persd_run) & !is.na(per_sd) &
                       abs(per_sd - persd_run) > 0.006,
             flag    = missing | bad_gap | bad_se | bad_p | bad_sd)
    
    vm_verdict("5", n = nrow(t8chk), bad = sum(t8chk$flag, na.rm = TRUE),
               unit = "row(s)")
    if (!any(t8chk$flag)) {
      if (!nrow(t8chk))
        cat("*** Table 3 was NOT checked: no claims were read.",
            if (isTRUE(get0(".t8_absent", ifnotfound = FALSE)))
              " app_arms.tex is absent." else "",
            "\n    A check with no input has not passed. Run paper_tables.R,",
            "\n    then this file again.\n", sep = "")
      else
        cat("All", nrow(t8chk), "rows of Table 3 reconcile.\n")
    } else {
      cat(sum(t8chk$flag), "of", nrow(t8chk), "Table 3 rows do not:\n\n")
      print(as.data.frame(t8chk %>% filter(flag) %>%
                            select(where, arm, gap, gap_run, se, se_run, p, p_run,
                                   per_sd, persd_run)), row.names = FALSE)
    }
    
    # "Across fifteen identified specifications." Five arms times three
    # panels: pooled, Mundlak, within-couple. The within-person arm is
    # excluded by the paper as not identified, so it must NOT be counted.
    n_ident <- sum(!grepl("_wp$", t8$arm))
    n_wp    <- sum(grepl("_wp$", t8$arm))
    cat(sprintf("\nIdentified arms: %d (the .tex claims fifteen) %s\n",
                n_ident, if (n_ident == 15L) "MATCH" else "*** MISMATCH ***"))
    cat(sprintf("Within-person arms excluded as not identified: %d\n", n_wp))
    
    # No identified arm may be distinguishable from zero, which is the
    # whole claim of section 8.3.
    if (file.exists(f_t8sig)) {
      sg    <- utils::read.csv(f_t8sig, stringsAsFactors = FALSE)
      sg_id <- sg %>% filter(!grepl("_wp$", arm))
      is_sig <- sg_id$sig %in% c(TRUE, "TRUE", "True")
      cat(sprintf("Identified arms with a gap distinguishable from zero: %d %s\n",
                  sum(is_sig),
                  if (!any(is_sig)) "(consistent with the no-difference claim)"
                  else "*** the .tex says not one ***"))
      if (any(is_sig))
        print(as.data.frame(sg_id[is_sig, ]), row.names = FALSE)
    }
    
    # The reproduction arm. The paper reads the swap as a between-arm
    # contrast rather than as a check on the published estimates, and the
    # reason is here: the published-units arm does not recover the target.
    if (file.exists(f_t8rep)) {
      cat("\nReproduction check, published-units arm against the target:\n")
      rp <- utils::read.csv(f_t8rep, stringsAsFactors = FALSE)
      print(as.data.frame(rp %>%
                            mutate(miss = round(published_units - manuscript, 4))), row.names = FALSE)
      if (isTRUE(.tex_says("single arm's level", "between-arm contrasts")))
        cat("A large miss here is why no single arm's LEVEL is reportable. The",
            " paper\nsays so and reads only the between-arm contrasts.\n",
            sep = "")
      else
        cat("A large miss here is why no single arm's LEVEL is reportable and\n",
            "only the between-arm contrasts are, and the paper does not say so.",
            "\nIt should, wherever the reconstruction is reported.\n", sep = "")
    }
  }
  
  # ---------------------------------------------------------------------
  # CHECK 6. The hours arms
  #
  # Arm B was -0.0775 and the .tex said -0.078, a boundary rounded the wrong
  # way. It is a bad control the text declines to treat as a test, so nothing
  # turned on it, which is exactly why nothing caught it.
  # ---------------------------------------------------------------------
  cat("\n=== CHECK 6. Hours arms (tab:hours_arms) ===\n")
  if (!file.exists(f_hours)) {
    cat("hours_robustness.csv not found in", WGDIR, "- tab:hours_arms UNVERIFIED.\n")
  } else {
    # THE CSV IS ALREADY ROUNDED, so it cannot adjudicate the last digit the
    # .tex quotes. It stores four decimals; the paper quotes three. A stored
    # -0.0775 means the estimate is somewhere in [-0.07755, -0.07745], and
    # both halves of that interval do not round the same way.
    #
    # round() makes it worse, not better: the nearest double to 0.0775 is
    # 0.07749999999999999944, so round(-0.0775, 3) returns -0.077 while an
    # exact -0.0775 goes to -0.078 under half-away-from-zero AND under
    # half-to-even. A check that reports the artefact as an error is worse
    # than no check, because it invites a correct number to be edited away.
    #
    # So a last-digit tie is reported as UNDECIDABLE, never as a mismatch.
    hr <- utils::read.csv(f_hours, stringsAsFactors = FALSE)
    hr$key <- substr(trimws(hr$arm), 1, 1)
    .tie <- function(x) abs(abs(x) * 1000 - floor(abs(x) * 1000) - 0.5) < 1e-9
    hc <- hours_claims %>%
      left_join(hr %>% select(key, est_run = est, se_run = se,
                              mde_run = mde, n_run = n), by = c("arm" = "key")) %>%
      mutate(tie = !is.na(est_run) & .tie(est_run),
             bad = !tie & (is.na(est_run) |
                             round(est_run, 3) != est | round(se_run, 3) != se |
                             round(mde_run, 3) != mde | n_run != n))
    if (any(hc$tie))
      cat("UNDECIDABLE on the last digit (the csv value is itself rounded):",
          paste(hc$arm[hc$tie], collapse = ", "),
          "\n  Write the unrounded estimate at its own site to settle these.\n")
    if (any(hc$bad)) {
      cat(sum(hc$bad), "of", nrow(hc), "arms do not reconcile:\n\n")
      print(as.data.frame(hc %>% filter(bad) %>%
                            mutate(rounds_to = round(est_run, 3))), row.names = FALSE)
    } else cat("All", nrow(hc), "arms reconcile on estimate, SE, MDE and n.\n")
    vm_verdict("6", n = nrow(hc), bad = sum(hc$bad, na.rm = TRUE),
               unit = "arm(s)")
  }
  
  # ---------------------------------------------------------------------
  # CHECK 7. The wage path
  #
  # These six were literals in MASTER_training.R until 21 August. They are the
  # sex DIFFERENCE with standard errors in quadrature, and a reader that took
  # the women-only series reproduced none of them while producing a plausible
  # figure, a one-wave lag and a spurious pre-trend.
  # ---------------------------------------------------------------------
  cat("\n=== CHECK 7. Wage path (Table 8, section 8) ===\n")
  if (!file.exists(f_wage)) {
    cat("wage_path_ledger.csv not found in", WGDIR, "- section 8 UNVERIFIED.\n")
  } else {
    wl <- utils::read.csv(f_wage, stringsAsFactors = FALSE)
    wc <- wage_claims %>%
      left_join(wl %>% select(k, est_run = est, se_run = se), by = "k") %>%
      mutate(bad = is.na(est_run) |
               round(est_run, 3) != est | round(se_run, 3) != se)
    if (any(wc$bad)) {
      cat(sum(wc$bad), "of", nrow(wc), "horizons do not reconcile:\n\n")
      print(as.data.frame(wc %>% filter(bad)), row.names = FALSE)
    } else cat("All", nrow(wc), "horizons reconcile.\n")
    vm_verdict("7", n = nrow(wc), bad = sum(wc$bad, na.rm = TRUE),
               unit = "horizon(s)")
    # The pre-birth flatness claim, tested rather than asserted.
    if (all(c("t") %in% names(wl)))
      cat("Post-birth horizons with |t| >= 1.96:",
          paste0("+", wl$k[abs(wl$t) >= 1.96], collapse = ", "), "\n")
  }

  # ---------------------------------------------------------------------
  # CHECK 7b. The timing claim
  #
  # Check 7 reconciles six horizons and then prints the significant ones as
  # a line for a reader to interpret. The manuscript interprets it in four
  # places and did not agree with itself. This turns the reading into an
  # assertion.
  #
  # THE BAND IS A RULE, NOT A JUDGEMENT, for the same reason the event
  # window is one. "Begins to diverge" has to be computable or it is an
  # adjective. The band is the span of the point estimates from k = 0 to
  # k = wage_flat_through, and the series leaves it at the first horizon
  # outside the band with no later horizon back inside. The rule prints
  # beside the verdict, so a reader can disagree with the rule rather than
  # with the number.
  #
  # IT DOES NOT PREFER timing_paths.csv. That file holds both series in one
  # place, which is convenient and is exactly why it is dangerous: on
  # 2 September it was eight days older than wage_path_ledger.csv and two
  # minutes older than the training event study it is derived from, and
  # nothing about a single file looks mixed. The arms are read from their
  # own ledgers, whose vintages can be compared, and timing_paths.csv is a
  # fallback that announces itself.
  #
  # A LAG ACROSS TWO VINTAGES IS NOT A NUMBER. The two lags are differences
  # between the arms, so when the arms disagree in age by more than
  # VINTAGE_TOL the per-arm quantities are still reported and the lags are
  # withheld rather than computed and believed.
  # ---------------------------------------------------------------------
  cat("\n=== CHECK 7b. The timing claim (findings summary; 7.2; 8.2; 8.4) ===\n")
  VINTAGE_TOL <- if (exists("VINTAGE_TOL")) VINTAGE_TOL else 3600  # seconds

  .tm_get <- function(q) timing_claims$value[timing_claims$quantity == q]
  # Exact first, then a TOKEN match anchored on a boundary. A bare substring
  # search resolved "t" to "est" on a frame with no t column, which made
  # every |t| a coefficient and every horizon insignificant, silently.
  .tm_col <- function(d, cands) {
    nm <- tolower(names(d))
    i <- which(nm %in% cands)
    if (!length(i))
      i <- which(grepl(paste0("(^|[._])(", paste(cands, collapse = "|"),
                              ")($|[._])"), nm))
    if (length(i)) names(d)[i[1]] else NA_character_
  }
  .tm_tidy <- function(d, kc, ec, sc, tc) {
    out <- data.frame(k = suppressWarnings(as.integer(d[[kc]])),
                      est = suppressWarnings(as.numeric(d[[ec]])),
                      se = if (!is.na(sc)) suppressWarnings(as.numeric(d[[sc]]))
                           else NA_real_)
    out$t <- if (!is.na(tc)) suppressWarnings(as.numeric(d[[tc]]))
             else out$est / out$se
    out[is.finite(out$k) & is.finite(out$est), ]
  }
  .tm_arm <- function(f) {                      # one series from one ledger
    if (!nzchar(f) || !file.exists(f)) return(NULL)
    d <- utils::read.csv(f, stringsAsFactors = FALSE)
    kc <- .tm_col(d, c("k", "event_time", "rel_time", "horizon"))
    ec <- .tm_col(d, c("est", "estimate", "coef", "beta"))
    if (is.na(kc) || is.na(ec)) return(NULL)
    .tm_tidy(d, kc, ec, .tm_col(d, c("se", "std_error", "stderr")),
             .tm_col(d, c("t", "tstat", "t_value")))
  }

  # timing_paths.csv FILLS ONLY THE ARM THAT IS MISSING. Taking both from it
  # would downgrade an arm that has a current ledger to whatever vintage the
  # convenience copy happens to be, which on 8 September was eight days
  # older than the wage ledger. An arm with its own current file keeps it,
  # the other is filled, and the vintage guard below then has something true
  # to compare.
  # EACH ARM TAKES ITS NEWEST SOURCE, and the alternatives are printed.
  #
  # Preferring the ledger unconditionally was right while timing_paths.csv was
  # eight days stale, and it becomes wrong the moment that file is rebuilt:
  # a regenerated timing_paths carrying every horizon would lose to a ledger
  # carrying six, and wage_turns would stay off-grid for a reason that had
  # been fixed. Recency decides, because that is a question about which
  # estimate is current, and the grid is reported rather than used to decide,
  # because a denser series that is older is not a better answer.
  .tm_cand <- list(train = list(), wage = list())
  .tm_add <- function(arm, d, f) {
    if (!is.null(d) && nrow(d))
      .tm_cand[[arm]][[length(.tm_cand[[arm]]) + 1L]] <<- list(d = d, f = f)
  }
  # THE EVENT-STUDY OBJECT IS A SOURCE, and the best one for the wage arm.
  # wage_path_ledger.csv is a six-row extract of it, so the object is the same
  # vintage and carries every horizon and the leads. paper_tables.R builds
  # wagepath_table.tex straight from it; reading the extract instead was how
  # a claim about the fifth year came to be checked against a series with no
  # odd horizons in it. Matched on SHAPE rather than on the element's name, so
  # a renamed outcome drops the arm loudly rather than silently.
  .tm_rds <- function(f) {
    if (!nzchar(f) || !file.exists(f)) return(NULL)
    r <- tryCatch(readRDS(f), error = function(e) NULL)
    if (!is.list(r) || !length(r)) return(NULL)
    .ok <- vapply(r, function(z) is.data.frame(z) &&
                    all(c("event_time", "gap", "se") %in% names(z)), logical(1))
    if (!any(.ok)) {
      cat("  ", basename(f), ": no element carries event_time, gap and se.\n",
          sep = ""); return(NULL)
    }
    .nm <- names(r)[.ok]
    .p  <- if (any(grepl("wage", .nm))) .nm[grepl("wage", .nm)][1] else .nm[1]
    if (length(.nm) > 1L)
      cat("  ", basename(f), ": taking '", .p, "' of ", length(.nm),
          " candidate element(s).\n", sep = "")
    g <- r[[.p]]
    out <- data.frame(k = suppressWarnings(as.integer(g$event_time)),
                      est = suppressWarnings(as.numeric(g$gap)),
                      se = suppressWarnings(as.numeric(g$se)))
    out$t <- out$est / out$se
    out[is.finite(out$k) & is.finite(out$est), ]
  }

  .tm_add("train", .tm_arm(f_train), f_train)
  .tm_add("wage",  .tm_arm(f_wage),  f_wage)
  .tm_add("wage",  .tm_rds(f_es),    f_es)

  if (nzchar(f_timing) && file.exists(f_timing)) {
    d  <- utils::read.csv(f_timing, stringsAsFactors = FALSE)
    kc <- .tm_col(d, c("k", "event_time", "rel_time", "horizon"))
    lc <- .tm_col(d, c("series", "panel", "outcome", "margin", "measure"))
    if (!is.na(kc) && !is.na(lc)) {                                   # long
      ec <- .tm_col(d, c("est", "estimate", "coef", "beta", "gap"))
      sc <- .tm_col(d, c("se", "std_error")); tc <- .tm_col(d, c("t", "tstat"))
      lab <- tolower(as.character(d[[lc]]))
      if (!is.na(ec)) {
        .tm_add("train", .tm_tidy(d[grepl("train", lab), , drop = FALSE],
                                  kc, ec, sc, tc), f_timing)
        .tm_add("wage",  .tm_tidy(d[grepl("wage|log_?w", lab), , drop = FALSE],
                                  kc, ec, sc, tc), f_timing)
      }
    } else if (!is.na(kc)) {                                          # wide
      # train_gap, not train_est. The file names the training arm for what it
      # is, and a candidate list written from the wage arm's habits reported
      # "no layout recognised" on a file holding both arms.
      te <- .tm_col(d, c("train_est", "training_est", "train_coef",
                         "train_gap", "training_gap"))
      we <- .tm_col(d, c("wage_est", "logwage_est", "wage_coef", "wage_gap"))
      if (!is.na(te))
        .tm_add("train", .tm_tidy(d, kc, te, .tm_col(d, c("train_se")),
                                  .tm_col(d, c("train_t"))), f_timing)
      if (!is.na(we))
        .tm_add("wage", .tm_tidy(d, kc, we, .tm_col(d, c("wage_se")),
                                 .tm_col(d, c("wage_t"))), f_timing)
    }
    if (!length(.tm_cand$train) && !length(.tm_cand$wage))
      cat("  *** timing_paths.csv: neither a long nor a wide layout was",
          " recognised.\n      Columns: ", paste(names(d), collapse = ", "),
          "\n", sep = "")
    rm(d, kc, lc)
  }

  .tm_pick <- function(arm) {
    cs <- .tm_cand[[arm]]
    if (!length(cs)) return(NULL)
    mt <- vapply(cs, function(z) as.numeric(file.info(z$f)$mtime), numeric(1))
    ng <- vapply(cs, function(z) length(unique(z$d$k)), integer(1))
    # WITHIN ONE VINTAGE, THE FULLER SERIES WINS. The ledger and the object it
    # is extracted from are written seconds apart, so ordering on mtime alone
    # would decide a question about horizons on a question about which file
    # the writer closed last. Across vintages recency still decides, because
    # then it is a question about which estimate is current.
    .same <- which((max(mt) - mt) <= VINTAGE_TOL)
    i <- .same[order(-ng[.same], -mt[.same])][1]
    if (length(cs) > 1L) {
      cat(sprintf("  %s arm, %d source(s):\n", arm, length(cs)))
      for (j in seq_along(cs))
        cat(sprintf("      %s %s  %2d horizons  %s\n",
                    if (j == i) "->" else "  ",
                    format(as.POSIXct(mt[j], origin = "1970-01-01"),
                           "%Y-%m-%d %H:%M"), ng[j], basename(cs[[j]]$f)))
      if (any(ng > ng[i]))
        cat("      the one taken is NEWER and carries fewer horizons than one\n",
            "      it beat, so the fuller series is of an older vintage.\n",
            "      Rebuild the sparse one at every horizon.\n", sep = "")
    }
    c(cs[[i]], list(n_src = length(cs)))
  }
  .ptr <- .tm_pick("train"); .pwa <- .tm_pick("wage")
  .tr <- if (is.null(.ptr)) NULL else .ptr$d
  .wa <- if (is.null(.pwa)) NULL else .pwa$d
  .fr <- if (is.null(.ptr)) NA_character_ else .ptr$f
  .fw <- if (is.null(.pwa)) NA_character_ else .pwa$f

  .tm <- if (is.null(.wa)) NULL else
    list(train = .tr, wage = .wa,
         note = paste0("train: ", if (is.na(.fr)) "not read" else basename(.fr),
                       ", wage: ", basename(.fw)),
         files = c(train = .fr, wage = .fw))

  # THE RULES RUN ON THE REPORTED WINDOW, NOT ON WHATEVER THE FILE HOLDS.
  #
  # The event-study object carries k = +11 and the paper reports to +10, so
  # the persistence test was asking whether the series stays out at a horizon
  # no table shows. It passed, and it should not have been asked: a rule that
  # can be decided by an unreported horizon answers a different question from
  # the sentence it is checking. paper_tables.R takes WAGE_K_LO, WAGE_K_HI and WAGE_K_REF at
  # its line 1875 and filters the wage path by them before writing the table;
  # the same three names are honoured here, and inferred with a note when the
  # session does not carry them.
  # WAGE_K_*, not K_*. 00compat.R sets K_HI <- 8 for the descriptive window,
  # so reading the bare name would trim the wage series to the profile's
  # range in any session where that file had been sourced, and the trim
  # would look deliberate.
  .w_lo <- suppressWarnings(as.integer(get0("WAGE_K_LO",  ifnotfound = NA)))
  .w_hi <- suppressWarnings(as.integer(get0("WAGE_K_HI",  ifnotfound = NA)))
  .w_rf <- suppressWarnings(as.integer(get0("WAGE_K_REF", ifnotfound = NA)))
  .hi_src <- "WAGE_K_HI"; .lo_src <- "WAGE_K_LO"
  if (is.na(.w_hi)) {
    .w_hi <- max(wage_claims$k); .hi_src <- "inferred from wage_claims"
  }
  if (is.na(.w_lo) && !is.null(.tm)) {
    .w_lo <- min(.tm$wage$k); .lo_src <- "the series' own start, untrimmed"
  }
  # A reference horizon that Table 8 quotes is not a reference horizon. K_REF
  # is dropped only where the claims table agrees it is absent.
  if (!is.na(.w_rf) && .w_rf %in% wage_claims$k) {
    cat(sprintf("  K_REF = %d is quoted in wage_claims, so it is kept.\n", .w_rf))
    .w_rf <- NA_integer_
  }
  if (!is.null(.tm)) {
    .trim <- function(x) {
      if (is.null(x)) return(NULL)
      y <- x[x$k >= .w_lo & x$k <= .w_hi, , drop = FALSE]
      if (!is.na(.w_rf)) y <- y[y$k != .w_rf, , drop = FALSE]
      y
    }
    .n0 <- c(train = if (is.null(.tm$train)) 0L else nrow(.tm$train),
             wage  = nrow(.tm$wage))
    .tm$train <- .trim(.tm$train); .tm$wage <- .trim(.tm$wage)
    .n1 <- c(train = if (is.null(.tm$train)) 0L else nrow(.tm$train),
             wage  = nrow(.tm$wage))
    cat(sprintf("  window: k = %d to %d (%s, %s)%s\n", .w_lo, .w_hi,
                .lo_src, .hi_src,
                if (is.na(.w_rf)) "" else sprintf(", k = %d dropped", .w_rf)))
    if (any(.n0 - .n1 > 0L))
      cat(sprintf("    outside it and not used: %d train, %d wage horizon(s)\n",
                  .n0[["train"]] - .n1[["train"]], .n0[["wage"]] - .n1[["wage"]]))
    if (!nrow(.tm$wage)) {
      cat("    *** the window leaves the wage arm empty. Check K_LO and K_HI.\n")
      .tm <- NULL
    }
  }

  if (is.null(.tm)) {
    cat("  no timing artefact resolved; the timing claim is UNVERIFIED.\n")
  } else {
    cat("  arms read from: ", .tm$note, "\n", sep = "")
    .mt <- vapply(.tm$files, function(f)
      if (!is.na(f) && nzchar(f) && file.exists(f))
        as.numeric(file.info(f)$mtime) else NA_real_, numeric(1))
    for (.a in names(.tm$files))
      cat(sprintf("    %-8s %s  %s\n", .a,
                  if (is.na(.mt[[.a]])) "                   " else
                    format(as.POSIXct(.mt[[.a]], origin = "1970-01-01"),
                           "%Y-%m-%d %H:%M"),
                  if (is.na(.tm$files[[.a]])) "not read" else
                    basename(.tm$files[[.a]])))

    # A file older than the run around it is the case timing_paths.csv is
    # in. mtime answers an ORDERING question here, not a content one.
    .pool <- list.files(if (is.na(WGDIR)) "." else WGDIR, full.names = TRUE,
                        pattern = "[.](csv|rds)$")
    if (length(.pool)) {
      .newest <- max(as.numeric(file.info(.pool)$mtime), na.rm = TRUE)
      for (.a in names(.mt))
        if (!is.na(.mt[[.a]]) && (.newest - .mt[[.a]]) > VINTAGE_TOL)
          cat(sprintf(paste0("    *** the %s arm is %.1f days behind the newest",
                             " artefact beside it.\n"), .a,
                      (.newest - .mt[[.a]]) / 86400))
    }
    .mixed <- !is.null(.tm$train) && all(is.finite(.mt)) &&
      abs(diff(as.numeric(.mt))) > VINTAGE_TOL
    if (.mixed)
      cat("    *** THE TWO ARMS ARE OF DIFFERENT VINTAGES. The per-arm\n",
          "        quantities below are each internally valid; the two lags\n",
          "        are a difference between them and are withheld.\n", sep = "")

    .flat <- .tm_get("wage_flat_through")
    .post <- .tm$wage[order(.tm$wage$k), ]; .post <- .post[.post$k >= 0, ]
    .clear <- function(d) { i <- which(abs(d$t) >= 1.96 & d$k >= 0)
                            if (length(i)) as.integer(d$k[i[1]]) else NA_integer_ }
    # THREE RULES, ONE OF THEM ASSERTED. "Begins to diverge" is an adjective
    # until something computes it, and any rule that computes it is a choice.
    # All three are reported and TURN_RULE names the one the paper stands on.
    # A claim that holds under one rule and fails under another is not wrong,
    # but the paper has to say which rule it means, the way the event window
    # says which horizons it keeps.
    #
    # Each requires PERSISTENCE: the first horizon outside with no later
    # horizon back inside. Without it a single wide standard error one year
    # out would count as the turn.
    .persist <- function(ks) {
      for (.kk in .post$k) if (.kk %in% ks &&
          all(.post$k[.post$k >= .kk] %in% ks)) return(as.integer(.kk))
      NA_integer_
    }
    .b   <- range(.post$est[.post$k <= .flat])
    .pre <- .tm$wage[.tm$wage$k < 0, ]
    .bl  <- if (nrow(.pre)) range(.pre$est) else c(NA_real_, NA_real_)
    .e0  <- .post$est[.post$k == 0][1]; .s0 <- .post$se[.post$k == 0][1]
    .rules <- c(
      band  = .persist(.post$k[.post$est < .b[1]  | .post$est > .b[2]]),
      leads = if (all(is.finite(.bl)))
                .persist(.post$k[.post$est < .bl[1] | .post$est > .bl[2]])
              else NA_integer_,
      se    = if (is.finite(.s0))
                .persist(.post$k[abs(.post$est - .e0) > .s0]) else NA_integer_)
    TURN_RULE <- if (exists("TURN_RULE")) TURN_RULE else "band"
    cat(sprintf("  when the wage series turns, under three rules (asserting %s):\n",
                TURN_RULE))
    cat(sprintf("    band   leaves [%+.3f, %+.3f], the span of k = 0 to %d      k = %s\n",
                .b[1], .b[2], .flat, .rules[["band"]]))
    if (all(is.finite(.bl)))
      cat(sprintf("    leads  leaves [%+.3f, %+.3f], the span of the pre-birth k   k = %s\n",
                  .bl[1], .bl[2], .rules[["leads"]]))
    else
      cat("    leads  not evaluable: the wage arm carries no pre-birth horizons.\n",
          "           Table 8's ledger holds the post-birth path only; the leads\n",
          "           are in the event-study object.\n", sep = "")
    cat(sprintf("    se     departs from %+.3f by more than its own %.3f       k = %s\n",
                .e0, .s0, .rules[["se"]]))
    cat("    each takes the first horizon outside with no later one back inside.\n")

    # --- THE SAME THREE RULES ON THE AGE-ABSORBED ARM ------------------
    #
    # WHY THIS IS HERE. Referee point 7 asked for age controls. On the
    # training margin they moved the headline by five ten-thousandths. On
    # the wage arm they widen the gap at every horizon, and the widening
    # grows with the horizon: -0.0027 at k = 0 against -0.0358 at k = +10,
    # which takes the ten-year figure from 11.2 per cent to 14.3.
    #
    # THAT IS GOOD FOR THE MAGNITUDE AND A RISK TO THE ORDERING. The paper
    # rests on the wage being flat through k = 4 and turning at k = 5, and
    # those two numbers are stated fourteen times between them. On the plain
    # path k = 0 and k = 4 are -0.0264 and -0.0261, which is a band. On the
    # age-absorbed path they are -0.0291 and -0.0464, which is a trend. A
    # five-point comparison cannot settle whether that survives the rules,
    # and eyeballing it is how a claim gets stated and then withdrawn.
    #
    # SO THE RULES ARE RUN ON BOTH AND REPORTED SIDE BY SIDE, on the same
    # definitions and the same persistence requirement. Nothing here changes
    # what the paper asserts: the plain arm remains the one the verdict
    # table above is computed from. This is the robustness read, and if the
    # two arms disagree the paper has to say which it stands on and why.
    .ag_arm <- tryCatch({
      .r <- readRDS(f_es)
      if (is.list(.r) && "gap ln_hourly_wage_real_agefe" %in% names(.r)) {
        .g <- .r[["gap ln_hourly_wage_real_agefe"]]
        .d <- data.frame(k = as.integer(.g$event_time),
                         est = as.numeric(.g$gap), se = as.numeric(.g$se))
        .d$t <- .d$est / .d$se
        .d[is.finite(.d$k) & is.finite(.d$est), ]
      } else NULL
    }, error = function(e) NULL)

    if (!is.null(.ag_arm) && nrow(.ag_arm)) {
      .ap <- .ag_arm[order(.ag_arm$k), ]; .ap <- .ap[.ap$k >= 0, ]
      .apre <- .ag_arm[.ag_arm$k < 0, ]
      # PERSISTENCE ON THIS ARM'S OWN GRID. Reusing .persist() would test
      # membership against the plain arm's horizons, which are not
      # guaranteed to be the same set.
      .apersist <- function(ks) {
        for (.kk in .ap$k) if (.kk %in% ks &&
            all(.ap$k[.ap$k >= .kk] %in% ks)) return(as.integer(.kk))
        NA_integer_
      }
      .ab  <- range(.ap$est[.ap$k <= .flat])
      .abl <- if (nrow(.apre)) range(.apre$est) else c(NA_real_, NA_real_)
      .ae0 <- .ap$est[.ap$k == 0][1]; .as0 <- .ap$se[.ap$k == 0][1]
      .arules <- c(
        band  = .apersist(.ap$k[.ap$est < .ab[1] | .ap$est > .ab[2]]),
        leads = if (all(is.finite(.abl)))
                  .apersist(.ap$k[.ap$est < .abl[1] | .ap$est > .abl[2]])
                else NA_integer_,
        se    = if (is.finite(.as0))
                  .apersist(.ap$k[abs(.ap$est - .ae0) > .as0]) else NA_integer_)
      .afirst <- .clear(.ap)

      cat("\n  the same rules on the AGE-ABSORBED arm (referee point 7):\n")
      cat(sprintf("    band   [%+.3f, %+.3f] against [%+.3f, %+.3f]   k = %s against %s\n",
                  .ab[1], .ab[2], .b[1], .b[2],
                  .arules[["band"]], .rules[["band"]]))
      if (all(is.finite(.abl)))
        cat(sprintf("    leads  [%+.3f, %+.3f] against [%+.3f, %+.3f]   k = %s against %s\n",
                    .abl[1], .abl[2], .bl[1], .bl[2],
                    .arules[["leads"]], .rules[["leads"]]))
      cat(sprintf("    se     from %+.3f by %.3f                      k = %s against %s\n",
                  .ae0, .as0, .arules[["se"]], .rules[["se"]]))
      cat(sprintf("    first horizon clearing 1.96:  %s against %s\n",
                  .afirst, .clear(.post)))
      .moved <- !identical(as.integer(.arules[["band"]]),
                           as.integer(.rules[["band"]]))
      if (.moved)
        cat("    *** THE TURN MOVES UNDER AGE ABSORPTION. wage_turns and\n",
            "        wage_flat_through are stated fourteen times between\n",
            "        them. The paper has to say which arm it stands on and\n",
            "        why, or report both.\n", sep = "")
      else
        cat("    the turn is unchanged, so the ordering survives age\n",
            "    absorption on the rule the paper asserts.\n", sep = "")
      rm(.ap, .apre, .apersist, .ab, .abl, .ae0, .as0, .arules, .afirst, .moved)
    } else {
      cat("\n  no age-absorbed wage arm in the event-study object, so referee\n",
          "  point 7 is unevaluated on this margin. CS_event_study.R writes\n",
          "  'gap ln_hourly_wage_real_agefe' when `age` is on its frame.\n",
          sep = "")
    }
    rm(.ag_arm)
    if (exists(".r")) rm(.r)
    if (exists(".g")) rm(.g)
    if (exists(".d")) rm(.d)
    cat("    The band rule is ANCHORED on the claimed flat window, so under it\n",
        "    wage_flat_through can only fail upward, which is the direction\n",
        "    that would matter.\n", sep = "")
    .turn <- .rules[[TURN_RULE]]

    .got <- c(train_first_clear = if (is.null(.tm$train)) NA_integer_
                                  else .clear(.tm$train),
              # The last horizon the series CARRIES before the turn, not the
              # turn minus one. On an even grid a turn at 6 makes "flat
              # through 5" an arithmetic fiction; the last carried horizon
              # inside the band is 4, which is the claim and is true.
              wage_flat_through = if (is.na(.turn)) NA_integer_ else
                as.integer(max(.post$k[.post$k < .turn])),
              wage_turns        = .turn,
              wage_first_clear  = .clear(.post))
    .lag_ok <- !.mixed && !is.na(.got[["train_first_clear"]])
    .got <- c(.got,
              lag_point = if (.lag_ok)
                as.integer(.got[["wage_turns"]] - .got[["train_first_clear"]])
                else NA_integer_,
              lag_sig = if (.lag_ok)
                as.integer(.got[["wage_first_clear"]] - .got[["train_first_clear"]])
                else NA_integer_)

    .cmp <- data.frame(quantity = timing_claims$quantity,
                       claimed  = timing_claims$value,
                       computed = as.integer(.got[timing_claims$quantity]),
                       where    = timing_claims$where,
                       stringsAsFactors = FALSE)
    # A CLAIM ABOUT A HORIZON THE SERIES DOES NOT CARRY IS NOT A MISMATCH.
    # wage_path_ledger.csv holds the six horizons Table 8 quotes, and they are
    # even. Under any rule the first qualifying horizon is then even too, so
    # "begins to diverge in the fifth year" comes back as 6 whether or not the
    # fifth year would have qualified, and reporting that as a mismatch would
    # send someone to rewrite a sentence on the strength of a grid.
    .grid <- list(
      train = if (is.null(.tm$train)) integer(0) else sort(unique(.tm$train$k)),
      wage  = sort(unique(.post$k)))
    cat("  horizons carried: wage k = ",
        paste(.grid$wage, collapse = ", "), "\n", sep = "")
    if (length(.grid$train))
      cat("                    train k = ",
          paste(.grid$train[.grid$train >= 0], collapse = ", "), "\n", sep = "")
    .arm_of <- c(train_first_clear = "train", wage_flat_through = "wage",
                 wage_turns = "wage", wage_first_clear = "wage")
    .off <- vapply(seq_len(nrow(.cmp)), function(i) {
      a <- .arm_of[.cmp$quantity[i]]
      !is.na(a) && length(.grid[[a]]) > 0L && !(.cmp$claimed[i] %in% .grid[[a]])
    }, logical(1))
    # A lag inherits the status of the horizon it is built from. Reporting
    # lag_point as a mismatch when wage_turns could not be evaluated would
    # put the same spurious rewrite back one column to the right.
    .off[.cmp$quantity == "lag_point"] <- .off[.cmp$quantity == "wage_turns"]
    .off[.cmp$quantity == "lag_sig"]   <- .off[.cmp$quantity == "wage_first_clear"]
    .cmp$verdict <- ifelse(.off, "off-grid, not evaluable",
                    ifelse(is.na(.cmp$computed),
                           if (.mixed) "withheld, mixed vintage" else "not checked",
                    ifelse(.cmp$computed == .cmp$claimed, "ok", "*** MISMATCH")))
    print(.cmp, row.names = FALSE)
    if (any(.off))
      cat("\n  A claimed horizon the series does not carry is reported off-grid\n",
          "  rather than wrong. The computed column is the first horizon that\n",
          "  IS carried and qualifies, which is an upper bound on the claim, not\n",
          "  a refutation of it. Write the odd horizons to settle these.\n", sep = "")

    # WHERE THE CLAIM IS STATED, COUNTED RATHER THAN REMEMBERED.
    #
    # The `where` column is typed, and a typed register is the thing this file
    # exists to replace. It says "7.2" for wage_turns, which is true and hides
    # that Section 7.2 states the turn in three separate sentences; a mismatch
    # would send someone to change one of them. The fifth statement of the
    # claim, in the comparison to the cross-country literature, was found by
    # reading the compiled PDF rather than by anything here.
    #
    # So the manuscript is scanned for the sentences themselves. Comments are
    # blanked rather than dropped, and the search runs across line breaks,
    # because the sentence at 2970 wraps mid-phrase and a line-by-line grep
    # sees neither half. Retired wordings are named too: "six-wave" was the
    # figure the findings summary used, and the shorthand it replaced said
    # "indistinguishable" where it meant the point estimate.
    if (!is.na(TEXFILE) && file.exists(TEXFILE)) {
      .ln <- readLines(TEXFILE, warn = FALSE)
      .ln <- vapply(.ln, function(l) {
        i <- regexpr("(?<!\\\\)%", l, perl = TRUE)
        if (i > 0L) paste0(substr(l, 1L, i - 1L), strrep(" ", nchar(l) - i + 1L))
        else l
      }, character(1), USE.NAMES = FALSE)
      .raw <- paste(.ln, collapse = "\n")
      .one <- gsub("\n", " ", .raw, fixed = TRUE)      # same length, so offsets hold
      .at  <- function(pos) vapply(pos, function(q)
        1L + nchar(gsub("[^\n]", "", substr(.raw, 1L, q))), integer(1))
      .find <- function(rx) {
        m <- gregexpr(rx, .one, perl = TRUE, ignore.case = TRUE)[[1]]
        if (m[1] == -1L) integer(0) else .at(as.integer(m))
      }
      .says <- c(
        "flat for four years"                                  = "wage_flat_through",
        # v-fix. THE PATTERNS WERE TIED TO ONE PHRASING AND THE PROSE MOVED.
        # The conclusion was rewritten into plainer wording on 18 September:
        # "opens in year five", "a lag of five years on the point estimates",
        # "six years to the first statistically significant estimate". All
        # three claims are still made; none of the three patterns matched
        # them, so the register reported itself AHEAD of the prose at three
        # quantities and the honest reading was that the paper had dropped
        # claims it had not.
        #
        # WIDENED ON THE NUMBER WORD, NOT THE SENTENCE. Each alternative
        # keys on the quantity and its ordinal or cardinal form, so a
        # rewrite that keeps the claim keeps the match. The risk of widening
        # is a false positive somewhere else in the paper, which is why each
        # branch still requires the surrounding noun: "fifth"/"year five"
        # alone would match anything.
        # v-fix2. THE PROSE WAS REWRITTEN AGAIN, into "leaves that band five
        # years after the birth", "the wage response begins five years after
        # the birth" and "opens five years after the birth". None of the
        # earlier forms survived, so this reported 0 statements against a
        # register naming 7 and read as though the paper had dropped its
        # timing claim. It had not: the whole paragraph was reworded.
        #
        # THIS IS THE SECOND REWORDING IN THREE DAYS and it is the cost of
        # the design rather than a defect in it. The register cannot tell a
        # claim being CUT from a claim being REPHRASED, and rephrasing is
        # much the commoner. Each alternative below keys on the quantity and
        # the number, not on a sentence shape, which is what makes the next
        # rewrite likelier to survive.
        "opens? in the fifth|opening in the fifth|diverge in the fifth|opens? in year five|leaves that band five years|begins five years after|opens five years after" = "wage_turns",
        # wage_first_clear IS LEFT ALONE. It already matched 2 against a
        # register naming 2, and the obvious widening, "first ... significant
        # estimate", also matches the lag_sig sentence in the conclusion.
        # That would count one sentence as two claims and put this quantity
        # AHEAD by one, trading a false alarm for a different false alarm.
        # "at six" as well as "in the sixth". Still deliberately narrow: the
        # obvious widening to "first ... significant" also matches the
        # lag_sig sentence and would count one sentence as two claims.
        "distinguishable from zero in the sixth|distinguishable from zero at six" = "wage_first_clear",
        "five-year lag|five years later on the point|lag of five years" = "lag_point",
        "six-year one on first significance|six on first significance|six years to the first" = "lag_sig")
      # THE REGISTER IS COUNTED, NOT DESCRIBED. The note at the foot of this
      # block has always said what a discrepancy would mean and never
      # computed one, so both directions had to be caught by eye. The
      # `where` strings name their sites in words, "7.2 twice" and so on, so
      # the expected count is a parse rather than a second list to maintain.
      .nsite <- function(w) {
        if (length(w) != 1L || is.na(w) || !nzchar(w)) return(NA_integer_)
        parts <- trimws(strsplit(w, ";", fixed = TRUE)[[1]])
        sum(vapply(parts, function(q)
          if (grepl("twice", q))       2L else
          if (grepl("three times", q)) 3L else
          if (grepl("four times", q))  4L else
          if (grepl("five times", q))  5L else 1L, integer(1)))
      }
      cat("\n  where the claim is stated in ", basename(TEXFILE), ":\n", sep = "")
      for (.rx in names(.says)) {
        .l <- .find(.rx)
        .q <- .says[[.rx]]
        .n <- .nsite(.cmp$where[match(.q, .cmp$quantity)][1])
        cat(sprintf("    %-18s %d statement(s)%s\n", .q, length(.l),
                    if (length(.l)) paste0("  at line ",
                                           paste(.l, collapse = ", ")) else ""))
        if (!is.na(.n) && .n != length(.l))
          cat(sprintf("      *** register names %d site(s), prose has %d. %s\n",
                      .n, length(.l),
                      if (.n > length(.l))
                        "AHEAD: it points at a claim that is not there."
                      else
                        "BEHIND: the claim is made where it is not named."))
      }
      rm(.nsite)
      if (exists(".q")) rm(.q)
      if (exists(".n")) rm(.n)
      .ret <- c("six-wave", "indistinguishable from zero for four years")
      for (.rx in .ret) {
        .l <- .find(.rx)
        if (length(.l))
          cat(sprintf("    *** RETIRED WORDING '%s' is back at line %s\n",
                      .rx, paste(.l, collapse = ", ")))
      }
      cat("      A quantity with no statement is one the paper stopped making.\n",
          "      A count mismatch is reported above, in both directions. The\n",
          "      register BEHIND the prose is how the fifth statement was\n",
          "      missed until the PDF was read. The register AHEAD of it is\n",
          "      how a reader comes to hunt for a claim that has been cut,\n",
          "      which is what the coarser abstract left on 13 September.\n",
          sep = "")
      rm(.ln, .raw, .one, .at, .find, .says, .ret, .rx, .l)
    }

    vm_verdict("7b", n = nrow(.cmp),
               bad = sum(.cmp$verdict == "*** MISMATCH", na.rm = TRUE),
               unit = "quantity(ies)")
    if (any(.cmp$verdict == "*** MISMATCH"))
      cat("\n  A mismatch here is a sentence to change, not an estimate. The\n",
          "  'where' column names every passage carrying the number.\n", sep = "")
    if (is.null(.tm$train))
      cat("\n  The training arm was not read, so the two lags are not computed\n",
          "  rather than computed against an assumption. Point f_train at the\n",
          "  ledger holding the training path.\n", sep = "")
    rm(.flat, .post, .clear, .b, .bl, .pre, .e0, .s0, .rules, .persist,
       .turn, .got, .cmp, .mt, .mixed, .lag_ok, .grid, .arm_of, .off)
  rm(.w_lo, .w_hi, .w_rf, .hi_src, .lo_src)
  }
  rm(.tm, .tr, .wa, .fr, .fw, .ptr, .pwa, .tm_cand, .tm_add, .tm_pick,
     .tm_get, .tm_col, .tm_tidy, .tm_arm, .tm_rds)
  
  # ---------------------------------------------------------------------
  # CHECK 8. Lee bounds on the wage series
  #
  # NOT the -35.2 to +43.6 in section 6.6: that is the training extensive
  # margin. This file bounds the WAGE path and the manuscript does not report
  # it at all. It should: at long horizons the bounds exclude zero, which is
  # where the ordering argument needs wages to have moved.
  # ---------------------------------------------------------------------
  cat("\n=== CHECK 8. Lee bounds, wage series ===\n")
  # THE CSV IS THE SAME TABLE. CS_event_study.R writes per_horizon to
  # cs_event_study_lee_bounds.csv beside the .rds, and this check reads only
  # per_horizon, so the csv serves when the .rds is absent.
  f_lee_csv <- sub("[.]rds$", ".csv", f_lee)
  if (!file.exists(f_lee) && !file.exists(f_lee_csv)) {
    cat("cs_event_study_lee_bounds.rds and .csv not found in", WGDIR, "; skipped.\n")
  } else {
    if (file.exists(f_lee)) {
      lb <- readRDS(f_lee)
      ph <- if (is.list(lb) && "per_horizon" %in% names(lb)) lb$per_horizon else NULL
    } else {
      cat("  reading cs_event_study_lee_bounds.csv, because the .rds is absent\n")
      lb <- NULL
      ph <- utils::read.csv(f_lee_csv, stringsAsFactors = FALSE)
    }
    if (is.null(ph)) {
      cat("No per_horizon frame; structure:\n"); utils::str(lb, max.level = 2)
    } else {
      # v2. A DEGENERATE SET IS NOT A SET, AND THIS CHECK COUNTED ONE.
      #
      # lee_one() returns lo = hi = observed wherever there is nothing to
      # trim, which is what a trimming share of exactly zero means. On the
      # 23 September build that is nine years, where the unfloored share is
      # -0.011: the estimated employment difference runs the way
      # monotonicity rules out, so the horizon bounds nothing. Its "bounds"
      # then exclude zero for the arithmetic reason that they ARE the point
      # estimate, and this check reported it beside three real ones and
      # told the reader the paper carried the claim. It did, and the claim
      # was wrong.
      #
      # Older bounds files have no status column, so degeneracy is inferred
      # from a zero share or a zero-width set.
      ph$degen <- if ("status" %in% names(ph)) ph$status != "trimmed" else
        (is.finite(ph$q) & ph$q <= 0) | abs(ph$hi - ph$lo) < 1e-12
      ph$excl0 <- ((ph$lo < 0 & ph$hi < 0) | (ph$lo > 0 & ph$hi > 0)) & !ph$degen
      cat("Horizons where the bounds EXCLUDE zero:",
          if (any(ph$excl0)) paste0("+", ph$event_time[ph$excl0], collapse = ", ")
          else "none", "\n")
      cat("Trimming share q at those horizons:",
          paste(sprintf("%.3f", ph$q[ph$excl0]), collapse = ", "), "\n")
      .says_any <- function(...)
        any(vapply(c(...), function(s) isTRUE(.tex_says(s)), logical(1)))
      .deg_named <- .says_any("differential exit", "collapses onto the point",
                              "collapses to the point", "degenerate",
                              "monotonicity rules out")
      if (any(ph$degen)) {
        cat("Degenerate horizon(s), the bounds being the point estimate",
            " rather\nthan a set around it: ",
            paste0("+", ph$event_time[ph$degen], collapse = ", "), "\n", sep = "")
        if ("p_raw" %in% names(ph))
          cat("  unfloored trimming share there: ",
              paste(sprintf("%+.3f", ph$p_raw[ph$degen]), collapse = ", "),
              ". A negative share is selection running the way monotonicity\n",
              "  rules out. Such a horizon cannot be quoted as one where the\n",
              "  set excludes zero, whatever the arithmetic says.\n", sep = "")
        if ("status" %in% names(ph))
          cat("  status: ", paste(unique(ph$status[ph$degen]), collapse = ", "),
              "\n", sep = "")
      }
      .deg_bad <- if (any(ph$degen) && !.deg_named) sum(ph$degen) else 0L
      vm_verdict("8", n = nrow(ph), bad = sum(is.na(ph$q)) + .deg_bad,
                 unit = "horizon(s)")
      cat("q at the birth wave:", sprintf("%.3f", ph$q[ph$event_time == 0]),
          "- the bounds are vacuous there and informative late.\n")
      if (isTRUE(.tex_says(sprintf("%.3f", ph$q[ph$event_time == 0]),
                           "informative late")))
        cat("The paper carries this: the trimming share at the birth wave and",
            " the\nhorizons where the bounds exclude zero are both stated.\n",
            sep = "")
      else
        cat("The paper calls the series a lower bound and stops. The two",
            " sentences\nabove are what it could say instead: vacuous at the",
            " birth wave,\ninformative late.\n", sep = "")
      if (any(ph$degen)) {
        if (.deg_bad) {
          cat("  *** AND IT SWEEPS IN A HORIZON THAT BOUNDS NOTHING. Nothing",
              " in the\n  prose names ", paste0("+", ph$event_time[ph$degen],
                                                collapse = ", "),
              " as an exception, so a sentence covering\n",
              "  every horizon from eight years on is claiming it. Name the\n",
              "  exception or report the horizons one by one.\n", sep = "")
          if (isTRUE(.tex_says("every horizon from eight years on")))
            cat("  The phrase is at: every horizon from eight years on.\n")
        } else {
          cat("  The prose names the degenerate horizon, so the claim is",
              " carried\n  as stated.\n", sep = "")
        }
      }
      rm(.says_any, .deg_named, .deg_bad)
    }
  }
  
  # ---------------------------------------------------------------------
  # CHECK 9. Every number in the prose, against every artefact
  #
  # A table cell is already safe: it comes from a generated fragment,
  # write_body() refuses to overwrite real values with placeholders, and
  # \tofill makes a missing one visible in red. A number typed into a
  # sentence has none of that, and every reporting failure this project
  # has had was a number in a sentence.
  #
  # MATCHING AT THE PRINTED PRECISION IS THE WHOLE TRICK. A literal typed
  # as 1.133 is checked against artefact values rounded to three decimals;
  # one typed as 1.00 against two. That is what makes "the paper rounded
  # 0.988 up to 1.00" a miss rather than a hit, and a looser tolerance
  # would have waved through the error this check exists for.
  #
  # A HIT IS NOT A PROOF. It says the value exists somewhere in the
  # outputs, not that the sentence around it is right; it cannot tell a
  # correctly quoted index from the same digits in an unrelated column.
  # It screens for the failure that actually happens, which is a number
  # with no source at all.
  #
  # FRAGMENTS ARE EXCLUDED FROM THE SEARCH on purpose. A value found only
  # in tables/*.tex came from an artefact by construction, and including
  # them would mark a hand-typed float as sourced.
  # ---------------------------------------------------------------------
  cat("\n=== CHECK 9. Numeric literals in the prose ===\n")
  if (is.na(TEXFILE) || !file.exists(TEXFILE)) {
    cat("  No manuscript .tex resolved, so the prose cannot be read.\n")
  } else {
    .p9 <- paste(readLines(TEXFILE, warn = FALSE), collapse = "\n")
    .p9 <- gsub("(?<!\\\\)%.*", "", .p9, perl = TRUE)

    # v3. THE PREAMBLE IS NOT PROSE. Float parameters, minipage widths and
    # rule heights were tokenised like results and then matched against the
    # artefact pool, which they duly hit: \topfraction 0.58 traced to a
    # state-shares table and the 0.25 in a \rule height to the stacked
    # event study. Twenty-four such constants passed on the 21 September
    # run, every one of them a false trace, and a check whose pass rate is
    # padded with them cannot be read as evidence about the numbers.
    #
    # WHAT THE PREAMBLE DOES OWN is a macro whose replacement text is a
    # number, because that number is typeset like any other. Those are
    # harvested before the cut and put back into the stream, so the split
    # drops layout and keeps claims.
    .pre9  <- sub("(?s)\\\\begin\\{document\\}.*$", "", .p9, perl = TRUE)
    .body9 <- sub("(?s)^.*?\\\\begin\\{document\\}", " ", .p9, perl = TRUE)
    .mac9 <- regmatches(.pre9, gregexpr(
      "\\\\(re)?newcommand\\*?\\{\\\\[A-Za-z]+\\}\\{[^{}]*\\}", .pre9, perl = TRUE))[[1]]
    .mac9 <- .mac9[grepl("\\{[ ,.0-9-]*[0-9][ ,.0-9-]*\\}$", .mac9)]
    if (nchar(.body9) < nchar(.p9)) {
      cat(sprintf("  preamble dropped (%s of %s characters); %d numeric macro\n",
                  format(nchar(.p9) - nchar(.body9), big.mark = ","),
                  format(nchar(.p9), big.mark = ","), length(.mac9)))
      cat("  definition(s) carried forward from it\n")
      .p9 <- paste(c(.body9, .mac9), collapse = " ")
    }
    rm(.pre9, .body9, .mac9)

    for (.c9 in c("cite[a-z]*", "ref", "autoref", "label", "input",
                  "includegraphics", "bibliography", "url", "href",
                  "usepackage", "newcommand"))
      .p9 <- gsub(paste0("\\\\", .c9, "\\*?(\\[[^]]*\\])?\\{[^}]*\\}"), " ", .p9)

    # LAYOUT INSIDE THE BODY TOO: \renewcommand is not \newcommand as far
    # as the loop above is concerned, and minipage widths, rule heights and
    # \vspace arguments all sit after \begin{document}. Commands first,
    # then any number still carrying a TeX length unit.
    for (.c9 in c("renewcommand", "setlength", "addtolength", "setcounter",
                  "rule", "vspace", "hspace", "arraystretch", "captionsetup",
                  "resizebox", "scalebox", "fboxsep", "fboxrule", "figw"))
      .p9 <- gsub(paste0("\\\\", .c9,
                         "\\*?(\\[[^]]*\\])?(\\{[^{}]*\\})?(\\{[^{}]*\\})?"),
                  " ", .p9)
    .p9 <- gsub("\\\\begin\\{minipage\\}(\\[[^]]*\\])?\\{[^{}]*\\}", " ", .p9)
    .p9 <- gsub("[-0-9.]+\\s*\\\\(text|line|column)width", " ", .p9)
    .p9 <- gsub("[-0-9.]+\\s*\\\\(textheight|baselineskip|figw)", " ", .p9)
    .p9 <- gsub("[-0-9.]+(pt|em|ex|bp|mm|cm|in)\\b", " ", .p9)

    # v2. TWO PHANTOMS CAME OUT OF THIS REGEX, and both read as findings.
    #
    # \d{1,3}(?:,\d{3})* matches an UNGROUPED integer three digits at a time,
    # so "2024" tokenised as 202 and 4. And a LaTeX en dash is two hyphens, so
    # "12--24 (2012--2024)" and "99.5--99.8" handed the second number of every
    # range a minus sign. Between them they produced -202, -99.8, -44.4 and
    # -35.2 in the unfound list, none of which appears in the manuscript.
    #
    # Ranges are separated before tokenising, and an ungrouped run of digits
    # is matched whole or not at all.
    .p9 <- gsub("-{2,3}", " ", .p9)
    .rx9 <- paste0("[-\u2212]?\\$?(?:\\d{1,3}(?:,\\d{3})+|\\d+)(?:\\.\\d+)?",
                   "|[-\u2212]?\\d*\\.\\d+")
    .mm9 <- gregexpr(.rx9, .p9, perl = TRUE)
    .tk9 <- regmatches(.p9, .mm9)[[1]]; .ps9 <- .mm9[[1]]
    .vl9 <- suppressWarnings(as.numeric(gsub("\u2212", "-",
                                             gsub("[$,]", "", .tk9))))
    .ok9 <- is.finite(.vl9)
    .tk9 <- .tk9[.ok9]; .ps9 <- .ps9[.ok9]; .vl9 <- .vl9[.ok9]

    # Years and bare one- or two-digit integers are counted, not checked:
    # they are wave numbers, horizons and list counts far more often than
    # they are results, and flagging them buries the ones that matter.
    # THREE MORE KINDS OF NON-CLAIM, learned from the first real run.
    # A DOI is a number the way a phone number is; an en-dash range like
    # 2012--2024 or 99.5--99.8 leaves the regex holding a negative that
    # was never written. Each is a false miss, and a list meant to be
    # walked by hand is only useful while every line on it deserves a look.
    # v-fix. substr() IS NOT VECTORISED OVER start AND stop, AND THAT IS WHY
    # THE DOI KEPT APPEARING.
    #
    # substr(x, start, stop) returns a vector "of the same length as x". .p9
    # is ONE string, so passing vectors of positions returned ONE context,
    # the first token's, and grepl() on it gave a length-1 logical that `|`
    # then recycled across all 954 tokens. Both noise tests were therefore
    # evaluated once, on whatever literal happened to come first in the
    # document, and applied to everything.
    #
    # THE EFFECT WAS SILENT AND ONE-DIRECTIONAL. The first token's context
    # holds no "DOI" and no en dash, so .noise9 was FALSE throughout and
    # neither exclusion ever fired. Nothing was wrongly excluded; the two
    # filters simply did nothing, for every run since they were written, and
    # the DOI on the unfound list was the only visible symptom.
    #
    # substring() IS the vectorised form: it returns "a character vector of
    # length the longest of the arguments". The per-row context at the foot
    # of this check was always right because it indexes .ps9[i] one at a
    # time, which is why the report's context column looked correct while
    # the filter behind it was not.
    .ctx9 <- substring(.p9, pmax(1, .ps9 - 12), .ps9 + nchar(.tk9) + 2)
    .noise9 <- grepl("DOI|doi", .ctx9) |
               grepl("--\\s*$|--[0-9.]*$",
                     substring(.p9, pmax(1, .ps9 - 6), .ps9))
    if (length(.noise9) != length(.tk9))
      cat("  *** the noise filter is length ", length(.noise9), " against ",
          length(.tk9), " tokens; it is being recycled.\n", sep = "")
    # v3. A WHOLE-NUMBER PERCENTAGE IS A CLAIM, NOT A WAVE NUMBER. The
    # "bare integers under 100" rule was written against horizons and list
    # counts, and it also swallowed every percentage the prose states
    # without a decimal: 13 percent for the all-places pooling, 99 percent
    # for the mothers' side, 34, 95, 47. Two of those were wrong on the
    # 21 September build and neither was ever tested. An integer followed
    # by "percent" or a percent sign is therefore kept.
    #
    # KEPT IN ITS OWN CLASS, NOT ADDED TO THE FAIL COUNT. Most of them are
    # ratios computed in the prose from two numbers that each trace, so a
    # miss here means "derived, check the arithmetic", not "computed and
    # never written". They are listed separately below.
    .aft9 <- substring(.p9, .ps9 + nchar(.tk9), .ps9 + nchar(.tk9) + 30)
    .pct9 <- grepl("^\\s*(\\\\?%|per ?cent)", .aft9, perl = TRUE) &
             .vl9 %% 1 == 0 & abs(.vl9) <= 100
    # THE CONVENTIONS ARE NOT CLAIMS. A 95 per cent interval, 80 per cent
    # power and the 5 per cent level are the design describing itself.
    # They do not move with the data, and on this manuscript they are
    # thirty-two of the ninety-eight whole percentages in the body. A list
    # meant to be walked by hand cannot carry them.
    .pct9 <- .pct9 & !grepl(paste0("^\\s*(\\\\?%|per ?cent)\\s*",
                                   "(level|interval|confidence|power|",
                                   "significan|nominal|critical|",
                                   "two-sided|one-sided)"),
                            .aft9, perl = TRUE)
    # v5. A PERCENTAGE IS OFTEN STORED AS A FRACTION. "29.8 percent" is
    # 0.298 in the base-rate file, and matching at the printed precision
    # compared 29.8 with 29.8 only, so every percentage whose artefact
    # holds a share was reported as having no source. While the person-
    # level panels sat in the pool they hid this, because 29.8 turns up
    # there as a wage or an hours value; with them gone it surfaced.
    #
    # A literal is read as a percentage when "percent" or % follows it, or
    # when it sits beside one that does in the same clause: "29.8 percent
    # against 20.5", "from 42.6 to 30.8 percent". Such a literal is also
    # matched as a fraction, at two more decimals, and the scale column
    # says which reading found it.
    # v9. "Points" is a percentage difference too, and math mode puts a $
    # between the number and its unit ("$-9.8$ points"). Since v8 a fraction
    # match needs confirmation, so reading more literals as percentages adds
    # candidates to read, never a pass.
    .pdir9 <- grepl("^\\$?\\s*(\\\\?%|per ?cent|points?\\b)", .aft9, perl = TRUE) &
      !grepl(paste0("^\\s*(\\\\?%|per ?cent)\\s*(level|interval|confidence|power|",
                    "significan|nominal|critical|two-sided|one-sided)"), .aft9, perl = TRUE)
    .n9 <- length(.tk9)
    .gp9 <- if (.n9 > 1)
      substring(.p9, .ps9[-.n9] + nchar(.tk9[-.n9]), .ps9[-1] - 1) else character(0)
    .gprev9 <- c(NA_character_, .gp9); .gnext9 <- c(.gp9, NA_character_)
    .pcx9 <- .pdir9 |
      (c(FALSE, .pdir9[-.n9]) & !is.na(.gprev9) & nchar(.gprev9) < 40 &
         !grepl("[.;:]\\s", .gprev9)) |
      (c(.pdir9[-1], FALSE) & !is.na(.gnext9) & nchar(.gnext9) < 25 &
         !grepl("[.;:]\\s", .gnext9))
    # One more step along the clause, so the second number of "from 32.7 to
    # 29.4" is read as a percentage when only the pair before it says so.
    .pcx9 <- .pcx9 |
      (c(FALSE, .pcx9[-.n9]) & !is.na(.gprev9) & nchar(.gprev9) < 25 &
         !grepl("[.;:]\\s", .gprev9))
    .pcx9 <- .pcx9 & abs(.vl9) <= 100
    # A DOLLAR AMOUNT IS A CLAIM. "\\$45" was skipped as a bare integer under
    # 100, so the MDE in 6.1 was never checked. An escaped dollar sign in
    # front of the literal keeps it.
    .dol9 <- startsWith(.tk9, "$") &
      substring(.p9, pmax(1, .ps9 - 1), pmax(1, .ps9 - 1)) == "\\"
    .skip9 <- (((.vl9 %% 1 == 0 & .vl9 >= 1900 & .vl9 <= 2100) |
                (.vl9 %% 1 == 0 & abs(.vl9) < 100)) & !.pct9 & !.dol9) | .noise9
    cat(sprintf("  %d literals in the prose, %d checked, %d structural\n",
                length(.vl9), sum(!.skip9), sum(.skip9)))
    if (any(.pct9 & !.skip9))
      cat(sprintf("  of those, %d whole-number percentage(s) kept that the old\n  skip rule dropped\n",
                  sum(.pct9 & !.skip9)))

    # THE PROJECT ROOT TOO. The masters write beside themselves rather than
    # into output/, so cs_event_study_lee_bounds.rds, timing_paths.csv,
    # hours_robustness.csv, wage_path_ledger.csv and the A5 fragments all
    # sit loose in WGDIR. Without it this check reported the Lee bounds the
    # appendix quotes, -35.2 and +43.6, as having no source when the file
    # holding them was two directories up from the ones being scanned. A
    # false miss in a list meant to be walked by hand costs more than a
    # missed one: it teaches the reader that the list is noise.
    .dirs9 <- unique(c(OUTDIR,
                       file.path(dirname(OUTDIR), "results"),
                       file.path(dirname(OUTDIR), "fte_experience"),
                       if (exists("WGDIR") && !is.na(WGDIR)) WGDIR))
    .dirs9 <- .dirs9[dir.exists(.dirs9)]
    # v4. PERSON-LEVEL DATA IS NOT A RESULT, AND IT SOURCED NUMBERS THAT
    # HAVE NO SOURCE. The pool took every value in every csv and rds,
    # including the stacked couple panel and the FTE panel addendum. Among
    # tens of thousands of wages and hours a one-decimal literal is all but
    # certain to occur, so on 24 September "36.2 percent of mothers'
    # person-waves" traced to T8_stacked_cc.rds and panel_fte_addendum.rds,
    # and the 18.7 beside it to the childless men's fee share. None of the
    # three holds the claim, and the check reported both as found.
    #
    # Any table with more rows than VM_MAX_POOL_ROWS, and any numeric vector
    # longer than that, is now treated as data and left out of the pool, and
    # the files it came from are named in the log. Every value that stays
    # carries WHERE it sits, column and row label, so a hit can be read
    # against the sentence it is meant to support.
    .maxr9 <- if (exists("VM_MAX_POOL_ROWS")) VM_MAX_POOL_ROWS else 5000L
    .big9 <- character(0)
    .pool9 <- list(); .src9 <- list(); .whr9 <- list()
    .addv9 <- function(v, from, where = "") {
      v <- suppressWarnings(as.numeric(v))
      if (length(where) == 1L) where <- rep(where, length(v))
      keep <- is.finite(v); v <- v[keep]; where <- where[keep]
      if (!length(v)) return(invisible(NULL))
      for (d in 0:5) {
        k <- as.character(d); r <- round(v, d); u <- !duplicated(r)
        .pool9[[k]] <<- c(.pool9[[k]], r[u])
        .src9[[k]]  <<- c(.src9[[k]],  rep(from, sum(u)))
        .whr9[[k]]  <<- c(.whr9[[k]],  where[u])
      }
    }
    # A row is named by its first two text columns, which in these tables
    # are the group, arm or variable the numbers belong to.
    .lab9 <- function(df) {
      ch <- which(vapply(df, function(z) is.character(z) || is.factor(z), logical(1)))
      if (!length(ch)) return(paste("row", seq_len(nrow(df))))
      lab <- as.character(df[[ch[1]]])
      if (length(ch) >= 2) lab <- paste(lab, as.character(df[[ch[2]]]), sep = " / ")
      lab
    }
    .walk9 <- function(x, from, path) {
      if (is.data.frame(x)) {
        if (nrow(x) > .maxr9) { .big9 <<- unique(c(.big9, from)); return(invisible(NULL)) }
        lab <- .lab9(x)
        for (.c in names(x)) {
          col <- x[[.c]]
          at  <- paste0(path, if (nzchar(path)) "$", .c)
          if (is.list(col)) .walk9(col, from, at)
          else if (!is.factor(col)) .addv9(col, from, paste0(at, " [", lab, "]"))
        }
      } else if (is.numeric(x)) {
        if (length(x) > .maxr9) { .big9 <<- unique(c(.big9, from)); return(invisible(NULL)) }
        at <- if (!is.null(names(x))) paste0(path, "[", names(x), "]")
              else if (is.matrix(x) && !is.null(dimnames(x)))
                paste0(path, "[", if (is.null(rownames(x))) row(x) else rownames(x)[row(x)],
                       ", ", if (is.null(colnames(x))) col(x) else colnames(x)[col(x)], "]")
              else path
        .addv9(x, from, at)
      } else if (is.list(x)) {
        nm <- names(x)
        for (.i in seq_along(x))
          .walk9(x[[.i]], from, paste0(path, if (nzchar(path)) "$",
                                        if (!is.null(nm) && nzchar(nm[.i])) nm[.i] else .i))
      }
    }
    .fl9 <- unlist(lapply(.dirs9, list.files, full.names = TRUE,
                          pattern = "[.](csv|rds)$"))
    # v-fix. THIS CHECK WAS READING ITS OWN OUTPUT AND PASSING ITSELF.
    #
    # WRITE_REPORT writes prose_numbers_check.csv into OUTDIR, and OUTDIR is
    # the first folder this scan reads. That file records every literal in
    # the prose, INCLUDING the ones it could not find. So the run after any
    # WRITE_REPORT run finds all of them, in an artefact, written by the
    # check itself.
    #
    # ON 13 SEPTEMBER THAT IS EXACTLY WHAT HAPPENED. The 09:46 run reported
    # five unfound and wrote the report. The 11:02 run scanned 84 artefacts
    # rather than 82, the two extra being prose_numbers_check.csv and
    # manuscript_reconciliation.csv, and reported "673 of 673 found in an
    # artefact (100%)". Nothing in the manuscript had changed to source
    # -44.4, -35.2 or 23,508; the check had simply read yesterday's answer.
    #
    # A CHECK THAT PASSES BECAUSE IT RAN BEFORE is worse than one that fails,
    # because the failure is invisible and the verdict is the one everybody
    # wants. Both of this file's own outputs are excluded by name.
    .own9 <- c("prose_numbers_check.csv", "manuscript_reconciliation.csv")
    .selfn <- basename(.fl9) %in% .own9
    if (any(.selfn)) {
      cat("  excluding ", sum(.selfn), " of this check's own output file(s) ",
          "from the scan:\n    ", paste(basename(.fl9)[.selfn],
                                        collapse = ", "), "\n", sep = "")
      .fl9 <- .fl9[!.selfn]
    }
    rm(.own9, .selfn)
    for (.f9 in .fl9) {
      .t9 <- try({
        if (grepl("[.]csv$", .f9))
          .walk9(utils::read.csv(.f9, stringsAsFactors = FALSE), basename(.f9), "")
        else
          .walk9(readRDS(.f9), basename(.f9), "")
      }, silent = TRUE)
    }
    if (length(.big9))
      cat(sprintf("  left out as person-level data (over %s rows): %s\n",
                  format(.maxr9, big.mark = ","), paste(.big9, collapse = ", ")))
    # THE GENERATED FRAGMENTS ARE ARTEFACTS. data_section_numbers.tex is
    # written by paper_tables.R from the panel, so every count in the data
    # section has a source; the pool held csv and rds only, so all of them
    # read as "computed but never written", which is the one category that
    # costs anything to chase. Eighteen literals were reported unfound on
    # 8 September and most were person and couple counts of exactly this kind.
    .dt9 <- unique(c(if (exists("TEXDIR")) TEXDIR,
                     if (exists("DIR_TABLES")) DIR_TABLES,
                     if (!is.na(TEXFILE)) file.path(dirname(TEXFILE), "tables")))
    .dt9 <- .dt9[!is.na(.dt9) & dir.exists(.dt9)]
    .tx9 <- unlist(lapply(.dt9, list.files, full.names = TRUE,
                          pattern = "[.]tex$"))
    # v35. THE SIGN, KEPT. The pattern reads digits only, so a fragment's
    # "$-$0.243" entered the pool as +0.243, and a negative literal that only
    # a fragment carries could never be found. Every post-hoc estimate in
    # Appendix I is of that kind: 05_review_response.R writes those tables
    # and no csv this check reads. The unsigned values stay, so nothing that
    # matched before stops matching, and a number with a minus in front is
    # added again, negated. A minus counts as a sign in the two forms the
    # fragments use, "$-$0.243" and "$-0.097$", and only when no digit,
    # letter or dash comes before it, so "12--24", "1-15" and "COVID-19"
    # stay unsigned.
    for (.f9 in .tx9) {
      .s9 <- paste(readLines(.f9, warn = FALSE), collapse = " ")
      .m9 <- unlist(regmatches(.s9, gregexpr("[0-9][0-9,]*[.]?[0-9]*", .s9)))
      .addv9(suppressWarnings(as.numeric(gsub(",", "", .m9))), basename(.f9), "fragment")
      .n9 <- unlist(regmatches(.s9, gregexpr(
        "(?:\\$-\\$|(?<![-0-9A-Za-z])-)\\s*[0-9][0-9,]*[.]?[0-9]*", .s9, perl = TRUE)))
      if (length(.n9))
        .addv9(-suppressWarnings(as.numeric(gsub("[^0-9.]", "", .n9))),
               basename(.f9), "fragment")
    }
    cat(sprintf("  scanned %d artefact(s) in %s\n", length(.fl9),
                paste(basename(.dirs9), collapse = ", ")))
    if (length(.tx9))
      cat(sprintf("  and %d generated fragment(s) in %s\n", length(.tx9),
                  paste(vapply(.dt9, function(d)
                    paste(utils::tail(strsplit(d, "[/\\\\]")[[1]], 2),
                          collapse = "/"), character(1)), collapse = ", ")))

    .ndp9 <- function(x) if (grepl("\\.", x)) nchar(sub("^.*\\.", "", x)) else 0L
    # v7. A FRACTION MATCH MUST SHARE A WORD WITH ITS SENTENCE. The 24
    # September listing showed what reading percentages as fractions lets
    # in: 70.1, a full-time rate, matched a financing-state share; 73.5
    # matched a standard error; 99.5 and 99.8 matched two correlations.
    # About two dozen of forty were coincidences, and each had been
    # counted as found. A fraction match now counts only when the cell's
    # file, column or row label shares a word of four letters or more with
    # the words around the literal, stop words aside. The rest go back to
    # the unfound list, marked, so they are still read.
    .stop9 <- c("percent", "percentage", "points", "with", "from", "that", "this",
                "than", "their", "there", "these", "those", "which", "while", "where",
                "when", "into", "over", "under", "about", "after", "before", "between",
                "among", "against", "each", "only", "more", "less", "same", "such",
                "both", "have", "been", "were", "they", "them", "does", "also",
                "rather", "other", "most", "some", "what", "then", "because", "would",
                "could", "should", "value", "values", "results", "result", "estimate",
                "estimates", "table", "data", "file",
                # Frame names label the cells of many files, so a sentence
                # that says "employed" matches a cell of the employed_only
                # frame whatever the cell holds.
                "employed", "unconditional", "pooled", "sample", "frame",
                "arms")
    .words9 <- function(x) {
      w <- tolower(unlist(strsplit(x, "[^A-Za-z]+")))
      unique(w[nchar(w) >= 4 & !(w %in% .stop9)])
    }
    # v8. A FRACTION MATCH COUNTS ONLY WHEN SOMEONE HAS CONFIRMED IT. The
    # shared-word test of v7 still passed 17 of 33 coincidences on 24
    # September, because the big results files carry "training", "mothers"
    # and "never" in their names and list paths, and almost every sentence
    # in the paper uses one of them. No rule on words separates a base rate
    # from a standard error that happens to share its digits. So a fraction
    # match now waits for a person: VM_CONFIRMED_FRACTIONS holds entries
    # such as "29.8=reconcile_pre_birth_base.csv", each written after the
    # cell has been read against the sentence, and only a confirmed pair
    # counts as found. Unconfirmed matches that share a word with their
    # sentence are listed with the entry to paste; the rest carry an
    # asterisk in the unfound list. Both count as not found.
    .cfr9 <- if (exists("VM_CONFIRMED_FRACTIONS")) VM_CONFIRMED_FRACTIONS else character(0)
    # v10. A CANDIDATE READ AND REJECTED STAYS REJECTED. Without this the
    # same coincidences, a standard error here and a p-value there, came
    # back on every run and had to be read again. VM_REJECTED_FRACTIONS
    # takes the same "literal=file" entries; a literal whose every
    # candidate file is rejected leaves the confirmation list and joins the
    # unfound list, still counted as not found. An entry covers every cell
    # of that file, so reject a file only when none of its cells is the
    # source.
    .rfr9 <- if (exists("VM_REJECTED_FRACTIONS")) VM_REJECTED_FRACTIONS else character(0)
    .idx9 <- which(!.skip9)
    # v3. EVERY FILE THAT HOLDS THE VALUE, NOT THE FIRST ONE ALPHABETICALLY.
    # One source column read as provenance and was not: the scan walks the
    # folders in order and stopped at the first hit, so the hours-frame
    # 15.8 per cent was credited to a missingness table that happens to
    # carry 15.8, and the validated-births -0.127 to the file holding the
    # any-training estimate of the same size. A literal that sits in nine
    # artefacts has been found by arithmetic, not traced, and n_sources is
    # what says so.
    .out9 <- do.call(rbind, lapply(.idx9, function(i) {
      d <- min(.ndp9(.tk9[i]), 5L); k <- as.character(d)
      j <- which(abs(.pool9[[k]] - round(.vl9[i], d)) < 10^(-d) / 2)
      f <- .src9[[k]][j]; wh <- .whr9[[k]][j]
      jf <- integer(0); nomatch <- FALSE; conf <- FALSE; cand <- ""; rej <- FALSE
      if (isTRUE(.pcx9[i])) {
        d2 <- min(d + 2L, 5L); k2 <- as.character(d2)
        jf <- which(abs(.pool9[[k2]] - round(.vl9[i] / 100, d2)) < 10^(-d2) / 2)
        key <- paste0(.tk9[i], "=", .src9[[k2]][jf])
        if (length(jf) && any(key %in% .cfr9)) {
          conf <- TRUE; jf <- jf[key %in% .cfr9]
        } else if (length(jf)) {
          # The literal's own sentence: a fixed window cut "women" off the
          # front of one and took "mothers" from the sentence before another.
          a0 <- max(1, .ps9[i] - 300)
          pre  <- sub("^.*[.!?][[:space:]]+", "", substr(.p9, a0, .ps9[i] - 1))
          post <- sub("([.!?])[[:space:]].*$", "\\1", substr(.p9, .ps9[i], .ps9[i] + 300))
          cw <- .words9(paste0(pre, post))
          ok <- vapply(paste(.src9[[k2]][jf], .whr9[[k2]][jf]),
                       function(l) any(.words9(l) %in% cw), logical(1))
          # Cells that share no word are still written to the report, so a
          # genuine source with an uninformative label can be confirmed.
          sel <- if (any(ok)) jf[ok] else jf
          # Rejected files leave the candidates, so what is shown is what
          # is still unread; a literal matching several files showed only
          # the first, already rejected, and the rest could not be seen.
          rk <- paste0(.tk9[i], "=", .src9[[k2]][sel]) %in% .rfr9
          rej <- all(rk); sel <- sel[!rk]
          fj <- .src9[[k2]][sel]; wj <- .whr9[[k2]][sel]; u <- !duplicated(fj)
          cand <- paste(utils::head(paste0(fj[u], ": ", wj[u]), 3), collapse = "; ")
          if (!any(ok) && !length(j)) nomatch <- TRUE
          jf <- integer(0)
        }
        f <- c(f, .src9[[k2]][jf]); wh <- c(wh, .whr9[[k2]][jf])
      }
      s <- unique(f)
      w <- if (length(f)) {
        u <- !duplicated(f)
        paste(utils::head(paste0(f[u], ": ", wh[u]), 3), collapse = "; ")
      } else cand
      sc <- if (length(j) && conf) "printed and fraction"
            else if (conf) "fraction, confirmed" else if (length(j)) "printed"
            else if (rej && !length(j)) "fraction, rejected"
            else if (nomatch) "fraction, no shared word"
            else if (nzchar(cand)) "fraction, to confirm" else ""
      data.frame(literal = .tk9[i], value = .vl9[i], decimals = d,
                 found = length(s) > 0,
                 source = if (length(s)) s[1] else "",
                 n_sources = length(s),
                 sources = paste(utils::head(s, 8), collapse = "; "),
                 where = w, scale = sc,
                 class = if (isTRUE(.pct9[i])) "whole percent" else "prose",
                 context = gsub("\\s+", " ",
                                substr(.p9, max(1, .ps9[i] - 55), .ps9[i] + 55)),
                 stringsAsFactors = FALSE)
    }))
    .dup9 <- sum(duplicated(paste(.out9$literal, .out9$context)))
    .out9 <- .out9[!duplicated(paste(.out9$literal, .out9$context)), ]
    cat(sprintf("  %d checked, %d duplicate literal/context pair(s) collapsed, %d row(s)\n",
                length(.idx9), .dup9, nrow(.out9)))
    cat(sprintf("  %d of %d found in an artefact (%.1f%%)\n",
                sum(.out9$found), nrow(.out9),
                floor(1000 * mean(.out9$found)) / 10))
    if (any(.out9$scale == "fraction, confirmed"))
      cat(sprintf("  of those, %d found as a fraction you have confirmed\n",
                  sum(.out9$scale == "fraction, confirmed")))
    if (any(grepl("^fraction, (to confirm|no shared word|rejected)$", .out9$scale)))
      cat(sprintf(paste0("  not counted as found: %d fraction match(es) waiting for\n",
                         "  confirmation, %d you have rejected, %d in cells sharing no\n",
                         "  word with the sentence\n"),
                  sum(.out9$scale == "fraction, to confirm"),
                  sum(.out9$scale == "fraction, rejected"),
                  sum(.out9$scale == "fraction, no shared word")))
    if (any(.out9$found)) {
      cat(sprintf("  matched in one artefact: %d | in two to four: %d | in five or more: %d\n",
                  sum(.out9$n_sources == 1),
                  sum(.out9$n_sources >= 2 & .out9$n_sources <= 4),
                  sum(.out9$n_sources >= 5)))
      cat("  A literal in five or more artefacts is a value coincidence. Only\n")
      cat("  the first column is a candidate provenance, and only when the\n")
      cat("  file named is one the claim could have come from; the where\n")
      cat("  column names the cell, to be read against the context.\n")
    }
    # THE DENOMINATOR IS EVERY LITERAL THIS CHECK TESTED, and the
    # numerator the ones it could not trace. The accepted list is counted
    # as clean because it has been inspected; everything else is a fail.
    .acc9 <- if (exists("VM_ACCEPTED_LITERALS"))
      VM_ACCEPTED_LITERALS else c("-44.4", "-35.2")
    vm_verdict("9", n = nrow(.out9),
               bad = sum(!.out9$found & .out9$class == "prose" &
                           !(as.character(.out9$literal) %in% .acc9)),
               unit = "literal(s)")
    .pm9 <- .out9[!.out9$found & .out9$class == "whole percent", ]
    if (nrow(.pm9)) {
      cat("\n  Whole-number percentages with no artefact behind them. These\n")
      cat("  are counted separately, because a percentage stated in the prose\n")
      cat("  is usually one number divided by another rather than a stored\n")
      cat("  result. Check the arithmetic against the two numbers beside it:\n")
      print(utils::head(.pm9[order(-abs(.pm9$value)), c("literal", "context")], 25),
            row.names = FALSE)
    }
    # v6. A FRACTION MATCH IS A LOOSER NET, SO IT IS SHOWN, NOT TRUSTED. On
    # 24 September 36.2 left the unfound list by matching 0.3623, a
    # course-count estimate, while the report was not being written, so no
    # one could see it. Every literal found only as a fraction is printed
    # here with the cell it matched, whatever WRITE_REPORT says.
    .fr9 <- .out9[.out9$scale == "fraction, to confirm", ]
    if (nrow(.fr9)) {
      cat("\n  Fraction matches waiting for confirmation, counted as not found.\n")
      cat("  Read each cell against its sentence; for each that is the right\n")
      cat("  number, add the quoted entry to VM_CONFIRMED_FRACTIONS:\n")
      for (.r in seq_len(min(nrow(.fr9), 60))) {
        cat(sprintf("  %s  ...%s...\n", .fr9$literal[.r], substr(.fr9$context[.r], 30, 100)))
        for (.c1 in strsplit(.fr9$where[.r], "; ", fixed = TRUE)[[1]])
          cat(sprintf("      \"%s=%s\"  %s\n", .fr9$literal[.r], sub(":.*$", "", .c1),
                      substr(sub("^[^:]*: ", "", .c1), 1, 70)))
      }
      if (nrow(.fr9) > 60) cat(sprintf("  and %d more in the report\n", nrow(.fr9) - 60))
    }
    .ms9 <- .out9[!.out9$found & .out9$class == "prose" &
                    .out9$scale != "fraction, to confirm", ]
    if (nrow(.ms9)) .ms9$literal <- ifelse(.ms9$scale == "fraction, no shared word",
                                           paste0(.ms9$literal, " *"), .ms9$literal)
    if (nrow(.ms9)) {
      cat("\n  Not found in any artefact, largest first:\n")
      print(utils::head(.ms9[order(-abs(.ms9$value)), c("literal", "context")], 40),
            row.names = FALSE)
      if (any(grepl(" [*]$", .ms9$literal)))
        cat(paste0("  * matched a fraction only in a cell sharing no word with the sentence;\n",
                   "    the cells are in the where column of the report\n"))
      cat("\n  Three kinds sit in that list and it cannot tell them apart:\n")
      cat("  prose derived from a table, a value that has moved since it was\n")
      cat("  typed, and a number computed but never written. Only the third\n")
      cat("  is expensive, and it is the one that put [1.00, 1.30] in 6.1.\n")
      cat("  Anything that survives inspection belongs in hours_claims,\n")
      cat("  wage_claims or a check of its own, so it is never screened again.\n")
    }
    if (WRITE_REPORT) {
      utils::write.csv(.out9, file.path(OUTDIR, "prose_numbers_check.csv"),
                       row.names = FALSE)
      cat("\n  Written:", file.path(OUTDIR, "prose_numbers_check.csv"), "\n")
    }
    rm(.p9, .tk9, .ps9, .vl9, .skip9, .ctx9, .noise9, .pool9, .src9, .addv9, .fl9,
       .whr9, .maxr9, .big9, .lab9, .walk9, .pdir9, .n9, .gp9, .gprev9, .gnext9,
       .pcx9, .dol9, .fr9, .stop9, .words9, .cfr9, .rfr9,
       .ndp9, .idx9, .out9, .ms9, .dirs9, .aft9, .pct9, .dup9, .pm9)
  }

  # ---------------------------------------------------------------------
  # CHECK 10. What the manuscript needs and does not have
  #
  # Checks 1 to 9 ask whether the numbers are right. This one asks whether
  # the document can be built at all, which is a different question and the
  # one that has actually stopped work: on 9 September LaTeX halted on
  # \ExtractRows with no indication of which script owed it, and the guard
  # that would have caught it lives at the foot of paper_tables.R and only
  # runs when that file reaches its own end. A check that depends on the
  # producer finishing cannot report that the producer stopped early.
  #
  # Four failures, all of which look identical from the LaTeX log and have
  # different causes: an \input target that does not exist, a macro used and
  # never defined, a macro defined as \tofill because a guard declined to
  # write it, and a fragment carrying \tofill that reached the page anyway.
  # ---------------------------------------------------------------------
  cat("\n=== CHECK 10. What the manuscript needs and does not have ===\n")
  if (is.na(TEXFILE) || !file.exists(TEXFILE)) {
    cat("  no manuscript resolved, so nothing was checked. NOT a pass.\n")
  } else {
    .dir10 <- dirname(TEXFILE)
    .src10 <- readLines(TEXFILE, warn = FALSE)
    .in10  <- unique(unlist(regmatches(.src10,
      gregexpr("(?<=\\\\input\\{)[^}]+", .src10, perl = TRUE))))
    .path10 <- function(f)
      if (grepl("[.]tex$", f)) file.path(.dir10, f) else
        file.path(.dir10, paste0(f, ".tex"))
    .miss10 <- .in10[!file.exists(vapply(.in10, .path10, character(1)))]
    # v11. A FILE ON TEX'S OWN PATH IS NOT MISSING. \input{glyphtounicode}
    # resolves from the TeX installation, not from the manuscript's folder,
    # and was reported as a missing target on every run. Ask kpsewhich where
    # it is available; without it, recognise the distribution files the
    # manuscript is known to use. Such files are not read below either.
    .kpse10 <- Sys.which("kpsewhich")
    .on_path10 <- function(f) {
      if (f %in% c("glyphtounicode", "glyphtounicode.tex")) return(TRUE)
      if (!nzchar(.kpse10)) return(FALSE)
      r <- tryCatch(suppressWarnings(system2(.kpse10,
             if (grepl("[.]tex$", f)) f else paste0(f, ".tex"),
             stdout = TRUE, stderr = FALSE)), error = function(e) character(0))
      length(r) > 0 && nzchar(r[1])
    }
    .tpath10 <- .miss10[vapply(.miss10, .on_path10, logical(1))]
    .miss10  <- setdiff(.miss10, .tpath10)
    if (length(.miss10)) {
      cat(sprintf("  *** %d \\input target(s) do not exist:\n", length(.miss10)))
      for (f in .miss10) cat("        ", f, "\n", sep = "")
    }

    # Everything the compile will actually see: the manuscript and every
    # fragment it inputs. A macro defined in a fragment is defined.
    .all10 <- .src10
    for (f in setdiff(.in10, c(.miss10, .tpath10)))
      .all10 <- c(.all10, readLines(.path10(f), warn = FALSE))
    .txt10 <- paste(.all10, collapse = "\n")

    .used10 <- unique(gsub("^\\\\", "", unlist(regmatches(.txt10,
      gregexpr("\\\\[A-Z][A-Za-z]{3,}(?![A-Za-z])", .txt10, perl = TRUE)))))
    .defs10 <- regmatches(.txt10, gregexpr(
      "\\\\(?:new|provide|renew)command\\s*\\{\\\\[A-Za-z]+\\}\\s*\\{[^}]*\\}",
      .txt10, perl = TRUE))[[1]]
    # ANCHORED, NOT GREEDY. ".*\\{\\\\(name)\\}" runs to the LAST brace group,
    # so \providecommand{\ExtractRows}{\tofill} yielded "tofill" and every
    # macro the file defined was reported as undefined.
    .name10 <- sub("^\\\\[a-z]+command\\s*\\{\\\\([A-Za-z]+)\\}.*$", "\\1",
                   .defs10)
    .tofl10 <- unique(.name10[grepl("\\{\\\\tofill\\}\\s*$", .defs10)])
    # v11. A FALLBACK THAT NEVER FIRES IS NOT A RED RULE. The preamble
    # carries \providecommand{\X}{\tofill} for macros a script writes, and
    # \providecommand defines only what is still undefined. The fragment
    # holding the real value is input first, so the fallback is a no-op and
    # the page prints the value. Counting it listed eight macros as red rules
    # on 25 September while the PDF printed all eight. A name counts only if
    # nothing the compile sees gives it a real value.
    .real10 <- unique(.name10[!grepl("\\{\\\\tofill\\}\\s*$", .defs10)])
    .tofl10 <- setdiff(.tofl10, .real10)
    .undef10 <- setdiff(.used10, .name10)

    # LaTeX's own names are used and never defined here, and reporting them
    # teaches the reader to skim. The list is the same one paper_tables.R
    # keeps, for the same reason.
    .tex_own10 <- c("AddToHook", "AtBeginDocument", "AtEndDocument", "Delta",
                    "DeclareHookRule", "ExplSyntaxOn", "ExplSyntaxOff",
                    "Gamma", "Omega", "FloatBarrier", "RaggedRight", "Huge",
                    "WriteBookmarks", "LaTeX", "TeX", "Roman", "Alph", "Large")
    # v35. THE LIST GROWS IN vm_settings.R. VM_LATEX_OWN_NAMES adds names
    # that LaTeX, a package or the class defines, and the built-in names
    # stay, so a new version of this file keeps what the settings know.
    # paper_tables.R writes \providecommand{\X}{\tofill} for every name its
    # own copy of this list lacks, which is how \MakeUppercase,
    # \DeclareRobustCommand, \IfFileExists and AEA.cls's \Keywords came to
    # be reported as red rules. \providecommand leaves a defined name
    # alone, so none of the four printed anything red. A leading backslash
    # in a setting is dropped.
    .tex_own10 <- unique(c(.tex_own10, sub("^\\\\", "", as.character(
      get0("VM_LATEX_OWN_NAMES", ifnotfound = character(0))))))
    .undef10 <- setdiff(.undef10, .tex_own10)

    if (length(.undef10)) {
      cat(sprintf("  *** %d macro(s) used and never defined. LaTeX will halt\n",
                  length(.undef10)))
      cat("      on the first of them:\n        ",
          paste(.undef10, collapse = ", "), "\n", sep = "")
    }
    # COUNT WHAT IS PRINTED. The count was of every \providecommand falling
    # back to \tofill and the list was of those that are not LaTeX's own, so
    # a run with twelve harmless no-ops and no real gaps reported "12 macros
    # fall back to \tofill" followed by nothing. A number with an empty list
    # under it is worse than silence: it looks like a failure to enumerate.
    .tofl10 <- setdiff(.tofl10, .tex_own10)
    if (length(.tofl10)) {
      cat(sprintf("  %d macro(s) fall back to \\tofill, so they print as a red\n",
                  length(.tofl10)))
      cat("      rule. Each is a value a script owes you:\n        ",
          paste(.tofl10, collapse = ", "), "\n", sep = "")
    }

    .frag10 <- if (dir.exists(.dir10))
      list.files(file.path(.dir10, "tables"), pattern = "[.]tex$",
                 full.names = TRUE) else character(0)
    .bad10 <- Filter(function(f) {
      .l <- readLines(f, warn = FALSE)
      .d <- grepl("\\\\(new|provide|renew)command\\s*\\{\\\\[A-Za-z]+\\}\\s*\\{\\\\tofill\\}",
                  .l)
      any(grepl("tofill", .l[!.d], fixed = TRUE))
    }, .frag10)
    if (length(.bad10)) {
      cat(sprintf("  *** %d fragment(s) beside the manuscript carry \\tofill in\n",
                  length(.bad10)))
      cat("      a cell, so a red rule is on the page:\n")
      for (f in .bad10) cat("        ", basename(f), "\n", sep = "")
    }
    # BEFORE THE BRANCH. The denominator is the macros the manuscript uses;
    # a fail is a name that is undefined, falls back to \tofill, or sits in
    # a fragment that carries one, plus any missing \input target.
    vm_verdict("10", n = length(.used10),
               bad = length(.miss10) + length(.undef10) +
                     length(.tofl10) + length(.bad10),
               unit = "macro(s)")
    if (!length(.miss10) && !length(.undef10) && !length(.tofl10) &&
        !length(.bad10))
      cat(sprintf("  %d \\input target(s) present, %d macro(s) used, all defined,\n  none falling back to \\tofill, no fragment carrying one.\n",
                  length(.in10), length(.used10)))
    rm(.dir10, .src10, .in10, .path10, .miss10, .all10, .txt10, .used10,
       .defs10, .name10, .tofl10, .undef10, .tex_own10, .frag10, .bad10,
       .kpse10, .on_path10, .tpath10, .real10)
  }

  # ---------------------------------------------------------------------
  # CHECK 11. The same artefact in two places
  #
  # Every failure worth a day this month has been one name resolving to two
  # files: fragments in TG/tables and TG/output/tables, the wage ledger in
  # output/results and the project root, a frame cached in memory and on
  # disk. The fragment audit in paper_tables.R covers .tex; nothing covered
  # the csv and rds artefacts, and on 9 September the master wrote eight of
  # them into the root beside older copies of themselves and no line said so.
  #
  # mtime answers an ordering question here, not a content one, so the
  # contents are compared and the timestamps are reported beside them.
  # ---------------------------------------------------------------------
  cat("\n=== CHECK 11. The same artefact in two places ===\n")
  # FIGURES TOO. A figure written beside the working directory instead of
  # beside the manuscript is the same fault as a ledger in two folders, and
  # it is harder to see: the paper keeps drawing the older one and nothing
  # in the compile says which it drew.
  .dirs11 <- unique(c(if (exists("PROJ_ROOT")) PROJ_ROOT,
                      if (exists("DIR_RESULTS")) DIR_RESULTS,
                      if (exists("DIR_DESIGNS")) DIR_DESIGNS,
                      if (exists("DIR_FIGURES")) DIR_FIGURES,
                      if (exists("PAPER_FIGURES")) PAPER_FIGURES,
                      if (exists("PROJ_ROOT")) file.path(PROJ_ROOT, "figures"),
                      OUTDIR, if (!is.na(WGDIR)) WGDIR))
  .dirs11 <- .dirs11[!is.na(.dirs11) & dir.exists(.dirs11)]
  .f11 <- do.call(rbind, lapply(.dirs11, function(d) {
    f <- list.files(d, pattern = "[.](csv|rds|pdf|png)$")
    if (!length(f)) NULL else data.frame(dir = d, file = f,
                                         stringsAsFactors = FALSE)
  }))
  .dup11 <- if (is.null(.f11)) character(0) else
    names(which(table(.f11$file) > 1L))
  # tag11 IS A FUNCTION DEFINED TWENTY LINES BELOW, not a count: the first
  # version of this called length() on it before it existed. The denominator
  # is the artefacts scanned, and the call sits before the branch so a
  # duplicate reports FAIL rather than NO REPORT.
  vm_verdict("11", n = if (is.null(.f11)) 0L else nrow(.f11),
             bad = length(.dup11), unit = "artefact(s)")
  if (!length(.dup11)) {
    cat(sprintf("  %d artefact(s) across %d folder(s), no name in two of them.\n",
                if (is.null(.f11)) 0L else nrow(.f11), length(.dirs11)))
  } else {
    .tag11 <- function(p) {
      z <- tryCatch(readBin(p, "raw", file.info(p)$size), error = function(e) raw(0))
      if (!length(z)) return("--------")
      sprintf("%08x", as.integer(sum(as.numeric(z) * seq_along(z)) %% 2^31))
    }
    cat(sprintf("  *** %d artefact name(s) exist in more than one folder:\n",
                length(.dup11)))
    for (nmx in sort(.dup11)) {
      .w <- .f11[.f11$file == nmx, ]
      .p <- file.path(.w$dir, nmx)
      .t <- vapply(.p, .tag11, character(1))
      cat("      ", nmx, if (length(unique(.t)) == 1L)
            "   (identical bytes)" else "   *** CONTENTS DIFFER", "\n", sep = "")
      for (i in seq_along(.p))
        cat(sprintf("          %s  %s  %s\n", .t[i],
                    format(file.info(.p[i])$mtime, "%Y-%m-%d %H:%M"),
                    dirname(.p[i])))
    }
    cat("      A reader that widens its search can take either. Delete the\n",
        "      copy that is not the one a stage writes, rather than the older\n",
        "      one: age is not provenance.\n", sep = "")
    rm(.tag11)
  }
  rm(.dirs11, .f11, .dup11)

  if (WRITE_REPORT) {
    utils::write.csv(chk, file.path(OUTDIR, "manuscript_reconciliation.csv"),
                     row.names = FALSE)
    cat("\nWritten:", file.path(OUTDIR, "manuscript_reconciliation.csv"), "\n")
  }
  
  cat("\n---------------------------------------------------------------\n")
  cat("Order of operations around a run of 03:\n")
  cat("  BEFORE it finishes : SNAPSHOT <- TRUE, source this file.\n")
  cat("  AFTER it finishes  : SNAPSHOT <- FALSE,\n")
  cat("                       DIFF_AGAINST <- '<snapshot name>', source again.\n")
  cat("  Before any circulated .tex compile: source with both at defaults.\n")

  # THE VERDICT TABLE, LAST. vm_summary() prints one row per check with its
  # denominator and status, and names any check that reported nothing. A
  # check still on the old cat() shows as NO REPORT, which is how a partial
  # retrofit stays visible instead of looking finished.
  vm_summary()
  
}  # end of the else branch on SNAPSHOT


# CLOSE ONLY WHAT WAS OPENED. A run that inherited a sink leaves it
# exactly as it found it.
vm_log_close()
rm(.vm_owns, .vm_sink_before, .vm_msg_before, .vm_log)
if (exists(".vm_dir")) rm(.vm_dir)

# source("paths.R")
# source("verify_manuscript_numbers.R")
