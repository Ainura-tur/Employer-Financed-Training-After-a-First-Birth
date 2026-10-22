# =============================================================================
# 00_contract.R  —  the handoff protocol
#
# Source FIRST, before 00compat.R and everything else.
#
# WHY THIS EXISTS. Three failures in this project were the same failure:
#
#   mde()   defined in 03training_designs.R (vectorised, parameterised on
#           POWER/ALPHA) and again in training_funding_divergence.R (scalar,
#           hard-coded 5%/80%). The second sourced later, so it won, and the
#           master died 1,300 lines away in a block that uses neither script.
#   ev      built by 03training_designs.R and picked up by MASTER_training.R
#           from whatever was left in the global environment. Run the scripts
#           in a different order, or let one fail halfway, and it is stale or
#           absent with nothing to say so.
#   panel   the master built the analysis sample (129,544 rows); the
#           divergence script read the file (134,131). Every estimate would
#           have sat on a different sample from the T-series.
#
# One cause: scripts talk through the global environment and nothing declares
# what passes between them. A collision is invisible until it errors
# somewhere unrelated; a wrong sample is invisible until a guard catches it.
#
# WHAT THIS DOES NOT DO. It does not merge the scripts. 03training_designs.R
# holds the blind ledger, and the pre-commitment is credible partly because
# the designation lives in a separate artefact with its own timestamp. A
# script that both seals estimates and reads them has given that up.
#
# THE PROTOCOL, in three rules:
#
#   1. SHARED HELPERS ARE DEFINED HERE AND NOWHERE ELSE. A script that needs
#      mde() uses this one. td_seal() makes redefinition an error rather
#      than a silent overwrite.
#   2. ANYTHING CROSSING A SCRIPT BOUNDARY GOES THROUGH td_put/td_get. The
#      registry is backed by disk, so each script stays runnable standalone
#      and a warm master run costs nothing.
#   3. INPUTS ARE ASSERTED, NOT ASSUMED. td_require() fails with the name of
#      what is missing, at the top of the script that needs it, rather than
#      600 lines later on a NULL.
# =============================================================================

# --- Namespace protection (avoid MASS/stats masking dplyr verbs) ------
# Defined once here. Delete the copy at the head of the other four scripts;
# four identical copies are harmless but they invite a fifth that is not.
select <- dplyr::select;       filter    <- dplyr::filter
mutate <- dplyr::mutate;       slice     <- dplyr::slice
recode <- dplyr::recode;       rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange <- dplyr::arrange;     count     <- dplyr::count;  lag <- dplyr::lag

# =============================================================================
# 0. ONE ROOT
# =============================================================================
# v25. TD_DIR AND LOG_DIR WERE RELATIVE, and that is the fourth instance of
# the failure in the header. "handoff" and "run_logs" resolve against
# getwd(), so a script run from a different directory got a DIFFERENT
# REGISTRY: td_has("frame") false in one process and true in another, with
# nothing to say the two disagreed because each was internally consistent.
# That is the `ev` failure again, one level down.
#
# The same relative-path habit put artefacts under
# Documents - Copy/Y2025/HILDA_project/training_designs while the panel was
# read from Desktop/WG. The master's v24.7 note records the cost: a skip
# audit reporting four artefacts ABSENT and PART 6 printing "NOTHING BELOW
# IS CHECKED" while the files sat intact the whole time.
#
# Everything now hangs off PROJ_ROOT. Override with the HILDA_ROOT
# environment variable rather than by editing the default, so the path
# travels to another machine without a diff.
#
# v26. TWO COPIES, ONE ROOT PER RUN, AND A SPLIT STOPS. The project exists
# twice, in C:/Users/tur277/TG and in OneDrive under Desktop/WG/TG. The
# default used to be the OneDrive copy, so a fresh session without
# HILDA_ROOT wrote there even when it was started in the local copy. On 24
# and 25 September 04, Referee 8 and paper_tables did exactly that, while
# the master, the event studies and the manuscript used the local copy, and
# one tables run built its macros from a stale frame it found there. The
# default is now the local copy, and two rules stop it happening again:
#   1. The log says where the root came from, HILDA_ROOT or the default, so
#      a stale environment variable shows in every run.
#   2. A session whose working directory is itself a copy of the project,
#      because it holds 00_contract.R, must use that copy. If PROJ_ROOT
#      names another, the contract stops rather than let the run read one
#      copy and write the other. options(hilda_allow_root_mismatch = TRUE)
#      allows a deliberate split.
PROJ_ROOT_FROM <- if (nzchar(Sys.getenv("HILDA_ROOT"))) "HILDA_ROOT" else "default"
PROJ_ROOT <- normalizePath(
  Sys.getenv("HILDA_ROOT",
             "C:/Users/tur277/TG"),
  winslash = "/", mustWork = FALSE)

if (!dir.exists(PROJ_ROOT))
  stop("PROJ_ROOT does not exist: ", PROJ_ROOT,
       "\n  Set HILDA_ROOT, or fix the default in 00_contract.R. Nothing ",
       "below is meaningful until this is right.", call. = FALSE)

.wd_now   <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
.same_dir <- function(a, b)
  identical(tolower(sub("/+$", "", a)), tolower(sub("/+$", "", b)))
if (file.exists(file.path(.wd_now, "00_contract.R")) &&
    !.same_dir(.wd_now, PROJ_ROOT) &&
    !isTRUE(getOption("hilda_allow_root_mismatch", FALSE))) {
  stop("This session runs in ", .wd_now, ", which is a copy of the project,",
       "\n  but PROJ_ROOT is ", PROJ_ROOT, " (from ", PROJ_ROOT_FROM, ").",
       "\n  The run would read one copy and write the other. Set HILDA_ROOT",
       "\n  to the copy you mean, or setwd() into it, and restart R.",
       "\n  options(hilda_allow_root_mismatch = TRUE) allows a deliberate split.",
       call. = FALSE)
}
rm(.wd_now, .same_dir)

DIR_R       <- file.path(PROJ_ROOT, "R")
DIR_DATA    <- file.path(PROJ_ROOT, "data")
DIR_DESIGNS <- file.path(PROJ_ROOT, "output", "designs")
DIR_RESULTS <- file.path(PROJ_ROOT, "output", "results")
DIR_TABLES  <- file.path(PROJ_ROOT, "output", "tables")

# WORK THAT IS NOT A DELIVERABLE, and needs somewhere that is not beside one.
#
# Pre_analysis_diagnostics2.R writes six .tex fragments and a csv, of which
# exactly one window's worth is ever kept: they are a triage step whose whole
# purpose is that most of them are discarded. Sent to DIR_TABLES they sat
# among the fragments the manuscript \input{}s, and the script's own v7.3
# note records the consequence, two files both called Table 4 disagreeing on
# the never-mother boundary with nothing to say which the paper draws.
#
# The fix is not a warning, it is a different folder. A directory whose name
# says "diagnostics" cannot be \input{} by accident, and a fragment that
# turns out to be a deliverable is moved deliberately.
DIR_DIAG    <- file.path(PROJ_ROOT, "output", "diagnostics")

# DIR_FIGURES IS DEFINED BELOW, not here, because it is now derived from the
# manuscript's location and MANUSCRIPT is set further down. See the block
# after PAPER_FIGURES.

# THE MANUSCRIPT, AND THE FOLDER IT READS.
#
# Both scripts resolve these by search when they are unbound, and both
# searches are correct, which is not the same as agreeing. Worse, the macro
# scan at the foot of paper_tables.R used only get0("MANUSCRIPT"), so it ran
# whenever the verifier had been sourced first in the same session and
# skipped in silence otherwise. After a restart on 9 September it skipped,
# no \providecommand fallbacks were written, and LaTeX halted on
# \ExtractRows with no indication of which script owed it.
#
# \input resolves relative to the .tex being compiled, so the table folder
# is the manuscript's neighbour and is derived rather than named twice.
MANUSCRIPT   <- file.path(PROJ_ROOT, "els-cas-templates",
                          "Motherhood_training_v5.tex")
PAPER_TABLES <- file.path(dirname(MANUSCRIPT), "tables")

# AND ITS FIGURES FOLDER, for the same reason and after the same failure.
# RUN_FIGURES_PAPER.R defaulted FIGDIR to the bare string "figures", which
# resolves against getwd(), so a run from the root wrote TG/figures while
# \includegraphics reads els-cas-templates/figures. Two folders, and the
# remedy that suggests itself is to copy, which is how every other pair in
# this project came to exist.
PAPER_FIGURES <- file.path(dirname(MANUSCRIPT), "figures")

# AND THERE IS ONLY ONE FIGURES FOLDER, WHICH IS THAT ONE.
#
# DIR_FIGURES used to be PROJ_ROOT/figures, on the reasoning that figures are
# looked at by hand more often than ledgers are and so belong near the top of
# the tree. That reasoning was fine and the consequence was not: the contract
# created PROJ_ROOT/figures on every load, td_out("figures", ...) wrote there,
# \includegraphics read els-cas-templates/figures, and the remedy that
# suggests itself is to copy. Twelve figures existed in both, and deleting
# either copy accomplished nothing because the next contract load rebuilt the
# folder it had just been told nothing writes to.
#
# So the two names now resolve to one directory, the one the .tex reads. A
# figure written by any route is a figure the manuscript will compile, and
# there is no second folder for a stale copy to wait in. DIR_FIGURES is kept
# as a name rather than retired because paths.R, RUN_FIGURES_PAPER.R and
# td_out() all reach for it; what changes is only where it points.
DIR_FIGURES <- PAPER_FIGURES

if (!file.exists(MANUSCRIPT))
  cat("  NOTE: MANUSCRIPT does not exist:\n        ", MANUSCRIPT,
      "\n        Update it here when the draft is renamed.\n", sep = "")


# =============================================================================
# THE REPORTED WAGE-PATH WINDOW
# =============================================================================
# These three were set at line 1875 of paper_tables.R, which is the only place
# that filtered by them, so any other script agreed with the window only by
# running in the same session. verify_manuscript_numbers.R ran alone, found
# them absent, and computed the pre-birth band of its timing rule on horizons
# reaching back past K_LO: estimates no table in the paper shows, deciding a
# sentence about when the wage series moves.
#
# A window is a claim about what the paper reports, so it belongs where the
# other claims about the project live rather than inside the script that
# happens to draw the table. K_REF is the omitted reference horizon; a series
# that carries it has not been normalised and the tables drop it.
# NAMED FOR THE SERIES, because K_HI was already taken. 00compat.R sets
# K_HI <- 8 for the DESCRIPTIVE event window, which is a different window of
# a different series: the profile runs k = -8 to 8 and the wage path runs
# -8 to +10. Sealing my three under the bare names made every 03 stage stop
# on "sealed object redefined by 00compat.R", and it was right to: two
# quantities were sharing one name and the last file sourced decided which
# window a caller got. Distinct names is the fix; sealing was only the thing
# that found it.
WAGE_K_LO  <- -8L
WAGE_K_HI  <-  10L
WAGE_K_REF <-  -1L


if (!(WAGE_K_REF >= WAGE_K_LO && WAGE_K_REF <= WAGE_K_HI))
  stop("WAGE_K_REF = ", WAGE_K_REF, " is outside [", WAGE_K_LO,
       ", ", WAGE_K_HI, "].",
       call. = FALSE)


TD_DIR  <- file.path(PROJ_ROOT, "handoff")
LOG_DIR <- file.path(PROJ_ROOT, "run_logs")

for (.d in c(DIR_DATA, DIR_DESIGNS, DIR_RESULTS, DIR_FIGURES, DIR_TABLES,
             DIR_DIAG, TD_DIR, LOG_DIR))
  if (!dir.exists(.d)) dir.create(.d, recursive = TRUE, showWarnings = FALSE)
rm(.d)

# td_out("designs", "training_ledger.csv") rather than file.path(OUTDIR, ...).
# A module names WHAT KIND of output it writes and not where it goes, so the
# layout changes here alone.
td_out <- function(kind = c("designs", "results", "figures", "tables",
                            "data", "logs", "handoff", "diagnostics"), ...) {
  kind <- match.arg(kind)
  file.path(switch(kind,
                   designs = DIR_DESIGNS, results = DIR_RESULTS,
                   figures = DIR_FIGURES, tables  = DIR_TABLES,
                   data    = DIR_DATA,    logs    = LOG_DIR,
                   handoff = TD_DIR,      diagnostics = DIR_DIAG), ...)
}

# .out_path() IS td_out("results", ...) AND IT LIVES HERE.
#
# Three files defined this helper, identically at first and then not. The
# master was corrected to write into output/results and nothing moved,
# because training_funding_divergence.R and CS_event_study.R each define
# their own at top level and those definitions survive their source() and
# shadow the caller's for every write that follows. On 9 September that was
# every write the master makes: timing_paths.csv, training_decomposition.csv,
# the two A5 files, hours_robustness.csv, train_ledger.csv and
# wage_path_ledger.csv all landed in the project root beside older copies of
# themselves in output/results, and nothing said so.
#
# It is the gap(w, m) problem the master already names: a module is entitled
# to use a name while it runs and not to change what the caller means by it
# afterwards. Sealed here, td_check_seals() catches a module that redefines
# it and source_logged() restores it afterwards.
.out_path <- function(f) td_out("results", f)


# BACK-COMPATIBILITY, DELIBERATELY TEMPORARY. Modules still carrying
# file.path(OUTDIR, ...) and file.path(project_dir, "training_designs")
# keep working, so migration is one module at a time rather than one commit
# that either works or does not.
#
# DELETE THESE THREE LINES once nothing but this comment mentions them. A
# permanent alias is how two names for one directory survive long enough to
# disagree, which is the header's first failure wearing different clothes.
OUTDIR      <- DIR_DESIGNS
TD_OUTDIR   <- DIR_DESIGNS
project_dir <- PROJ_ROOT

# ---- legacy locations, reported and NOT moved -----------------------
# Advisory by design. A contract that silently relocates a reader's files
# is worse than one that tells them to: the move is the moment to check
# that two copies of the ledger have not already drifted.
#
# THE FOUR FIGURE-SIDE ENTRIES ARE NEW, and they are here rather than in a
# cleanup script because the contract is what stopped writing to them. Once
# DIR_FIGURES resolves to the manuscript's own folder, PROJ_ROOT/figures has
# no writer; output/figures never had one and was only ever named; and
# els-cas-templates/figs is a sibling of the folder that is read, which is
# the shape a stale duplicate takes when someone fixes a path by copying.
# Each is reported with a count so the decision is made against what is in
# it rather than against its name.
.td_legacy <- c(
  file.path(path.expand("~"), "..", "Documents - Copy", "Y2025",
            "HILDA_project", "training_designs"),
  file.path(PROJ_ROOT, "training_designs"),
  file.path(PROJ_ROOT, "figures"),
  file.path(PROJ_ROOT, "output", "figures"),
  file.path(dirname(MANUSCRIPT), "figs"))
.td_legacy <- .td_legacy[dir.exists(.td_legacy)]
# A path that resolves to a directory still in use is not legacy. Compared
# after normalisation because PROJ_ROOT/figures and the manuscript's figures
# folder are different strings that could, on another tree, be one directory.
.td_live <- normalizePath(c(DIR_DESIGNS, DIR_FIGURES, DIR_RESULTS,
                            DIR_TABLES, DIR_DIAG), mustWork = FALSE)
.td_legacy <- .td_legacy[!normalizePath(.td_legacy, mustWork = FALSE)
                         %in% .td_live]
if (length(.td_legacy)) {
  cat("\n  LEGACY ARTEFACT DIRECTORIES STILL PRESENT. Nothing writes them now:\n")
  for (.p in .td_legacy)
    cat(sprintf("    %s  (%d files)\n", normalizePath(.p, mustWork = FALSE),
                length(list.files(.p, recursive = TRUE))))
  cat("  Move anything you still want into the folder that replaces it,\n",
      "  then delete them. Designs go to ", DIR_DESIGNS, ";\n",
      "  figures to ", DIR_FIGURES, ".\n", sep = "")
  rm(.p)
}
rm(.td_legacy, .td_live)

# =============================================================================
# PER-SCRIPT LOGGING
# =============================================================================
# td_log_open() / td_log_close() wrap a script in its own transcript. One log
# per script, not one per run, because the failures in this project have all
# been LOCAL: a helper redefined in one file, a frame resolved differently in
# another. A single combined log makes those harder to find, not easier.
#
# WHY sink() TWICE. sink() alone captures printed output; sink(type =
# "message") captures warnings and errors. Without the second, a script that
# dies leaves a log ending mid-sentence with no reason in it, which is how
# the 9.3 crash first presented.
#
# on.exit is not usable here because these run at top level in a sourced
# script, not inside a function. td_log_close() is therefore called
# explicitly at the end of each script AND registered as a fallback, so a
# script that stops early still closes its sinks rather than leaving the
# console redirected into a dead file.
# v24.8. THE REGISTRY SURVIVES A RE-SOURCE, and the sink depth this session
# started at is recorded once.
#
# It was `.TD_LOG <- new.env(...)` unconditionally, so re-sourcing the
# contract wiped `depth` and the registry forgot a log it still had open.
# td_log_open compensated by unwinding any sink it did not recognise, which
# is correct when the sink is a leak from a run that died and wrong when it
# belongs to a live caller. RUN_ALL.R opens its own sink with its own
# connection, outside this registry, so on the 31 August run the master
# unwound the driver's log: RUN_ALL_155040.log stops at its own sink setup,
# MASTER_155041.log four lines in, and 04 reported the same condition five
# minutes later.
#
# Keeping the registry means `depth` is not wiped, so a nested call is
# recognised as nested. .TD_SINK_BASE is the second half: it is the depth
# that existed when this session first loaded the contract, which is the
# caller's, and nothing below it is ever closed. Together they make the
# heuristic unnecessary rather than better.
if (!exists(".TD_LOG", envir = globalenv()) ||
    !is.environment(get(".TD_LOG", envir = globalenv())))
  assign(".TD_LOG", new.env(parent = emptyenv()), envir = globalenv())
.TD_LOG <- get(".TD_LOG", envir = globalenv())

if (!exists(".TD_SINK_BASE", envir = globalenv()))
  assign(".TD_SINK_BASE", sink.number(), envir = globalenv())

# v24. THE LOGGERS NEST, and until now they did not.
#
# WHAT HAPPENED. 03training_designs.R opens its log, then sources
# 00compat.R, which opens ITS log and closes it. The close ran
#
#     while (sink.number() > 0) sink()
#
# which unwound the OUTER script's sink too, and then wiped .TD_LOG. Control
# returned to 03 with no sink, no registry, and an orphaned connection: its
# log file ended at the exact line where 00compat finished, and its own
# td_log_close() returned immediately because .TD_LOG no longer held a `con`.
# The run continued for as long as it needed to, invisibly. This is the same
# defect the module-level sink fix removed one revision ago, living in the
# contract's own logger, which is a worse place for it because every script
# goes through here.
#
# THE FIX IS A DEPTH COUNT, not a stack of connections. RUN_ORDER wants ONE
# log per script; a nested open is a script sourcing another script, and the
# inner one should write into the outer one's record rather than open a
# rival. So the first open owns the sink, later opens announce themselves and
# increment, and only the matching close tears anything down.
td_log_open <- function(script, echo = TRUE) {
  # v24.4. CLEAN UP A LOG LEFT OPEN BY A FAILED RUN, before deciding whether
  # this call is nested.
  #
  # THIS PRODUCED TWO LOGS FROM ONE SESSION. A run at 19:37:49 died on a
  # missing mde(), so td_log_close() never executed: .TD_LOG kept depth = 1
  # and the sink stayed open. The re-run at 19:41:03 re-sourced the contract,
  # which rebuilt .TD_LOG and wiped depth, so td_log_open opened a SECOND
  # sink on top of the first. With split = TRUE the enclosing sink is the
  # second's "console", so every line went to both files. 193749.log ended
  # up with two opening banners and the 194103 run's output inside it.
  #
  # Neither file is a record of one run, which is worse than having no log:
  # a reader cannot tell which lines belong to which invocation.
  #
  # THE TEST IS THE SINK, NOT THE REGISTRY. depth is bookkeeping and a
  # re-source destroys it; sink.number() is the real state and survives.
  # If a sink is open that this registry does not account for, the previous
  # run died, and the right move is to unwind to where we came in rather
  # than nest on top of it.
  # v24.8. ONLY SINKS ABOVE THE CALLER'S BASELINE ARE LEAKS. .TD_SINK_BASE is
  # the depth when this session first loaded the contract; a sink at or below
  # it belongs to whoever sourced us and closing it destroys their log. Above
  # it, and unaccounted for by the registry, is a run that died.
  #
  # Since the registry now survives a re-source, `depth` is only absent when
  # no log is open in this session, so this branch fires far less often than
  # it did and never on a nested call.
  .base <- if (exists(".TD_SINK_BASE", envir = globalenv()))
    get(".TD_SINK_BASE", envir = globalenv()) else 0L
  if (!exists("depth", envir = .TD_LOG) && sink.number() > .base) {
    cat("  log: found", sink.number() - .base, "open sink(s) above the\n")
    cat("  caller's baseline of", .base, "and unaccounted for by the\n")
    cat("  registry, so a previous run died without closing its log.\n")
    cat("  Unwinding to the baseline, which leaves the caller's sink alone.\n")
    # sink.number(type = "message") RETURNS A CONNECTION NUMBER, NOT A
    # COUNT, and stderr is connection 2. So `> 0` is true with no message
    # sink at all, and the obvious loop here spins forever. Measured:
    #
    #   no sinks    out = 0   msg = 2
    #   both open   out = 1   msg = 3
    #
    # There is at most ONE message sink, so it is removed with a single
    # conditional call, not a loop. Only the OUTPUT sink stacks.
    if (sink.number(type = "message") > 2L) sink(type = "message")
    while (sink.number() > .base) sink()
  }
  # A CALLER'S SINK IS INHERITED, not replaced. Without this the master opens
  # a second file under RUN_ALL.R and the driver's log ends where the master
  # begins, which is what the 31 August logs show.
  if (!exists("depth", envir = .TD_LOG) && sink.number() > 0L) {
    assign("depth",  1L,      envir = .TD_LOG)
    assign("script", script,  envir = .TD_LOG)
    assign("file",   NA_character_, envir = .TD_LOG)
    assign("inherited", TRUE, envir = .TD_LOG)
    cat("  log: ", script, " inherits the caller's sink; no separate file,\n",
        "       so the run stays in one record.\n", sep = "")
    return(invisible(NA_character_))
  }
  if (exists("depth", envir = .TD_LOG)) {
    assign("depth", get("depth", envir = .TD_LOG) + 1L, envir = .TD_LOG)
    cat("  log: ", script, " inherits ",
        get("script", envir = .TD_LOG), "'s log (nested).\n", sep = "")
    return(invisible(get("file", envir = .TD_LOG)))
  }
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  f  <- file.path(LOG_DIR, sprintf("%s_%s.log", tools::file_path_sans_ext(script), ts))
  con <- file(f, open = "wt")
  assign("con", con, envir = .TD_LOG)
  assign("file", f, envir = .TD_LOG)
  assign("t0", Sys.time(), envir = .TD_LOG)
  assign("script", script, envir = .TD_LOG)
  assign("depth", 1L, envir = .TD_LOG)
  # AND AN EXIT HOOK, so the next failure does not leave this state behind.
  # The loader and RUN_FIGURES_EDA.R already do this; the contract's own
  # logger did not, which is why the cleanup above was needed at all.
  # on.exit cannot be used: it registers on the current frame, and this
  # function returns immediately.
  reg.finalizer(globalenv(), function(e) {
    if (exists("con", envir = .TD_LOG)) try(td_log_close("stopped"), silent = TRUE)
  }, onexit = TRUE)
  assign("sk_out", sink.number(), envir = .TD_LOG)
  assign("sk_msg", sink.number(type = "message"), envir = .TD_LOG)
  sink(con, split = echo)
  sink(con, type = "message")
  cat(strrep("=", 78), "\n", sep = "")
  cat(script, "  started ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", sep = "")
  cat(strrep("=", 78), "\n\n", sep = "")
  invisible(f)
}

td_log_close <- function(status = "completed") {
  # AN INHERITED SINK IS RELEASED, NOT CLOSED, and the registry is cleared so
  # the next call in this session sees no open log. Without the clear, `depth`
  # would persist across the re-source the registry now survives, and a later
  # standalone run would nest onto a log that does not exist.
  if (isTRUE(mget("inherited", envir = .TD_LOG,
                  ifnotfound = list(FALSE))$inherited)) {
    rm(list = intersect(c("depth", "script", "file", "inherited"),
                        ls(envir = .TD_LOG, all.names = TRUE)),
       envir = .TD_LOG)
    return(invisible(NULL))
  }
  if (!exists("con", envir = .TD_LOG)) return(invisible(NULL))
  # A nested close only decrements. The script that opened the log is the
  # one that closes it.
  dep <- if (exists("depth", envir = .TD_LOG)) get("depth", envir = .TD_LOG) else 1L
  if (dep > 1L) {
    assign("depth", dep - 1L, envir = .TD_LOG)
    return(invisible(NULL))
  }
  el <- round(as.numeric(difftime(Sys.time(), get("t0", envir = .TD_LOG),
                                  units = "secs")))
  cat("\n", strrep("=", 78), "\n", sep = "")
  cat(get("script", envir = .TD_LOG), "  ", status,
      " in ", el, "s\n", sep = "")
  cat(strrep("=", 78), "\n", sep = "")
  # Unwind to the depth this log FOUND, not to zero. A caller outside the
  # contract may hold sinks of its own and they are not ours to remove.
  .so <- if (exists("sk_out", envir = .TD_LOG)) get("sk_out", envir = .TD_LOG) else 0L
  .sm <- if (exists("sk_msg", envir = .TD_LOG)) get("sk_msg", envir = .TD_LOG) else 0L
  # ONE conditional call, not a loop: there is at most one message sink and
  # sink.number(type = "message") is a connection number floored at 2
  # (stderr), so a while() over it cannot terminate once it reaches the
  # floor. Comparing against a captured .sm made this safe by accident;
  # writing it as a loop invited the copy that was not.
  if (sink.number(type = "message") > max(.sm, 2L)) sink(type = "message")
  while (sink.number() > .so) sink()
  try(close(get("con", envir = .TD_LOG)), silent = TRUE)
  cat("log: ", get("file", envir = .TD_LOG), " (", el, "s)\n", sep = "")
  rm(list = ls(.TD_LOG), envir = .TD_LOG)
  invisible(NULL)
}

# Safety net: if a script errors, the sinks are still open and every
# subsequent message disappears into a closed file. This restores them.
td_log_rescue <- function() {
  # Same correction: `> 0` on a connection number that floors at 2 spins
  # forever. td_log_rescue() is the thing you call when a run has already
  # gone wrong, so an infinite loop here would be found at the worst moment.
  if (sink.number(type = "message") > 2L) sink(type = "message")
  while (sink.number() > 0) sink()
  invisible(NULL)
}

.TD <- new.env(parent = emptyenv())   # the registry
.TD_SEALED <- character(0)            # names that may not be redefined

# =============================================================================
# 1. SHARED HELPERS
# =============================================================================
# POWER and ALPHA are the project's, not a script's. A helper that hard-codes
# them reports a wrong MDE cleanly and is never noticed, which is worse than
# the crash that exposed the duplicate mde().
POWER <- 0.80
ALPHA <- 0.05

# Vectorised. `if` errors on a condition of length > 1 in R 4.2+, and every
# real call site passes a column.
mde <- function(se, power = POWER, alpha = ALPHA)
  ifelse(is.finite(se) & se > 0,
         (stats::qnorm(1 - alpha / 2) + stats::qnorm(power)) * se,
         NA_real_)

# Women-minus-men (or any two-path) gap. Variances add because the two paths
# are fitted on disjoint persons; that is the only case this is valid for and
# the name should not be reused for a within-sample contrast.
gap <- function(a, b, keys = c("event_time", "att", "se")) {
  e <- intersect(a[[keys[1]]], b[[keys[1]]])
  data.frame(
    event_time = e,
    gap = a[[keys[2]]][match(e, a[[keys[1]]])] - b[[keys[2]]][match(e, b[[keys[1]]])],
    se  = sqrt(a[[keys[3]]][match(e, a[[keys[1]]])]^2 +
                 b[[keys[3]]][match(e, b[[keys[1]]])]^2))
}

fmt_est <- function(est, se, digits = 4) {
  m <- mde(se)
  sprintf(paste0("%+.", digits, "f (SE %.", digits, "f) MDE %.", digits, "f%s"),
          est, se, m,
          if (is.finite(m) && abs(est) < m) "  [below MDE]" else "")
}

# --- sealing ----------------------------------------------------------
# After td_seal("mde"), a later `mde <- function(...)` in a sourced script
# still overwrites the binding — R gives no way to prevent that without
# lockBinding on globalenv, which breaks too much else. What td_check_seals()
# does is DETECT it, and it is called at the end of every script.
td_seal <- function(...) {
  nm <- c(...)
  for (n in nm)
    if (exists(n, envir = globalenv()))
      assign(paste0(".TD_SEAL_", n), get(n, envir = globalenv()), envir = .TD)
  .TD_SEALED <<- union(.TD_SEALED, nm)
  invisible(nm)
}

td_check_seals <- function(context = "") {
  bad <- character(0)
  for (n in .TD_SEALED) {
    key <- paste0(".TD_SEAL_", n)
    if (!exists(key, envir = .TD)) next
    if (!exists(n, envir = globalenv())) { bad <- c(bad, paste0(n, " (gone)")); next }
    if (!identical(get(n, envir = globalenv()), get(key, envir = .TD)))
      bad <- c(bad, n)
  }
  if (length(bad)) {
    # v24. PRINT BEFORE STOPPING, and flush.
    #
    # The v24 run reached this line with mde redefined, stop() fired, and the
    # message was INVISIBLE: it goes to the message sink, td_log_close() never
    # ran because stop() had already unwound the script, and the log
    # connection was never flushed or closed. The visible record ended on the
    # td_check_seals() call with no reason in it - the third silent stall in
    # this pipeline and the second caused by an error arriving after its own
    # log had stopped being written.
    #
    # cat() goes to the output sink, which is still live here, and flush()
    # forces it to disk before the condition unwinds anything.
    cat("\n", strrep("!", 78), "\n", sep = "")
    cat("CONTRACT: SEALED OBJECT(S) REDEFINED",
        if (nzchar(context)) paste(" BY", context) else "", "\n", sep = "")
    cat("  ", paste(bad, collapse = ", "), "\n", sep = "")
    # v24.6. SAY WHAT IT BECAME, not just that it changed.
    #
    # "gap was redefined" sends the reader grepping four scripts for an
    # assignment that may not be a top-level `gap <- `: a for-loop variable,
    # an assign(), a column pulled out of a data frame, or a definition
    # inside a module sourced from somewhere else. What the object IS now
    # narrows that to one line of one file in seconds. A function of a
    # different arity is a rival definition; a data frame is a stray
    # extraction; a numeric vector is almost always a loop variable.
    for (.b in bad) {
      .n <- sub(" \\(gone\\)$", "", .b)
      if (!exists(.n, envir = globalenv())) next
      .now <- get(.n, envir = globalenv())
      .was <- get(paste0(".TD_SEAL_", .n), envir = .TD)
      cat(sprintf("    %s: sealed as %s", .n,
                  if (is.function(.was))
                    paste0("function(", paste(names(formals(.was)), collapse = ", "), ")")
                  else paste0(class(.was)[1], " length ", length(.was))), "\n", sep = "")
      cat(sprintf("       now %s", 
                  if (is.function(.now))
                    paste0("function(", paste(names(formals(.now)), collapse = ", "), ")")
                  else paste0(class(.now)[1], " length ", length(.now),
                              if (length(.now) && length(.now) <= 6)
                                paste0(": ", paste(utils::head(as.character(.now), 6),
                                                   collapse = ", ")) else "")), "\n", sep = "")
      if (!is.function(.now) && is.function(.was))
        cat("       -> a FUNCTION became data. Look for a for-loop variable,\n",
            "          an assign(), or a column pulled out of a data frame.\n", sep = "")
      else if (is.function(.now) && is.function(.was))
        cat("       -> a RIVAL DEFINITION. Delete it and use the contract's.\n")
    }
    rm(.b, .n, .now, .was)
    cat("  Change the shared helper in 00_contract.R. Do not shadow it\n")
    cat("  locally: the last definition sourced wins and no call site says\n")
    cat("  which one it got.\n")
    cat(strrep("!", 78), "\n", sep = "")
    if (exists("con", envir = .TD_LOG))
      try(flush(get("con", envir = .TD_LOG)), silent = TRUE)
    stop("contract: sealed object(s) redefined", if (nzchar(context))
      paste0(" by ", context) else "", ": ", paste(bad, collapse = ", "),
      ". A shared helper must be changed in 00_contract.R, not shadowed ",
      "locally: the last definition sourced wins and no call site says so.",
      call. = FALSE)
  }
  invisible(TRUE)
}

td_seal("mde", "gap", "POWER", "ALPHA")

# v25. THE PATHS ARE SEALED TOO, and for the same reason the helpers are.
# 03 sets OUTDIR from its own project_dir; the master keeps a six-candidate
# search list; 04 had a third. Any one of them reassigning a path AFTER the
# contract loads sends that module's output somewhere the others do not
# read, and td_check_seals() is the only thing that would notice.
#
# A module that legitimately needs its own output directory should use
# td_out() or a local name, not reassign these.
td_seal("PROJ_ROOT", "TD_DIR", "LOG_DIR", "DIR_DATA", "DIR_DESIGNS",
        "DIR_RESULTS", "DIR_FIGURES", "DIR_TABLES", "DIR_DIAG", "td_out",
        "TD_SAMPLE_LO", "TD_SAMPLE_HI", "TD_SAMPLE_RULE",
        "WAGE_K_LO", "WAGE_K_HI", "WAGE_K_REF", ".out_path",
        "MANUSCRIPT", "PAPER_TABLES", "PAPER_FIGURES")

# =============================================================================
# 2. THE REGISTRY
# =============================================================================
# Backed by disk so a script runs standalone (cold: reads the file) and under
# the master (warm: reads the registry). The two paths give the same object,
# which is the point: a standalone debug run and a master run must not be
# estimating on different things.
td_put <- function(key, value, persist = TRUE) {
  assign(key, value, envir = .TD)
  if (persist)
    saveRDS(value, file.path(TD_DIR, paste0(key, ".rds")))
  invisible(value)
}

td_has <- function(key)
  exists(key, envir = .TD, inherits = FALSE) ||
  file.exists(file.path(TD_DIR, paste0(key, ".rds")))

td_get <- function(key, default = NULL) {
  if (exists(key, envir = .TD, inherits = FALSE)) return(get(key, envir = .TD))
  f <- file.path(TD_DIR, paste0(key, ".rds"))
  if (file.exists(f)) {
    v <- readRDS(f); assign(key, v, envir = .TD); return(v)
  }
  default
}

# UNLINKING THE FILE IS NOT ENOUGH, and every message in this file that said
# so was advice that does not work in the session it is printed in.
#
# td_get() caches into .TD on first read and td_has() looks there first, so a
# key deleted from disk is still served from memory for the rest of the
# session. On 9 September the frame was rebuilt three times against that:
# unlink() ran, td_frame() reported "registry key `frame`" again, and the
# stamp it carried stayed wrong, so \ExtractRows stayed blank through several
# rounds of looking at the wrong end of the pipeline.
td_forget <- function(...) {
  keys <- c(...)
  if (!length(keys)) keys <- c("frame", "ev")
  gone <- character(0)
  for (k in keys) {
    if (exists(k, envir = .TD, inherits = FALSE)) {
      rm(list = k, envir = .TD); gone <- c(gone, paste0(k, " (memory)"))
    }
    f <- file.path(TD_DIR, paste0(k, ".rds"))
    if (file.exists(f) && unlink(f) == 0L) gone <- c(gone, paste0(k, " (disk)"))
  }
  cat("  td_forget: ",
      if (length(gone)) paste(gone, collapse = ", ") else "nothing to drop",
      "\n", sep = "")
  invisible(gone)
}

# Assert inputs AT THE TOP of the script that needs them. The failure this
# replaces is a NULL travelling 600 lines and surfacing as an unrelated
# subscript error.
td_require <- function(..., context = "") {
  need <- c(...)
  miss <- need[!vapply(need, td_has, logical(1))]
  if (length(miss))
    stop("contract: ", if (nzchar(context)) paste0(context, " ") else "",
         "requires ", paste(miss, collapse = ", "),
         " but no such object is in the registry or in ", TD_DIR, "/. ",
         "Run the script that produces it first, or td_put() it by hand.",
         call. = FALSE)
  invisible(TRUE)
}

# =============================================================================
# 3. THE ANALYSIS FRAME, RESOLVED ONCE
# =============================================================================
# This replaces the tfd_frame option, the PREFER_FILE branch, and the
# master's own panel loading. One resolver, so the sample cannot differ
# between scripts without the difference being visible here.
#
# Precedence, most explicit first:
#   1. the registry           the caller has already declared it
#   2. in-memory `panel`      the caller is working on one
#   3. the RDS files          cold session
#
# A file was preferred over an in-memory frame once, and the divergence
# script spent a run estimating on the panel AS LOADED (134,131 rows) while
# the master worked on the ANALYSIS SAMPLE (129,544). Preferring a file over
# a frame the caller already holds is the wrong way round.
# v24. ORDER REVERSED, and the old order cost a whole run.
#
# hilda_panel_data_training.rds was first. It is a COLUMN SUBSET the loader
# writes for convenience - about 133 of the slim panel's 391 - and it carries
# no education column at all. So td_frame() resolved it, persisted it to
# handoff/frame.rds, and every script inherited a frame missing:
#
#   educ_years, education_level   T8's schooling term. It resolved <NA>, the
#                                 DML swap refused to run, and 03 finished in
#                                 SIX SECONDS where it used to take tens of
#                                 minutes. The log said only "wage, schooling
#                                 or experience column unresolved".
#   fulltime, public_sector       the master's derived columns, and every arm
#                                 of the 9.3 hours block.
#   num_children_under15,
#   group_ever, had_child_this_wave, hourly_wage_clean, any_training
#
# ONE WARNING ABOUT group_ever, PUT HERE BECAUSE EVERY SCRIPT SOURCES THIS
# FILE AND THE NAME HAS ALREADY MISLED TWO OF THEM. It is built from
# ever_parent, which reads 0 before a first birth and 1 from the birth wave
# onward, so it is ABSORBING BUT NOT PERSON-LEVEL: absorbing means it never
# switches back, not that it is constant within a person, and the "_ever"
# suffix reads as though it were. A cut on it puts a mother's pre-birth
# waves in the never-mothers arm and her post-birth waves in the mothers
# arm, so the arms partition person-WAVES exactly and do not partition
# PERSONS at all. That is a legitimate before-and-after contrast and
# several tables want precisely it. It is NOT a comparison of women who
# become mothers with women who do not, and a pre-birth slice of the
# mothers arm comes back empty by construction, which is how the defect
# was eventually found. Where a person-level grouping is meant, build it
# from max(ever_parent) per person and name it for the observation window
# rather than for a lifetime: "ever a parent within the panel" is not
# "ever a parent", and naming it for the second repeats the error.
#
# The narrow file is a legitimate artefact for a script that only needs the
# training items. It is the WRONG default for a shared frame, because the
# frame is persisted and whatever resolves first silently defines the column
# set for everything downstream. A superset costs memory; a subset costs
# results, and costs them quietly.
#
# THE FULL PANEL IS NOW FIRST. The training file remains as a fallback for a
# machine that only has that extract.
# v24.1. ONE FILE. hilda_panel_data_training.rds is no longer written: it
# was a column subset that resolved ahead of the full panel and cost two
# silent failures, the second of which made 03training_designs.R finish in
# six seconds with T8 skipped for want of an education column.
#
# A single-entry vector is the point rather than an accident of it. With two
# candidates the resolver had to be ordered correctly, and being wrong about
# the order produced a frame that worked well enough to run and not well
# enough to be right. With one, there is nothing to order.
# v25. ABSOLUTE CANDIDATES, so the resolver does not depend on getwd().
# This was `c("hilda_panel_data_W12_W24_slim.rds")`, tested with
# file.exists(f) against the working directory, so a script started from
# anywhere but the panel's own folder fell straight past it. The loop in
# td_frame() is unchanged; only what it iterates over is.
#
# ORDER IS PRECEDENCE. data/ first because that is where the loader should
# be writing; the root next for a panel not yet moved; "." last so a run
# in an unexpected directory still finds a local copy rather than stopping.
TD_FRAME_FILES <- file.path(c(DIR_DATA, PROJ_ROOT, "."),
                            "hilda_panel_data_W12_W24_slim.rds")

# THE COLUMNS A SHARED FRAME MUST CARRY, checked on every fresh resolve.
#
# need_cols in each td_frame() call states what THAT script needs. This is
# different: it is what any frame entering the registry must have, because
# the registry serves scripts that have not run yet and cannot object.
#
# KEPT AFTER THE EXTRACT WAS DROPPED, though that removed its original
# target. Two paths still reach the registry with a frame nobody has
# checked: an in-memory `panel` a caller happens to have narrowed, and a
# handoff/frame.rds persisted by an older build whose columns have since
# moved. Both would be inherited by every later script exactly as the
# extract was. Deleting one file removed the instance, not the class.
TD_FRAME_MUST_HAVE <- c("person_id", "wave", "female", "age",
                        "experience_years", "employed",
                        "educ_years", "fulltime", "had_child_this_wave")

# THE ANALYSIS RESTRICTION LIVES HERE, and this is the change that makes the
# rest of the contract mean anything.
#
# It used to live in MASTER_training.R, applied after its own readRDS. Under
# RUN_ORDER the master runs LAST, so the two scripts before it could not
# possibly have seen it: they resolved from the file at 134,131 rows while
# the master worked on 129,544. A resolver that reports agreement because
# each script agrees with itself is worse than no resolver, because it reads
# like a check.
#
# WHY NOT THE LOADER. It is an analysis restriction, not a data-quality
# screen. age - experience_years outside 10 to 35 is an implausible pairing
# FOR THIS DESIGN; another design on the same panel may reasonably keep
# those rows, and the loader serves both. The contract is the narrowest
# thing all four scripts share, so it is the right home for a sample
# definition all four must agree on and nobody else should inherit.
#
# TO CHANGE THE SAMPLE: change it here, delete handoff/frame.rds, re-run from
# the top. Changing it anywhere else now produces a frame that disagrees with
# the persisted one, and td_frame() will say so.
# v27. THE BOUNDS ARE SETTINGS NOW, and the reason is what the first run of
# the removals breakdown showed.
#
# THE LOWER BOUND HAS NEVER REMOVED A ROW. LOADING_ND3_v3.R caps experience
# at pmax(0, age - 13), so age - experience_years cannot fall below 13 and a
# bound at 10 cannot bind. The breakdown reports "below 10: 0". Methods
# describes a two-sided screen of which one side is dead code.
#
# THE UPPER BOUND IS THE WHOLE SCREEN, AND IT IS NOT NEUTRAL. It removes
# 4,587 person-waves, 3,538 of them mothers, at these rates: mothers 7.79
# per cent, fathers 1.44, childless men 1.00, never-mothers 0.99. Eight
# times the rate for the group the paper is about. A gap above 35 years
# between age and accumulated experience is not an implausible record; it is
# a long career interruption, which is the thing being studied. Section 5.2
# calls this "an implausibility screen rather than a substantive
# restriction". On those rates it is a substantive restriction.
#
# SO IT HAS TO BE A SETTING RATHER THAN A CONSTANT. A sample definition that
# removes the treated group at eight times the rate of the controls needs a
# run without it, and that run should cost one environment variable, not an
# edit to a sealed rule. TD_SAMPLE_HI = 60 exceeds the largest attainable
# gap in a 20-55 panel, so it turns the screen off without special-casing.
#
# ENVIRONMENT, NOT A GLOBAL, because RUN_ALL.R spawns each stage as a child
# Rscript and passes the parent environment through. A global set here would
# reach none of them, and the stages would screen differently from the
# session that launched them, which is the worst available outcome.
# v28, 14 OCTOBER. NO SCREEN BY DEFAULT, IN EVERY DESIGN.
#
# A referee asked for the principal results without the screen, and
# screen_audit.R measured what it does. It removes 4,587 person-waves, all at
# the upper bound and all with reported rather than imputed experience. It
# removes 7.85 per cent of mothers' person-waves (parenthood at the wave)
# against about 1 per cent in each other group, and the mothers it removes
# average 21.3 years out of the labour force against 3.8 for those it keeps.
# So it removes long career interruptions, which this paper studies, rather
# than records that cannot be true. It also barely reaches the design: 578
# of the removed rows are on the event-study roster, 89 of them employed,
# and the headline moves by 0.01 of its standard error without it.
#
# SO THE SCREEN IS OFF, in the experience designs as well. Every row it
# removed carries reported experience, so there is no data-quality case for
# keeping it there either.
#
# TD_SCREEN=on BRINGS IT BACK for the sensitivity run the appendix reports,
# at the bounds TD_SAMPLE_LO and TD_SAMPLE_HI set (10 and 35 by default).
# Those two do nothing while TD_SCREEN is off. All three are environment
# variables for the reason v27 gives: RUN_ALL.R's child processes inherit
# the environment and nothing else.
#
# WHAT "OFF" MEANS. td_apply_sample_rule() keeps every row, including a row
# with age or experience missing, and stamps the rule as "none". It still
# counts the rows the screen WOULD remove, by group, and td_frame() writes
# that count to screen_removals_by_group.csv with applied = FALSE, because
# the appendix describes the screen the paper no longer applies.
# A variable that is set but empty counts as unset, so TD_SCREEN= in
# .Renviron, or Sys.setenv(TD_SCREEN = ""), gives the default.
.td_env <- function(nm, default) {
  v <- trimws(Sys.getenv(nm, ""))
  if (nzchar(v)) v else default
}
TD_SCREEN <- tolower(.td_env("TD_SCREEN", "off"))
if (!TD_SCREEN %in% c("on", "off"))
  stop("TD_SCREEN must be \"on\" or \"off\"; got \"", Sys.getenv("TD_SCREEN"),
       "\".", call. = FALSE)
TD_SCREEN_ON <- identical(TD_SCREEN, "on")
TD_SAMPLE_LO <- suppressWarnings(as.numeric(.td_env("TD_SAMPLE_LO", "10")))
TD_SAMPLE_HI <- suppressWarnings(as.numeric(.td_env("TD_SAMPLE_HI", "35")))
rm(.td_env)
if (is.na(TD_SAMPLE_LO) || is.na(TD_SAMPLE_HI) || TD_SAMPLE_LO > TD_SAMPLE_HI)
  stop("TD_SAMPLE_LO/HI must be numbers with LO <= HI; got ",
       Sys.getenv("TD_SAMPLE_LO", "10"), " and ",
       Sys.getenv("TD_SAMPLE_HI", "35"), call. = FALSE)
# The screen's own rule, applied or not, and the rule in force.
TD_SAMPLE_RULE_SCREEN  <- sprintf("between(age - experience_years, %g, %g)",
                                  TD_SAMPLE_LO, TD_SAMPLE_HI)
TD_SAMPLE_RULE_DEFAULT <- "none"
TD_SAMPLE_RULE <- if (TD_SCREEN_ON) TD_SAMPLE_RULE_SCREEN else "none"
if (TD_SCREEN_ON) {
  cat("\n  *** SCREENED RUN: ", TD_SAMPLE_RULE,
      "\n      This is the sensitivity run. Its estimates are NOT the paper's",
      "\n      main estimates, and FINAL_PASS.R will not build the paper from it.\n",
      sep = "")
} else if (nzchar(Sys.getenv("TD_SAMPLE_LO")) || nzchar(Sys.getenv("TD_SAMPLE_HI"))) {
  cat("\n  NOTE: TD_SAMPLE_LO or TD_SAMPLE_HI is set, but TD_SCREEN is off, so\n",
      "  no screen applies. Set TD_SCREEN=on to screen at those bounds.\n",
      sep = "")
}
td_seal("TD_SCREEN", "TD_SCREEN_ON", "TD_SAMPLE_LO", "TD_SAMPLE_HI",
        "TD_SAMPLE_RULE", "TD_SAMPLE_RULE_SCREEN", "TD_SAMPLE_RULE_DEFAULT")

# write_breakdown decides whether screen_removals_by_group.csv is written. NA
# keeps the old behaviour: written when the screen is on and removes rows.
# td_frame() passes TRUE, so with the screen off the rows it would remove are
# written once per fresh frame. Other callers, which may pass a subset of the
# frame, cannot overwrite that count.
td_apply_sample_rule <- function(d, write_breakdown = NA) {
  # READ ANY EXISTING STAMP FIRST, because d is reassigned by the subset
  # below and the attribute would go with it.
  .prev_n <- attr(d, "rows_before_screen")
  .prev_p <- attr(d, "persons_before_screen")
  n0 <- nrow(d)
  p0 <- if ("person_id" %in% names(d)) length(unique(d$person_id)) else NA_integer_
  # SCREENING A SCREENED FRAME MUST NOT OVERWRITE THE STAMP. On 9 September
  # a frame went through this twice, the second pass stamped the post-screen
  # count as the pre-screen one, and \ExtractRows went blank in the paper.
  # The earliest stamp is the true pre-screen count, so it wins.
  .stamp <- function(x) {
    attr(x, "td_sample_rule")        <- TD_SAMPLE_RULE
    attr(x, "rows_before_screen")    <- if (!is.null(.prev_n)) .prev_n else n0
    attr(x, "persons_before_screen") <- if (!is.null(.prev_p)) .prev_p else p0
    attr(x, "screen_rule")           <- TD_SAMPLE_RULE
    x
  }
  if (!all(c("age", "experience_years") %in% names(d))) {
    if (!TD_SCREEN_ON) {
      cat("  sample rule: none | ", format(n0, big.mark = ","), " rows kept",
          " (age or experience_years absent, so nothing is counted)\n", sep = "")
      return(.stamp(d))
    }
    cat("  sample rule: SKIPPED, age or experience_years absent from this\n")
    cat("  frame. The narrow training file drops columns; if this is the\n")
    cat("  frame estimates run on, they are NOT on the analysis sample.\n")
    return(d)
  }
  .gap  <- d$age - d$experience_years
  .miss <- is.na(d$age) | is.na(d$experience_years)
  .lo   <- !.miss & .gap <  TD_SAMPLE_LO
  .hi   <- !.miss & .gap >  TD_SAMPLE_HI
  keep  <- if (TD_SCREEN_ON) !.miss & !.lo & !.hi else rep(TRUE, n0)
  # WHICH BOUND BITES, AND FOR WHOM. This function is the only place that
  # sees the frame on both sides of the rule, so it is the only place that
  # can say what the screen removes, or would remove. The breakdown is by
  # bound and by group, with each group's total, because the rate WITHIN a
  # group is the number the paper has to defend: mothers are most of the
  # removals partly because they are a large group.
  .scr <- NULL
  if (any(.lo | .hi)) {
    .gcol <- intersect(c("grp_life", "group_ever", "grp"), names(d))[1]
    .g <- if (!is.na(.gcol)) as.character(d[[.gcol]]) else
      if ("female" %in% names(d))
        ifelse(d$female == 1, "female", "male") else "all"
    .g[is.na(.g)] <- "unknown"
    .bound <- ifelse(.lo, sprintf("below %g", TD_SAMPLE_LO),
                     ifelse(.hi, sprintf("above %g", TD_SAMPLE_HI), "kept"))
    .drop  <- .bound != "kept"
    .scr <- as.data.frame(table(bound = .bound[.drop], group = .g[.drop]),
                          stringsAsFactors = FALSE)
    names(.scr)[3] <- "person_waves"
    .scr <- .scr[.scr$person_waves > 0, , drop = FALSE]
    .scr$grouping <- if (!is.na(.gcol)) .gcol else "female"
    .tot <- as.data.frame(table(group = .g), stringsAsFactors = FALSE)
    names(.tot)[2] <- "group_person_waves"
    .scr <- merge(.scr, .tot, by = "group", all.x = TRUE)
    .scr$pct_of_group <- round(100 * .scr$person_waves /
                                 .scr$group_person_waves, 2)
    # applied says whether these rows were removed or only counted.
    .scr$applied <- TD_SCREEN_ON
    .scr$rule    <- TD_SAMPLE_RULE_SCREEN
    .scr <- .scr[order(.scr$bound, -.scr$person_waves), , drop = FALSE]
  }
  d <- d[keep, , drop = FALSE]
  if (TD_SCREEN_ON) {
    cat(sprintf("  sample rule: %s | %s -> %s rows (-%s)\n", TD_SAMPLE_RULE,
                format(n0, big.mark = ","), format(nrow(d), big.mark = ","),
                format(n0 - nrow(d), big.mark = ",")))
    cat(sprintf("    below %g: %s | above %g: %s | age or experience NA: %s\n",
                TD_SAMPLE_LO, format(sum(.lo), big.mark = ","), TD_SAMPLE_HI,
                format(sum(.hi), big.mark = ","),
                format(sum(.miss), big.mark = ",")))
  } else {
    cat(sprintf("  sample rule: none | %s rows kept\n",
                format(n0, big.mark = ",")))
    cat(sprintf(paste0("    not applied: %s would remove %s",
                       " (below %g: %s, above %g: %s); age or experience NA,",
                       " kept: %s\n"),
                TD_SAMPLE_RULE_SCREEN, format(sum(.lo | .hi), big.mark = ","),
                TD_SAMPLE_LO, format(sum(.lo), big.mark = ","), TD_SAMPLE_HI,
                format(sum(.hi), big.mark = ","),
                format(sum(.miss), big.mark = ",")))
  }
  .write <- if (is.na(write_breakdown)) TD_SCREEN_ON else isTRUE(write_breakdown)
  if (.write && !is.null(.scr) && nrow(.scr) && exists("DIR_DESIGNS") &&
      dir.exists(DIR_DESIGNS)) {
    print(.scr, row.names = FALSE)
    utils::write.csv(.scr, file.path(DIR_DESIGNS, "screen_removals_by_group.csv"),
                     row.names = FALSE)
    cat("    wrote screen_removals_by_group.csv",
        if (TD_SCREEN_ON) "" else " (rows the screen would remove; none removed)",
        "\n", sep = "")
  }
  d <- .stamp(d)
  # A RULE THAT REMOVES NOTHING, WHEN A SCREEN IS ON, is either a frame
  # already screened or a rule that does not bite, and the stamp is
  # worthless in the first case. With no screen, removing nothing is the
  # point and says nothing about the frame.
  if (TD_SCREEN_ON && n0 == nrow(d) && is.null(.prev_n))
    cat("  *** the rule removed nothing, so this frame was probably screened\n",
        "      before it arrived. rows_before_screen now records the POST-\n",
        "      screen count and \\ExtractRows cannot be written from it.\n",
        "      Rebuild from the panel with no screened frame in the way:\n",
        "        rm(panel); td_forget()\n",
        sep = "")
  d
}

td_frame <- function(need_cols = c("person_id", "wave", "female"),
                     context = "") {
  src <- NULL; d <- NULL; from_registry <- FALSE
  if (td_has("frame")) {
    d <- td_get("frame"); src <- "registry key `frame`"; from_registry <- TRUE
    # v25. STALENESS CHECK. The registry copy is returned whenever it
    # exists, with no reference to the panel underneath, so REBUILDING THE
    # PANEL DID NOT REACH ANY MODULE: every script kept reading the frame
    # cached before the rebuild, the numbers came back identical, and a
    # loader fix looked like it had made no difference.
    #
    # That is the checkpoint failure one level down. CHECKPOINT_TAG guards
    # the wave cache against a loader edit; nothing guarded the frame cache
    # against a panel rebuild.
    #
    # It WARNS rather than refreshing. A silent refresh mid-run would swap
    # the frame under a script that had already resolved it, and the caller
    # has to decide whether the newer panel is the one they meant.
    .fsrc <- attr(d, "td_panel_file"); .fmt <- attr(d, "td_panel_mtime")
    if (is.null(.fsrc) || is.null(.fmt)) {
      cat("  frame: cached before staleness stamping; age NOT verifiable.\n")
      cat("    Clear it once to start stamping: td_forget()\n")
      cat("    (unlink() alone leaves the copy in memory)\n")
    }
    # THE SAME TEST FOR THE SCREEN STAMP, and it is the same failure a level
    # up: this branch guards the cache against a PANEL rebuild, and nothing
    # guarded it against a CONTRACT edit. td_apply_sample_rule() now records
    # rows_before_screen, but it only runs when the frame is built, so a
    # cached frame predating that change comes back screened and unstamped.
    # paper_tables.R then writes PanelRows correctly and no ExtractRows at
    # all, and the manuscript halts on an undefined macro with every count
    # in the file right.
    if (is.null(attr(d, "rows_before_screen"))) {
      cat("  frame: cached before the screen stamp, so the PRE-screen count\n")
      cat("    is unavailable and \\ExtractRows cannot be written. Every\n")
      cat("    post-screen count is still correct; only the extract is\n")
      cat("    missing. Clear the cache once to pick it up:\n")
      cat("      td_forget()   # unlink() alone leaves the memory copy\n")
    } else if (file.exists(.fsrc) && file.mtime(.fsrc) > .fmt) {
      cat("\n", strrep("!", 78), "\n", sep = "")
      cat("STALE FRAME. The panel has been rebuilt since this frame was cached.\n")
      cat("  panel:  ", .fsrc, "\n", sep = "")
      cat("  built:  ", format(file.mtime(.fsrc)), "\n", sep = "")
      cat("  cached: ", format(.fmt), "\n", sep = "")
      cat("  Every module below will read the OLD frame and reproduce the old\n")
      cat("  numbers exactly, which is indistinguishable from a fix that did\n")
      cat("  not matter. Clear the cache and re-run:\n")
      cat("    td_forget()   # unlink() alone leaves the memory copy\n")
      cat("  ev.rds too: it is derived from the frame and is stale with it.\n")
      cat(strrep("!", 78), "\n", sep = "")
    }
    # THE SAME FAILURE A THIRD TIME, and the one that would be silent.
    #
    # The two checks above guard the cache against a panel rebuild and
    # against the screen stamp being absent. Neither guards it against the
    # SAMPLE RULE CHANGING. Now that TD_SAMPLE_HI is a setting, a run with
    # the bound relaxed would find a cached frame screened at 35, return it,
    # and produce the default sample's estimates under a sensitivity run's
    # name. Nothing would look wrong: the counts would be internally
    # consistent and identical to the paper's, which is exactly the outcome
    # a sensitivity run exists to rule out.
    #
    # THIS ONE STOPS. The two above warn, because a newer panel may or may
    # not be the one the caller meant and that is a judgement. A frame
    # screened under a different rule is not a judgement: it is the wrong
    # sample for the rule now in force, and every number downstream would be
    # mislabelled.
    .srule <- attr(d, "td_sample_rule")
    if (!is.null(.srule) && !identical(.srule, TD_SAMPLE_RULE)) {
      cat("\n", strrep("!", 78), "\n", sep = "")
      cat("FRAME SCREENED UNDER A DIFFERENT RULE.\n")
      cat("  cached: ", .srule, "\n", sep = "")
      cat("  now:    ", TD_SAMPLE_RULE, "\n", sep = "")
      cat("  Returning it would give this run the other sample's estimates\n")
      cat("  with no visible sign. Clear the cache and re-run:\n")
      cat("    td_forget()   # unlink() alone leaves the memory copy\n")
      cat("  Before a RUN_ALL.R pass, whose stages are separate processes,\n")
      cat("  delete handoff/frame.rds and handoff/ev.rds instead.\n")
      cat(strrep("!", 78), "\n", sep = "")
      stop("td_frame: cached frame screened under ", .srule,
           ", current rule is ", TD_SAMPLE_RULE, ". td_forget() first.",
           call. = FALSE)
    }
    if (is.null(.srule))
      cat("  frame: cached before rule stamping, so it cannot be checked\n",
          "    against TD_SAMPLE_RULE. td_forget() once to start stamping.\n",
          sep = "")
    rm(.srule)
    rm(.fsrc, .fmt)
  }
  else if (exists("panel", envir = globalenv()) &&
           is.data.frame(get("panel", envir = globalenv()))) {
    d <- get("panel", envir = globalenv()); src <- "in-memory `panel`"
  } else for (f in TD_FRAME_FILES)
    if (file.exists(f)) { d <- readRDS(f); src <- f; break }
  
  if (is.null(d))
    stop("contract: no analysis frame. Run the loader, or td_put(\"frame\", x).",
         call. = FALSE)
  miss <- setdiff(need_cols, names(d))
  if (length(miss))
    stop("contract: the frame from ", src, " lacks ",
         paste(miss, collapse = ", "), ". A narrower file may be in play: ",
         "training_vars is a second select and has silently dropped columns ",
         "that were present on analysis_sample.", call. = FALSE)
  
  # Applied only on a FRESH resolve. A frame returned by the registry has
  # already been through it, and re-filtering would be silent. This rule
  # happens to be idempotent; the next one may not be.
  if (!from_registry) {
    # Refuse to persist a frame that is missing shared columns. A stop here
    # is recoverable in seconds; a narrow frame in handoff/ is a full run
    # that completes and under-reports.
    miss <- setdiff(TD_FRAME_MUST_HAVE, names(d))
    if (length(miss)) {
      cat("\n", strrep("!", 78), "\n", sep = "")
      cat("FRAME REJECTED: ", src, "\n", sep = "")
      cat("  missing shared columns: ", paste(miss, collapse = ", "), "\n", sep = "")
      cat("  This frame would be PERSISTED to handoff/ and inherited by every\n")
      cat("  later script, which cannot object because it has not run yet.\n")
      cat("  The narrow training extract has no education column: it makes T8\n")
      cat("  resolve schooling as NA and 03 finish in seconds.\n")
      cat(strrep("!", 78), "\n", sep = "")
      stop("td_frame: ", src, " lacks ", paste(miss, collapse = ", "),
           ". Resolve the full panel, or add the columns to the extract.",
           call. = FALSE)
    }
    d <- td_apply_sample_rule(d, write_breakdown = TRUE)
    # Stamp the source and its mtime so the check above can fire next time.
    if (!is.null(src) && file.exists(src)) {
      attr(d, "td_panel_file")  <- normalizePath(src, winslash = "/")
      attr(d, "td_panel_mtime") <- file.mtime(src)
    }
  }
  
  bt <- attr(d, "build_tag")
  cat(sprintf("  frame: %s | %s rows, %s persons | build_tag %s%s\n",
              src, format(nrow(d), big.mark = ","),
              format(dplyr::n_distinct(d$person_id), big.mark = ","),
              if (is.null(bt)) "ABSENT" else bt,
              if (nzchar(context)) paste0(" | ", context) else ""))
  
  # PERSIST = TRUE, and the FALSE it replaces was the second half of the same
  # bug. RUN_ORDER invokes the four scripts as separate Rscript processes, so
  # an in-memory registry does not survive between them: each started with an
  # empty .TD, found no `panel`, and fell through to the file. The frame was
  # resolved four times and "once" was a comment.
  #
  # Written to handoff/frame.rds, which is why that directory is deleted
  # before a first run and left alone afterwards.
  if (!td_has("frame")) td_put("frame", d, persist = TRUE)
  d
}

cat("contract loaded: helpers sealed (",
    paste(.TD_SEALED, collapse = ", "), "), registry at ", TD_DIR, "/\n", sep = "")
cat("  root:    ", PROJ_ROOT, "  (from ", PROJ_ROOT_FROM, ")\n", sep = "")
cat("  outputs: ", file.path(PROJ_ROOT, "output"), "\n", sep = "")
