# =============================================================================
# run_all.R
#
# Single entry point for the replication package. Run from the repository
# root:
#
#     Rscript run_all.R                    # synthetic mode (default)
#     DATA_MODE=real Rscript run_all.R     # restricted HILDA release
#
# SYNTHETIC MODE builds the synthetic panel (00b), checks it (00c), and then
# runs the analysis stages in the order the authors' build runs them, each in
# its own R process:
#
#     02 -> 03a -> 03b -> 03c -> 04 -> master -> [referee] -> 05
#        -> pre_analysis -> robust -> referee8 -> 06 -> figures -> tables
#
# It skips the stages that need the raw release or the manuscript: the
# loader, the Wave 24 calendar check, and the number verifiers. 03c fits its
# DML arms with a quick lasso pass, and the referee bootstrap is skipped,
# unless SYNTH_FULL=1. Results are illustrative, not the paper's estimates.
#
# REAL MODE follows the authors' verified sequence: the loader (if the panel
# is not on disk), FINAL_PASS.R, then the post-pass stages through RUN_ALL.R,
# whose checks compare every stage's log with the verified run.
#
# Each stage writes its own log to run_logs/; this script also keeps each
# stage's console in run_logs/run_all_<stage>_console.log.
# =============================================================================

# ── Resolve this file's folder and use it as the working directory ───────────
this_file <- tryCatch({
  args <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", args, value = TRUE)
  if (length(m)) sub("^--file=", "", m[1]) else NA_character_
}, error = function(e) NA_character_)
if (is.na(this_file) && !is.null(sys.frames()[[1]]$ofile))
  this_file <- sys.frames()[[1]]$ofile
repo_root <- if (!is.na(this_file)) normalizePath(dirname(this_file)) else getwd()
setwd(repo_root)
cat(sprintf("Working directory: %s\n", getwd()))

# ── Stage 1: dependencies, then configuration ────────────────────────────────
cat("\n==== PACKAGES.R ====\n")
source("PACKAGES.R")
source("config.R")

RSCRIPT <- file.path(R.home("bin"),
                     if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
if (!file.exists(RSCRIPT)) RSCRIPT <- Sys.which("Rscript")
LOGS <- file.path(getwd(), "run_logs")
dir.create(LOGS, showWarnings = FALSE, recursive = TRUE)

# Run one script in a fresh R process. `pre` is R code the child runs first.
run_child <- function(id, script, pre = "") {
  cap <- file.path(LOGS, sprintf("run_all_%s_console.log", id))
  code <- sprintf('setwd("%s"); SYNTHETIC_DATA <- %s; %ssource("%s", echo = TRUE)',
                  gsub("\\\\", "/", getwd()), if (DATA_MODE == "synthetic") "TRUE" else "FALSE",
                  pre, script)
  t0 <- Sys.time()
  status <- system2(RSCRIPT, c("--no-save", "--no-restore", "-e", shQuote(code)),
                    stdout = cap, stderr = cap)
  list(status = status, mins = as.numeric(difftime(Sys.time(), t0, units = "mins")),
       console = cap)
}
show_tail <- function(cap, n = 15) {
  if (!file.exists(cap)) return(invisible(NULL))
  for (l in utils::tail(readLines(cap, warn = FALSE), n)) cat("      | ", l, "\n", sep = "")
}

PANEL <- "hilda_panel_data_W12_W24_slim.rds"

# ═════════════════════════════════════════════════════════════════════════════
# REAL MODE
# ═════════════════════════════════════════════════════════════════════════════
if (DATA_MODE == "real") {
  for (f in c("RUN_ALL.R", "FINAL_PASS.R", "LOADING_ND3_v3.R"))
    if (!file.exists(f)) stop("Real mode needs ", f, " in the repository root.", call. = FALSE)
  if (!file.exists(PANEL)) {
    cat("\n==== Building the panel from the restricted release (loader) ====\n")
    RUN_STAGES <- "loader"
    source("RUN_ALL.R")
  }
  cat("\n==== FINAL_PASS.R: 02, 03a-03c, 04, master, referee, 05 and the tables ====\n")
  source("FINAL_PASS.R")
  cat("\n==== Post-pass stages through RUN_ALL.R ====\n")
  RUN_STAGES <- c("robust", "referee8", "06", "figures", "tables", "verify", "counts")
  source("RUN_ALL.R")
  cat("\nReal-mode pipeline complete.\n")
} else {

# ═════════════════════════════════════════════════════════════════════════════
# SYNTHETIC MODE
# ═════════════════════════════════════════════════════════════════════════════
cat("\n==== Synthetic panel ====\n")
cat("Results on synthetic data are illustrative, NOT the paper's estimates.\n")
if (!file.exists(PANEL) || identical(Sys.getenv("SYNTH_REBUILD"), "1")) {
  r <- run_child("00b", "00b_make_synthetic_hilda.R")
  if (r$status != 0) { show_tail(r$console); stop("00b_make_synthetic_hilda.R failed.", call. = FALSE) }
  cat(sprintf("  built in %.1f min\n", r$mins))
} else {
  cat("  ", PANEL, " is on disk; SYNTH_REBUILD=1 rebuilds it.\n", sep = "")
}
r <- run_child("00c", "00c_check_synthetic_panel.R")
if (r$status != 0) { show_tail(r$console); stop("00c_check_synthetic_panel.R failed.", call. = FALSE) }
cat("  schema and invariant checks passed (run_logs/run_all_00c_console.log)\n")

# The stage table, in the order of FINAL_PASS.R and the post-pass stages.
# `pre` mirrors what the authors' drivers run before each script: the
# referee stage and the figures need the contract in scope, and 05 runs
# every block. TD03_LEDGER_RESET goes to the first ledger contributor, as
# RUN_ALL.R does on a full pass. Pre_analysis_diagnostics2.R writes
# headline_figures.csv, which paper_tables.R reads.
SYN_STAGES <- list(
  list(id = "02",       script = "02fte_experience.R"),
  list(id = "03a",      script = "03a_design_audit.R", pre = "TD03_LEDGER_RESET <- TRUE; "),
  list(id = "03b",      script = "03b_fee_descriptives.R"),
  list(id = "03c",      script = "03c_experience_units.R",
       pre = if (SYNTH_FULL) "" else "DML_FAST <- TRUE; DML_REPS <- 1L; DML_FOLDS <- 2L; "),
  list(id = "04",       script = "04financing_space.R"),
  list(id = "master",   script = "MASTER_training_y.R"),
  list(id = "referee",  script = "REFEREE_RUN.R", slow = TRUE,
       pre = 'source("00_contract.R"); panel <- td_frame(context = "referee"); '),
  list(id = "05",       script = "05_review_response.R", pre = 'Sys.setenv(RR_BLOCKS = "all"); '),
  list(id = "pre_analysis", script = "Pre_analysis_diagnostics2.R"),
  list(id = "robust",   script = "robust_30_49.R"),
  list(id = "referee8", script = "referee8_couple_clustering.R"),
  list(id = "06",       script = "06_revision_inference.R"),
  list(id = "figures",  script = "RUN_FIGURES_PAPER.R", pre = 'source("00_contract.R"); '),
  list(id = "tables",   script = "paper_tables.R"))

res <- list()
for (st in SYN_STAGES) {
  row <- function(status, mins = NA_real_)
    data.frame(stage = st$id, script = st$script, status = status,
               minutes = round(mins, 1), stringsAsFactors = FALSE)
  if (isTRUE(st$slow) && !SYNTH_FULL) {
    cat(sprintf("\n[%s] %s  skipped (slow; SYNTH_FULL=1 runs it)\n", st$id, st$script))
    res[[st$id]] <- row("skipped"); next
  }
  if (!file.exists(st$script)) {
    cat(sprintf("\n[%s] %s  ABSENT from the repository\n", st$id, st$script))
    res[[st$id]] <- row("absent"); next
  }
  cat(sprintf("\n[%s] %s\n", st$id, st$script))
  r <- run_child(st$id, st$script, if (is.null(st$pre)) "" else st$pre)
  ok <- identical(as.integer(r$status), 0L)
  cat(sprintf("      %s in %.1f min\n", if (ok) "ok" else "ERROR", r$mins))
  res[[st$id]] <- row(if (ok) "ok" else "ERROR", r$mins)
  if (!ok) {
    cat("      last lines of ", basename(r$console), ":\n", sep = ""); show_tail(r$console)
    if (!SYNTH_KEEP_GOING) {
      cat("      stopping: later stages read this stage's output. ",
          "SYNTH_KEEP_GOING=1 runs them anyway.\n", sep = "")
      break
    }
  }
}

out <- do.call(rbind, res)
cat("\n", strrep("=", 70), "\nSUMMARY (synthetic mode)\n", strrep("=", 70), "\n", sep = "")
print(out, row.names = FALSE)
utils::write.csv(out, file.path(LOGS, paste0("run_all_", format(Sys.time(), "%Y%m%d_%H%M%S"),
                                             ".csv")), row.names = FALSE)
figs <- list.files(file.path("output", "figures"), pattern = "\\.(pdf|png)$")
tabs <- list.files(file.path("output", "tables"), pattern = "\\.tex$")
cat(sprintf("\nFigures written: %d   Table fragments written: %d\n", length(figs), length(tabs)))
cat("Every number these files hold comes from synthetic data and must not be reported.\n")
if (any(out$status %in% c("ERROR", "absent"))) {
  cat("\nNot every stage ran. Read the console log named above for each ERROR,\n",
      "and add any ABSENT script to the repository.\n", sep = "")
  if (!interactive()) quit(save = "no", status = 1)
}
}
