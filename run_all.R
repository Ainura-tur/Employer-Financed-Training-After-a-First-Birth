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
#     02 -> 03a -> 03b -> 03c -> 04 -> master -> referee -> 05
#        -> pre_analysis -> robust -> referee8 -> 06 -> screen -> figures
#        -> tables
#
# It skips the stages that need the raw release or the manuscript: the
# loader, the Wave 24 calendar check, and the number verifiers and their
# reconciliations. Unless SYNTH_FULL=1, 03c fits its DML arms with a quick
# lasso pass and every bootstrap uses 49 replicates. Results are
# illustrative, not the paper's estimates.
#
# REAL MODE follows the authors' verified sequence: the loader (if the panel
# is not on disk), FINAL_PASS.R, then the post-pass stages through RUN_ALL.R
# (robust, referee8, 06, screen, figures, tables, verify, counts), whose
# checks compare every stage's log with the verified run.
#
# LAYOUT. The pipeline sits in R/ and every stage runs with R/ as its working
# directory, because the authors' scripts source one another by bare file
# name and write their outputs beside them. So the panels, output/,
# handoff/, run_logs/ and els-cas-templates/ all appear under R/, and
# .gitignore keeps them out of the repository. The panel check lives in
# tests/. Each stage writes its own log to R/run_logs/; this script also
# keeps each stage's console in R/run_logs/run_all_<stage>_console.log.
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

# ── Every stage runs from R/ ─────────────────────────────────────────────────
setwd(PIPELINE_DIR)
cat(sprintf("Pipeline folder: %s\n", getwd()))

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
    if (!file.exists(f)) stop("Real mode needs ", f, " in R/.", call. = FALSE)
  if (!file.exists(PANEL)) {
    cat("\n==== Building the panel from the restricted release (loader) ====\n")
    RUN_STAGES <- "loader"
    source("RUN_ALL.R")
  }
  cat("\n==== FINAL_PASS.R: 02 to 04, master, referee, 05, figures, tables and the verifier ====\n")
  source("FINAL_PASS.R")
  cat("\n==== Post-pass stages through RUN_ALL.R ====\n")
  RUN_STAGES <- c("robust", "referee8", "06", "screen", "figures", "tables", "verify", "counts")
  source("RUN_ALL.R")
  cat("\nReal-mode pipeline complete.\n")
} else {

# ═════════════════════════════════════════════════════════════════════════════
# SYNTHETIC MODE
# ═════════════════════════════════════════════════════════════════════════════
cat("\n==== Synthetic panel ====\n")
cat("Results on synthetic data are illustrative, NOT the paper's estimates.\n")
if (!file.exists(PANEL) || identical(Sys.getenv("SYNTH_REBUILD"), "1")) {
  # A rebuilt panel makes every file the last run derived from the old one
  # stale, and some stages stop when a stale file disagrees with a new
  # estimate. So the previous synthetic run's outputs go first. Nothing is
  # removed unless the panel on disk is synthetic, and 00b will not
  # overwrite a panel that is not.
  if (file.exists(PANEL) && isTRUE(attr(readRDS(PANEL), "synthetic"))) {
    old <- c("output", "handoff", file.path("els-cas-templates", c("figures", "tables")))
    old <- old[dir.exists(old)]
    if (length(old)) {
      unlink(old, recursive = TRUE)
      cat("  removed the previous synthetic run's outputs: ",
          paste(old, collapse = ", "), "\n", sep = "")
    }
  }
  r <- run_child("00b", "00b_make_synthetic_hilda.R")
  if (r$status != 0) { show_tail(r$console); stop("00b_make_synthetic_hilda.R failed.", call. = FALSE) }
  cat(sprintf("  built in %.1f min\n", r$mins))
} else {
  cat("  ", PANEL, " is on disk; SYNTH_REBUILD=1 rebuilds it.\n", sep = "")
}
r <- run_child("00c", file.path(REPO_ROOT, "tests", "00c_check_synthetic_panel.R"))
if (r$status != 0) { show_tail(r$console); stop("tests/00c_check_synthetic_panel.R failed.", call. = FALSE) }
cat("  schema and invariant checks passed (R/run_logs/run_all_00c_console.log)\n")

# The stage table, in the order of FINAL_PASS.R and the post-pass stages.
# FINAL_PASS.R draws the figures and tables before robust, referee8 and 06,
# and RUN_ALL.R redraws them after; here they run once, at the end. `pre`
# mirrors what the authors' drivers run before each script: the referee
# stage and the figures need the contract in scope, and 05 runs every
# block. TD03_LEDGER_RESET goes to the first ledger contributor, as
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
  # The referee bootstrap draws 1000 replicates in the published run.
  list(id = "referee",  script = "REFEREE_RUN.R",
       pre = paste0('source("00_contract.R"); panel <- td_frame(context = "referee"); ',
                    if (SYNTH_FULL) "" else "BOOT_R <- 49L; ")),
  list(id = "05",       script = "05_review_response.R", pre = 'Sys.setenv(RR_BLOCKS = "all"); '),
  list(id = "pre_analysis", script = "Pre_analysis_diagnostics2.R"),
  list(id = "robust",   script = "robust_30_49.R"),
  # referee8 checks its roster against fixed real-data counts unless
  # R8_ROSTER is set; here it is set from 03a's own roster, so the check
  # still tests that both scripts draw the same roster.
  list(id = "referee8", script = "referee8_couple_clustering.R",
       pre = paste0(if (SYNTH_FULL) "" else "R8_B <- 49L; ",
                    '.ev <- readRDS(file.path("handoff", "ev.rds")); ',
                    "R8_ROSTER <- c(persons = length(unique(.ev$person_id)), ",
                    "treated = length(unique(.ev$person_id[.ev$treated == 1])), ",
                    "rows = nrow(.ev)); rm(.ev); ")),
  list(id = "06",       script = "06_revision_inference.R"),
  # screen_audit.R writes screen_audit_sa.csv, which paper_tables.R reads for
  # the screen comparison in the appendix.
  list(id = "screen",   script = "screen_audit.R"),
  # The figures and tables run as RUN_ALL.R's post-pass runs them, which is
  # the run that leaves the final versions in real mode.
  list(id = "figures",  script = "RUN_FIGURES_PAPER.R", pre = 'source("00_contract.R"); '),
  list(id = "tables",   script = "paper_tables.R"))

# Bootstrap sizes. The published runs use 199 or more replicates. A synthetic
# run only has to exercise the code, so every bootstrap (05, 06, referee and
# referee8) uses 49 unless SYNTH_FULL=1. For 05 and 06 a value already in
# the environment wins.
if (!SYNTH_FULL)
  for (.v in c("REV_BOOT_B", "RR_BOOT_B"))
    if (!nzchar(Sys.getenv(.v))) do.call(Sys.setenv, stats::setNames(list("49"), .v))

res <- list()
for (st in SYN_STAGES) {
  row <- function(status, mins = NA_real_)
    data.frame(stage = st$id, script = st$script, status = status,
               minutes = round(mins, 1), stringsAsFactors = FALSE)
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
# The contract writes figures beside the manuscript, in
# R/els-cas-templates/figures/, and table fragments to R/output/tables/,
# which paper_tables.R copies to R/els-cas-templates/tables/.
figs <- list.files(file.path("els-cas-templates", "figures"), pattern = "\\.(pdf|png)$")
tabs <- list.files(file.path("output", "tables"), pattern = "\\.tex$")
cat(sprintf("\nFigures in R/els-cas-templates/figures/: %d   Table fragments in R/output/tables/: %d\n",
            length(figs), length(tabs)))
cat("Every number these files hold comes from synthetic data and must not be reported.\n")
if (any(out$status %in% c("ERROR", "absent"))) {
  cat("\nNot every stage ran. Read the console log named above for each ERROR,\n",
      "and add any ABSENT script to the repository.\n", sep = "")
  if (!interactive()) quit(save = "no", status = 1)
}
}
