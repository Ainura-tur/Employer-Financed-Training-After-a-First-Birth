# =============================================================================
# config.R
#
# The settings a replicator may need to change, in one place. run_all.R
# sources this file first. Each pipeline stage then runs in its own R process
# and inherits the environment variables set here.
#
# Run contract: run everything from the repository root. The pipeline's
# 00_contract.R resolves every path from PROJ_ROOT, which it reads from the
# HILDA_ROOT environment variable; this file points HILDA_ROOT at the
# repository unless you have set it yourself.
# =============================================================================

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

REPO_ROOT <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

# ── Data mode ────────────────────────────────────────────────────────────────
# "synthetic" (default): 00b_make_synthetic_hilda.R writes a synthetic panel
#   with the real panel's schema, so the pipeline runs without the restricted
#   HILDA release. Results are illustrative and are NOT the paper's estimates.
# "real": the panel comes from the restricted release through
#   LOADING_ND3_v3.R. Only approved HILDA users can run this mode.
# Override without editing: DATA_MODE=real Rscript run_all.R
DATA_MODE <- tolower(Sys.getenv("DATA_MODE", unset = "synthetic"))
if (!DATA_MODE %in% c("synthetic", "real"))
  stop(sprintf("config.R: DATA_MODE must be 'synthetic' or 'real', not '%s'.", DATA_MODE),
       call. = FALSE)
Sys.setenv(DATA_MODE = DATA_MODE)

# ── Project root for the pipeline's contract ─────────────────────────────────
# 00_contract.R stops when the working directory holds 00_contract.R but
# PROJ_ROOT names another folder, so the two must agree.
.hr <- Sys.getenv("HILDA_ROOT")
if (!nzchar(.hr)) {
  Sys.setenv(HILDA_ROOT = REPO_ROOT)
} else if (!identical(tolower(normalizePath(.hr, winslash = "/", mustWork = FALSE)),
                      tolower(REPO_ROOT))) {
  message("config.R: HILDA_ROOT is set to ", .hr, ", not this repository (",
          REPO_ROOT, ").\n  The pipeline will read and write there. Unset ",
          "HILDA_ROOT to use this folder.")
}
rm(.hr)

# ── Raw HILDA release (real mode only) ───────────────────────────────────────
# The folder holding the General Release 24 Stata files. The loader reads it
# from HILDA_DATA_DIR; data/raw/ in this repository is the default.
HILDA_DATA_DIR <- Sys.getenv("HILDA_DATA_DIR",
                             unset = file.path(REPO_ROOT, "data", "raw"))
Sys.setenv(HILDA_DATA_DIR = HILDA_DATA_DIR)
if (DATA_MODE == "real" && !dir.exists(HILDA_DATA_DIR))
  message("config.R: no raw data folder at ", HILDA_DATA_DIR,
          ". Set HILDA_DATA_DIR before the loader runs.")

# ── Synthetic panel size and seed ────────────────────────────────────────────
# Passed to 00b_make_synthetic_hilda.R. Larger panels run slower but give the
# designs more treated parents in each cohort.
if (!nzchar(Sys.getenv("N_PERSONS")))  Sys.setenv(N_PERSONS = "6000")
if (!nzchar(Sys.getenv("SYNTH_SEED"))) Sys.setenv(SYNTH_SEED = "20261021")

# ── Synthetic run options ────────────────────────────────────────────────────
# SYNTH_FULL=1 also runs the stages that take hours on any panel (the
# referee bootstrap) and fits 03c's twenty DML arms at the published
# settings instead of a quick lasso pass.
# SYNTH_KEEP_GOING=1 runs every stage even after one fails.
SYNTH_FULL       <- identical(Sys.getenv("SYNTH_FULL"), "1")
SYNTH_KEEP_GOING <- identical(Sys.getenv("SYNTH_KEEP_GOING"), "1")

# ── Threads ──────────────────────────────────────────────────────────────────
# The pipeline reads MASTER_HH_THREADS (or SLURM_CPUS_PER_TASK) and otherwise
# caps itself at four threads.

cat(sprintf("config.R: DATA_MODE = %s | HILDA_ROOT = %s\n", DATA_MODE,
            Sys.getenv("HILDA_ROOT")))
if (DATA_MODE == "real") cat(sprintf("config.R: HILDA_DATA_DIR = %s\n", HILDA_DATA_DIR))
