# =====================================================================
# paths.R  -  where the project is, said once
# ---------------------------------------------------------------------
# Source this FIRST, before anything else, from any working directory.
#
# WHY IT EXISTS. Each script that guesses its own paths guesses
# differently, and the ones that guess wrong fail quietly, by writing
# somewhere plausible rather than by stopping. So the root is asserted
# once here and everything hangs off it.
#
# THE ROOT IS WG/TG. It was WG until 1 September, and the move is why
# this file and 00_contract.R briefly disagreed: the contract took
# PROJ_ROOT from HILDA_ROOT, which had been set to TG, while this file
# derived project_dir from the working directory and answered WG. Both
# assigned OUTDIR, so which survived depended on load order, and 03a,
# 03b and 03c wrote their ledgers to TG/output/designs while
# paper_tables.R and verify_manuscript_numbers.R read the four-day-old
# copies in WG/output/designs.
#
# The default below is the root, written down. HILDA_ROOT still wins if
# it is set, because the contract reads that variable and the two must
# not be able to differ; setting it is how the tree moves to another
# machine without editing this file.
# =====================================================================

# ONE ROOT, AND HILDA_ROOT DECIDES IT.
#
# This used to derive project_dir from the working directory alone: parent
# when the directory was named TG, getwd() otherwise. 00_contract.R derives
# PROJ_ROOT from Sys.getenv("HILDA_ROOT") instead, and once that variable was
# set to .../WG/TG the two disagreed. paths.R said WG and the contract said
# TG, both assigned OUTDIR, and which one survived depended on whether the
# contract loaded after this file. On 1 September that showed up as 03a, 03b
# and 03c writing their ledgers to TG/output/designs while paper_tables.R and
# verify_manuscript_numbers.R read WG/output/designs, four days stale. The
# verifier would then have compared the manuscript against an old run and
# reported mismatches that were not real.
#
# So the environment variable governs here too, and the TG rule survives only
# as the fallback for a session that has not set it. The contract stops if the
# root does not exist; this warns and carries on, because the paths it prints
# are the diagnosis.
# THE ROOT, AND THE MANUSCRIPT, WRITTEN DOWN. Everything else is derived.
# Change these two lines to move the tree; nothing else in any script
# should carry an absolute path.
#
# 25 SEPTEMBER: OFF ONEDRIVE. Sync locks stopped runs mid-pass, so the
# project now lives in C:/Users/tur277/TG and the OneDrive tree under
# Desktop/WG/TG is retired. These two lines still pointed there, and the
# export below then set HILDA_ROOT to the old tree in any session that
# sourced this file, so a session running in C:/Users/tur277/TG split
# across the two copies until 00_contract.R stopped it.
ROOT_DEFAULT  <- "C:/Users/tur277/TG"
PAPER_DEFAULT <- "C:/Users/tur277/TG/els-cas-templates"

project_dir <- local({
  .e <- Sys.getenv("HILDA_ROOT")
  .p <- if (nzchar(.e)) .e else ROOT_DEFAULT
  if (!dir.exists(.p))
    stop("The project root does not exist: ", .p,
         "\n  Set HILDA_ROOT, or fix ROOT_DEFAULT in paths.R. Nothing below ",
         "is meaningful until this is right.", call. = FALSE)
  normalizePath(.p, winslash = "/", mustWork = FALSE)
})

# HILDA_ROOT IS EXPORTED IF IT WAS NOT SET, so 00_contract.R derives the
# same PROJ_ROOT from the same value. Without this the contract falls back
# to its own hard-coded default, and the two can disagree again in exactly
# the way that split the ledgers.
if (!nzchar(Sys.getenv("HILDA_ROOT"))) {
  Sys.setenv(HILDA_ROOT = project_dir)
  cat("HILDA_ROOT was unset; exported as ", project_dir,
      "\n  so 00_contract.R derives the same root.\n", sep = "")
} else if (!identical(normalizePath(Sys.getenv("HILDA_ROOT"), winslash = "/",
                                    mustWork = FALSE), project_dir)) {
  cat("NOTE: HILDA_ROOT is set and differs from ROOT_DEFAULT. The variable\n",
      "      wins, here and in the contract.\n", sep = "")
}

OUTDIR    <- file.path(project_dir, "output", "designs")
TD_DIR    <- file.path(project_dir, "handoff")

# THE MANUSCRIPT'S INPUTS SIT BESIDE THE MANUSCRIPT, not under project_dir.
# \input and \includegraphics both resolve relative to the .tex being
# compiled, and here the scripts and the paper are in a subdirectory of the
# data root, so tables/ and figures/ belong next to them. All eight generated
# tables go to one folder and all four figures to another, which is what makes
# them collectable.
# THE MANUSCRIPT IS FOUND, AND tables/ AND figures/ SIT BESIDE IT.
# \input{tables/...} and \includegraphics{figures/...} resolve relative to
# the .tex being compiled, so those directories belong wherever the .tex is.
# It is not beside the scripts: it lives in els-cas-templates under the
# root (on the retired OneDrive tree it was wg_latex/els-cas-templates),
# which is why an earlier compile could not find
# tables/tab_t11_investment.tex while the file existed.
#
# Searched rather than hard-coded, because the name has been _res, _cut,
# _fixed and _v2 and every consumer that hard-coded one of those has since
# failed to find it. MANUSCRIPT and TEXFILE are the same value under the two
# names the consumers use: RUN_ALL.R's figure check reads MANUSCRIPT,
# verify_manuscript_numbers.R reads TEXFILE.
# THE MANUSCRIPT IS SEARCHED FOR BY PATTERN, because
# the name has been _res, _cut, _fixed and _v2 and every consumer that
# hard-coded one of those stopped finding it. PAPER_DEFAULT above is the
# directory; the recursive search under project_dir stays as a fallback for
# a tree where the paper does sit inside the root.
MANUSCRIPT <- local({
  .c <- character(0)
  if (dir.exists(PAPER_DEFAULT))
    .c <- list.files(PAPER_DEFAULT, pattern = "^Motherhood_training.*\\.tex$",
                     full.names = TRUE)
  if (!length(.c))
    .c <- list.files(project_dir, pattern = "^Motherhood_training.*\\.tex$",
                     recursive = TRUE, full.names = TRUE)
  .c <- .c[!grepl("/(superseded|run_logs)/", .c)]
  if (!length(.c)) return(NA_character_)
  .c[order(file.info(.c)$mtime, decreasing = TRUE)][1]
})
TEXFILE <- MANUSCRIPT
TEXROOT <- if (is.na(MANUSCRIPT)) getwd() else dirname(MANUSCRIPT)

# v2. TEXDIR AND FIGDIR ARE THE CONTRACT'S, NOT THE MANUSCRIPT'S.
#
# These pointed at TEXROOT/tables and TEXROOT/figures, the manuscript's own
# folders, while 00_contract.R seals DIR_TABLES and DIR_FIGURES to
# PROJ_ROOT/output/{tables,figures}. So there were two `tables` directories
# and two `figures` directories, the sealed ones sat empty, and an artefact
# reported as written could not be found because it was in the other one.
#
# WHY THE GENERATORS NO LONGER WRITE INTO THE PAPER. paper_tables.R wrote
# app_b2_profile.tex straight into the manuscript's folder with every cell
# set to \tofill, because the session objects it needed were absent. Six rows
# of real estimates were replaced by placeholders, the paper still compiled,
# and nothing between the generator and the page could have caught it. There
# was no such moment. Building into output/ and copying afterwards creates
# one, and sync_paper() below is where the check lives.
#
# PAPER_TEX and PAPER_FIG are the destination, kept separate so nothing can
# write to them by accident: they are not called TEXDIR.
TEXDIR <- if (exists("DIR_TABLES"))  DIR_TABLES  else
  file.path(project_dir, "output", "tables")
# The fallback matches the contract's DIR_FIGURES, which is PROJ_ROOT/figures
# rather than PROJ_ROOT/output/figures. Tables stay under output/ and figures
# do not, which is asymmetric on purpose: figures are opened by hand and the
# ledgers are not. If these two ever disagree, td_out("figures", ...) and
# FIGDIR write to different places and a figure reported as written is not
# where the sync looks for it.
FIGDIR <- if (exists("DIR_FIGURES")) DIR_FIGURES else
  file.path(project_dir, "figures")
PAPER_TEX <- file.path(TEXROOT, "tables")
PAPER_FIG <- file.path(TEXROOT, "figures")
for (.d in c(TEXDIR, FIGDIR, PAPER_TEX, PAPER_FIG))
  if (!dir.exists(.d)) dir.create(.d, recursive = TRUE)

# ---------------------------------------------------------------------
# sync_paper()  -  copy the built artefacts into the manuscript's tree
#
# CALLED BY THE GENERATORS, not by them writing there directly. It refuses
# to copy a fragment containing \tofill, because a placeholder reaching the
# paper is silent: the file compiles, the table renders blank, and the
# artefact report says "rebuilt". Refusing leaves the previous good copy in
# place, which is the outcome anyone would have chosen if asked.
#
# It reports every decision. A sync that copies quietly is the same
# unwatched step this replaced.
# ---------------------------------------------------------------------
sync_paper <- function(what = c("tables", "figures"), quiet = FALSE) {
  what <- match.arg(what, c("tables", "figures"), several.ok = TRUE)
  .n_ok <- .n_skip <- 0L
  for (w in what) {
    .from <- if (w == "tables") TEXDIR else FIGDIR
    .to   <- if (w == "tables") PAPER_TEX else PAPER_FIG
    .pat  <- if (w == "tables") "[.]tex$" else "[.](pdf|png)$"
    .f <- list.files(.from, .pat, full.names = TRUE)
    if (!length(.f)) next
    for (p in .f) {
      .bad <- w == "tables" &&
        any(grepl("tofill", readLines(p, warn = FALSE), fixed = TRUE))
      if (.bad) {
        if (!quiet)
          cat("  *** NOT SYNCED:", basename(p),
              "contains \\tofill. The copy already in the paper is kept.\n")
        .n_skip <- .n_skip + 1L
      } else {
        file.copy(p, file.path(.to, basename(p)), overwrite = TRUE)
        .n_ok <- .n_ok + 1L
      }
    }
  }
  if (!quiet)
    cat(sprintf("  sync_paper: %d copied, %d withheld -> %s\n",
                .n_ok, .n_skip, TEXROOT))
  invisible(c(copied = .n_ok, withheld = .n_skip))
}

# FS_OUTDIR IS DELIBERATELY NOT SET HERE. RUN_ALL.R sets it for the
# financing stage and clears it immediately after, which is the discipline
# that stops a value leaking into a stage that did not ask for it. Setting
# it in the session defeats that and makes RUN_ALL open with
# "*** FLAGS ALREADY SET IN THIS SESSION". Set it by hand only when running
# 04financing_space.R on its own, outside the pipeline.
PANEL     <- file.path(project_dir, "hilda_panel_data_W12_W24_slim.rds")
ADDENDUM  <- file.path(project_dir, "output", "fte_experience",
                       "panel_fte_addendum.rds")
# WHERE paper_tables.R WRITES IT, which is TEXDIR. Since v2 that is
# output/tables and the manuscript's copy arrives by sync_paper(); the
# fingerprint checks read the built one, which is the one just written.
# This named project_dir, where
# an older run left a copy, so the fingerprint checks in 02fte_experience.R
# and 03_common.R would have read a stale file and reported the introduction
# disagreeing with a build it actually matches.
MACROS    <- file.path(TEXDIR, "data_section_numbers.tex")

for (.d in c(OUTDIR, dirname(ADDENDUM)))
  if (!dir.exists(.d)) dir.create(.d, recursive = TRUE)

cat("project_dir : ", project_dir, "\n", sep = "")
cat("scripts in  : ", getwd(), "\n", sep = "")
for (.n in c("PANEL", "ADDENDUM", "MACROS")) {
  .p <- get(.n)
  cat(sprintf("%-12s: %s  %s\n", .n, .p,
              if (file.exists(.p)) "found" else "*** MISSING ***"))
}
cat("OUTDIR      : ", OUTDIR, "\n", sep = "")
cat("MANUSCRIPT  : ", if (is.na(MANUSCRIPT)) "*** NOT FOUND" else MANUSCRIPT,
    "\n", sep = "")
cat("TEXDIR      : ", TEXDIR, "   (built here)\n", sep = "")
cat("FIGDIR      : ", FIGDIR, "   (built here)\n", sep = "")
cat("PAPER_TEX   : ", PAPER_TEX, "   (synced here)\n", sep = "")
cat("PAPER_FIG   : ", PAPER_FIG, "   (synced here)\n", sep = "")
if (is.na(MANUSCRIPT))
  cat("  No Motherhood_training*.tex under ", project_dir, ".\n",
      "  TEXDIR and FIGDIR fall back to the working directory, and the\n",
      "  manuscript will not find what they hold. Set MANUSCRIPT by hand.\n",
      sep = "")
rm(.n, .p)
