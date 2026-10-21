# =============================================================================
# RUN_FIGURES_PAPER.R  -  the manuscript's figures, and nothing else
# -----------------------------------------------------------------------------
# WAS TWO FILES. RUN_FIGURES_EDA.R was 2,280 lines across sections A to G plus
# a Section P of manuscript candidates, and RUN_FIGURES_PAPER.R was a settings
# wrapper that sourced it. Folded, with A to G removed, this is what the paper
# actually needs.
#
# WHAT THE MANUSCRIPT INCLUDES, checked against \includegraphics in
# Motherhood_training_v5.tex rather than assumed:
#
#   P3_margin_split              extensive against intensive, Section 6.1
#   P4_wage_sample_selection     wage-sample coverage by event time, Section 4
#   alt_T1_ordering_pooled       investment against price, Section 7.2
#   alt_T1_which_side_pooled     mothers against fathers, Section 7.2
#   P2_training_experience_profile   COMMENTED OUT in the manuscript at the
#                                moment, kept here because the float and its
#                                caption are still in the .tex ready to be
#                                uncommented, and because the block warns when
#                                the conditional gap changes sign
#   SA_financing_contrast        the financing contrast as an event study,
#                                Section 7.1. v28: new, so the .tex draws it
#                                only once its float is pasted in. Read from
#                                06_revision_inference.R's output and written
#                                only when 06's contrast checks out
#
# v26. THE INVENTORY HAD THE PAIR BACKWARDS, and said so in one place while
# contradicting itself in two others. It listed T1_ordering and
# T1_which_side_moves as the manuscript's Section 7.2 figures and described
# the alt_ pair as "not included by the manuscript". The .tex draws
# alt_T1_ordering_pooled and alt_T1_which_side_pooled and draws neither T1_
# figure: those are its only two event-study \includegraphics lines. Both
# TRAIN_MEASURES (the note field on the second measure) and the manifest at
# the foot of this file already said the alt_ pair is the one drawn and is
# required rather than optional, so the header was the last copy still
# wrong. Corrected here, and the two statements below now agree with it.
#
# ALSO WRITTEN, and not drawn by the manuscript: T1_ordering and
# T1_which_side_moves, the same two figures on the workplace item
# (jttpewt_any) that carries every table and every triple difference in the
# paper. They are not diagnostics kept for curiosity. They are the arm the
# text quotes, so the two must be produced together and compared: the run
# report after the ordering loop is what makes the comparison, and
# Section 4.3 of the .tex cites it.
#
# The two T1 figures were Fig_ordering.R and Fig_which_side_moves.R and were
# folded in at v21.47. Neither file is used any more, and the inventory above
# said otherwise for two versions after the fold.
#
# v-cw. COMMON COHORT WEIGHTS. Every women-minus-men series drawn here is the
# difference formed inside each cohort-by-event cell and averaged on pooled
# cell counts (sa_common_weights.R). This file computes none of it: it reads
# gap and gap_se from training_decomposition.csv and timing_paths.csv, which
# MASTER_training_y.R now fills from the common-weight series, and the
# financing path from 06_revision_inference.R, which forms it the same way.
# The one place the old construction could leak in was the which-side band,
# drawn from the fathers' line to the mothers' line: that is each arm on its
# own weights. It is now drawn as the common-weight gap measured from the
# fathers' line, and the log reports how far the two constructions differ.
#
# P1_interruption_by_group is NOT in the manuscript and is not kept. It was in
# an earlier compile as Figure 2 and the text has since diverged from it.
#
# WHAT WAS DROPPED, AND WHAT THAT COSTS. Sections A to G were diagnostics and
# their findings are already written into the data section: the hours mixture
# and its percentiles (C), the wave-20 break (E), missingness by group (F),
# the sector and union panels (G), overlap and functional form (A, B), and the
# event-time composition checks (D). Those numbers are in the paper and cannot
# now be regenerated from this script. If the panel changes, they are the ones
# to re-derive, and the old file in version control is where to get them.
#
# ONE FIGURES FOLDER. figures/ holds everything. Exploratory renders, which
# this file only produces when PUB is FALSE, take an explore_ prefix so that
# a missing publication PDF can never let \includegraphics resolve to a
# half-sample panel captioned "Not for publication".
#
#   source("RUN_FIGURES_PAPER.R")      # or Rscript RUN_FIGURES_PAPER.R
#
# The settings below are in the file rather than left to the session, because
# RUN_FIGURES_EDA.R took its flags from the caller and `Rscript` starts with
# an empty environment: it defaulted every flag, succeeded, and wrote
# exploratory PNGs with the caption burned in without reporting an error.
# =============================================================================

# ---- settings, asserted here so an invocation cannot default them ----------
if (!exists("PUB"))        PUB        <- TRUE
if (!exists("HALF"))       HALF       <- "both"
if (!exists("PUB_ONLY"))   PUB_ONLY   <- TRUE
if (!exists("PUB_FORMAT")) PUB_FORMAT <- "pdf"
# PUBDIR FOLLOWS FIGDIR, which paths.R sets beside the manuscript. Defaulting
# it to the bare string "figures" resolved against the working directory, so
# svp() wrote P3 and P4 into TG/figures while the four ggsave figures went to
# FIGDIR under wg_latex. That is the split the note at the FIGDIR line warns
# about, arriving from the other side: on 2 September the contract reported
# the T1_* pair as 31 August because it was looking where they no longer were.
if (!exists("PUBDIR"))
  # v26. THE FALLBACK WAS A RELATIVE PATH AND THAT IS WHERE P2, P3 AND P4
  # HAVE BEEN GOING. FIGDIR is not resolved until line ~1397, hundreds of
  # lines below this one, so in a session that has not already set it this
  # test is FALSE and PUBDIR became "figures" relative to the working
  # directory, which is PROJ_ROOT. The manuscript reads
  # els-cas-templates/figures, so the P blocks ran, wrote three current
  # figures to TG/figures, and the staleness check at the foot compared
  # against FIGDIR and correctly called the manuscript's copies stale.
  # Three figures of the new panel under captions written for it, sitting
  # in a folder nothing compiles.
  #
  # THE CONTRACT KNOWS THE ANSWER BEFORE FIGDIR IS COMPUTED. DIR_FIGURES
  # and PAPER_FIGURES are both sealed at load and both resolve to the
  # manuscript's own figures folder, so they are available here. A relative
  # path stays as the last resort for a run with no contract at all, and it
  # is named in the console so it cannot pass unnoticed again.
  PUBDIR <- if (exists("FIGDIR", envir = globalenv()))
    get("FIGDIR", envir = globalenv()) else
      if (exists("DIR_FIGURES") && nzchar(DIR_FIGURES)) DIR_FIGURES else
        if (exists("PAPER_FIGURES") && nzchar(PAPER_FIGURES)) PAPER_FIGURES else
          "figures"

cat("\n", strrep("=", 70), "\n  MANUSCRIPT FIGURES\n", strrep("=", 70), "\n",
    sep = "")
cat(sprintf("  PUB %s | HALF %s | format %s -> %s/\n",
            PUB, HALF, PUB_FORMAT, PUBDIR))
if (isTRUE(PUB))
  cat("  Drawing on the FULL sample, confirmation half included. If the\n",
      "  specification is not yet fixed, set PUB <- FALSE and run again.\n",
      sep = "")


# RUN_FIGURES_explore.R
#
# Exploratory figures, to INFORM the master scripts. Nothing here goes in
# the paper.
#
# -----------------------------------------------------------------------------
# THE ONE RULE THIS SCRIPT ENFORCES: IT RUNS ON THE DISCOVERY HALF ONLY.
#
# Choosing a specification after looking at the data, then estimating it on
# the same data, is the garden of forking paths, and it is exactly how a
# project talks itself into a result that will not replicate. This project
# has already been bitten once: PAPER_ATTENTION 2.22 records a causal forest
# whose variable-importance rankings agreed across halves while its
# predictions ordered people OPPOSITELY, and the ranking agreement read as
# replication until the out-of-sample effect difference was computed
# (+0.0025, SE 0.0567, against an MDE of 0.111).
#
# So: persons are hashed into two halves by person_id, and everything below
# uses the DISCOVERY half. The confirmation half is never touched here and
# should not be looked at until the specification is fixed. The hash matches
# the one used in the forest work so the halves are the same people.
#
# What that buys: a shape seen here can be turned into a modelling decision
# (a spline knot, a subgroup, a functional form) and then estimated on the
# confirmation half honestly. What it does not buy: licence to report
# anything from this script as a finding.
#
# -----------------------------------------------------------------------------
# NO P-VALUES, AND THAT IS DELIBERATE.
#
# Every panel here is a picture or a count. There are no tests, no stars, no
# confidence bands on fitted lines. A p-value computed on data you are
# eyeballing is not a p-value, and printing one invites it to be quoted.
# Bands that DO appear are binomial or bootstrap intervals on the plotted
# means, which describe how well the BIN is estimated, not whether an effect
# exists.
#
# -----------------------------------------------------------------------------
# WHAT EACH SECTION IS FOR
#
#   A. Overlap. Who is comparable to whom. If mothers and childless women do
#      not share support on experience or education, no estimator fixes that
#      and the event study is extrapolating.
#   B. Functional form. The three non-linearities PAPER_ATTENTION already
#      documents (2.8 hump-shaped training-wage profile, 2.9 concave
#      experience profile, 2.19 the cross-partial falling 3.3-fold), drawn
#      so the modelling choice is made on the shape rather than by default.
#   C. Outcome distributions. Why total_courses and not
#      total_training_hours_z: the hours tail is the whole story. C5 is
#      the fee item, which is a training measure rather than an hours
#      one and sits here because C is where the measures are examined.
#   D. Event-time composition. Which people occupy each k cell, since a
#      coefficient path can move because the outcome moves or because the
#      sample does.
#   E. The wave-20 break, which is confounded between a questionnaire
#      renumbering and the pandemic and cannot be separated.
#   F. Coverage and missingness, by wave and by group.
#   P. Candidate MANUSCRIPT figures, gated behind PUB. Four claims the
#      paper currently asserts in prose with no figure behind them.
#
# Writes to figures_explore/. Nothing overwrites a paper figure: Section P
# writes to figures_paper/ and only when PUB is TRUE, which additionally
# requires HALF == "both" so a half-sample panel cannot reach the paper.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(tidyr)
})

# =====================================================================
# LOG
#
# v2. THE FIGURES SCRIPT KEPT NO RECORD EITHER. It prints which figures
# it wrote, which it skipped for want of an input, and the settings each
# was drawn under, and none of that survived the session. A figure whose
# provenance is a console line is a figure nobody can date, and this file
# writes into the paper's own figures directory.
#
# Same discipline as the rest of the pipeline: own the sink only when
# nothing else does, record the incoming depth, unwind to it.
# =====================================================================
.fg_sink_before <- sink.number()
.fg_msg_before  <- sink.number(type = "message")
.fg_owns <- (.fg_sink_before == 0L)
.fg_log  <- NA_character_
if (.fg_owns) {
  .fg_dir <- if (exists("LOG_DIR")) LOG_DIR else
    file.path(if (exists("PROJ_ROOT")) PROJ_ROOT else getwd(), "run_logs")
  dir.create(.fg_dir, showWarnings = FALSE, recursive = TRUE)
  .fg_log <- file.path(.fg_dir, paste0(
    "RUN_FIGURES_PAPER_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
  sink(.fg_log, split = TRUE)
  cat("Log: ", .fg_log, "\n", sep = "")
  cat("  (standalone run; this file owns the sink and will close it)\n")
  cat("  If a figure aborts, restore the console with: fg_log_close()\n")
} else {
  cat("Log: inheriting a sink already open (", .fg_sink_before,
      " output). This run is\n  written into whatever is already recording.\n",
      sep = "")
}
fg_log_close <- function() {
  if (!isTRUE(get0(".fg_owns", ifnotfound = FALSE))) {
    cat("fg_log_close(): this file does not own a sink; nothing to do.\n")
    return(invisible(NULL))
  }
  while (sink.number(type = "message") > .fg_msg_before) sink(type = "message")
  while (sink.number() > .fg_sink_before) sink()
  cat("Log written to ", .fg_log, "\n", sep = "")
  invisible(.fg_log)
}

# --- Namespace protection (avoid MASS/stats masking dplyr verbs) ------
select <- dplyr::select;       filter    <- dplyr::filter
mutate <- dplyr::mutate;       slice     <- dplyr::slice
recode <- dplyr::recode;       rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange <- dplyr::arrange;     count     <- dplyr::count;  lag <- dplyr::lag

# ONE FIGURES FOLDER, as of v21.47. There were three: figures_explore for
# the exploratory PNGs, figures_paper for the PUB renders, and figures/ for
# what the manuscript actually reads, which meant collecting a paper's
# exhibits from three places and hand-copying between two of them.
#
# figures_paper and figures/ were the same job done twice, so PUBDIR is now
# figures/ and svp() writes where \includegraphics looks.
#
# THE EXPLORATORY FILES NEEDED A PREFIX BEFORE THEY COULD SHARE IT. svp()
# under PUB = FALSE writes Section P figures under the SAME basenames as the
# publication versions, differing only in extension. Merged without a
# prefix, figures/P1_interruption_by_group.png would sit beside
# figures/P1_interruption_by_group.pdf, and if the PDF were ever missing
# \includegraphics{figures/P1_interruption_by_group} would resolve to the
# PNG in silence, putting "EXPLORATORY, discovery half only. Not for
# publication." under a figure in the compiled paper. EXPL_PREFIX makes that
# impossible and sorts the exploratory panels together in a listing.
# OUTDIR IS NOT THIS SCRIPT'S TO SET. It held "figures" here, and under
# RUN_ALL.R the figures stage runs before verify, so
# verify_manuscript_numbers.R inherited a relative path with no ledger in it
# and died on "Could not locate OUTDIR". Its own header describes this exact
# failure coming from RUN_FIGURES_EDA.R line 78; the fold carried it across.
#
# The exploratory saver wants a directory of its own, so it gets one under a
# name nobody else reads. OUTDIR keeps whatever paths.R set.
# v26. SAME ORDERING FAULT AS PUBDIR ABOVE, same fix. This one is less
# costly because the exploratory saver prefixes its files and nothing
# compiles them, but a discovery figure landing in the manuscript's folder
# or in the project root is still a file somebody has to identify later.
EXPL_DIR <- if (exists("FIGDIR", envir = globalenv()))
  get("FIGDIR", envir = globalenv()) else
    if (exists("DIR_FIGURES") && nzchar(DIR_FIGURES)) DIR_FIGURES else
      if (exists("PAPER_FIGURES") && nzchar(PAPER_FIGURES)) PAPER_FIGURES else
        "figures"
EXPL_PREFIX <- "explore_"

# --- SETTINGS A CALLER CAN OVERRIDE -----------------------------------
# These were plain assignments, and that was a bug with a quiet symptom.
# Setting PUB <- TRUE in the session and then sourcing this file wrote
# FALSE straight back over it, so the run produced exploratory PNGs with
# the "Not for publication" caption burned in and reported no error. The
# only way to turn publication mode on was to edit this file, which is
# not what a setting is for.
#
# Same pattern as .t_flag_note in the 03 design stage, and for the
# same reason: a value inherited from the session is doing exactly what it
# was told, nothing errors, and the only symptom is output that looks
# subtly wrong. So an inherited value is announced.
.eda_flag <- function(nm, default) {
  if (!exists(nm, envir = globalenv())) {
    assign(nm, default, envir = globalenv()); return(invisible(NULL))
  }
  v <- get(nm, envir = globalenv())
  if (!identical(v, default))
    cat(sprintf(paste0("  NOTE: %s = %s INHERITED from this session, not\n",
                       "        defaulted (%s). rm(%s) or restart to clear.\n"),
                nm, format(v), format(default), nm))
  invisible(NULL)
}
.eda_flag("HALF", "discovery")   # "discovery" | "confirmation" | "both"

# --- PUBLICATION MODE -------------------------------------------------
# THIS SCRIPT IS QUARANTINED FROM THE PAPER AND THAT IS DELIBERATE. It
# runs on the discovery half, it writes to figures_explore/, and CAP
# stamps every panel "Not for publication". Section P below adds figures
# that ARE candidates for the manuscript, which puts a hole in that wall,
# so the hole has a door on it.
#
# PUB = FALSE, the default, renders Section P into figures_explore/ with
# the exploratory caption like everything else. That is the mode for
# looking at them and deciding whether they earn a place.
#
# PUB = TRUE renders them into figures_paper/ without the caption, and
# REFUSES TO RUN UNLESS HALF == "both". A figure built on half the sample
# and printed in a paper that reports full-sample estimates beside it is
# the failure this project has already had once, in a different form:
# a discovery-half panel read as though it described the estimation
# frame. The guard is a stop() rather than a warning because the output
# of the mistake is a perfectly ordinary-looking figure.
.eda_flag("PUB", FALSE)      # TRUE renders Section P for the manuscript
# PUB_ONLY, on by default under PUB, suppresses the exploratory writes so a
# publication run leaves figures_explore/ exactly as the discovery run left
# it. Set it FALSE to redraw everything on the full sample deliberately.
.eda_flag("PUB_ONLY", TRUE)
.eda_flag("PUBDIR", if (exists("FIGDIR", envir = globalenv()))
                     get("FIGDIR", envir = globalenv()) else "figures")
# "pdf" or "png". PDF for consistency with the manuscript's existing
# figures/T1_ordering.pdf: vector throughout, so nothing in the set is
# resampled at print size and no figure is visibly softer than its
# neighbours. PNG remains available for a quick look. The manuscript loads
# these without an extension, so graphicx picks up whichever exists and a
# change here needs no edit there.
.eda_flag("PUB_FORMAT", "pdf")

# INHERITED IS NOT THE SAME AS INTENDED, and these two are checked.
#
# .eda_flag deliberately keeps a value the caller set, which is what makes
# PUB usable from the console instead of by editing this file. The cost is
# that a TYPO in a caller-set value now survives, where the old plain
# assignment would have quietly corrected it. That cost is real: PUB_FORMAT
# is pasted straight into a filename, so PUB_FORMAT <- "pdg" left in a
# session writes figures the manuscript cannot find, under names nothing
# opens by extension, and ggsave does not always object.
#
# Both settings below are small closed sets, so membership is checkable and
# a value outside the set is a mistake rather than a preference. Checked
# here, before anything is drawn, rather than at the point of writing.
if (!isTRUE(HALF %in% c("discovery", "confirmation", "both")))
  stop("HALF is '", HALF, "'. It must be \"discovery\", \"confirmation\" or ",
       "\"both\". If that value came from an earlier session, rm(HALF) and ",
       "run again.", call. = FALSE)
if (!isTRUE(PUB_FORMAT %in% c("pdf", "png")))
  stop("PUB_FORMAT is '", PUB_FORMAT, "'. It must be \"pdf\" or \"png\": it ",
       "becomes the file extension, so anything else writes a figure the ",
       "manuscript cannot find. If that value came from an earlier session, ",
       "rm(PUB_FORMAT) and run again.", call. = FALSE)
if (isTRUE(PUB) && !identical(HALF, "both"))
  stop("PUB is TRUE but HALF is '", HALF, "'. Publication figures must be ",
       "rendered on the full sample: set HALF <- \"both\". A half-sample ",
       "figure beside full-sample estimates misstates both.", call. = FALSE)
if (isTRUE(PUB)) dir.create(PUBDIR, showWarnings = FALSE)
SEED     <- 42
MIN_BIN  <- 40               # a bin below this is not plotted, it is noise
dir.create(EXPL_DIR, showWarnings = FALSE)
set.seed(SEED)

# =============================================================================
# 0. DATA AND THE SPLIT
# =============================================================================
# SOURCE ORDER: WIDEST FIRST.
# The loader builds training_panel as
#     analysis_sample %>% select(any_of(training_vars))
# so the slim file (analysis_sample) is a strict SUPERSET of the training
# file. An earlier version of this script preferred the training file and
# lost every covariate that training_vars does not name, which is how the
# education column went missing in what was section D: present in the master
# workspace, absent from the narrower file, and the failure looked like a
# loader bug rather than a source-selection one.
#
# Exploration wants breadth, so read the widest source available and say
# which one it was. The training file is a fallback for a machine that
# only has that.
# v2. THE FALLBACK RESOLVED BARE NAMES AND DID NOT SCREEN, which is the
# same defect fixed in CS_event_study.R and training_event_study.R. Two
# copies of the panel exist, so a bare name takes whichever is nearer, and
# a frame loaded here had 134,131 rows where the master hands in 129,544.
# Figures drawn on that frame are not the figures the estimates describe,
# and nothing in the output would say so.
#
# PANEL from paths.R is preferred when it is in scope, since that is the
# one answer to where the panel is. The in-memory branch is unchanged and
# is still first.
.fig_find <- function(nm) {
  .r <- unique(as.character(unlist(list(
    if (exists("PROJ_ROOT")) PROJ_ROOT,
    if (exists("project_dir")) project_dir,
    getwd(), normalizePath(file.path(getwd(), ".."), mustWork = FALSE)))))
  .p <- unique(c(if (exists("PANEL") &&
                     basename(PANEL) == nm) PANEL, file.path(.r, nm)))
  .h <- .p[file.exists(.p)]
  if (length(.h)) .h[1] else NA_character_
}
.src <- NULL
if (exists("panel") && is.data.frame(panel)) {
  d0 <- panel; .src <- "in-memory `panel`"
} else {
  .p <- .fig_find("hilda_panel_data_W12_W24_slim.rds")
  .narrow <- FALSE
  if (is.na(.p)) { .p <- .fig_find("hilda_panel_data_training.rds"); .narrow <- TRUE }
  if (is.na(.p)) stop("No panel found. Run the loader first.", call. = FALSE)
  d0 <- readRDS(.p)
  .src <- paste0(.p, if (.narrow) " (NARROW: a training_vars subset)"
                     else " (analysis_sample: widest)")

  # The screen, so the figures are drawn on the rows the estimates use.
  #
  # v28. td_frame(d0) WAS A MISCALL, the one CS_event_study.R found at its
  # v13. td_frame's first argument is need_cols, not a frame, so the call
  # errored on every run and the filter with 10 and 35 typed in ran instead.
  # That agreed with the contract only while its bounds were 10 and 35; under
  # TD_SAMPLE_HI=60 the estimates would drop the screen and these figures
  # would keep it. td_apply_sample_rule() takes a frame and applies the
  # contract's bounds; without the contract, its bounds are read if set and
  # 10 and 35 otherwise.
  # v-screen, 14 October (apply_screen_off.R). NO SCREEN BY DEFAULT. The
  # contract's rule is "none" unless TD_SCREEN=on. Without the contract, the
  # hand rule below follows the same setting, with its bounds from
  # TD_SAMPLE_LO and TD_SAMPLE_HI, so the figures use the estimates' rows.
  .n_raw  <- nrow(d0)
  .scr_on <- if (exists("TD_SCREEN_ON")) isTRUE(TD_SCREEN_ON) else
    identical(tolower(trimws(Sys.getenv("TD_SCREEN", "off"))), "on")
  .ruled  <- FALSE
  if (exists("td_apply_sample_rule", mode = "function")) {
    d0 <- td_apply_sample_rule(d0)
    .ruled <- TRUE
  } else if (!.scr_on) {
    cat("Contract not loaded, and no screen: TD_SCREEN is off.\n")
    .ruled <- TRUE
  } else if (all(c("age", "experience_years") %in% names(d0))) {
    .lo <- suppressWarnings(as.numeric(Sys.getenv("TD_SAMPLE_LO", "")))
    .hi <- suppressWarnings(as.numeric(Sys.getenv("TD_SAMPLE_HI", "")))
    if (is.na(.lo)) .lo <- 10
    if (is.na(.hi)) .hi <- 35
    .gp <- d0$age - d0$experience_years
    d0  <- d0[!is.na(.gp) & .gp >= .lo & .gp <= .hi, , drop = FALSE]
    cat(sprintf("Contract not loaded: screen applied by hand, %g to %g.\n", .lo, .hi))
    .ruled <- TRUE
    rm(.gp, .lo, .hi)
  }
  cat(sprintf("Sample rule: %s of %s rows retained.\n",
              format(nrow(d0), big.mark = ","), format(.n_raw, big.mark = ",")))
  if (!.ruled)
    cat("*** NO SCREEN WAS APPLIED although TD_SCREEN is on: the contract is\n",
        "    not loaded and the panel lacks age or experience_years. These\n",
        "    figures are on the whole extract and do not match the estimates.\n",
        sep = "")
  rm(.n_raw, .p, .narrow, .ruled, .scr_on)
}
cat("Source: ", .src, "\n", sep = "")

.bt <- attr(d0, "build_tag")
cat(sprintf("  %s rows x %d cols | build_tag %s\n",
            format(nrow(d0), big.mark = ","), ncol(d0),
            if (is.null(.bt)) "ABSENT" else .bt))

# Person-level hash. Deterministic, so the halves are stable across runs and
# across scripts, which is the only way a discovery/confirmation split means
# anything. Hashing the ID rather than sampling rows keeps whole persons
# together: a split that put a person's pre-birth waves in one half and
# post-birth waves in the other would leak the outcome across the boundary.
.ids  <- sort(unique(as.character(d0$person_id)))
.hash <- vapply(.ids, function(x)
  sum(utf8ToInt(x) * seq_along(utf8ToInt(x))) %% 2L, integer(1))
half_map <- data.frame(person_id = .ids,
                       half = ifelse(.hash == 0L, "discovery", "confirmation"),
                       stringsAsFactors = FALSE)
d0$person_id <- as.character(d0$person_id)
d0 <- left_join(d0, half_map, by = "person_id")

if (HALF != "both") d <- filter(d0, half == HALF) else d <- d0
cat(sprintf("  %s half: %s rows, %s persons (of %s)\n", HALF,
            format(nrow(d), big.mark = ","),
            format(n_distinct(d$person_id), big.mark = ","),
            format(n_distinct(d0$person_id), big.mark = ",")))
if (HALF == "confirmation")
  cat("  *** CONFIRMATION HALF. Only look at this once the specification\n",
      "      is fixed and written down. ***\n", sep = "")

# The clock the manifest at the foot compares against. Set once, here,
# before anything is drawn.
.FIG_RUN_START <- Sys.time()

# --- group labels -----------------------------------------------------
if (!"group_ever" %in% names(d)) {
  d <- d %>% mutate(group_ever = case_when(
    female == 1 & ever_parent == 1 ~ "mothers_ever",
    female == 1 & ever_parent == 0 ~ "never_mothers",
    female == 0 & ever_parent == 1 ~ "fathers_ever",
    TRUE                           ~ "childless_men_ever"))
}
.glab <- c(childless_men_ever = "Childless men", fathers_ever = "Fathers",
           never_mothers = "Never-mothers", mothers_ever = "Mothers")
d$grp <- factor(unname(.glab[d$group_ever]), levels = unname(.glab))

# ---------------------------------------------------------------------
# grp IS NOT A LIFETIME CLASSIFICATION, WHATEVER ITS NAME SUGGESTS.
#
# The loader builds group_ever from ever_mother, and ever_mother is
# ifelse(female == 1, ever_parent, NA) -- a straight copy of the
# WAVE-VARYING ever_parent, which is 0 before a first birth and 1 from
# the birth wave onward. The loader calls it "absorbing", and it is:
# absorbing means it never switches back. It does NOT mean constant
# within person. The "_ever" suffix and the label "Mothers" both read as
# though it were, which is how three rounds of debugging went past it.
#
# WHAT THAT MEANS FOR EVERY FIGURE CUT BY grp:
#   "Mothers"       = mother-waves, FROM the first birth onward
#   "Never-mothers" = true never-mothers PLUS mothers-to-be BEFORE theirs
#   likewise for fathers and childless men
# So a grp contrast is a before-and-after comparison with never-parents
# stacked into the "before" arm. That is a legitimate descriptive cut and
# several figures here want exactly it. It is NOT a comparison of women
# who become mothers against women who do not, and any panel that tries
# to take a pre-birth slice of the "Mothers" group gets the empty set by
# construction, because that group is defined as post-birth.
#
# grp_life IS the lifetime version, built here because the panel does not
# ship one. It is person-invariant, so a mother's pre-birth waves stay in
# the Mothers arm and the pre-birth question can actually be asked.
if ("ever_parent" %in% names(d)) {
  d <- d %>%
    group_by(person_id) %>%
    mutate(.ever_life = if (all(is.na(ever_parent))) NA_integer_
           else as.integer(max(ever_parent, na.rm = TRUE))) %>%
    ungroup()
  d$grp_life <- factor(unname(.glab[dplyr::case_when(
    d$female == 0 & d$.ever_life == 1 ~ "fathers_ever",
    d$female == 0 & d$.ever_life == 0 ~ "childless_men_ever",
    d$female == 1 & d$.ever_life == 1 ~ "mothers_ever",
    d$female == 1 & d$.ever_life == 0 ~ "never_mothers",
    TRUE ~ NA_character_)]), levels = unname(.glab))
  
  # THE CHECK IS PRINTED, NOT ASSUMED. If grp ever becomes person-
  # invariant upstream this says so, and if a future panel silently
  # swaps one for the other the row counts here will not match.
  .sw <- d %>% group_by(person_id) %>%
    summarise(nv = dplyr::n_distinct(grp[!is.na(grp)]), .groups = "drop")
  cat(sprintf("\n  grp switches within person for %s of %s persons: it is a\n",
              format(sum(.sw$nv > 1), big.mark = ","),
              format(nrow(.sw), big.mark = ",")))
  cat("  BEFORE/AFTER cut, not a lifetime one. grp_life is the lifetime\n")
  cat("  version and is what the pre-birth panels use.\n")
  print(rbind(grp = table(d$grp, useNA = "ifany"),
              grp_life = table(d$grp_life, useNA = "ifany")))
  rm(.sw)
} else {
  d$grp_life <- d$grp
  cat("\n  No ever_parent: grp_life falls back to grp. Pre-birth panels\n")
  cat("  are NOT meaningful in this run and should not be read.\n")
}

# Captions are wrapped at draw time rather than by hand, because subtitle and
# caption widths depend on the device size, which is not known when the string
# is written. A hand-placed break is a guess that expires. Lifted from the two
# figure drivers when they were folded in, where it was defined twice.
.wrap <- function(..., width = 108)
  paste(strwrap(paste(...), width = width), collapse = "\n")

theme_x <- theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank(),
        plot.title    = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 7.5, colour = "grey35"),
        plot.caption  = element_text(size = 6.5, colour = "grey45", hjust = 0),
        strip.text    = element_text(size = 8, face = "bold"),
        legend.position = "bottom", legend.title = element_blank())
pal <- c("Childless men" = "#4C72B0", "Fathers" = "#264653",
         "Never-mothers" = "#E76F51", "Mothers" = "#B2182B")

# UNUSED SINCE THE FOLD, kept because binscat() and the diagnostics that
# survive in version control call it, and because deleting it would make
# restoring any of section A to G a two-file job. Nothing in this file calls
# it: every figure here goes through svp().
sv <- function(p, name, w = 16, h = 10) {
  # IN PUB MODE THE EXPLORATORY FIGURES ARE NOT WRITTEN. PUB forces
  # HALF = "both", so every panel in sections A to G would be redrawn on the
  # full sample and written over the discovery-half copies the specification
  # was fixed from. That couples "render Section P for the manuscript" with
  # "spend the confirmation half on every diagnostic", which is not a trade
  # anyone chose. The blocks still run, because Section P reads objects they
  # build, but nothing is saved.
  if (isTRUE(PUB) && isTRUE(PUB_ONLY)) {
    .sv_suppressed <<- .sv_suppressed + 1L
    return(invisible(NULL))
  }
  .f <- file.path(EXPL_DIR, paste0(EXPL_PREFIX, name, ".png"))
  ggsave(.f, p, width = w/2.54, height = h/2.54, dpi = 200)
  cat("  ", .f, "\n", sep = "")
}
.sv_suppressed <- 0L

# Binned means. A raw scatter of 130,000 person-waves is a solid rectangle
# and shows nothing; equal-count bins show the conditional mean and its
# precision. Bins below MIN_BIN are dropped rather than drawn faint, because
# a visibly noisy point still gets read as a turning point.
binscat <- function(data, xvar, yvar, by = "grp", nbin = 20) {
  data %>%
    filter(is.finite(.data[[xvar]]), is.finite(.data[[yvar]])) %>%
    group_by(.data[[by]]) %>%
    mutate(.b = dplyr::ntile(.data[[xvar]], nbin)) %>%
    group_by(.data[[by]], .b) %>%
    summarise(x = mean(.data[[xvar]], na.rm = TRUE),
              y = mean(.data[[yvar]], na.rm = TRUE),
              se = stats::sd(.data[[yvar]], na.rm = TRUE) / sqrt(dplyr::n()),
              n = dplyr::n(), .groups = "drop") %>%
    filter(n >= MIN_BIN) %>%
    rename(grp = 1)
}

CAP <- paste0("EXPLORATORY, ", HALF, " half only. Not for publication. ",
              "Bands are precision of the plotted bin mean, not tests.")

# CAP's last clause is true of the binned-mean figures and false everywhere a
# bar is a SPREAD rather than a standard error. On C3b it contradicted the
# subtitle directly above it, which names the lower bar as an interquartile
# range in capitals while the caption called it precision. The stem is shared
# so HALF stays in one place; only the clause that varies is swapped, and a
# failed match leaves CAP untouched rather than producing a wrong caption.
.cap_spread <- sub("Bands are precision.*$",
                   "Bars are described in the subtitle and are not tests.",
                   CAP)

# Section P's caption and saver. Under PUB the caption goes empty, since a
# manuscript figure carries its note in the LaTeX float rather than burned
# into the image, and the file lands in PUBDIR. Otherwise both are the
# ordinary exploratory ones, so a P figure looked at in the default mode
# is stamped exactly like every other panel here.
CAP_P <- if (isTRUE(PUB)) NULL else CAP

# v27. THE PAPER'S FIGURES CARRY NO TEXT ABOVE OR BELOW THE PANELS. Emptying
# the caption left the title and subtitle in place, and the two T1 figures
# kept all three, so every figure in the paper had a title in the image
# above the float's own and the T1 pair repeated their notes. The in-image
# copy was in its own style ("per cent", "grey", hyphens for en dashes,
# capitals for emphasis), and its numbers were ones the verifier never
# reads, because it reads only the .tex. One disagreed with the text:
# never-mothers' mean experience, 10.8 years on the wage sample against the
# 10.9 the text quotes.
#
# THE FLOAT OWNS THE WORDS. Its \caption is the title, and its notes carry
# what the subtitles and captions said.
#
# THE STATEMENTS STILL REACH THE LOG. The subtitles are computed from the
# series so that a figure cannot contradict its own claim, and that check is
# worth keeping. So this prints what it removes before removing it, and the
# log is where to read the spans and crossings against the notes.
#
# labs(NULL) drops the text; the blank theme elements make sure no version
# of ggplot2 draws it or keeps its space.
.paper_plain <- function(p, name) {
  .lab <- tryCatch(p$labels, error = function(e) NULL)
  .txt <- list()
  for (.k in c("title", "subtitle", "caption")) {
    .v <- tryCatch(.lab[[.k]], error = function(e) NULL)
    if (is.character(.v) && length(.v) == 1L && nzchar(.v)) .txt[[.k]] <- .v
  }
  if (length(.txt)) {
    cat("  in-image text removed from ", name, "; the float carries it:\n",
        sep = "")
    for (.k in names(.txt))
      cat(sprintf("    %-9s%s\n", .k,
                  gsub("\n", "\n             ", .txt[[.k]], fixed = TRUE)))
  }
  p + labs(title = NULL, subtitle = NULL, caption = NULL) +
    theme(plot.title    = element_blank(),
          plot.subtitle = element_blank(),
          plot.caption  = element_blank())
}

svp <- function(p, name, w = 16, h = 10) {
  # v27. The paper's copy goes out plain; see .paper_plain(). The
  # exploratory copy keeps its title, subtitle and stamp.
  if (isTRUE(PUB)) p <- .paper_plain(p, name)
  # PUB_FORMAT under PUB, raster otherwise. The exploratory copies are
  # always PNG: they are looked at on screen and rewritten far more often
  # than they are printed.
  dir <- if (isTRUE(PUB)) PUBDIR else EXPL_DIR
  ext <- if (isTRUE(PUB)) paste0(".", PUB_FORMAT) else ".png"
  # The exploratory copy takes the prefix; the publication one must not,
  # because the manuscript names these files.
  nm  <- if (isTRUE(PUB)) name else paste0(EXPL_PREFIX, name)
  # cairo_pdf EMBEDS ITS FONTS. The default pdf device only names Helvetica,
  # so the paper carried five figures with an unembedded font, which journal
  # production rejects. The PNG copies keep the default device.
  ggsave(file.path(dir, paste0(nm, ext)), p,
         width = w/2.54, height = h/2.54,
         dpi = if (isTRUE(PUB)) 300 else 200,
         device = if (ext == ".pdf") grDevices::cairo_pdf else NULL)
  cat("  ", dir, "/", nm, ext, if (isTRUE(PUB)) "  [PAPER]" else "",
      "\n", sep = "")
}

# Column resolver.
# ---------------------------------------------------------------------
# This script runs against whichever panel it finds, and the training
# panel is a SUBSET of analysis_sample built by a second select. A column
# present in the master workspace can be absent from the file, which is
# how educ_years crashed what was section D: visible while developing, missing on
# the saved panel. Resolve names once, up front, and report what is
# missing rather than failing eleven sections in.
need <- function(..., what = NULL) {
  cand <- c(...)
  hit <- cand[cand %in% names(d)]
  if (length(hit)) return(hit[1])
  if (!is.null(what))
    cat(sprintf("  [absent: %s — tried %s]\n", what,
                paste(cand, collapse = ", ")))
  NA_character_
}
V <- list(
  educ  = need("educ_years", "education", "educ", what = "years of education"),
  # hourly_wage_clean is the trimmed version (NA outside 10-500); the raw
  # hourly_wage is preferred here so the coverage table describes the
  # variable rather than the trim. Both are on analysis_sample.
  wage  = need("hourly_wage", "hourly_wage_clean", what = "hourly wage"),
  lnw   = need("ln_hourly_wage_real", "ln_hourly_wage", what = "log wage"),
  exp   = need("experience_years", what = "experience"),
  hw    = need("housework_hours", what = "housework"),
  ten   = need("tenure_employer", what = "employer tenure"),
  prec  = need("precarious_score", what = "precarious score"),
  # THE MANUSCRIPT'S MEASURE FIRST. Section 4.3 defines employer-financed
  # as jttpewt, at the place of employment during paid work time, and says
  # the other-place pair is not used. training_paid_time is
  # jttopwt | jttpewt, so leading with it put Table 4's companion figure on
  # a different object from Table 4 itself.
  paid  = need("jttpewt_obs", "jttpewt_any", "training_paid_time_obs",
               "training_paid_time", what = "employer-financed training"),
  crs   = need("total_courses", what = "training courses"),
  hrs   = need("total_training_hours_z", "total_training_hours",
               what = "training hours"),
  days  = need("total_training_days", what = "training days"),
  aph   = need("avg_training_hours", what = "hours per course day"))
cat(sprintf("Resolved %d of %d covariates.\n",
            sum(!is.na(unlist(V))), length(V)))

# =============================================================================

# P. CANDIDATE MANUSCRIPT FIGURES
# =============================================================================
# WHAT EARNS A PLACE HERE. Not every true thing needs a figure. The test
# applied to each of the four below is that the manuscript already ASSERTS
# the claim in prose, the claim does real work in the argument, and no
# table currently shows the shape the prose describes. A figure that
# restates a table is a cost with no return; a figure that lets a reader
# check a sentence they would otherwise have to take on trust is not.
#
# WHAT IS DELIBERATELY ABSENT. The event-study paths are already Figure 1.
# The raw event-time gap is Table 4 and a figure of it would duplicate
# Figure 1's upper panel. Missingness by group is Table 3 and reads
# perfectly well as numbers. The education-experience cross-partial in
# Table 9 would make a good figure and is NOT here, because this script
# has no regression machinery and estimating one inside a figures script
# is how a figures script becomes a second analysis script with no ledger.
#
# P1  the interruption contrast, which is the descriptive foundation of
#     Section 4.2 and the sentence "mothers stand apart"
# P2  the training-experience profile with the group means marked, which
#     is the resolution of the cross-section against event-study tension
#     in Section 4.7 and the answer to the most natural referee objection
# P3  the bimodality of training hours, which is why the paper reports
#     courses rather than hours and drops the conditional dosage design
# P4  wage-sample selection by event time, which is why the paper uses
#     earnings rather than wages for the penalty
cat("\n--- P. Candidate manuscript figures ---\n")
if (isTRUE(PUB) && isTRUE(PUB_ONLY))
  cat("  PUB_ONLY is TRUE: sections A-G still run, because Section P reads\n",
      "  what they build, but their figures are NOT written. The\n",
      "  discovery-half copies in ", EXPL_DIR, "/ are left untouched.\n", sep = "")
if (isTRUE(PUB)) {
  cat("  PUB is TRUE: rendering to ", PUBDIR, "/ at 300 dpi, with no title,\n",
      "  subtitle or caption in the image; the log keeps their text.\n", sep = "")
} else {
  cat("  PUB is FALSE: rendering to ", EXPL_DIR, "/ with the exploratory\n", sep = "")
  cat("  caption. Set PUB <- TRUE and HALF <- \"both\" for the paper versions.\n")
}

# ---- P2. The profile that reconciles the cross-section with the event ---
# THE MANUSCRIPT DRAWS THIS ONE. Section 4.7 confronts a tension a referee
# will find unaided: in the cross-section active mothers and never-mothers
# differ in employer-financed training by 0.6 percentage points, while the
# event study reports eleven. The text resolves it in a paragraph. The two
# groups sit at different points of a training-by-experience profile that
# is far from flat, and what the data show is not one curve with the groups
# placed along it but two that cross: the difference is negative early,
# where most first births occur and where the event study identifies, and
# positive late, among women whose children have grown.
#
# THE EARLIER VERSION OF THAT PARAGRAPH SAID SOMETHING ELSE, that a single
# hump-shaped profile placed mothers in a higher-training region of itself
# and so offset the motherhood effect. The diagnostic at the foot of this
# block is what refuted it, by reading the never-mothers' profile at each
# group's own mean and finding it LOWER where the mothers sit. Composition
# on its own therefore predicts a negative gap and cannot be what cancels
# one. The diagnostic is kept because the claim it tests is the one a
# reader will reach for first.
#
# That is a claim about a SHAPE, and prose is a poor instrument for one.
# The figure draws the profile and marks where the two groups sit on it,
# so the offsetting is visible rather than asserted. A reader who doubts
# the paragraph can check it in one look, and a reader who was about to
# write "your own descriptives show no effect" can see why they do not.
#
# THE MARKERS ARE THE POINT, not decoration. Without them this is a
# training-by-experience profile and says nothing about mothers.
# REDRAWN. The first version plotted the four grp lines and marked the
# means of two group_3way_women categories on top of them, which is two
# different partitions in one panel: the lines were mother-WAVES against
# never-mother-waves (grp is absorbing but wave-varying, see the note at
# grp_life), while the markers came from the three-way split of women. The
# argument in Section 4.7 is about ONE comparison, active mothers against
# never-mothers in the wage sample, so that is the comparison drawn.
#
# WOMEN ONLY, for the same reason. The tension the figure explains is that
# these two groups of women differ by well under a point in the cross-section
# and by eleven in the event study, in opposite directions. Men are not part
# of that sentence and four extra lines make the crossing harder to see.
#
# THE FRAME IS THE WAGE SAMPLE, because Table 5 is. The diamonds are the two
# overall rates the paragraph beside this figure quotes, so they have to be
# the table's numbers or a reader who measures them finds a second pair. On
# the employed frame they came out near 26.0 and 24.3, a gap of about 1.7
# points, under a sentence that says 0.6. Where the wage column does not
# resolve the block falls back to the employed frame and says so in the
# subtitle, so the figure never claims a frame it was not drawn on.
#
# NOTHING HERE IS RESTATED FROM A PREVIOUS BUILD. Every number in the
# subtitle and in the diagnostic below is read off the profile that was just
# fitted, because the last version of this comment carried group means from
# the v21.43b build that no longer held and the .tex carried readings from
# the discovery half that no longer held either.
.exp <- V$exp; .pd <- V$paid
if (!is.na(.exp) && !is.na(.pd) && "group_3way_women" %in% names(d)) {
  .lab3 <- c(never_mothers = "Never-mothers", active_mothers = "Active mothers",
             post_mothers = "Post-mothers")
  .p2_wage <- !is.na(V$wage) && V$wage %in% names(d)
  # THE THREE-WAY SPLIT INHERITS THE WAVE-VARYING PROBLEM. group_3way_women
  # comes from motherhood_status, which the loader builds as
  # ever_parent == 0 ~ never_mother, and ever_parent is absorbing without
  # being constant within person. Its never_mothers level therefore holds
  # every future mother's pre-birth waves: the same trap grp_life was built
  # for above, walked into again by taking the loader's column instead of
  # the lifetime one.
  #
  # IT MATTERS MORE HERE THAN ANYWHERE. This figure exists to show two
  # profiles crossing, and the waves it would pool into the never-mothers
  # curve are the highest-training observations in the data, 29.8 per cent
  # against a never-mother rate of 27.0. Leaving them there lifts the
  # comparison curve using the treated group's own pre-birth observations,
  # which is the contamination Section 4.5 documents in the other direction
  # and fixes.
  #
  # Rebuilt on the lifetime maximum, so a woman who becomes a mother sits in
  # the mothers arm at every wave and never-mothers are women who never do.
  # The active/post split stays wave-varying and should: that one is about
  # children currently at home rather than about ever.
  .n_pre <- sum(d$female == 1 & d$group_3way_women == "never_mothers",
                na.rm = TRUE)
  # THE LOADER NOW SHIPS THIS. group_4way_women_life is built once in
  # LOADING_ND3_v3.R after the cummax fix, with a pre_mothers level so a
  # future mother's pre-birth waves are neither a control nor pooled with
  # post-mothers. Rebuilt here only when an older panel is loaded, and the
  # local version is the same rule so the two cannot drift.
  .b <- if ("group_4way_women_life" %in% names(d)) {
    d %>% mutate(group_3way_women = group_4way_women_life)
  } else {
    cat("  group_4way_women_life absent; rebuilding locally. Update the\n",
        "  loader rather than relying on this branch.\n", sep = "")
    d %>% group_by(person_id) %>%
      mutate(.ever_life = if (all(is.na(ever_parent))) NA_integer_
             else as.integer(max(ever_parent, na.rm = TRUE))) %>%
      ungroup() %>%
      mutate(group_3way_women = dplyr::case_when(
        female != 1                        ~ NA_character_,
        .ever_life == 0                    ~ "never_mothers",
        ever_parent == 0                   ~ "pre_mothers",
        group_3way_women == "post_mothers" ~ "post_mothers",
        TRUE                               ~ "active_mothers"))
  }
  cat("  never-mothers, loader column: ", format(.n_pre, big.mark = ","),
      " person-waves; lifetime column: ",
      format(sum(.b$female == 1 &
                 .b$group_3way_women == "never_mothers", na.rm = TRUE),
             big.mark = ","),
      ".\n  The difference is future mothers' pre-birth waves.\n", sep = "")
  .b <- .b %>%
    filter(employed == 1, female == 1, is.finite(.data[[.exp]]),
           !is.na(.data[[.pd]]), !is.na(group_3way_women),
           group_3way_women %in% c("never_mothers", "active_mothers")) %>%
    mutate(g3 = factor(unname(.lab3[group_3way_women]),
                       levels = unname(.lab3[c("never_mothers", "active_mothers")])))
  if (.p2_wage) .b <- .b %>% filter(!is.na(.data[[V$wage]]))
  .p2_frame <- if (.p2_wage) "wage sample" else
    "employed frame, NOT the wage sample Table 1 uses"
  if (!.p2_wage)
    cat("  *** P2: no hourly wage column resolved. Drawn on the employed\n",
        "      frame, so its two diamonds will not reconcile with Table 1.\n",
        sep = "")
  if (nrow(.b) >= MIN_BIN * 4) {
    prof <- binscat(.b, .exp, .pd, by = "g3", nbin = 12)
    .mk <- .b %>% group_by(g3) %>%
      summarise(x = mean(.data[[.exp]]), y = 100 * mean(.data[[.pd]]),
                n = dplyr::n(), .groups = "drop")
    if (nrow(prof) && nrow(.mk) == 2) {

      # ---------------------------------------------------------------
      # READ THE PROFILES BEFORE DRAWING THEM. The subtitle asserted
      # "large and negative early ... and reverses late" as a fixed
      # string, which is a claim about two quantities the figure computes
      # and about whether their bands separate, which it also computes.
      # Both are read here so the subtitle cannot outrun the panel.
      # ---------------------------------------------------------------
      .atv <- function(pr, g, xx) {
        s <- pr[pr$grp == g, ]
        if (nrow(s) < 2 || !is.finite(xx)) return(c(NA_real_, NA_real_))
        c(100 * stats::approx(s$x, s$y,  xout = xx, rule = 2)$y,
          100 * stats::approx(s$x, s$se, xout = xx, rule = 2)$y)
      }
      .at <- function(pr, g, xx) .atv(pr, g, xx)[1]
      .xn <- .mk$x[.mk$g3 == "Never-mothers"]
      .xa <- .mk$x[.mk$g3 == "Active mothers"]

      # THE SEPARATION TEST IS THE ONE A READER PERFORMS, which is whether
      # the two drawn ribbons touch, not whether a difference clears a
      # threshold. Each ribbon is the bin mean plus or minus 1.96 standard
      # errors, so they separate when the distance between the lines
      # exceeds 1.96 times the SUM of the two. That is conservative
      # against a formal test on the difference and it is what the panel
      # shows, which is the point of stating it in a subtitle.
      .gap <- function(xx) {
        a <- .atv(prof, "Active mothers", xx)
        n <- .atv(prof, "Never-mothers", xx)
        c(diff = a[1] - n[1], slack = abs(a[1] - n[1]) - 1.96 * (a[2] + n[2]))
      }
      .g_n <- .gap(.xn); .g_a <- .gap(.xa)
      .sepw <- function(g)
        if (is.na(g[["slack"]])) "the bands cannot be read" else
          if (g[["slack"]] > 0) "the bands separate" else "the bands overlap"

      # HOW MANY TIMES DO THEY CROSS. The title says the profiles cross,
      # and a reader follows the lines to the right edge to find out where.
      # Counted on the overlapping support rather than asserted, because a
      # late second crossing among thin bins is exactly the kind of thing a
      # title written once and left alone stops mentioning.
      .ov <- c(max(min(prof$x[prof$grp == "Never-mothers"]),
                   min(prof$x[prof$grp == "Active mothers"])),
               min(max(prof$x[prof$grp == "Never-mothers"]),
                   max(prof$x[prof$grp == "Active mothers"])))
      .grid  <- seq(.ov[1], .ov[2], length.out = 400)
      .dif   <- vapply(.grid, function(z) unname(.gap(z)["diff"]), numeric(1))
      .sgn   <- sign(.dif)
      .cidx  <- which(diff(.sgn) != 0)
      .cross_x <- .grid[.cidx]

      p <- ggplot(prof, aes(x, 100 * y, colour = grp)) +
        geom_ribbon(aes(ymin = 100 * (y - 1.96 * se),
                        ymax = 100 * (y + 1.96 * se), fill = grp),
                    alpha = 0.15, colour = NA) +
        geom_line(linewidth = 0.7) + geom_point(size = 1.5) +
        geom_vline(data = .mk, aes(xintercept = x, colour = g3),
                   linetype = "dashed", linewidth = 0.4, show.legend = FALSE) +
        geom_point(data = .mk, aes(x = x, y = y, colour = g3), size = 3.4,
                   shape = 18, show.legend = FALSE) +
        scale_colour_manual(values = c("Never-mothers" = "#E76F51",
                                       "Active mothers" = "#B2182B")) +
        scale_fill_manual(values = c("Never-mothers" = "#E76F51",
                                     "Active mothers" = "#B2182B"),
                          guide = "none") +
        # EMPLOYER-FINANCED, NOT EMPLOYER-FUNDED. Section 2 undertakes to
        # use the one term without variation, in headings and captions as
        # well as in the text, so that a change of wording is never a
        # change of object. This panel was the one place that varied it,
        # on the axis and twice in the subtitle.
        labs(title = paste("Two profiles that cross, not one profile with",
                           "the groups at different points"),
             subtitle = .wrap(sprintf(paste(
               "Employer-financed training against experience, women only,",
               "equal-count bins, %s. Dashed lines mark each group's mean",
               "experience; diamonds are each group's overall rate across its",
               "whole experience distribution, which is why a diamond need not",
               "sit on its own line. Mothers minus never-mothers is %+.1f",
               "points at %.1f years, where %s, and %+.1f points at %.1f,",
               "where %s."),
               .p2_frame, .g_n[["diff"]], .xn, .sepw(.g_n),
               .g_a[["diff"]], .xa, .sepw(.g_a))),
             # v27. "Labor", as the text and the notes spell it; "Labour"
             # belongs only to the survey's own name.
             x = "Labor market experience (years)",
             y = "Employer-financed training (%)", caption = CAP_P) +
        theme_x
      svp(p, "P2_training_experience_profile")

      cat("  frame: ", .p2_frame, "\n", sep = "")
      cat("  group means (the two numbers Section 4.3 and Appendix C.3 reconcile):\n")
      print(as.data.frame(.mk), row.names = FALSE, digits = 3)
      cat(sprintf("  diamond gap, mothers minus never-mothers: %+.1f points\n",
                  .mk$y[.mk$g3 == "Active mothers"] -
                    .mk$y[.mk$g3 == "Never-mothers"]))
      cat("  Table 1 quotes this pair on the wage sample. If the two",
          " disagree,\n  the figure and the paragraph are on different",
          " frames.\n", sep = "")

      # DOES THE COMPOSITION STORY IN SECTION 4.7 SURVIVE? An earlier
      # version of the paragraph said employer-financed training is
      # hump-shaped in experience, that active mothers sit further along
      # it than never-mothers, and that they therefore occupy a
      # higher-training region of it which offsets the motherhood effect
      # and produces a cross-sectional difference near zero.
      #
      # That is a claim with a direction, and the direction is checkable.
      # Read the NEVER-MOTHERS' profile at each group's own mean
      # experience. If the paragraph is right, the profile is HIGHER at
      # the active mothers' mean than at the never-mothers', so a mother
      # placed on the common profile would be predicted to train more and
      # the observed near-zero gap is that prediction cancelling a real
      # negative effect. If the profile is LOWER there, composition
      # predicts a negative gap on its own, it cannot be what cancels one,
      # and that version of the paragraph had the sign the wrong way round.
      # It did, which is why the paragraph now describes two curves.
      cat(sprintf(paste0("  Never-mothers' profile: %.1f%% at their own mean ",
                         "(%.1f yrs),\n                          %.1f%% at the ",
                         "active mothers' mean (%.1f yrs).\n"),
                  .at(prof, "Never-mothers", .xn), .xn,
                  .at(prof, "Never-mothers", .xa), .xa))
      cat(sprintf(paste0("  Active mothers' profile:  %.1f%% at the never-mothers' ",
                         "mean,\n                          %.1f%% at their own.\n"),
                  .at(prof, "Active mothers", .xn),
                  .at(prof, "Active mothers", .xa)))
      cat(sprintf(paste0("  Experience-conditional gap (mothers minus not): ",
                         "%+.1f pp at %.1f yrs (%s),\n  %+.1f pp at %.1f yrs ",
                         "(%s).\n"),
                  .g_n[["diff"]], .xn, .sepw(.g_n),
                  .g_a[["diff"]], .xa, .sepw(.g_a)))
      if (is.finite(.g_n[["diff"]]) && is.finite(.g_a[["diff"]]) &&
          sign(.g_n[["diff"]]) != sign(.g_a[["diff"]]))
        cat("  THE GAP CHANGES SIGN with experience, so there is no single\n",
            "  common profile the two groups sit on at different points. The\n",
            "  negative side falls where most first births occur, which is\n",
            "  where the event study identifies; the positive side is\n",
            "  late-career women whose children have grown. The cross-section\n",
            "  averages the two to nearly nothing.\n", sep = "")

      cat("  crossings on the overlapping support (",
          sprintf("%.1f", .ov[1]), " to ", sprintf("%.1f", .ov[2]),
          " yrs): ", length(.cross_x),
          if (length(.cross_x))
            paste0(" at ", paste(sprintf("%.1f", .cross_x), collapse = ", "),
                   " yrs") else "",
          "\n", sep = "")
      if (length(.cross_x) > 1)
        cat("  *** more than one crossing. The title says the profiles cross\n",
            "      and a reader will follow them to the right edge, so the\n",
            "      .tex note has to say where the later ones are and how thin\n",
            "      the bins carrying them are.\n", sep = "")

      # THE MAGNITUDE IS NOT THE EVENT-STUDY ESTIMATE and must not be
      # written as though it were. This is a raw difference between two
      # groups of women at a common experience level; the -11.7 in Section
      # 6.1 is a within-person change around an event. They can agree in
      # sign without agreeing in size, and an earlier draft of the .tex
      # comment claimed they were close on a reading that no longer holds.
      cat("  The early gap above is a cross-sectional contrast at common\n",
          "  experience. It is NOT the event-study estimate and should not\n",
          "  be quoted beside it as corroboration.\n", sep = "")

      rm(.atv, .at, .xn, .xa, .gap, .g_n, .g_a, .sepw,
         .ov, .grid, .dif, .sgn, .cidx, .cross_x)
    }
    rm(prof, .mk)
  }
  rm(.b, .lab3, .p2_wage, .p2_frame)
}

# ---- P3. Which margin moves: extensive against intensive ---------------
# REPLACES the hours density. That figure answered "is the distribution
# bimodal", which justifies a measurement choice, and Section 4.3 can make
# that case in prose from the percentiles it already quotes: median 24
# hours, p90 90, p99 400, with 2.56 per cent of the mass in the 240-2,000
# window. Three orders of magnitude is a claim a reader will take from
# numbers.
#
# What the paper asserts and does not show is the decomposition. Section
# 6.1 reports mothers 13.2 points less likely to train at all, against a
# conditional reduction of 18.6 per cent whose Lee bounds run from -35.2 to
# +43.6. That is a statement about two margins behaving differently, and it
# is a picture.
#
# It also makes the dropped-regression argument better than the density
# did. The density says a mean over a wide distribution is uninformative,
# which is about noise. This says the conditional sample is not the same
# set of people across the birth, which is about identification, and the
# shaded region says it at a glance.

# v3. THE WINDOW IS NAMED AND MATCHES THE ORDERING FIGURES.
#
# This ran k = -6 to +10 while alt_T1_ordering_pooled and
# alt_T1_which_side_pooled run K_LO = -8 to K_HI = +9. Three figures in the
# same paper on three different event axes invites a reader to compare
# horizons that are not there: Figure 2 appeared to start two years later
# than Figures 3 and 4 and to run a year further, and nothing in either
# caption said why.
#
# WHY -8 to +8 RATHER THAN -8 to +9. The lead matches the ordering figures
# exactly. The lag stops one short of theirs because this panel is cut by a
# different constraint: the intensive margin is a median over trainees only,
# and Section 6.1 already declines to plot cells below forty person-waves.
# The ordering figures are cut by a standard-error rule instead, which the
# raw shares here do not have. Matching the lead makes the three comparable
# where it matters, and stopping at +8 keeps this panel inside the cells it
# can actually support.
#
# THE LIMITS ARE STILL ADVISORY. The block below cuts to the range BOTH
# margins support and reports the cut, so a thin intensive margin can pull
# the drawn range inside this window. That message is the one to read; this
# pair is the ceiling, not a promise.
P3_KLO <- -8L
P3_KHI <-  8L
# Both margins are drawn on the whole window above. Setting this TRUE restores
# the older behaviour, which cut both back to the horizons they both support;
# the note at the cut below says why that is no longer the default.
P3_COMMON_RANGE <- FALSE

.pew_c3b <- need("jttpewt_any", "training_paid_time_obs", "training_paid_time",
                 what = "employer-financed training for the margin split")
.hr_c3b  <- need("total_training_hours_z", "total_training_hours",
                 what = "training hours for the margin split")
if (!is.na(.pew_c3b) && !is.na(.hr_c3b) &&
    "ever_parent" %in% names(d) && "employed" %in% names(d)) {
  cat(sprintf("\n  extensive margin: %s | intensive margin: %s\n",
              .pew_c3b, .hr_c3b))
  
  # Section D's event frame, REBUILT HERE RATHER THAN INHERITED. The guard
  # that reused an existing `ed` is what drew this panel on an axis the two
  # constants above do not describe. An `ed` left in the session by an
  # earlier run carries that run's window; the window filter sat INSIDE the
  # guard, so it never ran; and the figure came out on k = -6 to +10 while
  # P3_KLO and P3_KHI said -8 to +8, with nothing reporting the difference.
  # The definition of k is the thing the two sections must share and it is
  # copied verbatim. The window is this section's own and is applied on
  # every run.
  if (exists("ed") && is.data.frame(ed) && "k" %in% names(ed)) {
    .er <- range(ed$k, na.rm = TRUE)
    if (!all(is.finite(.er)) || .er[1] != P3_KLO || .er[2] != P3_KHI)
      cat(sprintf(paste0("  an `ed` spanning k = %.0f..%.0f is in the session ",
                         "and is NOT\n  used here; this block builds its own ",
                         "frame on k = %d..%d.\n"),
                  .er[1], .er[2], P3_KLO, P3_KHI))
    rm(.er)
  }
  ev3 <- d %>% group_by(person_id) %>% arrange(wave) %>%
    summarise(bw = { dd <- diff(ever_parent)
    i <- which(!is.na(dd) & dd > 0)
    if (length(i)) wave[i[1] + 1] else NA_real_ },
    .groups = "drop") %>% filter(!is.na(bw))
  ed3 <- d %>% inner_join(ev3, by = "person_id") %>%
    mutate(k = wave - bw, sex = ifelse(female == 1, "Women", "Men")) %>%
    filter(k >= P3_KLO, k <= P3_KHI)
  
  hh <- ed3 %>%
    filter(!is.na(employed), employed == 1) %>%
    mutate(hrs = ifelse(is.finite(.data[[.hr_c3b]]), .data[[.hr_c3b]], NA_real_),
           pew = .data[[.pew_c3b]])
  
  # EXTENSIVE. Employer-financed incidence over the employed-and-asked, which
  # is the paper's own outcome and the one whose pre-trend has been tested.
  ext <- hh %>%
    filter(!is.na(pew)) %>%
    group_by(sex, k) %>%
    summarise(n = dplyr::n(), persons = n_distinct(person_id),
              p = 100 * mean(pew),
              se = 100 * sqrt(mean(pew) * (1 - mean(pew)) / dplyr::n()),
              .groups = "drop") %>%
    filter(n >= MIN_BIN)
  
  # INTENSIVE. Hours among trainees only, with the trimming share attached.
  # The share is the sex gap in the extensive margin at k relative to the
  # pre-birth mean of that gap: it is how much of the conditional sample at
  # this horizon is in question, and it is the reason the conditional
  # estimate in Section 6.1 carries bounds that span zero.
  # The trimming share is about who is IN the conditional sample, so it is
  # computed on trainee status rather than on the extensive panel above. The
  # two are different outcomes now: the panel is employer-financed incidence
  # and the conditional sample is anyone with positive hours from any source.
  .tr <- hh %>%
    filter(is.finite(hrs)) %>%
    group_by(sex, k) %>%
    summarise(p = 100 * mean(hrs > 0), n = dplyr::n(), .groups = "drop") %>%
    filter(n >= MIN_BIN)
  .pre_gap <- .tr %>%
    filter(k < 0) %>%
    tidyr::pivot_wider(id_cols = k, names_from = sex, values_from = p) %>%
    summarise(g = mean(Women - Men, na.rm = TRUE)) %>% dplyr::pull(g)
  trim <- .tr %>%
    tidyr::pivot_wider(id_cols = k, names_from = sex, values_from = p) %>%
    mutate(trim = pmax(0, (.pre_gap - (Women - Men)) / pmax(Women, 1))) %>%
    select(k, trim)
  
  .q <- function(x, p) unname(stats::quantile(x, p, na.rm = TRUE))
  int <- hh %>%
    filter(is.finite(hrs), hrs > 0) %>%
    group_by(sex, k) %>%
    summarise(n = dplyr::n(), mean = mean(hrs), p50 = .q(hrs, .50),
              p25 = .q(hrs, .25), p75 = .q(hrs, .75), .groups = "drop") %>%
    filter(n >= MIN_BIN) %>%
    left_join(trim, by = "k")
  
  cat("\n  Extensive margin, employer-financed incidence (%):\n")
  print(as.data.frame(ext %>% filter(k %in% c(-8, -3, -1, 0, 1, 4, 8))),
        row.names = FALSE, digits = 3)
  cat("\n  Intensive margin, hours among trainees, with the trimming share:\n")
  print(as.data.frame(int %>% filter(k %in% c(-8, -3, -1, 0, 1, 4, 8))),
        row.names = FALSE, digits = 3)
  cat(sprintf("\n  Pre-birth gap in TRAINEE STATUS: %.1f points. Trimming share at k = 0:",
              .pre_gap))
  cat(sprintf(" %.2f\n", trim$trim[trim$k == 0]))
  cat("  The trimming share is what makes the intensive panel unreadable as a\n")
  cat("  level: at k = 0 the women behind the conditional line are a\n")
  cat("  differently selected set from those behind it at k = -1, which is\n")
  cat("  why Section 6.1 reports Lee bounds spanning zero on that margin.\n")
  
  # WHICH HORIZONS THE WINDOW ACTUALLY DELIVERS. The window is advisory and
  # MIN_BIN is not, so any cell the window asks for and the data do not
  # supply is named. That matters more now that the axis is pinned: a line
  # stopping short of the axis end is a cell that failed MIN_BIN, and the
  # console says which one rather than leaving the gap to be guessed at.
  .cover <- function(z, lab) for (s in sort(unique(z$sex))) {
    m <- setdiff(P3_KLO:P3_KHI, z$k[z$sex == s])
    if (length(m))
      cat(sprintf("  %s margin, %s: no cell at k = %s (below MIN_BIN = %d)\n",
                  lab, s, paste(m, collapse = ", "), MIN_BIN))
  }
  .cover(ext, "extensive"); .cover(int, "intensive")
  
  # A COMMON x RANGE, NOW OFF BY DEFAULT. MIN_BIN bites harder on the
  # conditional cells, so the intensive panel can end earlier than the
  # extensive one, and two panels with different extents read as missing
  # data rather than as a thinner denominator. That was the case for cutting
  # both to the range they share, and it was a case about a FLOATING axis.
  # The axis is pinned to P3_KLO..P3_KHI below, so both panels carry the
  # same extent whatever is drawn inside them and a short line reads as the
  # gap it is. The cut now only discards extensive-margin cells that are
  # fine, so it is reported instead of taken.
  .kmax <- min(max(ext$k), max(int$k)); .kmin <- max(min(ext$k), min(int$k))
  if (isTRUE(P3_COMMON_RANGE)) {
    if (max(ext$k) != .kmax || min(ext$k) != .kmin)
      cat(sprintf(paste0("  x range cut to k = %d..%d, the range both margins ",
                         "support at\n  MIN_BIN = %d; the extensive margin ",
                         "alone runs %d..%d.\n"),
                  .kmin, .kmax, MIN_BIN, min(ext$k), max(ext$k)))
    ext <- ext %>% filter(k >= .kmin, k <= .kmax)
    int <- int %>% filter(k >= .kmin, k <= .kmax)
  } else if (max(ext$k) != .kmax || min(ext$k) != .kmin) {
    cat(sprintf(paste0("  the margins support different ranges: extensive ",
                       "%d..%d, intensive\n  %d..%d. Both are drawn on the ",
                       "declared k = %d..%d axis, so the shorter\n  line is ",
                       "the thinner denominator and reads as one.\n"),
                min(ext$k), max(ext$k), min(int$k), max(int$k),
                P3_KLO, P3_KHI))
  }
  
  # THE MEAN IS CLIPPED, NOT DROPPED. Section C's whole point is that the
  # mean of this variable sits far above its median, and on some cells it
  # sits three times higher, which stretches the axis until the medians are
  # unreadable. Clipping keeps the panel legible; the clipped values are
  # printed so the tail is on the record rather than hidden by the choice.
  .ycap <- max(int$p75) * 1.35
  .clipped <- int %>% filter(mean > .ycap)
  if (nrow(.clipped)) {
    cat(sprintf("  %d trainee-cell mean(s) clipped at %.0f hours for the plot:\n",
                nrow(.clipped), .ycap))
    print(as.data.frame(.clipped %>% select(sex, k, n, p50, mean)),
          row.names = FALSE, digits = 3)
  }
  int <- int %>% mutate(mean_pl = pmin(mean, .ycap),
                        clipped = mean > .ycap)
  
  # WHERE THE CONDITIONAL PANEL MUST NOT BE READ AS A LEVEL. The fade in the
  # first version encoded this in alpha, under a solid ribbon, which is to
  # say it encoded nothing. It is a shaded region with a label now, because
  # the point has to survive being glanced at.
  .bad <- int %>% filter(trim >= 0.25) %>% dplyr::pull(k)
  # The label needs the panel column for the same reason the rect does.
  # annotate() carries no facet variable, so it draws in EVERY panel, which
  # in the first render put "not comparable" over the extensive panel: the
  # one panel that is comparable, and the one the figure exists to read.
  # The right edge is Inf, not .kmax + 0.5. Stopping at the last horizon
  # leaves the axis expansion unshaded, and the panel gridlines reappear in
  # that sliver where they are hidden everywhere else under the rect. Inf is
  # also the correct claim: the region is "from k = 0 onward", not "from
  # k = 0 to the last cell that survived MIN_BIN".
  .shade <- if (length(.bad))
    data.frame(x1 = min(.bad) - 0.5, x2 = Inf,
               lab_x = min(.bad) + 0.2,
               panel = "Intensive: hours among trainees") else NULL
  
  both <- dplyr::bind_rows(
    ext %>% transmute(sex, k, y = p, lo = p - 1.96 * se, hi = p + 1.96 * se,
                      mean_pl = NA_real_,
                      panel = "Extensive: employer-financed training (% of employed)"),
    int %>% transmute(sex, k, y = p50, lo = p25, hi = p75, mean_pl,
                      panel = "Intensive: hours among trainees"))
  
  p <- ggplot(both, aes(k, y, colour = sex)) +
    { if (!is.null(.shade))
      geom_rect(data = .shade, inherit.aes = FALSE,
                aes(xmin = x1, xmax = x2, ymin = -Inf, ymax = Inf),
                fill = "grey92") } +
    geom_vline(xintercept = -0.5, linetype = 3, colour = "grey40") +
    # Interval and IQR are DIFFERENT OBJECTS and are drawn differently: a
    # thin errorbar rather than a filled ribbon, so neither is mistaken for
    # the other and neither buries the line it qualifies.
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.18, linewidth = 0.35,
                  alpha = 0.75, position = position_dodge(width = 0.35)) +
    geom_line(linewidth = 0.7, position = position_dodge(width = 0.35)) +
    geom_point(size = 1.5, position = position_dodge(width = 0.35)) +
    geom_point(aes(y = mean_pl), shape = 4, size = 1.8, stroke = 0.7,
               na.rm = TRUE, show.legend = FALSE,
               position = position_dodge(width = 0.35)) +
    { if (!is.null(.shade))
      geom_text(data = .shade, inherit.aes = FALSE,
                aes(x = lab_x, y = Inf), hjust = 0, vjust = 1.6,
                label = "conditional sample not comparable", size = 2.5,
                colour = "grey30") } +
    facet_wrap(~ panel, ncol = 1, scales = "free_y", strip.position = "top") +
    scale_colour_manual(values = c(Men = "#4C72B0", Women = "#B2182B")) +
    # THE AXIS IS THE DECLARED WINDOW, not whatever the data reach. A
    # floating axis makes a horizon that failed MIN_BIN look like a horizon
    # that was never asked for, and it lets a frame built elsewhere redraw
    # the figure on a different span with nothing on the page looking wrong.
    # coord_cartesian rather than scale limits, because the dodge moves the
    # end points off the integers and a scale limit would drop them.
    #
    # THE PAD IS ADDITIVE AND SMALL. The default 5 per cent expansion, on a
    # seventeen-year window and on top of a half-year pad, put close to a
    # year and a half of blank panel outside each end horizon, which reads
    # as horizons that were estimated and came back empty. 0.35 of a year
    # clears the dodge offset and the error-bar cap and nothing else.
    scale_x_continuous(breaks = seq(P3_KLO, P3_KHI, 2),
                       expand = ggplot2::expansion(add = 0.35)) +
    coord_cartesian(xlim = c(P3_KLO, P3_KHI)) +
    labs(title = "One margin moves and the other cannot be read",
         subtitle = paste("Upper: employer-financed training, per cent of",
                          "employed person-waves, bars are 95 per cent",
                          "intervals.\nLower: hours among TRAINEES ONLY, point",
                          "is the median, bar the INTERQUARTILE RANGE, cross the",
                          "mean.\nThe shaded region is where trainee status has",
                          "itself moved enough that the conditional sample is no",
                          "longer\nthe pre-birth one, so its level is not",
                          "comparable across the birth."),
         x = "Years from first birth", y = NULL, colour = NULL,
         caption = CAP_P) +
    theme_x + theme(legend.position = "bottom")
  svp(p, "P3_margin_split", h = 13)
  
  cat("\n  DESCRIPTIVE. Neither panel is an estimate: no controls, no\n")
  cat("  never-treated comparison, no Sun-Abraham weighting. The extensive\n")
  cat("  panel is the one a design can identify; the intensive panel is drawn\n")
  cat("  so that its flatness cannot be read as an absence of response.\n")
  rm(hh, ext, int, trim, both, .tr, ed3, ev3, .cover)
}

# ---- P4. The wage sample discards the response it is measuring ---------
# Section 4 makes the paper's central measurement argument in two numbers:
# a wage-based design discards 36.2 per cent of mothers' person-waves
# against 18.7 of never-mothers', so the women it drops are exactly those
# whose labour-supply response was largest, and such a design conditions on
# the outcome it is trying to measure. That argument is why the penalty is
# estimated on earnings rather than on wages, which is a choice the reader
# has to accept early and on trust.
#
# The two pooled numbers do not show WHEN the selection opens. By event
# time it opens at the birth wave and does not close, which is a stronger
# statement than either number and is the one the design rests on. A
# reader can then see that the gap in coverage arrives with the event
# rather than being a standing difference between the sexes.
#
# EVENT TIME IS BUILT HERE, the way the 03 design stage builds it and the
# way C5b above does, for the reason given there: diff() misses a
# transition across a missing wave and files the person as never-treated.
.wg <- V$wage
if (!is.na(.wg) && "ever_parent" %in% names(d)) {
  bwp <- d %>%
    filter(!is.na(ever_parent)) %>%
    arrange(person_id, wave) %>%
    group_by(person_id) %>%
    summarise(ep_at_entry = ever_parent[which.min(wave)],
              bw = if (any(ever_parent == 1)) min(wave[ever_parent == 1])
              else NA_real_,
              .groups = "drop") %>%
    filter(ep_at_entry == 0, !is.na(bw))
  # THE WINDOW, NAMED AND MATCHED TO THE OTHERS. This ran k = -4 to +8 while
  # P3 runs -8 to +8 and the two ordering figures -8 to +9, so the paper had
  # a fourth event axis and this one started four years later than the rest.
  # Nothing in the caption said why, and a reader comparing Figure 1 with
  # Figure 2 would have been comparing different leads.
  #
  # THE LEAD EXTENDS SAFELY HERE, unlike the intensive panel in P3. This
  # plots a coverage SHARE over every person-wave in the cell, not a median
  # over trainees, so the binding constraint is the at-risk count rather than
  # a conditional subsample. The block below cuts to the cells that clear
  # MIN_BIN and reports the cut, so a thin lead pulls the drawn range inside
  # this window rather than plotting noise.
  P4_KLO <- -8L
  P4_KHI <-  8L
  .w <- d %>%
    inner_join(bwp, by = "person_id") %>%
    mutate(k = wave - bw, sex = ifelse(female == 1, "Women", "Men")) %>%
    filter(k >= P4_KLO, k <= P4_KHI) %>%
    group_by(sex, k) %>%
    summarise(n = dplyr::n(),
              pct = 100 * mean(!is.na(.data[[.wg]])),
              se = 100 * sqrt(mean(!is.na(.data[[.wg]])) *
                                (1 - mean(!is.na(.data[[.wg]]))) / dplyr::n()),
              .groups = "drop") %>%
    filter(n >= MIN_BIN)
  if (nrow(.w)) {
    p <- ggplot(.w, aes(k, pct, colour = sex, fill = sex)) +
      geom_vline(xintercept = -0.5, linetype = "dotted", colour = "grey40") +
      geom_ribbon(aes(ymin = pct - 1.96 * se, ymax = pct + 1.96 * se),
                  alpha = 0.15, colour = NA) +
      geom_line(linewidth = 0.7) + geom_point(size = 1.5) +
      scale_colour_manual(values = c(Men = "#264653", Women = "#B2182B")) +
      scale_fill_manual(values = c(Men = "#264653", Women = "#B2182B")) +
      # Breaks follow the window rather than repeating its bounds. Hard-coded
      # as seq(-4, 8, 2), they labelled only the old range and would have left
      # the widened lead unmarked.
      scale_x_continuous(breaks = seq(P4_KLO, P4_KHI, 2)) +
      labs(title = "A wage-based design drops the women whose response was largest",
           subtitle = paste("Share of person-waves carrying a valid hourly wage,",
                            "by event time. The coverage gap opens\nAT the birth",
                            "and does not close, so a wage sample conditions on",
                            "the outcome it measures."),
           x = "Years from first birth", y = "With a valid hourly wage (%)",
           caption = CAP_P) +
      theme_x
    svp(p, "P4_wage_sample_selection")
    print(as.data.frame(.w[, c("sex", "k", "n", "pct")]), row.names = FALSE,
          digits = 3)
    rm(.w)
  }
  rm(bwp)
}

cat("\nDone. Figures in ", if (isTRUE(PUB)) PUBDIR else EXPL_DIR, "/\n", sep = "")
if (isTRUE(PUB)) cat("Manuscript candidates in ", PUBDIR, "/\n", sep = "")
cat("REMINDER: ", HALF, " half only. Fix the specification from these\n", sep = "")
cat("pictures, write it down, THEN estimate on the confirmation half.\n")



# =============================================================================
# THE TWO ORDERING FIGURES
#
# WERE Fig_ordering.R AND Fig_which_side_moves.R, two stages of their own.
# Folded in at v21.47 so that all figure plotting is one script, which is what
# it should have been: each defined its own theme_x and .wrap, duplicating
# this file's, and each had to be resolved separately by RUN_ALL.R. That
# resolution had already gone wrong in both directions on one run:
# fig_ordering came back unresolved while fig_decomp reported "2 candidates,
# 2 with the marker" and picked between two copies by modification time.
#
# THEY DO NOT READ THE PANEL. Both work from training_decomposition.csv and
# timing_paths.csv, which MASTER_training_y.R writes in sections 8.2 and
# 8.2b, so they need this file's theme and .wrap and nothing else from the
# preamble. If they stop before drawing, the message names the CSV and the
# section that writes it.
#
# THEIR SAVES ARE LEFT AS THEY WERE, ggsave to FIGDIR in both pdf and png,
# rather than routed through svp(), which switches format and directory on
# PUB. FIGDIR is set to PUBDIR below so they still land in the one figures
# folder.
# v27. THEY NO LONGER CARRY A TITLE, SUBTITLE OR CAPTION IN THE IMAGE. They
# bypassed svp() partly to keep captions the float did not supply. The
# floats now supply all of it, so both go through .paper_plain() before
# saving, in either PUB mode, because their files take the manuscript's
# names in both.
# =============================================================================
# FIGDIR IS RESOLVED ONCE. It was assigned here from PUBDIR and then
# hard-coded to "figures" twice below, so the routing this line describes
# was dead and setting PUBDIR would have split the figure set in two: these
# two under "figures" and the svp() renders under PUBDIR.
# v2. THE DESTINATION IS THE MANUSCRIPT'S OWN FOLDER, NOT A BARE STRING.
#
# "figures" resolves against getwd(), so a run from the root wrote TG/figures
# while \includegraphics{figures/...} resolves relative to the .tex and reads
# els-cas-templates/figures. The figures were written and the paper drew the
# previous ones, and the natural repair is to copy them across, which is how
# this project acquired two tables folders, two wage ledgers and two panels.
#
# PAPER_FIGURES is sealed in 00_contract.R beside PAPER_TABLES. An explicit
# FIGDIR still wins, for a caller that means one.
# v3. AN INHERITED FIGDIR IS NOT A CALLER'S CHOICE.
#
# v2 honoured any FIGDIR already in the session on the principle that an
# explicit value wins. The value it found was "figures", left there by THIS
# script's own previous run, so the contract's PAPER_FIGURES lost to a
# default the file had set itself an hour earlier and the figures went to
# TG/figures again. .eda_flag() already distinguishes inherited from intended
# for HALF and PUB, and prints a NOTE for each; FIGDIR had no such guard.
#
# A RELATIVE PATH IS A DEFAULT, AN ABSOLUTE ONE IS A DECISION. Nothing that
# means a particular folder writes "figures" and hopes; paths.R sets a full
# path, and so does the contract. So a relative FIGDIR is discarded in favour
# of PAPER_FIGURES, and an absolute one is kept and reported as inherited.
.fig_abs <- function(x) is.character(x) && length(x) == 1L &&
  grepl("^(/|\\\\|[A-Za-z]:[/\\\\])", x)
FIGDIR <- if (exists("FIGDIR", envir = globalenv()) &&
              .fig_abs(get("FIGDIR", envir = globalenv())))
  get("FIGDIR", envir = globalenv()) else
    if (exists("PAPER_FIGURES", envir = globalenv()))
      get("PAPER_FIGURES", envir = globalenv()) else
        if (exists("PUBDIR") && .fig_abs(PUBDIR)) PUBDIR else "figures"
dir.create(FIGDIR, showWarnings = FALSE, recursive = TRUE)
cat("  FIGDIR: ", normalizePath(FIGDIR, winslash = "/", mustWork = FALSE),
    if (!exists("PAPER_FIGURES", envir = globalenv()))
      "   (no contract; this may not be the folder the .tex reads)"
    else if (!identical(normalizePath(FIGDIR, winslash = "/", mustWork = FALSE),
                        normalizePath(get("PAPER_FIGURES", envir = globalenv()),
                                      winslash = "/", mustWork = FALSE)))
      "   (NOT the manuscript's folder; an absolute FIGDIR was inherited)"
    else "", "\n", sep = "")

# PUBDIR and EXPL_DIR were resolved from FIGDIR near the top of this file,
# before the block above ran, so they still carry the relative default. They
# name the same place now that FIGDIR does.
PUBDIR   <- FIGDIR
EXPL_DIR <- FIGDIR

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
# RESOLVED, NOT ASSUMED. These were bare filenames, so they resolved against
# the working directory while MASTER_training_y.R writes them through
# .out_path(), which resolves against project_dir. The two agreed only while
# the working directory WAS the root; once the scripts and the root diverged,
# this stopped and the stage died on "Not found: training_decomposition.csv"
# with the file sitting where the master had put it.
#
# Same fault as break 0.5 in the master, carried across when the two ordering
# drivers were folded in here: the writes were qualified and the reads were
# not. project_dir first, then the working directory and its parent, so a
# file from an earlier layout is still found rather than silently missed.
# v-fix. THIS NEVER LOOKED WHERE MASTER WRITES, AND IT WAS NOT NOTICED
# BECAUSE A STALE COPY WAS ALWAYS THERE TO FIND.
#
# MASTER writes training_decomposition.csv and timing_paths.csv through
# .out_path(), into DIR_RESULTS = output/results. This searched the project
# root, the working directory and its parent: three places MASTER stopped
# writing to when outputs moved under output/. In the OneDrive tree a copy
# from before that move was still sitting in the root, so the lookup
# succeeded, and the two T1 panels were drawn from a file several builds old
# while the verifier's Check 7b read the current one from output/results.
# The figure and the numbers it illustrates could disagree and nothing would
# have said so.
#
# IT SURFACED ONLY WHEN THE TREE WAS COPIED CLEAN on 21 September, with no
# root-level leftovers: "Not found: training_decomposition.csv". The error
# was the first honest answer this lookup had given.
#
# DIR_RESULTS FIRST, because it is the contract's sealed name for where
# these files are written. The old locations stay as fallbacks for a run
# outside the contract, and the resolved path is printed so the source of
# each panel is on the page.
.fig_in <- function(f) {
  .r <- if (exists("project_dir", envir = globalenv()))
          get("project_dir", envir = globalenv()) else
        if (exists("PROJ_ROOT", envir = globalenv()))
          get("PROJ_ROOT", envir = globalenv()) else getwd()
  .c <- unique(c(if (exists("DIR_RESULTS", envir = globalenv()))
                   file.path(get("DIR_RESULTS", envir = globalenv()), f),
                 file.path(.r, "output", "results", f),
                 file.path(.r, f), file.path(getwd(), f),
                 file.path(dirname(getwd()), f)))
  .h <- .c[file.exists(.c)]
  if (length(.h)) {
    cat("  ", f, " <- ", .h[1],
        if (length(.h) > 1)
          paste0("\n      (", length(.h) - 1, " other copy/ies found and IGNORED:",
                 " older builds that a stale lookup would have read)") else "",
        "\n", sep = "")
    .h[1]
  } else .c[1]
}
DECOMP  <- .fig_in("training_decomposition.csv")
TIMING  <- .fig_in("timing_paths.csv")
ARM     <- "employed_only"

# WHICH TRAINING ITEM, AND WHY THIS IS A LIST RATHER THAN A CONSTANT.
#
# The loader builds two pairs from the four module items:
#
#   jttpewt_any        = jttpewt                 place of employment only
#   training_paid_time = jttopwt | jttpewt       pooled across place
#
# Section 4.3 of the manuscript states that only the first is used, and that
# pooling the other-place items raises the measured decline by roughly fifteen
# per cent because it counts training on paid time away from the workplace as
# employer-financed. Both of these figures were nonetheless hard-coded to
# training_paid_time while their panel labels read employer-financed, so the
# label and the variable disagreed. On the employed-only arm the two differ by
# about 13 per cent in the gap and about 25 per cent on the mothers' own
# coefficient, which is the manuscript's own fifteen arriving in the figure.
#
# The manuscript measure is drawn under the canonical filename. The pooled one
# is drawn beside it under alt_, so the choice can be seen rather than
# asserted, and so that a reader of Section 4.3 can check what the exclusion
# is worth.
#
# BOTH alt_ FILES ARE INCLUDED BY THE .tex. This comment said nothing in the
# manuscript used them, and the manifest below reported them as optional on
# that basis. The two event-study figures both \includegraphics the pooled
# pair, because their object is the shape of the response rather than its
# size and Section 4.3 sets out the departure. The workplace pair still
# carries every coefficient quoted in the text and in Table 9, so both pairs
# are required and a missing alt_ file is an error rather than a diagnostic
# that was not wanted.
#
# LABEL IS FOR A FACET STRIP, SHORT IS FOR AN AXIS TITLE, and they are not
# the same string because they are read along different edges. A facet strip
# runs the width of a 16 cm canvas and takes a qualifying clause without
# noticing. The which-side figure's y axis runs the HEIGHT of a 10 cm one,
# and at base_size 9 a rotated title advances about 1.6 mm per character, so
# it has room for roughly sixty before the string is longer than the canvas
# is tall and ggplot clips it at the top without reporting anything. Built
# from label, "Paid-time training, pooled across place" plus "Change in" and
# ", reference k = -1" came to sixty-six, and the reference period was cut
# off the drawn figure. Where short is absent .axis_y() falls back to label
# with any clause after the first comma dropped, so a measure added later
# cannot reintroduce the overflow by omitting the field.
TRAIN_MEASURES <- list(
  list(outcome = "jttpewt_any",
       ord     = "T1_ordering",
       side    = "T1_which_side_moves",
       label   = "Employer-financed training",
       short   = "Employer-financed training",
       note    = "jttpewt: workplace, paid work time. The manuscript's measure."),
  list(outcome = "training_paid_time",
       ord     = "alt_T1_ordering_pooled",
       side    = "alt_T1_which_side_pooled",
       # v27. The notes' own phrase; the strip read "pooled across place".
       label   = "Paid-time training at any place",
       short   = "Paid-time training",
       note    = "jttopwt | jttpewt. Pooled across place; the pair the .tex draws.")
)

# The axis title, and a width guard that prints rather than clips. The
# threshold is the arithmetic above with a margin: 46 characters is about
# 74 mm against a 100 mm canvas, which leaves room for the axis text, the
# legend and the caption beneath the panel.
AXIS_Y_MAX <- 46L
.axis_y <- function(m) {
  s <- if (!is.null(m$short)) m$short else sub(",.*$", "", m$label)
  s <- paste0("Change in ", tolower(substr(s, 1, 1)), substring(s, 2))
  if (nchar(s) > AXIS_Y_MAX)
    cat("  *** y-axis title is ", nchar(s), " characters against a guard of ",
        AXIS_Y_MAX, "; it will be clipped on a 10 cm canvas: \"", s, "\"\n",
        sep = "")
  s
}

# THE EVENT WINDOW, ONE PAIR FOR BOTH FIGURES.
#
# The rule is the paper's own: a horizon is shown while its standard error
# stays under three times the birth-wave value. On jttpewt, employed-only,
# every series the two figures plot clears that at both edges of this
# window and the training side fails immediately outside it.
#
#            k = -8   k = -7   k = +9   k = +10
#   gap        2.17     1.95     2.47      3.27
#   mothers    2.21     1.89     2.40      3.27
#   fathers    2.09     2.04     2.57      3.27
#   wage       2.04       --     2.69      2.86
#
# THE UPPER CUT IS +9 AND NOT +10. Three of the four series jump to 3.27
# at +10 together, which is the same at-risk cells thinning rather than a
# judgement about any one of them. The training gap there is -0.059
# against a standard error of 0.071, so its interval contains zero and
# also contains the birth-wave estimate; drawn at the right edge it reads
# as the gap having closed, which is the one thing the data cannot say in
# either direction.
#
# THE WAGE SERIES COULD GO TO +10 and does not, because the panels share
# an axis and the binding series governs. The ten-year wage estimate of
# -0.119, which the abstract quotes, is therefore cited from the wage-path
# table rather than drawn, and the figure note has to say so.
#
# NOT +8 EITHER. On this item the fathers' arm clears five per cent at two
# post-birth horizons, k = +2 and k = +9. Cutting at +8 removes one of the
# two points that works against the which-side figure's own title, and a
# window trimmed until an inconvenient point falls outside it is the move
# Section 5.4 spends a page promising not to make.
#
# THE LEAD CUT IS -8, one horizon inside what the rule licenses. It puts
# the largest pre-birth deviations in the picture rather than outside it:
# the training gap is +0.077 at k = -8 and the wage gap +0.067, both the
# biggest lead values in their series and neither distinguishable from
# zero, at |t| = 1.64 and 1.76. A lead window that stops at -6 excludes
# them while the text calls the leads flat, which is conservative in the
# direction that flatters the claim. Drawing them is the stronger position.
# v2. FIG_K_*, NOT K_*. 00compat.R sets K_HI <- 8 for the descriptive window
# and 00_contract.R seals WAGE_K_HI at 10 for the reported wage path; this
# figure draws to +9, the training series' last horizon, because the two
# panels share an axis and the wage series' tenth year is reported in
# Appendix B instead. Three windows, three meanings, and while two of them
# shared the name K_HI the value depended on which file was sourced last.
FIG_K_LO <- -8L
FIG_K_HI <-  9L

# The flat window the paper's rule is anchored on: the birth wave and the
# four years following it. Section 7.2 states it and Check 7b asserts it, so
# it is named here rather than written into the sprintf below.
FIG_FLAT_TO <- 4L

for (f in c(DECOMP, TIMING))
  if (!file.exists(f))
    stop("Not found: ", f,
         "\n  Looked under project_dir, the working directory and its parent.",
         "\n  Written by MASTER_training_y.R section 8.2. ",
         "Run it first.", call. = FALSE)

# Crossings are read off the series, not asserted: a subtitle that cannot
# contradict its own figure is worth the lines it costs.
#
# A FIRST CROSSING IS NOT A SPAN, and the subtitle was reading one as the
# other. .cross() returned the first horizon at which a series clears five
# per cent, and the string built from it said "significant from k = 0",
# which asserts every later horizon the figure draws. On the training
# series that is false at the right edge: the gap clears at k = 0 and holds
# through k = +8, and at k = +9 the standard error has grown enough that
# the interval contains zero. On the wage series only k = +6 clears inside
# this window; it falls back at +7, +8 and +9 and clears again at +10,
# which is outside what the shared axis draws.
#
# So the run is computed. .sigrun() returns the first significant horizon
# and the end of the CONTIGUOUS run that starts there, which is the only
# span the word "from" can honestly describe.
.sigrun <- function(d) {
  d <- d %>% filter(k >= 0) %>% arrange(k)
  s <- abs(d$est / d$se) >= 1.96
  if (!any(s)) return(c(NA_integer_, NA_integer_))
  i <- which(s)[1]; j <- i
  while (j < length(s) && s[j + 1L]) j <- j + 1L
  c(d$k[i], d$k[j])
}
# %+d renders zero as "+0", which is how the subtitle came to read
# "significant from k = +0". Signed everywhere except at the origin.
.kf <- function(x) if (is.na(x)) "NA" else if (x == 0) "0" else sprintf("%+d", x)
# Three phrasings because three things can be true, and one template would
# have to misdescribe two of them. "Onward" is reserved for a run that
# reaches the right edge, which is the only case in which the figure has
# not seen the series stop clearing.
# WHEN THE POINT ESTIMATE TURNS, which is a different quantity from when it
# first clears five per cent and is the one the paper's lag rests on. The
# subtitle asserted the significance comparison as "the lag", and Section 8.2
# reports that comparison and then says it does not rest on it, so the figure
# was making a claim the text declines to make. Same rule as Check 7b: the
# band is the span of the point estimates over k = 0 to FIG_FLAT_TO, and the
# series turns at the first horizon outside it with no later horizon back
# inside.
.turnf <- function(d, flat) {
  d <- d %>% filter(k >= 0) %>% arrange(k)
  if (!nrow(d) || !any(d$k <= flat)) return(NA_integer_)
  b <- range(d$est[d$k <= flat])
  out <- d$k[d$est < b[1] | d$est > b[2]]
  for (kk in d$k)
    if (kk %in% out && all(d$k[d$k >= kk] %in% out)) return(as.integer(kk))
  NA_integer_
}

.spanf <- function(r, kmax) {
  if (is.na(r[1]))         "at no horizon drawn"
  else if (r[1] == r[2])   sprintf("at k = %s alone", .kf(r[1]))
  else if (r[2] >= kmax)   sprintf("from k = %s onward", .kf(r[1]))
  else                     sprintf("from k = %s through k = %s",
                                   .kf(r[1]), .kf(r[2]))
}


# =============================================================================
# FIGURE: INVESTMENT MOVES FIRST
#
# THEY DO NOT READ THE PANEL. Both work from training_decomposition.csv and
# timing_paths.csv, which MASTER_training_y.R writes in sections 8.2 and 8.2b,
# so they need this file's theme and .wrap and nothing else from the preamble.
#
# THEIR SAVES ARE LEFT AS THEY WERE, ggsave to FIGDIR in both pdf and png,
# rather than routed through svp(). Because they bypass svp() they also take
# no explore_ prefix, which the manifest at the end now knows.
# v27. The title, subtitle and caption are still built, from the series, and
# .paper_plain() prints them to the log and saves the figure without them.
# The float's caption and notes carry the words.
# =============================================================================

.tm_raw <- utils::read.csv(TIMING, stringsAsFactors = FALSE)
if (!all(c("wage_est", "wage_se") %in% names(.tm_raw)))
  stop(TIMING, " lacks wage_est/wage_se. Columns: ",
       paste(names(.tm_raw), collapse = ", "), call. = FALSE)
wg <- .tm_raw %>%
  filter(!is.na(wage_est), k >= FIG_K_LO, k <= FIG_K_HI) %>%
  transmute(k, est = wage_est, se = wage_se, panel = "Log hourly wage")

# v-age. THE AGE-ABSORBED WAGE PATH, DRAWN BESIDE THE MAIN ONE.
#
# The drift objection reaches the wage arm, and CS_event_study.R answers it
# with a second wage fit that adds age fixed effects for each sex. MASTER
# section 8.2 writes that path to timing_paths.csv as wage_age_est and
# wage_age_se, and the wage panel draws it as a dashed grey line with its
# own band. Everything the subtitle and the log compute, .sigrun, .turnf and
# the lag, still reads the main path in wg, so the overlay cannot move the
# ordering claim.
#
# ABSENT COLUMNS ARE A NOTE, NOT AN ERROR. An older timing_paths.csv, or a
# run in which the age arm did not fit, draws the figure as before, with no
# legend.
FIG_SPEC     <- c(main = "Main specification", age = "With age fixed effects")
FIG_AGE_NOTE <- paste("The dashed grey line in the lower panel adds age fixed",
                      "effects to each sex's wage event study.")
.spec_col   <- stats::setNames(c("#B2182B", "grey30"), FIG_SPEC)
.spec_alpha <- stats::setNames(c(0.15, 0.10),          FIG_SPEC)
.spec_lty   <- stats::setNames(c("solid", "22"),       FIG_SPEC)
.spec_shp   <- stats::setNames(c(16, 1),               FIG_SPEC)

wa <- NULL
if (all(c("wage_age_est", "wage_age_se") %in% names(.tm_raw))) {
  wa <- .tm_raw %>%
    filter(!is.na(wage_age_est), k >= FIG_K_LO, k <= FIG_K_HI) %>%
    transmute(k, est = wage_age_est, se = wage_age_se, panel = "Log hourly wage")
  if (!nrow(wa)) wa <- NULL
}
if (is.null(wa)) {
  cat("  NOTE: timing_paths.csv has no age-absorbed wage path, so the wage\n",
      "        panel draws the main specification alone. Re-run MASTER\n",
      "        section 8.2 after CS_event_study.R to add it.\n", sep = "")
} else {
  # Printed once rather than per measure, because both ordering figures
  # share this panel. These are the numbers the figure note describes.
  .cmp <- merge(wg[wg$k >= 0, c("k", "est", "se")],
                wa[wa$k >= 0, c("k", "est", "se")],
                by = "k", suffixes = c("_main", "_age"))
  cat("\n  wage gap, main specification against age fixed effects, k >= 0:\n")
  cat(sprintf("    k = %2s   %+.4f (%.4f)   %+.4f (%.4f)   difference %+.4f\n",
              vapply(.cmp$k, .kf, character(1)),
              .cmp$est_main, .cmp$se_main, .cmp$est_age, .cmp$se_age,
              .cmp$est_age - .cmp$est_main), sep = "")
  cat("  significant: main ", .spanf(.sigrun(wg), FIG_K_HI),
      "; age fixed effects ", .spanf(.sigrun(wa), FIG_K_HI), "\n", sep = "")
  rm(.cmp)
}
rm(.tm_raw)

.dec_all <- utils::read.csv(DECOMP, stringsAsFactors = FALSE)

# v26. THE TWO RUNS ARE COLLECTED AND COMPARED, not left in two console
# blocks fifty lines apart.
#
# WHY IT MATTERS. Each measure already prints its own span, and that was
# enough while the paper only ever quoted one of them. It does not any more:
# the findings summary states a run of eight years, which is the DRAWN
# all-places item, while every table and every triple difference is on the
# workplace item. Two correct spans reported separately read as one span,
# which is how a summary came to carry one arm's timing beside another arm's
# magnitudes.
#
# WHAT THE REPORT IS FOR. Section 4.3 of the .tex says the two runs differ
# and does not say by how much, because no artefact stated it. This block
# states it. Read the workplace line, write its end horizon into that
# sentence, and the claim stops being a comparison the reader has to take on
# trust.
.RUNS <- list()

for (.M in TRAIN_MEASURES) {
  
  cat("\n  ", .M$ord, ": ", .M$outcome, "\n     ", .M$note, "\n", sep = "")
  
  tr <- .dec_all %>%
    filter(arm == ARM, outcome == .M$outcome,
           k >= FIG_K_LO, k <= FIG_K_HI) %>%
    transmute(k, est = gap, se = gap_se, panel = .M$label)
  
  if (!nrow(tr)) {
    cat("  *** no rows for arm='", ARM, "', outcome='", .M$outcome,
        "'. Skipped.\n", sep = "")
    next
  }
  
  # PANEL ORDER IS FIXED, not alphabetical. The figure's argument is that
  # investment moves before price, so investment goes on top; ggplot would
  # otherwise order the facets by factor level and put the wage panel first.
  # v-age. spec separates the two wage paths. The training panel and the
  # main wage path keep the main specification's style.
  pl <- bind_rows(tr %>% mutate(spec = FIG_SPEC[["main"]]),
                  wg %>% mutate(spec = FIG_SPEC[["main"]]),
                  if (!is.null(wa)) wa %>% mutate(spec = FIG_SPEC[["age"]])) %>%
    mutate(lo = est - 1.96 * se, hi = est + 1.96 * se,
           panel = factor(panel, levels = c(.M$label, "Log hourly wage")),
           spec  = factor(spec, levels = unname(FIG_SPEC)))
  
  cat(sprintf("  training: %d horizons, k = %+d to %+d\n",
              nrow(tr), min(tr$k), max(tr$k)))
  cat(sprintf("  wage:     %d horizons, k = %+d to %+d\n",
              nrow(wg), min(wg$k), max(wg$k)))
  # THE TWO PANELS SHARE AN AXIS AND MUST SHARE A WINDOW. If timing_paths
  # does not reach K_LO the wage panel is short and the figure carries two
  # different windows without saying so, which is invisible once drawn.
  if (min(wg$k) > FIG_K_LO || min(tr$k) > FIG_K_LO)
    cat("  *** panels do not share a lead edge: training starts at ",
        min(tr$k), ", wage at ", min(wg$k), ". The axis will show the",
        " wider of the two with one panel blank at the left.\n", sep = "")
  
  # v-cw. The gap drawn is the common-weight one when MASTER wrote it; the
  # arms' own-weight difference is in gap_separate, for the log only.
  .dsep <- .dec_all %>%
    filter(arm == ARM, outcome == .M$outcome, k >= FIG_K_LO, k <= FIG_K_HI)
  if ("gap_separate" %in% names(.dsep)) {
    .dd <- .dsep$gap - .dsep$gap_separate
    cat(sprintf(paste0("  common less own-weight gap: largest %+.4f at k = %s; ",
                       "mean over k >= 0 %+.4f\n"),
                .dd[which.max(abs(.dd))], .kf(.dsep$k[which.max(abs(.dd))]),
                mean(.dd[.dsep$k >= 0])))
    rm(.dd)
  } else {
    cat("  *** training_decomposition.csv has no gap_separate column, so it was\n",
        "      written before the common-weight patch and its gap is on each arm's\n",
        "      own weights. Rerun MASTER_training_y.R.\n", sep = "")
  }
  rm(.dsep)

  .tr <- .sigrun(tr); .wr <- .sigrun(wg)
  .tk <- .tr[1];      .wk <- .wr[1]
  .wt <- .turnf(wg, FIG_FLAT_TO)
  
  # Recorded for the comparison after the loop. The figure name is carried
  # so the report can say which of the two the .tex draws without a second
  # list to fall out of step with TRAIN_MEASURES.
  .RUNS[[.M$outcome]] <- list(
    outcome = .M$outcome, fig = .M$ord, label = .M$label,
    from = .tr[1], to = .tr[2], k_hi = FIG_K_HI,
    est0 = tr$est[tr$k == 0][1],
    gaps = tr$k[tr$k >= 0 & abs(tr$est / tr$se) < 1.96])
  
  # WRAPPING, not hand-placed newlines. The subtitle and caption previously
  # carried "\n" at points chosen by eye, and ran off the right edge the
  # moment the text changed: a subtitle line of ~150 characters is cut at
  # 16 cm and base_size 9, where ~110 fits. The numbers in these strings are
  # computed, so their width is not known when the string is written and any
  # hand-placed break is a guess that expires.
  p <- ggplot(pl, aes(k, est, colour = spec, fill = spec,
                      linetype = spec, shape = spec)) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey40") +
    geom_vline(xintercept = -0.5, linetype = 3, linewidth = 0.3) +
    geom_ribbon(aes(ymin = lo, ymax = hi, alpha = spec), colour = NA,
                show.legend = FALSE) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.5) +
    scale_colour_manual(values = .spec_col) +
    scale_fill_manual(values = .spec_col, guide = "none") +
    scale_alpha_manual(values = .spec_alpha, guide = "none") +
    scale_linetype_manual(values = .spec_lty) +
    scale_shape_manual(values = .spec_shp) +
    facet_wrap(~ panel, ncol = 1, scales = "free_y") +
    scale_x_continuous(breaks = seq(FIG_K_LO, FIG_K_HI, 2)) +
    labs(x = "Years from first birth", y = "Women \u2212 men",
         title = "Investment moves first",
         subtitle = if (!is.na(.tk) && !is.na(.wk))
           .wrap(sprintf(paste("The training gap is significant %s and the",
                               "wage gap %s.%s"),
                         .spanf(.tr, FIG_K_HI), .spanf(.wr, FIG_K_HI),
                         if (is.na(.wt))
                           sprintf(" Investment leads price by %d waves on",
                                   .wk - .tk)
                         else
                           sprintf(paste(" The wage point estimate leaves the",
                                         "band its first %d horizons span at",
                                         "k = %s, so investment leads price by",
                                         "%d years on the point estimates and",
                                         "%d on first significance."),
                                   FIG_FLAT_TO + 1L, .kf(.wt),
                                   .wt - .tk, .wk - .tk))) else
                           "Crossings could not be read from these series.",
         caption = .wrap(
           "Sun-Abraham event-study estimates, women minus men, formed within",
           "each cohort-by-event cell and averaged on common cohort weights,",
           "HILDA Waves 12-24. Bands are 95 per cent confidence intervals. The vertical",
           "scales differ: the upper panel is a probability and the lower a",
           "log wage, so the comparison to make is horizontal. The subtitle",
           "reads significance at five per cent, and a span that ends before",
           "the right edge means the later horizons drawn do not clear it",
           "rather than that the gap has closed.",
           if (!is.null(wa)) FIG_AGE_NOTE else "")) +
    theme_x +
    # v-age. One specification means no legend: the figure as it was.
    (if (is.null(wa)) theme(legend.position = "none"))
  p <- .paper_plain(p, .M$ord)   # v27: the words go to the log, not the image

  ggsave(file.path(FIGDIR, paste0(.M$ord, ".pdf")), p,
         width = 16/2.54, height = 12/2.54,
         device = grDevices::cairo_pdf)   # embeds its fonts; see svp()
  ggsave(file.path(FIGDIR, paste0(.M$ord, ".png")), p,
         width = 16/2.54, height = 12/2.54, dpi = 200)
  cat("  wrote ", FIGDIR, "/", .M$ord, ".pdf and .png\n", sep = "")
  cat("  training significant ", .spanf(.tr, FIG_K_HI),
      ", wage ", .spanf(.wr, FIG_K_HI), "\n",
      "  wage point estimate turns at k = ", .kf(.wt),
      "; lag ", if (is.na(.wt)) NA_integer_ else .wt - .tk,
      " on the point estimates, ", .wk - .tk, " on first significance\n",
      sep = "")
  # The horizons inside the window that do NOT clear, so the subtitle's
  # "through" can be checked against the series rather than trusted.
  # .kf is scalar by construction, so the vector goes through vapply
  # rather than being handed to an if() that would take its first element.
  .ns <- function(d) {
    v <- d$k[d$k >= 0 & abs(d$est / d$se) < 1.96]
    if (!length(v)) "none" else
      paste(vapply(v, .kf, character(1)), collapse = ", ")
  }
  cat("  not significant, k >= 0: training ", .ns(tr),
      " | wage ", .ns(wg), "\n", sep = "")
}

# --- the two training measures' significant runs, side by side ---------------
#
# THE DRAWN ITEM IS NAMED FROM THE .tex, not from a habit. Whichever of the
# two figure names appears in an \includegraphics line is the one the
# manuscript draws; if the .tex cannot be read the report still prints both
# runs and says only that it could not tell which is drawn, because a guess
# here would be a guess about the sentence in Section 4.3 that cites it.
if (length(.RUNS) >= 1L) {
  .drawn <- NA_character_
  # LENGTH FIRST, then value. get0 can return a zero-length character, and
  # `!is.na(character(0)) && ...` is an error in R 4.3 rather than FALSE,
  # which would abort the figure run over a name that merely was not set.
  .texf <- Filter(function(p) is.character(p) && length(p) == 1L &&
                    !is.na(p) && nzchar(p) && file.exists(p),
                  list(get0("MANUSCRIPT", ifnotfound = NA_character_),
                       get0("PAPER_TEX",  ifnotfound = NA_character_)))
  if (length(.texf)) {
    .txt <- paste(readLines(.texf[[1]], warn = FALSE), collapse = "\n")
    .txt <- gsub("(?m)(?<!\\\\)%.*$", "", .txt, perl = TRUE)
    .hit <- vapply(.RUNS, function(r)
      grepl(paste0("includegraphics[^{]*\\{[^}]*", r$fig, "\\b"), .txt,
            perl = TRUE), logical(1))
    if (any(.hit)) .drawn <- names(.RUNS)[.hit][1]
  }
  
  cat("\n  the two training measures' significant runs, k >= 0:\n")
  for (.r in .RUNS) {
    .end <- if (is.na(.r$to)) "none" else
      if (.r$to >= .r$k_hi) paste0(.kf(.r$to), " (the right edge)") else .kf(.r$to)
    cat(sprintf("    %-26s %-24s k = %s to %s%s\n",
                .r$outcome, .r$fig, .kf(.r$from), .end,
                if (identical(.r$outcome, .drawn)) "   <- drawn by the .tex" else ""))
    cat(sprintf("      birth-wave estimate %+.4f; inside the window and NOT clearing: %s\n",
                .r$est0,
                if (!length(.r$gaps)) "none"
                else paste(vapply(.r$gaps, .kf, character(1)), collapse = ", ")))
  }
  
  # A COMPARISON NEEDS TWO ARMS. One measure can be skipped above when the
  # decomposition holds no rows for it, and a single-element .RUNS would
  # otherwise report the two runs as agreeing, which is the one verdict a
  # missing arm must never produce.
  .ends <- vapply(.RUNS, function(r) as.integer(r$to), integer(1))
  if (length(.RUNS) < 2L) {
    cat("  only one measure produced a run, so there is nothing to compare.\n",
        "  Section 4.3's sentence about the two runs is UNCHECKED this pass.\n",
        sep = "")
  } else if (length(unique(.ends[!is.na(.ends)])) > 1L) {
    cat("  *** THE RUNS END AT DIFFERENT HORIZONS. Section 4.3 of the .tex\n",
        "      says they differ and does not say by how much; the numbers\n",
        "      above are what that sentence should carry. The findings\n",
        "      summary states the DRAWN item's run and names the arm, so a\n",
        "      change here is a change to Section 4.3 first and to the\n",
        "      summary only if the drawn item's own run has moved.\n", sep = "")
  } else if (!anyNA(.ends)) {
    cat("  the two runs end at the same horizon, so the distinction\n",
        "  Section 4.3 draws is currently one the data do not require.\n",
        "  Read that sentence before the next compile.\n", sep = "")
  }
  if (is.na(.drawn))
    cat("  (the .tex was not read, so neither run is marked as drawn. Set\n",
        "   MANUSCRIPT to the manuscript path before sourcing this file.)\n",
        sep = "")
  rm(.drawn, .texf, .ends)
  if (exists(".txt")) rm(.txt)
  if (exists(".hit")) rm(.hit)
  if (exists(".r"))   rm(.r)
  if (exists(".end")) rm(.end)
}


# =============================================================================
# FIGURE: WHICH SIDE MOVES
# =============================================================================

# THE WINDOW IS THE SHARED ONE DECLARED ABOVE, k = -8..+9. Both figures use
# it, so the two can be read side by side without the reader adjusting for a
# different axis, and the realised standard-error ratios are printed below
# so the rule can be checked against the item actually drawn rather than
# against numbers hard-coded in a comment.

# The palette is the one RUN_FIGURES_EDA.R uses for these two groups, so the
# figure reads as part of the same set rather than as an import.
pal2 <- c("Mothers vs never-mothers" = "#B2182B",
          "Fathers vs childless men" = "#264653")

for (.M in TRAIN_MEASURES) {
  
  cat("\n  ", .M$side, ": ", .M$outcome, "\n", sep = "")
  
  dec <- .dec_all %>%
    filter(arm == ARM, outcome == .M$outcome,
           k >= FIG_K_LO, k <= FIG_K_HI)
  
  if (!nrow(dec)) {
    cat("  *** no rows for arm='", ARM, "', outcome='", .M$outcome,
        "'. Available: ",
        paste(unique(paste(.dec_all$arm, .dec_all$outcome))[1:4],
              collapse = " | "), "\n", sep = "")
    next
  }
  
  cat(sprintf("  %d horizons, k = %+d to %+d, %s %s\n",
              nrow(dec), min(dec$k), max(dec$k), ARM, .M$outcome))
  
  # THE CUT RULE, COMPUTED. Standard errors as multiples of the birth wave,
  # so the K_HI choice above can be checked against the item actually drawn.
  .se_ratio <- dec$mothers_se[dec$k >= 0] / dec$mothers_se[dec$k == 0]
  cat("  mothers SE / birth-wave SE, k = 0..", max(dec$k), ": ",
      paste(sprintf("%.2f", .se_ratio), collapse = " "), "\n", sep = "")
  
  # ---------------------------------------------------------------------
  # Long form, one row per sex per horizon
  # ---------------------------------------------------------------------
  pl <- bind_rows(
    dec %>% transmute(k, est = mothers, se = mothers_se,
                      path = "Mothers vs never-mothers"),
    dec %>% transmute(k, est = fathers, se = fathers_se,
                      path = "Fathers vs childless men")) %>%
    mutate(lo = est - 1.96 * se, hi = est + 1.96 * se,
           path = factor(path, levels = c("Mothers vs never-mothers",
                                          "Fathers vs childless men")))
  
  # THE SUBTITLE IS BUILT FROM THE DATA rather than asserted.
  #
  # THE SHARE IS SIGNED, AND IT IS NOT IN THE SUBTITLE. It was
  # 100*abs(m0)/(abs(m0)+abs(f0)), which treats a fathers' coefficient of
  # the opposite sign as adding to the denominator. On jttpewt_any fathers
  # move +0.015 at the birth wave, so they WIDEN the gap; the absolute
  # formula books that as a narrowing contribution and returns 87 per cent
  # for the wrong reason, while on training_paid_time the signed share is
  # 101 per cent, which is correct and unprintable. What is robust to both
  # items is the fathers' t-ratio, so that is what the subtitle carries and
  # the share is printed below for the text to quote.
  .m0 <- dec$mothers[dec$k == 0]; .f0 <- dec$fathers[dec$k == 0]
  # v2. RENAMED FROM .fathers_t0. RUN_ALL.R uses .fathers_t0 for its start time, and this
# assigned it the fathers' t-statistic at the birth wave, 0.09. Elapsed then
# read 29,805,102.9 minutes, which is 0.09 seconds after the epoch, and
# .fresh() and the artefact ages were measured from 1970 for the rest of the
# run. A stage must not use a name the driver holds.
.fathers_t0 <- abs(dec$fathers_t[dec$k == 0])
  # The share divides by the arms' own-weight difference, m0 - f0, because it
  # apportions that difference between the two lines as drawn. It is not the
  # common-weight gap, which the band and the subtitle use.
  .sh <- 100 * .m0 / (.m0 - .f0)
  
  # THE GAP IS SHADED, and it is the reason this version exists. Trimming
  # the window to +9 left both lines ending at their lowest point, and two
  # parallel descents in the last third read as the sexes moving together,
  # which is the opposite of the title. What is stable is the vertical
  # distance: it opens at the birth and neither widens nor closes.
  #
  # POST-BIRTH ONLY. The two paths weave across each other before the event,
  # so shading there would fill a region that changes sign and mean nothing.
  #
  # v-cw. THE BAND IS THE COMMON-WEIGHT GAP, hung from the fathers' line.
  # Drawn from the fathers' line to the mothers' line it would be each arm on
  # its own weights, which is not the gap the ordering figure plots or the
  # text quotes. The two lines stay as estimated, so the band's upper edge
  # can sit slightly off the mothers' line; the log gives the distance, which
  # is the composition term horizon by horizon.
  gapband <- dec %>% filter(k >= 0) %>%
    transmute(k, ymin = fathers, ymax = fathers + gap)
  .off <- dec %>% filter(k >= 0) %>% transmute(k, d = (fathers + gap) - mothers)
  cat(sprintf(paste0("  band edge less mothers' line (common less own-weight gap): ",
                     "largest %+.4f at k = %s\n"),
              .off$d[which.max(abs(.off$d))], .kf(.off$k[which.max(abs(.off$d))])))
  rm(.off)
  
  p <- ggplot(pl, aes(k, est, colour = path, fill = path)) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey40") +
    geom_vline(xintercept = -0.5, linetype = 3, linewidth = 0.3) +
    geom_ribbon(data = gapband, inherit.aes = FALSE,
                aes(x = k, ymin = ymin, ymax = ymax),
                fill = "grey45", alpha = 0.16) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.12,
                colour = NA, show.legend = FALSE) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.5) +
    scale_colour_manual(values = pal2) +
    scale_fill_manual(values = pal2) +
    scale_x_continuous(breaks = seq(FIG_K_LO, FIG_K_HI, 2)) +
    labs(x = "Years from first birth",
         # NOT "training rate". These are event-study coefficients against
         # never-treated controls with k = -1 omitted, so -0.102 is a change
         # relative to the reference period and not a rate. An axis labelled
         # as a rate invites reading the birth-wave point as minus ten per
         # cent of training rather than as a fall of ten points.
         #
         # THE REFERENCE PERIOD IS IN THE CAPTION, not here. Naming it on
         # the axis is what pushed the title past the height of the canvas,
         # and the clause it added was the part that got clipped, so the
         # drawn figure carried the cost and none of the benefit.
         # v27. The in-image caption is no longer drawn, so the float's
         # note now says it: "Each series is the change from k = -1".
         y = .axis_y(.M),
         title = "The gap is mothers falling, not fathers rising",
         subtitle = .wrap(sprintf(paste("At the birth wave mothers move %+.3f",
                                        "against never-mothers while fathers",
                                        "move %+.3f against childless men, at",
                                        "t = %.2f. The shaded gap opens at the",
                                        "event and holds near %.2f; both paths",
                                        "drift at long lags, the distance",
                                        "between them does not."),
                                  .m0, .f0, .fathers_t0,
                                  abs(mean(dec$gap[dec$k >= 0])))),
         # NO FIGURE NUMBER IN THIS STRING. The manuscript owns the numbering
         # and its \caption cross-references already. This read "Figure 4
         # plots that gap" inside Figure 4.
         caption = .wrap("Sun-Abraham event-study estimates against",
                         "never-treated controls of the same sex, HILDA Waves",
                         "12-24, employed-only arm. Each series is the change",
                         "from k = -1, the omitted reference period, and not a",
                         "training rate. Bands are 95 per cent confidence",
                         "intervals; the grey region is the gap the ordering",
                         "figure plots as a single line, on common cohort",
                         "weights, measured from the fathers' line.")) +
    theme_x
  p <- .paper_plain(p, .M$side)  # v27: the words go to the log, not the image

  ggsave(file.path(FIGDIR, paste0(.M$side, ".pdf")), p,
         width = 16/2.54, height = 10/2.54,
         device = grDevices::cairo_pdf)   # embeds its fonts; see svp()
  ggsave(file.path(FIGDIR, paste0(.M$side, ".png")), p,
         width = 16/2.54, height = 10/2.54, dpi = 200)
  cat("  wrote ", FIGDIR, "/", .M$side, ".pdf and .png\n", sep = "")
  
  # -------------------------------------------------------------------
  # What the figure shows, printed so the caption and the manuscript can
  # be checked against it
  # -------------------------------------------------------------------
  cat("  k = 0:  mothers ", sprintf("%+.4f", .m0),
      " | fathers ", sprintf("%+.4f", .f0),
      " (t = ", sprintf("%.2f", .fathers_t0), ")",
      " | mothers' signed share of the gap ", sprintf("%.0f%%", .sh),
      "\n", sep = "")
  .pre <- dec %>% filter(k < 0)
  cat(sprintf("  pre-birth: mothers %+.4f to %+.4f, fathers %+.4f to %+.4f\n",
              min(.pre$mothers), max(.pre$mothers),
              min(.pre$fathers), max(.pre$fathers)))
  .fs <- dec %>% filter(k >= 0, abs(fathers_t) >= 1.96)
  cat("  post-birth horizons significant: mothers ",
      sum(dec$k >= 0 & abs(dec$mothers_t) >= 1.96), " of ", sum(dec$k >= 0),
      ", fathers ", nrow(.fs), " of ", sum(dec$k >= 0),
      if (nrow(.fs)) paste0(" (at k = ", paste0("+", .fs$k, collapse = ", "),
                            ")") else "", "\n", sep = "")
  cat(sprintf("  gap over k >= 0: %+.3f to %+.3f, mean %+.3f\n",
              min(dec$gap[dec$k >= 0]), max(dec$gap[dec$k >= 0]),
              mean(dec$gap[dec$k >= 0])))
}

cat("\n  THE MANUSCRIPT'S NUMBERS COME FROM THE FIRST BLOCK ABOVE, the\n")
cat("  jttpewt_any one. Section 7.2 and Table 8 must quote that block.\n")
cat("  THE DRAWN FIGURES ARE THE SECOND BLOCK, the pooled one, so the\n")
cat("  subtitles and axis titles printed above belong to it and the .tex\n")
cat("  float notes must reconcile the two, which Section 4.3 does.\n")


# =============================================================================
# FIGURE: THE FINANCING CONTRAST AS AN EVENT STUDY
#
# v28. ADDED IN REVISION, for Section 7.1. That section fits equation (es) by
# sex to employer-financed less own-time training within each employed
# person-wave, and quotes the post-birth average and the joint test of the
# leads. This figure draws the path that both numbers summarize: women less
# men, by year from the first birth.
#
# IT DRAWS NOTHING FROM THE PANEL. The SADDD block of 06_revision_inference.R
# writes the path to output/diagnostics/revision_financing_path.csv: each
# sex's coefficients, and the gap with standard errors that add the two
# sexes' covariance matrices. The panel's build tag is used only to check the
# path's build. Like the T1 pairs, the figure goes to FIGDIR as pdf and png
# under the name the manuscript uses, through .paper_plain(), in either PUB
# mode.
#
# IT IS DRAWN ONLY WHEN THE TEXT CAN QUOTE IT. paper_tables.R prints
# \SADddATT only if 06 marked its row ok, on this build of the panel, and no
# file 06 checked it against has gone or changed since. The same tests run
# here, so the figure cannot show a path whose average the text withholds.
# One test is the figure's own: the path file must be at least as new as the
# run that wrote the row. 06 writes the path partway through its block, so a
# block that fails before then leaves the old path in place.
#
# THE WINDOW IS THE SHARED ONE, FIG_K_LO to FIG_K_HI, so the three
# event-study figures sit on one axis. The precision rule that set that
# window is printed for this series, not applied: a horizon past it is named
# in the log and still drawn, and the float's note should say so.
#
# A FAILURE HERE DOES NOT STOP THE RUN. The block runs after every other
# figure, and stopping would skip the manifest and the copy to the paper's
# folder for all of them. A failure is reported instead, and the manifest
# repeats it.
# =============================================================================
FC_FIG   <- "SA_financing_contrast"
FC_PANEL <- "Employer-financed less own-time training"
FC_COL   <- "#B2182B"     # the gap's colour in the ordering figure
FC_LEADS <- -8:-2         # the leads Section 7.1 tests jointly, 06's REV_LEADS
FC_RULE  <- 3             # SE at most this multiple of the birth-wave SE

# 06 writes into DIR_DIAG, which .fig_in() does not search.
.fig_diag <- function(f) {
  .r <- if (exists("project_dir", envir = globalenv()))
          get("project_dir", envir = globalenv()) else
        if (exists("PROJ_ROOT", envir = globalenv()))
          get("PROJ_ROOT", envir = globalenv()) else getwd()
  .c <- unique(c(if (exists("DIR_DIAG", envir = globalenv()))
                   file.path(get("DIR_DIAG", envir = globalenv()), f),
                 file.path(.r, "output", "diagnostics", f)))
  .h <- .c[file.exists(.c)]
  if (!length(.h)) return(NA_character_)
  cat("  ", f, " <- ", .h[1], "\n", sep = "")
  .h[1]
}

cat("\n  ", FC_FIG, ": the financing contrast as an event study\n", sep = "")
.fc_pf  <- .fig_diag("revision_financing_path.csv")
.fc_sf  <- .fig_diag("revision_inference_summary.csv")
.fc_raw <- if (is.na(.fc_pf)) NULL else
  tryCatch(utils::read.csv(.fc_pf, stringsAsFactors = FALSE),
           error = function(e) NULL)
.fc_sum <- if (is.na(.fc_sf)) NULL else
  tryCatch(suppressWarnings(utils::read.csv(
    .fc_sf, stringsAsFactors = FALSE,
    colClasses = c(macro = "character", status = "character",
                   build_tag = "character", written = "character",
                   checked = "character", files = "character"))),
    error = function(e) NULL)
# One macro's row of the summary: its value and status.
.fc_val <- function(m) {
  r <- if (is.null(.fc_sum) || !("macro" %in% names(.fc_sum))) NULL else
    .fc_sum[!is.na(.fc_sum$macro) & .fc_sum$macro == m, , drop = FALSE]
  if (is.null(r) || nrow(r) != 1L) return(list(v = NA_real_, s = "absent"))
  list(v = suppressWarnings(as.numeric(r$value)), s = r$status)
}

# Why the figure cannot be drawn, or "" if it can. paper_tables.R's tests on
# \SADddATT, in its order, then the path file's own. 06 writes times in UTC.
.fc_gate <- function() {
  if (is.na(.fc_pf))
    return("06_revision_inference.R has not written revision_financing_path.csv")
  if (is.null(.fc_raw) || !all(c("event_time", "gap", "gap_se") %in% names(.fc_raw)))
    return(paste("revision_financing_path.csv cannot be read, or lacks",
                 "event_time, gap or gap_se"))
  if (!is.numeric(.fc_raw$event_time) || !is.numeric(.fc_raw$gap) ||
      !is.numeric(.fc_raw$gap_se))
    return("revision_financing_path.csv has a column that is not numeric")
  if (!any(.fc_raw$event_time %in% 0 & is.finite(.fc_raw$gap) &
           is.finite(.fc_raw$gap_se) & .fc_raw$gap_se > 0))
    return("revision_financing_path.csv has no usable birth-wave estimate")
  if (is.null(.fc_sum))
    return(paste("revision_inference_summary.csv is missing, so nothing says",
                 "06 reproduced the contrast"))
  if (!all(c("macro", "status", "build_tag", "written", "checked", "files") %in%
           names(.fc_sum)))
    return("revision_inference_summary.csv has an older layout")
  r <- .fc_sum[!is.na(.fc_sum$macro) & .fc_sum$macro == "SADddATT", , drop = FALSE]
  if (nrow(r) != 1L)
    return(sprintf("revision_inference_summary.csv has %d SADddATT rows", nrow(r)))
  if (!identical(r$status, "ok"))
    return(sprintf("06 marked the contrast \"%s\"", r$status))
  b <- if (is.null(.bt)) NA_character_ else as.character(.bt)
  if (!is.na(b) && !identical(r$build_tag, b))
    return(sprintf("06 ran on build %s, and this panel is build %s", r$build_tag, b))
  utc <- function(x) as.POSIXct(x, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  ck <- utc(r$checked); wr <- utc(r$written)
  if (is.na(ck) || is.na(wr))
    return("the summary does not say when 06 checked the contrast")
  f <- if (is.na(r$files)) character(0) else strsplit(r$files, "|", fixed = TRUE)[[1]]
  f <- f[nzchar(f) & !startsWith(f, "MISSING:")]
  if (!length(f))
    return("the summary does not name the files 06 checked the contrast against")
  gone <- f[!file.exists(f)]
  if (length(gone))
    return(sprintf("%s is no longer where 06 read it",
                   paste(basename(gone), collapse = " and ")))
  moved <- f[file.mtime(f) > ck]
  if (length(moved))
    return(sprintf("06 checked the contrast at %s UTC, and %s has changed since",
                   substr(r$checked, 1, 19), paste(basename(moved), collapse = " and ")))
  if (file.mtime(.fc_pf) < wr - 1)
    return(sprintf(paste("revision_financing_path.csv is older than the run of 06",
                         "that wrote the contrast, at %s UTC"), substr(r$written, 1, 19)))
  ""
}
.fc_withheld <- tryCatch(.fc_gate(), error = function(e)
  paste0("the check stopped: ", conditionMessage(e)))

if (nzchar(.fc_withheld)) {
  cat("  *** NOT DRAWN: ", .fc_withheld, ".\n",
      "      Rerun 06_revision_inference.R (REV_BOOT_B=0 is enough for this\n",
      "      figure), then this file.\n", sep = "")
  .fc_old <- file.path(FIGDIR, paste0(FC_FIG, ".pdf"))
  if (file.exists(.fc_old))
    cat("      The copy in FIGDIR, from ",
        format(file.info(.fc_old)$mtime, "%Y-%m-%d %H:%M"),
        ", is left in place, and the manuscript\n",
        "      draws it until then. The manifest below marks it stale.\n", sep = "")
  rm(.fc_old)
} else tryCatch({

  .fc <- .fc_raw %>%
    transmute(k = as.integer(round(event_time)), est = gap, se = gap_se) %>%
    filter(k >= FIG_K_LO, k <= FIG_K_HI, is.finite(est), is.finite(se)) %>%
    arrange(k)
  cat(sprintf("  %d horizons, k = %s to %s\n", nrow(.fc),
              .kf(min(.fc$k)), .kf(max(.fc$k))))

  # WHAT THE WINDOW HOLDS. k = -1 is the omitted reference, so it is never in
  # the path; anything else missing is named.
  .fc_miss <- setdiff(FIG_K_LO:FIG_K_HI, c(.fc$k, -1L))
  if (length(.fc_miss))
    cat("  *** no estimate at k = ",
        paste(vapply(.fc_miss, .kf, character(1)), collapse = ", "),
        ", so the figure has no point there.\n", sep = "")
  .fc_miss <- setdiff(FC_LEADS, .fc$k)
  if (length(.fc_miss))
    cat("  *** Section 7.1 tests the leads k = ", .kf(min(FC_LEADS)), " to ",
        .kf(max(FC_LEADS)), " jointly, and the figure has no estimate at k = ",
        paste(vapply(.fc_miss, .kf, character(1)), collapse = ", "), ".\n", sep = "")

  # THE PATH, printed so the note can be checked against it. The last column
  # is the precision rule, computed: each SE as a multiple of the birth-wave
  # SE. The shared window was set where the training series stay under
  # FC_RULE.
  .fc_rat <- .fc$se / .fc$se[.fc$k == 0]
  cat("      k       gap       SE       t   SE/SE(0)\n")
  cat(sprintf("    %3s  %+.4f  %.4f  %6.2f  %8.2f\n", vapply(.fc$k, .kf, character(1)),
              .fc$est, .fc$se, .fc$est / .fc$se, .fc_rat), sep = "")
  .fc_over <- .fc$k[.fc_rat >= FC_RULE]
  if (length(.fc_over))
    cat("  *** at k = ", paste(vapply(.fc_over, .kf, character(1)), collapse = ", "),
        " the SE is at least ", FC_RULE, " times the birth-wave SE, past the\n",
        "      rule that set the shared window. Drawn, so the three event-study\n",
        "      figures share one axis; the note should name these horizons.\n", sep = "")
  .fc_run <- .sigrun(.fc)
  .fc_ns  <- .fc$k[.fc$k >= 0 & abs(.fc$est / .fc$se) < 1.96]
  cat("  significant at 5 percent ", .spanf(.fc_run, FIG_K_HI),
      "; not significant at k >= 0: ",
      if (length(.fc_ns)) paste(vapply(.fc_ns, .kf, character(1)), collapse = ", ")
      else "none", "\n", sep = "")
  .fc_ld <- .fc[.fc$k %in% FC_LEADS, , drop = FALSE]
  if (nrow(.fc_ld)) {
    .fc_ld <- .fc_ld[which.max(abs(.fc_ld$est / .fc_ld$se)), ]
    cat(sprintf("  largest lead: %+.4f at k = %s, |t| = %.2f\n", .fc_ld$est,
                .kf(.fc_ld$k), abs(.fc_ld$est / .fc_ld$se)))
  }

  # THE TEXT'S THREE NUMBERS, from the same summary, so the figure and the
  # sentence beside it can be read together in this log.
  .fc_a <- .fc_val("SADddATT"); .fc_s <- .fc_val("SADddSE"); .fc_p <- .fc_val("SADddPreP")
  cat(sprintf(paste0("  Section 7.1's numbers, from the summary: post-birth average ",
                     "%+.4f (SE %.4f),\n  leads p = %.3f\n"),
              .fc_a$v, .fc_s$v, .fc_p$v))
  for (.fc_x in list(c("SADddSE", .fc_s$s), c("SADddPreP", .fc_p$s)))
    if (!identical(.fc_x[2], "ok"))
      cat("  *** ", .fc_x[1], " is \"", .fc_x[2], "\", so paper_tables.R leaves it ",
          "\\tofill.\n", sep = "")

  .fc$lo    <- .fc$est - 1.96 * .fc$se
  .fc$hi    <- .fc$est + 1.96 * .fc$se
  .fc$panel <- FC_PANEL
  # A ONE-PANEL FACET, for the strip. It names the outcome the way the
  # ordering figure's strips do, so the y axis can read "Women - men" there
  # and here.
  p <- ggplot(.fc, aes(k, est)) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey40") +
    geom_vline(xintercept = -0.5, linetype = 3, linewidth = 0.3) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = FC_COL, alpha = 0.15,
                colour = NA) +
    geom_line(colour = FC_COL, linewidth = 0.7) +
    geom_point(colour = FC_COL, size = 1.5) +
    facet_wrap(~ panel) +
    scale_x_continuous(breaks = seq(FIG_K_LO, FIG_K_HI, 2)) +
    # The axis is the shared window even when an edge horizon is missing.
    coord_cartesian(xlim = c(FIG_K_LO, FIG_K_HI)) +
    labs(x = "Years from first birth", y = "Women \u2212 men",
         title = "The financing contrast around first birth",
         subtitle = .wrap(sprintf(paste(
           "Women minus men, employer-financed less own-time training at the",
           "workplace, on employed person-waves that report both. Post-birth",
           "average %+.3f (SE %.3f); the joint test that the leads from k = %s",
           "to %s are zero gives p = %.3f. The contrast is significant %s."),
           .fc_a$v, .fc_s$v, .kf(min(FC_LEADS)), .kf(max(FC_LEADS)), .fc_p$v,
           .spanf(.fc_run, FIG_K_HI))),
         caption = .wrap(
           "Sun-Abraham event-study estimates by sex, each against",
           "never-treated controls of the same sex, HILDA Waves 12-24, from",
           "06_revision_inference.R. Each point is the change from k = -1, the",
           "omitted reference period. The gap is formed within each",
           "cohort-by-event cell and averaged on common cohort weights.",
           "Bands are 95 percent confidence intervals; the standard errors",
           "add the two sexes' variances.")) +
    theme_x
  p <- .paper_plain(p, FC_FIG)   # the words go to the log, as for the T1 pairs

  ggsave(file.path(FIGDIR, paste0(FC_FIG, ".pdf")), p,
         width = 16/2.54, height = 10/2.54,
         device = grDevices::cairo_pdf)   # embeds its fonts; see svp()
  ggsave(file.path(FIGDIR, paste0(FC_FIG, ".png")), p,
         width = 16/2.54, height = 10/2.54, dpi = 200)
  cat("  wrote ", FIGDIR, "/", FC_FIG, ".pdf and .png\n", sep = "")

}, error = function(e) {
  .fc_withheld <<- paste0("the block stopped: ", conditionMessage(e))
  cat("  *** ", FC_FIG, " NOT DRAWN: ", .fc_withheld, "\n", sep = "")
})
rm(list = intersect(c(".fc", ".fc_raw", ".fc_pf", ".fc_sf", ".fc_sum", ".fc_val",
                      ".fc_gate", ".fc_miss", ".fc_rat", ".fc_over", ".fc_run",
                      ".fc_ns", ".fc_ld", ".fc_a", ".fc_s", ".fc_p", ".fc_x"),
                    ls(all.names = TRUE)))

# =============================================================================
# WHAT LANDED
# =============================================================================
cat("\n", strrep("=", 70), "\n", sep = "")
# THE TWO T1 FIGURES BYPASS svp(). They take no explore_ prefix and are
# always written to FIGDIR in both formats, so checking them against PUB's
# prefix and extension reported both as missing on every non-PUB run: the
# one check meant to catch a missing figure raised a false alarm on the two
# that matter most. Resolved by extension rather than assumed.
# THE CHECK REPORTED ON THE DISK, NOT ON THE RUN, which is how a build
# that skipped three figures reported all of them "written". list.files()
# matches a file from any previous run, so on 6 September at 19:25 the
# four T1 figures rebuilt and P2, P3 and P4 stayed at their 4 September
# 20:11 versions, and the manifest said nothing. A driver that confirms
# what is on disk confirms nothing about what just happened.
#
# .T0 IS TAKEN BEFORE ANY FIGURE IS DRAWN, so a file older than it was
# written by an earlier run whatever its name. One second of slack, for a
# filesystem whose timestamps are coarser than R's.
.T0 <- get0(".FIG_RUN_START", ifnotfound = Sys.time()) - 1

# P2 WAS NOT IN THIS LIST AT ALL, so nothing reported on it either way.
# It is the figure most likely to skip, because its block is gated on a
# panel column while the T1 pair reads csv files that are always there.
.svp_made <- c("P2_training_experience_profile", "P3_margin_split",
               "P4_wage_sample_selection")
# THE alt_ PAIR IS REQUIRED, not optional: the .tex draws it in both
# event-study figures. .alt is kept as an empty vector so the reporting
# branch below stays in place for anything genuinely diagnostic added later.
# v28. SA_financing_contrast is required too, for Section 7.1. It is saved
# like the T1 pairs, to FIGDIR with no prefix, so it is looked for there.
.want <- c(.svp_made, "T1_ordering", "T1_which_side_moves",
           "alt_T1_ordering_pooled", "alt_T1_which_side_pooled", FC_FIG)
.alt  <- character(0)
for (.n in c(.want, .alt)) {
  .svp <- .n %in% .svp_made
  .dir <- if (.svp && !isTRUE(PUB)) EXPL_DIR else FIGDIR
  .pre <- if (.svp && !isTRUE(PUB)) EXPL_PREFIX else ""
  .f   <- list.files(.dir, pattern = paste0("^", .pre, .n, "\\."),
                     full.names = TRUE)
  .age <- if (length(.f)) file.info(.f[1])$mtime else NA
  .msg <- if (!length(.f)) {
    if (.n %in% .alt) "not written (diagnostic)" else
      "*** NOT WRITTEN, see the log above"
  } else if (isTRUE(.age >= .T0)) {
    paste("written this run,", basename(.f[1]))
  } else {
    paste0("*** STALE, from ", format(.age, "%Y-%m-%d %H:%M"),
           ". Its block did not run; the file is a previous build")
  }
  cat(sprintf("  %-32s %s\n", .n, .msg))
}
if (nzchar(.fc_withheld))
  cat("  ", FC_FIG, " was not drawn: ", .fc_withheld, ".\n", sep = "")
cat("  The alt_ pair is the pooled-place version and both figures in the\n")
cat("  manuscript draw it. Its absence IS an error.\n")
cat(strrep("=", 70), "\n", sep = "")
cat("  STALE is not a warning about tidiness. The manuscript draws\n")
cat("  whatever is in the folder, so a stale figure is a figure of the\n")
cat("  old panel sitting under a caption written for the new one.\n")
rm(.svp_made, .want, .alt, .dir, .pre, .n, .f, .svp, .T0, .age, .msg,
   .fc_withheld)

# ---------------------------------------------------------------------
# v2. THE FIGURES ARE BUILT INTO FIGDIR AND COPIED TO THE PAPER.
# Nothing here writes into the manuscript's own folder; sync_paper() does
# it, and reports what it copied. If sync_paper is not in scope this file
# was sourced without paths.R, and the figures are in FIGDIR either way.
# ---------------------------------------------------------------------
.fig_paper <- if (exists("PAPER_FIGURES", envir = globalenv()))
  get("PAPER_FIGURES", envir = globalenv()) else NA_character_
if (!is.na(.fig_paper) &&
    identical(normalizePath(FIGDIR, winslash = "/", mustWork = FALSE),
              normalizePath(.fig_paper, winslash = "/", mustWork = FALSE))) {
  cat("  written straight into the manuscript's folder; no copy needed.\n")
} else if (exists("sync_paper", mode = "function")) {
  sync_paper("figures")
} else if (!is.na(.fig_paper)) {
  # NOT A COPY. Two folders is what this project keeps paying for, so the
  # remedy is to say which one the .tex will read and let one of them go.
  cat("  *** the figures are in ", FIGDIR, "\n      and the manuscript reads ",
      .fig_paper, "\n      They were NOT copied. Set FIGDIR to the second, or",
      " unset it and let\n      the contract decide, then delete the first.\n",
      sep = "")
} else {
  cat("  no contract and no sync_paper; figures are in ", FIGDIR, ".\n", sep = "")
}
rm(.fig_paper)

# CLOSE ONLY WHAT WAS OPENED.
fg_log_close()
rm(.fg_owns, .fg_sink_before, .fg_msg_before, .fg_log)
if (exists(".fg_dir")) rm(.fg_dir)
