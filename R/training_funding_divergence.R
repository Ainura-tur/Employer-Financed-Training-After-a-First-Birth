# =====================================================================
# training_funding_divergence.R  (v2)
#
# Four blocks, one shared sample construction.
#
#   A. FUNDING DIVERGENCE. Does the employer-funded path diverge
#      significantly from the own-time path after first birth? Supplies
#      the contrast that comparing two confidence bands by eye does not.
#
#   B. FOUR-WAY TRIANGLE. The mothers-minus-fathers gap is one of four
#      contrasts the never-treated group already funds:

# ---------------------------------------------------------------------------
# WHERE THIS SCRIPT WRITES, said once. Every saveRDS and write.csv below used
# a bare filename, so the destination was getwd(): the same call put the file
# in WG when run from the project root and in WG/TG once the scripts moved
# into a subdirectory. That is how two copies of the panel and two of
# train_ledger.csv came to exist a directory apart, neither obviously current,
# and why paper_tables.R needs a four-path search to find one .rds file.
#
# project_dir is where these landed when the working directory WAS the root,
# so naming it changes nothing about the layout and makes the layout explicit.
# v-route. THE LOCAL DEFINITION WAS SHADOWING THE CONTRACT'S AND POINTING
# SOMEWHERE ELSE. 00_contract.R defines .out_path() as td_out("results", f)
# and seals DIR_RESULTS at PROJ_ROOT/output/results. This file redefined
# the same name to file.path(PROJ_ROOT, f), and because it is sourced AFTER
# the contract its version won: every saveRDS below read as routed and
# landed in the project root instead. On 9 September six .rds files sat
# beside the scripts at 11:01 while the master's own eight csv files, using
# the contract's helper, went to output/results in the same run.
#
# The contract's own note says it: use td_out() or a local NAME, do not
# reassign these. So the contract's helper is preferred when it is loaded,
# and the fallback below routes to the same place rather than to the root,
# so a standalone run puts files where a driven one does.
#
# v26. THE ASSIGNMENT IS NOW GUARDED, and routing to the right place was
# only half the fix. .out_path is sealed by 00_contract.R, so reassigning
# it here made td_check_seals() name this file on every run even though the
# two definitions pointed at the same directory. A verifier that reports a
# fault nobody needs to act on is a verifier people stop reading. Under the
# guard the contract's version survives untouched when it is loaded, and
# the block below is reached only by a standalone run, which is the case it
# was written for.
if (!exists(".out_path", mode = "function")) {
  .out_path <- if (exists("td_out", mode = "function")) {
    function(f) td_out("results", f)
  } else {
    function(f) {
      .root <- if (exists("PROJ_ROOT", envir = globalenv()))
        get("PROJ_ROOT", envir = globalenv())
      else if (exists("project_dir", envir = globalenv()))
        get("project_dir", envir = globalenv()) else getwd()
      .d <- file.path(.root, "output", "results")
      dir.create(.d, showWarnings = FALSE, recursive = TRUE)
      file.path(.d, f)
    }
  }
}

# Figures follow the same rule. A bare "figures/..." resolves against
# whatever the session's working directory happens to be, which is the
# defect one line up in a different costume.
.fig_path <- if (exists("td_out", mode = "function")) {
  function(f) td_out("figures", f)
} else {
  function(f) {
    .root <- if (exists("PROJ_ROOT", envir = globalenv()))
      get("PROJ_ROOT", envir = globalenv()) else getwd()
    .d <- file.path(.root, "figures")
    dir.create(.d, showWarnings = FALSE, recursive = TRUE)
    file.path(.d, f)
  }
}

#        (i)   mothers vs childless women       = the women-only SA ATT
#        (ii)  fathers vs childless men         = the men-only SA ATT
#        (iii) mothers vs fathers               = the women-minus-men gap
#        (iv)  childless women vs childless men = the placebo, Block D
#      (i) and (ii) need no new estimation, only reporting: the
#      sex-specific Sun-Abraham ATT is already identified against that
#      sex's never-treated group. They are printed because a referee
#      wants to know whether the gap is mothers falling or fathers
#      rising, and the pooled gap cannot say.
#
#   C. PARTNERSHIP EVENT. Does women's employer-funded training fall at
#      partnership formation, BEFORE any birth? If it does, employers are
#      updating on expected fertility rather than realised fertility. If
#      it does not, the response is to the realised event. Either answer
#      is informative, which is what makes it a test.
#
#   D. PLACEBO BY RANDOMIZATION INFERENCE. Never-treated persons are
#      given a synthetic birth wave drawn from the age-at-first-birth
#      distribution of their own sex, and the pipeline is re-run. If a
#      women-minus-men gap appears among people who never have children,
#      the design manufactures gaps and Block A is not about children.
#      Repeated N_PLACEBO times, so the output is a reference
#      distribution rather than one draw.
#
# ---------------------------------------------------------------------
# WHY BLOCK A DIFFERENCES THE OUTCOME RATHER THAN STACKING THE MODELS.
# The two training measures are recorded on the SAME person-wave, so the
# four ATTs are correlated and adding their variances (which is what
# gap() in training_event_study.R does) is wrong in an unknown direction.
# Differencing at the row level, d = paid - own, makes the funding
# contrast a single-equation object: one model per sex, covariance
# handled by construction. The remaining women-minus-men difference IS
# across disjoint persons, so variance addition there is legitimate.
#
# Two side effects. The structural zero (PAPER_ATTENTION 2.1) imputes a 0
# for never-asked person-waves in BOTH components, so it cancels exactly
# in d and can only dilute toward zero, never flip a sign. And any
# person-wave reporting propensity common to both items differences out.
#
# SCALE. Employer-funded training has a higher base rate than own-time
# (about 29.8% for women pre-birth), so an absolute contrast conflates
# "falls more" with "starts higher". Both are estimated:
#   d_abs = paid - own                     (percentage points)
#   d_rel = paid/base_paid - own/base_own  (proportional to own base)
# d_rel is the behavioural question.
#
# SELF-EMPLOYMENT. Employer-funded training is undefined for the
# self-employed, and the loader's own column contract says they must be
# separated rather than counted as untrained. DROP_SELFEMP removes them
# from the employed-only arm. Leaving them in loads zeros onto whichever
# group self-employs more, and mothers' self-employment rises from 4.0%
# pre-birth to 11.1% at k >= 6.
#
# LEVELS, AND WHAT THIS CANNOT CLAIM. Women receive MORE employer-funded
# training than men at every wage bin (PAPER_ATTENTION 2.8). Nothing here
# should be written as a gender gap in employer investment; the claim is
# about the response to an event, and Block D is what licenses that.
#
# v2.1, against loader BUILD_TAG v21.22:
#   - the GATED funding pairs (_obs) are run alongside the _any pairs,
#     because the structural zero cancels in the contrast but not in the
#     component levels Block B reports;
#   - the AIM pair (promotion vs maintenance, both _z) is added, which is
#     hypothesis 4;
#   - every pooled estimate carries a minimum detectable effect, so a null
#     bounds a mechanism instead of merely failing to find one;
#   - the wave-20 questionnaire break is documented and DROP_BREAK_WAVE
#     tests the part of it the sex-specific wave effects do not absorb.
#
# Sample construction, cohort definition, endpoint binning and clustering
# mirror training_event_study.R so the numbers match the log.
#
# Writes training_funding_divergence_results.rds.
# =====================================================================

stopifnot(requireNamespace("fixest", quietly = TRUE))
suppressPackageStartupMessages({ library(dplyr); library(fixest) })

# ---- contract -------------------------------------------------------
# Namespace protection and the shared helpers (mde, gap, POWER, ALPHA)
# moved to 00_contract.R. Four identical alias blocks were harmless; a
# fifth definition that DIFFERED would not have been, and mde() proved
# it by killing a run 1,300 lines from either copy.
if (!exists("td_frame", mode = "function")) {
  if (!file.exists("00_contract.R"))
    stop("00_contract.R not found. It defines the dplyr aliases, mde(), ",
         "gap(), the handoff registry and td_frame(). Nothing below is ",
         "safe without it.", call. = FALSE)
  source("00_contract.R")
}
td_log_open("training_funding_divergence.R")


# --- Configuration ----------------------------------------------------
K            <- 8L        # bin |event time| >= K, birth design
K_PART       <- 4L        # bin |event time| >= K, partnership design
PRE_BASE     <- -3:-1     # horizons defining the pre-birth base rate
EARLY        <- 0:1       # impact horizons, decay test
LATE         <- 4:K       # persistence horizons, decay test
DROP_SELFEMP <- TRUE      # exclude self-employed from employed-only arm
PARTNER_DEF  <- "either"  # "partner_id", "married", or "either"
# Block D draws (0 to skip; 50 resolves the RI p to 0.02). MASTER_compat.R
# sources this script for its estimates, not its placebo, so it sets
# options(tfd_fast = TRUE) and Block D is skipped there: 300 model fits is
# not something a master run should pay for on every pass.
N_PLACEBO    <- if (isTRUE(getOption("tfd_fast"))) 0L else 50L
# The loader declares TRAINING_GATE_POSITION_BREAK_WAVE and
# TRAINING_AIM_POSITION_BREAK_WAVE, both 20: the gate item moves PQ E1 to
# E6 and the aim battery PQ E7 to E12, a uniform +5 shift, in the same
# wave as the pandemic and not separable from it.
#
# MOSTLY ABSORBED HERE, AND IT IS WORTH BEING EXPLICIT ABOUT WHY. Women
# and men are fitted in SEPARATE models, so each carries its own wave
# fixed effects. Any wave-20 shock common within a sex, which is what a
# questionnaire renumbering is, is absorbed in both models and differences
# out of the women-minus-men gap. What survives is only a shock that hits
# treated and never-treated differently WITHIN a sex at wave 20. Setting
# DROP_BREAK_WAVE tests for exactly that residual; it costs a wave and
# unbalances the event-time cells, so it is a robustness arm, not the
# default.
DROP_BREAK_WAVE <- FALSE   # TRUE drops wave 20 entirely
BREAK_WAVE      <- 20
set.seed(42)

# Each entry is c(first, second) and the contrast is FIRST MINUS SECOND.
# The paid/own naming is historical; for the aim pairs it reads
# promotion minus maintenance.
#
# WHY THE GATED PAIRS ARE NOW HERE. The loader's own column contract says
# of the _any family: "_any imputes a zero where never asked; _obs DROPS
# those rows", and marks the _obs twins "THESE ARE THE COLUMNS TO USE".
# The header argument above is that the structural zero cancels in the
# DIFFERENCE, which is true, and it is why the _any pairs are kept. It is
# not true of Block B, which reports each component as a LEVEL, and a
# level built on an imputed zero moves at the birth wave because the
# employed-only composition moves. Both families are run so the two can
# be compared rather than assumed equivalent.
#
# THE AIM PAIRS ARE HYPOTHESIS 4. promotion_training_z is jttrpfj and
# maintenance_training_z is jttrmps, both on the unconditional gate, so
# a person-wave with no training is a measured zero rather than a
# dropped row. The wide variants add jttrhgs and jttrhsc respectively.
# The battery is genuinely multi-response, joint cell 20.7% against
# 19.9% under independence, so a differential response is behavioural
# rather than forced by exclusive coding.
ALL_PAIRS <- list(
  pooled_place  = c(paid = "training_paid_time",     own = "training_own_time"),
  pooled_gated  = c(paid = "training_paid_time_obs", own = "training_own_time_obs"),
  pe_only       = c(paid = "jttpewt_any",            own = "jttpeot_any"),
  pe_gated      = c(paid = "jttpewt_obs",            own = "jttpeot_obs"),
  aim           = c(paid = "promotion_training_z",   own = "maintenance_training_z"),
  aim_wide      = c(paid = "promotion_training_wide_z",
                    own  = "maintenance_training_wide_z")
)
# Runtime is roughly linear in the number of pairs and Blocks B and D are
# the expensive ones, so name only what this run needs.
RUN_PAIRS <- c("pooled_gated", "pe_gated", "aim")

# SOLO OUTCOMES. total_training_hours_z cannot join a pair: jtthrs is one
# item and the loader has no funding-split hours, so hours measure TOTAL
# training, not employer-funded training. It is the INTENSIVE margin of
# the phenomenon the binaries measure extensively, which is a different
# and better-powered question, not a third funding contrast.
#
# v21.48. total_courses_z, the gated twin: NA if never asked, 0 if asked
# and did not train, the count otherwise, the rule total_training_hours_z
# follows. The ungated total_courses read a missing count as 0, so on the
# unconditional arm it kept the never-asked as zeros while hours dropped
# them, and the two solo outcomes sat on different samples. On a panel
# built before loader v21.48 the frame check below falls back to
# total_courses and says so.
SOLO_OUTCOMES <- c("total_training_hours_z", "total_courses_z")
PAIRS <- ALL_PAIRS[intersect(RUN_PAIRS, names(ALL_PAIRS))]

# THE FEE ITEM IS NOT A PAIR, AND THE GUARD BELOW IS WHY IT WILL NOT
# BECOME ONE BY ACCIDENT.
#
# jttrcst is the only item in the training module that records who met
# the FEES, so it looks like the natural third funding contrast, and it
# arrived in the panel as training_self_funded at loader v18 with a
# gated twin training_self_funded_z at v21.43. It is not a third
# contrast.
#
# EVERY PAIR ABOVE CONTRASTS TWO ITEMS MEASURED ON THE SAME POPULATION.
# jttpewt and jttpeot are both put to everyone the module reaches, so a
# person who trained none contributes a zero to both arms and the
# structural zero cancels in the difference, which is the argument in
# this file's header for keeping the _any family at all. The aim pair is
# gated to the same end. jttrcst is asked ONLY of people who trained.
# Pairing it against jttpewt would put a conditional item on one side of
# a difference and an unconditional one on the other, so the contrast
# would move whenever training incidence moves, and training incidence
# is the outcome. That is the T3b problem, and T3b is graded secondary
# in 03training_designs.R for exactly this reason.
#
# THE GATED TWIN DOES NOT RESCUE THE PAIRING. training_self_funded_z
# fixes the conditioning and leaves the arms differing in what a ZERO
# MEANS: on jttpewt a zero is "no employer-provided training", on the
# fee column it is "no training, or training with no contribution".
# Differencing those is not a funding contrast, it is two constructs
# subtracted.
#
# WHERE THE FEE ITEM IS REPORTED INSTEAD. 03training_designs.R T3c
# carries it as a LEVEL, graded descriptive, with the birth-wave
# contrast kept for its MDE alone. The loader's v21.43a diagnostic found
# no birth-wave dynamic on this margin, |t| 0.98 with the k = 0 cell
# excluded, so there is no dynamic here for this script to estimate and
# adding a pair would manufacture a design around a known null.
#
# A COMMENT WOULD NOT HOLD. The decision is a stop(), because the cost
# of a future reader adding "fee" to RUN_PAIRS and getting a plausible
# number back is a result that no one can tell is wrong by looking at
# it. This file already turns two other decisions into checks.
.fee_cols <- c("training_self_funded", "training_self_funded_z", "jttrcst")
if (length(PAIRS) && any(vapply(PAIRS, function(p) any(p %in% .fee_cols),
                               logical(1))))
  stop("A fee item (jttrcst) has been placed in PAIRS. It is asked only ",
       "of trainees and cannot sit opposite an unconditional item; see ",
       "the note above this guard. The level is reported by ",
       "03training_designs.R T3c.", call. = FALSE)
if (any(SOLO_OUTCOMES %in% .fee_cols))
  stop("A fee item is in SOLO_OUTCOMES. Blocks B and D report solo ",
       "outcomes as LEVELS by event time, which is the birth-wave design ",
       "the v21.43a diagnostic ruled out on this margin. See T3c.",
       call. = FALSE)
rm(.fee_cols)

# =====================================================================
# 1. Data and birth cohorts, mirroring training_event_study.R
# =====================================================================
# ---- DATA SOURCE ----------------------------------------------------
# The original branch here, inherited from training_event_study.R, read
# `if (exists("panel")) tr_data <- panel` BEFORE looking at the RDS. A
# `panel` object left in the session by an earlier loader run therefore
# shadowed a panel you had just rebuilt, and the only symptom was a pair
# quietly missing from the results. The file wins by default now, and
# whichever source is used is reported with its build_tag and column
# count so a stale read is visible instead of silent.
# v23. THE CALLER CAN NAME THE FRAME, and under a master it must.
#
# THE FILE-FIRST DEFAULT IS RIGHT FOR STANDALONE USE and the reason above is
# a real incident: a stale in-memory `panel` shadowed a freshly rebuilt one
# and the only symptom was a missing pair. That default is unchanged.
#
# IT IS WRONG UNDER A MASTER, and quietly so. MASTER_train.R applies
#
#     filter(between(age - experience_years, 10, 35))
#
# on load, so its `panel` is 129,544 rows and 19,661 persons. This script
# reaches past that to hilda_panel_data_training.rds, which is a COLUMN
# subset of analysis_sample with NO row filter: 134,131 rows, 20,205
# persons. So a master run estimates T1 to T9 on one sample and the funding
# contrast on another, 4,587 person-waves and 544 people apart, and then
# prints -0.117 beside -0.063 in the same ledger table as though they were
# comparable.
#
# options(tfd_frame = "panel") makes the master hand over its own frame, the
# same way options(tfd_fast = TRUE) already hands over a run-length
# decision. Standalone behaviour is untouched: no option set, file first.
# ---- the frame, resolved by the contract -----------------------------
# This replaces about forty lines: the tfd_frame option, the PREFER_FILE
# branch, .try_file() and the file fallbacks. One resolver, used by every
# script, so the sample cannot differ between them without the difference
# appearing at a single point.
#
# The failure it removes: run standalone in a warm session and
# getOption("tfd_frame") is NULL, so the old block fell through to
# hilda_panel_data_training.rds - the panel AS LOADED, 134,131 rows -
# while the master was working on the ANALYSIS SAMPLE at 129,544.
.tr_names <- unique(unlist(PAIRS))
tr_data <- td_frame(need_cols = c("person_id", "wave", "female",
                                  "ever_parent", "employed", "age"),
                    context = "funding contrast")
# v24.11. THIS BANNER IS DELETED, NOT REPAIRED.
#
# It read `.src`, a variable from the forty lines the refactor above
# removed: the tfd_frame option, the PREFER_FILE branch and .try_file()
# each set it, and the td_frame() resolver that replaced them does not.
# One use, no assignment anywhere in the file, so the module died here on
# "object '.src' not found" three lines after successfully resolving its
# frame. Everything above ran; nothing below it did, including
# saveRDS(out, .out_path("training_funding_divergence_results.rds")) at the end.
#
# The failure was near-silent. source_logged() caught it, MASTER_training.R
# printed one NOTE and carried on, and the run record 800 lines later found
# a STALE `out` in the session, resolved all five of its keys to NA inside
# tryCatch, and printed five rows of "n/a (SE n/a)" with no indication that
# the module had not run.
#
# Repairing it would have restored a duplicate: td_frame() already prints
# "frame: <src> | <rows> rows, <persons> persons | build_tag <bt> | funding
# contrast" from the same resolution, including the context string passed
# above. Two provenance lines that can disagree is what this file has spent
# forty lines removing. The column count is the only thing lost, and the
# mismatch check immediately below reports on rows and persons instead,
# deliberately, for the reason its own comment gives.
#
# .bt went with it: it existed only to feed that sprintf and is read
# nowhere else, so keeping it would leave an assignment whose only purpose
# was a line that no longer exists.
# v23. THE MISMATCH CHECK COMPARES ROWS AND PERSONS, NOT COLUMNS.
#
# It used to test ncol only. Under a master that is exactly the wrong
# dimension: the column counts differ for a harmless reason (the training
# RDS is a deliberate column subset) so the note fired and said so, while
# the ROW difference that makes the estimates incomparable went unmentioned.
# A reader saw "a panel with 391 columns was not used", concluded the
# training panel had the columns it needed, and never learned the samples
# were 4,587 person-waves apart.
#
# A column difference is a NOTE. A row or person difference is a WARNING,
# because it means two sets of numbers in the same log are not on the same
# sample.
# The sample-mismatch check that stood here is gone with the source-
# selection block it policed. td_frame() resolves ONE frame for every
# script and prints its rows, persons and build_tag, so two scripts
# can no longer be on different samples for the check to detect.
stopifnot(all(c("person_id","wave","female","ever_parent","employed","age")
              %in% names(tr_data)))

# v23. THE SAMPLE CONTRACT, asserted rather than reported.
#
# The options(tfd_frame=) mechanism above still works and is still the way a
# caller hands over a frame. What it could not do was FAIL when it was
# forgotten: it printed a note and carried on, so the only defence was an
# analyst reading a row count in a log every run.
#
# This call stops the run instead. If a master has declared a fingerprint
# and this script is on a different sample, the estimates never reach a
# table. Under a standalone run no fingerprint is declared, the call prints
# one line and returns, and nothing changes.
if (exists("assert_analysis_sample"))
  assert_analysis_sample(tr_data, "training_funding_divergence")

.missing <- vapply(PAIRS, function(p) !all(p %in% names(tr_data)), logical(1))
if (any(.missing)) {
  .cols <- setdiff(unlist(PAIRS[.missing]), names(tr_data))
  cat("Pairs requested but NOT on this source (skipped): ",
      paste(names(PAIRS)[.missing], collapse = ", "), "\n",
      "  absent column(s): ", paste(.cols, collapse = ", "), "\n",
      "  The aim family arrives with loader v21.22 AND its training_vars\n",
      "  fix; a panel built before that has build_tag v21.22 but lacks the\n",
      "  columns, because training_vars is a second select. Expect 133 cols.\n",
      sep = "")
}
PAIRS <- PAIRS[!.missing]
if (!length(PAIRS)) stop("None of RUN_PAIRS is on the panel.")
cat("Pairs in this run:", paste(names(PAIRS), collapse = ", "), "\n")

# v21.48. THE GATED COURSE COUNT, OR SAY WHY NOT. Without this, an older
# panel would lose the courses rows of Block B2 to intersect() below
# without a word.
if (!"total_courses_z" %in% names(tr_data) && "total_courses" %in% names(tr_data)) {
  cat("NOTE: total_courses_z is absent (loader before v21.48), so B2 uses\n",
      "  total_courses, whose zeros include the never-asked and trainees\n",
      "  who gave no count.\n", sep = "")
  SOLO_OUTCOMES[SOLO_OUTCOMES == "total_courses_z"] <- "total_courses"
}

if (isTRUE(DROP_BREAK_WAVE)) {
  .n0 <- nrow(tr_data)
  tr_data <- tr_data[tr_data$wave != BREAK_WAVE, , drop = FALSE]
  cat(sprintf("DROP_BREAK_WAVE: wave %d removed, %s person-waves.\n",
              BREAK_WAVE, format(.n0 - nrow(tr_data), big.mark = ",")))
}

.outs <- unique(c(unlist(PAIRS), SOLO_OUTCOMES))
.outs <- intersect(.outs, names(tr_data))
tr_wave_ok <- tr_data %>% group_by(wave) %>%
  summarise(ok = any(vapply(across(all_of(.outs)),
                            function(x) any(!is.na(x) & x != 0), logical(1))),
            .groups = "drop")
tr_waves <- tr_wave_ok$wave[tr_wave_ok$ok]
cat(sprintf("Training module observed in waves %d-%d.\n",
            min(tr_waves), max(tr_waves)))
tpanel <- tr_data %>% filter(wave %in% tr_waves)

# First-birth timing on the FULL panel, so the training-wave restriction
# does not censor cohort assignment.
g_tbl <- tr_data %>% group_by(person_id) %>% arrange(wave) %>%
  summarise(ep_first = first(ever_parent), ep_last = last(ever_parent),
            trans = { d_ep <- diff(ever_parent)
            idx  <- which(!is.na(d_ep) & d_ep > 0)
            if (length(idx)) wave[idx[1] + 1] else NA_integer_ },
            .groups = "drop") %>%
  mutate(cohort = case_when(
    !is.na(trans)                ~ as.double(trans),
    ep_first == 0 & ep_last == 0 ~ 10000,
    TRUE                         ~ NA_real_))

# ---- COHORT CENSUS, PRINTED BEFORE THE TRAINING-WAVE RESTRICTION ----
# PURPOSE: to make this script's cohort assignment comparable with
# 03training_designs.R. The "Sample:" line further down counts persons in
# `cs`, which is AFTER tpanel restricts to training waves, so it cannot be
# read against 03's event-sample line. This one can: it counts g_tbl,
# which is the same object 03 calls `coh`.
#
# WHY IT IS HERE AT ALL. The two scripts assign cohorts by different
# rules. 03 and 02fte_experience.R filter ever_parent to non-missing,
# take the value at the earliest wave, drop anyone already a parent
# there, and set the birth wave to min(wave[ever_parent == 1]). This
# script uses diff(ever_parent) > 0 with first()/last() for the endpoints
# and no missing-value filter. On a panel where ever_parent has no
# missing values and is monotone the two agree exactly, and this panel
# appears to satisfy both conditions. "Appears to" is not a basis for a
# control run, so the counts are printed and compared rather than
# reasoned about: the diff() form is the same construction that silently
# dropped every parent from the C5b pre-birth panel in
# RUN_FIGURES_EDA.R, and it failed there precisely because a condition
# everyone assumed held did not.
#
# WHAT TO COMPARE. 03 prints:
#   Event sample: N person-waves, P persons (T treated)
# where P is persons with ep_at_entry == 0 and T is those with a first
# birth in panel. If the two lines below do not match P and T, the rules
# have diverged on this panel and the funding contrast in T3 is being
# estimated on a different population from the pairs here.
#
# THIS PRINTS AND NOTHING ELSE. No filter, no assignment, no change to
# any downstream object, so it is safe to add immediately before a
# control run.
.coh_n   <- nrow(g_tbl)
.coh_tr  <- sum(!is.na(g_tbl$cohort) & g_tbl$cohort < 10000)
.coh_nev <- sum(!is.na(g_tbl$cohort) & g_tbl$cohort == 10000)
.coh_na  <- sum(is.na(g_tbl$cohort))
cat(sprintf(paste0("\nCohort census (g_tbl, BEFORE the training-wave ",
                   "restriction):\n  %s persons assigned, of which %s ",
                   "treated and %s never-treated.\n  %s unassigned ",
                   "(left-censored parents, or a transition this rule ",
                   "could not\n  locate). Compare the first two against ",
                   "03training_designs.R's\n  'Event sample' line: persons ",
                   "and treated must match.\n"),
            format(.coh_tr + .coh_nev, big.mark = ","),
            format(.coh_tr, big.mark = ","),
            format(.coh_nev, big.mark = ","),
            format(.coh_na, big.mark = ",")))
if (.coh_na > 0) {
  # Split the unassigned, because the two reasons have different
  # consequences. Left-censored parents SHOULD be unassigned and 03 drops
  # them too. A person who starts childless, ends a parent, and still has
  # no transition is a rule failure, and 03 would have kept them.
  .lost <- g_tbl[is.na(g_tbl$cohort) & !is.na(g_tbl$ep_first) &
                   g_tbl$ep_first == 0 & !is.na(g_tbl$ep_last) &
                   g_tbl$ep_last == 1, ]
  cat(sprintf(paste0("  of the unassigned, %s start childless and end a ",
                     "parent.\n"), format(nrow(.lost), big.mark = ",")))
  if (nrow(.lost) > 0)
    cat("  *** THOSE ARE RULE FAILURES, NOT LEFT-CENSORING. This script's\n",
        "      diff() rule lost a transition that 03's min(wave) rule would\n",
        "      find, so the two scripts are on different populations. Do not\n",
        "      read the control runs against each other until this is 0. ***\n",
        sep = "")
  rm(.lost)
}
rm(.coh_n, .coh_tr, .coh_nev, .coh_na)

cs <- tpanel %>%
  inner_join(select(g_tbl, person_id, cohort), by = "person_id") %>%
  filter(!is.na(cohort))

clean_num <- function(x) {
  x <- unclass(x); attributes(x) <- NULL
  if (is.numeric(x)) x else suppressWarnings(as.numeric(as.character(x)))
}
.num <- c("wave","female","employed","age", .outs)
for (v in c("is_self_employed","married_num"))
  if (v %in% names(cs)) .num <- c(.num, v)
for (v in .num) cs[[v]] <- clean_num(cs[[v]])

cs <- cs %>%
  mutate(id     = as.integer(factor(person_id)),
         wave   = as.double(wave),
         cohort = as.double(cohort),
         rel    = ifelse(cohort < 10000, wave - cohort, NA_real_))
cs <- as.data.frame(cs)

cat(sprintf("Sample: %d persons (%d never-treated, %d treated); K = %d\n",
            n_distinct(cs$id), n_distinct(cs$id[cs$cohort == 10000]),
            n_distinct(cs$id[cs$cohort <  10000]), K))


# =====================================================================
# 2. Estimation helpers
# ---------------------------------------------------------------------
# THE FIXEST TRAP, AND HOW THIS LAYER WORKS AROUND IT.
# For a sunab model, coef(m) auto-aggregates across cohorts and returns
# event-time coefficients named "wave::-3", while vcov(m) does not return
# a matrix carrying those names. Indexing one with the other's names
# fails: silently with NAs on a named vector, loudly with "subscript out
# of bounds" on a matrix. training_event_study.R never hit this because
# it only used coef() and se(), both aggregated and both reliable.
#
# So this layer never requires the aggregated covariance matrix.
#   per-horizon SE   fixest::se(m), the aggregated diagonal. Always there.
#   pooled post ATT  summary(m, agg = "att"), which the original script
#                    already uses successfully.
#   window ATT       the same aggregation on a sample restricted to the
#                    window, which is exact rather than approximate: the
#                    controls and the pre-period are untouched and only
#                    post rows outside the window are dropped.
#   joint test       Holm across horizons on the marginal p-values. This
#                    controls the family-wise error rate, so "minimum
#                    adjusted p below 0.05" is a valid joint rejection.
#   decay            late minus early with the variances summed. The two
#                    windows share the control group and the pre-period,
#                    so their covariance is positive and the summed
#                    variance is CONSERVATIVE. A rejection therefore
#                    holds a fortiori; a non-rejection is inconclusive.
#
# es_obj() still tries to obtain the exact matrix, and uses it for the
# exact Wald and exact decay when it succeeds. vcov_diag() prints what is
# actually available on the first model so the exact route can be fixed
# later without another failed run.
# =====================================================================

fit <- function(d, y, kmax = K, fam = c("gaussian", "poisson")) {
  fam <- match.arg(fam)
  est <- if (fam == "poisson") fepois else feols
  bin <- stats::setNames(list((-200):(-kmax), kmax:200),
                         c(as.character(-kmax), as.character(kmax)))
  f <- stats::as.formula(
    paste0(y, " ~ sunab(cohort, wave, bin.rel = bin) | id + wave"))
  m <- tryCatch(est(f, data = d, cluster = ~ id), error = function(e) NULL)
  if (is.null(m))
    m <- tryCatch(est(stats::as.formula(
      paste0(y, " ~ sunab(cohort, wave) | id + wave")),
      data = d, cluster = ~ id), error = function(e) NULL)
  m
}

.et <- function(nms) {
  rt <- suppressWarnings(as.numeric(sub(".*::(-?\\d+).*", "\\1", nms)))
  ifelse(grepl("::", nms), rt, NA_real_)
}

.na3 <- c(chi2 = NA_real_, df = NA_real_, p = NA_real_)
.na4 <- c(est = NA_real_, se = NA_real_, z = NA_real_, p = NA_real_)
.z <- function(est, se) {
  if (!is.finite(est) || !is.finite(se) || se <= 0) return(.na4)
  c(est = unname(est), se = unname(se), z = unname(est / se),
    p = unname(2 * stats::pnorm(-abs(est / se))))
}

# Minimum detectable effect: two-sided 5%, 80% power. A null WITHOUT this
# beside it says only that nothing was found; a null WITH it bounds how
# large an effect the design could have found, which is a result. It
# matters most for the aim pair, where promotion sits near an 8.5%
# unconditional base and is the thinnest outcome in the design.
# mde() comes from 00_contract.R and is SEALED there. The definition that
# stood here was scalar and hard-coded 5%/80%: it broke every vector call
# and, worse, silently substituted its own constants for the project POWER
# and ALPHA wherever it happened to win the source order. td_check_seals()
# at the foot of this file now fails if any local definition reappears.
.MDE_K <- stats::qnorm(1 - ALPHA / 2) + stats::qnorm(POWER)
.fmt <- function(x) sprintf("%+.4f (SE %.4f) p %.4g [MDE %.4f]",
                            x["est"], x["se"], x["p"], mde(x["se"]))

# --- one-time report of what fixest actually exposes ------------------
.diag_done <- FALSE
vcov_diag <- function(m) {
  if (.diag_done) return(invisible(NULL))
  .diag_done <<- TRUE
  b <- stats::coef(m)
  cat("\n--- fixest object diagnostic (printed once) ---\n")
  cat(sprintf("  coef(m)          n=%d  e.g. %s\n", length(b),
              paste(utils::head(names(b), 3), collapse = ", ")))
  s <- tryCatch(fixest::se(m), error = function(e) NULL)
  cat(sprintf("  se(m)            %s\n", if (is.null(s)) "UNAVAILABLE" else
    sprintf("n=%d  names identical to coef: %s", length(s),
            identical(names(s), names(b)))))
  cand <- list("vcov(m)"                = function() stats::vcov(m),
               "vcov(summary(m))"       = function() stats::vcov(summary(m)),
               "summary(m)$cov.scaled"  = function() summary(m)$cov.scaled,
               "m$cov.scaled"           = function() m$cov.scaled)
  for (nm in names(cand)) {
    V <- tryCatch(cand[[nm]](), error = function(e) NULL)
    if (is.null(V) || !is.matrix(V)) { cat(sprintf("  %-22s UNAVAILABLE\n", nm)); next }
    cn <- colnames(V)
    cat(sprintf("  %-22s %dx%d  names: %s  matches coef: %s\n", nm,
                nrow(V), ncol(V),
                if (is.null(cn)) "NONE" else paste(utils::head(cn, 2), collapse = ", "),
                if (is.null(cn)) "no" else all(names(b) %in% cn)))
  }
  cat(sprintf("  m$coefficients   n=%d  e.g. %s\n", length(m$coefficients),
              paste(utils::head(names(m$coefficients), 3), collapse = ", ")))
  cat("  (exact joint/decay tests use the matrix when one matches;",
      "otherwise Holm and a conservative variance are used)\n")
}

# --- aggregated coefficients and SEs, with V when obtainable ----------
es_obj <- function(m) {
  vcov_diag(m)
  b <- stats::coef(m)
  keep <- !is.na(.et(names(b)))
  b <- b[keep]
  if (!length(b)) return(NULL)
  
  s <- tryCatch(fixest::se(m), error = function(e) NULL)
  se <- if (!is.null(s) && !is.null(names(s)) && all(names(b) %in% names(s)))
    s[names(b)] else rep(NA_real_, length(b))
  names(se) <- names(b)
  
  take <- function(V) {
    if (is.null(V) || !is.matrix(V) || is.null(colnames(V))) return(NULL)
    if (!all(names(b) %in% colnames(V))) return(NULL)
    V[names(b), names(b), drop = FALSE]
  }
  V <- take(tryCatch(stats::vcov(m), error = function(e) NULL))
  if (is.null(V)) V <- take(tryCatch(stats::vcov(summary(m)), error = function(e) NULL))
  if (is.null(V)) V <- take(tryCatch(summary(m)$cov.scaled, error = function(e) NULL))
  if (is.null(V)) V <- take(tryCatch(m$cov.scaled, error = function(e) NULL))
  
  list(b = b, se = se, V = V,
       map = data.frame(event_time = .et(names(b)), name = names(b),
                        stringsAsFactors = FALSE))
}

# --- Sun-Abraham pooled ATT straight from fixest ----------------------
att_agg <- function(m) {
  ct <- tryCatch(summary(m, agg = "att")$coeftable, error = function(e) NULL)
  if (is.null(ct) || !nrow(ct)) return(c(est = NA_real_, se = NA_real_))
  i <- grep("ATT|att", rownames(ct))
  if (!length(i)) i <- 1L
  c(est = unname(ct[i[1], 1]), se = unname(ct[i[1], 2]))
}

# Pooled ATT over a WINDOW of horizons. Controls and the whole pre-period
# are kept; only post rows outside the window are dropped, so agg = "att"
# then averages exactly the horizons in `ks`.
window_att <- function(d, y, kmax, ks) {
  rel <- d$wave - d$cohort
  relb <- pmax(pmin(rel, kmax), -kmax)
  keep <- d$cohort >= 10000 | rel < 0 | relb %in% ks
  keep[is.na(keep)] <- FALSE
  if (sum(keep) < 100) return(c(est = NA_real_, se = NA_real_))
  m <- fit(d[keep, , drop = FALSE], y, kmax)
  if (is.null(m)) return(c(est = NA_real_, se = NA_real_))
  att_agg(m)
}

# --- contrasts --------------------------------------------------------
own_path <- function(o) {
  cm <- o$map[order(o$map$event_time), ]
  est <- o$b[cm$name]; se <- o$se[cm$name]
  data.frame(event_time = cm$event_time, att = unname(est), se = unname(se),
             z = unname(est / se),
             p = unname(2 * stats::pnorm(-abs(est / se))))
}
wm_path <- function(ow, om) {
  # v-cw. The gap path on common weights when both arms have cells.
  cg <- .tfd_cg(ow, om)
  if (!is.null(cg) && !is.null(cg$V))
    return(data.frame(event_time = cg$k, delta = cg$g, se = cg$path$se,
                      z = cg$g / cg$path$se,
                      p = 2 * stats::pnorm(-abs(cg$g / cg$path$se))))
  ks <- sort(intersect(ow$map$event_time, om$map$event_time))
  nw <- ow$map$name[match(ks, ow$map$event_time)]
  nm <- om$map$name[match(ks, om$map$event_time)]
  est <- ow$b[nw] - om$b[nm]
  se  <- sqrt(ow$se[nw]^2 + om$se[nm]^2)
  data.frame(event_time = ks, delta = unname(est), se = unname(se),
             z = unname(est / se),
             p = unname(2 * stats::pnorm(-abs(est / se))))
}

# Holm across horizons. Controls FWER, so min_adj < 0.05 is a valid joint
# rejection. For PRE-trends the logic runs the other way: Holm makes
# rejection harder, so a non-rejection there is weak evidence of no
# pre-trend and n_raw_sig is reported alongside.
holm_joint <- function(path, ks) {
  p <- path$p[path$event_time %in% ks]
  p <- p[is.finite(p)]
  if (!length(p)) return(c(min_adj = NA_real_, n = NA_real_,
                           n_adj_sig = NA_real_, n_raw_sig = NA_real_))
  adj <- stats::p.adjust(p, method = "holm")
  c(min_adj = min(adj), n = length(p),
    n_adj_sig = sum(adj < 0.05), n_raw_sig = sum(p < 0.05))
}

# Exact joint Wald, used only when the covariance matrix was obtainable.
wm_wald <- function(ow, om, ks) {
  # v-cw. On the common-weight path and its covariance when both arms have
  # cells. The fallback below needs a vcov carrying fixest's aggregated
  # names, which a sunab fit never has, so before this it always gave NA.
  cg <- .tfd_cg(ow, om)
  if (!is.null(cg) && !is.null(cg$V)) {
    i <- which(cg$k %in% ks)
    if (!length(i)) return(.na3)
    Vi <- tryCatch(solve(cg$V[i, i, drop = FALSE]), error = function(e) NULL)
    if (is.null(Vi)) return(.na3)
    chi2 <- as.numeric(t(cg$g[i]) %*% Vi %*% cg$g[i])
    return(c(chi2 = chi2, df = length(i),
             p = stats::pchisq(chi2, df = length(i), lower.tail = FALSE)))
  }
  if (is.null(ow$V) || is.null(om$V)) return(.na3)
  ks <- intersect(intersect(ow$map$event_time, om$map$event_time), ks)
  if (!length(ks)) return(.na3)
  nw <- ow$map$name[match(ks, ow$map$event_time)]
  nm <- om$map$name[match(ks, om$map$event_time)]
  g  <- ow$b[nw] - om$b[nm]
  V  <- ow$V[nw, nw, drop = FALSE] + om$V[nm, nm, drop = FALSE]
  Vi <- tryCatch(solve(V), error = function(e) NULL)
  if (is.null(Vi)) return(.na3)
  chi2 <- as.numeric(t(g) %*% Vi %*% g)
  c(chi2 = chi2, df = length(ks),
    p = stats::pchisq(chi2, df = length(ks), lower.tail = FALSE))
}

# =====================================================================
# LEE (2009) BOUNDS FOR THE CONDITIONAL-ON-TRAINING ARM
# ---------------------------------------------------------------------
# Part 2 of the two-part model is estimated among trainees, and trainee
# status is itself affected by the birth: part 1 puts P(any training) down
# about 12.6 points for mothers. So the post-birth trainee pool is not the
# same population as the pre-birth pool, and part 2 is a comparison of two
# differently-composed groups. Lee bounds are the standard response.
#
# THE LOGIC. If birth only ever pushes women OUT of training and never in,
# the post-birth trainees are a subset of the women who would have trained
# anyway. The pre-birth pool is then larger by the selection share
#     p = (s_pre - s_post) / s_pre
# and the identified set for the effect among always-trainers is obtained
# by trimming the pre-birth outcome distribution by p from each tail in
# turn. Trimming from BELOW raises the pre mean and gives the LOWER bound
# on the post-minus-pre gap; trimming from ABOVE gives the UPPER bound.
#
# MONOTONICITY IS THE ASSUMPTION AND IT IS NOT FREE. It requires that no
# woman trains BECAUSE of the birth. Retraining after a career break is a
# real counterexample, and if it happens the bounds are not valid, merely
# indicative. The direction of the violation matters: two-sided flows make
# the true set WIDER than what is reported here, so these bounds are a
# best case, not a conservative one. Stated rather than assumed away.
#
# WHY THE MEN'S ARM IS NOT TRIMMED. Selection has to move for trimming to
# be needed. Men's P(any training) is flat at the birth (Block B puts the
# fathers-versus-childless-men contrast at about zero), so the men's arm
# is differenced in untrimmed and the function reports the men's selection
# share so that premise can be checked rather than believed.
lee_bounds <- function(d0, y, sel_var = NULL) {
  # sel_var defaults to "the outcome is positive", which is the selection
  # rule part 2 actually conditions on.
  sel <- if (is.null(sel_var)) as.numeric(d0[[y]] > 0) else d0[[sel_var]]
  d0$.sel <- sel
  trt <- d0$cohort < 10000
  
  share <- function(fem, post) {
    z <- d0[trt & d0$female == fem &
              if (post) d0$rel >= 0 else d0$rel %in% PRE_BASE, ]
    if (!nrow(z)) return(NA_real_)
    mean(z$.sel, na.rm = TRUE)
  }
  sw0 <- share(1, FALSE); sw1 <- share(1, TRUE)
  sm0 <- share(0, FALSE); sm1 <- share(0, TRUE)
  
  vals <- function(fem, post) {
    z <- d0[trt & d0$female == fem & d0$.sel == 1 &
              if (post) d0$rel >= 0 else d0$rel %in% PRE_BASE, ]
    v <- z[[y]]; v <- v[is.finite(v) & v > 0]
    log(v)
  }
  wpre <- vals(1, FALSE); wpost <- vals(1, TRUE)
  mpre <- vals(0, FALSE); mpost <- vals(0, TRUE)
  if (!length(wpre) || !length(wpost) || !length(mpre) || !length(mpost))
    return(NULL)
  
  men_gap <- mean(mpost) - mean(mpre)
  # v25. THE UNFLOORED SHARE IS CARRIED TOO. max(0, .) is right for the
  # bound, but a trimming share of zero means one of two different things:
  # selection into training is flat, or it runs the way monotonicity rules
  # out. The bounds collapse to the point estimate in both cases, and only
  # p_raw distinguishes them.
  p_raw <- if (is.finite(sw0) && is.finite(sw1) && sw0 > 0)
    (sw0 - sw1) / sw0 else NA_real_
  p <- if (is.finite(p_raw)) max(0, p_raw) else NA_real_
  if (!is.finite(p)) return(NULL)
  
  if (p <= 0) {
    # No differential attrition into the conditional sample: the trimming
    # share is zero and the bounds collapse to the point estimate. That is
    # informative in itself and is reported rather than hidden.
    lo <- hi <- (mean(wpost) - mean(wpre)) - men_gap
  } else {
    q_lo <- stats::quantile(wpre, p,     names = FALSE)  # drop below
    q_hi <- stats::quantile(wpre, 1 - p, names = FALSE)  # drop above
    pre_trim_lo <- wpre[wpre >= q_lo]   # pre mean UP   -> gap DOWN -> lower
    pre_trim_hi <- wpre[wpre <= q_hi]   # pre mean DOWN -> gap UP   -> upper
    lo <- (mean(wpost) - mean(pre_trim_lo)) - men_gap
    hi <- (mean(wpost) - mean(pre_trim_hi)) - men_gap
  }
  list(lo = lo, hi = hi, p = p, p_raw = p_raw,
       s_women = c(pre = sw0, post = sw1),
       s_men   = c(pre = sm0, post = sm1),
       n = c(w_pre = length(wpre), w_post = length(wpost),
             m_pre = length(mpre), m_post = length(mpost)))
}

# Unconditional quantile gaps: the assumption-free complement.
# ---------------------------------------------------------------------
# Lee bounds buy a conditional estimand at the price of monotonicity. The
# alternative is to stop conditioning. Because the outcome is non-negative
# with a mass at zero, every quantile ABOVE that mass is a well-defined
# feature of the UNCONDITIONAL distribution and needs no selection
# assumption at all: at tau = 0.9 the question is what the 90th percentile
# of training does, over everyone, trainees and non-trainees together.
#
# WHAT THIS IS NOT. A difference of quantiles is not a quantile treatment
# effect: it describes the distribution's movement, not any individual's,
# and reading it as an effect on the same person requires rank invariance,
# which nothing here establishes. Reported as a description of where in
# the distribution the loss falls, which is the question the mixture made
# interesting in the first place.
quantile_gaps <- function(d0, y, taus = c(0.5, 0.75, 0.9, 0.95)) {
  trt <- d0$cohort < 10000
  cell <- function(fem, post) {
    z <- d0[trt & d0$female == fem &
              if (post) d0$rel >= 0 else d0$rel %in% PRE_BASE, ]
    v <- z[[y]]; v[is.finite(v)]
  }
  wp <- cell(1, TRUE); wr <- cell(1, FALSE)
  mp <- cell(0, TRUE); mr <- cell(0, FALSE)
  if (!length(wp) || !length(wr) || !length(mp) || !length(mr)) return(NULL)
  do.call(rbind, lapply(taus, function(tt) {
    dw <- stats::quantile(wp, tt, names = FALSE) -
      stats::quantile(wr, tt, names = FALSE)
    dm <- stats::quantile(mp, tt, names = FALSE) -
      stats::quantile(mr, tt, names = FALSE)
    data.frame(tau = tt, women = dw, men = dm, gap = dw - dm)
  }))
}

rd <- function(df) {
  num <- vapply(df, is.numeric, logical(1))
  df[num] <- lapply(df[num], round, 4); df
}

# v-cw. COMMON COHORT WEIGHTS. sa_common_weights.R, beside this script, forms
# each women-minus-men contrast inside the cohort-by-event-time cells and
# averages it on one set of weights, pooled cell counts over common support.
# fit_pair() keeps each fit's cells, and wm_att(), wm_path() and wm_wald()
# read them, so every women-minus-men number below is on common weights. The
# per-sex rows keep each sex's own weights. .tfd_cells() returns NULL, with a
# note, when a fit's cells cannot be read; that pair then falls back to
# separate weights and says so.
if (!exists("sa_common_gap", mode = "function")) {
  .sa_f <- c("sa_common_weights.R",
             file.path(if (exists("PROJ_ROOT")) PROJ_ROOT else ".", "sa_common_weights.R"))
  .sa_f <- .sa_f[file.exists(.sa_f)]
  if (!length(.sa_f)) stop("sa_common_weights.R is not beside this script.", call. = FALSE)
  source(.sa_f[1]); rm(.sa_f)
}
.tfd_cells <- function(m, label) tryCatch(sa_cells(m), error = function(e) {
  cat("  *** sa_cells() failed for ", label, ": ", conditionMessage(e),
      "\n      this pair uses separate weights.\n", sep = "")
  NULL })
# The common-weight gap from two arms' cells, or NULL when either is missing.
.tfd_cg_cells <- function(cw, cm) {
  if (is.null(cw) || is.null(cm)) return(NULL)
  tryCatch(sa_common_gap(cw, cm), error = function(e) {
    cat("  *** common-weight gap failed: ", conditionMessage(e),
        "\n      separate weights used here.\n", sep = "")
    NULL })
}
.tfd_cg <- function(ow, om) .tfd_cg_cells(ow$cells, om$cells)
# The window refit window_att() makes, returning the fit itself so its cells
# can be read. Refitting is needed: dropping the post-birth rows outside the
# window changes the estimates of the cells that remain, so a window read
# off the full fit's cells would be a different estimate.
.tfd_window_fit <- function(d, y, kmax, ks) {
  rel <- d$wave - d$cohort
  relb <- pmax(pmin(rel, kmax), -kmax)
  keep <- d$cohort >= 10000 | rel < 0 | relb %in% ks
  keep[is.na(keep)] <- FALSE
  if (sum(keep) < 100) return(NULL)
  fit(d[keep, , drop = FALSE], y, kmax)
}
# Fit both sexes and keep everything downstream needs, including the
# models and the data, so window ATTs can be refitted.
fit_pair <- function(d, y, kmax = K, fam = "gaussian") {
  dw <- filter(d, female == 1); dm <- filter(d, female == 0)
  mw <- fit(dw, y, kmax, fam); mm <- fit(dm, y, kmax, fam)
  if (is.null(mw) || is.null(mm)) return(NULL)
  ow <- es_obj(mw); om <- es_obj(mm)
  if (is.null(ow) || is.null(om)) return(NULL)
  # v-cw. Each fit's cohort-by-event cells, for the common weights.
  ow$cells <- .tfd_cells(mw, paste(y, "women"))
  om$cells <- .tfd_cells(mm, paste(y, "men"))
  list(w = ow, m = om, mw = mw, mm = mm, dw = dw, dm = dm,
       y = y, kmax = kmax)
}

# Women-minus-men pooled ATT. ks = NULL uses the full post-birth window
# straight from fixest; otherwise the window is refitted.
# v-cw. ON COMMON WEIGHTS when both arms have cells. A window is refitted
# exactly as before, and its common-weight average is read from the refits'
# own cells. The separate-weight estimate rides along as attr "separate" and
# must reproduce fixest's own aggregate of the same two fits; a mismatch
# stops the run. The original branch below is the fallback when cells are
# missing.
wm_att <- function(pr, ks = NULL) {
  if (is.null(ks)) {
    mw <- pr$mw; mm <- pr$mm
    cg <- .tfd_cg(pr$w, pr$m)
  } else {
    mw <- .tfd_window_fit(pr$dw, pr$y, pr$kmax, ks)
    mm <- .tfd_window_fit(pr$dm, pr$y, pr$kmax, ks)
    cg <- if (is.null(mw) || is.null(mm)) NULL else
      .tfd_cg_cells(.tfd_cells(mw, paste(pr$y, "women, window")),
                    .tfd_cells(mm, paste(pr$y, "men, window")))
  }
  if (!is.null(cg) && is.finite(cg$post[["est"]])) {
    a <- att_agg(mw); b <- att_agg(mm)
    d <- unname(cg$separate$post[["est"]] - (a["est"] - b["est"]))
    if (is.finite(d) && abs(d) > 1e-5)
      stop("wm_att(): the separate-weight gap rebuilt from the cells differs from ",
           "fixest's aggregate by ", signif(d, 3), " for ", pr$y, ".", call. = FALSE)
    r <- .z(cg$post[["est"]], cg$post[["se"]])
    attr(r, "separate") <- cg$separate$post
    attr(r, "weights")  <- "common"
    return(r)
  }
  if (is.null(ks)) { a <- att_agg(pr$mw); b <- att_agg(pr$mm) }
  else {
    a <- window_att(pr$dw, pr$y, pr$kmax, ks)
    b <- window_att(pr$dm, pr$y, pr$kmax, ks)
  }
  if (!is.finite(a["est"]) || !is.finite(b["est"])) return(.na4)
  r <- .z(a["est"] - b["est"], sqrt(a["se"]^2 + b["se"]^2))
  attr(r, "separate") <- c(est = unname(r["est"]), se = unname(r["se"]))
  attr(r, "weights")  <- "separate"
  r
}

# =====================================================================
# 3. Arm construction and differenced outcomes
# =====================================================================
make_arm <- function(d, arm) {
  if (arm != "employed_only") return(d)
  d <- filter(d, employed == 1)
  if (DROP_SELFEMP && "is_self_employed" %in% names(d)) {
    n0 <- nrow(d)
    d <- filter(d, is.na(is_self_employed) | is_self_employed != 1)
    cat(sprintf("  [self-employed dropped: %d person-waves]\n", n0 - nrow(d)))
  }
  d
}

add_diff_outcomes <- function(d, paid, own) {
  d <- d[!is.na(d[[paid]]) & !is.na(d[[own]]), , drop = FALSE]
  d$d_abs <- d[[paid]] - d[[own]]
  bases <- d %>% filter(cohort < 10000, rel %in% PRE_BASE) %>%
    group_by(female) %>%
    summarise(b_paid = mean(.data[[paid]], na.rm = TRUE),
              b_own  = mean(.data[[own]],  na.rm = TRUE), .groups = "drop")
  d <- left_join(d, bases, by = "female")
  bad <- !is.finite(d$b_paid) | !is.finite(d$b_own) |
    d$b_paid < 0.01 | d$b_own < 0.01
  if (any(bad, na.rm = TRUE)) {
    warning("A sex-specific pre-birth base is below 1%; d_rel returned NA there.")
    d$b_paid[bad] <- NA_real_; d$b_own[bad] <- NA_real_
  }
  d$d_rel <- d[[paid]] / d$b_paid - d[[own]] / d$b_own
  attr(d, "bases") <- bases
  d
}

out <- list()

# =====================================================================
# BLOCK A. Funding divergence
# =====================================================================
cat("\n", strrep("#", 74), "\n#  BLOCK A: FUNDING DIVERGENCE\n",
    strrep("#", 74), "\n", sep = "")

for (pnm in names(PAIRS)) {
  paid <- unname(PAIRS[[pnm]]["paid"]); own <- unname(PAIRS[[pnm]]["own"])
  for (arm in c("unconditional", "employed_only")) {
    cat(sprintf("\n=== %s | %s   (%s minus %s) ===\n", pnm, arm, paid, own))
    dd <- add_diff_outcomes(make_arm(cs, arm), paid, own)
    print(as.data.frame(attr(dd, "bases")), row.names = FALSE, digits = 3)
    
    for (scale in c("d_abs", "d_rel")) {
      if (all(is.na(dd[[scale]]))) { cat("[", scale, "] unavailable\n"); next }
      pr <- fit_pair(dd, scale)
      if (is.null(pr)) { cat("[", scale, "] not estimable\n"); next }
      ow <- pr$w; om <- pr$m
      
      lab   <- paste("A", pnm, arm, scale, sep = " | ")
      path  <- wm_path(ow, om)
      preH  <- holm_joint(path, min(path$event_time):-2)
      postH <- holm_joint(path, 0:K)
      preW  <- wm_wald(ow, om, min(path$event_time):-2)
      postW <- wm_wald(ow, om, 0:K)
      pool  <- wm_att(pr)
      early <- wm_att(pr, EARLY)
      late  <- wm_att(pr, LATE)
      decay <- if (!is.finite(late["est"]) || !is.finite(early["est"])) .na4 else
        .z(late["est"] - early["est"],
           sqrt(late["se"]^2 + early["se"]^2))
      
      cat("\n-- ", lab, " --\n  DELTA(k):\n", sep = "")
      print(rd(path), row.names = FALSE)
      cat(sprintf("  T1 pre-trend k<=-2   Holm min p %.4f | %d of %d raw p<.05  [must NOT reject]\n",
                  preH["min_adj"], as.integer(preH["n_raw_sig"]), as.integer(preH["n"])))
      cat(sprintf("  T1 joint post k>=0   Holm min p %.4g | %d of %d adj p<.05\n",
                  postH["min_adj"], as.integer(postH["n_adj_sig"]), as.integer(postH["n"])))
      if (is.finite(postW["p"]))
        cat(sprintf("     exact Wald post   chi2 %.2f/%ddf p %.4g\n",
                    postW["chi2"], as.integer(postW["df"]), postW["p"]))
      cat(sprintf("  T2 pooled post       %s\n", .fmt(pool)))
      cat(sprintf("     early k%d-%d        %s\n",
                  min(EARLY), max(EARLY), .fmt(early)))
      cat(sprintf("     late  k%d-%d        %s\n",
                  min(LATE), max(LATE), .fmt(late)))
      cat(sprintf("  T3 decay (late-early)%s  [SE conservative]\n", .fmt(decay)))
      
      out[[lab]] <- list(path = path, pre_holm = preH, post_holm = postH,
                         pre_wald = preW, post_wald = postW, pool = pool,
                         early = early, late = late, decay = decay,
                         bases = attr(dd, "bases"))
    }
  }
}

# =====================================================================
# BLOCK B. Four-way comparison triangle
# ---------------------------------------------------------------------
# (i) and (ii) are the sex-specific SA ATTs, each identified against that
# sex's never-treated group; (iii) is their difference. Printing all
# three answers what the pooled gap cannot: is the gap mothers falling,
# or fathers rising?
# =====================================================================
cat("\n", strrep("#", 74), "\n#  BLOCK B: FOUR-WAY TRIANGLE\n",
    strrep("#", 74), "\n", sep = "")

for (pnm in names(PAIRS)) {
  pair <- PAIRS[[pnm]]
  tri_y <- c(unname(pair["paid"]), unname(pair["own"]), "d_abs", "d_rel")
  for (arm in c("unconditional", "employed_only")) {
    dd <- add_diff_outcomes(make_arm(cs, arm),
                            unname(pair["paid"]), unname(pair["own"]))
    for (y in tri_y) {
      if (!y %in% names(dd) || all(is.na(dd[[y]]))) next
      pr <- fit_pair(dd, y)
      if (is.null(pr)) next
      ow <- pr$w; om <- pr$m
      
      lab <- paste("B", pnm, arm, y, sep = " | ")
      cat(sprintf("\n=== %s ===\n", lab))
      
      pw <- own_path(ow); pm <- own_path(om); pg <- wm_path(ow, om)
      tri <- data.frame(
        event_time = pg$event_time,
        i_moth_vs_cl_w  = pw$att[match(pg$event_time, pw$event_time)],
        se_i            = pw$se [match(pg$event_time, pw$event_time)],
        ii_fath_vs_cl_m = pm$att[match(pg$event_time, pm$event_time)],
        se_ii           = pm$se [match(pg$event_time, pm$event_time)],
        iii_gap         = pg$delta, se_iii = pg$se, p_iii = pg$p)
      print(rd(tri), row.names = FALSE)
      
      aw <- att_agg(pr$mw); am <- att_agg(pr$mm)
      s <- list(i    = .z(aw["est"], aw["se"]),
                ii   = .z(am["est"], am["se"]),
                iii  = wm_att(pr),
                preW = holm_joint(pw, min(pw$event_time):-2),
                preM = holm_joint(pm, min(pm$event_time):-2),
                preG = holm_joint(pg, min(pg$event_time):-2))
      cat(sprintf("  (i)   mothers vs childless women %s | pre Holm %.3f, %d raw p<.05\n",
                  .fmt(s$i),   s$preW["min_adj"], as.integer(s$preW["n_raw_sig"])))
      cat(sprintf("  (ii)  fathers vs childless men   %s | pre Holm %.3f, %d raw p<.05\n",
                  .fmt(s$ii),  s$preM["min_adj"], as.integer(s$preM["n_raw_sig"])))
      cat(sprintf("  (iii) mothers vs fathers         %s | pre Holm %.3f, %d raw p<.05\n",
                  .fmt(s$iii), s$preG["min_adj"], as.integer(s$preG["n_raw_sig"])))
      
      out[[lab]] <- list(triangle = tri, summary = s)
    }
  }
}

# =====================================================================
# BLOCK B2. The dosage margin, and the extensive/intensive split
# ---------------------------------------------------------------------
# WHY THIS IS NOT A PAIR. jtthrs is a single item and the loader carries
# no funding-split hours, so total_training_hours_z is TOTAL training
# hours. It cannot be contrasted against an own-time counterpart and must
# never be read as an employer-funded intensive margin.
#
# WHAT IT IS FOR. The binaries measure whether training happened; hours
# measure how much. Together they decompose the loss:
#     total hours = P(any training) x E[hours | any]
# If hours fall by the same PROPORTION as incidence, the whole loss is
# fewer mothers training and those who still train train as much as
# before. If hours fall by more, there is an intensive component too.
# That is a real decomposition and it is the question the incidence
# results cannot answer.
#
# AND IT IS WHERE THE POWER IS. The v21.9 recode took coverage from 34.2%
# to 99.8% of the employed sample, and hours are continuous, so this
# outcome carries far more information than a binary sitting on a 14%
# base. The proportional estimand that d_rel could not resolve is
# resolvable here.
#
# FOUR FORMS, because hours are skewed with a mass at zero:
#   level     hours, interpretable, but the right tail dominates
#   prop      the level effect divided by the sex-specific pre-birth mean.
#             The base is treated as FIXED, so the SE is the level SE on
#             the same scale and understates uncertainty slightly. One
#             division, not two, so none of the d_rel amplification.
#   asinh     handles the zeros, approximately log in the tail, keeps
#             every person
#   poisson   the coefficient IS the proportional effect, estimated
#             without dividing by anything. CAVEAT: fepois drops persons
#             whose outcome is zero in every wave, so it conditions on
#             EVER training and its estimand is not the same as the other
#             three. The dropped count is reported; read it before
#             comparing.
# =====================================================================
cat("\n", strrep("#", 74), "\n#  BLOCK B2: DOSAGE AND THE EXTENSIVE/INTENSIVE SPLIT\n",
    strrep("#", 74), "\n", sep = "")

SOLO_OUTCOMES <- intersect(SOLO_OUTCOMES, names(cs))
if (!length(SOLO_OUTCOMES)) {
  cat("No solo outcomes on this panel; Block B2 skipped.\n")
} else {
  # v25. THE DECOMPOSITION IS AN ARTEFACT NOW. Section 6.5 quotes part 2 and
  # its Lee bounds as proportional effects; this block computed them in logs
  # and converted only inside cat(), so -35.2 and -44.4 existed on no file
  # and the manuscript check could not reach them. Every row below carries
  # both units, the trimming share floored and unfloored, and the sample it
  # was fitted on.
  .dose <- NULL
  for (arm in c("unconditional", "employed_only")) {
    dd <- make_arm(cs, arm)
    
    # extensive-margin reference on the IDENTICAL sample, so the
    # comparison is like-for-like rather than across two subsets
    ext <- NULL
    if ("any_training" %in% names(dd)) {
      de <- dd[!is.na(dd$any_training), , drop = FALSE]
      pe <- fit_pair(de, "any_training")
      if (!is.null(pe)) {
        b_ext <- de %>% filter(cohort < 10000, rel %in% PRE_BASE, female == 1) %>%
          summarise(b = mean(any_training, na.rm = TRUE)) %>% pull(b)
        g <- wm_att(pe)
        ext <- list(gap = g, base = b_ext,
                    prop = unname(g["est"]) / b_ext,
                    prop_se = unname(g["se"]) / b_ext)
      }
    }
    
    for (y in SOLO_OUTCOMES) {
      d0 <- dd[is.finite(dd[[y]]), , drop = FALSE]
      if (nrow(d0) < 1000) next
      d0$.asinh <- asinh(d0[[y]])
      base_w <- d0 %>% filter(cohort < 10000, rel %in% PRE_BASE, female == 1) %>%
        summarise(b = mean(.data[[y]], na.rm = TRUE)) %>% pull(b)
      
      cat(sprintf("\n=== B2 | %s | %s ===\n", arm, y))
      cat(sprintf("  n = %s person-waves | women's pre-birth mean = %.2f\n",
                  format(nrow(d0), big.mark = ","), base_w))
      
      pr_l <- fit_pair(d0, y)
      pr_a <- fit_pair(d0, ".asinh")
      pr_p <- fit_pair(d0, y, fam = "poisson")
      
      if (!is.null(pr_l)) {
        pw <- own_path(pr_l$w); pm <- own_path(pr_l$m); pg <- wm_path(pr_l$w, pr_l$m)
        print(rd(data.frame(event_time = pg$event_time,
                            i_moth_vs_cl_w  = pw$att[match(pg$event_time, pw$event_time)],
                            se_i            = pw$se [match(pg$event_time, pw$event_time)],
                            ii_fath_vs_cl_m = pm$att[match(pg$event_time, pm$event_time)],
                            se_ii           = pm$se [match(pg$event_time, pm$event_time)],
                            iii_gap = pg$delta, se_iii = pg$se, p_iii = pg$p)), row.names = FALSE)
        
        gl <- wm_att(pr_l)
        cat(sprintf("  level    gap %s\n", .fmt(gl)))
        if (is.finite(base_w) && base_w > 0) {
          gp <- .z(gl["est"] / base_w, gl["se"] / base_w)
          cat(sprintf("  prop     gap %s  [base fixed]\n", .fmt(gp)))
        } else gp <- .na4
      } else { gl <- .na4; gp <- .na4 }
      
      ga <- if (!is.null(pr_a)) wm_att(pr_a) else .na4
      gpo <- if (!is.null(pr_p)) wm_att(pr_p) else .na4
      if (!is.null(pr_a))  cat(sprintf("  asinh    gap %s\n", .fmt(ga)))
      if (!is.null(pr_p)) {
        # fixest does not export nobs(); the object carries $nobs, and
        # stats::nobs() dispatches to it. Both guarded.
        .n_of <- function(m) {
          n <- tryCatch(stats::nobs(m), error = function(e) NULL)
          if (is.null(n) || !is.finite(n)) n <- m$nobs
          if (is.null(n) || !is.finite(n)) NA_real_ else as.numeric(n)
        }
        nlost <- nrow(d0) - sum(.n_of(pr_p$mw), .n_of(pr_p$mm), na.rm = TRUE)
        cat(sprintf("  poisson  gap %s\n", .fmt(gpo)))
        cat(sprintf("           %s person-waves dropped as all-zero: this arm\n",
                    format(max(nlost, 0), big.mark = ",")))
        cat("           conditions on EVER training and is not comparable\n")
        cat("           to the three above without saying so.\n")
      }
      
      # ---- the intensive margin, stated rather than inferred ---------
      # The level and asinh rows answer different questions and the gap
      # between them is informative, but neither is a clean intensive
      # margin: asinh loads heavily on the zero/non-zero margin and, with
      # zeros present, its coefficient has no units-free interpretation
      # (Chen and Roth 2023), so it evidences an effect without giving a
      # magnitude. The margins are therefore separated explicitly.
      #
      # CONDITIONING ON hours > 0 SELECTS ON THE OUTCOME, because whether
      # hours are positive is itself the extensive margin and it moves at
      # the birth wave. The share retained is printed; treat this row as
      # descriptive, and read the two-part block below instead when the
      # question is about intensity.
      d1 <- d0[d0[[y]] > 0, , drop = FALSE]
      gi <- .na4
      if (nrow(d1) > 1000) {
        pr_i <- fit_pair(d1, y)
        if (!is.null(pr_i)) {
          gi <- wm_att(pr_i)
          b1 <- d1 %>% filter(cohort < 10000, rel %in% PRE_BASE, female == 1) %>%
            summarise(b = mean(.data[[y]], na.rm = TRUE)) %>% pull(b)
          cat(sprintf("  naive cond. gap %s  [%s>0 only, %.1f%% of rows,\n",
                      .fmt(gi), y, 100 * nrow(d1) / nrow(d0)))
          cat(sprintf("           pre-birth mean %.1f; SELECTS ON THE OUTCOME]\n", b1))
        }
      }
      
      # ---- TWO-PART MODEL, and why the mean-based rows above cannot
      # ---- carry the intensity question -------------------------------
      # The exploratory diagnostic settled what the hours tail is. The
      # construction jttdays x jtthrs is CORRECT (days x hours-per-day is
      # total hours), and applying the definitional bounds removes almost
      # nothing: about twenty person-waves report more than 16 hours per
      # course day, clustered at exactly 20 and 24, and about five report
      # more than 260 days. Everything else between 200 and 3,000 hours is
      # WITHIN BOUNDS: 60 to 250 days at 4 to 8 hours a day.
      #
      # So the tail is not error. It is people in full-time or near
      # full-time programmes, and the distribution is a MIXTURE of short
      # courses and long programmes. That is what defeats a mean-based
      # design: the mean sits between two modes and describes neither, and
      # its variance is dominated by which mode a person is in rather than
      # by how much training they got. Winsorising would delete the second
      # mode, and since mothers and fathers are unlikely to occupy it
      # equally, the trim would BE the result.
      #
      # A two-part model takes the mixture as the object rather than
      # fighting it:
      #   part 1  P(any training)                 -- the extensive margin
      #   part 2  E[hours | hours > 0], in logs   -- intensity among trainees
      # Part 2 is estimated in LOGS because within the positives the
      # distribution is still right-skewed but has no zeros, so a log is
      # well defined and its coefficient is a proportional effect with a
      # real interpretation. That is the thing asinh could not give: with
      # zeros present, asinh has no units-free reading and its magnitude
      # moves with the scale of the units (Chen and Roth 2023).
      #
      # WHAT PART 2 IS AND IS NOT. It is the effect among trainees, and
      # trainee status is selected. It is NOT the intensity effect for a
      # fixed population. Reported because the selection is INTO the
      # sample being described, which is a defensible estimand as long as
      # it is named that way; the naive conditional row above is the same
      # object and is kept only for comparison.
      two_part <- list(part1 = .na4, part2 = .na4, share = NA_real_,
                       n_pos = NA_real_)
      if (nrow(d1) > 1000) {
        d0$.pos <- as.numeric(d0[[y]] > 0)
        pr_p1 <- fit_pair(d0, ".pos")
        if (!is.null(pr_p1)) two_part$part1 <- wm_att(pr_p1)
        
        d1$.lny <- log(d1[[y]])
        pr_p2 <- fit_pair(d1, ".lny")
        if (!is.null(pr_p2)) two_part$part2 <- wm_att(pr_p2)
        
        two_part$share <- nrow(d1) / nrow(d0)
        two_part$n_pos <- nrow(d1)
        
        cat(sprintf("\n  TWO-PART, %s\n", y))
        cat(sprintf("    part 1  P(any)          %s\n", .fmt(two_part$part1)))
        cat(sprintf("    part 2  log(y | y>0)    %s\n", .fmt(two_part$part2)))
        if (is.finite(two_part$part2["est"]))
          cat(sprintf("            part 2 as a proportional effect: %+.1f%%\n",
                      100 * (exp(two_part$part2["est"]) - 1)))
        cat(sprintf("    %s of %s rows positive (%.1f%%)\n",
                    format(two_part$n_pos, big.mark = ","),
                    format(nrow(d0), big.mark = ","), 100 * two_part$share))
        cat("    Part 1 large with part 2 near zero: the loss is entirely\n")
        cat("    fewer mothers training, and those who train train as much\n")
        cat("    as before. Both negative: an intensity component too.\n")
        cat("    Part 2 is conditional on training and its sample is\n")
        cat("    selected; the bounds below are what that costs.\n")
        
        # ---- Lee bounds on part 2 ------------------------------------
        lb <- tryCatch(lee_bounds(d0, y), error = function(e) NULL)
        if (!is.null(lb)) {
          cat(sprintf("\n    LEE BOUNDS on part 2 (log scale): [%+.4f, %+.4f]\n",
                      lb$lo, lb$hi))
          cat(sprintf("      as proportional effects:      [%+.1f%%, %+.1f%%]\n",
                      100 * (exp(lb$lo) - 1), 100 * (exp(lb$hi) - 1)))
          cat(sprintf("      trimming share p = %.3f (unfloored %+.3f) | women\n",
                      lb$p, lb$p_raw))
          cat(sprintf("      select %.3f -> %.3f\n",
                      lb$s_women["pre"], lb$s_women["post"]))
          cat(sprintf("      men select %.3f -> %.3f (untrimmed; check this is flat)\n",
                      lb$s_men["pre"], lb$s_men["post"]))
          if (lb$lo < 0 && lb$hi > 0)
            cat("      The interval SPANS ZERO: part 2 is not signed once\n",
                "      selection into training is accounted for.\n", sep = "")
          else
            cat("      The interval EXCLUDES ZERO: part 2 survives selection.\n")
          cat("      Monotonicity assumed: no woman trains BECAUSE of the\n")
          cat("      birth. Retraining after a break violates it and would\n")
          cat("      make the true set wider than this.\n")
          two_part$lee <- lb
        }
        
        # ---- unconditional quantile gaps -----------------------------
        qg <- tryCatch(quantile_gaps(d0, y), error = function(e) NULL)
        if (!is.null(qg)) {
          cat("\n    UNCONDITIONAL QUANTILE GAPS (no selection assumption):\n")
          print(rd(qg), row.names = FALSE)
          cat("      Quantiles above the zero mass describe the intensity\n")
          cat("      margin WITHOUT conditioning on training. A gap that\n")
          cat("      grows with tau puts the loss in the upper tail, which\n")
          cat("      is where the full-time programmes are.\n")
          two_part$quantiles <- qg
        }
      }
      
      # ---- the decomposition ----
      if (!is.null(ext) && is.finite(gp["est"])) {
        cat(sprintf("\n  EXTENSIVE/INTENSIVE, %s\n", y))
        cat(sprintf("    incidence  %+.1f%% (base %.3f)\n",
                    100 * ext$prop, ext$base))
        cat(sprintf("    dosage     %+.1f%% (base %.2f)\n",
                    100 * gp["est"], base_w))
        dif <- gp["est"] - ext$prop
        sed <- sqrt(gp["se"]^2 + ext$prop_se^2)
        cat(sprintf("    difference %+.1f pp (SE %.1f) p %.3f  [SE conservative:\n",
                    100 * dif, 100 * sed, unname(.z(dif, sed)["p"])))
        cat("      the two share a sample, so their covariance is positive\n")
        cat("      and the summed variance overstates it]\n")
        cat("    Near zero: the whole loss is fewer mothers training, and\n")
        cat("    those who still train train as much as before. Negative:\n")
        cat("    an intensive component on top of the extensive one.\n")
      }
      
      # ---- one tidy row per arm and outcome, in both units -----------
      .pct <- function(b) if (is.finite(b)) 100 * (exp(b) - 1) else NA_real_
      .lb  <- two_part$lee
      .g1  <- function(v, nm) if (is.null(v)) NA_real_ else unname(v[nm])
      # v-cw. The separate-weight estimate wm_att() carries, and which
      # weights the gap columns are on.
      .gs  <- function(v) { s <- attr(v, "separate")
        if (is.null(s)) NA_real_ else unname(s[["est"]]) }
      .gw  <- function(v) { w <- attr(v, "weights"); if (is.null(w)) NA_character_ else w }
      .dose <- rbind(.dose, data.frame(
        arm = arm, outcome = y,
        n = nrow(d0), n_pos = two_part$n_pos, pos_share = two_part$share,
        base_women = base_w,
        # the any_training extensive reference, on the identical sample
        ext_any_gap  = if (is.null(ext)) NA_real_ else unname(ext$gap["est"]),
        ext_any_se   = if (is.null(ext)) NA_real_ else unname(ext$gap["se"]),
        ext_any_base = if (is.null(ext)) NA_real_ else ext$base,
        # part 1: P(this outcome > 0). The extensive margin OF THIS OUTCOME
        part1_est = .g1(two_part$part1, "est"),
        part1_se  = .g1(two_part$part1, "se"),
        part1_est_separate = .gs(two_part$part1),
        # part 2: E[log y | y > 0], and the same effect as a percentage
        part2_log    = .g1(two_part$part2, "est"),
        part2_log_se = .g1(two_part$part2, "se"),
        part2_pct    = .pct(.g1(two_part$part2, "est")),
        part2_log_separate = .gs(two_part$part2),
        part2_pct_separate = .pct(.gs(two_part$part2)),
        gap_weights  = paste(unique(stats::na.omit(c(.gw(two_part$part1),
                                                     .gw(two_part$part2)))),
                             collapse = "/"),
        # Lee bounds on part 2, in logs AND as proportional effects
        lee_lo_log = if (is.null(.lb)) NA_real_ else .lb$lo,
        lee_hi_log = if (is.null(.lb)) NA_real_ else .lb$hi,
        lee_lo_pct = if (is.null(.lb)) NA_real_ else .pct(.lb$lo),
        lee_hi_pct = if (is.null(.lb)) NA_real_ else .pct(.lb$hi),
        lee_p      = if (is.null(.lb)) NA_real_ else .lb$p,
        lee_p_raw  = if (is.null(.lb)) NA_real_ else .lb$p_raw,
        sel_women_pre  = if (is.null(.lb)) NA_real_ else unname(.lb$s_women["pre"]),
        sel_women_post = if (is.null(.lb)) NA_real_ else unname(.lb$s_women["post"]),
        sel_men_pre    = if (is.null(.lb)) NA_real_ else unname(.lb$s_men["pre"]),
        sel_men_post   = if (is.null(.lb)) NA_real_ else unname(.lb$s_men["post"]),
        # the level and fixed-base proportional gaps this block also prints
        level_est = .g1(gl, "est"), level_se = .g1(gl, "se"),
        prop_est  = .g1(gp, "est"), prop_se  = .g1(gp, "se"),
        stringsAsFactors = FALSE))
      
      out[[paste("B2", arm, y, sep = " | ")]] <-
        list(level = gl, prop = gp, asinh = ga, poisson = gpo,
             intensive = gi, two_part = two_part,
             base = base_w, ext = ext, n = nrow(d0))
    }
  }
  
  # ---- the dosage decomposition, written ------------------------------
  if (!is.null(.dose) && nrow(.dose)) {
    rownames(.dose) <- NULL
    utils::write.csv(.dose, .out_path("dosage_decomposition.csv"),
                     row.names = FALSE)
    cat(sprintf("\n  Written: dosage_decomposition.csv (%d arm-outcome row(s))\n",
                nrow(.dose)))
    .dp <- .dose[, c("arm", "outcome", "part1_est", "part2_log",
                     "part2_pct", "lee_lo_pct", "lee_hi_pct",
                     "lee_p", "lee_p_raw")]
    .dp[] <- lapply(.dp, function(z) if (is.numeric(z)) round(z, 3) else z)
    print(.dp, row.names = FALSE)
    cat("  part2_pct and the two lee_*_pct columns are exp(b) - 1 in per\n")
    cat("  cent, which is the unit section 6.5 quotes. lee_p_raw below zero\n")
    cat("  means the trimming share was floored and the bounds there are the\n")
    cat("  point estimate by construction.\n")
    rm(.dp)
  } else {
    cat("\n  *** dosage_decomposition.csv NOT written: Block B2 produced no\n")
    cat("      rows. Every solo outcome fell below the 1,000-row floor or\n")
    cat("      lost its two-part fit. Section 6.5's figures have no artefact\n")
    cat("      behind them until this runs.\n")
  }
}

# =====================================================================
# BLOCK C. Partnership formation, before any birth
# ---------------------------------------------------------------------
# Person-waves are censored at first birth, so every post-partnership
# horizon is childless BY CONSTRUCTION and the coefficient cannot be a
# birth response. The cost is that late horizons are selected toward
# people who delayed childbearing: at-risk counts are printed at each k
# so the selection is visible rather than assumed away, and K_PART is
# short for the same reason.
#
# Left-censored persons (already partnered at first observation) are
# dropped, mirroring the birth design's treatment of left-censored
# parents. Never-treated = never observed partnered within the window.
# =====================================================================
cat("\n", strrep("#", 74), "\n#  BLOCK C: PARTNERSHIP FORMATION (PRE-BIRTH)\n",
    strrep("#", 74), "\n", sep = "")

has_pid <- "partner_id"  %in% names(cs)
has_mar <- "married_num" %in% names(cs)
if (!has_pid && !has_mar) {
  cat("Neither partner_id nor married_num on the panel; Block C skipped.\n")
} else {
  pfr <- cs
  pid_ok <- if (has_pid) !is.na(pfr$partner_id) &
    nzchar(as.character(pfr$partner_id)) else FALSE
  mar_ok <- if (has_mar) !is.na(pfr$married_num) & pfr$married_num == 1 else FALSE
  pfr$partnered <- switch(
    PARTNER_DEF,
    partner_id = if (has_pid) as.integer(pid_ok) else rep(NA_integer_, nrow(pfr)),
    married    = if (has_mar) as.integer(mar_ok) else rep(NA_integer_, nrow(pfr)),
    either     = as.integer(pid_ok | mar_ok))
  cat(sprintf("Partnership defined as '%s'; %.1f%% of person-waves partnered.\n",
              PARTNER_DEF, 100 * mean(pfr$partnered, na.rm = TRUE)))
  
  # Censor at first birth: keep only strictly pre-birth waves.
  pfr <- pfr %>% filter(cohort == 10000 | wave < cohort) %>%
    filter(!is.na(partnered))
  
  pc <- pfr %>% group_by(id) %>% arrange(wave) %>%
    summarise(first_state = first(partnered),
              ptrans = { dp <- diff(partnered)
              ix <- which(!is.na(dp) & dp > 0)
              if (length(ix)) as.double(wave[ix[1] + 1]) else NA_real_ },
              ever_p = any(partnered == 1, na.rm = TRUE), .groups = "drop") %>%
    mutate(pcohort = case_when(
      first_state == 1 ~ NA_real_,      # left-censored: transition undatable
      !is.na(ptrans)   ~ ptrans,
      !ever_p          ~ 10000,
      TRUE             ~ NA_real_))
  
  pfr <- pfr %>% inner_join(select(pc, id, pcohort), by = "id") %>%
    filter(!is.na(pcohort)) %>%
    mutate(prel = ifelse(pcohort < 10000, wave - pcohort, NA_real_))
  
  cat(sprintf(
    "Block C sample: %d persons (%d never-partnered, %d partnering); K = %d\n",
    n_distinct(pfr$id), n_distinct(pfr$id[pfr$pcohort == 10000]),
    n_distinct(pfr$id[pfr$pcohort <  10000]), K_PART))
  
  atrisk <- pfr %>%
    filter(pcohort < 10000, !is.na(prel), abs(prel) <= K_PART) %>%
    group_by(prel, female) %>%
    summarise(n_persons = n_distinct(id), .groups = "drop")
  cat("\nAt-risk persons by partnership event time",
      "(late cells select on delayed childbearing):\n")
  print(as.data.frame(stats::reshape(as.data.frame(atrisk), idvar = "prel",
                                     timevar = "female", direction = "wide")),
        row.names = FALSE)
  
  for (pnm in names(PAIRS)) {
    pair <- PAIRS[[pnm]]
    for (arm in c("unconditional", "employed_only")) {
      dd <- add_diff_outcomes(make_arm(pfr, arm),
                              unname(pair["paid"]), unname(pair["own"]))
      dd$cohort <- dd$pcohort   # sunab reads `cohort`
      for (y in c(unname(pair["paid"]), unname(pair["own"]), "d_rel")) {
        if (!y %in% names(dd) || all(is.na(dd[[y]]))) next
        pr <- fit_pair(dd, y, kmax = K_PART)
        if (is.null(pr)) next
        ow <- pr$w; om <- pr$m
        
        lab <- paste("C", pnm, arm, y, sep = " | ")
        cat(sprintf("\n=== %s ===\n", lab))
        pw <- own_path(ow); pm <- own_path(om); pg <- wm_path(ow, om)
        print(rd(data.frame(
          event_time = pg$event_time,
          women = pw$att[match(pg$event_time, pw$event_time)],
          se_w  = pw$se [match(pg$event_time, pw$event_time)],
          men   = pm$att[match(pg$event_time, pm$event_time)],
          se_m  = pm$se [match(pg$event_time, pm$event_time)],
          gap   = pg$delta, se_gap = pg$se, p = pg$p)), row.names = FALSE)
        
        aw2 <- att_agg(pr$mw)
        wpool <- .z(aw2["est"], aw2["se"])
        gpool <- wm_att(pr)
        wpre  <- holm_joint(pw, min(pw$event_time):-2)
        gpre  <- holm_joint(pg, min(pg$event_time):-2)
        cat(sprintf("  women pooled post-partnership %s | pre Holm %.3f, %d of %d raw p<.05\n",
                    .fmt(wpool), wpre["min_adj"],
                    as.integer(wpre["n_raw_sig"]), as.integer(wpre["n"])))
        cat(sprintf("  women-minus-men gap           %s | pre Holm %.3f, %d of %d raw p<.05\n",
                    .fmt(gpool), gpre["min_adj"],
                    as.integer(gpre["n_raw_sig"]), as.integer(gpre["n"])))
        
        out[[lab]] <- list(path = pg, women = pw, men = pm,
                           women_pooled = wpool, gap_pooled = gpool,
                           pre_women = wpre, pre_gap = gpre, atrisk = atrisk)
      }
    }
  }
}

# =====================================================================
# BLOCK D. Placebo by randomization inference
# ---------------------------------------------------------------------
# Never-treated persons are split at random; half are given a synthetic
# birth wave at the age drawn from the age-at-first-birth distribution of
# their own sex, the rest remain the control group. The women-minus-men
# pipeline is then re-run on childless people only, and the real gap is
# compared against the resulting reference distribution.
# =====================================================================
cat("\n", strrep("#", 74), "\n#  BLOCK D: PLACEBO (RANDOMIZATION INFERENCE)\n",
    strrep("#", 74), "\n", sep = "")

if (N_PLACEBO > 0L) {
  afb <- cs %>% filter(cohort < 10000, wave == cohort, !is.na(age)) %>%
    group_by(female) %>% summarise(vals = list(age), .groups = "drop")
  afb_l <- stats::setNames(afb$vals, as.character(afb$female))
  cat(sprintf("Age at first birth: women median %.1f, men median %.1f\n",
              stats::median(afb_l[["1"]]), stats::median(afb_l[["0"]])))
  
  # WHY THIS IS BUILT THE WAY IT IS. The first version split the
  # never-treated frame into one data frame per person and looped over
  # roughly 15,000 of them inside every draw, so the split alone ran once
  # per draw per outcome and dominated the model fits by an order of
  # magnitude. The assignment is a grouped minimum over one column, so it
  # is done as one vectorised pass instead, and the per-person index is
  # built ONCE per frame rather than once per draw.
  placebo_prep <- function(dd) {
    nt <- dd[dd$cohort == 10000 & !is.na(dd$age), , drop = FALSE]
    list(nt  = nt,
         idx = data.frame(id = nt$id, wave = nt$wave, age = nt$age),
         per = nt %>% group_by(id, female) %>%
           summarise(.groups = "drop") %>% as.data.frame())
  }
  
  placebo_once <- function(prep, y) {
    per <- prep$per
    per$a <- vapply(per$female, function(f) {
      pool <- afb_l[[as.character(f)]]
      if (is.null(pool) || !length(pool)) NA_real_ else sample(pool, 1)
    }, numeric(1))
    # half the never-treated stay as controls
    per$a[stats::rbinom(nrow(per), 1, 0.5) == 0] <- NA_real_
    
    tgt  <- per[!is.na(per$a), c("id", "a")]
    pick <- merge(prep$idx, tgt, by = "id")
    if (!nrow(pick)) return(NA_real_)
    pick$d <- abs(pick$age - pick$a)
    o    <- order(pick$id, pick$d)
    pick <- pick[o, ][!duplicated(pick$id[o]), ]
    # the synthetic birth must land inside the observed window
    pick <- pick[pick$d <= 1, c("id", "wave")]
    if (nrow(pick) < 50) return(NA_real_)
    names(pick)[2] <- "pl"
    
    pd <- prep$nt
    pd$cohort <- pick$pl[match(pd$id, pick$id)]
    pd$cohort[is.na(pd$cohort)] <- 10000
    pr <- fit_pair(pd, y)
    if (is.null(pr)) return(NA_real_)
    unname(wm_att(pr)["est"])
  }
  
  # SCOPE. Block D exists to show the design does not manufacture gaps
  # among the childless, which is a statement about the HEADLINE contrast,
  # not about every arm. Running it on all pairs x arms x outcomes was
  # about 2,400 model fits. Name what the placebo has to cover.
  PLACEBO_ARMS     <- "employed_only"
  PLACEBO_OUTCOMES <- "d_rel"
  .n_fit <- length(PAIRS) * length(PLACEBO_ARMS) * length(PLACEBO_OUTCOMES) *
    N_PLACEBO * 2L
  cat(sprintf("Scope: %d pair(s) x %d arm(s) x %d outcome(s) x %d draws = ~%s fits.\n",
              length(PAIRS), length(PLACEBO_ARMS), length(PLACEBO_OUTCOMES),
              N_PLACEBO, format(.n_fit, big.mark = ",")))
  
  # fixest prints a singleton note per fit; at this volume that is the
  # output looking like a hang. Silenced here and restored after.
  .notes_on <- tryCatch(getFixest_notes(), error = function(e) TRUE)
  tryCatch(setFixest_notes(FALSE), error = function(e) NULL)
  on.exit(tryCatch(setFixest_notes(.notes_on), error = function(e) NULL),
          add = TRUE)
  
  for (pnm in names(PAIRS)) {
    pair <- PAIRS[[pnm]]
    for (arm in PLACEBO_ARMS) {
      dd   <- add_diff_outcomes(make_arm(cs, arm),
                                unname(pair["paid"]), unname(pair["own"]))
      prep <- placebo_prep(dd)
      for (y in PLACEBO_OUTCOMES) {
        if (!y %in% names(dd) || all(is.na(dd[[y]]))) next
        pr <- fit_pair(dd, y)
        if (is.null(pr)) next
        real <- unname(wm_att(pr)["est"])
        lab  <- paste("D", pnm, arm, y, sep = " | ")
        cat(sprintf("  %s: ", lab)); utils::flush.console()
        
        t0 <- Sys.time()
        reps <- numeric(N_PLACEBO)
        for (i in seq_len(N_PLACEBO)) {
          reps[i] <- tryCatch(placebo_once(prep, y), error = function(e) NA_real_)
          if (i %% 10 == 0) { cat("."); utils::flush.console() }
        }
        reps <- reps[is.finite(reps)]
        el <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
        if (length(reps) < 20) {
          cat(sprintf(" too few usable draws (%ds)\n", el)); next
        }
        p_ri <- mean(abs(reps) >= abs(real))
        cat(sprintf("\n    real %+.4f | placebo mean %+.4f sd %.4f | RI p %.3f | %d draws | %ds\n",
                    real, mean(reps), stats::sd(reps), p_ri, length(reps), el))
        out[[lab]] <- list(real = real, reps = reps, p_ri = p_ri,
                           n_draw = length(reps), secs = el)
      }
    }
  }
} else cat("N_PLACEBO = 0; Block D skipped.\n")

saveRDS(out, .out_path("training_funding_divergence_results.rds"))
cat("\nSaved training_funding_divergence_results.rds\n")

# =====================================================================
# Reading the output
# =====================================================================
cat("
BLOCK A. d_rel is the test the funding hypothesis is about; d_abs is
partly a statement about base rates. The pre-trend must NOT reject. T2 is
the headline: one contrast, one SE, one p-value. T3 is the persistence
claim, and a significant negative decay coefficient is its signature. If
T2 rejects but T3 does not, the margins differ in level but not in
persistence, which is the weaker version of the claim and should be
written as such.

BLOCK B. Read (i) and (ii) before (iii). If (i) is large and (ii) is near
zero, the gap is mothers falling and the paper is about mothers. If (ii)
is positive and comparable in size, the gap is partly fathers rising and
'motherhood penalty' is the wrong name for it. Pre-trend p-values are
reported for all three because (iii) can be flat while (i) and (ii) both
trend, which the pooled gap hides.

BLOCK C. Every post-partnership horizon is childless by construction, so
a negative women-only coefficient cannot be a birth response. Read the
at-risk table first: if the late cells are thin, use only early horizons.
A pre-trend rejection is more likely here than in Block A, because
partnership formation is chosen and correlates with job change; if it
rejects, the design fails rather than the hypothesis, and the honest
report is that the anticipation question could not be answered this way.

BLOCK D. The randomization-inference p is the fraction of synthetic draws
whose gap is at least as large in absolute value as the real one. Small
means the design does not manufacture gaps among the childless and Block
A is about children. Not small means Block A is picking up a
gender-specific life-cycle trend and the mothers-minus-fathers gap is
partly mechanical. This is the cheapest way to foreclose the most obvious
referee objection.

WHAT THE DOSAGE MARGIN ACTUALLY SUPPORTS, AFTER THE BOUNDS.

Part 1 is the result. Mothers are about 12.6 points less likely to train
at all, off a base of 0.427, so roughly -29.5% proportionally. Courses and
hours agree on it (-0.126 and -0.124) because both are P(any training) on
almost the same sample; that agreement is an internal check, not two
findings. No selection assumption, clean pre-trend.

Part 2 is NOT SIGNED. The point estimates look convincing (-18.4% for
courses at p 0.007) and do not survive Lee bounds: [-33.9%, +27.4%] for
courses, [-35.4%, +37.1%] for hours, spanning zero in all four cells. The
trimming share is about 0.25 BECAUSE part 1 is large, so this is
structural rather than unlucky. A big extensive-margin effect necessarily
makes the conditional-on-selection arm fragile, and the two results are
not independent.

And that interval is the BEST case, not a conservative one: monotonicity
assumes no woman trains BECAUSE of the birth, and retraining after a
career break violates it in the direction that widens the true set.

So the -43.2% courses gap is a COMPOSITE dominated by the extensive
margin. It must not be written up as evidence of an intensity effect. The
defensible sentence is: mothers are far less likely to train at all, and
among those who continue we cannot sign the change, with Lee bounds under
monotonicity running from -34% to +27%.

The unconditional quantile gaps agree from the other direction and need no
assumption at all. The median gap is zero in every cell because more than
half of person-waves have no training, so the mass point pins it; above
that the gaps are small and unstable, and the hours gap flips sign at
tau = 0.95. What is consistent is women's quantiles falling harder than
men's in the unconditional arm, which is the extensive margin showing
through: a woman who stops training drops from the upper tail to zero.

TWO-PART, AND WHY IT REPLACES THE MEAN-BASED ROWS FOR HOURS. The hours
tail survived the definitional bounds: days x hours-per-day is the correct
construction, and capping jtthrs at 16 or jttdays at 260 removes about
twenty-five person-waves out of thousands. The mass between 200 and 3,000
hours is real, people in full-time programmes, so the distribution is a
MIXTURE and its mean describes neither mode. Read part 1 and part 2, not
the level or prop rows, when the question is about hours. Part 2 is in
logs, so its coefficient is a proportional effect with a real reading,
which is what asinh could not provide once zeros were present.

Part 2 conditions on training and its sample is selected. That is a
defensible estimand as long as it is named as the effect AMONG TRAINEES
rather than the intensity effect for a fixed population.

For the course count (total_courses_z from loader v21.48) the mean-based
rows remain the right ones: it is a small count with no second mode, and
the two-part decomposition is reported for comparability rather than
because the mean fails.

AIM PAIR. promotion_training_z minus maintenance_training_z is
hypothesis 4: if the employer reallocates away from a worker it expects
to be less attached, the advancement aim should fall while the
compliance aim does not. Read d_rel, not d_abs: maintenance runs near a
17.4% unconditional base against promotion near 8.5%, so on the absolute
scale maintenance has twice the room to fall. Promotion is the thinnest
outcome in the design, so read the MDE on every cell before concluding
anything from a null.

BLOCK B2, THE DOSAGE MARGIN. Read the extensive/intensive line last and
read it as the point of the block. Incidence and dosage are both reported
as percentages of their own pre-birth base, so the difference between them
is the intensive margin: near zero means the entire loss is fewer mothers
training, while those who still train train as much as they did before;
negative means an intensive component on top. The first is a cleaner story
than the second and is well powered here, because hours are continuous
with 99.8% coverage rather than a binary on a 14% base.

Three cautions. Hours are TOTAL training, not employer-funded, so nothing
in this block speaks to the funding split. The prop row treats the
pre-birth base as fixed and so slightly understates uncertainty. And the
poisson row silently changes the estimand: fepois drops persons who are
zero in every wave, so it conditions on ever training. Compare it to the
other three only with that stated.

GATED VERSUS IMPUTED. pooled_gated and pe_gated should reproduce the
_any pairs closely in Block A, since the imputed zero cancels in the
difference. If they do NOT, the contrast is picking up the employed-only
composition shift at the birth wave rather than a funding response, and
the gated version is the one to report.

MULTIPLE TESTING. Blocks A, C and D plus the attachment-index test are
the confirmatory family. Correct within it (Romano-Wolf, or Holm at
minimum) and label everything else exploratory. Block B is a descriptive
decomposition of a contrast already being tested, not an extra test.

POWER. With about 2,704 treated persons and a k=0 employer-funded SE near
0.023, a two-way split takes the SE to roughly 0.033 and the detectable
effect to about 0.09 against a true effect near 0.12. Two-way splits are
feasible; three-way splits sit at the edge; finer splits will not have
power and should not be run. Compute this per block rather than trusting
the approximation.
")

# ---- contract: hand off, verify, close -------------------------------
# td_put persists to handoff/, so the next script reads THIS object
# rather than whatever survived in the global environment.
# td_check_seals fails if a shared helper was redefined above: the
# last definition sourced wins and no call site says which is live.
if (exists("out")) td_put("tfd_results", out)
td_check_seals("training_funding_divergence.R")
td_log_close()
