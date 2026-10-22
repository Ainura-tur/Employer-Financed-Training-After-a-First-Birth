# =====================================================================
# 00_compat_v21.R -- GUARDS FOR THE ESTIMATION STAGE
#
# Sourced at the top of every script that reads a v21 panel:
#
#   source("00_compat_v21.R")
#   panel <- readRDS("hilda_panel_data_W12_W24_slim.rds")
#   panel <- compat_panel_check(panel)
#
# WHAT THIS FILE IS FOR. The loader can label a regime; it cannot know
# whether the script reading the panel is about to pool across one. Five
# restrictions were settled off the v20 log, and each of them is a rule
# about a SPECIFICATION rather than about a column:
#
#   firm size      one regime at a time, never across wave 22
#   education      wave 24 is a composition break, not a trend, and the
#                  two study items are on a different instrument there
#   interruption   the within-person flow, never the level
#   superannuation household aggregate, so no individual comparison
#   wealth         waves 14, 18 and 22 and nowhere else
#   experience     the same recall vintage as the interruption levels,
#                  under a name this file used not to know (v21.6)
#   training gate  the _any indicators read a never-asked person-wave as
#                  a training zero; the _obs twins do not (v21.6)
#
# A NOTE ON WHAT A GUARD CANNOT DO. Every function here is passive: it
# fires when a script calls it and is inert otherwise. The v21 scan found
# 03_training_designs.R naming compat_panel_check, require_cols and
# assert_no_olf_level in its GUARD_MUST_DEFINE vector and then calling
# only the first, so two of the three have never fired anywhere. Adding a
# name to OLF_LEVEL_VARS does nothing for a script that does not call the
# guard. compat_guard_coverage() at the foot of this file reports which
# guards a script actually invokes, so that gap is visible rather than
# assumed away.
#
# Everything here is mechanical: it checks a frame against a rule and
# stops. What is NOT mechanical, and is therefore left to the analyst,
# is the pregnancy-versus-birth decision at each of the call sites that
# still use the names v17 removed. A default there would be a silent
# answer to a substantive question, so event_col() stops until the site
# is registered in EVENT_SITE_DECISIONS below.
#
# Nothing in this file loads data, estimates anything, or writes to disk.
# =====================================================================

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
# v21.7. THIS FILE NO LONGER OPENS A LOG.
#
# It called td_log_open() here and td_log_close() at the foot, and that
# combination hung a master run for three hours. td_log_close() prints its
# completion banner and THEN unwinds the sinks, and the pre-fix unwind
# looped on sink.number(type = "message") > 0 - a connection number that
# FLOORS AT 2, because stderr is connection 2, so the loop could not
# terminate. Every log in that run ended on "00compat.R completed in 0s",
# which is the last line printed before the spin.
#
# The loop is fixed in 00_contract.R. Both calls are removed anyway,
# because the fix makes the hang impossible while the calls remain wrong:
#
#   THIS FILE IS ALWAYS SOURCED, NEVER RUN ALONE. Every script that needs
#   the guards sources it, so a log of its own splits one run's record
#   across two files and produces a six-line artefact for a file whose
#   entire output is one line. Inheriting the caller's sink is what it
#   wanted all along.
#
# Run standalone, its output goes to the console, which is correct for a
# file that prints one line.


suppressPackageStartupMessages(library(dplyr))

# ---------------------------------------------------------------------
# CONSTANTS. Same values as the loader. The panel carries them as an
# attribute, and compat_panel_check() compares the two rather than
# trusting either copy, because two constants that can disagree are
# worse than one that can be wrong.
# ---------------------------------------------------------------------
# The guards are compatible with a RANGE of panel builds, not one string.
# A build that adds a diagnostic and no columns leaves the schema alone,
# so warning on it would be the same false alarm this file exists to
# prevent. The range is set below at COMPAT_MIN_PANEL_TAG.
#
# THIS CONSTANT IS THE VERSION OF THE GUARDS, NOT OF THE PANEL, and it is
# deliberately manual. Every message that uses it asserts something about
# THIS FILE: "rebuild with the v21.6 loader", "in the compat file
# (v21.6)". Syncing it to whatever panel arrives would make the
# "predates" warning unfireable and turn "one of the two is stale" into a
# comparison of a value with itself. A guard whose reference updates
# itself is not a guard.
COMPAT_BUILD_TAG             <- "v21.6"
# v21.5 and v21.6 both REQUIRE a rebuild: the variable map changed in
# each, so an older panel is a different dataset rather than an older
# edition of the same one. Accepting an older tag silently would be the
# false reassurance this file exists to prevent.
#
# v21.5 is NOT in the set. v21.6 adds jttrwrk and seven derived columns,
# and the guards below reference four of them by name, so a v21.5 panel
# and these guards disagree about what the frame contains. That is the
# same test v21 failed and for the same reason.
#
# v21.7 and v21.8 ARE in the set, and for the opposite reason. Neither
# touches the variable map, a derived column, or the final select: both
# are reporting-only builds that leave analysis_sample byte-identical to
# v21.6 except for build_tag and a larger training_gate_facts attribute.
# A panel written by any of the three carries the same columns with the
# same values, so refusing v21.7 or v21.8 would be a false negative.
#
# The extra facts v21.8 attaches, and which this file now reads:
#   rate_obs_nonemployed    jttpewt among the non-employed WHO WERE
#                           ASKED. The measured answer to whether the
#                           structural zero is a true zero.
#   implied_understatement  never-asked non-employed times that rate.
#   structural_zero_note    the sentence, already quantified.
# v21.9 REQUIRED A REBUILD and is a different dataset from the three
# before it: total_training_hours_z is new, fy_business_income_nonzero is
# new, and earnings_total_has_business is renamed. 313 columns become 315.
# It is accepted here BECAUSE the rename is registered in COMPAT_DEPRECATED
# below, so a script still reading the old name is stopped by name rather
# than by tag.
# v21.14e: mirrored from the loader so the asserts below do not depend on
# the loader being sourced. Keep in step; a mismatch here silently moves
# where the guards think the boundary is.
TRAINING_GATE_POSITION_BREAK_WAVE <- 20L
OCC_ORIGIN_BREAK_WAVE             <- 20L

# =====================================================================
# v21.34. THE WHITELIST BECOMES A RANGE, WITH AN EXPLICIT HOLE LIST.
# =====================================================================
# COMPAT_PANEL_TAGS had to be hand-extended on every loader bump, the
# loader header said so on every build, and it was still not done. The
# evidence is in the list it replaced:
#
#   v21.16a was missed, which is what prompted the v21.18 suffix rule;
#   v21.17, v21.18, v21.20 and v21.22 were absent from the MIDDLE of the
#     list although all four existed and two are referenced by name in
#     the comments of this very file;
#   the list ended at v21.27 while the loader reached v21.33, so every
#     run emitted the changelog warning and the warning stopped carrying
#     information.
#
# A floor plus a reviewed ceiling needs no edit for an ordinary build.
#
# THE EXCLUSION VECTOR KEEPS THE ONE THING A WHITELIST COULD DO THAT A
# RANGE CANNOT: name a build in the middle that breaks the guards. It is
# empty. A name added to it must carry the reason, the way the floor
# below does, because an unexplained exclusion is indistinguishable from
# a typo.
COMPAT_MIN_PANEL_TAG <- "v21.6"    # below this the guards reference absent columns
COMPAT_REVIEWED_TO   <- "v21.43b"   # highest build the guards were READ against
COMPAT_EXCLUDED_TAGS <- character(0)
# v21.34. MOVED, and on the check rather than on the tag. The rule above
# is that this constant moves only when someone has actually read the
# loader changelog against these guards, so the reason belongs here or
# the rule is decorative.
#
# v21.34 removes a coding assumption from noncash_module_asked and adds
# that column to the loader's constant-column watch list. v21.35 changes
# a comment and one post-combination diagnostic threshold and nothing
# else. v21.36 reworks that diagnostic's population and adds a
# roster check beside it. None of the three adds or removes a column, and grep for noncash,
# compensation and the _z suffix across this file returns only
# total_training_hours_z, which is a v21.9 name and unrelated. Nothing
# here reads any affected column, so the guards are unaffected by
# construction rather than by inspection of their behaviour.
#
# WHAT THIS DOES NOT CERTIFY. v21.28 through v21.33 were never read
# against these guards; the ceiling stood at v21.33 because that was the
# loader's tag when the range replaced the whitelist, not because anyone
# checked those six builds. Moving it to v21.34 carries that gap
# forward. The warning it silences was the one thing pointing at it, so
# it is written down here instead.

# WHY THE FLOOR IS v21.6, kept because it justifies the bound.
# v21 wrote different entry_cohort labels, so a v21 panel and these
# guards disagree about the CONTENTS of a column, not merely its
# presence. v21.5 lacks jttrwrk and the seven derived columns that four
# of the guards below reference by name. Both are different datasets
# rather than older editions of the same one, and accepting either
# silently would be the false reassurance this file exists to prevent.
#
# Two builds inside the range are worth remembering rather than
# re-deriving. v21.9 renamed earnings_total_has_business; it is accepted
# because the rename is registered in COMPAT_DEPRECATED below, so a
# script still using the old name is stopped BY NAME rather than by tag,
# which is the better failure. And v21.8 attaches the training-gate
# facts this file reads: rate_obs_nonemployed, implied_understatement,
# structural_zero_note.

# Ordering, because these are NOT comparable as strings: "v21.33" sorts
# BEFORE "v21.6" alphabetically, so any sort or >= on the raw tag is
# silently wrong in a way that looks correct for the first nine minor
# versions and then inverts. Letter suffixes are reporting-only
# revisions per the v21.18 convention and order after their base.
.tag_num <- function(tag) {
  if (is.null(tag) || is.na(tag)) return(NA_real_)
  m <- regmatches(tag, regexec("^v(\\d+)\\.(\\d+)([a-z]*)$", tag))[[1]]
  if (length(m) < 4) return(NA_real_)
  suf <- m[4]
  as.numeric(m[2]) * 1e6 + as.numeric(m[3]) * 1e3 +
    if (nchar(suf)) sum(utf8ToInt(suf) - 96L) else 0
}

.tag_accepted <- function(tag) {
  if (is.null(tag) || is.na(tag) || tag %in% COMPAT_EXCLUDED_TAGS) return(FALSE)
  n <- .tag_num(tag)
  !is.na(n) && n >= .tag_num(COMPAT_MIN_PANEL_TAG)
}

# Separate from acceptance, and deliberately so. A panel NEWER than the
# reviewed ceiling is usable; nobody has checked the guards against it,
# and that is worth saying once rather than treating as an error. This is
# the constant that moves, and only when someone has actually read the
# loader changelog against these guards.
.tag_reviewed <- function(tag) {
  n <- .tag_num(tag)
  !is.na(n) && n <= .tag_num(COMPAT_REVIEWED_TO)
}

FIRM_SIZE_BREAK_WAVE         <- 22L
STUDY_ITEM_BREAK_WAVE        <- 24L
EDUC_COMPOSITION_BREAK_WAVES <- 24L
WEALTH_MODULE_WAVES          <- c(14L, 18L, 22L)

# The interruption LEVELS. Comparable within a person, not across entry
# cohorts: the base stock is a retrospective report taken at panel entry,
# and on this window the original and 2011 intakes carry 4.93 years out
# of the labour force against 1.72 for the 2019-plus intake.
OLF_LEVEL_VARS <- c("years_not_in_lf", "years_unemployed",
                    "actual_experience", "years_since_education",
                    "nonemployment_years", "history_components")

# v21.6. THE HOLE THIS CLOSES. The list above has named actual_experience
# since v21 and has never named experience_years, which the loader builds
# as
#
#   experience_years = pmin(coalesce(actual_experience,
#                                    potential_experience), age - 13)
#
# so it IS actual_experience wherever ehtjb resolves, under a name the
# guard did not know. The one on the list is the one nothing uses:
# 03_training_designs.R resolves its experience regressor with
# pick(c("experience_years", "actual_experience", "exp")) and takes the
# first, so the published DML has always run on the unguarded name.
#
# The four hybrid stocks are built downstream in 02_fte_experience.R as
# base_exp_cal + an in-panel accumulation, where base_exp_cal is
# experience_years at the person's FIRST wave. They therefore carry the
# whole of the entry-cohort vintage in a single person-constant term.
# Person fixed effects absorb it; T8's DML has none.
#
# NOT on this list, deliberately: exp_cal_obs, exp_fte_obs, exp_one_obs
# and exp_fte_int are pure in-panel accumulations with no base stock in
# them at all, so they are clean in levels and this is the property that
# makes them worth having.
# v21.14j. NAMES DERIVED FROM A RESTRICTED COLUMN INHERIT ITS RESTRICTION.
#
# The master's Eq. 4 baseline stopped on experience_years, which is a
# CONTROL there. The object of interest is educ_exp_c, and educ_exp_c is
# educ_c * exp_c where exp_c is experience_years minus its mean. So the
# term the specification is about carries the entry-cohort recall vintage
# just as directly as the control the guard caught, and centring does not
# remove a person-constant component: it removes the sample mean.
#
# The guard knew the base names and not the constructions built from them,
# so it fired on the lesser of the two. Same shape as COALESCED_PARENTS in
# the loader, where occ2_origin escaped an entrant-only verdict its own
# component had earned.
#
# Add a row when a script builds a new term on a restricted column. This
# is only as complete as the list, which is why the guard reports the
# derivation it matched rather than only the name.
DERIVED_FROM_LEVEL <- list(
  educ_exp_c = "experience_years",   # educ_c * (experience_years - mean)
  exp_c      = "experience_years",   # experience_years - mean
  exp_sq     = "experience_years",
  experience_sq = "experience_years",
  age_exp_diff  = "experience_years"
)

EXPERIENCE_LEVEL_VARS <- c("experience_years",
                           "base_exp_cal",
                           "exp_hybrid_cal", "exp_hybrid_fte",
                           "exp_hybrid_fte_obs", "exp_hybrid_fte_int",
                           "exp_hybrid_one_obs", "exp_hybrid_one_int")

# The v21.6 loader ships the same list as an attribute, measured where it
# can be and declared where it cannot. Reading it means a future loader
# change does not need this file edited, which is the pattern v21.4
# established for the entrant-only columns.
olf_level_cols <- function(data = NULL) {
  measured <- if (!is.null(data)) attr(data, "experience_level_cols") else NULL
  sort(unique(c(OLF_LEVEL_VARS, EXPERIENCE_LEVEL_VARS, measured)))
}
# The flows that replace them, and the ratio that also survives the
# cohort problem because it normalises by career length.
OLF_FLOW_VARS  <- c("d_olf_years", "d_unemp_years", "d_paid_years",
                    "d_exposure_years", "history_flow_sum", "olf_year")
OLF_RATIO_VARS <- c("olf_share", "unemp_share", "employment_share")

# Columns that are empty on every row BY CONSTRUCTION, so that
# require_cols() can say which kind of empty it found. Both are empty
# because the item cannot support the series the name promises:
# firm_size is a filtered question before wave 22, and enrolled_in_course
# is put to new entrants only. Asking for either is still an error, but
# it is not the missing-mnemonic error and should not send anyone looking
# for a mnemonic.
COMPAT_EMPTY_BY_DESIGN <- c(
  firm_size_comparable = paste("coverage breaks at wave 22 and no recode",
                               "repairs it; use firm_size within one",
                               "firm_size_regime"),
  enrolled_in_course_comparable = paste("edcqen is asked of new entrants",
                                        "only, so there is no series to",
                                        "hold; use full_time_student_yes"),
  has_business_any = paste("ASCED has no business field in the edqo*",
                           "battery, so the CATEGORY does not exist;",
                           "this is not a mnemonic to go looking for")
)

# ---------------------------------------------------------------------
# THE TRAINING GATE (v21.6)
# ---------------------------------------------------------------------
# The loader builds these four as ifelse(coalesce(x_raw, 0) == 1, 1, 0),
# so they are never NA and three states arrive as the same zero: asked
# and did not train, asked and trained elsewhere, and never asked at all.
# The third is not a behavioural zero and it is not missing at random,
# because the module is put to the employed: at the birth wave the
# employed-only composition changes and a structural zero reads as a fall
# in training.
#
# The v21.6 loader adds training_module_asked and the _obs twins, which
# are the same values wherever the module was put and NA where it was
# not. The _any columns are kept unchanged so the published training
# event study reproduces, which is exactly why they still need a guard.
#
# This also matters for what a script can check on its own. Because the
# _any columns are never NA, 03's module-coverage test,
# 100 * mean(!is.na(jttpewt_any)), is 100.0 in every wave by
# construction, and every !is.na(jttpewt_any) filter in T2 and T3 is
# inert. Nothing downstream can see the problem from the column alone.
TRAINING_GATE_VARS <- c("jttpewt_any", "jttpeot_any",
                        "training_own_time", "training_paid_time")
TRAINING_GATED_ALT <- c(jttpewt_any        = "jttpewt_obs",
                        jttpeot_any        = "jttpeot_obs",
                        training_own_time  = "training_own_time_obs",
                        training_paid_time = "training_paid_time_obs")

training_gate_cols <- function(data = NULL) {
  measured <- if (!is.null(data)) attr(data, "training_gate_cols") else NULL
  sort(unique(c(TRAINING_GATE_VARS, measured)))
}

# WAGE NAMES. The loader puts the prefix on the level and the suffix on
# the log: real_hourly_wage against ln_hourly_wage_real. Nothing signals
# which convention applies to which, so a script reaching for the level
# guesses hourly_wage_real by symmetry with the log, finds nothing, and
# falls through to exp(ln_hourly_wage_real) without saying so. 04 did
# exactly that and its own comment at line 899 records the discovery.
#
# Registered here so the next script does not repeat it. Read like
# TRAINING_GATED_ALT: the name on the left is what a caller is likely to
# ask for, the name on the right is what the loader actually carries.
#
# The exponentiation is not wrong, only silent: the log is built from a
# strictly positive level with no compression, so exp(log(x)) recovers it
# exactly. What it costs is that the fallback and the real column are
# indistinguishable in a log, and a frame missing the level for a reason
# reads the same as one that simply named it differently.
WAGE_COL_ALT <- c(hourly_wage_real = "real_hourly_wage",
                  hourly_wage      = "real_hourly_wage",
                  ln_hourly_wage   = "ln_hourly_wage_real",
                  log_hourly_wage  = "ln_hourly_wage_real")

# Resolve a wage column against a frame, saying which convention was hit.
# Returns the column NAME, or NA if the frame carries neither, so a caller
# can decide between rebuilding and stopping rather than having the
# rebuild chosen for it.
wage_col <- function(data, want = c("level", "log"), quiet = FALSE) {
  want <- match.arg(want)
  cands <- if (want == "level")
    c("real_hourly_wage", "hourly_wage_real", "hourly_wage")
  else
    c("ln_hourly_wage_real", "ln_hourly_wage", "log_hourly_wage")
  hit <- cands[cands %in% names(data)]
  if (length(hit)) {
    if (!quiet && !identical(hit[1], cands[1]))
      cat(sprintf("  wage_col(%s): '%s', not the usual '%s'.\n",
                  want, hit[1], cands[1]))
    return(hit[1])
  }
  if (!quiet)
    cat(sprintf("  wage_col(%s): none of %s is present.\n",
                want, paste(cands, collapse = ", ")))
  NA_character_
}

# Columns that carry no individual comparison whatever their name suggests.
HOUSEHOLD_ONLY_VARS <- c("hh_super_balance", "hh_net_worth",
                         "hh_net_worth_pos", "hh_net_worth_neg",
                         "hh_income_disposable", "hh_income_gross",
                         "real_hh_income_disposable")

# Wealth-valued columns, restricted to the module waves.
WEALTH_VARS <- c("hh_super_balance", "hh_net_worth",
                 "hh_net_worth_pos", "hh_net_worth_neg")

# Names removed upstream, with their replacements. Read by compat_deprecated().
COMPAT_DEPRECATED <- c(
  event_birth         = "event_pregnancy (leprg) or event_child (lebth): decide per site",
  had_birth_this_wave = "had_pregnancy_this_wave or had_child_this_wave: decide per site",
  super_balance       = "hh_super_balance (household aggregate, no individual comparison)",
  # v21.5: removed rather than repaired. It rested on ujljryr, which is
  # absent from the release, and the quantity is already in
  # years_unemployed from the ehtuj History DV.
  years_unemp_since_school = "years_unemployed (from the ehtuj History DV)",
  # v21.9. The old name was ambiguous between a fact about the MEASURE and
  # a fact about the PERSON, and those differ by a factor of twelve on
  # this panel. Decide which was meant; they are not interchangeable.
  earnings_total_has_business = paste(
    "earnings_total_includes_business (provenance: whether",
    "fy_earnings_total carries the business component at all, 1 on every",
    "person-wave in this window) OR fy_business_income_nonzero (the",
    "substantive test, about 8.3%)")
)

# v21.6 FIX. This was an unconditional assignment, so the workflow the
# header documents, "set COMPAT_SELFTEST <- TRUE before sourcing", was
# overwritten the moment the file was sourced and the self-test could
# only ever be run by editing this line. The suite has therefore not
# been run by anyone following the instructions. Guarded now, so setting
# it in the calling session works as documented.
if (!exists("COMPAT_SELFTEST")) COMPAT_SELFTEST <- FALSE

# v21.14h. GUARD ACKNOWLEDGEMENTS WERE INVISIBLE IN THE ARCHIVED LOG.
#
# Every assert here announces a KEPT declaration with message(), which
# writes to stderr. Downstream scripts sink() the OUTPUT stream only, so
# 03's 87KB v21.14g log contains the ten-restriction manifest and not one
# "compat:" line, though T1 passes allow_structural_zero = TRUE and T2-T6
# pass allow_pooled_regime = TRUE.
#
# That defeats the point of those flags. They exist so a declared decision
# is ON THE RECORD, and a declaration missing from the archived log is not
# a record; the next reader sees a design using an imputed zero and no
# statement that anyone chose to.
#
# Fixed here rather than in each caller's sink(), because there are five
# callers with five logging arrangements and one place that emits. When a
# sink is active the message ALSO goes to output, so it lands in the file;
# when none is, message() alone keeps the console clean.
.compat_say <- function(...) {
  msg <- paste0(...)
  message(msg)
  if (sink.number() > 0L) cat(msg, "\n", sep = "")
  invisible(msg)
}

# v21.14i. AND THE STOPS WERE INVISIBLE TOO, which is worse than the
# acknowledgements being invisible.
#
# stop() writes to stderr like message(), so when a caller sinks output
# only, a guard that halts the run leaves a log that simply ENDS. The
# master's v21.14h run is the evidence: it stops mid-block at the Eq. 4
# baseline and the last thing in the file is the comment above the call.
# The reader sees a script that died for no stated reason.
#
# A guard that stops without saying why is worse than no guard, because
# the obvious response is to delete the call rather than read it. So the
# reason goes to output FIRST, then the condition is raised.
# call. is declared as a FORMAL, not left to ... , because eighteen call
# sites still pass call. = FALSE from when these were plain stop() calls.
# Absorbed here it is harmless; left in ... , paste0() would append
# "FALSE" to the end of every guard message.
# THE BANNER MUST NOT OUTLIVE THE STOP. cat() is not suppressed by
# try(silent = TRUE), which suppresses only the error message, so a guard
# called inside try() printed "*** COMPAT GUARD STOPPED THE RUN ***" and the
# run carried on. 04financing_space.R did exactly that on every run until
# v27, and a banner that lies about halting costs more than no banner,
# because the reader stops believing the next one.
#
# An enclosing try() is detected from the call stack. Inside one the banner
# says the guard OBJECTED, which is true, and leaves the halting claim to the
# case where the error really propagates.
.compat_stop <- function(..., call. = FALSE) {
  msg <- paste0(...)
  # An enclosing try() or tryCatch() is visible on the call stack.
  # show.error.messages was the obvious test and is the wrong one: try()
  # sets it around the handler, not during evaluation, so it reads TRUE here
  # and the banner claimed a halt either way.
  .caught <- any(vapply(sys.calls(), function(.c)
    is.call(.c) && length(.c) && is.name(.c[[1]]) &&
      as.character(.c[[1]]) %in% c("try", "tryCatch", "withCallingHandlers"),
    logical(1)))
  if (sink.number() > 0L)
    cat("\n*** COMPAT GUARD ", if (.caught)
          "OBJECTED (caught by the caller; the run continues)" else
          "STOPPED THE RUN", " ***\n", msg, "\n\n", sep = "")
  stop(msg)
}

.compat_ctx <- function(context)
  if (is.null(context) || !nzchar(context)) "" else paste0(" [", context, "]")

# =====================================================================
# 1. PANEL CHECK
# =====================================================================
# Confirms the panel was written by the build these guards were written
# for, and that the constants agree. A missing attribute means an older
# panel: that is a warning rather than a stop, because the guards still
# work off wave numbers, but the restriction columns will be absent and
# the flow helpers will have to rebuild them.
compat_panel_check <- function(data, quiet = FALSE) {
  stopifnot(is.data.frame(data))
  tag <- attr(data, "build_tag")
  if (is.null(tag)) {
    warning("compat: this panel carries no build_tag, so it predates ",
            COMPAT_BUILD_TAG, ". The restriction columns will be missing; ",
            "olf_flows() will rebuild the increments, but firm_size_regime, ",
            "the *_comparable study items and wealth_module_wave will not ",
            "exist. Rebuild with the ", COMPAT_BUILD_TAG, " loader.",
            call. = FALSE)
  } else if (!.tag_accepted(tag)) {
    warning("compat: panel build_tag is ", tag,
            if (tag %in% COMPAT_EXCLUDED_TAGS)
              ", which is explicitly EXCLUDED: see COMPAT_EXCLUDED_TAGS."
            else
              paste0(", which is below the floor these guards need (",
                     COMPAT_MIN_PANEL_TAG, "). The columns four of them ",
                     "reference by name do not exist on that panel."),
            " Rebuild before relying on a restriction.", call. = FALSE)
  } else if (!.tag_reviewed(tag)) {
    # NOT an error. The panel is inside the supported range; it is simply
    # newer than anything anyone has read the guards against. Saying so
    # once is the whole point, and it is the message that would have
    # caught v21.28 through v21.33 arriving unreviewed.
    warning("compat: panel build_tag is ", tag, " and these guards were ",
            "last reviewed against ", COMPAT_REVIEWED_TO, ". They will run. ",
            "Read the loader changelog for anything between the two, then ",
            "move COMPAT_REVIEWED_TO.", call. = FALSE)
  }
  k <- attr(data, "restriction_constants")
  if (!is.null(k)) {
    .cmp <- function(nm, mine) {
      if (!is.null(k[[nm]]) && !isTRUE(all.equal(k[[nm]], mine)))
        .compat_stop("compat: ", nm, " is ", paste(k[[nm]], collapse = ", "),
                     " on the panel and ", paste(mine, collapse = ", "),
                     " in the compat file (", COMPAT_BUILD_TAG, "). One of the two ",
                     "is stale; do not proceed on a guess.", call. = FALSE)
    }
    .cmp("firm_size_break_wave",  FIRM_SIZE_BREAK_WAVE)
    .cmp("study_item_break_wave", STUDY_ITEM_BREAK_WAVE)
    .cmp("wealth_module_waves",   WEALTH_MODULE_WAVES)
  }
  dep <- intersect(names(data), names(COMPAT_DEPRECATED))
  if (length(dep))
    .compat_stop("compat: the panel carries removed column(s): ",
                 paste(dep, collapse = ", "), ". A rename has been undone upstream.",
                 call. = FALSE)
  if (!quiet) {
    cat(sprintf("compat %s: panel %s rows, waves %d-%d",
                COMPAT_BUILD_TAG, format(nrow(data), big.mark = ","),
                min(data$wave, na.rm = TRUE), max(data$wave, na.rm = TRUE)))
    cat(if (is.null(tag)) ", build_tag ABSENT\n" else
      sprintf(", build_tag %s\n", tag))
  }
  invisible(data)
}

# =====================================================================
# 2. COLUMN GUARD, INCLUDING THE EMPTY-COLUMN CASE
# =====================================================================
# any_of() tolerates an absent name and hands back a silent all-NA
# column, and coalesce(NA == 1, FALSE) is FALSE, which is how a variable
# absent from the release came to read as an incidence of exactly zero
# across 134,131 person-waves. An all-NA column is a MISSING VARIABLE,
# not a zero, and a constant column is usually the same thing.
require_cols <- function(data, cols, context = NULL,
                         allow_empty = FALSE, allow_constant = TRUE) {
  absent <- setdiff(cols, names(data))
  if (length(absent))
    .compat_stop("compat: column(s) absent from the frame", .compat_ctx(context),
                 ": ", paste(absent, collapse = ", "), call. = FALSE)
  present <- intersect(cols, names(data))
  empty <- present[vapply(present,
                          function(cl) all(is.na(data[[cl]])), logical(1))]
  if (length(empty) && !allow_empty) {
    .by_design <- intersect(empty, names(COMPAT_EMPTY_BY_DESIGN))
    .unexpected <- setdiff(empty, .by_design)
    .compat_stop("compat: column(s) present but ALL-NA", .compat_ctx(context), ": ",
                 paste(empty, collapse = ", "), ". ",
                 if (length(.unexpected))
                   paste0(paste(.unexpected, collapse = ", "),
                          ": that is a missing variable, not a zero. Either the ",
                          "mnemonic is absent from this release or the frame has ",
                          "been filtered to rows where the item is not asked. ")
                 else "",
                 if (length(.by_design))
                   paste0(paste(sprintf("%s: empty by construction, %s",
                                        .by_design,
                                        COMPAT_EMPTY_BY_DESIGN[.by_design]),
                                collapse = "; "),
                          ".")
                 else "",
                 call. = FALSE)
  }
  const <- present[vapply(present, function(cl) {
    u <- unique(data[[cl]][!is.na(data[[cl]])])
    length(u) == 1L
  }, logical(1))]
  const <- setdiff(const, empty)
  if (length(const)) {
    msg <- paste0("compat: column(s) constant on this frame",
                  .compat_ctx(context), ": ", paste(const, collapse = ", "),
                  ". Check this is the sample restriction and not an ",
                  "unresolved mnemonic.")
    if (allow_constant) message(msg) else stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

# =====================================================================
# 3. FIRM SIZE: ONE REGIME AT A TIME
# =====================================================================
# The two code frames reconcile and the categories agree. Coverage does
# not: roughly 60% of the employed classified through wave 21 against
# 99% from wave 22, and a small-firm share near 5% against 26% once the
# frames are aligned. The old item is a filtered question over-sampling
# large employers, so the break is not something a recode can repair and
# firm_size_comparable is NA by construction.
#
# Call this with the frame that is about to be estimated on, AFTER every
# filter has been applied. Descriptive tabulation by wave is fine and is
# not what this stops: it stops a frame that carries classified firm size
# on both sides, because a control, split or interaction built on that
# reads the instrument change as an effect.
# vars is optional and narrows the guard to a SPECIFICATION. Without it
# the guard fires on the presence of a classified firm_size column, which
# is right when the frame is the specification but wrong when firm_size
# is merely along for the ride in a frame no spec reads it from. Supply
# vars and it only fires when firm size is actually in the equation.
assert_firm_size_regime <- function(data, context = NULL,
                                    var = "firm_size", vars = NULL) {
  if (!is.null(vars)) {
    if (inherits(vars, "formula")) vars <- all.vars(vars)
    if (!var %in% as.character(vars)) return(invisible(TRUE))
  }
  if (!var %in% names(data)) return(invisible(TRUE))
  keep <- !is.na(data[[var]])
  if (!any(keep)) return(invisible(TRUE))
  side <- if ("firm_size_regime" %in% names(data)) {
    data$firm_size_regime[keep]
  } else {
    ifelse(data$wave[keep] < FIRM_SIZE_BREAK_WAVE, "pre_break", "post_break")
  }
  sides <- sort(unique(side))
  if (length(sides) > 1L) {
    n <- table(side)
    .compat_stop("compat: ", var, " is classified on BOTH sides of the wave ",
                 FIRM_SIZE_BREAK_WAVE, " instrument change", .compat_ctx(context),
                 " (", paste(sprintf("%s n=%s", names(n), format(as.integer(n),
                                                                 big.mark = ",")),
                             collapse = ", "), "). ",
                 "Restrict to one regime with firm_size_side(), or drop firm size ",
                 "from this specification. There is no recode that makes the two ",
                 "comparable: the break is coverage, not mapping.", call. = FALSE)
  }
  invisible(TRUE)
}

# Restrict a frame to one firm-size regime. The post-break side is the
# default because it is the side with 99% coverage.
firm_size_side <- function(data, side = c("post", "pre")) {
  side <- match.arg(side)
  if (side == "post") dplyr::filter(data, wave >= FIRM_SIZE_BREAK_WAVE)
  else                dplyr::filter(data, wave <  FIRM_SIZE_BREAK_WAVE)
}

# =====================================================================
# 4. THE PREGNANCY-VERSUS-BIRTH DECISION, ONE SITE AT A TIME
# =====================================================================
# v17 removed event_birth and had_birth_this_wave because they conflated
# two different SCQ items: leprg is pregnancy, which for a first-time
# parent fires in two consecutive waves, and lebth is the birth or
# adoption of a new child. Which one a call site wanted is a substantive
# question about the design at that site, not a rename, and it differs
# between an anticipation window and a post-birth window.
#
# So there is no default here. Register each site below, then call
# event_col(). Until a site is registered the call stops and names the
# two options. The known call sites in this project, from the v20 scan:
#
#   03training_designs.R        26 references
#   Pre_analysis_diagnostics2.R  4 references
#   04variable_stability.R       2 references
#
# Format: "<file>:<label>" = "pregnancy" or "birth". The label is
# whatever identifies the site to you; keep it stable, because it is the
# record of the decision.
EVENT_SITE_DECISIONS <- c(
  # "03training_designs.R:anticipation_window"   = "pregnancy",
  # "03training_designs.R:post_birth_window"     = "birth",
  # "Pre_analysis_diagnostics2.R:birth_validation" = "birth",
  # "04variable_stability.R:event_coverage"      = "birth"
)

EVENT_RENAME <- list(
  event_birth = c(pregnancy = "event_pregnancy", birth = "event_child"),
  had_birth_this_wave = c(pregnancy = "had_pregnancy_this_wave",
                          birth     = "had_child_this_wave")
)

# Returns the NAME of the column this site should read. Pass want to
# decide inline; leave it NULL to have the decision come from the
# registry, which is the version that leaves a record.
event_col <- function(old_name = c("event_birth", "had_birth_this_wave"),
                      site, want = NULL) {
  old_name <- match.arg(old_name)
  if (missing(site) || !nzchar(site))
    .compat_stop("compat: event_col() needs a site label so the decision is on the ",
                 "record. Use \"<file>:<label>\".", call. = FALSE)
  # Named-vector lookup with [[ ]] errors on an absent name, and an
  # absent name is the ordinary case here, so it is checked first.
  if (is.null(want))
    want <- if (site %in% names(EVENT_SITE_DECISIONS))
      EVENT_SITE_DECISIONS[[site]] else NULL
  if (is.null(want) || is.na(want)) {
    opts <- EVENT_RENAME[[old_name]]
    .compat_stop("compat: ", old_name, " was removed in v17 and site \"", site,
                 "\" has no decision recorded. leprg (pregnancy) fires in the wave ",
                 "a pregnancy is reported, which for a first birth is the wave ",
                 "BEFORE the birth as well; lebth (birth) fires on the birth or ",
                 "adoption itself. Choose one for this site and add it to ",
                 "EVENT_SITE_DECISIONS:\n    \"", site, "\" = \"pregnancy\"  -> ",
                 opts[["pregnancy"]], "\n    \"", site, "\" = \"birth\"      -> ",
                 opts[["birth"]], call. = FALSE)
  }
  want <- match.arg(as.character(want), c("pregnancy", "birth"))
  EVENT_RENAME[[old_name]][[want]]
}

# Attach the chosen column under the old name, so a site can be migrated
# without rewriting every line of it at once. The alias is temporary by
# intent: the decision is recorded, but the code still reads a name that
# does not say which item it holds.
event_alias <- function(data, old_name, site, want = NULL) {
  new <- event_col(old_name, site = site, want = want)
  require_cols(data, new, context = site)
  data[[old_name]] <- data[[new]]
  .compat_say("compat: ", site, " reads ", new, " as ", old_name,
              ". Replace the alias when convenient.")
  data
}

# =====================================================================
# 5. WAVE 24 EDUCATION: A BREAK, NOT A TREND
# =====================================================================
# Wave 24 combines a sample refresher with an instrument change on edcqen
# and edfts, and observed coverage of those two items goes from 3.6% to
# 12.3%. The wave-24 cross-section is usable on its own terms; a series
# running through it is not.
# The _yes indicators are covered too. They are built from the same items
# across every wave, including 24, so a guard that watched only the raw
# columns would wave through the very columns the loader now recommends.
STUDY_RAW_VARS <- c("enrolled_in_course", "full_time_student",
                    "enrolled_in_course_yes", "full_time_student_yes")

# allow_span is the same escape hatch the interruption and module guards
# carry, and for the same reason. Wave 24 is a composition break, so a
# LEVEL compared across it compares two samples; a design with person and
# wave fixed effects absorbs a common shift, and the caller who knows that
# should say so rather than drop the wave. The declaration prints.
assert_study_comparable <- function(data, context = NULL,
                                    vars = STUDY_RAW_VARS,
                                    allow_span = FALSE) {
  vars <- intersect(vars, names(data))
  if (!length(vars)) return(invisible(TRUE))
  spans <- any(data$wave >= STUDY_ITEM_BREAK_WAVE, na.rm = TRUE) &&
    any(data$wave <  STUDY_ITEM_BREAK_WAVE, na.rm = TRUE)
  if (!spans) return(invisible(TRUE))
  obs <- vapply(vars, function(cl)
    any(!is.na(data[[cl]][data$wave >= STUDY_ITEM_BREAK_WAVE])), logical(1))
  if (any(obs) && allow_span) {
    .compat_say("compat: ", paste(vars[obs], collapse = ", "), " kept across wave ",
                STUDY_ITEM_BREAK_WAVE, .compat_ctx(context),
                " on the caller's declaration that the design absorbs the ",
                "refresher. Say so in the notes.")
    return(invisible(TRUE))
  }
  if (any(obs))
    .compat_stop("compat: ", paste(vars[obs], collapse = ", "), " observed at wave ",
                 STUDY_ITEM_BREAK_WAVE, " in a frame that also covers earlier waves",
                 .compat_ctx(context), ". Wave ", STUDY_ITEM_BREAK_WAVE,
                 " carries a sample REFRESHER: mean education years, degree share ",
                 "and n all move by several times their usual increment, so a ",
                 "level compared across that wave is comparing two samples. Use ",
                 "the *_comparable columns, or drop_study_break(), or add a wave-",
                 STUDY_ITEM_BREAK_WAVE, " indicator and say so in the notes. ",
                 "NOTE the reason changed in v21.3: this was written as an ",
                 "instrument break, and it is not one. Value labels are stable ",
                 "across the window on both items, edfts is observed for everyone ",
                 "in every wave, and the coverage jump on edcqen is routing, which ",
                 "assert_not_entrant_only() handles. What remains here is the ",
                 "refresher, which is a composition change and nothing else.",
                 call. = FALSE)
  invisible(TRUE)
}

drop_study_break <- function(data)
  dplyr::filter(data, !wave %in% EDUC_COMPOSITION_BREAK_WAVES)

# edcqen is put to respondents on the new-person questionnaire. The v21.3
# loader run measured it at 0.0% of 113,926 continuing person-waves
# against 52.2% of 9,355 entrant ones, so enrolled_in_course is an
# entrant variable in every wave rather than a sample enrolment rate with
# one bad wave. Entrants are younger, so a rate computed on it is not the
# panel's rate and a control built on it is missing for 96% of rows.
#
# This stops rather than warns, because the column looks exactly like a
# usable indicator and nothing in the data announces the routing. Pass
# entrant_frame = TRUE when the frame really is entrants and the
# statement is meant to be about them.
# v21.4 adds the qualification batteries. ednrs*, edtch* and edqo* are
# all NPQ, so the four field flags are missing for everyone who was not
# an entrant, and their counts read as panel counts: 76 nursing, 46
# teaching, 57 technical, 422 trade on a 134,131-row panel. Unlike the
# enrolment item these cannot be repaired by adding the continuing-person
# battery, because edqro* carries ASCED levels and no nursing, teaching
# or trade category. years_npq_total is on the list for the same reason,
# though the loader already guards the one place it is differenced.
#
# This vector is the FALLBACK. A v21.4 panel carries the measured
# verdict in its entrant_only_cols attribute, and entrant_only_cols()
# prefers that, so a release where the routing changes does not need
# this file edited.
ENTRANT_ONLY_VARS <- c("enrolled_in_course", "enrolled_in_course_yes",
                       "enrolled_in_course_comparable",
                       "has_nursing_any", "has_nursing_strict",
                       "has_teaching_any",
                       "has_trade_any", "has_technical_any",
                       "years_npq_total")

entrant_only_cols <- function(data = NULL) {
  measured <- if (!is.null(data)) attr(data, "entrant_only_cols") else NULL
  sort(unique(c(ENTRANT_ONLY_VARS, measured)))
}

assert_not_entrant_only <- function(vars, context = NULL,
                                    entrant_frame = FALSE, data = NULL) {
  if (inherits(vars, "formula")) vars <- all.vars(vars)
  hit <- intersect(as.character(vars), entrant_only_cols(data))
  if (!length(hit)) return(invisible(TRUE))
  if (entrant_frame) {
    .compat_say("compat: ", paste(hit, collapse = ", "), " kept on the caller's ",
                "declaration that this frame is new entrants", .compat_ctx(context),
                ".")
    return(invisible(TRUE))
  }
  .compat_stop("compat: ", paste(hit, collapse = ", "), " rest",
               if (length(hit) == 1L) "s" else "", " on new-person-questionnaire ",
               "item(s)", .compat_ctx(context), ", so ",
               if (length(hit) == 1L) "it is" else "they are",
               " missing for roughly 96% of person-waves and any count or rate ",
               "computed on ", if (length(hit) == 1L) "it" else "them",
               " is about entrants, who are younger. For enrolment use ",
               "full_time_student_yes, derived for everyone in every wave, or add ",
               "edccenr and rebuild. For the field-of-study flags there is no ",
               "continuing-person counterpart to add: edqro* carries ASCED levels ",
               "and no nursing, teaching or trade category. Pass entrant_frame = ",
               "TRUE if the statement is deliberately about entrants.",
               call. = FALSE)
}

# Education composition also breaks at wave 24. This is a warning rather
# than a stop, because educ_years is used as a control almost everywhere
# and the break is small relative to its variance; it is the TREND
# reading that is wrong.
assert_educ_trend <- function(data, context = NULL) {
  if (!any(data$wave %in% EDUC_COMPOSITION_BREAK_WAVES, na.rm = TRUE))
    return(invisible(TRUE))
  warning("compat: this frame includes wave(s) ",
          paste(intersect(unique(data$wave), EDUC_COMPOSITION_BREAK_WAVES),
                collapse = ", "),
          ", where education composition breaks", .compat_ctx(context),
          ". Do not read the last point of an education series as trend.",
          call. = FALSE)
  invisible(TRUE)
}

# =====================================================================
# 6. INTERRUPTION: THE FLOW, NOT THE LEVEL
# =====================================================================
# Pass a formula or a character vector of the variables entering the
# specification. Levels are stopped; the shares are allowed with a note,
# because dividing by career length is the other transformation that
# survives the cohort problem.
# v21.6 adds two arguments. data lets the guard read the panel's own
# experience_level_cols attribute rather than only this file's copy.
# allow_cohort_controlled is a SECOND escape hatch, and it exists because
# the first one was becoming a lie: allow_within_person announces that
# person fixed effects absorb the base stock, and a script with no person
# fixed effects that passes it is declaring something untrue in order to
# get past a guard. T8's DML is pooled across people, so the honest
# declaration there is that entry_cohort is controlled for, not that a
# fixed effect it does not have absorbs the problem.
assert_no_olf_level <- function(vars, context = NULL,
                                allow_within_person = FALSE,
                                allow_cohort_controlled = FALSE,
                                data = NULL) {
  if (inherits(vars, "formula")) vars <- all.vars(vars)
  vars <- as.character(vars)
  pool <- olf_level_cols(data)
  hit  <- intersect(vars, pool)
  # v21.14j: names built ON a restricted column count as the column.
  .der <- intersect(as.character(vars), names(DERIVED_FROM_LEVEL))
  if (length(.der)) {
    .via <- unique(unlist(DERIVED_FROM_LEVEL[.der]))
    hit <- unique(c(hit, intersect(.via, olf_level_cols(data))))
    if (length(hit))
      attr(hit, "derived_via") <- paste(
        sprintf("%s (built on %s)", .der, unlist(DERIVED_FROM_LEVEL[.der])),
        collapse = "; ")
  }
  .exp <- intersect(hit, c(EXPERIENCE_LEVEL_VARS,
                           if (!is.null(data))
                             attr(data, "experience_level_cols")))
  if (length(hit) && !allow_within_person && !allow_cohort_controlled)
    .compat_stop("compat: ", paste(hit, collapse = ", "),
                 " enters this specification as a LEVEL", .compat_ctx(context),
                 ". The base stock is a retrospective report taken at panel entry, ",
                 "so the level carries the entry cohort's recall vintage: 4.93 ",
                 "years out of the labour force for the original and 2011 intakes ",
                 "against 1.72 for the 2019-plus intake. ",
                 if (length(.exp))
                   paste0(paste(.exp, collapse = ", "),
                          " carries the SAME vintage through ehtjb: ",
                          "experience_years is actual_experience wherever ehtjb ",
                          "resolves, and the exp_hybrid_* stocks are ",
                          "base_exp_cal plus an in-panel accumulation, where ",
                          "base_exp_cal is experience_years at the person's first ",
                          "wave. For a within-person design use exp_cal_obs, ",
                          "exp_fte_obs or exp_one_obs, which are in-panel only and ",
                          "carry no base stock. ")
                 else "",
                 "Use ", paste(OLF_FLOW_VARS[1:2], collapse = " / "),
                 " or olf_share. If the level is inside person fixed effects and ",
                 "you want it anyway, pass allow_within_person = TRUE; if the ",
                 "specification is pooled across people but controls for ",
                 "entry_cohort, pass allow_cohort_controlled = TRUE. Say which in ",
                 "the notes; the two are not the same claim.", call. = FALSE)
  if (length(hit) && allow_within_person)
    .compat_say("compat: ", paste(hit, collapse = ", "), " kept as a level on ",
                "the caller's declaration that person fixed effects absorb the ",
                "base stock.")
  if (length(hit) && !allow_within_person && allow_cohort_controlled)
    .compat_say("compat: ", paste(hit, collapse = ", "), " kept as a level on ",
                "the caller's declaration that entry_cohort is controlled for",
                .compat_ctx(context), ". Report the cohort composition of the ",
                "estimation sample beside the estimate; a control is not a ",
                "balance check.")
  if (length(intersect(vars, OLF_RATIO_VARS)))
    .compat_say("compat: ", paste(intersect(vars, OLF_RATIO_VARS),
                                  collapse = ", "),
                " is a ratio, so it is cohort-robust in a way the level is not. ",
                "Report it beside a flow rather than instead of one.")
  invisible(TRUE)
}

# Rebuild the increments for a panel that predates v21. On a v21 panel
# this is a no-op and returns the columns already there, which is the
# point: one definition, wherever it is read.
olf_flows <- function(data, quiet = FALSE) {
  if (all(c("d_olf_years", "olf_flow_valid") %in% names(data))) {
    if (!quiet) .compat_say("compat: increments already on the panel; unchanged.")
    return(data)
  }
  require_cols(data, c("person_id", "wave", "years_not_in_lf",
                       "years_unemployed", "actual_experience",
                       "years_since_education"),
               context = "olf_flows")
  out <- data %>%
    dplyr::arrange(person_id, wave) %>%
    dplyr::group_by(person_id) %>%
    dplyr::mutate(
      entry_wave       = min(wave, na.rm = TRUE),
      d_wave           = wave - dplyr::lag(wave),
      d_paid_years     = actual_experience     - dplyr::lag(actual_experience),
      d_unemp_years    = years_unemployed      - dplyr::lag(years_unemployed),
      d_olf_years      = years_not_in_lf       - dplyr::lag(years_not_in_lf),
      d_exposure_years = years_since_education -
        dplyr::lag(years_since_education)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      dplyr::across(c(d_paid_years, d_unemp_years, d_olf_years,
                      d_exposure_years),
                    ~ ifelse(is.na(d_wave) | d_wave != 1L, NA_real_, .x)),
      history_flow_sum = d_paid_years + d_unemp_years + d_olf_years,
      olf_year         = ifelse(is.na(d_olf_years), NA_integer_,
                                as.integer(d_olf_years > 0.5)),
      olf_flow_valid   = as.integer(!is.na(d_olf_years)))
  if (!quiet)
    .compat_say("compat: increments rebuilt for a pre-", COMPAT_BUILD_TAG,
                " panel. Prefer a rebuilt panel.")
  out
}

# The estimation frame for anything reading a flow: consecutive waves
# only. An increment across a gap year is not an annual flow.
olf_flow_frame <- function(data) {
  data <- olf_flows(data, quiet = TRUE)
  dplyr::filter(data, olf_flow_valid == 1)
}

# =====================================================================
# 6b. THE TRAINING GATE (v21.6)
# =====================================================================
# Stops when one of the coalesced training indicators enters a
# specification, and names its gated twin. Two escape hatches, because
# there are two legitimate reasons to want the _any column.
#
#   allow_structural_zero  the design WANTS every person-wave scored,
#                          with "never asked" counted as "did not train".
#                          T1's unconditional estimand is exactly this,
#                          and it is a defensible object as long as it is
#                          named. It is not the same object as the
#                          employed-only rate and the two must not be
#                          quoted against each other without saying so.
#   employed_frame         the frame is already restricted to the
#                          employed, so the routing has been handled by
#                          the sample rather than by the column. This
#                          still messages, because employment is not the
#                          only reason the module goes unasked.
assert_training_gate <- function(vars, context = NULL,
                                 allow_structural_zero = FALSE,
                                 employed_frame = FALSE, data = NULL) {
  if (inherits(vars, "formula")) vars <- all.vars(vars)
  hit <- intersect(as.character(vars), training_gate_cols(data))
  if (!length(hit)) return(invisible(TRUE))
  alt <- unname(TRAINING_GATED_ALT[hit])
  alt <- alt[!is.na(alt)]
  if (allow_structural_zero) {
    # v21.8: the loader measured this rather than assuming it, so the
    # message carries the measured sentence where one is available
    # instead of a generic reassurance. On a v21.6 panel the fact is
    # absent and the old wording stands.
    .note <- tryCatch(training_gate_facts(data)$structural_zero_note,
                      error = function(e) NULL)
    .compat_say("compat: ", paste(hit, collapse = ", "), " kept on the caller's ",
                "declaration that a never-asked person-wave should count as a ",
                "training zero", .compat_ctx(context),
                ". That is the unconditional estimand; say so where it is ",
                "reported and do not put it in the same table as an ",
                "employed-only rate without the label.")
    if (!is.null(.note) && nzchar(.note))
      .compat_say("compat: the loader measured what that zero is. ", .note,
                  " Report this sentence, or the numbers in it, wherever the ",
                  "unconditional arm appears.")
    else
      .compat_say("compat: this panel carries no measured structural_zero_note, ",
                  "so the size and direction of the imputation are undeclared. ",
                  "Rebuild on v21.8 or later before reporting the ",
                  "unconditional arm.")
    return(invisible(TRUE))
  }
  if (employed_frame) {
    .compat_say("compat: ", paste(hit, collapse = ", "), " kept on an ",
                "employed-only frame", .compat_ctx(context),
                ". Employment is the main reason the module goes unasked but ",
                "not the only one; training_module_asked reports the residual ",
                "and should be checked once rather than assumed to be zero.")
    return(invisible(TRUE))
  }
  .compat_stop("compat: ", paste(hit, collapse = ", "), " read",
               if (length(hit) == 1L) "s" else "",
               " a person-wave the training module was NEVER PUT TO as a training ",
               "zero", .compat_ctx(context),
               ". The loader builds ", if (length(hit) == 1L) "it" else "them",
               " as ifelse(coalesce(x_raw, 0) == 1, 1, 0), so ",
               if (length(hit) == 1L) "it is" else "they are",
               " never NA and two states collapse into one: asked and did not ",
               "train, and never asked. The second is not a behavioural zero and ",
               "it is not missing at random, because the module is ",
               "employment-routed. Use ", paste(alt, collapse = ", "),
               ", which ", if (length(alt) == 1L) "is" else "are",
               " NA where the module was not put, or pass allow_structural_zero = ",
               "TRUE if the unconditional zero is the object you want, or ",
               "employed_frame = TRUE once the frame is restricted. NOTE that a ",
               "!is.na() filter on the _any column is inert and does not count as ",
               "handling this.")
}

# The count the loader measured at build time, so a script can report it
# without recomputing it. NULL on a pre-v21.6 panel.
training_gate_facts <- function(data) attr(data, "training_gate_facts")

# =====================================================================
# 7. NO INDIVIDUAL SUPERANNUATION COMPARISON
# =====================================================================
# hwsupei is a household aggregate: partners hold an identical value on
# essentially every couple-wave where both are observed. It supports
# household description and no individual terminal-value comparison,
# whatever the row it sits on suggests.
# v21.14e. THE TWO REGIMES THE LOADER DECLARES AND NOTHING ENFORCED.
#
# The v21.14e manifest carries ten restrictions. Eight had an assert in
# this file; training_gate_position and occ_origin_coverage did not, so
# the panel knew about them, the manifest printed them, and no guard
# stopped a design walking straight through either. A declared-and-
# unenforced restriction is worse than an undeclared one, because it
# reads as covered.
#
# --- ONE. The gate item moves PQ E1 -> E6 at wave 20 -------------------
#
# NOT a coverage break, so not modelled on assert_firm_size_regime. The
# population reaching jttrwrk does not change: training_module_asked runs
# 86.5% at wave 12 to 88.9% at wave 24 with no step. What changes is the
# CONTEXT, and wave 20 is also 2020, so the questionnaire move and the
# pandemic cannot be separated by any specification on this panel.
#
# That is why this WARNS with an acknowledgement rather than stopping.
# There is no correct side to restrict to and no correction to apply. The
# design has to decide, and the point of the guard is that the decision is
# made rather than inherited. Pass allow_pooled_regime = TRUE to record
# that it was made.
assert_training_gate_position <- function(vars, context = NULL,
                                          data = NULL,
                                          allow_pooled_regime = FALSE) {
  if (inherits(vars, "formula")) vars <- all.vars(vars)
  vars <- as.character(vars)
  gated <- intersect(vars, c(training_gate_cols(data), "any_training",
                             "total_training_hours", "total_training_hours_z",
                             "training_any_reported"))
  if (!length(gated)) return(invisible(TRUE))
  if (is.null(data) || !"wave" %in% names(data)) return(invisible(TRUE))
  sides <- unique(ifelse(data$wave < TRAINING_GATE_POSITION_BREAK_WAVE,
                         "E1", "E6"))
  if (length(sides) < 2L) return(invisible(TRUE))
  msg <- paste0("compat: ", paste(gated, collapse = ", "),
                " pool", if (length(gated) == 1L) "s" else "",
                " across the wave ",
                TRAINING_GATE_POSITION_BREAK_WAVE,
                " gate-item move", .compat_ctx(context),
                ". jttrwrk is PQ E1 in waves 7-19 and PQ E6 in 20-24, so ",
                "the item is asked in a different position on either side. ",
                "Wave ", TRAINING_GATE_POSITION_BREAK_WAVE,
                " is also 2020 and NOTHING on this panel separates the two. ",
                "There is no correct side to restrict to; report both, or ",
                "interact on training_gate_regime, and say which you did.")
  if (allow_pooled_regime) {
    message(msg, " Pooling kept on the caller's declaration.")
    return(invisible(TRUE))
  }
  warning(msg, " Pass allow_pooled_regime = TRUE once the choice is made.",
          call. = FALSE)
  invisible(FALSE)
}

# --- TWO. The origin-occupation coverage break, same wave --------------
#
# This one IS a coverage break and does stop, on the firm_size pattern.
# pjoo62 and pjoo6s end at wave 19, so the job-change route into
# occ2_origin disappears and coverage falls from about 6.8% of
# person-waves to 3.1%. The column is the same column; before the break
# it observes movers INCLUDING job changers and after it observes only
# the unemployed and not-in-work. Pooling compares two mover definitions.
OCC_ORIGIN_VARS <- c("occ2_origin", "occstat_origin", "occstat_change",
                     "occ_origin_source", "occ2_origin_jobchange",
                     "occstat_origin_jobchange")
assert_occ_origin_regime <- function(vars, context = NULL, data = NULL) {
  if (inherits(vars, "formula")) vars <- all.vars(vars)
  hit <- intersect(as.character(vars), OCC_ORIGIN_VARS)
  if (!length(hit) || is.null(data) || !"wave" %in% names(data))
    return(invisible(TRUE))
  side <- if ("occ_origin_regime" %in% names(data)) {
    data$occ_origin_regime
  } else {
    ifelse(data$wave < OCC_ORIGIN_BREAK_WAVE, "three_routes", "two_routes")
  }
  keep <- Reduce(`|`, lapply(hit, function(h)
    if (h %in% names(data)) !is.na(data[[h]]) else rep(FALSE, nrow(data))))
  sides <- sort(unique(side[keep]))
  if (length(sides) < 2L) return(invisible(TRUE))
  n <- table(side[keep])
  .compat_stop("compat: ", paste(hit, collapse = ", "), " observed on BOTH sides of ",
               "the wave ", OCC_ORIGIN_BREAK_WAVE, " origin-occupation coverage ",
               "break", .compat_ctx(context), " (",
               paste(sprintf("%s n=%s", names(n),
                             format(as.integer(n), big.mark = ",")),
                     collapse = ", "),
               "). The job-change route ends at wave ", OCC_ORIGIN_BREAK_WAVE - 1L,
               ", so before the break these observe movers INCLUDING job changers ",
               "and after they observe only the unemployed and not-in-work. ",
               "Restrict to one side, or interact on occ_origin_regime.",
               call. = FALSE)
}

assert_no_individual_super <- function(vars, context = NULL) {
  if (inherits(vars, "formula")) vars <- all.vars(vars)
  vars <- as.character(vars)
  if ("super_balance" %in% vars)
    .compat_stop("compat: super_balance was renamed hh_super_balance in ",
                 COMPAT_BUILD_TAG, .compat_ctx(context),
                 ". The item is a household aggregate.", call. = FALSE)
  hit <- intersect(vars, HOUSEHOLD_ONLY_VARS)
  if (length(hit))
    .compat_say("compat: ", paste(hit, collapse = ", "),
                " is measured at the HOUSEHOLD level", .compat_ctx(context),
                ". It cannot carry an individual comparison; a within-couple ",
                "contrast on it is a comparison of a household with itself.")
  invisible(TRUE)
}

# =====================================================================
# 8. WEALTH: WAVES 14, 18 AND 22
# =====================================================================
# ---------------------------------------------------------------------
# 8b. MODULE VARIABLES ARE NOT SERIES
# ---------------------------------------------------------------------
# Wealth was given a guard because its module shape was known. The v21.3
# loader's wave-hole sweep found the same shape elsewhere: ftcpg, ftipg,
# ftcpgm and ftcpgy exist only at waves 15, 19 and 23, so
# currently_pregnant, self_or_partner_pregnant, expected_birth_month and
# expected_birth_year are three-wave columns sitting in a thirteen-wave
# panel. That is the item family DECISIONS.txt records T7 as having
# failed on, and it failed by reading a coverage table without fixing the
# role first.
#
# The risk is not a value in the wrong wave. It is a mean taken over
# whichever module waves happen to fall inside the window and then read
# as a panel average. Registered here so the next module found by the
# sweep is one line rather than another function.
MODULE_WAVE_VARS <- list(
  wealth = list(
    waves = WEALTH_MODULE_WAVES,
    vars  = c("hh_super_balance", "hh_net_worth",
              "hh_net_worth_pos", "hh_net_worth_neg")),
  fertility = list(
    waves = c(15L, 19L, 23L),
    vars  = c("currently_pregnant", "self_or_partner_pregnant",
              "expected_birth_month", "expected_birth_year"))
)

# Stops when a module variable enters a specification whose frame reaches
# outside the module's waves, and reports how many module waves the frame
# actually has. Pass module_frame = TRUE once the frame has been
# restricted, or restrict it with module_frame().
assert_module_waves <- function(data, vars = NULL, context = NULL,
                                allow_span = FALSE) {
  if (inherits(vars, "formula")) vars <- all.vars(vars)
  for (nm in names(MODULE_WAVE_VARS)) {
    m <- MODULE_WAVE_VARS[[nm]]
    hit <- if (is.null(vars)) intersect(m$vars, names(data)) else
      intersect(as.character(vars), m$vars)
    if (!length(hit)) next
    off <- vapply(intersect(hit, names(data)), function(cl)
      sum(!is.na(data[[cl]]) & !data$wave %in% m$waves), numeric(1))
    if (length(off) && any(off > 0))
      .compat_stop("compat: ", nm, " values outside waves ",
                   paste(m$waves, collapse = ", "), .compat_ctx(context), ": ",
                   paste(sprintf("%s = %d", names(off)[off > 0],
                                 as.integer(off[off > 0])), collapse = ", "),
                   ". The module is periodic; a value elsewhere is a mapping ",
                   "error, not an observation.", call. = FALSE)
    spans <- any(!data$wave %in% m$waves, na.rm = TRUE)
    have  <- sort(intersect(unique(data$wave), m$waves))
    if (spans && !allow_span)
      .compat_stop("compat: ", paste(hit, collapse = ", "), " is a ", nm,
                   " MODULE variable, observed at wave(s) ",
                   paste(m$waves, collapse = ", "), " only",
                   .compat_ctx(context),
                   ", and this frame reaches outside them. A mean over it is a ",
                   "mean over ", length(have), " wave(s) presented as a panel ",
                   "average. Restrict with module_frame(data, \"", nm, "\") or ",
                   "pass allow_span = TRUE if the periodicity is the point.",
                   call. = FALSE)
    if (spans && allow_span)
      .compat_say("compat: ", nm, " module variable kept across non-module ",
                  "waves on the caller's declaration; it is observed in ",
                  length(have), " wave(s) here.")
  }
  invisible(TRUE)
}

module_frame <- function(data, module = names(MODULE_WAVE_VARS)) {
  module <- match.arg(module)
  w <- MODULE_WAVE_VARS[[module]]$waves
  out <- dplyr::filter(data, wave %in% w)
  if (!nrow(out))
    .compat_stop("compat: no rows left after restricting to the ", module,
                 " module waves (", paste(w, collapse = ", "), ").", call. = FALSE)
  out
}

wealth_frame <- function(data) {
  out <- dplyr::filter(data, wave %in% WEALTH_MODULE_WAVES)
  if (!nrow(out))
    .compat_stop("compat: no rows left after restricting to the wealth module ",
                 "waves (", paste(WEALTH_MODULE_WAVES, collapse = ", "), ").",
                 call. = FALSE)
  out
}

assert_wealth_waves <- function(data, context = NULL, vars = WEALTH_VARS) {
  vars <- intersect(vars, names(data))
  if (!length(vars)) return(invisible(TRUE))
  off <- vapply(vars, function(cl)
    sum(!is.na(data[[cl]]) & !data$wave %in% WEALTH_MODULE_WAVES),
    numeric(1))
  if (any(off > 0))
    .compat_stop("compat: wealth values outside waves ",
                 paste(WEALTH_MODULE_WAVES, collapse = ", "), .compat_ctx(context),
                 ": ", paste(sprintf("%s = %d", vars[off > 0],
                                     as.integer(off[off > 0])), collapse = ", "),
                 ". The module is four-yearly; a value elsewhere is a mapping ",
                 "error, not an observation.", call. = FALSE)
  waves <- sort(unique(data$wave[data$wave %in% WEALTH_MODULE_WAVES]))
  if (length(waves) < length(WEALTH_MODULE_WAVES))
    .compat_say("compat: wealth frame covers wave(s) ",
                paste(waves, collapse = ", "), " of ",
                paste(WEALTH_MODULE_WAVES, collapse = ", "),
                ", so at most ", length(waves), " observation(s) per person.")
  invisible(TRUE)
}

# =====================================================================
# 9. ONE CALL FOR THE WHOLE SET
# =====================================================================
# Convenience wrapper for the common case: an estimation frame plus the
# variables entering the specification. Each guard is also callable on
# its own, which is what a script wanting one of them should do.
compat_assert_spec <- function(data, vars = character(0), context = NULL,
                               allow_within_person_olf = FALSE,
                               allow_cohort_controlled_olf = FALSE,
                               allow_structural_zero_training = FALSE,
                               employed_frame = FALSE) {
  if (inherits(vars, "formula")) vars <- all.vars(vars)
  assert_firm_size_regime(data, context = context, vars = vars)
  assert_study_comparable(data, context = context)
  assert_no_olf_level(vars, context = context,
                      allow_within_person = allow_within_person_olf,
                      allow_cohort_controlled = allow_cohort_controlled_olf,
                      data = data)
  assert_training_gate(vars, context = context,
                       allow_structural_zero = allow_structural_zero_training,
                       employed_frame = employed_frame, data = data)
  assert_no_individual_super(vars, context = context)
  assert_not_entrant_only(vars, context = context, data = data)
  assert_module_waves(data, vars, context = context)
  if (any(intersect(vars, WEALTH_VARS) %in% vars) &&
      length(intersect(vars, WEALTH_VARS)))
    assert_wealth_waves(data, context = context)
  invisible(TRUE)
}

compat_manifest <- function() {
  m <- data.frame(
    restriction = c("firm_size", "education_w24", "enrolment",
                    "interruption", "experience", "training_gate",
                    "superannuation", "wealth",
                    "module items", "removed names"),
    rule = c(sprintf("one regime; never across wave %d",
                     FIRM_SIZE_BREAK_WAVE),
             sprintf("wave %d is a break, not a trend",
                     STUDY_ITEM_BREAK_WAVE),
             "NPQ items: entrant counts, not panel counts",
             "within-person flow, not the level",
             "same recall vintage; in-panel stocks are clean",
             "_any counts never-asked as a zero; _obs does not",
             "household aggregate; no individual comparison",
             sprintf("waves %s only",
                     paste(WEALTH_MODULE_WAVES, collapse = "/")),
             "periodic; not a thirteen-wave series",
             "pregnancy versus birth decided per site"),
    guard = c("assert_firm_size_regime()", "assert_study_comparable()",
              "assert_not_entrant_only()", "assert_no_olf_level()",
              "assert_no_olf_level()", "assert_training_gate()",
              "assert_no_individual_super()", "assert_wealth_waves()",
              "assert_module_waves()", "event_col()"),
    stringsAsFactors = FALSE)
  print(m, row.names = FALSE, right = FALSE)
  cat("\nColumn guard: require_cols(data, cols, context). An all-NA column\n")
  cat("is a missing variable, not a zero.\n")
  cat("Coverage: compat_guard_coverage(path) reports which of these a\n")
  cat("script actually calls. A guard named in a GUARD_MUST_DEFINE vector\n")
  cat("and never invoked has never fired.\n")
  invisible(m)
}

# =====================================================================
# 10. WHICH GUARDS DOES A SCRIPT ACTUALLY CALL? (v21.6)
# =====================================================================
# Every function here is passive. It fires when a script calls it and is
# inert otherwise, so a name added to OLF_LEVEL_VARS does nothing at all
# for a script that never calls assert_no_olf_level. The v21 scan found
# 03_training_designs.R requiring compat_panel_check, require_cols and
# assert_no_olf_level in GUARD_MUST_DEFINE and calling only the first,
# and using a plain setdiff() where require_cols() belongs, which is the
# check that cannot tell an absent column from an all-NA one.
#
# Text search, so it counts a mention rather than an execution. That is
# the right sensitivity: a guard mentioned nowhere has certainly never
# run, and that is the finding worth having cheaply.
COMPAT_GUARDS <- c("compat_panel_check", "require_cols",
                   "assert_firm_size_regime", "assert_study_comparable",
                   "assert_not_entrant_only", "assert_educ_trend",
                   "assert_no_olf_level", "assert_training_gate",
                   "assert_no_individual_super", "assert_module_waves",
                   "assert_wealth_waves", "event_col",
                   "olf_flows", "olf_flow_frame")

compat_guard_coverage <- function(paths, quiet = FALSE) {
  paths <- paths[file.exists(paths)]
  if (!length(paths)) {
    warning("compat: no readable script in the paths given.", call. = FALSE)
    return(invisible(NULL))
  }
  out <- do.call(rbind, lapply(paths, function(p) {
    src <- readLines(p, warn = FALSE)
    src <- src[!grepl("^\\s*#", src)]          # ignore commented mentions
    data.frame(script = basename(p), guard = COMPAT_GUARDS,
               calls = vapply(COMPAT_GUARDS, function(g)
                 sum(grepl(paste0("\\b", g, "\\s*\\("), src)), integer(1)),
               stringsAsFactors = FALSE)
  }))
  out$status <- ifelse(out$calls > 0, "called", "NEVER CALLED")
  if (!quiet) {
    for (sc in unique(out$script)) {
      z <- out[out$script == sc, ]
      cat("\n", sc, ": ", sum(z$calls > 0), " of ", nrow(z),
          " guards called\n", sep = "")
      miss <- z$guard[z$calls == 0]
      if (length(miss))
        cat(strwrap(paste("  never called:", paste(miss, collapse = ", ")),
                    width = 72, exdent = 4), sep = "\n")
    }
    cat("\nA guard is passive. One that is never called has never fired,\n")
    cat("whatever this file says about the restriction it enforces.\n")
  }
  invisible(out)
}

# The banner names the TAG, not a filename: the file has been saved under
# more than one name already, and a message that invents a path is a
# message that can be wrong.
cat("compat guards loaded, tag ", COMPAT_BUILD_TAG,
    ". compat_manifest() lists them.\n", sep = "")

# =====================================================================
# SELF-TEST. Set COMPAT_SELFTEST <- TRUE before sourcing to run it.
# Cheap enough to run whenever this file is edited, and it is the only
# way to know a guard still fires after a change to it.
# =====================================================================
if (isTRUE(COMPAT_SELFTEST)) {
  .t <- function(label, expr, expect = c("stop", "pass")) {
    expect <- match.arg(expect)
    got <- tryCatch({ suppressMessages(force(expr)); "pass" },
                    error = function(e) "stop")
    cat(sprintf("  %-46s %s\n", label,
                if (identical(got, expect)) "ok" else
                  sprintf("FAILED (expected %s, got %s)", expect, got)))
  }
  .d <- data.frame(
    person_id = rep(sprintf("P%02d", 1:10), each = 13),
    wave = rep(12:24, times = 10),
    firm_size = "Small", firm_size_regime = NA_character_,
    enrolled_in_course = 1, full_time_student = 1,
    years_not_in_lf = 1, years_unemployed = 1, actual_experience = 1,
    years_since_education = 1, hh_super_balance = NA_real_,
    experience_years = 5, exp_hybrid_cal = 6, exp_cal_obs = 2,
    jttpewt_any = 0L, jttpewt_obs = NA_integer_,
    all_na_col = NA_real_, stringsAsFactors = FALSE)
  .d$firm_size_regime <- ifelse(.d$wave < FIRM_SIZE_BREAK_WAVE, "pre", "post")
  cat("\ncompat self-test\n")
  .t("firm size on both sides stops",
     assert_firm_size_regime(.d, "selftest"), "stop")
  .t("firm size present but not in the spec passes",
     assert_firm_size_regime(.d, "selftest", vars = ~ y + female), "pass")
  .t("firm size on one side passes",
     assert_firm_size_regime(firm_size_side(.d, "post"), "selftest"), "pass")
  .t("study items across wave 24 stop",
     assert_study_comparable(.d, context = "selftest"), "stop")
  .t("study items across wave 24 with the declaration pass",
     assert_study_comparable(.d, "selftest", allow_span = TRUE), "pass")
  .t("study items after drop_study_break pass",
     assert_study_comparable(drop_study_break(.d), context = "selftest"),
     "pass")
  .t("OLF level in a spec stops",
     assert_no_olf_level(~ years_not_in_lf + female, "selftest"), "stop")
  .t("OLF flow in a spec passes",
     assert_no_olf_level(~ d_olf_years + female, "selftest"), "pass")
  .t("OLF level with the declaration passes",
     assert_no_olf_level(~ years_not_in_lf, "selftest",
                         allow_within_person = TRUE), "pass")
  .t("old super name stops",
     assert_no_individual_super(~ super_balance, "selftest"), "stop")
  .t("all-NA column stops",
     require_cols(.d, "all_na_col", "selftest"), "stop")
  .t("absent column stops",
     require_cols(.d, "no_such_column", "selftest"), "stop")
  .t("unregistered event site stops",
     event_col("had_birth_this_wave", site = "x.R:unregistered"), "stop")
  .t("event site with an inline decision passes",
     event_col("had_birth_this_wave", site = "x.R:site", want = "birth"),
     "pass")
  .t("entrant-only enrolment in a spec stops",
     assert_not_entrant_only(~ enrolled_in_course + female, "selftest"), "stop")
  .t("entrant-only enrolment with the declaration passes",
     assert_not_entrant_only(~ enrolled_in_course, "selftest",
                             entrant_frame = TRUE), "pass")
  .t("empty-by-design business flag stops with its own reason",
     require_cols(transform(.d, has_business_any = NA_real_),
                  "has_business_any", "selftest"), "stop")
  .t("removed column on a panel stops",
     compat_panel_check(transform(.d, years_unemp_since_school = 1),
                        quiet = TRUE), "stop")
  .t("field-of-study flag in a spec stops",
     assert_not_entrant_only(~ has_nursing_any + female, "selftest"), "stop")
  .t("measured attribute extends the list",
     { .p <- .d; attr(.p, "entrant_only_cols") <- "some_new_col"
     assert_not_entrant_only(~ some_new_col, "selftest", data = .p) }, "stop")
  .t("full_time_student_yes passes",
     assert_not_entrant_only(~ full_time_student_yes, "selftest"), "pass")
  .t("fertility module var across the window stops",
     assert_module_waves(transform(.d, currently_pregnant =
                                     ifelse(wave %in% c(15, 19, 23), 1, NA)),
                         "currently_pregnant", "selftest"), "stop")
  .t("fertility module var inside its own frame passes",
     assert_module_waves(module_frame(
       transform(.d, currently_pregnant =
                   ifelse(wave %in% c(15, 19, 23), 1, NA)), "fertility"),
       "currently_pregnant", "selftest"), "pass")
  .t("module var with the declaration passes",
     assert_module_waves(transform(.d, currently_pregnant =
                                     ifelse(wave %in% c(15, 19, 23), 1, NA)),
                         "currently_pregnant", "selftest",
                         allow_span = TRUE), "pass")
  .t("module value in a non-module wave stops",
     assert_module_waves(transform(.d, currently_pregnant =
                                     ifelse(wave == 16, 1, NA)),
                         "currently_pregnant", "selftest",
                         allow_span = TRUE), "stop")
  .t("study indicator across wave 24 stops",
     assert_study_comparable(transform(.d, full_time_student_yes = 1L),
                             context = "selftest"), "stop")
  .t("empty-by-design column stops with its own reason",
     require_cols(transform(.d, firm_size_comparable = NA_character_),
                  "firm_size_comparable", "selftest"), "stop")
  .t("wealth value outside the module waves stops",
     assert_wealth_waves(transform(.d, hh_super_balance =
                                     ifelse(wave == 15, 1, NA)), "selftest"),
     "stop")
  # ---- v21.6 ---------------------------------------------------------
  .t("experience_years as a level stops",
     assert_no_olf_level(~ experience_years + female, "selftest"), "stop")
  .t("exp_hybrid_cal as a level stops",
     assert_no_olf_level(~ exp_hybrid_cal + female, "selftest"), "stop")
  .t("in-panel exp_cal_obs passes",
     assert_no_olf_level(~ exp_cal_obs + female, "selftest"), "pass")
  .t("experience level with the cohort declaration passes",
     assert_no_olf_level(~ experience_years, "selftest",
                         allow_cohort_controlled = TRUE), "pass")
  .t("experience_level_cols attribute extends the list",
     { .p <- .d; attr(.p, "experience_level_cols") <- "some_stock"
     assert_no_olf_level(~ some_stock, "selftest", data = .p) }, "stop")
  .t("jttpewt_any in a spec stops",
     assert_training_gate(~ jttpewt_any + female, "selftest"), "stop")
  .t("jttpewt_obs in a spec passes",
     assert_training_gate(~ jttpewt_obs + female, "selftest"), "pass")
  .t("training gate with the structural-zero declaration passes",
     assert_training_gate(~ jttpewt_any, "selftest",
                          allow_structural_zero = TRUE), "pass")
  .t("training gate on an employed frame passes",
     assert_training_gate(~ jttpewt_any, "selftest",
                          employed_frame = TRUE), "pass")
  .t("training_gate_cols attribute extends the list",
     { .p <- .d; attr(.p, "training_gate_cols") <- "some_any_col"
     assert_training_gate(~ some_any_col, "selftest", data = .p) }, "stop")
  .t("v21.5 panel tag warns: below the floor",
     { .p <- .d; attr(.p, "build_tag") <- "v21.5"
     withCallingHandlers(compat_panel_check(.p, quiet = TRUE),
                         warning = function(w) stop("warned")) }, "stop")
  
  # v21.34. THE ORDERING TEST, which is the one that would have caught a
  # string comparison. "v21.33" < "v21.6" alphabetically, so a mechanism
  # that compared raw strings would reject the newest panel while
  # accepting an older one, and would look correct until minor version 10.
  .t("v21.33 orders above v21.6, not below",
     if (.tag_num("v21.33") > .tag_num("v21.6")) "pass" else stop("inverted"),
     "pass")
  .t("letter suffix orders after its base",
     if (.tag_num("v21.14g") > .tag_num("v21.14") &&
         .tag_num("v21.14g") < .tag_num("v21.15")) "pass" else stop("bad"),
     "pass")
  .t("a malformed tag is rejected, not coerced",
     if (is.na(.tag_num("21.6")) && is.na(.tag_num("v21")) &&
         !.tag_accepted("not-a-tag")) "pass" else stop("accepted"),
     "pass")
  
  # The gap the old whitelist actually had: four builds missing from the
  # middle. Under a range they are accepted, which is the intended
  # behaviour and worth asserting rather than assuming.
  .t("v21.22, absent from the old whitelist, is accepted by the range",
     if (.tag_accepted("v21.22")) "pass" else stop("rejected"), "pass")
  
  # And the exclusion vector still works, because a range alone cannot
  # express a hole.
  .t("an excluded tag is rejected even though it is above the floor",
     { .old <- COMPAT_EXCLUDED_TAGS
     COMPAT_EXCLUDED_TAGS <<- "v21.20"
     r <- .tag_accepted("v21.20")
     COMPAT_EXCLUDED_TAGS <<- .old
     if (!r) "pass" else stop("accepted an excluded tag") }, "pass")
  
  # A panel above the reviewed ceiling must WARN, not stop: it is usable
  # and merely unreviewed, and conflating the two would make every bump
  # look like a broken build.
  .t("a panel above COMPAT_REVIEWED_TO warns but does not stop",
     { .p <- .d; attr(.p, "build_tag") <- "v99.1"
     withCallingHandlers(compat_panel_check(.p, quiet = TRUE),
                         warning = function(w) stop("warned")) }, "stop")
  rm(.d, .t)
}

# ---- contract: hand off, verify, close -------------------------------
# td_put persists to handoff/, so the next script reads THIS object
# rather than whatever survived in the global environment.
# td_check_seals fails if a shared helper was redefined above: the
# last definition sourced wins and no call site says which is live.
td_check_seals("00compat.R")
# v21.7. td_log_close() removed with the td_log_open() above. Closing a
# log this file did not open is what unwound the caller's sinks.
