# =====================================================================
# 03_common.R  -  the prelude shared by 03a, 03b and 03c
# ---------------------------------------------------------------------
# EXTRACTED VERBATIM from 03training_designs.R lines 1-1281. Nothing in
# the body below was rewritten; it was cut at a boundary that parses on
# both sides and two lines were parameterised, both named at the foot of
# this header.
#
# WHY 03 WAS SPLIT. At 5,884 lines it did three jobs with different
# runtimes, different estimands and different sections of the paper, and
# MASTER_RUN_T8 and MASTER_RUN_PART2 were already a split implemented as
# flags inside one file. The line counts say the rest:
#
#     T8, the DML swap        1,989 lines   34% of the file
#     T3/T3b/T3c, the fee      648 lines
#     everything else        3,247 lines
#
# T8 is the whole of the separation argument. It estimates the return to
# experience, which is Section 7.3 and the PRICE of human capital. It is
# not a training design, it is the only slow block, and while it sat in
# this file an iteration run of the design audit could not avoid it
# except through a flag that then had to be threaded into the ledger, the
# skip audit and the master's run record.
#
#     03a_design_audit.R        T0b, T1, T2, T4, T5, T6, T7, T9
#     03b_fee_descriptives.R    T3, T3b, T3c
#     03c_experience_units.R    T8
#
# WHAT THIS FILE IS FOR. All three need the same contract guard, the same
# configuration constants, the same helpers (report, note, fe_block), the
# same panel and event frame, and the same benchmarks. Duplicating that
# across three files is the failure the contract exists to prevent: a
# fourth copy that differs is invisible until it errors somewhere else.
# One prelude, sourced by each.
#
# SOURCE IT LIKE THIS, and set the module name first so the log and the
# transcript carry it:
#
#     TD03_MODULE <- "03a_design_audit.R"
#     if (!isTRUE(.TD03_COMMON_DONE)) source("03_common.R")
#
# The guard is at the CALL SITE rather than wrapped around this file,
# because wrapping 1,281 lines in an if() puts every object one scope
# deeper and changes what <<- in note() reaches.
#
# THE TWO PARAMETERISED LINES, and they are the only edits:
#
#   td_log_open(...)   was td_log_open("03training_designs.R"); now reads
#                      TD03_MODULE when set, so three modules in one
#                      session do not all claim the same log name.
#   .t_log_con         was training_log.txt; now <module>_log.txt for the
#                      same reason. A shared name means the last module to
#                      run silently overwrites the transcript of the two
#                      before it.
#
# WHAT IS NOT HERE. The ledger write, the decisions file, the blind RDS
# and the sink close all live in 03_finish.R, because each module now
# contributes PART of a ledger the master reads as one file. Merging is a
# job for the finisher, not for whichever module happened to run last.
# =====================================================================

# =====================================================================
# 03_TRAINING_DESIGNS.R    revision 4
#
# Turning the training section from a corroborating illustration into a
# mechanism test.
#
# The manuscript reports two side-by-side event studies: employer-funded
# training falls 14.4 points at the birth wave and stays down, own-time
# training falls about 5 points and recovers. That asymmetry is the most
# informative thing in the section and it is currently left as a
# juxtaposition. A mother who was choosing to invest less in her own
# skills should cut own-time training too, and persistently. The fact
# that only the employer-provided margin stays down is a statement about
# who stopped, and it can be tested directly rather than inferred.
#
# Nine designs. T8 and T9 arrived after the original seven, and they run
# in the order T1 to T7, then T9, then T8: T9 can overturn a sentence
# already drafted, and T8 is the only block that takes tens of minutes.
#   T1  Estimand labelling: unconditional, employed-only, attached.
#   T2  Reference period, the calendar placebo, the sex-by-wave term.
#   T3  Funding-source triple difference.
#   T4  Full-time returners.
#   T5  Incidence against dosage, structural intensity zeros recovered.
#   T6  Within-couple, couple-by-wave fixed effects.
#   T7  Anticipation, on the life-event pregnancy item.
#   T9  Substitution into formal study.
#   T8  The DML swap: the return to experience in FTE units.
#
# ---------------------------------------------------------------------
# REVISION 4. The loader has implemented every recommendation T7 made,
# so most of this revision is deleting advice that has been taken and
# updating names. Tagged J1 to J7.
#
# NOTHING IS DECIDED HERE ABOUT PREGNANCY AGAINST BIRTH. That choice
# belongs to whoever wrote each site and it is not mechanical, so where a
# site is updated the update is a transcription of what the site already
# says about itself. T7 names its local indicator preg_rep, states three
# times that every row is a pregnancy contrast, and titles itself
# anticipation. It therefore reads the pregnancy column. Any site that
# does NOT say which it wants is left alone and reported.
#
#  J1 THE GUARDS ARE SOURCED. This script estimates on the panel, so it
#     now finds a compat file by search, checks the build tag, prints the
#     restriction manifest and declares its exemptions where they are
#     used. Found by pattern rather than by name: 02 revision 4 first
#     hard-coded 00_compat_v21.R and failed on the first run because the
#     file is saved as 00compat.R.
#
#  J2 THE v17 NAMES ARE GONE FROM THE PANEL. event_birth and
#     had_birth_this_wave were removed by the loader's v17 hard rename and
#     replaced by event_pregnancy and event_child, had_pregnancy_this_wave
#     and had_child_this_wave. Every reference here was inside a defensive
#     intersect() or a %in% test, so nothing errored; T7 simply fell
#     through to a fallback and reported that the life-event item was
#     absent when in fact it was present under a truer name.
#
#  J3 THE CODING IS NO LONGER GUESSED. T7 tested == 2 on the reasoning
#     that the life-event family codes 1 = No and 2 = Yes. The loader now
#     DETERMINES that from the data in every wave, by picking the pair of
#     codes that maximises the share of events carrying exactly one
#     quarter flag, and got the same answer in all thirteen: event code 2,
#     flag code 1, with 90 to 98 per cent of events single-flagged. So the
#     derived indicator is preferred and the raw test is the fallback,
#     which is what the block always intended.
#
#  J4 THE THREE RECOMMENDATIONS T7 MADE ARE DONE, and the notes that made
#     them are replaced by a record of what arrived. lebth is read and is
#     event_child with had_child_this_wave beside it. leprgq1 to leprgq4
#     and lebthq1 to lebthq4 are read, reconciled to an exact identity,
#     and summarised as preg_quarter_event, birth_quarter_event and
#     post_birth_window_share. Leaving the notes in would send a reader to
#     do work that has been done.
#
#  J5 T9 USES THE 0/1 INDICATOR. Its candidate list resolved to
#     full_time_student, which is a raw 1/2 questionnaire code, and the
#     == 1 test it applied happens to be right for that coding. The loader
#     now ships full_time_student_yes, so the design no longer depends on
#     a coding coincidence. enrolled_in_course is added to an explicit
#     DO-NOT-USE list with the reason: it is put only to new-person
#     questionnaire respondents, so it is observed for about 4 per cent of
#     person-waves in every wave and a rate computed on it is a statement
#     about entrants. It was never in T9's candidate list, which is luck
#     rather than design, and the list now says so.
#
#  J6 THE WAVE-24 SPAN IS DECLARED. Wave 24 carries a sample refresher, so
#     the guard stops a study series running through it. T9 is an event
#     study with person and wave fixed effects, which absorb a common
#     composition shift, so it declares the span rather than dropping the
#     wave, and the declaration is on the log beside the estimate.
#
#  J7 THE MODULE FALLBACK IS GUARDED. T7's fallback reads
#     self_or_partner_pregnant or currently_pregnant, which exist at waves
#     15, 19 and 23 only. The block already prints a warning to treat any
#     profile from them as uninterpretable; the guard now says the same
#     thing in a form that cannot be read past.
#
# ---------------------------------------------------------------------
# REVISION 3. Corrections to revision 2, all of them found by reading
# revision 2's own log against its own code. Tagged F1 to F12 and cited
# inline. The (Rn) tags from revision 2 are retained where they were
# correct; the numbers that had been reused for two or three unrelated
# changes are separated here into R21 to R28 so that a tag identifies one
# change.
#
#  F1 THE T8a DIVERGENCE GATE NOW TESTS THE ARM THAT CARRIES THE VERDICT.
#     Revision 2 computed the gate on sd(FTE - calendar) / sd(calendar)
#     for the TWO-SIDED stock, passed it at 0.108, and then issued the
#     verdict on the ONE-SIDED stock, whose ratio is 0.057 for men and
#     therefore below the pre-set 0.10. The gate is now computed per arm
#     and each contrast is gated on its own non-baseline arm, so an arm
#     cannot be read on another arm's divergence.
#
#  F2 THE ONE-SIDED STOCK IS NOW PAIRED WITH A GAP-CONSISTENT BASELINE.
#     Revision 2 said the one-sided stock differed from exp_hybrid_fte
#     "only in the ceiling". It did not. 02 builds exp_hybrid_fte from
#     exp_fte_int, which accumulates the weight over a FILLED person-by-
#     wave span grid with interior gaps linearly interpolated, while
#     exp_hybrid_cal accumulates coalesce(employed, 0) over the same grid
#     and so gives gap waves nothing. The stock built here accumulates
#     over observed panel rows, which makes it the analogue of 02's
#     exp_fte_obs, not of exp_fte_int. So the revision-2 calendar-against-
#     FTE contrast mixed the weight with the gap treatment, and the
#     one-sided arm mixed them in the opposite direction. How large the
#     confound is, is an empirical question rather than something to assume
#     from the fact that interior gaps might correlate with motherhood, and
#     it is measured by sex below and in 02's Section B rather than asserted
#     in either direction. Revision 3 reads every stock from the addendum,
#     which 02 revision 3 now builds in both gap treatments and all three
#     weights, and makes every reported contrast one in which both sides
#     share a gap treatment. The size of the confound is measured directly
#     as exp_hybrid_fte - exp_hybrid_fte_obs by sex.
#
#     ONE DEFINITION, NOT TWO. Revision 2 of this script built the
#     one-sided stock locally to avoid a rebuild of 02, which put a second
#     definition of the same object in a second file. 02 revision 3 ships
#     exp_hybrid_one_obs and exp_hybrid_one_int in the addendum and this
#     script prefers them. The local build is retained ONLY as a fallback
#     for an addendum written by 02 revision 2, and it announces itself
#     loudly when it fires, because a local build cannot be guaranteed to
#     match 02's accumulation grid.
#
#     NOTE THE NAME. 02 already uses exp_pt_obs for the count of part-time
#     CALENDAR YEARS, so the one-sided weighted stocks are called one and
#     never pt. Revision 3 of this script initially reused pt and would
#     have collided with that meaning the moment the addendum carried both.
#
#  F3 THE T8 ARMS ARE RUN ON ONE COMMON COMPLETE-CASE SAMPLE. Revision 2
#     asserted in prose that the between-arm contrasts were "computed on
#     identical rows" while run_dml applied complete.cases separately per
#     arm over that arm's own variable set. The common sample is now
#     constructed once, before any fit, and the within-couple demeaning is
#     applied AFTER that restriction so the couple means belong to the
#     estimation sample.
#
#  F4 T3's SATURATED TRIPLE DIFFERENCE REGAINS post_e. Under person_id^wave
#     the terms post and post_f are absorbed, but post_e = post x employer
#     varies within a person-wave and is not. Dropping it constrained
#     fathers' post-birth change in the employer-minus-own gap to zero, so
#     the "saturated" coefficient was a mothers-only difference in
#     differences and not the same parameter as the plain one. That is
#     also why its standard error fell under a stricter fixed-effect
#     structure. Both versions now estimate the same contrast.
#
#  F5 THE LEDGER CARRIES THE FIXED-EFFECT BLOCK. T2c told the reader to
#     consult an fe column that report() never wrote. It is now taken from
#     the fitted model rather than from a string, so it cannot disagree
#     with what was estimated.
#
#  F6 THE T8 REPRODUCTION DIAGNOSTIC USES THE PUBLISHED-UNITS ARM. Judging
#     the sample against the manuscript with the calendar-hybrid arm mixes
#     the sample difference with the treatment-construction difference.
#     The arm that shares the published treatment variable is the one whose
#     miss is diagnostic of the sample, and it is now the one used. The
#     his-against-hers asymmetry test also gains a middle band, on the same
#     ground as R17: at a ratio of 2.9 the two-band rule reported "both
#     arms miss by a similar amount" for a threefold asymmetry.
#
#  F7 NO MECHANISM IS NAMED THAT THE RUN HAS NOT ESTABLISHED. T2c already
#     refused to attribute its coefficient move to a sex-specific drift
#     sitting 1.2 SE from zero, then printed "which is the sign the placebo
#     drift predicts" immediately afterwards, and the T2 comment block
#     stated the direction of bias as settled. Both are now conditional on
#     the drift test. In T8 the partner-link conclusion is conditional on
#     its own premise, that a published couple-wave contributes two rows,
#     rather than being called confirmed.
#
#  F8 THE GAP STANDARD ERROR CARRIES ITS ASSUMPTIONS. his and hers are
#     estimated on disjoint sexes but a man and his partner are the same
#     couple, so sqrt(se_his^2 + se_her^2) treats correlated estimates as
#     independent. DoubleMLData is also built without a cluster column, so
#     neither arm's own standard error accounts for repeated person-waves.
#     Both are stated where the p-value is printed instead of being left
#     for the reader to notice.
#
#  F9 gap_per_sd IS REPORTED ONLY WHEN THE RESCALING MOVES. When sd(D) is
#     near-identical across arms the column multiplies every gap by the
#     same constant and looks like a second piece of evidence.
#
# F10 COUNTS AND RATES SHARE A DENOMINATOR. T7's report profile printed
#     n = n() beside percentages taken with na.rm = TRUE, so the two could
#     not be multiplied. The observed count is now printed per rate.
#
# F11 married_w IS DROPPED FROM THE WITHIN CONTROL SET. Both partners are
#     married, so demeaning it by couple leaves a column of zeros.
#
# ---------------------------------------------------------------------
# REVISION 3.1. Faults in revision 3, found by reading its own gate table
# against its own contrast list. Tagged F13 to F17.
#
# F13 THE GATE WAS PER ARM AND THE CONFOUND IS PER PAIR. F1 fixed the arm
#     mismatch: the divergence gate is now computed for each treatment and
#     each contrast is gated on its own non-baseline arm. That is necessary
#     and not sufficient. Nothing checked whether the TWO SIDES of a
#     contrast share a gap treatment before the units verdict was allowed
#     to fire, and on this panel that omission is decisive rather than
#     hypothetical: the only arm clearing the pre-set 0.10 is fte_int, the
#     gap-interpolated one, so the only two contrasts that would have
#     printed a verdict were calendar against interpolated FTE, which the
#     script itself labels as not gap-consistent, and gap-blind against
#     interpolated FTE, which varies no weight at all. Both would have got
#     units language for a movement that is not a units change.
#
#     Every contrast now declares what it varies, and the verdict follows
#     from that rather than from the gate alone:
#       weight  the two sides share a gap treatment. A units verdict is
#               permitted, subject to the gate.
#       gap     the two sides share a weight. The movement is printed in
#               gap-treatment language and no units verdict is issued.
#       both    the two sides differ in weight and gap treatment at once.
#               Refused before the gate is consulted, because no gate can
#               make a two-way movement attributable.
#
# F14 THE NEGATIVE RESULT IS STATED RATHER THAN LEFT TO BE ASSEMBLED. Only
#     one_obs and fte_obs are gap-consistent with the calendar baseline, and
#     on this panel both fail the gate, so no gap-consistent contrast can
#     answer the units question at all. That is a design limit worth one
#     printed sentence, and the reason is two blocks above it: the weight
#     touches only the in-panel part of the stock and that part is about a
#     third of it. The script also SUGGESTED restricting to person-waves
#     with a high in-panel share, and now tests that suggestion instead of
#     leaving it as advice, because it either rescues T8 on a stated
#     subsample or closes the question.
#
# F15 THE GAP-CREDIT TABLE WAS ON THE WRONG ROWS. Its comment said it was
#     on the T8 rows; it was on every row of the addendum join, 133,911
#     against the stacked sample's 52,649. Three different gap-credit
#     figures were then in circulation, none of them on the estimation
#     sample. It is computed on the stacked rows and the sample is named.
#
# F16 A p95 THAT IS STRUCTURALLY ZERO IS NOT A DIAGNOSTIC. With a credit on
#     under five percent of rows the 95th percentile is zero by
#     construction, which repeats pct_any and says nothing else. It is now
#     taken among the rows that carry a credit.
#
# F17 "None of the 7 has landed" needed a noun.
#
# ---------------------------------------------------------------------
# REVISION 3.2. Faults in 3.1, found by reading the completed T8 log.
# Tagged F18 to F23.
#
# F18 TWO BLOCKERS, REPORTED AS ONE. A units verdict has two independent
#     preconditions and the script tested them in two places without ever
#     putting them together:
#       DIVERGENCE  the two treatments must be far enough apart for a swap
#                   to relocate the gap. Gated in T8a, per arm.
#       A GAP       the baseline arm's gender gap must be distinguishable
#                   from zero, because a swap can only shrink something
#                   that is there. Checked inside swap_verdict, per pair.
#     Revision 3.1 summarised the first and left the second implicit, so
#     the log said the units question fails for want of divergence when on
#     this panel it fails for want of a gap as well: not one of the ten
#     fitted arms has a gap distinguishable from zero, pooled or within
#     couple, with p from 0.12 to 0.72. The two point at different fixes,
#     the first at the FTE construction and the second at the sample, so
#     conflating them sends the reader to the wrong place. Both are now
#     aggregated and named in one block after the arms are fitted.
#
# F19 THE REMEDY TEST CHECKED ONE PRECONDITION AND PROMISED A RE-RUN. It
#     found a gap-consistent arm clearing the gate on the high in-panel
#     share subsample and wrote a decision note saying to re-run the arms
#     there. That re-run cannot produce a verdict: the baseline gap is not
#     distinguishable from zero on the FULL sample, and a third of the rows
#     can only widen the interval, so swap_verdict would refuse on its
#     no-baseline-gap branch. The remedy verdict now waits until the arms
#     are fitted and requires both preconditions.
#
#     The remedy table also reported only the ratio. Since the ratio is
#     sd_div / sd_cal and the subsample is selected on having little
#     pre-panel base, part of any improvement is the DENOMINATOR shrinking
#     rather than the treatments moving apart, and a shrinking sd_cal is
#     less treatment variation, which is precision. Both components and
#     their change against the full sample are now in the table.
#
# F20 THE sd(D) SPREAD CHECK POOLED TWO DESIGNS. It ran over every arm at
#     once, so the range went from the couple-demeaned arms near 4.0 to the
#     pooled arms near 9.2 and reported a 57% spread. That is the demeaning,
#     not the units. Within the pooled arms the spread is 1.9%, which is the
#     near-constant case the check exists to catch. It now runs per panel.
#
# F21 FOUR DECISION NOTES WHERE TWO WERE NEEDED. The gate block and the
#     final note block each wrote the same divergence sentence, and the
#     no-gap-consistent-contrast note said the question "cannot be answered
#     on this panel" while the remedy note said a subsample clears the gate.
#     Both were true and they read as a contradiction. The divergence note
#     is written once, and the full-sample wording is explicit.
#
# F22 THE REPRODUCTION BLOCK DELEGATED ITS OWN INSTRUCTION. It printed
#     absolute misses and then told the reader to note which direction each
#     moved the gap. The misses are opposite in sign, +0.156 and -0.054, and
#     that is exactly why the gap collapses from -0.260 to -0.050. It is
#     computed rather than delegated.
#
# F23 A STRAY SPACE BEFORE A COMMA, from cat's argument separator.
#
# ---------------------------------------------------------------------
# F12 HOUSEKEEPING THAT WAS CAUSING SILENT RISK: braces added round the
#     final BLIND message, which printed a sentence fragment when BLIND was
#     FALSE; nt_bal removed, computed and never used; the grid loop's d, the
#     drift block's dm and df and the couple demeaning function no longer
#     collide on names at top level; the arrivals sentence is conditional on
#     the arrivals table instead of printing regardless of it; T4 names which
#     full-time definition is in force, since the loader's esdtl rule, the
#     35-hour fallback here and FT_HOURS = 38 are three different things;
#     the empty-grid guard in T8's search no longer compares NA against 10.
#
# ---------------------------------------------------------------------
# REVISION 2. What changed, and why.
#
#  R1 THE CLOCK COMPARISON IS RELEASED FROM BLIND. T2 asks whether the
#     birth-wave coefficient is a twelve-month reference window
#     straddling the birth. The diagnostic IS the comparison of the two
#     clocks, so withholding both coefficients withholds the design
#     decision rather than protecting it. The decision rule is fixed in
#     the configuration below, before the numbers are seen, which is what
#     the blinding was there to guarantee. Nothing else is released.
#
#  R2 THE ESTIMAND IS RECONCILED WITH THE PUBLISHED NUMBER. The employed
#     -only raw drop at the birth wave is materially smaller than the
#     manuscript's headline. If the published figure is unconditional,
#     the difference is the employment margin, which the experience path
#     already measures, and a large part of the headline training result
#     is then not a training decision at all. T1 now computes both raw
#     drops and the difference, so the section can be labelled correctly.
#
#  R3 BASE RATES ARE REPORTED BY SEX. Women train considerably more than
#     men before the birth, so the manuscript's pre-birth base of about
#     24 percent is the pooled base. "A reduction of more than half"
#     against women's own base is a different and smaller number, and it
#     is recomputed here.
#
#  R4 THE PLACEBO IS NOT FLAT, SO THE PROFILE IS ALSO REPORTED NET OF IT.
#     Never-treated women's training rate drifts upward across waves.
#     Wave fixed effects absorb what is common to a wave, and the raw
#     event-time profile mixes event time with calendar time. A second
#     profile, each treated person-wave differenced against the
#     never-treated rate of the same sex in the same wave, is reported
#     alongside.
#
#  R5 T5 DROPS THE CONDITIONAL-ON-TRAINING DOSAGE REGRESSION. Its MDE is
#     of the order of twenty hours, so it says nothing, and a footnote
#     saying nothing is still a claim. The measurement contribution, that
#     recoding structural zeros restores intensity coverage from a third
#     of the employed to essentially all of them, is stated on its own.
#
#  R6 PREFERRED SPECIFICATIONS ARE MARKED. T3 saturated, T4 on the "any"
#     rule, T6 interacted. Each is marked in the ledger rather than left
#     for the reader to infer from a precision comparison.
#
#  R7 T7 IS REFITTED ON THE LIFE-EVENT ITEM. It failed on measurement,
#     not power: only 1.3 percent of treated women were flagged at
#     k = -1 and 4.5 percent at k = +1, which is higher after the birth
#     than before it and cannot be a pregnancy profile. The ft* fertility
#     items are module-wave items, so they are missing for most of the
#     panel, and the life-event family codes yes as 2, so a == 1 test
#     selects the noes. event_birth (leprg) is asked every wave, is
#     already in the panel, and is reported by both sexes.
#
#  R8 PANEL FINGERPRINT. The previous run of 03 read the panel from a
#     desktop copy while 02 read project_dir. The two agreed on the
#     number of treated persons, which is not evidence that they read the
#     same file. Both scripts now print an identical fingerprint block.
#
#  R9 THE ONE-SIDED STOCK AND THE DIVERGENCE GATE. See F1 and F2 for what
#     revision 3 changed about both.
# R10 A SEX-SPECIFIC CALENDAR PATH, ABSORBED BY wave^female.
# R11 STANDARD ERRORS READ FROM fixest's OWN COEFFICIENT TABLE.
# R12 THE ROW CEILING CLOSES THE T8 SAMPLE SEARCH.
# R13 THE T8 WATERFALL IS PRINTED IN BOTH ORDERS.
# R14 THE T8 COUPLE-WAVE SUPPLY TEST.
# R15 n_cw COUNTS COUPLE-WAVES, NOT PERSON-WAVES.
# R16 A DECISION NOTE MAY NOT REOPEN WHAT THE TEST ABOVE CLOSED.
# R17 THREE BANDS, NOT TWO, ON A CONTINUOUS RATIO.
# R18 THE BALANCED-PANEL TEST, WHICH THE SEX RATIO ONLY PROXIES FOR.
# R19 THE DRIFT IS FITTED AT PERSON-WAVE LEVEL WITH CLUSTERED ERRORS.
# R20 A CORRECTION MAY NOT CITE A MECHANISM THE RUN CANNOT ESTABLISH.
# R21 SHARES ARE TAKEN AGAINST AN INTERNALLY CONSISTENT DENOMINATOR.
# R22 EVERY T8 RESTRICTION IS COUNTED IN TURN.
# R23 THE LEAD ARM LOSES ATTRITERS, SO ITS SAMPLE IS SELECTED.
# R24 T6's CONCLUSION IS COMPUTED FROM THE NUMBERS, NOT ASSERTED.
# R25 WHICH SAMPLE DEFINITION LANDS ON THE PUBLISHED PAIR.
# R26 THE ONE-SIDED STOCK IS CHECKED AGAINST ITS UPPER BOUND.
# R27 T6's PRECISION COMPARISON IS DECOMPOSED.
# R28 THE T8 GRID IS RANKED ON THE AXIS THAT IS NOT CAPPED.
#
# Input : hilda_panel_data_W12_W24_slim.rds
#         fte_experience/panel_fte_addendum.rds   (written by 02)
# Output: training_designs/*.csv, training_log.txt,
#         BLIND_training_estimates.rds
# =====================================================================

# ---- contract -------------------------------------------------------
# Namespace protection and the shared helpers (mde, gap, POWER, ALPHA)
# moved to 00_contract.R. Four identical alias blocks were harmless; a
# fifth definition that DIFFERED would not have been, and mde() proved
# it by killing a run 1,300 lines from either copy.
# v24.2. THE GUARD TESTS EVERYTHING IT NEEDS, not one function.
#
# It used to be `if (!exists("td_frame", mode = "function"))`, and that
# killed a run. A session where td_frame was present but mde() was not -
# a restored .RData from before mde moved into the contract is the easy
# way to get there - skipped the source() and then died 300 lines later
# on `could not find function "mde"`, after Block A had already computed
# and printed its whole event-time path.
#
# One name cannot stand for a file. The guard now names every object this
# script takes from the contract, sources when ANY is missing, and stops
# if any is still missing afterwards - which catches a contract that has
# been edited to no longer provide one of them, as opposed to one that
# simply was not loaded.
.TD_NEEDS <- c("td_frame", "td_log_open", "td_log_close", "td_check_seals",
               "td_put", "td_get", "td_has", "mde", "gap")
.td_missing <- .TD_NEEDS[!vapply(.TD_NEEDS, exists, logical(1),
                                 mode = "function")]
if (length(.td_missing) || !exists("POWER") || !exists("ALPHA")) {
  if (!file.exists("00_contract.R"))
    stop("00_contract.R not found. It defines the dplyr aliases, mde(), ",
         "gap(), the handoff registry and td_frame(). Nothing below is ",
         "safe without it.", call. = FALSE)
  source("00_contract.R")
}
.td_still <- .TD_NEEDS[!vapply(.TD_NEEDS, exists, logical(1),
                               mode = "function")]
if (length(.td_still) || !exists("POWER") || !exists("ALPHA"))
  stop("00_contract.R was loaded but does not provide: ",
       paste(c(.td_still, setdiff(c("POWER", "ALPHA"),
                                  ls(envir = globalenv()))),
             collapse = ", "),
       ". A partially-loaded contract fails far from here.", call. = FALSE)
rm(.td_missing, .td_still)
# v25. LOGGING MOVED TO 03_logging.R, which each module sources OUTSIDE
# the .TD03_COMMON_DONE guard. Called from here it was skipped in a warm
# session, so the second module of a pass ran inside the first module's
# transcript: on the 25 August 14:17 run a complete 03a produced its 26
# ledger rows into a file called 03training_designs. Nothing errored,
# which is why it is worth a guard rather than a comment.
if (!exists(".t_owns_sink"))
  stop("03_common.R expects 03_logging.R to have run first. Source a ",
       "module (03a, 03b or 03c) rather than this file directly.",
       call. = FALSE)


suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(fixest)
})

# ---------------------------------------------------------------------
# PATHS. Deferred to the contract (R8 superseded).
# ---------------------------------------------------------------------
# v25. THIS BLOCK USED TO RE-DERIVE ITS OWN PATHS and that quietly undid
# the single-root migration. It read
#
#   if (!exists("project_dir"))
#     project_dir <- ".../Documents - Copy/Y2025/HILDA_project"
#   OUTDIR <- file.path(project_dir, "training_designs")
#
# and it runs AFTER 00_contract.R, so the contract's OUTDIR was assigned
# and then overwritten one line later. The 25 August 14:11 run shows it:
# the contract had already been loaded, and OUTDIR still came out as
# project_dir/training_designs. Every CSV went back to the old tree while
# the log recorded a successful run.
#
# NOTHING TRIPPED, and that is the part worth noting. OUTDIR and
# project_dir are the three back-compat aliases the contract deliberately
# does NOT seal, because modules still assign them during migration and
# sealing would stop every run before the migration finished. The cost of
# that decision is exactly this: an unsealed alias can be reassigned
# without a warning, which is why the aliases are marked for deletion
# rather than kept.
#
# "Identical resolution logic in 02 and 03" was the right instinct and
# the wrong mechanism: two identical copies invite a third that differs.
# One resolver, in the contract, is the same argument that put mde()
# there.
if (!exists("PROJ_ROOT"))
  stop("00_contract.R has not been loaded, or is the pre-v25 version ",
       "without PROJ_ROOT.\n  This module no longer resolves its own ",
       "paths; the contract owns them.\n  Source the patched ",
       "00_contract.R first.", call. = FALSE)

data_dir   <- if (exists("data_dir")) data_dir else DIR_DATA
OUTDIR     <- DIR_DESIGNS
PANEL_FILE <- "hilda_panel_data_W12_W24_slim.rds"

find_file <- function(fname, dirs) {
  cand <- unique(file.path(dirs, fname))
  hit  <- cand[file.exists(cand)]
  if (!length(hit)) {
    cat("\n*** PANEL NOT FOUND ***\n")
    cat("  file wanted:", fname, "\n  looked in:\n")
    for (d in unique(dirs))
      cat("   ", d, if (dir.exists(d)) "" else "[dir does not exist]", "\n")
    cat("  working dir:", getwd(), "\n")
    for (d in unique(dirs)[dir.exists(unique(dirs))]) {
      rds <- list.files(d, pattern = "\\.rds$")
      if (length(rds)) {
        cat("  .rds files in", d, ":\n"); print(utils::head(rds, 20))
      }
    }
    stop("Set HILDA_ROOT so the contract resolves PROJ_ROOT correctly, or run the loader so the slim panel exists under data/.",
         call. = FALSE)
  }
  if (length(hit) > 1) hit <- hit[which.max(file.mtime(hit))]
  hit[1]
}
PANEL <- find_file(PANEL_FILE, c(DIR_DATA, PROJ_ROOT, getwd()))

# ---------------------------------------------------------------------
# CONFIGURATION
# ---------------------------------------------------------------------
# v25. K_LO IS -8, MATCHING THE FIGURES. It was -5, so Table 3 and the study
# profile started three horizons later than the descriptive figures drawing
# the same quantity, and Section 6.1 had to quote a range over one window
# while pointing at a figure over another. The paper carried six event
# windows across its exhibits and this closes two of them.
#
# WHAT WIDENS WITH IT. The three profiles in 03a that filter on K_LO: the raw
# event-time rates, the wave-adjusted version, and the study-enrolment
# profile. Nothing estimated moves, because the event studies set their own
# windows; only the descriptive tables get longer.
#
# THE ADDED CELLS ARE THIN and that is the cost. Around 250 to 400
# person-waves per sex at k = -8 to -6 against 1,100 at k = -1, so the gap
# column swings more at the left edge than anywhere else in the table. That
# is worth showing rather than hiding: the pre-trend test in Section 6.1 is
# what adjudicates flatness, and a table that stops before the noisy cells
# makes the leads look steadier than the design can claim.
#
# NOT GUARDED WITH if (!exists(...)), AND THE FIRST ATTEMPT WAS. That guard is
# right for RUN_T8, which is a switch a caller sets deliberately before
# sourcing. It is wrong for a configuration constant, because a warm session
# already holds the previous value and the guard then pins it: the file said
# -8, the session said -5, and the table came out with fourteen rows while the
# source read seventeen. Editing a constant and having the edit not apply is
# the same failure as OUTDIR being reassigned behind the guards, and it is
# quieter, because nothing errors.
#
# The file's value always wins. An override is deliberate and differently
# named, so it cannot happen by leftover state.
K_LO <- if (exists("K_LO_OVERRIDE")) as.integer(K_LO_OVERRIDE) else -8L
K_HI <- if (exists("K_HI_OVERRIDE")) as.integer(K_HI_OVERRIDE) else  8L
cat(sprintf("  descriptive event window: k = %d to %d%s\n", K_LO, K_HI,
            if (exists("K_LO_OVERRIDE") || exists("K_HI_OVERRIDE"))
              "   (OVERRIDDEN by the caller)" else ""))
POWER   <- 0.80
ALPHA   <- 0.05
# v23. OVERRIDABLE, same treatment as RUN_T8 and for a sharper reason.
#
# WHAT THE BLIND IS FOR. This module is a DESIGN AUDIT, not an estimator.
# It exists to decide which of nine designs can carry the claim, and that
# decision has to be made without looking at the coefficients, or it is not
# a decision, it is a selection. So point estimates go to
# BLIND_training_estimates.rds and the log shows SE, MDE and a verdict.
#
# WHY IT NOW HAS TO BE OVERRIDABLE. MASTER_train.R sources this module as
# the paper's evidence spine and then wants to check its ledger against the
# numbers. With BLIND hard-set TRUE the master had nothing to check: the
# estimates were sealed in a file it never opened, so its ledger compared
# hardcoded values against nothing. That is how "Any training, SE 0.019"
# survived pointing at a design that did not exist.
#
# THE DEFAULT DOES NOT MOVE. A plain source() still blinds. The master sets
# FALSE explicitly and logs that it did, so unblinding is an EVENT in the
# run record rather than a silent default - which is the property that makes
# the blind worth anything once the same module both decides and reports.
if (!exists("BLIND")) BLIND <- TRUE   # withhold point estimates; SEs and MDEs always shown
MIN_N   <- 100L     # a cell below this is reported as a count, not a point

# The one exception to BLIND, and the rule it runs under, both fixed
# before any number is seen (R1). If the shifted clock is larger than the
# natural clock by more than this fraction, the twelve-month reference
# window is doing enough work that the interview month has to be
# extracted; if not, the concern is closed and Block H is unnecessary.
RELEASE_CLOCK   <- TRUE
SHIFT_MATERIAL  <- 0.15

# Manuscript quantities, printed as benchmarks only.
# The introduction once gave a third panel size, 116,729 person-waves across
# 18,203 individuals, which matched neither pair below. It now quotes the
# estimation sample, so that pair and the fingerprint line that reported it
# are gone.
MS_ROWS      <- 134131   # manuscript, Data section: the EXTRACT, pre-screen
MS_PERSONS   <- 20205    # manuscript, Data section
# v25. THE ESTIMATION-SAMPLE PAIR, added because the fingerprint now needs
# it. Section 5.6: the age-minus-experience screen "removes 4,587 of
# 134,131 person-waves, leaving 129,544 across 19,661 individuals". Both
# pairs are the manuscript's and neither is wrong; which one a build
# should match depends on whether the screen has been applied yet. The
# contract's td_frame() applies it on resolve, so this is the pair that
# matters here and MS_ROWS is now the pre-screen reference.
# v29, 14 October. NO SCREEN BY DEFAULT (00_contract.R v28), so the
# estimation sample is the whole extract and the screened pair belongs to
# the sensitivity run (TD_SCREEN=on). Both pairs are kept; the rule in force
# picks one.
MS_ROWS_SCREENED    <- 129544
MS_PERSONS_SCREENED <- 19661
MS_SCREEN_ON        <- isTRUE(get0("TD_SCREEN_ON", ifnotfound = FALSE))
MS_ROWS_EST     <- if (MS_SCREEN_ON) MS_ROWS_SCREENED    else MS_ROWS
MS_PERSONS_EST  <- if (MS_SCREEN_ON) MS_PERSONS_SCREENED else MS_PERSONS
# THE HEADLINE IS READ, NOT TYPED. A typed copy held -0.144 from an earlier
# draft, then -0.117 while the paper printed -0.112. The paper now prints the
# headline from the registry, through \pn{ledger.T1_attached.est} and the keys
# derived from it, so this reads the same key. It is the number the last
# MASTER run registered, not this run's estimate, so BLIND still holds. It is
# NA until a MASTER run has registered the ledger, and the two places that
# use it say so rather than print a number.
#
# The two bases need no copy either. base_pewt, the pooled employed rate, and
# base_f, women's own pre-birth base, are computed below from this run.
.ms_registered <- function(key, part = "MASTER_training_y__ledger.csv") {
  d <- if (exists("PN_DIR")) PN_DIR else
    file.path(if (exists("PROJ_ROOT")) PROJ_ROOT else getwd(), "output", "numbers")
  f <- file.path(d, part)
  r <- if (file.exists(f)) tryCatch(utils::read.csv(f, stringsAsFactors = FALSE),
                                    error = function(e) NULL)
  v <- if (is.data.frame(r) && all(c("key", "value") %in% names(r)))
    suppressWarnings(as.numeric(r$value[r$key == key]))
  if (length(v) == 1L && is.finite(v)) v else NA_real_
}
MS_HEADLINE <- .ms_registered("ledger.T1_attached.est")   # T1_attached, as the paper prints it

# T8, the DML swap. Published Panel A of the Mincer-gap table, pooled:
# his +1.193%, hers +1.453%, gap -0.260%, p = 0.016; within-couple gap
# -0.072%, p = 0.675. Sample: 84,292 stacked person-waves, 4,661
# dual-earner couples.
MS_DML_GAP_POOLED <- -0.260
MS_DML_HIS_POOLED <-  1.193
MS_DML_HER_POOLED <-  1.453
MS_DML_GAP_WITHIN <- -0.072
# v21.11: RETIRED AS TARGETS. The sample search that ranked candidate
# rules on distance from these two is gone. They are kept because the
# couple-wave supply test above still prints them as CONTEXT for why the
# supply matters, and because 116,729-against-116,600 is unsettled; read
# any comparison against them as a magnitude note, never as a
# reproduction, and a miss as a difference rather than a defect.
# v21.31: RETIRED AS TARGETS, kept as context. PAPER_ATTENTION.md
# section 0 retires the sample search that ranked candidate rules on
# distance from these. They remain only because the couple-wave supply
# test prints them to show what the panel CANNOT reach; read any
# comparison as a magnitude note and a miss as a difference, never as a
# defect.
MS_DML_OBS        <- 84292
MS_DML_COUPLES    <- 4661
# v25. OVERRIDABLE, on the same contract RUN_T8 has carried since v23 and
# for the same reason. These were BARE ASSIGNMENTS, so a caller could turn
# T8 off but could not make it cheaper: setting DML_REPS before sourcing
# did nothing, because this line overwrote it thirty lines before the
# block that reads it. The only way to shorten a run was to edit this
# file, which is how a temporary edit becomes a committed one.
#
# Defaults are unchanged. A plain source() still gets the published
# five folds, three repetitions, random forest and all five arms.
if (!exists("DML_FOLDS"))  DML_FOLDS  <- 5L      # as published
if (!exists("DML_REPS"))   DML_REPS   <- 3L      # as published
if (!exists("DML_FAST"))   DML_FAST   <- FALSE   # TRUE swaps RF for lasso
if (!exists("DML_WITHIN")) DML_WITHIN <- TRUE    # within-couple demeaned arms
# Which T8 experience arms to run. Each arm is two DML fits, his and hers, so
# trimming this is the fastest way to shorten the block. The arm names carry
# the gap treatment as well as the weight, because 02 does not hold the two
# separate and revision 2 read a contrast that mixed them (F2):
#   pub      the published experience variable, for the reproduction check
#   cal      calendar hybrid, gap waves credited nothing. The baseline.
#   one_obs  one-sided FTE hybrid, gap waves credited nothing. PRIMARY.
#   fte_obs  two-sided FTE hybrid, gap waves credited nothing. Secondary.
#   fte_int  two-sided FTE hybrid, interior gaps interpolated. This is what
#            revision 2 called "the FTE hybrid". Retained only so that
#            fte_int against fte_obs measures the gap treatment on its own,
#            with the weight held fixed.
#   one_int  one-sided, interpolated. Off by default; the pair one_obs and
#            fte_obs already answers the question.
# The names are one and not pt: 02 uses exp_pt_obs for part-time calendar
# years, which is a different object.
if (!exists("T8_ARMS"))
  T8_ARMS <- c("pub", "cal", "one_obs", "fte_obs", "fte_int")

# ANNOUNCE THE EFFECTIVE CONFIGURATION, because these five now inherit.
# v24.2's note on RUN_T8 applies verbatim: `if (!exists(x)) x <- default`
# is the right contract and it is silent, and silence is what makes it a
# trap. A DML_REPS left at 1 from an earlier iteration run would make a
# publication pass three times faster and nothing would say so.
cat("\n  DML configuration in force:\n")
for (.nm in c("DML_FOLDS", "DML_REPS", "DML_FAST", "DML_WITHIN")) {
  .v <- get(.nm)
  .d <- switch(.nm, DML_FOLDS = 5L, DML_REPS = 3L,
               DML_FAST = FALSE, DML_WITHIN = TRUE)
  cat(sprintf("    %-11s %-6s %s\n", .nm, format(.v),
              if (identical(.v, .d)) "" else "<- INHERITED, not the default"))
}
cat(sprintf("    %-11s %s%s\n", "T8_ARMS", paste(T8_ARMS, collapse = ", "),
            if (length(T8_ARMS) == 5L) "" else
              "   <- NOT the published five"))
if (!identical(DML_REPS, 3L) || !identical(DML_FOLDS, 5L) ||
    isTRUE(DML_FAST) || length(T8_ARMS) < 5L)
  cat("    THIS IS NOT A PUBLICATION PASS. The arms below are cheaper than\n",
      "    the published specification and their standard errors are not\n",
      "    comparable with Table 11.\n", sep = "")
rm(.nm, .v, .d)
# T8 is the only slow section: twelve cross-fitted random-forest fits, tens
# of minutes. Every other section runs in a couple of minutes. When the
# change being tested is anywhere in T1 to T9, set this FALSE and iterate
# without paying for the DML; set it back before the run that gets read.
# v23. `if (!exists(...))` rather than a bare assignment, so a caller can
# turn T8 off WITHOUT editing this file. The default is unchanged: a plain
# `source()` still gets TRUE.
#
# WHY IT MATTERS AND IT IS NOT HYPOTHETICAL. MASTER_train.R sources this
# module and then runs the training event study, the funding contrast, the
# wage event study and two further blocks. Because the bare assignment
# overwrote the master's request, the v21.39 master run reached T8, spent
# the rest of its life growing trees, and NONE of those five things ran.
# The log ends mid-T8a at "Progress: 85%". Nothing had failed.
#
# T8 feeds exactly one claim - no gender difference in the return to
# experience across ten arms and five treatment constructions - and nothing
# else downstream depends on it, which is what makes it the right thing to
# gate rather than reorder.
# v24.2. SAY WHEN A FLAG IS INHERITED RATHER THAN DEFAULTED.
#
# `if (!exists(x)) x <- default` is the right contract - a caller must be
# able to override - but it is silent, and silence is what makes it a trap.
# run_T3c_only.R sets RUN_T8 <- FALSE and TD_NO_WRITE <- TRUE and leaves
# them in the global environment. The next `source("03training_designs.R")`
# in the same session inherits both, so a run intended as the full
# twenty-four-hour pass skips T8 and finishes in eight seconds, AND writes
# none of its artefacts. Both flags are doing exactly what they were told;
# nothing errors; the only symptom is a duration that looks wrong.
#
# This is the fourth failure in this project traceable to an object
# surviving between runs in one workspace, so the flags now announce
# themselves. A run that skips a day of computation should have to say so.
.t_flag_note <- function(nm, default) {
  if (!exists(nm, envir = globalenv())) {
    assign(nm, default, envir = globalenv()); return(invisible(NULL))
  }
  v <- get(nm, envir = globalenv())
  if (!identical(v, default))
    cat(sprintf(paste0("  NOTE: %s = %s was INHERITED from this session, not\n",
                       "        defaulted (%s). If that is not intended, rm(%s)\n",
                       "        or start a fresh session.\n"),
                nm, format(v), format(default), nm))
  invisible(NULL)
}
.t_flag_note("RUN_T8", TRUE)
# Fixed before any number is seen: how much of the pooled gap has to
# disappear in FTE units before the gap is called a units artefact.
DML_SHRINK_MATERIAL <- 0.50

# The FTE weight parameters, which MUST match 02_fte_experience.R. They are
# repeated here because T8b builds a one-sided stock that the addendum does
# not carry (F2). If 02 changes them, change them here.
FT_HOURS <- 38          # Australian standard full-time week
FTE_CAP  <- 1.25        # as in 02

# The quantile of in-panel share above which the remedy test in T8a
# recomputes the divergence (F14). 0.67 keeps the top third, which is large
# enough to re-estimate on and restrictive enough that the weight reaches
# most of the stock if it ever does.
IP_KEEP_Q <- 0.67

# Minimum divergence between the two experience treatments, as a fraction
# of the calendar treatment's own standard deviation, below which the swap
# is not run (R9, F1). Fixed before the divergence was measured. The reasoning
# is in T8a: a swap can only relocate a gap to the extent the two
# treatments differ, so this is the MDE-style gate that T8 was missing.
DML_DIV_MIN <- 0.10

# Absorb a SEX-SPECIFIC calendar path, not just a common one (R10). The
# never-treated placebo in T2 drifts upward faster for women than for men,
# and person_id + wave cannot remove that: sex is time-invariant and so is
# already inside the person effect, and a wave effect only removes what is
# common to the wave. FE_SEX_WAVE adds wave^female to every preferred
# specification. Set it FALSE to reproduce the earlier, uncorrected runs.
FE_SEX_WAVE <- TRUE
# Fixed before the comparison was run: how much post_f has to move when the
# sex-by-wave term is added before the earlier specification is treated as
# having been confounded rather than merely imprecise.
FE_MOVE_MATERIAL <- 0.15
PRE_LO       <- -3L      # window used for the pre-birth base rates (R3)
PRE_HI       <- -1L

dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)
# v21.16a. CAPTURE THE MESSAGE STREAM, using the pattern MASTER_hh.R
# already uses and this file does not.
#
# sink() with no type= diverts OUTPUT only. Every compat guard that
# acknowledges a declaration does so with message(), and every guard that
# STOPS raises to stderr, so the v21.14g run of this script produced 87KB
# containing the ten-restriction manifest and not one "compat:" line,
# though T1 passes allow_structural_zero = TRUE and T2-T6 pass
# allow_pooled_regime = TRUE. A declaration absent from the archived log
# is not a record of anything.
#
# The master solves this with ONE connection and both sinks pointed at
# it, which is why its own log carries the guard messages and the stop
# text. Copied verbatim rather than reinvented: an earlier attempt here
# passed a PATH to sink(type = "message"), which needs an open connection,
# and the arrangement that worked lost split = TRUE.
# v23. DO NOT TOUCH A CALLER'S SINKS. The three lines this replaces were
#
#     while (sink.number() > 0) sink()
#     if (sink.number(type = "message") > 0) sink(type = "message")
#     ... then open training_log.txt and sink to it
#
# and under MASTER_train.R they DESTROYED the master's log. The master
# opens one connection with both sinks pointed at it; this module arrived,
# unwound the stack to zero, put its own sinks on, and on exit removed
# those too. Control returned to the master with NO sinks at all, so every
# line from PART 3 onward - substitution, the timing exhibit, the hours
# arms, the ledger reconciliation - was written nowhere the master's log
# could see. Reproduced: the master's log file contains PART 0 and PART 1
# and then stops, at the exact point this file was sourced.
#
# The unwinding was defensible when this module only ever ran standalone.
# It is not defensible now, and "stopped running" is what it looks like
# from the outside, because the visible record ends where the module began.
#
# TWO RULES, both of which the old code broke:
#
#   If a caller already has a sink, LEAVE IT. The caller's log is the
#   record of the run and this module is part of that run, not a separate
#   one. Writing a second file would split the record in half.
#
#   If it opens anything, it closes exactly that and nothing else. The
#   counts are captured here and asserted on exit, so an imbalance is a
#   stated failure rather than a caller wondering where its output went.
# The sink block that stood here now lives in 03_logging.R, sourced by
# each module before this file. It sets .t_sink_out_before,
# .t_sink_msg_before, .t_owns_sink and .t_log_con, which 03_finish.R
# still closes against; the guard above fails loudly if it did not run.

# The fixed-effect block for a person-level event-study specification.
# Written once so that the sex-by-wave decision cannot be made differently
# in different sections (R10).
# ---------------------------------------------------------------------
# THE ESTIMATION-STAGE GUARDS (J1)
#
# Found by pattern and accepted on contents, because the file is saved
# under more than one name and carries a version in it. Each candidate is
# probed in a throwaway environment first, so a file that matches the name
# and throws cannot take the run down and a decoy cannot be accepted
# because the guards happen to be loaded already.
# ---------------------------------------------------------------------
GUARD_PATTERNS <- c("^00[_]?compat.*\\.[Rr]$", "compat.*\\.[Rr]$")
GUARD_MUST_DEFINE <- c("compat_panel_check", "require_cols",
                       "assert_no_olf_level")
.guard_dirs <- unique(c(PROJ_ROOT, DIR_R, dirname(PANEL), getwd()))
.guard_dirs <- .guard_dirs[dir.exists(.guard_dirs)]
.guard_hits <- unique(unlist(lapply(.guard_dirs, function(d)
  unlist(lapply(GUARD_PATTERNS, function(p)
    list.files(d, pattern = p, full.names = TRUE))))))
.guard_hits <- unique(normalizePath(.guard_hits, winslash = "/",
                                    mustWork = FALSE))
if (length(.guard_hits) > 1L)
  .guard_hits <- .guard_hits[order(file.mtime(.guard_hits), decreasing = TRUE)]
# v24. THE PROBE PARSES. IT DOES NOT EXECUTE.
#
# THIS COST 22 HOURS. The old loop probed each candidate with
#
#     invisible(capture.output(sys.source(.g, envir = .probe)))
#
# to see whether it defined the guards. sys.source RUNS the file.
# GUARD_PATTERNS[2] is "compat.*\\.[Rr]$", which matches MASTER_compat.R -
# 264 KB, 6,047 lines, the retired specialisation master. It reads the panel
# at line 231 and runs feols from 685 through the couple DML, the causal
# forest and the Oster bounds. The probe executed all of it, inside
# capture.output, so nothing reached the log or the console. The run had not
# hung. It was silently running someone else's paper.
#
# capture.output made it undiagnosable. A probe that discards output cannot
# be watched, and a probe that executes cannot be trusted; together they
# produce a process that looks stopped and is not.
#
# PARSING ANSWERS THE QUESTION ASKED. "Does this file define these three
# functions" is a property of the source text. Scanning the parse tree for
# top-level assignments costs milliseconds, cannot run anything, and rejects
# MASTER_compat.R correctly because that file SOURCES the guards rather than
# defining them.
#
# Only the winner is sourced, and by then it is known to be a guard file.
.guard_defines <- function(path, need) {
  ex <- tryCatch(parse(path), error = function(e) NULL)
  if (is.null(ex)) return(FALSE)
  defined <- character(0)
  for (e in ex) {
    if (is.call(e) && length(e) >= 3L &&
        as.character(e[[1]])[1] %in% c("<-", "=", "<<-") &&
        (is.name(e[[2]]) || is.character(e[[2]])))
      defined <- c(defined, as.character(e[[2]]))
  }
  all(need %in% defined)
}
.guard_used <- NA_character_
for (.g in .guard_hits) {
  if (isTRUE(.guard_defines(.g, GUARD_MUST_DEFINE))) {
    cat("  guards: sourcing", basename(.g), "\n")
    source(.g); .guard_used <- .g; break
  }
  cat("  skipped", basename(.g),
      ": matched the name but does not define the guards (parsed, not run).\n")
}
rm(.guard_defines)
if (is.na(.guard_used)) {
  cat("\n*** GUARD FILE NOT FOUND ***\n")
  cat("  looked for a file matching", paste(GUARD_PATTERNS, collapse = " or "),
      "\n  defining", paste(GUARD_MUST_DEFINE, collapse = ", "), "\n  in:\n")
  for (d in .guard_dirs) cat("   ", d, "\n")
  stop("A compat guard file is required. It holds the restrictions this ",
       "panel cannot express, and estimating without it is the failure it ",
       "was written to prevent.", call. = FALSE)
}
cat("Guards      :", .guard_used, "\n")
cat("  The panel itself is checked where it is READ, a few hundred lines\n")
cat("  below. Sourcing here only puts the functions in scope.\n")

fe_block <- function(extra = NULL) {
  parts <- c("person_id", "wave")
  if (FE_SEX_WAVE) parts <- c("person_id", "wave^female")
  paste(c(parts, extra), collapse = " + ")
}

# v24. THE LOCAL mde() IS DELETED. It came from 00_contract.R, sealed, and
# redefining it here is what stopped the v24 run at td_check_seals() after
# every estimate had already been computed.
#
# THE SEAL WAS RIGHT TO FIRE. The two were not cosmetically different:
#
#     contract  ifelse(is.finite(se) & se > 0, (z) * se, NA_real_)
#     here      ifelse(is.na(se),  NA_real_,   (z) * se)
#
# so for se = 0 this file returned 0 and the contract returns NA; for
# se = Inf, Inf against NA; for a negative se, a negative MDE against NA. A
# minimum detectable effect of zero grades every benchmark OK, which is the
# direction that turns a power failure into a pass. The contract's version
# is the correct one and the only one now.
#
# POWER and ALPHA are still assigned above and that is fine: they are set to
# the same values the contract holds, so identical() is TRUE and the seal
# does not fire. Left in place because this file is also read as the
# statement of its own configuration.

# v25. THE THREE ACCUMULATORS MOVED TO 03_logging.R, which each module
# sources OUTSIDE the .TD03_COMMON_DONE guard. Initialised here they
# survived a warm session, because the guard skips this file: on the
# 25 August 14:31 run 03b reported "contributes 38 ledger rows" when its
# own designs produce 12, and the merged ledger labelled all 26 of 03a's
# rows 03b_fee_descriptives. The tags and totals were right and the
# attribution was not, which is the harder version of the error to
# notice.
#
# Same shape as the logging fault two hundred lines above and the same
# fix: per-module state belongs in the file that runs per module.
if (!exists(".ledger") || !exists(".blind") || !exists(".decide"))
  stop("03_common.R expects 03_logging.R to have initialised .ledger, ",
       ".blind and .decide. Source a module (03a, 03b or 03c) rather ",
       "than this file directly.", call. = FALSE)

# The drift diagnostics stay here. They are written by T2c in 03a alone
# and read nowhere else, so carrying 03a's values into a later module is
# harmless and losing them would remove context from its log.
.drift_z <- NA_real_; .drift_est <- NA_real_; .drift_se <- NA_real_
note <- function(txt) .decide[[length(.decide) + 1]] <<- txt

# benchmark   is 25% of the POOLED pre-birth base, kept for comparability
#             with earlier runs of this script.
# benchmark_f is 25% of WOMEN'S OWN pre-birth base. Every post_f term is a
#             mother-specific contrast, so this is the scale the claim in
#             the text is actually made on. Both are carried; neither
#             overrides the other, and the ledger shows the two verdicts
#             side by side so a design that clears one and not the other
#             is visible rather than resolved silently.
report <- function(tag, label, m, term, unit, benchmark = NA_real_,
                   benchmark_f = NA_real_, status = "report") {
  ct <- tryCatch(fixest::coeftable(m), error = function(e) NULL)
  if (is.null(ct) || !(term %in% rownames(ct))) {
    cat(sprintf("  [%-22s] term absent: %s\n", tag, term))
    return(invisible(NULL))
  }
  se <- unname(ct[term, "Std. Error"]); es <- unname(ct[term, "Estimate"])
  .blind[[tag]] <<- ct[term, , drop = FALSE]
  grade <- function(bm) if (is.na(bm)) "" else
    if (mde(se) <= bm) "OK" else
      if (mde(se) <= 2 * bm) "LIMITED" else "NOT POSSIBLE"
  verdict <- grade(benchmark); verdict_f <- grade(benchmark_f)
  # The fixed-effect block, taken from the fitted object rather than from a
  # string, so the ledger cannot disagree with what was estimated (F5). T2c
  # tells the reader to consult this column; revision 2 never wrote it.
  # Two accessors tried in order, because a logging or naming change in a
  # fixest release must not be able to stop an estimation run: fixef_vars is
  # the documented element, and the fixef formula is the fallback.
  fe_used <- tryCatch({
    v <- m$fixef_vars
    if (is.null(v) || !length(v)) {
      ff <- tryCatch(m$fml_all$fixef, error = function(e) NULL)
      if (!is.null(ff)) paste(deparse(ff[[length(ff)]]), collapse = "") else ""
    } else paste(v, collapse = " + ")
  }, error = function(e) NA_character_)
  if (!length(fe_used) || is.na(fe_used[1]) || !nzchar(fe_used[1]))
    fe_used <- NA_character_
  # v28. THE ROW CARRIES THE SCRIPT THAT PRODUCED IT, stamped here rather
  # than at close. 03_finish.R used to write module <- TD03_MODULE over
  # every row it found, which is right while the only contributors are the
  # three 03 modules and wrong the moment another script writes into the
  # same accumulator. 04financing_space.R does: its .rep() dispatches to
  # this function, its rows join .ledger with no owner, and on 6 September
  # a hand-sourced 03_finish.R stamped all thirty of them 03a_design_audit,
  # thirty rows that then persisted through the registry into
  # training_ledger.csv under the wrong module.
  #
  # TD03_LEDGER_MODULE, not TD03_MODULE. A caller that is not an 03 module
  # must be able to own its rows WITHOUT claiming to be the module that
  # opened the transcript and will close it; those are different facts and
  # conflating them is what made this possible. NA here means "the
  # finisher may stamp it", which preserves the old behaviour for anything
  # that does not set it.
  .ledger[[length(.ledger) + 1]] <<- data.frame(
    tag = tag, label = label, n = m$nobs, se = round(se, 5),
    mde = round(mde(se), 5),
    benchmark = benchmark, verdict = verdict,
    benchmark_f = benchmark_f, verdict_f = verdict_f,
    unit = unit, fe = fe_used, status = status,
    module = get0("TD03_LEDGER_MODULE", ifnotfound = NA_character_),
    stringsAsFactors = FALSE)
  mark  <- if (identical(status, "report")) "" else paste0("  <", status, ">")
  vshow <- if (nzchar(verdict)) paste0(" ", verdict) else ""
  vfsh  <- if (nzchar(verdict_f) && !identical(verdict_f, verdict))
    paste0(" [women's base: ", verdict_f, "]") else ""
  if (BLIND)
    cat(sprintf("  [%-22s] n=%-8s SE=%.4f MDE=%.4f %s%s%s%s\n", tag,
                format(m$nobs, big.mark = ","), se, mde(se), unit, vshow,
                vfsh, mark))
  else
    cat(sprintf("  [%-22s] n=%-8s est=%+.4f SE=%.4f MDE=%.4f %s%s%s%s\n", tag,
                format(m$nobs, big.mark = ","), es, se, mde(se), unit, vshow,
                vfsh, mark))
  invisible(se)
}

# =====================================================================
# report_manual()  -  the ledger's second front door
#
# v28. report() BEGINS WITH fixest::coeftable(m), SO IT IS TYPED TO ONE
# ESTIMATOR. Anything that does not return a fixest fit cannot enter the
# ledger at all, and 03c_experience_units.R is exactly that: its twenty
# arms are DoubleML::DoubleMLPLR objects, which coeftable() cannot read.
# The consequence is not a warning anywhere; report() is simply never
# called, and after thirteen hours the module contributes zero rows while
# the paper describes a ledger that "carries all of them with their
# status, which is what makes the distinction checkable rather than
# asserted". The twenty experience arms were the ones it did not carry.
#
# THE FIX IS AN ENTRY POINT, NOT AN ADAPTER. Writing a coeftable shim for
# DoubleML would buy one estimator and leave the next one outside. What
# the ledger actually needs from a design is four numbers and a label,
# and any estimator can supply those. report() keeps its place as the
# convenience path for fixest, where reading n and the standard error off
# the object is safer than retyping them; this is the path for everything
# else.
#
# THE BENCHMARK IS OPTIONAL AND SHOULD USUALLY BE NA HERE. A quarter of
# the pre-birth training base is meaningless for an arm whose outcome is
# the return to experience in percentage points, and a wrong benchmark
# grades a row against a quantity it does not measure. Six rows in the
# ledger already carry NA and read correctly as "MDE reported, no
# benchmark defined". Pass one only when the design has a stated
# comparator of its own.
#
# STATUS IS NOT OPTIONAL, for the reason Section 5's blinding paragraph
# gives: a post-hoc arm that reaches the ledger unmarked is
# indistinguishable from a pre-designated one, and the ledger is the
# record that makes that distinction checkable.
report_manual <- function(tag, label, n, se, unit,
                          benchmark = NA_real_, benchmark_f = NA_real_,
                          fe = NA_character_, status = "secondary",
                          quiet = FALSE) {
  if (!is.finite(se) || se <= 0) {
    cat(sprintf("  [skip] %s: standard error is %s, so no MDE is defined.\n",
                tag, format(se)))
    return(invisible(NULL))
  }
  .m <- mde(se)
  .vd <- function(b) if (!is.finite(b)) "" else
    if (.m <= b) "OK" else if (.m <= 2 * b) "LIMITED" else "NOT POSSIBLE"
  verdict   <- .vd(benchmark)
  verdict_f <- .vd(benchmark_f)
  .ledger[[length(.ledger) + 1]] <<- data.frame(
    tag = tag, label = label, n = n, se = round(se, 5),
    mde = round(.m, 5),
    benchmark = benchmark, verdict = verdict,
    benchmark_f = benchmark_f, verdict_f = verdict_f,
    unit = unit, fe = fe, status = status,
    module = get0("TD03_LEDGER_MODULE", ifnotfound = NA_character_),
    stringsAsFactors = FALSE)
  if (!quiet) {
    mark  <- if (identical(status, "report")) "" else paste0("  <", status, ">")
    vshow <- if (nzchar(verdict)) paste0(" ", verdict) else ""
    cat(sprintf("  [%-22s] n=%-8s SE=%.4f MDE=%.4f %s%s%s\n", tag,
                format(n, big.mark = ","), se, .m, unit, vshow, mark))
  }
  invisible(.m)
}

cat("=====================================================================\n")
cat("TRAINING DESIGNS | revision 3 | blind:", BLIND, "\n")
cat("=====================================================================\n")

# v23. THE READ IS UNCONDITIONAL AND THAT IS THE BUG, not a style point.
#
# `panel <- readRDS(PANEL)` OVERWRITES whatever the caller had. Under
# MASTER_train.R the caller had a panel with
#
#     filter(between(age - experience_years, 10, 35))
#
# applied, 129,544 rows and 19,661 persons. This line replaced it with the
# file's 134,131 and 20,205, and the master's own log recorded both figures
# without anyone reading them together: "129,544 obs | 19,661 individuals"
# at the top, then "compat: panel 134,131 rows" a thousand lines later,
# which is this line's compat_panel_check firing on the re-read.
#
# TWO CONSEQUENCES, and the second is worse than the first.
#
#   T1 to T9 have always run UNFILTERED, including under the master. Any
#   statement that they share a sample with the master's other designs was
#   wrong.
#
#   AND `panel` STAYS OVERWRITTEN after this module returns. Everything the
#   master does afterwards - the beta2 profile, the hours arms, the interruption
#   table - silently changes sample mid-run, at a line eight hundred deep in a
#   sourced file. That is not a difference anyone would find by reading the
#   master.
#
# THE FIX IS A DECLARED FRAME, not a guard. `if (!exists("panel"))` would
# reintroduce the exact failure training_funding_divergence.R's header
# records: a stale in-memory panel shadowing a rebuilt one, symptom-free.
# options(td_frame=) makes the caller say so out loud, matching the
# tfd_frame convention already in use. No option set, the file is read, and
# a standalone run behaves exactly as before.
# v24. td_frame(), replacing the options(td_frame=) block this file carried.
#
# That block worked, but only for a caller in the SAME session that had
# already set the option. RUN_ORDER runs this script with Rscript, in its own
# process, so the option was never set and the else-branch always fired: the
# file, unfiltered, 134,131 rows, while the master ran on 129,544. The
# resolver in 00_contract.R applies the sample rule and persists the result,
# so whichever of the four scripts runs first fixes the frame and the rest
# inherit it across process boundaries.
#
# PANEL and TD_FRAME_FILES may name the same file. The difference is that
# td_frame() reports what it resolved and asserts the columns, so a narrower
# training file that silently lacks age or experience_years is caught here
# rather than as a wrong estimate later.
panel <- td_frame(
  need_cols = c("person_id", "wave", "female", "ever_parent", "employed"),
  # The context string reaches the log and the assertion message, so it
  # names a module that exists rather than the file this was extracted from.
  context = "03_common")

# The guards were sourced far above; the panel arrives here, so this is
# where it is checked. Revision 4 first put this call beside the source()
# and it failed on the first run against an object that did not exist
# yet: a check placed by where its inputs are DEFINED rather than by
# where they are AVAILABLE.
panel <- compat_panel_check(panel)
if (!is.null(attr(panel, "restrictions"))) {
  cat("\n--- Restrictions this panel carries ---\n")
  print(attr(panel, "restrictions"), row.names = FALSE, right = FALSE)
}
.eo <- attr(panel, "entrant_only_cols")
if (length(.eo))
  cat("Entrant-only columns measured by the loader:",
      paste(.eo, collapse = ", "), "\n")
rm(.eo)

# ---------------------------------------------------------------------
# PANEL FINGERPRINT (R8). Identical block in 02_fte_experience.R. Compare
# the two logs line for line rather than inferring agreement from one
# matching derived count.
# ---------------------------------------------------------------------
fingerprint <- function(path, p) {
  cat("\n--- Panel fingerprint ---\n")
  cat("  path        :", path, "\n")
  cat("  modified    :", format(file.mtime(path)), "\n")
  cat(sprintf("  size        : %.1f MB\n", file.size(path) / 1024^2))
  md5 <- tryCatch(unname(tools::md5sum(path)), error = function(e) NA_character_)
  cat("  md5         :", ifelse(is.na(md5), "not available", md5), "\n")
  cat(sprintf("  rows        : %s   (manuscript: extract %s, estimation %s)\n",
              format(nrow(p), big.mark = ","),
              format(MS_ROWS, big.mark = ","),
              format(MS_ROWS_EST, big.mark = ",")))
  cat(sprintf("  persons     : %s   (manuscript: extract %s, estimation %s)\n",
              format(dplyr::n_distinct(p$person_id), big.mark = ","),
              format(MS_PERSONS, big.mark = ","),
              format(MS_PERSONS_EST, big.mark = ",")))
  cat(sprintf("  columns     : %d\n", ncol(p)))
  cat(sprintf("  waves       : %d to %d\n", min(p$wave), max(p$wave)))
  # v25. COMPARE AGAINST THE ESTIMATION SAMPLE FIRST, because that is what
  # arrives here now. The contract's td_frame() applies the age-minus-
  # experience screen on the first resolve, so `panel` is 129,544 rows by
  # the time the fingerprint sees it. The check compared it against the
  # EXTRACT figures, 134,131 and 20,205, which are pre-screen, so a
  # perfectly correct build fell into the else branch and printed
  #
  #     NOTE: this build matches neither figure the manuscript gives. Do
  #           not quote any number from this run alongside a published one
  #
  # on the 25 August 14:11 run. That is a false positive on the strongest
  # warning in the file, and a warning that fires on a correct run is worse
  # than none: the next real mismatch is read as the same known noise.
  #
  # Both pairs are manuscript figures and neither is wrong. Section 4 gives
  # the extract at 134,131 and 20,205; Section 5.6 gives the estimation
  # sample at 129,544 and 19,661 after the screen removes 4,587. Which one
  # a build should match depends on WHERE THE SCREEN IS APPLIED, and since
  # the contract moved it upstream the answer changed.
  .fp_est     <- nrow(p) == MS_ROWS_EST &&
                 dplyr::n_distinct(p$person_id) == MS_PERSONS_EST
  .fp_extract <- nrow(p) == MS_ROWS &&
                 dplyr::n_distinct(p$person_id) == MS_PERSONS
  # v29. WHICH PAIR IS THE ESTIMATION SAMPLE DEPENDS ON THE RULE IN FORCE.
  # With no screen the estimation sample IS the extract, so .fp_est and
  # .fp_extract are both true and the first branch speaks. With the screen
  # on, the extract pair means the screen did not reach this frame. A
  # screened frame with the screen off means a frame cached before 14
  # October, which td_frame() should have refused.
  .fp_scr <- nrow(p) == MS_ROWS_SCREENED &&
             dplyr::n_distinct(p$person_id) == MS_PERSONS_SCREENED
  if (.fp_est && !MS_SCREEN_ON) {
    cat("  MATCH: the estimation sample, which is the whole extract. No\n")
    cat("  screen applies (TD_SCREEN is off), so every design below runs on\n")
    cat("  every person-wave.\n")
  } else if (.fp_est) {
    cat("  MATCH: the screened sample of the sensitivity run (TD_SCREEN=on).\n")
    cat("  The contract applied the age-minus-experience rule on resolve.\n")
  } else if (.fp_extract) {
    cat("  MATCH: the extract, UNSCREENED, although TD_SCREEN is on. The\n")
    cat("  designs below will run on the rows the screened run excludes.\n")
    cat("  Check that td_frame() resolved rather than a bare readRDS, and\n")
    cat("  that handoff/frame.rds is not a stale unscreened build.\n")
  } else if (.fp_scr) {
    cat("  *** SCREENED, although TD_SCREEN is off. This frame has the\n")
    cat("  screened run's rows, so it was cached before 14 October. Delete\n")
    cat("  handoff/frame.rds and handoff/ev.rds, or run td_forget(), and\n")
    cat("  start again.\n")
  } else {
    cat("  NOTE: this build matches neither manuscript figure. Do not quote\n")
    cat("        any number from this run alongside a published one until\n")
    cat("        the sample construction has been reconciled.\n")
    cat(sprintf("        Off the estimation sample by %+d rows, %+d persons.\n",
                nrow(p) - MS_ROWS_EST,
                dplyr::n_distinct(p$person_id) - MS_PERSONS_EST))
  }
  invisible(NULL)
}
fingerprint(PANEL, panel)


# ---------------------------------------------------------------------
# DISCOVERY ARRIVALS
#
# 01_discovery_extended.R answered its six questions against the 240c
# release. Several answers unlock designs below, and one closes a
# question negatively. This block reports which of the priority columns
# have actually landed in the panel, so a design that cannot run says so
# at the top rather than failing silently two hundred lines later.
# ---------------------------------------------------------------------
arrivals_check <- function(p, spec) {
  out <- do.call(rbind, lapply(seq_len(nrow(spec)), function(i) {
    cands <- trimws(strsplit(spec$candidates[i], "|", fixed = TRUE)[[1]])
    hit <- intersect(cands, names(p))
    closed <- !is.null(spec$closed) && isTRUE(spec$closed[i])
    data.frame(item = spec$item[i],
               status = if (length(hit)) "present" else
                 if (closed) "closed" else "not yet",
               column = if (length(hit)) hit[1] else "",
               unlocks = spec$unlocks[i], stringsAsFactors = FALSE)
  }))
  print(out, row.names = FALSE, right = FALSE)
  invisible(out)
}

.arrivals <- arrivals_check(panel, data.frame(
  item = c("maternity-leave reason", "full-time student (edfts)",
           "who paid for the training", "training aim, general (jttrdsg)",
           "training aim, current job (jttrisc)", "birth life event (lebth)",
           "pregnancy quarter (leprgq*)"),
  candidates = c("on_maternity_leave|on_parental_leave|nlf_reason_maternity|nlreapm",
                 paste0("full_time_student_yes|studying_now|",
                        "currently_studying|enrolled_study|",
                        "full_time_student|edfts"),
                 "training_self_funded|jttrcst",
                 # training_aim_*, not train_aim_*. The loader builds
                 # training_aim_general from jttrdsg, "develop skills
                 # generally", and training_aim_current from jttrisc,
                 # "improve skills in the current job", and has done since
                 # v17. This table reported both as "not yet" for a
                 # missing three letters, and T3 read that as a reason to
                 # stay a proxy contrast.
                 "training_aim_general|train_aim_general|jttrdsg",
                 "training_aim_current|train_aim_specific|jttrisc",
                 # BIRTH ONLY. Revision 4 first widened this to the whole
                 # event family and the table then reported the birth item
                 # as present "as event_pregnancy", which is the leprg and
                 # lebth conflation v17 removed, reappearing in a summary
                 # row. The pregnancy item has its own row in T7 and does
                 # not belong here.
                 paste0("event_child|had_child_this_wave|",
                        "event_birth_actual|lebth"),
                 "preg_quarter|leprgq1"),
  unlocks = c("third estimand in T1, employed or on leave",
              paste0("T9, whether the training fall is substitution into ",
                     "study. NOTE this is the FT-student DV, not the ",
                     "enrolment item: edcqen is put to new entrants only"),
              "T3 measures who paid rather than who provided the time",
              "T3 becomes general against specific, not a proxy for it",
              "same",
              "dates the BIRTH; leprg dates the PREGNANCY, see T7",
              "dates the pregnancy inside the twelve-month window"),
  stringsAsFactors = FALSE))
# Conditional on the table, not printed regardless of it (F12). Revision 2
# printed the sentence unconditionally, so it would have contradicted the
# table the moment any of the columns arrived.
.arr_missing <- sum(.arrivals$status != "present")
if (.arr_missing == nrow(.arrivals)) {
  cat("None of the", nrow(.arrivals),
      "items has landed in the panel. All are present\n")
  cat("in all 13 waves of the release, so each is an additive loader change.\n")
  cat("Batch them into one rebuild.\n")
} else if (.arr_missing > 0) {
  cat(sprintf("%d of %d have landed; the rest are additive loader changes.\n",
              nrow(.arrivals) - .arr_missing, nrow(.arrivals)))
  cat("Designs keyed to a column marked 'not yet' report that and skip.\n")
} else {
  cat("All of them are now in the panel, so every design below runs on the\n")
  cat("column it was written for rather than on a fallback.\n")
}

need <- c("person_id", "wave", "female", "ever_parent", "employed",
          "jttpewt_any", "jttpeot_any")
miss <- setdiff(need, names(panel))
if (length(miss))
  stop("Required columns absent: ", paste(miss, collapse = ", "), call. = FALSE)

# ---------------------------------------------------------------------
# Event time, matching the audit, 02_cs_event_study.R and 02_fte_experience.R
# exactly: treatment is the wave the absorbing ever_parent indicator turns
# 0 -> 1, never-treated are the always-childless, left-censored parents
# dropped.
# ---------------------------------------------------------------------
# WAVE-VARYING IS CORRECT HERE, and this is the block a reader will check
# first after the group_ever_life change, so it says why. ever_parent is
# absorbing without being constant within person, and a cohort definition
# needs exactly that: birth_wave is the first wave it equals 1, which a
# lifetime column could not locate because it equals 1 everywhere. The
# summarise below collapses to one row per person before anything is
# grouped, so `ever` is already the lifetime status and no cross-sectional
# cut is taken on the raw column. The contamination that affects Tables 3
# and 5 cannot arise here.
coh <- panel %>%
  filter(!is.na(ever_parent)) %>%
  arrange(person_id, wave) %>%
  group_by(person_id) %>%
  summarise(ep_at_entry = ever_parent[which.min(wave)],
            ever = max(ever_parent, na.rm = TRUE),
            birth_wave = if (any(ever_parent == 1))
              min(wave[ever_parent == 1]) else NA_integer_,
            .groups = "drop") %>%
  filter(ep_at_entry == 0) %>%
  mutate(cohort = ifelse(ever == 0, 10000L, as.integer(birth_wave)))

# v23. The contract, asserted on `panel` before ev is built from it. T1 to
# T9 read the ambient `panel`, so under a master they are on the analysis
# sample by construction and this call passes. It is here for the case that
# breaks it: someone re-extracting this module, or pointing it at an RDS,
# and getting numbers that silently do not belong beside the funding
# contrast. The failure mode is quiet, so the check has to be loud.
if (exists("assert_analysis_sample"))
  assert_analysis_sample(panel, "03_common (T1-T9 frame)")

ev <- panel %>%
  inner_join(coh %>% select(person_id, cohort), by = "person_id") %>%
  mutate(treated = as.integer(cohort != 10000L),
         k       = ifelse(treated == 1, wave - cohort, NA_integer_),
         post    = ifelse(treated == 1, as.integer(k >= 0), 0L),
         post_f  = post * female)

cat(sprintf("Event sample: %s person-waves, %s persons (%s treated)\n",
            format(nrow(ev), big.mark = ","),
            format(dplyr::n_distinct(ev$person_id), big.mark = ","),
            format(dplyr::n_distinct(ev$person_id[ev$treated == 1]),
                   big.mark = ",")))

# ---------------------------------------------------------------------
# BASE RATES (R3)
#
# A common percentage-point benchmark would ask the own-time design to
# detect a near total collapse while asking the employer-funded design
# for a quarter of its base, so each outcome gets a benchmark scaled to
# its own rate. Separately from the benchmark question, the sexes differ
# enough that a proportional claim about mothers needs women's own base as
# its denominator, not the pooled rate. The paper computes both shares from
# the registry; the last block of this section prints them for the log.
# ---------------------------------------------------------------------
BM_PROP <- 0.25
base_pewt <- mean(ev$jttpewt_any[ev$employed == 1] == 1, na.rm = TRUE)
base_peot <- mean(ev$jttpeot_any[ev$employed == 1] == 1, na.rm = TRUE)
# v23. any_training gets a base rate because it now gets a DiD arm. It was
# already asserted through the training gate at T1 and used as a denominator
# in T5, so the column was trusted everywhere except where it was estimated.
base_any  <- if ("any_training" %in% names(ev))
  mean(ev$any_training[ev$employed == 1] == 1, na.rm = TRUE) else NA_real_
bm_pewt <- BM_PROP * base_pewt
bm_peot <- BM_PROP * base_peot
bm_any  <- BM_PROP * base_any
cat(sprintf("\nBase rates among the employed: employer-funded %.1f%%, own-time %.1f%%\n",
            100 * base_pewt, 100 * base_peot))
cat(sprintf("Benchmarks at %.0f%% proportional: %.3f and %.3f\n",
            100 * BM_PROP, bm_pewt, bm_peot))

pre_base <- ev %>%
  filter(treated == 1, employed == 1, k >= PRE_LO, k <= PRE_HI,
         !is.na(jttpewt_any)) %>%
  group_by(female) %>%
  summarise(n = n(), base_pewt = round(mean(jttpewt_any), 4),
            base_peot = round(mean(jttpeot_any, na.rm = TRUE), 4),
            .groups = "drop")
cat(sprintf("\nPre-birth training base, k = %d to %d, employed only:\n",
            PRE_LO, PRE_HI))
print(as.data.frame(pre_base), row.names = FALSE)
write.csv(pre_base, file.path(OUTDIR, "T0_pre_birth_base_rates.csv"),
          row.names = FALSE)

base_any_f <- if ("any_training" %in% names(ev)) {
  .b <- ev %>% dplyr::filter(treated == 1, employed == 1, female == 1,
                             k >= PRE_LO, k <= PRE_HI, !is.na(any_training))
  if (nrow(.b)) mean(.b$any_training) else NA_real_
} else NA_real_
bm_f_any <- BM_PROP * base_any_f
base_f <- pre_base$base_pewt[pre_base$female == 1]
base_m <- pre_base$base_pewt[pre_base$female == 0]
base_f_peot <- pre_base$base_peot[pre_base$female == 1]
# Benchmarks on women's own base, for the mother-specific contrasts.
bm_f_pewt <- if (length(base_f) == 1) BM_PROP * base_f else NA_real_
bm_f_peot <- if (length(base_f_peot) == 1) BM_PROP * base_f_peot else NA_real_
cat(sprintf("Benchmarks on women's own base: %.3f employer-funded, %.3f own-time\n",
            bm_f_pewt, bm_f_peot))

# THE PROPORTIONAL CLAIM IS NO LONGER A NOTE TO THE TEXT. The paper computes
# it from the registry, ledger.T1_attached.prop.pct against women's own base
# and prop_pooled.pct against the pooled employed rate, so it cannot drift
# from the headline. This prints the same two shares for the log.
if (length(base_f) == 1 && length(base_m) == 1) {
  cat(sprintf("\nWomen train at %.1f%% before the birth against men's %.1f%%, and the\n",
              100 * base_f, 100 * base_m))
  cat(sprintf("pooled employed rate is %.1f%%. Women's own base is the denominator for\n",
              100 * base_pewt))
  cat("a claim about mothers; the paper computes both shares from the registry.\n")
  if (is.finite(MS_HEADLINE)) {
    cat(sprintf("  the paper's headline %.3f against the pooled rate  : %.0f%% reduction\n",
                abs(MS_HEADLINE), 100 * abs(MS_HEADLINE) / base_pewt))
    cat(sprintf("  the paper's headline %.3f against women's own base: %.0f%% reduction\n",
                abs(MS_HEADLINE), 100 * abs(MS_HEADLINE) / base_f))
  } else cat("  (no headline registered yet, so the two shares are not printed)\n")
}

# ---------------------------------------------------------------------
# MODULE COVERAGE BY WAVE
#
# Everything below treats calendar time as something wave fixed effects
# absorb, and the wave-adjusted profile in T2 treats the never-treated
# rate in a wave as a valid control for that wave. Both fail silently if
# the training module is not asked in some waves, because an unasked wave
# arrives as a run of zeros or of missings rather than as an error. The
# panel spans W12 to W24 while some builds carry the training items only
# from W15, so this is checked rather than assumed.
# ---------------------------------------------------------------------
cat("\n--- Training module coverage by wave ---\n")
# v21.8 REPAIR. This block used pct_obs_pewt = 100 * mean(!is.na(
# jttpewt_any)), which is 100.0 in every wave BY CONSTRUCTION, because
# the loader builds the column with a coalesce and it is never NA. The
# guard below then had three arms of which one could never fire, and the
# one that could never fire was the one this block exists for. Six
# revisions of "every wave carries the training items" were produced by
# a metric that could not say otherwise.
#
# training_module_asked is the column that answers the question. It is
# built on NON-MISSINGNESS of the gate item rather than on a code, so it
# is 0 exactly where the module was not put.
.have_gate <- "training_module_asked" %in% names(ev)
if (!.have_gate)
  cat("  NOTE: training_module_asked is absent, so coverage falls back to\n",
      "  the rate arms alone. Rebuild on the v21.6 loader or later.\n", sep = "")
cov_wave <- ev %>%
  group_by(wave) %>%
  summarise(n = n(),
            n_emp = sum(employed == 1, na.rm = TRUE),
            # % of person-waves the module was actually put to. Replaces
            # the two pct_obs_* columns, which were 100 by construction.
            asked_all = if (.have_gate)
              round(100 * mean(training_module_asked == 1), 1) else NA_real_,
            asked_emp = if (.have_gate)
              round(100 * mean(training_module_asked[which(employed == 1)] == 1),
                    1) else NA_real_,
            gap_emp = if (.have_gate)
              sum(training_module_asked[which(employed == 1)] == 0) else NA_integer_,
            rate_pewt = round(mean(jttpewt_any[which(employed == 1)],
                                   na.rm = TRUE), 3),
            rate_peot = round(mean(jttpeot_any[which(employed == 1)],
                                   na.rm = TRUE), 3),
            .groups = "drop")
print(as.data.frame(cov_wave), row.names = FALSE)
write.csv(cov_wave, file.path(OUTDIR, "T0_module_coverage_by_wave.csv"),
          row.names = FALSE)
cat("asked_emp rounds to 100.0 while gap_emp is still non-zero, so read\n")
cat("the count. gap_emp is the part of the module gap that lands inside\n")
cat("an employed-only estimation frame.\n")

bad_wave <- cov_wave$wave[(!is.na(cov_wave$asked_all) &
                             cov_wave$asked_all < 50) |
                            !is.finite(cov_wave$rate_pewt) |
                            cov_wave$rate_pewt == 0]
if (length(bad_wave)) {
  cat("\nWAVES WITHOUT THE MODULE:", paste(bad_wave, collapse = ", "), "\n")
  cat("Restrict the panel to the waves that carry the training items before\n")
  cat("reading anything below. As it stands those waves enter the wave fixed\n")
  cat("effects and the never-treated placebo as structural zeros, which will\n")
  cat("manufacture a calendar trend and contaminate the event-time profile.\n")
  note(paste("Module coverage: waves", paste(bad_wave, collapse = ", "),
             "lack the training items. Restrict the panel before reporting."))
} else {
  cat("\nEvery wave carries the training items, so the never-treated series\n")
  cat("is a genuine calendar-time control and its drift is not a coverage\n")
  cat("artefact. Composition is the other candidate: 04_variable_stability.R\n")
  cat("shows the panel losing respondents to wave 22 and gaining nearly a\n")
  cat("thousand at wave 24, and attrition selects on attachment, which is\n")
  cat("correlated with training. One check discriminates, and it is printed\n")
  cat("in the drift block below: attrition selects both sexes, so a purely\n")
  cat("compositional drift moves the male and female series together, and\n")
  cat("the ratio of the two movements is reported there. It discriminates\n")
  cat("only at the extremes; in between it says so rather than choosing.\n")
  cat("wave^female absorbs the differential either way.\n")
}


# ---- handoff ---------------------------------------------------------
# Put ev here rather than only in the finisher, so a module that fails
# partway still leaves a usable frame for the next one. Cheap and
# idempotent; the finisher puts it again.
if (exists("ev")) td_put("ev", ev)
.TD03_COMMON_DONE <- TRUE
