# =============================================================================
# v-notes-trwin13, 19 October: trwin k = 1 to 3 forms (apply_notes_trwin13.R).
# paper_numbers_derived.R
#
# Numbers the text states in a form no results table holds: a share as a
# percentage, an estimate in percentage points, a ratio of two estimates, a
# bound, a dollar amount to the hundred, or a count in words. Each is declared
# once here as a formula over registered keys. pn_write_tex() reads this file
# and computes every number from the registry each time it writes the macro
# file, so a derived number cannot drift from the estimates it comes from.
#
# Keep this file in the project root, beside paper_numbers.R. Do not source it
# yourself; paper_numbers.R reads it.
#
# A key that transforms one registered number is that key plus a suffix:
#   .pct  a share as a percentage       .pp   a proportion as percentage points
#   .d4   four decimals                 .dollars  thousands of dollars, to the $100
#   .words  in words, checked against the value
# A key that combines several numbers is named for what it measures.
# A phrase in words = "..." must suit the value (check_words() in
# paper_numbers.R holds the ranges), or pn_write_tex() stops.
# =============================================================================

# pn_write_tex() and pn_check() clear the declarations before they read this
# file. A source() by hand does not, so a session that has already read the
# file stopped at its first key as "declared twice". The line below clears
# them here too. Sourcing the file still writes nothing; pn_write_tex() does.
if (exists(".PN") && is.environment(.PN)) .PN$derive <- list()

# ---- Section 7: levels and proportions, employed frame (R1/R4 table) ----------

pn_derive("R1R4.all.kge0.E1.paid_time_workplace_the_paper_s_measure.prop.pct",
          ~ -100 * R1R4.all.kge0.E1.paid_time_workplace_the_paper_s_measure.prop, digits = 0,
          note = "employer-financed fall on the employed frame, percent of women's pre-birth base")
pn_derive("R1R4.all.kge0.O1.own_time_workplace_the_paper_s_worker_margin.prop.pct",
          ~ -100 * R1R4.all.kge0.O1.own_time_workplace_the_paper_s_worker_margin.prop, digits = 0,
          note = "own-time workplace fall, percent of its pre-birth base")
pn_derive("R1R4.all.kge0.O2.own_time_any_place.prop.pct",
          ~ -100 * R1R4.all.kge0.O2.own_time_any_place.prop, digits = 0,
          note = "own-time fall at any place, percent of its pre-birth base")
pn_derive("R1R4.all.kge0.O2.own_time_any_place.est.pp",
          ~ -100 * R1R4.all.kge0.O2.own_time_any_place.est, digits = 1,
          note = "own-time fall at any place, percentage points")
pn_derive("R1R4.all.kge0.O2.own_time_any_place.se.pp",
          ~ 100 * R1R4.all.kge0.O2.own_time_any_place.se, digits = 1)
pn_derive("R1R4.all.kge0.E1_over_O1.words",
          ~ R1R4.all.kge0.E1.paid_time_workplace_the_paper_s_measure.est /
            R1R4.all.kge0.O1.own_time_workplace_the_paper_s_worker_margin.est,
          words = pn_int_words, note = "the factor between the two falls in levels")
pn_derive("R1R4.all.kge0.O1.equal_proportion_level",
          ~ R1R4.all.kge0.O1.own_time_workplace_the_paper_s_worker_margin.base *
            R1R4.all.kge0.E1.paid_time_workplace_the_paper_s_measure.prop, digits = 3,
          note = "own-time fall in levels if it matched the employer-financed fall in proportion")
pn_derive("R1R4.all.kge0.E1.paid_time_workplace_the_paper_s_measure.est.d4",
          ~ R1R4.all.kge0.E1.paid_time_workplace_the_paper_s_measure.est, digits = 4)

# Formal study (R4), quoted to four decimals with its 95 percent interval.
pn_derive("R4.all.kge0.STUDY.full_time_student.est.d4", ~ R4.all.kge0.STUDY.full_time_student.est, digits = 4)
pn_derive("R4.all.kge0.STUDY.full_time_student.se.d4", ~ R4.all.kge0.STUDY.full_time_student.se, digits = 4)
pn_derive("R4.all.kge0.STUDY.full_time_student.ci_lo.d4",
          ~ R4.all.kge0.STUDY.full_time_student.est - qnorm(0.975) * R4.all.kge0.STUDY.full_time_student.se,
          digits = 4)
pn_derive("R4.all.kge0.STUDY.full_time_student.ci_hi.d4",
          ~ R4.all.kge0.STUDY.full_time_student.est + qnorm(0.975) * R4.all.kge0.STUDY.full_time_student.se,
          digits = 4)

# ---- Section 8: hours -----------------------------------------------------------

pn_derive("R1R4.preft_vs_all.kge0.E1.pct_larger",
          ~ 100 * (R1R4.preft.kge0.E1.paid_time_workplace_the_paper_s_measure.est /
                     R1R4.all.kge0.E1.paid_time_workplace_the_paper_s_measure.est - 1), digits = 0,
          note = "how much larger the fall is among women full time before the birth")
pn_derive("R1R4.preft.kge0.E1.paid_time_workplace_the_paper_s_measure.abs_t",
          ~ abs(R1R4.preft.kge0.E1.paid_time_workplace_the_paper_s_measure.est /
                  R1R4.preft.kge0.E1.paid_time_workplace_the_paper_s_measure.se), digits = 1)

# The fall in full-time waves as a share of the fall in all waves, from k = 2.
pn_derive("R14.ft_waves_share_of_fall",
          ~ R14.a_all_parents_kge2.E1.post_f.est / R1R4.all.kge2.E1.paid_time_workplace_the_paper_s_measure.est,
          digits = 3)
pn_derive("R14.ft_waves_share_of_fall.words", ~ R14.ft_waves_share_of_fall, words = "seven-tenths")
pn_derive("R14.hours_share_ft.pct", ~ 100 * (1 - R14.ft_waves_share_of_fall), digits = 0,
          note = "the part of the fall the full-time comparisons leave to hours")
pn_derive("R14.hours_share_ft.words", ~ 1 - R14.ft_waves_share_of_fall, words = "about a third")

# Hours accounting (R7): the share of the fall that hours account for.
pn_derive("R7single.transport.kge0.E1.share_explained.pct", ~ 100 * R7single.transport.kge0.E1.share_explained, digits = 0)
pn_derive("R7single.transport.kge2.E1.share_explained.pct", ~ 100 * R7single.transport.kge2.E1.share_explained, digits = 0)
pn_derive("R7single.g_estimation.kge0.E1.share_explained.pct", ~ 100 * R7single.g_estimation.kge0.E1.share_explained, digits = 0)
pn_derive("R7single.g_estimation.kge2.E1.share_explained.pct", ~ 100 * R7single.g_estimation.kge2.E1.share_explained, digits = 0)
pn_derive("R7single.share_explained.max.words",
          ~ max(R7single.transport.kge0.E1.share_explained, R7single.transport.kge2.E1.share_explained,
                R7single.g_estimation.kge0.E1.share_explained, R7single.g_estimation.kge2.E1.share_explained),
          words = "just over half", note = "the largest hours share across the two methods and windows")  # v-just-over-half, 16 October
# The first stage among women with no birth, against 35 to 44 hours, in points.
pn_derive("R7stage1.E1.women.1_15.pp", ~ -100 * R7stage1.E1.women.1_15, digits = 1)
pn_derive("R7stage1.E1.women.16_24.pp", ~ -100 * R7stage1.E1.women.16_24, digits = 1)
pn_derive("R7stage1.E1.women.25_34.pp", ~ -100 * R7stage1.E1.women.25_34, digits = 1)

# ---- Section 8: leave, part-time work, attachment ---------------------------------

pn_derive("R15mean.before.LEAVE_mothers.lo.words",
          ~ min(R15mean.m3.LEAVE_mothers, R15mean.m2.LEAVE_mothers, R15mean.m1.LEAVE_mothers),
          words = pn_int_words, note = "mothers' days of leave before the birth, lowest wave")
pn_derive("R15mean.before.LEAVE_mothers.hi.words",
          ~ max(R15mean.m3.LEAVE_mothers, R15mean.m2.LEAVE_mothers, R15mean.m1.LEAVE_mothers),
          words = pn_int_words, note = "mothers' days of leave before the birth, highest wave")
pn_derive("R15mean.0.LEAVE_fathers.words", ~ R15mean.0.LEAVE_fathers, words = pn_int_words)
pn_derive("R15mean.1.LEAVE_fathers.words", ~ R15mean.1.LEAVE_fathers, words = pn_int_words)

pn_derive("R13share.mothers_part_time_waves_after_the_birth.share_wanting_more_hours.pct",
          ~ 100 * R13share.mothers_part_time_waves_after_the_birth.share_wanting_more_hours, digits = 0)
pn_derive("R13share.never_mothers_part_time_waves.share_wanting_more_hours.pct",
          ~ 100 * R13share.never_mothers_part_time_waves.share_wanting_more_hours, digits = 0)

pn_derive("R1rec.ftret.E1.2pp4.5pp8.recovery_mde.pp", ~ 100 * R1rec.ftret.E1.2pp4.5pp8.recovery_mde, digits = 0,
          note = "the smallest recovery the test could detect, in points")

# The Introduction quotes three of the Section 8 falls in percentage points.
pn_derive("R14.a_all_parents_kge2.E1.post_f.est.pp", ~ -100 * R14.a_all_parents_kge2.E1.post_f.est, digits = 1)
pn_derive("R14.c_mothers_full_time_from_k_eq_1_against_all_fathers.E1.k2_plus_f.est.pp",
          ~ -100 * R14.c_mothers_full_time_from_k_eq_1_against_all_fathers.E1.k2_plus_f.est, digits = 1)
pn_derive("R11.c_mothers_saying_10_or_less_after_the_birth_kge2.E1.post_f.est.pp",
          ~ -100 * R11.c_mothers_saying_10_or_less_after_the_birth_kge2.E1.post_f.est, digits = 1)

# ---- Section 9: the wage path (R6, read from CS_event_study.R's results) ----------

pn_derive("R6.pre_specified.m8.abs_t", ~ abs(R6.pre_specified.m8.gap / R6.pre_specified.m8.gap_se), digits = 2)
pn_derive("R6.pre_specified.leads_m5_m2.bound",
          ~ ceiling(100 * max(abs(c(R6.pre_specified.m5.gap, R6.pre_specified.m4.gap,
                                    R6.pre_specified.m3.gap, R6.pre_specified.m2.gap)))) / 100, digits = 2,
          note = "the leads from five to two years before the birth lie within this distance of zero")
pn_derive("R6.pre_specified.post_0_4.min",
          ~ min(R6.pre_specified.0.gap, R6.pre_specified.1.gap, R6.pre_specified.2.gap,
                R6.pre_specified.3.gap, R6.pre_specified.4.gap), digits = 3)
pn_derive("R6.pre_specified.post_0_4.max",
          ~ max(R6.pre_specified.0.gap, R6.pre_specified.1.gap, R6.pre_specified.2.gap,
                R6.pre_specified.3.gap, R6.pre_specified.4.gap), digits = 3)
pn_derive("R6.pre_specified.10.gap.d4", ~ R6.pre_specified.10.gap, digits = 4)
pn_derive("R6.pre_specified.10.gap.prop", ~ exp(R6.pre_specified.10.gap) - 1, digits = 3,
          note = "the ten-year log gap as a proportion")
pn_derive("R6.pre_specified.10.gap.pct", ~ -100 * (exp(R6.pre_specified.10.gap) - 1), digits = 0,
          note = "the ten-year wage gap in percent, quoted throughout")
pn_derive("R6.age_absorbed.10.gap.pct", ~ -100 * (exp(R6.age_absorbed.10.gap) - 1), digits = 0)
pn_derive("R6.pre_specified.se_ratio_10_0", ~ R6.pre_specified.10.gap_se / R6.pre_specified.0.gap_se, digits = 2,
          note = "the ten-year standard error over the birth-wave one")

# ---- Section 9: earnings (R5), thousands of dollars to the nearest $100 -----------

pn_derive("R5win.EARN_K_pre_specified_form.k0.dollars",
          ~ pn_round(-1000 * R5win.EARN_K_pre_specified_form.k0, -2), digits = 0, money = TRUE)
pn_derive("R5win.EARN_K_pre_specified_form.k0_se.dollars",
          ~ pn_round(1000 * R5win.EARN_K_pre_specified_form.k0_se, -2), digits = 0, money = TRUE)
pn_derive("R5win.EARN_K_pre_specified_form.late_minus_early.dollars",
          ~ pn_round(-1000 * R5win.EARN_K_pre_specified_form.late_minus_early, -2), digits = 0, money = TRUE)
pn_derive("R5win.EARN_K_pre_specified_form.late_minus_early_se.dollars",
          ~ pn_round(1000 * R5win.EARN_K_pre_specified_form.late_minus_early_se, -2), digits = 0, money = TRUE)

# ---- MASTER_training_y.R ------------------------------------------------------------
# Keys from MASTER_paper_numbers.R: ledger (the 03a and 03b designs, read in PART
# 6A), side (8.2b, which side moves), wptest (CS_event_study.R's wage-path tests),
# A5prof and A5tilt (8.3) and hoursarm (9.3). The employed-only difference-in-
# differences and its own-time twin are the R1R4 keys 05 registers, which
# reproduce T1_jttpewt_any_employed and T1_jttpeot_any_employed exactly.

# The headline, T1_attached: employed or on parental leave.
pn_derive("ledger.T1_attached.est.pp", ~ -100 * ledger.T1_attached.est, digits = 1)
pn_derive("ledger.T1_attached.mde.pp", ~ 100 * ledger.T1_attached.mde, digits = 1)
# The ledger's benchmarks are a quarter of each base, so the bases are four times them.
pn_derive("ledger.base_f", ~ 4 * ledger.T1_attached.benchmark_f, digits = 3,
          note = "women's own pre-birth base, employer-financed")
pn_derive("ledger.base_f.d4", ~ 4 * ledger.T1_attached.benchmark_f, digits = 4)
pn_derive("ledger.base_f.pct", ~ 400 * ledger.T1_attached.benchmark_f, digits = 1)
pn_derive("ledger.base_pooled.d4", ~ 4 * ledger.T1_attached.benchmark, digits = 4,
          note = "the pooled employed rate behind the ledger's first grading")
pn_derive("ledger.base_pooled.pct", ~ 400 * ledger.T1_attached.benchmark, digits = 1,
          note = "the pooled employed rate, in percent")
pn_derive("ledger.T1_attached.prop_pooled.pct", ~ -100 * ledger.T1_attached.est / (4 * ledger.T1_attached.benchmark),
          digits = 0, note = "the headline fall, percent of the pooled employed rate (Online Appendix E.1)")
pn_derive("ledger.T1_attached.prop.pct", ~ -100 * ledger.T1_attached.est / ledger.base_f, digits = 0,
          note = "the headline fall, percent of women's own pre-birth base")
# The employed frame (05's R1R4, the same estimates as T1_*_employed).
pn_derive("R1R4.all.kge0.E1.paid_time_workplace_the_paper_s_measure.est.pp",
          ~ -100 * R1R4.all.kge0.E1.paid_time_workplace_the_paper_s_measure.est, digits = 1)
pn_derive("R1R4.all.kge0.O1.own_time_workplace_the_paper_s_worker_margin.est.d4",
          ~ R1R4.all.kge0.O1.own_time_workplace_the_paper_s_worker_margin.est, digits = 4)
pn_derive("R1R4.all.kge0.O1.own_time_workplace_the_paper_s_worker_margin.est.pp",
          ~ -100 * R1R4.all.kge0.O1.own_time_workplace_the_paper_s_worker_margin.est, digits = 2)
pn_derive("R1R4.all.kge0.O1.own_time_workplace_the_paper_s_worker_margin.ci_lo",
          ~ R1R4.all.kge0.O1.own_time_workplace_the_paper_s_worker_margin.est -
            qnorm(0.975) * R1R4.all.kge0.O1.own_time_workplace_the_paper_s_worker_margin.se, digits = 3)
pn_derive("R1R4.all.kge0.O1.own_time_workplace_the_paper_s_worker_margin.ci_hi",
          ~ R1R4.all.kge0.O1.own_time_workplace_the_paper_s_worker_margin.est +
            qnorm(0.975) * R1R4.all.kge0.O1.own_time_workplace_the_paper_s_worker_margin.se, digits = 3)
pn_derive("R1R4.all.kge0.O1.own_time_workplace_the_paper_s_worker_margin.mde_share_of_base.words",
          ~ R1R4.all.kge0.O1.own_time_workplace_the_paper_s_worker_margin.mde /
            R1R4.all.kge0.O1.own_time_workplace_the_paper_s_worker_margin.base,
          words = "about a third", note = "the own-time MDE as a share of its pre-birth base")
# The financing contrast, the couple arm, anticipation and study, as the
# Introduction, Discussion and Conclusion quote them.
pn_derive("ledger.T3_ddd_saturated.est.pp", ~ -100 * ledger.T3_ddd_saturated.est, digits = 1)
pn_derive("ledger.T3_ddd_saturated.se.d4", ~ ledger.T3_ddd_saturated.se, digits = 4)
pn_derive("ledger.T3_ddd.est.pp", ~ -100 * ledger.T3_ddd.est, digits = 1)
pn_derive("ledger.T3_ddd.se.d4", ~ ledger.T3_ddd.se, digits = 4)
pn_derive("ledger.T6_couple_x_wave.est.pp", ~ -100 * ledger.T6_couple_x_wave.est, digits = 1)
pn_derive("ledger.T6_couple_x_wave.se.d4", ~ ledger.T6_couple_x_wave.se, digits = 4)
pn_derive("ledger.T6_couple_sep_sexwave.se.d4", ~ ledger.T6_couple_sep_sexwave.se, digits = 4)
pn_derive("ledger.T7_pregnancy_gap.est.d4", ~ ledger.T7_pregnancy_gap.est, digits = 4)
pn_derive("ledger.T7_pregnancy_gap.est.pp", ~ -100 * ledger.T7_pregnancy_gap.est, digits = 1)
pn_derive("ledger.T7_pregnancy_gap.mde.pp", ~ 100 * ledger.T7_pregnancy_gap.mde, digits = 1)
pn_derive("ledger.T9_study_vs_training.est.pp", ~ 100 * ledger.T9_study_vs_training.est, digits = 1)
pn_derive("ledger.T9_study_vs_training.se.pp", ~ 100 * ledger.T9_study_vs_training.se, digits = 1)
pn_derive("ledger.T9_owntime_vs_employer.est.d4", ~ ledger.T9_owntime_vs_employer.est, digits = 4)
pn_derive("ledger.T9_study_vs_employer.threshold",
          ~ R1R4.all.kge0.E1.paid_time_workplace_the_paper_s_measure.est +
            qnorm(0.975) * ledger.T9_study_vs_employer.se, digits = 3,
          note = "the study coefficient below which study against employer-financed loses significance")
# The reference clock and the wave effects.
pn_derive("ledger.T2_clock.ratio", ~ ledger.T2_clock_shifted.est / ledger.T2_clock_natural.est, digits = 3,
          note = "interview-anchored clock over the wave-anchored one")
pn_derive("ledger.T2_clock.ratio.d2", ~ ledger.T2_clock_shifted.est / ledger.T2_clock_natural.est, digits = 2)
pn_derive("ledger.T2c.pct_larger", ~ 100 * (ledger.T2c_fe_sex_wave.est / ledger.T2c_fe_common.est - 1), digits = 0,
          note = "how much larger the sex-by-wave estimate is than the common-wave one")
# Dosage: total training hours, quoted to two decimals.
pn_derive("ledger.T5_hours_uncond.est.d2", ~ ledger.T5_hours_uncond.est, digits = 2)
pn_derive("ledger.T5_hours_uncond.se.d2", ~ ledger.T5_hours_uncond.se, digits = 2)
pn_derive("ledger.T5_hours_uncond.p.d2", ~ ledger.T5_hours_uncond.p, digits = 2)
pn_derive("ledger.T5_hours_uncond.mde.d2", ~ ledger.T5_hours_uncond.mde, digits = 2)
pn_derive("ledger.T5_hours_uncond.mde.d1", ~ ledger.T5_hours_uncond.mde, digits = 1)
pn_derive("ledger.T5_hours_ontrained_WITHHELD.mde.d1", ~ ledger.T5_hours_ontrained_WITHHELD.mde, digits = 1)
pn_derive("ledger.T5_hours.withheld_over_uncond.words",
          ~ ledger.T5_hours_ontrained_WITHHELD.mde / ledger.T5_hours_uncond.mde, words = pn_int_words)
# The rows the ledger table marks with a dagger.
pn_derive("ledgersum.dagger.n.words", ~ ledgersum.dagger.n, words = pn_int_words)
# The two wage-compression interactions, whose MDEs the heterogeneity appendix gives together.
pn_derive("ledger.T12.mde_mean.d2", ~ (ledger.T12_pub_interaction.mde + ledger.T12_uni_interaction.mde) / 2,
          digits = 2, note = "the two interactions' MDEs, which the text gives as near this value")

# Which side moves (8.2b). The training path of Figure 2 is the gap row of
# employed_only training_paid_time; the workplace item is jttpewt_any.
pn_derive("side.employed_only.jttpewt_any.0.mothers.pct_of_base",
          ~ -100 * side.employed_only.jttpewt_any.0.mothers / ledger.base_f, digits = 0,
          note = "mothers' own birth-wave fall, percent of women's pre-birth base")
pn_derive("side.employed_only.training_paid_time.0.mothers_t.words",
          ~ side.employed_only.training_paid_time.0.mothers_t, words = pn_int_words)
pn_derive("side.employed_only.training_own_time.0.mothers_t.words",
          ~ side.employed_only.training_own_time.0.mothers_t, words = pn_int_words)
pn_derive("side.employed_only.jttpeot_any.0.mothers_t.d1", ~ side.employed_only.jttpeot_any.0.mothers_t, digits = 1)
pn_derive("side.employed_only.training_paid_time.9.mothers.d2", ~ side.employed_only.training_paid_time.9.mothers,
          digits = 2)
pn_derive("side.employed_only.training_paid_time.gap_0_8.max",
          ~ max(side.employed_only.training_paid_time.0.gap, side.employed_only.training_paid_time.1.gap,
                side.employed_only.training_paid_time.2.gap, side.employed_only.training_paid_time.3.gap,
                side.employed_only.training_paid_time.4.gap, side.employed_only.training_paid_time.5.gap,
                side.employed_only.training_paid_time.6.gap, side.employed_only.training_paid_time.7.gap,
                side.employed_only.training_paid_time.8.gap), digits = 2,
          note = "the training path from the birth wave to eight years after it lies between min and max")
pn_derive("side.employed_only.training_paid_time.gap_0_8.min",
          ~ min(side.employed_only.training_paid_time.0.gap, side.employed_only.training_paid_time.1.gap,
                side.employed_only.training_paid_time.2.gap, side.employed_only.training_paid_time.3.gap,
                side.employed_only.training_paid_time.4.gap, side.employed_only.training_paid_time.5.gap,
                side.employed_only.training_paid_time.6.gap, side.employed_only.training_paid_time.7.gap,
                side.employed_only.training_paid_time.8.gap), digits = 2)
# The precision rule of the event window: the standard error at k = +10 over
# the birth-wave one, for the difference and for each sex's arm.
pn_derive("side.employed_only.training_paid_time.gap_se_ratio_10_0",
          ~ side.employed_only.training_paid_time.10.gap_se / side.employed_only.training_paid_time.0.gap_se,
          digits = 2)
pn_derive("side.employed_only.training_paid_time.mothers_se_ratio_10_0",
          ~ side.employed_only.training_paid_time.10.mothers_se / side.employed_only.training_paid_time.0.mothers_se,
          digits = 2)
pn_derive("side.employed_only.training_paid_time.fathers_se_ratio_10_0",
          ~ side.employed_only.training_paid_time.10.fathers_se / side.employed_only.training_paid_time.0.fathers_se,
          digits = 2)

# Any training over the birth wave and the two years after it, as a share of
# each sex's own pre-birth base (6.1, beside studies that do not split
# training by who finances it). trwin's pct is signed and the text gives its
# size with .pct.abs, so .verb carries the direction. .size_lo and .size_hi
# are the 95 percent interval on the scale of that size: for a fall, the
# interval of the fall.
.pn_fall_rise <- function(v) if (v < 0) "falls" else "rises"
pn_derive("trwin.employed_only.women.any_training.0.2.verb",
          ~ trwin.employed_only.women.any_training.0.2.pct, words = .pn_fall_rise)
pn_derive("trwin.employed_only.women.any_training.0.2.size_lo",
          ~ ifelse(trwin.employed_only.women.any_training.0.2.pct < 0,
                   -trwin.employed_only.women.any_training.0.2.pct_hi,
                   trwin.employed_only.women.any_training.0.2.pct_lo), digits = 0)
pn_derive("trwin.employed_only.women.any_training.0.2.size_hi",
          ~ ifelse(trwin.employed_only.women.any_training.0.2.pct < 0,
                   -trwin.employed_only.women.any_training.0.2.pct_lo,
                   trwin.employed_only.women.any_training.0.2.pct_hi), digits = 0)
pn_derive("trwin.employed_only.men.any_training.0.2.verb",
          ~ trwin.employed_only.men.any_training.0.2.pct, words = .pn_fall_rise)
pn_derive("trwin.employed_only.men.any_training.0.2.size_lo",
          ~ ifelse(trwin.employed_only.men.any_training.0.2.pct < 0,
                   -trwin.employed_only.men.any_training.0.2.pct_hi,
                   trwin.employed_only.men.any_training.0.2.pct_lo), digits = 0)
pn_derive("trwin.employed_only.men.any_training.0.2.size_hi",
          ~ ifelse(trwin.employed_only.men.any_training.0.2.pct < 0,
                   -trwin.employed_only.men.any_training.0.2.pct_lo,
                   trwin.employed_only.men.any_training.0.2.pct_hi), digits = 0)
# The same on the unconditional frame, for a comparison study that keeps
# people who are not employed.
pn_derive("trwin.unconditional.women.any_training.0.2.verb",
          ~ trwin.unconditional.women.any_training.0.2.pct, words = .pn_fall_rise)
pn_derive("trwin.unconditional.women.any_training.0.2.size_lo",
          ~ ifelse(trwin.unconditional.women.any_training.0.2.pct < 0,
                   -trwin.unconditional.women.any_training.0.2.pct_hi,
                   trwin.unconditional.women.any_training.0.2.pct_lo), digits = 0)
pn_derive("trwin.unconditional.women.any_training.0.2.size_hi",
          ~ ifelse(trwin.unconditional.women.any_training.0.2.pct < 0,
                   -trwin.unconditional.women.any_training.0.2.pct_lo,
                   trwin.unconditional.women.any_training.0.2.pct_hi), digits = 0)
pn_derive("trwin.unconditional.men.any_training.0.2.verb",
          ~ trwin.unconditional.men.any_training.0.2.pct, words = .pn_fall_rise)
pn_derive("trwin.unconditional.men.any_training.0.2.size_lo",
          ~ ifelse(trwin.unconditional.men.any_training.0.2.pct < 0,
                   -trwin.unconditional.men.any_training.0.2.pct_hi,
                   trwin.unconditional.men.any_training.0.2.pct_lo), digits = 0)
pn_derive("trwin.unconditional.men.any_training.0.2.size_hi",
          ~ ifelse(trwin.unconditional.men.any_training.0.2.pct < 0,
                   -trwin.unconditional.men.any_training.0.2.pct_lo,
                   trwin.unconditional.men.any_training.0.2.pct_hi), digits = 0)

# v-trwin-13, 19 October. The same three forms over the first to third year
# after the birth, k = 1 to 3, the window of Ma, Nakab, Navajas-Ahumada and
# Vidart (2026), who leave out the birth year. training_event_study.R writes
# these rows once TR_WINDOWS carries c(1L, 3L); until MASTER reruns, the keys
# below are skipped with a message, as any derived key with missing inputs is.
pn_derive("trwin.employed_only.women.any_training.1.3.verb",
          ~ trwin.employed_only.women.any_training.1.3.pct, words = .pn_fall_rise)
pn_derive("trwin.employed_only.women.any_training.1.3.size_lo",
          ~ ifelse(trwin.employed_only.women.any_training.1.3.pct < 0,
                   -trwin.employed_only.women.any_training.1.3.pct_hi,
                   trwin.employed_only.women.any_training.1.3.pct_lo), digits = 0)
pn_derive("trwin.employed_only.women.any_training.1.3.size_hi",
          ~ ifelse(trwin.employed_only.women.any_training.1.3.pct < 0,
                   -trwin.employed_only.women.any_training.1.3.pct_lo,
                   trwin.employed_only.women.any_training.1.3.pct_hi), digits = 0)
pn_derive("trwin.employed_only.men.any_training.1.3.verb",
          ~ trwin.employed_only.men.any_training.1.3.pct, words = .pn_fall_rise)
pn_derive("trwin.employed_only.men.any_training.1.3.size_lo",
          ~ ifelse(trwin.employed_only.men.any_training.1.3.pct < 0,
                   -trwin.employed_only.men.any_training.1.3.pct_hi,
                   trwin.employed_only.men.any_training.1.3.pct_lo), digits = 0)
pn_derive("trwin.employed_only.men.any_training.1.3.size_hi",
          ~ ifelse(trwin.employed_only.men.any_training.1.3.pct < 0,
                   -trwin.employed_only.men.any_training.1.3.pct_lo,
                   trwin.employed_only.men.any_training.1.3.pct_hi), digits = 0)
pn_derive("trwin.unconditional.women.any_training.1.3.verb",
          ~ trwin.unconditional.women.any_training.1.3.pct, words = .pn_fall_rise)
pn_derive("trwin.unconditional.women.any_training.1.3.size_lo",
          ~ ifelse(trwin.unconditional.women.any_training.1.3.pct < 0,
                   -trwin.unconditional.women.any_training.1.3.pct_hi,
                   trwin.unconditional.women.any_training.1.3.pct_lo), digits = 0)
pn_derive("trwin.unconditional.women.any_training.1.3.size_hi",
          ~ ifelse(trwin.unconditional.women.any_training.1.3.pct < 0,
                   -trwin.unconditional.women.any_training.1.3.pct_lo,
                   trwin.unconditional.women.any_training.1.3.pct_hi), digits = 0)
pn_derive("trwin.unconditional.men.any_training.1.3.verb",
          ~ trwin.unconditional.men.any_training.1.3.pct, words = .pn_fall_rise)
pn_derive("trwin.unconditional.men.any_training.1.3.size_lo",
          ~ ifelse(trwin.unconditional.men.any_training.1.3.pct < 0,
                   -trwin.unconditional.men.any_training.1.3.pct_hi,
                   trwin.unconditional.men.any_training.1.3.pct_lo), digits = 0)
pn_derive("trwin.unconditional.men.any_training.1.3.size_hi",
          ~ ifelse(trwin.unconditional.men.any_training.1.3.pct < 0,
                   -trwin.unconditional.men.any_training.1.3.pct_lo,
                   trwin.unconditional.men.any_training.1.3.pct_hi), digits = 0)

# The wage-path tests (CS_event_study.R).
pn_derive("wptest.plain.lead_drift_per_year_line_through_k_eq_m1.estimate.d4",
          ~ wptest.plain.lead_drift_per_year_line_through_k_eq_m1.estimate, digits = 4)
pn_derive("wptest.plain.lead_drift_per_year_line_through_k_eq_m1.se.d4",
          ~ wptest.plain.lead_drift_per_year_line_through_k_eq_m1.se, digits = 4)
pn_derive("wptest.plain.net_0_4.min",
          ~ min(wptest.plain.gap_net_of_the_lead_drift_k0.estimate, wptest.plain.gap_net_of_the_lead_drift_k1.estimate,
                wptest.plain.gap_net_of_the_lead_drift_k2.estimate, wptest.plain.gap_net_of_the_lead_drift_k3.estimate,
                wptest.plain.gap_net_of_the_lead_drift_k4.estimate), digits = 3,
          note = "net of the lead drift, the birth wave and the four years after it lie between min and max")
pn_derive("wptest.plain.net_0_4.max",
          ~ max(wptest.plain.gap_net_of_the_lead_drift_k0.estimate, wptest.plain.gap_net_of_the_lead_drift_k1.estimate,
                wptest.plain.gap_net_of_the_lead_drift_k2.estimate, wptest.plain.gap_net_of_the_lead_drift_k3.estimate,
                wptest.plain.gap_net_of_the_lead_drift_k4.estimate), digits = 3)
pn_derive("wptest.plain.gap_net_of_the_lead_drift_k10.abs_t",
          ~ abs(wptest.plain.gap_net_of_the_lead_drift_k10.estimate / wptest.plain.gap_net_of_the_lead_drift_k10.se),
          digits = 2)
pn_derive("R6.pre_specified.8.abs_t", ~ abs(R6.pre_specified.8.gap / R6.pre_specified.8.gap_se), digits = 2)

# The cross-partial at common experience (8.3), in A5_crosspartial_profile.csv's units.
pn_derive("A5prof.t_gap.absmax",
          ~ max(abs(c(A5prof.5.t_gap, A5prof.10.t_gap, A5prof.15.t_gap, A5prof.20.t_gap,
                      A5prof.25.t_gap, A5prof.30.t_gap))), digits = 2)
pn_derive("A5prof.t_sex.min_le20",
          ~ min(A5prof.5.t_men, A5prof.10.t_men, A5prof.15.t_men, A5prof.20.t_men,
                A5prof.5.t_women, A5prof.10.t_women, A5prof.15.t_women, A5prof.20.t_women), digits = 1,
          note = "each sex's cross-partial against zero, through twenty years: the smallest |t|")
pn_derive("A5prof.t_sex.max_le20",
          ~ max(A5prof.5.t_men, A5prof.10.t_men, A5prof.15.t_men, A5prof.20.t_men,
                A5prof.5.t_women, A5prof.10.t_women, A5prof.15.t_women, A5prof.20.t_women), digits = 1)
pn_derive("A5prof.mde_gap.min",
          ~ min(A5prof.5.mde_gap, A5prof.10.mde_gap, A5prof.15.mde_gap, A5prof.20.mde_gap,
                A5prof.25.mde_gap, A5prof.30.mde_gap), digits = 3)
pn_derive("A5prof.mde_gap.max",
          ~ max(A5prof.5.mde_gap, A5prof.10.mde_gap, A5prof.15.mde_gap, A5prof.20.mde_gap,
                A5prof.25.mde_gap, A5prof.30.mde_gap), digits = 3)
pn_derive("A5prof.gap.absmin",
          ~ min(abs(c(A5prof.5.gap, A5prof.10.gap, A5prof.15.gap, A5prof.20.gap, A5prof.25.gap, A5prof.30.gap))),
          digits = 3)
pn_derive("A5prof.gap.absmax",
          ~ max(abs(c(A5prof.5.gap, A5prof.10.gap, A5prof.15.gap, A5prof.20.gap, A5prof.25.gap, A5prof.30.gap))),
          digits = 3)
pn_derive("A5tilt.women_minus_men.range_5_30", ~ 25 * A5tilt.women_minus_men.slope, digits = 3,
          note = "the change in the gap from five to thirty years")
pn_derive("A5tilt.women_minus_men.range_5_30_se", ~ 25 * A5tilt.women_minus_men.slope_se, digits = 3)

# The hours arms (9.3): how far adding hours as a control moves the estimate.
pn_derive("hoursarm.BC.attenuation.words",
          ~ 1 - (hoursarm.B.est + hoursarm.C.est) / (2 * hoursarm.A.est), words = "about a third",
          note = "arms B and C against the baseline A, on average")

# v-pdfread-code, 19 October: four-decimal SEs set beside the couple-robust SEs (6.1, 9.2, D.4).
pn_derive("ledger.T1_attached.se.d4", ~ ledger.T1_attached.se, digits = 4,
          note = "static DiD SE, employed or on leave, beside the couple-robust SE")
pn_derive("wptest.plain.late_window_minus_early_window.se.d4",
          ~ wptest.plain.late_window_minus_early_window.se, digits = 4,
          note = "wage-window contrast SE, beside the couple-robust SE")

# v-armleads-d3, 19 October: the arm lead tests of Section 7.3 to three decimals.
pn_derive("R1rec.all.E2.2pp4.5pp8.leads_p_women.d3", ~ R1rec.all.E2.2pp4.5pp8.leads_p_women, digits = 3,
          note = "mothers' leads against never-mothers, joint p, three decimals as 7.1 prints p")
pn_derive("R1rec.all.E2.2pp4.5pp8.leads_p_men.d3", ~ R1rec.all.E2.2pp4.5pp8.leads_p_men, digits = 3,
          note = "fathers' leads against childless men, joint p, three decimals")

# v-levels-hours, 19 October: whether each at-given-hours level contrast is significant, in words.
pn_derive("R14.a_all_parents_kge2.CL_E1_O2.post_f.sig.words", ~ abs(R14.a_all_parents_kge2.CL_E1_O2.post_f.est) / R14.a_all_parents_kge2.CL_E1_O2.post_f.se,
          words = function(t) if (t >= 1.96) "distinguishable from zero at the 5 percent level" else "not distinguishable from zero",
          note = "8.3: is the Table I.3-design level contrast significant?")
pn_derive("R2.ftnow.kge2.E1_O2.level.sig.words", ~ abs(R2.ftnow.kge2.E1_O2.level.est) / R2.ftnow.kge2.E1_O2.level.boot_se,
          words = function(t) if (t >= 1.96) "distinguishable from zero at the 5 percent level" else "not distinguishable from zero",
          note = "8.3: is the full-time-person-waves level contrast significant (bootstrap SE)?")
