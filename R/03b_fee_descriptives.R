# =====================================================================
# 03b_fee_descriptives.R
# ---------------------------------------------------------------------
# The fee margin and the aim battery, as measurement rather than as designs.
#
# Designs: T3 funding-source triple difference, T3b purpose of training, T3c the fee margin as a level
# Paper:   Section 4.3, the training module's measurement properties
# Source:  03training_designs.R lines 2146-2793, extracted verbatim.
#
# SEPARATE BECAUSE THE ESTIMAND IS DIFFERENT. T3c is explicit that the fee item is asked only of trainees, so its denominator moves with the outcome and a birth-wave comparison on it conditions on the thing being measured. These rows are descriptive by construction and the file says so once, at the top, rather than design by design.
#
# HOW THIS FILE RELATES TO THE OTHER TWO. 03training_designs.R was 5,884
# lines doing three jobs. 03_common.R holds the prelude all three need:
# the contract guard, the configuration, report()/note()/fe_block(), the
# panel, the event frame and the benchmarks. 03_finish.R holds the ledger
# merge and the close. This file holds only its own designs, so it can be
# read, edited and re-run without the other two.
#
# THE LEDGER IS MERGED, NOT OVERWRITTEN. Re-running this module alone
# refreshes its own rows through the registry and leaves the other
# modules' rows in place. 03_finish.R explains why that is not the
# default-clobber behaviour the single file had.
# =====================================================================

TD03_MODULE <- "03b_fee_descriptives.R"
# 03_logging.R is sourced UNCONDITIONALLY and BEFORE the guard, so the
# transcript is named for the module producing the output rather than
# for whichever module happened to trigger the prelude. In a warm
# session the guard below skips 03_common.R, and when the logging lived
# inside it that skip took the log name with it.
source("03_logging.R")
if (!isTRUE(get0(".TD03_COMMON_DONE", ifnotfound = FALSE))) {
  source("03_common.R")
  .TD03_COMMON_DONE <- TRUE
} else {
  cat("  03_common.R already loaded this session; reusing ev and the",
      "helpers.\n")
}

# =====================================================================
# T3. FUNDING-SOURCE TRIPLE DIFFERENCE
#
# Stack the two outcomes so that each person contributes two rows per
# wave. person_id^source fixed effects remove each person's propensity to
# train through each channel, so the identifying variation is the change
# in the WITHIN-PERSON GAP between employer-funded and own-time training,
# differenced against fathers.
#
# Interpretation rests on one assumption, stated rather than buried:
# own-time training proxies the worker's own investment choice and
# employer-funded training proxies the firm's. Neither is clean, and the
# module records who provided the time rather than who paid, so this is a
# test of who stopped and not of who bore the cost.
# =====================================================================
cat("\n--- T3. Funding-source triple difference ---\n")

# age IS CARRIED THROUGH. transmute() keeps only what it names, so the
# age-absorbed arm below would have found no `age` column and skipped itself
# with a message that reads like a missing panel column rather than a
# dropped one. It is identical across the two rows of a person-wave, so it
# cannot touch the within-person-wave contrast; it is here to absorb the
# sex-specific source-by-wave path separately by age.
long <- bind_rows(
  ev %>% transmute(person_id, wave, k, treated, female, employed, post,
                   age = if ("age" %in% names(ev)) age else NA_real_,
                   source = "employer", y = jttpewt_any),
  ev %>% transmute(person_id, wave, k, treated, female, employed, post,
                   age = if ("age" %in% names(ev)) age else NA_real_,
                   source = "own", y = jttpeot_any)) %>%
  filter(employed == 1, !is.na(y)) %>%
  mutate(employer = as.integer(source == "employer"),
         post_f   = post * female,
         post_e   = post * employer,
         post_f_e = post * female * employer)

cat(sprintf("Stacked cell: %s person-wave-sources, %s persons\n",
            format(nrow(long), big.mark = ","),
            format(dplyr::n_distinct(long$person_id), big.mark = ",")))

# The sex-by-wave path has to be absorbed here as well, and by source,
# because the two outcomes drift differently in calendar time (R10). With
# wave^source^female in the block, post_f_e is identified off event timing
# within a sex-by-wave-by-source cell.
fe_ddd <- if (FE_SEX_WAVE) "person_id^source + wave^source^female" else
  "person_id^source + wave^source"
m_ddd <- fixest::feols(stats::as.formula(paste0(
  "y ~ post + post_f + post_e + post_f_e | ", fe_ddd)),
  data = long, cluster = ~person_id,
  notes = FALSE, warn = FALSE)
s_plain <- report("T3_ddd", "mother x post x employer, sex-specific source x wave", m_ddd, "post_f_e",
                  "pp", bm_pewt, bm_f_pewt, status = "secondary")
report("T3_ddd_post_f", "mother x post, common across sources", m_ddd,
       "post_f", "pp", bm_peot, bm_f_peot, status = "secondary")

# A version that also drops any wave-specific shock common to a person.
#
# post_e IS RETAINED (F4). person_id^wave absorbs post and post_f, both of
# which are constant within a person-wave, and it also absorbs wave^female
# because female is time-invariant, so the sex-by-wave path is removed here
# without naming it. post_e = post x employer is NOT absorbed: it varies
# across the two source rows of a person-wave. Revision 2 dropped it, which
# constrained fathers' post-birth change in the employer-minus-own gap to
# zero and left post_f_e as a mothers-only difference in differences rather
# than the triple difference the plain version estimates. That, and not the
# fixed effects, is why its standard error came out smaller under a stricter
# design. With post_e back the two rows are the same parameter and the
# precision comparison below means what it says.
m_ddd2 <- fixest::feols(y ~ post_e + post_f_e | person_id^wave + source^wave +
                          person_id^source, data = long, cluster = ~person_id,
                        notes = FALSE, warn = FALSE)
s_sat <- report("T3_ddd_saturated", "person x wave absorbed, common source x wave", m_ddd2,
                "post_f_e", "pp", bm_pewt, bm_f_pewt, status = "PREFERRED")

# --- THE SEX-SPECIFIC SOURCE-BY-WAVE PATH, PUT BACK -------------------
#
# THE TWO MODELS ABOVE DISAGREE ABOUT WHAT HAS TO BE ABSORBED, and the
# comment on the plain one states the case against the saturated one. It
# says the sex-by-wave path "has to be absorbed here as well, and by
# source, because the two outcomes drift differently in calendar time", and
# uses wave^source^female. m_ddd2 then drops female from the block and
# carries source^wave, a path common to both sexes. So the model the paper
# prefers is LESS saturated in the dimension the other one argues is the
# identifying one, and the labels have it backwards.
#
# WHAT THE PLAIN ESTIMATE ACTUALLY IS. -0.0978 is, to four decimals, the
# difference of the two separate difference-in-differences: -0.1163 less
# -0.0185. Check 2 of verify_manuscript_numbers.R prints exactly that and
# calls the match expected. On a balanced two-outcome stack the person-by-
# wave effects cannot move post_f_e, so the two specifications differ only
# in common against sex-specific wave effects. That is an identification
# choice, and the script below chose between them on standard errors.
#
# THIS MODEL IS THE TEST. person_id^wave and person_id^source as in
# m_ddd2, but wave^source^female in place of source^wave. It nests both:
# if post_f_e moves back toward -0.098, the -0.063 was the common-wave
# restriction rather than the finer person-by-wave design. If it stays near
# -0.063, the restriction was not doing the work and the preferred estimate
# survives on its own terms.
#
# post_f_e IS STILL IDENTIFIED. Within a wave-source-sex cell female and
# source are fixed, so post_f_e moves with post alone, and births are
# staggered, so post varies across persons inside every such cell. The cell
# absorbs the sex-specific employer-minus-own gap in each wave, which is
# precisely the path in dispute.
m_ddd3 <- fixest::feols(y ~ post_e + post_f_e | person_id^wave +
                          wave^source^female + person_id^source,
                        data = long, cluster = ~person_id,
                        notes = FALSE, warn = FALSE)
s_sat3 <- report("T3_ddd_sat_sexwave",
                 "saturated, sex-specific source x wave restored", m_ddd3,
                 "post_f_e", "pp", bm_pewt, bm_f_pewt, status = "post-hoc")

# --- AND THE SAME ARM WITH AGE ABSORBED -------------------------------
#
# The triple difference has just moved from -0.063 to -0.098, so it is the
# estimate the funding asymmetry now rests on and the one worth testing. The
# objection is the same as on the headline: the comparison pool runs 20 to 55
# and nothing in the specification carries age, so a post-birth move could
# carry an age gradient the controls average over.
#
# THE STACK MAKES THIS CHEAPER THAN IT LOOKS. Each person-wave appears twice,
# once per funding source, and age is identical across the two rows. So an
# age absorption cannot touch the within-person-wave contrast that identifies
# post_f_e; what it removes is variation in the sex-specific source-by-wave
# path across ages. If post_f_e is unmoved, that is the expected result and
# is worth reporting as such rather than as a finding.
# --- THE CONTRAST IN PROPORTIONS, NOT IN LEVELS -----------------------
#
# THE OBJECTION. The two margins have very different bases: employer-financed
# runs at about 0.30 among employed mothers pre-birth and own-time at about
# 0.07, roughly four times apart. A level triple difference therefore returns a
# negative number whenever the two margins fall by the same PROPORTION, which
# is not the asymmetry the paper claims. Equal proportional declines are the
# null the argument needs to reject, and a level contrast cannot test it.
#
# TWO ANSWERS, BOTH REPORTED. The first is arithmetic and needs no fit: scale
# each margin's own difference-in-differences by its own pre-birth base and
# subtract. The second is a Poisson fit of the same triple difference, whose
# coefficient is a proportional contrast by construction because the link is
# the log.
#
# WHY POISSON RATHER THAN LOGIT. Logit with person-by-wave fixed effects is
# an incidental-parameters problem on a two-row cell. Poisson with fixed
# effects is consistent for the conditional mean under far weaker assumptions
# and is the standard choice for a binary outcome under heavy absorption.
#
# THE SEPARATION COST IS REAL AND IS REPORTED. fepois drops any fixed-effect
# cell whose outcome is all zero, and on a two-row person-wave cell with a
# rare binary outcome that is most of the data. The dropped count is printed
# beside the estimate: a Poisson contrast fitted on a fifth of the rows is
# not the same population as the level contrast, and a reader has to be told
# which they are looking at. If the retained sample is small the ARITHMETIC
# comparison above is the one to quote.
# THE DENOMINATOR IS WOMEN'S OWN PRE-BIRTH BASE, NOT THE POOLED RATE.
#
# The first version of this block used base_pewt and base_peot, which
# 03_common.R computes over BOTH sexes and ALL event times: 0.2232 and
# 0.0480. It printed employer -52.1 per cent and own-time -38.5. Those are
# not the quantities the argument is about. post_f is the mother-specific
# response, so its denominator has to be mothers' own pre-birth rate, which
# is what Check 3 uses and what the referee's 39 and 26 per cent are computed
# on: base_f and base_f_peot, from pre_base restricted to treated, employed,
# k in [PRE_LO, PRE_HI], female == 1.
#
# A mother-specific numerator over an everyone-and-every-horizon denominator
# is a ratio of two different populations, and it inflated both percentages
# by about a third while leaving their difference looking plausible.
.bp_e <- if (exists("base_f"))      base_f[1]      else NA_real_
.bp_o <- if (exists("base_f_peot")) base_f_peot[1] else NA_real_
if (is.finite(.bp_e) && is.finite(.bp_o) && .bp_o > 0) {
  # BOTH LEVELS COME OUT OF m_ddd, which is why the plain model is used here
  # and not m_ddd3. Own time is the omitted source, so post_f is its
  # mother-by-post effect and post_f + post_f_e is the employer margin's.
  # m_ddd3 absorbs person^wave, which takes post_f with it, so it can give
  # the contrast but not the two levels the proportions need.
  .ct <- tryCatch(fixest::coeftable(m_ddd), error = function(e) NULL)
  .lv_o <- if (!is.null(.ct) && "post_f" %in% rownames(.ct))
    unname(.ct["post_f", "Estimate"]) else NA_real_
  .lv_e <- if (!is.null(.ct) && all(c("post_f", "post_f_e") %in% rownames(.ct)))
    unname(.ct["post_f", "Estimate"] + .ct["post_f_e", "Estimate"]) else NA_real_
  if (is.na(.lv_e) || is.na(.lv_o)) {
    cat("  m_ddd does not carry post_f and post_f_e, so the proportional\n",
        "  comparison is skipped.\n", sep = "")
  } else {
    cat(sprintf(paste0("\n  proportional declines: employer %+.1f%% of its own",
                       " base (%.4f),\n    own-time %+.1f%% of its own base",
                       " (%.4f); difference %+.1f points.\n"),
                100 * .lv_e / .bp_e, .bp_e,
                100 * .lv_o / .bp_o, .bp_o,
                100 * (.lv_e / .bp_e - .lv_o / .bp_o)))
    cat(sprintf(paste0("    A decline on own-time equal in PROPORTION to the",
                       " employer margin's\n    would be %+.4f in levels. The",
                       " level triple difference is negative\n    whenever the",
                       " own-time fall is smaller than that.\n"),
                .bp_o * .lv_e / .bp_e))
  }
  rm(.lv_e, .lv_o)
  if (exists(".ct")) rm(.ct)
}
rm(.bp_e, .bp_o)
.pois_ok <- requireNamespace("fixest", quietly = TRUE) &&
  all(c("post_e", "post_f_e") %in% names(long))
if (.pois_ok) {
  m_ddd_p <- tryCatch(
    fixest::fepois(y ~ post_e + post_f_e | person_id^wave +
                     wave^source^female + person_id^source,
                   data = long, cluster = ~person_id,
                   notes = FALSE, warn = FALSE),
    error = function(e) { cat("  fepois failed: ", conditionMessage(e),
                              "\n", sep = ""); NULL })
  if (!is.null(m_ddd_p)) {
    report("T3_ddd_poisson", "proportional triple difference, Poisson",
           m_ddd_p, "post_f_e", "log pt", NA_real_, NA_real_,
           status = "post-hoc")
    # stats::nobs, NOT fixest::nobs, WHICH IS NOT A FUNCTION. The call
    # errored, tryCatch returned NA, the retained-fraction line printed "NA
    # of 112,618" and the separation guard below compared NA and never
    # fired. It needed to: the fit retained 11,012 rows, under a tenth of
    # the stack, which is exactly the case the guard exists to announce.
    .np <- tryCatch(as.integer(stats::nobs(m_ddd_p)),
                    error = function(e)
                      tryCatch(as.integer(m_ddd_p$nobs),
                               error = function(e2) NA_integer_))
    cat(sprintf(paste0("  Poisson triple difference on %s of %s stacked rows",
                       " (%.0f%% retained).\n"),
                format(.np, big.mark = ","), format(nrow(long), big.mark = ","),
                100 * .np / nrow(long)))
    cat("    The coefficient is a proportional contrast: the log difference\n")
    cat("    between the two margins' post-birth declines. Read it against\n")
    cat("    the arithmetic comparison above, NOT against the level estimate.\n")
    if (is.na(.np))
      cat("    *** the retained count could not be read, so the separation\n",
          "        guard did not run. Read n on the report line above.\n",
          sep = "")
    if (!is.na(.np) && .np < 0.5 * nrow(long))
      cat("    *** Separation dropped more than half the stack. The retained\n",
          "        cells are those with a positive outcome on at least one\n",
          "        margin, which is a selected population. Quote the\n",
          "        arithmetic comparison instead.\n", sep = "")
    rm(.np)
  }
}
rm(.pois_ok)

if ("age" %in% names(long) && !all(is.na(long$age))) {
  m_ddd4 <- fixest::feols(y ~ post_e + post_f_e | person_id^wave +
                            wave^source^female + person_id^source + age,
                          data = long, cluster = ~person_id,
                          notes = FALSE, warn = FALSE)
  report("T3_ddd_agefe", "sex-specific source x wave, age absorbed", m_ddd4,
         "post_f_e", "pp", bm_pewt, bm_f_pewt, status = "post-hoc")
  .e4 <- tryCatch(unname(fixest::coeftable(m_ddd4)["post_f_e", "Estimate"]),
                  error = function(e) NA_real_)
  .e3b <- tryCatch(unname(fixest::coeftable(m_ddd3)["post_f_e", "Estimate"]),
                   error = function(e) NA_real_)
  if (!is.na(.e4) && !is.na(.e3b))
    cat(sprintf("  post_f_e with age absorbed: %+.4f against %+.4f (%+.4f).\n",
                .e4, .e3b, .e4 - .e3b))
  rm(.e4, .e3b)
} else {
  cat("  no `age` column on the stack, so the age-absorbed arm is skipped.\n")
}
if (!is.null(s_sat) && !is.null(s_sat3) && !is.na(s_sat) && !is.na(s_sat3)) {
  .e2 <- tryCatch(unname(fixest::coeftable(m_ddd2)["post_f_e", "Estimate"]),
                  error = function(e) NA_real_)
  .e3 <- tryCatch(unname(fixest::coeftable(m_ddd3)["post_f_e", "Estimate"]),
                  error = function(e) NA_real_)
  cat(sprintf(paste0("  post_f_e: %+.4f with source^wave, %+.4f with",
                     " wave^source^female (%+.4f).\n"),
              .e2, .e3, .e3 - .e2))
  cat("  Read the move against the plain model's -0.0978. A shift toward it\n")
  cat("  says the preferred estimate rested on the common-wave restriction,\n")
  cat("  which is the assumption T2c rejects elsewhere on identification\n")
  cat("  grounds. Precision is not the ground to choose on.\n")
  rm(.e2, .e3)
}
if (!is.null(s_plain) && !is.null(s_sat) && !is.na(s_plain) && !is.na(s_sat)) {
  cat(sprintf("  precision of saturated against plain: SE %.4f against %.4f (%.2fx)\n",
              s_sat, s_plain, s_sat / s_plain))
  if (s_sat <= s_plain)
    note("T3: report the saturated triple difference; it is the tighter design and the more precise one.")
  else
    note(sprintf(paste("T3: the saturated triple difference costs %.2fx in",
                       "precision. It is the better-identified design; report",
                       "both and state the cost."), s_sat / s_plain))
}

# Event-time version of the triple difference, so persistence is visible
# rather than collapsed into a single post indicator.
lk <- long %>% filter(treated == 1, k >= K_LO, k <= K_HI) %>%
  mutate(kf = factor(k, levels = K_LO:K_HI))
cells <- lk %>% count(k, female, employer) %>%
  pivot_wider(names_from = c(female, employer), values_from = n,
              names_prefix = "cell_", values_fill = 0L)
cat("\nCells per event time (female_employer):\n")
print(as.data.frame(cells), row.names = FALSE)
thin <- cells$k[apply(cells[, -1, drop = FALSE], 1, min) < MIN_N]
if (length(thin))
  cat("Horizons below", MIN_N, "in some cell, do not report as points:",
      paste(thin, collapse = ", "), "\n") else
        cat("Every horizon clears", MIN_N, "in every cell.\n")

# =====================================================================
# =====================================================================
# T3b. THE AIM CONTRAST, WHICH IS THE DIRECT TEST T3 PROXIES FOR
#
# T3 above is a funding-source triple difference: employer-funded against
# own-time, used as a PROXY for general against specific human capital on
# the reasoning that an employer pays for what it can appropriate. That
# proxy was necessary while nothing measured generality directly.
#
# The v21.29 loader measures it. The E12 aim battery asks the trainee what
# the training was FOR, and the loader now builds the full set. So the
# same triple difference runs on a REPORTED aim rather than an inferred
# one, and the funding contrast becomes a robustness check on the aim
# rather than a stand-in for it.
#
# WHICH COLUMNS, AND WHY THE _z ONES. The conditional columns
# (promotion_training, maintenance_training) are observed only where the
# person trained: coverage is about 27%, and training incidence itself
# falls roughly twelve points for mothers at the birth wave. A regression
# on them CONDITIONS ON THE OUTCOME, so the k = 0 cell is a differently
# composed group from k = -2 before any aim is examined. That is the
# dosage-margin problem in a new place and the loader has already solved
# it the same way: the _z twins are NA where the module was never asked,
# 0 where it was asked and no training was reported, and the indicator
# where the battery was answered.
#
# Both are run. The conditional pair answers "among trainees, which KIND
# of training", which is a composition question and correctly conditions.
# The _z pair answers "how much promotion-oriented training", which is a
# level question and must not.
if (exists("ev") && all(c("promotion_training_z", "maintenance_training_z")
                        %in% names(ev))) {
  cat("\n--- T3b. Aim contrast: promotion against maintenance ---\n")
  
  .aim_pairs <- list(
    unconditional = c(promo = "promotion_training_z",
                      maint = "maintenance_training_z"),
    among_trainees = c(promo = "promotion_training",
                       maint = "maintenance_training"))
  
  for (.nm in names(.aim_pairs)) {
    .pr <- .aim_pairs[[.nm]][["promo"]]; .mn <- .aim_pairs[[.nm]][["maint"]]
    if (!all(c(.pr, .mn) %in% names(ev))) {
      cat(sprintf("  [skip] %s: %s not on the frame.\n", .nm,
                  paste(setdiff(c(.pr, .mn), names(ev)), collapse = ", ")))
      next
    }
    .long <- dplyr::bind_rows(
      ev %>% dplyr::transmute(person_id, wave, k, treated, female, employed,
                              post, aim = "promotion", y = .data[[.pr]]),
      ev %>% dplyr::transmute(person_id, wave, k, treated, female, employed,
                              post, aim = "maintenance", y = .data[[.mn]])) %>%
      dplyr::filter(employed == 1, !is.na(y)) %>%
      dplyr::mutate(promo    = as.integer(aim == "promotion"),
                    post_f   = post * female,
                    post_p   = post * promo,
                    post_f_p = post * female * promo)
    cat(sprintf("  %s cell: %s person-wave-aims, %s persons, mean y %.4f\n",
                .nm, format(nrow(.long), big.mark = ","),
                format(dplyr::n_distinct(.long$person_id), big.mark = ","),
                mean(.long$y, na.rm = TRUE)))
    # v21.31a. REPORT THE GATE THAT MAKES THE _z COLUMNS INTERPRETABLE.
    #
    # The _z twins turn "trained, but not for this aim" and "asked, and
    # trained none" into the same 0. That is the right recode and it is
    # an ASSUMPTION about what a zero means, so the shares composing it
    # belong beside the estimate rather than in the loader's log.
    #
    # Same discipline the training gate already gets in T1: a coalesced
    # zero is reported with its composition, never silently.
    if (.nm == "unconditional" && "aim_battery_asked" %in% names(ev)) {
      .a <- ev$aim_battery_asked[ev$employed == 1]
      .t <- if ("any_training" %in% names(ev))
        ev$any_training[ev$employed == 1] else rep(NA_integer_, length(.a))
      cat(sprintf(paste("    of the employed: %.1f%% answered the battery,",
                        "%.1f%% trained none\n    (a TRUE zero here), and",
                        "%.1f%% trained but left it unanswered (NA).\n"),
                  100 * mean(.a == 1L, na.rm = TRUE),
                  100 * mean(!is.na(.t) & .t == 0, na.rm = TRUE),
                  100 * mean(!is.na(.t) & .t == 1 & .a == 0L, na.rm = TRUE)))
      rm(.a, .t)
    }
    if (nrow(.long) < 500 || dplyr::n_distinct(.long$y) < 2) {
      cat("  [skip] too few rows or no variation.\n"); next
    }
    .fe <- if (FE_SEX_WAVE) "person_id^aim + wave^aim^female" else
      "person_id^aim + wave^aim"
    .m <- try(fixest::feols(stats::as.formula(paste0(
      "y ~ post + post_f + post_p + post_f_p | ", .fe)),
      data = .long, cluster = ~person_id, notes = FALSE, warn = FALSE),
      silent = TRUE)
    if (inherits(.m, "try-error")) { cat("  [skip] fit failed.\n"); next }
    report(paste0("T3b_", .nm), "mother x post x promotion-aimed",
           .m, "post_f_p", "pp", NA_real_, NA_real_, status = "secondary")
    report(paste0("T3b_", .nm, "_post_f"), "mother x post, common across aims",
           .m, "post_f", "pp", NA_real_, NA_real_, status = "secondary")
  }
  
  cat("\n  READ THE TWO TOGETHER. If the unconditional and among-trainees\n")
  cat("  triples agree, the aim result is not an artefact of who trains.\n")
  cat("  If they disagree, the unconditional one is the answer to a level\n")
  cat("  question and the conditional one to a composition question, and\n")
  cat("  they are answers to different questions rather than a conflict.\n")
  cat("  T3's funding contrast is now a ROBUSTNESS CHECK on this, not a\n")
  cat("  stand-in for it.\n")
  rm(.aim_pairs)
} else if (exists("ev")) {
  cat("\n  [skip] T3b: the aim columns are not on this frame.\n")
  cat("  They reach the TRAINING panel at loader v21.30. A panel built\n")
  cat("  earlier has them on the slim panel and not here, which is the\n")
  cat("  gap v21.30 closed: v21.29 derived and selected them and put them\n")
  cat("  in no register, so the script that would test the hypothesis\n")
  cat("  could not see them.\n")
  cat(sprintf("  This panel's build_tag: %s. Rebuild on v21.30 or later.\n",
              tryCatch(attr(panel, "build_tag"), error = function(e) "unknown")))
}

# =====================================================================
# T3c. THE FEE MARGIN, AS A LEVEL
#
# REVISED after RUN_FIGURES_EDA.R C5 on the discovery half. The first
# version of this block contrasted FEMALE against male and was wrong
# about which margin the item moves on. C5 puts the four lifetime groups
# side by side and the answer is not a sex gap:
#
#     Childless men   20.7      Never-mothers   21.5
#     Fathers         20.2      Mothers         27.2
#
# The three non-mother groups sit on top of one another with overlapping
# intervals. Mothers stand alone, roughly six points clear of every
# other group and clear of all three intervals. A `female` coefficient
# averages a null (never-mothers against childless men) with a real
# difference (mothers against everyone) and reports the mean of the two,
# which is a number that describes no group in the data.
#
# THIS REVISION IS THE LICENSED USE OF THE SPLIT, NOT A VIOLATION OF IT.
# RUN_FIGURES_EDA.R exists so that a shape can be seen on the discovery
# half and turned into a specification. That is what happened. What
# would NOT be licensed is estimating the revised contrast on the
# discovery half and reporting it, so T3c_prebirth_conf below re-runs
# the central row on the CONFIRMATION half using the same person hash
# the EDA script uses, and that row is the only one here with any claim
# on a reader.
#
# WHAT THE GROUPING ACTUALLY IS, AND WHY IT DECIDES THE INTERPRETATION.
# group_ever is built by the loader from ever_mother and ever_father. It
# is a LIFETIME, person-level classification, so the "Mothers" bar pools
# every wave a mother is observed in, INCLUDING THE WAVES BEFORE HER
# FIRST BIRTH. It is not a post-birth bar. Read with the loader's
# v21.43a diagnostic, which found the mothers-against-fathers gap in the
# same item NOT MOVING across the birth (|t| 0.98, k = 0 excluded), the
# two facts together say something quite specific: the gap is present
# BEFORE the birth and does not grow at it.
#
# THAT IS NOT SUPPORT FOR THIS PAPER'S MECHANISM, AND MUST NOT BE
# WRITTEN AS IF IT WERE. A birth-response reading of the fee item would
# need the gap to open at the birth. It does not. The parsimonious
# reading is that women who go on to become mothers already differed
# from women who do not, in occupation, industry, employer, or in
# whatever else predicts meeting your own course fees. That is
# selection, and it belongs in the data section as a measured level
# difference with its timing stated, not in the results as a second
# margin pointing the same way as the first.
#
# ONE ARITHMETIC TRAP, CHECKED BELOW RATHER THAN ARGUED ABOUT. The C5
# bar is a share among TRAINEES, so it is a ratio whose denominator is
# training incidence:
#
#     share contributing  =  P(trained and contributed) / P(trained)
#                         =  p_s / p_t
#
# If employer-funded training falls for mothers and self-funded training
# does not, p_t falls, and the share rises with no change whatever in
# how often a mother meets a fee. The paper's headline result is exactly
# such a fall, so the figure could in principle be the headline result
# refracted through a conditional denominator. The decomposition below
# holds p_s at its never-mother value and recomputes the share at the
# mothers' own p_t: if that reproduces the observed mothers' share, the
# bar is arithmetic. The gated column settles the same question from the
# other side, since it has no conditional denominator to move.
#
# THE ROWS, AND WHAT EACH IS FOR.
#
#   T3c_level_mother      mother against every other group, among
#                         trainees, pooled over all waves. The C5 bar
#                         with a standard error. Descriptive.
#   T3c_prebirth          the same contrast restricted to waves BEFORE
#                         the birth, against never-mothers. The row that
#                         separates selection from response. Descriptive.
#   T3c_prebirth_conf     T3c_prebirth on the confirmation half. The one
#                         out-of-sample estimate of a discovery-derived
#                         contrast, and the only row here a reader
#                         should weigh.
#   T3c_level_z_mother    the same contrast on the gated column, whose
#                         denominator is the employed rather than the
#                         trainees and therefore cannot move with
#                         training incidence. Descriptive.
#   T3c_dynamic_mde       the birth-wave contrast, carried for its SE
#                         and MDE alone. Diagnostic.
#
# NO PERSON FIXED EFFECTS ON THE LEVEL ROWS, and that is forced rather
# than chosen: the group indicator is time-invariant, so person_id would
# absorb the term being estimated. Wave fixed effects and person-
# clustered standard errors are what remain, which makes these
# BETWEEN-person contrasts and not the within-person designs the rest of
# this file runs. Said out loud because a reader comparing a T3c
# standard error with a T1 one is comparing two different things.
#
# STATUS: POST-HOC throughout, and the disclosure is sharper than T4c's.
# T4c was designated after T4's coefficients had been seen. This block
# was designated after the loader printed the proportions it describes
# AND revised after the discovery half showed the first contrast was the
# wrong one. Nothing here is graded preferred and nothing should be.
.fee_need <- c("training_self_funded", "training_self_funded_z",
               "any_training", "female", "employed", "treated")
if (exists("ev") && all(.fee_need %in% names(ev))) {
  cat("\n--- T3c. The fee margin: who contributed to the cost ---\n")
  .fee_min_cell <- 100L   # below this a cell is suppressed, not shown

  # TD_NO_WRITE DOES NOT REACH THE CSVs THIS BLOCK WRITES, and on the fast
  # path that is a mismatch worth naming rather than discovering later.
  # v24.1 suppresses the ledger, the decisions file and the sealed
  # estimates so a partial run cannot overwrite a complete one. It does
  # not suppress per-design output, which is correct: a T3c-only run is
  # run precisely to refresh these tables. The consequence is that after
  # such a run the T3c CSVs are NEWER than the ledger sitting beside
  # them, and the ledger still carries whatever T3c rows the last full
  # pass produced. If those two disagree, the CSVs are the current ones.
  # The paper's fee paragraph cites the CSVs, so this is the right way
  # round, but it has to be said out loud in the log of the run that
  # creates the mismatch.
  if (exists("TD_NO_WRITE") && isTRUE(TD_NO_WRITE))
    cat("  NOTE: TD_NO_WRITE is TRUE. This block still writes its own CSVs,\n",
        "        so they will be newer than the preserved ledger. The ledger's\n",
        "        T3c rows are from the last full pass; these tables are not.\n",
        sep = "")

  # mother_ever is female AND treated. treated is person-level here, set
  # from the cohort variable, so this reproduces the loader's group_ever
  # exactly without needing that column on the frame.
  .fe_d <- ev %>%
    dplyr::filter(employed == 1) %>%
    dplyr::mutate(mother_ever = as.integer(female == 1 & treated == 1),
                  grp4 = dplyr::case_when(
                    female == 0 & treated == 0 ~ "childless_men",
                    female == 0 & treated == 1 ~ "fathers",
                    female == 1 & treated == 0 ~ "never_mothers",
                    TRUE                       ~ "mothers"))

  # ---- the four groups, which is what C5 plots -----------------------
  .tr <- .fe_d %>% dplyr::filter(!is.na(any_training), any_training == 1,
                                 !is.na(training_self_funded))
  if (nrow(.tr)) {
    .lv <- .tr %>%
      dplyr::group_by(grp4) %>%
      dplyr::summarise(n = dplyr::n(),
                       persons = dplyr::n_distinct(person_id),
                       pct_contributed = round(100 * mean(training_self_funded), 2),
                       .groups = "drop") %>%
      as.data.frame()
    cat(sprintf("  Among employed trainees: %s person-waves, %.2f%% contributed.\n",
                format(nrow(.tr), big.mark = ","),
                100 * mean(.tr$training_self_funded)))
    print(.lv, row.names = FALSE)
    write.csv(.lv, file.path(OUTDIR, "T3c_fee_level_by_group.csv"),
              row.names = FALSE)

    # THE ARITHMETIC CHECK. p_t is training incidence over the employed,
    # p_s is the gated fee column over the employed, and their ratio is
    # the conditional share the table above reports. Recomputing the
    # mothers' share at the never-mothers' NUMERATOR and the mothers' own
    # DENOMINATOR says how much of the gap needs no behavioural
    # difference at all. This is the one calculation that decides whether
    # C5 is a finding or a restatement of the paper's headline.
    .inc <- .fe_d %>%
      dplyr::filter(!is.na(any_training), !is.na(training_self_funded_z)) %>%
      dplyr::group_by(grp4) %>%
      dplyr::summarise(p_t = mean(any_training),
                       p_s = mean(training_self_funded_z),
                       .groups = "drop") %>%
      as.data.frame()
    .inc$share_implied <- round(100 * .inc$p_s / .inc$p_t, 2)
    print(within(.inc, {p_t <- round(p_t, 4); p_s <- round(p_s, 4)}),
          row.names = FALSE)
    .pm <- .inc$p_s[.inc$grp4 == "never_mothers"]
    .pt <- .inc$p_t[.inc$grp4 == "mothers"]
    .obs <- .inc$share_implied[.inc$grp4 == "mothers"]
    .nm <- .inc$share_implied[.inc$grp4 == "never_mothers"]
    if (length(.pm) && length(.pt) && length(.obs) && length(.nm) &&
        is.finite(.pt) && .pt > 0) {
      .cf <- 100 * .pm / .pt
      cat(sprintf(paste0("\n  DECOMPOSITION. Mothers' observed share %.2f%%, ",
                         "never-mothers' %.2f%%.\n  Holding the numerator at the ",
                         "never-mother rate and using mothers'\n  own training ",
                         "incidence gives %.2f%%, which is %.0f%% of the gap.\n"),
                  .obs, .nm, .cf,
                  100 * (.cf - .nm) / max(.obs - .nm, 1e-9)))
      cat("  A figure near 100 means C5 is the headline training result seen\n")
      cat("  through a conditional denominator and is NOT a second margin.\n")
      cat("  A figure near 0 means mothers genuinely meet fees more often.\n")
      note(sprintf(paste("T3c: denominator accounts for %.0f%% of the",
                         "mothers-against-never-mothers gap in the conditional",
                         "fee share. Read C5 accordingly."),
                   100 * (.cf - .nm) / max(.obs - .nm, 1e-9)))
      rm(.cf)
    }
    rm(.pm, .pt, .obs, .nm, .inc, .lv)
  }

  # ---- BY HORIZON AND SEX, WITH THE DENOMINATOR SHOWN -----------------
  # The v1 run of this block printed the conditional share by horizon and
  # sex and reported the RATIO. The ratio rose from about 1.05 before the
  # birth to about 1.45 after it, which reads as mothers starting to pay
  # their own way. Reading the two series instead of their quotient says
  # something else: on the v1 output women's share was flat across the
  # window, 25.5 before against 26.3 after, while men's fell from 23.7 to
  # 17.7. Roughly nine tenths of the widening was the male series falling.
  #
  # A RATIO IS THE WRONG STATISTIC WHEN BOTH SERIES MOVE, and it is the
  # wrong one here in the specific way that matters: it attributes to
  # mothers a change that happened to fathers. This table reports the two
  # series and never their quotient.
  #
  # AND IT REPORTS THE DENOMINATOR. The conditional share is p_s / p_t, so
  # a fall in men's share is either fathers self-funding less (p_s falls)
  # or fathers' employers financing more of their training (p_t rises)
  # with no change in behaviour. Those are different findings and the
  # quotient cannot tell them apart, so p_t and p_s are printed beside it.
  # If p_t rises for men after the birth, the fall is arithmetic and the
  # paper says nothing about fathers' fee behaviour.
  # THE TWO TABLES SIT ON OPPOSITE SIDES OF THE TRAINING GATE, and the
  # counts below will not match the stacked cells above because of it.
  # jttpewt_any imputes a zero where the module was never asked, so it is
  # never NA and a never-asked person-wave survives into the stack;
  # training_self_funded_z is the gated twin and is NA there, so the same row
  # drops out here. The panel's restriction table states the convention:
  # "_any imputes a zero where never asked; _obs DROPS those rows".
  #
  # On the 6 September pass that is exactly one male person-wave at k = -1,
  # 1,112 in the stacked cell against 1,111 here, with the other two
  # pre-birth horizons agreeing to the row. It is one of the small number of
  # employed person-waves Section 4.3 reports as never asked, and it is
  # reported rather than reconciled: a difference that IS the gate is not a
  # discrepancy, and someone comparing the two tables should be told which
  # side each is on instead of chasing a row.
  # THE WINDOWS DIFFER TOO, and that is the larger of the two reasons the
  # counts do not line up. The stacked cells run K_LO to K_HI, which is -8 to
  # 8; this table runs -4 to 6, because the fee item thins at the edges. The
  # pre-birth horizons the two are usually compared on, -3 to -1, are inside
  # both, so that comparison is about the gate alone. Anywhere else it is
  # about the window first.
  #
  # THE BOUNDS ARE NAMED ONCE. A first version of the gate count below used
  # K_LO and K_HI while this filter used the literals, so it would have
  # reported a figure over -8 to 8 to explain a table covering -4 to 6: a
  # number that does not match the thing it is explaining, which is the
  # failure this comment block exists to prevent.
  .hz_lo <- -4L; .hz_hi <- 6L
  .hz <- .fe_d %>%
    dplyr::filter(treated == 1, !is.na(k), k >= .hz_lo, k <= .hz_hi,
                  !is.na(any_training), !is.na(training_self_funded_z)) %>%
    dplyr::group_by(k, female) %>%
    dplyr::summarise(n = dplyr::n(),
                     p_t = mean(any_training),
                     p_s = mean(training_self_funded_z),
                     .groups = "drop") %>%
    dplyr::mutate(share = ifelse(p_t > 0, 100 * p_s / p_t, NA_real_)) %>%
    as.data.frame()
  if (nrow(.hz)) {
    .hz[, c("p_t", "p_s")] <- round(.hz[, c("p_t", "p_s")], 4)
    # Quantify the gate rather than leaving the two tables to be diffed.
    .gate_lost <- nrow(.fe_d %>% dplyr::filter(
      treated == 1, !is.na(k), k >= .hz_lo, k <= .hz_hi,
      is.na(any_training) | is.na(training_self_funded_z)))
    if (.gate_lost > 0)
      cat("  ", .gate_lost, " employed person-wave(s) over k in [", .hz_lo,
          ", ", .hz_hi, "] are in the stacked\n",
          "  cells above and not in the table below: the module was never\n",
          "  asked, so _any imputes a zero and the gated column is NA. That\n",
          "  is the gate, not a discrepancy. The stacked cells also span a\n",
          "  wider window, so outside that range the counts differ for a\n",
          "  second reason.\n", sep = "")
    rm(.gate_lost)
    .hz$share <- round(.hz$share, 1)
    cat("\n  By horizon and sex. p_t is training incidence, p_s the gated\n")
    cat("  fee column, share = p_s/p_t is what C5 and the v1 table plot.\n")
    print(.hz, row.names = FALSE)
    write.csv(.hz, file.path(OUTDIR, "T3c_fee_by_horizon_decomposed.csv"),
              row.names = FALSE)
    for (.g in c(0, 1)) {
      .a <- .hz[.hz$female == .g & .hz$k <  0, ]
      .b <- .hz[.hz$female == .g & .hz$k >= 0, ]
      if (nrow(.a) && nrow(.b)) {
        .w <- function(x, d) sum(x * d$n) / sum(d$n)
        cat(sprintf(paste0("  %-5s p_t %.3f -> %.3f (%+.3f); ",
                           "p_s %.3f -> %.3f (%+.3f); share %.1f -> %.1f (%+.1f)\n"),
                    if (.g == 1) "women" else "men",
                    .w(.a$p_t, .a), .w(.b$p_t, .b), .w(.b$p_t, .b) - .w(.a$p_t, .a),
                    .w(.a$p_s, .a), .w(.b$p_s, .b), .w(.b$p_s, .b) - .w(.a$p_s, .a),
                    .w(.a$share, .a), .w(.b$share, .b),
                    .w(.b$share, .b) - .w(.a$share, .a)))
      }
    }
    cat("  READ THE TWO SERIES, NOT THE RATIO. If men's share falls while\n")
    cat("  men's p_t rises, their employers are financing more and nothing\n")
    cat("  is happening to fathers' fee behaviour. If men's p_s falls with\n")
    cat("  p_t flat, fathers really do stop paying, and that is a claim\n")
    cat("  about fathers rather than about mothers.\n")
    note(paste("T3c: the widening women-to-men fee ratio around first birth",
               "is mostly the MALE series falling, not the female series",
               "rising. Do not write it as a statement about mothers."))
    rm(.hz)
  }

  # ---- benchmarks, on this margin's own base -------------------------
  .base_fee <- if (nrow(.tr)) mean(.tr$training_self_funded) else NA_real_
  .base_fee_f <- {
    .b <- .tr$training_self_funded[.tr$mother_ever == 1]
    if (length(.b)) mean(.b) else NA_real_
  }
  .base_fee_z <- {
    .b <- .fe_d$training_self_funded_z
    if (any(!is.na(.b))) mean(.b, na.rm = TRUE) else NA_real_
  }
  .base_fee_z_f <- {
    .b <- .fe_d$training_self_funded_z[.fe_d$mother_ever == 1]
    if (any(!is.na(.b))) mean(.b, na.rm = TRUE) else NA_real_
  }
  cat(sprintf(paste0("\n  Bases: %.4f among trainees (mothers %.4f); ",
                     "%.4f on the gated column (mothers %.4f).\n"),
              .base_fee, .base_fee_f, .base_fee_z, .base_fee_z_f))

  # ---- T3c_level_mother ----------------------------------------------
  if (nrow(.tr) >= 500 && dplyr::n_distinct(.tr$training_self_funded) > 1) {
    .m3c1 <- try(fixest::feols(training_self_funded ~ mother_ever | wave,
                               data = .tr, cluster = ~person_id,
                               notes = FALSE, warn = FALSE), silent = TRUE)
    if (!inherits(.m3c1, "try-error"))
      report("T3c_level_mother", "mother, share contributing among trainees",
             .m3c1, "mother_ever", "pp", BM_PROP * .base_fee,
             BM_PROP * .base_fee_f, status = "descriptive (post-hoc)")
    else cat("  [skip] T3c_level_mother: fit failed.\n")
  }

  # ---- T3c_prebirth and its confirmation-half twin -------------------
  # WOMEN ONLY, and mothers restricted to k < 0. If the gap is already
  # open before the first birth it is selection into motherhood and not
  # a response to it, and the paper's fee paragraph has to say so. This
  # is a LEVEL contrast on a pre-birth window, not an event study: the
  # loader has already established there is no birth-wave dynamic to
  # study and a design drawn around a known null would be the failure
  # the ledger exists to catch.
  .pre <- .tr %>%
    dplyr::filter(female == 1,
                  treated == 0 | (treated == 1 & !is.na(k) & k < 0))
  if (nrow(.pre) >= 500 && dplyr::n_distinct(.pre$mother_ever) > 1) {
    .m3c2 <- try(fixest::feols(training_self_funded ~ mother_ever | wave,
                               data = .pre, cluster = ~person_id,
                               notes = FALSE, warn = FALSE), silent = TRUE)
    if (!inherits(.m3c2, "try-error"))
      report("T3c_prebirth", "mothers-to-be vs never-mothers, PRE-BIRTH only",
             .m3c2, "mother_ever", "pp", BM_PROP * .base_fee,
             BM_PROP * .base_fee_f, status = "descriptive (post-hoc)")
    else cat("  [skip] T3c_prebirth: fit failed.\n")

    # The same hash RUN_FIGURES_EDA.R uses, reproduced rather than
    # imported so this file stays standalone. Persons, not person-waves,
    # so a person's pre- and post-birth records never straddle the halves.
    .h <- vapply(as.character(.pre$person_id), function(x)
      sum(utf8ToInt(x) * seq_along(utf8ToInt(x))) %% 2L, integer(1))
    .conf <- .pre[.h == 1L, , drop = FALSE]
    cat(sprintf("  Confirmation half holds %s of %s pre-birth trainee waves.\n",
                format(nrow(.conf), big.mark = ","),
                format(nrow(.pre), big.mark = ",")))
    if (nrow(.conf) >= 300 && dplyr::n_distinct(.conf$mother_ever) > 1) {
      .m3c2b <- try(fixest::feols(training_self_funded ~ mother_ever | wave,
                                  data = .conf, cluster = ~person_id,
                                  notes = FALSE, warn = FALSE), silent = TRUE)
      if (!inherits(.m3c2b, "try-error")) {
        .se_pre_conf <- report("T3c_prebirth_conf",
               "PRE-BIRTH, CONFIRMATION HALF",
               .m3c2b, "mother_ever", "pp", BM_PROP * .base_fee,
               BM_PROP * .base_fee_f, status = "confirmation (discovery-derived)")
        cat("  This is the only row in T3c estimated on data that did not\n")
        cat("  produce the specification. If the pre-birth gap is here, the\n")
        cat("  fee difference precedes motherhood and the paper says so.\n")
      }
    } else cat("  [skip] T3c_prebirth_conf: confirmation half too thin.\n")
    rm(.h, .conf)
  } else cat("  [skip] T3c_prebirth: too few pre-birth rows.\n")

  # ---- T3c_level_z_mother: no conditional denominator ----------------
  .dz <- .fe_d %>% dplyr::filter(!is.na(training_self_funded_z))
  if (nrow(.dz) >= 500 && dplyr::n_distinct(.dz$training_self_funded_z) > 1) {
    .m3c3 <- try(fixest::feols(training_self_funded_z ~ mother_ever | wave,
                               data = .dz, cluster = ~person_id,
                               notes = FALSE, warn = FALSE), silent = TRUE)
    if (!inherits(.m3c3, "try-error"))
      report("T3c_level_z_mother", "mother, gated fee column over the employed",
             .m3c3, "mother_ever", "pp", BM_PROP * .base_fee_z,
             BM_PROP * .base_fee_z_f, status = "descriptive (post-hoc)")
    else cat("  [skip] T3c_level_z_mother: fit failed.\n")
  }

  # ---- T3c_dynamic_mde: the power statement, and nothing else --------
  # THE POINT ESTIMATE IS NOT A RESULT AND MUST NOT BE QUOTED. The ledger
  # has no estimate column, so the written record is safe by
  # construction; the console is not, because the master unblinds.
  if (nrow(.dz) >= 500 && dplyr::n_distinct(.dz$training_self_funded_z) > 1) {
    .m3c4 <- try(fixest::feols(
      stats::as.formula(paste0("training_self_funded_z ~ post + post_f | ",
                               fe_block())),
      data = .dz, cluster = ~person_id, notes = FALSE, warn = FALSE),
      silent = TRUE)
    if (!inherits(.m3c4, "try-error")) {
      .se3c <- report("T3c_dynamic_mde", "birth-wave contrast, POWER ONLY",
                      .m3c4, "post_f", "pp", BM_PROP * .base_fee_z,
                      BM_PROP * .base_fee_z_f,
                      status = "diagnostic (power only)")
      cat("  READ THE MDE, NOT THE COEFFICIENT. The loader's raw\n")
      cat("  pre-against-post difference on this margin returned |t| 0.98\n")
      cat("  with k = 0 excluded, so this design was specified already\n")
      cat("  knowing its answer and the coefficient carries no evidential\n")
      cat("  weight. Quote the MDE and the level rows; not this number.\n")
      if (!is.null(.se3c) && is.finite(.se3c) && is.finite(.base_fee_z_f))
        note(sprintf(paste("T3c: the fee dynamic has MDE %.4f against mothers'",
                           "gated base of %.4f. The level difference is",
                           "PRE-BIRTH; the birth-wave response is unmeasurable,",
                           "not absent."), mde(.se3c), .base_fee_z_f))
      rm(.se3c)
    } else cat("  [skip] T3c_dynamic_mde: fit failed.\n")
  }

  # WHAT THIS BLOCK MAY CONCLUDE, stated against what the run produced
  # rather than asserted in advance.
  #
  # An earlier version printed, unconditionally, that the fee difference
  # "is already present before the first birth and does not open at it".
  # That was written under a misreading of the discovery-half figures, in
  # which group_ever was taken for a person-level grouping. It is not one,
  # and once the pre-birth panel was rebuilt on a lifetime grouping the
  # mothers-to-be sat with the fathers-to-be rather than apart from them,
  # so the sentence was probably false. Worse, it printed under BLIND,
  # where the coefficient it describes is sealed: the log asserted a
  # finding the run could not show, which is the one thing a blinded log
  # must never do.
  cat("\n  WHAT T3c LICENSES. The LEVEL rows above are descriptive and stand\n")
  cat("  on their own: they say how often each group meets a training fee.\n")
  cat("  Whether that difference PRECEDES the first birth is T3c_prebirth's\n")
  cat("  question, and under BLIND its coefficient is sealed, so this log\n")
  cat("  cannot answer it and does not pretend to. Read the coefficient\n")
  cat("  after unblinding, against the MDE printed beside it.\n")

  # AND SAY WHETHER THE CONFIRMATION ROW COULD HAVE ANSWERED IT. It is the
  # only row estimated on data that did not produce the specification,
  # which is the whole reason it exists, and a row that cannot resolve an
  # effect the size of the one already observed confirms nothing either
  # way. Better read off the log than discovered later.
  if (exists(".se_pre_conf") && !is.null(.se_pre_conf) &&
      is.finite(.se_pre_conf)) {
    .obs_gap <- abs(.base_fee_f - .base_fee)
    cat(sprintf(paste0("\n  T3c_prebirth_conf has MDE %.4f; the observed ",
                       "mother-minus-pooled level\n  gap among trainees is ",
                       "%.4f.\n"), mde(.se_pre_conf), .obs_gap))
    if (mde(.se_pre_conf) > .obs_gap) {
      cat("  THE CONFIRMATION ROW IS UNDERPOWERED FOR ITS OWN PURPOSE: it\n")
      cat("  cannot detect a gap the size of the one the level rows report,\n")
      cat("  so it can neither confirm nor refute the discovery-derived\n")
      cat("  contrast. Report it as a bound and do not read a null off it.\n")
      note(sprintf(paste("T3c: prebirth_conf MDE %.4f exceeds the observed",
                         "level gap %.4f, so the confirmation half cannot",
                         "settle the pre-birth question. Report the bound."),
                   mde(.se_pre_conf), .obs_gap))
    }
    rm(.obs_gap)
  }
  cat("\n  WHAT T3c DOES NOT LICENSE, whatever the coefficients show: reading\n")
  cat("  the fee margin as a second channel alongside the time margin. The\n")
  cat("  two are measured on different populations and the triple\n")
  cat("  difference contrasts neither of them against this item.\n")
  rm(.fe_d, .tr, .pre, .dz, .base_fee, .base_fee_f, .base_fee_z,
     .base_fee_z_f, .fee_min_cell)
  if (exists(".se_pre_conf")) rm(.se_pre_conf)
  if (exists(".b")) rm(.b)
} else if (exists("ev")) {
  cat("\n  [skip] T3c: the fee columns are not on this frame.\n")
  cat(sprintf("  Missing: %s\n",
              paste(setdiff(.fee_need, names(ev)), collapse = ", ")))
  cat("  training_self_funded has been derived since v18; its gated twin\n")
  cat("  training_self_funded_z arrives at loader v21.43. A panel built\n")
  cat("  earlier carries the ungated column alone, and the ungated column\n")
  cat("  conditions on the outcome, so the level rows would answer a\n")
  cat("  composition question while appearing to answer a level one.\n")
  cat(sprintf("  This panel's build_tag: %s. Rebuild on v21.43 or later.\n",
              tryCatch(attr(panel, "build_tag"), error = function(e) "unknown")))
}
rm(.fee_need)
if (exists(".hz_lo")) rm(.hz_lo, .hz_hi)


source("03_finish.R")
