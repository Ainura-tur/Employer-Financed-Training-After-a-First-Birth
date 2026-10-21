# =====================================================================
# 03a_design_audit.R
# ---------------------------------------------------------------------
# The design audit: which specifications can carry the claim.
#
# Designs: T0b complete-case rates, T1 estimand labelling, T2 reference period, T4 full-time returners, T5 incidence against dosage, T6 within-couple, T7 anticipation, T9 substitution into study
# Paper:   Section 6, the investment margin
# Source:  03training_designs.R lines 1282-2145 and 2794-3778, extracted verbatim.
#
# FAST. Every design here is a linear probability model or a small fixed-effects fit, so this is the module to re-run while iterating. It carries the headline: T1's employer-financed difference-in-differences is the -0.116 that Table 7 reports.
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

TD03_MODULE <- "03a_design_audit.R"
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

# Paper numbers: begun once here, registered where each number is computed
# below, and flushed once before 03_finish.R, so a run that stops midway
# leaves the previous complete registry rather than a partial one.
source("paper_numbers.R")
pn_begin("03a_design_audit.R", build_tag = attr(panel, "build_tag"))

# =====================================================================
# T0b. COMPLETE-CASE RATES BY GROUP, BEFORE ANY DESIGN RUNS
#
# The F1 diagnostic measures missingness separately for childless men,
# fathers, never-mothers and mothers, and mothers are highest on nearly
# every variable: tenure_employer 29.5% against 14.5% for never-mothers,
# precarious_score 37.5% against 19%, hourly_wage_clean 36% against 19%,
# training_paid_time_obs 22.5% against 8%.
#
# So a complete-case specification on any of those does not compare
# mothers with never-mothers. It compares the mothers who ANSWERED with
# the never-mothers who answered, and the first group is selected roughly
# twice as hard. T8 already shows this at the sex level: the stacked
# frame starts with equal halves and returns 909 more men than women.
#
# Reported here rather than inside each design, because the number a
# reader needs is the same one every time and it belongs before the
# estimates. It is a fact about the sample, not a fix: nothing below
# drops or imputes on the strength of it.
# THE GROUPING IS A CROSS-SECTION, SO IT NEEDS THE LIFETIME COLUMN.
# ever_parent is absorbing without being constant within a person: it is 0
# for a mother's pre-birth waves, correctly, because she was not a parent
# when they were recorded. Cutting a comparison group on it puts every
# future mother's pre-birth waves in never-mothers and only her post-birth
# ones in mothers, so the table below was comparing a group against part of
# itself. On the 4 September panel that is 6,245 person-waves across 1,423
# women.
#
# IT BIT HERE HARDER THAN THE COUNT SUGGESTS. Missingness in this table is
# employment appearing as missingness, and pre-birth waves are the most
# employed a mother ever is. They were sitting in the never-mothers column
# holding its rates down and out of the mothers column holding its rates up,
# which is the direction that makes the table's own conclusion look
# stronger. Moving them narrows the mother-minus-never gap on every
# employment-conditional row.
#
# group_ever_life comes from the loader and matches the rule
# MASTER_training_y.R applies. Where it is absent the lifetime maximum is
# taken here rather than falling back to the wave-varying column, because a
# fallback that reintroduces the contamination is worse than none.
.g4_src <- if ("group_ever_life" %in% names(panel)) "group_ever_life" else
  if ("group_ever" %in% names(panel) &&
      all(tapply(as.character(panel$group_ever), panel$person_id,
                 function(x) length(unique(x[!is.na(x)]))) <= 1, na.rm = TRUE))
    "group_ever" else "rebuild"
if (exists("panel") && all(c("female", "ever_parent") %in% names(panel))) {
  if (.g4_src == "rebuild") {
    cat("  T0b: no person-level group column, rebuilding one from\n",
        "      max(ever_parent) within person.\n", sep = "")
    .ep_life <- ave(as.numeric(ifelse(is.na(panel$ever_parent), 0,
                                      panel$ever_parent)),
                    panel$person_id, FUN = function(x) max(x, na.rm = TRUE))
  } else {
    cat("  T0b: grouping on ", .g4_src, ", which partitions persons.\n", sep = "")
    .ep_life <- as.numeric(panel[[.g4_src]] %in% c("mothers_ever", "fathers_ever"))
  }
  .g4 <- with(panel, ifelse(is.na(female) | is.na(.ep_life), NA_character_,
                            ifelse(female == 0 & .ep_life == 0, "childless men",
                                   ifelse(female == 0 & .ep_life == 1, "fathers",
                                          ifelse(female == 1 & .ep_life == 0, "never-mothers", "mothers")))))
  # v21.48: the gated course count where the panel has it, so this table
  # describes the column T5b estimates on. The ungated one is complete by
  # construction, which is why the manuscript flagged its zero.
  .crs_t0b <- if ("total_courses_z" %in% names(panel)) "total_courses_z" else
    "total_courses"
  .vars <- intersect(c("hourly_wage_clean", "training_paid_time_obs",
                       "jttpewt_obs", .crs_t0b,
                       "total_training_hours_z", "tenure_employer",
                       "precarious_score", "housework_hours",
                       "experience_years", "educ_years"), names(panel))
  if (length(.vars) && any(!is.na(.g4))) {
    cat("\n--- T0b. Missingness by group (%) ---\n")
    .tab <- do.call(rbind, lapply(.vars, function(v) {
      r <- tapply(is.na(panel[[v]]), .g4, mean)
      data.frame(variable = v, t(round(100 * r, 1)), check.names = FALSE)
    }))
    print(.tab, row.names = FALSE)
    # WRITE IT. This table is typed into the manuscript by hand, which is
    # why the provenance names a run log and no file for it and why nobody
    # could rebuild it when the grouping changed. paper_tables.R reads this.
    # AND SAY WHETHER IT WROTE. The first version guarded on
    # exists("OUTDIR") and did nothing when it was unset, silently, and
    # write.csv prints nothing on success either, so the 14:57 log showed
    # no trace of this line in either direction. A guard that skips
    # without a word is the failure mode this whole module exists to
    # catch, and paper_tables.R waits on this file: absent, it carries
    # the values typed into the manuscript and marks them red.
    .t0b_dir <- if (exists("OUTDIR")) OUTDIR else
      if (exists("DIR_DESIGNS")) DIR_DESIGNS else NA_character_
    if (is.na(.t0b_dir)) {
      cat("  *** T0b_missingness_by_group.csv NOT WRITTEN: neither OUTDIR\n",
          "      nor DIR_DESIGNS is set, so there is nowhere to put it.\n",
          "      paper_tables.R will fall back to the manuscript's typed\n",
          "      values, which are on the OLD grouping.\n", sep = "")
    } else {
      .t0b_out <- file.path(.t0b_dir, "T0b_missingness_by_group.csv")
      .ok <- tryCatch({ utils::write.csv(.tab, .t0b_out, row.names = FALSE)
                        TRUE }, error = function(e) { cat("  *** T0b csv failed: ",
                        conditionMessage(e), "\n", sep = ""); FALSE })
      if (.ok) cat("  wrote ", .t0b_out, " (", nrow(.tab), " rows x ",
                   ncol(.tab) - 1L, " groups)\n", sep = "")
      rm(.t0b_out, .ok)
    }
    rm(.t0b_dir)
    .num <- .tab[, setdiff(names(.tab), "variable"), drop = FALSE]
    .spread <- apply(.num, 1, function(z) diff(range(z, na.rm = TRUE)))
    cat(sprintf("\n  Widest between-group spread: %s, %.1f points.\n",
                .tab$variable[which.max(.spread)], max(.spread, na.rm = TRUE)))
    if (max(.spread, na.rm = TRUE) > 10)
      cat("  *** A complete-case design on that variable compares who\n",
          "      ANSWERED, not who was asked. Report the rates beside any\n",
          "      estimate conditioning on it, and prefer one that does not.\n",
          sep = "")
    # A variable at 0% in EVERY group is usually never-NA by
    # CONSTRUCTION rather than fully observed. experience_years is the
    # case in point: the Mincer proxy fills it wherever ehtjb is missing,
    # so its 0% conceals the 5.2%/4.4% proxy split.
    .zero <- .tab$variable[apply(.num, 1,
                                 function(z) all(z < 0.05, na.rm = TRUE))]
    if (length(.zero)) {
      cat(sprintf("  At 0%% in every group: %s.\n",
                  paste(.zero, collapse = ", ")))
      cat("  Check each is OBSERVED rather than NA-proof by construction;\n")
      cat("  a coalesced column cannot be missing and is not complete.\n")
    }
    rm(.tab, .num, .spread, .zero)
  }
  rm(.g4, .vars)
}


# =====================================================================
# T1. ESTIMAND LABELLING
# =====================================================================
cat("\n--- T1. Unconditional against employed-only ---\n")

# v21.8. What this block used to do: compute pct_coded_zero and
# pct_missing on jttpewt_any among the non-employed, then conclude from
# a high first and a low second that "the zeros are structural". Both
# numbers are fixed by construction. The column is coalesced, so
# pct_missing is 0 in every panel this script will ever see, and
# pct_coded_zero is then whatever one minus the rate happens to be. The
# conclusion was true, but it was not being tested; the block would have
# printed the same reassurance on a panel where it was false.
#
# The loader measures it now. rate_obs_nonemployed is jttpewt among the
# non-employed WHO WERE ASKED, which is the only group that can say
# whether the imputed zero is a true zero. jttpewt is a past-twelve-
# months item and employed is a status at interview, so a person who
# left a job in March can legitimately carry a 1.
.tg <- tryCatch(training_gate_facts(panel), error = function(e) NULL)

nonemp_zero <- ev %>% filter(employed == 0) %>%
  summarise(n = n(),
            pct_asked = if ("training_module_asked" %in% names(ev))
              round(100 * mean(training_module_asked == 1), 1) else NA_real_,
            # measured: among the non-employed who WERE asked
            rate_obs = if ("jttpewt_obs" %in% names(ev))
              round(mean(jttpewt_obs, na.rm = TRUE), 3) else NA_real_,
            # what the unconditional arm below actually reads, with the
            # never-asked imputed to zero
            rate_any = round(mean(jttpewt_any, na.rm = TRUE), 3))
cat("Among the non-employed:\n"); print(as.data.frame(nonemp_zero), row.names = FALSE)

if (!is.na(nonemp_zero$rate_obs)) {
  .n_imputed <- nonemp_zero$n - round(nonemp_zero$n * nonemp_zero$pct_asked / 100)
  cat(sprintf(paste0("rate_obs is MEASURED on the %.1f%% who were asked; ",
                     "rate_any imputes\nzero to the other %s and is what ",
                     "the unconditional arm below reads.\nThe gap between ",
                     "them is the imputation, not behaviour.\n"),
              nonemp_zero$pct_asked, format(.n_imputed, big.mark = ",")))
  if (nonemp_zero$rate_obs > 0.01) {
    cat(sprintf(paste0("THE ZERO IS NOT A TRUE ZERO. A non-employed ",
                       "person-wave that WAS asked\nreports employer-funded ",
                       "training %.1f%% of the time, so the unconditional\n",
                       "arm is understated by roughly %s person-waves. ",
                       "Report the direction\nas well as the existence of ",
                       "the imputation.\n"),
                100 * nonemp_zero$rate_obs,
                format(round(.n_imputed * nonemp_zero$rate_obs),
                       big.mark = ",")))
    note(paste0("T1: the unconditional arm imputes zero to ",
                format(.n_imputed, big.mark = ","), " non-employed ",
                "person-waves that were never asked, while those that WERE ",
                "asked report ", sprintf("%.3f", nonemp_zero$rate_obs),
                ". The unconditional path is understated by roughly ",
                format(round(.n_imputed * nonemp_zero$rate_obs),
                       big.mark = ","), " person-waves and the shortfall ",
                "moves with employment, therefore with event time."))
  } else {
    cat("The measured rate among the asked non-employed is below 0.01, so\n")
    cat("the imputed zero is supported by the measured one.\n")
  }
} else {
  cat("jttpewt_obs is absent, so whether the zero is true or imputed is\n")
  cat("UNTESTED here. Rebuild on the v21.6 loader or later.\n")
}
if (!is.null(.tg) && !is.null(.tg$structural_zero_note))
  cat("\nLoader's declaration:\n", strwrap(.tg$structural_zero_note,
                                           width = 72, prefix = "  "),
      sep = "\n")

# The unconditional arm WANTS the imputed zero: dropping it rebuilds the
# employed-only sample, which is the comparison T1 exists to make. That
# is a declaration, so it is made to the guard rather than left implicit.
assert_training_gate(c("jttpewt_any", "jttpeot_any"),
                     context = "T1 unconditional estimand",
                     allow_structural_zero = TRUE, data = panel)

# v21.14e. THE SECOND THING ABOUT THIS OUTCOME, declared in the loader's
# manifest and previously enforced nowhere. jttrwrk is PQ E1 in waves 7 to
# 19 and PQ E6 in 20 to 24, so every training design here pools across a
# position change in the gate item, and wave 20 is also 2020.
#
# Declared rather than fixed, because there is nothing to fix. Restricting
# to one side halves the window and does not identify which of the two
# things moved; interacting on training_gate_regime absorbs both together.
# The guard exists so the choice is visible in the log.
if (exists("assert_training_gate_position"))
  assert_training_gate_position(
    c("jttpewt_any", "jttpeot_any", "any_training"),
    context = "T1-T6 training outcomes, waves 12-24",
    data = panel, allow_pooled_regime = TRUE)

# v23. any_training JOINS THE LOOP, and the reason is a ledger row with no
# design behind it.
#
# TRAINING_PAPER_OUTLINE section 2 carries "Any training, -0.110 (SE 0.019),
# preferred". Every other row in that table matches a tag this module prints.
# That one did not. The nearest SE in the whole log is T4_any at 0.0191, and
# T4 is FULL-TIME RETURNERS on jttpewt_any, not any training - so the ledger
# was either mislabelled, or quoting a number nothing here produces.
#
# any_training was on the panel the whole time. It is asserted through the
# training gate three lines above and used as a denominator in T5. It was
# trusted everywhere except where it was estimated, which is the one place
# that would have caught this.
#
# THE BASE RATE CHANGES WITH THE OUTCOME and the old two-way `if` would have
# silently handed any_training the OWN-TIME benchmark, which is roughly a
# third of the employer-funded one. That would have graded a wide MDE as OK.
# Explicit switch instead.
for (out in c("jttpewt_any", "jttpeot_any", "any_training")) {
  if (!out %in% names(ev)) {
    cat(sprintf("  [T1_%s] not on the panel; skipped.\n", out)); next
  }
  bm  <- switch(out, jttpewt_any = bm_pewt,   jttpeot_any = bm_peot,   bm_any)
  bmf <- switch(out, jttpewt_any = bm_f_pewt, jttpeot_any = bm_f_peot, bm_f_any)
  d_all <- ev %>% filter(!is.na(.data[[out]]))
  m <- fixest::feols(stats::as.formula(paste0(out, " ~ post + post_f | ", fe_block())),
                     data = d_all, cluster = ~person_id, notes = FALSE, warn = FALSE)
  report(paste0("T1_", out, "_uncond"), paste(out, "unconditional"),
         m, "post_f", "pp", bm, bmf)
  d_emp <- filter(d_all, employed == 1)
  m2 <- fixest::feols(stats::as.formula(paste0(out, " ~ post + post_f | ", fe_block())),
                      data = d_emp, cluster = ~person_id, notes = FALSE, warn = FALSE)
  report(paste0("T1_", out, "_employed"), paste(out, "employed only"),
         m2, "post_f", "pp", bm, bmf)

  # --- AGE FIXED EFFECTS, THE SAME ARM WITH ONE MORE ABSORPTION --------
  #
  # WHAT IT ANSWERS. The comparison pool runs 20 to 55 and neither this
  # specification nor the Sun-Abraham one carries age. Mothers are younger
  # than the never-treated pool on average and training incidence falls with
  # age, so part of a post-birth decline could be the treated drifting up an
  # age gradient the controls are averaged over. Absorbing age compares
  # people of the SAME AGE in the SAME WAVE, one of whom has had a first
  # birth and one of whom has not.
  #
  # post_f IS STILL IDENTIFIED, and the reason is worth writing down because
  # the arithmetic looks collinear. Given person fixed effects, age is a
  # deterministic function of wave: age = wave - year of birth, and year of
  # birth is fixed within person. But `post` steps at the person's OWN birth
  # wave, which differs across people of the same age, so two people who are
  # 31 in wave 18 can differ in post. That variation is what identifies
  # post_f, and adding age removes the between-age comparison rather than
  # the one the estimate rests on.
  #
  # WHAT IT DOES NOT FIX. The age dummies themselves are the classic
  # age-period-cohort problem and are not separately interpretable here;
  # they are absorbed and never read. fixest drops whatever levels are
  # collinear, which is correct and will appear in the log.
  #
  # POST-HOC AND STAYS POST-HOC. The pre-analysis ledger does not contain
  # this arm. It is a robustness row answering a referee, and reporting it
  # as anything else would repeat the T4c promotion the table note has to
  # explain.
  if ("age" %in% names(d_emp)) {
    m3 <- fixest::feols(
      stats::as.formula(paste0(out, " ~ post + post_f | ", fe_block("age"))),
      data = d_emp, cluster = ~person_id, notes = FALSE, warn = FALSE)
    report(paste0("T1_", out, "_employed_agefe"),
           paste(out, "employed only, age absorbed"),
           m3, "post_f", "pp", bm, bmf, status = "post-hoc")
    .e2 <- tryCatch(unname(fixest::coeftable(m2)["post_f", "Estimate"]),
                    error = function(e) NA_real_)
    .e3 <- tryCatch(unname(fixest::coeftable(m3)["post_f", "Estimate"]),
                    error = function(e) NA_real_)
    if (!is.na(.e2) && !is.na(.e3))
      cat(sprintf(paste0("  [%s] age absorbed: %+.4f against %+.4f",
                         " without (%+.4f).\n"), out, .e3, .e2, .e3 - .e2))
    cat("  A move here is age composition, not parenthood.\n")
    if (isTRUE(get0("TD_SCREEN_ON", ifnotfound = FALSE)))
      cat("  The two are entangled by the screen, which cuts on age minus\n",
          "  experience.\n", sep = "")
    rm(.e2, .e3)
  } else {
    cat(sprintf("  [T1_%s] no `age` column, so the age-absorbed arm is skipped.\n",
                out))
  }
}

# ---- Paper numbers: A.5, who leaves the wage sample while still working ----
.need <- c("is_self_employed", "fy_business_income_nonzero", "fy_wages")
if (!all(.need %in% names(panel)))
  stop("A.5 paper numbers need ", paste(setdiff(.need, names(panel)), collapse = ", "),
       " in the frame.", call. = FALSE)
.mo <- ev %>% filter(treated == 1, female == 1, employed == 1)
pn_register("selfemp_mothers_pre", mean(.mo$is_self_employed[.mo$k < 0] == 1, na.rm = TRUE),
            digits = 1, scale = 100, note = "employed mothers, is_self_employed, k < 0")
pn_register("selfemp_mothers_post6", mean(.mo$is_self_employed[.mo$k >= 6] == 1, na.rm = TRUE),
            digits = 1, scale = 100, note = "employed mothers, is_self_employed, k >= 6")
.w <- panel %>% filter(female == 1)
pn_register("business_zero_wage_women",
            mean(.w$fy_business_income_nonzero == 1 & .w$fy_wages == 0, na.rm = TRUE),
            digits = 2, scale = 100, note = "women's person-waves in the frame, business income, zero wages")
rm(.mo, .w, .need)
# ---------------------------------------------------------------------
# T1b. THE EMPLOYMENT MARGIN  (was: reconciliation with the published
#      number, R2)
#
# v21.31 SCOPE. The heading and the framing below were about matching a
# published headline. There is no published number to match, and
# PAPER_ATTENTION.md section 0 says a comparison against a target nobody
# is pursuing frames disagreement as failure.
#
# What the block computes survives the reframing entirely, because the
# quantity is substantive and not a comparison: the raw women-minus-men
# change from k = -1 to k = 0 on BOTH samples, whose difference IS the
# employment margin. That margin is already counted in the experience
# path, so reporting it again as a training result would double-count the
# same behaviour. Read what follows as a measurement of that margin, not
# as a reconciliation.
#
# Both estimands are adequately powered, so the choice between them is a
# labelling decision rather than a design constraint. It is not, however,
# a cosmetic one. The raw women-minus-men change from k = -1 to k = 0 is
# computed on both samples below. If the published headline matches the
# unconditional version, then the difference between the two is the
# employment margin, and that margin is already counted in the experience
# path. Reporting it again as a training result would double-count the
# same behaviour and would contradict the claim in the discussion that
# the training margin is independent of the wage and experience measures.
# ---------------------------------------------------------------------
raw_gap_drop <- function(d, yvar, k_from = -1L, k_to = 0L) {
  g <- d %>%
    filter(treated == 1, k %in% c(k_from, k_to), !is.na(.data[[yvar]])) %>%
    group_by(k, female) %>%
    summarise(rate = mean(.data[[yvar]]), .groups = "drop") %>%
    pivot_wider(names_from = female, values_from = rate, names_prefix = "f")
  if (!all(c(k_from, k_to) %in% g$k) || !all(c("f0", "f1") %in% names(g)))
    return(NA_real_)
  r <- (g$f1[g$k == k_to] - g$f0[g$k == k_to]) -
    (g$f1[g$k == k_from] - g$f0[g$k == k_from])
  if (length(r) != 1) NA_real_ else r
}

drop_unc <- raw_gap_drop(ev, "jttpewt_any")
drop_emp <- raw_gap_drop(ev %>% filter(employed == 1), "jttpewt_any")
emp_margin <- drop_unc - drop_emp

cat("\nRaw women-minus-men change in employer-funded training, k = -1 to 0:\n")
cat(sprintf("  unconditional            : %+.4f\n", drop_unc))
cat(sprintf("  employed only            : %+.4f\n", drop_emp))
cat(sprintf("  difference, the employment margin: %+.4f\n", emp_margin))
cat(sprintf("  manuscript headline      : %s\n",
            if (is.finite(MS_HEADLINE)) sprintf("%+.4f", MS_HEADLINE) else "not registered yet"))
# The share is taken against the UNCONDITIONAL RAW DROP, not against the
# manuscript headline (R21). The first version divided by MS_HEADLINE, which
# is a Sun and Abraham estimate, while the numerator is a raw two-period
# change; the ratio then mixed estimators and understated the margin, 19%
# against the 23% the internally consistent denominator gives. The gap
# between the raw drop and the headline is a separate quantity and is
# reported as such, because it is a reproduction question rather than a
# decomposition.
if (!is.na(emp_margin) && !is.na(drop_unc) && drop_unc != 0)
  cat(sprintf("  employment margin as a share of the unconditional drop: %.0f%%\n",
              100 * emp_margin / drop_unc))
if (!is.na(drop_unc) && is.finite(MS_HEADLINE) && MS_HEADLINE != 0) {
  cat(sprintf("  raw unconditional drop against the headline: %+.4f vs %+.4f",
              drop_unc, MS_HEADLINE))
  cat(sprintf("  (%.0f%% of it)\n", 100 * drop_unc / MS_HEADLINE))
  cat("  The raw two-period change does not reproduce the published\n")
  cat("  coefficient and is not meant to: one is a difference in means, the\n")
  cat("  other an interaction-weighted estimate over all horizons. Quote\n")
  cat("  the margin as a share of the raw drop and keep the reproduction\n")
  cat("  question with G3, where the published specification is re-run.\n")
}

# The same reconciliation over k = -1 to +1. The twelve-month reference
# period means part of the response lands one wave after the birth, so a
# comparison struck at the birth wave alone understates both the total
# and the employment component of it. Report whichever window the text
# ends up using, but compute both before choosing.
drop_unc2 <- raw_gap_drop(ev, "jttpewt_any", k_from = -1L, k_to = 1L)
drop_emp2 <- raw_gap_drop(ev %>% filter(employed == 1), "jttpewt_any",
                          k_from = -1L, k_to = 1L)
emp_margin2 <- drop_unc2 - drop_emp2
cat("\nSame, over k = -1 to +1, which is the window the reference period\n")
cat("argues for:\n")
cat(sprintf("  unconditional            : %+.4f\n", drop_unc2))
cat(sprintf("  employed only            : %+.4f\n", drop_emp2))
cat(sprintf("  difference, the employment margin: %+.4f\n", emp_margin2))
if (!is.na(emp_margin2) && !is.na(drop_unc2) && drop_unc2 != 0)
  cat(sprintf("  employment margin as a share of the two-wave fall: %.0f%%\n",
              100 * emp_margin2 / drop_unc2))

cat("\nIf the headline is the unconditional path, that share of it is the\n")
cat("employment fall rather than a training decision, and the experience\n")
cat("path has already counted it. Name the estimand in the text, and if\n")
cat("the independence claim in the discussion is to stand, report the\n")
cat("employed-only version there.\n")
note(sprintf(paste("T1: name the estimand. The employment margin is %+.3f of the",
                   "%+.3f unconditional drop at the birth wave (%.0f%%) and",
                   "%.0f%% over the two-wave window. Report employed-only",
                   "where the independence claim is made."),
             emp_margin, drop_unc,
             100 * emp_margin / drop_unc,
             100 * emp_margin2 / drop_unc2))

# ---------------------------------------------------------------------
# T1c. THE THIRD ESTIMAND
#
# The choice above is between counting a mother who has left employment
# as a zero and dropping her. Neither is right, because a mother on paid
# parental leave has not left her employer and has not made a training
# decision either: she is attached and unavailable. The discovery run
# found nlreapm, "Not looking for work - Pregnancy/Maternity leave",
# present in all 13 waves. With it the sample can be defined as employed
# OR on leave, which is the estimand the text actually wants and which
# resolves the labelling problem rather than naming it.
# ---------------------------------------------------------------------
leave_col <- intersect(c("on_maternity_leave", "on_parental_leave",
                         "nlf_reason_maternity", "nlreapm"), names(ev))
if (length(leave_col)) {
  lv <- leave_col[1]
  ev$attached <- as.integer(ev$employed == 1 |
                              (!is.na(ev[[lv]]) & ev[[lv]] == 1))
  cat(sprintf("\nAttached (employed or on leave) using %s: %s person-waves,\n",
              lv, format(sum(ev$attached == 1), big.mark = ",")))
  cat(sprintf("against %s employed.\n",
              format(sum(ev$employed == 1, na.rm = TRUE), big.mark = ",")))
  d_att <- ev %>% filter(attached == 1, !is.na(jttpewt_any))
  m_att <- fixest::feols(stats::as.formula(paste0("jttpewt_any ~ post + post_f | ", fe_block())),
                         data = d_att, cluster = ~person_id,
                         notes = FALSE, warn = FALSE)
  report("T1_attached", "employer-funded, employed or on parental leave",
         m_att, "post_f", "pp", bm_pewt, bm_f_pewt, status = "PREFERRED")
  cat("  Compare against the two rows above. If this sits close to the\n")
  cat("  employed-only version, the composition change at the birth wave is\n")
  cat("  separation rather than leave and the employed-only estimand is\n")
  cat("  safe. If it sits close to the unconditional one, most of the\n")
  cat("  employment margin is leave and the unconditional path is not\n")
  cat("  measuring a training decision at all.\n")
  note("T1: report the employed-or-on-leave estimand; it is the one the text means.")
} else {
  cat("\nNo maternity-leave indicator in the panel, so the third estimand\n")
  cat("cannot be formed. nlreapm is present in all 13 waves of the release.\n")
  cat("Adding it is additive and it settles the estimand question instead\n")
  cat("of leaving it to a labelling sentence.\n")
}

# =====================================================================
# T2. REFERENCE PERIOD
#
# The items ask about the last twelve months. If the drop is a reference
# window straddling the birth rather than a post-birth response, three
# things should hold: the discontinuity sits at k = 0 under the natural
# clock and moves to k = -1 under a clock shifted by one wave; the men's
# series does not move; and the never-treated series is flat in calendar
# time. Test all three before reading the birth-wave coefficient.
# =====================================================================
cat("\n--- T2. Reference-period diagnostic ---\n")

# v21.8. Everything from here to T6 estimates on employed == 1, so the
# routing is handled by the SAMPLE rather than by the column, and the
# _any pair is legitimate on this frame. That is a declaration and it is
# made once, to the guard, rather than left to be inferred from the
# filters at each site.
#
# The message it prints is the one that matters: employment is the main
# reason the module goes unasked but not the only one, and the residual
# is gap_emp in the coverage table above. On the v21.7 loader run that
# residual was 17 person-waves in 108,199, spread across eleven of the
# thirteen waves, so it is keying rather than a wave-specific routing
# failure. It is small enough to leave in and too small to be worth
# changing published estimates over, but it should be a known 17 rather
# than an assumed zero.
#
# NOTE on the !is.na(jttpewt_any) filters below: they are INERT. The
# loader coalesces the column so it is never NA, and the filter drops
# nothing. They are left in place deliberately, because replacing them
# with the live test (training_module_asked == 1, or !is.na of the _obs
# twin) would drop those 17 rows and move published estimates in the
# fourth decimal for no substantive gain. Do not read them as
# non-response handling; the handling is the employed == 1 beside them.
assert_training_gate(c("jttpewt_any", "jttpeot_any"),
                     context = "T2-T6 employed-only frames",
                     employed_frame = TRUE, data = panel)

prof <- ev %>%
  filter(treated == 1, k >= K_LO, k <= K_HI, employed == 1,
         !is.na(jttpewt_any)) %>%
  group_by(k, female) %>%
  summarise(n = n(), rate = mean(jttpewt_any), .groups = "drop") %>%
  pivot_wider(names_from = female, values_from = c(n, rate)) %>%
  arrange(k) %>%
  mutate(gap = round(rate_1 - rate_0, 4),
         d_gap = round(gap - lag(gap), 4))
cat("Employer-funded training rate by event time (employed only):\n")
print(as.data.frame(prof %>% mutate(across(c(rate_0, rate_1), ~round(.x, 4)))),
      row.names = FALSE)
write.csv(prof, file.path(OUTDIR, "T2_profile_by_event_time.csv"),
          row.names = FALSE)

pre_rows <- prof %>% filter(k < 0)
if (nrow(pre_rows)) {
  cat(sprintf("\nPre-birth range: women %.3f to %.3f, men %.3f to %.3f.\n",
              min(pre_rows$rate_1), max(pre_rows$rate_1),
              min(pre_rows$rate_0), max(pre_rows$rate_0)))
  cat("The two levels are far apart, which is why the pooled base rate is\n")
  cat("the wrong denominator for a statement about mothers.\n")
}

# How much of the movement lands one wave late. This is the straddling
# signature the twelve-month reference period predicts.
gd <- function(kk) { v <- prof$d_gap[prof$k == kk]; if (length(v)) v else NA_real_ }
d0 <- gd(0L); d1 <- gd(1L)
cat(sprintf("\nd_gap at k = 0: %+.4f | at k = +1: %+.4f\n", d0, d1))
if (!is.na(d0) && !is.na(d1) && sign(d0) == sign(d1) && (d0 + d1) != 0) {
  cat(sprintf("Share of the two-wave movement landing one wave later: %.0f%%\n",
              100 * d1 / (d0 + d1)))
  cat("A material share here is the straddling signature, and it is the\n")
  cat("reason the birth-wave coefficient alone understates the response.\n")
} else if (!is.na(d0) && !is.na(d1)) {
  cat("The two changes do not share a sign, so the two-wave share is not\n")
  cat("informative; read the profile directly.\n")
}

# Never-treated placebo, in calendar time. It should be flat. If it moves,
# some of the raw event-time movement above is the survey rather than the
# birth (R4).
nt_rate <- ev %>%
  filter(treated == 0, employed == 1, !is.na(jttpewt_any)) %>%
  group_by(wave, female) %>%
  summarise(nt = mean(jttpewt_any), n = n(), .groups = "drop")
nt_wide <- nt_rate %>%
  select(wave, female, nt) %>%
  pivot_wider(names_from = female, values_from = nt, names_prefix = "female_")
cat("\nNever-treated employer-funded training rate by wave:\n")
print(as.data.frame(nt_wide %>% mutate(across(-wave, ~round(.x, 3)))),
      row.names = FALSE)
if ("female_1" %in% names(nt_wide)) {
  cat(sprintf("Range for never-treated women: %.3f to %.3f\n",
              min(nt_wide$female_1, na.rm = TRUE),
              max(nt_wide$female_1, na.rm = TRUE)))
  sl <- tryCatch(unname(stats::coef(stats::lm(female_1 ~ wave, data = nt_wide))[2]),
                 error = function(e) NA_real_)
  sl_m <- tryCatch(unname(stats::coef(stats::lm(female_0 ~ wave, data = nt_wide))[2]),
                   error = function(e) NA_real_)
  cat(sprintf("Linear drift: women %+.4f per wave, men %+.4f per wave.\n", sl, sl_m))
  
  # J8. THE SAME DRIFT WITHOUT THE REFRESHER WAVE. Wave 24 carries a
  # sample refresher, which the loader flags as a composition break and
  # records in educ_trend_comparable. A calendar drift estimated through
  # it is partly the new intake, and this series shows exactly that: on
  # the first v21.5 run the never-treated female rate ran 0.257 at wave
  # 23 and 0.296 at wave 24, so a single wave carried most of the
  # end-to-end movement while the male series FELL over the same step.
  #
  # This is a diagnostic split, not a design change. Nothing below drops
  # the wave. What it decides is whether the sentence about a sex-specific
  # calendar path is about a path or about an intake.
  .nt_pre <- nt_wide[nt_wide$wave < STUDY_ITEM_BREAK_WAVE, , drop = FALSE]
  if (nrow(.nt_pre) >= 4) {
    .sl_f2 <- tryCatch(unname(stats::coef(stats::lm(female_1 ~ wave,
                                                    data = .nt_pre))[2]),
                       error = function(e) NA_real_)
    .sl_m2 <- tryCatch(unname(stats::coef(stats::lm(female_0 ~ wave,
                                                    data = .nt_pre))[2]),
                       error = function(e) NA_real_)
    cat(sprintf(paste0("Excluding wave %d, the refresher: women %+.4f, ",
                       "men %+.4f per wave.\n"),
                STUDY_ITEM_BREAK_WAVE, .sl_f2, .sl_m2))
    if (all(is.finite(c(sl, sl_m, .sl_f2, .sl_m2)))) {
      .d_all <- sl - sl_m
      .d_pre <- .sl_f2 - .sl_m2
      cat(sprintf(paste0("Differential drift: %+.5f per wave with wave %d, ",
                         "%+.5f without.\n"),
                  .d_all, STUDY_ITEM_BREAK_WAVE, .d_pre))
      if (abs(.d_all) > 0 && abs(.d_pre) < 0.5 * abs(.d_all))
        cat("  More than half of the differential is the refresher wave.\n",
            "  Whatever the significance test below concludes, the drift\n",
            "  this correction is named after is mostly one wave of new\n",
            "  respondents rather than a path through calendar time.\n",
            sep = "")
    }
    rm(.sl_f2, .sl_m2)
  }
  rm(.nt_pre)
  # -------------------------------------------------------------------
  # THE DRIFT IS SEX-SPECIFIC, AND COMMON WAVE FIXED EFFECTS DO NOT
  # ABSORB IT (R10)
  #
  # The previous version of this block asserted that wave fixed effects
  # absorb the placebo drift in every regression below. That is false as
  # stated, and it is the script contradicting its own diagnostic. A wave
  # fixed effect absorbs whatever is COMMON to a wave. The drift measured
  # here is steeper for women than for men, and female is time-invariant
  # and therefore already absorbed by the person effect, so nothing in
  # person_id + wave removes a sex-specific calendar path. What removes it
  # is wave^female, which is nearly free: post and post_f stay identified
  # off variation in event timing across cohorts within a sex-by-wave cell,
  # which is the identification the staggered design claims anyway.
  #
  # Direction, IF the drift is established. Women's never-treated rate
  # rises faster in this sample, and the post period is later in calendar
  # time by construction, so an uncorrected post_f would be biased toward
  # zero and the true fall larger than reported. Whether the drift is
  # distinguishable from zero at all is tested below and the direction
  # claim is printed only when it is (F7). Revision 2 stated it here as
  # settled and then refused to attribute anything to it eighty lines
  # later, which is the script contradicting itself in the same run.
  # -------------------------------------------------------------------
  if (is.finite(sl) && is.finite(sl_m)) {
    dd <- sl - sl_m
    span <- diff(range(nt_wide$wave))
    cat(sprintf("Differential drift in the female-minus-male gap: %+.5f per wave,\n", dd))
    cat(sprintf("  %+.4f over the %d-wave span.\n", dd * span, span))
    # The composition test, run here because both drifts are already in hand.
    # Selective attrition acts on men and women alike, so it moves the two
    # series together; it cannot produce a divergence on its own.
    if (all(c("female_0", "female_1") %in% names(nt_wide))) {
      m_first <- nt_wide$female_0[which.min(nt_wide$wave)]
      m_last  <- nt_wide$female_0[which.max(nt_wide$wave)]
      f_first <- nt_wide$female_1[which.min(nt_wide$wave)]
      f_last  <- nt_wide$female_1[which.max(nt_wide$wave)]
      # Named so they cannot collide with anything at top level (F12).
      # Revision 2 bound dm and df here and then bound dm to a function and
      # df to an absolute miss inside T8.
      move_m <- m_last - m_first; move_f <- f_last - f_first
      move_ratio <- if (abs(move_f) > 0) abs(move_m) / abs(move_f) else NA_real_
      cat(sprintf("End to end: men %+.3f, women %+.3f, ratio %.2f.\n",
                  move_m, move_f, move_ratio))
      # Three bands, not two (R17). A single threshold turns a continuous
      # ratio into a categorical verdict, and on a ratio near the cut the
      # verdict flips on the third decimal of a mean. The middle band says
      # the test cannot discriminate rather than picking a side.
      if (is.na(move_ratio))
        cat("  The female series does not move, so the comparison is empty.\n")
      else if (move_ratio < 0.33)
        cat("  Men move less than a third as far, so the differential is not\n",
            " selective attrition: that acts on both sexes and would move both.\n")
      else if (move_ratio > 0.67)
        cat("  Both series move by comparable amounts, which is what a common\n",
            " compositional shift produces. Composition cannot be ruled out\n",
            " for the level.\n")
      else
        cat("  Men move appreciably but less than women. This test cannot\n",
            " discriminate at that ratio: partial attrition selection and a\n",
            " genuinely sex-specific path both produce it. Do not report a\n",
            " conclusion either way from this comparison alone.\n")
      cat("  Either way wave^female removes the differential, so no estimate\n",
          " below turns on the answer.\n")
      
      # The decisive test, which the ratio above is only a proxy for (R18).
      # Composition can only move a series by changing WHO is in it. Restrict
      # to persons present in every wave and composition is held fixed by
      # construction, so any drift that survives is behaviour. This answers
      # the question directly rather than inferring it from how the two
      # sexes move relative to each other.
      n_all <- dplyr::n_distinct(ev$wave)
      bal_ids <- ev %>% count(person_id) %>% filter(n == n_all) %>%
        pull(person_id)
      cat(sprintf("\nBalanced panel: %s of %s never-treated persons are present\n",
                  format(dplyr::n_distinct(
                    ev$person_id[ev$treated == 0 & ev$person_id %in% bal_ids]),
                    big.mark = ","),
                  format(dplyr::n_distinct(ev$person_id[ev$treated == 0]),
                         big.mark = ",")))
      cat(sprintf("in all %d waves. Composition is fixed within this group, so a\n",
                  n_all))
      cat("drift here cannot be attrition.\n")
      
      # Fitted at PERSON-WAVE level with clustered errors, not as a slope
      # through thirteen wave means (R19). The first version reported a
      # retained fraction with no standard error on either side of the
      # ratio. The balanced panel here is a small fraction of the
      # never-treated, so its differential slope is imprecise, and a ratio
      # whose numerator cannot be distinguished from zero cannot support a
      # verdict in either direction. This is the same fault the T8 shrinkage
      # rule had before its detectability gate was added.
      drift_fit <- function(d) {
        if (dplyr::n_distinct(d$person_id) < 50) return(c(est = NA, se = NA))
        # The interaction is built as a column rather than left to formula
        # expansion, so the term name is known rather than pattern-matched
        # out of the coefficient table.
        d$wave_female <- d$wave * d$female
        m <- tryCatch(fixest::feols(jttpewt_any ~ wave + female + wave_female,
                                    data = d, cluster = ~person_id,
                                    notes = FALSE, warn = FALSE),
                      error = function(e) NULL)
        if (is.null(m)) return(c(est = NA, se = NA))
        ct <- fixest::coeftable(m)
        if (!("wave_female" %in% rownames(ct))) return(c(est = NA, se = NA))
        c(est = unname(ct["wave_female", "Estimate"]),
          se  = unname(ct["wave_female", "Std. Error"]))
      }
      nt_full_pw <- ev %>% filter(treated == 0, employed == 1,
                                  !is.na(jttpewt_any))
      nt_bal_pw  <- nt_full_pw %>% filter(person_id %in% bal_ids)
      ff <- drift_fit(nt_full_pw); bb <- drift_fit(nt_bal_pw)
      cat(sprintf("  full panel     : differential %+.5f (SE %.5f), n = %s\n",
                  ff["est"], ff["se"], format(nrow(nt_full_pw), big.mark = ",")))
      cat(sprintf("  balanced panel : differential %+.5f (SE %.5f), n = %s\n",
                  bb["est"], bb["se"], format(nrow(nt_bal_pw), big.mark = ",")))
      if (all(is.finite(c(ff["est"], ff["se"], bb["est"], bb["se"])))) {
        zcrit <- stats::qnorm(1 - ALPHA / 2)
        z_ful <- abs(ff["est"]) / ff["se"]
        z_bal <- abs(bb["est"]) / bb["se"]
        # T2c cites this drift as the reason for its correction, so it needs
        # to know whether the drift is established (R20).
        .drift_z   <<- unname(z_ful)
        .drift_est <<- unname(ff["est"])
        .drift_se  <<- unname(ff["se"])
        # BOTH sides of the ratio have to be detectable. A denominator near
        # zero makes the retained fraction arbitrarily large or small
        # regardless of the numerator, which is how a synthetic test of this
        # block produced -5673%.
        if (z_ful < zcrit) {
          cat(sprintf("  The FULL-panel differential is %.1f standard errors from\n",
                      z_ful))
          cat("  zero, so there is no established drift to attribute. The\n")
          cat("  retained fraction is not computed: its denominator is noise.\n")
        } else if (z_bal < zcrit) {
          cat(sprintf("  The balanced differential is %.1f standard errors from zero,\n",
                      z_bal))
          cat("  so this panel cannot say whether the drift survives with\n")
          cat("  composition held fixed. The balanced group is a small and\n")
          cat("  unusual subset, and the test needs a larger one to resolve.\n")
          cat("  Report the drift as unattributed. No estimate depends on it.\n")
        } else {
          keep <- bb["est"] / ff["est"]
          cat(sprintf("  the balanced panel retains %.0f%% of the differential,\n",
                      100 * keep))
          cat(sprintf("  and that differential is %.1f standard errors from zero.\n",
                      z_bal))
          if (keep > 0.67)
            cat("  Most survives with composition fixed, so the drift is\n",
                " behaviour and the text can say so.\n")
          else if (keep < 0.33)
            cat("  Most disappears, so the drift is attrition and the\n",
                " never-treated series is not a clean calendar control.\n")
          else
            cat("  About half survives, so it is part behaviour and part\n",
                " composition. Report it as mixed.\n")
        }
      }
    }
    cat("A wave fixed effect absorbs what is common to a wave. This drift is\n")
    cat("NOT common to a wave: it differs by sex, and sex is absorbed by the\n")
    cat("person effect, so person_id + wave leaves it in. wave^female removes\n")
    cat("it. Every preferred specification below now carries that term, and\n")
    cat("T2c reports what it changes.\n")
    note(sprintf(paste("T2: the never-treated drift is sex-specific,",
                       "%+.5f per wave in the gap. Common wave FE do not",
                       "absorb it; wave^female does. See T2c."), dd))
  }
  cat("The raw event-time profile does not absorb any of it, because event\n")
  cat("time and calendar time are correlated across cohorts.\n")
}

# The same profile, each treated person-wave differenced against the
# never-treated rate of the same sex in the same wave. This is the raw
# picture with calendar time removed, and it is the one to plot.
prof_adj <- ev %>%
  filter(treated == 1, k >= K_LO, k <= K_HI, employed == 1,
         !is.na(jttpewt_any)) %>%
  left_join(nt_rate %>% select(wave, female, nt), by = c("wave", "female")) %>%
  filter(!is.na(nt)) %>%
  mutate(y_adj = jttpewt_any - nt) %>%
  group_by(k, female) %>%
  summarise(n = n(), adj = mean(y_adj), .groups = "drop") %>%
  pivot_wider(names_from = female, values_from = c(n, adj)) %>%
  arrange(k) %>%
  mutate(gap_adj = round(adj_1 - adj_0, 4),
         d_gap_adj = round(gap_adj - lag(gap_adj), 4))
cat("\nSame profile net of the never-treated rate in the same wave:\n")
print(as.data.frame(prof_adj %>% mutate(across(c(adj_0, adj_1), ~round(.x, 4)))),
      row.names = FALSE)
write.csv(prof_adj, file.path(OUTDIR, "T2_profile_wave_adjusted.csv"),
          row.names = FALSE)
cat("Compare gap_adj with gap above. Where they differ, the difference is\n")
cat("calendar drift, and the adjusted series is the one to show.\n")

# ---------------------------------------------------------------------
# T2b. THE TWO CLOCKS (R1)
# ---------------------------------------------------------------------
ev_shift <- ev %>%
  mutate(k_shift = ifelse(treated == 1, k + 1L, NA_integer_),
         post_shift = ifelse(treated == 1, as.integer(k_shift >= 0), 0L),
         post_shift_f = post_shift * female)
d <- ev_shift %>% filter(employed == 1, !is.na(jttpewt_any))
m_nat <- fixest::feols(stats::as.formula(paste0("jttpewt_any ~ post + post_f | ", fe_block())),
                       data = d, cluster = ~person_id, notes = FALSE, warn = FALSE)
report("T2_clock_natural", "employer-funded, natural clock", m_nat,
       "post_f", "pp", bm_pewt, bm_f_pewt)
m_shf <- fixest::feols(stats::as.formula(paste0("jttpewt_any ~ post_shift + post_shift_f | ", fe_block())),
                       data = d, cluster = ~person_id, notes = FALSE, warn = FALSE)
report("T2_clock_shifted", "employer-funded, clock shifted one wave",
       m_shf, "post_shift_f", "pp", bm_pewt, bm_f_pewt)

# The comparison of the two coefficients IS the diagnostic, so blinding
# both of them withholds the design decision rather than protecting it.
# The decision rule was fixed in the configuration above, before the
# numbers were seen, which is what the blinding was there to guarantee.
if (RELEASE_CLOCK) {
  cn <- fixest::coeftable(m_nat)["post_f", ]
  cs <- fixest::coeftable(m_shf)["post_shift_f", ]
  cat("\nClock comparison, released from BLIND under the pre-set rule:\n")
  cat(sprintf("  natural clock : %+.4f (SE %.4f)\n",
              unname(cn["Estimate"]), unname(cn["Std. Error"])))
  cat(sprintf("  shifted clock : %+.4f (SE %.4f)\n",
              unname(cs["Estimate"]), unname(cs["Std. Error"])))
  ratio <- abs(unname(cs["Estimate"])) / abs(unname(cn["Estimate"]))
  cat(sprintf("  ratio of magnitudes: %.3f | difference: %+.4f\n",
              ratio, unname(cs["Estimate"]) - unname(cn["Estimate"])))
  cat("  The two are estimated on the same rows and are strongly\n")
  cat("  correlated, so this is a descriptive comparison and not a test.\n")
  if (!is.na(ratio) && ratio > 1 + SHIFT_MATERIAL) {
    cat(sprintf("  VERDICT: the shifted clock is more than %.0f%% larger. The\n",
                100 * SHIFT_MATERIAL))
    cat("  twelve-month reference window is doing real work, and the birth\n")
    cat("  cannot be placed inside or outside it without the interview\n")
    cat("  month. Extract hhhqivw and proceed with Block H.\n")
    note("T2: shifted clock materially larger. Extract hhhqivw, run Block H.")
  } else {
    cat(sprintf("  VERDICT: the shifted clock is not more than %.0f%% larger.\n",
                100 * SHIFT_MATERIAL))
    cat("  The reference window is not driving the birth-wave coefficient.\n")
    cat("  Close the concern in a footnote and skip Block H entirely.\n")
    note("T2: shifted clock not materially larger. Skip Block H; footnote the bound.")
  }
}

cat("\nThis is a bound, not a fix. Dating the birth within the reference\n")
cat("window needs the interview month, which the loader does not read;\n")
cat("see the interview_date keyword set in 01_discovery_extended.R.\n")

# ---------------------------------------------------------------------
# T2c. WHAT THE SEX-BY-WAVE TERM CHANGES (R10)
#
# v21.31. WHAT THIS BLOCK MUST NOT BE READ AS SAYING.
# PAPER_ATTENTION.md section 2: "Do not write that the common-wave
# specification was confounded."
#
# The term moves post_f by -47.5% at a precision cost of 1.13x, which
# looks like a correction. It is not one. The never-treated differential
# drift that motivated it is +0.00174 with SE 0.00150, and 04's split of
# all 18 flagged level steps by sex finds NONE clearly differential at
# |z| >= 2.5.
#
# So the two specifications identify different contrasts rather than one
# being the corrected version of the other: person_id + wave compares
# women with men net of common wave effects; person_id + wave^female
# compares treated women with other women in the same wave. Choose on
# identification grounds and report both. The numbers below are the size
# of the difference, not evidence for either.
#
# The correction above is asserted on the placebo drift. Whether it matters
# is an empirical question, and the comparison IS the design decision, so
# it is released from blinding under the same pre-set rule the clock
# comparison uses: withholding both coefficients would withhold the
# decision rather than protect it.
#
# Read the direction. Women's never-treated rate rises faster than men's
# and the post period is later in calendar time, so the uncorrected
# coefficient should be biased toward zero. If the corrected one is LARGER
# in magnitude, that is the predicted sign and the correction is doing what
# the placebo said it would.
# ---------------------------------------------------------------------
cat("\n--- T2c. The sex-specific calendar trend ---\n")
m_common <- fixest::feols(jttpewt_any ~ post + post_f | person_id + wave,
                          data = d, cluster = ~person_id,
                          notes = FALSE, warn = FALSE)
m_sexwv  <- fixest::feols(jttpewt_any ~ post + post_f | person_id + wave^female,
                          data = d, cluster = ~person_id,
                          notes = FALSE, warn = FALSE)
est_of_m <- function(m, term) tryCatch(
  unname(fixest::coeftable(m)[term, "Estimate"]),
  error = function(e) NA_real_)
b_common <- est_of_m(m_common, "post_f")
b_sexwv  <- est_of_m(m_sexwv,  "post_f")
# Taken from fixest's own coefficient table, the same source report() reads.
# stats::se() does not exist, so the first version of this block silently
# returned NA and printed "SE NA" and a NA precision cost (R11).
se_of <- function(m, term) tryCatch(
  unname(fixest::coeftable(m)[term, "Std. Error"]),
  error = function(e) NA_real_)
se_common <- se_of(m_common, "post_f")
se_sexwv  <- se_of(m_sexwv,  "post_f")
report("T2c_fe_common", "employer-funded, common wave FE", m_common,
       "post_f", "pp", bm_pewt, bm_f_pewt, status = "diagnostic")
report("T2c_fe_sex_wave", "employer-funded, sex-by-wave FE", m_sexwv,
       "post_f", "pp", bm_pewt, bm_f_pewt, status = "PREFERRED")
if (is.finite(b_common) && is.finite(b_sexwv) && b_common != 0) {
  mv <- (b_sexwv - b_common) / abs(b_common)
  cat("\nReleased from BLIND under the pre-set rule, because the comparison\n")
  cat("is the design decision:\n")
  cat(sprintf("  common wave FE   : %+.4f (SE %.4f)\n", b_common, se_common))
  cat(sprintf("  sex-by-wave FE   : %+.4f (SE %.4f)\n", b_sexwv, se_sexwv))
  cat(sprintf("  change: %+.1f%% of the uncorrected magnitude", 100 * mv))
  cat(sprintf(" | precision cost %.2fx\n", se_sexwv / se_common))
  if (abs(mv) >= FE_MOVE_MATERIAL) {
    cat(sprintf("  VERDICT: the move exceeds the pre-set %.0f%%.\n",
                100 * FE_MOVE_MATERIAL))
    # Whether the move can be ATTRIBUTED to a sex-specific drift is a
    # separate question from whether it is large, and the drift block
    # answers it (R20). Naming a mechanism the data has not established
    # is the failure this script exists to prevent.
    if (is.finite(.drift_z) && .drift_z >= stats::qnorm(1 - ALPHA / 2)) {
      cat(sprintf("  The never-treated differential drift is %+.5f (SE %.5f),\n",
                  .drift_est, .drift_se))
      cat(sprintf("  %.1f SE from zero, so the common-wave specification was\n",
                  .drift_z))
      cat("  confounded by it. Report the sex-by-wave version and say so.\n")
      note(paste("T2c: the sex-by-wave term moves post_f materially and the",
                 "drift motivating it is established. Report the corrected",
                 "specification."))
    } else if (is.finite(.drift_z)) {
      cat(sprintf("  BUT the never-treated differential drift is %+.5f (SE %.5f),\n",
                  .drift_est, .drift_se))
      cat(sprintf("  only %.1f SE from zero. The mechanism this correction was\n",
                  .drift_z))
      cat("  motivated by is NOT established on this panel, so the move is\n")
      cat("  unexplained by its stated reason. Do not write that the\n")
      cat("  common-wave specification was confounded by a sex-specific\n")
      cat("  calendar path. What remains true is that the two specifications\n")
      cat("  identify different contrasts: person_id + wave compares women\n")
      cat("  with men net of common wave effects, person_id + wave^female\n")
      cat("  compares treated women with other women in the same wave.\n")
      cat("  Choose on identification grounds, report both, and state the\n")
      cat("  size of the difference.\n")
      note(paste("T2c: the sex-by-wave term moves post_f materially, but the",
                 "never-treated drift motivating it is not distinguishable",
                 "from zero. Report both specifications and choose on",
                 "identification grounds, not on the drift."))
    } else {
      cat("  Report the sex-by-wave version and state the move.\n")
      note("T2c: the sex-by-wave term moves post_f materially. Report both specifications.")
    }
  } else {
    cat(sprintf("  VERDICT: the move is under the pre-set %.0f%%, so the drift\n",
                100 * FE_MOVE_MATERIAL))
    cat("  does not carry the coefficient. Keep the corrected specification\n")
    cat("  anyway, since it costs almost nothing, and report the comparison\n")
    cat("  in a footnote as the answer to the placebo objection.\n")
    note(paste("T2c: the sex-by-wave term leaves post_f essentially unchanged.",
               "Keep it and footnote the comparison."))
  }
  if (is.finite(se_common) && is.finite(se_sexwv) && se_common > 0) {
    cat(sprintf(paste("  The same precision cost, about %.2fx, applies to every",
                      "specification\n  carrying the term."),
                se_sexwv / se_common))
    cat(" Designs whose MDE sat near a benchmark can\n")
    cat("  therefore flip verdict, so read the ledger AFTER the correction and\n")
    cat("  not the pre-correction run. The fe column in the ledger records\n")
    cat("  which block each row used.\n")
  }
  # The word "predicts" requires a drift to predict from, so the sign
  # sentence is gated on the same test the attribution above is gated on
  # (F7). Revision 2 printed it unconditionally, three lines after
  # declining to attribute the move to the drift at all.
  drift_established <- is.finite(.drift_z) &&
    .drift_z >= stats::qnorm(1 - ALPHA / 2)
  if (drift_established) {
    if (abs(b_sexwv) > abs(b_common))
      cat("  The corrected coefficient is LARGER in magnitude, which is the\n  sign the established drift predicts.\n")
    else
      cat("  The corrected coefficient is SMALLER in magnitude, which is NOT\n  the sign the established drift predicts. Check the drift by cohort\n  before reading either as the answer.\n")
  } else {
    cat(sprintf("  The corrected coefficient is %s in magnitude. With the\n",
                if (abs(b_sexwv) > abs(b_common)) "LARGER" else "SMALLER"))
    cat("  never-treated differential drift indistinguishable from zero there\n")
    cat("  is nothing here that predicts a sign, so read this as the size of\n")
    cat("  the difference between two identification strategies and not as a\n")
    cat("  confirmation of the placebo argument.\n")
  }
}

# =====================================================================
# T4. FULL-TIME RETURNERS
# =====================================================================
cat("\n--- T4. Full-time returners ---\n")

# Three definitions of full-time are in play across these scripts and they
# are not the same: the loader builds fulltime from HILDA's esdtl status
# code, the fallback here is an hours threshold, and the FTE weight in 02
# divides by FT_HOURS = 38. Which one is in force is printed rather than
# left to be inferred (F12).
if (!"fulltime" %in% names(ev)) {
  ev$fulltime <- ifelse(is.na(ev$hours_worked_clean), NA_real_,
                        as.numeric(ev$hours_worked_clean >= 35))
  cat("fulltime absent from the panel; built here as hours_worked_clean >= 35.\n")
  cat("  That is NOT the loader's definition (esdtl status code) and not the\n")
  cat("  FTE weight's reference week (FT_HOURS = 38). Reconcile before\n")
  cat("  quoting T4 beside anything from 02.\n")
  note("T4: fulltime was built from a 35-hour threshold here, not the loader's esdtl rule. Reconcile the definition.")
} else {
  cat("fulltime taken from the panel (loader definition, esdtl status code).\n")
}
ret_any <- ev %>% filter(treated == 1, k >= 1, k <= 5, fulltime == 1) %>%
  distinct(person_id) %>% pull(person_id)
# Stricter: full-time at EVERY observed horizon from +1 to +3, so the
# comparison is not contaminated by a single full-time wave.
ret_all <- ev %>% filter(treated == 1, k >= 1, k <= 3, !is.na(fulltime)) %>%
  group_by(person_id) %>%
  summarise(n_k = n(), n_ft = sum(fulltime == 1), .groups = "drop") %>%
  filter(n_k >= 2, n_ft == n_k) %>% pull(person_id)

for (nm in c("any", "all")) {
  ids <- if (nm == "any") ret_any else ret_all
  n_mo <- dplyr::n_distinct(ev$person_id[ev$person_id %in% ids & ev$female == 1])
  cat(sprintf("  returners (%s rule): %s persons, %s mothers\n", nm,
              format(length(ids), big.mark = ","), format(n_mo, big.mark = ",")))
  d4 <- ev %>% filter(employed == 1, !is.na(jttpewt_any),
                      person_id %in% ids | treated == 0)
  m <- fixest::feols(stats::as.formula(paste0("jttpewt_any ~ post + post_f | ", fe_block())),
                     data = d4, cluster = ~person_id, notes = FALSE, warn = FALSE)
  report(paste0("T4_", nm), paste("employer-funded, full-time returners", nm),
         m, "post_f", "pp", bm_pewt, bm_f_pewt,
         status = if (nm == "any") "PREFERRED" else "secondary")
}
# v23. THE "CLOSED" CLAIM IS WITHDRAWN. This block used to say that
# survival here closes the part-time composition story. It does not, and the
# reason is in T4's own construction: BOTH rules select on full-time status
# AFTER the birth, k = +1..+5 and k = +1..+3. That is selection on an
# outcome. Mothers observed back on full-time hours are not a random
# subsample of mothers; they are the ones for whom returning was feasible,
# and whatever made it feasible - a job that held, an employer that
# accommodated, a partner's hours - can bear on training directly. Survival
# in T4 is evidence. It is not closure.
cat("If the coefficient survives here the part-time composition story is\n")
cat("NARROWED, not closed: both rules select on full-time status AFTER the\n")
cat("birth, which is selection on an outcome. Report the 'any' rule, which\n")
cat("retains enough mothers to be informative, mention the strict rule only\n")
cat("as a direction-of-travel check (R6), and read T4c below beside both.\n")
note("T4: report the 'any' returner rule; the strict rule is too thin to carry a null.")
note(paste("T4: both returner rules select on POST-birth full-time status.",
           "Cite T4c, which restricts on pre-birth status, beside them."))

# ---------------------------------------------------------------------
# T4c. THE PRE-BIRTH RESTRICTION
#
# STATUS: POST-HOC. Designated AFTER the blind ledger was fixed and after
# T4's coefficients had been seen. That is recorded here rather than
# quietly, because the whole value of the ledger is that a reader can tell
# which specifications were chosen in advance, and an arm added later does
# not acquire that property by sitting next to ones that were. It is
# reported as secondary and must not be described as pre-committed.
#
# WHY IT IS ADDED AT ALL. TRAINING_PAPER_OUTLINE section 7 names hours as
# the one alternative the timing argument cannot dismiss: a mother working
# part time trains less AND accumulates less experience, so training and
# wages move in that order with no employer decision anywhere. T4 addresses
# it and, as above, addresses it with a post-treatment restriction.
#
# WHAT CHANGES HERE. Full-time status in k = -3..-1 is fixed BEFORE
# treatment, so restricting on it is conditioning and not selection. The
# question it answers is the one a referee asks: among women who were
# attached full time going in, does the employer-funded gap still open?
#
# WHY NOT A CONTROL. Adding fulltime or hours to the right-hand side is a
# bad control in the Angrist-Pischke sense - hours respond to the birth, so
# conditioning on them blocks part of the causal path and can induce bias
# with no sign guarantee. Attenuation under such a control would not be
# evidence that hours explain the gap. A restriction on a pre-determined
# value has no such problem, which is why this is a sample rule and not a
# regressor.
#
# READ T4c AND T4_any AS A PAIR. T4 shows the gap surviving among mothers
# who RETURNED to full time; T4c among mothers who WERE full time going in.
# Only the second is free of selection on an outcome. Two restrictions
# failing to kill the coefficient for different reasons is worth more than
# either alone, and the pair is what section 9.3 should cite.
if ("fulltime" %in% names(ev)) {
  pre_ft <- ev %>%
    filter(treated == 1, employed == 1, k >= -3, k <= -1, !is.na(fulltime)) %>%
    group_by(person_id) %>%
    summarise(n_k = n(), n_ft = sum(fulltime == 1), .groups = "drop") %>%
    filter(n_k >= 2, n_ft == n_k) %>% pull(person_id)
  n_mo_pre <- dplyr::n_distinct(ev$person_id[ev$person_id %in% pre_ft &
                                               ev$female == 1])
  cat(sprintf("\n  pre-birth full time (k = -3..-1, all observed waves): %s persons, %s mothers\n",
              format(length(pre_ft), big.mark = ","),
              format(n_mo_pre, big.mark = ",")))
  if (n_mo_pre < 100) {
    cat("  Too few mothers to carry a null. If the coefficient is\n")
    cat("  insignificant here, read the MDE before calling it one: a\n")
    cat("  pre-birth restriction on a three-wave window is a small\n")
    cat("  subsample and an underpowered arm is not evidence of absence.\n")
    note("T4c: fewer than 100 mothers; the arm cannot carry a null on its own.")
  }
  d4c <- ev %>% filter(employed == 1, !is.na(jttpewt_any),
                       person_id %in% pre_ft | treated == 0)
  m4c <- fixest::feols(
    stats::as.formula(paste0("jttpewt_any ~ post + post_f | ", fe_block())),
    data = d4c, cluster = ~person_id, notes = FALSE, warn = FALSE)
  report("T4c_pre_birth_ft", "employer-funded, full time BEFORE the birth",
         m4c, "post_f", "pp", bm_pewt, bm_f_pewt, status = "secondary (post-hoc)")
  cat("  T4c restricts on a PRE-treatment value, so it is conditioning and\n")
  cat("  not selection. It is the arm to cite when the hours alternative is\n")
  cat("  raised. It was designated post-hoc; say so.\n")
  
  # WHAT THE FIRST RUN SHOWED, and it is the argument for T4c in one line.
  #
  #   T4_any   1,567 persons,  538 mothers,  SE 0.0191
  #   T4_all     894 persons,  155 mothers,  SE 0.0282  LIMITED
  #   T4c      1,574 persons,  741 mothers,  SE 0.0182  OK
  #
  # T4c has MORE mothers than either returner rule on essentially the same
  # n. That is the selection problem made arithmetic: the post-birth rules
  # drop the 200-plus mothers who reduced hours after the birth, which is
  # precisely the group the hours alternative is about. Restricting on
  # pre-birth status keeps them and asks whether the gap opens anyway.
  #
  # It is also better powered than T4_all, which the ledger grades LIMITED
  # on 155 mothers. A pre-birth window that looks more demanding is in fact
  # less costly, because it does not condition on an outcome that the
  # treatment moves.
  cat(sprintf(paste0("  Mothers: T4c %s vs T4_any %s. The pre-birth rule KEEPS\n",
                     "  the mothers who reduced hours; the post-birth rules drop\n",
                     "  them, and they are the group the alternative is about.\n"),
              format(n_mo_pre, big.mark = ","),
              format(dplyr::n_distinct(ev$person_id[ev$person_id %in% ret_any &
                                                      ev$female == 1]),
                     big.mark = ",")))
} else {
  cat("\n  T4c SKIPPED: `fulltime` is not on the event frame, so the\n")
  cat("  pre-birth restriction cannot be built. Section 9.3 then rests on\n")
  cat("  T4 alone, which selects on POST-birth status. State that limit\n")
  cat("  rather than letting T4 stand as though it closed the question.\n")
  note("T4c: fulltime absent; the hours alternative is narrowed but not answered.")
}

# =====================================================================
# T5. INCIDENCE AGAINST DOSAGE
#
# total_training_hours is days x hours and is structurally missing for
# everyone coded as not training, so the intensive margin as it stands is
# observed only on the trained and is therefore selected. Recoding the
# structural missings to zero recovers the unconditional dosage margin,
# which is the one that combines with incidence.
# =====================================================================
cat("\n--- T5. Incidence against dosage ---\n")

# =====================================================================
# T5b. THE COURSE COUNT, WHICH IS A CLEANER DOSAGE THAN HOURS
#
# total_training_hours is jttdays x jtthrs, a PRODUCT of two self-reports,
# so its measurement error is multiplicative and its tail is a reporting
# problem in one component: the C1 diagnostic shows implausible cases
# sitting at hours-per-day above 16 and days above 260, not at extreme
# products of plausible parts.
#
# total_courses is jttnum, ONE self-report of a small count. No product,
# no tail, additive error. It has been on the panel since v17 and nothing
# in this file uses it, while the master does. The C4 diagnostic reports
# the courses gap resolving where the hours gap cannot, which is what a
# cleaner measure of the same quantity should do.
#
# So it is run beside the hours dosage rather than instead of it. Two
# measures of intensity agreeing is evidence; one of them agreeing with
# itself is not. And the interpretation differs: courses is EPISODES and
# hours is TIME, so a fall in courses with flat hours means fewer, longer
# spells rather than less training.
if (any(c("total_courses_z", "total_courses") %in% names(ev))) {
  # v21.48. THE LOADER SHIPS THE GATED COUNT NOW, as total_courses_z, so
  # the definition lives in one place, as it has for hours since v21.9.
  # The local gate below handled the never-asked and missed two cases the
  # loader column gets right. A trainee who gave no count stayed 0 here
  # and is NA there. A person-wave reporting courses with no place-and-time
  # item kept its count here and is 0 there, matching total_training_hours_z.
  # On the full 1 October panel that is 65 and 11 employed person-waves.
  if ("total_courses_z" %in% names(ev)) {
    .cz <- ev$total_courses_z
    cat("  courses: total_courses_z, gated in the loader.\n")
  } else {
    .cz <- ev$total_courses
    # Same structural-zero discipline as train_hours_z. total_courses is
    # coalesced to 0 in the loader wherever jttnum is missing, which
    # includes the never-asked, so it needs the gate applied here.
    if ("training_module_asked" %in% names(ev))
      .cz[is.na(ev$training_module_asked) | ev$training_module_asked != 1] <- NA_real_
    cat("NOTE: total_courses_z is absent, so T5b gated total_courses here.\n",
        "  A trainee who gave no count still reads 0. Rebuild on loader\n",
        "  v21.48 for the loader's rule.\n", sep = "")
  }
  ev$courses_z <- .cz
  cat(sprintf(paste("  courses: observed on %s person-waves after the module",
                    "gate\n  (%.1f%% of the frame), mean %.2f, max %s. No",
                    "product, so no\n  multiplicative tail.\n"),
              format(sum(!is.na(ev$courses_z)), big.mark = ","),
              100 * mean(!is.na(ev$courses_z)),
              mean(ev$courses_z, na.rm = TRUE),
              format(max(ev$courses_z, na.rm = TRUE))))
  .fe5 <- if (FE_SEX_WAVE) "person_id + wave^female" else "person_id + wave"
  .mc <- try(fixest::feols(stats::as.formula(paste0(
    "courses_z ~ post + post_f | ", .fe5)),
    data = ev[ev$employed == 1, , drop = FALSE],
    cluster = ~person_id, notes = FALSE, warn = FALSE), silent = TRUE)
  if (!inherits(.mc, "try-error"))
    report("T5b_courses", "mother x post, course COUNT (episodes)",
           .mc, "post_f", "courses", NA_real_, NA_real_, status = "secondary")
  cat("  Read against T5's hours result: courses is EPISODES and hours is\n")
  cat("  TIME. A fall in courses with flat hours is fewer and longer\n")
  cat("  spells; a fall in hours with flat courses is shorter ones. They\n")
  cat("  are not substitutes and the pair is more informative than either.\n")
  rm(.cz)
} else {
  cat("  [skip] T5b: neither total_courses_z nor total_courses is on this\n")
  cat("  frame. Both come from the loader; check its final select.\n")
}

if (all(c("any_training", "total_training_hours") %in% names(ev))) {
  ev <- ev %>%
    # v21.9: the loader ships this now, as total_training_hours_z, so the
    # definition lives in one place. The loader version differs from the
    # local one here in one way that matters: it leaves a NEVER-ASKED
    # person-wave as NA rather than recoding it to zero hours. The local
    # version keyed on any_training, which is coalesced, so it imputed a
    # zero dosage to person-waves the module was never put to and carried
    # the T1 structural-zero problem into T5 without saying so. Every zero
    # in the loader column is a MEASURED zero.
    mutate(train_hours_z = NA_real_)
  # Base assignment rather than a "." reference inside mutate(): the
  # branch is on whether a COLUMN EXISTS, which is a property of the
  # frame and not of a row, and data masking is the wrong tool for it.
  if ("total_training_hours_z" %in% names(ev)) {
    ev$train_hours_z <- ev$total_training_hours_z
  } else {
    ev$train_hours_z <- ifelse(!is.na(ev$any_training) & ev$any_training == 0,
                               0, ev$total_training_hours)
    cat("NOTE: total_training_hours_z is absent, so T5 rebuilt it locally\n",
        "  and its zeros include never-asked person-waves, which inherits\n",
        "  the T1 imputation. Rebuild on the v21.9 loader for measured\n",
        "  zeros only.\n", sep = "")
  }
  cov_gain <- ev %>% filter(employed == 1) %>%
    summarise(n = n(),
              pct_hours_obs_before = round(100 * mean(!is.na(total_training_hours)), 1),
              pct_hours_obs_after  = round(100 * mean(!is.na(train_hours_z)), 1))
  cat("Intensity coverage before and after recoding structural zeros:\n")
  print(as.data.frame(cov_gain), row.names = FALSE)
  cat("This is a measurement contribution in its own right and worth one\n")
  cat("sentence in the data section: the dosage margin as distributed is\n")
  cat("observed on a minority of the employed and is selected on the\n")
  cat("outcome; recoding the structural zeros recovers essentially all of\n")
  cat("them and makes the unconditional dosage margin estimable.\n")
  
  d5 <- ev %>% filter(employed == 1, !is.na(train_hours_z))
  m_unc <- fixest::feols(stats::as.formula(paste0("train_hours_z ~ post + post_f | ", fe_block())),
                         data = d5, cluster = ~person_id,
                         notes = FALSE, warn = FALSE)
  report("T5_hours_uncond", "training hours, structural zeros included",
         m_unc, "post_f", "hours", status = "PREFERRED")
  
  # The conditional-on-training version is dropped rather than footnoted
  # (R5). It is selected on the outcome AND uninformative; its MDE is
  # printed as the reason, and the estimate is not reported.
  d5b <- ev %>% filter(employed == 1, !is.na(total_training_hours),
                       total_training_hours > 0)
  if (nrow(d5b) > MIN_N) {
    m_sel <- fixest::feols(stats::as.formula(paste0("total_training_hours ~ post + post_f | ", fe_block())),
                           data = d5b, cluster = ~person_id,
                           notes = FALSE, warn = FALSE)
    ct <- tryCatch(fixest::coeftable(m_sel), error = function(e) NULL)
    if (!is.null(ct) && "post_f" %in% rownames(ct)) {
      se_sel <- unname(ct["post_f", "Std. Error"])
      .blind[["T5_hours_ontrained_WITHHELD"]] <- ct["post_f", , drop = FALSE]
      cat(sprintf("\n  conditional-on-training dosage: n=%s, MDE=%.1f hours\n",
                  format(m_sel$nobs, big.mark = ","), mde(se_sel)))
      cat("  DECISION: not reported. An MDE of that size cannot distinguish\n")
      cat("  any dosage change anyone would claim, and the sample is\n")
      cat("  selected on the outcome. Drop it from the paper rather than\n")
      cat("  footnote it; a footnote reporting nothing is still a claim.\n")
      note(sprintf("T5: drop the conditional-on-training dosage regression (MDE %.1f hours). Keep the coverage-recovery sentence.",
                   mde(se_sel)))
    }
  }
} else {
  cat("any_training or total_training_hours absent; T5 skipped.\n")
}

# =====================================================================
# T6. WITHIN-COUPLE, COUPLE BY WAVE
# =====================================================================
cat("\n--- T6. Within-couple training contrast ---\n")

if ("partner_id" %in% names(ev)) {
  cpl <- ev %>%
    filter(!is.na(partner_id), partner_id != "", employed == 1,
           !is.na(jttpewt_any), !is.na(k), k >= -3, k <= 5) %>%
    mutate(couple_id = paste0(pmin(person_id, partner_id), "_",
                              pmax(person_id, partner_id)))
  # Keep only couple-waves where BOTH partners are present, since the
  # design compares the two members of a household in the same wave.
  both <- cpl %>% count(couple_id, wave) %>% filter(n == 2)
  cpl <- cpl %>% semi_join(both, by = c("couple_id", "wave"))
  cat(sprintf("Couple-partner-waves with both partners answering: %s across %s couples\n",
              format(nrow(cpl), big.mark = ","),
              format(dplyr::n_distinct(cpl$couple_id), big.mark = ",")))
  
  if (nrow(cpl) > MIN_N) {
    s1 <- NULL; s2 <- NULL
    # couple_id^wave absorbs whatever the couple shares in that wave. It does
    # NOT absorb a sex-specific calendar path: the two partners sit on
    # opposite sides of the couple-wave mean, so a drift that moves women
    # relative to men survives it and lands in post_f, which is exactly the
    # coefficient of interest here (R10).
    fe_t6 <- if (FE_SEX_WAVE) "couple_id^wave + wave^female" else
      "couple_id^wave + female"
    m6 <- fixest::feols(stats::as.formula(paste0(
      "jttpewt_any ~ post_f | ", fe_t6)),
      data = cpl, cluster = ~couple_id,
      notes = FALSE, warn = FALSE)
    s1 <- report("T6_couple_x_wave", "within-couple training gap, couple x wave FE",
                 m6, "post_f", "pp", bm_pewt, bm_f_pewt, status = "PREFERRED")
    # DECOMPOSED (R27). The previous comparison changed two things at once:
    # m6 carried couple_id^wave AND wave^female while m6b carried neither, so
    # the printed "cost of the interaction" was the cost of the interaction
    # PLUS the cost of the sex-by-wave term. Those are separate decisions and
    # only one of them is what the sentence is about. m6c holds the sex-by-wave
    # term fixed and varies only the couple structure, which is the comparison
    # the claim needs.
    fe_t6c <- if (FE_SEX_WAVE) "couple_id + wave^female" else
      "couple_id + wave + female"
    m6c <- fixest::feols(stats::as.formula(paste0(
      "jttpewt_any ~ post + post_f | ", fe_t6c)),
      data = cpl, cluster = ~couple_id, notes = FALSE, warn = FALSE)
    s3 <- report("T6_couple_sep_sexwave",
                 "separate couple FE, sex-by-wave retained", m6c,
                 "post_f", "pp", bm_pewt, bm_f_pewt, status = "diagnostic")
    m6b <- fixest::feols(jttpewt_any ~ post + post_f | couple_id + wave + female,
                         data = cpl, cluster = ~couple_id,
                         notes = FALSE, warn = FALSE)
    s2 <- report("T6_couple_sep", "same, separate couple and wave FE", m6b,
                 "post_f", "pp", bm_pewt, bm_f_pewt, status = "secondary")
    if (!is.null(s1) && !is.null(s2) && !is.na(s1) && !is.na(s2)) {
      # Labelled as the COMBINED change (R15). This line used to call the
      # whole ratio the cost of the interaction, which the decomposition
      # immediately below then contradicted. The combined figure is worth
      # printing because it is what a reader comparing the two reported
      # specifications sees; it is not an attribution.
      cat(sprintf("  combined change from the separate-effects specification:\n  SE %.4f against %.4f (%.2fx), covering BOTH the couple-by-wave\n  interaction and the sex-by-wave term\n",
                  s1, s2, s1 / s2))
      cat("  The interacted version guarantees each partner is compared with\n")
      cat("  the other in the same wave, so household shocks difference out.\n")
      # The conclusion is COMPUTED, not asserted (R24). Before the sex-by-wave
      # term was added the cost here was 1.01x and calling the stronger design
      # free was correct. With the term it rose to about 1.25x and the design
      # crossed from OK to LIMITED on the pooled benchmark, at which point the
      # printed sentence contradicted the printed number. A conclusion that
      # cannot change when the numbers change is not a conclusion.
      if (!is.null(s3) && !is.na(s3)) {
        cat(sprintf("  decomposed: couple-by-wave costs %.2fx holding the\n",
                    s1 / s3))
        cat(sprintf("  sex-by-wave term fixed; the sex-by-wave term itself costs\n"))
        cat(sprintf("  %.2fx holding the couple structure fixed. Only the first\n",
                    s3 / s2))
        cat("  is the cost of the interaction, which is what this sentence is\n")
        cat("  about; the second is the price of the T2c correction and is\n")
        cat("  already paid everywhere else in the script.\n")
      }
      ratio <- if (!is.null(s3) && !is.na(s3)) s1 / s3 else s1 / s2
      if (is.finite(ratio) && ratio <= 1.10) {
        cat(sprintf("  At %.2fx the stronger design is close to free: use it.\n", ratio))
        note(sprintf(paste("T6: report the couple-by-wave interacted version;",
                           "precision cost %.2fx, effectively free."), ratio))
      } else {
        cat(sprintf("  At %.2fx the interaction is NOT free. It is still the\n", ratio))
        cat("  better-identified design, but the cost has to be stated and the\n")
        cat("  verdict re-read: check whether this row now clears its benchmark\n")
        cat("  on the pooled base, on women's own base, or on neither, and\n")
        cat("  report the separate-effects version beside it rather than\n")
        cat("  instead of it.\n")
        note(sprintf(paste("T6: the couple-by-wave interaction now costs %.2fx,",
                           "not the 1.01x of the uncorrected run. Report both",
                           "and state which benchmark each clears."), ratio))
      }
    }
  } else {
    cat("Cell too thin for the within-couple design.\n")
  }
} else {
  cat("partner_id absent; T6 skipped.\n")
}

# =====================================================================
# T7. ANTICIPATION  (R7)
#
# What went wrong the first time. The design used the ft* fertility
# items, which are asked in fertility-module waves only, so the indicator
# is missing across most of the panel and any mean over an event-time
# window is taken over whichever module waves happen to fall in it. The
# coding compounded it: the life-event family codes 1 = No and 2 = Yes,
# which is why the loader builds had_birth_this_wave as event_birth == 2,
# and a == 1 test on those items selects the noes. The symptom was a
# pregnancy indicator that fired more often after the birth than before
# it, which is not a power problem and could not have been fixed by a
# larger sample.
#
# event_birth (leprg) is asked every wave, is carried in the panel, and is
# reported by both sexes, so the mother-against-father interaction is
# estimable rather than collinear with sex.
#
# WHAT THE ITEM ACTUALLY IS. The discovery run against the 240c release
# settles this and corrects the loader. HILDA carries TWO life events
# here, not one:
#   leprg  SCQ:B23d  Pregnancy
#   lebth  SCQ:B23e  Birth/adoption of new child
# The loader maps leprg to a column called event_birth, with a comment
# reading "Pregnancy/birth of new child", and derives had_birth_this_wave
# from it. Both names are wrong: the column is a PREGNANCY report. lebth
# is present in all 13 waves and is not read at all.
#
# Nothing estimated below is invalidated by this, because the treatment
# cohort comes from ever_parent and never from this item. What changes is
# what the coefficient is called. The local indicator is therefore named
# preg_rep, and the anticipation measure is the contemporaneous pregnancy
# report in the wave before the birth, which needs no lead construction.
# The lead version is retained only as a comparison.
#
# Adding lebth would give the birth its own indicator, and leprgq1 to
# leprgq4 would place the pregnancy inside the twelve-month window. Both
# are additive. Until then, read every T7 row as a pregnancy contrast.
# =====================================================================
cat("\n--- T7. Anticipation ---\n")

# Coverage audit of every candidate indicator. This is the diagnostic
# that was missing the first time: the discovery run searched the
# fertility family on the ^tc pattern and never looked at ft* coverage.
# J2. The v17 names first, since a panel built before that rename may
# still be in circulation, then the current ones. event_child and
# had_child_this_wave are included for their COVERAGE only: the birth item
# now exists and its coverage belongs in this audit, but nothing below
# estimates on it, because T7 is an anticipation design and says so.
# v21.8: the two pre-v17 names are REMOVED from this list.
# compat_panel_check() stops on a panel carrying either name and it runs
# hundreds of lines above this, so these entries were unreachable. Their
# only live effect was to keep the loader's deprecation scan reporting
# this file on every run, and to leave eleven unregistered decisions
# about whether the clock is pregnancy or birth sitting in the tree.
cand <- intersect(c("had_pregnancy_this_wave", "event_pregnancy",
                    "had_child_this_wave", "event_child",
                    "self_or_partner_pregnant", "currently_pregnant",
                    "partner_pregnant"), names(ev))
if (length(cand)) {
  cov_tab <- bind_rows(lapply(cand, function(v) {
    x <- ev[[v]]
    data.frame(indicator = v,
               pct_nonmiss = round(100 * mean(!is.na(x)), 1),
               n_waves_observed = dplyr::n_distinct(ev$wave[!is.na(x)]),
               n_waves_total = dplyr::n_distinct(ev$wave),
               waves = paste(sort(unique(ev$wave[!is.na(x)])), collapse = " "),
               stringsAsFactors = FALSE)
  }))
  cat("Coverage of every candidate indicator:\n")
  print(as.data.frame(cov_tab %>% select(-waves)), row.names = FALSE)
  write.csv(cov_tab, file.path(OUTDIR, "T7_indicator_coverage.csv"),
            row.names = FALSE)
  cat("An indicator observed in a handful of waves cannot support an\n")
  cat("event-time profile, whatever its coding. The wave list for each is\n")
  cat("in T7_indicator_coverage.csv.\n")
}

# J3. The coding is determined by the loader rather than asserted here.
# It picks the (event, flag) code pair that maximises the share of events
# carrying exactly one quarter flag, and returned event code 2 and flag
# code 1 in all thirteen waves, with 90 to 98 per cent single-flagged.
.ev_raw <- intersect("event_pregnancy", names(ev))
if (length(.ev_raw)) {
  cat("\nRaw values of", .ev_raw[1],
      "by sex (loader-determined: 2 = Yes, 1 = No):\n")
  print(as.data.frame(ev %>% filter(!is.na(.data[[.ev_raw[1]]])) %>%
                        count(female, .data[[.ev_raw[1]]])), row.names = FALSE)
}

# Build the birth report, preferring the loader's own derived indicator so
# that this script and the loader cannot disagree about the coding.
# Missing is kept as missing rather than coded zero. A wave in which the
# life-event item was not asked would otherwise arrive as a run of noes,
# which is exactly the failure that produced the previous T7 result.
# J2. PREGNANCY, not birth, and not because I chose: the block is titled
# anticipation, the indicator is named preg_rep, and the comment above
# says to read every T7 row as a pregnancy contrast. The loader's derived
# indicator is preferred over a raw test so that the two files cannot
# disagree about the coding.
if ("had_pregnancy_this_wave" %in% names(ev)) {
  ev$preg_rep <- ifelse(is.na(ev$had_pregnancy_this_wave), NA_integer_,
                        as.integer(ev$had_pregnancy_this_wave == 1))
  birth_src <- "had_pregnancy_this_wave (loader-derived from leprg)"
} else if ("event_pregnancy" %in% names(ev)) {
  ev$preg_rep <- ifelse(is.na(ev$event_pregnancy), NA_integer_,
                        as.integer(ev$event_pregnancy == 2))
  birth_src <- "event_pregnancy == 2 (loader-determined code)"
} else {
  # v21.8: the two PRE-v17 fallback branches are GONE. Both read a column
  # compat_panel_check() refuses to let through, so neither could ever
  # execute, and a fallback that cannot run is not a safety net; it is an
  # undeclared decision about whether the event clock is pregnancy or
  # birth, sitting where a reader will take it for a live option.
  #
  # If the current names are absent the honest outcome is a stop, not a
  # silent NA series that makes T7 estimate on nothing.
  # The message names the removed columns via DEPRECATED_NAMES rather than
  # as literals. The loader's deprecation scan excludes comment lines but
  # not strings, so spelling them out here made this stop() the only
  # remaining hit in the whole tree, which is a false positive that
  # crowds out real ones.
  stop("T7: neither had_pregnancy_this_wave nor event_pregnancy is on the ",
       "panel. The two pre-v17 fallbacks were removed in v21.8 because ",
       "compat_panel_check() stops on a panel carrying either, so they ",
       "were unreachable; see COMPAT_DEPRECATED for their names. Rebuild ",
       "on the v21.6 loader or later.", call. = FALSE)
}

if (!is.na(birth_src)) {
  cat("\nBirth report built from:", birth_src, "\n")
  cat(sprintf("Person-waves with the item missing: %s (%.1f%%)\n",
              format(sum(is.na(ev$preg_rep)), big.mark = ","),
              100 * mean(is.na(ev$preg_rep))))
  
  # The lead, defined only where the next observed wave is contiguous, so
  # that a wave gap is not silently read as an adjacent year.
  d7 <- ev %>%
    arrange(person_id, wave) %>%
    group_by(person_id) %>%
    mutate(next_wave = dplyr::lead(wave),
           expecting = ifelse(!is.na(next_wave) & next_wave == wave + 1L,
                              dplyr::lead(preg_rep), NA_integer_)) %>%
    ungroup() %>%
    filter(employed == 1, !is.na(jttpewt_any))
  
  cat(sprintf(paste("Employed male person-waves reporting pregnancy or birth:",
                    "%.2f%%; flagged as expecting: %.2f%%\n"),
              100 * mean(d7$preg_rep[d7$female == 0], na.rm = TRUE),
              100 * mean(d7$expecting[d7$female == 0], na.rm = TRUE)))
  cat("  Both rates are on the employed estimation sample, not on the raw\n")
  cat("  cross-tabulation above, which is over all person-waves. The two\n")
  cat("  differ and appearing next to each other invites reading one as a\n")
  cat("  check on the other (R21).\n")
  # lebth is the BIRTH item and exists in all 13 waves of the release; leprg,
  # which the loader reads, is the PREGNANCY item with a twelve-month window.
  # Until lebth arrives, everything below dates the pregnancy report, so it
  # is named that way rather than called a birth.
  # J4. Done. lebth arrived in the loader's v17 rename and is event_child,
  # with had_child_this_wave derived from it, so the birth and the
  # pregnancy ARE separated in this run. Nothing below switches to it: T7
  # is an anticipation design and the pregnancy item is what it wants.
  # Which of the two any OTHER site wants is that site's decision.
  lebth_col <- intersect(c("had_child_this_wave", "event_child",
                           "lebth", "event_birth_new_child", "birth_event"),
                         names(ev))
  if (length(lebth_col)) {
    cat("  The birth item is in the panel as", lebth_col[1],
        "and the pregnancy item\n  as the source of preg_rep above. They are ",
        "separate columns, so any\n  contrast between them is available.\n")
    if (all(c("preg_rep", lebth_col[1]) %in% c(names(ev), "preg_rep"))) {
      .b <- ev[[lebth_col[1]]]
      .b <- if (grepl("^had_", lebth_col[1])) as.integer(.b == 1) else
        as.integer(.b == 2)
      cat(sprintf(paste0("  Coverage: pregnancy reported on %.2f%% of ",
                         "person-waves, birth on %.2f%%.\n"),
                  100 * mean(ev$preg_rep, na.rm = TRUE),
                  100 * mean(.b, na.rm = TRUE)))
      rm(.b)
    }
  } else {
    cat("  No birth item in the panel. It is present in all 13 waves of the\n",
        "  release and is an additive loader change.\n")
  }
  
  # Cross-check on the cohort definition itself, which is free once the
  # life-event item is in hand. The never-treated are the always-childless
  # by the absorbing ever_parent rule; a large share of them reporting a
  # birth would mean that rule is missing births, and every design in this
  # script and in 02 inherits the same cohort assignment.
  # Broken out by sex, because the direction is the diagnostic. If
  # ever_parent is built on RESIDENT children, the people it misclassifies
  # are non-resident parents, who are overwhelmingly fathers. A male share
  # materially above the female share is that misclassification showing
  # itself, and it contaminates the comparison group on the men's side of
  # every contrast in both scripts.
  nt_birth <- ev %>% filter(treated == 0) %>%
    group_by(person_id, female) %>%
    summarise(any_birth = as.integer(any(preg_rep == 1, na.rm = TRUE)),
              .groups = "drop")
  nt_tab <- nt_birth %>% group_by(female) %>%
    summarise(persons = n(), reporting = sum(any_birth),
              pct = round(100 * mean(any_birth), 1), .groups = "drop")
  cat("\nNever-treated persons reporting a PREGNANCY at any wave, by sex:\n")
  cat("  leprg is a pregnancy item, so a report here need not be a missed\n")
  cat("  birth: a pregnancy that did not end in a resident child, or a\n")
  cat("  partner's pregnancy, produces the same code. Read the male-female\n")
  cat("  difference as the diagnostic and the level as an upper bound on how\n")
  cat("  much ever_parent misses.\n")
  print(as.data.frame(nt_tab), row.names = FALSE)
  write.csv(nt_tab, file.path(OUTDIR, "T7_cohort_crosscheck.csv"),
            row.names = FALSE)
  pm <- nt_tab$pct[nt_tab$female == 0]; pf <- nt_tab$pct[nt_tab$female == 1]
  if (length(pm) == 1 && length(pf) == 1) {
    cat(sprintf("Male %.1f%% against female %.1f%%, a difference of %+.1f points.\n",
                pm, pf, pm - pf))
    if (pm > pf + 1)
      note(sprintf(paste("Cohort check: %.1f%% of never-treated men report a birth",
                         "against %.1f%% of never-treated women. ever_parent is",
                         "likely missing non-resident fathers, which contaminates",
                         "the male comparison group in both 02 and 03."), pm, pf))
    else
      note(sprintf(paste("Cohort check: never-treated birth reports are %.1f%%",
                         "male against %.1f%% female, so the ever_parent rule",
                         "is not differentially missing fathers."), pm, pf))
  }
  
  prof7 <- d7 %>%
    filter(treated == 1, k >= -3, k <= 2) %>%
    group_by(k, female) %>%
    # n_obs and n_lead are the denominators the two percentages are actually
    # taken over (F10). Revision 2 printed n = n() beside rates computed
    # with na.rm = TRUE, so the counts and the rates could not be multiplied
    # together, which is the fault R21 fixes elsewhere.
    summarise(n = n(),
              n_obs = sum(!is.na(preg_rep)),
              pct_preg_rep = round(100 * mean(preg_rep, na.rm = TRUE), 1),
              n_lead = sum(!is.na(expecting)),
              pct_expecting = round(100 * mean(expecting, na.rm = TRUE), 1),
              .groups = "drop") %>%
    pivot_wider(names_from = female,
                values_from = c(n, n_obs, pct_preg_rep, n_lead,
                                pct_expecting)) %>%
    arrange(k)
  cat("\nBirth reports and lead-of-birth by event time, treated persons:\n")
  print(as.data.frame(prof7), row.names = FALSE)
  write.csv(prof7, file.path(OUTDIR, "T7_preg_report_profile.csv"),
            row.names = FALSE)
  
  # What does the item date? leprg asks whether a PREGNANCY occurred in
  # the last twelve months, so for a first-time parent it fires in the
  # wave before the birth and again in the birth wave, since she was
  # pregnant during part of both reference years. Two-wave firing is
  # therefore the expected pattern rather than a puzzle, and the report
  # at k = -1 is the anticipation measure with no lead required.
  if ("pct_preg_rep_1" %in% names(prof7)) {
    at <- function(kk) {
      v <- prof7$pct_preg_rep_1[prof7$k == kk]
      if (length(v) == 1) v else NA_real_
    }
    r_m1 <- at(-1L); r_0 <- at(0L)
    pk <- prof7$k[which.max(prof7$pct_preg_rep_1)]
    cat(sprintf("\nTreated women: %.1f%% report at k = -1, %.1f%% at k = 0, peak at k = %d.\n",
                r_m1, r_0, pk))
    two_wave <- !is.na(r_m1) && !is.na(r_0) && r_m1 >= 0.5 * r_0 && r_m1 > 20
    if (two_wave) {
      cat("The item fires in BOTH waves, which is what a pregnancy question\n")
      cat("with a twelve-month reference window does for a first birth. The\n")
      cat("report at k = -1 is the anticipation measure in its own right.\n")
      cat("Note this is leprg, SCQ:B23d Pregnancy, NOT the birth item lebth,\n")
      cat("whatever the loader has called the column.\n")
      
      # How far the contemporaneous report and the lead disagree, at the
      # wave that matters. This is not a formality. A woman flagged by the
      # lead but not by the contemporaneous report is one who reports a
      # birth at k = 0 having reported nothing at k = -1, which means she
      # was not yet pregnant when the k = -1 interview happened. She cannot
      # have been anticipating anything, so including her in the regressor
      # is measurement error in the treatment and pulls the coefficient
      # towards zero.
      ov <- d7 %>% filter(treated == 1, female == 1, k == -1L,
                          !is.na(preg_rep), !is.na(expecting))
      if (nrow(ov) > 0) {
        n_both <- sum(ov$preg_rep == 1 & ov$expecting == 1)
        n_lead <- sum(ov$preg_rep == 0 & ov$expecting == 1)
        n_con  <- sum(ov$preg_rep == 1 & ov$expecting == 0)
        n_nei  <- sum(ov$preg_rep == 0 & ov$expecting == 0)
        cat("\nTreated women at k = -1, contemporaneous report against the lead:\n")
        print(table(contemporaneous = ov$preg_rep, lead = ov$expecting))
        cat(sprintf("  both %s | lead only %s | contemporaneous only %s | neither %s\n",
                    format(n_both, big.mark = ","), format(n_lead, big.mark = ","),
                    format(n_con, big.mark = ","), format(n_nei, big.mark = ",")))
        if (n_both + n_lead > 0)
          cat(sprintf("  %.0f%% of the women the lead flags were NOT pregnant at the\n",
                      100 * n_lead / (n_both + n_lead)))
        cat("  k = -1 interview by their own report. The lead is therefore not\n")
        cat("  a near-duplicate of the contemporaneous indicator; it is a\n")
        cat("  noisier version of it, and the noise is one-directional.\n")
      }
      note("T7: leprg is the PREGNANCY item and fires at k = -1 and k = 0. Report the contemporaneous pre-birth indicator; the lead adds women who were not yet pregnant.")
      # J4: this note recommended the loader rename the two columns and add
      # lebth. All three are done, so the note is a record rather than a
      # request. Left in the log because a reader of an archived run needs
      # to know which side of the change it was made on.
      note("Loader: DONE at v17. event_pregnancy and event_child are separate columns, had_pregnancy_this_wave and had_child_this_wave are derived from them, and the quarter items for both are read and reconciled.")
    } else if (!is.na(pk) && pk == 0L) {
      cat("The item fires only in the birth wave, so it dates the birth and\n")
      cat("the lead is the anticipation regressor.\n")
      note("T7: leprg dates the birth alone. Report the lead specification.")
    } else if (!is.na(pk) && pk == -1L) {
      cat("The item fires only before the birth, so the contemporaneous\n")
      cat("indicator is the anticipation measure and the lead double-counts.\n")
      note("T7: leprg leads the birth. Report the contemporaneous specification.")
    } else {
      cat("The profile matches none of the expected shapes. Check the cohort\n")
      cat("assignment against the life-event item on individual persons\n")
      cat("before reporting anything from this design.\n")
      note("T7: birth-report profile is not interpretable. Resolve before reporting.")
    }
  }
  
  # -------------------------------------------------------------------
  # A single pooled "expecting" indicator will not do, because the item
  # fires again for SUBSEQUENT pregnancies. In this panel a large share of
  # treated persons report a birth event two waves after the first birth,
  # which is second children, not anticipation of the first. Pooling them
  # makes the coefficient a weighted average of anticipating a first birth
  # and anticipating a later one, and those are different behaviours with
  # no reason to share a sign.
  #
  # The birth reports are therefore split into three mutually exclusive
  # indicators that together exhaust every wave in which the item fires:
  #   b_pre    treated, k = -1. The birth has not happened, so this is a
  #            PREGNANCY report, and it is the anticipation measure.
  #   b_at     treated, k = 0. The first birth wave itself.
  #   b_other  everything else: later pregnancies and births for treated
  #            persons, and any report by a never-treated person.
  # -------------------------------------------------------------------
  d7c <- d7 %>%
    filter(!is.na(preg_rep)) %>%
    mutate(b_pre   = as.integer(preg_rep == 1 & treated == 1 &
                                  !is.na(k) & k == -1L),
           b_at    = as.integer(preg_rep == 1 & treated == 1 &
                                  !is.na(k) & k == 0L),
           b_other = as.integer(preg_rep == 1 & b_pre == 0 & b_at == 0),
           b_pre_f = b_pre * female, b_at_f = b_at * female,
           b_other_f = b_other * female)
  
  cat(sprintf("\nPregnancy-report waves split: %s pre-birth, %s at the birth, %s other.\n",
              format(sum(d7c$b_pre), big.mark = ","),
              format(sum(d7c$b_at), big.mark = ","),
              format(sum(d7c$b_other), big.mark = ",")))
  oth <- d7c %>% filter(b_other == 1) %>%
    summarise(pct_treated_post = round(100 * mean(treated == 1 &
                                                    !is.na(k) & k >= 1), 1),
              pct_never_treated = round(100 * mean(treated == 0), 1),
              pct_treated_early = round(100 * mean(treated == 1 &
                                                     !is.na(k) & k <= -2), 1))
  cat("Composition of the other category:\n")
  print(as.data.frame(oth), row.names = FALSE)
  
  if (sum(d7c$b_pre) > MIN_N) {
    m7p <- fixest::feols(stats::as.formula(paste0("jttpewt_any ~ b_pre + b_pre_f + b_at + b_at_f + b_other + b_other_f +
        post + post_f | ", fe_block())),
                         data = d7c, cluster = ~person_id, notes = FALSE, warn = FALSE)
    report("T7_pregnancy_gap", "pregnancy wave before the first birth, women less men",
           m7p, "b_pre_f", "pp", bm_pewt, bm_f_pewt, status = "PREFERRED")
    report("T7_pregnancy_men", "same, men's own coefficient", m7p, "b_pre",
           "pp", bm_pewt, NA_real_, status = "secondary")
    report("T7_laterbirth_gap", "later pregnancies and births, women less men",
           m7p, "b_other_f", "pp", bm_pewt, bm_f_pewt, status = "report")
    report("T7_birthwave_gap", "the first birth wave itself, women less men",
           m7p, "b_at_f", "pp", bm_pewt, bm_f_pewt, status = "diagnostic")
    cat("  b_pre_f is the anticipation test. A negative coefficient is a\n")
    cat("  mother cutting training while pregnant, relative to a father in\n")
    cat("  the same wave, and conditional on the post-birth effect. It is\n")
    cat("  both a finding and a problem for the event study, because the\n")
    cat("  omitted reference period is the year before the birth and a\n")
    cat("  pregnant woman is already inside it.\n")
    cat("  b_other_f is a second result rather than a nuisance: it says\n")
    cat("  whether the same withdrawal happens before later births.\n")
  } else {
    cat("Too few pre-birth pregnancy waves; T7 not estimated.\n")
  }
  
  # The lead-based version, kept for continuity with the previous run and
  # as a check that the two constructions agree. Where the item fires in
  # both the pregnancy and the birth wave these select nearly the same
  # person-waves, so a material disagreement would mean the contiguity
  # restriction on the lead is doing something unintended.
  d7e <- d7 %>% filter(!is.na(expecting), !is.na(preg_rep)) %>%
    mutate(expecting_f = expecting * female)
  if (nrow(d7e) > MIN_N) {
    m7 <- fixest::feols(stats::as.formula(paste0(
      "jttpewt_any ~ expecting + expecting_f + post + post_f | ",
      fe_block())),
      data = d7e, cluster = ~person_id,
      notes = FALSE, warn = FALSE)
    report("T7_expecting_gap", "lead of the birth report, women less men",
           m7, "expecting_f", "pp", bm_pewt, bm_f_pewt, status = "secondary")
    # The lead arm's sample is smaller AND the loss is not random (R23). The
    # lead is undefined at a person's last observed wave, so an attriter can
    # never be flagged as expecting, and the people who leave the panel are
    # not a random draw on the outcome. The ledger's MDE column makes this
    # arm look better than the contemporaneous one because a regressor with
    # more variance buys a tighter standard error whatever it is measuring;
    # that precision is bought with a selected sample as well as with mixed
    # estimands, and neither is a reason to prefer it.
    n_contemp <- tryCatch(stats::nobs(m7p), error = function(e) NA_integer_)
    n_lead    <- tryCatch(stats::nobs(m7),  error = function(e) NA_integer_)
    if (is.finite(n_contemp) && is.finite(n_lead) && n_contemp > 0) {
      cat(sprintf(paste("\n  The lead arm loses %s of %s person-waves (%.0f%%)",
                        "relative to the\n  contemporaneous arm."),
                  format(n_contemp - n_lead, big.mark = ","),
                  format(n_contemp, big.mark = ","),
                  100 * (n_contemp - n_lead) / n_contemp))
      cat(" The lead is undefined at a person's final\n")
      cat("  observed wave, so the loss falls on attriters and is not random.\n")
      note(sprintf(paste("T7: the lead arm drops %.0f%% of the sample,",
                         "non-randomly, on top of pooling first and later",
                         "pregnancies. Prefer the contemporaneous indicator."),
                   100 * (n_contemp - n_lead) / n_contemp))
    }
  }
  
  # Sharpening this further needs the quarter in which the event fell.
  # J4. The loader reads leprgq1 to leprgq4 and lebthq1 to lebthq4,
  # reconciles them to an exact identity (6,669 dated plus 7 multiply
  # flagged plus 342 undated against 7,018 reported births) and summarises
  # them as preg_quarter_event, birth_quarter_event and
  # post_birth_window_share. The pattern below looks for those first.
  qtr <- grep(paste0("preg_quarter|birth_quarter|post_birth_window|",
                     "leprgq|lebthq|event_birth_q"),
              names(panel), value = TRUE, ignore.case = TRUE)
  if (!length(qtr)) {
    cat("\nThe leprgq* quarter items are not in the panel. With them the\n")
    cat("event could be placed within the twelve-month reference window\n")
    cat("directly, which would separate anticipation from a straddling\n")
    cat("reference period rather than bounding the two together. Add them\n")
    cat("to the loader's variable map alongside leprg if T7 is to carry\n")
    cat("weight in the paper.\n")
    note("T7: add leprgq* to the loader's variable map to date the event within the reference window. NOTE this was done at v19; if the pattern above found nothing, the panel predates it.")
  } else {
    cat("\nQuarter items present:", paste(qtr, collapse = ", "), "\n")
  }
} else {
  # Fallback to the fertility module, with the coding corrected. Kept only
  # so the script runs against an older panel build.
  preg_var <- intersect(c("self_or_partner_pregnant", "currently_pregnant"),
                        names(ev))
  if (length(preg_var)) {
    pv <- preg_var[1]
    cat("No life-event pregnancy item; falling back to", pv,
        "which is a module-wave item.\n")
    # J7. Said to the guard as well as to the reader. These exist at waves
    # 15, 19 and 23 only, so a mean over an event-time window is a mean
    # over whichever of those three falls inside it. allow_span is TRUE
    # because this branch is diagnostic and prints a warning of its own,
    # and stopping here would remove the fallback that lets the script run
    # against an older panel.
    assert_module_waves(ev, pv, context = "T7 module fallback",
                        allow_span = TRUE)
    d7 <- ev %>%
      mutate(preg = as.integer(!is.na(.data[[pv]]) & .data[[pv]] == 1)) %>%
      filter(employed == 1, !is.na(jttpewt_any))
    print(as.data.frame(
      d7 %>% filter(treated == 1, female == 1, k >= -3, k <= 1) %>%
        group_by(k) %>% summarise(n = n(),
                                  pct_pregnant = round(100 * mean(preg), 1),
                                  .groups = "drop")), row.names = FALSE)
    cat("Treat any profile from this indicator as uninterpretable until its\n")
    cat("wave coverage and its coding have both been checked.\n")
  } else {
    cat("No usable indicator; T7 skipped.\n")
  }
}

# =====================================================================
# T9. IS THE TRAINING FALL A SUBSTITUTION INTO FORMAL STUDY?
#
# This is the one design in the script that can overturn a sentence
# already drafted. The training subsection reads the post-birth fall as a
# reduction in skill investment. Formal study is the obvious substitute:
# a mother who stops attending employer courses and enrols in a diploma
# has not reduced her investment, she has moved it to a channel the
# training module does not see. If mothers' study enrolment rises when
# their training falls, the section is measuring a substitution.
#
# The discovery run found current enrolment in all 13 waves, under
# several spellings, so this clears the event-time coverage bar that the
# ft* fertility items failed. Two designs are run: the same event study
# as T2 with enrolment as the outcome, and a stacked version placing
# study beside the two training channels so the substitution appears as a
# single interaction rather than as two series read against each other.
# =====================================================================
cat("\n--- T9. Substitution into formal study ---\n")

# J5. full_time_student_yes first: it is the loader's 0/1 indicator, and
# the raw full_time_student is a 1/2 questionnaire code that the == 1 test
# below happens to read correctly. Depending on that coincidence is how a
# design breaks silently when a release changes a code frame.
#
# DO NOT ADD enrolled_in_course TO THIS LIST. It is put only to
# respondents on the new-person questionnaire, measured at 0.0 per cent of
# 113,926 continuing person-waves against 52.2 per cent of 9,355 entrant
# ones, so it is observed for roughly 4 per cent of the panel in EVERY
# wave and a rate computed on it is a statement about new entrants, who
# are younger. It was never in this list, which was luck rather than
# design. The guard below stops it if anyone adds it.
STUDY_DO_NOT_USE <- c("enrolled_in_course", "enrolled_in_course_yes",
                      "enrolled_in_course_comparable", "edcqen")
study_col <- intersect(c("full_time_student_yes", "studying_now",
                         "currently_studying", "enrolled_study",
                         "full_time_student", "student", "edfts"),
                       names(ev))
study_col <- setdiff(study_col, STUDY_DO_NOT_USE)
if (length(study_col)) {
  sc <- study_col[1]
  assert_not_entrant_only(sc, context = "T9 study outcome", data = panel)
  # J6. Wave 24 carries a sample refresher, so a study series running
  # through it compares two samples. This is an event study with person
  # and wave fixed effects, which absorb a common composition shift, so
  # the span is declared rather than the wave dropped.
  assert_study_comparable(ev, context = "T9 event study, person and wave FE",
                          vars = sc, allow_span = TRUE)
  ev$study_now <- as.integer(!is.na(ev[[sc]]) & ev[[sc]] == 1)
  base_study <- mean(ev$study_now[ev$employed == 1], na.rm = TRUE)
  cat(sprintf("Enrolment indicator: %s | base rate among the employed %.1f%%\n",
              sc, 100 * base_study))
  bm_study <- BM_PROP * base_study
  
  d9 <- ev %>% filter(employed == 1, !is.na(study_now))
  m9 <- fixest::feols(stats::as.formula(paste0("study_now ~ post + post_f | ", fe_block())),
                      data = d9, cluster = ~person_id,
                      notes = FALSE, warn = FALSE)
  report("T9_study_gap", "study enrolment, women less men, post-birth",
         m9, "post_f", "pp", bm_study, status = "PREFERRED")
  
  prof9 <- ev %>%
    filter(treated == 1, k >= K_LO, k <= K_HI, employed == 1,
           !is.na(study_now)) %>%
    group_by(k, female) %>%
    summarise(n = n(), rate = mean(study_now), .groups = "drop") %>%
    pivot_wider(names_from = female, values_from = c(n, rate)) %>%
    arrange(k) %>%
    mutate(gap = round(rate_1 - rate_0, 4))
  cat("Study enrolment by event time (employed only):\n")
  print(as.data.frame(prof9 %>%
                        mutate(across(c(rate_0, rate_1), ~round(.x, 4)))),
        row.names = FALSE)
  write.csv(prof9, file.path(OUTDIR, "T9_study_profile.csv"), row.names = FALSE)
  
  # Stacked against the two training channels. A positive coefficient on
  # the study interaction, against negative ones on the training channels,
  # IS the substitution, and it is estimated on one sample with one
  # standard error rather than inferred by comparing two tables.
  long9 <- bind_rows(
    ev %>% transmute(person_id, wave, k, treated, female, employed, post,
                     channel = "employer", y = jttpewt_any),
    ev %>% transmute(person_id, wave, k, treated, female, employed, post,
                     channel = "own_time", y = jttpeot_any),
    ev %>% transmute(person_id, wave, k, treated, female, employed, post,
                     channel = "study", y = study_now)) %>%
    filter(employed == 1, !is.na(y)) %>%
    mutate(is_study = as.integer(channel == "study"),
           post_f   = post * female,
           post_s   = post * is_study,
           post_f_s = post * female * is_study)
  m9b <- fixest::feols(y ~ post + post_f + post_s + post_f_s |
                         person_id^channel + wave^channel,
                       data = long9, cluster = ~person_id,
                       notes = FALSE, warn = FALSE)
  report("T9_study_vs_training", "mother x post x study, against training",
         m9b, "post_f_s", "pp", bm_study, status = "PREFERRED")

  # --- STUDY AGAINST EMPLOYER-FINANCED, NOT AGAINST BOTH CHANNELS -----
  #
  # WHAT m9b ABOVE ESTIMATES. Three channels are stacked and a SINGLE
  # post_f is fitted across all of them, with post_f_s shifting it only for
  # study. So the two training channels are constrained to share one
  # mother-by-post coefficient, and post_f_s is study against their
  # constrained average. The text and the abstract describe it as study
  # against employer-financed training, which is a different contrast: the
  # own-time channel falls too, by -0.0185, so pooling moves the comparison
  # base and the study coefficient inherits part of the training decline.
  #
  # THE BLOCK ALSO DROPS female, the same omission as the triple difference
  # in 03b. wave^channel is a path common to both sexes. Restored below, so
  # the sex-specific channel-by-wave drift is absorbed rather than assumed
  # away.
  #
  # WHAT THIS MODEL GIVES. Employer-financed is the omitted channel, so
  # post_f is its own mother-by-post effect, post_f_study is study MINUS
  # employer-financed, which is the contrast the paper claims to report,
  # and post_f_own is own-time minus employer-financed, which is the triple
  # difference of 03b estimated a second way on a third stack. If
  # post_f_own does not land near +0.098 the two designs disagree, and that
  # is worth knowing before either is quoted.
  #
  # THE STANDALONE STUDY RESPONSE ALREADY EXISTS as T9_study_gap above,
  # fitted on study_now alone. That is the quantity to report when the claim
  # is about study rather than about substitution, and it depends on none of
  # this.
  long9 <- long9 %>%
    mutate(ch_own       = as.integer(channel == "own_time"),
           ch_study     = as.integer(channel == "study"),
           post_own     = post * ch_own,
           post_study   = post * ch_study,
           post_f_own   = post_f * ch_own,
           post_f_study = post_f * ch_study)
  m9c <- fixest::feols(y ~ post + post_f + post_own + post_study +
                         post_f_own + post_f_study |
                         person_id^channel + wave^channel^female,
                       data = long9, cluster = ~person_id,
                       notes = FALSE, warn = FALSE)
  report("T9_study_vs_employer",
         "mother x post x study, against employer-financed",
         m9c, "post_f_study", "pp", bm_pewt, status = "post-hoc")
  report("T9_owntime_vs_employer",
         "mother x post x own-time, against employer-financed",
         m9c, "post_f_own", "pp", bm_pewt, status = "post-hoc")
  .c9 <- tryCatch(fixest::coeftable(m9c), error = function(e) NULL)
  if (!is.null(.c9) && all(c("post_f", "post_f_study", "post_f_own")
                           %in% rownames(.c9))) {
    cat(sprintf(paste0("  channel-specific mother x post: employer %+.4f,",
                       " own-time %+.4f, study %+.4f\n"),
                .c9["post_f", "Estimate"],
                .c9["post_f", "Estimate"] + .c9["post_f_own", "Estimate"],
                .c9["post_f", "Estimate"] + .c9["post_f_study", "Estimate"]))
    cat("  The three levels are what the pooled model constrained. Compare\n")
    cat("  the study level against T9_study_gap, which fits it alone: they\n")
    cat("  answer different questions and only one belongs in the abstract.\n")
    rm(.c9)
  }
  cat("  A POSITIVE coefficient here is substitution: mothers move into\n")
  cat("  study as they move out of employer-funded training, and the\n")
  cat("  training fall overstates the fall in skill investment. A zero or\n")
  cat("  negative coefficient closes the objection and should be reported\n")
  cat("  in the text, because a referee will raise it whether or not the\n")
  cat("  paper does.\n")
  note("T9: report the study-substitution result either way; it is the obvious objection to the training section.")
} else {
  cat("No study-enrolment indicator in the panel, so the substitution\n")
  cat("objection cannot be answered. The release carries edcqen, edccenr,\n")
  cat("edfts, caeft and caept in all 13 waves, so this clears the\n")
  cat("event-time coverage bar. Of the additive changes on the list this\n")
  cat("is the one that can change a claim rather than sharpen it: if\n")
  cat("mothers substitute study for employer training, the section is\n")
  cat("measuring a channel switch and not a reduction in investment.\n")
  note("T9: extract a current-study indicator (edcqen or edfts). Until then the substitution objection to the training section is unanswered.")
}


# =====================================================================
# T12. WAGE COMPRESSION: UNION AND PUBLIC SECTOR
# ---------------------------------------------------------------------
# Folded in from 03at12compression.R at v21.46. That file declared
# itself "Sourced by 03a_design_audit.R before 03_finish.R" and nothing
# sourced it: not 03a, not RUN_ALL.R, not either master. So T12 had
# never run in the pipeline, its ledger rows were never collected, and
# a question about its structural-zero block could not be answered from
# the files anyone was handed. A module that is not where its siblings
# are does not get run and does not get found.
#
# WHY THIS IS THE FRAMEWORK'S OWN UNTESTED PREMISE. Acemoglu and Pischke
# turn on delta < 1: wages compressed relative to productivity, so the
# firm retains (1 - delta)Delta and will finance general training.
# Section 3.2 says that compressed regime "is the regime this paper
# assumes" and then never checks it. Unions and public pay scales are
# what PRODUCE compression, so the assumption is testable on columns the
# panel already carries.
#
# TWO QUESTIONS, AND THEY HAVE VERY DIFFERENT POWER.
#
#   THE LEVEL. Is employer-financed training concentrated where wages
#   are compressed? Estimated on the full employed sample, precise, and
#   it turns an assumption into evidence. This is the part worth having.
#
#   THE INTERACTION. Does the withdrawal at first birth differ by
#   compression? Splits an already-split sample and is expected to be
#   underpowered. It is estimated anyway, because an LPM costs
#   milliseconds and the gate is about what may be CONCLUDED rather than
#   about compute, and it is graded by its own MDE like everything else.
#
# THE PREDICTION IS NOT SHARP, and saying so before the estimates is the
# point of writing it down first. A slacker investment condition in a
# compressed sector means a given fall in E[T]E[H] is LESS likely to flip
# the inequality, so a SMALLER withdrawal. A higher baseline means more
# to withdraw, so a LARGER absolute fall. The two point opposite ways in
# levels and the proportional statement is the one to read.
#
# AND PUBLIC SECTOR CONFOUNDS TWO CHANNELS. Stronger parental leave and
# greater job security also hold E[T] up at a birth, through expected
# tenure rather than through compression. Nothing here separates them, so
# a difference in either direction is consistent with both accounts. The
# union split is the cleaner compression proxy and the thinner cell.
# =====================================================================

.t12_need <- c("person_id", "wave", "k", "female", "treated", "post",
               "employed", "jttpewt_any", "union_member", "public_sector")

if (!exists("ev") || !all(.t12_need %in% names(ev))) {
  cat("\n  [skip] T12.\n")
  if (!exists("ev")) {
    cat("  Reason: no event frame.\n")
  } else {
    .miss <- setdiff(.t12_need, names(ev))
    cat("  Reason: missing columns:", paste(.miss, collapse = ", "), "\n")
    if (any(c("union_member", "public_sector") %in% .miss))
      cat("  NOTE: these two are in the loader's slim-panel select but NOT in\n",
          "  training_vars, so a frame built from hilda_panel_data_training.rds\n",
          "  will never have them. Use the slim panel, or add them to\n",
          "  training_vars and rebuild.\n", sep = "")
    rm(.miss)
  }
} else {
  
  cat("\n--- T12. Wage compression: union and public sector ---\n")
  
  # ---------------------------------------------------------------------
  # 1. THE STRUCTURAL ZERO, AUDITED BEFORE ANYTHING IS ESTIMATED
  # ---------------------------------------------------------------------
  # ONE COLUMN REPRODUCES THE jbcasab FAULT AND ONE DOES NOT, and the
  # first run of this block corrected the claim that both did. The audit
  # below is what caught it, which is why it prints counts rather than a
  # verdict.
  #
  # safe_numeric() maps HILDA's negative codes to NA. From there the two
  # constructions diverge:
  #
  #   union_member  = ifelse(safe_numeric(jbmtuea) == 1, 1, 0)
  #       NA == 1 is NA, so ifelse returns NA and the missing PROPAGATES.
  #       CORRECT AS WRITTEN. 10,329 of 10,410 non-employed person-waves
  #       are NA on this column, exactly as they should be.
  #
  #   public_sector = ifelse(safe_numeric(jbmmply) %in% c(2,5), 1, 0)
  #       %in% IS NA-BLIND. NA %in% c(2,5) is FALSE, never NA, so every
  #       not-asked person-wave becomes 0 and reads PRIVATE SECTOR.
  #       CONTAMINATED.
  #
  #   public_sector = ifelse(is.na(public_sector), 0, public_sector)
  #       DEAD CODE. The line above can never produce NA, so this never
  #       fires. It documents an intention the previous line had already
  #       defeated, which is the most misleading kind of dead line: it
  #       reads as the missing having been handled.
  #
  # THE TWO COLUMNS CROSS-VALIDATE, and the arithmetic is exact. Union's
  # 10,329 NAs identify which person-waves were never asked; 10,329
  # imputed zeros plus 67 genuine private answers is 10,396, the observed
  # zero count on public_sector to the person-wave. The audit reproduces
  # this on whatever frame it is given rather than trusting these numbers.
  #
  # THE FIX IS ON public_sector ALONE, and it is not the removal of the
  # dead line, which would change nothing. The guard has to come BEFORE
  # the %in%:
  #
  #   .jbmmply_num = safe_numeric(., v$jbmmply),
  #   public_sector = ifelse(is.na(.jbmmply_num), NA_real_,
  #                          as.numeric(.jbmmply_num %in% c(2, 5)))
  #
  # Note this makes public_sector incomplete where it was complete, so any
  # design using it as a control will lose rows. That is the point: those
  # rows were being compared as private-sector workers.
  #
  # THIS BLOCK GATES TO THE EMPLOYED regardless, so its own estimates are
  # clean either way. It does NOT fix the column for anything else.
  .t12_ne <- ev$employed != 1 | is.na(ev$employed)
  cat(sprintf("  non-employed person-waves: %s\n",
              format(sum(.t12_ne, na.rm = TRUE), big.mark = ",")))
  cat(sprintf("    of these, public_sector is NA in %s and 0 in %s\n",
              format(sum(is.na(ev$public_sector[.t12_ne])), big.mark = ","),
              format(sum(ev$public_sector[.t12_ne] == 0, na.rm = TRUE),
                     big.mark = ",")))
  cat(sprintf("    of these, union_member  is NA in %s and 0 in %s\n",
              format(sum(is.na(ev$union_member[.t12_ne])), big.mark = ","),
              format(sum(ev$union_member[.t12_ne] == 0, na.rm = TRUE),
                     big.mark = ",")))
  if (sum(is.na(ev$public_sector[.t12_ne])) == 0 && sum(.t12_ne) > 0) {
    cat("  CONFIRMED CONTAMINATED: no non-employed person-wave is NA on\n")
    cat("  public_sector. %in% is NA-blind, so the not-asked arrive as 0 and\n")
    cat("  read PRIVATE SECTOR.\n")
    # CROSS-VALIDATION, because union_member is the control that makes the
    # contamination countable. Its construction propagates NA correctly, so
    # its missing among the non-employed IS the never-asked set, and the
    # difference against public_sector's zeros is the genuine private
    # answers. If these do not reconcile, the two items are not routed
    # together and the count below is not the imputation.
    .n_never <- sum(is.na(ev$union_member[.t12_ne]))
    .n_zero  <- sum(ev$public_sector[.t12_ne] == 0, na.rm = TRUE)
    cat(sprintf("    never asked (union NA):        %s\n",
                format(.n_never, big.mark = ",")))
    cat(sprintf("    genuine private among the rest: %s\n",
                format(.n_zero - .n_never, big.mark = ",")))
    cat(sprintf("    imputed + genuine = %s against %s observed zeros: %s\n",
                format(.n_zero, big.mark = ","), format(.n_zero, big.mark = ","),
                if (.n_zero - .n_never >= 0) "reconciles" else
                  "DOES NOT RECONCILE, the two items are not co-routed"))
    cat(sprintf("    so %s person-waves are imputed private, %.1f%% of the\n",
                format(.n_never, big.mark = ","),
                100 * .n_never / max(sum(.t12_ne), 1)))
    cat("    non-employed.\n")
    # The residual is a reference-period disagreement rather than a fault:
    # `employed` is current status while the job module asks about the main
    # job, so a person can answer one and not the other.
    cat(sprintf("    %s non-employed person-waves carry a VALID job-module\n",
                format(sum(.t12_ne) - .n_never, big.mark = ",")))
    cat("    answer, which is the two items disagreeing on reference period\n")
    cat("    rather than a coding fault.\n")
    cat("  The employed gate below removes all of this from these estimates;\n")
    cat("  it does NOT fix the column for any other design that reads it.\n")
    rm(.n_never, .n_zero)
    note(paste("T12: union_member and public_sector coalesce the",
                "employment-routed missing to 0, the jbcasab fault. Every",
                "non-employed person-wave reads non-union and private.",
                "Fix in the loader before either column is used elsewhere."))
  }
  rm(.t12_ne)
  
  # ---------------------------------------------------------------------
  # 2. THE SPLIT IS PRE-BIRTH, NOT CONTEMPORANEOUS
  # ---------------------------------------------------------------------
  # Union membership and sector both move at a birth. Splitting on the
  # CURRENT value is Table 13's arms B and C, conditioning on an outcome
  # the event moves; splitting on a value fixed before treatment is arm E,
  # which is why arm E is the one that arm reports. Same distinction, same
  # choice.
  #
  # Treated persons take their status over k in [-3, -1], the window T4's
  # pre-birth restriction already uses. Never-treated persons have no
  # birth, so they take their first three observed waves. THE TWO RULES
  # ARE NOT THE SAME RULE: one is anchored on the event and one on panel
  # entry. Both are pre-treatment for the persons they apply to, which is
  # what the split needs, and the asymmetry is stated rather than hidden.
  .t12_pre_t <- ev %>%
    filter(treated == 1, employed == 1, !is.na(k), k >= -3, k <= -1) %>%
    dplyr::group_by(person_id) %>%
    summarise(pub_pre = as.integer(mean(public_sector, na.rm = TRUE) > 0.5),
              uni_pre = as.integer(mean(union_member,  na.rm = TRUE) > 0.5),
              .groups = "drop")
  .t12_pre_n <- ev %>%
    filter(treated == 0, employed == 1) %>%
    arrange(person_id, wave) %>%
    dplyr::group_by(person_id) %>%
    slice(1:3) %>%
    summarise(pub_pre = as.integer(mean(public_sector, na.rm = TRUE) > 0.5),
              uni_pre = as.integer(mean(union_member,  na.rm = TRUE) > 0.5),
              .groups = "drop")
  .t12_pre <- bind_rows(.t12_pre_t, .t12_pre_n)
  
  .d12 <- ev %>%
    filter(employed == 1) %>%
    dplyr::left_join(.t12_pre, by = "person_id") %>%
    filter(!is.na(pub_pre), !is.na(uni_pre)) %>%
    mutate(post_f     = post * female,
           post_pub   = post * pub_pre,
           post_f_pub = post * female * pub_pre,
           post_uni   = post * uni_pre,
           post_f_uni = post * female * uni_pre)
  
  cat(sprintf("\n  Estimation rows: %s of %s employed (%.1f%% lost to a missing\n",
              format(nrow(.d12), big.mark = ","),
              format(sum(ev$employed == 1, na.rm = TRUE), big.mark = ","),
              100 * (1 - nrow(.d12) / sum(ev$employed == 1, na.rm = TRUE))))
  cat("  pre-birth status, mostly persons never observed employed before\n")
  cat("  their first birth).\n")
  cat(sprintf("  public sector %.1f%% of persons; union %.1f%%.\n",
              100 * mean(.t12_pre$pub_pre, na.rm = TRUE),
              100 * mean(.t12_pre$uni_pre, na.rm = TRUE)))
  
  # ---------------------------------------------------------------------
  # 3. THE LEVELS. This is the part that is well powered.
  # ---------------------------------------------------------------------
  # If the compressed regime is the one the paper assumes, employer-
  # financed training should be HIGHER where wages are compressed. That is
  # a statement about levels, estimated on the whole employed sample, and
  # it does not need the event at all.
  cat("\n  Employer-financed training rate, by compression and sex:\n")
  .t12_lev <- .d12 %>%
    dplyr::group_by(female, pub_pre, uni_pre) %>%
    summarise(n = dplyr::n(),
              rate = mean(jttpewt_any, na.rm = TRUE),
              se = sqrt(rate * (1 - rate) / dplyr::n()),
              .groups = "drop") %>%
    filter(n >= MIN_N) %>%
    mutate(rate = round(rate, 4), se = round(se, 4)) %>%
    arrange(female, pub_pre, uni_pre)
  print(as.data.frame(.t12_lev), row.names = FALSE)
  
  # Defined BEFORE .t12_gap, which calls it. It was below, which parses
  # and fails on the first call: R resolves a closure's free variables at
  # call time, so the error surfaces as 'could not find function' inside
  # a function that is plainly correct.
  .mde_or <- if (exists("mde", mode = "function")) mde else function(s) 2.802 * s
  
  .t12_gap <- function(v, lab) {
    a <- mean(.d12$jttpewt_any[.d12[[v]] == 1], na.rm = TRUE)
    b <- mean(.d12$jttpewt_any[.d12[[v]] == 0], na.rm = TRUE)
    na_ <- sum(.d12[[v]] == 1, na.rm = TRUE); nb <- sum(.d12[[v]] == 0, na.rm = TRUE)
    s <- sqrt(a * (1 - a) / na_ + b * (1 - b) / nb)
    cat(sprintf("  %-14s %.4f against %.4f, difference %+.4f (SE %.4f, MDE %.4f)\n",
                lab, a, b, a - b, s, .mde_or(s)))
    # THE FOUR NUMBERS THE FOOTNOTE QUOTES ARE a AND b, and until this
    # returned them they existed only in the line above. Section 3.2's
    # premise rests on 37.6 against 18.8 and 36.6 against 19.5, and on the
    # 14 September pass Check 9 traced all four to T8_stacked_cc.rds and
    # T0b_missingness_by_group.csv, which have nothing to do with union
    # coverage or the public sector. They matched by coincidence, because
    # Check 9 asks whether a number appears somewhere in the corpus, not
    # whether it appears somewhere that could have produced it. A one
    # decimal value in the tens will collide across 82 artefacts.
    invisible(data.frame(variable = v, label = lab,
                         rate_yes = a, rate_no = b, diff = a - b, se = s,
                         mde = .mde_or(s), n_yes = na_, n_no = nb,
                         stringsAsFactors = FALSE))
  }
  cat("\n  Compression premium in the level:\n")
  .t12_gaps <- rbind(.t12_gap("pub_pre", "public sector"),
                     .t12_gap("uni_pre", "union"))
  cat("  A POSITIVE difference is the Acemoglu-Pischke premise showing up in\n")
  cat("  the data: the firm finances more where it can retain more of the\n")
  cat("  return. A negative or zero one does not refute the model, since\n")
  cat("  sector differs in much besides compression, but it removes the\n")
  cat("  easiest support for assuming the regime.\n")
  
  # ---------------------------------------------------------------------
  # 4. THE INTERACTION, ESTIMATED AND THEN GRADED
  # ---------------------------------------------------------------------
  # wave^pub_pre and wave^uni_pre are absorbed alongside the usual
  # sex-by-wave block. Sector-specific calendar shocks are exactly what a
  # training-budget cycle looks like, and without them a sector difference
  # in trend arrives as a sector difference in the birth response.
  #
  # pub_pre and uni_pre are person-constant by construction, so person FE
  # absorbs their main effects and only the interactions identify.
  for (.sp in c("pub", "uni")) {
    .term <- paste0("post_f_", .sp)
    .fml <- stats::as.formula(sprintf(
      "jttpewt_any ~ post + post_f + post_%s + %s | %s",
      .sp, .term, fe_block(paste0("wave^", .sp, "_pre"))))
    .m <- try(fixest::feols(.fml, data = .d12, cluster = ~person_id,
                            notes = FALSE, warn = FALSE), silent = TRUE)
    if (inherits(.m, "try-error")) {
      cat(sprintf("  [skip] T12_%s: fit failed.\n", .sp)); next
    }
    report(paste0("T12_", .sp, "_interaction"),
           paste("employer-financed withdrawal,", .sp, "interaction"),
           .m, .term, "pp", bm_pewt, bm_f_pewt,
           status = "descriptive (post-hoc)")
  }
  
  # THE GRADING IS THE RESULT HERE, and it was expected before the fit.
  # T1 gives -0.116 on 54,202 with SE 0.0156. A split at roughly a sixth of
  # the sample scales the thin arm's SE by about sqrt(6), their difference
  # by more, and the MDE lands near the size of the whole effect. Read the
  # MDE printed above against the effect it is being asked to split.
  cat("\n  READ THE MDE AGAINST THE EFFECT, NOT AGAINST ZERO. The quantity\n")
  cat("  being split is -0.116. An interaction MDE of the same order means\n")
  cat("  the design can detect only a heterogeneity as large as the whole\n")
  cat("  effect: 'zero in one arm and double in the other' is detectable,\n")
  cat("  anything gentler is not. On this project's own convention that is\n")
  cat("  a bound, and the precedent is explicit: the conditional-on-training\n")
  cat("  dosage regression was DROPPED rather than reported as a null\n")
  cat("  because its MDE of 21.1 exceeded its base of 17.9.\n")
  cat("\n  WHAT TO TAKE FROM T12. The LEVELS above, as evidence for the\n")
  cat("  regime Section 3.2 assumes. The interaction is reported with its\n")
  cat("  MDE and licenses a bound, not a heterogeneity claim, and the\n")
  cat("  public-sector arm would not separate compression from parental\n")
  cat("  leave entitlement even if it were powered.\n")
  
  note(paste("T12: the compression LEVEL is well powered and tests the",
              "Acemoglu-Pischke premise Section 3.2 assumes. The birth-response",
              "interaction is not; report it as a bound against its MDE."))
  
  # ARCHIVE THE LEVELS. They were computed, printed and dropped, so the
  # premise Section 3.2 assumes lived only in a console log that each run
  # overwrote: no ledger row, no artefact, nothing a snapshot could hold and
  # nothing Check 9 could trace. The interactions were registered because
  # they are coefficients; the levels are shares and fall through report().
  # That is why they need writing by hand.
  .t12_gaps$n_total   <- nrow(.d12)
  .t12_gaps$rate_yes_pct <- round(100 * .t12_gaps$rate_yes, 1)
  .t12_gaps$rate_no_pct  <- round(100 * .t12_gaps$rate_no,  1)
  utils::write.csv(.t12_gaps,
                   file.path(DIR_DESIGNS, "T12_compression_gaps.csv"),
                   row.names = FALSE)
  cat(sprintf(paste0("  wrote T12_compression_gaps.csv (%d row(s)). These carry",
                     " the four\n  percentages the Section 3.2 footnote quotes:",
                     " %s.\n"),
              nrow(.t12_gaps),
              paste(sprintf("%.1f/%.1f", .t12_gaps$rate_yes_pct,
                            .t12_gaps$rate_no_pct), collapse = " and ")))
  utils::write.csv(as.data.frame(.t12_lev),
                   file.path(DIR_DESIGNS, "T12_compression_levels.csv"),
                   row.names = FALSE)
  cat("  wrote T12_compression_levels.csv (", nrow(.t12_lev),
      " cell(s), female x pub_pre x uni_pre).\n", sep = "")

  rm(.t12_pre_t, .t12_pre_n, .t12_pre, .t12_lev, .d12, .t12_gap, .mde_or,
     .t12_gaps)
}
rm(.t12_need)

pn_flush()   # paper numbers registered above
source("03_finish.R")
