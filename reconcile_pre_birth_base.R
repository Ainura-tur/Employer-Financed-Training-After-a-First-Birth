# =====================================================================
# reconcile_pre_birth_base.R
#
# Why the pre-birth employer-financed base is 0.298 in
# T0_pre_birth_base_rates.csv (03_common.R) and 0.2867 / 0.3085 in the
# `bases` attribute of training_funding_divergence.R.
#
# The two differ on six things. This applies them one at a time, in the
# order they bite, and reports the base and the n after each step, so the
# residual after all six is either zero or a seventh difference nobody
# has named.
#
# READ-ONLY. Reads `panel` (or td_frame()) and writes CSVs to DIR_DESIGNS.
# No object any other script uses is created or modified. The two blocks at
# the foot reconcile two further claims: the validation of the birth
# transition, including what the 602 unmatched transitions are, and the
# experience level at which first births occur.
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr)
})

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

# --- logging ---------------------------------------------------------------
# WHY THIS FILE NEEDS ONE. It reconciles four figures the paper quotes, the
# pre-birth base, the 321 asked-not-employed person-waves, the composition
# of the 602 unmatched transitions, and the experience at a first birth, and
# until now its output existed only in whatever sink happened to be open.
# The CSVs carry the values; the log carries how they were reached,
# including the two target checks that warn when a figure no longer
# reproduces.
#
# THE SAME PATTERN AS referee8_couple_clustering.R, tested there on both
# paths. The branch td_log_open() took is read from the registry, not
# inferred from the sink count, which cannot tell a nested log from an
# orphan that was closed and replaced.
.rp_log <- exists("td_log_open", mode = "function")
if (.rp_log) {
  .rp_sink0 <- sink.number()
  .rp_try <- try(td_log_open("reconcile_pre_birth_base.R"), silent = TRUE)
  if (inherits(.rp_try, "try-error")) {
    cat("  td_log_open() failed: ", conditionMessage(attr(.rp_try, "condition")),
        "\n  Output goes to the console only.\n", sep = "")
  } else {
    .rp_f <- if (exists(".TD_LOG") && exists("file", envir = .TD_LOG))
      get("file", envir = .TD_LOG) else NA_character_
    if (is.na(.rp_f))
      cat("  no registry log is open; output is console only.\n")
    else if (grepl("reconcile_pre_birth_base", basename(.rp_f)))
      cat("  log: ", .rp_f,
          if (.rp_sink0 > 0)
            paste0("\n  (", .rp_sink0, " sink(s) were already open when this",
                   " started; the td_log_open line above says what it did",
                   " with them)") else "",
          "\n", sep = "")
    else
      cat("  writing into the caller's log: ", .rp_f, "\n", sep = "")
    rm(.rp_f)
  }
  rm(.rp_sink0, .rp_try)
} else {
  cat("  no contract loaded, so there is no log. Output is console only.\n")
}

# --- THE BODY RUNS INSIDE tryCatch(finally = close) -----------------------
# source() has no exit hook, so an error below would otherwise skip the
# close and leave this file's sink open, which is how every orphan sink in
# this project arose. finally runs on success and on error, the error still
# propagates after it, and top-level assignments still land in the global
# environment, where the waterfall's add() finds `steps`. Not re-indented,
# to keep the diff to the lines that changed.
.rp_ok <- FALSE
tryCatch({

PRE_LO <- -3L
PRE_HI <- -1L

# --- WHERE THE FIVE CSVs GO ------------------------------------------
# The fifth, T0n_never_treated_births.csv, sources E.1's 3.7 and 5.1.
# They were written to the working directory, which is why neither has
# ever been seen by a check: Check 9 scans designs, results and
# fte_experience, and a file beside the script is in none of them.
#
# THE BLOCKS FILE IS NOT A DIAGNOSTIC. It sources four numbers the
# manuscript prints in Online Appendix B, under Routing and imputation:
# the two blocks that move the base
# hold 130 men and 191 women asked without being flagged employed, which
# is the paper's 321, at rates 0.0846 and 0.0995, which are its 8.5 and
# 10.0 per cent. Those were typed. Written here they are checked.
#
# DESIGNS RATHER THAN DIAGNOSTICS, and the distinction is the one
# Pre_analysis_diagnostics2.R draws: a fragment nobody reads twice is
# triage, and a csv a later stage or a check reads is data. Both of these
# are read, so both go where the ledgers go.
.rp_dir <- if (exists("DIR_DESIGNS") && is.character(DIR_DESIGNS) &&
               nzchar(DIR_DESIGNS)) DIR_DESIGNS else "."
if (!identical(.rp_dir, ".")) dir.create(.rp_dir, showWarnings = FALSE,
                                         recursive = TRUE)
OUT        <- file.path(.rp_dir, "reconcile_pre_birth_base.csv")
OUT_BLOCKS <- file.path(.rp_dir, "reconcile_pre_birth_base_blocks.csv")
cat("Writing to  :", .rp_dir,
    if (identical(.rp_dir, ".")) "  (no contract loaded)" else "", "\n")

# ---------------------------------------------------------------------
# The frame. Prefer the sealed accessor; fall back to an ambient panel.
# ---------------------------------------------------------------------
.p <- if (exists("td_frame", mode = "function")) {
  td_frame(need_cols = c("person_id", "wave", "female", "employed",
                         "ever_parent"))
} else if (exists("panel")) {
  get("panel", envir = globalenv())
} else {
  stop("No panel. Source 00_contract.R or load the panel first.")
}
.p <- as.data.frame(.p)

cat(sprintf("Frame: %s person-waves, %s persons\n",
            format(nrow(.p), big.mark = ","),
            format(dplyr::n_distinct(.p$person_id), big.mark = ",")))

# ---------------------------------------------------------------------
# Two cohort rules, kept separate on purpose.
#
# rule A is 03_common.R: drop anyone already a parent at entry, birth
# wave is the first wave the indicator is 1.
# rule B is training_funding_divergence.R: the first positive first
# difference, with first()/last() endpoints and no missing filter.
# ---------------------------------------------------------------------
coh_A <- .p %>%
  filter(!is.na(ever_parent)) %>%
  arrange(person_id, wave) %>%
  group_by(person_id) %>%
  summarise(ep_at_entry = ever_parent[which.min(wave)],
            ever        = max(ever_parent, na.rm = TRUE),
            birth_wave  = if (any(ever_parent == 1))
              min(wave[ever_parent == 1]) else NA_integer_,
            .groups = "drop") %>%
  filter(ep_at_entry == 0) %>%
  mutate(cohort_A = ifelse(ever == 0, 10000, as.numeric(birth_wave))) %>%
  select(person_id, cohort_A)

coh_B <- .p %>%
  arrange(person_id, wave) %>%
  group_by(person_id) %>%
  summarise(ep_first = first(ever_parent),
            ep_last  = last(ever_parent),
            trans    = { d_ep <- diff(ever_parent)
                         idx  <- which(!is.na(d_ep) & d_ep > 0)
                         if (length(idx)) wave[idx[1] + 1] else NA_integer_ },
            .groups = "drop") %>%
  mutate(cohort_B = case_when(
    !is.na(trans)                                        ~ as.numeric(trans),
    !is.na(ep_first) & ep_first == 0 &
      !is.na(ep_last) & ep_last == 0                     ~ 10000,
    TRUE                                                 ~ NA_real_)) %>%
  select(person_id, cohort_B)

.rule_gap <- coh_A %>%
  full_join(coh_B, by = "person_id") %>%
  summarise(only_A = sum(!is.na(cohort_A) &  is.na(cohort_B)),
            only_B = sum( is.na(cohort_A) & !is.na(cohort_B)),
            both   = sum(!is.na(cohort_A) & !is.na(cohort_B)),
            disagree = sum(!is.na(cohort_A) & !is.na(cohort_B) &
                             cohort_A != cohort_B))
cat("\nCohort rules, person counts:\n"); print(as.data.frame(.rule_gap),
                                              row.names = FALSE)
if (.rule_gap$disagree > 0)
  cat("*** The two rules assign different birth waves to", .rule_gap$disagree,
      "persons. Every step below inherits that. ***\n")

# ---------------------------------------------------------------------
# Training-module waves, the tpanel restriction. Defined here the way the
# divergence script defines it: a wave counts if the employer-financed
# item is observed for anyone in it.
# ---------------------------------------------------------------------
.obs_col <- intersect(c("jttpewt_obs"), names(.p))[1]
.any_col <- intersect(c("jttpewt_any"), names(.p))[1]
.own_obs <- intersect(c("jttpeot_obs"), names(.p))[1]
if (is.na(.any_col)) stop("panel has no jttpewt_any")

tr_waves <- if (!is.na(.obs_col)) {
  .p %>% group_by(wave) %>%
    summarise(ok = any(!is.na(.data[[.obs_col]])), .groups = "drop") %>%
    filter(ok) %>% pull(wave)
} else sort(unique(.p$wave))
cat(sprintf("\nTraining module observed in waves %d-%d (%d of %d).\n",
            min(tr_waves), max(tr_waves), length(tr_waves),
            dplyr::n_distinct(.p$wave)))

# ---------------------------------------------------------------------
# One base, computed on whatever frame and column it is handed.
# ---------------------------------------------------------------------
base_of <- function(d, ycol, cohort_col) {
  k <- d[[cohort_col]]
  d$.rel <- ifelse(!is.na(k) & k < 10000, d$wave - k, NA_real_)
  d <- d[!is.na(d$.rel) & d$.rel >= PRE_LO & d$.rel <= PRE_HI, , drop = FALSE]
  d <- d[!is.na(d[[ycol]]), , drop = FALSE]
  if (!nrow(d)) return(c(n = 0, women = NA_real_, men = NA_real_))
  w <- mean(d[[ycol]][d$female == 1]); m <- mean(d[[ycol]][d$female == 0])
  c(n = nrow(d), women = w, men = m)
}

steps <- list()
add <- function(label, d, ycol, cohort_col) {
  b <- base_of(d, ycol, cohort_col)
  steps[[length(steps) + 1L]] <<- data.frame(
    step = length(steps) + 1L, label = label, column = ycol,
    n = unname(b["n"]), women = round(unname(b["women"]), 4),
    men = round(unname(b["men"]), 4), stringsAsFactors = FALSE)
}

d0 <- .p %>% left_join(coh_A, by = "person_id") %>%
  left_join(coh_B, by = "person_id")

# 0. 03_common.R as written.
add("03_common: rule A, all waves, employed, jttpewt_any",
    filter(d0, employed == 1), .any_col, "cohort_A")

# 1. + the training-wave restriction.
add("+ training waves only",
    filter(d0, employed == 1, wave %in% tr_waves), .any_col, "cohort_A")

# 2. + the divergence cohort rule.
add("+ cohort rule B (diff of ever_parent)",
    filter(d0, employed == 1, wave %in% tr_waves), .any_col, "cohort_B")

# 3. + the gated column.
if (!is.na(.obs_col))
  add("+ jttpewt_obs instead of jttpewt_any",
      filter(d0, employed == 1, wave %in% tr_waves), .obs_col, "cohort_B")

# 4. + listwise on both items of the pair.
if (!is.na(.obs_col) && !is.na(.own_obs))
  add("+ listwise on both _obs items",
      filter(d0, employed == 1, wave %in% tr_waves,
             !is.na(.data[[.obs_col]]), !is.na(.data[[.own_obs]])),
      .obs_col, "cohort_B")

# 5. drop the employment filter: this is the divergence unconditional arm.
if (!is.na(.obs_col) && !is.na(.own_obs))
  add("- employed filter  = pe_gated | unconditional",
      filter(d0, wave %in% tr_waves,
             !is.na(.data[[.obs_col]]), !is.na(.data[[.own_obs]])),
      .obs_col, "cohort_B")

# 6. restore it and drop the self-employed: the employed_only arm.
if (!is.na(.obs_col) && !is.na(.own_obs)) {
  d6 <- filter(d0, employed == 1, wave %in% tr_waves,
               !is.na(.data[[.obs_col]]), !is.na(.data[[.own_obs]]))
  if ("is_self_employed" %in% names(d6))
    d6 <- filter(d6, is.na(is_self_employed) | is_self_employed != 1)
  else
    cat("\nNOTE: is_self_employed absent; step 7 omits the self-employed drop.\n")
  add("+ employed, self-employed dropped = pe_gated | employed_only",
      d6, .obs_col, "cohort_B")
}

tab <- do.call(rbind, steps)
tab$d_women <- c(NA, round(diff(tab$women), 4))
tab$d_men   <- c(NA, round(diff(tab$men),   4))

cat("\n")
cat(strrep("=", 78), "\n")
cat("Pre-birth employer-financed base, k =", PRE_LO, "to", PRE_HI,
    ", treated only\n")
cat(strrep("=", 78), "\n")
print(tab, row.names = FALSE)

# THE TARGET WAS TYPED FROM A ROUNDED READING AND IS CORRECTED HERE.
# It said 0.2980 / 0.2050. T0_pre_birth_base_rates.csv holds 0.2981 and
# 0.2046, and Check 3 of verify_manuscript_numbers.R reads those two. The
# women's figure was rounding; the men's was not, and a target 0.0004 away
# from the artefact makes every row of the waterfall look as though it
# carries a residual it does not.
cat("\nTargets: 03_common writes 0.2981 women / 0.2046 men",
    "(T0_pre_birth_base_rates.csv).\n")
cat("         divergence prints 0.2867 / 0.1994 unconditional",
    "and 0.3085 / 0.2281 employed.\n")
cat("A residual at the last two rows is a seventh difference nobody has named.\n")
# ON 10 SEPTEMBER THERE WAS NO RESIDUAL. The two cohort rules agreed on
# all 10,650 persons with none in dispute, steps 1 to 5 left the base
# untouched, and the last two rows landed on the divergence module's
# published pairs exactly. Six named differences, two of which bite, and
# nothing left over. Recorded here because a reconciliation that closes
# is worth as much as one that does not, and is easier to forget.

write.csv(tab, OUT, row.names = FALSE)
cat("\nWrote", OUT, "\n")

# =====================================================================
# BLOCK COMPOSITION
#
# The waterfall shows only two steps move the base. Both are population
# changes, so report the populations rather than leaving their size and
# training rate to be inferred from the base movements.
# =====================================================================
if (!is.na(.obs_col) && !is.na(.own_obs)) {

  block_of <- function(d, ycol, cohort_col, label) {
    k <- d[[cohort_col]]
    d$.rel <- ifelse(!is.na(k) & k < 10000, d$wave - k, NA_real_)
    d <- d[!is.na(d$.rel) & d$.rel >= PRE_LO & d$.rel <= PRE_HI &
             !is.na(d[[ycol]]), , drop = FALSE]
    if (!nrow(d)) return(NULL)
    d %>% group_by(female) %>%
      summarise(block = label, n = n(),
                rate = round(mean(.data[[ycol]]), 4), .groups = "drop") %>%
      select(block, female, n, rate)
  }

  .asked <- filter(d0, wave %in% tr_waves,
                   !is.na(.data[[.obs_col]]), !is.na(.data[[.own_obs]]))

  # Rows the employed filter excludes but the module still reached.
  b_routed <- block_of(filter(.asked, is.na(employed) | employed != 1),
                       .obs_col, "cohort_B", "asked, not flagged employed")

  # Rows DROP_SELFEMP removes from the employed arm.
  b_selfemp <- if ("is_self_employed" %in% names(.asked))
    block_of(filter(.asked, employed == 1, !is.na(is_self_employed),
                    is_self_employed == 1),
             .obs_col, "cohort_B", "employed, self-employed") else NULL

  comp <- bind_rows(b_routed, b_selfemp)
  if (!is.null(comp) && nrow(comp)) {
    cat("\n")
    cat(strrep("-", 78), "\n")
    cat("The two blocks that move the base\n")
    cat(strrep("-", 78), "\n")
    print(as.data.frame(comp), row.names = FALSE)
    cat("\nfemale = 1 is women. `rate` is the employer-financed rate in that\n")
    cat("block, to be read against 0.2981 women / 0.2046 men in the employed\n")
    cat("pre-birth window. A block far below those is what moves the base.\n")
    cat("\nOnline Appendix B quotes this table, under Routing and imputation: the two rows of\n")
    cat("the first block sum to the 321 person-waves asked without being\n")
    cat("flagged employed, and their rates are its 8.5 and 10.0 per cent.\n")
    write.csv(comp, OUT_BLOCKS, row.names = FALSE)
    cat("Wrote", OUT_BLOCKS, "\n")
  }
}


# =====================================================================
# THE BIRTH TRANSITION: WHAT THE 602 UNMATCHED TRANSITIONS ARE
# =====================================================================
#
# THE CLAIM. Section 4 validates the transition against the birth item and
# says the transitions that do not match "mix genuine births recorded
# outside the reference window with household changes such as a child
# joining the household". That is testable, and it has not been tested.
#
# THE SPLIT THE PAPER'S FIGURES USE. A transition is ever_parent going from
# 0 to 1 within person, the rule in Pre_analysis_diagnostics2.R. Of 2,704,
# the birth item is observed on 2,388 and reports a birth on 2,102, which is
# the 77.7 percent. So the 602 unmatched are two different things: 316 with
# no birth item at all, and 286 with an item that says no birth. The paper
# treats them as one group.
#
# 2,102, NOT 2,101. The first version of this block targeted 2,101, back-
# computed from the rounded 88.0 percent: 0.880 x 2,388 = 2,101.4. The count
# is 2,102, which also rounds to 88.0, and the run on 22 September warned on
# a mismatch that was the target's fault. It is the error the pre-birth base
# comment above records: a target typed from a rounded reading.
#
# THE TEST. The validation window looks back one wave. A birth recorded
# LATE would show on the birth item at the next wave, and a real birth
# should usually have a pregnancy reported at or before it. Each unmatched
# transition goes to the first class it qualifies for:
#   1  birth item reports a birth at the next wave
#   2  pregnancy item reports a pregnancy at this wave or the one before
#   3  an item was observed and none corroborates a birth
#   4  no item observed in the window, so it cannot be classified
# Class 1 is the paper's "births recorded outside the reference window"
# directly. Class 3 is where a household change would sit.
#
# NEXT AND PREVIOUS MEAN THE ADJACENT OBSERVED WAVE, as in the validation
# code, which uses lag() without requiring consecutive waves. A gap in the
# person's record is therefore not the same as a missing item, and the
# counts below keep them apart.
.need_v <- c("had_child_this_wave", "had_pregnancy_this_wave",
             "experience_years", "ln_hourly_wage_real")
.miss_v <- setdiff(.need_v, names(.p))
if (length(.miss_v)) {
  cat("\n  Birth-validation blocks skipped; the frame lacks:",
      paste(.miss_v, collapse = ", "), "\n")
} else {
  .tr <- .p %>%
    arrange(person_id, wave) %>%
    group_by(person_id) %>%
    mutate(prev_ep = dplyr::lag(ever_parent),
           bx      = had_child_this_wave,
           bx_next = dplyr::lead(had_child_this_wave),
           px      = had_pregnancy_this_wave,
           px_prev = dplyr::lag(had_pregnancy_this_wave)) %>%
    ungroup() %>%
    filter(!is.na(prev_ep), prev_ep == 0, ever_parent == 1)

  .n_all <- nrow(.tr)
  .n_obs <- sum(!is.na(.tr$bx))
  .n_val <- sum(.tr$bx == 1, na.rm = TRUE)
  cat("\n", strrep("=", 78), "\n", sep = "")
  cat("BIRTH TRANSITION: THE UNMATCHED TRANSITIONS\n")
  cat(strrep("=", 78), "\n", sep = "")
  cat(sprintf("  transitions %s, birth item observed on %s, reporting a birth on %s\n",
              format(.n_all, big.mark = ","), format(.n_obs, big.mark = ","),
              format(.n_val, big.mark = ",")))
  cat(sprintf("  validated: %.1f%% of all transitions, %.1f%% of those with the item\n",
              100 * .n_val / .n_all, 100 * .n_val / .n_obs))
  # THE TARGET IS THE PAPER'S OWN SPLIT. A mismatch warns rather than
  # stops: the sample can legitimately change, but then the classes below
  # describe a different group from the one Section 4 reports.
  if (.n_all != 2704L || .n_obs != 2388L || .n_val != 2102L)
    cat("  *** These do not reproduce 2,704 / 2,388 / 2,102, so the classes\n",
        "      below are not the group behind the paper's 77.7 percent.\n",
        "      Reconcile before quoting them.\n", sep = "")

  .un <- .tr %>%
    filter(is.na(bx) | bx == 0) %>%
    mutate(
      item = ifelse(is.na(bx), "birth item missing", "birth item says no birth"),
      class = dplyr::case_when(
        !is.na(bx_next) & bx_next == 1 ~ "1 birth recorded next wave",
        (!is.na(px) & px == 1) | (!is.na(px_prev) & px_prev == 1) ~
          "2 pregnancy reported",
        !is.na(bx) | !is.na(bx_next) | !is.na(px) | !is.na(px_prev) ~
          "3 observed, no corroboration",
        TRUE ~ "4 no item observed"))

  .tab_v <- .un %>%
    count(item, class, name = "n") %>%
    group_by(item) %>%
    mutate(pct_of_item = round(100 * n / sum(n), 1)) %>%
    ungroup()
  .tot_v <- .un %>%
    count(class, name = "n") %>%
    mutate(item = "all unmatched", pct_of_item = round(100 * n / sum(n), 1))
  .out_v <- bind_rows(.tab_v, .tot_v)
  cat("\n")
  print(as.data.frame(.out_v), row.names = FALSE)

  .sx_v <- .un %>% count(item, female, name = "n")
  cat("\n  by sex (female = 1 is women):\n")
  print(as.data.frame(.sx_v), row.names = FALSE)

  .corr <- sum(.un$class %in% c("1 birth recorded next wave",
                                "2 pregnancy reported"))
  cat(sprintf(paste0("\n  %d of %d unmatched transitions (%.0f%%) are corroborated",
                     " as births, by a\n  birth reported at the next wave or a",
                     " pregnancy reported at or before it.\n"),
              .corr, nrow(.un), 100 * .corr / nrow(.un)))
  cat("  Class 3 is where a household change would sit, and class 4 cannot\n",
      "  be told apart from either. A large class 1 or 2 means misclassification\n",
      "  attenuates the pooled estimate, as the paper argues, rather than\n",
      "  generating it.\n", sep = "")

  OUT_UNMATCHED <- file.path(.rp_dir, "T0v_unmatched_transitions.csv")
  write.csv(.out_v, OUT_UNMATCHED, row.names = FALSE)
  cat("Wrote", OUT_UNMATCHED, "\n")

  # ===================================================================
  # EXPERIENCE AT THE FIRST BIRTH
  # ===================================================================
  #
  # THE CLAIM. The P2 paragraph says the never-mothers' mean experience,
  # 10.9 years, "is where most first births occur and where the event study
  # identifies". "Most" is a strong word: it needs more than half of first
  # births within a band a reader would accept as near 10.9. So this
  # reports the median and interquartile range and the share within 2, 3
  # and 5 years, on two frames. At the birth wave only about half of
  # mothers have a wage, and the P2 figure is drawn on the wage sample, so
  # the two can differ.
  .eb <- .tr %>% filter(female == 1)
  .rep28 <- function(x, frame) {
    x <- x[!is.na(x)]
    q <- stats::quantile(x, c(0.25, 0.5, 0.75))
    data.frame(frame = frame, n = length(x),
               median = round(q[[2]], 1), q25 = round(q[[1]], 1),
               q75 = round(q[[3]], 1),
               within_2 = round(100 * mean(abs(x - 10.9) <= 2), 1),
               within_3 = round(100 * mean(abs(x - 10.9) <= 3), 1),
               within_5 = round(100 * mean(abs(x - 10.9) <= 5), 1))
  }
  .out_e <- rbind(
    .rep28(.eb$experience_years, "all mothers at the birth wave"),
    .rep28(.eb$experience_years[!is.na(.eb$ln_hourly_wage_real)],
           "mothers at the birth wave with a wage"))
  cat("\n", strrep("=", 78), "\n", sep = "")
  cat("EXPERIENCE AT THE FIRST BIRTH, AGAINST THE 10.9-YEAR CLAIM\n")
  cat(strrep("=", 78), "\n", sep = "")
  print(.out_e, row.names = FALSE)
  cat("\n  within_k is the percent of first births within k years of 10.9.\n",
      "  'Most' needs more than half within a band a reader would accept.\n",
      "  If the median is close but within_3 is below 50, 'where first births\n",
      "  typically occur' is the defensible wording.\n", sep = "")
  OUT_EXPER <- file.path(.rp_dir, "T0e_experience_at_birth.csv")
  write.csv(.out_e, OUT_EXPER, row.names = FALSE)
  cat("Wrote", OUT_EXPER, "\n")

  rm(.tr, .n_all, .n_obs, .n_val, .un, .tab_v, .tot_v, .out_v, .sx_v, .corr,
     .eb, .rep28, .out_e)

  # -------------------------------------------------------------------
  # NEVER-TREATED BIRTH REPORTS (Online Appendix E.1)
  # -------------------------------------------------------------------
  # THE CLAIM. E.1 says never-treated birth reports run at 3.7 percent for
  # men against 5.1 for women, so the parenthood rule is not differentially
  # missing fathers. Nothing on disk computed it, and CS_event_study.R
  # could not, because the event-study frame lacks had_child_this_wave.
  # This frame has it.
  #
  # NEVER-TREATED IS RULE A'S: no child at entry and none later
  # (cohort_A = 10000), the roster the estimates use.
  #
  # WHICH BASE. The paper does not say whether its shares are of persons
  # or of person-waves, so both are printed; the one that reproduces 3.7
  # and 5.1 is the one it used.
  #
  # WHAT A REPORT MEANS. A report in the person's last observed wave cannot
  # be followed up. A report followed by later waves in which no child is
  # ever recorded is the case that needs a reason.
  .nt_ids <- coh_A$person_id[coh_A$cohort_A == 10000]
  .nt <- .p[.p$person_id %in% .nt_ids, , drop = FALSE]
  .nt <- .nt[order(.nt$person_id, .nt$wave), , drop = FALSE]
  .nt$bx <- suppressWarnings(as.numeric(.nt$had_child_this_wave))
  .has_age  <- "age" %in% names(.nt)
  .has_part <- "partnered" %in% names(.nt)
  if (.has_age)  .nt$age       <- suppressWarnings(as.numeric(.nt$age))
  if (.has_part) .nt$partnered <- suppressWarnings(as.numeric(.nt$partnered))
  .nt_pp <- .nt %>%
    group_by(person_id, female) %>%
    summarise(asked     = any(!is.na(bx)),
              reported  = any(bx == 1, na.rm = TRUE),
              first_rep = if (any(bx == 1, na.rm = TRUE))
                            min(wave[which(bx == 1)]) else NA_real_,
              last_wave = max(wave),
              age_rep   = if (.has_age && any(bx == 1, na.rm = TRUE))
                            age[which(bx == 1)[1]] else NA_real_,
              part_rep  = if (.has_part && any(bx == 1, na.rm = TRUE))
                            partnered[which(bx == 1)[1]] else NA_real_,
              pw_asked  = sum(!is.na(bx)),
              pw_rep    = sum(bx == 1, na.rm = TRUE),
              .groups = "drop")
  .out_n <- .nt_pp %>%
    group_by(female) %>%
    summarise(persons           = n(),
              asked             = sum(asked),
              reporting         = sum(reported),
              pct_of_persons    = round(100 * sum(reported) / n(), 1),
              pct_of_asked      = round(100 * sum(reported) / max(sum(asked), 1), 1),
              pct_of_pw_asked   = round(100 * sum(pw_rep) / max(sum(pw_asked), 1), 2),
              report_last_wave  = sum(reported & first_rep == last_wave, na.rm = TRUE),
              report_then_later = sum(reported & first_rep <  last_wave, na.rm = TRUE),
              more_than_one     = sum(pw_rep > 1),
              median_age_report = if (.has_age)
                                    stats::median(age_rep[reported], na.rm = TRUE) else NA_real_,
              partnered_at_report = if (.has_part)
                                    sum(part_rep[reported] == 1, na.rm = TRUE) else NA_real_,
              .groups = "drop") %>%
    mutate(sex = ifelse(female == 1, "women", "men")) %>%
    select(sex, everything(), -female)
  cat("\n", strrep("=", 78), "\n", sep = "")
  cat("NEVER-TREATED BIRTH REPORTS, AGAINST THE 3.7 AND 5.1 PERCENT CLAIM\n")
  cat(strrep("=", 78), "\n", sep = "")
  print(as.data.frame(.out_n), row.names = FALSE)
  cat("\n  E.1 says 3.7 percent for men and 5.1 for women. Whichever pct_ column\n",
      "  reproduces them is the base the paper used. report_then_later counts\n",
      "  people who report a birth, stay in the panel, and are never recorded\n",
      "  with a child: those are the ones E.1 has to explain.\n", sep = "")
  OUT_NEVER <- file.path(.rp_dir, "T0n_never_treated_births.csv")
  write.csv(.out_n, OUT_NEVER, row.names = FALSE)
  cat("Wrote", OUT_NEVER, "\n")
  rm(.nt_ids, .nt, .has_age, .has_part, .nt_pp, .out_n)
}
rm(.need_v, .miss_v)

.rp_ok <- TRUE          # the last line of the body: reaching it IS success
}, finally = {
  # CLOSE ONLY WHAT THIS FILE OPENED, ON EVERY PATH, and say which path.
  # td_log_close() defaults to "completed", so without the flag a run that
  # stopped on a missing frame would be stamped as a success.
  if (isTRUE(.rp_log) && exists("td_log_close", mode = "function"))
    try(td_log_close(if (isTRUE(.rp_ok)) "completed" else "stopped"),
        silent = TRUE)
  rm(.rp_log, .rp_ok)
})
