# =============================================================================
# robust_30_49.R   The headline estimates on the conventional 30-49 window
#
# WHAT IT DOES. Section 4 uses ages 20 to 55 and says why, and it promises the
# conventional 30-49 restriction as a robustness check. This script delivers
# it. It refits six headline quantities on person-waves aged 30 to 49:
#
#   sa     employer-financed training, Sun-Abraham average
#          over k >= 0, employed only, the paper's headline cs_post_period_att.csv
#   emp    employer-financed training, employed only        T1_jttpewt_any_employed
#   att    employer-financed training, employed or on leave T1_attached
#   own    own-time training, employed only                 T1_jttpeot_any_employed
#   ddd    the financing triple difference, sex-specific
#          source-by-wave path, as Section 6.1 reports it   T3_ddd
#   wage   the wage gap's late window less its early window
#          (years 5-10 less years 0-4), log points          wage_path_tests.csv
#
# REPRODUCE FIRST. Every quantity is fitted twice, on the full frame and then
# on the window. The full-frame fit must reproduce the published value: the
# sealed coefficient and standard error in BLIND_training_estimates.rds and
# the ledger's n for the four static arms, cs_post_period_att.csv for the
# Sun-Abraham average and wage_path_tests.csv for the wage window. If any
# does not, the script writes what it found and stops before the window fits.
# A window estimate from a specification that is not the paper's would
# compare nothing with nothing.
#
# v-cw, 14 OCTOBER. THE TWO SUN-ABRAHAM ARMS FOLLOW THE WEIGHTS CS PUBLISHED.
# With the common-weight patch (sa_common_weights.R and sa_patches.R),
# CS_event_study.R's gap row is the women-minus-men difference formed inside
# each cohort-by-event cell and averaged on pooled weights, the earlier
# estimator moves to the gap_separate row, and the wage tests use the common-
# weight path. A gap_separate row in cs_post_period_att.csv is the sign, and
# then sa and wage are formed the same way here, from the same fits, so the
# full window reproduces the paper and the 30-49 window uses its estimator.
# Without that row, both arms difference the two sexes' own averages, as
# before. The csv's `weights` column says which.
#
# THE SPECIFICATIONS ARE COPIED, NOT REWRITTEN. The four training arms are
# 03a's and 03b's formulas: feols(y ~ post + post_f | person_id + wave^female)
# for the three T1 arms, and the stacked employer-and-own-time triple
# difference with person^source and wave^source^female effects. The cohorts,
# the Sun-Abraham fit, the covariance helpers and the window arithmetic are
# CS_event_study.R's own code, reproduced below with its names.
#
# THE WINDOW IS APPLIED TO PERSON-WAVES, NOT TO PEOPLE. Event time comes from
# each person's full history on the frame, as in the main analysis, and only
# then are waves outside ages 30-49 dropped. Recomputing the cohorts inside
# the window would turn every mother whose first birth came before 30 into a
# left-censored parent, and drop her.
#
# POST-HOC, AND IT WRITES NOTHING THE BLINDING OWNS. The check was added after
# the estimates were unsealed. It writes neither the ledger nor the sealed
# estimates; its outputs are robust_30_49.csv in output/diagnostics and its log.
#
# NOT IN output/results. That folder holds MASTER's artefacts, and the
# verifier's Check 7b dates the timing paths against the newest file in it, so
# a csv written there this morning made the 24 September paths look 2.8 days
# stale. Diagnostics is where the referee scripts write, and paper_tables.R
# searches it. A copy left in output/results by an earlier run is removed.
#
# RUN from the project root: source("robust_30_49.R"). It loads 00_contract.R
# when the session lacks it and stops if it cannot.
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(fixest) })

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

# ---- the contract -------------------------------------------------------------
# The frame must be td_frame()'s, screened and stamped. Sourced here when the
# session lacks it, found the way paper_tables.R finds it.
.rb_ok <- function()
  exists("td_frame", mode = "function") &&
  exists("td_apply_sample_rule", mode = "function") && exists("PROJ_ROOT")
if (!.rb_ok()) {
  .c <- c("00_contract.R", file.path("..", "00_contract.R"))
  .c <- .c[file.exists(.c)]
  if (!length(.c))
    stop("00_contract.R not found from ", getwd(), ". setwd() to the ",
         "project root and source this file again.", call. = FALSE)
  cat("Contract: not loaded in this session; sourcing ",
      normalizePath(.c[1], winslash = "/"), "\n", sep = "")
  source(.c[1])
  rm(.c)
  if (!.rb_ok())
    stop("00_contract.R was loaded but does not provide td_frame(), ",
         "td_apply_sample_rule() and PROJ_ROOT.", call. = FALSE)
}
.rb_designs <- if (exists("DIR_DESIGNS")) DIR_DESIGNS else
  file.path(PROJ_ROOT, "output", "designs")
.rb_results <- if (exists("DIR_RESULTS")) DIR_RESULTS else
  file.path(PROJ_ROOT, "output", "results")
.rb_diag    <- if (exists("DIR_DIAGNOSTICS")) DIR_DIAGNOSTICS else
  file.path(PROJ_ROOT, "output", "diagnostics")
.rb_logdir  <- if (exists("LOG_DIR")) LOG_DIR else file.path(PROJ_ROOT, "run_logs")
dir.create(.rb_diag, showWarnings = FALSE, recursive = TRUE)
dir.create(.rb_logdir,  showWarnings = FALSE, recursive = TRUE)

# ---- the log ------------------------------------------------------------------
# Opened only if nothing else is recording, and closed in `finally`, so an
# error anywhere below cannot leave a sink open.
.rb_owns <- sink.number() == 0L
.rb_log  <- file.path(.rb_logdir, paste0("robust_30_49_",
                      format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
if (.rb_owns) sink(.rb_log, split = TRUE)

# NOTHING IS LEFT IN THE SESSION. The body runs in local(), so its objects,
# ev among them, cannot reach a later script: 03a reuses an `ev` it finds in
# the session instead of rebuilding it.
tryCatch(local({

# ---- settings ---------------------------------------------------------------
RB_AGE_LO <- 30L
RB_AGE_HI <- 49L
RB_TOL    <- 5e-4     # allowed |refit - published| on estimate and SE
RB_MDE    <- 2.802    # qnorm(.975) + qnorm(.8), as 04financing_space.R
K         <- 8L       # CS_event_study.R's binning constant
WT_EARLY  <- 0:4      # CS_event_study.R's windows
WT_LATE   <- 5:10

cat(strrep("=", 78), "\n", sep = "")
cat("robust_30_49.R  started ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", sep = "")
cat(strrep("=", 78), "\n", sep = "")
if (.rb_owns) cat("  log: ", .rb_log, "\n", sep = "")

# ---- CS_event_study.R's helpers, verbatim -----------------------------------
clean_num <- function(x) { x <- unclass(x); attributes(x) <- NULL
if (is.numeric(x)) x else suppressWarnings(as.numeric(as.character(x))) }

BIN <- stats::setNames(list((-200):(-K), K:200), c(as.character(-K), as.character(K)))
fit <- function(d, y, w = NULL, fe = "id + wave") {
  rhs <- "sunab(cohort, wave, bin.rel = BIN)"
  f <- as.formula(paste0(y, " ~ ", rhs, " | ", fe))
  m <- tryCatch(feols(f, data = d, weights = w, cluster = ~ id),
                error = function(e) NULL)
  if (is.null(m))                                  # fallback: no binning
    m <- feols(as.formula(paste0(y, " ~ sunab(cohort, wave) | ", fe)),
               data = d, weights = w, cluster = ~ id)
  m
}
.sunab_agg_vcov <- function(m, agg_names) {
  V <- tryCatch(stats::vcov(m), error = function(e) NULL)
  if (is.null(V) || is.null(rownames(V))) return(NULL)
  rn  <- rownames(V)
  sa  <- grepl(":cohort::", rn, fixed = TRUE)
  per <- sub(":cohort::.*$", "", rn)
  mm  <- tryCatch(stats::model.matrix(m), error = function(e) NULL)
  if (is.null(mm)) return(NULL)
  W <- matrix(0, length(agg_names), length(rn), dimnames = list(agg_names, rn))
  for (a in agg_names) {
    cols <- rn[sa & per == a]
    if (!length(cols) || !all(cols %in% colnames(mm))) return(NULL)
    x  <- sign(mm[, cols, drop = FALSE])
    sh <- if (!is.null(m$weights)) colSums(m$weights * x) else colSums(x)
    W[a, cols] <- sh / sum(sh)
  }
  W %*% V %*% t(W)
}
.event_vcov <- function(m) {
  b  <- stats::coef(m)
  nm <- names(b)
  rt <- suppressWarnings(as.numeric(sub(".*::(-?\\d+).*", "\\1", nm)))
  ok <- !is.na(rt) & grepl("::", nm)
  if (!any(ok)) return(NULL)
  s  <- unname(fixest::se(m))[ok]
  pick <- function(V) {
    if (is.null(V) || !all(nm[ok] %in% rownames(V))) return(NULL)
    V <- V[nm[ok], nm[ok], drop = FALSE]
    if (max(abs(sqrt(pmax(diag(V), 0)) - s)) > 1e-6 * max(1, max(s))) return(NULL)
    V
  }
  V <- pick(tryCatch(stats::vcov(m), error = function(e) NULL))
  if (is.null(V)) V <- pick(tryCatch(stats::vcov(summary(m)), error = function(e) NULL))
  if (is.null(V)) V <- pick(tryCatch(summary(m)$cov.scaled, error = function(e) NULL))
  if (is.null(V)) V <- pick(.sunab_agg_vcov(m, nm[ok]))
  if (is.null(V)) return(NULL)
  o <- order(rt[ok])
  list(k = rt[ok][o], b = unname(b[ok])[o], V = unname(V)[o, o, drop = FALSE],
       binned = isTRUE(tryCatch(
         grepl("bin.rel", paste(deparse(stats::formula(m)), collapse = "")),
         error = function(e) FALSE)))
}

# The late-less-early contrast, as .wage_path_tests() computes it: women less
# men at each horizon, the two sexes' covariances added, the binned tails left
# out of both windows.
.rb_contrast <- function(vw, vm) {
  k  <- sort(intersect(vw$k, vm$k))
  iw <- match(k, vw$k); im <- match(k, vm$k)
  g  <- vw$b[iw] - vm$b[im]
  V  <- vw$V[iw, iw, drop = FALSE] + vm$V[im, im, drop = FALSE]
  tails <- if (isTRUE(vw$binned) || isTRUE(vm$binned)) c(-K, K) else numeric(0)
  at <- function(h) which(k %in% setdiff(h, tails))
  iE <- at(WT_EARLY); iT <- at(WT_LATE)
  if (!length(iE) || !length(iT)) return(NULL)
  ix <- c(iE, iT)
  a  <- c(rep(-1 / length(iE), length(iE)), rep(1 / length(iT), length(iT)))
  est <- sum(a * g[ix])
  s   <- sqrt(max(drop(t(a) %*% V[ix, ix, drop = FALSE] %*% a), 0))
  list(est = est, se = s, p = 2 * stats::pnorm(-abs(est / s)),
       horizons = sprintf("%d to %d less %d to %d",
                          min(k[iT]), max(k[iT]), min(k[iE]), max(k[iE])))
}

# ---- the frame and the roster -----------------------------------------------
fr <- td_frame(context = "robust_30_49")
if (is.null(attr(fr, "rows_before_screen"))) {
  cat("  the frame carries no screen stamp; applying td_apply_sample_rule()\n")
  fr <- td_apply_sample_rule(fr)
}
.need <- c("person_id", "wave", "female", "ever_parent", "age", "employed",
           "jttpewt_any", "jttpeot_any", "ln_hourly_wage_real")
.miss <- setdiff(.need, names(fr))
if (length(.miss))
  stop("the frame lacks: ", paste(.miss, collapse = ", "), call. = FALSE)
# 03a's order of preference for the leave indicator.
.lv <- intersect(c("on_maternity_leave", "on_parental_leave",
                   "nlf_reason_maternity", "nlreapm"), names(fr))[1]
# Column by column with [[, which works the same on a data.frame, a tibble
# and a data.table.
.cols <- c(.need, if (!is.na(.lv)) .lv)
panel_rb <- data.frame(lapply(stats::setNames(.cols, .cols), function(v) fr[[v]]),
                       stringsAsFactors = FALSE, check.names = FALSE)
# Labels stripped before the cohorts are read; the values do not change.
for (v in c("wave", "ever_parent")) panel_rb[[v]] <- clean_num(panel_rb[[v]])
cat(sprintf("Frame: %s person-waves, %s persons, build_tag %s\n",
            format(nrow(panel_rb), big.mark = ","),
            format(n_distinct(panel_rb$person_id), big.mark = ","),
            if (is.null(attr(fr, "build_tag"))) "ABSENT" else attr(fr, "build_tag")))
rm(fr, .cols)

# CS_event_study.R's cohorts: the first 0-to-1 step in ever_parent is the
# birth wave; never a parent is cohort 10000; anyone else, a parent when
# first seen, is left-censored and dropped.
g_tbl <- panel_rb %>% group_by(person_id) %>% arrange(wave) %>%
  summarise(ep_first = first(ever_parent), ep_last = last(ever_parent),
            trans = { d_ep <- diff(ever_parent)
            idx  <- which(!is.na(d_ep) & d_ep > 0)
            if (length(idx)) wave[idx[1] + 1] else NA_integer_ },
            .groups = "drop") %>%
  mutate(cohort = case_when(
    !is.na(trans)                ~ as.double(trans),
    ep_first == 0 & ep_last == 0 ~ 10000,
    TRUE                         ~ NA_real_))
ev <- panel_rb %>%
  inner_join(select(g_tbl, person_id, cohort), by = "person_id") %>%
  filter(!is.na(cohort))
for (v in c("wave", "female", "age", "employed", "jttpewt_any", "jttpeot_any",
            "ln_hourly_wage_real", if (!is.na(.lv)) .lv))
  ev[[v]] <- clean_num(ev[[v]])
ev <- ev %>%
  mutate(id      = as.integer(factor(person_id)),
         cohort  = as.double(cohort),
         treated = as.integer(cohort < 10000),
         k       = ifelse(treated == 1, wave - cohort, NA_real_),
         post    = as.integer(treated == 1 & !is.na(k) & k >= 0),
         post_f  = post * female)
ev <- as.data.frame(ev)
# 03a's employed-or-on-leave indicator, the same expression.
if (!is.na(.lv))
  ev$attached <- as.integer(ev$employed == 1 | (!is.na(ev[[.lv]]) & ev[[.lv]] == 1))
cat(sprintf("Roster: %s persons (%s treated, %s never-treated), %s person-waves\n",
            format(n_distinct(ev$person_id), big.mark = ","),
            format(n_distinct(ev$person_id[ev$treated == 1]), big.mark = ","),
            format(n_distinct(ev$person_id[ev$treated == 0]), big.mark = ","),
            format(nrow(ev), big.mark = ",")))
if (is.na(.lv))
  cat("  no leave indicator on the frame; the employed-or-on-leave arm is skipped.\n")
else
  cat("  leave indicator: ", .lv, "\n", sep = "")
rm(g_tbl, panel_rb)

# ---- the published values -----------------------------------------------------
.rb_sealed <- local({
  f <- file.path(.rb_designs, "BLIND_training_estimates.rds")
  if (file.exists(f)) readRDS(f) else NULL
})
.rb_ledger <- local({
  f <- file.path(.rb_designs, "training_ledger.csv")
  if (file.exists(f)) utils::read.csv(f, stringsAsFactors = FALSE) else NULL
})
.rb_wpt <- local({
  f <- file.path(.rb_results, "wage_path_tests.csv")
  if (file.exists(f)) utils::read.csv(f, stringsAsFactors = FALSE) else NULL
})
.rb_pub <- function(tag) {
  est <- se <- n <- NA_real_
  m <- if (!is.null(.rb_sealed)) .rb_sealed[[tag]] else NULL
  if (is.matrix(m)) {
    col <- function(rx) { j <- grep(rx, colnames(m))[1]
      if (is.na(j)) NA_real_ else unname(m[1, j]) }
    est <- col("^Estimate"); se <- col("Std\\. *Error")
  }
  if (!is.null(.rb_ledger) && all(c("tag", "n") %in% names(.rb_ledger))) {
    r <- .rb_ledger$n[.rb_ledger$tag == tag]
    if (length(r)) n <- suppressWarnings(as.numeric(r[1]))
  }
  c(est = est, se = se, n = n)
}
.rb_pub_wage <- function() {
  if (is.null(.rb_wpt)) return(c(est = NA_real_, se = NA_real_, n = NA_real_))
  r <- .rb_wpt[.rb_wpt$path == "plain" &
               .rb_wpt$quantity == "late window minus early window", , drop = FALSE]
  if (!nrow(r)) return(c(est = NA_real_, se = NA_real_, n = NA_real_))
  c(est = r$estimate[1], se = r$se[1], n = NA_real_)
}
# v-rev. THE HEADLINE, published by CS_event_study.R as the gap row for
# jttpewt_emp in cs_post_period_att.csv. That file carries no n.
.rb_cspa <- local({
  f <- file.path(.rb_results, "cs_post_period_att.csv")
  if (file.exists(f)) utils::read.csv(f, stringsAsFactors = FALSE) else NULL
})
# v-cw. WHICH WEIGHTS CS PUBLISHED, and the functions to form them here.
RB_CW <- !is.null(.rb_cspa) && any(.rb_cspa$sex == "gap_separate")
if (RB_CW && !(exists("sa_common_gap", mode = "function") &&
               exists("sa_cells", mode = "function"))) {
  .sa_src <- unique(c("sa_common_weights.R", file.path(PROJ_ROOT, "sa_common_weights.R")))
  .sa_src <- .sa_src[file.exists(.sa_src)]
  if (!length(.sa_src))
    stop("cs_post_period_att.csv has common-weight rows, but sa_common_weights.R is ",
         "not beside the scripts, so they cannot be reproduced. Put it in the ",
         "project root.", call. = FALSE)
  source(.sa_src[1], local = TRUE)
  rm(.sa_src)
}
cat("  Sun-Abraham arms: ", if (RB_CW)
      "common cohort weights, as CS_event_study.R published them" else
      "each sex on its own weights, as CS_event_study.R published them", "\n", sep = "")
.rb_pub_sa <- function() {
  if (is.null(.rb_cspa)) return(c(est = NA_real_, se = NA_real_, n = NA_real_))
  r <- .rb_cspa[.rb_cspa$sex == "gap" & .rb_cspa$outcome == "jttpewt_emp", , drop = FALSE]
  if (!nrow(r)) return(c(est = NA_real_, se = NA_real_, n = NA_real_))
  c(est = r$att[1], se = r$se[1], n = NA_real_)
}

# ---- the fits -------------------------------------------------------------------
.rb_row <- function(m, coefname) {
  ct <- fixest::coeftable(m)
  if (!coefname %in% rownames(ct)) return(NULL)
  used <- fixest::obs(m)
  list(est = unname(ct[coefname, 1]), se = unname(ct[coefname, 2]),
       p = unname(ct[coefname, 4]), n = stats::nobs(m), used = used)
}
.rb_t1 <- function(d, y) {
  m <- fixest::feols(stats::as.formula(paste0(y, " ~ post + post_f | person_id + wave^female")),
                     data = d, cluster = ~person_id, notes = FALSE, warn = FALSE)
  r <- .rb_row(m, "post_f")
  r$persons <- n_distinct(d$person_id[r$used])
  r
}
.rb_fit_window <- function(e, window) {
  out <- list()
  d_all <- e[!is.na(e$jttpewt_any), , drop = FALSE]
  out$emp <- .rb_t1(d_all[d_all$employed %in% 1, , drop = FALSE], "jttpewt_any")
  if ("attached" %in% names(e))
    out$att <- .rb_t1(e[e$attached %in% 1 & !is.na(e$jttpewt_any), , drop = FALSE],
                      "jttpewt_any")
  d_own <- e[!is.na(e$jttpeot_any) & e$employed %in% 1, , drop = FALSE]
  out$own <- .rb_t1(d_own, "jttpeot_any")
  # 03b's stack, the same construction.
  long <- bind_rows(
    e %>% transmute(person_id, wave, k, treated, female, employed, post,
                    source = "employer", y = jttpewt_any),
    e %>% transmute(person_id, wave, k, treated, female, employed, post,
                    source = "own", y = jttpeot_any)) %>%
    filter(employed == 1, !is.na(y)) %>%
    mutate(employer = as.integer(source == "employer"),
           post_f   = post * female,
           post_e   = post * employer,
           post_f_e = post * female * employer)
  m_ddd <- fixest::feols(y ~ post + post_f + post_e + post_f_e |
                           person_id^source + wave^source^female,
                         data = long, cluster = ~person_id,
                         notes = FALSE, warn = FALSE)
  out$ddd <- .rb_row(m_ddd, "post_f_e")
  out$ddd$persons <- n_distinct(long$person_id[out$ddd$used])
  # CS_event_study.R's wage path, one fit per sex, then the contrast.
  vc <- list(); cl <- list(); nn <- 0; ids <- integer(0)
  for (sx in c("women", "men")) {
    d  <- e[e$female == ifelse(sx == "women", 1, 0), , drop = FALSE]
    m  <- fit(d, "ln_hourly_wage_real")
    vc[[sx]] <- .event_vcov(m)
    if (RB_CW) cl[[sx]] <- sa_cells(m)     # here, while the fit's data exist
    nn  <- nn + stats::nobs(m)
    ids <- c(ids, d$id[fixest::obs(m)])
  }
  # v-cw. On common weights the gap path is one arm against a zero arm, as
  # CS_event_study.R hands it to .wage_path_tests().
  cw <- if (RB_CW) {
    cg <- sa_common_gap(cl$women, cl$men)
    .rb_contrast(sa_as_arm(cg, binned = isTRUE(vc$women$binned)), sa_zero_arm(cg$k))
  } else if (!is.null(vc$women) && !is.null(vc$men)) .rb_contrast(vc$women, vc$men) else NULL
  if (is.null(cw))
    cat("  [", window, "] the wage contrast could not be formed: a covariance",
        " was not kept or a window is empty.\n", sep = "")
  else
    out$wage <- list(est = cw$est, se = cw$se, p = cw$p, n = nn,
                     persons = length(unique(ids)), horizons = cw$horizons)
  # v-rev. THE HEADLINE: the Sun-Abraham post-birth average of employer-
  # financed training on employed person-waves, women less men, the two arms'
  # variances summed, as CS_event_study.R computes it. Unbinned, as Section 5.1
  # says; on fixest 0.14 that is also what CS's fit() falls back to.
  e$jttpewt_emp <- ifelse(e$employed %in% 1, e$jttpewt_any, NA_real_)
  sa <- list(); cl <- list(); nn <- 0; ids <- integer(0)
  for (sx in c("women", "men")) {
    d <- e[e$female == ifelse(sx == "women", 1, 0), , drop = FALSE]
    m <- feols(jttpewt_emp ~ sunab(cohort, wave) | id + wave, data = d,
               cluster = ~id, notes = FALSE, warn = FALSE)
    a <- summary(m, agg = "att")$coeftable
    sa[[sx]] <- unname(a[grep("ATT|att", rownames(a))[1], 1:2])
    # In this iteration, because the fit reads `d` again and the next
    # iteration replaces it.
    if (RB_CW) cl[[sx]] <- sa_cells(m)
    nn  <- nn + stats::nobs(m)
    ids <- c(ids, d$id[fixest::obs(m)])
  }
  if (RB_CW) {
    cg  <- sa_common_gap(cl$women, cl$men)
    est <- unname(cg$post[["est"]]); s <- unname(cg$post[["se"]])
  } else {
    est <- sa$women[1] - sa$men[1]; s <- sqrt(sa$women[2]^2 + sa$men[2]^2)
  }
  out$sa <- list(est = est, se = s, p = 2 * stats::pnorm(-abs(est / s)), n = nn,
                 persons = length(unique(ids)))
  # Who identifies the static arms: treated people with employed, answered
  # person-waves on both sides of the birth inside the window.
  both <- d_all[d_all$employed %in% 1 & d_all$treated == 1, , drop = FALSE] %>%
    group_by(person_id, female) %>%
    summarise(pre = any(k < 0), postb = any(k >= 0), .groups = "drop") %>%
    filter(pre, postb)
  out$mothers_both <- sum(both$female == 1)
  out$fathers_both <- sum(both$female == 0)
  out
}

.RB_ARMS <- list(
  sa   = list(tag = "cs_post_period_att: gap, jttpewt_emp",
              label = "Employer-financed training, Sun-Abraham average, employed only"),
  emp  = list(tag = "T1_jttpewt_any_employed",
              label = "Employer-financed training, employed only"),
  att  = list(tag = "T1_attached",
              label = "Employer-financed training, employed or on leave"),
  own  = list(tag = "T1_jttpeot_any_employed",
              label = "Own-time training, employed only"),
  ddd  = list(tag = "T3_ddd",
              label = "Triple difference, employer-financed less own time"),
  wage = list(tag = "wage_path_tests: late window minus early window",
              label = "Wage gap, years 5-10 less years 0-4 (log points)"))

.rb_table <- function(res, window) {
  rows <- list()
  for (a in names(.RB_ARMS)) {
    r <- res[[a]]
    if (is.null(r)) next
    pub <- if (window == "20-55") {
      if (a == "wage") .rb_pub_wage() else if (a == "sa") .rb_pub_sa() else
        .rb_pub(.RB_ARMS[[a]]$tag)
    } else c(est = NA_real_, se = NA_real_, n = NA_real_)
    rep_ok <- if (window == "20-55") {
      isTRUE(abs(r$est - pub[["est"]]) <= RB_TOL) &&
        isTRUE(abs(r$se - pub[["se"]]) <= RB_TOL) &&
        (a %in% c("wage", "sa") || isTRUE(r$n == pub[["n"]]))
    } else NA
    rows[[length(rows) + 1L]] <- data.frame(
      window = window, arm = a, source = .RB_ARMS[[a]]$tag,
      label = .RB_ARMS[[a]]$label,
      estimate = r$est, se = r$se, mde = RB_MDE * r$se, p = r$p,
      n = r$n, persons = r$persons,
      horizons = if (a == "wage") r$horizons else "",
      published_estimate = pub[["est"]], published_se = pub[["se"]],
      published_n = pub[["n"]], reproduces = rep_ok,
      weights = if (a %in% c("sa", "wage")) (if (RB_CW) "common" else "separate") else "",
      stringsAsFactors = FALSE)
  }
  for (cn in c("mothers_both", "fathers_both"))
    rows[[length(rows) + 1L]] <- data.frame(
      window = window, arm = cn, source = "count",
      label = if (cn == "mothers_both")
        "Mothers with employed waves before and after the birth" else
        "Fathers with employed waves before and after the birth",
      estimate = res[[cn]], se = NA_real_, mde = NA_real_, p = NA_real_,
      n = NA_real_, persons = NA_real_, horizons = "",
      published_estimate = NA_real_, published_se = NA_real_,
      published_n = NA_real_, reproduces = NA, weights = "", stringsAsFactors = FALSE)
  do.call(rbind, rows)
}
.rb_print <- function(tb) {
  p <- tb[tb$source != "count", c("arm", "estimate", "se", "n", "persons",
                                  "published_estimate", "published_se",
                                  "published_n", "reproduces")]
  num <- c("estimate", "se", "published_estimate", "published_se")
  p[num] <- lapply(p[num], function(x) round(x, 4))
  print(p, row.names = FALSE)
}
.rb_out <- file.path(.rb_diag, "robust_30_49.csv")
.rb_old <- file.path(.rb_results, "robust_30_49.csv")
if (file.exists(.rb_old) &&
    normalizePath(.rb_old, winslash = "/") != normalizePath(.rb_out, winslash = "/", mustWork = FALSE)) {
  file.remove(.rb_old)
  cat("  removed the copy an earlier run left in output/results: ", .rb_old, "\n", sep = "")
}

# ---- 20-55: reproduce the paper -------------------------------------------------
cat("\n--- full window, 20-55: each fit must reproduce the published value ---\n")
full <- .rb_fit_window(ev, "20-55")
tb_full <- .rb_table(full, "20-55")
.rb_print(tb_full)
.fails <- tb_full$arm[tb_full$source != "count" & !tb_full$reproduces %in% TRUE]
if (length(.fails)) {
  utils::write.csv(tb_full, .rb_out, row.names = FALSE)
  cat("\n  *** NOT REPRODUCED: ", paste(.fails, collapse = ", "), ".\n",
      "      The window fits are not run. robust_30_49.csv holds only this\n",
      "      comparison, so paper_tables.R will not use it. A difference in n\n",
      "      is a different sample; the same n with a different estimate is a\n",
      "      different specification or a rebuilt panel.\n", sep = "")
  stop("robust_30_49.R: the full-window fits do not reproduce the published ",
       "values for ", paste(.fails, collapse = ", "), "; see the table above.",
       call. = FALSE)
}
cat("  all full-window fits reproduce the published values.\n")

# ---- 30-49: the conventional window ---------------------------------------------
ew <- ev[!is.na(ev$age) & ev$age >= RB_AGE_LO & ev$age <= RB_AGE_HI, , drop = FALSE]
cat(sprintf(paste0("\n--- conventional window, %d-%d, by person-wave: %s of %s roster",
                   " person-waves, %s persons (%s treated) ---\n"),
            RB_AGE_LO, RB_AGE_HI,
            format(nrow(ew), big.mark = ","), format(nrow(ev), big.mark = ","),
            format(n_distinct(ew$person_id), big.mark = ","),
            format(n_distinct(ew$person_id[ew$treated == 1]), big.mark = ",")))
win <- .rb_fit_window(ew, "30-49")
tb_win <- .rb_table(win, "30-49")
.rb_print(tb_win)
cat(sprintf(paste0("  identifying the static arms: %s mothers and %s fathers on 20-55,",
                   " %s and %s on 30-49\n"),
            format(full$mothers_both, big.mark = ","), format(full$fathers_both, big.mark = ","),
            format(win$mothers_both, big.mark = ","), format(win$fathers_both, big.mark = ",")))

out <- rbind(tb_full, tb_win)
out$status <- "post-hoc: added after unsealing"
utils::write.csv(out, .rb_out, row.names = FALSE)
cat("  wrote ", .rb_out, "\n", sep = "")

cat("\n", strrep("=", 78), "\n", sep = "")
cat("robust_30_49.R  completed ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", sep = "")
cat(strrep("=", 78), "\n", sep = "")

}), finally = {
  if (.rb_owns) while (sink.number() > 0L) sink()
  rm(list = intersect(c(".rb_ok", ".rb_designs", ".rb_results", ".rb_diag", ".rb_logdir",
                        ".rb_owns", ".rb_log"), ls(globalenv(), all.names = TRUE)),
     envir = globalenv())
})
