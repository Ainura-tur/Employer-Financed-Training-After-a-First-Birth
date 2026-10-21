# =============================================================================
# sa_common_weights.R   Women-minus-men Sun-Abraham contrasts on COMMON weights
#
# WHY. Every women-minus-men event-study number in the paper is built the same
# way: fit equation (3) once per sex against same-sex never-treated controls,
# let fixest aggregate each fit with its own cell counts, then difference the
# two aggregates. The counts are employed person-waves, and mothers' cells thin
# after the birth by different amounts across cohorts and horizons, so the two
# arms are averaged over different cohort compositions. The referee's point is
# that the difference then mixes the gender gap with that composition.
#
# WHAT. This file forms the difference inside each cohort-by-event-time cell
# and averages it with one set of weights:
#
#   Delta_k = sum_g w^C_gk (delta^f_gk - delta^m_gk)
#
# over the cells observed for both sexes. The preferred scheme pools the two
# arms' cell counts; "average" averages the two arms' own weights; "minimum"
# takes the smaller count. The post-birth average weights each cell with
# k >= post_from by its share of the pooled post-birth person-waves.
#
# HOW IT CONNECTS. Nothing here fits a model. It reads two fitted
# fixest::feols(... ~ sunab(cohort, wave) | ...) objects, one per sex, which
# CS_event_study.R, training_event_study.R, 05_review_response.R and
# 06_revision_inference.R already produce. sa_patches.R says where each call
# goes.
#
#   sa_cells(m)                 the cohort-by-event cells of one fit: est, n, V
#   sa_common_gap(cw, cm, ...)  the common-weight gap path and post average,
#                               with the separate-weight estimate beside it
#   sa_gap_fits(mw, mm, ...)    the two calls above in one
#   sa_schemes(cw, cm, ...)     the post average under every scheme, as rows
#   sa_as_arm(gp), sa_zero_arm  a gap path in the shape .wage_path_tests() and
#                               .rv_gap_path() take, so they need no change
#
# CHECKS. sa_cells() rebuilds fixest's own aggregation from the cells and
# stops if it does not reproduce coef(m), so a fixest version that aggregates
# differently fails here rather than producing a wrong gap. sa_common_gap()
# reports the separate-weight estimate, which must equal what the pipeline
# published before this change; 06_revision_inference.R checks that.
# =============================================================================

suppressPackageStartupMessages(library(dplyr))

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

SA_SCHEMES <- c("pooled", "average", "minimum")

# -----------------------------------------------------------------------------
# sa_cells(m): the cohort-by-event-time coefficients of one sunab fit.
#
# m$coefficients holds the cohort-level coefficients; coef(m) holds fixest's
# aggregated path. The cell count is the number of estimation rows carrying
# each sunab dummy, which is how fixest's aggregate() weights them (05's
# rr_es() and CS's .sunab_agg_vcov() use the same rule). vcov(m) is the
# cohort-level covariance. A fit with vcov = "iid", as the bootstrap uses, is
# fine: pass need_V = FALSE and V is not read.
# -----------------------------------------------------------------------------
sa_cells <- function(m, need_V = TRUE, check = TRUE, tol = 1e-8) {
  if (is.null(m)) return(NULL)
  cf  <- m$coefficients
  cn  <- names(cf)
  pat <- m$model_matrix_info$sunab$agg_period
  if (is.null(pat)) pat <- "(wave)::(-?[[:digit:]]+):cohort"
  sel <- which(grepl(pat, cn, perl = TRUE) & is.finite(cf))
  if (!length(sel)) stop("sa_cells(): no sunab cohort-by-period coefficients in this fit.")
  k <- as.numeric(gsub(paste0(".*", pat, ".*"), "\\2", cn[sel], perl = TRUE))
  g <- suppressWarnings(as.numeric(sub("^.*:cohort::", "", cn[sel])))
  if (anyNA(g)) stop("sa_cells(): could not read the cohort from ",
                     paste(utils::head(cn[sel][is.na(g)], 3), collapse = ", "))
  mm <- stats::model.matrix(m)
  if (!all(cn[sel] %in% colnames(mm)))
    stop("sa_cells(): model.matrix() lacks some sunab columns. Is the data the fit ",
         "used still in scope?")
  x  <- sign(mm[, cn[sel], drop = FALSE])
  n  <- if (!is.null(m$weights)) colSums(m$weights * x) else colSums(x)
  cells <- data.frame(term = cn[sel], g = g, k = k, est = unname(cf[sel]),
                      n = unname(n), stringsAsFactors = FALSE)
  V <- NULL
  if (need_V) {
    V <- stats::vcov(m)
    if (!all(cn[sel] %in% rownames(V))) stop("sa_cells(): vcov() lacks some sunab terms.")
    V <- V[cn[sel], cn[sel], drop = FALSE]
  }
  if (check) {
    # fixest's own path, rebuilt from the cells with each arm's own weights.
    ag <- stats::coef(m)
    if (!any(grepl(":cohort::", names(ag), fixed = TRUE))) {
      ak  <- suppressWarnings(as.numeric(sub(".*::(-?[0-9]+)$", "\\1", names(ag))))
      own <- cells %>% group_by(k) %>% summarise(b = sum(est * n) / sum(n), .groups = "drop")
      i   <- match(own$k, ak)
      d   <- suppressWarnings(max(abs(ag[i] - own$b), na.rm = TRUE))
      if (is.finite(d) && d > tol)
        stop("sa_cells(): the cells do not reproduce fixest's aggregated path (max ",
             "difference ", signif(d, 3), "). This fixest version aggregates differently.")
    }
  }
  list(cells = cells, V = V)
}

# -----------------------------------------------------------------------------
# sa_common_gap(cw, cm): women minus men on one set of cohort weights.
#
# cw, cm   sa_cells() of the women's and the men's fit
# scheme   "pooled" (preferred), "average" or "minimum"
# post_from  first horizon of the post-birth average (0)
# k_range  optional c(lo, hi); cells outside it are left out of the path and
#          of the post average, as binned endpoints should be
#
# Returns
#   k, g, V        the gap path and its covariance (rt is k, for 05's helpers)
#   path           data.frame(event_time, gap, se), the shape gap() returns
#   post           c(est, se): the common-weight post-birth average
#   post_w, post_m each arm on the common weights (their difference is post)
#   separate       list(post = c(est, se), path): each arm on its own weights
#                  over its own cells, then differenced; the old estimator
#   composition    separate post less common post
#   support        share of each arm's weight outside common support
#   weights        the cells, with both arms' counts and the common weights
# Standard errors add the two arms' variances, the paper's convention for
# disjoint persons; the couple block bootstrap supplies the covariance.
# -----------------------------------------------------------------------------
sa_common_gap <- function(cw, cm, scheme = "pooled", post_from = 0, k_range = NULL) {
  scheme <- match.arg(scheme, SA_SCHEMES)
  if (is.null(cw) || is.null(cm)) return(NULL)
  fw <- cw$cells; fm <- cm$cells
  if (!is.null(k_range)) {
    fw <- fw[fw$k >= k_range[1] & fw$k <= k_range[2], , drop = FALSE]
    fm <- fm[fm$k >= k_range[1] & fm$k <= k_range[2], , drop = FALSE]
  }
  both <- inner_join(fw, fm, by = c("g", "k"), suffix = c("_w", "_m"))
  both <- both[order(both$k, both$g), , drop = FALSE]
  if (!nrow(both)) stop("sa_common_gap(): the two arms share no cohort-by-event cell.")

  # Mass of each common cell, and the per-horizon and post-average weights.
  both <- both %>% group_by(k) %>%
    mutate(ow_w = n_w / sum(n_w), ow_m = n_m / sum(n_m)) %>% ungroup()
  mass <- switch(scheme, pooled = both$n_w + both$n_m,
                 minimum = pmin(both$n_w, both$n_m), average = NA_real_)
  if (scheme == "average") {
    both$w_k <- (both$ow_w + both$ow_m) / 2
  } else {
    both$w_k <- stats::ave(mass, both$k, FUN = function(z) z / sum(z))
  }
  post <- both$k >= post_from
  w_post <- if (scheme == "average") {
    (both$n_w * post / sum(both$n_w[post]) + both$n_m * post / sum(both$n_m[post])) / 2
  } else mass * post / sum(mass[post])
  both$w_post <- w_post

  # Linear combinations over each arm's own coefficient vector.
  ks <- sort(unique(both$k))
  Aw <- matrix(0, length(ks), nrow(fw), dimnames = list(ks, fw$term))
  Am <- matrix(0, length(ks), nrow(fm), dimnames = list(ks, fm$term))
  for (j in seq_along(ks)) {
    r <- both$k == ks[j]
    Aw[j, both$term_w[r]] <- both$w_k[r]
    Am[j, both$term_m[r]] <- both$w_k[r]
  }
  aw <- stats::setNames(numeric(nrow(fw)), fw$term); aw[both$term_w] <- w_post
  am <- stats::setNames(numeric(nrow(fm)), fm$term); am[both$term_m] <- w_post
  bw <- stats::setNames(fw$est, fw$term); bm <- stats::setNames(fm$est, fm$term)

  g      <- drop(Aw %*% bw - Am %*% bm)
  post_w <- sum(aw * bw); post_m <- sum(am * bm)
  haveV  <- !is.null(cw$V) && !is.null(cm$V)
  Vw <- if (haveV) cw$V[fw$term, fw$term, drop = FALSE] else NULL
  Vm <- if (haveV) cm$V[fm$term, fm$term, drop = FALSE] else NULL
  V  <- if (haveV) Aw %*% Vw %*% t(Aw) + Am %*% Vm %*% t(Am) else NULL
  post_se <- if (haveV) sqrt(drop(t(aw) %*% Vw %*% aw) + drop(t(am) %*% Vm %*% am)) else NA_real_

  # The old estimator: each arm on its own weights over all its own cells.
  own <- function(f, Vf) {
    p  <- f$k >= post_from
    a  <- stats::setNames(ifelse(p, f$n, 0) / sum(f$n[p]), f$term)
    se <- if (!is.null(Vf)) sqrt(drop(t(a) %*% Vf %*% a)) else NA_real_
    pk <- f %>% group_by(k) %>% summarise(b = sum(est * n) / sum(n), .groups = "drop")
    list(post = sum(a * f$est), se = se, path = pk)
  }
  sw <- own(fw, Vw); sm <- own(fm, Vm)
  sep_path <- inner_join(sw$path, sm$path, by = "k", suffix = c("_w", "_m"))
  separate <- list(post = c(est = sw$post - sm$post, se = sqrt(sw$se^2 + sm$se^2)),
                   path = data.frame(event_time = sep_path$k,
                                     gap = sep_path$b_w - sep_path$b_m))

  drop_share <- function(f, other) {
    key  <- paste(f$g, f$k); keep <- key %in% paste(other$g, other$k)
    p    <- f$k >= post_from
    c(all = sum(f$n[!keep]) / sum(f$n), post = sum(f$n[!keep & p]) / sum(f$n[p]))
  }
  support <- rbind(women = drop_share(fw, fm), men = drop_share(fm, fw))

  se_k <- if (haveV) sqrt(pmax(diag(V), 0)) else rep(NA_real_, length(ks))
  list(scheme = scheme, k = ks, rt = ks, g = unname(g), V = if (haveV) unname(V) else NULL,
       path = data.frame(event_time = ks, gap = unname(g), se = unname(se_k)),
       post = c(est = post_w - post_m, se = post_se),
       post_w = post_w, post_m = post_m,
       separate = separate,
       composition = unname(separate$post["est"] - (post_w - post_m)),
       support = support,
       weights = both[, c("g", "k", "n_w", "n_m", "w_k", "w_post")])
}

sa_gap_fits <- function(mw, mm, ..., need_V = TRUE)
  sa_common_gap(sa_cells(mw, need_V = need_V), sa_cells(mm, need_V = need_V), ...)

# -----------------------------------------------------------------------------
# sa_schemes(cw, cm): the post average under every scheme, one row each, plus
# the separate-weight estimate and the composition term. Rows are named the
# way cs_post_period_att.csv and training_post_att.csv name their sex column,
# so they register as csatt.gap_average.<outcome>.att and so on.
# -----------------------------------------------------------------------------
sa_schemes <- function(cw, cm, post_from = 0, k_range = NULL) {
  r <- lapply(SA_SCHEMES, function(s) sa_common_gap(cw, cm, s, post_from, k_range))
  names(r) <- SA_SCHEMES
  p <- r$pooled
  data.frame(
    sex = c("gap", "gap_average", "gap_minimum", "gap_separate", "gap_composition",
            "support_dropped_women", "support_dropped_men"),
    att = c(r$pooled$post[["est"]], r$average$post[["est"]], r$minimum$post[["est"]],
            p$separate$post[["est"]], p$composition,
            p$support["women", "post"], p$support["men", "post"]),
    se  = c(r$pooled$post[["se"]], r$average$post[["se"]], r$minimum$post[["se"]],
            p$separate$post[["se"]], NA_real_, NA_real_, NA_real_),
    stringsAsFactors = FALSE)
}

# A gap path in the shape of .event_vcov()'s output, and a zero arm to pair it
# with, so .wage_path_tests(vw, vm) and .rv_gap_path(vw, vm) take the common-
# weight gap without being rewritten: vw - 0 is the gap, V + 0 its covariance.
sa_as_arm   <- function(gp, binned = FALSE) list(k = gp$k, b = gp$g, V = gp$V, binned = binned)
sa_zero_arm <- function(k) list(k = k, b = rep(0, length(k)),
                                V = matrix(0, length(k), length(k)), binned = FALSE)

# =============================================================================
# SELF-TEST, run only when this file is executed directly:
#   Rscript sa_common_weights.R
# Simulated panel; the true effect is -0.10 for mothers and 0 for fathers in
# every cohort, and mothers' post-birth person-waves thin more in early
# cohorts, so the two arms' weights differ as on the employed frame.
# =============================================================================
if (sys.nframe() == 0L) {
  suppressPackageStartupMessages(library(fixest))
  set.seed(20261014)
  sim_arm <- function(n_treat, n_never, effect, thin) {
    ids <- seq_len(n_treat + n_never)
    coh <- c(sample(14:23, n_treat, replace = TRUE), rep(10000, n_never))
    fe  <- rnorm(length(ids), sd = 0.1)
    d <- expand.grid(id = ids, wave = 12:24)
    d$cohort <- coh[d$id]
    d$k <- ifelse(d$cohort < 10000, d$wave - d$cohort, NA)
    # heterogeneous effects by cohort, so composition matters
    het <- ifelse(d$cohort < 10000, (d$cohort - 18) * 0.01, 0)
    d$y <- 0.25 + fe[d$id] + 0.005 * (d$wave - 12) +
      ifelse(!is.na(d$k) & d$k >= 0, effect + effect * het * 3, 0) + rnorm(nrow(d), sd = 0.05)
    keep <- is.na(d$k) | d$k < 0 | runif(nrow(d)) > thin * (24 - d$cohort) / 10
    d[keep, ]
  }
  dw <- sim_arm(500, 900, -0.10, 0.7)
  dm <- sim_arm(500, 900,  0.00, 0.0)
  mw <- feols(y ~ sunab(cohort, wave) | id + wave, data = dw, cluster = ~id)
  mm <- feols(y ~ sunab(cohort, wave) | id + wave, data = dm, cluster = ~id)

  cw <- sa_cells(mw); cm <- sa_cells(mm)          # stops if fixest disagrees
  gp <- sa_common_gap(cw, cm)
  att <- function(m) unname(summary(m, agg = "att")$coeftable[1, 1:2])
  aw <- att(mw); am <- att(mm)
  cat(sprintf("separate weights, rebuilt here : %+.5f (SE %.5f)\n",
              gp$separate$post["est"], gp$separate$post["se"]))
  cat(sprintf("separate weights, fixest agg   : %+.5f (SE %.5f)\n",
              aw[1] - am[1], sqrt(aw[2]^2 + am[2]^2)))
  stopifnot(abs(gp$separate$post["est"] - (aw[1] - am[1])) < 1e-8,
            abs(gp$separate$post["se"] - sqrt(aw[2]^2 + am[2]^2)) < 1e-8)
  print(sa_schemes(cw, cm), digits = 4, row.names = FALSE)
  cat("\npath, first post-birth horizons:\n")
  print(head(gp$path[gp$path$event_time >= 0, ], 4), digits = 4, row.names = FALSE)

  # Point estimates only, as the bootstrap fits them.
  mwi <- feols(y ~ sunab(cohort, wave) | id + wave, data = dw, vcov = "iid")
  mmi <- feols(y ~ sunab(cohort, wave) | id + wave, data = dm, vcov = "iid")
  gi  <- sa_gap_fits(mwi, mmi, need_V = FALSE)
  stopifnot(abs(gi$post[["est"]] - gp$post[["est"]]) < 1e-10)

  # Binned endpoints, as 05's rr_sa() fits them, with the window applied.
  mwb <- feols(y ~ sunab(cohort, wave, bin.rel = list(`-9` = c(-11, -10, -9))) | id + wave,
               data = dw, cluster = ~id)
  mmb <- feols(y ~ sunab(cohort, wave, bin.rel = list(`-9` = c(-11, -10, -9))) | id + wave,
               data = dm, cluster = ~id)
  gb <- sa_gap_fits(mwb, mmb, k_range = c(-8, 9))
  cat(sprintf("\nbinned fit, window -8..9: post %+.5f (SE %.5f)\n", gb$post[1], gb$post[2]))
  cat("\nAll checks passed.\n")
}
