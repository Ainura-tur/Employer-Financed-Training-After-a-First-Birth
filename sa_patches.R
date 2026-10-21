# =============================================================================
# sa_patches.R   Where sa_common_weights.R plugs into the pipeline
#
# Seven scripts change. Each patch below gives a FIND line, copied exactly from
# the script, and the code that goes in its place or after it. The code sits
# inside if (FALSE) { } so this file parses as R (Rscript -e
# 'invisible(parse("sa_patches.R"))') but never runs. Copy what is between the
# braces, not the braces.
#
# WHAT CHANGES IN THE NUMBERS
#   Every women-minus-men Sun-Abraham quantity is formed inside each cohort-by-
#   event-time cell and averaged on pooled common weights. That covers the
#   headline (\SATrainingPost*), the Figure 2 training and wage paths, the
#   which-side gap (side.*.gap) and the place ratios built from it, the own-time
#   average (trpost.*.gap.*), the event-study financing contrast (\SADdd*), the
#   wage-window tests (wptest.*), the couple bootstrap (\SASECouple,
#   \SAArmCorr, \WageWindowSECouple) and 05's paths (R1, R5, R6b, R9).
#   The per-sex series (mothers against never-mothers, fathers against
#   childless men) are within-sex comparisons and keep their own weights.
#   The old estimator survives as rows labelled gap_separate, and 06 checks
#   that it still reproduces the published numbers.
#
# BEFORE THE FIRST RUN, move output/results/wage_path_ledger.csv aside.
# MASTER's PART 6A stops if the wage path in the RDS differs from that csv by
# more than 0.0005, and the common-weight path will differ from the old one.
# The same holds for any literal a check script compares against (05's R6b
# targets below, and any in verify_manuscript_numbers.R).
#
# RUN ORDER is unchanged: training_event_study.R and CS_event_study.R (via
# MASTER), then 05, then 06, then paper_tables.R. Put sa_common_weights.R
# beside the scripts.
#
# NOT COVERED: RUN_FIGURES_PAPER.R was not uploaded. If it differences the
# per-sex series itself, rather than reading timing_paths.csv or
# training_decomposition.csv, it needs the same change as MASTER's 8.2b below.
# =============================================================================


# =============================================================================
# 0. THE SOURCE BLOCK. Every script below starts its patch with this.
# =============================================================================
if (FALSE) {
  if (!exists("sa_common_gap", mode = "function")) {
    .sa_f <- c("sa_common_weights.R",
               file.path(if (exists("PROJ_ROOT")) PROJ_ROOT else ".", "sa_common_weights.R"))
    .sa_f <- .sa_f[file.exists(.sa_f)]
    if (!length(.sa_f)) stop("sa_common_weights.R is not beside this script.", call. = FALSE)
    source(.sa_f[1]); rm(.sa_f)
  }
  sa_try <- function(m, label) tryCatch(sa_cells(m), error = function(e) {
    cat("  *** sa_cells() failed for ", label, ": ", conditionMessage(e), "\n", sep = "")
    NULL })
}


# =============================================================================
# 1. CS_event_study.R  (headline, wage path, wage tests, Lee bounds)
# =============================================================================

# 1a. FIND, the three-line definition:
#       att_overall <- function(m) tryCatch({
#         a <- summary(m, agg = "att")$coeftable
#         a[grep("ATT|att", rownames(a))[1], 1:2] }, error = function(e) c(NA, NA))
#     INSERT AFTER IT: the source block from section 0.

# 1b. FIND:   .atts <- list()
#     INSERT AFTER IT:
if (FALSE) {
.cells <- list()         # cohort-by-event cells of every fit, for common weights
}

# 1c. FIND:       m <- fit(d, y); tag <- paste(sx, y); a <- att_overall(m)
#     INSERT AFTER IT:
if (FALSE) {
    .cells[[tag]] <- sa_try(m, tag)
}

# 1d. FIND:       res[[paste(sx, "ln_hourly_wage_real_agefe")]] <- es_tab(m)
#     INSERT AFTER IT:
if (FALSE) {
    .cells[[paste(sx, "ln_hourly_wage_real_agefe")]] <- sa_try(m, paste(sx, "agefe"))
}

# 1e. FIND:   .att_tab <- rbind(.att_tab, .g[, c("sex", "outcome", "att", "se")])
#     REPLACE WITH:
if (FALSE) {
  # v-cw. COMMON COHORT WEIGHTS. The gap row is now the women-minus-men
  # difference formed inside each cohort-by-event cell and averaged on pooled
  # cell counts over common support. The estimator it replaces is kept as
  # gap_separate, with gap_average, gap_minimum, the composition term and the
  # weight outside common support beside it; MASTER registers them all as
  # csatt.<row>.<outcome>.att.
  .sch <- do.call(rbind, lapply(.g$outcome, function(o) {
    cw <- .cells[[paste("women", o)]]; cm <- .cells[[paste("men", o)]]
    if (is.null(cw) || is.null(cm)) return(NULL)
    s <- sa_schemes(cw, cm); s$outcome <- o; s
  }))
  .nocw <- setdiff(.g$outcome, if (is.null(.sch)) character(0) else .sch$outcome)
  if (length(.nocw))
    cat("  *** no cells for ", paste(.nocw, collapse = ", "),
        ", so their gap rows keep separate weights.\n", sep = "")
  if (!is.null(.sch)) {
    .chk <- merge(.sch[.sch$sex == "gap_separate", c("outcome", "att")],
                  .g[, c("outcome", "att")], by = "outcome")
    .bad <- .chk$outcome[abs(.chk$att.x - .chk$att.y) > 1e-5]
    if (length(.bad))
      stop("the separate-weight gap rebuilt from the cells differs from fixest's for ",
           paste(.bad, collapse = ", "), call. = FALSE)
    rm(.chk, .bad)
  }
  .att_tab <- rbind(.att_tab, .g[.g$outcome %in% .nocw, c("sex", "outcome", "att", "se")],
                    if (!is.null(.sch)) .sch[, c("sex", "outcome", "att", "se")])
  rm(.sch, .nocw)
}

# 1f. FIND:   attr(res, "gap_definition") <- paste(
#     INSERT BEFORE IT. Every "gap <outcome>" element becomes the common-weight
#     path, so MASTER's 8.2, the wage ledger and 05's R6 read it unchanged.
#     (The plain-against-age-absorbed print a few lines up still shows the
#     separate-weight paths; it is a log line only.)
if (FALSE) {
for (.o in sub("^women ", "", grep("^women ", names(.cells), value = TRUE))) {
  .cg <- tryCatch(sa_common_gap(.cells[[paste("women", .o)]], .cells[[paste("men", .o)]]),
                  error = function(e) NULL)
  if (!is.null(.cg) && !is.null(.cg$V))
    res[[paste("gap", .o)]] <- data.frame(event_time = .cg$k, gap = round(.cg$g, 6),
                                          se = round(.cg$path$se, 6))
}
rm(list = intersect(c(".o", ".cg"), ls(all.names = TRUE)))
saveRDS(.cells, .out_path("cs_event_study_cells.rds"))
}
#     And AFTER the attr(res, "gap_definition") statement, add:
if (FALSE) {
attr(res, "gap_definition") <- paste(
  "gap rows are women minus men formed within each cohort-by-event-time cell and",
  "averaged with common weights, pooled cell counts on common support",
  "(sa_common_weights.R); se from the two arms' covariances summed.",
  "k = -1 is omitted and absent from every series.")
}

# 1g. FIND (inside the .wt_rows loop):
#         .wt_rows[[.arm$label]] <- .wage_path_tests(.arm$vc[["women"]], .arm$vc[["men"]],
#                                                    .arm$label)
#     REPLACE WITH:
if (FALSE) {
    .oc <- if (.arm$label == "plain") "ln_hourly_wage_real" else "ln_hourly_wage_real_agefe"
    .cg <- tryCatch(sa_common_gap(.cells[[paste("women", .oc)]], .cells[[paste("men", .oc)]]),
                    error = function(e) NULL)
    .wt_rows[[.arm$label]] <- if (!is.null(.cg) && !is.null(.cg$V)) {
      # the gap as one arm against a zero arm, so .wage_path_tests() is unchanged
      .wage_path_tests(sa_as_arm(.cg, binned = isTRUE(.arm$vc[["women"]]$binned)),
                       sa_zero_arm(.cg$k), .arm$label)
    } else {
      cat("  *** no common-weight wage gap for the ", .arm$label,
          " path; its tests use separate weights.\n", sep = "")
      .wage_path_tests(.arm$vc[["women"]], .arm$vc[["men"]], .arm$label)
    }
    rm(.oc, .cg)
}

# 1h. Lee bounds. FIND:   emp_gap <- gap(res[["women employed"]], res[["men employed"]])
#     REPLACE WITH:
if (FALSE) {
  emp_gap <- if (is.data.frame(res[["gap employed"]])) res[["gap employed"]] else
    gap(res[["women employed"]], res[["men employed"]])
}
#     FIND:   wage_gap <- gap(res[["women ln_hourly_wage_real"]],
#                             res[["men ln_hourly_wage_real"]])
#     REPLACE WITH:
if (FALSE) {
  wage_gap <- if (is.data.frame(res[["gap ln_hourly_wage_real"]]))
    res[["gap ln_hourly_wage_real"]] else
    gap(res[["women ln_hourly_wage_real"]], res[["men ln_hourly_wage_real"]])
}
#     FIND:   obs_pool <- att_overall(fit(filter(cs, female==1),"ln_hourly_wage_real"))[1] -
#               att_overall(fit(filter(cs, female==0),"ln_hourly_wage_real"))[1]
#     REPLACE WITH (also saves two refits):
if (FALSE) {
  obs_pool <- tryCatch(sa_common_gap(.cells[["women ln_hourly_wage_real"]],
                                     .cells[["men ln_hourly_wage_real"]])$post[["est"]],
                       error = function(e) NA_real_)
}


# =============================================================================
# 2. training_event_study.R  (Figure 2 training path, side.*, trpost.*)
# =============================================================================

# 2a. FIND:   res <- list(); .post <- list(); .win <- list()
#     INSERT BEFORE IT: the source block from section 0.
#     INSERT AFTER IT:
if (FALSE) {
.cells <- list()
}

# 2b. FIND:         tag <- paste(cond, sx, y); a <- att_overall(m)
#     INSERT AFTER IT:
if (FALSE) {
      .cells[[tag]] <- sa_try(m, tag)
}

# 2c. FIND:   saveRDS(res, .out_path("training_event_study_results.rds"))
#     INSERT BEFORE IT. The common-weight gap goes in as "<frame> gap <outcome>",
#     which MASTER's 8.2, 8.2b and mpn_side() read once patched (sections 5, 6).
if (FALSE) {
for (cond in c("unconditional", "employed_only")) for (y in tr_outcomes) {
  .cg <- tryCatch(sa_common_gap(.cells[[paste(cond, "women", y)]],
                                .cells[[paste(cond, "men", y)]]), error = function(e) NULL)
  if (!is.null(.cg) && !is.null(.cg$V))
    res[[paste(cond, "gap", y)]] <- data.frame(event_time = .cg$k, gap = round(.cg$g, 6),
                                               se = round(.cg$path$se, 6))
}
rm(list = intersect(".cg", ls(all.names = TRUE)))
saveRDS(.cells, .out_path("training_event_study_cells.rds"))
}

# 2d. FIND:   .pa <- rbind(.pa, .g); rownames(.pa) <- NULL
#     INSERT BEFORE IT:
if (FALSE) {
  # v-cw. The gap rows on common weights; the old ones become gap_separate.
  .sch <- if (is.null(.g)) NULL else do.call(rbind, lapply(seq_len(nrow(.g)), function(i) {
    cw <- .cells[[paste(.g$frame[i], "women", .g$outcome[i])]]
    cm <- .cells[[paste(.g$frame[i], "men",   .g$outcome[i])]]
    if (is.null(cw) || is.null(cm)) return(NULL)
    s <- sa_schemes(cw, cm)
    if (abs(s$att[s$sex == "gap_separate"] - .g$att[i]) > 1e-5)
      stop("separate-weight gap does not reproduce fixest's for ", .g$frame[i], " ",
           .g$outcome[i], call. = FALSE)
    data.frame(frame = .g$frame[i], sex = s$sex, outcome = .g$outcome[i],
               att = round(s$att, 6), se = round(s$se, 6),
               rule = ifelse(s$sex == "gap",
                             "women - men within cohort-by-event cells, pooled common weights; variances added",
                             ifelse(s$sex == "gap_separate",
                                    "each arm on its own weights, then differenced (before the revision)",
                                    "see sa_common_weights.R")),
               stringsAsFactors = FALSE)
  }))
  if (!is.null(.sch)) {
    .g <- rbind(.g[!paste(.g$frame, .g$outcome) %in% paste(.sch$frame, .sch$outcome), ], .sch)
    rm(.sch)
  }
}


# =============================================================================
# 3. 06_revision_inference.R  (reproduction, \SADdd*, the couple bootstrap)
# =============================================================================

# 3a. FIND:   stopifnot(exists("PROJ_ROOT"), exists("td_frame", mode = "function"))
#     INSERT AFTER IT: the source block from section 0.
#     THEN FIND the end of .rv_le(), the lines
#       structure(mean(g[iL]) - mean(g[iE]), horizons = paste(sort(k[iE | iL]), collapse = ","))
#     }
#     and INSERT AFTER THEM (it must sit inside the local() block, where
#     REV_EARLY and REV_LATE are defined):
if (FALSE) {
# Late less early on a gap path, for the bootstrap: equal weights over the
# horizons present, as .rv_window() uses.
.rv_le_gap <- function(gp) {
  if (is.null(gp)) return(NA_real_)
  iE <- gp$k %in% REV_EARLY; iL <- gp$k %in% REV_LATE
  if (!any(iE) || !any(iL)) return(NA_real_)
  structure(mean(gp$g[iL]) - mean(gp$g[iE]),
            horizons = paste(sort(gp$k[iE | iL]), collapse = ","))
}
}

# 3b. REPRO_SA. FIND:
#       R$sa <- list(att_w = aw[1], se_w = aw[2], att_m = am[1], se_m = am[2],
#                    gap = aw[1] - am[1], se = sqrt(aw[2]^2 + am[2]^2),
#                    n = stats::nobs(fe_$women) + stats::nobs(fe_$men))
#     REPLACE WITH:
if (FALSE) {
  cg_ <- sa_gap_fits(fe_$women, fe_$men)
  R$sa <- list(att_w = aw[1], se_w = aw[2], att_m = am[1], se_m = am[2],
               gap = cg_$post[["est"]], se = cg_$post[["se"]],
               gap_sep = aw[1] - am[1], se_sep = sqrt(aw[2]^2 + am[2]^2),
               composition = cg_$composition,
               n = stats::nobs(fe_$women) + stats::nobs(fe_$men))
  rm(cg_)
}
#     FIND:   .rv_show("its SE, variances summed", R$sa$se, pa("gap", "se")))
#     REPLACE WITH these two lines (the second checks that the old estimator
#     still reproduces what was published, now in the gap_separate row):
#             .rv_show("its SE, variances summed", R$sa$se, pa("gap", "se")),
#             .rv_show("separate weights, women less men", R$sa$gap_sep, pa("gap_separate", "att")))

# 3c. REPRO_WAGE. FIND these five lines:
#       vw <- .event_vcov(fw_$women); vm <- .event_vcov(fw_$men)
#       if (is.null(vw) || is.null(vm)) stop("the wage path's covariance could not be rebuilt")
#       gp <- .rv_gap_path(vw, vm)
#       win <- .rv_window(gp); lds <- .rv_leads(gp)
#       le <- .rv_le(data.frame(k = vw$k, b = vw$b), data.frame(k = vm$k, b = vm$b))
#     REPLACE WITH:
if (FALSE) {
  cg_ <- sa_gap_fits(fw_$women, fw_$men)
  gp  <- list(k = cg_$k, g = cg_$g, V = cg_$V)
  win <- .rv_window(gp); lds <- .rv_leads(gp)
  le  <- .rv_le_gap(gp)
  rm(cg_)
}

# 3d. SADDD. FIND these five lines:
#       a <- lapply(m, .rv_att)
#       g <- function(y) a[[paste("women", y)]][1] - a[[paste("men", y)]][1]
#       nn <- function(y) stats::nobs(m[[paste("women", y)]]) + stats::nobs(m[[paste("men", y)]])
#       dd <- g("fin_d")
#       dd_se <- sqrt(a[["women fin_d"]][2]^2 + a[["men fin_d"]][2]^2)
#     REPLACE WITH:
if (FALSE) {
  a  <- lapply(m, .rv_att)              # per arm, own weights: the linearity check
  cc <- lapply(m, sa_cells)
  cg <- lapply(stats::setNames(nm = c("fin_d", "fin_e", "fin_o", "jttpeot_emp")),
               function(y) sa_common_gap(cc[[paste("women", y)]], cc[[paste("men", y)]]))
  g  <- function(y) cg[[y]]$post[["est"]]
  nn <- function(y) stats::nobs(m[[paste("women", y)]]) + stats::nobs(m[[paste("men", y)]])
  dd <- g("fin_d")
  dd_se <- cg$fin_d$post[["se"]]
}
#     FIND:   gp <- .rv_gap_path(vw, vm)          (the one inside SADDD)
#     REPLACE WITH:
if (FALSE) {
  gp <- list(k = cg$fin_d$k, g = cg$fin_d$g, V = cg$fin_d$V)
}

# 3e. BOOT. FIND the helper:
#       one <- function(d, y, what) {
#         m <- tryCatch(.rv_sa(d, y, vcov = "iid"), error = function(e) NULL)
#         if (is.null(m)) return(if (what == "att") NA_real_ else NULL)
#         if (what == "att") .rv_att(m)[1] else .rv_path(m)
#       }
#     REPLACE WITH:
if (FALSE) {
  # Both arms refitted on the replicate, then differenced on that replicate's
  # common weights, so the bootstrap carries the weights' variation too.
  cgap <- function(w, m, y) {
    fw <- tryCatch(.rv_sa(w, y, vcov = "iid"), error = function(e) NULL)
    fm <- tryCatch(.rv_sa(m, y, vcov = "iid"), error = function(e) NULL)
    if (is.null(fw) || is.null(fm)) return(NULL)
    tryCatch(sa_gap_fits(fw, fm, need_V = FALSE), error = function(e) NULL)
  }
  pick <- function(z, f) if (is.null(z)) NA_real_ else f(z)
}
#     FIND these seven lines inside the replicate loop:
#       aw <- one(w, "jttpewt_emp", "att"); am <- one(m, "jttpewt_emp", "att")
#       pw <- one(w, "ln_hourly_wage_real", "path"); pm <- one(m, "ln_hourly_wage_real", "path")
#       le <- if (is.null(pw) || is.null(pm)) NA_real_ else .rv_le(pw, pm)
#       if (is.finite(le) && !is.null(R$wage) && !identical(attr(le, "horizons"), R$wage$horizons))
#         hz_bad <- hz_bad + 1L
#       dw <- one(w, "fin_d", "att"); dm <- one(m, "fin_d", "att")
#       out[b, ] <- c(aw, am, aw - am, as.numeric(le), dw, dm, dw - dm)
#     REPLACE WITH:
if (FALSE) {
    gh <- cgap(w, m, "jttpewt_emp")
    gw <- cgap(w, m, "ln_hourly_wage_real")
    gd <- cgap(w, m, "fin_d")
    le <- if (is.null(gw)) NA_real_ else .rv_le_gap(gw)
    if (is.finite(le) && !is.null(R$wage) && !identical(attr(le, "horizons"), R$wage$horizons))
      hz_bad <- hz_bad + 1L
    # att_w and att_m are each arm on the replicate's common weights, so
    # \SAArmCorr is the correlation of the two parts of the common-weight gap.
    out[b, ] <- c(pick(gh, function(z) z$post_w), pick(gh, function(z) z$post_m),
                  pick(gh, function(z) z$post[["est"]]), as.numeric(le),
                  pick(gd, function(z) z$post_w), pick(gd, function(z) z$post_m),
                  pick(gd, function(z) z$post[["est"]]))
}


# =============================================================================
# 4. 05_review_response.R  (R1 recovery, R5 earnings, R6b wage windows, R9)
# =============================================================================
# 4a. FIND:   source("paper_numbers.R")
#     INSERT AFTER IT: the source block from section 0.
#
# 4b. REPLACE the whole rr_path_by_sex() function with:
if (FALSE) {
rr_path_by_sex <- function(d, y, k_lo, k_hi, fe = "person_id + wave") {
  fw <- rr_sa(d[d$female == 1, , drop = FALSE], y, k_lo, k_hi, fe)
  fm <- rr_sa(d[d$female == 0, , drop = FALSE], y, k_lo, k_hi, fe)
  w <- rr_es(fw, k_lo, k_hi); m <- rr_es(fm, k_lo, k_hi)
  # The gap on common weights, inside the window so the binned ends stay out.
  cg <- if (is.null(fw) || is.null(fm)) NULL else
    tryCatch(sa_gap_fits(fw, fm, k_range = c(k_lo, k_hi)), error = function(e) {
      cat("    common-weight gap failed for ", y, ": ", conditionMessage(e), "\n", sep = "")
      NULL })
  gap <- if (is.null(cg)) rr_gap_es(w, m) else
    list(rt = cg$k, g = cg$g, V = cg$V,
         tab = data.frame(event_time = cg$k, gap = cg$g, se = sqrt(diag(cg$V))))
  list(women = w, men = m, gap = gap, gap_separate = rr_gap_es(w, m))
}
}
# 4c. R6b compares against the literals -0.058, 0.025 and 7.92, which are the
#     separate-weight values. After the change they are stale: update them to
#     the new wptest.plain.* values, or drop the paper_* columns.


# =============================================================================
# 5. MASTER_training_y.R  (8.2 timing path, 8.2b decomposition)
# =============================================================================

# 5a. 8.2. FIND:
#         .tim_train <- .tim_train[order(.tim_train$k), ]
#     INSERT BEFORE IT:
if (FALSE) {
        .gk <- paste(.arm, "gap", .out)
        if (is.data.frame(.te[[.gk]])) {
          .tim_train <- data.frame(k = as.integer(.te[[.gk]]$event_time),
                                   gap = .te[[.gk]]$gap, se = .te[[.gk]]$se)
        } else {
          cat("  *** ", .gk, " is absent, so the training path differences the two\n",
              "      arms on their own weights. Rerun training_event_study.R.\n", sep = "")
        }
        rm(.gk)
}
#     and in the .train_src sprintf that follows, change
#       "Sun-Abraham, %s %s, women minus men, SE in quadrature"
#     to
#       "Sun-Abraham, %s %s, women minus men on common cohort weights"

# 5b. 8.2b. FIND these two lines:
#         .arms <- unique(sub(" (women|men) .*$", "", names(.te)))
#         .outs <- unique(sub("^\\S+ (women|men) ", "", names(.te)))
#     REPLACE WITH (the gap elements are not arm-by-sex series):
if (FALSE) {
    .ps   <- names(.te)[grepl(" (women|men) ", names(.te))]
    .arms <- unique(sub(" (women|men) .*$", "", .ps))
    .outs <- unique(sub("^\\S+ (women|men) ", "", .ps))
    rm(.ps)
}
#     FIND these two lines inside the .decomp rbind:
#         gap     = round(.d$att_mo - .d$att_fa, 4),
#         gap_se  = round(sqrt(.d$se_mo^2 + .d$se_fa^2), 4),
#     REPLACE WITH:
#         gap     = round(.cw_col(.te, .a, .o, .d, "gap", .d$att_mo - .d$att_fa), 4),
#         gap_se  = round(.cw_col(.te, .a, .o, .d, "se", sqrt(.d$se_mo^2 + .d$se_fa^2)), 4),
#     and define .cw_col once, before the `.decomp <- NULL` line:
if (FALSE) {
.cw_col <- function(te, a, o, d, col, fallback) {
  cg <- te[[paste(a, "gap", o)]]
  if (!is.data.frame(cg)) return(fallback)
  v <- cg[[col]][match(d$event_time, cg$event_time)]
  ifelse(is.na(v), fallback, v)
}
}


# =============================================================================
# 6. MASTER_paper_numbers.R  (side.* keys, and place.* through them)
# =============================================================================
# REPLACE the whole mpn_side() function with:
if (FALSE) {
mpn_side <- function(te) {
  if (!is.list(te) || is.null(names(te))) {
    cat("  paper numbers: .te is not in memory, so side is not registered.\n")
    .MPN$done[["side"]] <- NA_integer_
    return(invisible(NA_integer_))
  }
  ps   <- names(te)[grepl(" (women|men) ", names(te))]
  arms <- unique(sub(" (women|men) .*$", "", ps))
  outs <- unique(sub("^\\S+ (women|men) ", "", ps))
  rows <- list()
  for (a in arms) for (o in outs) {
    kw <- paste(a, "women", o); km <- paste(a, "men", o)
    if (!all(c(kw, km) %in% names(te))) next
    d <- merge(te[[kw]][, c("event_time", "att", "se")], te[[km]][, c("event_time", "att", "se")],
               by = "event_time", suffixes = c("_mo", "_fa"))
    if (!nrow(d)) next
    # The gap is the common-weight one when training_event_study.R wrote it;
    # the difference of the two arms stays as gap_separate.
    sep <- d$att_mo - d$att_fa; sep_se <- sqrt(d$se_mo^2 + d$se_fa^2)
    cg  <- te[[paste(a, "gap", o)]]
    i   <- if (is.data.frame(cg)) match(d$event_time, cg$event_time) else rep(NA_integer_, nrow(d))
    gc  <- if (is.data.frame(cg)) cg$gap[i] else rep(NA_real_, nrow(d))
    gs  <- if (is.data.frame(cg)) cg$se[i]  else rep(NA_real_, nrow(d))
    rows[[length(rows) + 1L]] <- data.frame(
      arm = a, outcome = o, k = as.integer(d$event_time),
      mothers = d$att_mo, mothers_se = d$se_mo, fathers = d$att_fa, fathers_se = d$se_fa,
      gap = ifelse(is.na(gc), sep, gc), gap_se = ifelse(is.na(gs), sep_se, gs),
      gap_separate = sep, common_weights = !is.na(gc), stringsAsFactors = FALSE)
  }
  x <- do.call(rbind, rows)
  if (is.null(x)) return(.mpn_reg(NULL, "side", NULL))
  if (!all(x$common_weights))
    cat("  paper numbers: side has ", sum(!x$common_weights), " row(s) on separate weights;\n",
        "  rerun training_event_study.R with the common-weight patch.\n", sep = "")
  x$common_weights <- NULL
  x$mothers_t <- abs(x$mothers / x$mothers_se)
  x$fathers_t <- abs(x$fathers / x$fathers_se)
  x$gap_t     <- abs(x$gap / x$gap_se)
  .MPN$side <- x
  .mpn_reg(x, "side", id = c("arm", "outcome", "k"),
           digits = c(mothers_t = 2, fathers_t = 2, gap_t = 2))
}
}


# =============================================================================
# 7. paper_tables.R  (the new macros)
# =============================================================================
# FIND:       put("SATrainingPostSE", fmt_plain(.sar$se[1], digits = 4),
#                 "its standard error, the two arms' variances added")
# REPLACE the label string with "its standard error, common weights, the two
# arms' variances added", and INSERT AFTER the call:
if (FALSE) {
    .cw <- function(s) {
      r <- .sa[.sa$sex == s & .sa$outcome == "jttpewt_emp", , drop = FALSE]
      if (nrow(r)) r$att[1] else NA_real_
    }
    if (is.finite(.cw("gap_separate"))) {
      put("SAWeightSep", fmt(.cw("gap_separate"), digits = 4),
          "the headline with each arm on its own weights, the estimator before the revision")
      put("SAWeightAvg", fmt(.cw("gap_average"), digits = 4),
          "the headline on the average of the two arms' weights")
      put("SAWeightMin", fmt(.cw("gap_minimum"), digits = 4),
          "the headline on the smaller of the two cell counts")
      put("SAWeightComp", fmt(.cw("gap_composition"), digits = 4),
          "separate less pooled: the part of the old estimate due to cohort composition")
      put("SASupportDropF", fmt_plain(100 * .cw("support_dropped_women"), 1),
          "percent of mothers' post-birth weight outside common support")
      put("SASupportDropM", fmt_plain(100 * .cw("support_dropped_men"), 1),
          "percent of fathers' post-birth weight outside common support")
    } else {
      cat("  cs_post_period_att.csv has no gap_separate row, so the common-weight\n",
          "  macros stay \\tofill. Run the patched CS_event_study.R.\n", sep = "")
    }
    rm(.cw)
}
