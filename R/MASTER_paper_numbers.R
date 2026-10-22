# =============================================================================
# MASTER_paper_numbers.R
#
# Registers the numbers MASTER_training_y.R produces or reads, so the paper
# can quote them as \pn{key} rather than as typed copies. paper_numbers.R
# explains the registry; this file only feeds it.
#
# MASTER sources this file once the panel is resolved and then calls one
# function per block, each on results that already exist. A call that fails
# prints why, and the run continues. Nothing here changes an estimate.
#
#   mpn_begin(panel)                  after td_frame() and the compat check
#   mpn_module_csv("trpost"),
#   mpn_module_csv("trwin")           after training_event_study.R
#   mpn_module_csv("dose")            after training_funding_divergence.R
#   mpn_module_csv("csatt"),
#   mpn_module_csv("wptest")          after CS_event_study.R
#   mpn_side(.te)                     in 8.2b, while .te is in memory
#   mpn_place(.pl_ratio)              in 8.2b, after training_place_ratio.csv
#   mpn_A5(.A5_PROFILE, .A5_COEFS)    in 8.3, before the two are removed
#   mpn_hours(.hrs)                   in 9.3, after hours_robustness.csv
#   mpn_ledger(LEDGER, BLIND_EST, .led_dir)   in PART 6A
#   mpn_end()                         before the log closes
#
# Each part is written to output/numbers/MASTER_training_y__<part>.csv and is
# replaced only when it is registered again, as in 05.
#
# THE KEYS
#   trpost.<frame>.<sex>.<outcome>.{att,se,mde}       training_post_att.csv
#   trwin.<frame>.<sex>.<outcome>.<k_lo>.<k_hi>.<column>   training_window_att.csv:
#       the average over the window, its pre-birth base (base, n_base,
#       base_pct) and the average as a percentage of that base (pct, pct_se,
#       pct_lo, pct_hi), as in trwin.employed_only.women.any_training.0.2.pct
#   dose.<arm>.<outcome>.<column>                      dosage_decomposition.csv
#   csatt.<sex>.<outcome>.{att,se,mde}                 cs_post_period_att.csv
#   wptest.<path>.<quantity>.<column>                  wage_path_tests.csv; the
#       rows of the gap net of the lead drift carry their horizon, as in
#       wptest.plain.gap_net_of_the_lead_drift_k10.estimate
#   side.<arm>.<outcome>.<k>.<column>                  8.2b, both sexes and the
#       gap by event time, unrounded; the training path of Figure 2 is
#       side.employed_only.training_paid_time.<k>.gap. The gap is on common
#       cohort weights; gap_separate is the arms' own-weight difference
#   csatt.<row>.<outcome>.att, for row in gap_separate, gap_average,
#       gap_minimum, gap_composition, support_dropped_women and
#       support_dropped_men: the headline under each weighting scheme,
#       from the extra rows CS_event_study.R writes to cs_post_period_att.csv
#   place.<arm>.<pair>.<k_lo>.<k_hi>.<column>          training_place_ratio.csv
#   A5prof.<years>.<column>, A5coef.<sex>.<column>,
#   A5tilt.women_minus_men.{slope,slope_se,slope_t}    8.3
#   hoursarm.<A to E>.{est,se,t,mde,n,persons}         9.3
#   ledger.<tag>.{est,se,t,p,mde,n,benchmark,benchmark_f}
#   ledgersum.dagger.{n,mde_min,mde_max}               PART 6A
#
# WHEN A NUMBER IS LEFT OUT
#   A module's csv is registered only if this run wrote it. Otherwise its part
#   keeps its previous registration, with the build tag of the run that made
#   it, so pn_write_tex() still catches a part from an older build.
#   The ledger is read, not written, by MASTER: 03a, 03b and 03c write it, and
#   a row can be carried from an earlier pass. A ledger row is registered only
#   if the module that wrote it ran after the panel file was built, judged by
#   that module's training_ledger_<module>.csv. Rows that fail are named in the
#   log. If the panel file was only copied, not rebuilt, set
#   options(master_pn_ledger_any_age = TRUE) before running MASTER.
# =============================================================================

suppressPackageStartupMessages(library(dplyr))

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

if (!exists("pn_register_csv", mode = "function")) source("paper_numbers.R")

.MPN <- new.env()
.MPN$script <- "MASTER_training_y.R"
.MPN$parts  <- c("trpost", "trwin", "dose", "csatt", "wptest", "side", "place", "A5prof", "A5coef",
                 "A5tilt", "hoursarm", "ledger", "ledgersum")

# The contract's mde(); the same formula if a session lacks it.
.mpn_mde <- function(se) {
  if (exists("mde", mode = "function")) return(mde(se))
  ifelse(is.finite(se) & se > 0, (stats::qnorm(0.975) + stats::qnorm(0.8)) * se, NA_real_)
}

# Where MASTER reads its own outputs from.
.mpn_path <- function(f) {
  if (exists(".in_path", mode = "function")) return(.in_path(f))
  if (exists(".out_path", mode = "function")) return(.out_path(f))
  f
}

.mpn_part_file <- function(part)
  file.path(PN_DIR, paste0(sub("\\.[Rr]$", "", .MPN$script), "__", pn_key(part), ".csv"))

mpn_begin <- function(panel) {
  bt <- attr(panel, "build_tag")
  .MPN$bt <- if (is.null(bt) || !length(bt)) NA_character_ else as.character(bt[1])
  .MPN$t0 <- Sys.time()
  pm <- attr(panel, "td_panel_mtime"); pf <- attr(panel, "td_panel_file")
  .MPN$panel_mtime <- if (!is.null(pm)) as.POSIXct(pm) else
    if (!is.null(pf) && file.exists(pf)) file.mtime(pf) else NULL
  .MPN$done <- list(); .MPN$side <- NULL
  cat(sprintf("\n  paper numbers: MASTER registers into %s, build_tag %s\n",
              normalizePath(PN_DIR, winslash = "/", mustWork = FALSE), .MPN$bt))
  if (is.null(.MPN$panel_mtime))
    cat("  paper numbers: the panel carries no file time, so the ledger's age cannot be\n",
        "  checked against it; every ledger row will be registered.\n", sep = "")
  invisible(TRUE)
}

# One part of the registry, with the failure reported rather than raised.
.mpn_reg <- function(df, part, id, digits = NULL, default_digits = 3L) {
  if (is.null(.MPN$t0)) {
    cat("  paper numbers: mpn_begin() has not run, so ", part, " is not registered.\n", sep = "")
    return(invisible(NA_integer_))
  }
  if (is.null(df) || !nrow(df)) {
    cat("  paper numbers: ", part, " has no rows, so its previous registration stands.\n", sep = "")
    .MPN$done[[part]] <- NA_integer_
    return(invisible(NA_integer_))
  }
  n <- tryCatch(pn_register_csv(as.data.frame(df), prefix = part, part = part, script = .MPN$script,
                                build_tag = .MPN$bt, id = id, digits = digits,
                                default_digits = default_digits),
                error = function(e) {
                  cat("  *** paper numbers NOT registered for ", part, ": ", conditionMessage(e), "\n", sep = "")
                  NA_integer_
                })
  .MPN$done[[part]] <- n
  invisible(n)
}

# ---- The modules' own tables -------------------------------------------------------

# The quantity labels of wage_path_tests.csv are unique except the rows of the
# gap net of the lead drift, one per horizon, which carry it in the key.
.mpn_wp_row <- function(x) {
  q <- as.character(x$quantity); h <- trimws(as.character(x$horizons))
  net <- !is.na(q) & q == "gap net of the lead drift"
  hk <- suppressWarnings(as.integer(h))
  q[net] <- paste0(q[net], " k", ifelse(is.na(hk[net]), h[net], as.character(hk[net])))
  q
}

.MPN_CSV <- list(
  trpost = list(file = "training_post_att.csv", from = "training_event_study.R",
                id = c("frame", "sex", "outcome"), mde = TRUE, digits = NULL),
  trwin  = list(file = "training_window_att.csv", from = "training_event_study.R",
                id = c("frame", "sex", "outcome", "k_lo", "k_hi"), mde = TRUE,
                digits = c(base = 3, n_base = 0, base_pct = 1, pct = 0, pct_se = 1,
                           pct_lo = 0, pct_hi = 0)),
  dose   = list(file = "dosage_decomposition.csv", from = "training_funding_divergence.R",
                id = c("arm", "outcome"), mde = FALSE,
                digits = c(part2_pct = 1, lee_lo_pct = 1, lee_hi_pct = 1)),
  csatt  = list(file = "cs_post_period_att.csv", from = "CS_event_study.R",
                id = c("sex", "outcome"), mde = TRUE, digits = NULL),
  wptest = list(file = "wage_path_tests.csv", from = "CS_event_study.R",
                id = c("path", "row"), mde = FALSE, digits = c(stat = 2, df = 0, p = 3)))

mpn_module_csv <- function(part) {
  s <- .MPN_CSV[[part]]
  if (is.null(s)) stop("no csv is defined for part '", part, "'.")
  f <- .mpn_path(s$file)
  if (!file.exists(f)) {
    cat("  paper numbers: ", s$file, " is absent, so ", part, " is not registered.\n", sep = "")
    .MPN$done[[part]] <- NA_integer_
    return(invisible(NA_integer_))
  }
  if (!is.null(.MPN$t0) && file.mtime(f) < .MPN$t0 - 5) {
    cat("  paper numbers: ", s$file, " dates from ", format(file.mtime(f), "%Y-%m-%d %H:%M"),
        ", before this run, so ", s$from, " did not rewrite it.\n",
        "    ", part, " keeps its previous registration.\n", sep = "")
    .MPN$done[[part]] <- NA_integer_
    return(invisible(NA_integer_))
  }
  x <- utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  if (part == "wptest") x$row <- .mpn_wp_row(x)
  if (isTRUE(s$mde) && "se" %in% names(x)) x$mde <- .mpn_mde(x$se)
  .mpn_reg(x, part, s$id, s$digits)
}

# ---- 8.2b: which side moves, unrounded ---------------------------------------------
# The same table as training_decomposition.csv, built from .te before the
# rounding to four decimals that file applies. The training path of the
# timing figure is the gap row of employed_only training_paid_time, so it is
# registered here, by arm and outcome, rather than from timing_paths.csv.
#
# v-cw. THE GAP IS ON COMMON COHORT WEIGHTS. training_event_study.R now writes
# "<arm> gap <outcome>", the women-minus-men difference formed inside each
# cohort-by-event cell and averaged on pooled cell counts. side.*.gap and
# side.*.gap_se read it, and so does mpn_place() through .MPN$side. The
# difference of the two arms, each on its own weights, is kept as
# side.*.gap_separate. mothers and fathers are within-sex comparisons and
# keep their own weights.
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
    sep <- d$att_mo - d$att_fa; sep_se <- sqrt(d$se_mo^2 + d$se_fa^2)
    cg  <- te[[paste(a, "gap", o)]]
    ok  <- is.data.frame(cg) && all(c("event_time", "gap", "se") %in% names(cg))
    i   <- if (ok) match(d$event_time, cg$event_time) else rep(NA_integer_, nrow(d))
    gc  <- if (ok) cg$gap[i] else rep(NA_real_, nrow(d))
    gs  <- if (ok) cg$se[i]  else rep(NA_real_, nrow(d))
    rows[[length(rows) + 1L]] <- data.frame(
      arm = a, outcome = o, k = as.integer(d$event_time),
      mothers = d$att_mo, mothers_se = d$se_mo, fathers = d$att_fa, fathers_se = d$se_fa,
      gap = ifelse(is.na(gc), sep, gc), gap_se = ifelse(is.na(gs), sep_se, gs),
      gap_separate = sep, .cw = !is.na(gc), stringsAsFactors = FALSE)
  }
  x <- do.call(rbind, rows)
  if (is.null(x)) return(.mpn_reg(NULL, "side", NULL))
  if (!all(x$.cw))
    cat("  paper numbers: ", sum(!x$.cw), " side row(s) carry the separate-weight gap,\n",
        "  because training_event_study.R wrote no common-weight series for them.\n",
        "  Rerun it with the common-weight patch before quoting side.*.gap.\n", sep = "")
  x$.cw <- NULL
  x$mothers_t <- abs(x$mothers / x$mothers_se)
  x$fathers_t <- abs(x$fathers / x$fathers_se)
  x$gap_t     <- abs(x$gap / x$gap_se)
  .MPN$side <- x
  .mpn_reg(x, "side", id = c("arm", "outcome", "k"),
           digits = c(mothers_t = 2, fathers_t = 2, gap_t = 2))
}

# ---- 8.2b: the workplace pair against all places ------------------------------------
# MASTER computes these from the gaps rounded to four decimals. The windows are
# kept, and the two means are recomputed from the unrounded gaps when
# mpn_side() has run, so a percentage quoted to the unit is not rounded twice.
mpn_place <- function(pl) {
  if (is.null(pl) || !nrow(pl)) return(.mpn_reg(NULL, "place", NULL))
  pl <- as.data.frame(pl)
  s <- .MPN$side
  wide <- c(jttpewt_any = "training_paid_time", jttpeot_any = "training_own_time")
  if (!is.null(s)) for (i in seq_len(nrow(pl))) {
    w <- as.character(pl$pair[i]); al <- unname(wide[w])
    if (is.na(al)) next
    win <- function(o) s[s$arm == pl$arm[i] & s$outcome == o & s$k >= pl$k_lo[i] &
                           s$k <= pl$k_hi[i], c("k", "gap")]
    m <- merge(win(w), win(al), by = "k", suffixes = c("_w", "_a"))
    if (nrow(m) != pl$horizons[i]) next      # not the same horizons: keep MASTER's values
    mw <- mean(m$gap_w); ma <- mean(m$gap_a)
    ok <- is.finite(mw) && is.finite(ma) && mw != 0 && sign(mw) == sign(ma)
    r <- if (ok) ma / mw else NA_real_
    pl$mean_gap_workplace[i] <- mw; pl$mean_gap_all_places[i] <- ma
    pl$all_over_workplace[i] <- r
    pl$pooling_raises_pct[i]    <- if (ok) 100 * (r - 1) else NA_real_
    pl$workplace_smaller_pct[i] <- if (ok) 100 * (1 - 1 / r) else NA_real_
  }
  pl$comparable <- NULL
  .mpn_reg(pl, "place", id = c("arm", "pair", "k_lo", "k_hi"),
           digits = c(horizons = 0, pooling_raises_pct = 0, workplace_smaller_pct = 0))
}

# ---- 8.3: the cross-partial profile --------------------------------------------------
# Units are those of A5_crosspartial_profile.csv: percentage points, x100.
mpn_A5 <- function(profile, coefs) {
  if (!is.null(profile) && nrow(profile)) {
    p <- as.data.frame(profile)
    p$mde_gap <- .mpn_mde(p$se_gap)
    p$t_men   <- abs(p$men / p$se_men)
    p$t_women <- abs(p$women / p$se_women)
    .mpn_reg(p, "A5prof", id = "years",
             digits = c(men = 4, se_men = 4, women = 4, se_women = 4, gap = 4, se_gap = 4,
                        t_gap = 2, mde_gap = 3, t_men = 2, t_women = 2))
  } else .mpn_reg(NULL, "A5prof", NULL)
  if (!is.null(coefs) && nrow(coefs)) {
    cf <- as.data.frame(coefs)
    tilt <- c("gap_slope_per_year", "gap_slope_se", "gap_slope_t")
    .mpn_reg(cf[, setdiff(names(cf), tilt), drop = FALSE], "A5coef", id = "sex",
             digits = c(centre = 2, n = 0), default_digits = 4L)
    if (all(tilt %in% names(cf)))
      .mpn_reg(data.frame(contrast = "women_minus_men", slope = cf$gap_slope_per_year[1],
                          slope_se = cf$gap_slope_se[1], slope_t = cf$gap_slope_t[1]),
               "A5tilt", id = "contrast", digits = c(slope = 4, slope_se = 4, slope_t = 2))
  } else .mpn_reg(NULL, "A5coef", NULL)
  invisible(TRUE)
}

# ---- 9.3: the hours arms ---------------------------------------------------------------
# Arms are keyed by their letter. t and the MDE are recomputed unrounded.
mpn_hours <- function(hrs) {
  if (is.null(hrs) || !nrow(hrs)) return(.mpn_reg(NULL, "hoursarm", NULL))
  h <- as.data.frame(hrs)
  h$arm_id <- toupper(substr(trimws(as.character(h$arm)), 1, 1))
  h$t   <- h$est / h$se
  h$mde <- .mpn_mde(h$se)
  .mpn_reg(h[, intersect(c("arm_id", "est", "se", "t", "mde", "n", "persons"), names(h))],
           "hoursarm", id = "arm_id", digits = c(t = 2))
}

# ---- PART 6A: the ledger --------------------------------------------------------------
# Estimates come from the sealed list, unrounded; n and the benchmarks from the
# ledger. A sealed estimate with no ledger row, such as the withheld dosage
# arm, takes the module of the ledger rows in its design (T5 for T5_...).
mpn_ledger <- function(ledger, blind, dir) {
  if (is.null(ledger) && !length(blind)) {
    cat("  paper numbers: no ledger and no sealed estimates, so ledger is not registered.\n")
    .MPN$done[["ledger"]] <- NA_integer_
    return(invisible(NA_integer_))
  }
  led  <- if (is.null(ledger)) data.frame(tag = character(), stringsAsFactors = FALSE) else as.data.frame(ledger)
  tags <- unique(c(as.character(led$tag), names(blind)))
  tags <- tags[!is.na(tags) & nzchar(tags)]
  cell <- function(t, j) {
    b <- blind[[t]]
    if (is.null(b) || is.null(dim(b)) || ncol(b) < j) return(NA_real_)
    v <- suppressWarnings(as.numeric(b[1, j]))
    if (length(v) == 1L) v else NA_real_
  }
  i   <- match(tags, led$tag)
  col <- function(nm) if (nm %in% names(led)) suppressWarnings(as.numeric(led[[nm]][i])) else rep(NA_real_, length(tags))
  x <- data.frame(tag = tags,
                  est = vapply(tags, cell, 0, j = 1), se = vapply(tags, cell, 0, j = 2),
                  t   = vapply(tags, cell, 0, j = 3), p  = vapply(tags, cell, 0, j = 4),
                  n = col("n"), benchmark = col("benchmark"), benchmark_f = col("benchmark_f"),
                  stringsAsFactors = FALSE, row.names = NULL)
  x$se  <- ifelse(is.na(x$se), col("se"), x$se)
  x$mde <- .mpn_mde(x$se)

  # Which module wrote each row, and whether it ran after the panel was built.
  mod <- if ("module" %in% names(led)) as.character(led$module[i]) else rep(NA_character_, length(tags))
  des <- sub("^(T[0-9]+).*$", "\\1", tags)
  for (r in which(is.na(mod))) {
    m <- unique(stats::na.omit(mod[des == des[r]]))
    if (length(m) == 1L) mod[r] <- m
  }
  ran <- vapply(mod, function(m) {
    f <- file.path(dir, paste0("training_ledger_", m, ".csv"))
    if (is.na(m) || !file.exists(f)) NA_real_ else as.numeric(file.mtime(f))
  }, 0)
  pm <- .MPN$panel_mtime
  any_age <- isTRUE(getOption("master_pn_ledger_any_age", FALSE))
  fresh <- if (is.null(pm) || any_age) rep(TRUE, length(tags)) else !is.na(ran) & ran >= as.numeric(pm)
  if (any(!fresh)) {
    who <- unique(ifelse(is.na(mod[!fresh]), "no module recorded", mod[!fresh]))
    cat("  *** paper numbers: ", sum(!fresh), " ledger row(s) left out, because the module that\n",
        "      wrote them ran before the panel file was built: ", paste(who, collapse = ", "), ".\n",
        "      Rerun those modules. If the panel file was only copied, set\n",
        "      options(master_pn_ledger_any_age = TRUE) and run this block again.\n", sep = "")
  }
  if (!any(fresh)) {
    f <- .mpn_part_file("ledger")
    if (file.exists(f)) file.remove(f)
    cat("  paper numbers: no ledger row is current, so the ledger part is removed.\n")
    .MPN$done[["ledger"]] <- 0L
  } else {
    .mpn_reg(x[fresh, , drop = FALSE], "ledger", id = "tag",
             digits = c(t = 2, p = 3, benchmark = 4, benchmark_f = 4))
  }

  # The rows the ledger table marks with a dagger: limited against the pooled
  # base, OK against women's own. paper_tables.R applies the same rule.
  if (all(c("verdict", "verdict_f") %in% names(led))) {
    vd <- toupper(as.character(led$verdict)); vf <- toupper(as.character(led$verdict_f))
    dag <- as.character(led$tag[!is.na(vd) & vd == "LIMITED" & !is.na(vf) & vf == "OK"])
    k <- match(dag, x$tag)
    if (length(dag) && all(fresh[k]) && any(is.finite(x$mde[k]))) {
      .mpn_reg(data.frame(what = "dagger", n = length(dag),
                          mde_min = min(x$mde[k], na.rm = TRUE), mde_max = max(x$mde[k], na.rm = TRUE)),
               "ledgersum", id = "what")
    } else if (length(dag)) {
      f <- .mpn_part_file("ledgersum")
      if (file.exists(f)) file.remove(f)
      cat("  paper numbers: a daggered ledger row is not current, so ledgersum is not registered.\n")
    }
  }
  invisible(TRUE)
}

# ---- Close ------------------------------------------------------------------------------
mpn_end <- function() {
  d <- .MPN$done
  cat("\n  paper numbers registered by MASTER in this run:\n")
  if (!length(d)) cat("    none\n")
  for (p in names(d))
    cat(sprintf("    %-10s %s\n", p, if (is.na(d[[p]])) "not registered (see above)" else paste(d[[p]], "key(s)")))
  miss <- setdiff(.MPN$parts, names(d))
  if (length(miss))
    cat("    not reached in this run: ", paste(miss, collapse = ", "), ".\n",
        "    Their previous registrations, if any, stand.\n", sep = "")
  cat("  Next: pn_write_tex() writes the macro file (paper_tables.R calls it).\n")
  invisible(d)
}
