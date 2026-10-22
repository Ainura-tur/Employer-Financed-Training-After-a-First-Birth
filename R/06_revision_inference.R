# =============================================================================
# 06_revision_inference.R   The contrasts and standard errors the revision adds
#
# WHAT IT COMPUTES. The revised manuscript defines twelve macros that no
# script writes yet. Two are arithmetic on the headline, and paper_tables.R
# computes them from cs_post_period_att.csv (\SATrainingPostPP and
# \SATrainingPostPct). This script computes the other ten, plus the sample
# size of the headline for the new row of Table 2:
#
#   block      macro                 quantity
#   REPRO_SA   \SATrainingPostN      person-waves behind the Sun-Abraham headline
#   STATIC     \DiDSECouple          static DiD, employed or on leave, couple-clustered
#   STATIC     \DddSECouple          triple difference, sex-specific source-by-wave
#                                    effects, couple-clustered
#   SADDD      \SADddATT             Sun-Abraham average over k >= 0 of employer-
#                                    financed less own-time training, women less men
#   SADDD      \SADddSE              its SE, the two arms' variances summed
#   SADDD      \SADddPreP            p-value, joint test that its gaps at k = -8..-2
#                                    are zero
#   STAYLEAVE  \StayLeavePropDiff    stay less leave, proportional change from year 2,
#                                    both groups in one model against all fathers
#   STAYLEAVE  \StayLeavePropDiffSE  its SE, delta method, pre-birth bases as known
#   BOOT       \SASECouple           couple-bootstrap SD of the headline gap
#   BOOT       \SAArmCorr            couple-bootstrap correlation of the two arms
#   BOOT       \WageWindowSECouple   couple-bootstrap SD of the wage gap, years 5-10
#                                    less years 0-4
#
# COUPLE CLUSTERS. A cluster joins each person with every partner the cross-
# wave partner identifier links to them in any wave, and with the partners of
# those partners: the connected components of the partner graph. A person
# never partnered is a cluster alone. referee8_couple_clustering.R keys on the
# first partner only, so the two definitions differ for anyone with more than
# one partner, and the log counts them. revision_couple_clusters.csv holds
# the clusters, so referee8 can read the same ones.
#
# TWO FRAMES, EACH THE ONE ITS PUBLISHED ESTIMATE USED. The Sun-Abraham blocks
# rebuild CS_event_study.R's roster from td_frame() with CS's cohort rule. The
# static designs and the attachment groups use 03a's event frame,
# handoff/ev.rds, as 05_review_response.R does; without it they fall back to
# the CS roster and say so.
#
# REPRODUCE FIRST. Each value rests on a published estimate, and the script
# reproduces that estimate before it reports anything built on it:
#   the Sun-Abraham gap        cs_post_period_att.csv, gap row for jttpewt_emp
#   the wage window            wage_path_tests.csv, plain path, late less early
#   T1_attached and T3_ddd     BLIND_training_estimates.rds and the ledger's n
#   own-time Sun-Abraham       training_post_att.csv, employed_only gap
#   the two attachment groups  R11_exit_by_group.csv and R11_exit_expectations.csv
# A value whose base does not reproduce is written with status "not
# reproduced", and paper_tables.R then leaves its macro as \tofill.
#
# THE ESTIMATOR IS UNBINNED, AS SECTION 5.1 SAYS. CS_event_study.R asks fixest
# for endpoint bins at |k| >= 8 and falls back to the unbinned fit when fixest
# refuses them, which fixest 0.14 always does. This script fits the unbinned
# estimator directly. If the reproduction fails here and nothing else changed,
# CS may have run its binned branch on another fixest version.
#
# OUTPUTS, in output/diagnostics:
#   revision_inference_summary.csv   one row per quantity: quantity, macro, value,
#                                    status, block, basis, written, checked (UTC),
#                                    build_tag, inputs, files. paper_tables.R reads it.
#   revision_inference_boot.csv      the bootstrap replicates
#   revision_couple_clusters.csv     person_id, couple, persons in the cluster
#   revision_financing_path.csv      the Sun-Abraham financing contrast by event
#                                    time, for a figure
# A run of some blocks replaces their rows and keeps the others. Each kept row
# is checked again: "inputs" records the published values its block was
# checked against, and a kept row whose published values have since changed is
# marked "stale (inputs changed)"; one from another build of the panel is
# marked "stale (older build)". A kept row that passes gets a new "checked"
# time. "files" names the published files its block read; paper_tables.R
# withholds the row if one of them has gone or changed since "checked".
# "build_tag" is the frame's, except for the static designs and the attachment
# groups, which carry the build of handoff/ev.rds, the frame they were fitted
# on, so an ev.rds older than the panel cannot pass as current.
#
# SETTINGS are environment variables, as in 05_review_response.R:
#   REV_BOOT_B   couple-bootstrap replicates (default 199; 0 skips the bootstrap).
#                Below 199 the bootstrap rows are marked "test run" and stay
#                off the page.
#   REV_BLOCKS   all (default), or a list of STATIC, SADDD, STAYLEAVE, BOOT.
#                REPRO_SA and REPRO_WAGE always run, because other blocks check
#                against them.
#
# RUN from the project root after 03a, 03b, CS_event_study.R,
# training_event_study.R, 05_review_response.R (R11) and MASTER:
#   source("06_revision_inference.R")
# then paper_tables.R. Each bootstrap replicate fits six Sun-Abraham models,
# so 199 replicates fit 1,194; the log reports the time to go after the first
# ten. On one core and a roster of about 63,000 person-waves, a replicate took
# about five seconds in testing.
#
# POST-HOC. Everything here was added in revision. It writes nothing the ledger
# or the sealed estimates own.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
})

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

# ---- the contract -------------------------------------------------------------
if (!(exists("PROJ_ROOT") && exists("td_frame", mode = "function"))) {
  .c <- c("00_contract.R", file.path("..", "00_contract.R"))
  .c <- .c[file.exists(.c)]
  if (!length(.c))
    stop("00_contract.R not found from ", getwd(), ". setwd() to the ",
         "project root and source this file again.", call. = FALSE)
  cat("Contract: not loaded in this session; sourcing ",
      normalizePath(.c[1], winslash = "/"), "\n", sep = "")
  source(.c[1])
  rm(.c)
}
stopifnot(exists("PROJ_ROOT"), exists("td_frame", mode = "function"))
# v-cw. COMMON COHORT WEIGHTS. sa_common_weights.R, beside this script, forms
# each women-minus-men contrast inside the cohort-by-event-time cells and
# averages it on one set of weights. sa_try() returns one fit's cells, or
# NULL with a note when it cannot read them.
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

# ---- the log ------------------------------------------------------------------
# The contract's logger, as in referee8: it inherits a caller's sink rather
# than opening a rival, and the body below closes it on every exit.
.rv_log <- exists("td_log_open", mode = "function")
if (.rv_log) {
  .rv_try <- try(td_log_open("06_revision_inference.R"), silent = TRUE)
  if (inherits(.rv_try, "try-error"))
    cat("  td_log_open() failed: ", conditionMessage(attr(.rv_try, "condition")),
        "\n  Output goes to the console only.\n", sep = "")
  rm(.rv_try)
} else {
  cat("  no contract logger, so output goes to the console only.\n")
}

.rv_done <- FALSE
tryCatch(local({

# =============================================================================
# SETTINGS
# =============================================================================
REV_BOOT_B <- suppressWarnings(as.integer(Sys.getenv("REV_BOOT_B", "199")))
if (is.na(REV_BOOT_B) || REV_BOOT_B < 0L) REV_BOOT_B <- 199L
REV_BOOT_MIN <- 199L   # fewer replicates than this is a test run
REV_SEED   <- 20261009L
REV_BLOCKS <- toupper(trimws(strsplit(Sys.getenv("REV_BLOCKS", "all"), ",")[[1]]))
.bad <- setdiff(REV_BLOCKS, c("ALL", "STATIC", "SADDD", "STAYLEAVE", "BOOT"))
if (length(.bad))
  stop("REV_BLOCKS: unknown block(s) ", paste(.bad, collapse = ", "),
       ". Use all, or a list of STATIC, SADDD, STAYLEAVE, BOOT.", call. = FALSE)
rm(.bad)
rv_run     <- function(b) "ALL" %in% REV_BLOCKS || toupper(b) %in% REV_BLOCKS
# Reproduction tolerance. The published Sun-Abraham values are stored to six
# decimals and the sealed estimates in full, so a difference above this is a
# different sample or a different estimator, not rounding.
REV_TOL    <- 1e-4
REV_PRE    <- -3:-1    # the pre-birth base, as 03_common.R and 05 R11 define it
REV_LEADS  <- -8:-2    # the lead test
REV_EARLY  <- 0:4      # the wage windows, as CS_event_study.R defines them
REV_LATE   <- 5:10
REV_FE     <- "person_id + wave^female"   # 03a's fe_block(), and 05's RR_FE
REV_MAX_SHARED <- 10L  # a partner id shared by more people than this is a code
REV_LEAVE  <- c("on_maternity_leave", "on_parental_leave", "nlf_reason_maternity",
                "nlreapm")                # 03a's order of preference

DIAG <- if (exists("DIR_DIAG")) DIR_DIAG else file.path(PROJ_ROOT, "output", "diagnostics")
dir.create(DIAG, showWarnings = FALSE, recursive = TRUE)
OUT_SUMMARY  <- file.path(DIAG, "revision_inference_summary.csv")
OUT_BOOT     <- file.path(DIAG, "revision_inference_boot.csv")
OUT_CLUSTERS <- file.path(DIAG, "revision_couple_clusters.csv")
OUT_PATH     <- file.path(DIAG, "revision_financing_path.csv")
# Times are UTC to the millisecond, so paper_tables.R can compare them with
# file dates on any machine and in any time zone.
.rv_now <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%OS3", tz = "UTC")

cat("  blocks: ", paste(REV_BLOCKS, collapse = ", "), " | bootstrap replicates: ",
    REV_BOOT_B, " | fixest ", as.character(utils::packageVersion("fixest")), "\n", sep = "")

# =============================================================================
# THE PUBLISHED ARTEFACTS, FOUND BY NAME
# =============================================================================
# review_response before diagnostics, so a stray copy of an R11 table in
# diagnostics/ cannot win over the one 05 writes.
.rv_dirs <- unique(normalizePath(c(
  get0("DIR_RESULTS", ifnotfound = file.path(PROJ_ROOT, "output", "results")),
  get0("DIR_DESIGNS", ifnotfound = file.path(PROJ_ROOT, "output", "designs")),
  file.path(DIAG, "review_response"), DIAG, PROJ_ROOT), winslash = "/", mustWork = FALSE))
.rv_file <- function(name, quiet = FALSE) {
  p <- file.path(.rv_dirs, name)
  p <- p[file.exists(p)]
  if (!length(p)) {
    if (!quiet) cat("  [missing] ", name, "\n", sep = "")
    return(NA_character_)
  }
  if (length(p) > 1L && !quiet)
    cat("  NOTE: ", name, " is in ", length(p), " folders; reading ", p[1], "\n", sep = "")
  p[1]
}
.rv_csv <- function(name, quiet = FALSE) {
  p <- .rv_file(name, quiet)
  if (is.na(p)) NULL else utils::read.csv(p, stringsAsFactors = FALSE)
}
.rv_rds <- function(name, quiet = FALSE) {
  p <- .rv_file(name, quiet)
  if (is.na(p)) NULL else readRDS(p)
}
# A key from the paper-number registry, the value \pn{key} prints.
.rv_registered <- function(key) {
  d  <- if (exists("PN_DIR")) PN_DIR else file.path(PROJ_ROOT, "output", "numbers")
  fs <- list.files(d, pattern = "\\.csv$", full.names = TRUE)
  fs <- fs[order(basename(fs) != "MASTER_training_y__ledger.csv")]
  for (f in fs) {
    r <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.data.frame(r) && all(c("key", "value") %in% names(r)) && key %in% r$key)
      return(suppressWarnings(as.numeric(r$value[match(key, r$key)])))
  }
  NA_real_
}

# The published values each block checks against, as one string, with the
# files it read as an attribute. Taken when a block starts and again when it
# ends, and stored with its rows, so a later partial run can tell whether the
# values have moved and paper_tables.R can date the files themselves. The
# blocks that use the couple clusters carry their checksum too, so a change in
# how the clusters are built also counts as a change of input.
.rv_sig <- function(block) {
  used <- character(0)
  rd <- function(name, csv = TRUE) {
    p <- .rv_file(name, quiet = TRUE)
    used <<- c(used, if (is.na(p)) paste0("MISSING:", name) else p)
    if (is.na(p)) NULL else tryCatch(if (csv) utils::read.csv(p, stringsAsFactors = FALSE) else
      readRDS(p), error = function(e) NULL)
  }
  nm <- function(x) if (!length(x)) "NA" else
    paste(trimws(formatC(suppressWarnings(as.numeric(x)), digits = 10, format = "g")),
          collapse = ",")
  sa <- function() {
    p <- rd("cs_post_period_att.csv")
    m <- rd("cs_event_study_sample_manifest.rds", csv = FALSE)
    paste0("cspa:", if (is.null(p)) "missing" else
             nm(unlist(p[p$outcome == "jttpewt_emp", c("att", "se")])),
           ";manifest:", if (is.null(m)) "missing" else
             nm(c(m$n_persons_full, m$n_treated, m$n_obs_full)))
  }
  wage <- function() {
    w <- rd("wage_path_tests.csv")
    paste0("wpt:", if (is.null(w)) "missing" else
             nm(unlist(w[w$path == "plain" & w$quantity %in% c("late window minus early window",
                                                               "joint test, all leads zero"),
                         c("estimate", "se", "stat")])))
  }
  cl <- function() paste0(";clusters:", get0("CL_SIG", ifnotfound = "none"))
  out <- switch(block,
    REPRO_SA = sa(),
    REPRO_WAGE = wage(),
    BOOT = paste0(sa(), "|", wage(), cl()),
    STATIC = {
      s <- rd("BLIND_training_estimates.rds", csv = FALSE)
      l <- rd("training_ledger.csv")
      paste0("sealed:", if (is.null(s)) "missing" else
               nm(c(s[["T1_attached"]][1, 1:2], s[["T3_ddd"]][1, 1:2])),
             ";ledger:", if (is.null(l)) "missing" else
               nm(l$n[match(c("T1_attached", "T3_ddd"), l$tag)]), cl())
    },
    SADDD = {
      t <- rd("training_post_att.csv")
      paste0(sa(), "|trpost:", if (is.null(t)) "missing" else
               nm(t$att[t$frame == "employed_only" & t$sex == "gap" & t$outcome == "jttpeot_any"]))
    },
    STAYLEAVE = {
      g <- rd("R11_exit_by_group.csv")
      x <- rd("R11_exit_expectations.csv")
      paste0("R11g:", if (is.null(g)) "missing" else
               nm(unlist(g[g$window == "k>=2" & g$outcome == "E1", c("est", "se", "n", "group_base")])),
             ";R11b:", if (is.null(x)) "missing" else
               nm(x$est[x$label == "(b) by pre-birth chance, k>=2" & x$outcome == "E1"]))
    },
    "")
  structure(out, files = paste(unique(used), collapse = "|"))
}

# =============================================================================
# THE SUMMARY, AND THE BLOCK RUNNER
# =============================================================================
S <- new.env()
S$rows <- list()
S$ran  <- character(0)
R <- new.env()            # results that one block hands to another
.rv_add <- function(quantity, value, block, macro = "", status = "check", basis = "") {
  v <- suppressWarnings(as.numeric(value))
  S$rows[[length(S$rows) + 1L]] <- data.frame(
    quantity = quantity, macro = macro,
    value = if (length(v) == 1L && is.finite(v)) v else NA_real_,
    status = status, block = block, basis = basis, stringsAsFactors = FALSE)
}
# The macros each block owes, so a block that fails says so for each of them.
.REV_MACROS <- list(
  REPRO_SA   = c(sa_n = "SATrainingPostN"),
  REPRO_WAGE = character(0),
  STATIC     = c(did_attached_se_couple = "DiDSECouple", ddd_se_couple = "DddSECouple"),
  SADDD      = c(sa_dd_att = "SADddATT", sa_dd_se = "SADddSE", sa_dd_leads_p = "SADddPreP"),
  STAYLEAVE  = c(stay_leave_prop_diff = "StayLeavePropDiff",
                 stay_leave_prop_diff_se = "StayLeavePropDiffSE"),
  BOOT       = c(sa_gap_se_couple = "SASECouple", sa_arm_corr = "SAArmCorr",
                 wage_window_se_couple = "WageWindowSECouple"))
# Each block stamps its own rows: when it checked, against which published
# values and files, and on which build. The published values are read before
# the block runs and again after it; if they moved in between, its rows are
# marked stale rather than stored against values it never compared.
.rv_block <- function(name, label, expr) {
  cat("\n", strrep("=", 78), "\n", name, ": ", label, "\n", strrep("=", 78), "\n", sep = "")
  S$ran <- c(S$ran, name)
  t0  <- .rv_now()
  sg0 <- .rv_sig(name)
  n0  <- length(S$rows)
  tryCatch({
    force(expr)
    invisible(TRUE)
  }, error = function(e) {
    cat("  *** BLOCK FAILED: ", conditionMessage(e), "\n", sep = "")
    S$rows <- S$rows[seq_len(n0)]
    for (q in names(.REV_MACROS[[name]]))
      .rv_add(q, NA_real_, name, .REV_MACROS[[name]][[q]], "failed", conditionMessage(e))
    invisible(FALSE)
  })
  sg1 <- .rv_sig(name)
  moved <- !identical(as.character(sg0), as.character(sg1))
  if (moved)
    cat("  *** The published values this block checks against changed while it ran.\n",
        "      Its rows are marked stale; run the block again.\n", sep = "")
  on_ev <- name %in% c("STATIC", "STAYLEAVE")
  if (length(S$rows) > n0) for (i in (n0 + 1L):length(S$rows)) {
    r <- S$rows[[i]]
    r$written <- t0; r$checked <- t0
    r$build_tag <- if (on_ev) E3_BT else BT
    r$inputs <- as.character(sg0); r$files <- attr(sg0, "files")
    if (identical(r$status, "ok") && moved) r$status <- "stale (inputs changed during the run)"
    if (identical(r$status, "ok") && on_ev && isTRUE(E3_STALE))
      r$status <- paste0("stale (ev.rds is build ", E3_BT, ")")
    S$rows[[i]] <- r
  }
}
.rv_status <- function(ok) if (isTRUE(ok)) "ok" else "not reproduced"
.rv_show <- function(label, here, published, digits = 6, tol = REV_TOL) {
  d <- unname(here - published)
  cat(sprintf("  %-44s here %s  published %s  diff %s\n", label,
              formatC(unname(here), format = "f", digits = digits),
              formatC(unname(published), format = "f", digits = digits),
              if (is.finite(d)) formatC(d, format = "e", digits = 1) else "NA"))
  is.finite(d) && abs(d) <= tol
}

# =============================================================================
# ESTIMATORS
# =============================================================================
clean_num <- function(x) { x <- unclass(x); attributes(x) <- NULL
  if (is.numeric(x)) x else suppressWarnings(as.numeric(as.character(x))) }
# A column read as a number the way 03a and 05 read it: a factor by its
# labels, a logical as 0/1, a labelled column by its values. CS_event_study.R's
# clean_num() above stays for the columns CS converts, so the roster is CS's.
.rv_num <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  x <- unclass(x); attributes(x) <- NULL
  if (is.logical(x) || is.numeric(x)) return(as.numeric(x))
  suppressWarnings(as.numeric(as.character(x)))
}
# 05's rr_to01(): a logical or text column read as 0/1, a factor by its labels.
.rv_01 <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  x <- as.vector(unclass(x))
  if (is.logical(x)) return(as.numeric(x))
  if (is.factor(x) || is.character(x)) {
    x <- tolower(trimws(as.character(x)))
    return(ifelse(is.na(x), NA_real_, as.numeric(x %in% c("1", "true", "yes"))))
  }
  as.numeric(x)
}

# Sun-Abraham, unbinned: every cohort-by-event-time cell enters, the never-
# treated of the same sex are the only control cohort, person and wave
# effects, clustered on the person. vcov = "iid" is for the bootstrap, which
# needs point estimates only.
.rv_sa <- function(d, y, vcov = NULL) {
  f <- stats::as.formula(paste0(y, " ~ sunab(cohort, wave) | id + wave"))
  if (is.null(vcov)) feols(f, data = d, cluster = ~id, notes = FALSE, warn = FALSE)
  else feols(f, data = d, vcov = vcov, notes = FALSE, warn = FALSE)
}
# The post-birth average, equation (4): CS_event_study.R's att_overall().
.rv_att <- function(m) tryCatch({
  a <- summary(m, agg = "att")$coeftable
  unname(a[grep("ATT|att", rownames(a))[1], 1:2]) }, error = function(e) c(NA_real_, NA_real_))
# The aggregated event-time path, point estimates only.
.rv_path <- function(m) {
  b  <- stats::coef(m); nm <- names(b)
  k  <- suppressWarnings(as.numeric(sub(".*::(-?\\d+).*", "\\1", nm)))
  ok <- !is.na(k) & grepl("::", nm)
  data.frame(k = k[ok], b = unname(b[ok]))
}
# CS_event_study.R's covariance helpers, verbatim. coef() on a sunab fit is
# the aggregated path while vcov() is the cohort-by-period matrix, so the
# covariance of the path is rebuilt with the shares fixest aggregates by.
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
  list(k = rt[ok][o], b = unname(b[ok])[o], V = unname(V)[o, o, drop = FALSE])
}
# Women less men on the common horizons, the two arms' covariances summed:
# the late window less the early one, and the joint test of the leads.
.rv_gap_path <- function(vw, vm) {
  k  <- sort(intersect(vw$k, vm$k))
  iw <- match(k, vw$k); im <- match(k, vm$k)
  list(k = k, g = vw$b[iw] - vm$b[im],
       V = vw$V[iw, iw, drop = FALSE] + vm$V[im, im, drop = FALSE])
}
.rv_window <- function(gp) {
  iE <- which(gp$k %in% REV_EARLY); iL <- which(gp$k %in% REV_LATE)
  if (!length(iE) || !length(iL)) return(c(est = NA_real_, se = NA_real_, p = NA_real_))
  ix <- c(iE, iL)
  a  <- c(rep(-1 / length(iE), length(iE)), rep(1 / length(iL), length(iL)))
  est <- sum(a * gp$g[ix])
  s   <- sqrt(max(drop(t(a) %*% gp$V[ix, ix, drop = FALSE] %*% a), 0))
  c(est = est, se = s, p = 2 * stats::pnorm(-abs(est / s)))
}
.rv_leads <- function(gp) {
  i <- which(gp$k %in% REV_LEADS)
  if (!length(i)) return(c(chi2 = NA_real_, df = 0, p = NA_real_))
  st <- tryCatch(drop(t(gp$g[i]) %*% solve(gp$V[i, i, drop = FALSE], gp$g[i])),
                 error = function(e) NA_real_)
  c(chi2 = st, df = length(i), p = stats::pchisq(st, length(i), lower.tail = FALSE))
}
# Late less early from point estimates alone, for the bootstrap. The
# horizons used come back as an attribute, so a replicate that lost one can
# be counted.
.rv_le <- function(pw, pm) {
  k <- intersect(pw$k, pm$k)
  g <- pw$b[match(k, pw$k)] - pm$b[match(k, pm$k)]
  iE <- k %in% REV_EARLY; iL <- k %in% REV_LATE
  if (!any(iE) || !any(iL)) return(NA_real_)
  structure(mean(g[iL]) - mean(g[iE]), horizons = paste(sort(k[iE | iL]), collapse = ","))
}
# v-cw. Late less early on a gap path, for the bootstrap: equal weights over
# the horizons present, as .rv_window() uses.
.rv_le_gap <- function(gp) {
  if (is.null(gp)) return(NA_real_)
  iE <- gp$k %in% REV_EARLY; iL <- gp$k %in% REV_LATE
  if (!any(iE) || !any(iL)) return(NA_real_)
  structure(mean(gp$g[iL]) - mean(gp$g[iE]),
            horizons = paste(sort(gp$k[iE | iL]), collapse = ","))
}

# =============================================================================
# THE FRAME AND THE COUPLES
# =============================================================================
fr <- td_frame(context = "06_revision_inference")
if (is.null(attr(fr, "rows_before_screen"))) {
  cat("  the frame carries no screen stamp; applying td_apply_sample_rule()\n")
  fr <- td_apply_sample_rule(fr)
}
BT <- attr(fr, "build_tag"); BT <- if (is.null(BT)) NA_character_ else as.character(BT)
.need <- c("person_id", "wave", "female", "ever_parent", "employed", "jttpewt_any",
           "jttpeot_any", "ln_hourly_wage_real", "partner_id")
.miss <- setdiff(.need, names(fr))
if (length(.miss)) stop("the frame lacks: ", paste(.miss, collapse = ", "), call. = FALSE)
LV <- intersect(REV_LEAVE, names(fr))[1]
.cols <- c(.need, if (!is.na(LV)) LV, intersect("chance_leave_job", names(fr)))
pn <- data.frame(lapply(stats::setNames(.cols, .cols), function(v) fr[[v]]),
                 stringsAsFactors = FALSE, check.names = FALSE)
# Converted as CS_event_study.R and training_event_study.R convert them, with
# their clean_num(), so the roster is theirs row for row. ever_parent stays as
# it is, because CS reads the cohorts off the raw column. The leave indicator
# and the stated chance of leaving are read later, the way 03a and 05 read them.
for (v in intersect(c("wave", "female", "employed", "jttpewt_any", "jttpeot_any",
                      "ln_hourly_wage_real"), .cols)) pn[[v]] <- clean_num(pn[[v]])
cat(sprintf("Frame: %s person-waves, %s persons, build_tag %s; leave indicator %s\n",
            format(nrow(pn), big.mark = ","), format(n_distinct(pn$person_id), big.mark = ","),
            BT, if (is.na(LV)) "ABSENT" else LV))
rm(fr, .need, .miss, .cols)

# --- the couple clusters: connected components of the partner graph -----------
# Ids are compared as whole-number text with leading zeros dropped, so a
# numeric person_id and a zero-padded partner_id still meet.
.rv_key <- function(x) {
  if (is.factor(x)) x <- as.character(x)   # the labels, not the level codes
  x  <- as.vector(unclass(x))
  na <- is.na(x)
  if (is.numeric(x)) {
    if (any(x[!na] != round(x[!na])))
      stop("an id column holds non-whole numbers; the couple key cannot be built", call. = FALSE)
    x <- sprintf("%.0f", x)
  } else x <- trimws(as.character(x))
  x <- sub("^0+(?=[0-9])", "", x, perl = TRUE)
  x[na] <- NA_character_
  x
}
.pid <- .rv_key(pn$person_id)
.ptn <- .rv_key(pn$partner_id)
.has <- !is.na(.ptn) & nzchar(.ptn) & .ptn != .pid & !grepl("^-", .ptn) & .ptn != "0"
# A partner id shared by many people is a missing-value code, not a person,
# and would chain all of them into one cluster. HILDA's partner id is a
# person's cross-wave id, so no real partner is shared this widely.
.shared <- tapply(.pid[.has], .ptn[.has], function(z) length(unique(z)))
.code   <- names(.shared)[.shared > REV_MAX_SHARED]
if (length(.code)) {
  cat(sprintf(paste0("  *** %d partner id(s) are each the partner of more than %d people (%s).\n",
                     "      They are read as missing-value codes and their links dropped.\n"),
              length(.code), REV_MAX_SHARED,
              paste(sprintf("%s: %d", utils::head(.code, 5), .shared[utils::head(.code, 5)]),
                    collapse = ", ")))
  .has <- .has & !(.ptn %in% .code)
}
.edges <- unique(data.frame(a = pmin(.pid[.has], .ptn[.has]),
                            b = pmax(.pid[.has], .ptn[.has]), stringsAsFactors = FALSE))
.nodes <- unique(c(.pid, .edges$a, .edges$b))
.ia <- match(.edges$a, .nodes); .ib <- match(.edges$b, .nodes)
.par <- seq_along(.nodes)
for (r in seq_along(.ia)) {                   # union-find; the smaller index is the root
  x <- .ia[r]; while (.par[x] != x) x <- .par[x]
  y <- .ib[r]; while (.par[y] != y) y <- .par[y]
  if (x != y) { if (x < y) .par[y] <- x else .par[x] <- y }
}
.root <- vapply(seq_along(.nodes), function(i) { while (.par[i] != i) i <- .par[i]; i },
                integer(1))
CLUSTER <- stats::setNames(.nodes[.root], .nodes)
.persons <- unique(.pid)
.cl_p <- CLUSTER[.persons]
.size <- table(.cl_p)                        # persons on the frame per cluster
.nodes_per <- table(CLUSTER)                 # including partners off the frame
cat(sprintf(paste0("Couples: %s partner links between %s persons; %s partner ids are not on ",
                   "the frame.\n  %s persons fall into %s clusters: %s alone, %s in pairs, ",
                   "%s in clusters of three or more (largest %d).\n"),
            format(nrow(.edges), big.mark = ","),
            format(length(unique(c(.edges$a, .edges$b))), big.mark = ","),
            format(sum(!unique(c(.edges$a, .edges$b)) %in% .persons), big.mark = ","),
            format(length(.persons), big.mark = ","), format(length(.size), big.mark = ","),
            format(sum(.size == 1), big.mark = ","), format(2 * sum(.size == 2), big.mark = ","),
            format(sum(.size[.size >= 3]), big.mark = ","), max(.size)))
.off <- mean(!unique(.ptn[.has]) %in% .persons)
if (is.finite(.off) && .off > 0.5)
  cat(sprintf(paste0("  *** %.0f%% of partner ids match no person on the frame. Partners outside\n",
                     "      ages 20-55 explain some; most suggests the two id columns differ in\n",
                     "      format. Compare a few values of person_id and partner_id.\n"), 100 * .off))
# Against referee8's key, the first partner observed: the two agree for a
# person whose cluster holds at most that one partner.
.multi <- names(.nodes_per)[.nodes_per >= 3]
cat(sprintf(paste0("  %s persons sit in a cluster with three or more members, counting ",
                   "partners off the frame.\n  For them the first-partner key of referee8 ",
                   "is narrower than the cluster used here.\n"),
            format(sum(.cl_p %in% .multi), big.mark = ",")))
if (max(.size) > 20L)
  cat("  *** The largest cluster holds ", max(.size), " people. Serial partnering rarely\n",
      "      chains that many; check partner_id for a reused id before trusting the\n",
      "      couple-robust standard errors. revision_couple_clusters.csv lists them.\n", sep = "")
.ids <- pn$person_id[!duplicated(.pid)]
.cl_o <- unname(CLUSTER[.rv_key(.ids)])
utils::write.csv(data.frame(person_id = as.character(.ids), couple = .cl_o,
                            persons_in_cluster = as.integer(.size[.cl_o]),
                            stringsAsFactors = FALSE),
                 OUT_CLUSTERS, row.names = FALSE)
cat("  wrote ", OUT_CLUSTERS, "\n", sep = "")
# A checksum of the clusters, as paper_tables.R's .content_tag() computes one,
# so the blocks that use them can tell when the clusters change.
CL_SIG <- local({
  z <- utf8ToInt(paste(sort(paste(.persons, .cl_p)), collapse = "\n"))
  sprintf("%d/%08x", length(unique(.cl_p)),
          as.integer(sum(as.numeric(z) * seq_along(z)) %% 2^31))
})
# The stated chance of leaving, kept for 03a's frame if that lacks it.
CHANCE <- if ("chance_leave_job" %in% names(pn))
  data.frame(key = paste(.pid, pn$wave), chance_leave_job = pn$chance_leave_job,
             stringsAsFactors = FALSE) else NULL
rm(.pid, .ptn, .has, .shared, .code, .edges, .nodes, .ia, .ib, .par, .root, .persons, .cl_p,
   .size, .nodes_per, .multi, .off, .ids, .cl_o)
if (exists("x")) rm(x)
if (exists("y")) rm(y)
if (exists("r")) rm(r)

# --- the Sun-Abraham roster: CS_event_study.R's cohort rule ---------------------
# The first 0-to-1 step in ever_parent is the birth wave; never a parent is
# cohort 10000; a parent when first seen is left-censored and dropped.
g_tbl <- pn %>% group_by(person_id) %>% arrange(wave) %>%
  summarise(ep_first = first(ever_parent), ep_last = last(ever_parent),
            trans = { d_ep <- diff(ever_parent)
            idx  <- which(!is.na(d_ep) & d_ep > 0)
            if (length(idx)) wave[idx[1] + 1] else NA_integer_ },
            .groups = "drop") %>%
  mutate(cohort = case_when(
    !is.na(trans)                ~ as.double(trans),
    ep_first == 0 & ep_last == 0 ~ 10000,
    TRUE                         ~ NA_real_))
ev <- pn %>% inner_join(select(g_tbl, person_id, cohort), by = "person_id") %>%
  filter(!is.na(cohort)) %>% as.data.frame()
rm(g_tbl, pn)
ev$id      <- as.integer(factor(ev$person_id))
ev$wave    <- as.double(ev$wave)
ev$treated <- as.integer(ev$cohort < 10000)
ev$k       <- ifelse(ev$treated == 1, ev$wave - ev$cohort, NA_real_)
ev$post    <- as.integer(ev$treated == 1 & !is.na(ev$k) & ev$k >= 0)
ev$post_f  <- ev$post * ev$female
ev$couple  <- unname(CLUSTER[.rv_key(ev$person_id)])
if (anyNA(ev$couple)) stop("some roster persons have no couple cluster")
ev$attached <- if (is.na(LV)) as.integer(ev$employed %in% 1) else {
  .lvv <- .rv_num(ev[[LV]]); as.integer(ev$employed %in% 1 | (!is.na(.lvv) & .lvv == 1)) }
if (exists(".lvv")) rm(.lvv)
# The headline's frame, as CS_event_study.R builds it, and the financing
# contrast within each employed person-wave.
ev$jttpewt_emp <- ifelse(ev$employed %in% 1, ev$jttpewt_any, NA_real_)
ev$jttpeot_emp <- ifelse(ev$employed %in% 1, ev$jttpeot_any, NA_real_)
.both <- ev$employed %in% 1 & !is.na(ev$jttpewt_any) & !is.na(ev$jttpeot_any)
ev$fin_d <- ifelse(.both, ev$jttpewt_any - ev$jttpeot_any, NA_real_)
ev$fin_e <- ifelse(.both, ev$jttpewt_any, NA_real_)
ev$fin_o <- ifelse(.both, ev$jttpeot_any, NA_real_)
rm(.both)
cat(sprintf("Roster: %s persons (%s treated, %s never-treated), %s person-waves, %s couple clusters\n",
            format(n_distinct(ev$person_id), big.mark = ","),
            format(n_distinct(ev$person_id[ev$treated == 1]), big.mark = ","),
            format(n_distinct(ev$person_id[ev$treated == 0]), big.mark = ","),
            format(nrow(ev), big.mark = ","), format(n_distinct(ev$couple), big.mark = ",")))
# The overlap the couple corrections are about: employed person-waves in
# clusters that hold a mother and a father.
.par_cl <- ev %>% filter(treated == 1) %>% group_by(couple) %>%
  summarise(m = any(female == 1), f = any(female == 0), .groups = "drop") %>%
  filter(m, f) %>% pull(couple)
.emp <- ev$employed %in% 1
cat(sprintf(paste0("  %s clusters hold both a mother and a father; they carry %.1f%% of ",
                   "employed person-waves\n  and %.1f%% of treated employed person-waves.\n"),
            format(length(.par_cl), big.mark = ","),
            100 * mean(ev$couple[.emp] %in% .par_cl),
            100 * mean(ev$couple[.emp & ev$treated == 1] %in% .par_cl)))
rm(.par_cl, .emp)

# --- 03a's event frame, for the static designs and the attachment groups -------
# handoff/ev.rds is the frame 03a fitted T1 on, 03b stacked for T3 and 05 split
# for R11, so the blocks that reproduce those estimates read it. Its cohort
# rule is 03_common.R's, which agrees with CS's when ever_parent has no gaps.
E3 <- if (exists("td_has", mode = "function") && isTRUE(td_has("ev")))
  tryCatch(td_get("ev"), error = function(e) NULL) else NULL
# ITS OWN BUILD. ev.rds can predate the frame: a rebuilt panel and a rerun of
# CS leave it stale until 03a runs again. Its rows carry its tag, as 05's do,
# so they cannot pass as the current build.
E3_BT <- if (!is.null(E3)) attr(E3, "build_tag") else NULL
if (!is.null(E3)) E3 <- as.data.frame(E3)
.n3 <- c("person_id", "wave", "female", "employed", "post", "treated", "k",
         "jttpewt_any", "jttpeot_any")
if (!is.null(E3) && !all(.n3 %in% names(E3))) {
  cat("  handoff/ev.rds lacks ", paste(setdiff(.n3, names(E3)), collapse = ", "),
      "; the static blocks use the Sun-Abraham roster instead.\n", sep = "")
  E3 <- NULL
}
if (is.null(E3)) {
  cat("  *** 03a's event frame is not in the handoff, so the static designs and the\n",
      "      attachment groups use the Sun-Abraham roster. Their reproduction checks\n",
      "      say whether that matches what 03a and 05 fitted.\n", sep = "")
  E3 <- ev
  E3_SRC <- "the Sun-Abraham roster"
  E3_BT  <- BT
} else {
  for (v in c("female", "employed", "post", "treated")) E3[[v]] <- .rv_01(E3[[v]])
  for (v in c("wave", "k", "jttpewt_any", "jttpeot_any")) E3[[v]] <- .rv_num(E3[[v]])
  E3$post_f <- E3$post * E3$female
  .k3 <- .rv_key(E3$person_id)
  E3$couple <- unname(CLUSTER[.k3])
  if (anyNA(E3$couple)) {
    cat("  *** ", n_distinct(E3$person_id[is.na(E3$couple)]), " persons in handoff/ev.rds are not on ",
        "the frame; each is its own cluster.\n      ev.rds may predate the panel.\n", sep = "")
    E3$couple[is.na(E3$couple)] <- .k3[is.na(E3$couple)]
  }
  .lv3 <- intersect(REV_LEAVE, names(E3))[1]
  E3$attached <- if (is.na(.lv3)) as.integer(E3$employed %in% 1) else {
    .l <- .rv_num(E3[[.lv3]]); as.integer(E3$employed %in% 1 | (!is.na(.l) & .l == 1)) }
  if (!"chance_leave_job" %in% names(E3) && !is.null(CHANCE))
    E3$chance_leave_job <- CHANCE$chance_leave_job[match(paste(.k3, E3$wave), CHANCE$key)]
  # As 05 reads it: as.numeric() on the column, then divided by 100 in R11.
  if ("chance_leave_job" %in% names(E3))
    E3$chance_leave_job <- suppressWarnings(as.numeric(E3$chance_leave_job))
  E3_SRC <- "03a's event frame (handoff/ev.rds)"
  if (is.null(E3_BT)) {
    cat("  handoff/ev.rds carries no build tag, so it is taken to be the frame's, as 05 takes it.\n")
    E3_BT <- BT
  } else E3_BT <- as.character(E3_BT)
  cat(sprintf("03a's event frame: %s person-waves, %s persons; leave indicator %s\n",
              format(nrow(E3), big.mark = ","), format(n_distinct(E3$person_id), big.mark = ","),
              if (is.na(.lv3)) "ABSENT" else .lv3))
  rm(.k3, .lv3)
  if (exists(".l")) rm(.l)
}
E3_STALE <- !is.na(BT) && !is.na(E3_BT) && !identical(E3_BT, BT)
if (E3_STALE)
  cat("  *** handoff/ev.rds is from build ", E3_BT, " and the frame is build ", BT, ".\n",
      "      The static designs and the attachment groups would mix the two, so their\n",
      "      rows are marked stale. Rerun 03a, 03b and 05, then this script.\n", sep = "")
rm(.n3, CHANCE)

# =============================================================================
# REPRO_SA. THE PUBLISHED SUN-ABRAHAM HEADLINE
# =============================================================================
R$repro_sa <- FALSE; R$repro_wage <- FALSE
.rv_block("REPRO_SA", "the published Sun-Abraham headline", {
  # The roster first: CS_event_study.R records its own.
  man <- .rv_rds("cs_event_study_sample_manifest.rds")
  ros_ok <- NA
  if (!is.null(man)) {
    here <- c(persons = n_distinct(ev$person_id),
              treated = n_distinct(ev$person_id[ev$treated == 1]),
              never   = n_distinct(ev$person_id[ev$treated == 0]), rows = nrow(ev))
    pub  <- c(persons = man$n_persons_full, treated = man$n_treated,
              never = man$n_never_treated, rows = man$n_obs_full)
    ros_ok <- identical(as.numeric(here), as.numeric(pub))
    cat("  roster against CS's manifest: ", if (ros_ok) "the same" else "DIFFERENT", "\n", sep = "")
    if (!ros_ok) print(rbind(here = here, published = pub))
  }
  R$ros_ok <- ros_ok
  fe_ <- list()
  for (sx in c("women", "men"))
    fe_[[sx]] <- .rv_sa(ev[ev$female == (sx == "women"), , drop = FALSE], "jttpewt_emp")
  aw <- .rv_att(fe_$women); am <- .rv_att(fe_$men)
  # v-cw. The gap on common weights; the separate-weight gap is kept beside
  # it and checked against the gap_separate row below.
  cg_ <- sa_gap_fits(fe_$women, fe_$men)
  R$sa <- list(att_w = aw[1], se_w = aw[2], att_m = am[1], se_m = am[2],
               gap = cg_$post[["est"]], se = cg_$post[["se"]],
               gap_sep = aw[1] - am[1], se_sep = sqrt(aw[2]^2 + am[2]^2),
               composition = cg_$composition,
               n = stats::nobs(fe_$women) + stats::nobs(fe_$men))
  rm(cg_)
  pub <- .rv_csv("cs_post_period_att.csv")
  pa <- function(sex, col) {
    if (is.null(pub)) return(NA_real_)
    v <- pub[[col]][pub$sex == sex & pub$outcome == "jttpewt_emp"]
    if (length(v)) v[1] else NA_real_
  }
  cat("\n  Sun-Abraham post-birth average, employed only (jttpewt_emp):\n")
  ok <- c(.rv_show("women, ATT", aw[1], pa("women", "att")),
          .rv_show("women, SE", aw[2], pa("women", "se")),
          .rv_show("men, ATT", am[1], pa("men", "att")),
          .rv_show("men, SE", am[2], pa("men", "se")),
          .rv_show("women less men", R$sa$gap, pa("gap", "att")),
          .rv_show("its SE, variances summed", R$sa$se, pa("gap", "se")),
          .rv_show("separate weights, women less men", R$sa$gap_sep, pa("gap_separate", "att")))
  R$repro_sa <- all(ok) && !isFALSE(ros_ok)
  cat("  ", if (R$repro_sa) "REPRODUCED" else "*** NOT REPRODUCED", ": the headline gap",
      if (R$repro_sa) "" else ", so nothing built on it is marked ok", "\n", sep = "")
  # Women's own pre-birth base, the denominator of \SATrainingPostPct.
  bf <- mean(ev$jttpewt_any[ev$treated == 1 & ev$employed %in% 1 & ev$female == 1 &
                              ev$k %in% REV_PRE & !is.na(ev$jttpewt_any)])
  # The base paper_tables.R divides by, found the way it finds it: the key
  # ledger.base_f in the registry files, else 03_common.R's
  # T0_pre_birth_base_rates.csv. A derived key is computed only when the
  # macro file is written, so it is not in the registry files, and then the
  # csv is what paper_tables.R uses.
  bf_reg <- .rv_registered("ledger.base_f"); bf_src <- "the registry (ledger.base_f)"
  if (!is.finite(bf_reg)) {
    t0 <- .rv_csv("T0_pre_birth_base_rates.csv", quiet = TRUE)
    bf_reg <- if (!is.null(t0) && all(c("female", "base_pewt") %in% names(t0)))
      suppressWarnings(as.numeric(t0$base_pewt[t0$female == 1][1])) else NA_real_
    bf_src <- "T0_pre_birth_base_rates.csv"
  }
  cat(sprintf(paste0("\n  women's pre-birth base: %.4f here, %.4f in %s\n",
                     "  paper_tables.R will print %.1f points and %.0f percent of that base.\n"),
              bf, bf_reg, bf_src, 100 * abs(R$sa$gap), 100 * abs(R$sa$gap) / bf_reg))
  .rv_add("sa_gap_att", R$sa$gap, "REPRO_SA",
          basis = "Sun-Abraham ATT over k >= 0, women less men, employed only")
  .rv_add("sa_gap_se_summed", R$sa$se, "REPRO_SA", basis = "its SE, the two arms' variances summed")
  .rv_add("sa_att_women", aw[1], "REPRO_SA", basis = "women's arm")
  .rv_add("sa_att_men", am[1], "REPRO_SA", basis = "men's arm")
  .rv_add("sa_n", R$sa$n, "REPRO_SA", "SATrainingPostN", .rv_status(R$repro_sa),
          "person-waves in the two arms of the headline, for Table 2")
  .rv_add("base_f_here", bf, "REPRO_SA", basis = "women's employer-financed rate, k = -3..-1, employed")
  .rv_add("base_f_registry", bf_reg, "REPRO_SA", basis = paste0("the base paper_tables.R uses: ", bf_src))
})

# =============================================================================
# REPRO_WAGE. THE PUBLISHED WAGE WINDOW
# =============================================================================
.rv_block("REPRO_WAGE", "the published wage window", {
  fw_ <- list()
  for (sx in c("women", "men"))
    fw_[[sx]] <- .rv_sa(ev[ev$female == (sx == "women"), , drop = FALSE], "ln_hourly_wage_real")
  # v-cw. The wage gap path on common weights, with its covariance.
  cg_ <- sa_gap_fits(fw_$women, fw_$men)
  gp  <- list(k = cg_$k, g = cg_$g, V = cg_$V)
  win <- .rv_window(gp); lds <- .rv_leads(gp)
  le  <- .rv_le_gap(gp)
  rm(cg_)
  if (abs(le - win["est"]) > 1e-10)
    stop("the bootstrap's window arithmetic disagrees with the covariance version")
  R$wage <- list(est = unname(win["est"]), se = unname(win["se"]),
                 horizons = attr(le, "horizons"))
  wpt <- .rv_csv("wage_path_tests.csv")
  pw <- function(q, col) {
    if (is.null(wpt)) return(NA_real_)
    v <- wpt[[col]][wpt$path == "plain" & wpt$quantity == q]
    if (length(v)) v[1] else NA_real_
  }
  cat("  Wage gap, years 5-10 less years 0-4 (plain path):\n")
  okw <- c(.rv_show("late less early", win["est"], pw("late window minus early window", "estimate")),
           .rv_show("its SE, variances summed", win["se"], pw("late window minus early window", "se")),
           .rv_show("lead test, chi-squared", lds["chi2"], pw("joint test, all leads zero", "stat"),
                    4, tol = 1e-3 * max(1, abs(lds["chi2"]))))
  R$repro_wage <- all(okw) && !isFALSE(R$ros_ok)
  cat("  ", if (R$repro_wage) "REPRODUCED" else "*** NOT REPRODUCED", ": the wage window\n", sep = "")
  .rv_add("wage_window_est", win["est"], "REPRO_WAGE", basis = "wage gap, years 5-10 less years 0-4")
  .rv_add("wage_window_se_summed", win["se"], "REPRO_WAGE",
          basis = "its SE, the two arms' covariances summed")
})

# =============================================================================
# STATIC. THE STATIC DESIGNS, CLUSTERED ON COUPLES
# =============================================================================
if (!rv_run("STATIC")) cat("\nSTATIC: not selected in REV_BLOCKS\n") else
.rv_block("STATIC", "static DiD and triple difference, clustered on couples", {
  cat("  frame: ", E3_SRC, "\n", sep = "")
  sealed <- .rv_rds("BLIND_training_estimates.rds")
  led <- .rv_csv("training_ledger.csv")
  pub <- function(tag) {
    est <- se <- n <- NA_real_
    m <- if (!is.null(sealed)) sealed[[tag]] else NULL
    if (is.matrix(m)) {
      j1 <- grep("^Estimate", colnames(m))[1]; j2 <- grep("Std\\. *Error", colnames(m))[1]
      if (!is.na(j1)) est <- unname(m[1, j1])
      if (!is.na(j2)) se  <- unname(m[1, j2])
    }
    if (!is.null(led) && all(c("tag", "n") %in% names(led))) {
      v <- led$n[led$tag == tag]
      if (length(v)) n <- suppressWarnings(as.numeric(v[1]))
    }
    c(est = est, se = se, n = n)
  }
  # T1_attached: 03a's T1c arm, employed or on parental leave.
  d1 <- E3[E3$attached %in% 1 & !is.na(E3$jttpewt_any), , drop = FALSE]
  f1 <- stats::as.formula(paste0("jttpewt_any ~ post + post_f | ", REV_FE))
  m1p <- feols(f1, data = d1, cluster = ~person_id, notes = FALSE, warn = FALSE)
  m1c <- feols(f1, data = d1, cluster = ~couple, notes = FALSE, warn = FALSE)
  p1 <- pub("T1_attached")
  cat("  T1_attached, employer-financed training, employed or on leave:\n")
  ok1 <- c(.rv_show("estimate", coef(m1p)[["post_f"]], p1[["est"]]),
           .rv_show("SE, clustered on persons", fixest::se(m1p)[["post_f"]], p1[["se"]]),
           .rv_show("n", stats::nobs(m1p), p1[["n"]], 0))
  cat("  ", if (all(ok1)) "REPRODUCED" else "*** NOT REPRODUCED", ": T1_attached\n", sep = "")
  cat(sprintf("  SE clustered on couples %.4f (%s clusters) against %.4f on persons, ratio %.3f\n",
              fixest::se(m1c)[["post_f"]], format(n_distinct(d1$couple), big.mark = ","),
              fixest::se(m1p)[["post_f"]], fixest::se(m1c)[["post_f"]] / fixest::se(m1p)[["post_f"]]))
  .rv_add("did_attached_est", coef(m1p)[["post_f"]], "STATIC", basis = "T1_attached, post_f")
  .rv_add("did_attached_se_person", fixest::se(m1p)[["post_f"]], "STATIC", basis = "clustered on persons")
  .rv_add("did_attached_se_couple", fixest::se(m1c)[["post_f"]], "STATIC", "DiDSECouple",
          .rv_status(all(ok1)), "T1_attached, clustered on couple clusters")
  .rv_add("did_attached_couple_clusters", n_distinct(d1$couple), "STATIC", basis = "clusters in the sample")

  # T3_ddd: 03b's stack, with the sex-specific source-by-wave path.
  base <- E3[, c("person_id", "wave", "female", "employed", "post", "couple")]
  long <- rbind(data.frame(base, source = "employer", y = E3$jttpewt_any, stringsAsFactors = FALSE),
                data.frame(base, source = "own", y = E3$jttpeot_any, stringsAsFactors = FALSE))
  long <- long[long$employed %in% 1 & !is.na(long$y), , drop = FALSE]
  long$employer <- as.integer(long$source == "employer")
  long$post_f   <- long$post * long$female
  long$post_e   <- long$post * long$employer
  long$post_f_e <- long$post * long$female * long$employer
  f3 <- y ~ post + post_f + post_e + post_f_e | person_id^source + wave^source^female
  m3p <- feols(f3, data = long, cluster = ~person_id, notes = FALSE, warn = FALSE)
  m3c <- feols(f3, data = long, cluster = ~couple, notes = FALSE, warn = FALSE)
  p3 <- pub("T3_ddd")
  cat("\n  T3_ddd, triple difference with sex-specific source-by-wave effects:\n")
  ok3 <- c(.rv_show("estimate", coef(m3p)[["post_f_e"]], p3[["est"]]),
           .rv_show("SE, clustered on persons", fixest::se(m3p)[["post_f_e"]], p3[["se"]]),
           .rv_show("n", stats::nobs(m3p), p3[["n"]], 0))
  cat("  ", if (all(ok3)) "REPRODUCED" else "*** NOT REPRODUCED", ": T3_ddd\n", sep = "")
  cat(sprintf("  SE clustered on couples %.4f against %.4f on persons, ratio %.3f\n",
              fixest::se(m3c)[["post_f_e"]], fixest::se(m3p)[["post_f_e"]],
              fixest::se(m3c)[["post_f_e"]] / fixest::se(m3p)[["post_f_e"]]))
  .rv_add("ddd_est", coef(m3p)[["post_f_e"]], "STATIC", basis = "T3_ddd, post_f_e")
  .rv_add("ddd_se_person", fixest::se(m3p)[["post_f_e"]], "STATIC", basis = "clustered on persons")
  .rv_add("ddd_se_couple", fixest::se(m3c)[["post_f_e"]], "STATIC", "DddSECouple",
          .rv_status(all(ok3)), "T3_ddd, clustered on couple clusters")
})

# =============================================================================
# SADDD. THE FINANCING CONTRAST AS AN EVENT STUDY
# =============================================================================
# Equation (3) by sex on employer-financed less own-time training within each
# employed person-wave, averaged over k >= 0 with equation (4)'s weights,
# women less men. It needs both items, so it uses the employed person-waves
# on which both are observed. Its two inputs are checked on their own rows:
# the headline above, and the own-time average against training_event_study.R.
# When all three use the same rows, the estimator's linearity makes the
# contrast exactly the headline less the own-time average, and the log shows
# that sum. When own time is missing on some person-waves the headline keeps,
# the rows differ: the contrast is still the estimand Section 7.1 describes,
# but it no longer equals that difference exactly, and the log says by how
# much, so the text can say which rows it uses.
if (!rv_run("SADDD")) cat("\nSADDD: not selected in REV_BLOCKS\n") else
.rv_block("SADDD", "the financing contrast as an event study", {
  m <- list()
  for (sx in c("women", "men")) {
    d <- ev[ev$female == (sx == "women"), , drop = FALSE]
    for (y in c("fin_d", "fin_e", "fin_o", "jttpeot_emp")) m[[paste(sx, y)]] <- .rv_sa(d, y)
  }
  # v-cw. Each gap on common weights. a keeps each arm on its own weights,
  # for the linearity check.
  a  <- lapply(m, .rv_att)
  cc <- lapply(m, sa_cells)
  cg <- lapply(stats::setNames(nm = c("fin_d", "fin_e", "fin_o", "jttpeot_emp")),
               function(y) sa_common_gap(cc[[paste("women", y)]], cc[[paste("men", y)]]))
  g  <- function(y) cg[[y]]$post[["est"]]
  nn <- function(y) stats::nobs(m[[paste("women", y)]]) + stats::nobs(m[[paste("men", y)]])
  dd <- g("fin_d")
  dd_se <- cg$fin_d$post[["se"]]
  ident <- max(abs(c(a[["women fin_d"]][1] - (a[["women fin_e"]][1] - a[["women fin_o"]][1]),
                     a[["men fin_d"]][1] - (a[["men fin_e"]][1] - a[["men fin_o"]][1]))))
  n_dd <- nn("fin_d"); n_o <- nn("jttpeot_emp")
  cat(sprintf("  women less men, employer-financed less own time: %+.4f (SE %.4f), %s person-waves\n",
              dd, dd_se, format(n_dd, big.mark = ",")))
  cat(sprintf("  linearity: |D - (E - O)| at most %.1e across the two arms\n", ident))
  # The own-time input, on its own rows.
  tp <- .rv_csv("training_post_att.csv")
  own_pub <- if (!is.null(tp)) tp$att[tp$frame == "employed_only" & tp$sex == "gap" &
                                        tp$outcome == "jttpeot_any"][1] else NA_real_
  cat("  own-time average on its own rows, against training_event_study.R:\n")
  ok_o <- .rv_show("own time, women less men", g("jttpeot_emp"), own_pub)
  # The rows. The contrast's rows lie within both inputs' rows, so equal
  # counts mean the same rows.
  rows_same <- !is.null(R$sa) && n_dd == R$sa$n && n_dd == n_o
  cat(sprintf("  person-waves: %s in the contrast, %s in the headline, %s in the own-time average\n",
              format(n_dd, big.mark = ","),
              if (is.null(R$sa)) "NA" else format(R$sa$n, big.mark = ","), format(n_o, big.mark = ",")))
  ok_e <- NA
  if (rows_same && isTRUE(R$repro_sa)) {
    ok_e <- .rv_show("employer-financed part against the headline", g("fin_e"), R$sa$gap)
    cat(sprintf("  the same rows, so the contrast is the headline less the own-time average: %+.4f - (%+.4f) = %+.4f\n",
                R$sa$gap, g("jttpeot_emp"), R$sa$gap - g("jttpeot_emp")))
  } else if (!is.null(R$sa)) {
    cat(sprintf(paste0("  *** NOT THE SAME ROWS. On the contrast's rows the parts are %+.4f and %+.4f, against\n",
                       "      %+.4f and %+.4f on their own rows, so the contrast %+.4f differs from the headline\n",
                       "      less the own-time average, %+.4f, by %+.4f. If Section 7.1 sets them side by side,\n",
                       "      say the contrast uses the %s employed person-waves on which both items are observed.\n"),
                g("fin_e"), g("fin_o"), R$sa$gap, g("jttpeot_emp"), dd, R$sa$gap - g("jttpeot_emp"),
                dd - (R$sa$gap - g("jttpeot_emp")), format(n_dd, big.mark = ",")))
  }
  if (!isTRUE(R$repro_sa))
    cat("    *** The contrast is withheld, because the headline was not reproduced above.\n")
  else if (!isTRUE(ok_o))
    cat("    *** The contrast is withheld, because the own-time average was not reproduced:\n",
        "        training_event_study.R may have estimated on another frame.\n", sep = "")
  # The path and the lead test.
  vw <- .event_vcov(m[["women fin_d"]]); vm <- .event_vcov(m[["men fin_d"]])
  if (is.null(vw) || is.null(vm)) stop("the contrast's covariance could not be rebuilt")
  gp <- list(k = cg$fin_d$k, g = cg$fin_d$g, V = cg$fin_d$V)
  lds <- .rv_leads(gp)
  cat(sprintf("  joint test that the gaps at k = %d..%d are zero: chi-squared(%d) = %.2f, p = %.3f\n",
              min(REV_LEADS), max(REV_LEADS), as.integer(lds["df"]), lds["chi2"], lds["p"]))
  if (lds["df"] != length(REV_LEADS))
    cat("  *** only ", lds["df"], " of the ", length(REV_LEADS), " leads are estimated.\n", sep = "")
  sw <- sqrt(diag(vw$V)); sm <- sqrt(diag(vm$V))
  path <- data.frame(event_time = gp$k,
                     women = vw$b[match(gp$k, vw$k)], women_se = sw[match(gp$k, vw$k)],
                     men = vm$b[match(gp$k, vm$k)], men_se = sm[match(gp$k, vm$k)],
                     gap = gp$g, gap_se = sqrt(diag(gp$V)))
  utils::write.csv(path, OUT_PATH, row.names = FALSE)
  cat("  wrote ", OUT_PATH, "\n", sep = "")
  st <- .rv_status(isTRUE(R$repro_sa) && isTRUE(ok_o) && ident < 1e-8 && !isFALSE(ok_e))
  cat("  ", if (st == "ok") "REPRODUCED" else "*** NOT REPRODUCED",
      ": the headline and the own-time average the contrast is set beside\n", sep = "")
  .rv_add("sa_dd_att", dd, "SADDD", "SADddATT", st,
          paste0("Sun-Abraham ATT over k >= 0, employer-financed less own time, women less men",
                 if (rows_same) "" else sprintf("; on the %d person-waves with both items", n_dd)))
  .rv_add("sa_dd_se", dd_se, "SADDD", "SADddSE", st, "its SE, the two arms' variances summed")
  # The text names the leads from -8 to -2, so a test over fewer is not that test.
  st_p <- if (st == "ok" && lds["df"] != length(REV_LEADS))
    sprintf("only %d of %d leads", as.integer(lds["df"]), length(REV_LEADS)) else st
  .rv_add("sa_dd_leads_p", lds["p"], "SADDD", "SADddPreP", st_p,
          sprintf("joint test of the gaps at k = %d..%d", min(REV_LEADS), max(REV_LEADS)))
  .rv_add("sa_dd_leads_chi2", lds["chi2"], "SADDD", basis = "its chi-squared")
  .rv_add("sa_dd_leads_df", lds["df"], "SADDD", basis = "its degrees of freedom")
  .rv_add("sa_dd_n", n_dd, "SADDD", basis = "person-waves in the two arms")
  .rv_add("sa_dd_rows_same", as.numeric(rows_same), "SADDD",
          basis = "1 if the contrast, the headline and the own-time average use the same rows")
  .rv_add("sa_dd_emp_part", g("fin_e"), "SADDD", basis = "employer-financed average on the contrast's rows")
  .rv_add("sa_dd_own_part", g("fin_o"), "SADDD", basis = "own-time average on the contrast's rows")
  .rv_add("sa_own_att", g("jttpeot_emp"), "SADDD", basis = "own-time average on its own rows")
  .rv_add("sa_own_n", n_o, "SADDD", basis = "its person-waves")
  .rv_add("sa_dd_own_published", own_pub, "SADDD",
          basis = "training_post_att.csv, employed_only, gap, jttpeot_any")
  .rv_add("sa_dd_linearity", ident, "SADDD", basis = "largest |D - (E - O)| across the two arms")
})

# =============================================================================
# STAYLEAVE. STATED ATTACHMENT: STAY LESS LEAVE IN ONE MODEL
# =============================================================================
# 05's R11 (b) groups: mothers split at the median of their average stated
# chance of leaving the job over k = -3..-1, at or below it "expect to stay",
# above it "expect to leave". From year 2: the birth wave and the next are
# dropped for every parent, as 05's k>=2 window does. Employer-financed
# training on employed person-waves. Each group's change is divided by its
# own pre-birth rate, and the two proportional changes are differenced from
# one model in which both groups of mothers, all fathers and the never-
# treated share the wave effects. The bases are treated as known, as in
# Table I.8.
if (!rv_run("STAYLEAVE")) cat("\nSTAYLEAVE: not selected in REV_BLOCKS\n") else
.rv_block("STAYLEAVE", "stay less leave, proportional change from year 2, one model", {
  cat("  frame: ", E3_SRC, "\n", sep = "")
  if (!"chance_leave_job" %in% names(E3))
    stop("chance_leave_job is on neither 03a's frame nor td_frame()")
  d <- E3[E3$employed %in% 1 & !is.na(E3$jttpewt_any), , drop = FALSE]
  d$E1 <- d$jttpewt_any
  d$cl <- d$chance_leave_job / 100
  pre <- d[d$treated == 1 & d$female == 1 & !is.na(d$k) & d$k %in% REV_PRE & !is.na(d$cl), ]
  pm  <- stats::aggregate(cl ~ person_id, data = pre, FUN = mean)
  thr <- stats::median(pm$cl)
  stay  <- pm$person_id[pm$cl <= thr]
  leave <- pm$person_id[pm$cl >  thr]
  cat(sprintf("  split at the median pre-birth chance of leaving, %.2f: %d expect to stay, %d to leave\n",
              thr, length(stay), length(leave)))
  db <- d[!(d$treated == 1 & d$female == 1 & !(d$person_id %in% c(stay, leave))), , drop = FALSE]
  from2 <- function(x) x[!(x$treated == 1 & !is.na(x$k) & x$k %in% c(0, 1)), , drop = FALSE]
  base_of <- function(x) mean(x$E1[x$treated == 1 & x$female == 1 & !is.na(x$k) & x$k %in% REV_PRE])
  f1 <- stats::as.formula(paste0("E1 ~ post + post_f | ", REV_FE))
  # Each group on its own, as Table I.8 reports it.
  sep <- list()
  for (gname in c("stay", "leave")) {
    other <- if (gname == "stay") leave else stay
    dg <- db[!(db$treated == 1 & db$female == 1 & db$person_id %in% other), , drop = FALSE]
    mg <- feols(f1, data = from2(dg), cluster = ~person_id, notes = FALSE, warn = FALSE)
    b0 <- base_of(dg)
    sep[[gname]] <- c(est = coef(mg)[["post_f"]], se = fixest::se(mg)[["post_f"]],
                      n = stats::nobs(mg), base = b0, prop = coef(mg)[["post_f"]] / b0)
  }
  pub <- .rv_csv("R11_exit_by_group.csv")
  pg <- function(lab, col) {
    if (is.null(pub)) return(NA_real_)
    v <- pub[[col]][pub$label == lab & pub$window == "k>=2" & pub$outcome == "E1"]
    if (length(v)) v[1] else NA_real_
  }
  cat("  each group on its own, against R11_exit_by_group.csv:\n")
  ok <- c()
  for (gname in c("stay", "leave")) {
    lab <- paste("expect to", gname)
    ok <- c(ok,
            .rv_show(paste0(lab, ", estimate"), sep[[gname]][["est"]], pg(lab, "est")),
            .rv_show(paste0(lab, ", SE"), sep[[gname]][["se"]], pg(lab, "se")),
            .rv_show(paste0(lab, ", n"), sep[[gname]][["n"]], pg(lab, "n"), 0),
            .rv_show(paste0(lab, ", pre-birth base"), sep[[gname]][["base"]], pg(lab, "group_base")))
  }
  # One model.
  dj <- from2(db)
  dj$post_f_stay  <- dj$post_f * as.numeric(dj$person_id %in% stay)
  dj$post_f_leave <- dj$post_f * as.numeric(dj$person_id %in% leave)
  mj <- feols(stats::as.formula(paste0("E1 ~ post + post_f_stay + post_f_leave | ", REV_FE)),
              data = dj, cluster = ~person_id, notes = FALSE, warn = FALSE)
  b <- coef(mj)[c("post_f_stay", "post_f_leave")]
  V <- stats::vcov(mj)[names(b), names(b)]
  # 05 fits the same model as post_f + post_f_hi, so post_f is the stay
  # coefficient and post_f_hi the leave less stay one.
  pj <- .rv_csv("R11_exit_expectations.csv")
  pe <- function(term, col = "est") {
    if (is.null(pj)) return(NA_real_)
    v <- pj[[col]][pj$label == "(b) by pre-birth chance, k>=2" & pj$outcome == "E1" & pj$term == term]
    if (length(v)) v[1] else NA_real_
  }
  cat("  one model, against 05's R11 (b), from year 2:\n")
  okj <- c(.rv_show("expect to stay", b[["post_f_stay"]], pe("post_f")),
           .rv_show("leave less stay", b[["post_f_leave"]] - b[["post_f_stay"]], pe("post_f_hi")),
           .rv_show("n", stats::nobs(mj), pe("post_f", "n"), 0))
  bs <- sep$stay[["base"]]; bl <- sep$leave[["base"]]
  w  <- c(1 / bs, -1 / bl)
  pd <- sum(w * b)
  pd_se <- sqrt(drop(t(w) %*% V %*% w))
  cat(sprintf(paste0("  proportional change, one model: stay %+.3f, leave %+.3f\n",
                     "  stay less leave %+.3f (SE %.3f); separate fits give %+.3f less %+.3f = %+.3f\n"),
              b[["post_f_stay"]] / bs, b[["post_f_leave"]] / bl, pd, pd_se,
              sep$stay[["prop"]], sep$leave[["prop"]], sep$stay[["prop"]] - sep$leave[["prop"]]))
  st <- .rv_status(all(ok) && all(okj))
  cat("  ", if (st == "ok") "REPRODUCED" else "*** NOT REPRODUCED",
      ": 05's R11 groups, separately and in one model\n", sep = "")
  .rv_add("stay_leave_prop_diff", pd, "STAYLEAVE", "StayLeavePropDiff", st,
          "stay less leave, each over its own pre-birth rate, one model, from year 2")
  .rv_add("stay_leave_prop_diff_se", pd_se, "STAYLEAVE", "StayLeavePropDiffSE", st,
          "delta method, pre-birth bases treated as known")
  .rv_add("stay_prop_joint", b[["post_f_stay"]] / bs, "STAYLEAVE", basis = "one model")
  .rv_add("leave_prop_joint", b[["post_f_leave"]] / bl, "STAYLEAVE", basis = "one model")
  .rv_add("stay_prop_separate", sep$stay[["prop"]], "STAYLEAVE", basis = "Table I.8, own model")
  .rv_add("leave_prop_separate", sep$leave[["prop"]], "STAYLEAVE", basis = "Table I.8, own model")
  .rv_add("stay_base", bs, "STAYLEAVE", basis = "pre-birth rate, k = -3..-1")
  .rv_add("leave_base", bl, "STAYLEAVE", basis = "pre-birth rate, k = -3..-1")
  .rv_add("stay_leave_threshold", thr, "STAYLEAVE", basis = "median pre-birth chance of leaving")
  .rv_add("stay_mothers", length(stay), "STAYLEAVE", basis = "mothers at or below the median")
  .rv_add("leave_mothers", length(leave), "STAYLEAVE", basis = "mothers above the median")
})

# =============================================================================
# BOOT. THE COUPLE BLOCK BOOTSTRAP
# =============================================================================
# Each replicate draws couple clusters with replacement, gives each drawn
# person a new id per draw so a cluster drawn twice counts as two sets of
# people, and refits both sex-specific Sun-Abraham arms: the headline, the
# wage path and the financing contrast. The standard deviation across
# replicates carries the covariance between the arms that summing their
# variances leaves out. It also carries the variation in the cohort weights,
# which the analytic standard errors hold fixed.
if (!rv_run("BOOT")) cat("\nBOOT: not selected in REV_BLOCKS\n") else
if (REV_BOOT_B < 1L) {
  cat("\nBOOT: REV_BOOT_B is 0, so the bootstrap is skipped and its rows are kept\n",
      "      from the last run that made them.\n", sep = "")
} else
.rv_block("BOOT", sprintf("couple block bootstrap, %d replicates", REV_BOOT_B), {
  .seed0 <- if (exists(".Random.seed", envir = globalenv()))
    get(".Random.seed", envir = globalenv()) else NULL
  set.seed(REV_SEED)
  keep <- c("id", "female", "cohort", "wave", "jttpewt_emp", "ln_hourly_wage_real", "fin_d")
  ed <- ev[, keep]
  cl <- unique(ev$couple)
  by_cl <- split(seq_len(nrow(ed)), factor(ev$couple, levels = cl))
  max_id <- max(ed$id)
  cols <- c("att_w", "att_m", "gap", "wage_le", "dd_w", "dd_m", "dd_gap")
  out <- matrix(NA_real_, REV_BOOT_B, length(cols), dimnames = list(NULL, cols))
  hz_bad <- 0L
  t0 <- Sys.time()
  # v-cw. Both arms refitted on the replicate, then differenced on that
  # replicate's common weights, so the bootstrap carries the weights'
  # variation too.
  cgap <- function(w, m, y) {
    fw <- tryCatch(.rv_sa(w, y, vcov = "iid"), error = function(e) NULL)
    fm <- tryCatch(.rv_sa(m, y, vcov = "iid"), error = function(e) NULL)
    if (is.null(fw) || is.null(fm)) return(NULL)
    tryCatch(sa_gap_fits(fw, fm, need_V = FALSE), error = function(e) NULL)
  }
  pick <- function(z, f) if (is.null(z)) NA_real_ else f(z)
  for (b in seq_len(REV_BOOT_B)) {
    draw <- sample.int(length(cl), length(cl), replace = TRUE)
    rr <- by_cl[draw]
    db <- ed[unlist(rr, use.names = FALSE), , drop = FALSE]
    db$id <- db$id + max_id * (rep.int(seq_along(draw), lengths(rr)) - 1L)
    w <- db[db$female == 1, , drop = FALSE]; m <- db[db$female == 0, , drop = FALSE]
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
    if (b == 10L || b %% 25L == 0L || b == REV_BOOT_B) {
      el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
      cat(sprintf("  replicate %d of %d, %.1f min, about %.1f min to go\n", b, REV_BOOT_B,
                  el, el / b * (REV_BOOT_B - b)))
    }
  }
  if (is.null(.seed0)) rm(".Random.seed", envir = globalenv()) else
    assign(".Random.seed", .seed0, envir = globalenv())
  utils::write.csv(data.frame(replicate = seq_len(REV_BOOT_B), out), OUT_BOOT, row.names = FALSE)
  cat("  wrote ", OUT_BOOT, "\n", sep = "")
  need <- max(2L, floor(0.9 * REV_BOOT_B))
  sdv <- function(v) { v <- v[is.finite(v)]; if (length(v) >= need) stats::sd(v) else NA_real_ }
  okr <- stats::complete.cases(out[, c("att_w", "att_m")])
  rho <- if (sum(okr) >= need) stats::cor(out[okr, "att_w"], out[okr, "att_m"]) else NA_real_
  n_ok <- colSums(is.finite(out))
  se_gap <- sdv(out[, "gap"]); se_le <- sdv(out[, "wage_le"]); se_dd <- sdv(out[, "dd_gap"])
  cat(sprintf("  usable replicates: %s\n", paste(sprintf("%s %d", names(n_ok), n_ok), collapse = ", ")))
  if (hz_bad)
    cat("  ", hz_bad, " replicate(s) lost a horizon in the wage windows; their window means\n",
        "  use the horizons they kept, as .wage_path_tests() would.\n", sep = "")
  if (isTRUE(R$repro_sa)) {
    cat(sprintf(paste0("  headline gap: bootstrap SE %.4f against the summed %.4f (ratio %.3f);\n",
                       "  arm SDs %.4f and %.4f against their person-clustered SEs %.4f and %.4f\n"),
                se_gap, R$sa$se, se_gap / R$sa$se, sdv(out[, "att_w"]), sdv(out[, "att_m"]),
                R$sa$se_w, R$sa$se_m))
    cat(sprintf("  bootstrap mean of the gap %+.4f against the estimate %+.4f\n",
                mean(out[, "gap"], na.rm = TRUE), R$sa$gap))
  }
  if (isTRUE(R$repro_wage))
    cat(sprintf("  wage window: bootstrap SE %.4f against the summed %.4f (ratio %.3f)\n",
                se_le, R$wage$se, se_le / R$wage$se))
  cat(sprintf("  financing contrast: bootstrap SE %.4f\n", se_dd))
  # The sign is what matters: a positive correlation means summing the
  # variances overstates the gap's SE, a negative one that it understates it.
  # The correlation's own sampling error at this many replicates decides
  # whether the sign is known.
  rho_se <- if (is.finite(rho)) (1 - rho^2) / sqrt(sum(okr) - 1) else NA_real_
  if (is.finite(rho))
    cat(sprintf("  arm correlation %+.3f (sampling SE about %.3f): %s\n", rho, rho_se,
                if (rho - 1.96 * rho_se > 0) "positive, so summing the variances overstates the SE"
                else if (rho + 1.96 * rho_se < 0)
                  "*** NEGATIVE, so summing the variances understates the SE"
                else "its sign is not settled at this number of replicates"))
  # A short run is a test of the code, not a result.
  bst <- function(base_ok, v) {
    if (!isTRUE(base_ok)) return("not reproduced")
    if (!is.finite(v)) return("too few usable replicates")
    if (REV_BOOT_B < REV_BOOT_MIN) return(sprintf("test run (B < %d)", REV_BOOT_MIN))
    "ok"
  }
  if (REV_BOOT_B < REV_BOOT_MIN)
    cat("  ", REV_BOOT_B, " replicates is a test run: the bootstrap macros are marked so and\n",
        "  stay off the page. Run with REV_BOOT_B=", REV_BOOT_MIN, " or more for the paper.\n", sep = "")
  .rv_add("sa_gap_se_couple", se_gap, "BOOT", "SASECouple", bst(R$repro_sa, se_gap),
          sprintf("SD of the headline gap over %d couple-bootstrap replicates", n_ok[["gap"]]))
  .rv_add("sa_arm_corr", rho, "BOOT", "SAArmCorr", bst(R$repro_sa, rho),
          "correlation of the women's and men's post-birth averages across replicates")
  .rv_add("wage_window_se_couple", se_le, "BOOT", "WageWindowSECouple", bst(R$repro_wage, se_le),
          sprintf("SD of the wage gap, years 5-10 less 0-4, over %d replicates", n_ok[["wage_le"]]))
  .rv_add("sa_arm_corr_se", rho_se, "BOOT",
          basis = "approximate sampling SE of that correlation, (1 - r^2) / sqrt(B - 1)")
  .rv_add("sa_dd_se_couple", se_dd, "BOOT",
          basis = "SD of the Sun-Abraham financing contrast over the replicates")
  .rv_add("boot_sd_women", sdv(out[, "att_w"]), "BOOT", basis = "SD of the women's arm")
  .rv_add("boot_sd_men", sdv(out[, "att_m"]), "BOOT", basis = "SD of the men's arm")
  .rv_add("boot_wage_horizons_lost", hz_bad, "BOOT",
          basis = "replicates whose wage windows lost a horizon")
  .rv_add("boot_replicates", REV_BOOT_B, "BOOT", basis = "replicates drawn")
})

# =============================================================================
# THE SUMMARY
# =============================================================================
tab <- do.call(rbind, S$rows)
if (file.exists(OUT_SUMMARY)) {
  prev <- tryCatch(suppressWarnings(utils::read.csv(
    OUT_SUMMARY, stringsAsFactors = FALSE,
    colClasses = c(build_tag = "character", inputs = "character", files = "character",
                   written = "character", checked = "character"))),
    error = function(e) NULL)
  if (!is.null(prev) && !all(names(tab) %in% names(prev)))
    cat("\n  the last summary has an older layout, so its rows are dropped, not kept.\n",
        "  Run all blocks once to rebuild it.\n", sep = "")
  if (!is.null(prev) && all(names(tab) %in% names(prev))) {
    keep <- prev[!(prev$block %in% S$ran) & !(prev$quantity %in% tab$quantity), names(tab),
                 drop = FALSE]
    if (nrow(keep)) {
      # Checked again, block by block: the published values its block was
      # checked against, the build, and the headline it rests on. A row that
      # passes is stamped with the time of this check and the files read now.
      t_chk <- .rv_now()
      SIGS <- lapply(stats::setNames(nm = unique(keep$block)), .rv_sig)
      now_sig   <- unname(vapply(keep$block, function(b) as.character(SIGS[[b]]), ""))
      now_files <- unname(vapply(keep$block, function(b) attr(SIGS[[b]], "files"), ""))
      was_ok  <- keep$status %in% "ok"
      moved   <- was_ok & !(!is.na(keep$inputs) & keep$inputs == now_sig)
      older   <- was_ok & !moved & !is.na(BT) & (is.na(keep$build_tag) | keep$build_tag != BT)
      on_sa   <- keep$block == "SADDD" | keep$quantity %in% c("sa_gap_se_couple", "sa_arm_corr")
      on_wage <- keep$quantity == "wage_window_se_couple"
      lost    <- was_ok & !moved & !older &
        ((on_sa & !isTRUE(R$repro_sa)) | (on_wage & !isTRUE(R$repro_wage)))
      keep$status[moved] <- "stale (inputs changed)"
      keep$status[older] <- "stale (older build)"
      keep$status[lost]  <- "not reproduced"
      fine <- was_ok & !moved & !older & !lost
      keep$checked[fine] <- t_chk
      keep$files[fine]   <- now_files[fine]
      cat(sprintf("\n  kept %d row(s) from the last run of %s; of the %d marked ok, %d still check out%s%s%s\n",
                  nrow(keep), paste(unique(keep$block), collapse = ", "), sum(was_ok), sum(fine),
                  if (any(moved)) sprintf("; %d whose published inputs changed", sum(moved)) else "",
                  if (any(older)) sprintf("; %d from another build", sum(older)) else "",
                  if (any(lost)) sprintf("; %d resting on a headline no longer reproduced", sum(lost))
                  else ""))
      if (any(moved | older | lost))
        cat("  Rerun the block(s) concerned: ",
            paste(unique(keep$block[moved | older | lost]), collapse = ", "), "\n", sep = "")
      tab <- rbind(tab, keep)
    }
  }
}
utils::write.csv(tab, OUT_SUMMARY, row.names = FALSE)
cat("\n", strrep("=", 78), "\nSUMMARY\n", strrep("=", 78), "\n", sep = "")
.mt <- tab[!is.na(tab$macro) & nzchar(tab$macro), c("macro", "value", "status", "block")]
.mt$value <- ifelse(is.na(.mt$value), "NA",
                    ifelse(abs(.mt$value) >= 1000, formatC(.mt$value, format = "d", big.mark = ","),
                           formatC(.mt$value, format = "f", digits = 4)))
print(.mt, row.names = FALSE)
cat("  wrote ", OUT_SUMMARY, "\n", sep = "")
if (any(.mt$status != "ok"))
  cat("  Macros whose status is not ok stay \\tofill in paper_tables.R.\n")
.rv_done <<- TRUE

}), finally = {
  if (isTRUE(.rv_log) && exists("td_log_close", mode = "function"))
    try(td_log_close(if (isTRUE(.rv_done)) "completed" else "stopped"), silent = TRUE)
  rm(list = intersect(c(".rv_log", ".rv_done"), ls(all.names = TRUE)))
})
