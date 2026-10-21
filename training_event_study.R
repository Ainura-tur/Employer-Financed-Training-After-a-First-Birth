# =====================================================================
# v-ledger-trwin, 19 October: k = 1 to 3 window (apply_ledger_trwin.R).
# training_event_study.R
# Heterogeneity-robust staggered event study of TRAINING around the
# first birth, via Sun & Abraham (2021), fixest::sunab.
#
# Design mirrors CS_event_study.R exactly: treatment = wave the absorbing
# ever_parent indicator turns 0 -> 1; never-treated = always-childless;
# left-censored parents dropped; endpoints binned at |event time| = K;
# women and men estimated separately; women-minus-men gap reported.
#
# Outcomes (linear probability / counts):
#   training_paid_time   employer-time job-related training (jttopwt+jttpewt>0)
#   training_own_time    own-time job-related training      (jttopot+jttpeot>0)
#   any_training         either of the two above: the loader's any_training.
#       Added for Section 6.1, which sets the fall beside studies that do
#       not split training by who finances it.
#   total_training_days_z  days of training, last 12 months    (jttdays)
#   total_courses_z        number of courses, last 12 months   (jttnum)
#       Both gated, from loader v21.48: NA if never asked, 0 if asked and
#       did not train, the reported count otherwise. On an older panel the
#       ungated total_training_days and total_courses are used instead,
#       and the log says so; those read a missing count as 0.
#   jttpewt_any / jttpeot_any   PLACE-OF-EMPLOYMENT-ONLY variants, used
#       automatically when present. The amended loader (HILDA_LOADIND_IV.R)
#       now derives and carries both. HILDA coding confirmed: 1 = Yes,
#       2 = No, available from wave 7; all indicators use == 1 (the former
#       "> 0" rule misclassified 2 = No as training).
#
# Two estimands per outcome, run for both sexes:
#   (a) UNCONDITIONAL: non-employed coded 0 (the loader's coalesce), so the
#       path is the TOTAL effect including employment exit at birth. This is
#       the policy-relevant incidence path.
#   (b) EMPLOYED-ONLY: conditional on employment, subject to the same
#       post-birth selection caveat as wages (positive selection => the
#       conditional training drop is a lower bound). Comparison object for
#       Blundell, Costa-Dias, Goll & Meghir (2021).
#
# Run after MASTER_hh.R with the broad person-wave frame `panel` in the
# workspace, or standalone with the slim rds. Training variables exist
# from the W15-W24 loader; W12-W14 rows (if present) are dropped from the
# training sample with a message.
# Writes training_event_study_results.rds, training_post_att.csv and
# training_window_att.csv; figure only on standalone run
# (or set options(training_estimation_only = TRUE) when sourcing).
# =====================================================================
stopifnot(requireNamespace("fixest", quietly = TRUE))
suppressPackageStartupMessages({ library(dplyr); library(fixest) })

# ---------------------------------------------------------------------------
# WHERE THIS SCRIPT WRITES, said once. Every saveRDS and write.csv below used
# a bare filename, so the destination was getwd(): the same call put the file
# in WG when run from the project root and in WG/TG once the scripts moved
# into a subdirectory. That is how two copies of the panel and two of
# train_ledger.csv came to exist a directory apart, neither obviously current,
# and why paper_tables.R needs a four-path search to find one .rds file.
#
# project_dir is where these landed when the working directory WAS the root,
# so naming it changes nothing about the layout and makes the layout explicit.
# v-route. THE LOCAL DEFINITION WAS SHADOWING THE CONTRACT'S AND POINTING
# SOMEWHERE ELSE. 00_contract.R defines .out_path() as td_out("results", f)
# and seals DIR_RESULTS at PROJ_ROOT/output/results. This file redefined
# the same name to file.path(PROJ_ROOT, f), and because it is sourced AFTER
# the contract its version won: every saveRDS below read as routed and
# landed in the project root instead. On 9 September six .rds files sat
# beside the scripts at 11:01 while the master's own eight csv files, using
# the contract's helper, went to output/results in the same run.
#
# The contract's own note says it: use td_out() or a local NAME, do not
# reassign these. So the contract's helper is preferred when it is loaded,
# and the fallback below routes to the same place rather than to the root,
# so a standalone run puts files where a driven one does.
.out_path <- if (exists("td_out", mode = "function")) {
  function(f) td_out("results", f)
} else {
  function(f) {
    .root <- if (exists("PROJ_ROOT", envir = globalenv()))
      get("PROJ_ROOT", envir = globalenv())
    else if (exists("project_dir", envir = globalenv()))
      get("project_dir", envir = globalenv()) else getwd()
    .d <- file.path(.root, "output", "results")
    dir.create(.d, showWarnings = FALSE, recursive = TRUE)
    file.path(.d, f)
  }
}

# Figures follow the same rule. A bare "figures/..." resolves against
# whatever the session's working directory happens to be, which is the
# defect one line up in a different costume.
.fig_path <- if (exists("td_out", mode = "function")) {
  function(f) td_out("figures", f)
} else {
  function(f) {
    .root <- if (exists("PROJ_ROOT", envir = globalenv()))
      get("PROJ_ROOT", envir = globalenv()) else getwd()
    .d <- file.path(.root, "figures")
    dir.create(.d, showWarnings = FALSE, recursive = TRUE)
    file.path(.d, f)
  }
}

# --- Namespace protection (avoid MASS/stats masking dplyr verbs) ----
select <- dplyr::select;       filter    <- dplyr::filter
mutate <- dplyr::mutate;       slice     <- dplyr::slice
recode <- dplyr::recode;       rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange <- dplyr::arrange;     count     <- dplyr::count;  lag <- dplyr::lag

K <- 8L   # bin |event time| >= K into the endpoints

# A FALL STATED AS A SHARE OF THE PRE-BIRTH RATE NEEDS BOTH HALVES FROM HERE.
# Section 6.1 sets the fall in any training over the birth wave and the two
# years after it beside studies that report a share of the pre-birth rate.
# The average over those horizons needs their joint covariance, which only
# the fitted model holds, and the base must come from the people the event
# study uses. So both are computed in this file and written together.
# TR_PRE is 03_common.R's PRE_LO to PRE_HI.
# v-trwin-13, 19 October. A SECOND WINDOW, k = 1 to 3, the window of
# Ma, Nakab, Navajas-Ahumada and Vidart (2026), who leave out the birth year
# because parental leave dominates it. Section 6.1 sets their 17 to 21
# percent beside the k = 1 to 3 average; the k = 0 to 2 rows are kept.
TR_WINDOWS <- list(c(0L, 2L), c(1L, 3L))   # windows to average over; each within 0 to 9
TR_PRE     <- c(-3L, -1L)       # the pre-birth base: k = -3 to -1
if (exists("PRE_LO") && exists("PRE_HI") &&
    !identical(as.integer(c(PRE_LO, PRE_HI)), TR_PRE))
  cat("NOTE: PRE_LO and PRE_HI are ", PRE_LO, " and ", PRE_HI, " in this session.",
      " The base here uses k = ", TR_PRE[1], " to ", TR_PRE[2], ".\n", sep = "")

# Work on a LOCAL frame `tr_data`; never overwrite the master's `panel`.
# The master's slim W12-W24 panel has no training columns, so when sourced
# after the master we fall through to the training rds automatically.
.tr_names <- c("training_paid_time","training_own_time",
               "total_training_days_z","total_courses_z",
               "total_training_days","total_courses","jttpewt_any","jttpeot_any")
# v2. THE FALLBACK RESOLVED A BARE FILENAME AND DID NOT SCREEN.
#
# Both disk branches tested a bare name against getwd(), so with two copies of
# the panel on disk they took whichever was nearer, and neither applied the
# estimation screen. A frame loaded here therefore had 134,131 rows where the
# master hands in 129,544, and every event study built on it was on a
# different sample from the rest of the pipeline with nothing saying so.
#
# THE IN-MEMORY BRANCH IS UNCHANGED and is still first. When the master has
# already resolved and screened `panel`, that is the frame, and nothing below
# runs. This only governs a stand-alone run.
.tr_find <- function(nm) {
  .roots <- unique(as.character(unlist(list(
    if (exists("PROJ_ROOT")) PROJ_ROOT,
    if (exists("DIR_DATA"))  DIR_DATA,
    if (exists("project_dir")) project_dir,
    getwd(),
    normalizePath(file.path(getwd(), ".."), mustWork = FALSE)))))
  .p <- unique(file.path(.roots, nm))
  .h <- .p[file.exists(.p)]
  if (length(.h) > 1L) {
    cat("NOTE: more than one ", nm, ". Reading the first:\n", sep = "")
    for (.q in .h) cat("        ", .q, "  ",
                       format(file.info(.q)$mtime, "%Y-%m-%d %H:%M"), "\n",
                       sep = "")
  }
  if (length(.h)) .h[1] else NA_character_
}

tr_data <- NULL
if (exists("panel") && any(.tr_names %in% names(panel))) {
  tr_data <- panel
} else {
  .src <- .tr_find("hilda_panel_data_training.rds")
  if (is.na(.src)) .src <- .tr_find("hilda_panel_data_W12_W24_slim.rds")
  if (is.na(.src))
    stop("No training data: run the loader first to build ",
         "hilda_panel_data_training.rds, or hand in a `panel` carrying the ",
         "training columns.", call. = FALSE)
  if (exists("panel"))
    cat("In-memory `panel` carries no training variables; using\n  ",
        .src, "\n", sep = "")
  tr_data <- readRDS(.src)

  # The screen, so a stand-alone run is on the rows the master would have
  # handed in. Reported either way, because a count that does not move means
  # this is the raw extract and its estimates are not comparable.
  #
  # v-screen. td_frame(tr_data) WAS A MISCALL, the one CS_event_study.R found
  # at its v13. td_frame's first argument is need_cols, not a frame, so the
  # call errored on every run, the tryCatch swallowed it, and a filter with
  # 10 and 35 typed in ran instead. That agreed with the contract only while
  # its bounds were 10 and 35. Under TD_SAMPLE_HI=60, every other stage would
  # drop the screen and this one would keep it, with nothing to say so.
  # td_apply_sample_rule() takes a frame and applies the contract's bounds;
  # without the contract, its bounds are read if set and 10 and 35 otherwise.
  # v-screen, 14 October (apply_screen_off.R). NO SCREEN BY DEFAULT. The
  # contract's rule is "none" unless TD_SCREEN=on. Without the contract, the
  # hand rule below follows the same setting, with its bounds from
  # TD_SAMPLE_LO and TD_SAMPLE_HI, so a stand-alone run is on the same rows.
  .n_raw  <- nrow(tr_data)
  .scr_on <- if (exists("TD_SCREEN_ON")) isTRUE(TD_SCREEN_ON) else
    identical(tolower(trimws(Sys.getenv("TD_SCREEN", "off"))), "on")
  .ruled  <- FALSE
  if (exists("td_apply_sample_rule", mode = "function")) {
    tr_data <- td_apply_sample_rule(tr_data)
    .ruled <- TRUE
  } else if (!.scr_on) {
    cat("Contract not loaded, and no screen: TD_SCREEN is off.\n")
    .ruled <- TRUE
  } else if (all(c("age", "experience_years") %in% names(tr_data))) {
    .lo <- suppressWarnings(as.numeric(Sys.getenv("TD_SAMPLE_LO", "")))
    .hi <- suppressWarnings(as.numeric(Sys.getenv("TD_SAMPLE_HI", "")))
    if (is.na(.lo)) .lo <- 10
    if (is.na(.hi)) .hi <- 35
    .gap    <- tr_data$age - tr_data$experience_years
    tr_data <- tr_data[!is.na(.gap) & .gap >= .lo & .gap <= .hi, , drop = FALSE]
    cat(sprintf("Contract not loaded: screen applied by hand, %g to %g.\n", .lo, .hi))
    .ruled <- TRUE
    rm(.gap, .lo, .hi)
  }
  cat(sprintf("Sample rule: %s of %s rows retained (%s removed).\n",
              format(nrow(tr_data), big.mark = ","),
              format(.n_raw,        big.mark = ","),
              format(.n_raw - nrow(tr_data), big.mark = ",")))
  # Nothing removed is expected with no screen; it is a problem only when
  # TD_SCREEN is on and the screen could not be applied.
  if (!.ruled)
    cat("*** NO SCREEN WAS APPLIED although TD_SCREEN is on: the contract is\n",
        "    not loaded and the frame lacks age or experience_years. This run is\n",
        "    on the whole extract and its event studies are on a different\n",
        "    sample from the rest of the pipeline.\n", sep = "")
  rm(.n_raw, .src, .ruled, .scr_on)
}
stopifnot(all(c("person_id","wave","female","ever_parent","employed") %in% names(tr_data)))

# --- outcome set: use what the panel actually carries ----------------
# v21.48. THE GATED DOSAGE TWINS, when the panel has them. The ungated
# pair reads a missing count as 0, so a trainee who gave no count and a
# person the module never reached both count as no training. The _z pair
# is NA for both, as total_training_hours_z has been since v21.9. On the
# unconditional frame the never-asked therefore drop out of the two
# dosage outcomes, while the incidence outcomes keep their declared zero.
.dose_pick <- function(z, raw) {
  if (z %in% names(tr_data)) return(z)
  if (raw %in% names(tr_data)) {
    cat("NOTE: ", z, " is absent, so ", raw, " is used. Its zeros include\n",
        "  never-asked person-waves and trainees who gave no count.\n", sep = "")
    return(raw)
  }
  NA_character_
}
tr_core <- c("training_paid_time", "training_own_time",
             .dose_pick("total_training_days_z", "total_training_days"),
             .dose_pick("total_courses_z", "total_courses"),
             "any_training")
tr_core <- tr_core[!is.na(tr_core)]
tr_pe   <- c("jttpewt_any","jttpeot_any")           # pe-only, if loader amended
tr_outcomes <- c(intersect(tr_core, names(tr_data)),
                 intersect(tr_pe,   names(tr_data)))
if (!length(tr_outcomes))
  stop("No training variables found in `panel`. Check the loader's final select.")
cat("Training outcomes found:", paste(tr_outcomes, collapse = ", "), "\n")
if (!all(tr_pe %in% names(tr_data)))
  cat("(place-of-employment-only items not in panel; using pooled indicators.\n",
      " To add them, see the loader amendment in this script's header.)\n")

# --- training sample: waves where the training module is observed ----
# The W15-W24 loader supplies the training vars; if the in-memory panel is
# W12-W24, the early waves carry no training info. Keep waves where at least
# one training outcome is non-degenerate.
tr_wave_ok <- tr_data %>% group_by(wave) %>%
  summarise(ok = any(sapply(across(all_of(tr_outcomes)), function(x) any(!is.na(x) & x != 0))),
            .groups = "drop")
tr_waves <- tr_wave_ok$wave[tr_wave_ok$ok]
cat(sprintf("Training module observed in waves %d-%d; dropping other waves.\n",
            min(tr_waves), max(tr_waves)))
tpanel <- tr_data %>% filter(wave %in% tr_waves)

# --- first-birth timing: identical to CS_event_study.R ---------------
# (cohort defined on the FULL panel so timing is not censored by the
#  training-wave restriction)
g_tbl <- tr_data %>% group_by(person_id) %>% arrange(wave) %>%
  summarise(ep_first = first(ever_parent), ep_last = last(ever_parent),
            trans = { d_ep <- diff(ever_parent)
            idx  <- which(!is.na(d_ep) & d_ep > 0)
            if (length(idx)) wave[idx[1] + 1] else NA_integer_ },
            .groups = "drop") %>%
  mutate(cohort = case_when(
    !is.na(trans)                ~ as.double(trans),
    ep_first == 0 & ep_last == 0 ~ 10000,
    TRUE                         ~ NA_real_))

cs <- tpanel %>% inner_join(select(g_tbl, person_id, cohort), by = "person_id") %>%
  filter(!is.na(cohort))

clean_num <- function(x) { x <- unclass(x); attributes(x) <- NULL
if (is.numeric(x)) x else suppressWarnings(as.numeric(as.character(x))) }
for (v in c("wave","female","employed", tr_outcomes)) cs[[v]] <- clean_num(cs[[v]])
cs <- cs %>% mutate(id = as.integer(factor(person_id)),
                    wave = as.double(wave), cohort = as.double(cohort))
cs <- as.data.frame(cs)
cat(sprintf("Sample: %d persons (%d never-treated, %d treated); waves %d-%d; K=%d\n",
            n_distinct(cs$id), n_distinct(cs$id[cs$cohort == 10000]),
            n_distinct(cs$id[cs$cohort < 10000]),
            as.integer(min(cs$wave)), as.integer(max(cs$wave)), K))

BIN <- stats::setNames(list((-200):(-K), K:200), c(as.character(-K), as.character(K)))

fit <- function(d, y) {
  f <- as.formula(paste0(y, " ~ sunab(cohort, wave, bin.rel = BIN) | id + wave"))
  m <- tryCatch(feols(f, data = d, cluster = ~ id), error = function(e) NULL)
  if (is.null(m))
    m <- feols(as.formula(paste0(y, " ~ sunab(cohort, wave) | id + wave")),
               data = d, cluster = ~ id)
  m
}
es_tab <- function(m) {
  ct <- coef(m); s <- se(m); nm <- names(ct)
  rt <- suppressWarnings(as.numeric(sub(".*::(-?\\d+).*", "\\1", nm)))
  k <- !is.na(rt) & grepl("::", nm)
  # v25. SIX DECIMALS, NOT FOUR. training_decomposition.csv and the timing
  # figure are both built from this object, and the manuscript quotes three
  # decimals off the back of them. A coefficient stored as -0.1165 sits on
  # the three-decimal boundary and the printed figure then depends on which
  # rounding ran second. Six decimals decide the third; nothing else about
  # the table changes.
  data.frame(event_time = rt[k], att = round(unname(ct[k]), 6),
             se = round(unname(s[k]), 6))[order(rt[k]), ]
}
att_overall <- function(m) tryCatch({
  a <- summary(m, agg = "att")$coeftable
  a[grep("ATT|att", rownames(a))[1], 1:2] }, error = function(e) c(NA, NA))
# The average over a window of horizons, by the same fixest aggregation that
# att_overall() applies to every post-birth horizon: the cohort-by-horizon
# cells in the window are weighted by their size, and the standard error uses
# their joint covariance. The pattern has no capture group, so the window
# comes back as one row.
att_window <- function(m, lo, hi) tryCatch({
  if (lo < 0 || hi > 9 || lo > hi) stop("the window must lie within 0 to 9")
  a <- aggregate(m, c(window = paste0("wave::[", paste(lo:hi, collapse = ""), "]:cohort")))
  a[1, 1:2] }, error = function(e) c(NA_real_, NA_real_))
# The pre-birth base: the outcome's mean among the treated at k = TR_PRE, on
# the rows the model was fitted on.
pre_base <- function(d, y) {
  k <- d$wave - d$cohort
  s <- d$cohort < 10000 & k >= TR_PRE[1] & k <= TR_PRE[2] & !is.na(d[[y]])
  c(base = if (any(s)) mean(d[[y]][s]) else NA_real_, n = sum(s))
}
gap <- function(a, b) { e <- intersect(a$event_time, b$event_time)
data.frame(event_time = e,
           gap = round(a$att[match(e, a$event_time)] - b$att[match(e, b$event_time)], 6),
           se  = round(sqrt(a$se[match(e, a$event_time)]^2 +
                              b$se[match(e, b$event_time)]^2), 6)) }

# --- estimation: unconditional and employed-only ---------------------
# THE POST-BIRTH AGGREGATE IS KEPT, not just printed. att_overall() already
# computes it and line 265 prints it, and until now that was the end of it:
# Section 7.4 quotes -0.1086 (SE 0.0220) and no artefact carried either, so
# both were typed into the .tex and stayed there. The point estimate could
# arguably be rebuilt downstream from the per-horizon series; the standard
# error could not, because an average of correlated horizon estimates needs
# their joint covariance and the saved tables keep only the diagonal. This is
# the one place both exist.
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
res <- list(); .post <- list(); .win <- list()
.cells <- list()         # cohort-by-event cells of every fit, for common weights
for (cond in c("unconditional","employed_only")) {
  cat("\n", strrep("#", 70), "\n#  ", toupper(cond), "\n", strrep("#", 70), "\n", sep = "")
  base <- if (cond == "employed_only") filter(cs, employed == 1) else cs
  for (sx in c("women","men")) {
    d <- filter(base, female == ifelse(sx == "women", 1, 0))
    for (y in tr_outcomes) {
      m <- tryCatch(fit(d, y), error = function(e) NULL)
      if (is.null(m)) { cat(sprintf("  %s %s %s: not estimable\n", cond, sx, y)); next }
      tag <- paste(cond, sx, y); a <- att_overall(m)
      .cells[[tag]] <- sa_try(m, tag)
      cat(sprintf("\n== %s ==\noverall post-birth ATT = %.4f (SE %.4f)\n", tag, a[1], a[2]))
      .post[[tag]] <- data.frame(frame = cond, sex = sx, outcome = y,
                                 att = round(unname(a[1]), 6),
                                 se  = round(unname(a[2]), 6),
                                 stringsAsFactors = FALSE)
      .tw_b <- pre_base(d, y)
      for (.tw_w in TR_WINDOWS) {
        .tw_a <- att_window(m, .tw_w[1], .tw_w[2])
        .win[[paste(tag, .tw_w[1], .tw_w[2])]] <- data.frame(
          frame = cond, sex = sx, outcome = y, k_lo = .tw_w[1], k_hi = .tw_w[2],
          att = round(unname(.tw_a[1]), 6), se = round(unname(.tw_a[2]), 6),
          base = round(unname(.tw_b[["base"]]), 6), n_base = unname(.tw_b[["n"]]),
          stringsAsFactors = FALSE)
      }
      et <- es_tab(m); print(et); res[[tag]] <- et
    }
  }
  for (y in tr_outcomes) {
    w <- res[[paste(cond, "women", y)]]; m <- res[[paste(cond, "men", y)]]
    if (is.null(w) || is.null(m)) next
    cat(sprintf("\n== child penalty (women - men), %s [%s] ==\n", y, cond))
    print(gap(w, m))
  }
}
# v-cw. THE GAP PATHS ON COMMON WEIGHTS, stored as "<frame> gap <outcome>"
# beside the per-sex series. MASTER's 8.2 and 8.2b and mpn_side() in
# MASTER_paper_numbers.R read them. The gap() prints above still difference
# the two arms on their own weights; they are log lines only.
for (cond in c("unconditional", "employed_only")) for (y in tr_outcomes) {
  .cg <- tryCatch(sa_common_gap(.cells[[paste(cond, "women", y)]],
                                .cells[[paste(cond, "men", y)]]), error = function(e) NULL)
  if (!is.null(.cg) && !is.null(.cg$V))
    res[[paste(cond, "gap", y)]] <- data.frame(event_time = .cg$k, gap = round(.cg$g, 6),
                                               se = round(.cg$path$se, 6))
}
rm(list = intersect(".cg", ls(all.names = TRUE)))
saveRDS(.cells, .out_path("training_event_study_cells.rds"))
saveRDS(res, .out_path("training_event_study_results.rds"))
cat("\nSaved training_event_study_results.rds\n")

# --- the post-birth aggregate, written --------------------------------
# One row per frame, sex and outcome, plus a derived women-minus-men row so
# nothing downstream has to decide how the two combine. The variance rule is
# the paper's standing one and the same gap() uses for the per-horizon
# series: the two arms are fitted on disjoint persons, so the variances add.
# It is named in a column rather than left to a reader, because a gap row
# whose SE rule is implicit is the kind of number that gets quoted and then
# cannot be defended.
if (length(.post)) {
  .pa <- do.call(rbind, .post); rownames(.pa) <- NULL
  .pa$rule <- "estimated"
  .g <- do.call(rbind, lapply(
    split(.pa, list(.pa$frame, .pa$outcome), drop = TRUE), function(z) {
      w <- z[z$sex == "women", ]; m <- z[z$sex == "men", ]
      if (!nrow(w) || !nrow(m) || !is.finite(w$att) || !is.finite(m$att))
        return(NULL)
      data.frame(frame = w$frame[1], sex = "gap", outcome = w$outcome[1],
                 att = round(w$att[1] - m$att[1], 6),
                 se  = round(sqrt(w$se[1]^2 + m$se[1]^2), 6),
                 rule = "women - men; variances added, arms on disjoint persons",
                 stringsAsFactors = FALSE)
    }))
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
  .pa <- rbind(.pa, .g); rownames(.pa) <- NULL
  write.csv(.pa, .out_path("training_post_att.csv"), row.names = FALSE)
  cat("\nSaved training_post_att.csv: ", nrow(.pa), " rows (",
      sum(.pa$sex == "gap"), " derived gaps).\n", sep = "")
  print(.pa[.pa$sex == "gap" & .pa$outcome == "jttpewt_any", ])
  rm(.pa, .g)
}
rm(.post)

# --- the window average, as a share of the pre-birth base ---------------
# One row per frame, sex, outcome and window. pct is 100 * att / base, and
# its interval treats the base as fixed: the base is a mean over many
# person-waves (n_base), so its own sampling error is small beside the
# estimate's. There is no women-minus-men row, because the two shares have
# different bases.
if (length(.win)) {
  .wa <- do.call(rbind, .win); rownames(.wa) <- NULL
  .ok <- is.finite(.wa$base) & .wa$base > 0
  .wa$base_pct <- round(100 * .wa$base, 4)
  .wa$pct    <- ifelse(.ok, 100 * .wa$att / .wa$base, NA_real_)
  .wa$pct_se <- ifelse(.ok, 100 * .wa$se / .wa$base, NA_real_)
  .wa$pct_lo <- .wa$pct - stats::qnorm(0.975) * .wa$pct_se
  .wa$pct_hi <- .wa$pct + stats::qnorm(0.975) * .wa$pct_se
  .wa$base_window <- sprintf("treated, k = %d to %d", TR_PRE[1], TR_PRE[2])
  .wa$rule <- paste("fixest aggregate over the window, cells weighted by size;",
                    "pct = 100 att / base, base treated as fixed")
  write.csv(.wa, .out_path("training_window_att.csv"), row.names = FALSE)
  cat("\nSaved training_window_att.csv: ", nrow(.wa), " rows.\n", sep = "")
  .show <- .wa[.wa$outcome == "any_training",
               c("frame", "sex", "k_lo", "k_hi", "att", "se", "base", "pct", "pct_lo", "pct_hi")]
  if (nrow(.show)) {
    cat("Any training over the window, as a share of each sex's own base:\n")
    .show[] <- lapply(.show, function(z) if (is.numeric(z)) round(z, 3) else z)
    print(.show, row.names = FALSE)
  }
  rm(.wa, .ok, .show)
}
rm(list = intersect(c(".win", ".tw_b", ".tw_w", ".tw_a"), ls(all.names = TRUE)))

cat("\nNotes:\n",
    " * UNCONDITIONAL paths code the non-employed as 0 (the loader's coalesce),\n",
    "   so they bundle employment exit with the training margin: the total\n",
    "   incidence effect of the first birth on training receipt. The two\n",
    "   dosage outcomes are the exception: their gated twins drop the\n",
    "   never-asked, as total_training_hours_z does.\n",
    " * EMPLOYED-ONLY paths isolate the training margin among the employed but\n",
    "   inherit post-birth positive selection (cf. the wage lower-bound logic);\n",
    "   a conditional drop is therefore a lower bound on the true drop.\n",
    " * Read pre-birth coefficients as the parallel-trends test, preferably in\n",
    "   the women-minus-men gap where life-cycle training trends cancel.\n",
    " * Interpretation hook: Blundell et al. (2021) argue training partially\n",
    "   rebuilds post-birth earnings capacity; a post-birth training DEFICIT for\n",
    "   mothers means the rebuild channel is under-used precisely where the\n",
    "   experience channel bites, reinforcing the returnship policy lever.\n")

# =====================================================================
# Figure: women-minus-men training incidence paths (standalone run only)
# =====================================================================
if (!isTRUE(getOption("training_estimation_only"))) {
  suppressPackageStartupMessages({ library(ggplot2) })
  pick <- intersect(c("training_paid_time","training_own_time"), tr_outcomes)
  dat <- dplyr::bind_rows(lapply(pick, function(y) {
    g <- gap(res[[paste("unconditional","women",y)]],
             res[[paste("unconditional","men",y)]])
    transform(g, outcome = y)
  })) %>%
    mutate(lo = gap - 1.96 * se, hi = gap + 1.96 * se,
           event_time = pmax(pmin(event_time, K), -K))
  p <- ggplot(dat, aes(event_time, gap)) +
    facet_wrap(~outcome) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
    geom_vline(xintercept = -0.5, linetype = 2, linewidth = 0.3, colour = "grey60") +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15) +
    geom_line() + geom_point(size = 1.4) +
    labs(x = "Years from first birth (women minus men)",
         y = "ATT, training incidence") +
    theme_minimal(base_size = 11)
  .f <- .fig_path("fig_training_event_study.pdf")
  ggsave(.f, p, width = 9, height = 3.6)
  cat("wrote ", .f, "\n", sep = "")
  rm(.f)
}
