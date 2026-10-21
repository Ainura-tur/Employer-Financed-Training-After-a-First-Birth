# =============================================================================
# screen_audit.R   Where the experience screen bites, and what it moves
#
# A REVIEWER ASKS that the training results be estimated without the screen
# that keeps person-waves with age minus experience between 10 and 35. The
# loader caps experience at age minus 13, so only the upper bound ever binds.
# It removes people whose years in paid work fall far short of their age, that
# is, long career interruptions. This script measures the screen before
# anything is rerun, and answers three questions.
#
#   WHO. The removals and their rate under every grouping the project uses,
#        side by side, so the paper quotes one set of counts and names the
#        grouping behind them.
#   WHERE. Each removed row placed against the event-study roster: outside it
#        (people already parents when first seen, whom the design never uses)
#        or inside it, by sex, event time and employment. Only rows inside the
#        roster can move an event-study or difference-in-differences estimate.
#   HOW MUCH. The Sun-Abraham post-birth averages that CS_event_study.R
#        publishes, women less men, fitted on the roster with and without the
#        screen. The fit on the rule in force must reproduce the published
#        values first, so the comparison uses the paper's own specification
#        and rows.
#
# v2, 14 OCTOBER. THE SCREEN IS OFF BY DEFAULT NOW (00_contract.R v28), so the
# published values are the ones without it. The audit reproduces them on the
# rule in force, whichever it is, and fits the other sample beside them. The
# screen it describes is the contract's, TD_SAMPLE_LO to TD_SAMPLE_HI, 10 to
# 35 unless they are set. RUN_ALL.R runs it as the stage "screen", after
# MASTER, so its csvs are rebuilt on every pass.
#
# v3, 14 OCTOBER. TWO ESTIMATORS. With the common-weight patch
# (sa_common_weights.R, sa_patches.R), CS_event_study.R's gap row is the
# women-minus-men difference formed inside each cohort-by-event cell and
# averaged on pooled weights, and the earlier estimator, each arm on its own
# weights, moves to the gap_separate row. The audit forms both from the same
# two fits, so the screen's effect is measured on each, and it reproduces
# whichever rows CS published. Without sa_common_weights.R beside the
# scripts it measures the separate weights alone, as v2 did.
#
# "WITHOUT THE SCREEN" keeps every person-wave in the extract. The screen also
# drops rows whose age or experience is missing. None of the designs here uses
# experience, so those rows come back too, and the tables count them
# separately.
#
# It changes nothing the paper reads. It writes to output/diagnostics:
#   screen_audit_groups.csv     removals and rates under each grouping
#   screen_audit_location.csv   removed rows by roster status, sex, event time
#                               and employment
#   screen_audit_roster.csv     the roster with and without the screen, and
#                               the people whose status the screen changes
#   screen_audit_sa.csv         the Sun-Abraham averages with and without,
#                               one row per outcome and estimator;
#                               `published_estimator` marks the one CS
#                               published as its gap row
#
# RUN from the project root, after CS_event_study.R:
#   source("screen_audit.R")
# It fits 24 Sun-Abraham models, which takes a few minutes.
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
if (!(exists("PROJ_ROOT") && exists("td_apply_sample_rule", mode = "function"))) {
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
stopifnot(exists("PROJ_ROOT"))

# ---- the log ------------------------------------------------------------------
.sa_log <- exists("td_log_open", mode = "function")
if (.sa_log) {
  .sa_try <- try(td_log_open("screen_audit.R"), silent = TRUE)
  if (inherits(.sa_try, "try-error"))
    cat("  td_log_open() failed: ", conditionMessage(attr(.sa_try, "condition")),
        "\n  Output goes to the console only.\n", sep = "")
  rm(.sa_try)
} else {
  cat("  no contract logger, so output goes to the console only.\n")
}

.sa_done <- FALSE
.sa_width <- options(width = 160)
tryCatch(local({

# =============================================================================
# SETTINGS
# =============================================================================
# The contract's screen, applied or not, compared with none. Which of the two
# samples must reproduce the published values depends on the rule in force:
# the screened one under TD_SCREEN=on, the whole extract otherwise.
SCREEN_LO <- get0("TD_SAMPLE_LO", ifnotfound = 10)
SCREEN_HI <- get0("TD_SAMPLE_HI", ifnotfound = 35)
RULE_ON   <- isTRUE(get0("TD_SCREEN_ON", ifnotfound = FALSE))
cat("  the screen: ", SCREEN_LO, " to ", SCREEN_HI, ". Rule in force: ",
    if (RULE_ON) "the screen (TD_SCREEN=on)" else "none (TD_SCREEN is off)",
    ".\n", sep = "")
K        <- 8L      # CS_event_study.R's endpoint bins, |k| >= K
SA_TOL   <- 1e-4    # the published values are stored to six decimals

# The common-weight functions, from where sa_patches.R puts them. Sourced into
# this local block, so nothing is left in the session.
HAVE_CW <- exists("sa_common_gap", mode = "function") &&
  exists("sa_cells", mode = "function")
if (!HAVE_CW) {
  .sa_src <- unique(c("sa_common_weights.R", file.path(PROJ_ROOT, "sa_common_weights.R")))
  .sa_src <- .sa_src[file.exists(.sa_src)]
  if (length(.sa_src)) source(.sa_src[1], local = TRUE)
  HAVE_CW <- exists("sa_common_gap", mode = "function") &&
    exists("sa_cells", mode = "function")
  rm(.sa_src)
}
cat("  common-weight functions: ",
    if (HAVE_CW) "loaded, so both estimators are measured" else
      "not found (sa_common_weights.R), so the separate weights alone", ".\n", sep = "")
DIAG     <- if (exists("DIR_DIAG")) DIR_DIAG else file.path(PROJ_ROOT, "output", "diagnostics")
dir.create(DIAG, showWarnings = FALSE, recursive = TRUE)
OUT <- function(f) file.path(DIAG, f)

# Published artefacts, found by name where CS writes them.
.dirs <- unique(normalizePath(c(
  get0("DIR_RESULTS", ifnotfound = file.path(PROJ_ROOT, "output", "results")),
  file.path(PROJ_ROOT, "output", "results"), PROJ_ROOT), winslash = "/", mustWork = FALSE))
.find <- function(name) {
  p <- file.path(.dirs, name)
  p <- p[file.exists(p)]
  if (!length(p)) { cat("  [missing] ", name, "\n", sep = ""); return(NA_character_) }
  p[1]
}

# CS_event_study.R's conversion, so the roster and the rows are CS's.
clean_num <- function(x) { x <- unclass(x); attributes(x) <- NULL
  if (is.numeric(x)) x else suppressWarnings(as.numeric(as.character(x))) }
.n <- function(x) format(x, big.mark = ",")

# =============================================================================
# THE EXTRACT, BEFORE THE SCREEN
# =============================================================================
# The file td_frame() reads when it builds the frame, so the screened subset
# here is the contract's frame. Checked below against the cached frame.
.cands <- if (exists("TD_FRAME_FILES")) TD_FRAME_FILES else
  file.path(c(file.path(PROJ_ROOT, "data"), PROJ_ROOT, "."), "hilda_panel_data_W12_W24_slim.rds")
.pf <- .cands[file.exists(.cands)][1]
if (is.na(.pf)) stop("the panel file is in none of: ", paste(.cands, collapse = ", "), call. = FALSE)
ext <- readRDS(.pf)
BT  <- attr(ext, "build_tag"); BT <- if (is.null(BT)) NA_character_ else as.character(BT)
.need <- c("person_id", "wave", "female", "age", "experience_years", "ever_parent",
           "employed", "jttpewt_any", "ln_hourly_wage_real")
.miss <- setdiff(.need, names(ext))
if (length(.miss)) stop("the extract lacks: ", paste(.miss, collapse = ", "), call. = FALSE)
ext <- as.data.frame(ext)
cat(sprintf("Extract: %s person-waves, %s persons, build_tag %s\n  %s\n",
            .n(nrow(ext)), .n(n_distinct(ext$person_id)), BT, normalizePath(.pf, winslash = "/")))

.gap   <- clean_num(ext$age) - clean_num(ext$experience_years)
.nogap <- is.na(.gap)
ext$why <- ifelse(.nogap, "age or experience missing",
                  ifelse(.gap < SCREEN_LO, sprintf("below %g", SCREEN_LO),
                         ifelse(.gap > SCREEN_HI, sprintf("above %g", SCREEN_HI), "kept")))
ext$kept <- ext$why == "kept"
rm(.gap, .nogap)
cat("  the screen, ", SCREEN_LO, " to ", SCREEN_HI, ":\n", sep = "")
print(table(ext$why))
cat(sprintf("  screened: %s person-waves, %s persons\n",
            .n(sum(ext$kept)), .n(n_distinct(ext$person_id[ext$kept]))))

# REPRODUCE THE CONTRACT'S FRAME FIRST. The cached frame is the one every
# stage reads; if the screen here does not give its rows, nothing below is
# about the paper's sample. Read only if cached, so this script writes no frame.
FRAME_OK <- NA
if (exists("td_has", mode = "function") && exists("td_get", mode = "function") &&
    isTRUE(tryCatch(td_has("frame"), error = function(e) FALSE))) {
  fr <- tryCatch(td_get("frame"), error = function(e) NULL)
  if (!is.null(fr)) {
    fbt <- attr(fr, "build_tag"); fbt <- if (is.null(fbt)) NA_character_ else as.character(fbt)
    .ref    <- if (RULE_ON) ext$kept else rep(TRUE, nrow(ext))
    same_n  <- nrow(fr) == sum(.ref)
    same_id <- setequal(unique(as.character(fr$person_id)),
                        unique(as.character(ext$person_id[.ref])))
    same_bt <- identical(fbt, BT)
    FRAME_OK <- same_n && same_id && same_bt
    cat(sprintf("  against the cached frame: %s rows (%s), persons %s, build %s (%s)\n",
                .n(nrow(fr)), if (same_n) "the same" else "DIFFERENT",
                if (same_id) "the same" else "DIFFERENT", fbt,
                if (same_bt) "the same" else "DIFFERENT"))
    rm(fr, fbt, same_n, same_id, same_bt, .ref)
  }
}
if (is.na(FRAME_OK))
  cat("  no cached frame to check against; the reproduction of CS below is the check.\n")
if (isFALSE(FRAME_OK))
  cat("  *** The rule in force, applied here, does not give the contract's frame.\n",
      "      Check that the panel file is the one the frame was built from, and\n",
      "      that handoff/frame.rds was built under the same TD_SCREEN, before\n",
      "      reading on.\n", sep = "")

# =============================================================================
# THE ROSTER, BY CS_event_study.R'S RULE
# =============================================================================
# The first 0-to-1 step in ever_parent is the birth wave; never a parent is
# cohort 10000; a parent when first seen is left-censored and dropped.
.roster <- function(d) {
  d %>% group_by(person_id) %>% arrange(wave) %>%
    summarise(ep_first = first(ever_parent), ep_last = last(ever_parent),
              trans = { d_ep <- diff(ever_parent)
              idx  <- which(!is.na(d_ep) & d_ep > 0)
              if (length(idx)) wave[idx[1] + 1] else NA_integer_ },
              .groups = "drop") %>%
    mutate(cohort = case_when(
      !is.na(trans)                ~ as.double(trans),
      ep_first == 0 & ep_last == 0 ~ 10000,
      TRUE                         ~ NA_real_),
      status = case_when(
        !is.na(cohort) & cohort < 10000 ~ "treated",
        cohort %in% 10000               ~ "never-treated",
        ep_first %in% 1                 ~ "outside: already a parent when first seen",
        TRUE                            ~ "outside: parenthood not resolved")) %>%
    as.data.frame()
}
g_all <- .roster(ext)
g_scr <- .roster(ext[ext$kept, , drop = FALSE])

# The estimation rows, as CS builds them.
.cs <- function(d, g) {
  cs <- d %>% inner_join(select(g, person_id, cohort), by = "person_id") %>%
    filter(!is.na(cohort)) %>% as.data.frame()
  for (v in intersect(c("wave", "female", "ln_hourly_wage_real", "experience_years",
                        "employed", "jttpewt_any", "age"), names(cs)))
    cs[[v]] <- clean_num(cs[[v]])
  cs$id     <- as.integer(factor(cs$person_id))
  cs$wave   <- as.double(cs$wave)
  cs$cohort <- as.double(cs$cohort)
  cs$jttpewt_emp <- ifelse(cs$employed == 1, cs$jttpewt_any, NA_real_)
  cs
}
cs_all <- .cs(ext, g_all)
cs_scr <- .cs(ext[ext$kept, , drop = FALSE], g_scr)

# The roster on the rule in force against CS's own record of it.
ROSTER_OK <- NA
.mf <- .find("cs_event_study_sample_manifest.rds")
if (!is.na(.mf)) {
  man  <- readRDS(.mf)
  .csr <- if (RULE_ON) cs_scr else cs_all
  here <- c(persons = n_distinct(.csr$person_id),
            treated = n_distinct(.csr$person_id[.csr$cohort < 10000]),
            never   = n_distinct(.csr$person_id[.csr$cohort == 10000]),
            rows    = nrow(.csr))
  rm(.csr)
  pub  <- c(persons = man$n_persons_full, treated = man$n_treated,
            never = man$n_never_treated, rows = man$n_obs_full)
  ROSTER_OK <- identical(as.numeric(here), as.numeric(pub))
  cat("\n  roster on the rule in force against CS's manifest: ",
      if (ROSTER_OK) "the same" else "DIFFERENT", "\n", sep = "")
  print(rbind(here = here, published = pub))
  rm(man, here, pub)
}
rm(.mf)

# =============================================================================
# WHO: REMOVALS UNDER EACH GROUPING
# =============================================================================
fem <- clean_num(ext$female)
sexlab <- ifelse(fem %in% 1, "women", ifelse(fem %in% 0, "men", "sex missing"))
ep  <- clean_num(ext$ever_parent)
ep_life <- if ("ever_parent_life" %in% names(ext)) clean_num(ext$ever_parent_life) else
  stats::ave(ifelse(is.na(ep), -Inf, ep), ext$person_id, FUN = max)
ep_life[!is.finite(ep_life)] <- NA
.lab4 <- function(f, p) ifelse(is.na(f) | is.na(p), "missing",
                               ifelse(f == 1, ifelse(p == 1, "mothers", "never-mothers"),
                                      ifelse(p == 1, "fathers", "childless men")))
groupings <- list(
  "parenthood at the wave (ever_parent)" = .lab4(fem, ep),
  "lifetime parenthood (ever a parent in the panel)" = .lab4(fem, ep_life))
if ("group_ever" %in% names(ext))
  groupings[["group_ever, as the panel ships it"]] <- as.character(ext$group_ever)
if (all(c("motherhood_status_life", "fatherhood_status_life") %in% names(ext)))
  groupings[["four-way lifetime status (never, pre, active, post)"]] <-
    paste(sexlab, dplyr::coalesce(as.character(ext$motherhood_status_life),
                                  as.character(ext$fatherhood_status_life)), sep = ": ")
if ("group_current" %in% names(ext))
  groupings[["children under 15 at the wave (group_current)"]] <- as.character(ext$group_current)
st_row <- g_all$status[match(ext$person_id, g_all$person_id)]
groupings[["event-study roster, without the screen"]] <- paste(sexlab, st_row, sep = ": ")

.grp_tab <- function(lab, g) {
  g[is.na(g)] <- "missing"
  data.frame(grouping = lab, group = g, why = ext$why, stringsAsFactors = FALSE) %>%
    group_by(grouping, group) %>%
    summarise(person_waves = dplyr::n(),
              removed = sum(why != "kept"),
              removed_bound = sum(why == sprintf("above %g", SCREEN_HI) |
                                    why == sprintf("below %g", SCREEN_LO)),
              removed_missing = sum(why == "age or experience missing"),
              .groups = "drop") %>%
    mutate(pct_removed = round(100 * removed / person_waves, 2)) %>%
    as.data.frame()
}
GROUPS <- do.call(rbind, Map(.grp_tab, names(groupings), groupings))
cat("\n", strrep("=", 78), "\nWHO: removals under each grouping\n", strrep("=", 78), "\n", sep = "")
for (lab in unique(GROUPS$grouping)) {
  cat("\n  ", lab, "\n", sep = "")
  x <- GROUPS[GROUPS$grouping == lab, c("group", "person_waves", "removed", "pct_removed")]
  print(x[order(-x$pct_removed), ], row.names = FALSE)
}
cat("\n  Every grouping divides the same ", .n(sum(!ext$kept)), " removed person-waves.",
    "\n  Quote one grouping in the paper and name it.\n", sep = "")

# What the removed rows look like: whose experience is reported, and how long
# they have been out of the labour force.
# The fallback, age minus schooling minus five, puts age minus experience at
# schooling plus five, inside the bound, so only reported histories should fail.
if ("experience_source" %in% names(ext)) {
  .src <- as.character(ext$experience_source[!ext$kept])
  cat("\n  experience source of the removed rows:\n")
  print(table(source = .src, useNA = "ifany"))
  if (any(.src %in% "mincer_proxy"))
    cat("  *** ", sum(.src %in% "mincer_proxy"), " removed rows use the fallback, which should not be\n",
        "      able to fail the screen. Look at them before quoting the source split.\n", sep = "")
  rm(.src)
}
if ("years_not_in_lf" %in% names(ext)) {
  olf <- clean_num(ext$years_not_in_lf)
  x <- data.frame(group = groupings[["lifetime parenthood (ever a parent in the panel)"]],
                  kept = ext$kept, olf = olf) %>%
    group_by(group, kept) %>%
    summarise(mean_years_out_of_lf = round(mean(olf, na.rm = TRUE), 1),
              person_waves = dplyr::n(), .groups = "drop") %>% as.data.frame()
  cat("\n  years out of the labour force, removed against kept, lifetime groups:\n")
  print(x, row.names = FALSE)
  rm(olf, x)
}

# =============================================================================
# WHERE: THE REMOVED ROWS AGAINST THE ROSTER
# =============================================================================
coh <- g_all$cohort[match(ext$person_id, g_all$person_id)]
k   <- ifelse(!is.na(coh) & coh < 10000, clean_num(ext$wave) - coh, NA_real_)
band <- ifelse(is.na(k), "",
               ifelse(k <= -2, "k <= -2", ifelse(k == -1, "k = -1",
                      ifelse(k <= 4, "k = 0 to 4", ifelse(k <= 9, "k = 5 to 9", "k >= 10")))))
emp <- clean_num(ext$employed)
asked <- if ("training_module_asked" %in% names(ext))
  ifelse(clean_num(ext$training_module_asked) %in% 1, "asked", "not asked") else "not recorded"
LOC <- data.frame(why = ext$why, roster = st_row, sex = sexlab, event_time = band,
                  employed = ifelse(emp %in% 1, "employed", ifelse(is.na(emp), "missing", "not employed")),
                  asked = asked, stringsAsFactors = FALSE)[!ext$kept, , drop = FALSE] %>%
  count(why, roster, sex, event_time, employed, asked, name = "person_waves") %>%
  arrange(why, roster, sex, event_time, employed) %>% as.data.frame()

cat("\n", strrep("=", 78), "\nWHERE: the removed rows against the event-study roster\n",
    strrep("=", 78), "\n", sep = "")
x <- LOC %>% group_by(roster, sex) %>%
  summarise(removed = sum(person_waves),
            employed = sum(person_waves[employed == "employed"]), .groups = "drop") %>%
  as.data.frame()
x$share_of_removed <- round(100 * x$removed / sum(x$removed), 1)
print(x[order(-x$removed), ], row.names = FALSE)
.in <- sum(x$removed[x$roster %in% c("treated", "never-treated")])
cat(sprintf(paste0("\n  %s of the %s removed person-waves are inside the roster, where they could",
                   "\n  move an estimate; %s of those are employed, the rows the headline uses.\n"),
            .n(.in), .n(sum(x$removed)),
            .n(sum(x$employed[x$roster %in% c("treated", "never-treated")]))))
y <- LOC[LOC$roster == "treated", , drop = FALSE] %>% group_by(sex, event_time) %>%
  summarise(removed = sum(person_waves),
            employed = sum(person_waves[employed == "employed"]), .groups = "drop") %>%
  as.data.frame()
if (nrow(y)) {
  cat("\n  treated persons' removed rows, by event time:\n")
  print(y, row.names = FALSE)
}
rm(x, y, .in)

# =============================================================================
# THE ROSTER WITH AND WITHOUT THE SCREEN
# =============================================================================
.ros_tab <- function(cs, lab) {
  cs %>% mutate(sex = ifelse(female == 1, "women", "men"),
                status = ifelse(cohort < 10000, "treated", "never-treated")) %>%
    group_by(sex, status) %>%
    summarise(persons = n_distinct(person_id), person_waves = dplyr::n(),
              employed_person_waves = sum(employed %in% 1),
              headline_rows = sum(!is.na(jttpewt_emp)), .groups = "drop") %>%
    mutate(sample = lab) %>% as.data.frame()
}
ROSTER <- rbind(.ros_tab(cs_scr, "with the screen"), .ros_tab(cs_all, "without the screen"))
cat("\n", strrep("=", 78), "\nTHE ROSTER WITH AND WITHOUT THE SCREEN\n", strrep("=", 78), "\n", sep = "")
print(ROSTER[, c("sample", "sex", "status", "persons", "person_waves",
                 "employed_person_waves", "headline_rows")], row.names = FALSE)

# The people whose place in the design the screen changes. Dropping the birth
# wave moves the cohort to the next kept wave, and dropping every pre-birth
# wave makes a parent left-censored.
ch <- merge(g_all[, c("person_id", "cohort", "status")],
            g_scr[, c("person_id", "cohort", "status")],
            by = "person_id", all.x = TRUE, suffixes = c("_without", "_with"))
ch$change <- with(ch, ifelse(is.na(status_with),
                             paste0("every row removed by the screen (", status_without, ")"),
                      ifelse(status_without != status_with,
                             paste0(status_without, " without the screen, ", status_with, " with it"),
                             ifelse(status_without == "treated" & cohort_without != cohort_with,
                                    "treated either way, with a different birth wave",
                                    "unchanged"))))
fem_p <- clean_num(ext$female)[match(ch$person_id, ext$person_id)]
ch$sex <- ifelse(fem_p %in% 1, "women", ifelse(fem_p %in% 0, "men", "sex missing"))
CHANGES <- ch[ch$change != "unchanged", , drop = FALSE] %>%
  count(change, sex, name = "persons") %>% as.data.frame()
if (nrow(CHANGES)) {
  cat("\n  people whose place in the design the screen changes:\n")
  print(CHANGES, row.names = FALSE)
} else cat("\n  the screen changes no one's place in the design.\n")
rm(ch, fem_p)

# =============================================================================
# HOW MUCH: THE SUN-ABRAHAM AVERAGES WITH AND WITHOUT THE SCREEN
# =============================================================================
# CS_event_study.R's fit(): endpoint bins at |k| >= K where fixest accepts
# them, the unbinned estimator where it does not, which fixest 0.14 always
# does. Person and wave effects, clustered on the person.
BIN <- stats::setNames(list((-200):(-K), K:200), c(as.character(-K), as.character(K)))
.fit <- function(d, y, fe = "id + wave") {
  m <- tryCatch(feols(stats::as.formula(paste0(y, " ~ sunab(cohort, wave, bin.rel = BIN) | ", fe)),
                      data = d, cluster = ~id, notes = FALSE, warn = FALSE),
                error = function(e) NULL)
  binned <- !is.null(m)
  if (!binned)
    m <- feols(stats::as.formula(paste0(y, " ~ sunab(cohort, wave) | ", fe)),
               data = d, cluster = ~id, notes = FALSE, warn = FALSE)
  structure(list(m = m), binned = binned)
}
.att <- function(m) tryCatch({
  a <- summary(m, agg = "att")$coeftable
  unname(a[grep("ATT|att", rownames(a))[1], 1:2]) }, error = function(e) c(NA_real_, NA_real_))
OUTCOMES <- list(
  jttpewt_emp = list(y = "jttpewt_emp", fe = "id + wave",
                     label = "Employer-financed training, employed only (the headline)"),
  jttpewt_any = list(y = "jttpewt_any", fe = "id + wave",
                     label = "Employer-financed training, unconditional"),
  employed    = list(y = "employed", fe = "id + wave", label = "Employed"),
  ln_hourly_wage_real = list(y = "ln_hourly_wage_real", fe = "id + wave",
                             label = "Log hourly wage"),
  ln_hourly_wage_real_agefe = list(y = "ln_hourly_wage_real", fe = "id + wave + age",
                                   label = "Log hourly wage, age fixed effects"),
  experience_years = list(y = "experience_years", fe = "id + wave",
                          label = "Years of experience"))
pub <- { .p <- .find("cs_post_period_att.csv")
  if (is.na(.p)) NULL else utils::read.csv(.p, stringsAsFactors = FALSE) }
.pub <- function(sex, outcome, col) {
  if (is.null(pub)) return(NA_real_)
  v <- pub[[col]][pub$sex == sex & pub$outcome == outcome]
  if (length(v)) v[1] else NA_real_
}
# WHICH ESTIMATOR CS PUBLISHED. A gap_separate row means CS carries the
# common-weight patch: gap is then the common-weight average, the paper's
# estimator, and gap_separate the earlier one. Without it, gap is the
# separate-weight average.
CS_CW <- !is.null(pub) && any(pub$sex == "gap_separate")
EST_LABEL <- c(common   = if (CS_CW) "common weights, the paper's estimator" else
                 "common weights (CS publishes no common-weight rows yet)",
               separate = if (CS_CW) "separate weights, the estimator of earlier drafts" else
                 "separate weights, the estimator CS published")

cat("\n", strrep("=", 78), "\nHOW MUCH: Sun-Abraham post-birth averages, women less men\n",
    strrep("=", 78), "\n", sep = "")
cat("  CS_event_study.R published ",
    if (CS_CW) "common-weight gap rows, with the separate weights as gap_separate" else
      "separate-weight gap rows (no common-weight patch)", ".\n", sep = "")
if (CS_CW && !HAVE_CW)
  cat("  *** sa_common_weights.R is not beside the scripts, so the paper's estimator\n",
      "      cannot be formed or reproduced here. Put it in the project root.\n", sep = "")
# Both sexes fitted once; both estimators formed from the same two fits. The
# cells are read inside this function, where the data the fit used is in scope.
.one <- function(cs, o) {
  d <- cs
  if (grepl("age", o$fe)) d <- d[!is.na(d$age), , drop = FALSE]
  r <- list(); cells <- list()
  for (sx in c("women", "men")) {
    f <- .fit(d[d$female == (sx == "women"), , drop = FALSE], o$y, o$fe)
    r[[sx]] <- c(.att(f$m), n = stats::nobs(f$m), binned = attr(f, "binned"))
    if (HAVE_CW)
      cells[[sx]] <- tryCatch(sa_cells(f$m), error = function(e) {
        cat("    sa_cells() failed for ", sx, ", ", o$y, ": ", conditionMessage(e),
            "\n", sep = "")
        NULL })
  }
  cg <- if (HAVE_CW && !is.null(cells$women) && !is.null(cells$men))
    tryCatch(sa_common_gap(cells$women, cells$men), error = function(e) {
      cat("    sa_common_gap() failed for ", o$y, ": ", conditionMessage(e), "\n", sep = "")
      NULL }) else NULL
  list(separate = c(gap = unname(r$women[1] - r$men[1]),
                    se = unname(sqrt(r$women[2]^2 + r$men[2]^2)),
                    att_w = unname(r$women[1]), att_m = unname(r$men[1])),
       common = if (is.null(cg)) NULL else
         c(gap = unname(cg$post[["est"]]), se = unname(cg$post[["se"]]),
           att_w = unname(cg$post_w), att_m = unname(cg$post_m)),
       n = unname(r$women["n"] + r$men["n"]),
       binned = isTRUE(as.logical(r$women["binned"])) &&
         isTRUE(as.logical(r$men["binned"])))
}
.ok <- function(v, se, p, ps) is.finite(p) && is.finite(v) && abs(v - p) <= SA_TOL &&
  is.finite(ps) && is.finite(se) && abs(se - ps) <= SA_TOL
SA <- list(); ALL_OK <- !(CS_CW && !HAVE_CW)
for (nm in names(OUTCOMES)) {
  o <- OUTCOMES[[nm]]
  if (!(o$y %in% names(cs_all)) || (grepl("age", o$fe) && !("age" %in% names(cs_all)))) next
  w  <- .one(cs_scr, o)
  wo <- .one(cs_all, o)
  wr <- if (RULE_ON) w else wo
  cat("\n  ", o$label, "\n", sep = "")
  for (est in c("common", "separate")) {
    a <- w[[est]]; b <- wo[[est]]
    if (is.null(a) || is.null(b)) next
    # The published row for this estimator, if CS published one.
    prow <- if (est == "common") (if (CS_CW) "gap" else NA_character_) else
      (if (CS_CW) "gap_separate" else "gap")
    pg <- if (is.na(prow)) NA_real_ else .pub(prow, nm, "att")
    ps <- if (is.na(prow)) NA_real_ else .pub(prow, nm, "se")
    rep_ok <- if (is.na(prow)) NA else .ok(wr[[est]][["gap"]], wr[[est]][["se"]], pg, ps)
    if (isFALSE(rep_ok)) ALL_OK <- FALSE
    rep_tag <- if (is.na(rep_ok)) "" else if (rep_ok) ", reproduces the published value" else
      sprintf(", *** published %s: NOT REPRODUCED",
              if (is.finite(pg)) sprintf("%+.4f (SE %.4f)", pg, ps) else "value missing")
    # The estimator whose average CS published as the gap row.
    pub_est <- (est == "common") == CS_CW
    SA[[paste(nm, est)]] <- data.frame(
      outcome = nm, label = o$label, weights = est, published_estimator = pub_est,
      published = pg, published_se = ps,
      with_screen = a[["gap"]], with_se = a[["se"]], with_n = w$n,
      reproduced = rep_ok, rule_in_force = if (RULE_ON) "screen" else "none",
      without_screen = b[["gap"]], without_se = b[["se"]], without_n = wo$n,
      change = b[["gap"]] - a[["gap"]],
      change_in_se = (b[["gap"]] - a[["gap"]]) / a[["se"]],
      women_with = a[["att_w"]], women_without = b[["att_w"]],
      men_with = a[["att_m"]], men_without = b[["att_m"]],
      binned = w$binned, stringsAsFactors = FALSE)
    cat(sprintf(paste0("    %s\n      with the screen    %+.4f (SE %.4f), %s rows%s\n",
                       "      without the screen %+.4f (SE %.4f), %s rows%s\n",
                       "      change %+.4f, or %+.2f of its standard error\n"),
                EST_LABEL[[est]], a[["gap"]], a[["se"]], .n(w$n),
                if (RULE_ON) rep_tag else "",
                b[["gap"]], b[["se"]], .n(wo$n),
                if (RULE_ON) "" else rep_tag,
                b[["gap"]] - a[["gap"]], (b[["gap"]] - a[["gap"]]) / a[["se"]]))
  }
}
SA <- do.call(rbind, SA)
if (!is.null(SA) && nrow(SA)) {
  rownames(SA) <- NULL
  if (ALL_OK && any(SA$reproduced %in% TRUE))
    cat("\n  every fit on the rule in force reproduces the published value.\n")
  else
    cat("\n  *** Not every fit on the rule in force reproduces CS_event_study.R. The\n",
        "      comparison for those rows is not on the paper's specification; rerun\n",
        "      CS first, under the same TD_SCREEN and with the same weights patch.\n",
        sep = "")
  .pp <- SA[SA$published_estimator, , drop = FALSE]
  if (nrow(.pp))
    cat(sprintf(paste0("  on the estimator CS published (%s weights), the largest move is",
                       " %.2f of a standard error (%s).\n"),
                .pp$weights[1], max(abs(.pp$change_in_se)),
                .pp$outcome[which.max(abs(.pp$change_in_se))]))
  rm(.pp)
}
if (!is.null(SA) && any(!SA$binned))
  cat("\n  The fits are unbinned, as CS_event_study.R's are under fixest ",
      as.character(utils::packageVersion("fixest")), ".\n", sep = "")

# =============================================================================
# WRITE
# =============================================================================
GROUPS$build_tag <- BT; LOC$build_tag <- BT
ROSTER_OUT <- rbind(
  data.frame(table = "roster", ROSTER, change = "", stringsAsFactors = FALSE),
  if (nrow(CHANGES))
    data.frame(table = "status change", sample = "", sex = CHANGES$sex, status = "",
               persons = CHANGES$persons, person_waves = NA, employed_person_waves = NA,
               headline_rows = NA, change = CHANGES$change, stringsAsFactors = FALSE))
utils::write.csv(GROUPS, OUT("screen_audit_groups.csv"), row.names = FALSE)
utils::write.csv(LOC, OUT("screen_audit_location.csv"), row.names = FALSE)
ROSTER_OUT <- ROSTER_OUT[, c("table", "sample", "sex", "status", "persons", "person_waves",
                             "employed_person_waves", "headline_rows", "change")]
utils::write.csv(ROSTER_OUT, OUT("screen_audit_roster.csv"), row.names = FALSE)
if (!is.null(SA)) {
  SA$build_tag <- BT
  utils::write.csv(SA, OUT("screen_audit_sa.csv"), row.names = FALSE)
}
cat("\n  wrote screen_audit_groups.csv, screen_audit_location.csv, screen_audit_roster.csv",
    if (!is.null(SA)) " and screen_audit_sa.csv", "\n  to ", DIAG, "\n", sep = "")
if (isFALSE(FRAME_OK) || isFALSE(ROSTER_OK))
  cat("  *** Read the checks at the top first: the rows here are not the\n",
      "      contract's frame or CS's roster, so these tables describe another sample.\n", sep = "")
.sa_done <<- TRUE

}), finally = {
  options(.sa_width)
  if (isTRUE(.sa_log) && exists("td_log_close", mode = "function"))
    try(td_log_close(if (isTRUE(.sa_done)) "completed" else "stopped"), silent = TRUE)
  rm(list = intersect(c(".sa_log", ".sa_done", ".sa_width"), ls(all.names = TRUE)))
})
