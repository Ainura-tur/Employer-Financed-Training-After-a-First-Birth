# =====================================================================
# PRE_ANALYSIS_DIAGNOSTICS.R
# Everything computable from the v6 panel BEFORE any estimation.
#
# Produces, for BOTH age windows (20-55 and 30-49) side by side:
#   Item 1  Table 1 descriptives by absorbing group        -> tex
#   Item 1  Table 2 within-couple outcomes by parenthood   -> tex
#   Item 2  Couple counts, dual-earner counts, SD of S_ct
#   Item 3  Estimation-sample counts (tab:samples)         -> tex
#   Item 4  Introduction figures (panel size, contamination)
#   Item 11 Left-censored parents: count and characterisation
#   Item 12 First-birth validation against the life-event item
#   Item 14 Firm-size discontinuity by wave
#   Item 15 Event-time cell counts (panel-length contribution)
#
# Input : hilda_panel_data_W12_W24_slim.rds  (v21.43b or later)
# Output: console log + three .tex fragments + one .csv of headline numbers
#
# ---------------------------------------------------------------------
# v7 CORRECTION. THIS SCRIPT DID NOT RUN, AND WHEN IT LAST RAN IT WAS
# READING THE WRONG ITEM. Both are fixed here and neither was cosmetic.
#
# IT DID NOT RUN. The guard below required had_birth_this_wave. The
# loader deleted that column at v17 and never recreates it: it appears
# once in LOADING_ND3_v3.R, inside DEPRECATED_V17, and is never assigned.
# So this script loaded the panel, printed its dimensions, and stopped
# two lines later. pre_analysis_output/headline_figures.csv is therefore
# a pre-v17 artefact and its 87.9 and 88.3 are not a recomputation.
#
# IT READ THE PREGNANCY ITEM. had_birth_this_wave came from leprg. The
# loader states this twice and without hedging: "event_birth was leprg,
# the PREGNANCY item, for sixteen versions", and "event_birth was leprg
# (pregnancy), never lebth (birth). Any script reading the old names is
# reading a pregnancy clock." ITEM 12's own comment named leprg while
# calling it the birth of a new child, and the manuscript inherited it:
# Section 4.5 validates the absorbing indicator against a pregnancy
# report and says it uses the birth item.
#
# WHY THE FIX IS NOT A RENAME. leprg fires in two consecutive waves for
# a first birth, because it asks about the last twelve months; lebth
# fires once. 03a measures it: 64.9 per cent of treated women report the
# pregnancy item at k = -1 and 79.8 at k = 0. A validation built on an
# item that fires early is diluted at the transition wave, and widening
# by one wave cannot add much because the early firing is already inside
# the contemporaneous measure. That is the whole of footnote 5's
# argument, arriving as a property of the item the footnote says it did
# not use.
#
# SO ITEM 12 NOW COMPUTES BOTH and prints them side by side rather than
# swapping one column for the other and asking the reader to trust it.
# The birth item is what Section 4.5 should quote, the pregnancy item is
# what it quoted, and the difference between them is what footnote 5 is
# about. Reporting one without the other would leave the same claim
# resting on an unexamined choice, which is how this started.
#
# ALSO FIXED HERE, because both bear on the numbers:
#   - the panel is resolved and SCREENED, so the transition count is the
#     2,704 the roster reports rather than 2,710 on the raw extract
#   - 00compat.R is sourced, so the deprecation is enforced rather than
#     advisory
#   - PANEL and OUTDIR were bare relative strings; both are resolved
# ---------------------------------------------------------------------
#
# The point of running both windows is to make the choice on evidence:
# 20-55 buys the long post-birth horizons, 30-49 matches the current
# manuscript. Item 15 quantifies exactly what the narrow window costs.
# =====================================================================

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

library(dplyr)
library(tidyr)

# --- Guards FIRST, then paths -----------------------------------------
# v7.2. THE OUTPUT WENT SOMEWHERE ELSE, and the reason is a name collision
# I caused. v7.1 set OUTDIR, then sourced 00compat.R, and the contract
# behind it seals its own OUTDIR along with DIR_DATA, DIR_DESIGNS and
# DIR_TABLES. So the contract overwrote the setting and five .tex
# fragments and a .csv landed in output/designs among fifty other files.
#
# Two changes, and the second is the one that lasts. The guards are
# sourced BEFORE any path is set, so the contract cannot arrive halfway
# through and silently win. And the destination is called PRE_DIR, a name
# nothing else in the tree owns, so the collision cannot recur when the
# contract gains another sealed name.
.here  <- getwd()
.roots <- unique(c(.here, normalizePath(file.path(.here, ".."),
                                        mustWork = FALSE)))

.compat <- local({
  for (d in .roots) {
    p <- file.path(d, "00compat.R")
    if (file.exists(p)) return(p)
  }
  NA_character_
})
if (!is.na(.compat)) {
  source(.compat); cat("Guards      :", .compat, "\n")
} else {
  cat("*** 00compat.R NOT FOUND. The pregnancy-versus-birth deprecation\n",
      "    is unenforced in this run. Do not quote ITEM 12 from it.\n",
      sep = "")
}

# --- THIS RUN'S OWN TRANSCRIPT ----------------------------------------
# v8. IT WROTE NO LOG, AND EVERY OTHER STAGE DOES.
#
# The whole point of run_logs is that a set of them from one session is the
# only evidence the numbers in the paper came from one pass. This script
# supplies headline_figures.csv to paper_tables.R and the birth-item
# validation Section 4.5 quotes, and it was the one contributor whose run
# left nothing behind. Its output existed only in whatever console the
# reader happened to have open, which is a record that survives until the
# next scroll.
#
# AFTER THE GUARDS, NOT BEFORE. td_log_open() lives in 00_contract.R, which
# 00compat.R sources at its line 56, so the function does not exist until
# the block above has run. Opening here also puts the output-directory
# decision and the panel resolution inside the log, which are the two things
# a reader of an old log most often needs.
#
# TRIED, NOT REQUIRED. A session that can supply the panel but has no logger
# should still produce its tables rather than stop over a bookkeeping call.
# Same reasoning as 04financing_space.R at its line 219, and the same
# try(): this script is sourced by hand more often than any other here.
.pa_sink_out <- sink.number()
.pa_sink_msg <- sink.number(type = "message")
if (exists("td_log_open", mode = "function"))
  try(td_log_open("Pre_analysis_diagnostics2.R"), silent = TRUE) else
    cat("*** NO LOGGER. td_log_open() is not defined, so this run leaves no\n",
        "    transcript. Source 00_contract.R first if you want one.\n",
        sep = "")

# --- ONE FOLDER for everything this script writes ---------------------
# Precedence, and it is printed so the answer is never inferred from
# where the files turn up:
#   1. PRE_DIR set by the caller before sourcing, which always wins
#   2. the contract's DIR_DIAG, which is output/diagnostics
#   3. <working directory>/output/diagnostics, if no contract is loaded
#
# v8. THE DEFAULT IS NO LONGER DIR_TABLES, and the reason is the warning
# forty lines below rather than anything about paths. This script emits six
# .tex fragments across two age windows and exactly one window's worth is
# ever kept; that is what a triage step is. Sent to the folder the
# manuscript \input{}s, they sat beside the fragments it actually draws, and
# two files both answering to Table 4 disagreed on the never-mother
# boundary with nothing in the tree to say which one the paper takes.
#
# A WARNING WAS THE WRONG INSTRUMENT. v7.3 added one and it fires correctly
# on every run, which is the problem: it tells a reader that the collision
# exists and leaves them to act on it, every time, forever. Writing
# somewhere else removes the collision instead. A fragment in
# output/diagnostics cannot be \input{} by accident, and one that turns out
# to be a deliverable is promoted deliberately, which is the decision this
# script exists to prompt.
PRE_DIR <- if (exists("PRE_DIR", inherits = FALSE) &&
               is.character(PRE_DIR) && nzchar(PRE_DIR)) {
  cat("Output dir  : caller-supplied PRE_DIR\n"); PRE_DIR
} else if (exists("DIR_DIAG") && is.character(DIR_DIAG) &&
           nzchar(DIR_DIAG)) {
  cat("Output dir  : the contract's DIR_DIAG\n"); DIR_DIAG
} else {
  cat("Output dir  : no contract; falling back beside the working directory\n")
  file.path(.here, "output", "diagnostics")
}
dir.create(PRE_DIR, showWarnings = FALSE, recursive = TRUE)
cat("Writing to  :", PRE_DIR, "\n")

# THE CSV IS NOT A FRAGMENT AND DOES NOT GO WITH THEM. headline_figures.csv
# is the one thing here that later stages read: it carries the transition
# counts, the couple statistics and the birth-item validation, and the
# manuscript's Section 4.5 rests on the last of those. Data goes where the
# project keeps data. Only the .tex fragments are triage.
PRE_CSV_DIR <- if (exists("DIR_DESIGNS") && is.character(DIR_DESIGNS) &&
                   nzchar(DIR_DESIGNS)) DIR_DESIGNS else PRE_DIR
dir.create(PRE_CSV_DIR, showWarnings = FALSE, recursive = TRUE)
cat("headline csv:", PRE_CSV_DIR, "\n")

# --- The panel, and a warning if there is more than one ---------------
# v7.2. The last run resolved the panel to the copy in the working
# directory while the loader writes to the project root. Two copies of a
# 30 MB file diverge the moment one of them is rebuilt, and nothing here
# would notice. Every candidate is listed with its date and size, so a
# stale one is visible rather than silently preferred.
.cands <- Filter(file.exists,
                 file.path(.roots, "hilda_panel_data_W12_W24_slim.rds"))
if (!length(.cands))
  stop("hilda_panel_data_W12_W24_slim.rds not found in ",
       paste(.roots, collapse = " or "))
if (length(.cands) > 1L) {
  cat("*** MORE THAN ONE PANEL FILE. Reading the first; check they agree:\n")
  for (p in .cands) {
    fi <- file.info(p)
    cat(sprintf("      %s\n        %s, %.1f MB\n", p,
                format(fi$mtime, "%Y-%m-%d %H:%M"), fi$size / 1048576))
  }
}
PANEL <- .cands[1]
cat("Panel file  :", PANEL, "\n")
panel_raw <- readRDS(PANEL)

# --- The sample rule, the contract's rather than this script's -----------
# v9, 14 October. NO SCREEN BY DEFAULT. This block applied age minus
# experience between ten and thirty-five by hand. The contract now applies
# no screen unless TD_SCREEN=on, so the rule is taken from it, and the hand
# version below follows the same setting for a run without the contract.
# ITEM 12's transitions are therefore on the whole extract, as every other
# number in the paper now is.
.screen <- function(d) {
  if (exists("td_apply_sample_rule", mode = "function"))
    return(td_apply_sample_rule(d))
  if (!identical(tolower(trimws(Sys.getenv("TD_SCREEN", "off"))), "on")) {
    cat("Screen      : none (TD_SCREEN is off);",
        format(nrow(d), big.mark = ","), "rows kept\n")
    return(d)
  }
  if (!all(c("age", "experience_years") %in% names(d))) {
    cat("*** age or experience_years absent; screen NOT applied.\n")
    return(d)
  }
  lo   <- suppressWarnings(as.numeric(Sys.getenv("TD_SAMPLE_LO", "")))
  hi   <- suppressWarnings(as.numeric(Sys.getenv("TD_SAMPLE_HI", "")))
  if (is.na(lo)) lo <- 10
  if (is.na(hi)) hi <- 35
  gap  <- d$age - d$experience_years
  keep <- !is.na(gap) & gap >= lo & gap <= hi
  cat(sprintf("Screen      : %s of %s rows retained (%s removed)\n",
              format(sum(keep), big.mark = ","),
              format(nrow(d), big.mark = ","),
              format(sum(!keep), big.mark = ",")))
  d[keep, , drop = FALSE]
}
panel <- .screen(panel_raw)

# --- WHICH group_ever IS THIS? ----------------------------------------
# THE COLUMN HAS TWO MEANINGS AND THE SCRIPT CANNOT TELL WHICH IT GOT
# WITHOUT LOOKING, so it looks and says so in the log.
#
#   As the loader writes it, group_ever is derived from ever_parent and is
#   WAVE-VARYING: 0 before a first birth, 1 from the birth wave onward. It
#   is absorbing, meaning it never switches back, which is not the same as
#   constant within a person. A cut on it puts a mother's pre-birth waves
#   in the never-mothers arm and her post-birth waves in the mothers arm.
#
#   MASTER_training.R OVERWRITES it before any analysis, with a
#   person-level version built from max(ever_parent) and
#   total_children_ever_had. Under the master the arms partition persons;
#   read from the panel file directly they partition person-waves.
#
# So the same column name carries different semantics depending on how the
# script was invoked, and a number computed under one regime is not
# comparable with the same number computed under the other. Nothing here
# changes either way: the regime is detected, printed, and left alone, so
# the log records which one produced its numbers.
if ("group_ever" %in% names(panel) && "person_id" %in% names(panel)) {
  .ge_sw <- tapply(as.character(panel$group_ever), panel$person_id,
                   function(x) length(unique(x[!is.na(x)])))
  .ge_n  <- sum(.ge_sw > 1, na.rm = TRUE)
  cat(sprintf(paste0("\n  group_ever regime: %s. It changes value within ",
                     "%s of %s persons.\n"),
              if (.ge_n > 0) "WAVE-VARYING (loader)" else "PERSON-LEVEL (master)",
              format(.ge_n, big.mark = ","), format(length(.ge_sw), big.mark = ",")))
  if (.ge_n > 0) {
    cat("  The never-mothers arm therefore contains every mother-to-be's\n")
    cat("  pre-birth waves and the mothers arm only her post-birth ones, so\n")
    cat("  a mothers-against-never-mothers contrast is partly a before-and-\n")
    cat("  after of the same women. Comparisons BETWEEN the two childless\n")
    cat("  arms are unaffected, since both are contaminated the same way.\n")
  } else {
    cat("  The arms partition persons, so a person appears in exactly one.\n")
    cat("  Note this is the master's definition, which counts a parent via\n")
    cat("  total_children_ever_had as well as ever_parent and is therefore\n")
    cat("  BROADER than max(ever_parent) alone.\n")
  }
  rm(.ge_sw, .ge_n)
}
cat("  Table 1 below is built on these arms. The claim it carries -\n")
cat("  childless men and childless women alike, fathers no different,\n")
cat("  mothers apart - is a comparison BETWEEN the childless arms and\n")
cat("  survives either regime. The mothers-against-never-mothers rows are\n")
cat("  the ones to read with the regime in mind.\n")

cat("\n=====================================================================\n")
cat("PRE-ANALYSIS DIAGNOSTICS\n")
cat("Panel:", PANEL, "|", format(nrow(panel), big.mark = ","), "rows x",
    ncol(panel), "cols\n")
cat("=====================================================================\n")

# --- guard: fail loudly if the panel predates v6 ----------------------
needed <- c("in_age_30_49", "in_earnings_sample", "in_wage_sample",
            "years_not_in_lf", "years_since_education", "olf_share",
            "real_weekly_earnings", "real_annual_income", "fy_income_gross",
            "second_job_hours", "hours_worked_main_job", "hours_worked_clean",
            "group_ever", "group_3way_women", "ever_parent", "partner_id",
            "had_pregnancy_this_wave", "had_child_this_wave",
            "experience_is_proxy", "firm_size")
missing <- setdiff(needed, names(panel))
if (length(missing))
  stop("Panel is missing columns this script needs: ",
       paste(missing, collapse = ", "))

# The reverse check, which is the one that matters. A panel carrying a
# pre-v17 name is a panel this script must not read, because the name
# says birth while the column holds pregnancy.
.dead <- intersect(c("event_birth", "had_birth_this_wave"), names(panel))
if (length(.dead))
  stop("Panel carries pre-v17 name(s): ", paste(.dead, collapse = ", "),
       ". These were leprg (pregnancy), never lebth. Rebuild the panel.")
cat("Guard passed: required columns present, no pre-v17 names.\n")

# Two windows, run everything twice.
WINDOWS <- list(
  `20-55` = function(d) d,
  `30-49` = function(d) filter(d, in_age_30_49 == 1)
)

fmt  <- function(x) format(x, big.mark = ",", trim = TRUE)
rnd  <- function(x, k = 1) round(x, k)
# --- What else is in that folder --------------------------------------
# v7.3. Writing into the folder the manuscript inputs from is allowed and
# is what a single output directory means. It is not free, and the cost
# is named here rather than discovered later.
#
# THE COLLISION THAT MATTERS is Table 4. paper_tables.R writes
# descriptives_table.tex, which the manuscript inputs; this script writes
# tab1_descriptives_<window>.tex, which is the same table built from the
# same panel by a different split. The two disagree by roughly 5,421
# person-waves on the never-mother and post-mother boundary while
# agreeing to the digit on childless men, fathers and active mothers. So
# they are not two renderings of one number, they are two answers, and
# nothing in a LaTeX build would announce a swap between them.
#
# The other six files this script writes are two age windows of three
# tables, and only one window is ever kept. They are diagnostics that
# look like deliverables, sitting next to eight files that are
# deliverables. Listing them is the cheapest thing that prevents the
# wrong one being wired in.
.mine <- c(paste0("tab1_descriptives_", names(WINDOWS), ".tex"),
           paste0("tab2_couples_",      names(WINDOWS), ".tex"),
           paste0("tab_samples_",       names(WINDOWS), ".tex"))
.there <- setdiff(list.files(PRE_DIR, pattern = "\\.tex$"), .mine)
# v8. THE CHECK IS KEPT AND ITS MEANING HAS INVERTED. While PRE_DIR was
# DIR_TABLES this fired on every run and told the reader something they
# could not act on without moving the files by hand. Now that PRE_DIR is
# output/diagnostics, a stranger in this folder means either that a caller
# has pointed PRE_DIR back at a deliverables directory or that an older
# pass left fragments here under names this version no longer writes. Both
# are worth stopping for, and neither is normal.
if (length(.there)) {
  cat(sprintf("\n*** %d .tex file(s) in this folder are not written by this\n",
              length(.there)))
  cat("    script. output/diagnostics is meant to hold triage output and\n")
  cat("    nothing else, so one of two things has happened:\n")
  for (f in .there) cat("      ", f, "\n", sep = "")
  if ("descriptives_table.tex" %in% .there)
    cat("    descriptives_table.tex is Table 4 and so is\n",
        "    tab1_descriptives_<window>.tex below. Two answers, not two\n",
        "    renderings: they differ on the never-mother boundary. Keep the\n",
        "    manuscript on descriptives_table.tex until that is settled.\n",
        sep = "")
  cat("    Either PRE_DIR has been pointed at a deliverables folder, in\n")
  cat("    which case unset it and let the contract's DIR_DIAG win, or an\n")
  cat("    earlier pass left fragments here under names this version no\n")
  cat("    longer writes, in which case delete them.\n\n")
}

hdr  <- function(txt) {
  cat("\n---------------------------------------------------------------\n")
  cat(txt, "\n")
  cat("---------------------------------------------------------------\n")
}

headline <- list()   # collected for the CSV at the end


# =====================================================================
# ITEM 4 + 3: PANEL SIZE AND ESTIMATION-SAMPLE COUNTS
# =====================================================================
hdr("ITEM 3/4: Panel size and estimation-sample counts")

sample_counts <- lapply(names(WINDOWS), function(w) {
  d <- WINDOWS[[w]](panel)
  tibble::tibble(
    window  = w,
    sample  = c("Broad", "Wage sample", "Earnings sample",
                "Has interruption history"),
    pw      = c(nrow(d),
                sum(d$in_wage_sample == 1, na.rm = TRUE),
                sum(d$in_earnings_sample == 1, na.rm = TRUE),
                sum(!is.na(d$has_interruption))),
    persons = c(n_distinct(d$person_id),
                n_distinct(d$person_id[d$in_wage_sample == 1]),
                n_distinct(d$person_id[d$in_earnings_sample == 1]),
                n_distinct(d$person_id[!is.na(d$has_interruption)]))
  )
}) %>% bind_rows()

print(as.data.frame(sample_counts))

# The gain from the earnings design, by window: these are the person-waves
# a wage-based design discards, and they are disproportionately mothers.
gain <- sample_counts %>%
  filter(sample %in% c("Wage sample", "Earnings sample")) %>%
  select(window, sample, pw, persons) %>%
  pivot_wider(names_from = sample, values_from = c(pw, persons)) %>%
  mutate(gain_pw      = `pw_Earnings sample` - `pw_Wage sample`,
         gain_persons = `persons_Earnings sample` - `persons_Wage sample`)
cat("\nEarnings-design gain over the wage sample:\n")
print(as.data.frame(gain))


# =====================================================================
# ITEM 4: CONTAMINATION (post-mothers called childless)
# =====================================================================
hdr("ITEM 4: Contamination of the 'childless women' category")

contam <- lapply(names(WINDOWS), function(w) {
  d <- WINDOWS[[w]](panel) %>% filter(female == 1)
  # Standard definition: no children currently in household.
  std_childless <- filter(d, group_current == "childless_women")
  tibble::tibble(
    window            = w,
    n_std_childless   = nrow(std_childless),
    n_actually_parent = sum(std_childless$ever_parent == 1, na.rm = TRUE),
    pct_contaminated  = rnd(100 * mean(std_childless$ever_parent == 1,
                                       na.rm = TRUE), 1)
  )
}) %>% bind_rows()
print(as.data.frame(contam))
cat("\nManuscript currently states 24.5%. Update to the row matching the\n")
cat("window chosen for the main analysis.\n")


# =====================================================================
# ITEM 1: TABLE 1 -- DESCRIPTIVES BY ABSORBING GROUP (WAGE SAMPLE)
# =====================================================================
hdr("ITEM 1: Table 1 descriptives by absorbing motherhood group")

# Employment rate comes from the BROAD sample; everything else from the
# wage sample. Post-mothers are split out of mothers_ever for the last
# column, matching the manuscript layout.
build_tab1 <- function(d) {
  wage  <- filter(d, in_wage_sample == 1)
  broad <- d
  
  grp <- function(x, g) {
    if (g == "post_mothers") filter(x, group_3way_women == "post_mothers")
    else if (g == "active_mothers") filter(x, group_3way_women == "active_mothers")
    else filter(x, group_ever == g)
  }
  cols <- c("childless_men_ever", "fathers_ever", "never_mothers",
            "active_mothers", "post_mothers")
  
  out <- lapply(cols, function(g) {
    w <- grp(wage, g); b <- grp(broad, g)
    tibble::tibble(
      group      = g,
      n_obs      = nrow(w),
      n_persons  = n_distinct(w$person_id),
      mean_waves = rnd(nrow(w) / max(1, n_distinct(w$person_id)), 1),
      educ       = rnd(mean(w$educ_years, na.rm = TRUE), 1),
      exper      = rnd(mean(w$experience_years, na.rm = TRUE), 1),
      ln_wage    = rnd(mean(w$ln_hourly_wage_real, na.rm = TRUE), 2),
      pct_ft     = rnd(100 * mean(w$fulltime, na.rm = TRUE), 1),
      pct_emp    = rnd(100 * mean(b$employed, na.rm = TRUE), 1)
    )
  }) %>% bind_rows()
  out
}

tab1 <- lapply(names(WINDOWS), function(w) {
  build_tab1(WINDOWS[[w]](panel)) %>% mutate(window = w, .before = 1)
}) %>% bind_rows()
print(as.data.frame(tab1))

# Emit LaTeX for whichever window; write both, keep one.
emit_tab1 <- function(tt, w) {
  x <- filter(tt, window == w)
  g <- function(col) x[[col]]
  lines <- c(
    "\\begin{table}[htbp]", "\\centering",
    "\\caption{Descriptive Statistics by Absorbing Motherhood Group (Wage Sample)}",
    "\\label{tab:panel_descriptives}", "\\begin{threeparttable}", "\\small",
    "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}lccccc}", "\\toprule",
    "& Childless & Fathers & Never- & Active & Post- \\\\",
    "& men & (ever) & mothers & mothers & mothers \\\\", "\\midrule",
    paste0("\\textit{N (obs)}      & ", paste(fmt(g("n_obs")), collapse = " & "), " \\\\"),
    paste0("\\textit{N (persons)}  & ", paste(fmt(g("n_persons")), collapse = " & "), " \\\\"),
    paste0("\\textit{Mean waves}   & ", paste(g("mean_waves"), collapse = " & "), " \\\\[6pt]"),
    paste0("Education (yrs)       & ", paste(g("educ"), collapse = " & "), " \\\\"),
    paste0("Experience (yrs)      & ", paste(g("exper"), collapse = " & "), " \\\\"),
    paste0("Ln real wage          & ", paste(sprintf("%.2f", g("ln_wage")), collapse = " & "), " \\\\"),
    paste0("Full-time (\\%)        & ", paste(g("pct_ft"), collapse = " & "), " \\\\"),
    paste0("Employed (\\%)         & ", paste(g("pct_emp"), collapse = " & "), " \\\\"),
    "\\bottomrule", "\\end{tabular*}", "\\begin{tablenotes}[flushleft]", "\\small",
    paste0("\\item \\textit{Notes}: HILDA Waves 12--24, ages ", w,
           ". ``Ever'' groups use the absorbing definition. Active mothers: ",
           "currently have dependent children. Post-mothers: ever-parent but no ",
           "dependent children currently. Employment rates are from the broad ",
           "sample; all other rows are from the wage sample. Ln real wage is log ",
           "hourly wage deflated by the Australian Wage Price Index, in Wave~24 ",
           "(2024) dollars. Means and sample sizes are unweighted."),
    "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
  writeLines(lines, file.path(PRE_DIR, paste0("tab1_descriptives_", w, ".tex")))
}
invisible(lapply(names(WINDOWS), function(w) emit_tab1(tab1, w)))
cat("\nWrote tab1_descriptives_{20-55,30-49}.tex\n")


# =====================================================================
# ITEM 2: COUPLE MATCHING, COUPLE COUNTS, S_ct
# =====================================================================
hdr("ITEM 2: Couple counts, dual-earner counts, and SD of S_ct")

# Match partners within wave via the cross-wave partner id. A couple is
# keyed on the unordered pair of person ids so it is stable across waves
# even if the household id changes when they move.
build_couples <- function(d) {
  ppl <- d %>%
    filter(!is.na(partner_id), partner_id != "", !is.na(female)) %>%
    select(person_id, partner_id, wave, female, employed, in_wage_sample,
           hours_worked_clean, real_hourly_wage, fulltime, ever_parent,
           analysis_weight)
  
  # Self-join on (partner_id, wave) == (person_id, wave)
  cpl <- ppl %>%
    inner_join(ppl, by = c("partner_id" = "person_id", "wave" = "wave"),
               suffix = c("_self", "_ptnr")) %>%
    # keep heterosexual pairs only
    filter(female_self != female_ptnr) %>%
    # orient: him = male, her = female; each pair appears twice, keep one
    filter(female_self == 0) %>%
    mutate(
      couple_id = paste0(pmin(person_id, partner_id), "_",
                         pmax(person_id, partner_id)),
      his_hours = hours_worked_clean_self,
      her_hours = hours_worked_clean_ptnr,
      his_wage  = real_hourly_wage_self,
      her_wage  = real_hourly_wage_ptnr,
      his_emp   = employed_self,     her_emp   = employed_ptnr,
      his_ft    = fulltime_self,     her_ft    = fulltime_ptnr,
      dual      = as.integer(in_wage_sample_self == 1 & in_wage_sample_ptnr == 1),
      # Wave-specific parenthood status of the couple. Because ever_parent
      # is absorbing at the person level, this turns on at the couple's
      # first birth and stays on. A couple observed before and after that
      # point therefore contributes couple-waves to BOTH the childless and
      # the parent columns of Table 2, which is what the manuscript notes
      # already state. The couple COUNTS below consequently sum to more
      # than the number of distinct couples.
      parent    = pmax(ever_parent_self, ever_parent_ptnr, na.rm = TRUE),
      cpl_wt    = rowMeans(cbind(analysis_weight_self, analysis_weight_ptnr),
                           na.rm = TRUE),
      S_ct      = ifelse(!is.na(his_hours) & !is.na(her_hours) &
                           (his_hours + her_hours) > 0,
                         her_hours / (his_hours + her_hours), NA_real_)
    )
  cpl
}

couple_stats <- lapply(names(WINDOWS), function(w) {
  cpl <- build_couples(WINDOWS[[w]](panel))
  dual <- filter(cpl, dual == 1)
  
  # Mean within-couple SD of S_ct: SD computed within couple across waves,
  # then averaged over couples with 2+ observations.
  sd_within <- dual %>%
    filter(!is.na(S_ct)) %>%
    group_by(couple_id) %>%
    filter(n() >= 2) %>%
    summarise(sd_i = sd(S_ct), .groups = "drop") %>%
    summarise(m = mean(sd_i, na.rm = TRUE)) %>% pull(m)
  
  tibble::tibble(
    window            = w,
    couples           = n_distinct(cpl$couple_id),
    couple_waves      = nrow(cpl),
    dual_couples      = n_distinct(dual$couple_id),
    dual_couple_waves = nrow(dual),
    stacked_pw        = 2 * nrow(dual),
    mean_within_sd_S  = rnd(sd_within, 3),
    childless_couples = n_distinct(cpl$couple_id[cpl$parent == 0]),
    parent_couples    = n_distinct(cpl$couple_id[cpl$parent == 1])
  )
}) %>% bind_rows()
print(as.data.frame(couple_stats))
cat("\nManuscript states 6,048 couples / 68,083 couple-waves; dual-earner\n")
cat("4,662 couples / 42,147 couple-waves; SD of S_ct 0.063.\n")


# =====================================================================
# ITEM 1: TABLE 2 -- WITHIN-COUPLE OUTCOMES BY PARENTHOOD
# =====================================================================
hdr("ITEM 1: Table 2 within-couple outcomes by parenthood status")

build_tab2 <- function(d) {
  cpl <- build_couples(d)
  f <- function(sub) {
    tibble::tibble(
      his_wage  = weighted.mean(sub$his_wage,  sub$cpl_wt, na.rm = TRUE),
      her_wage  = weighted.mean(sub$her_wage,  sub$cpl_wt, na.rm = TRUE),
      his_hours = weighted.mean(sub$his_hours, sub$cpl_wt, na.rm = TRUE),
      her_hours = weighted.mean(sub$her_hours, sub$cpl_wt, na.rm = TRUE),
      his_emp   = 100 * weighted.mean(sub$his_emp, sub$cpl_wt, na.rm = TRUE),
      her_emp   = 100 * weighted.mean(sub$her_emp, sub$cpl_wt, na.rm = TRUE),
      his_ft    = 100 * weighted.mean(sub$his_ft,  sub$cpl_wt, na.rm = TRUE),
      her_ft    = 100 * weighted.mean(sub$her_ft,  sub$cpl_wt, na.rm = TRUE),
      n_couples = n_distinct(sub$couple_id),
      n_cw      = nrow(sub)
    )
  }
  bind_rows(f(filter(cpl, parent == 0)) %>% mutate(status = "childless", .before = 1),
            f(filter(cpl, parent == 1)) %>% mutate(status = "parent",    .before = 1))
}

tab2 <- lapply(names(WINDOWS), function(w) {
  build_tab2(WINDOWS[[w]](panel)) %>% mutate(window = w, .before = 1)
}) %>% bind_rows()
print(as.data.frame(tab2 %>% mutate(across(where(is.numeric), ~ rnd(.x, 1)))))

emit_tab2 <- function(tt, w) {
  x  <- filter(tt, window == w)
  cl <- filter(x, status == "childless"); pr <- filter(x, status == "parent")
  r <- function(lbl, hc, sc, hp, sp, k = 1) {
    sprintf("%s & %s & %s & %s & %s & %s & %s \\\\", lbl,
            rnd(hc, k), rnd(sc, k), sprintf("%+.1f", hc - sc),
            rnd(hp, k), rnd(sp, k), sprintf("%+.1f", hp - sp))
  }
  lines <- c(
    "\\begin{table}[htbp]", "\\centering",
    "\\caption{Within-Couple Labour Market Outcomes by Parenthood Status}",
    "\\label{tab:couple_descriptives}", "\\begin{threeparttable}", "\\small",
    "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}lcccccc}", "\\toprule",
    sprintf("& \\multicolumn{3}{c}{Childless couples ($N = %s$)} & \\multicolumn{3}{c}{Parent couples ($N = %s$)} \\\\",
            fmt(cl$n_couples), fmt(pr$n_couples)),
    "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}",
    "& His & Hers & Gap & His & Hers & Gap \\\\", "\\midrule",
    r("Hourly wage (\\$)", cl$his_wage, cl$her_wage, pr$his_wage, pr$her_wage),
    r("Weekly hours",      cl$his_hours, cl$her_hours, pr$his_hours, pr$her_hours),
    r("Employed (\\%)",    cl$his_emp, cl$her_emp, pr$his_emp, pr$her_emp),
    r("Full-time (\\%)",   cl$his_ft, cl$her_ft, pr$his_ft, pr$her_ft),
    "\\bottomrule", "\\end{tabular*}", "\\begin{tablenotes}[flushleft]",
    "\\small\\hsize\\linewidth",
    sprintf(paste0("\\item \\textit{Notes}: Matched heterosexual couples, HILDA ",
                   "Waves~12--24, ages %s. Childless: neither partner observed ",
                   "with a child yet (%s couples, %s couple-waves). Parent: post ",
                   "first birth under the absorbing definition (%s couples, %s ",
                   "couple-waves). Couples may contribute to both columns over ",
                   "time. Wages and hours are among employed partners; employment ",
                   "and full-time rates are computed over all couple-waves in each ",
                   "state. Hours are usual weekly hours across all jobs ",
                   "(\\texttt{jbhruc}). Means are weighted by the couple's HILDA ",
                   "cross-sectional weight (the average of the two partners' ",
                   "responding-person weights); sample sizes are unweighted."),
            w, fmt(cl$n_couples), fmt(cl$n_cw), fmt(pr$n_couples), fmt(pr$n_cw)),
    "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
  writeLines(lines, file.path(PRE_DIR, paste0("tab2_couples_", w, ".tex")))
}
invisible(lapply(names(WINDOWS), function(w) emit_tab2(tab2, w)))
cat("\nWrote tab2_couples_{20-55,30-49}.tex\n")


# =====================================================================
# ITEM 11: LEFT-CENSORED PARENTS
# =====================================================================
hdr("ITEM 11: Left-censored parents (already parents at first observation)")

# Three mutually exclusive types: never-treated (childless throughout),
# clean transitioners (observed 0 -> 1), and left-censored (ever_parent == 1
# in the first observed wave, so first birth predates panel entry).
censor <- lapply(names(WINDOWS), function(w) {
  d <- WINDOWS[[w]](panel) %>% filter(!is.na(ever_parent))
  per <- d %>%
    arrange(person_id, wave) %>%
    group_by(person_id) %>%
    summarise(female     = first(female),
              first_ep   = first(ever_parent),
              ever_ep    = max(ever_parent, na.rm = TRUE),
              n_waves    = n(),
              educ       = mean(educ_years, na.rm = TRUE),
              age_first  = first(age),
              exper      = mean(experience_years, na.rm = TRUE),
              olf        = mean(years_not_in_lf, na.rm = TRUE),
              .groups    = "drop") %>%
    mutate(type = case_when(first_ep == 1              ~ "left_censored",
                            first_ep == 0 & ever_ep == 1 ~ "transitioner",
                            TRUE                         ~ "never_treated"))
  per %>% group_by(female, type) %>%
    summarise(n_persons   = n(),
              pct         = rnd(100 * n() / nrow(per), 1),
              mean_waves  = rnd(mean(n_waves), 1),
              mean_age1   = rnd(mean(age_first, na.rm = TRUE), 1),
              mean_educ   = rnd(mean(educ, na.rm = TRUE), 1),
              mean_exper  = rnd(mean(exper, na.rm = TRUE), 1),
              mean_olf    = rnd(mean(olf, na.rm = TRUE), 2),
              .groups = "drop") %>%
    mutate(window = w, .before = 1)
}) %>% bind_rows()
print(as.data.frame(censor))
cat("\nLeft-censored parents are DROPPED from the event study. This table is\n")
cat("what the paper needs to characterise them: they are older at entry and\n")
cat("further through their careers, so the design selects toward LATER first\n")
cat("births and toward women attached to the panel before their first child.\n")


# =====================================================================
# ITEM 12: FIRST-BIRTH VALIDATION AGAINST THE LIFE-EVENT ITEM
# =====================================================================
hdr("ITEM 12: Does the absorbing transition coincide with a reported birth?")

# The absorbing indicator turns on when ANY child is reported in the
# household, which need not be a birth. Agreement with the life-event
# item is the validation.
#
# TWO ITEMS, BOTH REPORTED. lebth (SCQ B23e, birth or adoption of a new
# child) is the item this validation wants and is what Section 4.5
# should quote. leprg (SCQ B23d, pregnancy) is what the pre-v17 column
# held and therefore what the manuscript's 77.7 and 78.1 were computed
# on. Both are printed so the sentence and its footnote can be written
# from evidence instead of from an assumption about which column was
# read. If they agree, footnote 5 loses nothing; if they do not, the
# difference is the footnote's subject rather than an aside.
#
# WHAT THE WIDENED WINDOW MEANS DIFFERS BY ITEM, and this is the part
# footnote 5 turns on. A pregnancy for a first birth is reported in two
# consecutive waves, because the item asks about the last twelve months,
# so widening by one wave adds little on leprg for a mechanical reason.
# lebth fires once, so on the birth item a widened window is a genuine
# allowance for fieldwork timing. The same 0.4-point gain therefore
# means opposite things on the two items, which is why the denominators
# are printed beside the percentages rather than left implicit.
# v7.1. THE WIDENED MEASURE HELD ITS DENOMINATOR FIXED, and the first
# version did not. It used pmax(x, lag(x), na.rm = TRUE), which is non-NA
# whenever EITHER wave is observed, so the widened measure was computed
# on more rows than the contemporaneous one. The rows it added are
# transitions where the item is MISSING at the transition wave and
# present at the wave before, and on the birth item those enter as zeros,
# because a birth has not happened yet at k = -1. The widened figure came
# out BELOW the same-wave figure, 82.2 against 88.0, which a union cannot
# do. On the pregnancy item the same rows entered as ones, since leprg
# fires at k = -1 for about two thirds of treated women, and the widened
# figure rose 13.9 points. Both movements were the denominator, not the
# window.
#
# So the widened measure is now computed on exactly the rows where the
# contemporaneous item is observed, and a missing lag counts as no report.
# It can then only add, which is what a widened window is supposed to do,
# and the difference between the two columns is the window rather than
# a change of sample. n_lag_obs is printed so the reader can see how many
# of those rows actually had a previous wave to widen into.
.agree <- function(d, item, lbl) {
  if (!item %in% names(d))
    return(tibble::tibble(item = lbl, n_obs = NA_integer_,
                          pct_same = NA_real_, pct_window = NA_real_,
                          n_lag_obs = NA_integer_))
  d <- d %>% arrange(person_id, wave) %>% group_by(person_id) %>%
    mutate(.x = .data[[item]], .lagx = lag(.data[[item]])) %>%
    ungroup() %>% filter(transition == 1, !is.na(.x))
  win <- d$.x == 1 | (!is.na(d$.lagx) & d$.lagx == 1)
  tibble::tibble(
    item       = lbl,
    n_obs      = nrow(d),
    pct_same   = rnd(100 * mean(d$.x == 1), 1),
    pct_window = rnd(100 * mean(win), 1),
    n_lag_obs  = sum(!is.na(d$.lagx)))
}

birthval <- lapply(names(WINDOWS), function(w) {
  d <- WINDOWS[[w]](panel) %>%
    arrange(person_id, wave) %>%
    group_by(person_id) %>%
    mutate(prev_ep = lag(ever_parent),
           transition = as.integer(!is.na(prev_ep) & prev_ep == 0 &
                                     ever_parent == 1)) %>%
    ungroup()
  tr <- filter(d, transition == 1)
  b <- .agree(d, "had_child_this_wave",     "birth (lebth)")
  p <- .agree(d, "had_pregnancy_this_wave", "pregnancy (leprg)")
  tibble::tibble(
    window            = w,
    n_transitions     = nrow(tr),
    n_female          = sum(tr$female == 1, na.rm = TRUE),
    n_male            = sum(tr$female == 0, na.rm = TRUE),
    # The birth item carries the headline. Names are kept from the
    # previous version so headline_figures.csv stays readable against
    # older copies, but they now mean lebth and the suffix says so.
    n_obs             = b$n_obs,
    n_lag_obs         = b$n_lag_obs,
    pct_birth_same    = b$pct_same,
    pct_birth_window  = b$pct_window,
    n_obs_preg        = p$n_obs,
    pct_preg_same     = p$pct_same,
    pct_preg_window   = p$pct_window)
}) %>% bind_rows()
print(as.data.frame(birthval))

cat("\npct_birth_same is the figure Section 4.5 should quote: the share of\n")
cat("clean transitions coinciding with a BIRTH reported in the same wave,\n")
cat("from lebth. pct_preg_same is the same quantity on the pregnancy item,\n")
cat("which is what the pre-v17 column held and what the manuscript's 77.7\n")
cat("was computed on. Quote one, name it, and do not mix them.\n")

# The comparison is the point, so it is stated rather than left to the
# reader to difference two columns in a printed table.
for (i in seq_len(nrow(birthval))) {
  r <- birthval[i, ]
  cat(sprintf("\n  [%s] %s transitions, %s women and %s men\n", r$window,
              fmt(r$n_transitions), fmt(r$n_female), fmt(r$n_male)))
  cat(sprintf("    birth (lebth)    : %.1f%% same wave, %.1f%% widened  (n = %s, %s with a lag)\n",
              r$pct_birth_same, r$pct_birth_window, fmt(r$n_obs),
              fmt(r$n_lag_obs)))
  cat(sprintf("    pregnancy (leprg): %.1f%% same wave, %.1f%% widened  (n = %s)\n",
              r$pct_preg_same, r$pct_preg_window, fmt(r$n_obs_preg)))
  if (is.finite(r$pct_birth_same) && is.finite(r$pct_preg_same))
    cat(sprintf("    difference       : %+.1f points on the same-wave measure\n",
                r$pct_birth_same - r$pct_preg_same))
  if (is.finite(r$pct_birth_window) && is.finite(r$pct_birth_same))
    cat(sprintf("    widening the window buys %+.1f points on lebth and %+.1f on leprg\n",
                r$pct_birth_window - r$pct_birth_same,
                r$pct_preg_window - r$pct_preg_same))
}

cat("\nA high value on lebth supports the Data-section claim that the\n")
cat("transition is childbirth in the great majority of cases; a low value\n")
cat("means the absorbing indicator is picking up household-composition\n")
cat("changes and needs qualifying. Read the widened figure against the\n")
cat("item: on lebth it allows for fieldwork timing, on leprg it is\n")
cat("largely the same report arriving twice.\n")


# =====================================================================
# ITEM 14: FIRM-SIZE DISCONTINUITY
# =====================================================================
hdr("ITEM 14: Firm-size series break")

fs <- panel %>%
  filter(in_wage_sample == 1) %>%
  count(wave, firm_size) %>%
  group_by(wave) %>%
  mutate(pct = rnd(100 * n / sum(n), 1)) %>%
  select(-n) %>%
  pivot_wider(names_from = firm_size, values_from = pct, values_fill = 0)
print(as.data.frame(fs))
cat("\nCheck for a jump between waves 21 and 22. If present, do NOT use\n")
cat("firm_size as a control across the break without a wave interaction.\n")


# =====================================================================
# ITEM 15: EVENT-TIME CELL COUNTS -- WHAT A SHORT PANEL COSTS
# =====================================================================
hdr("ITEM 15: Event-time cell counts by panel length")

# For clean transitioners, count person-waves at each event time, and show
# what a 5-wave window would have recovered. This is the panel-length
# contribution: a short panel has no cells at the horizons where the wage
# penalty actually emerges.
event_cells <- lapply(names(WINDOWS), function(w) {
  d <- WINDOWS[[w]](panel) %>%
    arrange(person_id, wave) %>%
    group_by(person_id) %>%
    mutate(prev_ep = lag(ever_parent),
           tr_wave = ifelse(!is.na(prev_ep) & prev_ep == 0 & ever_parent == 1,
                            wave, NA_integer_)) %>%
    # First transition wave per person. Guard the all-NA case explicitly
    # rather than relying on min(NA, na.rm=TRUE) returning Inf with a
    # warning: never-treated and left-censored people have no transition.
    mutate(birth_wave = if (all(is.na(tr_wave))) NA_integer_
           else min(tr_wave, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(!is.na(birth_wave)) %>%
    mutate(k = wave - birth_wave)
  
  bind_rows(
    d %>% filter(k >= -8, k <= 10, in_wage_sample == 1) %>%
      count(female, k) %>% mutate(panel = "W12-W24 (13 waves)"),
    # A W12-W16 panel sees only births occurring inside it AND only
    # outcome waves inside it, so restrict on both.
    d %>% filter(k >= -8, k <= 10, in_wage_sample == 1,
                 wave <= 16, birth_wave <= 16) %>%
      count(female, k) %>% mutate(panel = "W12-W16 (5 waves)")
  ) %>% mutate(window = w, .before = 1)
}) %>% bind_rows()

for (w in names(WINDOWS)) {
  cat("\nWindow", w, "- wage-sample cells at each event time:\n")
  tb <- event_cells %>% filter(window == w) %>%
    pivot_wider(names_from = c(panel, female), values_from = n, values_fill = 0) %>%
    arrange(k)
  print(as.data.frame(tb))
}
cat("\nThe five-wave columns going to zero beyond k=+4 IS the panel-length\n")
cat("contribution: shorter panels in the literature cannot observe the\n")
cat("horizons at which the wage penalty emerges, only the flat early years.\n")


# =====================================================================
# EMIT tab:samples AND A HEADLINE CSV
# =====================================================================
hdr("Writing tab:samples and headline figures")

emit_samples <- function(w) {
  sc <- filter(sample_counts, window == w)
  cs <- filter(couple_stats,  window == w)
  g  <- function(s, col) sc[[col]][sc$sample == s]
  lines <- c(
    "\\begin{table}[htbp]", "\\centering",
    "\\caption{Estimation Samples by Test}", "\\label{tab:samples}",
    "\\begin{threeparttable}", "\\small",
    "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}llr}", "\\toprule",
    "Test & Unit and sample & $N$ \\\\", "\\midrule",
    sprintf("Earnings event study & Persons, earnings sample & %s pw / %s persons \\\\",
            fmt(g("Earnings sample", "pw")), fmt(g("Earnings sample", "persons"))),
    sprintf("Wage decomposition & Persons, wage sample & %s pw / %s persons \\\\",
            fmt(g("Wage sample", "pw")), fmt(g("Wage sample", "persons"))),
    sprintf("Interruption scarring & Persons with history & %s pw / %s persons \\\\",
            fmt(g("Has interruption history", "pw")),
            fmt(g("Has interruption history", "persons"))),
    sprintf("Static Becker & Couple-wave first differences & %s cw / %s couples \\\\",
            fmt(cs$couple_waves), fmt(cs$couples)),
    sprintf("Within-couple DML & Dual-earner couple-stacked & %s pw / %s couples \\\\",
            fmt(cs$stacked_pw), fmt(cs$dual_couples)),
    "\\bottomrule", "\\end{tabular*}", "\\begin{tablenotes}[flushleft]",
    "\\small\\hsize\\linewidth",
    sprintf(paste0("\\item \\textit{Notes}: ``pw'' denotes person-wave and ``cw'' ",
                   "couple-wave observations. HILDA Waves~12--24, ages %s. The ",
                   "earnings sample does not condition on employment; the wage ",
                   "sample does. Counts are unweighted."), w),
    "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
  writeLines(lines, file.path(PRE_DIR, paste0("tab_samples_", w, ".tex")))
}
invisible(lapply(names(WINDOWS), emit_samples))

headline_csv <- sample_counts %>%
  left_join(couple_stats, by = "window") %>%
  left_join(contam, by = "window") %>%
  left_join(birthval, by = "window")
write.csv(headline_csv, file.path(PRE_CSV_DIR, "headline_figures.csv"),
          row.names = FALSE)

cat("\nFragments written to", PRE_DIR, ":\n")
print(list.files(PRE_DIR))
cat("\nheadline_figures.csv written to", PRE_CSV_DIR, "\n")

cat("\n=====================================================================\n")
cat("DONE. Decide the age window from ITEM 15 (what the narrow window costs\n")
cat("at long horizons) against comparability with the existing literature,\n")
cat("then keep the matching .tex fragments and discard the others.\n")
cat("=====================================================================\n")

# --- close the transcript, and say so if it did not close cleanly -----
# A SCRIPT THAT LEAVES A SINK OPEN DISABLES LOGGING FOR EVERYTHING AFTER
# IT. verify_manuscript_numbers.R carries a whole block about reclaiming an
# orphaned sink, and the case it describes is a run that died with one open.
# Closing is therefore not optional bookkeeping, and the balance check below
# is what tells the next script whether it inherited a clean stack or a
# broken one. Checked before the close so the numbers compared are the ones
# this script is responsible for.
if (exists("td_log_close", mode = "function"))
  try(td_log_close(), silent = TRUE)
if (sink.number() != .pa_sink_out ||
    sink.number(type = "message") != .pa_sink_msg)
  warning("Pre_analysis_diagnostics2.R left the sink stack unbalanced ",
          "(output ", .pa_sink_out, " -> ", sink.number(), ", message ",
          .pa_sink_msg, " -> ", sink.number(type = "message"),
          "). Logging is dead from here on; td_log_rescue() restores it.",
          call. = FALSE)
rm(.pa_sink_out, .pa_sink_msg)