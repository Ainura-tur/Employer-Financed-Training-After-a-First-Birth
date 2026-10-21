# =============================================================================
# w24_calendar_check.R  -  does the HILDA calendar count leave as time in a job?
# -----------------------------------------------------------------------------
# WHY THIS EXISTS. The review-response analyses (R3 in 05_review_response.R)
# control for the share of the year in a job, fy_share_employed, built from
# _capj, and read it as exposure to the employer. That reading needs the
# calendar to count a spell of leave as time in a job. If it counted leave as
# time out of a job, the control would absorb part of the leave it is meant
# to sit beside.
#
# WHAT IT CHECKS. Wave 24 adds a paid parental leave calendar and three items
# on whether the person took parental leave: employer-paid, government-paid
# or unpaid. Among people who took any of the three, the per cent of the last
# financial year spent in a job (xcapj) should stay near 100. It should also
# stay high for people who report long spells of leave, counted as xalop
# (paid maternity, paternity, bereavement, family and carer's leave) plus
# xalup (unpaid leave). The log prints each item's label from the file, so
# this reading can be checked there rather than taken from this comment.
#
# WHAT THE PAPER QUOTES. 528 people took leave, their median xcapj is 100, and
# people with more than 180 days of leave average 96.5. The script compares
# its own results with these three numbers and says whether they match.
#
# TWO WINDOWS. xalop and xalup cover the twelve months before the interview,
# and xcapj covers the last financial year. The two overlap for most of the
# year but do not coincide, so part of a long spell can fall outside the year
# that xcapj measures.
#
# INPUT. The Wave 24 combined file, Combined_x240c.dta, from the dta_files
# folder the loader reads. To read it from elsewhere, set HILDA_W24_FILE or
# assign W24_FILE before sourcing. Unweighted, all respondents.
#
# OUTPUT. w24_calendar_check.csv in output/results and a log in run_logs,
# both through 00_contract.R when it loads. Without the contract the script
# runs on its own and writes to ./output/results.
# =============================================================================

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

# ---- the input file, resolved before any log opens ---------------------------
# The default is the loader's own folder: LOADING_ND3_v3.R sets project_dir to
# the HILDA_project folder below and reads file.path(project_dir, "dta_files").
# Two overrides let the path travel to another machine without a diff, as
# HILDA_ROOT does for the project root. A W24_FILE assigned before sourcing
# wins, then the HILDA_W24_FILE environment variable, then the default. A
# missing file stops here, so no log is left open behind the error.
W24_DEFAULT <- file.path("C:/Users/tur277/OneDrive - CSIRO/Documents - Copy",
                         "Y2025/HILDA_project/dta_files/Combined_x240c.dta")
W24_FILE <- if (exists("W24_FILE")) W24_FILE else
  Sys.getenv("HILDA_W24_FILE", unset = W24_DEFAULT)
if (!nzchar(W24_FILE) || !file.exists(W24_FILE))
  stop("Wave 24 file not found: '", W24_FILE, "'.\n  Set HILDA_W24_FILE to ",
       "the path of Combined_x240c.dta, or assign W24_FILE before sourcing.",
       call. = FALSE)

# ---- the contract, if it loads -----------------------------------------------
# The check needs nothing else from the pipeline. The contract supplies the
# log and the output folder, and a replicator without the full tree can still
# run this file on its own.
if (!exists("td_out", mode = "function") && file.exists("00_contract.R"))
  tryCatch(source("00_contract.R"), error = function(e)
    cat("  00_contract.R did not load: ", conditionMessage(e),
        "\n  Running standalone, with no log file.\n", sep = ""))
.w24_contract <- all(vapply(c("td_out", "td_log_open", "td_log_close"),
                            exists, logical(1), mode = "function"))
if (.w24_contract) td_log_open("w24_calendar_check.R")

# ---- read ----------------------------------------------------------------------
W24_VARS <- c("xcapj", "xalop", "xalup", "xalplemp", "xalplgov", "xalplupd")
w24 <- haven::read_dta(W24_FILE, col_select = dplyr::any_of(W24_VARS))
.miss <- setdiff(W24_VARS, names(w24))
if (length(.miss)) {
  if (.w24_contract) td_log_close("stopped")
  stop("Missing from ", basename(W24_FILE), ": ",
       paste(.miss, collapse = ", "), call. = FALSE)
}

cat("\n  items read from ", W24_FILE, ", ",
    format(nrow(w24), big.mark = ","), " respondents:\n", sep = "")
for (.v in W24_VARS) {
  .lab <- attr(w24[[.v]], "label")
  cat(sprintf("    %-9s %s\n", .v, if (is.null(.lab)) "(no label)" else .lab))
}

# HILDA codes "not asked", "not applicable", refusals and other nonresponse
# as negative values, so every negative value becomes missing.
w24 <- as.data.frame(lapply(haven::zap_labels(w24), function(v) {
  v <- as.numeric(v); v[v < 0] <- NA; v
}))

# xcapj is a per cent, so a value above 100 would be a code. The quoted
# numbers were computed without recoding, so this reports rather than recodes.
.over <- sum(w24$xcapj > 100, na.rm = TRUE)
if (.over)
  cat("  *** ", .over, " xcapj value(s) above 100. These are codes, not\n",
      "      per cents, and they are included in the numbers below.\n", sep = "")

# ---- leave ---------------------------------------------------------------------
# Days of leave in the last twelve months. Computed as in the run the paper
# quotes, where a person with both items missing counts as zero days. That
# affects only the zero-day row of the table, and the log says how many
# people it covers.
w24$leave_days <- rowSums(w24[, c("xalop", "xalup")], na.rm = TRUE)
.both_na <- sum(is.na(w24$xalop) & is.na(w24$xalup))
took <- with(w24, xalplemp %in% 1 | xalplgov %in% 1 | xalplupd %in% 1)

cat("\n  xcapj among people who took parental leave:\n")
print(summary(w24$xcapj[took]))

# ---- the table -----------------------------------------------------------------
bins <- cut(w24$leave_days, c(-1, 0, 30, 90, 180, 400))
.tab <- data.frame(
  group       = c("took parental leave", paste("leave days", levels(bins))),
  persons     = c(sum(took), as.integer(table(bins))),
  with_capj   = c(sum(!is.na(w24$xcapj[took])),
                  as.integer(tapply(!is.na(w24$xcapj), bins, sum))),
  mean_capj   = c(mean(w24$xcapj[took], na.rm = TRUE),
                  as.numeric(tapply(w24$xcapj, bins, mean, na.rm = TRUE))),
  median_capj = c(stats::median(w24$xcapj[took], na.rm = TRUE),
                  as.numeric(tapply(w24$xcapj, bins, stats::median,
                                    na.rm = TRUE))),
  stringsAsFactors = FALSE)
.tab$mean_capj   <- round(.tab$mean_capj, 1)
.tab$median_capj <- round(.tab$median_capj, 1)
cat("\n  per cent of the last financial year in a job, by group:\n")
print(.tab, row.names = FALSE)
cat("  ", format(.both_na, big.mark = ","), " people have both leave items",
    " missing and count as zero days.\n", sep = "")

# ---- the three numbers the paper quotes ----------------------------------------
# Printed in a fixed form, so RUN_ALL.R can carry them as expectations.
.n_took   <- .tab$persons[1]
.med_took <- .tab$median_capj[1]
.m180     <- .tab$mean_capj[.tab$group == "leave days (180,400]"]
cat(sprintf(paste0("\n  took parental leave: %d persons; median %.0f per cent",
                   " of the year in a job\n"), .n_took, .med_took))
cat(sprintf("  more than 180 days of leave: mean %.1f per cent of the year in a job\n",
            .m180))

W24_QUOTED <- c(persons = 528, median = 100, mean_180 = 96.5)
.got <- c(persons = .n_took, median = .med_took, mean_180 = .m180)
.ok  <- !is.na(.got) & abs(.got - W24_QUOTED) < c(0.5, 0.05, 0.05)
cat("  against the paper: ",
    paste(names(.got), ifelse(.ok, "matches", "DIFFERS"), collapse = "; "),
    "\n", sep = "")

# ---- write ---------------------------------------------------------------------
.out <- if (.w24_contract) td_out("results", "w24_calendar_check.csv") else {
  dir.create(file.path("output", "results"), recursive = TRUE,
             showWarnings = FALSE)
  file.path("output", "results", "w24_calendar_check.csv")
}
utils::write.csv(.tab, .out, row.names = FALSE)
cat("  wrote ", .out, "\n", sep = "")

if (.w24_contract) td_log_close()
rm(.v, .lab, .miss, .over, .both_na, .n_took, .med_took, .m180, .got, .ok, .out)
