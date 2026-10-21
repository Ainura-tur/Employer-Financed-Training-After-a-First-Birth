# =============================================================================
# paper_tables_revision_block.R
#
# Four additions to paper_tables.R for the revision, the last optional. Paste
# each where its header says; nothing here runs on its own.
#
#   PART 1  inside the Sun-Abraham block, after put("SATrainingPostSE", ...):
#           \SATrainingPostPP, \SATrainingPostPct and \SATrainingPostMDE, from
#           the same cs_post_period_att.csv row as \SATrainingPostATT
#   PART 2  after rm(.sa), at top level: the ten macros
#           06_revision_inference.R computes, plus \SATrainingPostN
#   PART 3  in CARRIED_TABLES$tab_training_main: the Sun-Abraham row, and
#           the sex-specific triple difference listed first (two edits)
#   PART 4  optional, with the revised robust_30_49.R: the Sun-Abraham
#           average as the first row of Table D.1, and \AgeWindowSA and
#           \AgeWindowSASE
# =============================================================================


# =============================================================================
# PART 1. Paste inside `if (nrow(.sar) == 1L) {`, directly after the
# put("SATrainingPostSE", ...) call.
# =============================================================================
    # v-rev. THE HEADLINE IN POINTS, AS A SHARE OF WOMEN'S BASE, AND ITS MDE.
    # The abstract, the Introduction and the Conclusion quote the Sun-Abraham
    # average in percentage points and as a percent of women's pre-birth base,
    # and Table 2's new row needs its MDE. All three come from the row that
    # SATrainingPostATT reads, so they cannot drift from it. The base is
    # ledger.base_f, the value \pn{ledger.base_f} prints beside the percent;
    # T0_pre_birth_base_rates.csv, where 03_common.R writes it, is the
    # fallback. The text says the training "falls", so a non-negative
    # average leaves both magnitudes as \tofill rather than print a fall.
    .pt_registered <- function(key) {
      .d  <- if (exists("PN_DIR")) PN_DIR else
        file.path(if (exists("PROJ_ROOT")) PROJ_ROOT else getwd(), "output", "numbers")
      .fs <- list.files(.d, pattern = "\\.csv$", full.names = TRUE)
      .fs <- .fs[order(basename(.fs) != "MASTER_training_y__ledger.csv")]
      for (.f in .fs) {
        .x <- tryCatch(utils::read.csv(.f, stringsAsFactors = FALSE), error = function(e) NULL)
        if (is.data.frame(.x) && all(c("key", "value") %in% names(.x)) && key %in% .x$key)
          return(suppressWarnings(as.numeric(.x$value[match(key, .x$key)])))
      }
      NA_real_
    }
    .att <- .sar$att[1]; .ase <- .sar$se[1]
    put("SATrainingPostMDE",
        fmt_plain(if (exists("mde", mode = "function")) mde(.ase) else 2.8016 * .ase, 3),
        "its MDE, 80 percent power at a two-sided 5 percent level")
    .bf <- .pt_registered("ledger.base_f")
    if (!is.finite(.bf)) {
      .t0 <- read_csv_if("T0_pre_birth_base_rates.csv")
      if (!is.null(.t0) && all(c("female", "base_pewt") %in% names(.t0)))
        .bf <- .t0$base_pewt[.t0$female == 1][1]
      rm(.t0)
    }
    if (is.finite(.att) && .att < 0) {
      put("SATrainingPostPP", fmt_plain(100 * abs(.att), 1),
          "its magnitude in percentage points")
      if (is.finite(.bf) && .bf > 0) {
        put("SATrainingPostPct", fmt_plain(100 * abs(.att) / .bf, 0),
            sprintf("its magnitude as a percent of women's pre-birth base, %.4f", .bf))
      } else {
        cat("  women's pre-birth base not found (ledger.base_f, T0_pre_birth_base_rates.csv),\n",
            "  so SATrainingPostPct stays \\tofill.\n", sep = "")
      }
    } else {
      cat("  *** the Sun-Abraham average is not negative, so the text's \"falls\" would be\n",
          "      wrong. SATrainingPostPP and SATrainingPostPct stay \\tofill.\n", sep = "")
    }
    rm(.pt_registered, .att, .ase, .bf)


# =============================================================================
# PART 2. Paste after the line rm(.sa) that closes the Sun-Abraham block, at
# top level, before "02's outputs, which nothing has ever read by name".
# =============================================================================
# --- the revision's inference, from 06_revision_inference.R ----------------
#
# Couple-robust standard errors for the four key contrasts, the event-study
# version of the financing contrast with its lead test, the one-model stay-
# less-leave difference, and the headline's sample size for Table 2. 06
# reproduces each published estimate first and marks a value "ok" only when
# it did. A macro is written only if its row is ok, comes from this build of
# the panel, and every published file its block read still exists and has not
# changed since the block checked it. Otherwise the macro keeps its
# \providecommand fallback, which prints the red rule, so a value whose base
# was not reproduced, or has moved since, never reaches the page.
#
# MATH MODE DECIDES THE FORMATTER, as above. Each macro sits in running text
# except SADddPreP, which the text writes inside $p = ...$, so it carries no
# dollars. Digits follow the number each one is set beside.
.rv <- read_csv_if("revision_inference_summary.csv")
.RV_FMT <- list(
  SATrainingPostN     = function(x) if (is.finite(x)) n_fmt(x) else TOFILL,  # Table 2, n
  DiDSECouple         = function(x) fmt_plain(x, 4),    # beside \pn{ledger.T1_attached.se.d4}
  DddSECouple         = function(x) fmt_plain(x, 4),    # beside \pn{ledger.T3_ddd.se.d4}
  SASECouple          = function(x) fmt_plain(x, 4),    # beside \SATrainingPostSE
  SAArmCorr           = function(x) fmt(x, 3),
  WageWindowSECouple  = function(x) fmt_plain(x, 3),    # beside \pn{wptest...se}
  SADddATT            = function(x) fmt(x, 4),
  SADddSE             = function(x) fmt_plain(x, 4),
  SADddPreP           = function(x) if (is.finite(x) && x < 0.001) "<0.001" else fmt_plain(x, 3),
  StayLeavePropDiff   = function(x) fmt(x, 3),
  StayLeavePropDiffSE = function(x) fmt_plain(x, 3))
# Why a row cannot be printed, or "" if it can. 06 writes the times in UTC.
.rv_why <- function(r) {
  if (!identical(r$status, "ok")) return(sprintf("status \"%s\"", r$status))
  .bt <- if (exists("bt") && !is.null(bt)) as.character(bt) else NA_character_
  if (!is.na(.bt) && !identical(as.character(r$build_tag), .bt))
    return(sprintf("checked on build %s, and this frame is build %s", r$build_tag, .bt))
  .ck <- as.POSIXct(as.character(r$checked), format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  if (is.na(.ck)) return("no time of checking")
  if (is.na(r$files)) return("no record of the files it was checked against")
  .f <- strsplit(as.character(r$files), "|", fixed = TRUE)[[1]]
  .f <- .f[nzchar(.f) & !startsWith(.f, "MISSING:")]
  if (!length(.f)) return("no record of the files it was checked against")
  .gone <- .f[!file.exists(.f)]
  if (length(.gone))
    return(sprintf("%s is no longer where 06 read it", paste(basename(.gone), collapse = " and ")))
  .new <- .f[file.mtime(.f) > .ck]
  if (length(.new))
    return(sprintf("checked %s UTC, before %s last changed", substr(r$checked, 1, 19),
                   paste(basename(.new), collapse = " and ")))
  ""
}
if (!is.null(.rv) && all(c("macro", "value", "status", "block", "checked", "build_tag",
                           "files") %in% names(.rv))) {
  .rv_stale <- FALSE
  for (.m in names(.RV_FMT)) {
    .r <- .rv[!is.na(.rv$macro) & .rv$macro == .m, , drop = FALSE]
    .w <- if (nrow(.r) != 1L)
      sprintf("%d row(s) in revision_inference_summary.csv", nrow(.r)) else .rv_why(.r)
    if (nzchar(.w)) {
      cat("  ", .m, ": ", .w, ", so it stays \\tofill\n", sep = "")
      if (nrow(.r) == 1L && (isTRUE(.r$status %in% "ok") || isTRUE(startsWith(.r$status, "stale"))))
        .rv_stale <- TRUE
    } else {
      put(.m, .RV_FMT[[.m]](suppressWarnings(as.numeric(.r$value))), .r$basis)
    }
  }
  if (.rv_stale)
    cat("  *** Rerun 06_revision_inference.R with REV_BOOT_B=0, which reruns every block\n",
        "      but the bootstrap and checks the bootstrap rows again. If those then say\n",
        "      \"stale (inputs changed)\", rerun it with REV_BLOCKS=BOOT as well.\n", sep = "")
  .p <- suppressWarnings(as.numeric(.rv$value[.rv$macro %in% "SADddPreP"]))
  if (length(.p) == 1L && is.finite(.p) && .p < 0.001)
    cat("  *** SADddPreP is below 0.001: write $p < 0.001$ in Section 7.1, not $p = $.\n")
  rm(.rv_stale, .p)
  if (exists(".m")) rm(.m)
  if (exists(".r")) rm(.r)
  if (exists(".w")) rm(.w)
} else if (!is.null(.rv)) {
  cat("  revision_inference_summary.csv has an older layout, so the revision's inference\n",
      "  macros stay \\tofill. Rerun 06_revision_inference.R.\n", sep = "")
} else {
  cat("  revision_inference_summary.csv not found, so the revision's inference macros\n",
      "  stay \\tofill. Run 06_revision_inference.R.\n", sep = "")
}
rm(.rv, .RV_FMT, .rv_why)


# =============================================================================
# PART 3. Two edits in CARRIED_TABLES$tab_training_main. The v36 comment
# between the heading and the triple-difference rows stays where it is.
# The new row reads its numbers from the macros above, so it cannot drift
# from the text. verify_manuscript_numbers.R parses the row, finds no digits
# in its n column and lists it as unmatched; that is expected, because the
# row has no ledger tag.
# =============================================================================
# (a) Directly after the "Extensive margin, employer-financed training"
#     heading, insert the Sun-Abraham row:
if (FALSE) c(
    "\\multicolumn{6}{l}{\\textit{Extensive margin, employer-financed training}} \\\\",
    "\\quad Sun--Abraham average, employed only & \\SATrainingPostN{} & \\SATrainingPostATT{} & \\SATrainingPostSE{} & \\SATrainingPostMDE{} & post-hoc\\tnote{e} \\\\",
    "\\quad Employed or on leave & 55,172 & $-0.112$ & 0.0155 & 0.043 & preferred \\\\")
# (b) Swap the two triple-difference rows, and move the \\[3pt] to the row
#     that is now last. The labels are unchanged, and the verifier matches
#     these two rows on their labels.
if (FALSE) c(
    "\\quad Sex-specific source-by-wave & 110,078 & $-0.092$ & 0.0161 & 0.045 & secondary \\\\",
    "\\quad Common source-by-wave       & 110,078 & $-0.059$ & 0.0143 & 0.040 & preferred \\\\[3pt]")


# =============================================================================
# PART 4. Optional, Table D.1. Only with the revised robust_30_49.R, whose csv
# carries an "sa" arm; with an older csv the block below withholds the whole
# table, because every arm it lists must have a 30-49 row. Three edits in the
# 30-49 block.
# =============================================================================
# (a) .AGW_ARMS gains the headline as its first entry:
if (FALSE) .AGW_ARMS <- c(
  sa   = "Employer-financed training, Sun--Abraham average",
  att  = "Employer-financed training, employed or on leave",
  emp  = "Employer-financed training, employed only",
  own  = "Own-time training, employed only",
  ddd  = "Triple difference, employer-financed less own time",
  wage = "Wage gap, years 5--10 less years 0--4")
# (b) in .agw_rows, the headline prints to four decimals, as the text quotes it:
#       dg <- if (a %in% c("own", "sa")) 4 else 3
# (c) in the macro loop:
#       for (.a in c("sa", "att", "emp", "own", "ddd", "wage")) {
#         .nm <- switch(.a, sa = "SA", att = "Att", emp = "Emp", own = "Own",
#                       ddd = "Ddd", wage = "Wage")
#         .dg <- if (.a %in% c("own", "sa")) 4 else 3
#     which writes \AgeWindowSA and \AgeWindowSASE.
