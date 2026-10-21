# =====================================================================
# vm_guards.R
#
# THE BUG THIS CLOSES. Four checks in verify_manuscript_numbers.R turned
# out to report something other than what they appeared to, and all four
# had one shape: a conclusion drawn over an input the check never
# described.
#
#   Check 5 read app_arms.tex, found it absent, parsed zero rows, and
#     printed "All 0 rows of Table 3 reconcile".
#   Check 9's noise filters were built with substr(), which is not
#     vectorised over start and stop. The mask came back length 1 and `|`
#     recycled it across 958 tokens, so both filters were evaluated once
#     and applied to everything.
#   Check 9 also scanned the folder it writes prose_numbers_check.csv
#     into, so the run after any WRITE_REPORT run found every literal in
#     its own output and announced 100 per cent.
#   app_arms.tex resolved from TEXDIR, set inside paper_tables.R, so the
#     lookup succeeded or failed on session state rather than on disk.
#
# In each case the check's verdict was arithmetically correct and
# substantively empty. Zero of zero do reconcile. A length-1 logical is a
# valid mask. A file the script wrote does contain the numbers it wrote.
#
# THE FIX IS NOT A BETTER VERDICT, IT IS A DENOMINATOR. A check that
# cannot say how many items it examined, which files it read, and what
# its filters removed, cannot be trusted when it says "clean" -- and no
# amount of care in any single check prevents the next one from failing
# the same way. So the reporting is moved out of the checks and into
# four functions every check routes through.
#
#   vm_verdict()  no verdict without a denominator. Zero items is
#                 NOT CHECKED, never a pass.
#   vm_mask()     a logical filter must be the length of what it filters.
#                 No recycling, and degenerate filters are named.
#   vm_input()    every file a check reads is recorded, and a file this
#                 run wrote is refused.
#   vm_summary()  one table at the end: every check, its inputs, its
#                 denominator, its verdict. A check that never reported
#                 appears as NO REPORT, which is the case none of the
#                 four faults above would have caught.
#
# vm_gate() returns FALSE unless every expected check reported a
# denominator above zero and none failed, so FINAL_PASS.R can stop on it.
#
# ---------------------------------------------------------------------
# HOW TO WIRE IT IN. Three steps, and the third can be done one check at
# a time: an un-retrofitted check shows up as NO REPORT rather than
# breaking.
#
#   1. source("vm_guards.R") near the top of verify_manuscript_numbers.R,
#      after the helpers and before CHECK 1.
#
#   2. vm_reset(own_outputs = c("prose_numbers_check.csv",
#                               "verify_report.csv"))
#      Name every file the verifier itself writes into a folder it also
#      reads. This is what stops the self-read.
#
#   3. Replace each check's closing cat() with a vm_verdict() call, and
#      wrap each file read in vm_input() and each logical filter in
#      vm_mask(). The retrofit map is at the foot of this file.
#
# The module has no dependencies beyond base R, deliberately: a guard
# that can be broken by a masked verb is not a guard.
# =====================================================================

#Namespace protection (avoid MASS/stats masking dplyr verbs)
select <- dplyr::select;       filter    <- dplyr::filter
mutate <- dplyr::mutate;       slice     <- dplyr::slice
recode <- dplyr::recode;       rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange <- dplyr::arrange;     count     <- dplyr::count;  lag <- dplyr::lag

.VM <- new.env(parent = emptyenv())

# The checks this file expects to hear from. A name here with no verdict
# is reported as NO REPORT, which is how a check whose branch never fired
# becomes visible. Edit when checks are added or retired.
.VM_EXPECTED <- c("0", "1", "1b", "2", "3", "4", "5", "6", "7", "7b",
                  "8", "9", "10", "11")

vm_reset <- function(expected = .VM_EXPECTED, own_outputs = character(0),
                     t0 = Sys.time()) {
  .VM$t0       <- t0
  .VM$verdicts <- list()
  .VM$inputs   <- list()
  .VM$masks    <- list()
  .VM$expected <- as.character(expected)
  .VM$own      <- as.character(own_outputs)
  # A verdict table from an earlier run in this session is exactly the
  # stale-input fault this file is about, so it goes here rather than
  # waiting to be overwritten by vm_summary().
  if (exists("VM_VERDICTS", envir = globalenv()))
    rm("VM_VERDICTS", envir = globalenv())
  cat("\n  vm_guards: ", length(.VM$expected), " check(s) expected to report",
      " a denominator.\n", sep = "")
  if (length(.VM$own))
    cat("            this run's own outputs, excluded from any folder it\n",
        "            also reads: ", paste(.VM$own, collapse = ", "), "\n", sep = "")
  invisible(TRUE)
}

.vm_started <- function() {
  if (is.null(.VM$t0)) vm_reset()
  invisible(TRUE)
}

# ---------------------------------------------------------------------
# vm_verdict(). THE DENOMINATOR IS NOT OPTIONAL.
#
#   check    "5", "7b", ... matching .VM_EXPECTED
#   n        how many items the check actually examined
#   bad      how many of them failed
#   unit     what an item is, for the sentence: "claim(s)", "row(s)"
#   expected how many items should have arrived, when that is knowable.
#            n < expected is SHORT: clean on what came, incomplete.
#   clean    optional replacement for the clean sentence. The count is
#            printed either way; the wording is yours.
#
# Zero items is NOT CHECKED and is never a pass, whatever `bad` says.
# ---------------------------------------------------------------------
vm_verdict <- function(check, n, bad = 0L, unit = "item(s)",
                       expected = NA_integer_, clean = NULL, note = "") {
  .vm_started()
  check <- as.character(check)[1]
  n     <- suppressWarnings(as.integer(n)[1]);   if (is.na(n))   n   <- 0L
  bad   <- suppressWarnings(as.integer(bad)[1]); if (is.na(bad)) bad <- 0L
  expected <- suppressWarnings(as.integer(expected)[1])

  status <- if (n == 0L) "NOT CHECKED"
            else if (bad > 0L) "FAIL"
            else if (!is.na(expected) && n < expected) "SHORT"
            else "clean"

  msg <- switch(
    status,
    "NOT CHECKED" = sprintf(paste0(
      "  NOT CHECKED. Zero %s reached this check, so it concluded nothing.\n",
      "      Zero of zero reconciling is not a pass. Find why the input is\n",
      "      empty before reading anything else in this section."), unit),
    "FAIL" = sprintf("  %d of %d %s do not reconcile.", bad, n, unit),
    "SHORT" = sprintf(paste0(
      "  %d of %d %s examined and all reconcile, but %d were expected.\n",
      "      %d never arrived, so this is clean on a partial input."),
      n, n, unit, expected, expected - n),
    "clean" = if (!is.null(clean)) paste0("  ", clean, sprintf(" [%d %s]", n, unit))
              else sprintf("  Clean: all %d %s reconcile.", n, unit))

  cat(msg, "\n", sep = "")
  if (nzchar(note)) cat("      ", note, "\n", sep = "")

  .VM$verdicts[[check]] <- list(check = check, n = n, bad = bad, unit = unit,
                                expected = expected, status = status,
                                note = note)
  invisible(status)
}

# ---------------------------------------------------------------------
# vm_mask(). A LOGICAL FILTER IS THE LENGTH OF WHAT IT FILTERS.
#
# The substr() fault produced a length-1 mask over 958 tokens and R
# recycled it silently. Recycling a filter is never intended, so this
# stops rather than warns. It also names a filter that removed nothing
# or everything, because a mask doing nothing looks identical to no mask
# at all in the output.
# ---------------------------------------------------------------------
vm_mask <- function(x, mask, what = "filter", check = NA) {
  .vm_started()
  n <- length(x)
  if (!is.logical(mask))
    stop(sprintf("vm_mask: %s is %s, not logical.", what, class(mask)[1L]),
         call. = FALSE)
  if (length(mask) != n)
    stop(sprintf(paste0(
      "vm_mask: %s has length %d against the %d item(s) it filters.\n",
      "  R would recycle it, so the filter would be evaluated on one item\n",
      "  and applied to all of them. This is the substr() fault: substr()\n",
      "  is not vectorised over start and stop and returns one element per\n",
      "  element of x, not per element of start. Use substring()."),
      what, length(mask), n), call. = FALSE)

  na <- sum(is.na(mask))
  if (na) {
    cat(sprintf("      %s: %d of %d NA, treated as FALSE.\n", what, na, n))
    mask[is.na(mask)] <- FALSE
  }
  kept <- sum(mask)
  .VM$masks[[length(.VM$masks) + 1L]] <-
    list(check = as.character(check)[1], what = what, n = n, kept = kept)

  if (n > 0L && kept == 0L)
    cat(sprintf(paste0("      %s matched NOTHING across %d item(s). It is ",
                       "having no effect;\n      confirm that is intended.\n"),
                what, n))
  if (n > 0L && kept == n)
    cat(sprintf(paste0("      %s matched EVERYTHING across %d item(s), so ",
                       "whatever it guards\n      is unreachable.\n"), what, n))
  mask
}

# ---------------------------------------------------------------------
# vm_input(). PROVENANCE, AND NO READING THIS RUN'S OWN OUTPUT.
#
# Returns the path when it is safe to read and NA_character_ otherwise,
# so a caller can branch on it. A file named in own_outputs, or one
# modified since vm_reset(), is this run's own work: reading it turns the
# check into a tautology.
# ---------------------------------------------------------------------
vm_input <- function(path, check = NA, role = "", required = TRUE) {
  .vm_started()
  path <- as.character(path)[1]
  ok   <- !is.na(path) && nzchar(path) && file.exists(path)
  mt   <- if (ok) file.mtime(path) else as.POSIXct(NA)
  own  <- ok && (basename(path) %in% .VM$own ||
                   (!is.na(mt) && mt >= .VM$t0))

  status <- if (!ok) (if (required) "ABSENT" else "absent, optional")
            else if (own) "SELF-WRITTEN" else "ok"

  .VM$inputs[[length(.VM$inputs) + 1L]] <-
    list(check = as.character(check)[1], path = path, role = role,
         mtime = mt, status = status)

  if (status == "ABSENT")
    cat(sprintf("      input MISSING: %s%s\n", path,
                if (nzchar(role)) paste0("  (", role, ")") else ""))
  if (status == "SELF-WRITTEN")
    cat(sprintf(paste0("      REFUSED, written by this run at %s: %s\n",
                       "      A check that reads its own output confirms ",
                       "only that it wrote it.\n"),
                format(mt, "%H:%M:%S"), basename(path)))
  if (status == "ok" && nzchar(role))
    cat(sprintf("      read %s  (%s, %s)\n", basename(path), role,
                format(mt, "%Y-%m-%d %H:%M")))

  if (status %in% c("ABSENT", "SELF-WRITTEN")) NA_character_ else path
}

# Folder scans: drop this run's own outputs from a vector of paths and
# say how many went, so a pass rate cannot improve because the scan grew.
vm_scan <- function(paths, check = NA) {
  .vm_started()
  paths <- as.character(paths)
  if (!length(paths)) return(paths)
  mt   <- file.mtime(paths)
  drop <- basename(paths) %in% .VM$own | (!is.na(mt) & mt >= .VM$t0)
  if (any(drop)) {
    cat(sprintf("      excluded %d file(s) written by this run: %s\n",
                sum(drop), paste(unique(basename(paths[drop])), collapse = ", ")))
    .VM$inputs[[length(.VM$inputs) + 1L]] <-
      list(check = as.character(check)[1],
           path = paste(basename(paths[drop]), collapse = ", "),
           role = "excluded, self-written", mtime = as.POSIXct(NA),
           status = "SELF-WRITTEN")
  }
  cat(sprintf("      scanning %d artefact(s).\n", sum(!drop)))
  paths[!drop]
}

# ---------------------------------------------------------------------
# vm_summary(). ONE TABLE, EVERY CHECK, ALWAYS.
# ---------------------------------------------------------------------
vm_summary <- function() {
  .vm_started()
  vs <- .VM$verdicts
  d <- if (length(vs)) do.call(rbind, lapply(vs, function(v) data.frame(
         check = v$check, n = v$n, fail = v$bad, unit = v$unit,
         status = v$status, stringsAsFactors = FALSE)))
       else data.frame(check = character(0), n = integer(0), fail = integer(0),
                       unit = character(0), status = character(0),
                       stringsAsFactors = FALSE)
  gone <- setdiff(.VM$expected, d$check)
  if (length(gone))
    d <- rbind(d, data.frame(check = gone, n = NA_integer_, fail = NA_integer_,
                             unit = "-", status = "NO REPORT",
                             stringsAsFactors = FALSE))
  ord <- c(.VM$expected, setdiff(d$check, .VM$expected))
  d <- d[order(match(d$check, ord)), , drop = FALSE]

  cat("\n=== VERDICTS, WITH DENOMINATORS ===\n")
  print(d, row.names = FALSE)

  bad_in <- Filter(function(i) i$status %in% c("ABSENT", "SELF-WRITTEN"),
                   .VM$inputs)
  if (length(bad_in)) {
    cat("\n  Inputs that were missing or refused:\n")
    for (i in bad_in)
      cat(sprintf("    check %-3s %-14s %s\n", i$check, i$status,
                  basename(i$path)))
  }
  dead <- Filter(function(m) m$n > 0L && (m$kept == 0L || m$kept == m$n),
                 .VM$masks)
  if (length(dead)) {
    cat("\n  Filters with no discriminating power:\n")
    for (m in dead)
      cat(sprintf("    check %-3s %s kept %d of %d\n", m$check, m$what,
                  m$kept, m$n))
  }

  n_nc <- sum(d$status == "NOT CHECKED"); n_nr <- sum(d$status == "NO REPORT")
  n_f  <- sum(d$status == "FAIL");        n_s  <- sum(d$status == "SHORT")
  cat(sprintf(paste0("\n  %d clean, %d fail, %d short, %d not checked, ",
                     "%d no report.\n"),
              sum(d$status == "clean"), n_f, n_s, n_nc, n_nr))
  if (n_nc || n_nr)
    cat("  A check that examined nothing, or never said what it examined,\n",
        "  is not evidence about the manuscript.\n", sep = "")
  assign("VM_VERDICTS", d, envir = globalenv())
  invisible(d)
}

# TRUE only when every expected check reported a non-empty denominator
# and none failed. FINAL_PASS.R gates on this.
vm_gate <- function() {
  d <- if (exists("VM_VERDICTS", envir = globalenv()))
         get("VM_VERDICTS", envir = globalenv()) else vm_summary()
  # SHORT does not pass. Clean on an input that arrived incomplete is the
  # failure this whole file exists to make visible.
  isTRUE(nrow(d) > 0L && all(d$status == "clean"))
}

# ---------------------------------------------------------------------
# RETROFIT MAP, against the 11 September file. Line numbers drift; the
# quoted text is the anchor. Each is one line in, one line out.
#
#  Check 1   ~971   cat("All", nrow(chk), "claims reconcile on estimate, SE and n.\n")
#                -> vm_verdict("1", nrow(chk), sum(chk$flag), "claim(s)",
#                              clean = "All claims reconcile on estimate, SE and n.")
#                   The !nrow(chk) branch above it can go: vm_verdict owns that case.
#
#  Check 1b  ~1019 cat("None within a fiftieth of a boundary.\n")
#                -> vm_verdict("1b", nrow(<the frame scanned>), <n near>, "estimate(s)",
#                              clean = "None within a fiftieth of a boundary.")
#                   As written this prints the same words whether it scanned
#                   90 estimates or none.
#
#  Check 2   ~1038 the DDD identity
#                -> vm_verdict("2", <arms found, 0 to 3>, <arms whose gap exceeds tol>,
#                              "arm(s)", expected = 3)
#                   Three arms are needed for the identity. SHORT is the
#                   verdict when one is missing, not silence.
#
#  Check 3   ~1076 proportional claims     -> vm_verdict("3",  n, bad, "claim(s)")
#  Check 4   ~1112 status as designated    -> vm_verdict("4",  nrow(cmp), n_diff, "row(s)")
#  Check 5   ~1177 "All", nrow(t8chk), "rows of Table 3 reconcile."
#                -> vm_verdict("5", nrow(t8chk), sum(t8chk$bad), "row(s)")
#                   and wrap the app_arms.tex read in vm_input(), which
#                   records the path that made this pass on zero rows.
#  Check 6   ~1268 arms reconcile          -> vm_verdict("6",  nrow(hc), sum(hc$bad), "arm(s)")
#  Check 7   ~1291 horizons reconcile      -> vm_verdict("7",  nrow(wc), sum(wc$bad), "horizon(s)")
#  Check 7b  ~1327 timing quantities       -> vm_verdict("7b", <quantities evaluated>,
#                                                        <off-grid or wrong>, "quantity(ies)",
#                                                        expected = 6)
#                   Off-grid is not clean: it is a claim the series cannot
#                   speak to, which is what SHORT is for.
#  Check 8   ~1765 "...not found ...; skipped."
#                -> p <- vm_input(<lee bounds rds>, "8", "Lee bounds")
#                   if (is.na(p)) vm_verdict("8", 0) else vm_verdict("8", n, bad, "horizon(s)")
#                   Skipped currently leaves no trace in any summary.
#  Check 9   ~1818 literals
#                -> wrap the folder scan in vm_scan(), both noise filters in
#                   vm_mask(), and close with
#                   vm_verdict("9", n_checked, n_unfound, "literal(s)")
#  Check 10  ~1982 macros   -> vm_verdict("10", length(.used10),
#                                         length(.tofl10) + length(.undef10), "macro(s)")
#  Check 11  ~2083 artefacts-> vm_verdict("11", <artefacts>, <duplicated>, "artefact(s)")
#
# Then at the foot of the file, before the footer: vm_summary()
# And in FINAL_PASS.R, after sourcing the verifier:
#   if (!vm_gate()) .fp_stop("the verifier did not return a clean denominator
#                             for every check; see VERDICTS, WITH DENOMINATORS.")
# =====================================================================
