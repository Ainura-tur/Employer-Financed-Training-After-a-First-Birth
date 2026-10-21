# =============================================================================
# check9_sources.R   Where the literals Check 9 cannot trace come from
# v-c9-followup, 19 October: parts 2, 3 and 8 read the text; see apply_c9_followup.R.
#
# WHAT IT DOES. The verifier's run of 9 October 11:39 left nine literals in
# Check 9 that no scanned artefact holds: 609, 236, 224, 195, 230, 88.1, 32.6,
# 25.1 and 22.1. Each has a source that Check 9 does not read as a number.
# This script goes to each source, recomputes the value and says whether it
# matches the manuscript. It also checks the numbers that share a sentence and
# a source with them, because Check 9 may have passed those by coincidence.
# E.1's birth-report numbers come from open_items_queries.csv when it holds
# them, and otherwise from the newest log that printed them.
#
# WHY CHECK 9 MISSES THEM.
#   609         a text cell: open_items_queries.csv holds "609 persons"
#   236, 224    a product: n x p_t in T3c_fee_by_horizon_decomposed.csv
#   195, 230    a folder Check 9 does not scan: the loader writes
#               v2114_fieldwork_window.csv to the project root
#   32.6, 25.1  averages that 03b prints and does not write
#   22.1        T3c_fee_level_by_group.csv stores two decimals. A stored 22.05
#               prints as 22.1 with sprintf() and formatC(), but round(),
#               which Check 9 uses, gives 22.0. Part 6 shows which applies.
#   88.1        a fraction match in a cell whose label shares no word with the
#               sentence, so it waits for confirmation
#   0.00121     training_decisions.txt holds the T2 drift, and Check 9 reads no
#               .txt file. On 9 October it matched a covariance cell in
#               cs_event_study_vcov.rds, which is a coincidence.
#   1.30        the corrected post-birth low of the B.2 ratio; no file stores
#               the ratio, which part 3 recomputes from the printed shares
#
# v-c9-roster, 19 October. Parts 3 and 5 read the numbers they check from
# the manuscript instead of typing them, so a corrected sentence is checked
# as it now stands (they had expected 236, 26.0 and 5.3). Part 7 now checks
# that B.6 prints its bootstrap probabilities through \pn{Grprob.*} keys,
# the roster arm of REFEREE_RUN.R block G2; the wider-frame 88.1 and 94.6
# left the text, and pn_check() verifies the keys against the registry.
# HOW TO READ IT. vm_settings.R marks with [c9] the entries this script
# checks. Each verdict line says "matches", "DIFFERS" or "NOT FOUND". For any
# [c9] entry that does not match, take it out of vm_settings.R before running
# the verifier: the text or the source needs a look. A partner that does not
# match is a number in the text that its own source does not give.
#
# It changes nothing on disk except its own log in run_logs. Run it from the
# project root:  source("check9_sources.R")
# =============================================================================

suppressPackageStartupMessages(library(dplyr))

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

# ---- the contract -------------------------------------------------------------
# Only its folder names are used. Sourced when the session lacks them, as
# robust_30_49.R does.
if (!all(vapply(c("PROJ_ROOT", "DIR_DESIGNS", "DIR_RESULTS", "LOG_DIR"),
                exists, logical(1)))) {
  .c <- c("00_contract.R", file.path("..", "00_contract.R"))
  .c <- .c[file.exists(.c)]
  if (!length(.c))
    stop("00_contract.R not found from ", getwd(), ". setwd() to the project ",
         "root and source this file again.", call. = FALSE)
  cat("Contract: not loaded in this session; sourcing ",
      normalizePath(.c[1], winslash = "/"), "\n", sep = "")
  source(.c[1])
  rm(.c)
}

# ---- the log --------------------------------------------------------------------
# Opened only if nothing else is recording, and closed in `finally`.
.c9_owns <- sink.number() == 0L
.c9_log  <- file.path(LOG_DIR, paste0("check9_sources_",
                      format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
.c9_op   <- options(width = max(getOption("width"), 160))
if (.c9_owns) sink(.c9_log, split = TRUE)

tryCatch(local({

cat(strrep("=", 78), "\n", sep = "")
cat("check9_sources.R  started ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", sep = "")
cat(strrep("=", 78), "\n", sep = "")

# ---- where to look ----------------------------------------------------------------
dirs <- unique(c(DIR_DESIGNS, DIR_RESULTS,
                 if (exists("DIR_DIAG")) DIR_DIAG,
                 file.path(PROJ_ROOT, "output", c("fte_experience", "diagnostics")),
                 PROJ_ROOT, getwd()))
dirs <- dirs[dir.exists(dirs)]
# The folders Check 9 scans. A file anywhere else cannot be traced by it.
scanned <- unique(normalizePath(c(DIR_DESIGNS, DIR_RESULTS,
                                  file.path(dirname(DIR_DESIGNS), "fte_experience")),
                                winslash = "/", mustWork = FALSE))

# Every copy is listed with its date; the newest is used.
find1 <- function(f) {
  p <- file.path(dirs, f)
  p <- unique(normalizePath(p[file.exists(p)], winslash = "/"))
  if (!length(p)) {
    cat("  ", f, ": not found in ", paste(dirs, collapse = ", "), "\n", sep = "")
    return(NA_character_)
  }
  p <- p[order(file.info(p)$mtime, decreasing = TRUE)]
  for (q in p)
    cat(sprintf("  source: %s  %s%s\n", format(file.info(q)$mtime, "%Y-%m-%d %H:%M"), q,
                if (dirname(q) %in% scanned) "" else "  (a folder Check 9 does not scan)"))
  if (length(p) > 1L) cat("  taking the newest\n")
  p[1]
}

# The literals vm_settings.R accepts or confirms, read without applying them,
# so the verdicts can say which lines are settings entries.
vm  <- new.env()
vmf <- unique(c("vm_settings.R", file.path(PROJ_ROOT, "vm_settings.R")))
vmf <- vmf[file.exists(vmf)]
if (length(vmf)) sys.source(vmf[1], envir = vm)
in_vm <- if (length(vmf)) c(get0("VM_ACCEPTED_LITERALS", vm, ifnotfound = character(0)),
                            sub("=.*$", "", get0("VM_CONFIRMED_FRACTIONS", vm, ifnotfound = character(0)))) else
  c("613", "237", "224", "195", "230", "219", "201", "32.6", "29.4", "25.1", "22.1",
    "0.00123", "1.30", "25.9", "17.8", "472", "6.0")
cat("  settings entries read from: ",
    if (length(vmf)) normalizePath(vmf[1], winslash = "/") else "none found; using the 9 October list",
    "\n", sep = "")

# One row per literal: what the source gives, as the text would print it.
res <- data.frame(literal = character(0), source_gives = character(0),
                  where = character(0), flagged = logical(0), stringsAsFactors = FALSE)
add <- function(lit, got, where) {
  got <- if (length(got) == 1L && !is.na(got)) as.character(got) else NA_character_
  res[nrow(res) + 1L, ] <<- list(lit, got, where, lit %in% in_vm)
}
# Half up, as a person rounds; the unrounded value is printed beside it.
r0 <- function(x) sprintf("%.0f", floor(x + 0.5))

# The manuscript, newest version, read only for two B.2 sentences (parts 3 and 5).
tex_file <- local({
  c1 <- c(Sys.glob(file.path(PROJ_ROOT, "els-cas-templates", "Motherhood_training_v*.tex")),
          Sys.glob(file.path(PROJ_ROOT, "Motherhood_training_v*.tex")))
  if (length(c1)) c1[which.max(file.info(c1)$mtime)] else NA_character_
})
tex <- if (is.na(tex_file)) character(0) else
  readLines(tex_file, warn = FALSE, encoding = "UTF-8")

# ---- 1. What Check 9 recorded ---------------------------------------------------------
cat("\n--- 1. What Check 9 recorded at its last run ---\n")
f_rep <- find1("prose_numbers_check.csv")
if (!is.na(f_rep)) {
  rp <- utils::read.csv(f_rep, stringsAsFactors = FALSE, colClasses = "character")
  rr <- rp[rp$literal %in% c("613", "237", "224", "195", "230", "32.6",
                             "25.1", "22.1", "0.00123"),
           intersect(c("literal", "found", "scale", "source", "where"), names(rp)),
           drop = FALSE]
  if ("where" %in% names(rr)) rr$where <- substr(rr$where, 1, 90)
  print(rr, row.names = FALSE)
}

# ---- 2. Parents seen five or more years after the birth, ages 30-49 (D.3) ---------------
cat("\n--- 2. Parents aged 30-49 seen five or more years after a first birth (D.3) ---\n")
# v-c9-followup, 19 October: the D.3 and E.1 counts are read from the manuscript.
t2 <- local({
  a  <- grep("fathers on the roster five or more", tex, value = TRUE, fixed = TRUE)
  m  <- if (length(a)) regmatches(a[1], regexec("observes ([0-9,]+) mothers and ([0-9,]+) fathers", a[1]))[[1]] else character(0)
  b  <- grep("percent of women ever report a birth", tex, value = TRUE, fixed = TRUE)
  mb <- if (length(b)) regmatches(b[1], regexec("([0-9.]+) percent of men and ([0-9.]+) percent of women", b[1]))[[1]] else character(0)
  d  <- grep("women stay in the panel afterwards", tex, value = TRUE, fixed = TRUE)
  md <- if (length(d)) regmatches(d[1], regexec("([0-9]+) men and ([0-9]+) women stay", d[1]))[[1]] else character(0)
  if (length(m) != 3L || length(mb) != 3L || length(md) != 3L)
    cat("  a D.3 or E.1 sentence was not read from the manuscript; using the 19 October text for it\n")
  c(w  = if (length(m)  == 3L) m[2]  else "690", m  = if (length(m)  == 3L) m[3]  else "613",
    pm = if (length(mb) == 3L) mb[2] else "0.7", pw = if (length(mb) == 3L) mb[3] else "0.5",
    fm = if (length(md) == 3L) md[2] else "14",  fw = if (length(md) == 3L) md[3] else "10")
})
cat(sprintf(paste0("  text: %s mothers and %s fathers at k >= 5 (D.3); %s percent of men and %s percent\n",
                   "        of women report a birth, and %s men and %s women stay afterwards (E.1)\n"),
            t2[["w"]], t2[["m"]], t2[["pm"]], t2[["pw"]], t2[["fm"]], t2[["fw"]]))
f_oi <- find1("open_items_queries.csv")
oi <- if (is.na(f_oi)) NULL else utils::read.csv(f_oi, stringsAsFactors = FALSE)
if (!is.null(oi) && !all(c("query", "sex", "value", "note") %in% names(oi))) {
  cat("  the file lacks query, sex, value or note\n"); oi <- NULL
}
q_note <- q_val <- function(rx, sx, also = NULL) NA_character_
if (!is.null(oi)) {
  print(oi, row.names = FALSE)
  pick <- function(rx, sx, also) {
    i <- which(grepl(rx, oi$query, fixed = TRUE) & oi$sex == sx &
                 (if (is.null(also)) TRUE else grepl(also, oi$query, fixed = TRUE)))
    if (length(i) == 1L) i else NA_integer_
  }
  # The first number in the note: "690 persons", "0.6% of persons observed; ...".
  q_note <- function(rx, sx, also = NULL) {
    i <- pick(rx, sx, also)
    if (is.na(i)) NA_character_ else sub("^\\s*([0-9.,]+).*$", "\\1", oi$note[i])
  }
  q_val <- function(rx, sx, also = NULL) {
    i <- pick(rx, sx, also)
    if (is.na(i)) NA_character_ else r0(as.numeric(oi$value[i]))
  }
}
oiq <- "open_items_queries.csv"
add(t2[["w"]], q_note("k >= 5", "women", "by person-wave"),
    paste(oiq, "note: women aged 30-49 at k >= 5, by person-wave"))
add(t2[["m"]], q_note("k >= 5", "men", "by person-wave"),
    paste(oiq, "note: men aged 30-49 at k >= 5, by person-wave"))
# E.1's birth reports. CS_event_study.R writes these rows only when its frame
# carries had_child_this_wave, and a run without that column rewrites the file
# without them. The paper took them from the printed table in the first place,
# so when the file lacks them the newest log holding that table is read.
find_oi_log <- function() {
  logs <- list.files(LOG_DIR, pattern = "[.](log|txt)$", full.names = TRUE, recursive = TRUE)
  logs <- logs[!grepl("^(check9_sources|verify_manuscript)_", basename(logs))]
  if (!length(logs)) { cat("  no logs in ", LOG_DIR, "\n", sep = ""); return(NULL) }
  logs <- logs[order(file.info(logs)$mtime, decreasing = TRUE)]
  for (lg in logs) {
    x <- tryCatch(readLines(lg, warn = FALSE), error = function(e) character(0))
    h <- grep("Open items: never-treated birth reports", x, fixed = TRUE)
    if (!length(h)) next
    blk <- x[h[length(h)]:min(length(x), h[length(h)] + 60L)]
    if (!any(grepl("% of persons observed", blk, fixed = TRUE))) next
    cat(sprintf("  log: %s  %s\n", format(file.info(lg)$mtime, "%Y-%m-%d %H:%M"), lg))
    show <- blk[grepl("reporting a birth|followed by later|% of persons observed", blk)]
    cat(paste0("    ", trimws(show, "right"), collapse = "\n"), "\n", sep = "")
    # The percent by sex where the line names it; in printed order, men first,
    # where a narrow console wrapped the note column onto lines of its own.
    pc <- regmatches(blk, regexec(paste0("reporting a birth\\s+(men|women)\\s+[0-9]+\\s+",
                                         "([0-9.]+)% of persons observed"), blk))
    pc <- do.call(rbind, pc[lengths(pc) == 3L])
    pm <- pw <- NA_character_
    if (!is.null(pc)) { pm <- pc[pc[, 2] == "men", 3][1]; pw <- pc[pc[, 2] == "women", 3][1] }
    if (is.na(pm) || is.na(pw)) {
      p2 <- regmatches(blk, regexpr("[0-9.]+(?=% of persons observed)", blk, perl = TRUE))
      if (length(p2) == 2L) { pm <- p2[1]; pw <- p2[2] }
    }
    fl <- regmatches(blk, regexec("report followed by later waves\\s+(men|women)\\s+([0-9]+)", blk))
    fl <- do.call(rbind, fl[lengths(fl) == 3L])
    fm <- fw <- NA_character_
    if (!is.null(fl)) { fm <- fl[fl[, 2] == "men", 3][1]; fw <- fl[fl[, 2] == "women", 3][1] }
    return(list(pm = pm, pw = pw, fm = fm, fw = fw, where = paste(basename(lg), "(log)")))
  }
  cat("  no log holds the open-items table with the birth reports\n")
  NULL
}
# v-c9-e1, 19 October. E.1's numbers come from T0n_never_treated_births.csv,
# which reconcile_pre_birth_base.R writes to output/designs from the analysis
# frame; that frame carries had_child_this_wave and the event-study frame does
# not. pct_of_asked is the percent and report_then_later the count, as
# apply_unscreened_values_3.R read them on 18 October. The open-items file and
# the logs remain the fallbacks.
br <- list(pm = NA, pw = NA, fm = NA, fw = NA, where = oiq)
f_nt <- find1("T0n_never_treated_births.csv")
nt <- if (is.na(f_nt)) NULL else utils::read.csv(f_nt, stringsAsFactors = FALSE)
if (!is.null(nt) && all(c("sex", "pct_of_asked", "report_then_later") %in% names(nt))) {
  print(nt, row.names = FALSE)
  g_nt <- function(sx, col, fmt) {
    v <- nt[[col]][nt$sex == sx]
    if (length(v) == 1L && !is.na(v)) sprintf(fmt, v) else NA_character_
  }
  br <- list(pm = g_nt("men", "pct_of_asked", "%.1f"), pw = g_nt("women", "pct_of_asked", "%.1f"),
             fm = g_nt("men", "report_then_later", "%.0f"), fw = g_nt("women", "report_then_later", "%.0f"),
             where = "T0n_never_treated_births.csv")
} else if (!is.null(oi) && any(grepl("reporting a birth", oi$query, fixed = TRUE))) {
  br <- list(pm = q_note("reporting a birth", "men"), pw = q_note("reporting a birth", "women"),
             fm = q_val("followed by later waves", "men"), fw = q_val("followed by later waves", "women"),
             where = oiq)
} else {
  cat("  The file has no birth-report rows: the run that wrote it had no\n",
      "  had_child_this_wave on its frame. Reading them from the logs instead.\n", sep = "")
  lg <- find_oi_log()
  if (!is.null(lg)) br <- lg
}
add(t2[["pm"]], br$pm, paste(br$where, "never-treated men reporting a birth, percent"))
add(t2[["pw"]], br$pw, paste(br$where, "never-treated women reporting a birth, percent"))
add(t2[["fm"]], br$fm, paste(br$where, "never-treated men, report followed by later waves"))
add(t2[["fw"]], br$fw, paste(br$where, "never-treated women, report followed by later waves"))

# ---- 3. Trainees behind the fee-share ratio (B.2) ---------------------------------------
cat("\n--- 3. Trainees behind the women-to-men fee-share ratio (B.2) ---\n")
# v-c9-roster: the four counts are read from the manuscript.
t3 <- local({
  a <- grep("three years after the birth, rests on", tex, value = TRUE, fixed = TRUE)
  b <- grep("women one year before the birth to", tex, value = TRUE, fixed = TRUE)
  m1 <- if (length(a)) regmatches(a[1], regexec("rests on ([0-9]+) men and ([0-9]+) women", a[1]))[[1]] else character(0)
  m2 <- if (length(b)) regmatches(b[1], regexec("from ([0-9]+) women one year before the birth to ([0-9]+) six", b[1]))[[1]] else character(0)
  if (length(m1) != 3L || length(m2) != 3L)
    cat("  the trainee counts were not read from the manuscript; using the 18 October text\n")
  c(if (length(m1) == 3L) m1[2:3] else c("237", "224"), if (length(m2) == 3L) m2[2:3] else c("472", "154"))
})
cat(sprintf("  text: rests on %s men and %s women; from %s women at k = -1 to %s at k = 6\n",
            t3[1], t3[2], t3[3], t3[4]))
f_hz <- find1("T3c_fee_by_horizon_decomposed.csv")
hz <- if (is.na(f_hz)) NULL else utils::read.csv(f_hz, stringsAsFactors = FALSE)
if (!is.null(hz) && !all(c("k", "female", "n", "p_t", "p_s", "share") %in% names(hz))) {
  cat("  the file lacks k, female, n, p_t, p_s or share\n"); hz <- NULL
}
tr <- function(kk, f) NA_character_
if (!is.null(hz)) {
  hz$trainees <- hz$n * hz$p_t
  m <- hz[hz$female == 0, ]; w <- hz[hz$female == 1, ]
  ks <- sort(intersect(m$k, w$k))
  sh <- function(d) { r <- d[match(ks, d$k), ]; ifelse(r$p_t > 0, 100 * r$p_s / r$p_t, NA_real_) }
  tb <- data.frame(k = ks,
                   n_men = m$n[match(ks, m$k)], trainees_men = round(m$trainees[match(ks, m$k)], 1),
                   n_women = w$n[match(ks, w$k)], trainees_women = round(w$trainees[match(ks, w$k)], 1),
                   share_men = round(sh(m), 1), share_women = round(sh(w), 1),
                   ratio_w_to_m = round(sh(w) / sh(m), 2))
  print(tb, row.names = FALSE)
  cat(sprintf("  trainees = n x p_t. The ratio is largest at k = %d.\n",
              tb$k[which.max(tb$ratio_w_to_m)]))
  tr <- function(kk, f) {
    v <- hz$trainees[hz$k == kk & hz$female == f]
    if (length(v) == 1L) r0(v) else NA_character_
  }
}
hzf <- "T3c_fee_by_horizon_decomposed.csv"
add(t3[1], tr(3, 0),  paste(hzf, "n x p_t: men, k = 3"))
add(t3[2], tr(3, 1),  paste(hzf, "n x p_t: women, k = 3"))
add(t3[3], tr(-1, 1), paste(hzf, "n x p_t: women, k = -1"))
add(t3[4], tr(6, 1),  paste(hzf, "n x p_t: women, k = 6"))

# v-c9-followup, 19 October. The sentence now gives only the range of the ratio
# before the birth and from it. The paper computes the ratio from the unrounded
# shares, as ratio_w_to_m above does; the ratio of the shares as printed to one
# decimal is shown beside it, because a reader dividing those gets that one.
s3 <- grep("women-to-men ratio runs from", tex, value = TRUE, fixed = TRUE)
mm <- if (length(s3)) regmatches(s3[1], gregexpr("from [0-9.]+ to [0-9.]+", s3[1]))[[1]] else character(0)
pp <- if (length(mm) >= 2L)
  do.call(rbind, strsplit(sub("^from ", "", utils::tail(mm, 2L)), " to ", fixed = TRUE)) else NULL
if (is.null(pp)) {
  cat("  the ratio sentence was not read from the manuscript; using the 18 October text\n")
  pp <- rbind(c("0.95", "1.30"), c("1.30", "1.96"))
} else cat("  text (", basename(tex_file), "): ", trimws(s3[1]), "\n", sep = "")
rr <- rp3 <- rep(NA_character_, 4)
if (!is.null(hz)) {
  st3 <- function(f, kk, printed) {
    r <- hz[hz$female == f & hz$k == kk, ]
    if (nrow(r) != 1L || !isTRUE(r$p_t > 0)) return(NA_real_)
    v <- 100 * r$p_s / r$p_t
    if (printed) as.numeric(sprintf("%.1f", v)) else v
  }
  rng3 <- function(printed) {
    ratio <- vapply(ks, function(kk) st3(1, kk, printed) / st3(0, kk, printed), numeric(1))
    pre <- ratio[ks %in% -4:-1]; post <- ratio[ks %in% 0:6]
    c(min(pre), max(pre), min(post), max(post))
  }
  u3 <- rng3(FALSE); p3 <- rng3(TRUE)
  rr <- sprintf("%.2f", u3); rp3 <- sprintf("%.2f", p3)
  cat(sprintf("  ratio of the unrounded shares: %.4f to %.4f before the birth, %.4f to %.4f from it\n",
              u3[1], u3[2], u3[3], u3[4]))
  cat(sprintf("  ratio of the printed shares:   %.4f to %.4f before the birth, %.4f to %.4f from it\n",
              p3[1], p3[2], p3[3], p3[4]))
  dd3 <- rr != rp3
  if (any(dd3))
    cat("  The two rules differ at the ", paste(c("lowest before", "highest before", "lowest from",
        "highest from")[dd3], collapse = ", "), " value: ", paste(rr[dd3], collapse = ", "),
        " unrounded against ", paste(rp3[dd3], collapse = ", "), " from the printed shares.\n", sep = "")
}
add(pp[1, 1], rr[1], paste(hzf, "women-to-men ratio of unrounded shares: lowest, k = -4..-1"))
add(pp[1, 2], rr[2], paste(hzf, "women-to-men ratio of unrounded shares: highest, k = -4..-1"))
add(pp[2, 1], rr[3], paste(hzf, "women-to-men ratio of unrounded shares: lowest, k = 0..6"))
add(pp[2, 2], rr[4], paste(hzf, "women-to-men ratio of unrounded shares: highest, k = 0..6"))

# ---- 4. Fieldwork span (A.4) ------------------------------------------------------------
cat("\n--- 4. Fieldwork span by wave, Waves 12-24 (A.4) ---\n")
cat("  text: \"a span of 195 to 230 days ... the last four waves average 219 days against\n",
    "        201 for the first nine\"\n", sep = "")
f_fw <- find1("v2114_fieldwork_window.csv")
g4 <- list(lo = NA, hi = NA, late = NA, early = NA)
if (!is.na(f_fw)) {
  fw <- utils::read.csv(f_fw, stringsAsFactors = FALSE)
  if (!all(c("wave", "span_days") %in% names(fw))) {
    cat("  the file lacks wave or span_days\n")
  } else {
    fw$wave <- suppressWarnings(as.numeric(fw$wave))
    fw <- fw[!is.na(fw$wave) & fw$wave >= 12 & fw$wave <= 24, , drop = FALSE]
    print(fw[, intersect(c("wave", "n", "first", "last", "span_days"), names(fw)), drop = FALSE],
          row.names = FALSE)
    if (nrow(fw) == 13L) {
      late <- mean(fw$span_days[fw$wave >= 21]); early <- mean(fw$span_days[fw$wave <= 20])
      cat(sprintf("  mean span: Waves 21-24 %.2f days, Waves 12-20 %.2f days\n", late, early))
      g4 <- list(lo = r0(min(fw$span_days)), hi = r0(max(fw$span_days)),
                 late = r0(late), early = r0(early))
    } else cat("  expected 13 waves, found ", nrow(fw), "\n", sep = "")
  }
}
fwf <- "v2114_fieldwork_window.csv"
add("195", g4$lo,    paste(fwf, "span_days: shortest, Waves 12-24"))
add("230", g4$hi,    paste(fwf, "span_days: longest, Waves 12-24"))
add("219", g4$late,  paste(fwf, "span_days: mean, Waves 21-24"))
add("201", g4$early, paste(fwf, "span_days: mean, Waves 12-20"))

# ---- 5. Fee rates either side of the birth (B.2) ----------------------------------------
cat("\n--- 5. Fee rates either side of the birth, by sex (B.2) ---\n")
# v-c9-roster: the twelve numbers are read from the three B.2 sentences, so a
# corrected sentence is checked as it now stands. Each sentence gives women's
# pair, then men's.
s5  <- grep("the share runs from", tex, value = TRUE, fixed = TRUE)
s5p <- grep("contribute to a training cost falls from", tex, value = TRUE, fixed = TRUE)
s5i <- grep("training incidence falls from", tex, value = TRUE, fixed = TRUE)
nums5 <- function(s, dflt) {
  p <- if (length(s)) regmatches(s[1], gregexpr("from [0-9.]+ to [0-9.]+", s[1]))[[1]] else character(0)
  if (length(p) < 2L) return(dflt)
  unlist(strsplit(sub("^from ", "", p[1:2]), " to ", fixed = TRUE))
}
shr5 <- nums5(s5,  c("25.1", "25.9", "23.5", "17.8"))
pay5 <- nums5(s5p, c("10.7", "8.0", "7.7", "5.2"))
inc5 <- nums5(s5i, c("42.7", "31.1", "32.6", "29.4"))
if (!(length(s5) && length(s5p) && length(s5i)))
  cat("  one or more B.2 sentences were not read from the manuscript; using the 18 October text for it\n")
for (s in c(s5[1], s5p[1], s5i[1])) if (!is.na(s)) cat("  text: ", trimws(s), "\n", sep = "")
tv <- c(w_inc_pre = inc5[1], w_inc_post = inc5[2], w_pay_pre = pay5[1], w_pay_post = pay5[2],
        w_shr_pre = shr5[1], w_shr_post = shr5[2],
        m_inc_pre = inc5[3], m_inc_post = inc5[4], m_pay_pre = pay5[3], m_pay_post = pay5[4],
        m_shr_pre = shr5[3], m_shr_post = shr5[4])
best <- NULL; reword <- FALSE
if (!is.null(hz)) {
  # 03b's own arithmetic: n-weighted means of p_t and p_s, printed to three
  # decimals, and of the share, printed to one. The text gives p_t and p_s in
  # percent, so they are read off the three-decimal value.
  wavg <- function(d, ks, col) { d <- d[d$k %in% ks, ]; sum(d[[col]] * d$n) / sum(d$n) }
  pct  <- function(x) sprintf("%.1f", 100 * as.numeric(sprintf("%.3f", x)))
  one  <- function(d, pre, post) c(
    pct(wavg(d, pre, "p_t")), pct(wavg(d, post, "p_t")),
    pct(wavg(d, pre, "p_s")), pct(wavg(d, post, "p_s")),
    sprintf("%.1f", wavg(d, pre, "share")), sprintf("%.1f", wavg(d, post, "share")))
  wins <- list("k -3..-1 | 0..2"        = list(-3:-1, 0:2),
               "k -3..-1 | 1..3"        = list(-3:-1, 1:3),
               "k -4..-1 | 0..6 (03b)"  = list(-4:-1, 0:6))
  qlab <- paste(rep(c("women", "men"), each = 6),
                rep(c("incidence, p_t", "contribution, p_s", "trainee share"), each = 2, times = 2),
                rep(c("before", "after"), times = 6), sep = ", ")
  cmp <- do.call(rbind, lapply(wins, function(x)
    c(one(hz[hz$female == 1, ], x[[1]], x[[2]]), one(hz[hz$female == 0, ], x[[1]], x[[2]]))))
  colnames(cmp) <- names(tv)
  hits <- apply(cmp, 1, function(r) sum(r == tv))
  out5 <- data.frame(quantity = qlab, text = tv, t(cmp), row.names = NULL,
                     check.names = FALSE, stringsAsFactors = FALSE)
  out5 <- rbind(out5, c("of 12, matching the text", "", as.character(hits)))
  print(out5, row.names = FALSE, right = FALSE)
  b <- which.max(hits)
  best <- cmp[b, ]
  cat(sprintf("  closest: %s, %d of 12 match\n", names(wins)[b], hits[b]))
  # Warn only while the sentence still says three-wave, or when it cannot be read.
  reword <- hits[b] == 12L && !startsWith(names(wins)[b], "k -3") &&
    (!length(s5) || grepl("three-wave", s5[1], fixed = TRUE))
  if (reword)
    cat("  *** These are averages over the four waves before the birth and the seven\n",
        "      from it, not three-wave averages. Reword the B.2 sentence; the numbers\n",
        "      stand.\n", sep = "")
}
g5 <- function(nm) if (is.null(best)) NA_character_ else unname(best[nm])
avg <- paste(hzf, "n-weighted mean, part 5:")
lab5 <- c(m_inc_pre = "men's p_t before", w_shr_pre = "women's share before",
          m_inc_post = "men's p_t after", w_inc_pre = "women's p_t before",
          w_inc_post = "women's p_t after", w_pay_pre = "women's p_s before",
          w_pay_post = "women's p_s after", m_pay_pre = "men's p_s before",
          m_pay_post = "men's p_s after", w_shr_post = "women's share after",
          m_shr_pre = "men's share before", m_shr_post = "men's share after")
for (nm in names(lab5)) add(unname(tv[[nm]]), g5(nm), paste(avg, lab5[[nm]]))

# ---- 6. Trainees who contribute to the cost, four groups (B.2) ---------------------------
cat("\n--- 6. Trainees who contribute to the cost, four groups (B.2) ---\n")
cat("  text: \"mothers contribute to the cost in 25.5 percent of person-waves against 22.1\n",
    "        for never-mothers, while fathers are at 20.2 and childless men at 18.8\"\n", sep = "")
f_lv <- find1("T3c_fee_level_by_group.csv")
g6 <- function(g) NA_character_
if (!is.na(f_lv)) {
  lv <- utils::read.csv(f_lv, stringsAsFactors = FALSE)
  if (all(c("grp4", "pct_contributed") %in% names(lv))) {
    lv$round_1   <- round(lv$pct_contributed, 1)
    lv$sprintf_1 <- sprintf("%.1f", lv$pct_contributed)
    print(lv, row.names = FALSE)
    tie <- lv$grp4[abs(lv$round_1 - as.numeric(lv$sprintf_1)) > 1e-9]
    if (length(tie))
      cat("  round() and sprintf() disagree for ", paste(tie, collapse = ", "), ". Check 9\n",
          "  rounds with round(), so it cannot find the value the text prints.\n", sep = "")
    g6 <- function(g) {
      v <- lv$sprintf_1[lv$grp4 == g]
      if (length(v) == 1L) v else NA_character_
    }
  } else cat("  the file lacks grp4 or pct_contributed\n")
}
lvf <- "T3c_fee_level_by_group.csv pct_contributed:"
add("22.1", g6("never_mothers"), paste(lvf, "never-mothers"))
add("25.5", g6("mothers"),       paste(lvf, "mothers"))
add("20.2", g6("fathers"),       paste(lvf, "fathers"))
add("18.8", g6("childless_men"), paste(lvf, "childless men"))

# ---- 7. Bootstrap probabilities (B.6) ------------------------------------------------------
# v-c9-roster, 19 October. B.6 prints the roster arm's bootstrap through \pn{}
# keys (Grprob, Grboot) from REFEREE_RUN.R block G2, and pn_check() verifies
# their values. What is left to check here is that no sentence still types a
# replicate share, as B.6 did with 88.1 and 94.6.
cat("\n--- 7. Bootstrap replicate shares (B.6) ---\n")
f_pr <- find1("referee_G_fee_states_roster_probs.csv")
if (!is.na(f_pr)) print(utils::read.csv(f_pr, stringsAsFactors = FALSE), row.names = FALSE)
b6    <- grep("percent of (bootstrap )?replicates", tex, value = TRUE)
typed <- b6[grepl("[0-9] percent of (bootstrap )?replicates", b6)]
keyed <- b6[grepl("\\\\pn\\{Grprob\\.[^}]+\\} percent of (bootstrap )?replicates", b6)]
cat(sprintf("  sentences on replicate shares: %d; through Grprob keys: %d; typed: %d\n",
            length(b6), length(keyed), length(typed)))
if (length(typed)) cat(paste0("    typed: ", substr(trimws(typed), 1, 120), collapse = "\n"), "\n", sep = "")
add("keys", if (!length(b6)) NA_character_ else if (length(typed)) "typed" else "keys",
    "B.6: replicate shares print through \\pn{Grprob.*} keys")

# ---- 8. The never-treated drift (E.1) -------------------------------------------------------
cat("\n--- 8. The never-treated drift (E.1) ---\n")
# v-c9-followup, 19 October: the drift is read from the manuscript.
t8 <- local({
  a <- grep("never-treated placebo drifts at", tex, value = TRUE, fixed = TRUE)
  m <- if (length(a)) regmatches(a[1], regexpr("[0-9]*[.][0-9]+(?=[$]? per wave)", a[1], perl = TRUE)) else character(0)
  if (length(m)) m else { cat("  the drift sentence was not read from the manuscript\n"); "0.00123" }
})
cat("  text: the never-treated placebo drifts at +", t8, " per wave in the gap\n", sep = "")
cat("  Part 1 shows where Check 9 matched it; the decisions record is the source.\n")
f_dec <- find1("training_decisions.txt")
g8 <- NA_character_
if (!is.na(f_dec)) {
  x <- grep("never-treated drift", readLines(f_dec, warn = FALSE), value = TRUE, fixed = TRUE)
  if (length(x)) {
    cat(paste0("  ", x, collapse = "\n"), "\n", sep = "")
    v <- regmatches(x[1], regexpr("[-+]?[0-9]*[.][0-9]+(?= per wave)", x[1], perl = TRUE))
    if (length(v)) g8 <- sub("^[+]", "", v)
  } else cat("  no line in it records the never-treated drift\n")
}
add(t8, g8, "training_decisions.txt T2: the never-treated drift per wave")

# ---- Verdicts ---------------------------------------------------------------------------------
res$verdict <- ifelse(is.na(res$source_gives), "NOT FOUND",
                      ifelse(res$source_gives == res$literal, "matches", "DIFFERS"))
cat("\n", strrep("=", 78), "\nVERDICTS\n", strrep("=", 78), "\n", sep = "")
cat("Entries vm_settings.R accepts or confirms, marked [c9] there:\n")
print(res[res$flagged, c("literal", "source_gives", "verdict", "where")], row.names = FALSE, right = FALSE)
cat("\nTheir partners, which share a sentence or a source with them and need no entry:\n")
print(res[!res$flagged, c("literal", "source_gives", "verdict", "where")], row.names = FALSE, right = FALSE)

bad9 <- res$literal[res$flagged & res$verdict != "matches"]
badp <- res$literal[!res$flagged & res$verdict != "matches"]
cat("\n")
if (!length(bad9) && !length(badp)) {
  cat("All ", nrow(res), " match. vm_settings.R stands; run the verifier.\n", sep = "")
} else {
  if (length(bad9))
    cat("Entries that did not match: ", paste(bad9, collapse = ", "), ". Remove ",
        if (length(bad9) == 1L) "it" else "them",
        " from vm_settings.R\nbefore running the verifier.\n", sep = "")
  if (length(badp))
    cat("Partners that did not match: ", paste(badp, collapse = ", "),
        ". Their own sources do not give\nthe numbers the text prints; the text or the source needs a look.\n",
        sep = "")
}
if (reword)
  cat("B.2 calls the averages in part 5 three-wave averages; they are not. Reword it.\n")

cat("\n", strrep("=", 78), "\n", sep = "")
cat("check9_sources.R  completed ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", sep = "")
cat(strrep("=", 78), "\n", sep = "")

}), finally = {
  options(.c9_op)
  if (.c9_owns && sink.number() > 0L) sink()
  if (.c9_owns) cat("Log: ", .c9_log, "\n", sep = "")
  rm(list = intersect(c(".c9_owns", ".c9_log", ".c9_op"), ls(globalenv(), all.names = TRUE)),
     envir = globalenv())
})
