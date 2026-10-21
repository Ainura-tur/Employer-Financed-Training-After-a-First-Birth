# =============================================================================
# compare_runs.R   14 October 2026
#
# What dropping the screen moved. Compares the screened run that
# archive_screened_run.R kept with the outputs of the run without the screen.
#
# THREE COMPARISONS, each written to output/diagnostics:
#   screen_compare_estimates.csv  every estimate that has a standard error,
#                                 in every csv both runs wrote, plus the
#                                 ledger: old and new estimate, SE and n, the
#                                 change, and the change in units of the old
#                                 SE
#   screen_compare_pn.csv         every \pn number, old and new
#   screen_compare_macros.csv     every macro paper_tables.R writes
# It prints the principal estimates first, then the largest movers.
#
# READ THE VINTAGES IT PRINTS. A file that one run did not rewrite compares
# an older pass with a newer one, and a change there is not the screen.
#
# AND THE WEIGHTS. Once CS_event_study.R carries the common-weight patch, every
# women-minus-men Sun-Abraham quantity is formed on common cohort weights. If
# the archive was built on the earlier, separate weights, those quantities move
# for two reasons at once. The script says so, and shows the screen alone on
# the separate weights for the six averages; the clean comparison is against a
# screened pass on the same weights, archived with AR_LABEL <- "cw".
#
# RUN IT after the pass without the screen, from the project root:
#     source("compare_runs.R")
# It reads the newest archive/screened_* folder; set CMP_OLD to a folder to
# choose another. It writes only the three csvs.
# =============================================================================

suppressPackageStartupMessages(library(dplyr))

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

if (!exists("PROJ_ROOT")) {
  if (!file.exists("00_contract.R"))
    stop("Run this from the project root, the folder that holds 00_contract.R.",
         call. = FALSE)
  source("00_contract.R")
}
.cr_log <- exists("td_log_open", mode = "function")
if (.cr_log) try(td_log_open("compare_runs.R"), silent = TRUE)

.cr_done <- FALSE
.cr_w <- options(width = 160)
tryCatch(local({

NEW <- normalizePath(PROJ_ROOT, winslash = "/")
OLD <- if (exists("CMP_OLD", envir = globalenv())) get("CMP_OLD", envir = globalenv()) else {
  a <- list.files(file.path(NEW, "archive"), pattern = "^screened_", full.names = TRUE)
  a <- a[dir.exists(a)]
  if (!length(a)) stop("no archive/screened_* folder. Run archive_screened_run.R ",
                       "before the pass without the screen.", call. = FALSE)
  a[order(file.info(a)$mtime, decreasing = TRUE)][1]
}
OLD <- normalizePath(OLD, winslash = "/", mustWork = TRUE)
DIAG <- get0("DIR_DIAG", ifnotfound = file.path(NEW, "output", "diagnostics"))
dir.create(DIAG, showWarnings = FALSE, recursive = TRUE)
cat("old (screened):   ", OLD, "\n", sep = "")
if (file.exists(file.path(OLD, "README.txt")))
  cat(paste0("                  ", readLines(file.path(OLD, "README.txt"))[2]), "\n")
cat("new (rule ", get0("TD_SAMPLE_RULE", ifnotfound = "?"), "): ", NEW, "\n", sep = "")

.n   <- function(x) format(x, big.mark = ",")
SUBS <- c("output/designs", "output/results", "output/diagnostics",
          "output/fte_experience")
# The comparison's own files, and the screen audit's, describe both runs at
# once, so comparing them across runs says nothing.
SKIP <- "^(screen_compare_|screen_audit_)"

.read <- function(p) tryCatch(utils::read.csv(p, stringsAsFactors = FALSE,
                                              check.names = FALSE),
                              error = function(e) NULL)

# =============================================================================
# 1. ESTIMATES
# =============================================================================
ID_NUM  <- c("k", "k_lo", "k_hi", "event_time", "horizon", "wave", "year",
             "years", "window_lo", "window_hi", "lo_k", "hi_k", "rel", "e", "h",
             "female", "sex_num")
NOT_KEY <- c("note", "notes", "comment", "written", "checked", "build_tag",
             "files", "inputs", "when", "timestamp", "built_at", "status",
             "verdict", "verdict_f", "reproduced", "reproduces", "stamp", "run",
             "date", "time", "mtime", "vintage", "source_file", "file", "path_file",
             "fe", "module", "rule", "rule_in_force", "src")
EST_LIKE <- c("est", "estimate", "att", "gap", "coef", "b", "value", "delta",
              "diff")
SPECIAL  <- c(gap_slope_per_year = "gap_slope_se")
N_COLS   <- c("n", "nobs", "n_obs", "N", "rows")

.pairs <- function(d) {
  num <- names(d)[vapply(d, is.numeric, logical(1))]
  out <- list()
  for (x in num) {
    if (grepl("^(se|se_.*|.*_se|t|t_.*|p|p_.*|.*_t|.*_p|mde.*|n|n_.*)$", x)) next
    cand <- c(SPECIAL[x], paste0("se_", x), paste0(x, "_se"),
              sub("_(est|gap|att|estimate|coef)$", "_se", x),
              if (x %in% EST_LIKE) "se")
    cand <- unique(cand[!is.na(cand) & cand != x])
    hit  <- cand[cand %in% num][1]
    if (!is.na(hit)) out[[x]] <- hit
  }
  out
}
.ids <- function(d) {
  ids <- names(d)[!vapply(d, is.numeric, logical(1))]
  ids <- c(ids, intersect(ID_NUM, names(d)[vapply(d, is.numeric, logical(1))]))
  setdiff(ids, NOT_KEY)
}
.key <- function(d, ids) {
  k <- if (length(ids)) do.call(paste, c(lapply(ids, function(i)
    paste0(i, "=", as.character(d[[i]]))), sep = "; ")) else rep("", nrow(d))
  paste0(k, "#", stats::ave(seq_along(k), k, FUN = seq_along))
}

.compare_csv <- function(rel) {
  o <- .read(file.path(OLD, rel)); w <- .read(file.path(NEW, rel))
  if (is.null(o) || is.null(w) || !nrow(o) || !nrow(w)) return(NULL)
  pr <- .pairs(w); pr <- pr[names(pr) %in% names(o) & unlist(pr) %in% names(o)]
  if (!length(pr)) return(NULL)
  ids <- intersect(.ids(w), .ids(o))
  ko <- .key(o, ids); kw <- .key(w, ids)
  nc <- intersect(N_COLS, intersect(names(o), names(w)))[1]
  res <- lapply(names(pr), function(x) {
    m <- match(kw, ko)
    data.frame(source = rel, key = sub("#1$", "", kw), quantity = x,
               old_est = o[[x]][m], old_se = o[[pr[[x]]]][m],
               new_est = w[[x]], new_se = w[[pr[[x]]]],
               old_n = if (!is.na(nc)) o[[nc]][m] else NA_real_,
               new_n = if (!is.na(nc)) w[[nc]] else NA_real_,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, res)
}

# The ledger: n and SE in training_ledger.csv, the estimate in the sealed list.
.ledger <- function(root) {
  f <- file.path(root, "output", "designs", "training_ledger.csv")
  b <- file.path(root, "output", "designs", "BLIND_training_estimates.rds")
  if (!file.exists(f)) return(NULL)
  L <- .read(f); B <- if (file.exists(b)) tryCatch(readRDS(b), error = function(e) NULL)
  if (is.null(L) || !"tag" %in% names(L)) return(NULL)
  .g <- function(tag, rx) {
    m <- if (is.list(B)) B[[tag]] else NULL
    if (!is.matrix(m)) return(NA_real_)
    j <- grep(rx, colnames(m))[1]
    if (is.na(j)) NA_real_ else unname(m[1, j])
  }
  data.frame(tag = L$tag, label = if ("label" %in% names(L)) L$label else L$tag,
             status = if ("status" %in% names(L)) L$status else NA_character_,
             est = vapply(L$tag, .g, numeric(1), rx = "^Estimate"),
             se  = vapply(L$tag, .g, numeric(1), rx = "Std\\. *Error"),
             se_ledger = if ("se" %in% names(L)) L$se else NA_real_,
             n = if ("n" %in% names(L)) L$n else NA_real_,
             stringsAsFactors = FALSE)
}
LO <- .ledger(OLD); LN <- .ledger(NEW)
LED <- if (!is.null(LO) && !is.null(LN)) {
  m <- match(LN$tag, LO$tag)
  data.frame(source = "ledger", key = LN$tag, quantity = "post_f",
             old_est = LO$est[m], old_se = ifelse(is.na(LO$se[m]), LO$se_ledger[m], LO$se[m]),
             new_est = LN$est, new_se = ifelse(is.na(LN$se), LN$se_ledger, LN$se),
             old_n = LO$n[m], new_n = LN$n, stringsAsFactors = FALSE)
} else NULL

# Every csv both runs wrote, by its path relative to the root.
.list <- function(root) unlist(lapply(SUBS, function(s) {
  d <- file.path(root, s)
  if (!dir.exists(d)) return(character(0))
  f <- list.files(d, pattern = "[.]csv$", recursive = TRUE)
  f <- f[!grepl(SKIP, basename(f))]
  file.path(s, f)
}))
REL <- intersect(.list(OLD), .list(NEW))
cat("\n", length(REL), " csv file(s) are in both runs; ", sum(!.list(NEW) %in% REL),
    " only in the new one, ", sum(!.list(OLD) %in% REL), " only in the old.\n", sep = "")

EST <- do.call(rbind, c(list(LED), lapply(REL, .compare_csv)))
if (is.null(EST) || !nrow(EST)) stop("nothing comparable was found in the two runs.",
                                     call. = FALSE)
EST$change      <- EST$new_est - EST$old_est
EST$change_in_se <- EST$change / EST$old_se
EST$se_ratio    <- EST$new_se / EST$old_se
# A file the new pass did not rewrite compares a run with itself, and one
# that did not move at all is reported as such rather than as a finding.
.mt <- function(root, rel) if (identical(rel, "ledger"))
  file.info(file.path(root, "output", "designs", "training_ledger.csv"))$mtime else
  file.info(file.path(root, rel))$mtime
.src <- unique(EST$source)
.vin <- data.frame(source = .src,
                   old_written = as.POSIXct(vapply(.src, function(s) as.numeric(.mt(OLD, s)), 0),
                                            origin = "1970-01-01"),
                   new_written = as.POSIXct(vapply(.src, function(s) as.numeric(.mt(NEW, s)), 0),
                                            origin = "1970-01-01"),
                   stringsAsFactors = FALSE)
EST <- merge(EST, .vin, by = "source", all.x = TRUE, sort = FALSE)
EST$same_file <- !is.na(EST$old_written) & !is.na(EST$new_written) &
  abs(as.numeric(EST$new_written) - as.numeric(EST$old_written)) < 1
utils::write.csv(EST, file.path(DIAG, "screen_compare_estimates.csv"), row.names = FALSE)

.stale <- unique(EST$source[EST$same_file])
if (length(.stale)) {
  cat("\n  *** NOT REWRITTEN by the new pass, so these compare a run with itself:\n")
  for (s in .stale) cat("      ", s, "\n", sep = "")
}

# ---- the principal estimates ------------------------------------------------------
.pick <- function(src, rx_key, qty, label) {
  r <- EST[EST$source == src & grepl(rx_key, EST$key) & EST$quantity == qty, , drop = FALSE]
  if (!nrow(r)) return(data.frame(label = label, old = "not found", new = "",
                                  change_in_se = NA_real_, n = "", note = "",
                                  stringsAsFactors = FALSE))
  r <- r[1, ]
  data.frame(label = label,
             old = sprintf("%+.4f (%.4f)", r$old_est, r$old_se),
             new = sprintf("%+.4f (%.4f)", r$new_est, r$new_se),
             change_in_se = round(r$change_in_se, 2),
             n = if (is.na(r$old_n) && is.na(r$new_n)) "" else
               paste(.n(r$old_n), "->", .n(r$new_n)),
             note = if (isTRUE(r$same_file)) "NOT REWRITTEN" else "",
             stringsAsFactors = FALSE)
}
.cs <- "output/results/cs_post_period_att.csv"
PRINCIPAL <- rbind(
  .pick(.cs, "sex=gap; outcome=jttpewt_emp(;|#|$)", "att",
        "SA: employer-financed training, employed (headline)"),
  .pick(.cs, "sex=gap; outcome=jttpewt_any(;|#|$)", "att",
        "SA: employer-financed training, unconditional"),
  .pick(.cs, "sex=gap; outcome=employed(;|#|$)", "att", "SA: employed"),
  .pick(.cs, "sex=gap; outcome=ln_hourly_wage_real(;|#|$)", "att", "SA: log hourly wage"),
  .pick(.cs, "sex=gap; outcome=ln_hourly_wage_real_agefe(;|#|$)", "att",
        "SA: log hourly wage, age effects"),
  .pick(.cs, "sex=gap; outcome=experience_years(;|#|$)", "att", "SA: years of experience"),
  .pick("ledger", "^T1_attached$", "post_f", "T1 attached (designated headline)"),
  .pick("ledger", "^T1_jttpewt_any_employed$", "post_f", "T1 employer-financed, employed"),
  .pick("ledger", "^T1_jttpeot_any_employed$", "post_f", "T1 own-time, employed"),
  .pick("ledger", "^T3_ddd$", "post_f", "T3 triple difference, sex-specific paths"),
  .pick("ledger", "^T3_ddd_saturated$", "post_f", "T3 triple difference, saturated"),
  .pick("ledger", "^T6_couple_x_wave$", "post_f", "T6 within couple"),
  .pick("ledger", "^T9_study_vs_training$", "post_f", "T9 study against training"),
  .pick("ledger", "^T4_any$", "post_f", "T4 full-time returners"),
  .pick("ledger", "^T4c_pre_birth_ft$", "post_f", "T4c full time before the birth"),
  .pick("ledger", "^T7_pregnancy_gap$", "post_f", "T7 anticipation"),
  .pick("output/results/wage_path_tests.csv",
        "path=plain; quantity=late window minus early window", "estimate",
        "wage window, late less early"),
  .pick("output/results/A5_crosspartial_coefs.csv", "sex=men", "gap_slope_per_year",
        "A5 tilt of the return-to-experience gap"))
cat("\n", strrep("=", 78), "\nPRINCIPAL ESTIMATES: screened (old) against unscreened (new)\n",
    strrep("=", 78), "\n", sep = "")
print(PRINCIPAL, row.names = FALSE, right = FALSE)

# ---- the Sun-Abraham weights of the two runs ---------------------------------------
# A gap_separate row in cs_post_period_att.csv means that run's gap rows are on
# common cohort weights.
.cw_of <- function(root) {
  x <- .read(file.path(root, .cs))
  !is.null(x) && "sex" %in% names(x) && any(x$sex == "gap_separate")
}
.cw_old <- .cw_of(OLD); .cw_new <- .cw_of(NEW)
cat("\n  Sun-Abraham weights: old run ", if (.cw_old) "common" else "separate",
    ", new run ", if (.cw_new) "common" else "separate", ".\n", sep = "")
if (!identical(.cw_old, .cw_new)) {
  cat("  *** THE WEIGHTS DIFFER, so every women-minus-men Sun-Abraham quantity\n",
      "      above and in the csvs moves with the weights as well as the screen.\n",
      "      Compare against a screened pass on the same weights (run\n",
      "      archive_screened_run.R after it with AR_LABEL <- \"cw\", then set\n",
      "      CMP_OLD to that folder), or read screen_audit_sa.csv, which forms\n",
      "      both estimators on both samples.\n", sep = "")
  if (.cw_new && !.cw_old) {
    .o <- .read(file.path(OLD, .cs)); .w <- .read(file.path(NEW, .cs))
    .lab <- c(jttpewt_emp = "SA: employer-financed training, employed (headline)",
              jttpewt_any = "SA: employer-financed training, unconditional",
              employed = "SA: employed", ln_hourly_wage_real = "SA: log hourly wage",
              ln_hourly_wage_real_agefe = "SA: log hourly wage, age effects",
              experience_years = "SA: years of experience")
    .sep <- do.call(rbind, lapply(names(.lab), function(oc) {
      a <- .o[.o$sex == "gap" & .o$outcome == oc, , drop = FALSE]
      b <- .w[.w$sex == "gap_separate" & .w$outcome == oc, , drop = FALSE]
      if (!nrow(a) || !nrow(b)) return(NULL)
      data.frame(label = .lab[[oc]],
                 old = sprintf("%+.4f (%.4f)", a$att[1], a$se[1]),
                 new = sprintf("%+.4f (%.4f)", b$att[1], b$se[1]),
                 change_in_se = round((b$att[1] - a$att[1]) / a$se[1], 2),
                 stringsAsFactors = FALSE)
    }))
    if (!is.null(.sep)) {
      cat("\n  the screen alone, on separate weights in both runs (old gap row against\n",
          "  new gap_separate row):\n", sep = "")
      print(.sep, row.names = FALSE, right = FALSE)
    }
  }
}

# ---- the experience arms, which the screen touched most directly -------------------
.t8 <- EST[grepl("T8", EST$source) | grepl("^T8", EST$key), , drop = FALSE]
if (nrow(.t8))
  cat(sprintf(paste0("\n  experience arms (T8): %d estimates, largest move %.2f of the",
                     " old SE.\n"), nrow(.t8), max(abs(.t8$change_in_se), na.rm = TRUE)))

# ---- everything, summarised ----------------------------------------------------------
.z <- abs(EST$change_in_se[is.finite(EST$change_in_se) & !EST$same_file])
cat(sprintf(paste0("\n  %s estimates compared from files both passes wrote. The change,",
                   " in old SEs:\n    under 0.25: %s | 0.25 to 0.5: %s | 0.5 to 1: %s |",
                   " 1 or more: %s | largest %.2f\n"),
            .n(length(.z)), .n(sum(.z < 0.25)), .n(sum(.z >= 0.25 & .z < 0.5)),
            .n(sum(.z >= 0.5 & .z < 1)), .n(sum(.z >= 1)),
            if (length(.z)) max(.z) else NA_real_))
.top <- EST[is.finite(EST$change_in_se) & !EST$same_file, , drop = FALSE]
.top <- .top[order(-abs(.top$change_in_se)), , drop = FALSE][seq_len(min(25, nrow(.top))), ]
if (nrow(.top)) {
  cat("\n  the 25 largest moves:\n")
  .show <- data.frame(source = basename(.top$source), key = substr(.top$key, 1, 60),
                      quantity = .top$quantity,
                      old = sprintf("%+.4f (%.4f)", .top$old_est, .top$old_se),
                      new = sprintf("%+.4f (%.4f)", .top$new_est, .top$new_se),
                      in_se = round(.top$change_in_se, 2), stringsAsFactors = FALSE)
  print(.show, row.names = FALSE, right = FALSE)
}

# =============================================================================
# 2. THE PAPER'S NUMBERS (\pn)
# =============================================================================
.pv <- function(root) .read(file.path(root, "output", "tables",
                                      "paper_numbers_provenance.csv"))
PO <- .pv(OLD); PN <- .pv(NEW)
if (is.null(PO) || is.null(PN)) {
  cat("\n  paper_numbers_provenance.csv is missing from ",
      if (is.null(PO)) "the old run" else "the new run", "; \\pn not compared.\n", sep = "")
} else {
  K <- union(PO$key, PN$key)
  PNC <- data.frame(key = K,
                    old_text = PO$text[match(K, PO$key)], new_text = PN$text[match(K, PN$key)],
                    old_value = PO$value[match(K, PO$key)], new_value = PN$value[match(K, PN$key)],
                    script = ifelse(is.na(PN$script[match(K, PN$key)]),
                                    PO$script[match(K, PO$key)], PN$script[match(K, PN$key)]),
                    stringsAsFactors = FALSE)
  PNC$changed <- is.na(PNC$old_text) | is.na(PNC$new_text) | PNC$old_text != PNC$new_text
  # The SE beside each estimate, where the key says which it is.
  .sek <- sub("[.](est|att|estimate|gap|coef)$", ".se", PNC$key)
  .has <- .sek != PNC$key & .sek %in% PNC$key
  PNC$change_in_se <- NA_real_
  PNC$change_in_se[.has] <- (PNC$new_value[.has] - PNC$old_value[.has]) /
    PNC$old_value[match(.sek[.has], PNC$key)]
  utils::write.csv(PNC, file.path(DIAG, "screen_compare_pn.csv"), row.names = FALSE)
  cat(sprintf(paste0("\n  \\pn: %s keys; %s print differently, %s are new, %s are gone.\n"),
              .n(nrow(PNC)), .n(sum(PNC$changed & !is.na(PNC$old_text) & !is.na(PNC$new_text))),
              .n(sum(is.na(PNC$old_text))), .n(sum(is.na(PNC$new_text)))))
  .zz <- PNC[is.finite(PNC$change_in_se), , drop = FALSE]
  if (nrow(.zz)) {
    .zz <- .zz[order(-abs(.zz$change_in_se)), , drop = FALSE][seq_len(min(15, nrow(.zz))), ]
    cat("  the \\pn estimates that moved most, in old SEs:\n")
    print(data.frame(key = substr(.zz$key, 1, 70), old = .zz$old_value, new = .zz$new_value,
                     in_se = round(.zz$change_in_se, 2)), row.names = FALSE, right = FALSE)
  }
}

# =============================================================================
# 3. THE MACROS paper_tables.R WRITES
# =============================================================================
.mac <- function(root) {
  f <- file.path(root, "output", "tables", "data_section_numbers.tex")
  if (!file.exists(f)) return(NULL)
  l <- readLines(f, warn = FALSE)
  rx <- "^\\\\(?:new|provide|renew)command\\*?\\{\\\\([A-Za-z@]+)\\}\\{(.*)\\}\\s*(%.*)?$"
  l <- l[grepl(rx, l, perl = TRUE)]
  m <- regmatches(l, regexec(rx, l, perl = TRUE))
  data.frame(name = vapply(m, `[`, "", 2), value = vapply(m, `[`, "", 3),
             stringsAsFactors = FALSE) %>% distinct(name, .keep_all = TRUE)
}
MO <- .mac(OLD); MN <- .mac(NEW)
if (is.null(MO) || is.null(MN)) {
  cat("\n  data_section_numbers.tex is missing from one run; macros not compared.\n")
} else {
  K <- union(MO$name, MN$name)
  MAC <- data.frame(name = K, old = MO$value[match(K, MO$name)],
                    new = MN$value[match(K, MN$name)], stringsAsFactors = FALSE)
  MAC$changed <- is.na(MAC$old) | is.na(MAC$new) | MAC$old != MAC$new
  utils::write.csv(MAC, file.path(DIAG, "screen_compare_macros.csv"), row.names = FALSE)
  cat(sprintf("\n  macros: %s, of which %s changed.\n", .n(nrow(MAC)), .n(sum(MAC$changed))))
  .cnt <- MAC[MAC$name %in% c("ExtractRows", "ExtractPersons", "PanelRows", "PanelPersons",
                              "ScreenDropped", "RosterPersons", "RosterRows",
                              "RosterEmployed", "WageRows", "WagePersons"), , drop = FALSE]
  if (nrow(.cnt)) { cat("  the sample counts:\n"); print(.cnt, row.names = FALSE, right = FALSE) }
}

cat("\n  wrote screen_compare_estimates.csv",
    if (exists("PNC")) ", screen_compare_pn.csv", if (exists("MAC")) ", screen_compare_macros.csv",
    "\n  to ", DIAG, "\n", sep = "")
.cr_done <<- TRUE
}), finally = {
  options(.cr_w)
  if (isTRUE(.cr_log) && exists("td_log_close", mode = "function"))
    try(td_log_close(if (isTRUE(.cr_done)) "completed" else "stopped"), silent = TRUE)
  rm(list = intersect(c(".cr_log", ".cr_done", ".cr_w"), ls(all.names = TRUE)))
})
