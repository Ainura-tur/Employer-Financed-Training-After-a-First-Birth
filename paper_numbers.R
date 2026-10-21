# =============================================================================
# paper_numbers.R
#
# Every number the paper quotes is registered where it is computed, written
# to one macro file, and used in the text by name. Checking a number then
# means looking up its key, not searching files for its value.
#
# In an analysis script, once its results exist:
#   source("paper_numbers.R")
#   pn_begin("03a_design_audit.R", build_tag = attr(fr, "build_tag"))
#   pn_register("selfemp_mothers_pre", 100 * mean(x), digits = 1,
#               note = "employed mothers, is_self_employed, k < 0")
#   pn_register_table(res, id = c("sample", "pair"), values = c(est = 3, se = 3),
#                     prefix = "R2")
#   pn_flush()                  # writes output/numbers/03a_design_audit.csv
#
# In paper_tables.R, just BEFORE its sync to the paper folder (the line that
# calls sync_paper("tables") or pt_sync_paper()), so the file is copied:
#   source("paper_numbers.R")
#   pn_write_tex(TEXDIR)        # writes paper_numbers.tex and paper_numbers_provenance.csv
#
# In the manuscript preamble:  \input{tables/paper_numbers}
# In the text:                 rises from \pn{selfemp_mothers_pre} percent before ...
# Before circulating:          pn_lint("els-cas-templates/Motherhood_training_v5.tex")
# If a key does not print:     pn_check("els-cas-templates/Motherhood_training_v5.tex")
# Moving typed numbers to keys: pn_apply(tex, map) with an explicit map; pn_candidates() to look keys up
#
# A number the text states in a form no table holds (a share as a percentage,
# a ratio of two estimates, a dollar amount to the hundred, a count in words)
# is declared once in paper_numbers_derived.R, in the project root, as a
# formula over registered keys:
#   pn_derive("R7single.transport.kge0.E1.share_explained.pct",
#             ~ 100 * R7single.transport.kge0.E1.share_explained, digits = 0)
# pn_write_tex() computes it from the registry each time it writes the macro
# file, so it cannot drift from the numbers it comes from.
#
# Three things fail loudly instead of printing a placeholder: a key the macro
# file does not define stops the compile; a key registered by two scripts, or
# registered on an older build than the newest one, stops pn_write_tex(); and
# pn_lint() stops when more results are typed in the text than max_typed
# allows. Set max_typed to today's count and lower it as sections move over.
# =============================================================================

suppressPackageStartupMessages(library(dplyr))

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

# The registry folder is anchored to the project root, so a script that
# registers and the script that writes the macro file find the same folder
# whatever their working directories.
if (!exists("PN_DIR"))
  PN_DIR <- file.path(if (exists("PROJ_ROOT") && is.character(PROJ_ROOT) && dir.exists(PROJ_ROOT))
                        PROJ_ROOT else getwd(), "output", "numbers")
# The derived numbers live beside this file, in the project root.
if (!exists("PN_DERIVED"))
  PN_DERIVED <- file.path(dirname(dirname(PN_DIR)), "paper_numbers_derived.R")
.PN <- new.env()

# ---- Opening files ---------------------------------------------------------------
# Every read and write goes through .pn_io(), so a failure names the file and
# says why: not there, or there but locked. On Windows, Excel locks a CSV for
# as long as it is open, and R can then neither read nor replace it; an
# editor holding a .tex file open does not lock it.
.pn_io <- function(expr, f, mode = c("read", "write")) {
  mode <- match.arg(mode); why <- ""
  withCallingHandlers(
    tryCatch(expr, error = function(e) {
      full <- normalizePath(f, winslash = "/", mustWork = FALSE)
      detail <- if (nzchar(why)) why else conditionMessage(e)
      msg <- if (mode == "read" && !file.exists(f))
        sprintf("Cannot read %s: no such file (looked from %s). Save it there or give the full path.",
                full, normalizePath(getwd(), winslash = "/"))
      else if (dir.exists(f)) sprintf("Cannot %s %s: it is a folder, not a file.", mode, full)
      else sprintf("Cannot %s %s: the file exists but is locked. If it is open in Excel or another program, close it and run again. (R: %s)",
                   mode, full, detail)
      stop(msg, call. = FALSE)
    }),
    warning = function(w) if (grepl("cannot open|not a regular file", conditionMessage(w))) {
      why <<- conditionMessage(w); invokeRestart("muffleWarning")
    })
}
# The content tag paper_tables.R stamps on every fragment it builds (its
# .content_tag()), computed the same way, so its fragment audit can tell a
# current copy of paper_numbers.tex from a stale one.
.pn_content_tag <- function(x) {
  z <- utf8ToInt(paste(x, collapse = "\n"))
  sprintf("%08x", as.integer(sum(as.numeric(z) * seq_along(z)) %% 2^31))
}
.pn_read_csv    <- function(f, ...) .pn_io(utils::read.csv(f, ...), f, "read")
.pn_read_lines  <- function(f, ...) .pn_io(readLines(f, ...), f, "read")
.pn_write_csv   <- function(x, f, ...) .pn_io(utils::write.csv(x, f, ...), f, "write")
.pn_write_lines <- function(x, f, ...) .pn_io(writeLines(x, f, ...), f, "write")

# ---- Registering -----------------------------------------------------------------

# Start a script's registry. Everything registered after this belongs to it.
pn_begin <- function(script, build_tag = NA_character_) {
  .PN$script <- script
  .PN$build_tag <- if (is.null(build_tag)) NA_character_ else as.character(build_tag)
  .PN$rows <- list()
  invisible(TRUE)
}

# Keys hold letters, digits, dots and underscores; anything else becomes _.
pn_key <- function(x) gsub("^_+|_+$", "", gsub("[^A-Za-z0-9._]+", "_", x))

# A row label made into a key part: operators become letters (k>=2 -> kge2,
# -8 -> m8), so labels that differ only in an operator stay apart.
pn_idval <- function(x) {
  x <- as.character(x)
  for (p in list(c(">=", "ge"), c("<=", "le"), c(">", "gt"), c("<", "lt"), c("=", "eq"), c("+", "plus")))
    x <- gsub(p[1], p[2], x, fixed = TRUE)
  x <- gsub("(?<![A-Za-z0-9])-(?=[0-9])", "m", x, perl = TRUE)
  gsub("\\.", "p", pn_key(x))
}

# The printed form, in TeX that works in text and in math alike.
pn_text <- function(value, digits, sign = FALSE, money = FALSE) {
  s <- formatC(abs(value), format = "f", digits = digits, big.mark = ",")
  s <- gsub(",", "{,}", s, fixed = TRUE)                 # a comma, not a math separator
  if (money) s <- paste0("\\$", s)
  lead <- if (value < 0 && round(abs(value), digits) > 0) "-" else if (sign && value > 0) "+" else ""
  paste0("\\ensuremath{", lead, s, "}")
}

# Rounding half away from zero, to any place: digits = -2 rounds to hundreds,
# for a dollar amount the text quotes to the hundred.
pn_round <- function(x, digits = 0) {
  m <- 10^digits
  sign(x) * floor(abs(x) * m + 0.5) / m
}

# A whole number from zero to twenty in words, for a count the text spells out.
pn_int_words <- function(x) {
  w <- c("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
         "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen",
         "eighteen", "nineteen", "twenty")
  n <- pn_round(x, 0)
  if (!is.finite(n) || n < 0 || n > 20) stop("pn_int_words() spells 0 to 20, not ", format(x), ".", call. = FALSE)
  w[n + 1]
}

# Register one number. value is what the script computed; scale turns a
# fraction into a percentage (scale = 100). words, if given, is printed
# instead of the number, so a phrase such as "seven-tenths" is tied to the
# value it describes; check_words() below keeps the two consistent.
pn_register <- function(key, value, digits = 3L, scale = 1, sign = FALSE, money = FALSE,
                        words = NULL, note = "") {
  if (is.null(.PN$script)) stop("Call pn_begin() before pn_register().", call. = FALSE)
  if (!identical(key, pn_key(key))) stop("Key '", key, "' may hold only letters, digits, dots and underscores.", call. = FALSE)
  if (length(value) != 1 || !is.numeric(value) || is.na(value))
    stop("Key '", key, "': value must be one number, not NA.", call. = FALSE)
  v <- value * scale
  txt <- if (is.null(words)) pn_text(v, digits, sign, money) else words
  txt_abs <- if (is.null(words)) pn_text(abs(v), digits, FALSE, money) else NA_character_
  if (key %in% names(.PN$rows)) warning("Key '", key, "' registered twice in ", .PN$script, "; the later value is kept.", call. = FALSE)
  .PN$rows[[key]] <- data.frame(key = key, value = v, digits = digits, text = txt, text_abs = txt_abs,
                                script = .PN$script,
                                build_tag = .PN$build_tag, written = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                note = note, stringsAsFactors = FALSE)
  invisible(txt)
}

# Register every cell of a results table at once. id names the columns that
# identify a row; values maps each value column to its digits.
# Example: pn_register_table(r2, id = c("sample", "window", "pair", "scale"),
#                            values = c(est = 3, se = 3), prefix = "R2")
# gives keys such as R2.all.k_0.E1_O2.proportional.est
pn_register_table <- function(df, id, values, prefix, scale = 1, sign = FALSE, note = "") {
  df <- as.data.frame(df)
  for (r in seq_len(nrow(df))) {
    stem <- paste(c(prefix, vapply(id, function(c) pn_key(as.character(df[[c]][r])), "")), collapse = ".")
    for (col in names(values)) {
      v <- df[[col]][r]
      if (is.na(v)) next
      pn_register(paste(stem, pn_key(col), sep = "."), v, digits = values[[col]], scale = scale,
                  sign = sign, note = if (nzchar(note)) note else paste0("row ", r, " of the ", prefix, " table"))
    }
  }
  invisible(length(.PN$rows))
}

# Register a whole results table as its own part of the registry, written at
# once to <PN_DIR>/<script>__<part>.csv. Identifying columns are the text
# columns plus whole-number columns named like event time (k, horizon, ...);
# the other numeric columns are values. A part is replaced whenever its table
# is written again, so a run that writes some tables refreshes only those.
#   pn_register_csv(tab, prefix = "R14", part = "R14_hours_fixed_fathers",
#                   script = "05_review_response.R", build_tag = bt,
#                   digits = c(est = 3, se = 3))
pn_register_csv <- function(df, prefix, part, script, build_tag = NA_character_,
                            id = NULL, digits = NULL, default_digits = 3L) {
  df <- as.data.frame(df)
  if (!nrow(df)) return(invisible(0L))
  idish <- function(nm, v) !is.numeric(v) ||
    (grepl("^(k|k_lo|k_hi|event_time|horizon|wave|year|window_lo|window_hi|lo_k|hi_k)$", nm) &&
       all(is.na(v) | v == round(v)))
  if (is.null(id)) id <- names(df)[vapply(names(df), function(n) idish(n, df[[n]]), logical(1))]
  vals <- setdiff(names(df)[vapply(df, is.numeric, logical(1))], id)
  dig <- function(col) {
    if (!is.null(digits) && col %in% names(digits)) return(as.integer(digits[[col]]))
    if (grepl("^(n|n_[a-z_]+|count|persons|rows|obs|nobs|n_obs|n_persons|n_ok|reps|b)$", col)) return(0L)
    as.integer(default_digits)
  }
  stem <- if (length(id)) apply(df[, id, drop = FALSE], 1, function(r) paste(c(prefix, pn_idval(r)), collapse = ".")) else
    paste0(prefix, ".r", seq_len(nrow(df)))
  dup <- duplicated(stem) | duplicated(stem, fromLast = TRUE)
  if (any(dup)) {                           # rows the labels do not tell apart
    stem[dup] <- paste0(stem[dup], ".r", seq_len(nrow(df))[dup])
    warning(part, ": ", sum(dup), " row(s) share labels; their keys carry the row number.", call. = FALSE)
  }
  bt <- if (is.null(build_tag)) NA_character_ else as.character(build_tag)
  when <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  out <- list()
  for (col in vals) {
    v <- df[[col]]; ok <- !is.na(v) & is.finite(v)
    if (!any(ok)) next
    d <- dig(col)
    out[[col]] <- data.frame(key = paste(stem[ok], pn_key(col), sep = "."), value = v[ok], digits = d,
                             text = vapply(v[ok], pn_text, "", digits = d),
                             text_abs = vapply(abs(v[ok]), pn_text, "", digits = d),
                             script = script, build_tag = bt, written = when,
                             note = paste0(part, ", row ", which(ok), ", ", col), stringsAsFactors = FALSE)
  }
  reg <- bind_rows(out)
  dir.create(PN_DIR, showWarnings = FALSE, recursive = TRUE)
  f <- file.path(PN_DIR, paste0(sub("\\.[Rr]$", "", script), "__", pn_key(part), ".csv"))
  .pn_write_csv(reg, f, row.names = FALSE)
  cat(sprintf("  paper numbers: %d key(s) from %s registered as %s\n", nrow(reg), part, basename(f)))
  invisible(nrow(reg))
}

# Words that describe a number are checked against it: the phrase must be one
# the table allows for the value, or the script stops.
check_words <- function(value, words, table = list("about a quarter" = c(0.20, 0.30),
                                                   "about a third"   = c(0.28, 0.38),
                                                   "about half"      = c(0.45, 0.55),
                                                   "a half"          = c(0.45, 0.55),
                                                   "just over half"  = c(0.50, 0.60),   # v-just-over-half, 16 October
                                                   "seven-tenths"    = c(0.65, 0.75),
                                                   "three quarters"  = c(0.72, 0.78))) {
  if (!words %in% names(table)) stop("No range defined for '", words, "'.", call. = FALSE)
  rg <- table[[words]]
  if (value < rg[1] || value > rg[2])
    {
      # v-replication, 21 October 2026. On the replication package's synthetic
      # panel no value is the paper's, so a phrase written for the paper
      # cannot describe it. run_all.R sets SYNTHETIC_DATA, and for that run
      # alone the mismatch is a warning. On real data the check stops.
      .cw_msg <- sprintf("'%s' does not describe %.3f (allowed %.2f to %.2f).",
                         words, value, rg[1], rg[2])
      if (isTRUE(get0("SYNTHETIC_DATA", ifnotfound = FALSE))) {
        warning(.cw_msg, " Synthetic data: reported, not enforced.", call. = FALSE)
        return(invisible(words))
      }
      stop(.cw_msg, call. = FALSE)
    }
  invisible(words)
}

# Write this script's registry, replacing its previous one, so a number the
# script no longer registers disappears from the paper's macro file.
pn_flush <- function() {
  if (is.null(.PN$script)) stop("Call pn_begin() first.", call. = FALSE)
  dir.create(PN_DIR, showWarnings = FALSE, recursive = TRUE)
  f <- file.path(PN_DIR, paste0(sub("\\.[Rr]$", "", .PN$script), ".csv"))
  if (!length(.PN$rows)) {                  # nothing registered: leave no stale registry behind
    if (file.exists(f)) file.remove(f)
    cat(sprintf("  paper numbers: %s registered nothing\n", .PN$script)); return(invisible(NULL))
  }
  rows <- bind_rows(.PN$rows)
  .pn_write_csv(rows, f, row.names = FALSE)
  cat(sprintf("  paper numbers: %d key(s) from %s written to %s\n", nrow(rows), .PN$script, f))
  invisible(f)
}

# ---- Derived numbers -------------------------------------------------------------
# Declared in PN_DERIVED (paper_numbers_derived.R), one call each. The formula
# names registered keys, or derived keys declared above it; words may be a
# phrase that check_words() must accept for the value, or a function of the
# value that returns the text, such as pn_int_words.
#   pn_derive("R14.ft_waves_share_of_fall.words",
#             ~ R14.a_all_parents_kge2.E1.post_f.est /
#               R1R4.all.kge2.E1.paid_time_workplace_the_paper_s_measure.est,
#             words = "seven-tenths")
# A derived key whose inputs are not registered is skipped with a message; a
# text that uses it then fails to compile, as for any unregistered key.
pn_derive <- function(key, formula, digits = 3L, scale = 1, sign = FALSE, money = FALSE,
                      words = NULL, note = "") {
  if (!identical(key, pn_key(key)))
    stop("Derived key '", key, "' may hold only letters, digits, dots and underscores.", call. = FALSE)
  if (!inherits(formula, "formula") || length(formula) != 2L)
    stop("Derived key '", key, "': give the formula as ~ expression.", call. = FALSE)
  if (!is.null(.PN$derive[[key]])) stop("Derived key '", key, "' is declared twice.", call. = FALSE)
  .PN$derive[[key]] <- list(key = key, expr = formula[[2]], digits = as.integer(digits), scale = scale,
                            sign = sign, money = money, words = words, note = note)
  invisible(key)
}

# The registry as one table, every part file in PN_DIR.
.pn_registry <- function() {
  fs <- list.files(PN_DIR, pattern = "\\.csv$", full.names = TRUE)
  reg <- bind_rows(lapply(fs, function(f) {
    x <- .pn_read_csv(f, stringsAsFactors = FALSE, colClasses = c(build_tag = "character", text = "character"))
    if (nrow(x)) x$part_file <- basename(f)
    x
  }))
  if (nrow(reg) && !"text_abs" %in% names(reg)) reg$text_abs <- NA_character_
  reg
}

# The registry with the derived numbers appended. attr(, "skipped") names the
# derived keys left out because an input is not registered.
.pn_add_derived <- function(reg, quiet = FALSE) {
  skipped <- character(0)
  if (!is.character(PN_DERIVED) || !nzchar(PN_DERIVED) || !file.exists(PN_DERIVED)) {
    attr(reg, "skipped") <- skipped; return(reg)
  }
  .PN$derive <- list()
  sys.source(PN_DERIVED, envir = new.env(parent = environment(pn_derive)))
  env <- new.env(parent = environment(pn_derive))
  if (nrow(reg)) list2env(stats::setNames(as.list(reg$value), reg$key), envir = env)
  rows <- list()
  for (s in .PN$derive) {
    if (s$key %in% reg$key)
      stop("Derived key '", s$key, "' is also registered by ", reg$script[match(s$key, reg$key)], ".", call. = FALSE)
    need <- all.vars(s$expr)
    miss <- need[!vapply(need, exists, logical(1), envir = env, inherits = FALSE)]
    if (length(miss)) { skipped[s$key] <- paste(miss, collapse = ", "); next }
    v <- tryCatch(eval(s$expr, env),
                  error = function(e) stop("Derived key '", s$key, "': ", conditionMessage(e), call. = FALSE))
    if (length(v) != 1L || !is.numeric(v) || !is.finite(v))
      stop("Derived key '", s$key, "' does not give one finite number.", call. = FALSE)
    v <- v * s$scale
    if (is.null(s$words)) {
      txt <- pn_text(v, s$digits, s$sign, s$money); txt_abs <- pn_text(abs(v), s$digits, FALSE, s$money)
    } else if (is.function(s$words)) {
      txt <- s$words(v); txt_abs <- NA_character_
    } else {
      tryCatch(check_words(v, s$words),
               error = function(e) stop("Derived key '", s$key, "': ", conditionMessage(e), call. = FALSE))
      txt <- s$words; txt_abs <- NA_character_
    }
    src  <- reg[match(intersect(need, reg$key), reg$key), , drop = FALSE]
    tags <- unique(stats::na.omit(src$build_tag[nzchar(src$build_tag)]))
    rows[[length(rows) + 1L]] <- data.frame(
      key = s$key, value = v, digits = s$digits, text = txt, text_abs = txt_abs,
      script = basename(PN_DERIVED), build_tag = if (length(tags)) paste(tags, collapse = ",") else NA_character_,
      written = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      note = trimws(paste0(s$note, if (nzchar(s$note)) ": " else "",
                           paste(deparse(s$expr, width.cutoff = 500L), collapse = " "))),
      part_file = basename(PN_DERIVED), stringsAsFactors = FALSE)
    assign(s$key, v, envir = env)              # later declarations may use it
  }
  if (length(skipped) && !quiet)
    cat(sprintf("  paper numbers: %d derived key(s) skipped, inputs not registered: %s\n", length(skipped),
                paste(sprintf("%s (needs %s)", utils::head(names(skipped), 5), utils::head(skipped, 5)), collapse = "; ")))
  out <- bind_rows(reg, bind_rows(rows))
  attr(out, "skipped") <- skipped
  out
}

# ---- The macro file --------------------------------------------------------------

pn_write_tex <- function(texdir, file = "paper_numbers.tex", allow_stale = FALSE) {
  reg <- .pn_registry()
  if (!nrow(reg)) {
    # Nothing registered yet: write the definitions alone, so the preamble's
    # \input compiles; a \pn{} in the text still stops the compile.
    reg <- data.frame(key = character(), value = numeric(), digits = integer(), text = character(),
                      text_abs = character(), script = character(), build_tag = character(),
                      written = character(), note = character(), stringsAsFactors = FALSE)
    cat("  paper numbers: nothing registered in ", normalizePath(PN_DIR, mustWork = FALSE),
        " yet; writing the definitions only\n", sep = "")
  } else {
    dup <- reg %>% group_by(key) %>% filter(n() > 1) %>% ungroup()
    if (nrow(dup))
      stop("Keys registered more than once:\n",
           paste(sprintf("  %s: %s", unique(dup$key),
                         tapply(dup$part_file, dup$key, paste, collapse = ", ")[unique(dup$key)]), collapse = "\n"),
           call. = FALSE)
    .tagged <- reg[!is.na(reg$build_tag) & nzchar(reg$build_tag), ]
    newest <- if (nrow(.tagged)) .tagged$build_tag[which.max(as.POSIXct(.tagged$written))] else NA_character_
    stale <- reg %>% filter(!is.na(build_tag), !is.na(newest), build_tag != newest) %>% distinct(script, build_tag)
    if (nrow(stale)) {
      msg <- paste0("Registered on an older build than ", newest, ":\n",
                    paste(sprintf("  %s (%s)", stale$script, stale$build_tag), collapse = "\n"),
                    "\nRerun those scripts.")
      if (allow_stale) warning(msg, call. = FALSE) else stop(msg, call. = FALSE)
    }
  }
  tags <- unique(stats::na.omit(reg$build_tag[nzchar(reg$build_tag)]))
  n_reg <- nrow(reg)
  reg <- .pn_add_derived(reg) %>% arrange(key)
  body <- c("\\makeatletter",
           "\\providecommand{\\pnset}[2]{\\expandafter\\gdef\\csname pn@#1\\endcsname{#2}}",
           "\\providecommand{\\pn}[1]{\\ifcsname pn@#1\\endcsname\\csname pn@#1\\endcsname\\else",
           "  \\PackageError{paper_numbers}{No number is registered under `#1'}%",
           "    {Run the script that registers it, then paper_tables.R.}\\fi}",
           "\\makeatother",
           if (nrow(reg)) sprintf("\\pnset{%s}{%s}%% %s", reg$key, reg$text, reg$script) else character(0),
           if (nrow(reg) && any(!is.na(reg$text_abs)))
             with(reg[!is.na(reg$text_abs), ], sprintf("\\pnset{%s.abs}{%s}", key, text_abs)) else character(0))
  # Line 2 is the stamp paper_tables.R's fragment audit reads: a copy in the
  # paper that differs from this build shows there as STALE.
  out <- c(paste0("% Generated by paper_numbers.R from the registries in ", normalizePath(PN_DIR, winslash = "/", mustWork = FALSE),
                  ". Do not edit by hand; rerun the registering script."),
           paste0("% ", format(Sys.time(), "%Y-%m-%d %H:%M"), "  build_tag: ",
                  if (length(tags)) paste(tags, collapse = ",") else "ABSENT",
                  "  content: ", .pn_content_tag(body)),
           paste0("% ", nrow(reg), " key(s) from ", n_distinct(reg$script), " script(s)."),
           body)
  .pn_write_lines(out, file.path(texdir, file))
  prov <- file.path(texdir, sub("\\.tex$", "_provenance.csv", file))
  .pn_write_csv(reg %>% select(key, text, text_abs, value, digits, script, build_tag, written, note), prov, row.names = FALSE)
  cat(sprintf("  wrote %s (%d keys, %d of them derived; content %s, from %s) and %s\n", file.path(texdir, file),
              nrow(reg), nrow(reg) - n_reg, .pn_content_tag(body), normalizePath(PN_DIR, mustWork = FALSE), basename(prov)))
  invisible(reg)
}

# ---- The lint ----------------------------------------------------------------------
# Numbers typed in the text that are results rather than structure: years,
# wave numbers, event times (k = ...), sub- and superscripts, DOIs, age bands
# and numbers inside \pn{}, macros, references and lengths are left out.

pn_lint <- function(tex_file, max_typed = 0L, out = "typed_numbers.csv") {
  x <- .pn_read_lines(tex_file, warn = FALSE, encoding = "UTF-8")
  x <- iconv(gsub("\u2013", "--", gsub("\u2212", "-", x, fixed = TRUE), fixed = TRUE), "UTF-8", "ASCII", sub = " ")
  b0 <- grep("^\\s*\\\\begin\\{document\\}", x)[1]; b1 <- utils::tail(grep("^\\s*\\\\end\\{document\\}", x), 1)
  blank <- function(s, rx) {
    m <- gregexpr(rx, s, perl = TRUE)[[1]]
    if (m[1] < 0) return(s)
    ln <- attr(m, "match.length")
    for (j in seq_along(m)) substr(s, m[j], m[j] + ln[j] - 1L) <- strrep(" ", ln[j])
    s
  }
  REMOVE <- c("\\\\pn\\{[^}]*\\}",
              "\\\\(ref|eqref|pageref|label|input|include|url|cite[a-zA-Z]*)\\*?(\\[[^]]*\\])*\\{[^}]*\\}",
              "\\\\(includegraphics|IfFileExists)(\\[[^]]*\\])*\\{[^}]*\\}",
              "\\\\(vspace|hspace|setlength|rule|multicolumn)\\*?\\{[^}]*\\}(\\{[^}]*\\})?",
              "[0-9.]+\\s*\\\\(textwidth|linewidth|textheight|columnwidth|figw)",
              "[0-9.]+\\s*(pt|em|ex|cm|mm|bp)(?![A-Za-z])", "\\\\\\\\\\[[^]]*\\]",
              "\\\\begin\\{[A-Za-z*]+\\}(\\[[^]]*\\])?(\\{[^}]*\\})*", "\\\\end\\{[A-Za-z*]+\\}",
              "\\\\[A-Za-z]+(\\{\\})?", "--+")
  NUM <- "(?<![A-Za-z0-9_.\\\\])(?:\\\\\\$)?[-+]?(?:[0-9]{1,3}(?:,[0-9]{3})+|[0-9]+)(?:\\.[0-9]+)?(?![0-9])"
  rows <- list(); sec <- ""
  for (i in seq(b0 + 1L, b1 - 1L)) {
    s <- sub("(?<!\\\\)%.*$", "", x[i], perl = TRUE)
    if (grepl("\\\\(sub)?section\\*?\\{", s)) sec <- sub("^.*\\\\(sub)?section\\*?\\{([^}]*)\\}.*$", "\\2", s)
    if (!nzchar(trimws(s)) || grepl("\\\\(affiliation|ead|cormark|credit|orcid|author|tnotetext)\\b", s, perl = TRUE)) next
    t <- s
    for (rx in REMOVE) t <- blank(t, rx)
    m <- gregexpr(NUM, t, perl = TRUE)[[1]]
    if (m[1] < 0) next
    for (j in seq_along(m)) {
      st <- m[j]; en <- st + attr(m, "match.length")[j] - 1L
      lit <- substring(t, st, en); before <- substring(t, max(1, st - 14), st - 1); after <- substring(t, en + 1, en + 12)
      a <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", lit)))
      structural <- grepl("[\\^_]\\{?\\s*$", before, perl = TRUE) || grepl("^\\^|^/", after, perl = TRUE) ||
        grepl("(?i)\\bwaves?~(\\s*|[0-9]+\\s+(and\\s+|to\\s+)?)$", before, perl = TRUE) ||
        grepl("k\\s*(=|\\\\geq?|\\\\leq?|<|>)\\s*[-+]?\\s*$", before, perl = TRUE) ||
        grepl("(?i)(doi|release)[~ :]*$", before, perl = TRUE) ||
        (!is.na(a) && !grepl("[.,]", lit) && a >= 1950 && a <= 2035)
      if (!structural) rows[[length(rows) + 1L]] <- data.frame(line = i, section = sec, literal = gsub("\\\\", "", lit),
                                                           text = substr(trimws(s), 1, 160), stringsAsFactors = FALSE)
    }
  }
  typed <- bind_rows(rows)
  if (!nrow(typed)) typed <- data.frame(line = integer(), section = character(), literal = character(), text = character())
  .pn_write_csv(typed, out, row.names = FALSE)
  by_sec <- if (nrow(typed)) sort(table(typed$section), decreasing = TRUE) else integer(0)
  cat(sprintf("pn_lint: %d number(s) typed in the text (allowed: %d). Listed in %s.\n", nrow(typed), max_typed, out))
  if (length(by_sec)) for (k in utils::head(names(by_sec), 10)) cat(sprintf("  %4d  %s\n", by_sec[[k]], k))
  if (nrow(typed) > max_typed)
    stop(sprintf("%d typed number(s) exceed the allowed %d. Register them and use \\pn{} in the text.",
                 nrow(typed), max_typed), call. = FALSE)
  invisible(typed)
}

# ---- Diagnosis -------------------------------------------------------------------
# Says why a \pn{} key does not print: no \input in the preamble, no macro file
# beside the manuscript, a macro file older than the registry, a copy that never
# reached the paper folder, or a key no script registers.
#   pn_check("els-cas-templates/Motherhood_training_v5.tex")

pn_check <- function(tex_file, texdir = file.path("output", "tables")) {
  x <- sub("(?<!\\\\)%.*$", "", .pn_read_lines(tex_file, warn = FALSE), perl = TRUE)
  used <- unique(unlist(regmatches(x, gregexpr("(?<=\\\\pn\\{)[^}]+(?=\\})", x, perl = TRUE))))
  inp <- regmatches(x, regexpr("\\\\input\\{[^}]*paper_numbers(\\.tex)?\\}", x))
  fs <- list.files(PN_DIR, pattern = "\\.csv$", full.names = TRUE)
  reg0 <- .pn_registry()
  reg <- .pn_add_derived(reg0, quiet = TRUE)
  skipped <- attr(reg, "skipped")
  keys_in <- function(f) if (file.exists(f))
    sub("^\\\\pnset\\{([^}]+)\\}.*$", "\\1", grep("^\\\\pnset\\{", .pn_read_lines(f, warn = FALSE), value = TRUE)) else character(0)
  paper_file <- if (length(inp)) {
    rel <- sub("^\\\\input\\{([^}]*)\\}$", "\\1", inp[1])
    file.path(dirname(tex_file), if (grepl("\\.tex$", rel)) rel else paste0(rel, ".tex"))
  } else file.path(dirname(tex_file), "tables", "paper_numbers.tex")
  build_file <- file.path(texdir, "paper_numbers.tex")
  in_paper <- keys_in(paper_file); in_build <- keys_in(build_file)
  src_files <- c(fs, if (file.exists(PN_DERIVED)) PN_DERIVED)
  newest_reg <- if (length(src_files)) max(file.info(src_files)$mtime) else NA
  # What each copy prints, and its content stamp, so a copy with the right
  # keys and old values is caught as well as one with missing keys.
  vals_in <- function(f) {
    if (!file.exists(f)) return(character(0))
    l <- grep("^\\\\pnset\\{", .pn_read_lines(f, warn = FALSE), value = TRUE)
    m <- regmatches(l, regexec("^\\\\pnset\\{([^}]+)\\}\\{((?:[^{}]|\\{(?:[^{}]|\\{[^{}]*\\})*\\})*)\\}", l, perl = TRUE))
    m <- m[lengths(m) == 3]
    stats::setNames(vapply(m, `[`, "", 3), vapply(m, `[`, "", 2))
  }
  stamp_of <- function(f) {
    if (!file.exists(f)) return(NA_character_)
    h <- .pn_read_lines(f, n = 2, warn = FALSE)
    m <- regmatches(h, regexpr("content: [0-9a-f]+", h))
    if (length(m)) sub("content: ", "", m[1]) else NA_character_
  }
  plain <- function(t) gsub("\\\\\\$", "$", gsub("{,}", ",", gsub("^\\\\ensuremath\\{|\\}$", "", t), fixed = TRUE))

  cat(sprintf("pn_check: %d key(s) used in %s\n", length(used), basename(tex_file)))
  cat(sprintf("  registry  %-48s %d key(s) in %d file(s), %d derived%s\n", PN_DIR, nrow(reg0), length(fs),
              nrow(reg) - nrow(reg0),
              if (length(src_files)) paste0(", newest ", format(newest_reg, "%Y-%m-%d %H:%M")) else ""))
  n_keys <- function(k) sum(!grepl("\\.abs$", k))
  cat(sprintf("  written   %-48s %s\n", build_file,
              if (file.exists(build_file)) sprintf("%d key(s), %s", n_keys(in_build),
                                                   format(file.info(build_file)$mtime, "%Y-%m-%d %H:%M")) else "missing"))
  cat(sprintf("  read      %-48s %s\n", paper_file,
              if (file.exists(paper_file)) sprintf("%d key(s), %s", n_keys(in_paper),
                                                   format(file.info(paper_file)$mtime, "%Y-%m-%d %H:%M")) else "missing"))
  problems <- character(0)
  if (!length(inp))
    problems <- c(problems, "The preamble has no \\input{tables/paper_numbers}; add it after \\input{tables/data_section_numbers}.")
  if (!file.exists(build_file) && !file.exists(paper_file))
    problems <- c(problems, "No paper_numbers.tex anywhere: run paper_tables.R, which calls pn_write_tex().")
  if (file.exists(build_file) && !is.na(newest_reg) && file.info(build_file)$mtime < newest_reg)
    problems <- c(problems, "paper_numbers.tex is older than the registry or paper_numbers_derived.R: run paper_tables.R (or pn_write_tex()) again.")
  missing <- setdiff(used, in_paper)
  if (length(missing)) {
    base_of    <- function(k) sub("\\.abs$", "", k)
    is_reg     <- base_of(missing) %in% reg$key
    copied_not <- intersect(missing, in_build)
    unwritten  <- setdiff(missing[is_reg], in_build)
    waiting    <- missing[!is_reg & base_of(missing) %in% names(skipped)]
    unknown    <- setdiff(missing[!is_reg], waiting)
    if (length(waiting))
      problems <- c(problems, sprintf("%d derived key(s) wait for inputs no script has registered yet: %s",
                                      length(waiting), paste(sprintf("%s (needs %s)", utils::head(waiting, 3),
                                                                     skipped[base_of(utils::head(waiting, 3))]), collapse = "; ")))
    if (length(copied_not))
      problems <- c(problems, sprintf("%d key(s) are in %s but not in the paper's copy: pn_write_tex() must run before paper_tables' sync, or copy the file. (%s)",
                                      length(copied_not), build_file, paste(utils::head(copied_not, 5), collapse = ", ")))
    if (length(unwritten))
      problems <- c(problems, sprintf("%d key(s) are registered but not yet written: run paper_tables.R. (%s)",
                                      length(unwritten), paste(utils::head(unwritten, 5), collapse = ", ")))
    if (length(unknown))
      problems <- c(problems, sprintf("%d key(s) are used in the text but no script registers them: check the spelling, or that the script ran to its pn_flush(). (%s)",
                                      length(unknown), paste(utils::head(unknown, 5), collapse = ", ")))
  }
  # The values the paper prints, against the values the registry holds.
  copy_flagged <- any(grepl("not in the paper's copy", problems, fixed = TRUE))
  if (nrow(reg) && file.exists(paper_file)) {
    pv <- vals_in(paper_file)
    rv <- stats::setNames(reg$text, reg$key)
    if ("text_abs" %in% names(reg)) {
      ab <- !is.na(reg$text_abs) & nzchar(reg$text_abs)
      rv <- c(rv, stats::setNames(as.character(reg$text_abs[ab]), paste0(reg$key[ab], ".abs")))
    }
    both <- intersect(used, intersect(names(pv), names(rv)))
    differ <- both[pv[both] != rv[both]]
    if (length(differ)) {
      ex <- utils::head(differ, 3)
      problems <- c(problems, sprintf(paste0("%d key(s) print a different value in the paper's copy than the registry holds, ",
                                             "so the paper is reading an older build: rerun paper_tables.R with pn_write_tex() ",
                                             "above its sync. (%s)"), length(differ),
                                      paste(sprintf("%s: paper %s, registry %s", ex, plain(pv[ex]), plain(rv[ex])), collapse = "; ")))
      copy_flagged <- TRUE
    }
  }
  # A sign typed in the text before a key that prints its own minus, as in
  # $+\pn{k}$ once k turns negative, would print +-0.005.
  if (nrow(reg)) {
    rv_all <- stats::setNames(reg$text, reg$key)
    ab <- !is.na(reg$text_abs) & nzchar(reg$text_abs)
    rv_all <- c(rv_all, stats::setNames(as.character(reg$text_abs[ab]), paste0(reg$key[ab], ".abs")))
    sx <- unlist(regmatches(x, gregexpr("(?<!-)[+-]\\s*\\\\pn\\{[^}]+\\}", x, perl = TRUE)))
    sk <- unique(sub("^[+-]\\s*\\\\pn\\{([^}]+)\\}$", "\\1", sx))
    clash <- sk[sk %in% names(rv_all) & startsWith(plain(rv_all[sk]), "-")]
    if (length(clash))
      problems <- c(problems, sprintf("%d key(s) now print a minus sign after a sign typed in the text, which shows as +- or --: remove the typed sign or rephrase. (%s)",
                                      length(clash), paste(utils::head(clash, 5), collapse = ", ")))
  }
  sb <- stamp_of(build_file); sp <- stamp_of(paper_file)
  if (!copy_flagged && !is.na(sb) && (is.na(sp) || sb != sp))
    problems <- c(problems, paste0("The paper's copy of paper_numbers.tex is not the one paper_tables.R last wrote ",
                                   "(content ", if (is.na(sp)) "unstamped" else sp, " against ", sb, "). ",
                                   "Move pn_write_tex() above paper_tables' sync and run it again."))
  if (length(problems)) { cat("\nProblems:\n"); cat(paste0("  - ", problems, collapse = "\n"), "\n") } else
    cat(sprintf("\nAll %d key(s) the text uses are defined in the file it reads, with the values the registry holds.\n",
                length(used)))
  invisible(list(used = used, missing = missing, problems = problems))
}

# ---- Moving typed numbers to keys -------------------------------------------------
# Typed numbers with their positions on each line, by the lint's rules.
.pn_typed <- function(tex_file) {
  x <- .pn_read_lines(tex_file, warn = FALSE, encoding = "UTF-8")
  y <- gsub("\u2212", "-", gsub("\u2013|\u2014", " ", x))      # same length, so positions hold
  b0 <- grep("^\\s*\\\\begin\\{document\\}", y)[1]; b1 <- utils::tail(grep("^\\s*\\\\end\\{document\\}", y), 1)
  blank <- function(s, rx) {
    m <- gregexpr(rx, s, perl = TRUE)[[1]]
    if (m[1] < 0) return(s)
    ln <- attr(m, "match.length")
    for (j in seq_along(m)) substr(s, m[j], m[j] + ln[j] - 1L) <- strrep(" ", ln[j])
    s
  }
  REMOVE <- c("\\\\pn\\{[^}]*\\}",
              "\\\\(ref|eqref|pageref|label|input|include|url|cite[a-zA-Z]*)\\*?(\\[[^]]*\\])*\\{[^}]*\\}",
              "\\\\(includegraphics|IfFileExists)(\\[[^]]*\\])*\\{[^}]*\\}",
              "\\\\(vspace|hspace|setlength|rule|multicolumn)\\*?\\{[^}]*\\}(\\{[^}]*\\})?",
              "[0-9.]+\\s*\\\\(textwidth|linewidth|textheight|columnwidth|figw)",
              "[0-9.]+\\s*(pt|em|ex|cm|mm|bp)(?![A-Za-z])", "\\\\\\\\\\[[^]]*\\]",
              "\\\\begin\\{[A-Za-z*]+\\}(\\[[^]]*\\])?(\\{[^}]*\\})*", "\\\\end\\{[A-Za-z*]+\\}",
              "\\\\(?!\\$)[A-Za-z]+(\\{\\})?", "--+")
  NUM <- "(?<![A-Za-z0-9_.\\\\])(?:\\\\\\$)?[-+]?(?:[0-9]{1,3}(?:,[0-9]{3})+|[0-9]+)(?:\\.[0-9]+)?(?![0-9])"
  rows <- list(); sec <- ""
  for (i in seq(b0 + 1L, b1 - 1L)) {
    s <- sub("(?<!\\\\)%.*$", "", y[i], perl = TRUE)
    if (grepl("\\\\(sub)?section\\*?\\{", s)) sec <- sub("^.*\\\\(sub)?section\\*?\\{([^}]*)\\}.*$", "\\2", s)
    if (!nzchar(trimws(s)) || grepl("\\\\(affiliation|ead|cormark|credit|orcid|author|tnotetext)\\b", s, perl = TRUE)) next
    t <- s
    for (rx in REMOVE) t <- blank(t, rx)
    m <- gregexpr(NUM, t, perl = TRUE)[[1]]
    if (m[1] < 0) next
    for (j in seq_along(m)) {
      st <- m[j]; en <- st + attr(m, "match.length")[j] - 1L
      lit <- substring(t, st, en); before <- substring(t, max(1, st - 14), st - 1); after <- substring(t, en + 1, en + 12)
      a <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", lit)))
      structural <- grepl("[\\^_]\\{?\\s*$", before, perl = TRUE) || grepl("^\\^|^/", after, perl = TRUE) ||
        grepl("(?i)\\bwaves?~(\\s*|[0-9]+\\s+(and\\s+|to\\s+)?)$", before, perl = TRUE) ||
        grepl("k\\s*(=|\\\\geq?|\\\\leq?|<|>)\\s*[-+]?\\s*$", before, perl = TRUE) ||
        grepl("(?i)(doi|release)[~ :]*$", before, perl = TRUE) ||
        (!is.na(a) && !grepl("[.,]", lit) && a >= 1950 && a <= 2035)
      if (!structural)
        rows[[length(rows) + 1L]] <- data.frame(line = i, section = sec, start = st, end = en, literal = lit,
                                                cue = substring(s, max(1, st - 16), st - 1), stringsAsFactors = FALSE)
    }
  }
  list(lines = x, typed = bind_rows(rows))
}

# Moving a typed number to a key is done from an explicit map, never from
# values alone: with thousands of registered cells, two keys often print the
# same text, so a match by value is a guess. The map names, for each number,
# a sentence anchor (a short phrase from its line), the number as typed, and
# the key it comes from. pn_apply() replaces a number only when that key
# prints exactly what is typed, and lists every case where it does not, so a
# wrong key or a rounding edge is caught rather than written in.
#   pn_apply("els-cas-templates/Motherhood_training_v5.tex", "pn_map_05_R2.csv")
#   pn_apply(tex, c("pn_map_05_batch2.csv", "pn_map_05_batch2_changes.csv"))
# Columns of the map: anchor, literal, key, and optionally occurrence (which
# appearance of the number on that line, 1 by default) and accept. A literal
# without digits is a number in words, such as seven-tenths.
# A key that prints something else is kept typed, except:
#   accept_near = TRUE   replaces it when the two differ by one unit in the
#                        last typed digit and share a sign (0.094 against
#                        0.093), a rounding difference;
#   accept = yes         in the map replaces it whatever it prints, for a
#                        typed number you have checked is out of date.
pn_apply <- function(tex_file, map, out_tex = sub("\\.tex$", "_pn.tex", tex_file), accept_near = FALSE,
                     report = "pn_apply_report.csv") {
  if (is.character(map))                       # one map file or several, read as text
    map <- bind_rows(lapply(map, function(f) .pn_read_csv(f, stringsAsFactors = FALSE, colClasses = "character")))
  if (!"occurrence" %in% names(map)) map$occurrence <- "1"
  map$occurrence[is.na(map$occurrence) | !nzchar(map$occurrence)] <- "1"
  if (!"accept" %in% names(map)) map$accept <- ""
  map$accept[is.na(map$accept)] <- ""
  reg <- .pn_add_derived(.pn_registry(), quiet = TRUE)
  skipped <- attr(reg, "skipped")
  plain <- function(t) gsub("\\\\\\$", "$", gsub("{,}", ",", gsub("^\\\\ensuremath\\{|\\}$", "", t), fixed = TRUE))
  prints <- function(k) {
    base <- sub("\\.abs$", "", k); i <- match(base, reg$key)
    if (is.na(i)) return(NA_character_)
    plain(if (grepl("\\.abs$", k)) reg$text_abs[i] else reg$text[i])
  }
  num_of <- function(s) suppressWarnings(as.numeric(gsub("[$,+]", "", s)))
  within_unit <- function(pr, typed) {
    a <- num_of(pr); b <- num_of(typed)
    if (is.na(a) || is.na(b)) return(FALSE)
    d <- if (grepl("\\.", typed)) nchar(sub("^[^.]*\\.", "", gsub("[^0-9.]", "", typed))) else 0L
    sign(a) * sign(b) >= 0 && abs(a - b) <= 10^(-d) + 1e-9
  }
  tp <- .pn_typed(tex_file); x <- tp$lines; typed <- tp$typed
  map$status <- ""; map$line <- NA_integer_; map$prints <- ""
  todo <- list()
  for (r in seq_len(nrow(map))) {
    ln <- grep(map$anchor[r], x, fixed = TRUE)
    if (length(ln) != 1) { map$status[r] <- sprintf("anchor found on %d lines", length(ln)); next }
    lit <- map$literal[r]
    occ <- as.integer(map$occurrence[r])
    words <- !grepl("[0-9]", lit)
    if (!words) {
      hits <- typed[typed$line == ln & gsub("\\\\", "", typed$literal) == gsub("\\\\", "", lit), c("start", "end")]
      typed_plain <- sub("^\\+", "", gsub("\\\\", "", lit))
      keep_plus <- startsWith(gsub("\\\\\\$", "", lit), "+")
    } else {                                    # a number in words
      m <- gregexpr(paste0("(?<![A-Za-z-])\\Q", lit, "\\E(?![A-Za-z-])"), x[ln], perl = TRUE)[[1]]
      hits <- if (m[1] > 0) data.frame(start = as.integer(m), end = as.integer(m) + attr(m, "match.length") - 1L) else
        data.frame(start = integer(), end = integer())
      typed_plain <- lit; keep_plus <- FALSE
    }
    if (nrow(hits) < occ) { map$status[r] <- "number not on that line"; next }
    h <- hits[occ, ]; map$line[r] <- ln
    pr <- prints(map$key[r]); map$prints[r] <- ifelse(is.na(pr), "", pr)
    if (is.na(pr)) {
      map$status[r] <- if (map$key[r] %in% names(skipped) || sub("\\.abs$", "", map$key[r]) %in% names(skipped))
        "derived key: its inputs are not registered yet" else "key not registered"
      next
    }
    go <- TRUE
    if (identical(pr, typed_plain)) map$status[r] <- "replaced" else
      if (tolower(map$accept[r]) %in% c("yes", "true", "1")) map$status[r] <- "replaced; accepted change" else
        if (accept_near && !words && within_unit(pr, typed_plain)) map$status[r] <- "replaced; rounding difference" else {
          map$status[r] <- "kept: key prints differently"; go <- FALSE
        }
    if (go) todo[[length(todo) + 1L]] <- data.frame(line = ln, start = h$start + keep_plus, end = h$end,
                                                    key = map$key[r], stringsAsFactors = FALSE)
  }
  go <- bind_rows(todo)
  if (nrow(go)) {
    go <- go[order(go$line, -go$start), ]
    for (r in seq_len(nrow(go)))
      x[go$line[r]] <- paste0(substr(x[go$line[r]], 1, go$start[r] - 1), "\\pn{", go$key[r], "}",
                              substr(x[go$line[r]], go$end[r] + 1, nchar(x[go$line[r]])))
  }
  .pn_write_lines(x, out_tex, useBytes = TRUE)
  .pn_write_csv(map, report, row.names = FALSE)
  tab <- table(map$status)
  cat(sprintf("pn_apply: %d mapped number(s): %s.\n  wrote %s; report in %s. The original is unchanged.\n",
              nrow(map), paste(sprintf("%d %s", tab, names(tab)), collapse = ", "), out_tex, report))
  bad <- map[map$status != "replaced", ]
  for (r in seq_len(nrow(bad)))
    cat(sprintf("  %-9s %-44s %s%s\n", bad$literal[r], bad$key[r], bad$status[r],
                if (nzchar(bad$prints[r])) paste0(" (registry: ", bad$prints[r], ")") else ""))
  invisible(map)
}

# Look keys up by part of their name, to write a map; nothing is changed.
#   pn_candidates("R2.ftret.kge2.E1_O2")
pn_candidates <- function(pattern) {
  reg <- .pn_add_derived(.pn_registry(), quiet = TRUE)
  hit <- reg[grepl(pattern, reg$key, fixed = TRUE), c("key", "text", "note")]
  hit$text <- gsub("^\\\\ensuremath\\{|\\}$", "", hit$text)
  hit
}
