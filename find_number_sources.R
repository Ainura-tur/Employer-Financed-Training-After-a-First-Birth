# =============================================================================
# find_number_sources.R
#
# Maps every number the manuscript reports to the output file, log line or
# script that holds it, and to the script that writes that file. Three stages:
#
#   1. STILL RED FIRST. Every number inside \pending{} is searched before
#      anything else, together with the other numbers in its sentence. The R
#      scripts are also searched for the sentence's key words, to point at the
#      code that should compute the number. With RECOMPUTE_STILL_RED = TRUE,
#      the topics it knows how to define are recomputed from td_frame(), and
#      the candidate nearest the red value shows which definition the paper
#      used, so that definition can be moved into the pipeline.
#   2. THE REFERENCE MAP. One row for every number in the text, every macro
#      the text uses and every number in the table fragments it inputs, each
#      with its best source and the script that writes that source. Only the
#      newest log of each script is read here (MAP_LATEST_LOGS_ONLY), since
#      older logs hold earlier builds; stage 1 reads them all, because an old
#      log is where a stale red value was printed.
#   3. BY SCRIPT. How many numbers each script supplies, and from which files.
#
# Writers. A file's writer is taken from the logs first: the newest log that
# says it "wrote" the file, or lists it under "written to <folder>:". Only
# when no log names it is the script text searched, and the map then says
# so. A log belongs to the script in its name; RUN_ALL's console copies
# belong to the stage script RUN_ALL lists, and rank below the stage's own log.
#
# Matching. A number matches every way it can appear once rounded: 44.2
# matches 44.2 and 44.15 to 44.25 with more digits, and a percentage is also
# looked for as a fraction (0.442). A count matches exactly, with or without
# its comma, and a dollar figure in hundreds is also looked for in thousands
# (13,300 as 13.3). Evidence is graded. A line that also holds another number
# from the same sentence is strong. A distinctive number alone (three or more
# significant digits) is moderate. A short number alone is weak, because it
# turns up everywhere. Echoed code in logs (lines starting with > or +) is
# skipped, and so are files that quote the paper rather than compute it.
# Binary files (.rds, .pdf) are not read, so a number held only in an .rds
# shows as not found.
#
# Run from the project root, the folder that holds run_logs/ and output/:
#   source("find_number_sources.R")
# Settings can be set before sourcing, e.g. MANUSCRIPT <- "path/to/file.tex".
#
# Writes, in the project root:
#   still_red_sources.csv           stage 1: candidate sources per red number
#   still_red_recomputed.csv        stage 1: recomputed candidates, if run
#   number_reference_map.csv        stage 2: one row per reported number
#   number_reference_by_script.csv  stage 3: numbers and files per script
# Everything else is read, not changed.
# =============================================================================

suppressPackageStartupMessages(library(dplyr))

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

# ---- Settings ----------------------------------------------------------------
# Any of these can be set before sourcing.

if (!exists("SEARCH_DIRS"))         SEARCH_DIRS         <- c("run_logs", "output")
if (!exists("SCRIPT_DIR"))          SCRIPT_DIR          <- "."
if (!exists("MANUSCRIPT"))          MANUSCRIPT          <- NA_character_  # newest Motherhood_training_v*.tex if NA
if (!exists("MACRO_FILE"))          MACRO_FILE          <- NA_character_  # tables/data_section_numbers.tex if NA
if (!exists("MAX_MB"))              MAX_MB              <- 25     # larger files are skipped
if (!exists("MAX_CANDIDATES"))      MAX_CANDIDATES      <- 3      # sources kept per number
if (!exists("FRAGMENT_CELLS"))      FRAGMENT_CELLS      <- TRUE   # also map numbers inside table fragments
if (!exists("RECOMPUTE_STILL_RED")) RECOMPUTE_STILL_RED <- TRUE   # needs 00_contract.R and the frame
if (!exists("EXTRA_GROUPS"))        EXTRA_GROUPS        <- list() # ad hoc groups, e.g. list("ages" = c("49.4", "30.7"))
if (!exists("MAP_LATEST_LOGS_ONLY")) MAP_LATEST_LOGS_ONLY <- TRUE # stage 2 reads only the newest log per script

OUT_STILL_RED  <- "still_red_sources.csv"
OUT_RECOMPUTED <- "still_red_recomputed.csv"
OUT_MAP        <- "number_reference_map.csv"
OUT_BY_SCRIPT  <- "number_reference_by_script.csv"

# Files that QUOTE the paper rather than compute anything. The verifier's
# report prints each sentence, so every number turns up there; these are
# echoes, not sources, and are left out, as are this script's own results.
echoes <- paste("prose_numbers_check", "manuscript_reconciliation", "verify_report",
                "verify_manuscript_", "verify_console", "vm_settings", "number_sources_search",
                "number_reference_", "still_red_", "results_review", "number_worklist",
                "script_io_calls", "output_files_check", "Motherhood_training", sep = "|")

# Shared helpers name every file they write on another script's behalf, so
# when no log settles a writer they are ranked last.
if (!exists("HELPER_SCRIPTS")) HELPER_SCRIPTS <- c("00_contract.R", "00compat.R", "03_common.R",
                                                   "03_logging.R", "paths.R", "vm_settings.R")

# ---- Paths ---------------------------------------------------------------------

if (is.na(MANUSCRIPT)) {
  .c <- unique(c(Sys.glob(file.path("els-cas-templates", "Motherhood_training_v*.tex")),
                 Sys.glob("Motherhood_training_v*.tex"),
                 Sys.glob(file.path("*", "Motherhood_training_v*.tex"))))
  if (!length(.c))
    stop("No manuscript found from ", getwd(),
         ". Set MANUSCRIPT <- \"path/to/file.tex\" and source again.", call. = FALSE)
  MANUSCRIPT <- .c[which.max(file.info(.c)$mtime)]
}
TEX_DIR <- dirname(MANUSCRIPT)
if (is.na(MACRO_FILE)) {
  .c <- c(file.path(TEX_DIR, "tables", "data_section_numbers.tex"),
          file.path("output", "tables", "data_section_numbers.tex"))
  MACRO_FILE <- if (any(file.exists(.c))) .c[file.exists(.c)][1] else NA_character_
}
cat("find_number_sources.R  ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n",
    "  manuscript: ", MANUSCRIPT, "\n",
    "  macros:     ", if (is.na(MACRO_FILE)) "none found" else MACRO_FILE, "\n", sep = "")

# ---- Text helpers ----------------------------------------------------------------

# Plain ASCII, so character positions and byte positions agree.
to_ascii <- function(x) {
  x <- enc2utf8(x)
  for (p in list(c("\u2013", "--"), c("\u2014", "---"), c("\u2212", "-"), c("\u00a0", " "),
                 c("\u2018", "'"), c("\u2019", "'"), c("\u201c", "\""), c("\u201d", "\"")))
    x <- gsub(p[1], p[2], x, fixed = TRUE)
  x <- iconv(x, "UTF-8", "ASCII", sub = " ")
  x[is.na(x)] <- ""
  x
}

# Index of the brace closing the one at position i (ch: one character each).
close_brace <- function(ch, i) {
  d <- 0L
  for (j in i:length(ch)) {
    if (ch[j] == "{" && (j == 1L || ch[j - 1L] != "\\")) d <- d + 1L
    if (ch[j] == "}" && (j == 1L || ch[j - 1L] != "\\")) {
      d <- d - 1L
      if (d == 0L) return(j)
    }
  }
  length(ch)
}

# Spans of the {argument} after each \cmd on a line, as a two-column matrix.
arg_spans <- function(s, cmd) {
  m <- gregexpr(paste0("\\\\", cmd, "\\{"), s, perl = TRUE)[[1]]
  if (m[1] < 0) return(matrix(integer(0), ncol = 2))
  ch <- strsplit(s, "", fixed = TRUE)[[1]]
  do.call(rbind, lapply(seq_along(m), function(j) {
    o <- m[j] + attr(m, "match.length")[j] - 1L
    c(o, close_brace(ch, o))
  }))
}
in_spans <- function(pos, sp)
  if (!nrow(sp)) rep(FALSE, length(pos)) else
    vapply(pos, function(p) any(p > sp[, 1] & p < sp[, 2]), logical(1))

# Overwrite every match with spaces, so positions on the line do not move.
blank <- function(s, rx) {
  m <- gregexpr(rx, s, perl = TRUE)[[1]]
  if (m[1] < 0) return(s)
  ln <- attr(m, "match.length")
  for (j in seq_along(m)) substr(s, m[j], m[j] + ln[j] - 1L) <- strrep(" ", ln[j])
  s
}

sig_digits <- function(lit) nchar(sub("^0+", "", gsub("[^0-9]", "", lit)))

# ---- Macros --------------------------------------------------------------------

read_macros <- function(f) {
  if (is.na(f) || !file.exists(f)) return(character(0))
  s <- to_ascii(paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  s <- gsub("(?<!\\\\)%[^\n]*", "", s, perl = TRUE)
  ch <- strsplit(s, "", fixed = TRUE)[[1]]
  m <- gregexpr("\\\\(?:re|provide)?newcommand\\*?\\{?\\\\[A-Za-z]+\\}?(?:\\[[0-9]\\])?\\s*\\{",
                s, perl = TRUE)[[1]]
  if (m[1] < 0) return(character(0))
  st <- as.integer(m); ln <- attr(m, "match.length")
  hit <- substring(s, st, st + ln - 1L)
  nm <- sub("^\\\\(?:re|provide)?newcommand\\*?\\{?\\\\([A-Za-z]+).*$", "\\1", hit, perl = TRUE)
  val <- vapply(seq_along(st), function(j) {
    o <- st[j] + ln[j] - 1L; e <- close_brace(ch, o)
    if (e - 1L >= o + 1L) paste(ch[(o + 1L):(e - 1L)], collapse = "") else ""
  }, character(1))
  setNames(val, nm)
}
macros <- read_macros(MACRO_FILE)
cat(sprintf("  %d macro(s) defined\n", length(macros)))

# ---- Parse the manuscript ----------------------------------------------------------
# One row per number. The paper is written one sentence per line, so a line
# is the unit within which numbers are searched together.

NUM_LIT <- "(?<![A-Za-z0-9_.\\\\])(?:\\\\\\$)?[-+]?(?:\\\\\\$)?(?:[0-9]{1,3}(?:,[0-9]{3})+|[0-9]+)(?:\\.[0-9]+)?(?![0-9])"
REMOVE  <- c(
  "\\\\(ref|eqref|pageref|autoref|label|input|include|url|cite[a-zA-Z]*)\\*?(\\[[^]]*\\])*\\{[^}]*\\}",
  "\\\\(includegraphics|IfFileExists)(\\[[^]]*\\])*\\{[^}]*\\}",
  "\\\\(vspace|hspace|setlength|addtolength|rule|resizebox|scalebox|multicolumn)\\*?\\{[^}]*\\}(\\{[^}]*\\})?",
  "\\\\(cmidrule|cline)(\\([^)]*\\))?\\{[0-9]+-[0-9]+\\}",
  "[0-9.]+\\s*\\\\(textwidth|linewidth|textheight|columnwidth|baselineskip|figw)",
  "[0-9.]+\\s*(pt|em|ex|cm|mm|bp)(?![A-Za-z])",
  "\\\\\\\\\\[[^]]*\\]",
  "\\\\begin\\{[A-Za-z*]+\\}(\\[[^]]*\\])?(\\{[^}]*\\})*",
  "\\\\end\\{[A-Za-z*]+\\}",
  "--+")                                   # a range, so 12--24 does not read as -24

classify <- function(lit, a, dec, before, after, sentence) {
  if (grepl("[\\^_]\\{?\\s*$", before, perl = TRUE) || grepl("^\\^", after, perl = TRUE))                    return("structural")
  if (grepl("(?i)\\bwaves?~\\s*$|(?i)\\bwaves?~[0-9]+\\s+(and\\s+|to\\s+)?$", before, perl = TRUE)) return("structural")
  if (grepl("k\\s*(=|\\\\geq?|\\\\leq?|<|>)\\s*[-+]?\\s*$", before, perl = TRUE))                return("structural")
  if (grepl("^/", after) || grepl("(?i)(doi|release)[~ :]*$", before, perl = TRUE))           return("structural")
  if (dec == 0 && a >= 1950 && a <= 2035 && !grepl(",", lit))                          return("structural")
  if (dec == 0 && a >= 15 && a <= 70 && grepl("(?i)\\bage", sentence, perl = TRUE) &&
      (grepl("(to|\\s)\\s*$", before, perl = TRUE) && grepl("^\\s*(to\\s+)?[0-9]", after, perl = TRUE) ||
       grepl("[0-9]\\s+(to\\s+)?$", before, perl = TRUE)))                                            return("design")
  if (grepl("\\$", lit, fixed = TRUE))                                                   return("money")
  if (grepl("^\\s*(\\\\%|percent|per cent|percentage point|points?\\b)", after, perl = TRUE))       return("percent")
  if (dec == 0 && a >= 100)                                                              return("count")
  if (dec >= 1)                                                                          return("estimate")
  "integer"
}

parse_tex <- function(lines, origin, part0) {
  lines <- to_ascii(lines)
  sec <- ""; subsec <- ""; lab <- ""; part <- part0; in_float <- FALSE
  rows <- list(); uses <- list(); inputs <- character(0)
  for (i in seq_along(lines)) {
    s <- sub("(?<!\\\\)%.*$", "", lines[i], perl = TRUE)
    if (!nzchar(trimws(s))) next
    if (grepl("^\\s*\\\\appendix\\b", s)) part <- "appendix"
    if (grepl("\\\\section\\*?\\{", s)) { sec <- sub("^.*\\\\section\\*?\\{([^}]*)\\}.*$", "\\1", s); subsec <- "" }
    if (grepl("\\\\subsection\\*?\\{", s)) subsec <- sub("^.*\\\\subsection\\*?\\{([^}]*)\\}.*$", "\\1", s)
    if (grepl("\\\\label\\{sec:", s)) lab <- sub("^.*\\\\label\\{(sec:[^}]*)\\}.*$", "\\1", s)
    if (grepl("\\\\begin\\{(figure|table)", s)) in_float <- TRUE
    inp <- regmatches(s, gregexpr("\\\\input\\{[^}]+\\}", s))[[1]]
    if (length(inp)) inputs <- c(inputs, sub("^\\\\input\\{([^}]+)\\}$", "\\1", inp))
    sentence <- gsub("\\\\\\$", "#USD#", s)
    sentence <- gsub("[{}$]|\\\\(pending|emph|textit|textbf|text)\\b", "", sentence, perl = TRUE)
    sentence <- substr(trimws(gsub("\\s+", " ", gsub("#USD#", "$", sentence, fixed = TRUE))), 1, 300)
    sp_pend <- arg_spans(s, "pending"); sp_foot <- arg_spans(s, "footnote"); sp_cap <- arg_spans(s, "caption")
    cmds <- unique(sub("^\\\\", "", regmatches(s, gregexpr("\\\\[A-Za-z]+", s))[[1]]))
    used <- cmds[cmds %in% names(macros)]
    for (nm in used)
      uses[[length(uses) + 1L]] <- data.frame(origin = origin, part = part, section = sec,
        subsection = subsec, label = lab, tex_line = i, macro = nm, macro_value = unname(macros[nm]),
        sentence = sentence, stringsAsFactors = FALSE)
    if (grepl("\\\\(affiliation|ead|cormark|credit|orcid|tnotetext|fnmark|fntext|shorttitle|shortauthors|author)\\b", s, perl = TRUE)) next
    t <- s
    for (rx in REMOVE) t <- blank(t, rx)
    for (nm in used) t <- blank(t, paste0("\\\\", nm, "(\\{\\})?(?![A-Za-z])"))
    m <- gregexpr(NUM_LIT, t, perl = TRUE)[[1]]
    if (m[1] > 0) {
      st <- as.integer(m); ln <- attr(m, "match.length")
      for (j in seq_along(st)) {
        lit <- substring(t, st[j], st[j] + ln[j] - 1L)
        num <- gsub("[\\\\$,+]", "", lit); neg <- grepl("-", num); num <- sub("-", "", num, fixed = TRUE)
        a <- suppressWarnings(as.numeric(num)); if (is.na(a)) next
        dec <- if (grepl(".", num, fixed = TRUE)) nchar(sub("^[^.]*\\.", "", num)) else 0L
        before <- substring(t, max(1L, st[j] - 14L), st[j] - 1L)
        after  <- substring(t, st[j] + ln[j], st[j] + ln[j] + 22L)
        cl <- classify(lit, a, dec, before, after, sentence)
        loc <- if (origin != "manuscript") "table cell" else
               if (in_spans(st[j], sp_foot)) "footnote" else
               if (in_spans(st[j], sp_cap)) "caption" else
               if (in_float) "float note" else "prose"
        rows[[length(rows) + 1L]] <- data.frame(origin = origin, part = part, section = sec,
          subsection = subsec, label = lab, tex_line = i, location = loc,
          literal = gsub("\\\\", "", lit), value = if (neg) -a else a, abs = a, dec = dec,
          neg = neg, class = cl, pending = in_spans(st[j], sp_pend), sentence = sentence,
          context_after = trimws(gsub("\\s+", " ", substring(t, st[j] + ln[j], st[j] + ln[j] + 70L))),
          stringsAsFactors = FALSE)
      }
    }
    if (grepl("\\\\end\\{(figure|table)", s)) in_float <- FALSE
  }
  list(lits = bind_rows(rows), uses = bind_rows(uses), inputs = unique(inputs))
}

tex_all <- readLines(MANUSCRIPT, warn = FALSE, encoding = "UTF-8")
b0 <- grep("\\\\begin\\{document\\}", tex_all)[1]
b1 <- utils::tail(grep("\\\\end\\{document\\}", tex_all), 1)
body <- tex_all; body[c(seq_len(b0), b1:length(body))] <- ""   # keep line numbers
ms <- parse_tex(body, "manuscript", "main")
lits <- ms$lits; uses <- ms$uses
cat(sprintf("  %d number(s) in the text (%d structural or design), %d macro use(s), %d input(s)\n",
            nrow(lits), sum(lits$class %in% c("structural", "design")), nrow(uses), length(ms$inputs)))

# ---- Files to search, and the number index -------------------------------------------

kind_of <- function(f) ifelse(grepl("\\.[Rr]$", f), "script",
                       ifelse(grepl("console", basename(f), ignore.case = TRUE), "console",
                       ifelse(grepl("\\.log$|_log\\.txt$", f, ignore.case = TRUE), "log",
                       ifelse(grepl("\\.tex$", f, ignore.case = TRUE), "table", "output"))))
KIND_RANK <- c(output = 1L, log = 2L, console = 3L, table = 4L, script = 5L)

files <- unlist(lapply(SEARCH_DIRS[dir.exists(SEARCH_DIRS)], list.files, recursive = TRUE,
                       full.names = TRUE, pattern = "\\.(log|txt|csv|tex|md)$", ignore.case = TRUE))
script_files <- list.files(SCRIPT_DIR, pattern = "\\.[Rr]$", full.names = TRUE)
script_files <- script_files[basename(script_files) != "find_number_sources.R"]
files <- c(files, script_files)
files <- files[!grepl(echoes, basename(files))]
fi <- file.info(files)
too_big <- files[!is.na(fi$size) & fi$size > MAX_MB * 1e6]
if (length(too_big)) cat(sprintf("  skipped %d file(s) over %d MB: %s\n", length(too_big), MAX_MB,
                                 paste(basename(too_big), collapse = ", ")))
files <- setdiff(files, too_big)
fi <- file.info(files)
FIL <- data.frame(fid = seq_along(files), path = files, kind = kind_of(files),
                mtime = fi$mtime, stringsAsFactors = FALSE)
FIL$rank <- KIND_RANK[FIL$kind]
cat(sprintf("  searching %d file(s): %s\n", nrow(FIL),
            paste(sprintf("%d %s", tabulate(match(FIL$kind, names(KIND_RANK)), 5), names(KIND_RANK)), collapse = ", ")))

lines_of <- function(f) {
  x <- tryCatch(readLines(f, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
  if (grepl("\\.log$|console", f, ignore.case = TRUE)) x[grepl("^[>+]", x)] <- ""
  x
}
text <- lapply(FIL$path, lines_of)

# Every number on every line, once, sorted by size, so a lookup is a
# binary search rather than a regex over every file.
NUM_TOKEN <- "(?<![A-Za-z0-9_.])-?(?:[0-9]{1,3}(?:,[0-9]{3})+|[0-9]+)(?:\\.[0-9]+)?(?:[eE][-+]?[0-9]+)?(?![0-9])"
tok <- bind_rows(lapply(seq_along(text), function(k) {
  x <- text[[k]]
  if (!length(x)) return(NULL)
  m <- regmatches(x, gregexpr(NUM_TOKEN, x, perl = TRUE, useBytes = TRUE))
  n <- lengths(m)
  if (!sum(n)) return(NULL)
  data.frame(fid = k, line = rep(seq_along(x), n), token = unlist(m), stringsAsFactors = FALSE)
}))
tok$neg <- startsWith(tok$token, "-")
.num <- gsub(",", "", sub("^-", "", tok$token))
tok$abs <- suppressWarnings(as.numeric(.num))
tok$dec <- ifelse(grepl("[eE]", .num), 10L,
                  ifelse(grepl(".", .num, fixed = TRUE), nchar(sub("^[^.]*\\.([0-9]*).*$", "\\1", .num)), 0L))
tok <- tok[!is.na(tok$abs), ]
tok <- tok[order(tok$abs), ]
tok$key <- paste(tok$fid, tok$line, sep = ":")
ABS <- tok$abs
rm(.num)
cat(sprintf("  indexed %s number(s)\n", format(nrow(tok), big.mark = ",")))

in_range <- function(lo, hi) {
  e <- 1e-9 * max(1, abs(lo), abs(hi))     # relative: a fixed 1e-12 vanishes above about 1,000
  i <- findInterval(lo - e, ABS) + 1L; j <- findInterval(hi + e, ABS)
  if (j >= i) i:j else integer(0)
}

# Rows of `tok` that print as this number once rounded.
match_cache <- new.env()
match_number <- function(a, dec, neg, cl, frac) {
  key <- paste(a, dec, neg, cl, frac)
  if (!is.null(match_cache[[key]])) return(match_cache[[key]])
  exact <- function(idx, x) idx[abs(tok$abs[idx] - x) < 1e-9 * max(1, x)]
  near  <- function(x, d) {
    idx <- in_range(x - 0.5 * 10^-d, x + 0.5 * 10^-d)
    idx[tok$dec[idx] > d | (tok$dec[idx] == d & abs(tok$abs[idx] - x) < 1e-9 * max(1, x))]
  }
  # A count matches exactly; a whole-dollar figure may be a rounded decimal.
  idx <- if (dec == 0 && cl == "money") near(a, 0L) else if (dec == 0) exact(in_range(a, a), a) else near(a, dec)
  if (dec == 0 && cl != "money") idx <- idx[tok$dec[idx] == 0]
  if (frac && a <= 100)                                          # 38 percent as 0.38
    idx <- c(idx, near(a / 100, dec + 2L))
  if (cl == "money" && dec == 0 && a >= 1000 && a %% 100 == 0)    # $13,300 as 13.3 thousand
    idx <- c(idx, near(a / 1000, if (a %% 1000 == 0) 0L else 1L))
  if (neg) idx <- idx[tok$neg[idx]]
  if (!neg && cl %in% c("percent", "count", "money")) idx <- idx[!tok$neg[idx]]
  idx <- unique(idx)
  assign(key, idx, envir = match_cache)
  idx
}

# ---- Who writes a file ---------------------------------------------------------------

script_text <- setNames(lapply(script_files, function(f) readLines(f, warn = FALSE, encoding = "UTF-8")),
                        basename(script_files))
WRITE_RX <- "write\\.csv|write_csv|fwrite|saveRDS|writeLines|sink\\(|ggsave|pdf\\(|png\\(|cat\\(.*file *=|file\\.path"
writer_cache <- new.env()
find_in_scripts <- function(pat) {
  out <- NULL
  for (sn in names(script_text)) {
    i <- grep(pat, script_text[[sn]], fixed = TRUE, useBytes = TRUE)
    if (length(i))
      out <- rbind(out, data.frame(script = sn, line = i,
                                   w = grepl(WRITE_RX, script_text[[sn]][i], useBytes = TRUE),
                                   stringsAsFactors = FALSE))
  }
  out
}

# A log belongs to the script in its name; RUN_ALL's console copies belong to
# the stage script RUN_ALL lists for them.
stage_script <- character(0)
if ("RUN_ALL.R" %in% names(script_text)) {
  .st <- regmatches(script_text[["RUN_ALL.R"]],
                    regexpr("id = \"[^\"]+\", script = \"[^\"]+\"", script_text[["RUN_ALL.R"]]))
  stage_script <- setNames(sub(".*script = \"([^\"]+)\".*", "\\1", .st), sub("^id = \"([^\"]+)\".*", "\\1", .st))
}
log_script <- function(b) {
  if (grepl("^(RUN_ALL|FINAL_PASS)_.*console", b)) {
    run <- sub("^(RUN_ALL|FINAL_PASS)_.*$", "\\1", b)
    id <- sub("^(RUN_ALL|FINAL_PASS)_(.*?)(_[0-9]{8}_[0-9]{6})?_console.*$", "\\2", b, perl = TRUE)
    sc <- unname(stage_script[id])
    return(c(if (length(sc) && !is.na(sc)) sc else paste0(run, ".R"), paste0("console copy of ", run, " stage ", id)))
  }
  pre <- sub("(_[0-9]{8}_[0-9]{6})?(_log)?\\.(log|txt)$", "", b)
  sc <- names(script_text)[sub("\\.[Rr]$", "", names(script_text)) == pre]
  if (!length(sc)) sc <- names(script_text)[startsWith(names(script_text), pre)]
  c(if (length(sc)) sc[1] else paste0(pre, ".R"), if (length(sc)) "log of" else "log of (script not found)")
}

# What the logs say each stage wrote: "wrote ...", "written: ...", and a
# "written to <folder>:" line followed by an indented list of names. The
# newest log naming a file decides its writer.
FILE_RX  <- "[A-Za-z0-9_.-]+\\.(csv|tex|txt|rds|pdf|png|md)\\b"
WROTE_RX <- "(?i)\\b(wrote|written|writing|saved|appended)\\b"
log_writes <- list()
for (k in which(FIL$kind %in% c("log", "console"))) {
  x <- text[[k]]; if (!length(x)) next
  sc <- log_script(basename(FIL$path[k]))[1]
  i <- which(grepl(WROTE_RX, x, perl = TRUE, useBytes = TRUE))
  hdr <- i[grepl(":\\s*$", x[i])]                     # "tables written to <folder>:"
  for (h in hdr) {
    j <- h + 1L
    while (j <= length(x) && grepl(paste0("^\\s+", FILE_RX, "\\s*$"), x[j], perl = TRUE)) { i <- c(i, j); j <- j + 1L }
  }
  nm <- regmatches(x[i], gregexpr(FILE_RX, x[i], perl = TRUE, useBytes = TRUE))
  if (sum(lengths(nm)))
    log_writes[[length(log_writes) + 1L]] <- data.frame(file = basename(unlist(nm)), script = sc,
      log = basename(FIL$path[k]), line = rep(i, lengths(nm)), mtime = FIL$mtime[k], stringsAsFactors = FALSE)
}
log_writes <- bind_rows(log_writes)
if (nrow(log_writes))
  log_writes <- log_writes %>% filter(!grepl("^(find_number_sources|run_logs)", file)) %>%
    arrange(desc(mtime)) %>% group_by(file) %>% slice(1) %>% ungroup()
cat(sprintf("  %d file(s) named as written in the logs\n", nrow(log_writes)))

# The newest log of each script (and of each RUN_ALL stage) is current; older
# ones hold earlier builds' values, which is what stage 1 wants and stage 2 does not.
FIL$owner <- ifelse(FIL$kind %in% c("log", "console"),
                    paste(FIL$kind, vapply(basename(FIL$path), function(b) log_script(b)[1], character(1))), FIL$path)
FIL$latest <- ave(as.numeric(FIL$mtime), FIL$owner, FUN = function(x) x == max(x)) == 1
cat(sprintf("  %d of %d log(s) are the newest of their script\n",
            sum(FIL$latest & FIL$kind %in% c("log", "console")), sum(FIL$kind %in% c("log", "console"))))

writer_of <- function(path) {
  if (!is.null(writer_cache[[path]])) return(writer_cache[[path]])
  b <- basename(path); res <- list(script = "", line = NA_integer_, how = "no log or script names it")
  if (grepl("\\.[Rr]$", b)) {
    res <- list(script = b, line = NA_integer_, how = "typed in the script")
  } else if (grepl("_[0-9]{8}_[0-9]{6}\\.(log|txt)$|_log\\.txt$|^(RUN_ALL|FINAL_PASS)_.*console", b)) {
    ls <- log_script(b); res <- list(script = ls[1], line = NA_integer_, how = ls[2])
  } else if (nrow(log_writes) && b %in% log_writes$file) {
    w <- log_writes[log_writes$file == b, ]
    res <- list(script = w$script[1], line = NA_integer_, how = paste0("its log says it wrote it (", w$log[1], ":", w$line[1], ")"))
  } else {
    stem <- sub("\\.[^.]+$", "", b); how <- "named"
    h <- find_in_scripts(b)
    if (is.null(h)) h <- find_in_scripts(paste0("\"", stem, "\""))
    if (is.null(h)) {                       # names built with paste0(): shorten at underscores
      parts <- strsplit(stem, "_", fixed = TRUE)[[1]]
      while (is.null(h) && length(parts) > 2) {
        parts <- utils::head(parts, -1)
        h <- find_in_scripts(paste(parts, collapse = "_")); how <- "named by prefix"
      }
    }
    if (!is.null(h)) {
      h <- h[order(h$script %in% HELPER_SCRIPTS, !h$w), ]
      cands <- unique(h$script)
      res <- list(script = h$script[1], line = h$line[1],
                  how = paste0(if (h$w[1]) sub("named", "written", how) else how, " in the script (no log names it",
                               if (length(cands) > 1) paste0("; also named in ", paste(utils::head(cands[-1], 4), collapse = ", ")) else "",
                               ")"))
    }
  }
  assign(path, res, envir = writer_cache)
  res
}

# ---- Find the sources of a set of numbers ------------------------------------------------
# d: rows with abs, dec, neg, class, and a group (numbers searched together).
# Returns the best MAX_CANDIDATES sources per row, graded.

find_sources <- function(d, n_keep = MAX_CANDIDATES, exclude_base = NULL, latest_only = FALSE) {
  if (!nrow(d)) return(NULL)
  d$rid <- seq_len(nrow(d))
  # A percentage is also looked for as a fraction when its sentence speaks in
  # percent; elsewhere 17.9 hours would match 0.179 by accident.
  if (!"frac" %in% names(d)) {
    pg <- tapply(d$class == "percent", d$group, any)
    d$frac <- d$class == "percent" | (pg[d$group] & d$dec <= 2 & d$abs >= 1)
  }
  keys <- lapply(seq_len(nrow(d)), function(r) {
    if (d$class[r] %in% c("structural", "design")) return(character(0))
    idx <- match_number(d$abs[r], d$dec[r], d$neg[r], d$class[r], d$frac[r])
    k <- unique(tok$key[idx])
    if (latest_only && length(k)) k <- k[FIL$latest[as.integer(sub(":.*$", "", k))]]
    if (!is.null(exclude_base) && length(k)) {
      fid <- as.integer(sub(":.*$", "", k))
      k <- k[!basename(FIL$path[fid]) %in% exclude_base]
    }
    k
  })
  out <- vector("list", nrow(d))
  for (r in seq_len(nrow(d))) {
    k <- keys[[r]]
    if (!length(k)) {
      out[[r]] <- data.frame(rid = r, evidence = "not found", n_candidates = 0L, partners = 0L,
                             fid = NA_integer_, line = NA_integer_, stringsAsFactors = FALSE)
      next
    }
    mates <- setdiff(which(d$group == d$group[r] & d$literal != d$literal[r]), r)
    np <- if (length(mates)) Reduce(`+`, lapply(mates, function(q) k %in% keys[[q]])) else rep(0L, length(k))
    fid <- as.integer(sub(":.*$", "", k)); ln <- as.integer(sub("^.*:", "", k))
    cand <- data.frame(fid = fid, line = ln, partners = np, stringsAsFactors = FALSE)
    cand <- cand[order(-cand$partners, FIL$rank[cand$fid], -as.numeric(FIL$mtime[cand$fid]), cand$fid, cand$line), ]
    top <- utils::head(cand, n_keep)
    ev <- if (top$partners[1] > 0) sprintf("with %d sentence partner(s)", top$partners[1]) else
          if (sig_digits(d$literal[r]) >= 3) "distinctive number alone" else "short number alone"
    out[[r]] <- data.frame(rid = r, evidence = ev, n_candidates = nrow(cand),
                           partners = top$partners, fid = top$fid, line = top$line,
                           stringsAsFactors = FALSE)
  }
  res <- bind_rows(out)
  ok <- !is.na(res$fid)
  res$source_kind <- ifelse(ok, FIL$kind[res$fid], "")
  res$source_file <- ifelse(ok, FIL$path[res$fid], "")
  res$source_modified <- ifelse(ok, format(FIL$mtime[res$fid], "%Y-%m-%d %H:%M"), "")
  res$source_text <- ifelse(ok, mapply(function(f, l) if (is.na(f)) "" else
                                 substr(trimws(text[[f]][l]), 1, 200), res$fid, res$line), "")
  w <- lapply(res$source_file, function(p) if (nzchar(p)) writer_of(p) else list(script = "", line = NA_integer_, how = ""))
  for (q in which(grepl("\\.csv$", res$source_file) & !is.na(res$line))) {
    f <- match(res$source_file[q], FIL$path); hdr <- text[[f]][1]
    if (!grepl("\\bmodule\\b", hdr, perl = TRUE) || res$line[q] < 2) next
    # parsed as CSV, since labels hold commas inside quotes
    val <- tryCatch(as.character(utils::read.csv(text = c(hdr, text[[f]][res$line[q]]),
                                                 stringsAsFactors = FALSE)$module[1]),
                    error = function(e) NA_character_)
    if (!is.na(val) && paste0(val, ".R") %in% names(script_text))
      w[[q]] <- list(script = paste0(val, ".R"), line = NA_integer_, how = "module column on the matched row")
  }
  res$writer_script <- vapply(w, `[[`, "", "script")
  res$writer_line   <- vapply(w, function(z) as.integer(z$line), integer(1))
  res$writer_how    <- vapply(w, `[[`, "", "how")
  res$fid <- NULL
  left_join(d, res, by = "rid") %>% select(-rid)
}

# =============================================================================
# STAGE 1. Still red first
# =============================================================================

red <- lits %>% filter(pending) %>% mutate(group = paste0("line ", tex_line))
if (length(EXTRA_GROUPS)) {
  for (g in names(EXTRA_GROUPS)) {
    x <- EXTRA_GROUPS[[g]]
    num <- gsub("[$,+]", "", x); neg <- grepl("-", num); num <- sub("-", "", num, fixed = TRUE)
    a <- as.numeric(num); dec <- ifelse(grepl(".", num, fixed = TRUE), nchar(sub("^[^.]*\\.", "", num)), 0L)
    red <- bind_rows(red, data.frame(origin = "extra group", part = "", section = g, subsection = "",
      label = "", tex_line = NA_integer_, location = "", literal = x, value = ifelse(neg, -a, a),
      abs = a, dec = dec, neg = neg, class = ifelse(dec == 0 & a >= 100, "count", ifelse(dec > 0, "estimate", "integer")),
      pending = TRUE, sentence = g, group = paste0("extra: ", g), stringsAsFactors = FALSE))
  }
}

cat("\n=============================================================================\n",
    "STAGE 1. STILL RED: ", nrow(red), " number(s) in ", length(unique(red$group)), " group(s)\n",
    "=============================================================================\n", sep = "")

sr_rows <- list(); recomputed <- NULL
if (nrow(red)) {
  red_found <- find_sources(red %>% mutate(class = ifelse(class %in% c("structural", "design"), "integer", class)),
                            n_keep = 10)
  sr_rows[[1]] <- red_found %>% mutate(search = "number")

  # The code that should compute each group: lines holding two or more of the
  # sentence's key words, in scripts first and then logs.
  STOP <- c("percent", "before", "after", "first", "birth", "their", "which", "rather", "inside",
            "outside", "measure", "entirely", "remaining", "persons", "person", "waves", "these",
            "those", "about", "between", "against", "there", "where", "while", "within", "without",
            "would", "could", "should", "other", "others", "still", "paper", "appendix", "section",
            "table", "figure", "itself", "because", "since", "under", "above", "below", "every",
            "group", "groups", "share", "shares", "women", "mothers", "fathers", "years", "sample",
            "based", "carry", "carries", "asked", "training", "reported", "online", "point", "points",
            "level", "levels", "never", "leave", "means", "cannot", "detect", "effect", "regression")
  kw_hits <- list()
  for (g in unique(red$group)) {
    sen <- tolower(red$sentence[red$group == g][1])
    w <- unique(regmatches(sen, gregexpr("[a-z][a-z'-]{4,}", sen))[[1]])
    w <- w[!w %in% STOP & !grepl("'s$", w)]
    if (!length(w)) next
    rx <- unique(gsub("-", "[-_ ]?", substr(w, 1, 8), fixed = TRUE))
    SYN <- c("another place|other place" = "jttop|other.?place|elsewhere",
             "self-employ"               = "self.?emp|is_self",
             "business income"           = "business",
             "dosage|conditional on"     = "dosage|ontrained|conditional.on.training",
             "pre-birth base"            = "base_?rate|base of")
    syn <- unname(SYN[vapply(names(SYN), function(z) grepl(z, sen, perl = TRUE), logical(1))])
    need <- if (length(rx) >= 2 || length(syn)) 2L else 1L
    for (k in which(FIL$kind %in% c("script", "log", "console"))) {
      x <- tolower(text[[k]]); if (!length(x)) next
      sc <- Reduce(`+`, lapply(rx, function(r) grepl(r, x, perl = TRUE, useBytes = TRUE)))
      if (length(syn)) sc <- sc + 2L * Reduce(`+`, lapply(syn, function(r) grepl(r, x, perl = TRUE, useBytes = TRUE)))
      i <- which(sc >= need)
      if (length(i))
        kw_hits[[length(kw_hits) + 1L]] <- data.frame(group = g, fid = k, line = i, score = sc[i],
                                                      stringsAsFactors = FALSE)
    }
  }
  if (length(kw_hits)) {
    kh <- bind_rows(kw_hits) %>%
      arrange(group, desc(score), FIL$rank[fid], desc(as.numeric(FIL$mtime[fid]))) %>%
      group_by(group) %>% slice(seq_len(min(n(), 8))) %>% ungroup()
    kh <- kh %>% mutate(source_kind = FIL$kind[fid], source_file = FIL$path[fid],
                        source_modified = format(FIL$mtime[fid], "%Y-%m-%d %H:%M"),
                        source_text = mapply(function(f, l) substr(trimws(text[[f]][l]), 1, 200), fid, line),
                        evidence = sprintf("%d key word(s) on the line", score), search = "key words",
                        writer_script = ifelse(source_kind == "script", basename(source_file), ""),
                        writer_line = ifelse(source_kind == "script", line, NA_integer_),
                        writer_how = ifelse(source_kind == "script", "code that may compute it", "")) %>%
      left_join(red %>% group_by(group) %>%
                  summarise(literal = paste(literal, collapse = " & "), section = first(section),
                            subsection = first(subsection), tex_line = first(tex_line),
                            sentence = first(sentence), .groups = "drop"), by = "group") %>%
      select(-fid, -score)
    sr_rows[[2]] <- kh
  }

  # ---- Recompute what can be defined -----------------------------------------
  # Candidates come from the current frame, so they will differ a little from
  # values printed on an older build. The candidate nearest the red value
  # shows the definition; its current value is the replacement.
  recompute_still_red <- function(red) {
    has_topic <- function(rx) any(grepl(rx, red$sentence, ignore.case = TRUE))
    topics <- c(selfemp = has_topic("self-employ|business income"),
                otherplace = has_topic("another place|other place"),
                dosage = has_topic("dosage|pre-birth base of"))
    topics <- names(topics)[topics]
    if (!length(topics)) { cat("  no red sentence matches a known recipe.\n"); return(NULL) }
    if (!exists("td_frame", mode = "function")) {
      .c <- c("00_contract.R", file.path("..", "00_contract.R")); .c <- .c[file.exists(.c)]
      if (length(.c)) {
        cat("  contract not loaded in this session; sourcing ", .c[1], "\n", sep = "")
        try(source(.c[1]), silent = TRUE)
      }
    }
    if (!exists("td_frame", mode = "function")) {
      cat("  td_frame() is not available, so nothing is recomputed.\n"); return(NULL)
    }
    fr <- tryCatch(td_frame(need_cols = c("person_id", "wave", "female", "ever_parent"),
                            context = "find_number_sources"),
                   error = function(e) { cat("  td_frame() failed: ", conditionMessage(e), "\n", sep = ""); NULL })
    if (is.null(fr)) return(NULL)
    fr <- as.data.frame(fr)
    first_of <- function(v) { v <- v[v %in% names(fr)]; if (length(v)) v[1] else NA_character_ }
    one <- function(v) !is.na(v) & v == 1
    # Event time from the first observed 0 -> 1 move in ever_parent, as
    # reconcile_pre_birth_base.R dates a first birth. Never-parents and people
    # with an observed first birth make up the roster here; the age-experience
    # screen is not applied, so roster shares are close to, not equal to, 03a's.
    ev <- fr %>% arrange(person_id, wave) %>% group_by(person_id) %>%
      mutate(.prev = dplyr::lag(ever_parent),
             .tr = !is.na(.prev) & .prev == 0 & one(ever_parent),
             .bw = if (any(.tr)) min(wave[.tr]) else NA_real_,
             .never = all(!one(ever_parent))) %>%
      ungroup() %>% mutate(k = wave - .bw, roster = .never | !is.na(.bw))
    out <- list()
    add <- function(topic, quantity, frame, x, stat = "share", hint = "") {
      x <- x[!is.na(x)]
      v <- if (!length(x)) NA_real_ else switch(stat, share = 100 * mean(x), mean = mean(x), median = stats::median(x))
      out[[length(out) + 1L]] <<- data.frame(topic = topic, quantity = quantity, frame = frame,
        n = length(x), value = round(v, 2), unit = if (stat == "share") "percent" else "hours",
        hint = hint, stringsAsFactors = FALSE)
    }
    if ("selfemp" %in% topics) {
      mo <- ev %>% filter(female == 1, !is.na(k))
      for (v in intersect(c("is_self_employed", "is_selfemp_narrow"), names(ev))) {
        for (den in c("all person-waves", "employed person-waves")) {
          d <- mo
          if (den == "employed person-waves") { if (!"employed" %in% names(d)) next; d <- filter(d, one(employed)) }
          add("selfemp", sprintf("mothers, %s, k < 0", v), den, one(d[[v]])[d$k < 0], hint = "before")
          add("selfemp", sprintf("mothers, %s, k = -3 to -1", v), den, one(d[[v]])[d$k >= -3 & d$k <= -1], hint = "before")
          add("selfemp", sprintf("mothers, %s, k >= 6", v), den, one(d[[v]])[d$k >= 6], hint = "six or more|after")
        }
      }
      if (all(c("fy_business_income_nonzero", "fy_wages") %in% names(ev))) {
        for (fl in c("all women in the frame", "women on the roster")) {
          d <- ev %>% filter(female == 1)
          if (fl == "women on the roster") d <- filter(d, roster)
          add("selfemp", "women, business income with zero wages", fl,
              one(d$fy_business_income_nonzero) & !is.na(d$fy_wages) & d$fy_wages == 0, hint = "business")
          add("selfemp", "women, business income with zero or missing wages", fl,
              one(d$fy_business_income_nonzero) & (is.na(d$fy_wages) | d$fy_wages <= 0), hint = "business")
        }
      } else cat("  business-income columns not in the frame; that recipe is skipped.\n")
    }
    if ("otherplace" %in% topics) {
      wp_paid <- first_of(c("jttpewt_obs", "jttpewt_any", "jttpewt_raw", "jttpewt"))
      wp_own  <- first_of(c("jttpeot_obs", "jttpeot_any", "jttpeot_raw", "jttpeot"))
      op_paid <- first_of(c("jttopwt_raw", "jttopwt_any", "jttopwt"))
      op_own  <- first_of(c("jttopot_raw", "jttopot_any", "jttopot"))
      if (!anyNA(c(wp_paid, wp_own, op_paid, op_own)) && "training_module_asked" %in% names(ev)) {
        e2 <- ev %>% filter(one(training_module_asked)) %>%
          mutate(.wp = one(.data[[wp_paid]]) | one(.data[[wp_own]]),
                 .op = one(.data[[op_paid]]) | one(.data[[op_own]]))
        for (fl in c("frame", "roster", "roster, employed")) {
          d <- e2
          if (fl != "frame") d <- filter(d, roster)
          if (fl == "roster, employed") { if (!"employed" %in% names(d)) next; d <- filter(d, one(employed)) }
          add("otherplace", "training only at another place, none at the workplace",
              paste("asked person-waves,", fl), d$.op & !d$.wp)
          add("otherplace", "paid-time training only at another place",
              paste("asked person-waves,", fl), one(d[[op_paid]]) & !d$.wp)
        }
      } else cat("  other-place columns not all in the frame; that recipe is skipped.\n")
    }
    if ("dosage" %in% topics) {
      h <- first_of("total_training_hours"); hz <- first_of(c("train_hours_z", "total_training_hours_z"))
      if (is.na(h)) h <- hz                   # trainees are then the person-waves with hours above zero
      if (!is.na(h) && "employed" %in% names(ev)) {
        pre <- ev %>% filter(one(employed), !is.na(k), k >= -3, k <= -1)
        for (sx in list(c("treated women", "1"), c("treated men", "0"))) {
          d <- pre %>% filter(female == as.numeric(sx[2]))
          tr <- d[[h]][!is.na(d[[h]]) & d[[h]] > 0]
          add("dosage", paste(sx[1], "k = -3 to -1, employed: mean hours among trainees"), "roster", tr, "mean")
          add("dosage", paste(sx[1], "k = -3 to -1, employed: median hours among trainees"), "roster", tr, "median")
          if (!is.na(hz)) add("dosage", paste(sx[1], "k = -3 to -1, employed: mean hours, zeros included"),
                              "roster", d[[hz]], "mean")
        }
      } else cat("  training-hours columns not in the frame; that recipe is skipped.\n")
    }
    res <- bind_rows(out)
    if (!nrow(res)) return(NULL)
    rx_of <- c(selfemp = "self-employ|business income", otherplace = "another place|other place",
               dosage = "dosage|pre-birth base of")
    res$nearest_red <- NA_character_; res$distance <- NA_real_
    for (r in seq_len(nrow(res))) {
      lv <- red %>% filter(grepl(rx_of[res$topic[r]], sentence, ignore.case = TRUE))
      if (nzchar(res$hint[r])) lv <- lv %>% filter(grepl(res$hint[r], context_after, ignore.case = TRUE))
      if (!nrow(lv)) next
      dd <- abs(lv$abs - res$value[r]); j <- which.min(dd)
      res$nearest_red[r] <- lv$literal[j]; res$distance[r] <- round(dd[j], 2)
    }
    res %>% group_by(nearest_red) %>% mutate(closest = !is.na(distance) & distance == min(distance, na.rm = TRUE)) %>%
      ungroup() %>% as.data.frame()
  }
  if (isTRUE(RECOMPUTE_STILL_RED)) {
    cat("\nRecomputing candidates from the frame:\n")
    recomputed <- tryCatch(recompute_still_red(red),
                           error = function(e) { cat("  recomputation stopped: ", conditionMessage(e), "\n", sep = ""); NULL })
    if (!is.null(recomputed)) write.csv(recomputed, OUT_RECOMPUTED, row.names = FALSE)
  }
}

sr <- bind_rows(sr_rows)
if (nrow(sr)) {
  sr <- sr %>% select(any_of(c("search", "section", "subsection", "tex_line", "literal", "evidence",
                               "source_kind", "source_file", "line", "source_text", "source_modified",
                               "writer_script", "writer_line", "writer_how", "sentence"))) %>%
    arrange(tex_line, match(search, c("number", "key words")))
  write.csv(sr, OUT_STILL_RED, row.names = FALSE)
}

# What stage 1 found, group by group.
for (g in unique(red$group)) {
  rg <- red %>% filter(group == g)
  cat(sprintf("\n[%s%s] %s\n", if (nzchar(rg$subsection[1])) rg$subsection[1] else rg$section[1],
              if (is.na(rg$tex_line[1])) "" else paste0(", line ", rg$tex_line[1]),
              paste(rg$literal, collapse = ", ")))
  cat("  ", substr(rg$sentence[1], 1, 160), "\n", sep = "")
  num <- sr %>% filter(search == "number", literal %in% rg$literal, tex_line %in% rg$tex_line)
  strong <- num %>% filter(!evidence %in% c("not found", "short number alone"))
  if (!nrow(strong)) cat("  files: no output, log or script holds these at the printed precision\n") else
    for (r in seq_len(min(nrow(strong), 4)))
      cat(sprintf("  %-6s %-28s %s:%s  %s\n", strong$literal[r], strong$evidence[r],
                  basename(strong$source_file[r]), strong$line[r], substr(strong$source_text[r], 1, 70)))
  kw <- sr %>% filter(search == "key words", tex_line %in% rg$tex_line)
  if (nrow(kw)) {
    cat("  code and log lines on the same topic:\n")
    for (r in seq_len(min(nrow(kw), 5)))
      cat(sprintf("    %s:%s  %s\n", basename(kw$source_file[r]), kw$line[r], substr(kw$source_text[r], 1, 90)))
  }
  if (!is.null(recomputed)) {
    rc <- recomputed %>% filter(nearest_red %in% rg$literal, closest)
    for (r in seq_len(nrow(rc)))
      cat(sprintf("  recomputed: %s is nearest %s (%s, %s, n = %s)\n", rc$nearest_red[r],
                  format(rc$value[r]), rc$quantity[r], rc$frame[r], format(rc$n[r], big.mark = ",")))
  }
}
cat(sprintf("\nStage 1 written to %s%s\n", OUT_STILL_RED,
            if (!is.null(recomputed)) paste0(" and ", OUT_RECOMPUTED) else ""))

# =============================================================================
# STAGE 2. The reference map
# =============================================================================

cat("\n=============================================================================\n",
    "STAGE 2. REFERENCE MAP\n",
    "=============================================================================\n", sep = "")

# 2a. Numbers typed in the text.
m_lits <- find_sources(lits %>% mutate(group = paste0("line ", tex_line)), latest_only = MAP_LATEST_LOGS_ONLY)
m_lits$number_type <- "typed in the text"

# 2b. Macros: written by whatever writes the macro file, from an input found
# by value.
m_uses <- NULL
if (nrow(uses)) {
  first_num <- function(z) {
    r <- regmatches(z, regexpr("[-+]?\\$?(?:[0-9]{1,3}(?:,[0-9]{3})+|[0-9]+)(?:\\.[0-9]+)?", z, perl = TRUE))
    if (length(r)) r else NA_character_
  }
  mv <- uses %>% mutate(.v = to_ascii(gsub("\\\\", "", macro_value)))
  mv$literal <- vapply(mv$.v, first_num, character(1), USE.NAMES = FALSE)
  mv <- mv %>% filter(!is.na(literal)) %>%
    mutate(num = gsub("[$,+]", "", literal), neg = grepl("-", num), num = sub("-", "", num, fixed = TRUE),
           abs = as.numeric(num), dec = ifelse(grepl(".", num, fixed = TRUE), nchar(sub("^[^.]*\\.", "", num)), 0L),
           value = ifelse(neg, -abs, abs), location = "macro", pending = FALSE,
           class = ifelse(grepl("$", literal, fixed = TRUE), "money", ifelse(dec == 0 & abs >= 100, "count",
                          ifelse(dec > 0, "estimate", "integer"))),
           group = paste0("macro ", macro), frac = dec <= 2 & abs >= 1 & abs <= 100) %>% select(-num, -.v)
  m_uses <- find_sources(mv, exclude_base = c(basename(MACRO_FILE)), latest_only = MAP_LATEST_LOGS_ONLY)
  mw <- if (!is.na(MACRO_FILE)) writer_of(MACRO_FILE) else list(script = "", how = "")
  m_uses <- m_uses %>% mutate(number_type = "macro",
                              macro_writer = paste0(mw$script, " writes ", basename(MACRO_FILE)))
}

# 2c. Table fragments the text inputs, and the numbers inside them.
frag <- NULL; m_cells <- NULL
pt_logs <- FIL$path[grepl("^paper_tables_[0-9]{8}_[0-9]{6}\\.log$", basename(FIL$path))]
pt_log <- if (length(pt_logs)) text[[match(pt_logs[which.max(file.info(pt_logs)$mtime)], FIL$path)]] else character(0)
if (length(ms$inputs)) {
  frag <- data.frame(input = ms$inputs, stringsAsFactors = FALSE) %>%
    mutate(path = file.path(TEX_DIR, ifelse(grepl("\\.tex$", input), input, paste0(input, ".tex"))),
           exists = file.exists(path), name = sub("\\.tex$", "", basename(path)))
  wf <- lapply(frag$path, writer_of)
  frag$writer_script <- vapply(wf, `[[`, "", "script")
  frag$writer_line   <- vapply(wf, function(z) as.integer(z$line), integer(1))
  frag$writer_how    <- vapply(wf, `[[`, "", "how")
  frag$carried <- vapply(frag$name, function(n) any(grepl(paste0("carried: wrote .*", n, "\\.tex"), pt_log)), logical(1))
  if (isTRUE(FRAGMENT_CELLS)) {
    cells <- list()
    for (r in which(frag$exists & !(frag$name %in% sub("\\.tex$", "", basename(MACRO_FILE))))) {
      fl <- readLines(frag$path[r], warn = FALSE, encoding = "UTF-8")
      p <- parse_tex(fl, paste0("table:", frag$name[r]), "table")$lits
      if (nrow(p)) cells[[length(cells) + 1L]] <- p %>% mutate(group = paste0(frag$name[r], " row ", tex_line),
                                                               fragment = frag$name[r])
    }
    if (length(cells)) {
      cl <- bind_rows(cells)
      m_cells <- bind_rows(lapply(split(cl, cl$fragment), function(x)
        find_sources(x, exclude_base = paste0(x$fragment[1], ".tex"), latest_only = MAP_LATEST_LOGS_ONLY))) %>%
        mutate(number_type = "table cell")
      fw <- frag %>% select(fragment = name, fragment_writer = writer_script, carried)
      m_cells <- m_cells %>% left_join(fw, by = "fragment")
    }
  }
}

map <- bind_rows(m_lits, m_uses, m_cells) %>%
  mutate(still_red = pending,
         confidence = case_when(
           class %in% c("structural", "design") ~ "not a result",
           grepl("^with", evidence) ~ "strong",
           evidence == "distinctive number alone" ~ "moderate",
           evidence == "short number alone" ~ "weak",
           TRUE ~ "not found"))
map <- map %>% select(any_of(c("number_type", "still_red", "part", "section", "subsection", "label",
                               "origin", "tex_line", "location", "literal", "value", "class", "macro",
                               "fragment", "confidence", "evidence", "n_candidates", "partners",
                               "source_kind", "source_file", "line", "source_text", "source_modified",
                               "writer_script", "writer_line", "writer_how", "macro_writer",
                               "fragment_writer", "carried", "sentence"))) %>%
  arrange(match(number_type, c("typed in the text", "macro", "table cell")), tex_line)
write.csv(map, OUT_MAP, row.names = FALSE)

# One row per number: the best source only.
best <- map %>% group_by(number_type, origin, tex_line, literal) %>% slice(1) %>% ungroup()
op <- options(width = max(getOption("width"), 180))
cat("\nNumbers by type and confidence (best source per number):\n")
print(table(best$number_type, factor(best$confidence,
            levels = c("strong", "moderate", "weak", "not found", "not a result"))))
if (!is.null(frag)) {
  cat("\nTable fragments the text inputs:\n")
  for (r in seq_len(nrow(frag)))
    cat(sprintf("  %-28s %-24s %s%s\n", frag$name[r], frag$writer_script[r], frag$writer_how[r],
                if (frag$carried[r]) "; CARRIED: values typed in the writing script" else
                if (!frag$exists[r]) "; file not found" else ""))
}

# =============================================================================
# STAGE 3. By script
# =============================================================================

by_script <- best %>%
  filter(confidence != "not a result") %>%
  mutate(writer_script = ifelse(nzchar(writer_script), writer_script, "(no source found)")) %>%
  group_by(writer_script) %>%
  summarise(numbers = n(), strong = sum(confidence == "strong"), moderate = sum(confidence == "moderate"),
            weak = sum(confidence == "weak"), macros = sum(number_type == "macro"),
            table_cells = sum(number_type == "table cell"), still_red = sum(still_red, na.rm = TRUE),
            files = paste(sort(unique(basename(source_file[nzchar(source_file)]))), collapse = "; "),
            .groups = "drop") %>%
  arrange(writer_script == "(no source found)", desc(numbers))
write.csv(by_script, OUT_BY_SCRIPT, row.names = FALSE)
cat("\nNumbers per writing script (best source per number):\n")
print(as.data.frame(by_script %>% select(-files)), row.names = FALSE)

nf <- best %>% filter(confidence == "not found", number_type == "typed in the text")
cat(sprintf("\n%d number(s) typed in the text are found nowhere%s\n", nrow(nf),
            if (nrow(nf)) ". The first 25:" else "."))
for (r in seq_len(min(nrow(nf), 25)))
  cat(sprintf("  line %4d  %-10s %s\n", nf$tex_line[r], nf$literal[r], substr(nf$sentence[r], 1, 100)))
options(op)
cat(sprintf("\nWritten: %s (%d rows), %s (%d scripts)\n", OUT_MAP, nrow(map), OUT_BY_SCRIPT, nrow(by_script)))
