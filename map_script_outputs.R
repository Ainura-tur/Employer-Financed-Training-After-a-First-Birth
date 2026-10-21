# =============================================================================
# map_script_outputs.R
#
# Maps every R script in the project folder to the files it writes, reads,
# copies, deletes and sources, by parsing the code. No script is sourced or
# run, so this is safe while Part 2 stays gated off.
#
# Run from the project root:  Rscript map_script_outputs.R
#
# Writes to output/provenance/:
#   script_io_calls.csv     every file call found, with script, line and target
#   script_output_scan.csv  one row per script and output, with the scripts
#                           that read it and the scripts that source the writer
#   script_map_check.csv    script_output_map.csv checked against the scan
#                           (only when that file is in the project root)
#   output_files_check.csv  every file on disk under the folders in cfg:
#                           who writes it, and whether it predates its script
#                           or one of that script's inputs
#
# Limits. A name built from data at run time, such as a loop over arm names,
# appears as a pattern like T8_*.csv. Calls made through do.call(), get(), or
# a function passed as an argument are not followed. Wrapper functions are
# followed: if write_design(df, name) calls fwrite(), each call to
# write_design() is reported with the name it was given.
#
# Revised 2026-09-24:
#   * Arguments passed through `...` reach the helper's path, so
#     .out_path("x.csv"), which calls td_out("results", ...), resolves to x.csv.
#   * name <- if (cond) function(...) ... else function(...) defines `name`.
#   * When a helper still cannot be resolved, a file name written literally
#     among its arguments is used.
#   * A function's own arguments are never looked up among assignments, so a
#     wrapper such as .out(x, f) no longer borrows `f` from a loop elsewhere.
#   * Writes inside cfg$archive_functions (snapshot copies) stay in
#     script_io_calls.csv but do not make a script the writer of the original.
#   * Files under cfg$archive_dirs get status "archive" and are not tested.
#   * script_output_map.csv, when present, adds writers for names built at run
#     time; output_files_check.csv lists them in writers_from_map.
#   * A script's own outputs are not counted as its inputs when testing
#     whether an output is older than an input.
#
# Statuses in output_files_check.csv: archive, no script writes it, older
# than an input, script edited since, ok.
# =============================================================================

library(dplyr)

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

# ---- Settings ---------------------------------------------------------------

cfg <- list(
  root        = ".",
  exclude     = c("map_script_outputs.R"),
  out_dir     = file.path("output", "provenance"),
  curated_map = "script_output_map.csv",
  disk_dirs   = c("output", "run_logs",
                  file.path("els-cas-templates", "tables"),
                  file.path("els-cas-templates", "figures")),
  # Functions whose writes are copies kept for the record, such as snapshots.
  archive_functions = c("take_snapshot"),
  # Folders whose files are records, not outputs to keep current.
  archive_dirs      = c("snapshots", "run_logs"),
  # TRUE when script_output_map.csv lists every output, so that outputs found
  # in the code but missing from the map are reported.
  map_is_complete   = FALSE
)

# ---- File functions the scan recognises ---------------------------------------
# formals: the leading formal arguments, in order, used to match positional
# arguments. target: the formal that holds the path (alternatives with |).

registry <- read.table(header = TRUE, stringsAsFactors = FALSE, text = "
role    fun             formals               target
write   write.csv       x,file                file
write   write.csv2      x,file                file
write   write.table     x,file                file
write   write           x,file                file
write   writeLines      text,con              con
write   saveRDS         object,file           file
write   save            ...                   file
write   save.image      file                  file
write   cat             ...                   file
write   sink            file                  file
write   capture.output  ...                   file
write   dput            x,file                file
write   dump            list,file             file
write   fwrite          x,file                file
write   write_csv       x,file                file|path
write   write_csv2      x,file                file|path
write   write_tsv       x,file                file|path
write   write_delim     x,file                file|path
write   write_rds       x,file                file|path
write   write_lines     x,file                file|path
write   write_file      x,file                file|path
write   write_xlsx      x,path                path
write   write.xlsx      x,file                file
write   saveWorkbook    wb,file               file
write   write_dta       data,path             path
write   ggsave          filename,plot         filename
write   pdf             file                  file
write   cairo_pdf       filename              filename
write   svg             filename              filename
write   png             filename              filename
write   jpeg            filename              filename
write   tiff            filename              filename
write   save_kable      x,file                file
write   etable          ...                   file
write   texreg          ...                   file
write   stargazer       ...                   out
write   modelsummary    models,output         output
write   datasummary     formula,data,output   output
write   print           ...                   file|target
write   qsave           x,file                file
write   write_parquet   x,sink                sink
write   write_feather   x,sink                sink
copy    file.copy       from,to               to
copy    file.rename     from,to               to
copy    file_copy       path,new_path         new_path
delete  unlink          x                     x
delete  file.remove     ...                   ...
delete  file_delete     path                  path
read    read.csv        file                  file
read    read.csv2       file                  file
read    read.table      file                  file
read    read.delim      file                  file
read    readRDS         file                  file
read    load            file                  file
read    readLines       con                   con
read    fread           input,file            input|file
read    read_csv        file                  file
read    read_csv2       file                  file
read    read_tsv        file                  file
read    read_delim      file                  file
read    read_rds        file                  file
read    read_lines      file                  file
read    read_file       file                  file
read    read_excel      path                  path
read    read_xlsx       path                  path
read    read.xlsx       xlsxFile              xlsxFile|file
read    read_dta        file                  file
read    qread           file                  file
read    read_parquet    file                  file
read    read_feather    file                  file
read    scan            file                  file
source  source          file                  file
source  sys.source      file                  file
")

# ---- Small utilities ----------------------------------------------------------

is_empty_arg <- function(x) is.symbol(x) && !nzchar(as.character(x))

call_name <- function(head) {
  if (is.symbol(head)) return(as.character(head))
  if (is.character(head) && length(head) == 1) return(head)
  if (is.call(head) && length(head) == 3 &&
      as.character(head[[1]]) %in% c("::", ":::")) return(as.character(head[[3]]))
  ""
}

one_line <- function(x, width = 160) {
  s <- paste(trimws(deparse(x, width.cutoff = 500L)), collapse = " ")
  if (nchar(s) > width) paste0(substr(s, 1, width - 3), "...") else s
}

collector <- function() {
  env <- new.env(parent = emptyenv())
  n <- 0L
  list(add = function(x) { n <<- n + 1L; assign(sprintf("%09d", n), x, envir = env) },
       get = function() if (n == 0L) list() else unname(mget(sprintf("%09d", seq_len(n)), envir = env)))
}

glob_to_regex <- function(p) {
  p <- gsub("([][.|()^{}+$\\\\])", "\\\\\\1", p, perl = TRUE)
  p <- gsub("*", ".*", p, fixed = TRUE)
  p <- gsub("?", ".", p, fixed = TRUE)
  paste0("^", p, "$")
}

# A pattern is informative when at least two letters or digits other than
# the extension are fixed, so neither "*.csv" nor "*_*.log" matches every
# csv or log in the project.
informative <- function(p) {
  nchar(gsub("[^[:alnum:]]", "", gsub("*", "", tools::file_path_sans_ext(p), fixed = TRUE))) >= 2
}

# For each pattern in a, the indices of the patterns in b that match it.
# Exact names are matched through a hash; each wildcard pattern costs one
# regex pass, so the cost grows linearly with the number of outputs.
match_index <- function(a, b) {
  res <- vector("list", length(a))
  if (!length(a) || !length(b)) return(res)
  ia <- informative(a); ib <- informative(b)
  wa <- grepl("*", a, fixed = TRUE); wb <- grepl("*", b, fixed = TRUE)
  exact <- which(!wb & ib)
  if (length(exact)) {
    key <- split(exact, b[exact])
    for (i in which(!wa & ia)) {
      k <- key[[a[i]]]
      if (!is.null(k)) res[[i]] <- k
    }
  }
  for (j in which(wb & ib)) {
    for (i in which(ia & grepl(glob_to_regex(b[j]), a))) res[[i]] <- c(res[[i]], j)
  }
  for (i in which(wa & ia)) {
    res[[i]] <- unique(c(res[[i]], which(ib & grepl(glob_to_regex(a[i]), b))))
  }
  res
}

# ---- Parsing and walking ------------------------------------------------------

read_exprs <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (length(lines)) lines[1] <- sub("^\xef\xbb\xbf", "", lines[1], useBytes = TRUE)
  bad <- is.na(iconv(lines, "UTF-8", "UTF-8"))
  if (any(bad)) lines[bad] <- iconv(lines[bad], "latin1", "UTF-8")
  tryCatch(parse(text = lines, keep.source = TRUE, srcfile = srcfilecopy(path, lines)),
           error = function(e) e)
}

# Walks one parsed script. Records every assignment to a name, every named
# function definition, and every call to a function in `interest`, each with
# the line of its statement, the enclosing named function, whether it sits
# inside an if() branch, and the arguments of the functions around it.
walk_script <- function(exprs, interest) {
  A <- collector(); D <- collector(); C <- collector()
  wanted <- new.env(hash = TRUE, parent = emptyenv())
  for (f in interest) assign(f, TRUE, envir = wanted)

  # The function expressions a right-hand side can evaluate to, so that
  # name <- if (cond) function(f) ... else function(f) ... defines `name`
  # just as name <- function(f) ... does.
  fun_branches <- function(e) {
    if (!is.call(e)) return(list())
    h <- call_name(e[[1]])
    if (identical(h, "function")) return(list(e))
    if (identical(h, "{") && length(e) > 1) return(fun_branches(e[[length(e)]]))
    if (h %in% c("(", "local") && length(e) >= 2) return(fun_branches(e[[2]]))
    if (identical(h, "if") && length(e) >= 3)
      return(c(fun_branches(e[[3]]), if (length(e) >= 4) fun_branches(e[[4]])))
    list()
  }
  formal_names <- function(f) {
    nm <- names(f[[2]])
    if (is.null(nm)) character() else nm
  }

  # `locals` holds the arguments of the functions enclosing e. Their values
  # come from the callers, so they are never looked up among assignments.
  walk <- function(e, line, fn, cond, locals) {
    if (!is.call(e)) return(invisible(NULL))
    fname <- call_name(e[[1]])
    n <- length(e)

    if (fname %in% c("<-", "<<-", "=") && n == 3) {
      if (is.symbol(e[[2]]) || is.character(e[[2]])) {
        nm <- as.character(e[[2]])
        fb <- fun_branches(e[[3]])
        if (length(fb)) {
          for (f in fb) {
            sr <- if (length(f) >= 4) f[[4]] else NULL
            l1 <- if (inherits(sr, "srcref")) as.integer(sr)[1] else line
            l2 <- if (inherits(sr, "srcref")) as.integer(sr)[3] else line
            D$add(list(name = nm, fun = f, line1 = l1, line2 = l2))
            walk(f[[3]], l1, nm, FALSE, formal_names(f))
          }
          return(invisible(NULL))
        }
        A$add(list(name = nm, value = e[[3]], line = line, fn = fn))
      }
      walk(e[[3]], line, fn, cond, locals)
      return(invisible(NULL))
    }
    if (fname == "function") {
      sr <- if (n >= 4) e[[4]] else NULL
      walk(e[[3]], if (inherits(sr, "srcref")) as.integer(sr)[1] else line, fn, cond,
           union(locals, formal_names(e)))
      return(invisible(NULL))
    }
    if (fname == "{") {
      sr <- attr(e, "srcref")
      for (k in seq_len(n)[-1]) {
        l <- if (!is.null(sr) && length(sr) >= k) as.integer(sr[[k]])[1] else line
        walk(e[[k]], l, fn, cond, locals)
      }
      return(invisible(NULL))
    }
    if (fname == "for" && n == 4 && is.symbol(e[[2]])) {
      A$add(list(name = as.character(e[[2]]), value = e[[3]], line = line, fn = fn))
    }
    if (fname == "if") {
      walk(e[[2]], line, fn, cond, locals)
      for (k in seq_len(n)[-(1:2)]) walk(e[[k]], line, fn, TRUE, locals)
      return(invisible(NULL))
    }
    if (fname %in% c("%>%", "%T>%", "%<>%") && n == 3 && is.call(e[[3]])) {
      walk(e[[2]], line, fn, cond, locals)
      rhs <- e[[3]]
      has_dot <- any(vapply(as.list(rhs)[-1],
                            function(a) is.symbol(a) && identical(as.character(a), "."),
                            logical(1)))
      if (!has_dot) rhs <- as.call(append(as.list(rhs), list(as.name(".PIPE_LHS")), after = 1))
      walk(rhs, line, fn, cond, locals)
      return(invisible(NULL))
    }
    if (nzchar(fname) && exists(fname, envir = wanted, inherits = FALSE))
      C$add(list(fname = fname, call = e, line = line, fn = fn, cond = cond, locals = locals))
    for (k in seq_len(n)[-1]) walk(e[[k]], line, fn, cond, locals)
    invisible(NULL)
  }

  sr <- attr(exprs, "srcref")
  for (i in seq_along(exprs)) {
    l <- if (!is.null(sr)) as.integer(sr[[i]])[1] else NA_integer_
    walk(exprs[[i]], l, "", FALSE, character())
  }
  list(assign = A$get(), defs = D$get(), calls = C$get())
}

# ---- Resolving arguments and names ----------------------------------------------

match_target <- function(call, formals_str, target_str) {
  fmls <- strsplit(formals_str, ",", fixed = TRUE)[[1]]
  tgts <- strsplit(target_str, "|", fixed = TRUE)[[1]]
  args <- as.list(call)[-1]
  if (!length(args)) return(NULL)
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  pick <- function(j) if (is_empty_arg(args[[j]])) NULL else args[[j]]

  hit <- which(nms %in% tgts)
  if (length(hit)) return(pick(hit[1]))
  if (identical(tgts, "...")) {
    un <- which(nms == "")
    return(if (length(un)) pick(un[1]) else NULL)
  }
  fixed <- if ("..." %in% fmls) fmls[seq_len(match("...", fmls) - 1)] else fmls
  resolved <- vapply(nms, function(a) {
    if (!nzchar(a)) return("")
    if (a %in% fixed) return(a)
    pm <- fixed[startsWith(fixed, a)]
    if (length(pm) == 1) pm else ""
  }, character(1))
  hit <- which(resolved %in% tgts)
  if (length(hit)) return(pick(hit[1]))
  free <- setdiff(fixed, resolved)
  un <- which(nms == "")
  for (i in seq_along(un)) {
    if (i > length(free)) break
    if (free[i] %in% tgts) return(pick(un[i]))
  }
  NULL
}

# Global index of assignments: by name, with script, line and enclosing function.
build_index <- function(walks) {
  a_all <- unlist(lapply(walks, `[[`, "assign"), recursive = FALSE)
  scr <- rep(names(walks), vapply(walks, function(w) length(w$assign), integer(1)))
  tab <- data.frame(name = vapply(a_all, `[[`, "", "name"),
                    script = scr,
                    line = vapply(a_all, function(a) as.numeric(a$line), numeric(1)),
                    fn = vapply(a_all, `[[`, "", "fn"),
                    k = seq_along(a_all), stringsAsFactors = FALSE)
  tab$line[is.na(tab$line)] <- 0
  by_name <- lapply(split(seq_len(nrow(tab)), tab$name), function(ix)
    list(script = tab$script[ix], line = tab$line[ix], fn = tab$fn[ix], k = tab$k[ix]))
  list(by_name = by_name, values = lapply(a_all, `[[`, "value"))
}

# Finds the assignment a name refers to from a given script, line and function:
# the same function first, then the same script, then the top level of the
# scripts this one sources, then the top level of any script.
lookup_assign <- function(nm, ctx) {
  if (length(ctx$locals) && nm %in% ctx$locals) return(NULL)
  cand <- ctx$idx$by_name[[nm]]
  if (is.null(cand)) return(NULL)
  at <- if (is.na(ctx$line)) Inf else ctx$line
  pick <- function(ix) {
    if (!length(ix)) return(NA_integer_)
    before <- ix[cand$line[ix] <= at]
    if (length(before)) before[which.max(cand$line[before])] else ix[which.min(cand$line[ix])]
  }
  same <- which(cand$script == ctx$script)
  j <- pick(same[cand$fn[same] == ctx$fn])
  if (is.na(j)) j <- pick(same[cand$fn[same] == ""])
  if (is.na(j)) {
    other <- which(cand$script != ctx$script & cand$fn == "")
    if (length(other)) {
      pref <- other[cand$script[other] %in% ctx$sourced]
      j <- if (length(pref)) pref[1] else other[1]
    }
  }
  if (is.na(j)) return(NULL)
  ctx2 <- ctx
  ctx2$script <- cand$script[j]; ctx2$line <- cand$line[j] - 0.5; ctx2$fn <- cand$fn[j]
  same_scope <- identical(cand$script[j], ctx$script) && identical(cand$fn[j], ctx$fn)
  ctx2$locals <- if (same_scope) ctx$locals else fn_formals(cand$fn[j], cand$script[j])
  list(value = ctx$idx$values[[cand$k[j]]], ctx = ctx2)
}

# The arguments of a named function, as defined in a given script.
fn_formals <- function(fn, script) {
  if (!nzchar(fn)) return(character())
  d <- find_def(fn, script)
  nm <- if (is.null(d)) NULL else names(d$fun[[2]])
  if (is.null(nm)) character() else nm
}

# File names written literally among a call's arguments.
literal_files <- function(args) {
  v <- unlist(lapply(args, function(a) if (is.character(a)) a else NULL))
  v <- v[!is.na(v) & grepl("\\.[A-Za-z0-9]{1,5}$", v)]
  if (length(v)) unique(v) else character()
}

# TRUE when no alternative fixes anything in the file name itself.
name_unknown <- function(p) {
  p <- p[!is.na(p)]
  !length(p) || all(!nzchar(gsub("*", "", sub(".*[/\\\\]", "", p), fixed = TRUE)))
}

# Turns a path expression into file-name patterns: literals kept, anything
# computed at run time replaced by *. Returns the alternatives as a vector,
# so a name built inside for (x in c("a", "b")) gives both names.
max_alts <- 50L

combine_parts <- function(parts, sep) {
  parts <- lapply(parts, function(p) if (length(p) > max_alts) "*" else p)
  out <- Reduce(function(a, b) as.vector(outer(a, b, paste, sep = sep)), parts)
  if (length(out) > max_alts) "*" else unique(out)
}

pattern_of <- function(x, ctx, depth = 5) {
  if (is.null(x) || is_empty_arg(x)) return(NA_character_)
  if (is.character(x)) return(if (length(x)) unique(x) else NA_character_)
  if (is.numeric(x) || is.logical(x)) return(as.character(x[1]))
  if (is.symbol(x)) {
    if (depth <= 0) return("*")
    r <- lookup_assign(as.character(x), ctx)
    if (is.null(r)) return("*")
    return(pattern_of(r$value, r$ctx, depth - 1))
  }
  if (!is.call(x)) return("*")
  fn <- call_name(x[[1]])
  args <- as.list(x)[-1]
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  part <- function(i) {
    p <- pattern_of(args[[i]], ctx, depth)
    p <- p[!is.na(p)]
    if (length(p)) p else "*"
  }
  parts_except <- function(drop) lapply(which(!(nms %in% drop)), part)
  if (fn %in% c("file.path", "here", "path")) {
    pr <- parts_except(c("fsep", "ext"))
    return(if (length(pr)) combine_parts(pr, "/") else "*")
  }
  if (fn == "paste0") {
    pr <- parts_except("collapse")
    return(if (length(pr)) combine_parts(pr, "") else "*")
  }
  if (fn == "paste") {
    sep <- if ("sep" %in% nms) part(which(nms == "sep")[1])[1] else " "
    pr <- parts_except(c("sep", "collapse"))
    return(if (length(pr)) combine_parts(pr, sep) else "*")
  }
  if (fn %in% c("sprintf", "gettextf") && length(args)) {
    rx <- "%[-+ 0#]*[0-9*]*(\\.[0-9*]+)?[a-zA-Z]"
    fmt <- part(1)
    if (length(fmt) == 1) {
      f <- gsub("%%", "\001", fmt, fixed = TRUE)
      m <- gregexpr(rx, f)[[1]]
      n_spec <- if (m[1] == -1) 0L else length(m)
      if (n_spec > 0 && n_spec == length(args) - 1) {
        pieces <- regmatches(f, list(m), invert = TRUE)[[1]]
        specs <- regmatches(f, list(m))[[1]]
        pr <- list(pieces[1])
        for (i in seq_len(n_spec)) {
          a <- if (grepl("s$", specs[i])) part(i + 1) else "*"
          pr <- c(pr, list(a), list(pieces[i + 1]))
        }
        return(unique(gsub("\001", "%", combine_parts(pr, ""), fixed = TRUE)))
      }
    }
    f <- gsub("%%", "\001", fmt, fixed = TRUE)
    return(unique(gsub("\001", "%", gsub(rx, "*", f), fixed = TRUE)))
  }
  if (fn %in% c("glue", "str_glue") && length(args)) return(unique(gsub("\\{[^}]*\\}", "*", part(1))))
  if (fn %in% c("stdout", "stderr", "stdin", "textConnection", "rawConnection", "pipe", "tempfile"))
    return(NA_character_)
  if (fn %in% c("file", "gzfile", "bzfile", "xzfile", "normalizePath", "path.expand",
                "basename", "as.character", "enc2utf8", "enc2native", "trimws",
                "tolower", "toupper", "I", "path_norm", "path_abs", "invisible",
                "return", "identity", "suppressWarnings", "suppressMessages") && length(args))
    return(part(1))
  if (fn == "if" && length(args) >= 2) {
    return(unique(c(part(2), if (length(args) >= 3) part(3))))
  }
  if (fn == "ifelse" && length(args) >= 3) return(unique(c(part(2), part(3))))
  if (fn == "c" && length(args)) {
    out <- unique(unlist(parts_except(character())))
    return(if (length(out) > max_alts) "*" else out)
  }
  if (fn == "switch" && length(args) >= 2) {
    key <- part(1)
    alt <- nms[-1]
    if (length(key) == 1 && key %in% alt[nzchar(alt)]) return(part(1 + match(key, alt)))
    out <- unique(unlist(lapply(seq_along(args)[-1], part)))
    return(if (length(out) > max_alts) "*" else out)
  }
  if (fn %in% c("file_path_sans_ext", "path_ext_remove") && length(args))
    return(if (is.character(args[[1]])) tools::file_path_sans_ext(args[[1]]) else "*")
  # A date format used as a file-name template, as in
  # format(Sys.time(), "run_%Y%m%d.log"): its codes vary, the rest is fixed.
  if (fn %in% c("format", "strftime", "format.Date", "format.POSIXct") && length(args)) {
    lit <- unlist(lapply(args, function(a) if (is.character(a)) a else NULL))
    lit <- lit[grepl("%", lit, fixed = TRUE)]
    return(if (length(lit)) unique(gsub("%[A-Za-z]", "*", lit)) else "*")
  }
  res <- NULL
  d <- find_def(fn, ctx$script)
  if (!is.null(d) && depth > 0) {
    body <- d$fun[[3]]
    last <- if (is.call(body) && identical(call_name(body[[1]]), "{") && length(body) > 1)
      body[[length(body)]] else body
    if (!is.null(last) && !is_empty_arg(last)) {
      last <- resolve_in(last, d$script, d$name, Inf, names(d$fun[[2]]), ctx$idx)
      ex <- bind_call(x, d$fun, last)
      if (!is.null(ex)) res <- pattern_of(ex, ctx, depth - 1)
    }
  }
  # A helper of the project's own that could not be followed still names its
  # file when the name is written out among its arguments, as in
  # .out_path("results.csv"). Functions from packages are not guessed at.
  if (!is.null(d) && (is.null(res) || name_unknown(res))) {
    lit <- literal_files(args)
    if (length(lit)) return(lit)
  }
  if (is.null(res)) "*" else res
}

# Substitutes a call's arguments, and the defaults it leaves out, into an
# expression written in terms of the called function's formal arguments.
bind_call <- function(call, fun_expr, expr) {
  clo <- tryCatch(eval(fun_expr, baseenv()), error = function(e) NULL)
  if (!is.function(clo)) return(NULL)
  mc <- tryCatch(match.call(clo, call), error = function(e) NULL)
  if (is.null(mc)) return(NULL)
  args <- as.list(mc)[-1]
  fm <- formals(clo)
  env <- list()
  named <- setdiff(names(fm), "...")
  for (a in named) {
    if (a %in% names(args)) {
      if (!is_empty_arg(args[[a]])) env[[a]] <- args[[a]]
    } else if (!is_empty_arg(fm[[a]])) {
      env[[a]] <- fm[[a]]
    }
  }
  out <- do.call(substitute, list(expr, env))
  if ("..." %in% names(fm)) {
    an <- names(args)
    if (is.null(an)) an <- rep("", length(args))
    out <- splice_dots(out, args[!(an %in% named)])
  }
  out
}

# Replaces a `...` argument inside expr with the arguments a call passed
# through the dots, so file.path(dir, ...) keeps the file name it was given.
splice_dots <- function(x, dots) {
  if (!is.call(x)) return(x)
  parts <- as.list(x)
  keep <- parts[1]
  for (k in seq_along(parts)[-1]) {
    if (is.symbol(parts[[k]]) && identical(as.character(parts[[k]]), "...")) {
      keep <- c(keep, dots)
    } else if (is.call(parts[[k]])) {
      piece <- parts[k]
      piece[[1]] <- splice_dots(parts[[k]], dots)
      keep <- c(keep, piece)
    } else {
      keep <- c(keep, parts[k])
    }
  }
  as.call(keep)
}

find_def <- function(name, script) {
  cand <- def_index[[name]]
  if (is.null(cand)) return(NULL)
  own <- Filter(function(d) d$script == script, cand)
  if (length(own)) own[[1]] else cand[[1]]
}

finish_pattern <- function(p) {
  p <- p[!is.na(p)]
  if (!length(p)) return(NA_character_)
  out <- gsub("\\*+", "*", sub(".*[/\\\\]", "", p))
  out <- unique(out[nzchar(out)])
  if (!length(out)) NA_character_ else paste(out, collapse = "|")
}

dir_of <- function(x, ctx, depth = 2) {
  if (is.symbol(x) && !is_empty_arg(x) && depth > 0) {
    r <- lookup_assign(as.character(x), ctx)
    return(if (is.null(r)) "" else dir_of(r$value, r$ctx, depth - 1))
  }
  if (!is.call(x)) return("")
  fn <- call_name(x[[1]])
  if (fn %in% c("file.path", "here", "path") && length(x) >= 3) {
    a <- as.list(x)[-c(1, length(x))]
    return(gsub('"', "", paste(vapply(a, one_line, character(1)), collapse = "/"), fixed = TRUE))
  }
  if (fn %in% c("file", "gzfile", "normalizePath", "path.expand") && length(x) >= 2)
    return(dir_of(x[[2]], ctx, depth))
  ""
}

# Replaces names assigned inside a function body with their values, stopping
# at the function's own arguments, so a wrapper's path is expressed in terms
# of what its callers pass.
resolve_in <- function(x, script, fn, line, stop, idx, depth = 4) {
  if (is.null(x) || depth <= 0) return(x)
  if (is.symbol(x)) {
    if (is_empty_arg(x)) return(x)
    nm <- as.character(x)
    if (nm %in% stop) return(x)
    cand <- idx$by_name[[nm]]
    if (is.null(cand)) return(x)
    at <- if (is.na(line)) Inf else line
    ix <- which(cand$script == script & cand$fn == fn & cand$line <= at)
    if (!length(ix)) return(x)
    j <- ix[which.max(cand$line[ix])]
    return(resolve_in(idx$values[[cand$k[j]]], script, fn, cand$line[j] - 0.5, stop, idx, depth - 1))
  }
  if (is.call(x)) {
    for (i in seq_along(x)[-1]) {
      if (is_empty_arg(x[[i]])) next
      xi <- resolve_in(x[[i]], script, fn, line, stop, idx, depth)
      if (!is.null(xi)) x[[i]] <- xi
    }
  }
  x
}

expand_helper <- function(call, h) {
  out <- lapply(h$templates, function(tp) {
    ex <- bind_call(call, h$fun, tp$expr)
    if (is.null(ex)) NULL else list(role = tp$role, fun = tp$fun, expr = ex)
  })
  Filter(Negate(is.null), out)
}

registry_targets <- function(rec) {
  r <- registry[registry$fun == rec$fname, ][1, ]
  out <- list()
  t <- match_target(rec$call, r$formals, r$target)
  if (!is.null(t)) out[[1]] <- list(role = r$role, fun = r$fun, expr = t)
  if (r$role == "copy") {
    from <- match_target(rec$call, r$formals, strsplit(r$formals, ",", fixed = TRUE)[[1]][1])
    if (!is.null(from)) out[[length(out) + 1]] <- list(role = "read", fun = r$fun, expr = from)
  }
  if (r$role == "write" && r$fun %in% c("modelsummary", "datasummary") && length(out)) {
    lit <- out[[1]]$expr
    if (is.character(lit) && !grepl("\\.[A-Za-z0-9]+$", lit)) out <- list()
  }
  out
}

choose_helper <- function(name, script, helpers) {
  keys <- helper_keys[[name]]
  if (!length(keys)) return(NULL)
  own <- keys[startsWith(keys, paste0(script, "::"))]
  helpers[[if (length(own)) own[1] else keys[1]]]
}

# ---- Scan ---------------------------------------------------------------------

scripts <- list.files(cfg$root, pattern = "\\.[Rr]$", full.names = TRUE)
scripts <- scripts[!basename(scripts) %in% cfg$exclude]
parsed <- setNames(lapply(scripts, read_exprs), basename(scripts))
failed <- names(parsed)[vapply(parsed, function(p) inherits(p, "error"), logical(1))]
ok <- setdiff(names(parsed), failed)

# The first walk finds the user-defined functions; the second records calls
# to them as well as to the file functions.
walk_all <- function(interest) setNames(lapply(ok, function(s) {
  tryCatch(walk_script(parsed[[s]], interest),
           error = function(e) list(assign = list(), defs = list(), calls = list(),
                                    error = conditionMessage(e)))
}), ok)
walks <- walk_all(registry$fun)
def_names <- unique(unlist(lapply(walks, function(w) vapply(w$defs, `[[`, "", "name"))))
walks <- walk_all(union(registry$fun, def_names))
walk_failed <- names(walks)[vapply(walks, function(w) !is.null(w$error), logical(1))]
idx <- build_index(walks)
def_index <- list()
for (s in ok) for (d in walks[[s]]$defs) {
  def_index[[d$name]] <- c(def_index[[d$name]], list(list(script = s, name = d$name, fun = d$fun)))
}
calls_by_fn <- lapply(walks, function(w) {
  if (!length(w$calls)) return(list())
  split(w$calls, vapply(w$calls, `[[`, "", "fn"))
})

# Which scripts source which. Resolved first, because name lookup prefers the
# scripts a script sources.
edges <- list()
for (s in ok) for (rec in walks[[s]]$calls) {
  if (!rec$fname %in% c("source", "sys.source")) next
  t <- match_target(rec$call, "file", "file")
  if (is.null(t)) next
  ctx <- list(script = s, line = rec$line, fn = rec$fn, idx = idx, sourced = character())
  p <- finish_pattern(pattern_of(t, ctx))
  if (is.na(p)) next
  hit <- ok[unique(unlist(match_index(strsplit(p, "|", fixed = TRUE)[[1]], ok)))]
  if (length(hit)) edges[[length(edges) + 1]] <- data.frame(
    from = s, to = hit, cond = rec$cond, stringsAsFactors = FALSE)
}
edges <- bind_rows(edges)
reach <- function(s, uncond_only = FALSE) {
  if (!nrow(edges)) return(character())
  e <- if (uncond_only) edges[!edges$cond, ] else edges
  seen <- character(); todo <- e$to[e$from == s]
  while (length(todo)) {
    x <- todo[1]; todo <- todo[-1]
    if (x %in% seen || x == s) next
    seen <- c(seen, x); todo <- c(todo, e$to[e$from == x])
  }
  seen
}
sourced <- setNames(lapply(ok, reach), ok)
sourced_uncond <- setNames(lapply(ok, reach, uncond_only = TRUE), ok)

# Wrapper functions: any named function whose body writes, reads, copies or
# sources a file, directly or through another wrapper.
helpers <- list()
helper_keys <- list()
for (round in 1:5) {
  changed <- FALSE
  for (s in ok) for (d in walks[[s]]$defs) {
    recs <- calls_by_fn[[s]][[d$name]]
    if (!length(recs)) next
    stop_at <- names(d$fun[[2]])
    tps <- list()
    for (rec in recs) {
      found <- if (rec$fname %in% registry$fun) registry_targets(rec) else {
        h <- if (rec$fname != d$name) choose_helper(rec$fname, s, helpers) else NULL
        if (is.null(h)) list() else expand_helper(rec$call, h)
      }
      for (f in found) {
        f$expr <- resolve_in(f$expr, s, d$name, rec$line, stop_at, idx)
        tps[[length(tps) + 1]] <- f
      }
    }
    if (length(tps)) {
      key <- paste0(s, "::", d$name)
      if (!identical(helpers[[key]]$templates, tps)) {
        helpers[[key]] <- list(script = s, name = d$name, fun = d$fun, templates = tps)
        helper_keys[[d$name]] <- union(helper_keys[[d$name]], key)
        changed <- TRUE
      }
    }
  }
  if (!changed) break
}

R <- collector()
for (s in ok) for (rec in walks[[s]]$calls) {
  ctx <- list(script = s, line = rec$line, fn = rec$fn, idx = idx, sourced = sourced[[s]],
              locals = rec$locals)
  if (rec$fname %in% registry$fun) {
    found <- registry_targets(rec); via <- ""
  } else {
    h <- if (!identical(rec$fname, rec$fn)) choose_helper(rec$fname, s, helpers) else NULL
    if (is.null(h)) next
    found <- expand_helper(rec$call, h)
    via <- if (h$script == s) h$name else paste0(h$name, " (", h$script, ")")
  }
  for (f in found) {
    p <- finish_pattern(pattern_of(f$expr, ctx))
    if (is.na(p)) next
    R$add(list(script = s, line = rec$line, role = f$role, fun = f$fun, via = via,
               in_function = rec$fn, conditional = rec$cond, pattern = p,
               dir_hint = dir_of(f$expr, ctx), target = one_line(f$expr)))
  }
}
io <- bind_rows(lapply(R$get(), as.data.frame, stringsAsFactors = FALSE))
if (!nrow(io)) stop("No file calls found. Is cfg$root the project folder?")
# Calls made inside an archive function, directly or through it, keep a copy
# for the record. They stay listed but do not make a script a file's writer.
io$archive <- io$in_function %in% cfg$archive_functions |
  sub(" \\(.*$", "", io$via) %in% cfg$archive_functions

# ---- Summaries ------------------------------------------------------------------

# One row per file: a name built in a loop over c("a", "b") gives two rows.
split_alts <- function(d) {
  if (!nrow(d)) return(d)
  parts <- strsplit(d$pattern, "|", fixed = TRUE)
  d <- d[rep(seq_len(nrow(d)), lengths(parts)), , drop = FALSE]
  d$pattern <- unlist(parts)
  d
}
writes <- io %>% filter(role %in% c("write", "copy")) %>% split_alts() %>% filter(pattern != "*")
reads  <- io %>% filter(role == "read") %>% split_alts() %>% filter(pattern != "*")

sourced_by <- function(s) {
  who <- names(sourced)[vapply(sourced, function(v) s %in% v, logical(1))]
  if (!length(who)) return("")
  gated <- !vapply(who, function(w) s %in% sourced_uncond[[w]], logical(1))
  paste0(who, ifelse(gated, " (conditional)", ""), collapse = "; ")
}

scan <- writes %>%
  group_by(script, output = pattern) %>%
  summarise(role = paste(sort(unique(role)), collapse = ";"),
            dir_hint = paste(setdiff(unique(dir_hint), ""), collapse = ";"),
            lines = paste(sort(unique(line)), collapse = ";"),
            via = paste(setdiff(unique(via), ""), collapse = ";"),
            conditional = all(conditional),
            archive = all(archive),
            .groups = "drop") %>%
  arrange(script, output)
hits <- match_index(scan$output, reads$pattern)
scan$read_by <- vapply(seq_len(nrow(scan)), function(i) {
  who <- setdiff(unique(reads$script[hits[[i]]]), scan$script[i])
  paste(sort(who), collapse = "; ")
}, character(1))
scan$sourced_by <- vapply(scan$script, sourced_by, character(1))
scan_live <- scan[!scan$archive, ]

dir.create(cfg$out_dir, recursive = TRUE, showWarnings = FALSE)
io_out <- io %>% arrange(script, line)
if (length(failed) || length(walk_failed)) {
  io_out <- bind_rows(io_out, data.frame(
    script = c(failed, walk_failed), line = NA_integer_, role = "not scanned",
    fun = "", via = "", in_function = "", conditional = NA, pattern = "", dir_hint = "",
    target = c(vapply(parsed[failed], conditionMessage, ""),
               vapply(walks[walk_failed], `[[`, "", "error")),
    stringsAsFactors = FALSE))
}
write.csv(io_out, file.path(cfg$out_dir, "script_io_calls.csv"), row.names = FALSE)
write.csv(scan, file.path(cfg$out_dir, "script_output_scan.csv"), row.names = FALSE)

# ---- The curated map against the scan --------------------------------------------

map_counts <- NULL
cm <- NULL
if (file.exists(cfg$curated_map)) {
  cm <- read.csv(cfg$curated_map, stringsAsFactors = FALSE, check.names = FALSE)
  check <- cm[, intersect(c("script", "output", "note"), names(cm))]
  check$is_file <- grepl("^[A-Za-z0-9_.*-]+\\.[A-Za-z0-9]{1,5}$", check$output)
  map_hits <- match_index(check$output, scan_live$output)
  verdict <- lapply(seq_len(nrow(check)), function(i) {
    s <- check$script[i]
    named <- nzchar(s) && !startsWith(s, "(")
    if (named && s %in% failed) return(c("script did not parse", ""))
    if (named && !s %in% ok) return(c("script not in folder", ""))
    if (!check$is_file[i]) return(c("not checked", "output is not a file name"))
    w <- unique(scan_live$script[map_hits[[i]]])
    if (!named) {
      return(if (length(w)) c("written by another script", paste(w, collapse = "; "))
             else c("no script writes it", ""))
    }
    if (s %in% w) return(c("confirmed", ""))
    via <- intersect(w, sourced[[s]])
    if (length(via)) return(c("confirmed via a sourced script", paste(via, collapse = "; ")))
    if (length(w)) return(c("written by another script", paste(w, collapse = "; ")))
    # A name built at run time shows in the scan only as a wildcard, such as
    # *.pdf, which is too loose to match. Accept it when the script has one.
    ext <- tools::file_ext(check$output[i])
    wild <- scan_live$output[scan_live$script %in% c(s, sourced[[s]]) &
                             grepl("*", scan_live$output, fixed = TRUE)]
    wild <- unique(wild[tools::file_ext(wild) == ext])
    if (nzchar(ext) && length(wild))
      return(c("run-time name", paste("the script writes", paste(wild, collapse = "; "))))
    c("not found in code", "")
  })
  check$scan <- vapply(verdict, `[`, "", 1)
  check$scan_detail <- vapply(verdict, `[`, "", 2)
  check <- select(check, -is_file)
  if (isTRUE(cfg$map_is_complete)) {
    back <- match_index(scan_live$output, check$output)
    in_map <- vapply(seq_len(nrow(scan_live)), function(i)
      any(check$script[back[[i]]] == scan_live$script[i]), logical(1))
    extra <- scan_live[!in_map & !grepl("*", scan_live$output, fixed = TRUE), ] %>%
      transmute(script, output, scan = "in code, not in map", scan_detail = paste("lines", lines))
    check <- bind_rows(check, extra)
  }
  check <- check %>% arrange(script, output)
  write.csv(check, file.path(cfg$out_dir, "script_map_check.csv"), row.names = FALSE)
  map_counts <- table(check$scan)
}

# ---- Files on disk ----------------------------------------------------------------

data_ext <- "\\.(rds|rda|rdata|csv|tsv|txt|tex|pdf|png|svg|xlsx|xls|log|dta|parquet|qs|json)$"
disk <- unlist(lapply(cfg$disk_dirs[dir.exists(cfg$disk_dirs)], list.files,
                      recursive = TRUE, full.names = TRUE))
top <- list.files(cfg$root, full.names = TRUE)
top <- top[!dir.exists(top) & grepl(data_ext, top, ignore.case = TRUE) &
             basename(top) != basename(cfg$curated_map)]
disk <- unique(c(disk, top))
norm <- function(x) normalizePath(x, winslash = "/", mustWork = FALSE)
disk <- disk[!startsWith(norm(disk), norm(cfg$out_dir))]
n_orphan <- 0L
if (length(disk)) {
  fi <- file.info(disk)
  files <- data.frame(path = disk, file = basename(disk), size_kb = round(fi$size / 1024, 1),
                      modified = fi$mtime, stringsAsFactors = FALSE)
  script_time <- setNames(file.info(file.path(cfg$root, ok))$mtime, ok)
  # Writers: the code, less archive copies, plus the curated map, which
  # supplies the names the code builds at run time.
  wtab <- data.frame(script = scan_live$script, output = scan_live$output,
                     from_map = FALSE, stringsAsFactors = FALSE)
  if (!is.null(cm)) {
    add <- cm[cm$script %in% ok &
              grepl("^[A-Za-z0-9_.*-]+\\.[A-Za-z0-9]{1,5}$", cm$output), c("script", "output")]
    if (nrow(add)) wtab <- rbind(wtab, data.frame(add, from_map = TRUE, stringsAsFactors = FALSE))
  }
  w_idx <- match_index(files$file, wtab$output)
  r_idx <- match_index(files$file, reads$pattern)
  files$writers <- vapply(w_idx, function(j) paste(sort(unique(wtab$script[j])), collapse = "; "),
                          character(1))
  files$writers_from_map <- vapply(w_idx, function(j) {
    m <- setdiff(wtab$script[j][wtab$from_map[j]], wtab$script[j][!wtab$from_map[j]])
    paste(sort(unique(m)), collapse = "; ")
  }, character(1))
  writer_list <- strsplit(files$writers, "; ", fixed = TRUE)
  parts <- strsplit(gsub("\\\\", "/", files$path), "/", fixed = TRUE)
  files$archive <- vapply(parts, function(p) any(head(p, -1) %in% cfg$archive_dirs), logical(1))
  # For each script, the files on disk that its read calls point to.
  reads_files <- split(rep(seq_along(r_idx), lengths(r_idx)), reads$script[unlist(r_idx)])
  files$edited_after <- vapply(seq_len(nrow(files)), function(i) {
    if (files$archive[i]) return("")
    w <- writer_list[[i]]
    w <- w[nzchar(w) & w %in% names(script_time)]
    paste(w[script_time[w] > files$modified[i]], collapse = "; ")
  }, character(1))
  files$newer_input <- vapply(seq_len(nrow(files)), function(i) {
    if (files$archive[i]) return("")
    w <- writer_list[[i]]
    w <- w[nzchar(w)]
    inp <- setdiff(unique(unlist(reads_files[intersect(w, names(reads_files))])), i)
    # A script's own outputs are not its inputs here. A script that appends
    # to a ledger it keeps, or reads back a fragment it wrote a moment
    # earlier, would otherwise look older than itself.
    if (length(inp)) inp <- inp[!vapply(inp, function(j) any(writer_list[[j]] %in% w), logical(1))]
    if (!length(inp)) return("")
    inp <- inp[files$modified[inp] > files$modified[i]]
    if (!length(inp)) "" else files$file[inp[which.max(files$modified[inp])]]
  }, character(1))
  files$status <- ifelse(files$archive, "archive",
                  ifelse(!nzchar(files$writers), "no script writes it",
                  ifelse(nzchar(files$newer_input), "older than an input",
                  ifelse(nzchar(files$edited_after), "script edited since", "ok"))))
  files$archive <- NULL
  files <- files %>% arrange(status, path)
  write.csv(files, file.path(cfg$out_dir, "output_files_check.csv"), row.names = FALSE)
  n_orphan <- sum(files$status == "no script writes it")
  status_counts <- table(files$status)
}

# ---- Report -----------------------------------------------------------------------

cat(sprintf("Scanned %d scripts; %d did not parse%s.\n", length(ok), length(failed),
            if (length(failed)) paste0(" (", paste(failed, collapse = ", "), ")") else ""))
cat(sprintf("Found %d file calls and %d script-output pairs.\n", nrow(io), nrow(scan)))
if (!is.null(map_counts)) {
  cat("script_output_map.csv against the code:\n")
  for (k in names(map_counts)) cat(sprintf("  %-32s %d\n", k, map_counts[[k]]))
}
cat(sprintf("Files on disk that no script writes: %d.\n", n_orphan))
if (exists("status_counts")) {
  cat("Files on disk by status:\n")
  for (k in names(status_counts)) cat(sprintf("  %-22s %d\n", k, status_counts[[k]]))
}
# The same output written at more than one line of one script: either
# alternative branches or a later write replacing an earlier one.
multi <- scan_live[grepl(";", scan_live$lines, fixed = TRUE) &
                   !grepl("*", scan_live$output, fixed = TRUE), ]
if (nrow(multi)) {
  cat("Outputs written at more than one line of the same script (check for overwrites):\n")
  for (i in seq_len(nrow(multi)))
    cat(sprintf("  %-32s %-36s lines %s\n", multi$script[i], multi$output[i], multi$lines[i]))
}
cat("Results in", cfg$out_dir, "\n")
