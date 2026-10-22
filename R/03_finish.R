# =====================================================================
# 03_finish.R  -  ledger, decisions, sealed estimates, close
# ---------------------------------------------------------------------
# Sourced as the last line of 03a, 03b and 03c. It replaces the tail of
# 03training_designs.R (lines 5768-5884), and it differs from that tail
# in exactly one respect, which is the respect the split forces.
#
# THE PROBLEM THE SPLIT CREATES. The master reads ONE training_ledger.csv,
# ONE training_decisions.txt and ONE BLIND_training_estimates.rds. Three
# modules now produce rows for all three. If each simply wrote the files,
# the last module to run would overwrite the other two and the master
# would check a third of a ledger against a full reconciliation table,
# reporting the missing two thirds as absent designs rather than as an
# overwrite.
#
# THE OLD FILE ALREADY WARNED ABOUT THIS SHAPE. Its tail carries a block
# on RUN_T8 being FALSE while TD_NO_WRITE is FALSE: "the decisions file
# and the sealed estimates are about to be rewritten WITHOUT any T8 rows
# ... they are being replaced by a subset". Splitting turns that from an
# edge case into the normal path, so it is handled rather than warned
# about.
#
# THE FIX IS THE REGISTRY, which is already the project's answer to
# cross-script state. Each module MERGES its rows into the accumulated
# ledger and writes the merged whole. Rows are keyed by tag and the
# current module's rows win, so re-running 03b alone refreshes its own
# rows and leaves 03a's and 03c's intact.
#
# CLEARING IS EXPLICIT. A publication pass wants a ledger built from one
# consistent run of all three, not a merge across runs of different
# vintages. RUN.R sets TD03_LEDGER_RESET <- TRUE before the first module,
# which drops the accumulated rows so the pass starts clean. Do not make
# this the default: a reset on every source is how running 03b alone
# silently deletes the other two thirds.
# =====================================================================

if (!exists("TD03_MODULE")) TD03_MODULE <- "03_unknown"
if (!exists("TD_NO_WRITE")) TD_NO_WRITE <- FALSE
if (exists(".t_flag_note", mode = "function"))
  .t_flag_note("TD_NO_WRITE", FALSE)

.fin_mod <- tools::file_path_sans_ext(basename(TD03_MODULE))
cat("\n=====================================================================\n")
cat("LEDGER AND DECISIONS  |  ", .fin_mod, "\n", sep = "")
cat("=====================================================================\n")

# ---- this module's rows ---------------------------------------------
.fin_led <- if (length(.ledger)) bind_rows(.ledger) else NULL
# v28. FILL, DO NOT OVERWRITE. report() now stamps each row with
# TD03_LEDGER_MODULE at the moment it is built, so a row that already
# names its producer keeps that name and only unowned rows inherit the
# module that happens to be finishing. The old unconditional assignment
# is how thirty of 04financing_space.R's rows came to be labelled
# 03a_design_audit, and it would have done it again on the next
# hand-sourced run of this file.
if (!is.null(.fin_led) && nrow(.fin_led)) {
  if (!"module" %in% names(.fin_led)) .fin_led$module <- NA_character_
  .fin_led$module[is.na(.fin_led$module) | !nzchar(.fin_led$module)] <- .fin_mod
  .fin_foreign <- setdiff(unique(.fin_led$module), .fin_mod)
  if (length(.fin_foreign))
    cat("  rows from another script in this accumulator, kept under their\n",
        "  own name: ", paste(.fin_foreign, collapse = ", "), "\n", sep = "")
}

if (isTRUE(get0("TD03_LEDGER_RESET", ifnotfound = FALSE))) {
  cat("  TD03_LEDGER_RESET is TRUE: starting a clean accumulation.\n")
  for (.k in c("ledger", "ledger_blind", "ledger_decisions"))
    unlink(file.path(TD_DIR, paste0(.k, ".rds")))
  rm(.k)
  # Once, not once per module: the second module of a pass must not wipe
  # the first. RUN.R sets it, this line unsets it.
  TD03_LEDGER_RESET <- FALSE
}

# ---- an untagged row cannot be de-duplicated, so it cannot be carried ----
#
# THE CARRY-FORWARD BELOW EXCLUDES BY TAG, and NA %in% anything is FALSE, so
# a row with no tag survives every exclusion and is re-bound on every pass.
# It is a pure accumulator: 30 untagged T10 rows became 60, then 90, and the
# shared ledger went 70 -> 100 across two passes that ran the same three
# stages. Nothing noticed, because the rows look like data, carry a label, a
# standard error and a status, and only the tag and n are missing.
#
# DROPPING THEM IS THE ONLY SAFE OPTION. Keeping them doubles them; keeping
# the first occurrence needs an identity they do not have. So they are
# dropped and COUNTED, because sixty rows leaving silently is the same
# failure in the other direction, and the count is what points at the block
# that wrote them without a tag.
.fin_drop_untagged <- function(x, where) {
  if (is.null(x) || !nrow(x)) return(x)
  if (!"tag" %in% names(x)) {
    cat("  *** ", where, " has no tag column at all; dropping all ", nrow(x),
        " rows.\n", sep = "")
    return(x[0, , drop = FALSE])
  }
  .bad <- is.na(x$tag) | !nzchar(trimws(as.character(x$tag)))
  if (any(.bad)) {
    cat("  *** ", sum(.bad), " untagged row(s) dropped from ", where, ".\n",
        sep = "")
    if ("label" %in% names(x))
      cat("      first: ", substr(as.character(x$label[which(.bad)[1]]), 1, 58),
          "\n", sep = "")
    cat("      A row with no tag cannot be excluded by tag, so carrying it\n",
        "      forward can only accumulate. Fix the block that wrote it:\n",
        "      report() must supply a tag, and an untagged row is an\n",
        "      estimate nothing can grade or reconcile.\n", sep = "")
  }
  x[!.bad, , drop = FALSE]
}

# ---- merge with whatever the other modules have contributed ----------
.fin_prev <- if (td_has("ledger")) td_get("ledger") else NULL
if (!is.null(.fin_prev) && !"module" %in% names(.fin_prev))
  .fin_prev$module <- "pre-split"
.fin_led  <- .fin_drop_untagged(.fin_led,  paste(.fin_mod, "this run"))
.fin_prev <- .fin_drop_untagged(.fin_prev, "the carried ledger")
.fin_all <- if (is.null(.fin_prev)) .fin_led else
  if (is.null(.fin_led)) .fin_prev else
    bind_rows(.fin_led, .fin_prev[!(.fin_prev$tag %in% .fin_led$tag), ,
                                  drop = FALSE])
if (!is.null(.fin_all)) .fin_all <- .fin_all %>% arrange(tag)

.fin_blind_prev <- if (td_has("ledger_blind")) td_get("ledger_blind") else list()
.fin_blind <- utils::modifyList(.fin_blind_prev, .blind)

.fin_dec_prev <- if (td_has("ledger_decisions")) td_get("ledger_decisions") else
  character(0)
.fin_dec <- unique(c(.fin_dec_prev, unlist(.decide)))

cat(sprintf("  %s contributes %d ledger rows; accumulated total %d.\n",
            .fin_mod, if (is.null(.fin_led)) 0L else nrow(.fin_led),
            if (is.null(.fin_all)) 0L else nrow(.fin_all)))
if (!is.null(.fin_all) && "module" %in% names(.fin_all)) {
  cat("  by module: ")
  print(table(.fin_all$module))
}

# ---- write -----------------------------------------------------------
if (isTRUE(TD_NO_WRITE)) {
  .fin_exist <- vapply(c("training_ledger.csv", "training_decisions.txt",
                         "BLIND_training_estimates.rds"),
                       function(f) file.exists(file.path(OUTDIR, f)),
                       logical(1))
  if (any(.fin_exist))
    cat(sprintf("  TD_NO_WRITE is TRUE: preserving %s\n",
                paste(names(.fin_exist)[.fin_exist], collapse = ", ")))
  rm(.fin_exist)
} else {
  if (!is.null(.fin_all))
    write.csv(.fin_all, file.path(OUTDIR, "training_ledger.csv"),
              row.names = FALSE)
  saveRDS(.fin_blind, file.path(OUTDIR, "BLIND_training_estimates.rds"))
  if (length(.fin_dec))
    writeLines(.fin_dec, file.path(OUTDIR, "training_decisions.txt"))
  # Per-module copies as well, so a reader can see which module produced
  # which rows without parsing the merged file.
  if (!is.null(.fin_led))
    write.csv(.fin_led,
              file.path(OUTDIR, paste0("training_ledger_", .fin_mod, ".csv")),
              row.names = FALSE)
  cat(sprintf("  written to %s\n", OUTDIR))
}

# ---- print this module's rows ---------------------------------------
if (!is.null(.fin_led) && nrow(.fin_led)) {
  .cols <- intersect(c("tag", "n", "se", "mde", "benchmark", "verdict",
                       "benchmark_f", "verdict_f", "fe", "status"),
                     names(.fin_led))
  print(as.data.frame(.fin_led %>% select(dplyr::all_of(.cols))),
        row.names = FALSE)
  rm(.cols)
} else cat("  (no rows from this module)\n")

if (length(.decide)) {
  cat("\n--- Decisions this module supports ---\n")
  for (.i in seq_along(.decide)) cat(sprintf("%2d. %s\n", .i, .decide[[.i]]))
  rm(.i)
}

if (isTRUE(get0("BLIND", ifnotfound = TRUE))) {
  cat("\nPoint estimates withheld to BLIND_training_estimates.rds.\n")
} else {
  cat("\nBLIND is FALSE: point estimates are printed above and are also in\n")
  cat("BLIND_training_estimates.rds.\n")
}

# ---- hand off --------------------------------------------------------
if (exists("ev")) td_put("ev", ev)
if (!is.null(.fin_all))   td_put("ledger", .fin_all)
if (length(.fin_blind))   td_put("ledger_blind", .fin_blind)
if (length(.fin_dec))     td_put("ledger_decisions", .fin_dec)

# ---- close the sink this module opened -------------------------------
# v23's rule, unchanged: close only what was opened, then prove the stack
# is level. A module that quietly changes its caller's sink depth produces
# a caller whose output goes nowhere, which is the least diagnosable
# failure in this pipeline.
# ---- close the log this module opened --------------------------------
# ONE SINK, held by the contract, so the close is td_log_close() and
# nothing else. 03_logging.R explains why the second sink this block used
# to unwind is no longer opened: stacking two with split = TRUE puts every
# line in both files and neither is a record of one run.
#
# The balance assertion stays. A module that quietly changes its caller's
# sink depth produces a caller whose output goes nowhere, which is the
# least diagnosable failure in this pipeline: nothing errors, the run
# continues, and the log simply stops.
if (isTRUE(get0(".t_owns_sink", ifnotfound = FALSE))) {
  # ANNOUNCED BEFORE THE CLOSE, VERIFIED AFTER. v1 of this block did both
  # after td_log_close(), so the confirmation went to the console and not
  # into the transcript: the 25 August 14:36 logs record the companion
  # being planned at the top and say nothing at the foot about whether it
  # was written. The line that matters to a reader opening the archive
  # later is the one naming its sibling, and it has to be inside the file.
  .fin_src <- get0(".t_log_file",  ifnotfound = NA_character_)
  .fin_dst <- get0(".t_companion", ifnotfound = NA_character_)
  if (!is.na(.fin_dst))
    cat(sprintf("Companion transcript: copying to %s in the designs directory.\n",
                basename(.fin_dst)))
  td_log_close()
  # From here the sink is closed, so this reaches the console only. It is
  # the failure path that needs saying out loud and the success path is
  # already recorded above.
  if (!is.na(.fin_src) && !is.na(.fin_dst) && file.exists(.fin_src)) {
    if (!file.copy(.fin_src, .fin_dst, overwrite = TRUE))
      cat("Companion copy FAILED:", .fin_dst, "\n")
  } else if (!is.na(.fin_dst)) {
    cat("Companion copy SKIPPED: the archive path was not recorded.\n")
  }
  rm(.fin_src, .fin_dst)
} else {
  # Inherited the caller's sink, so the caller closes it. Registering the
  # module with the contract's logger is still correct and is a no-op on
  # a sink this module does not own.
  td_log_close()
}

if (exists(".t_sink_out_before") &&
    (sink.number() != .t_sink_out_before ||
     sink.number(type = "message") != .t_sink_msg_before))
  warning(TD03_MODULE, " left the sink stack unbalanced (output ",
          .t_sink_out_before, " -> ", sink.number(), ", message ",
          .t_sink_msg_before, " -> ", sink.number(type = "message"),
          "). The caller's log may be dead from here on.", call. = FALSE)

td_check_seals(TD03_MODULE)

# v28b. TD03_MODULE IS CLEARED HERE, and the reason is a failure the
# 03_logging.R guard does not catch.
#
# That guard stops when TD03_MODULE is ABSENT. It cannot stop when the
# name is STALE, because a leftover value is indistinguishable from a
# fresh one. On 7 September a session that had finished 03c sourced
# 03_logging.R by hand before running 04financing_space.R, which sets no
# module name of its own; the guard saw 03c's name still sitting there,
# opened a transcript called 03c_experience_units_20260907_152731.log,
# and 04's entire run was written into it. Nothing errored and the file
# is named for a module that was not running.
#
# Clearing the name at close makes the two failures one. A module owns
# its name for the length of its own run and no longer, so the next
# caller either sets one or hits the stop. Anything in this file that
# needs it, including td_check_seals() above, has already used it.
if (exists("TD03_MODULE", envir = globalenv()))
  rm("TD03_MODULE", envir = globalenv())

rm(.fin_mod, .fin_led, .fin_prev, .fin_all, .fin_blind, .fin_blind_prev,
   .fin_dec, .fin_dec_prev)
