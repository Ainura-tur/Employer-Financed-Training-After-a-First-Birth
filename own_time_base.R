# =====================================================================
# own_time_base.R
#
# THE ONE NUMBER POINT 4 STILL NEEDS: the base of the own-time-ALONE
# cell.
#
# The abstract says the cells where a mother trains on her own time alone
# move -0.0025 against -0.0160 where she also trains on released time, so
# eighty-six per cent of the apparent own-time decline is employer
# withdrawal arriving through a definition that counts any own-time
# report. The eighty-six is a share of the FALL and it is right:
# 0.0160 / 0.0185.
#
# What has never been checked is the DENOMINATOR. The paper reads -0.0025
# against 0.0711, the base for ANY own-time training. But a mother who
# trains on her own time and on released time in the same wave is in the
# "both" cell, not the "alone" cell, so the alone cell's own base is much
# smaller. Against the right base, -0.0025 is a larger proportional
# movement than the sentence implies, and the bound the paper draws from
# it ("about a seventh") is drawn against the wrong quantity.
#
# The concentration index below is the honest summary: the share of the
# fall sitting in the "both" cells divided by the share of the base
# sitting there. An index near 1 means the fall is spread in proportion
# to where the training is, which would make "employer withdrawal
# arriving through a definition" a description of the definition rather
# than a finding about employers.
#
# WHY THIS IS NOT IN 03b. 03b reports the proportional declines against
# each margin's own base, 39.0% of 0.2981 and 26.0% of 0.0711, which is
# the right comparison for the two MARGINS. This script is one level
# down: it splits the own-time margin into its two cells and gives each
# its own base. Nothing in the pipeline does that.
#
# RUN AFTER 03a in the same session, so that `ev` exists. Nothing is
# written to any output folder; results print and collect in OTB.
# =====================================================================

#Namespace protection (avoid MASS/stats masking dplyr verbs)
select <- dplyr::select;       filter    <- dplyr::filter
mutate <- dplyr::mutate;       slice     <- dplyr::slice
recode <- dplyr::recode;       rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange <- dplyr::arrange;     count     <- dplyr::count;  lag <- dplyr::lag

suppressPackageStartupMessages({ library(dplyr); library(fixest) })

if (!exists("ev"))
  stop("`ev` is not in this session. Run 03a first, in the same session.")
.need <- c("person_id", "wave", "female", "employed", "treated", "k",
           "jttpewt_any", "jttpeot_any")
if (length(setdiff(.need, names(ev))))
  stop("ev lacks: ", paste(setdiff(.need, names(ev)), collapse = ", "))

.fe <- if (exists("fe_block", mode = "function")) fe_block() else
  "person_id + wave^female"
cat("fixed effects:", .fe, "\n")

emp <- ev %>%
  filter(employed == 1) %>%
  mutate(own_alone = as.integer(jttpeot_any == 1 & jttpewt_any == 0),
         both      = as.integer(jttpeot_any == 1 & jttpewt_any == 1))

.did <- function(y) {
  m <- feols(as.formula(paste(y, "~ post + post_f |", .fe)),
             data = emp, cluster = ~person_id, notes = FALSE)
  ct <- fixest::coeftable(m)
  c(est = unname(ct["post_f", 1]), se = unname(ct["post_f", 2]))
}

# ---------------------------------------------------------------------
# 1. BASES. Employed treated women before the birth, on the same window
#    03b uses for its proportional declines.
# ---------------------------------------------------------------------
base <- emp %>%
  filter(treated == 1, female == 1, k %in% -3:-1) %>%
  summarise(n = n(),
            released_any = mean(jttpewt_any, na.rm = TRUE),
            own_any      = mean(jttpeot_any, na.rm = TRUE),
            own_alone    = mean(own_alone,   na.rm = TRUE),
            both         = mean(both,        na.rm = TRUE))
cat("\n=== 1. Pre-birth bases, employed women, k = -3 to -1 ===\n")
print(as.data.frame(round(base, 4)), row.names = FALSE)
cat(sprintf("  check: own_alone + both = %.4f against own_any %.4f\n",
            base$own_alone + base$both, base$own_any))
cat(sprintf("  03b quotes 0.2981 and 0.0711 for the two margins.\n"))

# ---------------------------------------------------------------------
# 2. THE TWO CELLS, EACH AGAINST ITS OWN BASE.
# ---------------------------------------------------------------------
cat("\n=== 2. Each cell against its own base ===\n")
d_any   <- .did("jttpeot_any")
d_alone <- .did("own_alone")
d_both  <- .did("both")
.line <- function(lab, v, b)
  cat(sprintf("  %-28s %+.4f (SE %.4f)  base %.4f  = %+6.1f%% of it\n",
              lab, v[["est"]], v[["se"]], b, 100 * v[["est"]] / b))
.line("own-time, any (paper -0.0185)", d_any,   base$own_any)
.line("own time ALONE (paper -0.0025)", d_alone, base$own_alone)
.line("own time AND released time",     d_both,  base$both)

cat(sprintf("\n  The paper reads the alone cell against %.4f, the ANY base.\n",
            base$own_any))
cat(sprintf("  Against its OWN base of %.4f it is %+.1f%%, not %+.1f%%.\n",
            base$own_alone, 100 * d_alone[["est"]] / base$own_alone,
            100 * d_alone[["est"]] / base$own_any))

# ---------------------------------------------------------------------
# 3. THE BOUND THE PAPER DRAWS. "About a seventh" of 0.0711 is 0.0102;
#    the interval on the alone cell has to be read against 0.0186 or so.
# ---------------------------------------------------------------------
cat("\n=== 3. The bound, on both denominators ===\n")
.lo <- d_alone[["est"]] - 1.96 * d_alone[["se"]]
.hi <- d_alone[["est"]] + 1.96 * d_alone[["se"]]
cat(sprintf("  alone cell 95%% interval: [%+.4f, %+.4f]\n", .lo, .hi))
cat(sprintf("    as a share of the ANY base  (%.4f): [%+.1f%%, %+.1f%%]\n",
            base$own_any,   100 * .lo / base$own_any,   100 * .hi / base$own_any))
cat(sprintf("    as a share of its OWN base  (%.4f): [%+.1f%%, %+.1f%%]\n",
            base$own_alone, 100 * .lo / base$own_alone, 100 * .hi / base$own_alone))
cat("  A proportional fall matching the employer margin's 39.0% would be\n")
cat(sprintf("    %+.4f on the alone cell. That is %s the interval above.\n",
            -0.390 * base$own_alone,
            if (-0.390 * base$own_alone >= .lo) "INSIDE" else "outside"))

# ---------------------------------------------------------------------
# 4. CONCENTRATION. Share of the fall in the "both" cells, over their
#    share of the base. Near 1 means the fall follows the training.
# ---------------------------------------------------------------------
cat("\n=== 4. Is the fall concentrated, or does it follow the base? ===\n")
.share_fall <- d_both[["est"]] / (d_alone[["est"]] + d_both[["est"]])
.share_base <- base$both / base$own_any
cat(sprintf("  'both' cells hold %.1f%% of the own-time base\n",
            100 * .share_base))
cat(sprintf("  and carry %.1f%% of the own-time fall  (the paper's 86%%)\n",
            100 * .share_fall))
cat(sprintf("  concentration index: %.2f\n", .share_fall / .share_base))
cat("  An index near 1 means the fall is spread in proportion to where the\n",
    "  training is, and 'employer withdrawal arriving through a definition'\n",
    "  then describes the definition rather than a finding about employers.\n",
    sep = "")

OTB <- list(base = base, any = d_any, alone = d_alone, both = d_both,
            index = .share_fall / .share_base)
cat("\nDone. Collected in OTB.\n")
