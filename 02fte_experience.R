# =====================================================================
# 02_FTE_EXPERIENCE.R    revision 5
#
# Experience measured in hours, not in calendar years.
#
# WHY. experience_years is ehtjb, total years in paid work. It is a count
# of years and weights a twelve-hour year the same as a fifty-hour year.
# The descriptive table says mothers' dominant post-birth margin is
# intensive rather than extensive: full-time incidence falls 68.5 -> 36.0
# while employment falls 89.8 -> 75.1, so roughly eighteen of the
# thirty-two points of lost full-time work is a move into part-time
# employment. By construction ehtjb records none of that, which is why
# the estimated experience gap (2.3 years at k = +10) is close to what
# the extensive margin alone would generate, and why the 29% attribution
# built from it is a count-based lower bound on the accumulation channel.
#
# WHAT THIS DOES NOT DO. Re-expressing experience in FTE units is not a
# free increase in the attributed share. A CONSTANT rescaling of a
# regressor rescales its coefficient inversely and leaves the product
# return x gap untouched. The gain is real only because the FTE weight is
# person-and-wave-specific rather than constant, so it changes which
# variation identifies the return. Which way the product moves is an
# empirical question, and both sides of the multiplication have to be
# re-estimated in the same units. That is the point of Section D below.
#
# UNITS. The FTE weight can only be built where hours are observed, so
# in-panel. The pre-panel base stock stays in calendar years. This is
# coherent for every fixed-effects and event-study design, because the
# person effect absorbs the base stock and its entry-cohort recall error;
# it is NOT coherent for the descriptive level comparisons in
# tab:interruption, which must stay in calendar years with the unit
# stated.
#
# ---------------------------------------------------------------------
# TWO THINGS EVERY STOCK IN THIS SCRIPT IS INDEXED BY
#
# Revision 3 exists because revision 2 varied two things at once and
# reported the result as though it had varied one. Every cumulative stock
# here is defined by a pair of choices, and the pair has to be stated
# whenever two stocks are compared:
#
#   THE WEIGHT        what one employed wave contributes
#     calendar        1 for anyone employed
#     one-sided       min(h / FT_HOURS, 1), part-time discounted, no
#                     overtime credit
#     two-sided       the WEIGHT_RULE weight, which also CREDITS overtime
#                     up to FTE_CAP
#
#   THE GAP TREATMENT what an unobserved interior wave contributes
#     _obs            nothing
#     _int            the linear interpolation of the weight across the gap
#
# A comparison is only a comparison of weights if both sides share a gap
# treatment. Revision 2's headline, exp_fte_int against exp_cal_obs, did
# not: it was a two-sided interpolated stock against a calendar gap-blind
# one, so the ratio it reported mixed the weight with the gap credit.
# Every reported pair in revision 3 is gap-consistent, and the size of the
# confound is measured and printed rather than assumed small.
#
# ---------------------------------------------------------------------
# REVISION 5. A review of the revision 4 log. The arithmetic reconciled
# throughout; what it found was in control flow, in claims the code did
# not enforce, and in one printed conclusion that did not depend on the
# estimate it was a conclusion about. Tagged Y1 to Y6.
#
#  Y1 THE D2 BOUND DID NOT LOOK AT THE ESTIMATE. The condition was
#     mde(se_fte) < req_min, which is a statement about precision alone,
#     and the sentence it printed was "the gendered-price account is
#     excluded by the bound". Had fte_f been +0.03 with the same standard
#     error, the account would have been strongly CONFIRMED and this
#     block would have printed the identical sentence, with BLIND hiding
#     the estimate from anyone reading the log. That is the one item here
#     that could have put a wrong sentence in the paper.
#
#     A bound needs the confidence limit, not the minimum detectable
#     effect. The condition is now |estimate| + z * SE < req_min, read
#     from the object the estimates are already stored in, so nothing is
#     printed that BLIND withholds and the claim is about the interval
#     rather than about the design.
#
#     The wording is corrected too. req_min is the SMALLER of the two
#     required price gaps, so ruling it out is the harder test and the
#     conservative choice; calling it "the tighter of those" without
#     saying which way was ambiguous.
#
#  Y2 THE WEIGHTING GRID'S BASELINE CELL DID NOT REPRODUCE THE HEADLINE.
#     sens_one() accumulated on the panel joined to the cohorts, while
#     the D1 path accumulates on the panel joined to the ADDENDUM and
#     then the cohorts. So the (cap, 38, 1.25) cell reported -4.70 where
#     D1 reports -4.691. One rounding step in size, but a grid whose
#     entire purpose is to vary one thing at a time was varying the
#     sample as well as the weight. It now runs on the same
#     weight-observed frame, and the baseline cell is asserted against
#     the headline rather than left for a reader to notice.
#
#  Y3 THE WITHIN-COUPLE SAMPLE WAS ANNOUNCED AND NOT IMPOSED. The block
#     computes cw_pairs, prints its size, says "the printed sample is the
#     one the estimates come from", and then fits on cw. The printed
#     count matched the fitted n only because fixest drops singleton
#     couple_id^wave cells by default and no cell happened to survive
#     with one partner. Fitting on cw_pairs costs nothing and makes the
#     sentence true.
#
#  Y4 exp_ftna_obs IS NOT WHAT ITS LABEL SAYS. It is built from
#     employed == 1 & is.na(fulltime), and fulltime is never missing in
#     this build while hours are missing on 220 employed waves, so the
#     column is identically zero. The identity still closes, because it
#     is an identity in fulltime rather than in hours. What was wrong was
#     the printed line "residual, employed with hours missing", which
#     told the reader that the hours-missing waves contribute nothing to
#     the decomposition. They are not in the decomposition at all: they
#     sit inside exp_ft_obs or exp_pt_obs and are removed from Section D
#     by the addendum join. Two mechanisms, one label.
#
#  Y5 SECTION E'S "RETURN" COLUMNS DO NOT MEASURE A RETURN.
#     first_return_k is the minimum k over employed post-birth waves, so
#     it is 0 for anyone who never left, and return_ft is full-time
#     status at that wave. med_return_k = 0 for both sexes is therefore
#     mostly "most people are employed at k = 0", and pct_return_ft =
#     90.3 for fathers is "fathers are full-time at k = 0". The script
#     names pct_return_ft one of the two columns to quote, so the wording
#     carries weight: both are renamed to what they measure, and the
#     conditional version, restricted to those who actually left, is
#     reported beside them.
#
#  Y6 TWO COMMENTS THAT DENIED WHAT THE CODE DOES. interp_one() said it
#     "never extrapolates beyond the data" because the first and last
#     waves of a span are observed by construction. What is observed by
#     construction is a PANEL ROW, not a non-missing weight: a person
#     whose first or last panel wave is employed with hours missing gets
#     flat extrapolation from the nearest observed weight, and a person
#     with one observed weight gets it replicated across the span. Small
#     in this build and it feeds exp_fte_int, which D3 rests on entirely.
#     Spells, separately, are counted in observed ROWS rather than in
#     waves, so an unobserved interior wave welds two spells into one;
#     9.7% of persons have such a gap.
#
# ---------------------------------------------------------------------
# REVISION 4. Three changes, all of them consequences of the loader
# reaching v21.5 rather than of anything in the argument here.
#
#  H1 THE INCREMENTS ARE READ, NOT REBUILT. Section C derived d_paid and
#     d_olf from actual_experience and years_not_in_lf with a group_by and
#     a lag. The loader has built exactly those since v21, as
#     d_paid_years, d_unemp_years, d_olf_years and d_exposure_years, with
#     d_wave beside them and olf_flow_valid marking the consecutive-wave
#     restriction. Two definitions of the same quantity in two files is
#     how they drift, and the loader's version is the one the wage
#     designs read, so this reads it too. On a panel that predates v21 the
#     compat helper rebuilds them, so the script still runs against an
#     older build; it says which it did.
#
#  H2 THE COMPAT GUARDS ARE SOURCED AND THE PANEL IS CHECKED. This script
#     reads a panel and estimates on it, which is exactly the position
#     00_compat_v21.R exists to guard. It now checks the build tag, prints
#     the restriction manifest the loader stamped on the panel, and uses
#     require_cols so that a column present and ALL-NA fails as loudly as
#     one absent. The panel fingerprint block stays: it answers a
#     different question, which is whether two scripts read the same file.
#
#  H3 THE WITHIN-PERSON EXEMPTION IS DECLARED RATHER THAN ASSUMED. The
#     compat guard stops an interruption or experience LEVEL entering a
#     specification, because the base stock carries the entry cohort's
#     recall vintage: 4.93 years out of the labour force for the original
#     and 2011 intakes against 1.72 for the 2019-plus intake. Every wage
#     equation in Section D carries person fixed effects, which absorb
#     that base stock, and the UNITS note at the top of this file has said
#     so since revision 1. The guard is called with
#     allow_within_person = TRUE immediately before those specs, so the
#     claim is on the log next to the estimates that depend on it rather
#     than in a comment three hundred lines away. The descriptive level
#     comparisons remain outside the exemption, which is what the same
#     note has always said.
#
# ---------------------------------------------------------------------
# REVISION 3. What changed, and why. Tagged G1 to G9 and cited inline.
#
#  G1 THE HEADLINE RATIO IS GAP-CONSISTENT. Section D led with gap_fte
#     over gap_cal and with d_fte / d_cal, where the FTE series was
#     exp_fte_int and the calendar series was exp_cal_obs. Those differ in
#     the weight AND in whether interior wave gaps are credited, so the
#     ratio was not a comparison of weights. exp_fte_obs is now the
#     primary FTE series, gap-consistent with the calendar series, and
#     exp_fte_int is reported beside it as the gap-treatment sensitivity.
#     Both ratios are printed. The identity in D1b closed correctly in
#     revision 2 only because its three components were all gap-blind; the
#     FTE row sat outside that family without saying so.
#
#     HOW BIG THE CONFOUND IS, IS AN EMPIRICAL QUESTION AND THIS SCRIPT
#     ANSWERS IT RATHER THAN ASSUMING IT. Interior wave gaps could plausibly
#     be correlated with motherhood, which would make the credit larger for
#     women and inflate the ratio; they could equally be correlated with
#     job mobility, which runs the other way. The gap credit is therefore
#     measured by sex in Section B and printed, and the ratio is printed
#     under both gap treatments in Section D. Read those two numbers before
#     writing anything about the size or the sign of this confound. On the
#     build this script was last run against it came back SMALL and
#     REVERSED, larger for men than for women, but that is a property of
#     the build and not of the argument, and a different panel could give a
#     different answer.
#
#  G2 THE ONE-SIDED STOCK IS BUILT HERE AND SHIPPED IN THE ADDENDUM. The
#     WEIGHT_RULE weight is two-sided: it discounts a part-time year and
#     CREDITS an overtime year up to FTE_CAP. The hypothesis the paper
#     makes is one-sided, that a mother's calendar year holds fewer hours
#     than a father's, so the arm that answers it caps the weight at one.
#     Revision 2 of 03 built that stock locally over observed panel rows,
#     which silently made it the analogue of exp_fte_obs while it was
#     being compared against exp_fte_int. It is now built here, on the
#     same span grid as every other stock, in both gap treatments, and
#     written to the addendum so there is one definition and not two.
#     Note the name: exp_pt_obs is ALREADY TAKEN in this script for the
#     count of part-time calendar years, so the one-sided stocks are
#     exp_one_obs and exp_one_int. Do not call them pt.
#
#  G3 THE ADDENDUM'S SAMPLE RESTRICTION IS NAMED. observed was defined as
#     !is.na(fte_w), which is FALSE both for a wave the person was not
#     interviewed in and for a wave they were interviewed in while
#     employed with hours missing. The addendum filters on it, so those
#     rows were dropped from the addendum, from Section D here, and from
#     every arm of T8 in 03, without appearing in any row count as a
#     restriction. The two conditions are now separate flags, in_panel and
#     weight_obs, both carried, and the count dropped is printed.
#
#  G4 THE DEPRECIATION SPLIT DECLARES ITS DEPENDENCE ON INTERPOLATION. The
#     recency split needs a value at every wave, so it is built on the
#     interpolated stock by necessity rather than by choice. That is now
#     stated, a gap-blind version is estimated beside it, and the fact
#     that exp_fte_distant is mechanically zero for the first RECENT_K
#     waves of a person's span is printed with the share of the sample it
#     applies to.
#
#  G5 THE ONE-SIDED WEIGHT IS ALREADY A CELL OF THE B2 GRID. cap = 1.00
#     under the "cap" rule IS the one-sided weight, so the grid already
#     prices the choice G2 makes primary. That cell is now labelled, and
#     the grid reports it against the FTE_CAP cell so the overtime credit
#     is visible as the difference between two rows of an exhibit that was
#     already being produced.
#
#  G6 A RATIO IS NOT FORMED ON A DENOMINATOR THAT CAN BE ZERO. A2's
#     composition decomposition divided by its own total with no guard, so
#     a total near zero would have printed percentages of arbitrary size.
#     The same fault was fixed twice in 03 and in the audit; it is guarded
#     here.
#
#  G7 THE WEDGE VARIABLES ARE NAMED FOR WHAT THEY HOLD. wm held the
#     female value and wf the male one. The printed sentence was correct
#     and the names were inverted, which is the kind of thing that stays
#     correct until someone edits the line.
#
#  G8 A CONCAVE RULE WITH A CEILING AT OR BELOW ONE FALLS BACK TO THE HARD
#     CAP SILENTLY. That is right as arithmetic and wrong as a log: the
#     run would report rule = concave while applying a cap. It now says so.
#
#  G9 THE CLOSING INSTRUCTION POINTED AT THE WRONG COLUMN. Item 6 told the
#     reader to put exp_fte_int into the DML specification, which is the
#     gap-inconsistent choice G1 exists to correct. It now names the
#     gap-blind one-sided stock, which is what 03's T8 reads.
#
# ---------------------------------------------------------------------
# REVISION 3.1. Faults in revision 3 itself, found by reading its log
# against its own code. Tagged G10 to G16.
#
# G10 THE EVENT-SAMPLE ROW ACCOUNTING MISATTRIBUTED ITS OWN LOSS. G3 added
#     a line reporting that the join to the addendum drops the rows where
#     the FTE weight could not be computed, and it printed the difference
#     between the panel and the event sample as though the whole of it were
#     that. It is not: ev joins the addendum AND the cohort frame, and the
#     cohort frame drops left-censored parents, which is two orders of
#     magnitude larger. Attributing a loss to whichever restriction happens
#     to be named is the exact fault the two-order waterfall in 03's T8
#     exists to prevent, reintroduced here. The loss is now decomposed.
#
# G11 SECTION A REPORTS THE WEDGE UNDER BOTH WEIGHTS. The per-employed-year
#     wedge, which the section calls the mechanical source of everything in
#     Section D, was computed on the two-sided weight alone. Under that
#     weight a father's employed year is credited at more than one, so part
#     of the differential is an overtime credit to long male weeks rather
#     than a discount to short female ones. Both weights are now in the
#     table and the share of the differential that is the credit is
#     printed, because the paper's mechanism sentence is written from this
#     table and the one-sided figure is the defensible one.
#
# G12 A2's CONCLUSION DID NOT PARSE IN ONE BRANCH AND OVERSTATED IN THE
#     OTHER. The sentence was assembled from two cat() calls that only fit
#     together on one path, and the claim that ehtjb "measures a part-time
#     year the same way for both sexes" is not what the table shows: the
#     within-state wedges differ by sex in both states. The conclusion is
#     now written per branch and quotes the within-state differences.
#
# G13 THE HEADLINE READ THE ROUNDED DISPLAY TABLE. The identity residual
#     printed as +0.001 in the headline and as -0.000000 in D1b, on the
#     same decomposition, because the headline was built from the
#     three-decimal display frame. Same fault as the one fixed for
#     d_cal_hi twenty lines below it.
#
# G14 THE FE CONTRAST TABLE'S n MIXED TWO QUANTITIES. Fitted rows came from
#     m$nobs and the constant-outcome branch returned nrow(d), so one row
#     of the table counted supplied rows and the others counted used rows,
#     and the difference between them, which is singleton dropping, looked
#     like a larger sample. Both are now columns.
#
# G15 ord_chk's max WAS UNINFORMATIVE. Most rows sit at zero on both sides
#     of each ordering, so the maximum of the difference confirms the
#     ordering and says nothing about magnitude. The share nonzero and the
#     conditional mean are reported with it.
#
# G16 THE WITHIN-COUPLE BLOCK PRINTED A SAMPLE IT DID NOT FIT ON.
#     couple_id^wave silently drops couple-waves with only one partner
#     present, so the announced sample was a third larger than the fitted
#     one. The both-partners subset is now counted before the fit, the way
#     03's T6 does it explicitly.
#
# ---------------------------------------------------------------------
# REVISION 3.2. Faults in 3.1, again found by reading its log against its
# own code. Tagged G17 to G20.
#
# G17 THE CROSS-CHECK G11 ASKS FOR WAS NORMALISED SO THAT IT COULD NOT
#     PASS. Section A reports the overtime credit as a share of a per-
#     employed-year wedge, which is a purely INTENSIVE quantity, and it
#     came out near 30%. Section D reported it as a share of the whole
#     FTE-over-calendar ratio, and that ratio carries the EXTENSIVE margin
#     in both its numerator and its denominator, which dilutes the share to
#     17%. The script then told the reader those two should agree. The
#     comparable quantity in D is the credit as a share of the ratio's
#     excess over one, because that excess IS the intensive margin, and it
#     is 35%. Both sections now print the intensive share, D also prints the
#     extensive and intensive margins in years, and the comparison states
#     what residual difference to expect from the three estimands rather
#     than implying they should coincide.
#
#     The extensive and intensive margins turn out to be almost exactly
#     equal, so that decomposition is printed as a first-class line: it
#     needs no ratio and no normalisation beyond the weight, which is the
#     same reason gap_ft and gap_pt lead the table.
#
# G18 THE ONE-SIDED RATIO WAS ANNOUNCED TWICE. G11 inserted a share
#     sentence directly above a sentence that already reported the same
#     ratio, and left both.
#
# G19 SECTION A's DIFFERENTIALS WERE DIFFERENCED FROM ROUNDED VALUES. A
#     reported 0.267 and A2 reported 0.268 for the same quantity on the
#     same rows, because A differenced two figures already rounded to three
#     decimals. Same fault as G13, one section earlier.
#
# G20 ord_chk's max COLUMN BECAME REDUNDANT. When the ordering holds and any
#     rows are tied, the maximum of the difference is exactly zero by
#     construction, and the viol column already reports violations. It is
#     replaced by the median of the difference where the two stocks differ,
#     which says whether the typical gap is like the mean or the mean is
#     carried by a tail.
#
# ---------------------------------------------------------------------
# REVISION 2. What changed, and why.
#
#  R1 PRESENTATION. The event-time table now leads with gap_ft and gap_pt.
#     The full-time and part-time year counts make the point with no
#     full-time-week normalisation and no cap, so they establish the
#     result; the FTE series prices it. Column order follows that logic.
#
#  R2 NORMALISATION. Every gap is also reported as a CHANGE from k = -1.
#     The pre-birth gaps are not zero, so the level ratio fte_over_cal
#     reads 2.3 to 3.4 before the birth purely because both gaps are
#     small there. The ratio is now formed from changes and is suppressed
#     where the calendar change is too small to divide by.
#
#  R3 THE CAP IS DOING REAL WORK. Roughly a quarter of male person-waves
#     sit exactly at the 1.25 ceiling, and the weighting grid moves the
#     headline gap by more than half its own size. Two responses, both
#     implemented: the grid is reported as a first-class exhibit rather
#     than a footnote, and a concave weight is offered as an alternative
#     to the hard cap. The concave rule is the identity below a full-time
#     week and approaches the same ceiling asymptotically above it, so it
#     has no mass point and no kink for the estimator to sit on, and it
#     leaves the part-time region, where the substantive action is,
#     untouched.
#
#  R4 THE INTERNAL IDENTITY IS CHECKED, NOT ASSERTED. Calendar years are
#     full-time plus part-time years (plus the employed waves where hours
#     are missing). The decomposition of the calendar contrast into its
#     two components is therefore arithmetic, and the residual is printed
#     so that it can be seen to be arithmetic.
#
#  R5 THE SHAPE TEST IS DROPPED. Its MDE is far larger than any plausible
#     convexity in the ten-year penalty, and the support table shows why:
#     only a few hundred mothers occupy the rows where concentrated and
#     fragmented histories coexist. It is dropped the way the audit
#     dropped D7. The fit is still run so the MDE can be quoted as the
#     reason, but the estimate is withheld unconditionally, not under
#     BLIND, so it cannot be recovered and reported after the fact.
#
#  R6 THE DEPRECIATION TEST IS THE SURVIVING ONE. It needs no interrupted
#     men, and the two stocks are correlated loosely enough to separate.
#
#  R7 THE REPRICING SECTION IS RESTATED AS A BOUND. This specification is
#     less precise than the paper's DML, so it cannot detect the pooled
#     gap in the return to experience, let alone a change in it. What it
#     can do is bound the FTE version well inside what a gendered-price
#     account of the ten-year penalty would require. That is a stronger
#     null than a p-value, and it is what is now printed.
#
#  R8 PANEL FINGERPRINT. 02 and 03 must read the same build. Both now
#     resolve the panel the same way and print an identical fingerprint
#     block, including the counts the manuscript quotes, so two logs can
#     be compared directly instead of inferred to agree from one matching
#     number.
#
# Input : hilda_panel_data_W12_W24_slim.rds
# Output: output/fte_experience/panel_fte_addendum.rds   keys person_id, wave
#         output/fte_experience/*.csv                    diagnostics
#         output/fte_experience/fte_log.txt
# =====================================================================

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select <- dplyr::select;       filter    <- dplyr::filter
mutate <- dplyr::mutate;       slice     <- dplyr::slice
recode <- dplyr::recode;       rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange <- dplyr::arrange;     count     <- dplyr::count;  lag <- dplyr::lag

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(fixest)
})

# ---------------------------------------------------------------------
# CONFIGURATION
# ---------------------------------------------------------------------
FT_HOURS <- 38          # Australian standard full-time week
FTE_CAP  <- 1.25        # a sixty-hour week is not 1.6 years of accumulation
WEIGHT_RULE <- "cap"    # "cap" (hard ceiling) or "concave" (smooth, R3)
# The gap treatment for every PRIMARY series and for the addendum's hybrid
# stocks (G1). "obs" credits an unobserved interior wave with nothing;
# "int" credits it with the linear interpolation of the weight. Whichever is
# chosen, the other is built as well and reported beside it, because the
# difference between them is a quantity in its own right and not a nuisance.
# "obs" is the default because it makes the FTE series gap-consistent with
# the calendar series, which is what the headline ratio compares it to.
GAP_TREATMENT <- "obs"
RECENT_K <- 3           # waves counted as "recent" in the depreciation split
K_LO     <- -5L
K_HI     <- 10L
KBASE    <- -1L         # normalisation base for every event-time gap (R2)
POWER    <- 0.80
ALPHA    <- 0.05
BLIND    <- TRUE        # withhold WAGE-EQUATION point estimates (Section D)
stopifnot(GAP_TREATMENT %in% c("obs", "int"))
# Resolved once so no section can silently pick a different series (G1).
FTE_PRIMARY   <- paste0("exp_fte_", GAP_TREATMENT)
FTE_SECONDARY <- paste0("exp_fte_", setdiff(c("obs", "int"), GAP_TREATMENT))
ONE_PRIMARY   <- paste0("exp_one_", GAP_TREATMENT)

# Sensitivity grid for the weighting choice (R3). Reported so the FTE gap
# is never a single arbitrary normalisation.
FT_GRID   <- c(35, 38, 40)
CAP_GRID  <- c(1.00, 1.25, Inf)
RULE_GRID <- c("cap", "concave")

# Manuscript quantities, used only as printed benchmarks. Nothing is
# estimated against them; they are here so the log states what the
# published numbers are next to what this build produces.
#
# SCOPE, v21.11. These constants were reproduction targets. If the
# analysis is not matching a published figure, most of them have no
# object, and a comparison against a target that is not being pursued
# frames disagreement as failure. They are kept, with what each is now
# FOR written beside it, because deleting them would silently remove the
# checks that are still worth running.
#
# EXPECTED-BUILD constants. Not a paper: what this loader produced when
# the pipeline was last known good. A fingerprint that checks nothing is
# worse than no fingerprint, so these stay whatever the framing.
MS_ROWS      <- 134131   # expected build, W12-W24 broad sample
MS_PERSONS   <- 20205    # expected build
#
# The two figures below are the manuscript's introduction, which
# disagrees with its own Data section. Worth knowing, because 116,729
# sits 129 rows from 116,600, the count of person-waves the training
# module was put to, so it is probably a training-sample definition and
# not an error. Retire them once that is settled.
# v21.46. RETIRED, per the note above. The introduction no longer reports
# these figures directly: it takes them from \PanelRows and \PanelPersons in
# data_section_numbers.tex. So the literals described a state of the text
# that has since been fixed, and this block was reporting a manuscript error
# that no longer existed. The macro file is read instead, in the same form
# 03_common.R reads it, so the check tracks the manuscript rather than a
# memory of it.
.ms_intro <- local({
  # v29. THE FRAGMENT LIVES IN THE TABLES FOLDER, not the project root, so
  # this check has never found it and has never run. PAPER_TABLES is where
  # paper_tables.R syncs it and where the manuscript \inputs it from;
  # DIR_TABLES is where it is built. Both are tried before the two roots,
  # which are kept for a run with no contract.
  cand <- unique(c(
    if (exists("PAPER_TABLES")) file.path(PAPER_TABLES, "data_section_numbers.tex"),
    if (exists("DIR_TABLES"))   file.path(DIR_TABLES,   "data_section_numbers.tex"),
    if (exists("project_dir")) file.path(project_dir, "data_section_numbers.tex"),
    file.path(getwd(), "data_section_numbers.tex")))
  hit <- cand[file.exists(cand)]
  if (!length(hit)) return(list(rows = NA_integer_, persons = NA_integer_,
                                src = NA_character_, tried = cand))
  txt <- paste(readLines(hit[1], warn = FALSE), collapse = "\n")
  .num <- function(macro) {
    m <- regmatches(txt, regexpr(
      paste0("\\\\newcommand\\{\\\\", macro, "\\}\\{[^}]*(\\{,\\})?[^}]*\\}"), txt))
    if (!length(m)) return(NA_integer_)
    suppressWarnings(as.integer(gsub("[^0-9]", "", sub(
      paste0("\\\\newcommand\\{\\\\", macro, "\\}\\{"), "", m))))
  }
  list(rows = .num("PanelRows"), persons = .num("PanelPersons"),
       src = hit[1], tried = cand)
})
#
# v21.11. ONE QUESTION 02 NO LONGER HAS TO LEAVE OPEN.
#
# base_exp_cal is the pre-panel base stock in CALENDAR years, and every
# hybrid stock in the addendum inherits that: the FTE weight can only
# touch the in-panel part, which is 33.7% of the stock for men and 36.7%
# for women. Whether an FTE base stock could ever be built was the highest
# -value question in the discovery output, and 01 has closed it NEGATIVELY
# and twice over: ehtft, ehtpt, ehtft1 and ehtpt1 are absent from all
# thirteen waves, and the months-and-years components add nothing because
# the History totals are already fractional, with ehtse taking 5,855
# distinct values inside one wave.
#
# The retrospective labour-force calendar, which would have refined the
# in-panel weight instead, is also closed: 207 mnemonics exist and every
# one reads a median non-missing rate of 0.0.
#
# So the hybrid construction below is FINAL, not provisional. Neither
# route to extending the weight exists in this release. That is a sentence
# for the paper rather than an absence, and it means T8's divergence gate
# can never be cleared on the full sample: do not re-open either question,
# and do not read the gate as something a better base stock would fix.

# RETIRED AS A TARGET. Used once, at the calendar-gap table, printed
# beside this build's own gap purely so the two can be compared. That is a
# reproduction comparison and nothing else, so with no target it has no
# role. Keep the constant, ignore the comparison.
MS_EXP_GAP   <- 2.28
#
# POWER BENCHMARKS, and I had these two the wrong way round on the first
# pass. Neither is a reproduction comparison. Each is a MAGNITUDE the
# design is asked to have power against, and that question survives the
# reframing intact: "can this specification detect an effect the size of
# X" is about power, not about matching anybody.
#
#   MS_PENALTY10  the denominator of the calibration at the price-gap
#                 block (what per-year price gap a gendered-price account
#                 of a 0.14 log-point ten-year penalty would require) and
#                 the yardstick the shape test uses to declare its own
#                 MDE uninformative. Both are scale uses.
#   MS_POOLED_BX  the power target at the FTE specification: if the MDE
#                 exceeds it, the design cannot detect the pooled gap and
#                 therefore cannot detect a change in it. That reasoning
#                 does not depend on the number being published.
MS_PENALTY10 <- 0.14
MS_POOLED_BX <- 0.0026
#
# PLAUSIBILITY BENCHMARK. The Mincer return to experience near 1.2 to 1.5%
# per year is a literature magnitude rather than one paper's estimate, so
# expressing an MDE as a share of it is a sanity check on the wage and
# experience construction. The depreciation block does exactly that and
# reads correctly: an MDE of 0.0087 is 64% of the return.
MS_RETURN_X  <- 0.0135
MIN_D_CAL    <- 0.05     # below this the change ratio is not formed (R2)

# ---------------------------------------------------------------------
# PATHS. Identical resolution logic in 02 and 03 (R8).
# ---------------------------------------------------------------------
# THE ROOT COMES FROM paths.R OR THE CONTRACT, NEVER FROM A LITERAL HERE.
# This used to fall back to a hard-coded HILDA_project path that is not even
# this tree, so running 02 without paths.R wrote its addendum into another
# project and said nothing. A wrong root has to stop, not default.
if (!exists("project_dir")) {
  if (exists("PROJ_ROOT")) {
    project_dir <- PROJ_ROOT
  } else if (nzchar(Sys.getenv("HILDA_ROOT"))) {
    project_dir <- normalizePath(Sys.getenv("HILDA_ROOT"), winslash = "/",
                                 mustWork = FALSE)
  } else if (file.exists("paths.R")) {
    source("paths.R")
  } else {
    stop("project_dir is unset and paths.R is not in ", getwd(),
         ".\n  Source paths.R first: it is the one place the root is decided, ",
         "and\n  everything this script writes hangs off it.", call. = FALSE)
  }
}

data_dir   <- if (exists("data_dir")) data_dir else file.path(project_dir, "dta_files")
# v27. THE DESTINATION IS CALLED FTE_DIR, NOT OUTDIR, AND THAT IS THE FIX.
# OUTDIR is one of the three back-compat aliases 00_contract.R leaves
# unsealed. This script set it here, opened its log at line 595, and then
# sourced the guards at line ~780; the contract loaded behind them and
# assigned OUTDIR <- DIR_DESIGNS. Everything written before the guards
# landed in fte_experience and everything after landed in output/designs.
# On 1 September that put fte_log.txt alone in one folder and the addendum
# beside training_ledger.csv in another, and 03c could not find it, because
# an addendum has no business in a designs directory and 03c does not look
# there.
#
# Renaming rather than reordering is deliberate. Moving the guard block
# above the log would work and would also move the point at which a guard
# failure is reported, which is a bigger change than this bug warrants. A
# name the contract has never heard of cannot be reassigned by it, whenever
# it loads.
# UNDER output/, WHERE EVERY READER LOOKS. This was
# file.path(project_dir, "fte_experience"), one level above
# output/fte_experience, which is where paths.R sets ADDENDUM and where 03c
# searches. So 02 wrote the addendum, 03c reported it missing, and the answer
# was always to go and find the file by hand.
#
# AND THE ROOT IS AN OUTPUT ROOT, NOT project_dir. That was the second half
# of the same bug. project_dir is where the raw dta_files live: the loader
# builds data_dir from it, and on 5 September it resolved to
# Documents - Copy/Y2025/HILDA_project while the contract searched
# Desktop/WG/TG. Hanging the addendum off it puts the file beside the source
# data, one root away from every reader, which is the same shape as the
# one-level-up bug this comment already describes and just as invisible.
#
# PROJ_ROOT is the contract's root and getwd() is what TD_FRAME_FILES
# searches as ".", so either can be found by a later process. project_dir
# only can when it happens to coincide with one of them, which is what made
# this look fine for as long as it did.
#
# FTE_OUT overrides for a one-off; unset it, this is the destination.
.fte_root <- if (exists("PROJ_ROOT")) PROJ_ROOT else getwd()
FTE_DIR    <- if (exists("FTE_OUT")) FTE_OUT else
  file.path(.fte_root, "output", "fte_experience")

# v28. THE TRANSCRIPT BELONGS IN run_logs, NOT BESIDE THE ADDENDUM.
#
# FTE_DIR is a DATA destination: the addendum 03c reads and the ten csvs
# Section B and D write. The transcript is not data, and putting it there
# meant the one directory a downstream script searches for a .rds also
# accumulated a timestamped .log per run. Every other stage writes
# <module>_<timestamp>.log to run_logs under the project root and this one
# sat outside that for no reason beyond FTE_DIR being the variable that
# happened to be in scope when the sink was opened.
#
# THE ROOT IS RESOLVED THE SAME WAY, WHICH MEANS IT INHERITS THE SAME
# CAVEAT. This runs BEFORE the guards are sourced, so PROJ_ROOT may not
# exist yet and .fte_root may be getwd(). That was survivable when the log
# sat next to outputs written under the same guess; it is a live problem
# once the log has its own destination, because a log written under a
# guessed root is a log nobody finds. So the close block at the foot of
# this file re-resolves the path once the contract has loaded and copies
# the transcript across if the guess was wrong. Guessing and then checking
# is the only order available while the sink opens before the guards.
#
# FTE_LOG_OUT overrides for a one-off; DIR_LOGS is honoured if the
# contract ever defines one, so this stops being a local decision the
# moment there is a central one.
.fte_logdir <- if (exists("FTE_LOG_OUT")) FTE_LOG_OUT else
  if (exists("DIR_LOGS")) DIR_LOGS else file.path(.fte_root, "run_logs")

PANEL_FILE <- "hilda_panel_data_W12_W24_slim.rds"

find_file <- function(fname, dirs) {
  cand <- unique(file.path(dirs, fname))
  hit  <- cand[file.exists(cand)]
  if (!length(hit)) {
    cat("\n*** PANEL NOT FOUND ***\n")
    cat("  file wanted:", fname, "\n  looked in:\n")
    for (d in unique(dirs))
      cat("   ", d, if (dir.exists(d)) "" else "[dir does not exist]", "\n")
    cat("  working dir:", getwd(), "\n")
    for (d in unique(dirs)[dir.exists(unique(dirs))]) {
      rds <- list.files(d, pattern = "\\.rds$")
      if (length(rds)) {
        cat("  .rds files in", d, ":\n"); print(utils::head(rds, 20))
      }
    }
    stop("Set project_dir, or run the loader so the slim panel exists.",
         call. = FALSE)
  }
  if (length(hit) > 1) hit <- hit[which.max(file.mtime(hit))]
  hit[1]
}
# project_dir BEFORE getwd(). A copy of the panel left in the working
# directory used to win over the one in the root, so 02 could silently use a
# different vintage from the one 03 reads.
PANEL <- find_file(PANEL_FILE, c(project_dir, data_dir, getwd()))

cat("\n--- 02 paths ---\n")
cat("  project_dir :", project_dir, "  (input root: dta_files)\n")
cat("  output root :", .fte_root,
    if (exists("PROJ_ROOT")) "  (PROJ_ROOT)" else "  (getwd)", "\n")
if (!identical(normalizePath(.fte_root, winslash = "/", mustWork = FALSE),
               normalizePath(project_dir, winslash = "/", mustWork = FALSE)))
  cat("  the two differ, which is expected: inputs and outputs live in\n",
      "  different roots. Check the addendum path below is the one 03c\n",
      "  searches before letting this run.\n", sep = "")
cat("  panel       :", PANEL, "\n")
cat("  writing to  :", FTE_DIR, "  (data)\n")
cat("  run log     :", .fte_logdir, "  (transcript)\n")
cat("  addendum    :", file.path(FTE_DIR, "panel_fte_addendum.rds"), "\n")
cat("  That is the path paths.R calls ADDENDUM and 03c searches for. If it\n")
cat("  is not, stop now rather than after the run.\n")

dir.create(FTE_DIR, showWarnings = FALSE, recursive = TRUE)

# A FIXED FILENAME IS NOT A LOG, IT IS A SCRATCH FILE. This wrote
# fte_log.txt and overwrote it on every run, so there was no record of what
# any earlier run did and nothing to diff when a number moved. Every other
# stage writes <module>_<timestamp>.log and keeps a companion copy; this one
# sat outside that and it showed, because the way to see what 02 had done was
# to have kept the console.
#
# IT DOES NOT USE 03_logging.R, AND A FIRST ATTEMPT THAT DID BROKE 03c.
# That module only opens the sink; 03_finish.R closes it, guarded on
# .t_owns_sink, and 03_finish.R also writes the ledger, which is not this
# script's to write. So 02 could open the transcript and had no way to close
# it. The sink stayed open, and the next module to source 03_logging.R saw a
# live sink, took its "inheriting the caller's sink" branch, and wrote no log
# of its own. 03c then ran for its tens of minutes with its entire output
# appended to 02's file.
#
# A sink this script opens is a sink this script can close, which is the
# whole reason for doing it by hand here. The naming convention matches the
# rest of the pipeline; the companion copy and the accumulators do not exist,
# and that is the price of not owning the closing half of someone else's
# mechanism.
while (sink.number() > 0) sink()
dir.create(.fte_logdir, showWarnings = FALSE, recursive = TRUE)
.fte_log <- file.path(.fte_logdir, paste0(
  "02fte_experience_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
sink(.fte_log, split = TRUE)
cat(strrep("=", 78), "\n02fte_experience.R  started ",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", strrep("=", 78), "\n", sep = "")
cat("  log: ", .fte_log, "\n", sep = "")

# v26. THE LOCAL mde() IS DELETED. It came from 00_contract.R, sealed, and
# defining it here shadowed the sealed copy. td_check_seals() then fired at
# the foot of 00compat.R, inside this script's own guard probe, and the
# probe reported the failure as "00compat.R matched the name but does not
# define the guards". Both halves of that sentence were false: 00compat.R
# defines all three guards, at lines 502, 567 and 895, and it does not
# define mde at all. A seal failure names the file it was called FROM.
#
# THE SEAL WAS RIGHT TO FIRE, and 03_common.R deleted the identical
# definition at v24 for the same reason:
#
#     contract  ifelse(is.finite(se) & se > 0, (z) * se, NA_real_)
#     here      ifelse(is.na(se),  NA_real_,   (z) * se)
#
# so for se = 0 this file returned 0 and the contract returns NA; for
# se = Inf, Inf against NA; for a negative se, a negative MDE against NA. A
# minimum detectable effect of zero grades every benchmark OK, which is the
# direction that turns a power failure into a pass.
#
# POWER and ALPHA are still assigned above and that is fine: they hold the
# same values the contract does, so identical() is TRUE and no seal fires.

.blind <- list()

cat("=====================================================================\n")
cat("FTE EXPERIENCE CONSTRUCTION | revision 5\n")
cat(sprintf("FT week %d hours | cap %.2f | rule %s | recent window %d waves | blind %s\n",
            FT_HOURS, FTE_CAP, WEIGHT_RULE, RECENT_K, BLIND))
cat(sprintf("Gap treatment: %s | primary FTE series %s | one-sided %s\n",
            GAP_TREATMENT, FTE_PRIMARY, ONE_PRIMARY))
# A concave rule needs a ceiling above one to have anything to be concave
# about; below that it IS the hard cap, and a log saying "concave" while a
# cap is applied is worse than no log (G8).
if (identical(WEIGHT_RULE, "concave") && !(is.finite(FTE_CAP) && FTE_CAP > 1)) {
  cat("*** WEIGHT_RULE is 'concave' but FTE_CAP is", FTE_CAP,
      "so the weight is the HARD CAP.\n")
  cat("    The concave branch needs a finite ceiling above one. Reporting the\n")
  cat("    rule as 'cap' from here on so the log matches the arithmetic.\n")
  WEIGHT_RULE <- "cap"
}
cat("=====================================================================\n")

panel <- readRDS(PANEL)

# TAKEN AT READ TIME, NOT AT WRITE TIME. The stamp attached to the addendum
# at the foot of this file has to describe the panel this run actually
# opened. Reading the mtime again after Section E would describe whatever is
# on disk then, and a loader running in another window during a long 02 is
# exactly the case a provenance stamp exists to catch. build_tag is the
# loader's own content marker, so a stamp carrying it can answer whether two
# artefacts came from the same panel rather than merely which is newer.
.pan_mtime_at_read <- file.mtime(PANEL)
.pan_tag_at_read   <- {
  .t <- attr(panel, "build_tag")
  if (is.null(.t) || !length(.t)) NA_character_ else as.character(.t)[1]
}
cat("  panel mtime :", format(.pan_mtime_at_read, "%Y-%m-%d %H:%M:%S"),
    "\n  panel tag   :", .pan_tag_at_read, "\n")
if (is.na(.pan_tag_at_read))
  cat("  the panel carries no build_tag attribute, so the stamp below will\n",
      "  record the path and mtime only. 00compat.R reads a tag, so an\n",
      "  absent one here means this frame did not come from the loader.\n",
      sep = "")

# ---------------------------------------------------------------------
# PANEL FINGERPRINT (R8). Print this block in both 02 and 03 and compare
# the two logs line for line. Agreement on one derived count, such as the
# number of treated persons, is not evidence that two scripts read the
# same file.
# ---------------------------------------------------------------------
fingerprint <- function(path, p) {
  cat("\n--- Panel fingerprint ---\n")
  cat("  path        :", path, "\n")
  cat("  modified    :", format(file.mtime(path)), "\n")
  cat(sprintf("  size        : %.1f MB\n", file.size(path) / 1024^2))
  md5 <- tryCatch(unname(tools::md5sum(path)), error = function(e) NA_character_)
  cat("  md5         :", ifelse(is.na(md5), "not available", md5), "\n")
  cat(sprintf("  rows        : %s   (manuscript %s)\n",
              format(nrow(p), big.mark = ","), format(MS_ROWS, big.mark = ",")))
  cat(sprintf("  persons     : %s   (manuscript %s)\n",
              format(dplyr::n_distinct(p$person_id), big.mark = ","),
              format(MS_PERSONS, big.mark = ",")))
  cat(sprintf("  columns     : %d\n", ncol(p)))
  cat(sprintf("  waves       : %d to %d\n", min(p$wave), max(p$wave)))
  if (nrow(p) == MS_ROWS && dplyr::n_distinct(p$person_id) == MS_PERSONS) {
    cat("  MATCH: this is the panel the manuscript's Data section describes.\n")
    if (is.na(.ms_intro$src)) {
      cat("  data_section_numbers.tex not found, so the introduction's own\n")
      cat("  figures were not checked. Paths tried:\n")
      cat(paste0("    ", .ms_intro$tried, collapse = "\n"), "\n")
    } else if (identical(.ms_intro$rows, nrow(p)) &&
               identical(.ms_intro$persons, dplyr::n_distinct(p$person_id))) {
      cat(sprintf("  The introduction agrees, taking %s across %s from %s.\n",
                  format(.ms_intro$rows, big.mark = ","),
                  format(.ms_intro$persons, big.mark = ","),
                  basename(.ms_intro$src)))
    } else {
      cat(sprintf(paste("  The introduction reports %s across %s, from %s.",
                        "That disagrees\n  with this build and with the Data",
                        "section it should match. The macro\n  file is the",
                        "manuscript's own source, so check it before assuming\n",
                        " the build is wrong.\n"),
                  format(.ms_intro$rows, big.mark = ","),
                  format(.ms_intro$persons, big.mark = ","),
                  basename(.ms_intro$src)))
    }
  } else {
    cat("  NOTE: this build matches neither figure the manuscript gives. Do\n")
    cat("        not quote any number from this run alongside a published one\n")
    cat("        until the sample construction has been reconciled.\n")
  }
  invisible(NULL)
}
fingerprint(PANEL, panel)

# ---------------------------------------------------------------------
# THE ESTIMATION-STAGE GUARDS (H2)
#
# Sourced rather than optional. The restrictions this panel carries are
# not visible in the data: firm size cannot cross wave 22, wave 24 is a
# composition break, interruption enters as a flow, superannuation is a
# household aggregate, wealth is three waves, and several columns are put
# only to new entrants. A script that estimates without them can be
# wrong in ways no error message reports, which is the whole reason the
# file exists, so a missing copy stops the run.
# ---------------------------------------------------------------------
# FOUND BY SEARCH, NOT BY NAME. Revision 4 first wrote this as a literal
# "00_compat_v21.R" and it failed on the first run, because the file is
# saved as 00compat.R. That is exactly the failure 01_discovery_extended.R
# revision 3 fixed for the loader path, made again one file later: a
# script that names a sibling goes wrong the moment the sibling is renamed
# or versioned, and this one carries a version in its name.
#
# The version belongs INSIDE the file, in COMPAT_BUILD_TAG, which is
# checked against the panel a few lines below. So the search is loose on
# the name and strict on the contents: any file matching compat is a
# candidate, the newest is taken, and it is only accepted if it defines
# the functions a guard file must define.
GUARD_PATTERNS <- c("^00[_]?compat.*\\.[Rr]$", "compat.*\\.[Rr]$")
GUARD_MUST_DEFINE <- c("compat_panel_check", "require_cols",
                       "assert_no_olf_level")
.guard_dirs <- unique(c(getwd(), project_dir, dirname(PANEL),
                        file.path(project_dir, "R")))
.guard_dirs <- .guard_dirs[dir.exists(.guard_dirs)]
.guard_hits <- unique(unlist(lapply(.guard_dirs, function(d)
  unlist(lapply(GUARD_PATTERNS, function(p)
    list.files(d, pattern = p, full.names = TRUE))))))
.guard_hits <- unique(normalizePath(.guard_hits, winslash = "/",
                                    mustWork = FALSE))
if (length(.guard_hits) > 1L)
  .guard_hits <- .guard_hits[order(file.mtime(.guard_hits), decreasing = TRUE)]
if (length(.guard_hits)) {
  cat("\n--- Guard file candidates (newest first) ---\n")
  print(data.frame(file = basename(.guard_hits),
                   modified = as.character(file.mtime(.guard_hits)),
                   row.names = NULL), row.names = FALSE, right = FALSE)
}
# Each candidate is PROBED in a throwaway environment before it is
# sourced for real. Two reasons. A file that matches the name and then
# throws would otherwise take the run down with it, and exists() consulted
# in the global environment would accept a decoy outright whenever the
# guards happened to be loaded already from an earlier source() in the
# same session.
# v26. THE PROBE ASKED TWO QUESTIONS AND REPORTED ONE ANSWER. It read
#
#   .ok <- tryCatch({ ... sys.source(...) ...
#                     all(vapply(GUARD_MUST_DEFINE, exists, ...)) },
#                   error = function(e) FALSE)
#
# so a candidate that THREW and a candidate that LACKS THE GUARDS both
# came out FALSE, and the message below blamed the contents either way.
# The verdict is unchanged and both still reject. What changes is that the
# two are told apart and the error is printed in full.
#
# THE REJECTION IS DELIBERATELY KEPT. An earlier version of this patch
# accepted a file that threw, on the reasoning that the guards were bound
# and a late failure elsewhere should not stop the run. Applied to the
# case that prompted the patch it would have suppressed a correct seal
# failure and let this script estimate with a shadowed mde(). A guard file
# that cannot load cleanly is a session that cannot be trusted, whatever
# is bound in it.
.guard_used  <- NA_character_
.guard_error <- NULL

for (.g in .guard_hits) {
  .probe <- new.env(parent = globalenv())
  .err   <- NULL

  # Captured, not converted to a verdict. sys.source stops at the failing
  # expression, so what was bound before it is still inspectable and the
  # report can say how far the file got.
  tryCatch(invisible(capture.output(sys.source(.g, envir = .probe))),
           error = function(e) .err <<- conditionMessage(e))

  # inherits = FALSE stays, for the reason the original comment gives:
  # consulting the global environment would accept a decoy whenever the
  # guards happened to be loaded already from an earlier source() in the
  # same session. It also means pre-sourcing the guard file by hand is not
  # a workaround for a probe failure.
  .have <- vapply(GUARD_MUST_DEFINE, exists, logical(1),
                  envir = .probe, inherits = FALSE)

  if (is.null(.err) && all(.have)) {
    source(.g)
    .guard_used <- .g
    break
  }

  cat("\n  REJECTED ", basename(.g), "\n", sep = "")
  if (!is.null(.err)) {
    cat("    it threw: ", .err, "\n", sep = "")
    cat("    bound before it stopped: ", sum(.have), " of ",
        length(GUARD_MUST_DEFINE), " guards, ",
        length(ls(.probe, all.names = TRUE)), " objects in total\n", sep = "")
    if (all(.have)) {
      cat("    All the guards WERE defined, so the contents are not the\n")
      cat("    problem. Read the error as being about the session rather\n")
      cat("    than about this file.\n")
      if (grepl("sealed object", .err, fixed = TRUE))
        cat("    A seal failure names the file it was called FROM, not the\n",
            "    file that redefined the object. Check whether THIS script\n",
            "    defines it above the probe before looking anywhere else.\n",
            sep = "")
    } else {
      cat("    missing: ", paste(GUARD_MUST_DEFINE[!.have], collapse = ", "),
          "\n", sep = "")
    }
  } else {
    cat("    loaded cleanly but does not define ",
        paste(GUARD_MUST_DEFINE[!.have], collapse = ", "), ".\n", sep = "")
  }
  .guard_error <- .err
}
if (exists(".probe")) rm(.probe)
if (exists(".err"))   rm(.err)
if (exists(".have"))  rm(.have)
if (is.na(.guard_used)) {
  cat("\n*** GUARD FILE NOT FOUND ***\n")
  cat("  looked for a file matching", paste(GUARD_PATTERNS, collapse = " or "),
      "\n  defining", paste(GUARD_MUST_DEFINE, collapse = ", "),
      "\n  in:\n")
  for (d in .guard_dirs) cat("   ", d, "\n")
  if (length(.guard_hits)) {
    cat("  Files matched the name but were rejected:",
        paste(basename(.guard_hits), collapse = ", "), "\n")
    # "not the contents" was wrong whenever a candidate threw, and it is
    # the sentence that sent the last search into the wrong file.
    if (!is.null(.guard_error))
      cat("  The last one failed with:", .guard_error, "\n")
  }
  stop("A compat guard file is required. It holds the restrictions this ",
       "panel cannot express, and estimating without it is the failure it ",
       "was written to prevent. Save the one from the loader's directory ",
       "beside this script under any name containing compat.", call. = FALSE)
}
cat("  guards      :", .guard_used, "\n")
panel <- compat_panel_check(panel)

# v27. THE ESTIMATION SCREEN, APPLIED HERE AND NOT BEFORE.
#
# WHY THIS MATTERS MORE THAN THE PATH. Line 639 reads the panel with a bare
# readRDS, so this script has been running on the raw 134,131-row extract
# while 03 runs on the 129,544 the contract screens to on resolve. The
# addendum written at the foot inherits that: 133,911 rows against the
# 129,325 a screened build produces. Every stock in it is accumulated over
# a person's whole observed span, so a row the screen removes still
# contributes to the stock every later row carries, and 03c's inner_join
# cannot withdraw that contribution. Fixing the destination WITHOUT fixing
# this would have written a wrong-frame addendum over the only
# frame-consistent one on disk, which is why the two changes ship together.
#
# td_frame() IS THE CANONICAL ROUTE and is preferred when the contract has
# supplied it; the explicit rule is the fallback, and it is the same rule
# Section 5.6 states: age minus years of experience between ten and
# thirty-five. Either way the count is printed, because a screen that
# removes a different number than expected is a finding.
# v29. td_frame(panel) WAS A MISCALL, AND THE FALLBACK CONCEALED IT.
# td_frame's first argument is need_cols, not a frame, so this errored on
# every run. The log said so and the message was read as a routing problem:
# "td_frame() failed (`x` and `y` are not compatible...); falling back to the
# explicit rule." The explicit rule then screened at a hard-coded 10 to 35
# and the answer matched the contract by coincidence, for as long as the
# contract's bounds were 10 and 35. They are settings now, so a sensitivity
# run would have screened this stage at the default while every other stage
# used the new bound, and the addendum 03c reads would have come from a
# different sample than the arms fitted on it.
#
# AN ERROR HERE STOPS THE RUN. The fallback existed to keep a standalone run
# working without a contract, and that case is kept below. What is removed is
# the path where the contract IS loaded and its rule fails silently.
.n_raw <- nrow(panel)
# v30, 14 October. NO SCREEN BY DEFAULT. The contract's rule is "none" unless
# TD_SCREEN=on, and the hand rule below follows the same setting, so a run
# without the contract cannot screen while the rest of the pipeline does not.
.scr_on <- if (exists("TD_SCREEN_ON")) isTRUE(TD_SCREEN_ON) else
  identical(tolower(trimws(Sys.getenv("TD_SCREEN", "off"))), "on")
if (exists("td_apply_sample_rule", mode = "function")) {
  panel <- td_apply_sample_rule(panel)
} else if (.scr_on && all(c("age", "experience_years") %in% names(panel))) {
  .lo <- suppressWarnings(as.numeric(Sys.getenv("TD_SAMPLE_LO", "")))
  .hi <- suppressWarnings(as.numeric(Sys.getenv("TD_SAMPLE_HI", "")))
  if (is.na(.lo)) .lo <- 10
  if (is.na(.hi)) .hi <- 35
  .gap  <- panel$age - panel$experience_years
  .keep <- !is.na(.gap) & .gap >= .lo & .gap <= .hi
  panel <- panel[.keep, , drop = FALSE]
  cat(sprintf("  no contract: screen applied by hand, %g to %g.\n", .lo, .hi))
  rm(.gap, .keep, .lo, .hi)
} else if (!.scr_on) {
  cat("  no contract, and no screen: TD_SCREEN is off.\n")
}
cat(sprintf("\nEstimation screen: %s of %s rows retained (%s removed).\n",
            format(nrow(panel), big.mark = ","),
            format(.n_raw,      big.mark = ","),
            format(.n_raw - nrow(panel), big.mark = ",")))
if (.scr_on && nrow(panel) == .n_raw)
  cat("  *** NOTHING WAS REMOVED. Neither td_frame() nor age and\n",
      "      experience_years were available, so this run is on the raw\n",
      "      extract and the addendum it writes will NOT match 03's frame.\n",
      "      Do not let 03c read it.\n", sep = "")
rm(.n_raw)
if (!is.null(attr(panel, "restrictions"))) {
  cat("\n--- Restrictions this panel carries ---\n")
  print(attr(panel, "restrictions"), row.names = FALSE, right = FALSE)
}
.eo <- attr(panel, "entrant_only_cols")
if (length(.eo))
  cat("  Entrant-only columns measured by the loader:",
      paste(.eo, collapse = ", "), "\n")
rm(.eo)

need <- c("person_id", "wave", "female", "ever_parent", "employed",
          "hours_worked_clean", "experience_years", "actual_experience",
          "years_not_in_lf", "ln_hourly_wage_real")
# H2: require_cols rather than a setdiff, because a column that is
# present and ALL-NA is a missing variable and used to pass this check.
require_cols(panel, need, context = "02 inputs")
if (!"fulltime" %in% names(panel)) {
  cat("NOTE: fulltime absent; deriving it as hours >= 35.\n")
  panel$fulltime <- ifelse(is.na(panel$hours_worked_clean), NA_real_,
                           as.numeric(panel$hours_worked_clean >= 35))
}

# Cohort assignment is needed early, because the weighting sensitivity in
# Section B is reported on the event-time gap and not only on the mean
# terminal stock. Same rule as 02_cs_event_study.R and 03: treatment is
# the wave the absorbing ever_parent indicator turns 0 -> 1, never-treated
# are the always-childless, left-censored parents are dropped.
coh <- panel %>%
  filter(!is.na(ever_parent)) %>%
  arrange(person_id, wave) %>%
  group_by(person_id) %>%
  summarise(first_wave = min(wave),
            # ALREADY A PERSON-LEVEL SUMMARY, so this is not the
            # wave-varying trap. `ever` is max(ever_parent) within person,
            # which is the lifetime status; `ep_at_entry` and `birth_wave`
            # need the value AT a wave and would be wrong on anything else.
            # The summarise() collapses to one row per person before any
            # grouping happens, which is what a cohort definition requires
            # and what a cross-sectional cut on the raw column does not do.
            ep_at_entry = ever_parent[which.min(wave)],
            ever = max(ever_parent, na.rm = TRUE),
            birth_wave = if (any(ever_parent == 1))
              min(wave[ever_parent == 1]) else NA_integer_,
            .groups = "drop") %>%
  filter(ep_at_entry == 0) %>%
  mutate(cohort = ifelse(ever == 0, 10000L, as.integer(birth_wave)))

# =====================================================================
# SECTION A. THE FTE WEIGHT
# =====================================================================
cat("\n--- A. FTE weight ---\n")

# Two rules (R3).
#   cap      w = min(h/ft, cap). Simple, but it piles mass exactly on the
#            ceiling and the ceiling is a free parameter.
#   concave  w = h/ft below a full-time week; above it, 1 + s(1 - exp(-(r-1)/s))
#            with s = cap - 1. Value and slope both continuous at r = 1,
#            strictly increasing, strictly concave above 1, asymptotic to
#            cap. Long hours are discounted rather than truncated, and the
#            part-time region where the substantive result lives is
#            untouched.
fte_weight <- function(h, employed, ft = FT_HOURS, cap = FTE_CAP,
                       rule = WEIGHT_RULE) {
  r <- ifelse(is.na(employed), NA_real_,
              ifelse(employed == 0, 0,
                     ifelse(is.na(h), NA_real_, h / ft)))
  if (identical(rule, "concave") && is.finite(cap) && cap > 1) {
    s <- cap - 1
    ifelse(is.na(r), NA_real_,
           ifelse(r <= 1, r, 1 + s * (1 - exp(-(r - 1) / s))))
  } else {
    pmin(r, cap)
  }
}

# Both weights are built here, not one here and one in Section B, because
# the wedge table below has to be shown under each of them (G11). one_w is
# the same construction with the ceiling at one: it discounts a part-time
# year and credits no overtime, and it is the weight the paper's hypothesis
# names. It is exactly the cap = 1.00 cell of the B2 grid (G5).
panel <- panel %>%
  arrange(person_id, wave) %>%
  mutate(fte_w   = fte_weight(hours_worked_clean, employed),
         one_w   = fte_weight(hours_worked_clean, employed,
                              ft = FT_HOURS, cap = 1, rule = "cap"),
         raw_fte = ifelse(is.na(employed), NA_real_,
                          ifelse(employed == 0, 0,
                                 ifelse(is.na(hours_worked_clean), NA_real_,
                                        hours_worked_clean / FT_HOURS))))

cat("Distribution of the FTE weight among the employed:\n")
print(as.data.frame(
  panel %>% filter(employed == 1, !is.na(fte_w)) %>%
    group_by(female) %>%
    summarise(n = n(),
              p10 = round(unname(stats::quantile(fte_w, .10)), 2),
              p50 = round(unname(stats::quantile(fte_w, .50)), 2),
              p90 = round(unname(stats::quantile(fte_w, .90)), 2),
              mean_two_sided = round(mean(fte_w), 3),
              mean_one_sided = round(mean(one_w), 3),
              pct_above_ft_week = round(100 * mean(raw_fte > 1), 1),
              pct_at_ceiling = round(100 * mean(raw_fte >= FTE_CAP - 1e-9), 1),
              .groups = "drop")), row.names = FALSE)
cat("mean_two_sided above one means the weight is CREDITING overtime, not\n")
cat("only discounting part-time work. mean_one_sided cannot exceed one by\n")
cat("construction, and the difference between the two columns is the\n")
cat("credit (G11).\n")
cat("pct_at_ceiling is the share the hard cap binds on. Where that share\n")
cat("is large the cap is not a technical guard, it is a modelling choice\n")
cat("with a first-order effect on the headline gap; see the grid in B.\n")

# Employed waves with missing hours cannot be given an FTE weight and
# also break the calendar = full-time + part-time identity used in D.
# Reported here so the residual in that identity is not a surprise.
cat(sprintf("\nEmployed person-waves with hours missing: %s (%.2f%%)\n",
            format(sum(panel$employed == 1 & is.na(panel$hours_worked_clean),
                       na.rm = TRUE), big.mark = ","),
            100 * mean(is.na(panel$hours_worked_clean[panel$employed == 1]),
                       na.rm = TRUE)))
cat(sprintf("Employed person-waves with fulltime missing: %s (%.2f%%)\n",
            format(sum(panel$employed == 1 & is.na(panel$fulltime),
                       na.rm = TRUE), big.mark = ","),
            100 * mean(is.na(panel$fulltime[panel$employed == 1]),
                       na.rm = TRUE)))

# The gap between the calendar weight (1 for anyone employed) and the FTE
# weight IS the mismeasurement, per person-wave.
wedge <- panel %>%
  filter(employed == 1, !is.na(fte_w), !is.na(ever_parent)) %>%
  group_by(female, ever_parent) %>%
  summarise(n = n(),
            mean_calendar_weight = 1,
            mean_fte_weight = round(mean(fte_w), 3),
            mean_one_weight = round(mean(one_w), 3),
            wedge_two_sided = round(1 - mean(fte_w), 3),
            wedge_one_sided = round(1 - mean(one_w), 3),
            # Unrounded, carried for the arithmetic below and dropped from
            # the printed table (G19).
            mean_fte_raw = mean(fte_w),
            mean_one_raw = mean(one_w),
            .groups = "drop") %>%
  # Retained under its revision-2 name so nothing downstream that reads the
  # csv breaks, but the one-sided column beside it is the one the mechanism
  # sentence should be written from (G11).
  mutate(wedge_per_employed_year = wedge_two_sided)
cat("\nPer-employed-year wedge between the calendar and FTE weights:\n")
print(as.data.frame(wedge %>% select(-mean_fte_raw, -mean_one_raw)),
      row.names = FALSE)
write.csv(wedge, file.path(FTE_DIR, "A_fte_wedge.csv"), row.names = FALSE)

wp <- wedge %>% filter(ever_parent == 1)
if (nrow(wp) == 2) {
  # Named for what they hold (G7). Revision 2 called the female value wm and
  # the male value wf; the printed sentence was right and the names were
  # inverted, which stays correct only until someone edits the line.
  # From the UNROUNDED means, not by differencing the display columns (G19).
  # Revision 3.1 differenced wedge_two_sided, which is rounded to three
  # decimals, and printed 0.267 where A2 computes 0.268 for the same quantity
  # on the same rows. Two figures in one log that are meant to be the same
  # number must not differ because one of them went through round().
  wedge_mother <- 1 - wp$mean_fte_raw[wp$female == 1]
  wedge_father <- 1 - wp$mean_fte_raw[wp$female == 0]
  one_mother   <- 1 - wp$mean_one_raw[wp$female == 1]
  one_father   <- 1 - wp$mean_one_raw[wp$female == 0]
  d_two <- wedge_mother - wedge_father
  d_one <- one_mother - one_father
  cat("\nAmong parents, per year both are employed:\n")
  cat(sprintf("  two-sided weight: mother %+.3f, father %+.3f, differential %.3f\n",
              wedge_mother, wedge_father, d_two))
  cat(sprintf("  one-sided weight: mother %+.3f, father %+.3f, differential %.3f\n",
              one_mother, one_father, d_one))
  cat("A POSITIVE wedge is what ehtjb overstates. A NEGATIVE one is what it\n")
  cat("understates, which can only happen under the two-sided weight and\n")
  cat("only through the overtime credit.\n")
  # WHICH OF THE TWO THE MECHANISM SENTENCE SHOULD QUOTE (G11). The paper's
  # claim is that a mother's calendar year holds fewer hours than a
  # father's. Any part of the differential that comes from crediting HIS
  # long weeks above one is not that claim, and it is separable: it is
  # exactly the two-sided differential less the one-sided one.
  if (is.finite(d_two) && abs(d_two) > 1e-9) {
    # This share is of a PURELY INTENSIVE quantity: the wedge is per year
    # both are employed, so the extensive margin is conditioned out of it
    # before the share is taken. That matters for the cross-check in
    # Section D, which has to take its share of the intensive margin and not
    # of a ratio that carries the extensive margin as well (G17).
    credit_share <- (d_two - d_one) / d_two
    cat(sprintf(paste("The two differ by %.3f, which is %.0f%% of the two-sided",
                      "figure and is\nthe overtime credit to long male weeks",
                      "rather than the discount to\nshort female ones. The",
                      "wedge is per employed year, so this is a share\nof the",
                      "INTENSIVE margin; Section D reports the same share and",
                      "has to\nnormalise it the same way.\n"),
                d_two - d_one, 100 * credit_share))
    if (wedge_father < 0)
      cat(sprintf(paste("Note the sign: under the two-sided weight a father's",
                        "employed year is\ncredited at %.3f, ABOVE a calendar",
                        "year. Quote the one-sided\ndifferential of %.3f in the",
                        "mechanism sentence, and the two-sided\nfigure only",
                        "beside it with the credit named.\n"),
                  1 - wedge_father, d_one))
    if (credit_share > 0.20)
      cat("More than a fifth of the differential is the credit, so the two are\n",
          " not interchangeable in the text.\n")
  }
  cat("Either way this is arithmetic rather than estimated, and it is the\n")
  cat("mechanical source of everything in Section D.\n")
  # Carried so Section D can print the comparison rather than asking the
  # reader to make it across forty pages of log (G17).
  A_CREDIT_SHARE <- if (is.finite(d_two) && abs(d_two) > 1e-9)
    (d_two - d_one) / d_two else NA_real_
} else {
  A_CREDIT_SHARE <- NA_real_
}

# ---------------------------------------------------------------------
# A2. IS THE WEDGE GENDERED WITHIN STATE, OR ONLY THROUGH OCCUPANCY?
#
# This matters for how the measurement error has to be described. If the
# per-year wedge is the same for mothers and fathers conditional on
# working full-time or part-time, then ehtjb is not a gender-biased
# instrument in any direct sense; it is an unbiased instrument applied to
# a state distribution that differs sharply by sex. The error is then
# non-classical purely through composition, which is both the weaker
# claim about the survey and the stronger claim about the mechanism.
#
# Exact two-way decomposition at midpoint weights, so it closes with no
# residual:
#   wedge_f - wedge_m = SUM_s (p_fs - p_ms) wbar_s   composition
#                     + SUM_s pbar_s (w_fs - w_ms)   within state
# ---------------------------------------------------------------------
ws <- panel %>%
  filter(employed == 1, !is.na(fte_w), !is.na(fulltime),
         !is.na(ever_parent), ever_parent == 1) %>%
  mutate(state = ifelse(fulltime == 1, "full-time", "part-time")) %>%
  group_by(female, state) %>%
  summarise(n = n(), w = 1 - mean(fte_w), .groups = "drop") %>%
  group_by(female) %>% mutate(p = n / sum(n)) %>% ungroup()

cat("\nPer-year wedge and occupancy share by state, parents only:\n")
print(as.data.frame(ws %>% mutate(w = round(w, 3), p = round(p, 3))),
      row.names = FALSE)

sts <- intersect(ws$state[ws$female == 1], ws$state[ws$female == 0])
if (length(sts) >= 2) {
  gv <- function(fem, col) {
    x <- ws[[col]][ws$female == fem]
    stats::setNames(x, ws$state[ws$female == fem])[sts]
  }
  pf <- gv(1, "p"); wf2 <- gv(1, "w"); pm <- gv(0, "p"); wm2 <- gv(0, "w")
  comp   <- sum((pf - pm) * (wf2 + wm2) / 2)
  within <- sum((pf + pm) / 2 * (wf2 - wm2))
  tot    <- sum(pf * wf2) - sum(pm * wm2)
  cat(sprintf("\nDifferential %.3f decomposes into\n", tot))
  cat(sprintf("  composition, mothers being in part-time work more often : %+.3f\n",
              comp))
  cat(sprintf("  within state, the same state measured differently by sex: %+.3f\n",
              within))
  cat(sprintf("  residual (exact by construction)                        : %+.6f\n",
              tot - comp - within))
  # Shares only where the denominator can carry them (G6). A differential
  # near zero makes both percentages arbitrarily large with no warning, and
  # the two would still sum to 100 while meaning nothing.
  if (abs(tot) > 0.005) {
    sh_comp <- 100 * comp / tot
    sh_within <- 100 * within / tot
    cat(sprintf("  as shares of the differential: composition %.0f%%, within %.0f%%\n",
                sh_comp, sh_within))
    # The conclusion is written per branch and grounded in the per-state
    # numbers, not assembled from two cat() calls that only fit together on
    # one path (G12). The within-state differences are printed because the
    # sentence "the same state is measured the same way for both sexes" is a
    # claim about them and can be checked against them.
    ws_diff <- wf2 - wm2
    cat("  within-state differences, women less men, by state:\n")
    for (s in sts)
      cat(sprintf("    %-10s %+.3f\n", s, ws_diff[[s]]))
    if (sh_within < 10) {
      cat("  Composition carries essentially all of it. ehtjb measures a given\n")
      cat("  state almost identically for the two sexes, and the differential\n")
      cat("  is that mothers occupy the part-time state far more often. That is\n")
      cat("  a claim about a state distribution rather than about a gendered\n")
      cat("  instrument, and it is the harder one to argue with.\n")
    } else if (sh_comp > sh_within) {
      cat("  Composition carries most of it but NOT all: the within-state term\n")
      cat(sprintf("  is %.0f%% and the per-state differences above are not zero.\n",
                  sh_within))
      cat("  So the accurate sentence is that most of the differential is\n")
      cat("  mothers occupying the part-time state more often, with a smaller\n")
      cat("  residual from the same state being measured differently by sex.\n")
      cat("  Do NOT write that ehtjb measures a part-time year the same way for\n")
      cat("  both sexes; the table does not say that.\n")
    } else {
      cat("  The within-state term carries most of it, so the differential is\n")
      cat("  NOT mainly a composition effect: the same labour-market state is\n")
      cat("  measured differently by sex. That is the stronger claim about the\n")
      cat("  survey instrument and the weaker one about the mechanism, and it\n")
      cat("  is the opposite of what the section was drafted to say. Check the\n")
      cat("  per-state differences above before writing either version.\n")
    }
  } else {
    cat("  The differential is too close to zero to express the two terms as\n")
    cat("  shares of it; report them in levels only. A share of a near-zero\n")
    cat("  total is arbitrarily large and the two would still sum to 100%.\n")
  }
}

# =====================================================================
# SECTION B. IN-PANEL ACCUMULATION SPINE
#
# Cumulative stocks over the person's observed span. Interior wave gaps
# are a real problem: a stock that skips an unobserved wave understates
# accumulation. Every stock is therefore built in both gap treatments,
# _obs crediting a gap with nothing and _int crediting it with the linear
# interpolation of the weight, and the difference is reported rather than
# assumed away.
#
# Three weights, not two (G2). The calendar weight is 1 for anyone
# employed. The two-sided weight is the WEIGHT_RULE weight, which
# discounts a part-time year AND credits an overtime year up to FTE_CAP.
# The one-sided weight caps at one, so it discounts part-time and credits
# nothing, and it is the weight the paper's hypothesis actually names. It
# is built here rather than in 03 so that there is one definition of it.
#
# NAMING. exp_pt_obs in this script is the count of PART-TIME CALENDAR
# YEARS and has nothing to do with the one-sided weight. The one-sided
# stocks are exp_one_obs and exp_one_int. Do not introduce a third meaning
# for pt.
# =====================================================================
cat("\n--- B. In-panel accumulation ---\n")

span <- panel %>%
  group_by(person_id) %>%
  summarise(w0 = min(wave), w1 = max(wave), n_obs = n(), .groups = "drop") %>%
  mutate(n_span = w1 - w0 + 1L, n_gap = n_span - n_obs)

cat(sprintf("Persons: %s | with interior wave gaps: %s (%.1f%%)\n",
            format(nrow(span), big.mark = ","),
            format(sum(span$n_gap > 0), big.mark = ","),
            100 * mean(span$n_gap > 0)))

# one_w was built in Section A, alongside fte_w, so that the wedge table
# there could be shown under both weights (G11). It is not rebuilt here.
len  <- span$n_span
grid <- data.frame(person_id = rep(span$person_id, len),
                   wave      = sequence(len, from = span$w0),
                   stringsAsFactors = FALSE) %>%
  # An explicit marker rather than a duplicated key: selecting person_id
  # twice under two names breaks the join, and testing !is.na() on any
  # substantive column would confuse "not interviewed" with "interviewed and
  # this item missing", which is the very distinction G3 is drawing.
  left_join(panel %>% select(person_id, wave, fte_w, one_w, employed,
                             fulltime) %>% mutate(in_panel = 1L),
            by = c("person_id", "wave")) %>%
  mutate(in_panel = as.integer(!is.na(in_panel)))

# Y6. What is observed by construction is a PANEL ROW, not a non-missing
# weight. A person whose first or last panel wave is employed with hours
# missing has no weight there, so rule = 2 extrapolates flat from the
# nearest observed weight, and a person with exactly one observed weight
# has it replicated across the whole span. Both feed exp_fte_int, and D3's
# depreciation split rests entirely on the interpolated stock. It is 220
# rows in this build; the comment used to deny that it happens at all.
interp_one <- function(w, y) {
  ok <- !is.na(y)
  if (!any(ok)) return(y)
  if (sum(ok) == 1L) return(rep(y[ok][1], length(y)))
  # rule = 2 holds the end values flat. Inside a person's span the first
  # and last waves are observed by construction, so this never extrapolates
  # beyond the data; it only fills interior gaps.
  stats::approx(w[ok], y[ok], xout = w, rule = 2)$y
}

grid <- grid %>%
  arrange(person_id, wave) %>%
  group_by(person_id) %>%
  mutate(
    # TWO FLAGS, NOT ONE (G3). Revision 2 had a single `observed`, defined
    # as !is.na(fte_w), which is FALSE for a wave the person was not
    # interviewed in AND for a wave they were interviewed in while employed
    # with hours missing. The addendum filters on it, so the second group was
    # dropped from the addendum, from Section D here and from every arm of
    # T8 in 03, without ever appearing as a restriction in a row count.
    #   in_panel    the person has a row in the panel for this wave
    #   weight_obs  the FTE weight could be computed for this wave
    weight_obs = as.integer(!is.na(fte_w)),
    observed   = weight_obs,   # retained so nothing downstream breaks
    fte_obs    = coalesce(fte_w, 0),                 # gaps contribute nothing
    fte_int    = interp_one(wave, fte_w),            # gaps interpolated
    one_obs    = coalesce(one_w, 0),
    one_int    = interp_one(wave, one_w),
    exp_fte_obs = cumsum(fte_obs),
    exp_fte_int = cumsum(coalesce(fte_int, 0)),
    # The one-sided stocks (G2), on the same grid and the same two gap
    # treatments as everything else, so 03 does not have to rebuild them and
    # cannot rebuild them differently.
    exp_one_obs = cumsum(one_obs),
    exp_one_int = cumsum(coalesce(one_int, 0)),
    exp_cal_obs = cumsum(coalesce(as.numeric(employed), 0)),
    exp_ft_obs  = cumsum(coalesce(as.numeric(employed == 1 & fulltime == 1), 0)),
    exp_pt_obs  = cumsum(coalesce(as.numeric(employed == 1 & fulltime == 0), 0)),
    # Employed waves with hours missing. Carried so that
    # exp_cal_obs = exp_ft_obs + exp_pt_obs + exp_ftna_obs holds exactly
    # and the decomposition in D closes to machine precision (R4).
    # Y4: employed with FULL-TIME STATUS missing, which is what closes the
    # identity, and not employed with hours missing, which is the 220-row
    # addendum restriction reported in B. In this build the first is
    # empty and the second is not.
    exp_ftna_obs = cumsum(coalesce(as.numeric(employed == 1 & is.na(fulltime)), 0)),
    waves_in    = row_number(),
    waves_seen  = cumsum(weight_obs)
  ) %>%
  # Recency split: the last RECENT_K waves against everything before.
  # Depreciation implies recent stock prices higher than distant stock,
  # and a mother's stock is disproportionately distant after a spell.
  mutate(
    roll_recent = exp_fte_int - dplyr::lag(exp_fte_int, n = RECENT_K,
                                           default = 0),
    exp_fte_recent  = roll_recent,
    exp_fte_distant = exp_fte_int - roll_recent
  ) %>%
  ungroup()

# WHAT THE ADDENDUM'S FILTER COSTS, STATED AS A ROW COUNT (G3).
n_in_panel   <- sum(grid$in_panel == 1)
n_weight_obs <- sum(grid$weight_obs == 1)
n_lost       <- n_in_panel - sum(grid$in_panel == 1 & grid$weight_obs == 1)
cat(sprintf(paste("\nGrid rows: %s spanned, %s of them present in the panel,",
                  "%s with a\ncomputable FTE weight.\n"),
            format(nrow(grid), big.mark = ","),
            format(n_in_panel, big.mark = ","),
            format(n_weight_obs, big.mark = ",")))
cat(sprintf(paste("The addendum keeps rows with a computable weight, so %s",
                  "person-waves that\nARE in the panel are dropped from it",
                  "(%.2f%% of the panel rows in span).\n"),
            format(n_lost, big.mark = ","),
            100 * n_lost / max(1L, n_in_panel)))
if (n_lost > 0) {
  cat("Those are waves where employment is unknown, or where the person was\n")
  cat("employed with hours missing. That is not a random subset: it is\n")
  cat("selected on the hours variable the weight is built from. Section D\n")
  cat("here and every arm of T8 in 03 inherit the restriction through the\n")
  cat("addendum, so it is a sample definition and belongs in the row\n")
  cat("accounting rather than in a footnote.\n")
  lost_by <- grid %>% filter(in_panel == 1, weight_obs == 0) %>%
    left_join(panel %>% select(person_id, wave, female), by = c("person_id", "wave")) %>%
    count(female, employed)
  if (nrow(lost_by)) {
    cat("Composition of the dropped rows:\n")
    print(as.data.frame(lost_by), row.names = FALSE)
  }
}

# Orderings that hold by construction. The one-sided stock cannot exceed the
# two-sided one at the same gap treatment, and neither can exceed the
# calendar stock when both are gap-blind. Asserted rather than assumed,
# because every contrast in Section D and in 03's T8 rests on them.
# The maximum of the difference confirms the ORDERING and says nothing about
# MAGNITUDE, because most rows sit at zero on both sides of each comparison
# and the max is then taken over a mass of exact ties (G15). The share of
# rows where the two differ at all, and the mean difference among those, are
# what say whether the two stocks are actually distinguishable.
# viol is the check. The three columns after it are the magnitude, which the
# check itself does not give: the maximum of the difference is exactly zero
# whenever the ordering holds and any rows are tied, so it repeated viol and
# is gone (G20). mean and median together say whether the typical gap between
# two stocks is like the average or the average is carried by a tail, and
# differ says on how many rows the two are distinguishable at all.
ord_chk <- function(a, b, la, lb) {
  d <- grid[[a]] - grid[[b]]
  v <- sum(d > 1e-8, na.rm = TRUE)
  nz <- abs(d) > 1e-8 & !is.na(d)
  ad <- abs(d[nz])
  cat(sprintf(paste("  %-40s viol %s | differ %.1f%% | if differ: mean %.3f",
                    "med %.3f\n"),
              paste(la, "<=", lb), format(v, big.mark = ","),
              100 * mean(nz, na.rm = TRUE),
              if (length(ad)) mean(ad) else 0,
              if (length(ad)) stats::median(ad) else 0))
  if (v > 0)
    cat("    *** That is impossible by construction. Do not use either stock\n",
        "    until the weight or the accumulation grid is fixed.\n")
  invisible(v)
}
cat("\nConstruction checks on the accumulation spine:\n")
ord_chk("exp_one_obs", "exp_fte_obs", "one-sided obs", "two-sided obs")
ord_chk("exp_one_int", "exp_fte_int", "one-sided int", "two-sided int")
ord_chk("exp_one_obs", "exp_cal_obs", "one-sided obs", "calendar obs")
ord_chk("exp_fte_obs", "exp_fte_int", "two-sided obs", "two-sided int")
cat("The last is the gap credit and is the quantity that has to be held\n")
cat("fixed whenever two weights are compared. A pair that differs on almost\n")
cat("no rows is a pair the data cannot tell apart, whatever the ordering\n")
cat("check says.\n")

gap_effect <- grid %>%
  filter(weight_obs == 1) %>%
  group_by(person_id) %>%
  slice(n()) %>%
  ungroup() %>%
  summarise(n = n(),
            med_obs = round(stats::median(exp_fte_obs), 2),
            med_int = round(stats::median(exp_fte_int), 2),
            med_diff = round(stats::median(exp_fte_int - exp_fte_obs), 3),
            p95_diff = round(unname(stats::quantile(exp_fte_int - exp_fte_obs,
                                                    .95)), 3),
            .groups = "drop")
cat("\nTerminal in-panel FTE stock, observed-only against interpolated:\n")
print(as.data.frame(gap_effect), row.names = FALSE)

# BY SEX, because a median near zero over everyone is not the same as a gap
# credit that is equal across the sexes, and only the second licenses
# ignoring the gap treatment (G1). Interior wave gaps are correlated with
# motherhood, so this is the quantity to look at.
gap_by_sex <- grid %>%
  filter(weight_obs == 1) %>%
  left_join(panel %>% select(person_id, wave, female),
            by = c("person_id", "wave")) %>%
  filter(!is.na(female)) %>%
  group_by(person_id, female) %>% slice(n()) %>% ungroup() %>%
  group_by(female) %>%
  summarise(persons = n(),
            mean_credit = round(mean(exp_fte_int - exp_fte_obs), 4),
            med_credit  = round(stats::median(exp_fte_int - exp_fte_obs), 4),
            p95_credit  = round(unname(stats::quantile(
              exp_fte_int - exp_fte_obs, .95)), 3),
            pct_any     = round(100 * mean(exp_fte_int - exp_fte_obs > 1e-8), 1),
            .groups = "drop")
cat("\nTerminal gap credit, interpolated less observed-only, by sex:\n")
print(as.data.frame(gap_by_sex), row.names = FALSE)
write.csv(gap_by_sex, file.path(FTE_DIR, "B_gap_credit_by_sex.csv"),
          row.names = FALSE)
if (nrow(gap_by_sex) == 2) {
  gc_diff <- gap_by_sex$mean_credit[gap_by_sex$female == 1] -
    gap_by_sex$mean_credit[gap_by_sex$female == 0]
  cat(sprintf("Difference, women less men: %+.4f years.\n", gc_diff))
  cat("Any comparison between a gap-blind stock and an interpolated one\n")
  cat("carries that difference as well as the weight, which is why the\n")
  cat("primary FTE series in Section D is gap-consistent with the calendar\n")
  cat("series and the interpolated one is reported beside it (G1).\n")
}
cat("\nThe weighting grid below varies the weighting rule while holding the\n")
cat("gap treatment fixed at observed-only, which is the point of it.\n")

# Hybrid level: calendar base stock at panel entry plus in-panel FTE.
# Person fixed effects absorb the base, so the hybrid unit matters only
# where levels are compared across people.
base_stock <- panel %>%
  arrange(person_id, wave) %>%
  group_by(person_id) %>%
  summarise(base_exp_cal = experience_years[which.min(wave)],
            base_wave = min(wave), .groups = "drop")

# The addendum. Both gap treatments and all three weights are shipped, so a
# consumer can form any gap-consistent pair without rebuilding anything, and
# the two flags travel with the rows so the restriction is visible (G2, G3).
#
# v21.11 CORRECTION. in_panel does travel and does vary. weight_obs does
# NOT: filter(weight_obs == 1) runs two lines below, so the column is
# constant at 1 in everything written out, and the filter is exactly what
# makes the restriction invisible. Kept in the select because 03 names it
# and a silent removal is worse, but read it as a marker that the filter
# ran, not as a variable. The row count it removed is reported at the
# write.
#
# exp_hybrid_fte keeps its revision-2 definition, base plus the INTERPOLATED
# two-sided stock, because 03 and any earlier run read that name and a
# silent change of meaning under a stable name is worse than an extra
# column. The gap-consistent partners are named _obs and are new.
addendum <- grid %>%
  filter(weight_obs == 1) %>%
  select(person_id, wave, fte_w, one_w, in_panel, weight_obs,
         exp_fte_obs, exp_fte_int, exp_one_obs, exp_one_int, exp_cal_obs,
         exp_ft_obs, exp_pt_obs, exp_ftna_obs, exp_fte_recent,
         exp_fte_distant, waves_in, waves_seen) %>%
  left_join(base_stock, by = "person_id") %>%
  mutate(exp_hybrid_cal     = base_exp_cal + exp_cal_obs,
         exp_hybrid_fte     = base_exp_cal + exp_fte_int,   # revision-2 name
         exp_hybrid_fte_obs = base_exp_cal + exp_fte_obs,
         exp_hybrid_fte_int = base_exp_cal + exp_fte_int,   # explicit alias
         exp_hybrid_one_obs = base_exp_cal + exp_one_obs,
         exp_hybrid_one_int = base_exp_cal + exp_one_int)

# ---------------------------------------------------------------------
# B2. WEIGHTING SENSITIVITY (R3)
#
# Reported on two quantities, not one. The mean terminal stock gap is a
# level and is sensitive to who is in the panel and for how long. The
# event-time change from k = -1 to k = K_HI is the object the paper
# actually quotes, so it is the one whose stability matters.
# ---------------------------------------------------------------------
# Y2. The grid runs on the WEIGHT-OBSERVED frame, the same one D1
# accumulates on. Revision 4 built it on the full panel, so the baseline
# cell differed from the headline by the 220 rows the addendum drops, and
# a grid whose purpose is to vary one thing at a time was varying two.
sens_one <- function(ft, cap, rule) {
  .base <- panel %>% filter(!is.na(hours_worked_clean) | employed != 1)
  w <- fte_weight(.base$hours_worked_clean, .base$employed, ft, cap, rule)
  # The weight is attached by assignment rather than inside mutate(),
  # where `w` resolved from the function environment only because no
  # column happened to be called w.
  st <- .base %>% select(person_id, wave, female)
  st$w <- dplyr::coalesce(w, 0)
  st <- st %>%
    arrange(person_id, wave) %>%
    group_by(person_id) %>%
    mutate(stock = cumsum(w)) %>%
    ungroup()
  
  term <- st %>% group_by(person_id, female) %>% slice(n()) %>% ungroup()
  gap_term <- mean(term$stock[term$female == 1], na.rm = TRUE) -
    mean(term$stock[term$female == 0], na.rm = TRUE)
  
  e <- st %>%
    inner_join(coh %>% select(person_id, cohort), by = "person_id") %>%
    filter(cohort != 10000L) %>%
    mutate(k = wave - cohort) %>%
    filter(k %in% c(KBASE, K_HI)) %>%
    group_by(k, female) %>%
    summarise(s = mean(stock, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = female, values_from = s, names_prefix = "f") %>%
    mutate(gap = f1 - f0)
  pick <- function(kk) if (any(e$k == kk)) e$gap[e$k == kk] else NA_real_
  chg <- pick(K_HI) - pick(KBASE)
  
  data.frame(rule = rule, ft_hours = ft, cap = cap,
             gap_terminal = round(gap_term, 2),
             gap_change_k = round(chg, 2),
             stringsAsFactors = FALSE)
}

sens_spec <- expand.grid(rule = RULE_GRID, ft = FT_GRID, cap = CAP_GRID,
                         stringsAsFactors = FALSE) %>%
  # A concave rule with no ceiling is the linear rule, so it is not a
  # separate cell.
  filter(!(rule == "concave" & !is.finite(cap)),
         !(rule == "concave" & cap <= 1))

sens <- bind_rows(lapply(seq_len(nrow(sens_spec)), function(i)
  sens_one(sens_spec$ft[i], sens_spec$cap[i], sens_spec$rule[i]))) %>%
  arrange(rule, ft_hours, cap)

cat("\nWeighting sensitivity. gap_terminal is the women-minus-men mean\n")
cat("terminal in-panel stock; gap_change_k is the change in that gap from\n")
cat(sprintf("k = %d to k = %d, which is the quantity the paper quotes.\n",
            KBASE, K_HI))
print(as.data.frame(sens), row.names = FALSE)
write.csv(sens, file.path(FTE_DIR, "B_weight_sensitivity.csv"), row.names = FALSE)

# Y2. The baseline cell IS the headline, or the grid is not varying one
# thing at a time. Checked rather than asserted in a comment, and
# reported as a difference so a small mismatch is visible instead of
# fatal: the grid is a sensitivity display, not a result.
.base_cell <- sens$gap_change_k[sens$rule == WEIGHT_RULE &
                                  sens$ft_hours == FT_HOURS &
                                  is.finite(sens$cap) & sens$cap == FTE_CAP]
if (length(.base_cell) == 1L && exists("path_full")) {
  .hl <- path_full$d_fte[path_full$k == K_HI]
  if (length(.hl) == 1L)
    cat(sprintf(paste0("\nBaseline cell against the D1 headline: %.3f ",
                       "against %.3f (diff %.4f).\n"),
                .base_cell, .hl, .base_cell - .hl))
  rm(.hl)
}
rm(.base_cell)

rng <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) c(NA_real_, NA_real_) else c(min(x), max(x))
}
rt <- rng(sens$gap_terminal); rc <- rng(sens$gap_change_k)
if (all(is.finite(rt)))
  cat(sprintf("\nRange across the grid: terminal gap %.2f to %.2f, a spread of\n  %.0f%% of the larger endpoint.\n",
              rt[1], rt[2], 100 * abs(diff(rt)) / max(abs(rt))))
if (all(is.finite(rc)))
  cat(sprintf("Event-time change from k = %d to k = %d: %.2f to %.2f, a spread\n  of %.0f%%.\n",
              KBASE, K_HI, rc[1], rc[2], 100 * abs(diff(rc)) / max(abs(rc))))
cat("Report this grid in the appendix. If the two rules disagree by much\n")
cat("more than the ft-week choice does, the hard cap rather than the\n")
cat("normalisation is what the result rests on, and the concave rule is\n")
cat("the one to lead with.\n")

# =====================================================================
# SECTION C. ehtjb GRANULARITY
#
# The threat this section tests is differential measurement error in the
# OUTCOME of the central event study. If ehtjb increments in whole years,
# a part-year or part-time year is rounded, and the rounding falls
# overwhelmingly on mothers. The audit's A4 checks the Mincer-proxy
# share, which is a different question: it asks WHICH instrument measured
# experience, not how coarsely the chosen one measures it.
# =====================================================================
cat("\n--- C. ehtjb increment granularity ---\n")

# H1: the increments come from the panel. olf_flows() is a no-op on a
# v21 or later build and rebuilds them on an older one, saying which it
# did, so this section does not hold a second definition of a quantity
# the loader already defines. The three lags below are genuinely local:
# lagged hours, employment and full-time status are not on the panel.
panel <- olf_flows(panel)
inc <- panel %>%
  arrange(person_id, wave) %>%
  group_by(person_id) %>%
  mutate(h_lag  = lag(hours_worked_clean),
         e_lag  = lag(employed),
         f_lag  = lag(fulltime)) %>%
  ungroup() %>%
  filter(d_wave == 1, !is.na(d_paid_years)) %>%
  mutate(d_paid = d_paid_years,   # local names kept so the rest of the
         d_olf  = d_olf_years)    # section reads unchanged

cat(sprintf("Contiguous pairs with an observed increment: %s\n",
            format(nrow(inc), big.mark = ",")))

granular <- inc %>%
  mutate(is_integer = abs(d_paid - round(d_paid)) < 0.01) %>%
  group_by(female) %>%
  summarise(n = n(),
            pct_integer = round(100 * mean(is_integer), 1),
            .groups = "drop")
cat("\nShare of increments that are whole numbers:\n")
print(as.data.frame(granular), row.names = FALSE)

bucket_tab <- inc %>%
  mutate(bucket = cut(d_paid, breaks = c(-Inf, -0.01, 0.1, 0.4, 0.6, 0.9,
                                         1.1, Inf),
                      labels = c("negative", "~0", "~0.25", "~0.5", "~0.75",
                                 "~1", ">1.1"))) %>%
  count(female, bucket) %>%
  group_by(female) %>% mutate(pct = round(100 * n / sum(n), 1)) %>% ungroup()
cat("\nIncrement distribution by sex:\n")
print(as.data.frame(bucket_tab), row.names = FALSE)
write.csv(bucket_tab, file.path(FTE_DIR, "C_increment_buckets.csv"),
          row.names = FALSE)

# The decisive cell. Someone employed part-time in BOTH waves worked all
# year at roughly half hours. If ehtjb credits them a full year, the
# variable is a calendar count and the FTE wedge is entirely unrecorded.
cell <- inc %>%
  filter(!is.na(e_lag), !is.na(employed), !is.na(f_lag), !is.na(fulltime)) %>%
  mutate(state = case_when(
    e_lag == 1 & employed == 1 & f_lag == 1 & fulltime == 1 ~ "FT both waves",
    e_lag == 1 & employed == 1 & f_lag == 0 & fulltime == 0 ~ "PT both waves",
    e_lag == 1 & employed == 1                              ~ "FT/PT switch",
    e_lag == 0 & employed == 0                              ~ "not employed both",
    TRUE                                                    ~ "entry or exit")) %>%
  group_by(state, female) %>%
  summarise(n = n(),
            mean_d_paid = round(mean(d_paid), 3),
            med_d_paid  = round(stats::median(d_paid), 3),
            mean_fte_w  = round(mean(fte_w, na.rm = TRUE), 3),
            mean_d_olf  = round(mean(d_olf, na.rm = TRUE), 3),
            .groups = "drop") %>%
  arrange(state, female)
cat("\nIncrement by labour-market state, against the FTE weight:\n")
print(as.data.frame(cell), row.names = FALSE)
write.csv(cell, file.path(FTE_DIR, "C_increment_by_state.csv"), row.names = FALSE)

# The two rows that carry the claim, stated as a sentence rather than
# left for the reader to assemble from the table.
say_cell <- function(st) {
  r <- cell %>% filter(state == st)
  if (nrow(r) != 2) return(invisible(NULL))
  f <- r[r$female == 1, ]; m <- r[r$female == 0, ]
  cat(sprintf("\n%s: %s female against %s male person-waves (%.1fx).\n",
              st, format(f$n, big.mark = ","), format(m$n, big.mark = ","),
              f$n / m$n))
  cat(sprintf("  women  ehtjb credits %.3f years for an FTE weight of %.3f (%+.3f)\n",
              f$mean_d_paid, f$mean_fte_w, f$mean_d_paid - f$mean_fte_w))
  cat(sprintf("  men    ehtjb credits %.3f years for an FTE weight of %.3f (%+.3f)\n",
              m$mean_d_paid, m$mean_fte_w, m$mean_d_paid - m$mean_fte_w))
}
say_cell("PT both waves")
say_cell("FT both waves")
cat("\nA positive number in the last column is what ehtjb OVERSTATES per\n")
cat("year. The part-time cell is heavily female and overstated; the\n")
cat("full-time cell is male-weighted and understated. The two run in\n")
cat("opposite directions, so the measurement error is not classical and\n")
cat("does not attenuate towards zero.\n")

# =====================================================================
# SECTION D. EVENT TIME, AND THE FTE GAP AGAINST THE CALENDAR GAP
# =====================================================================
cat("\n--- D. Event time ---\n")

# TWO INNER JOINS, AND THEY COST DIFFERENT AMOUNTS (G10). The addendum join
# drops rows where the FTE weight could not be computed; the cohort join
# drops left-censored parents, who are excluded from the event design by the
# same absorbing ever_parent rule 03 uses. The second is two orders of
# magnitude larger than the first, so the two are counted separately below.
# Revision 3 printed the whole difference between the panel and the event
# sample as though it were the weight, which is the misattribution the
# two-order waterfall in 03's T8 exists to prevent.
ev_add <- panel %>%
  inner_join(addendum %>% select(person_id, wave,
                                 exp_fte_obs, exp_fte_int,
                                 exp_one_obs, exp_one_int, exp_cal_obs,
                                 exp_ft_obs, exp_pt_obs, exp_ftna_obs,
                                 exp_fte_recent, exp_fte_distant),
             by = c("person_id", "wave"))
ev <- ev_add %>%
  inner_join(coh %>% select(person_id, cohort), by = "person_id") %>%
  mutate(treated = as.integer(cohort != 10000L),
         k = ifelse(treated == 1, wave - cohort, NA_integer_),
         post = ifelse(treated == 1, as.integer(k >= 0), 0L),
         post_f = post * female)

cat(sprintf("Event sample: %s person-waves, %s persons (%s treated)\n",
            format(nrow(ev), big.mark = ","),
            format(dplyr::n_distinct(ev$person_id), big.mark = ","),
            format(dplyr::n_distinct(ev$person_id[ev$treated == 1]),
                   big.mark = ",")))
wf_ev <- data.frame(
  step = c("panel rows",
           "with a computable FTE weight (addendum join)",
           "in an event cohort (cohort join)"),
  person_waves = c(nrow(panel), nrow(ev_add), nrow(ev)),
  persons = c(dplyr::n_distinct(panel$person_id),
              dplyr::n_distinct(ev_add$person_id),
              dplyr::n_distinct(ev$person_id)),
  stringsAsFactors = FALSE)
wf_ev$lost_rows <- c(NA_integer_, -diff(wf_ev$person_waves))
cat("\nWhere the event sample comes from, one restriction at a time (G10):\n")
print(wf_ev, row.names = FALSE)
write.csv(wf_ev, file.path(FTE_DIR, "D_event_sample_waterfall.csv"),
          row.names = FALSE)
cat("The cohort restriction is the large one and it is a DESIGN choice: the\n")
cat("absorbing ever_parent rule drops respondents who were already parents\n")
cat("at panel entry, because their event time is unknown. The weight\n")
cat("restriction is the small one and it is a DATA limitation. Do not quote\n")
cat("either as the other.\n")

# ---------------------------------------------------------------------
# D1. THE RAW PATH (R1, R2)
#
# Column order is the argument. gap_ft and gap_pt come first because they
# are counts of years and require no full-time week, no cap and no
# weighting rule: a mother holds so many fewer full-time years and so
# many more part-time years than a father, and those two numbers net to
# the calendar gap. The FTE column then prices that substitution. The d_
# columns are changes from k = KBASE, which is how every one of these
# should be quoted, because the pre-birth gaps are not zero.
# ---------------------------------------------------------------------
# fte is the PRIMARY series and is gap-consistent with cal, ft and pt: all
# four credit an unobserved interior wave with nothing (G1). fti is the
# interpolated two-sided stock, which is what revision 2 put in this column,
# and one is the one-sided stock. Carrying all four means the weight and the
# gap treatment can be varied one at a time.
path_raw <- ev %>%
  filter(treated == 1, k >= K_LO, k <= K_HI) %>%
  group_by(k, female) %>%
  summarise(n = n(),
            cal = mean(exp_cal_obs, na.rm = TRUE),
            fte = mean(exp_fte_obs, na.rm = TRUE),
            fti = mean(exp_fte_int, na.rm = TRUE),
            one = mean(exp_one_obs, na.rm = TRUE),
            ft  = mean(exp_ft_obs,  na.rm = TRUE),
            pt  = mean(exp_pt_obs,  na.rm = TRUE),
            .groups = "drop") %>%
  pivot_wider(names_from = female,
              values_from = c(n, cal, fte, fti, one, ft, pt)) %>%
  arrange(k) %>%
  mutate(gap_ft  = ft_1  - ft_0,
         gap_pt  = pt_1  - pt_0,
         gap_cal = cal_1 - cal_0,
         gap_fte = fte_1 - fte_0,
         gap_fti = fti_1 - fti_0,
         gap_one = one_1 - one_0)

if (!any(path_raw$k == KBASE))
  stop("Normalisation base k = ", KBASE, " is not in the event window.",
       call. = FALSE)
b0 <- path_raw %>% filter(k == KBASE)

# Unrounded, so anything computed from it later is not computed from a
# three-decimal display value.
path_full <- path_raw %>%
  mutate(d_ft  = gap_ft  - b0$gap_ft,
         d_pt  = gap_pt  - b0$gap_pt,
         d_cal = gap_cal - b0$gap_cal,
         d_fte = gap_fte - b0$gap_fte,
         d_fti = gap_fti - b0$gap_fti,
         d_one = gap_one - b0$gap_one,
         ratio_chg     = ifelse(abs(d_cal) < MIN_D_CAL, NA_real_, d_fte / d_cal),
         ratio_chg_int = ifelse(abs(d_cal) < MIN_D_CAL, NA_real_, d_fti / d_cal),
         ratio_chg_one = ifelse(abs(d_cal) < MIN_D_CAL, NA_real_, d_one / d_cal))

path <- path_full %>%
  transmute(k, n_0, n_1,
            gap_ft  = round(gap_ft, 3),  gap_pt  = round(gap_pt, 3),
            gap_cal = round(gap_cal, 3), gap_fte = round(gap_fte, 3),
            d_ft    = round(d_ft, 3),    d_pt    = round(d_pt, 3),
            d_cal   = round(d_cal, 3),   d_fte   = round(d_fte, 3),
            ratio_chg = round(ratio_chg, 2))

cat("\nWomen-minus-men in-panel accumulation gap by event time.\n")
cat("gap_* are levels; d_* are changes from k =", KBASE, "; ratio_chg is\n")
cat("d_fte / d_cal and is left empty where d_cal is too small to divide by.\n")
cat("Every series in this table is gap-blind, so ratio_chg is a comparison\n")
cat("of WEIGHTS and of nothing else (G1).\n")
print(as.data.frame(path), row.names = FALSE)
write.csv(path_full, file.path(FTE_DIR, "D_gap_by_event_time.csv"),
          row.names = FALSE)

# The headline, written out with signs so it cannot be misread, and with
# the identity residual attached (R4). Read from the UNROUNDED frame (G13):
# built from the display table, the residual printed as +0.001 while D1b
# printed -0.000000 for the same decomposition, which invites a reader to
# think the identity closes only approximately.
h <- path_full %>% filter(k == K_HI)
if (nrow(h) == 1) {
  cat(sprintf("\nAt k = %d, relative to k = %d:\n", K_HI, KBASE))
  cat(sprintf("  change in the full-time-years gap  : %+.2f\n", h$d_ft))
  cat(sprintf("  change in the part-time-years gap  : %+.2f\n", h$d_pt))
  cat(sprintf("  the two summed                     : %+.2f\n", h$d_ft + h$d_pt))
  cat(sprintf("  change in the calendar-years gap   : %+.2f\n", h$d_cal))
  # Y4: this residual is the exp_ftna_obs term, which is
  # employed-with-fulltime-missing, NOT employed-with-hours-missing. The
  # two are different: fulltime is never missing in this build, so the
  # term is identically zero, while the 220 hours-missing waves sit
  # inside exp_ft_obs or exp_pt_obs and are removed from this section
  # altogether by the addendum join. The row accounting in B is where
  # those 220 appear.
  cat(sprintf("  residual, employed with full-time status missing: %+.6f\n",
              h$d_cal - h$d_ft - h$d_pt))
  cat(sprintf("  change in the FTE-years gap        : %+.2f  (%.2f times the\n",
              h$d_fte, ifelse(is.na(h$ratio_chg), NA_real_, h$ratio_chg)))
  cat("                                        calendar change)\n")
  cat("  both series gap-blind, so that ratio is a comparison of weights\n")
  cat(sprintf("  published calendar gap for comparison: %.2f\n", MS_EXP_GAP))
  cat("\nLead with the first two lines. They are counts of years, they need\n")
  cat("no normalisation, and they net to the calendar gap by construction.\n")
}

# The weight and the gap treatment, varied one at a time. Read the columns
# in pairs: fte against one is the overtime credit at a fixed gap treatment,
# fte against fti is the gap credit at a fixed weight. Revision 2 reported
# the diagonal of this table as though it were the first pair.
sens_gap <- path_full %>%
  transmute(k,
            d_cal = round(d_cal, 3),
            d_one_1sided_obs = round(d_one, 3),
            d_fte_2sided_obs = round(d_fte, 3),
            d_fti_2sided_int = round(d_fti, 3),
            r_one = round(ratio_chg_one, 2),
            r_fte = round(ratio_chg, 2),
            r_fti = round(ratio_chg_int, 2))
cat("\nThe same changes under each weight and gap treatment:\n")
print(as.data.frame(sens_gap), row.names = FALSE)
write.csv(sens_gap, file.path(FTE_DIR, "D_gap_treatment_sensitivity.csv"),
          row.names = FALSE)
if (nrow(h) == 1 && is.finite(h$ratio_chg) && is.finite(h$ratio_chg_int)) {
  cat(sprintf(paste("At k = %d the FTE-over-calendar ratio is %.2f gap-blind",
                    "and %.2f\ninterpolated, a difference of %.2f attributable",
                    "to the gap credit alone.\n"),
              K_HI, h$ratio_chg, h$ratio_chg_int,
              h$ratio_chg_int - h$ratio_chg))
  cat("Quote the gap-blind figure. It is the one whose denominator shares a\n")
  cat("gap treatment with its numerator.\n")
  
  # -------------------------------------------------------------------
  # THE TWO MARGINS, IN YEARS, WHICH NEED NO RATIO AT ALL (G17)
  #
  # The calendar change IS the extensive margin: a calendar stock only
  # accumulates in a wave the person was employed, so the women-minus-men
  # change in it is the accumulated employment difference. The FTE change
  # less the calendar change is therefore the intensive margin, the part
  # that comes from the hours inside those employed years. Stating the two
  # in years is cleaner than any ratio, because a ratio of the two invites
  # the share error this block was fixed for.
  # -------------------------------------------------------------------
  ext_m  <- h$d_cal
  int_two <- h$d_fte - h$d_cal
  int_one <- if (is.finite(h$d_one)) h$d_one - h$d_cal else NA_real_
  cat(sprintf("\nThe same thing in years rather than as a ratio, at k = %d:\n",
              K_HI))
  cat(sprintf("  extensive, the calendar change              : %+.3f\n", ext_m))
  cat(sprintf("  intensive, two-sided FTE less calendar      : %+.3f\n", int_two))
  if (is.finite(int_one))
    cat(sprintf("  intensive, one-sided FTE less calendar      : %+.3f\n", int_one))
  cat(sprintf("  the two summed, two-sided                   : %+.3f\n",
              ext_m + int_two))
  if (is.finite(ext_m) && is.finite(int_two) && abs(int_two) > 0)
    cat(sprintf(paste("The extensive and intensive margins are %.3f and %.3f",
                      "years, a ratio of\n%.2f. Where those two are close,",
                      "say so in the text: half the FTE\naccumulation gap is",
                      "mothers being employed in fewer waves and half is\nthe",
                      "hours inside the waves they were employed, and neither",
                      "sentence\nneeds a normalisation beyond the weight",
                      "itself.\n"),
                abs(ext_m), abs(int_two), abs(ext_m / int_two)))
  
  # -------------------------------------------------------------------
  # THE OVERTIME CREDIT, NORMALISED SO IT IS COMPARABLE TO SECTION A (G17)
  #
  # Revision 3.1 divided the credit by the whole ratio, which carries the
  # extensive margin in numerator and denominator alike, and then told the
  # reader the result should match Section A's share of a per-employed-year
  # wedge. It cannot: the two shares have different denominators by
  # construction, and the D figure is diluted by exactly the extensive
  # margin. The comparable share is of the INTENSIVE margin, which is the
  # ratio's excess over one.
  # -------------------------------------------------------------------
  if (is.finite(h$ratio_chg_one) && is.finite(int_two) && abs(int_two) > 1e-9) {
    credit_int <- (int_two - int_one) / int_two
    cat(sprintf(paste("\nThe one-sided ratio is %.2f against the two-sided",
                      "%.2f. Expressed as a\nshare of the INTENSIVE margin,",
                      "the overtime credit to fathers' long weeks\nis %.0f%%",
                      "of it, the rest being the discount to mothers' short",
                      "ones.\n"),
                h$ratio_chg_one, h$ratio_chg, 100 * credit_int))
    cat(sprintf(paste("For reference, as a share of the whole ratio it is",
                      "only %.0f%%, because the\nratio carries the extensive",
                      "margin in both its numerator and its\ndenominator. Do",
                      "not quote that figure against Section A's (G17).\n"),
                100 * (h$ratio_chg - h$ratio_chg_one) / h$ratio_chg))
    if (exists("A_CREDIT_SHARE") && is.finite(A_CREDIT_SHARE)) {
      cat(sprintf(paste("Section A's per-employed-year wedge puts the same",
                        "credit at %.0f%%.\nThis block puts it at %.0f%%.",
                        "Both are shares of an intensive quantity, so\nthey",
                        "are comparable; they are not the same estimand and",
                        "need not\ncoincide.\n"),
                  100 * A_CREDIT_SHARE, 100 * credit_int))
      cat("  A is a cross-sectional rate over all employed parents, pooled\n")
      cat("  over event time. This is a change in accumulated stocks at one\n")
      cat("  horizon on the treated only, so it weights by years employed and\n")
      cat("  by cohort. A spread of a few points between them is what those\n")
      cat("  differences produce; a spread of a factor of two would mean one\n")
      cat("  of the two is wrong.\n")
      if (is.finite(credit_int) && abs(credit_int) > 1e-9) {
        rel <- abs(100 * A_CREDIT_SHARE - 100 * credit_int)
        if (rel > 15)
          cat(sprintf(paste("  *** They differ by %.0f points, which is more",
                            "than composition and\n      event-time weighting",
                            "should produce. Reconcile before quoting\n",
                            "     either.\n"), rel))
      }
    }
  }
}

# ---------------------------------------------------------------------
# D1b. FIXED-EFFECTS CONTRASTS AND THE ARITHMETIC IDENTITY (R4)
# ---------------------------------------------------------------------
fe_contrast <- function(y) {
  d <- ev %>% filter(!is.na(.data[[y]]))
  # A constant outcome is not an error here. exp_ftna_obs is identically
  # zero whenever the loader supplies fulltime for every employed wave,
  # which is the normal case; it is carried only so the identity below
  # closes on panels where fulltime IS sometimes missing. Its contrast is
  # then zero by construction and there is nothing to estimate.
  v <- stats::var(d[[y]], na.rm = TRUE)
  if (!is.finite(v) || v == 0) {
    cat(sprintf("  [%s] constant at %.0f in this panel; contrast is zero by\n",
                y, d[[y]][1]))
    cat("      construction and is not estimated.\n")
    # n_rows is what was supplied, n_used is what a fit consumed. Revision 3
    # put nrow(d) in a column the fitted rows filled with m$nobs, so this row
    # looked like a larger sample when it is an unfitted one (G14).
    return(data.frame(outcome = y, n_rows = nrow(d), n_used = NA_integer_,
                      est = 0, se = NA_real_, stringsAsFactors = FALSE))
  }
  m <- tryCatch(
    fixest::feols(
      stats::as.formula(paste0(y, " ~ post + post_f | person_id + wave")),
      data = d, cluster = ~person_id, notes = FALSE, warn = FALSE),
    error = function(e) { cat(sprintf("  [%s] not estimable: %s\n", y,
                                      conditionMessage(e))); NULL })
  if (is.null(m))
    return(data.frame(outcome = y, n_rows = nrow(d), n_used = NA_integer_,
                      est = NA_real_, se = NA_real_, stringsAsFactors = FALSE))
  ct <- fixest::coeftable(m)
  data.frame(outcome = y, n_rows = nrow(d), n_used = m$nobs,
             est = unname(ct["post_f", "Estimate"]),
             se  = unname(ct["post_f", "Std. Error"]),
             stringsAsFactors = FALSE)
}

# The identity family is the four gap-blind calendar outcomes. exp_fte_obs
# and exp_one_obs join it as gap-blind weighted stocks, and exp_fte_int is
# kept last so the gap credit is visible as the difference between the two
# two-sided rows rather than being folded into the FTE-over-calendar ratio
# below (G1).
fe <- bind_rows(lapply(c("exp_cal_obs", "exp_ft_obs", "exp_pt_obs",
                         "exp_ftna_obs", "exp_one_obs", "exp_fte_obs",
                         "exp_fte_int"), fe_contrast)) %>%
  mutate(mde = mde(se))
cat("\nPost-birth women-minus-men contrast, accumulation outcomes:\n")
print(as.data.frame(fe %>% mutate(across(c(est, se, mde), ~round(.x, 4)))),
      row.names = FALSE)
write.csv(fe, file.path(FTE_DIR, "D_fe_contrasts.csv"), row.names = FALSE)
fe_fitted <- fe %>% filter(!is.na(n_used))
if (nrow(fe_fitted)) {
  drop_sing <- fe_fitted$n_rows[1] - fe_fitted$n_used[1]
  cat(sprintf(paste("n_rows is what was supplied, n_used is what the fit",
                    "consumed. The %s-row\ngap is singleton dropping under",
                    "person_id + wave, not a different sample (G14).\n"),
              format(drop_sing, big.mark = ",")))
  if (dplyr::n_distinct(fe_fitted$n_used) > 1)
    cat("*** The fitted rows do NOT all use the same sample, so the identity\n",
        "    below is not computed on one set of rows. Check the missingness\n",
        "    pattern before reading the residual.\n")
}

est_of <- function(y) fe$est[fe$outcome == y]
resid_id <- est_of("exp_cal_obs") - (est_of("exp_ft_obs") + est_of("exp_pt_obs") +
                                       est_of("exp_ftna_obs"))
cat(sprintf("\nIdentity check: %+.4f full-time years %+.4f part-time years\n",
            est_of("exp_ft_obs"), est_of("exp_pt_obs")))
cat(sprintf("  %+.4f employed-hours-missing years  =  %+.4f calendar years\n",
            est_of("exp_ftna_obs"), est_of("exp_ft_obs") + est_of("exp_pt_obs") +
              est_of("exp_ftna_obs")))
cat(sprintf("  residual against the estimated calendar contrast: %+.6f\n",
            resid_id))
cat("A residual at machine precision means the decomposition is\n")
cat("arithmetic, not an assumption, and can be stated as such.\n")

if (isTRUE(abs(est_of("exp_cal_obs")) > 1e-8)) {
  cal_e <- est_of("exp_cal_obs")
  cat("\nContrast over the calendar contrast, by weight and gap treatment:\n")
  for (nm in c("exp_one_obs", "exp_fte_obs", "exp_fte_int")) {
    e <- est_of(nm)
    if (length(e) == 1 && is.finite(e))
      cat(sprintf("  %-12s %.2f  (%+.4f against %+.4f)%s\n", nm, e / cal_e, e,
                  cal_e,
                  if (identical(nm, FTE_PRIMARY)) "   <- PRIMARY, gap-blind" else
                    if (identical(nm, FTE_SECONDARY)) "   gap-interpolated" else
                      if (identical(nm, ONE_PRIMARY)) "   one-sided weight" else ""))
  }
  cat("  Only the gap-blind rows share a gap treatment with the calendar\n")
  cat("  denominator, so only those ratios are comparisons of weights (G1).\n")
}

# ---------------------------------------------------------------------
# D2. REPRICING THE RETURN TO EXPERIENCE, STATED AS A BOUND (R7)
#
# The earlier framing of this section was wrong and is corrected here.
# The claim was that FTE re-measurement should shrink the POOLED gender
# gap in the return to experience, and that this would distinguish
# measurement from the sorting explanation. It would, but not in this
# specification: the MDE here is larger than the pooled gap itself, so
# the design cannot see the object it was supposed to move. Testing the
# shrinkage hypothesis means putting exp_hybrid_one_obs into the DML
# specification that produced the pooled gap, not into a two-way
# fixed-effects regression (G9). exp_hybrid_one_obs and not
# exp_hybrid_fte, because the latter is gap-interpolated while the
# calendar arm it would be compared against is not, and because the
# hypothesis is one-sided.
#
# What this section CAN do is bound. A gendered-price account of the
# ten-year penalty requires a per-year price gap of roughly penalty
# divided by the experience gap it works through. If the MDE is far below
# that, the account is excluded by the bound whatever the point estimate
# is, and that is a stronger statement than a non-rejection.
# ---------------------------------------------------------------------
cat("\nGender gap in the return to experience, by unit:\n")
# The FTE interaction is built on the PRIMARY gap-blind stock, so the
# calendar and FTE rows below differ in the weight alone (G1). The
# interpolated and one-sided versions are carried so the same regression can
# be run in each and the movement attributed.
wg <- ev %>%
  filter(!is.na(ln_hourly_wage_real)) %>%
  mutate(cal_f = exp_cal_obs * female,
         fte_f = exp_fte_obs * female,
         fti_f = exp_fte_int * female,
         one_f = exp_one_obs * female,
         ft_f  = exp_ft_obs * female,
         pt_f  = exp_pt_obs * female)

show <- function(tag, m, term, unit, blind_this) {
  ct <- tryCatch(fixest::coeftable(m), error = function(e) NULL)
  if (is.null(ct) || !(term %in% rownames(ct))) {
    cat(sprintf("  [%s] term absent: %s\n", tag, term)); return(invisible(NULL))
  }
  se <- unname(ct[term, "Std. Error"]); es <- unname(ct[term, "Estimate"])
  .blind[[tag]] <<- ct[term, , drop = FALSE]
  if (blind_this)
    cat(sprintf("  [%s] n=%s  SE=%.4f  MDE=%.4f %s  (estimate withheld)\n",
                tag, format(m$nobs, big.mark = ","), se, mde(se), unit))
  else
    cat(sprintf("  [%s] n=%s  est=%+.4f  SE=%.4f  MDE=%.4f %s\n",
                tag, format(m$nobs, big.mark = ","), es, se, mde(se), unit))
  invisible(se)
}

# H3. The declaration, made once, immediately before the specifications
# that rely on it. Every equation below carries person fixed effects,
# which absorb the pre-panel base stock and with it the entry-cohort
# recall vintage that makes an interruption or experience LEVEL
# incomparable across people. That is the UNITS note at the top of this
# file, stated to the guard rather than only to the reader.
#
# The exemption does NOT extend to the descriptive level comparisons in
# tab:interruption. Those stay in calendar years with the unit stated,
# and nothing here changes that.
assert_no_olf_level(c("actual_experience", "years_not_in_lf"),
                    context = "Section D wage equations, person FE",
                    allow_within_person = TRUE)
assert_firm_size_regime(wg, context = "Section D wage equations",
                        vars = all.vars(~ exp_cal_obs + exp_fte_obs +
                                          exp_one_obs + exp_fte_int + female))
assert_not_entrant_only(all.vars(~ ln_hourly_wage_real + exp_cal_obs +
                                   exp_fte_obs + exp_one_obs + exp_fte_int),
                        context = "Section D wage equations", data = panel)

m_cal <- fixest::feols(ln_hourly_wage_real ~ exp_cal_obs + cal_f |
                         person_id + wave, data = wg, cluster = ~person_id,
                       notes = FALSE, warn = FALSE)
se_cal <- show("betaX_gap_calendar", m_cal, "cal_f", "log points per year", BLIND)

m_fte <- fixest::feols(ln_hourly_wage_real ~ exp_fte_obs + fte_f |
                         person_id + wave, data = wg, cluster = ~person_id,
                       notes = FALSE, warn = FALSE)
se_fte <- show("betaX_gap_fte_obs", m_fte, "fte_f",
               "log points per FTE year, gap-blind", BLIND)

# The one-sided arm, which is the weight the paper's hypothesis names, and
# the interpolated arm, which is what revision 2 reported here. Both are run
# on the same rows as the two above, so the three FTE rows differ from each
# other in exactly one choice each.
m_one <- fixest::feols(ln_hourly_wage_real ~ exp_one_obs + one_f |
                         person_id + wave, data = wg, cluster = ~person_id,
                       notes = FALSE, warn = FALSE)
show("betaX_gap_one_sided", m_one, "one_f",
     "log points per one-sided FTE year", BLIND)
m_fti <- fixest::feols(ln_hourly_wage_real ~ exp_fte_int + fti_f |
                         person_id + wave, data = wg, cluster = ~person_id,
                       notes = FALSE, warn = FALSE)
show("betaX_gap_fte_int", m_fti, "fti_f",
     "log points per FTE year, interpolated", BLIND)
cat("  Four rows, one choice apart each: calendar against gap-blind FTE is\n")
cat("  the weight, gap-blind against interpolated FTE is the gap credit, and\n")
cat("  one-sided against two-sided is the overtime credit. Read them in\n")
cat("  pairs; the diagonal is not interpretable (G1).\n")

# The bound. Two normalisations of what the gendered-price account needs,
# because the penalty can be divided by the calendar gap or by the FTE
# gap and the honest statement covers both.
if (is.null(se_cal)) se_cal <- NA_real_
if (is.null(se_fte)) se_fte <- NA_real_
# From the unrounded frame, not from the three-decimal display table.
d_cal_hi <- path_full$d_cal[path_full$k == K_HI]
d_fte_hi <- path_full$d_fte[path_full$k == K_HI]
req_cal <- if (length(d_cal_hi) && abs(d_cal_hi) > 0) MS_PENALTY10 / abs(d_cal_hi) else NA_real_
req_fte <- if (length(d_fte_hi) && abs(d_fte_hi) > 0) MS_PENALTY10 / abs(d_fte_hi) else NA_real_
req_min <- suppressWarnings(min(c(req_cal, req_fte), na.rm = TRUE))
if (!is.finite(req_min)) req_min <- NA_real_

cat("\nHow to read those two rows.\n")
cat(sprintf("  pooled gap in the return to experience, from the DML : %.4f\n",
            MS_POOLED_BX))
cat(sprintf("  MDE of the FTE specification here                    : %.4f\n",
            mde(se_fte)))
if (!is.na(mde(se_fte)) && mde(se_fte) > MS_POOLED_BX)
  cat("  This specification cannot detect the pooled gap at all, so it\n",
      "  cannot detect a change in it either. Do not report it as evidence\n",
      "  that FTE re-measurement shrinks the pooled gap. To test that, swap\n",
      "  exp_hybrid_one_obs into the DML specification, which is what T8 in\n",
      "  03 does; the addendum ships that column for exactly this purpose\n",
      "  (G2, G9).\n")
cat(sprintf("  price gap a gendered-price account of the %.2f log-point\n",
            MS_PENALTY10))
cat(sprintf("  ten-year penalty would require: %.4f per calendar year, or\n",
            req_cal))
cat(sprintf("  %.4f per FTE year\n", req_fte))
# Y1. req_min is the SMALLER of the two required price gaps, so it is the
# harder one to rule out and taking the minimum is the conservative
# choice. Say which, because "the tighter of those" reads both ways.
cat(sprintf("  The harder of those to exclude is the smaller: %.4f.\n", req_min))

# THE BOUND IS ABOUT THE INTERVAL, NOT ABOUT THE DESIGN. Revision 4
# tested mde(se_fte) < req_min, which is a statement about precision and
# says nothing about where the estimate sits. An estimate of +0.03 with
# this standard error would CONFIRM the gendered-price account and print
# the same sentence, and BLIND would hide the estimate from the reader.
# The estimate is read from .blind, where show() has already stored it,
# so the condition uses it without printing it.
.es_fte <- tryCatch(unname(.blind[["betaX_gap_fte_obs"]][, "Estimate"]),
                    error = function(e) NA_real_)
.ucl_fte <- if (is.finite(.es_fte) && is.finite(se_fte))
  abs(.es_fte) + stats::qnorm(1 - ALPHA / 2) * se_fte else NA_real_
if (is.finite(.ucl_fte) && is.finite(req_min) && .ucl_fte < req_min) {
  cat(sprintf(paste0("  The %.0f%% upper limit on the magnitude of the price ",
                     "gap is %.1f times\n  smaller than that requirement, so ",
                     "the gendered-price account is\n  EXCLUDED by the ",
                     "interval rather than by a failure to reject. That is\n",
                     "  the sentence to write.\n"),
              100 * (1 - ALPHA), req_min / .ucl_fte))
  if (!is.na(mde(se_fte)))
    cat(sprintf(paste0("  Precision, reported separately because it is a ",
                       "different claim: the\n  MDE is %.1f times smaller ",
                       "than the requirement.\n"),
                req_min / mde(se_fte)))
} else if (is.finite(.ucl_fte)) {
  cat("  The upper limit on the magnitude of the price gap REACHES what the\n")
  cat("  gendered-price account requires, so this specification does not\n")
  cat("  exclude it. Report the design as uninformative on that question\n")
  cat("  and do not write the bound, whatever the MDE says.\n")
} else {
  cat("  The estimate could not be read, so no bound is stated. A bound\n")
  cat("  computed from the standard error alone is a statement about\n")
  cat("  precision and not about this account.\n")
}
rm(.es_fte, .ucl_fte)

# Separate prices for full-time and part-time years. This is the
# Costa Dias, Joyce and Parodi decomposition, which the paper cites as
# its closest comparison and does not implement.
#
# Reparameterised so that EQUALITY of the two prices is a single t-test
# rather than a joint restriction. With exp_total = ft + pt on the right
# hand side alongside exp_pt, the coefficient on exp_total is the
# full-time price and the coefficient on exp_pt is the part-time price
# MINUS the full-time price.
wg <- wg %>% mutate(exp_total_obs = exp_ft_obs + exp_pt_obs)
m_ftpt <- fixest::feols(ln_hourly_wage_real ~ exp_total_obs + exp_pt_obs |
                          person_id + wave, data = wg, cluster = ~person_id,
                        notes = FALSE, warn = FALSE)
show("price_fulltime_year", m_ftpt, "exp_total_obs",
     "log points per full-time year", BLIND)
show("price_parttime_minus_fulltime", m_ftpt, "exp_pt_obs",
     "log points, part-time less full-time", BLIND)

# ---------------------------------------------------------------------
# D3. DEPRECIATION. The surviving test of atrophy (R6).
#
# If the stock depreciates, recent accumulation prices higher than
# distant accumulation, and a mother's stock is disproportionately
# distant after a spell. The identification concern is that the two
# stocks move together within person; they diverge precisely for the
# interrupted, which is the variation used. This design needs no
# interrupted men and no spell-shape variation, which is why it survives
# where the shape test in Section E does not.
# ---------------------------------------------------------------------
# THIS TEST DEPENDS ON THE INTERPOLATED STOCK, BY NECESSITY (G4). A recency
# window needs a value at every wave of the span, so exp_fte_recent and
# exp_fte_distant are differences of exp_fte_int and there is no gap-blind
# version of the split that means the same thing. That is a defensible
# choice and it is not the choice G1 corrects, but it has to be stated: the
# gap credit measured in Section B sits inside both stocks here.
#
# Two further properties of the split, printed rather than left implicit.
# exp_fte_distant is zero for the first RECENT_K waves of a person's span,
# by construction, so early rows contribute no distant variation at all; and
# the pre-panel calendar base is in NEITHER stock, so "distant" means
# in-panel but more than RECENT_K waves ago and the person effect absorbs
# everything earlier.
zero_distant <- mean(wg$exp_fte_distant <= 1e-8, na.rm = TRUE)
cat(sprintf(paste("\n  Person-waves with no distant stock at all: %.1f%%.",
                  "Those are the first\n  %d waves of each person's span",
                  "and they identify the recent price only.\n"),
            100 * zero_distant, RECENT_K))
cat(sprintf("  correlation of recent and distant FTE stocks: %.3f\n",
            stats::cor(wg$exp_fte_recent, wg$exp_fte_distant,
                       use = "complete.obs")))
cat("  A correlation well below one is what makes the two prices\n")
cat("  separately identified. Report it next to the estimates.\n")
cat("  Both stocks are built from the INTERPOLATED accumulation, because a\n")
cat("  recency window has to have a value at every wave. State that where\n")
cat("  this result is reported.\n")
m_dep <- fixest::feols(ln_hourly_wage_real ~ exp_fte_recent + exp_fte_distant |
                         person_id + wave, data = wg, cluster = ~person_id,
                       notes = FALSE, warn = FALSE)
show("recent_stock_price", m_dep, "exp_fte_recent", "log points per FTE year",
     BLIND)
show("distant_stock_price", m_dep, "exp_fte_distant", "log points per FTE year",
     BLIND)

# The hypothesis is that the two prices DIFFER, and the specification above
# cannot test that: it gives two standard errors and no covariance, so the
# difference has no standard error of its own. Reparameterised the same way
# as the full-time and part-time prices. By construction the recent and
# distant stocks sum to exp_fte_int, so regressing on exp_fte_int alongside
# exp_fte_distant makes the coefficient on exp_fte_int the recent price and
# the coefficient on exp_fte_distant the distant price MINUS the recent
# price. Depreciation is then a single negative t-statistic rather than an
# eyeball comparison of two intervals.
m_dep2 <- fixest::feols(ln_hourly_wage_real ~ exp_fte_int + exp_fte_distant |
                          person_id + wave, data = wg, cluster = ~person_id,
                        notes = FALSE, warn = FALSE)
show("depreciation_recent_price", m_dep2, "exp_fte_int",
     "log points per recent FTE year", BLIND)
se_dep <- show("depreciation_distant_less_recent", m_dep2, "exp_fte_distant",
               "log points, distant less recent", BLIND)
cat("  The second row is the test. A negative coefficient is depreciation:\n")
cat("  a year of accumulation bought long ago prices below one bought\n")
cat("  recently, which is what a mother's stock is disproportionately made\n")
cat("  of after a spell. Report this row, not the two separate prices.\n")

# Every other MDE in this script is read against the quantity the claim
# requires; this one has to be too. The natural scale for a difference
# between two prices of a year is the price of a year itself.
if (!is.null(se_dep) && !is.na(se_dep)) {
  cat(sprintf("\n  MDE on the difference: %.4f log points per FTE year, against a\n",
              mde(se_dep)))
  cat(sprintf("  published return to experience of about %.4f per year.\n",
              MS_RETURN_X))
  cat(sprintf("  The smallest detectable difference is %.0f%% of the return itself.\n",
              100 * mde(se_dep) / MS_RETURN_X))
  if (mde(se_dep) > 0.5 * MS_RETURN_X) {
    cat("  So this design sees depreciation only if it removes something like\n",
        "  half or more of the value of a distant year. Report it as a bound\n",
        "  on strong depreciation, not as a test of depreciation in general,\n",
        "  and say what the bound excludes rather than what it fails to find.\n")
    # WHO THE BOUND IS ESTIMATED ON. A person-wave with no distant stock
    # contributes to the recent price only, and those are the first RECENT_K
    # waves of each span, so the distant price is identified off the
    # longer-observed. Longer observation is not random with respect to
    # attachment, and attachment is correlated with the wage. With an MDE
    # already this large relative to the return, that is a clause the bound
    # needs rather than a separate caveat.
    cat(sprintf(paste("  The distant price is identified off the %.0f%% of",
                      "person-waves that have\n  any distant stock at all,",
                      "which are the longer-observed. Longer\n  observation",
                      "selects on attachment, so state the bound as holding",
                      "on\n  the retained sample rather than on the panel.\n"),
                100 * (1 - zero_distant)))
  }
  else
    cat("  That is comfortably inside a plausible depreciation rate, so the\n",
        "  test is informative in either direction.\n")
}

# Within-couple versions of the same two tests.
if ("partner_id" %in% names(wg)) {
  cw <- wg %>%
    filter(!is.na(partner_id), partner_id != "") %>%
    mutate(couple_id = paste0(pmin(person_id, partner_id), "_",
                              pmax(person_id, partner_id)))
  # THE ANNOUNCED SAMPLE AND THE FITTED ONE ARE NOT THE SAME (G16).
  # couple_id^wave gives a fixed effect per couple-wave, and a couple-wave
  # with only one partner present is perfectly fitted by its own effect, so
  # fixest drops it. Roughly a third of the rows here are in that position.
  # 03's T6 imposes the both-partners requirement explicitly with a
  # count(couple_id, wave) filter on n == 2; the same thing is counted here
  # so the printed sample is the one the estimates come from.
  cw_both <- cw %>% count(couple_id, wave) %>% filter(n == 2)
  cw_pairs <- cw %>% semi_join(cw_both, by = c("couple_id", "wave"))
  cat(sprintf("\nWithin-couple frame: %s person-waves, %s couples\n",
              format(nrow(cw), big.mark = ","),
              format(dplyr::n_distinct(cw$couple_id), big.mark = ",")))
  cat(sprintf(paste("  of which both partners are present in the wave: %s",
                    "person-waves\n  across %s couples (%.0f%% of the frame).",
                    "The rest are singleton couple-waves\n  and are absorbed",
                    "by their own fixed effect, so the fit below uses only\n",
                    " the paired rows.\n"),
              format(nrow(cw_pairs), big.mark = ","),
              format(dplyr::n_distinct(cw_pairs$couple_id), big.mark = ","),
              100 * nrow(cw_pairs) / max(1L, nrow(cw))))
  if (nrow(cw_pairs) < nrow(cw))
    cat("  A partner appears without their partner when only one of the two is\n",
        "  an employed wage-observed respondent in that wave, so the paired\n",
        "  subset is selected on both partners being employed. Say that where\n",
        "  the within-couple estimates are reported.\n")
  # Y3: cw_pairs, the frame whose size is printed above. Fitting on cw
  # gave the same n only because fixest drops singleton couple_id^wave
  # cells by default, which is a coincidence of this build rather than
  # the restriction the sentence claims.
  m_cwc <- fixest::feols(ln_hourly_wage_real ~ exp_cal_obs + cal_f + female |
                           couple_id^wave, data = cw_pairs, cluster = ~couple_id,
                         notes = FALSE, warn = FALSE)
  show("betaX_gap_calendar_withincouple", m_cwc, "cal_f",
       "log points per year", BLIND)
  # The gap-blind stock here too, so the within-couple pair differs from the
  # pooled pair in the design and not also in the series (G1).
  m_cwf <- fixest::feols(ln_hourly_wage_real ~ exp_fte_obs + fte_f + female |
                           couple_id^wave, data = cw_pairs, cluster = ~couple_id,
                         notes = FALSE, warn = FALSE)
  show("betaX_gap_fte_obs_withincouple", m_cwf, "fte_f",
       "log points per FTE year, gap-blind", BLIND)
} else {
  cat("\npartner_id absent; the within-couple repricing is skipped.\n")
}

# =====================================================================
# SECTION E. INTERRUPTION GEOMETRY
#
# The descriptive half of this section is a finding in its own right and
# belongs in the paper whatever happens to the tests built on it: two
# thirds of mothers have a post-birth non-employed wave against a fifth
# of fathers, and fewer than half return to full-time work against nine
# tenths of fathers.
#
# The inferential half, the atrophy-against-forgone-accumulation contrast
# on spell SHAPE, is dropped (R5). See the power statement below.
# =====================================================================
cat("\n--- E. Interruption geometry ---\n")

geom <- ev %>%
  filter(treated == 1, k >= 0, !is.na(employed)) %>%
  arrange(person_id, wave) %>%
  group_by(person_id, female) %>%
  summarise(
    n_post_waves   = n(),
    n_nonemp       = sum(employed == 0),
    n_spells       = sum(employed == 0 &
                           (is.na(lag(employed)) | lag(employed) == 1)),
    longest_spell  = {
      r <- rle(employed == 0)
      if (any(r$values)) max(r$lengths[r$values]) else 0L
    },
    first_return_k = {
      kk <- k[employed == 1]
      if (length(kk)) min(kk) else NA_integer_
    },
    return_ft      = {
      i <- which(employed == 1)
      if (length(i)) fulltime[i[which.min(k[i])]] else NA_real_
    },
    .groups = "drop") %>%
  mutate(mean_spell = ifelse(n_spells > 0, n_nonemp / n_spells, 0),
         fragmented = as.integer(n_spells > 1))

geom_tab <- geom %>% filter(n_post_waves >= 4) %>%
  group_by(female) %>%
  summarise(persons = n(),
            med_post_waves = stats::median(n_post_waves),
            mean_nonemp = round(mean(n_nonemp), 2),
            pct_any_spell = round(100 * mean(n_nonemp > 0), 1),
            pct_multi_spell = round(100 * mean(fragmented == 1), 1),
            mean_longest = round(mean(longest_spell), 2),
            mean_spell_len = round(mean(mean_spell), 2),
            # Y5: renamed to what they measure. first_return_k is the
            # minimum k over EMPLOYED post-birth waves, which is 0 for
            # anyone who never left, so this is the first post-birth
            # employed observation and not a return. The conditional
            # versions beside them restrict to those who did leave, which
            # is what the word return implies.
            med_first_emp_k = stats::median(first_return_k, na.rm = TRUE),
            pct_ft_at_first_emp = round(100 * mean(return_ft, na.rm = TRUE), 1),
            med_return_k_if_left = stats::median(
              first_return_k[n_nonemp > 0], na.rm = TRUE),
            pct_return_ft_if_left = round(100 * mean(
              return_ft[n_nonemp > 0], na.rm = TRUE), 1),
            .groups = "drop")
# Y6. Spells are counted over OBSERVED ROWS, not over waves: both the
# rle() longest-spell and the lag() spell counter walk the within-person
# row sequence, so an unobserved interior wave welds two spells into one
# and shortens the measured gap. About 9.7% of persons have an interior
# gap, and the addendum removes a further 220 rows. Read longest_spell
# and n_spells as observed-row counts rather than as wave counts.
cat("Post-birth interruption shape, by sex:\n")
print(as.data.frame(geom_tab), row.names = FALSE)
write.csv(geom, file.path(FTE_DIR, "E_interruption_geometry.csv"),
          row.names = FALSE)
write.csv(geom_tab, file.path(FTE_DIR, "E_interruption_headline.csv"),
          row.names = FALSE)
cat("\nThe two columns to quote are pct_any_spell and pct_return_ft_if_left.\n")
cat("They are descriptive, they need no design, and they carry the\n")
cat("mechanism. Do NOT quote pct_ft_at_first_emp as a return rate: it is\n")
cat("full-time status at the first post-birth wave in which the person is\n")
cat("employed, which for someone who never left is their status at k = 0,\n")
cat("and that is why it reads near 90 for fathers (Y5).\n")

# Is there enough variation in SHAPE at a given TOTAL to identify the
# contrast? This is the power question, and it decides whether the
# atrophy-versus-forgone-accumulation test is worth running at all.
shape_support <- geom %>%
  filter(female == 1, n_post_waves >= 5, n_nonemp >= 1) %>%
  count(n_nonemp, fragmented) %>%
  pivot_wider(names_from = fragmented, values_from = n,
              names_prefix = "multi_spell_", values_fill = 0L)
cat("\nMothers by total non-employed waves and whether fragmented:\n")
print(as.data.frame(shape_support), row.names = FALSE)
write.csv(shape_support, file.path(FTE_DIR, "E_shape_support.csv"),
          row.names = FALSE)

both_cols <- intersect(c("multi_spell_0", "multi_spell_1"), names(shape_support))
if (length(both_cols) == 2) {
  ident_n <- shape_support %>%
    filter(multi_spell_0 > 0, multi_spell_1 > 0) %>%
    summarise(rows = n(), mothers = sum(multi_spell_0 + multi_spell_1))
  cat(sprintf("\nRows where both columns are populated: %d, covering %s mothers.\n",
              ident_n$rows, format(ident_n$mothers, big.mark = ",")))
  cat("That is the entire identifying variation for the shape contrast.\n")
}

# ---------------------------------------------------------------------
# E2. THE SHAPE TEST IS NOT REPORTED (R5).
#
# It is still fitted, because the MDE is the reason for dropping it and
# the reason has to be quotable. The estimate is withheld unconditionally
# rather than under BLIND, so that a later decision to report it cannot
# be made after seeing it. This mirrors the audit's treatment of D7.
# ---------------------------------------------------------------------
shp <- ev %>%
  filter(treated == 1, female == 1, k >= 5, k <= 8,
         !is.na(ln_hourly_wage_real)) %>%
  inner_join(geom %>% select(person_id, n_nonemp, longest_spell, n_spells),
             by = "person_id") %>%
  filter(!is.na(longest_spell))

if (nrow(shp) > 200) {
  m_shape <- fixest::feols(ln_hourly_wage_real ~ n_nonemp + longest_spell |
                             wave, data = shp, cluster = ~person_id,
                           notes = FALSE, warn = FALSE)
  ct <- tryCatch(fixest::coeftable(m_shape), error = function(e) NULL)
  cat(sprintf("\nShape test cell: %s person-waves, %s mothers\n",
              format(nrow(shp), big.mark = ","),
              format(dplyr::n_distinct(shp$person_id), big.mark = ",")))
  if (is.null(ct) || !("longest_spell" %in% rownames(ct))) {
    cat("  longest_spell was dropped from the fit, so the shape contrast is\n")
    cat("  not identified in this cell at all. Nothing to report.\n")
  } else {
    se_shape <- unname(ct["longest_spell", "Std. Error"])
    .blind[["shape_longest_spell_WITHHELD"]] <- ct["longest_spell", , drop = FALSE]
    if ("n_nonemp" %in% rownames(ct))
      .blind[["shape_total_nonemp_WITHHELD"]] <- ct["n_nonemp", , drop = FALSE]
    # Per wave of spell the MDE looks smaller than the penalty, which
    # invites the wrong conclusion. The comparable quantity is the MDE
    # evaluated at the spell length mothers actually have.
    ml <- mean(geom$longest_spell[geom$female == 1 & geom$n_post_waves >= 4],
               na.rm = TRUE)
    cat(sprintf("  MDE on longest_spell: %.4f log points per wave of spell\n",
                mde(se_shape)))
    cat(sprintf("  Mean longest spell among mothers: %.2f waves\n", ml))
    cat(sprintf("  Smallest detectable effect at that spell length: %.3f log points\n",
                mde(se_shape) * ml))
    cat(sprintf("  Ten-year penalty for comparison: %.3f log points\n",
                MS_PENALTY10))
    cat(sprintf("  Ratio: %.2f\n", mde(se_shape) * ml / MS_PENALTY10))
    # v21.11. TWO THINGS THAT RATIO GETS WRONG, both in the same
    # direction, so the decision below is safe and understated.
    #
    # THE SCALE. longest_spell enters beside n_nonemp, so its coefficient
    # is identified holding total non-employment FIXED: it is the effect
    # of concentrating a given amount of absence into one longer spell.
    # The relevant scale is therefore the WITHIN-cell spread in
    # longest_spell, not its mean. Multiplying by the mean compares a
    # mother at the average spell against one with no spell at all, which
    # is a level contrast the design does not identify.
    #
    # THE EFFECTIVE SAMPLE. The MDE is computed on every row in the cell,
    # and only the cells holding BOTH a single-spell and a multi-spell
    # mother contribute identifying variation. The block above counted
    # those: 9 cells, 354 mothers. The rest are in cells with no contrast
    # and inflate the apparent precision.
    #
    # THE READING THAT NEEDS NEITHER. The coefficient is per wave, so the
    # MDE IS the answer without any multiplication: it says the design
    # cannot see an atrophy rate below that many log points per wave of
    # continuous absence. Read it that way and the ratio near one stops
    # inviting "close enough to try harder".
    if (exists("ident_n") && is.data.frame(ident_n) && nrow(ident_n))
      cat(sprintf(paste("  Effective sample for the CONTRAST: %s mothers in",
                        "%d cells, not the\n  %s in the cell above. The",
                        "MDE is computed on all of them.\n"),
                  format(ident_n$mothers[1], big.mark = ","),
                  ident_n$rows[1], format(nrow(shp), big.mark = ",")))
    cat(sprintf(paste("  DIRECT READING, no spell-length multiplication: this",
                      "design cannot\n  detect atrophy below %.1f%% per wave",
                      "of continuous absence. No rate\n  anyone proposes is",
                      "that large, so the ratio above understates how far\n",
                      " short the design falls.\n"),
                100 * mde(se_shape)))
    cat("  DECISION: not reported. At the spell length mothers actually\n")
    cat("  have, the smallest shape effect this design could detect is of\n")
    cat("  the order of the entire ten-year penalty, so a null here is\n")
    cat("  uninformative about atrophy. Say in the text that the data\n")
    cat("  cannot separate atrophy from forgone accumulation on spell shape,\n")
    cat("  and point to the depreciation test in D3 instead. Estimates are\n")
    cat("  withheld unconditionally, not under BLIND.\n")
  }
} else {
  cat("\nShape test cell too thin to estimate; reporting support only.\n")
}

# =====================================================================
# WRITE THE ADDENDUM
# Keyed on person_id and wave so it left-joins onto the slim panel
# without touching the loader. The event studies in 02_cs_event_study.R
# can then be re-run with exp_fte_obs in place of experience_years, and
# 03's T8 reads exp_hybrid_one_obs and exp_hybrid_fte_obs from here rather
# than rebuilding them (G2).
#
# The join is on rows with a computable weight only, so a consumer that
# left-joins gets NA on the person-waves counted in Section B and a
# consumer that inner-joins silently drops them. Either way the count
# belongs in the sample definition (G3).
# =====================================================================
# v29. THE ADDENDUM RECORDS WHICH PANEL IT WAS BUILT FROM.
#
# 03c compares the addendum's mtime against the panel's and warns when the
# addendum is older, which is the right question asked with the wrong
# instrument. mtime answers "which file was written last", not "were these
# two built from the same data". On 9 September it reported the addendum as
# predating the panel, and it could not say whether that mattered: the panel
# had been rewritten the day before, and a rewrite that changes nothing but
# the timestamp looks identical from outside to one that changes a screen.
#
# THE FILE KNEW AND DID NOT SAY. This script opens the panel, so it is the
# one place that can record what it opened. Three fields settle it: the path,
# the panel's mtime AT THE TIME OF READING, and its build_tag, which the
# loader stamps and 00compat.R already checks its guards against. A reader
# comparing build_tags is comparing content; a reader comparing mtimes is
# comparing clocks.
#
# ATTRIBUTES, NOT COLUMNS. They survive saveRDS, cost nothing per row, and
# are invisible to every consumer that reads this frame by column name, so
# nothing downstream changes until something chooses to look.
attr(addendum, "panel_source")    <- normalizePath(PANEL, winslash = "/",
                                                   mustWork = FALSE)
attr(addendum, "panel_mtime")     <- .pan_mtime_at_read
attr(addendum, "panel_build_tag") <- .pan_tag_at_read
attr(addendum, "built_at")        <- Sys.time()
attr(addendum, "built_by")        <- "02fte_experience.R"
saveRDS(addendum, file.path(FTE_DIR, "panel_fte_addendum.rds"))
cat("  provenance stamped on the addendum:\n",
    "    panel      ", attr(addendum, "panel_source"), "\n",
    "    panel mtime", format(.pan_mtime_at_read, "%Y-%m-%d %H:%M:%S"), "\n",
    "    build_tag  ", .pan_tag_at_read, "\n",
    "  03c can compare these against the panel it resolves, which answers\n",
    "  whether the two were built from the same data rather than which\n",
    "  file was written last.\n", sep = "")
saveRDS(.blind, file.path(FTE_DIR, "BLIND_fte_estimates.rds"))
cat(sprintf("\nWrote panel_fte_addendum.rds: %s rows, %d columns\n",
            format(nrow(addendum), big.mark = ","), ncol(addendum)))
# v21.11. THE ADDENDUM IS SHORTER THAN THE PANEL AND SAID SO NOWHERE.
# addendum applies filter(weight_obs == 1), so the rows where the FTE
# weight is not observed are absent rather than flagged. 03 LEFT-JOINS
# this onto the panel, which gives those person-waves NA on all 26
# columns, and T8's complete-case step then drops them without comment.
# Small, and it should be a known number rather than an inferred one.
.add_gap <- nrow(panel) - nrow(addendum)
cat(sprintf(paste("  %s of the panel's %s person-waves are ABSENT, not NA:",
                  "weight_obs\n  is 0 or missing for them, so the",
                  "filter above removed them. A left_join\n  in 03 gives",
                  "them NA on every column here and T8 complete-cases",
                  "them\n  out. That is %.2f%% of the panel and it is",
                  "deliberate; it is reported so\n  the drop is not",
                  "rediscovered downstream.\n"),
            format(.add_gap, big.mark = ","),
            format(nrow(panel), big.mark = ","),
            100 * .add_gap / nrow(panel)))
if (all(addendum$weight_obs == 1L, na.rm = TRUE))
  cat("  NOTE weight_obs is CONSTANT at 1 here, because the filter that\n",
      "  created the addendum is the test the flag reports. The comment at\n",
      "  the construction says the flag travels 'so the restriction is\n",
      "  visible'; the filter is precisely what makes it invisible. Read\n",
      "  the unfiltered frame from `grid` if you need the 0s.\n", sep = "")
rm(.add_gap)
cat("Columns 03 reads from it, and what each one is:\n")
key_cols <- data.frame(
  column = c("exp_hybrid_cal", "exp_hybrid_one_obs", "exp_hybrid_fte_obs",
             "exp_hybrid_fte", "exp_fte_obs", "base_exp_cal",
             "in_panel", "weight_obs"),
  weight = c("calendar", "one-sided", "two-sided", "two-sided", "two-sided",
             "calendar base", "flag", "flag"),
  gap    = c("obs", "obs", "obs", "INTERPOLATED", "obs", "pre-panel",
             "flag", "flag"),
  stringsAsFactors = FALSE)
key_cols$present <- key_cols$column %in% names(addendum)
print(key_cols, row.names = FALSE)
if (!all(key_cols$present)) {
  cat("*** Some column 03 expects is absent. T8 there will fall back to\n")
  cat("    building the one-sided stock locally, which is what revision 3\n")
  cat("    exists to stop. Fix the select list above before running 03.\n")
} else {
  cat("All of them are present, so 03 reads its stocks from here and does\n")
  cat("not rebuild any of them (G2).\n")
}
cat("Pair a stock only with another stock in the same gap column.\n")

cat("\n--- What to do with this ---\n")
cat(sprintf(paste("1. Lead the section with gap_ft and gap_pt at k = %d, as",
                  "changes from\n   k = %d. No normalisation is involved, so",
                  "nothing in the claim\n   depends on the full-time week or",
                  "the cap.\n"), K_HI, KBASE))
cat("2. Put the weighting grid in the appendix and say which rule the\n")
cat("   text uses. If the concave and capped rules differ materially,\n")
cat("   lead with the concave one: the cap's mass point is an artefact.\n")
cat("3. Report the FTE series as the price of the substitution, not as\n")
cat("   the evidence for it, and use the gap-blind one. The interpolated\n")
cat("   series belongs in the appendix beside the gap-credit table, because\n")
cat("   the difference between the two is a gap treatment and not a unit.\n")
cat("4. Drop the spell-shape test and say why, in one sentence, with the\n")
cat("   MDE. Keep the depreciation test.\n")
cat("5. State the return-to-experience result as a bound, not as a null.\n")
cat("6. To test whether FTE units shrink the pooled gap, put\n")
cat("   exp_hybrid_one_obs into the DML specification, which is the\n")
cat("   one-sided gap-blind stock and the one the hypothesis names. 03's T8\n")
cat("   does that and gates the result on whether the two treatments\n")
cat("   diverge enough to be told apart. This script cannot answer it (G9).\n")
cat("7. Report the row count the addendum drops as part of the sample\n")
cat("   definition, not as a footnote. It is selected on hours (G3).\n")
if (BLIND)
  cat("Wage-equation point estimates were withheld to BLIND_fte_estimates.rds.\n")

# CLOSE IT, UNCONDITIONALLY. A sink left open sends the NEXT script's
# output into this file, and where that script uses 03_logging.R it will
# also decide it is inheriting a caller and write no transcript at all.
# That is not a tidy-up step; it is the difference between 03c logging its
# run and 03c appearing to have produced nothing.
cat("\n", strrep("=", 78), "\n02fte_experience.R  completed ",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", strrep("=", 78), "\n",
    sep = "")
while (sink.number() > 0) sink()

# v28. RE-RESOLVE THE DESTINATION NOW THAT THE GUARDS HAVE LOADED.
#
# The sink opened before 00_contract.R, so .fte_logdir was built from
# whatever root existed at that moment, which is getwd() when this script
# is run on its own. By here the contract has set PROJ_ROOT, so the
# question "where should this transcript be" has an answer it did not
# have at the top of the file. If the two disagree the log is copied to
# the right place and both paths are named, because a transcript in the
# wrong run_logs is worse than one in a data directory: at least the data
# directory is somewhere a person looks.
#
# COPY RATHER THAN MOVE. The file was just closed but the platform may
# still hold it, and a failed rename at the last line of a long run is a
# poor trade for tidiness. The stray copy under the guessed root is
# obvious and harmless.
.fte_logdir_final <- if (exists("FTE_LOG_OUT")) FTE_LOG_OUT else
  if (exists("DIR_LOGS")) DIR_LOGS else
    file.path(if (exists("PROJ_ROOT")) PROJ_ROOT else .fte_root, "run_logs")
.np <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
if (!identical(.np(.fte_logdir_final), .np(dirname(.fte_log)))) {
  dir.create(.fte_logdir_final, showWarnings = FALSE, recursive = TRUE)
  .moved_to <- file.path(.fte_logdir_final, basename(.fte_log))
  if (isTRUE(file.copy(.fte_log, .moved_to, overwrite = TRUE))) {
    cat("Log opened under a provisional root and copied to the resolved one.\n")
    cat("  opened at : ", .fte_log, "\n", sep = "")
    cat("  copied to : ", .moved_to, "\n", sep = "")
    cat("  This happens when 02 runs before the contract sets PROJ_ROOT.\n")
    cat("  The copy is the one to keep; delete the original.\n")
  } else {
    cat("*** Could not copy the log to ", .fte_logdir_final, ".\n", sep = "")
    cat("    It is at ", .fte_log, " and that is where to look for it.\n", sep = "")
  }
  rm(.moved_to)
} else {
  cat("Log written to ", .fte_log, "\n", sep = "")
}
rm(.fte_logdir_final, .np)

if (sink.number() != 0)
  cat("*** SINKS STILL OPEN: ", sink.number(), ". The next script will\n",
      "    write into this file instead of its own.\n", sep = "")
rm(.fte_log, .fte_logdir)
cat("=====================================================================\n")

# v28. The bare sink() that used to close this file is removed. The loop
# above has already popped every sink, so this called sink() with nothing
# to remove and R errors on that. At the last line of a script it turns a
# completed run into a non-zero exit under Rscript, which is exactly the
# wrong signal from a block whose entire purpose is leaving the sink stack
# clean. Guarded rather than deleted, in case a caller opened one.
if (sink.number() > 0) sink()
