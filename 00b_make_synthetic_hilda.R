# =============================================================================
# 00b_make_synthetic_hilda.R
#
# Writes SYNTHETIC stand-ins for the analysis panels that LOADING_ND3_v3.R
# (build v21.49) writes from the restricted HILDA release:
#
#   hilda_panel_data_W12_W24_slim.rds   every design (waves 12 to 24)
#   hilda_panel_data_extended.rds       the wave 15 to 24 subset
#
# Purpose: let a Data Editor, a referee, or anyone without HILDA access run
# the pipeline end to end and confirm that the code executes. The data are
# random. They carry the same 414 columns as the v21.49 panel, in the same
# order, with the same types, factor levels and panel attributes, and they
# build in the structure the designs depend on:
#   - an unbalanced panel with late entry, attrition and a wave-24 refresher;
#   - absorbing parenthood on own children only, with treated persons (first
#     birth inside the panel), never-treated persons and left-censored
#     parents, so the event-study roster forms as it does on the real data;
#   - couples linked by partner_id, both partners of an in-panel first birth
#     sharing the birth wave;
#   - an employment-routed training module: training_module_asked, the _any
#     and _obs twins, the gate, the dose, fee and aim items;
#   - a fall in employer-financed (paid-time) training for mothers at the
#     first birth, a smaller fall in own-time training, part of the fall
#     running through a move to part-time hours, and no change for fathers;
#   - a wage gap that stays small for four years and then opens;
#   - module items (fertility, wealth) only in their module waves, and
#     entrant-only items only for new entrants.
# They do NOT reproduce any published number. Every reported result needs
# the real HILDA release. The panel carries attr(, "synthetic") = TRUE.
#
# Usage, from the repository root:
#   Rscript 00b_make_synthetic_hilda.R                # 6,000 persons
#   N_PERSONS=12000 Rscript 00b_make_synthetic_hilda.R
#   SYNTH_SEED=7 Rscript 00b_make_synthetic_hilda.R   # a different draw
#
# The files go where the loader puts them: PROJ_ROOT when 00_contract.R is
# loaded, else HILDA_ROOT when set, else the working directory.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
})

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

.syn_t0 <- Sys.time()
SYNTH_SEED <- suppressWarnings(as.integer(Sys.getenv("SYNTH_SEED", "20261021")))
if (is.na(SYNTH_SEED)) SYNTH_SEED <- 20261021L
set.seed(SYNTH_SEED)

n_persons <- suppressWarnings(as.integer(Sys.getenv("N_PERSONS", "6000")))
if (is.na(n_persons) || n_persons < 1000L) n_persons <- 6000L

OUT_DIR <- if (exists("PROJ_ROOT", envir = globalenv())) {
  get("PROJ_ROOT", envir = globalenv())
} else if (nzchar(Sys.getenv("HILDA_ROOT"))) {
  Sys.getenv("HILDA_ROOT")
} else getwd()

cat(strrep("=", 72), "\n", sep = "")
cat("  00b_make_synthetic_hilda.R  --  SYNTHETIC DATA (results are not real)\n")
cat(sprintf("  %s persons, waves 12-24, seed %d\n",
            format(n_persons, big.mark = ","), SYNTH_SEED))
cat(strrep("=", 72), "\n", sep = "")

# ── 0. CONSTANTS (the loader's values) ───────────────────────────────────────
BUILD_TAG <- "v21.49"
WAVES <- 12:24
NW    <- length(WAVES)
FIRM_SIZE_BREAK_WAVE              <- 22L
STUDY_ITEM_BREAK_WAVE             <- 24L
EDUC_COMPOSITION_BREAK_WAVES      <- 24L
WEALTH_MODULE_WAVES               <- c(14L, 18L, 22L)
FERTILITY_MODULE_WAVES            <- c(15L, 19L, 23L)
TRAINING_GATE_POSITION_BREAK_WAVE <- 20L
OCC_ORIGIN_BREAK_WAVE             <- 20L
OLF_LEVEL_COMPARABLE              <- FALSE
SUPER_IS_HOUSEHOLD                <- TRUE

# WPI deflators, base wave 24 = 2024 (identical to the loader and the master)
DEFLATOR <- 149.6 / c(110.9, 114.6, 117.6, 120.4, 123.0, 125.4, 127.9,
                      130.9, 133.7, 135.7, 138.9, 143.7, 149.6)

clamp <- function(x, lo, hi) pmin(hi, pmax(lo, x))
rbern <- function(p) as.integer(stats::runif(length(p)) < p)
pick  <- function(n, x, prob = NULL)
  x[sample.int(length(x), n, replace = TRUE, prob = prob)]
in_rng <- function(a, lo, hi) !is.na(a) & a >= lo & a <= hi
# Lag within person on a frame sorted by person then wave.
lag_in <- function(x, id) {
  y <- c(NA, x[-length(x)])
  y[c(TRUE, id[-1] != id[-length(id)])] <- NA
  y
}
to_mat <- function(x) matrix(x, nrow = length(x) / NW, ncol = NW, byrow = TRUE)
to_vec <- function(m) as.vector(t(m))

# ── 1. PERSONS AND HOUSEHOLD UNITS ───────────────────────────────────────────
# Units are couples or singles. Treated couples share a first-birth wave, so
# mothers and fathers of the same child sit in the two arms of the design.
mk_units <- function(type, n_couple, n_single) {
  data.frame(
    type      = type,
    unit      = paste0(type, "_", c(rep(seq_len(n_couple), each = 2L),
                                    n_couple + seq_len(n_single))),
    in_couple = c(rep(TRUE, 2L * n_couple), rep(FALSE, n_single)),
    female    = c(rep(c(1L, 0L), n_couple), rbinom(n_single, 1L, 0.5)),
    stringsAsFactors = FALSE)
}
n_tc <- round(0.085 * n_persons); n_ts <- round(0.060 * n_persons)
n_nc <- round(0.070 * n_persons); n_ns <- round(0.240 * n_persons)
n_cc <- round(0.120 * n_persons)
n_cs <- n_persons - 2L * (n_tc + n_nc + n_cc) - n_ts - n_ns
stopifnot(n_cs > 0L)
P <- rbind(mk_units("treated", n_tc, n_ts), mk_units("never", n_nc, n_ns),
           mk_units("censored", n_cc, n_cs))
P <- P[sample.int(nrow(P)), ]
rownames(P) <- NULL
N <- nrow(P)
P$person_id <- sprintf("%07d", 100000L + seq_len(N))

U <- P[!duplicated(P$unit), c("unit", "type", "in_couple")]
nU <- nrow(U)
u_t <- U$type == "treated"; u_c <- U$type == "censored"
U$entry <- ifelse(u_t,
                  pick(nU, 12:22, c(0.62, rep(0.038, 10))),
                  pick(nU, 12:24, c(0.58, rep(0.0375, 8), rep(0.05 / 3, 3), 0.07)))
U$exit <- pmin(24L, U$entry + ifelse(u_t, 2L, 0L) +
                 rgeom(nU, ifelse(u_t, 0.040, 0.045)))
U$g <- NA_integer_
U$g[u_t] <- as.integer(U$entry[u_t] + 1L +
                         floor(stats::runif(sum(u_t)) *
                                 (U$exit[u_t] - U$entry[u_t])))
U$ab_f <- round(ifelse(u_t, clamp(rnorm(nU, 30.5, 4.5), 21, 44),
                       clamp(rnorm(nU, 28.5, 5.0), 18, 44)))
U$age12_base <- stats::runif(nU, 14, 53)
U$state_code <- pick(nU, 1:8, c(.32, .26, .20, .07, .10, .02, .01, .02))
U$household_id <- sprintf("%06d", 300000L + seq_len(nU))

ui <- match(P$unit, U$unit)
for (cl in c("entry", "exit", "g", "ab_f", "age12_base", "state_code",
             "household_id")) P[[cl]] <- U[[cl]][ui]

# Age at first birth by person: a partner is about two years older than the
# mother; a single father is two years older than the unit draw.
P$ab_p <- ifelse(P$female == 1L, P$ab_f,
                 clamp(P$ab_f + ifelse(P$in_couple, round(rnorm(N, 2, 3)), 2),
                       20, 55))
P$ab_p[P$type == "never"] <- NA_real_
P$age12_p <- ifelse(P$type == "never",
                    P$age12_base + ifelse(P$female == 1L, 0, 2), NA_real_)

# Left-censored parents: the first child was born before entry, young enough
# that the older partner is at most 55 at entry.
ab_max <- tapply(P$ab_p, P$unit, function(z) if (all(is.na(z))) NA_real_ else max(z, na.rm = TRUE))
U$ab_max <- as.numeric(ab_max[U$unit])
U$fb <- U$g
c_draw <- pick(nU, 1:28, prob = rev(seq_len(28)) + 10)
c_cap  <- pmax(1, 55 - U$ab_max)
U$fb[u_c] <- as.integer(U$entry[u_c] - pmin(c_draw[u_c], c_cap[u_c]))
U$sb <- ifelse(!is.na(U$fb) & stats::runif(nU) < 0.45,
               U$fb + pick(nU, 2:5), NA_integer_)
P$fb <- U$fb[ui]; P$sb <- U$sb[ui]

# Couple links: each partner carries the other's person_id.
P$partner_id <- NA_character_
cp <- which(P$in_couple)
mate <- split(cp, P$unit[cp])
for (m in mate) if (length(m) == 2L) {
  P$partner_id[m[1]] <- P$person_id[m[2]]
  P$partner_id[m[2]] <- P$person_id[m[1]]
}

# Persistent person traits.
EDUC_VALUES <- c(9, 10, 11, 12, 12.5, 13, 13.5, 14, 14.5, 15, 16, 16.5, 17, 18, 20)
EDUC_PROB   <- c(.02, .07, .05, .17, .03, .13, .06, .03, .05, .06, .15, .04,
                 .06, .06, .02)
P$educ0 <- pick(N, EDUC_VALUES, EDUC_PROB)
P$educ_up_wave <- ifelse(stats::runif(N) < 0.06, pick(N, 14:23), NA_integer_)
P$educ_up <- pmin(20, P$educ0 + pick(N, c(1, 1.5, 2, 3)))
P$u_wage <- rnorm(N, 0, 0.28)
P$a_emp  <- rnorm(N, 0, 1.0)
P$b_ft   <- rnorm(N, 0, 0.8)
P$v_e1   <- rnorm(N, 0, 0.09)
P$w_o1   <- rnorm(N, 0, 0.03)
P$t_oth  <- rnorm(N, 0, 0.05)
P$public <- rbern(ifelse(P$female == 1L, 0.30, 0.20))
P$union  <- rbern(0.20)
P$selfemp <- rbern(0.08)
P$selfemp_mum <- rbern(0.05)
P$proxy  <- rbern(ifelse(P$female == 1L, 0.044, 0.052))
P$share  <- clamp(ifelse(P$female == 0L, rnorm(N, 0.93, 0.05),
                         ifelse(P$type == "censored", rnorm(N, 0.72, 0.12),
                                rnorm(N, 0.89, 0.07))), 0.2, 1)
P$stepkid <- P$type == "never" & !P$in_couple & stats::runif(N) < 0.04
P$step_s0 <- ifelse(P$stepkid, pick(N, -3:12), NA_integer_)
P$sep_father <- P$female == 0L & P$type == "censored" & !P$in_couple &
  stats::runif(N) < 0.40
P$adult_res <- stats::runif(N) < 0.55
P$mother_educ <- ifelse(stats::runif(N) < 0.05, NA,
                        pick(N, c(8, 10, 11, 12, 13, 15, 16), c(.1, .25, .1, .2, .15, .08, .12)))
P$father_educ <- ifelse(stats::runif(N) < 0.07, NA,
                        pick(N, c(8, 10, 11, 12, 13, 15, 16), c(.1, .2, .1, .18, .2, .08, .14)))
P$weight_p <- stats::rlnorm(N, log(1050), 0.45)
P$lweight_p <- stats::rlnorm(N, log(950), 0.50)

# ── 2. PERSON BY WAVE GRID ───────────────────────────────────────────────────
# Everything that accumulates over time is computed on the full grid, so
# stocks keep growing across waves a person is not interviewed. Rows the
# panel does not observe are dropped in step 9.
G <- data.frame(i = rep(seq_len(N), each = NW), wave = rep(WAVES, times = N))
pi_ <- G$i
G$female <- P$female[pi_]
typ <- P$type[pi_]
fem <- G$female == 1L
fb <- P$fb[pi_]; sb <- P$sb[pi_]; gg <- P$g[pi_]
G$age <- as.integer(ifelse(is.na(P$ab_p[pi_]),
                           round(P$age12_p[pi_]) + (G$wave - 12L),
                           P$ab_p[pi_] + (G$wave - fb)))
entry <- P$entry[pi_]; exit <- P$exit[pi_]
gap <- rbern(rep(0.03, nrow(G))) == 1L & G$wave > entry & G$wave < exit
G$obs <- G$wave >= entry & G$wave <= exit & !gap & G$age >= 20L & G$age <= 55L

educ_ref <- ifelse(!is.na(P$educ_up_wave[pi_]) & G$wave >= P$educ_up_wave[pi_],
                   P$educ_up[pi_], P$educ0[pi_])

# ── 3. CHILDREN AND PARENTHOOD ───────────────────────────────────────────────
c1 <- ifelse(!is.na(fb) & G$wave >= fb, G$wave - fb, NA_integer_)
c2 <- ifelse(!is.na(sb) & G$wave >= sb, G$wave - sb, NA_integer_)
st <- ifelse(P$stepkid[pi_], P$step_s0[pi_] + (G$wave - 12L), NA_integer_)
st[!in_rng(st, 0, 17)] <- NA_integer_
sepf <- P$sep_father[pi_]; ares <- P$adult_res[pi_]
res_of <- function(ca) !is.na(ca) & !sepf & (ca <= 17 | (ca <= 24 & ares))
r1 <- res_of(c1); r2 <- res_of(c2)
nr1 <- !is.na(c1) & !r1; nr2 <- !is.na(c2) & !r2

G$own_res_children_0to4    <- (in_rng(c1, 0, 4) & r1) + (in_rng(c2, 0, 4) & r2)
G$own_res_children_5to14   <- (in_rng(c1, 5, 14) & r1) + (in_rng(c2, 5, 14) & r2)
G$own_res_children_15to24  <- (in_rng(c1, 15, 24) & r1) + (in_rng(c2, 15, 24) & r2)
G$own_res_children_25plus  <- (in_rng(c1, 25, 99) & r1) + (in_rng(c2, 25, 99) & r2)
G$own_nonres_children_0to4    <- (in_rng(c1, 0, 4) & nr1) + (in_rng(c2, 0, 4) & nr2)
G$own_nonres_children_5to14   <- (in_rng(c1, 5, 14) & nr1) + (in_rng(c2, 5, 14) & nr2)
G$own_nonres_children_15to24  <- (in_rng(c1, 15, 24) & nr1) + (in_rng(c2, 15, 24) & nr2)
G$own_nonres_children_25plus  <- (in_rng(c1, 25, 99) & nr1) + (in_rng(c2, 25, 99) & nr2)
for (cl in grep("^own_(non)?res_children_", names(G), value = TRUE))
  G[[cl]] <- as.numeric(G[[cl]])

G$num_children_0to4   <- as.numeric(G$own_res_children_0to4 + in_rng(st, 0, 4))
G$num_children_5to9   <- as.numeric((in_rng(c1, 5, 9) & r1) + (in_rng(c2, 5, 9) & r2) +
                                      in_rng(st, 5, 9))
G$num_children_10to14 <- as.numeric((in_rng(c1, 10, 14) & r1) + (in_rng(c2, 10, 14) & r2) +
                                      in_rng(st, 10, 14))
G$num_children_15to24 <- as.numeric(G$own_res_children_15to24 + in_rng(st, 15, 17))
G$num_children_under15 <- G$num_children_0to4 + G$num_children_5to9 +
  G$num_children_10to14
G$num_dependent_children <- G$num_children_under15 +
  as.numeric((in_rng(c1, 15, 17) & r1) + (in_rng(c2, 15, 17) & r2) + in_rng(st, 15, 17))
G$num_own_resident_children    <- as.numeric(r1 + r2)
G$num_own_nonresident_children <- as.numeric(nr1 + nr2)
G$total_own_children       <- G$num_own_resident_children + G$num_own_nonresident_children
G$total_own_children_narm0 <- G$total_own_children
G$total_children_ever_had  <- as.numeric((!is.na(c1)) + (!is.na(c2)))
G$total_children_died      <- 0
G$num_step_foster_grand    <- as.numeric(!is.na(st))
yc <- suppressWarnings(pmin(c1, c2, na.rm = TRUE))
G$age_youngest_own_child <- as.numeric(yc)

# Ages of resident children, oldest first, then the non-resident ones.
res_ages <- cbind(ifelse(r1, c1, NA), ifelse(r2, c2, NA), st)
nres_ages <- cbind(ifelse(nr1, c1, NA), ifelse(nr2, c2, NA))
sort_row <- function(m) t(apply(m, 1L, function(z) {
  z <- sort(z[!is.na(z)], decreasing = TRUE)
  c(z, rep(NA_real_, ncol(m) - length(z)))[seq_len(ncol(m))]
}))
ra <- sort_row(res_ages); na_ <- sort_row(nres_ages)
G$age_resident_child_1 <- as.numeric(ra[, 1]); G$age_resident_child_2 <- as.numeric(ra[, 2])
G$age_resident_child_3 <- as.numeric(ra[, 3])
G$age_nonresident_child_1 <- as.numeric(na_[, 1])
G$age_nonresident_child_2 <- as.numeric(na_[, 2])
G$has_nonresident_children_under18 <- (in_rng(c1, 0, 17) & nr1) | (in_rng(c2, 0, 17) & nr2)
G$has_any_nonresident_children <- (nr1 | nr2)

G$ever_parent <- as.integer(G$total_children_ever_had > 0)
k <- ifelse(typ == "treated", G$wave - gg, NA_integer_)
mpost <- fem & !is.na(k) & k >= 0
fpost <- !fem & !is.na(k) & k >= 0
mum_young <- fem & !is.na(yc) & yc <= 14

# ── 4. EMPLOYMENT AND HOURS ──────────────────────────────────────────────────
a_e <- P$a_emp[pi_]
mu_e <- ifelse(fem, 1.05, 1.35) + 0.06 * (educ_ref - 13) -
  0.60 * (G$age < 23) - 0.35 * (G$age >= 50) - 0.25 * (G$age >= 53)
pen <- ifelse(!fem | is.na(yc), 0,
              ifelse(yc == 0, -0.85, ifelse(yc <= 2, -0.70, ifelse(yc <= 4, -0.50,
                     ifelse(yc <= 9, -0.30, ifelse(yc <= 14, -0.15, 0))))))
pen <- pen + ifelse(!fem & !is.na(yc) & yc <= 4, 0.05, 0)
ar <- matrix(0, N, NW)
ar[, 1] <- rnorm(N, 0, 0.7 / sqrt(1 - 0.36))
for (j in 2:NW) ar[, j] <- 0.6 * ar[, j - 1] + rnorm(N, 0, 0.7)
G$employed <- as.numeric(a_e + mu_e + pen + to_vec(ar) > 0)

ft_pen <- ifelse(!fem | is.na(yc), 0,
                 ifelse(yc <= 4, -1.00, ifelse(yc <= 9, -0.75,
                        ifelse(yc <= 14, -0.45, -0.25))))
ft_lat <- P$b_ft[pi_] + ifelse(fem, 0.50, 1.40) - 0.50 * (G$age < 23) -
  0.20 * (G$age >= 52) + ft_pen + rnorm(nrow(G), 0, 0.6)
emp <- G$employed == 1
p_unemp <- ifelse(fem, ifelse(!is.na(yc) & yc <= 1, 0.08, 0.18), 0.35)
unemp <- !emp & stats::runif(nrow(G)) < p_unemp
G$esdtl_raw <- ifelse(emp & ft_lat > 0, 1, ifelse(emp, 2,
                      ifelse(unemp, pick(nrow(G), c(3, 4), c(.6, .4)),
                             pick(nrow(G), c(5, 6), c(.3, .7)))))
G$fulltime <- ifelse(G$esdtl_raw == 1, 1, 0)
G$parttime <- ifelse(G$esdtl_raw == 2, 1, 0)
nilf <- G$esdtl_raw %in% c(5, 6); unem <- G$esdtl_raw %in% c(3, 4)

hrs_ft <- clamp(round(38 + abs(rnorm(nrow(G), 0, 6)) + 3 * (!fem)), 35, 75)
hrs_pt <- clamp(round(rnorm(nrow(G), ifelse(mum_young, 18, 21), 7)), 1, 34)
G$hours_worked_clean <- ifelse(G$esdtl_raw == 1, hrs_ft,
                               ifelse(G$esdtl_raw == 2, hrs_pt, NA_real_))
sec <- emp & stats::runif(nrow(G)) < 0.06
sec_h <- ifelse(sec, pmin(G$hours_worked_clean - 1, round(stats::runif(nrow(G), 3, 15))), 0)
G$hours_worked_main_job <- ifelse(emp, G$hours_worked_clean - pmax(0, sec_h), NA_real_)
G$second_job_hours <- ifelse(emp, G$hours_worked_clean - G$hours_worked_main_job, NA_real_)

selfe <- emp & (P$selfemp[pi_] == 1L | (mum_young & P$selfemp_mum[pi_] == 1L))
G$emptype_raw <- ifelse(!emp, NA_real_,
                        ifelse(selfe, pick(nrow(G), c(2, 3, 4), c(.3, .5, .2)),
                               ifelse(stats::runif(nrow(G)) < 0.002, 6, 1)))

# ── 5. TRAINING MODULE ───────────────────────────────────────────────────────
emp_lag <- lag_in(G$employed, G$i)
asked <- ifelse(emp, rbern(rep(0.9999, nrow(G))),
                rbern(ifelse(!is.na(emp_lag) & emp_lag == 1, 0.33, 0.06)))
pub <- emp & P$public[pi_] == 1L
uni <- emp & P$union[pi_] == 1L
h <- G$hours_worked_clean
hrs_eff <- ifelse(is.na(h), 0, ifelse(h <= 15, -0.13, ifelse(h <= 24, -0.09,
                  ifelse(h <= 34, -0.04, ifelse(h <= 44, 0, 0.01)))))
mum_cens <- typ == "censored" & mum_young

p_e1 <- ifelse(fem, 0.29, 0.19) + 0.09 * pub + 0.06 * uni + hrs_eff +
  P$v_e1[pi_] + ifelse(fem, 0.0015 * (G$wave - 18), 0) +
  ifelse(mpost, ifelse(k <= 7, -0.075, -0.060), 0) - 0.03 * mum_cens
p_e1 <- ifelse(selfe, 0.3 * p_e1, p_e1)
p_e1 <- ifelse(emp, clamp(p_e1, 0.005, 0.90), 0.05)
p_o1 <- ifelse(fem, 0.080, 0.033) + P$w_o1[pi_] - 0.01 * (h <= 34 & !is.na(h)) +
  0.01 * pub - 0.0015 * (G$wave - 12) +
  ifelse(mpost, ifelse(k == 0, -0.05, -0.012), 0)
p_o1 <- ifelse(emp, clamp(p_o1, 0.002, 0.50), 0.03)
p_e2 <- ifelse(fem, 0.10, 0.085) + P$t_oth[pi_] + 0.5 * hrs_eff +
  ifelse(mpost, -0.035, 0)
p_e2 <- ifelse(emp, clamp(p_e2, 0.003, 0.60), 0.04)
p_o2 <- ifelse(fem, 0.055, 0.040) + 0.5 * P$t_oth[pi_] + ifelse(mpost, -0.012, 0)
p_o2 <- ifelse(emp, clamp(p_o2, 0.003, 0.50), 0.05)

ask <- asked == 1L
e1 <- ifelse(ask, rbern(p_e1), NA_integer_)
o1 <- ifelse(ask, rbern(p_o1), NA_integer_)
e2 <- ifelse(ask, rbern(p_e2), NA_integer_)
o2 <- ifelse(ask, rbern(p_o2), NA_integer_)
trained <- ask & (e1 == 1L | o1 == 1L | e2 == 1L | o2 == 1L)

G$training_module_asked <- as.integer(ask)
G$training_any_reported <- ifelse(ask, as.numeric(trained), NA_real_)
G$jttpewt_any <- ifelse(ask & e1 == 1L, 1, 0)
G$jttpeot_any <- ifelse(ask & o1 == 1L, 1, 0)
G$jttopwt_raw <- ifelse(trained, as.numeric(e2), NA_real_)
G$jttopot_raw <- ifelse(trained, as.numeric(o2), NA_real_)
G$training_paid_time <- ifelse(G$jttpewt_any == 1 | (!is.na(G$jttopwt_raw) & G$jttopwt_raw == 1), 1, 0)
G$training_own_time  <- ifelse(G$jttpeot_any == 1 | (!is.na(G$jttopot_raw) & G$jttopot_raw == 1), 1, 0)
G$jttpewt_obs <- ifelse(ask, G$jttpewt_any, NA_real_)
G$jttpeot_obs <- ifelse(ask, G$jttpeot_any, NA_real_)
G$training_own_time_obs  <- ifelse(ask, G$training_own_time, NA_real_)
G$training_paid_time_obs <- ifelse(ask, G$training_paid_time, NA_real_)
G$any_training <- as.numeric(G$training_own_time == 1 | G$training_paid_time == 1)

nG <- nrow(G)
courses <- pmin(30, 1 + rpois(nG, 1.1))
courses <- ifelse(stats::runif(nG) < 0.01, pick(nG, c(10, 12, 15, 20)), courses)
days <- clamp(round(exp(rnorm(nG, log(3), 1.0))), 1, 250)
days <- ifelse(mpost, pmax(1, round(0.85 * days)), days)
days <- ifelse(fpost & k == 0, pmax(1, round(0.85 * days)), days)
hpd  <- pick(nG, c(2, 3, 4, 5, 6, 7, 7.5, 8), c(.05, .08, .12, .12, .18, .20, .15, .10))
jttnum <- ifelse(trained, courses, NA_real_)
jttdays <- ifelse(trained, days, NA_real_)
jtthrs <- ifelse(trained, hpd, NA_real_)
G$total_training_days <- dplyr::coalesce(jttdays, 0)
G$avg_training_hours  <- jtthrs
G$total_courses       <- dplyr::coalesce(jttnum, 0)
G$total_training_hours <- ifelse(is.na(jttdays) | is.na(jtthrs), NA_real_, jttdays * jtthrs)
z_twin <- function(v) ifelse(!ask, NA_real_, ifelse(G$any_training == 0, 0, v))
G$total_training_hours_z <- z_twin(G$total_training_hours)
G$total_training_days_z  <- z_twin(jttdays)
G$total_courses_z        <- z_twin(jttnum)

own_only <- trained & G$training_own_time == 1 & G$training_paid_time == 0
p_fee <- 0.17 + 0.03 * fem + 0.03 * (fem & G$ever_parent == 1) + 0.15 * own_only -
  0.04 * (trained & G$training_paid_time == 1 & G$training_own_time == 0)
G$training_self_funded   <- ifelse(trained, as.numeric(rbern(p_fee)), NA_real_)
G$training_self_funded_z <- z_twin(G$training_self_funded)

aim_nr <- ifelse(trained, as.numeric(stats::runif(nG) < 0.01), NA_real_)
aim <- function(p) ifelse(trained & aim_nr == 0, as.numeric(rbern(p)), NA_real_)
G$training_aim_nonresponse <- aim_nr
G$training_aim_future     <- aim(rep(0.30, nG))
G$training_aim_current    <- aim(rep(0.65, nG))
G$training_aim_general    <- aim(rep(0.45, nG))
G$training_aim_getstarted <- aim(rep(0.08, nG))
G$training_aim_maintain   <- aim(ifelse(mpost, 0.31, 0.35))
G$training_aim_safety     <- aim(rep(0.25, nG))
G$aim_battery_asked <- as.integer(trained)
G$promotion_training   <- G$training_aim_future
G$maintenance_training <- G$training_aim_maintain
G$promotion_training_wide <- ifelse(is.na(G$training_aim_future) & is.na(G$training_aim_getstarted),
                                    NA_real_, as.numeric(dplyr::coalesce(G$training_aim_future, 0) == 1 |
                                                           dplyr::coalesce(G$training_aim_getstarted, 0) == 1))
G$maintenance_training_wide <- ifelse(is.na(G$training_aim_maintain) & is.na(G$training_aim_safety),
                                      NA_real_, as.numeric(dplyr::coalesce(G$training_aim_maintain, 0) == 1 |
                                                             dplyr::coalesce(G$training_aim_safety, 0) == 1))
G$promotion_training_z        <- z_twin(G$promotion_training)
G$maintenance_training_z      <- z_twin(G$maintenance_training)
G$promotion_training_wide_z   <- z_twin(G$promotion_training_wide)
G$maintenance_training_wide_z <- z_twin(G$maintenance_training_wide)
G$training_aim_current_z      <- z_twin(G$training_aim_current)
G$training_aim_general_z      <- z_twin(G$training_aim_general)
G$training_gate_regime <- ifelse(G$wave < TRAINING_GATE_POSITION_BREAK_WAVE,
                                 "E1_w7_w19", "E6_w20_w24")

# ── 6. EXPERIENCE STOCKS AND TENURE ──────────────────────────────────────────
yse_m <- to_mat(pmax(0, G$age - educ_ref - 5))
emp_m <- to_mat(as.numeric(emp)); un_m <- to_mat(as.numeric(unem))
olf_m <- to_mat(as.numeric(nilf))
share <- P$share
act <- matrix(0, N, NW); une <- matrix(0, N, NW); olf <- matrix(0, N, NW)
act[, 1] <- share * yse_m[, 1]
une[, 1] <- (1 - share) * yse_m[, 1] * ifelse(P$female == 1L, 0.2, 0.4)
olf[, 1] <- (1 - share) * yse_m[, 1] - une[, 1]
for (j in 2:NW) {
  inc <- pmax(0, yse_m[, j] - yse_m[, j - 1])
  act[, j] <- act[, j - 1] + inc * emp_m[, j]
  une[, j] <- une[, j - 1] + inc * un_m[, j]
  olf[, j] <- olf[, j - 1] + inc * olf_m[, j]
}
prox <- P$proxy[pi_] == 1L
G$years_since_education <- ifelse(prox, NA_real_, round(to_vec(yse_m), 1))
G$actual_experience <- ifelse(prox, NA_real_, round(to_vec(act), 1))
G$years_unemployed  <- ifelse(prox, NA_real_, round(to_vec(une), 1))
G$years_not_in_lf   <- ifelse(prox, NA_real_, round(to_vec(olf), 1))
G$potential_experience <- pmax(0, G$age - educ_ref - 5)
G$experience_years <- pmin(dplyr::coalesce(G$actual_experience, G$potential_experience),
                           pmax(0, G$age - 13))
G$experience_years <- ifelse(G$experience_years < 0, 0, G$experience_years)

jc_p <- 0.10 + 0.05 * (mpost & k <= 1)
jc <- to_mat(as.numeric(stats::runif(nG) < jc_p))
oc <- to_mat(as.numeric(stats::runif(nG) < 0.04))
ten <- matrix(0, N, NW); tocc <- matrix(0, N, NW); jchg <- matrix(0, N, NW)
ten[, 1] <- ifelse(emp_m[, 1] == 1, floor(stats::runif(N) * pmin(15, act[, 1] + 1)), 0)
tocc[, 1] <- ifelse(emp_m[, 1] == 1, pmax(ten[, 1], floor(stats::runif(N) * pmin(20, act[, 1] + 1))), 0)
for (j in 2:NW) {
  stay <- emp_m[, j] == 1 & emp_m[, j - 1] == 1
  jchg[, j] <- as.numeric(stay & jc[, j] == 1)
  ten[, j]  <- ifelse(emp_m[, j] == 0, 0, ifelse(!stay | jc[, j] == 1, 0, ten[, j - 1] + 1))
  tocc[, j] <- ifelse(emp_m[, j] == 0, 0, ifelse(!stay | oc[, j] == 1, 0, tocc[, j - 1] + 1))
}
G$tenure_employer   <- to_vec(ten)
G$tenure_occupation <- pmax(to_vec(tocc), G$tenure_employer)
G$job_change <- to_vec(jchg)

# ── 7. WAGES AND EARNINGS ────────────────────────────────────────────────────
ex <- G$experience_years
gap_k <- ifelse(mpost & k >= 5, -0.011 * (k - 4), 0)
lnw <- 2.50 + 0.065 * educ_ref + 0.034 * ex - 0.0006 * ex^2 +
  0.0015 * (educ_ref - 13) * (ex - 12) - 0.07 * fem + P$u_wage[pi_] +
  0.006 * (G$wave - 12) + gap_k - 0.04 * mum_cens + rnorm(nG, 0, 0.16)
defl <- DEFLATOR[G$wave - 11L]
wage_seen <- emp & (!selfe | stats::runif(nG) < 0.5)
hw_nom <- ifelse(wage_seen, exp(lnw) / defl, NA_real_)
G$weekly_earnings <- ifelse(wage_seen & !is.na(G$hours_worked_clean),
                            round(hw_nom * G$hours_worked_clean, 2), 0)
hourly <- ifelse(G$weekly_earnings > 0 & !is.na(G$hours_worked_clean),
                 G$weekly_earnings / G$hours_worked_clean, NA_real_)
G$hourly_wage_clean <- ifelse(is.na(hourly) | hourly < 10 | hourly > 500, NA_real_, hourly)
G$deflator <- defl
G$real_hourly_wage <- G$hourly_wage_clean * defl
G$ln_hourly_wage <- ifelse(G$hourly_wage_clean > 0, log(G$hourly_wage_clean), NA_real_)
G$ln_hourly_wage_real <- ifelse(G$real_hourly_wage > 0, log(G$real_hourly_wage), NA_real_)

# ── 8. LIFE EVENTS, PREGNANCY AND BIRTH DATING ───────────────────────────────
w <- G$wave
lebth <- ifelse((!is.na(fb) & w == fb & stats::runif(nG) < 0.915) |
                  (!is.na(sb) & w == sb & stats::runif(nG) < 0.90), 2, 1)
miss_fb <- !is.na(fb) & lag_in(as.numeric(w == fb & lebth == 1), G$i) %in% 1
lebth <- ifelse(miss_fb & stats::runif(nG) < 0.5, 2, lebth)
leprg <- ifelse((!is.na(fb) & w == fb - 1 & stats::runif(nG) < 0.55) |
                  (!is.na(fb) & w == fb & stats::runif(nG) < 0.45) |
                  (!is.na(sb) & w == sb - 1 & stats::runif(nG) < 0.50) |
                  stats::runif(nG) < 0.005, 2, 1)
lebth[stats::runif(nG) < 0.015] <- NA
leprg[stats::runif(nG) < 0.015] <- NA
G$event_child <- lebth
G$event_pregnancy <- leprg
G$had_child_this_wave     <- ifelse(lebth == 2, 1L, 0L)
G$had_pregnancy_this_wave <- ifelse(leprg == 2, 1L, 0L)

date_items <- function(ev, prefix) {
  is_ev <- !is.na(ev) & ev == 2
  q <- pick(nG, 1:4)
  multi <- is_ev & stats::runif(nG) < 0.03
  decl  <- is_ev & !multi & stats::runif(nG) < 0.02
  q2 <- ((q %% 4L) + 1L)
  out <- list()
  for (j in 1:4)
    out[[paste0(prefix, "_q", j)]] <- ifelse(is_ev, as.integer(!decl & (q == j | (multi & q2 == j))),
                                             NA_integer_)
  nset <- ifelse(is_ev, as.integer(!decl) * (1L + as.integer(multi)), NA_integer_)
  out[[paste0(prefix, "_q_declined")]] <- ifelse(is_ev, as.integer(decl), NA_integer_)
  out[[paste0(prefix, "_q_n_set")]] <- nset
  qq <- ifelse(is_ev & nset == 1L, q, NA_integer_)
  out[[paste0(prefix, "_quarter")]] <- qq
  out[[paste0(prefix, "_quarter_multi")]] <- ifelse(is_ev, as.integer(nset > 1L), NA_integer_)
  out[[paste0(prefix, "_undated")]] <- ifelse(is.na(ev), NA_integer_, as.integer(is_ev & nset == 0L))
  out[[paste0(prefix, "_q_declined_dated")]] <- ifelse(is_ev, as.integer(decl & nset > 0L), NA_integer_)
  out[[paste0(prefix, "_quarter_event")]] <- ifelse(is_ev, qq, NA_integer_)
  out[[paste0(prefix, "_quarter_orphan")]] <- as.integer(!is.na(qq) & !is_ev)
  out
}
for (nm in names(bq <- date_items(lebth, "birth"))) G[[nm]] <- bq[[nm]]
for (nm in names(pq <- date_items(leprg, "preg")))  G[[nm]] <- pq[[nm]]
G$post_birth_window_share <- ifelse(is.na(G$birth_quarter_event), NA_real_,
                                    (G$birth_quarter_event - 0.5) / 4)

fert <- w %in% FERTILITY_MODULE_WAVES
preg_now <- (!is.na(fb) & w == fb - 1) | (!is.na(sb) & w == sb - 1)
G$currently_pregnant <- ifelse(fert & fem & G$age <= 49,
                               ifelse(preg_now & stats::runif(nG) < 0.8, 1, 2), NA_real_)
G$partner_pregnant <- ifelse(fert & !fem & G$age <= 55,
                             ifelse(preg_now & P$in_couple[pi_] & stats::runif(nG) < 0.8, 1, 2),
                             NA_real_)
G$self_or_partner_pregnant <- ifelse(fert, ifelse(dplyr::coalesce(G$currently_pregnant, 2) == 1 |
                                                    dplyr::coalesce(G$partner_pregnant, 2) == 1, 1, 2),
                                     NA_real_)
G$expected_birth_month <- ifelse(fert & G$self_or_partner_pregnant == 1, pick(nG, 1:12), NA_real_)
G$expected_birth_year  <- ifelse(fert & G$self_or_partner_pregnant == 1, 2000 + w + 1, NA_real_)

yn <- function(p) ifelse(stats::runif(nG) < p, 2, 1)
G$event_married   <- yn(ifelse(P$in_couple[pi_], 0.02, 0.01))
G$event_separated <- yn(0.02)
G$event_job_change <- ifelse(G$job_change == 1, 2, 1)
G$event_fired     <- yn(ifelse(emp, 0.015, 0.05))
G$event_moved     <- yn(ifelse(mpost & k == 0, 0.20, 0.12))
for (cl in c("event_married", "event_separated", "event_fired", "event_moved"))
  G[[cl]][stats::runif(nG) < 0.015] <- NA
G$got_married_this_wave <- ifelse(G$event_married == 2, 1L, 0L)
G$separated_this_wave   <- ifelse(G$event_separated == 2, 1L, 0L)
G$changed_job_this_wave <- ifelse(G$event_job_change == 2, 1L, 0L)

# ── 9. KEEP THE OBSERVED PERSON-WAVES ────────────────────────────────────────
keep <- G$obs
D <- G[keep, setdiff(names(G), c("obs", "job_change"))]
pi_ <- D$i
k   <- k[keep]; mpost <- mpost[keep]; fpost <- fpost[keep]
yc  <- yc[keep]; mum_young <- mum_young[keep]; selfe <- selfe[keep]
educ_ref <- educ_ref[keep]; trained <- trained[keep]; ask <- ask[keep]
typ <- typ[keep]
rm(G); invisible(gc())
n <- nrow(D)
fem <- D$female == 1L; emp <- D$employed == 1
employee <- !is.na(D$emptype_raw) & D$emptype_raw == 1
nilf <- D$esdtl_raw %in% c(5, 6)
defl <- D$deflator

D$person_id <- P$person_id[pi_]
D$xwaveid <- D$person_id
D$partner_id <- P$partner_id[pi_]
D$household_id <- P$household_id[pi_]
D$year <- as.integer(2000L + D$wave)
D$wave <- as.integer(D$wave)
D$age <- as.numeric(D$age)
D$age_sq <- D$age^2
D$married <- as.numeric(P$in_couple[pi_])
D$married_num <- as.numeric(D$married)
D$fulltime_num <- as.numeric(D$fulltime)
D$employed <- as.numeric(D$employed)
D$fulltime_emp <- ifelse(is.na(D$employed) | D$employed != 1, NA_real_,
                         as.numeric(D$esdtl_raw == 1))

# Education: educ_years is the running maximum of the per-wave report.
D$educ_years_ref <- ifelse(!is.na(P$educ_up_wave[pi_]) & D$wave > P$educ_up_wave[pi_] &
                             stats::runif(n) < 0.02, P$educ0[pi_], educ_ref)
D$educ_years <- as.numeric(ave(D$educ_years_ref, D$person_id, FUN = cummax))
D$educ_sq <- D$educ_years^2

# Experience provenance.
D$experience_sq <- D$experience_years^2
D$experience_source <- ifelse(!is.na(D$actual_experience), "ehtjb_actual",
                              ifelse(!is.na(D$potential_experience), "mincer_proxy", NA_character_))
D$experience_is_proxy <- ifelse(is.na(D$experience_source), NA_real_,
                                as.numeric(D$experience_source == "mincer_proxy"))
D$experience_ceiling_bound <- as.numeric(
  dplyr::coalesce(D$actual_experience, D$potential_experience) > pmax(0, D$age - 13))
D$olf_share <- ifelse(!is.na(D$years_since_education) & D$years_since_education > 0,
                      D$years_not_in_lf / D$years_since_education, NA_real_)
D$unemp_share <- ifelse(!is.na(D$years_since_education) & D$years_since_education > 0,
                        D$years_unemployed / D$years_since_education, NA_real_)
D$employment_share <- ifelse(!is.na(D$years_since_education) & D$years_since_education > 0,
                             D$actual_experience / D$years_since_education, NA_real_)
D$nonemployment_years <- D$years_unemployed + D$years_not_in_lf
D$history_components <- D$actual_experience + D$years_unemployed + D$years_not_in_lf
D$history_residual <- D$years_since_education - D$history_components
D$has_interruption <- ifelse(is.na(D$nonemployment_years), NA_real_,
                             as.numeric(D$nonemployment_years > 0.5))
D$has_olf_spell <- ifelse(is.na(D$years_not_in_lf), NA_real_, as.numeric(D$years_not_in_lf > 0))

# Panel structure, within-person increments (the loader's restriction block).
D <- D[order(D$person_id, D$wave), ]
pi_ <- D$i
D$entry_wave <- as.integer(ave(D$wave, D$person_id, FUN = min))
D$entry_cohort <- ifelse(D$entry_wave <= min(WAVES), "in_panel_at_w12",
                         ifelse(D$entry_wave <= 19L, "entry_w13_w19", "entry_w20_plus"))
D$d_wave <- D$wave - lag_in(D$wave, D$person_id)
fl <- function(x) ifelse(is.na(D$d_wave) | D$d_wave != 1L, NA_real_, x - lag_in(x, D$person_id))
D$d_paid_years     <- fl(D$actual_experience)
D$d_unemp_years    <- fl(D$years_unemployed)
D$d_olf_years      <- fl(D$years_not_in_lf)
D$d_exposure_years <- fl(D$years_since_education)
D$history_flow_sum <- D$d_paid_years + D$d_unemp_years + D$d_olf_years
D$olf_year <- ifelse(is.na(D$d_olf_years), NA_integer_, as.integer(D$d_olf_years > 0.5))
D$olf_flow_valid <- as.integer(!is.na(D$d_olf_years))
new_entrant <- D$entry_wave > 12L & D$wave == D$entry_wave
D$years_npq_total <- ifelse(new_entrant & !is.na(D$years_since_education),
                            D$years_since_education + round(rnorm(n, 0, 0.3), 1), NA_real_)
D$npq_total_discrepancy <- D$years_npq_total - D$years_since_education

# These had to wait for the sort, because the row order changed above.
k <- ifelse(P$type[pi_] == "treated", D$wave - P$g[pi_], NA_integer_)
fem <- D$female == 1L
mpost <- fem & !is.na(k) & k >= 0
fpost <- !fem & !is.na(k) & k >= 0
yc <- D$age_youngest_own_child
mum_young <- fem & !is.na(yc) & yc <= 14
emp <- D$employed == 1
employee <- !is.na(D$emptype_raw) & D$emptype_raw == 1
nilf <- D$esdtl_raw %in% c(5, 6)
defl <- D$deflator
trained <- D$any_training == 1 & D$training_module_asked == 1L

# ── 10. PARENTHOOD LABELS (wave status and lifetime status) ──────────────────
D$ever_parent_ref <- as.integer(ave(as.integer(D$ever_parent == 1L | D$num_children_under15 > 0),
                                    D$person_id, FUN = cummax))
D$ever_mother <- ifelse(fem, D$ever_parent, NA_integer_)
D$ever_father <- ifelse(!fem, D$ever_parent, NA_integer_)
D$motherhood_status <- ifelse(!fem, NA_character_,
                              ifelse(D$ever_parent == 1L & D$num_children_under15 > 0, "active_mother",
                                     ifelse(D$ever_parent == 1L, "post_mother", "never_mother")))
D$fatherhood_status <- ifelse(fem, NA_character_,
                              ifelse(D$ever_parent == 1L & D$num_children_under15 > 0, "active_father",
                                     ifelse(D$ever_parent == 1L, "post_father", "never_father")))
D$age_oldest_child <- suppressWarnings(pmax(D$age_resident_child_1, D$age_resident_child_2,
                                            D$age_resident_child_3, D$age_nonresident_child_1,
                                            D$age_nonresident_child_2, na.rm = TRUE))
D$approx_age_at_first_birth <- ifelse(!is.na(D$age_oldest_child) & D$ever_parent == 1L,
                                      D$age - D$age_oldest_child, NA_real_)
D$ever_parent_life <- as.integer(ave(D$ever_parent, D$person_id, FUN = max))
D$ever_mother_life <- ifelse(fem, D$ever_parent_life, NA_integer_)
D$ever_father_life <- ifelse(!fem, D$ever_parent_life, NA_integer_)
D$motherhood_status_life <- ifelse(!fem, NA_character_,
  ifelse(D$ever_parent_life == 0L, "never_mother",
         ifelse(D$ever_parent == 0L, "pre_mother",
                ifelse(D$num_children_under15 > 0, "active_mother", "post_mother"))))
D$fatherhood_status_life <- ifelse(fem, NA_character_,
  ifelse(D$ever_parent_life == 0L, "never_father",
         ifelse(D$ever_parent == 0L, "pre_father",
                ifelse(D$num_children_under15 > 0, "active_father", "post_father"))))
D$group_ever <- ifelse(!fem, ifelse(D$ever_father == 1L, "fathers_ever", "childless_men_ever"),
                       ifelse(D$ever_mother == 1L, "mothers_ever", "never_mothers"))
D$group_current <- ifelse(!fem, ifelse(D$num_children_under15 > 0, "fathers", "childless_men"),
                          ifelse(D$num_children_under15 > 0, "mothers", "childless_women"))
D$group_3way_women <- ifelse(!fem, NA_character_,
                             c(never_mother = "never_mothers", active_mother = "active_mothers",
                               post_mother = "post_mothers")[D$motherhood_status])
D$group_ever_life <- ifelse(!fem, ifelse(D$ever_father_life == 1L, "fathers_ever", "childless_men_ever"),
                            ifelse(D$ever_mother_life == 1L, "mothers_ever", "never_mothers"))
D$group_4way_women_life <- ifelse(!fem, NA_character_,
                                  c(never_mother = "never_mothers", pre_mother = "pre_mothers",
                                    active_mother = "active_mothers",
                                    post_mother = "post_mothers")[D$motherhood_status_life])
D$has_young_children <- ifelse(D$num_children_0to4 > 0, 1, 0)
D$active_caregiver   <- ifelse(D$num_dependent_children > 0, 1, 0)
D$is_mother <- ifelse(fem & D$num_dependent_children > 0, 1, 0)
D$is_father <- ifelse(!fem & D$num_dependent_children > 0, 1, 0)
kid17 <- D$ever_parent == 1L & !is.na(yc) & yc <= 17
lik <- function() ifelse(kid17, as.numeric(pick(n, 1:7)), NA_real_)
D$parenting_resp <- lik(); D$parenting_hard <- lik(); D$parenting_tired <- lik()
D$parenting_trapped <- lik(); D$parenting_fairshare <- lik()

# ── 11. EARNINGS AND INCOME ──────────────────────────────────────────────────
fy_share <- ifelse(emp, ifelse(stats::runif(n) < 0.8, 1, round(stats::runif(n, 0.5, 1), 2)),
                   ifelse(stats::runif(n) < 0.3, round(stats::runif(n, 0, 0.6), 2), 0))
D$fy_share_employed <- fy_share
D$salary_sacrifice <- ifelse(employee & stats::runif(n) < (0.12 + 0.10 * (P$public[pi_] == 1L)),
                             round(stats::runif(n, 1000, 15000)), 0)
D$fy_wages <- round(D$weekly_earnings * 52 * fy_share * stats::runif(n, 0.92, 1.08))
D$fy_wages_inc_ss <- D$fy_wages + D$salary_sacrifice
D$annual_income <- dplyr::coalesce(D$fy_wages_inc_ss, D$fy_wages)
D$earnings_source_flag <- ifelse(!is.na(D$fy_wages_inc_ss), "inc_salary_sacrifice",
                                 ifelse(!is.na(D$fy_wages), "wages_only_fallback", NA_character_))
D$childcare_cost <- ifelse(!is.na(yc) & yc <= 5 & D$ever_parent == 1L,
                           round(stats::runif(n, 0, 400)), 0)
D$real_weekly_earnings <- D$weekly_earnings * defl
D$real_annual_income <- D$annual_income * defl
D$ihs_weekly_earnings <- ifelse(is.na(D$real_weekly_earnings), NA_real_, asinh(D$real_weekly_earnings))
D$ihs_annual_income <- ifelse(is.na(D$real_annual_income), NA_real_, asinh(D$real_annual_income))
D$earnings_is_zero <- ifelse(is.na(D$real_weekly_earnings), NA_real_, as.numeric(D$real_weekly_earnings == 0))
D$annual_income_is_zero <- ifelse(is.na(D$real_annual_income), NA_real_, as.numeric(D$real_annual_income == 0))
D$salary_sacrificed_weekly <- round(D$salary_sacrifice / 52, 2)
D$weekly_earnings_inc_ss <- D$weekly_earnings + D$salary_sacrificed_weekly
D$wage_imputed <- as.numeric(rbern(rep(0.03, n)))
D$wage_ss_imputed <- as.numeric(rbern(rep(0.02, n)))
D$fy_wage_imputed <- as.numeric(rbern(rep(0.03, n)))

D$is_employee <- ifelse(is.na(D$emptype_raw), NA_real_, as.numeric(D$emptype_raw == 1))
D$is_self_employed <- ifelse(is.na(D$emptype_raw) | is.na(D$employed), NA_real_,
                             as.numeric(D$emptype_raw %in% c(2, 3, 4)))
D$is_selfemp_narrow <- ifelse(is.na(D$emptype_raw) | is.na(D$employed), NA_real_,
                              as.numeric(D$emptype_raw %in% c(3, 4)))
D$is_unpaid_family <- ifelse(is.na(D$emptype_raw), NA_real_, as.numeric(D$emptype_raw == 6))
D$bifhave_raw <- ifelse(selfe | stats::runif(n) < 0.02, 1, 2)
D$had_business_lfy <- ifelse(is.na(D$bifhave_raw), NA_real_, as.numeric(D$bifhave_raw == 1))
D$fy_business_income <- ifelse(D$bifhave_raw == 1, round(rnorm(n, 38000, 30000)), 0)
D$real_fy_business_income <- D$fy_business_income * defl
D$fy_earnings_total <- D$fy_wages_inc_ss + D$fy_business_income
D$real_fy_earnings_total <- D$fy_earnings_total * defl
D$ihs_earnings_total <- asinh(D$real_fy_earnings_total)
D$earnings_total_is_zero <- as.numeric(D$real_fy_earnings_total == 0)
D$earnings_total_is_negative <- as.numeric(D$real_fy_earnings_total < 0)
D$earnings_total_includes_business <- 1
D$fy_business_income_nonzero <- as.numeric(D$fy_business_income != 0)
D$weekly_annual_gap <- ifelse(D$weekly_earnings > 0 & D$annual_income > 0,
                              D$annual_income / 52 / D$weekly_earnings, NA_real_)
D$fy_benefits <- ifelse(D$real_annual_income < 30000 | (mum_young & !is.na(yc) & yc <= 1),
                        round(stats::runif(n, 0, 22000)), round(stats::runif(n, 0, 2000) * (stats::runif(n) < 0.2)))
D$real_fy_benefits <- D$fy_benefits * defl
D$fy_income_gross_regular <- D$fy_earnings_total + D$fy_benefits + round(stats::runif(n, 0, 4000))
D$fy_income_gross <- D$fy_income_gross_regular
D$fy_income_disposable <- round(0.80 * D$fy_income_gross_regular)
D$hh_income_gross <- round(D$fy_income_gross_regular * ifelse(D$married == 1, 1.85, 1.10))
D$hh_income_disposable <- round(0.80 * D$hh_income_gross)
D$real_hh_income_disposable <- D$hh_income_disposable * defl

# ── 12. EDUCATION LABELS ─────────────────────────────────────────────────────
ey <- D$educ_years
D$education_level <- dplyr::case_when(
  ey >= 19.5 ~ "Doctorate", ey >= 18 ~ "Masters",
  ey >= 17 ~ "Graduate_Diploma_Certificate", ey >= 16.5 ~ "Bachelor_Honours",
  ey >= 16 ~ "Bachelor", ey >= 15 ~ "Advanced_Diploma", ey >= 14.5 ~ "Diploma",
  ey >= 14 ~ "Diploma_Other", ey >= 13.5 ~ "Certificate_IV",
  ey >= 13 ~ "Certificate_III", ey >= 12.5 ~ "Certificate_II",
  ey == 12 ~ "Year_12", ey > 12 & ey < 12.5 ~ "Certificate_I",
  ey == 11 ~ "Year_11", ey == 10 ~ "Year_10", ey == 9 ~ "Year_9",
  ey == 8 ~ "Year_8", ey == 7 ~ "Year_7", ey == 6 ~ "Primary_Complete",
  ey == 5 ~ "Primary_Incomplete", TRUE ~ "Other")
D$education_pathway <- dplyr::case_when(
  ey >= 19.5 ~ "Doctorate_General", ey >= 18 ~ "Masters_General",
  ey >= 17 ~ "GradDipCert_General", ey >= 16.5 ~ "Bachelor_Honours_General",
  ey >= 16 ~ "Bachelor_General", ey >= 15 ~ "Advanced_Diploma",
  ey >= 14 ~ "Diploma", ey >= 13.5 ~ "Certificate_IV",
  ey >= 13 ~ "Certificate_III", ey >= 12.5 ~ "Certificate_II",
  ey == 12 ~ "Year_12", ey == 11 ~ "Year_11", ey == 10 ~ "Year_10",
  TRUE ~ "Year_9")
D$education_data_source <- dplyr::case_when(
  ey >= 17 ~ "Specific_Postgrad", ey >= 16 ~ "Specific_Bachelor",
  ey >= 14 ~ "Specific_Diploma", ey >= 12.5 ~ "Specific_Certificate",
  TRUE ~ "School_Only")
D$has_degree <- (ey >= 16); D$has_postsecondary <- (ey > 12)
D$has_diploma_plus <- (ey >= 14); D$has_postgrad <- (ey >= 17)
npq <- function(p) ifelse(new_entrant, as.numeric(stats::runif(n) < p), NA_real_)
D$has_nursing_any <- npq(0.03); D$has_nursing_strict <- npq(0.02)
D$has_teaching_any <- npq(0.03); D$has_trade_any <- npq(0.10)
D$has_technical_any <- npq(0.05)
D$has_business_any <- NA_real_        # empty by design in the release
D$study_item_regime <- ifelse(D$wave >= STUDY_ITEM_BREAK_WAVE,
                              sprintf("w%d_refreshed_instrument", STUDY_ITEM_BREAK_WAVE),
                              sprintf("w%d_%d", min(WAVES), STUDY_ITEM_BREAK_WAVE - 1L))
D$educ_trend_comparable <- as.integer(!D$wave %in% EDUC_COMPOSITION_BREAK_WAVES)
p_fts <- ifelse(D$age < 25, 0.25, ifelse(D$age < 30, 0.05, 0.01)) + 0.004 * mpost
D$full_time_student <- ifelse(stats::runif(n) < p_fts, 1, 2)
D$full_time_student_yes <- as.numeric(D$full_time_student == 1)
D$full_time_student_comparable <- ifelse(D$wave >= STUDY_ITEM_BREAK_WAVE, NA_real_,
                                         as.numeric(D$full_time_student))
D$enrolled_in_course <- ifelse(new_entrant, ifelse(stats::runif(n) < 0.25, 1, 2), NA_real_)
D$enrolled_in_course_yes <- ifelse(is.na(D$enrolled_in_course), NA_real_,
                                   as.numeric(D$enrolled_in_course == 1))
D$enrolled_in_course_comparable <- NA_real_   # entrant-only item: NA everywhere

# ── 13. JOB CHARACTERISTICS AND JOB QUALITY ──────────────────────────────────
pt <- D$esdtl_raw == 2
cas <- ifelse(employee, as.numeric(stats::runif(n) < ifelse(pt, 0.35, 0.08)), NA_real_)
D$casual <- ifelse(emp, ifelse(employee, cas, 0), NA_real_)
D$permanent <- ifelse(emp, ifelse(employee, 1 - cas, 0), NA_real_)
D$public_sector <- ifelse(emp, as.numeric(P$public[pi_] == 1L), NA_real_)
D$union_member <- ifelse(emp, as.numeric(P$union[pi_] == 1L), NA_real_)
pre22 <- D$wave < FIRM_SIZE_BREAK_WAVE
fs_seen <- emp & stats::runif(n) < ifelse(pre22, 0.60, 0.99)
D$firm_size_raw <- ifelse(fs_seen, ifelse(pre22, pick(n, 1:9), pick(n, 1:8)), NA_real_)
D$firm_size <- dplyr::case_when(
  pre22 & D$firm_size_raw == 1 ~ "Small", pre22 & D$firm_size_raw %in% 2:3 ~ "Medium",
  pre22 & D$firm_size_raw %in% 4:9 ~ "Large",
  !pre22 & D$firm_size_raw %in% 1:4 ~ "Small", !pre22 & D$firm_size_raw %in% 5:6 ~ "Medium",
  !pre22 & D$firm_size_raw %in% 7:8 ~ "Large", TRUE ~ NA_character_)
D$firm_size_instrument <- ifelse(pre22, "jbmemsz", "jbmems2")
D$firm_size_comparable <- NA_character_       # empty by design: coverage breaks at wave 22
D$firm_size_post22 <- ifelse(pre22, NA_character_, D$firm_size)
D$firm_size_regime <- ifelse(pre22,
  sprintf("w%d_%d_jbmemsz", min(WAVES), FIRM_SIZE_BREAK_WAVE - 1L),
  sprintf("w%d_%d_jbmems2", FIRM_SIZE_BREAK_WAVE, max(WAVES)))
D$has_holiday_leave <- ifelse(emp, as.numeric(employee & D$permanent == 1), NA_real_)
D$has_sick_leave <- D$has_holiday_leave
D$leave_none <- ifelse(D$has_holiday_leave == 0 & D$has_sick_leave == 0, 1, 0)
sch <- pick(n, 1:3, c(.72, .15, .13))
D$has_standard_hours <- ifelse(emp, as.numeric(sch == 1), NA_real_)
D$has_shift_work <- ifelse(emp, as.numeric(sch == 2), NA_real_)
D$has_irregular_hours <- ifelse(emp, as.numeric(sch == 3), NA_real_)
hpr <- ifelse(pt, pick(n, 1:3, c(.10, .60, .30)), pick(n, 1:3, c(.25, .65, .10)))
D$underemployed <- ifelse(emp, as.numeric(hpr == 3), NA_real_)
D$overemployed <- ifelse(emp, as.numeric(hpr == 1), NA_real_)
D$precarious_score <- ifelse(emp, D$casual + D$leave_none + D$underemployed +
                               D$has_irregular_hours + as.numeric(D$tenure_employer < 1), NA_real_)
D$precarious_n_components <- ifelse(emp, 5L, NA_integer_)
D$precarious_category <- dplyr::case_when(
  is.na(D$precarious_score) ~ NA_character_, D$precarious_score < 0.5 ~ "Secure",
  D$precarious_score < 1.5 ~ "Mostly_Secure", D$precarious_score < 2.5 ~ "Somewhat_Precarious",
  TRUE ~ "Highly_Precarious")
D$flexibility_any <- ifelse(emp, as.numeric(stats::runif(n) < 0.5 + 0.1 * mum_young), NA_real_)
D$treatment <- D$flexibility_any
s10 <- function(m) ifelse(emp, as.numeric(clamp(round(rnorm(n, m, 2)), 0, 10)), NA_real_)
D$job_satisfaction <- s10(7.5); D$pay_fairness <- s10(6.5); D$job_initiative <- s10(6)
D$job_security <- s10(7); D$job_worry_future <- s10(3); D$job_autonomy <- s10(5.5)
D$looking_for_new_job <- ifelse(emp, ifelse(stats::runif(n) < 0.15, 1, 2), NA_real_)
ls10 <- function(m) as.numeric(clamp(round(rnorm(n, m, 1.6)), 0, 10))
D$losat <- ls10(7.9); D$losateo <- ls10(7.3); D$losatfs <- ls10(6.6)
D$losatft <- ls10(6.6); D$losathl <- ls10(8.0); D$losatlc <- ls10(6.8)
D$losatnl <- ls10(7.9); D$losatsf <- ls10(8.2); D$losatyh <- ls10(7.4)

OCC <- c("Managers", "Professionals", "Technicians_Trades", "Community_Service",
         "Clerical_Admin", "Sales", "Machinery_Operators", "Labourers")
occ <- ifelse(fem, pick(n, 1:8, c(.10, .28, .05, .16, .21, .11, .01, .08)),
              pick(n, 1:8, c(.15, .22, .23, .07, .06, .07, .10, .10)))
D$occupation_major_code <- ifelse(emp, as.numeric(occ), NA_real_)
D$occupation_major <- ifelse(emp, OCC[occ], NA_character_)
IND <- c("Agriculture", "Mining", "Manufacturing", "Utilities", "Construction",
         "Wholesale", "Retail", "Hospitality", "Transport", "Information",
         "Finance", "Real_Estate", "Professional", "Administrative",
         "Public_Admin", "Education", "Health", "Arts", "Other_Services")
ind <- pick(n, 1:19)
D$industry_major_code <- ifelse(emp, as.numeric(ind), NA_real_)
D$industry_major <- ifelse(emp, IND[ind], NA_character_)
D$occupation_2digit <- ifelse(emp, occ * 10 + pick(n, 1:6), NA_real_)
D$occupation_status_score <- ifelse(emp, round(clamp(c(80, 78, 45, 40, 50, 35, 25, 20)[occ] +
                                                       rnorm(n, 0, 10), 5, 100), 1), NA_real_)

# Origin occupation: observed only for movers, by three routes before wave 20
# and two from wave 20.
emp_lag <- lag_in(D$employed, D$person_id)
into_work <- emp & !is.na(emp_lag) & emp_lag == 0
D$occ2_origin_unemp <- ifelse(into_work & stats::runif(n) < 0.4, pick(n, 11:89), NA_real_)
D$occ2_origin_nonemp <- ifelse(new_entrant & !emp & stats::runif(n) < 0.3, pick(n, 11:89), NA_real_)
D$occ2_origin_jobchange <- ifelse(D$changed_job_this_wave %in% 1L & D$wave < OCC_ORIGIN_BREAK_WAVE,
                                  pick(n, 11:89), NA_real_)
st_of <- function(x) ifelse(is.na(x), NA_real_, round(stats::runif(n, 15, 90), 1))
D$occstat_origin_unemp <- st_of(D$occ2_origin_unemp)
D$occstat_origin_nonemp <- st_of(D$occ2_origin_nonemp)
D$occstat_origin_jobchange <- st_of(D$occ2_origin_jobchange)
D$occ2_origin <- dplyr::coalesce(D$occ2_origin_unemp, D$occ2_origin_nonemp, D$occ2_origin_jobchange)
D$occstat_origin <- dplyr::coalesce(D$occstat_origin_unemp, D$occstat_origin_nonemp,
                                    D$occstat_origin_jobchange)
D$occstat_change <- D$occupation_status_score - D$occstat_origin
D$occ_origin_source <- dplyr::case_when(
  !is.na(D$occ2_origin_unemp) ~ "cpq_unemployed", !is.na(D$occ2_origin_nonemp) ~ "npq_not_in_work",
  !is.na(D$occ2_origin_jobchange) ~ "cpq_job_change", TRUE ~ NA_character_)
D$occ_origin_regime <- ifelse(D$wave < OCC_ORIGIN_BREAK_WAVE, "three_routes_w12_w19",
                              "two_routes_w20_w24")

# ── 14. LEAVE, PAID PARENTAL LEAVE, EXIT CHANCES ─────────────────────────────
kk <- dplyr::coalesce(k, -99L)
D$paid_nonannual_leave_days <- ifelse(!employee, NA_real_, round(
  ifelse(mpost & kk == 0, stats::runif(n, 30, 100), ifelse(mpost & kk == 1, stats::runif(n, 0, 40),
         ifelse(fpost & kk == 0, stats::runif(n, 0, 15),
                stats::runif(n, 1, 10) * (stats::runif(n) < 0.15))))))
D$unpaid_leave_days <- ifelse(!employee, NA_real_, round(
  ifelse(mpost & kk == 0, stats::runif(n, 10, 150) * (stats::runif(n) < 0.6),
         ifelse(mpost & kk == 1, stats::runif(n, 5, 60) * (stats::runif(n) < 0.3),
                stats::runif(n, 1, 20) * (stats::runif(n) < 0.03)))))
ppl_m <- mpost & kk %in% 0:1; ppl_d <- fpost & kk == 0
ppl_any <- ppl_m | ppl_d
D$ppl_entitled_mat <- ifelse(fem & employee, as.numeric(stats::runif(n) < 0.6), NA_real_)
D$ppl_entitled_pat <- ifelse(!fem & employee, as.numeric(stats::runif(n) < 0.4), NA_real_)
D$ppl_received_fy <- ifelse(ppl_any, as.numeric(stats::runif(n) < ifelse(ppl_m, 0.7, 0.3)), NA_real_)
D$ppl_received_cur <- ifelse(ppl_any, as.numeric(stats::runif(n) < ifelse(ppl_m & kk == 0, 0.3, 0.05)), NA_real_)
D$ppl_amount_fy <- ifelse(D$ppl_received_fy %in% 1, round(stats::runif(n, 3000, 18000)), NA_real_)
D$ppl_weeks_fy <- ifelse(D$ppl_received_fy %in% 1, round(stats::runif(n, 2, 18)), NA_real_)
D$ppl_amount_cur <- ifelse(D$ppl_received_cur %in% 1, round(stats::runif(n, 600, 900)), NA_real_)
D$ppl_weeks_cur <- ifelse(D$ppl_received_cur %in% 1, round(stats::runif(n, 1, 18)), NA_real_)
D$ppl_howanswered_fy <- ifelse(D$ppl_received_fy %in% 1, pick(n, c(1, 2)), NA_real_)
D$ppl_howanswered_cur <- ifelse(D$ppl_received_cur %in% 1, pick(n, c(1, 2)), NA_real_)
D$ppl_in_wages <- ifelse(D$ppl_received_cur %in% 1, as.numeric(stats::runif(n) < 0.3), NA_real_)
D$ppl_in_fy_income <- ifelse(D$ppl_received_fy %in% 1, as.numeric(stats::runif(n) < 0.4), NA_real_)
D$ppl_in_wages_imp <- ifelse(D$ppl_in_wages %in% 1, round(stats::runif(n, 500, 900)), NA_real_)
D$ppl_in_wages_impflag <- ifelse(D$ppl_in_wages %in% 1, as.numeric(stats::runif(n) < 0.1), NA_real_)
D$ppl_in_fy_imp <- ifelse(D$ppl_in_fy_income %in% 1, round(stats::runif(n, 3000, 18000)), NA_real_)
D$ppl_in_fy_impflag <- ifelse(D$ppl_in_fy_income %in% 1, as.numeric(stats::runif(n) < 0.1), NA_real_)
pct5 <- function(m) as.numeric(pmin(100, 5 * round(stats::rexp(n, 1 / m) / 5)))
D$chance_leave_job <- ifelse(employee, pct5(18), NA_real_)
D$chance_lose_job  <- ifelse(employee, pct5(9), NA_real_)
D$chance_find_job  <- ifelse(employee, pct5(55), NA_real_)

# ── 15. NOT IN THE LABOUR FORCE, NETWORKS, TIME USE, WEALTH ─────────────────
nlf_code <- ifelse(nilf, ifelse(mpost & kk %in% 0:1 & stats::runif(n) < 0.6, 8,
                                pick(n, setdiff(1:14, 8))), NA_real_)
D$nlf_reason_code <- nlf_code
D$nlf_reason_maternity <- ifelse(is.na(nlf_code), NA_integer_, as.integer(nlf_code == 8))
D$club_member <- as.numeric(stats::runif(n) < 0.3)
D$social_freq <- as.numeric(pick(n, 1:7))
D$social_freq_01 <- (D$social_freq - 1) / 6
D$high_social <- as.integer(D$social_freq >= 5)
D$networking_index <- round(stats::runif(n), 3)
young <- !is.na(yc) & yc <= 4
D$housework_hours <- as.numeric(pmax(0, round(rnorm(n, ifelse(fem, 15, 7) + ifelse(young, ifelse(fem, 8, 3), 0), 5))))
D$care_intensity <- clamp(D$housework_hours, 0, 100)
D$hours_lshrcar <- as.numeric(ifelse(stats::runif(n) < 0.05, round(stats::runif(n, 1, 20)), 0))
D$hours_lshrchd <- as.numeric(ifelse(D$num_children_under15 > 0,
                                     pmax(0, round(rnorm(n, ifelse(fem, 25, 10) * (1 + young), 8))), 0))
D$care_own_children_hours <- D$hours_lshrchd
D$care_other_hours <- D$hours_lshrcar
D$commute_hours <- as.numeric(ifelse(emp, pmax(0, round(rnorm(n, 4, 2), 1)), 0))
D$volunteer_hours <- as.numeric(ifelse(stats::runif(n) < 0.15, round(stats::runif(n, 1, 10)), 0))
D$total_unpaid_hours_raw <- D$housework_hours + D$hours_lshrchd + D$hours_lshrcar + D$volunteer_hours
D$total_unpaid_hours <- clamp(D$total_unpaid_hours_raw, 0, 168)
D$total_unpaid_hours_capped <- pmin(D$total_unpaid_hours_raw, 112)
wm <- D$wave %in% WEALTH_MODULE_WAVES
D$hh_super_balance <- ifelse(wm, round(stats::rlnorm(n, log(120000), 1)), NA_real_)
D$hh_net_worth_pos <- ifelse(wm, round(stats::rlnorm(n, log(450000), 1)), NA_real_)
D$hh_net_worth_neg <- ifelse(wm, round(stats::rlnorm(n, log(20000), 1.5) * (stats::runif(n) < 0.3)), NA_real_)
D$hh_net_worth <- D$hh_net_worth_pos - D$hh_net_worth_neg
D$in_wealth_module <- as.integer(wm)
D$wealth_module_wave <- as.integer(wm)

# ── 16. NON-CASH BENEFITS AND COMPENSATION ───────────────────────────────────
nc_ask <- employee & stats::runif(n) < 0.75
nc_any <- nc_ask & stats::runif(n) < ifelse(fem, 0.20, 0.33)
D$noncash_module_asked <- as.integer(nc_ask)
D$noncash_any <- ifelse(nc_ask, as.integer(nc_any), NA_integer_)
nc_val <- ifelse(nc_any, round(stats::rlnorm(n, log(40), 1.0), 2), 0)
D$noncash_imputed <- as.numeric(nc_any & stats::runif(n) < 0.05)
D$noncash_benefits_weekly <- nc_val
D$noncash_benefits_weekly_preimp <- ifelse(D$noncash_imputed == 1, NA_real_, nc_val)
D$noncash_other_jobs_weekly <- 0
D$noncash_other_jobs_weekly_preimp <- 0
D$noncash_other_imputed <- 0
D$noncash_total_weekly <- D$noncash_benefits_weekly + D$noncash_other_jobs_weekly
D$noncash_any_value <- ifelse(is.na(D$noncash_total_weekly), NA_real_, as.numeric(D$noncash_total_weekly > 0))
D$noncash_share_of_comp <- ifelse(D$weekly_earnings + D$noncash_total_weekly > 0,
                                  D$noncash_total_weekly / (D$weekly_earnings + D$noncash_total_weekly), NA_real_)
D$noncash_benefits_weekly_z <- ifelse(D$noncash_module_asked == 0L, NA_real_, D$noncash_benefits_weekly)
D$noncash_total_weekly_z <- ifelse(D$noncash_module_asked == 0L, NA_real_, D$noncash_total_weekly)
D$noncash_any_value_z <- ifelse(D$noncash_module_asked == 0L, NA_real_, D$noncash_any_value)
D$weekly_compensation <- ifelse(is.na(D$weekly_earnings_inc_ss), NA_real_,
                                D$weekly_earnings_inc_ss + D$noncash_total_weekly)
D$real_weekly_compensation <- D$weekly_compensation * defl
D$real_noncash_benefits_weekly <- D$noncash_total_weekly * defl
hc <- ifelse(!is.na(D$hours_worked_clean) & D$weekly_compensation > 0,
             D$weekly_compensation / D$hours_worked_clean, NA_real_)
D$hourly_compensation_clean <- ifelse(is.na(hc) | hc < 10 | hc > 500, NA_real_, hc)
D$real_hourly_compensation <- D$hourly_compensation_clean * defl
D$ln_hourly_compensation_real <- ifelse(D$real_hourly_compensation > 0,
                                        log(D$real_hourly_compensation), NA_real_)
D$employer_childcare_weekly <- ifelse(nc_any & stats::runif(n) < 0.004, round(stats::runif(n, 20, 150)), 0)
D$employer_childcare <- ifelse(nc_ask, as.integer(D$employer_childcare_weekly > 0), NA_integer_)

# ── 17. INTERVIEW DATES, PARTNER, GEOGRAPHY, WEIGHTS ─────────────────────────
iv <- as.Date(sprintf("%d-08-01", 2000L + D$wave)) + round(220 * stats::rbeta(n, 1.3, 3.5))
D$interview_date <- iv
D$interview_year <- as.integer(format(iv, "%Y"))
D$interview_month <- as.integer(format(iv, "%m"))
D$interview_doy <- as.integer(format(iv, "%j"))
D$ref_window_start <- iv - 365
D$other_parent_employment <- ifelse(D$num_own_nonresident_children > 0 & !is.na(yc) & yc <= 17,
                                    pick(n, 1:3, c(.6, .25, .15)), NA_real_)
D$living_with_partner_last <- ifelse(D$event_separated %in% 2, pick(n, 1:2), NA_real_)
D$mother_educ_years <- P$mother_educ[pi_]
D$father_educ_years <- P$father_educ[pi_]
D$state <- c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT")[P$state_code[pi_]]
D$analysis_weight <- round(P$weight_p[pi_] * stats::rlnorm(n, 0, 0.10), 2)
D$population_weight <- round(D$analysis_weight * stats::runif(n, 0.95, 1.05), 2)
D$longitudinal_weight <- round(P$lweight_p[pi_] * stats::rlnorm(n, 0, 0.05), 2)

# ── 18. SAMPLE FLAGS, INTERACTIONS, PERIOD ───────────────────────────────────
q_lo <- ave(D$hourly_wage_clean, D$wave, FUN = function(x) stats::quantile(x, 0.005, na.rm = TRUE))
q_hi <- ave(D$hourly_wage_clean, D$wave, FUN = function(x) stats::quantile(x, 0.995, na.rm = TRUE))
D$in_wage_sample <- dplyr::case_when(
  !(D$esdtl_raw %in% c(1, 2)) ~ 0L, is.na(D$ln_hourly_wage_real) ~ 0L,
  D$hourly_wage_clean < q_lo | D$hourly_wage_clean > q_hi ~ 0L,
  is.na(D$hours_worked_clean) | D$hours_worked_clean <= 0 | D$hours_worked_clean > 100 ~ 0L,
  is.na(D$experience_years) | D$experience_years < 0 ~ 0L,
  is.na(D$analysis_weight) | D$analysis_weight <= 0 ~ 0L, TRUE ~ 1L)
D$in_earnings_sample <- dplyr::case_when(
  is.na(D$real_annual_income) & is.na(D$real_weekly_earnings) ~ 0L,
  is.na(D$experience_years) | D$experience_years < 0 ~ 0L,
  is.na(D$analysis_weight) | D$analysis_weight <= 0 ~ 0L, TRUE ~ 1L)
D$in_age_30_49 <- as.numeric(D$age >= 30 & D$age <= 49)
D$in_age_25_52 <- as.numeric(D$age >= 25 & D$age <= 52)
D$in_age_21_52 <- as.numeric(D$age >= 21 & D$age <= 52)
D$educ_centered <- D$educ_years - mean(D$educ_years, na.rm = TRUE)
D$educ_centered_sq <- D$educ_centered^2
D$educ_female <- D$educ_years * D$female
D$educ_mother <- D$educ_years * D$is_mother
D$educ_treatment <- D$educ_years * D$treatment
D$educ_precarious <- D$educ_years * (D$precarious_score >= 2)
D$educ_ever_mother <- D$educ_years * dplyr::coalesce(D$ever_mother, 0L)
D$educ_ever_father <- D$educ_years * dplyr::coalesce(D$ever_father, 0L)
D$period <- dplyr::case_when(D$year %in% 2015:2019 ~ "Pre_COVID",
                             D$year %in% 2020:2021 ~ "COVID",
                             D$year %in% 2022:2024 ~ "Post_COVID")
D$female <- as.numeric(D$female)

# ── 19. FINAL SELECT, IN THE LOADER'S ORDER ──────────────────────────────────
# The 414 columns of LOADING_ND3_v3.R v21.49, in the order of its final
# select(). The generator stops if any of them is missing.
col_order <- c(
  "person_id", "xwaveid", "wave", "year", "age", "age_sq", "female",
  "married", "ln_hourly_wage", "ln_hourly_wage_real", "hourly_wage_clean",
  "real_hourly_wage", "deflator", "weekly_earnings", "hours_worked_clean",
  "annual_income", "childcare_cost", "real_weekly_earnings",
  "real_annual_income", "ihs_weekly_earnings", "ihs_annual_income",
  "earnings_is_zero", "annual_income_is_zero", "earnings_source_flag",
  "salary_sacrifice", "emptype_raw", "is_employee", "is_self_employed",
  "fy_share_employed", "unpaid_leave_days", "is_selfemp_narrow",
  "is_unpaid_family", "bifhave_raw", "had_business_lfy",
  "fy_business_income", "real_fy_business_income", "fy_earnings_total",
  "real_fy_earnings_total", "ihs_earnings_total", "earnings_total_is_zero",
  "earnings_total_is_negative", "earnings_total_includes_business",
  "fy_business_income_nonzero", "fy_wages", "fy_wages_inc_ss",
  "hours_worked_main_job", "second_job_hours", "weekly_annual_gap",
  "fy_benefits", "real_fy_benefits", "fy_income_disposable",
  "fy_income_gross", "fy_income_gross_regular", "hh_income_disposable",
  "hh_income_gross", "real_hh_income_disposable", "educ_years",
  "educ_years_ref", "educ_sq", "education_level", "education_pathway",
  "has_degree", "has_postsecondary", "has_diploma_plus", "has_postgrad",
  "has_nursing_any", "has_nursing_strict", "has_teaching_any",
  "has_business_any", "has_trade_any", "has_technical_any",
  "education_data_source", "experience_years", "experience_sq",
  "actual_experience", "tenure_employer", "tenure_occupation",
  "potential_experience", "experience_source", "experience_is_proxy",
  "experience_ceiling_bound", "years_since_education", "years_unemployed",
  "years_not_in_lf", "years_npq_total", "olf_share", "unemp_share",
  "employment_share", "nonemployment_years", "history_components",
  "history_residual", "npq_total_discrepancy", "has_interruption",
  "has_olf_spell", "entry_wave", "entry_cohort", "d_wave", "d_paid_years",
  "d_unemp_years", "d_olf_years", "d_exposure_years", "history_flow_sum",
  "olf_year", "olf_flow_valid", "employed", "fulltime", "parttime",
  "married_num", "fulltime_num", "fulltime_emp", "esdtl_raw",
  "in_wage_sample", "in_earnings_sample", "in_age_30_49", "in_age_25_52",
  "in_age_21_52", "casual", "permanent", "public_sector", "union_member",
  "firm_size", "firm_size_raw", "firm_size_instrument",
  "firm_size_comparable", "firm_size_post22", "firm_size_regime",
  "has_holiday_leave", "has_sick_leave", "leave_none", "has_standard_hours",
  "has_shift_work", "has_irregular_hours", "underemployed", "overemployed",
  "precarious_score", "precarious_category", "precarious_n_components",
  "flexibility_any", "treatment", "job_satisfaction", "pay_fairness",
  "job_initiative", "job_security", "job_worry_future", "job_autonomy",
  "looking_for_new_job", "losat", "losateo", "losatfs", "losatft", "losathl",
  "losatlc", "losatnl", "losatsf", "losatyh", "occupation_major",
  "occupation_major_code", "industry_major", "industry_major_code",
  "any_training", "total_training_hours", "total_training_hours_z",
  "promotion_training", "maintenance_training", "training_aim_getstarted",
  "training_aim_maintain", "training_aim_safety", "training_aim_nonresponse",
  "aim_battery_asked", "promotion_training_wide",
  "maintenance_training_wide", "promotion_training_z",
  "maintenance_training_z", "promotion_training_wide_z",
  "maintenance_training_wide_z", "training_aim_current_z",
  "training_aim_general_z", "total_training_days", "avg_training_hours",
  "total_courses", "total_training_days_z", "total_courses_z",
  "training_own_time", "training_paid_time", "jttpewt_any", "jttpeot_any",
  "training_module_asked", "training_any_reported", "jttpewt_obs",
  "jttpeot_obs", "jttopot_raw", "jttopwt_raw", "training_own_time_obs",
  "training_paid_time_obs", "chance_leave_job", "chance_lose_job",
  "chance_find_job", "club_member", "social_freq", "social_freq_01",
  "high_social", "networking_index", "num_dependent_children",
  "num_children_under15", "num_children_0to4", "num_children_5to9",
  "num_children_10to14", "num_children_15to24", "has_young_children",
  "active_caregiver", "is_mother", "is_father", "housework_hours",
  "care_intensity", "hours_lshrcar", "hours_lshrchd",
  "care_own_children_hours", "care_other_hours", "total_unpaid_hours",
  "total_unpaid_hours_raw", "total_unpaid_hours_capped", "commute_hours",
  "volunteer_hours", "hh_super_balance", "hh_net_worth", "hh_net_worth_pos",
  "hh_net_worth_neg", "in_wealth_module", "wealth_module_wave",
  "has_nonresident_children_under18", "has_any_nonresident_children",
  "total_children_ever_had", "num_own_resident_children",
  "num_own_nonresident_children", "total_own_children",
  "total_own_children_narm0", "age_youngest_own_child",
  "total_children_died", "num_step_foster_grand", "own_res_children_0to4",
  "own_res_children_5to14", "own_res_children_15to24",
  "own_res_children_25plus", "own_nonres_children_0to4",
  "own_nonres_children_5to14", "own_nonres_children_15to24",
  "own_nonres_children_25plus", "age_resident_child_1",
  "age_resident_child_2", "age_resident_child_3", "age_nonresident_child_1",
  "age_nonresident_child_2", "ever_parent", "ever_parent_ref", "ever_mother",
  "ever_father", "motherhood_status", "fatherhood_status",
  "approx_age_at_first_birth", "age_oldest_child", "ever_parent_life",
  "ever_mother_life", "ever_father_life", "motherhood_status_life",
  "fatherhood_status_life", "group_ever", "group_current",
  "group_3way_women", "group_ever_life", "group_4way_women_life",
  "parenting_resp", "parenting_hard", "parenting_tired", "parenting_trapped",
  "parenting_fairshare", "currently_pregnant", "partner_pregnant",
  "self_or_partner_pregnant", "expected_birth_month", "expected_birth_year",
  "event_pregnancy", "event_child", "event_married", "event_separated",
  "event_job_change", "event_fired", "event_moved",
  "had_pregnancy_this_wave", "had_child_this_wave", "got_married_this_wave",
  "separated_this_wave", "changed_job_this_wave", "birth_q1", "birth_q2",
  "birth_q3", "birth_q4", "birth_q_declined", "birth_quarter",
  "birth_quarter_multi", "preg_q1", "preg_q2", "preg_q3", "preg_q4",
  "preg_q_declined", "preg_quarter", "preg_quarter_multi", "birth_undated",
  "birth_q_declined_dated", "birth_q_n_set", "preg_undated",
  "preg_q_declined_dated", "preg_q_n_set", "birth_quarter_event",
  "birth_quarter_orphan", "preg_quarter_event", "preg_quarter_orphan",
  "post_birth_window_share", "nlf_reason_code", "nlf_reason_maternity",
  "enrolled_in_course", "full_time_student", "enrolled_in_course_comparable",
  "full_time_student_comparable", "enrolled_in_course_yes",
  "full_time_student_yes", "study_item_regime", "educ_trend_comparable",
  "training_gate_regime", "occ_origin_regime", "training_self_funded",
  "training_self_funded_z", "training_aim_general", "training_aim_current",
  "training_aim_future", "occupation_2digit", "occupation_status_score",
  "occ2_origin", "occstat_origin", "occ_origin_source", "occstat_change",
  "occ2_origin_unemp", "occ2_origin_nonemp", "occ2_origin_jobchange",
  "occstat_origin_unemp", "occstat_origin_nonemp",
  "occstat_origin_jobchange", "ppl_in_wages", "ppl_in_fy_income",
  "ppl_entitled_mat", "ppl_entitled_pat", "ppl_received_cur",
  "ppl_received_fy", "ppl_amount_cur", "ppl_weeks_cur", "ppl_amount_fy",
  "ppl_weeks_fy", "ppl_howanswered_fy", "ppl_howanswered_cur",
  "weekly_earnings_inc_ss", "salary_sacrificed_weekly", "wage_imputed",
  "wage_ss_imputed", "fy_wage_imputed", "noncash_benefits_weekly",
  "noncash_benefits_weekly_preimp", "noncash_imputed",
  "noncash_other_jobs_weekly", "noncash_other_jobs_weekly_preimp",
  "noncash_other_imputed", "noncash_total_weekly", "noncash_any_value",
  "noncash_any", "noncash_share_of_comp", "noncash_module_asked",
  "noncash_benefits_weekly_z", "noncash_total_weekly_z",
  "noncash_any_value_z", "weekly_compensation", "real_weekly_compensation",
  "real_noncash_benefits_weekly", "hourly_compensation_clean",
  "real_hourly_compensation", "ln_hourly_compensation_real",
  "employer_childcare_weekly", "employer_childcare", "ppl_in_wages_imp",
  "ppl_in_wages_impflag", "ppl_in_fy_imp", "ppl_in_fy_impflag",
  "interview_date", "interview_year", "interview_month", "interview_doy",
  "ref_window_start", "paid_nonannual_leave_days", "other_parent_employment",
  "living_with_partner_last", "household_id", "partner_id",
  "mother_educ_years", "father_educ_years", "state", "analysis_weight",
  "population_weight", "longitudinal_weight", "educ_centered",
  "educ_centered_sq", "educ_female", "educ_mother", "educ_treatment",
  "educ_precarious", "educ_ever_mother", "educ_ever_father", "period"
)
missing_cols <- setdiff(col_order, names(D))
if (length(missing_cols))
  stop("Generator is missing columns: ", paste(missing_cols, collapse = ", "))
analysis_sample <- D[, col_order]
rownames(analysis_sample) <- NULL
num_cols <- vapply(analysis_sample, is.numeric, logical(1))
analysis_sample[num_cols] <- lapply(analysis_sample[num_cols],
                                    function(x) { x[is.infinite(x)] <- NA; x })

education_order <- c(
  "Other", "Primary_Incomplete", "Primary_Complete", "Year_7", "Year_8", "Year_9",
  "Year_10", "Year_11", "Year_12", "Certificate_I", "Certificate_II",
  "Certificate_III", "Certificate_IV", "Diploma_Other", "Diploma",
  "Advanced_Diploma", "Bachelor", "Bachelor_Honours",
  "Graduate_Diploma_Certificate", "Masters", "Doctorate")
education_pathway_order <- c(
  "Primary_Incomplete", "Primary_Complete", "Year_7", "Year_8", "Year_9",
  "Below_Year_10", "Year_10", "Year_11", "Year_12",
  "Certificate_II", "Cert_III_Trade", "Cert_III_Technical", "Cert_III_Nursing",
  "Certificate_III", "Cert_IV_Trade", "Cert_IV_Technical", "Certificate_IV",
  "Diploma_Trade", "Diploma_Teaching", "Diploma_Nursing", "Diploma",
  "Advanced_Diploma", "AdvDiploma_Trade",
  "Bachelor_General", "Bachelor_Honours_General", "Bachelor_Nursing",
  "Bachelor_Teaching", "GradDipCert_General", "GradDipCert_Nursing",
  "GradDipCert_Teaching", "Masters_General", "Masters_Teaching",
  "Doctorate_General", "Doctorate_Teaching", "Registered_Nurse")
stopifnot(all(unique(analysis_sample$education_level) %in% education_order),
          all(unique(analysis_sample$education_pathway) %in% education_pathway_order))
analysis_sample$education_level <- factor(analysis_sample$education_level,
                                          levels = education_order, ordered = TRUE)
analysis_sample$education_pathway <- factor(analysis_sample$education_pathway,
                                            levels = education_pathway_order, ordered = TRUE)
stopifnot(min(analysis_sample$wave) <= 12, max(analysis_sample$wave) >= 24)

# ── 20. PANEL ATTRIBUTES (what the compat guards and designs read) ───────────
RESTRICTIONS <- data.frame(
  restriction = c("firm_size", "education_w24", "entrant_only", "interruption",
                  "superannuation", "wealth", "training_gate", "experience_level",
                  "training_gate_position", "occ_origin_coverage",
                  "income_aggregates", "compensation_base"),
  rule = c(sprintf("do not pool across wave %d", FIRM_SIZE_BREAK_WAVE),
           sprintf("wave %d is a break, not a trend", STUDY_ITEM_BREAK_WAVE),
           "NPQ items: entrant counts, not panel counts",
           "use the within-person flow, not the level",
           "household aggregate; no individual comparison",
           sprintf("waves %s only", paste(WEALTH_MODULE_WAVES, collapse = "/")),
           "_any imputes a zero where never asked; _obs DROPS those rows",
           "carries the entry-cohort recall vintage; absorb it or share it",
           sprintf(paste("whole training module moves five questions later at",
                         "wave %d (gate E1 -> E6); confounded with 2020"),
                   TRAINING_GATE_POSITION_BREAK_WAVE),
           sprintf("job-change route ends at wave %d; coverage halves",
                   OCC_ORIGIN_BREAK_WAVE - 1L),
           "gross REGULAR vs disposable, one imputed one not",
           paste("wage, wage+sacrifice and wage+sacrifice+non-cash are THREE",
                 "outcomes: name which one, and do not mix them across tables")),
  panel_key = c("firm_size_regime", "study_item_regime / educ_trend_comparable",
                "entrant_only_cols attribute", "d_olf_years / olf_year / olf_flow_valid",
                "hh_super_balance", "wealth_module_wave",
                "training_module_asked / training_gate_cols attribute",
                "experience_level_cols attribute", "training_gate_regime",
                "occ_origin_regime / occ_origin_source",
                "fy_income_gross_regular / fy_income_disposable",
                "weekly_earnings / weekly_earnings_inc_ss / weekly_compensation"),
  stringsAsFactors = FALSE)

a_emp <- analysis_sample$employed == 1
a_ask <- analysis_sample$training_module_asked == 1L
r_obs_non <- mean(analysis_sample$jttpewt_obs[!a_emp & a_ask], na.rm = TRUE)
n_gap_non <- sum(!a_ask & !a_emp)
training_gate_facts <- list(
  n_not_asked = sum(!a_ask),
  n_not_asked_employed = sum(!a_ask & a_emp),
  n_not_asked_nonemployed = n_gap_non,
  pct_asked_employed = 100 * mean(a_ask[a_emp]),
  pct_asked_nonemployed = 100 * mean(a_ask[!a_emp]),
  rate_obs_nonemployed = r_obs_non,
  rate_any_nonemployed = mean(analysis_sample$jttpewt_any[!a_emp]),
  implied_understatement = round(n_gap_non * r_obs_non),
  structural_zero_note = paste0(
    "jttpewt_any and jttpeot_any read a never-asked person-wave as 0. ",
    "The module is employment-routed, so that zero is IMPUTED for the ",
    format(n_gap_non, big.mark = ","),
    " non-employed person-waves never asked and MEASURED for the ",
    sprintf("%.1f%%", 100 * mean(a_ask[!a_emp])), " who were. Among the latter ",
    "the observed rate is ", sprintf("%.3f", r_obs_non),
    ". SYNTHETIC DATA: these counts describe the synthetic panel only."))

attr(analysis_sample, "training_aim_cols") <- list(
  items = c("training_aim_future", "training_aim_current", "training_aim_general",
            "training_aim_getstarted", "training_aim_maintain", "training_aim_safety"),
  indices = c("promotion_training", "maintenance_training",
              "promotion_training_wide", "maintenance_training_wide"),
  gated = c("promotion_training_z", "maintenance_training_z",
            "promotion_training_wide_z", "maintenance_training_wide_z",
            "training_aim_current_z", "training_aim_general_z"),
  gate = "aim_battery_asked",
  nonresponse = "training_aim_nonresponse",
  not_an_aim = c("training_self_funded", "training_self_funded_z"),
  mapping = c(promotion = "jttrpfj", promotion_wide = "jttrpfj|jttrhgs",
              maintenance = "jttrmps", maintenance_wide = "jttrmps|jttrhsc"),
  note = paste("Ungated columns are observed only where the person trained",
               "and CONDITION ON THE OUTCOME. Use the _z twins for a design",
               "on everyone asked."))
attr(analysis_sample, "compensation_cols") <- list(
  published = "weekly_earnings",
  rungs = c("weekly_earnings", "weekly_earnings_inc_ss", "weekly_compensation"),
  hourly = c("ln_hourly_wage_real", "ln_hourly_compensation_real"),
  components = c(sacrifice = "salary_sacrificed_weekly",
                 noncash = "noncash_total_weekly", business = "fy_business_income"),
  noncash_parts = c("noncash_benefits_weekly", "noncash_other_jobs_weekly"),
  imputation = c("wage_imputed", "wage_ss_imputed", "noncash_imputed",
                 "noncash_other_imputed"),
  base_note = paste("weekly_earnings and weekly_earnings_inc_ss are ALL JOBS;",
                    "noncash_benefits_weekly is the MAIN JOB; business income",
                    "is ANNUAL and is not in weekly_compensation."),
  note = "Synthetic panel: same structure as the v21.49 attribute.")
attr(analysis_sample, "build_tag")    <- BUILD_TAG
attr(analysis_sample, "restrictions") <- RESTRICTIONS
attr(analysis_sample, "entrant_only_cols") <- c(
  "enrolled_in_course", "enrolled_in_course_yes", "has_nursing_any",
  "has_nursing_strict", "has_teaching_any", "has_trade_any",
  "has_technical_any", "years_npq_total")
attr(analysis_sample, "training_gate_cols") <-
  c("jttpewt_any", "jttpeot_any", "training_own_time", "training_paid_time")
attr(analysis_sample, "training_gate_facts") <- training_gate_facts
attr(analysis_sample, "experience_level_cols") <- c(
  "experience_years", "actual_experience", "years_since_education",
  "years_not_in_lf", "years_unemployed", "nonemployment_years",
  "base_exp_cal", "exp_hybrid_cal", "exp_hybrid_fte",
  "exp_hybrid_fte_obs", "exp_hybrid_one_obs")
attr(analysis_sample, "restriction_constants") <- list(
  firm_size_break_wave  = FIRM_SIZE_BREAK_WAVE,
  study_item_break_wave = STUDY_ITEM_BREAK_WAVE,
  wealth_module_waves   = WEALTH_MODULE_WAVES,
  olf_level_comparable  = OLF_LEVEL_COMPARABLE,
  super_is_household    = SUPER_IS_HOUSEHOLD)
attr(analysis_sample, "synthetic") <- TRUE
attr(analysis_sample, "synthetic_seed") <- SYNTH_SEED

# ── 21. SAVE AND SUMMARISE ───────────────────────────────────────────────────
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
f_slim <- file.path(OUT_DIR, "hilda_panel_data_W12_W24_slim.rds")
saveRDS(analysis_sample, f_slim)
cat(sprintf("Saved %s: %s rows x %d cols\n", f_slim,
            format(nrow(analysis_sample), big.mark = ","), ncol(analysis_sample)))
ext_sample <- dplyr::filter(analysis_sample, wave >= 15)
f_ext <- file.path(OUT_DIR, "hilda_panel_data_extended.rds")
saveRDS(ext_sample, f_ext)
cat(sprintf("Saved %s: %s rows (waves 15-24)\n", f_ext,
            format(nrow(ext_sample), big.mark = ",")))

roster <- analysis_sample %>%
  dplyr::group_by(person_id) %>%
  dplyr::summarise(ep0 = ever_parent[which.min(wave)], ever = max(ever_parent),
                   .groups = "drop")
n_lc <- sum(roster$ep0 == 1L)
n_tr <- sum(roster$ep0 == 0L & roster$ever == 1L)
n_nt <- sum(roster$ep0 == 0L & roster$ever == 0L)
cat(sprintf("Persons %s | treated %s + never-treated %s = roster %s | left-censored %s\n",
            format(nrow(roster), big.mark = ","), format(n_tr, big.mark = ","),
            format(n_nt, big.mark = ","), format(n_tr + n_nt, big.mark = ","),
            format(n_lc, big.mark = ",")))
cat(sprintf("Couples with both partners in the panel: %s\n",
            format(sum(!is.na(analysis_sample$partner_id) &
                         analysis_sample$partner_id %in% analysis_sample$person_id &
                         !duplicated(analysis_sample$person_id)) %/% 2, big.mark = ",")))
cat(sprintf("Done in %.1f s. SYNTHETIC DATA: the numbers these files produce are\n",
            as.numeric(difftime(Sys.time(), .syn_t0, units = "secs"))))
cat("random and must not be reported.\n")
