# =============================================================================
# 00c_check_synthetic_panel.R
#
# Checks the synthetic panel that 00b_make_synthetic_hilda.R writes. It tests
# the GENERATOR, not the paper. Three parts:
#
#   A. Schema. The 414 columns of the v21.49 panel, in the loader's order,
#      the factor levels, and the panel attributes the compat guards read.
#   B. Invariants the pipeline relies on: absorbing parenthood, the roster,
#      reciprocal couple links, the training gate, module and entrant-only
#      items, empty-by-design columns, no removed column names.
#   C. Planted structure. Simplified versions of the paper's estimating
#      equations, fitted on the synthetic panel: the static
#      difference-in-differences, the triple difference, the Sun-Abraham
#      women-minus-men average with common cohort weights, and the wage
#      windows. They confirm that the designs find the effects the generator
#      builds in. They are NOT the pipeline's code, and their numbers are
#      synthetic and must not be reported.
#
# Usage, from the repository root (after 00b_make_synthetic_hilda.R):
#   Rscript 00c_check_synthetic_panel.R
# It stops with an error if any check in A or B fails.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
})

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

ROOT <- if (exists("PROJ_ROOT", envir = globalenv())) {
  get("PROJ_ROOT", envir = globalenv())
} else if (nzchar(Sys.getenv("HILDA_ROOT"))) Sys.getenv("HILDA_ROOT") else getwd()
f_panel <- file.path(ROOT, "hilda_panel_data_W12_W24_slim.rds")
if (!file.exists(f_panel))
  stop("No panel at ", f_panel, ". Run 00b_make_synthetic_hilda.R first.", call. = FALSE)
panel <- readRDS(f_panel)
OUT <- file.path(ROOT, "output", "synthetic_check")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

fails <- character(0)
check <- function(label, ok) {
  ok <- isTRUE(ok)
  cat(sprintf("  [%s] %s\n", if (ok) "PASS" else "FAIL", label))
  if (!ok) fails <<- c(fails, label)
  invisible(ok)
}
cat(strrep("=", 72), "\n00c_check_synthetic_panel.R\n", strrep("=", 72), "\n", sep = "")
if (!isTRUE(attr(panel, "synthetic")))
  cat("NOTE: this panel does not carry attr(, \"synthetic\"). It may be the real\n",
      "panel; parts A and B still apply, and part C then estimates real data.\n", sep = "")

# ── A. SCHEMA ────────────────────────────────────────────────────────────────
cat("\nA. Schema\n")
gen <- file.path(ROOT, "00b_make_synthetic_hilda.R")
if (!file.exists(gen)) gen <- "00b_make_synthetic_hilda.R"
exprs <- parse(gen)
is_col_order <- vapply(exprs, function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
                         identical(e[[2]], as.name("col_order")), logical(1))
col_order <- eval(exprs[[which(is_col_order)[1]]][[3]])
check(sprintf("%d columns, in the loader's order", length(col_order)),
      identical(names(panel), col_order))
check("education_level is the loader's ordered factor",
      is.ordered(panel$education_level) && nlevels(panel$education_level) == 21L)
check("education_pathway is the loader's ordered factor",
      is.ordered(panel$education_pathway) && nlevels(panel$education_pathway) == 35L)
check("identifiers are character (person_id, xwaveid, partner_id, household_id)",
      all(vapply(panel[c("person_id", "xwaveid", "partner_id", "household_id")],
                 is.character, logical(1))))
check("interview dates are Date", inherits(panel$interview_date, "Date") &&
        inherits(panel$ref_window_start, "Date"))
need_attr <- c("build_tag", "restrictions", "entrant_only_cols", "training_gate_cols",
               "training_gate_facts", "experience_level_cols", "restriction_constants",
               "training_aim_cols", "compensation_cols")
check("panel attributes the guards read are present",
      all(vapply(need_attr, function(a) !is.null(attr(panel, a)), logical(1))))
rc <- attr(panel, "restriction_constants")
check("restriction constants match the compat file (22, 24, 14/18/22)",
      isTRUE(all.equal(rc$firm_size_break_wave, 22L)) &&
        isTRUE(all.equal(rc$study_item_break_wave, 24L)) &&
        isTRUE(all.equal(rc$wealth_module_waves, c(14L, 18L, 22L))))
cls <- vapply(panel, function(x) class(x)[1], character(1))
cat("  column classes:", paste(sprintf("%s %d", names(table(cls)), as.integer(table(cls))),
                             collapse = ", "), "\n")

# ── B. INVARIANTS ────────────────────────────────────────────────────────────
cat("\nB. Invariants\n")
check("one row per person-wave", !anyDuplicated(panel[c("person_id", "wave")]))
check("waves 12 to 24, ages 20 to 55",
      min(panel$wave) == 12L && max(panel$wave) == 24L &&
        min(panel$age) >= 20 && max(panel$age) <= 55)
mono <- panel %>% arrange(person_id, wave) %>% group_by(person_id) %>%
  summarise(ok = all(diff(ever_parent) >= 0), .groups = "drop")
check("ever_parent is absorbing within person", all(mono$ok))
check("ever_parent_life is constant within person",
      all(tapply(panel$ever_parent_life, panel$person_id, function(z) length(unique(z)) == 1L)))
pid <- panel$partner_id[!is.na(panel$partner_id)]
own <- panel$person_id[!is.na(panel$partner_id)]
link <- data.frame(a = own, b = pid)
link <- unique(link[link$b %in% panel$person_id, ])
back <- paste(link$b, link$a) %in% paste(link$a, link$b)
check("couple links are reciprocal", all(back))
asked <- panel$training_module_asked == 1L
check("_obs twins are NA exactly where the module was not asked",
      all(is.na(panel$jttpewt_obs) == !asked) && all(is.na(panel$jttpeot_obs) == !asked))
check("_any indicators read a never-asked person-wave as zero",
      all(panel$jttpewt_any[!asked] == 0) && all(panel$jttpeot_any[!asked] == 0))
check("training is reported only where the module was asked",
      all(panel$any_training[!asked] == 0))
check("almost every employed person-wave is asked (routing through employment)",
      mean(asked[panel$employed == 1]) > 0.999)
fert <- c("currently_pregnant", "partner_pregnant", "self_or_partner_pregnant",
          "expected_birth_month", "expected_birth_year")
check("fertility-module items only at waves 15, 19, 23",
      all(vapply(fert, function(v) all(is.na(panel[[v]][!panel$wave %in% c(15, 19, 23)])),
                 logical(1))))
wl <- c("hh_super_balance", "hh_net_worth", "hh_net_worth_pos", "hh_net_worth_neg")
check("wealth items only at waves 14, 18, 22",
      all(vapply(wl, function(v) all(is.na(panel[[v]][!panel$wave %in% c(14, 18, 22)])),
                 logical(1))))
ent <- panel %>% group_by(person_id) %>% mutate(.first = wave == min(wave),
                                                .late = min(wave) > 12L) %>% ungroup()
check("entrant-only items observed only at a new entrant's first wave",
      all(vapply(attr(panel, "entrant_only_cols"), function(v)
        all(is.na(ent[[v]][!(ent$.first & ent$.late)])), logical(1))))
check("empty-by-design columns are all NA",
      all(is.na(panel$firm_size_comparable)) && all(is.na(panel$enrolled_in_course_comparable)) &&
        all(is.na(panel$has_business_any)))
check("no column the loader removed (event_birth, super_balance, ...)",
      !any(c("event_birth", "had_birth_this_wave", "super_balance",
             "years_unemp_since_school", "earnings_total_has_business") %in% names(panel)))
check("firm size is classified on both sides of wave 22, as on the real panel",
      any(!is.na(panel$firm_size[panel$wave < 22])) && any(!is.na(panel$firm_size[panel$wave >= 22])))

# Roster, exactly as 03_common.R builds it.
coh <- panel %>% filter(!is.na(ever_parent)) %>% arrange(person_id, wave) %>%
  group_by(person_id) %>%
  summarise(ep_at_entry = ever_parent[which.min(wave)], ever = max(ever_parent),
            birth_wave = if (any(ever_parent == 1L)) min(wave[ever_parent == 1L]) else NA_integer_,
            .groups = "drop")
n_lc <- sum(coh$ep_at_entry == 1L)
coh <- coh %>% filter(ep_at_entry == 0L) %>%
  mutate(cohort = ifelse(ever == 0L, 10000L, as.integer(birth_wave)))
ev <- panel %>% inner_join(coh %>% select(person_id, cohort), by = "person_id") %>%
  mutate(treated = as.integer(cohort != 10000L),
         k = ifelse(treated == 1L, wave - cohort, NA_integer_),
         post = ifelse(treated == 1L, as.integer(k >= 0), 0L),
         post_f = post * female)
cat(sprintf("\n  Roster: %s person-waves, %s persons (%s treated, %s never-treated); left-censored %s\n",
            format(nrow(ev), big.mark = ","), format(n_distinct(ev$person_id), big.mark = ","),
            format(n_distinct(ev$person_id[ev$treated == 1]), big.mark = ","),
            format(n_distinct(ev$person_id[ev$treated == 0]), big.mark = ","),
            format(n_lc, big.mark = ",")))
check("the roster has treated mothers and fathers in every birth wave 13-24",
      all(13:24 %in% ev$cohort[ev$female == 1]) && all(13:24 %in% ev$cohort[ev$female == 0]))

pre <- ev %>% filter(treated == 1, employed == 1, k >= -3, k <= -1) %>%
  group_by(female) %>%
  summarise(n = n(), employer_financed = mean(jttpewt_any), own_time = mean(jttpeot_any),
            any_training = mean(any_training), .groups = "drop")
cat("\n  Pre-birth training among the employed, k = -3 to -1:\n")
print(as.data.frame(pre), row.names = FALSE, digits = 3)
grp <- panel %>% group_by(group_4way_women_life) %>%
  summarise(n = n(), employed = mean(employed), fulltime_if_emp = mean(fulltime_emp, na.rm = TRUE),
            wage_sample = mean(in_wage_sample), .groups = "drop")
cat("\n  Women by lifetime group (NA = men):\n")
print(as.data.frame(grp), row.names = FALSE, digits = 3)

# ── C. PLANTED STRUCTURE, ON THE PAPER'S EQUATIONS ───────────────────────────
cat("\nC. Planted structure (synthetic numbers, for checking only)\n")
emp_ev <- ev %>% filter(employed == 1)
res <- list()
add <- function(label, est, se, planted) {
  res[[length(res) + 1]] <<- data.frame(check = label, estimate = est, se = se,
                                        planted = planted, stringsAsFactors = FALSE)
  cat(sprintf("  %-46s %+.4f (SE %.4f)   planted: %s\n", label, est, se, planted))
}
did <- function(y, d) {
  m <- feols(as.formula(paste(y, "~ post + post_f | person_id + wave^female")),
             data = d, cluster = ~person_id)
  c(coef(m)[["post_f"]], se(m)[["post_f"]])
}
r <- did("jttpewt_any", emp_ev)
add("Static DiD, employer-financed (employed)", r[1], r[2],
    "-0.075 direct, plus the part-time shift")
r <- did("jttpeot_any", emp_ev)
add("Static DiD, own-time at the workplace", r[1], r[2], "-0.05 at k = 0, -0.012 after")

stk <- bind_rows(
  emp_ev %>% transmute(person_id, wave, female, post, post_f, y = jttpewt_any, src = 1L),
  emp_ev %>% transmute(person_id, wave, female, post, post_f, y = jttpeot_any, src = 0L)) %>%
  mutate(pid_src = paste(person_id, src), post_e = post * src, post_fe = post_f * src)
m3 <- feols(y ~ post + post_f + post_e + post_fe | pid_src + wave^src^female,
            data = stk, cluster = ~person_id)
add("Triple difference, employer less own time", coef(m3)[["post_fe"]], se(m3)[["post_fe"]],
    "about the employer DiD less the own-time DiD")

# Sun-Abraham by sex against same-sex never-treated, then the women-minus-men
# difference within each cohort-by-event-time cell, averaged with pooled counts.
sa_arm <- function(d, y) {
  d <- d %>% filter(!is.na(.data[[y]]))
  m <- feols(as.formula(paste(y, "~ sunab(cohort, wave) | person_id + wave")),
             data = d, cluster = ~person_id)
  # The cohort-by-period coefficients, named "wave::<k>:cohort::<g>".
  b <- coef(m, agg = FALSE); V <- vcov(m)
  keep <- grepl("^wave::-?[0-9]+:cohort::[0-9]+$", names(b))
  nm <- names(b)[keep]
  kk <- as.integer(sub("^wave::(-?[0-9]+):cohort::.*$", "\\1", nm))
  g  <- as.integer(sub("^.*:cohort::([0-9]+)$", "\\1", nm))
  cnt <- d %>% filter(treated == 1) %>% count(cohort, k, name = "N")
  list(est = data.frame(term = nm, g = g, k = kk, b = unname(b[keep])) %>%
         left_join(cnt, by = c(g = "cohort", k = "k")),
       V = V[nm, nm, drop = FALSE])
}
sa_gap <- function(d, y, kset) {
  fa <- sa_arm(d %>% filter(female == 1), y); ma <- sa_arm(d %>% filter(female == 0), y)
  cells <- inner_join(fa$est, ma$est, by = c("g", "k"), suffix = c("_f", "_m")) %>%
    filter(k %in% kset, !is.na(N_f), !is.na(N_m))
  w <- (cells$N_f + cells$N_m) / sum(cells$N_f + cells$N_m)
  est <- sum(w * (cells$b_f - cells$b_m))
  v <- t(w) %*% fa$V[cells$term_f, cells$term_f] %*% w +
    t(w) %*% ma$V[cells$term_m, cells$term_m] %*% w
  c(est, sqrt(as.numeric(v)))
}
emp_sa <- emp_ev %>% filter(is.na(k) | (k >= -8 & k <= 9))
r <- sa_gap(emp_sa, "jttpewt_any", 0:9)
add("Sun-Abraham, employer-financed, average k >= 0", r[1], r[2],
    "-0.075 direct, plus the part-time shift")
r_pre <- sa_gap(emp_sa, "jttpewt_any", -8:-2)
add("Sun-Abraham, employer-financed, leads k = -8..-2", r_pre[1], r_pre[2], "0 (no pre-trend)")

wage <- ev %>% filter(in_wage_sample == 1, is.na(k) | (k >= -8 & k <= 10))
early <- sa_gap(wage, "ln_hourly_wage_real", 0:4)
late  <- sa_gap(wage, "ln_hourly_wage_real", 5:10)
add("Wage gap, mean over k = 0..4", early[1], early[2], "0 directly (experience channel only)")
add("Wage gap, mean over k = 5..10", late[1], late[2], "-0.011 per year from k = 5")

out <- do.call(rbind, res)
utils::write.csv(out, file.path(OUT, "synthetic_panel_check.csv"), row.names = FALSE)
cat("\n  Written to ", file.path(OUT, "synthetic_panel_check.csv"), "\n", sep = "")

cat("\n", strrep("=", 72), "\n", sep = "")
if (length(fails)) {
  cat(length(fails), "check(s) FAILED:\n", paste0("  - ", fails, collapse = "\n"), "\n")
  stop("synthetic panel check failed", call. = FALSE)
}
cat("All schema and invariant checks passed. The estimates in part C are\n",
    "synthetic and only show that the designs recover the planted effects.\n", sep = "")
