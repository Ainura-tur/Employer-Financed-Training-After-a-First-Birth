# Replication Package

## Whose Investment Falls? Employer-Financed Training After a First Birth

Ainura Tursunalieva (CSIRO) and Ratbek Dzhumashev (Monash University)

This README follows the Social Science Data Editors template. It documents the
data, the code, the computing environment, and the steps a replicator needs to
reproduce every table and figure in the paper. The analysis data are
confidential, so this package does not include them; Section 2 explains how to
obtain them. The package does include a generator for a synthetic HILDA panel,
so the whole pipeline can run without the confidential data (Section 1).

---

## 1. Overview

The code builds the analysis panel from the raw HILDA release and reproduces
the paper's exhibits. It is written in R. One script, `run_all.R`, runs the
pipeline in one of two modes.

**Synthetic mode** is the default. `00b_make_synthetic_hilda.R` writes a
synthetic panel with the same columns, types, factor levels and panel
attributes as the real one, and `00c_check_synthetic_panel.R` checks it. The
analysis stages then run on it. This mode needs no access to HILDA. Use it to
check that the code installs and runs, to read the pipeline, or to develop
against it. **Results in this mode are illustrative and do not reproduce the
paper's estimates.**

**Real mode** (`DATA_MODE=real`) builds the panel from the restricted HILDA
release with `LOADING_ND3_v3.R` and runs the authors' verified sequence. Only
approved HILDA users can run it (Section 2).

### 1a. Running the pipeline

From the repository root:

```sh
Rscript run_all.R                      # synthetic mode
DATA_MODE=real Rscript run_all.R       # real mode, restricted data
```

In synthetic mode, `run_all.R`:

1. installs and version-checks the packages (`PACKAGES.R`);
2. reads the settings (`config.R`), which point the pipeline's contract at
   this folder;
3. builds the synthetic panel if it is not on disk (`00b`), then checks it
   (`00c`);
4. runs the analysis stages in the authors' order, each in its own R process:
   `02 -> 03a -> 03b -> 03c -> 04 -> master -> 05 -> pre_analysis -> robust
   -> referee8 -> 06 -> figures -> tables`;
5. prints a summary with each stage's status and run time.

It skips the steps that need the raw release or the manuscript: the loader,
the Wave 24 calendar check and the number verifiers. By default it also
shortens the two slow steps. Stage 03c fits its DML arms with a quick lasso
pass, and the referee bootstrap is skipped. `SYNTH_FULL=1` runs both at the
published settings.

Useful settings, all read from the environment:

| Variable | Default | Effect |
| --- | --- | --- |
| `DATA_MODE` | `synthetic` | `real` uses the restricted release |
| `N_PERSONS` | `6000` | size of the synthetic panel |
| `SYNTH_SEED` | `20261021` | seed of the synthetic panel |
| `SYNTH_REBUILD` | unset | `1` rebuilds the synthetic panel |
| `SYNTH_FULL` | unset | `1` runs the slow steps at published settings |
| `SYNTH_KEEP_GOING` | unset | `1` runs every stage after one fails |
| `HILDA_ROOT` | this folder | the root the pipeline's contract resolves |
| `HILDA_DATA_DIR` | `data/raw` | the raw release (real mode) |
| `MASTER_HH_THREADS` | up to 4 | threads for fixest, ranger and grf |

### 1b. What the synthetic panel is, and what it is not

The synthetic panel is random data with the schema of the analysis panel that
`LOADING_ND3_v3.R` (build v21.49) writes: the same 414 columns in the same
order, the same types and factor levels, and the panel attributes the
estimation guards read (`build_tag`, `restrictions`, `restriction_constants`,
`entrant_only_cols`, `training_gate_cols`, `training_gate_facts`,
`experience_level_cols`, `training_aim_cols`, `compensation_cols`). It also
carries `attr(, "synthetic") = TRUE`.

It builds in the structure the designs depend on:

- an unbalanced panel, with late entry, attrition and a wave-24 refresher;
- absorbing parenthood on own children only, so the event-study roster has
  treated persons, never-treated persons and left-censored parents;
- couples linked by `partner_id`, with both partners of an in-panel first
  birth sharing the birth wave;
- a training module routed through employment, with the gate, the `_any` and
  `_obs` twins, the dose, the fee item and the aim battery;
- module items (fertility, wealth) only in their module waves, and
  entrant-only items only for new entrants.

It also plants effects of the kind the paper studies, so the estimates carry
sensible signs:

| Planted effect | Value in the generator |
| --- | --- |
| Employer-financed training, mothers after the birth | -0.075, plus a lower rate in part-time jobs |
| Own-time training at the workplace, mothers | -0.05 at the birth wave, -0.012 after |
| Fathers' training | no change |
| Pre-birth training among the employed | about 0.29 for women, 0.20 for men |
| Mothers' hourly wage | no direct change for four years, then -0.011 a year |

Every number produced in synthetic mode is an artefact of the generator. These
numbers are not estimates of the paper's quantities, change with the seed and
the panel size, and must not be reported or cited. Every result in the
manuscript comes from real mode on HILDA General Release 24. The synthetic
panel and all outputs are gitignored and are not part of the package.

---

## 2. Data availability and provenance statements

### 2.1 Statement of rights

The authors certify that they have legitimate access to, and permission to use,
the data used in this manuscript, obtained under a confidentiality deed with the
Australian Data Archive. The authors did not obtain permission to redistribute
the data, so the data are not included in this package.

### 2.2 License for the data

The HILDA Survey data are confidential. They are governed by the Australian
Data Archive confidentiality deed and the conditions of the Department of
Social Services, and they cannot be redistributed. No part of the source data
is included in this repository.

### 2.3 Summary of data availability

The data cannot be made publicly available. Approved researchers can obtain
them free of charge through the restricted-access process described below.

### 2.4 Data Availability Statement

The analysis uses unit-record data from the HILDA Survey, General Release 24
(DOI 10.26193/6M1BMR), managed by the Melbourne Institute and funded by the
Australian Government Department of Social Services (DSS). The data are
confidential and cannot be redistributed. Approved researchers can apply for
them, free of charge, through the DSS Longitudinal Studies Dataverse in the
Australian Data Archive (https://dataverse.ada.edu.au/dataverse/hilda),
subject to a signed confidentiality deed. The General Release is available to
researchers outside Australia. All code needed to build the analysis sample
from the raw release and to reproduce every table and figure is in this
repository; the raw data are not. A synthetic panel generator is included so
the pipeline can run without the confidential data.

Note to the editor and Data Editor: access is restricted under a
confidentiality deed, so the authors cannot provide the microdata, even
privately. The Data Editor can obtain identical data by signing the same deed
with DSS through the Australian Data Archive. The authors commit to keeping the
code for at least five years and to answering reasonable requests for
clarification.

### 2.5 Data citation

Department of Social Services; Melbourne Institute of Applied Economic and Social
Research. 2024. *The Household, Income and Labour Dynamics in Australia (HILDA)
Survey, General Release 24.* ADA Dataverse. https://doi.org/10.26193/6M1BMR.

### 2.6 Details on the data source

| Item | Detail |
| --- | --- |
| Dataset | HILDA Survey, General Release 24, Waves 12 to 24 (2012 to 2024) |
| Provider | Australian Data Archive (ADA), DSS Longitudinal Studies Dataverse |
| DOI | 10.26193/6M1BMR |
| Access | Restricted; free to approved researchers under a confidentiality deed |
| Format expected | Stata files `Combined_l240c.dta` to `Combined_x240c.dta` |
| Location in package | `data/raw/`, or the folder named by `HILDA_DATA_DIR`; gitignored |

The HILDA release is the only source dataset. No other external data are used.

---

## 3. Dataset list

The loader (real mode) or the generator (synthetic mode) writes the analysis
panels to the repository root. The pipeline then writes derived files under
`output/`, `handoff/` and `fte_experience/`. All of them are gitignored.

| File | Written by | Used by |
| --- | --- | --- |
| `hilda_panel_data_W12_W24_slim.rds` | `LOADING_ND3_v3.R` / `00b_make_synthetic_hilda.R` | every design |
| `hilda_panel_data_extended.rds` | `LOADING_ND3_v3.R` / `00b_make_synthetic_hilda.R` | the wave 15 to 24 subset |
| `fte_experience/panel_fte_addendum.rds` | `02fte_experience.R` | 03c, the experience arms |
| `handoff/*.rds` | the contract's registry | later stages (frame, event sample, ledger) |
| `output/designs/training_ledger.csv` | 03a, 03b, 03c, 04 | the ledger table, `paper_tables.R` |

---

## 4. Computational requirements

### 4.1 Software

The reference results were produced with R 4.5 on Windows 11 x64, with fixest
0.14.2. `PACKAGES.R` installs and version-checks the CRAN packages: tidyverse,
fixest, lmtest, sandwich, patchwork, DoubleML, mlr3, mlr3learners, ranger,
glmnet, grf, lgr, haven and digest. `run_all.R` sources it first.

### 4.2 Controlled randomness

The synthetic panel uses its own seed (`SYNTH_SEED`, default 20261021), so a
given seed and panel size always give the same panel. The pipeline sets its own
seeds: the master sets `set.seed(42)`, the DML learners in 03c use fixed seeds,
and each bootstrap sets its seed where it runs.

### 4.3 Memory and runtime

A full real-data pass takes about sixteen hours, most of it in 03c's twenty DML
arms. A workstation with at least 16 GB of RAM is recommended. In synthetic
mode, the generator takes under a minute for 6,000 persons, and the check
about a minute; the stages run much faster than on the real panel, because the
panel is smaller and 03c uses the lasso pass by default.

---

## 5. Description of programs

Run everything from the repository root: the scripts read and write through
the contract's paths, which `config.R` points at this folder.

### 5.1 Package files

| File | Role |
| --- | --- |
| `run_all.R` | entry point; runs synthetic or real mode |
| `config.R` | data mode, project root, raw-data folder, synthetic options |
| `PACKAGES.R` | installs and version-checks the dependencies |
| `00b_make_synthetic_hilda.R` | writes the synthetic panels (Section 1b) |
| `00c_check_synthetic_panel.R` | checks the synthetic panel: schema, invariants, planted effects |
| `data/raw/README.md` | where real mode looks for the raw release |

`00c` tests the generator, not the paper. Its part C fits simplified versions
of the paper's equations (the static difference-in-differences, the triple
difference, the Sun and Abraham average with common cohort weights, and the
wage windows) only to confirm that the designs recover the planted effects.

### 5.2 Pipeline

| File | Role |
| --- | --- |
| `LOADING_ND3_v3.R` | raw release to the analysis panels (real mode) |
| `00_contract.R` | paths, the handoff registry and shared helpers; sourced by every stage |
| `00compat.R` | estimation-stage guards (the restrictions the panel carries) |
| `03_logging.R`, `03_common.R`, `03_finish.R` | prelude and finisher of the 03 modules |
| `02fte_experience.R` | full-time-equivalent experience addendum |
| `03a_design_audit.R` | designs T0b, T1, T2, T4 to T7 and T9; writes most ledger rows |
| `03b_fee_descriptives.R` | designs T3, T3b and T3c: the financing contrast and the fee margin |
| `03c_experience_units.R` | design T8: the twenty DML arms on experience units |
| `04financing_space.R` | designs T10 and T11: the financing states and their dollar value |
| `MASTER_training_y.R` | the master: event studies, the wage path, the cross-partial profile |
| `CS_event_study.R`, `training_event_study.R`, `training_funding_divergence.R`, `sa_common_weights.R` | sourced by the stages that need them |
| `REFEREE_RUN.R` | bootstrap behind the six-state decomposition |
| `05_review_response.R` | the post-hoc analyses and the Appendix I tables |
| `Pre_analysis_diagnostics2.R` | writes `headline_figures.csv`, which the tables read |
| `w24_calendar_check.R` | the Wave 24 parental-leave check; reads the raw Wave 24 file (real mode) |
| `paper_numbers.R`, `paper_numbers_derived.R`, `MASTER_paper_numbers.R` | the registry behind the numbers the text quotes |
| `robust_30_49.R` | estimates on the conventional 30 to 49 age window |
| `referee8_couple_clustering.R` | couple-robust standard errors |
| `06_revision_inference.R` | couple block bootstrap and the event-study financing contrast |
| `RUN_FIGURES_PAPER.R` | every figure |
| `paper_tables.R` | every table fragment and the macro files |
| `FINAL_PASS.R`, `RUN_ALL.R` | the authors' drivers, used in real mode |
| `reconcile_pre_birth_base.R`, `RECONCILE_COUNTS.R` | reconcile the counts the text quotes (real mode) |
| `verify_manuscript_numbers.R`, `vm_settings.R`, `vm_guards.R`, `check9_sources.R` | check the manuscript against the outputs (real mode) |

---

## 6. Instructions to replicators

### Synthetic mode (no restricted data)

```sh
Rscript run_all.R
```

This installs the dependencies, builds and checks the synthetic panel, runs
the stages, and writes tables and figures under `output/`. The results do not
match the paper. The summary at the end lists each stage as ok, ERROR,
skipped or absent, and `run_logs/` holds each stage's log.

### Real mode (reproduces the published results)

1. Obtain HILDA General Release 24 from the Australian Data Archive (Section 2).
   Put the Stata files in `data/raw/`, or set `HILDA_DATA_DIR` to their folder.
2. Run the full pipeline:

   ```sh
   DATA_MODE=real Rscript run_all.R
   ```

   `run_all.R` builds the panel with the loader if it is not on disk, runs
   `FINAL_PASS.R`, then the post-pass stages through `RUN_ALL.R`: robust,
   referee8, 06, figures, tables and the two verifiers. `RUN_ALL.R` compares
   each stage's log with the values from the verified run and stops at the
   first stage whose numbers differ.

To run one stage, use the authors' driver from R, for example
`RUN_STAGES <- c("03a", "04"); source("RUN_ALL.R")`.

---

## 7. Exhibits and the code that produces them

`paper_tables.R` writes every table fragment the manuscript inputs, and
`RUN_FIGURES_PAPER.R` writes every figure. Each reads results that an earlier
stage writes, named below.

### Tables

| Fragment | Exhibit | Upstream source |
| --- | --- | --- |
| `descriptives_table` | Table 1, descriptive statistics | the panel |
| `tab_training_main` | Table 2, mothers minus fathers | 03a, 03b, the master, 05 |
| `tab_t2_profile` | raw training rate by event time | `T2_profile_by_event_time.csv` (03a) |
| `tab_training_coverage` | module coverage by wave | `T0_module_coverage_by_wave.csv` (03a) |
| `missingness_group` | missingness by group | `T0b_missingness_by_group.csv` (03a) |
| `tab_which_side` | which side moves at the birth wave | the Sun and Abraham arms (master) |
| `tab_hours_arms` | the hours arms | `hours_robustness.csv` (master) |
| `tab_mde_ledger` | the specification ledger | `training_ledger.csv` (03a, 03b, 03c, 04) |
| `tab_fee_horizon` | the fee margin by horizon | `T3c_fee_by_horizon_decomposed.csv` (03b) |
| `tab_t11_investment` | employer investment by event time | `T11_investment_by_k.csv` (04) |
| `wagepath_table` | the wage path | `cs_event_study_results.rds` (master) |
| `app_age_window` | full and conventional age windows | `robust_30_49.R` |
| `app_b2_profile` | the cross-partial at common experience | the master, block A5 |
| `app_arms`, `app_gate`, `app_repro` | the twenty experience arms | `T8_dml_swap.csv` (03c) |
| `PH_*` | Appendix I, post-hoc analyses | `05_review_response.R`, block R16 |

### Figures

| File | Exhibit | Upstream source |
| --- | --- | --- |
| `P3_margin_split` | which margin moves | the panel |
| `alt_T1_ordering_pooled` | training and wages around the birth | the Sun and Abraham paths (master) |
| `alt_T1_which_side_pooled` | which side moves | the Sun and Abraham arms (master) |
| `SA_financing_contrast` | the financing contrast | `06_revision_inference.R` |
| `P4_wage_sample_selection` | wage-sample coverage | the panel |
| `P2_training_experience_profile` | training against experience | the panel |

---

## 8. License

The code is released under the BSD-3-Clause license; see `LICENSE`. The
license covers this repository only. It does not extend to the HILDA data,
which remain subject to the ADA confidentiality deed.

---

## 9. References

Chernozhukov, Victor, Denis Chetverikov, Mert Demirer, Esther Duflo, Christian
Hansen, Whitney Newey, and James Robins. 2018. "Double/Debiased Machine Learning
for Treatment and Structural Parameters." *Econometrics Journal* 21 (1): C1-C68.

Lee, David S. 2009. "Training, Wages, and Sample Selection: Estimating Sharp
Bounds on Treatment Effects." *Review of Economic Studies* 76 (3): 1071-1102.

Rambachan, Ashesh, and Jonathan Roth. 2023. "A More Credible Approach to
Parallel Trends." *Review of Economic Studies* 90 (5): 2555-2591.

Sun, Liyang, and Sarah Abraham. 2021. "Estimating Dynamic Treatment Effects in
Event Studies with Heterogeneous Treatment Effects." *Journal of Econometrics*
225 (2): 175-199.

Department of Social Services; Melbourne Institute of Applied Economic and Social
Research. 2024. *The Household, Income and Labour Dynamics in Australia (HILDA)
Survey, General Release 24.* ADA Dataverse. https://doi.org/10.26193/6M1BMR.
