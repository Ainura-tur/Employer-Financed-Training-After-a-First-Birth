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

**Synthetic mode** is the default. `R/00b_make_synthetic_hilda.R` writes a
synthetic panel with the same columns, types, factor levels and panel
attributes as the real one, and `tests/00c_check_synthetic_panel.R` checks it.
The analysis stages then run on it. This mode needs no access to HILDA. Use it to
check that the code installs and runs, to read the pipeline, or to develop
against it. **Results in this mode are illustrative and do not reproduce the
paper's estimates.**

**Real mode** (`DATA_MODE=real`) builds the panel from the restricted HILDA
release with `R/LOADING_ND3_v3.R` and runs the authors' verified sequence. Only
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
   `R/`, the folder every stage runs from;
3. builds the synthetic panel if it is not on disk (`00b`), then checks it
   (`00c`);
4. runs the analysis stages in the order of the authors' build, each in its
   own R process: `02 -> 03a -> 03b -> 03c -> 04 -> master -> referee ->
   05 -> pre_analysis -> robust -> referee8 -> 06 -> screen -> figures ->
   tables`;
5. prints a summary with each stage's status and run time.

It skips the steps that need the raw release or the manuscript: the loader,
the Wave 24 calendar check, and the number verifiers with their
reconciliations. It also shortens the slow steps. Stage 03c fits its DML arms
with a quick lasso pass, and the bootstraps in the referee stage, 05,
referee8 and 06 use 49 replicates. `SYNTH_FULL=1` runs all of them at the
published settings.

Many scripts compare their results with the published values and print a
warning when the two differ. Examples are the reproduction block in `05`,
the standard-error reconciliation in the master and the narrative checks in
`paper_tables.R`. On synthetic data these comparisons fail by design, and
they do not stop the run. The checks that guard the pipeline itself compare
one stage with another stage of the same run, so they still apply.
`robust_30_49.R` and `screen_audit.R` first refit the estimates this run
published, and stop if they do not reproduce them. `referee8` checks its
roster against 03a's. Its built-in counts are those of the real roster, so in
synthetic mode `run_all.R` passes it the counts of 03a's synthetic roster.

Useful settings, all read from the environment:

| Variable | Default | Effect |
| --- | --- | --- |
| `DATA_MODE` | `synthetic` | `real` uses the restricted release |
| `N_PERSONS` | `6000` | persons drawn for the synthetic panel |
| `SYNTH_SEED` | `20261021` | seed of the synthetic panel |
| `SYNTH_REBUILD` | unset | `1` rebuilds the synthetic panel and removes the previous synthetic run's outputs |
| `SYNTH_FULL` | unset | `1` runs the slow steps at published settings |
| `SYNTH_KEEP_GOING` | unset | `1` runs every stage after one fails |
| `HILDA_ROOT` | `R/` | the root the pipeline's contract resolves; if set, it must name `R/`, or `config.R` stops |
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
| Employer-financed training, mothers after the birth | -0.075 from the birth wave to k = 7, then -0.06; part-time hours lower the rate further |
| Own-time training at the workplace, mothers | -0.05 at the birth wave, -0.012 after |
| Fathers' training | no change |
| Employer-financed training among the employed, before any birth | base rates of 0.29 for women and 0.19 for men, raised by public-sector jobs, union membership and longer hours |
| Mothers' log hourly wage | no direct change through k = 4, then a fall of 0.011 a year |

Every number produced in synthetic mode is an artefact of the generator. These
numbers are not estimates of the paper's quantities, change with the seed and
the panel size, and must not be reported or cited. Two exceptions run the other
way: `paper_tables.R` writes Table 2 (`tab_training_main`) and the hours-arms
table (`tab_hours_arms`) from rows held in the script, so in synthetic mode
those two fragments still show the published estimates. Every result in the
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
panels to `R/`, the folder the pipeline runs from. The pipeline then writes
derived files under `R/output/`, `R/handoff/` and `R/run_logs/`. All of them
are gitignored.

| File | Written by | Used by |
| --- | --- | --- |
| `R/hilda_panel_data_W12_W24_slim.rds` | `LOADING_ND3_v3.R` / `00b_make_synthetic_hilda.R` | every design |
| `R/hilda_panel_data_extended.rds` | `LOADING_ND3_v3.R` / `00b_make_synthetic_hilda.R` | the wave 15 to 24 subset |
| `R/output/fte_experience/panel_fte_addendum.rds` | `02fte_experience.R` | 03c, the experience arms |
| `R/handoff/*.rds` | the contract's registry | later stages: the frame, the event sample, the ledger |
| `R/output/designs/training_ledger.csv` | 03a, 03b, 03c, 04 | the ledger table |
| `R/output/results/cs_event_study_results.rds` | `CS_event_study.R`, through the master | the wage path, 05, the tables |
| `R/output/diagnostics/*.csv` | robust, referee8, 06, screen | the appendix tables and macros |
| `R/output/numbers/*.csv` | the master, 05 and other stages | `paper_numbers.tex`, the numbers the text quotes |

---

## 4. Computational requirements

### 4.1 Software

The reference results were produced with R 4.5.2 on Windows 11 x64 (build
26100), with fixest 0.14.2. `PACKAGES.R` installs and version-checks the CRAN
packages: tidyverse, fixest, lmtest, sandwich, patchwork, DoubleML, mlr3,
mlr3learners, ranger, glmnet, grf, lgr, haven and digest. `run_all.R` sources
it first. The synthetic mode was also tested with R 4.4.3 on Linux, with
fixest 0.14.2 and newer versions of the other packages, among them mlr3 1.8.0,
ranger 0.18.0, glmnet 5.0 and grf 2.6.1.

### 4.2 Controlled randomness

The synthetic panel uses its own seed (`SYNTH_SEED`, default 20261021), so a
given seed and panel size always give the same panel. The pipeline sets its own
seeds: the master sets `set.seed(42)`, the DML learners in 03c use fixed seeds,
and each bootstrap sets its seed where it runs.

### 4.3 Memory and runtime

On the real data, 03c is the longest stage by far: its twenty DML arms take
about 100 minutes at the published settings. The bootstraps in the referee
stage, 05 and 06 take up to about half an hour each, and the other stages take
minutes, so a full real-data pass takes a few hours. Peak memory on the real
data was not recorded. A workstation with at least 16 GB of RAM is
recommended.

The default synthetic run was timed from a fresh copy of the repository, with
the packages already installed, on a Linux machine with two cores and 7 GB of
RAM. It took about 13 minutes:

| Step | Minutes |
| --- | --- |
| build and check the synthetic panel (00b, 00c) | 0.4 |
| master | 2.9 |
| 05 | 3.9 |
| 06 | 2.7 |
| each other stage | under 1 |

Timings vary with load: an earlier run on the same machine, with other work
running, took about 20 minutes. Peak memory was about 4 GB, in the master.
The real panel has about three times as many person-waves as the default
synthetic one (134,131 against 43,153), so it needs more.

`SYNTH_FULL=1` takes much longer, mostly in 03c. The default run fits 03c's
twenty arms by lasso with 2 folds and 1 repetition. Fitted with random forests,
as in the paper, at 2 folds and 1 repetition, 03c took 6 minutes on the same
machine. The published 5 folds and 3 repetitions fit 7.5 times as many forests
on larger folds, so expect roughly an hour and a quarter for 03c alone. On that
machine the forests ran on one thread. 03c gives them one thread fewer than the
machine has cores, up to four, unless `MASTER_HH_THREADS` sets the number, so a
machine with more cores runs it faster.

---

## 5. Description of programs and code

The entry point and the settings live at the repository root, and the
pipeline lives in `R/`. `run_all.R` runs every stage with `R/` as the working
directory. The authors' scripts source one another by bare file name and
write their outputs beside them, as they did in the authors' project folder.
So during a run the panels, `output/`, `handoff/`, `run_logs/` and
`els-cas-templates/` all appear under `R/`, and `.gitignore` keeps them out of
the repository. The order of the stages matters: each stage reads files that
earlier stages wrote, through the contract's handoff registry.

```
repo-root/
├── README.md                          this file
├── LICENSE                            BSD-3-Clause
├── .gitignore                         keeps data, panels and outputs out of the repository
├── run_all.R                          entry point; runs synthetic or real mode
├── config.R                           data mode, folders and synthetic-run options
├── PACKAGES.R                         installs and version-checks the R packages
├── data/
│   └── raw/                           empty; HILDA General Release 24 goes here (real mode)
├── tests/
│   └── 00c_check_synthetic_panel.R    checks the synthetic panel: schema, invariants and
│                                      planted effects
└── R/                                 the pipeline; every stage runs from here
    │   # the analysis panels
    ├── 00b_make_synthetic_hilda.R     synthetic stand-in for the two panels (synthetic mode)
    ├── LOADING_ND3_v3.R               raw HILDA release to the two panels (real mode)
    │   # shared code, sourced by the stages
    ├── 00_contract.R                  paths, the handoff registry and shared helpers
    ├── 00compat.R                     guards for the restrictions the panel carries
    ├── paths.R                        older path settings, kept as a fallback
    ├── 03_logging.R                   prelude of the 03 modules
    ├── 03_common.R                    frame and designs shared by the 03 modules
    ├── 03_finish.R                    finisher of the 03 modules; merges the ledger
    ├── sa_common_weights.R            women-minus-men contrasts on common cohort weights
    ├── paper_numbers.R                the registry behind the numbers the text quotes
    ├── paper_numbers_derived.R        numbers the text derives from the registry
    │   # the analysis stages, in run order
    ├── 02fte_experience.R             02: full-time-equivalent experience addendum
    ├── 03a_design_audit.R             03a: designs T0b, T1, T2, T4 to T7, T9 and T12
    ├── 03b_fee_descriptives.R         03b: designs T3, T3b and T3c; financing contrast, fees
    ├── 03c_experience_units.R         03c: design T8, the twenty DML arms
    ├── 04financing_space.R            04: designs T10 and T11; financing states, dollars
    ├── MASTER_training_y.R            master: event studies, wage path, cross-partial
    │                                  profile; sources the four modules below
    ├── CS_event_study.R               the Sun and Abraham wage path
    ├── training_event_study.R         the training event studies
    ├── training_funding_divergence.R  the funding contrast
    ├── MASTER_paper_numbers.R         registers the master's numbers
    ├── REFEREE_RUN.R                  referee: checks A to G, the six-state bootstrap
    ├── 05_review_response.R           05: post-hoc analyses and the Appendix I tables
    ├── w24_calendar_check.R           the Wave 24 leave check (real mode)
    ├── Pre_analysis_diagnostics2.R    pre_analysis: headline_figures.csv for the tables
    ├── robust_30_49.R                 robust: the conventional 30 to 49 age window
    ├── referee8_couple_clustering.R   referee8: couple-robust standard errors
    ├── 06_revision_inference.R        06: couple block bootstrap, event-study contrast
    ├── screen_audit.R                 screen: estimates with and without the experience screen
    ├── RUN_FIGURES_PAPER.R            figures: every figure
    ├── paper_tables.R                 tables: every table fragment and the macro files
    │   # the authors' drivers (real mode)
    ├── FINAL_PASS.R                   the full build
    ├── RUN_ALL.R                      runs named stages; checks each log against the
    │                                  verified run
    │   # checks against the manuscript (real mode)
    ├── reconcile_pre_birth_base.R     reconciles the pre-birth base the text quotes
    ├── RECONCILE_COUNTS.R             asserts the counts the text quotes
    ├── verify_manuscript_numbers.R    checks every number in the manuscript
    ├── vm_settings.R                  the verifier's accepted values
    └── vm_guards.R                    the verifier's guards
```

`run_all.R` sources `PACKAGES.R` and `config.R`, then moves into `R/`. In
synthetic mode it builds the panel with `00b_make_synthetic_hilda.R` if the
panel is not on disk, and checks it with `tests/00c_check_synthetic_panel.R`.
It then runs each stage in its own R process, in the order of the tree: 02,
03a, 03b, 03c, 04, master, referee, 05, pre_analysis, robust, referee8, 06,
screen, figures and tables. It leaves out the loader, the Wave 24 check and
the checks against the manuscript, because they need the raw release or the
manuscript. In real mode it runs the loader through `RUN_ALL.R` if the panel
is not on disk, then `FINAL_PASS.R`, then `RUN_ALL.R` for robust, referee8,
06, screen, figures, tables, verify and counts.

Several files are modules that other scripts source rather than stages that
run on their own. Every stage loads `00_contract.R`; the three 03 stages
do so through `03_logging.R`, and they also source `03_common.R` and
`03_finish.R`. The master sources
`CS_event_study.R`, `training_event_study.R`, `training_funding_divergence.R`
and `MASTER_paper_numbers.R`. `sa_common_weights.R` is sourced by
`CS_event_study.R`, the two training modules, 05, 06, robust and screen.
`paper_numbers.R` is sourced by 03a, the master, the referee stage, 05 and the
tables, and it reads `paper_numbers_derived.R`.

`00c` tests the generator, not the paper. Its part C fits simplified versions
of the paper's equations (the static difference-in-differences, the triple
difference, the Sun and Abraham average with common cohort weights, and the
wage windows) only to confirm that the designs recover the planted effects.

The scripts keep the authors' working file names, so the files in `R/` match
those in the authors' project folder one for one.

### 5.1 Edits made for this package

Three of the authors' scripts carry a small edit for this package. Each edit
is marked `v-replication, 21 October 2026` in the file, and none of them
changes a result on the real data.

| File | Edit |
| --- | --- |
| `LOADING_ND3_v3.R` | reads the raw-data folder from `HILDA_DATA_DIR` when it is set |
| `CS_event_study.R` | accepts the flagged synthetic panel, which is smaller than the script's floor of 15,000 persons |
| `paper_numbers.R` | on synthetic data, a phrase such as "seven-tenths" that no longer fits its number gives a warning instead of a stop |

One further edit corrects a coding error found while testing this package. It
is marked `v-jobchange, 22 October 2026`.

| File | Edit |
| --- | --- |
| `REFEREE_RUN.R` | block F counted every answered job-change item as a job change, because the loader codes the item 1 = no and 2 = yes; it now counts only a 2. This changes the job-change arm in Online Appendix F.1 |

Two more edits correct the run time that the authors' drivers give for 03c.
They are marked `v-runtime, 22 October 2026` and change no result.

| File | Edit |
| --- | --- |
| `RUN_ALL.R` | declares 03c at 100 minutes instead of 1000, so it prints "(about 100 min)" when the stage starts. Its notes and console message no longer say sixteen hours: they give 100 minutes for 03c and about three hours for the full pass |
| `FINAL_PASS.R` | a note set stage 02's three minutes against "03c's thousand"; it now says hundred |

Two edits put the last British spellings in the generated tables into US
spelling, to match the paper. They are marked `v-usspell, 22 October 2026` and
`v-usspell-e1, 22 October 2026` and change no result.

| File | Edit |
| --- | --- |
| `05_review_response.R` | the R16 notes for Online Appendix Tables I5 to I8 say "percent" and "centered" instead of "per cent" and "centred" |
| `paper_tables.R` | Online Appendix Table E1 labels its two study rows "study enrollment" instead of "study enrolment". The label for `T9_study_gap` is set in `LEDGER_ARM_LABELS`, so 03a and the ledger are unchanged |

---

## 6. Instructions to replicators

### Synthetic mode (no restricted data)

```sh
Rscript run_all.R
```

This installs the dependencies, builds and checks the synthetic panel and runs
the stages. The pipeline's contract decides where results go, all under `R/`.
Figures go to `R/els-cas-templates/figures/`, the folder the manuscript reads.
Table fragments go to `R/output/tables/`, and `paper_tables.R` copies each
finished fragment to `R/els-cas-templates/tables/`. Estimates and diagnostics
go to `R/output/designs/`, `R/output/results/` and `R/output/diagnostics/`.
The results do not match the paper. The summary at the end lists each stage as
ok, ERROR or absent, and `R/run_logs/` holds each stage's log.

### Real mode (reproduces the published results)

1. Obtain HILDA General Release 24 from the Australian Data Archive (Section 2).
   Put the Stata files in `data/raw/`, or set `HILDA_DATA_DIR` to their folder.
2. Run the full pipeline:

   ```sh
   DATA_MODE=real Rscript run_all.R
   ```

   `run_all.R` builds the panel with the loader if it is not on disk, runs
   `FINAL_PASS.R`, then the post-pass stages through `RUN_ALL.R`: robust,
   referee8, 06, screen, figures, tables and the two checks against the
   manuscript, verify and counts. `RUN_ALL.R` compares each stage's log with
   the values from the verified run and stops at the first stage whose
   numbers differ. The Wave 24 calendar check reads the raw Wave 24 file from
   `HILDA_W24_FILE`.
3. Three steps compare the results with the manuscript's text: the verifier
   step in `FINAL_PASS.R` and the verify and counts stages. They read the
   LaTeX source, which this repository does not hold, so put
   `Motherhood_training_v5.tex` in `R/els-cas-templates/` before the run. The
   figures and tables do not need it.

To run one stage, use the authors' driver. In R, from the repository root:

```r
source("config.R")      # points the contract at R/
setwd(PIPELINE_DIR)     # R/, where every stage runs
RUN_STAGES <- c("03a", "04"); source("RUN_ALL.R")
```

---

## 7. Exhibits and the code that produces them

`paper_tables.R` writes every table fragment the manuscript inputs, to
`R/output/tables/` and then `R/els-cas-templates/tables/`, and
`RUN_FIGURES_PAPER.R` writes every figure, to `R/els-cas-templates/figures/`.
Each reads results that an earlier stage writes, named below.

### Tables

| Fragment | Exhibit | Source |
| --- | --- | --- |
| `descriptives_table` | Table 1, descriptive statistics | the panel |
| `tab_training_main` | Table 2, mothers minus fathers | rows held in `paper_tables.R`, checked against the ledger by `verify_manuscript_numbers.R`; the Sun and Abraham row reads `cs_post_period_att.csv` |
| `tab_t2_profile` | raw training rate by event time | `T2_profile_by_event_time.csv` (03a) |
| `tab_training_coverage` | module coverage by wave | `T0_module_coverage_by_wave.csv` (03a) |
| `missingness_group` | missingness by group | `T0b_missingness_by_group.csv` (03a) |
| `tab_which_side` | which side moves at the birth wave | the Sun and Abraham arms (master) |
| `tab_hours_arms` | the hours arms | rows held in `paper_tables.R`, checked against `hours_robustness.csv` |
| `tab_mde_ledger` | the specification ledger | `training_ledger.csv` (03a, 03b, 03c, 04) |
| `tab_t11_investment` | employer investment by event time | `T11_investment_by_k.csv` (04) |
| `wagepath_table` | the wage path | `cs_event_study_results.rds` (master) |
| `app_age_window` | full and conventional age windows | `robust_30_49.R` |
| `app_b2_profile` | the cross-partial at common experience | the master, block A5 |
| `app_arms` | the twenty experience arms | `T8_dml_swap.csv` (03c) |
| `app_screen_compare` | estimates with and without the experience screen | `screen_audit_sa.csv` (`screen_audit.R`) |
| `PH_*` | Appendix I, post-hoc analyses | `05_review_response.R`, block R16 |
| `data_section_numbers`, `paper_numbers` | the numbers the text quotes, as macros | the stage outputs and the registry in `output/numbers/` |

`paper_tables.R` also writes three fragments that the current draft does not
input: `tab_fee_horizon`, the fee margin by horizon (03b), and `app_gate` and
`app_repro`, the divergence gate and the reproduction check behind the
experience arms (03c).

### Figures

| File | Exhibit | Source |
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
