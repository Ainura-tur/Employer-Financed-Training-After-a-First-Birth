# v-c9-followup, 19 October: 609 -> 613 and 0.00121 -> 0.00123 (apply_c9_followup.R).
# =====================================================================
# vm_settings.R
#
# Settings for verify_manuscript_numbers.R. Source this file, then the
# verifier, from the scripts folder:
#
#   source("vm_settings.R")
#   source("verify_manuscript_numbers.R")
#
# The settings live here, not in the verifier, so that a new version of
# the verifier does not wipe them. Entries take the form "literal=file",
# exactly as the verifier's log prints them.
# =====================================================================

# Write prose_numbers_check.csv and manuscript_reconciliation.csv.
WRITE_REPORT <- TRUE

# Literals checked by hand and accepted. Each comment names the file or log
# that holds the value, so the check can be redone. An accepted literal is
# exempt wherever it appears, so accept only values that occur once or
# always mean the same thing. check9_sources.R recomputes the entries
# marked [c9] from their sources; run it after any rerun that could move
# them.
VM_ACCEPTED_LITERALS <- c(
  # Arithmetic in the sentence.
  "78,300",                   # twice 39,150, the couple-waves in A.4 (v-unscreened-values, 18 October);
                              # was "74,990", twice 37,495, on the screened sample
  "8,194",                    # never-treated persons on the roster: 10,902 - 2,708, as paper_tables.R
                              # prints the split; was "8,135" (10,838 - 2,703) with the screen
  "4,009",                    # [c9] screen_audit_location.csv: removed rows outside the roster,
                              #      3,524 women + 485 men (A.2)
  "15.6", "11.1",             # Table C.1, employer tenure: 27.0 - 11.4 and 27.0 - 15.9 (C.2).
                              #      Was "13.4" (23.9 - 10.5) with the screen.
  "56,639",                   # 03a_design_audit log, T12: estimation rows, the sum of the eight
                              #      cells of the levels table (footnote 1). v-unscreened-values-6, 18 October
  "$261",                     # the incidence gap times the mean dose: 0.1271 x $2,052 (7.3 and F.3;
                              # 04financing_space log). Was "$262" with the screen. v-unscreened-values-3, 18 October
  # Not results.
  "3168",                     # postcode in the author address
  "-9031", "-2784", "-2762", "-2428",  # ORCID iDs in the author block, which the scan reads as numbers
  "250",                      # program-day bound set in the code
  "180",                      # the "more than 180 days" cut-off in the Wave 24 passage; a bin edge in w24_calendar_check.R
  # Held where Check 9 does not read them as numbers.
  "99.5", "99.8",             # loader log of 24 Sep 21:21, non-employment history table
  "195", "230",               # [c9] v2114_fieldwork_window.csv: shortest and longest span_days, Waves 12-24 (A.4).
                              #      The loader writes it to the project root, which Check 9 does not scan.
  "219", "201",               # [c9] the same file: mean span_days of Waves 21-24 and of Waves 12-20 (A.4)
  "613",                      # [c9] open_items_queries.csv (CS_event_study.R): fathers aged 30-49 seen at k >= 5,
                              #      held as the text "613 persons" in the note column (D.3). Was "609" with the screen;
                              #      corrected by check9_sources.R on 19 October (v-c9-followup)
  "237", "224",               # [c9] (was "236" with the screen) T3c_fee_by_horizon_decomposed.csv: n x p_t at k = 3, men and women (B.2)
  "32.6", "29.4",             # [c9] the same file: men's training incidence before and after the birth, the
                              #      n-weighted mean of p_t over k = -4..-1 and k = 0..6 that 03b prints
                              #      and does not write (B.2)
  "25.1",                     # [c9] the same file and windows: women's trainee fee share before the birth (B.2)
  "22.1",                     # [c9] T3c_fee_level_by_group.csv: never-mothers' pct_contributed (B.2). It is
                              #      stored as 22.05, which sprintf() and formatC() print as 22.1, but round(),
                              #      which Check 9 uses, gives 22.0.
  "0.00123",                  # [c9] training_decisions.txt, T2: the never-treated drift (E.1, twice). Check 9
                              #      reads no .txt file. Was "0.00121" with the screen, which Check 9 kept passing
                              #      through a covariance cell in cs_event_study_vcov.rds, a coincidence (v-c9-followup)
  "472",                      # [c9] T3c_fee_by_horizon_decomposed.csv: n x p_t at k = -1, women (B.2)
  "320",                      # 04financing_space log: median hours in the p99 tail (F.3)
  "$290",                     # 04financing_space log: MDE of T11_inv_hi, 289.50 (F.3). v-unscreened-values-4, 18 October
  "25.9", "17.8",             # [c9] T3c_fee_by_horizon_decomposed.csv: n-weighted trainee share over
                              #      k = 0..6, women and men (B.2; 03b prints these and does not write them)
  "2.5",                      # men's fall in the contribution rate on the printed endpoints, 7.7 - 5.2 (B.2).
                              #      v-unscreened-values-5, 18 October
  "1.30"                      # [c9] T3c_fee_by_horizon_decomposed.csv: the lowest women-to-men ratio of the
                              #      printed trainee shares from the birth wave on, 24.3 / 18.7 at k = 5 (B.2)
)

# No longer in the text on 9 October, so these are not applied. An accepted
# literal exempts every occurrence, and two of them had come to exempt
# different numbers: 13.1 is now the top of the middle half of experience at
# a first birth (C.4), and 18.7 is now the public-sector rate in footnote 1
# and F.8. Kept here as the record of where each value came from.
#   "-44.4", "-35.2", "38.4"   Lee bounds printed by block B2 of training_funding_divergence.R (MASTER log)
#   "$272", "$45"              T11_investment_dollars.csv: 0.1331 x $2,043.18; the MDE half-width
#   "2.86"                     timing_paths.csv: 0.0532 / 0.0186
#   "3.16", "3.17"             training_decomposition.csv, employed-only paid-time arms
#   "11.9", "3.4"              03b_fee_descriptives log of 21 Sep: -0.119 and -0.034
#   "42.6", "30.8", "32.7", "23.7", "17.7"   the same 03b log, lines 367-368
#   "25.9"                     referee_G_fee_states.csv: sums of the two fee states' shares
#   "227", "222", "481", "156" T3c_fee_by_horizon_decomposed.csv: n x p_t, rounded
#   "13.1"                     Table 4, employer tenure: 23.8 - 10.7
#   "$122"                     T11_investment_dollars.csv, 122.21; the text showed 0.133 x $918
#   "321"                      reconcile_pre_birth_base.R rerun: 130 + 191, printed, not written
#   "705", "596"               open_items_queries.csv holds these as text ("705 persons"); MASTER log, 24 Sep
#   "0.00163"                  training_decisions.txt, T2; the record is a .txt file Check 9 does not read.
#                              The drift is now 0.00121, accepted above.
#   "18.7"                     dosage_decomposition.csv, unconditional, total_courses_z: part2_pct -18.65
#   "637"                      05_review_response.R R12: mothers with a dated birth, the total of the share table its log prints
#   "21.4", "25.2"             referee_G_fee_states.csv, the wider frame; B.6 now prints the roster keys Gr and Grboot (v-G-roster-3, 18 October)

# Fraction matches read against their sentence and confirmed as the source.
VM_CONFIRMED_FRACTIONS <- c(
  "29.8=reconcile_pre_birth_base.csv",    # pre-birth base, women
  "29.8=T0_pre_birth_base_rates.csv",     # pre-birth base, women, where 03 computes it
  "6.0=T11_investment_waterfall.csv"     # mixed_state_share 0.0603: mixed states, share of the T11 estimation rows (F.3). v-G-roster-3, 18 October
)

# No longer in the text on 9 October, so these are not applied. A confirmed
# pair counts its literal as found wherever that file holds the fraction, so
# a pair kept after its sentence has gone could pass a different number later.
#   "20.6=referee_G_fee_states_probs.csv"   the four states preserve the ordering
#   "-9.8=train_ledger.csv"                 the triple difference
#   "5.9=T2_profile_by_event_time.csv"      the lead at k = -7, in points
#   "12.4=T2_profile_by_event_time.csv"     the lead at k = -8, in points
#   "11.7=tab_training_main.tex"            Table 2's headline -0.117, in percentage points
#   "91.6=referee_G_fee_states_probs.csv"   the fee-bearing share of workplace training rises; now 88.1
#   "15.4=PH_R7_hours_accounting.tex"       05 R7, Panel A: employer-financed training at 1-15 against 35-44 hours, people with no birth
#   "-6.3=train_ledger.csv"                 the pre-specified triple difference, quoted in points in the Introduction
#   "6.3=tab_training_main.tex"             the same, in the Discussion and the Conclusion
#   "93.4=referee_G_fee_states_probs.csv", "87.4=referee_G_fee_states_probs.csv"   the wider frame; B.6 now prints Grprob keys (v-G-roster-3, 18 October)
#   "69.8=own_time_decomposition.csv", "85.2=own_time_decomposition.csv"   04's base over every pre-birth wave; B.6 now prints Grownsum keys (v-G-roster-5, 18 October)

# Fraction matches read and rejected: standard errors, p-values and other
# cells that share the digits but not the quantity. An entry covers every
# cell of that file for that literal. Entries whose literal has left the
# text do no harm: a rejection can only keep a literal unfound.
VM_REJECTED_FRACTIONS <- c(
  "-35.2=training_funding_divergence_results.rds",
  "11.9=training_decomposition.csv",
  "11.9=training_event_study_results.rds",
  "11.9=training_funding_divergence_results.rds",
  "17.7=training_funding_divergence_results.rds",
  "17.8=training_decomposition.csv",
  "18.6=training_decomposition.csv",
  "18.6=training_funding_divergence_results.rds",
  "21.4=training_funding_divergence_results.rds",
  "21.6=training_funding_divergence_results.rds",
  "23.6=training_decomposition.csv",
  "23.6=training_event_study_results.rds",
  "23.6=training_funding_divergence_results.rds",
  "23.7=training_funding_divergence_results.rds",
  "25.9=training_funding_divergence_results.rds",
  "29.4=training_decomposition.csv",
  "29.4=training_funding_divergence_results.rds",
  "32.7=training_funding_divergence_results.rds",
  "36.2=training_funding_divergence_results.rds",
  "38.4=training_funding_divergence_results.rds",
  "42.6=training_funding_divergence_results.rds",
  "49.6=training_funding_divergence_results.rds",
  "62.9=training_funding_divergence_results.rds",
  "70.1=training_funding_divergence_results.rds",
  "73.5=training_funding_divergence_results.rds",
  "86.4=D_gap_by_event_time.csv",
  "86.4=training_funding_divergence_results.rds",
  "99.5=C_increment_by_state.csv",
  "99=screen_removals_by_group.csv",
  "99=training_decomposition.csv",
  "11.7=training_decomposition.csv",
  "11.7=training_event_study_results.rds",
  "11.7=training_funding_divergence_results.rds",
  "23.7=training_event_study_results.rds",
  "91.6=training_funding_divergence_results.rds",
  # Cells on the gated course count, and one aim p-value, that share the
  # digits of 15.4, 23.7 and 29.4 but not the quantity.
  "15.4=training_decomposition.csv",
  "15.4=training_event_study_results.rds",
  "15.4=training_funding_divergence_results.rds",
  "23.7=training_decomposition.csv",
  "29.4=training_event_study_results.rds",
  # Added 9 October: probabilities, standard errors and the other sex's cells.
  "99.8=training_funding_divergence_results.rds",     # a training probability; the sentence is about reconciling histories
  "25.1=training_funding_divergence_results.rds",     # a training probability; the sentence is the fee-contribution rate
  "32.6=training_funding_divergence_results.rds",     # women's selection share; the sentence is men's incidence
  "32.6=dosage_decomposition.csv",                    # the same
  "13.4=training_funding_divergence_results.rds",     # a standard error; the sentence is the tenure spread
  "22.1=training_decomposition.csv",                  # a standard error; the sentence is a group's cost share
  "29.4=training_ledger.csv",                         # a T8 standard error
  "29.4=training_ledger_03c_experience_units.csv"     # the same
)

# Command names that LaTeX, a package or the document class defines. Check 10
# of the verifier and the macro report in paper_tables.R both add these to
# their built-in list, so neither reports them as values a script owes.
# paper_tables.R still writes a \providecommand fallback for each, which
# LaTeX ignores because the name is already defined.
VM_LATEX_OWN_NAMES <- c(
  "MakeUppercase",            # LaTeX kernel; the running head
  "DeclareRobustCommand",     # LaTeX kernel; defines \authorfill
  "IfFileExists",             # LaTeX kernel; inputs Appendix I when it is present
  "Keywords",                 # AEA.cls, front matter
  "PackageError"              # LaTeX kernel; \pn's error for an unknown key, in paper_numbers.tex
)
