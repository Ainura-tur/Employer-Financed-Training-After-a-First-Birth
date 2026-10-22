# =====================================================================
# CLEAR THE WORKSPACE, AND CLEAR THE FLAGS THAT SAY IT IS NOT EMPTY
#
# v21.47. `rm(list = ls())` was unconditional here and it has now caused a
# failure two scripts away, by a route worth writing down in full.
#
# ls() DEFAULTS TO all.names = FALSE, so this line deletes every ordinary
# object and NO dot-name. 03_common.R sets `.TD03_COMMON_DONE <- TRUE`
# and defines FE_SEX_WAVE, fe_block, ev and the rest as ordinary names.
# After a loader run in the same session the constants are gone and the
# flag that says they are present survives. The next design script reads
# the flag, prints "03_common.R already loaded this session; reusing ev
# and the helpers", skips the rebuild, recovers ev from handoff/ev.rds
# because that one has a file, and then dies on the first plain constant
# it reaches. On 5 September that was
#   Error in eval(ei, envir) : object 'FE_SEX_WAVE' not found
# in 03b's T3 block, which aborted the run before 03_finish.R could close
# the transcript, which left a sink open, which is why the next two
# loader runs wrote their output into 03a's log as well as their own.
# One line, three symptoms, none of them near it.
#
# SO THE WIPE ALSO CLEARS THE SENTINELS. Named explicitly rather than by
# deleting all dot-names, because .Random.seed is a dot-name and a
# pipeline that sets a seed should not have it removed underneath.
#
# AND IT ONLY RUNS WHEN NO OTHER TRANSCRIPT IS OPEN. An open sink means
# another script is mid-run and this file is inside its session, which is
# the one case where clearing the workspace is certainly wrong. That test
# is better than checking sys.nframe(), which cannot tell the IDE's
# Source button from a script sourcing another script.
#
# LOADER_STANDALONE overrides in either direction.
.ld_standalone <- if (exists("LOADER_STANDALONE"))
  isTRUE(LOADER_STANDALONE) else sink.number() == 0L

.LD_SENTINELS <- c(".TD03_COMMON_DONE", ".TD03_FINISH_DONE",
                   ".TD_LOGGING_OPEN", ".t_owns_sink", ".guard_used")

if (.ld_standalone) {
  .ld_flags <- .LD_SENTINELS[vapply(.LD_SENTINELS, exists,
                                    logical(1), envir = globalenv())]
  rm(list = setdiff(ls(), c(".ld_standalone", ".LD_SENTINELS", ".ld_flags")))
  if (length(.ld_flags)) {
    rm(list = .ld_flags, envir = globalenv())
    cat("Workspace cleared, including the state flags: ",
        paste(.ld_flags, collapse = ", "), "\n",
        "  Left set, they would tell the next script that objects this\n",
        "  line has just deleted are still present.\n", sep = "")
  }
  rm(.ld_flags)
  gc()
} else {
  cat("LOADING_ND3_v3.R: another transcript is open, so this looks like a\n",
      "  run inside someone else's session and the workspace is LEFT ALONE.\n",
      "  Output will also be written into that transcript, because the log\n",
      "  below opens with split = TRUE. Set LOADER_STANDALONE <- TRUE to\n",
      "  clear the workspace anyway, but read the note above first.\n",
      sep = "")
}

# RE-ENTRANCY. A second entry while the first is still on the stack is
# always a caller bug, so it stops rather than warns.
if (isTRUE(get0(".LD_RUNNING", envir = globalenv(), ifnotfound = FALSE)))
  stop("LOADING_ND3_v3.R is already running in this session. Something ",
       "sourced it from inside itself, or a guard that should have been ",
       "satisfied by the first run re-fired.", call. = FALSE)
assign(".LD_RUNNING", TRUE, envir = globalenv())

# =====================================================================
# HILDA_LOADER_UNIFIED_v2.R -- SINGLE LOADER FOR ALL ANALYSIS DATASETS
#
# =====================================================================
# v21.49 CHANGES. NO REBUILD. Two fixes, both applied after the waves
# are combined, so the wave caches stay valid and CHECKPOINT_TAG stays
# at v21_48_training_dosage_aim_dk. Each fix keeps the value it
# replaces, as ever_parent_ref and educ_years_ref, so every earlier
# result can still be reproduced from this panel.
#
# PARENTHOOD. ever_parent no longer counts household children under 15
# (hhd0_4, hhd5_9, hhd1014). Those counts include every child in the
# household, not only the respondent's own, and the wave mutate checked
# them before the reported-zero rule. So a person who reported no
# children of their own became a parent while a partner's child or a
# young sibling lived with them, and the cummax made that permanent.
# The new rule reads own-child sources only: tchad, tcr + tcnr, and
# ncany. On the v21.48 panel the household count was the only sign of a
# child on 759 person-waves, in 319 persons.
#
# The roster changes. After the age-experience filter it reads 2,704
# treated and 8,134 never-treated, a roster of 10,838, with 8,823
# left-censored. The old rule gives 2,705 / 7,945 / 10,650 with 9,011;
# its one-person split from the design's 2,704 / 7,946 predates this
# build. Of the old left-censored, 106 become never-treated and 82
# become treated. Of the old treated, 83 become never-treated and 35
# stay treated with a different event wave. On all persons, the pooled
# left-censoring shares fall from 21.5 / 25.5 to 20.8 / 25.3.
# ever_parent_life follows, and 190 lifetime parents become
# never-parents (124 men, 66 women). The roster diagnostic prints both
# rules side by side.
#
# EDUCATION. educ_years takes the first source that answers. For
# continuing respondents the qualification items it reads first cover
# only qualifications completed since the last interview, and edhigh1
# lumps levels the items split. So educ_years fell when a graduate
# later finished a certificate, and when edhigh1 took over from an item
# (Cert IV read as Cert III, honours as bachelor, a doctorate as a
# masters). On the v21.48 panel 9,999 person-waves (7.45%) in 2,981
# persons fell below a level the same person had reached earlier. A
# completed qualification cannot be lost, so educ_years is now the
# person's running maximum over waves. Missing stays missing, so the
# Tier 1 sample does not change.
#
# The columns built from educ_years are rebuilt where it rose: educ_sq,
# education_level, education_pathway, the four has_ flags,
# potential_experience, and experience_years and experience_sq where
# ehtjb is missing. The interactions and educ_centered are built later
# and follow. has_degree switches on for 893 person-waves and
# has_postgrad for 403. A check confirms that the copied rules still
# reproduce the wave mutate wherever educ_years did not move.
#
# THE COST OF THE MAXIMUM. A single over-reported qualification now
# carries forward to every later wave. The level table in the log shows
# which pairs drive the raises; most are the lumping above.
#
# WHAT THE FIRST RUN SHOWS. All waves restore from cache. The arrival
# contract checks ever_parent_ref and educ_years_ref, and the
# fingerprint lists them as added, beside the columns the fixes change.
# EXPECTED_PANEL_CHANGES names those. Clear both lists once the run
# passes. Re-run every script that reads ever_parent, the roster, the
# parenthood groups or educ_years.
#
# LEFT ALONE. The active/post split in motherhood_status and
# fatherhood_status still reads household children under 15, as do
# group_current and is_mother. The own-child counts by age (tcr04,
# tcr514) are the obvious replacement, but the comment at 6B says they
# include step, foster and grandchildren. Check that against the
# dictionary before switching; it is a decision, not a fix.
#
# =====================================================================
# v21.48 CHANGES. FULL REBUILD. The map gains jttrdk and jttrrf, which
# moves the hash, and CHECKPOINT_TAG moves by hand as well, because
# derived columns change inside the wave mutate.
#
# A check of the training items against the Release 24 dictionary found
# the core coding sound and two gaps in it. Both are fixed here.
#
# DAYS AND COURSES ARE NOW GATED. total_training_days and total_courses
# coalesce a missing count to zero. So a trainee who gave no count, and a
# person the module never reached, both read as zero. The hours column
# has used the right rule since v21.9, and two new twins follow it:
# total_training_days_z and total_courses_z are NA if never asked, 0 if
# asked and not trained, and the reported count otherwise. The ungated
# pair is unchanged, so every published number still reproduces. Re-run
# the courses dosage estimate on total_courses_z and the days event study
# on total_training_days_z, and compare.
#
# DON'T KNOW AND REFUSED NO LONGER COUNT AS "NO AIM". The loader did not
# read jttrdk or jttrrf, so a trainee who could not say what the training
# was for scored zero on all six aims. Those trainees are now NA on the
# six items, and the indices and _z twins inherit the NA.
# training_aim_nonresponse marks them. jttros, "other", stays unread: a
# zero on a listed aim is correct for a trainee whose aim was something
# else. The cost item, jttrcst, is a separate question and does not move.
#
# TWO GUARDS COME WITH IT. Both flags go through the training-item coding
# audit, and the build stops if "mentioned" covers more than
# TRAINING_AIM_NONRESPONSE_MAX of the battery: an inverted code would
# blank the aims for almost every trainee.
#
# WHAT THE FIRST RUN SHOWS. The fingerprint reports three columns added
# and up to sixteen aim columns changed. EXPECTED_PANEL_CHANGES names
# them, so the verdict tells this build's own changes apart from a stale
# cache. Clear it once the run passes. RUN_ALL's 0/0/0 check fails once.
#
# ALSO. The wave-20 note and the manifest rule now say the whole training
# module moves five questions later, not the gate alone. BUILD_TAG had
# stayed at v21.46 through v21.47 and now moves with the checkpoint tag.
#
# =====================================================================
# v21.45 CHANGES. CHECKPOINT BUMPED, AND NOT FOR A LOGIC CHANGE. READ
# THE TAG NAME BEFORE INFERRING ANYTHING FROM THE BUMP.
#
# THE PROBLEM. The 5 September run restored all thirteen waves from
# cache, which was correct: nothing inside the wave mutate had changed
# since v21_44_na_guard, and the lifetime-parenthood work all runs after
# combination. But a fully cached run leaves .v17_presence$rows and
# .v17_presence$mapabsent empty, so the presence table and the absence
# audit describe the run and not the panel. The 207 empty mnemonics the
# audit is supposed to name cannot be counted from a cached run.
#
# THE DEEPER PROBLEM, WHICH IS THE ACTUAL REASON FOR THIS BATCH. The
# checkpoint hash has read c8d8b52d since v21.34. That is not evidence
# that the caches are sound; it is evidence that the AUTO-INVALIDATING
# half has not fired in ten builds, so every invalidation since v21.34
# has rested entirely on somebody remembering to move the tag by hand.
# This file records four occasions when somebody did not: the ehto
# rebuild, the RAW_COLS_KEEP fix, the v21.31 matrix column, and the
# v21.42 total_own_children NA fix. A cache whose integrity has been
# manual for ten builds and has failed four times is a cache worth
# rebuilding once, deliberately, rather than reasoning about.
#
# SO THE TAG MOVES, AND THE NAME SAYS WHY. v21.35 warns against a tag
# that moves when nothing changed, and that warning stands: there the
# re-extraction bought one reprinted line. Here it buys the absence
# audit and a full cache validation, and the tag is named
# v21_45_forced_cache_validation so that nobody reading the tag list in
# six months infers a derived-column change that did not happen. The
# rule "bump after changing derived-column logic" is a sufficient
# condition for bumping, not a necessary one.
#
# WHAT MAKES IT A VALIDATION RATHER THAN A REFRESH. Nothing has changed
# since the caches were written, so the re-extract MUST reproduce the
# panel. If it does, the caches were honest and nothing downstream is
# invalidated. If it does not, an earlier edit was masked, this build is
# the correct one, and every script from 02 onward has to be re-run. The
# fingerprint block near the panel save writes the invariants and diffs
# them against the previous build, so the question is answered by the
# log rather than by inspection.
#
# BASELINE THE FINGERPRINT FIRST. The comparison needs a fingerprint
# from the OLD build to compare against, so run once with the tag still
# at v21_44_na_guard to write panel_fingerprint.csv, which is cheap
# because it restores from cache, and only then bump.
#
# FORCE_REEXTRACT IS THE OTHER ROUTE TO THE SAME PLACE and is left at
# FALSE. It retires the checkpoint directory by renaming rather than
# moving the tag, which is the right instrument when the re-extract is
# routine. It is not the right instrument here, for two reasons: it
# depends on a directory rename succeeding, which is not a safe bet
# under OneDrive, and it has to be turned off again afterwards, which a
# bump does not.
#
# ONE BUG FIXED WHILE HERE. The absence audit splits stems into absent
# from ALL waves against absent from SOME using .nw_all, the number of
# waves on the combined frame, which is thirteen whatever ran. On a
# PARTIAL re-extract .ab carries rows only for the waves that extracted,
# so the ALL test can never fire and every genuinely absent stem is
# reported as absent from some waves. That is not a weaker audit, it is
# a wrong one. The audit now refuses to classify on a partial run.
#
# =====================================================================
# v21.39 CHANGES. NO REBUILD. RESOLVED, and it was never a data problem.
#
# THE ANSWER. The paper's 21.5% and 25.5% are shares of ALL PERSONS, not
# of men and of women:
#
#   left-censored men     9,831 x 44.2%  =  4,345   /20,205 = 21.51%
#   left-censored women  10,374 x 49.6%  =  5,146   /20,205 = 25.47%
#                                           ------             ------
#                                            9,491             46.97%
#
# The two reproduce the stated figures to the decimal and sum to the
# overall left-censored share. That is conclusive: the denominator is the
# pooled person count, used for both sexes.

# ---------------------------------------------------------------------------
# WHERE THIS SCRIPT WRITES, said once. Every saveRDS and write.csv below used
# a bare filename, so the destination was getwd(): the same call put the file
# in WG when run from the project root and in WG/TG once the scripts moved
# into a subdirectory. That is how two copies of the panel and two of
# train_ledger.csv came to exist a directory apart, neither obviously current,
# and why paper_tables.R needs a four-path search to find one .rds file.
#
# THE PRECEDENCE WAS BACKWARDS AND IT PUT EVERY OUTPUT IN THE WRONG PLACE.
# project_dir is an INPUT root: line 1765 builds data_dir from it as
# file.path(project_dir, "dta_files"), so it points at wherever the raw
# HILDA files are kept. Preferring it here sent the panel to sit beside the
# source data, in Documents - Copy/Y2025/HILDA_project, while the contract
# searches Desktop/WG/TG. The two are unrelated directories.
#
# NOTHING CAUGHT IT FOR AS LONG AS EVERYTHING RAN IN ONE SESSION. The next
# script found the panel in memory and never looked at a file, and where
# handoff/frame.rds survived it found that. It would break the first time
# RUN_ORDER launched a separate process, which reads TD_FRAME_FILES and
# would have got a build from weeks earlier without a word.
#
# PROJ_ROOT FIRST. It is the contract's own root and the one TD_FRAME_FILES
# is built from, so an output written there is an output the rest of the
# pipeline can find. project_dir stays as the fallback for a session that
# has not loaded the contract, which is how this file is often run.
# getwd() SITS ABOVE project_dir, and that is the whole of the fix. Moving
# PROJ_ROOT to the front was not enough: this file is normally run without
# the contract loaded, so PROJ_ROOT does not exist and the chain fell
# straight through to project_dir again. The 14:52 run proved it, reporting
# "resolved through project_dir" and writing to Documents - Copy a second
# time.
#
# getwd() is the right fallback because TD_FRAME_FILES is built over
# c(DIR_DATA, PROJ_ROOT, "."), and "." is the working directory. So an
# output written to getwd() is always one the contract can find, whatever
# else is or is not defined. project_dir cannot make that promise: it is an
# input root, holding dta_files, and it is only in this chain because the
# working directory once happened to be the same place.
.out_path <- function(f) {
  .root <- if (exists("PROJ_ROOT", envir = globalenv()))
             get("PROJ_ROOT", envir = globalenv())
           else getwd()
  file.path(.root, f)
}

#
# THE SENTENCE IS THEREFORE WRONG, and by a factor of two in the
# direction that matters. "They are 21.5% of men and 25.5% of women"
# states sex-specific shares. The sex-specific shares are 44.2% and
# 49.6%. Left-censoring removes NEARLY HALF of each sex, not a quarter.
#
# That is a correction to the prose rather than to any number in the
# pipeline - 2,704 and 7,946 are unaffected - but it is not cosmetic. The
# paper already calls the restriction consequential and characterises it
# rather than leaving it implicit, which is the right instinct; the
# figure quoted in support of that argument understates it by half. A
# referee who recomputes it per sex gets 44.2 and 49.6 and has to decide
# whether the discrepancy is an error or a definition. Both readings are
# printed from here on, labelled, so it cannot recur.
#
#  (1) BOTH DENOMINATORS PRINT, always, with the pooled column named as
#      the one the paper quotes.
#
#  (2) THE THREE SWEEPS ARE RETIRED BEHIND ROSTER_SWEEPS, not deleted.
#      They print about thirty lines answering a closed question, so they
#      are off by default. They are KEPT because each ruled something
#      out, and the eliminations are what made the arithmetic findable:
#      no age cap reconciled the roster with the shares, the wage base
#      moved the share the wrong way, and no parenthood definition came
#      within nine points. Having excluded age, base and definition, the
#      denominator was what was left. Delete them and the next person to
#      doubt the roster re-derives all three from nothing.
#
#  (3) THE PERSON-WAVE READING STAYS ON. The v21.38 run put it at
#      46.5 / 52.9 against the per-person 44.2 / 49.6, higher because
#      left-censored parents contribute more waves each. The two readings
#      differ in a direction worth seeing, and anyone quoting a share off
#      this panel should know which one they are holding.
#
# WHAT THIS COST AND WHAT IT BOUGHT. Three builds of sweeping to find a
# wording error. The sweeps were the right response - the roster matched
# to 0.6% and the age gap to 0.8 years while the shares were off by a
# factor of two, and that pattern genuinely looks like a sample
# definition problem - but the cheaper first move would have been to ask
# what denominator makes 21.5 and 25.5 sum to something meaningful. They
# sum to 47.0, which is the overall rate, and that is visible from the
# two numbers alone without any data at all.
#
# # =====================================================================
# v21.38 CHANGES. NO REBUILD. Fourth build running that the checkpoint
# holds.
#
# THE v21.37 SWEEP FAILED TO MATCH, WHICH IS THE RESULT. It was built to
# be able to fail and it did, and the SHAPE of the failure says more than
# a match would have. Two rows from the run:
#
#   cap NA   roster 10,714   shares 44.2 / 49.6   COUNTS match
#   cap 35   roster  9,269   shares 22.3 / 27.4   SHARES match
#
# The roster is right with NO age restriction, off by 64 in 10,650. The
# shares are right at a cap near 34, where the roster is 1,381 short.
# Both cannot hold. So the design's roster counts and its left-censoring
# shares are not computed on the same population AS EACH OTHER, and no
# further sweeping over age caps will reconcile them. The wage-sample
# base moved the shares the wrong way, as predicted, so it is out too.
#
# THE REMAINING SUSPECT IS ever_parent, and it is a good one. Its
# construction is deliberately maximal: tchad > 0, OR own resident plus
# non-resident children > 0, OR household children under 15 > 0, OR any
# non-resident child, any one sufficient. A sixty-year-old whose children
# left home thirty years ago satisfies it and lands in this loader's
# left-censored bucket.
#
# That is the correct definition of "has ever been a parent" and may be
# the wrong one for "was already a parent in the sense an event study
# means". A narrower one strips out exactly the old cohort: it halves
# the left-censored count while barely touching the roster, because
# transitioners are young and none of this reaches them. That fits BOTH
# observed facts, which no age cap did.
#
#  (1) THE DEFINITION IS NOW SWEPT. Four candidates, the roster rebuilt
#      under each, every one monotonised within person first - ever_parent
#      is the only one the cummax fix upstream touched, and comparing a
#      monotonic flag against three non-monotonic ones would manufacture
#      transitions that are not there. panel_full is the slim frame at
#      this point, so num_children_under15 and total_own_children are
#      already dropped; the survivors are tested and the rest are NAMED
#      as untestable rather than quietly skipped.
#
#  (2) AND THE PERSON-WAVE READING, one line, because "21.5% of men"
#      does not say of what and person-waves is the other plain reading
#      of it. If that table reads 21.5 / 25.5 then the denominator was
#      person-waves all along and the counts were never in conflict.
#
# IF NEITHER MATCHES, the loader has run out of things to test and the
# answer is in the estimation code. What would remain established is
# worth stating on its own: the roster reconstruction is within 0.6% and
# the age gap lands at 13.8 against ~13, so WHO is left-censored is
# right. It is only the reported SHARE that cannot be reproduced from
# the panel, and a share whose denominator is not stated is not yet a
# number a referee can check.
#
# # =====================================================================
# v21.37 CHANGES. NO REBUILD. Third build running that the checkpoint
# holds; the whole batch is post-combination reporting.
#
# THE v21.36 ROSTER CHECK DID ITS JOB AND FOUND SOMETHING. Two of its
# three outputs landed and one did not, and the pattern is informative
# rather than a defect in either place:
#
#   roster     2,710 / 8,004 / 10,714 against 2,704 / 7,946 / 10,650.
#              Within 0.6%. One filter apart, at most.
#   age gap    13.8 years, 39.9 at entry against 26.1, versus the
#              paper's ~13 and about 40 against 27. This is the
#              INDEPENDENT check on the classification and it passes,
#              so who counts as left-censored is right.
#   share      44.2% of men and 49.6% of women left-censored, against
#              21.5% and 25.5%. Almost exactly double.
#
# A WIDER DENOMINATOR CANNOT EXPLAIN IT. The base is already every
# person on the panel: 10,714 + 9,491 = 20,205, which is the "Unique
# individuals" figure the loader prints. So the design must count FEWER
# people as left-censored, and the arithmetic is unforgiving: 23.5% on a
# roster of 10,650 needs about 3,272 left-censored against the 9,491
# observed, so roughly 6,200 of them - two in three - are excluded by
# something this loader does not apply.
#
# AN AGE CAP IS THE OBVIOUS CANDIDATE AND PROBABLY NOT SUFFICIENT.
# Left-censored parents enter at 39.9 and transitioners at 26.1, so an
# upper bound removes the first heavily and the second barely, which
# fits a roster that is only 64 out. But removing two thirds of the
# left-censored needs a cap tight enough to start eating transitioners,
# and then the roster stops matching.
#
# SO IT IS SWEPT RATHER THAN ASSUMED, and the sweep is built to be able
# to FAIL. Caps run down to 30, past the point where the roster
# survives, so the table brackets the answer instead of stopping just
# short of it; a sweep that cannot fail is not a test. The wage sample
# is swept as a second base, and it is worth printing precisely because
# it moves the share the WRONG way: a smaller, more attached base raises
# the left-censored share rather than halving it, so if that is the
# design's base the discrepancy is larger than it looks.
#
# IF NO ROW MATCHES BOTH the counts and the shares - which the
# arithmetic above says is likely - then the two populations differ by
# something that is neither age nor the wage screen, and the sentence in
# the paper needs its denominator stated before the number means
# anything. Two candidates this loader cannot test: the share may be of
# PERSON-WAVES rather than persons, or of a starting sample that the
# estimation stage narrows before counting. Both are answerable from the
# estimation code and neither is answerable from here.
#
# THE DENSITY VERDICT IS UNAFFECTED. 56 of the 108 child-care positives
# fall on a roster-treated person, 0.021 per treated against a 0.05
# threshold, so employer child care remains far too thin for an event
# study whichever base is correct.
#
# # =====================================================================
# v21.36 CHANGES. NO REBUILD, for the second build running: the whole of
# this batch is a post-combination diagnostic and CHECKPOINT_TAG stays at
# v21_34_gate_no_coding_assumption. The v21.35 run confirmed the hold
# works, restoring all thirteen waves from that tag.
#
#  (1) THE v21.35 DENSITY DENOMINATOR WAS THE WRONG POPULATION, and it
#      was the third instance in three builds of one defect: a number on
#      one population printed beside a number on another with nothing
#      saying so. v21.33 had 43% against 74.8% coverage; v21.34 fixed
#      that; v21.35 introduced 4,332 treated persons against the 2,704
#      the file's own comments had named since v21.33. This one was mine
#      and it was in the fix for the previous one.
#
#      4,332 is every person with a birth in-panel. The event study does
#      not run on them. It runs on persons whose FIRST birth falls
#      inside the panel: 2,704 treated and 7,946 never-treated, roster
#      10,650. Individuals already parents when first observed are
#      left-censored and dropped, their event time being undefined. The
#      ~1,600 in the gap have an in-panel birth that is not their first.
#
#      So a design-feasibility test was computed on a population the
#      design does not use. Both ends now move: the denominator is
#      roster-treated, and the numerator counts only positives falling
#      on a roster-treated person, because a positive on a left-censored
#      parent contributes nothing to a study it is not in. The verdict
#      was never in doubt - 0.007 per treated person against a 0.05
#      threshold - but a feasibility number that is right by luck is not
#      a feasibility number.
#
#  (2) THE ROSTER IS RECONSTRUCTED AND PRINTED, in its own diagnostic
#      rather than inside the child-care block where it was first
#      needed. A roster check that vanishes whenever nbcmcca fails to
#      resolve is a check on the wrong thing.
#
#      It is PRINTED so it can be checked, not trusted. Left-censored is
#      ever_parent already 1 at the person's first observed wave;
#      treated is ever_parent turning on later; the cummax fix upstream
#      makes first and last sufficient. If the counts do not read
#      2,704 / 7,946 / 10,650 then this loader and the estimation stage
#      disagree about who is in the sample, which is a larger problem
#      than any density and would otherwise surface as an unexplained
#      difference in N three scripts downstream.
#
#  (3) THE LEFT-CENSORING CHARACTERISATION IS NOW MEASURED. The paper
#      states that left-censored parents are 21.5% of men and 25.5% of
#      women and enter about thirteen years older, near 40 against 27.
#      Four quantitative claims that nothing in the pipeline checked.
#      They are computed from the roster object and printed beside the
#      stated values.
#
#      EXPECT THE SEX SHARES TO NEED A DENOMINATOR CHECK on the first
#      run. This reconstruction takes persons with non-missing
#      ever_parent as the base; if the paper used another, the shares
#      will differ while the roster counts still match. That is a
#      reconciliation, not a defect, and it is exactly the ambiguity
#      that produced defects (1) and the two before it.
#
#      The age gap is the one that bears on external validity: the
#      design identifies the penalty for women whose first birth falls
#      while they are attached to the panel, which skews to later first
#      births than the population. That is a SCOPE CONDITION, not a
#      robustness check. No reweighting recovers a population whose
#      event time is undefined.
#
# # =====================================================================
# v21.35 CHANGES. NO REBUILD. The checkpoint tag deliberately does NOT
# move: one comment and one post-combination diagnostic change, nothing
# inside the wave mutate, so every cached wave is still correct. This is
# the first build in this batch where leaving the tag alone is the right
# call rather than the lazy one, and the reason is written at the
# constant so the next reader can tell the two apart.
#
# BOTH ITEMS ARE DEFECTS IN THE v21.32 DIAGNOSTICS, found by reading the
# v21.34 build log rather than the source. Neither would have surfaced
# any other way, which is the argument for reading logs.
#
#  (1) A THRESHOLD CHOSEN BY EYE WENT SILENT BY EIGHT. The employer
#      child-care check read `if (positives < 100)` and the build
#      printed "positive on 108 person-wave(s) of 109 observed", so the
#      warning did not fire. 108 person-waves over thirteen waves
#      against 2,704 treated persons cannot support an event study by
#      any margin at all, and the diagnostic went quiet on exactly the
#      case it exists to catch.
#
#      The test is now a DENSITY against the treated-person count,
#      which is the quantity that governs whether a design can run,
#      rather than a bare count that has to be re-guessed whenever the
#      panel changes size. Both numbers print, so the ratio is
#      checkable rather than trusted.
#
#      The wording also now separates "too thin for an event study"
#      from "too thin to report". Employer-provided child care being
#      close to non-existent in Australia across this window is a clean
#      null and a finding; the old text risked reading as a reason to
#      drop it.
#
#  (2) A COMMENT THAT WOULD HAVE SENT THE NEXT READER HUNTING. The gate
#      note gave nbcmany coverage as "about 43% of enumerated persons"
#      while the gate itself reports 74.8%. Both are right - 43% is the
#      raw .dta denominator, 74.8% is the restricted analysis panel -
#      but placed next to a gate the 43% reads as a prediction of its
#      output, and reconciling the two means looking for a bug in
#      something that works. Both denominators are now named, along
#      with the 72.7-78.8% across-wave bracket from the coding audit,
#      which is the figure to check the gate against.
#
#  WHAT THE v21.34 LOG SETTLED, recorded here because it closes the
#  question the last three builds were circling and the answer is not
#  in the source anywhere else:
#
#    noncash_any_raw        code set(s): 1/2   coverage 72.7-78.8%
#    employer_childcare_raw code set(s): 0/1   coverage 15.6-20.2%
#
#  So the `> 0` term v21.34 removed from the gate was INERT: nbcmany is
#  1/2 and no v21.33 output was wrong. The removal stands anyway, and
#  the second line is why. Two binaries, one batch, one module, two
#  DIFFERENT code sets. The assumption that HILDA yes/no items are 1/2
#  is false inside the very block that made it, and which of the two
#  mnemonics the gate happened to be built on was luck. The audit also
#  reports both consistent across all thirteen waves, which is the
#  other way it could have broken.
#
#  AND THE DIRECTION TEST CAME BACK, on 57,647 person-waves with both
#  measures observed: ln wage gap 0.1331, ln compensation gap 0.1469, a
#  difference of +0.0137 or 10.3% of the wage gap. Mothers' non-cash pay
#  falls 15.8% between k = -1 and k = 0 while fathers' is flat. That is
#  the WIDER branch, not the footnote branch: the compensation arm
#  belongs in the text with both series reported, and the fall at k = 0
#  is additive to the measured penalty rather than a correction to it.
#
# # =====================================================================
# v21.34 CHANGES. FULL REBUILD REQUIRED, and the map does NOT force it.
# Nine mnemonics forced the v21.32 rebuild by moving the hash; nothing
# here touches the map, so the hash is unmoved and only the hand bump of
# CHECKPOINT_TAG to v21_34_gate_no_coding_assumption invalidates the
# cache. Without it a restored wave keeps the old gate under the new
# build tag, which is the worst of the three outcomes: wrong values,
# right label.
#
#  (1) THE v21.33 GATE CARRIED A CODING ASSUMPTION. It read
#
#        noncash_module_asked = as.integer(!is.na(noncash_any_raw) &
#                                            noncash_any_raw > 0)
#
#      and the `> 0` asserts that a substantive answer to F10 is a
#      POSITIVE code. That holds under 1 = yes / 2 = no and fails under
#      0 = no / 1 = yes. The recode nine lines above it declines to make
#      that assumption, carrying `noncash_any_raw == 0 ~ 0L` because the
#      coding was never established, so one mutate held two readings of
#      one column seven lines apart.
#
#      The term is removed. !is.na() alone is the correct test and needs
#      no assumption: safe_numeric has already mapped -10..-1 to NA, so
#      non-missing means a substantive answer whatever integer the
#      release chose. It is also the form the other two gates in this
#      file use. training_module_asked and aim_battery_asked are pure
#      !is.na() disjunctions; the non-cash gate was the only one of the
#      three with a magnitude test in it, and being the odd one out was
#      the visible symptom nobody read.
#
#      WHAT IT WOULD HAVE COST if the coding is 0/1: four fifths of
#      genuine no answers gated out as never-asked, noncash_any_value_z
#      identically 1 by construction, noncash_benefits_weekly_z a
#      recipients-only mean. A synthetic run with a true asked-population
#      mean of $11.26 produced $56.29. Every one of those numbers looks
#      like a finding.
#
#      WHETHER IT WAS LIVE is answered by the v21.33 build log, not by
#      reading this file. The diagnostic prints "module asked: X%" and
#      the gate comment records nbcmany as non-missing on about 43% of
#      enumerated persons. X near 43 means the coding is 1/2 and the
#      `> 0` was inert; X near 10 means it was 0/1 and every _z column
#      on the v21.33 panel is wrong. Check it, because it also tells you
#      whether any v21.33 output needs discarding rather than just
#      rebuilding.
#
#  (2) THE GATE JOINS THE CONSTANT-COLUMN WATCH LIST, where
#      training_module_asked already sat for the same reason. A gate
#      cannot be all-NA, so its failure mode is a CONSTANT and nothing
#      else. An unresolved nbcmany makes this column 0 everywhere and
#      takes all three _z twins to all-NA with it, and none of the four
#      is in EXPECTED_NEW_COLUMNS, so the arrival contract would not
#      have stopped that either. Constant at 1 is the opposite failure
#      and equally worth naming: the gate admits everyone, the _z
#      columns are identical to the ungated ones, and the suffix says
#      they are not.
#
#  NO COLUMNS ARE ADDED OR REMOVED and no compat guard reads any of the
#  affected names, so COMPAT_REVIEWED_TO in 00compat.R moves to "v21.34"
#  on the strength of that check rather than on the tag alone. The
#  ordering there is numeric, so no other edit is needed.
#
#  STILL OUTSTANDING, and deliberately not done here: v21.33 has no
#  changelog block of its own. Its three substantive changes - the
#  pre-imputed columns keeping their missing values, the gate, and the
#  employer child-care null - are documented only at their sites.
#
# # =====================================================================
# v21.32 CHANGES. FULL REBUILD REQUIRED, and for both reasons at once:
# the variable map gains nine mnemonics, which invalidates the checkpoint
# hash by itself, and derived columns change inside the wave mutate, so
# CHECKPOINT_TAG is bumped by hand to v21_32_noncash_benefits as well.
# Relying on the map hash alone is what let the v21.31 matrix-column bug
# survive its own fix, and that is three times in one session.
#
#   [SUPERSEDED at v21.34. This paragraph told the reader to add
#   "v21.32" to COMPAT_PANEL_TAGS. That constant no longer exists: the
#   v21.34 rewrite of 00compat.R replaced the hand-extended whitelist
#   with COMPAT_MIN_PANEL_TAG / COMPAT_REVIEWED_TO and a numeric tag
#   ordering, precisely because the list was never extended and had
#   fallen eleven builds behind. Move COMPAT_REVIEWED_TO instead, and
#   only after reading the changelog against the guards.]
#
# THIS IS STAGE 6 OF REVISION_PLAN.md, TAKEN UP. That stage recorded the
# omission as a decision rather than an oversight and deferred it on the
# grounds that a fourth loader batch would sit between the current panel
# and the Stage 1 decisions. It is done here as ONE batch for the same
# reason v21.12 was: the cache invalidates on the map, so a second pass
# would cost thirteen more waves for nothing.
#
#  (1) THE THIRD COMPONENT OF COMPENSATION IS NOW READ. wscei is wages
#      and salary. Business income was documented at v7 and salary
#      sacrifice fixed at v21.23; employer non-cash benefits were read
#      nowhere at all. nbcm is the DV weekly value in the CURRENT MAIN
#      JOB - motor vehicle, telephone, computer, car park, low-interest
#      loans, shares, superannuation, housing rent, child care - with
#      nbcmi imputed, nbcmf the imputation flag and nbcmany the has-any
#      indicator, in waves 10 to 24. W12-W24 sits entirely inside that.
#
#      WHY THE DIRECTION MATTERS, and it is the same direction as the
#      sacrifice bias v21.23 corrected. Motor vehicle, car park,
#      telephone and computer concentrate in higher-status jobs, which
#      is where fathers are. Omitting them plausibly UNDERSTATES the
#      father-mother gap. The size is unknown and may be small; the
#      point until now was that nothing measured it.
#
#      weekly_compensation is built on weekly_earnings_inc_ss, not on
#      weekly_earnings, because wsces already carries the sacrifice and
#      adding non-cash to wscei would leave one of the three components
#      out of a column named for the total. weekly_earnings is UNTOUCHED
#      and every published number still rests on it.
#
#  (2) THE MAIN-JOB / ALL-JOBS MISMATCH, WHICH THE PLAN'S SIX MNEMONICS
#      DO NOT CLOSE. wscei is ALL JOBS and nbcm is the MAIN JOB, so
#      wscei + nbcmi is not a compensation total on one base. nbco is
#      the other-jobs counterpart, so nbco/nbcoi/nbcof are read as well
#      and weekly_compensation uses the sum. noncash_benefits_weekly
#      keeps the main job alone for anyone who wants the narrower base,
#      and the restriction manifest names the choice.
#
#  (3) EMPLOYER CHILD CARE IS THE ONE ITEM DIRECTLY ON TOPIC. nbcmcca
#      and nbcmccr are amount and receipt. That is a labour-supply
#      constraint measured at the individual level and it should move
#      around a birth if anything in this batch does. It is also where
#      the cash valuation is weakest: the dollar figure understates the
#      effect on the recipient's constraint set, which is a limitation
#      to state rather than a correction to apply here.
#
#  (4) THE CODING IS DETERMINED, NOT ASSUMED, and twice over. nbcmany
#      and nbcmccr get _raw twins and join the v21.12 binary audit, so
#      the == 1 rule stops the build if it is wrong for this release or
#      changes across waves. AND noncash_any_value is derived from the
#      DV itself rather than from the indicator, so the has-any margin
#      survives even if nbcmany is absent or coded some third way. The
#      diagnostic cross-tabulates the two. alop aggregated five leave
#      types and bnfppl produced four builds of wrong conclusions, both
#      because a label was read as a description; the six nbcm stems are
#      in the per-wave presence report for the same reason.
#
#  (5) THE DIRECTION IS TESTED, NOT ASSERTED. The diagnostic after the
#      salary-sacrifice block reports the non-cash share of compensation
#      by sex, its movement over event time, and the raw father-mother
#      gap in ln hourly wage against the same gap in ln hourly
#      compensation. If the second gap is WIDER, the omission was
#      understating the penalty and the arm belongs in the text; if the
#      two agree, this is a footnote. Either answer is reportable and
#      the loader does not decide which.
#
#  EXPECTED_NEW_COLUMNS IS FILLED IN for this build: eight columns that
#  must arrive populated. The conditional ones are left out on the
#  v21.12 rule - the contract stops on all-NA, so declaring a column
#  that may legitimately be empty turns a finding into a build failure.
#  Clear the constant once the build passes.
#
# =====================================================================
# v21.11 CHANGES. THIS BUILD FORCED A REBUILD, and the note that said it
# would not was wrong. Change (3) adds a term to the checkpoint hash. A
# hash is a function of its INPUT STRING, so adding "dropraw=FALSE"
# changed the hash even though the switch is off, and all thirteen waves
# re-extracted. The cost is one-off and already paid: the hash is stable
# at the new value, and flipping the switch will invalidate it again,
# which is exactly what it is there for. Left as is rather than reverted,
# because reverting would change the hash a third time and a hash that
# depends on whether a term is PRESENT rather than what it EQUALS is
# harder to reason about than one extra rebuild.
#
# COMPAT_PANEL_TAGS in 00_compat_v21.R needs "v21.11".
#
#  (1) THE ARRIVAL CONTRACT, EXPECTED_NEW_COLUMNS. 01's Part 0c documents
#      six edit sites for one added variable and notes that sites 2 to 6
#      are SILENT: a mnemonic added to the map and left out of the final
#      select() triggers a thirteen-wave rebuild and then does not appear,
#      with no error, because any_of() tolerates the absence. The header
#      of this file records that having happened.
#
#      Declare the columns you expect and the build stops if any is
#      absent or all-NA, before the saves. One edit covers sites 3, 4
#      and 5. This exists because PAPER_ATTENTION.md section 8 asks for
#      six new variables in one batch, which is six chances to hit the
#      silent path.
#
#  (2) THE CONSTANT TEST NO LONGER COPIES EVERY COLUMN. The degenerate
#      sweep tested length(unique(x[!is.na(x)])) <= 1 on every column,
#      and that subset allocates up to 134,131 values per column: 8 ms of
#      the 10 ms the test cost, on 2,927 columns. range(x, na.rm = TRUE)
#      answers the same question in one pass with no allocation. Measured
#      12.0 s to 5.5 s, and to 0.6 s with (3) enabled.
#
#      Verified against the old test on fourteen cases including all-NA,
#      NaN, integer, logical, character and both Inf forms. The first
#      attempt carried an is.finite() guard that made a column constant
#      at Inf read as varying; there is no guard now, and an all-NA
#      column returns range c(Inf, -Inf), unequal, so it reads FALSE
#      without a special case.
#
#  (3) DROP_WAVE_PREFIXED_RAWS, DEFAULT FALSE. Each wave frame carries
#      about 200 wave-prefixed mnemonics that the mutates have already
#      consumed and that the final select drops. They account for 2,606
#      of combined_data's 2,927 columns and are carried through the
#      checkpoint write, the combination, both sweeps, the restriction
#      block and the select.
#
#      The drop is exact rather than pattern-matched: unlist(v) IS the
#      set of names the map read, so the drop is that set less
#      RAW_COLS_KEEP, guarded by the same stop() the _raw drop uses.
#
#      IT IS A SWITCH BECAUSE IT CHANGES WHAT A CHECKPOINT HOLDS. A
#      directory of some 497-column and some 297-column checkpoints
#      would combine into a frame whose columns depend on when each wave
#      was written, and nothing downstream would notice. So the switch is
#      IN THE CHECKPOINT HASH: turning it on invalidates all thirteen
#      caches and forces a uniform re-extract. Turn it on with the
#      section 8 batch, which needs a rebuild anyway.
#
# =====================================================================
# v21.10 CHANGES. NO REBUILD. CHECKPOINT_TAG stays at
# v21_9_dosage_zeros_business_rename, so the thirteen waves the v21.9 run
# just wrote are restored from cache. One downstream edit:
# COMPAT_PANEL_TAGS in 00_compat_v21.R needs "v21.10".
#
#   ONE FIX, TO A BUG IN THE v21.7 SOURCE-COLUMN CLASSIFIER. The guard
#   "no HILDA mnemonic carries an underscore" is false. hhd0_4 and hhd5_9
#   are the household counts of children aged 0-4 and 5-9, so all 26 of
#   their wave-prefixed copies escaped classification on the v21.9 run:
#   22 sat in the wave-hole sweep's unexplained list and 4 were absorbed
#   by the single-wave shape entries, which is why "wave 23 only" and
#   "wave 24 only" reported 2 each instead of 0.
#
#   The guard now applies only to the PREFIX-ONLY arm. Where the sibling
#   test fires, a stem under two or more different wave letters is one
#   source variable under many names and the guard is not needed. Expect
#   source columns to rise from 2,606 to 2,632, the unexplained list to
#   fall from 24 to 2, and those 2 to be the real finding: job_distance_km
#   and job_distance_cat, populated waves 17 to 24 because jbmlkr arrives
#   at wave 17. That is the first genuine defect this sweep has surfaced.
#
# =====================================================================
# v21.9 CHANGES. THIS BUILD REQUIRED A FULL REBUILD. Unlike v21.7 and
# v21.8, this one changes DERIVED COLUMNS, so CHECKPOINT_TAG is bumped to
# v21_9_dosage_zeros_business_rename and the thirteen wave caches are
# invalidated. Delete wave_checkpoints/ or let the tag mismatch do it.
#
#   TWO DOWNSTREAM EDITS, NEITHER OPTIONAL:
#   (a) COMPAT_PANEL_TAGS in 00_compat_v21.R needs "v21.9".
#   (b) Anything reading earnings_total_has_business must move to one of
#       the two columns that replace it. The deprecation stop will name
#       the call sites; there were none in 00compat, 01, 02 or 03.
#
#   THE COLUMN COUNT CHANGES. 313 becomes 315: two columns added, one
#   renamed. The fingerprint blocks in 02 and 03 print the count and
#   match on rows and persons, so they will report 315 without failing.
#
#  (1) total_training_hours_z. THE DOSAGE MARGIN IS NO LONGER SELECTED
#      ON THE OUTCOME. total_training_hours is jttdays x jtthrs and both
#      items are put only to people who reported training, so it is NA
#      for everyone who trained none. It is therefore observed on about
#      a third of the employed, and observed EXACTLY WHERE THE DOSAGE IS
#      POSITIVE, which makes any regression on it a regression
#      conditional on training.
#
#      03_training_designs.R had been recoding this itself as
#      train_hours_z and recovering coverage from 32.5% to 99.8%. That is
#      the right recode in the wrong place: one definition per script is
#      how two scripts come to disagree about one variable.
#
#      ONE DELIBERATE DIFFERENCE FROM THE 03 VERSION. 03 keyed on
#      any_training == 0, which is itself coalesced, so a NEVER-ASKED
#      person-wave read as zero training and therefore zero HOURS,
#      pushing the T1 structural-zero problem into the dosage margin
#      undeclared. Here the never-asked are NA. The four states are:
#      never asked NA, asked and did not train 0, asked and trained the
#      product, asked and trained with the items missing NA. Every zero
#      in the new column is a MEASURED zero.
#
#  (2) earnings_total_has_business RENAMED, AND THE v21.8 DIAGNOSIS OF IT
#      WAS WRONG. v21.8 called it a defect because it is constant at 1
#      while only 8.3% of person-waves have non-zero business income.
#      It is constant, and correctly: it was built to record whether
#      fy_earnings_total INCLUDES the business component or fell back to
#      wages alone, the businc mnemonics resolve in every wave on this
#      window, and so the fallback never fired. The column reported that
#      nothing went wrong and was read as reporting something else.
#
#      The fault was the NAME, which reads as a fact about the person and
#      states a fact about the measure. Two columns now:
#        earnings_total_includes_business  the provenance flag, unchanged
#                                         in value, 1 on this window
#        fy_business_income_nonzero       the substantive test, about
#                                         8.3%, counting a LOSS as a
#                                         business because a loss is one
#      Neither substitutes for had_business_lfy (bifhave, 16.1%) or
#      is_self_employed (esempdt, 12.6%): three reference periods, three
#      questions.
#
#  (3) EXPECTED_CONSTANT_COLS, the companion to EXPECTED_EMPTY_COLS.
#      Nothing exempted the columns that are constant BY DESIGN, so all
#      four arrived in the v21.8 warning as defects when each one's
#      constant value is the result it exists to report: the business
#      provenance flag at 1, earnings_source_flag at
#      inc_salary_sacrifice because wsfes predates this window, and the
#      two *_q_declined_dated flags at 0, which is the proof the v18
#      double-coding bug stays fixed. They are still scanned and still
#      printed; they leave the warning count only. A passed test reported
#      as a defect is how a warning stops being read.
#
# =====================================================================
# v21.8 CHANGES. NO REBUILD, for the same reason as v21.7: no map entry,
# no derived column, no change to the final select, so the checkpoint
# hash is untouched and CHECKPOINT_TAG stays at v21_6_training_gate_w12_24.
#
#   COMPAT_PANEL_TAGS in 00_compat_v21.R needs "v21.8". If you have not
#   yet made the v21.7 edit, add both strings at once.
#
# This build exists because v21.7 asserted something it did not measure,
# which is the exact habit the deferred-coding-verification block was
# added to break, committed twenty lines after that block runs. The
# v21.7 console said the non-employed "truly received no employer-funded
# training, so the zero is right". No column on the panel measured that.
#
#  (1) rate_any_non AND rate_obs_non. The v21.7 table had bases for who
#      was ASKED on both sides of the employment line and bases for what
#      they ANSWERED on the employed side only. The missing pair is the
#      one that settles the question, because jttpewt is a PAST TWELVE
#      MONTHS item while employed is a status AT INTERVIEW: someone who
#      left a job in March can truthfully report workplace training in
#      the same wave, so a non-employed person-wave can legitimately
#      carry a 1. rate_obs_non measures how often. The gap between
#      rate_any_non and rate_obs_non IS the imputation.
#
#      The verdict prose now BRANCHES on that number against
#      TRAINING_STRUCTURAL_ZERO_MAX instead of asserting it, and when it
#      is exceeded it reports the implied understatement as a count:
#      never-asked non-employed times the measured rate. The warning and
#      structural_zero_note carry the same number, so a downstream script
#      re-derives the sentence rather than trusting it.
#
#      Twelve columns do not fit a console line, so the table PRINTS in
#      two blocks, who was asked and what they answered, and WRITES as
#      one CSV, v218_training_module_coverage.csv.
#
#  (2) n_asked_non AND e6_rate_non in the E6 per-wave table. On the
#      v21.7 run the non-employed E6 rate could only be backed out of
#      e6_rate and e6_rate_emp through three-decimal rounding. It is the
#      rate for the group the structural-zero question turns on, so it
#      is printed.
#
#  (3) THE MANIFEST RULE TEXT WAS WRONG. The training_gate row read
#      "_any reads never-asked as a zero; _obs is the gated column",
#      which reads as an instruction to prefer _obs, while the console
#      verdict and the warning both say an unconditional design must not
#      substitute it. It now reads "_any imputes a zero where never
#      asked; _obs DROPS those rows". Eight restrictions at that point;
#      v21.14e makes it ten.
#
# =====================================================================
# v21.7 CHANGES. THIS BUILD DOES NOT REQUIRE A REBUILD. Nothing here
# touches the variable map, a derived column, or the final select, so
# the checkpoint hash is unchanged, CHECKPOINT_TAG is DELIBERATELY NOT
# bumped, and a rerun restores all thirteen waves from cache. The saved
# panels are identical to v21.6 except for the build_tag attribute and
# an enlarged training_gate_facts attribute. Every change below is a
# REPORT: the loader says more about what it already built.
#
#   THE ONE DOWNSTREAM EDIT: COMPAT_PANEL_TAGS in 00_compat_v21.R must
#   gain "v21.7". Without it the guard rejects a panel it should accept.
#
#  (1) THE E6 CONSISTENCY CHECK, which v21.6 should have carried and
#      did not. jttrwrk (E6, "any work related training in the past 12
#      months") is the gate item and the place-and-time items are asked
#      after it, so the containment is logical: if any place item is
#      mentioned, E6 must be 1. A person-wave with E6 = 0 and
#      jttpewt = 1 is not a rare answer, it is two answers that cannot
#      both be right, and until now nothing looked. The new block
#      cross-tabulates E6 against the four place-and-time items on the
#      asked person-waves, names the impossible cell, and warns above
#      TRAINING_E6_CONTRADICTION_MAX. It reads the five _raw columns
#      rather than the derived pair, so the coalesce cannot launder a
#      disagreement into agreement.
#
#      The same block prints the E6 base rate among the employed, which
#      is the denominator that makes the 22.3% employer-funded figure
#      readable. A training rate of 22.3% against an all-training rate
#      near 34% is a share of trainees; against an unstated denominator
#      it is a number.
#
#  (2) THE COVERAGE TABLE NOW NAMES ITS BASES. Under v21.6 asked_all
#      and gate_gap were over everyone while rate_any and rate_obs were
#      employed-only, the header explained the first pair and not the
#      second, and adjacent columns therefore counted different people
#      with nothing on the page to say so. Every column now carries its
#      base in its own name: n_all, n_emp, asked_all, asked_emp,
#      asked_non, gap_all, gap_emp, rate_any_emp, rate_obs_emp.
#
#      gap_emp is the point of the change. asked_emp prints 100.0 in
#      every wave and there are still employed person-waves the module
#      was never put to; the count survives the rounding where the
#      percentage does not, and it is the only part of the gate that
#      lands inside an employed-only estimation sample.
#
#      asked_non is new and is the number the unconditional design
#      turns on. If the module is put to only a third of the
#      non-employed, then an unconditional training series is a mixture
#      of measured answers and assumed zeros whose proportions move
#      with employment, so a mother observed employed before a birth
#      and out of work after it has her outcome switch from measured to
#      assumed at exactly the wave the event study is reading. That is
#      a measurement fact, and it is now on the table per wave rather
#      than in a footnote.
#
# =====================================================================
# v21.12 CHANGES. FULL REBUILD REQUIRED, and deliberately so: the
# variable map gains fifteen mnemonics, which invalidates the hash by
# itself, and CHECKPOINT_TAG is bumped to v21_12_ppl_and_occupation_origin
# because derived columns change too.
#
#   COMPAT_PANEL_TAGS in 00_compat_v21.R needs "v21.12".
#
# ONE BATCH, because the cache invalidates on the map and a second pass
# would cost thirteen more waves for nothing. DROP_WAVE_PREFIXED_RAWS is
# turned on in the same run for the same reason.
#
#  (1) IS PAID PARENTAL LEAVE ALREADY INSIDE THE EARNINGS MEASURE?
#      wscplre and wsfplre record whether PPL was INCLUDED in the wages
#      and financial-year income the loader already reads. This is not a
#      covariate: if PPL sits inside wscei or wsfes for some mothers and
#      outside for others, then earnings at k = 0 is not one quantity
#      across mothers and part of the birth-wave coefficient is a
#      reporting convention. It is the reason the batch is not deferred.
#
#  (2) LEAVE AGAINST SEPARATION. Entitlement (jowppml, jowpppl), receipt
#      with amount and weeks (bncppl*, bnfppl*), and days of paid family
#      leave (alop). A mother on paid leave and still attached to her
#      employer is a different observation from one who separated, and
#      the panel could not tell them apart. This is what turns T1's
#      employed-or-on-leave estimand from a proxy on nlf_reason_maternity
#      into a measured one.
#
#  (3) THE ORIGIN OCCUPATION OF A MOVE. pjoto62/6s via CPQ, ujljo62/6s
#      via NPQ, pjoo62/6s for job-changers. The per-source columns are
#      kept beside the coalesced pair and occ_origin_source records which
#      questionnaire answered, so a design can use one routing alone.
#      All are conditional on a transition and the wave-22 coding shift
#      forces an occupation-by-wave interaction on any of them.
#
#  (4) THE SIX NEW BINARIES GET A CODING AUDIT ON THEIR FIRST BUILD.
#      Their 0/1 versions use the == 1 rule, which is an ASSUMPTION about
#      this release. The audit after combination reads the codes per wave
#      and STOPS on anything outside 0/1/2 or on a code set that changes
#      across waves. The training block earned that discipline over six
#      revisions of asserting; these get it immediately.
#
#      They are NA-PROPAGATING, not coalesced. A missing leave answer is
#      missing. Reading it as "no leave" would repeat the jttpewt_any
#      structural-zero mistake in a new place.
#
#  (5) THE PARTNER LINK IS MEASURED. T8 found partner_id is not perfectly
#      reciprocal and reported it inside a design block. It is a property
#      of the release, so it now sits beside the superannuation partner
#      check and lands in .v21_facts$partner_link.
#
#  EXPECTED_NEW_COLUMNS IS FILLED IN for this build: thirteen columns
#  that must arrive populated. The eight left out are conditional on a
#  payment or on a transition and may legitimately be empty, and the
#  contract stops on all-NA, so declaring them would turn a finding into
#  a build failure. Clear the constant once the build passes.
#
# =====================================================================
#      The written artefact is renamed to v217_training_module_coverage
#      .csv, because the v216 file has different columns under names
#      that no longer mean the same thing.
#
#  (3) THE TWO PANEL SWEEPS NOW SEPARATE SOURCE COLUMNS FROM DERIVED
#      ONES. The degenerate sweep and the wave-hole sweep scan every
#      column on combined_data, roughly 2,900 of them, and about 2,200
#      are wave-prefixed raw remnants: ledcqen, medcqen and so on, the
#      same variable under thirteen names, each populated in the one
#      wave whose letter it carries. They are single-wave BY
#      CONSTRUCTION, they are all dropped at the final select, and they
#      were being reported as unexplained wave holes. A sweep that
#      returns 2,227 unexplained columns is not read, so the defects it
#      exists to catch were arriving at a signal ratio near 1 in 2,000.
#
#      Both sweeps now classify each column with the same data-driven
#      test, count the remnants separately, and exclude them from the
#      unexplained list and from the warning. The nine wave-23 raws
#      sitting at the HILDA missing code, which reach the CONSTANT list
#      because they bypass safe_numeric, are classified the same way.
#      NOTHING IS DROPPED OR RENAMED: the same columns are scanned, the
#      same panel is written, the residual is simply readable.
#
#      The per-wave presence matrix is computed ONCE and shared by both
#      sweeps rather than built twice, which is also why the degenerate
#      sweep no longer walks every column to count its observations.
#
# =====================================================================
# v21.6 CHANGES. THIS BUILD REQUIRES A FULL REBUILD. The variable map
# gains jttrwrk, so the hash invalidates every checkpoint by itself, and
# CHECKPOINT_TAG is bumped as well rather than relying on that.
#
# Everything here is ADDITIVE under the Part 0 scope rule. No existing
# derived column changes value. jttpewt_any and jttpeot_any are byte
# identical to v21.5, so 03_training_designs.R and the published
# training event study reproduce exactly; the new columns sit beside
# them and the comparison between the two is an exhibit rather than a
# replacement.
#
#  (1) THE TRAINING GATE. jttpewt_any and jttpeot_any are built as
#      ifelse(coalesce(jttpewt_raw, 0) == 1, 1, 0), so they are NEVER
#      NA. Three separate states arrive as the same zero: asked and did
#      not train, asked and trained somewhere else, and never asked at
#      all. The third is the one that matters, because it is not a
#      behavioural zero and it is not distributed at random: the module
#      is put to the employed, so the non-employed carry a structural
#      zero and the training event study reads a composition change at
#      the birth wave as a fall in training.
#
#      Downstream this had a second effect nobody could see. The module
#      coverage check in 03 is pct_obs = 100 * mean(!is.na(jttpewt_any)),
#      which is 100.0 in every wave BY CONSTRUCTION, so the arm of that
#      guard testing for an unasked wave can never fire and only the
#      rate == 0 arm is live. Every !is.na(jttpewt_any) filter in T2, T3
#      and the base-rate blocks is inert for the same reason and reads
#      as non-response handling that is not happening.
#
#      E6, jttrwrk, "taken part in any work related training in past 12
#      months", is the gate item: it is asked of everyone the module is
#      put to, and the place-and-time items are asked only after it.
#      01_discovery_extended.R found it in all thirteen waves and the
#      map did not read it. It is now read, and four columns are built
#      on it:
#
#        training_module_asked   1 if the module was put to this person
#                                in this wave, from jttrwrk being
#                                answered, with the other seven training
#                                items as a fallback. Coding-independent:
#                                it tests non-missingness, not a code.
#        training_any_reported   E6 itself, NA-propagating.
#        jttpewt_obs             jttpewt_any where the module was asked,
#        jttpeot_obs             NA where it was not. THE COLUMNS TO
#                                USE. A design that wants the structural
#                                zero can still have it by reading the
#                                _any pair and saying so.
#        training_own_time_obs   the same treatment for the two pooled
#        training_paid_time_obs  place indicators.
#
#      Nothing is netted out of the _any pair. That would be a mutating
#      change and would forfeit the reproduction check.
#
#  (2) THE DEFERRED CODING VERIFICATION NOW RUNS. The v5 comment beside
#      the training indicators said "verify with table(jttpewt_raw) on
#      the first run" and the verification was never written, so the ==
#      1 rule has been asserted rather than checked for six revisions.
#      The post-combination block now prints the observed code set for
#      all five training items in every wave, stops if a code outside
#      0/1/2 appears, and stops if an item is 0/1-coded in one wave and
#      1/2-coded in another, which is the case that would silently make
#      == 1 mean two different things inside one pooled series. This is
#      the same discipline the v21.3 study-item block already applies.
#
#  (3) fulltime IS 0 FOR THE NON-EMPLOYED, and the name does not say so.
#      esdtl_raw == 1 is full-time employment, so anyone unemployed or
#      out of the labour force reads 0 rather than NA. Every current
#      consumer conditions on employed == 1 first and is correct, but
#      mean(fulltime) over a panel is the population full-time rate and
#      reads like the conditional one. fulltime_emp is added beside it,
#      NA off the employed, and fulltime is left exactly as it was.
#
#  (4) experience_years IS NOT IN 00_compat's OLF_LEVEL_VARS AND SHOULD
#      BE. That list carries actual_experience, which is ehtjb and which
#      shows the entry-cohort recall gradient the list exists to police.
#      experience_years is pmin(coalesce(actual_experience,
#      potential_experience), age - 13), so it IS actual_experience
#      wherever ehtjb is observed, under a name the guard does not know.
#      02_fte_experience.R's base_exp_cal is experience_years at the
#      person's first wave, so exp_hybrid_cal, exp_hybrid_fte,
#      exp_hybrid_fte_obs and exp_hybrid_one_obs inherit it too, and
#      03's T8 puts two of those into a specification with no person
#      fixed effects. A loader cannot enforce this. It can declare it,
#      so the experience_level_cols attribute now names all of them and
#      the restrictions manifest carries the row.
#
#      THE MATCHING EDIT IN 00_compat_v21.R IS NOT OPTIONAL:
#        OLF_LEVEL_VARS  add "experience_years", "base_exp_cal",
#                        "exp_hybrid_cal", "exp_hybrid_fte",
#                        "exp_hybrid_fte_obs", "exp_hybrid_one_obs"
#        COMPAT_PANEL_TAGS  add "v21.6"
#
#  (5) THE FIVE TRAINING CODE COLUMNS SURVIVE THE _raw SWEEP, added to
#      RAW_COLS_KEEP so the coding audit in (2) has something to read
#      after combination. They are not carried into analysis_sample.
#
# =====================================================================
# v21.5 CHANGES. THIS BUILD REQUIRES A FULL REBUILD. The variable map
# changes, so the hash invalidates every checkpoint by itself, and
# CHECKPOINT_TAG is bumped as well rather than relying on that.
#
# The 01_discovery_extended.R revision 7 run contained the answers to its
# own absent-name list. Six of the eight names that were absent AND read
# by a derived column are resolved here; only bnfip is still open.
#
#  (1) FIVE MNEMONICS WERE WRONG BY A LETTER OR A WORD. The release
#      carries tcn04, tcn514, tcn1524 and tcn25 for the age-banded counts
#      of own non-resident children, all thirteen waves at 74.9% observed;
#      the map asked for tcnr04 and so on. The error was a reasonable
#      extrapolation, since tcnr without a suffix IS the total and IS in
#      the release, and HILDA drops the r for the age bands. That is four
#      of the ten all-NA columns.
#
#      Overall job satisfaction is jbmsall, "C33f Overall job
#      satisfaction", thirteen waves. The map asked for josat. That is a
#      fifth.
#
#  (2) THE BUSINESS FIELD DOES NOT EXIST. edqobc was never going to
#      resolve: the edqo* battery runs ad, av, bd, c1 to c4, cd, dc, dn,
#      dp, gc, gd, hd, ms, nq, sh, sl, ta, tc, tq, and ASCED has no
#      business category in it. The map entry is removed and edqobc_raw
#      is a literal NA, so has_business_any is empty BY CONSTRUCTION
#      rather than by accident and is declared in EXPECTED_EMPTY_COLS.
#      The education_pathway branches that read it are left untouched:
#      they never fired, because case_when() skips an NA condition, and
#      leaving them costs nothing while editing them would be a mutating
#      change for no gain.
#
#  (3) years_unemp_since_school IS REMOVED, not repaired. It rested on
#      ujljryr, which is absent; the nearest real item is ehtujyr, which
#      is NPQ and so entrant-only; and the quantity it wanted is already
#      in years_unemployed, read from the ehtuj History DV for about 95%
#      of person-waves. Repointing it would have added an entrant-only
#      duplicate of a column that already works. The name is registered
#      as a deprecation so the scan finds any script still reading it.
#
#  (4) has_nursing_any HAS TWO INPUTS THAT ARE NOT NURSING. edqodn and
#      edqrodn are "Diploma NFI", not further identified, and the suffix
#      dn appears to have been read as diploma-nursing. The genuine item,
#      edqonq, is in the list too. So anyone reporting an unspecified
#      diploma currently reads as holding a nursing qualification.
#
#      Correcting it in place would change education_pathway, which is a
#      MUTATING change under the scope rule, so has_nursing_strict is
#      added BESIDE it from the nine ednrs* items and edqonq alone, and
#      the two counts are printed together. Note also that edqrodn is the
#      only CPQ input in any of the four field flags, so it is the only
#      thing that can make has_nursing_any non-missing for a continuing
#      person: if the entrant test returns "not routed" for nursing and
#      "entrant-only" for the other three, that is the erroneous input
#      talking, not coverage. has_nursing_strict is on the test list for
#      exactly that comparison.
#
#      The years mapping is NOT affected. It reads edqodn as 14.5 years,
#      which is right for a diploma whatever its field.
# =====================================================================
#
# v21.4 CHANGES. The routing report in 01_discovery_extended.R revision 5
# came back with 43 NPQ-only mnemonics rather than the one that prompted
# it, and 31 of those are the qualification batteries: ednrs* ten items,
# edtch* ten, edqo* twenty-one. They are the inputs to has_nursing_any,
# has_teaching_any, has_trade_any and has_technical_any, all four of
# which reach the panel and feed the enriched education_pathway labels.
#
#  (1) THE ENTRANT TEST IS GENERALISED rather than repeated. v21.2 wrote
#      a cross-tabulation for enrolled_in_course; this runs the same test
#      over a declared set of columns and prints one table. Label
#      prefixes are evidence and not proof, which is what the discovery
#      script says about derived variables, so the columns are MEASURED
#      here rather than marked on the strength of a prefix.
#
#  (2) THE VERDICT TRAVELS WITH THE PANEL. Columns that come back routed
#      are recorded in the entrant_only_cols attribute, so 00_compat's
#      guard reads what this build measured instead of holding a second
#      copy of the list that can drift from it.
#
#  (3) WHAT THE FLAGS ACTUALLY SAY. flag_any_set() returns NA when no
#      input was observed, so the four field flags are NA rather than
#      falsely zero for anyone not asked, which is the v19 fix working.
#      What that leaves is a column missing for most of the panel whose
#      counts read as panel counts: 76 nursing, 46 teaching, 57
#      technical and 422 trade. Australian nursing is nearer 2.5 to 3
#      per cent of employment and trade certificates near 20 per cent of
#      the population, and the gap is about the size of the entrant
#      share. These cannot be repaired by adding the CPQ battery either:
#      edqro* asks which qualifications were completed since the last
#      interview at ASCED LEVELS, with no nursing, teaching or trade
#      category. The fields exist only in the entrant instrument.
#
#  NOT CHANGED, deliberately. qual_count sums both batteries with
#  rowSums, so it is a lifetime count for entrants and a
#  since-last-interview count for everyone else. It does not reach the
#  panel through the select, so the defect is contained, and repairing it
#  would be a MUTATING change to a derived column under the scope rule.
#  Marked at the site instead. years_npq_total is also NPQ-only, and that
#  was already known and correctly guarded: npq_total_discrepancy returns
#  NA when either side is missing, and the comment beside it already says
#  ehttot is available for fewer person-waves.
# =====================================================================
#
# v21.3 CHANGES. The routing test came back ROUTED on the first uncached
# run, at 0.0% of 113,926 continuing person-waves against 52.2% of 9,355
# entrant ones, so two of these follow from that and one from what the
# first uncached absence audit turned up.
#
#  (1) A COLUMN CAN BE EMPTY IN SOME WAVES AND PASS EVERY AUDIT. The
#      all-NA sweep asks whether a column is empty everywhere; the
#      absence audit's first list asks whether a mnemonic is missing from
#      every wave. A variable present in three waves of thirteen answers
#      no to both and reaches the panel looking like a thirteen-wave
#      series. The uncached run showed four such columns:
#      currently_pregnant, self_or_partner_pregnant, expected_birth_month
#      and expected_birth_year, all resting on ft* items that exist only
#      at waves 15, 19 and 23. Wealth has the same shape and got
#      wealth_module_wave plus a hard stop; the pregnancy module got
#      nothing. The sweep now reports every column that is empty in at
#      least one wave and populated in another, and classifies each
#      against the known module shapes so that only the unexplained ones
#      need reading.
#
#  (2) enrolled_in_course IS ENTRANT-ONLY, so there is no comparable
#      series to hold. When the routing test returns ROUTED, the
#      *_comparable column is emptied for the whole panel rather than for
#      wave 24 alone, on the firm-size precedent: an item that cannot
#      support a series gets an empty comparable column and a reason, not
#      a restriction aimed at one wave. The blanking is driven by the
#      test rather than by a reading of a log, so a release that changes
#      the routing gets its column back.
#
#  (3) THE STUDY ITEMS ARE RAW CODES, NOT INDICATORS. edcqen carries 1
#      and 2, so mean(enrolled_in_course) is near 1.75 and sum() means
#      nothing. Two 0/1 columns are added beside the raw ones, built as
#      == 1 after the observed code set is printed, which is additive and
#      leaves the raw codes in place.
#
#  Also: pct_eq1 in the continuing-sample table printed NaN where the
#  continuing sample had no observations left, which is exactly the case
#  the table exists to show. It reports NA now.
# =====================================================================
#
# v21.2 CHANGES. One diagnostic, and a note. NO COLUMN CHANGES: a panel
# written by v21.1 is schema-identical to one written by v21.2 and does
# not need rebuilding. CHECKPOINT_TAG does not move either.
#
#  (1) THE ENROLMENT ITEM MAY BE ROUTED, WHICH WOULD MAKE THE WAVE 24
#      RESTRICTION TRUE FOR THE WRONG REASON. v21 read the jump in
#      enrolled_in_course coverage, 3.6% at wave 23 against 12.3% at
#      wave 24, as an instrument change; the run after it suggested a
#      sample refresher instead. The discovery catalogue points at a
#      third reading that fits both better. edccenr, "CPQ:A17b Currently
#      enrolled in a course", exists in all thirteen waves and is NOT in
#      this map, while the CPQ items that are read sit near 72%
#      observed. If edcqen is the new-person-questionnaire counterpart,
#      then its non-missing share is the NEW ENTRANT share, which is 2
#      to 4 per cent in an ordinary wave and jumps when a refresher
#      lands. On that reading the column describes new entrants in EVERY
#      wave and not merely at wave 24, which is a larger restriction
#      than the one v21 imposed and a different one.
#
#      The test is one cross-tabulation against entry_wave, which the
#      restriction block already builds, so it costs nothing and is
#      added there rather than argued about. Value labels are stable
#      across the window on both study items, so the instrument reading
#      has nothing behind it either way.
#
#  (2) CANDIDATES FOR THE NEXT REBUILD are listed beside the map. Adding
#      a mnemonic invalidates the hash and re-reads thirteen waves, so
#      the standing rule is to batch them; a list of what is waiting is
#      what makes batching possible. The hash is computed from the
#      map's VALUES, so a comment costs nothing.
# =====================================================================
#
# v21.1 CHANGES. Six fixes read off the v21 W12-W24 log. BUILD_TAG moves
# to v21.1 because entry_cohort's LABELS change, so a v21 panel and a
# v21.1 script disagree about a column. CHECKPOINT_TAG still does not
# move: nothing inside process_wave_complete() changed.
#
#  (1) THE CACHED-RUN FALSE ALARM. The v21 log reported all seven
#      candidate stems as "NONE RESOLVED - derived column is all NA"
#      while the same log showed 28.82 hours of own-child care, 30,504
#      wealth observations and 18,149 superannuation couple-waves. The
#      presence accumulator is written by process_wave_complete(), which
#      a cached wave never calls, so on a warm cache the presence table,
#      the absence audit and the candidate-stem report were all built
#      from nothing. Acting on that report would have deleted seven
#      working columns. The wave loop now records which waves were
#      processed and which were restored, every one of the three audits
#      says so, and the candidate report falls back to counting
#      non-missing values on the PANEL, which is true in either mode.
#
#  (2) THE STUDY ITEMS ARE NOT A PAIR. edfts is observed for 100% of
#      person-waves in every wave including 24; only edcqen moves, from
#      3.6% at wave 23 to 12.3% at wave 24. v21 said both. The table now
#      prints the share equal to one as well as coverage, for both
#      items, so a level break is visible where a coverage break is not.
#
#  (3) COVERAGE AGAINST COMPOSITION, decided rather than assumed. edcqen
#      is observed for 2-4% of person-waves, so its non-missing share is
#      closer to an enrolment rate than a coverage rate, and wave 24 adds
#      roughly 1,000 respondents through a refresher. Both an instrument
#      change and a sample change fit the jump. The continuing sample,
#      the persons observed at both wave 23 and wave 24, holds the sample
#      fixed and separates them; it is now printed.
#
#  (4) THE DEPRECATION SCAN reported eleven hits in 00_compat_v21.R,
#      which names every removed column because guarding them is its
#      job, exactly like this loader. The compat file is excluded, as are
#      the archived loaders and the synthetic-data builders, which CREATE
#      the old names rather than read them; both exclusions are counted
#      out loud. Paths are normalised before de-duplication and reported
#      relative to the scan root, because basename() made three copies of
#      00_load_hilda.R in three directories read as one file with
#      contradictory line numbers.
#
#  (5) firm_size_comparable is all-NA BY DESIGN, so it was diluting the
#      degenerate-column warning. EXPECTED_EMPTY_COLS names the columns
#      that are empty by construction; the warning now counts only the
#      surprises, and the sweep still lists everything.
#
#  (6) TWO LABELS THAT DID NOT MATCH THEIR NUMBERS. The cohort spread was
#      pooled across sex, printing 2.11 years where the paper reports
#      4.93 against 1.72 for women, so sex is now in the grouping. And
#      entry_cohort called entry at wave 20 or later "2019_plus" while
#      the wave-19 refresher entrants sat in the middle group; the labels
#      are now the waves they describe.
# =====================================================================
#
# v21 CHANGES. The five analysis-stage restrictions settled off the v20
# log, made operative in the panel rather than left as a note on a
# script, plus the two cosmetic items from the same log.
#
# CHECKPOINT_TAG is NOT bumped. Everything added here is built AFTER
# combination, so no wave is reprocessed and the per-wave cache stays
# valid. Delete wave_checkpoints/ only if you also change a mutate body.
#
#  (1) BUILD_TAG. Every runtime warning and stop now names the build
#      that emitted it by reading one constant. Warnings 3 and 4 in the
#      v20 log still announced themselves as v19 because the tag was
#      typed into each message by hand, which is the same
#      second-source-of-truth failure as the hard-coded deprecation
#      call-site list removed in v20. Section HEADINGS keep the version
#      that introduced the check; those are provenance, not a claim
#      about which build is running.
#
#  (2) ORPHAN QUARTERS are a NOTE, not a warning. They are known,
#      counted, split into contradictions and blank-event reports, and
#      excluded from *_quarter_event and from post_birth_window_share
#      either way. Four of them competing for attention with the
#      firm-size warning costs more than it buys. The note escalates
#      back to a warning above ORPHAN_QUARTER_MAX, which is what the
#      quarter items escaping the event filter would actually look like.
#
#  (3) FIRM SIZE cannot cross wave 22 in any specification. The frames
#      reconcile; the coverage does not, at roughly 60% of the employed
#      classified before the break against 99% after, and a small-firm
#      share near 5% against 26% once the frames are aligned. The single
#      FIRM_SIZE_VERIFIED switch is split into frames and coverage, so
#      confirming the frames can no longer open firm_size_comparable on
#      its own; firm_size_regime is written to the panel; and the guard
#      in 00_compat_v21.R stops an estimation frame carrying classified
#      firm size on both sides.
#
#  (4) WAVE 24 EDUCATION is a composition break, not a trend, and the
#      study items sit on a different instrument there: enrolled_in_course
#      and full_time_student go from 3.6% to 12.3% observed. The raw
#      columns are kept, *_comparable copies are NA at wave 24,
#      study_item_regime and educ_trend_comparable are written, and the
#      education table prints coverage for both items so the jump is on
#      the log rather than inside an event-study coefficient.
#
#  (5) OLF ENTERS AS A FLOW, NOT A LEVEL. The base stock is a
#      retrospective report taken at panel entry and rolled forward, so
#      its level carries the entry cohort's recall vintage: 4.93 years
#      for the original and 2011 intakes against 1.72 for the 2019-plus
#      intake. The within-person annual increments (d_olf_years,
#      d_unemp_years, d_paid_years, d_exposure_years, history_flow_sum,
#      olf_year, olf_flow_valid) are built once on the panel instead of
#      being recomputed in every downstream script, and entry_wave and
#      entry_cohort travel with them.
#
#  (6) SUPERANNUATION AND WEALTH. hwsupei is a household aggregate, so
#      there is no individual terminal-value comparison to be had from
#      it: super_balance is renamed hh_super_balance and the old name
#      becomes a deprecation with a hard stop. Wealth is a module
#      variable observed at waves 14, 18 and 22 inside this window, so
#      wealth_module_wave is written and a wealth value found outside
#      those waves halts the build rather than being averaged into a
#      trend. The deprecation matcher is now word-boundary aware, since
#      a fixed-string grep for super_balance also matches its own
#      replacement.
# =====================================================================
#
# v20 CHANGES. Six fixes from the v19 W12-W24 log. Tag bumped again.
#
#  (1) ORPHAN QUARTERS, and the correction to the v19 diagnosis. The
#      NA-box flag was NEVER double-coded: the v19 cross-tabulation
#      shows lebthna partitioning the 7,018 reports exactly, 342
#      undated and 0 dated. The four-case discrepancy is four
#      person-waves that carry a quarter flag WITHOUT reporting the
#      event, so birth_quarter had 6,673 non-missing against 6,669
#      reporter-dated births and v18 differenced an unrestricted count
#      against a restricted one. *_quarter_event (reporter-restricted)
#      and *_quarter_orphan are added; the orphans are split into
#      contradictions and blank-event reports by the new diagnostic.
#
#  (2) post_birth_window_share now reads *_quarter_event. Under v19 all
#      four orphans received a post-birth window share, i.e. treated
#      event time on an untreated person-wave.
#
#  (3) The year table restricts its dated and multiple counts to
#      reporters and prints Row_residual, which is 0 in every row by
#      construction. Under v19 four of thirteen rows did not add up.
#
#  (4) total_unpaid_hours_raw and total_unpaid_hours_capped were added
#      to the training keep list in v19 but never to the final select,
#      so the panel never received them. Fixed.
#
#  (5) DEGENERATE-COLUMN AUDIT generalised from a hand-kept list to a
#      sweep over EVERY column. The list missed fy_benefits, all-NA
#      because bnfip is absent from all thirteen waves, which is the
#      transfer income the FTB-B test is built on.
#
#  (6) FIRM SIZE re-diagnosed. The value labels resolve both frames and
#      the category assignments already agree across them, so this is
#      not a mapping error. It is coverage: ~60% of the employed
#      classified before the break against ~99% after. The crosswalk is
#      documented and the message corrected. Also: the hard-coded
#      deprecation call-site list is removed (every line number had
#      drifted and it missed a file), and the scan now recurses.
# =====================================================================
#
# v19 CHANGES. Seven fixes, all of them read off the v18 W12-W24 log.
# CHECKPOINT_TAG is bumped, so every wave reprocesses.
#
#  (1) QUARTER DECLINED vs UNDATED. birth_q_declined was conditioned on
#      the event report only, not on the absence of a quarter flag, so
#      it counted the NA box wherever it was ticked. It summed to 342
#      against 338 zero-flag reports and the reconciliation printed the
#      345 undated as its denominator, which they are not. Four
#      person-waves tick a quarter AND the box (2015, 2021, 2023 and
#      one 2024 case). birth_q_n_set now precedes the flag in the
#      mutate, and three columns are carried: the raw NA-box flag,
#      birth_undated (reported with no quarter, 338 by construction)
#      and birth_q_declined_dated (the double-coded cases). Event
#      dating must use *_undated. Same for the pregnancy item. The
#      reconciliation now states four mutually exclusive cells and
#      STOPS if they do not exhaust the reports.
#
#  (2) SILENT-NA INDICATORS. has_business_any read exactly 0 across all
#      134,131 person-waves because it rested on one unresolved
#      mnemonic and coalesce(NA == 1, FALSE) is FALSE. The field
#      indicators now use flag_any_set(), which returns NA where no
#      component is observed, and the per-wave print shows set over
#      observed so a missing variable cannot present as zero incidence.
#
#  (3) FULL-MAP ABSENCE AUDIT. The presence report covered 27 of ~228
#      mapped names while every wave silently reported 28-33 unmapped
#      columns. Absent names are now recorded per wave and the stems
#      absent from every wave are printed by name after combination.
#
#  (4) DEGENERATE-COLUMN AUDIT. The same failure from the other end:
#      derived indicators that come out constant or all-NA are listed
#      and warned on, so (2) cannot recur under a different mnemonic.
#
#  (5) FIRM SIZE. jbmemsz through wave 21, jbmems2 from wave 22. The
#      existing recode switches instrument but does not produce a
#      comparable series: the classified small-firm share goes 5% to
#      29% and coverage from ~6,500 to ~10,300 at the boundary. No
#      mapping is invented. The raw code frame and value labels are
#      printed per wave, firm_size_instrument is written to the panel,
#      the break is quantified after combination, and
#      firm_size_comparable is NA until FIRM_SIZE_VERIFIED is set.
#
#  (6) UNPAID HOURS. The 168-hour clamp was applied per component and
#      not to the sum, so 37 person-waves exceeded a week. The sum is
#      clamped, the pre-cap value kept in total_unpaid_hours_raw and
#      the affected rows flagged by total_unpaid_hours_capped.
#
#  (7) DEPRECATION SCAN and EDUCATION BREAK. The downstream-script grep
#      was left commented out while the log listed twelve live call
#      sites; it now runs by default and warns. Education composition
#      by wave is printed with a jump test, because wave 24 combines a
#      refresher with an instrument change and is not on trend.
#
# Also: namespace protection moved to the top of the file, and the v17
# and v18 print labels updated so an archived log names its own build.
# =====================================================================
#
# v2 CHANGES (earnings-based redesign). See CHANGELOG at end of header.
#   (a) Age window widened 21-52 -> 20-55 at Tier 1 so the analysis
#       stage can choose its own window without re-running the loader;
#       a set of ANALYSIS_AGE_* flags is written so downstream code can
#       subset without hard-coding.
#   (b) Earnings deflated: real_weekly_earnings, real_annual_income,
#       plus IHS transforms defined at zero for the non-employed.
#   (c) Earnings zeros made explicit: earnings_is_zero /
#       earnings_component_flag record whether a zero is a true zero
#       (non-employed) or a coalesce artefact.
#   (d) New income variables: government benefits, total FY income,
#       household disposable income (FTB band + couple totals).
#   (e) Employment-history decomposition. Verified against the HILDA
#       variable labels, the family is:
#         ehtse  History: Time since FT education - years  [DENOMINATOR]
#         ehtjb  History: Time in paid work - years
#         ehtuj  History: Time unemployed and looking for work - years
#         ehto   History: Time not working and not looking - years
#         ehttot NPQ:A28 Total years since left school - manual addition
#       The identity is  ehtse ~ ehtjb + ehtuj + ehto.
#
#       TWO CORRECTED MISREADINGS, recorded so they are not repeated:
#       v2 used the mnemonic `ehtnl` for the not-in-labour-force stock.
#       No such variable exists; it loaded as an all-NA column behind
#       the any_of() guard and failed silently. The correct mnemonic is
#       `ehto`.
#       v3 then read the "se" in `ehtse` as self-employment. It is
#       "since education". Summing ehtse with ehtjb double-counted the
#       denominator against its own components, giving a flow sum of 2
#       years per wave and an ehttot discrepancy rising with career
#       length (7 childless, 19 parents). There is NO self-employment
#       stock in this variable family.
#       Any panel built before v4 must be rebuilt (CHECKPOINT_VERSION).
#
#       NOTE: experience_years is built on ehtjb and was CORRECT
#       throughout. No published estimate is affected by either fix.
#
#   (i) v5 EARNINGS CORRECTION, superseding an incorrect v4 change.
#       Verified against HILDA variable labels:
#         wscei  DV: Current WEEKLY gross wages & salary - ALL JOBS
#         wsfei  DV: Financial year gross wages & salary
#         wsfes  DV: Financial year gross wages & salary [INC SALARY
#                SACRIFICE]  -- NOT self-employment income
#         tifefp DV: Financial year gross regular income (individual)
#         hifefp DV: HOUSEHOLD financial year gross regular income
#
#       v4 divided wscei by 2 on the strength of a median ratio of 0.5
#       between annual_income and wscei*52. That diagnosis was WRONG and
#       is reverted here. The label says weekly, and at wave 24 the
#       medians agree: wscei*52 = 78,000 against wsfei = 74,972, a 4%
#       gap consistent with job changes and part-year work. Halving
#       drove the median real hourly wage to $21.30 (against an
#       Australian full-time median near $40-45), pushed the multi-job
#       gap AWAY from zero (0.50 -> 0.75), and cost 9,182 person-waves
#       to the hourly_wage_clean < 10 filter, disproportionately
#       part-time mothers.
#
#       The 0.5 ratio was caused by annual_income itself. v2-v4 built it
#       as wsfei + wsfes on the assumption that wsfes was
#       self-employment income. It is the SAME wages and salary
#       including salary sacrifice, so the sum double-counted wages and
#       ran at roughly twice individual annual pay. annual_income is now
#       a single variable, and any earnings analysis run on a v2-v4
#       panel used a doubled annual measure.
#
#       NOTE: wscei covers ALL JOBS, not the main job. The multi_job_gap
#       diagnostic is retained but renamed, since it was never a
#       multiple-jobs measure.
#
#   (j) v5: fy_income_gross (tifefp, individual FY gross regular income)
#       is the PREFERRED child-penalty outcome. It captures total
#       individual income, so a mother who moves from wages onto
#       transfers shows the full income path rather than a drop to zero
#       that a wages-only measure would record.
#
#   (l) v7: SELF-EMPLOYMENT AND BUSINESS INCOME.
#       Self-employment is a post-birth flexibility margin used
#       disproportionately by mothers, and the loader has been blind to
#       it. wsfei/wsfes are wages and salary only, so a mother who moves
#       from employment into her own business reads as an earnings drop
#       to zero. fy_income_gross (tifefp) is NOT the substitute: it
#       includes government transfers, and family payments rise exactly
#       when the earnings penalty opens, so it understates the penalty.
#       v7 adds business income and a total-earnings measure that is
#       wages plus business income and excludes transfers.
#
#       DISCOVERY RATHER THAN GUESSWORK. The header above records what
#       happens when a mnemonic is guessed: `ehtnl` did not exist,
#       loaded as an all-NA column behind any_of(), and failed silently
#       for two versions. The two v7 mnemonics are therefore NOT
#       hard-coded. Set V7_DISCOVER <- TRUE and run once; v7_discover()
#       reads one wave file for structure only and reports candidates
#       three ways: by Stata variable LABEL (which haven preserves and
#       which is stable across releases in a way mnemonics are not), by
#       the jbm prefix (main-job attributes), and by the es prefix
#       (employment-status derivations). Paste the confirmed names into
#       V7_EMPTYPE and V7_BUSINC, set V7_DISCOVER <- FALSE, rerun.
#
#   (m) v8: MNEMONICS CONFIRMED BY DISCOVERY, and the business-income
#       sign convention verified.
#         jbmmply  C31 Which of these best describes employer/business
#                  Current MAIN JOB attribute (jbm prefix). The label
#                  search also returned pjottyp (previous job) and
#                  ujljtyp (last job); both are retrospective and are
#                  NOT what the audit needs. jbmmply is a PQ item, not
#                  a DV, so its category numbering must be read from
#                  the value labels rather than assumed.
#         bifip    DV: FY business income, imputed, POSITIVE values
#         bifin    DV: FY business income, imputed, NEGATIVE values
#
#       SIGN CONVENTION. bifin stores MAGNITUDES, not signed values.
#       At wave 24: 1,172 person-waves have bifip > 0, 118 have
#       bifin > 0, and NONE has bifin < 0, with a median non-zero bifin
#       of 7,125. Net business income is therefore the DIFFERENCE,
#       bifip - bifin. Summing would have recorded a $7,125 loss as
#       $7,125 of income, inverting the sign for precisely the
#       self-employed the margin exists to measure.
#
#       Losses are RETAINED as negative values rather than floored at
#       zero. A mother whose business runs at a loss has negative
#       labour income for that year, and flooring would understate the
#       penalty. fy_earnings_total can consequently be negative; the
#       IHS transform is defined on the whole real line and handles
#       this, which is a further reason it was preferred to the log.
#
#       Household equivalents hibifip / hibifin exist and are NOT used.
#
#   (n) v9: EMPLOYMENT-TYPE VARIABLE CORRECTED. The v8 candidate was
#       WRONG and the discovery gate caught it before a panel was built.
#
#       REJECTED: jbmmply (C31 "which of these best describes
#       employer/business"). Its value labels are 1 Private sector for
#       profit, 2 Government business enterprise, 3 Other commercial,
#       4 Private sector not for profit, 5 Other governmental, 6 Other
#       non-commercial. That is SECTOR, not employment type: there is no
#       employee-versus-self-employed distinction anywhere in it.
#       EMPTYPE_EMPLOYEE <- c(1) would have classified every government
#       and not-for-profit EMPLOYEE as self-employed. The label text
#       ("employer/business") read plausibly; only the value labels
#       revealed the problem, which is why the gate prints them.
#
#       CONFIRMED: esempdt, "DV: Current employment status (detailed)".
#         1 Employee                                        9,726
#         2 Employee of own business WITH other employees      363
#         3 Employee of own business WITHOUT other employees   223
#         4 Employer/Self-employed WITH employees              188
#         5 Employer/Self-employed WITHOUT employees           696
#         6 Unpaid family worker                                19
#       Among the employed (11,215 non-missing at wave 24) code 1 is
#       86.7% and codes 2-6 are 13.3%, consistent with Australian
#       self-employment near 15%. Being a DV it is also more stable
#       across waves than a raw PQ item.
#
#       NOTE esempdt and esempst use DIFFERENT numbering. esempst
#       collapses to four categories, and its code 3 means
#       "Employer/Self-employed" where esempdt's code 3 means "Employee
#       of own business without other employees". This is precisely the
#       CPQ/PQ divergence the EMPTYPE_EMPLOYEE placeholder warned about,
#       so if a wave lacks esempdt and falls back to esempst, the
#       constant must change with it.
#
#       THREE DEFINITIONS are carried, because the choice is
#       substantive rather than technical. Codes 2-3, "employee of own
#       business", are a LEGAL FORM: someone who incorporates and pays
#       themselves a salary is an employee of their own company but is
#       economically self-employed, and their income arrives in
#       wsfei/wsfes as WAGES rather than in bifip. A
#       business-income-only definition would miss them entirely. Since
#       incorporating a consultancy is exactly the post-birth
#       flexibility margin this variable exists to capture,
#       is_self_employed uses the ECONOMIC definition (codes 2-6) and
#       is_selfemp_narrow (codes 4-5) is reported alongside so the
#       choice is visible rather than buried in the loader.
#
#       bifhave (F42, work in own business/farm/silent partner/trust
#       beneficiary last financial year) is mapped as an INDEPENDENT
#       cross-check. Two measures of the same thing is what would have
#       caught the jbmmply error a turn earlier. It is also on a
#       last-financial-year basis, matching bifip/bifin, where esempdt
#       is current status.
#
#   (o) v10: bifhave coding corrected, and the checkpoint version is now
#       partly auto-derived.
#
#       bifhave (F42, own business last financial year) was read with
#       the 1=No, 2=Yes convention HILDA uses for many binaries. F42
#       uses 1=Yes, 2=No. The cross-check exposed its own error: it
#       reported 83.9% of workers running a business, and put 82.9% of
#       all employed person-waves in the cell where an EMPLOYEE also
#       has a business. Both are inverted-binary signatures. The
#       corrected table has employees-without-a-business dominant and
#       self-employed-with-a-business as the second diagonal.
#       NOTHING else depended on it: bifhave is a cross-check on
#       esempdt, not an input to is_self_employed.
#
#       CHECKPOINT_VERSION now appends a short hash of the variable map,
#       the v7 mnemonic constants and the employment-type codes, so a
#       change to any of them invalidates the cached wave files
#       automatically. This was added after three separate fixes were
#       silently masked by stale checkpoints: the version string is a
#       manual bump, and it is easiest to forget for exactly the small
#       fixes that are hardest to spot going missing. The hash does not
#       cover derived-column logic, so a change to a mutate() body still
#       needs a manual bump of the tag; see the note at the definition.
#
#       A hard resolution check sits immediately after read_dta: a
#       mnemonic that is NAMED but ABSENT from the wave file stops the
#       loader rather than producing an NA column. Before discovery has
#       been run both constants are NA_character_ and safe_numeric
#       returns NA columns, so the loader still builds. It is a WRONG
#       name, not a missing one, that halts it.
#   (f) experience_source flag: records whether experience_years came
#       from ehtjb or fell back to the Mincer potential proxy, so the
#       fallback rate can be reported by gender and event time.
#   (g) Training intensity carried through: total_training_hours is now
#       constructed explicitly rather than relied on via any_of().
#   (h) Weekly-vs-annual reconciliation diagnostic (both all-jobs).
#
#   (k) v6: hours labels corrected. HILDA labels are
#         jbhruc   DV: Hours per week usually worked in ALL JOBS
#         jbmhruc  DV: Hours per week usually worked in MAIN job
#       The "m" is MAIN, not multiple. v2-v5 had these inverted in the
#       comments and computed the derived gap as main minus all, the
#       negative of the second-job margin, which is why it returned
#       zero. Both variables are DVs in every wave 1-24, so the zero was
#       not a missing-column artefact.
#       NO SUBSTANTIVE RESULT IS AFFECTED. hours_worked has always been
#       jbhruc (all jobs), which is the correct denominator for the
#       hourly wage (wscei is also all-jobs) and the correct measure for
#       the specialisation regressor S_ct, the FTB-B labour-supply first
#       stage, and the within-person hours changes at parenthood.
#       Only the unused diagnostic was wrong; it is renamed
#       second_job_hours and given the correct sign here.
#
# Waves 12-24 (2012-2024), one pass over the Combined files, writes:
#   hilda_panel_data_W12_W24_slim.rds  full select, waves 12-24
#                                      (input to MASTER_hh.R Parts A-F)
#   hilda_panel_data_extended.rds      same select, waves 15-24
#                                      (input to MASTER_PART_G / Part H)
#   hilda_panel_data_training.rds      slim training extract, waves 12-24
#                                      (input to training_event_study.R)
# Part G's fallback file hilda_panel_data_W12_W24.rds is no longer
# needed (extended.rds always exists after this run).
#
# Lineage: supersedes HILDA_LOADIND_MASTER.R (W15-24, v5) and
# LOADIND_HILDA_MAIN_wo_partner.R (W15-24, pre-fix). Carries the v5
# corrections: NA-propagating potential_experience and employed;
# biological experience ceiling (age-13); -5 schooling offset;
# ==1 training indicators; column-restricted reads; versioned wave
# checkpoints. The W12-W24 slim panel it writes is the CANONICAL
# rebuild: it is NOT guaranteed byte-identical to the historical slim
# panel (whose loader is not in the repo), so after the first run,
# re-verify the MASTER FINAL VERIFICATION counts against the tex.
# =====================================================================

# ---- v21: build tag ------------------------------------------------
# Read by every runtime warning and stop in this file, so an archived
# log names its own build and cannot drift the way the hand-typed v19
# tags did. Bump this alone; CHECKPOINT_TAG is separate and governs the
# wave cache.
#
# v21.7: bumped, and CHECKPOINT_TAG deliberately NOT bumped. The two
# tags answer different questions. This one says which build wrote the
# log and the panel attribute; that one says whether the cached waves
# are still the right waves. v21.7 changes only what the loader reports
# about columns it already built, so the cache is valid and a rebuild
# would be thirteen waves of work for an identical result.
# v21.32: bumped with CHECKPOINT_TAG, because this batch changes both
# what is read and what is derived from it.
# v21.43a. NO CHECKPOINT BUMP. This batch changes one post-combination
# diagnostic and nothing inside the wave mutate, so every cached wave is
# still correct and re-extracting thirteen of them to reprint one verdict
# would teach the reader that the tag moves for nothing.
# v21.43b. STILL NO CHECKPOINT BUMP. Post-combination diagnostic only.
# v21.45. STILL NO CHECKPOINT BUMP. This batch adds a switch, a guard and
# a fingerprint, all outside the wave mutate. The switch below is the
# supported way to force an extraction; the tag is not.
# v21.46. STILL NO CHECKPOINT BUMP, and the placement is the reason.
# firm_size_comparable and firm_size_post22 are both derived from
# firm_size and the wave, both of which are on the combined frame, so
# they are built after combination. Two case_when() calls inside the wave
# mutate would have cost a fourteenth extraction of thirteen waves.
# v21.48. MOVES WITH CHECKPOINT_TAG, because this batch changes what is
# read as well as what is derived. v21.47 moved CHECKPOINT_TAG and left
# this at v21.46, so a v21.47 log named itself v21.46.
# v21.49. NO CHECKPOINT BUMP. Both fixes run after the waves are
# combined, so every cached wave is still correct.
BUILD_TAG <- "v21.49"

# v21.11. DROP THE WAVE-PREFIXED SOURCE COLUMNS AT THE END OF EACH WAVE.
#
# Each wave frame retains about 497 columns, of which roughly 200 are the
# wave-prefixed mnemonics the map read: ledcqen, lhhd0_4 and so on. Every
# one has already been consumed by the mutates into a derived column by
# the time the frame is finalised, and every one is dropped at the final
# select. Between those two points they are carried through the checkpoint
# write, the thirteen-way combination, both full-panel sweeps, the
# restriction block and the select itself.
#
# The cost is 2,606 of combined_data's 2,927 columns. The two sweeps loop
# over columns, so they pay it directly; the combination and the
# checkpoints pay it in memory.
#
# Dropping them is exact rather than pattern-matched: unlist(v) IS the set
# of wave-prefixed names the map asked for, so the drop takes that set
# minus anything RAW_COLS_KEEP still needs.
#
# WHY THIS IS A SWITCH AND NOT SIMPLY DONE. It changes what a checkpoint
# contains. A directory holding some 497-column checkpoints and some
# 297-column ones would combine into a frame whose columns depend on when
# each wave was written, and nothing downstream would notice. So the
# switch is folded into the checkpoint hash below: turning it on
# invalidates all thirteen caches automatically and forces a uniform
# re-extract. That is the whole cost, and it is why the default is FALSE.
#
# TURN IT ON when you next rebuild for another reason. The additions in
# section 8 of PAPER_ATTENTION.md need a rebuild, so that is the batch to
# join.
# v21.12: TURNED ON. The switch was written to be enabled with the next
# rebuild that happens for another reason, and this is that rebuild. It
# takes combined_data from about 2,927 columns to 295, shrinks every
# checkpoint by roughly 40%, and takes the constant sweep from 12 s to
# under 1 s. The map has changed anyway, so the hash was invalidating
# regardless and this costs nothing extra.
DROP_WAVE_PREFIXED_RAWS <- TRUE

# v21.45. FORCE A FULL RE-EXTRACTION WITHOUT TOUCHING PROVENANCE.
#
# WHY THIS IS NOT CHECKPOINT_TAG. The tag means "the derived-column logic
# inside the wave mutate changed". Bumping it to force an extraction for
# any other reason writes a claim into thirteen filenames that is not
# true, and a tag that moves when nothing changed is a tag nobody reads.
# The two reasons to force an extraction that are NOT logic changes:
#
#   1. The presence table and the absence audit are built DURING
#      extraction. A cached run leaves them empty, and the log then says
#      so rather than pretending otherwise, but the audit still has to be
#      run somehow.
#   2. Validating the cache. Nothing has changed, so a re-extract must
#      reproduce the panel exactly. If it does not, an edit was masked.
#
# WHY THIS IS NOT IN THE HASH EITHER. DROP_WAVE_PREFIXED_RAWS is in the
# hash because it changes what a checkpoint HOLDS; two runs with
# different values must not share a cache. This switch changes nothing
# about the contents, only whether the cache is consulted, so folding it
# in would force a re-extract every time it was flipped back off.
#
# WHAT IT DOES. Renames the checkpoint directory to
# wave_checkpoints_retired_<version>_<timestamp> and lets the ordinary
# loop find an empty directory. Nothing is deleted, so the previous
# build is recoverable and comparable. It is all-or-nothing by
# construction, which the absence audit requires: a partial re-extract
# produces a misleading audit rather than a partial one.
#
# TURN IT OFF AGAIN AFTERWARDS. Leaving it on re-extracts thirteen waves
# on every run. That is the one respect in which it is worse than a tag
# bump, which forces exactly one re-extraction and then stops on its own.
#
# v21.45 USED THE TAG, NOT THIS SWITCH, and this stays FALSE. The two are
# alternatives and doing both would be worse than either: the bump makes
# the old checkpoints non-matching and the loop reports them as IGNORED,
# while the switch would move them out of the directory first and the
# report would be silent. Use this one when the re-extraction is routine
# and the tag when it is an event worth recording.
FORCE_REEXTRACT <- FALSE

# v21.11. THE CONTRACT THAT CLOSES THE SILENT EDIT SITES.
#
# 01discovery_extended.R Part 0c documents six edit sites for one added
# variable, of which the checkpoint hash covers only the first. Sites 2 to
# 6 are silent: a mnemonic added to the map and omitted from the final
# select() triggers a full thirteen-wave rebuild and then does not appear
# in the panel, with no error, because any_of() tolerates the absence. The
# loader's own header records this having happened.
#
# Name the columns you expect here when you add variables. The check runs
# on analysis_sample AFTER the select and stops on anything absent or
# all-NA, which is one edit covering sites 3, 4 and 5 as a class. An
# all-NA column counts as a failure because that is what a mnemonic
# absent from the release produces, and it is the failure the degenerate
# sweep finds hundreds of lines later rather than at the mistake.
#
# Leave it empty between builds. It is a checklist for the build in
# progress; the permanent register is v2_expected.
EXPECTED_NEW_COLUMNS <- character(0)
# v21.49a: CLEARED. The v21.49 batch arrived on the 5 October run and
# the contract passed on both: ever_parent_ref and educ_years_ref, the
# per-wave values the two fixes replace. v2_expected is the permanent
# register.
#
# v21.48a: CLEARED. The v21.48 batch arrived on the 1 October run and the
# contract passed on all three: total_training_days_z, total_courses_z
# and training_aim_nonresponse. v2_expected is the permanent register.
#
# v21.48: the v21.32 non-cash declaration was cleared at the same time.
# That batch landed at v21.32, and all eight names are in v2_expected.
# The note below records why five were left out at the time.
#
# v21.32. THE FIVE LEFT OUT, DELIBERATELY. noncash_any depends on nbcmany
# resolving, noncash_other_jobs_weekly on nbcoi, and the three childcare
# columns are conditional on receiving employer child care at all, which
# is a small share of a small share. The contract stops on all-NA, so
# declaring any of them would turn "this release does not carry it" into
# a build failure and invite the reader to delete the name rather than
# read it. The per-wave presence report and the absence audit are where
# those five are checked.
#
# v21.30a: CLEARED. The v21.29 aim batch arrived and the contract passed
# on all seven. v2_expected (83 columns) and training_vars (133) are the
# permanent registers for them now.
#
# Leaving a landed declaration in place is the failure mode at this
# point: it stops the next unrelated build on columns it was never about,
# and the natural response to that is to delete the offending name rather
# than read it, which trains the reader to disarm the check.


# v21.21a: CLEARED. The v21.21 batch arrived and the contract passed on
# all five, including the two the select had been advertising and not
# constructing. v2_expected is the permanent register for them now.

# v21.14d: CLEARED. The v21.12 to v21.14 batch has arrived and been
# verified, so the checklist has done its work and v2_expected is the
# permanent register for those nineteen columns.
#
# It earned its place on this batch: it caught training_gate_regime
# written onto panel_full and left out of the final select, which is edit
# site 3 of the six and would otherwise have produced a panel silently
# missing the column.
#
# LEAVING IT POPULATED IS THE FAILURE MODE NOW. A stale declaration stops
# the next unrelated build on columns it was never about, and the natural
# response to that is to delete the offending name rather than read it,
# which trains the reader to disarm the check. Fill it when adding
# columns; clear it when they arrive.

# v21.48. THE COLUMNS THIS BUILD MEANS TO CHANGE, for the fingerprint.
#
# The fingerprint at the end of the file compares this panel with the
# previous build's. On a full re-extract its verdict assumed nothing in
# the wave mutate had changed, so it read every difference as a stale
# cache. That holds for a cache validation. It is wrong for a build that
# edits the mutate on purpose, and it would have fired on this one.
#
# So name here the columns the build adds or changes. The verdict then
# reports those as expected and keeps the stale-cache warning for any
# difference outside the list. Same discipline as EXPECTED_NEW_COLUMNS:
# fill it when a build means to change the panel, and clear it once the
# build passes. A stale list lets a later build change these columns
# without comment.
EXPECTED_PANEL_CHANGES <- character(0)
# v21.49a: CLEARED. The 5 October run restored every wave from cache,
# so the fingerprint drew no verdict. It listed 2 columns added,
# ever_parent_ref and educ_years_ref, and 23 changed, all of them
# declared. The declared status, group and education-level columns did
# not appear, because the fingerprint counts distinct values for text
# columns; their tables in the log moved as expected. Fill it again
# when a build means to change the panel.
#
# v21.48a: CLEARED. On the 1 October run the full re-extract changed
# exactly the 19 columns v21.48 declared and nothing else: 3 added, and
# 16 aim columns with 4 more missing values each, the four trainees who
# answered don't know. Fill it again when a build means to change the
# panel.

WAVES <- 12:24   # full analysis window; wave 12 = 2012

# ---- v2: Tier 1 age window ----------------------------------------
# Kept deliberately wide. The loader should not decide the estimation
# window; the analysis stage subsets using the in_age_* flags written
# into the panel. Widening from the previous 21-52 recovers post-birth
# horizons for late first births and pre-birth trajectories for early
# ones, both of which the old window silently truncated.
AGE_MIN <- 20
AGE_MAX <- 55

# ---- v7: self-employment and business income ----------------------
# These two mnemonics are DELIBERATELY not guessed. Run the discovery
# mode below, read the output, and paste the confirmed names here.
#
# Workflow:
#   1. V7_DISCOVER <- TRUE, source this file. It prints candidates and
#      stops before any wave is processed.
#   2. Read the three listings. Confirm which variable is the CURRENT
#      MAIN JOB employment type, not a previous or last job, and which
#      carries financial-year business income.
#   3. Paste the names below WITHOUT the wave-letter prefix, e.g.
#      V7_EMPTYPE <- "jbmo62" if wave 24 shows xjbmo62.
#   4. V7_DISCOVER <- FALSE, rerun, bump CHECKPOINT_VERSION.
#
# Leaving these NA is safe: the columns build as NA and the derived
# self-employment variables are NA throughout, which is visible in the
# verification block rather than silent.
# CONFIRMED by two discovery runs on wave 24. See header notes (m), (n).
#
# Discovery is now COMPLETE: both mnemonics are resolved and the
# employment-status coding has been read from the value labels rather
# than assumed. Set V7_DISCOVER back to TRUE only to re-run discovery
# against a new HILDA release.
V7_DISCOVER <- FALSE           # discovery complete; see header (n)
V7_EMPTYPE  <- "esempdt"       # DV: Current employment status (DETAILED)
V7_BUSINC   <- c("bifip", "bifin")   # FY business income: POSITIVE, NEGATIVE

# Which categories of V7_EMPTYPE denote an EMPLOYEE.
# CONFIRM THIS ON THE DISCOVERY RUN. The default c(1) is a PLACEHOLDER.
# The CPQ and PQ versions of the employment-type question number their
# categories differently, so print the value labels before setting it:
#   attr(d[[paste0("x", V7_EMPTYPE)]], "labels")
# v7_discover() does this automatically when V7_EMPTYPE is not NA.
#
# CONFIRMED from the esempdt value labels (see header note (n)):
#   1 Employee                                      <- EMPLOYEE
#   2 Employee of own business WITH other employees
#   3 Employee of own business WITHOUT other employees
#   4 Employer/Self-employed WITH employees
#   5 Employer/Self-employed WITHOUT employees
#   6 Unpaid family worker
# Only code 1 is an employee proper. Codes 2-3 are a legal form rather
# than an economic category and are treated as self-employed; see the
# is_selfemp_narrow alternative constructed below.
#
# WARNING: this constant is specific to esempdt's SIX-category coding.
# esempst uses FOUR categories with different meanings (its code 3 is
# "Employer/Self-employed", esempdt's code 3 is "Employee of own
# business without other employees"). If a wave lacks esempdt and the
# fallback is used, this must change with it.
EMPTYPE_EMPLOYEE <- c(1)

# Codes treated as SELF-EMPLOYED under the narrow, employer/own-account
# definition, excluding the employee-of-own-business legal form.
EMPTYPE_SELFEMP_NARROW <- c(4, 5)

# Unpaid family workers: in work but with no earnings. 0.2% of the
# employed, but they belong in neither category and would distort a
# binary employee/self-employed split, so they are flagged separately.
EMPTYPE_UNPAID_FAMILY <- c(6)

# If business income arrives as SEPARATE positive and negative
# components, which is the convention HILDA uses elsewhere (cf. the
# *p / *n suffix pairs), V7_BUSINC must become a length-2 vector and
# the sum is taken before deflating. The extraction below handles both
# cases: a length-1 value is read directly, a length-2 value is summed.

# THE RAW HILDA RELEASE, AND THE ONLY ABSOLUTE PATH IN THIS FILE. project_dir
# is an input root: the .dta files live under it, and nothing but the wave
# checkpoints is written there. It is not the project. Outputs go to
# PROJ_ROOT when the contract is loaded and to getwd() otherwise; see
# .out_path() near the top.
project_dir <- "C:/Users/tur277/OneDrive - CSIRO/Documents - Copy/Y2025/HILDA_project"
# v-replication, 21 October 2026. HILDA_DATA_DIR, when set, names the folder
# that holds the raw Combined_*.dta files, so a replicator need not edit
# this file. Unset, the folder is the one above, as before.
data_dir <- if (nzchar(Sys.getenv("HILDA_DATA_DIR"))) Sys.getenv("HILDA_DATA_DIR") else
  file.path(project_dir, "dta_files")

# =====================================================================
# LOG  (v21.41)
# =====================================================================
# The loader was the only script writing nothing to run_logs/. When a
# downstream script reported a missing column there was no record of what
# the loader had actually built, only of what the script failed to find.
#
# "run_logs" RELATIVE TO getwd(), matching 00_contract.R's LOG_DIR. The
# first version of this block used file.path(project_dir, "run_logs"), so on
# any machine where getwd() is not project_dir the log landed in a second
# directory nobody was looking in, which reads exactly like no log at all.
#
# SELF-CONTAINED, not td_log_open(). The contract resolves a shared frame
# from the artefacts this file WRITES, so a loader that sourced it would
# resolve the previous build's panel while replacing it.
#
# reg.finalizer, NOT on.exit. on.exit registers on the current function
# frame and a script has none at top level; called through do.call the
# handler fires against do.call's own frame and runs at once. The first
# attempt printed the CLOSING banner before the opening one.
.ld_t0  <- Sys.time()
.ld_dir <- "run_logs"
if (!dir.create(.ld_dir, showWarnings = FALSE, recursive = TRUE) &&
    !dir.exists(.ld_dir)) {
  .ld_dir <- tempdir()   # never fail a rebuild over logging
  message("could not create run_logs/; logging to ", .ld_dir)
}
.ld_file <- file.path(.ld_dir, sprintf("LOADING_ND3_v3_%s.log",
                                       format(.ld_t0, "%Y%m%d_%H%M%S")))
.ld_con <- file(.ld_file, open = "wt")
.ld_sk_out <- sink.number(); .ld_sk_msg <- sink.number(type = "message")
# v21.47. Say it in the log rather than leaving it to be worked out from
# two files with the same modification time. split = TRUE passes output
# down to whatever stream is already active, so an inherited sink means
# this run appears in someone else's transcript as well as its own. That
# is correct when a driver wants it and a symptom when a script errored
# before closing its own.
if (.ld_sk_out > 0L)
  cat("NOTE: ", .ld_sk_out, " output sink(s) already open. This run will be\n",
      "  written into the log below AND into whatever is already recording.\n",
      "  If that was not intended, a script errored before closing its\n",
      "  transcript; run sink() until sink.number() reaches 0.\n", sep = "")
sink(.ld_con, split = TRUE)
sink(.ld_con, type = "message")

.ld_close <- function(status = "finished") {
  if (isTRUE(get0(".ld_closed", envir = globalenv(), ifnotfound = FALSE)))
    return(invisible(NULL))
  assign(".ld_closed", TRUE, envir = globalenv())
  el <- round(as.numeric(difftime(Sys.time(), .ld_t0, units = "secs")))
  cat("\n", strrep("=", 78), "\n", sep = "")
  cat(sprintf("LOADING_ND3_v3.R  %s in %dm %ds\n", status, el %/% 60, el %% 60))
  cat(sprintf("  log written to: %s\n",
              normalizePath(.ld_file, winslash = "/", mustWork = FALSE)))
  cat(strrep("=", 78), "\n", sep = "")
  # Unwind to the depth found, not to zero: a caller may hold sinks of its
  # own and they are not this script's to remove.
  while (sink.number(type = "message") > .ld_sk_msg) sink(type = "message")
  while (sink.number() > .ld_sk_out) sink()
  try(close(.ld_con), silent = TRUE)
}
.ld_closed <- FALSE
reg.finalizer(globalenv(), function(e) {
  if (!isTRUE(get0(".ld_closed", envir = globalenv(), ifnotfound = FALSE)))
    try(.ld_close(), silent = TRUE)
}, onexit = TRUE)

cat(strrep("=", 78), "\n", sep = "")
cat(sprintf("LOADING_ND3_v3.R  started %s\n", format(.ld_t0, "%Y-%m-%d %H:%M:%S")))
cat(sprintf("  log: %s\n", normalizePath(.ld_file, winslash = "/",
                                         mustWork = FALSE)))
cat(sprintf("  wd : %s\n", getwd()))
cat(strrep("=", 78), "\n\n", sep = "")



library(tidyverse)
library(haven)
# truncnorm: legacy import, no longer used (no random imputation
# anywhere in this loader; output is fully deterministic)

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;     filter    <- dplyr::filter
mutate    <- dplyr::mutate;     slice     <- dplyr::slice
recode    <- dplyr::recode;     rename    <- dplyr::rename
summarise <- dplyr::summarise;  summarize <- dplyr::summarize
arrange   <- dplyr::arrange;    count     <- dplyr::count
lag       <- dplyr::lag


# =====================================================================
# SETUP: WPI DATA FOR REAL WAGES  ABS A2705194A in 2023 dollars
# =====================================================================

# wpi_data <- data.frame(
#   wave = 15:23,
#   deflator = 143.0 / c(122.7, 125.1, 127.7, 130.6, 133.5, 135.4, 138.5, 143.2, 149.3)
# )


# WPI index (ABS A2705194A), base wave 24 = 149.6. Waves 15-24 are the
# verified values used throughout the project. Waves 12-14 are ESTIMATES
# back-cast at published WPI growth (2012: 3.6%, 2013: 2.9%, 2014: 2.5%)
# -- VERIFY against the ABS series and replace before a production run.
WPI_12_14_VERIFIED <- TRUE   # ABS A2705194A, FY index (year ended June), confirmed
wpi_data <- data.frame(
  wave     = 12:24,
  deflator = 149.6 / c(110.9, 114.6, 117.6,                       # W12-14: ABS-confirmed
                       120.4, 123.0, 125.4, 127.9, 130.9,
                       133.7, 135.7, 138.9, 143.7, 149.6)
)
if (!WPI_12_14_VERIFIED)
  warning(BUILD_TAG, ": WPI for waves 12-14 are back-cast ESTIMATES; ",
          "verify against ",
          "ABS A2705194A and set WPI_12_14_VERIFIED <- TRUE.", call. = FALSE)
# =====================================================================
# WAVE FILE LOCATION (shared by the processor and v7_discover)
# =====================================================================
# HILDA names embed the RELEASE, not the wave:
# Combined_<waveletter><release>0c.dta (wave 12 in Release 24 is
# Combined_l240c.dta). Match any release for this wave letter and take
# the most recent if several releases coexist in the folder.
find_wave_file <- function(wave_letter, wave_num = NULL) {
  cand <- list.files(data_dir,
                     pattern = paste0("^Combined_", wave_letter,
                                      "[0-9]{2,3}c\\.dta$"),
                     full.names = TRUE)
  if (length(cand)) sort(cand, decreasing = TRUE)[1] else NULL
}

# =====================================================================
# v7 DISCOVERY: FIND THE SELF-EMPLOYMENT AND BUSINESS-INCOME MNEMONICS
# =====================================================================
# Reads ONE wave file for structure only (n_max = 0), so it is fast and
# holds no data in memory. Reports candidates three ways because the
# label search alone proved insufficient: searching for "employment
# type", "self-employ", "employer", "own business" and "unpaid family
# worker" returned only RETROSPECTIVE items (previous job, last job),
# none of which is the current main job the audit needs. The current-job
# variable therefore carries a differently phrased label, so modes 2 and
# 3 enumerate by PREFIX instead: jbm for main-job attributes and es for
# employment-status derivations. Both sets are small enough to read in
# full.
v7_discover <- function(wave_letter = "x", wave_num = 24) {
  
  cat("\n=====================================================================\n")
  cat("v7 DISCOVERY MODE -- wave", wave_num, "(", wave_letter, ")\n")
  cat("=====================================================================\n")
  
  fp <- find_wave_file(wave_letter, wave_num)
  if (is.null(fp)) {
    cat("Wave file not found. Check data_dir.\n"); return(invisible(NULL))
  }
  cat("File:", basename(fp), "\n")
  
  # Structure only: no rows read.
  d <- haven::read_dta(fp, n_max = 0)
  labs <- vapply(d, function(x) {
    l <- attr(x, "label"); if (is.null(l)) NA_character_ else as.character(l)
  }, character(1))
  nm <- names(d)
  cat("Variables in file:", format(length(nm), big.mark = ","), "\n")
  
  show <- function(idx, title) {
    cat("\n---", title, "---\n")
    if (!length(idx)) { cat("  (none)\n"); return(invisible(NULL)) }
    out <- data.frame(variable = nm[idx],
                      label    = substr(labs[idx], 1, 88),
                      row.names = NULL, stringsAsFactors = FALSE)
    print(out, right = FALSE)
  }
  
  # ---- MODE 1: label search ----------------------------------------
  # Kept because it is the right first pass and it is what identified
  # the ehtse/ehto family correctly. Note the retrospective-only result
  # documented above: read the labels, do not assume the first hit is
  # the current job.
  pat_emp <- paste("employment type", "self.?employ", "employer",
                   "own business", "unpaid family worker",
                   sep = "|")
  show(grep(pat_emp, labs, ignore.case = TRUE), "MODE 1a: employment-type labels")
  
  pat_bus <- paste("business income", "unincorporated", "partnership",
                   "sole trader", "own business income",
                   sep = "|")
  show(grep(pat_bus, labs, ignore.case = TRUE), "MODE 1b: business-income labels")
  
  # ---- MODE 2: jbm prefix (main-job attributes) ---------------------
  # HILDA prefixes CURRENT MAIN JOB attributes with jbm. This is the set
  # to read when the label search returns only previous/last-job items.
  show(grep(paste0("^", wave_letter, "jbm"), nm),
       "MODE 2: all jbm-prefixed variables (CURRENT MAIN JOB attributes)")
  
  # ---- MODE 3: es prefix (employment-status derivations) ------------
  # esdtl is already mapped as the detailed employment-status DV; the
  # rest of the es family may carry an employee/self-employed split.
  show(grep(paste0("^", wave_letter, "es"), nm),
       "MODE 3: all es-prefixed variables (employment-status derivations)")
  
  # ---- MODE 4: income prefixes for the business component -----------
  # Financial-year income DVs cluster under bi (business income), and
  # HILDA often splits a component into positive and negative parts with
  # p / n suffixes. If a PAIR appears here, V7_BUSINC takes both names.
  show(grep(paste0("^", wave_letter, "(bi|bnc|scc)"), nm),
       "MODE 4: bi/bnc/scc-prefixed variables (business income components)")
  
  # ---- Value labels for a named employment-type variable ------------
  # EMPTYPE_EMPLOYEE defaults to c(1), which is a PLACEHOLDER. The CPQ
  # and PQ versions of this question number their categories
  # differently, so the coding must be read, not assumed.
  if (!is.na(V7_EMPTYPE)) {
    vn <- paste0(wave_letter, V7_EMPTYPE)
    cat("\n--- VALUE LABELS for", vn, "---\n")
    if (vn %in% nm) {
      vl <- attr(d[[vn]], "labels")
      if (is.null(vl)) cat("  (no value labels attached)\n")
      else print(data.frame(code = as.vector(vl), label = names(vl),
                            row.names = NULL), right = FALSE)
      cat("\n  Set EMPTYPE_EMPLOYEE to the code(s) meaning EMPLOYEE.\n")
      cat("  Currently EMPTYPE_EMPLOYEE =",
          paste(EMPTYPE_EMPLOYEE, collapse = ", "), "\n")
    } else {
      cat("  NOT FOUND in this wave. V7_EMPTYPE is wrong.\n")
    }
  }
  
  cat("\n=====================================================================\n")
  cat("NEXT STEPS\n")
  cat("  1. Identify the CURRENT employment-type variable, and READ ITS\n")
  cat("     VALUE LABELS before trusting it. Previous-job and last-job\n")
  cat("     items are not what the audit needs, and neither is jbmmply,\n")
  cat("     whose label reads plausibly but which codes SECTOR rather\n")
  cat("     than employment type. esempdt is the confirmed variable.\n")
  cat("  2. Identify FY business income. If it appears as a POSITIVE and\n")
  cat("     NEGATIVE pair, set V7_BUSINC to BOTH names. The loader NETS\n")
  cat("     them (positive minus negative) before deflating, because the\n")
  cat("     negative element holds MAGNITUDES rather than signed values.\n")
  cat("     Verify that on any new release: n(neg < 0) should be zero.\n")
  cat("  3. Paste names (no wave letter) into V7_EMPTYPE / V7_BUSINC.\n")
  cat("  4. Rerun discovery to print the value labels, then set\n")
  cat("     EMPTYPE_EMPLOYEE from them.\n")
  cat("  5. V7_DISCOVER <- FALSE, bump CHECKPOINT_VERSION, rebuild.\n")
  cat("=====================================================================\n")
  invisible(NULL)
}

# ---- v7 DISCOVERY GATE ----------------------------------------------
# Runs discovery and STOPS before any wave is processed. This is the
# whole point of the mode: it must not be possible to half-run the
# loader with unconfirmed mnemonics and get a panel out the other side.
if (isTRUE(V7_DISCOVER)) {
  v7_discover(wave_letter = letters[max(WAVES)], wave_num = max(WAVES))
  stop("V7_DISCOVER is TRUE: discovery complete, loader halted by design. ",
       "Paste the confirmed mnemonics into V7_EMPTYPE / V7_BUSINC, ",
       "confirm EMPTYPE_EMPLOYEE from the printed value labels, then set ",
       "V7_DISCOVER <- FALSE and rerun.", call. = FALSE)
}

# =====================================================================
# TWO HELPER FUNCTIONS FOR DIFFERENT VARIABLE TYPES
# =====================================================================

# Function 1: For variables where zero is INVALID (wages, hours, codes)
safe_numeric <- function(data, varname) {
  if(varname %in% names(data)) {
    val <- as.numeric(data[[varname]])
    val[val >= -10 & val <= -1] <- NA  # HILDA missing codes
    return(val)
  } else {
    return(rep(NA_real_, nrow(data)))
  }
}

# Function 1b: dates. safe_numeric() would destroy one, because a Stata
# date arrives either as a Date, as a numeric count of days from
# 1960-01-01, or as a character, and as.numeric() on the third gives NA
# while on the second it gives a plausible-looking integer that is not a
# year. So the type is established first and converted second.
#
# The HILDA missing codes are negative and a real interview date is not,
# so the same -10..-1 rule applies before conversion.
safe_date <- function(data, varname) {
  if (!varname %in% names(data)) return(rep(as.Date(NA), nrow(data)))
  x <- data[[varname]]
  if (inherits(x, "Date")) return(x)
  if (is.numeric(x) || (haven::is.labelled(x) && is.numeric(unclass(x)))) {
    v <- as.numeric(x)
    v[v >= -10 & v <= -1] <- NA
    # Stata epoch. A count of days from 1960 puts this window near 19,000
    # to 24,000; anything far outside that is not a Stata day count and is
    # left as NA rather than silently converted into a date in 1971.
    v[!is.na(v) & (v < 10000 | v > 30000)] <- NA
    return(as.Date(v, origin = "1960-01-01"))
  }
  ch <- trimws(as.character(x))
  ch[ch %in% c("", "-1", "-2", "-3", "-4", "-5", "-6", "-7", "-8", "-9",
               "-10")] <- NA
  for (f in c("%Y-%m-%d", "%d/%m/%Y", "%d%b%Y", "%m/%d/%Y", "%Y%m%d")) {
    d <- suppressWarnings(as.Date(ch, format = f))
    if (sum(!is.na(d)) > 0.5 * sum(!is.na(ch))) return(d)
  }
  as.Date(rep(NA, length(ch)))
}

# v21.14a. The date PARTS, extracted defensively.
#
# The v21.14 run failed here with format.default() receiving "%Y" as its
# trim argument, which means interview_date was not carrying the Date
# class at the point the next mutate argument read it. Every branch of
# safe_date() returns a Date when tested in isolation, and format is not
# masked anywhere in this file, so I cannot reproduce the cause and will
# not guess at it in code that has to run.
#
# So the dispatch is removed rather than relied on: this coerces first and
# formats second, and the callers below re-derive from the RAW column
# rather than from the intermediate. Five cheap passes over 23,000 rows
# instead of one, in exchange for not depending on what an intermediate
# mutate column carries.
.date_part <- function(d, fmt) {
  if (!inherits(d, "Date"))
    d <- suppressWarnings(as.Date(d, origin = "1970-01-01"))
  as.integer(format(d, fmt))
}

# And a one-line report of what hhhqivw actually IS, so the next run
# names the storage type instead of leaving it to inference.
.report_date_type <- local({
  .done <- FALSE
  function(data, varname) {
    if (.done || is.null(varname) || !varname %in% names(data)) return(invisible())
    x <- data[[varname]]
    cat(sprintf(paste("  hhhqivw storage: class=%s typeof=%s; first",
                      "non-missing value: %s\n"),
                paste(class(x), collapse = "/"), typeof(x),
                { nn <- x[!is.na(x)]
                if (!length(nn)) "none" else paste(utils::head(nn, 1)) }))
    .done <<- TRUE
    invisible()
  }
})

# Function 2: For variables where zero is VALID (counts, tenure, experience)
safe_numeric_zero <- function(data, varname) {
  if(varname %in% names(data)) {
    val <- as.numeric(data[[varname]])
    val[val >= -10 & val <= -1] <- NA  # HILDA missing codes only
    # Keep zeros as valid values
    return(val)
  } else {
    return(rep(NA_real_, nrow(data)))
  }
}

# Function 3 (v19): NA-PRESERVING "any of these flags is set".
# The v18 field-of-study indicators were built as
#   coalesce(edqobc_raw == 1, FALSE)
# which returns FALSE for every row when the source mnemonic is ABSENT
# from the release, because safe_numeric hands back an all-NA column.
# has_business_any therefore read 0 across all 134,131 person-waves and
# looked like a substantive finding rather than a missing variable, the
# same silent-NA failure as `ehtnl`. This helper returns NA where NO
# component is observed and 0/1 where at least one is, so an absent item
# is visible as missingness and is caught by the degenerate-column audit
# after combination.
flag_any_set <- function(..., set_code = 1) {
  m <- cbind(...)
  if (is.null(dim(m)) || ncol(m) == 0L) return(NA_real_)
  observed <- rowSums(!is.na(m)) > 0
  hit      <- rowSums(m == set_code, na.rm = TRUE) > 0
  ifelse(observed, as.numeric(hit), NA_real_)
}

# Function 4 (v21.48): one E12 aim item as 0/1. NA where the item itself
# is missing, and NA where the trainee answered don't know or refused to
# the whole battery. Before v21.48 those trainees scored 0 on every aim,
# which reads as "trained for none of these reasons" when the truth is
# "did not say". nonresponse is NA for anyone not put the battery, and
# coalesce() turns that NA into FALSE, so it never blanks an observed
# item. The == 1 rule is the one every other aim item uses.
aim_item <- function(raw, nonresponse) {
  ifelse(is.na(raw) | dplyr::coalesce(nonresponse == 1, FALSE),
         NA_real_, as.numeric(raw == 1))
}
# =====================================================================

# =====================================================================
# v17: NEW-VARIABLE REGISTRY, PRESENCE REPORTING, DEPRECATIONS
# =====================================================================
# Three things live here, all of them consequences of the same lesson:
# any_of() tolerates an absent name and hands back a silent all-NA
# column, which is how `ehtnl` survived two versions unnoticed. Every
# name added in v17 is therefore declared, checked per wave, and
# reported by name whether it lands or not.
#
#   1. V17_REPORT_STEMS  the names checked and printed for every wave.
#   2. *_STEMS candidate sets for items whose mnemonic is not verified
#      against this release. The FIRST stem that resolves is used and
#      the choice is printed; if none resolve the derived column is NA
#      and the presence table says so.
#   3. DEPRECATED_V17     old column names removed by the lebth fix.
#
# The wealth and time-use stems below are CANDIDATES, not confirmed.
# Run one wave, read the presence table, then narrow these vectors to
# the names that actually resolved. Do not let an unresolved candidate
# reach an estimate.

# --- Confirmed present in all 13 waves by 01_discovery_extended.R -----
V17_CONFIRMED_STEMS <- c(
  "lebth",                                   # SCQ B23e birth/adoption
  "lebthq1", "lebthq2", "lebthq3", "lebthq4", "lebthna",
  "leprgq1", "leprgq2", "leprgq3", "leprgq4", "leprgna",
  "nlreapm",                                 # not looking: pregnancy/maternity leave
  "edcqen", "edfts",                         # current enrolment, FT student (T9)
  "jttrcst", "jttrdsg", "jttrisc", "jttrpfj",# training cost and aims (T3)
  "jttrdk", "jttrrf",                        # aims: don't know, refused (v21.48)
  "jttrwrk",                                 # E6 module gate (v21.6)
  "jbmo62", "jbmo6s"                         # 2-digit ANZSCO, AUSEI06 status
)

# --- Candidate sets: mnemonic UNVERIFIED against this release --------
# v18: narrowed to the stems that resolved against the 240c release.
# lshrcod, lshrpc, lshrcare, lshrcnr, lshrelc, lshrodj, lshrgdn,
# pwsupei, pwsuppi, hwsuppi, hwtwi, hwdebi and hwtdti are absent from
# all thirteen waves. They are dropped rather than left in a candidate
# list where they resolve to nothing and leave a named all-NA column.
# Care for a disabled or elderly relative and outdoor tasks are simply
# not in this release, so no column claims to measure them.
#
# TWO time-use items resolved, lshrcar and lshrchd, and nothing in the
# release says which is own-child care. v18 takes lshrcar first purely
# because it came first in a vector, which is the assumed-coding
# failure this patch family exists to prevent. Both are now carried
# under their own mnemonic and the labelling is decided from the data
# after combination.
TIME_USE_STEMS   <- c("lshrcar", "lshrchd")
COMMUTE_STEMS    <- c("lshrcom")
VOLUNTEER_STEMS  <- c("lshrvol")
SUPER_STEMS      <- c("hwsupei")
JOBEXIT_STEMS <- c("jbmplej", "jbmploj", "jbmpgj", "capj", "alup")

# HILDA splits a signed wealth quantity into a non-negative positive
# part and a non-negative negative part; the quantity is the
# DIFFERENCE. v17 read the positive part alone and called it net
# worth, which overstates it for every indebted household.
NETWORTH_POS_STEMS <- c("hwnwip")
NETWORTH_NEG_STEMS <- c("hwnwin")

# v21.32. THE NON-CASH STEMS ARE CANDIDATES UNTIL A WAVE HAS BEEN READ.
# Table 4.24 of the Release 24 manual gives the main-job trio nbcm /
# nbcmi / nbcmf and the other-jobs trio nbco / nbcoi / nbcof, so those
# six are documented. nbcmany, nbcmcca and nbcmccr are named in
# REVISION_PLAN Stage 6 and are NOT in that table, so they are asserted
# and not yet verified against this release. Putting all nine in the
# presence report means the first run says which resolved, per wave,
# before anything downstream rests on one.
#
# This is the alop lesson applied in advance. alop was read as "days of
# paid parental leave", turned out to aggregate five leave types, and
# cost a rename at v21.24. A stem whose LABEL has not been read is a
# candidate whatever the plan calls it.
NONCASH_STEMS <- c("nbcm", "nbcmi", "nbcmf", "nbcmany",
                   "nbco", "nbcoi", "nbcof",
                   "nbcmcca", "nbcmccr")

# Discriminator thresholds for the own-child care labelling.
CARE_RATIO_MIN  <- 3    # winner: mean with a child under 5 over mean for never-parents
CARE_MARGIN_MIN <- 2    # and it must beat the other item by this factor

# Determination rule for the nlreapm reason code.
NLF_FEMALE_MIN <- 0.90
NLF_MIN_N      <- 50

# --- Firm-size instrument break (v19, diagnosed v20) -----------------
# HILDA swaps jbmemsz for jbmems2 at wave 22. The value labels printed
# by the W12-W24 run resolve the two frames completely:
#
#   jbmems2 1-4  (self, 2-4, 5-9, 10-19)  <->  jbmemsz 1  (under 20)
#   jbmems2 5-10 (20-99 ... 20,000+)      <->  jbmemsz 2-7
#   jbmems2 12,13 (DK <100, DK 100+)      <->  jbmemsz 8,9
#   jbmems2 11   (DK fewer than 20)       <->  no old counterpart
#
# The category ASSIGNMENTS in Step 4 are consistent across the break on
# this crosswalk. The break is therefore NOT a mapping error, which is
# what v19 assumed. It is COVERAGE. Wave 21 classifies 6,468 of 10,500
# employed; wave 22 classifies 10,335 of 10,426. On the harmonised
# frame, under-20 is 28.5% at wave 22 against 5.1% at wave 21, and
# still 20.8% after excluding the one-person category the old item
# never captured. A four-fold gap after the frames are aligned is not
# something a recode can repair.
#
# v21.28, FROM THE DICTIONARY. The instruction below is DISCHARGED and the
# inference was right. jbmemsz is labelled "Number of people employed at
# LOCATIONS THROUGHOUT AUSTRALIA", so it is a multi-site organisation size,
# and a respondent only reaches it after indicating the employer HAS
# multiple locations. Single-site employers, which is most small firms,
# never see the item. That is why the under-20 band is nearly empty before
# wave 22 and why no recode repairs it: the missing 40% are not missing at
# random, they are disproportionately the small firms.
#
# The dictionary also shows jbmemsz running W1-W21 and STOPPING. It does
# not become sparse at wave 22, it ends and jbmems2 begins. Saying so is
# cleaner than calling it a coverage break and makes the regime split
# unambiguous.
#
# What remains open is narrower than the note below: look for a WORKPLACE
# size item ("number employed at this location") asked of everyone rather
# than only multi-site employers. If none exists, firm size is a
# wave-22-onward variable and firm_size_comparable stays NA for good.
#
# The signature, 60% coverage of the employed with 70% of the
# classified in the largest bands, is a filtered question over-sampling
# multi-site organisations.
# v21.13. THE TRAINING GATE MOVES QUESTION POSITION AT WAVE 20.
#
# jttrwrk, the item every training column in this panel is gated on, is
# asked as PQ E1 in waves 7 to 19 and as PQ E6 in waves 20 to 24. It goes
# from being the FIRST item of module E to the sixth. This file has called
# it "the E6 module gate" since v21.6, which is true for five of the
# thirteen waves in the window.
#
# v21.48. NOT THE GATE ALONE. The Release 24 dictionary shows the whole
# module moving five questions later at wave 20, from E1-E7 to E6-E12,
# with jttrnsk added after the aims. So every training item changes
# position, and the five items now ahead of the module are not part of
# it. The context argument below applies to every training column.
#
# WHY IT MATTERS, and why it is not the same as the firm-size break. The
# population reaching the item does NOT change: training_module_asked runs
# 86.5% at wave 12 to 88.9% at wave 24 with no step at 20, so E1 to E5 are
# not routing anyone away. What changes is the CONTEXT. An item asked
# first is a different stimulus from the same item asked after five
# other items, and the answer can move without the routing moving.
#
# AND IT IS CONFOUNDED WITH THE PANDEMIC. Wave 20 is 2020. The stability
# audit in 04 flags six variables stepping at wave 20 and the natural
# reading was COVID. A questionnaire renumbering in the same wave is an
# alternative reading and this design cannot separate them: there is one
# wave-20 and both things happened in it.
#
# Declared rather than corrected, because there is nothing to correct. The
# flag lets a specification interact the training outcome with the regime,
# or restrict to one side, and forces the choice to be made rather than
# inherited.
TRAINING_GATE_POSITION_BREAK_WAVE <- 20L

# v21.14b. THE ORIGIN-OCCUPATION COVERAGE BREAK, at the same wave.
# pjoo62 and pjoo6s, the job-change route into occ2_origin, are absent
# from waves 20 to 24. Coverage of the coalesced column falls from about
# 6.8% of person-waves to about 3.1%, so the variable is observed for a
# materially different population either side. Declared as a regime rather
# than repaired, because there is nothing to repair: the item stops being
# collected.
OCC_ORIGIN_BREAK_WAVE <- 20L

FIRM_SIZE_BREAK_WAVE <- 22L
# v21: the single switch is split. Under v20 one flag stood for two
# separate questions and reading the value labels was enough to look
# like grounds for setting it, which would have opened
# firm_size_comparable on a series whose coverage still breaks. The
# frames are settled; the coverage is not, and cannot be by a recode.
FIRM_SIZE_FRAMES_VERIFIED     <- TRUE    # value labels read off the W12-W24 log
FIRM_SIZE_COVERAGE_COMPARABLE <- FALSE   # ~60% classified before, ~99% after
FIRM_SIZE_VERIFIED <- FIRM_SIZE_FRAMES_VERIFIED && FIRM_SIZE_COVERAGE_COMPARABLE
# Warn if the small-firm share moves by more than this across the break.
FIRM_SIZE_SHARE_TOL  <- 0.05

# --- v21 analysis-stage restrictions ---------------------------------
# Constants for the four remaining restrictions. They are declared here,
# beside the firm-size ones, so the whole set is visible in one place and
# 00_compat_v21.R can hold the same values without either copy being the
# original. The panel carries them as an attribute at save time.
#
# STUDY_ITEM_BREAK_WAVE. edcqen and edfts are on a different instrument
# at wave 24 and observed coverage goes from 3.6% to 12.3%, at the same
# time as a sample refresh. Wave 24 is therefore not on the same series
# as waves 12-23 for those two items, whatever the level does.
STUDY_ITEM_BREAK_WAVE        <- 24L
# Waves whose education COMPOSITION is a break rather than a trend.
EDUC_COMPOSITION_BREAK_WAVES <- 24L
# The wealth module inside this window. Anything wealth-valued outside
# these waves is a mapping error, not an observation.
WEALTH_MODULE_WAVES          <- c(14L, 18L, 22L)
# The ehto/ehtuj/ehtjb LEVELS are anchored on a retrospective report
# taken at panel entry, so they are comparable within a person and not
# across entry cohorts: 4.93 years out of the labour force for the
# original and 2011 intakes against 1.72 for the 2019-plus intake. The
# flows differences that out.
OLF_LEVEL_COMPARABLE         <- FALSE
# hwsupei is a household aggregate. Determined from the data rather than
# assumed; the partner-identical share is printed after combination and
# recorded in .v21_facts.
SUPER_IS_HOUSEHOLD           <- TRUE
# v21.31. Minimum observed components before precarious_score is defined.
# Three of five keeps the employed with an item non-response and excludes
# the non-employed, for whom none of the five is defined.
PRECARIOUS_MIN_COMPONENTS    <- 3L
# Orphan quarter flags tolerated as a note rather than a warning. Four
# are known on each item across W12-W24; the headroom is there so a real
# regression in the event filter still surfaces.
ORPHAN_QUARTER_MAX           <- 10L

# --- Columns to test for entrant routing (v21.4) ---------------------
# An NPQ item is put to entrants alone, so a column built on one is
# missing for everyone else and its counts are entrant counts wearing
# panel clothes. The label prefix is what nominates a column for this
# list; the cross-tabulation after combination is what decides.
ENTRANT_TEST_COLS <- c("enrolled_in_course",
                       "has_nursing_any", "has_nursing_strict",
                       "has_teaching_any",
                       "has_trade_any", "has_technical_any",
                       "years_npq_total",
                       # v21.14f. 01's routing table classifies ujljo62 as
                       # "DV via NPQ" and pjoto62/pjoo62 as "DV via CPQ".
                       # The NPQ is the NEW PERSON questionnaire, so the
                       # not-in-paid-work origin route is very likely
                       # entrant-routed while the other two are not, and
                       # occ2_origin coalesces all three.
                       #
                       # The wave-24 signature already fits: NPQ origins
                       # jump to 193 against 47-82 in waves 20-23, which is
                       # the refresher intake. That is the shape an
                       # entrant-routed item makes and I read it as a
                       # curiosity rather than a diagnosis.
                       #
                       # Tested rather than asserted. If the NPQ source is
                       # entrant-only, occ2_origin mixes an entrant column
                       # with two continuing ones and its per-wave counts
                       # are not comparable; occ_origin_source is what
                       # separates them.
                       "occ2_origin_nonemp", "occstat_origin_nonemp",
                       "occ2_origin_unemp", "occ2_origin_jobchange",
                       "occ2_origin")
# Above this share of continuing person-waves observed, the column is not
# routed. Below it, with entrants materially higher, it is.
ENTRANT_ROUTED_MAX_CONTINUING <- 10
ENTRANT_ROUTED_MIN_RATIO      <- 5

# v21.14g. A COALESCED COLUMN INHERITS ITS COMPONENTS' ROUTING, and the
# ratio test alone will not see it.
#
# occ2_origin came back "not routed" at ratio 4.4 against a threshold of
# 5, while occ2_origin_nonemp, one of the three sources it coalesces, is
# ENTRANT-ONLY at 12.5% against 0.0%. The parent is 4.4x more observed for
# entrants and missed the verdict by 0.6, because a mixture of one routed
# and two unrouted sources sits between the two answers and a binary test
# at a hard threshold picks a side.
#
# That is the same defect as the |z| < 1.96 cut in 04's sex-split block:
# a categorical verdict read off a continuous statistic at a boundary. The
# fix there was an indeterminate band. The fix here is better, because the
# STRUCTURE is known: declare which columns are coalesces of which, and a
# parent whose component is routed inherits the restriction whatever its
# own ratio says.
#
# Add a row when you add a coalesce. The check is only as complete as this
# list, which is why it prints what it checked as well as what it found.
COALESCED_PARENTS <- list(
  occ2_origin    = c("occ2_origin_unemp", "occ2_origin_nonemp",
                     "occ2_origin_jobchange"),
  occstat_origin = c("occstat_origin_unemp", "occstat_origin_nonemp",
                     "occstat_origin_jobchange"),
  occstat_change = c("occstat_origin_unemp", "occstat_origin_nonemp",
                     "occstat_origin_jobchange")
)

# --- Known wave shapes (v21.3) ---------------------------------------
# A column empty in some waves and populated in others is either a module
# item, which is a fact about the instrument, or a defect. Naming the
# shapes that are facts leaves a short list that is not. The names are
# the module's, the values are the waves inside THIS window.
KNOWN_WAVE_SHAPES <- list(
  "wealth module"        = c(14L, 18L, 22L),
  "fertility module"     = c(15L, 19L, 23L),
  "3-yearly (16/20/24)"  = c(16L, 20L, 24L),
  "pre-break only"       = 12:21,
  "post-break only"      = 22:24,
  # v21.7 NOTE. These two counted 202 and 205 columns on the v21.6 run
  # and will now count a handful or none. That is not a regression. They
  # were absorbing the wave-24 and wave-23 RAW SOURCE columns, which are
  # single-wave because they carry that wave's letter, and the source
  # classifier in the sweep now removes those before the shape match
  # runs. What is left here is a DERIVED column genuinely confined to
  # one wave, which is the thing the entry was meant to name.
  "wave 24 only"         = 24L,
  "wave 23 only"         = 23L,
  # v21.14c. pjoo62 and pjoo6s are collected in waves 12 to 19 and not
  # after, so every column resting on them carries this profile. Naming it
  # moves three columns out of the wave-hole sweep's unexplained list,
  # where they were correct findings the first time and noise thereafter.
  # occstat_change is here for a second reason as well: it needs an origin
  # AND a current occupation on one row, which only the job-change route
  # supplies, so it could not span thirteen waves even if pjoo* did.
  "job-change route (12-19)" = 12:19
)
# Printed cap for the unexplained list. The explained ones are counted
# rather than listed, since a module shape is not news.
WAVE_HOLE_PRINT_N <- 40L

# --- Columns that are EMPTY BY CONSTRUCTION (v21.1) ------------------
# An all-NA column is normally a missing variable. These are the
# exceptions: firm_size_comparable is empty on purpose, because the
# wave-22 coverage break cannot be recoded away, and counting it as a
# surprise in the degenerate warning dilutes the ones that are.
# has_business_any joins the list in v21.5: ASCED has no business field
# in the edqo* battery, so the column is empty because the CATEGORY does
# not exist, which is a different thing from a mnemonic that failed to
# resolve and should not be reported as one.
# v21.46b. firm_size_comparable STAYS ON THIS LIST, and the first attempt
# to remove it was wrong about WHEN the sweep runs. The sweep reads
# combined_data, which is upstream of the analysis-stage restrictions
# block where the column is filled, so at sweep time it is legitimately
# all-NA and always will be. Removing the exemption did not add a check;
# it added a false alarm on every run, which is the thing the exemption
# list exists to prevent.
#
# THE CHECK THAT WAS WANTED LIVES WHERE THE FILL DOES. An assertion
# immediately after the fill stops the build if the column is still empty
# there, which is the only place the question has a meaningful answer.
EXPECTED_EMPTY_COLS <- c(
  # v21.14c: bncpplh is absent from this release, so its derived column
  # cannot be anything but empty. Exempted so the sweep's warning counts
  # only columns whose emptiness is a surprise.
  "ppl_howanswered_cur", "firm_size_comparable", "has_business_any")
# v21.32. THE NON-CASH CONDITIONALS ARE NOT EXEMPTED HERE, and that is
# the choice rather than an omission. noncash_any, employer_childcare*
# and the other-jobs trio may legitimately be empty if nbcmany, nbcmcca,
# nbcmccr or nbco fail to resolve, so exempting them would be defensible.
# But this is their FIRST build: an all-NA column here is exactly the
# information the build is being run to obtain, and the sweep's warning
# is the fastest way to see it. Move any of them into this vector on the
# NEXT build, once the presence report has said which are genuinely
# absent from the release; leaving the exemption in place before that
# would suppress the finding on the one run that can produce it.

# --- Degenerate-column audit (v19) -----------------------------------
# Any derived indicator that is constant across the whole panel is
# almost always an absent mnemonic rather than a finding:
# has_business_any read exactly 0 across 134,131 person-waves because
# edqobc resolves to nothing and coalesce(NA == 1, FALSE) is FALSE.
# These columns are checked after combination and reported by name.
DEGENERATE_CHECK_COLS <- c(
  "has_nursing_any", "has_nursing_strict",
  "has_teaching_any", "has_business_any",
  "has_trade_any", "has_technical_any",
  "nlf_reason_maternity", "enrolled_in_course", "full_time_student",
  "training_self_funded", "training_self_funded_z",
  "training_aim_general", "training_aim_current",
  "training_aim_future", "in_wealth_module",
  # v21.48. Constant 0 here would mean jttrdk and jttrrf resolved and never
  # fired over thirteen waves, which is likelier a coding problem than a
  # population that always knew why it trained.
  "training_aim_nonresponse",
  # v21.6. jttpewt_any and jttpeot_any cannot be all-NA by construction,
  # so a mnemonic that fails to resolve shows up here as a constant zero
  # and nowhere else. training_module_asked constant at 1 means the gate
  # collapsed to its fallback and is not gating anything.
  "training_module_asked", "training_any_reported",
  "jttpewt_any", "jttpeot_any", "jttpewt_obs", "jttpeot_obs",
  "birth_undated", "preg_undated",
  # v21.32. Same reasoning as jttpewt_any: noncash_any_value cannot be
  # all-NA by construction, so a failure of nbcmi to resolve shows up as
  # a constant 0 and nowhere else. employer_childcare is on the list
  # because a constant here means the receipt item collapsed, and a
  # constant 0 on a rare item looks like a substantive finding.
  "noncash_any_value", "noncash_any", "employer_childcare",
  # v21.34. THE GATE ITSELF, for the reason training_module_asked is two
  # entries above: a gate cannot be all-NA, so its failure mode is a
  # CONSTANT and nothing else. If nbcmany does not resolve,
  # noncash_any_raw is empty, this column is 0 everywhere, and all three
  # _z twins below go all-NA together. None of the four is in
  # EXPECTED_NEW_COLUMNS, so the arrival contract would not stop either.
  # Constant at 1 is the opposite failure and equally worth the name:
  # it means the gate is admitting everyone and gating nothing, which
  # leaves the _z columns identical to the ungated ones under a name
  # that says they are not.
  "noncash_module_asked",
  "birth_quarter_orphan", "preg_quarter_orphan",
  # v21.9: renamed to earnings_total_includes_business and kept on the
  # watch list under the new name. It is constant at 1 BY CONSTRUCTION
  # and belongs in EXPECTED_CONSTANT_COLS below, not in a defect list;
  # it is watched here only so that a wave where the business mnemonic
  # FAILS to resolve reports by name instead of moving the constant
  # quietly to 0.98.
  "earnings_total_includes_business", "fy_business_income_nonzero",
  "fy_benefits", "real_fy_benefits", "total_unpaid_hours_capped"
)

# v21.9. THE SAME IDEA FOR CONSTANTS. EXPECTED_EMPTY_COLS exempts the
# columns that are all-NA by design; nothing exempted the columns that
# are CONSTANT by design, so all four of them arrived in the v21.8
# warning as defects. Every one is a provenance or reconciliation flag
# whose constant value is the result it exists to report:
#
#   earnings_total_includes_business  1 everywhere because the business
#       mnemonic resolves in every wave on this window, so
#       fy_earnings_total never fell back to wages alone. A 0 anywhere
#       here would be the finding.
#   earnings_source_flag  inc_salary_sacrifice everywhere because wsfes
#       exists from wave 10 and this panel starts at 12. Any other value
#       would mean a wave lost the salary-sacrifice-inclusive measure.
#   birth_q_declined_dated / preg_q_declined_dated  0 everywhere because
#       no person-wave both ticks the cannot-say box AND carries a
#       quarter. That is the v18 double-coding bug, and 0 is the proof it
#       is fixed. Non-zero here would reopen it.
#
# These are still SCANNED and still PRINTED; they are excluded from the
# warning count only. A constant that is expected is a passed test, and a
# passed test reported as a defect is how a warning stops being read.
# What is NOT on this list: anything whose constant value would be a
# surprise. Add a name here only with the reason it cannot vary.
EXPECTED_CONSTANT_COLS <- c(
  "earnings_total_includes_business", "earnings_source_flag",
  "birth_q_declined_dated", "preg_q_declined_dated",
  # v21.48a. The one entry that CAN vary, so it bends the rule above. On
  # the 1 October run no trainee refused the aim battery in 36,648
  # person-waves, and the flag is present in all thirteen waves, so its
  # 0 is a measured zero and the finding the column exists to report.
  # The defect the sweep exists to catch, an absent mnemonic, would show
  # as all-NA, which this list does not exempt.
  "training_aim_refused_raw"
)

# --- E6 against the place items (v21.7) ------------------------------
# jttrwrk is the gate question and the four place-and-time items are the
# follow-ups, so a mention of any follow-up implies a yes on the gate.
# The reverse does not hold: training can be reported at the gate and
# then placed nowhere the four items cover, which is a real answer and
# not a contradiction.
#
# The check reads the _raw columns, not the derived _any pair. The
# derived pair coalesces a missing follow-up to zero, which would turn
# every unanswered follow-up into apparent agreement and leave the check
# unable to fail.
TRAINING_GATE_RAW  <- "jttrwrk_raw"
TRAINING_PLACE_RAW <- c("jttpewt_raw", "jttpeot_raw",
                        "jttopwt_raw", "jttopot_raw")
# Share of ASKED person-waves that may carry the impossible pair before
# the loader complains. Survey data is not arithmetic and a handful of
# keying errors in 108,000 answers is expected; a percent of them is a
# routing or a coding problem, not respondent noise.
TRAINING_E6_CONTRADICTION_MAX <- 0.005

# Above this observed rate among the NON-EMPLOYED WHO WERE ASKED, the
# structural zero stops being a true zero and becomes a downward-biased
# imputation. jttpewt is a past-twelve-months item and employment is a
# status at interview, so a non-employed person-wave CAN legitimately
# carry workplace training; the question is how often, and it has to be
# measured rather than assumed.
TRAINING_STRUCTURAL_ZERO_MAX <- 0.01

# v21.48. Ceiling on the share of person-waves, among those where the aim
# battery's don't-know and refused flags are observed, that mention
# either. Not knowing or refusing to say what the training was for should
# be rare. A share near the whole battery means the == 1 rule is reading
# "not mentioned" as "mentioned", and the aim fix would then blank the
# aims for almost every trainee. The build stops above this.
TRAINING_AIM_NONRESPONSE_MAX <- 0.05

JOBEXIT_STEMS <- c("jbmplej", "jbmploj", "jbmpgj")   # v21.47: % chance items, C25-C27

V17_CANDIDATE_STEMS <- unique(c(TIME_USE_STEMS, COMMUTE_STEMS,
                                VOLUNTEER_STEMS, SUPER_STEMS,
                                NETWORTH_POS_STEMS, NETWORTH_NEG_STEMS,
                                NONCASH_STEMS, JOBEXIT_STEMS))

V17_REPORT_STEMS <- c(V17_CONFIRMED_STEMS, V17_CANDIDATE_STEMS)

# --- Quarter-flag coding: thresholds for the empirical determination --
# The rule: among respondents who report the event, exactly one quarter
# flag should be set. Whichever (event code, flag code) pair delivers
# that is the coding. Assuming 1 = yes is what broke T7 the first time.
Q_MIN_EVENTS    <- 25     # below this a wave is reported, not judged
Q_SHARE_MIN     <- 0.90   # winner must reach this share of one-flag cases
Q_MARGIN_MIN    <- 0.25   # and beat the runner-up by this much

# --- Deprecations introduced by the lebth fix -------------------------
DEPRECATED_V17 <- c(
  event_birth         = "event_pregnancy (leprg, SCQ B23d) or event_child (lebth, SCQ B23e)",
  had_birth_this_wave = "had_pregnancy_this_wave or had_child_this_wave"
)

# v21: hwsupei is a household aggregate, so a column called
# super_balance sitting beside person_id invites exactly the individual
# comparison the data cannot support. Renamed rather than annotated.
DEPRECATED_V21 <- c(
  super_balance = paste("hh_super_balance (hwsupei is a HOUSEHOLD aggregate;",
                        "there is no individual superannuation comparison)"),
  # v21.5: removed rather than repaired. ujljryr is absent from the
  # release, the nearest real item is NPQ and so entrant-only, and the
  # quantity is already in years_unemployed for about 95% of
  # person-waves.
  years_unemp_since_school = "years_unemployed (from the ehtuj History DV)"
)

# v21.9: the column is constant at 1 by construction and correctly so,
# but "has_business" reads as a fact about the person and records a fact
# about the measure. Renamed rather than annotated, for the same reason
# super_balance was: a misleading name sitting next to person_id gets
# read the wrong way faster than a comment gets read at all.
DEPRECATED_V219 <- c(
  earnings_total_has_business = paste(
    "earnings_total_includes_business (provenance: whether",
    "fy_earnings_total carries the business component at all, 1 on every",
    "person-wave in this window) OR fy_business_income_nonzero (the",
    "substantive test, about 8.3%). They are not the same question and",
    "the old name was ambiguous between them.")
)

# v21.24. The alop rename. paid_family_leave_days named a quantity the
# dictionary does not describe: alop aggregates maternity, paternity,
# bereavement, family AND carers leave, so four of its five components
# have nothing to do with a birth. Registered so a downstream script
# still asking for the old name is stopped rather than silently given
# NA by any_of().
DEPRECATED_V2124 <- c(paid_family_leave_days = "paid_nonannual_leave_days")

DEPRECATED_NAMES <- c(DEPRECATED_V17, DEPRECATED_V21, DEPRECATED_V219,
                      DEPRECATED_V2124)

# v20: the hard-coded call-site list is REMOVED. Every line number in
# it had drifted by three to five lines by the v19 rebuild, and it
# missed Pre_analysis_diagnostics2.R entirely (four sites). A list of
# line numbers maintained by hand alongside a grep that computes the
# same thing is a second source of truth that can only rot. The live
# scan at the end of this file is now the record.

check_deprecated_usage <- function(paths,
                                   deprecated = names(DEPRECATED_NAMES),
                                   halt = TRUE, roots = NULL) {
  # v21.1: report a path relative to the scan root rather than a
  # basename. The v21 log listed 00_load_hilda.R three times with three
  # sets of line numbers, because three directories hold a copy and
  # basename() collapsed them into one apparently self-contradictory row.
  .label <- function(p) {
    pn <- normalizePath(p, winslash = "/", mustWork = FALSE)
    for (rt in roots) {
      rtn <- normalizePath(rt, winslash = "/", mustWork = FALSE)
      if (nzchar(rtn) && startsWith(pn, rtn))
        return(sub("^/+", "", substring(pn, nchar(rtn) + 1L)))
    }
    pn
  }
  paths <- paths[file.exists(paths)]
  hits <- list()
  # v21: matching on a fixed string was safe while every deprecated name
  # was replaced by something that did not contain it. super_balance ->
  # hh_super_balance is not, and a fixed-string grep reports every
  # correct use of the new name as a violation. R identifiers are
  # letters, digits, dot and underscore, so those characters bound the
  # match on either side.
  .bound <- function(d)
    paste0("(^|[^A-Za-z0-9._])", gsub("([.\\\\])", "\\\\\\1", d),
           "($|[^A-Za-z0-9._])")
  for (p in paths) {
    ln <- readLines(p, warn = FALSE)
    for (d in deprecated) {
      i <- grep(.bound(d), ln)
      i <- i[!grepl("^\\s*#", ln[i])]
      if (length(i))
        hits[[length(hits) + 1L]] <- data.frame(
          file = .label(p), line = i, name = d, stringsAsFactors = FALSE)
    }
  }
  if (!length(hits)) {
    cat("  check_deprecated_usage: clean.\n"); return(invisible(NULL))
  }
  h <- do.call(rbind, hits)
  cat("\n  DEPRECATED COLUMN NAMES STILL REFERENCED:\n")
  print(h, row.names = FALSE)
  msg <- paste0(BUILD_TAG, ": scripts still reference removed columns. ",
                "Replace: ",
                paste(sprintf("%s -> %s", names(DEPRECATED_NAMES),
                              DEPRECATED_NAMES),
                      collapse = "; "))
  if (halt) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  invisible(h)
}

# --- Per-wave presence accumulator -----------------------------------
.v17_presence <- new.env(parent = emptyenv())
.v17_presence$rows <- list()
.v17_presence$qcode <- list()

# --- v21: facts determined inside process_all_waves and needed after it
# The superannuation household-versus-individual determination is made
# from the data in the post-combination diagnostics, which run inside
# the function. The restriction block outside it needs the answer, and
# an environment is the honest way to carry it rather than recomputing
# the same join twice and risking the two disagreeing.
.v21_facts <- new.env(parent = emptyenv())
.v21_facts$super_partner_identical <- NA_real_
.v21_facts$super_couple_waves      <- NA_integer_
# v21.1: which waves actually ran this session. Every per-wave audit is
# written by process_wave_complete(), and a wave restored from checkpoint
# never calls it, so without this the audits cannot tell "absent from the
# release" from "not looked at today".
.v21_facts$waves_processed          <- integer(0)
.v21_facts$waves_cached             <- integer(0)
# v21.45: set inside process_all_waves so the fingerprint block can name
# the cache state the panel was built under without re-deriving it.
.v21_facts$force_reextract          <- NA
.v21_facts$checkpoint_version       <- NA_character_

# --- First-resolving-candidate reader --------------------------------
# Returns the first candidate stem present in the data, cleaned by the
# usual HILDA missing-code rule, and records which stem was used.
pick_first_numeric <- function(data, v, stems, zero_valid = TRUE,
                               label = NULL) {
  for (st in stems) {
    nm <- v[[st]]
    if (!is.null(nm) && !is.na(nm) && nm %in% names(data)) {
      if (!is.null(label))
        .v17_presence$rows[[paste0("pick_", label)]] <- st
      return(if (zero_valid) safe_numeric_zero(data, nm)
             else safe_numeric(data, nm))
    }
  }
  rep(NA_real_, nrow(data))
}

# VARIABLE MAP (shared by the processor and the checkpoint hash)
# =====================================================================
# v10: extracted from process_wave_complete(). It is now defined ONCE
# and used in two places, so the map the loader reads and the map the
# checkpoint hash fingerprints cannot drift apart. Anything that
# changes WHICH variables are read therefore invalidates the cache
# automatically.
get_wave_vars <- function(wave_letter) {
  
  v <- list(
    
    capj    = paste0(wave_letter, "capj"),    # Calendar DV: % of last financial year in jobs
    
    # Demographics
    hgage = paste0(wave_letter, "hgage"),
    hgsex = paste0(wave_letter, "hgsex"),
    mrcurr = paste0(wave_letter, "mrcurr"),
    
    # Employment
    esdtl = paste0(wave_letter, "esdtl"),
    wscei = paste0(wave_letter, "wscei"),
    # v21.23. THE SALARY-SACRIFICE ASYMMETRY, and it runs the wrong way
    # for this paper.
    #
    # The loader reads wsfes for the FINANCIAL-YEAR measure, which
    # INCLUDES salary sacrifice, and wscei for the CURRENT one, which
    # does not. The hourly wage - the paper's main outcome - is built on
    # the current measure. So the two earnings concepts in this panel
    # disagree about whether sacrificed income is income.
    #
    # WHY IT IS NOT NEUTRAL HERE. Salary sacrifice is overwhelmingly
    # superannuation, concentrated in higher-paying and public-sector
    # jobs, and it is a CHOICE that households commonly stop making when
    # income falls. A mother who stops sacrificing at k = 0 shows a RISE
    # in wscei with no change in total compensation, so the birth-wave
    # coefficient is biased toward zero. That is a mechanical bias in the
    # direction that UNDERSTATES the penalty, which is the direction a
    # referee will not forgive.
    #
    # wsces is the same variable including sacrifice, waves 10-24, so it
    # spans this window entirely. Read as a SECOND column rather than a
    # replacement: the difference between them is the sacrificed amount,
    # which is itself measurable and worth reporting, and the published
    # series stays on wscei.
    wsces = paste0(wave_letter, "wsces"),   # current, all jobs, INC sacrifice
    #
    # And the imputation flags. The loader carries bnfpplh for parental
    # leave and has nothing equivalent for the MAIN OUTCOME. wscef flags
    # the base measure, wscsf the sacrifice-inclusive one, wsfef the
    # financial-year one. HILDA imputes a material share of wage values
    # and a design cannot report robustness to imputation without them.
    wscef = paste0(wave_letter, "wscef"),   # imputation flag, wscei
    wscsf = paste0(wave_letter, "wscsf"),   # imputation flag, wsces
    wsfef = paste0(wave_letter, "wsfef"),   # imputation flag, wsfei
    #
    # v21.32. THE THIRD COMPONENT OF COMPENSATION, and the only one this
    # loader had never read.
    #
    # Three things sit outside wscei. Business income is documented and
    # carried (5.20% of men's and 3.39% of women's person-waves have it
    # with zero wages). Salary sacrifice was fixed at v21.23. Employer
    # non-cash benefits were read nowhere, and the omission survived
    # three separate audits because nothing on the panel referred to
    # them, which is the failure mode a missing variable has and a wrong
    # one does not.
    #
    # nbcm is the DV weekly value of non-cash benefits in the CURRENT
    # MAIN JOB: motor vehicle, telephone, computer, car park,
    # low-interest loans, shares, superannuation, housing rent and child
    # care. nbcmi is the imputed version and nbcmf the flag, exactly the
    # pre/post/flag structure of Table 4.24, and all three run waves 10
    # to 24, so W12-W24 is covered end to end.
    #
    # WHY THE DIRECTION MATTERS. Motor vehicle, car park, telephone and
    # computer concentrate in higher-status jobs, which is where fathers
    # are. Omitting them plausibly UNDERSTATES the father-mother gap,
    # the same direction as the sacrifice bias v21.23 corrected. The
    # magnitude is unknown and could be small; until this build nothing
    # measured it, which is the part that is not defensible.
    nbcm    = paste0(wave_letter, "nbcm"),    # weekly value, main job, pre-imp
    nbcmi   = paste0(wave_letter, "nbcmi"),   # same, imputed
    nbcmf   = paste0(wave_letter, "nbcmf"),   # imputation flag
    # nbcmany is the has-any indicator per REVISION_PLAN Stage 6. It is
    # NOT in the manual's Table 4.24, so it is asserted rather than
    # verified; noncash_any_value below derives the same margin from the
    # DV and does not depend on it resolving.
    nbcmany = paste0(wave_letter, "nbcmany"), # has any non-cash benefit
    #
    # THE BASE MISMATCH, which the Stage 6 list does not close. wscei is
    # ALL JOBS and nbcm is the MAIN JOB, so wscei + nbcmi is a numerator
    # assembled from two different bases. That is the same class of
    # error as reading tifditp without tifditn: not censoring, a wrong
    # value. nbco is the other-jobs counterpart with the same trio, so
    # the three are read and weekly_compensation uses the sum.
    #
    # It is a SMALL correction - a second job with non-cash benefits is
    # rare - and it is read anyway, because the alternative is a column
    # called compensation that quietly is not one.
    nbco    = paste0(wave_letter, "nbco"),    # weekly value, other jobs
    nbcoi   = paste0(wave_letter, "nbcoi"),   # same, imputed
    nbcof   = paste0(wave_letter, "nbcof"),   # imputation flag
    #
    # EMPLOYER CHILD CARE: the one item in this batch that is directly on
    # topic rather than a compensation correction. Amount and receipt, a
    # labour-supply constraint measured at the individual level, and the
    # thing that should move around a birth if anything here does.
    #
    # Its cash value is also where the valuation is weakest. A dollar of
    # employer child care relaxes the recipient's constraint set by more
    # than a dollar of wages does, so the amount is a LOWER BOUND on the
    # effect and the receipt indicator is the cleaner object. State that
    # as a limitation; do not try to correct it with an equivalence
    # scale, which is a household-poverty instrument and has no place in
    # an individual wage differential.
    nbcmcca = paste0(wave_letter, "nbcmcca"), # employer child care, amount
    nbcmccr = paste0(wave_letter, "nbcmccr"), # employer child care, receipt
    jbhruc = paste0(wave_letter, "jbhruc"),  # Hours/week usually worked, ALL JOBS
    
    # Financial year income (for FTB simulator)
    wsfei = paste0(wave_letter, "wsfei"),   # FY gross wages & salary
    # wsfes is the SAME wages & salary including salary sacrifice, NOT
    # self-employment income. Available from wave 10 onward only, so the
    # construction below falls back to wsfei where it is absent.
    wsfes = paste0(wave_letter, "wsfes"),   # FY wages & salary inc salary sacrifice
    ccactci = paste0(wave_letter, "ccactci"),   # annual childcare cost
    
    # ---- v2: broader income aggregates ----
    # Needed for (i) the earnings-based event study, where the outcome
    # must be defined at zero for the non-employed, and (ii) the FTB-B
    # primary/secondary income test, which is on total income and not
    # on wages alone.
    bnfip   = paste0(wave_letter, "bnfip"),   # FY Australian public transfers
    # v21.26. THE p/n PAIR RULE, WHICH THIS LOADER ALREADY APPLIES TO
    # BUSINESS INCOME AND DID NOT APPLY HERE.
    #
    # HILDA User Training 2025, slide 46, and the manual section 3.11:
    # variables that can legitimately go negative are supplied as TWO
    # columns, a positive one and a negative one holding the ABSOLUTE
    # value, and the true figure is p MINUS n. The manual's Figure 4.7
    # states it for exactly these: "FY disposable total income
    # (=_tifditp - _tifditn)", "FY gross regular income (= _tifefp -
    # _tifefn)".
    #
    # This file gets it right for bifip/bifin and says so at length in
    # its own header: summing would record a $7,125 loss as +$7,125. It
    # then read tifditp, tifefp and hifditp alone.
    #
    # THE CONSEQUENCE IS NOT CENSORING, IT IS A WRONG VALUE. A person
    # with -$20,000 disposable income has tifditp = 0 and tifditn =
    # 20000, and the loader recorded zero. I wrote in v21.25 that these
    # were "positive values only" and therefore censored; the dictionary
    # label says "Positive values" because it is the positive HALF of a
    # pair, not because negatives were dropped. The true value is
    # recoverable from a column sitting next to it.
    #
    # This matters here specifically: self-employment among mothers
    # trebles after the birth, and a loss-making business is precisely
    # the case that goes negative.
    tifditn = paste0(wave_letter, "tifditn"),
    tifefn  = paste0(wave_letter, "tifefn"),
    hifditn = paste0(wave_letter, "hifditn"),
    tifditp = paste0(wave_letter, "tifditp"), # FY total income, disposable, +ve
    tifefp  = paste0(wave_letter, "tifefp"),  # FY total income, gross, +ve
    hifditp = paste0(wave_letter, "hifditp"), # HH FY disposable income, +ve
    # v21.27. THE FOURTH ONE. hifefp is the household counterpart of
    # tifefp and its own comment said "+ve", which is the reading the
    # training deck rules out: it is the positive HALF of a pair, and
    # Figure 4.7 gives the household model the same p/n structure as the
    # person-level one. An audit of every p-suffix mnemonic the loader
    # maps found this as the only remaining case with an unread twin
    # among the genuine pairs; the other p endings in the map (alop,
    # lesep, hhwtrp and so on) are coincidental letters, not suffixes.
    hifefn  = paste0(wave_letter, "hifefn"),
    hifefp  = paste0(wave_letter, "hifefp"),  # HH FY gross regular, p half
    
    # ---- v3: employment-history decomposition ----
    # HILDA's history stocks partition time since leaving full-time
    # education:
    #   ehtse  time since leaving FT education   [THE DENOMINATOR]
    #   ehtjb  time in paid work                 (already mapped above)
    #   ehtuj  time unemployed AND looking for work
    #   ehto   time not working and NOT looking (the OLF component)
    #   ehttot NPQ:A28 manual total, cross-check only
    # Identity: ehtse ~ ehtjb + ehtuj + ehto. ehtse is NOT a
    # self-employment stock; the "se" is "since education". There is no
    # self-employment variable in this family.
    # ehto is the margin that matters for mothers: their interruption is
    # overwhelmingly out-of-labour-force time, not unemployment. Using
    # ehtuj alone measures unemployment, which is only weakly gendered
    # and produced a misleadingly ungendered interruption rate in v2.
    #
    # Provenance caveat: the source items are NPQ (A14c1/A14c2, with
    # D3c1/D3c2 at wave 1 and A26/A28 at the wave 11, 19 and 24
    # refreshers), i.e. a retrospective self-report taken at panel entry
    # and then rolled forward as a History variable. Recall error in the
    # base stock is a person-level constant, absorbed by person fixed
    # effects, but it does contaminate cross-person LEVEL comparisons and
    # differs by entry cohort. Prefer the within-person year-on-year
    # CHANGES built below (d_olf_years etc.) over the levels.
    ehtse   = paste0(wave_letter, "ehtse"),   # Time since FT education (DENOM)
    ehtuj   = paste0(wave_letter, "ehtuj"),   # Total years unemployed, looking
    ehto    = paste0(wave_letter, "ehto"),    # Total years not working, not looking
    ehttot  = paste0(wave_letter, "ehttot"),  # Total years since leaving FT education
    alup    = paste0(wave_letter, "alup"),    # Days of unpaid leave, last 12 months (W5+)
    # v21.5: ujljryr removed. Absent from the release, and the quantity
    # is already in years_unemployed from the ehtuj History DV.
    
    # ---- v6: main-job hours (all-jobs hours is jbhruc, mapped above) ----
    # The second-job margin is jbhruc - jbmhruc. Substantively worth
    # watching: a mother taking a second job after first birth is a
    # flexibility response that total hours conceals.
    jbmhruc = paste0(wave_letter, "jbmhruc"), # Hours per week, MAIN job
    
    # Education - Main
    edhigh1 = paste0(wave_letter, "edhigh1"),
    edhists = paste0(wave_letter, "edhists"),
    edsscmp = paste0(wave_letter, "edsscmp"),  # CPQ school completion
    
    # Education - Completed (edqro*)
    edqrodc = paste0(wave_letter, "edqrodc"),  # Doctorate completed
    edqroms = paste0(wave_letter, "edqroms"),  # Masters completed
    edqrogd = paste0(wave_letter, "edqrogd"),  # Graduate diploma completed
    edqrogc = paste0(wave_letter, "edqrogc"),  # Graduate certificate completed
    edqrohd = paste0(wave_letter, "edqrohd"),  # Honours bachelor completed
    edqrobd = paste0(wave_letter, "edqrobd"),  # Bachelor completed
    edqroad = paste0(wave_letter, "edqroad"),  # Associate degree completed
    edqroav = paste0(wave_letter, "edqroav"),  # Advanced diploma completed
    edqrodp = paste0(wave_letter, "edqrodp"),  # Diploma completed
    edqrodn = paste0(wave_letter, "edqrodn"),  # Diploma nursing completed
    edqroc4 = paste0(wave_letter, "edqroc4"),  # Certificate IV completed
    edqroc3 = paste0(wave_letter, "edqroc3"),  # Certificate III completed
    edqroc2 = paste0(wave_letter, "edqroc2"),  # Certificate II completed
    edqroc1 = paste0(wave_letter, "edqroc1"),  # Certificate I completed
    edqrocd = paste0(wave_letter, "edqrocd"),  # Certificate unknown level completed
    edqrosh = paste0(wave_letter, "edqrosh"),  # Secondary school highest completed
    edqrosl = paste0(wave_letter, "edqrosl"),  # Secondary school lower completed
    
    # Education - Obtained (edqo*)
    edqodc = paste0(wave_letter, "edqodc"),   # Doctorate obtained
    edqoms = paste0(wave_letter, "edqoms"),   # Masters obtained
    edqogd = paste0(wave_letter, "edqogd"),   # Graduate diploma obtained
    edqogc = paste0(wave_letter, "edqogc"),   # Graduate certificate obtained
    edqohd = paste0(wave_letter, "edqohd"),   # Honours bachelor obtained
    edqobd = paste0(wave_letter, "edqobd"),   # Bachelor obtained
    edqoad = paste0(wave_letter, "edqoad"),   # Associate degree obtained
    edqoav = paste0(wave_letter, "edqoav"),   # Advanced diploma obtained
    edqodp = paste0(wave_letter, "edqodp"),   # Diploma obtained
    edqodn = paste0(wave_letter, "edqodn"),   # Diploma nursing obtained
    edqoc4 = paste0(wave_letter, "edqoc4"),   # Certificate IV obtained
    edqoc3 = paste0(wave_letter, "edqoc3"),   # Certificate III obtained
    edqoc2 = paste0(wave_letter, "edqoc2"),   # Certificate II obtained
    edqoc1 = paste0(wave_letter, "edqoc1"),   # Certificate I obtained
    edqocd = paste0(wave_letter, "edqocd"),   # Certificate unknown level obtained
    edqota = paste0(wave_letter, "edqota"),   # Trade certificate obtained
    edqotc = paste0(wave_letter, "edqotc"),   # Technical certificate obtained
    edqonq = paste0(wave_letter, "edqonq"),   # Nursing qualification obtained
    edqotq = paste0(wave_letter, "edqotq"),   # Teaching qualification obtained
    # v21.5: edqobc removed. ASCED has no business field in the edqo*
    # battery, so this was never going to resolve. See the header.
    edqosh = paste0(wave_letter, "edqosh"),   # Secondary school highest obtained
    edqosl = paste0(wave_letter, "edqosl"),   # Secondary school lower obtained 
    
    # Nursing-specific qualifications
    ednrsdc = paste0(wave_letter, "ednrsdc"), # Nursing doctorate
    ednrsms = paste0(wave_letter, "ednrsms"), # Nursing masters
    ednrsgd = paste0(wave_letter, "ednrsgd"), # Nursing graduate diploma
    ednrsbd = paste0(wave_letter, "ednrsbd"), # Nursing bachelor
    ednrshd = paste0(wave_letter, "ednrshd"), # Nursing honours
    ednrsav = paste0(wave_letter, "ednrsav"), # Nursing advanced diploma
    ednrsdp = paste0(wave_letter, "ednrsdp"), # Nursing diploma
    ednrsc3 = paste0(wave_letter, "ednrsc3"), # Nursing certificate III
    ednrstd = paste0(wave_letter, "ednrstd"), # Registered Nurse qualification
    
    # Teaching-specific qualifications  
    edtchdc = paste0(wave_letter, "edtchdc"), # Teaching doctorate
    edtchms = paste0(wave_letter, "edtchms"), # Teaching masters
    edtchgd = paste0(wave_letter, "edtchgd"), # Teaching graduate diploma
    edtchgc = paste0(wave_letter, "edtchgc"), # Teaching graduate certificate
    edtchhd = paste0(wave_letter, "edtchhd"), # Teaching honours
    edtchbd = paste0(wave_letter, "edtchbd"), # Teaching bachelor
    edtchts = paste0(wave_letter, "edtchts"), # Trained Secondary Teachers Cert
    edtchtp = paste0(wave_letter, "edtchtp"), # Primary Teaching Cert
    edtchdp = paste0(wave_letter, "edtchdp"), # Teaching diploma
    edtchav = paste0(wave_letter, "edtchav"), # Teaching advanced diploma
    
    #Training
    jttdays  = paste0(wave_letter, "jttdays"),   # Number of days attended training course
    jtthrs   = paste0(wave_letter, "jtthrs"),    # Average number of training hours per course day
    jttnum   = paste0(wave_letter, "jttnum"),    # Number of different training courses attended last 12 months
    jttopot  = paste0(wave_letter, "jttopot"),   # Job-related training at some other place - in your own time
    jttopwt  = paste0(wave_letter, "jttopwt"),   # Job-related training at some other place - during paid work time
    jttpeot  = paste0(wave_letter, "jttpeot"),   # Job-related training at place of employment - in your own time
    jttpewt  = paste0(wave_letter, "jttpewt"),    # Job-related training at place of employment - during paid work time
    # v21.6. E6, the gate the whole module hangs off. Everything above is
    # asked only of respondents who answer this one, so its missingness
    # is the difference between "did not train" and "was never asked",
    # and without it the seven items above cannot tell the two apart.
    # Thirteen waves, per 01_discovery_extended.R.
    jttrwrk  = paste0(wave_letter, "jttrwrk"),   # E6 Taken part in any work related training in past 12 months
    
    # Experience and tenure
    ehtjb = paste0(wave_letter, "ehtjb"),
    jbempt = paste0(wave_letter, "jbempt"),
    jbocct = paste0(wave_letter, "jbocct"),
    
    # Job characteristics
    jbcasab = paste0(wave_letter, "jbcasab"),
    jbmhl = paste0(wave_letter, "jbmhl"),
    jbmsl = paste0(wave_letter, "jbmsl"),
    jbmsch = paste0(wave_letter, "jbmsch"),
    jbmday = paste0(wave_letter, "jbmday"),
    jbhrcpr = paste0(wave_letter, "jbhrcpr"),
    jbpmfhr = paste0(wave_letter, "jbpmfhr"),
    jbmtuea = paste0(wave_letter, "jbmtuea"),
    jbmmply = paste0(wave_letter, "jbmmply"),
    jbmemsz = paste0(wave_letter, "jbmemsz"),
    jbmems2 = paste0(wave_letter, "jbmems2"),
    jbptrea = paste0(wave_letter, "jbptrea"),
    
    # Occupation and Industry
    jbmo61 = paste0(wave_letter, "jbmo61"),
    jbmi61 = paste0(wave_letter, "jbmi61"),
    edpsfdn = paste0(wave_letter, "edpsfdn"),
    
    # Children - household counts (original)
    hhd0_4 = paste0(wave_letter, "hhd0_4"),
    hhd5_9 = paste0(wave_letter, "hhd5_9"),
    hhd1014 = paste0(wave_letter, "hhd1014"),
    hhd1524 = paste0(wave_letter, "hhd1524"),
    ncchk = paste0(wave_letter, "ncchk"),
    ncany = paste0(wave_letter, "ncany"),
    
    # ---- NEW: Fertility history (tc* prefix) ----
    # tchad: History variable — total children ever had (bio + adopted)
    #   Available most waves as History or PQ/NPQ. Absorbing: once > 0, stays > 0.
    tchad   = paste0(wave_letter, "tchad"),     # History: Total children ever had
    tchadn  = paste0(wave_letter, "tchadn"),    # Total children ever had (NPQ version)
    tchave  = paste0(wave_letter, "tchave"),     # Number of children (CPQ version)
    tchch   = paste0(wave_letter, "tchch"),      # Check G1: has ever had children (fertility module waves)
    
    # Own children counts (DV — available ALL waves)
    tcr     = paste0(wave_letter, "tcr"),        # DV: Number of own resident children
    tcnr    = paste0(wave_letter, "tcnr"),       # DV: Number of own non-resident children
    tcyng   = paste0(wave_letter, "tcyng"),      # DV: Age youngest own child (excl step/foster/grand)
    tcdied  = paste0(wave_letter, "tcdied"),     # History: Total children since died
    
    # Own resident children by age group (DV — all waves)
    tcr04   = paste0(wave_letter, "tcr04"),      # Resident own + step/foster/grand aged 0-4
    tcr514  = paste0(wave_letter, "tcr514"),     # Same, aged 5-14
    tcr1524 = paste0(wave_letter, "tcr1524"),    # Same, aged 15-24
    tcr25   = paste0(wave_letter, "tcr25"),      # Same, aged 25+
    
    # Own non-resident children by age group (DV — all waves)
    # v21.5: tcn*, not tcnr*. The total IS tcnr, above, but HILDA drops
    # the r for the age bands. All four are thirteen waves at 74.9%.
    tcn04   = paste0(wave_letter, "tcn04"),      # Non-resident own children aged 0-4
    tcn514  = paste0(wave_letter, "tcn514"),     # Same, aged 5-14
    tcn1524 = paste0(wave_letter, "tcn1524"),    # Same, aged 15-24
    tcn25   = paste0(wave_letter, "tcn25"),      # Same, aged 25+
    
    # Step/foster/grandchildren
    rcstepn = paste0(wave_letter, "rcstepn"),    # N resident step/foster/grandchildren, no parent in HH
    
    # Child ages (for computing age at first birth)
    rcage1  = paste0(wave_letter, "rcage1"),     # Age of resident child 1
    rcage2  = paste0(wave_letter, "rcage2"),     # Age of resident child 2
    rcage3  = paste0(wave_letter, "rcage3"),     # Age of resident child 3
    ncage1  = paste0(wave_letter, "ncage1"),     # Age of non-resident child 1
    ncage2  = paste0(wave_letter, "ncage2"),     # Age of non-resident child 2
    
    # ---- NEW: Parenting attitudes ----
    paresp  = paste0(wave_letter, "paresp"),     # Has parenting responsibilities, children <= 17
    pahard  = paste0(wave_letter, "pahard"),      # "Being a parent is harder than I thought"
    patird  = paste0(wave_letter, "patird"),      # "I often feel tired/exhausted from children's needs"
    patrap  = paste0(wave_letter, "patrap"),      # "I feel trapped by my responsibilities as a parent"
    pashare = paste0(wave_letter, "pashare"),     # "Do fair share of looking after children"
    
    # ---- NEW: Pregnancy (fertility module waves: 15, 19, 23 in our panel) ----
    ftcpg   = paste0(wave_letter, "ftcpg"),      # Currently pregnant
    ftcppg  = paste0(wave_letter, "ftcppg"),     # Partner currently pregnant
    ftipg   = paste0(wave_letter, "ftipg"),      # Check if self or partner currently pregnant
    ftcpgm  = paste0(wave_letter, "ftcpgm"),     # Expected birth month
    ftcpgy  = paste0(wave_letter, "ftcpgy"),     # Expected birth year
    
    # ---- NEW: Other parent / partner info ----
    ncesop  = paste0(wave_letter, "ncesop"),     # Employment status of the other parent
    prlpli  = paste0(wave_letter, "prlpli"),     # Living with partner when last interviewed
    hhrhid  = paste0(wave_letter, "hhrhid"),     # Household ID (changes when people move)
    hhpxid  = paste0(wave_letter, "hhpxid"),     # Partner's cross-wave person ID (direct link)
    
    # ---- NEW: Job satisfaction & workplace quality ----
    # v21.5: jbmsall, not josat. "C33f Overall job satisfaction",
    # thirteen waves, asked of the employed.
    jbmsall = paste0(wave_letter, "jbmsall"),     # Overall job satisfaction
    jompf   = paste0(wave_letter, "jompf"),       # "I get paid fairly for things I do"
    jomini  = paste0(wave_letter, "jomini"),      # "My job requires me to take initiative"
    jomsf   = paste0(wave_letter, "jomsf"),       # "I have a secure future in my job"
    jomwf   = paste0(wave_letter, "jomwf"),       # "I worry about the future of my job"
    jomls_say = paste0(wave_letter, "jomls"),     # "I have a lot of say about what happens on my job"
    jbnewjs = paste0(wave_letter, "jbnewjs"),     # Looked for new job in last 4 weeks
    jbmplej = paste0(wave_letter, "jbmplej"),   # C25 % chance leave job voluntarily, 12 months
    jbmploj = paste0(wave_letter, "jbmploj"),   # C26 % chance lose job, 12 months
    jbmpgj  = paste0(wave_letter, "jbmpgj"),    # C27 % chance find and accept a job at least as good
    
    # ---- NEW: Life-satisfaction domains (SCQ, all waves; 0-10 scale) ----
    # Used for the FTB-B IV income-effect placebo (Part G robustness):
    # losat is the overall measure; the domain items are off-channel
    # placebos, except losatfs (financial) which is the income-channel
    # diagnostic, and losateo/losatft which are labour-channel.
    losat   = paste0(wave_letter, "losat"),       # Overall life satisfaction
    losateo = paste0(wave_letter, "losateo"),     # Employment opportunities
    losatfs = paste0(wave_letter, "losatfs"),     # Financial situation
    losatft = paste0(wave_letter, "losatft"),     # Amount of free time
    losathl = paste0(wave_letter, "losathl"),     # The home you live in
    losatlc = paste0(wave_letter, "losatlc"),     # Feeling part of local community
    losatnl = paste0(wave_letter, "losatnl"),     # The neighbourhood
    losatsf = paste0(wave_letter, "losatsf"),     # How safe you feel
    losatyh = paste0(wave_letter, "losatyh"),     # Your health
    
    # ---- NEW: Life events (asked annually — all waves) ----
    leprg   = paste0(wave_letter, "leprg"),      # Life event: pregnancy/birth of new child
    lemar   = paste0(wave_letter, "lemar"),      # Life event: got married
    lesep   = paste0(wave_letter, "lesep"),      # Life event: separated from spouse
    lejob   = paste0(wave_letter, "lejob"),      # Life event: changed jobs
    lejlf   = paste0(wave_letter, "lejlf"),      # Life event: fired/made redundant
    lemvd   = paste0(wave_letter, "lemvd"),      # Life event: moved residence
    
    # ---- v17: the birth event, its quarter flags, and the T-series ----
    # HILDA carries two life events here. leprg is SCQ B23d, Pregnancy.
    # lebth is SCQ B23e, Birth or adoption of a new child. The loader
    # read leprg and called the column event_birth. Both are now
    # carried under their own names; see DEPRECATED_V17.
    lebth    = paste0(wave_letter, "lebth"),
    lebthq1  = paste0(wave_letter, "lebthq1"),
    lebthq2  = paste0(wave_letter, "lebthq2"),
    lebthq3  = paste0(wave_letter, "lebthq3"),
    lebthq4  = paste0(wave_letter, "lebthq4"),
    lebthna  = paste0(wave_letter, "lebthna"),   # reported, quarter declined
    leprgq1  = paste0(wave_letter, "leprgq1"),
    leprgq2  = paste0(wave_letter, "leprgq2"),
    leprgq3  = paste0(wave_letter, "leprgq3"),
    leprgq4  = paste0(wave_letter, "leprgq4"),
    leprgna  = paste0(wave_letter, "leprgna"),
    
    # T-series arrivals: T1c leave, T9 study substitution, T3 aims/cost
    nlreapm  = paste0(wave_letter, "nlreapm"),   # not looking: pregnancy/maternity leave
    # -----------------------------------------------------------------
    # CANDIDATES FOR THE NEXT REBUILD (v21.2). Confirmed present in all
    # thirteen waves by 01_discovery_extended.R revision 3 unless noted.
    # Adding any one of them re-reads every wave, so they go in TOGETHER
    # or not at all. Nothing here is in the map yet.
    #
    #   edccenr  CPQ:A17b currently enrolled. The continuing-person
    #            counterpart to edcqen below; see the routing test in
    #            the restriction block. Read these two together or the
    #            enrolment series describes new entrants only.
    #   edcqtyp  CPQ:A17c/NPQ:A22 studying full or part time.
    #   wscplre  whether paid parental leave was included in the wages
    #   wsfplre  already reported, and in FY income. These two resolve
    #            an ambiguity INSIDE the existing outcome rather than
    #            adding a covariate, which makes them the highest value
    #            of the set for the earnings design.
    #   jowppml  workplace entitlement: paid maternity leave
    #   jowpppl  workplace entitlement: paid paternity leave
    #   bncppl   PPL received, latest payment amount, and
    #   bncpplw  weeks covered; bnfppl and bnfpplw are the FY versions.
    #   alop     DV: days of paid family leave in the last 12 months.
    #   jttrhgs  the six E12 training-aim branches not yet read:
    #   jttrmps  get started in the job, maintain professional status,
    #   jttrhsc  health and safety, other, don't know, refused.
    #   jttros
    #   lfmew*   weeks and months employed per financial year, 2000/01
    #   lfmem*   to 2012/13, eleven waves, with lfneh* the unemployment
    #            arm. The only part-year history in the release, and a
    #            BASE STOCK item: the battery ENDS at 2012/13, which is
    #            wave 13 of this window, so it cannot touch the FTE
    #            weight for waves 14 to 24. CPQ-routed, so missing for
    #            entrants, and its labels carry three variants each;
    #            confirm the financial YEAR does not move across them
    #            before pooling anything.
    #
    # Added after the revision 5 and 6 discovery runs. Comments only, so
    # the map hash is untouched and no rebuild is triggered by reading
    # this.
    #
    #   ehtseha  NPQ:A27a how the time since leaving education was
    #            answered, in years or in months. The provenance flag for
    #            years_since_education, which is the denominator of
    #            olf_share. The experience path already reports
    #            pct_mincer_proxy by sex and parenthood on exactly this
    #            logic; the denominator has no equivalent, and a
    #            systematic difference in how men and recent leavers
    #            answer is measurement sitting inside a ratio the design
    #            leans on. NPQ-routed, which for a base-stock provenance
    #            flag is the right shape: one observation, in the wave
    #            the stock was reported.
    #   jowpptw  SCQ:D3c workplace entitlement, permanent part-time. The
    #            third item of the D3 battery whose other two are already
    #            listed above. Availability rather than take-up, so it
    #            speaks to the hours margin rather than to leave.
    #
    #   THE ORIGIN OCCUPATION OF A MOVE. jbmo62 and jbmo6s are already
    #   read and give the CURRENT job at two digits and on AUSEI06. What
    #   the panel cannot see is where a mover came FROM, which is the
    #   quantity the occupational-downgrade claim is about. Both are
    #   conditional on a transition, so the sample is not the panel, and
    #   the A6 coding shift still forces a wave interaction.
    #
    #   pjoo62   CPQ:C52 previous job where the job changed, two digits
    #   pjoo6s   and AUSEI06. Eight waves.
    #   ujljo62  NPQ:D29 last job where not currently in paid work, two
    #   ujljo6s  digits and AUSEI06. Thirteen waves, but DV-via-NPQ, so
    #            check the routing before treating it as a full series.
    # -----------------------------------------------------------------
    edcqen   = paste0(wave_letter, "edcqen"),    # currently enrolled in a course
    edfts    = paste0(wave_letter, "edfts"),     # DV: full-time student
    jttrcst  = paste0(wave_letter, "jttrcst"),   # contributed to the cost
    jttrdsg  = paste0(wave_letter, "jttrdsg"),   # aim: develop skills generally
    jttrisc  = paste0(wave_letter, "jttrisc"),   # aim: improve skills, current job
    jttrpfj  = paste0(wave_letter, "jttrpfj"),   # aim: prepare for a future job
    # v21.21. THE REST OF THE E12 AIM BATTERY. 01's discovery run found
    # six further branches present in all thirteen waves and the loader
    # read four. The three substantive ones are added here; jttros,
    # jttrdk and jttrrf are other/don't-know/refused and carry no aim.
    jttrhgs  = paste0(wave_letter, "jttrhgs"),   # aim: help getting started
    jttrmps  = paste0(wave_letter, "jttrmps"),   # aim: maintain professional status
    jttrhsc  = paste0(wave_letter, "jttrhsc"),   # aim: health and safety
    # v21.48. Don't know and refused on the same battery, read so that a
    # trainee who could not say what the training was for is missing on
    # the aims rather than zero on all six. jttros, "other", stays unread:
    # a zero on a listed aim is correct for a trainee whose aim was
    # something else.
    jttrdk   = paste0(wave_letter, "jttrdk"),    # aims: don't know
    jttrrf   = paste0(wave_letter, "jttrrf"),    # aims: refused
    
    # Occupation below one digit: the residual is attributed to lost
    # occupational progression while occupation is measured at eight
    # categories. jbmo62 is 2-digit ANZSCO, jbmo6s the AUSEI06 score.
    jbmo62   = paste0(wave_letter, "jbmo62"),
    jbmo6s   = paste0(wave_letter, "jbmo6s"),
    
    # ---- v21.12 BATCH ONE: the section-8 additions -------------------
    # Added together, in ONE rebuild, because the cache invalidates on the
    # map and a second pass would cost thirteen more waves. Every column
    # they produce is declared in EXPECTED_NEW_COLUMNS, so an omission at
    # any of the five silent edit sites stops the build by name.
    #
    # NOTHING HERE IS RECODED ON FAITH. The binary items arrive as _raw
    # and a coding audit after combination reports their code sets per
    # wave, the way the training items are handled. Read that block before
    # any of these enters a specification.
    #
    # (a) IS PAID PARENTAL LEAVE ALREADY INSIDE THE EARNINGS MEASURE?
    #     The highest-value pair in the whole discovery output, because it
    #     resolves an ambiguity INSIDE the outcome rather than adding a
    #     covariate. If PPL sits inside wscei or wsfes for some mothers
    #     and outside for others, earnings at k = 0 is not one quantity
    #     across mothers and part of the birth-wave coefficient is a
    #     reporting convention.
    wscplre  = paste0(wave_letter, "wscplre"),   # F30d PPL inside current wages
    wsfplre  = paste0(wave_letter, "wsfplre"),   # F61e PPL inside FY income
    #
    # v21.15. THE MANUAL SAYS I MAPPED THE WRONG HALF OF THIS FAMILY.
    #
    # HILDA-User-Manual-Release-24.0, Table 4.24 and the salary-sacrifice
    # table, gives the convention for exactly this kind of indicator:
    #   _sscmrei, _sscmref   salary sacrifice reported earlier, current
    #                        main job [imputed] (post-imputed, flag)
    #   _ssfarei, _ssfaref   the same, financial year
    # and the imputation section states it outright: "The Nearest
    # Neighbour Regression method (using logistic regression) was then
    # used to impute WHETHER SOMEONE INCLUDED THESE AMOUNTS in their
    # reporting of wages and salaries."
    #
    # So the "reported earlier" family runs base / base+i / base+f, and
    # wscplre and wsfplre are the PRE-IMPUTED members. That is why they
    # came back observed on 0.1-0.2% and 0.7-1.5% of person-waves: the
    # pre-imputed version is non-missing only where the respondent
    # actually answered, and HILDA imputes the rest precisely because
    # the raw item is so sparse.
    #
    # The post-imputed versions are what a design should read, with the
    # flag beside them so imputed cases can be excluded or split. This is
    # the item I called the reason not to defer the batch, and I mapped
    # the sparse half of it.
    wscplrei = paste0(wave_letter, "wscplrei"),  # post-imputed
    wscplref = paste0(wave_letter, "wscplref"),  # imputation flag
    wsfplrei = paste0(wave_letter, "wsfplrei"),
    wsfplref = paste0(wave_letter, "wsfplref"),
    #
    # (b) LEAVE AGAINST SEPARATION AT THE BIRTH WAVE. A mother on paid
    #     leave and still attached to her employer is a different
    #     observation from one who separated, and the panel cannot
    #     currently tell them apart. This is what turns the
    #     employed-or-on-leave estimand from a proxy on
    #     nlf_reason_maternity into a measured one.
    jowppml  = paste0(wave_letter, "jowppml"),   # SCQ:D3a paid maternity entitlement
    jowpppl  = paste0(wave_letter, "jowpppl"),   # SCQ:D3b paid paternity entitlement
    bncppl   = paste0(wave_letter, "bncppl"),    # F30a PPL received, current
    bncppla  = paste0(wave_letter, "bncppla"),   # F30b amount of latest payment
    bncpplw  = paste0(wave_letter, "bncpplw"),   # F30c weeks that payment covered
    bnfppl   = paste0(wave_letter, "bnfppl"),    # F61a PPL received, financial year
    bnfppla  = paste0(wave_letter, "bnfppla"),   # F61c annual amount
    # v21.13. THE HOW-ANSWERED FLAG, and it should have been in the first
    # batch. HILDA's income block carries a companion for each receipt
    # item recording whether the value was SELF-REPORTED, imputed, or
    # derived. ppl_received_fy landed on 13.3% of person-waves, roughly
    # ten times what the birth rate supports, and an imputation-inclusive
    # receipt flag is the likeliest explanation. Without this column the
    # rate cannot be diagnosed, only doubted.
    bnfpplh  = paste0(wave_letter, "bnfpplh"),   # F61cd how answered, FY
    # v21.14. THE INTERVIEW DATE, and it does more than date a birth.
    #
    # Every reference-period item in this panel is anchored on it. The
    # training module asks about "the past 12 months", so two respondents
    # in the same wave interviewed five months apart are answering about
    # windows that overlap by seven months and differ by five. That has
    # been invisible.
    #
    # It also bears on wave 20. The E1-to-E6 move and the pandemic are
    # confounded there, and this adds a third thing that can be MEASURED
    # rather than argued: if wave-20 fieldwork ran later or longer than
    # usual, then the twelve-month window it asks about covers a
    # different period, and part of the wave-20 step is neither the
    # questionnaire nor behaviour but the calendar.
    hhhqivw  = paste0(wave_letter, "hhhqivw"),   # HQ date of interview
    bncpplh  = paste0(wave_letter, "bncpplh"),   # how answered, current
    bnfpplw  = paste0(wave_letter, "bnfpplw"),   # F61b weeks received
    # v21.24. alop IS NOT PARENTAL LEAVE. The dictionary reads:
    #   "DV: Days of paid (maternity, paternity, bereavement, family,
    #    carers) leave in last 12 months"
    # Five leave types in one aggregate, four of which have nothing to do
    # with a birth. It was mapped as "DV: days of paid family leave" and
    # selected as paid_family_leave_days, which is closer to right than
    # the parental-leave reading the v21.12 batch note gave it, but a
    # design that treats it as a birth-related quantity is counting
    # bereavement and carer's leave alongside.
    #
    # Kept, because total paid leave taken is a real covariate and the
    # name now says what it is. NOT usable as a parental-leave measure:
    # bncppl and bnfppl are that, they are verified, and 8.3 has them.
    #
    # THE COMPONENTS EXIST FOR PART OF THE WINDOW. aloppl separates
    # "maternity or paternity or parental" from the rest, but only in
    # waves 20-22, and alppwk gives paid parental leave WEEKS in waves
    # 23-24. Neither spans W12-W24, so neither can carry an event study,
    # and a design that used aloppl would be estimating on three waves
    # without saying so.
    alop     = paste0(wave_letter, "alop"),      # DV: days of ALL paid
    # non-annual, non-sick leave
    #
    # (c) THE ORIGIN OCCUPATION OF A MOVE, per the note above. Three pairs
    #     because the same concept is put to the two questionnaires and to
    #     job-changers separately; read the CPQ and NPQ pair together,
    #     since the union covers the non-employed in both groups. ALL are
    #     conditional on a transition, so the sample is not the panel, and
    #     the wave-22 coding shift forces an occupation-by-wave
    #     interaction on any of them.
    pjoto62  = paste0(wave_letter, "pjoto62"),   # CPQ:D30 prev job if unemployed
    pjoto6s  = paste0(wave_letter, "pjoto6s"),   # ... AUSEI06 status
    ujljo62  = paste0(wave_letter, "ujljo62"),   # NPQ:D29 last job if not in work
    ujljo6s  = paste0(wave_letter, "ujljo6s"),   # ... AUSEI06 status
    pjoo62   = paste0(wave_letter, "pjoo62"),    # CPQ:C52 prev job if job changed
    pjoo6s   = paste0(wave_letter, "pjoo6s"),    # ... AUSEI06 status. Eight waves.
    
    # ---- v17 CANDIDATES: time use beyond housework, and wealth -------
    # Mnemonics NOT verified against this release. Mapped so the
    # presence report resolves them; derived columns take the first
    # candidate that lands. Narrow these after reading the report.
    lshrcod  = paste0(wave_letter, "lshrcod"),
    lshrpc   = paste0(wave_letter, "lshrpc"),
    lshrcar  = paste0(wave_letter, "lshrcar"),
    lshrchd  = paste0(wave_letter, "lshrchd"),
    lshrcare = paste0(wave_letter, "lshrcare"),
    lshrcnr  = paste0(wave_letter, "lshrcnr"),
    lshrelc  = paste0(wave_letter, "lshrelc"),
    lshrodj  = paste0(wave_letter, "lshrodj"),
    lshrgdn  = paste0(wave_letter, "lshrgdn"),
    lshrcom  = paste0(wave_letter, "lshrcom"),
    lshrvol  = paste0(wave_letter, "lshrvol"),
    # Wealth module runs every four waves: 14, 18 and 22 inside 12-24.
    pwsupei  = paste0(wave_letter, "pwsupei"),
    hwsupei  = paste0(wave_letter, "hwsupei"),
    pwsuppi  = paste0(wave_letter, "pwsuppi"),
    hwsuppi  = paste0(wave_letter, "hwsuppi"),
    hwnwip   = paste0(wave_letter, "hwnwip"),
    hwnwin   = paste0(wave_letter, "hwnwin"),
    hwtwi    = paste0(wave_letter, "hwtwi"),
    hwdebi   = paste0(wave_letter, "hwdebi"),
    hwtdti   = paste0(wave_letter, "hwtdti"),
    
    # Housework and care
    lshrhw = paste0(wave_letter, "lshrhw"),
    
    # ---- NEW: Social capital / networking ----
    lsclub  = paste0(wave_letter, "lsclub"),     # Active member of sporting/community/professional org (1=Yes)
    lssocal = paste0(wave_letter, "lssocal"),    # Social gathering frequency (1=every day ... 7=never)
    
    # Flexibility
    jomflex = paste0(wave_letter, "jomflex"),
    jowpfx = paste0(wave_letter, "jowpfx"),
    
    # Commute
    jbmlkr = paste0(wave_letter, "jbmlkr"),
    
    # Parental background
    fmmsch = paste0(wave_letter, "fmmsch"),
    fmfsch = paste0(wave_letter, "fmfsch"),
    
    # ---- v7: self-employment and business income ----
    # Both resolve to NA_character_ until discovery has been run, in
    # which case safe_numeric returns NA columns and the derived
    # variables are NA throughout. A NAMED but ABSENT mnemonic is a
    # different matter and halts the loader (see the resolution check
    # after read_dta).
    emptype = if (is.na(V7_EMPTYPE)) NA_character_
    else paste0(wave_letter, V7_EMPTYPE),
    # v9: INDEPENDENT cross-check on the employment-status measure.
    # F42, "work in own business/farm/silent partner/trust beneficiary
    # LAST FINANCIAL YEAR". Two things make it useful rather than
    # redundant: it is a different question, so disagreement with
    # esempdt is informative rather than a coding error; and it is on a
    # last-financial-year basis, which matches bifip/bifin where
    # esempdt is CURRENT status. Someone self-employed last year but an
    # employee now has business income and no current self-employment.
    bifhave = paste0(wave_letter, "bifhave"),
    businc  = if (all(is.na(V7_BUSINC))) NA_character_
    else paste0(wave_letter, V7_BUSINC),
    
    # Geography and weights
    hhstate = paste0(wave_letter, "hhstate"),
    hhsos = paste0(wave_letter, "hhsos"),
    hhwtrps = paste0(wave_letter, "hhwtrps"),
    hhwtrp = paste0(wave_letter, "hhwtrp"),
    lnwtrp = paste0(wave_letter, "lnwtrp")    # DV: Responding person longitudinal weight
  )
  
  v
}


# =====================================================================
# MAIN PROCESSING FUNCTION WITH ALL VARIABLES
# =====================================================================

process_wave_complete <- function(wave_num, data_dir, wpi_data) {
  
  wave_letter <- letters[wave_num]
  year_val <- 2000 + wave_num
  cat("\n===========================================\n")
  cat("Processing Wave", wave_num, "(", wave_letter, ") - Year", year_val, "\n")
  cat("===========================================\n")
  
  # Find file. HILDA names embed the RELEASE, not the wave:
  # Combined_<waveletter><release>0c.dta (e.g. wave 12 in Release 24 is
  # Combined_l240c.dta). Match any release for this wave letter and take
  # the most recent if several releases coexist in the folder.
  file_path <- find_wave_file(wave_letter, wave_num)
  if (!is.null(file_path)) cat("  Found file:", basename(file_path), "\n")
  
  if(is.null(file_path)) {
    cat("  ERROR: No file found for wave", wave_num, "\n")
    return(NULL)
  }
  
  # (Data read moved to AFTER the variable map `v` below, so only the
  #  columns this script actually uses are loaded from the ~6,000-column
  #  Combined file. See "Load data (column-restricted)".)
  
  # Get WPI deflator for this wave
  wave_deflator <- wpi_data$deflator[wpi_data$wave == wave_num]
  
  # ============================
  # COMPLETE VARIABLE MAPPING
  # ============================
  
  # v10: the map is built by the shared get_wave_vars() so the
  # checkpoint hash fingerprints exactly what the loader reads.
  v <- get_wave_vars(wave_letter)
  
  # ============================
  # STEP 1: CORE VARIABLES
  # ============================
  
  cat("  Step 1: Core variables...\n")
  
  # Load data (column-restricted): reading only the mapped columns plus the
  # cross-wave id cuts read time and peak memory by roughly an order of
  # magnitude versus reading the full Combined file. any_of() tolerates
  # variables absent in a given wave; the safe_numeric helpers already
  # return NA columns for those, so downstream behaviour is unchanged.
  needed_cols <- unique(c("xwaveid", unlist(v)))
  needed_cols <- needed_cols[!is.na(needed_cols)]
  raw <- haven::read_dta(file_path, col_select = dplyr::any_of(needed_cols))
  # v21.14a: report what hhhqivw IS, once, so the next run names the
  # storage type instead of leaving it to be inferred from a failure.
  .report_date_type(raw, v$hhhqivw)
  cat("  Loaded:", format(nrow(raw), big.mark = ","), "observations,",
      ncol(raw), "of", length(needed_cols), "mapped columns\n")
  
  # ---- v19 FULL-MAP PRESENCE AUDIT ---------------------------------
  # The v17 presence report covers 27 of the ~228 mapped names. The
  # other 201 load through any_of() and, when absent, become all-NA
  # columns that no diagnostic ever names. Every wave was already
  # reporting 28-33 unmapped columns without saying WHICH. Record the
  # absent names here; the panel-level audit after combination prints
  # the ones absent in EVERY wave, which is the set that can only
  # produce empty derived columns.
  .absent_now <- setdiff(needed_cols, names(raw))
  .v17_presence$mapabsent[[paste0("w", wave_num)]] <- data.frame(
    wave = wave_num, mnemonic = .absent_now,
    stem = sub(paste0("^", wave_letter), "", .absent_now),
    stringsAsFactors = FALSE
  )
  if (length(.absent_now))
    cat(sprintf("  Mapped but ABSENT from this file: %d (%s%s)\n",
                length(.absent_now),
                paste(utils::head(.absent_now, 12), collapse = ", "),
                if (length(.absent_now) > 12) ", ..." else ""))
  
  # ---- v7 HARD RESOLUTION CHECK ------------------------------------
  # any_of() tolerates absent variables, which is what allowed `ehtnl`
  # to load as an all-NA column and go unnoticed for two versions. For
  # the v7 mnemonics that tolerance is removed: if a name has been SET
  # and is not in the file, stop. An UNSET name (NA_character_) is fine
  # and yields NA columns, so the loader still builds before discovery
  # has been run. It is a WRONG name, not a missing one, that halts it.
  v7_named <- c(emptype = v$emptype, businc = v$businc)
  v7_named <- v7_named[!is.na(v7_named)]
  if (length(v7_named)) {
    v7_absent <- v7_named[!(v7_named %in% names(raw))]
    if (length(v7_absent)) {
      stop("v7 mnemonic(s) named but ABSENT from ", basename(file_path), ": ",
           paste(sprintf("%s -> '%s'", names(v7_absent), v7_absent),
                 collapse = "; "),
           ". Rerun with V7_DISCOVER <- TRUE and confirm the name. ",
           "Set the constant to NA_character_ to build without it.",
           call. = FALSE)
    }
    cat("  v7 mnemonics resolved:",
        paste(v7_named, collapse = ", "), "\n")
  }
  
  # ---- v17 NEW-MNEMONIC PRESENCE REPORT (per wave, printed) --------
  # Not a stop. A wave legitimately missing an item should degrade, not
  # halt. But it must be visible: eight of 187 mnemonics were already
  # absent from this release without anyone noticing.
  .v17_row <- data.frame(
    wave = wave_num,
    stem = V17_REPORT_STEMS,
    name = vapply(V17_REPORT_STEMS,
                  function(st) { nm <- v[[st]]
                  if (is.null(nm)) NA_character_ else as.character(nm) },
                  character(1)),
    stringsAsFactors = FALSE
  )
  .v17_row$present <- !is.na(.v17_row$name) & .v17_row$name %in% names(raw)
  .v17_row$nonmissing <- vapply(seq_len(nrow(.v17_row)), function(i) {
    if (!.v17_row$present[i]) return(NA_real_)
    mean(!is.na(safe_numeric(raw, .v17_row$name[i])))
  }, numeric(1))
  .v17_presence$rows[[paste0("w", wave_num)]] <- .v17_row
  
  cat(sprintf("  v19 presence, wave %d: %d of %d new mnemonics present\n",
              wave_num, sum(.v17_row$present), nrow(.v17_row)))
  for (i in seq_len(nrow(.v17_row))) {
    cat(sprintf("    %-9s %-10s %-7s %s\n",
                .v17_row$stem[i],
                ifelse(is.na(.v17_row$name[i]), "-", .v17_row$name[i]),
                ifelse(.v17_row$present[i], "PRESENT", "ABSENT"),
                ifelse(is.na(.v17_row$nonmissing[i]), "",
                       sprintf("non-missing %5.1f%%",
                               100 * .v17_row$nonmissing[i]))))
  }
  .v17_conf <- .v17_row[.v17_row$stem %in% V17_CONFIRMED_STEMS, ]
  if (any(!.v17_conf$present))
    cat(sprintf("    NOTE: %d confirmed-present mnemonic(s) missing this wave: %s\n",
                sum(!.v17_conf$present),
                paste(.v17_conf$stem[!.v17_conf$present], collapse = ", ")))
  
  # ---- v17 QUARTER-FLAG CODING, DETERMINED RATHER THAN ASSUMED -----
  # The flags are four separate binaries, not one categorical. Among
  # respondents who report the event, exactly one flag should be set.
  # Search the 2x2 grid of (event code, flag code) and take the pair
  # that delivers it. Halt if a wave with enough events yields none:
  # deriving a quarter under the wrong code is the T7 failure again.
  .q_families <- list(
    lebth = c("lebthq1", "lebthq2", "lebthq3", "lebthq4"),
    leprg = c("leprgq1", "leprgq2", "leprgq3", "leprgq4")
  )
  q_code <- list()
  for (fam in names(.q_families)) {
    ev_nm  <- v[[fam]]
    fl_nms <- unlist(v[.q_families[[fam]]], use.names = FALSE)
    fl_ok  <- !is.na(fl_nms) & fl_nms %in% names(raw)
    if (is.null(ev_nm) || is.na(ev_nm) || !(ev_nm %in% names(raw)) ||
        sum(fl_ok) < 4L) {
      q_code[[fam]] <- list(event = NA_integer_, flag = NA_integer_,
                            share = NA_real_, n = 0L)
      cat(sprintf("    [quarter] %s: event or flags absent; coding undetermined\n",
                  fam))
      next
    }
    ev <- safe_numeric(raw, ev_nm)
    fl <- vapply(fl_nms, function(n) safe_numeric(raw, n),
                 numeric(nrow(raw)))
    grid <- expand.grid(event = c(1L, 2L), flag = c(1L, 2L),
                        KEEP.OUT.ATTRS = FALSE)
    grid$n <- NA_integer_
    grid$share_one <- NA_real_
    for (i in seq_len(nrow(grid))) {
      sel <- which(!is.na(ev) & ev == grid$event[i])
      grid$n[i] <- length(sel)
      if (!length(sel)) next
      k <- rowSums(fl[sel, , drop = FALSE] == grid$flag[i], na.rm = TRUE)
      grid$share_one[i] <- mean(k == 1L)
    }
    cat(sprintf("    [quarter] %s coding grid, wave %d:\n", fam, wave_num))
    for (i in seq_len(nrow(grid)))
      cat(sprintf("        event==%d, flag==%d : n=%6d  one-flag share=%s\n",
                  grid$event[i], grid$flag[i], grid$n[i],
                  ifelse(is.na(grid$share_one[i]), "   n/a",
                         sprintf("%6.3f", grid$share_one[i]))))
    elig <- grid[!is.na(grid$share_one) & grid$n >= Q_MIN_EVENTS, ]
    if (!nrow(elig)) {
      q_code[[fam]] <- list(event = NA_integer_, flag = NA_integer_,
                            share = NA_real_, n = max(grid$n))
      cat(sprintf("        too few events (max n=%d < %d); reported, not judged\n",
                  max(grid$n), Q_MIN_EVENTS))
      next
    }
    elig <- elig[order(-elig$share_one), ]
    best <- elig[1, ]
    runner <- if (nrow(elig) > 1) elig$share_one[2] else 0
    if (best$share_one < Q_SHARE_MIN ||
        (best$share_one - runner) < Q_MARGIN_MIN) {
      stop("v17 quarter-flag coding could not be determined for '", fam,
           "' in wave ", wave_num, ". Best pair (event==", best$event,
           ", flag==", best$flag, ") gives a one-flag share of ",
           sprintf("%.3f", best$share_one), " against a runner-up of ",
           sprintf("%.3f", runner), ", failing the thresholds (share >= ",
           Q_SHARE_MIN, ", margin >= ", Q_MARGIN_MIN,
           "). Inspect the grid above before deriving any quarter.",
           call. = FALSE)
    }
    q_code[[fam]] <- list(event = best$event, flag = best$flag,
                          share = best$share_one, n = best$n)
    cat(sprintf("        determined: event==%d is yes, flag==%d is set (share %.3f, n=%d)\n",
                best$event, best$flag, best$share_one, best$n))
  }
  .v17_presence$qcode[[paste0("w", wave_num)]] <- data.frame(
    wave = wave_num, family = names(q_code),
    event_code = vapply(q_code, function(z) z$event, integer(1)),
    flag_code  = vapply(q_code, function(z) z$flag,  integer(1)),
    one_flag_share = vapply(q_code, function(z) z$share, numeric(1)),
    n_events = vapply(q_code, function(z) as.integer(z$n), integer(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )
  
  # Event yes-codes. The determination above is preferred; the life-event
  # family's documented 2 = yes is the fallback when a wave carried too
  # few events to judge. A DISAGREEMENT is printed, never absorbed.
  .yes_lebth <- if (!is.na(q_code$lebth$event)) q_code$lebth$event else 2L
  .yes_leprg <- if (!is.na(q_code$leprg$event)) q_code$leprg$event else 2L
  .set_lebth <- if (!is.na(q_code$lebth$flag))  q_code$lebth$flag  else NA_integer_
  .set_leprg <- if (!is.na(q_code$leprg$flag))  q_code$leprg$flag  else NA_integer_
  if (!is.na(q_code$lebth$event) && q_code$lebth$event != 2L)
    cat("    [quarter] WARNING: lebth yes-code determined as",
        q_code$lebth$event, "not the documented 2.\n")
  if (!is.na(q_code$leprg$event) && q_code$leprg$event != 2L)
    cat("    [quarter] WARNING: leprg yes-code determined as",
        q_code$leprg$event, "not the documented 2.\n")
  
  wave_data <- raw %>%
    mutate(
      # Identifiers
      person_id = as.character(xwaveid),
      wave = wave_num,
      year = year_val,
      
      # Demographics
      age = safe_numeric(., v$hgage),
      age_sq = age^2,
      female = ifelse(safe_numeric(., v$hgsex) == 2, 1, 0),
      # NA-propagating (same fix family as `employed`): missing mrcurr must
      # not be coded as unmarried.
      mrcurr_raw = safe_numeric(., v$mrcurr),
      married = ifelse(is.na(mrcurr_raw), NA_real_,
                       as.numeric(mrcurr_raw %in% c(1, 2))),
      
      # Employment
      esdtl_raw = safe_numeric(., v$esdtl),
      # NA-propagating: a missing esdtl must NOT be coded as non-employed
      # (the old %in% rule silently mapped NA -> 0, while fulltime/parttime
      # below correctly stay NA). Matters for extensive-margin estimates
      # (Part H) and the training event study; the published MASTER_hh
      # results do not use `employed`, so they are unaffected.
      employed = ifelse(is.na(esdtl_raw), NA_real_,
                        as.numeric(esdtl_raw %in% c(1, 2))),
      fulltime = ifelse(esdtl_raw == 1, 1, 0),
      # numeric aliases used directly in MASTER_hh.R formulas (the
      # historical W12-W24 loader carried these; never derived in-master)
      married_num  = as.numeric(married),
      fulltime_num = as.numeric(fulltime),
      parttime = ifelse(esdtl_raw == 2, 1, 0),
      # v21.6: fulltime is esdtl_raw == 1, so it is 0 for the unemployed
      # and for those out of the labour force as well as for part-time
      # workers. That is the right input to employed == 1 & fulltime == 1
      # and every current consumer uses it that way, but the NAME reads
      # as "full-time among the employed" and mean(fulltime) over a panel
      # is the population rate. This is that conditional quantity, NA off
      # the employed. fulltime itself is untouched.
      fulltime_emp = ifelse(is.na(employed) | employed != 1, NA_real_,
                            as.numeric(esdtl_raw == 1)),
      
      fy_share_employed = safe_numeric(., v$capj) / 100,
      fy_share_employed = ifelse(fy_share_employed > 1, NA_real_, fy_share_employed),
      # ---- v5: annual earnings, single variable ----
      # wsfei and wsfes are the SAME quantity on two definitions (the
      # latter includes salary sacrifice), so they must NOT be summed.
      # v2-v4 summed them and produced an annual measure at roughly twice
      # individual annual pay. wsfes is preferred where available
      # (wave 10 onward) because salary sacrifice is part of
      # compensation; wsfei is the fallback for earlier waves and for
      # person-waves where wsfes is missing.
      fy_wages        = safe_numeric_zero(., v$wsfei),
      fy_wages_inc_ss = safe_numeric_zero(., v$wsfes),
      annual_income   = coalesce(fy_wages_inc_ss, fy_wages),
      # Which definition annual_income came from. wsfes is unavailable
      # before wave 10, and salary sacrifice is concentrated among higher
      # earners, so a switch of source across waves would introduce a
      # level break. Our panel starts at wave 12, so this should be
      # "inc_salary_sacrifice" almost everywhere; verify on the first run.
      earnings_source_flag = case_when(
        !is.na(fy_wages_inc_ss) ~ "inc_salary_sacrifice",
        !is.na(fy_wages)        ~ "wages_only_fallback",
        TRUE                    ~ NA_character_
      ),
      # Salary sacrifice component, where both definitions are observed.
      salary_sacrifice = ifelse(
        is.na(fy_wages_inc_ss) | is.na(fy_wages), NA_real_,
        pmax(0, fy_wages_inc_ss - fy_wages)
      ),
      
      # ---- v2: other income aggregates ----
      fy_benefits          = safe_numeric_zero(., v$bnfip),
      # v21.25. THE THREE INCOME AGGREGATES DISAGREE ON THREE THINGS, and
      # the column names hide all three. Verified against the dictionary:
      #
      #   hifditp  HOUSEHOLD  disposable       [imputed]  positive only
      #   tifditp  individual disposable       NOT imputed positive only
      #   tifefp   individual GROSS REGULAR    [imputed]  positive only
      #
      # (a) fy_income_gross is GROSS REGULAR, not gross. It differs from
      #     fy_income_disposable by tax AND by the regular/irregular
      #     distinction, so their difference is not a tax wedge and must
      #     not be reported as one.
      #
      # (b) tifditp is the only one NOT imputed, so the household and
      #     individual measures of the SAME concept run on different
      #     imputation regimes. A ratio of the two moves when imputation
      #     coverage moves, which has nothing to do with income. Nothing
      #     in this loader takes that ratio today; the names are what
      #     would invite it.
      #
      # (c) ALL THREE are POSITIVE VALUES ONLY. Negative income is
      #     censored upstream, and that bites here specifically: 5.20% of
      #     men's and 3.39% of women's person-waves carry business income
      #     with zero wages, self-employment among mothers trebles after
      #     the birth, and a loss-making business is exactly the case
      #     these variables cannot represent.
      #
      # Renamed to carry (a). Left otherwise as they are, because every
      # published number rests on them and a silent respecification is
      # what the restriction manifest exists to prevent.
      # v21.26: p MINUS n, on the same pattern the business-income block
      # uses. safe_numeric_zero on both first, so a missing code cannot
      # become a spurious loss.
      .fy_disp_p = safe_numeric_zero(., v$tifditp),
      .fy_disp_n = safe_numeric_zero(., v$tifditn),
      .fy_greg_p = safe_numeric_zero(., v$tifefp),
      .fy_greg_n = safe_numeric_zero(., v$tifefn),
      fy_income_disposable = ifelse(
        is.na(.fy_disp_p) & is.na(.fy_disp_n), NA_real_,
        dplyr::coalesce(.fy_disp_p, 0) - dplyr::coalesce(.fy_disp_n, 0)),
      fy_income_gross_regular = ifelse(
        is.na(.fy_greg_p) & is.na(.fy_greg_n), NA_real_,
        dplyr::coalesce(.fy_greg_p, 0) - dplyr::coalesce(.fy_greg_n, 0)),
      fy_income_gross      = fy_income_gross_regular,   # deprecated alias
      # Kept so the correction is auditable rather than assumed: how many
      # person-waves the negative half actually moves.
      fy_disposable_negative = .fy_disp_n,
      
      # ---- v7: employment type and business income ----
      # emptype_raw is the raw code; is_employee / is_self_employed are
      # derived from EMPTYPE_EMPLOYEE, which MUST be confirmed against
      # the value labels (the default c(1) is a placeholder, and the CPQ
      # and PQ versions of the question number categories differently).
      # Business income. v$businc is length 1 for a single variable and
      # length 2 for the positive/negative pair convention HILDA uses in
      # the income DVs. Netting happens BEFORE deflating.
      #
      # v8 SIGN FIX: the second element holds MAGNITUDES, not signed
      # values. Verified at wave 24: 1,172 person-waves have bifip > 0,
      # 118 have bifin > 0, and NONE has bifin < 0 (median non-zero
      # bifin = 7,125). Net business income is the DIFFERENCE. The v7
      # draft summed them, which would have recorded a $7,125 loss as
      # $7,125 of income and inverted the sign for the self-employed
      # this variable exists to measure.
      #
      # Losses are NOT floored at zero: negative labour income for the
      # year is the correct value, and flooring would understate the
      # penalty for a mother whose business runs at a loss.
      #
      # Where the mnemonic is unset this is an NA column and
      # fy_earnings_total falls back to wages alone, which
      # earnings_total_has_business records.
      fy_business_income = if (length(v$businc) >= 2) {
        bi_pos <- safe_numeric_zero(., v$businc[1])
        bi_neg <- safe_numeric_zero(., v$businc[2])
        ifelse(is.na(bi_pos) & is.na(bi_neg), NA_real_,
               coalesce(bi_pos, 0) - coalesce(bi_neg, 0))
      } else {
        safe_numeric_zero(., v$businc)
      },
      
      emptype_raw = safe_numeric(., v$emptype),
      is_employee = ifelse(is.na(emptype_raw), NA_real_,
                           as.numeric(emptype_raw %in% EMPTYPE_EMPLOYEE)),
      
      # BROAD (economic) definition: in work and not an employee, so
      # codes 2-6. Includes "employee of own business", which is a legal
      # form rather than an economic category: someone who incorporates
      # and pays themselves a salary is economically self-employed, and
      # their income arrives in wsfei/wsfes as WAGES rather than in
      # bifip, so a business-income-only definition would miss them.
      # Incorporating a consultancy is exactly the post-birth
      # flexibility margin this variable exists to capture.
      #
      # Conditioning on `employed` keeps the non-employed as NA rather
      # than counting them as self-employed by residual.
      is_self_employed = ifelse(is.na(emptype_raw) | is.na(employed), NA_real_,
                                as.numeric(employed == 1 & is_employee == 0)),
      
      # NARROW definition: employer/own-account proper (codes 4-5),
      # excluding the employee-of-own-business legal form. Carried so
      # the definitional choice is explicit in the output rather than
      # buried here; any result that turns on which is used should say
      # so.
      is_selfemp_narrow = ifelse(is.na(emptype_raw) | is.na(employed), NA_real_,
                                 as.numeric(employed == 1 &
                                              emptype_raw %in% EMPTYPE_SELFEMP_NARROW)),
      
      # Unpaid family workers (code 6): in work, no earnings. A small
      # group, but they belong in neither category and would distort a
      # binary split, so they are identifiable and can be excluded.
      is_unpaid_family = ifelse(is.na(emptype_raw), NA_real_,
                                as.numeric(emptype_raw %in% EMPTYPE_UNPAID_FAMILY)),
      unpaid_leave_days = safe_numeric(., v$alup),
      
      # Cross-check: own business last financial year (F42).
      #
      # v10 CODING FIX: F42 uses 1 = Yes, 2 = No. v9 assumed the 1 = No,
      # 2 = Yes convention HILDA uses for many other binaries, and the
      # cross-check exposed the error itself: it reported 83.9% of
      # workers running their own business, and placed 82.9% of all
      # employed person-waves in the cell where an EMPLOYEE also has a
      # business. Both are inverted-binary signatures.
      #
      # Verify on any new release: pct_own_bus_lfy should be roughly
      # 12-15%, close to the esempdt self-employment share, NOT ~85%.
      bifhave_raw = safe_numeric(., v$bifhave),
      had_business_lfy = ifelse(is.na(bifhave_raw), NA_real_,
                                as.numeric(bifhave_raw == 1)),
      .hh_disp_p = safe_numeric_zero(., v$hifditp),
      .hh_disp_n = safe_numeric_zero(., v$hifditn),
      hh_income_disposable = ifelse(
        is.na(.hh_disp_p) & is.na(.hh_disp_n), NA_real_,
        dplyr::coalesce(.hh_disp_p, 0) - dplyr::coalesce(.hh_disp_n, 0)),
      .hh_greg_p = safe_numeric_zero(., v$hifefp),
      .hh_greg_n = safe_numeric_zero(., v$hifefn),
      hh_income_gross      = ifelse(
        is.na(.hh_greg_p) & is.na(.hh_greg_n), NA_real_,
        dplyr::coalesce(.hh_greg_p, 0) - dplyr::coalesce(.hh_greg_n, 0)),
      
      # Wages and hours
      # wscei: "DV: Current WEEKLY gross wages & salary - ALL JOBS".
      # Recorded as zero for the non-employed, so safe_numeric_zero is
      # the correct helper.
      #
      # v5: the v4 /2 divisor is REVERTED. wscei is weekly, per its label
      # and per the medians (wscei*52 = 78,000 vs wsfei = 74,972 at wave
      # 24). The 0.5 ratio that prompted the divisor came from the
      # annual_income double-count fixed above, not from wscei.
      weekly_earnings = safe_numeric_zero(., v$wscei),
      # v21.23: the sacrifice-inclusive twin and the difference between
      # them. NOT swapped into weekly_earnings: every published number in
      # this project rests on wscei, and a silent change of outcome
      # definition is the one thing the restriction manifest exists to
      # prevent. Use weekly_earnings_inc_ss as a robustness arm and report
      # both.
      weekly_earnings_inc_ss = safe_numeric_zero(., v$wsces),
      salary_sacrificed_weekly = ifelse(
        is.na(weekly_earnings) | is.na(weekly_earnings_inc_ss), NA_real_,
        weekly_earnings_inc_ss - weekly_earnings),
      wage_imputed      = safe_numeric(., v$wscef),
      wage_ss_imputed   = safe_numeric(., v$wscsf),
      fy_wage_imputed   = safe_numeric(., v$wsfef),
      
      # ---- v21.32: EMPLOYER NON-CASH BENEFITS -------------------------
      # safe_numeric_zero, not safe_numeric. These are DVs and zero is the
      # correct and common answer: an employee with no car and no phone
      # has nbcm = 0, and the non-employed have 0 as well. Reading a
      # legitimate zero as missing would put the whole no-benefits
      # majority into NA and leave a "non-cash share" computed on
      # recipients only, which is the number that flatters the omission.
      #
      # The IMPUTED version is primary and the pre-imputed one is carried
      # beside it, the same arrangement wscei/wsces have. HILDA imputes a
      # material share of income values; a design that wants to exclude
      # them has noncash_imputed to do it with, and one that does not
      # would otherwise be dropping observations silently.
      # NOT _preimp_raw. Any column ending in _raw is removed by the
      # name-based scratch drop unless it is in RAW_COLS_KEEP, and the
      # v9 comment on that list records emptype_raw and bifhave_raw
      # being deleted for exactly this reason and surfacing 400 lines
      # later as a missing column in the final select. These two are
      # OUTPUTS. _preimp also says what they are: the pre-imputed half
      # of the manual's pre/post/flag trio, not a scratch code column.
      # v21.33. THE PRE-IMPUTED COLUMNS KEEP THEIR MISSING VALUES.
      #
      # The IMPUTED DVs are complete: nbcmi and nbcoi are 100.0%
      # non-missing in every wave, so safe_numeric_zero has nothing to
      # coerce and is harmless there.
      #
      # The PRE-IMPUTED ones are not. nbcm is 74-75% non-missing, so
      # roughly 6,000 person-waves a wave carry a HILDA negative code,
      # and safe_numeric_zero turned every one of them into a measured
      # zero. That destroys the only thing the pre-imputed column exists
      # for. A researcher reaching for _preimp is trying to avoid
      # imputed values; handing them a hard-coded zero in place of the
      # imputation is worse than the imputation, because the imputation
      # at least carries a flag.
      #
      # So: imputed columns coerce, pre-imputed columns do not.
      noncash_benefits_weekly        = safe_numeric_zero(., v$nbcmi),
      noncash_benefits_weekly_preimp = safe_numeric(., v$nbcm),
      noncash_imputed             = safe_numeric(., v$nbcmf),
      # Other jobs, so the total sits on the same all-jobs base as wscei.
      noncash_other_jobs_weekly     = safe_numeric_zero(., v$nbcoi),
      noncash_other_jobs_weekly_preimp = safe_numeric(., v$nbco),
      noncash_other_imputed         = safe_numeric(., v$nbcof),
      # The all-jobs total. NA only where NEITHER part is observed: an
      # absent other-jobs column should not destroy a main-job value that
      # is present, which is what a bare sum would do.
      noncash_total_weekly = ifelse(
        is.na(noncash_benefits_weekly) & is.na(noncash_other_jobs_weekly),
        NA_real_,
        dplyr::coalesce(noncash_benefits_weekly, 0) +
          dplyr::coalesce(noncash_other_jobs_weekly, 0)),
      #
      # THE HAS-ANY MARGIN, TWICE, and the two are not redundant.
      #
      # noncash_any_value is arithmetic on the DV and cannot be wrong
      # about the coding because there is no coding in it. noncash_any
      # reads the nbcmany indicator through the == 1 rule, which is an
      # ASSUMPTION about this release and is audited after combination
      # like every other binary in this file. If nbcmany is absent, the
      # first survives and the second is NA, and the diagnostic prints
      # the cross-tabulation either way.
      #
      # The value route is the one to prefer if they disagree: a person
      # with nbcmi > 0 has a benefit whatever the indicator says.
      noncash_any_value = ifelse(is.na(noncash_total_weekly), NA_real_,
                                 as.numeric(noncash_total_weekly > 0)),
      noncash_any_raw = safe_numeric(., v$nbcmany),
      noncash_any = case_when(
        noncash_any_raw == 1 ~ 1L,
        noncash_any_raw == 2 ~ 0L,
        noncash_any_raw == 0 ~ 0L,
        TRUE ~ NA_integer_),
      #
      # ---- v21.33 THE GATE, and why the DV cannot supply it ----------
      # nbcmany is question F10, put to employees about the current main
      # job. nbcmi is a DV and is complete, because HILDA fills the
      # non-employed with a zero. Those are different populations and
      # the difference is roughly 7,000 person-waves a wave.
      #
      # TWO COVERAGE FIGURES, ON TWO DENOMINATORS, and they are both
      # right. On the raw wave files nbcmany is non-missing on about 43%
      # of rows; on this panel the gate reports 74.8%. The panel is the
      # restricted analysis population and the .dta is every enumerated
      # person, so the same item covers a larger share of the smaller
      # frame. The v21.34 coding audit brackets the panel figure at
      # 72.7-78.8% across waves, which is the number to check the gate
      # against.
      #
      # An earlier version of this comment gave only the 43% and read as
      # though it predicted the gate output. It does not, and anyone
      # reconciling the two would have gone looking for a bug in a gate
      # that was working.
      #
      # So a zero in noncash_benefits_weekly is a MIXTURE: an employee
      # who answered F10 with no and has a genuine zero, and a
      # non-employed person for whom the DV was set to zero without
      # anyone being asked anything. The mixing proportion moves with
      # employment, which is exactly what moves at a birth, so the
      # mixture is not innocuous in this design.
      #
      # Same structure as training_module_asked, and the same remedy: a
      # gate, and gated twins that carry NA where nothing was asked.
      #
      # v21.34. THE `> 0` TERM IS GONE, and it was the only part of this
      # gate that could be wrong.
      #
      # It asserted that a substantive answer to F10 is a POSITIVE code,
      # which holds under 1 = yes / 2 = no and fails under 0 = no /
      # 1 = yes. The recode nine lines above declines to make that
      # assumption - it carries `noncash_any_raw == 0 ~ 0L` precisely
      # because the coding was not established - so the same mutate both
      # permitted 0 as a valid no and treated 0 as never-asked. Two
      # readings of one column, seven lines apart.
      #
      # WHAT IT WOULD HAVE COST, if the coding is 0/1: four fifths of
      # genuine no answers gated out, noncash_any_value_z identically 1
      # by construction, and noncash_benefits_weekly_z a recipients-only
      # mean. On a synthetic run with a true asked-population mean of
      # $11.26 the gated column read $56.29. Every one of those is a
      # plausible-looking number, which is why it survived review.
      #
      # !is.na() IS ALREADY THE RIGHT TEST and needs no assumption.
      # safe_numeric has mapped -10..-1 to NA before this line runs, so
      # non-missing here means the respondent gave a substantive answer,
      # whatever integer the release chose for it. That is exactly the
      # form training_module_asked and aim_battery_asked use, and this
      # gate was the only one of the three carrying a magnitude test.
      #
      # Do not reintroduce the comparison to distinguish yes from no.
      # noncash_any does that, under audit; this column answers a
      # different question and only that one: was anything asked.
      noncash_module_asked = as.integer(!is.na(noncash_any_raw)),
      noncash_benefits_weekly_z = ifelse(noncash_module_asked == 0L,
                                         NA_real_, noncash_benefits_weekly),
      noncash_total_weekly_z = ifelse(noncash_module_asked == 0L,
                                      NA_real_, noncash_total_weekly),
      noncash_any_value_z = ifelse(noncash_module_asked == 0L,
                                   NA_real_, noncash_any_value),
      #
      # ---- v21.33 EMPLOYER CHILD CARE: BUILT, AND UNUSABLE ------------
      # A raw read of the thirteen .dta files settles what the 0.0%
      # coverage meant, and it is none of the three things suspected.
      # The item is FINE: of 121 person-waves reporting receipt across
      # the whole window, 110 carry a positive amount, so the amount is
      # asked, answered and recorded correctly.
      #
      # What is missing is the population. 121 receipts in 299,136 rows
      # is 0.04%, five to fifteen a wave against roughly 2,100 asked, so
      # the receipt rate among those asked is about 0.5%. Employer-
      # provided child care is close to non-existent in Australia over
      # this window.
      #
      # KEPT, BECAUSE THE ABSENCE IS ITSELF REPORTABLE, and a null this
      # clean is worth more than a caveat. But no event study can run on
      # it: with 2,704 treated persons the expected count of affected
      # person-waves is a handful. Do not build a design on these two
      # columns; cite the rate.
      # The amount is zero-valid for the same reason the benefit total
      # is; the receipt item is a binary and gets the _raw twin so the
      # coding audit can reach it.
      employer_childcare_weekly = safe_numeric_zero(., v$nbcmcca),
      employer_childcare_raw    = safe_numeric(., v$nbcmccr),
      employer_childcare = case_when(
        employer_childcare_raw == 1 ~ 1L,
        employer_childcare_raw == 2 ~ 0L,
        employer_childcare_raw == 0 ~ 0L,
        TRUE ~ NA_integer_),
      # jbhruc is ALL JOBS and is the analysis measure throughout: it is
      # the denominator matching the all-jobs wscei numerator, and total
      # market hours is the object the specialisation theory concerns.
      hours_worked          = safe_numeric(., v$jbhruc),   # all jobs
      hours_worked_main_job = safe_numeric(., v$jbmhruc),  # main job
      hours_worked_clean = case_when(
        is.na(hours_worked) ~ NA_real_,
        hours_worked <= 0 ~ NA_real_,
        hours_worked > 100 ~ NA_real_,
        TRUE ~ hours_worked
      ),
      
      hourly_wage = ifelse(
        !is.na(weekly_earnings) & !is.na(hours_worked_clean) & hours_worked_clean > 0,
        weekly_earnings / hours_worked_clean,
        NA_real_
      ),
      
      hourly_wage_clean = case_when(
        is.na(hourly_wage) ~ NA_real_,
        hourly_wage < 10 ~ NA_real_,
        hourly_wage > 500 ~ NA_real_,
        TRUE ~ hourly_wage
      ),
      
      ln_hourly_wage = ifelse(hourly_wage_clean > 0, log(hourly_wage_clean), NA_real_),
      
      # ---- v21.32: TOTAL COMPENSATION, THE ROBUSTNESS ARM -------------
      # Built on weekly_earnings_inc_ss and NOT on weekly_earnings. wsces
      # already carries the sacrifice, so starting from wscei would leave
      # one of the three components out of a column named for the total
      # and produce a measure that is neither the published one nor the
      # complete one. Business income is the fourth component and is
      # annual, not weekly, so it stays where it is in fy_earnings_total;
      # weekly_compensation is employee compensation, which is what the
      # hourly wage is a rate of.
      #
      # weekly_earnings IS UNTOUCHED. Every published figure in this
      # project rests on it and a silent change of outcome definition is
      # the one thing the restriction manifest exists to prevent. This is
      # a second column to report beside the first.
      weekly_compensation = ifelse(
        is.na(weekly_earnings_inc_ss) & is.na(noncash_total_weekly),
        NA_real_,
        dplyr::coalesce(weekly_earnings_inc_ss, 0) +
          dplyr::coalesce(noncash_total_weekly, 0)),
      # The share is the quantity the direction argument turns on: if it
      # is flat in sex and event time, the omission was harmless.
      noncash_share_of_comp = ifelse(
        !is.na(weekly_compensation) & weekly_compensation > 0 &
          !is.na(noncash_total_weekly),
        noncash_total_weekly / weekly_compensation, NA_real_),
      
      # REAL WAGE (WPI adjusted)
      deflator = wave_deflator,   # carried: MASTER_hh.R re-deflates with it
      real_hourly_wage = hourly_wage_clean * wave_deflator,
      ln_hourly_wage_real = ifelse(real_hourly_wage > 0, log(real_hourly_wage), NA_real_),
      
      # ---- v21.32: the same ladder for compensation --------------------
      # THE TRIMMING BOUNDS ARE THE WAGE BOUNDS, DELIBERATELY. A wider
      # window for compensation would make the two series differ for two
      # reasons at once, the added benefits and a different top code, and
      # nothing in the output would separate them. Identical bounds mean
      # any gap between ln_hourly_wage_real and ln_hourly_compensation_real
      # is the benefits and only the benefits.
      #
      # The cost is that the two samples are not identical: someone at
      # $9.60 in wages and $10.40 in compensation is in the second and not
      # the first. The diagnostic reports the overlap, and a like-for-like
      # comparison should condition on both being observed.
      hourly_compensation = ifelse(
        !is.na(weekly_compensation) & !is.na(hours_worked_clean) &
          hours_worked_clean > 0,
        weekly_compensation / hours_worked_clean, NA_real_),
      hourly_compensation_clean = case_when(
        is.na(hourly_compensation) ~ NA_real_,
        hourly_compensation < 10  ~ NA_real_,
        hourly_compensation > 500 ~ NA_real_,
        TRUE ~ hourly_compensation),
      real_hourly_compensation = hourly_compensation_clean * wave_deflator,
      ln_hourly_compensation_real = ifelse(
        real_hourly_compensation > 0, log(real_hourly_compensation), NA_real_),
      real_weekly_compensation = weekly_compensation * wave_deflator,
      real_noncash_benefits_weekly = noncash_total_weekly * wave_deflator,
      
      # ---- v2: REAL EARNINGS (WPI adjusted) ----
      # The hourly wage is observed only for the employed; earnings are
      # defined at zero for everyone. An event study on earnings therefore
      # avoids the wage-sample selection that otherwise forces Lee bounds
      # and a Heckman correction, and it is the object the child-penalty
      # literature reports, so estimates are directly comparable.
      real_weekly_earnings = weekly_earnings * wave_deflator,
      real_annual_income   = annual_income   * wave_deflator,
      real_fy_benefits     = fy_benefits     * wave_deflator,
      real_hh_income_disposable = hh_income_disposable * wave_deflator,
      real_fy_business_income   = fy_business_income   * wave_deflator,
      
      # ---- v7: TOTAL EARNINGS = wages + business income ----
      # The measure the child-penalty design needs. wsfei/wsfes are
      # wages and salary ONLY, so a mother who moves from employment
      # into her own business reads as an earnings drop to zero on
      # annual_income even though she is still earning.
      #
      # fy_income_gross (tifefp) is NOT the substitute. It is TOTAL
      # regular income and therefore includes government transfers, and
      # family payments rise exactly when the earnings penalty opens, so
      # it understates the penalty by construction. fy_earnings_total
      # excludes transfers: it is labour income from both sources.
      #
      # Where business income is unavailable (before discovery, or in a
      # wave lacking the variable) this falls back to wages alone, and
      # earnings_total_has_business records which.
      fy_earnings_total = ifelse(
        is.na(annual_income) & is.na(fy_business_income), NA_real_,
        coalesce(annual_income, 0) + coalesce(fy_business_income, 0)
      ),
      # v21.9. RENAMED, and MY EARLIER DIAGNOSIS OF IT WAS WRONG, which is
      # worth recording because the wrong diagnosis is the tempting one.
      #
      # v21.8 flagged this column as a defect on the grounds that it is
      # constant at 1 across all 134,131 person-waves while only 8.3% of
      # them have non-zero business income. It is constant, and it is NOT
      # a defect: read the comments above, the column was built to record
      # whether fy_earnings_total INCLUDES the business component or fell
      # back to wages alone, and the businc mnemonics resolve in every
      # wave on this window, so 1 everywhere is the correct answer and the
      # fallback never fired. The column did its job and reported that
      # nothing went wrong.
      #
      # What IS wrong is the NAME. "has_business" reads as a fact about
      # the PERSON and means a fact about the MEASURE, and those differ by
      # a factor of twelve. So the fix is a rename, not a recode, plus the
      # column a reader was reaching for when they misread this one.
      earnings_total_includes_business = ifelse(
        is.na(fy_earnings_total), NA_real_,
        as.numeric(!is.na(fy_business_income))
      ),
      # The substantive question, distinct from BOTH the provenance flag
      # above and from the two existing business indicators. Named after
      # its own test so it cannot be misread:
      #   fy_business_income_nonzero  business income in the financial
      #                               year was not zero: about 8.3%, and
      #                               it counts a LOSS as a business,
      #                               because a loss is a business
      #   had_business_lfy            bifhave, self-reported business last
      #                               financial year: about 16.1%
      #   is_self_employed            esempdt current status: about 12.6%
      # Three different reference periods and three different questions.
      # Do not treat any as a substitute for another.
      fy_business_income_nonzero = ifelse(
        is.na(fy_business_income), NA_real_,
        as.numeric(fy_business_income != 0)
      ),
      real_fy_earnings_total = fy_earnings_total * wave_deflator,
      
      # Inverse hyperbolic sine. Do NOT log the earnings measures: a large
      # share of mothers have zero earnings in the post-birth years and
      # log(0) drops exactly the observations the design exists to keep.
      # IHS is defined at zero and behaves like a log away from it.
      ihs_weekly_earnings = ifelse(
        is.na(real_weekly_earnings), NA_real_,
        log(real_weekly_earnings + sqrt(real_weekly_earnings^2 + 1))
      ),
      # IHS of total earnings, on the same footing as ihs_annual_income:
      # defined at zero, so a mother out of work stays in the sample.
      ihs_earnings_total = ifelse(
        is.na(real_fy_earnings_total), NA_real_,
        log(real_fy_earnings_total + sqrt(real_fy_earnings_total^2 + 1))
      ),
      earnings_total_is_zero = ifelse(
        is.na(real_fy_earnings_total), NA_real_,
        as.numeric(real_fy_earnings_total == 0)
      ),
      # v8: business losses make total earnings genuinely negative for a
      # small group, so the zero flag alone no longer describes the
      # distribution. IHS is defined on the whole real line and needs no
      # adjustment; this flag exists so the negative tail can be counted
      # and, if a specification requires it, excluded explicitly rather
      # than silently.
      earnings_total_is_negative = ifelse(
        is.na(real_fy_earnings_total), NA_real_,
        as.numeric(real_fy_earnings_total < 0)
      ),
      ihs_annual_income = ifelse(
        is.na(real_annual_income), NA_real_,
        log(real_annual_income + sqrt(real_annual_income^2 + 1))
      ),
      
      # Zero-earnings flags. A zero here should be a true zero (person not
      # employed), not a missing value silently coerced. Verify downstream
      # that earnings_is_zero agrees with employed == 0.
      earnings_is_zero        = ifelse(is.na(real_weekly_earnings), NA_real_,
                                       as.numeric(real_weekly_earnings == 0)),
      annual_income_is_zero   = ifelse(is.na(real_annual_income), NA_real_,
                                       as.numeric(real_annual_income == 0)),
      
      # Weekly-vs-annual reconciliation. BOTH measures are all-jobs, so
      # this was never a multiple-jobs diagnostic (the v2-v4 name
      # multi_job_gap was a misnomer and is retired). What remains is a
      # genuine reconciliation check: current weekly pay annualised
      # against financial-year pay. It should centre near zero, with
      # dispersion from job changes, part-year work, and pay rises.
      # A median near +0.5 indicates the annual measure is doubled;
      # near -1.0 indicates the weekly measure is.
      implied_annual_from_weekly = real_weekly_earnings * 52,
      weekly_annual_gap = ifelse(
        is.na(real_annual_income) | is.na(implied_annual_from_weekly) |
          real_annual_income <= 0,
        NA_real_,
        (real_annual_income - implied_annual_from_weekly) / real_annual_income
      ),
      # Second-job margin: all-jobs hours minus main-job hours. Positive
      # for multiple-job holders, zero otherwise. v2-v5 computed this
      # subtraction the other way round and so could never be positive.
      second_job_hours = ifelse(
        is.na(hours_worked) | is.na(hours_worked_main_job), NA_real_,
        pmax(0, hours_worked - hours_worked_main_job)
      )
    )
  
  cat("    - Core variables created\n")
  
  # ============================
  # STEP 2: ALL EDUCATION VARIABLES EXTRACTION
  # ============================
  
  cat("  Step 2: Extracting all education variables...\n")
  
  wave_data <- wave_data %>%
    mutate(
      
      childcare_cost = safe_numeric_zero(., v$ccactci),
      
      # Main education variables
      edhists_raw = safe_numeric(., v$edhists),
      edhigh1_raw = safe_numeric(., v$edhigh1),
      edsscmp_raw = safe_numeric(., v$edsscmp),
      
      # ALL completed qualifications (edqro*)
      edqrodc_raw = safe_numeric(., v$edqrodc),
      edqroms_raw = safe_numeric(., v$edqroms),
      edqrogd_raw = safe_numeric(., v$edqrogd),
      edqrogc_raw = safe_numeric(., v$edqrogc),
      edqrohd_raw = safe_numeric(., v$edqrohd),
      edqrobd_raw = safe_numeric(., v$edqrobd),
      edqroad_raw = safe_numeric(., v$edqroad),
      edqroav_raw = safe_numeric(., v$edqroav),
      edqrodp_raw = safe_numeric(., v$edqrodp),
      edqrodn_raw = safe_numeric(., v$edqrodn),
      edqroc4_raw = safe_numeric(., v$edqroc4),
      edqroc3_raw = safe_numeric(., v$edqroc3),
      edqroc2_raw = safe_numeric(., v$edqroc2),
      edqroc1_raw = safe_numeric(., v$edqroc1),
      edqrocd_raw = safe_numeric(., v$edqrocd),
      edqrosh_raw = safe_numeric(., v$edqrosh),
      edqrosl_raw = safe_numeric(., v$edqrosl),
      
      # ALL obtained qualifications (edqo*)
      edqodc_raw = safe_numeric(., v$edqodc),
      edqoms_raw = safe_numeric(., v$edqoms),
      edqogd_raw = safe_numeric(., v$edqogd),
      edqogc_raw = safe_numeric(., v$edqogc),
      edqohd_raw = safe_numeric(., v$edqohd),
      edqobd_raw = safe_numeric(., v$edqobd),
      edqoad_raw = safe_numeric(., v$edqoad),
      edqoav_raw = safe_numeric(., v$edqoav),
      edqodp_raw = safe_numeric(., v$edqodp),
      edqodn_raw = safe_numeric(., v$edqodn),
      edqoc4_raw = safe_numeric(., v$edqoc4),
      edqoc3_raw = safe_numeric(., v$edqoc3),
      edqoc2_raw = safe_numeric(., v$edqoc2),
      edqoc1_raw = safe_numeric(., v$edqoc1),
      edqocd_raw = safe_numeric(., v$edqocd),
      edqota_raw = safe_numeric(., v$edqota),
      edqotc_raw = safe_numeric(., v$edqotc),
      edqonq_raw = safe_numeric(., v$edqonq),
      edqotq_raw = safe_numeric(., v$edqotq),
      # v21.5: a literal NA rather than a lookup, because the category
      # does not exist. flag_any_set() and case_when() both treat it the
      # same way they treated an unresolved name, so nothing downstream
      # moves; what changes is that the map stops asking for it and the
      # absent-name report stops carrying it.
      edqobc_raw = NA_real_,
      edqosh_raw = safe_numeric(., v$edqosh),
      edqosl_raw = safe_numeric(., v$edqosl),
      
      # Nursing-specific
      ednrsdc_raw = safe_numeric(., v$ednrsdc),
      ednrsms_raw = safe_numeric(., v$ednrsms),
      ednrsgd_raw = safe_numeric(., v$ednrsgd),
      ednrsbd_raw = safe_numeric(., v$ednrsbd),
      ednrshd_raw = safe_numeric(., v$ednrshd),
      ednrsav_raw = safe_numeric(., v$ednrsav),
      ednrsdp_raw = safe_numeric(., v$ednrsdp),
      ednrsc3_raw = safe_numeric(., v$ednrsc3),
      ednrstd_raw = safe_numeric(., v$ednrstd),
      
      # Teaching-specific
      edtchdc_raw = safe_numeric(., v$edtchdc),
      edtchms_raw = safe_numeric(., v$edtchms),
      edtchgd_raw = safe_numeric(., v$edtchgd),
      edtchgc_raw = safe_numeric(., v$edtchgc),
      edtchhd_raw = safe_numeric(., v$edtchhd),
      edtchbd_raw = safe_numeric(., v$edtchbd),
      edtchts_raw = safe_numeric(., v$edtchts),
      edtchtp_raw = safe_numeric(., v$edtchtp),
      edtchdp_raw = safe_numeric(., v$edtchdp),
      edtchav_raw = safe_numeric(., v$edtchav),
      
      # Job-related training variables
      jttdays_raw  = safe_numeric(., v$jttdays),
      jtthrs_raw   = safe_numeric(., v$jtthrs),
      jttnum_raw   = safe_numeric(., v$jttnum),
      jttopot_raw  = safe_numeric(., v$jttopot),
      jttopwt_raw  = safe_numeric(., v$jttopwt),
      jttpeot_raw  = safe_numeric(., v$jttpeot),
      jttpewt_raw  = safe_numeric(., v$jttpewt),
      jttrwrk_raw  = safe_numeric(., v$jttrwrk),   # PQ E1 w7-19, E6 w20-24
      
    )
  
  cat("    - All education variables extracted\n")
  
  
  
  # ============================
  # STEP 3: COMPREHENSIVE EDUCATION YEARS AND PATHWAYS
  # ============================
  
  cat("  Step 3: Creating detailed education variables...\n")
  
  wave_data <- wave_data %>%
    mutate(
      # Extract school years from history for granularity
      school_years_from_hist = case_when(
        edhists_raw == 1 ~ 12,  # Year 12
        edhists_raw == 2 ~ 11,  # Year 11
        edhists_raw == 3 ~ 10,  # Year 10
        edhists_raw == 4 ~ 9,   # Year 9
        edhists_raw == 5 ~ 8,   # Year 8
        edhists_raw == 6 ~ 7,   # Year 7
        edhists_raw == 7 ~ 6,   # Finished primary
        edhists_raw == 8 ~ 5,   # Attended but didn't finish primary
        TRUE ~ NA_real_
      ),
      
      # Create detailed education years with maximum granularity.
      # v21.49: after the waves are combined, this per-wave value is kept
      # as educ_years_ref and educ_years becomes the person's running
      # maximum. The logic here is unchanged, so the caches stay valid.
      educ_years = case_when(
        # === DOCTORATE LEVEL (19-20 years) ===
        edqrodc_raw == 1 | edqodc_raw == 1 ~ 20,
        ednrsdc_raw == 1 ~ 19.5,  # Nursing doctorate
        edtchdc_raw == 1 ~ 19.5,  # Teaching doctorate
        
        # === MASTERS LEVEL (18-18.5 years) ===
        edqroms_raw == 1 | edqoms_raw == 1 ~ 18,
        ednrsms_raw == 1 ~ 18,    # Nursing masters
        edtchms_raw == 1 ~ 18,    # Teaching masters
        
        # === GRADUATE DIPLOMA/CERTIFICATE (17 years) ===
        edqrogd_raw == 1 | edqogd_raw == 1 ~ 17,
        ednrsgd_raw == 1 ~ 17,    # Nursing grad diploma
        edtchgd_raw == 1 ~ 17,    # Teaching grad diploma
        edqrogc_raw == 1 | edqogc_raw == 1 ~ 17,
        edtchgc_raw == 1 ~ 17,    # Teaching grad certificate
        
        # === BACHELOR LEVEL (16-16.5 years) ===
        edqrohd_raw == 1 | edqohd_raw == 1 ~ 16.5,  # Honours
        ednrshd_raw == 1 ~ 16.5,  # Nursing honours
        edtchhd_raw == 1 ~ 16.5,  # Teaching honours
        edqrobd_raw == 1 | edqobd_raw == 1 ~ 16,    # Ordinary bachelor
        ednrsbd_raw == 1 ~ 16,    # Nursing bachelor
        edtchbd_raw == 1 ~ 16,    # Teaching bachelor
        
        # === ADVANCED DIPLOMA LEVEL (15 years) ===
        edqroav_raw == 1 | edqoav_raw == 1 ~ 15,
        ednrsav_raw == 1 ~ 15,    # Nursing advanced diploma
        edtchav_raw == 1 ~ 15,    # Teaching advanced diploma
        edtchts_raw == 1 ~ 15,    # Secondary teaching cert
        
        # === DIPLOMA LEVEL (14-14.5 years) ===
        edqrodp_raw == 1 | edqodp_raw == 1 ~ 14.5,
        ednrsdp_raw == 1 ~ 14.5,  # Nursing diploma
        edtchdp_raw == 1 ~ 14.5,  # Teaching diploma
        edqroad_raw == 1 | edqoad_raw == 1 ~ 14.5,  # Associate degree
        edqrodn_raw == 1 | edqodn_raw == 1 ~ 14.5,  # Diploma nursing/unknown
        ednrstd_raw == 1 ~ 14,    # Registered Nurse diploma
        edtchtp_raw == 1 ~ 14,    # Primary teaching cert
        
        # === CERTIFICATE IV (13.5 years) ===
        edqroc4_raw == 1 | edqoc4_raw == 1 ~ 13.5,
        
        # === CERTIFICATE III/TRADE (13 years) ===
        edqroc3_raw == 1 | edqoc3_raw == 1 ~ 13,
        ednrsc3_raw == 1 ~ 13,    # Nursing cert III
        edqota_raw == 1 ~ 13,     # Trade certificate
        edqotc_raw == 1 ~ 13,     # Technical certificate
        
        # === CERTIFICATE II (12.5 years) ===
        edqroc2_raw == 1 | edqoc2_raw == 1 ~ 12.5,
        edqrocd_raw == 1 | edqocd_raw == 1 ~ 12.5,  # Cert unknown level
        
        # === CERTIFICATE I (12 years) ===
        edqroc1_raw == 1 | edqoc1_raw == 1 ~ 12,
        
        # === SECONDARY SCHOOL COMPLETION ===
        edqrosh_raw == 1 | edqosh_raw == 1 ~ 12,  # Year 12
        
        # === FROM edhigh1 (if not captured above) ===
        edhigh1_raw == 1 ~ 18,    # Postgrad
        edhigh1_raw == 2 ~ 17,    # Grad diploma/cert
        edhigh1_raw == 3 ~ 16,    # Bachelor
        edhigh1_raw == 4 ~ 15,    # Advanced diploma
        edhigh1_raw == 5 ~ 13,    # Cert III/IV
        edhigh1_raw == 8 ~ 12,    # Year 12
        
        # Use detailed school years when edhigh1 indicates below Year 12
        edhigh1_raw == 9 & !is.na(school_years_from_hist) ~ school_years_from_hist,
        edhigh1_raw == 9 & is.na(school_years_from_hist) ~ 10,  # Default to Year 10
        edhigh1_raw == 10 & !is.na(school_years_from_hist) ~ school_years_from_hist,
        
        # === FROM edsscmp (CPQ school completion) ===
        edsscmp_raw == 1 ~ 12,  # Year 12
        edsscmp_raw == 2 ~ 11,  # Year 11
        edsscmp_raw == 3 ~ 10,  # Year 10
        edsscmp_raw == 4 ~ 9,   # Year 9
        
        # === FROM edqrosl/edqosl (lower secondary) ===
        edqrosl_raw == 1 | edqosl_raw == 1 ~ 10,
        
        # === FALLBACK to school history ===
        !is.na(school_years_from_hist) ~ school_years_from_hist,
        
        # === DEFAULT ===
        TRUE ~ NA_real_
      ),
      
      # Create field indicators for enriched pathways.
      #
      # v19: coalesce(x == 1, FALSE) is replaced by flag_any_set(),
      # which returns NA where NO component is observed instead of
      # FALSE. Under the old form an absent mnemonic produced a column
      # of FALSE and a diagnostic reading "0 (0%)", which is what
      # has_business_any did for 134,131 person-waves on the strength of
      # a single unresolved name, edqobc. NA also keeps case_when()
      # falling through to the generic pathway, so the enriched labels
      # degrade rather than mislabel.
      # v21.5 WARNING, kept as it is on purpose. edqodn and edqrodn are
      # "Diploma NFI", not further identified, and are NOT nursing; the
      # suffix dn looks to have been read as diploma-nursing. So this
      # column counts anyone with an unspecified diploma as a nurse.
      # education_pathway consumes it, so correcting it here would be a
      # MUTATING change; the corrected version is beside it instead and
      # the two are printed together after combination.
      has_nursing_any = flag_any_set(
        ednrsdc_raw, ednrsms_raw, ednrsgd_raw,
        ednrsbd_raw, ednrshd_raw, ednrsav_raw,
        ednrsdp_raw, ednrsc3_raw, ednrstd_raw,
        edqrodn_raw, edqodn_raw, edqonq_raw),
      
      # The nine genuine nursing-qualification items plus the one
      # genuine nursing entry in the qualifications-obtained battery.
      # Note that dropping edqrodn removes the ONLY CPQ input in any of
      # the four field flags, so this column is purely NPQ and should
      # read entrant-only where has_nursing_any may not.
      has_nursing_strict = flag_any_set(
        ednrsdc_raw, ednrsms_raw, ednrsgd_raw,
        ednrsbd_raw, ednrshd_raw, ednrsav_raw,
        ednrsdp_raw, ednrsc3_raw, ednrstd_raw,
        edqonq_raw),
      
      has_teaching_any = flag_any_set(
        edtchdc_raw, edtchms_raw, edtchgd_raw,
        edtchgc_raw, edtchhd_raw, edtchbd_raw,
        edtchts_raw, edtchtp_raw, edtchdp_raw,
        edtchav_raw, edqotq_raw),
      
      has_business_any  = flag_any_set(edqobc_raw),
      has_trade_any     = flag_any_set(edqota_raw),
      has_technical_any = flag_any_set(edqotc_raw),
      
      # Count qualifications for complexity.
      #
      # v21.4 NOTE, not a fix. This sums BOTH batteries, and they are
      # different questions asked of different people. edqo* is
      # NPQ:A14a, qualifications obtained EVER, put to entrants; edqro*
      # is CPQ:A19, qualifications completed SINCE THE LAST INTERVIEW,
      # put to continuing persons. With na.rm every row gets a number,
      # so this column is a lifetime count for entrants and a
      # since-last-interview count for everyone else, with nothing
      # marking which. It does not reach the panel through the select,
      # so the defect is contained. Splitting it into two columns is
      # additive and welcome; changing this one is MUTATING under the
      # scope rule, so it is left alone and labelled.
      qual_count = rowSums(across(c(starts_with("edqro"), starts_with("edqo")) & 
                                    ends_with("_raw"), ~ .x == 1), na.rm = TRUE),
      
      # Create detailed education pathway
      education_pathway = case_when(
        # === DOCTORATE PATHWAYS ===
        educ_years >= 19 & has_nursing_any ~ "Doctorate_Nursing",
        educ_years >= 19 & has_teaching_any ~ "Doctorate_Teaching",
        educ_years >= 19 & has_business_any ~ "Doctorate_Business",
        educ_years >= 19 ~ "Doctorate_General",
        
        # === MASTERS PATHWAYS ===
        educ_years >= 18 & has_nursing_any ~ "Masters_Nursing",
        educ_years >= 18 & has_teaching_any ~ "Masters_Teaching",
        educ_years >= 18 & has_business_any ~ "Masters_Business",
        educ_years >= 18 ~ "Masters_General",
        
        # === GRADUATE DIPLOMA/CERT PATHWAYS ===
        educ_years >= 17 & has_nursing_any ~ "GradDipCert_Nursing",
        educ_years >= 17 & has_teaching_any ~ "GradDipCert_Teaching",
        educ_years >= 17 & has_business_any ~ "GradDipCert_Business",
        educ_years >= 17 ~ "GradDipCert_General",
        
        # === BACHELOR PATHWAYS ===
        educ_years >= 16.5 & has_nursing_any ~ "Bachelor_Honours_Nursing",
        educ_years >= 16.5 & has_teaching_any ~ "Bachelor_Honours_Teaching",
        educ_years >= 16.5 & has_business_any ~ "Bachelor_Honours_Business",
        educ_years >= 16.5 ~ "Bachelor_Honours_General",
        
        educ_years >= 16 & has_nursing_any ~ "Bachelor_Nursing",
        educ_years >= 16 & has_teaching_any ~ "Bachelor_Teaching",
        educ_years >= 16 & has_business_any ~ "Bachelor_Business",
        educ_years >= 16 ~ "Bachelor_General",
        
        # === SPECIAL PROFESSIONAL CERTIFICATES ===
        ednrstd_raw == 1 ~ "Registered_Nurse",
        edtchts_raw == 1 ~ "Secondary_Teaching_Cert",
        edtchtp_raw == 1 ~ "Primary_Teaching_Cert",
        
        # === ADVANCED DIPLOMA PATHWAYS ===
        educ_years >= 15 & has_nursing_any ~ "AdvDiploma_Nursing",
        educ_years >= 15 & has_teaching_any ~ "AdvDiploma_Teaching",
        educ_years >= 15 & has_trade_any ~ "AdvDiploma_Trade",
        educ_years >= 15 & has_technical_any ~ "AdvDiploma_Technical",
        educ_years >= 15 ~ "Advanced_Diploma",
        
        # === DIPLOMA PATHWAYS ===
        educ_years >= 14 & has_nursing_any ~ "Diploma_Nursing",
        educ_years >= 14 & has_teaching_any ~ "Diploma_Teaching",
        educ_years >= 14 & has_trade_any ~ "Diploma_Trade",
        educ_years >= 14 & has_technical_any ~ "Diploma_Technical",
        educ_years >= 14 & has_business_any ~ "Diploma_Business",
        educ_years >= 14 ~ "Diploma",
        
        # === CERTIFICATE IV PATHWAYS ===
        educ_years >= 13.5 & has_trade_any ~ "Cert_IV_Trade",
        educ_years >= 13.5 & has_technical_any ~ "Cert_IV_Technical",
        educ_years >= 13.5 & has_nursing_any ~ "Cert_IV_Nursing",
        educ_years >= 13.5 ~ "Certificate_IV",
        
        # === CERTIFICATE III/TRADE PATHWAYS ===
        educ_years >= 13 & has_trade_any ~ "Cert_III_Trade",
        educ_years >= 13 & has_technical_any ~ "Cert_III_Technical",
        educ_years >= 13 & has_nursing_any ~ "Cert_III_Nursing",
        educ_years >= 13 ~ "Certificate_III",
        
        # === CERTIFICATE II ===
        educ_years >= 12.5 ~ "Certificate_II",
        
        # === CERTIFICATE I ===
        educ_years > 12 & educ_years < 12.5 ~ "Certificate_I",
        
        # === SECONDARY SCHOOL ===
        educ_years == 12 ~ "Year_12",
        educ_years == 11 ~ "Year_11",
        educ_years == 10 ~ "Year_10",
        educ_years == 9 ~ "Year_9",
        educ_years == 8 ~ "Year_8",
        educ_years == 7 ~ "Year_7",
        educ_years == 6 ~ "Primary_Complete",
        educ_years == 5 ~ "Primary_Incomplete",
        
        # === NO EDUCATION DATA ===
        is.na(educ_years) ~ "No_Education_Data",
        TRUE ~ "Other"
      ),
      
      # Education squared for non-linear effects
      educ_sq = educ_years^2,
      
      # Create education level categories
      education_level = case_when(
        educ_years >= 19 ~ "Doctorate",
        educ_years >= 18 ~ "Masters",
        educ_years >= 17 ~ "Graduate_Diploma_Certificate",
        educ_years >= 16.5 ~ "Bachelor_Honours",
        educ_years >= 16 ~ "Bachelor",
        educ_years >= 15 ~ "Advanced_Diploma",
        educ_years >= 14 ~ "Diploma",
        educ_years >= 13.5 ~ "Certificate_IV",
        educ_years >= 13 ~ "Certificate_III",
        educ_years >= 12.5 ~ "Certificate_II",
        educ_years > 12 ~ "Certificate_I",
        educ_years == 12 ~ "Year_12",
        educ_years == 11 ~ "Year_11",
        educ_years == 10 ~ "Year_10",
        educ_years < 10 & !is.na(educ_years) ~ "Below_Year_10",
        TRUE ~ "Missing"
      ),
      
      # Binary indicators for analysis
      has_degree = (educ_years >= 16),
      has_postsecondary = (educ_years > 12),
      has_diploma_plus = (educ_years >= 14),
      has_postgrad = (educ_years >= 17),
      
      # Data quality indicator
      education_data_source = case_when(
        edqrodc_raw == 1 | edqroms_raw == 1 | edqrogd_raw == 1 | edqrogc_raw == 1 ~ "Specific_Postgrad",
        edqrohd_raw == 1 | edqrobd_raw == 1 ~ "Specific_Bachelor",
        edqroav_raw == 1 | edqrodp_raw == 1 | edqroad_raw == 1 ~ "Specific_Diploma",
        edqroc1_raw == 1 | edqroc2_raw == 1 | edqroc3_raw == 1 | edqroc4_raw == 1 ~ "Specific_Certificate",
        !is.na(edhigh1_raw) ~ "General_Highest",
        !is.na(edhists_raw) ~ "School_History",
        !is.na(edsscmp_raw) ~ "CPQ_School",
        TRUE ~ "No_Data"
      )
    )
  
  # Education diagnostics
  cat("    - Education variables created\n")
  cat("    - Education coverage:", 
      round(100 * sum(!is.na(wave_data$educ_years)) / nrow(wave_data), 1), "%\n")
  cat("    - Mean education years:", round(mean(wave_data$educ_years, na.rm = TRUE), 2), "\n")
  cat("    - Nursing professionals:", sum(wave_data$has_nursing_any, na.rm = TRUE), "\n")
  cat("    - Teaching professionals:", sum(wave_data$has_teaching_any, na.rm = TRUE), "\n")
  cat("    - With degrees (16+):", sum(wave_data$has_degree, na.rm = TRUE), "\n")
  cat("    - With postgrad (17+):", sum(wave_data$has_postgrad, na.rm = TRUE), "\n")
  
  # ============================
  # STEP 4: EXPERIENCE AND JOB CHARACTERISTICS (CORRECTED)
  # ============================
  
  cat("  Step 4: Experience and job characteristics...\n")
  
  wave_data <- wave_data %>%
    mutate(
      # Experience
      # Use HILDA's cumulative employment history variable (ehtjb): total years in
      # paid work since leaving full-time education, constructed from the employment
      # history interview and updated each wave from the labour market activity
      # calendar. Directly captures actual experience including work-study overlap
      # and career interruptions -- superior to the Mincer potential experience proxy.
      # Fallback to potential experience (age - educ_years - 5) only when ehtjb is
      # missing. The Mincer formula ceiling is NOT applied: ehtjb legitimately exceeds
      # it for educated workers who worked during study (38-55% of post-secondary
      # respondents). Only a biological ceiling (age - 13, Australia's minimum working
      # age) is applied, removing 10 person-wave observations across 3 individuals
      # with implied labour market entry below age 10 (< 0.02% of sample).
      actual_experience    = safe_numeric_zero(., v$ehtjb),
      # ---- v2: non-employment history ----
      years_since_education = safe_numeric_zero(., v$ehtse),
      years_unemployed     = safe_numeric_zero(., v$ehtuj),
      years_not_in_lf      = safe_numeric_zero(., v$ehto),
      years_npq_total      = safe_numeric_zero(., v$ehttot),
      # NA-propagating (fixed): with the old na.rm = TRUE, a missing age or
      # educ_years collapsed to potential_experience = 0, and if ehtjb was
      # also missing, experience_years became a spurious 0 instead of NA.
      # Now missing inputs yield NA and such rows drop from experience-based
      # samples. NOTE: this can shift sample counts relative to the panel
      # used for the current manuscript; re-verify the FINAL VERIFICATION
      # block (esp. paper Sec. 3 counts) after rebuilding.
      potential_experience = pmax(0, age - educ_years - 5),
      experience_years     = pmin(
        coalesce(actual_experience, potential_experience),
        pmax(0, age - 13)            # biological ceiling: implied start age >= 13
      ),
      experience_years     = ifelse(experience_years < 0, 0, experience_years),
      # ---- v2: experience provenance ----
      # experience_years is ehtjb where available and the Mincer potential
      # proxy otherwise. If the fallback rate differs between mothers and
      # fathers, or across event time, part of the measured experience gap
      # is a measurement artefact rather than a behavioural one. Report the
      # fallback rate by gender and event time before interpreting the gap.
      experience_source = case_when(
        !is.na(actual_experience)                       ~ "ehtjb_actual",
        is.na(actual_experience) & !is.na(potential_experience) ~ "mincer_proxy",
        TRUE                                            ~ NA_character_
      ),
      experience_is_proxy = ifelse(is.na(experience_source), NA_real_,
                                   as.numeric(experience_source == "mincer_proxy")),
      # Binding ceiling flag: experience_years was truncated by age-13.
      experience_ceiling_bound = ifelse(
        is.na(actual_experience) | is.na(age), NA_real_,
        as.numeric(actual_experience > (age - 13))
      ),
      # ---- v4: career exposure and interruption SHARES ----
      # ehtse (time since leaving FT education) is the natural exposure
      # denominator, and a better one than the Mincer construction
      # age - educ_years - 5 because it is reported rather than imputed.
      # Expressing the interruption as a SHARE of exposure normalises for
      # career length, which matters here: the ehto LEVEL showed a steep
      # entry-cohort gradient (mothers 1.72 years in the 2019 intake
      # against 4.93 in the original cohort), part age composition and
      # part recall vintage. Shares are far more comparable across
      # intakes. Prefer these, or the within-person flows, over levels.
      olf_share = ifelse(
        is.na(years_not_in_lf) | is.na(years_since_education) |
          years_since_education <= 0,
        NA_real_, years_not_in_lf / years_since_education
      ),
      unemp_share = ifelse(
        is.na(years_unemployed) | is.na(years_since_education) |
          years_since_education <= 0,
        NA_real_, years_unemployed / years_since_education
      ),
      employment_share = ifelse(
        is.na(actual_experience) | is.na(years_since_education) |
          years_since_education <= 0,
        NA_real_, actual_experience / years_since_education
      ),
      
      # Non-employment years. v3: now correctly includes OLF time (ehto),
      # which is where mothers' interruptions actually live. The v2
      # version summed unemployment only, because ehtnl was all-NA.
      nonemployment_years = ifelse(
        is.na(years_unemployed) & is.na(years_not_in_lf), NA_real_,
        coalesce(years_unemployed, 0) + coalesce(years_not_in_lf, 0)
      ),
      has_interruption = ifelse(is.na(nonemployment_years), NA_real_,
                                as.numeric(nonemployment_years > 0)),
      has_olf_spell = ifelse(is.na(years_not_in_lf), NA_real_,
                             as.numeric(years_not_in_lf > 0)),
      
      # ---- v4: reconciliation on the CORRECT identity ----
      # The three components should exhaust the denominator:
      #   ehtjb + ehtuj + ehto ~ ehtse
      # history_residual is the unaccounted time. A median near zero
      # means the decomposition is complete. A systematically positive
      # residual is time the three components do not classify, which
      # would be worth carrying as its own quantity.
      history_components = ifelse(
        is.na(actual_experience) & is.na(years_unemployed) &
          is.na(years_not_in_lf),
        NA_real_,
        coalesce(actual_experience, 0) + coalesce(years_unemployed, 0) +
          coalesce(years_not_in_lf, 0)
      ),
      history_residual = ifelse(
        is.na(history_components) | is.na(years_since_education), NA_real_,
        years_since_education - history_components
      ),
      # Secondary cross-check against the NPQ manual total. ehttot is an
      # NPQ item rather than a History DV, so it is available for fewer
      # person-waves and is a weaker check than history_residual.
      npq_total_discrepancy = ifelse(
        is.na(years_since_education) | is.na(years_npq_total), NA_real_,
        years_since_education - years_npq_total
      ),
      experience_sq        = experience_years^2,
      
      # Total number of training days across all types.
      # v21.48: UNGATED. A missing count reads as zero, both for a trainee
      # who gave none and for anyone never asked. Kept so published
      # numbers reproduce; total_training_days_z below is the gated twin.
      total_training_days = coalesce(jttdays_raw, 0),
      
      # Average hours per course day (already available as jtthrs_raw)
      avg_training_hours = jtthrs_raw,
      
      # Total number of courses attended in last 12 months.
      # v21.48: UNGATED, for the same reason. total_courses_z below is the
      # gated twin and is the one the courses dosage estimate should use.
      total_courses = coalesce(jttnum_raw, 0),
      
      # Indicators for training done in own time or paid work time
      # (pooled over place of employment + some other place)
      # NOTE: "mentioned" coding made robust with == 1, which is correct
      # under both 0/1 and 1/2 (yes/no) HILDA codings. The previous "> 0"
      # rule would count 2 = "not mentioned" as training if the items are
      # 1/2-coded; verify with table(jttpewt_raw) on the first run.
      training_own_time = ifelse(
        coalesce(jttopot_raw, 0) == 1 | coalesce(jttpeot_raw, 0) == 1, 1, 0),
      training_paid_time = ifelse(
        coalesce(jttopwt_raw, 0) == 1 | coalesce(jttpewt_raw, 0) == 1, 1, 0),
      
      # PLACE-OF-EMPLOYMENT-ONLY variants (for training_event_study.R):
      # jttpewt = at place of employment, during paid work time
      # jttpeot = at place of employment, in own time
      jttpewt_any = ifelse(coalesce(jttpewt_raw, 0) == 1, 1, 0),
      jttpeot_any = ifelse(coalesce(jttpeot_raw, 0) == 1, 1, 0),
      jttopwt_any = ifelse(coalesce(jttopwt_raw, 0) == 1, 1, 0),
      jttopot_any = ifelse(coalesce(jttopot_raw, 0) == 1, 1, 0),
      
      # ---- v21.6: THE MODULE GATE -------------------------------------
      # The four indicators above coalesce a missing answer to 0, so they
      # are never NA and three states arrive as the same zero: asked and
      # did not train, asked and trained elsewhere, and never asked. The
      # third is not a behavioural zero and it is not missing at random,
      # because the module is put to the employed; at the birth wave the
      # employed-only composition changes and a structural zero reads as
      # a fall in training.
      #
      # The gate is built on NON-MISSINGNESS, not on a code, so it is
      # right whether the release codes E6 as 0/1 or 1/2. jttrwrk is the
      # gate proper; the other seven items are a fallback so that a wave
      # where E6 failed to resolve still gets a usable gate rather than
      # an all-zero one.
      training_module_asked = as.integer(
        !is.na(jttrwrk_raw)  | !is.na(jttdays_raw) | !is.na(jtthrs_raw) |
          !is.na(jttnum_raw)   | !is.na(jttopot_raw) | !is.na(jttopwt_raw) |
          !is.na(jttpeot_raw)  | !is.na(jttpewt_raw)),
      # E6 itself, NA-propagating. == 1 matches the convention used for
      # every other mentioned-style item here and is verified after
      # combination rather than asserted.
      training_any_reported = ifelse(is.na(jttrwrk_raw), NA_real_,
                                     as.numeric(jttrwrk_raw == 1)),
      # The gated twins. Same value as the _any columns wherever the
      # module was asked, NA where it was not. THESE ARE THE COLUMNS TO
      # USE. A design that wants the structural zero can still read the
      # _any pair, but it then has to say that it is treating "never
      # asked" as "did not train".
      jttpewt_obs = ifelse(training_module_asked == 1L, jttpewt_any, NA_real_),
      jttpeot_obs = ifelse(training_module_asked == 1L, jttpeot_any, NA_real_),
      training_own_time_obs  = ifelse(training_module_asked == 1L,
                                      training_own_time,  NA_real_),
      training_paid_time_obs = ifelse(training_module_asked == 1L,
                                      training_paid_time, NA_real_),
      
      # ---- v2: training INTENSITY, not just incidence ----
      # jttdays x jtthrs is total hours of work-related training in the
      # last 12 months. Intensity is more informative than the binary
      # incidence measure: it separates mothers who stop training from
      # mothers who train less. Constructed explicitly here rather than
      # relied on via any_of() in the final select, where a silent absence
      # would go unnoticed.
      total_training_hours = ifelse(
        is.na(jttdays_raw) | is.na(jtthrs_raw), NA_real_,
        jttdays_raw * jtthrs_raw
      ),
      any_training = ifelse(
        is.na(training_own_time) & is.na(training_paid_time), NA_real_,
        as.numeric(coalesce(training_own_time, 0) == 1 |
                     coalesce(training_paid_time, 0) == 1)
      ),
      
      # v21.9. THE DOSAGE MARGIN, WITH ITS STRUCTURAL ZEROS.
      #
      # total_training_hours above is jttdays x jtthrs, and both items are
      # put only to people who reported training, so it is NA for everyone
      # who trained none. That is an NA meaning "no training", not an NA
      # meaning "not known", and leaving it as one makes the column
      # observed on about a THIRD of the employed and SELECTED ON THE
      # OUTCOME: the only person-waves with a dosage are the ones with a
      # positive dosage, so any regression on it conditions on training.
      #
      # 03_training_designs.R was recoding this itself, under the name
      # train_hours_z, and recovering coverage from 32.5% to 99.8% of the
      # employed. That is the right recode and the wrong place for it: the
      # next script wanting an unconditional dosage margin writes its own,
      # and then two scripts have two definitions of the same variable.
      #
      # ONE DIFFERENCE FROM THE 03 VERSION, DELIBERATE. 03 keyed on
      # any_training == 0, which is itself a coalesced column, so a
      # NEVER-ASKED person-wave read as zero training and therefore as
      # zero HOURS. That silently pushes the T1 structural-zero problem
      # into the dosage margin. Here a never-asked person-wave is NA:
      #   never asked                       -> NA, not known
      #   asked, reported no training       -> 0, a true zero
      #   asked, trained, days/hours given  -> the product
      #   asked, trained, days/hours missing-> NA, item non-response
      # The zeros in this column are therefore MEASURED zeros only. On the
      # v21.8 panel that is 17 employed person-waves' difference from the
      # 03 version, so the estimates move in the fourth decimal and the
      # meaning of the column changes entirely.
      total_training_hours_z = ifelse(
        training_module_asked == 0L, NA_real_,
        ifelse(!is.na(any_training) & any_training == 0, 0,
               total_training_hours)
      ),

      # v21.48. DAYS AND COURSES ON THE SAME FOUR STATES AS HOURS.
      #   never asked                     -> NA, not known
      #   asked, did not train            -> 0, a measured zero
      #   asked, trained, count given     -> the count
      #   asked, trained, count missing   -> NA, item non-response
      # "Did not train" is any_training == 0, exactly as in the hours
      # column, so the three dosage margins agree about who trained. The
      # dosage diagnostic after combination counts the person-waves where
      # that test overrides a reported count.
      total_training_days_z = ifelse(
        training_module_asked == 0L, NA_real_,
        ifelse(!is.na(any_training) & any_training == 0, 0, jttdays_raw)),
      total_courses_z = ifelse(
        training_module_asked == 0L, NA_real_,
        ifelse(!is.na(any_training) & any_training == 0, 0, jttnum_raw)),
      
      # Tenure
      tenure_employer = safe_numeric_zero(., v$jbempt),
      tenure_occupation = safe_numeric_zero(., v$jbocct),
      
      # Employment type 
      # v21.28. NA OFF THE POPULATION, NOT ZERO.
      #
      # jbcasab is a DV whose dictionary population is "Employed", coded
      # 1 = Casual, 2 = Permanent. The old ifelse(x == 1, 1, 0) sent every
      # non-1 value to ZERO, so the 7,453 wave-24 person-waves coded
      # [-1] Not asked (the non-employed) plus the [-4] refusals all
      # arrived as casual = 0 AND permanent = 0. A regression using either
      # column without conditioning on employment was contrasting casual
      # workers against permanent workers AND the non-employed pooled,
      # and nothing announced it: 0 is a legal value of an indicator.
      #
      # Both columns are now NA off the employed base, so a caller who
      # forgets the condition loses rows visibly instead of silently
      # comparing the wrong groups. HILDA missing codes run -1 to -10 and
      # are excluded explicitly rather than by relying on == 1 being
      # FALSE for them.
      #
      # THE ABS DEFINITION FOLDS "DON'T KNOW" INTO CASUAL. Per the
      # dictionary note, casual means not entitled to paid holiday leave
      # OR DID NOT KNOW, and the same for sick leave. The measure is a
      # lower bound on entitlement, not a clean contract type.
      .jbcasab_num = safe_numeric(., v$jbcasab),
      casual = ifelse(is.na(.jbcasab_num) | .jbcasab_num < 0, NA_real_,
                      as.numeric(.jbcasab_num == 1)),
      permanent = ifelse(is.na(.jbcasab_num) | .jbcasab_num < 0, NA_real_,
                         as.numeric(.jbcasab_num == 2)),
      
      # Leave
      has_holiday_leave = ifelse(safe_numeric(., v$jbmhl) == 1, 1, 0),
      has_sick_leave = ifelse(safe_numeric(., v$jbmsl) == 1, 1, 0),
      leave_none = ifelse(has_holiday_leave == 0 & has_sick_leave == 0, 1, 0),
      
      # Schedule
      jbmsch_raw = safe_numeric(., v$jbmsch),
      # v21.44. GUARD BEFORE MEMBERSHIP. %in% is NA-blind:
      # NA %in% c(2,3,4,5) is FALSE, never NA, so every person-wave
      # never asked jbmsch arrived as 0 on BOTH of the lines below and
      # read as a standard-hours worker on neither. jbmsch is
      # employment-routed, so that is the non-employed.
      #
      # has_standard_hours is UNCHANGED IN BEHAVIOUR: NA == 1 is NA, so
      # it always propagated correctly. It is rewritten in the same
      # idiom because two idioms side by side is what let the other two
      # hide, and because its NA count is the never-asked set that makes
      # the imputation on the others countable.
      has_standard_hours  = ifelse(is.na(jbmsch_raw), NA_real_,
                                   as.numeric(jbmsch_raw == 1)),
      has_shift_work      = ifelse(is.na(jbmsch_raw), NA_real_,
                                   as.numeric(jbmsch_raw %in% c(2,3,4,5))),
      has_irregular_hours = ifelse(is.na(jbmsch_raw), NA_real_,
                                   as.numeric(jbmsch_raw %in% c(6,7))),
      
      # Hours preferences
      jbhrcpr_raw = safe_numeric(., v$jbhrcpr),
      underemployed = ifelse(jbhrcpr_raw == 3, 1, 0),
      overemployed = ifelse(jbhrcpr_raw == 1, 1, 0),
      
      # Union and sector
      union_member = ifelse(safe_numeric(., v$jbmtuea) == 1, 1, 0),
      # v21.44. THE SAME FAULT, AND THE GUARD BELOW IT WAS DEAD CODE.
      # The old pair read:
      #   public_sector = ifelse(safe_numeric(jbmmply) %in% c(2,5), 1, 0)
      #   public_sector = ifelse(is.na(public_sector), 0, public_sector)
      # The first can never produce NA, so the second never fired while
      # reading as though the missing had been handled. Measured on the
      # event frame: 10,329 of 10,410 non-employed person-waves, 99.2%,
      # were imputed PRIVATE SECTOR. The count is exact because
      # union_member is co-routed and built with ==, so its NA count is
      # the never-asked set: 10,329 imputed plus 67 genuine private
      # answers is the 10,396 zeros observed.
      #
      # The intermediate follows .jbcasab_num, added when the same fault
      # was fixed on casual/permanent a few lines above.
      .jbmmply_num = safe_numeric(., v$jbmmply),
      public_sector = ifelse(is.na(.jbmmply_num), NA_real_,
                             as.numeric(.jbmmply_num %in% c(2,5))),
      
      # Part-time reason
      jbptrea_raw = safe_numeric(., v$jbptrea),
      pt_childcare = ifelse(parttime == 1 & jbptrea_raw == 2, 1, 0),
      pt_involuntary = ifelse(parttime == 1 & jbptrea_raw == 6, 1, 0)
    )
  
  # FIRM SIZE - SEPARATE MUTATE FOR CLARITY
  #
  # v19. HILDA replaces jbmemsz with jbmems2 at wave 22 and the loader
  # already switches instrument and mapping. It does NOT produce a
  # comparable series. Across the W12-W24 run the classified small-firm
  # share is 5% through wave 21 and 29% from wave 22, and non-missing
  # coverage rises from roughly 6,500 to 10,300 person-waves. Neither
  # jump is behaviour. Any firm-size control, split or interaction that
  # spans 2021/22 will read the instrument change as an effect.
  #
  # No harmonised mapping is invented here, because the correct one
  # cannot be recovered from the code without the code frames. Three
  # things are done instead: the raw distribution and value labels are
  # printed per wave so the frames can be read off the log; the
  # instrument is recorded on the panel as firm_size_instrument; and
  # firm_size_comparable is NA outside whichever regime is declared
  # verified below, so an unverified series cannot reach an estimate
  # silently. Set FIRM_SIZE_VERIFIED once the frames are confirmed.
  wave_data <- wave_data %>%
    mutate(
      # First extract the raw firm size variable
      firm_size_raw = if(wave_num <= FIRM_SIZE_BREAK_WAVE - 1L) {
        safe_numeric(., v$jbmemsz)
      } else {
        safe_numeric(., v$jbmems2)
      },
      firm_size_instrument = if (wave_num <= FIRM_SIZE_BREAK_WAVE - 1L)
        "jbmemsz" else "jbmems2"
    ) %>%
    mutate(
      # Then map to categories
      firm_size = case_when(
        # Old waves <=21
        wave_num <= 21 & firm_size_raw == 1 ~ "Small",
        wave_num <= 21 & firm_size_raw %in% 2:3 ~ "Medium",
        wave_num <= 21 & firm_size_raw %in% 4:9 ~ "Large",
        
        # New waves >=22
        wave_num >= 22 & firm_size_raw %in% 1:4 ~ "Small",
        wave_num >= 22 & firm_size_raw %in% 5:6 ~ "Medium",
        wave_num >= 22 & firm_size_raw %in% 7:10 ~ "Large",
        wave_num >= 22 & firm_size_raw == 11 ~ "Small",
        wave_num >= 22 & firm_size_raw %in% 12:13 ~ "Large",
        
        TRUE ~ NA_character_
      ),
      # v21.46. THIS IS NOW A PLACEHOLDER AND THE COLUMN IS FILLED AFTER
      # COMBINATION. The value written here is overwritten in the
      # analysis-stage restrictions block with the regime-tagged series,
      # which needs the wave and firm_size together and therefore cannot
      # be built one wave at a time without repeating the regime logic.
      # The line stays so that a cached wave carries the column and the
      # final select() finds it whether or not the fill has run.
      #
      # FIRM_SIZE_VERIFIED still gates it, and still reads FALSE, because
      # the coverage break it stands for is real. What changed is what an
      # ungated column looks like: not the raw three levels, which would
      # be the misuse the gate was protecting against, but six levels
      # that name their own regime and so cannot be averaged across it.
      firm_size_comparable = if (isTRUE(FIRM_SIZE_VERIFIED))
        firm_size else NA_character_,
      
      # Commute
      job_distance_cat = safe_numeric(., v$jbmlkr),
      job_distance_km = case_when(
        job_distance_cat == 0 ~ 0,
        job_distance_cat == 1 ~ 2.5,
        job_distance_cat == 2 ~ 7,
        job_distance_cat == 3 ~ 12,
        job_distance_cat == 4 ~ 17,
        job_distance_cat == 5 ~ 24.5,
        job_distance_cat == 6 ~ 39.5,
        job_distance_cat == 7 ~ 74.5,
        job_distance_cat == 8 ~ 124.5,
        job_distance_cat == 9 ~ 150,
        TRUE ~ NA_real_
      )
    )
  
  # Add diagnostic output for firm size
  cat("    - Firm size distribution (instrument: ",
      wave_data$firm_size_instrument[1], ")\n", sep = "")
  firm_dist <- table(wave_data$firm_size, useNA = "ifany")
  print(firm_dist)
  
  # v19: the raw code frame, which is what actually has to be compared
  # across the wave-22 break. Value labels are printed where haven
  # carried them, so the two frames can be aligned from the log without
  # opening the data dictionary.
  .fs_nm <- if (wave_num <= FIRM_SIZE_BREAK_WAVE - 1L) v$jbmemsz else v$jbmems2
  if (!is.null(.fs_nm) && !is.na(.fs_nm) && .fs_nm %in% names(raw)) {
    cat("    - Raw", .fs_nm, "codes (valid only):\n")
    print(table(wave_data$firm_size_raw, useNA = "no"))
    .lab <- attr(raw[[.fs_nm]], "labels")
    if (!is.null(.lab)) {
      .lab <- .lab[.lab >= 0]
      if (length(.lab)) {
        cat("    - Value labels:\n")
        print(data.frame(code = as.integer(.lab), label = names(.lab),
                         row.names = NULL))
      }
    }
    cat(sprintf("    - Classified %d of %d employed (%.1f%%)\n",
                sum(!is.na(wave_data$firm_size)),
                sum(wave_data$employed == 1, na.rm = TRUE),
                100 * sum(!is.na(wave_data$firm_size)) /
                  max(1, sum(wave_data$employed == 1, na.rm = TRUE))))
  } else {
    cat("    - Firm-size mnemonic ABSENT this wave; firm_size is NA.\n")
  }
  
  # ============================
  # STEP 5: OCCUPATION AND INDUSTRY
  # ============================
  
  cat("  Step 5: Occupation and industry...\n")
  
  wave_data <- wave_data %>%
    mutate(
      # Occupation
      occupation_major_code = safe_numeric(., v$jbmo61),
      occupation_major = case_when(
        occupation_major_code == 1 ~ "Managers",
        occupation_major_code == 2 ~ "Professionals",
        occupation_major_code == 3 ~ "Technicians_Trades",
        occupation_major_code == 4 ~ "Community_Service",
        occupation_major_code == 5 ~ "Clerical_Admin",
        occupation_major_code == 6 ~ "Sales",
        occupation_major_code == 7 ~ "Machinery_Operators",
        occupation_major_code == 8 ~ "Labourers",
        TRUE ~ NA_character_
      ),
      
      # Industry
      industry_major_code = safe_numeric(., v$jbmi61),
      industry_major = case_when(
        industry_major_code == 1 ~ "Agriculture",
        industry_major_code == 2 ~ "Mining",
        industry_major_code == 3 ~ "Manufacturing",
        industry_major_code == 4 ~ "Utilities",
        industry_major_code == 5 ~ "Construction",
        industry_major_code == 6 ~ "Wholesale",
        industry_major_code == 7 ~ "Retail",
        industry_major_code == 8 ~ "Hospitality",
        industry_major_code == 9 ~ "Transport",
        industry_major_code == 10 ~ "Information",
        industry_major_code == 11 ~ "Finance",
        industry_major_code == 12 ~ "Real_Estate",
        industry_major_code == 13 ~ "Professional",
        industry_major_code == 14 ~ "Administrative",
        industry_major_code == 15 ~ "Public_Admin",
        industry_major_code == 16 ~ "Education",
        industry_major_code == 17 ~ "Health",
        industry_major_code == 18 ~ "Arts",
        industry_major_code == 19 ~ "Other_Services",
        TRUE ~ NA_character_
      ),
      
      # Field of education
      occupation_field_code = safe_numeric(., v$edpsfdn)
    )
  
  # ============================
  # STEP 6: CHILDREN, FERTILITY HISTORY, AND FLEXIBILITY
  # ============================
  
  cat("  Step 6: Children, fertility history, flexibility, and care...\n")
  
  wave_data <- wave_data %>%
    mutate(
      # -----------------------------------------------------------
      # 6A. HOUSEHOLD CHILDREN COUNTS (original)
      # -----------------------------------------------------------
      num_children_0to4 = safe_numeric_zero(., v$hhd0_4),
      num_children_5to9 = safe_numeric_zero(., v$hhd5_9),
      num_children_10to14 = safe_numeric_zero(., v$hhd1014),
      num_children_15to24 = safe_numeric_zero(., v$hhd1524),
      
      num_dependent_children = rowSums(
        cbind(num_children_0to4, num_children_5to9, 
              num_children_10to14, num_children_15to24),
        na.rm = TRUE
      ),
      
      num_children_under15 = rowSums(
        cbind(num_children_0to4, num_children_5to9, num_children_10to14),
        na.rm = TRUE
      ),
      
      # Non-resident children (original)
      ncchk_raw = safe_numeric(., v$ncchk),
      ncany_raw = safe_numeric(., v$ncany),
      has_nonresident_children_under18 = (ncchk_raw %in% 1:2),
      has_any_nonresident_children = (ncany_raw == 1),
      
      # -----------------------------------------------------------
      # 6B. FERTILITY HISTORY (tc* prefix — NEW)
      # -----------------------------------------------------------
      
      # tchad: "Total children ever had" — History variable, absorbing
      #   Available most waves as History or PQ/NPQ
      tchad_raw  = safe_numeric_zero(., v$tchad),
      tchadn_raw = safe_numeric_zero(., v$tchadn),
      tchave_raw = safe_numeric_zero(., v$tchave),
      tchch_raw  = safe_numeric(., v$tchch),       # Has ever had children check (fertility module waves)
      
      # Best available total children ever had (prefer tchad, fallback tchadn, tchave)
      total_children_ever_had = coalesce(tchad_raw, tchadn_raw, tchave_raw),
      
      # Own children counts — DV, available ALL waves (most reliable)
      num_own_resident_children    = safe_numeric_zero(., v$tcr),
      num_own_nonresident_children = safe_numeric_zero(., v$tcnr),
      # v21.40. rowSums(..., na.rm = TRUE) RETURNS 0 WHEN EVERY COMPONENT
      # IS MISSING, and this file already knows that. The precarious_score
      # comment three thousand lines below says it in full - the
      # non-employed scored 0 and were labelled maximally secure - and
      # total_unpaid_hours applies the remedy. This column never got it.
      #
      # WHY IT MATTERS MORE HERE THAN THERE. It is not just that the column
      # is wrong. ever_parent reads it three lines down:
      #
      #     is.na(total_children_ever_had) & total_own_children == 0 &
      #       num_children_under15 == 0 & ... ~ 0L
      #
      # A person-wave with BOTH child components missing and tchad missing
      # satisfies `total_own_children == 0` and is classified CHILDLESS
      # rather than unknown. Those people become never-treated, so the
      # defect lands in the estimation roster on the side that is hardest
      # to notice: a never-treated group that is slightly too large looks
      # like a well-powered control group.
      #
      # NA now propagates, using the same coalesce form as
      # noncash_total_weekly: missing only when NEITHER component is
      # observed, so one observed component still gives a total.
      #
      # THIS CHANGES ever_parent AND THEREFORE THE ROSTER. The diagnostic
      # after this mutate reports how many person-waves and persons move,
      # because a fix that silently shifts the sample is worse than the
      # defect it removes. Check that count against 2,704 / 7,946 / 10,650
      # before accepting it.
      total_own_children = ifelse(
        is.na(num_own_resident_children) & is.na(num_own_nonresident_children),
        NA_real_,
        dplyr::coalesce(num_own_resident_children, 0) +
          dplyr::coalesce(num_own_nonresident_children, 0)),
      # Kept as the pre-fix value so the diagnostic can measure the change
      # rather than assert it.
      total_own_children_narm0 = rowSums(
        cbind(num_own_resident_children, num_own_nonresident_children),
        na.rm = TRUE
      ),
      
      # Age of youngest own child (DV, all waves)
      age_youngest_own_child = safe_numeric(., v$tcyng),
      
      # Children since died
      total_children_died = safe_numeric_zero(., v$tcdied),
      
      # Resident own children + step/foster/grand by age group (DV, all waves)
      own_res_children_0to4   = safe_numeric_zero(., v$tcr04),
      own_res_children_5to14  = safe_numeric_zero(., v$tcr514),
      own_res_children_15to24 = safe_numeric_zero(., v$tcr1524),
      own_res_children_25plus = safe_numeric_zero(., v$tcr25),
      
      # Non-resident own children by age group (DV, all waves)
      own_nonres_children_0to4   = safe_numeric_zero(., v$tcn04),
      own_nonres_children_5to14  = safe_numeric_zero(., v$tcn514),
      own_nonres_children_15to24 = safe_numeric_zero(., v$tcn1524),
      own_nonres_children_25plus = safe_numeric_zero(., v$tcn25),
      
      # Step/foster/grandchildren with no parent in HH
      num_step_foster_grand = safe_numeric_zero(., v$rcstepn),
      
      # Child ages (for estimating age at first birth)
      age_resident_child_1    = safe_numeric(., v$rcage1),
      age_resident_child_2    = safe_numeric(., v$rcage2),
      age_resident_child_3    = safe_numeric(., v$rcage3),
      age_nonresident_child_1 = safe_numeric(., v$ncage1),
      age_nonresident_child_2 = safe_numeric(., v$ncage2),
      
      # -----------------------------------------------------------
      # 6C. ABSORBING EVER-PARENT INDICATOR (NEW)
      # -----------------------------------------------------------
      # Three sources combined for maximum coverage:
      #   1. tchad > 0          (total children ever had — History, most waves)
      #   2. tcr + tcnr > 0     (own resident + non-resident — DV, all waves)
      #   3. hhd children > 0   (household children — original, all waves)
      #   4. ncany == 1         (any non-resident children — original, all waves)
      # Any one sufficient to classify as ever-parent.
      #
      # v21.49: this is the OLD rule. After the waves are combined, this
      # value is kept as ever_parent_ref and ever_parent is rebuilt
      # without source 3, which counts every child in the household.
      # The logic here is unchanged, so the caches stay valid.
      
      ever_parent = case_when(
        total_children_ever_had > 0 ~ 1L,
        total_own_children > 0 ~ 1L,
        num_children_under15 > 0 ~ 1L,
        has_any_nonresident_children == TRUE ~ 1L,
        # If tchad is NA but all other sources are 0, assume childless
        is.na(total_children_ever_had) & total_own_children == 0 & 
          num_children_under15 == 0 & (is.na(has_any_nonresident_children) | has_any_nonresident_children == FALSE) ~ 0L,
        # If all available and zero
        total_children_ever_had == 0 & total_own_children == 0 ~ 0L,
        TRUE ~ NA_integer_
      ),
      
      # Gender-specific ever-parent
      ever_mother = ifelse(female == 1, ever_parent, NA_integer_),
      ever_father = ifelse(female == 0, ever_parent, NA_integer_),
      
      # -----------------------------------------------------------
      # 6D. THREE-WAY MOTHERHOOD CLASSIFICATION (NEW)
      # -----------------------------------------------------------
      # never_mother:  ever_parent == 0 (never had children by any measure)
      # active_mother: ever_parent == 1 AND has children under 15 in HH now
      # post_mother:   ever_parent == 1 AND NO children under 15 in HH now
      
      motherhood_status = case_when(
        female == 0 ~ NA_character_,
        ever_parent == 1 & num_children_under15 > 0 ~ "active_mother",
        ever_parent == 1 & num_children_under15 == 0 ~ "post_mother",
        ever_parent == 0 ~ "never_mother",
        TRUE ~ NA_character_
      ),
      
      # Fatherhood equivalent
      fatherhood_status = case_when(
        female == 1 ~ NA_character_,
        ever_parent == 1 & num_children_under15 > 0 ~ "active_father",
        ever_parent == 1 & num_children_under15 == 0 ~ "post_father",
        ever_parent == 0 ~ "never_father",
        TRUE ~ NA_character_
      ),
      
      # Approximate age at first birth (from oldest known child)
      age_oldest_child = pmax(age_resident_child_1, age_resident_child_2,
                              age_resident_child_3, age_nonresident_child_1,
                              age_nonresident_child_2, na.rm = TRUE),
      approx_age_at_first_birth = ifelse(!is.na(age_oldest_child) & ever_parent == 1,
                                         age - age_oldest_child, NA_real_),
      
      # -----------------------------------------------------------
      # 6E. ORIGINAL CHILD INDICATORS (preserved for backward compatibility)
      # -----------------------------------------------------------
      has_young_children = ifelse(num_children_0to4 > 0, 1, 0),
      active_caregiver = ifelse(num_dependent_children > 0, 1, 0),
      is_mother = ifelse(female == 1 & num_dependent_children > 0, 1, 0),
      is_father = ifelse(female == 0 & num_dependent_children > 0, 1, 0),
      
      # -----------------------------------------------------------
      # 6F. PARENTING ATTITUDES (NEW)
      # -----------------------------------------------------------
      parenting_resp      = safe_numeric(., v$paresp),    # Has responsibilities, kids <= 17
      parenting_hard      = safe_numeric(., v$pahard),    # "Harder than I thought"
      parenting_tired     = safe_numeric(., v$patird),    # "Often tired/exhausted"
      parenting_trapped   = safe_numeric(., v$patrap),    # "Feel trapped"
      parenting_fairshare = safe_numeric(., v$pashare),   # "Do fair share"
      
      # -----------------------------------------------------------
      # 6G. PREGNANCY (fertility module waves: 15, 19, 23 in our panel)
      # Will be NA for non-fertility-module waves — this is expected
      # -----------------------------------------------------------
      currently_pregnant       = safe_numeric(., v$ftcpg),
      partner_pregnant         = safe_numeric(., v$ftcppg),
      self_or_partner_pregnant = safe_numeric(., v$ftipg),
      expected_birth_month     = safe_numeric(., v$ftcpgm),
      expected_birth_year      = safe_numeric(., v$ftcpgy),
      
      # -----------------------------------------------------------
      # 6H. LIFE EVENTS (asked annually — all waves) (NEW)
      # -----------------------------------------------------------
      # v17 HARD RENAME. event_birth was leprg, the PREGNANCY item,
      # for sixteen versions. Old names are gone, not aliased: an
      # alias would let 02, 03 and 06 keep reading a pregnancy clock
      # while believing it is a birth clock.
      event_pregnancy = safe_numeric(., v$leprg),   # SCQ B23d pregnancy
      event_child     = safe_numeric(., v$lebth),   # SCQ B23e birth/adoption
      event_married   = safe_numeric(., v$lemar),   # Got married
      event_separated = safe_numeric(., v$lesep),   # Separated from spouse
      event_job_change= safe_numeric(., v$lejob),   # Changed jobs
      event_fired     = safe_numeric(., v$lejlf),   # Fired/made redundant
      event_moved     = safe_numeric(., v$lemvd),   # Moved residence
      
      # Binary indicators for life events (HILDA coding: 1=No, 2=Yes)
      had_pregnancy_this_wave = ifelse(event_pregnancy == .yes_leprg, 1L, 0L),
      had_child_this_wave     = ifelse(event_child     == .yes_lebth, 1L, 0L),
      
      # --- v17 quarter flags, under the empirically determined code ---
      # lebthna is CARRIED, not collapsed into missing: a respondent who
      # reports the event and declines to date it is a different case
      # from one who did not report it.
      birth_q1 = as.integer(safe_numeric(., v$lebthq1) == .set_lebth),
      birth_q2 = as.integer(safe_numeric(., v$lebthq2) == .set_lebth),
      birth_q3 = as.integer(safe_numeric(., v$lebthq3) == .set_lebth),
      birth_q4 = as.integer(safe_numeric(., v$lebthq4) == .set_lebth),
      # v19: n_set MOVED ABOVE the declined flag. mutate() evaluates in
      # order, so birth_q_declined could not previously reference it.
      birth_q_n_set = rowSums(cbind(birth_q1, birth_q2, birth_q3, birth_q4),
                              na.rm = TRUE),
      # v18: lebthna is set for everyone who did not report the event,
      # so unconditionally it flagged 113,192 person-waves. Conditioned
      # on the event it is the raw survey act: the respondent reported a
      # birth and ticked the "cannot say which quarter" box.
      #
      # v19: this is NOT the same object as "reported but not dated".
      # Four respondents in the W12-W24 panel tick a quarter AND the NA
      # box (2015, 2021, 2023 and one of the 2024 multiple-flag cases),
      # so birth_q_declined summed to 342 against 338 zero-flag reports
      # and the identity reported = dated + declined + multiple missed
      # by exactly those four. Both objects are now carried: the raw
      # flag for the survey-behaviour question, birth_undated for event
      # dating. birth_undated is 338 BY CONSTRUCTION and is the one the
      # window logic and any event-time coding must use.
      birth_q_declined = as.integer(safe_numeric(., v$lebthna) == .set_lebth &
                                      event_child == .yes_lebth),
      birth_undated = as.integer(event_child == .yes_lebth &
                                   birth_q_n_set == 0L),
      birth_q_declined_dated = as.integer(birth_q_declined == 1L &
                                            birth_q_n_set > 0L),
      birth_quarter = dplyr::case_when(
        is.na(.set_lebth)     ~ NA_integer_,
        birth_q_n_set != 1L   ~ NA_integer_,
        birth_q1 == 1L        ~ 1L,
        birth_q2 == 1L        ~ 2L,
        birth_q3 == 1L        ~ 3L,
        birth_q4 == 1L        ~ 4L,
        TRUE                  ~ NA_integer_
      ),
      birth_quarter_multi = as.integer(birth_q_n_set > 1L),
      # v20: four person-waves in W12-W24 carry a single quarter flag
      # while had_child_this_wave is not 1. birth_quarter therefore has
      # 6,673 non-missing against 6,669 reporter-dated births, and the
      # v18 reconciliation subtracted an UNRESTRICTED dated count from a
      # reporter-restricted numerator, which is what produced the
      # apparent one-case overshoot in 2015, 2021, 2023 and 2024. The
      # NA-box flag was never double-coded: lebthna partitions the
      # reports exactly.
      #
      # birth_quarter is left as the raw derivation. birth_quarter_event
      # is the one every downstream use must take, because a birth
      # quarter for someone who did not report a birth is not
      # interpretable as event time. The orphans are flagged rather than
      # dropped: whether they are contradictions (event answered "no")
      # or recoverable reports (event left blank, quarter given) is
      # decided by the diagnostic after combination, not here.
      birth_quarter_event = ifelse(event_child == .yes_lebth,
                                   birth_quarter, NA_integer_),
      birth_quarter_orphan = as.integer(!is.na(birth_quarter) &
                                          (is.na(event_child) |
                                             event_child != .yes_lebth)),
      
      preg_q1 = as.integer(safe_numeric(., v$leprgq1) == .set_leprg),
      preg_q2 = as.integer(safe_numeric(., v$leprgq2) == .set_leprg),
      preg_q3 = as.integer(safe_numeric(., v$leprgq3) == .set_leprg),
      preg_q4 = as.integer(safe_numeric(., v$leprgq4) == .set_leprg),
      # v19: same reordering and same two-object split as the birth item.
      preg_q_n_set = rowSums(cbind(preg_q1, preg_q2, preg_q3, preg_q4),
                             na.rm = TRUE),
      preg_q_declined = as.integer(safe_numeric(., v$leprgna) == .set_leprg &
                                     event_pregnancy == .yes_leprg),
      preg_undated = as.integer(event_pregnancy == .yes_leprg &
                                  preg_q_n_set == 0L),
      preg_q_declined_dated = as.integer(preg_q_declined == 1L &
                                           preg_q_n_set > 0L),
      preg_quarter = dplyr::case_when(
        is.na(.set_leprg)    ~ NA_integer_,
        preg_q_n_set != 1L   ~ NA_integer_,
        preg_q1 == 1L        ~ 1L,
        preg_q2 == 1L        ~ 2L,
        preg_q3 == 1L        ~ 3L,
        preg_q4 == 1L        ~ 4L,
        TRUE                 ~ NA_integer_
      ),
      preg_quarter_multi = as.integer(preg_q_n_set > 1L),
      # v20: same restriction as the birth item, and the same four
      # orphan person-waves appear here, which is consistent with the
      # same respondents leaving both event boxes blank while dating
      # both.
      preg_quarter_event = ifelse(event_pregnancy == .yes_leprg,
                                  preg_quarter, NA_integer_),
      preg_quarter_orphan = as.integer(!is.na(preg_quarter) &
                                         (is.na(event_pregnancy) |
                                            event_pregnancy != .yes_leprg)),
      
      # --- v17 T-series arrivals -------------------------------------
      # v18: the yes-code is NOT assumed. This loader already uses 1 for
      # the training family and 2 for the life-event family, so HILDA is
      # inconsistent across families and a guess here is a coin flip.
      # The raw code is carried and the binary is built after
      # combination, where the female share identifies it.
      nlf_reason_code      = safe_numeric(., v$nlreapm),
      enrolled_in_course   = safe_numeric(., v$edcqen),
      full_time_student    = safe_numeric(., v$edfts),
      # v21.29 (a). THE FOUR OLDER E12 BRANCHES GET THE AUDIT THE THREE
      # NEW ONES ALREADY HAVE. These went straight from safe_numeric()
      # into columns downstream reads as 0/1, while jttrhgs, jttrmps and
      # jttrhsc are carried raw and converted on the == 1 rule. That left
      # promotion_training (= jttrpfj, RAW) and maintenance_training
      # (= jttrmps, BINARY) on different scales by construction. It reads
      # as a proportion only because the release codes these 0/1; one
      # wave coded 1/2 would put 2s in the column and the codes audit
      # could not see it, because that audit only covers items with a
      # _raw twin and these had none.
      #
      # BEHAVIOUR CHANGE: training_self_funded was a raw code and is now
      # 0/1 on the same rule as every other mentioned item. It is the
      # COST item, not an aim, and must never enter an aim index.
      training_self_funded_raw = safe_numeric(., v$jttrcst),
      training_aim_general_raw = safe_numeric(., v$jttrdsg),
      training_aim_current_raw = safe_numeric(., v$jttrisc),
      training_aim_future_raw  = safe_numeric(., v$jttrpfj),
      # v21.48. DON'T KNOW AND REFUSED. Both are flags on the same E12
      # battery. A trainee who gave either has no observed aim, so the six
      # aim items below are NA for them, through aim_item(). Before this
      # build they scored zero on all six, which reads as "none of these
      # aims" when the truth is "did not say".
      #
      # Carried raw so the training-item coding audit sees them before the
      # == 1 rule is trusted. An inverted code would blank the aims for
      # almost every trainee, so the diagnostic after combination also
      # stops the build if "mentioned" is implausibly common.
      training_aim_dk_raw      = safe_numeric(., v$jttrdk),
      training_aim_refused_raw = safe_numeric(., v$jttrrf),
      training_aim_nonresponse = ifelse(
        is.na(training_aim_dk_raw) & is.na(training_aim_refused_raw),
        NA_real_,
        as.numeric(dplyr::coalesce(training_aim_dk_raw, 0) == 1 |
                     dplyr::coalesce(training_aim_refused_raw, 0) == 1)),
      # The cost item is a separate question and is NOT blanked.
      training_self_funded = ifelse(
        is.na(training_self_funded_raw), NA_real_,
        as.numeric(training_self_funded_raw == 1)),
      training_aim_general = aim_item(training_aim_general_raw,
                                      training_aim_nonresponse),
      training_aim_current = aim_item(training_aim_current_raw,
                                      training_aim_nonresponse),
      training_aim_future  = aim_item(training_aim_future_raw,
                                      training_aim_nonresponse),
      # v21.21. THE PHANTOM COLUMNS, MADE REAL.
      #
      # The final select carried any_of(c("promotion_training",
      # "maintenance_training")) and neither was constructed anywhere, so
      # any_of() dropped both silently and the select advertised a
      # variable that did not exist. That is edit site 3 done and site 2
      # skipped: the mirror image of the training_gate_regime failure the
      # arrival contract caught, and it survived because any_of() cannot
      # tell a deliberate absence from an omission.
      #
      # The battery is now complete. Raw first, on the discipline the PPL
      # episode taught: nothing enters as 0/1 until the coding audit has
      # seen it, and these go through the same audit as the other aim
      # items.
      training_aim_getstarted_raw = safe_numeric(., v$jttrhgs),
      training_aim_maintain_raw   = safe_numeric(., v$jttrmps),
      training_aim_safety_raw     = safe_numeric(., v$jttrhsc),
      # v21.48: through aim_item(), so don't know and refused are NA here
      # as well as on the first three.
      training_aim_getstarted = aim_item(training_aim_getstarted_raw,
                                         training_aim_nonresponse),
      training_aim_maintain   = aim_item(training_aim_maintain_raw,
                                         training_aim_nonresponse),
      training_aim_safety     = aim_item(training_aim_safety_raw,
                                         training_aim_nonresponse),
      #
      # THE TWO REQUESTED NAMES ARE COMPOSITIONS AND THE MAPPING IS A
      # CHOICE. It is written out rather than buried so it can be
      # disagreed with:
      #
      #   promotion_training   <- jttrpfj, "prepare for a job in the
      #        future". The nearest thing HILDA's E12 battery has to a
      #        promotion aim; there is no item that says "promotion".
      #        Identical to training_aim_future, kept under both names
      #        because the select promised this one.
      #   maintenance_training <- jttrmps, "maintain professional
      #        status", optionally widened with jttrhsc, health and
      #        safety, since both are compliance rather than advancement.
      #        The NARROW version is used: adding safety would fold a
      #        legal requirement into a professional one and the two
      #        respond to different things.
      #
      # If either mapping is wrong for the hypothesis being tested, change
      # it HERE, where it is one line and visible, rather than in a
      # downstream recode where it is not.
      promotion_training   = training_aim_future,
      maintenance_training = training_aim_maintain,
      #
      # ---- v21.29 (b) THE AIM-BATTERY GATE ---------------------------
      # E12 is put only to people who reported training, so every aim
      # column above is NA for everyone who trained none: 27.3% coverage,
      # almost exactly where the dosage margin sat before v21.9 recoded
      # it, and for the same reason.
      aim_battery_asked = as.integer(
        !is.na(training_aim_future_raw)    | !is.na(training_aim_current_raw) |
          !is.na(training_aim_general_raw)   | !is.na(training_aim_getstarted_raw) |
          !is.na(training_aim_maintain_raw)  | !is.na(training_aim_safety_raw)),
      #
      # ---- v21.29 (c) THE WIDE VARIANTS ------------------------------
      # The narrow mapping above is a choice, and a choice that exists in
      # only one version cannot be tested. Both are built.
      #   promotion wide   adds jttrhgs (help getting started), which is
      #        forward-looking but about entering a job, not advancing.
      #   maintenance wide adds jttrhsc (health and safety), folding a
      #        legal requirement in with a professional one.
      # jttrisc (improve skills, current job) and jttrdsg (develop skills
      # generally) stay OUT of both: jttrisc is neither advancement nor
      # compliance, and putting it in maintenance would quietly redefine
      # that category as current-job-oriented, a different claim.
      promotion_training_wide = ifelse(
        is.na(training_aim_future) & is.na(training_aim_getstarted), NA_real_,
        as.numeric(coalesce(training_aim_future, 0) == 1 |
                     coalesce(training_aim_getstarted, 0) == 1)),
      maintenance_training_wide = ifelse(
        is.na(training_aim_maintain) & is.na(training_aim_safety), NA_real_,
        as.numeric(coalesce(training_aim_maintain, 0) == 1 |
                     coalesce(training_aim_safety, 0) == 1)),
      #
      # ---- v21.29 (d) THE UNCONDITIONAL TWINS ------------------------
      # THIS IS THE PAIR AN EVENT STUDY SHOULD USE. The columns above are
      # observed only where the person trained, so a regression on them
      # CONDITIONS ON THE OUTCOME: training incidence falls about twelve
      # points for mothers at the birth wave, so the k = 0 cell is a
      # differently composed group from k = -2 before any aim is examined.
      #
      # Same four states, same rule, as total_training_hours_z:
      #   never asked the module            -> NA, not known
      #   asked, measured no training       -> 0, a true zero
      #   trained, aim battery not answered -> NA, item non-response
      #   trained, aim battery answered     -> the indicator
      promotion_training_z = ifelse(
        training_module_asked == 0L, NA_real_,
        ifelse(!is.na(any_training) & any_training == 0, 0,
               ifelse(aim_battery_asked == 0L, NA_real_, promotion_training))),
      maintenance_training_z = ifelse(
        training_module_asked == 0L, NA_real_,
        ifelse(!is.na(any_training) & any_training == 0, 0,
               ifelse(aim_battery_asked == 0L, NA_real_, maintenance_training))),
      promotion_training_wide_z = ifelse(
        training_module_asked == 0L, NA_real_,
        ifelse(!is.na(any_training) & any_training == 0, 0,
               ifelse(aim_battery_asked == 0L, NA_real_, promotion_training_wide))),
      maintenance_training_wide_z = ifelse(
        training_module_asked == 0L, NA_real_,
        ifelse(!is.na(any_training) & any_training == 0, 0,
               ifelse(aim_battery_asked == 0L, NA_real_, maintenance_training_wide))),
      training_aim_current_z = ifelse(
        training_module_asked == 0L, NA_real_,
        ifelse(!is.na(any_training) & any_training == 0, 0,
               ifelse(aim_battery_asked == 0L, NA_real_, training_aim_current))),

      # v21.43. THE COST ITEM GETS THE GATED TWIN THE AIMS ALREADY HAVE.
      #
      # jttrcst is "contributed to the cost of job-related training", asked
      # only of people who trained. It has been read since v18 and derived
      # as training_self_funded on the == 1 rule, with a _raw twin in the
      # codes audit. What it never had is a _z twin, so it was the one
      # member of the E12 family still exposed to the conditioning problem
      # the rest of that family was fixed for.
      #
      # WHY IT MATTERS MORE FOR THIS ITEM THAN FOR THE AIMS. The paper's
      # central claim is a FINANCING split, and the split it currently
      # rests on is jttpewt against jttpeot, which separate training by
      # PLACE AND TIME - at the employer's premises during paid work hours
      # against the respondent's own time. Neither records who met the
      # fees. jttrcst does, from the worker's side, and it is the only
      # item in the module that speaks to fees at all. An ungated version
      # is observed only among trainees, and trainee status moves at the
      # birth, so a level comparison on it conditions on the outcome.
      #
      # THE THREE-STATE RULE IS THE AIMS' RULE AND FOR THE SAME REASONS.
      # NA where the module was never asked, because that zero is an
      # imputation. ZERO where the module was asked and no training was
      # reported, because a person who did not train did not contribute to
      # a cost and that is a measured zero rather than a missing one. The
      # indicator where the person trained and answered.
      #
      # The ungated column stays. It answers a composition question -
      # among training that happened, what share did the worker help pay
      # for - and correctly conditions on having trained. The _z column
      # answers a level question. They are not a robustness pair and
      # should not be reported as one.
      training_self_funded_z = ifelse(
        training_module_asked == 0L, NA_real_,
        ifelse(!is.na(any_training) & any_training == 0, 0,
               ifelse(aim_battery_asked == 0L, NA_real_, training_self_funded))),
      training_aim_general_z = ifelse(
        training_module_asked == 0L, NA_real_,
        ifelse(!is.na(any_training) & any_training == 0, 0,
               ifelse(aim_battery_asked == 0L, NA_real_, training_aim_general))),
      occupation_2digit    = safe_numeric(., v$jbmo62),
      occupation_status_score = safe_numeric(., v$jbmo6s),
      
      # ---- v21.12 (c) THE ORIGIN OCCUPATION OF A MOVE ---------------
      # Three sources for one concept. pjoto* is CPQ and asks the
      # unemployed; ujljo* is NPQ and asks anyone not in paid work; pjoo*
      # is CPQ and asks job-changers. The first two are complementary
      # rather than redundant, so the coalesce below takes them in that
      # order and the job-change version last.
      #
      # A COALESCE HERE IS A CHOICE AND IT IS RECORDED. The per-source
      # columns are kept beside the combined one so a design can use one
      # questionnaire alone, and occ_origin_source says which answered.
      # Without that column the combined variable would silently mix two
      # routings, which is the defect the enrolment item taught.
      occ2_origin_unemp     = safe_numeric(., v$pjoto62),
      occ2_origin_nonemp    = safe_numeric(., v$ujljo62),
      occ2_origin_jobchange = safe_numeric(., v$pjoo62),
      occstat_origin_unemp     = safe_numeric(., v$pjoto6s),
      occstat_origin_nonemp    = safe_numeric(., v$ujljo6s),
      occstat_origin_jobchange = safe_numeric(., v$pjoo6s),
      occ2_origin = coalesce(occ2_origin_unemp, occ2_origin_nonemp,
                             occ2_origin_jobchange),
      occstat_origin = coalesce(occstat_origin_unemp, occstat_origin_nonemp,
                                occstat_origin_jobchange),
      occ_origin_source = case_when(
        !is.na(occ2_origin_unemp)     ~ "cpq_unemployed",
        !is.na(occ2_origin_nonemp)    ~ "npq_not_in_work",
        !is.na(occ2_origin_jobchange) ~ "cpq_job_change",
        TRUE                          ~ NA_character_),
      # The move itself, only where both ends are observed. NA elsewhere
      # rather than zero: no observed origin is not "no move".
      #
      # v21.14c. WHY THIS IS A JOB-CHANGE-ONLY VARIABLE, which took three
      # runs to see and is structural rather than a coverage accident.
      #
      # It needs an origin AND a destination on the SAME ROW. Of the three
      # origin routes, two are asked of people who are NOT currently
      # employed: pjoto* is "previous job if unemployed" and ujljo* is
      # "last job if not in paid work". For exactly those people
      # occupation_status_score, which is the CURRENT job, is missing. So
      # they supply an origin with no destination and drop out here.
      #
      # Only pjoo*, "previous job where the job changed", is asked of
      # someone who IS employed and therefore has both ends. pjoo62 and
      # pjoo6s are absent from waves 20 to 24, so occstat_change inherits
      # the 12-19 window even though occstat_origin spans all thirteen.
      # That is the whole explanation for its appearance in the wave-hole
      # sweep, and nothing is wrong with it.
      #
      # THE CONSEQUENCE FOR THE PROGRESSION CLAIM. A within-wave
      # origin-to-destination move is observable ONLY for job changers,
      # in eight of thirteen waves, on about 6% of person-waves. The
      # unemployed and not-in-work routes say where a person came from
      # and cannot say where they went, because they had not yet gone.
      # Pairing those origins with a LATER wave's occupation is a real
      # design and a different variable; it is not built here, because
      # choosing the lag is an analysis decision.
      occstat_change = ifelse(
        is.na(occstat_origin) | is.na(occupation_status_score), NA_real_,
        occupation_status_score - occstat_origin),
      
      # ---- v21.12 (a) IS PPL INSIDE THE EARNINGS MEASURE? -----------
      # Read as _raw because the coding is unverified against this
      # release. The audit after combination reports the code sets; until
      # it has run, do not read these as 0/1.
      ppl_in_wages_raw     = safe_numeric(., v$wscplre),
      ppl_in_fy_income_raw = safe_numeric(., v$wsfplre),
      # v21.15: the post-imputed pair and their flags. NOT coalesced onto
      # the pre-imputed ones: a design that wants self-reports only should
      # be able to have them, and one that wants coverage should be able
      # to see how much of it is imputed. Both are on the panel and the
      # choice is the caller's.
      ppl_in_wages_imp     = safe_numeric(., v$wscplrei),
      ppl_in_wages_impflag = safe_numeric(., v$wscplref),
      ppl_in_fy_imp        = safe_numeric(., v$wsfplrei),
      ppl_in_fy_impflag    = safe_numeric(., v$wsfplref),
      
      # ---- v21.12 (b) LEAVE AGAINST SEPARATION ----------------------
      # Entitlement and receipt arrive as _raw for the same reason. The
      # amounts and week counts are numeric quantities and are safe to
      # take directly.
      ppl_entitled_mat_raw = safe_numeric(., v$jowppml),
      ppl_entitled_pat_raw = safe_numeric(., v$jowpppl),
      ppl_received_cur_raw = safe_numeric(., v$bncppl),
      ppl_received_fy_raw  = safe_numeric(., v$bnfppl),
      ppl_amount_cur       = safe_numeric(., v$bncppla),
      ppl_weeks_cur        = safe_numeric(., v$bncpplw),
      ppl_amount_fy        = safe_numeric(., v$bnfppla),
      # v21.14: the date, and the three things a design actually uses.
      # ref_window_start is the anchor for every "past 12 months" item,
      # including the training gate; an event is inside that item's
      # reference period iff it falls between it and the interview.
      interview_date  = safe_date(., v$hhhqivw),
      interview_year  = .date_part(safe_date(., v$hhhqivw), "%Y"),
      interview_month = .date_part(safe_date(., v$hhhqivw), "%m"),
      interview_doy   = .date_part(safe_date(., v$hhhqivw), "%j"),
      ref_window_start = safe_date(., v$hhhqivw) - 365,
      ppl_howanswered_fy   = safe_numeric(., v$bnfpplh),
      # v21.14c: bncpplh does NOT resolve in this release, so this column
      # is all-NA and the current-payment receipt item bncppl has no
      # how-answered companion. The financial-year pair does. So the 13.3%
      # receipt puzzle is diagnosable on bnfppl and NOT on bncppl, and
      # ppl_received_cur should not be used until the FY diagnosis
      # settles what the family is measuring.
      ppl_howanswered_cur  = safe_numeric(., v$bncpplh),
      ppl_weeks_fy         = safe_numeric(., v$bnfpplw),
      # v21.24: renamed to say what it aggregates. The old name,
      # paid_family_leave_days, invited exactly the reading the dictionary
      # rules out.
      paid_nonannual_leave_days = safe_numeric(., v$alop),
      
      # The 0/1 versions, on the == 1 rule, exactly as the training items
      # are built. That rule is an ASSUMPTION until the coding audit after
      # combination confirms it, and the audit STOPS the build if any of
      # these carries a code set the rule would misread. The training
      # block earned that discipline: under a > 0 rule, code 2 meaning
      # "no" would have counted as yes and scored nearly everyone.
      #
      # NA-PROPAGATING, not coalesced. These are not gated items with a
      # structural zero: a missing answer here is missing, and reading it
      # as "no leave" would repeat the jttpewt_any mistake in a new place.
      ppl_in_wages     = ifelse(is.na(ppl_in_wages_raw), NA_real_,
                                as.numeric(ppl_in_wages_raw == 1)),
      ppl_in_fy_income = ifelse(is.na(ppl_in_fy_income_raw), NA_real_,
                                as.numeric(ppl_in_fy_income_raw == 1)),
      ppl_entitled_mat = ifelse(is.na(ppl_entitled_mat_raw), NA_real_,
                                as.numeric(ppl_entitled_mat_raw == 1)),
      ppl_entitled_pat = ifelse(is.na(ppl_entitled_pat_raw), NA_real_,
                                as.numeric(ppl_entitled_pat_raw == 1)),
      ppl_received_cur = ifelse(is.na(ppl_received_cur_raw), NA_real_,
                                as.numeric(ppl_received_cur_raw == 1)),
      ppl_received_fy  = ifelse(is.na(ppl_received_fy_raw), NA_real_,
                                as.numeric(ppl_received_fy_raw == 1)),
      
      got_married_this_wave   = ifelse(event_married == 2, 1L, 0L),
      separated_this_wave     = ifelse(event_separated == 2, 1L, 0L),
      changed_job_this_wave   = ifelse(event_job_change == 2, 1L, 0L),
      
      # -----------------------------------------------------------
      # 6I. OTHER PARENT / PARTNER INFO (NEW)
      # -----------------------------------------------------------
      other_parent_employment = safe_numeric(., v$ncesop),
      living_with_partner_last = safe_numeric(., v$prlpli),
      # Household and partner linkage
      household_id = if (v$hhrhid %in% names(.)) as.character(.[[v$hhrhid]]) else NA_character_,
      partner_id   = if (v$hhpxid %in% names(.)) as.character(.[[v$hhpxid]]) else NA_character_,
      
      # -----------------------------------------------------------
      # 6J. JOB SATISFACTION & WORKPLACE QUALITY (NEW)
      # -----------------------------------------------------------
      job_satisfaction    = safe_numeric(., v$jbmsall),   # C33f overall job satisfaction
      pay_fairness        = safe_numeric(., v$jompf),     # "Paid fairly"
      job_initiative      = safe_numeric(., v$jomini),    # "Job requires initiative"
      job_security        = safe_numeric(., v$jomsf),     # "Secure future in my job"
      job_worry_future    = safe_numeric(., v$jomwf),     # "Worry about future of my job"
      job_autonomy        = safe_numeric(., v$jomls_say), # "Have a lot of say"
      looking_for_new_job = safe_numeric(., v$jbnewjs),   # Looked for new job last 4 weeks
      chance_leave_job = safe_numeric(., v$jbmplej),
      chance_lose_job  = safe_numeric(., v$jbmploj),
      chance_find_job  = safe_numeric(., v$jbmpgj),
      chance_leave_job = ifelse(chance_leave_job > 100, NA_real_, chance_leave_job),
      chance_lose_job  = ifelse(chance_lose_job  > 100, NA_real_, chance_lose_job),
      chance_find_job  = ifelse(chance_find_job  > 100, NA_real_, chance_find_job),
      
      # -----------------------------------------------------------
      # 6J-bis. LIFE SATISFACTION DOMAINS (NEW — SCQ, 0-10 scale)
      # -----------------------------------------------------------
      losat   = safe_numeric(., v$losat),    # Overall life satisfaction
      losateo = safe_numeric(., v$losateo),  # Employment opportunities
      losatfs = safe_numeric(., v$losatfs),  # Financial situation
      losatft = safe_numeric(., v$losatft),  # Amount of free time
      losathl = safe_numeric(., v$losathl),  # The home you live in
      losatlc = safe_numeric(., v$losatlc),  # Feeling part of local community
      losatnl = safe_numeric(., v$losatnl),  # The neighbourhood
      losatsf = safe_numeric(., v$losatsf),  # How safe you feel
      losatyh = safe_numeric(., v$losatyh),  # Your health
      
      # -----------------------------------------------------------
      # 6K. SOCIAL CAPITAL / NETWORKING (NEW — H4 mechanism)
      # -----------------------------------------------------------
      # lsclub: "Are you an active member of a sporting, hobby or community-based
      #          club or association?" 1=Yes, 2=No → recode to 0/1
      club_member_raw = safe_numeric(., v$lsclub),
      club_member = case_when(
        club_member_raw == 1 ~ 1L,
        club_member_raw == 2 ~ 0L,
        TRUE ~ NA_integer_
      ),
      # lssocal: "How often do you get together socially with friends/relatives
      #           not living with you?" 1=Every day, 2=Several times/week, ..., 7=Never
      #           REVERSE so higher = more social (1=never → 7=every day)
      social_freq_raw = safe_numeric(., v$lssocal),
      social_freq = case_when(
        social_freq_raw >= 1 & social_freq_raw <= 7 ~ 8L - as.integer(social_freq_raw),
        TRUE ~ NA_integer_
      ),
      social_freq_01 = ifelse(!is.na(social_freq), (social_freq - 1) / 6, NA_real_),
      high_social = ifelse(!is.na(social_freq), as.integer(social_freq >= 5), NA_integer_),
      
      # -----------------------------------------------------------
      # 6L. CARE AND FLEXIBILITY (original)
      # -----------------------------------------------------------
      
      # Care intensity (housework hours)
      housework_hours = safe_numeric_zero(., v$lshrhw),
      care_intensity = case_when(
        is.na(housework_hours) ~ 0,
        housework_hours < 0 ~ 0,
        housework_hours > 100 ~ 100,
        TRUE ~ housework_hours
      ),
      
      # -----------------------------------------------------------
      # 6L-b. v18: TIME USE BEYOND HOUSEWORK, AND WEALTH
      # -----------------------------------------------------------
      # Individually measured, unlike household expenditure. Carried
      # under their own mnemonics: nothing at wave level can tell
      # lshrcar from lshrchd, so nothing here pretends to. The
      # labelling, the total, and the net-worth difference are all
      # built after combination where they can be checked.
      hours_lshrcar   = pick_first_numeric(., v, "lshrcar",
                                           label = "lshrcar"),
      hours_lshrchd   = pick_first_numeric(., v, "lshrchd",
                                           label = "lshrchd"),
      commute_hours   = pick_first_numeric(., v, COMMUTE_STEMS,
                                           label = "commute"),
      volunteer_hours = pick_first_numeric(., v, VOLUNTEER_STEMS,
                                           label = "volunteer"),
      
      # Wealth module: waves 2, 6, 10, 14, 18, 22. Inside 12-24 that is
      # waves 14, 18 and 22, so three observations per person. Net
      # worth is carried as its two components; the difference is taken
      # after combination, where the split can be verified.
      super_balance    = pick_first_numeric(., v, SUPER_STEMS,
                                            label = "super"),
      hh_net_worth_pos = pick_first_numeric(., v, NETWORTH_POS_STEMS,
                                            label = "networth_pos"),
      hh_net_worth_neg = pick_first_numeric(., v, NETWORTH_NEG_STEMS,
                                            label = "networth_neg"),
      in_wealth_module = as.integer(any(!is.na(super_balance)) |
                                      any(!is.na(hh_net_worth_pos))),
      
      # Flexibility (treatment)
      jomflex_raw = safe_numeric(., v$jomflex),
      jowpfx_raw = safe_numeric(., v$jowpfx),
      has_flex_perception = ifelse(jomflex_raw >= 5, 1, 0),
      has_flex_entitlement = ifelse(jowpfx_raw == 1, 1, 0),
      flexibility_any = ifelse(has_flex_perception == 1 | has_flex_entitlement == 1, 1, 0),
      treatment = flexibility_any,
      treatment = ifelse(is.na(treatment), 0, treatment)
    )
  
  # Fertility diagnostics
  cat("    - Fertility history variables extracted\n")
  cat("    - tchad coverage:", 
      round(100 * sum(!is.na(wave_data$total_children_ever_had)) / nrow(wave_data), 1), "%\n")
  cat("    - tcr+tcnr coverage:", 
      round(100 * sum(!is.na(wave_data$num_own_resident_children)) / nrow(wave_data), 1), "%\n")
  cat("    - ever_parent == 1:", sum(wave_data$ever_parent == 1, na.rm = TRUE), 
      "(", round(100 * mean(wave_data$ever_parent == 1, na.rm = TRUE), 1), "%)\n")
  cat("    - ever_parent == 0:", sum(wave_data$ever_parent == 0, na.rm = TRUE), "\n")
  cat("    - ever_parent NA:", sum(is.na(wave_data$ever_parent)), "\n")
  
  # ============================
  # STEP 7: PRECARIOUSNESS AND FINAL VARIABLES
  # ============================
  
  cat("  Step 7: Final variables...\n")
  
  wave_data <- wave_data %>%
    mutate(
      # Precariousness score
      # v21.31. A SCORE THAT COULD NEVER BE NA, AND SCORED THE
      # NON-EMPLOYED AS THE MOST SECURE WORKERS IN THE PANEL.
      #
      # rowSums(..., na.rm = TRUE) returns 0 when EVERY component is
      # missing, and precarious_category then labelled that "Secure". All
      # five components are defined only for the employed, so the
      # non-employed scored 0 and were recorded as maximally secure. The
      # column reported 0% missing in every group, which reads as complete
      # coverage and is the opposite: it was never NA by construction.
      #
      # v21.28 made this sharper rather than causing it. Before that fix,
      # casual and permanent were 0 off the employed base, so 1 - permanent
      # contributed 1 and the non-employed scored at least 1; the artefact
      # showed as a floor. Once those two became correctly NA, they
      # contributed nothing and the score fell to 0.
      #
      # The score is now NA unless at least PRECARIOUS_MIN_COMPONENTS of
      # the five are observed, and it is rescaled to the five-item metric
      # so a person answering three is not mechanically less precarious
      # than one answering five. Anything below the threshold is NA, which
      # loses rows visibly instead of inventing security.
      # NO MATRIX COLUMN. cbind() inside mutate() creates a MATRIX-column,
      # which is a legal data-frame column and breaks any diagnostic that
      # walks the columns expecting vectors: as.integer() flattens it to
      # n * 5 elements against a grouping vector of length n, which is the
      # "incorrect length for group" failure in the coverage audit. Summed
      # term by term instead.
      #
      # NOTE ON THE WEIGHTING, WHICH IS INHERITED AND NOT CHANGED HERE.
      # casual and permanent both come from jbcasab, so casual and
      # 1 - permanent are the SAME indicator: a casual worker contributes 2
      # of the 5 points from one item. That doubling predates this fix and
      # is left alone so scores stay comparable with earlier builds, but it
      # means the score weights contract type twice as heavily as leave,
      # hours or underemployment. Say so if the score is ever interpreted
      # rather than used as a control.
      .prec_n = (!is.na(casual)) + (!is.na(permanent)) +
        (!is.na(leave_none)) + (!is.na(has_irregular_hours)) +
        (!is.na(underemployed)),
      precarious_score = ifelse(
        .prec_n < PRECARIOUS_MIN_COMPONENTS, NA_real_,
        (coalesce(casual, 0) + coalesce(1 - permanent, 0) +
           coalesce(leave_none, 0) + coalesce(has_irregular_hours, 0) +
           coalesce(underemployed, 0)) * 5 / pmax(.prec_n, 1)),
      precarious_n_components = .prec_n,
      
      # v21.31. NA propagates, and the cutpoints are HALF-OPEN INTERVALS
      # rather than equality tests. Rescaling to the five-item metric makes
      # the score non-integer whenever fewer than five components are
      # observed: three observed and two positive gives 2 * 5/3 = 3.33,
      # which matched none of the old == tests and fell through to NA. A
      # score that is defined must always have a category.
      precarious_category = case_when(
        is.na(precarious_score)  ~ NA_character_,
        precarious_score <  0.5  ~ "Secure",
        precarious_score <  1.5  ~ "Mostly_Secure",
        precarious_score <  2.5  ~ "Somewhat_Precarious",
        TRUE                     ~ "Highly_Precarious"
      ),
      
      # Parental education
      mother_educ_years = case_when(
        safe_numeric(., v$fmmsch) == 1 ~ 0,
        safe_numeric(., v$fmmsch) == 2 ~ 6,
        safe_numeric(., v$fmmsch) == 3 ~ 10,
        safe_numeric(., v$fmmsch) == 4 ~ 11,
        safe_numeric(., v$fmmsch) == 5 ~ 12,
        TRUE ~ NA_real_
      ),
      
      father_educ_years = case_when(
        safe_numeric(., v$fmfsch) == 1 ~ 0,
        safe_numeric(., v$fmfsch) == 2 ~ 6,
        safe_numeric(., v$fmfsch) == 3 ~ 10,
        safe_numeric(., v$fmfsch) == 4 ~ 11,
        safe_numeric(., v$fmfsch) == 5 ~ 12,
        TRUE ~ NA_real_
      ),
      
      # State
      state_code = safe_numeric(., v$hhstate),
      state = case_when(
        state_code == 1 ~ "NSW",
        state_code == 2 ~ "VIC",
        state_code == 3 ~ "QLD",
        state_code == 4 ~ "SA",
        state_code == 5 ~ "WA",
        state_code == 6 ~ "TAS",
        state_code == 7 ~ "NT",
        state_code == 8 ~ "ACT",
        TRUE ~ NA_character_
      ),
      
      # Weights
      analysis_weight = safe_numeric(., v$hhwtrps),
      population_weight = safe_numeric(., v$hhwtrp),
      longitudinal_weight = safe_numeric(., v$lnwtrp)  # For FE/panel analysis
    )
  
  # Networking index (composite of club_member + social_freq)
  # scale() requires full column, so computed after mutate
  wave_data <- wave_data %>%
    mutate(
      networking_index = {
        cm_z <- if (sd(club_member, na.rm = TRUE) > 0) as.numeric(scale(club_member)) else 0
        sf_z <- if (sd(social_freq, na.rm = TRUE) > 0) as.numeric(scale(social_freq)) else 0
        ifelse(!is.na(club_member) & !is.na(social_freq), cm_z + sf_z, NA_real_)
      }
    )
  
  cat("    - Networking: club_member non-NA =",
      sum(!is.na(wave_data$club_member)), 
      ", social_freq non-NA =", 
      sum(!is.na(wave_data$social_freq)), "\n")
  
  # Clean up raw data
  rm(raw)
  gc()
  
  # ============================
  # APPLY FILTERS — TWO-TIER APPROACH
  # ============================
  # 
  # Tier 1 (broad_sample): Everyone in age range with valid demographics.
  #   Includes unemployed, out-of-labor-force, missing wages.
  #   Used for: Heckman selection models, participation analysis,
  #   motherhood transition tracking, attrition diagnostics.
  #
  # Tier 2 (in_wage_sample): Employed with valid wages.
  #   Used for: Mincer equations, FE wage regressions.
  #   Flagged as in_wage_sample == 1 within the broad sample.
  
  cat("  Applying filters...\n")
  
  # Calculate wage quantiles (from employed only)
  wage_q005 <- quantile(wave_data$hourly_wage_clean, 0.005, na.rm = TRUE)
  wage_q995 <- quantile(wave_data$hourly_wage_clean, 0.995, na.rm = TRUE)
  
  # ---- TIER 1: Broad sample (includes non-employed) ----
  # v2: window widened from 21-52 to AGE_MIN-AGE_MAX (20-55). The old
  # window mechanically truncated the ten-year post-birth horizon for
  # anyone giving birth after 39 and removed pre-birth trajectories for
  # women giving birth in their late twenties, thinning exactly the event
  # times the design needs. Widening here costs nothing: the analysis
  # stage can still impose 30-49 via the flags written below, but it can
  # no longer do so silently.
  wave_broad <- wave_data %>%
    filter(
      # Demographics available
      !is.na(age), 
      age >= AGE_MIN, 
      age <= AGE_MAX,
      !is.na(female),
      !is.na(educ_years),
      !is.na(state)
    ) %>%
    mutate(
      # Legacy-window flags so downstream code can reproduce the old
      # samples exactly, and so any age restriction is explicit.
      in_age_30_49 = as.numeric(age >= 30 & age <= 49),
      in_age_25_52 = as.numeric(age >= 25 & age <= 52),
      in_age_21_52 = as.numeric(age >= 21 & age <= 52)
    )
  
  cat("  Broad sample (Tier 1):", format(nrow(wave_broad), big.mark = ","), "observations\n")
  
  # ---- TIER 2: Wage sample flag within broad ----
  wave_broad <- wave_broad %>%
    mutate(
      in_wage_sample = case_when(
        # Must be employed
        !(esdtl_raw %in% c(1, 2)) ~ 0L,
        # Must have valid wages
        is.na(ln_hourly_wage_real) ~ 0L,
        hourly_wage_clean < wage_q005 | hourly_wage_clean > wage_q995 ~ 0L,
        # Must have valid hours
        is.na(hours_worked_clean) | hours_worked_clean <= 0 | hours_worked_clean > 100 ~ 0L,
        # Must have valid experience
        is.na(experience_years) | experience_years < 0 ~ 0L,
        # Must have weights
        is.na(analysis_weight) | analysis_weight <= 0 ~ 0L,
        # All conditions met
        TRUE ~ 1L
      )
    )
  
  # ---- v2 TIER 2b: EARNINGS sample flag ----
  # The wage sample conditions on employment; the earnings sample does
  # not. This is the whole point: an earnings outcome is defined at zero
  # for the non-employed, so post-birth withdrawal from work stays in the
  # sample as a zero rather than dropping out as a missing wage. Use
  # in_earnings_sample for the child-penalty event study and
  # in_wage_sample only for the hourly-price decomposition.
  wave_broad <- wave_broad %>%
    mutate(
      in_earnings_sample = case_when(
        is.na(real_annual_income) & is.na(real_weekly_earnings) ~ 0L,
        is.na(experience_years) | experience_years < 0 ~ 0L,
        is.na(analysis_weight) | analysis_weight <= 0 ~ 0L,
        TRUE ~ 1L
      )
    )
  
  n_wage <- sum(wave_broad$in_wage_sample == 1)
  n_nonwage <- sum(wave_broad$in_wage_sample == 0)
  cat("  Earnings sample (Tier 2b):",
      format(sum(wave_broad$in_earnings_sample == 1), big.mark = ","),
      "observations\n")
  cat("  Zero-earnings share (earnings sample):",
      round(100 * mean(wave_broad$earnings_is_zero[wave_broad$in_earnings_sample == 1],
                       na.rm = TRUE), 1), "%\n")
  cat("  Experience from Mincer proxy:",
      round(100 * mean(wave_broad$experience_is_proxy, na.rm = TRUE), 1), "%\n")
  cat("  Wage sample (Tier 2):", format(n_wage, big.mark = ","), "observations\n")
  cat("  Non-wage (available for selection):", format(n_nonwage, big.mark = ","), "observations\n")
  cat("  Employment rate in broad sample:", 
      round(100 * mean(wave_broad$esdtl_raw %in% c(1,2), na.rm = TRUE), 1), "%\n")
  cat("  Retention (wage/total):", round(100 * n_wage / nrow(wave_data), 1), "%\n")
  
  # ============================
  # EDUCATION DIAGNOSTICS (WAGE SAMPLE)
  # ============================
  
  cat("\n  Education Diagnostics (wage sample):\n")
  
  wave_wage <- wave_broad %>% filter(in_wage_sample == 1)
  
  # Education level distribution
  educ_dist <- wave_wage %>%
    count(education_level) %>%
    mutate(pct = round(100 * n / sum(n), 1))
  
  cat("    Education level distribution:\n")
  print(educ_dist, n = 20)
  
  # Education pathway distribution (top 20)
  pathway_dist <- wave_wage %>%
    count(education_pathway, sort = TRUE) %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    head(20)
  
  cat("\n    Top 20 education pathways:\n")
  print(pathway_dist)
  
  # Field-specific education.
  # v19: the observed denominator is printed alongside the count. A zero
  # over an observed denominator of zero is an absent mnemonic; a zero
  # over a large denominator is a real zero. The v18 print collapsed the
  # two into "0 ( 0 %)".
  cat("\n    Field-specific education (set / observed):\n")
  .fld <- c(Nursing = "has_nursing_any", Teaching = "has_teaching_any",
            Business = "has_business_any", Trade = "has_trade_any",
            Technical = "has_technical_any")
  for (.f in names(.fld)) {
    .x <- wave_wage[[.fld[[.f]]]]
    .obs <- sum(!is.na(.x)); .set <- sum(.x == 1, na.rm = TRUE)
    cat(sprintf("      %-10s %5d / %6d  %s\n", .f, .set, .obs,
                if (.obs == 0L) "NOT OBSERVED: mnemonic absent this wave"
                else sprintf("%.2f%%", 100 * .set / .obs)))
  }
  
  # NEW: Fertility history diagnostics (wage sample)
  cat("\n  Fertility History Diagnostics (wage sample):\n")
  cat("    ever_parent == 1:", sum(wave_wage$ever_parent == 1, na.rm = TRUE), "\n")
  cat("    ever_parent == 0:", sum(wave_wage$ever_parent == 0, na.rm = TRUE), "\n")
  cat("    ever_parent NA:", sum(is.na(wave_wage$ever_parent)), "\n")
  if(sum(wave_wage$female == 1) > 0) {
    cat("    Women - motherhood_status:\n")
    print(table(wave_wage$motherhood_status, useNA = "ifany"))
  }
  cat("    Pregnancy reports this wave (leprg):",
      sum(wave_wage$had_pregnancy_this_wave == 1, na.rm = TRUE), "\n")
  cat("    Birth/adoption reports this wave (lebth):",
      sum(wave_wage$had_child_this_wave == 1, na.rm = TRUE), "\n")
  cat("    Births dated to a single quarter:",
      sum(!is.na(wave_wage$birth_quarter)), "\n")
  cat("    Births reported, no quarter at all (undated):",
      sum(wave_wage$birth_undated == 1, na.rm = TRUE), "\n")
  cat("    Births reported, multiple quarters flagged:",
      sum(wave_wage$birth_quarter_multi == 1, na.rm = TRUE), "\n")
  cat("    Births reported, NA box ticked (may also carry a quarter):",
      sum(wave_wage$birth_q_declined == 1, na.rm = TRUE), "\n")
  
  # NEW: Selection diagnostics (broad vs wage sample)
  cat("\n  Selection Diagnostics (broad sample):\n")
  selection_by_group <- wave_broad %>%
    filter(female == 1, !is.na(ever_parent)) %>%
    group_by(motherhood_status) %>%
    summarise(
      N_broad = n(),
      N_wage = sum(in_wage_sample == 1),
      Employment_rate = round(100 * mean(esdtl_raw %in% c(1,2), na.rm = TRUE), 1),
      Wage_sample_rate = round(100 * mean(in_wage_sample == 1), 1),
      .groups = "drop"
    )
  cat("    Women by motherhood status (broad vs wage):\n")
  print(selection_by_group)
  
  # Clean up
  rm(wave_data, wave_wage)
  gc()
  
  # ---- DROP intermediate columns to save memory ----
  # Drop the _raw scratch columns, EXCEPT those the final select and the
  # verification block still need.
  #
  # v9 FIX: this grep silently removed emptype_raw and bifhave_raw,
  # because both end in _raw by naming convention while being OUTPUT
  # columns rather than scratch. The failure surfaced 400 lines later as
  # "Column `emptype_raw` doesn't exist" in the final select, with no
  # indication of where it had gone. Any future _raw column that is
  # meant to survive must be added to RAW_COLS_KEEP.
  RAW_COLS_KEEP <- c("esdtl_raw",      # selection diagnostics
                     "emptype_raw",    # employment status, v9 output
                     "bifhave_raw",    # own-business cross-check, v9 output
                     # v21.6: the five training codes, kept so the coding
                     # audit after combination reads the release rather
                     # than the derived 0/1 columns. Dropped again at the
                     # analysis_sample select, so the slim panel does not
                     # carry them.
                     # v21.12: the six new binary items, kept for the
                     # same reason and dropped at the same place. Their
                     # coding is UNVERIFIED against this release and the
                     # audit after combination is what verifies it.
                     # v21.21: the three new aim branches, kept so the
                     # coding audit can see them before anything reads
                     # the 0/1 versions.
                     "training_aim_getstarted_raw",
                     "training_aim_maintain_raw",
                     "training_aim_safety_raw",
                     # v21.29: the four older branches now have _raw twins
                     # too. Without them here the drop below removes them
                     # and the coding audit intersects to nothing, SILENTLY:
                     # it would report a pass it never ran.
                     "training_aim_future_raw", "training_aim_current_raw",
                     "training_aim_general_raw", "training_self_funded_raw",
                     # v21.48: the battery's don't-know and refused flags.
                     # Left out, the drop below removes them and the
                     # training-item audit has nothing to read.
                     "training_aim_dk_raw", "training_aim_refused_raw",
                     "ppl_in_wages_raw", "ppl_in_fy_income_raw",
                     "ppl_entitled_mat_raw", "ppl_entitled_pat_raw",
                     "ppl_received_cur_raw", "ppl_received_fy_raw",
                     # v21.32: the two non-cash binaries, kept for the
                     # same reason and dropped at the same place. Missing
                     # them here is the v21.29 failure exactly: the drop
                     # is name-based, the audit intersects with
                     # names(panel_full), and an audit with nothing to
                     # read PRINTS A PASS IT NEVER RAN.
                     "noncash_any_raw", "employer_childcare_raw",
                     "jttrwrk_raw", "jttpewt_raw", "jttpeot_raw",
                     "jttopwt_raw", "jttopot_raw",
                     "firm_size_raw")  # v19: the uncollapsed code, needed
  # to reconcile jbmemsz against
  # jbmems2 across the wave-22 break
  raw_cols <- grep("_raw$", names(wave_broad), value = TRUE)
  raw_cols <- setdiff(raw_cols, RAW_COLS_KEEP)
  wave_broad <- wave_broad %>% dplyr::select(-any_of(raw_cols))
  
  # Fail here rather than 400 lines downstream if a kept column is gone.
  keep_missing <- setdiff(RAW_COLS_KEEP, names(wave_broad))
  if (length(keep_missing))
    stop("RAW_COLS_KEEP column(s) absent after the _raw drop: ",
         paste(keep_missing, collapse = ", "),
         ". They were never created, or were dropped earlier.", call. = FALSE)
  
  # v21.11. The wave-prefixed source columns, dropped exactly.
  if (isTRUE(DROP_WAVE_PREFIXED_RAWS)) {
    .src_read <- stats::na.omit(unlist(v, use.names = FALSE))
    .src_drop <- setdiff(intersect(.src_read, names(wave_broad)),
                         RAW_COLS_KEEP)
    .n_before <- ncol(wave_broad)
    wave_broad <- wave_broad %>% dplyr::select(-any_of(.src_drop))
    # Same discipline as the _raw drop above: fail here, not downstream.
    .still <- setdiff(RAW_COLS_KEEP, names(wave_broad))
    if (length(.still))
      stop("RAW_COLS_KEEP column(s) removed by the wave-prefixed drop: ",
           paste(.still, collapse = ", "),
           ". Add them to the setdiff() or set ",
           "DROP_WAVE_PREFIXED_RAWS <- FALSE.", call. = FALSE)
    cat(sprintf("  Wave-prefixed source columns dropped: %d (%d -> %d)\n",
                length(.src_drop), .n_before, ncol(wave_broad)))
    rm(.src_read, .src_drop, .n_before, .still)
  }
  cat("  Columns retained:", ncol(wave_broad), "\n")
  cat("  Memory:", format(object.size(wave_broad), units = "MB"), "\n")
  
  return(wave_broad)
}

# =====================================================================
# PROCESS ALL WAVES
# =====================================================================

process_all_waves <- function(data_dir, wpi_data) {
  
  cat("\n############################################\n")
  cat("# HILDA DATA PROCESSING - WAVES 15-24     #\n")
  cat("############################################\n")
  
  # ------------------------------------------------------------------
  # CHECKPOINT VERSION: bump this string whenever the wave processing
  # logic changes (variable construction, recoding, new variables).
  # Old checkpoints with a different version tag are ignored and the
  # wave is reprocessed from the raw .dta file.
  # Current change (v5): potential_experience fallback is NA-propagating
  # (missing age/educ_years -> NA, not 0); employed is NA-propagating;
  # raw reads are column-restricted. Earlier (v4): experience_years uses
  # ehtjb with biological ceiling (age-13) only; Mincer cap removed.
  # ------------------------------------------------------------------
  # v6: unified loader, waves 12-24, deflator column carried, all
  #     outputs written from one pass. (v5: NA-propagating experience
  #     and employed; column-restricted reads.)
  # v8: adds life-satisfaction domains (losat, losateo, losatfs, losatft,
  #     losathl, losatlc, losatnl, losatsf, losatyh) for the Part G IV
  #     income-effect placebo. Bumped from v7 so every wave reprocesses
  #     and the new columns are read from the raw .dta files.
  # v5: bumped because the wscei divisor is reverted and annual_income
  # is rebuilt from a single variable. Checkpoints are keyed on this
  # string; reusing an older value silently replays stale waves. Delete
  # wave_checkpoints/ as an alternative to bumping.
  # v7: bumped because the variable map gained emptype/businc and the
  # extraction gained the self-employment and business-income columns.
  # Checkpoints are keyed on this string; reusing an older value would
  # silently replay waves built without them.
  # v10: the tag below is the human-readable provenance label; a short
  # hash of the CONTENT-DETERMINING inputs is appended automatically.
  #
  # WHY. A checkpoint is stale whenever the wave files it caches would
  # be built differently today. The version string is supposed to encode
  # that, but bumping it is manual, and three fixes in this project have
  # now been silently masked by a stale cache: the ehto rebuild, the
  # RAW_COLS_KEEP fix, and its rerun. The failure mode is nasty because
  # the loader completes normally and emits a panel built to the OLD
  # rules, so the only symptom is a diagnostic that has not changed.
  #
  # WHAT IS COVERED. The variable map (which mnemonics are read), the
  # v7 constants, the employment-type code sets, and the age window.
  # Change any of these and the cache invalidates by itself.
  #
  # WHAT IS NOT. The bodies of the mutate() calls that derive columns.
  # Hashing those would mean hashing the source file, which invalidates
  # on a comment edit and defeats the point of caching at all. So: after
  # changing derived-column LOGIC, bump the tag manually. After changing
  # WHICH VARIABLES are read, do nothing; the hash handles it.
  # v17: derived-column LOGIC changed (hard rename of the birth event,
  # quarter derivations, care and wealth blocks). The map hash catches
  # the added names on its own; the tag must be bumped by hand for the
  # mutate bodies. Doing one without the other is how three earlier
  # fixes were masked by stale checkpoints.
  # v21.5: bumped because the mutate bodies changed as well as the map.
  # The map hash would have invalidated the cache on its own, but relying
  # on that couples two things that should not be coupled.
  # v21.28. BUMPED: casual/permanent changes inside the wave
  # mutate and the hash cannot see a derived column.
  # v21.29. BUMPED: the aim gate and _z twins are derived inside the
  # wave mutate and the hash cannot see a derived column.
  # v21.31b. BUMPED AGAIN, and the reason is worth recording because this
  # trap has now fired three times in one working session.
  #
  # v21.31 created .prec_mat as a MATRIX-column inside the wave mutate.
  # That poisoned the wave checkpoints. The fix removed the matrix but did
  # NOT bump this tag, so the hash still matched, every wave was restored
  # from the poisoned cache, and the identical error recurred on a file
  # that no longer contained the bug.
  #
  # THE RULE, restated because writing it down twice was not enough:
  # the hash covers the variable MAP and the constants. It CANNOT see a
  # derived column. Any edit inside the wave mutate needs this tag bumped
  # BY HAND, including an edit that only removes something.
  # v21.33. BUMPED: the pre-imputed columns and the new gate are derived
  # inside the wave mutate, and the hash cannot see a derived column.
  # v21.34. BUMPED. The map is UNCHANGED, so the hash would not have
  # moved on its own and a cached wave would have kept the old gate
  # values under the new build tag. That is the failure mode the note
  # above describes and this file has now hit three times.
  # v21.35. NOT BUMPED, and this is the first build in the batch where
  # that is the right answer. v21.34 changed a derived column inside the
  # wave mutate and had to move this by hand; v21.35 changes a COMMENT
  # and a diagnostic that runs after combination, so no cached wave
  # holds anything stale. Bumping anyway would cost thirteen wave
  # re-extractions to reprint one line, and a tag that moves when
  # nothing changed teaches the reader to ignore it.
  # v21.42. BUMPED, and the first v21.42 run proves why it had to be.
  #
  # The total_own_children NA fix is a DERIVED COLUMN CHANGE INSIDE THE WAVE
  # MUTATE. The variable map is untouched, so the hash did not move, so all
  # thirteen waves came back from
  # v21_34_gate_no_coding_assumption_c8d8b52d and the fix never executed.
  # The log read "completed in 0m 32s", printed no NA-preserving diagnostic,
  # and produced a roster identical to the previous build: a clean run in
  # which nothing that changed had happened.
  #
  # This is the failure the note above describes and it is now the fourth
  # occurrence in this file. The rule has no exception worth taking: if
  # anything inside the wave mutate changes, bump this by hand, because the
  # hash covers what is READ and not what is DERIVED from it.
  # v21.43. BUMPED BY HAND, and the map has NOT changed. jttrcst was
  # already mapped and already derived; what is new is training_self_funded_z,
  # a derived column inside the wave mutate. The hash covers what is READ and
  # not what is DERIVED from it, so without this bump every wave would come
  # back from cache and the new column would simply not exist - the exact
  # failure the v21.42 note above records for total_own_children.
  # v21.45. BUMPED, AND NOT FOR A DERIVED-COLUMN CHANGE. Nothing inside
  # the wave mutate moved; the lifetime-parenthood work all runs after
  # combination. This bump forces one deliberate full re-extraction, for
  # two things a cached run cannot give: the presence table and the
  # absence audit, which are built during extraction, and a validation of
  # caches whose integrity has rested on manual bumping since the hash
  # last moved at v21.34. The name is the documentation. If a later
  # reader is deciding whether v21.45 changed the panel, the answer is in
  # the fingerprint comparison in the log, and it should read "reproduced
  # exactly". If it does not, that is the finding.
  # v21.48. BUMPED BY HAND. The map gains jttrdk and jttrrf, so the hash
  # moves anyway. The tag moves too, because three derived columns are new
  # and six change inside the wave mutate, and the rule above is not to
  # rely on the hash for a derived change.
  #CHECKPOINT_TAG <- "v21_48_training_dosage_aim_dk"
  CHECKPOINT_TAG <- "v21_50"
  # Content-determining inputs. get_wave_vars() is called for a fixed
  # wave letter purely to enumerate the map deterministically; the
  # NAMES and the mnemonic SUFFIXES are what matter, not the letter.
  .ckpt_inputs <- tryCatch({
    vmap <- get_wave_vars("x")
    paste(
      # Names: which SLOTS exist in the map.
      paste(sort(names(vmap)), collapse = ","),
      # Mnemonics: strip the leading wave letter so the fingerprint is
      # wave-invariant. unlist() FIRST, because businc holds two
      # elements and list-level subsetting would misalign against an
      # unlisted NA mask.
      paste(sort(substring(stats::na.omit(unlist(vmap, use.names = FALSE)), 2)),
            collapse = ","),
      paste(V7_EMPTYPE, collapse = ","),
      paste(V7_BUSINC, collapse = ","),
      paste(EMPTYPE_EMPLOYEE, collapse = ","),
      paste(EMPTYPE_SELFEMP_NARROW, collapse = ","),
      paste(EMPTYPE_UNPAID_FAMILY, collapse = ","),
      AGE_MIN, AGE_MAX, paste(range(WAVES), collapse = "-"),
      # v21.11: in the hash because it changes what a checkpoint HOLDS.
      # Without it, flipping the switch would leave the old caches looking
      # valid and the combined frame would carry whichever columns each
      # wave happened to be written with.
      paste0("dropraw=", DROP_WAVE_PREFIXED_RAWS),
      sep = "|"
    )
  }, error = function(e) NA_character_)
  
  # Short hash. Uses digest if available, otherwise a base-R fallback so
  # the loader never fails for want of an optional package.
  .ckpt_hash <- if (is.na(.ckpt_inputs)) {
    "nohash"
  } else if (requireNamespace("digest", quietly = TRUE)) {
    substr(digest::digest(.ckpt_inputs, algo = "md5"), 1, 8)
  } else {
    # Deterministic 32-bit rolling hash over the raw bytes.
    b <- utf8ToInt(.ckpt_inputs)
    h <- 5381
    for (ch in b) h <- (h * 33 + ch) %% 4294967296
    sprintf("%08x", h)
  }
  
  CHECKPOINT_VERSION <- paste0(CHECKPOINT_TAG, "_", .ckpt_hash)
  
  temp_dir <- file.path(data_dir, "wave_checkpoints")
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  cat("  Checkpoint directory:", temp_dir, "\n")
  cat("  Checkpoint version:  ", CHECKPOINT_VERSION, "\n")
  cat("    tag ", CHECKPOINT_TAG, " (manual: bump after changing derived-column logic)\n")
  cat("    hash", .ckpt_hash,
      if (identical(.ckpt_hash, "nohash"))
        "(variable map unavailable; cache NOT auto-invalidating)"
      else "(auto: variable map, v7 constants, code sets, age window)", "\n")
  if (identical(.ckpt_hash, "nohash"))
    warning("Checkpoint hash could not be computed, so the cache will ",
            "not invalidate automatically. Bump CHECKPOINT_TAG by hand ",
            "after any change to the variable map.", call. = FALSE)
  
  # v21.45. FORCED RE-EXTRACTION. Retire the directory rather than delete
  # it: the previous build stays recoverable and stays available as a
  # diff target for the fingerprint comparison at the end of the file.
  # The tag and the hash are untouched, so the checkpoints written on the
  # way out carry the same version and the next run with the switch off
  # restores from them normally.
  .force <- isTRUE(get0("FORCE_REEXTRACT", ifnotfound = FALSE))
  if (.force) {
    .existing <- list.files(temp_dir, pattern = "\\.rds$", full.names = TRUE)
    if (length(.existing)) {
      .retired <- file.path(
        data_dir,
        paste0("wave_checkpoints_retired_", CHECKPOINT_VERSION, "_",
               format(Sys.time(), "%Y%m%d_%H%M%S")))
      .ok <- file.rename(temp_dir, .retired)
      if (!.ok) {
        # Rename can fail across a mount or with a file open. Say so and
        # stop rather than half-forcing, because a half-forced run is the
        # partial extraction the absence audit cannot survive.
        stop(BUILD_TAG, ": FORCE_REEXTRACT could not rename ", temp_dir,
             ". Close anything holding a checkpoint open, or move the ",
             "directory by hand, then rerun. A partial re-extract would ",
             "produce a misleading absence audit, so this does not ",
             "proceed on a best-effort basis.", call. = FALSE)
      }
      dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
      cat("\n  *** FORCE_REEXTRACT is ON. All", length(.existing),
          "checkpoint file(s) retired to:\n")
      cat("      ", .retired, "\n", sep = "")
      cat("      The tag has NOT moved and the hash has NOT moved. This is\n")
      cat("      a forced extraction, not a logic change, and the panel it\n")
      cat("      produces must match the previous one. See the fingerprint\n")
      cat("      comparison at the end of the run.\n")
      cat("      TURN THE SWITCH OFF AFTERWARDS.\n\n")
    } else {
      cat("\n  FORCE_REEXTRACT is ON but the directory is already empty,\n")
      cat("  so every wave extracts anyway and nothing was retired.\n\n")
    }
    rm(.existing)
  }
  .v21_facts$force_reextract    <- .force
  .v21_facts$checkpoint_version <- CHECKPOINT_VERSION

  # Warn if stale (unversioned) checkpoints exist from a previous run
  stale <- list.files(temp_dir, pattern = "^wave_\\d+\\.rds$", full.names = TRUE)
  if (length(stale) > 0) {
    cat("  WARNING:", length(stale), "unversioned checkpoint(s) found and will be ignored.\n")
    cat("  Delete them manually if you no longer need them:\n")
    cat("    unlink(list.files(\'" , temp_dir, "\', pattern=\'wave_\\\\d+\\.rds$\', full.names=TRUE))\n")
  }
  
  # v10: report checkpoints from OTHER versions. Under the old scheme
  # these were the files that got silently reused after a fix; now they
  # are simply ignored, and naming them makes the invalidation visible
  # rather than leaving the user wondering why a rebuild is running.
  others <- setdiff(
    list.files(temp_dir, pattern = "^wave_\\d+_.+\\.rds$"),
    paste0("wave_", WAVES, "_", CHECKPOINT_VERSION, ".rds")
  )
  if (length(others) > 0) {
    other_vers <- unique(sub("^wave_\\d+_(.+)\\.rds$", "\\1", others))
    cat("  Checkpoints from", length(other_vers), "other version(s) present and IGNORED:\n")
    for (ov in head(other_vers, 5)) cat("    ", ov, "\n")
    if (length(other_vers) > 5)
      cat("     ... and", length(other_vers) - 5, "more\n")
    cat("  These will be REPROCESSED under the current version.\n")
  }
  
  wave_files <- c()
  
  for(w in WAVES) {
    temp_file <- file.path(temp_dir, paste0("wave_", w, "_", CHECKPOINT_VERSION, ".rds"))
    
    # RESUME: skip only if a checkpoint with the CURRENT version tag exists
    if (file.exists(temp_file)) {
      cat(sprintf("  Wave %d: checkpoint found (%s), skipping processing\n", w, CHECKPOINT_VERSION))
      wave_files <- c(wave_files, temp_file)
      # v21.1: recorded, because the per-wave audits are built during
      # extraction and a skipped wave contributes nothing to them.
      .v21_facts$waves_cached <- c(.v21_facts$waves_cached, w)
      next
    }
    
    wave_result <- process_wave_complete(w, data_dir, wpi_data)
    
    if(!is.null(wave_result)) {
      # Save to disk immediately, free memory
      saveRDS(wave_result, temp_file)
      wave_files <- c(wave_files, temp_file)
      .v21_facts$waves_processed <- c(.v21_facts$waves_processed, w)
      rm(wave_result)
    }
    
    gc()
    cat("  Memory freed. Files saved:", length(wave_files), "\n\n")
  }
  
  # Combine by reading back one at a time
  cat("\n===========================================\n")
  cat("Combining all waves from disk...\n")
  cat("===========================================\n")
  
  combined_data <- NULL
  for(f in wave_files) {
    cat("  Reading:", basename(f), "...")
    wave_chunk <- readRDS(f)
    if(is.null(combined_data)) {
      combined_data <- wave_chunk
    } else {
      combined_data <- dplyr::bind_rows(combined_data, wave_chunk)
    }
    rm(wave_chunk)
    gc()
    cat(" Total rows:", format(nrow(combined_data), big.mark = ","), "\n")
  }
  
  # Versioned checkpoints are retained for resume capability.
  # To force full reprocessing, either bump CHECKPOINT_VERSION above
  # or delete the checkpoint folder:
  #   unlink(file.path(data_dir, "wave_checkpoints"), recursive = TRUE)
  cat("  Versioned checkpoints retained in:", temp_dir, "\n")
  cat("  (Bump CHECKPOINT_VERSION or delete folder to force reprocessing)\n")
  
  # ================================================================
  # v21.49  PARENTHOOD FROM OWN CHILDREN ONLY, AND EDUCATION AS A
  #         RUNNING MAXIMUM
  # ================================================================
  # Both fixes run here, after the waves are combined, so the wave
  # caches stay valid. Each keeps the value it replaces: ever_parent_ref
  # and educ_years_ref reproduce the v21.48 panel. The v21.49 note at
  # the top of the file gives the reasons and the numbers to expect.
  #
  # This block sits before the cummax fix and before "Add final
  # variables". So the cummax, the rebuilt statuses, the lifetime block,
  # the groups, the interactions and educ_centered all read the new
  # values without further edits.
  cat("\n--- v21.49: parenthood from own children; education as a running maximum ---\n")
  .v49_need <- c("person_id", "wave", "female", "age", "ever_parent",
                 "total_children_ever_had", "total_own_children",
                 "has_any_nonresident_children", "num_children_under15",
                 "educ_years", "actual_experience", "education_pathway",
                 "has_nursing_any", "has_teaching_any", "has_business_any",
                 "has_trade_any", "has_technical_any")
  .v49_gone <- setdiff(.v49_need, names(combined_data))
  if (length(.v49_gone))
    stop(BUILD_TAG, ": the v21.49 block needs column(s) the combined ",
         "frame lacks: ", paste(.v49_gone, collapse = ", "),
         ". A wave cache written before they existed would explain it.",
         call. = FALSE)
  rm(.v49_need, .v49_gone)

  # Both fixes run within person and forward in time.
  combined_data <- combined_data[order(combined_data$person_id,
                                       combined_data$wave), ]

  # ---- Parenthood -------------------------------------------------
  # The wave mutate's rule (6C) without source 3, the household count of
  # children under 15. That count includes every child in the household,
  # and 6C checks it before the reported-zero rule. So a person who
  # reported no children of their own became a parent while a partner's
  # child or a young sibling lived with them. Now a reported zero on
  # tchad or on tcr + tcnr decides.
  .ep_ref <- combined_data$ever_parent
  .ep_new <- with(combined_data, dplyr::case_when(
    total_children_ever_had > 0             ~ 1L,
    total_own_children > 0                  ~ 1L,
    has_any_nonresident_children %in% TRUE  ~ 1L,
    total_children_ever_had == 0            ~ 0L,
    total_own_children == 0                 ~ 0L,
    TRUE                                    ~ NA_integer_))

  cat("  Per-wave status before the cummax, old rule (rows) against the\n",
      "  v21.49 rule (columns):\n", sep = "")
  print(table(old_rule = .ep_ref, new_rule = .ep_new, useNA = "ifany"))
  .hh_only <- .ep_ref %in% 1L & .ep_new %in% 0L &
    dplyr::coalesce(combined_data$num_children_under15 > 0, FALSE)
  cat(sprintf(paste0("  A household child under 15 was the only sign of a child on ",
                     "%s person-waves\n  (%s persons).\n"),
              format(sum(.hh_only), big.mark = ","),
              format(length(unique(combined_data$person_id[.hh_only])),
                     big.mark = ",")))
  cat(sprintf(paste0("  Unknown under the v21.49 rule: %s person-waves. The cummax ",
                     "below reads\n  them as 0, as it did under the old rule.\n"),
              format(sum(is.na(.ep_new)), big.mark = ",")))

  # The old flag is made absorbing exactly as the cummax below does it,
  # so ever_parent_ref equals the v21.48 ever_parent.
  combined_data$ever_parent_ref <- as.integer(ave(
    as.numeric(dplyr::coalesce(.ep_ref, 0L)),
    combined_data$person_id, FUN = cummax))
  combined_data$ever_parent <- .ep_new

  # The wave mutate built approx_age_at_first_birth from the per-wave
  # flag, so it is rebuilt from the new one.
  if (all(c("approx_age_at_first_birth", "age_oldest_child") %in%
          names(combined_data))) {
    .aafb_was <- combined_data$approx_age_at_first_birth
    combined_data$approx_age_at_first_birth <- ifelse(
      !is.na(combined_data$age_oldest_child) & .ep_new %in% 1L,
      combined_data$age - combined_data$age_oldest_child, NA_real_)
    cat(sprintf("  approx_age_at_first_birth set to missing on %s person-waves.\n",
                format(sum(!is.na(.aafb_was) &
                             is.na(combined_data$approx_age_at_first_birth)),
                       big.mark = ",")))
    rm(.aafb_was)
  }

  # Lifetime status under each rule, built the way the lifetime block
  # below builds ever_parent_life: the maximum over waves of the flag
  # and of tchad above 0.
  .tche49   <- as.numeric(dplyr::coalesce(
    as.numeric(combined_data$total_children_ever_had), 0) > 0)
  .life_ref <- ave(pmax(as.numeric(combined_data$ever_parent_ref), .tche49),
                   combined_data$person_id, FUN = max)
  .life_new <- ave(pmax(as.numeric(dplyr::coalesce(.ep_new, 0L)), .tche49),
                   combined_data$person_id, FUN = max)
  .lost <- !duplicated(combined_data$person_id) &
    .life_ref == 1 & .life_new == 0
  cat(sprintf(paste0("  Lifetime parents under the old rule who are never-parents ",
                     "now: %s\n  (men %s, women %s).\n"),
              format(sum(.lost), big.mark = ","),
              format(sum(.lost & combined_data$female %in% 0), big.mark = ","),
              format(sum(.lost & combined_data$female %in% 1), big.mark = ",")))
  rm(.ep_ref, .ep_new, .hh_only, .tche49, .life_ref, .life_new, .lost)

  # ---- Education --------------------------------------------------
  # A completed qualification cannot be lost, so educ_years becomes the
  # highest level the person has reported up to that wave. Missing stays
  # missing: the maximum fills no gap and reaches no wave before the
  # first report, so the Tier 1 sample does not move.
  .run_max49 <- function(x) {
    .k <- !is.na(x)
    if (any(.k)) x[.k] <- cummax(x[.k])
    x
  }
  combined_data$educ_years_ref <- combined_data$educ_years
  combined_data$educ_years <- ave(combined_data$educ_years_ref,
                                  combined_data$person_id, FUN = .run_max49)
  .up <- which(!is.na(combined_data$educ_years_ref) &
                 combined_data$educ_years > combined_data$educ_years_ref)
  cat(sprintf(paste0("\n  educ_years raised to an earlier level on %s person-waves ",
                     "(%.2f%%)\n  across %s persons.\n"),
              format(length(.up), big.mark = ","),
              100 * length(.up) / nrow(combined_data),
              format(length(unique(combined_data$person_id[.up])),
                     big.mark = ",")))

  # The wave mutate's rules for the columns built from educ_years, copied
  # from Step 3 and Step 4. They rebuild the raised person-waves. A check
  # runs first and confirms that they still reproduce the wave mutate on
  # every other person-wave.
  #
  # education_pathway also reads ednrstd, edtchts and edtchtp, which the
  # _raw drop removes at the end of each wave. Their three labels can
  # fire only below bachelor level. A raised person-wave still below
  # that level was below it before, so its stored label records them.
  # The stored label therefore stands in for the three items exactly.
  .lvl49 <- function(e) dplyr::case_when(
    e >= 19   ~ "Doctorate",
    e >= 18   ~ "Masters",
    e >= 17   ~ "Graduate_Diploma_Certificate",
    e >= 16.5 ~ "Bachelor_Honours",
    e >= 16   ~ "Bachelor",
    e >= 15   ~ "Advanced_Diploma",
    e >= 14   ~ "Diploma",
    e >= 13.5 ~ "Certificate_IV",
    e >= 13   ~ "Certificate_III",
    e >= 12.5 ~ "Certificate_II",
    e > 12    ~ "Certificate_I",
    e == 12   ~ "Year_12",
    e == 11   ~ "Year_11",
    e == 10   ~ "Year_10",
    e < 10 & !is.na(e) ~ "Below_Year_10",
    TRUE      ~ "Missing")
  .path49 <- function(e, stored, nur, tch, bus, trd, tec) dplyr::case_when(
    e >= 19 & nur ~ "Doctorate_Nursing",
    e >= 19 & tch ~ "Doctorate_Teaching",
    e >= 19 & bus ~ "Doctorate_Business",
    e >= 19       ~ "Doctorate_General",
    e >= 18 & nur ~ "Masters_Nursing",
    e >= 18 & tch ~ "Masters_Teaching",
    e >= 18 & bus ~ "Masters_Business",
    e >= 18       ~ "Masters_General",
    e >= 17 & nur ~ "GradDipCert_Nursing",
    e >= 17 & tch ~ "GradDipCert_Teaching",
    e >= 17 & bus ~ "GradDipCert_Business",
    e >= 17       ~ "GradDipCert_General",
    e >= 16.5 & nur ~ "Bachelor_Honours_Nursing",
    e >= 16.5 & tch ~ "Bachelor_Honours_Teaching",
    e >= 16.5 & bus ~ "Bachelor_Honours_Business",
    e >= 16.5       ~ "Bachelor_Honours_General",
    e >= 16 & nur ~ "Bachelor_Nursing",
    e >= 16 & tch ~ "Bachelor_Teaching",
    e >= 16 & bus ~ "Bachelor_Business",
    e >= 16       ~ "Bachelor_General",
    stored %in% "Registered_Nurse"        ~ "Registered_Nurse",
    stored %in% "Secondary_Teaching_Cert" ~ "Secondary_Teaching_Cert",
    stored %in% "Primary_Teaching_Cert"   ~ "Primary_Teaching_Cert",
    e >= 15 & nur ~ "AdvDiploma_Nursing",
    e >= 15 & tch ~ "AdvDiploma_Teaching",
    e >= 15 & trd ~ "AdvDiploma_Trade",
    e >= 15 & tec ~ "AdvDiploma_Technical",
    e >= 15       ~ "Advanced_Diploma",
    e >= 14 & nur ~ "Diploma_Nursing",
    e >= 14 & tch ~ "Diploma_Teaching",
    e >= 14 & trd ~ "Diploma_Trade",
    e >= 14 & tec ~ "Diploma_Technical",
    e >= 14 & bus ~ "Diploma_Business",
    e >= 14       ~ "Diploma",
    e >= 13.5 & trd ~ "Cert_IV_Trade",
    e >= 13.5 & tec ~ "Cert_IV_Technical",
    e >= 13.5 & nur ~ "Cert_IV_Nursing",
    e >= 13.5       ~ "Certificate_IV",
    e >= 13 & trd ~ "Cert_III_Trade",
    e >= 13 & tec ~ "Cert_III_Technical",
    e >= 13 & nur ~ "Cert_III_Nursing",
    e >= 13       ~ "Certificate_III",
    e >= 12.5 ~ "Certificate_II",
    e > 12 & e < 12.5 ~ "Certificate_I",
    e == 12 ~ "Year_12",
    e == 11 ~ "Year_11",
    e == 10 ~ "Year_10",
    e == 9  ~ "Year_9",
    e == 8  ~ "Year_8",
    e == 7  ~ "Year_7",
    e == 6  ~ "Primary_Complete",
    e == 5  ~ "Primary_Incomplete",
    is.na(e) ~ "No_Education_Data",
    TRUE ~ "Other")
  .in49  <- c("educ_years", "age", "actual_experience", "education_pathway",
              "has_nursing_any", "has_teaching_any", "has_business_any",
              "has_trade_any", "has_technical_any")
  .out49 <- intersect(c("educ_sq", "education_level", "education_pathway",
                        "has_degree", "has_postsecondary", "has_diploma_plus",
                        "has_postgrad", "potential_experience",
                        "experience_years", "experience_sq"),
                      names(combined_data))
  .rebuild49 <- function(rows) {
    d   <- combined_data[rows, .in49, drop = FALSE]
    e   <- d$educ_years
    pot <- pmax(0, d$age - e - 5)
    ex  <- pmin(dplyr::coalesce(d$actual_experience, pot), pmax(0, d$age - 13))
    ex  <- ifelse(ex < 0, 0, ex)
    list(
      educ_sq = e^2,
      education_level = .lvl49(e),
      education_pathway = .path49(e, d$education_pathway,
                                  d$has_nursing_any, d$has_teaching_any,
                                  d$has_business_any, d$has_trade_any,
                                  d$has_technical_any),
      has_degree = (e >= 16), has_postsecondary = (e > 12),
      has_diploma_plus = (e >= 14), has_postgrad = (e >= 17),
      potential_experience = pot, experience_years = ex,
      experience_sq = ex^2)[.out49]
  }
  # Elementwise "differs", with missing against observed counted as a
  # difference. Logicals compare as 0/1.
  .diff49 <- function(a, b) {
    if (is.logical(a)) a <- as.numeric(a)
    if (is.logical(b)) b <- as.numeric(b)
    d <- xor(is.na(a), is.na(b))
    k <- !is.na(a) & !is.na(b)
    d[k] <- if (is.numeric(a) && is.numeric(b)) abs(a[k] - b[k]) > 1e-9
            else as.character(a[k]) != as.character(b[k])
    d
  }

  # The check. Where the maximum did not move, the copies must give back
  # exactly what the wave mutate stored.
  .same <- which(!is.na(combined_data$educ_years_ref) &
                   combined_data$educ_years == combined_data$educ_years_ref)
  .chk  <- .rebuild49(.same)
  .mis  <- vapply(names(.chk), function(.nm)
    as.numeric(sum(.diff49(.chk[[.nm]], combined_data[[.nm]][.same]))),
    numeric(1))
  if (any(.mis > 0)) {
    cat("  *** The copied rules disagree with the wave mutate on unchanged\n",
        "      person-waves, by column:\n", sep = "")
    print(.mis[.mis > 0])
    warning(BUILD_TAG, ": the v21.49 copies of the Step 3 and Step 4 rules ",
            "no longer reproduce the wave mutate (",
            paste(names(.mis)[.mis > 0], collapse = ", "), "). A rule ",
            "changed in the wave mutate and not in the copy, or a cached ",
            "wave is stale. The raised person-waves were rebuilt with the ",
            "copies.", call. = FALSE)
  } else {
    cat(sprintf(paste0("  The copied rules reproduce the wave mutate on all %s ",
                       "unchanged person-waves\n  (%d columns).\n"),
                format(length(.same), big.mark = ","), length(.mis)))
  }

  if (length(.up)) {
    .e <- combined_data$educ_years[.up]
    .o <- combined_data$educ_years_ref[.up]
    cat("  Size of the raise, years:\n")
    print(summary(.e - .o))
    cat(sprintf("  has_degree switches on for %s person-waves, has_postgrad for %s.\n",
                format(sum(.e >= 16 & .o < 16), big.mark = ","),
                format(sum(.e >= 17 & .o < 17), big.mark = ",")))
    cat("  Earlier level (rows) against the level read at that wave (columns):\n")
    print(table(earlier = .e, read = .o))
    .exp_was <- combined_data$experience_years[.up]
    .new <- .rebuild49(.up)
    for (.nm in names(.new)) combined_data[[.nm]][.up] <- .new[[.nm]]
    .moved <- .diff49(.exp_was, combined_data$experience_years[.up])
    cat(sprintf(paste0("  Rebuilt on the raised person-waves: %s.\n",
                       "  experience_years moved on %s of them; %s of those have ",
                       "ehtjb (expect 0).\n"),
                paste(names(.new), collapse = ", "),
                format(sum(.moved), big.mark = ","),
                format(sum(.moved & !is.na(combined_data$actual_experience[.up])),
                       big.mark = ",")))
    rm(.e, .o, .exp_was, .new, .nm, .moved)
  }
  rm(.run_max49, .up, .lvl49, .path49, .in49, .out49, .rebuild49, .diff49,
     .same, .chk, .mis)
  gc()

  # ================================================================
  # CUMMAX FIX: Enforce monotonicity in ever_parent
  # ================================================================
  # Back-switching detected: 88 individuals (0.54%) had ever_parent
  # go from 1→0 due to tchad reporting inconsistencies across waves.
  # Fix: within each person, once ever_parent = 1, it stays 1.
  # Uses base R ave() to avoid dplyr memory overhead.
  
  cat("\n--- Enforcing ever_parent monotonicity with cummax() ---\n")
  n_before <- sum(combined_data$ever_parent == 1, na.rm = TRUE)
  
  # Sort by person + wave (required for cummax to work forward in time)
  combined_data <- combined_data[order(combined_data$person_id, combined_data$wave), ]
  
  # Base R cummax via ave() — no tibble copies, minimal memory
  combined_data$ever_parent <- as.integer(ave(
    as.numeric(coalesce(combined_data$ever_parent, 0L)),
    combined_data$person_id,
    FUN = cummax
  ))
  gc()
  
  n_after <- sum(combined_data$ever_parent == 1, na.rm = TRUE)
  cat(sprintf("  ever_parent == 1: %s -> %s (gained %d from cummax)\n",
              format(n_before, big.mark = ","),
              format(n_after, big.mark = ","),
              n_after - n_before))
  
  # Re-derive all downstream parenthood variables after cummax fix
  combined_data$ever_mother <- ifelse(combined_data$female == 1, combined_data$ever_parent, NA_integer_)
  combined_data$ever_father <- ifelse(combined_data$female == 0, combined_data$ever_parent, NA_integer_)
  
  combined_data$motherhood_status <- with(combined_data, case_when(
    female == 0 ~ NA_character_,
    ever_parent == 1 & num_children_under15 > 0 ~ "active_mother",
    ever_parent == 1 & num_children_under15 == 0 ~ "post_mother",
    ever_parent == 0 ~ "never_mother",
    TRUE ~ NA_character_
  ))
  
  combined_data$fatherhood_status <- with(combined_data, case_when(
    female == 1 ~ NA_character_,
    ever_parent == 1 & num_children_under15 > 0 ~ "active_father",
    ever_parent == 1 & num_children_under15 == 0 ~ "post_father",
    ever_parent == 0 ~ "never_father",
    TRUE ~ NA_character_
  ))
  
  # ================================================================
  # LIFETIME COUNTERPARTS: ever_parent_life and the _life statuses
  # ================================================================
  # THE CUMMAX ABOVE MAKES ever_parent ABSORBING, NOT CONSTANT. Once it
  # turns on it stays on, which is what monotonicity means and is all the
  # cummax was for. It is still 0 for a mother's pre-birth waves, because
  # she was not yet a parent when they were recorded, and that is correct.
  #
  # THE _ever SUFFIX AND THE LABELS READ AS THOUGH IT WERE CONSTANT, and
  # that has cost real work. Every consumer that cuts a cross-section by
  # motherhood_status gets a never_mother category holding true
  # never-mothers PLUS every future mother's pre-birth waves. That is a
  # legitimate before-and-after cut and several descriptive figures want
  # exactly it; it is NOT a comparison of women who become mothers against
  # women who do not, and it has been read as the second at least three
  # times. RUN_FIGURES_PAPER.R had to build its own grp_life to get round
  # it, and the experience-profile figure walked into it again by taking
  # this column instead.
  #
  # It matters most where the pre-birth waves are unusual. On the training
  # outcome they are: women who go on to become mothers train at 29.8 per
  # cent before a first birth, the highest rate anywhere in these data,
  # against 27.0 for the never_mother category that contains them. A
  # cross-sectional contrast on that category is measured against a
  # comparison group holding the treated group's own pre-birth
  # observations at the top of the distribution.
  #
  # So the lifetime version is built HERE, once, rather than rebuilt by
  # each consumer. Both are shipped and named for what they are. The rule
  # for choosing:
  #
  #   ever_parent, motherhood_status        a status AT a wave. Use for
  #                                         event studies, before-and-after
  #                                         cuts, anything indexed by k.
  #   ever_parent_life, *_status_life       a property OF a person. Use for
  #                                         cross-sections, descriptive
  #                                         tables, group means, and any
  #                                         comparison group meant to
  #                                         exclude the treated.
  #
  # THE ACTIVE/POST SPLIT STAYS WAVE-VARYING IN BOTH, deliberately. Whether
  # a woman currently has children under 15 at home is a fact about the
  # wave and not about her life, so _life differs from the original only in
  # which women are called never-mothers at all.
  # TWO SOURCES, NOT ONE. MASTER_training_y.R builds the same lifetime status
  # and takes the maximum over ever_parent AND total_children_ever_had, which
  # is the better rule: it catches a woman whose children never appear in the
  # household roster, and that is exactly the population Section 4.5 says a
  # current-children measure loses. Matched here so the loader and the master
  # cannot disagree about who is a parent.
  # THE SECOND INPUT IS A PER-WAVE COLUMN AND THIS BLOCK IS NOT, so it is
  # checked rather than assumed. total_children_ever_had is built inside
  # process_wave_complete() and therefore comes back from the wave cache;
  # everything here runs after the waves are combined. If a cache written
  # before that column existed were ever restored, coalesce would silently
  # read it as zero and the lifetime status would quietly fall back to
  # max(ever_parent), which is narrower and would no longer match the rule
  # MASTER_training_y.R applies. Absent means say so, not carry on.
  if (!"total_children_ever_had" %in% names(combined_data)) {
    cat("  *** total_children_ever_had is absent from the combined frame.\n",
        "      The lifetime status falls back to max(ever_parent), which is\n",
        "      NARROWER than the master's rule and will disagree with it for\n",
        "      women whose children never appear in the household roster.\n",
        "      Bump CHECKPOINT_TAG and rebuild the wave caches.\n", sep = "")
    .tche <- rep(0, nrow(combined_data))
  } else {
    .tche <- as.numeric(coalesce(combined_data$total_children_ever_had, 0L) > 0)
  }
  .ep_any <- pmax(as.numeric(coalesce(combined_data$ever_parent, 0L)),
                  .tche, na.rm = TRUE)
  combined_data$ever_parent_life <- as.integer(ave(
    .ep_any, combined_data$person_id, FUN = function(x) max(x, na.rm = TRUE)))
  rm(.ep_any, .tche)

  combined_data$ever_mother_life <-
    ifelse(combined_data$female == 1, combined_data$ever_parent_life, NA_integer_)
  combined_data$ever_father_life <-
    ifelse(combined_data$female == 0, combined_data$ever_parent_life, NA_integer_)

  # FOUR LEVELS, NOT THREE, and the fourth is the whole point. A woman
  # observed before her first birth is not a never-mother, and she is not a
  # post-mother either. A first draft of this block had no pre_ level and
  # the fallback swept her pre-birth waves into post_mother, which is worse
  # than leaving them in never_mother: post-mothers are the oldest and most
  # experienced women in the panel and she is among the youngest.
  #
  # With the fourth level the split is strictly more informative than
  # either alternative and nothing has to be guessed. Collapse pre_ into
  # never_ and you have the wave-varying column back; collapse it into
  # active_ and you have a lifetime binary; leave it separate and the
  # contamination is visible as its own column, which is what a table
  # decomposing it wants.
  combined_data$motherhood_status_life <- with(combined_data, case_when(
    female == 0              ~ NA_character_,
    ever_parent_life == 0    ~ "never_mother",
    ever_parent == 0         ~ "pre_mother",
    num_children_under15 > 0 ~ "active_mother",
    TRUE                     ~ "post_mother"
  ))

  combined_data$fatherhood_status_life <- with(combined_data, case_when(
    female == 1              ~ NA_character_,
    ever_parent_life == 0    ~ "never_father",
    ever_parent == 0         ~ "pre_father",
    num_children_under15 > 0 ~ "active_father",
    TRUE                     ~ "post_father"
  ))

  # THE SIZE OF THE RECLASSIFICATION IS PRINTED, NOT ASSUMED. If a future
  # build makes ever_parent person-invariant upstream these counts go to
  # zero and the log says so, which is the signal that the two columns can
  # be merged. Until then a non-zero count here is the measure of how much
  # a cross-section cut on the wave-varying column would misplace.
  .n_moved <- sum(combined_data$female == 1 &
                    combined_data$motherhood_status == "never_mother" &
                    combined_data$motherhood_status_life != "never_mother",
                  na.rm = TRUE)
  .p_moved <- length(unique(combined_data$person_id[
    combined_data$female == 1 &
      combined_data$motherhood_status == "never_mother" &
      combined_data$motherhood_status_life != "never_mother"]))
  cat("\n--- Lifetime parenthood counterparts ---\n")
  cat(sprintf("  never_mother person-waves: %s wave-varying -> %s lifetime\n",
              format(sum(combined_data$motherhood_status == "never_mother",
                         na.rm = TRUE), big.mark = ","),
              format(sum(combined_data$motherhood_status_life == "never_mother",
                         na.rm = TRUE), big.mark = ",")))
  cat(sprintf("  moved out: %s person-waves across %s women. These are\n",
              format(.n_moved, big.mark = ","), format(.p_moved, big.mark = ",")))
  cat("  future mothers' pre-birth waves. The wave-varying column places\n")
  cat("  them in the comparison group; the lifetime one gives them their\n")
  cat("  own pre_mother level so they are neither a control nor pooled\n")
  cat("  with post-mothers.\n")
  if (.n_moved == 0)
    cat("  *** ZERO. ever_parent is person-invariant in this build, so the\n",
        "      two columns are the same and the _life set is redundant.\n", sep = "")
  rm(.n_moved, .p_moved)

  # Verify: no back-switching should remain
  n_bad <- sum(ave(
    as.numeric(combined_data$ever_parent),
    combined_data$person_id,
    FUN = function(x) any(diff(x) < 0, na.rm = TRUE)
  ) > 0) / max(table(combined_data$person_id))  # rough count
  # Cleaner check:
  ep_check <- tapply(combined_data$ever_parent, combined_data$person_id,
                     function(x) any(diff(x) < 0, na.rm = TRUE))
  n_bad_persons <- sum(ep_check, na.rm = TRUE)
  cat(sprintf("  Back-switching after fix: %d individuals (should be 0)\n", n_bad_persons))
  if (n_bad_persons > 0) warning("cummax() did not eliminate all back-switching!")
  
  # Report updated group counts
  cat("\n  Updated group counts after cummax:\n")
  cat("  ever_parent == 1:", sum(combined_data$ever_parent == 1, na.rm = TRUE), "\n")
  
  # ================================================================
  # v17 POST-COMBINATION: PRESENCE SUMMARY, QUARTER DIRECTION,
  #                       POST-BIRTH WINDOW SHARE, DEPRECATION REPORT
  # ================================================================
  cat("\n===========================================\n")
  cat("v19 NEW-MNEMONIC PRESENCE, ALL WAVES\n")
  cat("===========================================\n")
  # v21.1: state which waves ran BEFORE reporting anything built during
  # extraction. On a fully cached run the three audits in this section
  # have no input, and the v21 log showed what that looks like: seven
  # candidate stems reported as unresolved while the panel held tens of
  # thousands of observations on each of them.
  .n_proc <- length(.v21_facts$waves_processed)
  .n_cach <- length(.v21_facts$waves_cached)
  cat(sprintf("  Waves extracted this run: %d%s\n", .n_proc,
              if (.n_proc) paste0(" (", paste(.v21_facts$waves_processed,
                                              collapse = ", "), ")") else ""))
  cat(sprintf("  Waves restored from checkpoint: %d%s\n", .n_cach,
              if (.n_cach) paste0(" (", paste(.v21_facts$waves_cached,
                                              collapse = ", "), ")") else ""))
  if (.n_proc == 0L) {
    cat("  NOTE: the presence table and the absence audit below are built\n")
    cat("        during extraction, which did not run. They describe THIS\n")
    cat("        RUN and not the panel, and absence of a report here is\n")
    cat("        NOT evidence that a mnemonic failed to resolve. The\n")
    cat("        degenerate-column sweep further down reads the panel and\n")
    cat("        is the audit to believe on a cached run. To re-evaluate\n")
    cat("        these, delete wave_checkpoints/ and rerun.\n")
  }
  if (length(.v17_presence$rows)) {
    .pres <- do.call(rbind, Filter(is.data.frame, .v17_presence$rows))
    .tab <- tapply(.pres$present, list(.pres$stem, .pres$wave),
                   function(z) if (isTRUE(z[1])) "Y" else ".")
    print(as.data.frame(.tab))
    .cov <- tapply(.pres$present, .pres$stem, function(z) sum(z))
    .nw  <- length(unique(.pres$wave))
    cat("\n  Waves carrying each new mnemonic (of", .nw, "):\n")
    for (nmz in names(.cov))
      cat(sprintf("    %-10s %2d/%2d  %s\n", nmz, .cov[[nmz]], .nw,
                  ifelse(.cov[[nmz]] == 0, "ABSENT FROM RELEASE",
                         ifelse(.cov[[nmz]] < .nw, "partial coverage", ""))))
    .never <- names(.cov)[.cov == 0 & names(.cov) %in% V17_CONFIRMED_STEMS]
    if (length(.never))
      warning(BUILD_TAG, ": mnemonic(s) recorded as confirmed-present are absent ",
              "from every wave: ", paste(.never, collapse = ", "),
              ". The variable map is wrong, not the release.", call. = FALSE)
  }
  if (length(.v17_presence$qcode)) {
    cat("\n  Quarter-flag coding by wave:\n")
    print(do.call(rbind, .v17_presence$qcode), row.names = FALSE)
  }
  .picks <- .v17_presence$rows[grepl("^pick_", names(.v17_presence$rows))]
  # v21.1: the stem that resolved is recorded during extraction, but
  # whether the derived column HAS DATA can be read off the panel on any
  # run, cached or not. Both are printed, and the panel count is what
  # settles it: a column with observations resolved, whatever this run
  # happens to remember about it.
  .pick_col <- c(lshrcar      = "hours_lshrcar",
                 lshrchd      = "hours_lshrchd",
                 commute      = "commute_hours",
                 volunteer    = "volunteer_hours",
                 super        = "super_balance",
                 networth_pos = "hh_net_worth_pos",
                 networth_neg = "hh_net_worth_neg")
  cat("\n  Candidate stems: which one resolved, and what the panel holds\n")
  for (fam in names(.pick_col)) {
    got <- .picks[[paste0("pick_", fam)]]
    .cl <- .pick_col[[fam]]
    .nob <- if (.cl %in% names(combined_data))
      sum(!is.na(combined_data[[.cl]])) else NA_integer_
    .msg <- if (is.na(.nob)) {
      "derived column NOT ON THE PANEL"
    } else if (.nob > 0L) {
      sprintf("%s, %s obs on the panel",
              if (is.null(got)) "stem not recorded this run (cache)"
              else paste0("using ", got),
              format(.nob, big.mark = ","))
    } else {
      sprintf("ALL-NA on the panel%s",
              if (is.null(got)) " and no stem recorded this run"
              else paste0(" despite resolving to ", got))
    }
    cat(sprintf("    %-12s %-12s %s\n", fam, .cl, .msg))
  }
  cat("  Narrow the *_STEMS vectors on the PANEL column, not on the stem\n")
  cat("  record: the latter is empty whenever the waves came from cache.\n")
  
  # ---- v21.33 NON-CASH DIAGNOSTIC ---------------------------------
  # Three things worth seeing before anyone uses these columns, and none
  # is visible from a coverage percentage.
  #
  # (1) THE GATE. How much of the zero mass was never asked. The DV
  #     fills the non-employed with zero, so an ungated mean of
  #     non-cash benefits is a mean over employees AND non-employees
  #     with the latter contributing a structural zero.
  # (2) THE TWO HAS-ANY ROUTES. noncash_any_value is arithmetic on the
  #     DV; noncash_any reads F10 through the == 1 rule. They cover
  #     different populations, so they are cross-tabulated rather than
  #     assumed to agree.
  # (3) THE UPPER TAIL. Maxima run past $20,000 a week and one wave
  #     reaches $47,980, which is $2.5m a year in non-cash benefits.
  #     That is either an executive package or a coding error, and the
  #     training-hours episode says to look rather than assume. Printed
  #     against the wage so the ratio is visible: a benefit worth many
  #     multiples of pay is the case to inspect.
  # The combined frame under whichever name is in scope here; panel_full
  # does not exist until later.
  .nc_src <- if (exists("combined_data")) combined_data else
    if (exists("analysis_sample")) analysis_sample else NULL
  if (!is.null(.nc_src) &&
      all(c("noncash_benefits_weekly", "noncash_module_asked",
            "noncash_any_value", "noncash_any") %in% names(.nc_src))) {
    cat("\n--- v21.33 non-cash benefits ---\n")
    .nc <- .nc_src
    cat(sprintf("  module asked: %s of %s person-waves (%.1f%%)\n",
                format(sum(.nc$noncash_module_asked == 1L, na.rm = TRUE), big.mark = ","),
                format(nrow(.nc), big.mark = ","),
                100 * mean(.nc$noncash_module_asked == 1L, na.rm = TRUE)))
    .z_all  <- sum(.nc$noncash_benefits_weekly == 0, na.rm = TRUE)
    .z_ask  <- sum(.nc$noncash_benefits_weekly == 0 &
                     .nc$noncash_module_asked == 1L, na.rm = TRUE)
    cat(sprintf("  zeros: %s total, %s of them asked (%.1f%%). The rest are\n",
                format(.z_all, big.mark = ","), format(.z_ask, big.mark = ","),
                100 * .z_ask / max(.z_all, 1)))
    cat("  DV zeros assigned without the question being put.\n")
    cat("\n  Has-any, the two routes:\n")
    print(table(from_value = .nc$noncash_any_value,
                from_F10   = .nc$noncash_any, useNA = "ifany"))
    cat("  Off-diagonal cells are the disagreement. Prefer the VALUE route:\n")
    cat("  a positive DV means a benefit whatever F10 says.\n")
    if ("weekly_earnings" %in% names(.nc)) {
      .hi <- .nc %>%
        dplyr::filter(is.finite(noncash_benefits_weekly),
                      noncash_benefits_weekly > 0,
                      is.finite(weekly_earnings), weekly_earnings > 0) %>%
        dplyr::mutate(ratio = noncash_benefits_weekly / weekly_earnings)
      if (nrow(.hi)) {
        cat(sprintf("\n  Non-cash as a multiple of weekly earnings, positives only:\n"))
        print(round(stats::quantile(.hi$ratio,
                                    c(.5, .9, .99, 1), na.rm = TRUE), 3))
        cat(sprintf("  Above 2x pay: %s person-waves (%.2f%% of positives).\n",
                    format(sum(.hi$ratio > 2), big.mark = ","),
                    100 * mean(.hi$ratio > 2)))
        cat("  Inspect before trimming. A definitional bound is available\n")
        cat("  here, unlike an outcome trim, so use one if any is needed.\n")
      }
      rm(.hi)
    }
    rm(.nc, .z_all, .z_ask)
  }
  
  # ---- v21.31b MATRIX-COLUMN GUARD --------------------------------
  # A data frame may legally hold a MATRIX as a column, and dplyr will
  # create one without complaint if a mutate() assigns cbind(). Nothing
  # downstream expects it: the coverage audit below walks the columns and
  # calls as.integer(!is.na(x)), which flattens an n x k matrix to n * k
  # elements against a grouping vector of length n, and fails with
  # "incorrect length for group" a long way from the cause.
  #
  # Cheap to check, so check it here, before the audits that would
  # otherwise report the symptom.
  # Checks whichever combined frame is in scope here; panel_full does not
  # exist until later.
  .guard_df <- if (exists("combined_data")) combined_data else
    if (exists("analysis_sample")) analysis_sample else NULL
  .mat_cols <- if (is.null(.guard_df)) character(0) else
    names(.guard_df)[vapply(.guard_df,
                            function(x) is.matrix(x) || is.array(x),
                            logical(1))]
  if (length(.mat_cols)) {
    stop(BUILD_TAG, ": matrix or array column(s) on the combined frame: ",
         paste(.mat_cols, collapse = ", "),
         ". A cbind() inside mutate() creates these. Sum term by term ",
         "instead, and remember the checkpoint tag: a cached wave built ",
         "before the fix still carries the matrix.", call. = FALSE)
  }
  rm(.mat_cols, .guard_df)
  
  # ---- v19 FULL-MAP ABSENCE AUDIT ---------------------------------
  # The table above covers 27 of roughly 228 mapped names. This one
  # covers the rest. A stem absent from EVERY wave cannot produce
  # anything but an all-NA column, and any indicator built on it will
  # be constant. That is how has_business_any came to read exactly zero
  # across the whole panel from a single unresolved name.
  cat("\n===========================================\n")
  cat("v19 FULL-MAP ABSENCE AUDIT\n")
  cat("===========================================\n")
  if (!length(.v17_presence$mapabsent) && .n_proc == 0L)
    cat("  Not evaluated: every wave was restored from checkpoint, so no\n",
        "  wave reported its absent names this run. This is not a clean\n",
        "  result; it is no result.\n", sep = "")
  # v21.45. A PARTIAL RE-EXTRACT MAKES THIS AUDIT WRONG, NOT WEAK.
  # .nw_all counts the waves on the COMBINED frame, which is all of them
  # however many extracted, while .ab carries rows only for the waves
  # that ran. On a partial run the ALL-waves test can therefore never
  # fire, and a stem genuinely absent everywhere is reported as absent
  # from some waves, which reads as a wave-coverage quirk rather than as
  # a dead mnemonic. Refuse to classify rather than classify wrongly.
  .partial <- .n_proc > 0L && .n_proc < length(WAVES)
  if (.partial) {
    cat("  PARTIAL RUN:", .n_proc, "of", length(WAVES), "waves extracted.\n")
    cat("  The ALL-waves against SOME-waves split below cannot be formed,\n")
    cat("  because the absent-name rows exist only for the waves that ran\n")
    cat("  while the wave count comes from the combined frame. Names are\n")
    cat("  listed with the waves that reported them and are NOT classified.\n")
    cat("  Set FORCE_REEXTRACT <- TRUE for an audit that can be read.\n")
  }
  if (length(.v17_presence$mapabsent)) {
    .ab <- do.call(rbind, Filter(is.data.frame, .v17_presence$mapabsent))
    if (nrow(.ab) && .partial) {
      .cnt <- sort(table(.ab$stem), decreasing = TRUE)
      cat(sprintf("\n  Mapped stems reported absent by at least one of the %d\n  waves that ran: %d\n",
                  .n_proc, length(.cnt)))
      print(as.data.frame(.cnt))
      cat("\n  Unclassified by design. See the note above.\n")
    } else if (nrow(.ab)) {
      .nw_all <- length(unique(combined_data$wave))
      .cnt <- sort(table(.ab$stem), decreasing = TRUE)
      .always <- names(.cnt)[.cnt >= .nw_all]
      .sometimes <- names(.cnt)[.cnt < .nw_all]
      cat(sprintf("  Mapped stems absent from ALL %d waves: %d\n",
                  .nw_all, length(.always)))
      if (length(.always))
        cat(strwrap(paste(sort(.always), collapse = ", "),
                    width = 72, prefix = "    "), sep = "\n")
      cat(sprintf("\n  Mapped stems absent from SOME waves: %d\n",
                  length(.sometimes)))
      if (length(.sometimes)) {
        .st <- .ab[.ab$stem %in% .sometimes, ]
        print(as.data.frame(tapply(rep(TRUE, nrow(.st)),
                                   list(.st$stem, .st$wave),
                                   function(z) "absent")))
      }
      cat("\n  Any derived column resting solely on a stem in the first\n")
      cat("  list is empty by construction. Remove it from the map or\n")
      cat("  find the correct mnemonic; do not leave it to surface as a\n")
      cat("  zero in a table.\n")
    } else {
      cat("  Every mapped mnemonic resolved in every wave.\n")
    }
  }
  
  # ---- v19 DEGENERATE-COLUMN AUDIT --------------------------------
  # Complements the audit above from the other end: instead of asking
  # which names failed to load, ask which derived columns came out
  # constant. Either symptom on its own is easy to read past; together
  # they name the column and the reason.
  #
  # v20: this now sweeps EVERY column on the panel, not a hand-kept
  # list. The list missed fy_benefits, which is all-NA because bnfip is
  # absent from all thirteen waves, and which feeds the transfer income
  # the FTB-B income test is built on. A hand-kept list can only catch
  # the failure someone already thought of.
  # ---- v21.7 PER-WAVE PRESENCE, COMPUTED ONCE -----------------------
  # Both sweeps need to know how many observations each column has in
  # each wave. It used to be computed twice, once as a column total here
  # and once as a per-wave matrix below, so this block walked every
  # column for a number the next block already had.
  .wv_lev <- sort(unique(combined_data$wave))
  .grp    <- factor(combined_data$wave, levels = .wv_lev)
  .nm_w   <- vapply(combined_data, function(x)
    as.integer(rowsum(as.integer(!is.na(x)), .grp, reorder = TRUE)),
    integer(length(.wv_lev)))
  rownames(.nm_w) <- as.character(.wv_lev)
  .obs_n  <- colSums(.nm_w)
  
  # ---- v21.7 WAVE-PREFIXED SOURCE COLUMNS ---------------------------
  # About four columns in five on combined_data are raw HILDA source
  # columns still carrying their wave letter: ledcqen, medcqen, nedcqen
  # and so on, thirteen names for one variable, each populated in the
  # single wave whose letter it carries. They are dropped at the final
  # select and never reach a saved panel.
  #
  # Both sweeps were reporting them as findings. The wave-hole sweep in
  # particular returned over two thousand columns "matching no known
  # shape", when the shape is the most ordinary fact about them, and a
  # list that long is not read at all. Naming them here leaves the
  # sweeps reporting on the derived columns they were built for.
  #
  # The test is on the data and the name together, not on a list:
  #   sibling  the same stem appears under at least one OTHER wave
  #            letter, which is what "one variable under thirteen names"
  #            means and holds even for a column that is all-NA;
  #   prefix   the column is populated in exactly one wave and its first
  #            letter is that wave's letter, which catches a source
  #            column that exists in one wave only and so has no sibling.
  # The underscore guard is the belt: every derived column in this file
  # carries one and no HILDA mnemonic does.
  #
  # Misclassifying here moves a name from one line of a report to
  # another. NOTHING IS DROPPED, RENAMED, OR EXCLUDED FROM THE PANEL.
  .all_nm  <- colnames(.nm_w)
  .wv_ltr  <- letters[.wv_lev]
  .first   <- substring(.all_nm, 1, 1)
  .stem    <- substring(.all_nm, 2)
  .is_wl   <- .first %in% .wv_ltr
  .stem_ct <- table(.stem[.is_wl])
  .has_sib <- .is_wl &
    !is.na(.stem_ct[.stem]) & as.integer(.stem_ct[.stem]) > 1L
  .only_ltr <- vapply(seq_along(.all_nm), function(j) {
    p <- which(.nm_w[, j] > 0L)
    if (length(p) == 1L) letters[.wv_lev[p]] else NA_character_
  }, character(1))
  # v21.10 FIX. The underscore guard was applied to BOTH arms and it is
  # only sound on one of them. It rested on "no HILDA mnemonic carries an
  # underscore", which is false: hhd0_4 and hhd5_9 are the counts of
  # children aged 0-4 and 5-9 in the household, and the v21.9 run left all
  # 26 of their wave-prefixed copies unclassified. Twenty-two landed in
  # the unexplained list and four were absorbed by the single-wave shape
  # entries, which is why "wave 23 only" and "wave 24 only" read 2 each
  # rather than 0.
  #
  # The guard now applies ONLY to the prefix-only arm, where a derived
  # column could conceivably be single-wave and start with that wave's
  # letter by coincidence. It does NOT apply where the sibling test fires,
  # because a stem appearing under two or more DIFFERENT wave letters is
  # the definition of one source variable under many names, and no derived
  # column in this file has sibling copies of itself.
  .src_nm <- .all_nm[.is_wl &
                       (.has_sib |
                          (!grepl("_", .all_nm, fixed = TRUE) &
                             !is.na(.only_ltr) & .only_ltr == .first))]
  .is_src <- function(nm) nm %in% .src_nm
  
  # v21.11. The constant test used unique(x[!is.na(x)]) on every column,
  # which builds a hash table of up to 134,131 values to answer a question
  # that needs two of them. On a numeric column the range answers it in
  # one pass with no allocation, and the range is exactly what "constant"
  # means for a number. Non-numeric columns keep the old path, where the
  # early exit at two distinct values does most of the work anyway.
  #
  # The reported value changes from unique(...)[1] to the first non-NA
  # value, which is the same thing for a constant column and is only ever
  # read for a constant column, so the one remaining subset is paid about
  # thirteen times per run instead of 2,927.
  #
  # Measured: 12.0 s to 5.5 s over 2,927 columns, and to 0.6 s once
  # DROP_WAVE_PREFIXED_RAWS leaves only the 321 derived ones.
  .is_const <- function(x) {
    if (is.numeric(x)) {
      # No subset: range() takes one pass and allocates nothing, where
      # x[!is.na(x)] allocates a copy of up to 134,131 values for every
      # column scanned. On a varying column that subset is 8 ms of the
      # 10 ms the old test cost, and the sweep is almost all varying
      # columns. NO is.finite() guard: a column constant at Inf IS
      # constant, and the guard made the answer wrong for it. An all-NA
      # column returns c(Inf, -Inf), which are unequal, so it reads
      # FALSE without a special case; the caller reaches this only when
      # .obs > 0 in any event.
      r <- suppressWarnings(range(x, na.rm = TRUE))
      isTRUE(r[1] == r[2])
    } else {
      y <- x[!is.na(x)]
      length(y) > 0L && length(unique(y)) <= 1L
    }
  }
  cat("\n--- v20 degenerate columns: full-panel sweep ---\n")
  .n_rows <- nrow(combined_data)
  .empty <- character(0); .const <- character(0)
  for (.cn in names(combined_data)) {
    .x <- combined_data[[.cn]]
    if (is.list(.x)) next
    .obs <- .obs_n[[.cn]]
    if (.obs == 0L) {
      .empty <- c(.empty, .cn)
    } else if (.is_const(.x)) {
      .const <- c(.const, sprintf("%s = %s (%s obs)", .cn,
                                  format(.x[!is.na(.x)][1]),
                                  format(.obs, big.mark = ",")))
    }
  }
  cat(sprintf("  Columns scanned: %d over %s rows\n",
              ncol(combined_data), format(.n_rows, big.mark = ",")))
  cat(sprintf("    wave-prefixed source columns: %d (dropped at the final select)\n",
              length(.src_nm)))
  cat(sprintf("    derived and carried columns:  %d\n",
              ncol(combined_data) - length(.src_nm)))
  # v21.7: the two populations are reported apart. A source column that
  # is all-NA or constant is a fact about one wave of the release; a
  # DERIVED column that is either is a defect in this file.
  .const_nm  <- sub(" =.*$", "", .const)
  .empty_src <- .empty[.is_src(.empty)]
  .empty_drv <- .empty[!.is_src(.empty)]
  .const_src <- .const[.is_src(.const_nm)]
  .const_drv <- .const[!.is_src(.const_nm)]
  cat(sprintf("\n  ALL-NA columns: %d derived, %d source\n",
              length(.empty_drv), length(.empty_src)))
  if (length(.empty_drv))
    cat(strwrap(paste(sort(.empty_drv), collapse = ", "), width = 72,
                prefix = "    "), sep = "\n")
  cat(sprintf("\n  CONSTANT columns: %d derived, %d source\n",
              length(.const_drv), length(.const_src)))
  if (length(.const_drv)) for (.c in sort(.const_drv)) cat("    ", .c, "\n", sep = "")
  if (length(.const_src))
    cat(sprintf(paste0("    (%d source column(s) are constant within their ",
                       "one wave, several\n     at the HILDA missing code ",
                       "because a raw remnant never passes\n     through ",
                       "safe_numeric. Not listed: none of them is read.)\n"),
                length(.const_src)))
  # v21.9: split the derived constants the same way the all-NA list is
  # split. A provenance flag that cannot vary is a passed test, not a
  # defect, and counting it as one is what made the v21.8 warning name
  # four columns none of which needed looking at.
  .const_expected   <- .const_drv[.const_nm[!.is_src(.const_nm)] %in%
                                    EXPECTED_CONSTANT_COLS]
  .const_unexpected <- .const_drv[!(.const_nm[!.is_src(.const_nm)] %in%
                                      EXPECTED_CONSTANT_COLS)]
  if (length(.const_expected))
    cat("\n  Of those, constant BY CONSTRUCTION (a passed test, not a\n",
        "  defect): ", paste(sort(sub(" =.*$", "", .const_expected)),
                             collapse = ", "), "\n", sep = "")
  # v21.1: separate the columns that are empty BY CONSTRUCTION from the
  # ones that are empty because something did not resolve. Under v21 the
  # warning counted firm_size_comparable among ten all-NA columns, which
  # is a designed emptiness reported as a defect.
  .empty_expected   <- intersect(.empty_drv, EXPECTED_EMPTY_COLS)
  .empty_unexpected <- setdiff(.empty_drv, EXPECTED_EMPTY_COLS)
  if (length(.empty_expected))
    cat("\n  Of those, empty BY CONSTRUCTION (not a defect): ",
        paste(sort(.empty_expected), collapse = ", "), "\n", sep = "")
  if (length(.empty_unexpected) || length(.const_unexpected)) {
    cat("\n  Cross-reference each against the absence audit above. A\n")
    cat("  column that is all-NA because its mnemonic is absent from\n")
    cat("  the release is a MISSING VARIABLE, not a zero, and anything\n")
    cat("  reading it is reading nothing.\n")
    warning(BUILD_TAG, ": ", length(.empty_unexpected),
            " unexpected all-NA and ", length(.const_unexpected),
            " unexpected constant DERIVED column(s) on the panel. See the ",
            "sweep above. Wave-prefixed source columns and the ",
            "constant-by-construction flags are counted separately and are ",
            "not part of this figure.",
            call. = FALSE)
  } else {
    cat("\n  No unexplained all-NA or constant DERIVED column on the panel.\n")
  }
  # Targeted re-check of the indicators most likely to be read as
  # incidence rates, printed separately so they are not lost in the
  # sweep.
  .watch <- intersect(DEGENERATE_CHECK_COLS, names(combined_data))
  .hit <- .watch[.watch %in% .empty |
                   vapply(.watch, function(z)
                     any(startsWith(.const, paste0(z, " "))), logical(1))]
  if (length(.hit))
    cat("\n  Of the watched indicators, degenerate: ",
        paste(.hit, collapse = ", "), "\n", sep = "")
  
  # ---- v21.3 WAVE-HOLE SWEEP --------------------------------------
  # The third kind of empty. A column with no observations in some waves
  # and observations in others passes the all-NA sweep above and the
  # absence audit's absent-from-every-wave list, then arrives in the
  # panel indistinguishable from a full series. Four columns on the v21.2
  # panel are three-wave items for this reason.
  #
  # rowsum() rather than a split or a per-wave subset: it is C-level, it
  # runs once per column over the whole frame, and on 2,835 columns it
  # costs seconds rather than the minutes a repeated frame subset would.
  # v21.7: the matrix is built once, above, and shared with the
  # degenerate sweep rather than rebuilt here.
  cat("\n--- v21.3 wave holes: empty in some waves, populated in others ---\n")
  .n_zero <- colSums(.nm_w == 0L)
  .hole   <- names(.n_zero)[.n_zero > 0L & .n_zero < length(.wv_lev)]
  .hole_src <- .hole[.is_src(.hole)]
  .hole_drv <- .hole[!.is_src(.hole)]
  cat(sprintf("  Columns scanned: %d over %d waves. With holes: %d\n",
              ncol(combined_data), length(.wv_lev), length(.hole)))
  cat(sprintf(paste0("  Of those, %d are wave-prefixed source columns, ",
                     "single-wave BY\n  CONSTRUCTION and dropped at the ",
                     "final select. Excluded below, so\n  what remains is ",
                     "%d derived and carried columns.\n"),
              length(.hole_src), length(.hole_drv)))
  .hole <- .hole_drv
  if (length(.hole)) {
    .shape_of <- function(cl) {
      pres <- .wv_lev[.nm_w[, cl] > 0L]
      hit <- names(KNOWN_WAVE_SHAPES)[vapply(KNOWN_WAVE_SHAPES,
                                             function(z) setequal(z, pres),
                                             logical(1))]
      if (length(hit)) hit[1] else NA_character_
    }
    .tab <- data.frame(
      column = .hole,
      waves_present = vapply(.hole, function(cl)
        paste(.wv_lev[.nm_w[, cl] > 0L], collapse = " "), character(1)),
      n_waves = vapply(.hole, function(cl) sum(.nm_w[, cl] > 0L), integer(1)),
      shape = vapply(.hole, .shape_of, character(1)),
      stringsAsFactors = FALSE, row.names = NULL)
    .known <- .tab[!is.na(.tab$shape), , drop = FALSE]
    .unk   <- .tab[is.na(.tab$shape), , drop = FALSE]
    .unk   <- .unk[order(.unk$n_waves, .unk$column), , drop = FALSE]
    if (nrow(.known)) {
      cat("  Matching a known module shape (a fact about the instrument):\n")
      print(as.data.frame(table(.known$shape)), row.names = FALSE)
    }
    cat(sprintf("  Not matching any known shape: %d%s\n", nrow(.unk),
                if (nrow(.unk) > WAVE_HOLE_PRINT_N)
                  sprintf(", showing the %d sparsest", WAVE_HOLE_PRINT_N)
                else ""))
    if (nrow(.unk))
      print(utils::head(.unk[, c("column", "n_waves", "waves_present")],
                        WAVE_HOLE_PRINT_N), row.names = FALSE, right = FALSE)
    cat("  A module shape here is not a defect; it is a variable that\n")
    cat("  cannot carry a thirteen-wave profile and must not be averaged\n")
    cat("  into one. Wealth carries wealth_module_wave for exactly this\n")
    cat("  reason. Anything in the unexplained list is either a mnemonic\n")
    cat("  that arrived partway through the window or a derived column\n")
    cat("  resting on one; check it against the absence audit above.\n")
    rm(.tab, .known, .unk, .shape_of)
  }
  rm(.wv_lev, .grp, .nm_w, .n_zero, .hole, .hole_src, .hole_drv)
  rm(.obs_n, .all_nm, .wv_ltr, .first, .stem, .is_wl, .stem_ct,
     .has_sib, .only_ltr, .src_nm, .is_src, .is_const)
  
  # ---- v19 FIRM-SIZE INSTRUMENT BREAK ------------------------------
  # jbmemsz through wave 21, jbmems2 from wave 22, different code
  # frames. Print coverage and composition either side so the size of
  # the discontinuity is on the record rather than discovered in an
  # event study.
  if (all(c("firm_size", "firm_size_instrument") %in% names(combined_data))) {
    cat("\n--- v19 firm-size instrument break ---\n")
    .fs <- combined_data[combined_data$employed == 1 &
                           !is.na(combined_data$employed), ]
    .fstab <- .fs %>%
      group_by(wave, firm_size_instrument) %>%
      summarise(n_employed = n(),
                pct_classified = round(100 * mean(!is.na(firm_size)), 1),
                pct_small  = round(100 * mean(firm_size == "Small",
                                              na.rm = TRUE), 1),
                pct_medium = round(100 * mean(firm_size == "Medium",
                                              na.rm = TRUE), 1),
                pct_large  = round(100 * mean(firm_size == "Large",
                                              na.rm = TRUE), 1),
                .groups = "drop")
    print(as.data.frame(.fstab))
    .pre  <- .fstab$pct_small[.fstab$wave == FIRM_SIZE_BREAK_WAVE - 1L]
    .post <- .fstab$pct_small[.fstab$wave == FIRM_SIZE_BREAK_WAVE]
    if (length(.pre) && length(.post)) {
      .shift <- abs(.post - .pre) / 100
      cat(sprintf("  Small-firm share %.1f%% at wave %d against %.1f%% at wave %d.\n",
                  .pre, FIRM_SIZE_BREAK_WAVE - 1L, .post, FIRM_SIZE_BREAK_WAVE))
      if (.shift > FIRM_SIZE_SHARE_TOL) {
        cat("  The two code frames ARE reconcilable (see the crosswalk at\n")
        cat("  FIRM_SIZE_BREAK_WAVE); the category assignments agree. What\n")
        cat("  does not agree is COVERAGE: roughly 60% of the employed are\n")
        cat("  classified before the break against 99% after. The old item\n")
        cat("  is a filtered question over-sampling large employers, so no\n")
        cat("  recode makes the series comparable. firm_size is descriptive\n")
        cat("  within a regime only. Use firm_size_comparable, whose six\n")
        cat("  levels carry the regime, or firm_size_post22, which is one\n")
        cat("  regime and three levels. Neither repairs the coverage break;\n")
        cat("  they make it impossible to cross it without noticing.\n")
        warning(BUILD_TAG, ": firm-size composition shifts by ",
                sprintf("%.1f", 100 * .shift), " points at the wave ",
                FIRM_SIZE_BREAK_WAVE, " instrument change, and coverage ",
                "shifts from ~60% to ~99% of the employed. Do not use ",
                "firm_size across the break.", call. = FALSE)
      }
    }
    rm(.fs, .fstab)
  }
  
  # ---- v19 EDUCATION COMPOSITION BY WAVE ---------------------------
  # Wave 24 moves mean years by 0.13 against a typical 0.05, lifts
  # Bachelor_Honours from 34 to 108 and Doctorate from 9 to 30, and
  # triples edcqen coverage, all at the same time as a sample refresh.
  # Print the series so the last point is never read as trend.
  cat("\n--- v19 education composition by wave ---\n")
  .ed <- combined_data %>%
    group_by(wave) %>%
    summarise(n = n(),
              mean_years = round(mean(educ_years, na.rm = TRUE), 3),
              pct_degree   = round(100 * mean(educ_years >= 16, na.rm = TRUE), 1),
              pct_postgrad = round(100 * mean(educ_years >= 17, na.rm = TRUE), 1),
              pct_enrolled_obs = if ("enrolled_in_course" %in% names(combined_data))
                round(100 * mean(!is.na(enrolled_in_course)), 1) else NA_real_,
              # v21.1: coverage AND the share equal to one, for both
              # items. The v21 table printed coverage only, and edfts is
              # observed for everyone in every wave, so coverage cannot
              # show whether that item breaks. Its level can.
              pct_enrolled_eq1 = if ("enrolled_in_course" %in% names(combined_data))
                round(100 * mean(enrolled_in_course == 1, na.rm = TRUE), 1)
              else NA_real_,
              pct_fts_obs = if ("full_time_student" %in% names(combined_data))
                round(100 * mean(!is.na(full_time_student)), 1) else NA_real_,
              pct_fts_eq1 = if ("full_time_student" %in% names(combined_data))
                round(100 * mean(full_time_student == 1, na.rm = TRUE), 1)
              else NA_real_,
              .groups = "drop") %>%
    mutate(d_mean_years = round(mean_years - lag(mean_years), 3))
  print(as.data.frame(.ed))
  cat("  Wave ", STUDY_ITEM_BREAK_WAVE, " carries a sample REFRESHER, and the\n",
      "  education composition breaks there: mean years, degree share and n\n",
      "  all move by several times their usual increment.\n",
      "  On the study items the two do NOT behave alike, which v21 got\n",
      "  wrong by treating them as a pair. edfts is observed for ~100% of\n",
      "  person-waves in every wave, so its coverage cannot break and only\n",
      "  pct_fts_eq1 can tell you anything. edcqen coverage is the item\n",
      # Read off .ed rather than typed in, so the sentence states what
      # THIS run found. The v21.1 patch briefly hard-coded 3.6 and 12.3
      # from the W12-W24 log, which is a number in a comment pretending
      # to be a measurement.
      "  that jumps, from ",
      sprintf("%.1f%%", .ed$pct_enrolled_obs[.ed$wave == STUDY_ITEM_BREAK_WAVE - 1L]),
      " at wave ", STUDY_ITEM_BREAK_WAVE - 1L, " to ",
      sprintf("%.1f%%", .ed$pct_enrolled_obs[.ed$wave == STUDY_ITEM_BREAK_WAVE]),
      " at wave ", STUDY_ITEM_BREAK_WAVE, ".\n",
      "  Both *_comparable columns are still NA at wave ",
      STUDY_ITEM_BREAK_WAVE, ", on the refresher\n",
      "  alone; see the restriction block after combination.\n", sep = "")
  
  # ---- v21.1 COVERAGE OR COMPOSITION: the continuing sample --------
  # edcqen is observed for 2-4% of person-waves, so its non-missing share
  # behaves more like an enrolment rate than a coverage rate, and wave 24
  # adds roughly a thousand respondents. An instrument change and a
  # younger intake both predict the jump. Restricting to the persons
  # observed in BOTH waves holds the sample fixed: if the jump survives
  # that, it is the item; if it disappears, it was the refresher.
  .sw <- c(STUDY_ITEM_BREAK_WAVE - 1L, STUDY_ITEM_BREAK_WAVE)
  .st_cols <- intersect(c("enrolled_in_course", "full_time_student"),
                        names(combined_data))
  if (length(.st_cols) && all(.sw %in% combined_data$wave)) {
    .id_a <- combined_data$person_id[combined_data$wave == .sw[1]]
    .id_b <- combined_data$person_id[combined_data$wave == .sw[2]]
    .both <- intersect(.id_a, .id_b)
    cat(sprintf("\n  Continuing sample: %s persons observed at BOTH wave %d and %d\n",
                format(length(.both), big.mark = ","), .sw[1], .sw[2]))
    if (length(.both) >= 100L) {
      .cs <- combined_data[combined_data$wave %in% .sw &
                             combined_data$person_id %in% .both, ]
      .tab <- do.call(rbind, lapply(.sw, function(w) {
        z <- .cs[.cs$wave == w, ]
        out <- data.frame(wave = w, n = nrow(z))
        for (cl in .st_cols) {
          out[[paste0("pct_obs_", cl)]] <-
            round(100 * mean(!is.na(z[[cl]])), 1)
          # v21.3: mean() of an empty selection is NaN, and an empty
          # selection is precisely what this table exists to reveal, so
          # NaN would be the interesting cell printed as a mistake.
          out[[paste0("pct_eq1_", cl)]] <- if (any(!is.na(z[[cl]])))
            round(100 * mean(z[[cl]] == 1, na.rm = TRUE), 1) else NA_real_
        }
        out
      }))
      print(.tab, row.names = FALSE)
      .d_all  <- .ed$pct_enrolled_obs[.ed$wave == .sw[2]] -
        .ed$pct_enrolled_obs[.ed$wave == .sw[1]]
      .d_cont <- .tab$pct_obs_enrolled_in_course[.tab$wave == .sw[2]] -
        .tab$pct_obs_enrolled_in_course[.tab$wave == .sw[1]]
      if (length(.d_all) == 1L && length(.d_cont) == 1L) {
        cat(sprintf("  edcqen coverage change: %+.1f points overall, %+.1f in the continuing sample.\n",
                    .d_all, .d_cont))
        cat("  Similar in both: the ITEM changed, and the caveat is an\n")
        cat("  instrument break. Much smaller in the continuing sample:\n")
        cat("  the SAMPLE changed, and the caveat is a refresher. Either\n")
        cat("  way wave ", STUDY_ITEM_BREAK_WAVE,
            " stays off the series; the wording differs.\n", sep = "")
      }
      rm(.cs, .tab)
    } else {
      cat("  Too few to compare; the two readings cannot be separated here.\n")
    }
    rm(.id_a, .id_b, .both)
  }
  rm(.sw, .st_cols)
  # v21.49a. THIS WARNING STOPPED PRINTING AT v21.49, AND THE BREAK DID
  # NOT GO AWAY. educ_years became a running maximum, which steepens the
  # trend: the median increment rose from 0.051 to 0.061 years, so the
  # bar rose from 0.153 to 0.183, and wave 24's jump of 0.156 (0.160
  # before) no longer clears it. The flag the panel carries does not
  # depend on this test: EDUC_COMPOSITION_BREAK_WAVES keeps
  # educ_trend_comparable at 0 for wave 24, and the refresher note above
  # prints regardless. The running maximum also makes the step from wave
  # 12 to 13 the largest increment (0.091), because wave 12 has no
  # earlier wave to carry a level from.
  .typ <- stats::median(abs(.ed$d_mean_years), na.rm = TRUE)
  if (!is.na(.typ) && .typ > 0) {
    .big <- .ed$wave[which(abs(.ed$d_mean_years) > 3 * .typ)]
    if (length(.big))
      cat("  Wave-on-wave jump in mean education years exceeds three times\n",
          "  the typical increment at wave(s): ", paste(.big, collapse = ", "),
          ".\n  Treat those waves as a composition break, not a trend.\n",
          sep = "")
  }
  rm(.ed)
  
  # ---- Quarter DIRECTION: which flag is nearest the interview? -----
  # Also determined, not assumed. A birth in the quarter closest to the
  # interview means the pregnancy occupied most of the preceding twelve
  # months, so the pregnancy item should fire in the SAME wave. A birth
  # in the furthest quarter means the pregnancy sat mostly in the prior
  # window. So the quarter whose births show the highest same-wave
  # pregnancy rate is the one nearest the interview.
  cat("\n--- v19 quarter direction diagnostic ---\n")
  Q_DIRECTION <- NA_character_
  if (all(c("birth_quarter", "had_pregnancy_this_wave") %in%
          names(combined_data))) {
    .qd <- combined_data[!is.na(combined_data$birth_quarter), ]
    if (nrow(.qd) >= 4 * Q_MIN_EVENTS) {
      .rate <- tapply(.qd$had_pregnancy_this_wave, .qd$birth_quarter,
                      function(z) mean(z, na.rm = TRUE))
      .n    <- tapply(.qd$had_pregnancy_this_wave, .qd$birth_quarter, length)
      for (q in names(.rate))
        cat(sprintf("    quarter %s: n=%5d  same-wave pregnancy rate %.3f\n",
                    q, .n[[q]], .rate[[q]]))
      if (all(c("1", "4") %in% names(.rate)) &&
          !is.na(.rate[["1"]]) && !is.na(.rate[["4"]])) {
        .gap <- .rate[["1"]] - .rate[["4"]]
        if (abs(.gap) < 0.05) {
          cat("    q1 and q4 differ by", sprintf("%.3f", .gap),
              "which is too little to call. Q_DIRECTION left NA;\n",
              "    the post-birth window share will be NA until it is set by hand.\n")
        } else {
          Q_DIRECTION <- if (.gap > 0) "q1_nearest" else "q4_nearest"
          cat("    determined: ", Q_DIRECTION,
              sprintf(" (q1 minus q4 = %+.3f)\n", .gap), sep = "")
        }
      }
    } else {
      cat("    too few dated births to determine direction.\n")
    }
  }
  
  # ---- Post-birth share of the twelve-month reference window -------
  # The training items ask about the twelve months before the same
  # interview the quarter flags are measured against. So for a treated
  # person-wave the share of that window falling after the birth is
  # observed, and the straddling account becomes testable rather than
  # bounded: it predicts the birth-wave coefficient scales with this
  # share, and so predicts nothing where the share is near one.
  combined_data$post_birth_window_share <- NA_real_
  if (!is.na(Q_DIRECTION) &&
      "birth_quarter_event" %in% names(combined_data)) {
    # v20: birth_quarter_event, NOT birth_quarter. The raw column dates
    # four person-waves that never reported a birth, and under v19 each
    # of them received a post-birth window share, i.e. a treated event
    # time attached to an untreated person-wave.
    .q <- combined_data$birth_quarter_event
    .k <- if (identical(Q_DIRECTION, "q1_nearest")) .q else (5L - .q)
    # k = 1 is the quarter nearest the interview. Using quarter
    # midpoints, a birth in quarter k leaves (k - 0.5)/4 of the window
    # after it.
    combined_data$post_birth_window_share <- (.k - 0.5) / 4
    cat("  post_birth_window_share built under", Q_DIRECTION, ":",
        sum(!is.na(combined_data$post_birth_window_share)),
        "person-waves dated (reported births only).\n")
  } else {
    cat("  post_birth_window_share left NA: quarter direction undetermined.\n")
  }
  
  # ---- Deprecation report ------------------------------------------
  cat("\n===========================================\n")
  cat("v19 DEPRECATIONS: HARD RENAME, NO ALIAS\n")
  cat("===========================================\n")
  for (i in seq_along(DEPRECATED_V17))
    cat(sprintf("  %-20s REMOVED -> %s\n",
                names(DEPRECATED_V17)[i], DEPRECATED_V17[i]))
  cat("\n  event_birth was leprg (pregnancy), never lebth (birth). Any\n")
  cat("  script reading the old names is reading a pregnancy clock.\n")
  cat("  Call sites are NOT listed here: see the live deprecation scan\n")
  cat("  at the end of this script, which greps the tree at run time.\n")
  cat("\n  Cross-tabulation of the two events (rows pregnancy, cols birth):\n")
  if (all(c("had_pregnancy_this_wave", "had_child_this_wave") %in%
          names(combined_data)))
    print(table(pregnancy = combined_data$had_pregnancy_this_wave,
                birth     = combined_data$had_child_this_wave,
                useNA = "ifany"))
  
  # ================================================================
  # v18 CORRECTIONS AND DIAGNOSTICS
  # ================================================================
  # Four columns v17 got wrong are rebuilt here, and two questions the
  # v17 log raised are answered. Each block prints the evidence it
  # decided on, so a wrong call is visible in the log rather than
  # buried in a column name.
  
  # ---- 1. Quarter-declined: acceptance check ----------------------
  # v19: the v18 print asserted that the 345 undated reports were the
  # denominator for the 342 with lebthna set. They are not nested: the
  # flag was conditioned on the event report only, so it counted the
  # declined box wherever it was ticked, including alongside a quarter.
  # The identity is now stated over four MUTUALLY EXCLUSIVE cells and
  # the double-coded cases are counted separately rather than absorbed.
  .q_recon <- function(dat, label, reported, dated, multi, undated,
                       declined, both) {
    # v19: every cell is computed OVER REPORTERS ONLY. Computing dated
    # and multi over all rows would leave the identity dependent on no
    # non-reporter ever carrying a quarter flag, which is true in this
    # release but is not guaranteed, and the stop below would then fire
    # an hour into a rebuild on a benign edge case. Restricted this way
    # the four cells partition the reports by n_set (0, 1, >1) and the
    # residual is zero by construction, so a non-zero residual is a real
    # derivation fault and worth halting on.
    .rep  <- which(dat[[reported]] == 1)
    .nb   <- length(.rep)
    .ndat <- sum(!is.na(dat[[dated]][.rep]))
    .nmul <- sum(dat[[multi]][.rep]    == 1, na.rm = TRUE)
    .nund <- sum(dat[[undated]][.rep]  == 1, na.rm = TRUE)
    .ndec <- sum(dat[[declined]][.rep] == 1, na.rm = TRUE)
    .nbth <- sum(dat[[both]][.rep]     == 1, na.rm = TRUE)
    # Flags on non-reporters: not part of the identity, but if this is
    # ever non-zero the quarter items are being read outside the filter.
    .nonrep <- sum(!is.na(dat[[dated]])) - .ndat
    cat(sprintf("\n  %s\n", label))
    cat(sprintf("    reported %d = dated %d + multiple flags %d + undated %d\n",
                .nb, .ndat, .nmul, .nund))
    cat(sprintf("    identity residual %d (must be 0 by construction)\n",
                .nb - .ndat - .nmul - .nund))
    cat(sprintf("    NA box ticked %d, of which also carrying a quarter %d\n",
                .ndec, .nbth))
    cat(sprintf("    NA box ticked and no quarter %d, undated %d, gap %d\n",
                .ndec - .nbth, .nund, .nund - (.ndec - .nbth)))
    cat(sprintf("    quarter flags on NON-reporters %d (expect 0)\n", .nonrep))
    if (.nb - .ndat - .nmul - .nund != 0L)
      stop(BUILD_TAG, ": the four quarter cells do not exhaust the reports ",
           "for ", label, ". The flags are not mutually exclusive as derived.",
           call. = FALSE)
    # v21: the non-reporter count IS the orphan count, and the orphans
    # are now understood: they are dated person-waves that never reported
    # the event, they are split into contradictions and blank-event
    # reports a few blocks below, and they are excluded from
    # *_quarter_event and from post_birth_window_share either way. At
    # four per item that is a note. Left as a warning it sat in the same
    # end-of-run list as the firm-size break, which is a decision, and
    # competed with it for attention.
    if (.nonrep != 0L && .nonrep <= ORPHAN_QUARTER_MAX)
      cat(sprintf(paste0("    NOTE: %d orphan quarter flag(s) on ",
                         "non-reporters of %s. Known,\n",
                         "          benign at this scale, and excluded from ",
                         "*_quarter_event and\n",
                         "          from post_birth_window_share. Split below.\n"),
                  .nonrep, label))
    if (.nonrep > ORPHAN_QUARTER_MAX)
      warning(BUILD_TAG, ": ", .nonrep, " quarter flag(s) set on person-waves ",
              "that did not report ", label, ", against ", ORPHAN_QUARTER_MAX,
              " tolerated. The quarter items are being read outside the ",
              "event filter.", call. = FALSE)
    if (.nund - (.ndec - .nbth) != 0L)
      cat("    NOTE: some undated reports do NOT tick the NA box. Those\n",
          "         are item non-response rather than a refusal to date;\n",
          "         both are undated and both are handled by *_undated.\n",
          sep = "")
    invisible(c(reported = .nb, dated = .ndat, multi = .nmul,
                undated = .nund, declined = .ndec, both = .nbth))
  }
  cat("\n--- v19 quarter reconciliation (mutually exclusive cells) ---\n")
  cat("  Two distinct objects, both retained:\n")
  cat("    *_q_declined  the respondent ticked the cannot-say box\n")
  cat("    *_undated     the report carries no quarter at all\n")
  cat("  Event dating must use *_undated. Under v18 the first was used\n")
  cat("  for both and overstated the undated group by the four\n")
  cat("  double-coded person-waves (2015, 2021, 2023, 2024).\n")
  .q_recon(combined_data, "births (lebth)",
           "had_child_this_wave", "birth_quarter", "birth_quarter_multi",
           "birth_undated", "birth_q_declined", "birth_q_declined_dated")
  .q_recon(combined_data, "pregnancies (leprg)",
           "had_pregnancy_this_wave", "preg_quarter", "preg_quarter_multi",
           "preg_undated", "preg_q_declined", "preg_q_declined_dated")
  
  # Cross-tabulation of the NA box against the number of quarter flags,
  # restricted to reporters. This is the table that decides what the
  # flag means; the summary counts above are derived from it.
  cat("\n  lebthna against number of quarter flags, birth reporters only:\n")
  .xt <- combined_data[which(combined_data$had_child_this_wave == 1), ]
  print(table(NA_box   = ifelse(is.na(.xt$birth_q_declined), 0L,
                                .xt$birth_q_declined),
              n_quarter_flags = pmin(.xt$birth_q_n_set, 2L)))
  cat("  Column 0 with NA_box 1 is the well-formed declined group.\n")
  cat("  Columns 1 and 2 with NA_box 1 are the double-coded cases.\n")
  rm(.xt)
  
  # ---- v20 ORPHAN QUARTERS ----------------------------------------
  # A quarter flag on a person-wave that did not report the event. Four
  # of these in W12-W24, on both items. They are the entire reason the
  # v18 reconciliation appeared not to close, and under v19 each was
  # still receiving a post-birth window share. The split below decides
  # what they are: an explicit "no" alongside a dated quarter is a
  # contradiction and stays excluded, whereas a BLANK event box with a
  # dated quarter is a birth report with item non-response on the event
  # item and could be recovered by treating the quarter as the report.
  cat("\n--- v20 orphan quarters (dated, event not reported) ---\n")
  for (.fam in list(c("births", "birth_quarter_orphan", "event_child",
                      "had_child_this_wave"),
                    c("pregnancies", "preg_quarter_orphan", "event_pregnancy",
                      "had_pregnancy_this_wave"))) {
    if (!all(.fam[2:4] %in% names(combined_data))) next
    .o <- combined_data[which(combined_data[[.fam[2]]] == 1L), ]
    cat(sprintf("  %-12s orphan quarters: %d\n", .fam[1], nrow(.o)))
    if (nrow(.o)) {
      .blank <- sum(is.na(.o[[.fam[4]]]))
      cat(sprintf("    event box BLANK (recoverable as a report): %d\n", .blank))
      cat(sprintf("    event answered NO  (contradiction, excluded): %d\n",
                  nrow(.o) - .blank))
      cat("    waves: ", paste(sort(unique(.o$wave)), collapse = ", "), "\n",
          sep = "")
    }
  }
  cat("  Excluded from *_quarter_event and from post_birth_window_share\n")
  cat("  either way. If the blank count is the larger, consider treating\n")
  cat("  a dated quarter as its own report and rebuilding.\n")
  
  # ---- 2. Which time-use item is own-child care -------------------
  # Own-child care should be near zero for people who never had a
  # child and high where a child under five is present. Care for a
  # disabled or elderly relative should do neither.
  cat("\n--- v19 care-item discriminator ---\n")
  .tu <- intersect(paste0("hours_", TIME_USE_STEMS), names(combined_data))
  care_child_col <- NA_character_
  .stat <- NULL
  if (length(.tu)) {
    .b <- combined_data[!is.na(combined_data$ever_parent), ]
    .stat <- do.call(rbind, lapply(.tu, function(cl) {
      x <- .b[[cl]]
      m0 <- mean(x[.b$ever_parent == 0], na.rm = TRUE)
      m1 <- mean(x[.b$ever_parent == 1 & .b$num_children_0to4 > 0],
                 na.rm = TRUE)
      m2 <- mean(x[.b$ever_parent == 1 & .b$num_children_0to4 == 0],
                 na.rm = TRUE)
      data.frame(column = cl,
                 never_parent = round(m0, 2),
                 parent_child_under5 = round(m1, 2),
                 parent_no_child_under5 = round(m2, 2),
                 ratio = round(m1 / max(m0, 1e-8), 2),
                 stringsAsFactors = FALSE)
    }))
    .stat$ratio[!is.finite(.stat$ratio)] <- 0
    print(.stat, row.names = FALSE)
    .o <- order(-.stat$ratio)
    .win <- .stat[.o[1], ]
    .oth <- if (nrow(.stat) > 1) .stat$ratio[.o[2]] else 0
    if (isTRUE(.win$ratio >= CARE_RATIO_MIN) &&
        isTRUE(.win$ratio >= CARE_MARGIN_MIN * max(.oth, 1e-8))) {
      care_child_col <- .win$column
      cat("  determined: ", care_child_col,
          " is own-child care (ratio ", sprintf("%.2f", .win$ratio),
          " against ", sprintf("%.2f", .oth), ")\n", sep = "")
    } else {
      cat("  NOT determined: neither item separates on parenthood by the\n")
      cat("  required margin. Both stay under their mnemonic and nothing\n")
      cat("  is labelled child care.\n")
    }
  }
  combined_data$care_own_children_hours <-
    if (!is.na(care_child_col)) combined_data[[care_child_col]] else NA_real_
  .oth_col <- if (!is.na(care_child_col)) setdiff(.tu, care_child_col) else character(0)
  combined_data$care_other_hours <-
    if (length(.oth_col)) combined_data[[.oth_col[1]]] else NA_real_
  
  # ---- 3. Total unpaid hours: scoped and clamped ------------------
  # Housework plus the two mapped time-use items. Commute and
  # volunteering are excluded deliberately: neither is household
  # production. Components clamped to the week; v17 summed unclamped
  # values while the neighbouring care_intensity clamps at 100.
  cat("\n--- v19 total unpaid hours ---\n")
  # v19: the clamp was applied to each COMPONENT and not to the SUM, so
  # 80 hours of housework plus 100 of childcare passed every guard and
  # totalled 180. 37 person-waves exceeded the 168-hour week on the
  # W12-W24 run. The sum is now clamped too and the affected rows are
  # flagged rather than quietly rescaled, so they can be dropped or
  # inspected instead of entering an estimate as a plausible 168.
  .clamp <- function(x) ifelse(is.na(x), NA_real_, pmin(pmax(x, 0), 168))
  .cmp <- cbind(.clamp(combined_data$housework_hours),
                .clamp(combined_data$care_own_children_hours),
                .clamp(combined_data$care_other_hours))
  .anyc <- rowSums(!is.na(.cmp)) > 0
  .raw_total <- ifelse(.anyc, rowSums(.cmp, na.rm = TRUE), NA_real_)
  combined_data$total_unpaid_hours_raw    <- .raw_total
  combined_data$total_unpaid_hours_capped <-
    as.integer(!is.na(.raw_total) & .raw_total > 168)
  combined_data$total_unpaid_hours <- .clamp(.raw_total)
  cat(sprintf("  components: housework%s%s\n",
              if (!is.na(care_child_col)) ", own-child care" else "",
              if (length(.oth_col)) ", other care" else ""))
  if (any(.anyc)) {
    cat(sprintf("  observed %s person-waves, median %.1f, p99 %.1f\n",
                format(sum(.anyc), big.mark = ","),
                stats::median(combined_data$total_unpaid_hours, na.rm = TRUE),
                stats::quantile(combined_data$total_unpaid_hours, 0.99,
                                na.rm = TRUE)))
    cat(sprintf("  uncapped total exceeded the 168h week in %d person-waves; %d after the cap\n",
                sum(combined_data$total_unpaid_hours_capped == 1, na.rm = TRUE),
                sum(combined_data$total_unpaid_hours > 168, na.rm = TRUE)))
    cat("  Those rows are flagged by total_unpaid_hours_capped and the\n")
    cat("  pre-cap value is kept in total_unpaid_hours_raw. Drop them or\n")
    cat("  inspect them; do not read a capped 168 as a measurement.\n")
    if (sum(combined_data$total_unpaid_hours_capped == 1, na.rm = TRUE) >
        0.001 * sum(.anyc))
      warning(BUILD_TAG, ": more than 0.1% of unpaid-hours totals exceed a week ",
              "before capping; check the component units.", call. = FALSE)
  } else {
    cat("  no component observed; total_unpaid_hours is NA throughout.\n")
  }
  
  # ---- 4. Net worth from its two components -----------------------
  cat("\n--- v19 net-worth components ---\n")
  combined_data$hh_net_worth <- NA_real_
  if (all(c("hh_net_worth_pos", "hh_net_worth_neg") %in% names(combined_data))) {
    .m <- !is.na(combined_data$hh_net_worth_pos) |
      !is.na(combined_data$hh_net_worth_neg)
    .both <- sum(combined_data$hh_net_worth_pos > 0 &
                   combined_data$hh_net_worth_neg > 0, na.rm = TRUE)
    cat(sprintf("  module person-waves %s, both components strictly positive %s\n",
                format(sum(.m), big.mark = ","),
                format(.both, big.mark = ",")))
    combined_data$hh_net_worth <- ifelse(
      .m,
      dplyr::coalesce(combined_data$hh_net_worth_pos, 0) -
        dplyr::coalesce(combined_data$hh_net_worth_neg, 0),
      NA_real_)
    if (any(.m)) {
      .q <- stats::quantile(combined_data$hh_net_worth[.m],
                            c(0.1, 0.5, 0.9), na.rm = TRUE)
      cat(sprintf("  net worth p10 %s, p50 %s, p90 %s; negative %.1f%%\n",
                  format(round(.q[1]), big.mark = ","),
                  format(round(.q[2]), big.mark = ","),
                  format(round(.q[3]), big.mark = ","),
                  100 * mean(combined_data$hh_net_worth[.m] < 0, na.rm = TRUE)))
    }
    cat("  A negative share of exactly zero would mean the two are not a\n")
    cat("  positive/negative split and the difference is the wrong object.\n")
  }
  
  # ---- 5. nlreapm: which code is the maternity reason -------------
  cat("\n--- v19 nlreapm coding ---\n")
  combined_data$nlf_reason_maternity <- NA_integer_
  if ("nlf_reason_code" %in% names(combined_data)) {
    .t <- table(code = combined_data$nlf_reason_code,
                female = combined_data$female)
    if (nrow(.t) && all(c("0", "1") %in% colnames(.t))) {
      .codes <- rownames(.t)
      .n     <- rowSums(.t)
      .fem   <- .t[, "1"] / pmax(.n, 1)
      for (i in seq_along(.codes))
        cat(sprintf("    code %-6s n = %6d   female share %.3f\n",
                    .codes[i], .n[i], .fem[i]))
      .ok <- which(.n >= NLF_MIN_N & .fem >= NLF_FEMALE_MIN)
      if (length(.ok) == 1L) {
        .c <- as.numeric(.codes[.ok])
        combined_data$nlf_reason_maternity <-
          as.integer(combined_data$nlf_reason_code == .c)
        cat("  determined: code ", .c,
            " is the pregnancy/maternity reason (",
            sum(combined_data$nlf_reason_maternity == 1, na.rm = TRUE),
            " person-waves)\n", sep = "")
      } else {
        cat("  NOT determined: no single code is at least ",
            round(100 * NLF_FEMALE_MIN), "% female with n >= ", NLF_MIN_N,
            ". nlf_reason_maternity left NA; use nlf_reason_code.\n", sep = "")
      }
    } else {
      cat("  nlf_reason_code carries no usable values this build.\n")
    }
  }
  
  # ---- 6. Is superannuation individual or a household aggregate? --
  # This decides whether the wealth line is open at all. A household
  # total cannot answer a question about her terminal wealth.
  cat("\n--- v19 superannuation: individual or household? ---\n")
  if (all(c("super_balance", "partner_id", "person_id", "wave") %in%
          names(combined_data))) {
    .s <- combined_data %>%
      dplyr::filter(!is.na(super_balance), !is.na(partner_id),
                    partner_id != "") %>%
      dplyr::select(person_id, partner_id, wave, super_balance)
    if (nrow(.s) > 0) {
      .p <- .s %>%
        dplyr::select(person_id, wave, partner_super = super_balance)
      .j <- .s %>%
        dplyr::inner_join(.p, by = c("partner_id" = "person_id",
                                     "wave" = "wave"))
      if (nrow(.j) > 0) {
        .same <- mean(abs(.j$super_balance - .j$partner_super) < 1e-6)
        # v21: recorded rather than only printed, so the restriction
        # block downstream states the determination instead of asserting
        # it a second time from memory.
        .v21_facts$super_partner_identical <- .same
        .v21_facts$super_couple_waves      <- nrow(.j)
        cat(sprintf("  couple-waves with both partners observed: %s\n",
                    format(nrow(.j), big.mark = ",")))
        cat(sprintf("  partners hold an identical value: %.1f percent\n",
                    100 * .same))
        cat("  Near 100 means hwsupei is a HOUSEHOLD aggregate and cannot\n")
        cat("  carry an individual terminal-value comparison. Well below\n")
        cat("  means it is individual and the wealth line is open.\n")
      } else {
        cat("  no couple-waves with both partners in a module wave.\n")
      }
    } else {
      cat("  no observed super_balance with a partner link.\n")
    }
  }
  
  # ---- 7. Births with no same-wave pregnancy report, by sex -------
  # If that cell is overwhelmingly male, leprg is in practice a female
  # item and T7 cannot report a fathers' own coefficient off it.
  cat("\n--- v19 birth reports with no same-wave pregnancy report ---\n")
  .bb <- combined_data[!is.na(combined_data$had_child_this_wave) &
                         combined_data$had_child_this_wave == 1, ]
  print(table(pregnancy = .bb$had_pregnancy_this_wave,
              female = .bb$female, useNA = "ifany"))
  .fp <- mean(combined_data$female[
    !is.na(combined_data$had_pregnancy_this_wave) &
      combined_data$had_pregnancy_this_wave == 1], na.rm = TRUE)
  cat(sprintf("  female share of all pregnancy reports: %.3f\n", .fp))
  cat("  A share near 0.5 means fathers report the partner's pregnancy\n")
  cat("  and the anticipation design can carry a men's own coefficient.\n")
  
  
  cat("  ever_parent == 0:", sum(combined_data$ever_parent == 0, na.rm = TRUE), "\n")
  cat("  Mothers ever:", sum(combined_data$ever_mother == 1, na.rm = TRUE), "\n")
  cat("  Never-mothers:", sum(combined_data$ever_mother == 0, na.rm = TRUE), "\n")
  cat("  Post-mothers:", sum(combined_data$motherhood_status == "post_mother", na.rm = TRUE), "\n")
  gc()
  
  # Add final variables
  combined_data <- combined_data %>%
    mutate(
      # Period indicators
      period = case_when(
        year %in% 2015:2019 ~ "Pre_COVID",
        year %in% 2020:2021 ~ "COVID",
        year %in% 2022:2024 ~ "Post_COVID"
      ),
      
      # Interaction terms (original)
      educ_female = educ_years * female,
      educ_mother = educ_years * is_mother,
      educ_treatment = educ_years * treatment,
      educ_precarious = educ_years * (precarious_score >= 2),
      
      # NEW: Interaction terms with absorbing motherhood
      educ_ever_mother = educ_years * coalesce(ever_mother, 0L),
      educ_ever_father = educ_years * coalesce(ever_father, 0L),
      
      # Centered education
      educ_centered = educ_years - mean(educ_years, na.rm = TRUE),
      educ_centered_sq = educ_centered^2,
      
      # ================================================================
      # GROUP CLASSIFICATIONS
      # ================================================================
      
      # PRIMARY (new): Ever-parent based — absorbing, no back-switching
      group_ever = case_when(
        female == 0 & ever_father == 1  ~ "fathers_ever",
        female == 0 & ever_father == 0  ~ "childless_men_ever",
        female == 1 & ever_mother == 1  ~ "mothers_ever",
        female == 1 & ever_mother == 0  ~ "never_mothers",
        TRUE ~ NA_character_
      ),
      
      # SENSITIVITY (current): Original children-under-15 based — time-varying
      group_current = case_when(
        female == 0 & num_children_under15 > 0 ~ "fathers",
        female == 0 & num_children_under15 == 0 ~ "childless_men",
        female == 1 & num_children_under15 > 0 ~ "mothers",
        female == 1 & num_children_under15 == 0 ~ "childless_women",
        TRUE ~ NA_character_
      ),
      
      # THREE-WAY WOMEN SPLIT (for decomposing contamination)
      group_3way_women = case_when(
        female == 0 ~ NA_character_,
        motherhood_status == "never_mother"  ~ "never_mothers",
        motherhood_status == "active_mother" ~ "active_mothers",
        motherhood_status == "post_mother"   ~ "post_mothers",
        TRUE ~ NA_character_
      ),

      # LIFETIME COUNTERPARTS. Built from the _life statuses defined after
      # the cummax fix, so the never- categories are women and men who never
      # become parents at all rather than everyone not yet a parent at that
      # wave. See the note there for which to use where; the short version
      # is that anything indexed by event time wants the wave-varying pair
      # and anything cross-sectional wants these.
      #
      # THE CONTAMINATION THESE FIX IS THE MIRROR OF THE ONE group_3way_women
      # WAS BUILT FOR. That split exists to stop post-mothers being counted
      # as childless, which is contamination from the past. The wave-varying
      # never- category has the same problem running the other way, and the
      # two are the same size in reverse: a comparison group holding people
      # who are on the treated side of the event at some other wave.
      group_ever_life = case_when(
        female == 0 & ever_father_life == 1  ~ "fathers_ever",
        female == 0 & ever_father_life == 0  ~ "childless_men_ever",
        female == 1 & ever_mother_life == 1  ~ "mothers_ever",
        female == 1 & ever_mother_life == 0  ~ "never_mothers",
        TRUE ~ NA_character_
      ),

      # FOUR-WAY, because motherhood_status_life has a pre_ level and
      # dropping it here would put the contamination back. A consumer that
      # wants three collapses pre_mothers into whichever side its question
      # needs, and has to say which.
      group_4way_women_life = case_when(
        female == 0 ~ NA_character_,
        motherhood_status_life == "never_mother"  ~ "never_mothers",
        motherhood_status_life == "pre_mother"    ~ "pre_mothers",
        motherhood_status_life == "active_mother" ~ "active_mothers",
        motherhood_status_life == "post_mother"   ~ "post_mothers",
        TRUE ~ NA_character_
      )
    )
  
  # ============================
  # COMPREHENSIVE EDUCATION DIAGNOSTICS
  # ============================
  
  cat("\n############################################\n")
  cat("# COMPREHENSIVE EDUCATION DIAGNOSTICS      #\n")
  cat("############################################\n")
  
  # Overall education statistics
  cat("\n=== EDUCATION STATISTICS ===\n")
  educ_stats <- combined_data %>%
    summarise(
      N = n(),
      Mean = mean(educ_years, na.rm = TRUE),
      SD = sd(educ_years, na.rm = TRUE),
      Min = min(educ_years, na.rm = TRUE),
      Q1 = quantile(educ_years, 0.25, na.rm = TRUE),
      Median = median(educ_years, na.rm = TRUE),
      Q3 = quantile(educ_years, 0.75, na.rm = TRUE),
      Max = max(educ_years, na.rm = TRUE),
      Missing = sum(is.na(educ_years))
    )
  print(educ_stats)
  
  # Education level distribution
  cat("\n=== EDUCATION LEVEL DISTRIBUTION ===\n")
  educ_level_dist <- combined_data %>%
    count(education_level) %>%
    mutate(pct = round(100 * n / nrow(combined_data), 2)) %>%
    arrange(desc(n))
  print(educ_level_dist, n = 20)
  
  # Education pathway distribution
  cat("\n=== TOP 30 EDUCATION PATHWAYS ===\n")
  pathway_dist <- combined_data %>%
    count(education_pathway) %>%
    mutate(pct = round(100 * n / nrow(combined_data), 2)) %>%
    arrange(desc(n)) %>%
    head(30)
  print(pathway_dist)
  
  # Field-specific breakdown
  cat("\n=== FIELD-SPECIFIC EDUCATION ===\n")
  field_stats <- combined_data %>%
    summarise(
      Nursing_N = sum(has_nursing_any, na.rm = TRUE),
      Nursing_Pct = round(100 * mean(has_nursing_any, na.rm = TRUE), 2),
      Teaching_N = sum(has_teaching_any, na.rm = TRUE),
      Teaching_Pct = round(100 * mean(has_teaching_any, na.rm = TRUE), 2),
      Business_N = sum(has_business_any, na.rm = TRUE),
      Business_Pct = round(100 * mean(has_business_any, na.rm = TRUE), 2),
      Trade_N = sum(has_trade_any, na.rm = TRUE),
      Trade_Pct = round(100 * mean(has_trade_any, na.rm = TRUE), 2),
      Technical_N = sum(has_technical_any, na.rm = TRUE),
      Technical_Pct = round(100 * mean(has_technical_any, na.rm = TRUE), 2)
    )
  print(t(field_stats))
  
  # Education by gender
  cat("\n=== EDUCATION BY GENDER ===\n")
  gender_educ <- combined_data %>%
    group_by(female) %>%
    summarise(
      N = n(),
      Mean_Years = round(mean(educ_years, na.rm = TRUE), 2),
      Pct_Degree = round(100 * mean(has_degree, na.rm = TRUE), 1),
      Pct_Postgrad = round(100 * mean(has_postgrad, na.rm = TRUE), 1),
      Pct_Nursing = round(100 * mean(has_nursing_any, na.rm = TRUE), 1),
      Pct_Teaching = round(100 * mean(has_teaching_any, na.rm = TRUE), 1)
    )
  print(gender_educ)
  
  # Education data source quality
  cat("\n=== EDUCATION DATA SOURCE ===\n")
  source_dist <- combined_data %>%
    count(education_data_source) %>%
    mutate(pct = round(100 * n / nrow(combined_data), 2)) %>%
    arrange(desc(n))
  print(source_dist)
  
  # ============================
  # NEW: FERTILITY HISTORY DIAGNOSTICS
  # ============================
  
  cat("\n############################################\n")
  cat("# FERTILITY HISTORY DIAGNOSTICS            #\n")
  cat("############################################\n")
  
  cat("\n=== EVER-PARENT DISTRIBUTION ===\n")
  print(table(combined_data$ever_parent, combined_data$female, useNA = "ifany",
              dnn = c("ever_parent", "female")))
  
  cat("\n=== GROUP CLASSIFICATIONS ===\n")
  cat("\nAbsorbing (group_ever):\n")
  print(table(combined_data$group_ever, useNA = "ifany"))
  cat("\nTime-varying (group_current):\n")
  print(table(combined_data$group_current, useNA = "ifany"))
  cat("\nThree-way women (group_3way_women):\n")
  print(table(combined_data$group_3way_women, useNA = "ifany"))

  # THE TWO SPLITS SIDE BY SIDE, because the difference between them is
  # the quantity Section 4.5 of the paper is about and nobody has ever
  # printed it. The off-diagonal is future mothers' pre-birth waves.
  cat("\nThree-way women, lifetime (group_4way_women_life):\n")
  print(table(combined_data$group_4way_women_life, useNA = "ifany"))
  cat("\nWave-varying (rows) against lifetime (columns):\n")
  print(table(combined_data$group_3way_women,
              combined_data$group_4way_women_life, useNA = "ifany"))
  cat("  Read the never_mothers row: everything off its diagonal is a\n")
  cat("  woman the wave-varying split calls a never-mother at this wave\n")
  cat("  and who becomes a mother later.\n")
  
  cat("\n=== CONTAMINATION CHECK ===\n")
  cat("Women classified 'childless' under current definition who are ever_parent==1:\n")
  contam <- combined_data %>%
    filter(female == 1, group_current == "childless_women") %>%
    summarise(
      N_total = n(),
      N_ever_parent = sum(ever_parent == 1, na.rm = TRUE),
      Pct_contaminated = round(100 * mean(ever_parent == 1, na.rm = TRUE), 1)
    )
  print(contam)
  
  cat("\n=== BIRTH AND PREGNANCY EVENTS ACROSS PANEL ===\n")
  cat("  v17: these are two different items. lebth is the birth or\n")
  cat("  adoption of a new child; leprg is pregnancy, which for a\n")
  cat("  first-time parent fires in two consecutive waves.\n")
  birth_events <- combined_data %>%
    group_by(year) %>%
    summarise(
      N_births       = sum(had_child_this_wave == 1, na.rm = TRUE),
      N_women_births = sum(had_child_this_wave == 1 & female == 1, na.rm = TRUE),
      N_men_births   = sum(had_child_this_wave == 1 & female == 0, na.rm = TRUE),
      N_pregnancy    = sum(had_pregnancy_this_wave == 1, na.rm = TRUE),
      N_births_dated = sum(!is.na(birth_quarter_event)),
      N_births_multi = sum(birth_quarter_multi == 1 &
                             had_child_this_wave == 1, na.rm = TRUE),
      N_births_orphan_q = sum(birth_quarter_orphan == 1, na.rm = TRUE),
      N_births_undated   = sum(birth_undated == 1, na.rm = TRUE),
      N_quarter_declined = sum(birth_q_declined == 1, na.rm = TRUE),
      N_declined_dated   = sum(birth_q_declined_dated == 1, na.rm = TRUE),
      # v20: with the dated and multiple counts restricted to reporters
      # this residual is zero in every row by construction. Under v19 it
      # was non-zero in 2015, 2021, 2023 and 2024, which is the whole of
      # the discrepancy that looked like a mis-specified NA-box flag.
      Row_residual = N_births - N_births_dated - N_births_multi -
        N_births_undated,
      .groups = "drop"
    )
  print(as.data.frame(birth_events))
  
  # Summary
  cat("\n############################################\n")
  cat("# PROCESSING COMPLETE                      #\n")
  cat("############################################\n")
  cat("Total observations (broad sample):", format(nrow(combined_data), big.mark = ","), "\n")
  cat("Wage sample observations:", format(sum(combined_data$in_wage_sample == 1), big.mark = ","), "\n")
  cat("Non-wage observations:", format(sum(combined_data$in_wage_sample == 0), big.mark = ","), "\n")
  cat("Unique individuals:", format(n_distinct(combined_data$person_id), big.mark = ","), "\n")
  cat("Unique individuals in wage sample:", 
      format(n_distinct(combined_data$person_id[combined_data$in_wage_sample == 1]), big.mark = ","), "\n")
  
  # Selection into employment by motherhood status
  cat("\n=== EMPLOYMENT SELECTION BY MOTHERHOOD STATUS ===\n")
  selection_overall <- combined_data %>%
    filter(female == 1, !is.na(motherhood_status)) %>%
    group_by(motherhood_status) %>%
    summarise(
      N_broad = n(),
      N_employed = sum(esdtl_raw %in% c(1,2), na.rm = TRUE),
      N_wage_sample = sum(in_wage_sample == 1),
      Employment_rate = round(100 * N_employed / N_broad, 1),
      Wage_sample_rate = round(100 * N_wage_sample / N_broad, 1),
      .groups = "drop"
    )
  print(selection_overall)
  
  return(combined_data)
}

# =====================================================================
# EXECUTE DATA PROCESSING
# =====================================================================

# data_dir IS SET ONCE, from project_dir near the top. This line used to set
# it again from its own copy of the path, so moving the data meant editing
# two lines, and missing one would read the old folder without a word.
if (!dir.exists(data_dir))
  stop("data_dir does not exist: ", data_dir,
       "\n  Set project_dir, near the top of this file, to the folder that ",
       "holds dta_files.", call. = FALSE)

# Process all waves
cat(sprintf("\nProcessing HILDA waves %d-%d...\n", min(WAVES), max(WAVES)))
analysis_sample <- process_all_waves(data_dir, wpi_data)

# =====================================================================
# SAVE OUTPUTS
# =====================================================================

# Full all-variables frame: large and rarely needed. Off by default;
# set SAVE_FULL <- TRUE to archive it (xz: smallest, slow to write).
SAVE_FULL <- FALSE
if (SAVE_FULL) saveRDS(analysis_sample, .out_path("hilda_analysis_full.rds"), compress = "xz")

panel_full <- analysis_sample   # no disk round trip


# =====================================================================
# v21 ANALYSIS-STAGE RESTRICTIONS
# =====================================================================
# Five decisions about what this panel can support, settled off the v20
# log. They belong in the data rather than in a comment on whichever
# script reads it next:
#
#   firm size      does not cross wave 22 in any specification
#   education      wave 24 is a composition break, and the two study
#                  items are on a different instrument there
#   interruption   enters as a within-person flow, never as a level
#   superannuation household aggregate, so no individual comparison
#   wealth         observed at waves 14, 18 and 22 and nowhere else
#
# All of it is built AFTER combination, so no wave is reprocessed and
# CHECKPOINT_TAG does not move. This block writes the columns and the
# regime keys; it cannot enforce the restrictions, because a loader can
# label a regime and only the script running the regression knows
# whether it is about to pool across one. The enforcement lives in
# 00_compat_v21.R, which every downstream script sources first.

cat("\n=====================================================================\n")
cat(BUILD_TAG, " ANALYSIS-STAGE RESTRICTIONS\n", sep = "")
cat("=====================================================================\n")

# ---- Firm size: one regime at a time --------------------------------
# The two code frames reconcile and the category assignments agree. The
# coverage does not: roughly 60% of the employed classified through wave
# 21 against 99% from wave 22, and a small-firm share near 5% against
# 26% once the frames are aligned. The old item is a filtered question
# over-sampling large employers, so no recode repairs the series.
# firm_size_regime is the key the guard reads.
#
# v21.46: firm_size_comparable is no longer empty, and the reasoning
# above is why it is SIX levels rather than three. Nothing here claims
# the series is repaired; the column carries its regime so that a design
# spanning the break has to say what it is doing with it.
panel_full$firm_size_regime <- ifelse(
  panel_full$wave < FIRM_SIZE_BREAK_WAVE,
  sprintf("w%d_%d_jbmemsz", min(WAVES), FIRM_SIZE_BREAK_WAVE - 1L),
  sprintf("w%d_%d_jbmems2", FIRM_SIZE_BREAK_WAVE, max(WAVES)))

if ("firm_size_instrument" %in% names(panel_full)) {
  .fs_mismatch <- sum(
    (panel_full$wave <  FIRM_SIZE_BREAK_WAVE &
       panel_full$firm_size_instrument != "jbmemsz") |
      (panel_full$wave >= FIRM_SIZE_BREAK_WAVE &
         panel_full$firm_size_instrument != "jbmems2"), na.rm = TRUE)
  if (.fs_mismatch > 0L)
    stop(BUILD_TAG, ": firm_size_instrument disagrees with the wave on ",
         .fs_mismatch, " person-wave(s). A checkpoint written under a ",
         "different break wave is being replayed; delete wave_checkpoints/.",
         call. = FALSE)
}

# SCRUBBED HERE AND REBUILT A FEW LINES DOWN. A checkpoint written while
# FIRM_SIZE_VERIFIED was still a single switch could carry a non-NA value
# onto the panel, and this is the column a specification reaches for by
# name, so it is emptied unconditionally before anything fills it.
#
# v21.46. THE ORDER MATTERS AND THE FIRST ATTEMPT GOT IT WRONG. The fill
# was originally placed ABOVE this line, so this scrub ran afterwards and
# wiped it: the run completed, printed a populated six-level table, and
# saved an all-NA column. The panel fingerprint caught it on its first
# real comparison, reporting one column added and none changed when one
# had certainly changed. Anything that fills this column belongs BELOW
# this line.
panel_full$firm_size_comparable <- NA_character_

.fs_emp  <- !is.na(panel_full$employed) & panel_full$employed == 1
.fs_pre  <- .fs_emp & panel_full$wave <  FIRM_SIZE_BREAK_WAVE
.fs_post <- .fs_emp & panel_full$wave >= FIRM_SIZE_BREAK_WAVE
cat(sprintf(paste0("\nFirm size. Classified %.1f%% of the employed on waves ",
                   "%d-%d against %.1f%%\n  on waves %d-%d. ",
                   "firm_size is descriptive WITHIN one\n",
                   "  firm_size_regime; the two columns built below are the\n",
                   "  ones a specification should reach for.\n"),
            100 * mean(!is.na(panel_full$firm_size[.fs_pre])),
            min(WAVES), FIRM_SIZE_BREAK_WAVE - 1L,
            100 * mean(!is.na(panel_full$firm_size[.fs_post])),
            FIRM_SIZE_BREAK_WAVE, max(WAVES)))
rm(.fs_emp, .fs_pre, .fs_post)

# v21.46. THE TWO USABLE FIRM-SIZE COLUMNS, BUILT HERE AND NOT IN THE
# WAVE MUTATE.
#
# WHY HERE. Both derive from firm_size and the wave, and both are on the
# combined frame already, so building them after combination costs
# nothing and leaves CHECKPOINT_TAG where it is. Putting them inside the
# wave mutate would need a hand bump and a fourteenth re-extraction for
# two case_when() calls, which is the trade v21.35 declined and this one
# declines for the same reason.
#
# WHAT WAS WRONG WITH LEAVING firm_size_comparable EMPTY. Nothing, as a
# statement about the series: the frames reconcile and the coverage does
# not, and no recode repairs a filtered question. What was wrong was the
# NAME. A permanently empty column called _comparable reads as "we could
# not do this" when the accurate statement is "this is comparable within
# a regime and there is no such thing as across one". A column that says
# that in its VALUES cannot be misread, and an empty one cannot say it
# at all.
#
# firm_size_comparable IS THE REGIME-TAGGED SERIES. Six levels, of the
# form pre_Small or post_Large, populated wherever firm_size is. The
# regime is inside the value rather than beside it in firm_size_regime,
# which is the difference between a design that CAN interact with the
# break and one that cannot avoid doing so. No level spans wave 22, so
# no single coefficient can average across the instrument change.
#
# IT DOES NOT REPAIR THE COVERAGE BREAK AND MUST NOT BE READ AS DOING SO.
# Comparing the pre_Small coefficient with post_Small still compares a
# filtered question with an unfiltered one. What the tagging buys is that
# the comparison has to be written down to be made.
#
# firm_size_post22 IS THE CLEAN ONE. Three levels, one regime, NA before
# the break. Comparable by construction and useless for any design that
# needs a pre-2022 person-wave, which is most of them. It exists so that
# "restrict to one regime" is one column rather than a filter each caller
# writes for itself.
if ("firm_size" %in% names(panel_full)) {
  .fs_pre <- panel_full$wave < FIRM_SIZE_BREAK_WAVE
  panel_full$firm_size_comparable <- ifelse(
    is.na(panel_full$firm_size), NA_character_,
    paste0(ifelse(.fs_pre, "pre_", "post_"), panel_full$firm_size))
  panel_full$firm_size_post22 <- ifelse(
    .fs_pre, NA_character_, panel_full$firm_size)

  cat("\n--- Firm size: the two derived columns ---\n")
  print(table(panel_full$firm_size_comparable, useNA = "ifany"))
  cat("  firm_size_post22 non-missing: ",
      format(sum(!is.na(panel_full$firm_size_post22)), big.mark = ","),
      " of ", format(nrow(panel_full), big.mark = ","),
      " person-waves (waves ", FIRM_SIZE_BREAK_WAVE, "-", max(WAVES), ").\n",
      sep = "")
  # v21.46b. THE CHECK THE SWEEP CANNOT MAKE. combined_data is swept
  # upstream of here, so the sweep sees this column legitimately empty and
  # exempts it. This is the point at which "empty" stops being expected,
  # and a silent all-NA here is exactly what the misplaced fill produced
  # in the first v21.46 run: a populated table printed, an empty column
  # saved. Stop rather than warn, because every downstream consumer
  # reaches for this column by name.
  if (all(is.na(panel_full$firm_size_comparable)))
    stop(BUILD_TAG, ": firm_size_comparable is all-NA after the fill. ",
         "Something between the scrub and here has emptied it, or ",
         "firm_size is itself empty. A run that prints the table above ",
         "and saves an empty column is the failure this stop() exists ",
         "for.", call. = FALSE)
  if (sum(!is.na(panel_full$firm_size_comparable)) !=
      sum(!is.na(panel_full$firm_size)))
    stop(BUILD_TAG, ": firm_size_comparable and firm_size disagree on ",
         "coverage. The tagging is a relabelling and must not add or ",
         "drop an observation.", call. = FALSE)

  cat("  Use firm_size_comparable when the design spans the break and\n")
  cat("  firm_size_post22 when it does not. firm_size itself is\n")
  cat("  descriptive within a regime and should not enter an estimate\n")
  cat("  that crosses wave ", FIRM_SIZE_BREAK_WAVE, ".\n", sep = "")

  # THE BREAK, MEASURED WITHIN PERSON RATHER THAN ASSERTED. The composition
  # jump is a marginal and a marginal cannot separate an instrument change
  # from a coverage change. This crosses the same people at wave 21 and 22:
  # a dominant diagonal says the crosswalk holds and the jump is coverage,
  # while systematic Large-to-Medium drift says the bands do not line up.
  # The row that matters is the one for people unclassified before and
  # classified after, because that is where the extra coverage lands.
  .b <- FIRM_SIZE_BREAK_WAVE
  if (all(c(.b - 1L, .b) %in% panel_full$wave)) {
    .l <- panel_full[panel_full$wave == .b - 1L, c("person_id", "firm_size")]
    .r <- panel_full[panel_full$wave == .b,      c("person_id", "firm_size")]
    names(.l)[2] <- "at_pre"; names(.r)[2] <- "at_post"
    .j <- merge(.l, .r, by = "person_id")
    if (nrow(.j)) {
      cat(sprintf("\n  Within-person transition across the break, %d person(s) in both waves:\n",
                  nrow(.j)))
      print(table(pre = addNA(.j$at_pre), post = addNA(.j$at_post)))
      .seen <- !is.na(.j$at_pre) & !is.na(.j$at_post)
      if (any(.seen))
        cat(sprintf("  Of the %d classified in both, %.1f%% keep the same category.\n",
                    sum(.seen),
                    100 * mean(.j$at_pre[.seen] == .j$at_post[.seen])))
      .gain <- is.na(.j$at_pre) & !is.na(.j$at_post)
      if (any(.gain)) {
        cat(sprintf("  %d gain a value at the break; their post categories:\n",
                    sum(.gain)))
        print(round(100 * prop.table(table(.j$at_post[.gain])), 1))
        cat("  If those are overwhelmingly Small, the pre-break item was\n")
        cat("  filtered on size and the coverage break is the whole story.\n")
      }
      rm(.seen, .gain)
    }
    rm(.l, .r, .j)
  }
  rm(.fs_pre, .b)
}

# ---- Wave 24 education: a break, not a trend ------------------------
# Wave 24 combines a sample refresher with an instrument change on
# edcqen and edfts, and observed coverage of the two study items roughly
# triples. The raw columns are kept, because the wave-24 cross-section
# is perfectly usable on its own terms. What is not usable is a series
# that runs through it, so the *_comparable copies stop at wave 23.
panel_full$study_item_regime <- ifelse(
  panel_full$wave >= STUDY_ITEM_BREAK_WAVE,
  sprintf("w%d_refreshed_instrument", STUDY_ITEM_BREAK_WAVE),
  sprintf("w%d_%d", min(WAVES), STUDY_ITEM_BREAK_WAVE - 1L))
panel_full$educ_trend_comparable <-
  as.integer(!panel_full$wave %in% EDUC_COMPOSITION_BREAK_WAVES)

# v21.13: which side of the E1/E6 move a person-wave sits on. Not a
# correction and not a comparability flag in the firm-size sense, because
# the item is the same item; it is a regime key, so a design can interact
# on it or restrict to one side and has to decide which.
panel_full$occ_origin_regime <- ifelse(
  panel_full$wave < OCC_ORIGIN_BREAK_WAVE, "three_routes_w12_w19",
  "two_routes_w20_w24")

panel_full$training_gate_regime <- ifelse(
  panel_full$wave < TRAINING_GATE_POSITION_BREAK_WAVE, "E1_w7_w19", "E6_w20_w24")

.study_cols <- intersect(c("enrolled_in_course", "full_time_student"),
                         names(panel_full))
for (.cl in .study_cols)
  panel_full[[paste0(.cl, "_comparable")]] <-
  ifelse(panel_full$wave >= STUDY_ITEM_BREAK_WAVE, NA_real_,
         as.numeric(panel_full[[.cl]]))

if (length(.study_cols)) {
  .stcov <- panel_full %>%
    dplyr::group_by(wave) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(.study_cols),
                                   ~ round(100 * mean(!is.na(.x)), 1),
                                   .names = "pct_obs_{.col}"),
                     .groups = "drop")
  cat("\nStudy items: observed coverage by wave, percent\n")
  print(as.data.frame(.stcov), row.names = FALSE)
  rm(.stcov)
}
cat(sprintf(paste0("  Wave %d is a refresher AND an instrument change. The\n",
                   "  *_comparable copies are NA there and ",
                   "educ_trend_comparable is 0.\n",
                   "  Read wave %d as a level in its own right, never as the ",
                   "last point\n  of a trend.\n"),
            STUDY_ITEM_BREAK_WAVE, STUDY_ITEM_BREAK_WAVE))
rm(.study_cols)

# ---- Interruption enters as a flow, not a level ---------------------
# ehtse, ehtjb, ehtuj and ehto are History stocks anchored on a
# retrospective NPQ report taken at panel entry and rolled forward. The
# recall error in the base stock is a person-level constant, so it
# differences out of a within-person increment; it does not leave a
# LEVEL that is comparable across entry cohorts. On this window the
# original and 2011 intakes carry 4.93 years out of the labour force
# against 1.72 for the 2019-plus intake, which is age composition and
# instrument vintage rather than behaviour.
#
# The increments were being rebuilt at the foot of this file and again
# in each script that wanted them. Built once here instead. d_wave is
# carried because an increment across a gap year is not an annual flow
# and must not be read as one.
panel_full <- panel_full %>%
  dplyr::arrange(person_id, wave) %>%
  dplyr::group_by(person_id) %>%
  dplyr::mutate(
    entry_wave       = min(wave, na.rm = TRUE),
    d_wave           = wave - dplyr::lag(wave),
    d_paid_years     = actual_experience     - dplyr::lag(actual_experience),
    d_unemp_years    = years_unemployed      - dplyr::lag(years_unemployed),
    d_olf_years      = years_not_in_lf       - dplyr::lag(years_not_in_lf),
    d_exposure_years = years_since_education - dplyr::lag(years_since_education)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    dplyr::across(c(d_paid_years, d_unemp_years, d_olf_years,
                    d_exposure_years),
                  ~ ifelse(is.na(d_wave) | d_wave != 1L, NA_real_, .x)),
    history_flow_sum = d_paid_years + d_unemp_years + d_olf_years,
    olf_year         = ifelse(is.na(d_olf_years), NA_integer_,
                              as.integer(d_olf_years > 0.5)),
    olf_flow_valid   = as.integer(!is.na(d_olf_years)),
    # entry_wave is entry into THIS window, so the first group is
    # everyone already in the panel at wave 12: the original intake and
    # the 2011 top-up together.
    #
    # v21.1: the labels now name the waves they contain. The cut points
    # are unchanged, and they are the ones behind the published figures
    # of 4.93 years against 1.72 for women, but v21 called the last group
    # "2019_plus" when it is entry at wave 20 or later, i.e. 2020 onward.
    # The wave-19 refresher entrants are in the middle group. If the
    # manuscript describes that group as entering from 2019, either the
    # wording follows this cut or the cut moves and the figure is
    # restated; they cannot both stand.
    entry_cohort     = dplyr::case_when(
      entry_wave <= min(WAVES) ~ "in_panel_at_w12",
      entry_wave <= 19L        ~ "entry_w13_w19",
      TRUE                     ~ "entry_w20_plus")
  )

# v21.1: sex is in the grouping. Pooled, the spread reads 2.11 years,
# which is not a number anywhere in the paper; the reported contrast is
# women's, 4.93 years against 1.72, and pooling across sex hides most of
# it because men's levels barely move by cohort.
# female is guarded rather than assumed: this block runs at the very end
# of a long build and should not fail there over a grouping variable.
.has_sex <- "female" %in% names(panel_full)
.coh <- panel_full %>%
  dplyr::group_by(dplyr::across(dplyr::any_of(c("entry_cohort", "female")))) %>%
  dplyr::summarise(person_waves   = dplyr::n(),
                   mean_olf_level = round(mean(years_not_in_lf,
                                               na.rm = TRUE), 2),
                   pct_olf_year   = round(100 * mean(olf_year,
                                                     na.rm = TRUE), 1),
                   .groups = "drop")
cat("\nInterruption by entry cohort and sex: the level moves, the flow does not\n")
print(as.data.frame(.coh), row.names = FALSE)
.rng <- function(z) suppressWarnings(diff(range(z, na.rm = TRUE)))
.spread_by_sex <- if (.has_sex)
  vapply(split(.coh$mean_olf_level, .coh$female), .rng, numeric(1)) else
    c(pooled = .rng(.coh$mean_olf_level))
.flow_by_sex   <- if (.has_sex)
  vapply(split(.coh$pct_olf_year, .coh$female), .rng, numeric(1)) else
    c(pooled = .rng(.coh$pct_olf_year))
.sex_lab <- if (.has_sex) paste0("female=", names(.spread_by_sex)) else
  names(.spread_by_sex)
.v21_facts$olf_level_spread <- .spread_by_sex
.olf_spread <- suppressWarnings(max(.spread_by_sex, na.rm = TRUE))
if (is.finite(.olf_spread)) {
  cat("  Cohort spread in the LEVEL: ",
      paste(sprintf("%s %.2f yrs", .sex_lab, .spread_by_sex),
            collapse = ", "), "\n", sep = "")
  cat("  Cohort spread in the FLOW:  ",
      paste(sprintf("%s %.1f pts", .sex_lab, .flow_by_sex),
            collapse = ", "), "\n", sep = "")
  cat(sprintf(paste0("  Widest level spread %.2f years against a flow that ",
                     "moves a fraction of that.\n  OLF_LEVEL_COMPARABLE is ",
                     "%s. Use d_olf_years and olf_year, or\n  olf_share, ",
                     "which normalises by career length. years_not_in_lf ",
                     "is a\n  descriptive stock and a within-person control, ",
                     "not a cross-person\n  regressor.\n"),
              .olf_spread, OLF_LEVEL_COMPARABLE))
}
rm(.coh, .spread_by_sex, .flow_by_sex, .rng, .sex_lab, .has_sex)

# ---- Superannuation: no individual comparison -----------------------
# hwsupei is a household aggregate, determined from the couple
# cross-check in the post-combination diagnostics rather than assumed.
# A column called super_balance sitting beside person_id invites exactly
# the individual terminal-value comparison the item cannot support, so
# it is renamed rather than annotated, and the old name is a deprecation
# with a hard stop.
if ("super_balance" %in% names(panel_full)) {
  panel_full <- dplyr::rename(panel_full, hh_super_balance = super_balance)
  cat("\nSuperannuation. super_balance renamed hh_super_balance.\n")
}
.sp <- .v21_facts$super_partner_identical
if (!is.na(.sp)) {
  cat(sprintf("  Partners hold an identical value on %.1f%% of %s couple-waves.\n",
              100 * .sp, format(.v21_facts$super_couple_waves, big.mark = ",")))
  if (.sp < 0.5 && isTRUE(SUPER_IS_HOUSEHOLD))
    warning(BUILD_TAG, ": partners hold an identical superannuation value on ",
            sprintf("%.1f%%", 100 * .sp), " of couple-waves, which is not the ",
            "signature of a household aggregate. SUPER_IS_HOUSEHOLD is still ",
            "TRUE; re-read the couple cross-check before leaving it there.",
            call. = FALSE)
} else {
  cat("  The couple cross-check did not run this build, so the household\n")
  cat("  reading rests on SUPER_IS_HOUSEHOLD alone.\n")
}
cat("  No individual superannuation comparison is available from this\n")
cat("  release. Household-level description is.\n")
rm(.sp)

# ---- Wealth: waves 14, 18 and 22 ------------------------------------
# The module runs every four years, so inside this window it is three
# observations per person. A wealth value outside those waves is a
# mapping error rather than an observation, and averaging one into a
# trend is the failure this stop exists to prevent.
panel_full$wealth_module_wave <-
  as.integer(panel_full$wave %in% WEALTH_MODULE_WAVES)
.wealth_cols <- intersect(c("hh_super_balance", "hh_net_worth",
                            "hh_net_worth_pos", "hh_net_worth_neg"),
                          names(panel_full))
if (length(.wealth_cols)) {
  .off <- vapply(.wealth_cols, function(cl)
    sum(!is.na(panel_full[[cl]]) & panel_full$wealth_module_wave == 0L),
    numeric(1))
  .inn <- vapply(.wealth_cols, function(cl)
    sum(!is.na(panel_full[[cl]]) & panel_full$wealth_module_wave == 1L),
    numeric(1))
  cat(sprintf("\nWealth module waves: %s\n",
              paste(WEALTH_MODULE_WAVES, collapse = ", ")))
  print(data.frame(column = .wealth_cols,
                   obs_in_module_waves = .inn,
                   obs_outside         = .off,
                   row.names = NULL), row.names = FALSE)
  if (any(.off > 0))
    stop(BUILD_TAG, ": wealth values observed OUTSIDE waves ",
         paste(WEALTH_MODULE_WAVES, collapse = ", "), ": ",
         paste(sprintf("%s = %d", .wealth_cols[.off > 0], .off[.off > 0]),
               collapse = ", "),
         ". Either WEALTH_MODULE_WAVES is wrong for this release or the ",
         "mnemonic is not the module item it is taken for.", call. = FALSE)
  rm(.off, .inn)
}
if ("in_wealth_module" %in% names(panel_full)) {
  .wdis <- sum(panel_full$in_wealth_module != panel_full$wealth_module_wave,
               na.rm = TRUE)
  cat(sprintf("  in_wealth_module (data-derived) disagrees with the wave list on %s rows.\n",
              format(.wdis, big.mark = ",")))
  if (.wdis > 0)
    warning(BUILD_TAG, ": in_wealth_module and wealth_module_wave disagree on ",
            .wdis, " person-wave(s). One of the two is not measuring module ",
            "membership.", call. = FALSE)
  rm(.wdis)
}
rm(.wealth_cols)

# ---- Is the enrolment item routed to new entrants? ------------------
# Three readings fit the wave-24 jump in enrolled_in_course coverage:
# the item changed, the sample changed, or the item is asked only of
# respondents doing the new-person questionnaire and the refresher
# simply brought more of them. The third is testable here, because
# entry_wave was built a few lines above, and it is the reading with the
# largest consequence: if it holds, the column describes new entrants in
# every wave and not just at wave 24.
#
# Wave 12 is excluded. entry_wave is entry into THIS window, so everyone
# is an entrant at the first wave and including it would guarantee the
# answer the test is looking for.
if (all(c("enrolled_in_course", "entry_wave") %in% names(panel_full))) {
  # First, what does NA mean here? If the column carries two codes then a
  # missing value means the question was not put, and the non-missing
  # share is a COVERAGE rate. If it carries one code then NA means "no",
  # the share is an ENROLMENT rate, and a rise at wave 24 is a younger
  # refreshed sample enrolling more rather than anything about the item.
  # The two readings call for opposite responses, so this comes first.
  .vals <- sort(unique(panel_full$enrolled_in_course[
    !is.na(panel_full$enrolled_in_course)]))
  cat("\nEnrolment item: what is behind the wave-", STUDY_ITEM_BREAK_WAVE,
      " coverage jump?\n", sep = "")
  cat("  Observed codes: ", paste(utils::head(.vals, 6), collapse = ", "),
      if (length(.vals) > 6) " ..." else "",
      sprintf("  (%d distinct)\n", length(.vals)), sep = "")
  .one_code <- length(.vals) <= 1L
  
  .rt <- panel_full %>%
    dplyr::filter(wave > min(WAVES)) %>%
    dplyr::mutate(new_entrant = wave == entry_wave) %>%
    dplyr::group_by(new_entrant) %>%
    dplyr::summarise(
      person_waves   = dplyr::n(),
      pct_edcqen_obs = round(100 * mean(!is.na(enrolled_in_course)), 1),
      pct_edfts_obs  = if ("full_time_student" %in% names(panel_full))
        round(100 * mean(!is.na(full_time_student)), 1) else NA_real_,
      .groups = "drop")
  print(as.data.frame(.rt), row.names = FALSE)
  .p_new <- .rt$pct_edcqen_obs[.rt$new_entrant]
  .p_old <- .rt$pct_edcqen_obs[!.rt$new_entrant]
  
  if (length(.p_new) == 1L && length(.p_old) == 1L) {
    .ratio <- if (.p_old > 0) .p_new / .p_old else Inf
    .v21_facts$edcqen_entrant_rates <- c(new = .p_new, continuing = .p_old)
    cat(sprintf("  New entrants %.1f%% against continuing %.1f%%, ratio %s.\n",
                .p_new, .p_old,
                if (is.finite(.ratio)) sprintf("%.1f", .ratio) else "infinite"))
    if (.one_code) {
      cat("  ONE CODE, so this is an enrolment rate and not a coverage\n")
      cat("  rate. The entrant gap above is then age and composition, not\n")
      cat("  routing: new entrants are younger and enrol more. On this\n")
      cat("  reading nothing about the item changed at wave ",
          STUDY_ITEM_BREAK_WAVE, " and the\n  refresher is what has to be ",
          "controlled for. Confirm against the\n  continuing-sample ",
          "comparison in the education diagnostic above.\n", sep = "")
    } else if (.p_old < 10 && .ratio >= 5) {
      cat("  ROUTED. Two codes and a near-zero continuing rate: the item is\n")
      cat("  put to new entrants, so its coverage is the entrant share and\n")
      cat("  wave ", STUDY_ITEM_BREAK_WAVE, " is a refresher intake rather ",
          "than a break. The column\n  then describes NEW ENTRANTS in every ",
          "wave, which is a stronger\n  restriction than this build ",
          "imposes: do not read it as a sample\n  enrolment rate anywhere. ",
          "full_time_student (edfts) is derived for\n  everyone in every ",
          "wave and is the study measure that works today,\n  and edccenr ",
          "(CPQ:A17b) is the continuing-person counterpart to read\n  ",
          "beside edcqen; see the candidate list beside the variable map.\n",
          sep = "")
      # v21.3: act on it. There is no comparable enrolment series to
      # hold, so the comparable column is emptied for the whole panel
      # rather than for wave 24 alone. Same treatment as
      # firm_size_comparable, and for the same reason: an item that
      # cannot support a series gets an empty column and a stated
      # reason, not a restriction aimed at one wave of it.
      .v21_facts$edcqen_routed <- TRUE
      if ("enrolled_in_course_comparable" %in% names(panel_full)) {
        panel_full$enrolled_in_course_comparable <- NA_real_
        cat("  enrolled_in_course_comparable is now NA on the WHOLE panel,\n")
        cat("  not at wave ", STUDY_ITEM_BREAK_WAVE, " alone. The raw column ",
            "is kept and is\n  usable for a statement about new entrants, ",
            "which is what it measures.\n", sep = "")
      }
    } else if (.p_old >= 30) {
      .v21_facts$edcqen_routed <- FALSE
      cat("  NOT ROUTED. The continuing rate is substantial, so coverage\n")
      cat("  does not track entrant status and the wave-", STUDY_ITEM_BREAK_WAVE,
          " jump is the\n  instrument or the composition after all. The ",
          "*_comparable columns\n  are aimed correctly.\n", sep = "")
    } else {
      cat("  PARTIAL. Neither reading is clean at these rates. Read the\n")
      cat("  ratio above beside the continuing-sample comparison in the\n")
      cat("  education diagnostic; a ratio well above one with a small\n")
      cat("  continuing rate is still routing, only leakier.\n")
    }
    rm(.ratio)
  }
  
  # ---- 0/1 indicators beside the raw codes (v21.3) ------------------
  # Both study items are raw questionnaire codes. edcqen carries 1 and 2,
  # so mean() of it is near 1.75 and sum() of it counts nothing. The
  # indicator is built as == 1 after the code set is printed, which is
  # right for a 1/2 item and for a 0/1 one, and the raw columns stay.
  for (.cl in intersect(c("enrolled_in_course", "full_time_student"),
                        names(panel_full))) {
    .codes <- sort(unique(panel_full[[.cl]][!is.na(panel_full[[.cl]])]))
    panel_full[[paste0(.cl, "_yes")]] <-
      ifelse(is.na(panel_full[[.cl]]), NA_integer_,
             as.integer(panel_full[[.cl]] == 1))
    cat(sprintf("  %-20s codes %s -> %s_yes is 1 on %.1f%% of observed\n",
                .cl, paste(utils::head(.codes, 5), collapse = "/"), .cl,
                100 * mean(panel_full[[paste0(.cl, "_yes")]], na.rm = TRUE)))
    if (!all(.codes %in% c(0, 1, 2)))
      warning(BUILD_TAG, ": ", .cl, " carries code(s) outside 0, 1 and 2: ",
              paste(setdiff(.codes, c(0, 1, 2)), collapse = ", "),
              ". The == 1 indicator may be wrong for this release.",
              call. = FALSE)
    rm(.codes)
  }
  
  rm(.rt, .p_new, .p_old, .vals, .one_code)
}

# ---- Training module: coding and coverage (v21.6) -------------------
# Two things the loader has asserted rather than checked since v5.
#
# FIRST, the coding. Every mentioned-style training item is read with
# == 1, which is right under a 0/1 coding and under a 1/2 one, and wrong
# under anything else. The v5 comment said to verify this with
# table(jttpewt_raw) on the first run and no verification was ever
# written. It runs here, on all five items in all thirteen waves. A code
# outside 0/1/2 stops the build. So does a MIXED coding, an item that is
# 0/1 in one wave and 1/2 in another, because == 1 then means two
# different things inside one pooled series and nothing downstream can
# see it.
#
# SECOND, the coverage. This is the report that 03's module check cannot
# produce, because the columns it tests are never NA.
# v21.48: the aim battery's don't-know and refused flags are audited here,
# not in the v21.12 block with the other aim items. Refused is rare enough
# that some waves may have no one mentioning it, so its code set in those
# waves is just the "not mentioned" code. The v21.12 block stops on ANY
# change of code set across waves and would halt the build on that. This
# block stops only on a real mixture: 0/1 in one wave and 1/2 in another.
.tr_code_cols <- intersect(c("jttrwrk_raw", "jttpewt_raw", "jttpeot_raw",
                             "jttopwt_raw", "jttopot_raw",
                             "training_aim_dk_raw",
                             "training_aim_refused_raw"),
                           names(panel_full))
if (length(.tr_code_cols)) {
  cat("\n--- Training item coding, by wave (v21.6) ---\n")
  .tc <- do.call(rbind, lapply(.tr_code_cols, function(cl) {
    do.call(rbind, lapply(sort(unique(panel_full$wave)), function(w) {
      x <- panel_full[[cl]][panel_full$wave == w]
      u <- sort(unique(x[!is.na(x)]))
      data.frame(item = cl, wave = w,
                 n_obs = sum(!is.na(x)),
                 codes = if (length(u)) paste(u, collapse = "/") else "",
                 stringsAsFactors = FALSE)
    }))
  }))
  .cs <- tapply(.tc$codes, .tc$item, function(z) unique(z[nzchar(z)]))
  for (nm in names(.cs))
    cat(sprintf("  %-12s code set(s) across waves: %s\n", nm,
                paste(.cs[[nm]], collapse = "  |  ")))
  # Bare filename, matching every other artefact this loader writes.
  write.csv(.tc, .out_path("v216_training_item_codes.csv"), row.names = FALSE)
  
  .bad_code <- unique(unlist(lapply(.tr_code_cols, function(cl) {
    u <- unique(panel_full[[cl]][!is.na(panel_full[[cl]])])
    if (all(u %in% c(0, 1, 2))) character(0) else cl
  })))
  if (length(.bad_code))
    stop(BUILD_TAG, ": training item(s) carry a code outside 0, 1 and 2: ",
         paste(.bad_code, collapse = ", "),
         ". The == 1 rule in the training indicators is wrong for this ",
         "release. Read v216_training_item_codes.csv before changing it.",
         call. = FALSE)
  
  # A mixed coding is the dangerous case, because both codings are
  # individually legal and == 1 survives each on its own.
  .mixed <- names(.cs)[vapply(.cs, function(z) {
    has01 <- any(vapply(z, function(s) "0" %in% strsplit(s, "/")[[1]],
                        logical(1)))
    has2  <- any(vapply(z, function(s) "2" %in% strsplit(s, "/")[[1]],
                        logical(1)))
    has01 && has2
  }, logical(1))]
  if (length(.mixed))
    stop(BUILD_TAG, ": training item(s) mix a 0/1 and a 1/2 coding across ",
         "waves: ", paste(.mixed, collapse = ", "),
         ". == 1 means a different thing on each side of the change, so ",
         "the pooled series is not one quantity. Split the window or ",
         "recode per wave; see v216_training_item_codes.csv.",
         call. = FALSE)
  cat("  Coding check passed: every item is 0/1/2 and consistent across waves.\n")
  rm(.tc, .cs, .bad_code, .mixed)
}
rm(.tr_code_cols)

# ---- v21.12 CODING AUDIT FOR THE NEW BINARY ITEMS -------------------
# Same discipline as the training block above, applied to the six items
# this batch adds. Their 0/1 versions are built on the == 1 rule, which is
# an ASSUMPTION about the release; this reads the codes and says whether
# the assumption holds. Six revisions of asserting is what that block was
# written to end, so the new items get the check on their first build
# rather than their sixth.
.ppl_code_cols <- intersect(
  c("ppl_in_wages_raw", "ppl_in_fy_income_raw", "ppl_entitled_mat_raw",
    "ppl_entitled_pat_raw", "ppl_received_cur_raw", "ppl_received_fy_raw",
    # v21.21: the aim branches get the same treatment. Their 0/1 versions
    # use the == 1 rule and that is an assumption until this has run.
    "training_aim_getstarted_raw", "training_aim_maintain_raw",
    "training_aim_safety_raw",
    # v21.29: and the four older branches, which had no _raw twin and so
    # could never be audited at all.
    "training_aim_future_raw", "training_aim_current_raw",
    "training_aim_general_raw", "training_self_funded_raw",
    # v21.32: the two non-cash binaries. nbcmany is not in the manual's
    # Table 4.24 and nbcmccr's coding has not been read, so both reach
    # the == 1 rule as assumptions and both are audited before anything
    # rests on them. noncash_any_value needs no audit because it is
    # arithmetic on the DV.
    "noncash_any_raw", "employer_childcare_raw"),
  names(panel_full))
if (length(.ppl_code_cols)) {
  cat("\n--- New binary item coding, by wave (v21.12) ---\n")
  .pc <- do.call(rbind, lapply(.ppl_code_cols, function(cl) {
    do.call(rbind, lapply(sort(unique(panel_full$wave)), function(w) {
      x <- panel_full[[cl]][panel_full$wave == w]
      u <- sort(unique(x[!is.na(x)]))
      data.frame(item = cl, wave = w,
                 codes = if (length(u)) paste(u, collapse = "/") else "none",
                 n_obs = sum(!is.na(x)),
                 pct_obs = round(100 * mean(!is.na(x)), 1),
                 stringsAsFactors = FALSE)
    }))
  }))
  for (cl in .ppl_code_cols) {
    .sets <- unique(.pc$codes[.pc$item == cl & .pc$codes != "none"])
    .cov  <- range(.pc$pct_obs[.pc$item == cl])
    cat(sprintf("  %-22s code set(s): %-12s coverage %.1f-%.1f%%\n",
                cl, paste(.sets, collapse = " | "), .cov[1], .cov[2]))
  }
  write.csv(.pc, .out_path("v2112_new_item_codes.csv"), row.names = FALSE)
  # Anything outside 0/1/2 means the == 1 rule is reading something else.
  .pbad <- unique(.pc$item[vapply(seq_len(nrow(.pc)), function(i) {
    if (.pc$codes[i] == "none") return(FALSE)
    any(!(as.numeric(strsplit(.pc$codes[i], "/")[[1]]) %in% c(0, 1, 2)))
  }, logical(1))])
  if (length(.pbad))
    stop(BUILD_TAG, ": new item(s) carry a code outside 0, 1 and 2: ",
         paste(.pbad, collapse = ", "),
         ". The == 1 rule is wrong for this release. Read ",
         "v2112_new_item_codes.csv and fix the derivation before using ",
         "the 0/1 columns.", call. = FALSE)
  # A code set that CHANGES across waves is the dangerous case: both
  # codings are individually valid and the derived column then means two
  # different things in one series.
  .pmix <- vapply(.ppl_code_cols, function(cl) {
    length(unique(.pc$codes[.pc$item == cl & .pc$codes != "none"])) > 1L
  }, logical(1))
  if (any(.pmix))
    stop(BUILD_TAG, ": new item(s) are coded DIFFERENTLY in different ",
         "waves: ", paste(.ppl_code_cols[.pmix], collapse = ", "),
         ". A single == 1 rule cannot be right in every wave. See ",
         "v2112_new_item_codes.csv.", call. = FALSE)
  cat("  Coding check passed: every new item is 0/1/2 and consistent\n")
  cat("  across waves, so the == 1 rule reads the same thing everywhere.\n")
  cat("  Coverage is the other half: an item observed on a few per cent\n")
  cat("  of person-waves is a statement about that few per cent.\n")
  rm(.pc, .pbad, .pmix)
}
rm(.ppl_code_cols)

# ---- v21.29 AIM-BATTERY DIAGNOSTIC ----------------------------------
# Three things must be true before the aim contrast means anything, and
# none is visible from the marginals (31.2% and 64.0% positive on 27.3%
# coverage is equally consistent with two readings that imply opposite
# conclusions).
#
# (1) OVERLAP. E12 is multi-response, so a course can be both. If the
#     joint cell is empty the two indicators are mechanically
#     anti-correlated and "promotion falls while maintenance does not" is
#     guaranteed by the coding rather than by employer behaviour.
# (2) ITEM NON-RESPONSE AMONG TRAINERS. The _z recode coalesces a
#     no-training person-wave to 0, which is right only if the NA is
#     driven by not training rather than by trainers skipping E12.
# (3) PER-WAVE COVERAGE. If a constituent item is absent in some waves
#     the index is unbalanced and an event study on it confounds the aim
#     contrast with wave composition.
if (all(c("promotion_training", "maintenance_training",
          "aim_battery_asked", "any_training") %in% names(panel_full))) {
  cat("\n--- Aim battery: overlap (v21.29) ---\n")
  .ov <- panel_full[!is.na(panel_full$promotion_training) &
                      !is.na(panel_full$maintenance_training), ]
  .tb <- as.data.frame(table(promotion = .ov$promotion_training,
                             maintenance = .ov$maintenance_training))
  .tb$pct <- round(100 * .tb$Freq / sum(.tb$Freq), 1)
  print(.tb, row.names = FALSE)
  .joint <- sum(.ov$promotion_training == 1 & .ov$maintenance_training == 1)
  if (.joint == 0) {
    warning("AIM BATTERY IS MUTUALLY EXCLUSIVE: the joint cell is empty, ",
            "so the two indicators are anti-correlated by construction and ",
            "the promotion-versus-maintenance contrast is not a behavioural ",
            "test. Check for a priority rule upstream.", call. = FALSE)
  } else {
    cat(sprintf("  Joint cell %d (%.1f%%): multi-response, so the contrast is\n",
                .joint, 100 * .joint / nrow(.ov)))
    cat("  between two OVERLAPPING aims, not a reallocation between",
        "exclusive\n  categories. Say so wherever it is reported.\n")
  }
  
  cat("\n--- Aim battery: item non-response among trainers ---\n")
  .nr <- panel_full[!is.na(panel_full$any_training), ]
  print(as.data.frame(stats::aggregate(
    list(pct_asked = .nr$aim_battery_asked),
    by = list(any_training = .nr$any_training),
    FUN = function(x) round(100 * mean(x), 1))), row.names = FALSE)
  cat("  Among any_training == 1 this should be near 100. Whatever is\n")
  cat("  missing there is TRUE item non-response and stays NA in _z.\n")
  
  cat("\n--- Aim battery: coverage by wave ---\n")
  .aw <- do.call(rbind, lapply(sort(unique(panel_full$wave)), function(w) {
    x <- panel_full[panel_full$wave == w, ]
    data.frame(wave = w,
               pct_aim_asked = round(100 * mean(x$aim_battery_asked == 1L), 1),
               pct_promo_obs = round(100 * mean(!is.na(x$promotion_training)), 1),
               pct_maint_obs = round(100 * mean(!is.na(x$maintenance_training)), 1),
               pct_promo_z   = round(100 * mean(!is.na(x$promotion_training_z)), 1))
  }))
  print(.aw, row.names = FALSE)
  cat("  pct_promo_obs and pct_maint_obs should be EQUAL in every wave, and\n")
  cat("  pct_promo_z far higher, near the module gate. A wave where the two\n")
  cat("  ungated columns differ has one branch missing.\n")
  rm(.ov, .tb, .joint, .nr, .aw)
}

# ---- v21.48 AIM BATTERY: DON'T KNOW AND REFUSED ---------------------
# What the aim fix moved, measured rather than assumed. A trainee who
# answered don't know or refused is now NA on the six aim items instead
# of zero. This counts them, stops if the == 1 rule looks inverted, and
# checks that no one gave a listed aim alongside either flag.
if (all(c("training_aim_nonresponse", "training_aim_dk_raw",
          "training_aim_refused_raw", "aim_battery_asked") %in%
        names(panel_full))) {
  cat("\n--- Aim battery: don't know and refused (v21.48) ---\n")
  .anr  <- panel_full$training_aim_nonresponse
  .aput <- !is.na(.anr)
  if (!any(.aput)) {
    cat("  *** jttrdk and jttrrf did not resolve, so the aim fix did nothing:\n",
        "      a trainee who did not say still scores zero on all six aims.\n",
        "      The arrival contract stops the build on this.\n", sep = "")
  } else {
    .ais  <- .aput & .anr == 1
    .ashr <- sum(.ais) / sum(.aput)
    .araw <- intersect(c("training_aim_general_raw", "training_aim_current_raw",
                         "training_aim_future_raw", "training_aim_getstarted_raw",
                         "training_aim_maintain_raw", "training_aim_safety_raw"),
                       names(panel_full))
    .aany <- if (length(.araw))
      rowSums(as.matrix(as.data.frame(panel_full[, .araw, drop = FALSE])) == 1,
              na.rm = TRUE) > 0 else rep(FALSE, nrow(panel_full))
    cat(sprintf("  Flags observed on %s person-waves.\n",
                format(sum(.aput), big.mark = ",")))
    cat(sprintf("  Don't know %s, refused %s, either %s (%.2f%%).\n",
                format(sum(panel_full$training_aim_dk_raw == 1, na.rm = TRUE),
                       big.mark = ","),
                format(sum(panel_full$training_aim_refused_raw == 1,
                           na.rm = TRUE), big.mark = ","),
                format(sum(.ais), big.mark = ","), 100 * .ashr))
    cat(sprintf(paste0("  %s of them had aim items observed. They read zero ",
                       "on all six before\n  this build and read NA now.\n"),
                format(sum(.ais & panel_full$aim_battery_asked == 1L),
                       big.mark = ",")))
    cat(sprintf(paste0("  %s also mention a listed aim. Expect 0, since don't ",
                       "know and refused\n  are exclusive answers. Any other ",
                       "count means the fix also blanks a\n  stated aim for ",
                       "those trainees.\n"),
                format(sum(.ais & .aany), big.mark = ",")))
    if (.ashr > TRAINING_AIM_NONRESPONSE_MAX)
      stop(BUILD_TAG, ": don't know or refused reads as mentioned on ",
           sprintf("%.1f%%", 100 * .ashr), " of the person-waves where ",
           "the flags are observed, against a ceiling of ",
           sprintf("%.0f%%", 100 * TRAINING_AIM_NONRESPONSE_MAX), ". ",
           "That is the signature of an inverted code: the == 1 rule is ",
           "reading 'not mentioned' as 'mentioned' and blanks the aims for ",
           "almost every trainee. Read v216_training_item_codes.csv and the ",
           "value labels of jttrdk and jttrrf before rebuilding.",
           call. = FALSE)
    rm(.ais, .ashr, .araw, .aany)
  }
  rm(.anr, .aput)
}

# ---- v21.48 TRAINING DOSAGE: UNGATED AGAINST GATED ------------------
# The ungated days and courses columns read a missing count as zero; the
# gated twins do not. This shows where the two differ among the employed,
# the sample the training designs use. Hours is the reference row, since
# its gated twin has existed since v21.9.
.dose <- list(days    = c("total_training_days",  "total_training_days_z"),
              courses = c("total_courses",        "total_courses_z"),
              hours   = c("total_training_hours", "total_training_hours_z"))
if (all(c(unlist(.dose), "training_module_asked", "any_training",
          "employed") %in% names(panel_full))) {
  cat("\n--- Training dosage: ungated against gated (v21.48) ---\n")
  .de  <- !is.na(panel_full$employed) & panel_full$employed == 1
  .ask <- panel_full$training_module_asked == 1L
  .tr1 <- .ask & !is.na(panel_full$any_training) & panel_full$any_training == 1
  .tr0 <- .ask & !is.na(panel_full$any_training) & panel_full$any_training == 0
  cat(sprintf(paste0("  Employed person-waves: %s. Never asked the module: %s.\n",
                     "  Those read zero on ungated days and courses and NA on ",
                     "the gated twins.\n"),
              format(sum(.de), big.mark = ","),
              format(sum(.de & !.ask), big.mark = ",")))
  .dt <- do.call(rbind, lapply(names(.dose), function(nm) {
    u <- panel_full[[.dose[[nm]][1]]]
    g <- panel_full[[.dose[[nm]][2]]]
    data.frame(margin           = nm,
               trainee_no_count = sum(.de & .tr1 & is.na(g)),
               override_to_zero = sum(.de & .tr0 & !is.na(u) & u > 0),
               mean_ungated     = round(mean(u[.de], na.rm = TRUE), 3),
               mean_gated       = round(mean(g[.de], na.rm = TRUE), 3),
               pct_gated_obs    = round(100 * mean(!is.na(g[.de])), 1),
               stringsAsFactors = FALSE)
  }))
  print(.dt, row.names = FALSE)
  cat("  trainee_no_count: trained and gave no count. Zero on ungated days\n")
  cat("  and courses, NA on the twins. mean_ungated for hours is a mean over\n")
  cat("  trainees only, because that column was never zero-filled.\n")
  .ov0 <- max(.dt$override_to_zero)
  if (.ov0 > 0) {
    cat(sprintf(paste0("  override_to_zero: up to %s employed person-waves ",
                       "report a positive\n  count while no place-and-time ",
                       "item is mentioned, so any_training is 0.\n  All three ",
                       "twins code them zero, as total_training_hours_z has ",
                       "since\n  v21.9. If the count is material, test 'did ",
                       "not train' on E6\n  (training_any_reported) for all ",
                       "three instead.\n"),
                format(.ov0, big.mark = ",")))
  } else {
    cat("  override_to_zero is 0 for all three: no asked person-wave reports\n")
    cat("  a count without a place-and-time item, so the shared test of\n")
    cat("  'did not train' discards nothing.\n")
  }
  cat("  Re-run the courses dosage estimate on total_courses_z and the days\n")
  cat("  event study on total_training_days_z, and compare.\n")
  rm(.de, .ask, .tr1, .tr0, .dt, .ov0)
}
rm(.dose)

# ---- v21.12b WHICH QUESTIONNAIRE ANSWERED, AND CAN THE PPL FLAGS WORK
#
# Two things the first v21.12 run left as puzzles rather than answers.
#
# ONE. occ2_origin coalesces three sources and occ_origin_source records
# which one answered, but nothing printed it, so the wave-hole sweep
# reported occstat_change as an eight-wave column (waves 12 to 19) with no
# way to see why. pjoo62 and pjoo6s are absent from waves 20 to 24, which
# explains the job-change pair; it does NOT explain the coalesced column,
# because pjoto* and ujljo* resolved in all thirteen waves. Either those
# two are empty, or they are answered by nobody, and a coverage table is
# the difference between knowing and guessing.
#
# TWO. wscplre and wsfplre were the reason this batch was not deferred,
# and they came back observed on 0.1 to 0.2% and 0.7 to 1.5% of
# person-waves. They are follow-ups put only to PPL recipients, so the
# question is what share of RECIPIENTS they characterise, not what share
# of the panel. That is a cross-tabulation and it decides whether the
# ambiguity inside the earnings measure is answerable or merely bounded.
if ("occ_origin_source" %in% names(panel_full)) {
  cat("\n--- Origin occupation: which questionnaire answered (v21.12b) ---\n")
  .os <- panel_full %>%
    dplyr::group_by(wave) %>%
    dplyr::summarise(
      cpq_unemployed  = sum(occ_origin_source == "cpq_unemployed",  na.rm = TRUE),
      npq_not_in_work = sum(occ_origin_source == "npq_not_in_work", na.rm = TRUE),
      cpq_job_change  = sum(occ_origin_source == "cpq_job_change",  na.rm = TRUE),
      any_origin      = sum(!is.na(occ_origin_source)),
      pct_any         = round(100 * mean(!is.na(occ_origin_source)), 2),
      .groups = "drop")
  print(as.data.frame(.os), row.names = FALSE)
  write.csv(.os, .out_path("v2112_occ_origin_source.csv"), row.names = FALSE)
  .tot <- colSums(.os[, c("cpq_unemployed", "npq_not_in_work",
                          "cpq_job_change")])
  cat(sprintf(paste("\n  Totals: CPQ-unemployed %s, NPQ-not-in-work %s,",
                    "CPQ-job-change %s.\n"),
              format(.tot[1], big.mark = ","), format(.tot[2], big.mark = ","),
              format(.tot[3], big.mark = ",")))
  if (.tot[1] + .tot[2] == 0) {
    cat("  *** pjoto* and ujljo* CONTRIBUTE NOTHING. They resolved in all\n",
        "  thirteen waves and answered on no person-wave, so occ2_origin is\n",
        "  the job-change pair alone. Read the value labels before treating\n",
        "  the coalesce as a thirteen-wave series.\n", sep = "")
  } else if (min(.os$pct_any) == 0) {
    cat("  Some waves carry NO origin at all; the coalesced column is not a\n",
        "  thirteen-wave series and must not be averaged into one.\n", sep = "")
  } else {
    # v21.14b. The real defect is a COVERAGE BREAK, not an empty source.
    # pjoo62 and pjoo6s are absent from waves 20 to 24, so the job-change
    # route disappears and coverage of the coalesced column roughly halves
    # at wave 20. The variable is the same variable on both sides; the
    # POPULATION it is observed for is not, and a design that pools across
    # wave 20 compares movers-including-job-changers against movers-
    # excluding-them.
    .pre  <- .os$pct_any[.os$wave <  OCC_ORIGIN_BREAK_WAVE]
    .post <- .os$pct_any[.os$wave >= OCC_ORIGIN_BREAK_WAVE]
    if (length(.pre) && length(.post)) {
      cat(sprintf(paste("\n  Coverage: %.2f%% mean before wave %d, %.2f%%",
                        "after. The job-change route\n  ends at that",
                        "boundary because pjoo62/pjoo6s are absent from",
                        "waves\n  %d onward.\n"),
                  mean(.pre), OCC_ORIGIN_BREAK_WAVE, mean(.post),
                  OCC_ORIGIN_BREAK_WAVE))
      if (mean(.pre) > 1.5 * mean(.post))
        cat("  *** DO NOT POOL ACROSS THIS BOUNDARY without an",
            " occupation-origin\n      regime interaction. The column is",
            " the same column; the people it\n      is observed for are",
            " not.\n", sep = "")
    }
  }
  rm(.os, .tot)
}

if (all(c("ppl_received_fy", "ppl_in_fy_income") %in% names(panel_full))) {
  cat("\n--- Can the PPL inclusion flags do their job? (v21.12b) ---\n")
  .rec <- !is.na(panel_full$ppl_received_fy) & panel_full$ppl_received_fy == 1
  .n_rec <- sum(.rec)
  .n_flag <- sum(.rec & !is.na(panel_full$ppl_in_fy_income))
  
  # v21.19. A PLAUSIBILITY BOUND ON THE COUNT ITSELF, because a figure
  # from this block was carried for four builds and was wrong.
  #
  # The v21.12 run printed 17,847 recipients. The value labels say bnfppl
  # is a clean 0/1 receipt flag, the wave-16 file has it equal to 1 on
  # 2.7% of the observed, and the saved panel has 1,776 recipients, 6.4%
  # of the observed and 1.3% of person-waves. So 17,847 was ten times the
  # truth and nothing in this block noticed, and the conclusion drawn from
  # it - that the inclusion flag characterises 6.7% of recipients and can
  # therefore only BOUND the ambiguity - was the exact reverse of the
  # right one. At 1,776 the flag characterises 68% and the ambiguity is
  # ANSWERABLE.
  #
  # PPL follows a birth. Australia records roughly 300,000 births a year
  # in 25 million people, so about 1.2% of adults have a birth in any
  # year; counting partner pay and a twelve-month financial-year reference
  # window, a receipt rate above about 5% of person-waves is not a survey
  # result, it is an error. This states the bound and fails loudly rather
  # than printing a number that reads like one.
  .rec_rate <- .n_rec / nrow(panel_full)
  cat(sprintf("  Receipt rate: %.2f%% of person-waves (%s of %s).\n",
              100 * .rec_rate, format(.n_rec, big.mark = ","),
              format(nrow(panel_full), big.mark = ",")))
  if (.rec_rate > 0.05) {
    cat("  *** IMPLAUSIBLE. PPL follows a birth, and roughly 1.2% of adults\n")
    cat("      have one in a year. A rate above 5% is a derivation fault,\n")
    cat("      not a finding. Check table(panel$ppl_received_fy) against the\n")
    cat("      raw item in the wave file BEFORE reading anything below.\n")
  } else if (.rec_rate < 0.002) {
    cat("  *** SUSPICIOUSLY LOW. Below 0.2% suggests the item resolved but\n")
    cat("      is barely answered, which is a coverage problem rather than\n")
    cat("      a rate.\n")
  } else {
    cat("  Consistent with the birth rate, so the count can be read.\n")
  }
  cat(sprintf(paste("  %s person-waves report receiving PPL in the financial",
                    "year.\n  Of those, %s (%.1f%%) also answer whether it",
                    "was INCLUDED in the\n  income already reported.\n"),
              format(.n_rec, big.mark = ","), format(.n_flag, big.mark = ","),
              if (.n_rec) 100 * .n_flag / .n_rec else NA_real_))
  if (.n_rec && .n_flag / .n_rec > 0.8) {
    cat("  The flag characterises most recipients, so the ambiguity inside\n")
    cat("  the earnings measure is ANSWERABLE: condition on it.\n")
  } else {
    cat("  *** The flag characterises a MINORITY of recipients, so it cannot\n")
    cat("      resolve the ambiguity for the rest. What it can do is BOUND\n")
    cat("      it: PPL can only contaminate the earnings measure for people\n")
    cat("      who received PPL, and that is the first number above. Report\n")
    cat("      the bound, and do not condition on a flag that is missing for\n")
    cat("      most of the group it describes.\n")
  }
  # v21.12c. A CODING AUDIT CANNOT CONFIRM MEANING, and the first run
  # showed why. ppl_received_fy == 1 landed on 17,847 person-waves, 13.3%
  # of the panel. Australia records roughly 300,000 births a year in 25m
  # people, so about 1.2% of adults have a birth in any year; even
  # counting partner pay, a PPL receipt rate of 13.3% is an order of
  # magnitude too high for the thing the label names.
  #
  # The consequence is not cosmetic. The 6.7% flagged-among-recipients
  # verdict above depends entirely on that denominator: if true receipt
  # were near 1.3%, the same 1,204 flagged rows would be about 69% of
  # recipients and the flag would RESOLVE the ambiguity rather than merely
  # bound it. The two readings point opposite ways.
  #
  # So the item is tested against the event it is supposed to follow. PPL
  # receipt should concentrate on person-waves at or just after a birth.
  # If it is flat across event time, bnfppl is not measuring what the
  # label says and neither the flag verdict nor the bound can be reported.
  if (all(c("person_id", "wave", "had_child_this_wave") %in%
          names(panel_full))) {
    .bw <- panel_full %>%
      dplyr::filter(had_child_this_wave == 1) %>%
      dplyr::group_by(person_id) %>%
      dplyr::summarise(bwv = min(wave), .groups = "drop")
    .ck <- panel_full %>%
      dplyr::inner_join(.bw, by = "person_id") %>%
      dplyr::mutate(k = wave - bwv) %>%
      dplyr::filter(k >= -3, k <= 3, !is.na(ppl_received_fy)) %>%
      dplyr::group_by(k) %>%
      dplyr::summarise(pct = round(100 * mean(ppl_received_fy == 1), 1),
                       n = dplyr::n(), .groups = "drop")
    if (nrow(.ck)) {
      cat("\n  PPL receipt by event time, as a meaning check:\n")
      print(as.data.frame(.ck), row.names = FALSE)
      .peak <- .ck$pct[.ck$k %in% c(0, 1)]
      .base <- .ck$pct[.ck$k %in% c(-3, -2)]
      if (length(.peak) && length(.base) &&
          max(.peak) < 2 * max(c(.base, 0.001))) {
        cat("  *** FLAT ACROSS THE BIRTH. bnfppl is not behaving like paid\n")
        cat("      parental leave: receipt should spike at k = 0 or +1 and\n")
        cat("      does not. Read its value labels before ppl_received_fy,\n")
        cat("      the 6.7% figure above, or any bound derived from it is\n")
        cat("      reported. The most likely reading is that F61a is a\n")
        cat("      battery row about transfer income generally rather than\n")
        cat("      about PPL specifically.\n")
      } else {
        cat("  Receipt peaks around the birth, so the item behaves like\n")
        cat("  paid parental leave and the denominator above can be used.\n")
      }
      rm(.peak, .base)
    }
    rm(.bw, .ck)
  }
  # ---- v21.13b WHAT IS bnfppl ACTUALLY MEASURING? --------------------
  # The event-time check came back FLAT: 8.5% at k = -3 rising only to
  # 9.9% at k = 0. PPL receipt should be near zero away from a birth and
  # large at it, so the item is not measuring what its label names and no
  # design should use it as receipt.
  #
  # The how-answered flag is what can say WHY. HILDA records for each
  # income component whether the value was self-reported, imputed from a
  # model, or derived from another source. A receipt indicator that
  # includes imputed and derived cases will be far commoner than the
  # underlying event and will not track it, which is exactly the shape
  # above. This cross-tabulation either confirms that or rules it out.
  if ("ppl_howanswered_fy" %in% names(panel_full)) {
    cat("\n  How the PPL financial-year value was answered:\n")
    .ha <- panel_full[!is.na(panel_full$ppl_howanswered_fy),
                      c("ppl_howanswered_fy", "ppl_received_fy")]
    .tb <- as.data.frame(table(how = .ha$ppl_howanswered_fy,
                               received = .ha$ppl_received_fy))
    print(.tb, row.names = FALSE)
    .self <- sum(.ha$ppl_received_fy == 1 & .ha$ppl_howanswered_fy == 1,
                 na.rm = TRUE)
    cat(sprintf(paste("\n  Of %s person-waves flagged as receiving, %s carry",
                      "how-answered = 1.\n"),
                format(sum(.ha$ppl_received_fy == 1, na.rm = TRUE),
                       big.mark = ","), format(.self, big.mark = ",")))
    # v21.19a. THE VALUE LABELS HAVE NOW BEEN READ and this block was
    # asserting a hypothesis that is false.
    #
    #   bnfpplh  1 = Annual Amount
    #            2 = Average per fortnight
    #
    # It records the RESPONSE FORMAT, not whether a value was imputed.
    # HILDA's imputation companions carry an "f" suffix per the manual's
    # Table 4.24; "h" is How Answered and is a different family. So this
    # cross-tabulation cannot diagnose imputation and never could, and
    # the text that used to sit here told you to read it as if it could.
    #
    # It is still worth printing, for a smaller reason: a shift in the
    # annual-against-fortnightly split across waves would mean the amount
    # variables change basis, and bnfppla explicitly excludes fortnightly
    # responses.
    cat("  bnfpplh is the RESPONSE FORMAT: 1 = annual amount, 2 = average\n")
    cat("  per fortnight. It is NOT an imputation flag; HILDA marks those\n")
    cat("  with an f suffix. Read this only for a change in the split\n")
    cat("  across waves, which would move the basis of bnfppla, and that\n")
    cat("  variable excludes fortnightly responses by construction.\n")
    rm(.ha, .tb, .self)
  }
  
  # THE ENTITLEMENT PAIR IS A SEPARATE QUESTION AND IS NOT CONDEMNED BY
  # THE ABOVE. jowppml and jowpppl are SCQ D3a/D3b, put to EMPLOYEES about
  # their job, and coverage of 51-61% and 47-57% is what an employee-routed
  # SCQ item should look like on a panel that is about 81% employed with an
  # SCQ return rate near 88%. Entitlement is a JOB characteristic, so it
  # should NOT spike at a birth and the event-time test does not apply to
  # it. Check it against EMPLOYMENT instead.
  if (all(c("ppl_entitled_mat", "employed") %in% names(panel_full))) {
    .e <- panel_full[!is.na(panel_full$ppl_entitled_mat) &
                       !is.na(panel_full$employed), ]
    if (nrow(.e)) {
      .by_emp <- tapply(.e$ppl_entitled_mat, .e$employed, mean)
      cat(sprintf(paste("\n  Paid maternity entitlement, by employment:",
                        "%.1f%% of the employed and\n  %.1f%% of the",
                        "non-employed report it. An employee-routed job\n",
                        " characteristic should be concentrated on the",
                        "employed; if it is\n  not, this item is not what",
                        "its label says either.\n"),
                  100 * .by_emp[["1"]], 100 * .by_emp[["0"]]))
      rm(.by_emp)
    }
    rm(.e)
  }
  .v21_facts$ppl_flags <- list(n_recipients_fy = .n_rec,
                               n_flagged = .n_flag,
                               pct_flagged = if (.n_rec) 100 * .n_flag / .n_rec
                               else NA_real_)
  rm(.rec, .n_rec, .n_flag)
}

# ---- v21.16 THE BORJAS-HAMERMESH TEST, ON ehtjb ---------------------
#
# Borjas and Hamermesh (2026, Labour Economics 102) compare retrospective
# annual recall of work time in the CPS-ASEC against the same person's
# CONTEMPORANEOUS monthly reports in the Basic CPS. Roughly 8% of
# respondents report weeks worked that contradict their own work history,
# the rate exceeds 50% among people who move in and out of the workforce,
# it is higher for women than for men in every year, and it TRENDS. When
# they correct for it the gender wage gap narrows by four more log points
# than the uncorrected series shows.
#
# That is this panel's experience problem, in another survey, with the
# discrepancy measured rather than bounded. actual_experience is ehtjb, a
# RETROSPECTIVE report of years in paid work, and experience_years is
# actual_experience wherever ehtjb resolves. The restriction on it has
# been justified by an entry-cohort gradient; this is the direct test.
#
# THE TEST HILDA MAKES POSSIBLE. ehtjb is asked EVERY wave, so d_paid_years
# above is the respondent's own year-on-year revision of a retrospective
# quantity, and employment is recorded contemporaneously at each
# interview. Between two consecutive waves the increment should be about
# 1 for someone employed throughout and about 0 for someone not employed.
# A person employed at both interviews who reports NO extra year, or not
# employed at either who reports a FULL one, is the HILDA version of the
# logical inconsistency they count.
#
# Reported, not corrected. Their point is that the error is nonrandom and
# gendered, and a rate that differs by sex is a reason to distrust a level
# and to prefer the flow, which is what the interruption restriction
# already says. This measures how much that restriction is worth.
if (all(c("d_paid_years", "employed", "female", "d_wave") %in%
        names(panel_full))) {
  cat("\n--- ehtjb against contemporaneous employment (v21.16) ---\n")
  .bh <- panel_full %>%
    dplyr::arrange(person_id, wave) %>%
    dplyr::group_by(person_id) %>%
    dplyr::mutate(emp_lag = dplyr::lag(employed)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(d_paid_years), !is.na(employed), !is.na(emp_lag),
                  !is.na(d_wave), d_wave == 1L, !is.na(female))
  if (nrow(.bh) > 100) {
    .bh <- .bh %>% dplyr::mutate(
      both_emp  = employed == 1 & emp_lag == 1,
      both_non  = employed == 0 & emp_lag == 0,
      # Employed at both interviews and the retrospective stock does not
      # move: the year of work the panel watched happen is not in the
      # respondent's own history.
      miss_year = both_emp & d_paid_years < 0.5,
      # Not employed at either and a full year appears anyway.
      ghost_year = both_non & d_paid_years > 0.5,
      # Backwards: the stock FALLS. Years in paid work cannot decrease.
      negative  = d_paid_years < -0.05)
    .tab <- .bh %>% dplyr::group_by(female) %>%
      dplyr::summarise(
        n = dplyr::n(),
        pct_miss_year  = round(100 * sum(miss_year) / pmax(sum(both_emp), 1), 1),
        pct_ghost_year = round(100 * sum(ghost_year) / pmax(sum(both_non), 1), 1),
        pct_negative   = round(100 * mean(negative), 2),
        .groups = "drop") %>%
      dplyr::mutate(sex = ifelse(female == 1, "Women", "Men"))
    cat("  pct_miss_year : employed at BOTH interviews, ehtjb does not rise\n")
    cat("  pct_ghost_year: employed at NEITHER, ehtjb rises a full year\n")
    cat("  pct_negative  : ehtjb FALLS, which is impossible\n\n")
    print(as.data.frame(.tab[, c("sex", "n", "pct_miss_year",
                                 "pct_ghost_year", "pct_negative")]),
          row.names = FALSE)
    write.csv(.tab, .out_path("v2116_ehtjb_consistency.csv"), row.names = FALSE)
    .gap <- abs(diff(.tab$pct_miss_year))
    cat(sprintf(paste("\n  The sex gap in pct_miss_year is %.1f points. Borjas",
                      "and Hamermesh\n  find the analogous discrepancy",
                      "higher for women in every year from\n  1978 to 2018,",
                      "and correcting for it narrows the gender wage gap by\n",
                      " four log points more than the uncorrected series",
                      "shows.\n"), .gap))
    # v21.16a. DO NOT ISSUE THE POOLED VERDICT BEFORE THE EVENT-TIME
    # TABLE, because on the v21.16 run it contradicted it.
    #
    # Pooled: men 0.8%, women 1.0%, gap 0.2 points, and this block printed
    # "not differentially mismeasured". Three lines later the event-time
    # table read 4.0% for women at k = 0 against 0.45% at their own k = -1
    # and -2, a ninefold spike, and 0.3% for men at the same k. The
    # discrepancy is not absent; it is concentrated in one wave and the
    # pooled average divides it by thirteen.
    #
    # Borjas and Hamermesh make exactly this point: their overall rate is
    # about 8% and it exceeds 50% among people MOVING in and out of work.
    # An average over a population most of whom are not moving tells you
    # about the ones who are not.
    cat("  Pooled gap: ", sprintf("%.1f", .gap), " points. HOLD THIS ",
        "VERDICT until the event-time\n  table below: a discrepancy ",
        "concentrated in one wave is divided by\n  thirteen here and can ",
        "read as absent when it is not.\n", sep = "")
    # And by event time, because their >50% figure is for people MOVING in
    # and out of work, which is what a birth does.
    if ("had_child_this_wave" %in% names(panel_full)) {
      .bwx <- panel_full %>%
        dplyr::filter(had_child_this_wave == 1) %>%
        dplyr::group_by(person_id) %>%
        dplyr::summarise(bwv = min(wave), .groups = "drop")
      .ek <- .bh %>% dplyr::inner_join(.bwx, by = "person_id") %>%
        dplyr::mutate(k = wave - bwv) %>%
        dplyr::filter(k >= -2, k <= 3, both_emp) %>%
        dplyr::group_by(k, female) %>%
        dplyr::summarise(pct = round(100 * mean(miss_year), 1),
                         n = dplyr::n(), .groups = "drop")
      if (nrow(.ek)) {
        cat("\n  Same rate by event time, employed at both interviews:\n")
        print(as.data.frame(.ek), row.names = FALSE)
        cat("  Borjas and Hamermesh put the discrepancy above 50% for people\n")
        cat("  moving in and out of work. A birth is that transition, so a\n")
        cat("  rise here is the mechanism appearing where it matters most.\n")
        # The verdict belongs HERE, on the k = 0 contrast, not on the
        # pooled average above.
        .w0 <- .ek$pct[.ek$k == 0 & .ek$female == 1]
        .m0 <- .ek$pct[.ek$k == 0 & .ek$female == 0]
        .wb <- mean(.ek$pct[.ek$k < 0 & .ek$female == 1])
        if (length(.w0) && length(.m0) && is.finite(.wb) && .wb > 0) {
          cat(sprintf(paste("\n  At k = 0: women %.1f%%, men %.1f%%. Women's",
                            "own pre-birth baseline is\n  %.2f%%, so the",
                            "birth-wave rate is %.0f times it and %.0f times",
                            "the fathers'\n  rate in the same wave.\n"),
                      .w0, .m0, .wb, .w0 / .wb,
                      if (.m0 > 0) .w0 / .m0 else NA_real_))
          if (.w0 > 3 * .wb) {
            cat("  *** GENDERED AND EVENT-TIME CONCENTRATED. Mothers employed\n")
            cat("      at both interviews record no extra year of paid work\n")
            cat("      at the birth wave, and fathers do not. The likeliest\n")
            cat("      reading is parental leave: time on leave is not being\n")
            cat("      counted as time in paid work by some respondents.\n")
            cat("      That answers 01's question 2, second half, which has\n")
            cat("      been open since the first discovery run, and it does\n")
            cat("      so for a MINORITY: most mothers do count it.\n")
            cat("      Any experience-based design should carry this, and it\n")
            cat("      is a reason to prefer the flow over the level at\n")
            cat("      exactly the wave the event study reads.\n")
          }
          rm(.w0, .m0, .wb)
        }
      }
      rm(.bwx, .ek)
    }
    rm(.tab, .gap)
  } else {
    cat("  Too few consecutive-wave pairs with both employment and the\n")
    cat("  increment observed; test skipped.\n")
  }
  rm(.bh)
}

# ---- v21.26 HOW MUCH DID THE MISSING NEGATIVE HALF MOVE? -------------
# v21.25 read the "Positive values" label as censoring and measured the
# pile-up at zero. The training deck says otherwise: it is the positive
# HALF of a p/n pair and the loader was dropping the other half. This
# now measures the correction instead - how many person-waves carry a
# negative half, and whether they are concentrated in self-employment,
# which is the group a loss-making business puts there.
if ("fy_income_disposable" %in% names(panel_full)) {
  cat("\n--- Positive-only censoring in the FY income aggregates ---\n")
  .z <- panel_full$fy_income_disposable
  .obs <- !is.na(.z)
  .neg <- if ("fy_disposable_negative" %in% names(panel_full))
    !is.na(panel_full$fy_disposable_negative) &
    panel_full$fy_disposable_negative > 0 else rep(FALSE, nrow(panel_full))
  cat(sprintf("  fy_income_disposable: %.1f%% of the observed sit at 0, and\n",
              100 * mean(.z[.obs] == 0)))
  cat(sprintf("  %s person-waves (%.2f%%) carry a NEGATIVE half that v21.25 and\n  earlier recorded as zero.\n",
              format(sum(.neg), big.mark = ","), 100 * mean(.neg)))
  if (any(.neg))
    cat(sprintf("    median loss among them: $%s\n",
                format(round(stats::median(
                  panel_full$fy_disposable_negative[.neg])), big.mark = ",")))
  if ("is_self_employed" %in% names(panel_full)) {
    .se <- panel_full$is_self_employed
    .a <- mean(.neg[!is.na(.se) & .se == 1])
    .b <- mean(.neg[!is.na(.se) & .se == 0])
    cat(sprintf("    negative half: self-employed %.2f%% against employees %.2f%%",
                100 * .a, 100 * .b))
    if (is.finite(.a) && is.finite(.b) && .a > 2 * .b) {
      cat(" -- CONCENTRATED.\n")
      cat("    Losses are concentrated in self-employment, as expected, and\n")
      cat("    self-employment among mothers trebles after the birth. So the\n")
      cat("    pre-v21.26 error was not orthogonal to the treatment: it set\n")
      cat("    those person-waves to zero disproportionately among mothers\n")
      cat("    after the event. Any income result estimated before this\n")
      cat("    build should be re-run.\n")
    } else {
      cat(" -- not concentrated.\n")
    }
    rm(.se, .a, .b)
  }
  rm(.z, .obs)
}

# ---- v21.23 THE SALARY-SACRIFICE GAP, AND WHETHER IT MOVES AT k = 0 --
# The reason wsces was added rather than noted. If sacrifice is small and
# stable, wscei is fine and the robustness arm is a footnote. If it moves
# around the birth, the main outcome is contaminated at exactly the wave
# the event study reads, and in the direction that understates.
if (all(c("salary_sacrificed_weekly", "weekly_earnings") %in%
        names(panel_full))) {
  cat("\n--- Salary sacrifice against the wage measure (v21.23) ---\n")
  .ss <- panel_full$salary_sacrificed_weekly
  .nz <- !is.na(.ss) & .ss > 0
  cat(sprintf("  Observed on %s person-waves; nonzero on %s (%.1f%% of those\n  observed).\n",
              format(sum(!is.na(.ss)), big.mark = ","),
              format(sum(.nz), big.mark = ","),
              100 * sum(.nz) / max(sum(!is.na(.ss)), 1)))
  if (any(.nz)) {
    cat(sprintf("  Among sacrificers, median $%.0f a week, %.1f%% of the\n  base wage.\n",
                stats::median(.ss[.nz]),
                100 * stats::median(.ss[.nz] /
                                      pmax(panel_full$weekly_earnings[.nz], 1))))
  }
  if ("had_child_this_wave" %in% names(panel_full)) {
    .bw <- panel_full %>%
      dplyr::filter(had_child_this_wave == 1) %>%
      dplyr::group_by(person_id) %>%
      dplyr::summarise(bwv = min(wave), .groups = "drop")
    .ek <- panel_full %>%
      dplyr::inner_join(.bw, by = "person_id") %>%
      dplyr::mutate(k = wave - bwv) %>%
      dplyr::filter(k >= -2, k <= 2, !is.na(salary_sacrificed_weekly),
                    !is.na(female)) %>%
      dplyr::group_by(k, female) %>%
      dplyr::summarise(pct_sacrificing = round(
        100 * mean(salary_sacrificed_weekly > 0), 1),
        n = dplyr::n(), .groups = "drop")
    if (nrow(.ek)) {
      cat("\n  Share sacrificing, by event time and sex:\n")
      print(as.data.frame(.ek), row.names = FALSE)
      .w <- .ek$pct_sacrificing[.ek$female == 1]
      .kk <- .ek$k[.ek$female == 1]
      if (length(.w) >= 4 && all(c(-1, 0) %in% .kk)) {
        .drop <- .w[.kk == -1] - .w[.kk == 0]
        cat(sprintf(paste("\n  Women: %.1f%% at k = -1 to %.1f%% at k = 0,",
                          "a fall of %.1f points.\n"),
                    .w[.kk == -1], .w[.kk == 0], .drop))
        if (abs(.drop) > 2)
          cat("  *** MOVES AT THE BIRTH. Anyone who stops sacrificing shows a\n",
              "      RISE in wscei with no change in total compensation, so\n",
              "      the birth-wave coefficient on the wscei-based wage is\n",
              "      biased TOWARD ZERO. Re-run the event study on\n",
              "      weekly_earnings_inc_ss and report the difference.\n",
              sep = "")
        else
          cat("  Stable across the birth, so wscei is not contaminated at\n",
              "  k = 0 and the inclusive measure is a footnote.\n", sep = "")
      }
      rm(.w, .kk)
    }
    rm(.bw, .ek)
  }
  rm(.ss, .nz)
}

# ---- v21.32 NON-CASH BENEFITS: SIZE, GRADIENT, AND DIRECTION --------
# Four questions, in the order that decides how much of the paper this
# touches. None is answered by asserting that in-kind pay favours
# fathers; the whole point of reading nbcm was to stop asserting it.
#
#   ONE   how big is it, and on how many person-waves
#   TWO   is it gendered, and does the gradient move around a birth
#   THREE does nbcmany agree with the DV about who has a benefit
#   FOUR  does including it WIDEN the father-mother gap
#
# Four is the one a referee asks. If the compensation gap is wider than
# the wage gap, the omission was understating the penalty and the arm
# belongs in the text. If they agree, this is a footnote. The loader
# reports the answer and does not decide which.
if ("noncash_total_weekly" %in% names(panel_full)) {
  cat("\n--- Employer non-cash benefits (v21.32) ---\n")
  .nc  <- panel_full$noncash_total_weekly
  .obs <- !is.na(.nc)
  .pos <- .obs & .nc > 0
  cat(sprintf("  Observed on %s person-waves (%.1f%%); positive on %s (%.1f%%\n  of observed).\n",
              format(sum(.obs), big.mark = ","),
              100 * mean(.obs),
              format(sum(.pos), big.mark = ","),
              100 * sum(.pos) / max(sum(.obs), 1)))
  if (any(.pos))
    cat(sprintf("  Among recipients, median $%.0f a week; mean $%.0f.\n",
                stats::median(.nc[.pos]), mean(.nc[.pos])))
  
  # ---- the main-job / all-jobs split, so the base is visible ---------
  if ("noncash_other_jobs_weekly" %in% names(panel_full)) {
    .oj <- panel_full$noncash_other_jobs_weekly
    if (any(!is.na(.oj))) {
      cat(sprintf(paste0("  Other-jobs component: positive on %s person-wave(s), ",
                         "%.2f%% of\n  the total value. The main-job base is ",
                         "nearly the whole of it.\n"),
                  format(sum(!is.na(.oj) & .oj > 0), big.mark = ","),
                  100 * sum(.oj, na.rm = TRUE) / max(sum(.nc, na.rm = TRUE), 1)))
    } else {
      cat("  nbco/nbcoi did not resolve, so noncash_total_weekly is the\n")
      cat("  MAIN JOB alone and is not on the same base as wscei. Say so\n")
      cat("  wherever the compensation measure is reported.\n")
    }
    rm(.oj)
  }
  
  # ---- TWO: the gradient, among the employed ------------------------
  # Restricted to the employed on purpose. Pooling in the non-employed
  # zeros would report a sex difference in EMPLOYMENT as a sex difference
  # in benefits, which is the composition trap the training block spent
  # six revisions learning.
  if (all(c("employed", "female") %in% names(panel_full))) {
    .g <- panel_full %>%
      dplyr::filter(employed == 1, !is.na(female), !is.na(noncash_total_weekly)) %>%
      dplyr::group_by(female) %>%
      dplyr::summarise(
        n            = dplyr::n(),
        pct_any      = round(100 * mean(noncash_total_weekly > 0), 1),
        mean_value   = round(mean(noncash_total_weekly), 2),
        med_if_pos   = round(stats::median(
          noncash_total_weekly[noncash_total_weekly > 0]), 1),
        pct_of_comp  = round(100 * mean(noncash_share_of_comp, na.rm = TRUE), 2),
        .groups = "drop")
    if (nrow(.g) == 2) {
      cat("\n  Among the employed, by sex (female = 1):\n")
      print(as.data.frame(.g), row.names = FALSE)
      .dm <- .g$mean_value[.g$female == 0] - .g$mean_value[.g$female == 1]
      cat(sprintf(paste0("  Men hold $%.2f a week more in non-cash pay than ",
                         "women.\n"), .dm))
      if (.dm > 0)
        cat("  That is the sign the omission argument predicts: leaving it\n",
            "  out of the wage measure removes a component men have more\n",
            "  of, which UNDERSTATES the gap. Size is question four.\n",
            sep = "")
      else
        cat("  The sign runs the OTHER way, so the omission was not\n",
            "  understating the gap and the Stage 6 argument does not\n",
            "  hold on this panel. Report it as measured.\n", sep = "")
      rm(.dm)
    }
    rm(.g)
  }
  
  # ---- and whether it moves at the birth ----------------------------
  if (all(c("had_child_this_wave", "female", "employed") %in% names(panel_full))) {
    .bw <- panel_full %>%
      dplyr::filter(had_child_this_wave == 1) %>%
      dplyr::group_by(person_id) %>%
      dplyr::summarise(bwv = min(wave), .groups = "drop")
    .ek <- panel_full %>%
      dplyr::inner_join(.bw, by = "person_id") %>%
      dplyr::mutate(k = wave - bwv) %>%
      dplyr::filter(k >= -2, k <= 2, employed == 1,
                    !is.na(noncash_total_weekly), !is.na(female)) %>%
      dplyr::group_by(k, female) %>%
      dplyr::summarise(pct_any = round(100 * mean(noncash_total_weekly > 0), 1),
                       mean_val = round(mean(noncash_total_weekly), 2),
                       n = dplyr::n(), .groups = "drop")
    if (nrow(.ek)) {
      cat("\n  Non-cash benefits by event time and sex (employed only):\n")
      print(as.data.frame(.ek), row.names = FALSE)
      .wv <- .ek$mean_val[.ek$female == 1]; .wk <- .ek$k[.ek$female == 1]
      if (length(.wv) >= 4 && all(c(-1, 0) %in% .wk)) {
        .fall <- .wv[.wk == -1] - .wv[.wk == 0]
        cat(sprintf(paste0("\n  Women: $%.2f at k = -1 to $%.2f at k = 0, a ",
                           "change of $%+.2f (%+.1f%%).\n"),
                    .wv[.wk == -1], .wv[.wk == 0], -.fall,
                    -100 * .fall / max(.wv[.wk == -1], 1e-8)))
        # BRANCH, DO NOT NARRATE. The first draft of this block printed
        # "a fall here is a fall in total compensation" unconditionally,
        # so it asserted a direction on a +$0.02 change and survived a
        # first reading of the output. That is the same class of error as
        # v21.25 reading a label as a description, in a diagnostic whose
        # whole purpose is to stop the paper asserting this direction.
        .base <- max(.wv[.wk == -1], 1e-8)
        if (.fall > 0.05 * .base) {
          cat("  *** FALLS AT THE BIRTH. A fall in TOTAL COMPENSATION that\n",
              "      the wscei series cannot see at all, so it is ADDITIVE\n",
              "      to the measured penalty rather than a correction to\n",
              "      it. Report it beside the wage result.\n", sep = "")
        } else if (.fall < -0.05 * .base) {
          cat("  *** RISES AT THE BIRTH, which the omission argument does\n",
              "      not predict. Check the employed-only restriction first:\n",
              "      if mothers who keep working after a birth are selected\n",
              "      toward better jobs, this is composition and not a gain\n",
              "      in benefits.\n", sep = "")
        } else {
          cat("  Flat across the birth. Non-cash pay is not moving at k = 0,\n",
              "  so it contributes to the LEVEL gap and not to the event\n",
              "  study, and the compensation arm is a robustness check\n",
              "  rather than a second result.\n", sep = "")
        }
        rm(.base)
      }
      rm(.wv, .wk)
    }
    rm(.bw, .ek)
  }
  
  # ---- THREE: does the indicator agree with the value? --------------
  if ("noncash_any" %in% names(panel_full) &&
      any(!is.na(panel_full$noncash_any))) {
    .x <- panel_full$noncash_any
    .y <- panel_full$noncash_any_value
    .both <- !is.na(.x) & !is.na(.y)
    if (any(.both)) {
      .agree <- mean(.x[.both] == .y[.both])
      cat(sprintf(paste0("\n  nbcmany against the DV: agree on %.1f%% of the ",
                         "%s person-waves\n  where both are observed.\n"),
                  100 * .agree, format(sum(.both), big.mark = ",")))
      print(table(nbcmany = .x[.both], value_positive = .y[.both]))
      if (.agree < 0.98)
        warning(BUILD_TAG, ": nbcmany and nbcmi > 0 disagree on ",
                sprintf("%.1f%%", 100 * (1 - .agree)),
                " of person-waves where both are observed. They measure ",
                "different things or the == 1 rule is wrong. Prefer ",
                "noncash_any_value and read v2112_new_item_codes.csv.",
                call. = FALSE)
      rm(.agree)
    }
    rm(.x, .y, .both)
  } else {
    cat("\n  nbcmany did not resolve on this release, so noncash_any is NA\n")
    cat("  throughout and noncash_any_value carries the has-any margin.\n")
    cat("  That is the reason it was built from the DV rather than the\n")
    cat("  indicator; nothing downstream needs changing.\n")
  }
  
  # ---- FOUR: does it widen the gap? ---------------------------------
  # Raw and descriptive: a difference in means among employed parents,
  # not an estimate. It is enough to say whether the arm is worth
  # running, which is all a loader should be deciding.
  if (all(c("ln_hourly_wage_real", "ln_hourly_compensation_real",
            "female", "ever_parent") %in% names(panel_full))) {
    .cmp <- panel_full %>%
      dplyr::filter(ever_parent == 1, !is.na(female),
                    !is.na(ln_hourly_wage_real),
                    !is.na(ln_hourly_compensation_real))
    if (nrow(.cmp) > 1000) {
      .gw <- mean(.cmp$ln_hourly_wage_real[.cmp$female == 0]) -
        mean(.cmp$ln_hourly_wage_real[.cmp$female == 1])
      .gc <- mean(.cmp$ln_hourly_compensation_real[.cmp$female == 0]) -
        mean(.cmp$ln_hourly_compensation_real[.cmp$female == 1])
      cat(sprintf(paste0("\n  Raw father-mother gap on %s person-waves with ",
                         "BOTH observed:\n    ln wage         %.4f\n",
                         "    ln compensation %.4f\n    difference      ",
                         "%+.4f (%.1f%% of the wage gap)\n"),
                  format(nrow(.cmp), big.mark = ","), .gw, .gc, .gc - .gw,
                  100 * (.gc - .gw) / max(abs(.gw), 1e-8)))
      if (.gc > .gw)
        cat("  *** WIDER on compensation. The wage measure was understating\n",
            "      the gap, in the direction v21.23 found for sacrifice.\n",
            "      Re-run the event study on ln_hourly_compensation_real\n",
            "      and report both series.\n", sep = "")
      else
        cat("  Not wider. Including non-cash pay does not enlarge the raw\n",
            "  gap, so the omission was not biasing it downward and the\n",
            "  arm is a footnote with this number in it.\n", sep = "")
      rm(.gw, .gc)
    }
    # The sample cost of the identical trimming bounds, so the comparison
    # above is read against the right denominator.
    .nw <- sum(!is.na(panel_full$ln_hourly_wage_real))
    .nc2 <- sum(!is.na(panel_full$ln_hourly_compensation_real))
    cat(sprintf(paste0("  Wage series %s person-waves, compensation series ",
                       "%s (%+d).\n"),
                format(.nw, big.mark = ","), format(.nc2, big.mark = ","),
                .nc2 - .nw))
    rm(.cmp, .nw, .nc2)
  }
  
  # ---- v21.43 THE COST ITEM: IS THE FEE MARGIN USABLE? ---------------
  # jttrcst is the only item in the training module that speaks to FEES.
  # Everything else in the funding split separates training by place and by
  # time. So this block asks the one question that decides whether the
  # paper's financing claim can be made literally rather than by proxy: is
  # there enough of it, and does it move.
  #
  # THE ANSWER IS PRINTED, NOT ASSUMED, because the temptation with a newly
  # available item is to build a design on it before checking the base. The
  # aim battery is the cautionary case: observed on about a quarter of
  # person-waves by construction, and the conditional dosage regression
  # built on the same footing had to be dropped at an MDE of 21.1 hours
  # against a base of 17.9.
  if (all(c("training_self_funded_z", "training_module_asked")
          %in% names(panel_full))) {
    .sf <- panel_full$training_self_funded_z
    cat("\n--- Self-funded training, the fee item (v21.43) ---\n")
    cat(sprintf("  gated column observed on %s person-waves (%.1f%%); positive on %s.\n",
                format(sum(!is.na(.sf)), big.mark = ","),
                100 * mean(!is.na(.sf)),
                format(sum(!is.na(.sf) & .sf > 0), big.mark = ",")))
    if ("any_training" %in% names(panel_full)) {
      .tr <- !is.na(panel_full$any_training) & panel_full$any_training == 1
      .an <- .tr & !is.na(panel_full$training_self_funded)
      cat(sprintf("  among trainees: %s answered, %.1f%% contributed to cost.\n",
                  format(sum(.an), big.mark = ","),
                  100 * mean(panel_full$training_self_funded[.an])))
      rm(.tr, .an)
    }
    # The event-time cells are what a design would actually rest on, and
    # they are the number to look at before writing anything.
    if (all(c("had_child_this_wave", "female", "employed") %in% names(panel_full))) {
      .bw <- panel_full %>%
        dplyr::filter(had_child_this_wave == 1) %>%
        dplyr::group_by(person_id) %>%
        dplyr::summarise(bwv = min(wave), .groups = "drop")
      .ek <- panel_full %>%
        dplyr::inner_join(.bw, by = "person_id") %>%
        dplyr::mutate(k = wave - bwv) %>%
        dplyr::filter(k >= -2, k <= 2, employed == 1, !is.na(female),
                      !is.na(training_self_funded_z)) %>%
        dplyr::group_by(k, female) %>%
        dplyr::summarise(n = dplyr::n(),
                         pct = round(100 * mean(training_self_funded_z > 0), 1),
                         .groups = "drop")
      if (nrow(.ek)) {
        cat("\n  Gated self-funding by event time and sex (employed only):\n")
        print(as.data.frame(.ek), row.names = FALSE)
        .min <- min(.ek$n)
        cat(sprintf("  Thinnest cell: %s person-waves.\n",
                    format(.min, big.mark = ",")))

        # v21.43b. THE FEMALE-TO-MALE CELL RATIO, PRINTED, because it is
        # the reason the summary below excludes the birth wave and because
        # it is a finding in its own right.
        #
        # At k = 0 every treated person is present by construction, so the
        # cell should PEAK for both sexes. On the v21.43a run it did for
        # men - 1,800, the largest of five - and for women it was 1,097,
        # the SMALLEST. Men +29 per cent against k = -1, women -20 per
        # cent, in opposite directions at the one horizon the test is
        # built around.
        #
        # That is the employment margin. This block conditions on employed
        # == 1, and employment is what collapses for women at the birth
        # wave, so the composition of the female cell changes exactly
        # where the comparison looks. Section 4.3 of the paper documents
        # the same trap for the module gate: asked with probability one in
        # the wave before, 0.325 at the birth wave. Gating the column
        # fixed that for the module; conditioning on employment
        # reintroduces it at one horizon.
        .rat <- merge(
          .ek[.ek$female == 0, c("k", "n")],
          .ek[.ek$female == 1, c("k", "n")], by = "k", suffixes = c("_m", "_w"))
        if (nrow(.rat)) {
          .rat$ratio <- round(.rat$n_w / .rat$n_m, 2)
          cat("\n  Cell counts by sex, and the ratio:\n")
          print(as.data.frame(.rat), row.names = FALSE)
          .r0 <- .rat$ratio[.rat$k == 0]
          .rp <- .rat$ratio[.rat$k < 0]
          if (length(.r0) && length(.rp) && .r0 < 0.85 * min(.rp))
            cat(sprintf(paste0("  The k = 0 ratio (%.2f) is well below the ",
                               "pre-birth ratios (%s). Women leave\n  ",
                               "employment at the birth wave, so that cell ",
                               "is composed differently\n  from every other ",
                               "and is excluded from the summary below.\n"),
                        .r0, paste(sprintf("%.2f", .rp), collapse = ", ")))
          rm(.r0, .rp)
        }
        rm(.rat)
        if (.min < 200)
          cat("  *** TOO THIN FOR AN EVENT STUDY on this margin. Report the\n",
              "      level share among trainees as a measurement fact and do\n",
              "      NOT build a birth-wave design on it. The paper's\n",
              "      financing split stays on the time margin, and the fee\n",
              "      item is a descriptive complement.\n", sep = "")
        else
          cat("  Cells are large enough to look at.\n")

        # v21.43a. TEST THE DYNAMIC, do not tell the reader to.
        #
        # The first version of this block stopped at "check the MDE before
        # treating it as a second funding split", which is an instruction
        # and not a result. The v21.43 run printed cell sizes, the reader
        # computed the pre/post difference by hand, and the conclusion -
        # that the gap does not move at the birth - ended up in an email
        # rather than in the log. That is the failure this project has
        # spent six builds removing: a number that exists only in prose.
        #
        # The non-cash block three sections below has done this from the
        # start. It reports the direction and names the branch. There is no
        # reason for the fee margin to be held to a weaker standard, and
        # the fee margin is the one that would make the paper's financing
        # claim literal rather than proxied, so it should be held to a
        # stronger one.
        #
        # WHAT THIS IS AND IS NOT. A difference of raw proportions, pooled
        # pre against pooled post, with a normal-approximation standard
        # error. No fixed effects, no controls, no event-study weighting.
        # It is enough to say whether a design is worth specifying and
        # nothing more, which is what a loader diagnostic should decide.
        # POST IS k >= 1, NOT k >= 0. The birth-wave cell is the one whose
        # composition moves, so including it puts the employment response
        # inside a statistic meant to measure the fee response. On the
        # v21.43a numbers it changed the answer from -1.05 to -1.32 and the
        # |t| from 0.98 to 1.34 - the same verdict, but a fifth of the
        # movement came from the one cell that cannot carry it.
        #
        # The birth wave is still in the table above. It is excluded from
        # the SUMMARY, not from the report, because the k = 0 cell is
        # informative about employment even where it is uninformative about
        # fees.
        .pre <- .ek[.ek$k < 0, ]; .pst <- .ek[.ek$k >= 1, ]
        .gp <- function(z) {
          w <- z$pct[z$female == 1]; m <- z$pct[z$female == 0]
          nw <- z$n[z$female == 1];  nm <- z$n[z$female == 0]
          if (!length(w) || !length(m)) return(c(NA_real_, NA_real_))
          g  <- mean(w) - mean(m)
          se <- sqrt(sum(w/100*(1-w/100)/nw)/length(w)^2 +
                     sum(m/100*(1-m/100)/nm)/length(m)^2) * 100
          c(g, se)
        }
        .a <- .gp(.pre); .b <- .gp(.pst)
        if (all(is.finite(c(.a, .b)))) {
          .dd <- .b[1] - .a[1]; .sd <- sqrt(.a[2]^2 + .b[2]^2)
          cat(sprintf(paste0("\n  Women-minus-men self-funding gap:\n",
                             "    pre-birth  (k < 0)  %+.2f pp (SE %.2f)\n",
                             "    post-birth (k >= 1) %+.2f pp (SE %.2f)\n",
                             "    change              %+.2f pp (SE %.2f, |t| %.2f)\n"),
                      .a[1], .a[2], .b[1], .b[2], .dd, .sd, abs(.dd / .sd)))
          if (abs(.dd / .sd) >= 1.96)
            cat("  *** THE GAP MOVES AT THE BIRTH. The fee margin is a second\n",
                "      funding split and not only a level. Specify the design\n",
                "      in the 03 design stage and designate it BEFORE\n",
                "      estimating; do not report this raw difference.\n", sep = "")
          else
            cat("  THE GAP DOES NOT MOVE at the birth on this raw difference.\n",
                "  Report the LEVEL - women bear more of the fee burden at\n",
                "  every horizon - and do not build a birth-wave design on\n",
                "  it. The level is the complement to the time margin; the\n",
                "  dynamic is not there to be found.\n", sep = "")
          # The single-horizon caution, because one cell drives the pre-mean.
          .rng <- range(.pre$pct[.pre$female == 1] - .pre$pct[.pre$female == 0])
          if (diff(.rng) > 2)
            cat(sprintf(paste0("  CAUTION: the pre-birth gap ranges %.1f to ",
                               "%.1f pp across horizons, so the pre-mean\n  ",
                               "rests on which leads are included. Read the ",
                               "table, not the summary.\n"), .rng[1], .rng[2]))
          rm(.dd, .sd, .rng)
        }
        rm(.pre, .pst, .gp, .a, .b)
        rm(.min)
      }
      rm(.bw, .ek)
    }
    rm(.sf)
  }

  # ---- v21.40 WHAT THE total_own_children FIX MOVED -----------------
  # Measured, not asserted. The NA fix above changes ever_parent for anyone
  # whose child components were all missing, and ever_parent is what the
  # roster is built from, so the cost has to be visible in the same log as
  # the roster it changes.
  if (all(c("total_own_children", "total_own_children_narm0")
          %in% names(panel_full))) {
    .chg <- which(is.na(panel_full$total_own_children) &
                    panel_full$total_own_children_narm0 == 0)
    cat("\n--- total_own_children: NA-preserving fix (v21.40) ---\n")
    cat(sprintf("  person-waves that were 0 by rowSums and are now NA: %s (%.2f%%)\n",
                format(length(.chg), big.mark = ","),
                100 * length(.chg) / nrow(panel_full)))
    if (length(.chg)) {
      cat(sprintf("  distinct persons touched: %s\n",
                  format(dplyr::n_distinct(panel_full$person_id[.chg]),
                         big.mark = ",")))
      cat("  Each of these was reported CHILDLESS by the old rule and is\n")
      cat("  unknown under the new one. They leave the never-treated group,\n")
      cat("  so compare the roster below against 2,704 / 7,946 / 10,650: a\n")
      cat("  never-treated count that has FALLEN is this fix working.\n")
    } else {
      cat("  None. Both child components are observed wherever the panel has\n")
      cat("  a row, so the defect was latent and the roster is unchanged.\n")
    }
    rm(.chg)
  }
  
  # ---- v21.36 THE ESTIMATION ROSTER, RECONSTRUCTED AND CHECKED ------
  # Lifted out of the child-care block below, which is where it was first
  # needed, because a roster check that disappears whenever nbcmcca fails
  # to resolve is a check on the wrong thing. It is built once here and
  # the block below reads it.
  #
  # WHY IT IS HERE AT ALL. The event study runs on persons whose FIRST
  # birth falls inside the panel: 2,704 treated and 7,946 never-treated,
  # a roster of 10,650. Individuals already parents when first observed
  # are left-censored and dropped because their event time is undefined.
  # Nothing in this loader knew that, so the v21.35 child-care density
  # used every person with a birth in-panel, 4,332, and reported a
  # design-feasibility number on a population the design does not use.
  #
  # THE RECONSTRUCTION IS PRINTED RATHER THAN TRUSTED. Left-censored is
  # ever_parent already 1 at the person's FIRST observed wave; treated is
  # ever_parent turning on later. If the counts below do not match the
  # design's, this loader and the estimation stage disagree about who is
  # in the sample, which is a larger problem than any density.
  #
  # ever_parent is monotonised by the cummax fix above, so first and last
  # are enough and no within-person scan is needed.
  .roster <- NULL
  if (all(c("ever_parent", "person_id", "wave") %in% names(panel_full))) {
    .roster <- panel_full %>%
      dplyr::filter(!is.na(ever_parent)) %>%
      dplyr::arrange(person_id, wave) %>%
      dplyr::group_by(person_id) %>%
      dplyr::summarise(ep_first = dplyr::first(ever_parent),
                       ep_last  = dplyr::last(ever_parent),
                       .groups  = "drop") %>%
      dplyr::mutate(
        left_censored = as.integer(ep_first == 1),
        treated       = as.integer(ep_first == 0 & ep_last == 1),
        never_treated = as.integer(ep_first == 0 & ep_last == 0))
    cat("\n--- Estimation roster (v21.36) ---\n")
    cat(sprintf(paste0("  %s treated + %s never-treated = %s. Left-censored ",
                       "and dropped: %s.\n"),
                format(sum(.roster$treated), big.mark = ","),
                format(sum(.roster$never_treated), big.mark = ","),
                format(sum(.roster$treated) + sum(.roster$never_treated),
                       big.mark = ","),
                format(sum(.roster$left_censored), big.mark = ",")))
    cat("  Against the design's 2,704 / 7,946 / 10,650, which came from the\n",
        "  old parenthood rule. v21.49 changed the rule; the table below\n",
        "  rebuilds the old roster from ever_parent_ref.\n", sep = "")
    
    # v21.40. THE 64-PERSON GAP IS THE age_exp_diff FILTER, and the master's
    # run log settled it. The estimation stage applies
    #
    #     filter(between(age - experience_years, 10, 35))
    #
    # on load, taking the panel from 20,205 persons to 19,661. This loader
    # reconstructs the roster BEFORE that filter, so it counts people the
    # design does not. The arithmetic closed exactly:
    #
    #     roster       10,714 - 64  = 10,650   the design's figure
    #     left-cens     9,491 - 480 =  9,011
    #     total                     = 19,661   the master's person count
    #
    # The reconstruction was right and its POPULATION was one filter wide.
    # Both rows print. The unfiltered one stays because this loader's columns
    # are built on the unfiltered panel, and a diagnostic that silently
    # adopted a downstream restriction would misreport its own coverage.
    #
    # AND THE PAPER USES BOTH BASES. The roster counts are post-filter; the
    # 21.5% / 25.5% left-censoring shares reproduce on the UNFILTERED 20,205
    # with a pooled denominator. Two adjacent statistics, two person bases,
    # nothing saying so. Name the base wherever either number appears.
    if (all(c("age", "experience_years") %in% names(panel_full))) {
      # NOT a leading-dot name inside mutate(): an argument beginning with a
      # dot is invisible to the pipe's data mask and fails with "object
      # 'age' not found", which reads as a missing column and is a naming
      # collision. aed_tmp instead.
      .aed <- panel_full %>%
        dplyr::mutate(aed_tmp = age - experience_years) %>%
        dplyr::filter(!is.na(aed_tmp), aed_tmp >= 10, aed_tmp <= 35) %>%
        dplyr::distinct(person_id) %>% dplyr::pull(person_id)
      .rf <- .roster[.roster$person_id %in% .aed, , drop = FALSE]
      cat(sprintf(paste0("  After the estimation stage's age_exp_diff filter ",
                         "(10 to 35):\n    %s treated + %s never-treated = %s; ",
                         "left-censored %s; %s persons.\n"),
                  format(sum(.rf$treated), big.mark = ","),
                  format(sum(.rf$never_treated), big.mark = ","),
                  format(sum(.rf$treated) + sum(.rf$never_treated), big.mark = ","),
                  format(sum(.rf$left_censored), big.mark = ","),
                  format(nrow(.rf), big.mark = ",")))
      cat("  This row uses the v21.49 rule. Under the old rule it is the row\n",
          "  to compare with 2,704 / 7,946 / 10,650; see the table below.\n",
          sep = "")
      rm(.aed, .rf)
    } else {
      cat("  age or experience_years absent, so the filtered comparison\n")
      cat("  cannot be made here. The unfiltered roster above will read\n")
      cat("  about 64 persons high against the design's figure.\n")
    }

    # ---- v21.49 THE ROSTER UNDER BOTH PARENTHOOD RULES ---------------
    # From v21.49 ever_parent counts own children only. ever_parent_ref
    # keeps the old rule, which also counted household children under
    # 15. The design's 2,704 / 7,946 / 10,650 and the paper's 21.5 / 25.5
    # come from the old rule, so its rows are the ones to check against
    # them. men_pct and women_pct use the paper's denominator: all
    # persons on that base, both sexes together.
    if ("ever_parent_ref" %in% names(panel_full)) {
      .rost49 <- function(flag) {
        .d <- data.frame(person_id = panel_full$person_id,
                         wave      = panel_full$wave,
                         female    = panel_full$female,
                         flag      = flag)
        .d <- .d[!is.na(.d$flag), , drop = FALSE]
        .d <- .d[order(.d$person_id, .d$wave), , drop = FALSE]
        .f <- !duplicated(.d$person_id)
        .l <- !duplicated(.d$person_id, fromLast = TRUE)
        .s <- data.frame(person_id = .d$person_id[.f],
                         female    = .d$female[.f],
                         first     = .d$flag[.f],
                         last      = .d$flag[.l])
        .on <- .d[.d$flag == 1, , drop = FALSE]
        .s$event_wave <- .on$wave[match(.s$person_id, .on$person_id)]
        .s$status <- ifelse(.s$first == 1, "left_censored",
                            ifelse(.s$last == 1, "treated", "never_treated"))
        .s
      }
      .ae49  <- panel_full$age - panel_full$experience_years
      .aed49 <- unique(panel_full$person_id[(.ae49 >= 10 & .ae49 <= 35) %in% TRUE])
      .row49 <- function(s, rule, base) data.frame(
        rule = rule, base = base,
        treated   = sum(s$status == "treated"),
        never     = sum(s$status == "never_treated"),
        roster    = sum(s$status != "left_censored"),
        left_cens = sum(s$status == "left_censored"),
        men_pct   = round(100 * sum(s$status == "left_censored" &
                                      s$female %in% 0) / nrow(s), 1),
        women_pct = round(100 * sum(s$status == "left_censored" &
                                      s$female %in% 1) / nrow(s), 1),
        stringsAsFactors = FALSE)
      .s_ref <- .rost49(panel_full$ever_parent_ref)
      .s_new <- .rost49(panel_full$ever_parent)
      cat("\n--- v21.49: the roster under both parenthood rules ---\n")
      print(rbind(
        .row49(.s_ref, "old", "all"),
        .row49(.s_ref[.s_ref$person_id %in% .aed49, ], "old", "age-exp 10-35"),
        .row49(.s_new, "v21.49", "all"),
        .row49(.s_new[.s_new$person_id %in% .aed49, ], "v21.49", "age-exp 10-35")),
        row.names = FALSE)
      cat("  old is ever_parent_ref and v21.49 is ever_parent. Compare the old\n",
          "  age-exp row with the design's 2,704 / 7,946 / 10,650. men_pct and\n",
          "  women_pct are left-censored men and women as shares of all persons\n",
          "  on that base, the denominator of the paper's 21.5 / 25.5.\n", sep = "")
      .mv <- merge(.s_ref[, c("person_id", "female", "status", "event_wave")],
                   .s_new[, c("person_id", "status", "event_wave")],
                   by = "person_id", suffixes = c("_old", "_new"))
      .mv <- .mv[.mv$person_id %in% .aed49, , drop = FALSE]
      cat("\n  Persons by status, old rule (rows) against the v21.49 rule\n",
          "  (columns), age-exp 10-35:\n", sep = "")
      print(stats::ftable(table(
        sex      = ifelse(.mv$female %in% 1, "women", "men"),
        old_rule = .mv$status_old,
        new_rule = .mv$status_new), row.vars = 1:2))
      cat(sprintf("  Treated under both rules with a different event wave: %s\n",
                  format(sum(.mv$status_old == "treated" &
                               .mv$status_new == "treated" &
                               .mv$event_wave_old != .mv$event_wave_new,
                             na.rm = TRUE), big.mark = ",")))
      rm(.rost49, .ae49, .aed49, .row49, .s_ref, .s_new, .mv)
    }
    
    # THE LEFT-CENSORING CHARACTERISATION, which the paper states and
    # nothing measured. Two claims: left-censored parents are 21.5% of
    # men and 25.5% of women, and they enter about thirteen years older,
    # near 40 against 27. Both are checkable from the object just built
    # and both bear on external validity, so they are reported here
    # rather than asserted in prose.
    if (all(c("female", "age") %in% names(panel_full))) {
      .entry <- panel_full %>%
        dplyr::arrange(person_id, wave) %>%
        dplyr::group_by(person_id) %>%
        dplyr::summarise(female = dplyr::first(female),
                         age_at_entry = dplyr::first(age), .groups = "drop") %>%
        dplyr::inner_join(.roster, by = "person_id") %>%
        dplyr::filter(!is.na(female))
      .by_sex <- .entry %>%
        dplyr::group_by(female) %>%
        dplyr::summarise(n = dplyr::n(),
                         pct_left_censored = round(100 * mean(left_censored), 1),
                         .groups = "drop")
      # v21.39. BOTH DENOMINATORS, ALWAYS, because the difference between
      # them is a factor of two and the v21.36 to v21.38 sweeps spent
      # three builds hunting a data discrepancy that was a wording one.
      #
      # RESOLVED: the paper's 21.5% and 25.5% are shares of ALL PERSONS,
      # not of men and of women. 4,345 left-censored men over 20,205 is
      # 21.51%; 5,146 left-censored women over the same 20,205 is 25.47%;
      # and the two sum to 46.97%, the overall left-censored share. The
      # sex-specific shares, which is what "of men" and "of women" say,
      # are 44.2% and 49.6%.
      #
      # THE RESTRICTION IS THEREFORE TWICE AS SEVERE AS THE SENTENCE
      # READS. It removes nearly half of each sex, not a quarter. That
      # changes how much weight the external-validity caveat has to
      # carry, so it is a correction to the prose and not a rounding
      # quibble. Both columns print from here on and the pooled one is
      # labelled, so the ambiguity cannot come back.
      .n_all <- nrow(.entry)
      .by_sex$pct_of_all_persons <- round(
        100 * vapply(.by_sex$female, function(.f)
          sum(.entry$left_censored[.entry$female == .f]) / .n_all, numeric(1)), 1)
      cat("\n  Left-censored share by sex (female = 1), BOTH denominators:\n")
      print(as.data.frame(.by_sex), row.names = FALSE)
      cat(sprintf(paste0("  pct_left_censored is out of that sex; ",
                         "pct_of_all_persons is out of all %s.\n"),
                  format(.n_all, big.mark = ",")))
      cat("  The paper's 21.5 / 25.5 are the SECOND column under the old\n")
      cat("  rule; the v21.49 table above gives both rules. The first column\n")
      cat("  is the share of each sex, about twice the second.\n")
      rm(.n_all)
      .age <- .entry %>%
        dplyr::filter(left_censored == 1 | treated == 1) %>%
        dplyr::group_by(left_censored) %>%
        dplyr::summarise(n = dplyr::n(),
                         mean_age_at_entry = round(mean(age_at_entry, na.rm = TRUE), 1),
                         .groups = "drop")
      cat("\n  Age at panel entry, left-censored parents vs transitioners:\n")
      print(as.data.frame(.age), row.names = FALSE)
      if (nrow(.age) == 2) {
        .gap <- .age$mean_age_at_entry[.age$left_censored == 1] -
          .age$mean_age_at_entry[.age$left_censored == 0]
        cat(sprintf(paste0("  Gap of %.1f years, against the paper's ~13 ",
                           "(about 40 vs 27).\n"), .gap))
        cat("  The design identifies the penalty for women whose first birth\n")
        cat("  falls while they are attached to the panel, which skews to\n")
        cat("  LATER first births than the population. State it as a scope\n")
        cat("  condition, not a robustness check: no reweighting recovers a\n")
        cat("  population whose event time is undefined.\n")
        rm(.gap)
      }
      
      # ---- v21.37/38 THE SWEEPS, RETIRED BUT KEPT -------------------
      # v21.39. These three sweeps - age cap, alternative base,
      # alternative parenthood definition - were built to locate a
      # discrepancy that turned out to be a denominator wording error,
      # resolved above. They are OFF by default now: they print about
      # thirty lines and answer a question that is closed.
      #
      # They are kept rather than deleted because each one RULED SOMETHING
      # OUT, and the eliminations are what made the arithmetic above
      # findable. No age cap reconciled the roster with the shares; the
      # wage base moved the share the wrong way; no parenthood definition
      # came within 9 points. Having excluded age, base and definition,
      # what was left was the denominator. Delete them and the next person
      # to doubt the roster re-derives all three from nothing.
      #
      # Set ROSTER_SWEEPS <- TRUE to run them.
      if (!exists("ROSTER_SWEEPS")) ROSTER_SWEEPS <- FALSE
      if (isTRUE(ROSTER_SWEEPS)) {
        # ---- v21.37 WHICH AGE WINDOW? READ IT OFF, DO NOT GUESS --------
        # The v21.36 run left one discrepancy and it has one shape. The
        # roster came within 0.6% of the design's (2,710 / 8,004 / 10,714
        # against 2,704 / 7,946 / 10,650) and the age gap landed on 13.8
        # against ~13, so WHO is a transitioner and who is left-censored is
        # right. But the left-censored SHARE read 44.2% of men and 49.6% of
        # women against the paper's 21.5% and 25.5%, almost exactly double.
        #
        # A larger denominator cannot explain it: the base is already every
        # person on the panel, 20,205, which the log confirms. So the
        # design must count FEWER people as left-censored, about 4,750
        # rather than 9,491.
        #
        # ONE RESTRICTION EXPLAINS ALL THREE MISSES AT ONCE. Left-censored
        # parents enter at 39.9 and transitioners at 26.1, so an upper
        # bound on age at entry removes the first group heavily and the
        # second barely. That is exactly the observed pattern: the roster
        # falls by 64 while the left-censored count halves. A childbearing
        # age window is the obvious candidate and this loader does not know
        # which one the design uses.
        #
        # So it is READ OFF rather than assumed. The row whose counts match
        # 2,704 / 7,946 / 10,650 and whose shares match 21.5 / 25.5 names
        # the window. If NO row matches both, the difference is not an age
        # restriction and the hypothesis above is wrong, which is equally
        # worth knowing and is why the whole sweep prints rather than a
        # single best guess.
        #
        # HOW FAR THE HYPOTHESIS HAS TO STRETCH, stated before the table so
        # the table is read against a number rather than for a match. To
        # reach 23.5% on a roster of 10,650 the left-censored count must be
        # about 3,272. It is 9,491, so the design excludes roughly 6,200 of
        # them, or two in three. An age cap that severe starts removing
        # transitioners too, and the roster is only 64 out. So the sweep
        # may well find NO row satisfying both, and that is a real result:
        # it would mean the two populations differ by something other than
        # age and this hypothesis is wrong.
        #
        # The caps therefore run down to 30, past the point where the
        # roster survives, so the table BRACKETS the answer instead of
        # stopping just short of it. A sweep that cannot fail is not a
        # test.
        #
        # The wage sample is swept as a second base because it is the other
        # candidate the loader can construct, and because the design
        # estimates on wages. It moves the share the WRONG way - a smaller,
        # more attached base raises the left-censored share rather than
        # halving it - so if it is the base, the discrepancy is larger than
        # it looks and not smaller. Worth printing for that reason alone.
        if (nrow(.entry) && any(!is.na(.entry$age_at_entry))) {
          .caps <- c(30, 35, 40, 45, 50, 55, Inf)
          .mk_sweep <- function(.dat, .label) {
            do.call(rbind, lapply(.caps, function(.cap) {
              .e <- .dat[!is.na(.dat$age_at_entry) &
                           .dat$age_at_entry <= .cap, , drop = FALSE]
              if (!nrow(.e) || !any(.e$female == 0) || !any(.e$female == 1))
                return(NULL)
              data.frame(
                base = .label,
                cap  = if (is.finite(.cap)) .cap else NA_real_,
                treated = sum(.e$treated), never_tr = sum(.e$never_treated),
                roster  = sum(.e$treated) + sum(.e$never_treated),
                left_cens = sum(.e$left_censored),
                pct_men   = round(100 * mean(.e$left_censored[.e$female == 0]), 1),
                pct_women = round(100 * mean(.e$left_censored[.e$female == 1]), 1))
            }))
          }
          .sweep <- .mk_sweep(.entry, "all")
          if ("in_wage_sample" %in% names(panel_full)) {
            .ws <- unique(panel_full$person_id[
              !is.na(panel_full$in_wage_sample) & panel_full$in_wage_sample == 1])
            .sweep <- rbind(.sweep,
                            .mk_sweep(.entry[.entry$person_id %in% .ws, ,
                                             drop = FALSE], "wage"))
            rm(.ws)
          }
          cat("\n  Left-censoring by upper bound on age at entry, two bases:\n")
          print(.sweep, row.names = FALSE)
          cat("  Target: roster 2,704 / 7,946 / 10,650 with 21.5% / 25.5%.\n")
          cat("  A row matching BOTH names the design's restriction. If none\n")
          cat("  does - and the arithmetic above says one probably will not -\n")
          cat("  then the populations differ by something that is NOT age or\n")
          cat("  the wage screen, and the next place to look is whatever else\n")
          cat("  the estimation stage drops before counting.\n")
          rm(.caps, .mk_sweep, .sweep)
        }
        
        # ---- v21.38 THE LAST UNTESTED DIMENSION: THE DEFINITION --------
        # The v21.37 sweep did what it was built to do and FAILED TO MATCH,
        # and the shape of the failure is the finding. Read the two rows:
        #
        #   cap NA   roster 10,714  shares 44.2 / 49.6   counts MATCH
        #   cap 35   roster  9,269  shares 22.3 / 27.4   shares  MATCH
        #
        # The roster is right with NO age restriction and the shares are
        # right at a cap near 34. Those cannot both hold, so the design's
        # roster counts and its left-censoring shares are not computed on
        # the same population as each other. No cap reconciles them and no
        # further sweeping over caps will.
        #
        # THE REMAINING SUSPECT IS ever_parent ITSELF, and it is a good
        # one. Read its construction: four sources, any one sufficient,
        # deliberately maximal. tchad > 0, own resident plus non-resident
        # children > 0, household children under 15 > 0, or ANY
        # non-resident child. So a 60-year-old whose children left home
        # thirty years ago is ever_parent == 1 and lands in this loader's
        # left-censored bucket.
        #
        # That is the right definition for "has ever been a parent" and
        # possibly the wrong one for "was already a parent in the sense the
        # event study means". A narrower one - resident children, or the
        # birth history alone - would strip out exactly the old cohort:
        # it would HALVE the left-censored count while barely touching the
        # roster, because transitioners are young and none of this reaches
        # them. That fits both facts at once, which no age cap did.
        #
        # So the definition is varied and the roster rebuilt under each.
        # Every candidate is monotonised within person first, because
        # ever_parent is the only one the cummax fix upstream touched and
        # comparing a monotonic flag against three non-monotonic ones would
        # manufacture spurious transitions in the treated column.
        #
        # panel_full is the slim frame here, so num_children_under15 and
        # total_own_children are already gone. Whichever candidates survive
        # the select are tested and the rest are named as unavailable
        # rather than silently skipped.
        .defs <- list(
          ever_parent    = "ever_parent",
          tchad_gt0      = "total_children_ever_had",
          own_resident   = "num_own_resident_children",
          has_youngest   = "age_youngest_own_child")
        .def_have <- .defs[vapply(.defs, function(z) z %in% names(panel_full),
                                  logical(1))]
        .def_miss <- setdiff(unlist(.defs), unlist(.def_have))
        if (length(.def_have) > 1L) {
          .dsweep <- do.call(rbind, lapply(names(.def_have), function(.nm) {
            .col <- .def_have[[.nm]]
            .x <- panel_full[[.col]]
            .flag <- if (.nm == "has_youngest") as.integer(!is.na(.x)) else
              as.integer(!is.na(.x) & .x > 0)
            .r <- data.frame(person_id = panel_full$person_id,
                             wave = panel_full$wave, flag = .flag)
            .r <- .r[!is.na(.r$flag), , drop = FALSE]
            .r <- .r[order(.r$person_id, .r$wave), ]
            # monotonise, so every definition is absorbing like ever_parent
            .r$flag <- as.integer(stats::ave(as.numeric(.r$flag),
                                             .r$person_id, FUN = cummax))
            .s <- .r %>% dplyr::group_by(person_id) %>%
              dplyr::summarise(f1 = dplyr::first(flag),
                               fL = dplyr::last(flag), .groups = "drop") %>%
              dplyr::inner_join(
                .entry[, c("person_id", "female")], by = "person_id")
            data.frame(
              definition = .nm,
              treated   = sum(.s$f1 == 0 & .s$fL == 1),
              never_tr  = sum(.s$f1 == 0 & .s$fL == 0),
              roster    = sum(.s$f1 == 0),
              left_cens = sum(.s$f1 == 1),
              pct_men   = round(100 * mean(.s$f1[.s$female == 0] == 1), 1),
              pct_women = round(100 * mean(.s$f1[.s$female == 1] == 1), 1))
          }))
          cat("\n  Roster under alternative parenthood definitions, no age cap:\n")
          print(.dsweep, row.names = FALSE)
          cat("  Target: 2,704 / 7,946 / 10,650 with 21.5% / 25.5%.\n")
          if (length(.def_miss))
            cat("  Not testable on the slim frame:",
                paste(.def_miss, collapse = ", "), "\n")
          cat("  A definition matching BOTH the counts and the shares with no\n")
          cat("  age cap is the design's, and ever_parent is then too broad\n")
          cat("  for this purpose while remaining correct for its own. If\n")
          cat("  none matches, the difference is not the definition either,\n")
          cat("  and the answer is in the estimation code rather than here.\n")
          rm(.dsweep)
        }
        rm(.defs, .def_have, .def_miss)
        
      }   # end ROSTER_SWEEPS
      
      # The person-wave reading, kept ON. It is one line, and it is the
      # standing check that the shares above are per-person: the v21.38
      # run put person-waves at 46.5 / 52.9, HIGHER than the per-person
      # 44.2 / 49.6, because left-censored parents contribute more waves
      # each. So the two readings differ in a direction worth seeing, and
      # anyone quoting a share from this panel should know which they
      # have.
      if ("female" %in% names(panel_full)) {
        .pw <- panel_full %>%
          dplyr::inner_join(.roster[, c("person_id", "left_censored")],
                            by = "person_id") %>%
          dplyr::filter(!is.na(female)) %>%
          dplyr::group_by(female) %>%
          dplyr::summarise(person_waves = dplyr::n(),
                           pct_lc = round(100 * mean(left_censored), 1),
                           .groups = "drop")
        cat("\n  Same share counted in PERSON-WAVES rather than persons:\n")
        print(as.data.frame(.pw), row.names = FALSE)
        cat("  If this reads 21.5 / 25.5 the paper's denominator is\n")
        cat("  person-waves and the counts were never in conflict.\n")
        rm(.pw)
      }
      rm(.entry, .by_sex, .age)
    }
  }
  .cc_roster <- .roster
  
  # ---- employer child care, the on-topic item -----------------------
  if ("employer_childcare_weekly" %in% names(panel_full) &&
      any(!is.na(panel_full$employer_childcare_weekly))) {
    .cc <- panel_full$employer_childcare_weekly
    .cc_pos <- sum(!is.na(.cc) & .cc > 0)
    cat(sprintf(paste0("\n  Employer child care: positive on %s person-wave(s) ",
                       "of %s observed.\n"),
                format(.cc_pos, big.mark = ","),
                format(sum(!is.na(.cc)), big.mark = ",")))
    #
    # v21.34. THE THRESHOLD WAS A BARE 100 AND IT LET 108 THROUGH.
    #
    # The v21.34 build printed "positive on 108 person-wave(s) of 109
    # observed" and then said NOTHING, because 108 is not below 100. One
    # hundred and eight person-waves over thirteen waves against 2,704
    # treated persons cannot support an event study by any margin, and
    # the diagnostic went quiet on precisely the case it was written to
    # warn about. A constant chosen by eye is a constant that will be
    # wrong by a few units at exactly the wrong moment.
    #
    # So the test is now against the treated-person count, which is the
    # quantity that actually governs whether a design can run. Both
    # numbers are printed so the ratio is checkable rather than trusted,
    # and the wording separates "too thin for an event study" from "too
    # thin to report at all" - a clean null on a rare item is worth
    # more than a caveat and should not be talked out of existence.
    # v21.36. THE DENOMINATOR IS THE ESTIMATION ROSTER, NOT THE PANEL.
    #
    # v21.35 counted every person with had_child_this_wave == 1 and got
    # 4,332. The event study does not run on those people. It runs on
    # persons whose FIRST birth falls inside the panel: 2,704 treated
    # and 7,946 never-treated, a roster of 10,650. The gap is the ~1,600
    # persons who have a birth in-panel that is not their first, and who
    # are left-censored and dropped because their event time is
    # undefined.
    #
    # So the printed density was a panel-wide fact wearing the label of
    # a design-feasibility test, and it was the SAME defect as the 43%
    # against 74.8% coverage figures fixed one build earlier: a number on
    # one population sitting beside a number on another with nothing
    # saying so. Third instance in three builds, and this one was mine.
    #
    # The numerator moves with it. Childcare positives are counted among
    # ROSTER-TREATED persons only, because a positive on a left-censored
    # parent contributes nothing to an event study it is not in.
    #
    # THE ROSTER IS RECONSTRUCTED HERE AND ITS SIZES ARE PRINTED, so the
    # construction is checked rather than trusted: left-censored means
    # ever_parent is already 1 at the person's FIRST observed wave, and
    # treated means it turns on later. If the three counts below do not
    # read 2,704 / 7,946 / 10,650 then this reconstruction and the one
    # the design uses disagree, and the density is the lesser problem.
    .cc_treated <- if (!is.null(.cc_roster))
      sum(.cc_roster$treated) else NA_integer_
    # Positives restricted to roster-treated persons. Still an UPPER
    # BOUND on what a design would see: it does not require the positive
    # to fall inside the event window, only on a person who is in it.
    if (!is.null(.cc_roster)) {
      .cc_tp <- .cc_roster$person_id[.cc_roster$treated == 1]
      .cc_pos_treated <- sum(!is.na(.cc) & .cc > 0 &
                               panel_full$person_id %in% .cc_tp)
      cat(sprintf(paste0("  Of the %s positives, %s fall on a roster-treated ",
                         "person.\n"),
                  format(.cc_pos, big.mark = ","),
                  format(.cc_pos_treated, big.mark = ",")))
      .cc_pos <- .cc_pos_treated
      rm(.cc_tp, .cc_pos_treated)
    }
    if (!is.na(.cc_treated) && .cc_treated > 0) {
      .cc_rate <- .cc_pos / .cc_treated
      cat(sprintf(paste0("  That is %.3f positives per treated person ",
                         "(%s treated).\n"),
                  .cc_rate, format(.cc_treated, big.mark = ",")))
      if (.cc_rate < 0.05)
        cat("  *** TOO THIN FOR AN EVENT STUDY, by two orders of magnitude.\n",
            "      Fewer than one positive per twenty treated persons means\n",
            "      the expected count in any k cell is a handful. Do not\n",
            "      build a design on these two columns.\n",
            "      REPORT THE RATE INSTEAD: employer-provided child care is\n",
            "      close to non-existent in Australia over this window, and\n",
            "      a null that clean is a finding rather than a caveat.\n",
            sep = "")
      else
        cat("  Above the density where an event study is hopeless. Check the\n",
            "  per-k cell counts before committing to one.\n", sep = "")
      rm(.cc_rate)
    } else if (.cc_pos < 1000) {
      # Fallback when the birth flag is not on the frame. Still not a
      # bare 100: an item this rare needs a margin, not a boundary.
      cat("  Too thin to support an event study on its own. Report the rate\n",
          "  as a descriptive fact, which is itself the finding.\n", sep = "")
    }
    rm(.cc, .cc_pos, .cc_treated)
  } else {
    cat("\n  nbcmcca/nbcmccr did not resolve on this release. The employer\n")
    cat("  child-care columns are NA and no design should reference them.\n")
  }
  rm(.nc, .obs, .pos)
  if (exists(".roster")) rm(.roster)
  if (exists(".cc_roster")) rm(.cc_roster)
}

# ---- v21.14 THE FIELDWORK WINDOW, BY WAVE ---------------------------
# What the interview date buys, in order of value.
#
# ONE, the reference period is not a wave. Every "past 12 months" item is
# anchored on the interview, so two respondents in the same wave
# interviewed months apart answer about different windows. This reports
# the spread so a design can decide whether to control for it.
#
# TWO, wave 20. The E1-to-E6 gate move and the pandemic are confounded
# there and neither can be measured. Fieldwork timing CAN be. If wave-20
# interviews ran later or over a longer span than the others, then the
# twelve months they ask about cover a different period, and part of the
# wave-20 step is the calendar rather than the questionnaire or behaviour.
# That does not resolve the confound; it turns a two-way argument into a
# three-way one with one arm measured.
#
# THREE, an event can now be placed INSIDE or OUTSIDE the reference
# window rather than bounded. Section 2.4 of PAPER_ATTENTION.md settled
# for a bound because this column was unread.
if ("interview_date" %in% names(panel_full)) {
  cat("\n--- Fieldwork window, by wave (v21.14) ---\n")
  .fw <- panel_full %>%
    dplyr::filter(!is.na(interview_date)) %>%
    dplyr::group_by(wave) %>%
    dplyr::summarise(
      n         = dplyr::n(),
      # as.Date() first, for the same reason as .date_part: if the column
      # arrives without its class, min/max still work but the subtraction
      # gives a bare number and span_days silently means something else.
      first     = min(as.Date(interview_date)),
      med       = stats::median(as.Date(interview_date)),
      last      = max(as.Date(interview_date)),
      span_days = as.integer(difftime(max(as.Date(interview_date)),
                                      min(as.Date(interview_date)),
                                      units = "days")),
      .groups   = "drop")
  .cov <- sum(!is.na(panel_full$interview_date))
  if (!nrow(.fw) || .cov == 0) {
    cat("  *** NO DATES PARSED. hhhqivw resolved but safe_date() could not\n")
    cat("      read it, so every column derived from it is NA. Check its\n")
    cat("      storage type before using any of them.\n")
  } else {
    print(as.data.frame(.fw), row.names = FALSE)
    write.csv(.fw, .out_path("v2114_fieldwork_window.csv"), row.names = FALSE)
    cat(sprintf("\n  Dated: %s of %s person-waves (%.1f%%).\n",
                format(.cov, big.mark = ","),
                format(nrow(panel_full), big.mark = ","),
                100 * .cov / nrow(panel_full)))
    # interview_doy is already an integer from .date_part, so nothing here
    # depends on Date dispatch. Kept explicit because that is what broke.
    .md <- panel_full %>%
      dplyr::filter(!is.na(interview_doy)) %>%
      dplyr::group_by(wave) %>%
      dplyr::summarise(m = stats::median(interview_doy), .groups = "drop")
    .w20 <- .md$m[.md$wave == 20]
    .oth <- stats::median(.md$m[.md$wave != 20])
    if (length(.w20)) {
      cat(sprintf(paste("\n  Median interview day-of-year: wave 20 = %d,",
                        "other waves = %d,\n  a shift of %+d days.\n"),
                  as.integer(.w20), as.integer(.oth),
                  as.integer(.w20 - .oth)))
      if (abs(.w20 - .oth) > 21)
        cat("  *** WAVE-20 FIELDWORK IS SHIFTED BY MORE THAN THREE WEEKS,\n",
            "      so the twelve months its reference-period items ask\n",
            "      about is not the window the other waves ask about. The\n",
            "      wave-20 step in the training series now has a THIRD\n",
            "      candidate explanation, and unlike the other two this\n",
            "      one is measured.\n", sep = "")
      else
        cat("  Wave-20 timing is close to the others, so the reference\n",
            "  window is NOT a third explanation for the step.\n", sep = "")
    }
    cat(sprintf(paste("\n  Widest fieldwork span in a wave: %d days. Two",
                      "respondents that far apart\n  answer 'the past 12",
                      "months' about windows overlapping by about %d\n",
                      " of those 12 months.\n"),
                max(.fw$span_days), max(0L, 12L - max(.fw$span_days) %/% 30L)))
    .v21_facts$fieldwork <- list(pct_dated = 100 * .cov / nrow(panel_full),
                                 max_span_days = max(.fw$span_days))
    rm(.md, .w20, .oth)
  }
  rm(.fw, .cov)
}

# ---- v21.12 THE PARTNER LINK, MEASURED ------------------------------
# T8 found that partner_id is not perfectly reciprocal, four couple-waves
# carrying only one partner's row, and reported it inside a design block.
# That is a property of the RELEASE and belongs here, beside the
# superannuation partner check, so no design has to rediscover it.
if (all(c("person_id", "partner_id", "wave") %in% names(panel_full))) {
  .pl <- panel_full[!is.na(panel_full$partner_id) &
                      panel_full$partner_id != "",
                    c("person_id", "partner_id", "wave")]
  .key  <- paste(.pl$person_id, .pl$wave)
  .back <- paste(.pl$partner_id, .pl$wave)
  .recip <- .back %in% .key
  .v21_facts$partner_link <- list(
    n_with_partner_id = nrow(.pl),
    n_reciprocal      = sum(.recip),
    n_one_sided       = sum(!.recip),
    pct_one_sided     = 100 * mean(!.recip))
  cat(sprintf(paste("\nPartner link. %s person-waves name a partner; %s of",
                    "them (%.2f%%) name\n  someone who does not appear in",
                    "the same wave. A couple-wave is therefore\n  NOT",
                    "always two person-waves, and a design that assumes it",
                    "is will\n  silently drop or double-count those rows.\n"),
              format(nrow(.pl), big.mark = ","),
              format(sum(!.recip), big.mark = ","), 100 * mean(!.recip)))
  rm(.pl, .key, .back, .recip)
}

# .emp is defined here rather than inside the coverage block, because the
# E6 consistency block below needs the same mask and two definitions of
# "employed" thirty lines apart is how they come to disagree. On the
# v21.6 panel employed has no missing values at all, 108,199 against
# 25,932, but the is.na() arm stays because a future release need not
# be so tidy and a silent NA in a mask propagates into every count.
.emp <- if ("employed" %in% names(panel_full))
  !is.na(panel_full$employed) & panel_full$employed == 1 else NULL
if (is.null(.emp))
  cat("\nNOTE: employed is not on the panel, so the training coverage and\n",
      "E6 blocks below are skipped rather than run on an all-NA mask.\n",
      sep = "")

if (!is.null(.emp) &&
    all(c("training_module_asked", "jttpewt_any", "jttpeot_any") %in%
        names(panel_full))) {
  # v21.7: EVERY COLUMN NAMES ITS BASE. The v21.6 table put asked_all and
  # gate_gap, which counted everyone, next to rate_any and rate_obs,
  # which counted only the employed, and explained the first pair in the
  # header and not the second. Adjacent columns describing different
  # people is the one thing a coverage table must not do quietly.
  #
  # v21.8: the non-employed half of the rate pair is filled in. v21.7 had
  # bases for who was ASKED on both sides of the employment line and
  # bases for what they ANSWERED on the employed side only, and then the
  # prose made a substantive claim about the non-employed that no column
  # measured. rate_any_non and rate_obs_non measure it.
  cat("\n--- Training module coverage, by wave (v21.8) ---\n")
  cat("  Bases: n_all is every person-wave in the wave, n_emp the\n")
  cat("  employed ones, and n_all - n_emp the rest. Each column below\n")
  cat("  carries its base in its name. Printed as two tables because\n")
  cat("  twelve columns do not fit a console line, written as one CSV.\n\n")
  cat("  WHO WAS ASKED\n")
  cat("  asked_all     % of n_all put the module\n")
  cat("  asked_emp     % of n_emp put the module\n")
  cat("  asked_non     % of the NON-employed put the module. This is the\n")
  cat("                mixing weight: where it is below 100, an\n")
  cat("                unconditional training series is part measured\n")
  cat("                answer and part assumed zero.\n")
  cat("  gap_all       count of n_all never asked, reading jttpewt_any = 0\n")
  cat("  gap_emp       the part of gap_all inside an employed-only\n")
  cat("                estimation sample. asked_emp rounds to 100.0 while\n")
  cat("                this is still non-zero, so read the count, not the\n")
  cat("                percentage.\n\n")
  cat("  WHAT THEY ANSWERED\n")
  cat("  rate_any_emp  employer-funded rate over n_emp, coalesced column\n")
  cat("  rate_obs_emp  and gated column. They differ by the gap_emp rows.\n")
  cat("  rate_any_non  the same pair over the NON-employed. rate_obs_non\n")
  cat("  rate_obs_non  is the one that settles whether the structural\n")
  cat("                zero is TRUE or merely ASSUMED: it is measured on\n")
  cat("                the non-employed who WERE asked. rate_any_non is\n")
  cat("                what an unconditional specification actually\n")
  cat("                reads, with the never-asked imputed to zero, so\n")
  cat("                the gap between the two IS the imputation.\n")
  .mc <- do.call(rbind, lapply(sort(unique(panel_full$wave)), function(w) {
    i  <- panel_full$wave == w
    ie <- i & .emp
    io <- i & !.emp
    data.frame(wave = w, n_all = sum(i), n_emp = sum(ie),
               asked_all = round(100 * mean(panel_full$training_module_asked[i]), 1),
               asked_emp = if (sum(ie)) round(100 * mean(panel_full$training_module_asked[ie]), 1) else NA_real_,
               asked_non = if (sum(io)) round(100 * mean(panel_full$training_module_asked[io]), 1) else NA_real_,
               gap_all   = sum(panel_full$training_module_asked[i] == 0L),
               gap_emp   = sum(panel_full$training_module_asked[ie] == 0L),
               rate_any_emp = if (sum(ie)) round(mean(panel_full$jttpewt_any[ie], na.rm = TRUE), 3) else NA_real_,
               rate_obs_emp = if (sum(ie)) round(mean(panel_full$jttpewt_obs[ie], na.rm = TRUE), 3) else NA_real_,
               rate_any_non = if (sum(io)) round(mean(panel_full$jttpewt_any[io], na.rm = TRUE), 3) else NA_real_,
               rate_obs_non = if (sum(io)) round(mean(panel_full$jttpewt_obs[io], na.rm = TRUE), 3) else NA_real_,
               stringsAsFactors = FALSE)
  }))
  cat("\n")
  print(.mc[, c("wave", "n_all", "n_emp", "asked_all", "asked_emp",
                "asked_non", "gap_all", "gap_emp")], row.names = FALSE)
  cat("\n")
  print(.mc[, c("wave", "rate_any_emp", "rate_obs_emp",
                "rate_any_non", "rate_obs_non")], row.names = FALSE)
  # Renamed at each version because the column set changes meaning: two
  # files with the same name and different bases is the error this whole
  # change exists to remove.
  write.csv(.mc, .out_path("v218_training_module_coverage.csv"), row.names = FALSE)
  cat("\n  Written to v218_training_module_coverage.csv, all twelve columns.",
      "\n  The v216 and v217 files are superseded; delete them.\n")
  
  .gap_n <- sum(panel_full$training_module_asked == 0L)
  cat(sprintf(paste0("\n  %s of %s person-waves (%.1f%%) were never put the ",
                     "training module\n  and read as jttpewt_any = 0. Of ",
                     "those, %s are employed person-waves,\n  which is the ",
                     "part that lands inside the estimation sample.\n"),
              format(.gap_n, big.mark = ","),
              format(nrow(panel_full), big.mark = ","),
              100 * mean(panel_full$training_module_asked == 0L),
              format(sum(panel_full$training_module_asked == 0L & .emp),
                     big.mark = ",")))
  # The gate has to bite differently on the employed and the non-employed,
  # or it is not measuring what it claims to. If it does not, the module
  # was put to everyone and the coalesce was harmless.
  .a_emp <- mean(panel_full$training_module_asked[.emp])
  .a_non <- mean(panel_full$training_module_asked[!.emp])
  cat(sprintf("  Module asked: %.1f%% of employed against %.1f%% of non-employed.\n",
              100 * .a_emp, 100 * .a_non))
  if (abs(.a_emp - .a_non) < 0.05) {
    cat("  The two are within five points, so the module is NOT an\n")
    cat("  employment-routed item in this release and the structural zero\n")
    cat("  is small. jttpewt_any and jttpewt_obs will agree closely; say so\n")
    cat("  rather than switching columns for no reason.\n")
  } else {
    cat("  The module IS employment-routed, so the zeros in jttpewt_any\n")
    cat("  carry the extensive margin as well as the training decision.\n")
    cat("  For an EMPLOYED-ONLY design the two column families are the\n")
    cat("  same column up to gap_emp rows, and rate_any_emp against\n")
    cat("  rate_obs_emp above says by how much.\n")
    # v21.7 wrote a substantive claim here and did not compute it: that
    # the non-employed "truly received no employer-funded training", so
    # the zero was right. That is the assertion habit the coding-audit
    # block upstream exists to break, committed twenty lines after that
    # block runs. v21.8 measures it.
    #
    # The test is jttpewt_obs among the NON-EMPLOYED, which is
    # jttpewt_any among the non-employed who were asked. jttpewt is
    # training at the place of employment in paid work time over the
    # PAST TWELVE MONTHS, while employed is status AT INTERVIEW, so a
    # person who left a job in March can truthfully report workplace
    # training in the same wave. If that rate is materially above zero
    # the structural zero is not merely assumed for the never-asked, it
    # is WRONG for them, and the imputation is biased down by roughly
    # the never-asked count times the measured rate.
    .r_obs_non <- mean(panel_full$jttpewt_obs[!.emp], na.rm = TRUE)
    .r_any_non <- mean(panel_full$jttpewt_any[!.emp], na.rm = TRUE)
    .n_non_gap <- sum(panel_full$training_module_asked == 0L & !.emp)
    .implied   <- .n_non_gap * .r_obs_non
    cat("  For an UNCONDITIONAL design, DO NOT switch to jttpewt_obs.\n")
    cat("  Dropping the non-employed to NA rebuilds the employed-only\n")
    cat("  sample under another name, which is the comparison the design\n")
    cat("  exists to make. Keep the zero and declare what it is.\n")
    cat(sprintf(paste0("  What it is: %.1f%% of the non-employed were ",
                       "asked, and among\n  those the MEASURED rate is ",
                       "%.3f. The remaining %s are\n  imputed to zero, ",
                       "which the coalesce does silently.\n"),
                100 * .a_non, .r_obs_non,
                format(.n_non_gap, big.mark = ",")))
    if (.r_obs_non > TRAINING_STRUCTURAL_ZERO_MAX) {
      cat(sprintf(paste0("  THE STRUCTURAL ZERO IS NOT A TRUE ZERO. A ",
                         "non-employed person-wave\n  that WAS asked ",
                         "reports employer-funded training %.1f%% of the ",
                         "time,\n  so imputing zero to the %s never asked ",
                         "understates them by\n  roughly %s person-waves. ",
                         "rate_any_non reads %.3f against a\n  measured ",
                         "%.3f, and that gap is the imputation, not ",
                         "behaviour.\n"),
                  100 * .r_obs_non, format(.n_non_gap, big.mark = ","),
                  format(round(.implied), big.mark = ","),
                  .r_any_non, .r_obs_non))
      cat("  Declare the direction as well as the existence of the zero.\n")
    } else {
      cat(sprintf(paste0("  The measured rate among the asked non-employed ",
                         "is %.3f, below\n  the %.3f tolerance, so the ",
                         "assumed zero is supported by the\n  measured one ",
                         "and the imputation costs little.\n"),
                  .r_obs_non, TRAINING_STRUCTURAL_ZERO_MAX))
    }
    cat(sprintf(paste0("  Either way the mix is not constant: a mother ",
                       "employed at k = -1 is\n  asked with probability ",
                       "one and the same mother out of work at\n  k = 0 ",
                       "with probability %.3f, so her outcome changes from ",
                       "an\n  answer to an assumption at the wave the ",
                       "event study reads it.\n"), .a_non))
    warning(BUILD_TAG, ": the training module is employment-routed (",
            sprintf("%.1f%% vs %.1f%%", 100 * .a_emp, 100 * .a_non),
            "). An unconditional specification on jttpewt_any reads ",
            sprintf("%.1f%%", 100 * .a_non), " measured answers and ",
            sprintf("%.1f%%", 100 * (1 - .a_non)), " imputed zeros among ",
            "the non-employed, and the mix moves with the outcome. ",
            if (.r_obs_non > TRAINING_STRUCTURAL_ZERO_MAX)
              paste0("The imputed zero is WRONG on average: the measured ",
                     "rate among asked non-employed is ",
                     sprintf("%.3f", .r_obs_non), ", so the series is ",
                     "understated by roughly ",
                     format(round(.implied), big.mark = ","),
                     " person-waves. ")
            else
              paste0("The measured rate among asked non-employed is ",
                     sprintf("%.3f", .r_obs_non), ", so the imputation is ",
                     "close to harmless. "),
            "Keep the structural zero, declare it, and do not substitute ",
            "jttpewt_obs, which drops the arm rather than fixing it.",
            call. = FALSE)
  }
  .v21_facts$training_gate <- list(
    n_not_asked = .gap_n,
    n_not_asked_employed = sum(panel_full$training_module_asked == 0L & .emp),
    n_not_asked_nonemployed = sum(panel_full$training_module_asked == 0L & !.emp),
    pct_asked_employed = 100 * .a_emp,
    pct_asked_nonemployed = 100 * .a_non,
    # v21.8: the measured numbers behind the note below, so a downstream
    # script can re-derive the sentence rather than trust it.
    rate_obs_nonemployed = if (exists(".r_obs_non")) .r_obs_non else NA_real_,
    rate_any_nonemployed = if (exists(".r_any_non")) .r_any_non else NA_real_,
    implied_understatement = if (exists(".implied")) round(.implied) else NA_real_,
    # v21.7: the sentence a design has to print when it sets
    # allow_structural_zero = TRUE, written once here so that three
    # scripts do not write three versions of it. v21.8 replaces the
    # asserted half of it with the measured rate.
    structural_zero_note = paste0(
      "jttpewt_any and jttpeot_any read a never-asked person-wave as 0. ",
      "The module is employment-routed, so that zero is IMPUTED for the ",
      format(sum(panel_full$training_module_asked == 0L & !.emp),
             big.mark = ","),
      " non-employed person-waves never asked and MEASURED for the ",
      sprintf("%.1f%%", 100 * .a_non), " who were. Among the latter the ",
      "observed rate is ",
      if (exists(".r_obs_non")) sprintf("%.3f", .r_obs_non) else "not measured",
      ", so the imputation ",
      if (exists(".r_obs_non") && .r_obs_non > TRAINING_STRUCTURAL_ZERO_MAX)
        paste0("understates the non-employed by roughly ",
               format(round(.implied), big.mark = ","), " person-waves. ")
      else "costs little. ",
      "Do not convert the zero to NA: that rebuilds the employed-only ",
      "sample. The mixing proportion moves with employment and therefore ",
      "with event time."))
  rm(.mc, .gap_n, .a_emp, .a_non)
  if (exists(".r_obs_non")) rm(.r_obs_non, .r_any_non, .n_non_gap, .implied)
}

# ---- E6 against the place items (v21.7) -----------------------------
# The check v21.6 should have carried. jttrwrk asks whether the person
# took part in ANY work-related training in the past twelve months, and
# the four place-and-time items are asked afterwards, so the containment
# runs one way: a mention of any place item requires a yes at the gate.
# A person-wave with jttrwrk = no and jttpewt = mentioned is not an
# unusual answer, it is two answers that cannot both be right, and until
# now nothing in the pipeline compared them.
#
# Read on the _raw columns. The derived _any pair coalesces an
# unanswered follow-up to zero, so running the same test on jttpewt_any
# would convert every non-response into apparent agreement and the check
# could not fail.
#
# The block also prints the E6 base rate among the employed, which is
# the denominator the employer-funded rate is a share OF. 22.3% is a
# number on its own and a two-thirds share against an all-training rate
# near 34%.
if (!is.null(.emp) &&
    all(c(TRAINING_GATE_RAW, "training_module_asked") %in% names(panel_full)) &&
    length(intersect(TRAINING_PLACE_RAW, names(panel_full)))) {
  cat("\n--- E6 against the place items (v21.7) ---\n")
  .pl_cols <- intersect(TRAINING_PLACE_RAW, names(panel_full))
  if (length(.pl_cols) < length(TRAINING_PLACE_RAW))
    cat("  NOTE: only ", length(.pl_cols), " of the four place items are on\n",
        "  the panel (", paste(.pl_cols, collapse = ", "), "), so the\n",
        "  containment test is weaker than it should be.\n", sep = "")
  
  # Any place-and-time item mentioned. NA-propagating in the sense that
  # a person-wave with no place item answered at all reads FALSE for
  # "mentioned" and is counted separately below rather than pooled with
  # a genuine no.
  .pl_mat  <- as.matrix(as.data.frame(panel_full[, .pl_cols, drop = FALSE]))
  .pl_any  <- rowSums(.pl_mat == 1, na.rm = TRUE) > 0
  .pl_seen <- rowSums(!is.na(.pl_mat)) > 0
  .e6      <- panel_full[[TRAINING_GATE_RAW]]
  
  # The population for the test: the module was put AND E6 was answered.
  .test <- panel_full$training_module_asked == 1L & !is.na(.e6)
  .n_test <- sum(.test)
  
  cat(sprintf("  Population: %s person-waves where the module was put and\n  E6 was answered (%s of %s on the panel).\n",
              format(.n_test, big.mark = ","),
              sprintf("%.1f%%", 100 * .n_test / nrow(panel_full)),
              format(nrow(panel_full), big.mark = ",")))
  
  .xt <- table(E6 = ifelse(.e6[.test] == 1, "E6 yes", "E6 no"),
               place = ifelse(.pl_any[.test], "place mentioned",
                              ifelse(.pl_seen[.test], "no place mentioned",
                                     "no place item answered")))
  cat("\n")
  print(.xt)
  
  # The impossible cell.
  .bad   <- .test & .e6 != 1 & .pl_any
  .n_bad <- sum(.bad)
  .p_bad <- if (.n_test) .n_bad / .n_test else NA_real_
  # The same cell restricted to the outcome column the training designs
  # actually read, which is the one the review named.
  .n_bad_pewt <- if ("jttpewt_raw" %in% names(panel_full))
    sum(.test & .e6 != 1 &
          !is.na(panel_full$jttpewt_raw) & panel_full$jttpewt_raw == 1) else NA_integer_
  # The legal off-diagonal, printed so it is not mistaken for the other
  # one. Training reported at the gate and placed nowhere the four items
  # cover is a real answer: a course run by someone other than the
  # employer, or a follow-up left blank.
  .n_gap <- sum(.test & .e6 == 1 & !.pl_any)
  
  cat(sprintf("\n  E6 = no with a place item mentioned: %s (%.3f%% of the\n  population above). Of those, %s mention jttpewt, the column the\n  training designs read.\n",
              format(.n_bad, big.mark = ","), 100 * .p_bad,
              format(.n_bad_pewt, big.mark = ",")))
  cat(sprintf("  E6 = yes with no place item mentioned: %s. This is LEGAL,\n  not a contradiction: training placed outside the four items, or a\n  follow-up left blank. It is the gap between the E6 rate and the\n  place-item rate below.\n",
              format(.n_gap, big.mark = ",")))
  
  if (!is.na(.p_bad) && .p_bad > TRAINING_E6_CONTRADICTION_MAX) {
    warning(BUILD_TAG, ": ", format(.n_bad, big.mark = ","), " person-wave(s), ",
            sprintf("%.2f%%", 100 * .p_bad), " of those asked, answer E6 as ",
            "NO and still mention a place-and-time item. The two cannot ",
            "both be right. Either the == 1 rule is wrong for one of the ",
            "items in this release, or the follow-ups are not gated on E6 ",
            "the way the module documentation says. Read ",
            "v218_training_e6_consistency.csv before trusting either ",
            "column.", call. = FALSE)
  } else {
    cat(sprintf("  Below the %.1f%% tolerance: E6 and the place items are\n  consistent, so the == 1 rule reads the same thing at the gate and\n  at the follow-ups.\n",
                100 * TRAINING_E6_CONTRADICTION_MAX))
  }
  
  # Per-wave, because a coding change confined to one wave averages away
  # over thirteen and this is exactly the shape the mixed-coding stop
  # upstream is built to catch.
  .e6w <- do.call(rbind, lapply(sort(unique(panel_full$wave)), function(w) {
    i  <- panel_full$wave == w & .test
    ie <- panel_full$wave == w & .test & .emp
    io <- panel_full$wave == w & .test & !.emp
    data.frame(wave = w,
               n_asked      = sum(i),
               e6_rate      = if (sum(i)) round(mean(.e6[i] == 1), 3) else NA_real_,
               e6_rate_emp  = if (sum(ie)) round(mean(.e6[ie] == 1), 3) else NA_real_,
               # v21.8: printed rather than backed out of the other two.
               # The non-employed who WERE asked are the group that
               # settles whether the structural zero is a true zero.
               n_asked_non  = sum(io),
               e6_rate_non  = if (sum(io)) round(mean(.e6[io] == 1), 3) else NA_real_,
               place_rate_emp = if (sum(ie)) round(mean(.pl_any[ie]), 3) else NA_real_,
               contradictions = sum(panel_full$wave == w & .bad),
               stringsAsFactors = FALSE)
  }))
  cat("\n  By wave. e6_rate is over the asked; the _emp columns are over\n")
  cat("  the asked AND employed, which is the base rate_any_emp uses; the\n")
  cat("  _non columns are over the asked AND NOT employed, which is the\n")
  cat("  group the structural-zero question turns on.\n\n")
  print(.e6w, row.names = FALSE)
  write.csv(.e6w, .out_path("v218_training_e6_consistency.csv"), row.names = FALSE)
  
  # The denominator that makes the employer-funded rate readable.
  .emp_test <- .test & .emp
  .e6_emp   <- if (sum(.emp_test)) mean(.e6[.emp_test] == 1) else NA_real_
  .pewt_emp <- if (sum(.emp_test) && "jttpewt_any" %in% names(panel_full))
    mean(panel_full$jttpewt_any[.emp_test]) else NA_real_
  .share    <- if (is.na(.e6_emp) || .e6_emp == 0) NA_real_ else .pewt_emp / .e6_emp
  if (is.na(.share)) {
    cat("\n  The employed-and-asked base rate is unavailable, so the\n")
    cat("  employer-funded rate has no denominator to be a share of.\n")
  } else {
    cat(sprintf(paste0("\n  Among the employed who were asked: %.1f%% report ",
                       "training of any\n  kind, %.1f%% report it at the ",
                       "place of employment in paid work\n  time. The second ",
                       "is %.0f%% of the first, so the employer-funded rate ",
                       "is\n  a SHARE OF TRAINEES and should be reported ",
                       "against this base.\n"),
                100 * .e6_emp, 100 * .pewt_emp, 100 * .share))
  }
  
  if (!is.null(.v21_facts$training_gate)) {
    .v21_facts$training_gate$e6_n_tested       <- .n_test
    .v21_facts$training_gate$e6_rate_employed  <- .e6_emp
    .v21_facts$training_gate$e6_contradictions <- .n_bad
    .v21_facts$training_gate$e6_contradiction_share <- .p_bad
    .v21_facts$training_gate$employer_share_of_trainees <- .share
  }
  rm(.pl_cols, .pl_mat, .pl_any, .pl_seen, .e6, .test, .n_test, .xt,
     .bad, .n_bad, .p_bad, .n_bad_pewt, .n_gap, .e6w, .emp_test,
     .e6_emp, .pewt_emp, .share)
}
rm(.emp)

# ---- Which columns are entrant-only? --------------------------------
# The enrolment test above, run over every column a label prefix
# nominates. Reported together because the answer is the same shape for
# all of them and because one table is read where six paragraphs are not.
#
# Wave 12 is excluded for the same reason as above: entry_wave is entry
# into this window, so everyone is an entrant there.
.et_cols <- intersect(ENTRANT_TEST_COLS, names(panel_full))
if (length(.et_cols) && "entry_wave" %in% names(panel_full)) {
  .z  <- panel_full[panel_full$wave > min(WAVES), , drop = FALSE]
  .ne <- .z$wave == .z$entry_wave
  .et <- do.call(rbind, lapply(.et_cols, function(cl) {
    pn <- round(100 * mean(!is.na(.z[[cl]][.ne])), 1)
    pc <- round(100 * mean(!is.na(.z[[cl]][!.ne])), 1)
    data.frame(column = cl, pct_obs_entrant = pn, pct_obs_continuing = pc,
               ratio = if (pc > 0) round(pn / pc, 1) else Inf,
               stringsAsFactors = FALSE)
  }))
  .et$verdict <- ifelse(
    .et$pct_obs_continuing < ENTRANT_ROUTED_MAX_CONTINUING &
      (is.infinite(.et$ratio) | .et$ratio >= ENTRANT_ROUTED_MIN_RATIO),
    "ENTRANT-ONLY", "not routed")
  cat("\nEntrant routing, measured rather than read off a label prefix\n")
  print(.et, row.names = FALSE, right = FALSE)
  .entrant_only <- .et$column[.et$verdict == "ENTRANT-ONLY"]
  # v21.14g: inheritance, before the fact is recorded, so a parent that
  # inherits is on the list every downstream guard reads.
  .inh <- character(0)
  for (.par in names(COALESCED_PARENTS)) {
    .kids <- intersect(COALESCED_PARENTS[[.par]], .entrant_only)
    if (length(.kids) && !.par %in% .entrant_only) {
      .inh <- c(.inh, .par)
      cat(sprintf(paste("\n  INHERITS ENTRANT ROUTING: %s coalesces %s,",
                        "which %s\n  ENTRANT-ONLY above. Its own ratio",
                        "did not clear the threshold because a\n  mixture",
                        "of routed and unrouted sources sits between the",
                        "two\n  verdicts. It is added to the entrant-only",
                        "list on the STRUCTURE,\n  not on the ratio.\n"),
                  .par, paste(.kids, collapse = ", "),
                  if (length(.kids) == 1L) "is" else "are"))
    }
  }
  if (length(.inh)) {
    cat(sprintf("  Coalesces checked: %d. Inheriting: %s.\n",
                length(COALESCED_PARENTS), paste(.inh, collapse = ", ")))
    cat("  occ_origin_source separates the routes, so a design can use the\n")
    cat("  CPQ pair alone and lose the not-in-paid-work group, or use all\n")
    cat("  three and declare the mixture. It cannot do neither.\n")
    .entrant_only <- sort(unique(c(.entrant_only, .inh)))
  } else if (length(COALESCED_PARENTS)) {
    cat(sprintf(paste("\n  Coalesces checked: %d. None inherits entrant",
                      "routing.\n"), length(COALESCED_PARENTS)))
  }
  rm(.inh)
  .v21_facts$entrant_only_cols <- .entrant_only
  if (length(.entrant_only)) {
    cat("  ENTRANT-ONLY: ", paste(.entrant_only, collapse = ", "), "\n",
        sep = "")
    cat("  These are missing for most of the panel and their counts are\n")
    cat("  entrant counts, not panel counts. The four field flags cannot\n")
    cat("  be repaired by adding the CPQ battery: edqro* carries ASCED\n")
    cat("  LEVELS and no nursing, teaching or trade category, so the\n")
    cat("  fields exist only in the entrant instrument. Use them for a\n")
    cat("  statement about entrants or not at all; 00_compat_v21.R stops\n")
    cat("  them entering a specification without that declaration.\n")
    if (any(c("has_nursing_any", "has_teaching_any", "has_trade_any",
              "has_technical_any") %in% .entrant_only) &&
        "education_pathway" %in% names(panel_full)) {
      .enr <- grepl("_(Nursing|Teaching|Trade|Technical)$",
                    as.character(panel_full$education_pathway))
      cat(sprintf(paste0("  education_pathway carries an enriched label on ",
                         "%.2f%% of person-waves;\n  those branches are ",
                         "reachable for entrants only. The generic labels ",
                         "are\n  unaffected and the column as a whole is ",
                         "not entrant-only.\n"),
                  100 * mean(.enr, na.rm = TRUE)))
      rm(.enr)
    }
  }
  rm(.z, .ne, .et, .entrant_only)
}
rm(.et_cols)

# ---- The two nursing flags, side by side ----------------------------
# has_nursing_any includes edqodn and edqrodn, which are "Diploma NFI"
# and not nursing. has_nursing_strict drops them. The gap between the
# two counts is the size of the error, and printing it is what makes the
# correction an exhibit rather than a silent edit.
if (all(c("has_nursing_any", "has_nursing_strict") %in% names(panel_full))) {
  .n_any <- sum(panel_full$has_nursing_any == 1, na.rm = TRUE)
  .n_str <- sum(panel_full$has_nursing_strict == 1, na.rm = TRUE)
  cat(sprintf(paste0("\nNursing flags: has_nursing_any %s person-wave(s), ",
                     "has_nursing_strict %s.\n"),
              format(.n_any, big.mark = ","), format(.n_str, big.mark = ",")))
  if (.n_any > .n_str)
    cat(sprintf(paste0("  %s of them hold an unspecified diploma rather ",
                       "than a nursing\n  qualification. education_pathway ",
                       "still reads has_nursing_any; switching it\n  is a ",
                       "mutating change and is left to the analyst.\n"),
                format(.n_any - .n_str, big.mark = ",")))
  rm(.n_any, .n_str)
}

# ---- The manifest ---------------------------------------------------
# Written to the console and attached to the saved panels, so a rebuild
# carries its own rules and a downstream script can read them off the
# data rather than from a version of this file it may not have.
# v21.14e. TWO REGIMES WERE ON THE PANEL AND NOT IN THIS MANIFEST.
#
# v21.13 added training_gate_regime and v21.14b added occ_origin_regime,
# both as columns, and neither was added here. This table is what
# 00_compat_v21.R reads and what every downstream script prints, so a
# regime key that is not in it is a key nobody is told about. The panel
# carried the information and the manifest said eight restrictions.
#
# Both are the same class as firm_size: the variable is unchanged and the
# thing it is measured on is not.
#
#   training_gate_position  jttrwrk is PQ E1 in waves 7-19 and PQ E6 in
#       20-24, and every other training item moves with it, E1-E7 to
#       E6-E12. The population reaching it does not change, so this is not
#       a coverage break; the CONTEXT does, and wave 20 is also 2020, so
#       the two cannot be separated. Declared so a design has to choose
#       rather than inherit.
#   occ_origin_coverage     pjoo62/pjoo6s end at wave 19, so the
#       job-change route into occ2_origin disappears and coverage falls
#       from about 6.8% of person-waves to 3.1%. Same column, different
#       population either side.
RESTRICTIONS <- data.frame(
  restriction = c("firm_size", "education_w24", "entrant_only",
                  "interruption", "superannuation", "wealth",
                  "training_gate", "experience_level",
                  "training_gate_position", "occ_origin_coverage",
                  "income_aggregates", "compensation_base"),
  rule = c(
    sprintf("do not pool across wave %d", FIRM_SIZE_BREAK_WAVE),
    sprintf("wave %d is a break, not a trend", STUDY_ITEM_BREAK_WAVE),
    "NPQ items: entrant counts, not panel counts",
    "use the within-person flow, not the level",
    "household aggregate; no individual comparison",
    sprintf("waves %s only",
            paste(WEALTH_MODULE_WAVES, collapse = "/")),
    "_any imputes a zero where never asked; _obs DROPS those rows",
    "carries the entry-cohort recall vintage; absorb it or share it",
    sprintf(paste("whole training module moves five questions later at",
                  "wave %d (gate E1 -> E6); confounded with 2020"),
            TRAINING_GATE_POSITION_BREAK_WAVE),
    sprintf("job-change route ends at wave %d; coverage halves",
            OCC_ORIGIN_BREAK_WAVE - 1L),
    paste("gross REGULAR vs disposable, one imputed one not, all",
          "positive-only: do not difference or ratio them"),
    paste("wage, wage+sacrifice and wage+sacrifice+non-cash are THREE",
          "outcomes: name which one, and do not mix them across tables")),
  panel_key = c("firm_size_regime", "study_item_regime / educ_trend_comparable",
                "entrant_only_cols attribute",
                "d_olf_years / olf_year / olf_flow_valid",
                "hh_super_balance", "wealth_module_wave",
                "training_module_asked / training_gate_cols attribute",
                "experience_level_cols attribute",
                "training_gate_regime",
                "occ_origin_regime / occ_origin_source",
                "fy_income_gross_regular / fy_income_disposable",
                "weekly_earnings / weekly_earnings_inc_ss / weekly_compensation"),
  stringsAsFactors = FALSE)
cat("\nRestrictions written into this panel:\n")
print(RESTRICTIONS, row.names = FALSE, right = FALSE)
cat("Enforcement for the estimation stage is in 00_compat_v21.R.\n")



# =====================================================================
# NAME-BASED VARIABLE SELECTION
# =====================================================================
# Using dplyr::select with any_of() for variables that may not exist
# in all configurations (e.g., mechanism variables created in analysis script)

analysis_sample <- panel_full %>%
  dplyr::select(
    # === Identifiers & panel structure ===
    person_id, any_of("xwaveid"), wave, year,
    
    # === Demographics ===
    age, age_sq, female, married,
    
    # === Wages (outcome) ===
    ln_hourly_wage, ln_hourly_wage_real,
    hourly_wage_clean, real_hourly_wage, deflator,
    weekly_earnings, hours_worked_clean,
    annual_income, childcare_cost,
    
    # === v2: Earnings (primary outcome for the child-penalty design) ===
    real_weekly_earnings, real_annual_income,
    ihs_weekly_earnings, ihs_annual_income,
    earnings_is_zero, annual_income_is_zero,
    earnings_source_flag, salary_sacrifice,
    
    # === v7: self-employment and total earnings ===
    emptype_raw, is_employee, is_self_employed, any_of("fy_share_employed"),
    any_of("unpaid_leave_days"), is_selfemp_narrow, is_unpaid_family,
    bifhave_raw, had_business_lfy,
    fy_business_income, real_fy_business_income,
    fy_earnings_total, real_fy_earnings_total, ihs_earnings_total,
    earnings_total_is_zero, earnings_total_is_negative,
    earnings_total_includes_business, fy_business_income_nonzero,
    fy_wages, fy_wages_inc_ss,
    hours_worked_main_job, second_job_hours, weekly_annual_gap,
    
    # === v2: Other income (FTB band, couple totals, transfers) ===
    fy_benefits, real_fy_benefits,
    fy_income_disposable, fy_income_gross, fy_income_gross_regular,
    hh_income_disposable, hh_income_gross, real_hh_income_disposable,
    
    # === Education ===
    # v21.49: educ_years is the person's running maximum; educ_years_ref
    # is the per-wave value it replaced.
    educ_years, educ_years_ref, educ_sq, education_level, education_pathway,
    has_degree, has_postsecondary, has_diploma_plus, has_postgrad,
    has_nursing_any, has_nursing_strict, has_teaching_any, has_business_any,
    has_trade_any, has_technical_any,
    education_data_source,
    
    # === Experience & tenure ===
    experience_years, experience_sq, actual_experience,
    tenure_employer, tenure_occupation,
    
    # === v2: Experience provenance and non-employment history ===
    potential_experience, experience_source, experience_is_proxy,
    experience_ceiling_bound,
    years_since_education, years_unemployed, years_not_in_lf,
    years_npq_total,
    olf_share, unemp_share, employment_share,
    nonemployment_years, history_components, history_residual,
    npq_total_discrepancy, has_interruption, has_olf_spell,
    # v21: within-person increments, built once in the restriction block
    # above. These are what the scarring design reads. The LEVELS on the
    # two lines above are descriptive and within-person only, because
    # the base stock carries the entry cohort's recall vintage.
    entry_wave, entry_cohort, d_wave,
    d_paid_years, d_unemp_years, d_olf_years, d_exposure_years,
    history_flow_sum, olf_year, olf_flow_valid,
    
    # === Employment characteristics ===
    employed, fulltime, parttime, married_num, fulltime_num,
    # v21.6: fulltime is esdtl_raw == 1, so it is 0 off the employed as
    # well as for part-timers. fulltime_emp is the conditional rate, NA
    # off the employed. fulltime itself is unchanged.
    fulltime_emp,
    any_of("esdtl_raw"),
    in_wage_sample, in_earnings_sample,
    in_age_30_49, in_age_25_52, in_age_21_52,
    casual, permanent,
    public_sector, union_member,
    firm_size, firm_size_raw, firm_size_instrument, firm_size_comparable,
    firm_size_post22,   # v21.46: three levels, one regime, NA before it
    firm_size_regime,   # v21: the key the cross-break guard reads
    
    # === Job quality ===
    has_holiday_leave, has_sick_leave, leave_none,
    has_standard_hours, has_shift_work, has_irregular_hours,
    underemployed, overemployed,
    # v21.45. precarious_n_components is CARRIED, not dropped. It is
    # built at L5990 and was selected away, which left the score on the
    # panel without the count that decides whether the score is NA at
    # all. That mattered the moment has_irregular_hours stopped being an
    # imputed zero: an employed person-wave with a missing jbmsch now
    # loses one component, so the same score can rest on 4 components
    # where it used to rest on 5, and a person-wave at the
    # PRECARIOUS_MIN_COMPONENTS boundary can drop out entirely. Without
    # this column that change is invisible from the panel, and a fall in
    # coverage reads as attrition rather than as the fix arriving.
    precarious_score, precarious_category, precarious_n_components,
    flexibility_any, treatment,
    
    # === Job satisfaction & workplace quality (NEW) ===
    job_satisfaction, pay_fairness, job_initiative,
    job_security, job_worry_future, job_autonomy,
    looking_for_new_job,
    
    # === Life-satisfaction domains (NEW — SCQ 0-10) ===
    losat, losateo, losatfs, losatft, losathl,
    losatlc, losatnl, losatsf, losatyh,
    
    # === Occupation & industry ===
    occupation_major, occupation_major_code,
    industry_major, industry_major_code,
    
    # === Training ===
    # v2: any_training and total_training_hours are now constructed
    # explicitly upstream, so they are named rather than wrapped in
    # any_of(); a missing column is now an error rather than a silent drop.
    any_training, total_training_hours, total_training_hours_z,
    # v21.21: NAMED, not any_of(). any_of() tolerates absence, which is
    # what let these two sit here unconstructed. Named, a missing
    # derivation is an error at the select rather than a column that
    # quietly is not there.
    promotion_training, maintenance_training,
    training_aim_getstarted, training_aim_maintain, training_aim_safety,
    # v21.48: 1 where a trainee answered don't know or refused to the aim
    # battery. Those trainees are NA on all six aim items.
    training_aim_nonresponse,
    # v21.29: gate, wide variants and unconditional twins. The _z pair is
    # what an event study should use; the ungated pair is kept so a design
    # that wants to condition on training still can, but it must say so.
    aim_battery_asked,
    promotion_training_wide, maintenance_training_wide,
    promotion_training_z, maintenance_training_z,
    promotion_training_wide_z, maintenance_training_wide_z,
    training_aim_current_z, training_aim_general_z,
    total_training_days, avg_training_hours, total_courses,
    # v21.48: the gated twins. Use these, not the pair above, for any
    # design that runs on everyone asked rather than on trainees alone.
    total_training_days_z, total_courses_z,
    training_own_time, training_paid_time,
    jttpewt_any, jttpeot_any,   # place-of-employment-only indicators
    # v21.6: the gate and the NA-propagating twins. The _any pair above
    # is unchanged and still reads a never-asked person-wave as a zero;
    # the _obs pair is NA there. Pair a rate only with another rate on
    # the same side of that choice.
    training_module_asked, training_any_reported,
    jttpewt_obs, jttpeot_obs,
    any_of(c("jttopot_raw", "jttopwt_raw")),   # other-place items, for a measured O4
    training_own_time_obs, training_paid_time_obs,
    any_of(c("chance_leave_job", "chance_lose_job", "chance_find_job")),
    
    # === Networking / social capital (created in analysis script) ===
    any_of(c("club_member", "social_freq", "social_freq_01",
             "high_social", "networking_index")),
    
    # === Children & care (original — time-varying) ===
    num_dependent_children, num_children_under15,
    num_children_0to4, num_children_5to9, num_children_10to14,
    num_children_15to24,
    has_young_children, active_caregiver, is_mother, is_father,
    housework_hours, care_intensity,
    hours_lshrcar, hours_lshrchd,
    care_own_children_hours, care_other_hours, total_unpaid_hours,
    total_unpaid_hours_raw, total_unpaid_hours_capped,
    commute_hours, volunteer_hours,
    # v21: hh_super_balance, not super_balance. hwsupei is a household
    # aggregate and supports no individual comparison.
    hh_super_balance, hh_net_worth, hh_net_worth_pos, hh_net_worth_neg,
    in_wealth_module, wealth_module_wave,
    
    # === Non-resident children (original) ===
    has_nonresident_children_under18, has_any_nonresident_children,
    
    # === Fertility history (NEW — absorbing) ===
    total_children_ever_had,
    num_own_resident_children, num_own_nonresident_children, total_own_children,
    # v21.42. The pre-fix value, carried ONLY so the impact diagnostic can
    # MEASURE what the NA fix moved rather than assert it. The diagnostic is
    # guarded on this column's presence, so leaving it out of the select
    # made its absence look like "no rows affected" - which is exactly how
    # the first v21.42 run read.
    total_own_children_narm0,
    age_youngest_own_child, total_children_died,
    num_step_foster_grand,
    
    # Own children by age group
    own_res_children_0to4, own_res_children_5to14,
    own_res_children_15to24, own_res_children_25plus,
    own_nonres_children_0to4, own_nonres_children_5to14,
    own_nonres_children_15to24, own_nonres_children_25plus,
    
    # Child ages (for age at first birth)
    age_resident_child_1, age_resident_child_2, age_resident_child_3,
    age_nonresident_child_1, age_nonresident_child_2,
    
    # === Absorbing parenthood indicators (NEW) ===
    # Absorbing means never switching back. It does NOT mean constant
    # within person: these are the status AT a wave, and a mother's
    # pre-birth waves are correctly 0 here.
    # v21.49: ever_parent counts own children only; ever_parent_ref keeps
    # the old rule, which also counted household children under 15.
    ever_parent, ever_parent_ref, ever_mother, ever_father,
    motherhood_status, fatherhood_status,
    approx_age_at_first_birth, age_oldest_child,
    
    # === Lifetime parenthood indicators ===
    # The same facts as properties OF a person. Use these for any
    # cross-section, group mean or comparison group meant to exclude the
    # treated; use the pair above for anything indexed by event time. The
    # crosstab printed upstream gives the size of the difference.
    ever_parent_life, ever_mother_life, ever_father_life,
    motherhood_status_life, fatherhood_status_life,
    
    # === Group classifications (NEW) ===
    group_ever, group_current, group_3way_women,
    group_ever_life, group_4way_women_life,
    
    # === Parenting attitudes (NEW) ===
    parenting_resp, parenting_hard, parenting_tired,
    parenting_trapped, parenting_fairshare,
    
    # === Pregnancy (NEW — fertility module waves only) ===
    currently_pregnant, partner_pregnant, self_or_partner_pregnant,
    expected_birth_month, expected_birth_year,
    
    # === Life events (all waves; v17 names) ===
    event_pregnancy, event_child,
    event_married, event_separated,
    event_job_change, event_fired, event_moved,
    had_pregnancy_this_wave, had_child_this_wave,
    got_married_this_wave,
    separated_this_wave, changed_job_this_wave,
    
    # === v17 event dating within the 12-month reference window ===
    birth_q1, birth_q2, birth_q3, birth_q4, birth_q_declined,
    birth_quarter, birth_quarter_multi,
    preg_q1, preg_q2, preg_q3, preg_q4, preg_q_declined,
    preg_quarter, preg_quarter_multi,
    # v19: the strict "reported and carries no quarter" indicators. Use
    # these for event dating, not the NA-box flags above.
    birth_undated, birth_q_declined_dated, birth_q_n_set,
    preg_undated,  preg_q_declined_dated,  preg_q_n_set,
    # v20: *_quarter_event is the reporter-restricted quarter and is the
    # one event-time code must read. *_quarter_orphan flags the dated
    # person-waves that never reported the event.
    birth_quarter_event, birth_quarter_orphan,
    preg_quarter_event,  preg_quarter_orphan,
    any_of("post_birth_window_share"),
    
    # === v17 T-series arrivals ===
    nlf_reason_code, nlf_reason_maternity,
    enrolled_in_course, full_time_student,
    # v21: NA at wave 24, where both study items change instrument.
    enrolled_in_course_comparable, full_time_student_comparable,
    # v21.3: 0/1 indicators. The two above them are raw 1/2 codes.
    enrolled_in_course_yes, full_time_student_yes,
    study_item_regime, educ_trend_comparable,
    # v21.14a FIX. training_gate_regime is written onto panel_full in the
    # restriction block beside educ_trend_comparable, and the final select
    # named its sibling and not it, so it existed on panel_full and never
    # reached analysis_sample. That is edit site 3 of the six, and the
    # arrival contract caught it by name before the saves ran.
    training_gate_regime, occ_origin_regime,
    training_self_funded, training_self_funded_z,
    training_aim_general, training_aim_current,
    training_aim_future,
    occupation_2digit, occupation_status_score,
    # v21.12: the origin of a move, plus which questionnaire answered.
    occ2_origin, occstat_origin, occ_origin_source, occstat_change,
    occ2_origin_unemp, occ2_origin_nonemp, occ2_origin_jobchange,
    occstat_origin_unemp, occstat_origin_nonemp, occstat_origin_jobchange,
    # v21.12: paid parental leave. The first two resolve an ambiguity
    # INSIDE the earnings measure and are the reason for this batch.
    ppl_in_wages, ppl_in_fy_income,
    ppl_entitled_mat, ppl_entitled_pat,
    ppl_received_cur, ppl_received_fy,
    ppl_amount_cur, ppl_weeks_cur, ppl_amount_fy, ppl_weeks_fy,
    ppl_howanswered_fy, ppl_howanswered_cur,
    weekly_earnings_inc_ss, salary_sacrificed_weekly,
    wage_imputed, wage_ss_imputed, fy_wage_imputed,
    # v21.32: employer non-cash benefits, the third component of
    # compensation. Named explicitly rather than through any_of(),
    # because tolerating their absence here is edit site 3 of the six and
    # is how training_gate_regime was built and then not delivered.
    noncash_benefits_weekly, noncash_benefits_weekly_preimp, noncash_imputed,
    noncash_other_jobs_weekly, noncash_other_jobs_weekly_preimp,
    noncash_other_imputed, noncash_total_weekly,
    noncash_any_value, noncash_any, noncash_share_of_comp,
    # v21.33: the gate and its twins. A design on the LEVEL of non-cash
    # benefits should use the _z family; the ungated columns mix a
    # measured zero with a DV zero assigned to the non-employed.
    noncash_module_asked,
    noncash_benefits_weekly_z, noncash_total_weekly_z, noncash_any_value_z,
    weekly_compensation, real_weekly_compensation,
    real_noncash_benefits_weekly,
    hourly_compensation_clean, real_hourly_compensation,
    ln_hourly_compensation_real,
    employer_childcare_weekly, employer_childcare,
    ppl_in_wages_imp, ppl_in_wages_impflag,
    ppl_in_fy_imp, ppl_in_fy_impflag,
    interview_date, interview_year, interview_month, interview_doy,
    ref_window_start,
    paid_nonannual_leave_days,
    
    # === Other parent / partner (NEW) ===
    other_parent_employment, living_with_partner_last,
    any_of(c("household_id", "partner_id")),
    
    # === Parental background (instruments) ===
    mother_educ_years, father_educ_years,
    
    # === Geography & weights ===
    state, analysis_weight, population_weight, longitudinal_weight,
    
    # === Derived interactions ===
    educ_centered, educ_centered_sq,
    educ_female, educ_mother, educ_treatment, educ_precarious,
    educ_ever_mother, educ_ever_father,
    
    # === Period ===
    period
  )



# 
# Fix education_level to have unique categories
analysis_sample <- analysis_sample %>%
  mutate(
    education_level = case_when(
      educ_years >= 19.5 ~ "Doctorate",
      educ_years >= 18 & educ_years < 19.5 ~ "Masters",
      educ_years >= 17 & educ_years < 18 ~ "Graduate_Diploma_Certificate",
      educ_years >= 16.5 & educ_years < 17 ~ "Bachelor_Honours",
      educ_years >= 16 & educ_years < 16.5 ~ "Bachelor",
      educ_years >= 15 & educ_years < 16 ~ "Advanced_Diploma",
      educ_years >= 14.5 & educ_years < 15 ~ "Diploma",
      educ_years >= 14 & educ_years < 14.5 ~ "Diploma_Other",  # Or "Registered_Nurse" if that's what it represents
      educ_years >= 13.5 & educ_years < 14 ~ "Certificate_IV",
      educ_years >= 13 & educ_years < 13.5 ~ "Certificate_III",
      educ_years >= 12.5 & educ_years < 13 ~ "Certificate_II",
      educ_years == 12 ~ "Year_12",                # MUST precede the Cert_I band:
      educ_years > 12 & educ_years < 12.5 ~ "Certificate_I",  # first-match would
      # otherwise swallow all 12s
      educ_years == 11 ~ "Year_11",
      educ_years == 10 ~ "Year_10",
      educ_years == 9 ~ "Year_9",
      educ_years == 8 ~ "Year_8",
      educ_years == 7 ~ "Year_7",
      educ_years == 6 ~ "Primary_Complete",
      educ_years == 5 ~ "Primary_Incomplete",
      TRUE ~ "Other"
    )
  )

# Verify no duplicates
check_duplicates <- analysis_sample %>%
  group_by(education_level) %>%
  summarise(
    mean_years = mean(educ_years),
    min_years = min(educ_years),
    max_years = max(educ_years),
    n = n()
  ) %>%
  arrange(desc(mean_years))

print(check_duplicates)


education_order <- c(
  "Other",
  "Primary_Incomplete", "Primary_Complete",
  "Year_7", "Year_8", "Year_9",
  "Year_10", "Year_11", "Year_12",
  "Certificate_I", "Certificate_II", "Certificate_III", "Certificate_IV",
  "Diploma_Other", "Diploma", "Advanced_Diploma",
  "Bachelor", "Bachelor_Honours",
  "Graduate_Diploma_Certificate", "Masters", "Doctorate"
)
# guard: every produced category must be a level, or factor() silently NAs it
stopifnot(all(unique(analysis_sample$education_level) %in% education_order))

analysis_sample$education_level <- factor(
  analysis_sample$education_level,
  levels = education_order,
  ordered = TRUE
)

education_pathway_order <- c(
  "Primary_Incomplete", "Primary_Complete", "Year_7", "Year_8", "Year_9",
  "Below_Year_10", "Year_10", "Year_11", "Year_12",
  "Certificate_II", "Cert_III_Trade", "Cert_III_Technical", "Cert_III_Nursing", "Certificate_III",
  "Cert_IV_Trade", "Cert_IV_Technical", "Certificate_IV",
  "Diploma_Trade", "Diploma_Teaching", "Diploma_Nursing", "Diploma", "Advanced_Diploma", "AdvDiploma_Trade",
  "Bachelor_General", "Bachelor_Honours_General", "Bachelor_Nursing", "Bachelor_Teaching",
  "GradDipCert_General", "GradDipCert_Nursing", "GradDipCert_Teaching",
  "Masters_General", "Masters_Teaching",
  "Doctorate_General", "Doctorate_Teaching",
  "Registered_Nurse"
)


analysis_sample$education_pathway <- factor(
  analysis_sample$education_pathway,
  levels = education_pathway_order,
  ordered = TRUE
)

# =====================================================================
# WRITE ALL ANALYSIS DATASETS (single source of truth)
# =====================================================================
stopifnot(min(analysis_sample$wave) <= 12, max(analysis_sample$wave) >= 24)

# ---- v21.11 ARRIVAL CONTRACT ----------------------------------------
# Before the saves, so a failure costs the diagnostics already read and
# not a panel that would then be used.
if (length(EXPECTED_NEW_COLUMNS)) {
  .want  <- EXPECTED_NEW_COLUMNS
  .gone  <- setdiff(.want, names(analysis_sample))
  .empty <- setdiff(.want, .gone)
  .empty <- .empty[vapply(.empty,
                          function(cl) all(is.na(analysis_sample[[cl]])),
                          logical(1))]
  cat("\n--- Arrival contract:", length(.want), "declared column(s) ---\n")
  if (!length(.gone) && !length(.empty)) {
    cat("  All present and populated. Clear EXPECTED_NEW_COLUMNS for the\n")
    cat("  next build; v2_expected is the permanent register.\n")
  } else {
    if (length(.gone))
      cat("  ABSENT from analysis_sample: ",
          paste(sort(.gone), collapse = ", "),
          "\n  The mutate ran and the final select() does not name them, or\n",
          "  the mutate never ran. Edit site 3 in 01's Part 0c.\n", sep = "")
    if (length(.empty))
      cat("  PRESENT but ALL-NA: ", paste(sort(.empty), collapse = ", "),
          "\n  The column exists and its mnemonic did not resolve. Check the\n",
          "  absence audit above for the name the map asked for.\n", sep = "")
    stop(BUILD_TAG, " arrival contract: ", length(.gone), " absent and ",
         length(.empty), " all-NA of ", length(.want),
         " declared column(s), named above. Fix the edit site rather than ",
         "the declaration.", call. = FALSE)
  }
  rm(.want, .gone, .empty)
}

# v21: the build tag and the restriction manifest travel WITH the panel.
# A script that reads a panel from disk should not have to infer which
# build wrote it, and 00_compat_v21.R checks these before it guards
# anything.
# v21.28: the jbcasab scratch column is working state, not a deliverable.
# Dropped here rather than in the select so it cannot be forgotten if the
# select is reorganised.
if (".jbcasab_num" %in% names(analysis_sample))
  analysis_sample$.jbcasab_num <- NULL

# v21.29. THE AIM FAMILY, DECLARED, so a downstream script can check what
# it is getting rather than discover an absence as an all-NA column.
attr(analysis_sample, "training_aim_cols") <- list(
  items      = c("training_aim_future", "training_aim_current",
                 "training_aim_general", "training_aim_getstarted",
                 "training_aim_maintain", "training_aim_safety"),
  indices    = c("promotion_training", "maintenance_training",
                 "promotion_training_wide", "maintenance_training_wide"),
  gated      = c("promotion_training_z", "maintenance_training_z",
                 "promotion_training_wide_z", "maintenance_training_wide_z",
                 "training_aim_current_z", "training_aim_general_z"),
  gate       = "aim_battery_asked",
  nonresponse = "training_aim_nonresponse",
  not_an_aim = c("training_self_funded", "training_self_funded_z"),
  mapping    = c(promotion = "jttrpfj", promotion_wide = "jttrpfj|jttrhgs",
                 maintenance = "jttrmps", maintenance_wide = "jttrmps|jttrhsc"),
  note       = paste("Ungated columns are observed only where the person",
                     "trained and CONDITION ON THE OUTCOME. Use the _z",
                     "family for event studies. jttrisc and jttrdsg are in",
                     "neither index. training_self_funded is the cost item,",
                     "and training_self_funded_z is its gated twin: use the",
                     "_z form for a level or event-time claim about who met",
                     "the fees, and the ungated form only for a composition",
                     "share among trainees. jttrcst is the ONLY item in the",
                     "module that speaks to fees; jttpewt and jttpeot",
                     "separate training by place and time, not by payer.",
                     "From v21.48 a trainee who answered don't know or",
                     "refused to the battery (training_aim_nonresponse = 1)",
                     "is NA on all six aim items, not zero."))
stopifnot(all(c(attr(analysis_sample, "training_aim_cols")$indices,
                attr(analysis_sample, "training_aim_cols")$gated,
                attr(analysis_sample, "training_aim_cols")$gate,
                attr(analysis_sample, "training_aim_cols")$nonresponse)
              %in% names(analysis_sample)))

# v21.31: the precarious scratch column is working state.
if (".prec_n" %in% names(analysis_sample)) analysis_sample$.prec_n <- NULL

# v21.32. THE COMPENSATION LADDER, DECLARED, for the same reason the aim
# family is: three outcomes differing by one component each is exactly
# the arrangement in which a table silently mixes two of them. A
# downstream script can read which rung it is standing on rather than
# infer it from a column name.
attr(analysis_sample, "compensation_cols") <- list(
  published = "weekly_earnings",
  rungs     = c("weekly_earnings", "weekly_earnings_inc_ss",
                "weekly_compensation"),
  hourly    = c("ln_hourly_wage_real", "ln_hourly_compensation_real"),
  components = c(sacrifice = "salary_sacrificed_weekly",
                 noncash   = "noncash_total_weekly",
                 business  = "fy_business_income"),
  noncash_parts = c("noncash_benefits_weekly",
                    "noncash_other_jobs_weekly"),
  imputation = c("wage_imputed", "wage_ss_imputed", "noncash_imputed",
                 "noncash_other_imputed"),
  base_note = paste("weekly_earnings and weekly_earnings_inc_ss are ALL",
                    "JOBS (wscei/wsces). noncash_benefits_weekly is the",
                    "MAIN JOB (nbcmi); noncash_total_weekly adds the",
                    "other-jobs part (nbcoi) so the total sits on the",
                    "all-jobs base. Business income is ANNUAL and is not",
                    "in weekly_compensation."),
  note = paste("weekly_earnings is the published outcome and is",
               "unchanged. hourly_compensation_clean uses the SAME $10",
               "and $500 bounds as hourly_wage_clean, so any gap between",
               "the two ln series is the benefits and not a different",
               "top code. The samples are not identical; condition on",
               "both being observed for a like-for-like comparison."))
stopifnot(all(c(attr(analysis_sample, "compensation_cols")$rungs,
                attr(analysis_sample, "compensation_cols")$hourly)
              %in% names(analysis_sample)))

attr(analysis_sample, "build_tag")      <- BUILD_TAG
attr(analysis_sample, "restrictions")   <- RESTRICTIONS
# v21.4: the measured verdict, so the compat guard reads what this build
# found rather than holding its own copy of the list.
attr(analysis_sample, "entrant_only_cols") <- .v21_facts$entrant_only_cols
# v21.6. Two declarations a loader can make and cannot enforce.
#
# training_gate_cols names the pair that reads a never-asked person-wave
# as a zero, so 00_compat_v21.R can require the declaration the same way
# it does for the entrant-only columns.
attr(analysis_sample, "training_gate_cols") <-
  c("jttpewt_any", "jttpeot_any", "training_own_time", "training_paid_time")
# v21.7: the facts list now also carries the E6 consistency counts, the
# E6 base rate among the employed, and structural_zero_note, which is
# the sentence a design setting allow_structural_zero = TRUE has to
# print. Reading it off the panel keeps one wording in circulation
# instead of three.
attr(analysis_sample, "training_gate_facts") <- .v21_facts$training_gate
# experience_level_cols names every column carrying the ehtjb retrospect,
# which shows a steep entry-cohort gradient: mothers 1.72 years in the
# 2019 intake against 4.93 in the original one, part age composition and
# part recall vintage. 00_compat's OLF_LEVEL_VARS knows about
# actual_experience and does not know that experience_years IS
# actual_experience wherever ehtjb resolves, nor that
# 02_fte_experience.R's base_exp_cal is experience_years at the person's
# first wave and that the four exp_hybrid_* stocks are built on it.
# ADD THESE NAMES TO OLF_LEVEL_VARS IN 00_compat_v21.R.
attr(analysis_sample, "experience_level_cols") <- c(
  "experience_years", "actual_experience", "years_since_education",
  "years_not_in_lf", "years_unemployed", "nonemployment_years",
  # built downstream in 02_fte_experience.R, named here because this is
  # where the vintage enters and because nothing else declares them
  "base_exp_cal", "exp_hybrid_cal", "exp_hybrid_fte",
  "exp_hybrid_fte_obs", "exp_hybrid_one_obs")
attr(analysis_sample, "restriction_constants") <- list(
  firm_size_break_wave  = FIRM_SIZE_BREAK_WAVE,
  study_item_break_wave = STUDY_ITEM_BREAK_WAVE,
  wealth_module_waves   = WEALTH_MODULE_WAVES,
  olf_level_comparable  = OLF_LEVEL_COMPARABLE,
  super_is_household    = SUPER_IS_HOUSEHOLD)

# (1) Full-window panel for MASTER_hh.R Parts A-F (name kept for
#     drop-in compatibility with the master's readRDS call)
# THE PATH, NOT JUST THE FILENAME. .out_path() resolves to PROJ_ROOT when the
# contract is loaded and to getwd() otherwise, so where this lands depends on
# which of those the session happens to have set. Reporting the bare
# filename made that
# invisible: on 5 September this printed "Saved hilda_panel_data_W12_W24
# _slim.rds: 134,131 rows" while every copy the contract searches was still
# dated 25 August, and the discrepancy only surfaced two stages later when
# 03c compared timestamps. A run cannot be checked against a location it
# does not print.
#
# WHY IT MATTERS BEYOND TIDINESS. 03_common resolves the panel through
# TD_FRAME_FILES, which is file.path(c(DIR_DATA, PROJ_ROOT, "."), ...). If
# this writes anywhere else, a script running in the SAME session still
# works, because the panel is in memory; a script running as a separate
# process does not, and RUN_ORDER runs them as separate processes. The
# failure is therefore invisible in an interactive session and certain in a
# batch one.
.pan_out <- .out_path("hilda_panel_data_W12_W24_slim.rds")
saveRDS(analysis_sample, .pan_out)
cat(sprintf("Saved hilda_panel_data_W12_W24_slim.rds: %s rows (waves %d-%d)\n",
            format(nrow(analysis_sample), big.mark = ","),
            min(analysis_sample$wave), max(analysis_sample$wave)))
cat("  to ", normalizePath(.pan_out, winslash = "/", mustWork = FALSE), "\n", sep = "")
cat("  .out_path resolved through ",
    if (exists("PROJ_ROOT", envir = globalenv())) "PROJ_ROOT" else "getwd()",
    "\n", sep = "")

# =====================================================================
# v21.45. PANEL FINGERPRINT, AND THE DIFF AGAINST THE PREVIOUS BUILD
#
# A forced re-extract under an unmoved tag is a claim: that the cache was
# honest and the panel is unchanged. This block tests the claim instead
# of asserting it. It is cheap enough to run on every build, and running
# it on every build is what makes the comparison possible at all: the
# fingerprint has to exist BEFORE the run you want to check.
#
# WHAT IS FINGERPRINTED. Per column, the missing count, and a summary
# that suits the type: the sum for numerics, the distinct count for
# everything else. Together those catch a column that gained rows, lost
# observations, changed coding or silently became all-NA, which is the
# family of failures a stale checkpoint produces. It is not a hash of
# the data and does not need to be; a stale cache changes aggregates.
# =====================================================================
# A fingerprint must never be the thing that breaks a build, so every
# column is wrapped. Logicals get summed rather than counted distinct: a
# column flipping wholesale from TRUE to FALSE keeps two distinct values
# and would otherwise pass.
.fp_one <- function(x) tryCatch({
  if (is.numeric(x))
    c(n_missing = sum(is.na(x)), summary_stat = sum(as.numeric(x), na.rm = TRUE))
  else if (is.logical(x))
    c(n_missing = sum(is.na(x)), summary_stat = sum(x, na.rm = TRUE))
  else
    c(n_missing = sum(is.na(x)), summary_stat = length(unique(x[!is.na(x)])))
}, error = function(e) c(n_missing = NA_real_, summary_stat = NA_real_))
.fp <- data.frame(
  column = names(analysis_sample),
  do.call(rbind, lapply(analysis_sample, .fp_one)),
  row.names = NULL, stringsAsFactors = FALSE)
.fp$summary_stat <- signif(.fp$summary_stat, 12)

.fp_head <- data.frame(
  key = c("build_tag", "checkpoint_version", "force_reextract",
          "waves_extracted", "waves_cached", "rows", "persons",
          "wave_min", "wave_max", "n_columns", "ever_parent_1"),
  value = c(BUILD_TAG,
            as.character(.v21_facts$checkpoint_version),
            as.character(.v21_facts$force_reextract),
            paste(.v21_facts$waves_processed, collapse = "|"),
            paste(.v21_facts$waves_cached, collapse = "|"),
            nrow(analysis_sample),
            length(unique(analysis_sample$person_id)),
            min(analysis_sample$wave), max(analysis_sample$wave),
            ncol(analysis_sample),
            sum(analysis_sample$ever_parent == 1, na.rm = TRUE)),
  stringsAsFactors = FALSE)
cat("\n--- Panel fingerprint ---\n")
print(.fp_head, row.names = FALSE)

.fp_path <- .out_path("panel_fingerprint.csv")
.fp_prev <- if (file.exists(.fp_path))
  tryCatch(utils::read.csv(.fp_path, stringsAsFactors = FALSE),
           error = function(e) NULL) else NULL

if (is.null(.fp_prev)) {
  cat("  No previous fingerprint, so nothing to compare. This build is\n")
  cat("  the baseline for the next one.\n")
} else {
  .m <- merge(.fp_prev, .fp, by = "column", all = TRUE,
              suffixes = c("_prev", "_now"))
  .gone <- .m$column[is.na(.m$summary_stat_now)]
  .new  <- .m$column[is.na(.m$summary_stat_prev)]
  .both <- .m[!is.na(.m$summary_stat_now) & !is.na(.m$summary_stat_prev), ]
  # Relative tolerance, elementwise. An exact test would flag a column on
  # floating-point noise in a sum over 134,000 rows and teach the reader
  # to ignore the block.
  .tol <- 1e-9
  .moved <- .both$column[
    .both$n_missing_prev != .both$n_missing_now |
      abs(.both$summary_stat_prev - .both$summary_stat_now) >
        .tol * pmax(1, abs(.both$summary_stat_prev))]
  .moved <- .moved[!is.na(.moved)]
  cat(sprintf("  Against the previous fingerprint: %d column(s) dropped, %d added,\n  %d changed of %d compared.\n",
              length(.gone), length(.new), length(.moved), nrow(.both)))
  if (length(.gone)) cat("    dropped: ", paste(head(.gone, 20), collapse = ", "), "\n", sep = "")
  if (length(.new))  cat("    added:   ", paste(head(.new, 20), collapse = ", "), "\n", sep = "")
  if (length(.moved)) {
    cat("    changed:\n")
    print(head(.both[.both$column %in% .moved,
                     c("column", "n_missing_prev", "n_missing_now",
                       "summary_stat_prev", "summary_stat_now")], 30),
          row.names = FALSE)
  }
  # v21.45. The verdict fires on a FULL extraction however it was forced.
  # A tag bump and FORCE_REEXTRACT reach the same place by different
  # routes, and the reading of the comparison is the same either way; a
  # PARTIAL extraction is a different situation and gets no verdict,
  # because a cached wave and a fresh one in the same panel means the
  # comparison cannot attribute a difference to either.
  .full_extract <- length(.v21_facts$waves_processed) == length(WAVES)
  .partial_extract <- length(.v21_facts$waves_processed) > 0 && !.full_extract
  # v21.48. Split the differences into the ones this build declared in
  # EXPECTED_PANEL_CHANGES and the rest. Only the rest can mean a stale
  # cache or an edit that reached further than intended.
  .diff_all   <- unique(c(.gone, .new, .moved))
  .declared   <- get0("EXPECTED_PANEL_CHANGES", ifnotfound = character(0))
  .unexpected <- setdiff(.diff_all, .declared)
  .quiet      <- setdiff(.declared, .diff_all)
  if (.full_extract) {
    if (!length(.diff_all)) {
      cat("\n  FULL RE-EXTRACT REPRODUCED THE PANEL EXACTLY. The caches were\n")
      cat("  honest, nothing downstream is invalidated by this build, and\n")
      cat("  the presence and absence audits above are the first ones in\n")
      cat("  this build series that describe the panel rather than the run.\n")
      cat("  That is the answer the manual-bump discipline has been assumed\n")
      cat("  to give since the hash last moved at v21.34, and it is now\n")
      cat("  measured rather than assumed.\n")
    } else if (!length(.unexpected)) {
      cat("\n  FULL RE-EXTRACT CHANGED ONLY THE DECLARED COLUMNS. Every\n")
      cat("  difference above is in EXPECTED_PANEL_CHANGES, so this build\n")
      cat("  changed what it meant to change and nothing else. Re-run the\n")
      cat("  scripts that read those columns, then clear the list.\n")
      if (length(.quiet)) {
        cat("  Declared but unchanged, which is fine when no person-wave\n")
        cat("  reaches them:\n")
        cat(paste0(strwrap(paste(.quiet, collapse = ", "), width = 66,
                           prefix = "    "), "\n"), sep = "")
      }
    } else {
      cat("\n  *** FULL RE-EXTRACT DID NOT REPRODUCE THE PANEL outside the\n")
      cat("      columns this build declared. Undeclared differences:\n")
      cat(paste0(strwrap(paste(utils::head(.unexpected, 30), collapse = ", "),
                         width = 66, prefix = "        "), "\n"), sep = "")
      cat("      Nothing else in the wave mutate was meant to change, so a\n")
      cat("      difference here means a cached wave was stale and some\n")
      cat("      earlier edit never ran on it, or an edit in this build\n")
      cat("      reached further than it should have.\n")
      cat("      THIS BUILD IS THE CORRECT ONE. Every downstream script from\n")
      cat("      02 onward has to be re-run, and any result already drawn\n")
      cat("      from the previous panel is drawn from a panel built to\n")
      cat("      rules the file no longer contains. The changed columns\n")
      cat("      above name where it happened; the previous checkpoints are\n")
      cat("      still on disk under their old version tag if you want to\n")
      cat("      confirm which wave carried it.\n")
    }
  } else if (.partial_extract) {
    cat("\n  PARTIAL EXTRACT: ", length(.v21_facts$waves_processed), " of ",
        length(WAVES), " waves ran, so no verdict is drawn. A difference\n",
        "  below cannot be attributed to a stale cache or to a code change\n",
        "  when the panel mixes fresh and cached waves.\n", sep = "")
  }
  rm(.full_extract, .partial_extract, .diff_all, .declared, .unexpected,
     .quiet)
}
utils::write.csv(.fp, .fp_path, row.names = FALSE)
utils::write.csv(.fp_head, .out_path("panel_fingerprint_header.csv"),
                 row.names = FALSE)
cat("  fingerprint written to ", .fp_path, "\n", sep = "")
rm(.fp_one, .fp, .fp_head, .fp_path, .fp_prev)

# Will the next process find it? Answer now, in the loader, rather than
# leaving it to be inferred from a timestamp three stages downstream.
.pan_seen <- unique(normalizePath(file.path(
  c(if (exists("DIR_DATA")) DIR_DATA, if (exists("PROJ_ROOT")) PROJ_ROOT, "."),
  "hilda_panel_data_W12_W24_slim.rds"), winslash = "/", mustWork = FALSE))
if (!normalizePath(.pan_out, winslash = "/", mustWork = FALSE) %in% .pan_seen) {
  cat("  *** THIS IS NOT WHERE THE CONTRACT LOOKS. TD_FRAME_FILES searches\n")
  for (.q in .pan_seen) cat("      ", .q, "\n", sep = "")
  cat("      A script in this session will still work, because the panel is\n",
      "      in memory. A separate process will read whatever is at those\n",
      "      paths, which is an older build. Copy the file across, or rerun\n",
      "      with the working directory at the contract's root.\n", sep = "")
  rm(.q)
} else {
  cat("  this is one of the paths TD_FRAME_FILES searches, so a separate\n",
      "  process will resolve it.\n", sep = "")
}
rm(.pan_out, .pan_seen)

# (2) W15-24 subset for PART G / PART H (clean Reform 1 identification)
ext_sample <- dplyr::filter(analysis_sample, wave >= 15)
saveRDS(ext_sample, .out_path("hilda_panel_data_extended.rds"))
cat(sprintf("Saved hilda_panel_data_extended.rds: %s rows (waves %d-%d)\n",
            format(nrow(ext_sample), big.mark = ","),
            min(ext_sample$wave), max(ext_sample$wave)))
rm(ext_sample)

# =====================================================================
# SLIM TRAINING PANEL (input for training_event_study.R)
# =====================================================================
# Only the columns the training event study needs: identifiers, cohort
# inputs (female, ever_parent), employment, training outcomes, weights,
# and a couple of context variables. Indicators/counts stored as integer
# (halves in-memory size and compresses better); xz for smallest file.
# =====================================================================
# THE TRAINING EXTRACT IS NO LONGER WRITTEN  (v21.42)
# =====================================================================
# hilda_panel_data_training.rds was a ~133-column subset of analysis_sample,
# built by a second select(any_of(training_vars)). It is gone, with
# training_vars, the aim-battery check and the missing_tv note.
#
# WHY. It cost two silent failures. 00_contract.R listed it FIRST in
# TD_FRAME_FILES, so td_frame() resolved it, persisted it to handoff/, and
# every script inherited a frame with no education column: T8 resolved
# schooling as NA, the DML swap refused to run, and 03training_designs.R
# finished in six seconds. Before that the same select() had silently
# dropped the aim battery, because any_of() does not complain about a name
# it cannot find - correct for any_of, wrong for a specification.
#
# WHY NOT FIX IT. Each fix was sound and each left the trap: a second
# artefact, narrower than the panel, that resolves first if present. Adding
# missing columns one failure at a time converges on the full panel under a
# different name.
#
# hilda_panel_data_W12_W24_slim.rds is analysis_sample and is now the only
# frame. TD_FRAME_FILES names it alone.


# =====================================================================
# v2 VERIFICATION BLOCK
# =====================================================================
# Run once after the first v2 build. Each check corresponds to a claim
# the redesign relies on; if one fails, the corresponding estimate is not
# safe to report. Namespace protection first (avoid MASS/stats masking
# dplyr verbs).
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

cat("\n#####################################################\n")
cat("# v2 VERIFICATION                                   #\n")
cat("#####################################################\n")

# ---- 1. New columns present -----------------------------------------
# ---- item 16: COLUMN GUARD, RELOCATED --------------------------------
# The previous guard sat at the FOOT of the file, after every check had
# already executed, so a missing column surfaced as a confusing
# downstream error inside a summarise() rather than as a clear failure.
# This one runs BEFORE the checks and covers the columns the audit
# actually depends on, so it fails fast and names what is missing.
v7_audit_needs <- c(
  "hours_worked_clean", "hours_worked_main_job", "second_job_hours",
  "years_not_in_lf", "years_since_education", "olf_share",
  "real_weekly_earnings", "real_annual_income", "fy_income_gross",
  "real_fy_earnings_total", "ihs_earnings_total",
  "earnings_total_is_zero", "earnings_total_is_negative",
  "fy_business_income", "is_self_employed", "is_selfemp_narrow",
  "is_unpaid_family", "had_business_lfy", "emptype_raw",
  "experience_is_proxy", "ever_parent", "female", "employed",
  "in_wage_sample", "in_earnings_sample", "in_age_30_49"
)
v7_audit_missing <- setdiff(v7_audit_needs, names(analysis_sample))
if (length(v7_audit_missing))
  stop("Verification block references columns absent from the panel: ",
       paste(v7_audit_missing, collapse = ", "),
       ". The select() and the checks have drifted apart.", call. = FALSE)
cat("Column guard passed:", length(v7_audit_needs), "audit columns present.\n")

v2_expected <- c(
  "real_weekly_earnings", "real_annual_income",
  "ihs_weekly_earnings", "ihs_annual_income",
  "earnings_is_zero", "annual_income_is_zero", "earnings_source_flag",
  "salary_sacrifice", "fy_benefits", "hh_income_disposable",
  "fy_income_gross", "fy_income_disposable",
  "hours_worked_main_job", "second_job_hours", "weekly_annual_gap",
  "experience_source", "experience_is_proxy", "experience_ceiling_bound",
  "years_since_education", "years_unemployed", "years_not_in_lf",
  "olf_share", "unemp_share", "employment_share",
  "nonemployment_years", "history_components", "history_residual",
  "has_interruption", "has_olf_spell",
  "total_training_hours", "total_training_hours_z", "any_training",
  # v21.48: the gated days and courses twins and the aim non-response flag.
  "total_training_days_z", "total_courses_z", "training_aim_nonresponse",
  # v21.49: the per-wave values the parenthood and education fixes replace.
  "ever_parent_ref", "educ_years_ref",
  # v21.12. These are in v2_expected as the PERMANENT register;
  # EXPECTED_NEW_COLUMNS is the checklist for this build only and should
  # be cleared once it passes.
  "ppl_in_wages", "ppl_in_fy_income", "ppl_entitled_mat",
  "ppl_entitled_pat", "ppl_received_cur", "ppl_received_fy",
  "ppl_amount_fy", "ppl_weeks_fy", "paid_nonannual_leave_days",
  "occ2_origin", "occstat_origin", "occ_origin_source", "occstat_change",
  "ppl_howanswered_fy", "training_gate_regime",
  # v21.30: the v21.29 aim columns. Derived and selected since v21.29 and
  # in no register, so a later select edit could have dropped any of them
  # without the build noticing.
  "aim_battery_asked",
  "promotion_training_z", "maintenance_training_z",
  "promotion_training_wide", "maintenance_training_wide",
  "promotion_training_wide_z", "maintenance_training_wide_z",
  "interview_date", "interview_month", "occ_origin_regime",
  "ref_window_start",
  # v21.32: the non-cash family, permanent register. The conditional
  # members (noncash_any, employer_childcare*, the other-jobs trio) are
  # deliberately NOT here: this list is checked for PRESENCE, and a
  # column that exists and is all-NA passes it, so listing them would
  # assert nothing while a later select edit that dropped one would go
  # unnoticed either way. The arrival contract is where populated-ness
  # was tested, once, on the build that added them.
  "noncash_benefits_weekly", "noncash_benefits_weekly_preimp",
  "noncash_imputed", "noncash_total_weekly", "noncash_any_value",
  "noncash_share_of_comp", "weekly_compensation",
  "real_weekly_compensation", "hourly_compensation_clean",
  "real_hourly_compensation", "ln_hourly_compensation_real",
  # v21.6 gate columns. A missing one here is not cosmetic: without them
  # the only training outcomes on the panel are the ones that cannot tell
  # "did not train" from "was never asked".
  "training_module_asked", "training_any_reported",
  "jttpewt_obs", "jttpeot_obs",
  "training_own_time_obs", "training_paid_time_obs", "fulltime_emp",
  "in_earnings_sample", "in_age_30_49", "in_age_25_52", "in_age_21_52",
  # v21 restriction columns. If one of these is missing the restriction
  # is unenforceable downstream, which is worth the same failure as a
  # missing outcome.
  "firm_size_regime", "firm_size_comparable", "firm_size_post22",
  "study_item_regime", "educ_trend_comparable",
  "enrolled_in_course_comparable", "full_time_student_comparable",
  "enrolled_in_course_yes", "full_time_student_yes",
  "entry_wave", "entry_cohort", "d_olf_years", "olf_year",
  "olf_flow_valid", "hh_super_balance", "wealth_module_wave"
)
v2_missing <- setdiff(v2_expected, names(analysis_sample))
if (length(v2_missing)) {
  cat("FAIL: v2 columns missing:", paste(v2_missing, collapse = ", "), "\n")
} else {
  cat("OK: all", length(v2_expected), "v2 columns present\n")
}

# ---- 2. Age window --------------------------------------------------
cat(sprintf("\nAge range in panel: %d-%d (expected %d-%d)\n",
            min(analysis_sample$age, na.rm = TRUE),
            max(analysis_sample$age, na.rm = TRUE), AGE_MIN, AGE_MAX))
cat(sprintf("  Retained under the legacy 30-49 window: %s of %s (%.1f%%)\n",
            format(sum(analysis_sample$in_age_30_49 == 1, na.rm = TRUE), big.mark = ","),
            format(nrow(analysis_sample), big.mark = ","),
            100 * mean(analysis_sample$in_age_30_49 == 1, na.rm = TRUE)))

# ---- 3. Earnings are defined at zero for the non-employed ------------
# This is the property the whole earnings-based redesign rests on. If the
# non-employed carry NA rather than 0, the earnings event study inherits
# exactly the selection the wage-based design suffers from, and the Lee
# bounds cannot be dropped.
cat("\n--- Earnings defined at zero for the non-employed? ---\n")
zero_check <- analysis_sample %>%
  filter(!is.na(employed)) %>%
  group_by(employed) %>%
  summarise(
    n              = n(),
    pct_wkly_NA    = round(100 * mean(is.na(real_weekly_earnings)), 1),
    pct_wkly_zero  = round(100 * mean(real_weekly_earnings == 0, na.rm = TRUE), 1),
    pct_ann_NA     = round(100 * mean(is.na(real_annual_income)), 1),
    pct_ann_zero   = round(100 * mean(real_annual_income == 0, na.rm = TRUE), 1),
    .groups = "drop"
  )
print(as.data.frame(zero_check))
cat("Expect: employed==0 shows a HIGH pct_zero and LOW pct_NA.\n")
cat("If pct_NA is high instead, wscei/wsfei are missing rather than zero\n")
cat("for the non-employed and must be recoded to 0 before use.\n")

# ---- 4. Experience provenance by gender and parenthood ---------------
# If the Mincer-proxy fallback rate differs between mothers and fathers,
# part of the measured experience gap is a measurement artefact.
cat("\n--- Experience source by gender x parenthood ---\n")
if (all(c("experience_is_proxy", "ever_parent") %in% names(analysis_sample))) {
  prov <- analysis_sample %>%
    filter(!is.na(experience_is_proxy), !is.na(ever_parent)) %>%
    group_by(female, ever_parent) %>%
    summarise(n = n(),
              pct_mincer_proxy = round(100 * mean(experience_is_proxy), 1),
              pct_ceiling_bound = round(100 * mean(experience_ceiling_bound,
                                                   na.rm = TRUE), 1),
              .groups = "drop")
  print(as.data.frame(prov))
  cat("Report this table in the paper. A gendered gap in pct_mincer_proxy\n")
  cat("means the experience path is partly measurement, not behaviour.\n")
}

# ---- 5. Non-employment history coverage ------------------------------
# ehtuj (unemployment) and ehto (out of labour force) drive the
# interruption-scarring design. They must be reported SEPARATELY:
# unemployment is only weakly gendered, OLF time strongly so, and
# pooling them was what made v2's interruption rate look ungendered.
cat("\n--- Non-employment history (ehtuj / ehto) coverage ---\n")
interrupt <- analysis_sample %>%
  group_by(female, ever_parent) %>%
  summarise(
    n                = n(),
    pct_ehtuj_obs    = round(100 * mean(!is.na(years_unemployed)), 1),
    pct_ehto_obs     = round(100 * mean(!is.na(years_not_in_lf)), 1),
    mean_unemp_yrs   = round(mean(years_unemployed, na.rm = TRUE), 2),
    mean_olf_yrs     = round(mean(years_not_in_lf, na.rm = TRUE), 2),
    mean_exposure_yrs = round(mean(years_since_education, na.rm = TRUE), 2),
    med_olf_share    = round(median(olf_share, na.rm = TRUE), 3),
    pct_olf_gt0      = round(100 * mean(has_olf_spell, na.rm = TRUE), 1),
    med_residual     = round(median(history_residual, na.rm = TRUE), 2),
    pct_resid_near0  = round(100 * mean(abs(history_residual) < 0.5,
                                        na.rm = TRUE), 1),
    .groups = "drop"
  )
print(as.data.frame(interrupt))
cat("\nREAD THIS TABLE AS FOLLOWS:\n")
cat(" * pct_ehto_obs should be ~95%, matching pct_ehtuj_obs. If it is 0,\n")
cat("   the panel is STALE: rebuild with a bumped CHECKPOINT_VERSION.\n")
cat(" * mean_olf_yrs should be MUCH higher for mothers than fathers,\n")
cat("   while mean_unemp_yrs should be similar. That contrast is what\n")
cat("   makes OLF the right interruption measure.\n")
cat(" * pct_olf_gt0 for female==0 is the BINDING CONSTRAINT: it is the\n")
cat("   pool of interrupted men available to match to mothers. If it is\n")
cat("   in the low single digits, DROP the scarring design rather than\n")
cat("   report it underpowered.\n")
cat(" * med_residual should be near ZERO and pct_resid_near0 high: the\n")
cat("   identity is ehtse ~ ehtjb + ehtuj + ehto, so the three\n")
cat("   components should exhaust the exposure denominator. A large\n")
cat("   positive residual is time the components do not classify.\n")
cat(" * med_olf_share is the cohort-comparable version of mean_olf_yrs.\n")
cat("   Prefer it, or the within-person flows below, over the LEVEL,\n")
cat("   which varies by entry-cohort recall vintage.\n")

# Coverage by wave: ehto is a History variable anchored on a
# retrospective NPQ report at panel entry, so check it is populated in
# every wave rather than only in module years.
cat("\n--- ehto coverage by wave ---\n")
olf_by_wave <- analysis_sample %>%
  group_by(wave) %>%
  summarise(pct_ehto_obs = round(100 * mean(!is.na(years_not_in_lf)), 1),
            mean_olf_yrs = round(mean(years_not_in_lf, na.rm = TRUE), 2),
            .groups = "drop")
print(as.data.frame(olf_by_wave))

# Entry-cohort check. The NPQ item number differs at the wave 11, 19 and
# 24 refreshers, so the baseline stock is measured on a different
# instrument vintage for each intake. If mean_olf differs sharply by
# cohort conditional on sex, restrict the scarring design to WITHIN-PERSON
# changes in ehto rather than using the level.
# v21: entry_wave and entry_cohort are built in the restriction block and
# read off the panel here. Recomputing them was a second definition of
# the same quantity that could drift from the one the panel carries.
cat("\n--- ehto by entry cohort (recall-vintage check) ---\n")
cohort_chk <- analysis_sample %>%
  group_by(cohort = entry_cohort, female) %>%
  summarise(n = n(),
            pct_obs     = round(100 * mean(!is.na(years_not_in_lf)), 1),
            mean_olf    = round(mean(years_not_in_lf, na.rm = TRUE), 2),
            pct_olf_gt0 = round(100 * mean(has_olf_spell, na.rm = TRUE), 1),
            .groups = "drop")
print(as.data.frame(cohort_chk))

# ---- 5b. Within-person interruption flow ------------------------------
# The LEVEL of ehto carries all the base-stock recall error; the
# year-on-year CHANGE does not. A person whose ehto rises by ~1 between
# waves spent that year out of the labour force. These flow measures are
# the preferred inputs to the scarring design and line up directly with
# event-study horizons.
# v21: the increments are columns on the panel now, so this reads them
# rather than deriving them a second time. olf_flow_valid is the
# consecutive-wave restriction; an increment across a gap year is not an
# annual flow.
cat("\n--- Within-person history flows (annual increments) ---\n")
flows <- analysis_sample %>%
  filter(olf_flow_valid == 1) %>%
  mutate(flow_sum = history_flow_sum)
flow_chk <- flows %>%
  group_by(female) %>%
  summarise(n              = n(),
            med_flow_sum   = round(median(flow_sum, na.rm = TRUE), 2),
            med_d_exposure = round(median(d_exposure_years, na.rm = TRUE), 2),
            pct_sum_near_1 = round(100 * mean(abs(flow_sum - 1) < 0.5,
                                              na.rm = TRUE), 1),
            pct_olf_year   = round(100 * mean(d_olf_years > 0.5,
                                              na.rm = TRUE), 1),
            pct_unemp_year = round(100 * mean(d_unemp_years > 0.5,
                                              na.rm = TRUE), 1),
            .groups = "drop")
print(as.data.frame(flow_chk))
cat("med_flow_sum AND med_d_exposure should both be ~1: the three\n")
cat("components (paid + unemployed + OLF) should gain one year per wave\n")
cat("between them, matching the gain in the ehtse denominator. v3 read\n")
cat("~2 here because it summed the denominator with its own components.\n")
cat("Where the sum is not ~1, the history variables are being re-anchored\n")
cat("rather than incremented; flag those person-waves.\n")
cat("pct_olf_year for female==0 is the annual-flow analogue of the\n")
cat("binding constraint above.\n")

# ---- 5c. Wage and earnings LEVELS -------------------------------------
# Plausibility gate before any dollar figure is trusted. Australian
# full-time median hourly earnings are roughly $40-45 in 2024 dollars
# and median full-time weekly earnings roughly $1,600.
cat("\n--- Real hourly wage and earnings levels ---\n")
wage_lvl <- analysis_sample %>%
  filter(in_wage_sample == 1, wave == max(wave)) %>%
  summarise(n   = n(),
            hr_p10 = round(quantile(real_hourly_wage, .10, na.rm = TRUE), 1),
            hr_p50 = round(quantile(real_hourly_wage, .50, na.rm = TRUE), 1),
            hr_p90 = round(quantile(real_hourly_wage, .90, na.rm = TRUE), 1),
            med_weekly = round(median(real_weekly_earnings, na.rm = TRUE), 0),
            med_annual = round(median(real_annual_income, na.rm = TRUE), 0),
            med_fy_gross = round(median(fy_income_gross[fy_income_gross > 0],
                                        na.rm = TRUE), 0))
print(as.data.frame(wage_lvl))
cat("Expect hr_p50 ~$40-45, med_weekly ~$1,300-1,600, med_annual ~$75-80k.\n")
cat("If hr_p50 is ~$21 the wscei divisor was reapplied; if ~$85, doubled.\n")
cat("If med_annual is ~$150k the wsfei+wsfes double-count has returned.\n")

# Weekly-vs-annual reconciliation. Both measures are all-jobs, so this
# should centre near ZERO. A median near +0.5 means annual_income is
# doubled (the v2-v4 wsfei+wsfes bug); near -1.0 means weekly is halved
# (the v4 divisor bug). Either value means DO NOT PROCEED.
cat("\n--- Weekly x 52 vs annual reconciliation ---\n")
recon <- analysis_sample %>%
  filter(in_wage_sample == 1, real_weekly_earnings > 0,
         real_annual_income > 0) %>%
  mutate(ann_ratio = real_annual_income / (real_weekly_earnings * 52)) %>%
  group_by(female) %>%
  summarise(n = n(),
            med_ratio     = round(median(ann_ratio, na.rm = TRUE), 3),
            med_gap       = round(median(weekly_annual_gap, na.rm = TRUE), 3),
            pct_within_25 = round(100 * mean(abs(ann_ratio - 1) < 0.25,
                                             na.rm = TRUE), 1),
            .groups = "drop")
print(as.data.frame(recon))
cat("med_ratio should be ~1.0 and med_gap ~0. Dispersion is expected from\n")
cat("job changes, part-year work and pay rises; a clean 0.5 or 2.0 is not.\n")

# Which definition annual_income came from. wsfes (inc salary sacrifice)
# exists from wave 10, so on a W12-W24 panel this should be ~100%
# inc_salary_sacrifice. A mix would introduce a level break across waves.
cat("\n--- annual_income source ---\n")
print(as.data.frame(count(analysis_sample, earnings_source_flag)))
print(as.data.frame(
  analysis_sample %>%
    filter(in_wage_sample == 1, !is.na(salary_sacrifice)) %>%
    summarise(pct_any_ss  = round(100 * mean(salary_sacrifice > 0), 1),
              med_ss_if_gt0 = round(median(salary_sacrifice[salary_sacrifice > 0]),
                                    0))))

# ---- 5d. v8: business income and self-employment ----------------------
# Three things to confirm before the self-employment margin is used.
cat("\n--- Business income and self-employment ---\n")

# (a) Is EMPTYPE_EMPLOYEE set correctly? If the employee share is not
#     around 85-90% of the employed, the category coding is wrong.
#     jbmmply is a PQ item and its numbering differs between the CPQ and
#     PQ versions, so this is the check that catches a bad constant.
emp_split <- analysis_sample %>%
  filter(employed == 1, !is.na(emptype_raw)) %>%
  summarise(n              = n(),
            pct_employee   = round(100 * mean(is_employee, na.rm = TRUE), 1),
            pct_selfemp    = round(100 * mean(is_self_employed, na.rm = TRUE), 1),
            n_categories   = dplyr::n_distinct(emptype_raw))
print(as.data.frame(emp_split))
cat("Expect pct_employee around 85-90%. At wave 24 esempdt gives 86.7%\n")
cat("employee against 13.3% self-employed, consistent with Australian\n")
cat("self-employment near 15%. A wildly different value means\n")
cat("EMPTYPE_EMPLOYEE does not match the coding of whichever variable\n")
cat("V7_EMPTYPE points at: rerun with V7_DISCOVER <- TRUE and read the\n")
cat("value labels. NOTE esempdt (6 categories) and esempst (4) number\n")
cat("their categories DIFFERENTLY.\n")

# (a2) Broad versus narrow self-employment, and the cross-check.
# The gap between the two definitions is the employee-of-own-business
# group (codes 2-3), a legal form whose members appear in wsfei/wsfes as
# WAGES. If a result turns on which definition is used, say so.
cat("\n--- Self-employment definitions compared ---\n")
se_def <- analysis_sample %>%
  filter(employed == 1, !is.na(is_self_employed)) %>%
  summarise(n                = n(),
            pct_broad        = round(100 * mean(is_self_employed), 1),
            pct_narrow       = round(100 * mean(is_selfemp_narrow,
                                                na.rm = TRUE), 1),
            pct_unpaid_fam   = round(100 * mean(is_unpaid_family,
                                                na.rm = TRUE), 2),
            pct_own_bus_lfy  = round(100 * mean(had_business_lfy,
                                                na.rm = TRUE), 1))
print(as.data.frame(se_def))
cat("pct_broad - pct_narrow is the employee-of-own-business group.\n")
cat("pct_own_bus_lfy should be ROUGHLY COMPARABLE to pct_broad (both\n")
cat("measure self-employment, on different reference periods). A value\n")
cat("near 85% means the bifhave binary is inverted: F42 uses 1=Yes,\n")
cat("2=No, unlike many other HILDA binaries.\n")

# (a3) Agreement between the two INDEPENDENT measures. esempdt is
# CURRENT status; bifhave is LAST FINANCIAL YEAR, so perfect agreement
# is not expected and disagreement is informative rather than an error:
# someone self-employed last year but an employee now has business
# income and no current self-employment. Gross disagreement, though,
# would mean one of the two is being read wrongly, which is the failure
# the jbmmply episode showed is easy to miss with a single measure.
cat("\n--- esempdt vs bifhave cross-check ---\n")
xchk <- analysis_sample %>%
  filter(employed == 1, !is.na(is_self_employed), !is.na(had_business_lfy)) %>%
  count(is_self_employed, had_business_lfy) %>%
  mutate(pct = round(100 * n / sum(n), 1))
print(as.data.frame(xchk))
cat("Expect the two diagonal cells to dominate. Off-diagonal cells are\n")
cat("transitions into or out of self-employment across the reference\n")
cat("periods, not necessarily coding errors.\n")

# (b) Self-employment by gender and parenthood. The substantive reason
#     for adding this: self-employment is a post-birth flexibility
#     margin, so mothers should show a higher rate than never-mothers.
se_grp <- analysis_sample %>%
  filter(employed == 1, !is.na(is_self_employed)) %>%
  group_by(female, ever_parent) %>%
  summarise(n           = n(),
            pct_selfemp = round(100 * mean(is_self_employed), 1),
            .groups = "drop")
print(as.data.frame(se_grp))

# (c) Business income: sign, incidence, and what it adds to earnings.
#     med_business_if_nonzero should be POSITIVE overall; a negative
#     median would suggest the bifip/bifin subtraction is inverted.
bus <- analysis_sample %>%
  filter(!is.na(fy_business_income)) %>%
  summarise(n                = n(),
            pct_nonzero      = round(100 * mean(fy_business_income != 0), 1),
            pct_negative     = round(100 * mean(fy_business_income < 0), 2),
            med_if_positive  = round(median(fy_business_income[fy_business_income > 0]), 0),
            med_if_negative  = round(median(fy_business_income[fy_business_income < 0]), 0))
print(as.data.frame(bus))
cat("med_if_negative should be NEGATIVE. If it is positive, the\n")
cat("bifip/bifin netting has the wrong sign.\n")

# (d) What the total-earnings measure adds over wages alone. This is the
#     whole point: a mother who moves into her own business reads as an
#     earnings drop to zero on annual_income but not on
#     fy_earnings_total.
tot <- analysis_sample %>%
  filter(!is.na(real_fy_earnings_total), !is.na(real_annual_income)) %>%
  group_by(female) %>%
  summarise(n                 = n(),
            med_wages_only    = round(median(real_annual_income), 0),
            med_total         = round(median(real_fy_earnings_total), 0),
            pct_zero_wages_but_business = round(
              100 * mean(real_annual_income == 0 &
                           real_fy_earnings_total != 0), 2),
            pct_total_negative = round(100 * mean(earnings_total_is_negative,
                                                  na.rm = TRUE), 2),
            .groups = "drop")
print(as.data.frame(tot))
cat("pct_zero_wages_but_business is the group a wages-only measure\n")
cat("records as zero earnings when they are in fact still earning.\n")

# ---- 6. Hours: all jobs vs main job -----------------------------------
# jbhruc (ALL JOBS) minus jbmhruc (MAIN job) is the genuine second-job
# margin, unlike the weekly-vs-annual earnings gap, which compares two
# all-jobs measures on different reference periods.
cat("\n--- Hours: second-job margin (all jobs minus main job) ---\n")
mj <- analysis_sample %>%
  filter(in_wage_sample == 1, !is.na(second_job_hours)) %>%
  group_by(female, ever_parent) %>%
  summarise(n              = n(),
            med_all_jobs  = round(median(hours_worked_clean, na.rm = TRUE), 1),
            med_main_job   = round(median(hours_worked_main_job, na.rm = TRUE), 1),
            pct_second_job = round(100 * mean(second_job_hours > 0.5, na.rm = TRUE), 1),
            med_extra_hrs  = round(median(second_job_hours[second_job_hours > 0.5],
                                          na.rm = TRUE), 1),
            .groups = "drop")
print(as.data.frame(mj))
cat("pct_second_job is the share holding a second job: expect single\n")
cat("digits, somewhat higher for women. If it is exactly 0 for every\n")
cat("cell, jbhruc and jbmhruc are returning identical values and the\n")
cat("second-job margin is not recoverable from this pair.\n")

# ---- 7. Training intensity -------------------------------------------
cat("\n--- Training intensity coverage ---\n")
tr <- analysis_sample %>%
  filter(in_wage_sample == 1) %>%
  summarise(
    pct_any_training_obs = round(100 * mean(!is.na(any_training)), 1),
    pct_hours_obs        = round(100 * mean(!is.na(total_training_hours)), 1),
    # v21.9: the same coverage on the structural-zero version. The first
    # is observed only where the dosage is POSITIVE, so it is selected on
    # the outcome; the second adds the measured zeros and is not.
    pct_hours_z_obs      = round(100 * mean(!is.na(total_training_hours_z)), 1),
    mean_hours_if_obs    = round(mean(total_training_hours, na.rm = TRUE), 1),
    mean_hours_z         = round(mean(total_training_hours_z, na.rm = TRUE), 1)
  )
print(as.data.frame(tr))

# ---- 7b. Precarious score, on how many components ---------------------
# v21.45. The score is an available-case mean: a missing component leaves
# the numerator and the denominator together, so it does not impute, and
# a score below PRECARIOUS_MIN_COMPONENTS is NA rather than low. That
# makes the component count, not the score, the thing that moves when an
# input's missingness changes, and a build can shed thousands of scored
# person-waves with the score's own distribution barely moving.
#
# It moved at v21.44. has_irregular_hours is no longer an imputed zero,
# so every person-wave with a missing jbmsch now observes one component
# fewer than it did. Two groups come out of that, and they need separating
# because the table alone cannot tell them apart:
#
#   fell below the threshold   they were scored on a fabricated zero plus
#                              casual and 1 - permanent, which are the same
#                              instrument counted twice, then scaled by
#                              5/3. NA is the right answer and the loss of
#                              coverage is the fix working.
#   still at or above it       the score survives on a smaller denominator,
#                              so the same numerator is scaled up. The
#                              value changes; the row does not disappear.
#
# The attribution assumes the guard cost exactly ONE component, which is
# true of v21.44 and of nothing else. If a later fix makes a second
# component NA-able, the arithmetic below undercounts and needs revising
# rather than reusing.
if ("precarious_n_components" %in% names(analysis_sample)) {
  cat("\n--- Precarious score: components observed ---\n")
  .pc_tab <- as.data.frame(table(
    components = analysis_sample$precarious_n_components, useNA = "ifany"),
    stringsAsFactors = FALSE)
  .pc_tab$pct <- round(100 * .pc_tab$Freq / nrow(analysis_sample), 1)
  print(.pc_tab, row.names = FALSE)
  cat(sprintf("  Threshold is %d. Scored: %s. NA for too few components: %s.\n",
              PRECARIOUS_MIN_COMPONENTS,
              format(sum(!is.na(analysis_sample$precarious_score)),
                     big.mark = ","),
              format(sum(is.na(analysis_sample$precarious_score) &
                           !is.na(analysis_sample$precarious_n_components)),
                     big.mark = ",")))

  .n_comp <- analysis_sample$precarious_n_components

  if ("has_irregular_hours" %in% names(analysis_sample)) {
    .short   <- is.na(analysis_sample$has_irregular_hours)
    .crossed <- sum(.short & .n_comp == PRECARIOUS_MIN_COMPONENTS - 1L,
                    na.rm = TRUE)
    .rescale <- sum(.short & .n_comp >= PRECARIOUS_MIN_COMPONENTS,
                    na.rm = TRUE)
    cat(sprintf("  Short one component to the jbmsch guard: %s person-wave(s).\n",
                format(sum(.short), big.mark = ",")))
    cat(sprintf("    lost a score they would have had: %s\n",
                format(.crossed, big.mark = ",")))
    cat(sprintf("    kept a score, now on one component fewer: %s\n",
                format(.rescale, big.mark = ",")))
    cat("  Everything below that is short a component was already below the\n")
    cat("  threshold before the guard and is not attributable to it.\n")
  } else {
    cat("  has_irregular_hours is not on the panel, so the fall in coverage\n")
    cat("  cannot be attributed and the cell counts are an upper bound only.\n")
  }

  # v21.45. WHO IS BELOW THE THRESHOLD, AND WHY. The guard accounts for
  # almost none of it. jbcasab, jbmhl and jbmsl are EMPLOYEE items, so the
  # self-employed lose contract type and leave entitlement, and because
  # casual and 1 - permanent are the same item that is three of the five
  # components at once. What is left, schedule and underemployment, is two,
  # which is below the threshold by construction. The score is therefore
  # NA for the self-employed as a class rather than through non-response,
  # and it always has been. That matters for anything using the score as a
  # control on an event sample: self-employment among mothers trebles after
  # the birth, so the exclusion is concentrated in the treated group after
  # the event and is not orthogonal to treatment.
  if (all(c("is_self_employed", "employed") %in% names(analysis_sample))) {
    .emp_i  <- analysis_sample$employed == 1 & !is.na(analysis_sample$employed)
    .se_i   <- analysis_sample$is_self_employed == 1 &
      !is.na(analysis_sample$is_self_employed)
    .below  <- .n_comp < PRECARIOUS_MIN_COMPONENTS &
      !is.na(analysis_sample$precarious_n_components)
    cat(sprintf(paste0("  Below the threshold and EMPLOYED: %s, of which %s ",
                       "(%.1f%%) are\n  self-employed. Employed self-employed ",
                       "on the panel: %s, scored: %s.\n"),
                format(sum(.below & .emp_i), big.mark = ","),
                format(sum(.below & .emp_i & .se_i), big.mark = ","),
                100 * sum(.below & .emp_i & .se_i) / max(1, sum(.below & .emp_i)),
                format(sum(.emp_i & .se_i), big.mark = ","),
                format(sum(.emp_i & .se_i &
                             !is.na(analysis_sample$precarious_score)),
                       big.mark = ",")))
    cat("  A scored count near zero there is structural exclusion, not\n")
    cat("  missingness, and it should be declared wherever the score is used.\n")
  }

  cat("  casual and 1 - permanent are the same item, so contract type\n")
  cat("  carries 2 of the 5 points. The count does not adjust for that,\n")
  cat("  which is why a 2-component score is not worth having and the\n")
  cat("  threshold exists; see the note at the score's construction.\n")
} else {
  cat("\n*** precarious_n_components is not on the panel. It is built at\n")
  cat("    L5990; if it has been selected away again, the score's basis\n")
  cat("    is unreadable from the panel and this check cannot run.\n")
}

# ---- 8. Sample sizes for each redesign estimand ----------------------
cat("\n--- Estimation sample sizes (v2) ---\n")
sizes <- data.frame(
  sample = c("Broad (all ages)", "Broad, 30-49", "Wage sample",
             "Earnings sample", "Earnings sample, 30-49",
             "Has interruption history"),
  n = c(
    nrow(analysis_sample),
    sum(analysis_sample$in_age_30_49 == 1, na.rm = TRUE),
    sum(analysis_sample$in_wage_sample == 1, na.rm = TRUE),
    sum(analysis_sample$in_earnings_sample == 1, na.rm = TRUE),
    sum(analysis_sample$in_earnings_sample == 1 &
          analysis_sample$in_age_30_49 == 1, na.rm = TRUE),
    sum(!is.na(analysis_sample$has_interruption))
  )
)
sizes$persons <- c(
  dplyr::n_distinct(analysis_sample$person_id),
  dplyr::n_distinct(analysis_sample$person_id[analysis_sample$in_age_30_49 == 1]),
  dplyr::n_distinct(analysis_sample$person_id[analysis_sample$in_wage_sample == 1]),
  dplyr::n_distinct(analysis_sample$person_id[analysis_sample$in_earnings_sample == 1]),
  dplyr::n_distinct(analysis_sample$person_id[analysis_sample$in_earnings_sample == 1 &
                                                analysis_sample$in_age_30_49 == 1]),
  dplyr::n_distinct(analysis_sample$person_id[!is.na(analysis_sample$has_interruption)])
)
print(sizes)
cat("\nThe gain from the earnings design is the difference between the\n")
cat("earnings sample and the wage sample: those are the person-waves the\n")
cat("wage-based design discards, and they are disproportionately mothers.\n")

cat("\n#####################################################\n")
cat("# v2 VERIFICATION COMPLETE                          #\n")
cat("#####################################################\n")

needed <- c("hours_worked_clean", "hours_worked_main_job", "second_job_hours",
            "years_not_in_lf", "years_since_education", "olf_share",
            "real_weekly_earnings", "real_annual_income", "fy_income_gross",
            "d_olf_years", "olf_flow_valid", "firm_size_regime",
            "study_item_regime", "wealth_module_wave")
missing <- setdiff(needed, names(analysis_sample))
if (length(missing)) stop("Verification block references missing columns: ",
                          paste(missing, collapse = ", "))

# ---- What this build asks of the estimation stage --------------------
# Printed last so it is the final thing in an archived log. None of it is
# enforced here; 00_compat_v21.R enforces it where the specifications
# are written.
cat("\n#####################################################\n")
cat("# ", BUILD_TAG, " RESTRICTIONS FOR THE ESTIMATION STAGE",
    strrep(" ", max(0, 14 - nchar(BUILD_TAG))), "#\n", sep = "")
cat("#####################################################\n")
print(RESTRICTIONS, row.names = FALSE, right = FALSE)
cat("\nSource 00_compat_v21.R at the top of every downstream script.\n")

# =====================================================================
# INVALIDATE THE HANDOFF REGISTRY  (v21.41)
# =====================================================================
# This run just replaced the panel, so handoff/frame.rds holds a frame built
# from data that no longer exists. 00_contract.R checks the registry BEFORE
# any file, so every downstream script would keep resolving the old frame -
# silently, and reporting agreement, because they would all agree with each
# other on the stale one.
#
# frame.rds only. ev.rds and ledger.rds belong to the 03 design stage.
.ld_handoff <- file.path("handoff", "frame.rds")
if (file.exists(.ld_handoff)) {
  if (isTRUE(file.remove(.ld_handoff)))
    cat("\nhandoff/frame.rds deleted: the panel was rebuilt, so the cached\n",
        "frame is stale. The next script resolves fresh.\n", sep = "")
  else
    cat("\n*** COULD NOT DELETE handoff/frame.rds. Delete it by hand before\n",
        "    running anything downstream.\n", sep = "")
} else {
  cat("\nhandoff/frame.rds absent; nothing to invalidate.\n")
}
rm(.ld_handoff)

# The re-entrancy flag is cleared here and not in .ld_close(), because a
# run that stops early should keep the flag set only until the session
# unwinds, and .ld_close() is also reached by the finalizer.
assign(".LD_RUNNING", FALSE, envir = globalenv())
.ld_close("completed")
