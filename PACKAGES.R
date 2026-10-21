# =============================================================================
# PACKAGES.R
#
# Installs and version-checks the packages the pipeline uses. run_all.R
# sources it first, so a separate call is not needed for a full run.
#
# The pins record the authors' reference environment (R 4.5, Windows 11,
# fixest 0.14.2). Newer versions usually work; a mismatch prints a note and
# does not stop the run. Set SKIP_PACKAGE_INSTALL=1 to check without
# installing.
# =============================================================================

# Namespace protection (avoid MASS/stats masking dplyr verbs)
# PACKAGES.R attaches nothing, so the aliases are set only once dplyr is
# installed; the block below runs at the foot of this file.

cran_pkgs <- c(
  tidyverse    = "2.0.0",   # dplyr, tidyr, ggplot2, readr, purrr, stringr, ...
  fixest       = "0.14.2",  # fixed effects, sunab() event studies
  lmtest       = "0.9-40",  # coefficient tests
  sandwich     = "3.1-1",   # robust and clustered covariance
  patchwork    = "1.3.2",   # figure composition
  DoubleML     = "1.0.2",   # double machine learning (03c, the master)
  mlr3         = "1.2.0",   # learner backend for DoubleML
  mlr3learners = "0.13.0",  # ranger and glmnet learners for mlr3
  ranger       = "0.17.0",  # random-forest nuisance learner
  glmnet       = "4.1-10",  # lasso nuisance learner (DML fast pass)
  grf          = "2.5.0",   # causal forest (the master)
  lgr          = "0.5.0",   # quietens the mlr3 logger
  haven        = "2.5.5",   # read_dta() for the raw release (loader)
  digest       = "0.6.37"   # panel fingerprints (loader)
)

repos <- getOption("repos")
if (is.null(repos) || identical(unname(repos["CRAN"]), "@CRAN@"))
  repos <- c(CRAN = "https://cloud.r-project.org")

if (!identical(Sys.getenv("SKIP_PACKAGE_INSTALL"), "1")) {
  for (pkg in names(cran_pkgs)) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(sprintf("Installing '%s' ...", pkg))
      tryCatch(utils::install.packages(pkg, repos = repos),
               error = function(e) message("  could not install ", pkg, ": ",
                                           conditionMessage(e)))
    }
  }
}

cat("\nDependency check (reference run: R 4.5):\n")
cat(sprintf("  R: installed %s\n", getRversion()))
.missing_pkgs <- character(0)
for (pkg in names(cran_pkgs)) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("  %-13s MISSING\n", pkg)); .missing_pkgs <- c(.missing_pkgs, pkg); next
  }
  have <- as.character(utils::packageVersion(pkg))
  want <- cran_pkgs[[pkg]]
  cat(sprintf("  %-13s %-10s %s\n", pkg, have, if (identical(have, want)) "ok" else
    sprintf("reference %s", want)))
}
if (length(.missing_pkgs))
  cat("\n  Missing: ", paste(.missing_pkgs, collapse = ", "),
      ". The generator and the checks need only dplyr and fixest;\n",
      "  the stages that use a missing package will stop and name it.\n", sep = "")

if (requireNamespace("dplyr", quietly = TRUE)) {
  # Namespace protection (avoid MASS/stats masking dplyr verbs)
  select    <- dplyr::select;    filter    <- dplyr::filter
  mutate    <- dplyr::mutate;    slice     <- dplyr::slice
  recode    <- dplyr::recode;    rename    <- dplyr::rename
  summarise <- dplyr::summarise; summarize <- dplyr::summarize
  arrange   <- dplyr::arrange;   count     <- dplyr::count
  lag       <- dplyr::lag
}
cat("\nPACKAGES.R complete.\n")
