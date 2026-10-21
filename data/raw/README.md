# data/raw

This folder is empty in the repository. It is where real mode looks for the
restricted HILDA release.

To reproduce the published results, place the HILDA General Release 24 Stata
files (`Combined_l240c.dta` to `Combined_x240c.dta`, waves 12 to 24) here, or
set the `HILDA_DATA_DIR` environment variable to the folder that holds them.
Then run `DATA_MODE=real Rscript run_all.R` from the repository root.

The files are confidential. `.gitignore` keeps everything in this folder
except this README out of version control. Never commit them.

Synthetic mode, the default, does not use this folder.
