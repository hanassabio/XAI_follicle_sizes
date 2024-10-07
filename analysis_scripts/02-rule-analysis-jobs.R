## This script creates a set of job parameters for performing statistical tests
## comparing trigger rules for oocyte maturation.

### Header ---------------------------------------------------------------------

rm(list = ls())

### Requirements ---------------------------------------------------------------

library(data.table)
library(tools)

### Job Grid -------------------------------------------------------------------

job.grid <- expand.grid(
  opt_range = c("13-18", "15-18", "11-26"),
  y_name = c("mateggs_prop_tot", "mateggs_prop_tot11",
             "mat_rate", "fert_rate", "blast_rate", "live_birth"),
  prop_step = c(0.05),
  tot_fol_group = c("all", "<10", "10-19", "20+"),
  stringsAsFactors = FALSE
)
job.grid <- data.table(job.grid)
job.grid <- job.grid[order(opt_range, y_name, prop_step)]
job.grid

job.grid[, test := "mann_whitney"]
job.grid[y_name == "live_birth", test := "prop_test"]

job.grid[, job_id := 1:.N]
setcolorder(job.grid, "job_id")
print(job.grid)

job.grid.file <-  "./02-rule-analysis-jobs.csv"
fwrite(job.grid, job.grid.file)

print(md5sum(job.grid.file))
