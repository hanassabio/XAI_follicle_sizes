## Make a data summary table for each left-out clinic in LOCO-CV by
## train/test split.

### Header ---------------------------------------------------------------------

rm(list = ls())

### Requirements ---------------------------------------------------------------

library(data.table)
library(ggplot2)

### Data -----------------------------------------------------------------------

dt <- readRDS("../data/working/dummy-working.rds")
dim(dt)

uniqueN(dt$PatientIdentifier)

(clinics.map <- fread("../input/clinics-map.csv"))

stopifnot(identical(sort(clinics.map$clinic), sort(unique(dt$clinic))))

dt <- dt[clinics.map, on = "clinic"]
dim(dt)

dt[, .N, .(clinic, clinic_id)]

### Summary by Clinic and Train/Test Split -------------------------------------

Summary <- function(d, v, clinic, type) {
  x <- d[[v]]
  q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  q <- round(q, 2)
  data.table(Variable = v,
             Clinic = clinic,
             DataType = type,
             N = sum(!is.na(x)),
             Min = min(x, na.rm = TRUE),
             LQ = q[1],
             Median = median(x, na.rm = TRUE),
             Mean = round(mean(x, na.rm = TRUE), 2),
             UQ = q[2],
             Max = max(x, na.rm = TRUE),
             IQR = IQR(x, na.rm = TRUE),
             SD = round(sd(x, na.rm = TRUE), 2),
             Var = round(var(x, na.rm = TRUE), 2))
}

res.colleggs <- list()
res.mateggs <- list()
for (clin in unique(dt$clinic_id)) {
  train <- dt[clinic_id != clin]
  test <- dt[clinic_id == clin]
  ## Total oocytes
  res.colleggs[[clin]] <- rbind(Summary(train, "colleggs", clin, "train"),
                                Summary(test, "colleggs", clin, "test"))
  ## MII oocytes
  res.mateggs[[clin]] <- rbind(Summary(train, "mateggs", clin, "train"),
                               Summary(test, "mateggs", clin, "test"))
}

(res.colleggs <- rbindlist(res.colleggs))
(res.mateggs <- rbindlist(res.mateggs))

res <- rbind(res.colleggs, res.mateggs)

setnames(res, "Clinic", "LOCO-CV Test Clinic")
setnames(res, "DataType", "Data Type")

res[Variable == "colleggs", Variable := "Total Oocytes"]
res[Variable == "mateggs", Variable := "MII Oocytes"]

res[, N := prettyNum(N, big.mark = ",")]

res

fwrite(res, "../output/11-train-test-split-summaries/train-test-summary-by-clinic.csv")

### Print Session Information  -------------------------------------------------

sink("../logs/session-info/11-train-test-split-summaries.Rout")
sessionInfo()
sink()
