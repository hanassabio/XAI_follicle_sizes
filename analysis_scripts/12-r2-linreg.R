## Calcualte multiple R² by regressing mature oocyte counts against individual
## follicle size counts.

### Header ---------------------------------------------------------------------

rm(list = ls())

### Requirements ---------------------------------------------------------------

library(data.table)

### Data -----------------------------------------------------------------------

dt <- readRDS("../data/working/dummy-working.rds")
dim(dt)

uniqueN(dt$PatientIdentifier)

(clinics.map <- fread("../input/clinics-map.csv"))

stopifnot(identical(sort(clinics.map$clinic), sort(unique(dt$clinic))))

dt <- dt[clinics.map, on = "clinic"]
dim(dt)

dt[, .N, .(clinic, clinic_id)]

(fs.cols <- paste0("fs", 6:26))

stopifnot(all(fs.cols %in% colnames(dt)))

### Summary by Clinic and Train/Test Split -------------------------------------

res <- list()
for (clin in unique(dt$clinic_id)) {
  test <- dt[clinic_id == clin, c("mateggs", fs.cols), with = FALSE]
  test <- test[!is.na(mateggs)]
  stopifnot(!anyNA(test))
  r2 <- summary(lm(mateggs ~ ., test))$r.squared
  r2.log1p <- summary(lm(log1p(mateggs) ~ ., test))$r.squared
  p.cor <- cor(test$mateggs,
               apply(test[, fs.cols, with = FALSE], 1, sum),
               method = "spearman")
  res[[clin]] <- data.table(Clinic = clin,
                            `R²` = round(r2, 2),
                            `R² [log(Y + 1)]` = round(r2.log1p, 2),
                            `Spearman Cor.` = round(p.cor, 2))
}

(res <- rbindlist(res))

fwrite(res, "../output/12-r2-linreg/r-squared-y-log1p.csv")

### Print Session Information  -------------------------------------------------

sink("../logs/session-info/12-r2-linreg.Rout")
sessionInfo()
sink()
