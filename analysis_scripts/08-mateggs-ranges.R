## Fit linear regresson models on all contiguous follicle size ranges and
## calucalte model performance on test sets using LOCO-CV.
## Y: total number of mature eggs,
## X: number of total follicles in the range.

### Header ---------------------------------------------------------------------

rm(list = ls())

library(data.table)
library(ggplot2)

### Settings -------------------------------------------------------------------

log.y <- TRUE

### Data -----------------------------------------------------------------------

dt <- readRDS("../data/working/dummy-working.rds")
dim(dt)

uniqueN(dt$PatientIdentifier)

d <- dt[, c("clinic", "mateggs", paste0("fs", 6:26)), with = FALSE]

d <- d[!is.na(mateggs)]

anyNA(d)

summary(d)

dim(d)

### Contiguous Ranges ----------------------------------------------------------

cranges <- NULL
for (i in 6:26) {
  for (j in i:26) {
    cranges[[paste0(i, "--", j)]] <- paste0("fs", i:j)
  }
}

length(cranges)

### LOCO-CV --------------------------------------------------------------------

(clinics <- unique(d$clinic))

res <- NULL
j <- 0
for (i in seq_along(cranges)) {
  r <- cranges[i]
  print(r)
  for (clin in clinics) {
    print(clin)
    train <- d[clinic != clin]
    train[, X := apply(train[, r[[1]], with = FALSE], 1, sum)]
    test <- d[clinic == clin]
    test[, X := apply(test[, r[[1]], with = FALSE], 1, sum)]
    if(log.y) {
      mod <- lm(log1p(mateggs) ~ X, train)
      pred.log <- predict(mod, newdata = test)
      pred <- exp(pred.log) - 1
    } else {
      mod <- lm(mateggs ~ X, train)
      pred <- predict(mod, newdata = test)
    }
    print(summary(mod))
    mae <- sum(abs(test$mateggs - pred)) / nrow(test)
    mse <- sum((test$mateggs - pred) ^ 2) / nrow(test)
    ## R^2 on predicted values.
    num <- sum((test$mateggs - pred)^2)
    denom <- sum((test$mateggs - mean(test$mateggs))^2)
    r2.test <- 1 - num / denom
    j <- j + 1
    res[[j]] <- data.table(
      range = names(cranges)[i],
      test_clinic = clin,
      MAE = mae,
      MSE = mse,
      R2_train = summary(mod)$r.squared,
      R2_test = r2.test)
  }
}

res <- rbindlist(res)

res.by.range <- res[, .(Mean_MAE = mean(MAE), SD_MAE = sd(MAE),
                        Mean_MSE = mean(MSE), SD_MSE = sd(MSE),
                        Mean_R2_train = mean(R2_train), SD_R2_train = sd(R2_train),
                        Mean_R2_test = mean(R2_test), SD_R2_test = sd(R2_test)),
                    .(range)]

### Output ---------------------------------------------------------------------

infix <- ifelse(log.y, "logged-y", "raw-counts")

fwrite(res, sprintf("../output/08-mateggs-ranges/mateggs-ranges-%s.csv", infix))
fwrite(res.by.range, sprintf("../output/08-mateggs-ranges/mateggs-ranges-%s-summarised.csv", infix))

### Print Session Information  -------------------------------------------------

sink("../logs/session-info/08-mateggs-ranges.Rout")
sessionInfo()
sink()
