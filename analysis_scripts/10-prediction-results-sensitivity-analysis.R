## Examine predictions from the models with curated data for MII oocytes.

### Header ---------------------------------------------------------------------

rm(list = ls())

### Requirements ---------------------------------------------------------------

library(data.table)
library(ggplot2)

### Data -----------------------------------------------------------------------

dt <- readRDS("../data/working/dummy-working.rds")
dim(dt)

stopifnot(uniqueN(dt$PatientIdentifier) == nrow(dt))

dt <- dt[!is.na(mateggs)]

## Apply the data curation rule 1.
dt <- dt[mateggs >= 1 & mateggs <= 30]
dim(dt)

## Apply the data curation rule 2.
dt <- dt[mateggs <= TFC_6_26mm]
dim(dt)

summary(dt$mateggs)

(clinics.map <- fread("../input/clinics-map.csv"))

stopifnot(identical(sort(clinics.map$clinic), sort(unique(dt$clinic))))

dt <- dt[clinics.map, on = "clinic"]
dim(dt)

dt[, .N, .(clinic, clinic_id)]

## Predictions for MII oocytes on the LOCO-CV test sets.
pred.mateggs <- fread("../input/predictions-dummy-mateggs-curated.csv")
dim(pred.mateggs)

head(pred.mateggs, 5)

## Rename clinic names.
setnames(pred.mateggs, "Clinic", "clinic")

stopifnot(identical(sort(clinics.map$clinic), sort(unique(pred.mateggs$clinic))))

pred.mateggs <- pred.mateggs[clinics.map, on = "clinic"]
dim(pred.mateggs)

### Prepare Predictions Data ---------------------------------------------------

ProcessPredictions <- function(pred_, d, y.var) {
  stopifnot(all(c("actual", "pred", "clinic_id") %in% colnames(pred_)))
  stopifnot("clinic_id" %in% colnames(d))
  stopifnot(y.var %in% colnames(d))
  d <- d[!is.na(d[[y.var]])]
  pred <- copy(pred_)
  ## Cast actual counts to integer.
  pred[, actual := as.integer(round(actual, 0))]
  ## prediction errors
  pred[, error := actual - pred]
  pred[, abs_error := abs(error)]
  pred[, clinic_id := factor(clinic_id, levels = clinics.map$clinic_id)]
  stopifnot(!anyNA(pred))
  pred
}

pred.mateggs <- ProcessPredictions(pred.mateggs, dt, "mateggs")

head(pred.mateggs, 2)

### Predictions Plots ----------------------------------------------------------

clinic.colours <- clinics.map$clinic_colour
names(clinic.colours) <- clinics.map$clinic_id
clinic.colours

PlotPredictions <- function(pred, y.name) {
  stopifnot(all(c("pred", "actual", "clinic_id") %in% colnames(pred)))
  stopifnot(floor(min(pred$pred)) >= 1)
  max.actual <- max(pred$actual)
  max.pred <- ceiling(max(pred$pred))
  plt <- ggplot(pred, aes(x = pred, y = actual, col = clinic_id)) +
    geom_point(size = 2) +
    geom_abline(intercept = 0, slope = 1, colour = "blue", linetype = 2) +
    xlab(sprintf("Predicted %s", y.name)) +
    ylab(sprintf("Actual %s", y.name)) +
    scale_x_continuous(breaks = 1:max.pred,
                       limits = c(1, max.pred),
                       expand = expansion(add = 1)) +
    scale_y_continuous(breaks = 1:max.actual,
                       limits = c(1, max.actual),
                       expand = expansion(add = 1)) +
    scale_color_manual(guide = guide_legend(title = "Clinic:", nrow = 1),
                       values = clinic.colours[levels(pred$clinic_id)]) +
    theme_bw() +
    theme(
      legend.position = "top",
      ## axis.text = element_text(size = 8),
      panel.grid.minor = element_blank()
    )
  ggsave(sprintf("../output/10-prediction-results-sensitivity-analysis/plot-%s-pred-vs-actual.png",
                 tolower(gsub(" +", "-", y.name))),
         plt, width = 20, height = 20, units = "cm", dpi = 700)
  plt
}

PlotPredictions(pred.mateggs, "MII Oocytes")

PlotPredictionsByClinic <- function(pred, y.name) {
  stopifnot(all(c("pred", "actual", "clinic_id") %in% colnames(pred)))
  plt <- ggplot(pred, aes(x = pred, y = actual)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, colour = "blue", linetype = 2) +
    facet_wrap(clinic_id ~ .) +
    xlab(sprintf("Predicted %s", y.name)) +
    ylab(sprintf("Actual %s", y.name)) +
    theme_bw()
  ggsave(sprintf("../output/10-prediction-results-sensitivity-analysis/plot-%s-pred-vs-actual-by-clinic.png",
                 tolower(gsub(" +", "-", y.name))),
         plt, width = 20, height = 20, units = "cm", dpi = 700)
  plt
}

PlotPredictionsByClinic(pred.mateggs, "MII Oocytes")

### Prediction Performance Metrics ---------------------------------------------

R2 <- function(y, yhat) {
  num <- sum((y - yhat)^2)
  denom <- sum((y - mean(y))^2)
  1 - num / denom
}

GetMetrics <- function(pred) {
  metrics <- pred[, .(MAE = mean(abs(actual - pred)),
                      MedAE = median(abs(actual - pred)),
                      R2 = R2(actual, pred),
                      RMSE = sqrt(mean((actual - pred)^2)),
                      `Max Error` = max(abs(actual - pred))),
                  clinic_id]
  mm <- melt(metrics, id.vars = "clinic_id")
  mm[, variable := gsub("R2", "R²", variable)]
  mm[, variable := factor(variable, levels = c("MAE", "MedAE", "R²", "RMSE", "Max Error"))]
  mm
}

metrics.mateggs <- GetMetrics(pred.mateggs)

head(metrics.mateggs, 2)

PlotMetrics <- function(metrics, y.name) {
  stopifnot(all(c("variable", "value", "clinic_id") %in% colnames(metrics)))
  plt <- ggplot(metrics, aes(x = variable, y = value, col = clinic_id)) +
    geom_jitter(size = 3, height = 0) +
    facet_wrap(~ variable, scales = "free", nrow = 1) +
    xlab(sprintf("Prediction Performance Metric on %s", y.name)) +
    ylab("Value") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_color_manual(guide = guide_legend(title = "Clinic", nrow = 1),
                       values = clinic.colours[levels(metrics$clinic_id)]) +
    theme_bw() +
    theme(
      legend.position = "top"
    )
  ggsave(sprintf("../output/10-prediction-results-sensitivity-analysis/plot-perfromance-metrics-%s.png",
                 tolower(gsub(" +", "-", y.name))),
         plt, width = 20, height = 20, units = "cm", dpi = 700)
  plt
}

PlotMetrics(metrics.mateggs, "MII Oocytes")

### Plot Absolute Errors -------------------------------------------------------

PlotAbsoluteErrors <- function(pred, y.name) {
  stopifnot(all(c("abs_error", "clinic_id") %in% colnames(pred)))
  mae <- pred[, .(MAE = mean(abs_error)), clinic_id]
  max.abs.err <- ceiling(max(pred$abs_error))
  plt <- ggplot(pred, aes(x = clinic_id, y = abs_error)) +
    geom_boxplot() +
    geom_point(data = mae, aes(x = clinic_id, y = MAE, colour = "Mean")) +
    geom_point(aes(colour = "Outlier"), alpha = 0) +
    geom_line(aes(colour = "Median"), alpha = 0) +
    xlab("Clinic") +
    ylab(sprintf("Absolute Prediction Error on %s", y.name)) +
    scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5, seq(5, max.abs.err, 5)),
                       limits = c(0, max.abs.err),
                       expand = expansion(add = 1)) +
    scale_color_manual(name = NULL,
                       values = c("Mean" = "red", "Median" = "black", "Outlier" = "black"),
                       labels = c("Mean", "Median", "Outlier")) +
    guides(color = guide_legend(
             override.aes = list(size = c(2, 1, 2),
                                 shape = c(16, NA, 16),
                                 linetype = c("blank", "solid", "blank"),
                                 linewidth = c(NA, 1, NA),
                                 alpha = c(NA, 1, NA))
           )) +
    theme_bw() +
    theme(
      ## axis.text = element_text(size = 8),
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
  ggsave(sprintf("../output/10-prediction-results-sensitivity-analysis/plot-absolute-error-%s.png",
                 tolower(gsub(" +", "-", y.name))),
         plt, width = 20, height = 25, units = "cm", dpi = 700)
  plt
}

PlotAbsoluteErrors(pred.mateggs, "MII Oocytes")

### Plot Actual Errors ---------------------------------------------------------

PlotErrors <- function(pred, y.name) {
  stopifnot(all(c("error", "clinic_id") %in% colnames(pred)))
  plt <- ggplot(pred, aes(x = clinic_id, y = error)) +
    geom_boxplot() +
    geom_point(aes(colour = "Outlier"), alpha = 0) +
    geom_line(aes(colour = "Median"), alpha = 0) +
    xlab("Clinic") +
    ylab(sprintf("Prediction Error on %s (Actual - Predicted)", y.name)) +
    scale_y_continuous(breaks = c(-rev(seq(5, ceiling(abs(min(pred$error))), 5)),
                                  -4:4,
                                  seq(5, ceiling(max(pred$error)), 5)),
                       expand = expansion(add = 1)) +
    scale_color_manual(name = NULL,
                       values = c("Median" = "black", "Outlier" = "black"),
                       labels = c("Median", "Outlier")) +
    guides(color = guide_legend(
             override.aes = list(size = c(1, 2),
                                 shape = c(NA, 16),
                                 linetype = c("solid", "blank"),
                                 linewidth = c(1, NA),
                                 alpha = c(1, NA))
           )) +
    theme_bw() +
    theme(
      ## axis.text = element_text(size = 8),
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
  ggsave(sprintf("../output/10-prediction-results-sensitivity-analysis/plot-error-%s.png",
                 tolower(gsub(" +", "-", y.name))),
         plt, width = 20, height = 25, units = "cm", dpi = 700)
  plt
}

PlotErrors(pred.mateggs, "MII Oocytes")

### Print Session Information  -------------------------------------------------

sink("../logs/session-info/10-prediction-results-sensitivity-analysis.Rout")
sessionInfo()
sink()
