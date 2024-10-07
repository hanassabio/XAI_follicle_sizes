## Logistic regression models for live birth regressed against %-in-range or mean
## follicle size, plus the adjustment variables.

### Header ---------------------------------------------------------------------

rm(list = ls())

library(data.table)
library(ggplot2)

### Data -----------------------------------------------------------------------

dt <- readRDS("../data/working/dummy-working.rds")
dim(dt)

### Y --------------------------------------------------------------------------

dt[, .(N = .N, prop = .N / nrow(dt)), live_birth][order(live_birth)]

### X --------------------------------------------------------------------------

## Age
summary(dt[, `Age at Egg Collection`])

## Type of trigger administered
dt[, trigger_map := as.factor(trigger_map)]
dt[, .N, trigger_map]

## Total follicle count
summary(dt[, TFC_6_26mm])

## Percentage of follicles between 13-18 mm
summary(dt[, in_range13])

## Mean follicle size
summary(dt$mean_size)

### Model 1 ---------------------------------------------------------------------

d1 <- dt[, .(live_birth,
             Age = `Age at Egg Collection`,
             trigger_map,
             TFC_6_26mm,
             in_range13 = in_range13 * 10)]
dim(d1)

summary(d1)

mod1 <- glm(live_birth ~ ., d1, family = "binomial")
summary(mod1)

dim(model.matrix(mod1))

## OR
or1 <- exp(cbind(OR = coef(mod1), confint(mod1)))
or1 <- data.table(var = rownames(or1), or1)
or1

sink("../output/05-lbr-lr-models/model-inrange.txt")
print("coef for in_range13 is in terms of 10 percentage points change")
summary(mod1)
or1
sink()

fwrite(or1, "../output/05-lbr-lr-models/model-inrange-or.csv")

### Model 1.1 -------------------------------------------------------------------

mod1.1 <- glm(live_birth ~ .  + in_range13 * TFC_6_26mm, d1, family = "binomial")
summary(mod1.1)

dim(model.matrix(mod1.1))

## OR
or1.1 <- exp(cbind(OR = coef(mod1.1), confint(mod1.1)))
or1.1 <- data.table(var = rownames(or1.1), or1.1)
or1.1

sink("../output/05-lbr-lr-models/model-inrange-x-interaction-with-tfc.txt")
print("coef for in_range13 is in terms of 10 percentage points change")
summary(mod1.1)
or1.1
sink()

fwrite(or1.1, "../output/05-lbr-lr-models/model-inrange-x-interaction-with-tfc-or.csv")

### Model 2 ---------------------------------------------------------------------

d2 <- dt[, .(live_birth,
             Age = `Age at Egg Collection`,
             trigger_map,
             TFC_6_26mm,
             mean_size)]
dim(d2)

summary(d2)

mod2 <- glm(live_birth ~ ., d2, family = "binomial")
summary(mod2)

dim(model.matrix(mod2))

## OR
or2 <- exp(cbind(OR = coef(mod2), confint(mod2)))
or2 <- data.table(var = rownames(or2), or2)
or2

sink("../output/05-lbr-lr-models/model-mean-size.txt")
summary(mod2)
or2
sink()

fwrite(or2, "../output/05-lbr-lr-models/model-mean-size-or.csv")

### Correlation ----------------------------------------------------------------

cor.test(dt$live_birth, log(dt$mean_size), use = "pair")

sink("../output/05-lbr-lr-models/lbr-mean-size-corr.txt")
cor.test(dt$live_birth, log(dt$mean_size), use = "pair")
sink()

### PDP ------------------------------------------------------------------------
## library(pdp)

## partial(mod1, pred.var = c("in_range13"), plot = TRUE, prob = TRUE)
## partial(mod2, pred.var = c("mean_size"), plot = TRUE, prob = TRUE)

### Check Mean Size ------------------------------------------------------------

st <- fread("../data/input/dummy-2.csv")
dim(st)

st[, PatientIdentifier := gsub("^ +", "", PatientIdentifier)]

stopifnot(identical(dt$PatientIdentifier, st$PatientIdentifier))

d2.2 <- dt[, .(PatientIdentifier,
               live_birth,
               Age = `Age at Egg Collection`,
               trigger_map,
               TFC_6_26mm)]
dim(d2.2)

d2.2 <- merge(d2.2, st[, .(PatientIdentifier, mean_size)],
              on = "PatientIdentifier", sort = FALSE)

d2.2[, PatientIdentifier := NULL]

summary(d2.2)

mod2.2 <- glm(live_birth ~ ., d2.2, family = "binomial")

summary(mod2)
summary(mod2.2)

sink("../output/05-lbr-lr-models/model-mean-size-check.txt")
print("Mean size for 6-26")
summary(mod2)
print("Mean size for 5-26")
summary(mod2.2)
sink()

### Print Session Information  -------------------------------------------------

sink("../logs/session-info/05-lbr-lr-models.Rout")
sessionInfo()
sink()
