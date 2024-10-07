## This script applies statistical tests for comparing groups that satisfy
## specific rules for triggering oocyte maturation. Various parameters for
## comparisons are specified by the ./02-rule-analysis-jobs.R script.
##
## Note the proportions test is performed only for one variable: "live_birth".

### Header ---------------------------------------------------------------------

rm(list = ls())

if (!interactive()) options(width = 190, length = 99999)

### Requirements ---------------------------------------------------------------

library(data.table)
library(ggplot2)

### Command Line Arguments -----------------------------------------------------

commandArgs()

job.id <- commandArgs(trailingOnly = TRUE)[1]
job.id <- as.numeric(job.id)

## For interactive use only.
## Set job ID manually. See the associated jobs file.
if (is.na(job.id)) job.id <- 1

job.id

### Job Parameters -------------------------------------------------------------

verbose <- TRUE

job.grid <- fread("./02-rule-analysis-jobs.csv")
job.grid[job.id]

output.suffix <- NULL
for (v in names(job.grid)) {
  assign(v, job.grid[job.id][[v]])
  job.pars <- sprintf("%s=%s", v, get(v))
  cat(job.pars, "\n")
  output.suffix <- c(output.suffix, job.pars)
}

output.suffix <- paste0(output.suffix, collapse = "-")
(output.suffix <- tolower(output.suffix))

### Data -----------------------------------------------------------------------

d <- readRDS("../data/working/dummy-working.rds")
dim(d)

uniqueN(d$PatientIdentifier)

### Data Subsets ---------------------------------------------------------------

if (tot_fol_group == "<10") {
  d <- d[TFC_6_26mm < 10]
} else if (tot_fol_group == "10-19") {
  d <- d[TFC_6_26mm >= 10 & TFC_6_26mm <= 19]
} else if (tot_fol_group == "20+") {
  d <- d[TFC_6_26mm >= 20]
} else if (tot_fol_group == "all") {
  ## Use all of the data.
} else {
  stop("tot_fol_group not recognised.")
}

dim(d)

if (job.id == 1) {
  grpn <- cut(d$TFC_6_26mm, breaks = c(0, 9, 19, 29, Inf), include.lowest = TRUE)
  fwrite(as.data.table(table(grpn)), "../output/03-rule-analysis/TFC-group-N.csv")
}

### Old Rules ------------------------------------------------------------------

old.rules <- c("criterion17_2met_DoT",
               "criterion18_2met_DoT",
               "criterion17_3met_DoT",
               "criterion18_3met_DoT")

for (rule in old.rules) {
  d[, (rule) := as.factor(get(rule))]
}

summary(d[, old.rules, with = FALSE])

### Optimal Range --------------------------------------------------------------

## follicle size columns
(fs.cols <- colnames(d)[grep("^fs\\d\\d?$", colnames(d))])

## total follicles in the optimal range
(range.limits <- as.integer(strsplit(opt_range, '-')[[1]]))
(fs.opt.range <- paste0('fs', range.limits[1]:range.limits[2]))

fs.opt.range.sum <- apply(d[, fs.opt.range, with = FALSE], 1, sum)
summary(fs.opt.range.sum)

fs.tot.sum <- apply(d[, fs.cols, with = FALSE], 1, sum)
summary(fs.tot.sum)

## total for 11+
fs11.tot.sum <- apply(d[, paste0('fs', 11:26), with = FALSE], 1, sum)
summary(fs11.tot.sum)

d[, fs_opt_range_sum := fs.opt.range.sum]
d[, fs_tot_sum := fs.tot.sum]

d[, fs_opt_range_prop := fs_opt_range_sum / fs_tot_sum]

d[, fs_tot_sum_11 := fs11.tot.sum]

d[, mateggs_prop_tot := mateggs / fs_tot_sum]
summary(d$mateggs_prop_tot)

d[, mateggs_prop_tot11 := mateggs / fs_tot_sum_11]
summary(d$mateggs_prop_tot11)

### Rule Equal to Categorised Proportion Rule ----------------------------------

## Categorise optimal range proportions.
(prop.breaks <- c(seq(0, 0.7, prop_step), 1))

d[, fs_opt_range_prop_cat :=
      cut(fs_opt_range_prop, breaks = prop.breaks, include.lowest = TRUE)]

levels(d$fs_opt_range_prop_cat)

ModifyCatLables <- Vectorize(function(x) {
  ## Tidy up category labels for printing.
  p <- gsub('([][()])([0-9.]+),([0-9.]+)([][()])', '\\1_\\2_\\3_\\4', x)
  s <- strsplit(p, '_')[[1]]
  l <- sprintf('%.2f', as.numeric(s[2]))
  u <- sprintf('%.2f', as.numeric(s[3]))
  sprintf('%s%s, %s%s', s[1], l, u, s[4])
})

d[, fs_opt_range_prop_cat_orig := fs_opt_range_prop_cat]
d[, fs_opt_range_prop_cat := ModifyCatLables(fs_opt_range_prop_cat)]

d[, .(fs_opt_range_prop, fs_opt_range_prop_cat_orig, fs_opt_range_prop_cat)]

prop.breaks.levels <- data.table(old = levels(d$fs_opt_range_prop_cat_orig))
prop.breaks.levels[, new := ModifyCatLables(old)]
prop.breaks.levels

d[, fs_opt_range_prop_cat :=
      factor(fs_opt_range_prop_cat, levels = prop.breaks.levels$new)]

## Check re-labelling levels.
d[, .N, .(fs_opt_range_prop_cat_orig,
          fs_opt_range_prop_cat)][order(fs_opt_range_prop_cat)]

new.rule.cols <- NULL
for (cat in levels(d$fs_opt_range_prop_cat)) {
  col.name <- cat
  d[, (col.name) := ifelse(fs_opt_range_prop_cat == cat, 'T', 'F')]
  d[, (col.name) := as.factor(get(col.name))]
  new.rule.cols <- c(new.rule.cols, col.name)
  if (verbose) {
    tmp <- d[, .N, by = .(fs_opt_range_prop_cat, `T/F` = get(col.name))]
    setnames(tmp, "T/F", col.name)
    tmp <- tmp[order(fs_opt_range_prop_cat)]
    print(tmp)
    rm(tmp)
  }
}
rm(cat, col.name)

summary(d[, new.rule.cols, with = FALSE])

### Rule Greater or Equal to Proportion Rule -----------------------------------

new.rule.geq.cols <- NULL
for (brk in prop.breaks[-c(1, length(prop.breaks))]) {
  brk.label <- sprintf('≥%.2f', brk)
  new.rule.geq.cols <- c(new.rule.geq.cols, brk.label)
  d[, (brk.label) := ifelse(fs_opt_range_prop >= brk, 'T', 'F')]
  d[, (brk.label) := as.factor(get(brk.label))]
  if (verbose) {
    cat('\n\n## Variable:', brk.label, '\n\n')
    print(by(d$fs_opt_range_prop, d[[brk.label]], summary))
  }
}
rm(brk, brk.label)

summary(d[, new.rule.geq.cols, with = FALSE])

### Utility Functions ----------------------------------------------------------

PropTest <- function(nums, denoms) {
  ## First element in each should correspond to trues.
  ## Second element in each should correspond to falses.
  ##
  ## Check arguments.
  stopifnot(length(nums) == 2)
  stopifnot(length(denoms) == 2)
  stopifnot(all(nums <= denoms))
  ## Proportions
  prop.trues = nums[1] / denoms[1]
  prop.falses = nums[2] / denoms[2]
  ## No test for small numerators.
  if (nums[1] < 10 | nums[2] < 10) {
    res <- data.table(
      test_type = 'prop-test',
      prop_trues = prop.trues,
      prop_falses = prop.falses,
      prop_trues_ci_l = "NP",
      prop_trues_ci_u = "NP",
      prop_trues_ci_str = "NP",
      prop_falses_ci_l = "NP",
      prop_falses_ci_u = "NP",
      prop_falses_ci_str = "NP",
      prop_diff = prop.trues - prop.falses,
      pvalue = "NP"
    )
    return(list(test = "NP", results = res))
  }
  ## Perform proportions tests.
  pt <- prop.test(nums, denoms)
  prop.trues.ci <- binom.test(nums[1], denoms[1])$conf.int
  ptcil <- prop.trues.ci[1]
  ptciu <- prop.trues.ci[2]
  ptcistr <- sprintf("%.2f—%.2f", ptcil, ptciu)
  prop.falses.ci <- binom.test(nums[2], denoms[2])$conf.int
  pfcil <- prop.falses.ci[1]
  pfciu <- prop.falses.ci[2]
  pfcistr <- sprintf("%.2f—%.2f", pfcil, pfciu)
  ## Results
  res <- data.table(
    test_type = 'prop-test',
    prop_trues = prop.trues,
    prop_falses = prop.falses,
    prop_trues_ci_l = ptcil,
    prop_trues_ci_u = ptciu,
    prop_trues_ci_str = ptcistr,
    prop_falses_ci_l = pfcil,
    prop_falses_ci_u = pfciu,
    prop_falses_ci_str = pfcistr,
    prop_diff = prop.trues - prop.falses,
    pvalue = pt$p.value
  )
  list(test = pt, results = res)
}

PropTestBinaryY <- function(dt, y.name, rule) {
  ## Check arguments.
  stopifnot(is.data.table(dt))
  stopifnot(y.name %in% colnames(dt))
  stopifnot(!any(is.infinite(dt[[y.name]])))
  stopifnot(rule %in% colnames(dt))
  stopifnot(all(dt[[rule]] %in% c("T", "F")))
  ## Remove NAs
  dt <- dt[!is.na(dt[[y.name]])]
  stopifnot(all(dt[[y.name]] %in% c(0L, 1L)))
  ## Split patients by rule realisation.
  trues <- dt[[rule]] == "T"
  falses <- dt[[rule]] == "F"
  ## Split Y.
  y.trues <- dt[trues][[y.name]]
  y.falses <- dt[falses][[y.name]]
  ## Number of successes
  s <- c(sum(y.trues == 1L), sum(y.falses == 1L))
  ## Number of trials
  n <- c(length(y.trues), length(y.falses))
  res <- PropTest(s, n)
  res$results <- cbind(data.table(rule = rule, y_name = y.name), res$results)
  res$results <- cbind(res$results, CalcSummaries(y.trues, y.falses))
  stopifnot(nrow(res$results) == 1)
  res
}

MannWhitneyTest <- function(dt, y.name, rule) {
  ## Check arguments.
  stopifnot(is.data.table(dt))
  stopifnot(y.name %in% colnames(dt))
  stopifnot(!any(is.infinite(dt[[y.name]])))
  stopifnot(rule %in% colnames(dt))
  stopifnot(all(dt[[rule]] %in% c("T", "F")))
  ## Remove NAs
  dt <- dt[!is.na(dt[[y.name]])]
  ## Split patients by rule realisation.
  trues <- dt[[rule]] == "T"
  falses <- dt[[rule]] == "F"
  ## Split Y.
  y.trues <- dt[trues][[y.name]]
  y.falses <- dt[falses][[y.name]]
  ## No test for small samples.
  if (length(y.trues) < 10 | length(y.falses) < 10) {
    res <- data.table(
      rule = rule,
      y_name = y.name,
      test_type = "mann-whitney-test",
      mw_estimate = "NP",
      mw_ci_l = "NP",
      mw_ci_u = "NP",
      pvalue = "NP"
    )
    res <- cbind(res, CalcSummaries(y.trues, y.falses))
    stopifnot(nrow(res) == 1)
    return(list(test = "NP", results = res))
  }
  ## Perform Mann-Whitney test.
  mw <- wilcox.test(y.trues, y.falses, exact = TRUE, conf.int = TRUE)
  ## results
  res <- data.table(
    rule = rule,
    y_name = y.name,
    test_type = "mann-whitney-test",
    mw_estimate = mw$estimate,
    mw_ci_l = mw$conf.int[1],
    mw_ci_u = mw$conf.int[2],
    pvalue = mw$p.value
  )
  res <- cbind(res, CalcSummaries(y.trues, y.falses))
  stopifnot(nrow(res) == 1)
  list(test = mw, results = res)
}

CalcSummaries <- function(y.trues, y.falses) {
  ## Check arguments.
  stopifnot(!any(is.infinite(y.trues)))
  stopifnot(!any(is.infinite(y.falses)))
  stopifnot(!any(is.na(y.trues)))
  stopifnot(!any(is.na(y.falses)))
  ## Quartiles
  lq.trues = quantile(y.trues, probs = 0.25)
  uq.trues = quantile(y.trues, probs = 0.75)
  lq.falses = quantile(y.falses, probs = 0.25)
  uq.falses = quantile(y.falses, probs = 0.75)
  ## Summaries
  res <- data.table(
    n = length(y.trues) + length(y.falses),
    n_trues = length(y.trues),
    n_falses = length(y.falses),
    mean_trues = mean(y.trues),
    sd_trues = sd(y.trues),
    median_trues = median(y.trues),
    iqr_trues = IQR(y.trues),
    lq_trues = lq.trues,
    uq_trues = uq.trues,
    iqr_trues_str = sprintf("%.2f--%.2f", lq.trues, uq.trues),
    mean_falses = mean(y.falses),
    sd_falses = sd(y.falses),
    median_falses = median(y.falses),
    iqr_falses = IQR(y.falses),
    lq_falses = lq.falses,
    uq_falses = uq.falses,
    iqr_falses_str = sprintf("%.2f--%.2f", lq.falses, uq.falses),
    median_diff = median(y.trues) - median(y.falses),
    mean_diff = mean(y.trues) - mean(y.falses)
  )
}

## (rules <- c(old.rules, new.rule.cols, new.rule.geq.cols))
(rules <- c(old.rules, new.rule.geq.cols))

### Statistical Tests ----------------------------------------------------------

if (test == "prop_test") {
  TestFun <- PropTestBinaryY
} else if (test == "mann_whitney") {
  TestFun <- MannWhitneyTest
}

results <- list()
for (rule in rules) {
  cat("## Processing rule: ", rule, "\n\n", sep = "")
  results[[rule]] <- TestFun(d, y_name, rule)
  if (verbose) print(results[[rule]])
}

warnings()

results.dt <- rbindlist(lapply(results, function(x) x$results))
results.dt[, rule := factor(rule, levels = rules)]

fwrite(results.dt,
       sprintf('../output/03-rule-analysis/tests/%s.csv', output.suffix),
       na = "NA")

### Plots ----------------------------------------------------------------------

if (y_name == "live_birth") {
  p1 <- ggplot(results.dt, aes(x = rule, y = prop_diff)) +
    geom_point(size = 3) +
    xlab(paste0("step=", prop_step, "\nN=", nrow(d))) +
    ylab(paste0(y_name, "\nmedian=black"))
} else {
  p1 <- ggplot(results.dt, aes(x = rule, y = median_diff)) +
    geom_point(size = 3) +
    geom_point(aes(x = rule, y = mean_diff), col = "red", size = 3) +
    xlab(paste0("step=", prop_step, "\nN=", nrow(d))) +
    ylab(paste0(y_name, "\nmedian=black, mean=red"))
}

p1 <- p1 + ggtitle(output.suffix) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

if (interactive()) p1

ggsave(sprintf("../output/03-rule-analysis/plots/%s.png", output.suffix),
       plot = p1, dpi = 500, width = 30, height = 15, units = "cm")

warnings()

### Print Session Information  -------------------------------------------------

if (job.id == 1) {
  sink("../logs/session-info/03-rule-analysis.Rout")
  print(sessionInfo())
  sink()
}
