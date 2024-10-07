## Script for building Table 2.

### Header ---------------------------------------------------------------------

rm(list = ls())

### Preamlbe -------------------------------------------------------------------

library(data.table)
library(ggplot2)

### Data -----------------------------------------------------------------------

dt <- readRDS("../data/working/dummy-working.rds")
dim(dt)

uniqueN(dt$PatientIdentifier)

### Settings -------------------------------------------------------------------

groupby <- "country"
## groupby <- "clinic"

if (groupby == "country") {
  dt[, groupby := country]
} else if (groupby == "clinic") {
  dt[, groupby := clinic]
}

### Variables ------------------------------------------------------------------

## Variable labels
varnames <- list(
  Age         = "Age at treatment",
  BMI         = "Body mass index",
  AFC         = "Antral follicle count",
  FC11p       = "Follicles >10mm on DoT",
  TFC         = "Total follicles on DoT (6-26mm)",
  DTT         = "Days to trigger",
  Eggs        = "No. oocytes collected",
  MatEggs     = "No. MII oocytes",
  Zygotes     = "No. 2PN zygotes",
  Blastocysts = "No. HQ blastocysts",
  MEY         = "Mature oocyte yield",
  MR          = "Maturation rate",
  FR          = "Fertilization rate",
  BR          = "Blastulation rate",
  LB          = "Live birth"
)

### Table 2 Variable -----------------------------------------------------------

dt2 <- dt[, .(
  Age         = `Age at Egg Collection`,
  BMI         = BMI,
  AFC         = AFC_result,
  FC11p       = `total_follicles_11+_DoT`,
  TFC         = TFC_6_26mm,
  DTT         = days_to_trigger,
  Eggs        = colleggs,
  MatEggs     = mateggs,
  Zygotes     = `No. 2PNs`,
  Blastocysts = `No. Suitable Blastocysts`,
  MEY         = mateggs / TFC_6_26mm,
  MR          = mat_rate,
  FR          = fert_rate,
  BR          = blast_rate,
  LB          = live_birth,
  groupby
)]

group.n <- dt2[, .N, groupby][order(-N)]
group.n[, group := paste0(groupby, " (N=", prettyNum(N, big.mark = ","), ")")]
group.n[, group := factor(group, levels = group)]
group.n

dt2 <- dt2[group.n, on = "groupby"]

dt2[, groupby := NULL]
dt2[, N := NULL]

dt2[, .N, group]

summary(dt2)

stopifnot(all(!unlist(lapply(dt2, function(x) any(is.infinite(x))))))

### Summaries -------------------------------------------------------------------

MissPerc<- function(x) {
  prop <- sum(is.na(x)) / length(x)
  ifelse(prop > 0 & prop < 0.01, "<1%", sprintf("%.0f%%", 100 * prop))
}

Summary <- function(d) {
  res <- list()
  for (col in colnames(d)) {
    if (col %in% c("clinic", "country", "group")) next
    x <- d[[col]]
    stopifnot(all(!is.infinite(x)))
    if (col %in% c("LB")) {
      res[[col]] <- data.table(
        Variable = varnames[[col]],
        Summary = sprintf("%.0f%% [%s]; N=%s",
                          round(100 * sum(x == 1, na.rm = TRUE) / sum(!is.na(x)), 0),
                          MissPerc(x),
                          prettyNum(sum(!is.na(x)), big.mark = ","))
      )
    } else if (col %in% c("Age")) {
      res[[col]] <- data.table(
        Variable = varnames[[col]],
        Summary = sprintf("%.2f (%.2f) [%.2f, %.2f]; N=%s",
                          mean(x, na.rm = TRUE),
                          sd(x, na.rm = TRUE),
                          min(x, na.rm = TRUE),
                          max(x, na.rm = TRUE),
                          prettyNum(sum(!is.na(x)), big.mark = ","))
      )
    } else {
      q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
      res[[col]] <- data.table(
        Variable = varnames[[col]],
        Summary = sprintf("%.2f (%.2f--%.2f) [%.2f, %.2f]; N=%s",
                          median(x, na.rm = TRUE),
                          q[1], q[2],
                          min(x, na.rm = TRUE),
                          max(x, na.rm = TRUE),
                          prettyNum(sum(!is.na(x)), big.mark = ","))
      )
    }
  }
  rbindlist(res)
}

sm <- Summary(dt2)

sm[, Variable := factor(Variable, unlist(varnames))]
sm

(smc <- dt2[, Summary(.SD), group])

smc <- dcast(smc, Variable~group)
smc[match(varnames, Variable)]

(group.levels <- levels(dt2$group))

### Plots ----------------------------------------------------------------------

Plot <- function(d, v, density = TRUE) {
  stopifnot(v %in% colnames(d))
  stopifnot("group" %in% colnames(d))
  m <- melt(d[, c(v, "group"), with = FALSE], id.vars = "group")
  geom <- ifelse(density, geom_density,
                 function() {geom_histogram(binwidth = 1, aes(fill = group))})
  p <- ggplot(m, aes(x = value, col = group)) +
    geom() +
    facet_grid(group ~ ., scales = "free_y") +
    xlab(varnames[v]) +
    theme_bw() +
    theme(
      text = element_text(size = 12),
      legend.position = "none"
    )
  ggsave(sprintf("../output/04-table-2/plot-by-%s-%s.png", groupby, v), p,
         width = 20, height = 30, units = "cm")
  p
}

Plot(dt2, "Age")
Plot(dt2, "BMI")
Plot(dt2, "AFC", FALSE)
Plot(dt2, "FC11p", FALSE)
Plot(dt2, "DTT")
Plot(dt2, "Eggs", FALSE)
Plot(dt2, "MatEggs", FALSE)
Plot(dt2, "Zygotes", FALSE)
Plot(dt2, "Blastocysts", FALSE)
Plot(dt2, "MEY")
Plot(dt2, "MR")
Plot(dt2, "FR")
Plot(dt2, "BR")

### Tests ----------------------------------------------------------------------

if (groupby == "country") {
  stopifnot(length(group.levels) == 2)
  tests <- list()
  for (col in colnames(dt2)) {
    if (col %in% c("clinic", "country", "group")) next
    x1 <- dt2[group == group.levels[1]][[col]]
    x2 <- dt2[group == group.levels[2]][[col]]
    if (col %in% c("Age")) {
      test <- t.test(x1, x2)
    } else if (col %in% c("LB")) {
      test <- prop.test(
        c(sum(x1 == 1, na.rm = TRUE), sum(x2 == 1, na.rm = TRUE)),
        c(sum(!is.na(x1)), sum(!is.na(x2)))
      )
    } else {
      test <- wilcox.test(x1, x2)
    }
    tests[[col]] <- data.table(
      Variable = varnames[[col]],
      `P-value` = ifelse(test$p.value < 0.001, "<0.001",
                         sprintf("%.3f", test$p.value))
    )
    rm(x1, x2, test)
  }
  tests <- rbindlist(tests)
  print(tests)
  smc <- merge(smc, tests, by = "Variable", all.x = TRUE, sort = FALSE)
}

smc[, Variable := factor(Variable, unlist(varnames))]
smc <- smc[order(Variable)]
smc

### Output ---------------------------------------------------------------------

fwrite(sm, "../output/04-table-2/table-2.csv")
fwrite(smc, sprintf("../output/04-table-2/table-2-by-%s.csv", groupby))

### Alternative Output ---------------------------------------------------------

dt2[, G := 1]

alt.summary <- dt2[, {
  res <- list()
  for (v in names(varnames)) {
    x <- .SD[[v]]
    q <- quantile(x, probs = c(0.025, 0.25, 0.75, 0.975), na.rm = TRUE)
    res[[v]] <- data.table(
      Variable = varnames[[v]],
      Min = min(x, na.rm = TRUE),
      ## P2.5 = q[1],
      Lower_Quartile = q[2],
      Median = median(x, na.rm = TRUE),
      Mean = mean(x, na.rm = TRUE),
      Upper_Quartile = q[3],
      ## P97.5 = q[4],
      Max = max(x, na.rm = TRUE),
      SD = sd(x, na.rm = TRUE),
      IQR = IQR(x, na.rm = TRUE),
      `N` = prettyNum(sum(!is.na(x)), big.mark = ","),
      `Missing %` = MissPerc(x))
  }
  rbindlist(res)
}, G]

alt.summary[, Variable := factor(Variable, unlist(varnames))]
(alt.summary <- alt.summary[order(Variable)])

fwrite(alt.summary, "../output/04-table-2/table-2-alt.csv")

### Print Session Information  -------------------------------------------------

sink("../logs/session-info/04-table-2.Rout")
sessionInfo()
sink()
