## Fit linear regression models on all contiguous follicle size ranges.
## Y: total number of eggs collected or total number of mature eggs,
## X: number of total follicles in the range.
## Log both Y and X.

### Header ---------------------------------------------------------------------

rm(list = ls())

library(data.table)
library(ggplot2)

### Data -----------------------------------------------------------------------

dt <- readRDS("../data/working/dummy-working.rds")
dim(dt)

uniqueN(dt$PatientIdentifier)

d <- dt[, c("colleggs", "mateggs", paste0("fs", 6:26)), with = FALSE]

anyNA(d)

summary(d)

### Contiguous Ranges ----------------------------------------------------------

cranges <- NULL
for (i in 6:26) {
  for (j in i:26) {
    cranges[[paste0(i, "--", j)]] <- paste0("fs", i:j)
  }
}

length(cranges)

### Total Follicle Counts in Range ---------------------------------------------

dtfc <- lapply(cranges, function(r) {
  data.table(colleggs = d$colleggs,
             mateggs = d$mateggs,
             TFC = rowSums(d[, r, with = FALSE]))
})

### R²  ------------------------------------------------------------------------

res <- NULL
for (i in seq_along(dtfc)) {
  mod.colleggs <- lm(log1p(colleggs) ~ log1p(TFC), dtfc[[i]])
  mod.mateggs <- lm(log1p(mateggs) ~ log1p(TFC), dtfc[[i]])
  res[[i]] <- rbind(
    data.table(y = "colleggs",
               range = names(dtfc)[i],
               R2 = summary(mod.colleggs)$r.squared,
               SpearmanR = cor(dtfc[[i]]$colleggs, dtfc[[i]]$TFC, method = "spearman")),
    data.table(y = "mateggs",
               range = names(dtfc)[i],
               R2 = summary(mod.mateggs)$r.squared,
               SpearmanR = cor(dtfc[[i]]$mateggs, dtfc[[i]]$TFC, method = "spearman", use = "pair"))
  )
}

res <- rbindlist(res)

### Output ---------------------------------------------------------------------

Plot <- function(d, ytype, rtype, top = 80) {
  d <- d[y == ytype]
  d <- d[order(-get(rtype))][1:top]
  d[, range := factor(range, levels = rev(range))]
  p <- ggplot(d, aes_string(x = rtype, y = "range")) +
    geom_point() +
    xlab(rtype) +
    ggtitle(paste0(ytype, ": top 80 ranges")) +
    theme(axis.text.x = element_text(size = 6))
  ggsave(sprintf("../output/07-total-follicles-in-range/plot-tfc-in-range-%s-%s.png",
                 ytype, rtype),
         p, width = 20, height = 30, units = "cm")
  p
}

p.colleggs.r2 <- Plot(res, "colleggs", "R2")
p.colleggs.r2

p.colleggs.spearman <- Plot(res, "colleggs", "SpearmanR")
p.colleggs.spearman

p.mateggs.r2 <- Plot(res, "mateggs", "R2")
p.mateggs.r2

p.mateggs.spearman <- Plot(res, "mateggs", "SpearmanR")
p.mateggs.spearman

fwrite(res, "../output/07-total-follicles-in-range/table-tfc-in-range.csv")

### Print Session Information  -------------------------------------------------

sink("../logs/session-info/07-total-follicles-in-range.Rout")
sessionInfo()
sink()
