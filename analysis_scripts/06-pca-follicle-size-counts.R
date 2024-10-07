## Perform PCA analysis on the logged counts of follicle sizes.

### Header ---------------------------------------------------------------------

rm(list = ls())

### Preamble -------------------------------------------------------------------

library(data.table)
library(ggplot2)

### Data -----------------------------------------------------------------------

dt <- readRDS("../data/working/dummy-working.rds")
dim(dt)

dt[, .N, country]

### PCA ------------------------------------------------------------------------

(fs.cols <- paste0('fs', 6:26))

d <- dt[, fs.cols, with = FALSE]
d <- log1p(d)

d <- scale(d, center = TRUE, scale = TRUE)
## for (j in colnames(d)) set(d, j = j, value = d[[j]] - median(d[[j]]))

pca <- princomp(d)
pcs <- as.data.table(pca$scores[, 1:2])
setnames(pcs, c("PC1", "PC2"))
pcs[, country := dt$country]
pcs[, clinic := dt$clinic]

varp <- pca$sdev^2
(varp <- varp / sum(varp))

p <- ggplot(pcs, aes(x = PC1, y = PC2, col = country)) +
  geom_point(size = 3, alpha = 0.3) +
  facet_grid(country ~ .) +
  xlab(sprintf("PC1 (%.0f%s)", varp[1] * 100, "%")) +
  ylab(sprintf("PC2 (%.0f%s)", varp[2] * 100, "%")) +
  theme_bw() +
  theme(legend.position = "none")
p

ggsave("../output/06-pca-follicle-size-counts/pca-follicle-size-counts.png", p,
       width = 8)

pca$loadings

biplot(pca)

### Print Session Information  -------------------------------------------------

sink("../logs/session-info/06-pca-follicle-size-counts.Rout")
sessionInfo()
sink()
