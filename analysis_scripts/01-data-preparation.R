## This scripts prepares data received from the main author for further
## analyses. It performs a few sanity checks and deals with the coding of
## missing values. It also creates a few additional variables and renames some
## of the variables according to R conventions.

### Header ---------------------------------------------------------------------

rm(list = ls())

### Requirements ---------------------------------------------------------------

library(data.table)
library(tools)
library(ggplot2)

### Received Data --------------------------------------------------------------

data.file.path <- "../data/input/dummy-1.csv"
(data.md5sum <- md5sum(data.file.path))

d0 <- fread(data.file.path)
dim(d0)

stopifnot(uniqueN(d0[, PatientIdentifier]) == nrow(d0))

## print(unlist(lapply(d0, class)))
## summary(data.table(d0, stringsAsFactors = TRUE))

## Latest data with some updates: live_birth and percentage in the optimal
## follicle size range.
data.new.file.path <- "../data/input/dummy-2.csv"
(data.new.md5sum <- md5sum(data.new.file.path))

## stopifnot(data.new.md5sum == "50dc0e982e429773ecfec90a750fbfa4")

dnew <- fread(data.new.file.path)
dim(dnew)

dnew[, PatientIdentifier := gsub("^ +", "", PatientIdentifier)]

stopifnot(identical(d0$PatientIdentifier, dnew$PatientIdentifier))

### Column Names ---------------------------------------------------------------

d1 <- copy(d0)

## Rename some of the column names that start with numbers.
(fs.cols.orig <- paste0(6:26, '_mm'))
(fs.cols <- paste0('fs', fs.cols.orig, sep = ''))
(fs.cols <- gsub('_mm', '', fs.cols))
setnames(d1, fs.cols.orig, fs.cols)
## TFC -- total follicle count
setnames(d1, "6_26mm", "TFC_6_26mm")

## Rename oocyte count columns.
setnames(d1, "No. Eggs Collected", "colleggs")
setnames(d1, "No. Mature Eggs", "mateggs")

## head(d1, 2)

## summary(data.table(d1, stringsAsFactors = TRUE))

### Live birth -----------------------------------------------------------------

## Latest data on live birth.
lbr <- dnew[, .(PatientIdentifier, live_birth)]
dim(lbr)

d1 <- merge(d1, lbr, on = "PatientIdentifier", sort = FALSE)
dim(d1)

d1[, .N, live_birth][order(live_birth)]

### Mature eggs ----------------------------------------------------------------

stopifnot(identical(d1$mateggs, dnew[, `No. Mature Eggs`]))

d1[mateggs == -1, .N]
d1[mateggs == -1, mateggs := NA]
summary(d1$mateggs)

stopifnot(all(d1$mateggs >= 0, na.rm = TRUE))

### Collected Eggs -------------------------------------------------------------

stopifnot(identical(d1$colleggs, dnew[, `No. Eggs Collected`]))

summary(d1$colleggs)

stopifnot(all(d1$colleggs >= 0, na.rm = TRUE))

### Mature rate ----------------------------------------------------------------
## cycles['mat_rate'] = cycles['No. Mature Eggs']/cycles['No. Eggs Collected']

stopifnot(identical(round(d1$mat_rate, 3),
                    round(dnew$mat_rate, 3)))

d1[mat_rate < 0, .N]
d1[mat_rate < 0, mat_rate := NA]

summary(d1$mat_rate)

stopifnot(all(d1$mat_rate >= 0 | d1$mat_rate <= 1, na.rm = TRUE))

### Fertilisation rate ---------------------------------------------------------
## cycles['fert_rate'] = cycles['No. 2PNs']/cycles['No. Mature Eggs']

stopifnot(identical(round(d1$fert_rate, 3),
                    round(dnew$fert_rate, 3)))

d1[is.na(mateggs) | `No. 2PNs` == -1, fert_rate := NA]

d1[is.infinite(fert_rate), .N, .(`No. 2PNs`, mateggs)][order(`No. 2PNs`)]

d1[is.infinite(fert_rate), fert_rate := NA]

d1[fert_rate > 1, .N]
d1[fert_rate > 1, .(`No. 2PNs`, mateggs)]
d1[fert_rate > 1, fert_rate := NA]

summary(d1$fert_rate)

stopifnot(all(d1$fert_rate >= 0 & d1$fert_rate <= 1, na.rm = TRUE))

### Blastocyst rate ------------------------------------------------------------
## cycles['blast_rate'] = cycles['No. Suitable Blastocysts']/cycles['No. 2PNs']

stopifnot(identical(round(d1$blast_rate, 3),
                    round(dnew$blast_rate, 3)))

d1[`No. Suitable Blastocysts` == -1 | `No. 2PNs` == -1, blast_rate := NA]

d1[is.infinite(blast_rate), .N, .(`No. Suitable Blastocysts`, `No. 2PNs`)]
d1[is.infinite(blast_rate), blast_rate := NA]

d1[blast_rate > 1, .N]
d1[blast_rate > 1, .(`No. Suitable Blastocysts`, `No. 2PNs`)][
  order(`No. Suitable Blastocysts`)
]
d1[blast_rate > 1, blast_rate := NA]

summary(d1$blast_rate)

stopifnot(all(d1$blast_rate >= 0 & d1$blast_rate <= 1, na.rm = TRUE))

### Trigger map ----------------------------------------------------------------

stopifnot(identical(d1$trigger_map, dnew$trigger_map))

d1[, .N, trigger_map]
d1[trigger_map == "NONE", trigger_map := NA]
d1[, .N, trigger_map]

### Protocol map ---------------------------------------------------------------

stopifnot(identical(d1$protocol_map, dnew$protocol_map))

d1[, .N, protocol_map]
d1[protocol_map == "NONE", protocol_map := NA]
d1[, .N, protocol_map]

### Age ------------------------------------------------------------------------

stopifnot(identical(d1[, `Age at Egg Collection`],
                    dnew[, `Age at Egg Collection`]))

d1[`Age at Egg Collection` == -100, `Age at Egg Collection` := NA]

summary(d1[, `Age at Egg Collection`])

### BMI ------------------------------------------------------------------------

stopifnot(identical(d1$BMI, dnew$BMI))

d1[BMI == -1, BMI := NA]

d1[BMI < 18 | BMI > 50, BMI := NA]

summary(d1$BMI)

### Days to trigger ------------------------------------------------------------

setnames(d1, "days to trig", "days_to_trigger")

d1[days_to_trigger < 0, .N]
d1[days_to_trigger < 0, days_to_trigger := NA]

d1[days_to_trigger > 32, .N]
sort(d1[days_to_trigger > 32, days_to_trigger])
d1[days_to_trigger > 32, days_to_trigger := NA]

summary(d1$days_to_trigger)

### Antral follicle count ------------------------------------------------------

stopifnot(identical(d1$AFC_result, dnew$AFC_result))

d1[AFC_result == -1, AFC_result := NA]

ggplot(d1, aes(y = AFC_result, x = "AFC")) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 140, 10)) +
  xlab(element_blank()) +
  theme_bw()

ggsave("../output/01-data-preparation/afc-boxplot.png")

summary(d1$AFC_result)

### Total follicles 11+ --------------------------------------------------------

stopifnot(identical(d1[, `total_follicles_11+_DoT`],
                    dnew[, `total_follicles_11+_DoT`]))

summary(d1$`total_follicles_11+_DoT`)

### Zygotes --------------------------------------------------------------------

stopifnot(identical(d1[, `No. 2PNs`], dnew[, `No. 2PNs`]))

d1[`No. 2PNs` == -1, `No. 2PNs` := NA]

summary(d1[, `No. 2PNs`])

### Blastocysts ----------------------------------------------------------------

stopifnot(identical(d1[, `No. Suitable Blastocysts`],
                    dnew[, `No. Suitable Blastocysts`]))

d1[`No. Suitable Blastocysts` == -1, `No. Suitable Blastocysts` := NA]

summary(d1[, `No. Suitable Blastocysts`])

### Total follicle count -------------------------------------------------------

stopifnot(identical(d1$TFC_6_26mm, dnew[, `6_26mm`]))

summary(d1[, TFC_6_26mm])

## Double check the counts.
tfc <- rowSums(d1[, fs.cols, with = FALSE])

stopifnot(identical(d1$TFC_6_26mm, as.integer(tfc)))

### Mean follicle size ---------------------------------------------------------

summary(d1$mean_size)

mfs <- sapply(d1[, `DoT Follicles`], function(x) {
  a <- gsub("\\[|\\]", "", x)
  a <- as.integer(strsplit(a, ",")[[1]])
  mean(a[a >= 6 & a <= 26])
}, USE.NAMES = FALSE)

summary(d1$mean_size)
summary(mfs)

d1[, mean_size := mfs]

### Percentage in 13-18 --------------------------------------------------------

## d1 <- merge(d1, dnew[, .(PatientIdentifier, in_range13)], sort = FALSE)
## dim(d1)

summary(d1$in_range13)

### Progesterone ---------------------------------------------------------------

## d1 <- merge(d1, dnew[, .(PatientIdentifier, p4_dot)], sort = FALSE)
## dim(d1)

summary(d1$p4_dot)

sum(!is.na(d1$p4_dot) & !is.na(d1$mateggs))

### Clinics --------------------------------------------------------------------

setnames(d1, "Clinic", "clinic")

d1[, .N, clinic][order(-N)]

### Countries ------------------------------------------------------------------

(polish.clinics <- fread("../input/polish-clinics.csv")$ID)

d1[, country := 'UK']
d1[clinic %in% polish.clinics, country := 'Poland']
d1[, country := factor(country, levels = c("UK", "Poland"))]

d1[, .(N = .N, prop = round(.N / nrow(d1), 2)), .(clinic, country)][order(-N)]
d1[, .(N = .N, prop = round(.N / nrow(d1), 2)), country]

### Output ---------------------------------------------------------------------

stopifnot(identical(d1$PatientIdentifier, d0$PatientIdentifier))

## Save the processed dataset.
output.rds.path <- "../data/working/dummy-working.rds"
saveRDS(d1, output.rds.path)
print(md5sum(output.rds.path))

output.csv.path <- "../data/working/dummy-working.csv"
fwrite(d1, output.csv.path)
print(md5sum(output.csv.path))

### Print Session Information  -------------------------------------------------

sink("../logs/session-info/01-data-preparation.Rout")
sessionInfo()
sink()
