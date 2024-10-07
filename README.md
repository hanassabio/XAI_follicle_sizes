# XAI for Follicle Sizes

## For the scripts contained in ./ML_modelling

All files should run on Linux or Windows 11 with relevant packages installed as noted in each workbook. Each file corresponds to one model developed. Uses "dummy_data.xlsx" as the data source. Models with dummy data should take no more than 15 minutes to run for each model.

## For the scripts contained in ./PDP_plots

All files should run on Linux or Windows with relevant packages installed as noted in each workbook. These are to generate the data for the partial dependence plots (PDPs) in Figures 5a and 5b. Uses "dummy_data.xlsx" as the data source. This should take no longer than a handful of minutes with the dummy data.

## For the scripts contained in ./analysis
### Description:
-----------

All scripts are provided in the ./scripts folder. Each scripts is numbered
according to its execution sequence and has a header that describes what the
script does. Uses data in "data/input/" as the data sources. Uses "/input/" for supplementary information regarding clinics. Each script should take a handful of minutes to run with the dummy data provided.


### Required OS:
-----------

Linux


### Required Linux Tools:
--------------------

bash
awk
md5sum


### Required R packages:
-------------------

tools
data.table
ggplot2


### R Session Information:
---------------------

R version 4.3.3 (2024-02-29)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 24.04 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.12.0
LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.12.0

locale:
 [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C
 [3] LC_TIME=en_GB.UTF-8        LC_COLLATE=en_GB.UTF-8
 [5] LC_MONETARY=en_GB.UTF-8    LC_MESSAGES=en_GB.UTF-8
 [7] LC_PAPER=en_GB.UTF-8       LC_NAME=C
 [9] LC_ADDRESS=C               LC_TELEPHONE=C
[11] LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C

time zone: Europe/London
tzcode source: system (glibc)

attached base packages:
[1] tools     stats     graphics  grDevices utils     datasets  methods   base

other attached packages:
[1] ggplot2_3.4.4      data.table_1.14.10

loaded via a namespace (and not attached):
 [1] utf8_1.2.4       R6_2.5.1         tidyselect_1.2.0 magrittr_2.0.3
 [5] gtable_0.3.4     glue_1.7.0       tibble_3.2.1     pkgconfig_2.0.3
 [9] generics_0.1.3   dplyr_1.1.4      lifecycle_1.0.4  cli_3.6.2
[13] fansi_1.0.5      scales_1.3.0     grid_4.3.3       vctrs_0.6.5
[17] withr_2.5.0      compiler_4.3.3   munsell_0.5.0    pillar_1.9.0
[21] colorspace_2.1-0 rlang_1.1.3      MASS_7.3-60.0.1
