## Header ------------------------------------------------------------
##
## M19 PHS 5254 Using Administrative Data for Health Services Research
## Washington University School of Medicine in St. Louis
##
## Demonstrate merging the CORE and Cost-to-Charge Ratio files.


## Setup -------------------------------------------------------------

library(haven)
library(tidyverse)

# setwd("//storage1.ris.wustl.edu/colditzg/Active/admin_course_jsahrmann")


## Input data --------------------------------------------------------

# Read the 1% sample of the Florida SID CORE files.
core1p <- read_rds("../admin_course_data/fl_sidc_core_1pSample.rds")

# Read the annual AHA Linkage files.
ahal15 <- read_sas("../admin_course_data/fl_sidc_2015_ahal.sas7bdat")
ahal16 <- read_sas("../admin_course_data/fl_sidc_2016_ahal.sas7bdat")
ahal17 <- read_sas("../admin_course_data/fl_sidc_2017_ahal.sas7bdat")
ahal18 <- read_sas("../admin_course_data/fl_sidc_2018_ahal.sas7bdat")
ahal19 <- read_sas("../admin_course_data/fl_sidc_2019_ahal.sas7bdat")
# Stack across years.
ahal <- bind_rows(ahal15, ahal16, ahal17, ahal18, ahal19)
ahal

# Read the annual SID cost-to-charge ratio files. These are provided
# by HCUP as comma-separated value (CSV) files, so reading them into R
# is a bit more complicated. See `?read_csv` for more information on
# the additional arguments.
ccr15 <- read_csv(
  "../admin_course_data/cc2015CD_v2.csv", col_types = "iicidddd",
  na = ".", quote = "\'")
ccr16 <- read_csv(
  "../admin_course_data/cc2016CD_v2.csv", col_types = "iicidddd",
  na = ".", quote = "\'")
ccr17 <- read_csv(
  "../admin_course_data/cc2017CD_v3.csv", col_types = "iicidddd",
  na = ".", quote = "\'")
ccr18 <- read_csv(
  "../admin_course_data/cc2018CDSID_v3.csv", col_types = "iicidddd",
  na = ".", quote = "\'")
ccr19 <- read_csv(
  "../admin_course_data/cc2019CDSID.csv", col_types = "iicidddd",
  na = ".", quote = "\'")
# Stack across years, and keep only Florida records.
ccr <- bind_rows(ccr15, ccr16, ccr17, ccr18, ccr19) %>%
  filter(Z013 == "FL")
ccr


## Sample Problem  ---------------------------------------------------

# Compare the characteristics of hospitals in the admissions that see
# patients residing in the poorest and wealthiest quartile of ZIP
# codes. Use `MEDINCSTQ` to get the income quartile of each patient's
# ZIP code and `HTYPE` (from the HCUP cost-to-charge ratio files) for
# hospital characteristics.


## Data exploration ---------------

# Examine frequencies of `MEDINCSTQ` in the 1% sample data set and
# `HTYPE` in the cost-to-charge ratio files.


# See Table 2 (pp 6--8) in
# https://www.hcup-us.ahrq.gov/db/ccr/ip-ccr/IPCCR-UserGuide-2012-2019.pdf
# for an explanation of the codes. Also copied below for convenience:

# Hospital type for grouping peer hospitals, calculated within State,
# using bed size, ownership/control, and urban/rural location
# 1 = investor-owned, under 100 beds
# 2 = investor-owned, 100 or more beds
# 3 = not-for-profit, rural, under 100 beds
# 4 = not-for-profit, rural, 100 or more beds
# 5 = not-for-profit, urban, under 100 beds
# 6 = not-for-profit, urban, 100–299 beds
# 7 = not-for-profit, urban, 300 or more beds.


## Data management ----------------

# The CORE files identify hospitals by `DSHOSPID`, while the
# cost-to-charge-ratio files identify hospitals using `HOSPID`. The
# two data sets don't share a key on which to merge. Therefore, we
# need to first link to the AHA linkage files before we can add
# `HTYPE` to the 1% sample data set (and the CORE files generally).

# First, merge the 1% sample data set and the AHA linkage data set,
# which will add `HOSPID` to the 1% sample data set. The AHA files are
# released each year, so we will use both `DSHOSPID` *and* `YEAR` as
# keys.


# Check if there are missing values in `HOSPID`.


# Now merge the '1% sample + AHA linkage' data set to the
# cost-to-charge ratio data set.
