## Header ------------------------------------------------------------
##
## M19 PHS 5254 Using Administrative Data for Health Services Research
## Washington University School of Medicine in St. Louis
##
## Demonstrate assessing the timing of procedures.


## Setup -------------------------------------------------------------

library(tidyverse)

# setwd("//storage1.ris.wustl.edu/colditzg/Active/admin_course_jsahrmann")

source("../admin_course_data/code2023/coder.R")


## Constant definitions ----------------------------------------------

# Mastectomy
# 0HTT0ZZ, 0HTU0ZZ, 0HTV0ZZ
pr10_mastectomy <- c("0HTT0ZZ", "0HTU0ZZ", "0HTV0ZZ")
# Extirpation of breast
# 0HC[TUV][0378X]ZZ
pr10_extirpation_of_breast <- c(
  "0HCT0ZZ", "0HCT3ZZ", "0HCT7ZZ", "0HCT8ZZ", "0HCTXZZ",
  "0HCU0ZZ", "0HCU3ZZ", "0HCU7ZZ", "0HCU8ZZ", "0HCUXZZ",
  "0HCV0ZZ", "0HCV3ZZ", "0HCV7ZZ", "0HCV8ZZ", "0HCVXZZ"
)


## Input data --------------------------------------------------------

# Read the full SID data.
core <- bind_rows(
  read_rds("../admin_course_data/fl_sidc_2015q4_core.rds"),
  read_rds("../admin_course_data/fl_sidc_2016_core.rds"),
  read_rds("../admin_course_data/fl_sidc_2017_core.rds"),
  read_rds("../admin_course_data/fl_sidc_2018_core.rds"),
  read_rds("../admin_course_data/fl_sidc_2019_core.rds")
)


## Procedure dates ---------------------------------------------------

# Identify admissions with a procedure code for mastectomy, and assign
# a date to the procedure.
dsch_mastectomy <- core %>%
  mutate_date_pr(
    codes = pr10_mastectomy, name = date_pr_mastectomy
  ) %>%
  filter(!is.na(date_pr_mastectomy))

# Search for a procedure code indicating complication after
# mastectomy, and if the procedure occurs after the mastectomy, flag
# it as an outcome.
dsch_mastectomy_with_complications <- dsch_mastectomy %>%
  mutate_date_pr(
    codes = pr10_extirpation_of_breast,
    name = date_pr_extirpation_of_breast
  ) %>%
  mutate(
    mastectomy_complication = case_when(
      is.na(date_pr_extirpation_of_breast)                ~ NA_real_,
      date_pr_extirpation_of_breast >= date_pr_mastectomy ~ 1,
      TRUE                                                ~ 0
    )
  )

# Examine the frequency of this type of complication after mastectomy.
table(dsch_mast_with_complications$complication, useNA = "ifany")
