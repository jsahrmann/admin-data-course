## Header ------------------------------------------------------------
##
## M19 PHS 5254 Using Administrative Data for Health Services Research
## Washington University School of Medicine in St. Louis
##
## Demonstrate typical data management tasks for an admission-level
## analysis.


# Sample problem: Identify admissions with a primary diagnosis of
# acute myocardial infarction. Exclude admissions where age or sex is
# missing, age is less than 18, or that were transfers in. Check for
# diagnoses indicating congestive heart failure among the remaining
# admissions. Calculate the rate of in-hospital mortality overall,
# stratified by pre-existing CHF, and then further stratified by sex.


## Setup -------------------------------------------------------------

library(tidyverse)

# setwd("//storage1.ris.wustl.edu/colditzg/Active/admin_course_jsahrmann")

source("../admin_course_data/code2023/coder.R")


## Constant definitions ----------------------------------------------


## Diagnoses ----------------------

# Acute myocardial infarction
dx10_acutemi <- c(
  "I2101", "I2102", "I2109", "I2111", "I2119", "I2121", "I2129",
  "I213", "I214", "I220", "I221", "I222", "I228", "I229"
)
# Congestive heart failure
dx10_chf <- c(
  "I0981", "I501", "I5020", "I5021", "I5022", "I5023", "I5030",
  "I5031", "I5032", "I5033", "I5040", "I5041", "I5042", "I5043",
  "I50810", "I50811", "I50812", "I50813", "I50814", "I5082", "I5083",
  "I5084", "I5089", "I509"
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
# 600 s


## Primary inclusion criterion ---------------------------------------

# Identify admissions with a primary diagnosis of acute myocardial
# infarction.
core_acutemi <- core %>%
  mutate_flag_dx1(codes = dx10_acutemi, name = dx1_acutemi) %>%
  filter(dx1_acutemi == 1)
# 38 s

# Equivalently,
# core_acutemi <- core %>%
#   mutate(
#     dx1_acutemi = case_when(
#       I10_DX_Admitting %in% dx10_acutemi ~ 1,
#       I10_DX1 %in% dx10_acutemi          ~ 1,
#       TRUE                               ~ 0
#     )
#   ) %>%
#   filter(dx1_acutemi == 1)

# We no longer need the full CORE data set, so remove it in order to
# free up memory.
rm(core)

# Record the initial sample size (in number of admissions).
nrow(core_acutemi)                 # 216949


## Exclusions --------------------------------------------------------

# Exclude for missing age or age < 18. (We could do this directly with
# `filter`. In this case, we'll create variables flagging admissions
# for exclusion. Once we've identified all exclusion criteria, we'll
# apply each one-by-one. The benefit of this approach is that it makes
# it easy to reorder our exclusions later.)
core_acutemi <- core_acutemi %>%
  mutate(
    exclude_missing_age = ifelse(is.na(AGE), 1, 0),
    exclude_age_less_than_18 = ifelse(AGE < 18, 1, 0)
  )

# Flag exclusions for missing sex.
core_acutemi <- core_acutemi %>%
  mutate(exclude_missing_sex = ifelse(is.na(FEMALE), 1, 0))

# Flag exclusions for transfer in.
core_acutemi <- core_acutemi %>%
  mutate(exclude_transfer_in = ifelse(TRAN_IN != 0, 1, 0))

# Apply exclusions, and record the number excluded.

core_acutemi_age_exclusion <- core_acutemi %>%
  filter(exclude_missing_age == 0)
nrow(core_acutemi_age_exclusion)        # 216944

core_acutemi_age_exclusion <- core_acutemi_age_exclusion %>%
  filter(exclude_age_less_than_18 == 0)
nrow(core_acutemi_age_exclusion)        # 216938

core_acutemi_sex_exclusion <- core_acutemi_age_exclusion %>%
  filter(exclude_missing_sex == 0)
nrow(core_acutemi_sex_exclusion)        # 216931

core_acutemi_transfer_exclusion <- core_acutemi_sex_exclusion %>%
  filter(exclude_transfer_in == 0)
nrow(core_acutemi_transfer_exclusion)   # 193489

# Give the final data set from this step a more memorable name.
core_acutemi_include <- core_acutemi_transfer_exclusion

# Check the distributions of the variables involved in
# inclusion/exclusion to make sure everything worked correctly.
summary(core_acutemi_include$AGE)
table(core_acutemi_include$FEMALE, useNA = "ifany")
table(core_acutemi_include$TRAN_IN, useNA = "ifany")


## Covariates --------------------------------------------------------

# Flag admissions with a diagnosis of congestive heart failure.
core_acutemi_flag_chf <- core_acutemi_include %>%
  mutate_flag_dx(codes = dx10_chf, name = dx_chf)
table(core_acutemi_flag_chf$dx_chf, useNA = "ifany")

# Give the final data set from this step a more memorable name.
core_acutemi_covariates <- core_acutemi_flag_chf


## Outcomes ----------------------------------------------------------

table(core_acutemi_covariates$DIED, useNA = "ifany")

# DIED will serve as the outcome variable, so no further programming
# is needed here.

# Give the final data set from this step a more memorable name.
core_acutemi_final <- core_acutemi_covariates


## Analysis ----------------------------------------------------------

# Calculate the rate of in-hospital mortality across the entire
# sample. We'll express it as the number of in-hospital deaths per
# 1,000 admissions for acute MI.
mean(core_acutemi_final$DIED) * 1000

# Calculate the rate by dx_chf.
core_acutemi_final %>%
  group_by(dx_chf) %>%
  summarise(mortality_per1000 = mean(DIED) * 1000)

# Equivalently,
# mean(core_acutemi_final$DIED[core_acutemi_final$dx_chf == 0]) * 1000
# mean(core_acutemi_final$DIED[core_acutemi_final$dx_chf == 1]) * 1000
# Or,
# tapply(core_acutemi_final$DIED, core_acutemi_final$dx_chf, mean) * 1000

# Calculate the rate by dx_chf and FEMALE.
core_acutemi_final %>%
  group_by(dx_chf, FEMALE) %>%
  summarise(mortality_per1000 = mean(DIED) * 1000)
