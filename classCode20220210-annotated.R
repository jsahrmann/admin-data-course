# Front matter ------------------------------------------------------
#
# SP2022.M19.PHS.5254.01
# Using Administrative Data for Clinical and Health Services Research
#
# Demonstrate data set merging with the Florida SID.
#
# John Sahrmann
# 20220210


# Preface -----------------------------------------------------------

library(tidyverse)

setwd("//storage1.ris.wustl.edu/colditzg/Active/admin_course_jsahrmann")


# Input data ---------------------------------------------------------

# Read the 1% sample of the Florida SID CORE files.
core1p <- read_rds("../admin_course_data/fl_sidc_core_1pSample.rds")

# Read the (complete) annual Florida detailed charges files.
chgs15q4 <- read_rds("../admin_course_data/fl_sidc_2015q4_chgs.rds")
chgs16 <- read_rds("../admin_course_data/fl_sidc_2016_chgs.rds")
chgs17 <- read_rds("../admin_course_data/fl_sidc_2017_chgs.rds")
chgs18 <- read_rds("../admin_course_data/fl_sidc_2018_chgs.rds")
chgs19 <- read_rds("../admin_course_data/fl_sidc_2019_chgs.rds")
# Stack across years.
chgs <- bind_rows(chgs15q4, chgs16, chgs17, chgs18, chgs19)


# Sample Problem 1 ---------------------------------------------------

# Characterize use of operating room services in the 1% sample of the
# Florida SID. Create an overall summary as well as summaries
# stratified by (1) `FEMALE` and (2) age. Only use records in the 1%
# sample with a nonmissing value for total charges (`TOTCHG`).


# Data exploration ----------------

# Examine total charges in the 1% sample file.
summary(core1p$TOTCHG)
summary(core1p$TOTCHG_X)
# (In this case, `TOTCHG` and `TOTCHG_X` are the same, but there will
# be cases when working with the full SID data where `TOTCHG` is
# missing but `TOTCHG_x` is not.)

# Examine OR charges in the complete charges file.
summary(chgs$CHG12)
# No missing values


# Data management -----------------

# Keep only discharges with a valid `TOTCHG`.
dis_validTOTCHG <- core1p %>%
  # Replace missing `TOTCHG` with values from source if available.
  mutate(TOTCHG = if_else(is.na(TOTCHG), TOTCHG_X, TOTCHG)) %>%
  # Remove any remaining records with missing charges information.
  filter(!is.na(TOTCHG))

# Link the discharge records with the detailed charges.
dis_allCHG <- dis_validTOTCHG %>%
  inner_join(chgs, by = "KEY") %>%
  rename(ORCHG = CHG12)

# Examine OR charges in the 1% sample.
summary(dis_allCHG$ORCHG)

# Define an indicator variable for OR charges greater than zero.
dis_allCHGwAnyOR <- dis_allCHG %>%
  mutate(anyOR = if_else(ORCHG > 0, 1, 0))

# Produce an overall summary.
ftab_anyOR <- table(dis_allCHGwAnyOR$anyOR, useNA = "ifany")
ftab_anyOR
proportions(ftab_anyOR)
quantile(dis_allCHGwAnyOR$ORCHG, probs = c(0.25, 0.50, 0.75,0.95))
mean(dis_allCHGwAnyOR$ORCHG)
sd(dis_allCHGwAnyOR$ORCHG)

# Produce a summary stratified by `FEMALE`.

table(dis_allCHGwAnyOR$FEMALE, useNA = "ifany")
# No missing values.

summary_ORByFemale <- dis_allCHGwAnyOR %>%
  group_by(FEMALE) %>%
  summarise(
    n_anyOR = sum(anyOR),
    pct_anyOR = mean(anyOR) * 100,
    p25_ORCHG = quantile(ORCHG, 0.25),
    p50_ORCHG = quantile(ORCHG, 0.50),
    p75_ORCHG = quantile(ORCHG, 0.75),
    p95_ORCHG = quantile(ORCHG, 0.95),
    mean_ORCHG = mean(ORCHG),
    sd_ORCHG = sd(ORCHG)
  )
summary_ORByFemale

# Produce a summary stratified by age group.

summary(dis_allCHGwAnyOR$AGE)
# One missing value.

summary_ORByAgeGroup <- dis_allCHGwAnyOR %>%
  # Remove records with missing AGE.
  filter(!is.na(AGE)) %>%
  # Define a (fairly arbitrary) age grouping scheme.
  mutate(
    ageGroup = cut(
      AGE, breaks = c(0, 4, 17, 39, 64, 74, 108),
      labels = c(
        "0-4 y", "5-17 y", "18-39 y", "40-64 y", "65-74 y", "75+ y"),
      include.lowest = TRUE)
  ) %>%
  # Group by the new `ageGroup` variable so we can compute summary
  # statistics within each group.
  group_by(ageGroup) %>%
  summarise(
    n_anyOR = sum(anyOR),
    pct_anyOR = mean(anyOR) * 100,
    p25_ORCHG = quantile(ORCHG, 0.25),
    p50_ORCHG = quantile(ORCHG, 0.50),
    p75_ORCHG = quantile(ORCHG, 0.75),
    p95_ORCHG = quantile(ORCHG, 0.95),
    mean_ORCHG = mean(ORCHG),
    sd_ORCHG = sd(ORCHG)
  )
summary_ORByAgeGroup


# Sample Problem 2 ---------------------------------------------------

# For each Florida resident in the 1% sample of the Florida SID, find
# the earliest discharge record with evidence of admission via the ED
# using HCUP's recommended variable (`HCUP_ED`). Among patients with
# an admission via the ED, in how many cases was the initial ED
# admission preceded by an admission in the prior 30 days.


# Data exploration ----------------

# Examine the distribution of values of `HCUP_ED` in the 1% sample.
table(core1p$HCUP_ED, useNA = "ifany")


# Data management -----------------

# Subset the 1% sample to Florida residents.
dis_flRes <- core1p %>%
  filter(PSTATE == "FL")

# Subset to all discharges with evidence of admission via the ED.
dis_flResAnyED <- dis_flRes %>%
  # Define an indicator for evidence of any ED services.
  mutate(anyED = if_else(HCUP_ED %in% c(2, 4), 1, 0)) %>%
  # Filter to just these records.
  filter(anyED == 1)

# Examine the distribution of the ED indicator.
table(dis_flResAnyED$anyED, useNA = "ifany")

# Save the 'admission date' of the earliest admission via the ED for
# each patient.
p_flResEarliestEDAdmit <- dis_flResAnyED %>%
  group_by(VisitLink) %>%
  summarise(earliestEDAdmit = min(DaysToEvent))
p_flResEarliestEDAdmit

# Select all discharges for patients with an admission via the ED.
dis_flRes2 <- p_flResEarliestEDAdmit %>%
  # In this case, a left join of p_flResEarliestEDAdmit to dis_flRes
  # by `VisitLink` is equivalent to an inner join (because all of the
  # patients in p_flResEarliestEDAdmit are also in dis_flRes. However,
  # a right join of p_flResEarliestEDAdmit to dis_flRes by `VisitLink
  # would *not* be equivalent.
  left_join(dis_flRes, by = "VisitLink") %>%
  # Select just a subset of columns so we can more easily view the
  # output.
  select(KEY, VisitLink, DaysToEvent, LOS, HCUP_ED, earliestEDAdmit)
dis_flRes2

# Filter to discharges where admission was within 30 days of the
# earliest ED admission.
dis_flRes2AdmitPrior30Days <- dis_flRes2 %>%
  filter(
    (earliestEDAdmit - 30) <= DaysToEvent,
    DaysToEvent < earliestEDAdmit
  )
dis_flRes2AdmitPrior30Days

# How many patients does this represent?
p_flResEDAdmitWithAdmitPrior30Days <- dis_flRes2AdmitPrior30Days %>%
  distinct(VisitLink)

nrow(p_flResEarliestEDAdmit)
nrow(p_flResEDAdmitWithAdmitPrior30Days)


# Sample Problem 3 ---------------------------------------------------

# In addition to reporting the number of exclusions for each exclusion
# criterion, Hammond et al. (2020, p 2132) discuss differences in the
# frequencies of certain demographic characteristics between included
# and excluded admissions. Use the data set produced after applying
# the final exclusion in Assignment 2 Task 2 to create a data set
# consisting only of the excluded admissions.

# This is how I solved Assignment 2 Task 2. Note that
# core1p_nonmissingTOTCHG would be the 'data set produced after
# applying the final exclusion' referred to in the problem statement.
nrow(core1p)                       # 102733
core1p_nonmissingRurality <- core1p %>%
  filter(!is.na(PL_NCHS))
nrow(core1p_nonmissingRurality)    # 101683
core1p_adults <- core1p_nonmissingRurality %>%
  filter(!is.na(AGE), AGE >= 18)
nrow(core1p_adults)                #  99377
core1p_nonmissingGender <- core1p_adults %>%
  filter(!is.na(FEMALE))
nrow(core1p_nonmissingGender)      #  99377
core1p_nonmissingZIPIncome <- core1p_nonmissingGender %>%
  filter(!is.na(ZIPINC_QRTL))
nrow(core1p_nonmissingZIPIncome)   #  98113
core1p_nonmissingPAY1 <- core1p_nonmissingZIPIncome %>%
  filter(!is.na(PAY1))
nrow(core1p_nonmissingPAY1)        #  98113
core1p_nonmissingTRANInfo <- core1p_nonmissingPAY1 %>%
  filter(!is.na(TRAN_IN) & !is.na(TRAN_OUT))
nrow(core1p_nonmissingTRANInfo)    #  97782
core1p_nonmissingDISPUNIFORM <- core1p_nonmissingTRANInfo %>%
  filter(!is.na(DISPUNIFORM))
nrow(core1p_nonmissingDISPUNIFORM) #  97782
core1p_nonmissingLOS <- core1p_nonmissingDISPUNIFORM %>%
  filter(!is.na(LOS_X))
nrow(core1p_nonmissingLOS)         #  97782
core1p_nonmissingTOTCHG <- core1p_nonmissingLOS %>%
  filter(!is.na(TOTCHG_X))
nrow(core1p_nonmissingTOTCHG)      #  97781

# Excluded for missing rurality = 1050
# Excluded for missing age or age<18 = 2306
# Excluded for missing gender = 0
# Excluded for missing zip code-level income = 1264
# Excluded for missing primary payer = 0
# Excluded for missing transfer status = 331
# Excluded for missing discharge destination = 0
# Excluded for missing length of stay = 0
# Excluded for missing total charges = 1
# Total excluded = 4952

# Create a data set of excluded discharges by anti-joining the data
# set of included records (i.e., the data set produced after applying
# the final exclusion) with the original data set.
dis_excluded <- core1p %>%
  anti_join(core1p_nonmissingTOTCHG, by = "KEY")
nrow(dis_excluded)
# 4952 is the same as 'Total excluded' above.
