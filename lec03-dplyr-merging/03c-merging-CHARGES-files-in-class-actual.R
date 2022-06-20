## Header ------------------------------------------------------------
##
## M19 PHS 5254 Using Administrative Data for Health Services Research
## Washington University School of Medicine in St. Louis
##
## Demonstrate merging the CORE and CHARGES files.


## Setup -------------------------------------------------------------

library(tidyverse)

# setwd("//storage1.ris.wustl.edu/colditzg/Active/admin_course_jsahrmann")


## Input data --------------------------------------------------------

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
chgs


## Sample Problem  ---------------------------------------------------

# Characterize use of operating room services in the 1% sample of the
# Florida SID. Create an overall summary as well as summaries
# stratified by (1) `FEMALE` and (2) age. Only use records of Florida
# resident with a nonmissing value for total charges (`TOTCHG`).


## Data exploration ---------------

# Examine total charges in the 1% sample file.
summary(core1p$TOTCHG)
summary(core1p$TOTCHG_X)

# Examine OR charges in the complete charges file. See the HCUP SID
# documentation for an explanation of which revenue center codes
# correspond to each column in the CHARGES files.
summary(chgs$CHG12)


## Data management ----------------

# Keep only dischcharges for Florida residents with a valid `TOTCHG`.
disch_flres_and_valid_TOTCHG <- core1p %>%
  mutate(TOTCHG = ifelse(is.na(TOTCHG), TOTCHG_X, TOTCHG)) %>%
  filter(!is.na(TOTCHG) & PSTATE == "FL")
disch_flres_and_valid_TOTCHG

# Link the dischcharge records with the detailed charges.
disch_all_CHG <- disch_flres_and_valid_TOTCHG %>%
  inner_join(chgs, by = "KEY") %>%
  rename(ORCHG = CHG12)

# Examine OR charges in the 1% sample.
summary(disch_all_CHG$ORCHG)

# Define an indicator variable for OR charges greater than zero.
disch_all_CHG_any_OR <- disch_all_CHG %>%
  mutate(any_OR = ifelse(ORCHG > 0, 1, 0))

# Produce an overall summary.
ftab_any_OR <- table(disch_all_CHG_any_OR$any_OR, useNA = "ifany")
ftab_any_OR
mean(disch_all_CHG_any_OR$any_OR) * 100
quantile(disch_all_CHG_any_OR$ORCHG, probs = c(.25, .5, .75, .95))
mean(disch_all_CHG_any_OR$ORCHG)
sd(disch_all_CHG_any_OR$ORCHG)

# Produce a summary stratified by `FEMALE`.

table(disch_all_CHG_any_OR$FEMALE, useNA = "ifany")

summary_OR_by_FEMALE <- disch_all_CHG_any_OR %>%
  group_by(FEMALE) %>%
  summarise(
    n_any_OR = sum(any_OR),
    pct_any_OR = mean(any_OR) * 100,
    p25_ORCHG = quantile(ORCHG, .25),
    p50_ORCHG = quantile(ORCHG, .5),
    p75_ORCHG = quantile(ORCHG, .75),
    p95_ORCHG = quantile(ORCHG, .95),
    mean_ORCHG = mean(ORCHG),
    sd_ORCHG = sd(ORCHG)
  )
summary_OR_by_FEMALE


# Produce a summary stratified by age group.

summary(disch_all_CHG_any_OR$AGE)

summary_OR_by_age_group <- disch_all_CHG_any_OR %>%
  # Remove records with missing AGE.
  filter(!is.na(AGE)) %>%
  # Define a (fairly arbitrary) age grouping scheme.
  mutate(
    age_group = cut(
      AGE, breaks = c(0, 4, 17, 39, 64, 74, 108),
      labels = c(
        "0-4 y", "5-17 y", "18-39 y", "40-64 y", "65-74 y", "75+ y"),
      include.lowest = TRUE)
    ) %>%
  # Group by the new age group variable so we can compute summary
  # statistics within each group.
  group_by(age_group) %>%
  summarise(
    n_any_OR = sum(any_OR),
    pct_any_OR = mean(any_OR) * 100,
    p25_ORCHG = quantile(ORCHG, 0.25),
    p50_ORCHG = quantile(ORCHG, 0.50),
    p75_ORCHG = quantile(ORCHG, 0.75),
    p95_ORCHG = quantile(ORCHG, 0.95),
    mean_ORCHG = mean(ORCHG),
    sd_ORCHG = sd(ORCHG)
  )
summary_OR_by_age_group
