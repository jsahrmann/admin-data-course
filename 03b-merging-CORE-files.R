## Header ------------------------------------------------------------
##
## M19 PHS 5254 Using Administrative Data for Health Services Research
## Washington University School of Medicine in St. Louis
##
## Demonstrate merging within the CORE files.


## Setup -------------------------------------------------------------

library(tidyverse)

# setwd("//storage1.ris.wustl.edu/colditzg/Active/admin_course_jsahrmann")


## Input data --------------------------------------------------------

# Read the 1% sample of the Florida SID CORE files.
core1p <- read_rds("../admin_course_data/fl_sidc_core_1pSample.rds")


## Sample Problem  ---------------------------------------------------

# Using the 1% sample of the Florida SID, select records of patients
# younger than 18 years of age who were admitted via the ED. Use
# `group_by`/`summarise` to find the earliest `DaysToEvent` of these
# admissions within each patient, and record this in a variable. Merge
# this new variable back to the full set of admissions by pediatric
# patients.


# Data exploration ----------------

# We'll use the `HCUP_ED` variable to identify admissions via the ED.
table(core1p$HCUP_ED, useNA = "ifany")
# Values 2 and 4 represent evidence of admission via the ED. There are
# no missing values.

# Check the distribution of `AGE`.
summary(core1p$AGE)
# The first quartile is 43 years, so pediatric admissions make up less
# than 25% of records in the 1% sample data set.


# Data management -----------------

# Filter to discharges where patient age is less than 18 years.
disch_peds <- core1p %>%
  filter(!is.na(AGE) & AGE < 18)
disch_peds

# Define a dichotomous covariate for admission via ED based on
# `HCUP_ED`, and filter to just these admissions.
disch_peds_ED_admit <- disch_peds %>%
  mutate(ED_admit = ifelse(HCUP_ED %in% c(2, 4), 1, 0)) %>%
  filter(ED_admit == 1) %>%
  relocate(HCUP_ED, ED_admit, .after = AGE)
disch_peds_ED_admit

# Count the number of unique `VisitLink`s in this data set.
n_distinct(disch_peds_ED_admit$VisitLink)

# Notice that this patient has two ED admission.
filter(disch_peds_ED_admit, VisitLink == 24420019)

# For each patient, record the earliest `DaysToEvent` across all
# admissions via the ED.
p_peds_earliest_ED_admit <- disch_peds_ED_admit %>%
  group_by(VisitLink) %>%
  summarise(earliest_ED_admit = min(DaysToEvent))
p_peds_earliest_ED_admit

# Check that our example patient has only one record and that it
# contains the earliest `DaysToEvent` across the two admission.
filter(p_peds_earliest_ED_admit, VisitLink == 24420019)

# Merge this variable into the full data set of pediatric admissions.
disch_peds_with_earliest_ED_admit <- disch_peds %>%
  left_join(p_peds_earliest_ED_admit, by = "VisitLink") %>%
  relocate(earliest_ED_admit)
disch_peds_with_earliest_ED_admit

# Check that `earliest_ED_admit` has been added to all of the records
# of our example patient.
filter(disch_peds_with_earliest_ED_admit, VisitLink == 24420019)

# Create a dichotomous variable for 'has an ED admission'.
disch_peds_with_earliest_ED_admit <- disch_peds_with_earliest_ED_admit %>%
  mutate(
    has_ED_admit_ever = ifelse(!is.na(earliest_ED_admit), 1, 0),
    .before = 1
  )

filter(disch_peds_with_earliest_ED_admit, VisitLink == 24420019)
