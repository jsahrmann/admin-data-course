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
# admissions within each patient, and record this in a new variable.
# Merge this new variable back into the full set of admissions by
# pediatric patients.


## Data exploration ---------------

# We'll use the `HCUP_ED` variable to identify admissions via the ED.

# Check the distribution of `AGE`.


## Data management ----------------

# Filter to discharges where patient age is less than 18 years.


# Define a dichotomous covariate for admission via ED based on
# `HCUP_ED`, and filter to just these admissions.


# Count the number of unique `VisitLink`s in this data set.


# For each patient, record the earliest `DaysToEvent` across all
# admissions via the ED.


# Merge this variable into the full data set of pediatric admissions.


# Create a dichotomous variable for 'has an ED admission'.
