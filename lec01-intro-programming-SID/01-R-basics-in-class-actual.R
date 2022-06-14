## Header ------------------------------------------------------------
##
## M19 PHS 5254	Using Administrative Data for Health Services Research
## Washington University School of Medicine in St. Louis
##
## Demonstrate basic R usage with the Florida SID.


## Setup -------------------------------------------------------------

# Load installed packages so we can use their functions.
library(arsenal)
library(crosstable)
library(gmodels)
library(Hmisc)
library(tidyverse)

# Set working directory for this script. Note that file paths on
# Windows leads to conflicts because `\` has special meaning in
# R. Either replace each instance of `\` with `/` or add an additional
# `\` to each existing one.
setwd("//storage1.ris.wustl.edu/colditzg/Active/admin_course_jsahrmann")
# or
setwd("\\\\storage1.ris.wustl.edu\\colditzg\\Active\\admin_course_jsahrmann")

# On Macs, this will look something like
# setwd("/Volumes/Active/admin_course_jsahrmann")


## Input data --------------------------------------------------------

# Read the 1% sample of the Florida SID CORE files. Note the use of a
# relative file path (as opposed to an absolute file path like the one
# used to set the working directory). `..` means 'up one level' in the
# directory structure.
core1p <- read_rds("../admin_course_data/fl_sidc_core_1pSample.rds")


## Data exploration --------------------------------------------------

# Examine data set structure.
str(core1p, list.len = 200)

## Missingness --------------------

x <- c(2, 8, 1, 9, 3, NA)
mean(x, na.rm = TRUE)

1 == 1
1 == 2
1 == NA

is.na(1)
is.na(NA)

## Binary/categorical variables ---

# Get univariate frequencies of expected primary payer.
ftab_PAY1 <- table(core1p$PAY1, useNA = "ifany")
ftab_PAY1 

# Get crosstabs of expected primary payer by year (of discharge).
xtab_PAY1_YEAR <- table(core1p$PAY1, core1p$YEAR, useNA = "ifany")
xtab_PAY1_YEAR

# Get row and column sums.
marginSums(xtab_PAY1_YEAR, margin = 1)
marginSums(xtab_PAY1_YEAR, margin = 2)

# Get cell, row, and column proportions.
proportions(xtab_PAY1_YEAR)
proportions(xtab_PAY1_YEAR, margin = 1)
proportions(xtab_PAY1_YEAR, margin = 2)


# Other packages offer functions with more detailed output.

# `arsenal::freqlist`
summary(freqlist(ftab_PAY1))
summary(freqlist(xtab_PAY1_YEAR))

# `gmodels::CrossTable`
CrossTable(core1p$RACE, core1p$YEAR, prop.chisq = FALSE)

# `crosstable::crosstable`
crosstable(core1p, PAY1, unique_numeric = 20)


## Numeric variables --------------

# Produce a summary of `AGE`.
summary(core1p$AGE)

# Get the first, second, and third quartiles of `TOTCHG`, i.e., the
# median and interquartile range.
quantile(core1p$AGE, probs = c(.25, .5, .75), na.rm = TRUE)

# See the "Summary Functions" section of the dplyr cheat sheet for
# other examples:
# https://www.rstudio.com/resources/cheatsheets/


## Defining variables ------------------------------------------------


## Defining a variable using a simple expression

# Express total charges in thousands of dollars.
summary(core1p$TOTCHG)
core1p_TOTCHG1000 <- mutate(core1p, TOTCHG1000 = TOTCHG / 1000)
summary(core1p_TOTCHG1000$TOTCHG1000)

## Defining a variable using a simple condition

# Replace missing values in `LOS` with the corresponding values in `LOS_X`.
summary(core1p$LOS)
core1p_LOS <- mutate(core1p, LOS = ifelse(is.na(LOS), LOS_X, LOS))
summary(core1p_LOS$LOS)

# Define a variable to distinguish between discharges to home and all
# other types of discharges.
table(core1p$DISPUNIFORM, useNA = "ifany")
core1p_discharge_home <- mutate(
  core1p, discharge_home = ifelse(DISPUNIFORM == 1, 1, 0)
)
table(core1p_discharge_home$discharge_home, useNA = "ifany")

# Define a simplified expected payer variable that groups categories
# 4, 5, and 6 together.
core1p_PAY14a <- mutate(
  core1p,
  PAY14 =
    ifelse(PAY1 == 1, "Medicare",
      ifelse(PAY1 == 2, "Medicaid",
        ifelse(PAY1 == 3, "Private", "Other"))))
table(core1p_PAY14a$PAY14, useNA = "ifany")

core1p_PAY14b <- mutate(
  core1p,
  PAY14 = case_when(
    PAY1 == 1 ~ "Medicare",
    PAY1 == 2 ~ "Medicaid",
    PAY1 == 3 ~ "Private",
    TRUE ~ "Other"
  )
)
table(core1p_PAY14b$PAY14, useNA = "ifany")


## Defining a variable with an expression using multiple columns

# Flag admissions with any type of transfer event.
table(core1p$TRAN_IN, useNA = "ifany")
table(core1p$TRAN_OUT, useNA = "ifany")
core1p_any_transfer <- mutate(
  core1p,
  any_transfer = ifelse(is.na(TRAN_IN), NA,
                   ifelse(TRAN_IN %in% c(1, 2) | TRAN_OUT %in% c(1, 2), 1, 0))
)
table(core1p_any_transfer$any_transfer, useNA = "ifany")


## Categorizing a continuous variable

# Create a variable for age group that matches the grouping used in
# the Census Bureau county-level population data.
summary(core1p$AGE)
core1p_census_age_groups <- mutate(
  core1p,
  census_age_group = cut(
    AGE,
    breaks = c(0, seq(4, 84, by = 5), 108),
    include.lowest = TRUE,
    right = TRUE
  )
)
table(core1p_census_age_groups$census_age_group, useNA = "ifany")

## Defining multiple variables in a single call to `mutate`

# `mutate` allows any number of variables to be defined at once; just
# separate expressions with a comma.
core1p_new_vars <- mutate(
  core1p,
  TOTCHG1000 = TOTCHG / 1000,
  LOS = ifelse(is.na(LOS), LOS_X, LOS),
  discharge_home = ifelse(DISPUNIFORM == 1, 1, 0),
  PAY14 = case_when(
    PAY1 == 1 ~ "Medicare",
    PAY1 == 2 ~ "Medicaid",
    PAY1 == 3 ~ "Private",
    TRUE ~ "Other"),
  any_transfer =
    ifelse(is.na(TRAN_IN), NA,
      ifelse(TRAN_IN %in% c(1, 2) | TRAN_OUT %in% c(1, 2), 1,
        0)),
  census_age_group = cut(
    AGE,
    breaks = c(
      0, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79,
      84, 108),
    include.lowest = TRUE, right = TRUE)
)
