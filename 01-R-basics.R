## Header ------------------------------------------------------------
##
## M19 PHS 5254 Using Administrative Data for Health Services Research
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

# A good starting point with any new data set is to examine its
# structure using the function `str`. The output is somewhat similar
# to that of a SAS PROC CONTENTS.
str(core1p, list.len = ncol(core1p))

# More detailed (but also much lengthier) output for each variable can
# be generated with `Hmisc::describe`.
describe(core1p)

# We'll mostly be working with data sets. Data sets are collections of
# columns arranged as a table similar to an Excel spreadsheet. Each
# column is a vector---a list of data of the same type (most commonly
# character or numeric). Base R calls data sets `data.frame`s, but
# we'll use a variation called a `tibble` that provides some
# convenient additional features. (Notice that the first line of the
# output of `str` above describes `core1p` as a `tibble`).


## Missingness --------------------

# Understanding missingness is critical. R codes missing values with
# `NA` and is much stricter than SAS.
#
# Any calculations including `NA` will often produce `NA` by default.
mean(core1p$AGE)                   # prints NA
# Setting the argument `na.rm` to `TRUE` will perform the calculation
# ignoring missing values.
mean(core1p$AGE, na.rm = TRUE)
# We also can't test for missingness in the same way as other values.
1 == 1                             # prints TRUE
1 == 2                             # prints FALSE
NA == NA                           # prints NA
# Instead, use the function `is.na`.
is.na(1)                           # prints FALSE
is.na(NA)                          # prints TRUE

# The output of `Hmisc::describe` includes the number of missing
# values for each variable.


## Binary/categorical variables ---

# `table` is the most basic tool for generating frequencies and
# crosstabulations. Unfortunately, `table` doesn't show missing as a
# separate category unless specified. For unfamiliar data, be sure to
# include `useNA = "ifany"` in the function call. Here, we're
# assigning the results to a variable so we can use it as input to
# other functions, but we can skip the assignment if we just want to
# see the frequencies.

# Get univariate frequencies of expected primary payer.
ftab_PAY1 <- table(core1p$PAY1, useNA = "ifany")
ftab_PAY1

# Get crosstabs of expected primary payer by year (of discharge).
xtab_PAY1_YEAR <- table(core1p$PAY1, core1p$YEAR, useNA = "ifany")
xtab_PAY1_YEAR

# We can get row and column sums with `marginSums`. In this function
# (as well as several others), the argument `margin` specifies whether
# we want to apply the function by row (`margin = 1`) or column
# (`margin = 2`). To help remember the distinction, recall that cells
# of a matrix are addressed using the row first and the column second,
# i.e., X[i,j] is the element of the matrix X in the ith row and jth
# column.
marginSums(xtab_PAY1_YEAR, margin = 1)
marginSums(xtab_PAY1_YEAR, margin = 2)

# Similarly, `proportions` prints cell, row, and column proportions.
proportions(xtab_PAY1_YEAR)
proportions(xtab_PAY1_YEAR, margin = 1)
proportions(xtab_PAY1_YEAR, margin = 2)

# Other packages offer functions with more detailed output.

# `arsenal::freqlist` produces output similar to PROC FREQ in SAS (for
# univariate frequencies) and PROC FREQ with the `LIST` option for
# crosstabs.
summary(freqlist(ftab_PAY1))
summary(freqlist(xtab_PAY1_YEAR))

# `gmodels::CrossTable` produces output for crosstabs similar to PROC
# FREQ without the `LIST` option. Beware that this function does not
# report the number of missing values or allow for a separate
# row/column corresponding to `NA`.
CrossTable(core1p$RACE, core1p$YEAR, prop.chisq = FALSE)

# `crosstable::crosstable` is more advanced but allows for lots of
# customization. Note that it assumes numeric variables are continuous
# unless there are fewer than `unique_numeric` unique values. Many
# numeric variables in the SID are actually categorical, so adjust the
# argument as necessary.
crosstable(core1p, PAY1, unique_numeric = 20)
crosstable(core1p, PAY1, by = YEAR, unique_numeric = 20)


## Numeric variables --------------

# `summary` gives a five-number summary along with the mean and number
# of missing values (if any).
summary(core1p$AGE)
# `quantile` returns the quantiles at specified percentiles.
quantile(core1p$TOTCHG, probs = c(0.05, 0.95), na.rm = TRUE)
# See the "Summary Functions" section of the dplyr cheat sheet for
# other examples:
# https://www.rstudio.com/resources/cheatsheets/


## Defining variables ------------------------------------------------

# I encourage the use of the Tidyverse collection of packages for
# manipulating data sets because its functions are designed to be
# easier to understand and use than base R equivalents. The
# documentation is also generally easier to read.
#
# The Tidyverse uses the function `mutate` to define new variables or
# redefine existing variables in a data set. Like many functions for
# manipulating data, the first argument is the name of the data set,
# and subsequent expressions operate on the variables within the input
# data set.
#
# my_data2 <- mutate(
#   my_data,
#   new_var1 = <definition>,
#   new_var2 = <definition>,
#   etc...
# )
#
# We can assign the output to a new data set or overwrite the original
# one; I'll save to new data sets in the examples below.


## Defining a variable using a simple expression

# Suppose we want to express total charges (`TOTCHG`) in thousands of
# dollars.
summary(core1p$TOTCHG)
core1p_TOTCHG1000 <- mutate(core1p, TOTCHG1000 = TOTCHG / 1000)
summary(core1p_TOTCHG1000$TOTCHG1000)


## Defining a variable using a simple condition

# Length of stay (`LOS`) is capped at 365 days to be consistent with
# practices in other states; values larger than that are set to
# missing. Since we're just using Florida, we may as well use the
# actual LOS reported by the source, which is stored in the variable
# `LOS_X`.
summary(core1p$LOS)
core1p_LOS <- mutate(core1p, LOS = ifelse(is.na(LOS), LOS_X, LOS))
summary(core1p_LOS$LOS)

# Let's define a binary variable to distinguish between discharges to
# home and all other types of discharges.
table(core1p$DISPUNIFORM, useNA = "ifany")
core1p_discharge_home <- mutate(
  core1p, discharge_home = ifelse(DISPUNIFORM == 1, 1, 0))
table(core1p_discharge_home$discharge_home, useNA = "ifany")

# As we saw in our data exploration, `PAY1` consists of six unique
# categories, with no missing values. Categories 4, 5, and 6 are
# comparatively rare, so let's create a simplified four-category
# expected payer variable.
#
# One way is with a series of nested `ifelse` function calls.
core1p_PAY14a <- mutate(
  core1p,
  PAY14 =
    ifelse(PAY1 == 1, "Medicare",
      ifelse(PAY1 == 2, "Medicaid",
        ifelse(PAY1 == 3, "Private", "Other"))))
table(core1p_PAY14a$PAY14, useNA = "ifany")
# A somewhat simpler approach is to use `case_when`:
core1p_PAY14b <- mutate(
  core1p,
  PAY14 = case_when(
    PAY1 == 1 ~ "Medicare",
    PAY1 == 2 ~ "Medicaid",
    PAY1 == 3 ~ "Private",
    TRUE ~ "Other"))               # i.e., everything else is "Other"
table(core1p_PAY14b$PAY14, useNA = "ifany")
# But be careful that missing values don't mistakenly end up being
# classified as "Other" by the last clause.


## Defining a variable with an expression using multiple columns

# Our data include variables for transfers in (`TRAN_IN`) and
# transfers out (`TRAN_OUT`). (See documentation for details.) Suppose
# we want to flag admissions that involve any type of transfer event.
table(core1p$TRAN_IN, core1p$TRAN_OUT, useNA = "ifany")
core1p_any_transfer <- mutate(
  core1p,
  any_transfer =
    ifelse(is.na(TRAN_IN), NA,
      ifelse(TRAN_IN %in% c(1, 2) | TRAN_OUT %in% c(1, 2), 1,
        0))
)
table(core1p_any_transfer$any_transfer, useNA = "ifany")
# Note the use of `%in%` to check if the existing variables match a
# particular set of values, as well as the `|` operator for logical
# 'or'. In other words, if either `TRAN_IN` or `TRAN_OUT` (or both)
# indicates a transfer event, then set `any_transfer` to 1.


## Categorizing a continuous variable

# The county-level Census Bureau data that we can link to the SID data
# contain total population but also population in five-year age groups
# (0--4, 5--9, 10--14, ..., 80--84, 85+). To count the number of
# admissions in each age group, we'll first need to create a variable
# that codes those age groups.
max(core1p$AGE, na.rm = TRUE)      # age of oldest patient = 108 years
core1p_census_age_groups <- mutate(
  core1p,
  census_age_group = cut(
    AGE,
    breaks = c(
      0, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79,
      84, 108),
    include.lowest = TRUE, right = TRUE))
table(core1p_census_age_groups$census_age_group, useNA = "ifany")
# Recall that in mathematics, "(" means 'open' or 'exclusive' and "["
# means 'closed' or 'inclusive'. `right = TRUE` means that intervals
# are inclusive on the right boundary and exclusive on the
# left. `include.lowest = TRUE` means that the left boundary of the
# smallest group (i.e., AGE == 0) should be included, making the first
# interval "[0,4]".
#
# Or even better,
core1p_census_age_groups <- mutate(
  core1p,
  census_age_group = cut(
    AGE, breaks = c(0, seq(4, 84, by = 5), 108),
    include.lowest = TRUE, right = TRUE))
table(core1p_census_age_groups$census_age_group, useNA = "ifany")


## Defining multiple variables in a single call to `mutate`

# `mutate` allows any number of variables to be defined at once, just
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
