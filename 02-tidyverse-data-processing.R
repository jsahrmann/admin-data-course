## Header ------------------------------------------------------------
##
## M19 PHS 5254 Using Administrative Data for Health Services Research
## Washington University School of Medicine in St. Louis
##
## Demonstrate common data processing tasks in the Tidyverse.


## Setup -------------------------------------------------------------

library(tidyverse)

# setwd("//storage1.ris.wustl.edu/colditzg/Active/admin_course_jsahrmann")


## Input data --------------------------------------------------------

# Read the 1% sample of the Florida SID CORE files.
core1p <- read_rds("../admin_course_data/fl_sidc_core_1pSample.rds")


## Data management ---------------------------------------------------


## Selecting columns --------------

# Specific columns in a data set can be chosen using `select`, a
# function defined by the Tidyverse package dplyr. `select` offers a
# variety of ways to choose multiple columns at once, making it a
# significant improvement on solutions offered in base R.

# Here are a few examples:

# Print the column names of the 1% sample data set for reference.
colnames(core1p)

# By name
#
# Create a data set of dischcharge- and patient-specific identifiers.
disch_identifiers <- select(core1p, KEY, VisitLink, DaysToEvent, LOS)
colnames(disch_identifiers)

# By a character vector of column names
#
# Create a data set containing the variables related to patient
# residence.
vars_patient_residence <- c(
  "PSTATE", "PSTCO", "PSTCO2", "PSTCO_GEO", "ZIP")
disch_patient_residence <- select(core1p, all_of(vars_patient_residence))
colnames(disch_patient_residence)

# By a common prefix
#
# Create a data set consisting of the procedure code columns.
disch_prcodes <- select(core1p, starts_with("I10_PR"))
colnames(disch_prcodes)

# By a common suffix
#
# Select all of the "from source" columns (i.e., columns whose values
# were not edited by HCUP).
disch_from_source <- select(core1p, ends_with("_X"))
colnames(disch_from_source)

# By a common sequence of characters somewhere in the names
#
# Select all columns with income-related information.
disch_income <- select(core1p, contains("INC"))
colnames(disch_income)

# By sequence based on column order
#
# Select `VisitLink` and `DSHOSPID` and everything in between.
disch_VisitLink_to_DSHOSPID <- select(core1p, VisitLink:DSHOSPID)
colnames(disch_VisitLink_to_DSHOSPID)

# Note that using column positions can be risky. For example, if we
# wanted to select all of the columns with diagnosis codes, we might
# think to write
disch_all_dx_codes_wrong <- select(
  core1p, I10_DX_Admitting:I10_DX34, I10_ECAUSE1:I10_ECAUSE6)
colnames(disch_all_dx_codes_wrong)
# However, recall that different years of data have different numbers
# of diagnosis codes. Because the 1% sample data set was formed by
# stacking the annual CORE files, it turns out that
# I10_DX_Admitting--I10_DX31 are contiguous but not with
# I10_DX32--I10_DX34, so we end up with many more columns than we
# intended. In general, selection based on column names is more
# reliable.
disch_all_dx_codes_right <- select(
  core1p, starts_with("I10_DX"), starts_with("I10_ECAUSE"))
colnames(disch_all_dx_codes_right)
# Regardless, always check your selection with `str`, `colnames`, or
# by printing or `View`ing the resulting data set.

# By negation, i.e., "select everything except..."
#
# Create a data set excluding all columns related to diagnosis and
# procedures codes, including POA codes and the `PRDAY`n fields.
disch_no_dx_or_pr_codes <- select(
  core1p,
  !c(
    starts_with("I10_"), starts_with("DXPOA"), starts_with("E_POA"),
    starts_with("PRDAY"))
)
colnames(disch_no_dx_or_pr_codes)


# Filtering data ------------------

# Note `!` is used for logical negation. The most common use cases are
# `!is.na(x)` returns a logical vector consisting of TRUE (if the
# corresponding value in `x` is a nonmissing value) and FALSE (if the
# corresponding value in `x` is missing).
# !(x %in% y) returns a logical vector consisting of TRUE (if the
# corresponding value in `x` is not in the vector `y`) and FALSE (if
# the corresponding value in `x` is in the vector `y`).
# A shorthand `%nin%` is defined in the Hmisc package.

# Filtering data relies on expressions that return TRUE or FALSE. We
# used several of these types of expressions when defining variables
# last week:
#
# is.na(LOS)
# AGE >= 18
# PAY1 == 1
# TRAN_IN %in% c(1, 2)
#
# Each of these produces a column (technically, a vector) of
# TRUE/FALSE. When creating variables, this was used to determine what
# value was assigned to the variable being created. When subsetting or
# filtering data, the TRUE/FALSE vector (called a logical vector) is
# used to pick which records are included in the output.
#
# In the Tidyverse, we subset data using `filter` (provided by the
# dplyr package). Much like `mutate`, the first argument is a data
# set, and the second and subsequent arguments are expressions that
# return logical vectors. We can combine multiple expressions or write
# them as separate arguments to `filter`, where the arguments are
# implicitly combined using `&`. In other words, the following are
# equivalent:

disch_adults1 <- filter(core1p, !is.na(AGE) & AGE >= 18)
disch_adults2 <- filter(core1p, !is.na(AGE), AGE >= 18)
disch_adults1
disch_adults2

# Expressions connected by 'or' will have to be combined.

disch_transfer_in_or_out <- filter(
  core1p, TRAN_IN %in% c(1, 2) | TRAN_OUT %in% c(1, 2))


# Grouping data -------------------

# We can tell dplyr we want to process data by a grouping factor using
# `group_by`. `group_by` itself doesn't directly change the data set,
# but it changes how other functions work.

# `group_by` is most often used with `summarise` to compute summary
# statistics.

# Create a data set containing the number of dischcharges for each
# patient within the timeframe covered by the data.
disch_grouped_by_patient <- group_by(core1p, VisitLink)
p_dischcharge_counts <- summarise(
  disch_grouped_by_patient, dischchargeCount = n())
# Notice that `summarise` paired with `group_by` changes the level of
# organization of the data---here from dischcharge level to patient
# level.


# The pipe `%>%` operator ---------

# One of the hallmarks of the Tidyverse is the use of a special syntax
# involving the pipe operator `%>%`. The pipe operator takes the
# result of the expression on the left-hand side and passes it as the
# first argument to the expression on the right-hand side. This is
# useful because each of the tidyverse functions we've looked at so
# far---`mutate`, `select`, `filter`, `group_by`, and `summarise`---
# all take a data set as their first argument and produce a new data
# set as output.

# So we can write
disch_identifiers <- select(core1p, KEY, VisitLink, DaysToEvent, LOS)
# as
disch_identifiers <- core1p %>%
  select(KEY, VisitLink, DaysToEvent, LOS)

# The real utility of the pipe operator comes out when we want to
# chain several commands together without needing to save intermediate
# results or writing a nested set of function calls that may be
# difficult to read.

# Suppose we want the procedure code and procedure day columns for all
# adults starting in 2016. We could write

disch_adults <- filter(core1p, !is.na(AGE), AGE >= 18, YEAR >= 2016)
disch_adults_pra <- select(
  disch_adults,
  KEY, VisitLink, DaysToEvent, LOS, starts_with("I0_PR"),
  starts_with("PRDAY"))

# or

disch_adults_prb <-
  select(
    filter(
      core1p,
      !is.na(AGE), AGE >= 18, YEAR >= 2016
    ),
    KEY, VisitLink, DaysToEvent, LOS, starts_with("I0_PR"),
    starts_with("PRDAY")
  )

# But a bit clearer would be

disch_adults_prc <- core1p %>%
  filter(!is.na(AGE), AGE >= 18, YEAR >= 2016) %>%
  select(
    KEY, VisitLink, DaysToEvent, LOS, starts_with("I0_PR"),
    starts_with("PRDAY")
  )
