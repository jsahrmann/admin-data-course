## Header ------------------------------------------------------------
##
## M19 PHS 5254 Using Administrative Data for Health Services Research
## Washington University School of Medicine in St. Louis
##
## Demonstrate identifying diagnosis and procedure codes.


## Setup -------------------------------------------------------------

library(tidyverse)

# setwd("//storage1.ris.wustl.edu/colditzg/Active/admin_course_jsahrmann")


## Constant definitions ----------------------------------------------


## Diagnoses ----------------------

# Acute myocardial infarction
# I21.0-I21.4, I22.0-I22.2, I22.8, I22.9
dx10_acutemi <- c(
  "I2101", "I2102", "I2109", "I2111", "I2119", "I2121", "I2129",
  "I213", "I214", "I220", "I221", "I222", "I228", "I229"
)
# Ischemic Stroke
# I63.0-I63.6, I63.8, I63.9
dx10_istroke <- c(
  "I6300", "I63011", "I63012", "I63013", "I63019", "I6302", "I63031",
  "I63032", "I63033", "I63039", "I6309", "I6310", "I63111", "I63112",
  "I63113", "I63119", "I6312", "I63131", "I63132", "I63133", "I63139",
  "I6319", "I6320", "I63211", "I63212", "I63213", "I63219", "I6322",
  "I63231", "I63232", "I63233", "I63239", "I6329", "I6330", "I63311",
  "I63312", "I63313", "I63319", "I63321", "I63322", "I63323",
  "I63329", "I63331", "I63332", "I63333", "I63339", "I63341",
  "I63342", "I63343", "I63349", "I6339", "I6340", "I63411", "I63412",
  "I63413", "I63419", "I63421", "I63422", "I63423", "I63429",
  "I63431", "I63432", "I63433", "I63439", "I63441", "I63442",
  "I63443", "I63449", "I6349", "I6350", "I63511", "I63512", "I63513",
  "I63519", "I63521", "I63522", "I63523", "I63529", "I63531",
  "I63532", "I63533", "I63539", "I63541", "I63542", "I63543",
  "I63549", "I6359", "I636", "I638", "I6381", "I6389", "I639"
)


## Procedures ---------------------

# Coronary artery bypass graft
# 02100*, 021048*, 021049*, 02104A*, 02104J*, 02104K*, 02104Z*,
# 02110*, 021148*, 021149*, 02114A*, 02114J*, 02114K*, 02114Z*,
# 02120*, 021248*, 021249*, 02124A*, 02124J*, 02124K*, 02124Z*,
# 02130*, 021348*, 021349*, 02134A*, 02134J*, 02134K*, 02134Z*
pr10_cabg <- c(
  "0210083", "0210088", "0210089", "021008C", "021008F", "021008W",
  "0210093", "0210098", "0210099", "021009C", "021009F", "021009W",
  "02100A3", "02100A8", "02100A9", "02100AC", "02100AF", "02100AW",
  "02100J3", "02100J8", "02100J9", "02100JC", "02100JF", "02100JW",
  "02100K3", "02100K8", "02100K9", "02100KC", "02100KF", "02100KW",
  "02100Z3", "02100Z8", "02100Z9", "02100ZC", "02100ZF", "0210444",
  "0210483", "0210488", "0210489", "021048C", "021048F", "021048W",
  "0210493", "0210498", "0210499", "021049C", "021049F", "021049W",
  "02104A3", "02104A8", "02104A9", "02104AC", "02104AF", "02104AW",
  "02104D4", "02104J3", "02104J8", "02104J9", "02104JC", "02104JF",
  "02104JW", "02104K3", "02104K8", "02104K9", "02104KC", "02104KF",
  "02104KW", "02104Z3", "02104Z8", "02104Z9", "02104ZC", "02104ZF",
  "0211083", "0211088", "0211089", "021108C", "021108F", "021108W",
  "0211093", "0211098", "0211099", "021109C", "021109F", "021109W",
  "02110A3", "02110A8", "02110A9", "02110AC", "02110AF", "02110AW",
  "02110J3", "02110J8", "02110J9", "02110JC", "02110JF", "02110JW",
  "02110K3", "02110K8", "02110K9", "02110KC", "02110KF", "02110KW",
  "02110Z3", "02110Z8", "02110Z9", "02110ZC", "02110ZF", "0211344",
  "02113D4", "0211444", "0211483", "0211488", "0211489", "021148C",
  "021148F", "021148W", "0211493", "0211498", "0211499", "021149C",
  "021149F", "021149W", "02114A3", "02114A8", "02114A9", "02114AC",
  "02114AF", "02114AW", "02114D4", "02114J3", "02114J8", "02114J9",
  "02114JC", "02114JF", "02114JW", "02114K3", "02114K8", "02114K9",
  "02114KC", "02114KF", "02114KW", "02114Z3", "02114Z8", "02114Z9",
  "02114ZC", "02114ZF", "0212083", "0212088", "0212089", "021208C",
  "021208F", "021208W", "0212093", "0212098", "0212099", "021209C",
  "021209F", "021209W", "02120A3", "02120A8", "02120A9", "02120AC",
  "02120AF", "02120AW", "02120J3", "02120J8", "02120J9", "02120JC",
  "02120JF", "02120JW", "02120K3", "02120K8", "02120K9", "02120KC",
  "02120KF", "02120KW", "02120Z3", "02120Z8", "02120Z9", "02120ZC",
  "02120ZF", "0212344", "02123D4", "0212444", "0212483", "0212488",
  "0212489", "021248C", "021248F", "021248W", "0212493", "0212498",
  "0212499", "021249C", "021249F", "021249W", "02124A3", "02124A8",
  "02124A9", "02124AC", "02124AF", "02124AW", "02124D4", "02124J3",
  "02124J8", "02124J9", "02124JC", "02124JF", "02124JW", "02124K3",
  "02124K8", "02124K9", "02124KC", "02124KF", "02124KW", "02124Z3",
  "02124Z8", "02124Z9", "02124ZC", "02124ZF", "0213083", "0213088",
  "0213089", "021308C", "021308F", "021308W", "0213093", "0213098",
  "0213099", "021309C", "021309F", "021309W", "02130A3", "02130A8",
  "02130A9", "02130AC", "02130AF", "02130AW", "02130J3", "02130J8",
  "02130J9", "02130JC", "02130JF", "02130JW", "02130K3", "02130K8",
  "02130K9", "02130KC", "02130KF", "02130KW", "02130Z3", "02130Z8",
  "02130Z9", "02130ZC", "02130ZF", "0213344", "02133D4", "0213444",
  "0213483", "0213488", "0213489", "021348C", "021348F", "021348W",
  "0213493", "0213498", "0213499", "021349C", "021349F", "021349W",
  "02134A3", "02134A8", "02134A9", "02134AC", "02134AF", "02134AW",
  "02134D4", "02134J3", "02134J8", "02134J9", "02134JC", "02134JF",
  "02134JW", "02134K3", "02134K8", "02134K9", "02134KC", "02134KF",
  "02134KW", "02134Z3", "02134Z8", "02134Z9", "02134ZC", "02134ZF"
)


## Input data --------------------------------------------------------

# Read the 1% sample of the Florida SID CORE files.
core1p <- read_rds("../admin_course_data/fl_sidc_core_1pSample.rds")


## Variable creation -------------------------------------------------

# Our goal is to determine whether a particular diagnosis or procedure
# was coded during an admission. To put this in more
# programming-specific terms, we want to ask whether any of the
# diagnosis codes recorded in `I10_DX`n (and perhaps `I10_ECAUSE`n)
# match any of the codes for our diagnosis or procedure of interest.

# In order to get a sense for what's involved, we'll pretend we have
# an admission with a single code.

my_data <- data.frame(KEY = 1, DX1 = "I220")

# Suppose we want to know if this code matches any of the codes for
# acute MI. One way we've compared values in the past is with `==`. So
# we could write

# my_data %>%
#   mutate(
#     dx_acutemi = ifelse(DX1 == "I2101" | DX1 == "I2102" | ..., 1, 0)
#   )

# However, with the sheer number of codes in ICD-10-CM/PCS, this isn't
# practical. We've already seen a way of checking whether a value is
# in a specific set of values using `%in%`, so we can write this more
# concisely as

my_data %>%
  mutate(
    dx_acutemi = ifelse(DX1 %in% dx10_acutemi, 1, 0)
  )
#   KEY  DX1 dx_acutemi
# 1   1 I220          1

# To add one layer of complexity, what if we have multiple admissions
# each with a single diagnosis code column, and we want to know
# whether the code associated with each admission is in the list of
# codes for the diagnosis or procedure of interest? We've actually
# been doing something like this all along, albeit with other
# variables (e.g., `PAY1`, `AHOUR`), so the fact that we're asking
# whether the code for each admission is in the list doesn't require
# anything new.

my_data <- data.frame(
  KEY = 1:2,
  DX1 = c("I220", "E0800")
)

my_data %>%
  mutate(dx_acutemi = ifelse(DX1 %in% dx10_acutemi, 1, 0))
#   KEY   DX1 dx_acutemi
# 1   1  I220          1
# 2   2 E0800          0

# R automatically 'vectorizes' the operation over the entire column
# `DX1`.

# But we have multiple columns of diagnosis and procedure codes for
# each admission, and we need to ask whether (for each admission) any
# of the codes in the relevant columns match any of the codes in the
# list of diagnosis or procedure codes of interest.

# Again, this isn't really anything new.

my_data <- data.frame(
  KEY = 1:3,
  DX1 = c("I220", "I209", "J80"),
  DX2 = c("E0800", "I220", "O9921")
)

my_data %>%
  mutate(
    dx_acutemi = ifelse(
      DX1 %in% dx10_acutemi | DX2 %in% dx10_acutemi, 1, 0)
  )
#   KEY  DX1   DX2 dx_acutemi
# 1   1 I220 E0800          1
# 2   2 I209  I220          1
# 3   3  J80 O9921          0

# So really our main problem is that we have LOTS of columns with
# diagnosis and procedure codes, and since we're also likely to have
# many diagnoses and procedures that we'd like to identify, it can be
# tedious writing out such a long expression every time.

# One way to tackle this is with a long call to the `case_when`
# function.

core1p2 <- core1p %>%
  mutate(
    dx_acutemi = case_when(
      I10_DX_Admitting %in% dx10_acutemi ~ 1,
      I10_DX1 %in% dx10_acutemi ~ 1,
      I10_DX2 %in% dx10_acutemi ~ 1,
      I10_DX3 %in% dx10_acutemi ~ 1,
      I10_DX4 %in% dx10_acutemi ~ 1,
      I10_DX5 %in% dx10_acutemi ~ 1,
      I10_DX6 %in% dx10_acutemi ~ 1,
      I10_DX7 %in% dx10_acutemi ~ 1,
      I10_DX8 %in% dx10_acutemi ~ 1,
      I10_DX9 %in% dx10_acutemi ~ 1,
      I10_DX10 %in% dx10_acutemi ~ 1,
      I10_DX11 %in% dx10_acutemi ~ 1,
      I10_DX12 %in% dx10_acutemi ~ 1,
      I10_DX13 %in% dx10_acutemi ~ 1,
      I10_DX14 %in% dx10_acutemi ~ 1,
      I10_DX15 %in% dx10_acutemi ~ 1,
      I10_DX16 %in% dx10_acutemi ~ 1,
      I10_DX17 %in% dx10_acutemi ~ 1,
      I10_DX18 %in% dx10_acutemi ~ 1,
      I10_DX19 %in% dx10_acutemi ~ 1,
      I10_DX20 %in% dx10_acutemi ~ 1,
      I10_DX21 %in% dx10_acutemi ~ 1,
      I10_DX22 %in% dx10_acutemi ~ 1,
      I10_DX23 %in% dx10_acutemi ~ 1,
      I10_DX24 %in% dx10_acutemi ~ 1,
      I10_DX25 %in% dx10_acutemi ~ 1,
      I10_DX26 %in% dx10_acutemi ~ 1,
      I10_DX27 %in% dx10_acutemi ~ 1,
      I10_DX28 %in% dx10_acutemi ~ 1,
      I10_DX29 %in% dx10_acutemi ~ 1,
      I10_DX30 %in% dx10_acutemi ~ 1,
      I10_DX31 %in% dx10_acutemi ~ 1,
      I10_DX32 %in% dx10_acutemi ~ 1,
      I10_DX33 %in% dx10_acutemi ~ 1,
      I10_DX34 %in% dx10_acutemi ~ 1,
      I10_ECAUSE1 %in% dx10_acutemi ~ 1,
      I10_ECAUSE2 %in% dx10_acutemi ~ 1,
      I10_ECAUSE3 %in% dx10_acutemi ~ 1,
      I10_ECAUSE4 %in% dx10_acutemi ~ 1,
      I10_ECAUSE5 %in% dx10_acutemi ~ 1,
      I10_ECAUSE6 %in% dx10_acutemi ~ 1,
      TRUE ~ 0
    )
  )
table(core1p2$dx_acutemi, useNA = "ifany")

# Things are much easier if we're only concerned with 'primary' (i.e.,
# in `I10_DX1` only) diagnoses of our condition of interest.

core1p2 <- core1p %>%
  mutate(
    dx1_acutemi = ifelse(I10_DX1 %in% dx10_acutemi, 1, 0)
  )
table(core1p2$dx1_acutemi, useNA = "ifany")

# Identifying procedures works in a similar way.

core1p2 <- core1p %>%
  mutate(
    pr_cabg = case_when(
      I10_PR1 %in% pr10_cabg ~ 1,
      I10_PR2 %in% pr10_cabg ~ 1,
      I10_PR3 %in% pr10_cabg ~ 1,
      I10_PR4 %in% pr10_cabg ~ 1,
      I10_PR5 %in% pr10_cabg ~ 1,
      I10_PR6 %in% pr10_cabg ~ 1,
      I10_PR7 %in% pr10_cabg ~ 1,
      I10_PR8 %in% pr10_cabg ~ 1,
      I10_PR9 %in% pr10_cabg ~ 1,
      I10_PR10 %in% pr10_cabg ~ 1,
      I10_PR11 %in% pr10_cabg ~ 1,
      I10_PR12 %in% pr10_cabg ~ 1,
      I10_PR13 %in% pr10_cabg ~ 1,
      I10_PR14 %in% pr10_cabg ~ 1,
      I10_PR15 %in% pr10_cabg ~ 1,
      I10_PR16 %in% pr10_cabg ~ 1,
      I10_PR17 %in% pr10_cabg ~ 1,
      I10_PR18 %in% pr10_cabg ~ 1,
      I10_PR19 %in% pr10_cabg ~ 1,
      I10_PR20 %in% pr10_cabg ~ 1,
      I10_PR21 %in% pr10_cabg ~ 1,
      I10_PR22 %in% pr10_cabg ~ 1,
      I10_PR23 %in% pr10_cabg ~ 1,
      I10_PR24 %in% pr10_cabg ~ 1,
      I10_PR25 %in% pr10_cabg ~ 1,
      I10_PR26 %in% pr10_cabg ~ 1,
      I10_PR27 %in% pr10_cabg ~ 1,
      I10_PR28 %in% pr10_cabg ~ 1,
      I10_PR29 %in% pr10_cabg ~ 1,
      I10_PR30 %in% pr10_cabg ~ 1,
      I10_PR31 %in% pr10_cabg ~ 1,
      TRUE ~ 0
    )
  )
table(core1p2$pr_cabg, useNA = "ifany")

# (Note that unlike diagnosis codes, there's no special significance
# to the code in `I10_PR1`.)


## Programming options -----------------------------------------------

# The problem of identifying diagnoses and procedures across a set of
# columns is a difficult one without an obvious, simple answer. The
# approach outlined above, while verbose, is relatively easy to copy
# and modify, and it is much faster than a few alternatives that I've
# explored.

# Another option is to make use of a set of functions that I've
# defined that encapsulates the code written above. The simplest
# functions take as input a data set (such as the 1% sample data set),
# a vector of diagnosis/procedure codes like those defined above, and
# the name of the new variable you wish to create. It outputs the data
# set, but with the new column added to it. Because the functions use
# `mutate` under the hood, they're named `mutate_flag_dx`,
# `mutate_flag_pr`, etc. Here are examples that mirror the ones run
# previously.

# First we need to read the R script in which the functions are
# defined so that we have access to them. (This is similar to running
# `library` for a package.) We do this with `source`:

source("//storage1.ris.wustl.edu/colditzg/Active/admin_course_data/code2023/coder.R")

# This specifies the absolute path to the script on a Windows
# machine. For Mac users, you would need to use this

# source("/Volumes/Active/admin_course_data/code2023/coder.R")


## `mutate_flag_dx` for creating 0/1 variables for diagnoses

core1p2a <- mutate_flag_dx(core1p, dx10_acutemi, dx_acutemi)
# Notice that `dx10_acutemi` is the name of the vector containing our
# diagnosis codes, and `dx_acutemi` is the name of the new variable we
# want to create.
table(core1p2a$dx_acutemi, useNA = "ifany")

# As with `mutate`, we can use `%>%` to produce a chain of function
# calls.

core1p2b <- core1p %>%
  mutate_flag_dx(dx10_acutemi, dx_acutemi)
table(core1p2b$dx_acutemi, useNA = "ifany")

# This makes it possible to define more than one variable in the same
# block of code.

core1p3 <- core1p %>%
  mutate_flag_dx(dx10_acutemi, dx_acutemi) %>%
  mutate_flag_dx(dx10_istroke, dx_istroke)
table(core1p3$dx_acutemi, useNA = "ifany")
table(core1p3$dx_istroke, useNA = "ifany")

# While still somewhat awkward, this beats having to constantly
# copy-paste the long `case_when` function call for each diagnosis of
# interest.


## `mutate_flag_dx1` for creating 0/1 variables for primary diagnoses

# The coder.R script also defines a function for flagging primary
# diagnoses. (However, as shown above, this doesn't take much code, so
# you can create those variables yourself if you choose; I provide
# this function for consistency.)

core1p4 <- core1p %>%
  mutate_flag_dx1(dx10_acutemi, dx1_acutemi)
table(core1p4$dx1_acutemi, useNA = "ifany")


## `mutate_flag_pr` for creating 0/1 variables for procedures

# A similar function works for procedure codes.

core1p5 <- core1p %>%
  mutate_flag_pr(pr10_cabg, pr_cabg)
table(core1p5$pr_cabg, useNA = "ifany")
