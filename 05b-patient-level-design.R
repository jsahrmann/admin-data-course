## Header ------------------------------------------------------------
##
## M19 PHS 5254 Using Administrative Data for Health Services Research
## Washington University School of Medicine in St. Louis
##
## Demonstrate typical data management tasks for a patient-level
## analysis.


# Sample problem:
# 1. Identify admissions with a diagnosis of fall.
# 2. Select the first such admission per patient.
# 3. Obtain all hospitalizations for these patients.
# 4. Recreate the data set of index admissions to use for index
#    admission-specific inclusion/exclusion criteria and covariates.
# 5. Create a data set consisting of index admissions and any
#    admissions in the year prior, which will be used for
#    inclusion/exclusion criteria and to define baseline covariates.
# 6. Create a data set of admissions in the three months following
#    index admissions to determine outcomes.


## Setup -------------------------------------------------------------

library(tidyverse)

# setwd("//storage1.ris.wustl.edu/colditzg/Active/admin_course_jsahrmann")

source("../admin_course_data/code2023/coder.R")


## Constant definitions ----------------------------------------------


## Diagnoses ----------------------

# Slipping, tripping, stumbling and falls
# W00--W19
dx10_fall <- c(
  "W000XXA", "W000XXD", "W000XXS", "W001XXA", "W001XXD", "W001XXS",
  "W002XXA", "W002XXD", "W002XXS", "W009XXA", "W009XXD", "W009XXS",
  "W010XXA", "W010XXD", "W010XXS", "W0110XA", "W0110XD", "W0110XS",
  "W01110A", "W01110D", "W01110S", "W01111A", "W01111D", "W01111S",
  "W01118A", "W01118D", "W01118S", "W01119A", "W01119D", "W01119S",
  "W01190A", "W01190D", "W01190S", "W01198A", "W01198D", "W01198S",
  "W03XXXA", "W03XXXD", "W03XXXS", "W04XXXA", "W04XXXD", "W04XXXS",
  "W050XXA", "W050XXD", "W050XXS", "W051XXA", "W051XXD", "W051XXS",
  "W052XXA", "W052XXD", "W052XXS", "W06XXXA", "W06XXXD", "W06XXXS",
  "W07XXXA", "W07XXXD", "W07XXXS", "W08XXXA", "W08XXXD", "W08XXXS",
  "W090XXA", "W090XXD", "W090XXS", "W091XXA", "W091XXD", "W091XXS",
  "W092XXA", "W092XXD", "W092XXS", "W098XXA", "W098XXD", "W098XXS",
  "W100XXA", "W100XXD", "W100XXS", "W101XXA", "W101XXD", "W101XXS",
  "W102XXA", "W102XXD", "W102XXS", "W108XXA", "W108XXD", "W108XXS",
  "W109XXA", "W109XXD", "W109XXS", "W11XXXA", "W11XXXD", "W11XXXS",
  "W12XXXA", "W12XXXD", "W12XXXS", "W130XXA", "W130XXD", "W130XXS",
  "W131XXA", "W131XXD", "W131XXS", "W132XXA", "W132XXD", "W132XXS",
  "W133XXA", "W133XXD", "W133XXS", "W134XXA", "W134XXD", "W134XXS",
  "W138XXA", "W138XXD", "W138XXS", "W139XXA", "W139XXD", "W139XXS",
  "W14XXXA", "W14XXXD", "W14XXXS", "W15XXXA", "W15XXXD", "W15XXXS",
  "W16011A", "W16011D", "W16011S", "W16012A", "W16012D", "W16012S",
  "W16021A", "W16021D", "W16021S", "W16022A", "W16022D", "W16022S",
  "W16031A", "W16031D", "W16031S", "W16032A", "W16032D", "W16032S",
  "W16111A", "W16111D", "W16111S", "W16112A", "W16112D", "W16112S",
  "W16121A", "W16121D", "W16121S", "W16122A", "W16122D", "W16122S",
  "W16131A", "W16131D", "W16131S", "W16132A", "W16132D", "W16132S",
  "W16211A", "W16211D", "W16211S", "W16212A", "W16212D", "W16212S",
  "W16221A", "W16221D", "W16221S", "W16222A", "W16222D", "W16222S",
  "W16311A", "W16311D", "W16311S", "W16312A", "W16312D", "W16312S",
  "W16321A", "W16321D", "W16321S", "W16322A", "W16322D", "W16322S",
  "W16331A", "W16331D", "W16331S", "W16332A", "W16332D", "W16332S",
  "W1641XA", "W1641XD", "W1641XS", "W1642XA", "W1642XD", "W1642XS",
  "W16511A", "W16511D", "W16511S", "W16512A", "W16512D", "W16512S",
  "W16521A", "W16521D", "W16521S", "W16522A", "W16522D", "W16522S",
  "W16531A", "W16531D", "W16531S", "W16532A", "W16532D", "W16532S",
  "W16611A", "W16611D", "W16611S", "W16612A", "W16612D", "W16612S",
  "W16621A", "W16621D", "W16621S", "W16622A", "W16622D", "W16622S",
  "W16711A", "W16711D", "W16711S", "W16712A", "W16712D", "W16712S",
  "W16721A", "W16721D", "W16721S", "W16722A", "W16722D", "W16722S",
  "W16811A", "W16811D", "W16811S", "W16812A", "W16812D", "W16812S",
  "W16821A", "W16821D", "W16821S", "W16822A", "W16822D", "W16822S",
  "W16831A", "W16831D", "W16831S", "W16832A", "W16832D", "W16832S",
  "W1691XA", "W1691XD", "W1691XS", "W1692XA", "W1692XD", "W1692XS",
  "W170XXA", "W170XXD", "W170XXS", "W171XXA", "W171XXD", "W171XXS",
  "W172XXA", "W172XXD", "W172XXS", "W173XXA", "W173XXD", "W173XXS",
  "W174XXA", "W174XXD", "W174XXS", "W1781XA", "W1781XD", "W1781XS",
  "W1782XA", "W1782XD", "W1782XS", "W1789XA", "W1789XD", "W1789XS",
  "W1800XA", "W1800XD", "W1800XS", "W1801XA", "W1801XD", "W1801XS",
  "W1802XA", "W1802XD", "W1802XS", "W1809XA", "W1809XD", "W1809XS",
  "W1811XA", "W1811XD", "W1811XS", "W1812XA", "W1812XD", "W1812XS",
  "W182XXA", "W182XXD", "W182XXS", "W1830XA", "W1830XD", "W1830XS",
  "W1831XA", "W1831XD", "W1831XS", "W1839XA", "W1839XD", "W1839XS",
  "W1840XA", "W1840XD", "W1840XS", "W1841XA", "W1841XD", "W1841XS",
  "W1842XA", "W1842XD", "W1842XS", "W1843XA", "W1843XD", "W1843XS",
  "W1849XA", "W1849XD", "W1849XS", "W19XXXA", "W19XXXD", "W19XXXS"
)


## Function definitions ----------------------------------------------

# Print `TRUE` if the given data set is patient-level (i.e., one in
# which the number of `VisitLink`s is equal to the number of records).
is_patient_level <- function(.data) {
  n_distinct(.data$VisitLink) == nrow(.data)
}


## Input data --------------------------------------------------------

# Read the full SID data. (The full SID contains a moderate number of
# records where `VisitLink` and `DaysToEvent` are missing. These are
# key to a patient-level analysis, so we can discard records where
# they're missing at the outset.)
core <- bind_rows(
  read_rds("../admin_course_data/fl_sidc_2015q4_core.rds"),
  read_rds("../admin_course_data/fl_sidc_2016_core.rds"),
  read_rds("../admin_course_data/fl_sidc_2017_core.rds"),
  read_rds("../admin_course_data/fl_sidc_2018_core.rds"),
  read_rds("../admin_course_data/fl_sidc_2019_core.rds")
) %>%
  filter(!is.na(VisitLink))
# 646 s


## 1. Primary inclusion criterion ------------------------------------

# Identify admissions with a diagnosis of fall.
core_fall <- core %>%
  mutate_flag_dx(codes = dx10_fall, name = dx_fall) %>%
  filter(dx_fall == 1)

is_patient_level(core_fall)

# (Note that we cannot remove `core` at this point because we'll need
# it for the 'all records' data in step 3.)

# Record the initial sample size (in number of admissions).
nrow(core_fall)                    # 572477


## 2. Earliest admissions per patient --------------------------------

# Use `group_by`/`summarise` to create a data set containing each
# patient and the 'date' of his/her earliest admission with a
# fall. (Date is in quotes because we're using `DaysToEvent` rather
# than a true date.)
pat_earliest_fall <- core_fall %>%
  group_by(VisitLink) %>%
  summarise(earliest_fall = min(DaysToEvent))

pat_earliest_fall
nrow(pat_earliest_fall)            # 458647
is_patient_level(pat_earliest_fall)


## 3. 'All records' data set -----------------------------------------

# Merge our patient-level data set to the full SID data to obtain all
# hospitalizations for these patients.
all_records <- pat_earliest_fall %>%
  inner_join(core, by = "VisitLink")

nrow(all_records)                  # 1729605
is_patient_level(all_records)

rm(core)


## 4. Index records data set -----------------------------------------

# (Attempt to) recreate the patient-level data set of earliest
# falls. In creating the all records data set, we kept the variable
# `earliest_fall`, so we can compare that to `DaysToEvent` to get
# these records.
not_quite_pat_level_earliest_fall <- all_records %>%
  filter(DaysToEvent == earliest_fall)

nrow(not_quite_pat_level_earliest_fall)  # 459693
is_patient_level(not_quite_pat_level_earliest_fall)
nrow(not_quite_pat_level_earliest_fall) -
  n_distinct(not_quite_pat_level_earliest_fall$VisitLink)
# This data set isn't patient-level! That means that there are a small
# number of records with identical `VisitLink`s and
# `DaysToEvent`s. These could be mistakes, but we can't be
# sure. Dealing with this would require further investigation and is
# beyond the scope of this demo. These decisions often rely on
# discussion between programmers and clinical investigators.


## 5. Baseline records data set --------------------------------------

# Create a data set of index admissions and any admissions in the year
# prior.
dsch_time_to_index <- all_records %>%
  mutate(time_to_index = DaysToEvent - earliest_fall)

summary(dsch_time_to_index$time_to_index)
# `time_to_index` < 0   => before index admission date
# `time_to_index` >= 0  => after index admission date

dsch_baseline <- dsch_time_to_index %>%
  filter(between(time_to_index, -365, 0))

nrow(dsch_baseline)                # 753742
is_patient_level(dsch_baseline)


## 6. Follow-up records data set -------------------------------------

# Create a data set of admissions in the three months following index
# admissions.
dsch_follow_up <- dsch_time_to_index %>%
  filter(between(time_to_index, 1, 90))

nrow(dsch_follow_up)
is_patient_level(dsch_follow_up)


## Bonus: Producing an analytic data set -----------------------------

# Define dummy indicator variables for each of the data sets produced
# in parts 4--6.


## Index records data set ---------

not_quite_pat_level_earliest_fall2 <- not_quite_pat_level_earliest_fall %>%
  mutate(index_record = ifelse(DaysToEvent == earliest_fall, 1, 0))

pat_index <- not_quite_pat_level_earliest_fall2 %>%
  group_by(VisitLink) %>%
  summarise(has_index_record = max(index_record))


## Baseline records data set ------

dsch_baseline2 <- dsch_baseline %>%
  mutate(pre_index_record = ifelse(DaysToEvent < earliest_fall, 1, 0))

pat_baseline <- dsch_baseline2 %>%
  group_by(VisitLink) %>%
  summarise(has_pre_index_record = max(pre_index_record))


## Follow-up records data set -----

dsch_follow_up2 <- dsch_follow_up %>%
  mutate(post_index_record = ifelse(DaysToEvent > earliest_fall, 1, 0))

pat_follow_up <- dsch_follow_up2 %>%
  group_by(VisitLink) %>%
  summarise(has_post_index_record = max(post_index_record))


## Analytic data set --------------

# Merge the patient-level data sets to produce a single data set with
# variables from index, baseline, and follow-up periods.
pat_analytic <- pat_index %>%
  left_join(pat_baseline, by = "VisitLink") %>%
  left_join(pat_follow_up, by = "VisitLink")

pat_analytic
nrow(pat_analytic)
is_patient_level(pat_analytic)
