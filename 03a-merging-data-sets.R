## Header ------------------------------------------------------------
##
## M19 PHS 5254 Using Administrative Data for Health Services Research
## Washington University School of Medicine in St. Louis
##
## Demonstrate data set merging using dplyr.


## Setup -------------------------------------------------------------

library(tidyverse)

# setwd("//storage1.ris.wustl.edu/colditzg/Active/admin_course_jsahrmann")


## Input data --------------------------------------------------------

# Create data sets for the examples below.
payer_info <- tribble(
  ~VisitLink, ~PAY1,
  46571, 1,
  93576, 3,
  69250, 3
)
charge_info <- tribble(
  ~VisitLink, ~TOTCHG,
  46571, 28004,
  93576, 7140,
  73038, 82450
)
payer_info2 <- tribble(
  ~VisitLink, ~PAY1, ~AGE,
  46571, 1, 25,
  93576, 3, 31,
  69250, 3, 44
)
charge_info2 <- tribble(
  ~VisitLink, ~TOTCHG, ~AGE,
  46571, 28004, 25,
  93576, 7140, 31,
  73038, 82450, 76
)
payer_info
charge_info


## Examples ----------------------------------------------------------

## Inner Join ---------------------

payer_and_charge_info <- inner_join(
  payer_info, charge_info, by = "VisitLink")
payer_and_charge_info

# Or using `%>%`,

payer_and_charge_info <- payer_info %>%
  inner_join(charge_info, by = "VisitLink")
payer_and_charge_info


## Left Join ----------------------

payer_and_maybe_charge_info <- payer_info %>%
  left_join(charge_info, by = "VisitLink")
payer_and_maybe_charge_info

# The equivalent right join would be

payer_and_maybe_charge_info <- charge_info %>%
  right_join(payer_info, by = "VisitLink")
payer_and_maybe_charge_info


## Full join ----------------------

payer_or_charge_info <- payer_info %>%
  full_join(charge_info, by = "VisitLink")
payer_or_charge_info


## Anti Join ----------------------

missing_payer_info <- payer_or_charge_info %>%
  filter(is.na(PAY1))
missing_payer_info

charge_info_but_only_if_payer_info <- charge_info %>%
  anti_join(missing_payer_info, by = "VisitLink")
charge_info_but_only_if_payer_info


## Merging tip -------------------------------------------------------

payer_info2
charge_info2

# Make sure the only shared variables are the key variable(s).

payer_and_charge_info_with_duplicate_age <- payer_info2 %>%
  inner_join(charge_info2, by = "VisitLink")
payer_and_charge_info_with_duplicate_age

# Solution 1: Remove shared variables from one data set

payer_and_charge_info_without_duplicate_age <- payer_info2 %>%
  select(!AGE) %>%
  inner_join(charge_info2, by = "VisitLink")
payer_and_charge_info_without_duplicate_age

# Solution 2: Add shared variables as another key

payer_and_charge_info_without_duplicate_age <- payer_info2 %>%
  inner_join(charge_info2, by = c("VisitLink", "AGE"))
payer_and_charge_info_without_duplicate_age
