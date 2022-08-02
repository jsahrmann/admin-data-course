## Header ------------------------------------------------------------
##
## M19 PHS 5254 Using Administrative Data for Health Services Research
## Washington University School of Medicine in St. Louis
##
## Define functions for working with diagnosis and procedure code
## columns in the HCUP SID.


## Setup -------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))


## Function definitions ----------------------------------------------

# Based on the slowness and inefficiency of the
# `rowwise`/`c_across`-based functions used in the Spring 2020
# semester, these functions use a vectorized column-wise approach
# based on `case_when`. The downside is that the functions can't take
# a generic 'row of codes' as input, so we have to hard code the
# column names instead. (An alternative approach based on `apply` or a
# purrr equivalent is possible but would require a similar degree of
# hard coding, especially for a function that checks multiple columns
# like `I10_DX`n and `DXPOA`n.)

# To (hopefully) make these functions more intuitive to use, they have
# been named with 'mutate_' as a prefix to indicate that they should
# be used as standalone steps in a chain of function calls similar to
# the usual `select, `mutate`, etc.

#' Produce the input data set with a new column containing an
#' indicator variable for whether any of a specified set of codes has
#' an exact match among the `I10_DX`n (including `I10_DX_Admitting`)
#' columns.
#'
#' @param .data Input data set, as in `dplyr::mutate`
#' @param codes Character vector of diagnosis codes for which an
#'   *exact* match is to be checked
#' @param name Name of column containing the new variable (can be a
#'   name or a string)
#' @return The input data set with the additional column
#' @examples
#' ## 1
#' my_data <- my_data %>% mutate_flag_dx("R05", dx_cough)
#' ## 2
#' my_data <- my_data %>% mutate_flag_dx("R05", "dx_cough")
#' ## 3
#' dx10_acutemi <- c(
#'   "I2101", "I2102", "I2109", "I2111", "I2119", "I2121", "I2129",
#'   "I213", "I214", "I220", "I221", "I222", "I228", "I229")
#' my_data <- mutate_flag_dx(my_data, dx10_acutemi, dx_acutemi)
mutate_flag_dx <- function(.data, codes, name) {
  .data %>%
    mutate(
      {{name}} := case_when(
        I10_DX_Admitting %in% codes ~ 1,
        I10_DX1 %in% codes ~ 1,
        I10_DX2 %in% codes ~ 1,
        I10_DX3 %in% codes ~ 1,
        I10_DX4 %in% codes ~ 1,
        I10_DX5 %in% codes ~ 1,
        I10_DX6 %in% codes ~ 1,
        I10_DX7 %in% codes ~ 1,
        I10_DX8 %in% codes ~ 1,
        I10_DX9 %in% codes ~ 1,
        I10_DX10 %in% codes ~ 1,
        I10_DX11 %in% codes ~ 1,
        I10_DX12 %in% codes ~ 1,
        I10_DX13 %in% codes ~ 1,
        I10_DX14 %in% codes ~ 1,
        I10_DX15 %in% codes ~ 1,
        I10_DX16 %in% codes ~ 1,
        I10_DX17 %in% codes ~ 1,
        I10_DX18 %in% codes ~ 1,
        I10_DX19 %in% codes ~ 1,
        I10_DX20 %in% codes ~ 1,
        I10_DX21 %in% codes ~ 1,
        I10_DX22 %in% codes ~ 1,
        I10_DX23 %in% codes ~ 1,
        I10_DX24 %in% codes ~ 1,
        I10_DX25 %in% codes ~ 1,
        I10_DX26 %in% codes ~ 1,
        I10_DX27 %in% codes ~ 1,
        I10_DX28 %in% codes ~ 1,
        I10_DX29 %in% codes ~ 1,
        I10_DX30 %in% codes ~ 1,
        I10_DX31 %in% codes ~ 1,
        I10_DX32 %in% codes ~ 1,
        I10_DX33 %in% codes ~ 1,
        I10_DX34 %in% codes ~ 1,
        I10_ECAUSE1 %in% codes ~ 1,
        I10_ECAUSE2 %in% codes ~ 1,
        I10_ECAUSE3 %in% codes ~ 1,
        I10_ECAUSE4 %in% codes ~ 1,
        I10_ECAUSE5 %in% codes ~ 1,
        I10_ECAUSE6 %in% codes ~ 1,
        TRUE ~ 0
      )
    )
}

#' Produce the input data set with a new column containing an
#' indicator variable for whether any of a specified set of codes has
#' an exact match among the `I10_DX`n columns while also *not having*
#' `DXPOA`n indicating the diagnosis was present on
#' admission. (`I10_DX_Admitting` is not checked, as it is present on
#' admission be definition.)
#'
#' @param .data Input data set, as in `dplyr::mutate`
#' @param codes Character vector of diagnosis codes for which an
#'   *exact* match is to be checked
#' @param name Name of column containing the new variable (can be a
#'   name or a string)
#' @return The input data set with the additional column
#' @examples
#' ## 1
#' my_data <- my_data %>% mutate_flag_dx_not_POA("R05", dx_cough)
#' ## 2
#' my_data <- my_data %>% mutate_flag_dx_not_POA("R05", "dx_cough")
#' ## 3
#' dx10_acutemi <- c(
#'   "I2101", "I2102", "I2109", "I2111", "I2119", "I2121", "I2129",
#'   "I213", "I214", "I220", "I221", "I222", "I228", "I229")
#' my_data <- mutate_flag_dx_not_POA(
#'   my_data, dx10_acutemi, dx_acutemi)
mutate_flag_dx_not_POA <- function(.data, codes, name) {
  .data %>%
    mutate(
      {{name}} := case_when(
        I10_DX1 %in% codes & DXPOA1 != "Y" ~ 1,
        I10_DX2 %in% codes & DXPOA2 != "Y" ~ 1,
        I10_DX3 %in% codes & DXPOA3 != "Y" ~ 1,
        I10_DX4 %in% codes & DXPOA4 != "Y" ~ 1,
        I10_DX5 %in% codes & DXPOA5 != "Y" ~ 1,
        I10_DX6 %in% codes & DXPOA6 != "Y" ~ 1,
        I10_DX7 %in% codes & DXPOA7 != "Y" ~ 1,
        I10_DX8 %in% codes & DXPOA8 != "Y" ~ 1,
        I10_DX9 %in% codes & DXPOA9 != "Y" ~ 1,
        I10_DX10 %in% codes & DXPOA10 != "Y" ~ 1,
        I10_DX11 %in% codes & DXPOA11 != "Y" ~ 1,
        I10_DX12 %in% codes & DXPOA12 != "Y" ~ 1,
        I10_DX13 %in% codes & DXPOA13 != "Y" ~ 1,
        I10_DX14 %in% codes & DXPOA14 != "Y" ~ 1,
        I10_DX15 %in% codes & DXPOA15 != "Y" ~ 1,
        I10_DX16 %in% codes & DXPOA16 != "Y" ~ 1,
        I10_DX17 %in% codes & DXPOA17 != "Y" ~ 1,
        I10_DX18 %in% codes & DXPOA18 != "Y" ~ 1,
        I10_DX19 %in% codes & DXPOA19 != "Y" ~ 1,
        I10_DX20 %in% codes & DXPOA20 != "Y" ~ 1,
        I10_DX21 %in% codes & DXPOA21 != "Y" ~ 1,
        I10_DX22 %in% codes & DXPOA22 != "Y" ~ 1,
        I10_DX23 %in% codes & DXPOA23 != "Y" ~ 1,
        I10_DX24 %in% codes & DXPOA24 != "Y" ~ 1,
        I10_DX25 %in% codes & DXPOA25 != "Y" ~ 1,
        I10_DX26 %in% codes & DXPOA26 != "Y" ~ 1,
        I10_DX27 %in% codes & DXPOA27 != "Y" ~ 1,
        I10_DX28 %in% codes & DXPOA28 != "Y" ~ 1,
        I10_DX29 %in% codes & DXPOA29 != "Y" ~ 1,
        I10_DX30 %in% codes & DXPOA30 != "Y" ~ 1,
        I10_DX31 %in% codes & DXPOA31 != "Y" ~ 1,
        I10_DX32 %in% codes & DXPOA32 != "Y" ~ 1,
        I10_DX33 %in% codes & DXPOA33 != "Y" ~ 1,
        I10_DX34 %in% codes & DXPOA34 != "Y" ~ 1,
        I10_ECAUSE1 %in% codes & E_POA1 != "Y" ~ 1,
        I10_ECAUSE2 %in% codes & E_POA2 != "Y" ~ 1,
        I10_ECAUSE3 %in% codes & E_POA3 != "Y" ~ 1,
        I10_ECAUSE4 %in% codes & E_POA4 != "Y" ~ 1,
        I10_ECAUSE5 %in% codes & E_POA5 != "Y" ~ 1,
        I10_ECAUSE6 %in% codes & E_POA6 != "Y" ~ 1,
        TRUE ~ 0
      )
    )
}

#' Produce the input data set with a new column containing an
#' indicator variable for whether any of a specified set of codes has
#' an exact match among the `I10_DX`n (including `I10_DX_Admitting`)
#' columns while also *having* `DXPOA`n indicating the diagnosis was
#' present on admission (or that the diagnosis is exempt from
#' POA_reporting).
#'
#' @param .data Input data set, as in `dplyr::mutate`
#' @param codes Character vector of diagnosis codes for which an
#'   *exact* match is to be checked
#' @param name Name of column containing the new variable (can be a
#'   name or a string)
#' @return The input data set with the additional column
#' @examples
#' ## 1
#' my_data <- my_data %>% mutate_flag_dx_POA("R05", dx_cough)
#' ## 2
#' my_data <- my_data %>% mutate_flag_dx_POA("R05", "dx_cough")
#' ## 3
#' dx10_acutemi <- c(
#'   "I2101", "I2102", "I2109", "I2111", "I2119", "I2121", "I2129",
#'   "I213", "I214", "I220", "I221", "I222", "I228", "I229")
#' my_data <- mutate_flag_dx_POA(my_data, dx10_acutemi, dx_acutemi)
mutate_flag_dx_POA <- function(.data, codes, name) {
  .data %>%
    mutate(
      {{name}} := case_when(
        I10_DX_Admitting %in% codes ~ 1,
        I10_DX1 %in% codes & DXPOA1 %in% c("1", "E", "Y") ~ 1,
        I10_DX2 %in% codes & DXPOA2 %in% c("1", "E", "Y") ~ 1,
        I10_DX3 %in% codes & DXPOA3 %in% c("1", "E", "Y") ~ 1,
        I10_DX4 %in% codes & DXPOA4 %in% c("1", "E", "Y") ~ 1,
        I10_DX5 %in% codes & DXPOA5 %in% c("1", "E", "Y") ~ 1,
        I10_DX6 %in% codes & DXPOA6 %in% c("1", "E", "Y") ~ 1,
        I10_DX7 %in% codes & DXPOA7 %in% c("1", "E", "Y") ~ 1,
        I10_DX8 %in% codes & DXPOA8 %in% c("1", "E", "Y") ~ 1,
        I10_DX9 %in% codes & DXPOA9 %in% c("1", "E", "Y") ~ 1,
        I10_DX10 %in% codes & DXPOA10 %in% c("1", "E", "Y") ~ 1,
        I10_DX11 %in% codes & DXPOA11 %in% c("1", "E", "Y") ~ 1,
        I10_DX12 %in% codes & DXPOA12 %in% c("1", "E", "Y") ~ 1,
        I10_DX13 %in% codes & DXPOA13 %in% c("1", "E", "Y") ~ 1,
        I10_DX14 %in% codes & DXPOA14 %in% c("1", "E", "Y") ~ 1,
        I10_DX15 %in% codes & DXPOA15 %in% c("1", "E", "Y") ~ 1,
        I10_DX16 %in% codes & DXPOA16 %in% c("1", "E", "Y") ~ 1,
        I10_DX17 %in% codes & DXPOA17 %in% c("1", "E", "Y") ~ 1,
        I10_DX18 %in% codes & DXPOA18 %in% c("1", "E", "Y") ~ 1,
        I10_DX19 %in% codes & DXPOA19 %in% c("1", "E", "Y") ~ 1,
        I10_DX20 %in% codes & DXPOA20 %in% c("1", "E", "Y") ~ 1,
        I10_DX21 %in% codes & DXPOA21 %in% c("1", "E", "Y") ~ 1,
        I10_DX22 %in% codes & DXPOA22 %in% c("1", "E", "Y") ~ 1,
        I10_DX23 %in% codes & DXPOA23 %in% c("1", "E", "Y") ~ 1,
        I10_DX24 %in% codes & DXPOA24 %in% c("1", "E", "Y") ~ 1,
        I10_DX25 %in% codes & DXPOA25 %in% c("1", "E", "Y") ~ 1,
        I10_DX26 %in% codes & DXPOA26 %in% c("1", "E", "Y") ~ 1,
        I10_DX27 %in% codes & DXPOA27 %in% c("1", "E", "Y") ~ 1,
        I10_DX28 %in% codes & DXPOA28 %in% c("1", "E", "Y") ~ 1,
        I10_DX29 %in% codes & DXPOA29 %in% c("1", "E", "Y") ~ 1,
        I10_DX30 %in% codes & DXPOA30 %in% c("1", "E", "Y") ~ 1,
        I10_DX31 %in% codes & DXPOA31 %in% c("1", "E", "Y") ~ 1,
        I10_DX32 %in% codes & DXPOA32 %in% c("1", "E", "Y") ~ 1,
        I10_DX33 %in% codes & DXPOA33 %in% c("1", "E", "Y") ~ 1,
        I10_DX34 %in% codes & DXPOA34 %in% c("1", "E", "Y") ~ 1,
        I10_ECAUSE1 %in% codes & E_POA1 %in% c("1", "E", "Y") ~ 1,
        I10_ECAUSE2 %in% codes & E_POA2 %in% c("1", "E", "Y") ~ 1,
        I10_ECAUSE3 %in% codes & E_POA3 %in% c("1", "E", "Y") ~ 1,
        I10_ECAUSE4 %in% codes & E_POA4 %in% c("1", "E", "Y") ~ 1,
        I10_ECAUSE5 %in% codes & E_POA5 %in% c("1", "E", "Y") ~ 1,
        I10_ECAUSE6 %in% codes & E_POA6 %in% c("1", "E", "Y") ~ 1,
        TRUE ~ 0
      )
    )
}

#' Produce the input data set with a new column containing a 'date'
#' variable for whether any of a specified set of codes has an exact
#' match among the `I10_DX`n (including `I10_DX_Admitting`) columns,
#' with 'date' meaning something similar to `DaysToEvent`. Diagnoses
#' in `I10_DX_Admitting` and `I10_DX1` are set to `DaysToEvent`;
#' diagnoses in other columns are set to the discharge date (default)
#' or the midpoint of the admission based on the value of the `date_2`
#' parameter.
#'
#' @param .data Input data set, as in `dplyr::mutate`
#' @param codes Character vector of diagnosis codes for which an
#'   *exact* match is to be checked
#' @param name Name of column containing the new variable (can be a
#'   name or a string)
#' @param date_2 One of c("discharge", "midpoint") specifying whether
#'   diagnoses in `I10_DX`2+ should be assigned to discharge or the
#'   midpoint of the admission
#' @return The input data set with the additional column
#' @examples
#' ## 1
#' my_data <- my_data %>% mutate_date_dx("R05", date_cough)
#' ## 2
#' my_data <- my_data %>% mutate_date_dx("R05", "date_cough")
#' ## 3
#' dx10_acutemi <- c(
#'   "I2101", "I2102", "I2109", "I2111", "I2119", "I2121", "I2129",
#'   "I213", "I214", "I220", "I221", "I222", "I228", "I229")
#' my_data <- mutate_date_dx(my_data, dx10_acutemi, date_acutemi)
mutate_date_dx <- function(.data, codes, name, date_2 = "discharge") {
  if (date_2 == "discharge") {
    .data %>%
      mutate(
        {{name}} := case_when(
          I10_DX_Admitting %in% codes ~ DaysToEvent,
          I10_DX1 %in% codes ~ DaysToEvent,
          I10_DX2 %in% codes ~ DaysToEvent + LOS,
          I10_DX3 %in% codes ~ DaysToEvent + LOS,
          I10_DX4 %in% codes ~ DaysToEvent + LOS,
          I10_DX5 %in% codes ~ DaysToEvent + LOS,
          I10_DX6 %in% codes ~ DaysToEvent + LOS,
          I10_DX7 %in% codes ~ DaysToEvent + LOS,
          I10_DX8 %in% codes ~ DaysToEvent + LOS,
          I10_DX9 %in% codes ~ DaysToEvent + LOS,
          I10_DX10 %in% codes ~ DaysToEvent + LOS,
          I10_DX11 %in% codes ~ DaysToEvent + LOS,
          I10_DX12 %in% codes ~ DaysToEvent + LOS,
          I10_DX13 %in% codes ~ DaysToEvent + LOS,
          I10_DX14 %in% codes ~ DaysToEvent + LOS,
          I10_DX15 %in% codes ~ DaysToEvent + LOS,
          I10_DX16 %in% codes ~ DaysToEvent + LOS,
          I10_DX17 %in% codes ~ DaysToEvent + LOS,
          I10_DX18 %in% codes ~ DaysToEvent + LOS,
          I10_DX19 %in% codes ~ DaysToEvent + LOS,
          I10_DX20 %in% codes ~ DaysToEvent + LOS,
          I10_DX21 %in% codes ~ DaysToEvent + LOS,
          I10_DX22 %in% codes ~ DaysToEvent + LOS,
          I10_DX23 %in% codes ~ DaysToEvent + LOS,
          I10_DX24 %in% codes ~ DaysToEvent + LOS,
          I10_DX25 %in% codes ~ DaysToEvent + LOS,
          I10_DX26 %in% codes ~ DaysToEvent + LOS,
          I10_DX27 %in% codes ~ DaysToEvent + LOS,
          I10_DX28 %in% codes ~ DaysToEvent + LOS,
          I10_DX29 %in% codes ~ DaysToEvent + LOS,
          I10_DX30 %in% codes ~ DaysToEvent + LOS,
          I10_DX31 %in% codes ~ DaysToEvent + LOS,
          I10_DX32 %in% codes ~ DaysToEvent + LOS,
          I10_DX33 %in% codes ~ DaysToEvent + LOS,
          I10_DX34 %in% codes ~ DaysToEvent + LOS,
          I10_ECAUSE1 %in% codes ~ DaysToEvent + LOS,
          I10_ECAUSE2 %in% codes ~ DaysToEvent + LOS,
          I10_ECAUSE3 %in% codes ~ DaysToEvent + LOS,
          I10_ECAUSE4 %in% codes ~ DaysToEvent + LOS,
          I10_ECAUSE5 %in% codes ~ DaysToEvent + LOS,
          I10_ECAUSE6 %in% codes ~ DaysToEvent + LOS,
          TRUE ~ NA_real_
        )
      )
  } else if (date_2 == "midpoint") {
    .data %>%
      mutate(
        {{name}} := case_when(
          I10_DX_Admitting %in% codes ~ DaysToEvent,
          I10_DX1 %in% codes ~ DaysToEvent,
          I10_DX2 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX3 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX4 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX5 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX6 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX7 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX8 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX9 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX10 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX11 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX12 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX13 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX14 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX15 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX16 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX17 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX18 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX19 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX20 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX21 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX22 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX23 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX24 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX25 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX26 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX27 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX28 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX29 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX30 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX31 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX32 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX33 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_DX34 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_ECAUSE1 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_ECAUSE2 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_ECAUSE3 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_ECAUSE4 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_ECAUSE5 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          I10_ECAUSE6 %in% codes ~ ceiling(DaysToEvent + LOS/2),
          TRUE ~ NA_real_
        )
      )
  }
}

mutate_date_dx_not_POA <- function(
  .data, codes, name, date_2 = "discharge"
) {
  if (date_2 == "discharge") {
    .data %>%
      mutate(
        {{name}} := case_when(
          I10_DX1 %in% codes & DXPOA1 != "Y" ~ DaysToEvent,
          I10_DX2 %in% codes & DXPOA2 != "Y" ~ DaysToEvent + LOS,
          I10_DX3 %in% codes & DXPOA3 != "Y" ~ DaysToEvent + LOS,
          I10_DX4 %in% codes & DXPOA4 != "Y" ~ DaysToEvent + LOS,
          I10_DX5 %in% codes & DXPOA5 != "Y" ~ DaysToEvent + LOS,
          I10_DX6 %in% codes & DXPOA6 != "Y" ~ DaysToEvent + LOS,
          I10_DX7 %in% codes & DXPOA7 != "Y" ~ DaysToEvent + LOS,
          I10_DX8 %in% codes & DXPOA8 != "Y" ~ DaysToEvent + LOS,
          I10_DX9 %in% codes & DXPOA9 != "Y" ~ DaysToEvent + LOS,
          I10_DX10 %in% codes & DXPOA10 != "Y" ~ DaysToEvent + LOS,
          I10_DX11 %in% codes & DXPOA11 != "Y" ~ DaysToEvent + LOS,
          I10_DX12 %in% codes & DXPOA12 != "Y" ~ DaysToEvent + LOS,
          I10_DX13 %in% codes & DXPOA13 != "Y" ~ DaysToEvent + LOS,
          I10_DX14 %in% codes & DXPOA14 != "Y" ~ DaysToEvent + LOS,
          I10_DX15 %in% codes & DXPOA15 != "Y" ~ DaysToEvent + LOS,
          I10_DX16 %in% codes & DXPOA16 != "Y" ~ DaysToEvent + LOS,
          I10_DX17 %in% codes & DXPOA17 != "Y" ~ DaysToEvent + LOS,
          I10_DX18 %in% codes & DXPOA18 != "Y" ~ DaysToEvent + LOS,
          I10_DX19 %in% codes & DXPOA19 != "Y" ~ DaysToEvent + LOS,
          I10_DX20 %in% codes & DXPOA20 != "Y" ~ DaysToEvent + LOS,
          I10_DX21 %in% codes & DXPOA21 != "Y" ~ DaysToEvent + LOS,
          I10_DX22 %in% codes & DXPOA22 != "Y" ~ DaysToEvent + LOS,
          I10_DX23 %in% codes & DXPOA23 != "Y" ~ DaysToEvent + LOS,
          I10_DX24 %in% codes & DXPOA24 != "Y" ~ DaysToEvent + LOS,
          I10_DX25 %in% codes & DXPOA25 != "Y" ~ DaysToEvent + LOS,
          I10_DX26 %in% codes & DXPOA26 != "Y" ~ DaysToEvent + LOS,
          I10_DX27 %in% codes & DXPOA27 != "Y" ~ DaysToEvent + LOS,
          I10_DX28 %in% codes & DXPOA28 != "Y" ~ DaysToEvent + LOS,
          I10_DX29 %in% codes & DXPOA29 != "Y" ~ DaysToEvent + LOS,
          I10_DX30 %in% codes & DXPOA30 != "Y" ~ DaysToEvent + LOS,
          I10_DX31 %in% codes & DXPOA31 != "Y" ~ DaysToEvent + LOS,
          I10_DX32 %in% codes & DXPOA32 != "Y" ~ DaysToEvent + LOS,
          I10_DX33 %in% codes & DXPOA33 != "Y" ~ DaysToEvent + LOS,
          I10_DX34 %in% codes & DXPOA34 != "Y" ~ DaysToEvent + LOS,
          I10_ECAUSE1 %in% codes & E_POA1 != "Y" ~ DaysToEvent + LOS,
          I10_ECAUSE2 %in% codes & E_POA2 != "Y" ~ DaysToEvent + LOS,
          I10_ECAUSE3 %in% codes & E_POA3 != "Y" ~ DaysToEvent + LOS,
          I10_ECAUSE4 %in% codes & E_POA4 != "Y" ~ DaysToEvent + LOS,
          I10_ECAUSE5 %in% codes & E_POA5 != "Y" ~ DaysToEvent + LOS,
          I10_ECAUSE6 %in% codes & E_POA6 != "Y" ~ DaysToEvent + LOS,
          TRUE ~ NA_real_
        )
      )
  } else if (date_2 == "midpoint") {
    .data %>%
      mutate(
        {{name}} := case_when(
          I10_DX1 %in% codes & DXPOA1 != "Y" ~
            DaysToEvent,
          I10_DX2 %in% codes & DXPOA2 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX3 %in% codes & DXPOA3 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX4 %in% codes & DXPOA4 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX5 %in% codes & DXPOA5 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX6 %in% codes & DXPOA6 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX7 %in% codes & DXPOA7 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX8 %in% codes & DXPOA8 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX9 %in% codes & DXPOA9 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX10 %in% codes & DXPOA10 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX11 %in% codes & DXPOA11 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX12 %in% codes & DXPOA12 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX13 %in% codes & DXPOA13 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX14 %in% codes & DXPOA14 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX15 %in% codes & DXPOA15 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX16 %in% codes & DXPOA16 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX17 %in% codes & DXPOA17 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX18 %in% codes & DXPOA18 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX19 %in% codes & DXPOA19 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX20 %in% codes & DXPOA20 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX21 %in% codes & DXPOA21 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX22 %in% codes & DXPOA22 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX23 %in% codes & DXPOA23 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX24 %in% codes & DXPOA24 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX25 %in% codes & DXPOA25 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX26 %in% codes & DXPOA26 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX27 %in% codes & DXPOA27 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX28 %in% codes & DXPOA28 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX29 %in% codes & DXPOA29 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX30 %in% codes & DXPOA30 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX31 %in% codes & DXPOA31 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX32 %in% codes & DXPOA32 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX33 %in% codes & DXPOA33 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX34 %in% codes & DXPOA34 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_ECAUSE1 %in% codes & E_POA1 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_ECAUSE2 %in% codes & E_POA2 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_ECAUSE3 %in% codes & E_POA3 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_ECAUSE4 %in% codes & E_POA4 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_ECAUSE5 %in% codes & E_POA5 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          I10_ECAUSE6 %in% codes & E_POA6 != "Y" ~
            ceiling(DaysToEvent + LOS/2),
          TRUE ~ NA_real_
        )
      )
  }
}

mutate_date_dx_POA <- function(.data, codes, name, date_2 = "discharge") {
  if (date_2 == "discharge") {
    .data %>%
      mutate(
        {{name}} := case_when(
          I10_DX_Admitting %in% codes ~
            DaysToEvent,
          I10_DX1 %in% codes & DXPOA1 %in% c("1", "E", "Y") ~
            DaysToEvent,
          I10_DX2 %in% codes & DXPOA2 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX3 %in% codes & DXPOA3 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX4 %in% codes & DXPOA4 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX5 %in% codes & DXPOA5 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX6 %in% codes & DXPOA6 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX7 %in% codes & DXPOA7 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX8 %in% codes & DXPOA8 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX9 %in% codes & DXPOA9 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX10 %in% codes & DXPOA10 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX11 %in% codes & DXPOA11 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX12 %in% codes & DXPOA12 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX13 %in% codes & DXPOA13 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX14 %in% codes & DXPOA14 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX15 %in% codes & DXPOA15 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX16 %in% codes & DXPOA16 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX17 %in% codes & DXPOA17 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX18 %in% codes & DXPOA18 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX19 %in% codes & DXPOA19 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX20 %in% codes & DXPOA20 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX21 %in% codes & DXPOA21 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX22 %in% codes & DXPOA22 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX23 %in% codes & DXPOA23 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX24 %in% codes & DXPOA24 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX25 %in% codes & DXPOA25 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX26 %in% codes & DXPOA26 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX27 %in% codes & DXPOA27 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX28 %in% codes & DXPOA28 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX29 %in% codes & DXPOA29 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX30 %in% codes & DXPOA30 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX31 %in% codes & DXPOA31 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX32 %in% codes & DXPOA32 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX33 %in% codes & DXPOA33 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_DX34 %in% codes & DXPOA34 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_ECAUSE1 %in% codes & E_POA1 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_ECAUSE2 %in% codes & E_POA2 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_ECAUSE3 %in% codes & E_POA3 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_ECAUSE4 %in% codes & E_POA4 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_ECAUSE5 %in% codes & E_POA5 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          I10_ECAUSE6 %in% codes & E_POA6 %in% c("1", "E", "Y") ~
            DaysToEvent + LOS,
          TRUE ~ NA_real_
        )
      )
  } else if (date_2 == "midpoint") {
    .data %>%
      mutate(
        {{name}} := case_when(
          I10_DX_Admitting %in% codes ~
            DaysToEvent,
          I10_DX1 %in% codes & DXPOA1 %in% c("1", "E", "Y") ~
            DaysToEvent,
          I10_DX2 %in% codes & DXPOA2 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX3 %in% codes & DXPOA3 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX4 %in% codes & DXPOA4 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX5 %in% codes & DXPOA5 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX6 %in% codes & DXPOA6 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX7 %in% codes & DXPOA7 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX8 %in% codes & DXPOA8 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX9 %in% codes & DXPOA9 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX10 %in% codes & DXPOA10 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX11 %in% codes & DXPOA11 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX12 %in% codes & DXPOA12 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX13 %in% codes & DXPOA13 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX14 %in% codes & DXPOA14 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX15 %in% codes & DXPOA15 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX16 %in% codes & DXPOA16 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX17 %in% codes & DXPOA17 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX18 %in% codes & DXPOA18 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX19 %in% codes & DXPOA19 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX20 %in% codes & DXPOA20 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX21 %in% codes & DXPOA21 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX22 %in% codes & DXPOA22 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX23 %in% codes & DXPOA23 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX24 %in% codes & DXPOA24 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX25 %in% codes & DXPOA25 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX26 %in% codes & DXPOA26 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX27 %in% codes & DXPOA27 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX28 %in% codes & DXPOA28 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX29 %in% codes & DXPOA29 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX30 %in% codes & DXPOA30 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX31 %in% codes & DXPOA31 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX32 %in% codes & DXPOA32 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX33 %in% codes & DXPOA33 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_DX34 %in% codes & DXPOA34 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_ECAUSE1 %in% codes & E_POA1 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_ECAUSE2 %in% codes & E_POA2 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_ECAUSE3 %in% codes & E_POA3 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_ECAUSE4 %in% codes & E_POA4 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_ECAUSE5 %in% codes & E_POA5 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          I10_ECAUSE6 %in% codes & E_POA6 %in% c("1", "E", "Y") ~
            ceiling(DaysToEvent + LOS/2),
          TRUE ~ NA_real_
        )
      )
  }
}

mutate_flag_dx1 <- function(.data, codes, name) {
  .data %>%
    mutate(
      {{name}} := case_when(
        I10_DX1 %in% codes ~ 1,
        TRUE ~ 0
      )
    )
}

mutate_flag_dx1_not_POA <- function(.data, codes, name) {
  .data %>%
    mutate(
      {{name}} := case_when(
        I10_DX1 %in% codes & DXPOA1 != "Y" ~ 1,
        TRUE ~ 0
      )
    )
}

mutate_flag_dx1_POA <- function(.data, codes, name) {
  .data %>%
    mutate(
      {{name}} := case_when(
        I10_DX1 %in% codes & DXPOA1 %in% c("1", "E", "Y") ~ 1,
        TRUE ~ 0
      )
    )
}

mutate_date_dx1 <- function(.data, codes, name) {
  .data %>%
    mutate(
      {{name}} := case_when(
        I10_DX1 %in% codes  ~ DaysToEvent,
        TRUE ~ NA_real_
      )
    )
}

mutate_date_dx1_not_POA <- function(.data, codes, name) {
  .data %>%
    mutate(
      {{name}} := case_when(
        I10_DX1 %in% codes & DXPOA1 != "Y"  ~ DaysToEvent,
        TRUE ~ NA_real_
      )
    )
}

mutate_date_dx1_POA <- function(.data, codes, name) {
  .data %>%
    mutate(
      {{name}} := case_when(
        I10_DX1 %in% codes & DXPOA1 %in% c("1", "E", "Y") ~
          DaysToEvent,
        TRUE ~ NA_real_
      )
    )
}

mutate_flag_pr <- function(.data, codes, name) {
  .data %>%
    mutate(
      {{name}} := case_when(
        I10_PR1 %in% codes ~ 1,
        I10_PR2 %in% codes ~ 1,
        I10_PR3 %in% codes ~ 1,
        I10_PR4 %in% codes ~ 1,
        I10_PR5 %in% codes ~ 1,
        I10_PR6 %in% codes ~ 1,
        I10_PR7 %in% codes ~ 1,
        I10_PR8 %in% codes ~ 1,
        I10_PR9 %in% codes ~ 1,
        I10_PR10 %in% codes ~ 1,
        I10_PR11 %in% codes ~ 1,
        I10_PR12 %in% codes ~ 1,
        I10_PR13 %in% codes ~ 1,
        I10_PR14 %in% codes ~ 1,
        I10_PR15 %in% codes ~ 1,
        I10_PR16 %in% codes ~ 1,
        I10_PR17 %in% codes ~ 1,
        I10_PR18 %in% codes ~ 1,
        I10_PR19 %in% codes ~ 1,
        I10_PR20 %in% codes ~ 1,
        I10_PR21 %in% codes ~ 1,
        I10_PR22 %in% codes ~ 1,
        I10_PR23 %in% codes ~ 1,
        I10_PR24 %in% codes ~ 1,
        I10_PR25 %in% codes ~ 1,
        I10_PR26 %in% codes ~ 1,
        I10_PR27 %in% codes ~ 1,
        I10_PR28 %in% codes ~ 1,
        I10_PR29 %in% codes ~ 1,
        I10_PR30 %in% codes ~ 1,
        I10_PR31 %in% codes ~ 1,
        TRUE ~ 0
      )
    )
}

mutate_date_pr <- function(.data, codes, name) {
  .data %>%
    mutate(
      {{name}} := case_when(
        I10_PR1 %in% codes ~ DaysToEvent + PRDAY1,
        I10_PR2 %in% codes ~ DaysToEvent + PRDAY2,
        I10_PR3 %in% codes ~ DaysToEvent + PRDAY3,
        I10_PR4 %in% codes ~ DaysToEvent + PRDAY4,
        I10_PR5 %in% codes ~ DaysToEvent + PRDAY5,
        I10_PR6 %in% codes ~ DaysToEvent + PRDAY6,
        I10_PR7 %in% codes ~ DaysToEvent + PRDAY7,
        I10_PR8 %in% codes ~ DaysToEvent + PRDAY8,
        I10_PR9 %in% codes ~ DaysToEvent + PRDAY9,
        I10_PR10 %in% codes ~ DaysToEvent + PRDAY10,
        I10_PR11 %in% codes ~ DaysToEvent + PRDAY11,
        I10_PR12 %in% codes ~ DaysToEvent + PRDAY12,
        I10_PR13 %in% codes ~ DaysToEvent + PRDAY13,
        I10_PR14 %in% codes ~ DaysToEvent + PRDAY14,
        I10_PR15 %in% codes ~ DaysToEvent + PRDAY15,
        I10_PR16 %in% codes ~ DaysToEvent + PRDAY16,
        I10_PR17 %in% codes ~ DaysToEvent + PRDAY17,
        I10_PR18 %in% codes ~ DaysToEvent + PRDAY18,
        I10_PR19 %in% codes ~ DaysToEvent + PRDAY19,
        I10_PR20 %in% codes ~ DaysToEvent + PRDAY20,
        I10_PR21 %in% codes ~ DaysToEvent + PRDAY21,
        I10_PR22 %in% codes ~ DaysToEvent + PRDAY22,
        I10_PR23 %in% codes ~ DaysToEvent + PRDAY23,
        I10_PR24 %in% codes ~ DaysToEvent + PRDAY24,
        I10_PR25 %in% codes ~ DaysToEvent + PRDAY25,
        I10_PR26 %in% codes ~ DaysToEvent + PRDAY26,
        I10_PR27 %in% codes ~ DaysToEvent + PRDAY27,
        I10_PR28 %in% codes ~ DaysToEvent + PRDAY28,
        I10_PR29 %in% codes ~ DaysToEvent + PRDAY29,
        I10_PR30 %in% codes ~ DaysToEvent + PRDAY30,
        I10_PR31 %in% codes ~ DaysToEvent + PRDAY31,
        TRUE ~ NA_real_
      )
    )
}
