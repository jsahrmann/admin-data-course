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

dx_flag <- function(x, codes) {
  y <- c(
    x[["I10_DX_Admitting"]], x[["I10_DX1"]], x[["I10_DX2"]],
    x[["I10_DX3"]], x[["I10_DX4"]], x[["I10_DX5"]], x[["I10_DX6"]],
    x[["I10_DX7"]], x[["I10_DX8"]], x[["I10_DX9"]], x[["I10_DX10"]],
    x[["I10_DX11"]], x[["I10_DX12"]], x[["I10_DX13"]],
    x[["I10_DX14"]], x[["I10_DX15"]], x[["I10_DX16"]],
    x[["I10_DX17"]], x[["I10_DX18"]], x[["I10_DX19"]],
    x[["I10_DX20"]], x[["I10_DX21"]], x[["I10_DX22"]],
    x[["I10_DX23"]], x[["I10_DX24"]], x[["I10_DX25"]],
    x[["I10_DX26"]], x[["I10_DX27"]], x[["I10_DX28"]],
    x[["I10_DX29"]], x[["I10_DX30"]], x[["I10_DX31"]],
    x[["I10_DX32"]], x[["I10_DX33"]], x[["I10_DX34"]],
    x[["I10_ECAUSE1"]], x[["I10_ECAUSE2"]], x[["I10_ECAUSE3"]],
    x[["I10_ECAUSE4"]], x[["I10_ECAUSE5"]], x[["I10_ECAUSE6"]])
  y <- y[!is.na(y) & y != ""]
  as.integer(any(y %in% codes))
}

core1p3 <- core1p %>%
  mutate(
    dx_acutemi = apply(core1p, 1, dx_flag, dx10_acutemi)
  )
table(core1p3$dx_acutemi)


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


## Testing -----------------------------------------------------------

setwd("//storage1.ris.wustl.edu/colditzg/Active/admin_course_jsahrmann")
core1p <- read_rds("../admin_course_data/fl_sidc_core_1pSample.rds")

dx10_acutemi <- c(
  "I2101", "I2102", "I2109", "I2111", "I2119", "I2121", "I2129",
  "I213", "I214", "I220", "I221", "I222", "I228", "I229"
)
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

test01 <- core1p %>%
  mutate_flag_dx("R05", dx_cough)
table(test01$dx_cough, useNA = "ifany")

test02 <- core1p %>%
  mutate_flag_dx_not_POA("R05", dx_cough)
table(test02$dx_cough, useNA = "ifany")

test03 <- core1p %>%
  mutate_flag_dx_POA("R05", dx_cough)
table(test03$dx_cough, useNA = "ifany")

test01 <- core1p %>%
  mutate_date_dx("R05", date_cough0)
n_distinct(test01$date_cough0)
test01 <- test01 %>%
  mutate_date_dx("R05", date_cough, date_2 = "discharge")
all(test01$date_cough0 == test01$date_cough, na.rm = TRUE)
test01 <- core1p %>%
  mutate_date_dx("R05", date_cough, date_2 = "midpoint")
n_distinct(test01$date_cough)

test02 <- core1p %>%
  mutate_date_dx_not_POA("R05", date_cough)
n_distinct(test02$date_cough)

test03 <- core1p %>%
  mutate_date_dx_POA("R05", date_cough)
n_distinct(test03$date_cough)


test01 <- core1p %>%
  mutate_flag_dx(dx10_acutemi, dx_acutemi)
table(test01$dx_acutemi, useNA = "ifany")

test02 <- core1p %>%
  mutate_flag_dx_not_POA(dx10_acutemi, dx_acutemi)
table(test02$dx_acutemi, useNA = "ifany")

test03 <- core1p %>%
  mutate_flag_dx_POA(dx10_acutemi, dx_acutemi)
table(test03$dx_acutemi, useNA = "ifany")

test01 <- core1p %>%
  mutate_date_dx(dx10_acutemi, date_acutemi0)
n_distinct(test01$date_acutemi0)
test01 <- test01 %>%
  mutate_date_dx(dx10_acutemi, date_acutemi, date_2 = "discharge")
all(test01$date_acutemi0 == test01$date_acutemi, na.rm = TRUE)
test01 <- core1p %>%
  mutate_date_dx(dx10_acutemi, date_acutemi, date_2 = "midpoint")
n_distinct(test01$date_acutemi)

test02 <- core1p %>%
  mutate_date_dx_not_POA(dx10_acutemi, date_acutemi)
n_distinct(test02$date_acutemi)

test03 <- core1p %>%
  mutate_date_dx_POA(dx10_acutemi, date_acutemi)
n_distinct(test03$date_acutemi)
