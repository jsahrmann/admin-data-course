## Header ------------------------------------------------------------
##
## M19 PHS 5254 Using Administrative Data for Health Services Research
## Washington University School of Medicine in St. Louis
##
## Create a table tracking changes to ICD-10-CM diagnosis codes across
## years.
##
## Reference:
## <https://www.cms.gov/Medicare/Coding/ICD10>


## Setup -------------------------------------------------------------

library(dplyr)
library(readr)
library(tidyr)

# Create temporary files to store Zip files downloaded from CMS and
# their contents after unzipping.
temp_zip_2015 <- tempfile()
temp_files_2015 <- tempfile()
temp_zip_2016 <- tempfile()
temp_files_2016 <- tempfile()
temp_zip_2017 <- tempfile()
temp_files_2017 <- tempfile()
temp_zip_2018 <- tempfile()
temp_files_2018 <- tempfile()
temp_zip_2019 <- tempfile()
temp_files_2019 <- tempfile()
temp_zip_2020 <- tempfile()
temp_files_2020 <- tempfile()
temp_zip_2021 <- tempfile()
temp_files_2021 <- tempfile()
temp_zip_2022 <- tempfile()
temp_files_2022 <- tempfile()
temp_zip_2023 <- tempfile()
temp_files_2023 <- tempfile()


## Constant definitions ----------------------------------------------

# Store the download URLs from CMS.
url15 <- paste0(
  "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/",
  "2015-code-descriptions.zip")
url16 <- paste0(
  "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/",
  "2016-Code-Descriptions-in-Tabular-Order.zip")
url17 <- paste0(
  "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/",
  "2017-ICD10-Code-Descriptions.zip")
url18 <- paste0(
  "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/",
  "2018-ICD-10-Code-Descriptions.zip")
url19 <- paste0(
  "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/",
  "2019-ICD-10-CM-Code-Descriptions.zip")
url20 <- paste0(
  "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/",
  "2020-ICD-10-CM-Codes.zip")
url21 <- paste0(
  "https://www.cms.gov/files/zip/",
  "2021-code-descriptions-tabular-order-updated-12162020.zip")
url22 <- paste0(
  "https://www.cms.gov/files/zip/",
  "2022-code-descriptions-tabular-order-updated-02012022.zip")
url23 <- paste0(
  "https://www.cms.gov/files/zip/",
  "2023-code-descriptions-tabular-order.zip")

# Number of rows for readr::read_fwf to use in determining column
# widths
#
# This needs to be large to ensure that code descriptions longer than
# the longest description among the first 1000 rows (readr::read_fwf's
# default) aren't split into an extra column.
INITIAL_READ_ROWS <- 70000


## Input -------------------------------------------------------------

# Download the yearly code files. The structure of the files is
# identical starting in 2016 (2015 doesn't seem to have a 'codes' file
# or at least not in the usual place), but the directory structure of
# the Zip archives (needlessly) differs, so there really isn't any way
# to condense this.
download.file(url15, temp_zip_2015)
unzip(temp_zip_2015, exdir = temp_files_2015)
dx10_2015 <- readr::read_fwf(
  file.path(temp_files_2015, "icd10cm_order_2015.txt"),
  guess_max = INITIAL_READ_ROWS
) %>%
  dplyr::rename(
    order_number = X1, icd10cm = X2, valid = X3,
    short_description = X4, long_description = X5) %>%
  dplyr::filter(valid == 1) %>%
  dplyr::select(icd10cm, long_description)

download.file(url16, temp_zip_2016)
unzip(temp_zip_2016, exdir = temp_files_2016)
dx10_2016 <- readr::read_fwf(
  file.path(temp_files_2016, "icd10cm_codes_2016.txt"),
  guess_max = INITIAL_READ_ROWS
) %>%
  dplyr::rename(icd10cm = X1, long_description = X2)

download.file(url17, temp_zip_2017)
unzip(temp_zip_2017, exdir = temp_files_2017)
dx10_2017 <- readr::read_fwf(
  file.path(temp_files_2017, "icd10cm_codes_2017.txt"),
  guess_max = INITIAL_READ_ROWS
) %>%
  dplyr::rename(icd10cm = X1, long_description = X2)

download.file(url18, temp_zip_2018)
unzip(temp_zip_2018, exdir = temp_files_2018)
dx10_2018 <- readr::read_fwf(
  file.path(temp_files_2018, "icd10cm_codes_2018.txt"),
  guess_max = INITIAL_READ_ROWS
) %>%
  dplyr::rename(icd10cm = X1, long_description = X2)

download.file(url19, temp_zip_2019)
unzip(temp_zip_2019, exdir = temp_files_2019)
dx10_2019 <- readr::read_fwf(
  file.path(temp_files_2019, "icd10cm_codes_2019.txt"),
  guess_max = INITIAL_READ_ROWS
) %>%
  dplyr::rename(icd10cm = X1, long_description = X2)

download.file(url20, temp_zip_2020)
unzip(temp_zip_2020, exdir = temp_files_2020)
dx10_2020 <- readr::read_fwf(
  file.path(
    temp_files_2020,
    "/2020 Code Descriptions/icd10cm_codes_2020.txt"),
  guess_max = INITIAL_READ_ROWS
) %>%
  dplyr::rename(icd10cm = X1, long_description = X2)

download.file(url21, temp_zip_2021)
unzip(temp_zip_2021, exdir = temp_files_2021)
dx10_2021 <- readr::read_fwf(
  file.path(
    temp_files_2021,
    "/2021-code-descriptions-tabular-order/icd10cm_codes_2021.txt"),
  guess_max = INITIAL_READ_ROWS
) %>%
  dplyr::rename(icd10cm = X1, long_description = X2)

download.file(url22, temp_zip_2022)
unzip(temp_zip_2022, exdir = temp_files_2022)
dx10_2022 <- readr::read_fwf(
  file.path(
    temp_files_2022,
    "/Code Descriptions/icd10cm_codes_2022.txt"),
  guess_max = INITIAL_READ_ROWS
) %>%
  dplyr::rename(icd10cm = X1, long_description = X2)

download.file(url23, temp_zip_2023)
unzip(temp_zip_2023, exdir = temp_files_2023)
dx10_2023 <- readr::read_fwf(
  file.path(
    temp_files_2023,
    paste0(
      "/2023 Code Descriptions in Tabular Order/",
      "icd10cm_codes_2023.txt")),
  guess_max = INITIAL_READ_ROWS
) %>%
  dplyr::rename(icd10cm = X1, long_description = X2)


## Data management ---------------------------------------------------

dx10_1523 <- dx10_2015 %>%
  dplyr::rename(long_description2015 = long_description) %>%
  dplyr::full_join(
    dplyr::rename(dx10_2016, long_description2016 = long_description),
    by = "icd10cm"
  ) %>%
  dplyr::full_join(
    dplyr::rename(dx10_2017, long_description2017 = long_description),
    by = "icd10cm"
  ) %>%
  dplyr::full_join(
    dplyr::rename(dx10_2018, long_description2018 = long_description),
    by = "icd10cm"
  ) %>%
  dplyr::full_join(
    dplyr::rename(dx10_2019, long_description2019 = long_description),
    by = "icd10cm"
  ) %>%
  dplyr::full_join(
    dplyr::rename(dx10_2020, long_description2020 = long_description),
    by = "icd10cm"
  ) %>%
  dplyr::full_join(
    dplyr::rename(dx10_2021, long_description2021 = long_description),
    by = "icd10cm"
  ) %>%
  dplyr::full_join(
    dplyr::rename(dx10_2022, long_description2022 = long_description),
    by = "icd10cm"
  ) %>%
  dplyr::full_join(
    dplyr::rename(dx10_2023, long_description2023 = long_description),
    by = "icd10cm"
  )


x <- dx10_1523 %>%
  tidyr::pivot_longer(
    cols = dplyr::starts_with("long_description"),
    names_to = "year",
    names_prefix = "long_description",
    values_to = "long_description"
  )

avail <- x %>%
  dplyr::filter(!is.na(long_description)) %>%
  dplyr::mutate(year = as.integer(year)) %>%
  dplyr::group_by(icd10cm) %>%
  dplyr::summarise(available = paste(min(year), max(year), sep = "--"))

library(Rcpp)
Cpp_boundedCumsum <- cppFunction('NumericVector boundedCumsum(NumericVector x){
  int n = x.size();
  NumericVector out(n);
  double tmp;
  out[0] = x[0];
  for(int i = 1; i < n; ++i){
     tmp = out[i-1] + x[i];
     if(tmp < 0.0 || tmp > 1.0) 
        out[i] = out[i-1];
     else 
        out[i] = tmp;
  }
  return out;
}')


Cpp_cumequals <- cppFunction("NumericVector cumequals(CharacterVector x) {
  int n = x.size();
  NumericVector out(n);
  String tmp;
  out[0] = 0;
  for (int i = 1; i < n; ++i) {
    if (x[i-1] == x[i]) {
      out[i] = 0;
    } else {
      out[i] = 1;
    }
  }
  return out;
}")

z <- c("dog", "dog", "cat", "cat", "dog", "dog", "dog")

Cpp_cumequals(z)

change <- x %>%
  dplyr::group_by(icd10cm) %>%
  dplyr::mutate(changed = Cpp_cumequals(long_description)) %>%
  dplyr::ungroup()
