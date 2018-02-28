# Script to download latest LEMIS data from AWS, clean, generate the compressed
# fst file and upload to GitHub
# Access to the EHA AWS bucket is required and it is expected that
# credentials are in .aws/credentials

library(aws.s3)
library(tidyverse)
library(stringi)
library(fst)
h <- here::here
aws.signature::use_credentials()
save_object(
  "cleaned_data/lemis_2000_2013_cleaned.csv", bucket = "eha.wild.db",
  file = h("data-raw", "lemis_2000_2013_cleaned.csv")
)
save_object(
  "csv_by_year/lemis_2000_2013.csv", bucket = "eha.wild.db",
  file = h("data-raw", "lemis_2000_2013.csv")
)
save_object(
  "csv_by_year/lemis_2000_2013.csv", bucket = "eha.wild.db",
  file = h("data-raw", "lemis_2000_2013.csv")
)
# get_bucket_df("eha.wild.db", prefix="csv_by_year/") %>%
#   pull(Key) %>%
#   stri_subset_regex("\\/$", negate = TRUE) %>%
#   map(~save_object(., bucket = "eha.wild.db", file = h("inst", .)))
#
# lemis_files <- fs::dir_ls( h("data-raw", "csv_by_year"), regex="lemis_")
lemis_raw <- read_csv(
  h("data-raw", "lemis_2000_2013_cleaned.csv"),
  col_types = cols(
    .default = col_character(),
    control_number = col_integer(),
    quantity = col_character(),
    disposition_date = col_date(format = ""),
    shipment_date = col_date(format = ""),
    shipment_year = col_integer(),
    quantity_bkp = col_integer(),
    Wild = col_integer(),
    Live = col_integer(),
    NonAq = col_integer()
  )
)

lemis <- lemis_raw %>%
  select(
    control_number, species_code, taxa, genus, species, subspecies,
    specific_name, generic_name, description = wildlife_description, quantity,
    unit, value, country_origin = country_origin_iso2c,
    country_imp_exp = country_imp_exp_iso2c, purpose, source = source_,
    action, disposition, disposition_date, shipment_date, import_export,
    port, us_co, foreign_co
  ) %>%
  mutate(
    quantity = as.integer(as.numeric(quantity)),
    value = as.integer(readr::parse_number(value))
  ) %>%
  mutate_at(
    c("us_co", "foreign_co"),
    funs(stri_replace_all_fixed(., "&amp;", "&"))
  ) %>%
  mutate_if(is.character, funs(if_else(. == "na", NA_character_, .))) %>%
  mutate(port = stri_trans_toupper(port))
# TODO Look for non-legal field codes
write_fst(lemis, h("data-raw", "lemis.fst"), compress = 100)
# lemis:::lemis_release(description = "Initial test release", filename = h("data-raw", "lemis.fst"),
#                      target = "master", ignore_dirty = FALSE)
