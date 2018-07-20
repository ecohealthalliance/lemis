# Script to get the latest cleaned LEMIS data file, generate the compressed
# fst file, and upload to GitHub


# Load packages
library(tidyverse)
library(stringi)
library(fst)

h <- here::here

# ==============================================================================


# Load and compress cleaned LEMIS data


lemis_for_compression <- read_csv(
  h("data-raw", "lemis_2000_2014_cleaned.csv"),
  col_types = cols(
    .default = col_character(),
    control_number = col_integer(),
    quantity = col_integer(),
    disposition_date = col_date(format = ""),
    shipment_date = col_date(format = ""),
    shipment_year = col_integer(),
    quantity_bkp = col_integer(),
    Wild = col_integer(),
    Live = col_integer(),
    NonAq = col_integer()
  )
) %>%
  select(
    control_number, species_code, taxa, genus,
    species, subspecies, specific_name, generic_name,
    description = wildlife_description,
    quantity, unit, value,
    country_origin = country_origin_iso2c,
    country_imp_exp = country_imp_exp_iso2c,
    purpose,
    source = source_,
    action, disposition, disposition_date, shipment_date,
    import_export, port, us_co, foreign_co,
    cleaning_notes
  ) %>%
  mutate(
    value = as.integer(readr::parse_number(value))
  ) %>%
  mutate_if(is.character, funs(if_else(. == "na", NA_character_, .)))

# Write the compressed data to local disk
write_fst(lemis_for_compression, h("data-raw", "lemis.fst"), compress = 100)

# Release the compressed data
# lemis:::lemis_release(description = "First major package update (v2.0.0)",
#                       filename = h("data-raw", "lemis.fst"),
#                       target = "master", ignore_dirty = FALSE)
