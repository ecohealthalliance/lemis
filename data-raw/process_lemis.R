# Script to get the latest cleaned LEMIS data file, generate the compressed
# fst file, and upload to GitHub


# Load packages
library(fst)
library(readr)

h <- here::here

#==============================================================================


# Load and compress cleaned LEMIS data


lemis_for_compression <- read_csv(
  h("data-raw", "lemis_2000_2014_cleaned.csv"),
  col_types = cols(
    .default = col_character(),
    control_number = col_integer(),
    quantity = col_integer(),
    value = col_integer(),
    disposition_date = col_date(format = ""),
    disposition_year = col_integer(),
    shipment_date = col_date(format = ""),
    shipment_year = col_integer(),
    quantity_original_value = col_integer()
  )
)

# Write the compressed data to local disk
write_fst(lemis_for_compression, h("data-raw", "lemis.fst"), compress = 100)

# Release the compressed data
# lemis:::lemis_release(description = "First major package update (v2.0.0)",
#                       filename = h("data-raw", "lemis.fst"),
#                       target = "master", ignore_dirty = FALSE)
