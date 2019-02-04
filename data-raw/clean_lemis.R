# LEMIS data cleaning script based on WildDB script originally written
# by Allison White


# Load packages
library(assertthat)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)

h <- here::here

#==============================================================================


# Function to clean the intermediate LEMIS dataset variables given valid codes

get_cleaned_lemis <- function(variable, valid.values) {

  # Get the column index of the variable to clean
  index <- which(colnames(lemis) == variable)

  lemis %>%
    # Add cleaning notes based on the values in the variable
    mutate(
      cleaning_notes = case_when(
        !(.[[index]] %in% valid.values) & !is.na(.[[index]]) & is.na(cleaning_notes) ~
          paste0("Original value in ", variable, " column: ", .[[index]]),
        !(.[[index]] %in% valid.values) & !is.na(.[[index]]) & !is.na(cleaning_notes) ~
          paste0(cleaning_notes, ", ", variable, " column: ", .[[index]]),
        TRUE ~ cleaning_notes
      )
    ) %>%
    # Add non-standard values to the variable in question where appropriate
    mutate(
      UQ(rlang::sym(variable)) :=
        ifelse(!(UQ(rlang::sym(variable)) %in% valid.values) & !is.na(UQ(rlang::sym(variable))),
               "non-standard value", UQ(rlang::sym(variable))),
      UQ(rlang::sym(variable)) := as.factor(UQ(rlang::sym(variable)))
    )
}

#==============================================================================


# Merge all LEMIS data


lemis.cols <- c(
  "control_number",
  "species_code",
  "genus",
  "species",
  "subspecies",
  "specific_name",
  "generic_name",
  "wildlife_description",
  "quantity",
  "unit",
  "value",
  "country_origin_iso2c",
  "country_imp_exp_iso2c",
  "purpose",
  "source_",
  "action",
  "disposition",
  "disposition_date",
  "shipment_date",
  "import_export",
  "port",
  "us_co",
  "foreign_co"
)

yearly.files <- dir(path = h("data-raw", "csv_by_year"), full.names = TRUE)
lemis_raw <- data.frame()

# Merge yearly LEMIS CSV files into one dataframe
for (file in yearly.files) {

  print(file)

  lemis_raw <-
    rbind(
      lemis_raw,
      read_csv(file,
               col_names = lemis.cols,
               skip = 1,
               col_types = cols(.default = col_character()),
               na = c(
                 "", " ", "na", "NA", "NULL",
                 "*", "**", "***", "****", "*****", "******"
               )
      )
    )
}

# Eliminate rows of all NA values
lemis <- lemis_raw[apply(lemis_raw, 1, function(x) any(!is.na(x))), ]

#==============================================================================


# Clean up control number 2006798504

# In our raw data, we discovered that the record for control number
# 2006798504 actually contains a "foreign_co" value with multiple other records
# embedded within it. We need to extract these "hidden records" and clean up
# the surrounding data


# Extract the "hidden records"
hidden.rows <- lemis %>%
  # Filter down to the problematic record
  filter(control_number == "2006798504") %>%
  # Pull out the "foreign_co" value
  pull(foreign_co) %>%
  # Split along the newline character
  str_split("\n") %>%
  # Create a dataframe that contains one row for each record that was
  # hiding in the "foreign_co" column
  data.frame() %>%
  # Get rid of the first row, since this only corresponds to the actual
  # "foreign_co" value of the original problematic record
  slice(-1) %>%
  # Separate out the dataframe into columns based on tab characters
  separate(col = 1, into = lemis.cols, sep = "\t") %>%
  # Clean up the "foreign_co" column
  mutate(foreign_co = str_replace(foreign_co, "\r\r", ""))

# The row following the problematic row in the raw data was cut off and
# incomplete. We need to combine information from the last row of
# "hidden.rows" with this data
last.row.index <- nrow(hidden.rows)
incomplete.row <- lemis[lemis$control_number == "38735", ]

# The split between the last row of "hidden.rows" and "incomplete.row" happened
# over the "shipment_date" column. From examining the raw data, it was apparent
# that the correct "shipment_date" for the record is "01/18/2006"
hidden.rows[last.row.index, "shipment_date"] <- "01/18/2006"
# The second through the fifth columns of "incomplete.row" actually hold what
# should be the final four columns of data for the last row in "hidden.rows"
hidden.rows[last.row.index, 20:23] <- incomplete.row[ , 2:5]

# Now, remove the incomplete row from the lemis data
lemis <- lemis[lemis$control_number != "38735", ]

# Replace the "foreign_co" value of the original problematic record
lemis[lemis$control_number == "2006798504", "foreign_co"] <- "EL ARPA"

# Bind on the previously hidden rows
lemis <- bind_rows(lemis, hidden.rows)

#==============================================================================


# Clean up columns


lemis <- lemis %>%
  mutate(
    control_number = as.numeric(control_number),
    species_code = toupper(species_code),
    genus = tolower(genus),
    species = tolower(species),
    # convert "species" and NA values in the species column to "spp."
    species = case_when(
      species == "species" ~ "spp.",
      !is.na(genus) & is.na(species) ~ "spp.",
      TRUE ~ species
    ),
    subspecies = tolower(subspecies),
    specific_name = toupper(specific_name),
    generic_name = toupper(generic_name),
    description = toupper(wildlife_description), # uppercase codes
    quantity = as.numeric(gsub(",", "", quantity)),
    unit = toupper(unit), # uppercase codes
    value = as.numeric(gsub(",", "", value)),
    country_origin = toupper(country_origin_iso2c), # uppercase codes
    country_imp_exp = toupper(country_imp_exp_iso2c), # uppercase codes
    purpose = toupper(purpose), # uppercase codes
    source = toupper(source_), # uppercase codes
    action = toupper(action), # uppercase codes
    disposition = toupper(disposition), # uppercase codes
    disposition_date = as.Date(disposition_date, format = "%Y-%m-%d"),
    shipment_date = as.Date(shipment_date, format = "%Y-%m-%d"),
    import_export = toupper(import_export),
    port = toupper(port), # uppercase codes
    us_co = toupper(us_co),
    foreign_co = toupper(foreign_co)
  )

# ==============================================================================


# Further data filtering


lemis <- lemis %>%
  # remove any duplicated shipment records
  distinct() %>%
  # remove any "E", "T", and NA records from the import_export column,
  # leaving only importation data
  filter(import_export != "E", import_export != "T", !is.na(import_export))

assert_that(sum(lemis$import_export == "I") == nrow(lemis))


# Issue of US to US importation shipments. Looking at the country of origin
# information in the database shows that the US is among the top 10 countries
# importing to the US!
count(lemis, country_origin, sort = TRUE)
# Currently, this data is still included in the LEMIS database, so be careful


# Generate a cleaning notes column to hold automatically generated notes
lemis$cleaning_notes <- rep(NA_character_, nrow(lemis))

#==============================================================================


# Cleaning of non-standard descriptions present in the LEMIS data


sort(unique(lemis$description))

valid.description.codes <-
  c(
    "BAL", "BAR", "BOC", "BOD", "BON", "BOP", "BUL", "CAL",
    "CAP", "CAR", "CAV", "CHP", "CLA", "CLO", "COR", "CPR",
    "CUL", "CUT", "DEA", "DER", "DPL", "EAR", "EGG", "EGL",
    "ESH", "EXT", "FEA", "FIB", "FIG", "FIN", "FLO", "FOO",
    "FPT", "FRU", "GAB", "GAL", "GAR", "GEN", "GRS", "HAI",
    "HAP", "HOC", "HOP", "HOR", "IJW", "IVC", "IVP", "JWL",
    "KEY", "LEG", "LIV", "LOG", "LPL", "LPS", "LVS", "MEA",
    "MED", "MUS", "NES", "OIL", "PIV", "PLA", "PLY", "POW",
    "ROC", "ROO", "RUG", "SAW", "SCA", "SDL", "SEE", "SHE",
    "SHO", "SID", "SKE", "SKI", "SKP", "SKU", "SOU", "SPE",
    "SPR", "STE", "SWI", "TAI", "TEE", "TIM", "TRI", "TRO",
    "TUS", "UNS", "VEN", "WAX", "WNG", "WPR"
  )

# Which values are not in the valid codes?
index.invalid.codes <- !(unique(lemis$description) %in% valid.description.codes)
sort(unique(lemis$description)[index.invalid.codes])

# Convert irregular values to good values
lemis <- lemis %>%
  mutate(
    description = case_when(
      # Change "JEW" to "JWL" under the assumption this was meant to indicate
      # "jewelry"
      description == "JEW" ~ "JWL",
      # Change "LI" to "LIV"
      description == "LI" ~ "LIV",
      # Chnage "MAE" to "MEA" under the assumption this was meant to indicate
      # "meat"
      description == "MAE" ~ "MEA",
      TRUE ~ description
    )
  )

# Remove remaining non-standard descriptions
lemis <- get_cleaned_lemis("description", valid.description.codes)

# Assertion for quality checking. All levels should be a valid code
assert_that(all(levels(lemis$description) %in%
  c(valid.description.codes, "non-standard value")))

summary(lemis$description)


# Cleaning of non-standard units present in the LEMIS data


sort(unique(lemis$unit))

valid.unit.codes <-
  c(
    "C2", "C3", "CM", "GM", "KG", "LT",
    "M2", "M3", "MG", "ML", "MT", "NO"
  )

# Which values are not in the valid codes?
index.invalid.codes <- !(unique(lemis$unit) %in% valid.unit.codes)
sort(unique(lemis$unit)[index.invalid.codes])

# The unit "LB" may stand for pounds, so we need to change this to kilograms
# where 1 LB = 0.454 KG
lemis <- lemis %>%
  mutate(
    # backup quantity and unit columns to preserve the originals
    quantity_original_value = quantity,
    unit_original_value = unit,
    # convert pounds to kilograms by dividing the pound units by 0.454
    quantity = case_when(
      unit == "LB" ~ quantity / 0.454,
      TRUE ~ quantity
    ),
    unit = ifelse(unit == "LB", "KG", unit)
  )

# All variations of the "number of specimens" entries should be recoded
# as "NO"
lemis$unit <- ifelse(lemis$unit %in% c("N", "N0"), "NO", lemis$unit)

# Remove remaining non-standard units
lemis <- get_cleaned_lemis("unit", valid.unit.codes)

# Assertion for quality checking. All levels should be a valid code
assert_that(all(levels(lemis$unit) %in%
  c(valid.unit.codes, "non-standard value")))

summary(lemis$unit)


# Cleaning of non-standard country origins present in the LEMIS data


sort(unique(lemis$country_origin))

valid.country.codes <- read_csv("inst/extdata/iso_2_country_codes.csv") %>%
  pull(Value)
# Add on other valid codes
valid.country.codes <- c(
  valid.country.codes,
  "BL", "BQ", "CW", "GG", "IM", "JE", "ME", "MF", "PS", "RS",
  "SX", "TL", "VS", "XX", "ZZ"
)

# Which values are not in the valid codes?
index.invalid.codes <- !(unique(lemis$country_origin) %in% valid.country.codes)
sort(unique(lemis$country_origin)[index.invalid.codes])

# Convert irregular country_origin values to good values
lemis <- lemis %>%
  mutate(
    country_origin = case_when(
      # Change "X" to "XX"
      country_origin == "X" ~ "XX",
      TRUE ~ country_origin
    )
  )

# Remove remaining non-standard country origins
lemis <- get_cleaned_lemis("country_origin", valid.country.codes)

# Assertion for quality checking. All levels should be a valid code
assert_that(all(levels(lemis$country_origin) %in%
  c(valid.country.codes, "non-standard value")))

summary(lemis$country_origin)


# Cleaning of non-standard country import/export present in the LEMIS data


sort(unique(lemis$country_imp_exp))

# Which values are not in the valid codes?
index.invalid.codes <- !(unique(lemis$country_imp_exp) %in% valid.country.codes)
sort(unique(lemis$country_imp_exp)[index.invalid.codes])

# Convert irregular country_origin values to good values
lemis <- lemis %>%
  mutate(
    country_imp_exp = case_when(
      # Change "**" to NA
      str_detect(country_imp_exp, fixed("**", TRUE)) ~ NA_character_,
      TRUE ~ country_imp_exp
    )
  )

# Remove remaining non-standard country import/export
lemis <- get_cleaned_lemis("country_imp_exp", valid.country.codes)

# Assertion for quality checking. All levels should be a valid code
assert_that(all(levels(lemis$country_imp_exp) %in%
  c(valid.country.codes, "non-standard value")))

summary(lemis$country_imp_exp)


# Cleaning of non-standard purposes present in the LEMIS data


sort(unique(lemis$purpose))

valid.purpose.codes <-
  c("B", "E", "G", "H", "L", "M", "P", "Q", "S", "T", "Y", "Z")

# Which values are not in the valid codes?
index.invalid.codes <- !(unique(lemis$purpose) %in% valid.purpose.codes)
sort(unique(lemis$purpose)[index.invalid.codes])

# Remove remaining non-standard purposes
lemis <- get_cleaned_lemis("purpose", valid.purpose.codes)

# Assertion for quality checking. All levels should be a valid code
assert_that(all(levels(lemis$purpose) %in%
  c(valid.purpose.codes, "non-standard value")))

summary(lemis$purpose)


# Cleaning of non-standard sources present in the LEMIS data


sort(unique(lemis$source))

valid.source.codes <-
  c("A", "C", "D", "F", "I", "P", "R", "U", "W")

# Which values are not in the valid codes?
index.invalid.codes <- !(unique(lemis$source) %in% valid.source.codes)
sort(unique(lemis$source)[index.invalid.codes])

# Remove remaining non-standard sources
lemis <- get_cleaned_lemis("source", valid.source.codes)

# Assertion for quality checking. All levels should be a valid code
assert_that(all(levels(lemis$source) %in%
  c(valid.source.codes, "non-standard value")))

summary(lemis$source)


# Cleaning of non-standard actions present in the LEMIS data


sort(unique(lemis$action))

valid.action.codes <- c("C", "R")

# Which values are not in the valid codes?
index.invalid.codes <- !(unique(lemis$action) %in% valid.action.codes)
sort(unique(lemis$action)[index.invalid.codes])

# Assertion for quality checking. All levels should be a valid code
assert_that(all(levels(lemis$action) %in%
  c(valid.action.codes, "non-standard value")))

summary(as.factor(lemis$action))


# Cleaning of non-standard dispositions present in the LEMIS data


sort(unique(lemis$disposition))

valid.disposition.codes <- c("A", "C", "R", "S")

# Which values are not in the valid codes?
index.invalid.codes <- !(unique(lemis$disposition) %in% valid.disposition.codes)
sort(unique(lemis$disposition)[index.invalid.codes])

# Remove remaining non-standard dispositions
lemis <- get_cleaned_lemis("disposition", valid.disposition.codes)

# Assertion for quality checking. All levels should be a valid code
assert_that(all(levels(lemis$disposition) %in%
  c(valid.disposition.codes, "non-standard value")))

summary(lemis$disposition)


# Cleaning of non-standard ports present in the LEMIS data


sort(unique(lemis$port))

valid.port.codes <-
  c(
    "1", "2", "3", "4", "5", "6", "7", "8",
    "AG", "AL", "AN", "AT", "BA", "BL", "BN",
    "BO", "BV", "CA", "CH", "CL", "CP", "CX",
    "DE", "DF", "DG", "DL", "DN", "DR", "DS",
    "DU", "EA", "EL", "FB", "GP", "HA", "HN",
    "HO", "HS", "IF", "JK", "JU", "LA", "LK",
    "LO", "LR", "LV", "MC", "ME", "MI", "MP",
    "NF", "NG", "NO", "NW", "NY", "PA", "PB",
    "PH", "PL", "PT", "PX", "RY", "SE", "SF",
    "SJ", "SL", "SP", "SS", "SU", "SW", "SY",
    "TP", "XX"
  )

# Which values are not in the valid codes?
index.invalid.codes <- !(unique(lemis$port) %in% valid.port.codes)
sort(unique(lemis$port)[index.invalid.codes])

# Convert irregular values to good values
lemis <- lemis %>%
  mutate(
    port = case_when(
      # Change "03" to "3"
      port == "03" ~ "3",
      # Change NA values to "XX" since that represents unknown port of entry
      is.na(port) ~ "XX",
      TRUE ~ port
    )
  )

# Remove remaining non-standard ports
lemis <- get_cleaned_lemis("port", valid.port.codes)

# Assertion for quality checking. All levels should be a valid code
assert_that(all(levels(lemis$port) %in%
  c(valid.port.codes, "non-standard value")))

summary(lemis$port)

#==============================================================================


# Join in taxonomic information and create new variables


# Generate a table of taxa information
taxa_code <- read.csv("inst/extdata/Taxalist_reviewed.csv",
                      na.strings = c(" ", "")
) %>%
  select(species_code_taxa = SPEC_CODE, taxa = Taxa) %>%
  mutate(
    species_code_taxa = toupper(species_code_taxa),
    taxa = as.factor(tolower(taxa))
  ) %>%
  distinct()

# Join this table with the LEMIS data
lemis <- lemis %>%
  left_join(taxa_code, by = c("species_code" = "species_code_taxa")) %>%
  mutate(species_code = as.factor(species_code))

#==============================================================================


# Data saving


lemis_to_save <- lemis %>%
  # extract only the year component of the disposition and shipment dates
  mutate(
    disposition_year = as.numeric(format(disposition_date, "%Y")),
    shipment_year = as.numeric(format(shipment_date, "%Y"))
  ) %>%
  select(
    control_number,
    taxa,
    species_code,
    genus,
    species,
    subspecies,
    specific_name,
    generic_name,
    description,
    quantity,
    unit,
    value,
    country_origin,
    country_imp_exp,
    purpose,
    source,
    action,
    disposition,
    disposition_date,
    disposition_year,
    shipment_date,
    shipment_year,
    import_export,
    port,
    us_co,
    foreign_co,
    cleaning_notes,
    quantity_original_value,
    unit_original_value
  ) %>%
  mutate_all(funs(as.character(.))) %>%
  mutate_at(
    c("control_number",
      "quantity",
      "value",
      "disposition_year",
      "shipment_year",
      "quantity_original_value"),
    funs(as.integer(.))
  ) %>%
  mutate_if(is.character, funs(if_else(. == "na", NA_character_, .))) %>%
  arrange(shipment_date, control_number)

# Write a cleaned CSV file of all LEMIS data
write_csv(
  lemis_to_save,
  h("data-raw",
    paste0("lemis_",
           min(lemis_to_save$shipment_year, na.rm = TRUE), "_",
           max(lemis_to_save$shipment_year, na.rm = TRUE), "_cleaned.csv")
  )
)

#==============================================================================


# Directory cleanup

# At this point it should be safe to delete subdirectories holding
# intermediate LEMIS files
unlink(h("data-raw", "by_year"), recursive = TRUE)
unlink(h("data-raw", "csv_by_year"), recursive = TRUE)
