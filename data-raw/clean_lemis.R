# LEMIS data cleaning script based on WildDB script originally written
# by Allison White


# Load packages
library(dplyr)
library(readr)
library(countrycode)
library(assertthat)

# ==============================================================================


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

yearly.files <- dir(path = "data-raw/csv_by_year", full.names = TRUE)
lemis <- data.frame()

# Merge yearly LEMIS csv files into one dataframe
for (file in yearly.files) {
  print(file)

  lemis <-
    rbind(
      lemis,
      read_csv(file,
        col_names = lemis.cols,
        col_types = cols(.default = col_character()),
        na = c(
          "", " ", "NA", "NULL",
          "*", "**", "***", "****",
          "*****", "******"
        )
      ) %>%
        # Need to slice off the first row because the header gets imported
        # as a row on every file given this particular header formatting
        slice(-1)
    )
}

# Some rows of all NA values were generated during data import. Eliminate those
lemis <- lemis[apply(lemis, 1, function(x) any(!is.na(x))), ]

# ==============================================================================


# Clean up column names


# Define revised format for each column
lemis <- lemis %>%
  mutate(
    control_number = as.numeric(control_number),
    species_code = as.factor(species_code),
    genus = as.factor(genus),
    subspecies = as.factor(subspecies),
    specific_name = as.factor(specific_name),
    generic_name = as.factor(generic_name),
    wildlife_description = as.factor(toupper(wildlife_description)),
    quantity = as.numeric(gsub(",", "", quantity)),
    unit = as.factor(unit),
    country_origin_iso2c = as.factor(toupper(country_origin_iso2c)),
    country_imp_exp_iso2c = as.factor(toupper(country_imp_exp_iso2c)),
    purpose = as.factor(purpose),
    source_ = as.factor(source_),
    action = as.factor(action),
    disposition = as.factor(disposition),
    disposition_date = as.Date(disposition_date,
      format = "%Y-%m-%d"
    ),
    shipment_date = as.Date(shipment_date,
      format = "%Y-%m-%d"
    ),
    import_export = as.factor(import_export),
    port = as.factor(toupper(port)),
    us_co = as.factor(us_co),
    foreign_co = as.factor(foreign_co)
  )

# Column clean up and additions
lemis <- lemis %>%
  mutate(
    genus = as.factor(tolower(genus)),
    # convert "SPECIES" and NA values in the species column to "spp."
    species = plyr::revalue(species, c(SPECIES = "spp.")),
    species = ifelse(is.na(species), "spp.", species),
    species = as.factor(tolower(species)),
    # create a binomial column
    binomial = paste(genus, species, sep = "_"),
    # in the binomial column, convert "NA_spp." to NA values and anything
    # with "noncites entry" for the genus column should be "noncites"
    binomial = case_when(
      binomial == "NA_spp." ~ NA_character_,
      genus == "noncites entry" ~ "noncites",
      TRUE ~ binomial
    ),
    binomial = as.factor(binomial),
    # extract only the year component of the shipment date
    shipment_year = as.numeric(format(shipment_date, "%Y")),
    # add country, continent, and region, based on the
    # country of origin (ISO2)
    country_origin = as.factor(countrycode(
      country_origin_iso2c, "iso2c", "country.name"
    )),
    continent_origin = as.factor(countrycode(
      country_origin_iso2c, "iso2c", "continent"
    )),
    region_origin = as.factor(countrycode(
      country_origin_iso2c, "iso2c", "region"
    ))
  ) %>%
  droplevels()

# ==============================================================================


# Further data filtering


lemis <- lemis %>%
  # remove any duplicated shipment records
  distinct() %>%
  # remove record related to "AFRICAN FIELD SPORTS"
  filter(control_number != 38735) %>%
  # remove any "E", "T", and NA records from the import_export column,
  # leaving only importation data
  filter(import_export != "E", import_export != "T", !is.na(import_export)) %>%
  droplevels()


# Issue of US to US importation shipments. Looking at the country of origin
# information in the database shows that the US is among the top 10 countries
# importing to the US!
count(lemis, country_origin_iso2c, sort = T)
# Currently, this data is still included in the LEMIS database so be careful

# ==============================================================================


# Generate a cleaning notes column to hold automatically generated notes
lemis$cleaning_notes <- rep(NA_character_, nrow(lemis))


# Cleaning of non-standard country origins present in the LEMIS data


summary(lemis$country_origin_iso2c)

valid.country.codes <- read_csv("inst/extdata/iso_2_country_codes.csv") %>%
  pull(Value)
# Add on other valid codes
valid.country.codes <- c(
  valid.country.codes,
  "BL", "BQ", "CW", "GG", "IM", "JE",
  "ME", "MF", "PS", "RS", "SX", "TL",
  "VS", "XX", "ZZ"
)

# Relevel X to XX
lemis$country_origin_iso2c <- plyr::revalue(lemis$country_origin_iso2c, c(X = "XX"))

# Remove non-standard country origins from the analysis
lemis <- lemis %>%
  mutate(
    country_origin_iso2c = as.character(country_origin_iso2c),
    cleaning_notes = case_when(
      !(country_origin_iso2c %in% valid.country.codes) & !is.na(country_origin_iso2c) & is.na(cleaning_notes) ~
      paste0("Original value in country_origin column: ", country_origin_iso2c),
      !(country_origin_iso2c %in% valid.country.codes) & !is.na(country_origin_iso2c) & !is.na(cleaning_notes) ~
      paste0(cleaning_notes, ", country_origin column: ", country_origin_iso2c),
      TRUE ~ cleaning_notes
    ),
    country_origin_iso2c = case_when(
      !(country_origin_iso2c %in% valid.country.codes) & !is.na(country_origin_iso2c) ~ "non-standard value",
      TRUE ~ country_origin_iso2c
    ),
    country_origin_iso2c = as.factor(country_origin_iso2c)
  )

# Assertion for quality checking. All levels should be a valid code
assert_that(all(levels(lemis$country_origin_iso2c) %in%
  c(valid.country.codes, "non-standard value")))

summary(lemis$country_origin_iso2c)


# Cleaning of non-standard country import/export present in the LEMIS data


summary(lemis$country_imp_exp_iso2c)

# Remove non-standard country origins from the analysis
lemis <- lemis %>%
  mutate(
    country_imp_exp_iso2c = as.character(country_imp_exp_iso2c),
    cleaning_notes = case_when(
      !(country_imp_exp_iso2c %in% valid.country.codes) & !is.na(country_imp_exp_iso2c) & is.na(cleaning_notes) ~
      paste0("Original value in country_imp_exp column: ", country_imp_exp_iso2c),
      !(country_imp_exp_iso2c %in% valid.country.codes) & !is.na(country_imp_exp_iso2c) & !is.na(cleaning_notes) ~
      paste0(cleaning_notes, ", country_imp_exp column: ", country_imp_exp_iso2c),
      TRUE ~ cleaning_notes
    ),
    country_imp_exp_iso2c = case_when(
      !(country_imp_exp_iso2c %in% valid.country.codes) & !is.na(country_imp_exp_iso2c) ~ "non-standard value",
      TRUE ~ country_imp_exp_iso2c
    ),
    country_imp_exp_iso2c = as.factor(country_imp_exp_iso2c)
  )

# Assertion for quality checking. All levels should be a valid code
assert_that(all(levels(lemis$country_imp_exp_iso2c) %in%
  c(valid.country.codes, "non-standard value")))

summary(lemis$country_imp_exp_iso2c)


# Cleaning of non-standard descriptions present in the LEMIS data


summary(lemis$wildlife_description)

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

# Remove non-standard descriptions from the analysis
lemis <- lemis %>%
  mutate(
    wildlife_description = as.character(wildlife_description),
    cleaning_notes = case_when(
      !(wildlife_description %in% valid.description.codes) & !is.na(wildlife_description) & is.na(cleaning_notes) ~
      paste0("Original value in description column: ", wildlife_description),
      !(wildlife_description %in% valid.description.codes) & !is.na(wildlife_description) & !is.na(cleaning_notes) ~
      paste0(cleaning_notes, ", description column: ", wildlife_description),
      TRUE ~ cleaning_notes
    ),
    wildlife_description = case_when(
      !(wildlife_description %in% valid.description.codes) & !is.na(wildlife_description) ~ "non-standard value",
      TRUE ~ wildlife_description
    ),
    wildlife_description = as.factor(wildlife_description)
  )

# Assertion for quality checking. All levels should be a valid code
assert_that(all(levels(lemis$wildlife_description) %in%
  c(valid.description.codes, "non-standard value")))

summary(lemis$wildlife_description)


# Cleaning of non-standard ports present in the LEMIS data


summary(lemis$port)

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

# All NA values can be converted to "XX" since that represents unknown port
# of entry
lemis <- lemis %>%
  mutate(
    port = ifelse(is.na(port), "XX", as.character(port)),
    port = as.factor(port)
  )

# Remove non-standard ports from the analysis
lemis <- lemis %>%
  mutate(
    port = as.character(port),
    cleaning_notes = case_when(
      !(port %in% valid.port.codes) & !is.na(port) & is.na(cleaning_notes) ~
      paste0("Original value in port column: ", port),
      !(port %in% valid.port.codes) & !is.na(port) & !is.na(cleaning_notes) ~
      paste0(cleaning_notes, ", port column: ", port),
      TRUE ~ cleaning_notes
    ),
    port = case_when(
      !(port %in% valid.port.codes) & !is.na(port) ~ "non-standard value",
      TRUE ~ port
    ),
    port = as.factor(port)
  )

# Assertion for quality checking. All levels should be a valid code
assert_that(all(levels(lemis$port) %in%
  c(valid.port.codes, "non-standard value")))

summary(lemis$port)


# Cleaning of non-standard units present in the LEMIS data


summary(lemis$unit)

valid.unit.codes <-
  c(
    "C2", "C3", "CM", "GM", "KG", "LT",
    "M2", "M3", "MG", "ML", "MT", "NO"
  )

# The unit "LB" may stand for pounds, so we need to change this to kilograms
# where 1 LB = 0.454 KG
lemis <- lemis %>%
  mutate(
    # backup quantity and unit columns to preserve the originals
    quantity_bkp = quantity,
    unit_bkp = unit,
    # convert pounds to kilograms by dividing the pound units by 0.454
    quantity = case_when(
      unit == "LB" ~ quantity / 0.454,
      TRUE ~ quantity
    ),
    unit = plyr::revalue(unit, c(LB = "KG"))
  )

# All variations of the "number of specimens" entries should be recoded
# as "NO"
lemis$unit <- plyr::revalue(
  lemis$unit,
  c(N = "NO", N0 = "NO", no = "NO", No = "NO")
)

# Remove non-standard units from the analysis
lemis <- lemis %>%
  mutate(
    unit = as.character(unit),
    cleaning_notes = case_when(
      !(unit %in% valid.unit.codes) & !is.na(unit) & is.na(cleaning_notes) ~
      paste0("Original value in unit column: ", unit),
      !(unit %in% valid.unit.codes) & !is.na(unit) & !is.na(cleaning_notes) ~
      paste0(cleaning_notes, ", unit column: ", unit),
      TRUE ~ cleaning_notes
    ),
    unit = case_when(
      !(unit %in% valid.unit.codes) & !is.na(unit) ~ "non-standard value",
      TRUE ~ unit
    ),
    unit = as.factor(unit)
  )

# Assertion for quality checking. All levels should be a valid code
assert_that(all(levels(lemis$unit) %in%
  c(valid.unit.codes, "non-standard value")))

summary(lemis$unit)


# Cleaning of non-standard purposes present in the LEMIS data


summary(lemis$purpose)

valid.purpose.codes <-
  c("B", "E", "G", "H", "L", "M", "P", "Q", "S", "T", "Y", "Z")

# Remove non-standard purposes from the analysis
lemis <- lemis %>%
  mutate(
    purpose = as.character(purpose),
    cleaning_notes = case_when(
      !(purpose %in% valid.purpose.codes) & !is.na(purpose) & is.na(cleaning_notes) ~
      paste0("Original value in purpose column: ", purpose),
      !(purpose %in% valid.purpose.codes) & !is.na(purpose) & !is.na(cleaning_notes) ~
      paste0(cleaning_notes, ", purpose column: ", purpose),
      TRUE ~ cleaning_notes
    ),
    purpose = case_when(
      !(purpose %in% valid.purpose.codes) & !is.na(purpose) ~ "non-standard value",
      TRUE ~ purpose
    ),
    purpose = as.factor(purpose)
  )

# Assertion for quality checking. All levels should be a valid code
assert_that(all(levels(lemis$purpose) %in%
  c(valid.purpose.codes, "non-standard value")))

summary(lemis$purpose)
nrow(lemis)



# Cleaning of non-standard sources present in the LEMIS data


summary(lemis$source_)

valid.source.codes <-
  c("A", "C", "D", "F", "I", "P", "R", "U", "W")

# Relevel lowercase variations of source_ entries
lemis$source_ <- plyr::revalue(lemis$source_, c(w = "W"))

# Remove non-standard sources from the analysis
lemis <- lemis %>%
  mutate(
    source_ = as.character(source_),
    cleaning_notes = case_when(
      !(source_ %in% valid.source.codes) & !is.na(source_) & is.na(cleaning_notes) ~
      paste0("Original value in source column: ", source_),
      !(source_ %in% valid.source.codes) & !is.na(source_) & !is.na(cleaning_notes) ~
      paste0(cleaning_notes, ", source column: ", source_),
      TRUE ~ cleaning_notes
    ),
    source_ = case_when(
      !(source_ %in% valid.source.codes) & !is.na(source_) ~ "non-standard value",
      TRUE ~ source_
    ),
    source_ = as.factor(source_)
  )

# Assertion for quality checking. All levels should be a valid code
assert_that(all(levels(lemis$source_) %in%
  c(valid.source.codes, "non-standard value")))

summary(lemis$purpose)


# Cleaning of non-standard actions present in the LEMIS data


summary(lemis$action)
nrow(lemis)

valid.action.codes <- c("C", "R")

# Assertion for quality checking. All levels should be a valid code
assert_that(all(levels(lemis$action) %in%
  c(valid.action.codes, "non-standard value")))

summary(lemis$action)


# Cleaning of non-standard dispositions present in the LEMIS data


summary(lemis$disposition)

valid.disposition.codes <- c("A", "C", "R", "S")

# Remove non-standard dispositions from the analysis
lemis <- lemis %>%
  mutate(
    disposition = as.character(disposition),
    cleaning_notes = case_when(
      !(disposition %in% valid.disposition.codes) & !is.na(disposition) & is.na(cleaning_notes) ~
      paste0("Original value in disposition column: ", disposition),
      !(disposition %in% valid.disposition.codes) & !is.na(disposition) & !is.na(cleaning_notes) ~
      paste0(cleaning_notes, ", disposition column: ", disposition),
      TRUE ~ cleaning_notes
    ),
    disposition = case_when(
      !(disposition %in% valid.disposition.codes) & !is.na(disposition) ~ "non-standard value",
      TRUE ~ disposition
    ),
    disposition = as.factor(disposition)
  )

# Assertion for quality checking. All levels should be a valid code
assert_that(all(levels(lemis$disposition) %in%
  c(valid.disposition.codes, "non-standard value")))

summary(lemis$disposition)

# ==============================================================================


# Join in taxonomic information and create new variables


# Generate a table of taxa information
taxa_code <- read.csv("inst/extdata/Taxalist_reviewed.csv",
  na.strings = c(" ", "")
) %>%
  select(species_code_taxa = SPEC_CODE, taxa = Taxa) %>%
  mutate(
    species_code_taxa = as.factor(toupper(species_code_taxa)),
    taxa = as.factor(tolower(taxa))
  ) %>%
  distinct()

# Join this table with the LEMIS data
lemis <- lemis %>%
  left_join(taxa_code, by = c("species_code" = "species_code_taxa")) %>%
  mutate(species_code = as.factor(species_code))

# Create variables for Wild/Captive, Live/Not live, and bird|mammal|reptile
# or not
lemis$Wild <- ifelse(lemis$source_ == "W", 1, 0)
lemis$Live <- ifelse(lemis$wildlife_description == "LIV", 1, 0)
lemis$NonAq <- ifelse(lemis$taxa == "bird" | lemis$taxa == "mammal" |
  lemis$taxa == "reptile", 1, 0)

summary(lemis$Wild)
summary(lemis$Live)
summary(lemis$NonAq)

# ==============================================================================


# Data saving and directory cleanup


# Clean column types to ensure good parsing in process_lemis.R
lemis <- lemis %>%
  mutate_at(
    c("control_number", "quantity", "shipment_year", "quantity_bkp"),
    funs(as.integer(.))
  )

# Write a cleaned CSV file of all LEMIS data
write_csv(
  lemis,
  paste0(
    "data-raw/lemis_", min(lemis$shipment_year), "_",
    max(lemis$shipment_year), "_cleaned.csv"
  )
)

# At this point it should be safe to delete subdirectories holding
# intermediate LEMIS files
unlink("data-raw/by_year/", recursive = TRUE)
unlink("data-raw/csv_by_year/", recursive = TRUE)
