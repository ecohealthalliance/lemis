# LEMIS data cleaning script based on WildDB script originally written
# by Allison White


# Load packages
library(assertthat)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)

h <- here::here
source(h("data-raw", "R", "lemis_cleaning_functions.R"))

#==============================================================================


# Merge all LEMIS data


# Merge yearly LEMIS CSV files into one data frame
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
  "foreign_co",
  "file_num"
)

yearly.files <- dir(path = h("data-raw", "csv_by_year"), full.names = TRUE)
lemis_raw <- data.frame()

for (file in yearly.files) {

  print(file)

  lemis_raw <-
    rbind(
      lemis_raw,
      read_csv(file,
               col_names = lemis.cols,
               skip = 1,
               col_types = cols(.default = col_character()),
               na = character()
      )
    )
}

# Generate values for NA checking
periods <- sapply(1:10, function(x) paste0(rep(".", x), collapse = ""))
asterisks <- sapply(1:100, function(x) paste0(rep("*", x), collapse = ""))
dashes <- sapply(1:10, function(x) paste0(rep("-", x), collapse = ""))
slashes <- sapply(1:10, function(x) paste0(rep("/", x), collapse = ""))

na.characters <- c(
  periods, asterisks, dashes, slashes,
  "", " ", "NA", "N/A", "NULL",
  "*8", "*****8", "`*", "*`", "**`", "******`"
)

# Convert NA values
lemis <- lemis_raw %>%
  mutate_all(
    funs(if_else(. %in% na.characters, NA_character_, .))
  )

# Eliminate rows of all NA values
lemis <- lemis[apply(select(lemis, -file_num), 1, function(x) any(!is.na(x))), ]
# (should eliminate 28 rows with all missing data)

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

# Replace empty strings in hidden.rows (i.e., "" or " ") with NA values
hidden.rows <- hidden.rows %>%
  mutate_at(
    vars(colnames(.)),
    funs(ifelse(. == "" | . == " ", NA_character_, .))
  ) %>%
  # And indicate "file_num" is 1 for these records
  mutate(file_num = "1")

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

#==============================================================================


# Further data filtering


# 1)
# What proportion of the data does not have a value of "I" in the
# import_export column?
nrow(lemis[lemis$import_export != "I", ])/nrow(lemis)
# Remove any "E", "T", and NA records from the import_export column,
# leaving only importation data
lemis <- lemis %>%
  filter(import_export != "E", import_export != "T", !is.na(import_export))

assert_that(sum(lemis$import_export == "I") == nrow(lemis))


# 2)
# Identify problematic records that have NA values for "value" and are
# otherwise exact duplicates of other records
grouping.vars <- colnames(lemis)[-c(11, 24)]

problem.row.set <- lemis %>%
  group_by_(.dots = grouping.vars) %>%
  summarize(
    row.count = n(),
    NA.count = sum(is.na(value)),
    distinct_value_values = paste(unique(value), collapse = ", "),
    distinct_file_num_values = paste(unique(file_num), collapse = ", ")
  ) %>%
  filter(row.count > 1) %>%
  ungroup()

dups <- problem.row.set %>%
  filter(NA.count > 0,
         str_detect(distinct_value_values, ",")) %>%
  mutate(value = NA_real_,
         file_num = str_extract(distinct_file_num_values, "."))

dup.years <- dups %>%
  pull(shipment_date) %>%
  str_extract(., "20.{1,2}") %>%
  unique(.)

# Verify all of these records come from 2013 data
assert_that(dup.years == "2013")

# Remove the duplicate, NA-containing records from the data
lemis.row.count <- nrow(lemis)
dups.NA.count <- sum(dups$NA.count)
lemis <- anti_join(lemis, dups, by = lemis.cols)
assert_that(nrow(lemis) == (lemis.row.count - dups.NA.count))


# 3)
# Address potential exact duplicate records
grouping.vars2 <- colnames(lemis)[-24]

problem.row.set2 <- lemis %>%
  group_by_(.dots = grouping.vars2) %>%
  summarize(
    row.count = n(),
    distinct_file_num_values = paste(unique(file_num), collapse = ", ")
  ) %>%
  filter(row.count > 1) %>%
  ungroup()

# How many of these "duplicate" records are in fact from the same original
# data file, indicating they are probably not errors generated from
# collating together multiple data sheets within years?
from.same.file.count <-
  sum(!str_detect(problem.row.set2$distinct_file_num_values, ","))

assert_that(from.same.file.count == nrow(problem.row.set2))
# Since duplicate records of a given record all come from the same
# original data file, it's probably best to keep them and treat them
# as intentionally duplicated


# 4)
# Issue of US to US importation shipments. Looking at the country of origin
# information in the database shows that the US is among the top 10 countries
# importing to the US!
count(lemis, country_origin, sort = TRUE)
# Currently, this data is still included in the LEMIS database, so be careful

#==============================================================================


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
      # Change "GMT" to "GAR", under the assumption this was meant to indicate
      # "garment"
      description == "GMT" ~ "GAR",
      # Change "JEW" to "JWL" when the foreign company is an arts and crafts
      # dealer, under the assumption this was meant to indicate "jewelry"
      description == "JEW" & foreign_co == "BAYEAD ARTS AND CRAFTS" ~ "JWL",
      # Change "LI" to "LIV"
      description == "LI" ~ "LIV",
      # Change "MAE" to "MEA" when the unit is a weight, under the assumption
      # this was meant to indicate "meat"
      description == "MAE" & unit == "KG" ~ "MEA",
      # Change "SK" to "SKI" when the foreign company is "BASS RIVER FARMS",
      # under the assumption this was meant to indicate "skin"
      description == "SK" & foreign_co == "BASS RIVER FARMS" ~ "SKI",
      # Change "SP" and "SPW" to "SPR" when the generic name is either "MOLLUSC",
      # "CLAM", or "SHELL" and unit is "NO", under the assumption this
      # was meant to indicate a shell product
      description == "SP" & generic_name %in% c("MOLLUSC", "CLAM", "SHELL") &
        unit == "NO" ~ "SPR",
      description == "SPW" & generic_name %in% c("MOLLUSC", "CLAM", "SHELL") &
        unit == "NO" ~ "SPR",
      # Change "TWO" to "TRO" when species_code is "ELAN", under the
      # assumption this was meant to indicate a trophy
      description == "TWO" & species_code == "ELAN" ~ "TRO",
      TRUE ~ description
    )
  )

# Which values are not in the valid codes?
index.invalid.codes <- !(unique(lemis$description) %in% valid.description.codes)
sort(unique(lemis$description)[index.invalid.codes])

# Remove remaining non-standard descriptions
lemis <- get_cleaned_lemis("description", valid.description.codes)

# Assertion for quality checking. All levels should be a valid code
assert_that(all(levels(lemis$description) %in%
  c(valid.description.codes, "non-standard value")))

summary(lemis$description)

#==============================================================================


# Cleaning of non-standard units present in the LEMIS data


sort(unique(lemis$unit))

valid.unit.codes <-
  c("C2", "C3", "CM", "GM", "KG", "LT", "M2", "M3", "MG", "ML", "MT", "NO")

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

lemis <- lemis %>%
  mutate(unit = case_when(
    # All variations of the "number of specimens" entries should be recoded
    # as "NO" ("CT" is likely "count" while "PC" is likely "piece")
    unit %in% c("CT", "N", "N0", "PC") ~ "NO",
    # Recode "L" as "liters"
    unit == "L" ~ "LT",
    TRUE ~ unit
    )
  )

# Which values are not in the valid codes?
index.invalid.codes <- !(unique(lemis$unit) %in% valid.unit.codes)
sort(unique(lemis$unit)[index.invalid.codes])

# Remove remaining non-standard units
lemis <- get_cleaned_lemis("unit", valid.unit.codes)

# Assertion for quality checking. All levels should be a valid code
assert_that(all(levels(lemis$unit) %in%
  c(valid.unit.codes, "non-standard value")))

summary(lemis$unit)

#==============================================================================


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
      # Change "1D" to "ID", indicating Indonesia
      country_origin == "1D" & country_imp_exp == "ID" ~ "ID",
      # Change "FP" to "PF", indicating French Polynesia
      country_origin == "FP" ~ "PF",
      # Change "UK" to "GB", indicating the United Kingdom
      country_origin == "UK" & country_imp_exp == "GB" ~ "GB",
      # Change "X" to "XX"
      country_origin == "X" ~ "XX",
      # Change NA values to "XX" since that represents unknown country
      is.na(country_origin) ~ "XX",
      TRUE ~ country_origin
    )
  )

# Which values are not in the valid codes?
index.invalid.codes <- !(unique(lemis$country_origin) %in% valid.country.codes)
sort(unique(lemis$country_origin)[index.invalid.codes])

# Remove remaining non-standard country origins
lemis <- get_cleaned_lemis("country_origin", valid.country.codes)

# Assertion for quality checking. All levels should be a valid code
assert_that(all(levels(lemis$country_origin) %in%
  c(valid.country.codes, "non-standard value")))

summary(lemis$country_origin)

#==============================================================================


# Cleaning of non-standard country import/export present in the LEMIS data


sort(unique(lemis$country_imp_exp))

# Which values are not in the valid codes?
index.invalid.codes <- !(unique(lemis$country_imp_exp) %in% valid.country.codes)
sort(unique(lemis$country_imp_exp)[index.invalid.codes])

# Convert irregular country_origin values to good values
lemis <- lemis %>%
  mutate(
    country_imp_exp = case_when(
      # Change "**" to "XX" since that represents unknown country
      str_detect(country_imp_exp, fixed("**", TRUE)) ~ "XX",
      # Change NA values to "XX" since that represents unknown country
      is.na(country_imp_exp) ~ "XX",
      TRUE ~ country_imp_exp
    )
  )

# Remove remaining non-standard country import/export
lemis <- get_cleaned_lemis("country_imp_exp", valid.country.codes)

# Assertion for quality checking. All levels should be a valid code
assert_that(all(levels(lemis$country_imp_exp) %in%
  c(valid.country.codes, "non-standard value")))

summary(lemis$country_imp_exp)

#==============================================================================


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

#==============================================================================


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

#==============================================================================


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

#==============================================================================


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

#==============================================================================


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


# Clean "disposition_date" column


# Verify that the vast majority of disposition dates occur on or after the
# shipment date
filter(lemis, !is.na(shipment_date) & !is.na(disposition_date)) %>%
  mutate(date_test = (disposition_date - shipment_date) >= 0) %>%
  summarize(sum(date_test)/n())

# Clean disposition dates
lemis <- lemis %>%
  mutate(
    disposition_date_original_value = disposition_date,
    # extract only the year component of the disposition and shipment dates
    disposition_year = format(disposition_date, "%Y"),
    shipment_year = format(shipment_date, "%Y"),
    disposition_date = case_when(
      # clean cases where "disposition_date" is far later than "shipment_date"
      shipment_date == "2012-02-13" & disposition_date == "2016-02-16" ~
        "2012-02-16",
      shipment_date == "2014-06-15" & disposition_date == "2017-06-17" ~
        "2014-06-17",
      shipment_date == "2014-12-17" & disposition_date == "2019-12-17" ~
        "2014-12-17",
      shipment_year == "2002" & disposition_year == "2020" ~
        str_replace(disposition_date, "2020", "2002"),
      shipment_date == "2002-03-27" & disposition_date == "2027-03-27" ~
        "2002-03-27",
      shipment_year == "2003" & disposition_year == "2030" ~
        str_replace(disposition_date, "2030", "2003"),
      shipment_date == "2002-11-27" & disposition_date == "2030-02-06" ~
        "2003-02-06",
      shipment_year == "2003" & disposition_year == "2033" ~
        str_replace(disposition_date, "2033", "2003"),
      shipment_year == "2004" & disposition_year == "2044" ~
        str_replace(disposition_date, "2044", "2004"),
      shipment_year == "2001" & disposition_year == "2201" ~
        str_replace(disposition_date, "2201", "2001"),
      shipment_year == "2002" & disposition_year == "2202" ~
        str_replace(disposition_date, "2202", "2002"),
      shipment_year == "2001" & disposition_year == "2991" ~
        str_replace(disposition_date, "2991", "2001"),
      shipment_year == "2003" & disposition_year == "3003" ~
        str_replace(disposition_date, "3003", "2003"),
      shipment_year == "2004" & disposition_year == "3004" ~
        str_replace(disposition_date, "3004", "2004"),
      shipment_date == "2003-12-28" & disposition_date == "5004-01-06" ~
        "2004-01-06",
      # clean cases where "disposition_date" is far earlier than "shipment_date"
      disposition_date == "1900-01-01" ~ NA_character_,
      disposition_date == "1933-07-15" ~ NA_character_,
      (shipment_year == "2003" | shipment_year == "2004") &
        disposition_year == "1996" ~ NA_character_,
      shipment_date == "2002-07-23" & disposition_date == "1954-07-26" ~
        "2002-07-26",
      shipment_date == "2002-07-24" & disposition_date == "1954-07-26" ~
        "2002-07-26",
      shipment_date == "2002-01-07" & disposition_date == "1992-01-11" ~
        "2002-01-11",
      shipment_date == "2000-02-29" & disposition_date == "1996-03-04" ~
        "2000-03-04",
      shipment_year == "2000" & disposition_year == "1998" ~
        str_replace(disposition_date, "1998", "2000"),
      # keep all others
      TRUE ~ as.character(disposition_date)
    )
  )

date.check.file <- read_csv("data-raw/data/disposition_date_check.csv") %>%
  mutate_at(c("disposition_date", "shipment_date",
              "replacement_disposition_date"),
            funs(as.character(as.Date(., format = "%m/%d/%y")))) %>%
  filter(!is.na(replacement_disposition_date))

for(i in 1:nrow(date.check.file)) {

  lemis[which(lemis$disposition_date == date.check.file$disposition_date[i] &
                lemis$shipment_date == date.check.file$shipment_date[i]),
        "disposition_date"] <-
    date.check.file$replacement_disposition_date[i]
}

#==============================================================================


# Data saving

na.characters <- c("na", "?", ".", "/", "`", "=", "-")

lemis_intermediate <- lemis %>%
  # extract only the year component of the disposition and shipment dates
  mutate(
    disposition_date = as.Date(disposition_date, format = "%Y-%m-%d"),
    shipment_date = as.Date(shipment_date, format = "%Y-%m-%d"),
    disposition_year = as.numeric(format(disposition_date, "%Y")),
    shipment_year = as.numeric(format(shipment_date, "%Y"))
  ) %>%
  # select columns to keep
  select(
    control_number,
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
    cleaning_notes
  ) %>%
  # change column types
  mutate_all(funs(as.character(.))) %>%
  mutate_at(
    c("control_number",
      "quantity",
      "value",
      "disposition_year",
      "shipment_year"),
    funs(as.integer(.))
  ) %>%
  # clean remaining values that should be NA characters
  mutate_if(
    is.character,
    funs(if_else(. %in% na.characters, NA_character_, .))
  ) %>%
  arrange(shipment_date, control_number)

# Write a cleaned CSV file of intermediate LEMIS data
write_csv(
  lemis_intermediate,
  h("data-raw", "lemis_intermediate.csv")
)

#==============================================================================


# Directory cleanup

# Delete subdirectories containing intermediate LEMIS files, if desired
# unlink(h("data-raw", "by_year"), recursive = TRUE)
# unlink(h("data-raw", "csv_by_year"), recursive = TRUE)
