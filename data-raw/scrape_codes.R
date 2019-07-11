# Script to scrape the field codes from the PDFs provided by USFWS (in the
# inst/extdata directory), harmonize and save as package data

library(tabulizer)
library(dplyr)
library(stringi)
h <- here::here
cf2009 <- h("inst", "extdata", "Import_Export_General_Keys_Nov_2009.pdf")
cf2013 <- h("inst", "extdata", "Import_Export_General_Keys_Feb_2013.pdf")

# dput(locate_areas(cf2009, pages=1))
desc_2009_1 <- list(structure(c(
  52.8637715590013, 36.953717815438, 386.312176777281,
  741.8239141251
), .Names = c("top", "left", "bottom", "right")))
desc_2009_raw <- extract_tables(cf2009, pages = 1, area = desc_2009_1)
desc_2009 <- data_frame(
  year = "2009",
  field = "description",
  code = c(desc_2009_raw[[1]][, 1], desc_2009_raw[[1]][, 3]),
  value = c(desc_2009_raw[[1]][, 2], desc_2009_raw[[1]][, 4])
)

# dput(locate_areas(cf2009, pages=1))
ports_2009_1 <- list(structure(c(
  395.461675700951, 47.642558006804, 511.355328734131,
  627.39096358701
), .Names = c("top", "left", "bottom", "right")))
ports_2009_raw <- extract_tables(cf2009, pages = 1, area = ports_2009_1)
ports_2009 <- data_frame(
  year = "2009",
  field = "port",
  code = c(ports_2009_raw[[1]][-1, c(2, 4, 6, 8, 10)]),
  value = c(ports_2009_raw[[1]][-1, c(3, 5, 7, 9, 11)])
) %>%
  mutate(value = stri_extract_last_regex(value, "(?<=-).+$")) %>%
  filter(!(is.na(code) | code == "")) %>%
  mutate(code = stri_replace_all_regex(code, "^0(\\d)$", "$1"))

# dput(locate_areas(cf2009, pages=1))
unit_2009_1 <- list(structure(c(
  517.45499468325, 50.21604706872, 551.003157403381,
  350.23460921035
), .Names = c("top", "left", "bottom", "right")))
unit_2009_raw <- extract_tables(cf2009, pages = 1, area = unit_2009_1)
unit_2009 <- data_frame(
  year = "2009",
  field = "unit",
  code = c(unit_2009_raw[[1]][-1, c(2, 4, 6)]),
  value = c(unit_2009_raw[[1]][-1, c(3, 5, 7)])
)

# dput(locate_areas(cf2009, pages=1))

purpose_2009_1 <- list(structure(c(
  561.264914613467, 49.500019073486, 599.498439766114,
  532.12520503998
), .Names = c("top", "left", "bottom", "right")))
purpose_2009_raw <- extract_tables(cf2009, pages = 1, area = purpose_2009_1)
purpose_2009 <- data_frame(
  year = "2009",
  field = "purpose",
  code = c(purpose_2009_raw[[1]][-1, c(2, 4, 6)]),
  value = c(purpose_2009_raw[[1]][-1, c(3, 5, 7)])
)

# dput(extract_areas(cf2009, pages=2))

source_2009_raw <- list(structure(c(
  "SOURCE", "", "", "", "", "", "", "", "", "",
  "", "A", "C", "D", "F", "I", "P", "R", "U", "W", "", "Plants that are artificially propagated, parts and derivatives",
  "Animals bred in captivity, parts and derivatives", "Appendix-I animals bred in captivity for commercial purposes and Appendix-I plants artificially propagated for commercial purposes, as well as parts and derivatives thereof, exported under the provisions of Article VII, paragraph 4 of the Convention",
  "Animals born in captivity (F1 or subsequent generations) that do not fulfill the definition of \"bred in captivity\" in Resolution Conf. 10.16, as well as parts and derivatives thereof",
  "Confiscated or seized specimens", "Pre-Convention", "Specimens originating from a ranching operation",
  "Source unknown (must be justified)", "Specimens taken from the wild"
), .Dim = c(10L, 3L)))

source_2009 <- data_frame(
  year = "2009",
  field = "source",
  code = c(source_2009_raw[[1]][-1, 2]),
  value = c(source_2009_raw[[1]][-1, 3])
)

# dput(extract_areas(cf2009, pages=2))
action_2009_raw <- list(structure(c("C", "R", "Cleared", "Refused"), .Dim = c(
  2L,
  2L
)))
action_2009 <- data_frame(
  year = "2009",
  field = "action",
  code = c(action_2009_raw[[1]][, 1]),
  value = c(action_2009_raw[[1]][, 2])
)

# dput(extract_areas(cf2009, pages=2))
disposition_2009_raw <- list(structure(c(
  "A", "C", "R", "S", "Abandoned", "Cleared",
  "Reexport", "Seized"
), .Dim = c(4L, 2L)))
disposition_2009 <- data_frame(
  year = "2009",
  field = "disposition",
  code = c(disposition_2009_raw[[1]][, 1]),
  value = c(disposition_2009_raw[[1]][, 2])
)

# dput(locate_areas(cf2009))
country_2009_2 <- list(NULL, structure(c(
  200.817046665151, 46.248193002965, 582.249402578312,
  623.19440071496
), .Names = c("top", "left", "bottom", "right")))
country_2009_raw <- extract_tables(cf2009, area = country_2009_2)
country_2009_raw[[2]][, c(4, 5)] <- country_2009_raw[[2]][, 4] %>%
  stri_split_regex(" (?=[A-Z]{2}\\-)", simplify = TRUE)
country_2009 <- data_frame(
  year = "2009",
  field = "country",
  code = c(country_2009_raw[[2]][-1, c(2, 3, 4, 5, 6)])
) %>%
  tidyr::separate(code, into = c("code", "value"), sep = "-", extra = "merge", fill = "right") %>%
  filter(!is.na(value))

codes_2009 <- bind_rows(
  desc_2009,
  ports_2009,
  unit_2009,
  purpose_2009,
  source_2009,
  action_2009,
  disposition_2009,
  country_2009
)

# dput(locate_areas(cf2013, pages=1))
desc_2013_1 <- list(structure(c(
  54.74760383387, 25.691146589259, 297.20127795527,
  762.40058055152
), .Names = c("top", "left", "bottom", "right")))
desc_2013_raw <- extract_tables(cf2013, pages = 1, area = desc_2013_1)
desc_2013_raw[[1]][6, 6] <- paste(desc_2013_raw[[1]][c(6, 7), 6], collapse = " ")
desc_2013_raw[[1]][7, 6] <- ""
desc_2013 <- data_frame(
  year = "2013",
  field = "description",
  code = c(desc_2013_raw[[1]][, c(1, 3, 5)]),
  value = c(desc_2013_raw[[1]][, c(2, 4, 6)])
) %>%
  filter(code != "")
# Manually add the last entry of the 2013 descriptions table that's missed by
# the extract_tables() call above
desc_2013 <- rbind(
  desc_2013,
  c(
    "2013", "description",
    "WPR", "Wood product (including furniture, rainsticks)"
  )
)

# dput(locate_areas(cf2013, pages=1))
port_2013_1 <- list(structure(c(
  305.02236421725, 47.186647314948, 383.23322683706,
  733.0885341074
), .Names = c("top", "left", "bottom", "right")))
port_2013_raw <- extract_tables(cf2013, pages = 1, area = port_2013_1)
port_2013 <- data_frame(
  year = "2013",
  field = "port",
  code = c(port_2013_raw[[1]][-1, -1])
) %>%
  filter(code != "") %>%
  tidyr::separate(code, into = c("code", "value"), sep = "-", extra = "merge", fill = "right") %>%
  mutate(code = stri_replace_all_regex(code, "^0(\\d)$", "$1"))


# dput(extract_areas(cf2013, pages=1))
unit_2013_raw <- list(structure(c(
  "C2", "C3", "CM", "Square Centimeter", "Cubic Centimeters",
  "Centimeters", "GM", "KG", "LT", "Grams", "Kilograms", "Liters",
  "M2", "M3", "MG", "Square Meters", "Cubic Meters", "Milligrams",
  "ML", "MT", "NO", "Milliliters", "Meters", "Number of Specimens"
), .Dim = c(3L, 8L)))
unit_2013 <- data_frame(
  year = "2013",
  field = "unit",
  code = c(unit_2013_raw[[1]][, c(1, 3, 5, 7)]),
  value = c(unit_2013_raw[[1]][, c(2, 4, 6, 8)])
)

# dput(extract_areas(cf2013, pages=1))
purpose_2013_raw <- list(structure(c(
  "B", "E", "G", "H", "Breeding in captivity or artificial propagation",
  "Educational", "Botanic gardens", "Hunting Trophies", "L", "M",
  "P", "Q", "Law Enforcement / Judicial / Forensic use only", "Biomedical research",
  "Personal", "Circuses/traveling exhibitions", "S", "T", "Y",
  "Z", "Scientific", "Commercial", "Reintroduction/introduction into the wild",
  "Zoos"
), .Dim = c(4L, 6L)))
purpose_2013 <- data_frame(
  year = "2013",
  field = "purpose",
  code = c(purpose_2013_raw[[1]][, c(1, 3, 5)]),
  value = c(purpose_2013_raw[[1]][, c(2, 4, 6)])
)

# dput(extract_areas(cf2013, pages=1))
source_2013_raw <- list(structure(c(
  "A", "C", "D", "Plants that are artificially propagated, parts and derivatives",
  "Animals bred in captivity, parts and derivatives", "Commercially bred",
  "F", "I", "P", "Animals born in captivity, Resolution conf. 10.16",
  "Confiscated or seized specimen", "Pre-convention", "R", "U",
  "W", "Specimens originating from a ranching operation", "Source unknown (must be justified)",
  "Specimens taken from the wild"
), .Dim = c(3L, 6L)))
source_2013 <- data_frame(
  year = "2013",
  field = "source",
  code = c(source_2013_raw[[1]][, c(1, 3, 5)]),
  value = c(source_2013_raw[[1]][, c(2, 4, 6)])
)

# dput(extract_areas(cf2013, pages=1))
action_2013 <- structure(c("C", "R", "Cleared", "Refused"), .Dim = c(
  2L,
  2L
)) %>%
  as.data.frame() %>%
  rename(code = V1, value = V2) %>%
  mutate(year = "2013", field = "action") %>%
  select(year, field, code, value)

# dput(extract_areas(cf2013, pages=1))

disposition_2013_raw <- structure(c(
  "A", "C", "Abandoned", "Cleared", "R", "S",
  "Reexport", "Seized"
), .Dim = c(2L, 4L))
disposition_2013 <- data_frame(
  year = "2013",
  field = "disposition",
  code = c(disposition_2013_raw[, c(1, 3)]),
  value = c(disposition_2013_raw[, c(2, 4)])
)

country_2013_raw <- extract_tables(cf2013, pages = 2)
country_2013 <- data_frame(
  year = "2013",
  field = "country",
  code = c(country_2013_raw[[1]])
) %>%
  filter(code != "") %>%
  tidyr::separate(code, into = c("code", "value"), sep = "-", extra = "merge", fill = "right")

codes_2013 <- bind_rows(
  desc_2013,
  port_2013,
  unit_2013,
  purpose_2013,
  source_2013,
  action_2013,
  disposition_2013,
  country_2013
)

# daff::render_diff(daff::diff_data(codes_2009[,2:4], codes_2013[,2:4]))
lemis_codes_ <- full_join(
  select(codes_2009, -year), select(codes_2013, -year),
  by = c("field", "code")
) %>%
  mutate_all(stri_trim_both) %>%
  filter(value.x != "Ivory Coast") %>%
  filter(!(code == "GB" & !(value.x == "United Kingdom" & value.y == "United Kingdom"))) %>%
  filter(!(code == "ES" & !(value.x == "Spain" & value.y == "Spain"))) %>%
  filter(!(code == "AN" & value.x == "Curacao")) %>%
  mutate(
    value = ifelse(is.na(value.y), value.x, value.y),
    value = case_when(
      # clean country codes
      field == "country" & code == "AN" ~ "Curacao / Netherlands Antilles",
      field == "country" & value == "Cocos (keeling) Islands" ~ "Cocos (Keeling) Islands",
      field == "country" & value == "Saint Helena, Ascension, and Tristan da" ~ "Saint Helena, Ascension, and Tristan da Cunha",
      field == "country" & value == "Saint Vincent & The" ~ "Saint Vincent & The Grenadines",
      # clean description codes
      field == "description" & value == "Eggshell - raw or unworked" ~ "Eggshell (raw or unworked)",
      field == "description" & value == "Fin - (fresh, frozen or dried fins or parts)" ~ "Fin (fresh, frozen or dried fins or parts)",
      field == "description" & value == "Root, (dead)" ~ "Root (dead)",
      field == "description" & value == "Scale ( turtle, other reptile, fish, pangolin)" ~ "Scale (turtle, other reptile, fish, pangolin)",
      # clean port codes
      field == "port" & value == "Miami. FL" ~ "Miami",
      field == "port" & value == "Minneapolis./St. Paul" ~ "Minneapolis/St. Paul",
      # clean unit codes
      field == "unit" & value == "Square Centimeter" ~ "Square Centimeters",
      TRUE ~ value
    ),
    post_feb_2013 = !(is.na(value.y) | value.y == "")
  ) %>%
  select(-value.x, -value.y) %>%
  arrange(field, code)

# Add additional country codes used in the data but not described in the
# metadata
lemis_codes_ <- lemis_codes_ %>%
  bind_rows(.,
            data.frame(
              field = c("country", "country", "country"),
              code = c("PC", "YU", "ZR"),
              value = c("Pacific Islands (Trust Territory)", "Yugoslavia", "Zaire"),
              post_feb_2013 = c(NA_character_, NA_character_, NA_character_)
            )
  ) %>%
  arrange(field, code)


lemis_metadata_ <- dplyr::tribble(
  ~field_name, ~description,
  "control_number", "Shipment ID number",
  "species_code", "USFWS code for the wildlife product",
  "taxa", "USFWS-derived broad taxonomic categorization",
  "class", "EHA-derived class-level taxonomic designation",
  "genus", "Genus (or higher-level taxonomic name) of the wildlife product",
  "species", "Species of the wildlife product",
  "subspecies", "Subspecies of the wildlife product",
  "specific_name", "A specific common name for the wildlife product",
  "generic_name", "A general common name for the wildlife product",
  "description", "Type/form of the wildlife product (see lemis_codes())",
  "quantity", "Numeric quantity of the wildlife product",
  "unit", "Unit for the numeric quantity (see lemis_codes())",
  "value", "Reported value of the wildlife product in US dollars",
  "country_origin", "Code for the country of origin of the wildlife product (see lemis_codes())",
  "country_imp_exp", "Code for the country to/from which the wildlife product is shipped (see lemis_codes())",
  "purpose", "The reason the wildlife product is being imported (see lemis_codes())",
  "source", "The type of source within the origin country (e.g., wild, bred; see lemis_codes())",
  "action", "Action taken by USFWS on import ((C)leared/(R)efused)",
  "disposition", "Fate of the import (see lemis_codes())",
  "disposition_date", "Full date when disposition occurred",
  "disposition_year", "Year when disposition occurred (derived from 'disposition_date')",
  "shipment_date", "Full date when the shipment arrived",
  "shipment_year", "Year when the shipment arrived (derived from 'shipment_date')",
  "import_export", "Whether the shipment is an (I)mport or (E)xport",
  "port", "Port or region of shipment entry (see lemis_codes())",
  "us_co", "US party of the shipment",
  "foreign_co", "Foreign party of the shipment",
  "cleaning_notes", "Notes generated during data cleaning"
)

devtools::use_data(lemis_codes_, lemis_metadata_, internal = TRUE, overwrite = TRUE)
