# LEMIS data cleaning script to import and harmonize taxonomic information


# Load packages
library(assertthat)
library(dplyr)
library(googlesheets)
library(readr)
library(stringr)
library(taxadb)

h <- here::here
td_create("col")
td_create("itis")

#==============================================================================


# Import intermediate LEMIS data

lemis_intermediate <- read_csv(
  h("data-raw", "lemis_intermediate.csv"),
  col_types = cols(
    .default = col_character(),
    control_number = col_integer(),
    quantity = col_integer(),
    value = col_integer(),
    disposition_date = col_date(format = "%Y-%m-%d"),
    disposition_year = col_integer(),
    shipment_date = col_date(format = "%Y-%m-%d"),
    shipment_year = col_integer()
  )
)

#==============================================================================


# Join in USFWS taxonomic information

# Generate a table of taxa information
taxa_code <- read.csv("inst/extdata/Taxalist_reviewed.csv",
                      na.strings = c(" ", "")
) %>%
  select(
    species_code_taxa = SPEC_CODE,
    taxa = Taxa) %>%
  mutate(
    species_code_taxa = toupper(species_code_taxa),
    taxa = tolower(taxa)
  ) %>%
  distinct()

# Join this table with the LEMIS data
lemis_taxa_added <- lemis_intermediate %>%
  left_join(., taxa_code, by = c("species_code" = "species_code_taxa"))

#==============================================================================


# Modify data entry errors for taxonomic data

lemis_taxa_added <- lemis_taxa_added %>%
  mutate(
    genus = case_when(
      genus == "butterlies" | genus == "butetrflies" ~ "butterflies",
      genus == "saxicola" & generic_name == "CLAM" ~ "saxidomus",
      genus == "sialis" & generic_name == "BLUEBIRD" ~ "sialia",
      genus == "afrilaxus" & generic_name == "FROG" ~ "afrixalus",
      genus == "afrilaxus" & species == "pygmecus" ~ "afrixalus",
      genus == "afrilaxus" & species == "dorsalis" ~ "afrixalus",
      genus == "tompterna" & generic_name == "BULLFROG" ~ "tomopterna",
      genus == "bufo(rhinella)" ~ "bufo",
      genus == "insecta" & str_detect(species, "era$") ~ species,
      genus == "unk" & species == "unk" ~ "unknown",
      TRUE ~ genus
    ),
    species = case_when(
      genus == "afrilaxus" & species == "pygmecus" ~ "pygmaeus",
      genus == "insecta" & str_detect(species, "era$") ~ "sp.",
      genus == "unknown" & species == "unk" ~ "unknown",
      TRUE ~ species
    ),
    generic_name = case_when(
      generic_name == "TURTLES, TORTOISES" ~ "TURTLES,TORTOISES",
      TRUE ~ generic_name
    ),
    taxa = case_when(
      genus == "insecta" & str_detect(species, "era$") ~ "insect",
      TRUE ~ taxa
    )
  )

#==============================================================================


# Generate a table of LEMIS species for which taxonomic information (the
# class of the organism) needs to be gathered

species_to_classify <- lemis_taxa_added %>%
  filter(
    !is.na(genus),
    !is.na(species),
    genus != "noncites entry",
    genus != "none"
  ) %>%
  distinct(genus, species) %>%
  arrange(genus, species) %>%
  mutate(binomial = paste(genus, species))

assert_that(
  nrow(distinct(species_to_classify, genus, species)) ==
    nrow(species_to_classify)
)

#==============================================================================


# Use taxadb functionality to lookup taxonomic information for the species
# using both the Catalogue of Life (COL) and the Integrated Taxonomic
# Information System (ITIS) databases


# Gather COL and ITIS IDs and accepted names
taxatbl1 <- species_to_classify %>%
  mutate(
    col_id = get_ids(binomial, "col"),
    itis_id = get_ids(binomial, "itis"),
    accepted_name_col = get_names(col_id, "col"),
    accepted_name_itis = get_names(itis_id, "itis")
  )

# Gather class information from COL database
taxatbl1 <- taxa_tbl("col") %>%
  select(taxonID, class) %>%
  collect() %>%
  left_join(taxatbl1, ., by = c("col_id" = "taxonID")) %>%
  rename(class_col = class)

# Gather class information from ITIS database
taxatbl1 <- taxa_tbl("itis") %>%
  select(taxonID, class) %>%
  collect() %>%
  left_join(taxatbl1, ., by = c("itis_id" = "taxonID")) %>%
  rename(class_itis = class)

# Create a table that can be used to harmonize the class hierarchy
# adopted by ITIS with that adopted by the COL database
col_itis_harmonize <- taxatbl1 %>%
  filter(!is.na(class_col), !is.na(class_itis)) %>%
  select(class_col, class_itis) %>%
  distinct() %>%
  arrange(class_col)

assert_that(
  nrow(col_itis_harmonize) ==
    n_distinct(col_itis_harmonize$class_itis)
)

col_itis_harmonize <- col_itis_harmonize %>%
  rename(class_itis_harmonize = class_col)

# Harmonize the ITIS and COL class identifications to create a single class
# variable
taxatbl2 <- taxatbl1 %>%
  left_join(., col_itis_harmonize, by = "class_itis") %>%
  mutate(
    class = ifelse(!is.na(class_col), class_col, class_itis_harmonize)
  ) %>%
  select(class, genus, species, binomial)

#==============================================================================


genera_to_resolve <- taxatbl2 %>%
  filter(is.na(class)) %>%
  pull(genus) %>%
  unique(.)

genera_taxa_info <- taxa_tbl("col") %>%
  distinct(class, genus) %>%
  arrange(genus, class) %>%
  filter(genus %in% stringi::stri_trans_totitle(genera_to_resolve)) %>%
  collect() %>%
  filter(
    !is.na(class),
    class != "Not assigned"
  )

ambiguous_genera <- genera_taxa_info %>%
  group_by(genus) %>%
  summarize(n = n_distinct(class)) %>%
  filter(n > 1) %>%
  pull(genus)

unambiguous_genera_taxa_info <- genera_taxa_info %>%
  filter(!(genus %in% ambiguous_genera)) %>%
  mutate(genus = tolower(genus))

taxatbl3 <- taxatbl2 %>%
  left_join(., unambiguous_genera_taxa_info, by = c("genus")) %>%
  mutate(class = ifelse(!is.na(class.x), class.x, class.y)) %>%
  select(class, genus, species)

#==============================================================================


lemis_taxa_added <- lemis_taxa_added %>%
  left_join(., taxatbl3, by = c("genus", "species")) %>%
  mutate(
    class = case_when(

      is.na(class) & taxa == "amphibian" &
        (
          genus == "noncites entry" |
            str_detect(genus, "formes$|idae$|inae$") |
            (is.na(genus) & generic_name == "AMPHIBIANS") |
            (is.na(genus) & generic_name == "ALL AMPHIBIANS") |
            (is.na(genus) & is.na(generic_name))
        )
      ~ "Amphibia",

      is.na(class) & taxa == "bird" &
        (
          genus == "noncites entry" |
            genus == "migratory bird" |
            (genus == "aves" & generic_name == "ALL BIRDS") |
            (genus == "unknown" & species == "cites bird") |
            str_detect(genus, "formes$|idae$|inae$") |
            (is.na(genus) & generic_name == "BIRD") |
            (genus == "unknown" & generic_name == "BIRDS") |
            (is.na(genus) & generic_name == "BIRDS") |
            (genus == "aves" & generic_name == "ALL BIRDS") |
            (is.na(genus) & generic_name == "ALL BIRDS") |
            (is.na(genus) & is.na(generic_name))
        )
      ~ "Aves",

      is.na(class) & taxa == "insect" &
        (
          genus == "noncites entry" |
            str_detect(genus, "formes$|idae$|inae$") |
            (is.na(genus) & generic_name == "BUTTERFLIES") |
            (is.na(genus) & generic_name == "ALL INSECTS") |
            (is.na(genus) & is.na(generic_name))
        )
      ~ "Insecta",

      is.na(class) & taxa == "mammal" &
        (
          genus == "noncites entry" |
            (genus == "mammalia" & generic_name == "ALL MAMMALS") |
            (genus == "unknown" & species == "cites mammal") |
            str_detect(genus, "formes$|idae$|inae$") |
            (is.na(genus) & generic_name == "MAMMALS") |
            (is.na(genus) & generic_name == "ALL MAMMALS") |
            (is.na(genus) & generic_name == "CANIDS") |
            (is.na(genus) & is.na(generic_name))
        )
      ~ "Mammalia",

      is.na(class) & taxa == "reptile" &
        (
          genus == "noncites entry" |
            (genus == "reptilia" & generic_name == "ALL REPTILES") |
            (genus == "sauria" & generic_name == "LIZARDS") |
            (genus == "serpentes" & generic_name == "SNAKES") |
            (genus == "squamata" & generic_name == "LIZARDS,SNAKES") |
            (genus == "testudinata" & generic_name == "TURTLES,TORTOISES") |
            (genus == "testudines" & generic_name == "TURTLES,TORTOISES") |
            str_detect(genus, "formes$|idae$|inae$") |
            (is.na(genus) & generic_name == "REPTILES") |
            (is.na(genus) & generic_name == "ALL REPTILES") |
            (is.na(genus) & is.na(generic_name))
        )
      ~ "Reptilia",

      is.na(class) & taxa == "spider" &
        (
          genus == "noncites entry" |
            str_detect(genus, "formes$|idae$|inae$") |
            (is.na(genus) & generic_name == "ALL TARANTULAS") |
            (is.na(genus) & is.na(generic_name))
        )
      ~ "Arachnida",

      TRUE ~ class
    )
  )

unclassified <- lemis_taxa_added %>%
  filter(is.na(class)) %>%
  distinct(genus, species, generic_name, specific_name, generic_name, taxa) %>%
  arrange(taxa, genus, species)

write_csv(unclassified, h("data-raw", "lemis_taxa_unclassified.csv"))

#==============================================================================


manual_tax <- gs_title("LEMIS manual taxonomy harmonization") %>%
  gs_read()

lemis_taxa_added <- lemis_taxa_added %>%
  left_join(., manual_tax,
            by = c("genus", "species", "generic_name",
                   "specific_name", "taxa")
  ) %>%
  mutate(class = ifelse(!is.na(class.x), class.x, class.y)) %>%
  select(-class.x, -class.y)

error_checking <- lemis_taxa_added %>%
  filter(!is.na(taxa)) %>%
  group_by(class, taxa) %>%
  count() %>%
  arrange(class)

#==============================================================================


# Data saving

lemis_to_save <- lemis_taxa_added %>%
  # select final columns to keep
  select(
    control_number,
    taxa,
    species_code,
    class,
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
  arrange(shipment_date, control_number)

# Write a CSV file of final LEMIS data
write_csv(
  lemis_to_save,
  h("data-raw",
    paste0("lemis_",
           min(lemis_to_save$shipment_year, na.rm = TRUE), "_",
           max(lemis_to_save$shipment_year, na.rm = TRUE), "_cleaned.csv")
  )
)