# LEMIS data cleaning script to import and harmonize taxonomic information


# Load packages
library(assertthat)
library(dplyr)
library(googlesheets4)
library(readr)
library(stringr)
library(taxadb)

h <- here::here
source(h("data-raw", "R", "lemis_cleaning_functions.R"))

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


# Resolve cases where there is a species code provided but no further
# taxonomic information

species_code_no_taxonomic_info <-
  read_csv(
    h("data-raw", "data", "species_code_no_taxonomic_info.csv"),
    col_types = cols(.default = col_character())
  ) %>%
  mutate(
    new_genus = tolower(new_genus),
    new_species = tolower(new_species),
    new_subspecies = tolower(new_subspecies)
  )

lemis_taxa_added <- lemis_intermediate %>%
  left_join(
    ., species_code_no_taxonomic_info,
    by = c("species_code", "genus", "species",
           "subspecies", "specific_name", "generic_name")
  ) %>%
  mutate(
    genus =
      ifelse(!is.na(new_genus), new_genus, genus),
    species =
      ifelse(!is.na(new_species), new_species, species),
    subspecies =
      ifelse(!is.na(new_subspecies), new_subspecies, subspecies),
    specific_name =
      ifelse(!is.na(new_specific_name), new_specific_name, specific_name),
    generic_name =
      ifelse(!is.na(new_generic_name), new_generic_name, generic_name)
  ) %>%
  select(
    -c(new_genus, new_species, new_subspecies,
       new_specific_name, new_generic_name)
  )

#==============================================================================


# Join in broad USFWS taxonomic information

# Generate a table of taxa information
taxa_code <-
  read_csv("inst/extdata/Taxalist_reviewed.csv", na = c(" ", "")) %>%
  select(
    species_code_taxa = SPEC_CODE,
    taxa = Taxa
  ) %>%
  mutate(
    species_code_taxa = toupper(species_code_taxa),
    taxa = tolower(taxa)
  ) %>%
  distinct() %>%
  # Filter out species_code values that lead to erroneous taxa calls
  filter(
    !(species_code_taxa %in%
        c("AGS?", "AMCY", "HSQU", "HTS?", "MEGM", "PLOM"))
  )

# Join this table with the LEMIS data
lemis_taxa_added <- lemis_taxa_added %>%
  left_join(., taxa_code, by = c("species_code" = "species_code_taxa"))

#==============================================================================


# Initial cleanup of taxonomic data fields

# Manual formatting/stylistic changes
lemis_taxa_added <- lemis_taxa_added %>%
  mutate(
    # get rid of end of line period characters in the genus field
    genus = str_replace(genus, "\\.$", ""),
    # convert relevant values in the species column to "sp."
    species = case_when(
      species %in%
        c("?", "species", "sp", "spp", "spp.", "undescribed sp.",
          "unknown", "(genus)") ~ "sp.",
      !is.na(genus) & is.na(species) ~ "sp.",
      TRUE ~ species
    ),
    species = str_replace_all(species, fixed(" spp"), " sp"),
    # convert relevant values in the subspecies column to "sp."
    subspecies = case_when(
      subspecies == "ssp." ~ "sp.",
      TRUE ~ subspecies
    )
  )

# Import taxonomic corrections
taxonomic_corrections <-
  read_csv(
    h("data-raw", "data", "taxonomic_corrections.csv"),
    col_types = cols(.default = col_character())
  ) %>%
  mutate_all(list(~ str_replace_all(., "\\s", " "))) %>%
  mutate(cleaning_notes = NA_character_) %>%
  get_taxonomic_cleaning_notes()

# Correct data entry errors for taxonomic data
# Note, these corrections also contain some cosmetic formatting changes
lemis_taxa_added <- lemis_taxa_added %>%
  left_join(
    ., taxonomic_corrections,
    by = c("genus", "species", "subspecies", "specific_name", "generic_name")
  ) %>%
  mutate(
    genus =
      ifelse(!is.na(new_genus), new_genus, genus),
    species =
      ifelse(!is.na(new_genus), new_species, species),
    subspecies =
      ifelse(!is.na(new_genus), new_subspecies, subspecies),
    specific_name =
      ifelse(!is.na(new_genus), new_specific_name, specific_name),
    generic_name =
      ifelse(!is.na(new_genus), new_generic_name, generic_name)
  ) %>%
  mutate(
    cleaning_notes.y =
      ifelse(!is.na(cleaning_notes.x),
             str_replace(cleaning_notes.y, "Original value in", ","),
             cleaning_notes.y
      ),
    cleaning_notes = case_when(
      !is.na(cleaning_notes.x) & !is.na(cleaning_notes.y) ~
        paste0(cleaning_notes.x, cleaning_notes.y),
      is.na(cleaning_notes.x) & !is.na(cleaning_notes.y) ~
        cleaning_notes.y,
      TRUE ~ cleaning_notes.x
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

# Filter out ITIS classes that do not map unambiguously onto COL taxonomy
ambiguous_itis_classes <- col_itis_harmonize %>%
  group_by(class_itis) %>%
  count() %>%
  filter(n > 1) %>%
  pull(class_itis)

col_itis_harmonize <- col_itis_harmonize %>%
  filter(!(class_itis %in% ambiguous_itis_classes))

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


# Further automatic taxonomic classification using genus-level information

# Gather the names of genera for which species have not been automatically
# classified
genera_to_resolve <- taxatbl2 %>%
  filter(is.na(class)) %>%
  pull(genus) %>%
  unique(.)

# Gather genera-level class information from COL database
generatbl_col <- taxa_tbl("col") %>%
  filter(taxonomicStatus == "accepted") %>%
  distinct(class, genus) %>%
  arrange(genus, class) %>%
  filter(
    genus %in% stringi::stri_trans_totitle(genera_to_resolve),
    !is.na(class),
    class != "Not assigned"
  ) %>%
  collect() %>%
  rename(class_col = class)

# Gather genera-level class information from ITIS database
generatbl_itis <- taxa_tbl("itis") %>%
  filter(taxonomicStatus == "accepted") %>%
  distinct(class, genus) %>%
  arrange(genus, class) %>%
  filter(
    genus %in% stringi::stri_trans_totitle(genera_to_resolve),
    !is.na(class),
    class != "Not assigned"
  ) %>%
  collect() %>%
  rename(class_itis = class)

# Remove any genera for which there are multiple potential class
# affiliations to ensure our eventual taxonomic calls are unambiguous
ambiguous_genera_col <- taxa_tbl("col") %>%
  filter(taxonomicStatus != "provisionally accepted name") %>%
  distinct(class, genus) %>%
  arrange(genus, class) %>%
  group_by(genus) %>%
  summarize(n = n_distinct(class)) %>%
  filter(n > 1) %>%
  pull(genus)

ambiguous_genera_itis <- taxa_tbl("itis") %>%
  filter(taxonomicStatus != "provisionally accepted name") %>%
  distinct(class, genus) %>%
  arrange(genus, class) %>%
  group_by(genus) %>%
  summarize(n = n_distinct(class)) %>%
  filter(n > 1) %>%
  pull(genus)

ambiguous_genera <- union(ambiguous_genera_col, ambiguous_genera_itis)

generatbl_col <- generatbl_col %>%
  filter(!(genus %in% ambiguous_genera)) %>%
  mutate(genus = tolower(genus))

generatbl_itis <- generatbl_itis %>%
  filter(!(genus %in% ambiguous_genera)) %>%
  mutate(genus = tolower(genus))

# Join together genera-level taxonomic information from COL and ITIS
generatbl <-
  full_join(generatbl_col, generatbl_itis, by = "genus") %>%
  select(genus, class_col, class_itis) %>%
  left_join(., col_itis_harmonize, by = "class_itis") %>%
  mutate(class = ifelse(!is.na(class_col), class_col, class_itis_harmonize)) %>%
  select(genus, class)

# Join this newly generated genera-level taxonomic information onto the
# previous data
taxatbl3 <- taxatbl2 %>%
  left_join(., generatbl, by = c("genus")) %>%
  mutate(class = ifelse(!is.na(class.x), class.x, class.y)) %>%
  select(class, genus, species)

#==============================================================================


# Add automatically-generated taxonomic information onto the LEMIS data

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
            str_detect(genus, "formes$|idae$|inae$") |
            (genus == "aves" & generic_name == "ALL BIRDS") |
            (genus == "unknown" & generic_name == "BIRDS") |
            (genus == "unknown" & species == "cites bird") |
            (is.na(genus) & generic_name == "BIRD") |
            (is.na(genus) & generic_name == "BIRDS") |
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
            str_detect(genus, "formes$|idae$|inae$") |
            (genus == "mammalia" & generic_name == "ALL MAMMALS") |
            (genus == "unknown" & species == "cites mammal") |
            (is.na(genus) & generic_name == "MAMMALS") |
            (is.na(genus) & generic_name == "ALL MAMMALS") |
            (is.na(genus) & generic_name == "CANIDS") |
            (is.na(genus) & is.na(generic_name))
        )
      ~ "Mammalia",

      is.na(class) & taxa == "reptile" &
        (
          genus == "noncites entry" |
            str_detect(genus, "formes$|idae$|inae$") |
            (genus == "reptilia" & generic_name == "ALL REPTILES") |
            (genus == "sauria" & generic_name == "LIZARDS") |
            (genus == "serpentes" & generic_name == "SNAKES") |
            (genus == "squamata" & generic_name == "LIZARDS,SNAKES") |
            (genus == "testudinata" & generic_name == "TURTLES,TORTOISES") |
            (genus == "testudines" & generic_name == "TURTLES,TORTOISES") |
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
  ) %>%
  # extra error checking needed to correct some erroneous automatic calls
  mutate(
    class = case_when(
      genus == "ampullaria" & generic_name == "SNAIL" ~ "Gastropoda",
      genus == "chondrilla" & unit == "ML" ~ "Demospongiae",
      genus == "polypus" & str_detect(cleaning_notes, "condietur") ~ "Cephalopoda",
      genus == "riopa" & generic_name == "SKINK" ~ "Reptilia",
      genus == "undaria" & species == "pinnatifida" ~ "Phaeophyceae",
      TRUE ~ class
    )
  )

#==============================================================================


# Add manually-curated taxonomic information onto the LEMIS data

# Which taxa remain unclassified after automated taxonomy calling?
unclassified <- lemis_taxa_added %>%
  filter(is.na(class)) %>%
  distinct(genus, species, subspecies, specific_name, generic_name, taxa) %>%
  arrange(taxa, genus, species)

# Download manually-curated information from Google Sheets and load it as
# a local CSV
read_sheet(
  "1NR_vIhqvNgI8aOOTN4cQNdzS1TwPGBuwEPS8z-MZ-jw",
  col_types = "c", na = "NA"
) %>%
  write_csv(., h("data-raw", "data", "manual_taxonomic_harmonization.csv"))

manual_tax <- read_csv(
  h("data-raw", "data", "manual_taxonomic_harmonization.csv"),
  col_types = cols(.default = col_character())
)

# Ensure the taxa described in the manually-curated sheet match with
# those that are unclassified via automated calling
assert_that(
  all_equal(unclassified, select(manual_tax, 1:6))
)

# Join the manually-curated taxonomic information onto the LEMIS data
lemis_taxa_added <- lemis_taxa_added %>%
  left_join(., manual_tax,
            by = c("genus", "species", "subspecies",
                   "specific_name", "generic_name", "taxa")
  ) %>%
  mutate(class = ifelse(!is.na(class.x), class.x, class.y)) %>%
  select(-class.x, -class.y)


# Examine the relationship between the "taxa" and "class" classifications
error_checking <- lemis_taxa_added %>%
  filter(!is.na(taxa)) %>%
  group_by(class, taxa) %>%
  count() %>%
  arrange(class)

# Where possible, use "class" information to add "taxa" information to
# missing records since "class" always maps unambiguously to "taxa"
manual_class_taxa_additions <- data.frame(
  class = c("Cubozoa", "Gymnolaemata", "Hexactinellida", "Ostracoda", "Pycnogonida", "Thaliacea", "Trematoda"),
  taxa = c("coral", "other", "other", "crustacean", "other", "other", "other")
)

lemis_taxa_added <- lemis_taxa_added %>%
  left_join(
    .,
    error_checking %>%
      filter(!is.na(class)) %>%
      select(class, taxa) %>%
      bind_rows(manual_class_taxa_additions) %>%
      rename(new_taxa = taxa),
    by = c("class")
  ) %>%
  mutate(
    taxa = ifelse(is.na(taxa) & !is.na(new_taxa), new_taxa, taxa)
  ) %>%
  select(-new_taxa)

# Ensure that all "class" values appearing in the data are valid
# COL classes
col.unique.classes <- taxa_tbl("col") %>%
  pull(class) %>%
  unique()
col.unique.classes <- sort(c(col.unique.classes, "Phaeophyceae", "Ulvophyceae"))

lemis.unique.classes <- lemis_taxa_added %>%
  distinct(class) %>%
  filter(!is.na(class)) %>%
  pull(class)

assert_that(all(lemis.unique.classes %in% col.unique.classes))

# Ensure that the data frame to be saved has the same number of rows as
# "lemis_intermediate"
assert_that(nrow(lemis_intermediate) == nrow(lemis_taxa_added))

#==============================================================================


# Data saving

lemis_to_save <- lemis_taxa_added %>%
  mutate(
    genus = case_when(
      genus == "noncites entry" ~ "Non-CITES entry",
      TRUE ~ str_to_sentence(genus)
    ),
    species = str_replace_all(species, "cites ", "CITES "),
    generic_name = ifelse(generic_name == "NON CITES", "NON-CITES", generic_name)
  ) %>%
  # select final columns to keep
  select(
    control_number,
    species_code,
    taxa,
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
  arrange(shipment_date, control_number, species_code)

# Write a CSV file of final LEMIS data
write_csv(
  lemis_to_save,
  h("data-raw",
    paste0("lemis_",
           min(lemis_to_save$shipment_year, na.rm = TRUE), "_",
           max(lemis_to_save$shipment_year, na.rm = TRUE), "_cleaned.csv")
  )
)
