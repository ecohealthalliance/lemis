library(tidyverse)
library(lemis)
library(countrycode)
#devtools::install_github("thomasp85/transformr")

### Get data
mdat <- lemis_metadata()

dat <- lemis_data() %>%
  collect()

codes <- lemis_codes()

purpose <- codes %>% filter(field=="purpose") %>%
  select(code, value) %>%
  rename(purpose = code, purpose_desc = value)

cref <- CoordinateCleaner::countryref %>%
  filter(type == "country") %>%
  distinct(iso2, .keep_all = TRUE) %>%
  select(iso2, centroid.lon, centroid.lat)

# mapd <- map_data("world") %>%
#   filter(region != "Antarctica") %>%
#   rename(country_name = region) %>%
#   select(-subregion)

### Process data
sdat <- dat %>%
  mutate(year = as.numeric(format(shipment_date,"%Y"))) %>%
  select(country_imp_exp, year, taxa, purpose) %>%
  group_by(country_imp_exp, year, taxa, purpose) %>%
  mutate(n_by_country_taxa_purpose = n()) %>%
  group_by(country_imp_exp, year, purpose) %>%
  mutate(n_by_country_purpose = n()) %>%
  group_by(country_imp_exp, year, taxa) %>%
  mutate(n_by_country_taxa = n()) %>%
  group_by(country_imp_exp, year) %>%
  mutate(n_by_country = n()) %>%
  ungroup() %>%
  distinct() %>%
  mutate(country_name  = countrycode(sourcevar = country_imp_exp,
                                     origin = "iso2c",
                                     destination = "country.name")) %>%
  mutate(continent  = countrycode(sourcevar = country_imp_exp,
                                  origin = "iso2c",
                                  destination = "continent")) %>%
  mutate(iso3c  = countrycode(sourcevar = country_imp_exp,
                              origin = "iso2c",
                              destination = "iso3c")) %>%
  left_join(purpose) %>%
  na.omit() %>%
  left_join(cref, by = c("country_imp_exp" = "iso2")) %>%
  janitor::clean_names() %>%
  mutate(taxa = paste0(toupper(substr(taxa, 1, 1)), tolower(substring(taxa, 2))))

tdat <- sdat %>%
  select(continent, country_name, iso3c, taxa, year, n_by_country_taxa, n_by_country, centroid_lon, centroid_lat) %>%
  distinct()

write_csv(tdat, "inst/shiny/widgets/lemis_dat_format.csv")

library(cartogram)
library(plotly)
library(sf)

udat <- tdat %>%
  select(-n_by_country, -centroid_lon, -centroid_lat)

w <- maps::map('world', plot = FALSE, fill = TRUE) %>%
  st_as_sf() %>%
  mutate(iso3c  = countrycode(sourcevar = ID,
                              origin = "country.name",
                              destination = "iso3c")) %>%
  left_join(udat) %>%
  filter(!is.na(n_by_country_taxa))

write_rds(w, "inst/shiny/widgets/lemis_dat_by_taxa_sf.rds")

vdat <- tdat %>%
  group_by(continent, country_name, iso3c, year) %>%
  filter(n_by_country_taxa==max(n_by_country_taxa, na.rm=TRUE)) %>%
  mutate(most_common_taxa = taxa) %>%
  ungroup() %>%
  select(-n_by_country_taxa, -taxa, -centroid_lon, -centroid_lat) %>%
  distinct()

w2 <- maps::map('world', plot = FALSE, fill = TRUE) %>%
  st_as_sf() %>%
  mutate(iso3c  = countrycode(sourcevar = ID,
                              origin = "country.name",
                              destination = "iso3c")) %>%
  left_join(vdat)%>%
  filter(!is.na(n_by_country))

write_rds(w2, "inst/shiny/widgets/lemis_dat_by_country_sf.rds")

# dorling data prep

w_cross <-tdat %>%
  dplyr::select(taxa, year) %>%
  distinct()

w_dor <-  map2(w_cross$taxa, w_cross$year, function(x, y){
  cartogram_dorling(w %>% filter(taxa == x, year == y),
                    "n_by_country_taxa",
                    k=1) %>%
    as_tibble() %>%
    plotly:::to_basic.GeomSf()

})
names(w_dor) <- paste(w_cross$taxa, w_cross$year, sep = "_")

write_rds(w_dor, "inst/shiny/widgets/lemis_dat_by_country_dor.rds")

