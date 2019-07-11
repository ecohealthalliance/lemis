library(lemis)
library(dplyr)
library(tidyr)
library(lubridate)
library(countrycode)
ldata <- collect(lemis_data())
lcodes <- lemis_codes()

portnames <- lcodes %>%
  filter(field == "port") %>%
  select(port = code, portname = value)

continents <- lcodes %>%
  filter(field == "country") %>%
  mutate(continent = countrycode(value, "country.name", "continent") %>%
                       coalesce("Other")) %>%
  select(country_origin = code, continent_origin = continent)

summary_by_year <- ldata %>%
  filter(import_export == "I") %>%
  mutate(year = year(shipment_date)) %>%
  left_join(portnames, by = "port") %>%
  left_join(continents, by = "country_origin") %>%
  group_by(year, continent_origin, portname) %>%
  summarize(shipments = n()) %>%
  filter_all(all_vars(!is.na(.)))


chord_data <- summary_by_year %>%
  spread(continent_origin, shipments, fill = 0) %>%
  split(.$year) %>%
  lapply(function(x) {
    mat <- as.matrix(x[,-(1:2)])
    rownames(mat) <- x$portname
    mat
  })


saveRDS(chord_data, here::here("inst", "shiny", "trade_matrices.rds"))
