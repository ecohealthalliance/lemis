## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----install_me, eval = FALSE--------------------------------------------
#  source("https://install-github.me/ecohealthalliance/lemis")

## ----table1, echo=FALSE, message=FALSE, fig.cap="Table 1. LEMIS database field names"----
knitr::kable(lemis::lemis_metadata(), format = "markdown")

## ----tutorial, eval = FALSE----------------------------------------------
#  library(lemis)
#  library(dplyr)
#  
#  # Load LEMIS data first time (downloads ~160MB file), returns remote
#  # dplyr source to database
#  l <- lemis_data()
#  
#  rm(l)
#  # Load again (will NOT download data, it's already on your machine)
#  l <- lemis_data()
#  
#  # Load data into memory (~Will take up 1GB RAM)
#  ll <- collect(l)
#  ll
#  rm(ll)
#  
#  # Filter on-disk and load into memory (smaller!)
#  
#  l_filtered <- l %>%
#    filter(import_export == "I", port = "SS") %>%
#    select(-taxa)
#  l_filtered
#  ll_filtered %>% collect()
#  ll_filtered
#  rm(ll_filtered)
#  
#  # Get data dictionary and codes
#  lemis_metadata()
#  lemis_codes()
#  
#  # View interactive searchable data dictionary
#  ?lemis_metadata
#  ?lemis_codes
#  
#  # List versions of LEMIS data on your machine
#  lemis_versions()
#  lemis_version_current()
#  
#  # Remove a downloaded version
#  lemis_del(lemis_version_current())
#  
#  # Now list local versions again
#  lemis_versions()
#  # List versions of LEMIS data available remotely
#  lemis_versions(local=FALSE)

## ----setup2, message=FALSE, warning=FALSE, results='hide'----------------
library(lemis)
library(tidyverse)
library(stringi)
theme_set(theme_minimal())

## ------------------------------------------------------------------------
sss_port <- # Lookup the port code
  lemis_codes() %>% 
  filter(field == "port", value=="Sault Sainte Marie") %>% 
  pull(code)
lemis_data() %>% 
  filter(port == sss_port, import_export == "I") %>% #Filter before loading the file to disk
  collect() %>% 
  group_by(generic_name) %>% 
  summarise(shipments = n()) %>% 
  arrange(desc(shipments)) %>% 
  head(10) %>% 
ggplot(aes(x = fct_reorder(generic_name, shipments), y = shipments)) +
  geom_col() +
  coord_flip() +
  xlab("Generic Animal Type") + ylab("Shipments via Sault Sainte Marie 2000-2013")

## ------------------------------------------------------------------------
lemis_data() %>% 
  filter(genus == "pteropus", import_export == "I") %>% 
  collect() %>% 
  group_by(description) %>% 
  summarize(shipments = n()) %>% 
  left_join(filter(lemis_codes(), field=="description"), by = c("description"="code")) %>% 
  arrange(desc(shipments)) %>% 
ggplot(aes(x = fct_reorder(value, shipments), y = shipments)) +
  geom_col() +
  coord_flip() +
  xlab("Description of Pteropus Imports") + ylab("No. Shipments 2000-2013")
  

## ------------------------------------------------------------------------
library(taxize)
bat_genera <- downstream(get_tsn("Chiroptera", rows=1), downto = "Genus", db = "itis") %>% 
  `[[`(1)%>% 
  filter(rankname == "genus") %>% 
  pull(taxonname) %>% 
  stri_trans_tolower()
lemis_data() %>% 
  filter(import_export == "I", description == "LIV", genus %in% bat_genera) %>% 
  group_by(country_origin) %>% 
  summarize(number = sum(quantity)) %>% 
  left_join(filter(lemis_codes(), field=="country"),
            by=c("country_origin"="code")) %>% 
  ggplot(aes(x=fct_reorder(value, number), y=number)) +
  geom_col() +
  coord_flip() +
  xlab("Origin Country") + ylab("No. Live Bats Imported to U.S., 2000-2013")
  

## ------------------------------------------------------------------------
lemis_data() %>% 
  filter(import_export == "I", description == "LIV", genus %in% bat_genera) %>% 
  group_by(us_co) %>% 
  summarize(number = sum(quantity)) %>% 
  ggplot(aes(x=fct_reorder(us_co, number), y=number)) +
  geom_col() +
  coord_flip() +
  xlab("U.S. Importer") + ylab("No. Live Bats Imported to U.S., 2000-2013")

## ------------------------------------------------------------------------
lemis_data() %>% 
  filter(import_export == "I") %>% 
  group_by(country_origin) %>% 
  summarize(frac_seized = sum(disposition == "S")/n()) %>% 
  left_join(filter(lemis_codes(), field=="country"), by=c("country_origin"="code")) %>%
  arrange(desc(frac_seized)) %>% 
  filter(!is.na(value)) %>% 
  head(30) %>% 
  ggplot(aes(x=fct_reorder(value, frac_seized), y=frac_seized)) +
  geom_col() +
  coord_flip() +
  labs(title="Top 30 Origin Countries by Fraction of Wildlife Shipments Seized",
       x="Origin Country", y="Fraction of Shipments Seized, 2000-2013")

