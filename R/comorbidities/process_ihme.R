# ------------------------------------------------------------------------------------------------ #
#' Processing ihme data                                                                                       
# ------------------------------------------------------------------------------------------------ #

# Packages
library(tidyverse)
library(here)

# Data
ncds <- read_csv(here("data/raw/IHME_ageNCDs/IHME-GBD_2017_DATA-f8d93ddd-1.csv"))
iso_codes <- read_csv(here("output/iso_codes.csv"))

# Filter to countries 
ihme_countries <- unique(ncds$location_name)
iso_codes$ihme_countries <- ihme_countries[apply(adist(iso_codes$country, 
                                                       ihme_countries, 
                                                       partial = FALSE), 1, 
                                                 which.min)]
# Manual fixes to match names
iso_codes %>%
  mutate(ihme_countries = 
           case_when(country == "Gambia" ~ "The Gambia", 
                     country %in% c("Mayotte", "Saint Helena", "Reunion", "Western Sahara") ~ "NA", 
                     !(country %in% c("Mayotte", "Saint Helena", "Reunion", "Western Sahara", 
                             "Gambia")) ~ ihme_countries)) -> iso_codes

ncds %>%
  filter(location_name %in% iso_codes$ihme_countries | location_name %in% c("Italy", "China", "France"), # For comparing IFR estimates
         age_name != "Age-standardized", measure_name == "Prevalence") %>%
  pivot_wider(id_cols = c("measure_name", "location_name", "age_name", "year"), 
              names_from = cause_name, values_from = val) %>%
  rowwise() %>%
  mutate(`Chronic respiratory diseases (-Asthma)` = if_else(`Chronic respiratory diseases` < Asthma, 0, 
                                             `Chronic respiratory diseases` - Asthma), 
         mean_100k = mean(`Chronic respiratory diseases (-Asthma)`, `Cardiovascular diseases`, Neoplasms,
                               `Diabetes mellitus`), 
         sum_100k = sum(`Chronic respiratory diseases (-Asthma)`, `Cardiovascular diseases`, Neoplasms,
                              `Diabetes mellitus`)) %>%
  select(-c("Asthma", "Chronic respiratory diseases")) -> ncd_prev

ncds %>%
  filter(location_name %in% iso_codes$ihme_countries | location_name %in% c("Italy", "China", "France"), # For comparing IFR estimates
         age_name != "Age-standardized", measure_name == "Deaths") %>%
  pivot_wider(id_cols = c("measure_name", "location_name", "age_name", "year"), 
              names_from = cause_name, values_from = val) %>%
  rowwise() %>%
  mutate(`Chronic respiratory diseases (-Asthma)` = if_else(`Chronic respiratory diseases` < Asthma, 0, 
                                                            `Chronic respiratory diseases` - Asthma), 
         mean_100k = mean(`Chronic respiratory diseases (-Asthma)`, `Cardiovascular diseases`, Neoplasms,
                               `Diabetes mellitus`), 
         sum_100k = sum(`Chronic respiratory diseases (-Asthma)`, `Cardiovascular diseases`, Neoplasms,
                             `Diabetes mellitus`)) %>%
  select(-c("Asthma", "Chronic respiratory diseases")) -> ncd_deaths

ncd_mets <- bind_rows(ncd_deaths, ncd_prev)
ncd_mets$iso <- iso_codes$iso[match(ncd_mets$location_name, iso_codes$ihme_countries)]
write_csv(ncd_mets, here("data/processed/ihme_cleaned.csv"))

