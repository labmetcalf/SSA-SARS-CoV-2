# ------------------------------------------------------------------------------------------------ #
#' Clean UN Population Data                                                                                       
# ------------------------------------------------------------------------------------------------ #

# Packages
library(tidyverse)
library(readxl)
library(here)

# Data
un_ages <- read_excel(here("data/raw/WPP2019_INT_F03_1_POPULATION_BY_AGE_ANNUAL_BOTH_SEXES.xlsx"), 
                      skip = 16)
iso_codes <- read_csv(here("data/Data/iso_codes.csv"))

# UN Data -------------------------------------------------------------------------------------
# Clean and match to country iso codes
un_ages %>%
  filter(Type == "Country/Area", `Reference date (as of 1 July)` == 2020) -> un_ages
  
# UN Countries
un_countries <- unique(un_ages$`Region, subregion, country or area *`)
iso_codes$un_countries <- un_countries[apply(adist(iso_codes$country, 
                                                       un_countries, 
                                                       partial = FALSE), 1, 
                                                 which.min)]
# Manual fixes to match names
iso_codes %>%
  mutate(un_countries = 
           case_when(country == "Republic of Congo" ~ "Congo", 
                     country == "Swaziland" ~ "Eswatini", 
                     country == "Tanzania" ~ "United Republic of Tanzania",
                     country == "Saint Helena" ~ "NA", 
                     !(country %in% c("Republic of Congo", "Saint Helena", "Swaziland",
                                      "United Republic of Tanzania")) ~ un_countries)) -> iso_codes
un_ages %>%
  rename(un_country = `Region, subregion, country or area *`) %>%
  filter(un_country %in% iso_codes$un_countries | un_country %in% c("Italy", "China", "France")) %>%
  mutate_at(vars(`0`:`100`), function(x) as.numeric(x)*1000) %>%
  select(un_country, year = `Reference date (as of 1 July)`, `0`:`100`) %>%
  pivot_longer(`0`:`100`, names_to = "age", values_to = "pop") %>%
  left_join(iso_codes, by = c("un_country" = "un_countries")) -> un_ages

write_csv(un_ages, here("data/processed/un_ages.csv"))

# World Pop data -------------------------------------------------------------------------

# TO DO

# Summarize prop over 65 -----------------------------------------------------------------
indicators <- read_csv(here("data/processed/SSA.health.indicators.csv"))

un_ages %>%
  group_by(iso) %>%
  summarize(pop_total = sum(pop), pop_over60 = sum(pop[as.numeric(age) > 60]),
            prop_over60 = pop_over60/pop_total) %>%
  filter(!is.na(iso)) %>%
  left_join(distinct(select(indicators, COUNTRY_CODE, COUNTRY_NAME)), 
            by = c("iso" = "COUNTRY_CODE")) -> pop_over60

pop_over60 %>%
  select(COUNTRY_CODE = iso, value = prop_over60) %>%
  mutate(alias = "prop_over60", 
         indicator_label_standard = "Proportion of population over age 60") %>%
  bind_rows(indicators) -> indicators

write_csv(indicators, here("data/processed/SSA_indicators_all.csv"))
          
          

