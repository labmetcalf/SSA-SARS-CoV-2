
# Libraries
library(tidyverse)
library(glue)
library(sf)
library(cowplot)
library(stringr)
library(patchwork)
library(scales)
library(plotly)
library(leaflet)
library(htmltools)
library(here)

source(here("R/_functions/map_bivar.R"))

# Load in country dataset
countries <- st_read(here("data/processed/shapefiles/country.shp"), 
                     quiet = TRUE)
countries$loc_id <- countries$iso # for matching to indicator data
ssa_countries <- read_csv(here("data/processed/ssa_countries.csv"))
countries <- filter(countries, iso %in% ssa_countries$iso)

indicators <- read_csv(here("data/processed/SSA.health.indicators.csv"))

indicators %>%
  group_by(alias) %>%
  summarize(label = gsub("popn", "people", indicator_label_standard[1])) -> indicator_labs

indicators %>% 
  pivot_wider(id_cols = c("COUNTRY_CODE", "COUNTRY_NAME"), names_from = alias, 
              values_from = value) -> indicators_wide

test <- map_bivariate(x = indicators_wide$`2_2_prev_diabetes_20_79yos`, 
                      y = indicators_wide$`1_1_physicians_p100k`, 
                      lab_x = indicator_labs$label[indicator_labs$alias == "2_2_prev_diabetes_20_79yos"], 
                      lab_y = indicator_labs$label[indicator_labs$alias == "1_1_physicians_p100k"],
                      loc_ids = indicators_wide$COUNTRY_CODE, 
                      loc_labs = indicators_wide$COUNTRY_NAME,
                      sf_obj = countries, continuous = TRUE, trans_x = "log", trans_y = "log", 
                      probs_x = c(0.25, 0.75), probs_y = c(0.25, 0.75), static = TRUE, 
                      add_scatter = FALSE,
                      rev_x = FALSE, rev_y = TRUE)
ggsave("figs/main/bivariate_example.jpeg", test, height = 8, width = 8)





