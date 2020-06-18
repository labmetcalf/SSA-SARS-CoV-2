# ------------------------------------------------------------------------------------------------ #
#' Write out simplified shapefiles for use throughout project                  
# ------------------------------------------------------------------------------------------------ #

library(rgdal)
library(malariaAtlas)
library(rmapshaper)
library(tidyverse)
library(here)
library(raster)
library(data.table)

# metadata
metadata <- read_csv(here("data/raw/Africa_1km_Age_structures_2020/Demographic_data_organisation_per country_AFRICA.csv"))
iso_codes <- data.frame(iso = metadata$ISO_3, country = metadata$name_english)
write_csv(iso_codes, here("output/iso_codes.csv"))

# Country level
countries <- getShp(ISO = metadata$ISO_3)
countries@data <- data.frame(apply(countries@data, 2, iconv, from ='utf-8', to ='ascii', sub=''))
countries <- ms_simplify(countries, keep_shapes = TRUE, sys = TRUE)
writeOGR(countries, dsn = here("data/processed/shapefiles"), layer = "country", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

# Admin 1
admin1 <- getShp(ISO = metadata$ISO_3, admin_level = "admin1")
admin1@data <- data.frame(apply(admin1@data, 2, iconv, from ='utf-8', to ='ascii', sub=''))
admin1 <- ms_simplify(admin1, keep_shapes = TRUE, sys = TRUE)
writeOGR(admin1, dsn = here("data/processed/shapefiles"), layer = "admin1", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

# Admin 2
admin2 <- getShp(ISO = metadata$ISO_3, admin_level = "admin2")
admin2@data <- data.frame(apply(admin2@data, 2, iconv, from ='utf-8', to ='ascii', sub=''))
admin2 <- ms_simplify(admin2, keep_shapes = TRUE, sys = TRUE)
admin2@data %>%
  mutate(id_1 = case_when(is.na(type_1) ~ as.character(id_0), 
                          !is.na(type_1) ~ as.character(id_1)), 
         id_2 = case_when(is.na(type_2) ~ as.character(id_1), 
                          !is.na(type_2) ~ as.character(id_2)),
         min_admin = case_when(is.na(type_1) ~ "Admin 0",
                               is.na(type_2) ~ "Admin 1",
                               !is.na(type_2) ~ "Admin 2"),
         min_admin_type = case_when(is.na(type_1) ~ as.character(type_0),
                                    is.na(type_2) ~ as.character(type_1),
                                    !is.na(type_2) ~ as.character(type_2))) -> admin2@data
writeOGR(admin2, dsn = here("data/processed/shapefiles"), layer = "admin2", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

# Admin 3
# simplify admin 3 using the sys argument
# For this you'll need to install node.js & mapshaper first 
# (see readme here: https://github.com/ateucher/rmapshaper)
admin3 <- getShp(ISO = metadata$ISO_3, admin_level = "admin3")
admin3@data <- data.frame(apply(admin3@data, 2, iconv, from ='utf-8', to ='ascii', sub=''))
admin3 <- ms_simplify(admin3, keep_shapes = TRUE, sys = TRUE)
writeOGR(admin3, dsn = here("data/processed/shapefiles"), layer = "admin3", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

# Master shapefile (finest admin unit available)
admin3@data %>%
  mutate(id_1 = case_when(is.na(type_1) ~ as.character(id_0), 
                     !is.na(type_1) ~ as.character(id_1)), 
         id_2 = case_when(is.na(type_2) ~ as.character(id_1), 
                          !is.na(type_2) ~ as.character(id_2)),
         id_3 = case_when(is.na(type_3) ~ as.character(id_2), 
                          !is.na(type_3) ~ as.character(id_3)),
         min_admin = case_when(is.na(type_1) ~ "Admin 0",
                               is.na(type_2) ~ "Admin 1",
                               !is.na(type_3) ~ "Admin 3",
                               is.na(type_3) ~ "Admin 2"),
         min_admin_type = case_when(is.na(type_1) ~ as.character(type_0),
                                    is.na(type_2) ~ as.character(type_1),
                                    !is.na(type_3) ~ as.character(type_3),
                                    is.na(type_3) ~ as.character(type_2))) -> admin3@data

writeOGR(admin3, dsn = here("data/processed/shapefiles"), layer = "master", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

# GADM Admin shapefiles 
gadm_admin2 <- readOGR(here("data/raw/gadm_adm2/gadm36_2.shp"))
gadm_admin2 <- gadm_admin2[gadm_admin2$GID_0 %in% iso_codes$iso, ]

# pops for Jess (rasterize) + data.table (do it @ finer scale other wise some admin end up NA)
files <- list.files(here('data/raw/Africa_1km_Age_structures_2020'), full.names = TRUE)
files <- files[grepl(".tif$", files)] # only tifs
age_rasts <- lapply(files, raster)
age_rasts <- stack(age_rasts)
pop <- sum(age_rasts, na.rm = TRUE) # Takes a bit like 7 minutes

# rasterize
id_match <- rasterize(gadm_admin2, pop) # Takes too long...1 hr ish try pkg fasterize if repeated
out <- data.table(feature_id = values(id_match), pop = values(pop))
out <- out[, .(pop = sum(pop, na.rm = TRUE)), by = c("feature_id")]
gadm_admin2$feature_id <- 1:nrow(gadm_admin2@data)
gadm_admin2@data <- left_join(gadm_admin2@data, out)

# match to travel times 
ttimes <- read.csv("data/raw/SSA_BigData_v1.2.csv")
ttimes$pop <- gadm_admin2$pop[match(ttimes$Administrative.level.2.unit.code, gadm_admin2$GID_2)]
write_csv(ttimes, "data/processed/ttimes_SSA.csv")
