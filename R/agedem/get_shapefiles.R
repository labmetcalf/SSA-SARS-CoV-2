# ------------------------------------------------------------------------------------------------ #
#' Write out simplified shapefiles for cluster                  
# ------------------------------------------------------------------------------------------------ #

library(rgdal)
library(malariaAtlas)
library(rmapshaper)

# metadata
metadata <- read.csv("wp_data/Africa_1km_Age_structures_2020/Demographic_data_organisation_per country_AFRICA.csv")
iso_codes <- data.frame(iso = metadata$ISO_3, country = metadata$name_english)
write.csv(iso_codes, "output/iso_codes.csv", row.names = FALSE)

# Country level
countries <- getShp(ISO = metadata$ISO_3)
countries@data <- data.frame(apply(countries@data, 2, iconv, from ='utf-8', to ='ascii', sub=''))
countries <- ms_simplify(countries, keep_shapes = TRUE, sys = TRUE)
writeOGR(countries, dsn = "shapefiles", layer = "country", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

# Admin 1
admin1 <- getShp(ISO = metadata$ISO_3, admin_level = "admin1")
admin1@data <- data.frame(apply(admin1@data, 2, iconv, from ='utf-8', to ='ascii', sub=''))
admin1 <- ms_simplify(admin1, keep_shapes = TRUE, sys = TRUE)
writeOGR(admin1, dsn = "shapefiles", layer = "admin1", 
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
writeOGR(admin2, dsn = "shapefiles", layer = "admin2", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

# Admin 3
# simplify admin 3 using the sys argument
# For this you'll need to install node.js & mapshaper first 
# (see readme here: https://github.com/ateucher/rmapshaper)
admin3 <- getShp(ISO = metadata$ISO_3, admin_level = "admin3")
admin3@data <- data.frame(apply(admin3@data, 2, iconv, from ='utf-8', to ='ascii', sub=''))
admin3 <- ms_simplify(admin3, keep_shapes = TRUE, sys = TRUE)
writeOGR(admin3, dsn = "shapefiles", layer = "admin3", 
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

writeOGR(admin3, dsn = "shapefiles", layer = "master", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)
