# ------------------------------------------------------------------------------------------------ #
#' Summarize data to admin level                                                                         
# ------------------------------------------------------------------------------------------------ #

# set up cluster
library(doParallel) 
cl <- makeCluster(3)
registerDoParallel(cl)
getDoParWorkers()
Sys.time()

# packages
library(raster)
library(rgdal)
library(data.table)
library(tidyverse)

# data
admin1 <- readOGR("output/shapefiles/admin1.shp")

# Aggregate Alegana stats ---------------------------------------------------------------------
# The rasters are not the same so we have to do this individually for each one!
# Takes abt 2 - 5 minutes per raster

files <- list.files("output", recursive = TRUE, full.names = TRUE)
files <- files[grepl("alegana", files)] # only tifs
val_rasts <- lapply(files, raster)
id_rasts <- lapply(val_rasts, function(x) {values(x) <- 1:ncell(x); return(x);})

# Match to finest admin level available
admin1$id_match <- 1:length(admin1)
# for combining dts

multicomb <- function(x, ...) {
  mapply(rbind, x, ..., SIMPLIFY = FALSE)
}

foreach(i= 1:length(id_rasts), .combine = multicomb, .packages = c("raster", "data.table")) %dopar% {
  # rasterize
  id_match <- values(rasterize(admin1, id_rasts[[i]], field = "id_match"))
  dt <- data.table(cell_id = values(val_rasts[[i]]), 
                   id_match, iso = admin1$iso[id_match],
                   value = values(val_rasts[[i]]))
  
  admin_dt <- dt[, .(value = mean(value, na.rm = TRUE)), by = c("id_match")]
  country_dt <- dt[, .(value = mean(value, na.rm = TRUE)), by = c("iso")]
  
  admin_dt$type <- country_dt$type <- gsub("alegana", "", names(val_rasts[[i]]))
  list(admin_dt, country_dt)
  
} -> summ_dt

# Close out
stopCluster(cl)

# Pivot so that each metric is a column
summ_dt[[1]] %>%
  pivot_wider(id_cols = id_match, names_from = type, values_from = value) %>%
  filter(!is.na(id_match)) -> admin1_dt # shape id matches the row id from the master shapefiles
admin1_dt$iso <- admin1$iso[admin1_dt$id_match]
summ_dt[[2]] %>%
  pivot_wider(id_cols = iso, names_from = type, values_from = value) %>%
  filter(!is.na(iso)) -> country_dt # shape id matches the row id from the master shapefiles

# Aggregate age + pop totals! ----------------------------------------------------------------
raster_base <- raster("wp_data/africa_10km_2020/afr_f_A0004_2020_10km.tif")
values(raster_base) <- 1:ncell(raster_base)
id_match <- values(rasterize(admin1, raster_base, field = "id_match"))
out_mat <- fread("output/temp_out_afr.gz")

afr_age <- data.table(cell_id = values(raster_base), iso = admin1$iso[id_match], 
                     id_match = id_match, out_mat)

afr_admin1 <- afr_age[, lapply(.SD, sum, na.rm = TRUE), .SDcols = 4:ncol(afr_age), 
                     by = c("id_match")]
afr_admin1$pop <- rowSums(afr_admin1[, 4:ncol(afr_admin1), with = FALSE])
afr_country <- afr_age[, lapply(.SD, sum, na.rm = TRUE), .SDcols = 4:ncol(afr_age), 
                                                                by = c("iso")]
afr_country$pop <- rowSums(afr_country[, 4:ncol(afr_country), with = FALSE])

# Join with alegana stats
afr_admin1 <- afr_admin1[admin1_dt, on = "id_match"]
afr_country <- afr_country[country_dt, on = "iso"]

# Join with necessary spatial data (names & admin types!)
afr_admin1 %>%
  left_join(select(admin1@data, country = name_0, admin_name = name_1, 
                   admin_type = type_1, id_match)) -> afr_admin1
afr_country$country <- admin1$name_0[match(afr_country$iso, admin1$iso)]

fwrite(afr_admin1, "output/admin1_dt.csv")
fwrite(afr_country, "output/country_dt.csv")


