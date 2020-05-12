# ------------------------------------------------------------------------------------------------
#' Getting mada estimates 
# ------------------------------------------------------------------------------------------------ 

# set up cluster on single node with do Parallel
library(doParallel) 
cl <- makeCluster(3)
registerDoParallel(cl)
getDoParWorkers()
Sys.time()

library(raster)
library(data.table)
library(rgdal)
library(foreach)
library(iterators)
library(glue)

# directories
directory <- "wp_data/Africa_1km_Age_structures_2020//"
out_dir <- "wp_data/africa_10km_2020/"

files <- list.files(directory, recursive = TRUE)
files <- files[grepl(".tif$", files)] # only tifs
ages <- unique(substring(files, 9, 13)) # get the unique age bins

foreach(i = 1:length(ages), .combine = cbind, .packages = c("raster", "glue"),
        .export = c("directory", "out_dir", "ages")) %dopar% {
          
          popM <- raster(glue("{directory}AFR_PPP_{ages[i]}_M_2020_adj_v5.tif"))
          popF <- raster(glue("{directory}AFR_PPP_{ages[i]}_F_2020_adj_v5.tif"))
          
          popM <- aggregate(popM, fact = 10, fun = sum, na.rm = TRUE)
          writeRaster(popM, glue("{out_dir}afr_m_{ages[i]}_2020_10km.tif"))
          
          popF <- aggregate(popF, fact = 10, fun = sum, na.rm = TRUE)
          writeRaster(popF, glue("{out_dir}afr_f_{ages[i]}_2020_10km.tif"))
          
          pop <- popM + popF
          values(pop)
          
        } -> out_mat

colnames(out_mat) <- ages
fwrite(out_mat, "output/temp_out_afr.gz")

# Read in shapefiles --------------------------------------------------------------------------
raster_base <- raster("wp_data/africa_10km_2020/afr_f_A0004_2020_10km.tif")
values(raster_base) <- 1:ncell(raster_base)

# Admin 3
admin3 <- readOGR("shapefiles/admin3.shp")
admin3$id_match <- 1:length(admin3)
id_match <- values(rasterize(admin3, raster_base, field = "id_match"))

afr_dt <- data.table(cell_id = values(raster_base), iso_code = admin3$iso[id_match], 
                     admin1_code = admin3$id_1[id_match], admin2_code = admin3$id_2[id_match],
                     admin3_code = admin3$id_3[id_match], out_mat)

fwrite(afr_dt, "output/afr_dt.gz")

afr_admin1 <- afr_dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = 6:ncol(afr_dt), 
                       by = c("admin1_code")]
fwrite(afr_admin1, "output/afr_admin1.csv")

afr_admin2 <- afr_dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = 6:ncol(afr_dt), 
                       by = c("admin2_code")]
fwrite(afr_admin2, "output/afr_admin2.csv")

afr_admin3 <- afr_dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = 6:ncol(afr_dt), 
                       by = c("admin3_code")]
fwrite(afr_admin3, "output/afr_admin3.csv")

# Make into raster brick per Marjolein -------------------------------------------------------
# Load data
allfiles <- as.list(list.files('wp_data/africa_10km_2020',full.names=TRUE))

# Combine into one brick file
dat <- lapply(allfiles, raster)
namess <- lapply(dat, names) # file names
uniquenames <- substr(namess, 6, 21) # exl gender

# Sum M + F
datsubs <- list()
for (i in 1:14) {
  datsubs[[i]] <- dat[[i]] + dat[[which(uniquenames == uniquenames[i])[2]]]
}

agelower <- substr(namess[1:14], 8, 9)
ageupper <- substr(namess[1:14], 10, 11)
ageclasses <- as.numeric(agelower) + 2
names(datsubs) <- paste(agelower, ageupper)

dat <- brick(datsubs)

# save output
writeRaster(dat, filename = 'output/demoMapAfrica2020.tif', format = "GTiff",
            overwrite = TRUE, options = c("INTERLEAVE=BAND", "COMPRESS=LZW"))

# Close out
stopCluster(cl)
Sys.time()