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
directory <- here("data/raw/Africa_1km_Age_structures_2020/")
out_dir <- here("data/processed/africa_10km_2020/")

files <- list.files(directory, recursive = TRUE)
files <- files[grepl(".tif$", files)] # only tifs
ages <- unique(substring(files, 9, 13)) # get the unique age bins

# Aggregate up to make easier -------------------------------------------------------
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

# Make into raster brick per Marjolein -------------------------------------------------------
# Load data
allfiles <- as.list(list.files(here('data/processed/africa_10km_2020'), full.names = TRUE))

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
writeRaster(dat, filename = here('data/processed/demoMapAfrica2020.tif'), format = "GTiff",
            overwrite = TRUE, options = c("INTERLEAVE=BAND", "COMPRESS=LZW"))

# Close out
stopCluster(cl)
Sys.time()