# ------------------------------------------------------------------------------------------------ #
#' Process travel times and reporting                                                                       
# ------------------------------------------------------------------------------------------------ #

# set up cluster
library(doParallel) 
cl <- makeCluster(3)
registerDoParallel(cl)
getDoParWorkers()
Sys.time()

# libraries
library(raster)
library(here)

directories <- c("data/raw/alegana_pseek", "data/raw/alegana_ttimes")

files <- list.files(directories, recursive = TRUE, full.names = TRUE)
names <- list.files(directories, recursive = TRUE)
files <- files[grepl(".tif$", files)] # only tifs
names <- gsub(".tif", "", names[grepl(".tif$", names)])

foreach(i = 1:length(files), .packages = "raster",
        .export = c("files", "names")) %dopar% {
          toagg <- raster(here(files[i]))
          agg <- aggregate(toagg, fact = 10, fun = mean, na.rm = TRUE) # take mean 
          names(agg) <- names[i]
          writeRaster(agg, filename = paste0(here("data/processed/alegana/"), names[i], "10x10.tif"), overwrite = TRUE)
  }

# Close out
stopCluster(cl)
Sys.time()
