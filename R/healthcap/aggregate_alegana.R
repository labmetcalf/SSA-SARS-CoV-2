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

directories <- c("wp_data/Probability.tif", "wp_data/Traveltime")

files <- list.files(directories, recursive = TRUE, full.names = TRUE)
names <- list.files(directories, recursive = TRUE)
files <- files[grepl(".tif$", files)] # only tifs
names <- gsub(".tif", "", names[grepl(".tif$", names)])

foreach(i = 1:length(files), .packages = "raster",
        .export = c("files", "names")) %dopar% {
          toagg <- raster(files[i])
          agg <- aggregate(toagg, fact = 10, fun = mean, na.rm = TRUE) # take mean 
          names(agg) <- names[i]
          writeRaster(agg, filename = paste0("output/alegana", names[i], "10x10.tif"), overwrite = TRUE)
  }

# Close out
stopCluster(cl)
Sys.time()
