#Function
library("pals")
library(geosphere)
require("plotrix")
require("raster")
require("rgdal")

HomeDirectory <- "~/Dropbox/Coronavirus/CoAuthorWork/AfrClimFiles"

# load continental shapefile
source('Functions.R', encoding = 'UTF-8')
setwd("TM_WORLD_BORDERS")
overlay <- readOGR(".","TM_WORLD_BORDERS-0.3")
overlay <- spTransform(overlay, CRS("+proj=longlat +datum=WGS84"))
suboverlay <- overlay[overlay$REGION==2,] # African continent
suboverlay$namechr <- as.character(suboverlay$NAME)
suboverlay2 <- suboverlay[suboverlay$namechr %in% c("Western Sahara","Morocco","Egypt","Libyan Arab Jamahiriya","Tunisia","Algeria"),]
# load humidity data, trim to within land-mass lines
setwd(HomeDirectory)
q  <- brick("era5.q2m.w1clim1981-2017.nc")
q <- rotate(q)
qmask <- mask(q,overlay)
qpt <- rasterToPoints(qmask)
qpt <- as.data.frame(qpt)
names(qpt)[1:2] <- c("lon","lat")


latlon <- read.csv("20200413 cities list_provisional_w_coords.csv")

plot(seq(1,2,1/364),seq(0,0.2,l=365),type="n")
outall <- NULL
dfmaxs <- NULL
for(i in 1:nrow(latlon)){
  pred <- runModel(R0min = 1.5, R0max = 2.5, Immunity = 66.25*7, LatCity = latlon$Latitude[i], 
                   LonCity = latlon$Longitude[i], Var = -227.5, SHDAT = qpt, pop = 8e06, Lead = 60)
  latalt <- 0
  if(is.na(pred$I[100])){  
    
    latmax = latlon$Latitude[i] + 2.5
    latmin = latlon$Latitude[i] - 2.5
    lonmax = latlon$Longitude[i] + 2.5
    lonmin = latlon$Longitude[i] - 2.5
    qsub <- qpt[qpt$lon <= lonmax & qpt$lon >= lonmin & qpt$lat >= latmin & qpt$lat <= latmax,]
    if(nrow(qsub) > 0){
    mindistlist <- rep(NA, length = length(qsub$lon))
    for(j in 1:nrow(qsub)){
      mindist <- distm(c(latlon$Longitude[i], latlon$Latitude[i]), c(qsub$lon[j], qsub$lat[j]), fun = distHaversine)
      mindistlist[j] <- mindist
    }
    minid <- which.min(mindistlist)
    pred <- runModel(R0min = 1.5, R0max = 2.5, Immunity = 66.25*7, LatCity = qsub$lat[minid], 
                                           LonCity = qsub$lon[minid], Var = -227.5, SHDAT = qpt, pop = 8e06, Lead = 60)
  latalt <- 1}
  }
  lines(seq(1,5,1/364)[1:length(pred$I)],pred$I/8e06, col=parula(58)[i])
  predfin <- c(as.character(latlon$Country[i]), as.character(latlon$City[i]),
               latlon$Latitude[i], latlon$Longitude[i], latalt, pred$I/8e+06) 
  outall <- rbind(outall, predfin)
  dfmaxs <- rbind(dfmaxs ,data.frame(Lat = latlon$Latitude[i], Lon = latlon$Longitude[i], maxI = max(pred$I/8e+06)) )
  
}

outall <- as.data.frame(outall)
names(outall)[1:5] <- c("Country","City","Lat","Lon","NearestLandBased")
write.csv(outall, file="TSAfrica.csv")

qmask <- mask(q,suboverlay)
minq <- calc(qmask, fun = min)
maxq <- calc(qmask, fun = max)
rangeq <- maxq - minq
dfmaxs <- dfmaxs[!is.na(dfmaxs$maxI),]

pdf(width=6,height=6,file='ClimMap.pdf')
par(mar=c(1,1,1,1))
plot(rangeq,col = rev(brewer.spectral(30)),xlim=c(-25,60),ylim=c(-40,40),
     xlab='',ylab='',bty='n',box=F,axes=F, legend = F)
plot(suboverlay2, add = T, col="grey32", border = "white")
rbPal <- colorRampPalette(c('white','black'))
dfmaxs$col <- rbPal(10)[as.numeric(cut(c(dfmaxs$maxI,0,0.15),breaks = 10))][1:nrow(dfmaxs)] # fix palette between 0 and 0.14
points(dfmaxs$Lon, dfmaxs$Lat, pch = 21, bg=dfmaxs$col, cex=1.2)
gradient.rect(-15,-17,5,-15,col=colorRampPalette(c('white','black'))(30))
text(seq(-15,5,l=4),rep(-13,5),c(0,0.05,0.10,0.15))
gradient.rect(-15,-28,5,-26,col=rev(brewer.spectral(30)))
text(seq(-15,5,l=4),rep(-24,5),1000*seq(0,0.015,l=4))

text(-5,-30,'Range q (g/kg)')
text(-5,-19,'Peak I/N')
dev.off()





