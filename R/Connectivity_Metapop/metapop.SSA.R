
## Set working directory      ###
setwd("/Users/jessicametcalf/Downloads/MetapopCodeData/")


## Bring in necessary package  ############################################################################
require(RColorBrewer)


## Build a stochastic simulator ~ 1 week time interval ~ approximates SARS-CoV-2 serial interval ###########
##
#@param connect.mat describes the connectivity matrix for all admin2 levels
#@param pop describes the population size for all admin2 levels in the same order as the connectiivity mat
#@param births describes the national birth rate per week
#@param beta describes the transmission rate (~approx R0 given that we are operating in the serial interval)
#@param Tmax number of time-steps (in weeks)
#
#@return list with infecteds through time, susceptibles through time, and number of introductions at each time-step

stochSim <- function(connect.mat=matrix(1,5,5), pop=rep(1e5,5),
	 births=1e5*((20/1000)/52),
	 beta = 3, Tmax=200) {

	 storeS <- matrix(0,length(pop),Tmax)
	 storeI <- matrix(0,length(pop),Tmax)
	 storeR <- matrix(0,length(pop),Tmax)
	 intros <- matrix(0,length(pop),Tmax)

	 ## initiate 
	 storeS[,1] <- pop
	 biggest <- which(pop==max(pop),arr.ind=TRUE)[1]
	 storeI[biggest,1] <- 1

	 for (t in 2:Tmax) { 
	     lambda <- 1-exp(-beta*(storeI[,t-1]^0.97)/pop)
	     storeI[,t] <- storeS[,t-1]*lambda
	     storeS[,t] <- pmax(storeS[,t-1] - storeI[,t]+births,1)
	     
	     intros[,t] <- rbinom(length(pop),1,1-exp(-connect.mat%*%matrix(storeI[,t],length(pop),1)/pop))
	     storeI[,t] <- storeI[,t] + intros[,t]

	     #ensure biggest doesn't go extinct
	     storeI[biggest,t] <- pmax(storeI[biggest,t],1)

	 }

	 return(list(storeI=storeI,storeS=storeS, intros=intros))

}







## Bring in the travel data from Weiss et al.     ######################################################################
df1<-read.csv("./data/ttimes_SSA.csv", stringsAsFactors=FALSE)

## Coordinates of the centroids of adminLevel 2
df.c<-read.csv("./data/gadm_centroids.csv", stringsAsFactors=FALSE)
mtch <- match(df1$Administrative.level.2.unit.code,df.c$feature_id)
## cbind(df1$Administrative.level.2.unit.code,df.c$feature_id[mtch])  #check match
df1$x <- df.c$x[mtch]
df1$y <- df.c$y[mtch]

## Births per 1000 per year for each country; match up some countries 
df.b <- read.csv("./data/WorldBankBirths1000Year.June.2020.download.csv", stringsAsFactors=FALSE)
df.b$Country.Name[df.b$Country.Name=="Cote d'Ivoire"] <- "Côte d'Ivoire"
df.b$Country.Name[df.b$Country.Name=="Congo, Dem. Rep."] <- "Democratic Republic of the Congo"
df.b$Country.Name[df.b$Country.Name=="Congo, Rep."] <- "Republic of Congo"
df.b$Country.Name[df.b$Country.Name=="Cabo Verde"] <- "Cape Verde"
df.b$Country.Name[df.b$Country.Name=="Gambia, The"] <- "Gambia"
df.b$Country.Name[df.b$Country.Name=="Eswatini"] <- "Swaziland"
df.b$Country.Name[df.b$Country.Name=="Sao Tome and Principe"] <- "São Tomé and Príncipe"
mtch <- match(df1$Country.name,df.b$Country.Name)
df1$births <- rowMeans(df.b[mtch,(ncol(df.b)-2):ncol(df.b)],na.rm=TRUE)
#unique(df1$Country.name[is.na(df1$births)])  ## 
df1$births[is.na(df1$births)] <- mean(df1$births,na.rm=TRUE)  ## replace missing births with mean for the region

# Input Files for accessibility
require(gdistance)
friction.surface.filename <- './data/2015_friction_surface_v1.geotiff'

## define unique countries
countries <- unique(df1$Country.name)

## storage for loop
Tmax <- 520   ## this is 10 years in weeks		
ninfected <- matrix(NA,nrow(df1),Tmax)
tot.time <- matrix(NA,length(countries),Tmax)  #tot infected by week in each country

nsim <- 50; rc <- rcI <- matrix(NA,nsim,Tmax);
prop.at.Tmax <- prop.at.1yr <- rc.time.to <- matrix(NA,length(countries),3); prop.reach <- rep(NA,length(countries)); test.prop <- 0.25
mean.admin2.delay <- matrix(NA,length(countries),3)
tstore <- rep(NA,length(countries))
firstExtinct <- Admin2notReached <- matrix(NA,length(countries),nsim)

#dev.off()
par(mfrow=c(5,2))

## do the loop
j<-1
for (j in 1:length(countries)) { 
    print(countries[j])

	## pick out the focal country ##
	chs <- which(df1$Country.name==countries[j], arr.ind=TRUE)
	df.here <- df1[chs,]

	## get the internal travel measure
	delay.till.minutes <- df.here$Mean.travel.time.to.the.nearest.urban.centre..mins.  ## this is in minutes
	mean.admin2.delay[j,] <- quantile(delay.till.minutes/60, c(0.25,0.5,0.75))

	## do accessibility using:  https://www.nature.com/articles/nature25181 
	do.run <- FALSE  ## - if already run, don't do it again, but pull in from storage 'output.filename'
	output.filename <- paste('./output/',countries[j],'study.area.accessibility.csv',sep="")
	if (sum(is.na(df.here$x))==nrow(df.here)) next()
	if (do.run) { 
	  points <- data.frame(X_COORD=df.here$x, Y_COORD=df.here$y) # Just 2 columns.  Structured as [X_COORD, Y_COORD] aka [LONG, LAT].  Use a header.
	  source('./R_generic_accessibilty_mapping_script.r', chdir = TRUE)
	} else {	
	  costTo <- as.matrix(read.csv(output.filename))	
	}
	
	connect.mat <- costTo 
	diag(connect.mat) <- 2                          ## an overall scalar
	connect.mat <- connect.mat[1,1]/connect.mat     ## assume higher friction results in lower connectivitiy
	diag(connect.mat) <-0                           ## a location shouldn't contribute to introductions to itself, make diagonaal zeros
	tstore[j] <- sum(connect.mat[which(df.here$pop==max(df.here$pop,na.rm=TRUE)),])

	#fix for Kenya (one population size missin)
	df.here$pop[is.na(df.here$pop)] <- 1

	## stochastic simulation
	do.stoch <- TRUE
	if (do.stoch) { 
    	for (s in 1:nsim) { 
           tmp <- stochSim(connect.mat=connect.mat,pop=df.here$pop,
	       	  births=df.here$pop*(df.here$births[1]/1000)/52,  ## assume same births for now
		  beta=3,Tmax=Tmax)
       	   rc[s,] <- cumsum(colSums(tmp$storeI,na.rm=TRUE))
	   rcI[s,] <- colSums(tmp$storeI,na.rm=TRUE)
	   Admin2notReached[j,s] <- sum(rowSums(tmp$storeI)==0)/nrow(tmp$storeI)
	   firstExtinct[j,s] <- which(colSums(tmp$storeI)<2)[2]
   	   }

        prop <- rc/sum(tmp$storeS[,1])
   	matplot((1:Tmax)/52,t(prop[,]), type="l",xlab="Time (years)", ylab="Fraction of the population", col="grey",lty=1, ylim=c(0,1), xlim=c(0,5))
   	title(countries[j]); abline(v=c(0:10),lty=3,col="grey")

   	matplot((1:Tmax)/52,t(rcI), type="l",xlab="Time (years)", ylab="Number of infected", col="grey",lty=1, log="y", xlim=c(0,5))
   	title(countries[j]); abline(v=c(0:10),lty=3,col="grey")

        time.to <- which(prop>test.prop,arr.ind=TRUE)
   	if (length(time.to)==0) time.to <- matrix(Tmax,1,2)
   	rc.time.to[j,] <- quantile(time.to[,2],c(0.25,0.5,0.75))
   	prop.reach[j] <- length(unique(time.to[,1]))/nsim
	prop.at.1yr[j,] <- quantile(prop[,52],c(0.25,0.5,0.75))
	prop.at.Tmax[j,] <- quantile(prop[,Tmax],c(0.25,0.5,0.75))
	}
	
}

dev.off()

## Dotchart of number admin2s not reached
ad2nr <- t(apply(Admin2notReached,1,quantile,c(0.025,0.5,0.975),na.rm=T))
o1 <- order(ad2nr[,2])
plot(ad2nr[o1,2],1:length(countries), axes=FALSE, xlab="Proportion admin2 not reached", ylab="",  xlim=range(ad2nr,na.rm=TRUE))
axis(1)
axis(2, at=1:length(countries),lab=countries[o1], las=2, cex.axis=0.5)
for (j in 1:length(countries)) points(ad2nr[o1[j],c(1,3)],c(j,j), type="l",col="grey")
points(ad2nr[o1,2],1:length(countries),pch=19)


## Dotchart of time first extinction - right boundary is 'never'; and below is prop reached in the same order
par(mfrow=c(1,2),mar=c(5,9,2,2))
fstE <- t(apply(pmin(firstExtinct,max(firstExtinct,na.rm=TRUE)+10,na.rm=TRUE),1,quantile,c(0.025,0.5,0.975),na.rm=T))
o1 <- order(fstE[,2])
plot(fstE[o1,2]/52,1:length(countries), axes=FALSE, xlab="Time of first extinction", ylab="",  xlim=range(fstE/52,na.rm=TRUE))
axis(1); axis(2, at=1:length(countries),lab=countries[o1], las=2, cex.axis=0.5)
for (j in 1:length(countries)) points(fstE[o1[j],c(1,3)]/52,c(j,j), type="l",col="grey")
points(fstE[o1,2]/52,1:length(countries),pch=19)

par(mar=c(5,7,2,2))
plot(prop.at.1yr[o1,2],1:length(countries), axes=FALSE, xlab="Fraction pop reached in one year", ylab="", xlim=range(prop.at.1yr,na.rm=TRUE))
axis(1); axis(2, at=1:length(countries),lab=countries[o1], las=2, cex.axis=0.4)
for (j in 1:length(countries)) points(prop.at.1yr[o1[j],c(1,3)],c(j,j), type="l",col="grey")
points(prop.at.1yr[o1,2],1:length(countries),pch=19)




### Show time-series of three places, and the overall patterns ###

pdf("/Users/jessicametcalf/Downloads/figCountriesMetapop.pdf", width=15,height=10)


nsim <- 10
layout(matrix(c(1,1,2,2,3,3,4,5),2,4))
par(bty="l", mar=c(5,2,3,1))

chs.countries <- c("Malawi", "Gabon", "Madagascar")

for (j in 1:length(chs.countries)) { 
   
	## pick out the focal country ##
	chs <- which(df1$Country.name==chs.countries[j], arr.ind=TRUE)
	df.here <- df1[chs,]
	output.filename <- paste('/Users/jessicametcalf/Downloads/DHS.all.3/output/',chs.countries[j],'study.area.accessibility.csv',sep="")
	costTo <- as.matrix(read.csv(output.filename))	
	
	connect.mat <- costTo
	diag(connect.mat) <- 2
	connect.mat <- connect.mat[1,1]/connect.mat
	diag(connect.mat) <-0 ## shouldn't contribute to introductions to one's own place
	tstore[j] <- sum(connect.mat[which(df.here$pop==max(df.here$pop,na.rm=TRUE)),])

	## stochastic simulation
    	for (s in 1:nsim) { 
           tmp <- stochSim(connect.mat=connect.mat,pop=df.here$pop,
	       	  births=df.here$pop*(df.here$births[1]/1000)/52,  ## assume same births for now
		  beta=3,Tmax=Tmax)
       	   rc[s,] <- cumsum(colSums(tmp$storeI,na.rm=TRUE))
	   rcI[s,] <- colSums(tmp$storeI,na.rm=TRUE)
   	   }

   	matplot((1:Tmax)/52,t(rcI[1:nsim,]), type="l",xlab="Time (years)", ylab="Number of infected", col="grey",lty=1, log="y", xlim=c(0,5))
   	title(paste(chs.countries[j],"-like", sep="")); abline(v=c(0:10),lty=3,col="grey")
}

par(bty="l", mar=c(5,4,3,3))

## Look for hump shaped distribution - fast that get everywhere, or fast that stall
plot(prop.at.1yr[,2],fstE[,2], xlab = "Proportion pop reached in 1 year", ylab="Time first extinction", log="y", pch=19)
for (j in 1:length(countries)) points(prop.at.1yr[j,c(1,3)],fstE[c(j,j),2], type="l",col="grey")
for (j in 1:length(countries)) points(prop.at.1yr[c(j,j),2],fstE[j,c(1,3)], type="l",col="grey")
points(prop.at.1yr[,2],fstE[,2], pch=19)
text(prop.at.1yr[,2],fstE[,2],countries, col=4, cex=0.4, pos=1)


## Compare between and within using regions that escape
plot(ad2nr[,2],mean.admin2.delay[,2], xlab = "Between region spread (prop admin2 unreached)", ylab="Within region spread (travel time, hrs)",
				      log="y", pch=19, ylim=c(0.2,20))
for (j in 1:length(countries)) points(ad2nr[j,c(1,3)],mean.admin2.delay[c(j,j),2], type="l",col="grey")
for (j in 1:length(countries)) points(ad2nr[c(j,j),2],mean.admin2.delay[j,c(1,3)], type="l",col="grey")
points(ad2nr[,2],mean.admin2.delay[,2], pch=19)
text(ad2nr[,2],mean.admin2.delay[,2],countries, col=4, cex=0.4, pos=1)

dev.off()


### Write the data out ##
#data.for.metapop <- data.frame(countries,prop.at.1yr[,2])
#write.csv(data.for.metapop,"./data/dataMetapop.csv")






### Comparison of predictions with testing  #####################################################################
data.for.metapop <- read.csv("./data/dataMetapop.csv", header=TRUE, stringsAsFactors=FALSE)

## Bring in testing data, sort out the names ##
data.testing <- read.csv("./data/manual_africa_cdc_data_20200629.csv", header=TRUE, stringsAsFactors=FALSE)
data.testing$COUNTRY_NAME[data.testing$COUNTRY_NAME=="Cabo Verde"] <- "Cape Verde"
data.testing$COUNTRY_NAME[data.testing$COUNTRY_NAME=="Cote d Ivoire"] <- "Côte d'Ivoire"
data.testing$COUNTRY_NAME[data.testing$COUNTRY_NAME=="Equatorial Guinea "] <- "Equatorial Guinea"
data.testing$COUNTRY_NAME[data.testing$COUNTRY_NAME=="Eswatini"] <- "Swaziland"
data.testing$COUNTRY_NAME[data.testing$COUNTRY_NAME=="Guinea Bissau"] <- "Guinea-Bissau"
data.testing$COUNTRY_NAME[data.testing$COUNTRY_NAME=="Republic of the Congo"] <- "Republic of Congo"
data.testing$COUNTRY_NAME[data.testing$COUNTRY_NAME=="Sao Tome and Principe"] <- "São Tomé and Príncipe"


## Line up the data 
mtch <- match(data.testing$COUNTRY_NAME,data.for.metapop$countries)
#cbind(data.testing$COUNTRY_NAME,data.for.metapop$countries[mtch])
data.testing$prop1yr <- data.for.metapop$prop.at.1yr...2.[mtch]

par(mfrow=c(1,2),bty="l")
plot(data.testing$cases, data.testing$prop1yr,log="x",
     ylab="Expected proportion after 1 year (metapop)", xlab="Number of cases reported by June 28th", pch=19)
cor.test(log(data.testing$cases), data.testing$prop1yr)

plot(data.testing$cases/data.testing$tests, data.testing$prop1yr,log="x",
     ylab="Expected proportion after 1 year (metapop)", xlab="Test positivity reported by June 28th", pch=19)

cor.test(log(data.testing$cases/data.testing$tests), data.testing$prop1yr)


