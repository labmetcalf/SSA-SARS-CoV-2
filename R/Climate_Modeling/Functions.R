
## wrapper for running the SIRS model
runModel  <- function(pop = 3000000,
                                                R0max = 2.2,
                                                R0min = 1.2,
                                                Immunity = 40, # weeks that correspond to duration of immunity
                                                LatCity = 40.7128,
                                                LonCity = -74.6, SSet = "Orig", Lead = 0, Var = -180, birthrate=0,
                                                timestart = 0, timeend = 0, R0fixed = 1, timeLengthSim = 364, SHDAT = SHDAT){
  
  library(deSolve)
  library(doBy)
  library(viridis)
  library("raster")
  library("lubridate")
  library("ncdf4")
  library("psych")
  library("sp")
  library("maps")
  library("maptools")
  library("rgdal")

  latlist <- unique(SHDAT$lat)
  lonlist <- unique(SHDAT$lon)
  latid <- latlist[which.min(abs(latlist - LatCity))]
  lonid <- lonlist[which.min(abs(lonlist - LonCity))]
  
  SHDATRow <- as.numeric(SHDAT[SHDAT$lat==latid & SHDAT$lon==lonid,])
  
  qout <- as.numeric(SHDATRow[3:54])
  if(Lead==0){
  qout <- rep(qout, each = 7)}
  if(Lead > 0){
    qout <- rep(qout, each = 7)
    qoutNew <- c(qout[Lead:length(qout)], qout[1:(Lead - 1)])
    qout <- qoutNew
  }
  if(SSet == "Orig"){ S = pop - 1}
  if(SSet != "Orig"){
    S  = pop*SSet}
  
  I = 1
  R = pop - S - I
  xstart = c(S = S, I = I, R = R)
  times = seq(1, timeLengthSim, by = 1)
  qList <- rep(qout, length = length(times))
  paras = list(D = 5, L = Immunity, R0max = R0max, R0min = R0min, qList = qList, var = Var, birthrate = birthrate,
               timestart = timestart, timeend = timeend, R0fixed= R0fixed)
  
  # with all parameters set, run model
  out = as.data.frame(ode(xstart, times, SIRS_InterventionGlobal, paras))
  
  if(Lead > 0){ dfadd <- data.frame(time = rep(0,times = Lead), S = rep(pop, times = Lead), I = rep(0, times = Lead), R =rep(0, times = Lead) )
  out <- rbind(dfadd, out)   }
  
  return(out)
}




SIRS_InterventionGlobal <- function(time, state ,theta) {
  #browser()
  ## Parameters:
  qList <- theta[["qList"]]
  D <- theta[["D"]]
  L <- theta[["L"]]
  R0base <- theta[["R0max"]]
  R0min <- theta[["R0min"]]
  var <- theta[["var"]]
  birthrate <- theta[["birthrate"]]
  timestart <- theta[["timestart"]]
  timeend <- theta[["timeend"]]
  R0fixed <- theta[["R0fixed"]]
  
  #derived
  quse <- qList[time]
  
  ## States:
  S <- state["S"]
  I <- state["I"]
  R <- state["R"]
  N <- S + I + R
  
  ## ODEs:
  R0 = exp(var*quse + log(R0base - R0min)) + R0min
  if(time > timestart & time <= timeend){
    R0 = R0fixed
  }

  beta = R0/D
  dS <- birthrate*N + (R/L) -beta * S * I/N - S*birthrate
  dI <- beta * S * I/N - (I/D) - I*birthrate
  dR <- (I/D) - (R/L) - R*birthrate
  
  return(list(c(dS, dI, dR)))
}





grabR0Quick  <- function(R0min = 1.5, R0max = 2.5,
                            LatCity = 40.7128,
                            LonCity = -74.6, Var = -180, 
                             SHDAT = SHDAT){
  

  latlist <- unique(SHDAT$lat)
  lonlist <- unique(SHDAT$lon)
  latid <- latlist[which.min(abs(latlist - LatCity))]
  lonid <- lonlist[which.min(abs(lonlist - LonCity))]
  
  SHDATRow <- as.numeric(SHDAT[SHDAT$lat==latid & SHDAT$lon==lonid,])
  
  qout <- as.numeric(SHDATRow[3:54])
  R0 = exp(Var*qout + log(R0max - R0min)) + R0min
  
  
  return(R0)
}



