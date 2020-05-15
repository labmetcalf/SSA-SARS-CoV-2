# ------------------------------------------------------------------------------------------------ #
#' Plotting bivariate risk maps      
#' Currently just working with demography (proportion of people > 55) vs. pseeks                                                                                 
# ------------------------------------------------------------------------------------------------ #

# load packages

library("here")
library("rnaturalearth")
library("rnaturalearthdata")
library("ggplot2")
library("sf")
library("RColorBrewer")

# load and clean data
health.indicators<-read.csv(here("data","processed","SSA.health.indicators.csv"),stringsAsFactors = F) #from Ben's github
demog<-read.csv(here("data","processed","world.pop.data.csv"),stringsAsFactors = F) 

## clean demog data

demog$Region..subregion..country.or.area..[45]<-"Ivory Coast"
demog$Region..subregion..country.or.area..[52]<-"Democratic Republic of the Congo"
demog$Region..subregion..country.or.area..[43]<-"Republic of Congo"
demog$Region..subregion..country.or.area..[80]<-"Guinea Bissau"
demog$Region..subregion..country.or.area..[166]<-"Somalia"
demog$Region..subregion..country.or.area..[62]<-"Swaziland"
demog$Region..subregion..country.or.area..[88]<-"Iran"
demog$Region..subregion..country.or.area..[171]<-"Palestine"
demog$Region..subregion..country.or.area..[176]<-"Syria"

sub.sahara.index<-which(ne_countries()$region_wb=="Sub-Saharan Africa")
north.africa.index<-which(ne_countries()$region_wb=="Middle East & North Africa")
sub.sahara.names<-ne_countries()$admin[sub.sahara.index]
sub.sahara.data.names<-sub.sahara.names
sub.sahara.data.names[36]<-"Somalia"
north.africa.names<-ne_countries()$admin[north.africa.index]
north.africa.data.names<-north.africa.names

demog<-cbind(demog,"pop"=rowSums(demog[,9:109])) #calculate total population
demog<-cbind(demog,"p.60.plus"=rowSums(demog[,69:109])/demog$pop)

## clean health indicators data
health.indicators$COUNTRY_NAME[grep("Cote d Ivoire",health.indicators$COUNTRY_NAME)]<-"Ivory Coast"
health.indicators$COUNTRY_NAME[grep("Djibouti ",health.indicators$COUNTRY_NAME)]<-"Djibouti"
health.indicators$COUNTRY_NAME[grep("Eswatini",health.indicators$COUNTRY_NAME)]<-"Swaziland"
health.indicators$COUNTRY_NAME[which(health.indicators$COUNTRY_NAME=="Republic of the Congo")]<-"Republic of Congo"
health.indicators$COUNTRY_NAME[grep("Tanzania",health.indicators$COUNTRY_NAME)]<-"United Republic of Tanzania"

# age structure 

## convenience functions
get.indicator.dat<-function(out.name,indicator_label_standard_name)
{
  assign("temp.out",subset(health.indicators,indicator_label_standard==indicator_label_standard_name))
  temp.out$value[which(is.na(temp.out$YEAR_recent))]<-NA #put in NAs where data is missing
  assign(out.name,temp.out,envir=.GlobalEnv)
}

get.value<-function(i,name.list,dataset,datacol,namescol="Region..subregion..country.or.area..")
{
  dataset[which(dataset[,namescol]==name.list[i]),datacol]
}

## params
p.infected<-.4 #ballpark estimate
p.symptomatic<-.8  # roughly matches https://www.medrxiv.org/content/10.1101/2020.03.19.20039107v1
hosp.rates<-c(.001,.003,.012,.032,.049,.102,.166,.243,.273) #data https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-NPI-modelling-16-03-2020.pdf
icu.rates.given.hosp<-c(.05,.05,.05,.05,.063,.122,.274,.432,.709) #data https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-NPI-modelling-16-03-2020.pdf
IFR <- c(.00002,.00006,.0003,.0008,.0015,.006,.022,.051,.093) #data https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-NPI-modelling-16-03-2020.pdf

age.class.columns<-list(9:18,19:28,29:38,39:48,49:58,59:68,69:78,79:88,89:109)

## calculate severe cases
severe.cases<-rep(NA,nrow(demog))

for (i in 1:nrow(demog))
{
  for(j in 1:length(hosp.rates))
  {
    severe.cases[i]<-sum(severe.cases[i],p.infected*p.symptomatic*hosp.rates[j]*sum(demog[i,age.class.columns[[j]]]),na.rm=T)
  }
}

demog<-cbind(demog,"severe.cases"=severe.cases)
demog<-cbind(demog,"severe.cases.per.capita"=demog$severe.cases/demog$pop)

# plotting functions
color.func<-function(x,y,alpha=.5*max(x,y)+.5*x*y)
{
  col.val<-rep(NA,3)
  if(x==0 && y==0) 
    {
      col.val[1]=col.val[2]=col.val[3]=255
    }
  else 
    {
      if (x>=y){col.val<-colorRamp(c("red2","purple3"))(y/x)}
      if (x<y){col.val<-colorRamp(c("mediumblue","purple3"))(x/y)}
    }
  rgb(col.val[1],col.val[2],col.val[3],alpha=alpha*255,maxColorValue = 255)
} 

# plot bivariate
get.indicator.dat("hand.wash","% Urban popn with handwashing facilities at home")

demog.vals<-c()
indicator.vals<-c()
for(i in (1:length(sub.sahara.index)))
{
  demog.vals<-c(demog.vals,get.value(i,sub.sahara.data.names,demog,"severe.cases"))
  indicator.vals<-c(indicator.vals,get.value(i,sub.sahara.data.names,hand.wash,"value","COUNTRY_NAME"))
}

demog.percentile<-ecdf(demog.vals)
indicator.percentile<-ecdf(indicator.vals)

sp::plot(0,0,type="n",xlim=c(-18,64),ylim=c(-36,41),asp=1,axes=F,xlab="",ylab="")

for(i in (1:length(sub.sahara.index))[-2])
{
demog.val<-demog.percentile(get.value(i,sub.sahara.data.names,demog,"severe.cases"))
indicator.val<-indicator.percentile(get.value(i,sub.sahara.data.names,hand.wash,"value","COUNTRY_NAME"))
if(isTRUE(!is.na(demog.val) && !is.na(indicator.val))) {col<-color.func(demog.val,indicator.val)} else {col<-"grey60"}
sp::plot(ne_countries(country=sub.sahara.names[i]),add=T,col=col)
}

# plot color legend
par(mar=c(4,4,4,4))
plot(0,0,xlim=c(-.05,1.05),ylim=c(-.05,1.05),axes=F,xlab="",ylab="",type="n")
for(i in seq(0,1,.1))
{
  for(j in seq(0,1,.1))
  {
    rect(i-.05,j-.05,i+.05,j+.05,col=color.func(i,j))
  }
}

#axis(1,at=seq(.0,.5,.1),labels=c(10,50,100,500,1000,1500),lwd=0,line=-1)
#axis(2,at=0:5)
mtext(side=1,"demography",line=2)
mtext(side=2,"health indicator",line=2.5)
rect(.2,1.1,.25,1.15,col="grey60")
par(xpd=T)
text(.25,1.125,labels="insufficient / missing data",pos=4)

