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
health.indicators$COUNTRY_NAME[grep("Republic of the Congo",health.indicators$COUNTRY_NAME)]<-"Republic of Congo"
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

# plot severe cases
plot.max<-1750 #world max is 70000
plot.col.pal<-colorRampPalette(RColorBrewer::brewer.pal(9,"YlOrRd"))
pop.cols<-plot.col.pal(plot.max)

sp::plot(0,0,type="n",xlim=c(-18,64),ylim=c(-36,41),asp=1,axes=F,xlab="",ylab="")

for(i in (1:length(sub.sahara.index))[-2])
{
  sp::plot(ne_countries(country=sub.sahara.names[i]),add=T,col=pop.cols[round(get.value(i,sub.sahara.data.names,demog,"severe.cases"))])
}

#for(i in 1:length(north.africa.names))
#{
#  sp::plot(ne_countries(country=north.africa.names[i]),add=T,col=pop.cols[round(get.value(i,north.africa.names,demog,"severe.cases"))])
#}

n.legend.points<-1000
bottom=-37
top=-2
left=-15
right=-10
tick.x<-right+1.5
text.x<-right+3
y.cords<-seq(bottom,top,length.out = n.legend.points)
space<-diff(y.cords)[1]/2
for(i in 1:n.legend.points)
{
  rect(left,y.cords[i]-space,right,y.cords[i]+space,border = NA,col=plot.col.pal(n.legend.points)[i])
}
rect(left,bottom-space,right,top+space)
segments(rep(right,5),seq(bottom,top,length.out = 5),rep(tick.x,5),seq(bottom,top,length.out = 5))
text(rep(text.x,5),seq(bottom,top,length.out = 5),labels=seq(0,plot.max,length.out = 5),cex=.75,adj=0)
mtext("Severe cases (in thousands)",cex=2)

# plot severe cases per capita
plot.max<-1700
plot.col.pal<-colorRampPalette(RColorBrewer::brewer.pal(9,"YlOrRd"))
pop.cols<-plot.col.pal(plot.max)

sp::plot(0,0,type="n",xlim=c(-18,64),ylim=c(-36,41),asp=1,axes=F,xlab="",ylab="")

for(i in (1:length(sub.sahara.index))[-2])
{
  sp::plot(ne_countries(country=sub.sahara.names[i]),add=T,col=pop.cols[round(100000*get.value(i,sub.sahara.data.names,demog,"severe.cases.per.capita"))])
}

#for(i in 1:length(north.africa.names))
#{
#  sp::plot(ne_countries(country=north.africa.names[i]),add=T,col=pop.cols[round(100000*get.value(i,north.africa.names,demog,"severe.cases.per.capita"))])
#}

n.legend.points<-1000
bottom=-37
top=-2
left=-15
right=-10
tick.x<-right+1.5
text.x<-right+3
y.cords<-seq(bottom,top,length.out = n.legend.points)
space<-diff(y.cords)[1]/2
for(i in 1:n.legend.points)
{
  rect(left,y.cords[i]-space,right,y.cords[i]+space,border = NA,col=plot.col.pal(n.legend.points)[i])
}
rect(left,bottom-space,right,top+space)
segments(rep(right,5),seq(bottom,top,length.out = 5),rep(tick.x,5),seq(bottom,top,length.out = 5))
text(rep(text.x,5),seq(bottom,top,length.out = 5),labels=seq(0,plot.max/100000,length.out = 5),cex=.75,adj=0)
mtext("Severe cases per capita",cex=2)

# plot handwashing
plot.max<-75 #world max is 70000
plot.col.pal<-colorRampPalette(RColorBrewer::brewer.pal(9,"YlOrRd"))
pop.cols<-plot.col.pal(plot.max)

sp::plot(0,0,type="n",xlim=c(-18,64),ylim=c(-36,41),asp=1,axes=F,xlab="",ylab="")

get.indicator.dat("hand.wash","% Urban popn with handwashing facilities at home")

for(i in (1:length(sub.sahara.index))[-2])
{
  sp::plot(ne_countries(country=sub.sahara.names[i]),add=T,col=pop.cols[get.value(i,sub.sahara.data.names,hand.wash,"value","COUNTRY_NAME")])
}

#for(i in 1:length(north.africa.names))
#{
#  sp::plot(ne_countries(country=north.africa.names[i]),add=T,col=pop.cols[round(get.value(i,north.africa.names,demog,"severe.cases"))])
#}

n.legend.points<-1000
bottom=-37
top=-2
left=-15
right=-10
tick.x<-right+1.5
text.x<-right+3
y.cords<-seq(bottom,top,length.out = n.legend.points)
space<-diff(y.cords)[1]/2
for(i in 1:n.legend.points)
{
  rect(left,y.cords[i]-space,right,y.cords[i]+space,border = NA,col=plot.col.pal(n.legend.points)[i])
}
rect(left,bottom-space,right,top+space)
segments(rep(right,5),seq(bottom,top,length.out = 5),rep(tick.x,5),seq(bottom,top,length.out = 5))
text(rep(text.x,5),seq(bottom,top,length.out = 5),labels=seq(0,plot.max,length.out = 5),cex=.75,adj=0)
mtext("% urban households\nwith handwashing facilities",cex=2)

