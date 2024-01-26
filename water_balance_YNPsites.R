#This code derives monthly water balance for each site at YNP

setwd("C:\\Users\\fridley\\OneDrive - Clemson University\\academic\\projects\\YNP_Doug")
#setwd("C://Users/dafrank/OneDrive - Syracuse University/Desktop/2023 OPUS/Data/fielddata/Composite")
#dat<-read.csv(file="datcorrected1016.csv")
dat<-read.csv(file="dat011624.csv")
sort(unique(dat$yr))
  #from 1988 to 2014; start with 1987

latlon = read.csv("C:\\Users\\fridley\\OneDrive - Clemson University\\academic\\projects\\YNP_Doug\\latitudes.csv")
latlon

#climate data from PRISM
library(prism)
library(ggplot2)
#location of downloaded PRISM datasets (see https://prism.oregonstate.edu/)
prism.dir = "C:\\Users\\fridley\\OneDrive - Clemson University\\academic\\gis\\prism\\"
prism_set_dl_dir(prism.dir)
#get_prism_monthlys(type = "tmean", year = 1900:2020, mon = 1:12, keepZip = FALSE) #takes ~ hr
#get_prism_monthlys(type = "ppt", year = 1900:2020, mon = 1:12, keepZip = FALSE) #take ~ hr
prism_archive_ls()
to_slice <- prism_archive_subset("tmean", "monthly", mon = 1:12,years=c(1987:2014))
pt_slice <- prism_archive_subset("ppt", "monthly", mon = 1:12, years=c(1987:2014))

#create matrix of monthly data, one for temp, one for precip
temp = matrix(0,nrow=dim(latlon)[1],ncol=12*28)
rain = matrix(0,nrow=dim(latlon)[1],ncol=12*28)
for(i in 1:dim(latlon)[1]) {
    print(i)
    temp[i,] = pd_plot_slice(to_slice,c(latlon[i,3],latlon[i,2]))$data$data
    rain[i,] = pd_plot_slice(pt_slice,c(latlon[i,3],latlon[i,2]))$data$data
}
prism.date = as.Date(pd_plot_slice(to_slice,c(latlon[1,3],latlon[1,2]))$data$date)

#inspect
for(i in 1:27) {
  plot(prism.date,temp[i,],type="l")
  readline()
}
  
for(i in 1:27) {
  plot(prism.date,rain[i,],type="l")
  readline()
}
colnames(temp) = prism.date
rownames(temp) = latlon$site

#save(temp,rain,prism.date,file="YNPprism.RData")
#load("C:\\Users\\fridley\\OneDrive - Clemson University\\academic\\projects\\YNP_Doug\\YNPprism.RData")

#ET (mm) and standardized 
#Pederson paper use Thornthwaite for PET, calculating SPEI for 6- and 12-mo intervals
#"SPEI is a multiscalar climatic drought index (i.e. it can be calculated for different temporal scales)
#that considers precipitation and the effect of temperature on drought severity through the inclusion of 
#evapotranspiration (Vicente-Serrano et al., 2010). Here, we used the Thornthwaite equation to estimate 
#potential evapotranspiration (Thornthwaite, 1948) and calculated SPEI for 6- and 12-month periods."
#Also: July SPEI6 represents the standardized difference between precipitation and potential evapotranspiration from February to July.
  #note standardized means the mean is always zero, so can't compare mean values across sites
library(SPEI)

#create parallel matrices for PET, SPEI, and WB

pet = matrix(0,nrow=dim(latlon)[1],ncol=12*28)
spei = matrix(0,nrow=dim(latlon)[1],ncol=12*28)
wb = matrix(0,nrow=dim(latlon)[1],ncol=12*28)

for(i in 1:dim(latlon)[1]) {
  print(i)
  pet[i,] = thornthwaite(Tave=temp[i,],lat=latlon[i,2])
  spei[i,] = spei(rain[i,] - pet[i,], scale = 6)$fitted
  wb[i,] = rain[i,] - pet[i,]
}
  
#inspect
for(i in 1:27) {
  plot(prism.date,pet[i,],type="l")
  readline()
}

#inspect
for(i in 1:27) {
  plot(prism.date,spei[i,],type="l")
  readline()
}

#inspect
for(i in 1:27) {
  plot(prism.date,wb[i,],type="l"); abline(h=0)
  readline()
}

#add to dataset
dat$spei = NA
dat$watbal = NA
dat$pet = NA

#loop through dataset and add appropriate values;
  #if day of the month end date is 15 or before, use previous month; if after use current month
for(i in 1:dim(dat)[1]) {
  site = dat$site[i]
  per = regexpr("\\.",dat$end.date[i])[1] #position of period
  mo = as.numeric(substr(dat$end.date[i],1,per-1)) #month
  day = as.numeric(substr(dat$end.date[i],per+1,10)) #day
  mo2 = if(day>15) mo else mo-1 #take this month or previous month
  col = (dat$yr[i]-1987)*12 + mo2 #column of associated year and month
  row = c(1:27)[latlon$site==site]
  dat$watbal[i] = wb[row,col]
  dat$spei[i] = spei[row,col]
  dat$pet[i] = pet[row,col]
}
save(dat,file="YNP_Jan25.RData")

plot(dat$pet,dat$watbal)
plot(dat$pet,dat$prismint.temp)
plot(dat$watbal,dat$prismint.ppt)
  #seems ok


#plotting: for later

plot(prism.df$prism.date,sugar.SPEI,type="l",col="white")
  x = c(prism.df$prism.date[1],prism.df$prism.date,tail(prism.df$prism.date,1))
  y1 = c(0,sugar.SPEI,0); y1[y1<0] = 0; y1[1:6] = 0
  y2 = c(0,sugar.SPEI,0); y2[y1>0] = 0; y2[1:6] = 0
polygon(x,y1,col="blue",border="black",lwd=.5)
polygon(x,y2,col="red",border="black",lwd=.5)
ls = predict(loess(sugar.SPEI ~ as.numeric(prism.df$prism.date),span=.2)) #smooth for display
lines(as.numeric(prism.df$prism.date)[-c(1:5)],ls,lwd=3)


plot(prism.df$prism.date,cling.SPEI,type="l",col="white")
x = c(prism.df$prism.date[1],prism.df$prism.date,tail(prism.df$prism.date,1))
y1 = c(0,cling.SPEI,0); y1[y1<0] = 0; y1[1:6] = 0
y2 = c(0,cling.SPEI,0); y2[y1>0] = 0; y2[1:6] = 0
polygon(x,y1,col="blue",border="black",lwd=.5)
polygon(x,y2,col="red",border="black",lwd=.5)

plot(prism.df$prism.date,sugar.BAL,type="l",col="white")
x = c(prism.df$prism.date[1],prism.df$prism.date,tail(prism.df$prism.date,1))
y1 = c(0,sugar.BAL,0); y1[y1<0] = 0; y1[1:6] = 0
y2 = c(0,sugar.BAL,0); y2[y1>0] = 0; y2[1:6] = 0
polygon(x,y1,col="blue",border="blue",lwd=.5)
polygon(x,y2,col="red",border="red",lwd=.5)
ls = predict(loess(sugar.BAL ~ as.numeric(prism.df$prism.date),span=.2)) #smooth for display
lines(as.numeric(prism.df$prism.date),ls,lwd=3)

plot(prism.df$prism.date,cling.BAL,type="l",col="white")
x = c(prism.df$prism.date[1],prism.df$prism.date,tail(prism.df$prism.date,1))
y1 = c(0,cling.BAL,0); y1[y1<0] = 0; y1[1:6] = 0
y2 = c(0,cling.BAL,0); y2[y1>0] = 0; y2[1:6] = 0
polygon(x,y1,col="blue",border="blue",lwd=.5)
polygon(x,y2,col="red",border="red2",lwd=.5)
ls = predict(loess(cling.BAL ~ as.numeric(prism.df$prism.date),span=.2)) #smooth for display
lines(as.numeric(prism.df$prism.date),ls,lwd=3)


#climate variables for plotting
par(mfcol=c(4,2),mar=c(0,5,0,0),oma=c(3,1,3,1))
sugar.col = "orange"
cling.col = "darkgreen"

plot(clim$year,clim$sugar.MAT,type="l",ylim=c(8,14),lwd=3,col=sugar.col,xlab="",xaxt="n",ylab="MAT (°C)",
    cex.axis=1,cex.lab=1.3)
lines(clim$year,clim$cling.MAT,type="l",col=cling.col,lwd=3)
axis(side=1,labels=F)
axis(side=3,cex.axis=1.2)
abline(v=c(1900,1920,1940,1960,1980,2000,2020),lty=2,col="gray",lwd=.5)

plot(clim$year,clim$sugar.TAP/10,type="l",ylim=c(100,280),col=sugar.col,xlab="",xaxt="n",ylab="Rainfall (cm)",
     cex.lab=1.3)
ls = predict(loess(sugar.TAP/10 ~ clim$year,span=.2)) #smooth for display
lines(clim$year,ls,lwd=3,col=sugar.col)
lines(clim$year,clim$cling.TAP/10,type="l",col=cling.col)
ls = predict(loess(cling.TAP/10 ~ clim$year,span=.2)) #smooth for display
lines(clim$year,ls,lwd=3,col=cling.col)
axis(side=1,labels=F)
abline(v=c(1900,1920,1940,1960,1980,2000,2020),lty=2,col="gray",lwd=.5)

plot(prism.df$prism.date,sugar.BAL,type="p",col=sugar.col,pch=".",ylim=c(-100,500),xlab="",xaxt="n",
     ylab="Water balance (mm)",cex.lab=1.3,xlim=as.Date(c("1900-01-01","2020-01-01")),cex=2)
ls = predict(loess(sugar.BAL ~ as.numeric(prism.df$prism.dat),span=.2)) #smooth for display
lines(as.numeric(prism.df$prism.date),ls,lwd=3,col=sugar.col)
abline(h=0,lty=2,col="black")
points(prism.df$prism.date,cling.BAL,type="p",col=cling.col,pch=".",cex=2)
ls = predict(loess(cling.BAL ~ as.numeric(prism.df$prism.date),span=.2)) #smooth for display
lines(as.numeric(prism.df$prism.date),ls,lwd=3,col=cling.col)
abline(h=0,lty=2,col="black")
axis(side=1,labels=F,at=as.Date(c("1900-01-01","1920-01-01","1940-01-01","1960-01-01","1980-01-01","2000-01-01","2020-01-01")))
abline(v=as.Date(c("1900-01-01","1920-01-01","1940-01-01","1960-01-01","1980-01-01","2000-01-01","2020-01-01")),lty=2,col="gray",lwd=.5)

plot(prism.df$prism.date,sugar.SPEI,type="l",col="white",xlab="",xaxt="n",
     ylab="SPEI",cex.lab=1.3,xlim=as.Date(c("1900-01-01","2020-01-01")),cex=2)
x = c(prism.df$prism.date[1],prism.df$prism.date,tail(prism.df$prism.date,1))
y1 = c(0,sugar.SPEI,0); y1[y1<0] = 0; y1[1:6] = 0
y2 = c(0,sugar.SPEI,0); y2[y1>0] = 0; y2[1:6] = 0
polygon(x,y1,col="blue",border="black",lwd=.5)
polygon(x,y2,col="red",border="black",lwd=.5)
ls = predict(loess(sugar.SPEI ~ as.numeric(prism.df$prism.date),span=.2)) #smooth for display
lines(as.numeric(prism.df$prism.date)[-c(1:5)],ls,lwd=3)
axis(side=1,labels=c(1900,1920,1940,1960,1980,2000,2020),cex.axis=1.2,at=as.Date(c("1900-01-01","1920-01-01","1940-01-01","1960-01-01","1980-01-01","2000-01-01","2020-01-01")))
abline(v=as.Date(c("1900-01-01","1920-01-01","1940-01-01","1960-01-01","1980-01-01","2000-01-01","2020-01-01")),lty=2,col="gray",lwd=.5)



#agents of change: year vectors

time = c(1900:2023)
N = length(time)

#ozone: Look Rock, 1989-2016, June-July-Aug 24 hr W126 Maxima, Neufeld et al. 2019 supplement, ppm-hrs
ozone = c(18.608,40.803,23.451,19.066,43.071,30.267,NA,54.118,50.481,53.562,69.284,48.991,31.401,69.790,35.564,26.222,23.657,43.955,43.538,35.669,12.355,19.152,40.791,23.328,5.383,4.854,5.041,8.341)
oz.yrs = c(1989:2016)
plot(oz.yrs,ozone,type="l")

#NOx emissions
#TVA from 1970s
#https://www.tva.com/Environment/Environmental-Stewardship/Air-Quality/Nitrogen-Oxides
#also shown in Neufeld et al. 2019
nox.yrs1 = c(1974:2018)
#nox in tons per year
nox.tva = c(395042,370960,434352,410299,394159,375975,393331,377320,304664,310195,302708,356072,384226,369676,391712,        365292,        404305,        405080,        464215,        508013,        469079,        534012,        495625,       511157,        414390,        357554,        286142,        270815,        263562,        236822,        199812,        191305,        198122,        196278,      168434,
        58497,72139,63645,54619,46898,51281,43682,40706,30297,23640)
#from EPA report 1985, from 1900 although larger area
nox.yrs2 = seq(1900,1970,by=5)
nox.epa = c(25000,33000,38000,43000,50000,60000,100000,80000,90000,100000,120000,150000,180000,250000,320000)
nox.yrs = c(nox.yrs2,nox.yrs1)
nox = c(nox.epa,nox.tva)
nox = nox/(1102) #convert to metric kiloton

plot(nox.yrs,nox,type="l")
par(new=T)
plot(oz.yrs,ozone,type="l",col="blue",xlim=c(1900,2018))

#CO2
#values until 1958 are from Friedli et al. 1986 Nature: https://www.nature.com/articles/324237a0.pdf
#Mauna Loa values are from NOAA: https://gml.noaa.gov/ccgg/trends/data.html
co2 = read.csv("co2_1900to_today.csv")
co2$date = as.Date(paste0(co2$year,"-",co2$month,"-1"))
plot(co2$date,co2$average,type="l")

#invasive insects, disease
chestnut.year = 1925
balsam.year = 1959
beechbark.year = 1986
dogwood.year = 1987
hemlock.year = 2002
chestnut = rep(0,N); chestnut[time>=chestnut.year] = 1
balsam = rep(0,N); balsam[time>=balsam.year] = 1
beechbark = rep(0,N); beechbark[time>=beechbark.year] = 1
dogwood = rep(0,N); dogwood[time>=dogwood.year] = 1
hemlock = rep(0,N); hemlock[time>=hemlock.year] = 1

#mammalian herbivore abundance
#deer
deer = rep(0,N)
#Briggs et al. 2006 Fig 1, per sq. km
deer[is.element(time,c(1970,1971,1973,1979,1980,1981,1989))] = c(36,10,34,44,24,23,37)
#Briggs estimates
deer[is.element(time,c(1930,1935,1940,1945,1950,1955,1960,1965,1990,1995,2000,2005))] = c(0,2,3,4,6,10,14,20,38,37,36,34)
plot(time,deer)
time2 = time[deer>0]
deer2 = deer[deer>0]
plot(time2,deer2,type="l")

#elk
elk = c(rep(0,100),seq(52,200,length=24))
#hogs
hogs = c(rep(0,length=40),seq(1,500,length=20),seq(500,1500,length=20),rep(1500,44))
hogs.spline = predict(loess(hogs ~ time,span=.3)) #smooth for display
hogs.spline[hogs.spline<0] = 0
hogs.spline[hogs.spline>max(hogs)] = max(hogs)

plot(time,hogs.spline,type="l",lwd=2)
lines(time,elk,lwd=2,col="purple")

plot(time,chestnut,type="n",yaxt="n",ylab="",xlab="",ylim=c(0,6))
lines(time[chestnut==1],rep(1,sum(chestnut)),col="brown",lwd=2)
  text(chestnut.year-8,1,"CB",cex=1.4,col="brown")
lines(time[balsam==1],rep(2,sum(balsam)),col="darkgreen",lwd=2)
  text(balsam.year-10,2,"BWA",cex=1.4,col="darkgreen")
lines(time[beechbark==1],rep(3,sum(beechbark)),lwd=2,col="darkgray")
  text(beechbark.year-10,3,"BBD",cex=1.4,col="darkgray")
lines(time[dogwood==1],rep(4,sum(dogwood)),lwd=2,col="lightblue")
  text(dogwood.year-8,4,"DA",cex=1.4,col="lightblue")
lines(time[hemlock==1],rep(5,sum(hemlock)),lwd=2,col="green")
  text(hemlock.year-10,5,"HWA",cex=1.4,col="green")
text(1950,5.8,"Invasive insects, disease",cex=1.5)

#logging and fire
#invasive insects, disease
corp.log = rep(0,N); corp.log[1:31] = 1
fire.sup = rep(0,N); fire.sup[31:97] = 1
fire.effects = rep(0,N); fire.effects[97:N] = 1
fire.scars = rep(0,N)
  fire.scars[1:5] = .8
  fire.scars[6:15] = 1.3
  fire.scars[16:25] = .1
  fire.scars[26:35] = .2

plot(time,corp.log,type="n",yaxt="n",ylab="",xlab="",ylim=c(0,3.5))
lines(time[corp.log==1],rep(3,sum(corp.log)),lwd=3)
arrows(1930,2.85,1930,3.15,length=.1,code=1,angle=0,lwd=3)
lines(time[fire.sup==1],rep(2,sum(fire.sup)),lwd=3)
arrows(1930,1.85,1930,2.15,length=.1,code=1,angle=0,lwd=3)
arrows(1996,1.85,1996,2.15,length=.1,code=1,angle=0,lwd=3)
lines(time[fire.effects==1],rep(1,sum(fire.effects)),lwd=3)
arrows(1996,0.85,1996,1.15,length=.1,code=1,angle=0,lwd=3)
text(1900,3.3,"Corporate logging and fire",adj=0,cex=1.4)
text(1965,2.3,"Fire suppression",cex=1.4)
text(2020,1.3,"Prescribed fire",adj=1,cex=1.4)
par(new=T)
plot(time,fire.scars,ylim=c(0,3),type="l",xaxt="n",yaxp=c(0,1.5,3),ylab="")
mtext("Fires per decade",cex=1.3,at=.7,side=2,line=2.5)



#########################################################################
#Final figure

pdf("agentsChange.pdf",width=9,height=8)

par(mfcol=c(4,2),mar=c(0,4,0,2),oma=c(4,3,3,2))

#mean annual temperature, sugarlands and clingman's dome
sugar.col = "cornflowerblue"
cling.col = "darkgreen"
plot(clim$year,clim$sugar.MAT,type="l",ylim=c(8,14.5),lwd=3,col=sugar.col,xlab="",xaxt="n",ylab="MAT (°C)",
     cex.axis=1,cex.lab=1.3)
lines(clim$year,clim$cling.MAT,type="l",col=cling.col,lwd=3)
axis(side=1,labels=F)
axis(side=3,cex.axis=1.2)
abline(v=c(1900,1920,1940,1960,1980,2000,2020),lty=2,col="gray",lwd=.5)
#text(1870,par('usr')[4]-.5,"A",cex=3,xpd=NA)
text(1905,14,"A",cex=3)

#total annual rainfall, sugarlands and clingman's dome
plot(clim$year,clim$sugar.TAP/10,type="l",ylim=c(90,290),col=sugar.col,xlab="",xaxt="n",ylab="Rainfall (cm)",
     cex.lab=1.3)
ls = predict(loess(sugar.TAP/10 ~ clim$year,span=.2)) #smooth for display
lines(clim$year,ls,lwd=3,col=sugar.col)
lines(clim$year,clim$cling.TAP/10,type="l",col=cling.col)
ls = predict(loess(cling.TAP/10 ~ clim$year,span=.2)) #smooth for display
lines(clim$year,ls,lwd=3,col=cling.col)
axis(side=1,labels=F)
text(1950,265,"Clingman's Dome (2025 m)",col=cling.col,cex=1.4)
text(1980,96,"Sugarlands (446 m)",col=sugar.col,cex=1.4)
#text(1870,par('usr')[4]-.5,"B",cex=3,xpd=NA)
polygon(x=c(1900,1910,1910,1900,1900),y=c(280,280,250,250,280),density=NA,col="white")
text(1905,270,"B",cex=3)
abline(v=c(1900,1920,1940,1960,1980,2000,2020),lty=2,col="gray",lwd=.5)

#monthly water balance, sugarlands and clingman's dome
plot(prism.df$prism.date,sugar.BAL,type="p",col=sugar.col,pch=".",ylim=c(-100,500),xlab="",xaxt="n",
     ylab="Water balance (mm)",cex.lab=1.3,xlim=as.Date(c("1900-01-01","2020-01-01")),cex=2)
ls = predict(loess(sugar.BAL ~ as.numeric(prism.df$prism.dat),span=.2)) #smooth for display
lines(as.numeric(prism.df$prism.date),ls,lwd=3,col=sugar.col)
abline(h=0,lty=2,col="black")
points(prism.df$prism.date,cling.BAL,type="p",col=cling.col,pch=".",cex=2)
ls = predict(loess(cling.BAL ~ as.numeric(prism.df$prism.date),span=.2)) #smooth for display
lines(as.numeric(prism.df$prism.date),ls,lwd=3,col=cling.col)
abline(h=0,lty=2,col="black")
axis(side=1,labels=F,at=as.Date(c("1900-01-01","1920-01-01","1940-01-01","1960-01-01","1980-01-01","2000-01-01","2020-01-01")))
abline(v=as.Date(c("1900-01-01","1920-01-01","1940-01-01","1960-01-01","1980-01-01","2000-01-01","2020-01-01")),lty=2,col="gray",lwd=.5)
#text(1870,par('usr')[4]-.5,"C",cex=3,xpd=NA)
text(as.Date("1905-01-1"),440,"C",cex=3)

#SPEI, sugarlands (clingmans is similar trend)
plot(prism.df$prism.date,sugar.SPEI,type="l",col="white",xlab="",xaxt="n",
     ylab="SPEI drought index",cex.lab=1.3,xlim=as.Date(c("1900-01-01","2020-01-01")),cex=2)
x = c(prism.df$prism.date[1],prism.df$prism.date,tail(prism.df$prism.date,1))
y1 = c(0,sugar.SPEI,0); y1[y1<0] = 0; y1[1:6] = 0
y2 = c(0,sugar.SPEI,0); y2[y1>0] = 0; y2[1:6] = 0
polygon(x,y1,col="blue",border="black",lwd=.5)
polygon(x,y2,col="red",border="black",lwd=.5)
ls = predict(loess(sugar.SPEI ~ as.numeric(prism.df$prism.date),span=.2)) #smooth for display
lines(as.numeric(prism.df$prism.date)[-c(1:5)],ls,lwd=3)
axis(side=1,labels=c(1900,1920,1940,1960,1980,2000,2020),cex.axis=1.2,at=as.Date(c("1900-01-01","1920-01-01","1940-01-01","1960-01-01","1980-01-01","2000-01-01","2020-01-01")))
abline(v=as.Date(c("1900-01-01","1920-01-01","1940-01-01","1960-01-01","1980-01-01","2000-01-01","2020-01-01")),lty=2,col="gray",lwd=.5)
#text(1870,par('usr')[4]-.5,"D",cex=3,xpd=NA)
text(as.Date("1905-01-01"),-2.4,"D",cex=3)

#logging, fires
plot(time,corp.log,type="n",yaxt="n",ylab="",xlab="",ylim=c(0,2.5),xlim=c(1900,2020),xaxt="n")
abline(v=c(1900,1920,1940,1960,1980,2000,2020),lty=2,col="gray",lwd=.5)
lines(time[corp.log==1],rep(2,sum(corp.log)),lwd=3)
arrows(1930,1.85,1930,2.15,length=.1,code=1,angle=0,lwd=3)
lines(time[fire.sup==1],rep(1.3,sum(fire.sup)),lwd=3)
arrows(1930,1.15,1930,1.45,length=.1,code=1,angle=0,lwd=3)
arrows(1996,1.15,1996,1.45,length=.1,code=1,angle=0,lwd=3)
lines(time[fire.effects==1],rep(.5,sum(fire.effects)),lwd=3)
arrows(1996,0.35,1996,0.65,length=.1,code=1,angle=0,lwd=3)
text(1900,2.3,"Corporate logging and fire",adj=0,cex=1.4)
text(1965,1.5,"Fire suppression",cex=1.4)
text(2023,0.82,"Prescribed fire",adj=1,cex=1.4)
par(new=T)
plot(time,fire.scars,ylim=c(0,2),type="l",xaxt="n",yaxp=c(0,1.5,3),ylab="",xlim=c(1900,2020))
mtext("Fires per decade",cex=.9,at=1,side=2,line=3)
axis(side=3,cex.axis=1.2)
#text(1870,par('usr')[4]-.5,"E",cex=3,xpd=NA)
text(1905,.25,"E",cex=3)

#pollution: NOx and ozone, CO2
plot(nox.yrs,nox,type="l",xaxt="n",cex.lab=1.3,xlim=c(1900,2020),ylab="Annual Emissions (kmt)",lwd=3,yaxt="n")
abline(v=c(1900,1920,1940,1960,1980,2000,2020),lty=2,col="gray",lwd=.5)
axis(yaxp=c(100,400,2),side=2)
text(1940,150,"NOx",cex=1.7)
par(new=T)
plot(co2$date,co2$average,col="brown",type="l",ylab="",xlim=as.Date(c("1900-01-01","2020-01-01")),xaxt="n",yaxt="n")
axis(side=4,col.ticks="brown",col.axis="brown")
mtext(expression("CO"[2]*" (ppm)"),side=4,line=2.6,col="brown",cex=.9)
par(new=T)
plot(oz.yrs,ozone,type="l",col="darkgray",xlim=c(1900,2020),yaxt="n",xaxt="n",cex.lab=1.3,ylab="",lwd=2)
axis(side=1,labels=F)
axis(side=4,col.ticks="darkgray",col.axis="darkgray",tck = 0.02,labels=F)
text(rep(2020,6),c(10,20,30,40,50,60),c(10,20,30,40,50,60),las=3,col="darkgray",srt=90)
text(2008,63,expression("O"[3]),cex=1.7,col="darkgray")
text(1970,10,expression("CO"[2]),cex=1.7,col="brown")
#mtext("Ozone (ppm-hr)",side=2,line=2.15,col="darkgray",cex=.9)
#text(1870,par('usr')[4]-.5,"F",cex=3,xpd=NA)
text(1905,62,"F",cex=3)

#mammals
plot(time,hogs.spline,type="l",lwd=3,xaxt="n",ylab="Density",ylim=c(0,1700),cex.lab=1.3)
lines(time,elk,lwd=3,col="black")
abline(v=c(1900,1920,1940,1960,1980,2000,2020),lty=2,col="gray",lwd=.5)
axis(side=1,labels=F)
text(2010,300,"Elk",cex=1.6,col="black")
text(1952,870,"Hogs",cex=1.6)
par(new=T)
plot(time2,deer2,type="l",col="firebrick4",xlim=c(1900,2020),yaxt="n",xaxt="n",lwd=3,ylab="")
mtext(expression("Density per km"^2),side=4,line=2.6,col="firebrick4",cex=.9)
axis(side=4,col.ticks="firebrick4",col.axis="firebrick4")
text(2000,30,"Deer",cex=1.5,col="firebrick4")
#text(1870,par('usr')[4]-.5,"G",cex=3,xpd=NA)
text(1905,39,"G",cex=3)

#disease and insects
plot(time,chestnut,type="n",yaxt="n",ylab="",xlab="",ylim=c(0,6),xaxt="n",pch=15,cex.lab=1.3)
abline(v=c(1900,1920,1940,1960,1980,2000,2020),lty=2,col="gray",lwd=.5)
lines(time[chestnut==1],rep(1,sum(chestnut)),col="black",lwd=5)
text(chestnut.year-13,1.25,"Chestnut",cex=1.3)
text(chestnut.year-13,.7,"blight",cex=1.3)
lines(time[balsam==1],rep(2,sum(balsam)),lwd=5)
text(balsam.year-2,2,"Balsam wooly adelgid",cex=1.3,adj=1)
lines(time[beechbark==1],rep(3,sum(beechbark)),lwd=5)
text(beechbark.year-2,3,"Beech bark disease",cex=1.3,adj=1)
lines(time[dogwood==1],rep(4,sum(dogwood)),lwd=5)
text(dogwood.year-2,4,"Dogwood anthracnose",cex=1.3,adj=1)
lines(time[hemlock==1],rep(5,sum(hemlock)),lwd=5)
text(hemlock.year-2,5,"Hemlock wooly adelgid",cex=1.3,adj=1)
#text(1950,5.8,"Invasive insects, disease",cex=1.5)
axis(side=1,cex.axis=1.2)
#text(1870,par('usr')[4]-.5,"H",cex=3,xpd=NA)
text(1905,5.2,"H",cex=3)

dev.off()

