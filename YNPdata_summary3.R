#YNP dataset summary

setwd("C:\\Users\\fridley\\OneDrive - Clemson University\\academic\\projects\\YNP_Doug")

#update from Doug, 12-20-22
#dat = read.csv("dat2b.csv")
#update from Doug, 2-6-23
#dat = read.csv("dat_02-06-23.csv")
#update from Doug 3-1-23
#dat = read.csv("dat_03-01-23.csv")
#update from Doug 3-21-23
dat = read.csv("dat_03-21-23.csv")


dat$hits.pin = as.numeric(as.character(dat$hits.pin))
dat$mo.cons = as.numeric(as.character(dat$mo.cons))
dat$mo.stim = as.numeric(as.character(dat$mo.stim))
dat$soilc = as.numeric(as.character(dat$soilc))
dat$clay = as.numeric(as.character(dat$clay))
dat$silt = as.numeric(as.character(dat$silt))
dat$gs.temp = as.numeric(as.character(dat$gs.temp))

str(dat)

#Inspection of each column
table(dat$yr)
table(dat$end_date) #converted to date object
dat$Date = as.Date(paste0(dat$yr,".",dat$end_date),format="%Y.%m.%d")
hist(dat$Date,100)
hist(dat$juldate)
table(dat$sample.) #decimals are permanent exclosures, 88-89
table(dat$site) 
table(dat$trt)
  #Treatments
  #gr: grazed grassland N=3633
  #te:temporary exclosures N=1487
  #pe: permanent exclosure N=744
  #sgr: grazed under shrub N=101; exclude
  #ste: Temporary exclosure under shrub N=7; exclude
table(dat$plotid) #1-15, although just a few for 13-15
table(dat$mo.nap) #column is empty, statistically significant amount of ANP
  #derivative of biomass
table(dat$npdate) #npdate: date that forage was sampled for N and P analysis; converted to date object
dat$NPDate = as.Date(paste0(dat$yr,".",dat$npdate),format="%Y.%m.%d")
hist(dat$NPDate,100)
table(dat$npjuldate) #DoY of npdate
table(dat$nsample.) #The sample# that forage samples were collected, which usually was the same as biomass was sampled, except in rare cases in 2012 when closest whole sample# was provided 
table(dat$mo.lfnppm) #mo.lfnppm: average N of random bulk samples from community, except in 1988 & 89 when average of graminoids sampled was used
dat$mo.lfNperc = dat$mo.lfnppm/1000000 # leaf N g/g
hist(dat$mo.lfNperc)
table(dat$mo.lfpppm) #phosphorus
dat$mo.lfPperc = dat$mo.lfpppm/1000000 #leaf P g/g
hist(dat$mo.lfPperc) #outliers? exclude
  plot(dat$mo.lfNperc,dat$mo.lfPperc)
  summary(dat$mo.lfPperc)
  length(dat$mo.lfPperc[!is.na(dat$mo.lfPperc)]) #176 non NA values
  dat$mo.lfPperc[dat$mo.lfPperc>.015] = NA
table(dat$pinnumb) #? two values (25, 50)
hist(dat$mo.gram)
hist(dat$mo.forb)
hist(dat$mo.shrub)
hist(dat$mo.live)
hist(dat$mo.dead)
hist(dat$tothits)
hist(dat$hits.pin)
hist(dat$mo.srtot) #total species richness of sample unit?
hist(dat$mo.srliv) #live species richness?
hist(dat$mo.cons) #mo.cons: statistically significant amount of consumption
  #ignore for now
table(dat$mo.stim) #mo.stim:  statistically significant amount of stimulation
  #note, empty column; ignore for now
hist(dat$grambiom,50)
  #strange distribution with numbers well beyond 2000 g/m2... ??
  plot(dat$mo.gram,dat$grambiom)
hist(dat$forbbiom)
  plot(dat$mo.forb,dat$forbbiom)
  #ditto
hist(dat$shrubbiom)  
  plot(dat$mo.shrub,dat$shrubbiom)  
  #ditto
hist(dat$live.biom)  
dat$dead.biom = dat$dadbiom #relable, typo
hist(dat$dead.biom)
plot(dat$mo.live,dat$live.biom)
  #ditto
plot(dat$mo.dead,dat$dead.biom)
hist(dat$totshtbiom)
  plot(dat$tothits,dat$totshtbiom)

  
  
hist(dat$an.gNAP) ??
hist(dat$soilc) #soil percent C pooled per site
hist(dat$soiln) #soil percent N pooled per site
hist(dat$clay)
hist(dat$silt)
hist(dat$days) #between 10 and 60, temp exclosure duration
hist(dat$mo.ppt) #pt during sampling interval (mm)
hist(dat$mo.temp) #presumably mean temp during sampling interval in C
hist(dat$mo.pptday) #mo.ppt divided by days in interval
hist(dat$gs.temp) #April-Aug ppt (mm) BLANK COLUMN  
hist(dat$wateryr.ppt) #wateryr: Nov - Aug ppt (mm); site-year specific but overlapping weather stations
hist(dat$gsyr.ppt) #annual growing season precip
hist(dat$aet) #BLANK COL
hist(dat$elevation) #in m

#these are (mostly?) blank: annual summaries?
dat$an.gNAP
dat$an.gpkscrp
dat$an.cons
dat$an.pkforb
dat$an.pkshrub
dat$an.stim
dat$an.pksr

#Notes:
#  Climate values- Missing temperature and ppt values from the climate records were supplied by the measure from the nearest weather station, 
  #weighted by the average difference between stations
#pe lfN values are from permanently fenced plots that were unclipped through the season and clipped at the end of the season
#Used the dissertation calibration for CB for other years.
##Used Lvbench calibration for Becca


###---------------------------------
###Analysis based on Doug's questions:

#The general question is: What factors control aboveground production?
  
  #Specific questions that can be answered from the data set:
  #1. How does aboveground production during the sampling periods (measured every 3-5 weeks) 
    #vary during the snow free season, from snowmelt to the last Sept-Oct sample date, 
    #and how do those dynamics vary with elevation, temp, precipitation, soil texture 
    #(i.e., clay content) and soil C?

#approach that considers temporary exclosures and ambient (gr) treatments to calculate ANPP

#5 colors from green (spring) to brown (fall)
colfunc <- colorRampPalette(c("orange", "darkgreen"))
col5 = colfunc(5)

#example: site cb, year 2001, only plots with te and gr
#cb01 = dat[dat$site=="cb"&dat$yr=="2001"&dat$plotid<5,]
cb01 = dat[dat$site=="becca"&dat$plotid<5,]
cb01 = cb01[order(cb01$juldate,cb01$plotid,cb01$trt),] #sort by plot
str(cb01)#create matched gr-te dataset
#par(mfrow=c(2,4),mar=c(5,5,1,1))
par(mfrow=c(1,3),mar=c(5,5,1,1))
for(i in 1:length(unique(cb01$yr))) {
  y = sort(unique(cb01$yr))[i]
  gr1 = cb01[cb01$trt=="gr"&cb01$yr==y,c("juldate","plotid","totshtbiom","sample.")]
te1 = cb01[cb01$trt=="te"&cb01$yr==y,c("juldate","plotid","totshtbiom","sample.")]
grte = merge(gr1,te1,by=c("juldate","plotid")); names(grte)[c(3,4,5)] = c("gr.biom","sample","te.biom")
plot(grte$gr.biom,grte$te.biom,xlim=c(0,300),ylim=c(0,300),col=col5[as.numeric(grte$plotid)],lwd=2,cex=2,
     xlab="Biomass, grazed",ylab="Biomass, temporary exclosure")
abline(0,1)
text(grte$gr.biom,grte$te.biom,grte$sample,col=col5[as.numeric(grte$plotid)])
title(main=y)
#readline()
}

table(cb01$trt,cb01$plotid,cb01$juldate)
#plot ambient biomass
plot(cb01$juldate[cb01$trt=="gr"],cb01$totshtbiom[cb01$trt=="gr"],col=as.numeric(cb01$plotid[cb01$trt=="gr"]),
     xlab="Day of the Year",ylab="Standing shoot biomass",ylim=c(0,160),xlim=c(120,280))
points(cb01$juldate[cb01$trt=="te"]+5,cb01$totshtbiom[cb01$trt=="te"],col=as.numeric(cb01$plotid[cb01$trt=="te"]),pch=19)
segments(cb01$juldate[cb01$trt=="gr"],cb01$totshtbiom[cb01$trt=="gr"],cb01$juldate[cb01$trt=="te"]+5,cb01$totshtbiom[cb01$trt=="te"],col=as.numeric(cb01$plotid[cb01$trt=="gr"]))     


#start with an example
  table(gr$site)
  #becca has the most samples = 204, but only 2012-14
  table(gr$site,gr$yr)
  #cb has all years
  gr = gr[gr$site=="cb",]
  
  #plot standing biomass per julian date, 1 year only
  plot(gr$juldate[gr$yr=="2001"],gr$totshtbiom[gr$yr=="2001"],col=as.numeric(gr$plotid[gr$yr=="2001"]),
       xlab="Day of the Year",ylab="Standing shoot biomass")
  gr01 = gr[gr$yr=="2001",]
  gr01 = gr01[order(gr01$juldate),]
  for(i in 1:length(unique(gr01$plotid))) {
    x = levels(gr$plotid)[i]
    lines(gr01$juldat[gr01$plotid==x],gr01$totshtbiom[gr01$plotid==x],col=i)
  }
  
  #Lots of spatial (site) variation. Let's put that aside for now and focus on temporal variation.
  #Note temporal variation is nonlinear. GAM model with site as random effect?
  library(mgcv)
  
  gam1 <- gamm(totshtbiom~s(juldate),family=gaussian,data=gr01,random=list(plotid=~1))
  gam1 <- gamm(totshtbiom~s(juldate)+s(plotid,bs="re"),family=gaussian,data=gr[gr$yr=="2001",])
  gam1 <- gamm(totshtbiom~s(juldate),family=gaussian,data=gr)
  
  plot(gam1$gam)
  summary(gam1$gam)
  anova(gam1$gam)
  ranef(gam1$gam)
  gam.check(gam1$gam)
  plot(gr$juldate[gr$yr=="2001"],gr$totshtbiom[gr$yr=="2001"],col=as.numeric(gr$plotid[gr$yr=="2001"]))
  points(gr$juldate[gr$yr=="2001"],fitted(gam1$gam),col="blue",pch=19)
  
  #compare to gam of fixed effect only
  gam2 <- gamm(totshtbiom~s(juldate),family=gaussian,data=gr[gr$yr=="2001",])
  points(gr$juldate[gr$yr=="2001"],fitted(gam2$gam),col="green",pch=19)
    #exactly the same, no random effect?
  
  #try gamm4 package instead
  library(gamm4)
  gr01 = gr[gr$yr=="2001",]
  gam3 <- gamm4(totshtbiom~s(juldate),family=gaussian,data=gr[gr$yr=="2001",],random=~(1|plotid))
  summary(gam3$gam)
  ranef(gam3$mer)
    #fits random effects but none vary from zero, perhaps that is just nature of data as starting from no shoot mass?
  
  #try a different tack: precip and temp
  plot(gr01$mo.ppt,gr01$totshtbiom)
  
  
  #2. How does grazing intensity affect aboveground production during sampling periods 
    #and how do those animal effects interact with the climate and site factors?
  
  #3. How is annual aboveground production (ANPP) determined by climate (different growing season
    #ppt and water year ppt amounts measured during different years), elevation, and site soil properties (texture, C)?
  
  #4. How does grazing determine ANPP and interact with the same climate (growing season, water yr) 
    #and site (elevation, soil) factors. Note that you’ll need to use the “pe” (permanent exclosure) 
    #treatment to calculate the grazing effect-we’ll need to probably talk about this.



#try a beta function fit to seasonal data  
cb01 = dat[dat$site=="cb"&dat$plotid<30&dat$trt=="te",]
cb01 = cb01[order(cb01$juldate,cb01$plotid,cb01$trt),] #sort by plot  
par(mfrow=c(1,1))
plot(cb01$juldate[cb01$yr=="2001"],cb01$totshtbiom[cb01$yr=="2001"],col=as.numeric(as.factor(cb01$plotid)),pch=19)  

#fit a beta function model with a random intercept for plot in JAGS
library(R2jags)

yr = "2001"
plot = cb01$plotid[cb01$yr==yr]
x = cb01$juldate[cb01$yr==yr] / 365  #DoY in fraction format
y = cb01$totshtbiom[cb01$yr==yr]
yscale = max(y)/4 #constant to convert back to biomass
y = 4*y/max(y)  #seems to work best if Y scaled from 0-4
plot(x,y,col=plot,pch=19)


mod = "model { 
     for(i in 1:N) { 
        y[i] ~ dnorm(mu[i],tau) 
        #mu[i] <- (x[i]^(alpha-1))*((1-x[i])^(beta-1)) / b  #beta function
        mu[i] <- b.plot[plot[i]] + (x[i]^(alpha-1))*((1-x[i])^(beta-1)) / b  #beta function with plot RE
      } 
    
    #plot random effects
    for(i in 1:G) {
        b.plot[i] ~ dnorm(0,tau.group) #mean of zero
    }
    
    tau.group <- sigma.group^-2 #coverts sd to precision
    sigma.group ~ dunif(0, 100)  #uniform prior for standard deviation
    sigma ~ dunif(0,100)
    tau <- sigma^-2 #converts SD to precision
    alpha ~ dunif(1,100)
    beta ~ dunif(1,100)
    b <- exp(loggam(alpha))*exp(loggam(beta))/exp(loggam(alpha+beta)) #beta constant
    date.peak <- (alpha-1) / (alpha+beta-2) #mode of beta function
}" 
write(mod, "model.txt")
inits = function() list(alpha=runif(1)*100,beta=runif(1)*100,sigma=runif(1)*100)
param = c("alpha","beta","sigma","date.peak","mu","b.plot")
data = list(x=x,y=y,N=length(y),G=max(plot),plot=plot)
mod.lm = jags(model="model.txt",data=data,inits=inits,param=param,n.iter=30000,n.chain=3,n.thin=3,n.burnin=500)
mod.lm
attach.jags(mod.lm)
hist(date.peak)
hist(alpha)  
hist(mu)
hist(beta)

#show fit
plot(x*365,y*yscale,col=plot,pch=19,ylab="Production (g/m2)",xlab="Day of Year")
points(x*365,colMeans(mu)*yscale,pch=21,cex=1.5,type="p",col="gray")
lines(seq(0,1,length=100)*365, dbeta(seq(0,1,length=100), colMeans(alpha), colMeans(beta))*yscale, type='l',col="gray")
title(main="Seasonal productivity (te only)")




