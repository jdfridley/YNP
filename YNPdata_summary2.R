#YNP dataset summary

setwd("C:\\Users\\fridley\\OneDrive - Clemson University\\academic\\projects\\YNP_Doug")

#update from Doug, 12-20-22
#dat = read.csv("dat2b.csv")
#update from Doug, 2-6-23
#dat = read.csv("dat_02-06-23.csv")
#update from Doug 3-1-23
dat = read.csv("dat_03-01-23.csv")


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
ditto
plot(dat$mo.dead,dat$dead.biom)
hist(dat$totshtbiom)
  plot(dat$tothits,dat$totshtbiom)

  
  
hist(dat$an.nNAP)
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
