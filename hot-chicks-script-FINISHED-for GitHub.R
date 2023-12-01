
# These scripts use the CRAGS monitoring shy albatross on Albatross Island
# Chapter 4 of Claire Mason's PhD 
# Submitted to MEPS Heatwave Impacts Special Issue 2022

# AUTHOR: Claire Mason <claire.j.mason@csiro.au> 
# Last updated 1 December 2023, by CM 



# content of this script in order
#----------------------------------------
#1# read in and clean chick mortality data from the CRAGS camera 
#2# read in and clean BOM Cape Grim weather data 
#3# create new dataframe combining mortality and weather data 
#4# plot figures 1-5 and summaries for Table 1
#5# read in and clean Albatross Island weather data "ai"
#6# plot and analyse data for supp material 
#----------------------------------------


# load libraries 

library(tidyverse);library(survival);library(muhaz);library(zoo);library(lubridate);library(survminer);library(viridis)


# 1 # ##### SURVIVAL DATA #####
#----------------------------------------

# read in raw survival data 
chks <- read.csv("data/survival-data-artifical-seperate.csv")

# remove artifical nests 
chksNat <- chks %>% filter(type=="NAT")

# format columns
chksNat$year <- factor(chksNat$year)
chksNat$time <- as.numeric(chksNat$time.1.Oct)

# add season column 
chksNat <- chksNat %>% mutate(season = 
                                ifelse(year =="2014" , "2014/15", 
                                       ifelse(year =="2015" , "2015/16", 
                                              ifelse(year =="2016" , "2016/17",       
                                                     ifelse(year =="2017" , "2017/18",
                                                            ifelse(year =="2018" , "2018/19",
                                                                   ifelse(year =="2019" , "2019/20",
                                                                          ifelse(year =="2020" , "2020/21", NA))))))) ) 



#write.csv(chksNat, "chksNat.csv")


# Table 1 

t1 <- chksNat %>% group_by(season) %>% summarise(n()) 
t2 <- chksNat %>% group_by(season) %>% filter(status==1) %>% summarise(n()) 
t3 <- full_join(t1, t2, by="season")
t3 %>% mutate(per = `n().y`/`n().x` * 100)



# plot as survival object to check 
fit <- survfit(Surv(time, status) ~ year, data = chksNat) 
plot(fit)

?Surv()

# cubic smooth cumulative hazard to produce hazard rate h(t) 
# library(muhaz)
kpfit <- kphaz.fit(chksNat$time,chksNat$status, strata= chksNat$year, q=1, method="nelson")

# unlist for plotting 
kp <- as.data.frame(kpfit)

# add season column 
kpdf <- kp %>% mutate(season = 
                        ifelse(strata ==1 , "2014/15", 
                               ifelse(strata ==2 , "2015/16",        
                                      ifelse(strata ==3 , "2016/17",       
                                             ifelse(strata ==4 , "2017/18",
                                                    ifelse(strata ==5 , "2018/19",
                                                           ifelse(strata ==6 , "2019/20",
                                                                  ifelse(strata ==7 , "2020/21", NA))))))) ) 









# 2 # ##### BOM CAPE GRIM WEATHER STATION DATA #####
#----------------------------------------

bom <- read.csv("data/CAPE GRIM NEW 20220301.csv")

#format a date-time column 

bom <- bom %>% mutate(date = make_date(Year.Month.Day.Hour.Minutes.in.YYYY, MM, DD))

bom <- bom %>% mutate(time = paste(HH24, MI.format.in.Local.time, sep=":"))

bom$dt <- as.POSIXct(paste(bom$date, bom$time))

# for another graph. The rest just use 2014 onwards
bom.historical <- bom


# clip to time span of interest 

bom <- bom %>% filter(dt >= as.POSIXct("2014-11-01 00:00:00"))

bom <- bom %>% filter(dt <= ("2021-06-01 00:00:00"))

bom <- bom %>% filter(month(dt) %in% c(11, 12, 1, 2, 3, 4, 5))


# make season a column 

bom <- bom %>% 
  
  mutate(season = ifelse(
    
    dt < as.POSIXct("2015-06-01 24:00:00"), "2014/15", 
    
    ifelse(dt > as.POSIXct("2015-06-01 24:00:00") & dt < as.POSIXct("2016-06-01 24:00:00"), "2015/16", 
           
           ifelse(dt > as.POSIXct("2016-06-01 24:00:00") & dt < as.POSIXct("2017-06-01 24:00:00"), "2016/17", 
                  
                  ifelse(dt > as.POSIXct("2017-06-01 24:00:00") & dt < as.POSIXct("2018-06-01 24:00:00"), "2017/18", 
                         
                         ifelse(dt > as.POSIXct("2018-06-01 24:00:00") & dt < as.POSIXct("2019-06-01 24:00:00"), "2018/19",
                                
                                ifelse(dt > as.POSIXct("2019-06-01 24:00:00") & dt < as.POSIXct("2020-06-01 24:00:00"), "2019/20", 
                                       
                                       ifelse(dt > as.POSIXct("2020-06-01 24:00:00") & dt < as.POSIXct("2021-06-01 24:00:00"), "2020/21", NA))))))))

glimpse(bom)                                   

# made dates universal regardless of year

bom <- bom %>% 
  mutate(ymd = ifelse(month(dt) %in% c(11, 12), 
                      format(date, '2000-%m-%d'),
                      format(date, '2001-%m-%d')),
         ymd = as.Date(ymd))


# convert this to days since Nov 1 to append to mortality data 

bom <- bom %>% mutate(days1Nov= difftime(ymd, as.Date("2000-11-01"), units = "days"))




# END read in data 

#2# create new data - combine mortality and weather data 


# use BOM-derived WBGT not the solar and equation 
# make both time measurements as integers /whole numbers 
bom$days1Nov <- as.integer(bom$days1Nov) 
kpdf$days1Nov <- as.integer(kpdf$time) # this will round down all the 0.5 and assign them to the day i.e. 16.5 = 16

#create new data with where it steps up hazard wise
haz.wbgt <- full_join(bom, kpdf, by= c("season", "days1Nov")) # match all, haz just shows with time matches, rest NA
plot(haz.wbgt$haz) # yep beautiful


# DEFINITELY don't fill. Because haz increases when a new death occurs, it isn't the same for the days after cause there are no new deaths 


# hazard mortality days 
quantile(haz.wbgt$haz, .95, na.rm=T) # 95% is 0.0279734 

# hot days above "threshold" 


# don't use half hourly, use max daily 
# half hourly gives 18.9 as .99 quantile quantile(haz.wbgt$Wet.bulb.temperature.in.degrees.C, 0.99, na.rm=T)

# 99th of daily max
bom %>% group_by(date) %>% summarise(max(Wet.bulb.temperature.in.degrees.C)) %>% 
  summarise(quantile(`max(Wet.bulb.temperature.in.degrees.C)`, .99,  na.rm=T))


bomHOT <- bom %>% filter(Wet.bulb.temperature.in.degrees.C >= 20.2)


#3# make figures 

#----------------------------------------
#Fig 1# KP survival models
#Fig 2# Hazard rates
#Fig 3# Close look at 2018 event temporal overlap - removed for now 
#Fig 4# Boxplot of hazard rates across temperatures
#Fig 5# Historical Cape Grim and WBGT > 22 deg
#----------------------------------------







# 1 # ##### FIGURE 1 #####
#----------------------------------------

ggsurvplot(fit, data = chksNat, conf.int=FALSE, 
           palette = turbo(7), ##viridis(7)
           xlim = c(0, 210), 
           
           legend= c(0.2,0.2) , #"right", 
           legend.title= "Season", 
           legend.labs= c("2014/15 (n=90)", 
                          "2015/16 (n=86)", 
                          "2016/17 (n=86)", 
                          "2017/18 (n=135)", 
                          "2018/19 (n=194)", 
                          "2019/20 (n=221)",
                          "2020/21 (n=224)"),
           xlab= "Days since 1 November (t)", 
           ylab= "Survival probability S(t)", 
           font.legend = 20, font.x = 20, font.y= 20, font.tickslab= c("20", "plain", "black"), 
           ggtheme= theme_classic())



# 2 # ##### FIGURE 2 #####
#----------------------------------------


ggplot() + 
  geom_step(data=kpdf, mapping=aes(x=time, y=haz,  col=season), size=1) + 
  scale_y_continuous(breaks = seq(0, 0.12, by = 0.06)) +
  scale_color_manual(values = turbo(7)) +
  geom_hline(yintercept=0.02877847, # new value with haz filled across each step, for 0.99
             linetype=2, color="gray", size=1) +
  
  
  facet_wrap(~season, ncol=1)  + theme_bw(base_size=15) + 
  
  ylab("Hazard estimate h(t)") + xlab("Days since 1 November (t)")+
  
  theme(legend.position = "none") 





# 5 # ##### FIGURE 3 ##### 
#----------------------------------------

# add extra space around plot 
par(mar=c(5, 4, 4, 6) + 0.1)


plot((bom[bom$season == "2017/18" , ])$dt, 
     y=(bom[bom$season == "2017/18" , ])$Wet.bulb.temperature.in.degrees.C, 
     xlab=NA, ylab="WBGT", main="2017/18", ylim=c(5,25)) + 
  abline(h = 20.2,  lty = 2, lwd=2) #+
# points((bomHOT[bomHOT$season == "2017/18" , ])$dt, 
#    y=(bomHOT[bomHOT$season == "2017/18" , ])$Wet.bulb.temperature.in.degrees.C,
#   col="red", pch=19) # red is the 99th percentile of max DAILY wbgt for all data .. not using half hourly data which gives 18.9 



# now to overlap with survival/mortality data
## add in survival curve 
#par(new = TRUE) # run this after temp data to overlay survival data on top 

chksNat2017 <- chksNat %>% filter(year=="2017")
s2017 <- survfit(Surv(time.1.Oct, status) ~ year, data = chksNat2017)
s2017df <- data.frame(time.1.Oct= s2017$time, Surv= s2017$surv) 
s2017df2 <- left_join(s2017df, chksNat2017, by= "time.1.Oct")

s2017df2$date2 <- as.Date(s2017df2$date.chick.death, format="%d/%m/%Y")
# to match x axis limits to mortality data 
s2017df2 %>% add_row(date2 = "2017-11-01")

# set xlims as dates so can match scale of bom data 
xlimL <- as.Date("2017-11-01", format="%Y-%m-%d")
xlimU <- as.Date("2018-06-01", format="%Y-%m-%d")


par(new=TRUE)

plot(x=s2017df2$date2, y=(s2017df2$Surv), axes=FALSE, xlab=NA, ylab=NA, col="blue", type="l", xlim= c(xlimL, xlimU) )
axis(side=4, col="blue", las=1, col.lab="blue") 
mtext("Survival", side=4, line=3)



# 4 # ##### FIGURE 4 ##### 
#----------------------------------------

## R2.4 - MEPS review 1 
#"Why not reverting the axes to compare the hazard rates across temperature conditions, instead of temperatures across hazard rate observations? 
#Potentially, the 90% percentiles of the observed temperatures could be considered instead of the 95% percentiles, to include more data. 
#If the analysis suggested here turns out to be significant, it might be the most relevant way to show that extreme temperatures increase the hazard rate


#20.2 is the 99th percentile for daily maxes - but some of the outliers are on "cooler days" so try 80%
daily <- haz.wbgt %>% group_by(date, haz) %>% summarise(dailymax= max(Wet.bulb.temperature.in.degrees.C)) 

daily$haz <- as.numeric(daily$haz)
daily$dailymax <- as.numeric(daily$dailymax)
glimpse(daily)

quantile(daily$dailymax, .95,  na.rm=T)
#.9 = 18.2
#.95 = 19 ; 74 in high temp group
#.975 = 19.6 ; 38 in hightemp group
#.98
daily <- daily %>% mutate(hightemps= if_else(dailymax >=  19  , 1, 0))

#par(mfrow=c(1,1));par(mar = c(5,5,5,5))
boxplot(haz ~ hightemps, data=daily, names= c("low heat stress days", "high heat stress days"), 
        xlab=NULL, ylab="hazard rate h(t)", cex.lab=1.2, cex.axis=1.2)



# test if different means 
#t.test(haz ~ hightemps, var.equal = TRUE, paired=FALSE, na.action="na.omit", data=daily)
# haz not normally distributed so do a mann-whitney u test 

hightemps <- daily %>% filter(hightemps==1)
lowtemps <- daily %>% filter(hightemps==0)

wilcox_result <- wilcox.test(na.omit(hightemps$haz), na.omit(lowtemps$haz), alternative = "g")

# Access the p-value from the result
wilcox_result$p.value


#
#HS1 <- as.data.frame(haz.wbgt.daily %>% filter(hightemps==1))
#HS0 <- as.data.frame(haz.wbgt.daily %>% filter(hightemps==0))
#var.test(HS1$haz, HS0$haz)


# FIGURE 5 #
######## how many times does Cape Grim get above 99 percentile using my methods 



# filter to DTs where over 99th percentile
hot.bom.historical <- bom.historical %>% filter(Wet.bulb.temperature.in.degrees.C > 20.2)

## convert to seasons 
hot.bom.historical <- hot.bom.historical %>% filter(month(dt) %in% c(11, 12, 1, 2, 3, 4, 5)) 

hot.bom.historical <- hot.bom.historical %>% mutate(year= year(dt))

#and filter to breeding season 
hot.bom.historical <- hot.bom.historical  %>% 
  mutate(season = ifelse(month(dt) %in% c(1, 2, 3, 4, 5), 
                         year-1 ,
                         year))

hot.bom.historical.summary <- hot.bom.historical %>% group_by(season) %>% count() # how many occurrences in the half-hourly data?
hot.bom.historical.summary <- hot.bom.historical.summary %>% mutate(hours= n/2) # how many hours in total



x <- barplot(hot.bom.historical.summary$hours, 
             
             #names.arg= c("1997/98", "2000/01", "2002/02", "2006/07", "2007/08","2008/09" ,"2009/10" ,"2011/12", 
             #"2012/13", "2014/15", "2016/17", "2017/18", "2018/19" ,"2019/20", "2021/22"), 
             
             
             
             ylab = "Hours recorded in top 1% of WBGT (>20.2 °C)", 
             
             ylim = c(0, 50)) # , las=2) # rotates x axis labels

labs <-  c("1997/98", "2000/01", "2002/02", "2006/07", "2007/08", "2008/09" ,"2009/10" ,"2011/12", "2012/13", "2014/15", "2016/17", "2017/18", "2018/19" , "2019/20", "2021/22")

text(cex=1, x=x-.25, y=-3.5, labs, xpd=TRUE, srt=45)
box()





# create supplementary figure S2 for revision3
# four main variables: CG x AI correlation 


# load libraries 
library(lubridate);library(tidyverse)


# make a column for each date, for creating a mean value for each weather variable 
#ai$ceiling <- ceiling_date(ai$dt, "day")
#met$ceiling <- ceiling_date(met$dt, "day") 


ai <- read.csv("data/AI_weather_raw_Jan22_colnames.csv")

ai$dt<- as.POSIXct(strptime(ai$Timestamp, "%d/%m/%Y %H:%M", tz = "GMT"))  


# add column to calculate dew point temperature 
ai$AIairtemp <- as.numeric(ai$airtemp)
ai$AIRH <- as.numeric(ai$RH) # this raw data has humidity values above 1, remove these 
is.na(ai$AIRH) <- ai$AIRH > 1  # replace all RH values above 1 with NA 
ai$AIsolar <- as.numeric(ai$solar)
ai$AIwindsp <- as.numeric(ai$windsp)*3.6

# use met - it includes solar as well as bom 

met$CGwindsp <- as.numeric(met$Wind.speed.in.km.h)
met$CGRH <- as.numeric((met$Relative.humidity.in.percentage..)/100)
met$CGairtemp <- as.numeric(met$Air.Temperature.in.degrees.C) 
met$CGsolar <- as.numeric(((met$value)*1000000)/1800) # covert cape grim solar to same units as alby isalnd 



ai  <- ai %>% mutate(
  date = as.Date(dt),
  hour = hour(dt)
) 





met  <- met %>% mutate(
  date = as.Date(dt),
  hour = hour(dt)
) 


aihourlymean <- ai %>% group_by(date, hour) %>% 
  summarise( uAIairtemp = mean(AIairtemp), uAIRH = mean(AIRH), uAIsolar = mean(AIsolar), 
             uAIwindsp = mean(AIwindsp))





methourlymean <- met %>% group_by(date, hour) %>% 
  summarise(   uCGairtemp = mean(CGairtemp), uCGRH = mean(CGRH), uCGsolar = mean(CGsolar), 
               uCGwindsp = mean(CGwindsp)) 




meansh <- full_join(methourlymean , aihourlymean , by=c("date", "hour"))



################## daily means re run 

aidailymean <- aihourlymean %>% group_by(date) %>% 
  summarise( dailyAIairtemp = mean(uAIairtemp), dailyAIRH = mean(uAIRH), dailyAIsolar = mean(uAIsolar), 
             dailyAIwindsp = mean(uAIwindsp))


metdailymean <- methourlymean  %>% group_by(date) %>% 
  summarise(   dailyCGairtemp = mean(uCGairtemp), dailyCGRH = mean(uCGRH), dailyCGsolar = mean(uCGsolar), 
               dailyCGwindsp = mean(uCGwindsp)) 


meansd <- full_join(metdailymean , aidailymean , by="date")



##### PLOTTING DAILY MEANS 


# AIR TEMPERATURE 

plot(meansd$dailyAIairtemp  , meansd$dailyCGairtemp  , xlim= c(0,25), ylim= c(0,25),
     xlab = "Albatross Island",
     ylab ="Cape Grim", 
     main = "Daily mean air temperature °C")
abline(lm(  meansd$dailyCGairtemp~ meansd$dailyAIairtemp ) , col = "red", lwd = 3)


# Pearson correlation
round(cor(meansd$dailyCGairtemp , meansd$dailyAIairtemp, use = "complete.obs"), 2)


#linear model 
air <- lm(meansd$dailyCGairtemp~ meansd$dailyAIairtemp)
summary(air) # for p value
nrow(model.frame(air)) # for sample size

text(paste("Correlation=0.90, p<0.0001, n=282"), x = 5, y = 25)


# RELATIVE HUMIDITY 



plot(meansd$dailyAIRH  , meansd$dailyCGRH  , xlim= c(0.5,1), ylim= c(0.5,1),
     xlab = "Albatross Island",
     ylab ="Cape Grim", 
     main = "Daily mean relative humidity %")
abline(lm( meansd$dailyCGRH  ~ meansd$dailyAIRH  ) , col = "red", lwd = 3)


# Pearson correlation
round(cor(meansd$dailyCGRH , meansd$dailyAIRH , use = "complete.obs"), 2)


#linear model 
rh <- lm(meansd$dailyCGRH ~ meansd$dailyAIRH)
summary(rh) # for p value
nrow(model.frame(rh)) # for sample size

text(paste("Correlation=0.66, p<0.0001, n=219"), x = 0.6, y = 1)





# WIND SPEED 



plot(meansd$dailyAIwindsp  , meansd$dailyCGwindsp  , xlim= c(0,60), ylim= c(0,60),
     xlab = "Albatross Island",
     ylab ="Cape Grim", 
     main = "Daily mean wind speed km hr-1")
abline(lm( meansd$dailyCGwindsp  ~ meansd$dailyAIwindsp  ) , col = "red", lwd = 3)


# Pearson correlation
round(cor(meansd$dailyCGwindsp , meansd$dailyAIwindsp , use = "complete.obs"), 2)


#linear model 
ws <- lm(meansd$dailyCGwindsp ~ meansd$dailyAIwindsp)
summary(ws) # for p value
nrow(model.frame(ws)) # for sample size

text(paste("Correlation=0.48, p<0.0001, n=255"), x = 11, y = 60)





# SOLAR 



plot(meansd$dailyAIsolar  , meansd$dailyCGsolar  , xlim= c(0,600), ylim= c(0,600),
     xlab = "Albatross Island",
     ylab ="Cape Grim", 
     main = "Daily mean solar W/m^2")
abline(lm( meansd$dailyCGsolar  ~ meansd$dailyAIsolar  ) , col = "red", lwd = 3)


# Pearson correlation
round(cor(meansd$dailyCGsolar , meansd$dailyAIsolar , use = "complete.obs"), 2)


#linear model 
sol <- lm(meansd$dailyCGsolar ~ meansd$dailyAIsolar)
summary(sol) # for p value
nrow(model.frame(sol)) # for sample size

text(paste("Correlation=0.90, p<0.0001, n=215"), x = 120, y = 600)



# plot all together as 2x2 
par(mfrow=c(2,2))

# then run all again 
