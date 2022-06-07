


# script 1 
# read in and clean data 
#----------------------------------------
#1# chick mortality data from the CRAGS camera 
#2# BOM Cape Grim weather data 
#3# Albatross Island weather data "ai"
#----------------------------------------


# load libraries 

library(tidyverse);library(survival);library(muhaz);library(zoo)



# 1 # ##### SURVIVAL DATA #####
#----------------------------------------

# read in raw survival data 
chks <- read.csv("data/survival-data-artifical-seperate.csv")

# remove artifical nests 
chksNat <- chks %>% filter(type=="NAT")

# year as factor 
chksNat$year <- factor(chksNat$year)

# add season column 
chksNat <- chksNat %>% mutate(season = 
                                ifelse(year =="2014" , "2014/15", 
                                ifelse(year =="2015" , "2015/16", 
                                ifelse(year =="2016" , "2016/17",       
                                ifelse(year =="2017" , "2017/18",
                                ifelse(year =="2018" , "2018/19",
                                ifelse(year =="2019" , "2019/20",
                                ifelse(year =="2020" , "2020/21", NA))))))) ) 


# plot as survival object to check 
fit <- survfit(Surv(time, status) ~ year, data = chksNat) 
plot(fit)


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

bom$days1Nov <- as.integer(bom$days1Nov)


# add in solar data (needed for WBGT calculations)

solar <- read.csv("data/HH-CapeGrimGLOBALSolar-20220301_uncertainty_omitted.csv" )




#convert all columns to numeric 

end <- ncol(solar)- 1

solar[ , 6:end] <- apply(solar[ , 6:end], 2,        
                         function(x) as.numeric(as.character(x)))

# make nas zero
solar[is.na(solar)] <- 0


# need to make half hourly to match other bom data 
# reshape/gather data into wide/long format 

solar <- solar %>% gather("key", "value", 6:53)

# extract time from "key" character value
substr(solar$key[1], 48, 51)

# create date and time formatted columns 
# NOTE - IT IS IN SOLAR TIME - see BOM data notes 
# It is in local time - so uses daylight saving when appropriate. 

solar <- solar %>% mutate(hour= substr(key, 48, 49))

solar <- solar %>% mutate(min= substr(key, 50, 51))

solar <- solar %>% mutate(time= paste(hour, min, "00", sep=":"))

solar <- solar %>%
  mutate(date = make_date(Year, Month, Day))

solar$dt <- as.POSIXct(paste(solar$date, solar$time), format="%Y-%m-%d %H:%M:%S")


# clip to time span of interest 

solar <- solar %>% filter(date >= as.POSIXct("2014-11-01 00:00:00"))

solar<- solar %>% filter(date <= ("2021-06-01 00:00:00"))

solar <- solar %>%
  filter(month(date) %in% c(11, 12, 1, 2, 3, 4, 5))


# make season a column 
solar <- solar %>% mutate(season = ifelse(date < as.POSIXct("2015-06-01 24:00:00"), "2014/15", 
                                          ifelse(date > as.POSIXct("2015-06-01 24:00:00") & date < as.POSIXct("2016-06-01 24:00:00"), "2015/16", 
                                                 ifelse(date > as.POSIXct("2016-06-01 24:00:00") & date < as.POSIXct("2017-06-01 24:00:00"), "2016/17", 
                                                        ifelse(date > as.POSIXct("2017-06-01 24:00:00") & date < as.POSIXct("2018-06-01 24:00:00"), "2017/18", 
                                                               ifelse(date > as.POSIXct("2018-06-01 24:00:00") & date < as.POSIXct("2019-06-01 24:00:00"), "2018/19",
                                                                      ifelse(date > as.POSIXct("2019-06-01 24:00:00") & date < as.POSIXct("2020-06-01 24:00:00"), "2019/20", 
                                                                             ifelse(date > as.POSIXct("2020-06-01 24:00:00") & date < as.POSIXct("2021-06-01 24:00:00"), "2020/21", NA))))))))


# made dates universal regardless of year

solar <- solar %>% 
  mutate(ymd = ifelse(month(date) %in% c(11, 12), 
                      format(date, '2000-%m-%d'),
                      format(date, '2001-%m-%d')),
         ymd = as.Date(ymd))

solar <- solar %>% mutate(days1Nov= difftime(ymd, as.Date("2000-11-01"), units = "days"))

solar$days1Nov <- as.integer(solar$days1Nov)



# combine solar with bom to then create a WBGT from the 'HeatStress' package 

# cut only relevant columns 

glimpse(bom)

bom <- bom[, c("dt", "season", "days1Nov", "Wind.speed.in.km.h", "Relative.humidity.in.percentage..", "Air.Temperature.in.degrees.C", "Dew.point.temperature.in.degrees.C")]


solar <- solar[, c("dt", "season", "days1Nov", "value")]

met <- full_join(bom, solar, by= "dt")


View(met) # yep few missing but all good 


# Calculating 'wet bulb globe temperature' from meterological data 

#Calculation of wet bulb globe temperature from air temperature, dew point temperature, radiation and wind.
#This corresponds to the implementation for outdoors or in the sun conditions (Liljegren et al. 2008). Original fortran code by James C. Liljegren, translated by Bruno Lemke into Visual Basic (VBA) and Ana Casanueva into R.


#https://rdrr.io/github/anacv/HeatStress/man/wbgt.Liljegren.html 


#install.packages("cli") #fully uninstall and re-install before devtools to get rid of error message "loadNamespace..."
#install.packages("devtools")
#devtools::install_github("anacv/HeatStress")
#library(HeatStress)
#indexShow()
#?wbgt.Liljegren() 


wbgt.outdoors <- wbgt.Liljegren(tas= met$Air.Temperature.in.degrees.C, 
                                dewp= met$Dew.point.temperature.in.degrees.C, 
                                wind= (met$Wind.speed.in.km.h / 3.6), 
                                radiation= (met$value*3.6)/10^3, 
                                dates=met$dt, 
                                lon=-144.6558, lat=-40.37471, 
                                hour=TRUE)

#unlist(wbgt.outdoors[1])

# add as a column to met dataframe 
met <- met %>% mutate(wbgt = wbgt.outdoors[[1]]) 


# make a summary bom for just daily max 

bom_summary <- met %>% group_by(season.x, days1Nov.x) %>% summarise(max_wbgt = max(wbgt))


# rolling cumualtive lagged sum 
#library(zoo)
bom_summary <- bom_summary %>% group_by(season.x) %>% 
  mutate(wbgtBOM4= rollsumr(max_wbgt, k = 4, fill = NA))




# 3 # ##### ALBATROSS ISLAND WEATHER STATION DATA #####
#----------------------------------------

# recorded in km/hr, in excel /3.6 to convert these back to m/s

ai <- read.csv("data/AI_weather_raw_Jan22_colnames.csv")

ai$dt<- as.POSIXct(strptime(ai$Timestamp, "%d/%m/%Y %H:%M", tz = "GMT"))  


# add column to calculate dew point temperature 
ai$airtemp <- as.numeric(ai$airtemp)
ai$RH <- as.numeric(ai$RH)
ai$solar <- as.numeric(ai$solar)
ai$windsp <- as.numeric(ai$windsp)



ai$Tdw <- ai$airtemp - ((100 - ((ai$RH)*100) )/5)


wbgt.outdoors <- wbgt.Liljegren(tas=ai$airtemp, dewp=ai$Tdw, 
                                wind=ai$windsp, 
                                radiation=ai$solar, 
                                dates=ai$dt, 
                                lon=-144.6558, lat=-40.37471, 
                                hour=TRUE)


ai <- ai %>% mutate(wbgtAI = wbgt.outdoors[[1]]) 

