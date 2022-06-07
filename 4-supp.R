


# script 4 
# supplementary material 
#----------------------------------------
#1# weather station comparison  
#2# selection of lag 
#3# rainfall exploration 
#----------------------------------------


# 1 # ##### Weather station comparison: Cape Grim vs Albatross Island #####
#----------------------------------------

## join to met BOM data to compare for supp material 
##  round up to the hours 

ai$ceiling <- ceiling_date(ai$dt, "hour")

aiMax <- ai %>% group_by(ceiling) %>% summarise(maxAI=max(wbgtAI))

met$ceiling <- ceiling_date(met$dt, "hour")
metMax <- met %>% group_by(ceiling) %>% summarise(maxMet=max(wbgt))


metAI <- inner_join(metMax, aiMax, by="ceiling")

metAI <- metAI %>% mutate(diff= maxAI - maxMet)
summary(metAI$diff)

metAIsumm <- metAI %>% group_by(date(ceiling)) %>% summarise(weekavg= mean(diff))

ggplot(metAIsumm) + 
  geom_point(aes(x=`date(ceiling)`, y=weekavg), pch=1) + 
  geom_hline(yintercept = 2.2088,  linetype="dashed") +
  geom_hline(yintercept=0) +
  
  theme_classic(base_size=15) +
  xlab(NULL) + ylab("difference in WBGT °C
(Albatross Island - Cape Grim)") +
  theme(axis.text.x=element_text(colour="black"), axis.text.y=element_text(colour="black")) + 
  scale_x_date(date_labels = "%b %Y")



coef(lm(weekavg ~ `date(ceiling)`, data = metAIsumm))

## summary stats 

mean(as.numeric(metAI$diff))

par(mar = c(2,2,2,2))
par(mfrow=c(1,1))

plot(month(ai$dt), wbgt.outdoors$data, xlab = "month", ylab = "deg C") 
points(month(ai$dt), ai$X.C.Air.Temperature, col="blue")
legend("bottomleft", c("WBGT", "Air Temp"), fill=c("black", "blue"))    ## wow yep jan and dec hottest, no data for feb


max(wbgt.outdoors$data, na.rm = T)
max(ai$X.C.Air.Temperature, na.rm = T)


# plot Jan temps by year 

ai %>% filter(month(dt)== 1 | month(dt)== 12) %>% group_by(year(dt)) %>% summarise(max(X.C.Air.Temperature))



# 2 # ##### Lag time exploration #####
#----------------------------------------


# quick model 

kpdf$days1Nov <- as.integer(kpdf$time)

kpbom <- left_join(kpdf, bom_summary, by=c("season", "days1Nov"))
glimpse(kpbom)


library(plotly)
fig <- plot_ly(data = kpbom, x = ~wbgtBOM4, y = ~haz)
fig


par(mfrow=c(2,4))
plot(x = kpbom$rain, y= log(kpbom$haz), main="1 day total log")
abline(lm(log(haz) ~ rain, data = kpbom))

plot(x = kpbom$rain2, y= log(kpbom$haz), main="2 day total log")
abline(lm(log(haz) ~ rain2, data = kpbom))

plot(x = kpbom$rain3, y= log(kpbom$haz), main="3 day total log")
abline(lm(log(haz) ~ rain3, data = kpbom))

plot(x = kpbom$rain7, y= log(kpbom$haz), main="7 day total log")
abline(lm(log(haz) ~ rain7, data = kpbom))



plot(x = kpbom$rain3, y= kpbom$haz, main="sum 1 days")
abline(lm(log(haz) ~ rain2, data = kpbom))


plot(x = kpbom$lag3, y= kpbom$haz, main="sum 3 days")
abline(lm(haz ~ lag3, data = kpbom))


plot(x=kpbom$lag7, y= kpbom$haz, main="sum 7 days")
abline(lm(haz ~ lag7, data = kpbom))


plot(x=kpbom$lag14, y= kpbom$haz, main= "sum 14 days")
abline(lm(haz ~ lag14, data = kpbom))



# playing around with lag times 
plot(kpbom$mean7, kpbom$haz)
plot(kpbom$mean7, log(kpbom$haz))
abline(lm(haz ~ mean7, data = kpbom))

m0 <- lm(log(haz) ~ mean7, data=kpbom)
summary(m0)

m1 <- lm(haz ~ log(lag1), data = kpbom)
m2 <- lm(haz ~ log(lag2), data = kpbom)
m3 <- lm(haz ~ log(lag3), data = kpbom)
m4 <- lm(haz ~ log(lag4), data = kpbom)
m5 <- lm(haz ~ log(lag5), data = kpbom)
m7 <- lm(haz ~ log(lag7), data = kpbom)
m10 <- lm(haz ~ log(lag10), data = kpbom)
m14 <- lm(haz ~ log(lag14), data = kpbom)


AIC(m1, m2, m3, m4, m5, m7, m10, m14)



# 3 # ##### Rainfall #####
#----------------------------------------

# and also to explore total rainfall for the season with overall breeding success 

# read in BS 

# filter to breeding season dates and make "SEASON" a category 
# make season a column 
# keep in data since 2010 - could also do long-term one 

bom <- bom %>% mutate(season = ifelse(month(date) %in% c(7, 8, 9, 10, 11, 12), 
                                      year(date), 
                                      year(bom$date %m-% years(1))))


## for each " season" cut to just 1 sept to 1 April 
bom <- bom %>%
  filter(month(dt) %in% c(9, 10, 11, 12, 1, 2, 3, 4))

#check
unique(month(bom$dt))

glimpse(bom)

# is a cumulative sum = Precipitation.since.9am.local.time.in.mm
# extract the 0830am rainfall measurement , or the max for each day 

bom$Precipitation.since.9am.local.time.in.mm[is.na(bom$Precipitation.since.9am.local.time.in.mm)] <- 0

dailyrain <- bom %>% group_by(season, date) %>% #filter(time == "08:30:00") %>% 
  #  na.omit() %>%
  summarise(dailytotal= max(Precipitation.since.9am.local.time.in.mm)) 



seasonrain <- dailyrain %>% group_by(season) %>% summarise(totalrain= sum(dailytotal))

# breeding success 

bs <- read.csv("data/bs.csv" )
View(bs)

rain_bs <- full_join(seasonrain, bs, by="season")
View(rain_bs) # few missing years 

rain_bs<- na.omit(rain_bs) 

rain
plot(x = kpbom$lag3, y= kpbom$haz, main="sum 3 days")
abline(lm(haz ~ lag3, data = kpbom))

plot(x = rain_bs$totalrain, y = rain_bs$BS)
abline(lm(totalrain ~ BS, data = rain_bs))



model<-glm(BS ~ 0 + totalrain, data=rain_bs)
summary(model)
anova(model)

rain_bs$olre <- 1:length(rain_bs$totalrain) 

# make BS between 0-100? Interger? 

rain_bs <- rain_bs %>% mutate(BS= BS*100)

class(rain_bs$BS) <- "integer"

library(lme4)
mod.1 <- glmer(cbind(BS, 100 - BS) ~ 
                 1 + totalrain +  ( 1| olre ), #binomial commonly overdispersed and a random effect accounts for this
               family = binomial(link="logit"),
               data=rain_bs)

summary(mod.1)


model.log = glm(cbind(BS, 100 - BS) ~  1 + totalrain ,
                data = rain_bs,
                family = binomial(link="logit"))
summary(model.log)