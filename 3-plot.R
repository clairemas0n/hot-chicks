


# script 3 
# make figures
#----------------------------------------
#Table 1# How many 'high stress days' each year? 
#Fig 1# KP survival models
#Fig 2# Hazard rates
#Fig 3# Event timing and 'high stress days' 
#Fig 4# Linear regression haz v 4-day-WBGT
#Fig 5# Boxplot between high and low stress days
#Fig 6# Historical Cape Grim and WBGT > 22 deg
#----------------------------------------


library(tidyverse);library(survival);library(survminer);library(viridis)



#  # ##### TABLE 1 #####
#----------------------------------------
# in text summary 
summary(kpdf$haz)
quantile(kpdf$haz,.95)


# Table 1 - number of days with hazard function above 0.03
kpdf %>% group_by(season) %>% count(haz > 0.028) 



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
  ylab("Hazard estimate h(t)") + xlab("Days since 1 November (t)")+
  
  facet_wrap(~season, ncol=1)  + theme_bw(base_size=15) + 
  theme(legend.position = "none") 


# 3 # ##### FIGURE 3 ##### 
#----------------------------------------


ggplot() + 
  geom_point(aes(x=time, y=season), data= chksNat, shape=4, size=3, color="black") + 
  
  geom_point(aes(x=time, y=season), 
             data=kpdf028, shape=18, color="red", alpha=0.4, size=10) +
  xlab("days since 1 November (t)") + ylab(NULL)+
  scale_y_discrete(limits = c("2020/21", "2019/20","2018/19", "2017/18","2016/17",  "2015/16","2014/15")) +
  theme_bw(base_size=20)



# 4 # ##### FIGURE 4 #####
#----------------------------------------






# 5 # ##### FIGURE 5 ##### 
#----------------------------------------

## now to add the wgbt "days over 22 deg WBGT" 


#bom_summary with 7daylag column added 

kpdf028 # this is the 'high stress day data' 

# round up 'time' to be a full day
kpdf028$days1Nov.x <- ceiling(kpdf028$time)

bom_summary$season <- bom_summary$season.x

highstress <- left_join(kpdf028, bom_summary, by=c("season", "days1Nov.x"))

## now just bar plot of max wbgt in the lead up to high stress days and normal days 

boxplot(highstress$wbgtBOM7)
boxplot(bom_summary$wbgtBOM7)


## okay no, first combine haz with

kpdf # why are their .5 ?? 

left_join


kpdf028 <- kpdf %>% filter(haz > 0.028)
#%>% add_row(time=NA, haz=NA, var=NA, strata=1, season="2014/15") %>% add_row(time=NA, haz=NA, var=NA, strata=1, season="2016/17")# append the blank years 


# instead ... just add a mutate column labelled 'high stress' to this 

kpmet <- kpmet %>% mutate(highstress= if_else(haz > 0.028, 1, 0))

kpmet <- kpmet %>% mutate(highstress2 = if_else(season=="2018/19", 2, highstress))


#kpdf$days1Nov.x <- ceiling(kpdf$time)

#bom_summary$season <- bom_summary$season.x

#HS <- left_join(kpdf, bom_summary, by=c("season", "days1Nov.x"))

kpmet$highstress2 <- as.factor(kpmet$highstress2)

ggplot(data=kpmet)+ 
  geom_boxplot(aes(x=highstress2, y=lag2)) + theme_classic()

par(mar = c(6,6,6,6))
boxplot(lag4 ~ highstress, data=kpmet, names= c("Low stress day", "High stress day"), xlab=NULL, ylab="summed WBGT (deg C) 4 days prior", cex.lab=1.8, cex.axis=2)

HS1 <- as.data.frame(kpmet %>% filter(highstress==1))
HS0 <- as.data.frame(kpmet %>% filter(highstress==0))
var.test(HS1, HS0)


t.test(lag4 ~ highstress, var.equal = TRUE, paired=FALSE, data=kpmet)

   
