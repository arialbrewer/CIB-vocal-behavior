#HOBO vs NOAA tide plots
#call rate model with HOBO water level as covariate

#load libraries
library(tidyverse)
library(lme4)
library(MASS)
library(performance)
library(parameters)
library(see)
library(DHARMa)
library(Tides)


#HOBO and NOAA tide data for 7 days of 2022 CIB data
tide_total <- read.csv("2022_combined tide data.csv") %>% 
  group_by(date) %>% 
  mutate(date = mdy(date),
         time = hm(time))

#plots of all days
ggplot(data=tide_total) + 
  geom_line(aes(x=time,y=water_level_HOBO,group=date),color="blue") +
  geom_line(aes(x=time,y=water_level_NOAA,group=date),color="red")+
  theme_classic() +
  labs(x="Time", y="Water level") +
  scale_x_time() +
  theme(axis.text.x=element_text(angle=45,size=8,hjust=1)) +
  facet_wrap(~date) 
  
#individual day
ggplot(data=tide_total %>% filter(date=="2022-08-08")) + 
  geom_line(aes(x=time,y=water_level_HOBO,group=date),color="blue") +
  geom_line(aes(x=time,y=water_level_NOAA,group=date),color="red") +
  theme_classic() +
  labs(x="Time", y="Water level") +
  scale_x_time() 


###calculate difference between HOBO and NOAA height data
# HOBO <- read.csv("HOBO_14July22_8Sept22.csv") %>%
#   mutate(date = mdy(date),
#          time = hm(time))
# 
# NOAA <- read.csv("NOAA_anc_14July22_8Sept22.csv") %>%
#   mutate(date = mdy(date),
#          time = hm(time))

# #plots of all days NOAA tide
# ggplot(data=NOAA) + 
#   geom_line(aes(x=time,y=water_level_NOAA,group=date))+
#   theme_classic() +
#   labs(x="Time", y="Water level") +
#   scale_x_time() +
#   scale_y_continuous(breaks=seq(-10,40,by=10)) +
#   theme(axis.text.x=element_text(angle=45,size=8,hjust=1)) +
#   facet_wrap(~date)
# 
# #filtered by date
# ggplot(data=NOAA %>% filter(date=="2022-07-15")) + 
#   geom_line(aes(x=time,y=water_level_NOAA,group=date))+
#   theme_classic() +
#   labs(x="Time", y="Water level") +
#   scale_y_continuous(breaks=seq(-10,40,by=5)) +
#   scale_x_time()

###calculate diff height per day
# NOAA_diffheight <- NOAA %>% 
#   group_by(date) %>% 
#   mutate(max_height=max(water_level_NOAA),
#          min_height=min(water_level_NOAA),
#          diff_height=abs(max_height-min_height)) %>% 
#   dplyr::select(-time,-water_level_NOAA) %>% 
#   distinct(date,max_height,min_height,diff_height)
# 
# #plot
# ggplot(data=NOAA_diffheight,aes(x=date,y=diff_height))+
#   geom_bar(stat="identity") +
#   theme_classic() +
#   labs(x="Date", y="Water height (ft)") +
#   scale_x_date(date_breaks="1 week") +
#   scale_y_continuous(expand=c(0,0),breaks=seq(0,40,by=5)) +
#   theme(axis.text.x=element_text(angle=45,size=8,hjust=1)) 


### calculate time difference between high and low NOAA data
#read in high/low NOAA tide data
NOAA_highlow <- read.csv("NOAA_highlow.csv")
  






### calculate time difference between HOBO and NOAA high tides
high_tides <- read.csv("2022_high tides combined.csv") %>% 
  mutate(date_hobo = mdy(date_hobo),
         time_hobo = hm(time_hobo),
         date_noaa = mdy(date_noaa),
         time_noaa = hm(time_noaa))
#plot
ggplot(data=high_tides,aes(x=diff_time))  +
  geom_histogram(stat="count") +
  theme_classic() +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,30,by=5)) +
  labs(x="Time difference", y="Count") +
  ggtitle("Time difference between NOAA and HOBO high tides") +
  theme(plot.title=element_text(hjust=0.5)) 






##############model beluga data with tide data
#load beluga data
setwd("C:/Users/Arial/Desktop/Ch.2 vocal behavior/CIB vocal behavior code/")
acoustic_files <- list.files(pattern = "acoustic.csv")
behavior_files <- list.files(pattern = "behavior.csv")

#tidy data
acoustic_data <- acoustic_files %>%  
  map(read_csv) %>% 
  reduce(rbind) %>% 
  dplyr::select(-begin_Time,-end_Time,-low_Freq,-high_Freq,-selection,-call_type)

behavior_data <- behavior_files %>%  
  map(read_csv) %>% 
  reduce(rbind) %>% 
  dplyr::select(-sample_round,-group_number,-comments)

#combine behavioral & acoustic data and only include 2022 encounters
data_total <- behavior_data %>% 
  left_join(acoustic_data, by = c("date","time"), multiple = "all") %>% 
  filter(encounter %in% c(11,12,13,14,15,16,17,18,19,20,21)) 

#Bin data into calls per minute 
callrate_total <- data_total %>%
  mutate(num.calls = case_when(is.na(call_category)~0,TRUE~1)) %>% 
  group_by(date,time,encounter,tide,group_size,calf_presence,behavior) %>% 
  summarise(n_minute = sum(num.calls)) %>% 
  mutate(date = mdy(date),
         time = hms(time),
         behavior = as.factor(behavior),
         calf_presence = as.factor(calf_presence),
         tide = as.factor(tide),
         encounter = as.factor(encounter))


#read in 2022 HOBO data for 7 days of beluga data
hobo <- read.csv("2022_HOBO data_7days.csv") %>% 
  mutate(date = mdy(date),
         time = hm(time))

#combine hobo data and beluga data
new_data <- callrate_total %>% 
  left_join(hobo, by = c("date","time"), multiple = "all")


####### Run call rate model
#Poisson with just tide and encounter
pois.uni<-glmer(n_minute ~ water_level_HOBO + (1|encounter),
             family=poisson(link="log"), data=new_data)

summary(pois.uni)
plot(parameters(pois.uni))

#check overdispersion parameter manually (X2/df.resid) Overdispersed > 1
sum(residuals(pois.uni,type="pearson")^2)/83  #overdispersed

#check overdispersion with performance package
check_overdispersion(pois.uni) #overdispersed

#check zero-inflation
check_zeroinflation(pois.uni)  #zero-inflation



#Poisson with all covariates
pois.multi<-glmer(n_minute ~ behavior + group_size + calf_presence + tide + water_level_HOBO + (1|encounter),
            family=poisson(link="log"), data=new_data)

summary(pois.multi)
plot(parameters(pois.multi))

#check overdispersion parameter manually (X2/df.resid) Overdispersed > 1
sum(residuals(pois.multi,type="pearson")^2)/79   #overdispersed

#check overdispersion with performance package
check_overdispersion(pois.multi) #overdispersed

#check zero-inflation
check_zeroinflation(pois.multi)  #zero-inflation

#check residuals
simulateResiduals(fittedModel = pois.multi, plot = T)


### Poisson model is overdispersed and zero-inflated, use negative binomial
##NB model with just tide and encounter
nb.uni<-glmer.nb(n_minute ~ water_level_HOBO + (1|encounter),
                  data=new_data)

summary(nb.uni)
plot(parameters(nb.uni))

#check overdispersion parameter manually (X2/df.resid)
sum(residuals(nb.uni,type="pearson")^2)/82  #no over-dispersion

#check overdispersion with performance package
check_overdispersion(nb.uni)  #no over-dispersion

#check zero-inflation
check_zeroinflation(nb.uni)   #no zero-inflation

#check residuals
simulateResiduals(fittedModel = nb.uni, plot = T)


#NB model with all covariates
#log(group_size)=0.35 so can't use offset
test<-glmer.nb(n_minute ~ behavior + log(group_size) + calf_presence + tide + water_level_HOBO + (1|encounter),
                data=new_data)

summary(test)
plot(parameters(test))


nb.multi<-glmer.nb(n_minute ~ behavior + group_size + calf_presence + tide + water_level_HOBO + (1|encounter),
                data=new_data)

summary(nb.multi)
plot(parameters(nb.multi))

#check overdispersion parameter manually (X2/df.resid)
sum(residuals(nb.multi,type="pearson")^2)/78

#check overdispersion with performance package
check_overdispersion(nb.multi)  #no over-dispersion

#check zero-inflation
check_zeroinflation(nb.multi)   #no zero-inflation

#check residuals
simulateResiduals(fittedModel = nb.multi, plot = T)












