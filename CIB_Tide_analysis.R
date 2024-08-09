#HOBO vs NOAA tide plots

#load libraries
library(tidyverse)
library(lme4)
library(MASS)
library(performance)
library(parameters)
library(see)
library(DHARMa)


#HOBO and NOAA tide data for 7 days of 2022 CIB data
tide_total <- read.csv("2022_combined tide data.csv") %>% 
  group_by(date) %>% 
  mutate(date = mdy(date),
         time = hm(time))

#plots of all days
ggplot(data=tide_total) + 
  geom_line(aes(x=time,y=water_level_HOBO,group=date,color="red")) +
  geom_line(aes(x=time,y=water_level_NOAA,group=date,color="blue")) +
  theme_classic() +
  labs(x="Time", y="Water level") +
  scale_x_time() +
  theme(axis.text.x=element_text(angle=45,size=8,hjust=1)) +
  facet_wrap(~date) 
  
#individual day
ggplot(data=tide_total %>% filter(date=="2022-08-08")) + 
  geom_line(aes(x=time,y=water_level_HOBO,group=date,color="red")) +
  geom_line(aes(x=time,y=water_level_NOAA,group=date,color="blue")) +
  theme_classic() +
  labs(x="Time", y="Water level") +
  scale_x_time() 


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

#bin by 8 minutes
breaks <- seq(0,8,1)

bin_data <- callrate_total %>% 
  mutate(bin_durations= cut(time,breaks=breaks)) %>% 
  group_by(encounter,time,bin_durations) %>% 
  summarise(bin_n_miunte=sum(n_minute))

bin_data2<- callrate_total %>% 
  group_by(encounter,time=floor_hms(time,"8minute")) %>% 
  summarise(bin_n_minute=sum(n_minute))




#read in 2022 HOBO data for 7 days of beluga data
hobo <- read.csv("2022_HOBO data_7days.csv") %>% 
  mutate(date = mdy(date),
         time = hm(time))

#combine hobo data and beluga data
new_data <- callrate_total %>% 
  left_join(hobo, by = c("date","time"), multiple = "all")


###run call rate model
#poisson
#with just tide and encounter
test0<-glmer(n_minute ~ water_level_HOBO + (1|encounter),
             family=poisson(link="log"), data=new_data)

#with all covariates
test0<-glmer(n_minute ~ behavior + group_size + calf_presence + tide + water_level_HOBO + (1|encounter),
            family=poisson(link="log"), data=new_data)

summary(test0)

#check overdispersion parameter manually (X2/df.resid) Overdispersed > 1
sum(residuals(test0,type="pearson")^2)/79

#check overdispersion with performance package
check_overdispersion(test0) #overdispersed

#check zero-inflation
check_zeroinflation(test0)  #zero-inflation

#check residuals- better with nb
simulateResiduals(fittedModel = test0, plot = T)


### Poisson model is overdispersed and zero-inflated, use negative binomial
##model with tide data only
test1<-glmer.nb(n_minute ~ water_level_HOBO + (1|encounter),
                  data=new_data)

summary(test1)
plot(parameters(test1))

#Fit negative binomial model with all covariates
#log(group_size)=0.35 so can't use offset
test2<-glmer.nb(n_minute ~ behavior + log(group_size) + calf_presence + tide + water_level_HOBO + (1|encounter),
                data=new_data)

test2<-glmer.nb(n_minute ~ behavior + group_size + calf_presence + tide + water_level_HOBO + (1|encounter),
                data=new_data)

summary(test2)
plot(parameters(test2))

#check overdispersion parameter manually (X2/df.resid)
sum(residuals(test2,type="pearson")^2)/78

#check overdispersion with performance package
check_overdispersion(test2)  #no over-dispersion

#check zero-inflation
check_zeroinflation(test2)   #no zero-inflation

#check residuals
simulateResiduals(fittedModel = test2, plot = T)












