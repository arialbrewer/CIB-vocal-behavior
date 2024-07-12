#Arial Brewer
#PhD- Chapter 2 Vocal Behavior
#Explore call category patterns in 2021 and 2022 data

#load packages
library(tidyverse)
library(viridis)

#read in data
setwd("C:/Users/Arial/OneDrive - UW/Desktop/Ch.2 vocal behavior/CIB vocal behavior code/")

acoustic_files <- list.files(pattern = "acoustic.csv")
behavior_files <- list.files(pattern = "behavior.csv")

acoustic_data <- acoustic_files %>%  
  map(read_csv) %>% 
  reduce(rbind) %>% 
  dplyr::select(-begin_Time,-end_Time,-low_Freq,-high_Freq,-selection,-call_type)

behavior_data <- behavior_files %>%  
  map(read_csv) %>% 
  reduce(rbind) %>% 
  dplyr::select(-sample_round,-group_number,-dot,-count_white,-count_gray,-count_calf,-comments)

#join acoustic and behavior datasets
data_total <- behavior_data %>% 
  left_join(acoustic_data, by = c("date","time"), multiple = "all") 

#bin into calls per minute
callrate <- data_total %>%
  mutate(num.calls = case_when(is.na(call_category)~0,TRUE~1)) %>% 
  group_by(date,time,encounter,tide,group_size,calf_presence,behavior) %>% 
  summarise(n_minute = sum(num.calls)) %>% 
  mutate(behavior = as.factor(behavior),
         calf_presence = as.factor(calf_presence),
         tide = as.factor(tide),
         encounter = as.factor(encounter),
         date = mdy(date),
         time = hms(time)) %>% 
  group_by(encounter) %>% 
  mutate(minute=row_number())

#calls per minute by call category
callrate_cattype <- data_total %>%
  group_by(date,time,encounter,tide,group_size,calf_presence,behavior,call_category) %>% 
  summarise(n_minute = n()) %>% 
  mutate(n_minute = case_when(is.na(call_category)~0, TRUE~n_minute)) %>% 
  pivot_wider(names_from = call_category, values_from = n_minute) %>% 
  replace(is.na(.), 0) %>% 
  mutate(behavior = as.factor(behavior),
         calf_presence = as.factor(calf_presence),
         tide = as.factor(tide),
         encounter = as.factor(encounter),
         date = mdy(date),
         time = hms(time)) %>% 
  group_by(encounter) %>% 
  mutate(minute=row_number())


###Individual encounter plots
##2021 data
#encounter 3
ggplot(data=callrate_cattype %>% filter(encounter==3)) +
  geom_line(aes(x=minute,y=ws),color="cyan4",size=1) +
  geom_line(aes(x=minute,y=pc),color="darkseagreen",size=1) +
  geom_line(aes(x=minute,y=cc),color="gold2",size=1) +
  theme_classic() +
  labs(x="Minutes since start of encounter",y="Count")+
  ggtitle("2021- encounter 3") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,50,by=2)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,100,by=2)) 

#encounter 4
ggplot(data=callrate_cattype %>% filter(encounter==4)) +
  geom_line(aes(x=minute,y=ws),color="cyan4",size=1) +
  geom_line(aes(x=minute,y=pc),color="darkseagreen",size=1) +
  geom_line(aes(x=minute,y=cc),color="gold2",size=1) +
  theme_classic() +
  labs(x="Minutes since start of encounter",y="Count")+
  ggtitle("2021- encounter 4") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,50,by=2)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,100,by=2)) 

#encounter 5
ggplot(data=callrate_cattype %>% filter(encounter==5)) +
  geom_line(aes(x=minute,y=ws),color="cyan4",size=1) +
  geom_line(aes(x=minute,y=pc),color="darkseagreen",size=1) +
  geom_line(aes(x=minute,y=cc),color="gold2",size=1) +
  theme_classic() +
  labs(x="Minutes since start of encounter",y="Count")+
  ggtitle("2021- encounter 5") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,50,by=2)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,100,by=2)) 

#2022 data
#encounter 13
ggplot(data=callrate_cattype %>% filter(encounter==13)) +
  geom_line(aes(x=minute,y=ws),color="cyan4",size=1) +
  geom_line(aes(x=minute,y=pc),color="darkseagreen",size=1) +
  geom_line(aes(x=minute,y=cc),color="gold2",size=1) +
  theme_classic() +
  labs(x="Minutes since start of encounter",y="Count")+
  ggtitle("2021- encounter 13") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,50,by=2)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,200,by=10)) 

#encounter 15
ggplot(data=callrate_cattype %>% filter(encounter==15)) +
  geom_line(aes(x=minute,y=ws),color="cyan4",size=1) +
  geom_line(aes(x=minute,y=pc),color="darkseagreen",size=1) +
  geom_line(aes(x=minute,y=cc),color="gold2",size=1) +
  theme_classic() +
  labs(x="Minutes since start of encounter",y="Count")+
  ggtitle("2021- encounter 15") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,50,by=2)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,300,by=5)) 

#encounter 16
ggplot(data=callrate_cattype %>% filter(encounter==16)) +
  geom_line(aes(x=minute,y=ws),color="cyan4",size=1) +
  geom_line(aes(x=minute,y=pc),color="darkseagreen",size=1) +
  geom_line(aes(x=minute,y=cc),color="gold2",size=1) +
  theme_classic() +
  labs(x="Minutes since start of encounter",y="Count")+
  ggtitle("2021- encounter 16") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,10,by=1)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,80,by=5)) 


#potential code for ribbon and area
#geom_ribbon(aes(ymin=18.3,ymax=18.4,fill=calf_presence),show.legend=F)+
#geom_area(aes(y=17.8,fill=behavior),alpha=0.3,show.legend=F)+


############################ Setting behavior changes to time zero
behav <- callrate_cattype %>% 
  select(date,minute,encounter,behavior) %>% 
  ungroup() %>% 
  group_by(encounter,behavior) %>% 
  mutate(behav_s=min(minute)) %>% 
  mutate(difftime_s=(minute-behav_s)) %>% 
  #mutate(behav_next=min(?????????????) %>% 
  mutate(difftime_next=(minute-behav_next))

  
#create new dataframe for milling to traveling change and rejoin with call category data
mill.travel <- behav %>% 
  select(minute,behavior,difftime_s,difftime_next) %>% 
  mutate(t_index=case_when(behavior=='mill'~difftime_next,
                           behavior=='travel'~difftime_s)) %>% 
  select(time,behavior,t_index) %>% 
  left_join(callrate_cattype)
  
  
#create new dataframe for traveling to milling change and rejoin with call category data
travel.mill <- behav %>% 
  select(minute,behavior,difftime_s,difftime_next) %>% 
  mutate(t_index=case_when(behavior=='travel'~difftime_next,
                           behavior=='mill'~difftime_s)) %>% 
  select(time,behavior,t_index) %>% 
  left_join(callrate_cattype)
  

#plot milling to traveling change
ggplot(mill.travel) +
  geom_line(aes(x=t_index,y=ws),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=pc),color="darkseagreen",size=1) +
  geom_line(aes(x=t_index,y=cc),color="gold2",size=1) +
  theme_classic()
  
  
#plot traveling to milling change
ggplot(travel.mill) +
  geom_line(aes(x=t_index,y=ws),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=pc),color="darkseagreen",size=1) +
  geom_line(aes(x=t_index,y=cc),color="gold2",size=1) +
  theme_classic()








