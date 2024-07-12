#Arial Brewer
#PhD- Chapter 2 Vocal Behavior
#Explore call type patterns in 2021 data

#load packages
library(tidyverse)

#read in data
setwd("C:/Users/Arial/OneDrive - UW/Desktop/Ch.2 vocal behavior/CIB vocal behavior code/")

acoustic_files <- intersect(list.files(pattern = "ER21"),list.files(pattern="acoustic.csv"))
behavior_files <- intersect(list.files(pattern = "ER21"),list.files(pattern="behavior.csv"))

acoustic_data <- acoustic_files %>%  
  map(read_csv) %>% 
  reduce(rbind) %>% 
  dplyr::select(-begin_Time,-end_Time,-low_Freq,-high_Freq,-selection)

behavior_data <- behavior_files %>%  
  map(read_csv) %>% 
  reduce(rbind) %>% 
  dplyr::select(-sample_round,-group_number,-dot,-count_white,-count_gray,-count_calf,-comments)

#join acoustic and behavior datasets
data_total <- behavior_data %>% 
  left_join(acoustic_data, by = c("date","time"), multiple = "all") 

calltype_count <- data_total %>% 
  group_by(call_type,call_category) %>% 
  summarise(number=n()) %>% 
  na.omit()

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
callrate_calltype <- data_total %>%
  group_by(date,time,encounter,tide,group_size,calf_presence,behavior,call_type) %>% 
  summarise(n_minute = n()) %>% 
  mutate(n_minute = case_when(is.na(call_type)~0, TRUE~n_minute)) %>% 
  pivot_wider(names_from = call_type, values_from = n_minute) %>% 
  replace(is.na(.), 0) %>% 
  mutate(behavior = as.factor(behavior),
         calf_presence = as.factor(calf_presence),
         tide = as.factor(tide),
         encounter = as.factor(encounter),
         date = mdy(date),
         time = hms(time)) %>% 
  group_by(encounter) %>% 
  mutate(minute=row_number())



#call type counts
pal <- c("gold2","darkseagreen","cyan4")
ggplot(data=calltype_count, aes(x=number, y=reorder(call_type,number),fill=call_category)) +
  geom_col()+
  theme_classic()+
  labs(x="Number", y="Call type")+
  scale_fill_manual(values=pal) +
  scale_x_continuous(expand=c(0,0))


###Individual encounter plots
##2021 data
#encounter 3
ggplot(data=callrate_calltype %>% filter(encounter==3)) +
  geom_line(aes(x=minute,y=?????????     #####how to put line for 40 call types?????
    
  
  geom_line(aes(x=minute,y=ws),color="cyan4",size=1) +
  geom_line(aes(x=minute,y=pc),color="darkseagreen",size=1) +
  geom_line(aes(x=minute,y=cc),color="gold2",size=1) +
  theme_classic() +
  labs(x="Minutes since start of encounter",y="Count")+
  ggtitle("2021- encounter 16") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,10,by=1)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,80,by=5)) 
  
  
  
#encounter 4
  
#encounter 5

