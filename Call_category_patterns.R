#Arial Brewer
#PhD- Chapter 2 Vocal Behavior
#Explore call category patterns in 2021 and 2022 data

#load packages
library(tidyverse)

#read in data
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
  dplyr::select(-sample_round,-group_number,-dot,-count_white,-count_gray,-count_calf,-comments)

#merge acoustic and behavior data
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


###Individual encounter plots of the encounters where transitions occur
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

#encounter 7
ggplot(data=callrate_cattype %>% filter(encounter==7)) +
  geom_line(aes(x=minute,y=ws),color="cyan4",size=1) +
  geom_line(aes(x=minute,y=pc),color="darkseagreen",size=1) +
  geom_line(aes(x=minute,y=cc),color="gold2",size=1) +
  theme_classic() +
  labs(x="Minutes since start of encounter",y="Count")+
  ggtitle("2021- encounter 7") +
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
  ggtitle("2022- encounter 13") +
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
  ggtitle("2022- encounter 15") +
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
  ggtitle("2022- encounter 16") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,10,by=1)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,80,by=5)) 

#encounter 20
ggplot(data=callrate_cattype %>% filter(encounter==20)) +
  geom_line(aes(x=minute,y=ws),color="cyan4",size=1) +
  geom_line(aes(x=minute,y=pc),color="darkseagreen",size=1) +
  geom_line(aes(x=minute,y=cc),color="gold2",size=1) +
  theme_classic() +
  labs(x="Minutes since start of encounter",y="Count")+
  ggtitle("2022- encounter 20") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,50,by=2)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,80,by=5)) 



####### Behavioral transitions
#filter by the 6 encounters where transitions occur and set transitions to time zero
# behav_tran_cat <- callrate_cattype %>% 
#   filter(encounter %in% c(3,4,7,13,15,16)) %>% 
#   dplyr::select(date,minute,encounter,behavior,ws,pc,cc) %>% 
#   ungroup() %>% 
#   group_by(encounter,behavior) %>% 
#   mutate(behav_s=min(minute)) %>% 
#   mutate(difftime_s=(minute-behav_s))
# mutate(behav_next=min(??????) %>%  #behav_s of next group
# mutate(difftime_next=(minute-behav_next))

#couldn't get code to work for last two lines so saving and doing manually
#write_csv(behav_tran_cat,"C:/Users/Arial/OneDrive - UW/Desktop/Ch.2 vocal behavior/CIB vocal behavior code/behav_tran_cat.csv")

#manually added behav_next and difftime_next
behav <- read_csv("behav_tran_cat.csv")

## create new dataframe for milling to traveling change
# mill.travel <- behav %>% 
#   dplyr::select(minute,behavior,difftime_s,difftime_next,ws,pc,cc) %>% 
#   mutate(t_index=case_when(behavior=='Mill'~difftime_next,
#                            behavior=='Travel'~difftime_s)) %>% 
#   dplyr::select(minute,behavior,t_index,ws,pc,cc) 

#code not working with multiple transitions within one encounter, will manually edit
#write_csv(mill.travel,"C:/Users/Arial/OneDrive - UW/Desktop/Ch.2 vocal behavior/CIB vocal behavior code/mill.travel_cat.csv")
  
#read in updated data and change call categories from wider to longer format
mill.travel <- read_csv("mill.travel_cat.csv") %>% 
  pivot_longer(cols = c("ws","pc","cc"), names_to = "call_cat", values_to = "num_calls")
  
#plot milling to traveling change
pal <- c("gold2","darkseagreen","cyan4")
ggplot(data=mill.travel, aes(x=t_index,y=num_calls,fill=call_cat)) + 
  geom_bar(stat="identity") +
  theme_classic() +
  geom_vline(xintercept=0, size=0.5,lty=2) +
  labs(x="Time", y="Count",fill="Call category") +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_manual(values=pal) +
  ggtitle("Milling to traveling") +
  theme(plot.title=element_text(hjust=0.5)) 


#create new dataframe for traveling to milling change 
# travel.mill <- behav_new %>% 
#   dplyr::select(minute,behavior,difftime_s,difftime_next,ws,pc,cc) %>% 
#   mutate(t_index=case_when(behavior=='Travel'~difftime_next,
#                            behavior=='Mill'~difftime_s)) %>% 
#   dplyr::select(minute,behavior,t_index,ws,pc,cc)

#code not working with multiple transitions within one encounter, will manually edit
#write_csv(travel.mill,"C:/Users/Arial/OneDrive - UW/Desktop/Ch.2 vocal behavior/CIB vocal behavior code/travel.mill_cat.csv")

#read in updated data and change call categories from wider to longer format
travel.mill <- read_csv("travel.mill_cat.csv") %>% 
  pivot_longer(cols = c("ws","pc","cc"), names_to = "call_cat", values_to = "num_calls")

#plot milling to traveling change
ggplot(data=travel.mill, aes(x=t_index,y=num_calls,fill=call_cat)) + 
  geom_bar(stat="identity") +
  theme_classic() +
  geom_vline(xintercept=0, size=0.5,lty=2) +
  labs(x="Time", y="Count",fill="Call category") +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_manual(values=pal) +
  ggtitle("Traveling to milling") +
  theme(plot.title=element_text(hjust=0.5)) 



####### Calf transitions
#filter by the 5 encounters where transitions occur
# calf_tran_cat <- callrate_cattype %>% 
#   filter(encounter %in% c(3,5,7,13,20)) %>% 
#   dplyr::select(date,minute,encounter,calf_presence,ws,pc,cc) %>% 
#   ungroup() %>% 
#   group_by(encounter,calf_presence) %>% 
#   mutate(calf_s=min(minute)) %>% 
#   mutate(difftime_s=(minute-calf_s))
# mutate(behav_next=min(??????) %>%  #behav_s of next group
# mutate(difftime_next=(minute-behav_next))

#couldn't get code to work for last two lines so saving and doing manually
#write_csv(calf_tran_cat,"C:/Users/Arial/OneDrive - UW/Desktop/Ch.2 vocal behavior/CIB vocal behavior code/calf_tran_cat.csv")

#manually added calf_next and difftime_next
calf_new <- read_csv("calf_tran_cat.csv")

#create new dataframe for calf to no calf change
calf.nocalf <- calf_new %>% 
  dplyr::select(minute,calf_presence,difftime_s,difftime_next,ws,pc,cc) %>% 
  mutate(t_index=case_when(calf_presence=='yes'~difftime_next,
                           calf_presence=='no'~difftime_s)) %>% 
  dplyr::select(minute,calf_presence,t_index,ws,pc,cc) 

#pivot longer
calf.nocalf <- calf.nocalf %>% 
  pivot_longer(cols = c("ws","pc","cc"), names_to = "call_cat", values_to = "num_calls")

#plot calf to no calf change
ggplot(data=calf.nocalf, aes(x=t_index,y=num_calls,fill=call_cat)) + 
  geom_bar(stat="identity") +
  theme_classic() +
  geom_vline(xintercept=0, size=0.5,lty=2) +
  labs(x="Time", y="Count",fill="Call category") +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(breaks=seq(-80,40,by=20)) +
  scale_fill_manual(values=pal) +
  ggtitle("Calves to no calves") +
  theme(plot.title=element_text(hjust=0.5)) 


#create new dataframe for no calf to calf change
# nocalf.calf <- calf_new %>% 
#   dplyr::select(minute,calf_presence,difftime_s,difftime_next,ws,pc,cc) %>% 
#   mutate(t_index=case_when(calf_presence=='no'~difftime_next,
#                            calf_presence=='yes'~difftime_s)) %>% 
#   dplyr::select(minute,calf_presence,t_index,ws,pc,cc)

#code not working with multiple transitions within one encounter, will manually edit
#write_csv(nocalf.calf,"C:/Users/Arial/OneDrive - UW/Desktop/Ch.2 vocal behavior/CIB vocal behavior code/nocalf.calf_cat.csv")

#read in updated data and change call categories from wider to longer format
nocalf.calf <- read_csv("nocalf.calf_cat.csv") %>% 
  pivot_longer(cols = c("ws","pc","cc"), names_to = "call_cat", values_to = "num_calls")

#plot no calf to calf
ggplot(data=nocalf.calf, aes(x=t_index,y=num_calls,fill=call_cat)) + 
  geom_bar(stat="identity") +
  theme_classic() +
  geom_vline(xintercept=0, size=0.5,lty=2) +
  labs(x="Time", y="Count",fill="Call category") +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(breaks=seq(-40,60,by=20)) +
  scale_fill_manual(values=pal) +
  ggtitle("No calves to calves") +
  theme(plot.title=element_text(hjust=0.5)) 




