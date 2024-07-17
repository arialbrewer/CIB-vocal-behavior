#Arial Brewer
#PhD- Chapter 2 Vocal Behavior
#Explore call type patterns in 2021 data

#load packages
library(tidyverse)
library(viridis)
library(ggridges)

#read in data
setwd("C:/Users/Arial/OneDrive - UW/Desktop/Ch.2 vocal behavior/CIB vocal behavior code/")
acoustic_files <- intersect(list.files(pattern = "ER21"),list.files(pattern="acoustic.csv"))
behavior_files <- intersect(list.files(pattern = "ER21"),list.files(pattern="behavior.csv"))

#tidy data
acoustic_data <- acoustic_files %>%  
  map(read_csv) %>% 
  reduce(rbind) %>% 
  dplyr::select(-begin_Time,-end_Time,-low_Freq,-high_Freq,-selection)

behavior_data <- behavior_files %>%  
  map(read_csv) %>% 
  reduce(rbind) %>% 
  dplyr::select(-sample_round,-group_number,-dot,-count_white,-count_gray,-count_calf,-comments)

#merge acoustic and behavior data
data_total <- behavior_data %>% 
  left_join(acoustic_data, by = c("date","time"), multiple = "all") 

#bin into calls per minute
callrate <- data_total %>%
  mutate(num.calls = case_when(is.na(call_type)~0,TRUE~1)) %>% 
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

#calls per minute by call type
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

#call type distribution- all 2021 encounters
calltype_count <- data_total %>% 
  group_by(call_type,call_category) %>% 
  summarise(number=n()) %>% 
  na.omit()

pal <- c("gold2","darkseagreen","cyan4")
ggplot(data=calltype_count, aes(x=number, y=reorder(call_type,number),fill=call_category)) +
  geom_col()+
  theme_classic()+
  labs(x="Number", y="Call type")+
  scale_fill_manual(values=pal) +
  scale_x_continuous(expand=c(0,0))


###### Behavioral transitions
#call type distribution- behav transition-only encounters
calltype_count_behav <- data_total %>% 
    filter(encounter %in% c(3,4,7)) %>% 
    group_by(call_type,call_category) %>% 
    summarise(number=n()) %>% 
    na.omit() 

ggplot(data=calltype_count_behav, aes(x=number, y=reorder(call_type,number),fill=call_category)) +
  geom_col()+
  theme_classic()+
  labs(x="Number", y="Call type")+
  scale_fill_manual(values=pal) +
  scale_x_continuous(expand=c(0,0))


#filter by the 3 encounters where transitions occur, remove calls with n=1, and set transitions to time zero
# behav_tran_type <- callrate_calltype %>% 
#   filter(encounter %in% c(3,4,7)) %>% 
#   dplyr::select(date,minute,encounter,behavior,pulse.flat, flatws,aws,dws,pulse.flat.seg, pulse.d,modws,pulse.a,
#                 uws,modws.seg,flatws.seg,pulse.mod,pulse.n,nws,c.13,nws.seq,modws.m,c.9,aws.seg,trill,rws,pulse.mod.seg,
#                 c.10,pulse.mod.bc,pulse.flat.seg.2,c.12) %>% 
#   ungroup() %>% 
#   group_by(encounter,behavior) %>% 
#   mutate(behav_s=min(minute)) %>% 
#   mutate(difftime_s=(minute-behav_s))
# mutate(behav_next=min(??????) %>%  #behav_s of next group
# mutate(difftime_next=(minute-behav_next))

#couldn't get code to work for last two lines so saving and doing manually
#write_csv(behav_tran_type,"C:/Users/Arial/OneDrive - UW/Desktop/Ch.2 vocal behavior/CIB vocal behavior code/behav_tran_type.csv")

#manually added behav_next and difftime_next
behav <- read_csv("behav_tran_type.csv")

#create new dataframe for milling to traveling change
# mill.travel <- behav %>% 
#   dplyr::select(minute,behavior,difftime_s,difftime_next,pulse.flat, flatws,aws,dws,pulse.flat.seg, pulse.d,modws,pulse.a,
#                 uws,modws.seg,flatws.seg,pulse.mod,pulse.n,nws,c.13,nws.seq,modws.m,c.9,aws.seg,trill,rws,pulse.mod.seg,
#                 c.10,pulse.mod.bc,pulse.flat.seg.2,c.12) %>% 
#   mutate(t_index=case_when(behavior=='Mill'~difftime_next,
#                            behavior=='Travel'~difftime_s)) %>% 
#   dplyr::select(minute,behavior,t_index,pulse.flat, flatws,aws,dws,pulse.flat.seg, pulse.d,modws,pulse.a,
#                 uws,modws.seg,flatws.seg,pulse.mod,pulse.n,nws,c.13,nws.seq,modws.m,c.9,aws.seg,trill,rws,pulse.mod.seg,
#                 c.10,pulse.mod.bc,pulse.flat.seg.2,c.12) 

#code not working with multiple transitions within one encounter, will manually edit
#write_csv(mill.travel,"C:/Users/Arial/OneDrive - UW/Desktop/Ch.2 vocal behavior/CIB vocal behavior code/mill.travel_type.csv")

#read in updated data and change call types from wider to longer format
mill.travel <- read_csv("mill.travel_type.csv") %>% 
  pivot_longer(cols = c("pulse.flat","flatws","aws","dws","pulse.flat.seg","pulse.d","modws","pulse.a","uws",
                        "modws.seg","flatws.seg","pulse.mod","pulse.n","nws","c.13","nws.seq","modws.m","c.9",
                        "aws.seg","trill","rws","pulse.mod.seg","c.10","pulse.mod.bc","pulse.flat.seg.2","c.12"), 
               names_to = "call_type", values_to = "num_calls") %>% 
  mutate(call_type=as.factor(call_type))

###Plots- milling to traveling
#barplot
ggplot(data=mill.travel, aes(x=t_index,y=num_calls,fill=call_type)) + 
  geom_bar(stat="identity") +
  theme_classic() +
  geom_vline(xintercept=0, size=0.5,lty=2) +
  labs(x="Time", y="Count",fill="Call type") +
  scale_y_continuous(expand=c(0,0)) +
  ggtitle("Milling to traveling") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_fill_viridis(discrete=TRUE,option="D")


#ridge plot- having issue with plotting height
ggplot(mill.travel) +
  geom_density_ridges(aes(x=t_index,y=reorder(call_type,num_calls),group=call_type, #height=num_calls,
                          fill=call_type,alpha=0.8)) +
  geom_vline(xintercept=0, size=0.5,lty=2) +
  labs(x="Time", y="Call type") +
  ggtitle("Milling to traveling") +
  scale_fill_viridis(discrete=TRUE,option="D")


#OR
ggplot(mill.travel) +
  geom_ridgeline(aes(x=t_index,y=reorder(call_type,num_calls),height=num_calls,fill=call_type,alpha=0.8)) +
  geom_vline(xintercept=0, size=0.5,lty=2) +
  labs(x="Time", y="Call type") +
  ggtitle("Milling to traveling") +
  scale_fill_viridis(discrete=TRUE,option="D")




#create new dataframe for traveling to milling change 
# travel.mill <- behav_new %>% 
#   dplyr::select(minute,behavior,difftime_s,difftime_next,pulse.flat, flatws,aws,dws,pulse.flat.seg, pulse.d,modws,pulse.a,
#                 uws,modws.seg,flatws.seg,pulse.mod,pulse.n,nws,c.13,nws.seq,modws.m,c.9,aws.seg,trill,rws,pulse.mod.seg,
#                 c.10,pulse.mod.bc,pulse.flat.seg.2,c.12) %>% 
#   mutate(t_index=case_when(behavior=='Travel'~difftime_next,
#                            behavior=='Mill'~difftime_s)) %>% 
#   dplyr::select(minute,behavior,t_index,pulse.flat, flatws,aws,dws,pulse.flat.seg, pulse.d,modws,pulse.a,
#                 uws,modws.seg,flatws.seg,pulse.mod,pulse.n,nws,c.13,nws.seq,modws.m,c.9,aws.seg,trill,rws,pulse.mod.seg,
#                 c.10,pulse.mod.bc,pulse.flat.seg.2,c.12)

#code not working with multiple transitions within one encounter, will manually edit
#write_csv(travel.mill,"C:/Users/Arial/OneDrive - UW/Desktop/Ch.2 vocal behavior/CIB vocal behavior code/travel.mill_type.csv")

#read in updated data and change call types from wider to longer format
travel.mill <- read_csv("travel.mill_type.csv") %>% 
  pivot_longer(cols = c("pulse.flat","flatws","aws","dws","pulse.flat.seg","pulse.d","modws","pulse.a","uws",
                        "modws.seg","flatws.seg","pulse.mod","pulse.n","nws","c.13","nws.seq","modws.m","c.9",
                        "aws.seg","trill","rws","pulse.mod.seg","c.10","pulse.mod.bc","pulse.flat.seg.2","c.12"), 
               names_to = "call_type", values_to = "num_calls")

#plot traveling to milling change










###### Calf transitions
#call type distribution- calf transition-only encounters
calltype_count_calf <- data_total %>% 
  filter(encounter %in% c(3,5,7)) %>% 
  group_by(call_type,call_category) %>% 
  summarise(number=n()) %>% 
  na.omit() 

ggplot(data=calltype_count_calf, aes(x=number, y=reorder(call_type,number),fill=call_category)) +
  geom_col()+
  theme_classic()+
  labs(x="Number", y="Call type")+
  scale_fill_manual(values=pal) +
  scale_x_continuous(expand=c(0,0))


#filter by the 5 encounters where transitions occur
# calf_tran_type <- callrate_calltype %>% 
#   filter(encounter %in% c(3,5,7)) %>% 
#   dplyr::select(date,minute,encounter,calf_presence,flatws,pulse.flat,dws,aws,pulse.flat.seg,flatws.seg,pulse.d,
#                 aws.seg,uws,pulse.mod,modws) %>% 
#   ungroup() %>% 
#   group_by(encounter,calf_presence) %>% 
#   mutate(calf_s=min(minute)) %>% 
#   mutate(difftime_s=(minute-calf_s))
# mutate(behav_next=min(??????) %>%  #behav_s of next group
# mutate(difftime_next=(minute-behav_next))

#couldn't get code to work for last two lines so saving and doing manually
#write_csv(calf_tran_type,"C:/Users/Arial/OneDrive - UW/Desktop/Ch.2 vocal behavior/CIB vocal behavior code/calf_tran_type.csv")

#manually added behav_next and difftime_next
calf_new <- read_csv("calf_tran_type.csv")


#create new dataframe for calf to no calf change
calf.nocalf <- calf_new %>% 
  dplyr::select(minute,calf_presence,difftime_s,difftime_next,flatws,pulse.flat,dws,aws,pulse.flat.seg,flatws.seg,pulse.d,
                aws.seg,uws,pulse.mod,modws) %>% 
  mutate(t_index=case_when(calf_presence=='yes'~difftime_next,
                           calf_presence=='no'~difftime_s)) %>% 
  dplyr::select(minute,calf_presence,t_index,flatws,pulse.flat,dws,aws,pulse.flat.seg,flatws.seg,pulse.d,
                aws.seg,uws,pulse.mod,modws) 

calf.nocalf <- calf.nocalf %>% 
  pivot_longer(cols = c("flatws","pulse.flat","dws","aws","pulse.flat.seg","flatws.seg","pulse.d","aws.seg","uws","pulse.mod",
                        "modws",), 
               names_to = "call_type", values_to = "num_calls")

#plot calf to no calf change






#create new dataframe for no calf to calf change
# nocalf.calf <- calf_new %>% 
#   dplyr::select(minute,calf_presence,difftime_s,difftime_next,flatws,pulse.flat,dws,aws,pulse.flat.seg,flatws.seg,pulse.d,
#                 aws.seg,uws,pulse.mod,modws) %>% 
#   mutate(t_index=case_when(calf_presence=='no'~difftime_next,
#                            calf_presence=='yes'~difftime_s)) %>% 
#   dplyr::select(minute,calf_presence,t_index,flatws,pulse.flat,dws,aws,pulse.flat.seg,flatws.seg,pulse.d,
#                 aws.seg,uws,pulse.mod,modws)

#code not working with multiple transitions within one encounter, will manually edit
#write_csv(nocalf.calf,"C:/Users/Arial/OneDrive - UW/Desktop/Ch.2 vocal behavior/CIB vocal behavior code/nocalf.calf_type.csv")

#read in updated data and change call types from wider to longer format
nocalf.calf_new <- read_csv("nocalf.calf_type.csv") %>% 
  pivot_longer(cols = c("flatws","pulse.flat","dws","aws","pulse.flat.seg","flatws.seg","pulse.d","aws.seg","uws","pulse.mod",
                        "modws",), 
               names_to = "call_type", values_to = "num_calls")

#plot no calf to calf change







