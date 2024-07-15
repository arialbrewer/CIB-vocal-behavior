#Arial Brewer
#PhD- Chapter 2 Vocal Behavior
#Explore call type patterns in 2021 data

#load packages
library(tidyverse)

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
behav_tran_type <- callrate_calltype %>% 
  filter(encounter %in% c(3,4,7)) %>% 
  dplyr::select(date,minute,encounter,behavior,pulse.flat, flatws,aws,dws,pulse.flat.seg, pulse.d,modws,pulse.a,
                uws,modws.seg,flatws.seg,pulse.mod,pulse.n,nws,c.13,nws.seq,modws.m,c.9,aws.seg,trill,rws,pulse.mod.seg,
                c.10,pulse.mod.bc,pulse.flat.seg.2,c.12) %>% 
  ungroup() %>% 
  group_by(encounter,behavior) %>% 
  mutate(behav_s=min(minute)) %>% 
  mutate(difftime_s=(minute-behav_s))
# mutate(behav_next=min(??????) %>%  #behav_s of next group
# mutate(difftime_next=(minute-behav_next))

#couldn't get code to work for last two lines so saving and doing manually
#write_csv(behav_tran_type,"C:/Users/Arial/OneDrive - UW/Desktop/Ch.2 vocal behavior/CIB vocal behavior code/behav_tran_type.csv")

#manually added behav_next and difftime_next
behav_new <- read_csv("behav_tran_type.csv")

#create new dataframe for milling to traveling change
mill.travel <- behav_new %>% 
  dplyr::select(minute,behavior,difftime_s,difftime_next,pulse.flat, flatws,aws,dws,pulse.flat.seg, pulse.d,modws,pulse.a,
                uws,modws.seg,flatws.seg,pulse.mod,pulse.n,nws,c.13,nws.seq,modws.m,c.9,aws.seg,trill,rws,pulse.mod.seg,
                c.10,pulse.mod.bc,pulse.flat.seg.2,c.12) %>% 
  mutate(t_index=case_when(behavior=='Mill'~difftime_next,
                           behavior=='Travel'~difftime_s)) %>% 
  dplyr::select(minute,behavior,t_index,pulse.flat, flatws,aws,dws,pulse.flat.seg, pulse.d,modws,pulse.a,
                uws,modws.seg,flatws.seg,pulse.mod,pulse.n,nws,c.13,nws.seq,modws.m,c.9,aws.seg,trill,rws,pulse.mod.seg,
                c.10,pulse.mod.bc,pulse.flat.seg.2,c.12) 

#write_csv(mill.travel,"C:/Users/Arial/OneDrive - UW/Desktop/Ch.2 vocal behavior/CIB vocal behavior code/mill.travel_type.csv")

#code not working with multiple transitions within one encounter, manually edited:
mill.travel_new <- read_csv("mill.travel_type.csv")


#plot milling to traveling change
ggplot(mill.travel_new) +
  geom_line(aes(x=t_index,y=pulse.flat),color="darkseagreen",size=1) +
  geom_line(aes(x=t_index,y=flatws),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=aws),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=dws),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=pulse.flat.seg),color="darkseagreen",size=1) +
  geom_line(aes(x=t_index,y=pulse.d),color="darkseagreen",size=1) +
  geom_line(aes(x=t_index,y=modws),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=pulse.a),color="darkseagreen",size=1) +
  geom_line(aes(x=t_index,y=uws),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=modws.seg),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=flatws.seg),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=pulse.mod),color="darkseagreen",size=1) +
  geom_line(aes(x=t_index,y=pulse.n),color="darkseagreen",size=1) +
  geom_line(aes(x=t_index,y=nws),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=c.13),color="gold2",size=1) +
  geom_line(aes(x=t_index,y=nws.seq),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=modws.m),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=c.9),color="gold2",size=1) +
  geom_line(aes(x=t_index,y=aws.seg),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=trill),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=rws),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=pulse.mod.seg),color="darkseagreen",size=1) +
  geom_line(aes(x=t_index,y=c.10),color="gold2",size=1) +
  geom_line(aes(x=t_index,y=pulse.mod.bc),color="darkseagreen",size=1) +
  geom_line(aes(x=t_index,y=pulse.flat.seg.2),color="darkseagreen",size=1) +
  geom_line(aes(x=t_index,y=c.12),color="gold2",size=1) +
  geom_vline(xintercept=0, size=1,lty=2) +
  theme_classic() +
  labs(x="Time",y="Count") +
  ggtitle("Milling to traveling") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,50,by=5)) 


##ridge plot
library(ggridges)
ggplot(mill.travel_new) +
  geom_density_ridges(aes(x=t_index,y=pulse.flat)) +
  geom_density_ridges(aes(x=t_index,y=flatws)) +
  theme_ridges()




#create new dataframe for traveling to milling change 
travel.mill <- behav_new %>% 
  dplyr::select(minute,behavior,difftime_s,difftime_next,pulse.flat, flatws,aws,dws,pulse.flat.seg, pulse.d,modws,pulse.a,
                uws,modws.seg,flatws.seg,pulse.mod,pulse.n,nws,c.13,nws.seq,modws.m,c.9,aws.seg,trill,rws,pulse.mod.seg,
                c.10,pulse.mod.bc,pulse.flat.seg.2,c.12) %>% 
  mutate(t_index=case_when(behavior=='Travel'~difftime_next,
                           behavior=='Mill'~difftime_s)) %>% 
  dplyr::select(minute,behavior,t_index,pulse.flat, flatws,aws,dws,pulse.flat.seg, pulse.d,modws,pulse.a,
                uws,modws.seg,flatws.seg,pulse.mod,pulse.n,nws,c.13,nws.seq,modws.m,c.9,aws.seg,trill,rws,pulse.mod.seg,
                c.10,pulse.mod.bc,pulse.flat.seg.2,c.12)

#write_csv(travel.mill,"C:/Users/Arial/OneDrive - UW/Desktop/Ch.2 vocal behavior/CIB vocal behavior code/travel.mill_type.csv")

#code not working with multiple transitions within one encounter, manually edited:
travel.mill_new <- read_csv("travel.mill_type.csv")


#plot traveling to milling change
ggplot(travel.mill_new) +
  geom_line(aes(x=t_index,y=pulse.flat),color="darkseagreen",size=1) +
  geom_line(aes(x=t_index,y=flatws),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=aws),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=dws),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=pulse.flat.seg),color="darkseagreen",size=1) +
  geom_line(aes(x=t_index,y=pulse.d),color="darkseagreen",size=1) +
  geom_line(aes(x=t_index,y=modws),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=pulse.a),color="darkseagreen",size=1) +
  geom_line(aes(x=t_index,y=uws),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=modws.seg),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=flatws.seg),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=pulse.mod),color="darkseagreen",size=1) +
  geom_line(aes(x=t_index,y=pulse.n),color="darkseagreen",size=1) +
  geom_line(aes(x=t_index,y=nws),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=c.13),color="gold2",size=1) +
  geom_line(aes(x=t_index,y=nws.seq),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=modws.m),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=c.9),color="gold2",size=1) +
  geom_line(aes(x=t_index,y=aws.seg),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=trill),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=rws),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=pulse.mod.seg),color="darkseagreen",size=1) +
  geom_line(aes(x=t_index,y=c.10),color="gold2",size=1) +
  geom_line(aes(x=t_index,y=pulse.mod.bc),color="darkseagreen",size=1) +
  geom_line(aes(x=t_index,y=pulse.flat.seg.2),color="darkseagreen",size=1) +
  geom_line(aes(x=t_index,y=c.12),color="gold2",size=1) +
  geom_vline(xintercept=0, size=1,lty=2) +
  theme_classic() +
  labs(x="Time",y="Count") +
  ggtitle("Traveling to milling") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,50,by=5)) 





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
calf_tran_type <- callrate_calltype %>% 
  filter(encounter %in% c(3,5,7)) %>% 
  dplyr::select(date,minute,encounter,calf_presence,flatws,pulse.flat,dws,aws,pulse.flat.seg,flatws.seg,pulse.d,
                aws.seg,uws,pulse.mod,modws) %>% 
  ungroup() %>% 
  group_by(encounter,calf_presence) %>% 
  mutate(calf_s=min(minute)) %>% 
  mutate(difftime_s=(minute-calf_s))
# mutate(behav_next=min(??????) %>%  #behav_s of next group
# mutate(difftime_next=(minute-behav_next))

#couldn't get code to work for last two lines so saving and doing manually
#write_csv(calf_tran_type,"C:/Users/Arial/OneDrive - UW/Desktop/Ch.2 vocal behavior/CIB vocal behavior code/calf_tran_type.csv")

#manually added calf_next and difftime_next
calf_new <- read_csv("calf_tran_type.csv")


#create new dataframe for calf to no calf change
calf.nocalf <- calf_new %>% 
  dplyr::select(minute,calf_presence,difftime_s,difftime_next,flatws,pulse.flat,dws,aws,pulse.flat.seg,flatws.seg,pulse.d,
                aws.seg,uws,pulse.mod,modws) %>% 
  mutate(t_index=case_when(calf_presence=='yes'~difftime_next,
                           calf_presence=='no'~difftime_s)) %>% 
  dplyr::select(minute,calf_presence,t_index,flatws,pulse.flat,dws,aws,pulse.flat.seg,flatws.seg,pulse.d,
                aws.seg,uws,pulse.mod,modws) 

#plot calf to no calf change
colors <- c("flatws"="cyan4",
            "pulse.flat"="darkgreen",
            "dws"="steelblue",
            "aws"="skyblue4",
            "pulse.flat.seg"= "forestgreen",
            "flatws.seg"="cyan3",
            "pulse.d"="darkseagreen",
            "aws.seg"="skyblue2",
            "uws"="royalblue",
            "pulse.mod"="blue",
            "modws"="darkblue")

ggplot(calf.nocalf) +
  geom_line(aes(x=t_index,y=flatws,color="flatws"),size=1) +
  geom_line(aes(x=t_index,y=pulse.flat,color="pulse.flat"),size=1) +
  geom_line(aes(x=t_index,y=dws,color="dws"),size=1) +
  geom_line(aes(x=t_index,y=aws,color="aws"),size=1) +
  geom_line(aes(x=t_index,y=pulse.flat.seg,color="pulse.flat.seg"),size=1) +
  geom_line(aes(x=t_index,y=flatws.seg,color="flatws.seg"),size=1) +
  geom_line(aes(x=t_index,y=pulse.d,color="pulse.d"),size=1) +
  geom_line(aes(x=t_index,y=aws.seg,color="aws.seg"),size=1) +
  geom_line(aes(x=t_index,y=uws,color="uws"),size=1) +
  geom_line(aes(x=t_index,y=pulse.mod,color="pulse.mod"),size=1) +
  geom_line(aes(x=t_index,y=modws,color="modws"),size=1) +
  geom_vline(xintercept=0, size=1,lty=2) +
  theme_classic() +
  labs(x="Time",y="Count",color="Legend") +
  scale_color_manual(values=colors) +
  ggtitle("Calf to no calf") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,50,by=5)) 




#create new dataframe for no calf to calf change
nocalf.calf <- calf_new %>% 
  dplyr::select(minute,calf_presence,difftime_s,difftime_next,flatws,pulse.flat,dws,aws,pulse.flat.seg,flatws.seg,pulse.d,
                aws.seg,uws,pulse.mod,modws) %>% 
  mutate(t_index=case_when(calf_presence=='no'~difftime_next,
                           calf_presence=='yes'~difftime_s)) %>% 
  dplyr::select(minute,calf_presence,t_index,flatws,pulse.flat,dws,aws,pulse.flat.seg,flatws.seg,pulse.d,
                aws.seg,uws,pulse.mod,modws)

#write_csv(nocalf.calf,"C:/Users/Arial/OneDrive - UW/Desktop/Ch.2 vocal behavior/CIB vocal behavior code/nocalf.calf_type.csv")

#code not working with multiple transitions within one encounter, manually edited:
nocalf.calf_new <- read_csv("nocalf.calf_type.csv")

#plot no calf to calf change
ggplot(nocalf.calf_new) +
  geom_line(aes(x=t_index,y=flatws),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=pulse.flat),color="darkseagreen",size=1) +
  geom_line(aes(x=t_index,y=dws),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=aws),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=pulse.flat.seg),color="darkseagreen",size=1) +
  geom_line(aes(x=t_index,y=flatws.seg),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=pulse.d),color="darkseagreen",size=1) +
  geom_line(aes(x=t_index,y=aws.seg),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=uws),color="cyan4",size=1) +
  geom_line(aes(x=t_index,y=pulse.mod),color="darkseagreen",size=1) +
  geom_line(aes(x=t_index,y=modws),color="cyan4",size=1) +
  geom_vline(xintercept=0, size=1,lty=2) +
  theme_classic() +
  labs(x="Time",y="Count") +
  ggtitle("No calf to calf") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,50,by=5)) 




