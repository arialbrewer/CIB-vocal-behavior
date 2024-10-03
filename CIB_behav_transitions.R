#Arial Brewer
#PhD- Chapter 2 Vocal Behavior
#Explore behavioral transitions with call types in 2021 data

#load packages
library(tidyverse)
library(ggridges)
library(PNWColors)

#read in data
setwd("C:/Users/Arial/Desktop/Ch.2 vocal behavior/CIB vocal behavior code/")
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

pal3 <- c("gold2","darkseagreen","cyan4")
ggplot(data=calltype_count, aes(x=number, y=reorder(call_type,number),fill=call_category)) +
  geom_col()+
  theme_classic()+
  labs(x="Number", y="Call type")+
  scale_fill_manual(values=pal) +
  scale_x_continuous(expand=c(0,0))


############### Behavioral transitions
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





#################### MILLING TO TRAVELING

#read in data then remove call types with no pattern and change remaining call types from wider to longer format
mill.travel <- read_csv("mill.travel_type.csv") %>% 
  dplyr::select(-aws.seg) %>% #remove calls types that don't occur
  pivot_longer(cols = c("pulse.flat","flatws","aws","dws","pulse.flat.seg","pulse.d","modws",
                        "pulse.a","uws","modws.seg","flatws.seg","pulse.mod","pulse.n","nws",
                        "c.13","nws.seq","modws.m","c.9","trill","rws","pulse.mod.seg",
                        "c.10","pulse.mod.bc","pulse.flat.seg.2","c.12"),
               names_to = "call_type", values_to = "num_calls") %>% 
  mutate(date = mdy(date),
         minute=as.factor(minute),
         encounter = as.factor(encounter),
         behavior = as.factor(behavior),
         behavior2 = as.factor(behavior2),
         transition = as.factor(transition),
         call_type=as.factor(call_type)) %>% 
  group_by(transition,encounter)


#barplot by call type 
# ggplot(data=mill.travel, aes(x=t_index,y=num_calls,fill=call_type)) + 
#   geom_bar(stat="identity") +
#   theme_classic() +
#   geom_vline(xintercept=0, size=0.5,lty=2) +
#   labs(x="Time", y="Count",fill="Call type") +
#   scale_y_continuous(expand=c(0,0)) +
#   xlim(-15,5) +
#   ggtitle("Milling to traveling") +
#   theme(plot.title=element_text(hjust=0.5)) +
#   scale_fill_manual(values=pnw_palette("Bay",n=21)) +
#   facet_wrap(~call_type)

#barplot by encounter- need to zoom out of -15 to 5 to see encounter 3 and 6
# ggplot(data=mill.travel, aes(x=t_index,y=num_calls,fill=encounter)) + 
#   geom_bar(stat="identity") +
#   theme_classic() +
#   geom_vline(xintercept=0, size=0.5,lty=2) +
#   labs(x="Time", y="Count",fill="Encounter") +
#   scale_y_continuous(expand=c(0,0)) +
#   xlim(-15,5) +
#   ggtitle("Milling to traveling") +
#   theme(plot.title=element_text(hjust=0.5)) +
#   scale_fill_manual(values=pnw_palette("Bay",n=3)) +
#   facet_wrap(~call_type)

#Line plot zoomed in with just encounter 4
pal1 <- c("cyan4")
mill.travel %>% filter(encounter %in% c(4)) %>% 
ggplot(aes(x=t_index,y=num_calls,group=encounter,color=encounter)) + 
  geom_line(linewidth=1) +
  theme_classic() +
  geom_vline(xintercept=0, size=0.5,lty=2) +
  labs(x="Time", y="Count",fill="Call type") +
  scale_y_continuous(expand=c(0,0)) +
  xlim(-15,5) +
  ggtitle("Milling to traveling") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_color_manual(values=pal1) +
  facet_wrap(~call_type)

#line plot zoomed out with both encounters
ggplot(data=mill.travel,aes(x=t_index,y=num_calls,group=encounter,color=encounter)) + 
  geom_line(linewidth=1) +
  theme_classic() +
  geom_vline(xintercept=0, size=0.5,lty=2) +
  labs(x="Time", y="Count",fill="Call type") +
  scale_y_continuous(expand=c(0,0)) +
  #xlim(-15,5) +
  ggtitle("Milling to traveling") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_color_manual(values=pnw_palette("Bay",n=2)) +
  facet_wrap(~call_type)




#################### TRAVELING TO MILLING

#read in data, remove call types with no pattern and change remaining call types from wider to longer format
travel.mill <- read_csv("travel.mill_type.csv") %>% 
  dplyr::select(-c.10,-c.12,-c.13,-c.9,-modws.m,-nws,-nws.seq,-pulse.a,-pulse.flat.seg.2,-pulse.mod.bc,
                -pulse.mod.seg,-pulse.n,-rws,-trill) %>% #remove calls types that don't occur
  pivot_longer(cols = c("aws.seg","pulse.flat","flatws","aws","dws","pulse.flat.seg","pulse.d","modws",
                        "uws","modws.seg","flatws.seg","pulse.mod"),
               names_to = "call_type", values_to = "num_calls") %>% 
  mutate(date = mdy(date),
         minute=as.factor(minute),
         encounter = as.factor(encounter),
         behavior = as.factor(behavior),
         behavior2 = as.factor(behavior2),
         transition = as.factor(transition),
         call_type=as.factor(call_type)) %>% 
  group_by(transition,encounter)


###Plots- traveling to milling 
#barplot by call type
# ggplot(data=travel.mill, aes(x=t_index,y=num_calls,fill=call_type)) + 
#   geom_bar(stat="identity") +
#   theme_classic() +
#   geom_vline(xintercept=0, size=0.5,lty=2) +
#   labs(x="Time", y="Count",fill="Call type") +
#   scale_y_continuous(expand=c(0,0)) +
#   xlim(-15,5) +
#   ggtitle("Traveling to milling") +
#   theme(plot.title=element_text(hjust=0.5)) +
#   scale_fill_manual(values=pnw_palette("Bay",n=14)) +
#   facet_wrap(~call_type)

#barplot by encounter
# ggplot(data=travel.mill, aes(x=t_index,y=num_calls,fill=encounter)) + 
#   geom_bar(stat="identity") +
#   theme_classic() +
#   geom_vline(xintercept=0, size=0.5,lty=2) +
#   labs(x="Time", y="Count",fill="Encounter") +
#   scale_y_continuous(expand=c(0,0)) +
#   xlim(-15,5) +
#   ggtitle("Traveling to milling") +
#   theme(plot.title=element_text(hjust=0.5)) +
#   scale_fill_manual(values=pnw_palette("Bay",n=3)) +
#   facet_wrap(~call_type)

#Line plot zoomed in
ggplot(data=travel.mill, aes(x=t_index,y=num_calls,group=encounter,color=encounter)) + 
  geom_line(linewidth=1) +
  theme_classic() +
  geom_vline(xintercept=0, size=0.5,lty=2) +
  labs(x="Time", y="Count",fill="Call type") +
  scale_y_continuous(expand=c(0,0)) +
  xlim(-15,5) +
  ggtitle("Traveling to milling") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_color_manual(values=pnw_palette("Bay",n=2)) +
  facet_wrap(~call_type)

#Line plot zoomed out
ggplot(data=travel.mill, aes(x=t_index,y=num_calls,group=encounter,color=encounter)) + 
  geom_line(linewidth=1) +
  theme_classic() +
  geom_vline(xintercept=0, size=0.5,lty=2) +
  labs(x="Time", y="Count",fill="Call type") +
  scale_y_continuous(expand=c(0,0)) +
  #xlim(-15,5) +
  ggtitle("Traveling to milling") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_color_manual(values=pnw_palette("Bay",n=2)) +
  facet_wrap(~call_type)




#################### Ridge plots
#New data format for ridge plots, data above has call counts per minute, ridge needs one call per row

### MILL TO TRAVEL
mill.travel_ridge <- read.csv("mill.travel_ridge.csv") %>% 
  mutate(date = mdy(date),
         time = hms(time),
         minute=as.factor(minute),
         encounter = as.factor(encounter),
         behavior = as.factor(behavior),
         call_type=as.factor(call_type),
         call_category=as.factor(call_category)) %>% 
  na.omit() %>%
  #keep call types n>2
  filter(call_type %in% c("aws","c.10","c.13","c.9","dws","flatws","flatws.seg","modws","modws.m",
                          "modws.seg","nws","nws.seq","pulse.a","pulse.d","pulse.flat","pulse.flat.seg",
                          "pulse.mod","pulse.mod.seg","pulse.n","rws","trill","uws"))
                      

#by call category
pal3 <- c("gold2","darkseagreen","cyan4")
ggplot(mill.travel_ridge, aes(x=t_index,y=reorder(call_type,desc(t_index)), fill=call_category)) +
  geom_density_ridges(scale=2,alpha=0.7) +
  theme_ridges(grid=F) +
  #theme(legend.position = "none") +
  geom_vline(xintercept=0, size=0.7,lty=2) +
  xlim(-15,5) +
  labs(x="Time", y="Call type") +
  ggtitle("Milling to traveling") +
  theme(text=element_text(family="serif", size=14)) +
  scale_fill_manual(values=pal)

#by encounter
pal1 <- c("cyan4")
ggplot(mill.travel_ridge, aes(x=t_index,y=reorder(call_type,desc(t_index)), fill=encounter)) +
  geom_density_ridges(stat="density_ridges",alpha=0.7) +
  theme_ridges(grid=F) +
  #theme(legend.position = "none") +
  geom_vline(xintercept=0, size=0.7,lty=2) +
  xlim(-15,5) +
  labs(x="Time", y="Call type") +
  ggtitle("Milling to traveling") +
  theme(text=element_text(family="serif", size=14)) +
  scale_fill_manual(values=pal1)

#by encounter zoomed out
ggplot(mill.travel_ridge, aes(x=t_index,y=reorder(call_type,desc(t_index)), fill=encounter)) +
  geom_density_ridges(scale=2,alpha=0.7) +
  theme_ridges(grid=F) +
  #theme(legend.position = "none") +
  geom_vline(xintercept=0, size=0.7,lty=2) +
  labs(x="Time", y="Call type") +
  ggtitle("Milling to traveling") +
  theme(text=element_text(family="serif", size=14)) +
  scale_fill_manual(values=pal1)

#ridgeline- doesn't remove call types with n=2 or 3
#requires height
# ggplot(mill.travel, aes(x=t_index,y=reorder(call_type,desc(t_index)),height=num_calls,fill=encounter)) +
#   geom_ridgeline(scale=0.5,alpha=0.7) +
#   theme_ridges(grid=F) +
#   geom_vline(xintercept=0, size=0.7,lty=2) +
#   xlim(-15,5) +
#   labs(x="Time", y="Call type") +
#   ggtitle("Milling to traveling") +
#   scale_fill_manual(values=pnw_palette("Bay",n=2))



### TRAVEL TO MILL
travel.mill_ridge <- read_csv("travel.mill_ridge.csv") %>% 
  mutate(time = hms(time),
         minute=as.factor(minute),
         encounter = as.factor(encounter),
         behavior = as.factor(behavior),
         call_type=as.factor(call_type),
         call_category=as.factor(call_category))  %>% 
  na.omit() %>% 
  group_by(encounter) %>% 
  #keep call types n>2
  filter(call_type %in% c("aws","aws.seg","dws","flatws","flatws.seg","modws",
                          "pulse.flat","pulse.flat.seg","pulse.mod"))


#by call category
pal3 <- c("gold2","darkseagreen","cyan4")
ggplot(travel.mill_ridge, aes(x=t_index,y=reorder(call_type,desc(t_index)), fill=call_category)) +
  geom_density_ridges(scale=2,alpha=0.7) +
  theme_ridges(grid=F) +
  geom_vline(xintercept=0, size=0.7,lty=2) +
  xlim(-15,5) +
  labs(x="Time", y="Call type") +
  ggtitle("Traveling to milling") +
  theme(text=element_text(family="serif", size=14)) +
  scale_fill_manual(values=pal3)

#by encounter
pal2 <- c("darkseagreen","gold2")
ggplot(travel.mill_ridge, aes(x=t_index,y=reorder(call_type,desc(t_index)), fill=encounter)) +
  geom_density_ridges(scale=2,alpha=0.7) +
  theme_ridges(grid=F) +
  geom_vline(xintercept=0, size=0.7,lty=2) +
  xlim(-15,5) +
  labs(x="Time", y="Call type") +
  ggtitle("Traveling to milling") +
  theme(text=element_text(family="serif", size=14)) +
  scale_fill_manual(values=pal2)

#by encounter zoomed out
pal2 <- c("darkseagreen","gold2")
ggplot(travel.mill_ridge, aes(x=t_index,y=reorder(call_type,desc(t_index)), fill=encounter)) +
  geom_density_ridges(scale=2,alpha=0.7) +
  theme_ridges(grid=F) +
  geom_vline(xintercept=0, size=0.7,lty=2) +
  labs(x="Time", y="Call type") +
  ggtitle("Traveling to milling") +
  theme(text=element_text(family="serif", size=14)) +
  scale_fill_manual(values=pal2)


#ridgeline- doesn't remove call types with n=2 or 3
#requires height
# ggplot(travel.mill, aes(x=t_index,y=reorder(call_type,desc(t_index)),height=num_calls,fill=encounter)) +
#   geom_ridgeline(scale=0.5,alpha=0.7) +
#   theme_ridges(grid=F) +
#   geom_vline(xintercept=0, size=0.7,lty=2) +
#   xlim(-15,5) +
#   labs(x="Time", y="Call type") +
#   ggtitle("Traveling to milling") +
#   scale_fill_manual(values=pnw_palette("Bay",n=2))






####################### call rate ~ time_to_transition models
library(lme4)
library(performance)
library(DHARMa)

#####data with call rates for each call type independently
# data <- read_csv("mill.travel_type.csv") %>% 
#   mutate(encounter=as.factor(encounter))
# 
# test <-glmer(aws ~ t_index + (1|encounter), family=poisson(link="log"), data=data)




####call rates total

##mill to travel
#histogram
ggplot(data=mill.travel, aes(x=num_calls)) +
  geom_histogram(bins=50,fill="cyan4",color="grey",alpha=0.9) +
  theme_classic() +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,70,by=10)) +
  labs(x="Total call rate (#calls/minute)",y="Count") 

#poisson model
m.t_model <- glmer(num_calls ~ t_index + (1|encounter), family=poisson(link="log"), data=mill.travel)

summary(m.t_model)
check_overdispersion(m.t_model)
check_zeroinflation(m.t_model)

#negative binomial model
m.t_nb <-glmer.nb(num_calls ~ t_index + (1|encounter), data=mill.travel)

summary(m.t_nb)
check_overdispersion(m.t_nb)
check_zeroinflation(m.t_nb)

#check residuals
simulateResiduals(fittedModel = m.t_nb, plot = T)


####travel to mill
#histogram
ggplot(data=travel.mill, aes(x=num_calls)) +
  geom_histogram(bins=50,fill="cyan4",color="grey",alpha=0.9) +
  theme_classic() +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,70,by=10)) +
  labs(x="Total call rate (#calls/minute)",y="Count") 

#poisson model
t.m_model <- glmer(num_calls ~ t_index + (1|encounter), family=poisson(link="log"), data=travel.mill)

summary(t.m_model)
check_overdispersion(t.m_model)
check_zeroinflation(t.m_model)

#negative binomial model
t.m_nb <-glmer.nb(num_calls ~ t_index + (1|encounter), data=travel.mill)

summary(t.m_nb)
check_overdispersion(t.m_nb)
check_zeroinflation(t.m_nb)

#check residuals
simulateResiduals(fittedModel = t.m_nb, plot = T)


