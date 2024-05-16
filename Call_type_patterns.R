#EXPLORE CALL TYPE FREQUENCY DURING ENCOUNTER

#load packages
library(tidyverse)

#read in data
setwd("C:/Users/Arial/OneDrive - UW/Desktop/Ch.2 vocal behavior/CIB vocal behavior code/")

acoustic_files <- list.files(pattern = "acoustic.csv")
behavior_files <- list.files(pattern = "behavior.csv")

acoustic_data <- acoustic_files %>%  
  map(read_csv) %>% 
  reduce(rbind) %>% 
  dplyr::select(-begin_Time,-end_Time,-low_Freq,-high_Freq,-selection)

behavior_data <- behavior_files %>%  
  map(read_csv) %>% 
  reduce(rbind) %>% 
  dplyr::select(-sample_round,-group_number,-dot,-count_white,-count_gray,-count_calf,-comments)

#combine behavioral & acoustic data and remove NAs (zeros don't matter)
calltype_total <- behavior_data %>% 
  left_join(acoustic_data, by = c("date","time"), multiple = "all") %>% 
  na.omit()

#write_csv(calltype_total,"C:/Users/Arial/OneDrive - UW/Desktop/CH.2 Vocal behavior/CIB vocal behavior code/calltype_total.csv")

table(calltype_total$call_type)


####read in individual encounters
enc1_2021 <- read_csv("2021enc_1.csv")
enc3_2021 <- read_csv("2021enc_3.csv")
enc4_2021 <- read_csv("2021enc_4.csv")
enc5_2021 <- read_csv("2021enc_5.csv")
enc6_2021 <- read_csv("2021enc_6.csv")
enc7_2021 <- read_csv("2021enc_7.csv")
enc8_2021 <- read_csv("2021enc_8.csv")

###plots
#behavior
ggplot(enc3_2021, aes(x=time, y= ))+
  geom_line(stat="count", aes(color=call_type), size=1)+
  geom_ribbon(aes(ymin=15.9,ymax=16,fill=behavior))+
  theme_classic()+
  labs(x="Time",y="Count")+
  scale_y_continuous(breaks=seq(1,30,by=1))

#calf presence
ggplot(enc5_2021, aes(x=time, y= ))+
  geom_line(stat="count", aes(color=call_type), size=1)+
  geom_ribbon(aes(ymin=15.9,ymax=16,fill=calf_presence))+
  theme_classic()+
  labs(x="Time",y="Count")+
  scale_y_continuous(breaks=seq(1,30,by=1))

#tide
ggplot(enc8_2021, aes(x=time, y= ))+
  geom_line(stat="count", aes(color=call_type), size=1)+
  geom_ribbon(aes(ymin=15.9,ymax=16,fill=tide))+
  theme_classic()+
  labs(x="Time",y="Count")+
  scale_y_continuous(breaks=seq(1,30,by=1))



  facet_wrap(~call_type, ncol = 4))


