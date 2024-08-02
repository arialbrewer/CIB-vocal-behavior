#HOBO vs NOAA tide plots

library(tidyverse)

tide <- read.csv("2022_combined tide data.csv") %>% 
  group_by(date) %>% 
  mutate(date = mdy(date),
        time = hm(time))



ggplot(data=tide) + 
  geom_line(aes(x=time,y=water_level_HOBO,group=date,color="red")) +
  geom_line(aes(x=time,y=water_level_NOAA,group=date,color="blue")) +
  theme_classic() +
  facet_wrap(~date) +
  labs(x="Time", y="Water level") +
  scale_x_continuous(expand=c(0,0),breaks=seq(0:00,23:00,by=2)) 

#OR

ggplot(data=tide%>% filter(date=="2022-08-08")) + 
  geom_line(aes(x=time,y=water_level_HOBO,group=date,color="red")) +
  geom_line(aes(x=time,y=water_level_NOAA,group=date,color="blue")) +
  theme_classic() +
  labs(x="Time", y="Water level") 





















