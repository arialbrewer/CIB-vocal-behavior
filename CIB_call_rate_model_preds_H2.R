###A.Brewer and S.Converse- CIB calling rate model predictions (Hurdle model part 2- truncated neg.bin.)

#load packages
library(tidyverse)
library(glmmTMB)

#load data
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

#combine behavioral & acoustic data
data_total <- behavior_data %>% 
  left_join(acoustic_data, by = c("date","time"), multiple = "all") 

#Bin data into calls per minute 
callrate_total <- data_total %>%
  mutate(num.calls = case_when(is.na(call_category)~0,TRUE~1)) %>% 
  group_by(date,time,encounter,tide,group_size,calf_presence,behavior) %>% 
  summarise(n_minute = sum(num.calls)) %>% 
  mutate(behavior = as.factor(behavior),
         calf_presence = as.factor(calf_presence),
         tide = as.factor(tide),
         encounter = as.factor(encounter))

#check levels of covariates 
levels(callrate_total$behavior) 
levels(callrate_total$calf_presence) 
levels(callrate_total$tide) 

#tide is switched around, set ebb as reference level
callrate_total$tide <- relevel(callrate_total$tide,ref = "Ebb")
levels(callrate_total$tide) 

#include only non-zero data
callrate_total_trunc <- callrate_total[which(callrate_total$n_minute>0),]

#create log group size in the dataset 
callrate_total_trunc$lgroup_size <- log(callrate_total_trunc$group_size)

nb2<-glmmTMB(n_minute ~ behavior + calf_presence + lgroup_size + tide + (1|encounter),
             family=truncated_nbinom2, data=callrate_total_trunc)

summary(nb2)

#save random effect variance
sigma.obs <- VarCorr(nb2)$cond$encounter[1]

#set up a new data frame with values we want for predictions 
#note you could choose either "Travel" or "Mill"/"yes" or "no" for the third and fourth options
#I'm choosing "Mill" and "no" because these coefficients then just drop out of the model predictions 
newData1 <- as.data.frame(expand.grid(seq(1,53,1),c("Ebb","Flood"),c("Mill"),c("no")))
colnames(newData1) <- c("group_size","tide","behavior","calf_presence")
#add in the log of group size 
newData1$lgroup_size <- log(newData1$group_size)

#bootstrap to get uncertainty around predictions  
#store summary data 
summary <- matrix(NA,nrow = nrow(newData1), ncol=4)

#do at least 1000, possibly more (10000 if you can - not sure how long it will take)
boots <- 100

#store predictions 
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)

#simulate from the model (parametric bootstrap) and then rerun the model with the new data
#type="link" for log space, type="response" for normal space
for(i in 1:boots){
  y.sim <- unlist(simulate(nb2))
  ymod <- update(nb2,y.sim ~ ., y.sim ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="link", re.form=NA) + rnorm(1,0,sigma.obs)
}

#summarize 
for(i in 1:nrow(newData1)){
  summary[i,1] <- mean(yest[i,]) 
  summary[i,2] <- quantile(yest[i,],probs=0.025) 
  summary[i,3] <- quantile(yest[i,],probs=0.975)
  summary[i,4] <- sd(yest[i,])
  
}


#sarah plot
#first group of 53 are with Flood tide, second group of 53 are with Ebb tide  
plot(newData1$group_size[1:53],summary[1:53,1],type="l",col="red",xlim=c(0,10),ylim=c(0,10))
lines(newData1$group_size[1:53],summary[1:53,2],lty="dashed",col="red")
lines(newData1$group_size[1:53],summary[1:53,3],lty="dashed",col="red")
lines(newData1$group_size[54:106],summary[54:106,1],type="l",col="blue")
lines(newData1$group_size[54:106],summary[54:106,2],lty="dashed",col="blue")
lines(newData1$group_size[54:106],summary[54:106,3],lty="dashed",col="blue")


#Arial made into ggplot
summary <- as.data.frame(summary)
colnames(summary) <- c("mean","conf.low","conf.high","sd")

#combine newData and summary
data <- cbind(newData1,summary)


#predictions on link  
ggplot() +
  geom_line(data=data, aes(x=exp(lgroup_size), y=mean, color=tide), linewidth = 1) +
  geom_ribbon(data=data, aes(x=exp(lgroup_size), ymin=conf.low, ymax=conf.high, fill=tide), alpha = 0.05, color=NA) +
  #geom_point(data=callrate_total_trunc,aes(x=group_size, y=n_minute, color=tide), position="jitter",alpha=0.3) +
  theme_classic() +
  labs(x="Group size", y="log(Predicted calling rate)") +
  theme(text=element_text(family="sans", size=16),
        axis.text = element_text(size=16),
        axis.ticks.length = unit(0.4,"cm")) +
  scale_color_manual(values=c("darkgoldenrod","darkcyan")) +
  scale_fill_manual(values=c("darkgoldenrod","darkcyan")) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,6,by=1)) +
  scale_x_continuous(expand=c(0,0))


#predictions exponentiated out of log space 
ggplot() +
  geom_line(data=data, aes(x=exp(lgroup_size), y=exp(mean), color=tide), linewidth = 1) +
  geom_ribbon(data=data, aes(x=exp(lgroup_size), ymin=exp(conf.low), ymax=exp(conf.high), fill=tide), alpha = 0.05, color=NA) +
  #geom_point(data=callrate_total_trunc,aes(x=group_size, y=n_minute, color=tide), position="jitter",alpha=0.3) +
  theme_classic() +
  labs(x="Group size", y="Predicted calling rate") +
  theme(text=element_text(family="sans", size=16),
        axis.text = element_text(size=16),
        axis.ticks.length = unit(0.4,"cm")) +
  scale_color_manual(values=c("darkgoldenrod","darkcyan")) +
  scale_fill_manual(values=c("darkgoldenrod","darkcyan")) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,300,by=50)) +
  scale_x_continuous(expand=c(0,0))


#log-log space to check that both are straight lines
ggplot() +
  geom_line(data=data, aes(x=lgroup_size, y=mean, color=tide), linewidth = 1) +
  geom_ribbon(data=data, aes(x=lgroup_size, ymin=conf.low, ymax=conf.high, fill=tide), alpha = 0.05, color=NA) +
  theme_classic() +
  labs(x="log(Group size)", y="log(Predicted calling rate)") +
  theme(text=element_text(family="sans", size=16),
        axis.text = element_text(size=16),
        axis.ticks.length = unit(0.4,"cm")) +
  scale_color_manual(values=c("darkgoldenrod","darkcyan")) +
  scale_fill_manual(values=c("darkgoldenrod","darkcyan")) +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0))




