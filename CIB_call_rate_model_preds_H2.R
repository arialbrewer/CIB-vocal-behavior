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

#create log(group size) in the dataset 
callrate_total_trunc$lgroup_size <- log(callrate_total_trunc$group_size)

nb2<-glmmTMB(n_minute ~ behavior + calf_presence + lgroup_size + tide + (1|encounter),
             family=truncated_nbinom2, data=callrate_total_trunc)

summary(nb2)

#save random effect variance
var.obs <- VarCorr(nb2)$cond$encounter[1]
sigma.obs <- sqrt(var.obs)

#set up a new data frame with values we want for predictions 
#note you could choose either "Travel" or "Mill"/"yes" or "no" for the third and fourth options
newData1 <- as.data.frame(expand.grid(seq(1,53,1),c("Ebb","Flood"),c("Travel"),c("yes")))
colnames(newData1) <- c("group_size","tide","behavior","calf_presence")
newData1$dummy_tide <- rep(1,nrow(newData1))
newData1$dummy_tide[which(newData1$tide=="Ebb")] <- 0
colnames(newData1) <- c("group_size","tide","behavior","calf_presence","dummy_tide")

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
for(i in 1:boots){
  y.sim <- unlist(simulate(nb2))  #simulate new response data from model
  ymod <- update(nb2,y.sim ~ .)   #refit model with simulated response
  par <- summary(ymod)$coefficients$cond[,1]  #save coefficients to index
  yest[,i] <- exp(predict(ymod,newdata = newData1, type="link", re.form=NA) + rnorm(1,0,sigma.obs))  #store predictions and transform out of link space
  #yest[,i] <- exp(rnbinom(n=1, mu=(par[1] + par[4] * newData1[,6] + par[5] * newData1[,5] + rnorm(1,0,sigma.obs)), size=summary(ymod)$sigma))
}

#summarize 
for(i in 1:nrow(newData1)){
  summary[i,1] <- mean(yest[i,]) 
  summary[i,2] <- quantile(yest[i,],probs=0.025) 
  summary[i,3] <- quantile(yest[i,],probs=0.975)
  summary[i,4] <- sd(yest[i,])
  
}

#save as dataframe and rename columns
summary <- as.data.frame(summary)
colnames(summary) <- c("mean","conf.low","conf.high","sd")

#combine newData and summary
preds <- cbind(newData1,summary)


#predictions plot in linear space
ggplot() +
  geom_line(data=preds, aes(x=group_size, y=mean, color=tide), linewidth = 1.75) +
  geom_ribbon(data=preds, aes(x=group_size, ymin=conf.low, ymax=conf.high, fill=tide), alpha = 0.1, color=NA) +
  geom_point(data=callrate_total_trunc,aes(x=group_size, y=n_minute, color=tide), position="jitter",alpha=0.4,size=2) +
  theme_classic() +
  labs(x="Group size", y="Predicted group calling rate (# calls/minute)") +
  theme(text=element_text(family="sans"),
        axis.text = element_text(size=22),
        axis.ticks.length = unit(0.4,"cm"),
        axis.line=element_line(colour='black', size=1)) +
  scale_color_manual(values=c("peachpuff3","darkslategray")) +
  scale_fill_manual(values=c("peachpuff3","darkslategray")) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,55,by=10))


#log-log space to check that both are straight lines
ggplot() +
  geom_line(data=preds, aes(x=lgroup_size, y=log(mean), color=tide), linewidth = 1.75) +
  #geom_ribbon(data=preds, aes(x=lgroup_size, ymin=log(conf.low), ymax=log(conf.high), fill=tide), alpha = 0.1, color=NA) +
  #geom_point(data=callrate_total_trunc,aes(x=group_size, y=n_minute, color=tide), position="jitter",alpha=0.4,size=2) +
  theme_classic() +
  labs(x="Group size", y="Predicted group calling rate (# calls/minute)") +
  theme(text=element_text(family="sans"),
        axis.text = element_text(size=22),
        axis.ticks.length = unit(0.4,"cm"),
        axis.line=element_line(colour='black', size=1)) +
  scale_color_manual(values=c("peachpuff3","darkslategray")) +
  scale_fill_manual(values=c("peachpuff3","darkslategray")) 


