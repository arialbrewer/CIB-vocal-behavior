###A.Brewer and S.Converse- CIB probability of calling model predictions (Hurdle model part 1)

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


###Model (just the binary portion)
#add new column to create 0 and 1
callrate_total$n_minute2 <- as.numeric(callrate_total$n_minute)

#change non-zeros to 1
callrate_total$n_minute2[callrate_total$n_minute2>0] <- 1

#Zero-inflated portion of hurdle model (0s and 1s)
model1<-glmmTMB(n_minute2 ~ behavior + calf_presence + group_size + tide + (1|encounter),
                family=binomial(link="logit"), data=callrate_total)

summary(model1)

#save random effect variance
var.obs <- VarCorr(model1)$cond$encounter[1]
sigma.obs <- sqrt(var.obs)


#set up a new data frame with values for predictions 
#note you could choose either "ebb" or "flood"/"yes" or "no" for the third and fourth options
#I'm choosing "ebb" and "no" because these coefficients then just drop out of the model predictions 
newData1 <- as.data.frame(expand.grid(seq(1,53,1),c("Mill","Travel"),c("Ebb"),c("no")))
colnames(newData1) <- c("group_size","behavior","tide","calf_presence")

###bootstrap to get uncertainty around predictions  
#store summary data 
summary <- matrix(NA,nrow = nrow(newData1), ncol=4)

#do at least 1000, possibly more (10000 if you can - not sure how long it will take)
boots <- 5

#store predictions 
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)

#simulate from the model (parametric bootstrap) and then rerun the model with the new data
for(i in 1:boots){
  y.sim <- simulate(model1)    #simulate new response data from model
  y.sim.ones <- y.sim[[1]][,1]    #only choose column for successes
  ymod <- update(model1,y.sim.ones ~ .)   #refit model with simulated response
  yest[,i] <- 1/(1+exp(-(predict(ymod,newdata = newData1, type="link", re.form=NA) + rnorm(1,0,sigma.obs))))  #store predictions and transform out of link space
}

#summarize 
for(i in 1:nrow(newData1)){
  summary[i,1] <- mean(yest[i,]) 
  summary[i,2] <- quantile(yest[i,],probs=0.025) 
  summary[i,3] <- quantile(yest[i,],probs=0.975)
  summary[i,4] <- sd(yest[i,])
}

#save output
summary <- as.data.frame(summary)
colnames(summary) <- c("mean","conf.low","conf.high","sd")

#combine newData and summary
preds <- cbind(newData1,summary)

#plot  
ggplot() +
  geom_line(data=preds, aes(x=group_size, y=mean, color=behavior), linewidth = 1.75) +
  geom_ribbon(data=preds, aes(x=group_size, ymin=conf.low, ymax=conf.high, fill=behavior), alpha = 0.1, color=NA) +
  geom_jitter(data=callrate_total,aes(x=group_size, y=n_minute2, color=behavior),height=0.001,alpha=0.05,size=2) +
  theme_classic() +
  labs(x="Group size", y="Predicted probability of calling") +
  theme(text=element_text(family="sans"),
        axis.text = element_text(size=24),
        axis.ticks.length = unit(0.4,"cm"),
        axis.line=element_line(colour='black', linewidth=1)) +
  scale_color_manual(values=c("sienna3","darkolivegreen")) +
  scale_fill_manual(values=c("sienna3","darkolivegreen")) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,55,by=10)) 

