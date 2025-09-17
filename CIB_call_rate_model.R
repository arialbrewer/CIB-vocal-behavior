#Arial Brewer
#PhD Chapter 2: CIB Vocal Behavior- calling rate model

#load packages
library(tidyverse)
library(corrplot)
library(performance)
library(parameters)
library(glmmTMB)
library(lme4)
library(viridis)

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

  
####Test for correlation between variables
##create duplicate data for binary variables
callrate_total2 <- data_total %>%
  mutate(num.calls = case_when(is.na(call_category)~0,TRUE~1)) %>% 
  group_by(date,time,encounter,tide,group_size,calf_presence,behavior) %>% 
  summarise(n_minute = sum(num.calls))

##change categorical variables to binary
#Mill=0, Travel=1
callrate_total2$behavior <- ifelse(callrate_total2$behavior=="Travel",1,0)

#no=0, yes=1
callrate_total2$calf_presence <- ifelse(callrate_total2$calf_presence=="yes",1,0)
  
#Ebb=0, Flood=1
callrate_total2$tide <- ifelse(callrate_total2$tide=="Flood",1,0)

#correlation matrix
x <- cor(callrate_total2[3:7])
corrplot(x, type="upper",order="hclust",addCoef.col = "black")


#######################################Exploratory plots####################

####Exploratory plots
behavior_type <- callrate_total %>% 
  group_by(behavior) %>% 
  summarise(number=n()) %>% 
  mutate(perc=number/sum(number))%>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

calf_type <- callrate_total %>% 
  group_by(calf_presence) %>% 
  summarise(number=n()) %>% 
  mutate(perc=number/sum(number))%>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

tide_type <- callrate_total %>% 
  group_by(tide) %>% 
  summarise(number=n()) %>% 
  mutate(perc=number/sum(number))%>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

###independent variables
#behavior distribution
ggplot(data=behavior_type, aes(x="", y=number,fill=behavior)) +
  geom_bar(stat='identity',width=1, color='white') +
  coord_polar("y",start=0) +
  theme_void() + 
  geom_label(aes(label = labels), 
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  scale_fill_manual(values=c("gold2","cyan4"))

#calf distribution
ggplot(data=calf_type, aes(x="", y=number,fill=calf_presence)) +
  geom_bar(stat='identity',width=1, color='white') +
  coord_polar("y",start=0) +
  theme_void() + 
  geom_label(aes(label = labels), 
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  scale_fill_manual(values=c("gold2","cyan4"))

#tide distribution
ggplot(data=tide_type, aes(x="", y=number,fill=tide)) +
  geom_bar(stat='identity',width=1, color='white')+
  coord_polar("y",start=0)+
  theme_void() + 
  geom_label(aes(label = labels), 
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  scale_fill_manual(values=c("gold2","cyan4"))

#group size
ggplot(data=callrate_total, aes(x=group_size)) +
  geom_histogram(bins=50,fill="cyan4",color="grey",alpha=0.9) +
  theme_classic() +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,400,by=50)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,60,by=5)) +
  labs(x="Group size",y="Count") 

###dependent variable
#Call rate
ggplot(data=callrate_total, aes(x=n_minute)) +
  geom_histogram(bins=50,fill="cyan4",color="grey",alpha=0.9) +
  theme_classic() +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,70,by=10)) +
  labs(x="Total call rate (#calls/minute)",y="Count") 


###### Group size vs calling rate (#calls/minute) fit with line
ggplot(callrate_total, aes(x=group_size, y=n_minute)) +
  geom_point(alpha=0.2, size=3) +
  theme_classic() +
  geom_smooth(method="glm") +
  labs(x="Group size",y="Calling rate (# calls/minute)") +
  scale_y_continuous(breaks=seq(0,60,by=10)) +
  scale_x_continuous(breaks=seq(0,60,by=5)) 


##### Group size vs calling rate by variable
#behavior
ggplot(callrate_total, aes(x=group_size, y=n_minute, color=behavior)) +
  geom_point(position="jitter",alpha=0.5,size=2) +
  theme_classic() +
  scale_color_viridis(discrete=T,begin=0.3,end=0.8) +
  labs(x="Group size",y="Calling rate (# calls/minute)") +
  scale_y_continuous(breaks=seq(0,60,by=10)) +
  scale_x_continuous(breaks=seq(0,60,by=5)) 

#calf presence
ggplot(callrate_total, aes(x=group_size, y=n_minute, color=calf_presence)) +
  geom_point(position="jitter",alpha=0.5,size=2) +
  theme_classic() +
  scale_color_viridis(discrete=T,begin=0.3,end=0.8) +
  labs(x="Group size",y="Calling rate (# calls/minute)") +
  scale_y_continuous(breaks=seq(0,60,by=10)) +
  scale_x_continuous(breaks=seq(0,60,by=5))

#tide
ggplot(callrate_total, aes(x=group_size, y=n_minute, color=tide)) +
  geom_point(position="jitter",alpha=0.5,size=2) +
  theme_classic() +
  scale_color_viridis(discrete=T,begin=0.3,end=0.8) +
  labs(x="Group size",y="Calling rate (# calls/minute)") +
  scale_y_continuous(breaks=seq(0,60,by=10)) +
  scale_x_continuous(breaks=seq(0,60,by=5))

###explore mean pattern
groupsize_mean <- callrate_total %>% 
  group_by(group_size) %>% 
  summarise(mean_callrate=mean(n_minute),
            sd_callrate=sd(n_minute))

#line fit with glm
ggplot(groupsize_mean, aes(x=group_size, y=mean_callrate))+
  geom_point() +
  theme_classic() +
  geom_smooth(method="glm") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x="Group size", y="Mean calling rate")

#how many zeros are present vs. non-zeros- data very zero-inflated
table(callrate_total$n_minute)
sum(callrate_total$n_minute>0)
sum(callrate_total$n_minute<1)

#data likely overdispersed
mean(callrate_total$n_minute)
var(callrate_total$n_minute)

##########################################################



################################### Model building
#check covariate levels
levels(callrate_total$behavior)
levels(callrate_total$calf_presence)
levels(callrate_total$tide) 

#tide is switched around from call category model, set ebb as reference level
callrate_total$tide <- relevel(callrate_total$tide,ref = "Ebb")
levels(callrate_total$tide) 

###Poisson model
pois<-glmmTMB(n_minute ~ behavior + group_size + calf_presence + tide + (1|encounter),
              family=poisson, data=callrate_total)
summary(pois)

#Goodness of fit test to test for overdispersion. Null hypothesis= model is correctly specified
X2 <- sum((callrate_total$n_minute - fitted(pois))^2 / fitted(pois))
df <- length(callrate_total$n_minute)-length(coef(pois))
pchisq(X2, df,lower.tail = FALSE)
#reject null- model not a good fit


###Negative binomial model to account for overdispersion
nb<-glmmTMB(n_minute ~ behavior + group_size + calf_presence + tide + (1|encounter),
            family=nbinom2, data=callrate_total)
summary(nb)

#Another Goodness of fit test to see if zero-inflation is occurring
X2 <- sum((callrate_total$n_minute - fitted(nb))^2 / fitted(nb))
df <- length(callrate_total$n_minute)-length(coef(nb))
pchisq(X2, df,lower.tail = FALSE)
#reject null- model still not a good fit- we still have zero-inflation


###Negative Binomial Hurdle Model to account for zero-inflation & overdispersion
#glmmTMB sets reference to 1 so we model the probability of not producing a call 
#vs producing a call- this is inverse of what I want- will change in code farther down

#Using log(group size) so we can model group size as a power function instead of exponential 
#to capture sublinear, linear, or superlinear scaling 
hur.mod<-glmmTMB(n_minute ~ behavior + calf_presence + log(group_size) + tide + (1|encounter),
             ziformula= ~ behavior + calf_presence + group_size + tide + (1|encounter),
             family=truncated_nbinom2, data=callrate_total)

summary(hur.mod)
plot(parameters(hur.mod))
ranef(hur.mod)

#calculate 95% CI
confint(hur.mod)

###plot coefficients and CI
#conditional (second part of hurdle- truncated negative binomial)
hurdle2 <- data.frame(variable=c("Behavior","Calf presence","Group size","Tide"),
                      coefficient=c(0.043,-0.537,0.699,1.412),
                      lower=c(-0.400,-1.609,0.444,0.499),
                      upper=c(0.486,0.535,0.955,2.325),
                      sig=c("no","no","yes","yes")) %>% 
  mutate(variable=as.factor(variable))

#couldn't get behavior to be first so reversed order and will manually change level labels
ggplot(data=hurdle2,aes(x=coefficient, y=rev(variable), color=sig)) +
  geom_point(size=4) +
  geom_pointrange(aes(xmin=lower,xmax=upper),lwd=1) +
  geom_vline(xintercept=0,lty=2,lwd=1) +
  theme_classic() +
  scale_x_continuous(breaks=seq(-4,4,by=1)) +
  labs(x="Coefficient", y=" Variable", color="Significant") +
  theme(text=element_text(family="sans"),
        axis.text = element_text(size=24),
        axis.line=element_line(colour='black', size=1)) +
  scale_color_manual(values=c("red3","deepskyblue4"))


#####calculating changes in variables with effect on calling rate
##conditional model (part two of hurdle)

#tide (flood)
exp(1.41173)

#calculate SE of tide beta using model output
#SE(exp(beta)) = (SE(beta)^2 * exp(beta)^2)^(1/2) 
summary(hur.mod)
((0.46573)^2 * exp(1.41173)^2)^(1/2) 


### Model diagnostics
#examining residuals    
E <- residuals(hur.mod)

#group size
callrate_total$rate_group_size <- cut(callrate_total$group_size, seq(0, 60, by=10))
plot(callrate_total$rate_group_size, E, xlab="Group size",ylab="Residuals")

#behavior
plot(callrate_total$behavior, E, xlab="Behavior", ylab="Residuals")

#calf presence
plot(callrate_total$calf_presence, E, xlab="Calf presence", ylab="Residuals")

#tide
plot(callrate_total$tide, E, xlab="Tide", ylab="Residuals")



########## Isolating hurdle part 1 to test which is reference (0 or 1)
#add new column to create 0 and 1
callrate_total$n_minute2 <- as.numeric(callrate_total$n_minute)

#change non-zeros to 1
callrate_total$n_minute2[callrate_total$n_minute2>0] <- 1

#double check this worked and matches counts from original n_minute column
sum(callrate_total$n_minute2>0)
sum(callrate_total$n_minute2<1)

#test model with just a binomial for first part of hurdle
zi.hur<-glmmTMB(n_minute2 ~ behavior + calf_presence + group_size + tide + (1|encounter),
                 family=binomial(link="logit"), data=callrate_total)

summary(zi.hur)
ranef(zi.hur)
plot(parameters(zi.hur))

#this is the opposite of what we see in glmmTMB hurdle. So for the hurdle model,
#it is modeling the probability that they are NOT calling (0) with a reference of calling (1)
#will use this version so I can model the probability of calling (1) vs. not calling (0)

#calculate 95% CI
confint(zi.hur)

###plot coefficients and CI
#zero-inflation (first part of hurdle- binomial)
zi.hur.data <- data.frame(variable=c("Behavior","Calf presence","Group size","Tide"),
                      coefficient=c(0.804,0.768,0.088,-0.157),
                      lower=c(0.313,-0.048,0.053,-2.314),
                      upper=c(1.294,1.583,0.123,2.001),
                      sig=c("yes","no","yes","no")) %>% 
  mutate(variable=as.factor(variable))

#couldn't get behavior to be first so reversed order and will manually change level labels
ggplot(data=zi.hur.data,aes(x=coefficient, y=rev(variable), color=sig)) +
  geom_point(size=4) +
  geom_pointrange(aes(xmin=lower,xmax=upper),lwd=1) +
  geom_vline(xintercept=0,lty=2,lwd=1) +
  theme_classic() +
  scale_x_continuous(breaks=seq(-4,4,by=1)) +
  labs(x="Coefficient", y=" Variable", color="Significant") +
  theme(text=element_text(family="sans"),
        axis.text = element_text(size=24),
        axis.line=element_line(colour='black', size=1)) +
  scale_color_manual(values=c("red3","deepskyblue4"))


#####calculating odds (percentage) from coefficients- [(exp(coef)-1)*100]
### ZI model (part one of hurdle)
#behavior (travel)
(exp(0.804)-1)*100

#calf presence (yes)
(exp(0.768)-1)*100

#group size
(exp(0.088)-1)*100

#tide (flood)
(exp(-0.157)-1)*100



############################## Predictions 
#####Hurdle part 1- binary 0s and 1s
summary(zi.hur)

#save random effect variance
sigma.obs1 <- sqrt(VarCorr(zi.hur)$cond$encounter[1])

#set up new data frame with values for predictions 
newData1 <- as.data.frame(expand.grid(seq(1,53,1),c("Mill","Travel"),c("Ebb"),c("yes")))
colnames(newData1) <- c("group_size","behavior","tide","calf_presence")

#store summary data 
summary1 <- matrix(NA,nrow = nrow(newData1), ncol=4)

#bootstrap to get uncertainty around predictions  
boots <- 1000

#store predictions 
yest1 <- matrix(NA,nrow=nrow(newData1),ncol=boots)

#simulate from the model (parametric bootstrap) and then rerun the model with the new data
for(i in 1:boots){
  y.sim <- simulate(zi.hur)    #simulate new response data from model
  y.sim.ones <- y.sim[[1]][,1]    #only choose column for successes
  ymod1 <- update(zi.hur,y.sim.ones ~ .)   #refit model with simulated response
  yest1[,i] <- 1/(1+exp(-(predict(ymod1,newdata = newData1, type="link", re.form=NA) + rnorm(1,0,sigma.obs1))))  #store predictions and transform out of link space
}

#summarize 
for(i in 1:nrow(newData1)){
  summary1[i,1] <- mean(yest1[i,]) 
  summary1[i,2] <- quantile(yest1[i,],probs=0.025) 
  summary1[i,3] <- quantile(yest1[i,],probs=0.975)
  summary1[i,4] <- sd(yest1[i,])
}

#save output
summary1 <- as.data.frame(summary1)
colnames(summary1) <- c("mean1","conf.low1","conf.high1","sd1")

#combine newData and summary
preds1 <- cbind(newData1,summary1)

#plot  
ggplot() +
  geom_line(data=preds1, aes(x=group_size, y=mean1, color=behavior), linewidth = 1.75) +
  geom_ribbon(data=preds1, aes(x=group_size, ymin=conf.low1, ymax=conf.high1, fill=behavior), alpha = 0.1, color=NA) +
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


###Individual level predictions
#function to calculate p (probability of individual calling)
back.p1 <- function(mean,group_size){
  p <- 1-(1-mean)^(1/group_size)
  return(p)
}

#add column for back.p (probability at individual level)
preds1$back.p <- back.p1(preds1$mean,preds1$group_size)
preds1$back.p.low <- back.p1(preds1$conf.low,preds1$group_size)
preds1$back.p.high <- back.p1(preds1$conf.high,preds1$group_size)

#update so predictions can't go above plot limit
preds1$back.p.high <- ifelse(preds1$back.p.high>0.5,0.5,preds1$back.p.high)

#calculate prob of calling for raw data and divide by group size
back.p.raw <- function(n_minute2,group_size){
  p <- 1-(1-n_minute2)^(1/group_size)
  return(p)
}

#new dataframe with counts of 0s and 1s at each group size
df.raw <- callrate_total %>% 
  select(group_size,n_minute2,behavior) %>% 
  group_by(behavior) %>% 
  count(group_size,n_minute2) %>% 
  mutate(n_minute2=ifelse(n_minute2 == 0, "no","yes")) %>% 
  tidyr::spread(n_minute2, n) %>% 
  replace(is.na(.),0)

#add column that is 0s + 1s to be divided by 
df.raw$total <- df.raw$no + df.raw$yes
df.raw$P.star <- df.raw$yes/df.raw$total

back.p.raw <- function(P.star,group_size){
  p <- 1-(1-P.star)^(1/group_size)
  return(p)
}

#calculate p with back.p equation
df.raw$back.p <- back.p.raw(df.raw$P.star,df.raw$group_size)

#plot
ggplot() +
  geom_line(data=preds1, aes(x=group_size, y=back.p, color=behavior), linewidth = 1.75) +
  geom_ribbon(data=preds1, aes(x=group_size, ymin=back.p.low, ymax=back.p.high, fill=behavior), alpha = 0.1, color=NA) +
  geom_point(data=df.raw,aes(x=group_size, y=back.p, color=behavior),alpha=0.7,size=2) +
  theme_classic() +
  labs(x="Group size", y="Probability of calling") +
  theme(text=element_text(family="sans"),
        axis.text = element_text(size=24),
        axis.ticks.length = unit(0.4,"cm"),
        axis.line=element_line(colour='black', size=1)) +
  scale_color_manual(values=c("sienna3","darkolivegreen")) +
  scale_fill_manual(values=c("sienna3","darkolivegreen")) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,55,by=10))




#####Hurdle part 2- non-zero count data
#include only non-zero data
callrate_total_trunc <- callrate_total[which(callrate_total$n_minute>0),]

#create log(group size) in the dataset 
callrate_total_trunc$lgroup_size <- log(callrate_total_trunc$group_size)

#run model just for this data
nb.hur<-glmmTMB(n_minute ~ behavior + calf_presence + lgroup_size + tide + (1|encounter),
             family=truncated_nbinom2, data=callrate_total_trunc)

summary(nb.hur)

#save random effect variance
sigma.obs2 <- sqrt(VarCorr(nb.hur)$cond$encounter[1])

#set up a new data frame with values we want for predictions 
newData2 <- as.data.frame(expand.grid(seq(1,53,1),c("Ebb","Flood"),c("Travel"),c("yes")))
colnames(newData2) <- c("group_size","tide","behavior","calf_presence")
#newData2$dummy_tide <- rep(1,nrow(newData2))
#newData2$dummy_tide[which(newData2$tide=="Ebb")] <- 0
#colnames(newData2) <- c("group_size","tide","behavior","calf_presence","dummy_tide")
#add in the log of group size 
newData2$lgroup_size <- log(newData2$group_size)

#store summary data 
summary2 <- matrix(NA,nrow = nrow(newData2), ncol=4)

#bootstrap to get uncertainty around predictions  
boots <- 1000

#store predictions 
yest2 <- matrix(NA,nrow=nrow(newData2),ncol=boots)

#simulate from the model (parametric bootstrap) and then rerun the model with the new data
for(i in 1:boots){
  y.sim <- unlist(simulate(nb.hur))  #simulate new response data from model
  ymod2 <- update(nb.hur,y.sim ~ .)   #refit model with simulated response
  par <- summary(ymod2)$coefficients$cond[,1]  #save coefficients to index
  yest2[,i] <- exp(predict(ymod2,newdata = newData2, type="link", re.form=NA) + rnorm(1,0,sigma.obs2))  #store predictions and transform out of link space
  #yest2[,i] <- exp(rnbinom(n=1, mu=(par[1] + par[4] * newData2[,6] + par[5] * newData2[,5] + rnorm(1,0,sigma.obs2)), size=summary(ymod2)$sigma))
}

#summarize 
for(i in 1:nrow(newData2)){
  summary2[i,1] <- mean(yest2[i,]) 
  summary2[i,2] <- quantile(yest2[i,],probs=0.025) 
  summary2[i,3] <- quantile(yest2[i,],probs=0.975)
  summary2[i,4] <- sd(yest2[i,])
}

#save as dataframe and rename columns
summary2 <- as.data.frame(summary2)
colnames(summary2) <- c("mean2","conf.low2","conf.high2","sd2")

#combine newData and summary
preds2 <- cbind(newData2,summary2)

#predictions plot in linear space
ggplot() +
  geom_line(data=preds2, aes(x=group_size, y=mean2, color=tide), linewidth = 1.75) +
  geom_ribbon(data=preds2, aes(x=group_size, ymin=conf.low2, ymax=conf.high2, fill=tide), alpha = 0.1, color=NA) +
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


# #log-log scale to check that both are linear
# ggplot() +
#   geom_line(data=preds2, aes(x=lgroup_size, y=log(mean2), color=tide), linewidth = 1.75) +
#   theme_classic() +
#   scale_color_manual(values=c("peachpuff3","darkslategray")) +
#   scale_fill_manual(values=c("peachpuff3","darkslategray")) +
#   scale_x_continuous(expand=c(0,0)) +
#   labs(x="log(group size)", y="log(coefficient)")
# 
# #check quotients
# quotient <- data.frame(preds2[54:106,6]/preds2[1:53,6])
# colnames(quotient) <- c("quotient")
# quotient <- quotient %>% 
#   mutate(group_size=c(1:53))
# 
# ggplot() +
#   geom_line(data=quotient, aes(x=group_size, y=quotient), linewidth = 1.75) +
#   theme_classic() +
#   labs(x="Group size", y="Tide quotient")+
#   scale_x_continuous(expand=c(0,0),breaks=seq(0,55,by=10)) +
#   ylim(0,8)


####individual level predictions
#calculate individual call rate and low and high conf. 
preds2$ind.call <- preds2$mean2/preds2$group_size
preds2$ind.call.low <- preds2$conf.low2/preds2$group_size
preds2$ind.call.high <- preds2$conf.high2/preds2$group_size
callrate_total_trunc$raw.N <- callrate_total_trunc$n_minute/callrate_total_trunc$group_size

#update so predictions can't go above plot limit
preds2$ind.call.high  <- ifelse(preds2$ind.call.high >10,8.5,preds2$ind.call.high)

#plot
ggplot() +
  geom_line(data=preds2, aes(x=group_size, y=ind.call, color=tide), linewidth = 1.75) +
  geom_ribbon(data=preds2, aes(x=group_size, ymin=ind.call.low, ymax=ind.call.high, fill=tide), alpha = 0.1, color=NA) +
  geom_point(data=callrate_total_trunc,aes(x=group_size, y=raw.N, color=tide), position="jitter",alpha=0.4,size=2) +
  theme_classic() +
  labs(x="Group size", y="Predicted calling rate (# calls/minute)") +
  theme(text=element_text(family="sans"),
        axis.text = element_text(size=24),
        axis.ticks.length = unit(0.4,"cm"),
        axis.line=element_line(colour='black', size=1)) +
  scale_color_manual(values=c("peachpuff3","darkslategray")) +
  scale_fill_manual(values=c("peachpuff3","darkslategray")) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,55,by=10)) +
  scale_y_continuous(breaks=seq(0,8,by=2)) 




#####Full Hurdle model
###Hurdle part 1
#set up new data frame with values for predictions, these must include all variables
#in both parts of hurdle model (group size, behavior, tide. Calf set to yes)
newData_h1 <- as.data.frame(expand.grid(seq(1,53,1),c("Mill","Travel"),c("Ebb","Flood"),c("yes")))
colnames(newData_h1) <- c("group_size","behavior","tide","calf_presence")

#store summary data 
summary_h1 <- matrix(NA,nrow = nrow(newData_h1), ncol=4)

#bootstrap to get uncertainty around predictions  
boots <- 1000

#store predictions 
yest_h1 <- matrix(NA,nrow=nrow(newData_h1),ncol=boots)

#simulate from the model (parametric bootstrap) and then rerun the model with the new data
for(i in 1:boots){
  y.sim <- simulate(zi.hur)    #simulate new response data from model
  y.sim.ones <- y.sim[[1]][,1]    #only choose column for successes
  ymod <- update(zi.hur,y.sim.ones ~ .)   #refit model with simulated response
  yest_h1[,i] <- 1/(1+exp(-(predict(ymod,newdata = newData_h1, type="link", re.form=NA) + rnorm(1,0,sigma.obs1))))  #store predictions and transform out of link space
}

#summarize 
for(i in 1:nrow(newData_h1)){
  summary_h1[i,1] <- mean(yest_h1[i,]) 
  summary_h1[i,2] <- quantile(yest_h1[i,],probs=0.025) 
  summary_h1[i,3] <- quantile(yest_h1[i,],probs=0.975)
  summary_h1[i,4] <- sd(yest_h1[i,])
}

#save output
summary_h1 <- as.data.frame(summary_h1)
colnames(summary_h1) <- c("mean1","conf.low1","conf.high1","sd1")

#combine newData and summary
preds_h1 <- cbind(newData_h1,summary_h1)


###Hurdle part 2
#set up a new data frame with values we want for predictions 
newData_h2 <- as.data.frame(expand.grid(seq(1,53,1),c("Mill","Travel"),c("Ebb","Flood"),c("yes")))
colnames(newData_h2) <- c("group_size","behavior","tide","calf_presence")

#add in the log of group size 
newData_h2$lgroup_size <- log(newData_h2$group_size)

#store summary data 
summary_h2 <- matrix(NA,nrow = nrow(newData_h2), ncol=4)

#bootstrap to get uncertainty around predictions  
boots <- 1000

#store predictions 
yest_h2 <- matrix(NA,nrow=nrow(newData_h2),ncol=boots)

#simulate from the model (parametric bootstrap) and then rerun the model with the new data
for(i in 1:boots){
  y.sim <- unlist(simulate(nb.hur))  #simulate new response data from model
  ymod <- update(nb.hur,y.sim ~ .)   #refit model with simulated response
  par <- summary(ymod)$coefficients$cond[,1]  #save coefficients to index
  yest_h2[,i] <- exp(predict(ymod,newdata = newData_h2, type="link", re.form=NA) + rnorm(1,0,sigma.obs2))  #store predictions and transform out of link space
}

#summarize 
for(i in 1:nrow(newData_h2)){
  summary_h2[i,1] <- mean(yest_h2[i,]) 
  summary_h2[i,2] <- quantile(yest_h2[i,],probs=0.025) 
  summary_h2[i,3] <- quantile(yest_h2[i,],probs=0.975)
  summary_h2[i,4] <- sd(yest_h2[i,])
}

#save as dataframe and rename columns
summary_h2 <- as.data.frame(summary_h2)
colnames(summary_h2) <- c("mean2","conf.low2","conf.high2","sd2")

#remove lgroup_size column so can combine
newData_h2 <- newData_h2 %>% 
  select(-lgroup_size)

#combine newData and summary
preds_h2 <- cbind(newData_h2,summary_h2)

#create new column in preds1 that combines behavior and tide to create condition and removes columns not in use
# preds_h1 <- preds_h1 %>% 
#   mutate(combined=paste(behavior,tide,sep="-")) %>% 
#   mutate(combined = as.factor(combined)) %>% 
#   select(-behavior,-tide,-calf_presence,-sd1) 

#create new column in preds2 that combines behavior and tide to create condition and removes columns not in use
# preds_h2 <- preds_h2 %>% 
#   mutate(combined=paste(behavior,tide,sep="-")) %>% 
#   mutate(combined = as.factor(combined)) %>% 
#   select(-behavior,-tide,-calf_presence,-sd2) 

#merge both prediction dataframes
final_preds <- merge(preds_h1,preds_h2) 

#multiply across
final_preds <- final_preds %>% 
  mutate(mean=mean1*mean2,
         conf.low=conf.low1*conf.low2,
         conf.high=conf.high1*conf.high2)

#Plot
ggplot() +
  geom_line(data=final_preds,aes(x=group_size, y=mean, color=interaction(behavior,tide)), linewidth=1.75) +
  geom_ribbon(data=final_preds,aes(x=group_size, ymin=conf.low, ymax=conf.high, fill=interaction(behavior,tide)), alpha = 0.07, color=NA) +
  geom_point(data=callrate_total,aes(x=group_size, y=n_minute,color=interaction(behavior,tide)),
             position="jitter",alpha=0.3,size=2) +
  theme_classic() +
  labs(x="Group size", y="Calling rate (# calls/minute)") +
  theme(text=element_text(family="sans"),
        axis.text = element_text(size=24),
        axis.ticks.length = unit(0.4,"cm"),
        axis.line = element_line(colour='black', size=1),
        legend.title = element_blank()) +
  scale_color_manual(values=c("goldenrod2","indianred","darkseagreen","deepskyblue4")) +
  scale_fill_manual(values=c("goldenrod2","indianred","darkseagreen","deepskyblue4")) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,55,by=10)) +
  scale_y_continuous(breaks=seq(0,150,by=50)) 


##individual level
#divide estimates and confidence intervals by group size
final_preds$ind.call <- final_preds$mean/final_preds$group_size
final_preds$ind.call.low <- final_preds$conf.low/final_preds$group_size
final_preds$ind.call.high <- final_preds$conf.high/final_preds$group_size
callrate_total$raw.N <- callrate_total$n_minute/callrate_total$group_size

#Plot
ggplot() +
  geom_line(data=final_preds,aes(x=group_size, y=ind.call, color=interaction(behavior,tide)), linewidth=1.75) +
  geom_ribbon(data=final_preds,aes(x=group_size, ymin=ind.call.low, ymax=ind.call.high, fill=interaction(behavior,tide)), alpha = 0.08, color=NA) +
  geom_point(data=callrate_total,aes(x=group_size, y=raw.N,color=interaction(behavior,tide)),
             position="jitter",alpha=0.3,size=2) +
  theme_classic() +
  labs(x="Group size", y="Calling rate (# calls/minute)") +
  theme(text=element_text(family="sans",size=4),
        axis.text = element_text(size=24),
        axis.ticks.length = unit(0.4,"cm"),
        axis.line=element_line(colour='black', size=1),
        legend.title = element_blank()) +
  scale_color_manual(values=c("goldenrod2","indianred","darkseagreen","deepskyblue4")) +
  scale_fill_manual(values=c("goldenrod2","indianred","darkseagreen","deepskyblue4")) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,55,by=10)) 



