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
library(marginaleffects)

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




######################### Predictions

#####Hurdle part 1 only
pred.h1 <- plot_predictions(zi.hur,condition=c("group_size","behavior"),re.form=NULL,vcov=TRUE, draw=FALSE)

#update so predictions can't go above 1
pred.h1$conf.high <- ifelse(pred.h1$conf.high>1,1,pred.h1$conf.high)

ggplot() +
  geom_line(data=pred.h1, aes(x=group_size, y=estimate, color=behavior), linewidth = 1.75) +
  geom_ribbon(data=pred.h1, aes(x=group_size, ymin=conf.low, ymax=conf.high, fill=behavior), alpha = 0.1, color=NA) +
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



#####Hurdle part 2 only
#Truncate data to non-zeros
callrate_total_trunc <- callrate_total[which(callrate_total$n_minute>0),]

#add log(group size)
callrate_total_trunc$lgroup_size <- log(callrate_total_trunc$group_size)

#run model
nb.hur<-glmmTMB(n_minute ~ behavior + calf_presence + lgroup_size + tide + (1|encounter),
             family=truncated_nbinom2, data=callrate_total_trunc)
        

summary(nb.hur)

pred.h2 <- plot_predictions(nb.hur,condition=c("lgroup_size","tide"),re.form=NA, vcov=TRUE, draw=FALSE)

ggplot() +
   geom_line(data=pred.h2, aes(x=exp(lgroup_size), y=estimate, color=tide), linewidth = 1.75) +
   geom_ribbon(data=pred.h2, aes(x=exp(lgroup_size), ymin=conf.low, ymax=conf.high, fill=tide), alpha = 0.1, color=NA) +
   geom_point(data=callrate_total_trunc,aes(x=group_size, y=n_minute, color=tide), 
                     position="jitter",alpha=0.4,size=2) +
   theme_classic() +
   labs(x="Group size", y="Predicted group calling rate (# calls/minute)") +
   theme(text=element_text(family="sans"),
                axis.text = element_text(size=22),
                axis.ticks.length = unit(0.4,"cm"),
                axis.line=element_line(colour='black', size=1)) +
   scale_color_manual(values=c("peachpuff3","darkslategray")) +
   scale_fill_manual(values=c("peachpuff3","darkslategray")) +
   scale_x_continuous(expand=c(0,0),breaks=seq(0,55,by=10))

#save csv
#write_csv(pred.h2,"C:/Users/arial/Desktop/Ch.2 Vocal behavior/CIB vocal behavior code/pred.h2.csv")



#####Full Hurdle model
pred.full <- plot_predictions(hur.mod,condition=c("group_size","tide","behavior"), re.form=NA, vcov=TRUE, draw=FALSE)

ggplot() +
  geom_line(data=pred.full,aes(x=group_size, y=estimate, color=interaction(behavior,tide)), linewidth=1.75) +
  geom_ribbon(data=pred.full,aes(x=group_size, ymin=conf.low, ymax=conf.high,fill=interaction(behavior,tide)), alpha = 0.07, color=NA) +
  geom_point(data=callrate_total,aes(x=group_size, y=n_minute,color=interaction(behavior,tide)),
             position="jitter",alpha=0.2,size=2) +
  theme_classic() +
  labs(x="Group size", y="Calling rate (# calls/minute)") +
  theme(text=element_text(family="sans"),
        axis.text = element_text(size=24),
        axis.ticks.length = unit(0.4,"cm"),
        axis.line=element_line(colour='black', size=1)) +
  scale_color_manual(values=c("goldenrod2","indianred","darkseagreen","deepskyblue4")) +
  scale_fill_manual(values=c("goldenrod2","indianred","darkseagreen","deepskyblue4")) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,150,by=25)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,55,by=10)) 





########Individual level calling plots
##Hurdle part 1
#function to calculate p (probability of individual calling)
back.p1 <- function(estimate,group_size){
  p <- 1-(1-estimate)^(1/group_size)
  return(p)
}

#add column for back.p (probability at individual level)
pred.h1$back.p <- back.p1(pred.h1$estimate,pred.h1$group_size)
pred.h1$back.p.low <- back.p1(pred.h1$conf.low,pred.h1$group_size)
pred.h1$back.p.high <- back.p1(pred.h1$conf.high,pred.h1$group_size)

#update so predictions can't go above plot limit
pred.h1$back.p.high <- ifelse(pred.h1$back.p.high>0.999,0.35,pred.h1$back.p.high)

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
  geom_line(data=pred.h1, aes(x=group_size, y=back.p, color=behavior), linewidth = 1.75) +
  geom_ribbon(data=pred.h1, aes(x=group_size, ymin=back.p.low, ymax=back.p.high, fill=behavior), alpha = 0.1, color=NA) +
  geom_point(data=df.raw,aes(x=group_size, y=back.p, color=behavior),alpha=0.7,size=2) +
  theme_classic() +
  labs(x="Group size", y="Probability of calling") +
  theme(text=element_text(family="sans"),
        axis.text = element_text(size=24),
        axis.ticks.length = unit(0.4,"cm"),
        axis.line=element_line(colour='black', size=1)) +
  scale_color_manual(values=c("sienna3","darkolivegreen")) +
  scale_fill_manual(values=c("sienna3","darkolivegreen")) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,55,by=10)) +
  ylim(0,0.35)




##Hurdle part 2
#calculate prediction estimate/group size and add exp(lgroupsize) to unlog group size
pred.h2$group_size <- exp(pred.h2$lgroup_size)

#calculate individual call rate and low and high conf. 
pred.h2$ind.call <- pred.h2$estimate/pred.h2$group_size
pred.h2$ind.call.low <- pred.h2$conf.low/pred.h2$group_size
pred.h2$ind.call.high <- pred.h2$conf.high/pred.h2$group_size

callrate_total_trunc$raw.N <- callrate_total_trunc$n_minute/callrate_total_trunc$group_size

#plot
ggplot() +
  geom_line(data=pred.h2, aes(x=group_size, y=ind.call, color=tide), linewidth = 1.75) +
  geom_ribbon(data=pred.h2, aes(x=group_size, ymin=ind.call.low, ymax=ind.call.high, fill=tide), alpha = 0.1, color=NA) +
  geom_point(data=callrate_total_trunc,aes(x=group_size, y=raw.N, color=tide), 
             position="jitter",alpha=0.4,size=2) +
  theme_classic() +
  labs(x="Group size", y="Predicted calling rate (# calls/minute)") +
  theme(text=element_text(family="sans"),
        axis.text = element_text(size=24),
        axis.ticks.length = unit(0.4,"cm"),
        axis.line=element_line(colour='black', size=1)) +
  scale_color_manual(values=c("peachpuff3","darkslategray")) +
  scale_fill_manual(values=c("peachpuff3","darkslategray")) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,60,by=10))




##Full hurdle
#divide estimates and confidence intervals by group size
pred.full$ind.call <- pred.full$estimate/pred.full$group_size
pred.full$ind.call.low <- pred.full$conf.low/pred.full$group_size
pred.full$ind.call.high <- pred.full$conf.high/pred.full$group_size

#update so predictions can't go above or below plot limit
#pred.full$ind.call.high <- ifelse(pred.full$ind.call.high>2.5,2.5,pred.full$ind.call.high)
pred.full$ind.call.low <- ifelse(pred.full$ind.call.low<0,0,pred.full$ind.call.low)

callrate_total$raw.N <- callrate_total$n_minute/callrate_total$group_size

#zoomed in to see curves
ggplot() +
  geom_line(data=pred.full,aes(x=group_size, y=ind.call, color=interaction(behavior,tide)), linewidth=1.75) +
  geom_ribbon(data=pred.full,aes(x=group_size, ymin=ind.call.low, ymax=ind.call.high, fill=interaction(behavior,tide)), alpha = 0.08, color=NA) +
  geom_point(data=callrate_total,aes(x=group_size, y=raw.N,color=interaction(behavior,tide)),
             position="jitter",alpha=0.3,size=2) +
  theme_classic() +
  labs(x="Group size", y="Calling rate (# calls/minute)") +
  theme(text=element_text(family="sans",size=4),
        axis.text = element_text(size=24),
        axis.ticks.length = unit(0.4,"cm"),
        axis.line=element_line(colour='black', size=1)) +
  scale_color_manual(values=c("goldenrod2","indianred","darkseagreen","deepskyblue4")) +
  scale_fill_manual(values=c("goldenrod2","indianred","darkseagreen","deepskyblue4")) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,55,by=10)) +
  ylim(0,2)


#zoomed out for full plot
ggplot() +
  geom_line(data=pred.full,aes(x=group_size, y=ind.call, color=interaction(behavior,tide)), linewidth=1.75) +
  geom_ribbon(data=pred.full,aes(x=group_size, ymin=ind.call.low, ymax=ind.call.high, fill=interaction(behavior,tide)), alpha = 0.08, color=NA) +
  geom_point(data=callrate_total,aes(x=group_size, y=raw.N,color=interaction(behavior,tide)),
             position="jitter",alpha=0.3,size=2) +
  theme_classic() +
  labs(x="Group size", y="Calling rate (# calls/minute)") +
  theme(text=element_text(family="sans",size=4),
        axis.text = element_text(size=24),
        axis.ticks.length = unit(0.4,"cm"),
        axis.line=element_line(colour='black', size=1)) +
  scale_color_manual(values=c("goldenrod2","indianred","darkseagreen","deepskyblue4")) +
  scale_fill_manual(values=c("goldenrod2","indianred","darkseagreen","deepskyblue4")) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,55,by=10)) 




