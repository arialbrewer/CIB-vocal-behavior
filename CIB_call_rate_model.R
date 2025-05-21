#Arial Brewer
#PhD Chapter 2: CIB Vocal Behavior- calling rate model

#load packages
library(tidyverse)
library(corrplot)
library(patchwork)
library(performance)
library(parameters)
library(DHARMa)
library(glmmTMB)
library(lme4)
library(bbmle) 
library(viridis)
library(lmtest)
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



################################### Model building
#check covariate levels
levels(callrate_total$behavior)
levels(callrate_total$calf_presence)
levels(callrate_total$tide) 

#tide is switched around from call cat model, set ebb as reference level
callrate_total$tide <- relevel(callrate_total$tide,ref = "Ebb")
levels(callrate_total$tide) 


###Poisson model
pois<-glmmTMB(n_minute ~ behavior + group_size + calf_presence + tide + (1|encounter),
              family=poisson, data=callrate_total)

summary(pois)

#test for overdispersion (from Bolker 2024)
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

overdisp_fun(pois)   #model is overdispersed

#check overdispersion parameter manually (X2/df.resid) 
#Overdispersed > 1
sum(residuals(pois,type="pearson")^2)/1045 

#Chi2 Goodness of fit test 
#null hypothesis= model is correctly specified
X2 <- sum((callrate_total$n_minute - fitted(pois))^2 / fitted(pois))
df <- length(callrate_total$n_minute)-length(coef(pois))
pchisq(X2, df,lower.tail = FALSE)
#reject null- model not a good fit

###Negative binomial model to account for overdispersion
nb<-glmmTMB(n_minute ~ behavior + group_size + calf_presence + tide + (1|encounter),
            family=nbinom2, data=callrate_total)

summary(nb)

#check overdispersion
overdisp_fun(nb)    #model is no longer overdispersed

#check overdispersion parameter manually (X2/df.resid) Overdispersed > 1
sum(residuals(nb,type="pearson")^2)/1044 

#Chi2 Goodness of fit test
#null hypothesis= model is correctly specified
X2 <- sum((callrate_total$n_minute - fitted(nb))^2 / fitted(nb))
df <- length(callrate_total$n_minute)-length(coef(nb))
pchisq(X2, df,lower.tail = FALSE)
#reject null- model still not a good fit- we still have zero-inflation

###comparing pois and nb models
#likelihood ratio test
lrtest(pois,nb)   #nb is better model



###Negative Binomial Hurdle Model to account for zero-inflation & overdispersion
#glmmTMB sets reference to 1 so we model the probability of not producing a call 
#vs producing a call- this is inverse of what I want- will change in code farther down
hur.nb<-glmmTMB(n_minute ~ behavior + calf_presence + group_size + tide + (1|encounter),
             ziformula= ~ behavior + calf_presence + group_size + tide + (1|encounter),
             family=truncated_nbinom2, data=callrate_total)

summary(hur.nb)
plot(parameters(hur.nb))

#check residuals
simulateResiduals(fittedModel = hur.nb, plot = T)

###comparing pois,nb,hur.nb
#likelihood ratio test
lrtest(pois,nb,hur.nb)  
#AIC
AICtab(pois,nb,hur.nb)   #hur.nb is better model


#calculate 95% CI
confint(hur.nb)

###plot coefficients and CI
#conditional (second part of hurdle- truncated negative binomial)
hurdle2 <- data.frame(variable=c("Behavior","Calf presence","Group size","Tide"),
                      coefficient=c(0.027,0.820,0.041,1.408),
                      lower=c(-0.353,-0.006,0.027,0.847),
                      upper=c(0.407,1.646,0.054,1.969),
                      sig=c("no","no","yes","yes")) %>% 
  mutate(variable=as.factor(variable))

#couldn't get behavior to be first so reversed order and will manually change level labels
ggplot(data=hurdle2,aes(x=coefficient, y=rev(variable), color=sig)) +
  geom_point(size=3.5) +
  geom_pointrange(aes(xmin=lower,xmax=upper),lwd=1) +
  geom_vline(xintercept=0,lty=2,lwd=0.7) +
  theme_classic() +
  scale_x_continuous(breaks=seq(-4,4,by=1)) +
  labs(x="Coefficient", y=" Variable", color="Significant") +
  theme(text=element_text(family="serif", size=20)) +
  scale_color_manual(values=c("red3","deepskyblue4"))



#####calculating odds percentage from coefficients- [(exp(coef)-1)*100]
##conditional model (part two of hurdle)
#behavior (travel)
(exp(0.027)-1)*100

#calf presence (yes)
(exp(0.820)-1)*100

#group size
(exp(0.041)-1)*100

#tide (flood)
(exp(1.408)-1)*100



### Model diagnostics
#examining residuals    
E <- residuals(hur.nb)

#group size
callrate_total$rate_group_size <- cut(callrate_total$group_size, seq(0, 60, by=10))
plot(callrate_total$rate_group_size, E, xlab="Group size",ylab="Residuals")

#behavior
plot(callrate_total$behavior, E, xlab="Behavior", ylab="Residuals")

#calf presence
plot(callrate_total$calf_presence, E, xlab="Calf presence", ylab="Residuals")

#tide
plot(callrate_total$tide, E, xlab="Tide", ylab="Residuals")




##################### Isolating hurdle part 1 to test which is reference (0 or 1)
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
  geom_point(size=3.5) +
  geom_pointrange(aes(xmin=lower,xmax=upper),lwd=1) +
  geom_vline(xintercept=0,lty=2,lwd=0.7) +
  theme_classic() +
  scale_x_continuous(breaks=seq(-4,4,by=1)) +
  labs(x="Coefficient", y=" Variable", color="Significant") +
  theme(text=element_text(family="serif", size=20)) +
  scale_color_manual(values=c("red3","deepskyblue4"))


#####calculating odds percentage from coefficients- [(exp(coef)-1)*100]
### ZI model (part one of hurdle)
#behavior (travel)
(exp(0.804)-1)*100

#calf presence (yes)
(exp(0.768)-1)*100

#group size
(exp(0.088)-1)*100

#tide (flood)
(exp(-0.157)-1)*100


######################### Predictions- Hurdle part 1
#behavior
plot_predictions(zi.hur, condition="behavior",vcov=TRUE) +
  theme_classic() +
  labs(x="Behavior", y="Predicted probability of calling") 

#group size 
plot_predictions(zi.hur, condition="group_size",vcov=TRUE) +
  theme_classic() +
  labs(x="Group size", y="Predicted probability of calling") +
  scale_y_continuous(breaks=seq(0,1,by=0.25)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,60,by=10)) 

#both via plot_predictions
plot_predictions(zi.hur,condition=c("group_size","behavior"),vcov=TRUE) +
  theme_classic() +
  labs(x="Group size", y="Predicted probability of calling") +
  theme(text=element_text(family="serif", size=20),
        axis.text = element_text(size=20),
        axis.ticks.length = unit(0.4,"cm")) +
  scale_color_manual(values=c("red3","blue3")) +
  scale_fill_manual(values=c("red","deepskyblue")) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,60,by=10)) 


#both using predictions then put into ggplot for plot customization
pred.h1 <- plot_predictions(zi.hur,condition=c("group_size","behavior"),vcov=TRUE, draw=FALSE)

#with rug
ggplot(pred.h1, aes(x = group_size, color = behavior, fill=behavior)) +
  geom_line(aes(y = estimate), linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, color=NA) +
  geom_rug(data=callrate_total, aes(x=group_size),linewidth=1) +
  theme_classic() +
  labs(x="Group size", y="Predicted probability of calling") +
  theme(text=element_text(family="serif", size=20),
        axis.text = element_text(size=20),
        axis.ticks.length = unit(0.4,"cm")) +
  scale_color_manual(values=c("red3","blue3")) +
  scale_fill_manual(values=c("red","deepskyblue")) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,60,by=10)) 


#with raw data points instead of rug
ggplot() +
  geom_line(data=pred.h1, aes(x=group_size, y=estimate, color=behavior), linewidth = 1) +
  geom_ribbon(data=pred.h1, aes(x=group_size, ymin=conf.low, ymax=conf.high, fill=behavior), alpha = 0.1, color=NA) +
  geom_point(data=callrate_total,aes(x=group_size, y=n_minute2, color=behavior)) +
  theme_classic() +
  labs(x="Group size", y="Predicted probability of calling") +
  theme(text=element_text(family="serif", size=20),
        axis.text = element_text(size=20),
        axis.ticks.length = unit(0.4,"cm")) +
  scale_color_manual(values=c("sienna3","darkslategrey")) +
  scale_fill_manual(values=c("sienna3","darkslategrey")) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,60,by=10)) 

#with jittered points
ggplot() +
  geom_line(data=pred.h1, aes(x=group_size, y=estimate, color=behavior), linewidth = 1) +
  geom_ribbon(data=pred.h1, aes(x=group_size, ymin=conf.low, ymax=conf.high, fill=behavior), alpha = 0.1, color=NA) +
  geom_jitter(data=callrate_total,aes(x=group_size, y=n_minute2, color=behavior),height=0.001,alpha=0.1) +
  theme_classic() +
  labs(x="Group size", y="Predicted probability of calling") +
  theme(text=element_text(family="serif", size=20),
        axis.text = element_text(size=20),
        axis.ticks.length = unit(0.4,"cm")) +
  scale_color_manual(values=c("sienna3","darkslategrey")) +
  scale_fill_manual(values=c("sienna3","darkslategrey")) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,60,by=10)) 



######################### Predictions- Hurdle part 2
#group size
plot_predictions(hur.nb,condition="group_size",vcov=TRUE) +
  theme_classic() +
  labs(x="Group size", y="Predicted calling rate (# calls/minute)") +
  scale_y_continuous(breaks=seq(0,40,by=10)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,55,by=5)) 

#make in ggplot
pred.gs <- plot_predictions(hur.nb,condition="group_size",vcov=TRUE, draw=FALSE)

#plot
ggplot(pred.gs, aes(x = group_size)) +
  geom_line(aes(y = estimate), linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, color=NA) +
  geom_rug(data=callrate_total, aes(x=group_size),linewidth=1) +
  theme_classic() +
  labs(x="Group size", y="Predicted calling rate (# calls/minute)")

#tide
plot_predictions(hur.nb,condition="tide",vcov=TRUE) +
  theme_classic() +
  labs(x="Tide", y="Predicted calling rate (# calls/minute)") 


#both via plot_predictions
plot_predictions(hur.nb,condition=c("group_size","tide"),vcov=TRUE) +
  theme_classic() +
  labs(x="Group size", y="Predicted calling rate (# calls/minute)") +
  theme(text=element_text(family="serif", size=20),
        axis.text = element_text(size=20),
        axis.ticks.length = unit(0.4,"cm")) +
  scale_color_manual(values=c("hotpink4","grey30")) +
  scale_fill_manual(values=c("hotpink4","grey30")) +
  scale_y_continuous(breaks=seq(0,150,by=25)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,70,by=10))


#both using predictions then put into ggplot for plot customization
pred.h2 <- plot_predictions(hur.nb,condition=c("group_size","tide"),vcov=TRUE, draw=FALSE)

ggplot(pred.h2, aes(x = group_size, color = tide, fill = tide)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, color=NA) +
  geom_line(aes(y = estimate), linewidth = 1) +
  geom_rug(data=callrate_total, aes(x=group_size),linewidth=1) +
  theme_classic() +
  labs(x="Group size", y="Predicted calling rate (# calls/minute)") +
  theme(text=element_text(family="serif", size=20),
        axis.text = element_text(size=20),
        axis.ticks.length = unit(0.4,"cm")) +
  scale_color_manual(values=c("hotpink4","grey30")) +
  scale_fill_manual(values=c("hotpink4","grey30")) +
  scale_y_continuous(breaks=seq(0,150,by=25)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,60,by=10))






########### Adding log(group size) for TNB portion of the model (Hurdle part 2)
#Truncate data to non-zeros
callrate_total_trunc <- callrate_total[which(callrate_total$n_minute>0),]

#tide is switched around from call cat model, set ebb as reference level
callrate_total_trunc$tide <- relevel(callrate_total_trunc$tide,ref = "Ebb")
levels(callrate_total_trunc$tide) 

#add log(group size)
callrate_total_trunc$lgroup_size <- log(callrate_total_trunc$group_size)

nb2<-glmmTMB(n_minute ~ behavior + calf_presence + lgroup_size + tide + (1|encounter),
             family=truncated_nbinom2, data=callrate_total_trunc)

summary(nb2)
plot(parameters(nb2))


#calculate 95% CI
confint(nb2)

###plot coefficients and CI
#conditional (second part of hurdle- truncated negative binomial)
hurdle2.2 <- data.frame(variable=c("Behavior","Calf presence","Group size","Tide"),
                      coefficient=c(0.04282,-0.53717,0.69973,1.41173),
                      lower=c(-0.4004486,-1.6093171,0.4439716,0.4989139),
                      upper=c(0.48609652,0.53497778,0.95548433,2.32455091),
                      sig=c("no","no","yes","yes")) %>% 
  mutate(variable=as.factor(variable))

#couldn't get behavior to be first so reversed order and will manually change level labels
ggplot(data=hurdle2.2,aes(x=coefficient, y=rev(variable), color=sig)) +
  geom_point(size=3.5) +
  geom_pointrange(aes(xmin=lower,xmax=upper),lwd=1) +
  geom_vline(xintercept=0,lty=2,lwd=0.7) +
  theme_classic() +
  scale_x_continuous(breaks=seq(-4,4,by=1)) +
  labs(x="Coefficient", y=" Variable", color="Significant") +
  theme(text=element_text(family="serif", size=20)) +
  scale_color_manual(values=c("red3","deepskyblue4"))



#####calculating odds percentage from coefficients- [(exp(coef)-1)*100]
#behavior (travel)
(exp(0.043)-1)*100

#calf presence (yes)
(exp(-0.537)-1)*100

#log(group size)
(exp(0.699)-1)*100

#tide (flood)
(exp(1.412)-1)*100


####Predictions
#group size
pred.gs <- plot_predictions(nb2,condition="lgroup_size",vcov=TRUE, draw=FALSE)

ggplot(pred.gs, aes(x = exp(lgroup_size))) +
  geom_line(aes(y = estimate), linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, color=NA) +
  theme_classic() +
  labs(x="Group size", y="Predicted calling rate (# calls/minute)")

#tide
plot_predictions(nb2,condition="tide",vcov=TRUE) +
  theme_classic() +
  labs(x="Tide", y="Predicted calling rate (# calls/minute)") 


#both predictions then put into ggplot for plot customization
pred2 <- plot_predictions(nb2,condition=c("lgroup_size","tide"),vcov=TRUE, draw=FALSE)

#with rug
ggplot(pred2, aes(x = exp(lgroup_size), color = tide, fill = tide)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, color=NA) +
  geom_line(aes(y = estimate), linewidth = 1) +
  geom_rug(data=callrate_total, aes(x=group_size),linewidth=1) +
  theme_classic() +
  labs(x="Group size", y="Predicted calling rate (# calls/minute)") +
  theme(text=element_text(family="serif", size=20),
        axis.text = element_text(size=20),
        axis.ticks.length = unit(0.4,"cm")) +
  scale_color_manual(values=c("darkgoldenrod","darkcyan")) +
  scale_fill_manual(values=c("darkgoldenrod","darkcyan")) +
  scale_y_continuous(breaks=seq(0,150,by=25)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,60,by=10))

#with raw data points 
ggplot() +
  geom_line(data=pred2, aes(x=exp(lgroup_size), y=estimate, color=tide), linewidth = 1) +
  geom_ribbon(data=pred2, aes(x=exp(lgroup_size), ymin=conf.low, ymax=conf.high, fill=tide), alpha = 0.1, color=NA) +
  geom_point(data=callrate_total_trunc,aes(x=group_size, y=n_minute, color=tide)) +
  theme_classic() +
  labs(x="Group size", y="Predicted group calling rate (# calls/minute)") +
  theme(text=element_text(family="serif", size=20),
        axis.text = element_text(size=20),
        axis.ticks.length = unit(0.4,"cm")) +
  scale_color_manual(values=c("darkgoldenrod","darkcyan")) +
  scale_fill_manual(values=c("darkgoldenrod","darkcyan")) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,150,by=25)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,55,by=10))

#with jitter points
ggplot() +
  geom_line(data=pred2, aes(x=exp(lgroup_size), y=estimate, color=tide), linewidth = 1) +
  geom_ribbon(data=pred2, aes(x=exp(lgroup_size), ymin=conf.low, ymax=conf.high, fill=tide), alpha = 0.05, color=NA) +
  geom_point(data=callrate_total_trunc,aes(x=group_size, y=n_minute, color=tide), 
             position="jitter",alpha=0.3) +
  theme_classic() +
  labs(x="Group size", y="Predicted group calling rate (# calls/minute)") +
  theme(text=element_text(family="serif", size=20),
        axis.text = element_text(size=20),
        axis.ticks.length = unit(0.4,"cm")) +
  scale_color_manual(values=c("darkgoldenrod","darkcyan")) +
  scale_fill_manual(values=c("darkgoldenrod","darkcyan")) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,150,by=25)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,55,by=10))



##Individual calling rate
#prediction estimate/group size
#add exp(lgroupsize) to unlog group size
pred2$group_size <- exp(pred2$lgroup_size)

#calculate individual call rate and low and high conf. 
pred2$ind.call <- pred2$estimate/pred2$group_size
pred2$ind.call.low <- pred2$conf.low/pred2$group_size
pred2$ind.call.high <- pred2$conf.high/pred2$group_size

ggplot(pred2, aes(x = group_size, color = tide, fill = tide)) +
  geom_line(aes(y = ind.call), linewidth = 1) +
  geom_ribbon(aes(ymin = ind.call.low, ymax = ind.call.high), alpha = 0.1, color=NA) +
  #geom_rug(data=callrate_total, aes(x=group_size),linewidth=1) +
  theme_classic() +
  labs(x="Group size", y="Predicted calling rate (# calls/minute)") +
  theme(text=element_text(family="serif", size=20),
        axis.text = element_text(size=20),
        axis.ticks.length = unit(0.4,"cm")) +
  scale_color_manual(values=c("darkgoldenrod","darkcyan")) +
  scale_fill_manual(values=c("darkgoldenrod","darkcyan")) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,10,by=1)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,60,by=10))









