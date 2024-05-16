# Arial Brewer
# Ch. 2 Behavior and call use
# Model 2- calling rate ~ behavior + group size + calf presence + tide


#load packages
library(tidyverse)
library(viridis)

#Read in and tidy data 
setwd("C:/Users/Arial/OneDrive - UW/Desktop/Ch.2 vocal behavior/CIB vocal behavior code/")

acoustic_files <- list.files(pattern = "acoustic.csv")
behavior_files <- list.files(pattern = "behavior.csv")

acoustic_data <- acoustic_files %>%  
  map(read_csv) %>% 
  reduce(rbind) %>% 
  dplyr::select(-begin_Time,-end_Time,-low_Freq,-high_Freq,-selection,-call_type)

behavior_data <- behavior_files %>%  
  map(read_csv) %>% 
  reduce(rbind) %>% 
  dplyr::select(-sample_round,-group_number,-comments)

#combine behavioral & acoustic data and leave NAs for call rate model (zeros matter)
data_total <- behavior_data %>% 
  left_join(acoustic_data, by = c("date","time"), multiple = "all") 

#Bin data into calls per minute and add column (call_presence) as binary for model
callrate_total <- data_total %>%
  mutate(num.calls = case_when(is.na(call_category)~0,TRUE~1)) %>% 
  group_by(date,time,encounter,tide,group_size,calf_presence,behavior) %>% 
  summarise(n_minute = sum(num.calls),
            call_presence = ifelse(n_minute>0,1,0))


#### Test for correlation between variables ####
#create duplicate data for binary variables
callrate_total2 <- data_total %>%
  mutate(num.calls = case_when(is.na(call_category)~0,TRUE~1)) %>% 
  group_by(date,time,encounter,tide,group_size,calf_presence,behavior) %>% 
  summarise(n_minute = sum(num.calls)) 

#change categorical variables to binary
#Mill=0, Travel=1
callrate_total2$behavior <- ifelse(callrate_total2$behavior=="Travel",1,0)

#no=0, yes=1
callrate_total2$calf_presence <- ifelse(callrate_total2$calf_presence=="yes",1,0)
  
#Ebb=0, Flood=1
callrate_total2$tide <- ifelse(callrate_total2$tide=="Flood",1,0)

#correlation matrix
library(corrplot)
x <- cor(callrate_total2[3:7])
corrplot(x, type="upper",order="hclust",addCoef.col = "black")


#### Explore patterns ####
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

#behavior distribution
ggplot(data=behavior_type, aes(x="", y=number,fill=behavior)) +
  geom_bar(stat='identity',width=1, color='white') +
  coord_polar("y",start=0) +
  theme_void() + 
  geom_label(aes(label = labels), 
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  scale_fill_viridis(discrete=T,begin=0.5,end=1,direction=-1)

#calf distribution
ggplot(data=calf_type, aes(x="", y=number,fill=calf_presence)) +
  geom_bar(stat='identity',width=1, color='white') +
  coord_polar("y",start=0) +
  theme_void() + 
  geom_label(aes(label = labels), 
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  scale_fill_viridis(discrete=T,begin=0.5,end=1,direction=-1)

#tide distribution
ggplot(data=tide_type, aes(x="", y=number,fill=tide)) +
  geom_bar(stat='identity',width=1, color='white')+
  coord_polar("y",start=0)+
  theme_void() + 
  geom_label(aes(label = labels), 
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  scale_fill_viridis(discrete=T,begin=0.5,end=1,direction=-1)




##############################################################################
###explore mean patterns 
groupsize_mean <- callrate_total %>% 
  group_by(group_size) %>% 
  summarise(mean_callrate=mean(n_minute),
            sd_callrate=sd(n_minute))

behavior_mean <- callrate_total %>% 
  group_by(behavior) %>% 
  summarise(mean_callrate=mean(n_minute),
            sd_callrate=sd(n_minute))

calfpres_mean <- callrate_total %>% 
  group_by(calf_presence) %>% 
  summarise(mean_callrate=mean(n_minute),
            sd_callrate=sd(n_minute))

tide_mean <- callrate_total %>% 
  group_by(tide) %>% 
  summarise(mean_callrate=mean(n_minute),
            sd_callrate=sd(n_minute))

#line fit with gam
ggplot(groupsize_mean, aes(x=group_size, y=mean_callrate))+
  geom_point() +
  theme_classic() +
  geom_smooth(method="gam") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x="Group size", y="Mean calling rate")

#line fit with glm
ggplot(groupsize_mean, aes(x=group_size, y=mean_callrate))+
  geom_point() +
  theme_classic() +
  geom_smooth(method="glm") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x="Group size", y="Mean calling rate")


###########################################################
#Check for over-dispersion
hist(callrate_total$n_minute,10)

ggplot(callrate_total, aes(x=n_minute))+
  geom_histogram(fill="darkred")+
  theme_classic()+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  labs(x="Calling rate (number of calls per minute)",y="Count")
  
mean(callrate_total$n_minute)
var(callrate_total$n_minute)
#data is overdispersed



############################## Model building #################################
#change characters to factors
callrate_total$tide=factor(callrate_total$tide)
callrate_total$calf_presence=factor(callrate_total$calf_presence)
callrate_total$behavior=factor(callrate_total$behavior)
callrate_total$encounter=factor(callrate_total$encounter)


###compare glm vs gam
library(MASS)
library(mgcv)

###GAM
#poisson
gamp <- gam(n_minute ~ s(group_size,bs="cs",k=5) + behavior + tide + calf_presence +
           s(encounter,bs="re",k=5),
           family = poisson,
           data = callrate_total)

summary(gamp)
plot(gamp)
par(mfrow=c(2,2))
gam.check(gamp)

#tweedy
gamtw <- gam(n_minute ~ s(group_size,bs="cs",k=5) + behavior + tide + calf_presence +
          s(encounter,bs="re",k=5),
           family = tw,
           data = callrate_total)

summary(gamtw)
plot(gamtw)
par(mfrow=c(2,2))
gam.check(gamtw)
k.check(gamtw)

#negative binomial
gamnb <- gam(n_minute ~ s(group_size,bs="cs",k=5) + behavior + tide + calf_presence +
            s(encounter,bs="re",k=5),
            family = nb,
            data = callrate_total)

summary(gamnb)
plot(gamnb)
par(mfrow=c(2,2))
plot(gamnb, all.terms = TRUE)
k.check(gamnb)
gam.check(gamnb)

#compare all models- tw and nb extremely similar, better than poisson
AIC(gamp,gamtw,gamnb)


#model selection with nb model
gamnb1 <- gam(n_minute ~ s(group_size,bs="cs",k=5) +
               s(encounter,bs="re",k=5),
             family = nb,
             data = callrate_total)

gamnb2 <- gam(n_minute ~ s(group_size,bs="cs",k=5) + behavior +
               s(encounter,bs="re",k=5),
             family = nb,
             data = callrate_total)

gamnb3 <- gam(n_minute ~ s(group_size,bs="cs",k=5) + behavior + tide +
               s(encounter,bs="re",k=5),
             family = nb,
             data = callrate_total)

gamnb4 <- gam(n_minute ~ s(group_size,bs="cs",k=5) + behavior + tide + calf_presence +
               s(encounter,bs="re",k=5),
             family = nb,
             data = callrate_total)

summary(gamnb4)

#model 4 (full model) is best model
AIC(gamnb1,gamnb2,gamnb3,gamnb4)

####how to account for zero-inflation??????????????????????



############################# GLM models ######################################
#### Hurdle model (doesn't work with random effect)
library(pscl)
library(countreg)

#zero.dist for zero count, dist for non-zero count
hurdle.pois <- hurdle(n_minute ~ behavior + group_size + calf_presence + tide + (1|encounter), 
                zero.dist="binomial", dist="poisson",
                data = callrate_total)

summary(hurdle.pois)
rootogram(hurdle.pois, max = 70)


#negative binomial
hurdle.nb<- hurdle(n_minute ~ behavior + group_size + calf_presence + tide + (1|encounter), 
                     zero.dist="binomial", dist="negbin",
                     data = callrate_total)

summary(hurdle.nb)
rootogram(hurdle.nb, max = 70)




############################### GLMM OPTIONS #############################
###glmmTMB
library(glmmTMB)
library(performance)
library(parameters)
library(see)


##zero inflation models
pois<-glmmTMB(n_minute ~ behavior + group_size + calf_presence + tide + (1|encounter),
                 family=poisson, data=callrate_total)

nb<-glmmTMB(n_minute ~ behavior + group_size + calf_presence + tide + (1|encounter),
               family=nbinom2, data=callrate_total)

zinb<-glmmTMB(n_minute ~ behavior + group_size + calf_presence + tide + (1|encounter),
                 ziformula= ~ behavior + group_size + calf_presence + tide,
                 family=nbinom2, data=callrate_total)

#compare models- zinb better model
anova(pois,nb,zinb)


#zinb model selection for variables
zinb0<-glmmTMB(n_minute ~ group_size + (1|encounter),
                 ziformula= ~ group_size + (1|encounter),
                 family=nbinom2, data=callrate_total)

zinb1<-glmmTMB(n_minute ~ behavior + group_size + (1|encounter),
               ziformula= ~ behavior + group_size + (1|encounter),
               family=nbinom2, data=callrate_total)

zinb2<-glmmTMB(n_minute ~ behavior + group_size + calf_presence + (1|encounter),
               ziformula= ~ behavior + group_size + calf_presence + (1|encounter),
               family=nbinom2, data=callrate_total)

zinb.full<-glmmTMB(n_minute ~ behavior + group_size + calf_presence + tide + (1|encounter),
               ziformula= ~ behavior + group_size + calf_presence + tide + (1|encounter),
               family=nbinom2, data=callrate_total)

#full model with all variables is best
AIC(zinb0,zinb1,zinb2,zinb.full)


summary(zinb.full) 
check_collinearity(zinb.full)
check_overdispersion(zinb.full)
check_zeroinflation(zinb.full)  #####still zero-inflated??????????????????????
plot(parameters(zinb4))


##hurdle model
hnb<-glmmTMB(n_minute ~ behavior + group_size + calf_presence + tide + (1|encounter),
               ziformula= ~ behavior + group_size + calf_presence + tide + (1|encounter),
               family=truncated_nbinom2, data=callrate_total)

#same significant variables as zinb
summary(hnb)
check_collinearity(hnb)
check_overdispersion(hnb)
check_zeroinflation(hnb)
plot(parameters(hnb))

#compare zinb and hurdle model- zinb better model
AIC(zinb.full,hnb)



#############simulating from fitted model (from Brooks paper)###############
library(plyr)
library(reshape)
library(knitr)
sims=simulate(zinb4, seed=1, nsim=10)

simdatlist=lapply(sims, function(n_minute){
  cbind('n_minute', callrate_total[,c('group_size','behavior','calf_presence','tide')])
})
simdatsums=lapply(simdatlist, function(x){
  ddply(x, ~group_size + behavior + calf_presence + tide, summarize,
        absence=mean('n_minute'==0),
        mu=mean('n_minute'))
})
ssd=do.call(rbind, simdatsums)


real = ddply(callrate_total, ~group_size + behavior + calf_presence + tide, summarize,
             absence=mean(n_minute==0),
             mu=mean(n_minute))
ggplot(ssd, aes(x=absence, color=calf_presence))+
  geom_density(adjust=4)+
  facet_wrap(~behavior)+
  geom_point(data=real, aes(x=absence, y=1, color=behavior), size=2)+
  xlab("Probability that belugas are not calling")+ylab(NULL)







############################################################################


###GLMMadaptive
library(GLMMadaptive)
library(performance)


#zinb model
ADAPTmodel1<-mixed_model(fixed = n_minute ~ behavior,
                         random = ~ 1|encounter,
                         data = callrate_total,
                         family = zi.negative.binomial(), 
                         zi_fixed = ~ behavior,
                         zi_random = ~ 1|encounter)

ADAPTmodel2<-mixed_model(fixed = n_minute ~ behavior + group_size,
                         random = ~ 1|encounter,
                         data = callrate_total,
                         family = zi.negative.binomial(), 
                         zi_fixed = ~ behavior + group_size,
                         zi_random = ~ 1|encounter)

ADAPTmodel3<-mixed_model(fixed = n_minute ~ behavior + group_size + calf_presence,
                         random = ~ 1|encounter,
                         data = callrate_total,
                         family = zi.negative.binomial(), 
                         zi_fixed = ~ behavior + group_size + calf_presence,
                         zi_random = ~ 1|encounter)

ADAPTmodel.full<-mixed_model(fixed = n_minute ~ behavior + group_size + calf_presence + tide,
                         random = ~ 1|encounter,
                         data = callrate_total,
                         family = zi.negative.binomial(), 
                         zi_fixed = ~ behavior + group_size + calf_presence + tide,
                         zi_random = ~ 1|encounter)

#full model is best model
AIC(ADAPTmodel1,ADAPTmodel2,ADAPTmodel3,ADAPTmodel4)

summary(ADAPTmodel.full)
check_zeroinflation(ADAPTmodel4) #####still zero-inflated??????????????????????




#hurdle
ADAPTmodel5<-mixed_model(fixed = n_minute ~ behavior + group_size + calf_presence + tide,
                         random = ~ 1|encounter,
                         data = callrate_total,
                         family = hurdle.negative.binomial(), 
                         zi_fixed = ~ behavior + group_size + calf_presence + tide,
                         zi_random = ~ 1|encounter)

#same results as zinb above
summary(ADAPTmodel5) 


#zinb better model
AIC(ADAPTmodel4,ADAPTmodel5)







