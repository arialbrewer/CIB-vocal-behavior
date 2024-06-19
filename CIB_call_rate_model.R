#Arial Brewer
#PhD- Chapter 2 Vocal Behavior
#Model 2- calling rate ~ behavior + group size + calf presence + tide + (1|encounter)

#load packages
library(tidyverse)
library(corrplot)
library(viridis)
library(performance)
library(parameters)
library(see)
library(patchwork)
library(lmtest)
library(lme4)
library(MASS)
library(DHARMa)

#load data
setwd("C:/Users/Arial/OneDrive - UW/Desktop/Ch.2 vocal behavior/CIB vocal behavior code/")
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

#combine behavioral & acoustic data and leave NAs for call rate model (zeros matter)
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
         #n_minute_group = n_minute/group_size)

  
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



####Explore raw data patterns
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

pal <- c("gold2","darkseagreen","cyan4")
pal2 <- c("gold2","cyan4")

###independent variables
#behavior distribution
ggplot(data=behavior_type, aes(x="", y=number,fill=behavior)) +
  geom_bar(stat='identity',width=1, color='white') +
  coord_polar("y",start=0) +
  theme_void() + 
  geom_label(aes(label = labels), 
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  scale_fill_manual(values=pal2)

#calf distribution
ggplot(data=calf_type, aes(x="", y=number,fill=calf_presence)) +
  geom_bar(stat='identity',width=1, color='white') +
  coord_polar("y",start=0) +
  theme_void() + 
  geom_label(aes(label = labels), 
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  scale_fill_manual(values=pal2)

#tide distribution
ggplot(data=tide_type, aes(x="", y=number,fill=tide)) +
  geom_bar(stat='identity',width=1, color='white')+
  coord_polar("y",start=0)+
  theme_void() + 
  geom_label(aes(label = labels), 
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  scale_fill_manual(values=pal2)

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

#Call rate without zeros to see shape
nonzeros_total<-callrate_total[callrate_total$n_minute>0,]

ggplot(data=nonzeros_total, aes(x=n_minute)) +
  geom_histogram(bins=50, fill="cyan4",color="grey",alpha=0.9) +
  theme_classic() +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,70,by=10)) +
  labs(x="Total call rate (#calls/minute)",y="Count") 


# Per capital call rate
# ggplot(data=callrate_total, aes(x=n_minute_group)) +
#   geom_histogram(bins=50,fill="turquoise4",color="grey",alpha=0.9) +
#   theme_classic() +
#   scale_y_continuous(expand=c(0,0)) +
#   scale_x_continuous(expand=c(0,0),breaks=seq(0,6,by=1)) +
#   labs(x="Per capita call rate (#calls/minute/whale)",y="Count")

#Per capita call rate without zeros to see shape
# nonzeros_relative<-callrate_total[callrate_total$n_minute_group>0,]
# ggplot(data=nonzeros_relative, aes(x=n_minute_group)) +
#   geom_histogram(bins=50,fill="turquoise4",color="grey",alpha=0.9) +
#   theme_classic() +
#   scale_y_continuous(expand=c(0,0)) +
#   scale_x_continuous(expand=c(0,0),breaks=seq(0,6,by=1)) +
#   labs(x="Per capita call rate (#calls/minute/whale)",y="Count")


###### Group size vs calling rate (#calls/minute) fit with line
ggplot(callrate_total, aes(x=group_size, y=n_minute)) +
  geom_point(alpha=0.2, size=3) +
  theme_classic() +
  geom_smooth(method="glm") +
  labs(x="Group size",y="Calling rate (# calls/minute)") +
  ggtitle("Calling rate") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(breaks=seq(0,60,by=10)) +
  scale_x_continuous(breaks=seq(0,60,by=5)) 

###### Group size vs per capita calling rate (#calls/minute/whale)
# #ggplot(callrate_total, aes(x=group_size, y=n_minute_group)) +
#   geom_point(alpha=0.2, size=3) +
#   theme_classic() +
#   geom_smooth(method="glm") +
#   labs(x="Group size",y="Calling rate (# calls/minute/whale)") +
#   ggtitle("Per capita calling rate") +
#   theme(plot.title=element_text(hjust=0.5)) +
#   scale_y_continuous(breaks=seq(0,6,by=1)) +
#   scale_x_continuous(breaks=seq(0,60,by=5)) 


#Separate call rate by call category
callrate_by_cat <- data_total %>%
  group_by(date,time,encounter,tide,group_size,calf_presence,behavior, call_category) %>% 
  summarise(n_minute = n()) %>% 
  mutate(n_minute = case_when(is.na(call_category)~0, TRUE~n_minute)) %>% 
  pivot_wider(names_from = call_category, values_from = n_minute) %>% 
  replace(is.na(.), 0) %>% 
  mutate(behavior = as.factor(behavior),
         calf_presence = as.factor(calf_presence),
         tide = as.factor(tide),
         encounter = as.factor(encounter),
         n_minute_group_pc = pc, 
         n_minute_group_ws = ws,
         n_minute_group_cc = cc)

#whistles
ggplot(callrate_by_cat, aes(x=group_size, y=n_minute_group_ws)) +
  geom_point(alpha=0.2, size=3) +
  theme_classic() +
  labs(x="Group size",y="Calling rate (# calls/minute)") +
  ggtitle("Calling rate- whistles") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(breaks=seq(0,60,by=10)) +
  scale_x_continuous(breaks=seq(0,60,by=10)) 

#pulsed calls
ggplot(callrate_by_cat, aes(x=group_size, y=n_minute_group_pc)) +
  geom_point(alpha=0.2, size=3) +
  theme_classic() +
  labs(x="Group size",y="Calling rate (# calls/minute)") +
  ggtitle("Calling rate- pulsed calls") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(breaks=seq(0,60,by=10)) +
  scale_x_continuous(breaks=seq(0,60,by=10)) 

#combined calls
ggplot(callrate_by_cat, aes(x=group_size, y=n_minute_group_cc)) +
  geom_point(alpha=0.2, size=3) +
  theme_classic() +
  labs(x="Group size",y="Calling rate (# calls/minute)") +
  ggtitle("Calling rate- combined calls") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(breaks=seq(0,20,by=5)) +
  scale_x_continuous(breaks=seq(0,60,by=10)) 


##Plot for Manolo- divide by sub-encounters when group size changes
#callrate_total_subencounters <- read_csv("callrate_total_subencounters.csv")

# ggplot(callrate_total_subencounters, aes(x=group_size, y=n_minute, color=encounter)) +
#   geom_point(alpha=0.2, size=3) +
#   theme_classic() +
#   labs(x="Group size",y="Relative calling rate (# calls/minute/whale)") +
#   scale_y_continuous(breaks=seq(0,6,by=1)) +
#   scale_x_continuous(breaks=seq(0,60,by=5))


##### Group size vs calling rate by variable
#behavior
ggplot(callrate_total, aes(x=group_size, y=n_minute, color=behavior)) +
  geom_point(size=2) +
  theme_classic() +
  scale_color_viridis(discrete=T,begin=0.3,end=0.8) +
  labs(x="Group size",y="Calling rate (# calls/minute)") +
  scale_y_continuous(breaks=seq(0,60,by=10)) +
  scale_x_continuous(breaks=seq(0,60,by=5)) 

#calf presence
ggplot(callrate_total, aes(x=group_size, y=n_minute, color=calf_presence)) +
  geom_point(size=2) +
  theme_classic() +
  scale_color_viridis(discrete=T,begin=0.3,end=0.8) +
  labs(x="Group size",y="Calling rate (# calls/minute)") +
  scale_y_continuous(breaks=seq(0,60,by=10)) +
  scale_x_continuous(breaks=seq(0,60,by=5))

#tide
ggplot(callrate_total, aes(x=group_size, y=n_minute, color=tide)) +
  geom_point(size=2) +
  theme_classic() +
  scale_color_viridis(discrete=T,begin=0.3,end=0.8) +
  labs(x="Group size",y="Calling rate (# calls/minute)") +
  scale_y_continuous(breaks=seq(0,60,by=10)) +
  scale_x_continuous(breaks=seq(0,60,by=5))


#### violin plots of call rate by categorical variables
#Behavior
p1 <- callrate_total %>%
  ggplot(aes(x=behavior, y=n_minute, fill=behavior)) +
  geom_violin(show.legend = FALSE) +
  theme_classic() +
  scale_fill_manual(values=pal2) +
  labs(x="Behavior", y="Calling rate (# calls/minute)") +
  ggtitle("All")

p2 <- callrate_total %>%
  ggplot(aes(x=behavior, y=n_minute, fill=behavior)) +
  geom_violin(show.legend = FALSE) +
  theme_classic() +
  scale_fill_manual(values=pal2) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x="Behavior", y="Calling rate (# calls/minute)") +
  ggtitle("Non-zeros") +
  ylim(1,64)

p1+p2

#Calf presence
p3 <- callrate_total %>%
  ggplot(aes(x=calf_presence, y=n_minute, fill=calf_presence)) +
  geom_violin(show.legend = FALSE) +
  theme_classic() +
  scale_fill_manual(values=pal2) +
  labs(x="Calf presence", y="Calling rate (# calls/minute)") +
  ggtitle("All")

p4 <- callrate_total %>%
  ggplot(aes(x=calf_presence, y=n_minute, fill=calf_presence)) +
  geom_violin(show.legend = FALSE) +
  theme_classic() +
  scale_fill_manual(values=pal2) +
  labs(x="Calf presence", y="Calling rate (# calls/minute)") +
  ggtitle("Non-zeros") +
  ylim(1,64)

p3+p4

#Tide
p5 <- callrate_total %>%
  ggplot(aes(x=tide, y=n_minute, fill=tide)) +
  geom_violin(show.legend = FALSE) +
  theme_classic() +
  scale_fill_manual(values=pal2) +
  labs(x="Tidal state", y="Calling rate (# calls/minute)") +
  ggtitle("All")

p6 <- callrate_total %>%
  ggplot(aes(x=tide, y=n_minute, fill=tide)) +
  geom_violin(show.legend = FALSE) +
  theme_classic() +
  scale_fill_manual(values=pal2) +
  labs(x="Tidal state", y="Calling rate (# calls/minute)") +
  ggtitle("Non-zeros") +
  ylim(1,64)

p5+p6


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


#mean total and relative calling rates by variable
stats.behavior <- callrate_total %>% 
  group_by(behavior) %>% 
  summarise(total_mean=mean(n_minute),total_sd=sd(n_minute))
            #relative_mean=mean(n_minute_group),relative_sd=sd(n_minute_group)) 

stats.calf <- callrate_total %>% 
  group_by(calf_presence) %>% 
  summarise(total_mean=mean(n_minute),total_sd=sd(n_minute))
            #relative_mean=mean(n_minute_group),relative_sd=sd(n_minute_group)) 

stats.tide <- callrate_total %>% 
  group_by(tide) %>% 
  summarise(total_mean=mean(n_minute),total_sd=sd(n_minute))
            #relative_mean=mean(n_minute_group),relative_sd=sd(n_minute_group)) 


#possible over-dispersion in calling rate- test with model
mean(callrate_total$n_minute)
var(callrate_total$n_minute)


############ Model building - GLMM
##poisson test model to see coefficient of group size
test.model<-glmer(n_minute ~ behavior + log(group_size) + calf_presence + tide + (1|encounter),
                 family=poisson(link="log"), data=callrate_total)

summary(test.model)
#coefficient= 1.1 so SC says to use offset for group size

#Poisson without random effect
glm.pois<-glm(n_minute ~ behavior + offset(log(group_size)) + calf_presence + tide,
              family=poisson(link="log"), data=callrate_total)

summary(glm.pois)

check_overdispersion(glm.pois) #over-dispersed
check_zeroinflation(glm.pois)  #zero-inflated

#check residuals- very over-dispersed and zero-inflated
simulateResiduals(fittedModel = glm.pois, plot = T)


#Poisson with random effect to see if that helps residual plot
glmm.pois<-glmer(n_minute ~ behavior + offset(log(group_size)) + calf_presence + tide + (1|encounter),
              family=poisson(link="log"), data=callrate_total)

summary(glmm.pois)

check_overdispersion(glmm.pois) #over-dispersed
check_zeroinflation(glmm.pois)  #zero-inflation

#check residuals- better with random effect but still over-dispersed
simulateResiduals(fittedModel = glmm.pois, plot = T)

#better model includes random effect of encounter
AIC(glm.pois,glmm.pois)


##Run negative binomial since pois model is overdispersed and zero-inflated:
glmm.nb<-glmer.nb(n_minute~ behavior + offset(log(group_size)) + calf_presence + tide + (1|encounter),
                  data=callrate_total)

summary(glmm.nb)
plot(parameters(glmm.nb))

check_overdispersion(glmm.nb)  #no over-dispersion
check_zeroinflation(glmm.nb)   #no zero-inflation

#check residuals- better with nb
simulateResiduals(fittedModel = glmm.nb, plot = T)


#likelihood ratio test to compare models
lrtest(glmm.pois,glmm.nb)  #nb better model


#model selection on nb model
glmm.nb1<-glmer.nb(n_minute ~ behavior + (1|encounter),
                  data=callrate_total)

summary(glmm.nb1) 
plot(parameters(glmm.nb1))


glmm.nb2<-glmer.nb(n_minute ~ behavior + offset(log(group_size)) + (1|encounter),
                  data=callrate_total)

summary(glmm.nb2) 
plot(parameters(glmm.nb2))


glmm.nb3<-glmer.nb(n_minute ~ behavior + offset(log(group_size)) + calf_presence + (1|encounter),
                  data=callrate_total)

summary(glmm.nb3) 
plot(parameters(glmm.nb3))

glmm.nb4<-glmer.nb(n_minute ~ behavior + offset(log(group_size)) + calf_presence + tide + (1|encounter),
                  data=callrate_total)

summary(glmm.nb4) 
plot(parameters(glmm.nb4))


#model selection
AIC(glmm.nb1,glmm.nb2,glmm.nb3,glmm.nb4)  #nb3 is the best model


#model summary
summary(glmm.nb3) 
plot(parameters(glmm.nb3))

#model diagnostic plots
check_model(glmm.nb3)

#model overall performance
#Conditional R2: takes both the fixed and random effects into account.
#Marginal R2: considers only the variance of the fixed effects.
model_performance(glmm.nb3)
check_collinearity(glmm.nb3)

#check over-dispersion
#with performance package
check_overdispersion(glmm.nb3)

#manually
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(glmm.nb3)

#check zero inflation
check_zeroinflation(glmm.nb3)  


#95% confidence intervals
confint(glmm.nb3)


############ Model diagnostics
# Examining residuals     # ASK SARAH ABOUT THIS
E <- residuals(glmm.nb3)
F <- fitted(glmm.nb3)

callrate_total$rate_group_size <- cut(callrate_total$group_size, seq(0, 60, by=10))
plot(callrate_total$rate_group_size,E, xlab="Group size",ylab="Residuals")
plot(callrate_total$calf_presence,E, xlab="Calf presence", ylab="Residuals")
plot(callrate_total$behavior,E, xlab="Behavior", ylab="Residuals")
plot(callrate_total$encounter,E, xlab="Encounter", ylab="Residuals")

#OR

#DHARMa randomized quantile residuals
simulationOutput <- simulateResiduals(fittedModel = glmm.nb3, plot = T)
residuals(simulationOutput)

#plots by predictor variables
plotResiduals(simulationOutput, form = callrate_total$behavior)
plotResiduals(simulationOutput, form = callrate_total$calf_presence)
plotResiduals(simulationOutput, form = callrate_total$group_size)


#raw ?????
residuals(glmm.nb3,type="response")
plot(residuals(glmm.nb3, type="response"))   

#pearsons ?????
residuals(glmm.nb3,type="pearson")
plot(residuals(glmm.nb3, type="pearson")) 

#deviance ????
quantile(resid(glmm.nb3))
plot(quantile(resid(glmm.nb3)))

#residual deviance (deviance residuals squared and added)
deviance(glmm.nb3)



############ Predictions   # ASK SARAH ABOUT THIS
predict(glmm.nb3)
plot(predict(glmm.nb3))  

#OR

library(ggeffects)
#predictions by all variables
pred <- predict_response(glmm.nb3,terms=c("behavior","calf_presence","group_size"),condition=c(group_size=1))
print(pred,collapse_ci=TRUE)
plot(pred)



#predictions by focal variable
#behavior
predict_response(glmm.nb3,terms="behavior")
plot(predict_response(glmm.nb3,terms="behavior"))

#calf presence
predict_response(glmm.nb3,terms="calf_presence")
plot(predict_response(glmm.nb3,terms="calf_presence"))

#group size
predict_response(glmm.nb3,terms="group_size")
plot(predict_response(glmm.nb3,terms="group_size"))
















########## OR WE CAN USE GLMMTMB and compare with above
library(glmmTMB)

#to show that the effect of group size is 1, meaning a 1:1 ratio of group size to calling rate
#with every one individual added, the calling rate increases by 1
test<-glmmTMB(n_minute ~ behavior + log(group_size) + calf_presence + tide + (1|encounter),
              family=poisson, data=callrate_total)
summary(test)


#poisson
pois.tmb<-glmmTMB(n_minute ~ behavior + offset(log(group_size)) + calf_presence + tide + (1|encounter),
                 family=poisson, data=callrate_total)

summary(pois.tmb)
check_overdispersion(pois.tmb)  #overdispersed
check_zeroinflation(pois.tmb)   #zero-inflated

#negative binomial
nb.tmb<-glmmTMB(n_minute ~ behavior + offset(log(group_size)) + calf_presence + tide + (1|encounter),
               family=nbinom2, data=callrate_total)

summary(nb.tmb)
check_overdispersion(nb.tmb)  #not overdispersed
check_zeroinflation(nb.tmb)   #not zero-inflated

#zero-inflated negative binomial
zinb.tmb<-glmmTMB(n_minute ~ behavior + offset(log(group_size)) + calf_presence + tide + (1|encounter),
                 ziformula= ~ behavior + offset(log(group_size)) + calf_presence + tide + (1|encounter),
                 family=nbinom2, data=callrate_total)

##model convergence problem
summary(zinb.tmb)
check_overdispersion(zinb.tmb)  #not overdispersed
check_zeroinflation(zinb.tmb)   #not zero-inflated


#likelihood ratio test on models, can't use AIC on models with different distributions
lrtest(pois.tmb,nb.tmb,zinb.tmb)    #nb best model


#nb model selection for variables
tmbnb.1<-glmmTMB(n_minute ~ group_size + (1|encounter),
               family=nbinom2, data=callrate_total)

tmbnb.2<-glmmTMB(n_minute ~ behavior + offset(log(group_size)) + (1|encounter),
               family=nbinom2, data=callrate_total)

tmbnb.3<-glmmTMB(n_minute ~ behavior + offset(log(group_size)) + calf_presence + (1|encounter),
               family=nbinom2, data=callrate_total)

tmbnb.4<-glmmTMB(n_minute ~ behavior + offset(log(group_size)) + calf_presence + tide + (1|encounter),
                   family=nbinom2, data=callrate_total)

#model selection
AIC(tmbnb.1,tmbnb.2,tmbnb.3,tmbnb.4)   #tmbnb3 is best model

#model summary
summary(tmbnb.3) 

#model diagnostic plots
check_model(tmbnb.3)

model_performance(tmbnb.3)
check_collinearity(tmbnb.3)
check_overdispersion(tmbnb.3)
check_zeroinflation(tmbnb.3)  
plot(parameters(tmbnb.3))

#95% confidence intervals
confint(tmbnb.3)

#residuals
residuals(tmbnb.3)
plot(residuals(tmbnb.3))   # ASK SARAH ABOUT THIS

#predictions
predict(tmbnb.3)
plot(predict(tmbnb.3))




####################################### test model without big group
test <- read.csv("callrate_total_wo big group.csv") %>% 
      mutate(behavior = as.factor(behavior),
       calf_presence = as.factor(calf_presence),
       tide = as.factor(tide),
       encounter = as.factor(encounter))

library(lme4)

#poisson test model to see coefficient of group size
test.model<-glmer(n_minute ~ behavior + log(group_size) + calf_presence + tide + (1|encounter),
                  family=poisson(link="log"), data=test)

summary(test.model)
#coefficient= 0.8 

#poisson
glmm.pois<-glmer(n_minute ~ behavior + offset(log(group_size)) + calf_presence + tide + (1|encounter),
                 family=poisson(link="log"), data=test)

summary(glmm.pois)
plot(parameters(glmm.pois))

#check over-dispersion
check_overdispersion(glmm.pois) #over-dispersed

#check zero-inflation
check_zeroinflation(glmm.pois)  #zero-inflation

#poisson is over-dispersed and zero-inflated. Run negative binomial:
glmm.nb<-glmer.nb(n_minute~ behavior + offset(log(group_size)) + calf_presence + tide + (1|encounter),
                  data=test)

summary(glmm.nb)
check_overdispersion(glmm.nb)  #no over-dispersion
check_zeroinflation(glmm.nb)   #no zero-inflation
plot(parameters(glmm.nb))

#likelihood ratio test to compare models
lrtest(glmm.pois,glmm.nb)  #nb better model


#model selection on nb model
glmm.nb1<-glmer.nb(n_minute ~ behavior + (1|encounter),
                   data=test)

glmm.nb2<-glmer.nb(n_minute ~ behavior + offset(log(group_size)) + (1|encounter),
                   data=test)

glmm.nb3<-glmer.nb(n_minute ~ behavior + offset(log(group_size)) + calf_presence + (1|encounter),
                   data=test)

glmm.nb4<-glmer.nb(n_minute ~ behavior + offset(log(group_size)) + calf_presence + tide + (1|encounter),
                   data=test)

#model selection
AIC(glmm.nb1,glmm.nb2,glmm.nb3,glmm.nb4)  #nb3 is the best model

#model summary
summary(glmm.nb3) 
plot(parameters(glmm.nb3))

#model diagnostic plots
check_model(glmm.nb3)

#check over-dispersion
check_overdispersion(glmm.nb3)

#check zero inflation
check_zeroinflation(glmm.nb3)  
