#Arial Brewer
#PhD- Chapter 2 Vocal Behavior
#Model: calling rate by call category ~ behavior + group size + calf presence + tide + (1|encounter)

#load packages
library(tidyverse)
library(lme4)
library(MASS)
library(performance)
library(parameters)
library(see)
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
         encounter = as.factor(encounter))

##call rate by group size plots 
#whistles
ggplot(callrate_by_cat, aes(x=group_size, y=ws)) +
  geom_point(alpha=0.2, size=3) +
  geom_smooth(method="glm") +
  theme_classic() +
  labs(x="Group size",y="Calling rate (# calls/minute)") +
  ggtitle("Calling rate- whistles") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(breaks=seq(0,60,by=10)) +
  scale_x_continuous(breaks=seq(0,60,by=10)) 

#pulsed calls
ggplot(callrate_by_cat, aes(x=group_size, y=pc)) +
  geom_point(alpha=0.2, size=3) +
  geom_smooth(method="glm") +
  theme_classic() +
  labs(x="Group size",y="Calling rate (# calls/minute)") +
  ggtitle("Calling rate- pulsed calls") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(breaks=seq(0,60,by=10)) +
  scale_x_continuous(breaks=seq(0,60,by=10)) 

#combined calls
ggplot(callrate_by_cat, aes(x=group_size, y=cc)) +
  geom_point(alpha=0.2, size=3) +
  geom_smooth(method="glm") +
  theme_classic() +
  labs(x="Group size",y="Calling rate (# calls/minute)") +
  ggtitle("Calling rate- combined calls") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(breaks=seq(0,20,by=5)) +
  scale_x_continuous(breaks=seq(0,60,by=10)) 


################### Model building: whistles
##test model to see coefficient of group size
test.ws<-glmer(ws ~ behavior + log(group_size) + calf_presence + tide + (1|encounter),
            family=poisson(link="log"), data=callrate_by_cat)

summary(test.ws)
#coefficient= 1.2 -can use offset for group size

#Poisson with offset on group size
glmm.pois.ws<-glmer(ws~ behavior + offset(log(group_size)) + calf_presence + tide + (1|encounter),
                 family=poisson(link="log"), data=callrate_by_cat)

summary(glmm.pois.ws)

#check overdispersion parameter manually (X2/df.resid). Overdispersed > 1
sum(residuals(glmm.pois.ws,type="pearson")^2)/1046 

#check overdispersion 
check_overdispersion(glmm.pois.ws) #over-dispersed

#check zero-inflation 
check_zeroinflation(glmm.pois.ws)  #zero-inflation

#check residuals
simulateResiduals(fittedModel = glmm.pois.ws, plot = T)

##Fit negative binomial since poisson model is over-dispersed and zero-inflated:
glmm.nb.ws<-glmer.nb(ws~ behavior + offset(log(group_size)) + calf_presence + tide + (1|encounter),
                  data=callrate_by_cat)

summary(glmm.nb.ws)

#check overdispersion parameter manually (X2/df.resid)
sum(residuals(glmm.nb.ws,type="pearson")^2)/1045 

#with performance package
check_overdispersion(glmm.nb.ws)  #no over-dispersion
check_zeroinflation(glmm.nb.ws)   #no zero-inflation

#check residuals- better with nb
simulateResiduals(fittedModel = glmm.nb.ws, plot = T)


####model selection for fixed effects
glmm.nb0.ws<-glmer.nb(ws ~ offset(log(group_size)) + (1|encounter),
                   data=callrate_by_cat)

glmm.nb1.ws<-glmer.nb(ws ~ offset(log(group_size)) + behavior + (1|encounter),
                   data=callrate_by_cat)

glmm.nb2.ws<-glmer.nb(ws ~ offset(log(group_size)) + behavior + calf_presence + (1|encounter),
                   data=callrate_by_cat)

glmm.nb3.ws<-glmer.nb(ws ~ offset(log(group_size)) + behavior + tide + (1|encounter),
                   data=callrate_by_cat)

glmm.nb4.ws<-glmer.nb(ws ~ offset(log(group_size)) + behavior + calf_presence + tide + (1|encounter),
                   data=callrate_by_cat)

glmm.nb5.ws<-glmer.nb(ws ~ offset(log(group_size)) + calf_presence + (1|encounter),
                   data=callrate_by_cat)

glmm.nb6.ws<-glmer.nb(ws ~ offset(log(group_size)) + calf_presence + tide + (1|encounter),
                   data=callrate_by_cat)

glmm.nb7.ws<-glmer.nb(ws ~ offset(log(group_size)) + tide + (1|encounter),
                   data=callrate_by_cat)


#model selection
AIC(glmm.nb0.ws,glmm.nb1.ws,glmm.nb2.ws,glmm.nb3.ws,glmm.nb4.ws,glmm.nb5.ws,glmm.nb6.ws,glmm.nb7.ws) 
#nb2 is the best model (no tide)

#model summary
summary(glmm.nb2.ws) 
plot(parameters(glmm.nb2.ws))

#model overall performance
#Conditional R2: takes both the fixed and random effects into account.
#Marginal R2: considers only the variance of the fixed effects.
model_performance(glmm.nb2.ws)

#check overdispersion 
check_overdispersion(glmm.nb2.ws)

#manually (null=there is no over-dispersion)
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(glmm.nb2.ws) #fail to reject null=no over-dispersion

#check zero-inflation
check_zeroinflation(glmm.nb2.ws)  

#95% confidence intervals
confint(glmm.nb2.ws, level=0.95)

#manually plot with CI
#nb2.summ <- read_csv("call_rate_model_summ.csv") 

ggplot(data=nb2.summ, aes(x=coefficient, y=variable, color=sig)) +
  geom_point() +
  geom_pointrange(aes(xmin=lower,xmax=upper)) +
  geom_vline(xintercept=0,lty=2) +
  theme_classic() +
  scale_x_continuous(breaks=seq(-1.5,1.5,by=0.5)) +
  labs(x="Coefficient", y=" Variable", color="Significant")


## Model diagnostics
#examining residuals    
E.ws <- residuals(glmm.nb2.ws)

#group size
callrate_by_cat$rate_group_size <- cut(callrate_by_cat$group_size, seq(0, 60, by=10))
plot(callrate_by_cat$rate_group_size, E.ws, xlab="Group size",ylab="Residuals")

#behavior
plot(callrate_by_cat$behavior, E.ws, xlab="Behavior", ylab="Residuals")

#calf presence
plot(callrate_by_cat$calf_presence, E.ws, xlab="Calf presence", ylab="Residuals")

#encounter
plot(callrate_by_cat$encounter, E.ws, xlab="Encounter", ylab="Residuals")



################### Model building: pulsed calls
##test model to see coefficient of group size
test.pc<-glmer(pc ~ behavior + log(group_size) + calf_presence + tide + (1|encounter),
               family=poisson(link="log"), data=callrate_by_cat)

summary(test.pc)
#coefficient= 1.0 -can use offset for group size

#Poisson with offset on group size
glmm.pois.pc<-glmer(pc~ behavior + offset(log(group_size)) + calf_presence + tide + (1|encounter),
                    family=poisson(link="log"), data=callrate_by_cat)

summary(glmm.pois.pc)

#check overdispersion parameter manually (X2/df.resid). Overdispersed > 1
sum(residuals(glmm.pois.pc,type="pearson")^2)/1046 

#check overdispersion 
check_overdispersion(glmm.pois.pc) #over-dispersed

#check zero-inflation 
check_zeroinflation(glmm.pois.pc)  #zero-inflation

#check residuals
simulateResiduals(fittedModel = glmm.pois.pc, plot = T)

##Fit negative binomial since poisson model is over-dispersed and zero-inflated:
glmm.nb.pc<-glmer.nb(pc~ behavior + offset(log(group_size)) + calf_presence + tide + (1|encounter),
                     data=callrate_by_cat)

summary(glmm.nb.pc)

#check overdispersion parameter manually (X2/df.resid)
sum(residuals(glmm.nb.pc,type="pearson")^2)/1045 

#check overdispersion
check_overdispersion(glmm.nb.pc)  #no over-dispersion

#check zero-inflation
check_zeroinflation(glmm.nb.pc)   #no zero-inflation

#check residuals- better with nb
simulateResiduals(fittedModel = glmm.nb.pc, plot = T)


####model selection for fixed effects
glmm.nb0.pc<-glmer.nb(pc ~ offset(log(group_size)) + (1|encounter),
                      data=callrate_by_cat)

glmm.nb1.pc<-glmer.nb(pc ~ offset(log(group_size)) + behavior + (1|encounter),
                      data=callrate_by_cat)

glmm.nb2.pc<-glmer.nb(pc ~ offset(log(group_size)) + behavior + calf_presence + (1|encounter),
                      data=callrate_by_cat)

glmm.nb3.pc<-glmer.nb(pc ~ offset(log(group_size)) + behavior + tide + (1|encounter),
                      data=callrate_by_cat)

glmm.nb4.pc<-glmer.nb(pc ~ offset(log(group_size)) + behavior + calf_presence + tide + (1|encounter),
                      data=callrate_by_cat)

glmm.nb5.pc<-glmer.nb(pc ~ offset(log(group_size)) + calf_presence + (1|encounter),
                      data=callrate_by_cat)

glmm.nb6.pc<-glmer.nb(pc ~ offset(log(group_size)) + calf_presence + tide + (1|encounter),
                      data=callrate_by_cat)

glmm.nb7.pc<-glmer.nb(pc ~ offset(log(group_size)) + tide + (1|encounter),
                      data=callrate_by_cat)


#model selection
AIC(glmm.nb0.pc,glmm.nb1.pc,glmm.nb2.pc,glmm.nb3.pc,glmm.nb4.pc,glmm.nb5.pc,glmm.nb6.pc,glmm.nb7.pc) 
#nb2 is the best model (no tide)

#model summary
summary(glmm.nb2.pc) 
plot(parameters(glmm.nb2.pc))

#model overall performance
#Conditional R2: takes both the fixed and random effects into account.
#Marginal R2: considers only the variance of the fixed effects.
model_performance(glmm.nb2.pc)

#check overdispersion 
check_overdispersion(glmm.nb2.pc)

#manually (null=there is no over-dispersion)
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(glmm.nb2.pc) #fail to reject null=no over-dispersion

#check zero-inflation
check_zeroinflation(glmm.nb2.pc)  

#95% confidence intervals
confint(glmm.nb2.pc, level=0.95)
#error

#manually calculate 95% CI = (Coef +/- 1.96 * SE).
#behavior
0.5387 + 1.96*0.3509
0.5387 - 1.96*0.3509

#calf presence
-0.8889 + 1.96*0.4598
-0.8889 - 1.96*0.4598

#manually plot with CI
#nb2.summ <- read_csv("call_rate_model_summ.csv") 

ggplot(data=nb2.summ, aes(x=coefficient, y=variable, color=sig)) +
  geom_point() +
  geom_pointrange(aes(xmin=lower,xmax=upper)) +
  geom_vline(xintercept=0,lty=2) +
  theme_classic() +
  scale_x_continuous(breaks=seq(-1.5,1.5,by=0.5)) +
  labs(x="Coefficient", y=" Variable", color="Significant")


## Model diagnostics
#examining residuals    
E.pc <- residuals(glmm.nb2.pc)

#group size
plot(callrate_by_cat$rate_group_size, E.pc, xlab="Group size",ylab="Residuals")

#behavior
plot(callrate_by_cat$behavior, E.pc, xlab="Behavior", ylab="Residuals")

#calf presence
plot(callrate_by_cat$calf_presence, E.pc, xlab="Calf presence", ylab="Residuals")

#encounter
plot(callrate_by_cat$encounter, E.pc, xlab="Encounter", ylab="Residuals")




################### Model building: combined calls
##test model to see coefficient of group size
glmm.pois.cc<-glmer(cc ~ behavior + log(group_size) + calf_presence + tide + (1|encounter),
               family=poisson(link="log"), data=callrate_by_cat)

summary(glmm.pois.cc)
#coefficient= 8.3 -CANNOT use offset for group size

#check overdispersion parameter manually (X2/df.resid). Overdispersed > 1
sum(residuals(glmm.pois.cc,type="pearson")^2)/1045 
#not overdispersed

#check overdispersion 
check_overdispersion(glmm.pois.cc) #not over-dispersed

#check zero-inflation 
check_zeroinflation(glmm.pois.cc)  #not zero-inflation

#check residuals
simulateResiduals(fittedModel = glmm.pois.cc, plot = T)

#Chi2 Goodness of fit test (null hypothesis= model is correctly specified)  
X2 <- sum((callrate_by_cat$cc - fitted(glmm.pois.cc))^2 / fitted(glmm.pois.cc))
df <- length(callrate_by_cat$cc)-length(coef(glmm.pois.cc))
pchisq(X2, df,lower.tail = FALSE)
# fail to reject null- model is a good fit


####model selection for fixed effects
glmm.pois0.cc<-glmer(cc ~ (1|encounter),
                     family=poisson(link="log"), data=callrate_by_cat)

glmm.pois1.cc<-glmer(cc ~ behavior + (1|encounter),
                     family=poisson(link="log"), data=callrate_by_cat)

glmm.pois2.cc<-glmer(cc ~ behavior + calf_presence + (1|encounter),
                     family=poisson(link="log"), data=callrate_by_cat)

glmm.pois3.cc<-glmer(cc ~ behavior + calf_presence + group_size + (1|encounter),
                     family=poisson(link="log"), data=callrate_by_cat)

glmm.pois4.cc<-glmer(cc ~ behavior + calf_presence + group_size + tide + (1|encounter),
                     family=poisson(link="log"), data=callrate_by_cat)

glmm.pois5.cc<-glmer(cc ~ calf_presence + (1|encounter),
                     family=poisson(link="log"), data=callrate_by_cat)

glmm.pois6.cc<-glmer(cc ~ calf_presence + group_size + (1|encounter),
                     family=poisson(link="log"), data=callrate_by_cat)

glmm.pois7.cc<-glmer(cc ~ calf_presence + group_size +tide + (1|encounter),
                     family=poisson(link="log"), data=callrate_by_cat)

glmm.pois8.cc<-glmer(cc ~ calf_presence +tide + (1|encounter),
                     family=poisson(link="log"), data=callrate_by_cat)

glmm.pois9.cc<-glmer(cc ~ group_size + (1|encounter),
                     family=poisson(link="log"), data=callrate_by_cat)

glmm.pois10.cc<-glmer(cc ~ group_size + tide + (1|encounter),
                      family=poisson(link="log"), data=callrate_by_cat)

glmm.pois11.cc<-glmer(cc ~ group_size + behavior + (1|encounter),
                      family=poisson(link="log"), data=callrate_by_cat)

glmm.pois12.cc<-glmer(cc ~ group_size + behavior + tide + (1|encounter),
                      family=poisson(link="log"), data=callrate_by_cat)

glmm.pois13.cc<-glmer(cc ~ tide + (1|encounter),
                      family=poisson(link="log"), data=callrate_by_cat)

glmm.pois14.cc<-glmer(cc ~ tide + behavior + (1|encounter),
                      family=poisson(link="log"), data=callrate_by_cat)

glmm.pois15.cc<-glmer(cc ~ tide + behavior + calf_presence + (1|encounter),
                      family=poisson(link="log"), data=callrate_by_cat)


#model selection
AIC(glmm.pois0.cc,glmm.pois1.cc,glmm.pois2.cc,glmm.pois3.cc,glmm.pois4.cc,glmm.pois5.cc,glmm.pois6.cc,glmm.pois7.cc,
    glmm.pois8.cc,glmm.pois9.cc,glmm.pois10.cc,glmm.pois11.cc,glmm.pois12.cc,glmm.pois13.cc,glmm.pois14.cc,glmm.pois15.cc)
#11 (299.3) 
#2 (301.16)
#12 (301.17)

#model summary
summary(glmm.pois11.cc) 
plot(parameters(glmm.pois11.cc))

#check overdispersion 
check_overdispersion(glmm.pois11.cc)

#manually (null=there is no over-dispersion)
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(glmm.pois11.cc) #fail to reject null=no over-dispersion

#check zero-inflation
check_zeroinflation(glmm.pois11.cc)  

#95% confidence intervals
confint(glmm.pois11.cc, level=0.95)
#error

#manually calculate 95% CI = (Coef +/- 1.96 * SE).
#group size
0.7789 + 1.96*0.2258
0.7789 - 1.96*0.2258

#behavior
-5.4990 + 1.96*1.2088
-5.4990 - 1.96*1.2088


#manually plot with CI
#nb2.summ <- read_csv("call_rate_model_summ.csv") 

ggplot(data=nb2.summ, aes(x=coefficient, y=variable, color=sig)) +
  geom_point() +
  geom_pointrange(aes(xmin=lower,xmax=upper)) +
  geom_vline(xintercept=0,lty=2) +
  theme_classic() +
  scale_x_continuous(breaks=seq(-1.5,1.5,by=0.5)) +
  labs(x="Coefficient", y=" Variable", color="Significant")


################### Model diagnostics
#examining residuals    
E.cc <- residuals(glmm.pois11.cc)

#group size
plot(callrate_by_cat$rate_group_size, E.cc, xlab="Group size",ylab="Residuals")

#behavior
plot(callrate_by_cat$behavior, E.cc, xlab="Behavior", ylab="Residuals")

#calf presence
plot(callrate_by_cat$calf_presence, E.cc, xlab="Calf presence", ylab="Residuals")

#encounter
plot(callrate_by_cat$encounter, E.cc, xlab="Encounter", ylab="Residuals")





