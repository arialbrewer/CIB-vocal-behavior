#Arial Brewer
#PhD- Chapter 2 Vocal Behavior
#Model 2- calling rate ~ behavior + group size + calf presence + tide + (1 | encounter)

#load packages
library(tidyverse)
library(viridis)
library(corrplot)
library(performance)
library(parameters)
library(see)
library(DHARMa)
library(patchwork)
library(lmtest)

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

  
###Test for correlation between variables
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

###independent variables
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

#group size
ggplot(data=callrate_total, aes(x=group_size)) +
  geom_histogram(bins=50,fill="turquoise4",color="grey",alpha=0.9) +
  theme_classic() +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,400,by=50)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,60,by=5)) +
  labs(x="Group size",y="Count") 

###dependent variable
#Call rate
ggplot(data=callrate_total, aes(x=n_minute)) +
  geom_histogram(bins=50,fill="turquoise4",color="grey",alpha=0.9) +
  theme_classic() +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,70,by=10)) +
  labs(x="Total call rate (#calls/minute)",y="Count") 

#Call rate without zeros to see shape
nonzeros_total<-callrate_total[callrate_total$n_minute>0,]
ggplot(data=nonzeros_total, aes(x=n_minute)) +
  geom_histogram(bins=50, fill="turquoise4",color="grey",alpha=0.9) +
  theme_classic() +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,70,by=10)) +
  labs(x="Total call rate (#calls/minute)",y="Count") 

#Per capital call rate
#ggplot(data=callrate_total, aes(x=n_minute_group)) +
  #geom_histogram(bins=50,fill="turquoise4",color="grey",alpha=0.9) +
  #theme_classic() +
  #scale_y_continuous(expand=c(0,0)) +
  #scale_x_continuous(expand=c(0,0),breaks=seq(0,6,by=1)) +
  #labs(x="Per capita call rate (#calls/minute/whale)",y="Count") 

#Per capita call rate without zeros to see shape
#nonzeros_relative<-callrate_total[callrate_total$n_minute_group>0,]
#ggplot(data=nonzeros_relative, aes(x=n_minute_group)) +
  #geom_histogram(bins=50,fill="turquoise4",color="grey",alpha=0.9) +
  #theme_classic() +
  #scale_y_continuous(expand=c(0,0)) +
  #scale_x_continuous(expand=c(0,0),breaks=seq(0,6,by=1)) +
  #labs(x="Per capita call rate (#calls/minute/whale)",y="Count") 


###### Group size vs calling rate (#calls/minute)
ggplot(callrate_total, aes(x=group_size, y=n_minute)) +
  geom_point(alpha=0.2, size=3) +
  theme_classic() +
  geom_smooth(method="gam") +
  labs(x="Group size",y="Calling rate (# calls/minute)") +
  ggtitle("Calling rate") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(breaks=seq(0,60,by=10)) +
  scale_x_continuous(breaks=seq(0,60,by=5)) 

###### Group size vs per capita calling rate (#calls/minute/whale)
ggplot(callrate_total, aes(x=group_size, y=n_minute_group)) +
  geom_point(alpha=0.2, size=3) +
  theme_classic() +
  geom_smooth(method="glm") +
  labs(x="Group size",y="Calling rate (# calls/minute/whale)") +
  ggtitle("Per capita calling rate") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(breaks=seq(0,6,by=1)) +
  scale_x_continuous(breaks=seq(0,60,by=5)) 


##Plot for Amy- separate call rate into call rate per call category
callrate_total_cattype <- data_total %>%
  group_by(date,time,encounter,tide,group_size,calf_presence,behavior, call_category) %>% 
  summarise(n_minute = n()) %>% 
  mutate(n_minute = case_when(is.na(call_category)~0, TRUE~n_minute)) %>% 
  pivot_wider(names_from = call_category, values_from = n_minute) %>% 
  replace(is.na(.), 0) %>% 
  mutate(behavior = as.factor(behavior),
         calf_presence = as.factor(calf_presence),
         tide = as.factor(tide),
         encounter = as.factor(encounter),
         n_minute_group_pc = pc/group_size, 
         n_minute_group_ws = ws/group_size,
         n_minute_group_cc = cc/group_size)

#whistles
ggplot(callrate_total_cattype, aes(x=group_size, y=n_minute_group_ws)) +
  geom_point(alpha=0.2, size=3) +
  theme_classic() +
  labs(x="Group size",y="Calling rate (# calls/minute/whale)") +
  ggtitle("Per capita calling rate- whistles") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(breaks=seq(0,6,by=1)) +
  scale_x_continuous(breaks=seq(0,60,by=5)) 

#pulsed calls
ggplot(callrate_total_cattype, aes(x=group_size, y=n_minute_group_pc)) +
  geom_point(alpha=0.2, size=3) +
  theme_classic() +
  labs(x="Group size",y="Calling rate (# calls/minute/whale)") +
  ggtitle("Per capita calling rate- pulsed calls") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(breaks=seq(0,6,by=1)) +
  scale_x_continuous(breaks=seq(0,60,by=5)) 

#combined calls
ggplot(callrate_total_cattype, aes(x=group_size, y=n_minute_group_cc)) +
  geom_point(alpha=0.2, size=3) +
  theme_classic() +
  labs(x="Group size",y="Calling rate (# calls/minute/whale)") +
  ggtitle("Per capita calling rate- combined calls") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(breaks=seq(0,6,by=1)) +
  scale_x_continuous(breaks=seq(0,60,by=5)) 


##Plot for Manolo- divide by sub-encounters when group size changes
#Group size vs #calls/minute/group size
callrate_total_subencounters <- read_csv("callrate_total_subencounters.csv")

ggplot(callrate_total_subencounters, aes(x=group_size, y=n_minute_group, color=encounter)) +
  geom_point(alpha=0.2, size=3) +
  theme_classic() +
  labs(x="Group size",y="Relative calling rate (# calls/minute/whale)") +
  scale_y_continuous(breaks=seq(0,6,by=1)) +
  scale_x_continuous(breaks=seq(0,60,by=5)) 


##### Group size vs calling rate by variable
#behavior
ggplot(callrate_total, aes(x=group_size, y=n_minute, color=behavior)) +
  geom_point(size=2) +
  theme_classic() +
  scale_color_viridis(discrete=T,begin=0.3,end=0.8) +
  labs(x="Group size",y="Calling rate (# calls/minute)") +
  scale_y_continuous(breaks=seq(0,6,by=1)) +
  scale_x_continuous(breaks=seq(0,60,by=5)) 

#calf presence
ggplot(callrate_total, aes(x=group_size, y=n_minute, color=calf_presence)) +
  geom_point(size=2) +
  theme_classic() +
  scale_color_viridis(discrete=T,begin=0.3,end=0.8) +
  labs(x="Group size",y="Calling rate (# calls/minute)") +
  scale_y_continuous(breaks=seq(0,6,by=1)) +
  scale_x_continuous(breaks=seq(0,60,by=5))

#tide
ggplot(callrate_total, aes(x=group_size, y=n_minute, color=tide)) +
  geom_point(size=2) +
  theme_classic() +
  scale_color_viridis(discrete=T,begin=0.3,end=0.8) +
  labs(x="Group size",y="Calling rate (# calls/minute)") +
  scale_y_continuous(breaks=seq(0,6,by=1)) +
  scale_x_continuous(breaks=seq(0,60,by=5))


#### violin plots of call rate by categorical variables
#By behavior
p1 <- callrate_total %>%
  ggplot(aes(x=behavior, y=n_minute, fill=behavior)) +
  geom_violin(show.legend = FALSE) +
  theme_minimal() +
  scale_fill_viridis(discrete=T,begin=0.5,end=1,direction=-1) +
  labs(x="Behavior", y="Calling rate (# calls/minute)") +
  ggtitle("All")

p2 <- callrate_total %>%
  ggplot(aes(x=behavior, y=n_minute, fill=behavior)) +
  geom_violin(show.legend = FALSE) +
  theme_minimal() +
  scale_fill_viridis(discrete=T,begin=0.5,end=1,direction=-1) +
  labs(x="Behavior", y="Calling rate (# calls/minute)") +
  ggtitle("Non-zeros") +
  ylim(1,64)

p1+p2

#By calf presence
p3 <- callrate_total %>%
  ggplot(aes(x=calf_presence, y=n_minute, fill=calf_presence)) +
  geom_violin(show.legend = FALSE) +
  theme_minimal() +
  scale_fill_viridis(discrete=T,begin=0.5,end=1,direction=-1) +
  labs(x="Calf presence", y="Calling rate (# calls/minute)") +
  ggtitle("All")

p4 <- callrate_total %>%
  ggplot(aes(x=calf_presence, y=n_minute, fill=calf_presence)) +
  geom_violin(show.legend = FALSE) +
  theme_minimal() +
  scale_fill_viridis(discrete=T,begin=0.5,end=1,direction=-1) +
  labs(x="Calf presence", y="Calling rate (# calls/minute)") +
  ggtitle("Non-zeros") +
  ylim(1,64)

p3+p4

#By tidal state
p5 <- callrate_total %>%
  ggplot(aes(x=tide, y=n_minute, fill=tide)) +
  geom_violin(show.legend = FALSE) +
  theme_minimal() +
  scale_fill_viridis(discrete=T,begin=0.5,end=1,direction=-1) +
  labs(x="Tidal state", y="Calling rate (# calls/minute)") +
  ggtitle("All")

p6 <- callrate_total %>%
  ggplot(aes(x=tide, y=n_minute, fill=tide)) +
  geom_violin(show.legend = FALSE) +
  theme_minimal() +
  scale_fill_viridis(discrete=T,begin=0.5,end=1,direction=-1) +
  labs(x="Tidal state", y="Calling rate (# calls/minute)") +
  ggtitle("Non-zeros") +
  ylim(1,64)

p5+p6


###explore mean patterns 
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


#possible over-dispersion?
## Calling rate (# calls/minute)
mean(callrate_total$n_minute)
var(callrate_total$n_minute)

#possible zero-inflation?
hist(callrate_total$n_minute,50)



##### Model building - GLMM
## Total calling rate (#calls/minute)
library(lme4)

#poisson test model to see coefficient of group size
test.model<-glmer(n_minute ~ behavior + log(group_size) + calf_presence + tide + (1|encounter),
                 family=poisson(link="log"), data=callrate_total)

summary(test.model)
#coefficient= 1.1 so SC says to use offset for group size

#poisson
glmm.pois<-glmer(n_minute ~ behavior + offset(log(group_size)) + calf_presence + tide + (1|encounter),
              family=poisson(link="log"), data=callrate_total)

summary(glmm.pois)
plot(parameters(glmm.pois))

#check over-dispersion
#with performance package
check_overdispersion(glmm.pois) #over-dispersed

#manually
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(glmm.pois)

#check zero-inflation
check_zeroinflation(glmm.pois)  #zero-inflation

#poisson is over-dispersed and zero-inflated. Run negative binomial:
glmm.nb<-glmer.nb(n_minute~ behavior + offset(log(group_size)) + calf_presence + tide + (1|encounter),
                  data=callrate_total)

summary(glmm.nb)
check_overdispersion(glmm.nb)  #no over-dispersion
check_zeroinflation(glmm.nb)   #no zero-inflation
plot(parameters(glmm.nb))

#likelihood ratio test to compare models
lrtest(glmm.pois,glmm.nb)  #nb better model


#model selection on nb model
glmm.nb1<-glmer.nb(n_minute ~ behavior + (1|encounter),
                  data=callrate_total)

glmm.nb2<-glmer.nb(n_minute ~ behavior + offset(log(group_size)) + (1|encounter),
                  data=callrate_total)

glmm.nb3<-glmer.nb(n_minute ~ behavior + offset(log(group_size)) + calf_presence + (1|encounter),
                  data=callrate_total)

glmm.nb4<-glmer.nb(n_minute ~ behavior + offset(log(group_size)) + calf_presence + tide + (1|encounter),
                  data=callrate_total)

#model selection
AIC(glmm.nb1,glmm.nb2,glmm.nb3,glmm.nb4)  #nb3 is the best model

#model summary
summary(glmm.nb3) 
plot(parameters(glmm.nb3))

#model diagnostic plots
check_model(glmm.nb3)

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

###residuals     # ASK SARAH ABOUT THIS
#raw 
residuals(glmm.nb3,type="response")
plot(residuals(glmm.nb3, type="response"))   

#deviance 
quantile(resid(glmm.nb3))
plot(quantile(resid(glmm.nb3)))

#residual deviance (deviance residuals squared and added)
deviance(glmm.nb3)
LRstats(glmm.nb3)

#pearsons 
residuals(glmm.nb3,type="pearson")
plot(residuals(glmm.nb3, type="pearson")) 

#predictions
predict(glmm.nb3)
plot(predict(glmm.nb3))  # ASK SARAH ABOUT THIS




# OR WE CAN USE GLMMTMB- compare with above

#####glmmTMB
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





############################################## OTHER MODEL OPTIONS
###GLMMTMB
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
AIC(ADAPTmodel1,ADAPTmodel2,ADAPTmodel3,ADAPTmodel.full)

summary(ADAPTmodel.full)
check_zeroinflation(ADAPTmodel4) #####still zero-inflated??????????????????????



#hurdle
ADAPTmodel.hurdle<-mixed_model(fixed = n_minute ~ behavior + group_size + calf_presence + tide,
                         random = ~ 1|encounter,
                         data = callrate_total,
                         family = hurdle.negative.binomial(), 
                         zi_fixed = ~ behavior + group_size + calf_presence + tide,
                         zi_random = ~ 1|encounter)

#same results as zinb above
summary(ADAPTmodel.hurdle) 


#zinb better model
AIC(ADAPTmodel4,ADAPTmodel.hurdle)





###GAM
library(MASS)
library(mgcv)

#see what coefficient of group size is
test.gam <- gam(n_minute ~ behavior + log(group_size) + calf_presence + tide + s(encounter,bs="re"),
            family = poisson,
            data = callrate_total)
summary(test.gam)

#poisson
gamp <- gam(n_minute ~ behavior + log(group_size) + calf_presence + tide + s(encounter,bs="re"),
            family = poisson,
            data = callrate_total)

summary(gamp)
check_overdispersion(gamp)
check_zeroinflation(gamp)
plot(gamp)
par(mfrow=c(2,2))
gam.check(gamp)

#tweedy
gamtw <- gam(n_minute ~ s(group_size,bs="cs") + behavior + tide + calf_presence + s(encounter,bs="re"),
             family = tw,
             data = callrate_total)

summary(gamtw)
plot(gamtw)
par(mfrow=c(2,2))
gam.check(gamtw)
k.check(gamtw)

#negative binomial
gamnb <- gam(n_minute ~ s(group_size,bs="cs") + behavior + tide + calf_presence +
               s(encounter,bs="re",k=5),
             family = nb,
             data = callrate_total)

summary(gamnb)
check_overdispersion(gamnb) 
check_zeroinflation(gamnb)  
plot(gamnb)
par(mfrow=c(2,2))
plot(gamnb, all.terms = TRUE)
k.check(gamnb)
gam.check(gamnb)

#compare all models- nb best model
AIC(gamp,gamtw,gamnb)


#model selection 
gamnb1 <- gam(n_minute_group ~ s(group_size,bs="cs") +
                s(encounter,bs="re"),
              family = nb,
              data = callrate_total)

gamnb2 <- gam(n_minute_group ~ s(group_size,bs="cs") + behavior +
                s(encounter,bs="re"),
              family = nb,
              data = callrate_total)

gamnb3 <- gam(n_minute_group ~ s(group_size,bs="cs") + behavior + tide +
                s(encounter,bs="re"),
              family = nb,
              data = callrate_total)

gamnb4 <- gam(n_minute_group ~ s(group_size,bs="cs") + behavior + tide + calf_presence +
                s(encounter,bs="re"),
              family = nb,
              data = callrate_total)

#model 4 (full model) is best model
AIC(gamnb1,gamnb2,gamnb3,gamnb4)

summary(gamnb4)


################ OLD options without random effects
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


