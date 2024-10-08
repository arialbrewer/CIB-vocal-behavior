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
library(bbmle) 
library(lmtest)

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
#nonzeros_total<-callrate_total[callrate_total$n_minute>0,]
# #ggplot(data=nonzeros_total, aes(x=n_minute)) +
#   geom_histogram(bins=50, fill="cyan4",color="grey",alpha=0.9) +
#   theme_classic() +
#   scale_y_continuous(expand=c(0,0)) +
#   scale_x_continuous(expand=c(0,0),breaks=seq(0,70,by=10)) +
#   labs(x="Total call rate (#calls/minute)",y="Count") 

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




################### Model building- Hurdle model
#Poisson
hur.pois<-glmmTMB(n_minute ~ behavior + group_size + calf_presence + tide + (1|encounter),
                  ziformula= ~ behavior + group_size + calf_presence + tide + (1|encounter),
                  family=truncated_poisson, data=callrate_total)

summary(hur.pois)
plot(parameters(hur.pois))

#check overdispersion parameter manually (X2/df.resid) Overdispersed > 1
sum(residuals(hur.pois,type="pearson")^2)/1039 

#check residuals
simulateResiduals(fittedModel = hur.pois, plot = T)

#Chi2 Goodness of fit test (null hypothesis= model is correctly specified)  
X2 <- sum((callrate_total$n_minute - fitted(hur.pois))^2 / fitted(hur.pois))
df <- length(callrate_total$n_minute)-length(coef(hur.pois))
pchisq(X2, df,lower.tail = FALSE)
#reject null- model not a good fit


#negative binomial
hur.nb<-glmmTMB(n_minute ~ behavior + group_size + calf_presence + tide + (1|encounter),
             ziformula= ~ behavior + group_size + calf_presence + tide + (1|encounter),
             family=truncated_nbinom2, data=callrate_total)

summary(hur.nb)
plot(parameters(hur.nb))

#check residuals
simulateResiduals(fittedModel = hur.nb, plot = T)

#comparing hurdle poisson and hurdle nb
lrtest(hur.pois,hur.nb)   #nb is better model

AICtab(hur.pois,hur.nb)   #nb is better model

#calculate 95% CI
confint(hur.nb)

###plot coefficients and CI
#zero-inflation (first part of hurdle- binomial)
hurdle1 <- data.frame(variable=c("Behavior","Group size","Calf presence","Tide"),
                      coefficient=c(-0.804,-0.088,-0.768,-0.157),
                      lower=c(-1.294,-0.123,-1.583,-2.314),
                      upper=c(-0.313,-0.053,0.048,2.001),
                      sig=c("yes","yes","no","no")) %>% 
  mutate(variable=as.factor(variable))

#couldn't get behavior to be first so reversed order and will manually change level labels
pal <- c("red","deepskyblue4")
ggplot(data=hurdle1,aes(x=coefficient, y=rev(variable), color=sig)) +
  geom_point(size=5) +
  geom_pointrange(aes(xmin=lower,xmax=upper),lwd=1) +
  geom_vline(xintercept=0,lty=2,lwd=0.5) +
  theme_classic() +
  scale_x_continuous(breaks=seq(-4,4,by=1)) +
  labs(x="Coefficient", y=" Variable", color="Significant") +
  theme(text=element_text(family="serif", size=14)) +
  scale_color_manual(values=pal)



#conditional (second part of hurdle- truncated neg bin)
hurdle2 <- data.frame(variable=c("Behavior","Group size","Calf presence","Tide"),
                      coefficient=c(0.027,0.041, 0.820,-1.408),
                      lower=c(-0.353,0.027,-0.006,-1.969),
                      upper=c(0.407,0.054,1.646,-0.847),
                      sig=c("no","yes","no","yes")) %>% 
  mutate(variable=as.factor(variable))

#couldn't get behavior to be first so reversed order and will manually change level labels
pal <- c("red","deepskyblue4")
ggplot(data=hurdle2,aes(x=coefficient, y=rev(variable), color=sig)) +
  geom_point(size=5) +
  geom_pointrange(aes(xmin=lower,xmax=upper),lwd=1) +
  geom_vline(xintercept=0,lty=2,lwd=0.5) +
  theme_classic() +
  scale_x_continuous(breaks=seq(-4,4,by=1)) +
  labs(x="Coefficient", y=" Variable", color="Significant") +
  theme(text=element_text(family="serif", size=14)) +
  scale_color_manual(values=pal)



## Model diagnostics
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





###### Predictions   ???
library(ggeffects)
#predictions by all variables
pred <- predict_response(hur.nb,terms=c("behavior","calf_presence","group_size","tide"))
print(pred,collapse_ci=TRUE)
plot(pred)


###Sarah recommends this way
#set up new prediction dataframe
# newData <- expand.grid(behavior=c("mill","travel"),calf_presence=c("no","yes"),group_size=c(1:50))
# 
# pred <- predict(glmm.nb2,newData,type="response")
# 
# #number of bootstrap samples and set up to store the results 
# boot.samps <- 500 
# pv <- matrix(NA,nrow=boot.samps,ncol=nrow(newData))
# #simulate from the model, update the model with new predictor 
# for(j in 1:boot.samps){
#   y <- unlist(simulate(glmm.nb2))
#   b.mod <- update(glmm.nb2,y ~ .)
#   #then predict from the updated model if the bootstrap sample converged 
#   if(is.null(summary(b.mod)$optinfo$conv$lme4$messages)==TRUE){
#     RE.sd <- as.data.frame(VarCorr(b.mod))$sdcor[1]
#     pv[j,] <- (1/(1+exp(-predict(b.mod, re.form = ~0, newData) + rnorm(1,0,sd=RE.sd))))
#   }
# }
# pv.mean <- apply(pv,2,function(x)mean(x,na.rm=TRUE))
# pv.lowr <- apply(pv,2,function(x)quantile(x,p=0.025,na.rm=TRUE))
# pv.uppr <- apply(pv,2,function(x)quantile(x,p=0.975,na.rm=TRUE))
# 
# 
# 
# #Plot predictions 
# all.plot <- data.frame(newData[c(1:3),],pv.mean[c(1:3)],pv.lowr[c(1:3)],pv.uppr[c(1:3)],
#                        newData[c(4:6),],pv.mean[c(4:6)],pv.lowr[c(4:6)],pv.uppr[c(4:6)],
#                        newData[c(7:9),],pv.mean[c(7:9)],pv.lowr[c(7:9)],pv.uppr[c(7:9)],
#                        newData[c(10:12),],pv.mean[c(10:12)],pv.lowr[c(10:12)],pv.uppr[c(10:12)],
#                        newData[c(13:15),],pv.mean[c(13:15)],pv.lowr[c(13:15)],pv.uppr[c(13:15)],
#                        newData[c(16:18),],pv.mean[c(16:18)],pv.lowr[c(16:18)],pv.uppr[c(16:18)])
# colnames(all.plot) <- c("behavior1","calf1","group1","mean1","lwr1","uppr1",
#                         "behavior2","calf2","group2","mean2","lwr2","uppr2",
#                         "behavior3","calf3","group3","mean3","lwr3","uppr3",
#                         "behavior4","calf4","group4","mean4","lwr4","uppr4",
#                         "behavior5","calf5","group5","mean5","lwr5","uppr5",
#                         "behavior6","calf6","group6","mean6","lwr6","uppr6")



