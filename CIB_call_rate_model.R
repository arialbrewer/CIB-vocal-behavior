#Arial Brewer
#PhD- Chapter 2 Vocal Behavior
#Model: calling rate ~ behavior + group size + calf presence + tide + (1|encounter)

#load packages
library(tidyverse)
library(corrplot)
library(viridis)
library(lme4)
library(MASS)
library(performance)
library(parameters)
library(see)
library(patchwork)
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


#possible over-dispersion in calling rate- test with model
mean(callrate_total$n_minute)
var(callrate_total$n_minute)


################### Model building - GLMM
##test model to see coefficient of group size
test<-glmer(n_minute ~ behavior + log(group_size) + calf_presence + tide + (1|encounter),
                 family=poisson(link="log"), data=callrate_total)

summary(test)
#coefficient= 1.1 -S.Converse says can use offset for group size


#Poisson with offset on group size
glmm.pois<-glmer(n_minute ~ behavior + offset(log(group_size)) + calf_presence + tide + (1|encounter),
              family=poisson(link="log"), data=callrate_total)

summary(glmm.pois)

#check overdispersion parameter manually (X2/df.resid)
sum(residuals(glmm.pois,type="pearson")^2)/1046 

#check overdispersion with performance package
check_overdispersion(glmm.pois) #over-dispersed

#check zero-inflation with performance package
check_zeroinflation(glmm.pois)  #zero-inflation

#check residuals
simulateResiduals(fittedModel = glmm.pois, plot = T)

#Chi2 Goodness of fit test (null hypothesis= model is correctly specified)  
X2 <- sum((callrate_total$n_minute - fitted(glmm.pois))^2 / fitted(glmm.pois))
df <- length(callrate_total$n_minute)-length(coef(glmm.pois))
pchisq(X2, df,lower.tail = FALSE)
#reject null- model not a good fit


##Fit negative binomial since poisson model is over-dispersed and zero-inflated:
glmm.nb<-glmer.nb(n_minute~ behavior + offset(log(group_size)) + calf_presence + tide + (1|encounter),
                  data=callrate_total)

summary(glmm.nb)

#check overdispersion parameter manually (X2/df.resid)
sum(residuals(glmm.nb,type="pearson")^2)/1045 

#with performance package
check_overdispersion(glmm.nb)  #no over-dispersion
check_zeroinflation(glmm.nb)   #no zero-inflation

#check residuals- better with nb
simulateResiduals(fittedModel = glmm.nb, plot = T)


####model selection for fixed effects
glmm.nb0<-glmer.nb(n_minute ~ offset(log(group_size)) + (1|encounter),
                   data=callrate_total)

glmm.nb1<-glmer.nb(n_minute ~ offset(log(group_size)) + behavior + (1|encounter),
                  data=callrate_total)

glmm.nb2<-glmer.nb(n_minute ~ offset(log(group_size)) + behavior + calf_presence + (1|encounter),
                  data=callrate_total)

glmm.nb3<-glmer.nb(n_minute ~ offset(log(group_size)) + behavior + tide + (1|encounter),
                   data=callrate_total)

glmm.nb4<-glmer.nb(n_minute ~ offset(log(group_size)) + behavior + calf_presence + tide + (1|encounter),
                  data=callrate_total)

glmm.nb5<-glmer.nb(n_minute ~ offset(log(group_size)) + calf_presence + (1|encounter),
                  data=callrate_total)

glmm.nb6<-glmer.nb(n_minute ~ offset(log(group_size)) + calf_presence + tide + (1|encounter),
                   data=callrate_total)

glmm.nb7<-glmer.nb(n_minute ~ offset(log(group_size)) + tide + (1|encounter),
                   data=callrate_total)


#model selection
AIC(glmm.nb0,glmm.nb1,glmm.nb2,glmm.nb3,glmm.nb4,glmm.nb5,glmm.nb6,glmm.nb7) #nb2 is the best model (no tide)

#model summary
summary(glmm.nb2) 
plot(parameters(glmm.nb2))

#model overall performance
#Conditional R2: takes both the fixed and random effects into account.
#Marginal R2: considers only the variance of the fixed effects.
model_performance(glmm.nb2)

#check overdispersion parameter manually (X2/df.resid)
sum(residuals(glmm.nb2,type="pearson")^2)/1046

#with performance package
check_overdispersion(glmm.nb2)

#another way manually (null=there is no over-dispersion)
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(glmm.nb2) #fail to reject null=no over-dispersion

#check zero-inflation
check_zeroinflation(glmm.nb2)  

#95% confidence intervals
confint(glmm.nb2, level=0.95)

#manually plot with CI
nb2.summ <- read_csv("call_rate_model_summ.csv") 

ggplot(data=nb2.summ, aes(x=coefficient, y=variable, color=sig)) +
  geom_point() +
  geom_pointrange(aes(xmin=lower,xmax=upper)) +
  geom_vline(xintercept=0,lty=2) +
  theme_classic() +
  scale_x_continuous(breaks=seq(-1.5,1.5,by=0.5)) +
  labs(x="Coefficient", y=" Variable", color="Significant")



################### Model diagnostics
#examining residuals    
E <- residuals(glmm.nb2)

#group size
callrate_total$rate_group_size <- cut(callrate_total$group_size, seq(0, 60, by=10))
plot(callrate_total$rate_group_size, E, xlab="Group size",ylab="Residuals")

#behavior
plot(callrate_total$behavior, E, xlab="Behavior", ylab="Residuals")

#calf presence
plot(callrate_total$calf_presence, E, xlab="Calf presence", ylab="Residuals")

#encounter
plot(callrate_total$encounter, E, xlab="Encounter", ylab="Residuals")

###Other options to examine residuals
#DHARMa randomized quantile residuals
simulationOutput <- simulateResiduals(fittedModel = glmm.nb2, plot = T)

#performance package
check_model(glmm.nb2)



############ Predictions   
library(ggeffects)
#predictions by all variables- condition used since we have an offset on group size
pred <- predict_response(glmm.nb2,terms=c("behavior","calf_presence","group_size"),condition=c(group_size=1))
print(pred,collapse_ci=TRUE)
plot(pred)


###Sarah recommends this way
#set up new prediction dataframe
newData <- expand.grid(behavior=c("mill","travel"),calf_presence=c("no","yes"),group_size=c(1:50))


pred <- predict(glmm.nb2,newData,type="response")


#number of bootstrap samples and set up to store the results 
boot.samps <- 500 
pv <- matrix(NA,nrow=boot.samps,ncol=nrow(newData))
#simulate from the model, update the model with new predictor 
for(j in 1:boot.samps){
  y <- unlist(simulate(glmm.nb2))
  b.mod <- update(glmm.nb2,y ~ .)
  #then predict from the updated model if the bootstrap sample converged 
  if(is.null(summary(b.mod)$optinfo$conv$lme4$messages)==TRUE){
    RE.sd <- as.data.frame(VarCorr(b.mod))$sdcor[1]
    pv[j,] <- (1/(1+exp(-predict(b.mod, re.form = ~0, newData) + rnorm(1,0,sd=RE.sd))))
  }
}
pv.mean <- apply(pv,2,function(x)mean(x,na.rm=TRUE))
pv.lowr <- apply(pv,2,function(x)quantile(x,p=0.025,na.rm=TRUE))
pv.uppr <- apply(pv,2,function(x)quantile(x,p=0.975,na.rm=TRUE))



#Plot predictions 
all.plot <- data.frame(newData[c(1:3),],pv.mean[c(1:3)],pv.lowr[c(1:3)],pv.uppr[c(1:3)],
                       newData[c(4:6),],pv.mean[c(4:6)],pv.lowr[c(4:6)],pv.uppr[c(4:6)],
                       newData[c(7:9),],pv.mean[c(7:9)],pv.lowr[c(7:9)],pv.uppr[c(7:9)],
                       newData[c(10:12),],pv.mean[c(10:12)],pv.lowr[c(10:12)],pv.uppr[c(10:12)],
                       newData[c(13:15),],pv.mean[c(13:15)],pv.lowr[c(13:15)],pv.uppr[c(13:15)],
                       newData[c(16:18),],pv.mean[c(16:18)],pv.lowr[c(16:18)],pv.uppr[c(16:18)])
colnames(all.plot) <- c("behavior1","calf1","group1","mean1","lwr1","uppr1",
                        "behavior2","calf2","group2","mean2","lwr2","uppr2",
                        "behavior3","calf3","group3","mean3","lwr3","uppr3",
                        "behavior4","calf4","group4","mean4","lwr4","uppr4",
                        "behavior5","calf5","group5","mean5","lwr5","uppr5",
                        "behavior6","calf6","group6","mean6","lwr6","uppr6")



