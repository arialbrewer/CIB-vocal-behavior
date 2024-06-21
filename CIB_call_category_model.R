#Arial Brewer
#PhD- Chapter 2 Vocal Behavior
#Model 1- call category ~ behavior + group size + calf presence + tide + (1|encounter)

#load packages
library(tidyverse)
library(corrplot)
library(mgcv)
library(mgcViz)
library(performance)
library(parameters)
library(see)
library(gratia)
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
  dplyr::select(-sample_round,-group_number,-dot,-count_white,-count_gray,-count_calf,-comments)

#combine behavioral & acoustic data, remove NAs (zeros don't matter), and change to factors
callcat_total <- behavior_data %>% 
  left_join(acoustic_data, by = c("date","time"), multiple = "all") %>% 
  na.omit() %>% 
  mutate(behavior=as.factor(behavior),
        calf_presence=as.factor(calf_presence),
        tide=as.factor(tide),
        encounter=as.factor(encounter),
        group_size=as.integer(group_size),
        call_category=as.factor(call_category))


###Test for correlation between covariates
##create duplicate dataframe
callcat_total2 <- behavior_data %>% 
  left_join(acoustic_data, by = c("date","time"), multiple = "all") %>% 
  na.omit()

##change categorical variables to binary
#Mill=0, Travel=1
callcat_total2$behavior <- ifelse(callcat_total2$behavior=="Travel",1,0)

#no=0, yes=1
callcat_total2$calf_presence <- ifelse(callcat_total2$calf_presence=="yes",1,0)

#Ebb=0, Flood=1
callcat_total2$tide <- ifelse(callcat_total2$tide=="Flood",1,0)

#correlation matrix
x <- cor(callcat_total2[3:7])
corrplot(x, type="upper",order="hclust",addCoef.col = "black")


###Explore raw data patterns
behavior_type <- callcat_total %>% 
  group_by(behavior) %>% 
  summarise(number=n()) %>% 
  mutate(perc=number/sum(number))%>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

calf_type <- callcat_total %>% 
  group_by(calf_presence) %>% 
  summarise(number=n()) %>% 
  mutate(perc=number/sum(number))%>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

tide_type <- callcat_total %>% 
  group_by(tide) %>% 
  summarise(number=n()) %>% 
  mutate(perc=number/sum(number))%>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

callcat_type <- callcat_total %>% 
  group_by(call_category) %>% 
  summarize(number=n()) %>% 
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
ggplot(data=callcat_total, aes(x=group_size)) +
  geom_histogram(bins=50,fill="cyan4",color="grey",alpha=0.9) +
  theme_classic() +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,500,by=50)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,60,by=5)) +
  labs(x="Group size",y="Count") 


###dependent variable
#call category distribution
ggplot(data=callcat_type, aes(x="", y=number,fill=call_category)) +
  geom_bar(stat='identity',width=1, color='white')+
  coord_polar("y",start=0)+
  theme_void() + 
  geom_label(aes(label = labels), 
             position = position_stack(vjust = 0.5),
             show.legend = F) +
  labs(fill="Call category") +
  scale_fill_manual(values=pal)

###summarize call categories by covariates
#behavior
callcat_behavior <- callcat_total %>% 
  group_by(behavior,call_category) %>% 
  summarize(number=n())

ggplot(data=callcat_behavior, aes(x=behavior,y=number,fill=call_category)) + 
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  labs(x="Behavior", y="Number",fill="Call category") +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_manual(values=pal)

#calf presence
callcat_calf <- callcat_total %>% 
  group_by(calf_presence,call_category) %>% 
  summarize(number=n())

ggplot(data=callcat_calf, aes(x=calf_presence,y=number,fill=call_category)) + 
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  labs(x="Calf presence", y="Number", fill="Call category") +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_manual(values=pal)

#tide
callcat_tide <- callcat_total %>% 
  group_by(tide,call_category) %>% 
  summarize(number=n())

ggplot(data=callcat_tide, aes(x=tide,y=number,fill=call_category)) + 
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  labs(x="Tide", y="Number", fill="Call category") +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_manual(values=pal)

#group size
callcat_groupsize <- callcat_total %>% 
  group_by(group_size,call_category) %>% 
  summarize(number=n())

ggplot(data=callcat_groupsize, aes(x=group_size,y=number,fill=call_category)) + 
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  labs(x="Group size", y="Count", fill="Call category") +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,60,by=5)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_manual(values=pal)


## violin plots of call category by categorical variables by group size
#call category by behavior
callcat_total %>%
  ggplot(aes(x=behavior, y=group_size, fill=call_category)) +
  geom_violin() +
  theme_classic() +
  labs(x="Behavior", y="Group size", fill="Call category") +
  scale_y_continuous(breaks=seq(0,60,by=10)) +
  scale_fill_manual(values=pal)

#call category by calf presence
callcat_total %>%
  ggplot(aes(x=calf_presence, y=group_size, fill=call_category)) +
  geom_violin() +
  theme_classic() +
  labs(x="Calf presence", y="Group size", fill="Call category") +
  scale_y_continuous(breaks=seq(0,60,by=10)) +
  scale_fill_manual(values=pal)

#call category by tide
callcat_total %>%
  ggplot(aes(x=tide, y=group_size, fill=call_category)) +
  geom_violin() +
  theme_classic() +
  labs(x="Tidal state", y="Group size", fill="Call category") +
  scale_y_continuous(breaks=seq(0,60,by=10)) +
  scale_fill_manual(values=pal)



#################### Model building
#multinomial that works with a random effect- mgcv with multinomial distribution

#default levels are cc,pc,ws
levels(callcat_total$call_category)

#relevel so ws is level 0 (reference level)
callcat_total$call_category <- relevel(callcat_total$call_category,ref = "ws")
callcat_total$call_category2 <- as.numeric(callcat_total$call_category)-1
levels(callcat_total$call_category)

#categories must be coded 0 to K
all(callcat_total$call_category2 %in% c(0L, 1L, 2L))

##K=number of levels of response-1. Because there is K=2 we repeat the formula twice within a list

#with smoother on group size
gam.mn1 <- gam(list(call_category2 ~ behavior + s(group_size) + calf_presence + tide + s(encounter,bs="re"),
                                   ~ behavior + s(group_size) + calf_presence + tide + s(encounter,bs="re")),
               data = callcat_total,
               family = multinom(K=2),
               method = "REML",
               optimizer = "efs")
summary(gam.mn1)

#without smoother on group size
gam.mn2 <- gam(list(call_category2 ~ behavior + group_size + calf_presence + tide + s(encounter,bs="re"),
                                   ~ behavior + group_size + calf_presence + tide + s(encounter,bs="re")),
               data = callcat_total,
               family = multinom(K=2),
               method = "REML",
               optimizer = "efs")

summary(gam.mn2)

#compare models
AIC(gam.mn1,gam.mn2)  #model 1 with smoother on group size and random effect is best



#model selection with best model (with group size smoother)
#if decide to go with smoother on group size, add to model selection below

################################################################################################
#model selection with best model (without group size smoother) 
mn0 <- gam(list(call_category2 ~ s(encounter,bs="re"),
                               ~ s(encounter,bs="re")),
           data = callcat_total, family = multinom(K=2), method = "REML", optimizer = "efs")

mn1 <- gam(list(call_category2 ~ behavior + s(encounter,bs="re"),
                               ~ behavior + s(encounter,bs="re")),
           data = callcat_total, family = multinom(K=2), method = "REML", optimizer = "efs")

mn2 <- gam(list(call_category2 ~ behavior + calf_presence + s(encounter,bs="re"),
                               ~ behavior + calf_presence + s(encounter,bs="re")),
           data = callcat_total, family = multinom(K=2), method = "REML", optimizer = "efs")

mn3 <- gam(list(call_category2 ~ behavior + calf_presence + group_size + s(encounter,bs="re"),
                               ~ behavior + calf_presence + group_size + s(encounter,bs="re")),
           data = callcat_total, family = multinom(K=2), method = "REML", optimizer = "efs")

mn4 <- gam(list(call_category2 ~ behavior + calf_presence + group_size + tide + s(encounter,bs="re"),
                               ~ behavior + calf_presence + group_size + tide + s(encounter,bs="re")),
           data = callcat_total, family = multinom(K=2), method = "REML", optimizer = "efs")

mn5 <- gam(list(call_category2 ~ calf_presence + s(encounter,bs="re"),
                               ~ calf_presence + s(encounter,bs="re")),
           data = callcat_total, family = multinom(K=2), method = "REML", optimizer = "efs")

mn6 <- gam(list(call_category2 ~ calf_presence + group_size + s(encounter,bs="re"),
                               ~ calf_presence + group_size + s(encounter,bs="re")),
           data = callcat_total, family = multinom(K=2), method = "REML", optimizer = "efs")

mn7 <- gam(list(call_category2 ~ calf_presence + group_size + tide + s(encounter,bs="re"),
                               ~ calf_presence + group_size + tide + s(encounter,bs="re")),
           data = callcat_total, family = multinom(K=2), method = "REML", optimizer = "efs")

mn8 <- gam(list(call_category2 ~ calf_presence + tide + s(encounter,bs="re"),
                               ~ calf_presence + tide + s(encounter,bs="re")),
           data = callcat_total, family = multinom(K=2), method = "REML", optimizer = "efs")

mn9 <- gam(list(call_category2 ~ group_size + s(encounter,bs="re"),
                               ~ group_size + s(encounter,bs="re")),
           data = callcat_total, family = multinom(K=2), method = "REML", optimizer = "efs")

mn10 <- gam(list(call_category2 ~ group_size + tide + s(encounter,bs="re"),
                                ~ group_size + tide + s(encounter,bs="re")),
           data = callcat_total, family = multinom(K=2), method = "REML", optimizer = "efs")

mn11 <- gam(list(call_category2 ~ group_size + behavior + s(encounter,bs="re"),
                                ~ group_size + behavior + s(encounter,bs="re")),
           data = callcat_total, family = multinom(K=2), method = "REML", optimizer = "efs")

mn12 <- gam(list(call_category2 ~ group_size + behavior + tide + s(encounter,bs="re"),
                                ~ group_size + behavior + tide + s(encounter,bs="re")),
           data = callcat_total, family = multinom(K=2), method = "REML", optimizer = "efs")

mn13 <- gam(list(call_category2 ~ tide + s(encounter,bs="re"),
                                ~ tide + s(encounter,bs="re")),
            data = callcat_total, family = multinom(K=2), method = "REML", optimizer = "efs")

mn14 <- gam(list(call_category2 ~ tide + behavior + s(encounter,bs="re"),
                                ~ tide + behavior + s(encounter,bs="re")),
            data = callcat_total, family = multinom(K=2), method = "REML", optimizer = "efs")

mn15 <- gam(list(call_category2 ~ tide + behavior + calf_presence + s(encounter,bs="re"),
                                ~ tide + behavior + calf_presence + s(encounter,bs="re")),
            data = callcat_total, family = multinom(K=2), method = "REML", optimizer = "efs")



#model selection
AIC(mn0,mn1,mn2,mn3,mn4,mn5,mn6,mn7,mn8,mn9,mn10,mn11,mn12,mn13,mn14,mn15)  

#best models
AIC(mn2,mn3,mn4,mn15) 
#mn4 is best which is the full model
#####################################################################




#model summary
summary(mn4)

#model summary plots
plot(parameters(mn4))

##confidence intervals 
#smoothed parameter (group size)
ci.group <- confint(mn.full,parm="s(group_size)",level=0.95)

ggplot(ci.group, aes(x=group_size, y=.estimate))+
  geom_line() +
  geom_ribbon(aes(ymin=.lower_ci, ymax=.upper_ci),linetype=2, alpha=0.2) +
  theme_classic() +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0))

#Manually calculate 95% CI for non-smoothed variables (Coef +/- 1.96 * SE).
#cc for behavior-travel
-4.90850 + 1.96*2.36445
-4.90850 - 1.96*2.36445

#cc for calf presence-yes
27.02764 + 1.96*112.49524
27.02764 - 1.96*112.49524

#cc for tide-flood
4.25866 + 1.96*3.28930
4.25866 - 1.96*3.28930

#pc for behavior-travel
0.03963 + 1.96*0.37791
0.03963 - 1.96*0.37791

#pc for calf presence-yes
-2.07098 + 1.96*0.87053
-2.07098 - 1.96*0.87053

#pc for tide-flood
-0.17028 + 1.96*1.80379
-0.17028 - 1.96*1.80379


#### Model-diagnostics- residual plots 
plot(mn.full, pages=1, all.terms = TRUE, rug=FALSE, residuals=TRUE, shade=TRUE, shift=coef(mn.full)[1])

#plots of just smoothed terms with partial residual points in blue
draw(mn.full, residuals=TRUE)

#check k values
gam.check(mn4)

#model diagnostic plots
appraise(mn4)

#examining qq plot further
qq.gam(mn.full,pch=1)

#examining residuals- Dave said all these look good!
E <- residuals(mn4)
F <- fitted(mn4)

callcat_total$cat_group_size <- cut(callcat_total$group_size, seq(0, 60, by=10))
plot(callcat_total$cat_group_size,E, xlab="Group size",ylab="Residuals")
plot(callcat_total$tide,E, xlab="Tide", ylab="Residuals")
plot(callcat_total$calf_presence,E, xlab="Calf presence", ylab="Residuals")
plot(callcat_total$behavior,E, xlab="Behavior", ylab="Residuals")
plot(callcat_total$encounter,E, xlab="Encounter", ylab="Residuals")



#### Predictions (1=ws, 2=cc, 3=pc)    #ASK SARAH ABOUT THIS
preds <- predict(mn.full, type="response")
boxplot(preds, type="response")

#Dave added this, not sure what it's showing?
preds <- apply(preds, 1, which.max)
plot(mn.full$y, preds-1, xlab="observed", ylab="highest probability category")
abline(a=0,b=1, col="red")



## predictions 
preds <- predict(mn4, type="response", se=TRUE)
head(preds)

#model matrix?
model.matrix.gam(mn4)

library(ggeffects)  #can't get to work with gam multinomial????????
#predictions by all variables
pred <- predict_response(mn.full,terms=c("behaviorTravel","calf_presenceyes","tideFlood","group_size"))

plot(pred)

#predictions by focal variable
#behavior
predict_response(mn.full,terms="behavior")
plot(predict_response(mn.full,terms="behavior"))

#calf presence
predict_response(mn.full,terms="calf_presence")
plot(predict_response(mn.full,terms="calf_presence"))

#group size
predict_response(mn.full,terms="group_size")
plot(predict_response(mn.full,terms="group_size"))





















###############################
#switch pc and cc to test model
callcat_total <-  callcat_total %>% 
  mutate(call_category = fct_relevel(call_category,c("ws","pc","cc")))


######################################
#remove cc to test binomial with ws and pc
#test <- callcat_total %>% 
      filter(call_category==c('ws','pc'))
  
#not showing the correct amount of data rows


#manually removed cc
test <- read_csv("callcat_total_ws_pc.csv") %>% 
  mutate(behavior=as.factor(behavior),
         calf_presence=as.factor(calf_presence),
         tide=as.factor(tide),
         encounter=as.factor(encounter),
         group_size=as.integer(group_size),
         call_category=as.factor(call_category))

levels(test$call_category)

#relevel so ws is reference
test$call_category <- relevel(test$call_category,ref = "ws")
levels(test$call_category)
test$call_category2 <- as.numeric(test$call_category)-1
all(test$call_category2 %in% c(0L, 1L))

#remove one category to make binomial
binom_model <- gam(call_category ~ behavior + s(group_size) + calf_presence + tide + s(encounter,bs="re"),
               data = test,
               family = binomial(link="logit"),
               method = "REML",
               optimizer = "efs")

summary(binom_model)


#manually removed pc
test2 <- read_csv("callcat_total_ws_cc.csv") %>% 
  mutate(behavior=as.factor(behavior),
         calf_presence=as.factor(calf_presence),
         tide=as.factor(tide),
         encounter=as.factor(encounter),
         group_size=as.integer(group_size),
         call_category=as.factor(call_category))

levels(test2$call_category)

#relevel so ws is reference
test2$call_category <- relevel(test2$call_category,ref = "ws")
levels(test2$call_category)
test2$call_category2 <- as.numeric(test2$call_category)-1
all(test2$call_category2 %in% c(0L, 1L))

#remove one category to make binomial
binom_model2 <- gam(call_category ~ behavior + s(group_size) + calf_presence + tide + s(encounter,bs="re"),
               data = test2,
               family = binomial(link="logit"),
               method = "REML",
               optimizer = "efs")

summary(binom_model2)




