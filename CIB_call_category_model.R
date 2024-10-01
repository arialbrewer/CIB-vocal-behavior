#Arial Brewer
#PhD- Chapter 2 Vocal Behavior
#Model: call category ~ behavior + group size + calf presence + tide + (1|encounter)

#load packages
library(tidyverse)
library(corrplot)
library(mgcv)
library(mgcViz)
library(performance)
library(parameters)
library(see)
library(gratia)
library(MASS)
library(lme4)
library(DHARMa)
library(MuMIn)

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
#GAM with multinomial distribution using mgcv package

#default levels are cc,pc,ws
levels(callcat_total$call_category)

#relevel so ws is level 0 (reference level)
callcat_total$call_category <- relevel(callcat_total$call_category,ref = "ws")
callcat_total$call_category2 <- as.numeric(callcat_total$call_category)-1
levels(callcat_total$call_category)

#categories must be coded 0 to K
all(callcat_total$call_category2 %in% c(0L, 1L, 2L))

##K=number of levels of response-1. Because there we have K=2, we repeat the formula twice within a list

#manual model selection 
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
#mn4 (full model) is best 

#model summary
summary(mn4)

##calculate 95% CI= (Coef +/- 1.96 * SE).
#cc for behavior-travel
-2.469e+00 + 1.96*5.634e-01
-2.469e+00 - 1.96*5.634e-01

#cc for calf presence-yes
2.174e+01 + 1.96*1.028e+05
2.174e+01 - 1.96*1.028e+05

#cc for group size
5.079e-02 + 1.96*3.980e-02
5.079e-02 - 1.96*3.980e-02

#cc for tide-flood
2.473e-01 + 1.96*1.540e+00
2.473e-01 - 1.96*1.540e+00

#pc for behavior-travel
-9.265e-01 + 1.96*1.969e-01
-9.265e-01 - 1.96*1.969e-01

#pc for calf presence-yes
-2.674e+00 + 1.96*4.562e-01
-2.674e+00 - 1.96*4.562e-01

#pc for group size
2.885e-03 + 1.96*5.103e-03
2.885e-03 - 1.96*5.103e-03

#pc for tide-flood
-9.783e-01 + 1.96*1.339e+00
-9.783e-01 - 1.96*1.339e+00


###plot model coefficients, CI and significance 
#without CC-calf presence (masks other variable CI)
#new dataframe with model coefficients and CI
model_summ <- data.frame(variable=c("CC-Behavior [trave]","CC-Group size","CC-Tide [flood]", 
                                    "PC-Behavior [travel]","PC-Calf presence [yes]","PC-Group size","PC-Tide [flood]"),
                         coefficient=c(-2.469,0.051,0.247,-0.927,-2.674,0.003,-0.978),
                         lower=c(-3.573,-0.027,-2.771,-1.312,-3.568,-0.007,-3.603),
                         upper=c(-1.365,0.129,3.266,-0.541,-1.78,0.013,1.646),
                         sig=c("yes","no","no","yes","yes","no","no"))


#reorder variables so cc is first                   
# #mn4.summ %>% mutate(variable=factor(variable, levels=c("CC-Behavior-travel", "CC-Group size", "CC-Tide-flood", 
#                                                   "PC-Behavior-travel", "PC-Calf presence-yes",
#                                                   "PC-Group size", "PC-Tide-flood")))

ggplot(data=model_summ, aes(x=coefficient, y=variable, color=sig)) +
  geom_point() +
  geom_pointrange(aes(xmin=lower,xmax=upper)) +
  geom_vline(xintercept=0,lty=2) +
  theme_classic() +
  scale_x_continuous(breaks=seq(-4,4,by=1)) +
  labs(x="Coefficient", y=" Variable", color="Significant")

#new dataframe with CC-calf presence only
model_summ_cc.calf <- data.frame(variable="CC-Calf presence [yes]",
                         coefficient=21.74,
                         lower=-201466.3,
                         upper=201509.7,
                         sig="no")

ggplot(data=model_summ_cc.calf, aes(x=coefficient, y=variable,color=sig)) +
  geom_point() +
  geom_pointrange(aes(xmin=lower,xmax=upper)) +
  geom_vline(xintercept=0,lty=2) +
  theme_classic() +
  labs(x="Coefficient", y=" Variable", color="Significant") 


#### Model-diagnostics
plot(mn4, pages=1, all.terms = TRUE, rug=FALSE, residuals=TRUE, shade=TRUE, shift=coef(mn4)[1])

#examining qq plot further
qq.gam(mn4,pch=1)

#examining residuals
E <- residuals(mn4)

#group size
callcat_total$cat_group_size <- cut(callcat_total$group_size, seq(0, 60, by=10))
plot(callcat_total$cat_group_size, E, xlab="Group size",ylab="Residuals")

#behavior
plot(callcat_total$behavior, E, xlab="Behavior", ylab="Residuals")

#calf presence
plot(callcat_total$calf_presence, E, xlab="Calf presence", ylab="Residuals")

#tide
plot(callcat_total$tide, E, xlab="Tide", ylab="Residuals")

#encounter
plot(callcat_total$encounter, E, xlab="Encounter", ylab="Residuals")



# #### Predictions (1=ws, 2=cc, 3=pc)    
# preds <- predict(mn4, type="response")
# head(preds)
# boxplot(preds, type="response")



###############################################################################
###remove cc to test binomial with ws and pc
#read in data with just ws and pc
callcat_binom <- read.csv("call_cat_ws_pc.csv") %>% 
    mutate(behavior=as.factor(behavior),
           calf_presence=as.factor(calf_presence),
           tide=as.factor(tide),
           encounter=as.factor(encounter),
           group_size=as.integer(group_size),
           call_category=as.factor(call_category))

levels(callcat_binom$call_category)

#relevel so ws is reference
callcat_binom$call_category <- relevel(callcat_binom$call_category,ref = "ws")
#check levels
levels(callcat_binom$call_category)

#call_cat_binom$call_category2 <- as.numeric(call_cat_binom$call_category)-1
#all(call_cat_binom$call_category2 %in% c(0L, 1L))


#full binomial model
binom_model <- glmer(call_category ~ behavior + group_size + calf_presence + tide + (1|encounter),
                      data=callcat_binom,
                      family="binomial",na.action="na.fail")

summary(binom_model)


#model selection on fixed effects
binom0 <- glmer(call_category ~ (1|encounter),
           data = callcat_binom, family="binomial")

binom1 <- glmer(call_category ~ behavior + (1|encounter),
           data = callcat_binom, family="binomial")

binom2 <- glmer(call_category ~ behavior + calf_presence + (1|encounter),
           data = callcat_binom, family="binomial")

binom3 <- glmer(call_category ~ behavior + calf_presence + group_size + (1|encounter),
           data = callcat_binom, family="binomial")

binom4 <- glmer(call_category ~ behavior + calf_presence + group_size + tide + (1|encounter),
           data = callcat_binom, family="binomial")

binom5 <- glmer(call_category ~ calf_presence + (1|encounter),
           data = callcat_binom, family="binomial")

binom6 <- glmer(call_category ~ calf_presence + group_size + (1|encounter),
           data = callcat_binom, family="binomial")

binom7 <- glmer(call_category ~ calf_presence + group_size + tide + (1|encounter),
           data = callcat_binom, family="binomial")

binom8 <- glmer(call_category ~ calf_presence + tide + (1|encounter),
           data = callcat_binom, family="binomial")

binom9 <- glmer(call_category ~ group_size + (1|encounter),
           data = callcat_binom, family="binomial")

binom10 <- glmer(call_category ~ group_size + tide + (1|encounter),
            data = callcat_binom, family="binomial")

binom11 <- glmer(call_category ~ group_size + behavior + (1|encounter),
            data = callcat_binom, family="binomial")

binom12 <- glmer(call_category ~ group_size + behavior + tide + (1|encounter),
            data = callcat_binom, family="binomial")

binom13 <- glmer(call_category ~ tide + (1|encounter),
            data = callcat_binom, family="binomial")

binom14 <- glmer(call_category ~ tide + behavior + (1|encounter),
            data = callcat_binom, family="binomial")

binom15 <- glmer(call_category ~ tide + behavior + calf_presence + (1|encounter),
            data = callcat_binom, family="binomial")


AIC(binom0,binom1,binom2,binom3,binom4,binom5,binom6,binom7,binom8,binom9,binom10,binom11,binom12,binom13,binom14,binom15)  
#binom2 best model (behavior,calf)

#model selection with dredge
dredge(binom_model) #same result as AIC (behavior,calf)

summary(binom2)
plot(parameters(binom2))

#check overdispersion with performance package
check_overdispersion(binom2)

#check overdispersion parameter manually (X2/df.resid) Overdispersed > 1
sum(residuals(binom2,type="pearson")^2)/1672   #not overdispersed

#check residuals
simulateResiduals(fittedModel = binom2, plot = T)

#95% confidence intervals
confint(binom2, level=0.95)


###examining residuals
E <- residuals(binom2)

#behavior
plot(callcat_binom$behavior, E, xlab="Behavior", ylab="Residuals")

#calf presence
plot(callcat_binom$calf_presence, E, xlab="Calf presence", ylab="Residuals")



