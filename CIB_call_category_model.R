#Arial Brewer
#PhD Chapter 2: CIB Vocal Behavior- call category model

#load packages
library(tidyverse)
library(corrplot)
library(mgcv)
library(mgcViz)
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
  dplyr::select(-sample_round,-group_number,-dot,-count_white,-count_gray,-count_calf,-comments)

#combine behavioral & acoustic data and remove NAs
callcat_total <- behavior_data %>% 
  left_join(acoustic_data, by = c("date","time"), multiple = "all") %>% 
  na.omit() %>% 
  mutate(behavior=as.factor(behavior),
        calf_presence=as.factor(calf_presence),
        tide=as.factor(tide),
        encounter=as.factor(encounter),
        group_size=as.integer(group_size),
        call_category=as.factor(call_category))


###Test for correlation between variables
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


########################################################
###Exploratory plots
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
ggplot(data=callcat_total, aes(x=group_size)) +
  geom_histogram(bins=50,fill="cyan4",color="grey",alpha=0.9) +
  theme_classic() +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,500,by=50)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0,60,by=5)) +
  labs(x="Group size",y="Count") 


###dependent variableExplorat
#call category distribution
ggplot(data=callcat_type, aes(x="", y=number,fill=call_category)) +
  geom_bar(stat='identity',width=1, color='white')+
  coord_polar("y",start=0)+
  theme_void() + 
  geom_label(aes(label = labels), size = 8,
             position = position_stack(vjust = 0.5), show.legend = F) +
  labs(fill="Call category") +
  theme(text=element_text(family="sans", size=15)) +
  scale_fill_manual(values=c("gold2","darkseagreen","cyan4")) 


###summarize call categories by variables
#behavior
callcat_behavior <- callcat_total %>% 
  group_by(behavior,call_category) %>% 
  summarize(number=n())

ggplot(data=callcat_behavior, aes(x=behavior,y=number,fill=call_category)) + 
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  labs(x="Behavior", y="Number",fill="Call category") +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_manual(values=c("gold2","darkseagreen","cyan4"))

#calf presence
callcat_calf <- callcat_total %>% 
  group_by(calf_presence,call_category) %>% 
  summarize(number=n())

ggplot(data=callcat_calf, aes(x=calf_presence,y=number,fill=call_category)) + 
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  labs(x="Calf presence", y="Number", fill="Call category") +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_manual(values=c("gold2","darkseagreen","cyan4"))

#tide
callcat_tide <- callcat_total %>% 
  group_by(tide,call_category) %>% 
  summarize(number=n())

ggplot(data=callcat_tide, aes(x=tide,y=number,fill=call_category)) + 
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  labs(x="Tide", y="Number", fill="Call category") +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_manual(values=c("gold2","darkseagreen","cyan4"))

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
  scale_fill_manual(values=c("gold2","darkseagreen","cyan4"))

###############################################################



####################################### Model building
#Multinomial GLMM using mgcv package

#default levels are cc,pc,ws
levels(callcat_total$call_category)

#relevel so ws is level 0 (reference level)
callcat_total$call_category <- relevel(callcat_total$call_category,ref = "ws")
callcat_total$call_category2 <- as.numeric(callcat_total$call_category)-1
levels(callcat_total$call_category)

#check levels of fixed effects
levels(callcat_total$behavior)
levels(callcat_total$calf_presence)
levels(callcat_total$tide)


#categories must be coded 0 to K
all(callcat_total$call_category2 %in% c(0L, 1L, 2L))

##K=number of levels of response-1. Because we have K=2, we repeat the formula twice within a list
#model selection 
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


###plot model coefficients and CI (without CC-calf presence because parameter is unidentifiable)
#create new dataframe with model coefficients and CI for cc and pc
model_summ <- data.frame(variable=c("CC-Behavior [travel]","CC-Group size","CC-Tide [flood]", 
                                    "PC-Behavior [travel]","PC-Calf presence [yes]","PC-Group size","PC-Tide [flood]"),
                         coefficient=c(-2.469,0.051,0.247,-0.927,-2.674,0.003,-0.978),
                         lower=c(-3.573,-0.027,-2.771,-1.312,-3.568,-0.007,-3.603),
                         upper=c(-1.365,0.129,3.266,-0.541,-1.78,0.013,1.646),
                         sig=c("yes","no","no","yes","yes","no","no")) %>% 
   mutate(variable=as.factor(variable))

#relevel so cc is first
model_summ %>% 
  mutate(variable=fct_relevel(variable,"CC-Behavior [travel]","CC-Group size","CC-Tide [flood]", 
                              "PC-Behavior [travel]","PC-Calf presence [yes]","PC-Group size","PC-Tide [flood]")) %>% 
ggplot(aes(x=coefficient, y=variable, color=sig)) +
  geom_point() +
  geom_pointrange(aes(xmin=lower,xmax=upper)) +
  geom_vline(xintercept=0,lty=2) +
  theme_classic() +
  scale_x_continuous(breaks=seq(-4,4,by=1)) +
  labs(x="Coefficient", y=" Variable", color="Significant") +
  scale_color_manual(values=c("red","deepskyblue4"))


###cc only
cc_summ <- data.frame(variable=c("Behavior","Group size","Tide"),
                         coefficient=c(-2.469,0.051,0.247),
                         lower=c(-3.573,-0.027,-2.771),
                         upper=c(-1.365,0.129,3.266),
                         sig=c("yes","no","no")) %>% 
  mutate(variable=as.factor(variable))

#couldn't get behavior to be first so reversed order and will manually change level labels
ggplot(data=cc_summ,aes(x=coefficient, y=rev(variable), color=sig)) +
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


###pc only
pc_summ <- data.frame(variable=c("Behavior","Calf presence","Group size","Tide"),
                      coefficient=c(-0.927,-2.674,0.003,-0.978),
                      lower=c(-1.312,-3.568,-0.007,-3.603),
                      upper=c(-0.541,-1.78,0.013,1.646),
                      sig=c("yes","yes","no","no")) %>% 
  mutate(variable=as.factor(variable))

#couldn't get behavior to be first so reversed order and will manually change level labels
ggplot(data=pc_summ,aes(x=coefficient, y=rev(variable), color=sig)) +
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


#####calculating odds percentage from coefficients- [(exp(coef)-1)*100]
###Combined calls
#behavior (travel)
(exp(-2.469)-1)*100

#group size
(exp(0.051)-1)*100

#tide (flood)
(exp(0.247)-1)*100


###Pulsed calls
#behavior (travel)
(exp(-0.927)-1)*100

#calf presence (yes)
(exp(-2.674)-1)*100

#group size
(exp(0.003)-1)*100

#tide (flood)
(exp(-0.978)-1)*100


#### Model-diagnostics
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



################ Predictions (1=ws, 2=cc, 3=pc)    
#behavior
pred.behav <- plot_predictions(mn4,condition="behavior",vcov=TRUE,draw=FALSE)

ggplot(pred.behav, aes(x = behavior)) +
  facet_wrap(~group) +
  geom_pointrange(aes(y=estimate, ymin=conf.low, ymax=conf.high), lwd=1, size=1) +
  theme_classic() +
  labs(x="Behavior", y="Probability") +
  theme(text=element_text(family="sans", size=20),
        axis.text = element_text(size=24),
        axis.ticks.length = unit(0.4,"cm"),
        axis.line=element_line(colour='black', size=1),
        panel.spacing = unit(0.3,"cm")) +
  ylim(0,1)


#calf presence
pred.calf <- plot_predictions(mn4,condition="calf_presence",vcov=TRUE,draw=FALSE)

ggplot(pred.calf, aes(x = calf_presence)) +
  facet_wrap(~group) +
  geom_pointrange(aes(y=estimate, ymin=conf.low, ymax=conf.high), lwd=1, size=1) +
  theme_classic() +
  labs(x="Calf presence", y="Probability") +
  theme(text=element_text(family="sans", size=20),
        axis.text = element_text(size=24),
        axis.ticks.length = unit(0.4,"cm"),
        axis.line=element_line(colour='black', size=1),
        panel.spacing = unit(0.3,"cm"))





###########Switching reference level for behavior & calf so can see effect on mill & no calf

#relevel so ws is level 0 (reference level)
callcat_total$call_category <- relevel(callcat_total$call_category,ref = "ws")
callcat_total$call_category2 <- as.numeric(callcat_total$call_category)-1
levels(callcat_total$call_category)

#categories must be coded 0 to K
all(callcat_total$call_category2 %in% c(0L, 1L, 2L))

#relevel so travel is reference
callcat_total$behavior <- relevel(callcat_total$behavior,ref = "Travel")
levels(callcat_total$behavior)

#relevel so no calf is reference
callcat_total$calf_presence <- relevel(callcat_total$calf_presence,ref = "yes")
levels(callcat_total$calf_presence)

#relevel so flood is reference
callcat_total$tide <- relevel(callcat_total$tide,ref = "Flood")
levels(callcat_total$tide)


#re-run model
mn4.flip <- gam(list(call_category2 ~ behavior + calf_presence + group_size + tide + s(encounter,bs="re"),
                               ~ behavior + calf_presence + group_size + tide + s(encounter,bs="re")),
           data = callcat_total, family = multinom(K=2), method = "REML", optimizer = "efs")

#model summary
summary(mn4.flip)


##calculate 95% CI= (Coef +/- 1.96 * SE).
#cc for behavior-mill
2.470e+00 + 1.96*5.636e-01
2.470e+00 - 1.96*5.636e-01

#cc for calf presence-no
-5.695e+02 + 1.96*9.082e+123
-5.695e+02 - 1.96*9.082e+123

#cc for group size
5.098e-02 + 1.96*3.997e-02
5.098e-02 - 1.96*3.997e-02

#cc for tide-ebb
-2.504e-01 + 1.96*1.548e+00
-2.504e-01 - 1.96*1.548e+00

#pc for behavior-mill
9.303e-01  + 1.96*1.964e-01 
9.303e-01  - 1.96*1.964e-01 

#pc for calf presence-no
2.672e+00 + 1.96*4.553e-01 
2.672e+00 - 1.96*4.553e-01 

#pc for group size
2.907e-03 + 1.96*5.098e-03
2.907e-03 - 1.96*5.098e-03

#pc for tide-ebb
9.668e-01 + 1.96*1.212e+00
9.668e-01 - 1.96*1.212e+00



#####calculating odds percentage from coefficients- [(exp(coef)-1)*100]
###Combined calls
#behavior (mill)
(exp(2.469)-1)*100

#group size
(exp(0.051)-1)*100

#tide (ebb)
(exp(-0.25)-1)*100


###Pulsed calls
#behavior (mill)
(exp(0.930)-1)*100

#calf presence (no)
(exp(2.672)-1)*100

#group size
(exp(0.003)-1)*100

#tide (ebb)
(exp(0.967)-1)*100






