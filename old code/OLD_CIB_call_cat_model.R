################ OLD options without random effects
#multinom package- DOESN'T WORK WITH RANDOM EFFECTS 
library(nnet) 

#multinom needs reference level- make ws the reference level
callcat_total$call_category <- as.factor(callcat_total$call_category)
callcat_total$call_category <- relevel(callcat_total$call_category,ref = "ws")
levels(callcat_total$call_category)

#Model selection
m0 <- multinom(call_category ~ behavior + (1 | encounter), 
               data = callcat_total)

m1 <- multinom(call_category ~ behavior + group_size + (1 | encounter), 
               data = callcat_total)

m2 <- multinom(call_category ~ behavior + group_size + calf_presence + (1 | encounter), 
               data = callcat_total)

m3 <- multinom(call_category ~ behavior + group_size + calf_presence + tide + (1 | encounter), 
               data = callcat_total)

#no random effect
m3.noRE <- multinom(call_category ~ behavior + group_size + calf_presence + tide, 
                    data = callcat_total)

#AIC
AIC(m3,m4,m3.noRE)
anova(m3,m3.noRE)


#my model
multi_model <- multinom(call_category ~ behavior + group_size + calf_presence + tide + (1 | encounter), 
                        data = callcat_total)

summary(multi_model)
plot(parameters(multi_model))


#Manually calculate 95% confidence intervals (Coef +/- 1.96 * SE). Sig if interval doesn't include 0
#cc for behavior-travel
-2.671946 + 1.96*0.4834784
-2.671946 - 1.96*0.4834784

#cc for group size
0.008579445  + 1.96*0.015946047 
0.008579445  - 1.96*0.015946047 

#cc for calf presence-yes
4.898329 + 1.96*0.1096456
4.898329 - 1.96*0.1096456

#cc for tide-flood
-0.5666855 + 1.96*0.3595537 
-0.5666855 - 1.96*0.3595537 

#cc random effect of encounter
-3.2754491 + 1.96*0.1096733
-3.2754491 - 1.96*0.1096733

#pc for behavior-travel
-1.405756 + 1.96*0.1382587
-1.405756 - 1.96*0.1382587

#pc for group size
0.016994715  + 1.96*0.003287415 
0.016994715  - 1.96*0.003287415 

#pc for calf presence-yes
-1.306898 + 1.96*0.4009730
-1.306898 - 1.96*0.4009730

#pc for tide-flood
-0.8791619 + 1.96*0.1448903 
-0.8791619 - 1.96*0.1448903 

#pc random effect of encounter
0.9303281 + 1.96*0.2022982
0.9303281 - 1.96*0.2022982

#multinom delivers the coefficients and standard errors of the two call categories 
#against reference category.To determine whether specific input variables are 
#significant we need to calculate p-values of the coefficients manually by calculating 
#the z-statistics and converting.

#calculate z-statistics of coefficients
z_stats <- summary(multi_model)$coefficients/summary(multi_model)$standard.errors

#convert to p-values
p_values <- (1-pnorm(abs(z_stats)))*2

#display p-values in transposed data frame
data.frame(t(p_values))

#Odds/ odds ratio [exp(coef)]
#the larger the number, the more likely event is to be found.
odds_ratios <- exp(summary(multi_model)$coefficients)
odds_ratios

#display odds ratios in transposed data frame
data.frame(t(odds_ratios))

#Percentage [(exp(coef)-1)*100]
percent <- (exp(summary(multi_model)$coefficients)-1)*100
percent

#predicted probabilities?????
pp <- fitted(multi_model)
head(pp)

#examining residuals
E <- resid(multi_model)
F <- fitted(multi_model)
plot(F,E, xlab="Fitted values", ylab="Residuals")
hist(E, xlab="Residuals", main="")
plot(callcat_total$tide, E, xlab="tide", ylab="Residuals")
plot(callcat_total$group_size,E, xlab="group size")
plot(callcat_total$calf_presence,E, xlab="calf presence", ylab="Residuals")
plot(callcat_total$behavior,E, xlab="behavior", ylab="Residuals")



############################## other options for random effect
#brms
library(brms)
model <- brm(call_category ~ behavior + group_size + calf_presence + tide + (1 | encounter),
             data = callcat_total, family = "multinomial", 
             prior = c(prior(normal(0, 10), class = Intercept),
                       prior(normal(0, 10), class = b), 
                       prior(exponential(1), class = sd)),
             iter = 100, chains = 4)


#mclogit
library(mclogit)
model <- mclogit(cbind(call_category,test) ~ behavior + group_size + calf_presence + tide,
                 random = ~ 1 | encounter,  
                 data = callcat_total)

summary(model)


mclogit.fit(model)
dispersion(model)


