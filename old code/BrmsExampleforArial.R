#Brm example code from Fetal Sex Ratio model
#Zoe Rand
#10/12/23

library(tidyverse)
library(brms)

options(mc.cores = parallel::detectCores()) #this is so you can run chains in parallel
options(brms.backend = "cmdstanr") #you don't need this, but if you want to switch which type of Stan it's using this is how

#Model code
#this is a betabinomial model with the response as number of males (I also give it the totla number of fetuses)
#binned_center_sc is my fixed covariate
#random effects of slope and intercept of species
#if you just want random intercept you can get rid of the fixed covariate name in the parentheses:
#+(1|SpName)
#brms will automatically pick priors for you, but I wanted to change some of the priors so I added them in manually
#the warmup is the Stan equivalent of burn-in (you won't get the results from these back)
#the iter is total number of iterations (it needs to be bigger than warmup)
#the control parameters control the acceptance parameter (adapt delta) and the treedepth which are Stan-specific parameters
#you don't need to change these unless you get divergent transitions or warnings about hitting the max tree depth
Mod1_AllRE<-brm(Nmale | trials(Total) ~ binned_center_sc + (1+binned_center_sc|SpName), 
                data = Binned3Iinches, family = "beta_binomial", 
                prior = c(prior(normal(0, 10), class = Intercept),
                          prior(normal(0, 10), class = b), 
                          prior(exponential(1), class = sd)),
                warmup = 1000, 
                iter   = 2000, 
                chains = 4, control = list(adapt_delta = 0.95))


#summary gets you the parameter estimates (like you'd ge in lme4)
summary(Mod1_AllRE)

#to see the priors that it chose for you (and make sure any you inputted were correct)
#this can also give you the "class" names for inputting your own priors
Mod1_AllRE$prior 

#generic posteriors and traceplots
plot(Mod1_AllRE)

#you can also use whatever bayesian plotting package you want
#I use: bayesplot and also some tidybayes for manipulating the chains
library(tidybayes)
library(bayesplot)

#you can use tidybayes to get the variable names (which helps with subsetting for plotting/manipulating)
tidybayes::get_variables(Mod1_AllRE)

#here's an example plot of my species specific slopes
pal<-c('#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854','#ffd92f','#e5c494','#b3b3b3')

GroupSlopes_allRE<-Mod1_AllRE %>% spread_draws(b_binned_center_sc, r_SpName[c,t]) %>% mutate(Species = gsub("\\.", replacement = " ", c)) %>%
  filter(t == "binned_center_sc") %>%
  ggplot() +
  stat_halfeye(aes(y = Species, x = r_SpName, fill = Species), alpha = 0.8) + theme_classic() +  labs(x = "Species slope", y = "Species") + 
  geom_vline(aes(xintercept = 0.0), linetype = "dashed") +
  scale_fill_manual(values = pal, drop = TRUE) + 
  scale_y_discrete(expand = c(0, 0.03), limits = c("Antarctic blue", "Pygmy blue", "Fin",   
                                                   "Humpback","Sei", "Antarctic minke",
                                                   "Common minke", "Brydes"), 
                   labels = c("Antarctic blue", "Pygmy blue", "Fin",   
                              "Humpback","Sei", "Antarctic minke",
                              "Common minke", "Brydes")) + 
  theme(legend.position = "none", axis.title.y = element_blank()) + xlim(c(-0.08, 0.08))


GroupSlopes_allRE

#here's how I got the probability of a slope being negative
ndraws<-4*1000

#probability slope is negative
Mod1_AllRE %>% spread_draws(b_binned_center_sc) %>% mutate(neg = ifelse(b_binned_center_sc < 0, 1, 0)) %>%
  summarise(TotNeg = sum(neg), ProbNeg = TotNeg/ndraws)