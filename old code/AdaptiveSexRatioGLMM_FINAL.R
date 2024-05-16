#Bayesian GLMM Mother Length Fetal Sex
#Zoe Rand
#11/22/23


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(brms)
library(bayesplot)
library(patchwork)
library(tidybayes)
options(mc.cores = parallel::detectCores())
options(brms.backend = "cmdstanr")


# Fetal sex correction ----------------------------------------------------
#NOTE NEEDS TO BE UPDATED (11/22/23)
BayesRes<-read_csv("Code/Results/MisIDBayesResults.csv")
SeiRes<-read_csv("Code/Results/seiBayesResults.csv")
CmmRes<-read_csv("Code/Results/cmmBayesResults.csv")

CmmRes<- CmmRes %>% add_column("Sp" = rep("Common Minke Whale", nrow(CmmRes)), .before = "Model")

BayesRes_v2<-BayesRes %>% filter(Sp != "Sei Whale") %>% filter(Sp != "Common Minke Whale") %>% 
  bind_rows(SeiRes) %>% bind_rows(CmmRes)

BestMod<-BayesRes_v2 %>% filter(DeltaWAIC == 0) %>% filter(Sp != "Gray Whale") %>% filter(Sp != "Right Whale")
#above is reliable for which ones need small corrections, but may need another strategy to choose model for different purpose
#Gray and Right do not have enough data to model
#pygmy blue whales have too few small fetuses to model the correction, but are going to be included in this model, will use minimum length from ABW--which is likely conservative since Pygmy blue whales are smaller
#best model for Cmm minke did not use the correction, but some evidence for a correction needed in the data, so using Antarctic minke whale length minimum


# Data --------------------------------------------------------------------

#from MotherLenghtFetalSexDataSummary.R
MLenFDat<-read_csv("Data/MotherLengthFetalsex_62923.csv")
table(MLenFDat$SpName)

#removing species with too small sample sizes:
MLenFDat2<-MLenFDat %>% 
  filter(SpName %in% c("Sperm", "Humpback","Fin","Sei","Pygmy blue","Brydes","Antarctic minke", 
                       "Common minke", "Antarctic blue"))
table(MLenFDat2$SpName)

#removing sperm whales because not rorquals
Roq_MLen<-MLenFDat2 %>% filter(SpName != "Sperm")

#adding Male 1 0 column

Roq_MLen<-Roq_MLen %>% mutate(Male = ifelse(F_S == 1, 1, 0)) #males are coded as 1s and females as 2s
head(Roq_MLen)


# Removing small fetuses --------------------------------------------------

#function to calculate minimum length (from solving logistic selectivity equation for L)
MinLength<-function(delta, L50, p){
  L = ((delta*log((1/p) - 1))/-log(19)) + L50
  return(L)
}

#calculate for each model
CutoffLength <-tibble(Sp = BestMod$Sp)
CutoffLength$Len<-mapply(MinLength, BestMod$deltamed, BestMod$L50med, 0.99, SIMPLIFY = TRUE)

#minke whales are the same
CutoffLength$Len[CutoffLength$Sp == "Common Minke Whale"]<-CutoffLength$Len[CutoffLength$Sp == "Antarctic Minke Whale"]
#blue whales are the same
CutoffLength$Len[CutoffLength$Sp == "Pygmy Blue Whale"]<-CutoffLength$Len[CutoffLength$Sp == "Antarctic Blue Whale"]

#just for now, using Sei cutoff for Bryde's because of similar adult size but should run misidentificaiton models for them
CutoffLength<-add_row(CutoffLength, Sp = "Brydes Whale", Len = CutoffLength$Len[CutoffLength$Sp == "Sei Whale"])



#names don't match
BestMod$Sp
unique(MLenFDat2$SpName)
#fixing name issues: 
#function to remove words including in "stopwords" vector
removeWords <- function(str, stopwords) {
  x <- unlist(strsplit(str, " "))
  paste(x[!x %in% stopwords], collapse = " ")
}

BestMod$SpName<-sapply(BestMod$Sp, removeWords, c("Whale")) %>% stringr::str_to_sentence()

#names should match: 
BestMod$SpName
unique(MLenFDat2$SpName)
CutoffLength<-BestMod %>% select(Sp, SpName) %>% right_join(CutoffLength)
#adding in Brydes for now
CutoffLength$SpName[9]<-"Brydes"
#filter small fetuses out of data
SpList<-CutoffLength$SpName
SpList<-SpList[SpList != "Sperm"]#removing sperm whales

SubsetFetuses<-function(Sp_dat){
  Cutoff<-CutoffLength %>% filter(SpName == Sp_dat) %>% select(Len) %>% unlist()
  FilteredDat<-Roq_MLen %>% filter(SpName == Sp_dat) %>% filter(F_dec_ft >= round(Cutoff, 4))
  return(FilteredDat)
  #return(Cutoff)
}

#subsetting all species
FilteredDat<-lapply(SpList,SubsetFetuses) %>% bind_rows()


# Binning Data ------------------------------------------------------------

c(1.5, 4.5, 7.5, 10.5)/12

#binning
BinVals<-FilteredDat  %>% filter(M_dec_ft != 0) %>% 
  group_by(SpName) %>% mutate(minL = floor(min(M_dec_ft)), maxL = ceiling(max(M_dec_ft))) %>% 
  select(SpName, minL, maxL) %>% unique() 

Bins<-mapply(seq,BinVals$minL - 0.125, BinVals$maxL + 0.125, by = 0.25)
Bins

labels <- lapply(Bins, function(x){format(x[-11], digits = 5)}) # Custom labels as lower value of each bin
labels
#center scale function
scale_this <- function(x){ #need to write your own in order to use it for grouped data
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}


Binned3Iinches<-FilteredDat %>% filter(M_dec_ft != 0) %>% 
  group_by(SpName) %>%
  mutate(binned = cut(M_dec_ft, breaks = Bins[[which(SpList == SpName[1])]], include.lowest = TRUE, labels = labels[[which(SpList == SpName[1])]])) %>%
  mutate(binned_center_sc = scale_this(as.numeric(as.character(binned)))) %>%
  group_by(SpName, binned, binned_center_sc) %>%
  summarise(Total = n(), Nmale = sum(Male)) %>% #create binomial data
  filter(Total != 0) %>% #remove empty bins
  filter(binned_center_sc <= 4 && binned_center_sc >= -4)

ggplot(Binned3Iinches) + geom_point(aes(x = binned_center_sc, y = Total)) +
  facet_wrap(~SpName, scales = "free")


# Model -------------------------------------------------------------------
#Random slope and intercept, no correlation parameter
Mod1_ALLRE<-brm(Nmale | trials(Total) ~ binned_center_sc + (1+binned_center_sc||SpName), #double bar means no correlation
                       data = Binned3Iinches, family = "beta_binomial", 
                       prior = c(prior(normal(0, 10), class = Intercept),
                                 prior(normal(0, 10), class = b), 
                                 prior(exponential(1), class = sd)),
                       warmup = 3000, 
                       iter   = 4000, 
                       chains = 4, control = list(adapt_delta = 0.95))

plot(Mod1_ALLRE)
summary(Mod1_ALLRE)
# Plots -------------------------------------------------------------------
library(ggthemes)
library(gghighlight)
library(png)
Mod1<-Mod1_ALLRE

#colors

pal<-c('#66c2a5','#8da0cb','#fc8d62','#e78ac3','#a6d854','#ffd92f','#e5c494','#822681FF')
pal<-c("#256676", "#5ce8ef", "#c20da6", "#57b230", "#835a9d", "#ffbd59",  "#476af9", "#34f50e")

col <-setNames(pal, SpList)

get_variables(Mod1)

#Slope
species_slopes<-Mod1 %>% spread_draws(b_binned_center_sc, r_SpName[c,t]) %>% mutate(Species = gsub("\\.", replacement = " ", c)) %>%
  filter(t == "binned_center_sc") %>% mutate(Sp_slope = b_binned_center_sc + r_SpName)

pop_slope<-spread_draws(Mod1, b_binned_center_sc) %>% mutate(Species = "Group")

#using mode and hdi because distributions are skewed
GroupSlopes_allRE<-
  ggplot(species_slopes) +
  annotate("rect", xmin = -0.06, xmax = 0.042, ymin = "Group", ymax = "Antarctic blue", fill = "gray", alpha = 0.5)+
  stat_halfeye(aes(y = Species, x = Sp_slope, fill = Species), point_interval = mode_hdi, scale = 0.8, alpha = 0.8, normalize = "panels") + 
  stat_halfeye(data = pop_slope, aes(x = b_binned_center_sc, y = Species), point_interval = mode_hdi, scale= 0.9, fill = "gray20", interval_color = "red", point_color = "red") + 
  theme_classic() +  
  labs(x = "Slope", y = "Density") + 
  geom_vline(aes(xintercept = 0.0), linetype = "dashed") +
  scale_fill_manual(values = col, drop = TRUE) + 
  scale_y_discrete(expand = c(0.002, 0), limits = c("Group", "Antarctic blue", "Pygmy blue", "Fin",   
                                                   "Humpback","Sei", "Brydes", "Antarctic minke",
                                                   "Common minke"), 
                   labels = c("Group", "Antarctic blue", "Pygmy blue", "Fin",   
                              "Humpback","Sei", "Brydes", "Antarctic minke",
                              "Common minke")) + 
  theme(legend.position = "none", axis.ticks.y = element_blank(), 
        axis.title.y = element_text(size = 12, vjust = 3),
        axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 11), 
        axis.text.y = element_text(hjust = 0, vjust = -5.4, margin = margin(l = 0, r = -85))) + 
  scale_x_continuous(limits = c(-0.06, 0.042), breaks = seq(-0.08, 0.08, by = 0.02), expand = c(0,0))

#adding silhouettes
#length at sexual maturity for females (approximately): 
#blue: 24 m 
#pygmy blue: 19 m, 
#fin: 20 m 
#sei: 13.6 m 
#byrdes: 12.5-13 (depending in population) 
#Antarctic minke: 8 m 
#Common minke: 7m 
#humpback: 13.9 m 
ut<-(-0.026--0.061)/24

blue<-grid::rasterGrob(readPNG("Figures/Chris Huh Phylopic/blue.png", native = TRUE))
fin<-grid::rasterGrob(readPNG("Figures/Chris Huh Phylopic/Fin.png", native = TRUE))
hump<-grid::rasterGrob(readPNG("Figures/Chris Huh Phylopic/humpback.png", native = TRUE))
sei<-grid::rasterGrob(readPNG("Figures/Chris Huh Phylopic/sei.png", native = TRUE))
minke<-grid::rasterGrob(readPNG("Figures/Chris Huh Phylopic/minke.png", native = TRUE))
brydes<-grid::rasterGrob(readPNG("Figures/Chris Huh Phylopic/Brydes.png", native = TRUE))

GroupSlopes_allRE_annot<-GroupSlopes_allRE +  annotation_custom(blue, xmin = -0.061, xmax = -0.026,ymin = 0.5, ymax = 4) +
  annotation_custom(blue, xmin = -0.061, xmax = -0.031,ymin = 1.5, ymax =5) +
  annotation_custom(fin, xmin = -0.061, xmax = -0.031,ymin = 4, ymax = 7) +
  annotation_custom(hump, xmin = -0.061, xmax = -0.041,ymin = 4, ymax = 7.9) + 
  annotation_custom(sei, xmin = -0.061, xmax = -0.042,ymin = 6.1, ymax = 8) +
  annotation_custom(minke, xmin = -0.060, xmax = -0.051,ymin = 8.5, ymax = 11) + 
  annotation_custom(minke, xmin = -0.060, xmax = -0.050,ymin = 7.5, ymax = 10) + 
  annotation_custom(brydes, xmin = -0.060, xmax = -0.042,ymin = 7.1, ymax =9)

GroupSlopes_allRE_annot
ggsave("Figures/SlopeResults_Silh.png", dpi = 900, width = 7, height = 7, units = "in")

#linear plot
species_vals <- Mod1 %>% spread_draws(b_Intercept, b_binned_center_sc, r_SpName[c,t]) %>% pivot_wider(names_from = "t", values_from = "r_SpName") %>% mutate(Species = gsub("\\.", replacement = " ", c))

get_pred<-function(species_val){
  Sp_lengths<-seq(-2, 2, by = 0.01)
  sr_pred_logit<-species_val["b_Intercept"] + species_val["Intercept"] + (species_val["b_binned_center_sc"] + species_val["binned_center_sc"])*Sp_lengths
  #return(sr_pred_logit)
  return(plogis(sr_pred_logit))
}


species_preds<-apply(species_vals[,c("b_Intercept", "b_binned_center_sc", "Intercept", "binned_center_sc")], 1, get_pred)
id<-length(seq(-2,2, by = 0.01))
#draw_sp<-species_vals[,c(".draw", "Species")]
pred_tibble_all<-tibble(draw = rep(species_vals$`.draw`, each = id), sp = rep(species_vals$Species, each = id), 
                    mat_length = rep(seq(-2, 2, by = 0.01), nrow(species_vals)), ypred = as.vector(species_preds))
pred_tibble_summ<-pred_tibble_all %>% group_by(sp, mat_length) %>% summarise(med = median(ypred), lwr = quantile(ypred, 0.025), upr = quantile(ypred,0.975))


pred_tibble_summ %>% filter(sp == "Antarctic blue") %>% 
  ggplot() + geom_ribbon(aes(x = mat_length, ymin = lwr, ymax = upr)) + geom_line(aes(x = mat_length, y = med))


linesplot<-ggplot(pred_tibble_summ) +
  geom_hline(aes(yintercept = 0.5), color = 'black')+
  geom_line(aes(x = mat_length, y = med, color = sp), alpha = 1, linewidth = 0.7) + 
  geom_ribbon(aes(x = mat_length, ymin = lwr, ymax = upr, color = sp), alpha = 0, linetype = "dashed", linewidth = 0.6) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0.46, 0.54)) + 
  scale_x_continuous(expand = c(0, 0)) +
  gghighlight(use_direct_label = FALSE, unhighlighted_params = list(linewidth = 0.1, color = "gray60")) +
  scale_color_manual(values = col, drop = TRUE) + 
  labs(x = "Maternal length (scaled)", y = "Fetal sex ratio") + 
  facet_wrap(~factor(sp, levels = c("Common minke", "Antarctic minke", "Brydes", 
                                    "Sei", "Humpback", "Fin", "Pygmy blue", "Antarctic blue")), nrow = 4) + 
  theme_tufte(base_size = 12, base_family = "Arial") + 
  theme(legend.position = "none", 
        #text = element_text(family = "Arial", size = 12), 
        strip.text = element_text(color = "black", size = 11, face = "bold", vjust = -1), 
        panel.grid.major.y = element_blank(), 
        panel.spacing = unit(0.8, "lines"))

linesplot

ggsave("Figures/predictions_species.png", linesplot, width = 7, height = 7, units = "in")
#Intercepts
species_intercepts<-Mod1 %>% spread_draws(b_Intercept, r_SpName[c,t]) %>% mutate(Species = gsub("\\.", replacement = " ", c)) %>%
  filter(t == "Intercept") %>% mutate(Sp_Intercept_logit = (b_Intercept + r_SpName), Sp_Intercept_real = plogis(Sp_Intercept_logit))


pop_intercept<-spread_draws(Mod1, b_Intercept) %>% mutate(b_Intercept_real = plogis(b_Intercept), Species = "Group")

#using mode and hdi because distributions are skewed
GroupIntercepts<-
  ggplot(species_intercepts) +
  annotate("rect", xmin = 0.475, xmax = 0.530, ymin = "Group", ymax = "Antarctic blue", fill = "gray", alpha = 0.5)+
  stat_halfeye(aes(y = Species, x = Sp_Intercept_real, fill = Species), point_interval = mode_hdi, scale = 0.8, alpha = 0.8, normalize = "panels") + 
  stat_halfeye(data = pop_intercept, aes(x = b_Intercept_real, y = Species), point_interval = mode_hdi, scale= 0.9, fill = "gray20", interval_color = "red", point_color = "red") + 
  theme_classic() +  
  labs(x = "Intercept", y = "Species") + 
  geom_vline(aes(xintercept = 0.5), linetype = "dashed") +
  scale_fill_manual(values = col, drop = TRUE) + 
  scale_y_discrete(expand = c(0.002, 0),  limits = c("Group", "Antarctic blue", "Pygmy blue", "Fin",   
                                                     "Humpback","Sei", "Brydes", "Antarctic minke",
                                                     "Common minke"), 
                   labels = c("Group", "Antarctic blue", "Pygmy blue", "Fin",   
                              "Humpback","Sei", "Brydes", "Antarctic minke",
                              "Common minke")) + 
  theme(legend.position = "none", axis.title.y = element_blank(), 
        axis.text = element_text(size = 11), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_x_continuous(limits = c(0.475, 0.530), breaks = seq(0.47, 0.52, by = 0.01), expand = c(0,0)) 



GroupIntercepts
lay<-"
AAAAAABBBBB
AAAAAABBBBB
AAAAAABBBBB"

slop_int<-GroupSlopes_allRE_annot + GroupIntercepts + plot_layout(design = lay)
slop_int
ggsave("Figures/Slope_Intercepts.png", dpi = 900, width = 10, height = 7, units = "in")


#other parameters summary: 
Mod1 %>% gather_draws(sd_SpName__Intercept, sd_SpName__binned_center_sc, phi) %>% median_qi()
#other parameters--11/22/23 NOTE: Not working currently, come back and fix later
plot(Mod1)
sd_draws<-Mod1 %>% spread_draws(sd_SpName__Intercept, sd_SpName__binned_center_sc) %>% 
  rename(sd_Intercept = sd_SpName__Intercept, sd_Slope = sd_SpName__binned_center_sc) %>% pivot_longer(c(sd_Intercept, sd_Slope), names_to = "par", values_to = "sd")
sd_prior<-tibble(sd_Intercept = rnorm(10000, 0, 10), sd_Slope = rnorm(10000, 0, 10)) %>% 
  pivot_longer(c(sd_Intercept, sd_Slope), names_to = "par", values_to = "sd") %>% filter(sd >= 0) %>% mutate(sd_dens = dnorm(sd, 0, 10))
phi_draws<-Mod1 %>% spread_draws(phi) %>% mutate(par = "phi")
phi_prior<-tibble(phi = rgamma(1000, 0.01, scale = 0.01), par = "phi")

ggplot() +
  stat_halfeye(data = sd_draws, aes(y = par, x = sd, fill = par), scale = 0.8, alpha = 0.8, normalize = "groups") + 
  geom_density(data = sd_prior, aes(x = sd_dens, y = par), stat = "identity", fill = "transparent", linetype = "dashed") + 
  theme_classic() +  
  labs(x = "Estimate", y = "Density") +
  #scale_fill_manual(values = col, drop = TRUE) + 
  scale_y_discrete(expand = c(0.02, 0), limits = c("sd_Intercept", "sd_Slope")) + 
  theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text = element_text(size = 11)) + 
  scale_x_continuous(limits = c(0, 0.201), breaks = seq(0,0.2, by = 0.05), expand = c(0,0))


# Numerical Results -------------------------------------------------------

ndraws<-4*1000
#probability slope is negative
species_slopes<-Mod1 %>% spread_draws(b_binned_center_sc, r_SpName[c,t]) %>% mutate(Species = gsub("\\.", replacement = " ", c)) %>%
  filter(t == "binned_center_sc") %>% mutate(Sp_slope = b_binned_center_sc + r_SpName)

species_neg<-species_slopes %>% mutate(neg = ifelse(Sp_slope < 0, 1, 0)) %>% 
  group_by(Species) %>% summarise(TotNeg = sum(neg), prob_neg = TotNeg/ndraws)

pop_slope_neg<-spread_draws(Mod1, b_binned_center_sc) %>% mutate(Species = "Group") %>% 
  mutate(neg = ifelse(b_binned_center_sc < 0, 1, 0)) %>% summarise(TotNeg = sum(neg), prob_neg = TotNeg/ndraws)


#intercepts

species_intercepts_sum<-Mod1 %>% spread_draws(b_Intercept, r_SpName[c,t]) %>% mutate(Species = gsub("\\.", replacement = " ", c)) %>%
  filter(t == "Intercept") %>% mutate(Sp_Intercept_logit = (b_Intercept + r_SpName), Sp_Intercept_real = plogis(Sp_Intercept_logit)) %>%
  summarise(med = median(Sp_Intercept_real), low = quantile(Sp_Intercept_real, 0.025), up = quantile(Sp_Intercept_real, 0.975))


pop_intercept_sum<-spread_draws(Mod1, b_Intercept) %>% 
  mutate(b_Intercept_real = plogis(b_Intercept), Species = "Group") %>%
  summarise(med = median(b_Intercept_real), low = quantile(b_Intercept_real, 0.025), up = quantile(b_Intercept_real, 0.975))
