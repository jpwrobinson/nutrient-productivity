source('scripts/0_plot_theme.R')
library(rfishprod)
library(tidyverse)


## Script to estimate productivity of all individual fish in WCS dataset

##--------------------------------------------------##
## Data prep of fish and db (Morais) before modelling ##
##--------------------------------------------------##

fish<-read.csv('data/wcs/wcs_nutrients_individuals.csv') %>% 
    mutate(dietP = diet) %>% 
    filter(!is.na(biomass_kgha)) %>%  ## 1 fish in Belize with no L-W conversion
    filter(!is.na(lmax)) %>%  ## 0.6% of biomass / 1.4% abundance dropped
    filter(size >= 5 & lmax >= 6) ## 1.4% of biomass dropped
    
summary(fish$size)
#min = 5 cm, max = 280 cm, median = 17.5cm

hist(fish$size)
hist(log10(fish$size))

##--------------------------------------------------##
# 1. Check if any individuals are larger than species max size:
##--------------------------------------------------##
fish %>% filter(size >= lmax) %>% dim() ## 913 observations
fish %>% filter(size >= lmax) %>% summarise(sum(count)) ## 2700 fishes

# These need to be reduced to equal lmax (prod= exactly 0) 
# OR 0.1cm below lmax (tiny prod values)

fish$size2 <- ifelse(fish$size >= fish$lmax, fish$lmax-0.1, fish$size)
#Now use size2 for further analyses

##--------------------------------------------------##
# 2. Need to update db (rfishprod) with our diet cats
##--------------------------------------------------##
load(file = 'data/trait/wcs_sp_lmax_diet.rds')
diet<-read.csv('data/trait/parravicini_trophic_guilds_2020.csv') %>% janitor::clean_names() %>% 
  mutate(species = str_replace_all(species, '_', '\\ '))

## Paravicinni db has good resolution on invertivores, leading to many groups. Condense into mobile and sessile to be closer with Morais db
diet <- diet %>% mutate(dietP = recode(trophic_guild_predicted_text, 'microinvertivore' = 'Mobile invertivore',
                                   'macroinvertivore' = 'Mobile invertivore',
                                   'crustacivore' = 'Mobile invertivore',
                                   'corallivore' = 'Sessile invertivore',
                                   'sessile invertivores' = 'Sessile invertivore',
                                   'planktivore' = 'Planktivore',
                                   'piscivore' = 'Piscivore'))

db$dietP<-diet$dietP[match(db$Species, diet$species)] ## 111 missing species

## check how Renato diets correspond with Vale diets
cat_key<-db %>% filter(!is.na(dietP)) %>% distinct(Species, Diet, dietP) %>% 
          group_by(Diet) %>% mutate(tot = length(Species)) %>%
          group_by(Diet, dietP, tot) %>% summarise(n = length(dietP)) %>% ungroup() %>% 
          mutate(prop = n / tot * 100)

pdf(file = 'fig/explore/diet_categories_Kmax.pdf', height=6, width=10)
ggplot(cat_key, aes(dietP, prop)) + geom_bar(stat='identity') + facet_wrap(~Diet) +
  coord_flip() +
  labs(x = 'Parravicini diet', y = 'Number of species', 
       subtitle = 'Number of Parravicini diet categories (bars) within Morais diet categories (panels)')
dev.off()

## assign missing species using Morais categories and cat_key fig
db$dietP[is.na(db$dietP) & db$Diet == 'Plktiv']<-'Planktivore'
db$dietP[is.na(db$dietP) & db$Diet == 'InvMob']<-'Mobile invertivore'
db$dietP[is.na(db$dietP) & db$Diet == 'FisCep']<-'Piscivore'
db$dietP[is.na(db$dietP) & db$Diet == 'HerDet']<-'Herbivores Microvores Detritivores'
db$dietP[is.na(db$dietP) & db$Diet == 'HerMac']<-'Herbivores Microvores Detritivores'

## omnivores by species (fishbase)
db$dietP[is.na(db$dietP) & db$Diet == 'Omnivr' & db$Species == 'Diplodus cervinus']<-'Mobile invertivore'
db$dietP[is.na(db$dietP) & db$Diet == 'Omnivr' & db$Species == 'Hyporhamphus australis']<-'Herbivores Microvores Detritivores'

db %>% filter(is.na(dietP)) %>% distinct(Species, Diet)

##--------------------------------------------------##
## 3. add SST (see morais_sst_script.R)
##--------------------------------------------------##
sst<-read.csv('data/env/sst_mean_extracted.csv')
fish$sstmean<-sst$sst

##--------------------------------------------------##
# 4. Predicting Kmax, the standardised VBGF parameter (Recommendation: use 100s to 1000s iterations) 
##--------------------------------------------------##
# change db names to match fish names
db$lmax<-db$MaxSizeTL

## drop levels
db<-droplevels(db)
fish<-droplevels(fish)

# adapt formula from Morais & Bellwood 2018
fmod <- formula (~ sstmean + lmax + dietP) 

# fit xgboost model to predict Kmax
fishp <- predKmax (fish,
                     dataset = db,
                     fmod = fmod,
                     niter = 1000,
                     return = 'pred')

# save Kmax predictions
fishp <- fishp$pred
hist(fishp$Kmax)
save(fishp, file = 'results/wcs_productivity.rds')

# 
# # Positioning fish in their growth trajectory 
# # i.e. what's the size they're supposed to have on the next day? 
fishp$L.1day <- with (fishp, applyVBGF (Lmeas = size2,
                                            Lmax = lmax,
                                            Kmax = Kmax))

head(fishp) #each fish has grown a tiny amount (in length).


#Calculate age estimates:
fishp$EstAge <- (1/fishp$Kmax)*log((fishp$lmax)/((1-fishp$size2/fishp$lmax)*fishp$lmax))
summary(fishp$EstAge)
#(if lengths not reduced below lmax, will have infinite age values)


#Calculate growth with von Bertalanffy growth function (VBGF), using ages:
#VBGF:  Lt = Lmax*(1-exp(-K*t))
# t = Estimated age + time interval
# Morais & Bellwood (2019), interval = 1day (i.e. 1/365)

# Calculate over a full year: age + (1:365)/365

#Age to add for each day of the year:
age <- (1:365)/365

#Create table for new length: 1 column per day, 1 row per individual fish
VB_lngth <-  matrix(ncol=length(age), nrow=nrow(fishp), dimnames=list(NULL, paste("Day", 1:365, sep="_")))

# for each individual, calculate new length for each day using VBGF formula
for(u in 1:nrow(fishp)) {
  VB_lngth[u, ] <- fishp$lmax[u]*(1-exp(-fishp$Kmax[u]*(fishp$EstAge[u] + age)))
}

#Now have a matrix of lengths for each day of the year per fish
range(VB_lngth)
#  1.009271   101.314415

# 
# #Convert lengths to weights using a & b coefficents:
# VB_wt <-  apply(VB_lngth, 2, function(x) fishp$a*(x^fishp$b))  
# head(VB_wt)
# nrow(VB_wt)
# # 5219 rows
# # units = mass in grams (per fish)
# 
# biomass.g <- (fishp$a * fishp$size2^fishp$b)
# VB_wt2 <- cbind(biomass.g, VB_wt)
# head(VB_wt2)
# colnames(VB_wt2)[1] <- "Day_0"
# dim(VB_wt2)
# # 5219   366
# # First column = mass when observed during survey, then after 1 day, 2 days etc.
# 
# #create new object:
# VB_prod_wt <- VB_wt2
# 
# #calculate daily mass produced (productivity)
# for(u in 2:ncol(VB_prod_wt))
#   for(v in 1:nrow(VB_prod_wt)) {
#     VB_prod_wt[v,u] <- VB_wt2[v,u] - VB_wt2[v,(u-1)]
#   }
# 
# #Delete first column (still total mass in g)
# VB_prod_wt2 <- VB_prod_wt[,2:ncol(VB_prod_wt)]
# head(VB_prod_wt2) 
# # table of daily production in g/individual over a year (not cumulative)
# 
# 
# # Now sum row totals to get POTENTIAL productivity over 1 year (i.e. without accounting for mortality):
# prod_yr <- rowSums(VB_prod_wt2)
# prod_yr  # 1 value per fish (row)
# summary(prod_yr)
# #   Min.   1st Qu.    Median      Mean   3rd Qu.       Max. 
# # 0.1418   11.5866   26.8228   60.8488   77.3024  1171.2566 
# 
# 
# #Take value after 1 day as daily productivity value:
# prod_day <- VB_prod_wt2[,1] 
# prod_day
# summary(prod_day)
# #      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# # 0.0002209 0.0286485 0.0635776 0.1480279 0.1862414 3.1542922  
# 
# prod.per.fish <- cbind(prod_day,prod_yr)
# head(prod.per.fish, 10)
# 
# # Now have daily and annual potential productivity estimates per fish.
# 
# #write.csv(prod.per.fish, "data/Potential-prod-test_rfishprod.csv", row.names=F)
# 
# #prod.per.fish <- read.csv("data/Potential-prod-test_rfishprod.csv")
# 
# fish2 <- cbind(fish, prod.per.fish, Kmax=fishp$Kmax)