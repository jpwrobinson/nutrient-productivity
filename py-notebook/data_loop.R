pacman::p_load(tidyverse, funk, install=FALSE)
theme_set(theme_bw())

## load datasets
load(file = 'results/wcs_productivity.rds')
load(file = 'results/wcs_nut_prod.rds')
load('data/wcs/wcs_fish_benthic.rds')

## recode and estimate nutrient proportion per site per fg
prod_fg<-prod_fg %>% 
  mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                               'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3\nfatty acids')) %>% 
  ## drop invert sessile as these are small proportion, consistently, and not fished
  filter(!fg %in% c('invertivore-sessile', 'detritivore')) %>% 
  group_by(country, site, year, nutrient,nutrient_lab) %>% 
  mutate(tnut = sum(nut_prod_day_ha), 
         nutprop = nut_prod_day_ha / tnut,
         tprod = sum(prod_g_day_ha),
         prodprop = prod_g_day_ha / tprod) 

prod_reef<-prod_reef %>% 
  mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                               'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3\nfatty acids')) 
  
## looping by nutrient
nut.vec<-unique(prod_fg$nutrient)

# model data - extract for fg
# for(i in 1:length(nut.vec)){
#   nut<-nut.vec[i]
#   print(paste('Data mangle + scale for', nut))
#   source('py-notebook/data_extract.R')
# }
# 
# ## productivity only
# nut = 'productivity'
# source('py-notebook/data_extract.R')

# model data - extract for reef
for(i in 1:length(nut.vec)){
  nut<-nut.vec[i]
  print(paste('Data mangle + scale for', nut))
  source('py-notebook/data_extract_reef.R')
}



focal<-read.csv('py-notebook/zinc.mg_scaled.csv') %>% 
  pivot_longer(browser:scraper.excavator, names_to = 'fg', values_to = 'nutprop')
ggplot(focal, aes(nutprop)) + geom_histogram() + facet_wrap(~fg)
# ggplot(focal, aes(depth, nutprop, col=fg)) + geom_point() + geom_smooth()
# ggplot(focal, aes(grav_nc, nutprop, col=fg)) + geom_point() + geom_smooth()
# ggplot(focal, aes(fg, nutprop, fill=fg)) + geom_boxplot() + facet_wrap(~country)
ggplot(focal, aes(management_rules, nutprop, fill=fg)) + geom_boxplot() + facet_wrap(~country)
# focal %>% group_by(fg) %>% summarise(median(nutprop), min(nutprop), max(nutprop))

focal<-read.csv('py-notebook/zinc.mg_reef_unscaled.csv') 
ggplot(focal, aes(country, biomass_kgha, fill=management_rules)) + geom_boxplot()


test<-read.csv('py-notebook/zinc.mg_reef_unscaled.csv')
pairs2(test[,c(10:14,17, 19:22)])

test<-read.csv('py-notebook/zinc.mg_reef_unscaled.csv')
hist(log(test$nut_turnover))
