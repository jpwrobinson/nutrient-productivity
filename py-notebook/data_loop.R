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
  filter(trophic_group != 'invertivore-sessile') %>% 
  group_by(country, site, year, nutrient,nutrient_lab) %>% 
  mutate(tnut = sum(nut_prod_day_ha), 
         nutprop = nut_prod_day_ha / tnut) 

focal<-read.csv('py-notebook/zinc.mg_scaled.csv') %>% 
    pivot_longer(herbivore.detritivore:planktivore, names_to = 'fg', values_to = 'nutprop')
ggplot(focal, aes(nutprop)) + geom_histogram() + facet_wrap(~fg)
ggplot(focal, aes(depth, nutprop, col=fg)) + geom_point() + geom_smooth()
ggplot(focal, aes(grav_nc, nutprop, col=fg)) + geom_point() + geom_smooth()
ggplot(focal, aes(fg, nutprop, fill=fg)) + geom_boxplot() + facet_wrap(~country)
focal %>% group_by(fg) %>% summarise(median(nutprop), min(nutprop), max(nutprop))

nut.vec<-unique(prod_fg$nutrient)

# model data
for(i in 1:length(nut.vec)){
  nut<-nut.vec[i]
  print(paste('Data mangle + scale for', nut))
  source('py-notebook/data_extract.R')
}

test<-read.csv('py-notebook/zinc.mg_unscaled.csv')
hist(test$depth)



unique(rowSums(focal.scaled[,c(18:23)]))

pairs2(focal.scaled[,c(18:23)])

tt<-prod_fg %>% filter(trophic_group=='planktivore') %>% 
  ungroup() %>% 
  select(site, year, country, nutrient, nutprop, prod_g_day_ha) %>% 
  pivot_wider(names_from = nutrient, values_from = nutprop)
pairs2(tt[,-c(1:3)])