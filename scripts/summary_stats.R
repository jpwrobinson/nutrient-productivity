
library(tidyverse)
load(file = 'results/wcs_productivity.rds')
load(file = 'results/wcs_nut_prod.rds')

## UVC metadata
fishp %>% summarise(n=n_distinct(site))
fishp %>% group_by(country, year) %>% summarise(n=n_distinct(site))
fishp %>% group_by(country) %>% summarise(n=n_distinct(site))
fishp %>% group_by(country, site) %>% summarise(n=n_distinct(year)) %>% 
          group_by(country,n) %>% count()


## Figure 1 metadata
fish2<-fishp %>% group_by(fish_taxon, fish_family, dietP, lmax, nscore,
                          calcium.mg, iron.mg, selenium.mug, zinc.mg, omega3.g, vitamin_a.mug) %>% 
  summarise(Kmax =mean(Kmax)) %>% ungroup() 

cor(fish2$Kmax, fish2$nscore)
fish2 %>% filter(nscore > 350) %>% summarise(mean(Kmax))
fish2 %>% filter(nscore < 200 & nscore > 90) %>% summarise(range(Kmax))
fish2 %>% filter(Kmax > 1) %>% distinct(fish_taxon,  fish_family, dietP) %>% data.frame()

## Figure 2 metadata
source('scripts/fig/Figure2.R')
prod_fg2 %>% mutate_if(is.numeric, function(x) round(x*100, 0))
prod_fg %>% mutate_if(is.numeric, function(x) round(x*100, 0)) %>% filter(nutrient=='vitamin_a.mug')
prod_fg %>% group_by(nutrient) %>% summarise(trophic_lab[which.max(nutprop)])
prod_fg %>% filter(trophic_lab %in% c('Herbivore (detritivore)', 'Invertivore (mobile)', 'Piscivore')) %>% 
  group_by(nutrient) %>% summarise(trophic_lab[which.max(nutprop)])

## excluding vitA
prod_fg %>% filter(nutrient!='vitamin_a.mug') %>%  
  group_by(dietP_lab) %>% summarise(se = se(nutprop), nutprop = mean(nutprop)) %>% 
  mutate(lower = nutprop - 2*se, upper = nutprop + 2*se) %>% 
  mutate_if(is.numeric, function(x) round(x*100, 0))

## benthic + threats
threat<-read.csv('data/threat/sites-threats.csv')  %>% 
  rename_at(vars(starts_with('andrello')), ~str_replace_all(., 'andrello_', '')) %>% 
  mutate(nutrient_load = nutrient) 

fish_avg %>% select(hard_coral, turf_algae, macroalgae, bare_substrate) %>% 
      ungroup() %>% 
      summarise(across(hard_coral:bare_substrate, ~range(.x, na.rm=TRUE)))

threat %>% select(pop_count, grav_nc) %>% 
  ungroup() %>% 
  summarise(across(pop_count:grav_nc, ~range(.x, na.rm=TRUE)))

focal %>% group_by(management_rules) %>% summarise(n_distinct(site))
