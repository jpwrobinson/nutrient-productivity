
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

## excluding vitA
prod_fg %>% filter(nutrient!='vitamin_a.mug') %>%  
  group_by(dietP_lab) %>% summarise(se = se(nutprop), nutprop = mean(nutprop)) %>% 
  mutate(lower = nutprop - 2*se, upper = nutprop + 2*se) %>% 
  mutate_if(is.numeric, function(x) round(x*100, 0))