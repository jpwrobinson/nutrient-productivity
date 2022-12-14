
library(tidyverse)
load(file = 'results/wcs_productivity.rds')
load(file = 'results/wcs_nut_prod.rds')

## UVC metadata
fishp %>% summarise(n=n_distinct(site))
fishp %>% group_by(country, year) %>% summarise(n=n_distinct(site))
fishp %>% group_by(country) %>% summarise(n=n_distinct(site))
fishp %>% group_by(country) %>% distinct(management_rules)
fishp %>% group_by(country, site) %>% summarise(n=n_distinct(year)) %>% 
          group_by(country,n) %>% count()


## Figure 1 metadata
fish2<-fishp %>% group_by(fish_taxon, fish_family, trophic_group, lmax, nscore,
                          calcium.mg, iron.mg, selenium.mug, zinc.mg, omega3.g, vitamin_a.mug) %>% 
  summarise(Kmax =mean(Kmax)) %>% ungroup() 

cor(fish2$Kmax, fish2$nscore)
fish2 %>% filter(nscore > 350) %>% summarise(mean(Kmax))
fish2 %>% filter(nscore < 200 & nscore > 90) %>% summarise(range(Kmax))
fish2 %>% filter(log10(Kmax) > 0) %>% distinct(fish_taxon,  fish_family, trophic_group) %>% data.frame()

## top nutrient productivity species
tops<-c('Pterocaesio tile', 'Caesio teres', 'Chlorurus sordidus', 
        'Lutjanus gibbus', 'Sparisoma viride')

fish2 %>% filter(fish_taxon %in% tops) %>% mutate(Kmax = log10(Kmax)) %>% data.frame()

## Figure 2 metadata
source('scripts/fig/Figure2.R')
prod_fg2 %>% mutate_if(is.numeric, function(x) round(x, 0))
prod_fg_avg %>% mutate_if(is.numeric, function(x) round(x, 0)) %>% filter(nutrient=='vitamin_a.mug')
prod_fg_co %>% group_by(nutrient, fg_lab) %>% summarise(nutprop= mean(nutprop)) %>% 
    mutate(fg2 = ifelse(str_detect(fg_lab, 'Herb|mobile'), 'herbi-mobi', 'other')) %>% 
    group_by(fg2, nutrient) %>% summarise(sum(nutprop))
                        

## Sessile inverts
# go to 03_estimate_nut_prod hashtag the filter
prod_fg_avg %>% group_by(fg) %>% summarise(mean(nutprop))


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


### productivity metrics
load(file = 'results/wcs_productivity.rds')
load(file = 'results/wcs_nut_prod.rds')
ggplot(prod_reef, aes(prod_day_ha, nut_prod_day_ha, col=country)) + geom_point() + facet_wrap(~nutrient, scales='free')
ggplot(prod_reef, aes(biomass_kgha, nut_prod_day_ha, col=country)) + geom_point() + facet_wrap(~nutrient, scales='free')
ggplot(prod_reef, aes(biomass_kgha, nut_turnover, col=country)) + geom_point() + facet_wrap(~nutrient, scales='free')
ggplot(prod_reef, aes(biomass_kgha, prod_day_ha, col=country)) + geom_point() + facet_wrap(~nutrient, scales='free')

## correlation between nutrient productivity metrics
turns<-prod_reef %>% ungroup() %>% dplyr::select(country, site, year, nutrient, nut_turnover, biomass_turnover) %>% 
  pivot_wider(names_from = 'nutrient', values_from = 'nut_turnover')
pairs2(turns[,-c(1:3)])

prods<-prod_reef %>% ungroup() %>% dplyr::select(country, site, year, nutrient, nut_prod_day_ha) %>% pivot_wider(names_from = 'nutrient', values_from = 'nut_prod_day_ha')
pairs2(prods[,-c(1:3)])
