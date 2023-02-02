
library(tidyverse)
load(file = 'results/wcs_productivity.rds')
load(file = 'results/wcs_nut_prod.rds')
# fish<-read.csv(file = 'data/wcs/wcs_nutrients_individuals.csv')


## UVC metadata
fishp %>% summarise(n=n_distinct(site))
fishp %>% group_by(country, year) %>% summarise(n=n_distinct(site))
fishp %>% group_by(country) %>% summarise(n=n_distinct(site))
fishp %>% group_by(country) %>% distinct(management_rules)
fishp %>% group_by(country, site) %>% summarise(n=n_distinct(year)) %>% 
          group_by(country,n) %>% count()

with(fishp, table(transect_width, transect_length)/dim(fishp)[1]*100)
fishp %>% group_by(country, site) %>% summarise(n = n_distinct(transect_number)) %>% 
  ungroup() %>% 
  reframe(range(n), median(n))

## browsers
fishp %>% filter(fg == 'browser') %>% group_by(country) %>% distinct(fish_taxon) %>% 
  data.frame()
fishp %>% filter(fg == 'browser' & country=='Belize') %>% group_by(site,transect_number, fish_taxon) %>% 
  summarise(biom = sum(biomass_kgha)) %>% 
  group_by(fish_taxon) %>% 
  summarise(biom = mean(biom)) 

## Figure 1 metadata
fish2<-fishp %>% group_by(fish_taxon, fish_family, trophic_group, lmax, nscore,
                          calcium.mg, iron.mg, selenium.mug, zinc.mg, omega3.g, vitamin_a.mug) %>% 
  summarise(Kmax =mean(Kmax)) %>% ungroup() 

cor(fish2$Kmax, fish2$nscore)
fish2 %>% filter(nscore > 350) %>% summarise(mean(Kmax))
fish2 %>% filter(nscore > 350 & Kmax > 1) %>% pull(fish_taxon)
fish2 %>% filter(nscore < 250 & nscore > 90) %>% reframe(range(Kmax))
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


## depth 
foc<-read.csv(paste0('py-notebook/calcium.mg_unscaled.csv'))
py<-read.csv(file = 'results/pyramid_preds.csv')

py$depth<-foc$depth[match(py$id2, foc$id2)]
py$biomass_kgha<-foc$biomass_kgha[match(py$id2, foc$id2)]

ggplot(py, aes(depth, tb, col = country)) + geom_point() + 
  scale_x_continuous(breaks=seq(0,15,1)) +
  facet_wrap(~nutrient_lab)

# ggplot(py, aes(depth, biomass_kgha, col = country)) + geom_point() + facet_wrap(~nutrient_lab)
  