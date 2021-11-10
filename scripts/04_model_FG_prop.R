pacman::p_load(tidyverse, skimr, cowplot, here, funk,disco, patchwork, rethinking, rstan, brms,  install=FALSE)

# Load reef pressure data
# https://github.com/WCS-Marine/local-reef-pressures
# devtools::load_all('../local-reef-pressures') ## fails
threat<-read.csv('data/wcs/wcs_threat_indicators.csv')  %>% mutate(nutrient_load = nutrient) %>%  select(-X, -country, -nutrient)

## load fish nut prod, estimate proportional by FG
load(file = 'results/wcs_productivity.rds')
load(file = 'results/wcs_nut_prod.rds')

prod_fg<-prod_fg %>% 
  mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                               'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3\nfatty acids')) %>% 
  group_by(country, nutrient,nutrient_lab) %>% 
  mutate(nut_prod_day_ha_scaled = scale(nut_prod_day_ha)[,1]) %>% 
  arrange((nut_prod_day_ha))  %>% 
  mutate(tnut = sum(nut_prod_day_ha), 
         nutprop = nut_prod_day_ha / tnut * 100) 

## set up model data

# read benthic
load('data/wcs/wcs_fish_benthic.rds')
focal<-left_join(prod_fg, 
                fish_avg %>% ungroup() %>%  select(site, reef_type, reef_zone, management_rules, hard_coral, macroalgae, turf_algae, bare_substrate, 
                                                   depth, fish_richness),
                by='site') %>% 
        filter(!is.na(depth)) #%>%  # dropping 2 sites (NK02 in Madasgascar and WaiE1 in Fiji)
        # left_join(threat, by = 'site') %>% ungroup() ## lots of sites missing, incl. all of Belize

## convert data to list, characters to factors for rethinking
focal<-focal %>% ungroup() %>% droplevels() %>%  mutate_if(is.character, as.factor) %>% 
  select(
  nutprop, nutrient, nutrient_lab, country, site, dietP, 
  hard_coral, macroalgae, turf_algae, bare_substrate, reef_type, reef_zone, depth,
  management_rules)
  # grav_NC, management_rules, sediment, nutrient_load, pop_count) 

## scale numeric
focal.list<-scaler(focal %>% filter(nutrient=='calcium.mg'), 
                    ID = c('nutprop', 'country', 'site', 'dietP', 'reef_type', 'reef_zone', 'management_rules'), cats = FALSE) %>% as.list()
summary(focal.list)

focal.scaled<-scaler(focal %>% filter(nutrient=='calcium.mg' & dietP=='Herbivores Microvores Detritivores'), 
                   ID = c('nutprop', 'country', 'site', 'dietP', 'reef_type', 'reef_zone', 'management_rules'), cats = FALSE) %>% 
  mutate(nutprop = nutprop/100)

# add list of region ID for each country name
focal.list$countryLookup<-focal %>% distinct(site, country) %>% pull(country) %>% as.factor()

## check reponse hist
hist(focal.list$nutprop)
hist(log10(focal.list$nutprop))


## temp fix on zero nutprops
focal.list$nutprop[focal.list$nutprop==0]<-0.1


## testing multinomial model
fit1 <-
   brm(
    bf(nutprop ~ hard_coral + turf_algae,
       phi ~ hard_coral + turf_algae,
       zi ~ 1),
    data = focal.scaled,
    family = zero_inflated_beta(),
    chains = 4, iter = 2000, warmup = 1000,
    cores = 4, seed = 1234
  )

