pacman::p_load(tidyverse, skimr, cowplot, here, funk,disco, patchwork, bayesplot,
               broom, broom.mixed, rethinking, rstan, brms, tidybayes,emmeans, install=FALSE)

# ## set up model data
# dp<-'Herbivores Microvores Detritivores'
# nut<-'calcium.mg'

# join prod estimates with benthic + fishing covariates
focal<-left_join(prod_fg, 
                fish_avg %>% ungroup() %>%  
                  select(site, reef_type, reef_zone, management_rules, 
                         hard_coral, macroalgae, turf_algae, bare_substrate, depth, fish_richness),
                by='site') %>% 
        # recode management_rules
        mutate(management_rules = recode(management_rules, 'periodic closure' = 'access restriction',
                                                            'gear restriction' = 'gear restriction',
                                                            'periodic closure; access restriction' = 'access restriction',
                                                            'open access' = 'open-access',
                                                            'no take' = 'no-take',
                                                            'access restriction' = 'access restriction')) %>% 
        filter(!is.na(depth)) %>%  # dropping 2 sites (NK02 in Madasgascar and WaiE1 in Fiji)
        # left_join(threat, by = 'site') %>% ungroup() ## lots of sites missing, incl. all of Belize
        mutate_if(is.character, as.factor) %>% 
        select(
          nutprop, nutrient, nutrient_lab, country, site, year, dietP, 
          hard_coral, macroalgae, turf_algae, bare_substrate, reef_type, reef_zone, depth,
          management_rules) %>%  # grav_NC, management_rules, sediment, nutrient_load, pop_count) 
        filter(nutrient==nut & dietP==dp) %>% 
        mutate(management_rules = fct_relevel(management_rules, 'open-access', after=0)) ## open-access is reference level

## scale numeric
source('scripts/scaler.R')
focal.scaled<-scaler(focal, 
                   ID = c('nutprop','nutrient','nutrient_lab', 'country', 'site','year',
                          'dietP', 'reef_type', 'reef_zone',
                          'management_rules'), cats = FALSE) 

## check reponse hist, bounded 0 - 1
hist(focal.scaled$nutprop)
focal.scaled$nutprop[focal.scaled$nutprop==1]<-0.99
  
## testing multinomial model
fit1 <-
     brm(
      bf(nutprop ~ hard_coral + turf_algae + macroalgae + bare_substrate + depth + management_rules + 
           (1 | country) + (1 | year),
         phi ~ hard_coral + turf_algae + macroalgae + bare_substrate + depth + management_rules + 
           (1 | country) + (1 | year),
         zi ~ 1),
      data = focal.scaled,
      family = zero_inflated_beta(),
      chains = 4, iter = 5000, warmup = 1000,
      cores = 4, seed = 43
    )

save(focal, focal.scaled, fit1, file = paste0('results/mods/', nut,'_', dp, '_model.Rdata'))
