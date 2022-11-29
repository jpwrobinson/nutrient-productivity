library(tidyverse)
focal<-read.csv('py-notebook/zinc.mg_scaled.csv')# %>% 
  # pivot_longer(herbivore.detritivore:planktivore, names_to = 'fg', values_to = 'prop')
  # mutate(rel = herbivore.detritivore / invertivore.mobile)


library(brms)

m<-brm(herbivore.detritivore ~ hard_coral + 
        macroalgae + turf_algae + bare_substrate +
         rubble + grav_nc + pop_count + sediment + nutrient_load + depth + 
         management_rules + (1 | country),
        data = focal, family = Beta)

m
pp_check(m)
conditional_effects(m)