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

## scale and average nutrient productivity
t3<-read.csv('py-notebook/zinc.mg_reef_unscaled.csv') %>% 
    mutate(nut_prod_day_ha = nut_prod_day_ha / which.max(nut_prod_day_ha))
t4<-read.csv('py-notebook/calcium.mg_reef_unscaled.csv') %>% 
    mutate(nut_prod_day_ha = nut_prod_day_ha / which.max(nut_prod_day_ha))
t5<-read.csv('py-notebook/iron.mg_reef_unscaled.csv') %>% 
    mutate(nut_prod_day_ha = nut_prod_day_ha / which.max(nut_prod_day_ha))
t6<-read.csv('py-notebook/vitamin_a.mug_reef_unscaled.csv') %>% 
    mutate(nut_prod_day_ha = nut_prod_day_ha / which.max(nut_prod_day_ha))
t7<-read.csv('py-notebook/selenium.mug_reef_unscaled.csv') %>% 
    mutate(nut_prod_day_ha = nut_prod_day_ha / which.max(nut_prod_day_ha))
t8<-read.csv('py-notebook/omega3.g_reef_unscaled.csv') %>% 
    mutate(nut_prod_day_ha = nut_prod_day_ha / which.max(nut_prod_day_ha))

nut_comb<-(t3$nut_prod_day_ha + t4$nut_prod_day_ha +
  t5$nut_prod_day_ha + t6$nut_prod_day_ha + t7$nut_prod_day_ha + t8$nut_prod_day_ha) / 6

focal<-cbind(data.frame(y = nut_comb), t3)
hist(focal$y)
m<-brm(biomass_kgha ~ hard_coral + 
         macroalgae + turf_algae + bare_substrate +
         rubble + grav_nc + pop_count + sediment + nutrient_load + depth + 
         management_rules + (1 | country),
       data = focal, family = lognormal)

m<-brm(y ~  log10(biomass_kgha) * management_rules,
       data = focal, family = lognormal)

m
pp_check(m)
conditional_effects(m)
