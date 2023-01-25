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
    mutate(prop = nut_prod_day_ha / max(nut_prod_day_ha),
           zinc.mg = prop)
t4<-read.csv('py-notebook/calcium.mg_reef_unscaled.csv') %>% 
    mutate(prop = nut_prod_day_ha / max(nut_prod_day_ha),
           calcium.mg = prop)
t5<-read.csv('py-notebook/iron.mg_reef_unscaled.csv') %>% 
    mutate(prop = nut_prod_day_ha / max(nut_prod_day_ha),
           iron.mg = prop)
t6<-read.csv('py-notebook/vitamin_a.mug_reef_unscaled.csv') %>% 
    mutate(prop = nut_prod_day_ha / max(nut_prod_day_ha),
           vitamin_a.mug = prop)
t7<-read.csv('py-notebook/selenium.mug_reef_unscaled.csv') %>% 
    mutate(prop = nut_prod_day_ha / max(nut_prod_day_ha),
           selenium.mug = prop)
t8<-read.csv('py-notebook/omega3.g_reef_unscaled.csv') %>% 
    mutate(prop = nut_prod_day_ha / max(nut_prod_day_ha),
           omega3.g = prop)

nut_comb<-(t3$prop + t4$prop +
  t5$prop + t6$prop + t7$prop + t8$prop) / 6

nut_main<-cbind(t3, calcium.mg = t4$calcium.mg, iron.mg = t5$iron.mg, vitamin_a.mug = t6$vitamin_a.mug, selenium.mug = t7$selenium.mug, omega3.g = t8$omega3.g) %>%
  pivot_longer(c(zinc.mg:omega3.g), names_to = 'nut', values_to = 'mu')

focal<-cbind(data.frame(y = nut_comb), t3)
hist(focal$y)

m<-brm(mu ~ hard_coral + 
         macroalgae + turf_algae + bare_substrate +
         rubble + grav_nc + pop_count + sediment + nutrient_load + depth + 
         management_rules + (1 | country),
       data = nut_main %>% filter(nut == 'iron.mg'), family = lognormal)

m<-brm(mu ~  log10(biomass_kgha) * nut * management_rules,
       data = nut_main, family = lognormal)

m
pp_check(m)
conditional_effects(m)


## dirichlet
bind <- function(...) cbind(...)


t3<-read.csv('py-notebook/zinc.mg_unscaled.csv') %>% mutate(id = paste0(site, year))
t8<-t8 %>% mutate(id = paste0(site, year))
t3$biomass_kgha<-t8$biomass_kgha[match(t3$id, t8$id)]

t6<-read.csv('py-notebook/vitamin_a.mug_unscaled.csv') %>% mutate(id = paste0(site, year))
t6$biomass_kgha<-t8$biomass_kgha[match(t6$id, t8$id)]

fit_vita <- brm(bind(herbivore.detritivore, herbivore.macroalgae,
                invertivore.mobile, omnivore,
                piscivore, planktivore) ~ log10(biomass_kgha) + scale(hard_coral), data=t6, dirichlet)

fit_zn <- brm(bind(herbivore.detritivore, herbivore.macroalgae,
                     invertivore.mobile, omnivore,
                     piscivore, planktivore) ~ log10(biomass_kgha) + scale(hard_coral), data=t3, dirichlet)
