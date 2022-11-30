## dirichlet
library(brms)
library(janitor)
bind <- function(...) cbind(...)

reef<-read.csv('py-notebook/zinc.mg_reef_unscaled.csv') 
  # mutate(id = paste0(site, year))
reef$management<-prod$management[match(reef$site, prod$site)]
reef$id<-reef$management
reef<-reef %>% group_by(id, management) %>% summarise(biomass_kgha = mean(biomass_kgha))

## zinc
focal<-read.csv('py-notebook/zinc.mg_scaled.csv') %>% clean_names() #%>% mutate(id = paste0(site, year))
focal$biomass_kgha<-reef$biomass_kgha[match(focal$management, reef$management)]

focal$tot<-rowSums(focal[,c('herbivore.detritivore', 'herbivore.macroalgae',
'invertivore.mobile', 'omnivore',
'piscivore', 'planktivore')])

focal$planktivore[focal$planktivore==0]<-0.001
focal$omnivore[focal$omnivore==0]<-0.001
focal$herbivore_detritivore[focal$herbivore_detritivore==0]<-0.001
focal$piscivore[focal$piscivore==0]<-0.001
focal$other[focal$other==0]<-0.001

focal$tot<-rowSums(focal[,c('herbivore_detritivore', 'other',
                            'invertivore_mobile', 'omnivore',
                            'piscivore', 'planktivore')])

focal$planktivore<-focal$planktivore / focal$tot
focal$omnivore<-focal$omnivore / focal$tot
focal$herbivore_detritivore<-focal$herbivore_detritivore / focal$tot
focal$invertivore_mobile<-focal$invertivore_mobile / focal$tot
focal$piscivore<-focal$piscivore / focal$tot
focal$other<-focal$other / focal$tot


fit <- brm(bind(herbivore_detritivore, other,invertivore_mobile, 
                omnivore, piscivore, planktivore) ~ log10(biomass_kgha) + 
                hard_coral + macroalgae + turf_algae + bare_substrate +
                rubble + depth, data=focal, dirichlet)

conditional_effects(fit, categorical = TRUE)
save(fit, file  = 'results/mod/zinc_brms.Rdata')

## calcium
focal<-read.csv('py-notebook/calcium.mg_scaled.csv') %>% mutate(id = paste0(site, year))
focal$biomass_kgha<-reef$biomass_kgha[match(focal$id, reef$id)]

fit <- brm(bind(herbivore.detritivore, herbivore.macroalgae,
                invertivore.mobile, omnivore,
                piscivore, planktivore) ~ log10(biomass_kgha) + 
             hard_coral + macroalgae + turf_algae + bare_substrate +
             rubble + grav_nc + pop_count + sediment + nutrient_load + depth + 
             management_rules + (1 | country), data=focal, dirichlet)

pp_check(fit)
save(fit, file  = 'results/mod/calcium_brms.Rdata')

## iron
focal<-read.csv('py-notebook/iron.mg_scaled.csv') %>% mutate(id = paste0(site, year))
focal$biomass_kgha<-reef$biomass_kgha[match(focal$id, reef$id)]

fit <- brm(bind(herbivore.detritivore, herbivore.macroalgae,
                invertivore.mobile, omnivore,
                piscivore, planktivore) ~ log10(biomass_kgha) + 
             hard_coral + macroalgae + turf_algae + bare_substrate +
             rubble + grav_nc + pop_count + sediment + nutrient_load + depth + 
             management_rules + (1 | country), data=focal, dirichlet)

pp_check(fit)
save(fit, file  = 'results/mod/iron_brms.Rdata')


## selenium
focal<-read.csv('py-notebook/selenium.mug_scaled.csv') %>% mutate(id = paste0(site, year))
focal$biomass_kgha<-reef$biomass_kgha[match(focal$id, reef$id)]

fit <- brm(bind(herbivore.detritivore, herbivore.macroalgae,
                invertivore.mobile, omnivore,
                piscivore, planktivore) ~ log10(biomass_kgha) + 
             hard_coral + macroalgae + turf_algae + bare_substrate +
             rubble + grav_nc + pop_count + sediment + nutrient_load + depth + 
             management_rules + (1 | country), data=focal, dirichlet)

pp_check(fit)
save(fit, file  = 'results/mod/selenium_brms.Rdata')

## vitamin_a
focal<-read.csv('py-notebook/vitamin_a.mug_scaled.csv') %>% mutate(id = paste0(site, year))
focal$biomass_kgha<-reef$biomass_kgha[match(focal$id, reef$id)]

fit <- brm(bind(herbivore.detritivore, herbivore.macroalgae,
                invertivore.mobile, omnivore,
                piscivore, planktivore) ~ log10(biomass_kgha) + 
             hard_coral + macroalgae + turf_algae + bare_substrate +
             rubble + grav_nc + pop_count + sediment + nutrient_load + depth + 
             management_rules + (1 | country), data=focal, dirichlet)

pp_check(fit)
save(fit, file  = 'results/mod/vitamin_a_brms.Rdata')

## omega3
focal<-read.csv('py-notebook/omega3.g_scaled.csv') %>% mutate(id = paste0(site, year))
focal$biomass_kgha<-reef$biomass_kgha[match(focal$id, reef$id)]

fit <- brm(bind(herbivore.detritivore, herbivore.macroalgae,
                invertivore.mobile, omnivore,
                piscivore, planktivore) ~ log10(biomass_kgha) + 
             hard_coral + macroalgae + turf_algae + bare_substrate +
             rubble + grav_nc + pop_count + sediment + nutrient_load + depth + 
             management_rules + (1 | country), data=focal, dirichlet)

pp_check(fit)
save(fit, file  = 'results/mod/omega3_brms.Rdata')
