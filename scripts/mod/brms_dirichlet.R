## dirichlet
library(brms)
library(janitor)
library(rethinking)
bind <- function(...) cbind(...)

reef<-read.csv('py-notebook/zinc.mg_reef_unscaled.csv') 
  # mutate(id = paste0(site, year))
reef$management<-prod$management[match(reef$site, prod$site)]
reef$id<-reef$management
reef<-reef %>% group_by(id, management) %>% summarise(biomass_kgha = mean(biomass_kgha))

## zinc
focal<-read.csv('py-notebook/zinc.mg_scaled.csv') %>% clean_names() #%>% mutate(id = paste0(site, year))
focal$biomass_kgha<-reef$biomass_kgha[match(focal$management, reef$management)]

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
                omnivore, piscivore, planktivore) ~ log10(biomass_kgha) + depth +
             country, data=focal, dirichlet)

conditional_effects(fit, categorical = TRUE)

nd<-expand.grid(biomass_kgha = seq(min(focal$biomass_kgha), max(focal$biomass_kgha), 1), 
                country = unique(focal$country)[1], depth=mean(focal$depth))

pred<-posterior_epred(fit, newdata = nd,  re_formula=NA)
name<-dimnames(pred)[[3]]
for(i in 1:6){
  t<-pred[,,i]
  mu<-apply(t, 2, median)
  lower<-apply(t, 2, HPDI, 0.95)[1,]
  upper<-apply(t, 2, HPDI, 0.95)[2,]
  nd$mu<-mu
  nd$lower<-lower
  nd$upper<-upper
  colnames(nd)[colnames(nd)=='mu']<-paste(name[i], 'mu', sep = '-')
  colnames(nd)[colnames(nd)=='lower']<-paste(name[i], 'lower', sep = '-')
  colnames(nd)[colnames(nd)=='upper']<-paste(name[i], 'upper', sep = '-')
}

ndl<-nd %>% pivot_longer(-c(biomass_kgha, country, depth),
                                    names_to = c(".value", "var"),
                                    names_sep = "-") %>% 
  pivot_longer(-c(biomass_kgha, country, depth, var), names_to = 'fg', values_to = 'pred') %>% 
  pivot_wider(names_from = 'var', values_from = 'pred')

ggplot(ndl, aes(biomass_kgha, mu, fill=fg)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.5) +
  geom_line(aes(col=fg)) 


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
