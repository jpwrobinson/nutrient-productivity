pacman::p_load(tidyverse, skimr, cowplot, here, funk,disco, patchwork, bayesplot,tidybayes,
               broom, broom.mixed, rethinking, rstan, brms, tidybayes,emmeans, install=FALSE)
source('scripts/0_plot_theme.R')

dp1<-'herbivore-detritivore'
dp2<-'piscivore'
dp3<-'invertivore-mobile'
dp4<-'herbivore-macroalgae'
dps<-c(dp1, dp2, dp3, dp4)

## load datasets
load(file = 'results/wcs_productivity.rds')
load(file = 'results/wcs_nut_prod.rds')
load('data/wcs/wcs_fish_benthic.rds')

## recode and estimate nutrient proportion per site per fg
prod_fg<-prod_fg %>% 
  mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                               'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3\nfatty acids')) %>% 
  group_by(country, site, nutrient,nutrient_lab) %>% 
  mutate(tnut = sum(nut_prod_day_ha), 
         nutprop = nut_prod_day_ha / tnut) 

## sample management effects for each FG
nut.vec<-unique(prod_fg$nutrient)

pp<-numeric()
for(i in 1:length(dps)){
  load(file = paste0('results/mods/', nut.vec[6],'_', dps[i], '_model.Rdata'))
  pred<-focal.scaled %>% modelr::data_grid(management_rules = 'open-access',
                                           year=2016,
                                           country = 'Fiji',
                                           hard_coral = min(hard_coral),
                                           turf_algae = max(turf_algae),
                                           macroalgae = 0,
                                           bare_substrate = 0,
                                           depth = 0,
                                           grav_nc = max(grav_nc),
                                           pop_count = max(pop_count)) %>% 
    add_epred_draws(fit1, ndraws=1000, re_formula = NA) %>% 
    mutate(fg = dps[i])
  pp<-rbind(pp, data.frame(pred))
}
meds_open_degraded<-pp %>% #group_by(fg) %>% 
      # summarise(med = median(.epred)) %>% 
      mutate(type = 'Open, degraded')

pp<-numeric()
for(i in 1:length(dps)){
  load(file = paste0('results/mods/', nut.vec[6],'_', dps[i], '_model.Rdata'))
  pred<-focal.scaled %>% modelr::data_grid(management_rules = 'gear restriction',
                                           year=2016,
                                           country = 'Fiji',
                                           hard_coral = min(hard_coral),
                                           turf_algae = max(turf_algae),
                                           macroalgae = 0,
                                           bare_substrate = 0,
                                           depth = 0,
                                           grav_nc = max(grav_nc),
                                           pop_count = max(pop_count)) %>% 
    add_epred_draws(fit1, ndraws=1000, re_formula = NA) %>% 
    mutate(fg = dps[i])
  pp<-rbind(pp, data.frame(pred))
}
meds_managed_degraded<-pp %>% #group_by(fg) %>% 
      # summarise(med = median(.epred)) %>% 
      mutate(type = 'Managed, degraded')



pp<-numeric()
for(i in 1:length(dps)){
  load(file = paste0('results/mods/', nut.vec[6],'_', dps[i], '_model.Rdata'))
  pred<-focal.scaled %>% modelr::data_grid(management_rules = 'gear restriction',
                                           year=2016,
                                           country = 'Fiji',
                                           hard_coral = max(hard_coral),
                                           turf_algae = 0,
                                           macroalgae = min(macroalgae),
                                           bare_substrate = 0,
                                           depth = 0,
                                           grav_nc = min(grav_nc),
                                           pop_count = min(pop_count)) %>% 
    add_epred_draws(fit1, ndraws=1000, re_formula = NA) %>% 
    mutate(fg = dps[i])
  pp<-rbind(pp, data.frame(pred))
}
meds_managed_intact<-pp %>% group_by(fg) %>%
      # summarise(med = median(.epred)) %>%
      mutate(type = 'Managed, intact')


pp<-numeric()
for(i in 1:length(dps)){
  load(file = paste0('results/mods/', nut.vec[6],'_', dps[i], '_model.Rdata'))
  pred<-focal.scaled %>% modelr::data_grid(management_rules = 'no-take',
                                           year=2016,
                                           country = 'Fiji',
                                           hard_coral = max(hard_coral),
                                           turf_algae = 0,
                                           macroalgae = min(macroalgae),
                                           bare_substrate = 0,
                                           depth = 0,
                                           grav_nc = 0,
                                           pop_count = 0) %>% 
    add_epred_draws(fit1, ndraws=1000, re_formula = NA) %>% 
    mutate(fg = dps[i])
  pp<-rbind(pp, data.frame(pred))
}
meds_remote_intact<-pp %>% #group_by(fg) %>% 
      # summarise(med = median(.epred)) %>% 
      mutate(type = 'Remote, intact')

mm<-rbind(meds_open_degraded, meds_managed_degraded, meds_managed_intact, meds_remote_intact)

# ggplot(mm, aes(type, med, fill=fg)) + geom_bar(stat='identity')

ggplot(mm %>% filter(type %in% c('Managed, degraded', 'Open, degraded')), 
        aes(.epred, fill=fg)) + 
  stat_sample_slabinterval(alpha=0.5, .width=0.5) + facet_wrap(~type)
 

meds<-mm %>% filter(type %in% c('Managed, degraded', 'Open, degraded')) %>% 
  group_by(type, fg) %>% 
  summarise(med = median(.epred))

ggplot(meds, aes(type, med, fill=fg)) + geom_bar(stat='identity')

fish2<-fishp %>% group_by(fish_taxon, trophic_group) %>% 
  distinct(calcium.mg, iron.mg, selenium.mug, zinc.mg, omega3.g, vitamin_a.mug) %>% 
  pivot_longer(-c(fish_taxon, trophic_group), names_to = 'nutrient', values_to = 'conc') %>% 
  group_by(trophic_group, nutrient) %>% 
  summarise(conc =mean(conc)) %>% 
  group_by(trophic_group, nutrient) %>% 
  summarise(conc =mean(conc)) %>% 
  mutate(fg = trophic_group)

md<-fish2 %>% left_join(meds %>% filter(type=='Managed, degraded'),  by ='fg') %>% 
  filter(!is.na(med)) %>% 
  group_by(nutrient) %>% 
  summarise(weighted.mean(conc, w = med))

od<-fish2 %>% left_join(meds %>% filter(type=='Open, degraded'),  by ='fg') %>% 
  filter(!is.na(med)) %>% 
  group_by(nutrient) %>% 
  summarise(weighted.mean(conc, w = med))


pivot_wider(names_from = 'nutrient', values_from ='conc') 