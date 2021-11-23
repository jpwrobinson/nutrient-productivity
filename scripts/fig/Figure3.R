pacman::p_load(tidyverse, skimr, cowplot, here, funk,disco, patchwork, bayesplot,tidybayes,
               broom, broom.mixed, rethinking, rstan, brms, tidybayes,emmeans, install=FALSE)
source('scripts/0_plot_theme.R')

dp<-'Herbivores Microvores Detritivores'

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

for(i in 1:length(nut.vec)){
  
  load(file = paste0('results/mods/', nut.vec[3],'_', dp, '_model.Rdata'))
  focal.scaled %>% modelr::data_grid(management_rules, 
                                     hard_coral = 0,
                                     turf_algae = 0,
                                     macroalgae = 0,
                                     bare_substrate = 0,
                                     depth = 0,
                                     grav_nc = 0,
                                     pop_count = 0) %>% 
    add_predicted_draws(fit1, ndraws=1000, re_formula = NA) %>% 
    ggplot(aes(x = .prediction, y = management_rules)) +
    stat_slab(.width=0.95)
  
  
}