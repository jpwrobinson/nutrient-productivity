pacman::p_load(tidyverse, skimr, cowplot, here, funk,disco, patchwork, 
               broom, broom.mixed, rethinking, rstan, brms, tidybayes,emmeans, install=FALSE)
source('scripts/0_plot_theme.R')

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
  group_by(country, site, nutrient,nutrient_lab) %>% 
  mutate(tnut = sum(nut_prod_day_ha), 
         nutprop = nut_prod_day_ha / tnut) 

## set up model data
dp<-'Herbivores Microvores Detritivores'
nut<-'calcium.mg'

# read benthic
load('data/wcs/wcs_fish_benthic.rds')
focal<-left_join(prod_fg, 
                fish_avg %>% ungroup() %>%  
                  select(site, reef_type, reef_zone, management_rules, hard_coral, macroalgae, turf_algae, bare_substrate, 
                                                   depth, fish_richness),
                by='site') %>% 
        filter(!is.na(depth)) %>%  # dropping 2 sites (NK02 in Madasgascar and WaiE1 in Fiji)
        # left_join(threat, by = 'site') %>% ungroup() ## lots of sites missing, incl. all of Belize
        ## convert data to list, characters to factors for rethinking
        ungroup() %>% droplevels() %>%  mutate_if(is.character, as.factor) %>% 
  select(
    nutprop, nutrient, nutrient_lab, country, site, dietP, 
    hard_coral, macroalgae, turf_algae, bare_substrate, reef_type, reef_zone, depth,
    management_rules) %>%  # grav_NC, management_rules, sediment, nutrient_load, pop_count) 
  filter(nutrient==nut & dietP==dp)
  
## scale numeric
focal.scaled<-scaler(focal, 
                   ID = c('nutprop', 'country', 'site', 'dietP', 'reef_type', 'reef_zone',
                          'management_rules'), cats = FALSE) 

## check reponse hist, bounded 0 - 1
hist(focal.scaled$nutprop)
  
## testing multinomial model
fit1 <-
     brm(
      bf(nutprop ~ hard_coral + turf_algae + macroalgae + bare_substrate + depth + management_rules + (1 | country),
         phi ~ hard_coral + turf_algae + macroalgae + bare_substrate + depth + management_rules + (1 | country),
         zi ~ 1),
      data = focal.scaled,
      family = zero_inflated_beta(),
      chains = 4, iter = 3000, warmup = 1000,
      cores = 4, seed = 1234
    )

## PDF the posterior effects

pdf(file = paste0('results/betam_posteriors/', nut,'/post_summary', dp, '.pdf'), height=7, width=12)
tidy(fit1, effects = "fixed") %>% filter(component == 'cond' & !str_detect(term, 'phi')) %>% 
    ggplot(aes(term, estimate, ymin = conf.low, ymax = conf.high)) + 
    geom_hline(yintercept = 0, col='grey', linetype=5) +
    geom_pointrange() + 
    coord_flip() +
    labs(subtitle = paste(nut, dp))
dev.off()

pdf(file = paste0('results/betam_posteriors/', nut,'/management_', dp, '.pdf'), height=7, width=12)
plot(fit1, pars = 'b_management')
dev.off()

pdf(file = paste0('results/betam_posteriors/', nut,'/benthic_', dp, '.pdf'), height=7, width=12)
plot(fit1, pars = 'b_turf_algae|b_macroalgae|b_hard_coral|b_bare|b_depth')
dev.off()


save(focal, focal.scaled, fit1, file = paste0('results/mods/', nut,'_', dp, '_model_.Rdata'))