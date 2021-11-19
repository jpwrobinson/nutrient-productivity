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
        filter(nutrient==nut & dietP==dp)


## scale numeric
source('scripts/scaler.R')
focal.scaled<-scaler(focal, 
                   ID = c('nutprop','nutrient','nutrient_lab', 'country', 'site','year',
                          'dietP', 'reef_type', 'reef_zone',
                          'management_rules'), cats = FALSE) 

## check reponse hist, bounded 0 - 1
print(
    hist(focal.scaled$nutprop)
    )
  
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
      chains = 4, iter = 3000, warmup = 1000,
      cores = 4, seed = 43
    )

## pred vs. obs
pdf(file = paste0('results/betam_posteriors/', nut,'/post_obs_', dp, '.pdf'), height=7, width=12)
plot(predict(fit1)[,1], focal.scaled$nutprop, xlim=c(0,1))
abline(0,1)

ppc_dens_overlay(y = focal.scaled$nutprop,
                 yrep = posterior_predict(fit1, nsamples = 100))


# pareto diagnostic
loo(fit1)

## PDF the posterior effects
pdf(file = paste0('results/betam_posteriors/', nut,'/post_summary_', dp, '.pdf'), height=7, width=12)
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


save(focal, focal.scaled, fit1, file = paste0('results/mods/', nut,'_', dp, '_model.Rdata'))
