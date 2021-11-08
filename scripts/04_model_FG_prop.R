pacman::p_load(tidyverse, skimr, cowplot, here, funk,disco, patchwork, rethinking,  install=FALSE)

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
  group_by(country, nutrient,nutrient_lab) %>% 
  mutate(nut_prod_day_ha_scaled = scale(nut_prod_day_ha)[,1]) %>% 
  arrange((nut_prod_day_ha))  %>% 
  mutate(tnut = sum(nut_prod_day_ha), 
         nutprop = nut_prod_day_ha / tnut * 100) 

## set up model data

# read benthic
load('data/wcs/wcs_fish_benthic.rds')
focal<-left_join(prod_fg, 
                fish_avg %>% ungroup() %>%  select(site, reef_type, reef_zone, management_rules, hard_coral, macroalgae, turf_algae, bare_substrate, 
                                                   depth, fish_richness),
                by='site') %>% 
        filter(!is.na(depth)) %>%  # dropping 2 sites (NK02 in Madasgascar and WaiE1 in Fiji)
        left_join(threat, by = 'site') %>% ungroup() 

## convert data to list, characters to factors for rethinking
focal<-droplevels(focal %>% mutate_if(is.character, as.factor)) %>% select(
  nutprop, nutrient, nutrient_lab, country, site, dietP, hard_coral, macroalgae, turf_algae, bare_substrate, reef_type, reef_zone, depth,
  grav_NC, management_rules, sediment, nutrient_load, pop_count) 

## scale numeric
focal.list<-scaler(focal %>% filter(nutrient=='calcium.mg'), ID = c('nutprop', 'country', 'site', 'dietP', 'reef_type', 'reef_zone', 'management_rules'), cats = FALSE) %>% as.list()

str(focal.list)
# add list of region ID for each country name
focal.list$countryLookup<-focal %>% distinct(site, country) %>% pull(country) %>% as.factor()

## check reponse hist
hist(focal.list$nutprop)
hist(log10(focal.list$nutprop))


m <- ulam(
  alist(
    nutprop ~ dlnorm(mu, sigma) ,
    # nutprop ~ dgamma2(mu, scale) ,
    
    ## mu (biomass) from seascape-level intercepts and site-level covariates
    log(mu)<- site_X[site] + 
      coral*hard_coral + ma*macroalgae + turf*turf_algae +  ## benthic
      # reef_ex[reef_exposure] + reef_ty[reef_type] + reef_zo[reef_zone] + 
      depth*depth + # wave*we + ## biophysical small-scale
      nutr*nutrient_load + #productivity*npp +# temperature*temp + ## biophysical large-scale,
      management[management_rules] + gravity*grav_NC , ## human
    
    # ## ## now linking site to country
    site_X[site] ~ dnorm(mu_country, sigma_country),
    mu_country <- country_X[countryLookup],

    # ## global intercept, plus country-level covariates [HDI / population size / ???]
    country_X <- y ,
    
    # # weak prior on 0
    y ~ dnorm(0,10),
    
    site_X[site] ~ dnorm(0,1),
    
    ## weak prior on continuous
    c(coral,
      ma,
      depth,
      # wave,
      gravity,
      # sed,
      nutr
    ) ~ dnorm(0, 10),
    
    ## weak prior on categorical
    # reef_ex[reef_exposure] ~ dnorm(0, 1),
    # reef_ty[reef_type] ~ dnorm(0, 1),
    # reef_zo[reef_zone] ~ dnorm(0, 1),
    management[management_rules] ~ dnorm(0, 1),
    
    ## hyper-priors
    sigma ~ dexp(2),
    sigma_country ~ dexp(1) # Variation among sites
  ) , 
  data=focal.list , chains=3 , cores=6, control=list(adapt_delta=0.99), warmup=1500, iter=3000, log_lik=TRUE )

dashboard(m)
precis(m)
plot(precis(m,2))

## compare model predictions
p.index<-link(m)$mu ## note this returns predictions for mu_small and mu_large too, so only taking $mu here
index_mu<-apply( p.index , 2 , median )

plot(log(focal.list$nutprop), index_mu)
abline(0, 1)

save(m, focal.list, file = 'results/mod.rds')


