pacman::p_load(tidyverse, skimr, cowplot, here, funk,disco, patchwork, rethinking,  install=FALSE)

# Load reef pressure data
# https://github.com/WCS-Marine/local-reef-pressures
# devtools::load_all('../local-reef-pressures') ## fails
threat<-read.csv('data/wcs/wcs_threat_indicators.csv') %>% select(-X)

## load fish nut prod, estimate proportional by FG
load(file = 'results/wcs_productivity.rds')
load(file = 'results/wcs_nut_prod.rds')

focal<-prod %>% 
  # transect-level: sum biom + prod for each FG
  group_by(country, management, site, sample_date, transect_number, depth, nutrient, dietP, trophic_group) %>% 
  summarise(
    nut_prod_day_ha = sum(nut_prod_day_ha), 
    nut_biomass_kgha = sum(nut_biomass_kgha),
    prod_g_day_ha = sum(prod_g_day_ha),
    biomass_kgha = sum(biomass_kgha)) %>% 
  ungroup() %>% 
  # site-level: complete biom + prod by species for each transect, then average
  dplyr::select(transect_number, country, site, nutrient, dietP, trophic_group, nut_prod_day_ha:biomass_kgha) %>% 
  mutate(id = paste(transect_number, site, sep = '_')) %>% 
  group_by(site, country, nutrient) %>% 
  complete(nesting(trophic_group, dietP, site, transect_number), 
           fill = list(nut_prod_day_ha = 0, nut_biomass_kgha = 0, prod_g_day_ha =0, biomass_kgha = 0)) %>%
  group_by(country, management, site, nutrient, dietP, trophic_group) %>% 
  summarise(
    nut_prod_day_ha = mean(nut_prod_day_ha), 
    nut_biomass_kgha = mean(nut_biomass_kgha),
    prod_day_ha = mean(prod_g_day_ha),
    biomass_kgha = mean(biomass_kgha)) %>% 
  ungroup() %>% 
  
  group_by(country, site, nutrient, dietP, trophic_group) %>% 
  summarise(nut_prod_day_ha = mean(nut_prod_day_ha), 
            nut_biomass_kgha = mean(nut_biomass_kgha),
            prod_day_ha = mean(prod_day_ha),
            biomass_kgha = mean(biomass_kgha))


## drop periods from names
fish$country.x<-NULL
fish$latitude.x<-NULL
fish$longitude.x<-NULL

sw<-unique(fish$site[fish$latitude  < -20])
ne<-unique(fish$site[fish$latitude  > -18 & fish$latitude  < -14.1])
nw<-unique(fish$site[fish$latitude  > -15 ])

fish$seascape<-'Northwest'
fish$seascape[fish$site %in% sw]<-'Southwest'
fish$seascape[fish$site %in% ne]<-'Northeast'

## vector of seascapes for each site, and countries for each seascape

## set up model data
## convert data to list, characters to factors for rethinking
fish<-droplevels(fish %>% mutate_if(is.character, as.factor)) %>% select(
  biomass_kgha_avg, country, site, seascape, hard_coral, macroalgae, reef_type, reef_zone, reef_exposure, depth_avg, we, grav_NC, management_rules, year_fish, sediment, nutrient, npp
) 

## scale numeric
focal.list<-scaler(fish, ID = c('biomass_kgha_avg', 'country', 'site', 'seascape', 'reef_type', 'reef_zone','reef_exposure', 'management_rules', 'year_fish'), cats = FALSE) %>% as.list()

str(focal.list)
# add list of region ID for each country name
focal.list$seascapeLookup<-fish %>% distinct(site, seascape) %>% pull(seascape) %>% as.factor()
focal.list$countryLookup<-fish %>% distinct(seascape, country) %>% pull(country) %>% as.factor()
focal.list$year_fish<-as.factor(focal.list$year_fish)

## check reponse hist
hist(focal.list$biomass_kgha_avg)
hist(log(focal.list$biomass_kgha_avg))

m_biom <- ulam(
  alist(
    biomass_kgha_avg ~ dlnorm(mu, sigma) ,
    # biomass_kgha_avg ~ dgamma2(mu, scale) ,
    
    ## mu (biomass) from seascape-level intercepts and site-level covariates
    log(mu)<- seascape_X[seascape] + 
      coral*hard_coral + ma*macroalgae +  ## benthic
      # reef_ex[reef_exposure] + reef_ty[reef_type] + reef_zo[reef_zone] + 
      depth*depth_avg + wave*we + ## biophysical small-scale
      sed*sediment + nutr*nutrient + productivity*npp +# temperature*temp + ## biophysical large-scale,
      management[management_rules] + gravity*grav_NC + ## human
      year[year_fish], ## confounding / correlated
    
    # # ## seascape intercepts, sampling normal from the mean of each country
    seascape_X[seascape] ~ dnorm(mu_seascape, sigma_seascape),
    # 
    # ## now linking site to country
    # ## country-level intercepts and seascape-level covariates [maybe don't exist?]
    # # mu_country <- country_X[countryLookup],
    # 
    # # ## country intercepts, drawing from global mu
    # # country_X[countryLookup] ~ dnorm(mu_country, sigma_country),
    # 
    # ## global intercept, plus country-level covariates [HDI / population size / ???]
    mu_seascape <- y ,
    
    # 
    # # weak prior on 0
    y ~ dnorm(0,10),
    
    # seascape_X[seascape] ~ dnorm(0,1),
    
    ## weak prior on continuous
    c(coral,
      ma,
      depth,
      wave,
      gravity,
      sed,
      nutr,
      productivity
    ) ~ dnorm(0, 10),
    
    ## weak prior on categorical
    # reef_ex[reef_exposure] ~ dnorm(0, 1),
    # reef_ty[reef_type] ~ dnorm(0, 1),
    # reef_zo[reef_zone] ~ dnorm(0, 1),
    management[management_rules] ~ dnorm(0, 1),
    year[year_fish] ~ dnorm(0, 1),
    
    
    ## hyper-priors
    sigma ~ dexp(2),
    sigma_seascape ~ dexp(1) # Variation among seascapes
    # sigma_country ~ dexp(1) # Variation among country
  ) , 
  data=focal.list , chains=3 , cores=6, control=list(adapt_delta=0.99), warmup=1500, iter=3000, log_lik=TRUE )

dashboard(m_biom)
precis(m_biom)
plot(precis(m_biom,2))

## compare model predictions
p.index<-link(m_biom)$mu ## note this returns predictions for mu_small and mu_large too, so only taking $mu here
index_mu<-apply( p.index , 2 , median )

plot(log(focal.list$biomass_kgha_avg), index_mu)
abline(0, 1)

save(m_biom, fish, focal.list, file = 'results/mod.rds')


