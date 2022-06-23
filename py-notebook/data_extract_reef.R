
## Data prep for fitting dirichlet models in python

# ## set up model data
# nut<-'calcium.mg'

# Load reef pressure data
# https://github.com/WCS-Marine/local-reef-pressures
# devtools::load_all('../local-reef-pressures') ## fails
threat<-read.csv('data/threat/sites-threats.csv')  %>% 
  rename_at(vars(starts_with('andrello')), ~str_replace_all(., 'andrello_', '')) %>% 
  mutate(nutrient_load = nutrient, id2=paste(site, country, sep='_')) 

# get country averages for filling
threat_co<-threat %>% group_by(country) %>% summarise_at(vars(grav_nc:nutrient_load), mean, na.rm=TRUE)

summary(threat) ## 11 NAs in Fiji + Madagascar
threat$grav_nc[is.na(threat$grav_nc)]<-threat_co$grav_nc[match(threat$country[is.na(threat$grav_nc)], threat_co$country)]
threat$sediment[is.na(threat$sediment)]<-threat_co$sediment[match(threat$country[is.na(threat$sediment)], threat_co$country)]
threat$nutrient_load[is.na(threat$nutrient_load)]<-threat_co$nutrient_load[match(threat$country[is.na(threat$nutrient_load)], threat_co$country)]
threat$pop_count[is.na(threat$pop_count)]<-threat_co$pop_count[match(threat$country[is.na(threat$pop_count)], threat_co$country)]
threat<-threat %>% dplyr::select(-country, -nutrient)

## transform skwewed predictors
threat$grav_nc<-log10(threat$grav_nc)
threat$pop_count<-log10(threat$pop_count+1)
threat$sediment<-log10(threat$sediment+1)
threat$nutrient_load<-log10(threat$nutrient_load+1)

# management
manage<-read.csv(file = 'data/wcs/mermaid_management_clean.csv') %>% dplyr::select(-country)

# join prod estimates with benthic + fishing covariates
focal<-left_join(data.frame(prod_reef) %>% mutate(id=paste(site, year, country, sep='_'), 
                                                  id2=paste(site, country, sep='_')), 
                 fish_avg %>% ungroup() %>%  
                   dplyr::select(site, reef_type, reef_zone, management_rules, 
                          hard_coral, macroalgae, turf_algae, bare_substrate, rubble, depth, fish_richness,id, id2),
                 by=c('id', 'id2')) %>% 
  left_join(threat, by = 'id2') %>% 
  # left_join(manage, by = 'site') %>% 
  # recode management_rules
  # mutate(management_rules = recode(management_rules, 'periodic closure' = 'time restriction',
  #                                  'gear restriction' = 'gear restriction',
  #                                  'periodic closure; access restriction' = 'time restriction',
  #                                  'open access' = 'open-access',
  #                                  'no take' = 'no-take',
  #                                  'access restriction' = 'access restriction')) %>%
  mutate(management_rules = recode(management_rules, 'periodic closure' = 'restriction',
                                   'gear restriction' = 'restriction',
                                   'periodic closure; access restriction' = 'restriction',
                                   'open access' = 'open-access',
                                   'no take' = 'no-take',
                                   'access restriction' = 'restriction')) %>% 
  filter(!is.na(depth)) %>%  # dropping 2 sites (NK02 in Madasgascar and WaiE1 in Fiji)
  # left_join(threat, by = 'site') %>% ungroup() ## lots of sites missing, incl. all of Belize
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(
    nut_turnover, nut_prod_day_ha, prod_day_ha, biomass_kgha, nscore3, nutrient, nutrient_lab, country, site, year, 
    hard_coral, macroalgae, turf_algae, bare_substrate,rubble, reef_type, reef_zone, depth,
    management_rules, grav_nc, sediment, nutrient_load, pop_count) %>%  
  filter(nutrient==nut) %>% 
  mutate(management_rules = fct_relevel(management_rules, 'open-access', after=0))
# gears_category = fct_relevel(gears_category, 'None', after=0),
# times_category = fct_relevel(times_category, 'None', after=0)) ## open-access is reference level

## check reponse hist
hist(focal$nut_turnover, main = nut, xlab = 'Nutrient turnover, %')

## scale numeric, pivot wider
source('scripts/scaler.R')
focal.scaled<-scaler(focal, 
                     ID = c('biomass_kgha','nut_turnover','nut_prod_day_ha','prod_day_ha','nscore3', 'nutrient','nutrient_lab',
                            'country', 'site','year','id2',
                             'reef_type', 'reef_zone',
                            'management_rules'), cats = FALSE) 

write.csv(focal, file = paste0('py-notebook/', nut, '_reef_unscaled.csv'))
write.csv(focal.scaled, file = paste0('py-notebook/', nut, '_reef_scaled.csv'))
