
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
threat<-threat %>% select(-country, -nutrient)

## transform skwewed predictors
threat$grav_nc<-log10(threat$grav_nc+1)
threat$pop_count<-log10(threat$pop_count+1)

# management
manage<-read.csv(file = 'data/wcs/mermaid_management_clean.csv') %>% select(-country)

# join prod estimates with benthic + fishing covariates
focal<-left_join(data.frame(prod_fg) %>% mutate(id2=paste(site, country, sep='_')), 
                 fish_avg %>% ungroup() %>%  
                   select(site, reef_type, reef_zone, management_rules, 
                          hard_coral, macroalgae, turf_algae, bare_substrate, depth, fish_richness, id2),
                 by='id2') %>% 
  left_join(threat, by = 'id2') %>% 
  # left_join(manage, by = 'site') %>% 
  # recode management_rules
  mutate(management_rules = recode(management_rules, 'periodic closure' = 'time restriction',
                                   'gear restriction' = 'gear restriction',
                                   'periodic closure; access restriction' = 'time restriction',
                                   'open access' = 'open-access',
                                   'no take' = 'no-take',
                                   'access restriction' = 'access restriction')) %>%
  filter(!is.na(depth)) %>%  # dropping 2 sites (NK02 in Madasgascar and WaiE1 in Fiji)
  # left_join(threat, by = 'site') %>% ungroup() ## lots of sites missing, incl. all of Belize
  mutate_if(is.character, as.factor) %>% 
  select(
    nutprop, nutrient, nutrient_lab, country, site, year, trophic_group, 
    hard_coral, macroalgae, turf_algae, bare_substrate, reef_type, reef_zone, depth,
    management_rules, grav_nc, sediment, nutrient_load, pop_count) %>%  
  mutate(management_rules = fct_relevel(management_rules, 'open-access', after=0))
# gears_category = fct_relevel(gears_category, 'None', after=0),
# times_category = fct_relevel(times_category, 'None', after=0)) ## open-access is reference level

## check reponse hist, bounded 0 - 1
hist(focal$nutprop, main = nut, xlab = 'Proportion of productivity')
focal$nutprop[focal$nutprop==1]<-0.99

write.csv(focal %>% pivot_wider(names_from = trophic_group, values_from = nutprop), 
          file = paste0('py-notebook/avg_nut_prod_unscaled.csv'))

