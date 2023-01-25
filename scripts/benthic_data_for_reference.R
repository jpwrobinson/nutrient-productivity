pacman::p_load(tidyverse, skimr, cowplot, here, funk,disco, patchwork, mermaidr, janitor, install=FALSE)

ben<-read.csv(file='data/mermaidr_csv/benthic_sites.csv') %>% 
		group_by(site, id, id2) %>% 
		summarise_at(vars(sand:other_benthos), mean) %>% 
		ungroup() %>% 
		select(-site)

## estimating total biomass by species / fg per transect, then average per site
fish_sp<-read.csv(file='data/mermaidr_csv/fish_individuals.csv') %>% 
		group_by(id, id2, project, country, site, latitude, longitude, reef_type, reef_zone, reef_exposure,
			management, management_rules, sample_date, transect_number,
			fish_family, fish_genus, fish_taxon, trophic_group, year) %>% 
			summarise(biomass_kgha = sum(biomass_kgha)) %>% 
		group_by(id, id2, country, site, latitude, longitude, reef_type, reef_zone, reef_exposure,
			management, management_rules,
			fish_family, fish_genus, fish_taxon, trophic_group, year) %>% 
		summarise(biomass_kgha = mean(biomass_kgha)) %>% 
		left_join(ben, by =c('id', 'id2'))

fish_fg<-read.csv(file='data/mermaidr_csv/fish_individuals.csv') %>% 
		group_by(id, id2, project, country, site, latitude, longitude, reef_type, reef_zone, reef_exposure,
			management, management_rules, sample_date, transect_number, trophic_group, year) %>% 
			summarise(biomass_kgha = sum(biomass_kgha)) %>% 
		group_by(id, id2, country, site, latitude, longitude, reef_type, reef_zone, reef_exposure,
			management, management_rules, trophic_group, year) %>% 
		summarise(biomass_kgha = mean(biomass_kgha)) %>% 
		pivot_wider(names_from = trophic_group, values_from = 'biomass_kgha', values_fill=0) %>% 
		clean_names() %>% 
		rowwise() %>% 
		mutate(biomass_kgha = herbivore_detritivore + herbivore_macroalgae + invertivore_mobile +
			invertivore_sessile + omnivore + piscivore + planktivore) %>% 
		left_join(ben, by =c('id', 'id2'))

## drop sites missing benthic data
drops<-c('WaiE1_2019_Fiji')
drops<-read.csv('missing_benthic_data.csv') %>% mutate(id = paste(site, year, country, sep = '_')) %>% pull(id)

fish_sp<-fish_sp %>% filter(!id %in% drops)
fish_fg<-fish_fg %>% filter(!id %in% drops)


## get site average for report cards
fish_avg<-fish_fg %>% 
	# select(-na) %>% 
	ungroup() %>% 
	group_by(id2, country, site, reef_type, reef_zone, management_rules) %>% 
	summarise_at(vars(latitude, longitude, herbivore_detritivore:other_benthos), mean)  

## get species richness (fish)
rich<-fish_sp %>% group_by(site) %>% summarise(richness=n_distinct(fish_taxon))
fish_avg$fish_richness<-rich$richness[match(fish_avg$site, rich$site)]

save(fish_sp, fish_fg, fish_avg, file = 'data/wcs_fish_benthic.rds')


## missing trohpic groups
fish_sp %>% filter(is.na(trophic_group)) %>% ungroup() %>% distinct(fish_taxon)
# 1 Gnathodentex aureolineatus     
# 2 Herklotsichthys quadrimaculatus
# 3 Lactophrys triqueter 

## checking dupes (none currently)
nn<-fish_fg %>% group_by(year)  %>%  count(id2) %>% filter(n > 1)
nn
fish_fg %>% filter(id %in% nn$id) %>% data.frame()
# fish_avg %>% filter(id %in% nn$id) %>% data.frame()