library(tidyverse)
source('scripts/0_plot_theme.R')
# join mean nutrient concentrations with productivity estimates
## pivot and estimate nutrient productivity per nutrient
# definitions: https://github.com/jpwrobinson/nut-prod/issues/1
load(file = 'results/wcs_productivity.rds')

prod<-fishp %>% rowwise() %>%mutate(nscore3 = sum(ca_rda, fe_rda, zn_rda)) %>% 
    pivot_longer(calcium.mg:vitamin_a.mug, names_to = 'nutrient', values_to = 'conc') %>% 
    mutate(nut_prod_day_ha = conc / 100 * prod_g_day_ha * 0.87, ## per gram edible yield
           nut_biomass_kgha = conc * 10 * 0.87 * biomass_kgha) ## per kg edible yield 

## change here to set the base FG for all analyses
prod<-prod %>% mutate(fg = trophic_group)
  
## reef level estimates of nutrient productivity metrics
prod_reef<-prod %>% group_by(country, fish_taxon, trophic_group, dietP, 
                             management, site, year, sample_date, transect_number, depth, nutrient, nscore3) %>% 
            summarise(
              nut_prod_day_ha = sum(nut_prod_day_ha), 
              nut_biomass_kgha = sum(nut_biomass_kgha),
              prod_day_ha = sum(prod_g_day_ha),
              biomass_kgha = sum(biomass_kgha)) %>% 
            group_by(country, fish_taxon, trophic_group, management, site, year, depth, nutrient, nscore3) %>% 
            summarise(
              nut_prod_day_ha = mean(nut_prod_day_ha), 
              nut_biomass_kgha = mean(nut_biomass_kgha),
              prod_day_ha = mean(prod_day_ha),
              biomass_kgha = mean(biomass_kgha)) %>% 
            ungroup() %>% 
            group_by(country,site, year, nutrient) %>% 
            summarise(nscore3 = weighted.mean(nscore3, w = biomass_kgha),
                      nut_prod_day_ha = sum(nut_prod_day_ha), 
                      nut_biomass_kgha = sum(nut_biomass_kgha),
                      prod_day_ha = sum(prod_day_ha),
                      biomass_kgha = sum(biomass_kgha)) %>% 
      mutate(nut_turnover = ((nut_prod_day_ha / 1000) / nut_biomass_kgha) * 100,
         biomass_turnover = ((prod_day_ha / 1000) / biomass_kgha) * 100) 


pdf(file = 'fig/explore/wcs_nutrient_prod_reef.pdf', height=7, width=12)
ggplot(prod_reef, aes(biomass_kgha, nut_prod_day_ha)) + geom_point() + facet_wrap(~nutrient, scales='free')
ggplot(prod_reef, aes(nut_biomass_kgha, nut_prod_day_ha)) + geom_point() + facet_wrap(~nutrient, scales='free')
ggplot(prod_reef, aes(prod_day_ha, nut_prod_day_ha)) + geom_point() + facet_wrap(~nutrient, scales='free')
ggplot(prod_reef, aes(biomass_kgha, nut_turnover)) + geom_point() + facet_wrap(~nutrient, scales='free')
ggplot(prod_reef, aes(biomass_kgha, prod_day_ha)) + geom_point() + facet_wrap(~nutrient, scales='free')
ggplot(prod_reef, aes(biomass_turnover, nut_turnover)) + geom_point() + facet_wrap(~nutrient, scales='free')
dev.off()

## species level estimates of nut prod metrics
prod_sp<-prod %>% 
  group_by(country, management_rules, site, year, sample_date, 
           fish_taxon, transect_number, depth, nutrient, fg) %>% 
  summarise(
    nut_prod_day_ha = sum(nut_prod_day_ha), 
    nut_biomass_kgha = sum(nut_biomass_kgha),
    prod_g_day_ha = sum(prod_g_day_ha),
    biomass_kgha = sum(biomass_kgha)) %>% 
  ungroup() %>% 
  dplyr::select(country, site,transect_number,year, management_rules,  fish_taxon, 
                nutrient, fg, nut_prod_day_ha:biomass_kgha) %>% 
  # group_by(nutrient, country, site, year) %>% 
  # complete(transect_number, nesting(fish_taxon, dietP, trophic_group, management_rules),
  #          fill = list(nut_prod_day_ha = 0, nut_biomass_kgha = 0, prod_g_day_ha =0, biomass_kgha = 0)) #%>% 
  group_by(country, management_rules, site, year, fish_taxon, nutrient, fg) %>%
  summarise(
    nut_prod_day_ha = mean(nut_prod_day_ha), 
    nut_biomass_kgha = mean(nut_biomass_kgha),
    prod_g_day_ha = mean(prod_g_day_ha),
    biomass_kgha = mean(biomass_kgha)) %>% 
  group_by(country, management_rules, nutrient, year) %>%
  complete(site, nesting(fish_taxon, fg),
           fill = list(nut_prod_day_ha = 0, nut_biomass_kgha = 0, prod_g_day_ha = 0, biomass_kgha = 0)) %>% 
  ungroup() %>% 
  # mutate(nut_turnover = nut_prod_day_ha / nut_biomass_kgha,
  #        biomass_turnover = prod_day / biomass_kg) %>% 
  group_by(country, fish_taxon, nutrient, fg) %>% 
  summarise(nut_prod_day_ha = mean(nut_prod_day_ha), 
            nut_biomass_kgha = mean(nut_biomass_kgha),
            prod_g_day_ha = mean(prod_g_day_ha),
            biomass_kgha = mean(biomass_kgha))
  
## Rows are filled with zeroes if species were observed in country-year-management_rule and are not in site
# prod_sp %>% group_by(site, country, year, management_rules) %>% summarise(n_distinct(fish_taxon))

# FG level nutrient productivity
prod_fg<-prod %>% 
  mutate(id = paste(site, year, sep = '_')) %>% 
  ## transect level: total metric by diet group
  group_by(country, site, year, id, transect_number,fg, nutrient) %>% 
  summarise(
    nut_prod_day_ha = sum(nut_prod_day_ha), 
    nut_biomass_kgha = sum(nut_biomass_kgha),
    prod_g_day_ha = sum(prod_g_day_ha),
    biomass_kgha = sum(biomass_kgha)) %>% 
  ungroup() %>% 
  group_by(country, site,year,fg, nutrient) %>% 
  summarise(
    nut_prod_day_ha = mean(nut_prod_day_ha), 
    nut_biomass_kgha = mean(nut_biomass_kgha),
    prod_g_day_ha = mean(prod_g_day_ha),
    biomass_kgha = mean(biomass_kgha)) %>% 
  group_by(nutrient, country, year) %>%
  complete(site,fg,
           fill = list(nut_prod_day_ha = 0, nut_biomass_kgha = 0, prod_g_day_ha =0, biomass_kgha = 0))  

## Rows are filled with zeroes if FG were not observed at a site
prod_fg %>% group_by(site, country, year) %>% summarise(n_distinct(fg))
  
## labelling for plots
# prod_sp$trophic_lab<-trophic.cols$FG_lab[match(prod_sp$trophic_group, trophic.cols$FG)]
prod_sp$fg_lab<-fg.cols$FG_lab[match(prod_sp$fg, trophic.cols$FG)]
# prod_sp$dietP_lab<-diet.cols$dietP_lab[match(prod_sp$dietP, diet.cols$dietP)]
# prod_fg$dietP_lab<-diet.cols$dietP_lab[match(prod_fg$dietP, diet.cols$dietP)]

# estimate top 20 nutrient productivity species, by nutrient
prod_top_nut<-prod_sp %>% ungroup() %>% 
      group_by(nutrient) %>% slice_max(nut_prod_day_ha, n=20) %>% 
      group_by(nutrient, fish_taxon, fg, fg_lab) %>% 
      summarise(nut_prod_day_ha = mean(nut_prod_day_ha))

# scale nutrient productivity, estimate summed nutrient productivity across 6 nutriens
prod_scale<-prod_sp %>% group_by(nutrient) %>% 
      mutate(nut_prod_day_ha_scale = scale(nut_prod_day_ha)) %>% 
      ungroup() %>% 
      group_by(fish_taxon, fg_lab) %>% 
      summarise(nut_prod_score = sum(nut_prod_day_ha_scale),
                biomass_kgha = unique(biomass_kgha))

pdf(file = 'fig/explore/wcs_nutrient_prod_species.pdf', height=18, width=20)
ggplot(prod_top_nut, aes(fct_reorder(fish_taxon, nut_prod_day_ha), nut_prod_day_ha, fill=fg_lab)) + 
    geom_bar(stat='identity') + coord_flip() + 
    facet_wrap(~nutrient, scales='free') +
  labs(x = '', y = 'Nutrient productivity') +
  scale_fill_manual(values = trophic_cols.named) +
  theme(legend.position = c(0.9, 0.1)) +
  scale_y_continuous(expand=c(0,0))

dev.off()

pdf(file = 'fig/explore/wcs_nutrient_prod_species_score.pdf', height=14, width=8)
ggplot(prod_scale, aes(fct_reorder(fish_taxon, nut_prod_score), nut_prod_score, fill=fg_lab)) + 
  geom_bar(stat='identity') +
  labs(x = '', y = 'Combined nutrient productivity score')+
  coord_flip() +
  scale_fill_manual(values = trophic_cols.named) +
  theme(legend.position = c(0.7, 0.1)) +
  scale_y_continuous(expand=c(0,0))
dev.off()


save(prod, prod_reef, prod_sp, prod_fg, prod_scale, file = 'results/wcs_nut_prod.rds')
