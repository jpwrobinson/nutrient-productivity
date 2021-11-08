library(tidyverse)
source('scripts/0_plot_theme.R')
# join mean nutrient concentrations with productivity estimates
## pivot and estimate nutrient productivity per nutrient
# definitions: https://github.com/jpwrobinson/nut-prod/issues/1
load(file = 'results/wcs_productivity.rds')

prod<-fishp %>% 
    pivot_longer(calcium.mg:vitamin_a.mug, names_to = 'nutrient', values_to = 'conc') %>% 
    mutate(nut_prod_day_ha = conc / 100 * prod_g_day_ha * 0.87,
           nut_biomass_kgha = conc * 0.87 * biomass_kgha)
  
## reef level estimates of nutrient productivity metrics
prod_reef<-prod %>% group_by(country, fish_taxon, trophic_group, dietP, management, site, sample_date, transect_number, depth, nutrient) %>% 
            summarise(
              nut_prod_day_ha = sum(nut_prod_day_ha), 
              nut_biomass_kgha = sum(nut_biomass_kgha),
              prod_day_ha = sum(prod_g_day_ha),
              biomass_kgha = sum(biomass_kgha)) %>% 
            group_by(country, fish_taxon, trophic_group, management, site, depth, nutrient) %>% 
            summarise(
              nut_prod_day_ha = mean(nut_prod_day_ha), 
              nut_biomass_kgha = mean(nut_biomass_kgha),
              prod_day_ha = mean(prod_day_ha),
              biomass_kgha = mean(biomass_kgha)) %>% 
            ungroup() %>% 
            group_by(country,site, nutrient) %>% 
            summarise(nut_prod_day_ha = sum(nut_prod_day_ha), 
                      nut_biomass_kgha = sum(nut_biomass_kgha),
                      prod_day_ha = sum(prod_day_ha),
                      biomass_kgha = sum(biomass_kgha)) %>% 
      mutate(nut_turnover = nut_prod_day_ha / nut_biomass_kgha,
         biomass_turnover = prod_day_ha / biomass_kgha) 

ggplot(prod_reef, aes(nut_prod_day_ha, nut_turnover)) + geom_point() + facet_wrap(~nutrient, scales='free') +
      stat_smooth(method='gam')
ggplot(prod_reef, aes(prod_day_ha, biomass_turnover)) + geom_point() + facet_wrap(~nutrient, scales='free') +
  stat_smooth(method='gam')
ggplot(prod_reef, aes(biomass_kgha, biomass_turnover)) + geom_point() + facet_wrap(~nutrient, scales='free') +
  stat_smooth(method='gam')
ggplot(prod_reef, aes(biomass_kgha, nut_turnover)) + geom_point() + facet_wrap(~nutrient, scales='free')


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
  group_by(country, management, site, sample_date, fish_taxon, transect_number, depth, nutrient, dietP, trophic_group) %>% 
  summarise(
    nut_prod_day_ha = sum(nut_prod_day_ha), 
    nut_biomass_kgha = sum(nut_biomass_kgha),
    prod_g_day_ha = sum(prod_g_day_ha),
    biomass_kgha = sum(biomass_kgha)) %>% 
  ungroup() %>% 
  dplyr::select(country, site,transect_number,management,  fish_taxon, nutrient, dietP, trophic_group, nut_prod_day_ha:biomass_kgha) %>% 
  group_by(nutrient) %>% 
  complete(transect_number, nesting(fish_taxon, dietP, trophic_group, site, country),
           fill = list(nut_prod_day_ha = 0, nut_biomass_kgha = 0, prod_g_day_ha =0, biomass_kgha = 0)) %>% 
  group_by(country, management, site, fish_taxon, nutrient, dietP, trophic_group) %>% 
  summarise(
    nut_prod_day_ha = mean(nut_prod_day_ha), 
    nut_biomass_kgha = mean(nut_biomass_kgha),
    prod_g_day_ha = mean(prod_g_day_ha),
    biomass_kgha = mean(biomass_kgha)) %>% 
  ungroup() %>% 
  # mutate(nut_turnover = nut_prod_day_ha / nut_biomass_kgha,
  #        biomass_turnover = prod_day / biomass_kg) %>% 
  group_by(country, fish_taxon, nutrient, dietP, trophic_group) %>% 
  summarise(nut_prod_day_ha = mean(nut_prod_day_ha), 
            nut_biomass_kgha = mean(nut_biomass_kgha),
            prod_g_day_ha = mean(prod_g_day_ha),
            biomass_kgha = mean(biomass_kgha))

prod_sp$trophic_lab<-trophic.cols$FG_lab[match(prod_sp$trophic_group, trophic.cols$FG)]
prod_sp$dietP_lab<-diet.cols$dietP_lab[match(prod_sp$dietP, diet.cols$dietP)]

# estimate top 20 nutrient productivity species, by nutrient
prod_top_nut<-prod_sp %>% ungroup() %>% group_by(nutrient) %>% slice_max(nut_prod_day_ha, n=20) %>% 
      group_by(nutrient, fish_taxon, dietP, dietP_lab) %>% 
      summarise(nut_prod_day_ha = mean(nut_prod_day_ha))
  

# scale nutrient productivity, estimate summed nutrient productivity across 6 nutriens
prod_scale<-prod_sp %>% group_by(nutrient) %>% 
      mutate(nut_prod_day_ha_scale = scale(nut_prod_day_ha)) %>% 
      ungroup() %>% 
      group_by(fish_taxon, dietP_lab) %>% 
      summarise(nut_prod_score = sum(nut_prod_day_ha_scale),
                biomass_kgha = unique(biomass_kgha))

pdf(file = 'fig/explore/wcs_nutrient_prod_species.pdf', height=18, width=20)
ggplot(prod_top_nut, aes(fct_reorder(fish_taxon, nut_prod_day_ha), nut_prod_day_ha, fill=dietP_lab)) + 
    geom_bar(stat='identity') + coord_flip() + 
    facet_wrap(~nutrient, scales='free') +
  labs(x = '', y = 'Nutrient productivity') +
  scale_fill_manual(values = diet_cols.named) +
  theme(legend.position = c(0.9, 0.1)) +
  scale_y_continuous(expand=c(0,0))

dev.off()

pdf(file = 'fig/explore/wcs_nutrient_prod_species_score.pdf', height=14, width=8)
ggplot(prod_scale, aes(fct_reorder(fish_taxon, nut_prod_score), nut_prod_score, fill=dietP_lab)) + 
  geom_bar(stat='identity') +
  labs(x = '', y = 'Combined nutrient productivity score')+
  coord_flip() +
  scale_fill_manual(values = diet_cols.named) +
  theme(legend.position = c(0.7, 0.1)) +
  scale_y_continuous(expand=c(0,0))
dev.off()


save(prod, prod_reef, prod_sp, prod_scale, file = 'results/wcs_nut_prod.rds')
