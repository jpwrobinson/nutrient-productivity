library(tidyverse)

# join mean nutrient concentrations with productivity estimates
## pivot and estimate nutrient productivity per nutrient
# definitions: https://github.com/jpwrobinson/nut-prod/issues/1
load(file = 'results/wcs_productivity.rds')

prod<-fishp %>% 
    pivot_longer(calcium.mg:vitamin_a.mug, names_to = 'nutrient', values_to = 'conc') %>% 
    mutate(nut_prod_day = conc / 100 * prod_g_day * 0.87,
           nut_biomass_kgha = conc * 0.87 * biomass_kgha)

## reef level estimates of nutrient productivity metrics
prod_reef<-prod %>% group_by(country, fish_taxon, trophic_group, management, site, sample_date, transect_number, depth, nutrient) %>% 
          summarise(
            nut_prod_day = sum(nut_prod_day), 
            nut_biomass = sum(nut_biomass),
            prod_day = sum(prod_g_day),
            biomass_g = sum(mass)) %>% 
          group_by(country, fish_taxon, trophic_group, management, site, depth, nutrient) %>% 
          summarise(
            nut_prod_day = mean(nut_prod_day), 
            nut_biomass = mean(nut_biomass),
            prod_day = mean(prod_day),
            biomass_g = mean(biomass_g)) %>% 
          ungroup() %>% 
          mutate(nut_turnover = nut_prod_day / nut_biomass,
                 biomass_turnover = prod_day / biomass_g) %>% 
          group_by(country,site, nutrient) %>% 
          summarise(nut_prod_day = sum(nut_prod_day), 
                    nut_biomass = sum(nut_biomass),
                    prod_day = sum(prod_day),
                    biomass_g = sum(biomass_g),
                    nut_turnover=sum(nut_turnover),
                    biomass_turnover = sum(biomass_turnover))

ggplot(prod_reef, aes(nut_prod_day, nut_turnover)) + geom_point() + facet_wrap(~nutrient, scales='free') +
      stat_smooth(method='gam')
ggplot(prod_reef, aes(prod_day, biomass_turnover)) + geom_point() + facet_wrap(~nutrient, scales='free') +
  stat_smooth(method='gam')
ggplot(prod_reef, aes(biomass_g, biomass_turnover)) + geom_point() + facet_wrap(~nutrient, scales='free') +
  stat_smooth(method='gam')
ggplot(prod_reef, aes(biomass_g, nut_turnover)) + geom_point()


pdf(file = 'fig/explore/wcs_nutrient_prod_reef.pdf', height=7, width=12)
ggplot(prod_reef, aes(biomass_g, nut_prod_day)) + geom_point() + facet_wrap(~nutrient, scales='free')
ggplot(prod_reef, aes(nut_biomass, nut_prod_day)) + geom_point() + facet_wrap(~nutrient, scales='free')
ggplot(prod_reef, aes(prod_day, nut_prod_day)) + geom_point() + facet_wrap(~nutrient, scales='free')
ggplot(prod_reef, aes(biomass_g, nut_turnover)) + geom_point() + facet_wrap(~nutrient, scales='free')
ggplot(prod_reef, aes(biomass_g, prod_day)) + geom_point() + facet_wrap(~nutrient, scales='free')
ggplot(prod_reef, aes(biomass_turnover, nut_turnover)) + geom_point() + facet_wrap(~nutrient, scales='free')
dev.off()

## species level estimates of nut prod metrics
prod_sp<-prod %>% 
  group_by(country, management, site, sample_date, species, transect_number, depth, nutrient, trophic_group) %>% 
  summarise(
    nut_prod_day = sum(nut_prod_day), 
    nut_biomass = sum(nut_biomass),
    prod_day = sum(prod_day),
    biomass_g = sum(mass)) %>% 
  ungroup() %>% 
  group_by(country, management, site, species, nutrient, trophic_group) %>% 
  summarise(
    nut_prod_day = mean(nut_prod_day), 
    nut_biomass = mean(nut_biomass),
    prod_day = mean(prod_day),
    biomass_g = mean(biomass_g)) %>% 
  ungroup() %>% 
  select(country, site, species, nutrient, trophic_group, nut_prod_day:biomass_g) %>% 
  complete(site, nutrient, nesting(species, trophic_group, country),
           fill = list(nut_prod_day = 0, nut_biomass = 0, prod_day =0, biomass_g = 0)) %>%
  # mutate(nut_turnover = nut_prod_day / nut_biomass,
  #        biomass_turnover = prod_day / biomass_g) %>% 
  group_by(country, species, nutrient, trophic_group) %>% 
  summarise(nut_prod_day = mean(nut_prod_day), 
            nut_biomass = mean(nut_biomass),
            prod_day = mean(prod_day),
            biomass_g = mean(biomass_g))

prod_sp$trophic_lab<-trophic.cols$FG_lab[match(prod_sp$trophic_group, trophic.cols$FG)]

# estimate top 20 nutrient productivity species, by nutrient
prod_top_nut<-prod_sp %>% ungroup() %>% group_by(nutrient) %>% slice_max(nut_prod_day, n=20) 


# scale nutrient productivity, estimate summed nutrient productivity across 6 nutriens
prod_scale<-prod_sp %>% group_by(nutrient) %>% 
      mutate(nut_prod_day_scale = scale(nut_prod_day)) %>% 
      ungroup() %>% 
      group_by(species, trophic_group, trophic_lab) %>% 
      summarise(nut_prod_score = sum(nut_prod_day_scale),
                biomass_g = unique(biomass_g))

pdf(file = 'fig/explore/wcs_nutrient_prod_species.pdf', height=18, width=20)
ggplot(prod_top_nut, aes(fct_reorder(species, nut_prod_day), nut_prod_day, fill=trophic_lab)) + 
    geom_bar(stat='identity') + coord_flip() + 
    facet_wrap(~nutrient, scales='free') +
  labs(x = '', y = 'Nutrient productivity') +
  scale_fill_manual(values = trophic_cols.named) +
  theme(legend.position = c(0.9, 0.1)) +
  scale_y_continuous(expand=c(0,0))

dev.off()

pdf(file = 'fig/explore/wcs_nutrient_prod_species_score.pdf', height=14, width=8)
ggplot(prod_scale, aes(fct_reorder(species, nut_prod_score), nut_prod_score, fill=trophic_lab)) + 
  geom_bar(stat='identity') +
  labs(x = '', y = 'Combined nutrient productivity score')+
  coord_flip() +
  scale_fill_manual(values = trophic_cols.named) +
  theme(legend.position = c(0.7, 0.1)) +
  scale_y_continuous(expand=c(0,0))
dev.off()


save(prod_reef, prod_sp, prod_scale, file = 'results/prodagascar_nut_prod.rds')