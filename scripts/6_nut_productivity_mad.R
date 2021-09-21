source('scripts/5_add_nutrients.R')
nut<-sey_species_nut %>% 
        select(species, ends_with('mu'), -Protein_mu) %>% 
        rename_at(vars(ends_with('mu')), ~str_replace_all(.x, '_mu', ''))

# join mean nutrient concentrations with productivity estimates
## pivot and estimate nutrient productivity per nutrient
# definitions: https://github.com/jpwrobinson/nut-prod/issues/1
mad<-read.csv("data/wcs/madagascar_potential-prod-test_rfishprod.csv") %>% 
    left_join(nut, by = 'species') %>% 
    pivot_longer(Selenium:Vitamin_A, names_to = 'nutrient', values_to = 'conc') %>% 
    mutate(mass = a * size^b,
           nut_prod_day = conc / 100 * prod_day * 0.87,
           nut_biomass = conc * 0.87 * mass)

## reef level estimates of nutrient productivity metrics
mad_reef<-mad %>% group_by(country, species, trophic_group, management, site, sample_date, transect_number, depth, nutrient) %>% 
          summarise(
            nut_prod_day = sum(nut_prod_day), 
            nut_biomass = sum(nut_biomass),
            prod_day = sum(prod_day),
            biomass_g = sum(mass)) %>% 
          group_by(country, species, trophic_group, management, site, depth, nutrient) %>% 
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

pdf(file = 'fig/explore/mad_nutrient_prod_reef.pdf', height=7, width=12)
ggplot(mad_reef, aes(biomass_g, nut_prod_day)) + geom_point() + facet_wrap(~nutrient, scales='free')
ggplot(mad_reef, aes(nut_biomass, nut_prod_day)) + geom_point() + facet_wrap(~nutrient, scales='free')
ggplot(mad_reef, aes(prod_day, nut_prod_day)) + geom_point() + facet_wrap(~nutrient, scales='free')
ggplot(mad_reef, aes(biomass_g, nut_turnover)) + geom_point() + facet_wrap(~nutrient, scales='free')
ggplot(mad_reef, aes(biomass_g, prod_day)) + geom_point() + facet_wrap(~nutrient, scales='free')
ggplot(mad_reef, aes(biomass_turnover, nut_turnover)) + geom_point() + facet_wrap(~nutrient, scales='free')
dev.off()

## species level estimates of nut prod metrics
mad_sp<-mad %>% 
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

mad_sp$trophic_lab<-trophic.cols$FG_lab[match(mad_sp$trophic_group, trophic.cols$FG)]

# estimate top 20 nutrient productivity species, by nutrient
mad_top_nut<-mad_sp %>% ungroup() %>% group_by(nutrient) %>% slice_max(nut_prod_day, n=20) 


# scale nutrient productivity, estimate summed nutrient productivity across 6 nutriens
mad_scale<-mad_sp %>% group_by(nutrient) %>% 
      mutate(nut_prod_day_scale = scale(nut_prod_day)) %>% 
      ungroup() %>% 
      group_by(species, trophic_group, trophic_lab) %>% 
      summarise(nut_prod_score = sum(nut_prod_day_scale),
                biomass_g = unique(biomass_g))

pdf(file = 'fig/explore/mad_nutrient_prod_species.pdf', height=18, width=20)
ggplot(mad_top_nut, aes(fct_reorder(species, nut_prod_day), nut_prod_day, fill=trophic_lab)) + 
    geom_bar(stat='identity') + coord_flip() + 
    facet_wrap(~nutrient, scales='free') +
  labs(x = '', y = 'Nutrient productivity') +
  scale_fill_manual(values = trophic_cols.named) +
  theme(legend.position = c(0.9, 0.1)) +
  scale_y_continuous(expand=c(0,0))

dev.off()

pdf(file = 'fig/explore/mad_nutrient_prod_species_score.pdf', height=14, width=8)
ggplot(mad_scale, aes(fct_reorder(species, nut_prod_score), nut_prod_score, fill=trophic_lab)) + 
  geom_bar(stat='identity') +
  labs(x = '', y = 'Combined nutrient productivity score')+
  coord_flip() +
  scale_fill_manual(values = trophic_cols.named) +
  theme(legend.position = c(0.7, 0.1)) +
  scale_y_continuous(expand=c(0,0))
dev.off()


save(mad_reef, mad_sp, mad_scale, file = 'results/madagascar_nut_prod.rds')