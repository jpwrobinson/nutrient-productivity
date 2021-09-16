source('scripts/5_add_nutrients.R')
nut<-sey_species_nut %>% select(species, ends_with('mu'), -Protein_mu)

# join mean nutrient concentrations with productivity estimates
## pivot and estimate nutrient productivity per nutrient
# definitions: https://github.com/jpwrobinson/nut-prod/issues/1
mad<-read.csv("data/wcs/madagascar_potential-prod-test_rfishprod.csv") %>% 
    left_join(nut, by = 'species') %>% 
    pivot_longer(Selenium_mu:Vitamin_A_mu, names_to = 'nutrient', values_to = 'conc') %>% 
    mutate(mass = a * size^b,
           nut_prod_day = conc / 100 * prod_day * 0.87,
           nut_biomass = conc * 0.87 * mass)


mad_reef<-mad %>% group_by(country, management, site, sample_date, transect_number, depth, nutrient) %>% 
          summarise(
            nut_prod_day = sum(nut_prod_day), 
            nut_biomass = sum(nut_biomass),
            prod_day = sum(prod_day),
            biomass_g = sum(mass)) %>% 
          mutate(nut_turnover = nut_prod_day / nut_biomass,
                 biomass_turnover = prod_day / biomass_g)

ggplot(mad_reef, aes(biomass_g, nut_prod_day)) + geom_point() + facet_wrap(~nutrient, scales='free')
ggplot(mad_reef, aes(nut_biomass, nut_prod_day)) + geom_point() + facet_wrap(~nutrient, scales='free')
ggplot(mad_reef, aes(prod_day, nut_prod_day)) + geom_point() + facet_wrap(~nutrient, scales='free')
ggplot(mad_reef, aes(biomass_g, nut_turnover)) + geom_point() + facet_wrap(~nutrient, scales='free')
ggplot(mad_reef, aes(biomass_g, prod_day)) + geom_point() + facet_wrap(~nutrient, scales='free')
ggplot(mad_reef, aes(biomass_turnover, nut_turnover)) + geom_point() + facet_wrap(~nutrient, scales='free')