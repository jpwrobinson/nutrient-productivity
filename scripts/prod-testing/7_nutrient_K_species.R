source('scripts/0_plot_theme.R')

## Script examining nutrient density and K of reef fishery target species
nut<-sey_species_nut %>% select(species, ends_with('mu'), -Protein_mu) %>% 
      mutate(nut_density = rowSums(across(c(Selenium_mu:Vitamin_A_mu))))

# join nutrient concentrations with K estimate
mad<-read.csv("data/wcs/madagascar_potential-prod-test_rfishprod.csv") %>% 
  distinct(species, Kmax, trophic_group)

nut<-left_join(nut, mad, by = 'species') %>% droplevels()

ggplot(nut, aes(Kmax, nut_density, col=trophic_group)) + geom_point() +
      geom_label(data = nut %>% filter(nut_density>400), aes(label = species), size=2) +
      geom_label(data = nut %>% filter(Kmax>0.4), aes(label = species), size=2) 

g2<-ggplot(nut, aes(fct_reorder(species, Kmax), Kmax, col=nut_density)) + 
      geom_point() +
      coord_flip() +
      labs(x='', y = 'Kmax')

g3<-ggplot(nut, aes(fct_reorder(species, nut_density), nut_density, col=Kmax)) + 
  geom_point() +
  coord_flip() +
  labs(x='', y = 'Nutrient density, %')

pdf(file = 'fig/explore/species_Kmax.pdf', height=12, width = 5)
g2
dev.off()

pdf(file = 'fig/explore/species_nutrient_density.pdf', height=12, width = 5)
g3
dev.off()