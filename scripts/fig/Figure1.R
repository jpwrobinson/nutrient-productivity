source('scripts/0_plot_theme.R')
set.seed(47)
## Script examining nutrient density and K of reef fishery target species
nut<-sey_species_nut %>% select(species, ends_with('mu'), -Protein_mu) %>% 
  mutate(nut_density = rowSums(across(c(Selenium_mu:Vitamin_A_mu))))

# join nutrient concentrations with K estimate
mad<-read.csv("data/wcs/madagascar_potential-prod-test_rfishprod.csv") %>% 
  distinct(species, Kmax, trophic_group)

nut<-left_join(nut, mad, by = 'species') %>% droplevels() 
nut$trophic_lab<-trophic.cols$FG_lab[match(nut$trophic_group, trophic.cols$FG)]

## species labs
sp<-str_split_fixed(nut$species, '\\ ', 2)
# nut$species.lab<-paste0(substring(sp[,1],1,5),  '. ', sp[,2], '.')
nut$species.lab<-paste0(sp[,1],'\n', sp[,2])


gg<-ggplot(nut, aes(Kmax, nut_density, col=trophic_lab)) + 
  geom_point(size=4, pch=21, col='black', aes(fill=trophic_lab)) +
  geom_label_repel(data = nut %>% filter(nut_density>400), aes(label = species.lab), fontface=1,size=2, show.legend=FALSE) +
  geom_label_repel(data = nut %>% filter(Kmax>0.4), aes(label = species.lab), fontface=1,size=2, show.legend=FALSE) +
  labs(x = 'Growth coefficent (Kmax)', y = 'Nutrient density, %') +
  scale_x_continuous(breaks=seq(0, 0.6, by = 0.1)) +
  th +
  theme(legend.position = c(0.85, 0.75)) +
  scale_fill_manual(values = trophic_cols.named) +
  scale_colour_manual(values = trophic_cols.named)

# load('results/madagascar_nut_prod.rds')
# mad_sp$Kmax<-nut$Kmax[match(mad_sp$species, nut$species)]
# mad_sp$nut_density<-nut$nut_density[match(mad_sp$species, nut$species)]
# ggplot(mad_sp, aes(log10(biomass_g), Kmax)) + geom_point()
# ggplot(mad_sp, aes(log10(biomass_g), nut_density)) + geom_point()

pdf(file = 'fig/Figure1.pdf', height=6, width=8)
gg
dev.off()