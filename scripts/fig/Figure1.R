source('scripts/0_plot_theme.R')
set.seed(47)
## Script examining nutrient density and K of reef fishery target species
nut<-sey_species_nut %>% select(species, ends_with('mu'), -Protein_mu) %>% 
  mutate(nut_density = rowSums(across(c(Selenium_mu:Vitamin_A_mu)))) %>% 
  rename_at(vars(ends_with('mu')), ~str_replace_all(.x, '_mu', ''))

# join nutrient concentrations with K estimate
mad<-read.csv("data/wcs/madagascar_potential-prod-test_rfishprod.csv") %>% 
  distinct(species, Kmax, trophic_group)

nut<-left_join(nut, mad, by = 'species') %>% droplevels() 
nut$trophic_lab<-trophic.cols$FG_lab[match(nut$trophic_group, trophic.cols$FG)]

## species labs
sp<-str_split_fixed(nut$species, '\\ ', 2)
# nut$species.lab<-paste0(substring(sp[,1],1,5),  '. ', sp[,2], '.')
nut$species.lab<-paste0(sp[,1],'\n', sp[,2])

# long version
nutl <- nut %>% pivot_longer(Selenium:Vitamin_A, names_to = 'nutrient', values_to = 'conc')

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

gpan<-ggplot(nutl, aes(Kmax, conc, col=trophic_lab)) +
  geom_point(size=4, pch=21, col='black', aes(fill=trophic_lab)) +
  labs(x = 'Growth coefficent (Kmax)', y = expression(paste0('Nutrient concentration 100g'^'-1'))) +
  scale_x_continuous(breaks=seq(0, 0.6, by = 0.1)) +
  th +
  facet_wrap(~nutrient, scales='free', nrow=5) +
  theme(legend.position = 'none') +
  scale_fill_manual(values = trophic_cols.named) +
  scale_colour_manual(values = trophic_cols.named)


pdf(file = 'fig/Figure1.pdf', height=6, width=8)
gg
dev.off()

pdf(file = 'fig/FigureS1.pdf', height=6, width=8)
gpan
dev.off()