pacman::p_load(tidyverse, skimr, cowplot, here, funk,disco, patchwork, mermaidr, mermaidreporting, install=FALSE)
source('scripts/0_plot_theme.R')



## Figure viz of nutrient density and K of reef fishery target species

## load Kmax predictions for WCS fish
load(file = 'results/wcs_productivity.rds')
## get average Kmax, as these have uncertainty and were predicted for each observed species in fish
fish2<-fishp %>% group_by(fish_taxon, trophic_group, dietP, lmax, nscore,
                            calcium.mg, iron.mg, selenium.mug, zinc.mg, omega3.g, vitamin_a.mug) %>% 
    summarise(Kmax =mean(Kmax)) 

fish2$trophic_lab<-trophic.cols$FG_lab[match(fish2$trophic_group, trophic.cols$FG)]
fish2$dietP_lab<-diet.cols$dietP_lab[match(fish2$dietP, diet.cols$dietP)]

## species labs
sp<-str_split_fixed(fish2$fish_taxon, '\\ ', 2)
# fish2$species.lab<-paste0(substring(sp[,1],1,5),  '. ', sp[,2], '.')
fish2$species.lab<-paste0(sp[,1],'\n', sp[,2])

# long version
fish2l <- fish2 %>% pivot_longer(calcium.mg:vitamin_a.mug, names_to = 'nutrient', values_to = 'conc') %>% 
  mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                               'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3\nfatty acids'))
fish2l$nutrient_lab<-factor(fish2l$nutrient_lab, levels=(unique(fish2l$nutrient_lab)[c(1,2,4,6,3,5)])

gg<-ggplot(fish2, aes(log10(Kmax), nscore, col=dietP_lab)) + 
  geom_point(size=3.5, pch=21, col='black', aes(fill=dietP_lab)) +
  # geom_label_repel(data = fish2 %>% filter(nscore>400), aes(label = species.lab), fontface=1,size=2, show.legend=FALSE) +
  # geom_label_repel(data = fish2 %>% filter(Kmax>0.4), aes(label = species.lab), fontface=1,size=2, show.legend=FALSE) +
  labs(x = 'Derived growth coefficent (Kmax)', y = 'Nutrient density, %') +
  # scale_x_continuous(breaks=seq(0, 0.6, by = 0.1)) +
  th +
  theme(legend.position = c(0.8, 0.88), 
        panel.border=element_rect(color='black'),
        axis.line = element_blank()) +
  scale_fill_manual(values = diet_cols.named) +
  scale_colour_manual(values = diet_cols.named)

gpan<-ggplot(fish2l, aes(log10(Kmax), conc)) +
  geom_point(size=2.5, pch=21, col='black', aes(fill=dietP_lab)) +
  labs(x = 'Derived growth coefficent (Kmax)', y = conc_lab) +
  # scale_x_continuous(breaks=seq(0, 0.6, by = 0.1)) +
  th +
  facet_wrap(~nutrient_lab, scales='free', nrow=5) +
  theme(legend.position = 'right') +
  scale_fill_manual(values = diet_cols.named) +
  scale_colour_manual(values = diet_cols.named) +
  geom_smooth(method = 'gam', col='black')


pdf(file = 'fig/Figure1.pdf', height=6, width=8)
print(ggExtra::ggMarginal(gg, type='histogram', fill='grey50', col='white', bins=20))
dev.off()

pdf(file = 'fig/FigureS1.pdf', height=8, width=11)
print(gpan)
dev.off()

