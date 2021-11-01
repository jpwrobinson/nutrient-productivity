
## FG level nutrient productivity
source('scripts/0_plot_theme.R')

load(file = 'results/wcs_productivity.rds')
load(file = 'results/wcs_nut_prod.rds')
fishp$dietP_lab<-diet.cols$dietP_lab[match(fishp$dietP, diet.cols$dietP)]

prod_sp<-prod_sp %>% group_by(nutrient) %>% 
            arrange((nut_prod_day_ha))  %>% 
            mutate(tnut = sum(nut_prod_day_ha), nutprop = nut_prod_day_ha / tnut * 100)

prod_fg<-prod_sp %>% group_by(dietP_lab, nutrient) %>% 
                      summarise(
                        nut_prod_day_ha = sum(nut_prod_day_ha),
                        prod_day_ha = sum(prod_day_ha),
                        biomass_kgha = sum(biomass_kgha)) %>% 
                  group_by(nutrient) %>% 
                  mutate(nut_prod_day_ha_scaled = scale(nut_prod_day_ha)[,1]) %>% 
                  arrange((nut_prod_day_ha))  %>% 
                  mutate(tnut = sum(nut_prod_day_ha), nutprop = nut_prod_day_ha / tnut * 100, id = paste(dietP_lab, nutrient))

prod_fg_country<-prod_sp %>% group_by(country, dietP_lab,nutrient) %>% 
  summarise(
    nut_prod_day_ha = sum(nut_prod_day_ha),
    prod_day_ha = sum(prod_day_ha),
    biomass_kgha = sum(biomass_kgha)) %>% 
  group_by(nutrient, country) %>% 
  mutate(nut_prod_day_ha_scaled = scale(nut_prod_day_ha)) %>% 
  arrange((nut_prod_day_ha))  %>% 
  mutate(tnut = sum(nut_prod_day_ha), nutprop = nut_prod_day_ha / tnut * 100)

## average across nutrients for panel A
prod_fg2<-prod_fg %>% group_by(dietP_lab) %>% summarise(se = se(nutprop), nutprop = mean(nutprop)) %>% 
        mutate(lower = nutprop - 2*se, upper = nutprop + 2*se)

ggplot(prod_fg, aes(nutrient, nut_prod_day_ha, fill=dietP_lab)) + 
      geom_bar(stat='identity') +
      coord_flip() +
      theme(legend.position = 'none') +
      scale_fill_manual(values = diet_cols.named) +
      scale_color_manual(values = 'white') +
      facet_wrap(~nutrient, scales='free', nrow=6)

## average nut contr by country + nutrient
prod_fg_co<-prod_fg_country %>% group_by(nutrient, country, dietP_lab) %>% summarise(se = se(nutprop), nutprop = mean(nutprop)) %>% 
  mutate(lower = nutprop - 2*se, upper = nutprop + 2*se)

prod_fg_co_biom<-prod_fg_country %>% filter(nutrient=='calcium.mg') %>% 
        group_by(country) %>% mutate(tb = sum(biomass_kgha)) %>% 
        group_by(tb, country, dietP_lab) %>% summarise(biomass = sum(biomass_kgha)) %>% 
        mutate(nutprop = biomass / tb*100, nutrient="Biomass, kgha")

prod_fg_co<-rbind(prod_fg_co, prod_fg_co_biom)

ggplot(prod_fg_co, aes(nutrient, nutprop, fill=dietP_lab)) + 
  geom_bar(stat='identity') +
  coord_flip() +
  theme(legend.position = 'none') +
  scale_fill_manual(values = diet_cols.named) +
  scale_color_manual(values = 'white') +
  facet_grid(~country, scales='free')

ggplot(prod_fg, aes(scale(prod_day_ha), 
                    nut_prod_day_ha_scaled, col=dietP_lab)) + 
          geom_point() + geom_abline(intercept=0, slope=1 ,col='grey', linetype=5) +
          # geom_smooth(method='lm', se = FALSE) +
          scale_colour_manual(values = diet_cols.named)


plabs<-c('Herbivore/detritivore zinc.mg', 'Herbivore/detritivore vitamin_a.mug', 'Invertivore (mobile) vitamin_a.mug')
g1<-ggplot(prod_fg, aes(fct_reorder(dietP_lab, nutprop),nutprop, fill=dietP_lab),col='black') + 
  # geom_segment(aes(x = fct_reorder(dietP_lab, nut_prod_day_ha), xend =fct_reorder(dietP_lab, nut_prod_day_ha), y =-Inf, yend = nut_prod_day_ha), col='grey') +
  geom_jitter(size=2, pch=21, width = 0.25) +
  geom_pointrange(data = prod_fg2, aes(ymin = lower, ymax = upper), size=1.5, pch=21) +
  geom_label(data = prod_fg %>% filter(id %in% plabs), aes(label=nutrient), fill='white', size=2) +
  coord_flip() +
  th + theme(legend.position = 'none') +
  scale_fill_manual(values = diet_cols.named)  +
  scale_color_manual(values = diet_cols.named) +
  labs(x = '', y = "Mean proportion of nutrient productivity") 

g2<-ggplot(prod_sp, aes(log10(biomass_kgha), nutprop, col=dietP_lab)) + 
  geom_point(alpha=0.8, size=1.5) + facet_wrap(~nutrient, scales='free') +
  scale_color_manual(values = diet_cols.named) +
  labs(x = 'Log10 biomass, kg ha-1', y = 'Proportion of total nutrient productivity, %') +
  th + theme(legend.position = 'none')


pdf(file='fig/Figure2.pdf', height=6, width=14)
print(
  plot_grid(g1, g2, nrow=1, labels=c('A', 'B'), rel_widths=c(0.7, 1))
)
dev.off()

pdf(file='fig/FigureS2.pdf', height=6, width=10)
print(g2)
dev.off()