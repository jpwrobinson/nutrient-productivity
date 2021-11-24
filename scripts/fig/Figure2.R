
## FG level nutrient productivity
source('scripts/0_plot_theme.R')

load(file = 'results/wcs_productivity.rds')
load(file = 'results/wcs_nut_prod.rds')
fishp$dietP_lab<-diet.cols$dietP_lab[match(fishp$dietP, diet.cols$dietP)]
fishp$trophic_lab<-trophic.cols$FG_lab[match(fishp$trophic_group, trophic.cols$FG)]
prod_fg$trophic_lab<-trophic.cols$FG_lab[match(prod_fg$trophic_group, trophic.cols$FG)]

prod_sp<-prod_sp %>% group_by(nutrient, country) %>% 
            # arrange((nut_prod_day_ha))  %>% 
            mutate(tnut = sum(nut_prod_day_ha), nutprop = nut_prod_day_ha / tnut * 100) %>% 
            mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                                     'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3\nfatty acids'))

prod_fg<-prod_fg %>% 
                  mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                               'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3\nfatty acids')) %>% 
                  group_by(country, nutrient,nutrient_lab) %>% 
                  mutate(nut_prod_day_ha_scaled = scale(nut_prod_day_ha)[,1]) %>% 
                  arrange((nut_prod_day_ha))  %>% 
                  mutate(tnut = sum(nut_prod_day_ha), 
                         nutprop = nut_prod_day_ha / tnut * 100) %>% 
                  group_by(trophic_lab, nutrient, nutrient_lab) %>% 
                  summarise(nutprop = mean(nutprop)) %>% 
                  mutate(id = paste(trophic_lab, nutrient))

prod_fg_country<-prod_sp %>% 
  group_by(country, trophic_lab,nutrient,nutrient_lab) %>% 
  summarise(
    nut_prod_day_ha = sum(nut_prod_day_ha),
    prod_g_day_ha = sum(prod_g_day_ha),
    biomass_kgha = sum(biomass_kgha)) %>% 
  group_by(nutrient, country) %>% 
  mutate(nut_prod_day_ha_scaled = scale(nut_prod_day_ha)) %>% 
  arrange((nut_prod_day_ha))  %>% 
  mutate(tnut = sum(nut_prod_day_ha), nutprop = nut_prod_day_ha / tnut * 100)

## average across nutrients for panel A
prod_fg2<-prod_fg %>% group_by(trophic_lab) %>% summarise(se = funk::se(nutprop), nutprop = mean(nutprop)) %>% 
        mutate(lower = nutprop - 2*se, upper = nutprop + 2*se)

## average nut contr by country + nutrient
prod_fg_co<-prod_fg_country %>% group_by(nutrient,nutrient_lab, country, trophic_lab) %>% 
  summarise(se = funk::se(nutprop), nutprop = mean(nutprop)) %>% 
  mutate(lower = nutprop - 2*se, upper = nutprop + 2*se)

prod_fg_co_biom<-prod_fg_country %>% filter(nutrient=='calcium.mg') %>% 
        group_by(country) %>% mutate(tb = sum(biomass_kgha), tp = sum(prod_g_day_ha)) %>% 
        group_by(tb, tp, country, trophic_lab) %>% 
        summarise(biomass = sum(biomass_kgha), prod = sum(prod_g_day_ha)) %>% 
        mutate('Standing biomass' = biomass / tb * 100, 
               'Daily productivity' = prod / tp * 100) %>% 
        pivot_longer('Standing biomass':'Daily productivity', values_to = 'nutprop', names_to = 'nutrient') %>% 
        mutate(nutrient_lab = nutrient)

prod_fg_co<-rbind(prod_fg_co, prod_fg_co_biom) 
prod_fg_co$trophic_lab<-factor(prod_fg_co$trophic_lab, levels=rev(unique(prod_fg_co$trophic_lab)[c(7,2,1,4,3,5,6)]))
prod_fg_co$nutrient_lab<-factor(prod_fg_co$nutrient_lab, levels=rev(unique(prod_fg_co$nutrient_lab)[c(7,8,1,2,4,6,3,5)]))

## rank country by median biomass
meds<-prod_reef %>% filter(nutrient == 'calcium.mg') %>% group_by(country) %>% 
        summarise(med = median(biomass_kgha)) %>% 
        arrange(desc(med))

prod_fg_co$country<-factor(prod_fg_co$country, levels = meds$country)
prod_reef$country<-factor(prod_reef$country, levels = meds$country)

# ggplot(prod_fg, aes(scale(prod_day_ha), 
#                     nut_prod_day_ha_scaled, col=trophic_lab)) + 
#           geom_point() + geom_abline(intercept=0, slope=1 ,col='grey', linetype=5) +
#           # geom_smooth(method='lm', se = FALSE) +
#           scale_colour_manual(values = trophic_cols.named)
# 

# plabs<-c('Herbivore/detritivore zinc.mg', 'Herbivore/detritivore vitamin_a.mug', 'Invertivore (mobile) vitamin_a.mug')
g1<-ggplot(prod_fg, aes(fct_reorder(trophic_lab, nutprop),nutprop, fill=trophic_lab),col='black') + 
  # geom_segment(aes(x = fct_reorder(trophic_lab, nut_prod_day_ha), xend =fct_reorder(trophic_lab, nut_prod_day_ha), y =-Inf, yend = nut_prod_day_ha), col='grey') +
  geom_jitter(size=2, pch=21, width = 0.25) +
  geom_pointrange(data = prod_fg2, aes(ymin = lower, ymax = upper), size=1.5, pch=21) +
  # geom_label_repel(data = prod_fg %>% filter(id %in% plabs), aes(label=nutrient_lab), fill='white', size=2) +
  coord_flip() +
  th + theme(legend.position = 'none') +
  scale_fill_manual(values = trophic_cols.named)  +
  scale_color_manual(values = trophic_cols.named) +
  scale_y_continuous(breaks=seq(0, 0.8, by = 0.1), labels=seq(0, 80, by = 10)) +
  labs(x = '', y = "Mean proportion of nutrient productivity, %") 

g2<-ggplot(prod_fg_co, aes(nutrient_lab, nutprop, fill=trophic_lab)) + 
  geom_bar(stat='identity') +
  coord_flip() +
  theme(legend.position = 'none') +
  labs(x = '', y = 'Proportion of assemblage biomass, productivity or nutrient production, %') +
  scale_fill_manual(values = trophic_cols.named) +
  scale_color_manual(values = 'white') +
  scale_y_continuous(expand=c(0,0)) +
  facet_grid(~country, scales='free') + th +
  theme(legend.position = 'none', strip.text.x = element_blank(), plot.margin=unit(c(0, 0.5, 0.1, 0.1), 'cm')) 

## biom distributions
g3<-ggplot(prod_reef %>% filter(nutrient == 'calcium.mg') %>% mutate(x = log10(biomass_kgha)),
                aes(x = x)) +
        geom_histogram(col = 'black', fill='grey60', bins = 10) +
        facet_grid(~country) +
        labs(x = 'Log10 biomass kgha', y = 'N sites') +
        stat_summary(aes(x = 0.1, y = x, xintercept = stat(y), group = country), 
               fun = median, geom = "vline", linetype = 5, col='#e31a1c',size = 0.8) +
        scale_y_continuous(expand=c(0,0)) +
        scale_x_continuous(expand=c(0,0)) +
        th

## sup fig
g4<-ggplot(prod_sp, aes(log10(biomass_kgha), nutprop, col=trophic_lab)) + 
  geom_point(alpha=0.8, size=1.5) + facet_wrap(~nutrient, scales='free') +
  scale_color_manual(values = trophic_cols.named) +
  labs(x = 'Log10 biomass, kg ha-1', y = 'Proportion of total nutrient productivity, %') +
  th + theme(legend.position = 'none')


pdf(file='fig/Figure2.pdf', height=5, width=14)
rp<-plot_grid(g3, g2, nrow =2, rel_heights=c(0.5, 1), align='v')
print(
  plot_grid(g1, rp, nrow=1, labels=c('A', 'B'), rel_widths=c(0.7, 1))
)
dev.off()

pdf(file='fig/FigureS2.pdf', height=6, width=10)
print(g4)
dev.off()

