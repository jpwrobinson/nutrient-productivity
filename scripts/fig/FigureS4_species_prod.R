
## FG level nutrient productivity
source('scripts/0_plot_theme.R')

load(file = 'results/wcs_productivity.rds')
load(file = 'results/wcs_nut_prod.rds')
fishp$dietP_lab<-diet.cols$dietP_lab[match(fishp$dietP, diet.cols$dietP)]

prod_sp<-prod_sp %>% group_by(nutrient, country) %>% 
  # arrange((nut_prod_day_ha))  %>% 
  mutate(tnut = sum(nut_prod_day_ha), nutprop = nut_prod_day_ha / tnut * 100) %>% 
  mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                               'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3\nfatty acids'))

spp<-prod_sp %>% ungroup() %>% 
    distinct(fish_taxon, dietP, dietP_lab, country, prod_g_day_ha) %>% 
    group_by(fish_taxon, dietP_lab) %>% 
    summarise(prod_g_day_ha = mean(prod_g_day_ha)) %>% 
    ungroup() %>% 
    mutate(fish_taxon = fct_reorder(fish_taxon, prod_g_day_ha)) 

ggplot(spp %>% 
         slice_max(fish_taxon, n = 20), aes(fish_taxon, prod_g_day_ha, col=dietP_lab)) + 
      geom_point(size=4) +
      coord_flip() +
      scale_color_manual(values = diet_cols.named) +
      labs('productivity, grams day hectare')

ggplot(spp , aes(fish_taxon, prod_g_day_ha, col=dietP_lab)) + 
  geom_point(size=4) +
  coord_flip() +
  scale_color_manual(values = diet_cols.named) +
  labs('productivity, grams day hectare')

ggplot(prod_fg, aes((prod_g_day_ha), fill=dietP_lab)) + 
  geom_density(alpha=0.7, col='transparent') +
  # coord_flip() +
  # facet_wrap(~country) +
  scale_x_log10(labels=scales::comma) +
  scale_fill_manual(values = diet_cols.named) +
  labs('productivity, grams day hectare') +
  theme(legend.position = 'none')  

