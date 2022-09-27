source('scripts/0_plot_theme.R')
th_ticks<-theme(axis.ticks.x = element_line(colour='black'))

load(file = 'results/wcs_productivity.rds')
load(file = 'results/wcs_nut_prod.rds')

fishp$fg_lab<-trophic.cols$FG_lab[match(fishp$fg, trophic.cols$FG)]
prod_fg$fg_lab<-trophic.cols$FG_lab[match(prod_fg$fg, trophic.cols$FG)]
prod_sp$fg_lab<-trophic.cols$FG_lab[match(prod_sp$fg, trophic.cols$FG)]

# fishp$fg_lab<-fg.cols$FG_lab[match(fishp$fg, fg.cols$FG)]
# prod_sp$fg_lab<-fg.cols$FG_lab[match(prod_sp$fg, fg.cols$FG)]
# prod_fg$fg_lab<-fg.cols$FG_lab[match(prod_fg$fg, fg.cols$FG)]

prod_sp<-prod_sp %>% group_by(nutrient, country) %>% 
  # arrange((nut_prod_day_ha))  %>% 
  mutate(tnut = sum(nut_prod_day_ha), nutprop = nut_prod_day_ha / tnut * 100) %>% 
  mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                               'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3\nfatty acids')) %>% 
  mutate(species = factor(ifelse(nutprop<5, 'Other', fish_taxon))) 


top5<-prod_sp %>% 
  group_by(nutrient, country) %>% 
  slice_max(nut_prod_day_ha, n = 5)

g1<-ggplot(prod_sp, aes(country, y=prod_g_day_ha, fill = fct_reorder(species, nutprop))) + geom_col(position = 'fill') + 
      theme(legend.position = 'right') +
      facet_grid(~nutrient) +
      scale_y_continuous(expand=c(0,0)) +
      scale_fill_manual(values = c('grey', RColorBrewer::brewer.pal(12, 'Paired'), RColorBrewer::brewer.pal(12, 'Set3')))


ggplot(prod_sp %>% group_by(country, nutrient) %>% slice_max(n = 5, nut_prod_day_ha), 
           aes(y=nut_prod_day_ha, nutrient)) + 
  geom_point(aes(col=country)) + 
  # theme(legend.position = 'right') +
  facet_wrap(~nutrient, scales='free') +
  # scale_y_continuous(expand=c(0,0)) +
  scale_fill_manual(values = c('grey', RColorBrewer::brewer.pal(12, 'Paired'), RColorBrewer::brewer.pal(12, 'Set3')))


pdf(file = 'fig/FigureSX_key_nutprod_species.pdf', height=7, width=22)
print(g1)
dev.off()



ggplot(prod_reef, aes(nut_turnover)) + geom_histogram() + facet_wrap(~nutrient, scales='free')

