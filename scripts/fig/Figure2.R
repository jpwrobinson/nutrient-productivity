
## FG level nutrient productivity
source('scripts/0_plot_theme.R')

load(file = 'results/madagascar_nut_prod.rds')
mad_sp$trophic_lab<-trophic.cols$FG_lab[match(mad_sp$trophic_group, trophic.cols$FG)]

mad_fg<-mad_sp %>% group_by(trophic_lab,nutrient) %>% 
                      summarise(
                        nut_prod_day = sum(nut_prod_day),
                        prod_day = sum(prod_day),
                        biomass_g = sum(biomass_g)) %>% 
                  group_by(nutrient) %>% 
                  mutate(nut_prod_day_scaled = scale(nut_prod_day))

mad_fg2<-mad_fg %>% group_by(trophic_lab) %>% 
  summarise(
    nut_prod_day = mean(nut_prod_day_scaled),
    prod_day = unique(prod_day),
    biomass_g = unique(biomass_g)) 

ggplot(mad_fg, aes(nutrient, nut_prod_day, fill=trophic_lab)) + 
      geom_bar(stat='identity') +
      coord_flip() +
      theme(legend.position = 'none') +
      scale_fill_manual(values = trophic_cols.named) +
      scale_color_manual(values = 'white') +
      facet_wrap(~nutrient, scales='free', nrow=6)

ggplot(mad_sp, aes(trophic_lab, nut_prod_day, fill=species)) + 
  geom_bar(stat='identity') +
  coord_flip() +
  theme(legend.position = 'none') +
  # scale_fill_manual(values = trophic_cols.named) +  
  scale_color_manual(values = 'white') +
  facet_wrap(~nutrient, scales='free', nrow=6)

ggplot(mad_fg, aes(scale(prod_day), 
                    nut_prod_day_scaled, col=trophic_lab)) + 
          geom_point() + geom_abline(intercept=0, slope=1 ,col='grey', linetype=5) +
          # geom_smooth(method='lm', se = FALSE) +
          scale_colour_manual(values = trophic_cols.named)
  
g1<-ggplot(mad_fg2, aes(fct_reorder(trophic_lab, nut_prod_day),nut_prod_day, fill=trophic_lab),col='black') + 
  geom_segment(aes(x = fct_reorder(trophic_lab, nut_prod_day), xend =fct_reorder(trophic_lab, nut_prod_day), y =-Inf, yend = nut_prod_day), col='grey') +
  geom_point(size=5, pch=21) +
  coord_flip() +
  th + theme(legend.position = 'none') +
  scale_fill_manual(values = trophic_cols.named)  +
  scale_color_manual(values = trophic_cols.named) +
  labs(x = '', y = "Mean nutrient productivity (scaled across 6 nutrients)") 

pdf(file='fig/Figure2.pdf', height=6, width=9)
g1
dev.off()