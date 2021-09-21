
## FG level nutrient productivity
source('scripts/0_plot_theme.R')

load(file = 'results/madagascar_nut_prod.rds')
mad_sp$trophic_lab<-trophic.cols$FG_lab[match(mad_sp$trophic_group, trophic.cols$FG)]

mad_sp<-mad_sp %>% group_by(nutrient) %>% 
            arrange((nut_prod_day))  %>% 
            mutate(tnut = sum(nut_prod_day), nutprop = nut_prod_day / tnut * 100)

mad_fg<-mad_sp %>% group_by(trophic_lab,nutrient) %>% 
                      summarise(
                        nut_prod_day = sum(nut_prod_day),
                        prod_day = sum(prod_day),
                        biomass_g = sum(biomass_g)) %>% 
                  group_by(nutrient) %>% 
                  mutate(nut_prod_day_scaled = scale(nut_prod_day)) %>% 
                  arrange((nut_prod_day))  %>% 
                  mutate(tnut = sum(nut_prod_day), nutprop = nut_prod_day / tnut * 100)

## average across nutrients for panel A
mad_fg2<-mad_fg %>% group_by(trophic_lab) %>% summarise(se = se(nutprop), nutprop = mean(nutprop)) %>% 
        mutate(lower = nutprop - 2*se, upper = nutprop + 2*se)

ggplot(mad_fg, aes(nutrient, nut_prod_day, fill=trophic_lab)) + 
      geom_bar(stat='identity') +
      coord_flip() +
      theme(legend.position = 'none') +
      scale_fill_manual(values = trophic_cols.named) +
      scale_color_manual(values = 'white') +
      facet_wrap(~nutrient, scales='free', nrow=6)

ggplot(mad_fg, aes(scale(prod_day), 
                    nut_prod_day_scaled, col=trophic_lab)) + 
          geom_point() + geom_abline(intercept=0, slope=1 ,col='grey', linetype=5) +
          # geom_smooth(method='lm', se = FALSE) +
          scale_colour_manual(values = trophic_cols.named)
  
g1<-ggplot(mad_fg, aes(fct_reorder(trophic_lab, nutprop),nutprop, fill=trophic_lab),col='black') + 
  # geom_segment(aes(x = fct_reorder(trophic_lab, nut_prod_day), xend =fct_reorder(trophic_lab, nut_prod_day), y =-Inf, yend = nut_prod_day), col='grey') +
  geom_jitter(size=2, pch=21) +
  geom_pointrange(data = mad_fg2, aes(ymin = lower, ymax = upper), size=1.5, pch=21) +
  coord_flip() +
  th + theme(legend.position = 'none') +
  scale_fill_manual(values = trophic_cols.named)  +
  scale_color_manual(values = trophic_cols.named) +
  labs(x = '', y = "Mean proportion of nutrient productivity") 

g2<-ggplot(mad_sp, aes(log10(biomass_g), nutprop, col=trophic_lab)) + 
  geom_point(alpha=0.8, size=2) + facet_wrap(~nutrient, scales='free') +
  scale_color_manual(values = trophic_cols.named) +
  labs(x = 'Log10 biomass, g', y = 'Proportion of total nutrient productivity, %') +
  th + theme(legend.position = 'none')


pdf(file='fig/Figure2.pdf', height=6, width=14)
print(
  plot_grid(g1, g2, nrow=1, labels=c('A', 'B'), rel_widths=c(0.7, 1))
)
dev.off()