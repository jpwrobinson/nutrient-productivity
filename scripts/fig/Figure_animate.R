pacman::p_load(gganimate, transformr)
## FG level nutrient productivity
source('scripts/0_plot_theme.R')

load(file = 'results/wcs_productivity.rds')
load(file = 'results/wcs_nut_prod.rds')
fishp$dietP_lab<-diet.cols$dietP_lab[match(fishp$dietP, diet.cols$dietP)]

# 
# prod_fg<-prod_fg %>% 
#   mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
#                                'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3\nfatty acids')) %>% 
#   group_by(country, site, nutrient,nutrient_lab) %>% 
#   mutate(nut_prod_day_ha_scaled = scale(nut_prod_day_ha)[,1]) %>% 
#   arrange((nut_prod_day_ha))  %>% 
#   mutate(tnut = sum(nut_prod_day_ha), 
#          nutprop = nut_prod_day_ha / tnut * 100) %>% 
#   filter(dietP_lab != 'Invertivore (sessile)')

prod_fg<-prod_fg %>%  
  mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                                                    'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3\nfatty acids'))


g3<-ggplot(prod_fg, aes((nut_prod_day_ha), col=dietP_lab, fill=dietP_lab)) + 
  geom_density(alpha=0.7) +
  # scale_x_continuous(breaks=seq(0, 100, 25), labels=c(0, '25%', '50%', '75%', '100%'), expand=c(0,0)) +
  scale_x_log10(breaks=c(0.1, 1, 10, 100, 1000), labels=c(0.1, 1, 10, 100, '1,000'), expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_manual(values = diet_cols.named) +
  scale_color_manual(values = diet_cols.named) +
  labs(x=expression(paste('daily nutrient production (day'^'-1',' ha'^'-1', ')')), y = 'density') +
  th + theme(legend.position = 'none', plot.title = element_text(size=16, face='bold', hjust=0.5, vjust=0.5)) +
  transition_states(nutrient_lab, transition_length = 4, state_length = 5) +
  ease_aes('cubic-in-out') + 
  # view_follow(fixed_x = TRUE)  +
  labs(title = '{closest_state}')

anim2<-animate(g3, renderer = magick_renderer(), res=300)
anim_save("fig/nut_prod.gif", anim2)
