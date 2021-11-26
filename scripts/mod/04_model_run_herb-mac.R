pacman::p_load(tidyverse, skimr, cowplot, here, funk,disco, patchwork, bayesplot,
               broom, broom.mixed, rethinking, rstan, brms, tidybayes,emmeans, install=FALSE)
source('scripts/0_plot_theme.R')

#### RUN ON 
dp<-'herbivore-macroalgae'

## load datasets
load(file = 'results/wcs_productivity.rds')
load(file = 'results/wcs_nut_prod.rds')
load('data/wcs/wcs_fish_benthic.rds')

## recode and estimate nutrient proportion per site per fg
prod_fg<-prod_fg %>% 
  mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                               'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3\nfatty acids')) %>% 
  group_by(country, site, nutrient,nutrient_lab) %>% 
  mutate(tnut = sum(nut_prod_day_ha), 
         nutprop = nut_prod_day_ha / tnut) 

nut.vec<-unique(prod_fg$nutrient)

# model data
for(i in 1:length(nut.vec)){
  nut<-nut.vec[i]
  print(paste('Running model for', nut))
  source('scripts/mod/00_model_FG_prop_template.R')
}

for(i in 1:length(nut.vec)){
  nut<-nut.vec[i]
  print(paste('Compiling diagnostic figures for', nut))
  source('scripts/mod/00_model_diagnostic.R')
}

## master effect plot across nutrients
tmast<-numeric()
for(i in 1:length(nut.vec)){
  load(file = paste0('results/mods/', nut.vec[i],'_', dp, '_model.Rdata'))
  tt<-tidy(fit1, effects = "fixed") %>% filter(component == 'cond' & !str_detect(term, 'phi|Intercept')) %>%   
    mutate(nut = nut.vec[i])
  tmast<-rbind(tmast, tt)
}

pdf(file = paste0('results/betam_posteriors/post_summary_', dp, '.pdf'), height=7, width=12)
print(
  tmast %>% 
  ggplot(aes(term, estimate, ymin = conf.low, ymax = conf.high, col=nut)) + 
  geom_hline(yintercept = 0, col='grey', linetype=5) +
  geom_pointrange() + 
  coord_flip() +
  facet_wrap(~nut)+
  labs(subtitle = dp)
)
dev.off()

