pacman::p_load(tidyverse, skimr, cowplot, here, funk,disco, patchwork, bayesplot,tidybayes,
               broom, broom.mixed, rethinking, rstan, brms, tidybayes,emmeans, install=FALSE)
source('scripts/0_plot_theme.R')

dp1<-'herbivore-detritivore'
dp2<-'piscivore'
dp3<-'invertivore-mobile'
dp4<-'herbivore-macroalgae'
dps<-c(dp1, dp2, dp3, dp4)

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

## sample management effects for each FG
nut.vec<-unique(prod_fg$nutrient)

pp<-numeric()
for(i in 1:length(dps)){
  load(file = paste0('results/mods/', nut.vec[6],'_', dps[i], '_model.Rdata'))
  pred<-focal.scaled %>% modelr::data_grid(management_rules = 'time restriction',
                                           year=2016,
                                           country = 'Fiji',
                                           hard_coral = min(hard_coral),
                                           turf_algae = max(turf_algae),
                                           macroalgae = max(macroalgae),
                                           bare_substrate = max(bare_substrate),
                                           depth = 0,
                                           grav_nc = 0,
                                           pop_count = 0) %>% 
    add_epred_draws(fit1, ndraws=1000, re_formula = NA) %>% 
    mutate(fg = dps[i])
  pp<-rbind(pp, data.frame(pred))
}
meds_managed_degraded<-pp %>% #group_by(fg) %>% 
      # summarise(med = median(.epred)) %>% 
      mutate(type = 'Managed, degraded')



pp<-numeric()
for(i in 1:length(dps)){
  load(file = paste0('results/mods/', nut.vec[6],'_', dps[i], '_model.Rdata'))
  pred<-focal.scaled %>% modelr::data_grid(management_rules = 'time restriction',
                                           year=2016,
                                           country = 'Fiji',
                                           hard_coral = max(hard_coral),
                                           turf_algae = min(turf_algae),
                                           macroalgae = min(macroalgae),
                                           bare_substrate = 1,
                                           depth = 0,
                                           grav_nc = 0,
                                           pop_count = 0) %>% 
    add_epred_draws(fit1, ndraws=1000, re_formula = NA) %>% 
    mutate(fg = dps[i])
  pp<-rbind(pp, data.frame(pred))
}
meds_managed_intact<-pp %>% group_by(fg) %>%
      # summarise(med = median(.epred)) %>%
      mutate(type = 'Managed, intact')


mm<-rbind(meds_managed_degraded, meds_managed_intact)

trophic.cols<-trophic.cols %>% mutate(fg = FG, FG_lab2 = str_replace_all(FG_lab, '\\ ', '\n'))
mm<-mm %>% left_join(trophic.cols, by = 'fg') 
trophic_cols.named2<-setNames(as.character(trophic.cols$col), trophic.cols$FG_lab2)

pdf(file = 'fig/temp/figure_temp.pdf', height=5, width=5)
ggplot(mm %>% filter(type %in% c('Managed, intact')), 
        aes(FG_lab2, .epred, col=FG_lab2, fill=FG_lab2, shape=type)) + 
  stat_pointinterval(.width=0.95, show_slab=FALSE, scale=0.9, stroke=3, side='left', position = position_dodge(width=0.2)) +
  scale_colour_manual(values = trophic_cols.named2) +
  scale_fill_manual(values = trophic_cols.named2) +
  scale_y_continuous(limits=c(0.1, 0.5)) +
  th + theme(legend.position = 'none') +
  labs(x = '', y = 'Proportion of nutrient production, %')

ggplot(mm %>% filter(type %in% c('Managed, degraded', 'Managed, intact')), 
       aes(FG_lab2, .epred, col=FG_lab2, fill=FG_lab2, shape=fct_rev(type))) + 
  stat_pointinterval(.width=0.95, show_slab=FALSE, scale=0.9, stroke=3, side='left', position = position_dodge(width=0.2)) +
  scale_colour_manual(values = trophic_cols.named2) +
  scale_fill_manual(values = trophic_cols.named2) +
  scale_y_continuous(limits=c(0.1, 0.5)) +
  th + theme(legend.position = 'none') +
  labs(x = '', y = 'Proportion of nutrient production, %')

dev.off()

## benthic scenario
ben<-with(fish_avg, data.frame(hard_coral = 40,
                turf_algae = mean(turf_algae, na.rm=TRUE),
                macroalgae = 5,
                bare_substrate = 20,
                other_benthos = 100 - 40 - 12 - 5 - 20,
                type = 'Managed, intact'))


ben2<-with(fish_avg, data.frame(hard_coral = 5,
                               turf_algae = 40,
                               macroalgae = 15,
                               bare_substrate = 35,
                               other_benthos = 100-40-35-15 - 5,
                               type = 'Managed, degraded'))

bens<-rbind(ben,ben2) %>% pivot_longer(-type, names_to = 'benthic', values_to = 'cover') %>% 
  mutate(benthic=factor(benthic, levels=unique(benthic))) %>% 
  group_by(type) %>% 
  arrange(desc(benthic), .by_group=TRUE) %>%
  mutate(lab.ypos = cumsum(cover) - 0.5*cover)

pdf(file = 'fig/temp/ben_donuts.pdf', height=5, width=6)
ggplot(bens, aes(x=2, y = cover, fill = benthic)) + geom_bar(stat='identity') + 
  coord_polar(theta='y', start=0) + facet_wrap(~type) + 
  theme_void() +
  geom_text(aes(y = lab.ypos, label = paste0(round(cover,0), "%")), color = "black", fontface='bold', size=3.5)+
  scale_fill_manual(values = ben_cols.named) +
  theme(legend.position = 'none') +
  xlim(0.5, 2.5)

ggplot(bens, aes(x=2, y = cover, fill = benthic)) + geom_bar(stat='identity') + 
  coord_polar(theta='y', start=0) + facet_wrap(~type) + 
  theme_void() +
  # geom_text(aes(y = lab.ypos, label = paste0(round(cover,0), "%")), color = "black", fontface='bold', size=3.5)+
  scale_fill_manual(values = ben_cols.named) +
  theme(legend.position = 'none') +
  xlim(0.5, 2.5)
dev.off()

meds<-mm %>% 
  group_by(type, fg) %>% 
  summarise(med = median(.epred))

ggplot(meds, aes(type, med, fill=fg)) + geom_bar(stat='identity')

fish2<-fishp %>% group_by(fish_taxon, trophic_group) %>% 
  distinct(calcium.mg, iron.mg, selenium.mug, zinc.mg, omega3.g, vitamin_a.mug) %>% 
  pivot_longer(-c(fish_taxon, trophic_group), names_to = 'nutrient', values_to = 'conc') %>% 
  group_by(trophic_group, nutrient) %>% 
  summarise(conc =mean(conc)) %>% 
  group_by(trophic_group, nutrient) %>% 
  summarise(conc =mean(conc)) %>% 
  mutate(fg = trophic_group)

md<-fish2 %>% left_join(meds %>% filter(type=='Managed, degraded'),  by ='fg') %>% 
  filter(!is.na(med)) %>% 
  group_by(nutrient) %>% 
  summarise(conc = weighted.mean(conc, w = med))

od<-fish2 %>% left_join(meds %>% filter(type=='Managed, intact'),  by ='fg') %>% 
  filter(!is.na(med)) %>% 
  group_by(nutrient) %>% 
  summarise(conc = weighted.mean(conc, w = med))

source('scripts/rda_reader.R')
nuts<-rbind(md, od) %>% left_join(rda %>% mutate(nutrient=nutrient2), by='nutrient') %>% 
        mutate(rda = conc  / rda_kids * 100)

