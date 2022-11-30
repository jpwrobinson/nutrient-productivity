pacman::p_load(tidyverse, funk, install=FALSE)
theme_set(theme_bw())

## load datasets
load(file = 'results/wcs_productivity.rds')
load(file = 'results/wcs_nut_prod.rds')
load('data/wcs/wcs_fish_benthic.rds')

prod_reef<-prod_reef %>% 
  mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                               'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3\nfatty acids')) 

## recode and estimate nutrient proportion per site per fg
prod_fg<-prod_fg %>%
  # filter(country != 'Belize') %>% 
  # group_by(site, year) %>%
  group_by(management) %>%
  # filter(!any(prod_g_day_ha==0)) %>%
  mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                               'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3\nfatty acids')) %>% 
  # group_by(country, site, year, nutrient,nutrient_lab) %>% 
  group_by(country, management,nutrient,nutrient_lab) %>%
  mutate(tnut = sum(nut_prod_day_ha), 
         nutprop = nut_prod_day_ha / tnut,
         tprod = sum(biomass_turnover),
         prodprop = biomass_turnover / tprod,
         tbiom = sum(biomass_kgha),
         biomprop = biomass_kgha / tbiom) 

# length(unique(prod_reef$site))
tt<-prod_fg %>% filter(prodprop==0 & nutrient=='calcium.mg')
with(tt, table(country,fg))
# with(prod_fg, table(site, country))
# prod_fg %>% filter(country=='Belize') %>% ungroup() %>% distinct(site)

## looping by nutrient
nut.vec<-unique(prod_fg$nutrient)

# model data - extract for fg
for(i in 1:length(nut.vec)){
  nut<-nut.vec[i]
  print(paste('Data mangle + scale for', nut))
  source('py-notebook/data_extract.R')
}

## biomass or productivity only
nut = 'productivity'
source('py-notebook/data_extract.R')

nut = 'biomass'
source('py-notebook/data_extract.R')

# model data - extract for reef
for(i in 1:length(nut.vec)){
  nut<-nut.vec[i]
  print(paste('Data mangle + scale for', nut))
  source('py-notebook/data_extract_reef.R')
}


## checking nut proportion df
focal<-read.csv('py-notebook/zinc.mg_scaled.csv') %>%
  pivot_longer(herbivore.detritivore:planktivore, names_to = 'fg', values_to = 'nutprop')
# ggplot(focal, aes(nutprop)) + geom_histogram() + facet_wrap(~fg)
# ggplot(focal, aes(depth, nutprop, col=fg)) + geom_point() + geom_smooth()
# ggplot(focal, aes(grav_nc, nutprop, col=fg)) + geom_point() + geom_smooth()
ggplot(focal, aes(rubble, nutprop, col=fg)) + geom_point() + geom_smooth()
# ggplot(focal, aes(fg, nutprop, fill=fg)) + geom_boxplot() + facet_wrap(~country)
# ggplot(focal, aes(management_rules, nutprop, fill=fg)) + geom_boxplot() + facet_wrap(~country)
# focal %>% group_by(fg) %>% summarise(median(nutprop), min(nutprop), max(nutprop))

## checking reef-scale nutrient df
focal<-read.csv('py-notebook/iron.mg_reef_unscaled.csv')
ggplot(focal, aes(country, biomass_kgha, fill=management_rules)) + geom_boxplot()
ggplot(focal, aes(log(biomass_kgha), nut_prod_day_ha, col=management_rules)) + 
  geom_point() + geom_smooth()
ggplot(focal, aes(log(biomass_kgha), nut_turnover, col=management_rules)) + 
  geom_point() + geom_smooth() + facet_wrap(~country)
ggplot(focal, aes(hard_coral, nut_prod_day_ha, col=country)) + geom_point()
ggplot(focal, aes(country, nut_turnover, fill=management_rules)) + geom_boxplot()

# covariate correlations
# test<-read.csv('py-notebook/zinc.mg_reef_unscaled.csv')
# pairs2(test[,c(11:15,18, 20:23)])
# pairs2(test[,c(1:6)])
# hist(log(test$nut_turnover))

t2 %>% pivot_longer(herbivore.detritivore:planktivore, names_to = 'fg', values_to = 'prop') %>%
  ggplot(aes(hard_coral, prop, col=fg)) + geom_point() + geom_smooth() + facet_wrap(~fg)

# fg correlations
t1<-read.csv('py-notebook/productivity_unscaled.csv')
t2<-read.csv('py-notebook/biomass_unscaled.csv')
t3<-read.csv('py-notebook/zinc.mg_unscaled.csv')
t4<-read.csv('py-notebook/calcium.mg_unscaled.csv')
t5<-read.csv('py-notebook/iron.mg_unscaled.csv')
t6<-read.csv('py-notebook/vitamin_a.mug_unscaled.csv')
t7<-read.csv('py-notebook/selenium.mug_unscaled.csv')
t8<-read.csv('py-notebook/omega3.g_unscaled.csv')
hnames<-c("herbivore.detritivore","herbivore.macroalgae","invertivore.mobile","piscivore","planktivore", "omnivore")
# hnames<-c("herbivore","invertivore.mobile","piscivore","planktivore", "omnivore")

pdf(file = 'fig/model/fg_proportion_correlations.pdf', height=7, width=12)
for(i in 1:length(hnames)){
  # paste('Correlation:', hnames[i])
  print(
    pairs2(cbind(t1 %>% pull(hnames[i]),
            t2 %>% pull(hnames[i]),
            t3 %>% pull(hnames[i]),
            t4 %>% pull(hnames[i]),
            t5 %>% pull(hnames[i]),
            t6 %>% pull(hnames[i]),
            t7 %>% pull(hnames[i]),
            t8 %>% pull(hnames[i])
            ),
         labels = c('productivity', 'biomass', 'zinc', 'calcium', 'iron', 'vitA', 'selenium', 'O-3')))
  title(hnames[i])
  }
dev.off()

t3<-read.csv('py-notebook/zinc.mg_reef_unscaled.csv')
t4<-read.csv('py-notebook/calcium.mg_reef_unscaled.csv')
t5<-read.csv('py-notebook/iron.mg_reef_unscaled.csv')
t6<-read.csv('py-notebook/vitamin_a.mug_reef_unscaled.csv')
t7<-read.csv('py-notebook/selenium.mug_reef_unscaled.csv')
t8<-read.csv('py-notebook/omega3.g_reef_unscaled.csv')

pdf(file = 'fig/model/reef_prod_correlations.pdf', height=7, width=12)
  print(
    pairs2(cbind(t3 %>% pull(biomass_turnover),
                 t3 %>% pull(biomass_kgha),
                 t3 %>% pull(nut_prod_day_ha),
                 t4 %>% pull(nut_prod_day_ha),
                 t5 %>% pull(nut_prod_day_ha),
                 t6 %>% pull(nut_prod_day_ha),
                 t7 %>% pull(nut_prod_day_ha),
                 t8 %>% pull(nut_prod_day_ha)
    ),
    labels = c('productivity', 'biomass', 'zinc', 'calcium', 'iron', 'vitA', 'selenium', 'O-3')))

dev.off()

