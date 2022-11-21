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
  mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                               'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3\nfatty acids')) %>% 
  ## drop invert sessile as these are small proportion, consistently, and not fished
  filter(!fg %in% c('invertivore-sessile', 'detritivore')) %>% 
  group_by(country, site, year, nutrient,nutrient_lab) %>% 
  mutate(tnut = sum(nut_prod_day_ha), 
         nutprop = nut_prod_day_ha / tnut,
         tprod = sum(biomass_turnover),
         prodprop = biomass_turnover / tprod,
         tbiom = sum(biomass_kgha),
         biomprop = biomass_kgha / tbiom) 

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
# focal<-read.csv('py-notebook/zinc.mg_scaled.csv') %>%
#   pivot_longer(herbivore.detritivore:planktivore, names_to = 'fg', values_to = 'nutprop')
# ggplot(focal, aes(nutprop)) + geom_histogram() + facet_wrap(~fg)
# ggplot(focal, aes(depth, nutprop, col=fg)) + geom_point() + geom_smooth()
# ggplot(focal, aes(grav_nc, nutprop, col=fg)) + geom_point() + geom_smooth()
# ggplot(focal, aes(fg, nutprop, fill=fg)) + geom_boxplot() + facet_wrap(~country)
# ggplot(focal, aes(management_rules, nutprop, fill=fg)) + geom_boxplot() + facet_wrap(~country)
# focal %>% group_by(fg) %>% summarise(median(nutprop), min(nutprop), max(nutprop))

## checking reef-scale nutrient df
# focal<-read.csv('py-notebook/zinc.mg_reef_unscaled.csv') 
# ggplot(focal, aes(country, biomass_kgha, fill=management_rules)) + geom_boxplot()
# ggplot(focal, aes(hard_coral, nscore3)) + geom_point()
# ggplot(focal, aes(nscore3, nscore)) + geom_point()
# ggplot(focal, aes(management_rules, nscore3, fill=country)) + geom_boxplot()

# covariate correlations
# test<-read.csv('py-notebook/zinc.mg_reef_unscaled.csv')
# pairs2(test[,c(11:15,18, 20:23)])
# pairs2(test[,c(1:6)])
# hist(log(test$nut_turnover))


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

pdf(file = 'fig/model/fg_proportion_correlations.pdf', height=7, width=12)
for(i in 1:6){
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

