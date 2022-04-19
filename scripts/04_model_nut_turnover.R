library(tidyverse)

dat<-read.csv('py-notebook/calcium.mg_reef_unscaled.csv')
dat$type<-with(dat, paste(country, management_rules, sep = '_'))


ggplot(dat, aes(log(biomass_kgha), nut_prod_day_ha, col=management_rules))  + 
  geom_point() + 
  facet_wrap(~country, scales='free')
