library(tidyverse)
source('scripts/0_plot_theme.R')
theme_set(funk::theme_sleek())

## Proportion predictions for alternate habitat regimes

## Productivity
figname='fig/model/future_productivity_regimes_fg.pdf'
csvname='results/future_productivity_regimes_fg.csv'
filename='/fg/prod/'
var_name = 'prod'
source('py-notebook/func_posterior_read_AltRegimes_fg.R')
prod_main_alt<-main

# ## Biomass
figname='fig/model/future_biomass_regimes_fg.pdf'
csvname='results/future_biomass_regimes_fg.csv'
filename='/fg/biom/'
var_name = 'biom'
source('py-notebook/func_posterior_read_AltRegimes_fg.R')
biom_main_alt<-main

## Calcium
figname='fig/model/future_calcium_regimes_fg.pdf'
csvname='results/future_calcium_regimes_fg.csv'
filename='/fg/calcium/'
var_name = 'calcium'
source('py-notebook/func_posterior_read_AltRegimes_fg.R')
calcium_main_alt<-main

## Iron
figname='fig/model/future_iron_regimes_fg.pdf'
csvname='results/future_iron_regimes_fg.csv'
filename='/fg/iron/'
var_name = 'iron'
source('py-notebook/func_posterior_read_AltRegimes_fg.R')
iron_main_alt<-main

## Zinc
figname='fig/model/future_zinc_regimes_fg.pdf'
csvname='results/future_zinc_regimes_fg.csv'
filename='/fg/zinc/'
var_name = 'zinc'
source('py-notebook/func_posterior_read_AltRegimes_fg.R')
zinc_main_alt<-main

## VitA
figname='fig/model/future_vita_regimes_fg.pdf'
csvname='results/future_vita_regimes_fg.csv'
filename='/fg/vitaminA/'
var_name = 'vitaminA'
source('py-notebook/func_posterior_read_AltRegimes_fg.R')
vita_main_alt<-main

## Selenium
figname='fig/model/future_selenium_regimes_fg.pdf'
csvname='results/future_selenium_regimes_fg.csv'
filename='/fg/selenium/'
var_name = 'selenium'
source('py-notebook/func_posterior_read_AltRegimes_fg.R')
selenium_main_alt<-main

## Omega-3
figname='fig/model/future_omega_regimes_fg.pdf'
csvname='results/future_omega_regimes_fg.csv'
filename='/fg/omega/'
var_name = 'omega'
source('py-notebook/func_posterior_read_AltRegimes_fg.R')
omega_main_alt<-main

## average of 3 nutrients
nuts<-rbind(calcium_main_alt %>% mutate(nutrient = 'Ca'),
			iron_main_alt %>% mutate(nutrient = 'Fe'),
			zinc_main_alt %>% mutate(nutrient = 'Zn'),
			omega_main_alt %>% mutate(nutrient = 'O-3'),
			vita_main_alt %>% mutate(nutrient = 'VitA'),
			selenium_main_alt %>% mutate(nutrient = 'Se'))  %>% 
		mutate(id = paste(nutrient, fg))

nuts_agg<-nuts %>% group_by(X, X_raw, fg, cov) %>% 
		summarise(prop = mean(prop)) 



## average of fishery services
servs<-rbind(nuts_agg %>% mutate(service = 'Nutrient\nproduction'),
			biom_main_alt %>% mutate(service = 'Standing\nbiomass'),
			prod_main_alt %>% mutate(service = 'Biomass\nturnover')) %>% 
		mutate(id = paste(service, fg))

servs_agg<-servs %>% group_by(X, X_raw, fg, cov) %>% 
		summarise(prop = mean(prop))
