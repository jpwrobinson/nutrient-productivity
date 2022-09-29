library(tidyverse)
source('scripts/0_plot_theme.R')
theme_set(funk::theme_sleek())


## Proportion predictions for continuous covariates

## Productivity
figname='fig/model/future_productivity_regimes_fg.pdf'
csvname='results/future_productivity_regimes_fg.csv'
filename='/fg/prod/'
var_name = 'prod'
source('py-notebook/func_posterior_read_fg.R')
prod_main<-main

## Biomass
figname='fig/model/future_biomass_regimes_fg.pdf'
csvname='results/future_biomass_regimes_fg.csv'
filename='/fg/biom/'
var_name = 'biom'
source('py-notebook/func_posterior_read_fg.R')
biom_main<-main

## Calcium
figname='fig/model/future_calcium_regimes_fg.pdf'
csvname='results/future_calcium_regimes_fg.csv'
filename='/fg/calcium/'
var_name = 'calcium'
source('py-notebook/func_posterior_read_fg.R')
calcium_main<-main

## Iron
figname='fig/model/future_iron_regimes_fg.pdf'
csvname='results/future_iron_regimes_fg.csv'
filename='/fg/iron/'
var_name = 'iron'
source('py-notebook/func_posterior_read_fg.R')
iron_main<-main

## Zinc
figname='fig/model/future_zinc_regimes_fg.pdf'
csvname='results/future_zinc_regimes_fg.csv'
filename='/fg/zinc/'
var_name = 'zinc'
source('py-notebook/func_posterior_read_fg.R')
zinc_main<-main

## average of 3 nutrients
nuts<-rbind(calcium_main %>% mutate(nutrient = 'Ca'),
			iron_main %>% mutate(nutrient = 'Fe'),
			zinc_main %>% mutate(nutrient = 'Zn')) %>% 
		group_by(X, X_raw, fg, cov) %>% 
		summarise(prop = mean(prop))

