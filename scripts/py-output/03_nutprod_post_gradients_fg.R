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

## Figure 3
bens<-c('hard_coral','macroalgae','bare_substrate','turf_algae','rubble')

pdf(file = 'fig/Figure3.pdf', height=9, width = 7.5)

g0<-ggplot(biom_main %>% filter(cov %in% bens), 
		aes(X_raw, prop, fill=fg)) + 
      # geom_ribbon(alpha=0.2) +
      geom_line(size=0.8, aes(col=fg)) + 
      labs(x = '% cover', y = 'proportion community, %', subtitle = 'Standing biomass') +
      scale_colour_manual(values = trophic_cols.named2) +
      facet_wrap(~cov, nrow=1, scales='free_x') +
      scale_y_continuous(expand=c(0,0), limits=c(0,.62), breaks =seq(0, .5, by = .1), labels=seq(0, 50, by = 10)) +
      theme(legend.position = 'none')

g1<-ggplot(prod_main %>% filter(cov %in% bens), 
		aes(X_raw, prop, fill=fg)) + 
      # geom_ribbon(alpha=0.2) +
      geom_line(size=0.8, aes(col=fg)) + 
      labs(x = '% cover', y = 'proportion community, %', subtitle = 'Biomass production') +
      scale_colour_manual(values = trophic_cols.named2) +
      facet_wrap(~cov, nrow=1, scales='free_x') +
      scale_y_continuous(expand=c(0,0), limits=c(0,.62), breaks =seq(0, .5, by = .1), labels=seq(0, 50, by = 10)) +
      theme(legend.position = 'none')

g2<-ggplot(nuts %>% filter(cov %in% bens), 
		aes(X_raw, prop, fill=fg)) + 
      # geom_ribbon(alpha=0.2) +
      geom_line(size=0.8, aes(col=fg)) + 
      labs(x = '% cover', y = 'proportion community, %', subtitle = 'Nutrient production (Ca, Fe, Zn)') +
      scale_colour_manual(values = trophic_cols.named2) +
      facet_wrap(~cov, nrow=1, scales='free_x') +
      scale_y_continuous(expand=c(0,0), limits=c(0,.62), breaks =seq(0, .5, by = .1), labels=seq(0, 50, by = 10)) +
      theme(legend.position = 'none') 


print(
	plot_grid(g0, g1, g2, nrow = 3 , labels = c('A', 'B', 'C'))
	)

dev.off()

## Sup fig - anthro drivers

pdf(file = 'fig/FigureSX_anthro_drivers.pdf', height=9, width = 7.5)

g0S<-ggplot(biom_main %>% filter(!cov %in% bens), 
		aes(X_raw, prop, fill=fg)) + 
      # geom_ribbon(alpha=0.2) +
      geom_line(size=0.8, aes(col=fg)) + 
      labs(x = '', y = 'proportion community, %', subtitle = 'Standing biomass') +
      scale_colour_manual(values = trophic_cols.named2) +
      facet_wrap(~cov, nrow=1, scales='free', strip.position = 'bottom') +
      scale_y_continuous(expand=c(0,0), limits=c(0,.62), breaks =seq(0, .5, by = .1), labels=seq(0, 50, by = 10)) +
      theme(legend.position = 'none', strip.placement='outside')

g1S<-ggplot(prod_main %>% filter(!cov %in% bens), 
		aes(X_raw, prop, fill=fg)) + 
      # geom_ribbon(alpha=0.2) +
      geom_line(size=0.8, aes(col=fg)) + 
      labs(x = '', y = 'proportion community, %', subtitle = 'Biomass production') +
      scale_colour_manual(values = trophic_cols.named2) +
      facet_wrap(~cov, nrow=1, scales='free', strip.position = 'bottom') +
      scale_y_continuous(expand=c(0,0), limits=c(0,.62), breaks =seq(0, .5, by = .1), labels=seq(0, 50, by = 10)) +
      theme(legend.position = 'none', strip.placement='outside')

g2S<-ggplot(nuts %>% filter(!cov %in% bens), 
		aes(X_raw, prop, fill=fg)) + 
      # geom_ribbon(alpha=0.2) +
      geom_line(size=0.8, aes(col=fg)) + 
      labs(x = '', y = 'proportion community, %', subtitle = 'Nutrient production (Ca, Fe, Zn)') +
      scale_colour_manual(values = trophic_cols.named2) +
      facet_wrap(~cov, nrow=1, scales='free', strip.position = 'bottom') +
      scale_y_continuous(expand=c(0,0), limits=c(0,.62), breaks =seq(0, .5, by = .1), labels=seq(0, 50, by = 10)) +
      theme(legend.position = 'none', strip.placement='outside') 

print(
	plot_grid(g0S, g1S, g2S, nrow = 3, labels = c('A', 'B', 'C'))
	)

dev.off()