library(tidyverse)
theme_set(funk::theme_sleek())


th<-theme(panel.spacing=unit(.05, "lines"),
          strip.text.y = element_blank(),
      panel.border = element_blank(),
      legend.position = 'none',
      axis.line = element_line(colour='black'))

## READ SERVICE MODELS
# FG = FALSE
# ## Turnover - zinc
# p1<-read.csv('py-notebook/turnover/zinc_posterior_summary.csv') 
# p2<-read.csv('py-notebook/turnover/zinc_posterior_summary_50.csv') 
# filename = 'posterior_params_turnover'
# source('py-notebook/func_posterior_plot.R')

# ## Productivity
# p1<-read.csv('py-notebook/prod/posterior_summary.csv') 
# p2<-read.csv('py-notebook/prod/posterior_summary_50.csv') 
# filename = 'posterior_params_prod'
# source('py-notebook/func_posterior_plot.R')

# ## Biomass
# p1<-read.csv('py-notebook/biomass/posterior_summary.csv') 
# p2<-read.csv('py-notebook/biomass/posterior_summary_50.csv') 
# filename = 'posterior_params_biomass'
# source('py-notebook/func_posterior_plot.R')

# ## Density
# p1<-read.csv('py-notebook/density/posterior_summary.csv') 
# p2<-read.csv('py-notebook/density/posterior_summary_50.csv') 
# filename = 'posterior_params_density'
# source('py-notebook/func_posterior_plot.R')

## READ FG MODELS
FG = TRUE

## FG - calcium
p1<-read.csv('py-notebook/fg/calcium/calcium_posterior_summary.csv')
p2<-read.csv('py-notebook/fg/calcium/calcium_posterior_summary_50.csv') 
filename = 'posterior_params_calcium_fg'
source('py-notebook/func_posterior_plot.R')

## FG - iron
p1<-read.csv('py-notebook/fg/iron/iron_posterior_summary.csv')
p2<-read.csv('py-notebook/fg/iron/iron_posterior_summary_50.csv') 
filename = 'posterior_params_iron_fg'
source('py-notebook/func_posterior_plot.R')

## FG - zinc
p1<-read.csv('py-notebook/fg/zinc/zinc_posterior_summary.csv')
p2<-read.csv('py-notebook/fg/zinc/zinc_posterior_summary_50.csv') 
filename = 'posterior_params_zinc_fg'
source('py-notebook/func_posterior_plot.R')

## FG - prod
p1<-read.csv('py-notebook/fg/prod/prod_posterior_summary.csv')
p2<-read.csv('py-notebook/fg/prod/prod_posterior_summary_50.csv') 
filename = 'posterior_params_prod_fg'
source('py-notebook/func_posterior_plot.R')

## FG - biom
p1<-read.csv('py-notebook/fg/biom/biom_posterior_summary.csv')
p2<-read.csv('py-notebook/fg/biom/biom_posterior_summary_50.csv') 
filename = 'posterior_params_biom_fg'
source('py-notebook/func_posterior_plot.R')


