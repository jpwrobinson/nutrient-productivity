library(tidyverse)
theme_set(funk::theme_sleek())


th<-theme(panel.spacing=unit(.05, "lines"),
          strip.text.y = element_blank(),
      panel.border = element_blank(),
      legend.position = 'none',
      axis.line = element_line(colour='black'))

## Turnover - zinc
p1<-read.csv('py-notebook/turnover/zinc_posterior_summary.csv') 
p2<-read.csv('py-notebook/turnover/zinc_posterior_summary_50.csv') 
filename = 'posterior_params_turnover'
source('py-notebook/func_posterior_plot.R')

## Productivity
p1<-read.csv('py-notebook/prod/posterior_summary.csv') 
p2<-read.csv('py-notebook/prod/posterior_summary_50.csv') 
filename = 'posterior_params_prod'
source('py-notebook/func_posterior_plot.R')

## Biomass
p1<-read.csv('py-notebook/biomass/posterior_summary.csv') 
p2<-read.csv('py-notebook/biomass/posterior_summary_50.csv') 
filename = 'posterior_params_biomass'
source('py-notebook/func_posterior_plot.R')

## Density
p1<-read.csv('py-notebook/density/posterior_summary.csv') 
p2<-read.csv('py-notebook/density/posterior_summary_50.csv') 
filename = 'posterior_params_density'
source('py-notebook/func_posterior_plot.R')