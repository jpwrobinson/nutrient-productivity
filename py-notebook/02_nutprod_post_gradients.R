## Plot predicted covariate values

## prod
focal<-read.csv('py-notebook/zinc.mg_reef_unscaled.csv')
filename='py-notebook/prod/'
figname='fig/model/posterior_predicted_productivity.pdf'
source('py-notebook/func_posterior_pred_plot.R')

## biomass
focal<-read.csv('py-notebook/zinc.mg_reef_unscaled.csv')
filename='py-notebook/biomass/'
figname='fig/model/posterior_predicted_biomass.pdf'
source('py-notebook/func_posterior_pred_plot.R')

## turnover
focal<-read.csv('py-notebook/zinc.mg_reef_unscaled.csv')
filename='py-notebook/turnover/'
figname='fig/model/posterior_predicted_turnover.pdf'
source('py-notebook/func_posterior_pred_plot.R')