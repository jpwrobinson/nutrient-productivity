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

### Future scenarios
figname='fig/model/future_biomass.pdf'
filename='py-notebook/biomass/'
var_name = 'biomass, kg ha'
source('py-notebook/func_posterior_futures.R')

figname='fig/model/future_productivity.pdf'
filename='/prod'
var_name = 'productivity, g day ha'
source('py-notebook/func_posterior_futures.R')


## Future scenarios holding background conditions
figname='fig/model/future_biomass_background.pdf'
filename='/biomass'
var_name = 'biomass, kg ha'
source('py-notebook/func_posterior_read.R')

figname='fig/model/future_productivity_background.pdf'
filename='/prod'
var_name = 'productivity, g day ha'
source('py-notebook/func_posterior_read.R')




## testing brms alt
# library(brms)
# focal$ylog<-log(focal$biomass_kgha)
# f1<-brm(ylog ~ grav_nc + hard_coral + macroalgae + bare_substrate +
#           turf_algae + rubble + pop_count + depth + management_rules, data=focal)
# summary(f1)