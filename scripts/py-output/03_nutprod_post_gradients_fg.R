library(tidyverse)
source('scripts/0_plot_theme.R')
theme_set(funk::theme_sleek())


## Future scenarios holding regimes conditions
figname='fig/model/future_productivity_regimes_fg.pdf'
filename='/fg/prod/'
var_name = 'prod'
source('py-notebook/func_posterior_read_fg.R')
