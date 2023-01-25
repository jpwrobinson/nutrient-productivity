pacman::p_load(tidyverse, skimr, cowplot, here, funk,disco, patchwork, bayesplot,
               broom, broom.mixed, rethinking, rstan, brms, tidybayes,emmeans, install=FALSE)
source('scripts/0_plot_theme.R')


source('scripts/mod/01_model_run_herb-det.R')
source('scripts/mod/04_model_run_herb-mac.R')
source('scripts/mod/02_model_run_mobinv.R')
source('scripts/mod/03_model_run_pisc.R')