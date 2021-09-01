source("scripts/1_Mad-Sey-prod-prep.R")
library(rfishprod)
library(tidyverse)
data(db) ## load Morais reef fish growth data


## set fixed parameters
T = 28 # temperature, celsius
beta = 0.25
Ea = 0.65

## mixed effects model for missing species / genus
mod<-lme4::lmer(
  log(K) ~ (1 | Species) + (1 | Family),
  data = db
)

## correct growth rate for temperature
growth_cor <- function(K, Tobs, Tpred=28){
  K = Kc - exp((Ea / kb) * (1/Tobs - 1/Tpred))
}
  


## productivity equation
lplus<-linf*(1 - exp(-k*(T-t0)))



head(mada.prod)