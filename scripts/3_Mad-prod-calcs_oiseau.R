source("scripts/1_Mad-Sey-prod-prep.R")
library(rfishprod)
library(tidyverse)
library(rethinking)
ctoK<-function(x){x + 273.2}

## load Morais reef fish growth data
data(db) 

## Questions:
# 1) MTE is in Kelvin, as this relates to some constants
# 2) Mixed effect model structure - intercepts, slopes?
# 3) K conversion equation should have a negative in the exponential?

## set fixed parameters
temp = ctoK(28) # temperature, celsius
Kb = 8.62 * 10^-5 # Boltzmann's constant
beta = 0.25
Ea = 0.65

## mixed effects model for missing species / genus
dat.list<-as.list(db)
dat.list$Kb<-Kb
dat.list$temp<-temp
dat.list$LogMaxSizeTL<-log(db$MaxSizeTL)
dat.list$logK<-log(db$K)
dat.list$Family<-as.factor(dat.list$Family)

mod<-ulam(
  alist(
  logK ~ dnorm(mu, sigma),
  mu <- B1*LogMaxSizeTL + B2*1/Kb*sstmean, #+ B_fam[Family],
  
  c(B1, B2) ~ dnorm(0, 100),
  # B_fam[family] ~ dnorm(0, sigmar),
  c(sigma, sigmar) ~ dexp(1)
  ),
  data = dat.list,  chains=3 ,iter = 2000, warmup=500, cores=4)

## correct growth rate for temperature
growth_cor <- function(K, Tobs, Tpred){
  Kc = K * exp(-(Ea / Kb) * ((1/Tobs) - (1/Tpred)))
  return(Kc)
}
  
db$K_cor<-growth_cor(K=db$K, 
                     Tobs=ctoK(db$sstmean), 
                     Tpred=temp)
plot(db$K_cor, db$K)

## productivity equation
lplus<-linf*(1 - exp(-k*(T-t0)))



head(mada.prod)


exp(-(Ea / Kb) * ((1/a) - (1/T)))