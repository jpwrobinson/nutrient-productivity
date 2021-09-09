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

## 1. correct growth rate for temperature (for observed species)
growth_cor <- function(K, Tobs, Tpred){
  Kc = K * exp(-(Ea / Kb) * ((1/Tobs) - (1/Tpred)))
  return(Kc)
}

db$K_cor<-growth_cor(K=db$K, 
                     Tobs=ctoK(db$sstmean), 
                     Tpred=temp)
plot(db$K_cor, db$K)

## 2. mixed effects model for missing species / genus
dat.list<-as.list(db)
dat.list$Kb<-Kb
dat.list$temp<-temp
dat.list$LogMaxSizeTL<-log(db$MaxSizeTL)
dat.list$logK<-log(db$K)
dat.list$Family<-as.factor(dat.list$Family)
dat.list$sst_kelvin<-ctoK(dat.list$sstmean)
dat.list$MaxMass<-dat.list$a * dat.list$MaxSizeTL^dat.list$b
dat.list$LogMaxMass<-log(dat.list$MaxMass)

## Nicolas did not find strong Ea effect (B0)
## We want to estimate B1 and out sample family-level allometric relationship for K ~ M
mod<-ulam(
  alist(
  logK ~ dnorm(mu, sigma),
  mu <- log(K0) - # normalization constant
      (B_E/(8.62e-5*sst_kelvin)) - # boltzmann and temperature scaling
      B1[Family]*LogMaxMass, # allometric scaling with max size, family-level effect
  
  c(K0) ~ dnorm(0, 1000),
  B1[Family] ~ dnorm(B1_mean, sigmar),
  B1_mean ~ dnorm(0.25, 1),
  B_E ~ dnorm(0.65, 1),
  c(sigma) ~ dcauchy(0,2),
  sigmar ~ dexp(1)
  ),
  data = dat.list,  chains=3 ,iter = 2000, warmup=500, cores=4)

dashboard(mod)
precis(mod,2)

## B1_mean was -0.23 (Nicolas and Sibly)
precis(mod,2)[rownames(precis(mod,2)) =='B1_mean',]

## Ea was -0.37 (Nicolas) and -0.31 (Sibly)
precis(mod,2)[rownames(precis(mod,2)) =='B_E',]

## check K predictions
preds<-sim(mod)
db$K_pred<-exp(apply(preds, 2, median))
with(db, plot(K, K_pred))
abline(0, 1)

## 3. out-sample K based on temperature (28C) and family 
#   assign K to each Madagascar observation
mada.prod$MaxMass<-with(mada.prod, a * MaxSizeTL^b)
mada.prod$LogMaxMass<-log(mada.prod$MaxMass)
mada.prod$sst_kelvin<-ctoK(mada.prod$sstmean)
mada.prod$Family<-factor(mada.prod$fish_family)

post<-extract.samples(mod)
meds<-link(mod, data=mada.prod, n=1000, post=post)

mada.prod$K<-exp(apply(meds, 2, median))
mada.prod$lower95 <- exp(apply( meds , 2 , HPDI , prob=0.95 )[1,])
mada.prod$upper95 <- exp(apply( meds , 2 , HPDI , prob=0.95 )[2,])


## 4. productivity equation
lplus<-linf*(1 - exp(-k*(temp-t0)))



head(mada.prod)


exp(-(Ea / Kb) * ((1/a) - (1/T)))