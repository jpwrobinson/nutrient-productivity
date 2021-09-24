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
# growth_cor <- function(K, Tobs, Tpred){
#   Kc = K * exp(-(Ea / Kb) * ((1/Tobs) - (1/Tpred)))
#   return(Kc)
# }
# 
# db$K_cor<-growth_cor(K=db$K, 
#                      Tobs=ctoK(db$sstmean), 
#                      Tpred=temp)
# plot(db$K_cor, db$K)

## 2. mixed effects model to predict K based on temperature and mass
## this can be used to out-sample K for new species / genus

# data setup as list
dat.list<-as.list(db)
dat.list$Kb<-Kb
dat.list$temp<-temp
dat.list$LogMaxSizeTL<-log(db$MaxSizeTL)
dat.list$logK<-log(db$K)
dat.list$Family<-as.factor(dat.list$Family)
dat.list$Species<-as.factor(dat.list$Species)
dat.list$Diet<-as.factor(dat.list$Diet)
dat.list$sst_kelvin<-ctoK(dat.list$sstmean)
dat.list$MaxMass<-dat.list$a * dat.list$LinfTL^dat.list$b
dat.list$LogMaxMass<-log(dat.list$MaxMass)

## Nicolas did not find strong Ea effect (B0)
## We want to estimate B1 and out sample family-level allometric relationship for K ~ M
mod<-ulam(
  alist(
  logK ~ dnorm(mu, sigma),
  mu <- log(K0) - # normalization constant
      (Ea/(8.62e-5*sst_kelvin)) - # boltzmann and temperature scaling
      B1[Family]*LogMaxMass, # allometric scaling with max size, family-level effect
      # B2[Species]*LogMaxMass , # allometric scaling with max size, family-level effect
      # B2[Diet], # diet effect
  
  # priors, weakly informative or taken from Sibly et al. 2015 (PNAS)
  c(K0) ~ dnorm(0, 1000),
  B1[Family] ~ dnorm(B1_mean, sigmar),
  B1_mean ~ dnorm(0.25, 10),
  # B2[Species] ~ dnorm(0.36, 1),
  Ea ~ dnorm(0.65, 10),
  c(sigma) ~ dcauchy(0,2),
  sigmar ~ dexp(1)
  ),
  data = dat.list,  chains=3 ,iter = 2000, warmup=500, cores=4)

# model diagnostics
dashboard(mod)
precis(mod,2)

## B1_mean was -0.22 (Nicolas and Sibly)
precis(mod,2)[rownames(precis(mod,2)) =='B1_mean',] # 0.28

## Ea was -0.37 (Nicolas) and -0.31 (Sibly)
precis(mod,2)[rownames(precis(mod,2)) =='Ea',] # 0.19

## check K predictions
preds<-sim(mod)
db$K_pred<-exp(apply(preds, 2, median))

par(mfrow=c(1,1))
with(db, plot(log(K), log(K_pred)))
abline(0, 1)

# 3. estimate Kmax

## We want to estimate Kmax based on the relationship in Morais+Bellwood 2018, using K and Lmax
# sl=-2.18
# db$gpi<-log10(db$K_pred) - sl*log10(db$LinfTL)
# hist(db$gpi)
# 
# # convert K to Kmax
# db$Kmax <- 10^(db$gpi + sl*log10(db$MaxSizeTL))
# with(db, plot(K, Kmax))

## 4. out-sample K based on temperature (28C) and family 
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
lplus<-function(linf, K, age, days=1/365){linf*(1 - exp(-K*(age+days)))}
# next_size <- MaxSizeTL[u]*(1-exp(-Kmax[u]*(EstAge[u] + age))) # renato approach

age_est<-function(linf, lcensus, K, l0=0){(1/K)*log((linf - l0)/((1-lcensus)*linf))}
# EstAge <- (1/Kmax)*log((MaxSizeTL)/((1-size2/MaxSizeTL)*MaxSizeTL)) # renato approach

## note that individuals above max size need to be reduced to max size
mada.prod$size2 <- ifelse(mada.prod$size >= mada.prod$MaxSizeTL, mada.prod$MaxSizeTL-0.1, mada.prod$size)
# convert length to mass
mada.prod$mass<-mada.prod$a * mada.prod$size2 ^ mada.prod$b

# estimate age of each fish (eq. 3 in Depczynski et al. 20070)
mada.prod$age<-age_est(linf=mada.prod$MaxSizeTL, lcensus=mada.prod$size2/mada.prod$MaxSizeTL, K = mada.prod$K)

## estimate productivity of each fish
mada.prod$size_nextday<-lplus(linf = mada.prod$MaxSizeTL, K = mada.prod$K, age = mada.prod$age ) 
mada.prod$prod_cm_day <- mada.prod$size_nextday - mada.prod$size2 
mada.prod$prod_g_day<-mada.prod$a * mada.prod$size_nexday ^ mada.prod$b - mada.prod$mass
with(mada.prod, plot(prod_cm_day, prod_g_day))

write.csv(mada.prod, "data/wcs/madagascar_potential-prod-test_mte.csv", row.names=F)
