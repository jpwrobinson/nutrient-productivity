# Productivity calcs - rfishprod version (Morais & Bellwood 2020)

source("scripts/1_Mad-Sey-prod-prep.R")

ls()
head(mada.prod)


# Check "size"
summary(mada.prod$size)
#min = 1cm, max = 100cm, median = 12cm

plot(mada.prod$size)
# Seems to be quite a lot of small fish. Few > 40cm.



# Check if any individuals are larger than species max size:
length(mada.prod$size)
# 5219

summary(mada.prod[mada.prod$size > mada.prod$MaxSizeTL,])
# 57
57/5219   
# 1.0 %

# or equal to max size
summary(mada.prod[mada.prod$size == mada.prod$MaxSizeTL,])
# 147
147/5219
# 2.8 %

#quite a lot. These need to be reduced to equal MaxSizeTL (prod= exactly 0) or 0.1cm below MaxSizeTL (tiny prod values)

mada.prod$size2 <- ifelse(mada.prod$size >= mada.prod$MaxSizeTL, mada.prod$MaxSizeTL-0.1, mada.prod$size)
head(mada.prod)
#Now use size2 for further analyses



###########################################################################



# Use rfishprod package to estimate productivity (i.e. biomass production over time)

#library(devtools)
#devtools::install_github("renatoamorais/rfishprod")

library(rfishprod)

mada.prod <- tidytrait (mada.prod, sey.trait2) 

# from Morais & Bellwood 2018:
fmod <- formula (~ sstmean + MaxSizeTL + Diet + Position + Method) 



# Predicting Kmax, the standardised VBGF parameter (Recommendation: use 100s to 1000s iterations) 
# (takes a while)
?predKmax
datagr <- predKmax (mada.prod, 
                    dataset = db,
                    fmod = fmod,
                    niter = 1000,
                    return = 'pred')

datagr <- datagr$pred



# Positioning fish in their growth trajectory 
# i.e. what's the size they're supposed to have on the next day? 
datagr$L.1day <- with (datagr, applyVBGF (Lmeas = size2,
                                          Lmax = MaxSizeTL,
                                          Kmax = Kmax))

head(datagr)
#each fish has grown a tiny amount (in length).


#Calculate age estimates:
datagr$EstAge <- (1/datagr$Kmax)*log((datagr$MaxSizeTL)/((1-datagr$size2/datagr$MaxSizeTL)*datagr$MaxSizeTL))
summary(datagr$EstAge)
#(if lengths not reduced below MaxSizeTL, will have infinite age values)


#Calculate growth with von Bertalanffy growth function (VBGF), using ages:
#VBGF:  Lt = Lmax*(1-exp(-K*t))
# t = Estimated age + time interval
# Morais & Bellwood (2019), interval = 1day (i.e. 1/365)

# Calculate over a full year: age + (1:365)/365

#Age to add for each day of the year:
age <- (1:365)/365

#Create table for new length: 1 column per day, 1 row per individual fish
VB_lngth <-  matrix(ncol=length(age), nrow=nrow(datagr), dimnames=list(NULL, paste("Day", 1:365, sep="_")))

# for each individual, calculate new length for each day using VBGF formula
for(u in 1:nrow(datagr)) {
  VB_lngth[u, ] <- datagr$MaxSizeTL[u]*(1-exp(-datagr$Kmax[u]*(datagr$EstAge[u] + age)))
}
VB_lngth

#Now have a matrix of lengths for each day of the year per fish
range(VB_lngth)
#  1.009271   101.314415


# !!! NEED TO ADD a AND b COEFFICIENTS FROM SEYCHELLES !!! #

#Convert lengths to weights using a & b coefficents:
VB_wt <-  apply(VB_lngth, 2, function(x) datagr$a*(x^datagr$b))  
head(VB_wt)
nrow(VB_wt)
# units= grams (per fish)







