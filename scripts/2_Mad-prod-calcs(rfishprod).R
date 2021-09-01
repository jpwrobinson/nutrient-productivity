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



# Use rfishprod package to estimate productivity (i.e. biomass production over time)
library(rfishprod)








