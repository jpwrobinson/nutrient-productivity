

prod_mte<-read.csv("data/wcs/madagascar_potential-prod-test_mte.csv")
prod_morais<-read.csv("data/wcs/madagascar_potential-prod-test_rfishprod.csv")

par(mfrow=c(1,2))
plot(prod_mte$prod_g_day, prod_morais$prod_day, 
     xlab = 'MTE productivity (g per day)', ylab = 'Renato productivity (g per day)')
abline(0, 1)
title('Daily productivity (grams)')

plot(prod_mte$K, prod_morais$Kmax, 
     xlab = 'MTE K', ylab = 'Renato Kmax')
abline(0,1)
title('K from MTE vs K from Morais')

