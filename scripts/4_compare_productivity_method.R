

prod_mte<-read.csv("data/wcs/madagascar_potential-prod-test_mte.csv")
prod_morais<-read.csv("data/wcs/madagascar_potential-prod-test_rfishprod.csv")

plot(prod_mte$prod_g_day, prod_morais$prod_day, 
     xlab = 'MTE productivity (g per day)', ylab = 'Renato productivity (g per day)')

plot(prod_mte$K, prod_morais$Kmax, 
     xlab = 'MTE K', ylab = 'Renato Kmax')
abline(0,1)

prod_morais %>% filter(prod_day>2)