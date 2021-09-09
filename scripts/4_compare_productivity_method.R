

prod_mte<-read.csv("data/wcs/madagascar_potential-prod-test_mte.csv")
prod_morais<-read.csv("data/wcs/madagascar_potential-prod-test_rfishprod.csv")

plot(prod_mte$prod_day, prod_morais$prod_day)