
library(tidyverse)
load(file = 'results/wcs_productivity.rds')
load(file = 'results/wcs_nut_prod.rds')


fishp %>% summarise(n=n_distinct(site))
fishp %>% group_by(country, year) %>% summarise(n=n_distinct(site))
fishp %>% group_by(country) %>% summarise(n=n_distinct(site))
fishp %>% group_by(country, site) %>% summarise(n=n_distinct(year)) %>% 
          group_by(country,n) %>% count()