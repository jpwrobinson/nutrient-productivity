library(tidyverse)
library(rfishbase)
library(janitor)

kdb<-read.csv(file = 'data/species_lmax_diet.csv')
# trait<-read.csv(file = 'data/trait/all_traits_active.csv') %>% 
#       mutate(species=str_replace_all(species, '_', '\\ '))
  
pax<-load_taxa() %>% as.data.frame() %>%  clean_names()

kdb<-kdb %>% left_join(pax, by = 'species')


## checking number of positions + diets by family
fams<-kdb %>% group_by(family) %>% summarise(pos=n_distinct(position), 
                                             n_diet = n_distinct(diet), 
                                             n_sp = n_distinct(species)) 

## position
fams %>% filter(pos == 1) %>% pull(family) ## 24 families only have 1 position
fams %>% filter(pos == 2) %>% pull(family) ## 11 families have 2 positions
fams %>% filter(pos > 1) %>% pull(family) ## 32 families have more than 1 position

fams %>% filter(pos == 1) %>% summarise(sum(n_sp)) ## 50 species in families with 1 position
fams %>% filter(pos == 2) %>% summarise(sum(n_sp)) ## 64 species in families with 2 positions
fams %>% filter(pos > 1) %>% summarise(sum(n_sp)) ## 652 species in families with more than 1 position

hist(fams$pos)

## now check diets
fams %>% filter(n_diet == 1) %>% pull(family) ## 23 families only have 1 diet
fams %>% filter(n_diet == 2) %>% pull(family) ## 9 families have 2 diets
fams %>% filter(n_diet > 1) %>% pull(family) ## 33 families have more than 1 diet

fams %>% filter(n_diet == 1) %>% summarise(sum(n_sp)) ## 46 species in families with 1 diet
fams %>% filter(n_diet == 2) %>% summarise(sum(n_sp)) ## 77 species in families with 2 diets
fams %>% filter(n_diet > 1) %>% summarise(sum(n_sp)) ## 656 species in families with more than 1 diet

hist(fams$n_diet)