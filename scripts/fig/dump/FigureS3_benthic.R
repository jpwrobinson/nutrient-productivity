pacman::p_load(tidyverse, skimr, cowplot, here, funk,disco, patchwork, GGally, install=FALSE)
source('scripts/0_plot_theme.R')

# load data
load(file = 'results/wcs_productivity.rds')
load(file = 'results/wcs_nut_prod.rds')

# read benthic
load('data/wcs/wcs_fish_benthic.rds')
focal<-left_join(prod_fg, 
                 fish_avg %>% ungroup() %>%  
                   select(site, reef_type, reef_zone, management_rules, hard_coral, macroalgae, turf_algae, bare_substrate, 
                          depth, fish_richness),
                 by='site') %>% 
  filter(!is.na(depth)) %>%  # dropping 2 sites (NK02 in Madasgascar and WaiE1 in Fiji)
  # left_join(threat, by = 'site') %>% ungroup() ## lots of sites missing, incl. all of Belize
  ## convert data to list, characters to factors for rethinking
  ungroup() %>% distinct(site, hard_coral, macroalgae, turf_algae, bare_substrate)

ggpairs(focal %>% select(-site))


  


