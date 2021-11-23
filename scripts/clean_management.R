pacman::p_load(tidyverse)
m<-read_excel('data/wcs/mermaid_management_rules.xlsx')

m<-m %>% mutate(site = strsplit(as.character(ecol_sites), ",")) %>% 
  unnest(site) %>% 
  mutate(site = trimws(site)) %>% 
  select(-ecol_sites)

write.csv(m, file = 'data/wcs/mermaid_management_clean.csv')