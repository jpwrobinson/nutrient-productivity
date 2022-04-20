library(tidyverse)

posts<-read.csv('py-notebook/prod_posterior_summary.csv') %>% 
  mutate(var = str_split_fixed(X, '\\[', 2)[,1]) %>% 
  filter(!var %in% c('intercept', 'alpha', 'β0_cnc', 'β0_managenc')) %>% 
  mutate(varname = str_split_fixed(varname, '\\[', 2)[,1],
         varnum=as.numeric(factor(var)),
         fill = ifelse(hdi_2.5. > 0 & hdi_97.5. > 0, 'darkgreen', 'grey'),
         fill = ifelse(hdi_2.5. < 0 & hdi_97.5. < 0, 'darkred', fill))

conts<-c('hard_coral', 'bare_sub', 'gravity', 'turf', 'population', 'macroalgae', 'depth', 'sediment', 'nut_load')

ggplot(posts, aes(fg, varname, fill=fill)) + geom_tile() + scale_fill_identity()


