library(tidyverse)
theme_set(funk::theme_sleek())

th<-theme(panel.spacing=unit(.05, "lines"),
          strip.text.y = element_blank(),
          panel.border = element_blank(),
          legend.position = 'none',
          axis.line = element_line(colour='black'))

p1$hdi_25.<-p2$hdi_25.
p1$hdi_75.<-p2$hdi_75.

if(FG == FALSE){
  posts<-p1 %>% 
    mutate(var = str_split_fixed(X, '\\[', 2)[,1]) %>% 
    # drop hierarchical terms than contain the intercept
    filter(!var %in% c( 'alpha', 'β0_c', 'β0_manage')) %>% 
    mutate(varname = str_split_fixed(varname, '\\[', 2)[,1],
           varnum=as.numeric(factor(var)),
           sig = ifelse(hdi_2.5. > 0 & hdi_97.5. > 0, 'black', 'white'),
           sig = ifelse(hdi_2.5. < 0 & hdi_97.5. < 0, 'black', sig),
           sig = ifelse(hdi_25. < 0 & hdi_75. < 0 & hdi_97.5. > 0, 'grey', sig),
           sig = ifelse(hdi_25. > 0 & hdi_75. > 0 & hdi_2.5. < 0, 'grey', sig))
  
  conts<-c('hard_coral', 'bare_sub', 'gravity', 'turf','rubble', 'population', 'macroalgae', 'depth', 'sediment', 'nut_load')
  
  g1<-ggplot(posts %>% filter(!varname %in% conts), aes(fct_reorder(varname,varnum), 
                                                        mean, ymin = hdi_2.5., ymax = hdi_97.5.)) +
    geom_hline(yintercept=0, linetype=5, colour='grey') +
    geom_pointrange(fatten=0)  +
    geom_pointrange(fatten=0, aes(ymin = hdi_25., ymax = hdi_75.), size=1.1)  +
    geom_point(aes(fill=sig), size=2.5, pch=21) +
    coord_flip() +
    scale_fill_identity() +
    labs(x = '', y = 'Posterior mean') +
    th
  
  g2<-ggplot(posts %>% filter(varname%in%conts), 
             aes(varname, mean, ymin = hdi_2.5., ymax = hdi_97.5.)) +
    geom_hline(yintercept=0, linetype=5, colour='grey') +
    geom_pointrange(fatten=0)  +
    geom_pointrange(fatten=0, aes(ymin = hdi_25., ymax = hdi_75.), size=1.1)  +
    geom_point(aes(fill=sig), size=2.5, pch=21) +
    coord_flip() +
    scale_fill_identity() +
    labs(x = '', y = 'Posterior mean') +
    th
}

if(FG == TRUE){
  posts<-p1 %>% 
    mutate(var = str_split_fixed(X, '\\[', 2)[,1]) %>% 
    # drop hierarchical terms than contain the intercept
    filter(!var %in% c( 'alpha', 'β0_c', 'β0_manage')) %>% 
    mutate(varname = str_split_fixed(varname, '\\[', 2)[,1],
           varnum=as.numeric(factor(var)),
           sig = ifelse(hdi_2.5. > 0 & hdi_97.5. > 0, 'black', 'white'),
           sig = ifelse(hdi_2.5. < 0 & hdi_97.5. < 0, 'black', sig),
           sig = ifelse(hdi_25. < 0 & hdi_75. < 0 & hdi_97.5. > 0, 'grey', sig),
           sig = ifelse(hdi_25. > 0 & hdi_75. > 0 & hdi_2.5. < 0, 'grey', sig))
  
  conts<-c('hard_coral', 'bare_sub', 'gravity', 'turf','rubble', 'population', 'macroalgae', 'depth', 'sediment', 'nut_load')
  
  g1<-ggplot(posts %>% filter(!varname %in% conts), aes(fct_reorder(varname,varnum),
                                                        mean, ymin = hdi_2.5., ymax = hdi_97.5.)) +
    geom_hline(yintercept=0, linetype=5, colour='grey') +
    geom_pointrange(fatten=0)  +
    geom_pointrange(fatten=0, aes(ymin = hdi_25., ymax = hdi_75.), size=1.1)  +
    geom_point(aes(fill=sig), size=2.5, pch=21) +
    coord_flip() +
    facet_grid(~fg) +
    scale_fill_identity() +
    labs(x = '', y = 'Posterior mean') +
    th
  
  g2<-ggplot(posts %>% filter(varname%in%conts), 
             aes(varname, mean, ymin = hdi_2.5., ymax = hdi_97.5.)) +
    geom_hline(yintercept=0, linetype=5, colour='grey') +
    geom_pointrange(fatten=0)  +
    geom_pointrange(fatten=0, aes(ymin = hdi_25., ymax = hdi_75.), size=1.1)  +
    geom_point(aes(fill=sig), size=2.5, pch=21) +
    coord_flip() +
    facet_grid(~fg) +
    scale_fill_identity() +
    labs(x = '', y = 'Posterior mean') +
    th
}

pdf(file = paste0('fig/model/', filename, '.pdf'), height=7, width=12)
print(g1)
print(g2)
dev.off()

rm(filename)