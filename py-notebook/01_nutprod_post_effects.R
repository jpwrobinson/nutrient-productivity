theme_set(theme_sleek())

th<-theme(panel.spacing=unit(.05, "lines"),
          strip.text.y = element_blank(),
      panel.border = element_blank(),
      legend.position = 'none',
      axis.line = element_line(colour='black'))

posts<-read.csv('py-notebook/zinc_posterior_summary.csv') %>% 
      mutate(var = str_split_fixed(X, '\\[', 2)[,1]) %>% 
      filter(!var %in% c('intercept', 'alpha', 'β0_cnc', 'β0_managenc')) %>% 
      mutate(varname = str_split_fixed(varname, '\\[', 2)[,1],
             varnum=as.numeric(factor(var)),
             sig = ifelse(hdi_2.5. > 0 & hdi_97.5. > 0, 'black', 'white'),
             sig = ifelse(hdi_2.5. < 0 & hdi_97.5. < 0, 'black', sig))

conts<-c('hard_coral', 'bare_sub', 'gravity', 'turf','rubble', 'population', 'macroalgae', 'depth', 'sediment', 'nut_load')

g1<-ggplot(posts %>% filter(!varname %in% conts), aes(fct_reorder(varname,varnum), 
                                                      mean, ymin = hdi_2.5., ymax = hdi_97.5.)) +
      geom_hline(yintercept=0, linetype=5, colour='grey') +
      geom_pointrange(fatten=0)  +
      geom_point(aes(fill=sig), size=2.5, pch=21) +
      coord_flip() +
      scale_fill_identity() +
      facet_grid(varname~fg, scales='free') +
      labs(x = '', y = 'Posterior mean') +
      th

g2<-ggplot(posts %>% filter(varname%in%conts), 
           aes(varname, mean, ymin = hdi_2.5., ymax = hdi_97.5.)) +
  geom_hline(yintercept=0, linetype=5, colour='grey') +
  geom_pointrange(fatten=0)  +
  geom_point(aes(fill=sig), size=2.5, pch=21) +
  coord_flip() +
  scale_fill_identity() +
  facet_grid(varname~fg, scales='free') +
  labs(x = '', y = 'Posterior mean') +
  th

pdf(file = 'fig/model/posterior_params.pdf', height=7, width=12)
print(g1)
print(g2)
dev.off()
# 
# 
# 
# 
# posts<-read.csv('py-notebook/prod_posterior_summary.csv') %>% 
#   mutate(var = str_split_fixed(X, '\\[', 2)[,1]) %>% 
#   filter(!var %in% c('intercept', 'alpha', 'β0_cnc', 'β0_managenc')) %>% 
#   mutate(varname = str_split_fixed(varname, '\\[', 2)[,1],
#          varnum=as.numeric(factor(var)))
# 
# conts<-c('hard_coral', 'bare_sub', 'gravity', 'turf', 'population', 'macroalgae', 'depth', 'sediment', 'nut_load')
# 
# g1<-ggplot(posts %>% filter(!varname %in% conts), aes(fct_reorder(varname,varnum), mean, ymin = hdi_2.5., ymax = hdi_97.5., col=fg)) +
#   geom_hline(yintercept=0, linetype=5) +
#   geom_pointrange(position = position_dodge(width=0.5))  +
#   coord_flip() +
#   labs(x = '', y = 'Posterior mean')
# 
# g2<-ggplot(posts %>% filter(varname%in%conts), 
#            aes(varname, mean, ymin = hdi_2.5., ymax = hdi_97.5., col=fg)) +
#   geom_hline(yintercept=0, linetype=5) +
#   geom_pointrange(position = position_dodge(width=0.5))  +
#   coord_flip() +
#   labs(x = '', y = 'Posterior mean')
# 
# g1
# g2