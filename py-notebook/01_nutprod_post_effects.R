

posts<-read.csv('py-notebook/zinc_posterior_summary.csv') %>% 
      mutate(var = str_split_fixed(X, '\\[', 2)[,1]) %>% 
      filter(!var %in% c('intercept', 'alpha', 'β0_cnc', 'β0_managenc')) %>% 
      mutate(varname = str_split_fixed(varname, '\\[', 2)[,1],
             varnum=as.numeric(factor(var)))

conts<-c('hard_coral', 'bare_sub', 'gravity', 'turf', 'population', 'macroalgae', 'depth', 'sediment', 'nut_load')

g1<-ggplot(posts %>% filter(!varname %in% conts), aes(fct_reorder(varname,varnum), mean, ymin = hdi_2.5., ymax = hdi_97.5., col=fg)) +
      geom_hline(yintercept=0, linetype=5) +
      geom_pointrange(position = position_dodge(width=0.5))  +
      coord_flip() +
      labs(x = '', y = 'Posterior mean')

g2<-ggplot(posts %>% filter(varname%in%conts), 
           aes(varname, mean, ymin = hdi_2.5., ymax = hdi_97.5., col=fg)) +
  geom_hline(yintercept=0, linetype=5) +
  geom_pointrange(position = position_dodge(width=0.5))  +
  coord_flip() +
  labs(x = '', y = 'Posterior mean')

pdf(file = 'fig/model/posterior_params.pdf', height=5, width=10)
print(g1)
print(g2)
dev.off()




posts<-read.csv('py-notebook/prod_posterior_summary.csv') %>% 
  mutate(var = str_split_fixed(X, '\\[', 2)[,1]) %>% 
  filter(!var %in% c('intercept', 'alpha', 'β0_cnc', 'β0_managenc')) %>% 
  mutate(varname = str_split_fixed(varname, '\\[', 2)[,1],
         varnum=as.numeric(factor(var)))

conts<-c('hard_coral', 'bare_sub', 'gravity', 'turf', 'population', 'macroalgae', 'depth', 'sediment', 'nut_load')

g1<-ggplot(posts %>% filter(!varname %in% conts), aes(fct_reorder(varname,varnum), mean, ymin = hdi_2.5., ymax = hdi_97.5., col=fg)) +
  geom_hline(yintercept=0, linetype=5) +
  geom_pointrange(position = position_dodge(width=0.5))  +
  coord_flip() +
  labs(x = '', y = 'Posterior mean')

g2<-ggplot(posts %>% filter(varname%in%conts), 
           aes(varname, mean, ymin = hdi_2.5., ymax = hdi_97.5., col=fg)) +
  geom_hline(yintercept=0, linetype=5) +
  geom_pointrange(position = position_dodge(width=0.5))  +
  coord_flip() +
  labs(x = '', y = 'Posterior mean')

g1
g2