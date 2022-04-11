

posts<-read.csv('py-notebook/zinc_posterior_summary.csv') %>% 
      mutate(var = str_split_fixed(X, '\\[', 2)[,1]) %>% 
      filter(var != 'intercept')

ggplot(posts, aes(var, mean, ymin = hdi_3., ymax = hdi_97., col=fg)) +
      geom_hline(yintercept=0, linetype=5) +
      geom_pointrange(position = position_dodge(width=0.5))  +
      coord_flip() +
      labs(x = '', y = 'Posterior mean')