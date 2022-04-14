library(tidyverse)

stdize<-function(x){(x-mean(x))/(2*sd(x))}

focal<-read.csv('py-notebook/zinc.mg_unscaled.csv')
gravS<-stdize(focal$grav_nc)

fgnames<-data.frame(nfg = 0:5, fg = c('herbivore-detritivore',
          'herbivore-macroalgae',
          'invertivore-mobile',
          'omnivore',
          'piscivore',
          'planktivore'))

post<-read.csv('py-notebook/zinc_posterior_gravity.csv')
names(post)<-fgnames$fg
post$grav_nc<-seq(min(gravS), max(gravS), length.out=100)
post$grav_nc_raw<-seq(min(focal$grav_nc), max(focal$grav_nc), length.out=100)
post<-post %>% pivot_longer(-c(grav_nc, grav_nc_raw), names_to = 'fg', values_to = 'mu') 

lo<-read.csv('py-notebook/zinc_posterior_gravity_hpd_lo.csv')
names(lo)<-fgnames$fg
lo<-lo %>% pivot_longer(cols=everything(), names_to = 'fg', values_to = 'mu') 

hi<-read.csv('py-notebook/zinc_posterior_gravity_hpd_hi.csv')
names(hi)<-fgnames$fg
hi<-hi %>% pivot_longer(cols=everything(), names_to = 'fg', values_to = 'mu') 

post$lo<-lo$mu
post$hi<-hi$mu

ggplot(post, aes(grav_nc_raw, mu)) + 
  geom_ribbon(aes(ymin = lo, ymax = hi, group=fg), alpha=0.5, fill='grey90') +
  geom_line(aes(col=fg)) +
  labs(x = 'Gravity', y = 'Proportion zinc production') +
  facet_wrap(~fg)