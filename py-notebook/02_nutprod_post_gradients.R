library(tidyverse)

stdize<-function(x){(x-mean(x))/(2*sd(x))}

focal<-read.csv('py-notebook/zinc.mg_unscaled.csv')


fgnames<-data.frame(nfg = 0:5, fg = c('herbivore-detritivore',
          'herbivore-macroalgae',
          'invertivore-mobile',
          'omnivore',
          'piscivore',
          'planktivore'))

covs<-c('gravity',
        'hard_coral',
        'macroalgae',
        'bare_substrate',
        'turf',
        'manage')

covs2<-c('grav_nc',
        'hard_coral',
        'macroalgae',
        'bare_substrate',
        'turf_algae',
        'management_rules')

pdf(file = 'fig/model/posterior_predicted.pdf', height=7, width=12)

for(i in 1:length(covs)){
  post<-read.csv(paste0('py-notebook/zinc_posterior_', covs[i],'.csv'))
  names(post)<-fgnames$fg
  if(covs[i]!='manage'){
    S<-stdize(focal[,covs2[i]])
    post$X<-seq(min(S), max(S), length.out=100)
    post$X_raw<-seq(min(focal[,covs2[i]]), max(focal[,covs2[i]]), length.out=100)
  } else {
    post$X<-c('access restriction',
              'gear restriction',
              'no-take',
              'open-access',
              'time restriction')
    post$X_raw<-post$X
    }
  
  post<-post %>% pivot_longer(-c(X, X_raw), names_to = 'fg', values_to = 'mu') 
  
  lo<-read.csv(paste0('py-notebook/zinc_posterior_', covs[i],'_hpd_lo.csv'))
  names(lo)<-fgnames$fg
  lo<-lo %>% pivot_longer(cols=everything(), names_to = 'fg', values_to = 'mu') 
  
  hi<-read.csv(paste0('py-notebook/zinc_posterior_', covs[i],'_hpd_hi.csv'))
  names(hi)<-fgnames$fg
  hi<-hi %>% pivot_longer(cols=everything(), names_to = 'fg', values_to = 'mu') 
  
  post$lo<-lo$mu
  post$hi<-hi$mu
  
  g<-ggplot(post, aes(X_raw, mu)) + 
    geom_ribbon(aes(ymin = lo, ymax = hi, group=fg), alpha=0.5, fill='grey90') +
    geom_line(aes(col=fg)) +
    labs(x = covs[i], y = 'Proportion zinc production', subtitle = covs[i]) +
    facet_wrap(~fg)
  print(g)
}

dev.off()