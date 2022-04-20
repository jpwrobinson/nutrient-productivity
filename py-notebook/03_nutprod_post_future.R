library(tidyverse)

stdize<-function(x){(x-mean(x))/(2*sd(x))}

focal<-read.csv('py-notebook/zinc.mg_unscaled.csv')


fgnames<-data.frame(nfg = 0:6, fg = c('browser',
                                      'cropper/grazer',
                                      'invertivore-mobile',
                                      'scraper-excavator',
                                      'piscivore',
                                      'planktivore',
                                      'mixed-diet feeder'))

covs<-c('future_hc')
covs2<-c('hard_coral')

managecovs<-c('Belizegear restriction',
              'Belizeno-take',
              'Fijiaccess restriction',
              'Fijino-take',
              'Fijitime restriction',
              'Madagascargear restriction',
              'Madagascarno-take',
              'Madagascaropen-access',
              'Solomon Islandsaccess restriction',
              'Solomon Islandsno-take',
              'Solomon Islandstime restriction')

managelab<-c('gear restriction',
             'no-take',
             'access restriction',
             'no-take',
             'time restriction',
             'gear restriction',
             'no-take',
             'open-access',
             'access restriction',
             'no-take',
             'time restriction')

country<-c('Belize',
           'Belize',
           'Fiji',
           'Fiji',
           'Fiji',
           'Madagascar',
           'Madagascar',
           'Madagascar',
           'Solomon Islands',
           'Solomon Islands',
           'Solomon Islands')

pdf(file = 'fig/model/posterior_predicted_future.pdf', height=5, width=12)

for(i in 1:length(covs)){
  post<-read.csv(paste0('py-notebook/zinc_posterior_', covs[i],'.csv'))
  names(post)<-fgnames$fg
  
  S<-stdize(focal[,covs2[i]])
  post$X<-rep(seq(min(S), max(S), length.out=100), each = 11)
  post$X_raw<-rep(seq(min(focal[,covs2[i]]), max(focal[,covs2[i]]), length.out=100), each = 11)
  post$manage<-rep(managecovs, times = 100)
  post$manage_lab<-rep(managelab, times = 100)
  post$country<-rep(country, times = 100)
  
  post<-post %>% pivot_longer(-c(X, X_raw, manage, manage_lab, country), names_to = 'fg', values_to = 'mu') 
  
  lo<-read.csv(paste0('py-notebook/zinc_posterior_', covs[i],'_hpd_lo.csv'))
  names(lo)<-fgnames$fg
  lo<-lo %>% pivot_longer(cols=everything(), names_to = 'fg', values_to = 'mu') 
  
  hi<-read.csv(paste0('py-notebook/zinc_posterior_', covs[i],'_hpd_hi.csv'))
  names(hi)<-fgnames$fg
  hi<-hi %>% pivot_longer(cols=everything(), names_to = 'fg', values_to = 'mu') 
  
  post$lo<-lo$mu
  post$hi<-hi$mu
  
  g<-ggplot(post, aes(X_raw, mu)) + 
    labs(x = covs[i], y = 'Proportion zinc production', subtitle = covs[i]) +
    facet_grid(country~manage_lab) +
    # geom_ribbon(aes(ymin = lo, ymax = hi, group=fg), alpha=0.5, fill='grey90') +
    geom_line(aes(col=fg))
  
  print(g)
}

dev.off()