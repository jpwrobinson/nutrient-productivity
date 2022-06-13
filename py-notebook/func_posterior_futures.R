library(tidyverse)

stdize<-function(x){(x-mean(x))/(2*sd(x))}

focal<-read.csv('py-notebook/zinc.mg_reef_unscaled.csv')

covs<-c('future_hc')
covs2<-c('hard_coral')

managecovs<-c('Belizeno-take',
              'Belizerestriction',
              'Fijino-take',
              'Fijirestriction',
              'Madagascarno-take',
              'Madagascaropen-access',
              'Madagascarrestriction',
              'Solomon Islandsno-take',
              'Solomon Islandsrestriction')

managelab<-c('no-take',
             'restriction',
             'no-take',
             'restriction',
             'no-take',
             'open-access',
             'restriction',
             'no-take',
             'restriction')

country<-c('Belize',
           'Belize',
           'Fiji',
           'Fiji',
           'Madagascar',
           'Madagascar',
           'Madagascar',
           'Solomon Islands',
           'Solomon Islands')

pdf(file = figname, height=5, width=12)

for(i in 1:length(covs)){
  post<-read.csv(paste0(filename, 'posterior_', covs[i],'.csv'))
  names(post)<-'mu'
  post$mu <- post$mu
  
  S<-stdize(focal[,covs2[i]])
  post$X<-rep(seq(min(S), max(S), length.out=100), each = 9)
  post$X_raw<-rep(seq(min(focal[,covs2[i]]), max(focal[,covs2[i]]), length.out=100), each = 9)
  post$manage<-rep(managecovs, times = 100)
  post$manage_lab<-rep(managelab, times = 100)
  post$country<-rep(country, times = 100)
  
  ## read HPDI  
  lo<-read.csv(paste0(filename, 'posterior_', covs[i],'_hpd_lo.csv'))
  names(lo)<-'mu'
  
  hi<-read.csv(paste0(filename, 'posterior_', covs[i],'_hpd_hi.csv'))
  names(hi)<-'mu'
  
  post$lo<-lo$mu
  post$hi<-hi$mu
  
  g<-ggplot(post, aes(X_raw, mu)) + 
    labs(x = covs[i], y = var_name, subtitle = covs[i]) +
    facet_grid(~manage_lab) +
    # geom_ribbon(aes(ymin = lo, ymax = hi, group=fg), alpha=0.5, fill='grey90') +
    geom_line(aes(col=country))
  
  print(g)
}

dev.off()
