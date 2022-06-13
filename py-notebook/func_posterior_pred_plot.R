library(tidyverse)

stdize<-function(x){(x-mean(x))/(2*sd(x))}

# fgnames<-data.frame(nfg = 0:6, fg = c('browser',
#                                       'cropper/grazer',
#                                       'invertivore-mobile',
#                                       'scraper-excavator',
#                                       'piscivore',
#                                       'planktivore',
#                                       'mixed-diet feeder'))

covs<-c('gravity',
        'hard_coral',
        'macroalgae',
        'bare_substrate',
        'turf',
        'manage',
        # 'country',
        'pop',
        'sediment',
        'nut_load',
        'rubble')

covs2<-c('grav_nc',
         'hard_coral',
         'macroalgae',
         'bare_substrate',
         'turf_algae',
         'management_rules',
         # 'country',
         'pop_count', 
         'sediment', 
         'nutrient_load',
         'rubble')


managecovs<-c('Belizerestriction',
              'Belizeno-take',
              'Fijirestriction',
              'Fijino-take',
              'Madagascarrestriction',
              'Madagascarno-take',
              'Madagascaropen-access',
              'Solomon Islandsrestriction',
              'Solomon Islandsno-take')

countrycovs<-c('Belize','Fiji', 'Madgascar', 'Solomon Islands')

pdf(file = figname, height=5, width=12)

for(i in 1:length(covs)){
  post<-read.csv(paste0(filename, 'posterior_', covs[i],'.csv'))
  # names(post)<-fgnames$fg
  names(post) = 'mu'
  post$mu <- post$mu
  
  if(!covs[i] %in% c('manage', 'country')){
    S<-stdize(focal[,covs2[i]])
    post$X<-seq(min(S), max(S), length.out=100)
    post$X_raw<-seq(min(focal[,covs2[i]]), max(focal[,covs2[i]]), length.out=100)
  } else if (covs[i] == 'manage'){
    post$X<-managecovs
    post$X_raw<-post$X
  } else if (covs[i] == 'country') {
    post$X<-countrycovs
    post$X_raw<-post$X
  }

  ## read HPDI  
  lo<-read.csv(paste0(filename, 'posterior_', covs[i],'_hpd_lo.csv'))
  names(lo)<-'mu'
  
  hi<-read.csv(paste0(filename, 'posterior_', covs[i],'_hpd_hi.csv'))
  names(hi)<-'mu'
  
  post$lo<-lo$mu
  post$hi<-hi$mu
  
  g<-ggplot(post, aes(X_raw, mu)) + 
    labs(x = covs[i], y = 'Posterior predicted', subtitle = covs[i])
  
  if(!covs[i] %in% c('manage', 'country')){
    print(g + geom_ribbon(aes(ymin = lo, ymax = hi), alpha=0.5, fill='grey90') +
                                geom_line())} else {
                                  print(g + geom_pointrange(aes(ymin = lo, ymax = hi, col=X)))
                                }
}

dev.off()