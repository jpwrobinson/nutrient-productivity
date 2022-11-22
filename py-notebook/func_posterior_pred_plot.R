library(tidyverse)

stdize<-function(x){(x-mean(x))/(2*sd(x))}
# hnames<-c("herbivore-detritivore","herbivore-macroalgae","invertivore-mobile","piscivore","planktivore", "omnivore")
hnames=c("herbivore","invertivore-mobile","piscivore","planktivore", "omnivore")


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


managecovs<-c('Belizeno-take',
              'Belizerestriction',
              'Fijino-take',
              'Fijirestriction',
              'Madagascarno-take',
              'Madagascaropen-access',
              'Madagascarrestriction',
              'Solomon Islandsno-take',
              'Solomon Islandsrestriction')

countrycovs<-c('Belize','Fiji', 'Madgascar', 'Solomon Islands')


if(fg == FALSE){repper = 1} else {repper = length(hnames)}

pdf(file = figname, height=5, width=12)

for(i in 1:length(covs)){
  post<-read.csv(paste0(filename, 'posterior_', covs[i],'.csv'))

  if(fg==FALSE){
  names(post) = 'mu'
  post$mu <- post$mu}

  if(fg==TRUE){
    post<-post %>% pivot_longer(everything(), names_to = 'fg', values_to = 'mu')
    post$fg<-rep(hnames, times = dim(post)[1]/6)
  }
  
  if(!covs[i] %in% c('manage', 'country')){
    S<-stdize(focal[,covs2[i]])
    post$X<-rep(seq(min(S), max(S), length.out=100), each = repper)
    post$X_raw<-rep(seq(min(focal[,covs2[i]]), max(focal[,covs2[i]]), length.out=100), each = repper)
  } else if (covs[i] == 'manage'){
    post$X<-rep(managecovs, each = repper)
    post$X_raw<-post$X
  } else if (covs[i] == 'country') {
    post$X<-rep(countrycovs, each = repper)
    post$X_raw<-post$X
  }

  ## read HPDI  
  if(fg==FALSE){
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

  ## read HPDI  
  if(fg==TRUE){
    lo<-read.csv(paste0(filename, 'posterior_', covs[i],'_hpd_lo.csv')) %>% 
      pivot_longer(everything(), names_to = 'fg', values_to = 'lo')
    lo$fg<-rep(hnames, times = dim(lo)[1]/6)
    
    hi<-read.csv(paste0(filename, 'posterior_', covs[i],'_hpd_hi.csv')) %>% 
      pivot_longer(everything(), names_to = 'fg', values_to = 'hi')
    hi$fg<-rep(hnames, times = dim(hi)[1]/6)
    
    post$lo<-lo$lo
    post$hi<-hi$hi
    
    g<-ggplot(post, aes(X_raw, mu)) + 
      labs(x = covs[i], y = 'Posterior predicted', subtitle = covs[i]) +
      facet_grid(~fg)
    
    if(!covs[i] %in% c('manage', 'country')){
      print(g + geom_ribbon(aes(ymin = lo, ymax = hi), alpha=0.5, fill='grey90') +
                                  geom_line())} else {
                                    print(g + geom_pointrange(aes(ymin = lo, ymax = hi, col=X)))
                                  }
  }
}

dev.off()