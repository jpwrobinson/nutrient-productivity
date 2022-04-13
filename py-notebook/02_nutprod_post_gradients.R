library(tidyverse)

stdize<-function(x){(x-mean(x))/(2*sd(x))}


fgnames<-data.frame(nfg = 0:5, fg = c('herbivore-detritivore',
          'herbivore-macroalgae',
          'invertivore-mobile',
          'omnivore',
          'piscivore',
          'planktivore'))

post<-read_csv('py-notebook/zinc_posterior_trace.csv', col_select = -starts_with("alpha")) %>% 
      mutate(id = 'Zinc') %>% 
      pivot_longer(-id, names_to = 'var', values_to = 'value') %>% 
      filter(var != 'Sigma_country') %>% 
      mutate(varname = str_split_fixed(var, '__', 2)[,1],
             nfg = str_split_fixed(var, '__', 2)[,2]) %>% 
      mutate(nfg = ifelse(nchar(nfg)>1, str_split_fixed(var, '_', 2)[,2], nfg),
             nfg=as.numeric(nfg)) %>% 
    left_join(fgnames)

focal<-read.csv('py-notebook/zinc.mg_unscaled.csv')
grav_nc<-stdize(focal$grav_nc)

grav<-data.frame(grav = seq(min(grav_nc), max(grav_nc), length.out=100))
gravM<-matrix(replicate(6,grav$grav),nrow=100)

int<-post %>% filter(varname=='intercept') %>% group_by(fg) %>% 
  summarise(m = median(value)) %>% ungroup() %>% summarise(mu=sum(m))

# now get gravity slopes for each fg
g<-post %>% filter(varname=='gravity') %>% select(value, fg) %>% 
  pivot_wider(., names_from = fg, values_from = value) %>% 
  unnest()

# multiply slopes by gravity vector
gs<-rowSums(gravM%*%colMeans(g))
# sum expected proportion across each gravity value
gExp<-gs + int$mu

# pull out fg expectations for gravity
pp<-data.frame(alpha_0 = gExp)
for(i in 1:6){
  temp<-post %>% filter(fg == fgnames$fg[i]) 
  pp[,i+1]<-median(temp$value[temp$varname=='intercept']) + 
    median(temp$value[temp$varname=='gravity'])*grav$grav
}
colnames(pp)[2:7]<-fgnames$fg

# correct fg ex by alpha_0
pp<-pp %>% pivot_longer(-alpha_0, names_to = 'fg', values_to = 'mu') %>% 
        mutate(gravity = rep(grav$grav, each = 6),
               ex = mu / alpha_0)

ggplot(pp, aes(gravity, ex, col=fg)) + geom_line()




