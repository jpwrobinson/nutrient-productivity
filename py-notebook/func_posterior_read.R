library(tidyverse)

stdize<-function(x){(x-mean(x))/(2*sd(x))}

focal<-read.csv('py-notebook/zinc.mg_reef_unscaled.csv')
scaled<-read.csv('py-notebook/zinc.mg_reef_scaled.csv')

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

pdf(file = figname, height=5, width=8)

post<-read.csv(paste0('py-notebook/', filename,filename, '_posterior_trace.csv'))

S<-stdize(focal[,covs2])
foc_seq<-rep(seq(min(S), max(S), length.out=100), each = 4)
foc_seq_raw<-rep(seq(min(focal$hard_coral), max(focal$hard_coral), length.out=100), each = 4)

scaled_avg<-scaled %>% filter(management_rules == 'restriction') %>% 
    group_by(country, management_rules) %>% 
  summarise(across(hard_coral:pop_count, ~ mean(.x)))
country_ints = c(colMeans(post[c(21,23,26,28)]))
int = mean(post$intercept)
mu<-with(post,
         rep(mean(intercept), times=400) + 
           mean(hard_coral)*foc_seq + 
           mean(macroalgae)*rep(scaled_avg$macroalgae, times=100) +
           mean(bare_sub)*rep(scaled_avg$bare_substrate, times=100) +
           mean(turf)*rep(scaled_avg$turf_algae, times=100) +
           mean(rubble)*rep(scaled_avg$rubble, times=100) +
           mean(population)*rep(scaled_avg$pop_count, times=100) +
           mean(gravity)*rep(scaled_avg$grav_nc, times=100) +
           mean(sediment)*rep(scaled_avg$sediment, times=100) +
           mean(nut_load)*rep(scaled_avg$nutrient_load, times=100) +
           rep(country_ints, times = 100))

pred<-data.frame(mu=exp(mu))
pred$manage<-names(mu)
pred$country<-str_replace_all(pred$manage, 'restriction', '')
pred$X<-foc_seq
pred$X_raw<-foc_seq_raw

g<-ggplot(pred, aes(X_raw, mu)) + 
  labs(x = covs, y = var_name, subtitle = covs) +
  # geom_ribbon(aes(ymin = lo, ymax = hi, group=fg), alpha=0.5, fill='grey90') +
  geom_line(aes(col=country))

print(g)


dev.off()
