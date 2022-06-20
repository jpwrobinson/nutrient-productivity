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
foc_seq<-rep(seq(min(S), max(S), length.out=100), times = 4)
foc_seq_raw<-rep(seq(min(focal$hard_coral), max(focal$hard_coral), length.out=100), times = 4)

scaled_avg<-scaled %>% filter(management_rules == 'restriction') %>% 
    group_by(country, management_rules) %>% 
  summarise(across(hard_coral:pop_count, ~ mean(.x)))
scaled_max<-scaled %>% filter(management_rules == 'restriction') %>% 
  group_by(country, management_rules) %>% 
  summarise(across(hard_coral:pop_count, ~ max(.x)))
country_ints = c(colMeans(post[c(21,23,26,28)]))
int = mean(post$intercept)

## setup counterfacs
# Belize = inc macroalgae
# Fiji = inc macroalgae
# Madagascar = inc macroalgae
# Solomon = inc rubble
S<-stdize(focal[,'macroalgae'])
ma_seq<-seq(min(S), max(S), length.out=100)

S<-stdize(focal[,'rubble'])
rub_seq<-seq(min(S), max(S), length.out=100)



mu<-with(post,
         rep(mean(intercept), times=400) + 
           mean(hard_coral)*foc_seq + 
           mean(macroalgae)*c(rep(ma_seq, times = 3), rep(scaled_avg$macroalgae[4], times = 100)) + 
           mean(bare_sub)*rep(scaled_avg$bare_substrate, each=100) +
           mean(turf)*rep(scaled_avg$turf_algae, each=100) +
           mean(rubble)*c(rep(scaled_avg$rubble[1:3], each = 100), rub_seq) +
           mean(population)*rep(scaled_avg$pop_count, each=100) +
           mean(gravity)*rep(scaled_avg$grav_nc, each=100) +
           mean(sediment)*rep(scaled_avg$sediment, each=100) +
           mean(nut_load)*rep(scaled_avg$nutrient_load, each=100) +
           rep(country_ints, each = 100))

pred<-data.frame(mu=exp(mu))
pred$manage<-names(mu)
pred$country<-str_replace_all(pred$manage, 'restriction', '')
pred$country<-str_replace_all(pred$country, '\\.', '\\ ')
pred$X<-foc_seq
pred$X_raw<-foc_seq_raw
pred<-pred %>% group_by(country) %>% mutate(prop_mu = mu / max(mu)*100, max = max(mu))

# filter unobserved benthic
cc<-unique(focal$country)
for(i in 1:4){
  maxer<-max(focal$hard_coral[focal$country==cc[i]])
  pred$X_raw[pred$X_raw > maxer & pred$country==cc[i]]<-NA
}

g<-ggplot(pred %>% filter(!is.na(X_raw)), aes(X_raw, mu)) + 
  labs(x = covs, y = var_name, subtitle = covs) +
  # geom_ribbon(aes(ymin = lo, ymax = hi, group=fg), alpha=0.5, fill='grey90') +
  geom_line(aes(col=country)) +
  scale_y_log10() +
  theme(legend.position = c(0.7, 0.6), legend.title=element_blank())

g2<-ggplot(pred %>% filter(!is.na(X_raw)), aes(X_raw, prop_mu)) +
  labs(x = covs, y = paste0('Proportion of maximum ', var_name,'%'), subtitle = covs) +
  # geom_ribbon(aes(ymin = lo, ymax = hi, group=fg), alpha=0.5, fill='grey90') +
  geom_line(aes(col=country)) +
  theme(legend.position = 'none') +
  facet_wrap(~country)

print(cowplot::plot_grid(g, g2))


dev.off()
