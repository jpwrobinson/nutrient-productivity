library(tidyverse)

stdize<-function(x){(x-mean(x))/(2*sd(x))}

focal<-read.csv('py-notebook/zinc.mg_reef_unscaled.csv')
scaled<-read.csv('py-notebook/zinc.mg_reef_scaled.csv')

covs<-c('future_hc')
covs2<-c('hard_coral')
# 
# managecovs<-c('Belizeno-take',
#              'Belizerestriction',
#              'Fijino-take',
#              'Fijirestriction',
#              'Madagascarno-take',
#              'Madagascaropen-access',
#              'Madagascarrestriction',
#              'Solomon Islandsno-take',
#              'Solomon Islandsrestriction')
# 
# managelab<-c('no-take',
#              'restriction',
#              'no-take',
#              'restriction',
#              'no-take',
#              'open-access',
#              'restriction',
#              'no-take',
#              'restriction')
# 
# country<-c('Belize',
#            'Belize',
#            'Fiji',
#            'Fiji',
#            'Madagascar',
#            'Madagascar',
#            'Madagascar',
#            'Solomon Islands',
#            'Solomon Islands')

pdf(file = figname, height=5, width=8)

post<-read.csv(paste0('py-notebook', filename, filename, '_posterior_trace.csv'))

S<-stdize(focal[,covs2])
foc_seq<-seq(min(S), max(S), length.out=100)
foc_seq_raw<-seq(min(focal$hard_coral), max(focal$hard_coral), length.out=100)

scaled_avg<-scaled %>% #filter(management_rules == 'restriction') %>% 
    group_by(country) %>%
  summarise(across(hard_coral:pop_count, ~ mean(.x)))
scaled_max<-scaled %>% #filter(management_rules == 'restriction') %>% 
  # group_by(country, management_rules) %>% 
  summarise(across(hard_coral:pop_count, ~ max(.x)))

# country_ints = c(colMeans(post[c(21,23,26,28)]))

## setup counterfacs
# Belize = inc macroalgae
# Fiji = inc macroalgae
# Madagascar = inc macroalgae
# Solomon = inc rubble
S_ma<-stdize(focal[,'macroalgae'])
ma_seq<-seq(min(S_ma), max(S_ma), length.out=100)
ma_seq_raw<-seq(min(focal$macroalgae), max(focal$macroalgae), length.out=100)

S_rub<-stdize(focal[,'rubble'])
rub_seq<-seq(min(S_rub), max(S_rub), length.out=100)
rub_seq_raw<-seq(min(focal$rubble), max(focal$rubble), length.out=100)

S_ta<-stdize(focal[,'turf_algae'])
ta_seq<-seq(min(S_ta), max(S_ta), length.out=100)
ta_seq_raw<-seq(min(focal$turf_algae), max(focal$turf_algae), length.out=100)

mu_ma<-with(post,
         rep(mean(intercept), times=100) + 
           mean(hard_coral)*foc_seq +
           mean(macroalgae)*rev(ma_seq))

mu_ta<-with(post,
            rep(mean(intercept), times=100) + 
              mean(hard_coral)*foc_seq +
              mean(turf)*rev(ta_seq))

mu_rub<-with(post,
            rep(mean(intercept), times=100) + 
              mean(hard_coral)*foc_seq +
              mean(rubble)*rev(rub_seq))

if(filename=='/density'){
  pred<-data.frame(mu_ma=mu_ma, mu_ta = mu_ta, mu_rub = mu_rub)} else {
    pred<-data.frame(mu_ma=exp(mu_ma), mu_ta = exp(mu_ta), mu_rub = exp(mu_rub))
  }
pred$hc_raw<-foc_seq_raw
pred$ma_raw<-rev(ma_seq_raw)
pred$rub_raw<-rev(rub_seq_raw)
pred$ta_raw<-rev(ta_seq_raw)

pred<-pred %>% pivot_longer(mu_ma:mu_rub, names_to = 'benthic', values_to = 'mu') %>% 
    group_by(benthic) %>% mutate(prop_mu = mu / max(mu)*100, max = max(mu))

# filter unobserved benthic
# cc<-unique(focal$country)
# for(i in 1:4){
#   maxer<-max(focal$hard_coral[focal$country==cc[i]])
#   pred$X_raw[pred$X_raw > maxer & pred$country==cc[i]]<-NA
# }

g<-ggplot(pred %>% filter(!is.na(hc_raw)), aes(hc_raw, mu)) + 
  labs(x = covs, y = var_name, subtitle = covs) +
  # geom_ribbon(aes(ymin = lo, ymax = hi, group=fg), alpha=0.5, fill='grey90') +
  geom_line(aes(col=benthic)) +
  scale_y_log10() +
  theme(legend.position = c(0.2, 0.8), legend.title=element_blank())

g2<-ggplot(pred %>% filter(!is.na(hc_raw)), aes(hc_raw, prop_mu)) +
  labs(x = covs, y = paste0('Proportion of maximum ', var_name,'%'), subtitle = covs) +
  # geom_ribbon(aes(ymin = lo, ymax = hi, group=fg), alpha=0.5, fill='grey90') +
  geom_line(aes(col=benthic)) +
  theme(legend.position = 'none') 

print(cowplot::plot_grid(g, g2))


dev.off()
