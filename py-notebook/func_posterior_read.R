library(tidyverse)

th<-theme(panel.grid.major = element_line(color = "grey85"))

stdize<-function(x){(x-mean(x))/(2*sd(x))}

focal<-read.csv('py-notebook/zinc.mg_reef_unscaled.csv')
scaled<-read.csv('py-notebook/zinc.mg_reef_scaled.csv')

ben.cols<-data.frame(col = c('#498FC9','#B6B400','#a1d99b', 'grey60'),
                     benthic = c('hard_coral', 'macroalgae', 'turf_algae', 'rubble'),
                     benthic_lab = c('Hard coral', 'Macroalgae', 'Turf algae', 'Rubble'))

ben_cols.named<-setNames(as.character(ben.cols$col), ben.cols$benthic)


pdf(file = figname, height=4, width=7)

post<-read.csv(paste0('py-notebook', filename, filename, '_posterior_trace.csv'))

S<-stdize(focal[,'hard_coral'])
foc_seq<-seq(min(S), max(S), length.out=100)
foc_seq_raw<-seq(min(focal$hard_coral), max(focal$hard_coral), length.out=100)

# scaled_avg<-scaled %>% #filter(management_rules == 'restriction') %>% 
#     group_by(country) %>%
#   summarise(across(hard_coral:pop_count, ~ mean(.x)))

# country_ints = c(colMeans(post[c(21,23,26,28)])) ## restricted
country_ints = c(colMeans(post[c(20,22,24,27)])) ## no-take

## setup counterfacs

S_ma<-stdize(focal[,'macroalgae'])
ma_seq<-seq(min(S_ma), max(S_ma), length.out=100)
ma_seq_raw<-seq(min(focal$macroalgae), max(focal$macroalgae), length.out=100)

S_rub<-stdize(focal[,'rubble'])
rub_seq<-seq(min(S_rub), max(S_rub), length.out=100)
rub_seq_raw<-seq(min(focal$rubble), max(focal$rubble), length.out=100)

S_ta<-stdize(focal[,'turf_algae'])
ta_seq<-seq(min(S_ta), max(S_ta), length.out=100)
ta_seq_raw<-seq(min(focal$turf_algae), max(focal$turf_algae), length.out=100)

times = 1

macroalgae<-with(post,
         rep(mean(intercept), times=100*times) + 
           rep(mean(country_ints), times=100*times) + 
           mean(hard_coral)*rep(foc_seq, times = times) +
           mean(macroalgae)*rep(rev(ma_seq), times=times))

turf_algae<-with(post,
            rep(mean(intercept), times=100) + 
              rep(mean(country_ints), times=100) +
              mean(hard_coral)*foc_seq +
              mean(turf)*rev(ta_seq))

rubble<-with(post,
            rep(mean(intercept), times=100) + 
              rep(mean(country_ints), times=100) +
              mean(hard_coral)*foc_seq +
              mean(rubble)*rev(rub_seq))

if(filename=='/density'){
  pred<-data.frame(macroalgae=macroalgae, turf_algae = turf_algae, rubble = rubble)} else {
    pred<-data.frame(macroalgae=exp(macroalgae), turf_algae = exp(turf_algae), rubble = exp(rubble))
  }
pred$hc_raw<-foc_seq_raw
pred$ma_raw<-rev(ma_seq_raw)
pred$rub_raw<-rev(rub_seq_raw)
pred$ta_raw<-rev(ta_seq_raw)
# pred$country <- rep(unique(focal$country), each = 100)

pred<-pred %>% pivot_longer(macroalgae:rubble, names_to = 'benthic', values_to = 'mu') %>% 
    group_by(benthic) %>% 
  mutate(prop_mu = mu / max(mu)*100, max = max(mu),
         prop_hc = hc_raw / max(hc_raw)*100)

g<-ggplot(pred %>% filter(!is.na(hc_raw)), aes(hc_raw, mu)) + 
  labs(x = 'hard coral, %', y = var_name) +
  # geom_ribbon(aes(ymin = lo, ymax = hi, group=fg), alpha=0.5, fill='grey90') +
  geom_line(size=1.3, aes(col=benthic)) +
  scale_colour_manual(values=ben_cols.named) +
  scale_y_log10() +
  theme(legend.position = c(0.2, 0.8), legend.title=element_blank()) + th

g2<-ggplot(pred %>% filter(!is.na(hc_raw)), aes(prop_hc, prop_mu)) +
  labs(x = 'proportion of max. hard coral', y = paste0('proportion of max. ', var_name)) +
  # geom_ribbon(aes(ymin = lo, ymax = hi, group=fg), alpha=0.5, fill='grey90') +
  geom_line(size=1.3, aes(col=benthic)) +
  scale_y_continuous(expand=c(0,0),breaks=seq(20, 100, 20), limits=c(20,100), labels=c('20%', '40%', '60%', '80%', '100%')) +
  scale_x_continuous(expand=c(0,0),breaks=seq(0, 100, 20), labels=c('0%', '20%', '40%', '60%', '80%', '100%')) +
  scale_colour_manual(values=ben_cols.named) +
  # facet_wrap(country~benthic) +
  theme(legend.position = 'none', plot.margin = unit(c(0.5,1,0.5,0.5), 'cm')) + th

print(cowplot::plot_grid(g, g2))

dev.off()

## predict maximum biomass by country
scaled_max<-scaled %>% #filter(management_rules == 'restriction') %>%
  group_by(country) %>%
  summarise(across(hard_coral:pop_count, ~ max(.x)))

unscaled_max<-focal %>% #filter(management_rules == 'restriction') %>%
  group_by(country) %>%
  summarise(across(hard_coral:pop_count, ~ max(.x)))

mu<-with(post,
            mean(hard_coral)*scaled_max$hard_coral + 
              # mean(macroalgae)*scaled_max$macroalgae +
              # mean(bare_sub)*scaled_max$bare_substrate +
              # mean(turf)*scaled_max$turf_algae +
              # mean(rubble)*scaled_max$rubble +
              # mean(population)*scaled_max$pop_count +
              # mean(gravity)*scaled_max$grav_nc +
              # mean(sediment)*scaled_max$sediment +
              # mean(nut_load)*scaled_max$nutrient_load +
              country_ints + mean(intercept))


if(filename=='/density'){
  maxer<-data.frame(max_val = mu)} else {
    maxer<-data.frame(max_val = exp(mu))
  }

maxer$max_hc = unscaled_max$hard_coral
maxer$hc_50<-0.5 * maxer$max_hc
maxer$country = unique(focal$country)


# filter unobserved benthic by country
# cc<-unique(focal$country)
# 
# pred2<-rbind(pred, pred, pred, pred)
# pred2$country<-rep(cc, each = dim(pred)[1])
# 
# for(i in 1:4){
#   maxit<-max(focal$hard_coral[focal$country==cc[i]])
#   pred2$hc_raw[pred2$hc_raw > maxit & pred2$country==cc[i]]<-NA
# }
# 
# pred2<-pred2 %>% group_by(country, benthic) %>% mutate(prop_mu = mu / max(mu)*100, max = max(mu))
# 
# ggplot(pred2 %>% filter(!is.na(hc_raw)), aes(hc_raw, prop_mu)) +
#   labs(x = 'hard coral, %', y = paste0('% of ', var_name,' at maximum coral cover')) +
#   # geom_ribbon(aes(ymin = lo, ymax = hi, group=fg), alpha=0.5, fill='grey90') +
#   geom_line(size=1.3, aes(col=benthic)) +
#   scale_colour_manual(values=ben_cols.named) +
#   facet_wrap(country~benthic) +
#   theme(legend.position = 'none') 