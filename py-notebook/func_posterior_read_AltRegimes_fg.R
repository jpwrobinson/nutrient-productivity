library(tidyverse)

th<-theme(panel.grid.major = element_line(color = "grey85"))

stdize<-function(x){(x-mean(x))/(2*sd(x))}

focal<-read.csv('py-notebook/productivity_unscaled.csv')
scaled<-read.csv('py-notebook/productivity_scaled.csv')

ben.cols<-data.frame(col = c('#498FC9','#B6B400','#a1d99b', 'grey60'),
                     benthic = c('hard_coral', 'macroalgae', 'turf_algae', 'rubble'),
                     benthic_lab = c('Hard coral', 'Macroalgae', 'Turf algae', 'Rubble'))

ben_cols.named<-setNames(as.character(ben.cols$col), ben.cols$benthic)

fg.cols<-data.frame(col=c('#fdae61', '#377eb8','#80cdc1', '#4d9221', '#d9ef8b', '#bebada'),
                    FG=c('invertivore-mobile', 'piscivore','omnivore', 'herbivore-macroalgae', 'herbivore-detritivore','planktivore'),
                    FG_lab=c("Invertivore (mobile)", 'Piscivore','Omnivore', 'Herbivore (browser)','Herbivore (grazer/scraper)', 'Planktivore'))
fg_cols.named<-setNames(as.character(fg.cols$col), fg.cols$FG)

# hnames<-names(focal  %>% select(herbivore.detritivore:planktivore)) %>% str_replace_all('\\.', '-')
hnames<-c("herbivore-detritivore","herbivore-macroalgae","invertivore-mobile","piscivore","planktivore", "omnivore")


covs<-c('macroalgae','bare_substrate','turf_algae','rubble')
covs2<-c('macroalgae','bare_sub','turf','rubble')

pdf(file = figname, height=5, width=6)

post<-read.csv(paste0('py-notebook', filename,var_name, '_posterior_trace.csv'))

ints_country<-post %>% 
  select(starts_with(c('Belize', 'Fiji', 'Sol', 'Mad'))) %>% 
  select(!contains(c('restriction', 'no.take', 'open'))) %>% 
  rename(Belize.0 = Belize, Fiji.0 = Fiji, Madagascar.0 = Madagascar, Solomon.Islands.0 = Solomon.Islands) %>% 
   rename_with(.fn = ~ str_replace(.x, "Solomon.Islands", "Solomon_Islands"),
                .cols = starts_with("Solomon.Islands")) %>% 
    pivot_longer(
      cols = everything(),
      names_to = c("cov", "fg"),
      names_sep = "\\.",
      values_to = "mu"
    )

ints_fringing<-post %>% 
  select(contains('fringing')) %>% 
   rename(fringing.0 = fringing) %>% 
    pivot_longer(
      cols = everything(),
      names_to = c("cov", "fg"),
      names_sep = "\\.",
      values_to = "mu"
    )

ints<-post %>% 
  select(starts_with(c('Intercept'))) %>% 
    pivot_longer(
      cols = everything(),
      names_to = c("cov", "fg"),
      names_sep = "__",
      values_to = "mu"
    )

conts<-post %>% select(starts_with(c( 'hard', 'mac', 'bare', 'turf', 'rub', 'pop', 'nut', 'grav', 'sed', 'dep'))) %>% 
  pivot_longer(
    cols = everything(),
    names_to = c("cov", "fg"),
    names_sep = "__",
    values_to = "mu"
  )

## add real fg names
ints$fg<-rep(hnames, times = dim(ints)[1]/6)
ints_country$fg<-rep(hnames, times = dim(ints_country)[1]/6)
ints_fringing$fg<-rep(hnames, times = dim(ints_fringing)[1]/6)
conts$fg<-rep(hnames, times = dim(conts)[1]/6)

meanInts<-ints  %>%  group_by(fg) %>% summarise(med = median(mu),
      lo = rethinking::HPDI(mu, prob=.95)[1], hi = rethinking::HPDI(mu, prob=.95)[2])

meanIntsCo<-ints_country  %>%  filter(cov != 'Belize') %>% group_by(fg) %>% summarise(med = median(mu),
      lo = rethinking::HPDI(mu, prob=.95)[1], hi = rethinking::HPDI(mu, prob=.95)[2])

meanIntsFringing<-ints_fringing  %>%  group_by(fg) %>% summarise(med = median(mu),
      lo = rethinking::HPDI(mu, prob=.95)[1], hi = rethinking::HPDI(mu, prob=.95)[2])


main<-numeric()
for(a in 1:length(covs)){
  # pull focal covariate, scale and raw versions
  S<-stdize(focal[,covs[a]])
  foc_seq<-seq(min(S), max(S), length.out=100)
  foc_seq_raw<-seq(min(focal[,covs[a]]), max(focal[,covs[a]]), length.out=100)

  # get hard coral sequence, reversed
  coral_seq<-stdize(focal[,'hard_coral'])
  coral_seq<-seq(max(coral_seq), min(coral_seq), length.out=100)
  coral_seq_raw<-seq(max(focal[,'hard_coral']), min(focal[,'hard_coral']), length.out=100)


  pred<-numeric()
    for(i in 1:length(hnames)){

      # get median and HPDI for each FG and covariate
      dd<-conts %>% filter(fg == hnames[i]) %>% group_by(cov) %>% 
        summarise(med = median(mu), lo = rethinking::HPDI(mu, prob=.95)[1], hi = rethinking::HPDI(mu, prob=.95)[2]) %>% 
        filter(cov == covs2[a])

      # pull stats 
      p<-dd %>% pull(med)*foc_seq  +  dd %>% pull(med)*coral_seq +
            # meanInts$med[meanInts$fg == hnames[i]] 
            # meanIntsFringing$med[meanIntsFringing$fg == hnames[i]] +
            meanIntsCo$med[meanIntsCo$fg == hnames[i]] 

      lo<-dd %>% pull(lo)*foc_seq  +  dd %>% pull(lo)*coral_seq +
            # meanInts$lo[meanInts$fg == hnames[i]] 
            # meanIntsFringing$lo[meanIntsFringing$fg == hnames[i]] +
            meanIntsCo$lo[meanIntsCo$fg == hnames[i]] 

      hi<-dd %>% pull(hi)*foc_seq  +  dd %>% pull(hi)*coral_seq +
            # meanInts$hi[meanInts$fg == hnames[i]] 
            # meanIntsFringing$hi[meanIntsFringing$fg == hnames[i]] +
            meanIntsCo$hi[meanIntsCo$fg == hnames[i]] 

      
      ## save
      pred<-rbind(pred, data.frame(mu = exp(p), lo = exp(lo), hi = exp(hi),
                                  X = foc_seq, 
                                  X_raw = foc_seq_raw, 
                                  coral = coral_seq, 
                                  coral_raw = coral_seq_raw,
                                  fg = hnames[i]))
    }

      pred <- pred %>% group_by(X, X_raw, coral, coral_raw) %>% 
        mutate(tot = sum(mu), prop = mu / tot)  %>% 
        ungroup() %>% 
        mutate(tot_lo = tot - mu + lo, prop_lo = lo / tot_lo,
              tot_hi = tot - mu + hi, prop_hi = hi / tot_hi)


    # plot 
  print(
    ggplot(pred, aes(X_raw, prop, fill=fg, ymin = prop_lo, ymax = prop_hi)) + 
      # geom_ribbon(alpha=0.2) +
      geom_line(aes(col=fg)) + 
      labs(x = covs[a], y = 'proportion community, %') +
      scale_fill_manual(values = trophic_cols.named2) +
      scale_colour_manual(values = trophic_cols.named2) +
      # facet_grid(~fg) +
      scale_y_continuous(expand=c(0,0)) +
      theme(legend.position = 'none')
      )

    main<-rbind(main, data.frame(pred %>% mutate(cov = covs[a])))
}



write.csv(main, file = csvname, row.names=FALSE)
dev.off()