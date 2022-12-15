## Functional group nutrient production by country
source('scripts/0_plot_theme.R')
library(rethinking)
library(brms)
library(ggradar)
nuts<-c('zinc.mg','calcium.mg','iron.mg','vitamin_a.mug','selenium.mug','omega3.g')

colcol<-trophic_cols.named3[c(1,2,4,6)]
post<-numeric()
covs<-numeric()
pyramid<-numeric()

for(i in 1:length(nuts)){
  nut<-nuts[i]
  load(paste0('results/mod/', nut, '_brms.Rdata'))
  pp<-as_draws_df(fit, variable = "depth|coral|algae|rubble|bare", regex=TRUE) %>% 
    select(-.chain, -.iteration, -.draw) %>% 
    pivot_longer(everything(), names_to = 'fg', values_to = 'mu') %>% 
    mutate(fg = str_replace_all(fg, 'b_mu', ''),
           var = str_split_fixed(fg, '_', 2)[,2],
           fg = str_split_fixed(fg, '_', 2)[,1]) %>% 
    group_by(fg, var) %>% 
    summarise(med = median(mu), 
              lw = HPDI(mu)[1], hi = HPDI(mu)[2],
              lw50 = HPDI(mu, prob=.5)[1], hi50 = HPDI(mu, prob=.5)[2]) 
  # 
  # # use back-transformation to extract the estimated composition for level 1 (herbivores)
  # fix <- c("herbivore_bare_substrate" = 0,"herbivore_depth"= 0,
  #             "herbivore_hard_coral"= 0,"herbivore_macroalgae"= 0,
  #             "herbivore_rubble"= 0,"herbivore_turf_algae" = 0, 
  #             fixef(fit, robust = TRUE)[, 'Estimate']) %>%  as.data.frame() 
  # colnames(fix) <- c('mu')
  # fix$var<-str_split_fixed(rownames(fix), '_', 2)[,2]
  # fix$fg<-str_split_fixed(rownames(fix), '_', 2)[,1]
  # fix<-fix %>% group_by(var) %>% mutate(all = sum(mu), mu = mu / all)
  
  post<-rbind(post, ndl %>% mutate(nutrient = nut))
  pyramid<-rbind(pyramid, focal %>% clean_names() %>% select(id2, nutrient, country, herbivore_mu, piscivore_mu,invertivore_mobile_mu)) 
  covs<-rbind(covs, pp %>% as.data.frame() %>% mutate(nutrient = nut))
}

post<-post %>% mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                                            'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3'))
post$nutrient_lab<-factor(post$nutrient_lab, levels=c('Calcium', 'Iron', 'Zinc', 'Selenium', 'Vitamin A', 'Omega-3'))

covs<-covs %>% mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                                            'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3'))
covs$nutrient_lab<-factor(covs$nutrient_lab, levels=c('Calcium', 'Iron', 'Zinc', 'Selenium', 'Vitamin A', 'Omega-3'))

pyramid<-pyramid %>% mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                                            'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3'))
pyramid$nutrient_lab<-factor(pyramid$nutrient_lab, levels=c('Calcium', 'Iron', 'Zinc', 'Selenium', 'Vitamin A', 'Omega-3'))

# truncate biomass
foc<-read.csv(paste0('py-notebook/', nut[1], '_unscaled.csv'))
rr<-foc %>% 
  mutate(biomass_kgha = scale(log10(biomass_kgha))) %>% 
  group_by(country) %>% 
  summarise(min = min(biomass_kgha),max = max(biomass_kgha))

post<-left_join(post, rr, by = 'country') %>% filter(fg != 'omnivore') %>% 
  mutate(country = recode(country, 'Solomon Islands' = 'Solomon\nIslands'))

post$biomass_kgha[post$biomass_kgha < post$min]<-NA
post$biomass_kgha[post$biomass_kgha > post$max]<-NA

## original scale biomass
ss<-scale(log10(foc$biomass_kgha))
post$biomass_kgha_org<-10^(post$biomass_kgha * attr(ss, 'scaled:scale') + attr(ss, 'scaled:center')) 

## get post averages by country - nutrient
post_avg<-post %>% group_by(country, fg) %>% 
  summarise(mu = mean(mu)*100) %>% 
  mutate(fg_lab = recode(fg, herbivore = 'Herbivore', invertivore_mobile = 'Invertivore', piscivore = 'Piscivore'))

## get top vs bottom heavy
pys<-pyramid %>% mutate(tb = log(piscivore_mu / herbivore_mu ))
# bot<-seq(0, 1, length.out=100)
# top<-rev(bot)
# plot(top, log(bot/top))

## biomass pyramids
load(paste0('results/mod/biomass_brms.Rdata'))
pypy<-focal %>% clean_names() %>% 
  mutate(tb = log(piscivore_mu / herbivore_mu ), nutrient = 'Standing biomass')
load(paste0('results/mod/productivity_brms.Rdata'))
pypy<-rbind(pypy, focal %>% clean_names() %>% 
              mutate(tb = log(piscivore_mu / herbivore_mu ), nutrient = 'Biomass turnover'))

labber2<-data.frame(lab = c('Standing\nbiomass', 'Biomass\nturnover'),
                    nutrient='Standing biomass',
                   x = c(-1, -3), y = c(2,.6), country='Fiji')
labber3<-data.frame(lab = c('Bottom-heavy', 'Top-heavy'),
                    nutrient='Standing biomass',
                    x = c(-1, 1), y = Inf, country='Belize')
labber4<-data.frame(lab = c('Bottom-heavy', 'Top-heavy'),
                    nutrient_lab='Calcium',
                    x = c(-1, 1), y = Inf, country='Belize')
labber5<-data.frame(lab = c('Calcium', 'Iron', 'Zinc', 'Vitamin A', 'Omega-3', 'Selenium'),
                    nutrient_lab='Calcium',
                    x = c(-1.8, -1, -2.2, 1, 0.6, 0.1), y = c(1.7, 2, 1.3, 1, 1.5, 1.9), country='Belize')

## summary stats

## n reefs with top heavy biomass
pypy %>% group_by(nutrient) %>% summarise(prop_bottom = length(which(tb<0))/length(tb)*100)
pys %>% group_by(nutrient_lab) %>% summarise(prop_bottom = length(which(tb<0))/length(tb)*100)

gr<-ggplot(pypy, aes(tb, fill=nutrient)) + 
  geom_density(alpha=0.5, col='transparent') + 
  facet_wrap(~country, nrow=4, scales='free_y', strip.position = 'right') +
  geom_text(data = labber2, aes(x = x, y = y, label = lab), col=c('black', 'red'), hjust = 0.5, size=2.3) +
  geom_text(data = labber3, aes(x = x, y = y, label = lab), col='black',vjust=0, hjust = 0.5, size=2.3) +
  geom_vline(xintercept = 0, linetype=2, col='black', size=0.4) +
  labs(x = 'log(herbivore/piscivore)', y ='') +
  scale_fill_manual(values = c('black', 'red')) +
  scale_y_continuous(expand=c(0,0),position = 'left') +
  scale_x_continuous(expand=c(0,0), limits=c(-4, 1)) +
  scale_x_log10(expand=c(0,0)) +
  theme(axis.text.x = element_text(), legend.position = 'none',
        axis.text.y = element_text(size=9),
        axis.ticks.x = element_blank(),
        axis.line = element_line(colour='grey'),
        panel.border=element_blank(),
        strip.text.y=element_text(angle=0, hjust=0.5),
        # plot.margin=unit(c(0,-.2,0,-.3), 'cm')
  ) +
  coord_cartesian(clip = "off")

gr2<-ggplot(pys, aes(tb, fill=nutrient_lab)) + 
  geom_density(alpha=0.5, col='transparent') + 
  facet_wrap(~country, nrow=4, scales='free_y', strip.position = 'right') +
  geom_vline(xintercept = 0, linetype=2, col='black', size=0.4) +
  geom_text(data = labber4, aes(x = x, y = y, label = lab), col='black',vjust=0, hjust = 0.5, size=2.3) +
  geom_text(data = labber5, aes(x = x, y = y, label = lab), col=as.character(nut.cols[1:6]),vjust=0, hjust = 0.5, size=2) +
  labs(x = 'log(herbivore/piscivore)', y ='') +
  scale_fill_manual(values = nut.cols) +
  scale_y_continuous(expand=c(0,0),position = 'right') +
  scale_x_continuous(expand=c(0,0), limits=c(-5, 2)) +
  theme(axis.text.x = element_text(), legend.position = 'none',
        axis.text.y = element_text(size=9),
        axis.ticks.x = element_blank(),
        axis.line = element_line(colour='grey'),
        panel.border=element_blank(), 
        strip.text.y=element_blank(),
        plot.margin=unit(c(0,-.2,0,-.3), 'cm')
  ) +
  coord_cartesian(clip = "off")

pdf(file = 'fig/Figure3.pdf', width=7, height = 4)
print(plot_grid(
  gr, gr2, align='h', rel_widths=c(1, 1), axis='tb', labels=c('(a)', '(b)'), label_size=9))
dev.off()


## nutrient prod over biomass gradient
labber<-data.frame(fg = unique(post$fg), lab = c("Invertivore", 'Herbivore', 'Piscivore'), 
                   x = 250, y = c(8, 70, 23), country='Belize', nutrient_lab = 'Calcium')

g1<-ggplot(post, aes(biomass_kgha_org, 100*mu, col=fg, fill=fg)) +
  geom_ribbon(col=NA, alpha=0.5, aes(ymin = 100*lower, ymax = 100*upper)) +
  geom_line() +
  geom_text(data = labber, aes(x = x, y = y, label = lab), hjust = 1, size=2.7) +
  scale_colour_manual(values=colcol) +
  scale_fill_manual(values=colcol) +
  scale_x_log10(breaks=c(30, 100, 500, 5000)) +
  facet_grid(country~nutrient_lab) +
  labs(y = '% nutrient production', x=expression(paste('fishable biomass kg ha'^-1))) +
  theme(strip.text.y=element_text(angle=360, hjust=.5),
        legend.title=element_blank(),
        legend.position = 'none')



pdf(file = 'fig/FigureSX_nutprod_gradients.pdf', width=10.5, height = 5)
# print(plot_grid(
#   g1, gr, align='h', rel_widths=c(1,.2), axis='tb'))
print(g1)
dev.off()

# background panels
rects <- data.frame(ystart = c(1,3,5)-0.5, yend = c(2,4,6)-0.5, med = 0, var = 1)

levs<-c('hard_coral','macroalgae', 'bare_substrate', 'turf_algae','rubble', 'depth')
labs<-c('Hard coral','Macroalgae', 'Bare substrate', 'Turf algae','Rubble','Depth')
covs$var<-factor(covs$var, levels=rev(levs))
covs$fg[covs$fg=='invertivoremobile']<-'invertivore_mobile'

g2<-ggplot(covs, aes(med, var)) +
  geom_rect(data = rects, aes(ymin = ystart, ymax = yend, xmin = -Inf, xmax = Inf), fill = 'grey', alpha = 0.4) +
  geom_vline(xintercept = 0, linetype=5) +
  geom_pointrange(aes(col=fg, xmin = lw, xmax = hi),fatten=0, size=0.4, position = position_dodge(0.75)) +
  geom_pointrange(aes(col=fg, xmin = lw50, xmax = hi50), fatten=1.1, size=1.2, position = position_dodge(0.75)) +
  labs(x = 'posterior value', y = '', col='') +
  scale_colour_manual(values=colcol[-4]) +
  scale_fill_manual(values=colcol[-4]) +
  scale_y_discrete(labels=rev(labs)) +
  facet_grid(~nutrient_lab)

pdf(file = 'fig/FigureSX.pdf', width=10, height = 3)
print(g2)
dev.off()