## Functional group nutrient production by country
source('scripts/0_plot_theme.R')
library(rethinking)
nuts<-c('zinc.mg','calcium.mg','iron.mg','vitamin_a.mug','selenium.mug','omega3.g')

colcol<-trophic_cols.named3[c(1,2,4,6)]
post<-numeric()
covs<-numeric()

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
    summarise(med = median(mu), lw = HPDI(mu)[1], hi = HPDI(mu)[2]) 
  
  post<-rbind(post, ndl %>% mutate(nutrient = nut))
  covs<-rbind(covs, pp %>% as.data.frame() %>% mutate(nutrient = nut))
}

post<-post %>% mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                                            'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3'))
post$nutrient_lab<-factor(post$nutrient_lab, levels=c('Calcium', 'Iron', 'Zinc', 'Selenium', 'Vitamin A', 'Omega-3'))

covs<-covs %>% mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                                            'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3'))
covs$nutrient_lab<-factor(covs$nutrient_lab, levels=c('Calcium', 'Iron', 'Zinc', 'Selenium', 'Vitamin A', 'Omega-3'))

g1<-ggplot(post, aes(biomass_kgha, 100*mu, ymin = 100*lower, ymax = 100*upper, col=fg, fill=fg)) +
  geom_ribbon(col=NA, alpha=0.5) +
  geom_line() +
  scale_colour_manual(values=colcol) +
  scale_fill_manual(values=colcol) +
  facet_grid(country~nutrient_lab) +
  labs(y = 'proportion of assemblage, %', x='log10(biomass) kg ha') +
  theme(strip.text.y=element_text(angle=360),
        legend.title=element_blank())

# background panels
rects <- data.frame(ystart = c(1,3,5)-0.5, yend = c(2,4,6)-0.5, med = 0, var = 1)

levs<-c('hard_coral','macroalgae', 'bare_substrate', 'turf_algae','rubble', 'depth')
labs<-c('Hard coral','Macroalgae', 'Bare substrate', 'Turf algae','Rubble','Depth')
covs$var<-factor(covs$var, levels=rev(levs))
covs$fg[covs$fg=='invertivoremobile']<-'invertivore_mobile'

g2<-ggplot(covs, aes(med, var)) +
  geom_rect(data = rects, aes(ymin = ystart, ymax = yend, xmin = -Inf, xmax = Inf), fill = 'grey', alpha = 0.4) +
  geom_vline(xintercept = 0, linetype=5) +
  geom_pointrange(aes(col=fg, xmin = lw, xmax = hi), position = position_dodge(0.5)) +
  labs(x = 'posterior value', y = '', col='') +
  scale_colour_manual(values=colcol) +
  scale_fill_manual(values=colcol) +
  scale_y_discrete(labels=rev(labs)) +
  facet_grid(~nutrient_lab)


pdf(file = 'fig/Figure3.pdf', width=11, height = 5)
print(g1)
dev.off()

pdf(file = 'fig/FigureSX.pdf', width=10, height = 4)
print(g2)
dev.off()