library(tidyverse)
source('scripts/0_plot_theme.R')

conts<-c('hard_coral', 'bare_sub', 'gravity', 'turf', 'population', 'macroalgae', 'sediment', 'nut_load')

pBiom<-read.csv('py-notebook/biomass/posterior_summary.csv') %>% mutate(service = 'Biomass')
pProd<-read.csv('py-notebook/prod/posterior_summary.csv') %>% mutate(service = 'Productivity')
pCa<-read.csv('py-notebook/turnover/calcium_posterior_summary.csv') %>% mutate(service = 'Calcium turnover')
pFe<-read.csv('py-notebook/turnover/iron_posterior_summary.csv') %>% mutate(service = 'Iron turnover')
pZn<-read.csv('py-notebook/turnover/zinc_posterior_summary.csv') %>% mutate(service = 'Zinc turnover')
pDens<-read.csv('py-notebook/density/posterior_summary.csv') %>% mutate(service = 'Nutrient density')

# join posts
posts<-rbind(pBiom, pProd, pCa, pFe, pZn, pDens) %>% 
  mutate(var = str_split_fixed(X, '\\[', 2)[,1]) %>% 
  # filter(!var %in% c('intercept', 'alpha', 'β0_cnc', 'β0_managenc')) %>%
  filter(var %in% conts) %>%
  mutate(varname = str_split_fixed(varname, '\\[', 2)[,1],
         varnum=as.numeric(factor(var)),
         fill = ifelse(hdi_2.5. > 0 & hdi_97.5. > 0, 'darkgreen', 'grey'),
         fill = ifelse(hdi_2.5. < 0 & hdi_97.5. < 0, 'darkred', fill))

levs<-c('hard_coral','macroalgae', 'bare_sub', 'turf',  'gravity', 'population', 'sediment', 'nut_load')
labs<-c('Hard coral','Macroalgae', 'Bare substrate', 'Turf algae',  'Market gravity', 'Human population', 'Sedimentation', 'Nutrient load')
posts$varname<-factor(posts$varname, levels=rev(levs))

posts$service<-factor(posts$service, levels = c('Biomass', 'Productivity', 'Calcium turnover', 'Iron turnover', 'Zinc turnover', 'Nutrient density'))

g1<-ggplot(posts, aes(mean, varname, xmin = hdi_2.5., xmax = hdi_97.5., fill=fill)) + 
      geom_vline(xintercept = 0, linetype=5) +
      geom_pointrange(pch=21, fatten=8) + 
      scale_fill_identity() +
      scale_y_discrete(labels=rev(labs)) +
      labs(x = 'posterior', y = '') +
      facet_grid(~service, scales='free') +
      theme(panel.grid.minor.x = element_line(colour='grey')) +
      th 

pdf(file = 'fig/FigureSX_posterior_services.pdf', height =7, width=12)
print(g1)
dev.off()
