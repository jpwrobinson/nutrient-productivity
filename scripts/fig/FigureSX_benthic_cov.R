library(tidyverse)
library(brms)

ben.cols<-data.frame(col = c('#498FC9','#9CE5FA','#B6B400','#a1d99b'),
                     benthic = c('hard_coral', 'bare_substrate', 'macroalgae', 'turf_algae'),
                     benthic_lab = c('Hard coral', 'Bare substrate', 'Macroalgae', 'Turf algae'))

ben_cols.named<-setNames(as.character(ben.cols$col), ben.cols$benthic)

load('data/wcs/wcs_fish_benthic.rds')

ben<-fish_avg %>% filter(country %in% c('Belize', 'Fiji', 'Madagascar', 'Solomon Islands')) %>% 
      ungroup() %>% 
      select(site, country, hard_coral, macroalgae, bare_substrate, turf_algae) %>% 
      pivot_longer(-c(site, country, hard_coral), names_to = 'sub', values_to = 'cover') %>% 
      mutate(id = paste0(country, sub)) %>% 
      filter(site != 'NK02') %>% 
      mutate(cover = ifelse(cover == 0, 0.001, cover))

# Determine posterior slopes for each country - benthic cat
subs<-unique(ben$sub)
slopes<-numeric()
intercepts<-numeric()

# fitting gamma-GLM with hard coral slope by country, extracting posterior summary for slope
for(i in 1:length(subs)){
  foc<-ben %>% filter(sub ==subs[i]) %>% mutate(hard_coral_scale = scale(hard_coral))
  brm_glm_reg <- brm(cover~(1 + hard_coral_scale | country), 
                     data = foc, 
                     family=Gamma(link="log"),
                     chains=2,iter=1000, cores=4)
  
  plot((brm_glm_reg))
  slope<-as.data.frame(coef(brm_glm_reg)$country)[,c(5,7,8)] %>% mutate(sub = subs[i])
  slope$country<-rownames(slope)
  rownames(slope)<-NULL
  slopes<-rbind(slopes, slope)
  
  intercept<-as.data.frame(coef(brm_glm_reg)$country)[,c(1,3,4)] %>% mutate(sub = subs[i])
  intercept$country<-rownames(intercept)
  rownames(intercept)<-NULL
  intercepts<-rbind(intercepts, intercept)
}

coef(brm_glm_reg)
## 
colnames(slopes)<-str_replace_all(colnames(slopes), '.hard_coral_scale', '')
colnames(intercepts)<-str_replace_all(colnames(intercepts), '.hard_coral_scale', '')

g1<-ggplot(slopes, aes(fct_rev(country), Estimate, ymin = Q2.5, ymax = Q97.5)) +
          geom_hline(yintercept=0, col='grey', linetype=5) +
          geom_pointrange(fatten=0, aes(colour=sub), position = position_dodge(width=0.5)) + 
          geom_point(size = 2.5, pch=21, colour='black', aes( fill=sub), position = position_dodge(width=0.5)) +
          coord_flip() + 
          theme(legend.position = 'none') +
          scale_fill_manual(values = ben_cols.named)  +
          scale_color_manual(values = ben_cols.named)  +
          labs(y = 'Slope with hard coral cover', x = '') 

g2<-ggplot(ben, aes(hard_coral, cover)) + 
  geom_point(alpha=0.8, size=1.5, aes(col=sub)) + 
  facet_grid(country~sub, scales='free') +
  scale_y_continuous(limits=c(0, NA)) +
  scale_colour_manual(values = ben_cols.named) +
  theme(legend.position = 'none') +
  labs(x = 'hard coral cover, %', y = 'benthic cover, %')

pdf(file = 'fig/FigureSX_benthic_cov.pdf', height=5, width=9)
print(cowplot::plot_grid(g2, g1, nrow=1, rel_widths=c(1,0.6), labels=c('a', 'b')))
dev.off()