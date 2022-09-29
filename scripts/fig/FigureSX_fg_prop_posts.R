library(tidyverse)
source('scripts/0_plot_theme.R')

conts<-c('hard_coral', 'bare_sub', 'gravity', 'turf', 'population', 'macroalgae', 'sediment', 'nut_load')


## read posts by model
filenames=c('/fg/prod/', '/fg/biom/', '/fg/calcium/', '/fg/iron/', '/fg/zinc/')
var_name<-str_replace_all(filenames, '/fg/', '')
var_name<-str_replace_all(var_name, '/', '')
posts<-numeric()
for(i in 1:length(filenames)){
    post<-read.csv(paste0('py-notebook', filenames[i],var_name[i], '_posterior_summary.csv')) %>% 
            mutate(mod = var_name[i])
    posts<-rbind(posts, post)
}

# join posts
posts<-posts %>% 
  mutate(var = str_split_fixed(X, '\\[', 2)[,1]) %>% 
  # filter(!var %in% c('intercept', 'alpha', 'β0_cnc', 'β0_managenc')) %>%
  filter(var %in% conts) %>%
  mutate(varname = str_split_fixed(varname, '\\[', 2)[,1],
         varnum=as.numeric(factor(var)),
         fill = ifelse(hdi_2.5. > 0 & hdi_97.5. > 0, 'darkgreen', 'grey'),
         fill = ifelse(hdi_2.5. < 0 & hdi_97.5. < 0, 'darkred', fill)) %>% 
  mutate(mod = recode(mod, prod = 'Biomass production', biom = 'Standing biomass', calcium = 'Calcium production', 
                    iron = 'Iron production', zinc = 'Zinc production'))

levs<-c('hard_coral','macroalgae', 'bare_sub', 'turf',  'gravity', 'population', 'sediment', 'nut_load')
labs<-c('Hard coral','Macroalgae', 'Bare substrate', 'Turf algae',  'Market gravity', 'Human population', 'Sedimentation', 'Nutrient load')
posts$varname<-factor(posts$varname, levels=rev(levs))

# unique(posts$mod)
posts$mod<-factor(posts$mod, levels = unique(posts$mod)[c(2,1,3,4,5)])

# background panels
rects <- data.frame(ystart = c(1,3,5,7)-0.5, yend = c(2,4,6,8)-0.5, mean = 0, varname = 1)

g1<-ggplot(posts, aes(mean, varname, xmin = hdi_2.5., xmax = hdi_97.5., fill = fg)) + 
      geom_rect(data = rects, aes(ymin = ystart, ymax = yend, xmin = -Inf, xmax = Inf), fill = 'grey', alpha = 0.4) +
      geom_vline(xintercept = 0, linetype=5) +
      geom_pointrange(pch=21, fatten=5.5, aes(group=fg), position = position_dodge(width=.8)) + 
      # scale_fill_identity() +
      scale_fill_manual(values = trophic_cols.named2) +
      scale_y_discrete(labels=rev(labs)) +
      labs(x = 'posterior effect size', y = '') +
      facet_grid(~mod, scales='free') +
      th +
      theme(panel.grid.minor.x = element_line(colour='grey'), legend.position = 'right') 

pdf(file = 'fig/FigureSX_fg_effect_sizes.pdf', height =8, width=11)
print(g1)
dev.off()
