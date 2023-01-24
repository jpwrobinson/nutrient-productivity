## Functional group nutrient production by country
source('scripts/0_plot_theme.R')
library(rethinking)
library(brms)
library(ggradar)
nuts<-c('zinc.mg','calcium.mg','iron.mg','vitamin_a.mug','selenium.mug','omega3.g')

colcol<-trophic_cols.named3[c(1,2,4,6)]
ranef<-numeric()

for(i in 1:length(nuts)){
  nut<-nuts[i]
  load(paste0('results/mod/', nut, '_brms.Rdata'))
  
  ## ranef
  rans<-posterior_summary(fit) %>% as.data.frame()
  rans$var <- rownames(rans)
  rownames(rans)<-NULL
  
  # tidy names
  rans<-rans %>% 
    filter(str_detect(var, 'r_country*|r_management*')) %>% 
    filter(!str_detect(var, 'cor_country')) %>% 
    mutate(cat = ifelse(str_detect(var, 'r_country'), 'Country', 'Management rules')) %>% 
    mutate(var2 = str_replace_all(var, 'r_', ''),
           var2 = str_replace_all(var2, 'mu', ''),
           fg = str_split_fixed(var2, '__', 2)[,2],
           var2 = str_split_fixed(var2, '__', 2)[,1],
           covariate_type = str_split_fixed(fg, '\\[', 2)[,2],
           fg = str_split_fixed(fg, '\\[', 2)[,1],
           level = str_split_fixed(covariate_type, '\\,', 2)[,1],
           covariate_type = str_split_fixed(covariate_type, '\\,', 2)[,2],
           covariate_type = str_replace_all(covariate_type, '\\]', '')) %>% select(-var2, -var)
  
  ranef<-rbind(ranef, rans %>% as.data.frame() %>% mutate(nutrient = nut))
}

ranef<-ranef %>% mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                                            'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3'))
ranef$nutrient_lab<-factor(ranef$nutrient_lab, levels=c('Calcium', 'Iron', 'Zinc', 'Selenium', 'Vitamin A', 'Omega-3'))

ranef<-ranef %>% 
  mutate(fg = recode(fg, 'invertivoremobile' = 'invertivore_mobile'),
         level = recode(level, 'Solomon.Islands' = 'Solomon Islands'))


g1<-ggplot(ranef %>% filter(covariate_type=='Intercept'),
       aes(level, Estimate, col = fg, ymin = Q2.5, ymax = Q97.5)) + 
  geom_pointrange(position = position_dodge(width=0.5)) +
  coord_flip() +
  geom_hline(yintercept= 0, linetype = 5) +
  scale_colour_manual(values=colcol) +
  facet_grid(~nutrient_lab) +
  labs(x = '', y = 'Posterior median ± 95% HPDI') +
  theme(legend.title = element_blank())


pdf(file = 'fig/FigureSX_manage_covs.pdf', width=10, height = 6)
print(g1)
dev.off()
