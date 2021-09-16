pacman::p_load(here, funk, knitr, tidyverse, skimr, cowplot, readxl, gghighlight, ggrepel)


theme_set(theme_sleek())

th<-theme(axis.text.y=element_text(size=10, colour='black'),
          axis.text.x=element_text(size=10, colour='black'),
          axis.line = element_line(colour = "grey"), 
          strip.text = element_text(face="bold", colour='black', size=10),
          legend.position =c(0.8, 0.6), 
          legend.title=element_blank(),
          axis.ticks=element_blank())


pd<-position_dodge(width=0.4)

## colour vectors

nut.cols<-c('Calcium'='#de2d26', 'Iron'='#636363', 'Zinc'='#3182bd', 'Vitamin A'='#31a354',
            'Omega-3' = '#F77D29', 'Selenium' = '#776EB0')

