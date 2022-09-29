
source('scripts/py-output/03_nutprod_post_gradients_fg.R')


## Figure 3
bens<-c('hard_coral','macroalgae','bare_substrate','turf_algae','rubble')

pdf(file = 'fig/Figure3.pdf', height=9, width = 7.5)

g0<-ggplot(biom_main %>% filter(cov %in% bens), 
		aes(X_raw, prop, fill=fg)) + 
      # geom_ribbon(alpha=0.2) +
      geom_line(size=0.8, aes(col=fg)) + 
      labs(x = '% cover', y = 'proportion community, %', subtitle = 'Standing biomass') +
      scale_colour_manual(values = trophic_cols.named2) +
      facet_wrap(~cov, nrow=1, scales='free_x') +
      scale_y_continuous(expand=c(0,0), limits=c(0,.62), breaks =seq(0, .5, by = .1), labels=seq(0, 50, by = 10)) +
      theme(legend.position = 'none')

g1<-ggplot(prod_main %>% filter(cov %in% bens), 
		aes(X_raw, prop, fill=fg)) + 
      # geom_ribbon(alpha=0.2) +
      geom_line(size=0.8, aes(col=fg)) + 
      labs(x = '% cover', y = 'proportion community, %', subtitle = 'Biomass production') +
      scale_colour_manual(values = trophic_cols.named2) +
      facet_wrap(~cov, nrow=1, scales='free_x') +
      scale_y_continuous(expand=c(0,0), limits=c(0,.62), breaks =seq(0, .5, by = .1), labels=seq(0, 50, by = 10)) +
      theme(legend.position = 'none')

g2<-ggplot(nuts %>% filter(cov %in% bens), 
		aes(X_raw, prop, fill=fg)) + 
      # geom_ribbon(alpha=0.2) +
      geom_line(size=0.8, aes(col=fg)) + 
      labs(x = '% cover', y = 'proportion community, %', subtitle = 'Nutrient production (Ca, Fe, Zn)') +
      scale_colour_manual(values = trophic_cols.named2) +
      facet_wrap(~cov, nrow=1, scales='free_x') +
      scale_y_continuous(expand=c(0,0), limits=c(0,.62), breaks =seq(0, .5, by = .1), labels=seq(0, 50, by = 10)) +
      theme(legend.position = 'none') 


print(
	plot_grid(g0, g1, g2, nrow = 3 , labels = c('A', 'B', 'C'))
	)

dev.off()

## Sup fig - anthro drivers

pdf(file = 'fig/FigureSX_anthro_drivers.pdf', height=9, width = 7.5)

g0S<-ggplot(biom_main %>% filter(!cov %in% bens), 
		aes(X_raw, prop, fill=fg)) + 
      # geom_ribbon(alpha=0.2) +
      geom_line(size=0.8, aes(col=fg)) + 
      labs(x = '', y = 'proportion community, %', subtitle = 'Standing biomass') +
      scale_colour_manual(values = trophic_cols.named2) +
      facet_wrap(~cov, nrow=1, scales='free', strip.position = 'bottom') +
      scale_y_continuous(expand=c(0,0), limits=c(0,.62), breaks =seq(0, .5, by = .1), labels=seq(0, 50, by = 10)) +
      theme(legend.position = 'none', strip.placement='outside')

g1S<-ggplot(prod_main %>% filter(!cov %in% bens), 
		aes(X_raw, prop, fill=fg)) + 
      # geom_ribbon(alpha=0.2) +
      geom_line(size=0.8, aes(col=fg)) + 
      labs(x = '', y = 'proportion community, %', subtitle = 'Biomass production') +
      scale_colour_manual(values = trophic_cols.named2) +
      facet_wrap(~cov, nrow=1, scales='free', strip.position = 'bottom') +
      scale_y_continuous(expand=c(0,0), limits=c(0,.62), breaks =seq(0, .5, by = .1), labels=seq(0, 50, by = 10)) +
      theme(legend.position = 'none', strip.placement='outside')

g2S<-ggplot(nuts %>% filter(!cov %in% bens), 
		aes(X_raw, prop, fill=fg)) + 
      # geom_ribbon(alpha=0.2) +
      geom_line(size=0.8, aes(col=fg)) + 
      labs(x = '', y = 'proportion community, %', subtitle = 'Nutrient production (Ca, Fe, Zn)') +
      scale_colour_manual(values = trophic_cols.named2) +
      facet_wrap(~cov, nrow=1, scales='free', strip.position = 'bottom') +
      scale_y_continuous(expand=c(0,0), limits=c(0,.62), breaks =seq(0, .5, by = .1), labels=seq(0, 50, by = 10)) +
      theme(legend.position = 'none', strip.placement='outside') 

print(
	plot_grid(g0S, g1S, g2S, nrow = 3, labels = c('A', 'B', 'C'))
	)

dev.off()