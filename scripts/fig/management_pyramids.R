

## biomass pyramids
load(paste0('results/mod/biomass_brms.Rdata'))
pypy<-focal %>% clean_names() %>% 
  mutate(tb = log(piscivore_mu / herbivore_mu ), nutrient = 'Standing biomass')
load(paste0('results/mod/productivity_brms.Rdata'))
pypy<-rbind(pypy, focal %>% clean_names() %>% 
              mutate(tb = log(piscivore_mu / herbivore_mu ), nutrient = 'Biomass turnover'))

g1<-ggplot(pypy, aes(tb, fill=management_rules)) + 
  geom_density(alpha=0.5) + facet_grid(country ~ nutrient) +
  scale_x_continuous(limits=c(-4, 1)) +
  labs(x = 'log(piscivore/herbivore)', y ='')  +
  guides(fill = 'none')

pys$management_rules<-pypy$management_rules[match(pys$id2, pypy$id2)]
g2<-ggplot(pys, aes(tb, fill=management_rules)) + 
  geom_density(alpha=0.5) + facet_grid(country ~ nutrient) +
  scale_x_continuous(limits=c(-4, 3)) +
  labs(x = 'log(piscivore/herbivore)', y ='') 



pdf(file = 'fig/explore/management_pyramids.pdf', height = 7, width=15)
print(plot_grid(
  g1, g2, align='h', rel_widths=c(.7, 1), axis='tb', labels=c('(a)', '(b)'), label_size=9))
dev.off()