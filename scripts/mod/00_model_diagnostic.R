
load(file = paste0('results/mods/', nut,'_', dp, '_model.Rdata'))
## pred vs. obs
pdf(file = paste0('results/betam_posteriors/', nut,'/post_obs_', dp, '.pdf'), height=7, width=12)
print(plot(predict(fit1)[,1], focal.scaled$nutprop, xlim=c(0,1)))
abline(0,1)

print(ppc_dens_overlay(y = focal.scaled$nutprop,
                       yrep = posterior_predict(fit1, nsamples = 100)))
dev.off()

# pareto diagnostic
loo(fit1)

## PDF the posterior effects
pdf(file = paste0('results/betam_posteriors/', nut,'/post_summary_', dp, '.pdf'), height=7, width=12)
print(
  tidy(fit1, effects = "fixed") %>% filter(component == 'cond' & !str_detect(term, 'phi|Intercept')) %>% 
    ggplot(aes(term, estimate, ymin = conf.low, ymax = conf.high)) + 
    geom_hline(yintercept = 0, col='grey', linetype=5) +
    geom_pointrange() + 
    coord_flip() +
    labs(subtitle = paste(nut, dp))
)
dev.off()

pdf(file = paste0('results/betam_posteriors/', nut,'/management_', dp, '.pdf'), height=7, width=12)
print(plot(fit1, pars = 'b_management|b_grav_nc|b_pop_count'))
dev.off()

pdf(file = paste0('results/betam_posteriors/', nut,'/benthic_', dp, '.pdf'), height=7, width=12)
print(plot(fit1, pars = 'b_turf_algae|b_macroalgae|b_hard_coral|b_bare|b_depth'))
dev.off()

