
load(file = paste0('results/mods/', nut,'_', dp, '_model.Rdata'))
## pred vs. obs
pdf(file = paste0('results/betam_posteriors/', nut,'/post_obs_', dp, '.pdf'), height=7, width=12)
print(plot(predict(fit1)[,1], focal.scaled$nutprop, xlim=c(0,1)))
abline(0,1)

## check posterior predictive with observed
yrep1 <- posterior_predict(fit1, nsamples = 100)
yrep2 <- posterior_predict(fit1)

print(
  ppc_dens_overlay(y = focal.scaled$nutprop,
                       yrep = yrep1) + th + theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=14))
  )

# check accuracy of median posterior prediction

print(
  ppc_stat(focal.scaled$nutprop, yrep1, stat = 'median')
  )

## check LOO - where does each y fall in its predictive distribution
loo1 <- loo(log_lik(fit1), save_psis = TRUE)
print(
  ppc_loo_pit_overlay(yrep = yrep2, y = focal.scaled$nutprop, lw = weights(loo1$psis_object, 100)) + ggtitle("LOO-PIT Model 1")
)

dev.off()

# pareto diagnostic
# loo(fit1)

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
print(plot(fit1, pars = 'b_management'))
dev.off()

pdf(file = paste0('results/betam_posteriors/', nut,'/benthic_', dp, '.pdf'), height=7, width=12)
print(plot(fit1, pars = 'b_turf_algae|b_macroalgae|b_hard_coral|b_bare|b_depth'))
dev.off()

