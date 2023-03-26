
nn<-numeric()

for(i in 1:length(nuts)){
  load(file  = paste0('results/mod/', nuts[i], 'benthic_grad_post.Rdata'))
  ndl_all$nutrient <- nuts[i]
  nn<-rbind(nn, ndl_all)
}


ggplot(nn, aes(focal_x, mu, fill=fg)) + 
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.5) +
    geom_line(aes(col=fg)) +
    facet_wrap(focal~nutrient) 

