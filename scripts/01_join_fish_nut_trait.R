pacman::p_load(tidyverse, skimr, cowplot, here, funk,disco, patchwork, mermaidr, mermaidreporting, install=FALSE)

# load nutrient + trait data
load('data/WCS_nutrient_profiles.rds')
load(file = 'data/trait/wcs_sp_lmax_diet.rds')

## merge fish with nutrients and check coverage in each country
fish<-read.csv(file='data/wcs/fish_individuals.csv')
uniques(fish$fish_taxon[fish$fish_taxon %in% nut$Species_corrected]) # 508 in nut
uniques(fish$fish_taxon[!fish$fish_taxon %in% nut$Species_corrected]) # 53 missing
print(paste(round(53/508, 2) * 100, '% species missing nutrient preds'))

## add trophic group for Parapercis tetracantha
fish$trophic_group[fish$fish_taxon == 'Parapercis tetracantha']<-'invertivore-mobile'
fish$functional_group[fish$fish_taxon == 'Parapercis tetracantha']<-'micro-invertivore'

# check biomass of missing species
biom<-fish %>% group_by(fish_taxon) %>% summarise(b = sum(biomass_kgha))
tot<-sum(biom$b, na.rm=TRUE)
sum(biom$b[!biom$fish_taxon %in% nut$Species_corrected])/ tot * 100  ## 7.7% biomass missing species-level nutrients

# key (missing) species
biom[!biom$fish_taxon %in% nut$Species_corrected,] %>% slice_max(order_by = b, n = 10)

## now match

## 1. nutrients
fish$calcium.mg<-nut$Calcium_mu[match(fish$fish_taxon,nut$Species_corrected)]
fish$iron.mg<-nut$Iron_mu[match(fish$fish_taxon,nut$Species_corrected)]
fish$selenium.mug<-nut$Selenium_mu[match(fish$fish_taxon,nut$Species_corrected)]
fish$zinc.mg<-nut$Zinc_mu[match(fish$fish_taxon,nut$Species_corrected)]
fish$omega3.g<-nut$Omega_3_mu[match(fish$fish_taxon,nut$Species_corrected)]
fish$vitamin_a.mug<-nut$Vitamin_A_mu[match(fish$fish_taxon,nut$Species_corrected)]

##  anything missing gets genus mean
fish$calcium.mg[is.na(fish$calcium.mg)]<-genus$Calcium_mu[match(fish$fish_genus[is.na(fish$calcium.mg)],genus$Genus)]
fish$iron.mg[is.na(fish$iron.mg)]<-genus$Iron_mu[match(fish$fish_genus[is.na(fish$iron.mg)],genus$Genus)]
fish$selenium.mug[is.na(fish$selenium.mug)]<-genus$Selenium_mu[match(fish$fish_genus[is.na(fish$selenium.mug)],genus$Genus)]
fish$zinc.mg[is.na(fish$zinc.mg)]<-genus$Zinc_mu[match(fish$fish_genus[is.na(fish$zinc.mg)],genus$Genus)]
fish$omega3.g[is.na(fish$omega3.g)]<-genus$Omega_3_mu[match(fish$fish_genus[is.na(fish$omega3.g)],genus$Genus)]
fish$vitamin_a.mug[is.na(fish$vitamin_a.mug)]<-genus$Vitamin_A_mu[match(fish$fish_genus[is.na(fish$vitamin_a.mug)],genus$Genus)]

##  anything missing gets family mean
fish$calcium.mg[is.na(fish$calcium.mg)]<-fam$Calcium_mu[match(fish$fish_family[is.na(fish$calcium.mg)],fam$Family)]
fish$iron.mg[is.na(fish$iron.mg)]<-fam$Iron_mu[match(fish$fish_family[is.na(fish$iron.mg)],fam$Family)]
fish$selenium.mug[is.na(fish$selenium.mug)]<-fam$Selenium_mu[match(fish$fish_family[is.na(fish$selenium.mug)],fam$Family)]
fish$zinc.mg[is.na(fish$zinc.mg)]<-fam$Zinc_mu[match(fish$fish_family[is.na(fish$zinc.mg)],fam$Family)]
fish$omega3.g[is.na(fish$omega3.g)]<-fam$Omega_3_mu[match(fish$fish_family[is.na(fish$omega3.g)],fam$Family)]
fish$vitamin_a.mug[is.na(fish$vitamin_a.mug)]<-fam$Vitamin_A_mu[match(fish$fish_family[is.na(fish$vitamin_a.mug)],fam$Family)]

## 2. Lmax + trophic group
fish$lmax<-trait$lmax[match(fish$fish_taxon, trait$Species)]
fish$diet<-trait$diet[match(fish$fish_taxon, trait$Species)]


## What is missing?

# 1. Nutrients
unique(fish$fish_taxon[is.na(fish$calcium.mg)]) ## 76 species missing nutrients
sum(fish$biomass_kgha[is.na(fish$calcium.mg)]) ## 170,210 kg biomass 
sum(fish$biomass_kgha[is.na(fish$calcium.mg)])/tot*100 ## 13% biomass 

# 2. Lmax
unique(fish$fish_taxon[is.na(fish$lmax)]) ## families missing Lmax
sum(fish$biomass_kgha[is.na(fish$lmax)]) ## 90,766 kg biomass 
sum(fish$biomass_kgha[is.na(fish$lmax)])/tot*100 ## 7% biomass 

# 2. Diet
unique(fish$fish_taxon[is.na(fish$diet)]) ## 75 species missing Diet
sum(fish$biomass_kgha[is.na(fish$diet)]) ## 
sum(fish$biomass_kgha[is.na(fish$diet)])/tot*100 ## 


drops<-unique(fish$fish_taxon[is.na(fish$calcium.mg)])
## drop these species (no genus info)
fish<-fish %>% filter(!(fish_taxon %in% drops))

## estimate nutrient score
## rda for nut density
# https://www.nationalacademies.org/our-work/summary-report-of-the-dietary-reference-intakes
## calculating average for 6 months - <5 years (4.5 years)
source('scripts/rda_reader.R')
rda$nutrient<-rda$nutrient2
fish<-fish %>% left_join(rda %>% select(nutrient, rda_kids), by = 'nutrient')

fish$ca_rda<-fish$calcium.mg/ca*100
fish$fe_rda<-fish$iron.mg/fe*100
fish$se_rda<-fish$selenium.mug/se*100
fish$zn_rda<-fish$zinc.mg/zn*100
fish$vita_rda<-fish$vitamin_a.mug/vita*100
fish$nscore<-with(fish, ca_rda + fe_rda + se_rda + zn_rda + vita_rda)

write.csv(fish, file = 'data/wcs/wcs_nutrients_individuals.csv')