#Extract traits (Lmax and data for reef position) from FishBase

#Load libraries
require(rfishbase)
require(rvest)
require(stringr)
require(xml2)
require(tidyverse)
require(xlsx)

library(mermaidr)

#Load functions 
source("scripts/trait/traits_from_fishbase_functions.R")

#Get fish Taxonomy from Fishbase 
tax <- load_taxa() %>% 
        filter(!is.na(SpecCode)) %>% 
        filter(!SpecCode %in% c('<p>', '', '.')) %>% 
        collect()
tax<-tax[!duplicated(tax),]

## dupes in SpecCode cause problems - duplicated SpecCodes can have NA for taxonomic fields
dupe<-tax %>% as.data.frame() %>% janitor::get_dupes(SpecCode) 

# count the NAs for each dupe, then select the dupe with fewest NAs (ie most taxa info)
dupe$NAsum<-rowSums(is.na(dupe[,3:9]))
duped<-dupe %>% group_by(SpecCode) %>% slice(which.min(NAsum)) %>% select(-dupe_count, -NAsum)

## remove dupes in tax and replace with the single, most informative value from duped
tax<-tax %>% filter(!SpecCode %in% dupe$SpecCode) %>% rbind(duped) %>% 
      mutate(Species_corrected = Species)

#Import species list here
# sp <- read.csv("data/trait/all_species.csv")
sp<-mermaid_get_reference(reference = c("fishspecies", 'fishgenera', 'fishfamilies'))

# species_list <- sp$species
species_list <- sort(sp$fishspecies$display) # 3,293 species

# validate names
sp_data <- getTaxo(sp = species_list , tax = tax)
sp_data<- sp_data[!duplicated(sp_data),]
save(sp_data, file = 'data/trait/wcs_sp_data.rds')

sp_data %>% filter(is.na(SpecCode))



#Get Lmax
# fix a species_corrected NA (Upeneus indicus)
sp_data$Species_corrected[is.na(sp_data$Species_corrected)]<-sp_data$Species[is.na(sp_data$Species_corrected)]
lmax <- getLmax(sp_data)


## get Diet (Parravicini et al. 2020, PLoS ONE)
diet<-read.csv('data/trait/parravicini_trophic_guilds_2020.csv') %>% janitor::clean_names() %>% 
      mutate(species = str_replace_all(species, '_', '\\ '))

lmax$diet<-diet$trophic_guild_predicted_text[match(lmax$Species_corrected, diet$species)]

#rename and save
trait<-lmax
save(trait, file = 'data/trait/wcs_sp_lmax_diet.rds')

#END


########################################
# #Testing area for Reef position with Seychelles data
# sey <- read.csv("/Users/maire/Downloads/Species traits_Seychelles_exactMaxSizeTL.csv")
# 
# species_list <- sey$species
# 
# #Load GASPAR database
# gaspar <- read.csv("data/trait/gaspar.csv")
# 
# #Test MainFood
# Diet <- read.xlsx("data/trait/SAU data/FeedingPathway_Fishbase.xlsx",sheetName = "Diet")
# Food <- read.xlsx("data/trait/FeedingPathway_Fishbase.xlsx",sheetName = "Food Items")
# 
# mainfood <- getMainFood(sp_data,Diet, Food)
# mainfood
# 
# #Test Demersal versus Pelagic
# dem <- getDemersPelag(sp_data)
# dem
# 
# #Test biology
# vert <- getVerticalPosition(sp_data,gaspar)
# vert
