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
tax <- load_taxa() 

#Import species list here
# sp <- read.csv("data/trait/all_species.csv")
sp<-mermaid_get_reference(reference = c("fishspecies", 'fishgenera', 'fishfamilies'))

# species_list <- sp$species
species_list <- sort(sp$fishspecies$species) # 3,293 species
sp_data <- getTaxo(sp = species_list , tax = tax)

#Test Lmax
lmax <- getLmax(sp_data)
head(lmax)
summary(lmax)

########################################
#Testing area for Reef position with Seychelles data
sey <- read.csv("/Users/maire/Downloads/Species traits_Seychelles_exactMaxSizeTL.csv")

species_list <- sey$species

#Load GASPAR database
gaspar <- read.csv("data/trait/gaspar.csv")

#Test MainFood
Diet <- read.xlsx("data/trait/SAU data/FeedingPathway_Fishbase.xlsx",sheetName = "Diet")
Food <- read.xlsx("data/trait/FeedingPathway_Fishbase.xlsx",sheetName = "Food Items")

mainfood <- getMainFood(sp_data,Diet, Food)
mainfood

#Test Demersal versus Pelagic
dem <- getDemersPelag(sp_data)
dem

#Test biology
vert <- getVerticalPosition(sp_data,gaspar)
vert

#END



