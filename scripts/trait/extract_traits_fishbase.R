#Extract traits (Lmax and data for reef position) from FishBase

#Load libraries
require(rfishbase)
require(rvest)
require(stringr)
require(xml2)
require(tidyverse)
require(xlsx)

#Load functions 
source("scripts/trait/traits_from_fishbase_functions.R")

#Load GASPAR database
gaspar <- read.csv("data/trait/gaspar.csv")

#Get fish Taxonomy from Fishbase 
tax <- load_taxa() 

#Import species list
#example with Seychelles' list
sey <- read.csv("/Users/maire/Downloads/Species traits_Seychelles_exactMaxSizeTL.csv")
species_list <- sey$species
sp_data <- getTaxo(sp = species_list , tax = tax)

#Test Lmax
#lmax <- getLmax(sp_data)
#lmax

#Test MainFood
#mainfood <- getMainFood(sp_data,Diet, Food)
#mainfood

#Test Demersal versus Pelagic
#dem <- getDemersPelag(sp_data)
#dem

#Test biology
vert <- getVerticalPosition(sp_data,gaspar)
vert

#



