## Data prep for nutrient productivity calcs##

# for comparison between calculation methods. Run on Madagascar data, using known species traits from Seychelles

# Read in WCS Madagascar data:
mada <- read.csv("data/wcs/madagascar_individuals.csv", header=T)
head(mada)
str(mada)

#Remove first column (row numbers):
mada <- mada[,-c(1)]


summary(unique(mada$fish_taxon))
# 356 species

# Read in Seychelles data:
sey.fish <- read.csv("data/SEY_UVC_fish_1994-2017.csv", header=T)
str(sey.fish)

summary(unique(sey.fish$species))
# 130 species


#create object with all Seychelles UVC fish species:
SEY.sp <- unique(sey.fish$species)
SEY.sp

#determine which species from SEY.sp are in mada dataframe:
mada$SEY_UVC <- sapply(mada$fish_taxon, function (x){
  if (x %in% SEY.sp)  return("TRUE")
  else F
}) 
head(mada,50)

#Create new dataset from mada, containing only species present in Seychelles UVC:
sey.mada <- mada[which(mada$SEY_UVC=="TRUE"),]
head(sey.mada)
summary(unique(sey.mada$fish_taxon))
# 105 species from Seychelles dataset also present in Madagascar data


#Load species trait data (from Seychelles productivity):
sey.trait <- read.csv("data/Species_traits_Seychelles.csv", header=T)
head(sey.trait)

#Add in a and b coefficients (to convert lengths to weights later):
sey.ab <- read.csv("data/Sey_species_a&b.csv", header=T)
head(sey.ab)

sey.trait <- merge(sey.trait, sey.ab, by=c("species"), all.x=T)
head(sey.trait)


#Keep species present in sey.mada:
sey.trait$MADA <- sapply(sey.trait$species, function (x){
  if (x %in% sey.mada$fish_taxon)  return("TRUE")
  else F
}) 
head(sey.trait,50)


#Create new trait dataset containing only the 105 species:
sey.trait2 <- sey.trait[which(sey.trait$MADA=="TRUE"),]
head(sey.trait2)
# This traits dataset will be used for productivity calcs.


# Change "fish_taxon" to species, then merge traits with data:
names(sey.mada)[23] <- "species"
head(sey.mada)

length(unique(sey.trait2$species))   #105
length(unique(sey.mada$species))  #105


#Merge survey & trait data:
mada.prod <- merge(sey.mada, sey.trait2, by=c("species"), all.x=T)
head(mada.prod)
# (data is now ordered by species, alphabetically)


# Now run productivity calculations on each individual fish (row) in the dataset.





