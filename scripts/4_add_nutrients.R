#Extract species nutrient content from nutrient database

#Import nutrient database
predictions <- read.csv("data/Species_Nutrient_Predictions.csv",header=T,sep=",",dec=".")
predictions <- unique(predictions)
length(unique(predictions$species)) #4547

#Import Seychelles species list
sp <- read.csv("data/Species traits_Seychelles.csv")

length(unique(sp$species)) #130 species

#check missing species
sp$species[which((sp$species%in%predictions$species)==F)] 

sp$species[which(sp$species=="Scolopsis frenatus")] <- "Scolopsis frenata"
sp$species[which(sp$species=="Plectorhinchus orientalis")] <- "Plectorhinchus vittatus"

length(which((sp$species%in%predictions$species)==T)) #130

sey_species_nut <- merge(sp,predictions,by="species",all.x=T)

write.csv(sey_species_nut, file = "data/sey_species_nut.csv",row.names=FALSE)

#end




