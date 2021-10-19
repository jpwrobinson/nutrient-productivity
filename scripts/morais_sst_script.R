
########################################################################## 
#####      Supporting Information: Supplementary Script          ##### 
##### RA Morais and DR Bellwood - Global drivers of reef fish growth ##### 
########################################################################## 

#---------------------------------------------------# 
##  Calculating pelagic primary productivity from  ## 
## satellite-derived mean chlorophyl concentration ## 
#---------------------------------------------------# 

require (rgdal) 
require (raster) 
require (geosphere) 

#-----------------------------------------------------------------------------------# 
## Loading mean chlorophyl, mean sea surface temperature and photosynthetic  ##  
## active radiation rasters saved from Bio-Oracle       ##  
#-----------------------------------------------------------------------------------# 

data.sstmean <- readGDAL(paste (getwd(), "/Env data BioOracle","/sstmean.asc", sep = "")) 
data.chlomean <- readGDAL(paste (getwd(), "/Env data BioOracle","/chlomean.asc", sep = "")) 
data.parmean <- readGDAL(paste (getwd(), "/Env data BioOracle","/parmean.asc", sep = "")) 

#------------------------------------------------------------------# 
## A data-frame with the geographic coordinates to extract values ## 
## It is important that these coordinates are in decimal degrees  ## 
#------------------------------------------------------------------# 

coords <- read.csv ("coords.csv", h = T, stringsAsF = F) 

#-------------------------------------------------# 
## Extracting the values from the loaded rasters ## 
#-------------------------------------------------# 

sstmean  <- raster::extract (raster (data.sstmean), coords, method = "bilinear") 
chlomean <- raster::extract (raster (data.chlomean), coords, method = "bilinear") 
parmean <- raster::extract (raster (data.parmean), coords, method = "bilinear") 

#---------------------------------------# 
## Calculating mean annual photoperiod ## 
#---------------------------------------# 

photop <- vector ("numeric", nrow (coords) ) 

for (i in 1:nrow (coords) ) { 
  
  photop [i] <- mean (daylength (coords$lat [i], 1:365) )/24 
  
} 

#----------------------------------------------------------------------------------------# 
## Estimating the depth of the euphotic zone from chlorophyl using the regression from  ## 
## Lee et al. (2007) Euphotic zone depth: its derivation and implication to ocean-color ## 
## remote sensing. Journal of Geophysical Research, 112, doi: 10.1029/2006JC003802      ## 
#----------------------------------------------------------------------------------------# 

zeu <- 34 * chlomean ^ -0.39 


#---------------------------------------------------------------------------------------------# 
## Estimating pbopt from the multiple regression on sstmean (Behrenfeld and Falkowski. 1997  ##  
## Photosynthetic rates derived from satellite-based chlorophyl concentration. Limnology and ## 
## Oceanography 42, 1-20             ##  
#---------------------------------------------------------------------------------------------# 

pbopt <- - ( (3.27*10 ^ -8) * (sstmean ^ 7) ) +  
  ( (3.4132*10 ^ -6) * (sstmean ^ 6) ) - ( (1.348*10 ^ -4) * (sstmean ^ 5) ) + 
  ( (2.462*10 ^ -3) * (sstmean ^ 4) ) - (0.0205 * (sstmean ^ 3) ) +  
  (0.0617 * (sstmean ^ 2) ) + (0.2749 * sstmean) + 1.2952 


#-------------------------------------------------------------------------------# 
## Modelling daily primary production based on Behrenfeld and Falkowski (1997) ## 
#-------------------------------------------------------------------------------# 

pelnpp <- 0.66125 * pbopt * (parmean / parmean + 4.1) * zeu * chlomean * photop 
