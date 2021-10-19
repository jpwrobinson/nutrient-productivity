
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

data.sstmean <- readGDAL('data/env/Present.Surface.Temperature.Mean.asc') 

#------------------------------------------------------------------# 
## A data-frame with the geographic coordinates to extract values ## 
## It is important that these coordinates are in decimal degrees  ## 
#------------------------------------------------------------------# 
fish<-read.csv(file='data/wcs/fish_individuals.csv')

coords <- fish %>% dplyr::select(longitude, latitude)

#-------------------------------------------------# 
## Extracting the values from the loaded rasters ## 
#-------------------------------------------------# 

sstmean  <- raster::extract (raster (data.sstmean), coords, method = "bilinear") 

write.csv(data.frame(sst=sstmean), 'data/env/sst_mean_extracted.csv', row.names=FALSE)
