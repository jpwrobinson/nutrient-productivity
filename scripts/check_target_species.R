

## read NOAA solomons data
sol<-readxl::read_excel('data/wcs/NOAA Coop CPUE Analysis 2023.xlsx') %>% 
  clean_names()

load(file = 'results/wcs_productivity.rds')

sol2<-fishp %>% filter(country=='Solomon Islands')

unique(sol$latin_name) # 382 species targeted
unique(sol2$fish_taxon[sol2$fish_taxon %in% sol$latin_name]) ## 165 species in UVC
unique(sol$latin_name[!sol$latin_name %in% sol2$fish_taxon]) ## 216 species targeted, not in UVC
unique(sol$family_4[!sol$latin_name %in% sol2$fish_taxon]) ## 216 species targeted, not in UVC

216 / 382 * 100
