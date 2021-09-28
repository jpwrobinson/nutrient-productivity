#Extract Lmax from FishBase

#Load functions 
#Taxonomy
getTaxo <- function(sp,tax){
  
  #Check Names
  Species <- sp
  Species_corrected <- rep("NA",length(sp))
  SpecCode <- rep("NA",length(sp))
  
  for(k in 1:length(Species)){
    test <- validate_names(Species[k])
    if(length(test)==1){
      Species_corrected[k] <- test
      SpecCode[k] <- as.numeric(rfishbase::species(test,fields="SpecCode"))
    }else{
      Species_corrected[k] <- test[1]
      SpecCode[k] <- as.numeric(rfishbase::species(test[1],fields="SpecCode"))
      next
    }
    
  }#end of k
  
  correctNames <- data.frame(Species,Species_corrected,SpecCode)
  
  taxoFB <- as.data.frame(tax)
  
  Genus <- rep("NA",nrow(correctNames))
  Family <- rep("NA",nrow(correctNames))
  Order <- rep("NA",nrow(correctNames))
  Class <- rep("NA",nrow(correctNames))
  
  for(k in 1:nrow(correctNames)){
    id <- which(taxoFB$Species == correctNames$Species_corrected[k])
    if(length(id)==1){
      Genus[k] <- taxoFB$Genus[id]
      Family[k] <- taxoFB$Family[id]
      Order[k] <- taxoFB$Order[id]
      Class[k] <- taxoFB$Class[id]
    }else{
      next
    }
  } #end of k
  
  taxo <- data.frame(Species_corrected,Genus,Family,Order,Class)
  sp_data<-merge(correctNames,taxo,by="Species_corrected",all.x=T)
  
  return(sp_data)
  
}#end of function getTaxo

#################################################
#Lmax
getLmax<-function(sp_data){
  
  scraplinks <- function(url){
    # Create an html document from the url
    webpage <- xml2::read_html(url)
    # Extract the URLs
    url_ <- webpage %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href")
    # Extract the link text
    link_ <- webpage %>%
      rvest::html_nodes("a") %>%
      rvest::html_text()
    return(tibble(link = link_, url = url_))
  }#end of scraplinks
  
  
  #Delete missing species and create empty vector to fill in
  sp <- unique(sp_data$Species_corrected)
  
  lmax <- vector()
  lmaxLevel <- rep("species",length(sp))
  lmaxtype <- vector()
  
  for(i in 1:length(sp)) {
    cat("i",i,"\n")
    # Grab species page
    
    urly = paste('https://www.fishbase.se/summary/',str_replace_all(sp[i]," ", "-"),'.html',sep="")
    
    #Extract URLs and Links
    glinks <- scraplinks(urly)
    idlink <- which(glinks$link == "Life-history tool")
    
    url <- paste("https://www.fishbase.se",glinks$url[idlink],sep="")
    
    html <- read_html(url)
    
    #Extract Lmax
    value <- html %>%
      html_nodes("tr")
    
    id <- grep("Lmax", value)
    
    if(length(id)==0){
      lmax[i] <- NA
      lmaxLevel[i] <- NA
      lmaxtype[i] <- 'unknown'
    }else{
      h <- value[id]
      lmax[i] <- as.numeric(html_nodes(h, "input[type='text']") %>% html_attr("value"))
      
      #Extract type if drop-down menu
      x <- gsub(">", ">\n", h)
      x <- paste0(x, collapse = "")
      x <- unlist(strsplit(x, "\n"))
      
      ## Extract selected options ----
      test <- grep("option.*selected", x)
      
      if(length(test)==1){
        type <- x[test]
        type <- gsub("\"", "", regmatches(type, regexpr("\"[A-Z]{2}\"", type)))
        
      }else{
        #Extract type if text (fixed) value
        l_text <- value %>%
          html_text()
        
        #Identify Lmax section and extract type
        id <- grep("Lmax", l_text)
        
        #Delete multiples spaces etc.
        lraw <- gsub("\r|\n|\t", " ", l_text[id])
        lclean <- gsub("\\s+", " ", str_trim(lraw))
        lval <- strsplit(lclean," ")[[1]]
        
        type <- lval[length(lval)]
      }
      
      #Save
      lmaxtype[i] <- type
    }
    
    
  }#end of i
  
  len <- data.frame(sp,lmax,lmaxtype,lmaxLevel)
  names(len)[1] <- "Species_corrected"
  
  res <- merge(sp_data,len,by="Species_corrected",all.x=T) 
  
  return(res) 
  
}#end of getLmax

#Load libraries
require(rfishbase)
require(rvest)
require(stringr)
require(xml2)
require(tidyverse)

#Get fish Taxonomy from Fishbase 
tax <- load_taxa() 

#Import species list
#example here with:
species_list <- c("Siganus sutor","Caesio varilineata","Chlorurus sordidus")
sp_data <- getTaxo(sp = species_list , tax = tax)

lmax <- getLmax(sp_data)
lmax
#END
