country_register_github <- function(){
  req <- readr::read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/country/CL_FI_COUNTRY_ITEM.csv", guess_max = 0)
  out <- data.frame(
    code = req$ISO3_Code,
    label = req$Name_En,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$code),]
  return(out)
}

sector_register<- function(){
  req <- unique(readr::read_csv("https://data.apps.fao.org/catalog/dataset/83182b5f-a709-4d58-a2c9-7b25848c4364/resource/a2c5efcf-1a06-449e-a4ed-05393c0f5b72/download/fao-iati-projects.csv")[c("SECTOR","SECTOR_DESC")])
  out <- data.frame(
    code = req$SECTOR,
    label = req$SECTOR_DESC,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$code),]
  return(out)
}

funder_register<- function(){
  req <- unique(readr::read_csv("https://data.apps.fao.org/catalog/dataset/83182b5f-a709-4d58-a2c9-7b25848c4364/resource/a2c5efcf-1a06-449e-a4ed-05393c0f5b72/download/fao-iati-projects.csv")[c("PARTICIPATING_ORG_FUNDING")])
  out <- data.frame(
    code = gsub(" ", "_",req$PARTICIPATING_ORG_FUNDING),
    label = req$PARTICIPATING_ORG_FUNDING,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$code),]
  return(out)
}

region_register_github <-function(){
  req<-readr::read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/country/CL_FI_COUNTRY_GEOREGION.csv")  
  out <- data.frame(
    code = req$UN_Code,
    label = req$Name_En,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$code),]
  return(out)
}