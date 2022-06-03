#georegion
georegion_register <- function(config){
  req = readr::read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/country/CL_FI_COUNTRY_GEOREGION.csv")
  out <- data.frame(
    code = as.character(req$Identifier),
    label = req$Name_En,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$code),]
  return(out)
}

#sector
sector_register <- function(config){
  req = readr::read_csv("https://iatistandard.org/reference_downloads/203/codelists/downloads/clv1/codelist/Sector.csv")
  out <- data.frame(
    code = as.character(req$code),
    label = gsub("/","-",req$name),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$code),]
  return(out)
}

#activity_status
activity_status_register <- function(config){
  req = readr::read_csv("https://iatistandard.org/reference_downloads/203/codelists/downloads/clv1/codelist/ActivityStatus.csv")
  out <- data.frame(
    code = as.character(req$code),
    label = req$name,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$code),]
  return(out)
}

#country
country_register <- function(config){
  req <- readr::read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/country/CL_FI_COUNTRY_ITEM.csv", guess_max = 0)
  out <- data.frame(
    code = req$ISO3_Code,
    label = req$Name_En,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$code),]
  return(out)
}



