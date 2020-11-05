
# Function for module server logic
RenameCode <- function(input, output, session,data) {
  
  data_rename<-reactiveValues(
    data=NULL
  )
  observe({
data<-as.data.frame(data())

#Rename Region
data<-merge(data,region_register_github(),by.x="region",by.y="code",all.x=T,all.y=F)
data<-subset(data,select=-c(region))
names(data)[names(data) == 'label'] <- 'region'
#Rename Sector
data<-merge(data,sector_register(),by.x="sector_code",by.y="code",all.x=T,all.y=F)
data<-subset(data,select=-c(sector_code))
names(data)[names(data) == 'label'] <- 'sector_code'

#Rename Country
data<-merge(data,country_register_github(),by.x="country_iso3_code",by.y="code",all.x=T,all.y=F)
data<-subset(data,select=-c(country_iso3_code))
names(data)[names(data) == 'label'] <- 'country_iso3_code'

#Rename Funder
data<-merge(data,funder_register(),by.x="participating_org_funding",by.y="code",all.x=T,all.y=F)
data<-subset(data,select=-c(participating_org_funding))
names(data)[names(data) == 'label'] <- 'participating_org_funding'

data_rename$data<-data
})
return(data_rename)
}
