
# Function for module server logic
RenameCode <- function(input, output, session,data) {
  
  data_rename<-reactiveValues(
    data=NULL
  )
  observe({
data<-as.data.frame(data())

#Rename Geo Region
data<-merge(data,georegion_register(),by.x="georegion_id",by.y="code",all.x=T,all.y=F)
data<-subset(data,select=-c(georegion_id))
names(data)[names(data) == 'label'] <- 'georegion_id'

#Rename Sector
data<-merge(data,sector_register(),by.x="sector_code",by.y="code",all.x=T,all.y=F)
data<-subset(data,select=-c(sector_code))
names(data)[names(data) == 'label'] <- 'sector_code'

#Rename Country
data<-merge(data,country_register(),by.x="country_iso3_code",by.y="code",all.x=T,all.y=F)
data<-subset(data,select=-c(country_iso3_code))
names(data)[names(data) == 'label'] <- 'country_iso3_code'

#Rename Status
data<-merge(data,activity_status_register(),by.x="activity_status_code",by.y="code",all.x=T,all.y=F)
data<-subset(data,select=-c(activity_status_code))
names(data)[names(data) == 'label'] <- 'activity_status_code'


data_rename$data<-data
})
return(data_rename)
}
