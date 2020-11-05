###Module
# Function for module UI
SubsetRegionUI <- function(id) {
  ns <- NS(id)
  
  # Options for Spinner
  options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)
  tabPanel("Subset", fluidRow(
    selectizeInput(ns("region"),"Choose region",c(""), multiple = FALSE),
    tags$div(actionButton(ns("gobutton"),"Update selection"))
    
  ))
  
}

# Function for module server logic
SubsetRegion <- function(input, output, session,data) {
  
  data_subset<-reactiveValues(
    data=NULL
  )
  observe({
    tab<-data()
    class(tab) <- "data.frame"
    print("Here we are")
    print(sprintf("We have selected %s rows in data", nrow(data_subset$data)))
    

    region<- unique(tab$region)
    if (is.null(region))
      region<- character(0)
  
    # Update FLAG
    updateSelectizeInput(session,
                         inputId ="region",
                         choices = unique(region),
                         selected = unique(region)[1])
   
     # data_subset$data<-tab
  })
 # data_subset$data<-tab
  ###Reformat
  observeEvent(input$gobutton, {
    
    
    #select_region<-if(is.null(input$region))unique(tab$region)else(input$region)
    
    data_subset$data<-data()%>%
      filter(region %in% input$region)
    
    
    print(data_subset$data)
    
  })
  
  
  return(data_subset)
  
}
####