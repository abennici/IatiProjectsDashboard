library(shiny)
library(ggplot2)
library(DT)
library(DBI)
library(shinyjs)
library(shinydashboard)
library(data.table)

layer="fao_iati_projects_dbquery_adv_layer"
wfs_server="https://geoserver-sdi-lab.d4science.org/geoserver/sdilab_fisheriesatlas/ows"
wfs_version="1.0.0"
strategy="ogc_viewparams"
par="aggregation_method:none_withgeom"

#Connect to OGC WFS to get DATA
WFS <- WFSClient$new(
  url = wfs_server,
  serviceVersion = wfs_version,
  logger = "INFO"
)
#Get feature type for dataset
ft <- WFS$capabilities$findFeatureTypeByName(layer)

#Get columns names for propertyName argument
desc <- ft$getDescription(TRUE) 

ColumnName<-desc[,"name"]  

propertyName<-paste(ColumnName, collapse = ',')

if(is.null(par)){
  data_nsp <- ft$getFeatures()
}

if(!is.null(par)){
  data_nsp <- switch(strategy,
                     "ogc_filters"=ft$getFeatures(outputFormat ="json",propertyName=propertyName,cql_filter = gsub(" ", "%20", gsub("''", "%27%27", URLencode(par)))),
                     "ogc_viewparams"=ft$getFeatures(outputFormat ="json",propertyName=propertyName,viewparams = URLencode(par))
  )
}
head(data_nsp)

ui <- fluidPage(
  title = "Examples of DataTables",
  mainPanel(tabsetPanel(
    id = 'dataset',
    tabPanel(
            # verbatimTextOutput("text"),
            fluidRow( 
              column(6,htmlOutput("value2"),offset=6
             # verbatimTextOutput('printTitle'),
             # verbatimTextOutput('printDesc'),
             # verbatimTextOutput('printResult'),
             # verbatimTextOutput('printBudget'),
             # verbatimTextOutput('printFunder'),
             # verbatimTextOutput('printDuration')
             )),
            hr(),
            "tab 1", DT::dataTableOutput("tab1"),)
  ))
  
)

server <- function(input, output) {
  
  printText <- reactiveValues(brands = '')
  
  buttonInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  data_nsp<-as.data.frame(data_nsp)
  vals <- reactiveValues()
  observe({
  vals$Data2<-as.data.table(data_nsp)
  vals$Data <- data.table(
    iati_identifier = data_nsp$iati_identifier,
    Sector = data_nsp$sector_code,
    Language = data_nsp$language,
    Status = data_nsp$status,
    
    Action = buttonInput(
      FUN = actionButton,
      len = length(data_nsp$iati_identifier),
      id = 'button_',
      label = "Show detail",
      onclick = 'Shiny.onInputChange(\"lastClick\",  this.id)'
    )
  )
  
  
  output$tab1 <- DT::renderDataTable({
    DT = vals$Data
    datatable(DT, escape = F)
  })
  
  observeEvent(input$lastClick, {
    selectedRow <- as.numeric(strsplit(input$lastClick, "_")[[1]][2])
    printText$title <<- paste('<b>Title :</b> ',iconv(vals$Data2[selectedRow,"title"], to = "ISO-8859-1"))
    printText$desc <<- paste('<b>Description :</b> ',iconv(vals$Data2[selectedRow,"description"], to = "ISO-8859-1"))
    printText$result <<- paste('<b>Result :</b> ',ifelse(is.na(vals$Data2[selectedRow,"result"])|vals$Data2[selectedRow,"result"]=="NA","<i>information not available</i>",
                               iconv(vals$Data2[selectedRow,"result"], to = "ISO-8859-1")))
    printText$budget <<- paste('<b>Budget :</b> ',vals$Data2[selectedRow,"budget_usd"],"$")
    printText$funder <<- paste('<b>Funder :</b> ',gsub(vals$Data2[selectedRow,"participating_org_funding"],"_"," "))
    printText$duration <<- paste('<b>Start :</b> ',as.character(vals$Data2[selectedRow,"start_date"]), ' <b>End :</b> ',as.character(vals$Data2[selectedRow,"end_date"]))
  })
 #  output$text <- renderText({
 # paste(printText$title,
 #            printText$desc,
 #            printText$result,
 #            printText$budget,
 #            printText$funder,
 #            printText$duration,
 #            sep = "")
 #            })
  
  output$value2 <- renderUI({
    HTML(paste(printText$title,printText$desc,printText$result,printText$budget,printText$funder, printText$duration,sep ="<br/>"))
  })

  # output$printTitle <- renderText({
  #   printText$title
  # })
  # output$printDesc <- renderText({
  #   printText$desc
  # })
  # output$printResult <- renderText({
  # printText$result
  # })
  # output$printBudget <- renderText({
  #   printText$budget
  # })
  # output$printFunder <- renderText({
  #   printText$funder
  # })
  # output$printDuration <- renderText({
  #   printText$duration
  # })
  })
}

shinyApp(ui, server)