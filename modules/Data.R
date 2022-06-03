###Module
# Function for module UI
DataUI <- function(id) {
  ns <- NS(id)
  # Options for Spinner
  options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)
  tabPanel("Data", 
           fluidPage(
             div(DT::dataTableOutput(ns("tab1")), style = "font-size:80%") , 
              fluidRow(
               column(2, downloadButton(ns('downloadData'), "Original Dataset"), offset = 10))
             
           )       
  )
  
  
  
}

# Function for module server logic
Data <- function(input, output, session,data) {
  ns = session$ns
  #################
  printText <- reactiveValues(brands = '')
  
  buttonInput <- function(FUN, len, id,ns, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  vals <- reactiveValues()
  
  observe({
    ###Reformat
    data<-as.data.frame(data())
    vals$Data2<-data
    vals$Data <- data.table(
      Region =data$rgeoregion_id,
      Country =data$country_iso3_code,
      Sector = data$sector_code,
      Language = data$description_narrative_xml_lang,
      Identifier = data$other_identifier_ref,
      Title = ifelse(data$title_narrative=="NA","(not available)",paste(substring(as.character(iconv(data$title_narrative, to = "UTF-8")),1,70),"...")),
      Status = data$activity_status_code,
      
      'More informations' = buttonInput(
        FUN = actionButton,
        len = length(data$other_identifier_ref),
        style='padding:4px; font-size:80%;width:80%',
        #width='80%',
        id = 'button_',
        label = "Show detail",
        onclick = paste0('Shiny.onInputChange(\"' , ns("select_button"), '\", this.id)')
      )
    )
    
    
    output$tab1 <- DT::renderDataTable({
      DT = vals$Data
      datatable(DT, 
        #server = FALSE,
        escape = FALSE,
        rownames = FALSE,
        extensions = c("Buttons"), 
        options = list(
          autoWidth = TRUE,
          dom = 'Bfrtip',
          deferRender = TRUE,
          scroll = FALSE,
          buttons = list(
            list(extend = 'copy'),
            list(extend = 'csv', filename =  paste0(Sys.Date(),"Iati_Projects_Summary"), title = NULL, header = TRUE),
	          list(extend = 'excel', filename =  paste0(Sys.Date(),"Iati_Projects_Summary"), title = NULL, header = TRUE),
            list(extend = "pdf", filename = paste0(Sys.Date(),"Iati_Projects_Summary"), title = "Iati_Projects_Summary", header = TRUE),
            list(extend = 'print')
	),
	exportOptions = list(
	  modifiers = list(page = "all", selected = TRUE)
	)
  )
)
    })
    
    observeEvent(input$select_button, {
      selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
      printText$title <<- paste('<b>Title :</b><br/> ',iconv(vals$Data2[selectedRow,"title_narrative"], to = "UTF-8"))
      printText$desc <<- paste('<b>Description :</b><br/> ',iconv(vals$Data2[selectedRow,"description_narrative"], to = "UTF-8"))
      printText$budget <<- paste('<b>Budget :</b> ',vals$Data2[selectedRow,"budget_value"],"$")
      printText$sector <<- paste('<b>Sector :</b> ',vals$Data2[selectedRow,"sector_code"])
      printText$duration <<- paste('<b>Start :</b> ',as.character(vals$Data2[selectedRow,"activity_start_date"]), ' <b>End :</b> ',as.character(vals$Data2[selectedRow,"activity_end_date"]))
      printText$status <<- paste('<b>Status :</b> ',vals$Data2[selectedRow,"activity_status_code"])
      if (is.null(selectedRow) || selectedRow == '') {} else{
        
        showModal(modalDialog(
          title = "Summary of Project",
          HTML(paste(printText$title,printText$desc,printText$budget,printText$sector, printText$duration,printText$status,sep ="<br/>")),
          size = "m",
          easyClose = TRUE,
          footer = NULL
        ))
      }
      
    })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste(Sys.Date(),"Iati_Projects_Resume", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(data, file, row.names = FALSE)
      }
    )
    
  })
}
####