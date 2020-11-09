###Module
# Function for module UI
CountryUI <- function(id) {
  ns <- NS(id)
  
  tabPanel("By_Country", 
           fluidPage(
             fluidRow("Please click on a country for show more information",
               column(width=6,leafletOutput(ns("map"),height = "350px")),
               column(width=6,
                      plotOutput(ns("plot1"),height = "170px"),
                      plotOutput(ns("plot2"),height = "170px"))
               ),
             hr(),
             fluidRow(
               plotlyOutput(ns("gantt"))
                       )
           )
  )           
}

Country <- function(input, output, session,data) {
  ns = session$ns
  rv <- reactiveValues()
  rv$myDf <- NULL
  #################
  
  observe({
    ###Reformat
    test<-as.data.frame(data())
    test<-ddply(test,.(country_iso3_code),summarise,project=length(iati_identifier))
    test<-merge(test,unique(data()[c("geometry","country_iso3_code")]),all.x=T,all.y=F)
    head(test)
    
    output$map <- renderLeaflet({
      
      leaflet()%>% 
        addTiles()%>%
        addPolygons(data=st_as_sf(test),
                    fillColor ="grey",
                    color="grey",
                    layerId=~country_iso3_code,
                    highlightOptions = highlightOptions(color = "white",
                                                        weight = 2,
                                                        bringToFront = TRUE))
    })
    
    observeEvent(input$map_shape_click, { 
      proxy <- leafletProxy("map")
      event <- input$map_shape_click
      sel_lines <- data()[data()$country_iso3_code == event$id,]
      unsel_lines<- data()[data()$country_iso3_code != event$id,]
      # Remove layer based on clicked-ID
    #  proxy %>% removeShape(layerId = event$id)
      proxy %>% clearShapes()
      # Add Filtered Lines
     proxy %>% addPolygons(data = st_as_sf(unsel_lines),
                     layerId = ~country_iso3_code,
                     color="grey", weight=5,opacity=1,
                     highlightOptions = highlightOptions(color = "white",
                                                         weight = 2,
                                                         bringToFront = TRUE))%>%
               addPolygons(data = st_as_sf(sel_lines),
                    layerId = ~country_iso3_code,
                    color="orange", weight=5,opacity=1,
                    highlightOptions = highlightOptions(color = "white",
                                                        weight = 2,
                                                        bringToFront = TRUE))
     
      rv$myDf<-as.data.frame(data()[c("country_iso3_code","iati_identifier","sector_code","start_date","end_date","budget_usd")][data()$country_iso3_code == event$id,])
      # Convert to dates
      
      if(nrow(rv$myDf)>0){
        
        flag = unique(rv$myDf$country_iso3_code)
        
        
        # Initialize empty plot
        fig <- plot_ly()
        
        for(i in 1:(nrow(rv$myDf))){
          fig <- add_trace(fig,
                           x = c(rv$myDf$start_date[i], rv$myDf$end_date[i]),  
                           y = c(i, i),  
                           mode = "lines",
                           line = list(width = 20),
                           showlegend = F,
                           hoverinfo = "text",
                           
                           # Create custom hover text
                           
                           text = paste("ID: ", rv$myDf$iati_identifier[i], "<br>",
                                        "Duration: ", rv$myDf$end_date[i]-rv$myDf$start_date[i], "days<br>",
                                        "Sector: ", rv$myDf$sector_code[i]),
                           
                           evaluate = T  # needed to avoid lazy loading
          )
        }
        
        fig <- layout(fig,
                      
                      # Axis options:
                      # 1. Remove gridlines
                      # 2. Customize y-axis tick labels and show task names instead of numbers
                      
                      xaxis = list(showgrid = F, tickfont = list(color = "#333333")),
                      
                      yaxis = list(showgrid = F, tickfont = list(color = "#333333"),
                                   tickmode = "array", tickvals = 1:nrow(rv$myDf), ticktext = unique(rv$myDf$iati_identifier),
                                   domain = c(0, 0.9))
                      
                      # plot_bgcolor = "#333333",  # Chart area color
                      #paper_bgcolor = "#333333"
        ) # Axis area color
        
        a <- list(xref = "paper",
                  yref = "paper",
                  x = 0.80,
                  y = 0.1,
                  text = paste0("Total Duration: ", sum(rv$myDf$end_date-rv$myDf$start_date), " days<br>",
                                "Total Projects: ", length(unique(rv$myDf$iati_identifier)), "<br>"),
                  font = list(color = '#264E86', size = 12),
                  ax = 0,
                  ay = 0,
                  align = "left",
                  showarrow = FALSE)
        
        # Add client name and title on top
        b <- list(xref = "paper",
                  yref = "paper",
                  x = 0.1,
                  y = 1,
                  xanchor = "left",
                  text = paste0("Projects in ", flag),
                  font = list(color = '#264E86', size = 20, family = "Times New Roman"),
                  ax = 0,
                  ay = 0,
                  align = "left",
                  showarrow = FALSE)
        
        
        fig <- fig %>% layout(annotations = a) 
        fig <- fig %>% layout(annotations = b)
        output$gantt <- renderPlotly(fig)
        
        
        rv$myDf$st_year<-as.integer(substring(rv$myDf$start_date,1,4))
        cumul<-ddply(rv$myDf,.(st_year),summarise,new_project=length(iati_identifier),budget=sum(budget_usd))
        cumul$cumul_new_project<-cumsum(cumul$new_project)
        cumul$cumul_budget<-cumsum(cumul$budget)
        
        plot1<-ggplot(cumul)  + 
          geom_bar(aes(x=as.factor(st_year), y=new_project),stat="identity", fill="tan1", colour="sienna3")+
          geom_point(aes(x=as.factor(st_year), y=cumul_new_project,group=1),stat="identity",colour="sienna3")+
          geom_line(aes(x=as.factor(st_year), y=cumul_new_project,group=1),stat="identity",linetype="dashed",colour="tan1")+
          labs(x="Year",y="New Projects")
        
        plot2<-ggplot(cumul)  + 
          geom_bar(aes(x=as.factor(st_year), y=budget),stat="identity", fill="lightgreen", colour="darkgreen")+
          geom_point(aes(x=as.factor(st_year), y=cumul_budget,group=1),stat="identity",colour="darkgreen")+
          geom_line(aes(x=as.factor(st_year), y=cumul_budget,group=1),stat="identity",linetype="dashed",colour="lightgreen")+
          labs(x="Year",y="Budget")+
          scale_y_continuous(labels=scales::dollar_format())
        
        output$plot1<-renderPlot({plot1})
        output$plot2<-renderPlot({plot2})
        
      }else{output$gantt <- renderPlotly(NULL)
      output$plot1<-renderPlot({NULL})
      output$plot2<-renderPlot({NULL})}
      
    })
  })
    
  }

    

    
