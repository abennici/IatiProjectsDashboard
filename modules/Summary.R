###Module
# Function for module UI
SummaryUI <- function(id) {
  ns <- NS(id)

  tabPanel("Data", 
           fluidPage(
                fluidRow(infoBoxOutput(ns("nb_project"),width=3),infoBoxOutput(ns("nb_country"),width=3),infoBoxOutput(ns("nb_budget"),width=3),infoBoxOutput(ns("nb_funder"),width=3)),
                    
                fluidRow(
                      column(4,plotOutput(ns("plot1"),height = "230px")),
                      column(4,plotOutput(ns("plot2"),height = "230px")),
                      column(4,plotlyOutput(ns("plot3"),height = "230px"))
                      ),
                hr(),
                
                fluidRow(
                      column(4,plotOutput(ns("plot4"),height = "270px")),
                      column(4,plotOutput(ns("plot5"),height = "270px")),
                      column(4,plotOutput(ns("plot6"),height = "270px"))
                      )
              
           )       
  )
 
  
}

# Function for module server logic
Summary <- function(input, output, session,data) {
 ns = session$ns
#################

  observe({
    ###Reformat
     data<-as.data.frame(data())
    
   
  #Register
    nb_project<-length(unique(data$iati_identifier))
    nb_country<-length(unique(data$country_iso3_code))
    nb_budget<-sum(data$budget_usd)
    nb_funder<-length(unique(data$participating_org_funding))
    output$nb_project<-renderInfoBox({
      infoBox(
        "Number of Projects",
        nb_project,
        icon = icon("folder-open")
      )
    })
    
    output$nb_country<-renderInfoBox({
      infoBox(
        "Number of Countries",
        nb_country,
        icon = icon("flag")
      )
    })
    
    output$nb_budget<-renderInfoBox({
      infoBox(
        "Total Budget",
        paste0(nb_budget,"$"),
        icon = icon("dollar")
      )
    })
    
    output$nb_funder<-renderInfoBox({
      infoBox(
        "Number of Funders",
        nb_funder,
        icon = icon("hand-holding-usd")
      )
    })

    tab<-data[c("region","country_iso3_code","sector_code","language","iati_identifier","title","status")]
    names(tab)<-c("Region","Country","Sector","Language","ID","Title","Status")
    
    data$st_year<-as.integer(substring(data$start_date,1,4))
    cumul<-ddply(data,.(st_year),summarise,new_project=length(iati_identifier),budget=sum(budget_usd))
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
   
   pie<-ddply(data,.(sector_code),summarise,project=length(iati_identifier))

   fig <- plot_ly(pie, labels = ~as.factor(sector_code), values = ~project,type='pie',textinfo = 'none')
   #fig <- fig %>% add_pie(hole = 0.6)
   fig <- fig %>% layout(title = '',
                         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                         showlegend = TRUE,
   legend = list(font = list(family = "sans-serif", size = 5)),
   margin = list(l = 1, r = 3, t = 1, b = 1))
   
   
   
   
   output$plot1<-renderPlot({plot1})
   output$plot2<-renderPlot({plot2})
   output$plot3 <- renderPlotly(fig)
   
   top<-ddply(data,.(country_iso3_code),summarise,project=length(iati_identifier),budget=sum(budget_usd))
   top_p<-top[order(-top$project),]
   top_p<-top[1:10,]
   top_p$rank<-rank(top_p$project,ties.method = "last")
   
   top_b<-top[order(-top$budget),]
   top_b<-top[1:10,]
   top_b$rank<-rank(top_b$budget,ties.method = "last")
   
   top_f<-ddply(data,.(participating_org_funding),summarise,budget=sum(budget_usd))
   top_f<-top_f[order(-top_f$budget),]
   top_f<-top_f[1:10,]
   top_f$rank<-rank(top_f$budget,ties.method = "last")
   top_f$participating_org_funding<-gsub("_"," ",top_f$participating_org_funding)

   top_prog <-  ggplot(top_p,aes(x = rank,y = project, group = country_iso3_code,fill=as.factor(rank))) +
     geom_tile(aes(y = project / 2, height = project), width = 0.9) +
     geom_text(aes(y = -0.2, label = country_iso3_code), vjust = 0.2, hjust = 1) +
     geom_text(aes( y = max(project)+0.2,label = project), hjust = "left", colour = "grey30") +
     coord_flip(clip="off") +
     scale_y_continuous("",labels=scales::comma) +
     scale_fill_paletteer_d("ggsci::orange_material")+
     guides(color = FALSE, fill = FALSE) +
     theme(axis.line=element_blank(),
           axis.text.x=element_blank(),
           axis.text.y=element_blank(),
           axis.ticks=element_blank(),
           axis.title.x=element_blank(),
           axis.title.y=element_blank(),
           legend.position="none",
           panel.background=element_blank(),
           panel.border=element_blank(),
           panel.grid.major=element_blank(),
           panel.grid.minor=element_blank(),
           panel.grid.major.x = element_line( size=.1, color="grey" ),
           panel.grid.minor.x = element_line( size=.1, color="grey" ),
           plot.title=element_text(size=15, hjust=0.5, face="bold", colour="grey", vjust=-1),
           plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
           plot.caption =element_text(size=9, hjust=0.5, face="italic", color="grey"),
           plot.background=element_blank(),
           plot.margin = margin(1,2, 1, 2, "cm"))+
     labs(title='Top 10 Countries by Number of Projects',
          caption=" Data Source: FAO IATI") 
   
   top_budget <-  ggplot(top_b,aes(x = rank,y = budget, group = country_iso3_code,fill=as.factor(rank))) +
     geom_tile(aes(y = budget / 2, height = budget), width = 0.9) +
     geom_text(aes(y = -50000, label = country_iso3_code), vjust = 0.2, hjust = 1) +
     geom_text(aes( y = max(budget)+10,label = paste("$",budget,sep=" ")), hjust = "left", colour = "grey30") +
     coord_flip(clip="off") +
     scale_y_continuous("",labels=scales::comma) +
     scale_fill_paletteer_d("ggsci::green_material")+
     guides(color = FALSE, fill = FALSE) +
     theme(axis.line=element_blank(),
           axis.text.x=element_blank(),
           axis.text.y=element_blank(),
           axis.ticks=element_blank(),
           axis.title.x=element_blank(),
           axis.title.y=element_blank(),
           legend.position="none",
           panel.background=element_blank(),
           panel.border=element_blank(),
           panel.grid.major=element_blank(),
           panel.grid.minor=element_blank(),
           panel.grid.major.x = element_line( size=.1, color="grey" ),
           panel.grid.minor.x = element_line( size=.1, color="grey" ),
           plot.title=element_text(size=15, hjust=0.5, face="bold", colour="grey", vjust=-1),
           plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
           plot.caption =element_text(size=9, hjust=0.5, face="italic", color="grey"),
           plot.background=element_blank(),
           plot.margin = margin(1,2, 1, 2, "cm"))+
     labs(title='Top 10 Country by Total Budget',
          caption=" Data Source: FAO IATI") 
   
   top_funder <-  ggplot(top_f,aes(x = rank,y = budget, group = participating_org_funding,fill=as.factor(rank))) +
     geom_tile(aes(y = budget / 2, height = budget), width = 0.9) +
     geom_text(aes(y = -50000, label=participating_org_funding), vjust = 0.2, hjust = 1) +
     geom_text(aes( y = max(budget)+10,label = paste("$",budget,sep=" ")), hjust = "left", colour = "grey30") +
     coord_flip(clip="off") +
     scale_y_continuous("",labels=scales::comma) +
     scale_fill_paletteer_d("ggsci::teal_material")+
     guides(color = FALSE, fill = FALSE) +
     theme(axis.line=element_blank(),
           axis.text.x=element_blank(),
           axis.text.y=element_blank(),
           axis.ticks=element_blank(),
           axis.title.x=element_blank(),
           axis.title.y=element_blank(),
           legend.position="none",
           panel.background=element_blank(),
           panel.border=element_blank(),
           panel.grid.major=element_blank(),
           panel.grid.minor=element_blank(),
           panel.grid.major.x = element_line( size=.1, color="grey" ),
           panel.grid.minor.x = element_line( size=.1, color="grey" ),
           plot.title=element_text(size=15, hjust=0.5, face="bold", colour="grey", vjust=-1),
           plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
           plot.caption =element_text(size=9, hjust=0.5, face="italic", color="grey"),
           plot.background=element_blank(),
           plot.margin = margin(1,3, 1, 7, "cm"))+
     labs(title='Top 10 Funder by Total Budget',
          caption=" Data Source: FAO IATI") 
   
   output$plot4<-renderPlot({top_prog})
   output$plot5<-renderPlot({top_budget})
   output$plot6<-renderPlot({top_funder})
 
 })
}
####