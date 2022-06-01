




library(plotly)

# Read in data
#df <- read.csv("https://cdn.rawgit.com/plotly/datasets/master/GanttChart-updated.csv", stringsAsFactors = F)
df<-as.data.frame(data_nsp[c("country_iso3_code","iati_identifier","sector_code","start_date","end_date")])
# Convert to dates
#df$Start <- as.Date(df$Start, format = "%m/%d/%Y")

# Sample client name
client = unique(df$country_iso3_code)

# Choose colors based on number of resources
cols <- c("blue","green")
df$color <- factor(df$sector_code, labels = cols)

# Initialize empty plot
fig <- plot_ly()

# Each task is a separate trace
# Each trace is essentially a thick line plot
# x-axis ticks are dates and handled automatically

for(i in 1:(nrow(df))){
  fig <- add_trace(fig,
                   x = c(df$start_date[i], df$end_date[i]),  # x0, x1
                   y = c(i, i),  # y0, y1
                   mode = "lines",
                   line = list(color = df$color[i], width = 20),
                   showlegend = F,
                   hoverinfo = "text",
                   
                   # Create custom hover text
                   
                   text = paste("ID: ", df$iati_identifier[i], "<br>",
                                "Duration: ", df$end_date[i]-df$start_date[i], "days<br>",
                                "Sector: ", df$sector_code[i]),
                   
                   evaluate = T  # needed to avoid lazy loading
  )
}

 fig <- layout(fig,
               
               # Axis options:
               # 1. Remove gridlines
               # 2. Customize y-axis tick labels and show task names instead of numbers
               
               xaxis = list(showgrid = F, tickfont = list(color = "#333333")),
               
               yaxis = list(showgrid = F, tickfont = list(color = "#333333"),
                            tickmode = "array", tickvals = 1:nrow(df), ticktext = unique(df$iati_identifier),
                            domain = c(0, 0.9))
               
              # plot_bgcolor = "#333333",  # Chart area color
               #paper_bgcolor = "#333333"
              ) # Axis area color

a <- list(xref = "paper",
          yref = "paper",
          x = 0.80,
          y = 0.1,
          text = paste0("Total Duration: ", sum(df$end_date-df$start_date), " days<br>",
                        "Total Projects: ", length(unique(df$iati_identifier)), "<br>"),
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
          text = paste0("Gantt Chart: ", client),
          font = list(color = '#264E86', size = 20, family = "Times New Roman"),
          ax = 0,
          ay = 0,
          align = "left",
          showarrow = FALSE)


fig <- fig %>% layout(annotations = a) 
fig <- fig %>% layout(annotations = b)
fig


test<-as.data.frame(data_nsp)

test<-ddply(test,.(country_iso3_code),summarise,project=length(iati_identifier))
test<-merge(test,unique(data_nsp[c("geometry","country_iso3_code")]),all.x=T,all.y=F)
head(test)

leaflet()%>% 
  addTiles()%>%
  addPolygons(data=st_as_sf(test),
              fillColor ="grey",
              color="grey",
              layerId=~country_iso3_code)
