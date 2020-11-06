
 ## ui.R ##
 sidebar <- dashboardSidebar(
    tags$style(".left-side, .main-sidebar {padding-top: 25px}"),
   width=100,
   #collapsed = TRUE,
   sidebarMenu(id="sidebarmenu",
    menuItem("Summary", tabName = "Summary"),
    menuItem("Data", tabName = "Data"),
    menuItem("By Country", tabName = "By_Country")
  #  menuItem("Region", tabName = "Region", icon = icon("bar-chart-o"))
   )
 )

 body <- dashboardBody(
    tags$head(tags$style(HTML('.info-box {min-height: 50px;} .info-box-icon {height: 50px; line-height: 50px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
    
     tabItems(
     tabItem(tabName = "Summary",SummaryUI(id = "summary")),
     tabItem(tabName = "Data",DataUI(id = "data")),
     tabItem(tabName = "By_Country",CountryUI(id = "country"))
     #tabItem(tabName = "Region",ResumeUI(id = "region"))
       )
   )
 # # Put them together into a dashboardPage
 # dashboardPage(
 #   dashboardHeader(titleWidth = 0,title = "",tags$li(class = "dropdown",
 #                                                      tags$a(href="https://www.blue-cloud.org",
 #                                                             #target="_blank",
 #                                                            # tags$img(height = "20px", alt="SNAP Logo", src="https://www.blue-cloud.org/sites/all/themes/arcadia/logo.png")
 #                                                            tags$img(height = "20px",src="https://www.blue-cloud.org/sites/all/themes/arcadia/logo.png")
 #                                                      )
 #   )),
 #   sidebar,
 #   body
 # )
 
 dashboardPage(
    dashboardHeader(titleWidth = 0,title = "",tags$li(class = "dropdown",
         tags$style(".main-header {max-height: 25px}"),
         tags$style(".main-header .logo {height: 25px;}"),
         tags$style(".sidebar-toggle {height: 25px; padding-top: 1px !important;}"),
         tags$style(".navbar {min-height:25px !important}"),
         tags$img(height = "20px", alt="SNAP Logo", src="https://www.blue-cloud.org/sites/all/themes/arcadia/logo.png")
                )),
    sidebar,
    body
 )
# navbarPage(title = "",
#            tabPanel("Summary",
#                     ResumeUI(id = "resume")),
#            tabPanel("Data",
#                     DataUI(id = "data")) ,         
#             tabPanel("Region",
#                      SubsetRegionUI(id="subset"),
#                      ResumeUI(id = "region"))
#           # tabPanel("Country",
#           #          ResumeUI(id = "country")),
#                     )
#                     