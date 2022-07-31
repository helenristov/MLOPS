#Taxi MLOps

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(shinyjs)
library(ggplot2)
#library(networkD3)
library(lubridate)
#library(radarchart)
library(plotly)
#library(sankeywheel)
library(DT)
library(tidyverse)
#library(fmsb)
library(REcharts3)



taxi_data <- read_csv("C:/Users/alkrebs/OneDrive - Capgemini/Desktop/Krebs_personal/ShinyProjects/17 - Taxicab_MLOps/Taxi_Trips_-_2020.csv")


ui <- dashboardPage(
  #includeCSS("www/stars.css"),
  dashboardHeader(
    title = tagList(
      tags$head(
        HTML("<link rel='icon' 
             type='image/png' 
             href='rsz_cap2.png'>")
        ),
      span(class = "logo-lg","Taxicab MLOps",style="color:#FFFFFF;"), 
      img(class = "logo-mini", src = "rsz_cap2.png"))
      ),
  dashboardSidebar(collapsed = FALSE,
                   #disable = TRUE,
                   
                   # Purple-gradient title-bar styling:
                   # tags$head(tags$style(HTML('.logo {
                   #                           background-color: #333366 !important;
                   #                           }
                   #                           .navbar {
                   #                           background-color: #333366 !important;
                   #                           }
                   #                           '))),
                   
                   sidebarMenu(
                     id = "sbm", #sbm = Side Bar Menu
                     menuItem(
                       "Data",
                       tabName = "data",
                       icon = icon("list-alt")
                     ),
                     menuItem(
                       "Models",
                       tabName = "models",
                       icon = icon("line-chart")
                     ),
                     # menuItem(
                     #   "Scenario Comparison",
                     #   tabName = "comparison",
                     #   icon = icon("bar-chart")
                     # ),
                     menuItem(
                       "Deployments",
                       tabName = "deployments",
                       icon = icon("gears")
                     ),
                     menuItem(
                       "Insights",
                       tabName = "insights",
                       icon = icon("search")
                     )
                   )
  ),
  dashboardBody(
    shinyDashboardThemes(
      #theme = "grey_light"
      #theme = "flat_red"
      theme = "grey_dark"
    ),
    
    useShinyjs(),
    
    tabItems(
      tabItem(
        tabName = "data",
        
        fluidPage(
          tags$style(HTML('
                                /* Tab Element Styling: Active v inactive bg colors and text colors*/
                                .tabbable > .nav > li > a               {background-color: #ffffff;  color: #555555; border-top: 3px solid #808080}
                                .tabbable > .nav > li[class=active] > a {background-color: #CF395C50; color:black; border-top: 3px solid #CF395C}
                                .tabbable > .nav > li > a               {padding: 2px}
                                .btn.disabled:hover                     {background-color: black; color: white;}
                                # .highcharts-background {background-color: #000000}
                                # .highcharts-root {background-color: #3E4C7E} /* Sankey Background Color*/
                                # .highcharts-title {font-family: "Helvetica Neue",Helvetica,Arial,sans-serif; font-weight: normal}
                                .sidebar-mini.sidebar-collapse .main-sidebar {width: 75px!important} /*size of collapsed sidebar*/
                                .fa-arrow-down {color:#E87722}
                                .fa-arrow-up {color:#808080}
                                .fa-times-circle {color:red}
                                .fa-check-circle {color:green}
                                .box-header {height:0px; padding:0px}
                                '))
        
        
      ),
        column(width = 12,
               fluidRow(
                 box(width = 3, solidHeader = F,headerBorder = F,style = "text-align:center",
                         p(
                           HTML(paste0('<b>',"Last Update","</b>"))
                         ),
                         h3(
                           HTML(paste0("<b>",
                                       format.Date(Sys.Date(),"%b %d, %Y"),
                                       "</b>"
                           ))
                         ),
                         icon("clock"), "Next refresh in 2 days"
                 ),
                 box(width = 3, solidHeader = F, headerBorder=F,style = "text-align:center",
                         p(
                           HTML(paste0('<b>',"Number of New Records","</b>"))
                         ),
                         h3(
                           HTML(paste0("<b>",
                                       sample(x = c(100,500), size = 1),
                                       "</b>"
                           ))
                         ),
                         icon("arrow-down"), "100 fewer records than previous"
                 ),
                 box(width = 3, solidHeader = F, headerBorder = F,#style = "text-align:center",
                         p(
                           HTML(paste0('<b>',"Data Quality Checks","</b>")),
                           style = "text-align:center"
                         ),
                         column(width = 6,
                                #chart here
                                plotlyOutput("donut_quality", height = "70px")
                         ),
                         column(width = 6,
                                icon("check-circle"), "8 Passing",br(),
                                icon("times-circle"), "0 Failing"
                                )
                         # icon("arrow-down"), "100 fewer records than previous"
                 ),
                 box(width = 3, solidHeader = F, headerBorder = F,#style = "text-align:center",
                     p(
                       HTML(paste0('<b>',"Data Drift Summary","</b>")),
                       style = "text-align:center"
                     ),
                     column(width = 6,
                            plotlyOutput("donut_quality",height = "70px")
                     ),
                     column(width = 6,
                            icon("check-circle"), "6 Passing",br(),
                            icon("times-circle"), "2 Failing"
                     )
                     # icon("arrow-down"), "100 fewer records than previous"
                 )
               ),
               fluidRow(
                 column(width = 4,
                        box(width = 12, solidHeader = F, headerBorder=F,
                        plotlyOutput(outputId = "plot_records_ts",width = "100%")
                        )
                 ),
                 column(width = 4,
                        box(width = 12, solidHeader = F, headerBorder=F,
                        plotlyOutput("plot_quality_ts")
                        )
                        ),
                 column(width =4,
                        box(width = 12, solidHeader = F, headerBorder=F,
                        plotlyOutput("plot_drift_ts")
                        )
                 )
               ),
               fluidRow(
                 column(width = 12,
                        #div(style = "overflow-x:scroll",
                            DT::dataTableOutput("tbl_data_sample")
                        #)
                        )
               )
        )
        
      ),
    tabItem(
      tabName = "models",
      
      fluidPage(
        tags$style(HTML('
                                /* Tab Element Styling: Active v inactive bg colors and text colors*/
                        .tabbable > .nav > li > a               {background-color: #ffffff;  color: #555555; border-top: 3px solid #808080}
                        .tabbable > .nav > li[class=active] > a {background-color: #CF395C50; color:black; border-top: 3px solid #CF395C}
                        .tabbable > .nav > li > a               {padding: 2px}
                        # .highcharts-background {background-color: #000000}
                        # .highcharts-root {background-color: #3E4C7E} /* Sankey Background Color*/
                        # .highcharts-title {font-family: "Helvetica Neue",Helvetica,Arial,sans-serif; font-weight: normal}
                        .sidebar-mini.sidebar-collapse .main-sidebar {width: 75px!important} /*size of collapsed sidebar*/
                        '))
        
      ),
      column(width = 12)
    ),
    
  tabItem(
    tabName = "deployments",
    fluidPage(
      tags$style(HTML('
                      /* Tab Element Styling: Active v inactive bg colors and text colors*/
                      .tabbable > .nav > li > a               {background-color: #ffffff;  color: #555555; border-top: 3px solid #808080}
                      .tabbable > .nav > li[class=active] > a {background-color: #CF395C50; color:black; border-top: 3px solid #CF395C}
                      .tabbable > .nav > li > a               {padding: 2px}
                      .skin-blue .input-group-btn > .btn                 {background-color: #CF395C50; border: 1px solid black}
                      # .highcharts-background {background-color: #000000}
                      # .highcharts-root {background-color: #3E4C7E} /* Sankey Background Color*/
                      # .highcharts-title {font-family: "Helvetica Neue",Helvetica,Arial,sans-serif; font-weight: normal}
                      .sidebar-mini.sidebar-collapse .main-sidebar {width: 75px!important} /*size of collapsed sidebar*/
                      '))
      
      ),
    column(width = 12,
           DT::dataTableOutput("mytable")
    )
    ),
  tabItem(tabName = "insights",
          # Include the custom styling
          # tags$head(
          #   tags$link(rel = "stylesheet", type = "text/css", href = "stars.css")
          # ),
          
          fluidPage(
            tags$style(HTML('
                                /* Tab Element Styling: Active v inactive bg colors and text colors*/
                                .tabbable > .nav > li > a               {background-color: #ffffff;  color: #555555; border-top: 3px solid #808080}
                                .tabbable > .nav > li[class=active] > a {background-color: #CF395C50; color:black; border-top: 3px solid #CF395C}
                                .tabbable > .nav > li > a               {padding: 2px}
                                # .highcharts-background {background-color: #000000}
                                # .highcharts-root {background-color: #3E4C7E} /* Sankey Background Color*/
                                # .highcharts-title {font-family: "Helvetica Neue",Helvetica,Arial,sans-serif; font-weight: normal}
                                .sidebar-mini.sidebar-collapse .main-sidebar {width: 75px!important} /*size of collapsed sidebar*/
                                '))
            
            
          ),
          column(width = 12
          )
  )#End of tabItem1
  )
  )
)



#Server-----
server <- function(input, output) {
  
  print("any server??")
  
  # output$donut_quality <- renderPlotly({
  #   
  # })
  # 
  # output$donut_drift <- renderPlotly({
  #   
  # })
  
  
  output$plot_records_ts <- renderPlotly({
    dta <- data.frame(dt = seq.Date(from = as.Date("2020-12-01"),
                                    to = as.Date("2020-12-31"),
                                    by = "1 day"),
                      ct = sample(500:1000, size = 31))
    
    p <- ggplot(dta, aes(x = dt, y = ct))+
      geom_line()+
      geom_point()+
      theme_bw()
    
    ggplotly(p)
  })
  
  
  # output$plot_quality_ts <- renderPlotly({
  #   
  # })
  # 
  # 
  # output$plot_drift_ts <- renderPlotly({
  #   
  # })
  
  
  output$tbl_data_sample <- DT::renderDataTable({
    DT::datatable(data = taxi_data %>% slice(1:100),
                  selection = list(mode = 'single'))
  })
 
  
  
  
  
  
  

  
} 

shinyApp(ui, server)