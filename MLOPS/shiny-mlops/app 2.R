#Taxi MLOps


#Library Load -----
library(shiny)
library(flexdashboard)
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
#library(REcharts3)
library(radarchart)
library(RColorBrewer)


#Data Load (from local) -----

taxi_data <- read_csv("Taxi_Trips_-_2020.csv")

# connect to sql
library(RMySQL)

db <- dbConnect(
  MySQL(),
  host = "34.121.160.220",
  user = "root",
  password = "naklPqlt8G2HjqCM",
  dbname = "db"
)
dbListTables(db)

#Query SQL
curr_run = dbGetQuery(db, "SELECT * FROM DATA_METRICS ORDER BY data_pushtime DESC limit 1")
prev_run = dbGetQuery(db, "SELECT *  FROM DATA_METRICS WHERE DATA_PUSHTIME < (SELECT MAX(DATA_PUSHTIME) FROM DATA_METRICS) ORDER BY data_pushtime DESC limit 1")
radial_df = dbGetQuery(db, "SELECT * FROM RADIAL_METRICS WHERE data_pushtime = (SELECT MAx(DATA_PUSHTIME) FROM RADIAL_METRICS)")
mt = dbGetQuery(db, "SELECT * FROM DATA_METRICS")

#*******************************************************************************-----
#*****UI Start*****-------

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
  
  #Sidebar-----
  dashboardSidebar(collapsed = FALSE,
                   
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
  
  #Dashboard body-----
  dashboardBody(
    shinyDashboardThemes(
      #theme = "grey_light"
      #theme = "flat_red"
      theme = "grey_dark"
    ),
    
    useShinyjs(),
    
    tabItems(
      
      #********************Data Screen ------
      tabItem(
        tabName = "data",
        
        
        #At the top of the page, include any custom styling (this will persist in all tabs):
        fluidPage(
          tags$style(HTML('
                                /* Tab Element Styling: Active v inactive bg colors and text colors*/
                                .tabbable > .nav > li > a               {background-color: #ffffff;  color: #555555; border-top: 3px solid #808080}
                                .tabbable > .nav > li[class=active] > a {background-color: #CF395C50; color:black; border-top: 3px solid #CF395C}
                                .tabbable > .nav > li > a               {padding: 2px}
                                .btn.disabled:hover                     {background-color: black; color: white;}
                                .sidebar-mini.sidebar-collapse .main-sidebar {width: 75px!important} /*size of collapsed sidebar*/
                                .fa-arrow-down                          {color:#E87722}
                                .fa-arrow-up                            {color:#A8D60D}
                                .fa-times-circle                        {color:red}
                                .fa-check-circle                        {color:green}
                                .box-header                             {height:0px; padding:0px}
                                .html-widget.gauge svg                  {height: 300px;}
                                .html-widget.gauge svg > .text         {border-bottom: 10px}
                                .col-sm-6 > .box.box-solid              {height: 600px}
                                .box:hover                              { box-shadow: 0px 8px 8px 0px rgb(255, 255, 255, 0.6);}
                                .nav-tabs-custom:hover                  { box-shadow: 0px 8px 8px 0px rgb(255, 255, 255, 0.6);}
                                .h3,h3                                  {margin-top: 2px}
                                .p                                      {margin-bottom: 2px}
                                .col-sm-12                              {padding-right: 0px}
                                '))
          
          
        ),
        
        # First Row of Info Boxes -----
        column(width = 12, style = "padding-left: 15px",
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
                     icon("clock"), "Next auto-refresh in 2 days"#,
                     #br(),
                     #br(),
                     #actionButton("btn_refresh_data", label = "Refresh Data",icon = icon("refresh"))
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
                            plotlyOutput("donut_quality" 
                                         ,height = "54px"
                            )
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
                            plotlyOutput("donut_drift",height = "54px")
                     ),
                     column(width = 6,
                            icon("check-circle"), "6 Passing",br(),
                            icon("times-circle"), "2 Failing"
                     )
                     # icon("arrow-down"), "100 fewer records than previous"
                 )
               ),
               
               #Second Row of Info Boxes -----
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
               
               # Data Table -----
               fluidRow(
                 column(width = 12, style = "padding-right: 15px",
                        box(width = 12, solidHeader = F, headerBorder = F,
                        div(style = "overflow-x:scroll",
                            DT::dataTableOutput("tbl_data_sample")
                        )
                        )
                 )
               )
        )
        
      ),
      # ********************Model Screen -----
      tabItem(
        tabName = "models",
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "stars.css")
        ),
        fluidPage(
          
        ),
        column(width = 12,
               
               #First Row of Info Boxes -----
               fluidRow(
                 box(width = 3, solidHeader = F,headerBorder = F,style = "text-align:center",
                     p(
                       HTML(paste0('<b>',"Current Model Deployment Date","</b>"))
                     ),
                     h3(
                       HTML(paste0("<b>",
                                   format.Date(curr_run$data_pushtime,"%b %d, %Y"),
                                   "</b>"
                       ))
                     )#,
                     #icon("clock"), "2 days old"
                 ),
                 box(width = 3, solidHeader = F, headerBorder=F,style = "text-align:center",
                     p(
                       HTML(paste0('<b>',"Observations in Trained Model","</b>"))
                     ),
                     h3(
                       HTML(paste0("<b>",
                                   paste0(formatC(curr_run$num_records/1000, big.mark = ","),"k"),
                                   "</b>"
                       ))
                     )#,
                     #icon("arrow-down"), "1,000 fewer records than previous"
                 ),
                 box(width = 3, solidHeader = F, headerBorder = F,style = "text-align:center",
                     p(
                       HTML(paste0('<b>',"Date Range of Training Data","</b>")),
                       style = "text-align:center"
                     ),
                     h3(
                       HTML(paste0("<b>",
                                   format.Date(curr_run$start_date, "%b %d, %Y"),
                                   " - ",
                                   format.Date(curr_run$end_date, "%b %d, %Y"),
                                   "</b>"
                       ))
                     )
                     # icon("arrow-down"), "100 fewer records than previous"
                 ),
                 box(width = 3, solidHeader = F, headerBorder = F,style = "text-align:center",
                     p(
                       HTML(paste0('<b>',"Previous MAPE","</b>")),
                       style = "text-align:center"
                     ),
                     h3(
                       HTML(paste0("<b>",
                                   round(prev_run$mape,2),#round(rnorm(n = 1, mean = 6, sd = 1.5),2),
                                   "%",
                                   "</b>"
                       ))
                     )
                     #icon("arrow-down"), "100 fewer records than previous"
                 )
               ),
               
               #Second Row of Info Boxes -----
               fluidRow(
                 box(width = 3, solidHeader = F, headerBorder = F,style = "text-align:center",
                     p(
                       HTML(paste0('<b>',"Average Rider Rating","</b>")),
                       style = "text-align:center; margin-bottom:0px;"
                     ),
                     tags$div(class = "ratings",
                              tags$div(class = "empty-stars",
                                       uiOutput("stars_ui")
                              ),
                              # Boxes need to be put in a row (or column)
                              # fluidRow(
                              #   sliderInput(inputId = "n_stars", label = "", min = 0,  max = 5, value = 3, step = .15)
                              #   )
                              #actionButton(inputId = "btn",label = "new rating"),
                              textOutput("txt")
                     ),
                     br(),
                     icon("arrow-up"), "18% lift from model"
                 ),
                 box(width = 3, solidHeader = F, headerBorder = F,style = "text-align:center",
                     p(
                       HTML(paste0('<b>',"Value of Model","</b>")),
                       style = "text-align:center"
                     ),
                     h3(
                       HTML(paste0("<b>$",
                                   round(rnorm(n = 1, mean = 6, sd = 1.5),2),
                                   "M</b>"
                       ))
                     ),
                     #br(),
                     icon("arrow-up"), "5% increase since inception"
                 ),
                 box(width = 3, solidHeader = F, headerBorder = F,style = "text-align:center",
                     p(
                       HTML(paste0('<b>',"Monthly Riders","</b>")),
                       style = "text-align:center"
                     ),
                     h3(
                       HTML(paste0("<b>",
                                   round(rnorm(n = 1, mean = 6, sd = 1.5),2),
                                   "K</b>"
                       ))
                     ),
                     
                     #br(),
                     icon("arrow-down"), "3% decrease YoY"
                 ),
                 box(width = 3, solidHeader = F, headerBorder = F,style = "text-align:center",
                     p(
                       HTML(paste0('<b>',"Placeholder 2","</b>")),
                       style = "text-align:center"
                     ),
                     actionButton(inputId = "button_refresh", label = "Refresh", icon = icon('refresh'))
                     #br(),
                     #icon("arrow-up"), "[additional info here]"
                 )
               ),
               fluidRow(
                 
                 #Gauge Chart (semi-circle)-----
                 
                 box(width = 4,solidHeader = F, headerBorder = F,height = 500,
                     textOutput("txt_model_health"),
                     br(),
                     gaugeOutput("gauge"
                                    #,width = "100%",
                                    #,height = "150px"
                                    # "gauge" is modified in CSS rule above: .html-widget.gauge svg {height: 450px}
                                    ),
                     br()
                        
                        ),
                 
                 #Deployment History Time Series Chart -----
                 
                 box(width = 8, solidHeader = F, headerBorder = F, height = 500,
                     fluidRow(
                       column(width = 8,
                              dateRangeInput(inputId = "input_deployment_date_range", width = "75%",
                                             label = "",
                                             start = as.Date(min(mt$data_pushtime)),
                                             end = as.Date(max(mt$data_pushtime)),
                                             format = "M. d, yyyy")
                       ),
                       column(width = 4,
                              div(downloadButton("report_comps1", label = "Download Report"),style = "display: inline-block;vertical-aligh:middle;float:right")
                       )
                       ),
                   plotlyOutput("plot_deployments")
                 )
               ),
               
               #Radar Charts-----
               fluidRow(
                 box(width = 6, solidHeader = F, headerBorder = F, height = 600,
                     chartJSRadarOutput("radar_dow", width = "450", height = "300")
                 ),
                 tabBox(id = "box_radars",selected = "Overall Hourly", height = 600,
                   tabPanel("Overall Hourly", chartJSRadarOutput("radar", width = 450, height = 300)),
                   tabPanel("Mon",chartJSRadarOutput("radar_mon")),
                   tabPanel("Tue",chartJSRadarOutput("radar_tue")),
                   tabPanel("Wed",chartJSRadarOutput("radar_wed")),
                   tabPanel("Thu",chartJSRadarOutput("radar_thu")),
                   tabPanel("Fri",chartJSRadarOutput("radar_fri")),
                   tabPanel("Sat",chartJSRadarOutput("radar_sat")),
                   tabPanel("Sun",chartJSRadarOutput("radar_sun"))
                 )
               )
               
        )
      ),
      
      tabItem(
        tabName = "deployments",
        fluidPage(
          
          
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
                
              ),
              column(width = 12
              )
      )
    )
  )
)



#*******************************************************************************-----
#*******************************************************************************-----


#*****Server Start*****-----
server <- function(input, output) {
  
  
  rv <- reactiveValues(
    curr_run = dbGetQuery(db, "SELECT * FROM DATA_METRICS ORDER BY data_pushtime DESC limit 1"),
    prev_run = dbGetQuery(db, "SELECT *  FROM DATA_METRICS WHERE DATA_PUSHTIME < (SELECT MAX(DATA_PUSHTIME) FROM DATA_METRICS) ORDER BY data_pushtime DESC limit 1"),
    radial_df = dbGetQuery(db, "SELECT * FROM RADIAL_METRICS WHERE data_pushtime = (SELECT MAX(DATA_PUSHTIME) FROM RADIAL_METRICS)"),
    mt = dbGetQuery(db, "SELECT * FROM DATA_METRICS")
    )
  
  #Data Loading from Backend------
  observeEvent(input$button_refresh, ignoreInit = F,{
    # rv <- reactiveValues(
    #   curr_run = dbGetQuery(db, "SELECT * FROM DATA_METRICS ORDER BY data_pushtime DESC limit 1"),
    #   prev_run = dbGetQuery(db, "SELECT *  FROM DATA_METRICS WHERE DATA_PUSHTIME < (SELECT MAX(DATA_PUSHTIME) FROM DATA_METRICS) ORDER BY data_pushtime DESC limit 1"),
    #   radial_df = dbGetQuery(db, "SELECT * FROM RADIAL_METRICS WHERE data_pushtime = (SELECT MAx(DATA_PUSHTIME) FROM RADIAL_METRICS)"),
    #   mt = dbGetQuery(db, "SELECT * FROM DATA_METRICS")
    #   )
    rv$curr_run = dbGetQuery(db, "SELECT * FROM DATA_METRICS ORDER BY data_pushtime DESC limit 1")
    rv$prev_run = dbGetQuery(db, "SELECT *  FROM DATA_METRICS WHERE DATA_PUSHTIME < (SELECT MAX(DATA_PUSHTIME) FROM DATA_METRICS) ORDER BY data_pushtime DESC limit 1")
    rv$radial_df = dbGetQuery(db, "SELECT * FROM RADIAL_METRICS WHERE data_pushtime = (SELECT MAx(DATA_PUSHTIME) FROM RADIAL_METRICS)")
    rv$mt = dbGetQuery(db, "SELECT * FROM DATA_METRICS")
    
    #print("is this working")
    #print(rv$curr_run)
    #print(str(rv))
  })
  
  #********************Data Page Server: -----
  
  # Donut Charts for Data Quality Checks-----
  output$donut_quality <- renderPlotly({
    dta <- data.frame(
      category=c("Pass", "Fail"),
      count=c(8,0)
    )
    
    # Compute percentages
    dta$fraction <- dta$count / sum(dta$count)
    
    # Compute the cumulative percentages (top of each rectangle)
    dta$ymax <- cumsum(dta$fraction)
    
    # Compute the bottom of each rectangle
    dta$ymin <- c(0, head(dta$ymax, n=-1))
    
    # Compute label position
    dta$labelPosition <- (dta$ymax + dta$ymin) / 2
    
    # Compute a good label
    dta$label <- paste0(dta$category, "\n value: ", dta$count)
    
    fig <- dta %>% plot_ly(labels = ~category, 
                           values = ~count, 
                           marker = list(colors = c('rgb(118, 230, 110)', 'rgb(240, 86, 86)'),
                                         line = list(color = '#FFFFFF', width = 1)))
    fig <- fig %>% add_pie(hole = 0.8)
    fig <- fig %>% layout(showlegend = F, plot_bgcolor='rgba(0,0,0,0)',paper_bgcolor='rgba(0,0,0,0)',
                          margin = list(
                            l = 0,
                            r = 0,
                            b = 0,
                            t = 0,
                            pad = 4
                          ),
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% 
      config(displayModeBar = F)
    
    fig
  })
  
  output$donut_drift <- renderPlotly({
    dta <- data.frame(
      category=c("Pass", "Fail"),
      count=c(6,2)
    )
    
    # Compute percentages
    dta$fraction <- dta$count / sum(dta$count)
    
    # Compute the cumulative percentages (top of each rectangle)
    dta$ymax <- cumsum(dta$fraction)
    
    # Compute the bottom of each rectangle
    dta$ymin <- c(0, head(dta$ymax, n=-1))
    
    # Compute label position
    dta$labelPosition <- (dta$ymax + dta$ymin) / 2
    
    # Compute a good label
    dta$label <- paste0(dta$category, "\n value: ", dta$count)
    
    fig <- dta %>% plot_ly(labels = ~category, 
                           values = ~count, 
                           marker = list(colors = c('rgb(118, 230, 110)', 'rgb(240, 86, 86)'),
                                         line = list(color = '#FFFFFF', width = 1)))
    fig <- fig %>% add_pie(hole = 0.8)
    fig <- fig %>% layout(showlegend = F, plot_bgcolor='rgba(0,0,0,0)',paper_bgcolor='rgba(0,0,0,0)',
                          margin = list(
                            l = 0,
                            r = 0,
                            b = 0,
                            t = 0,
                            pad = 4
                          ),
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% 
      config(displayModeBar = F)
    
    fig
  })
  
  
  # Plot of Number of Ingested Records over Time-----
  output$plot_records_ts <- renderPlotly({
    dta <- data.frame(dt = seq.Date(from = as.Date("2020-12-01"),
                                    to = as.Date("2020-12-31"),
                                    by = "1 day"),
                      ct = sample(500:1000, size = 31))
    
    p <- ggplot(dta, aes(x = dt, y = ct))+
      geom_line(color = "white")+
      geom_point(color = "white")+
      scale_x_date(breaks = sort(dta$dt[which(1:nrow(dta) %% 7 == 0)]), date_labels = "%b %d, %Y")+
      theme(plot.background = element_rect(fill = "#00000000"),
            panel.background = element_rect(fill="#00000000"),
            panel.grid.major = element_line(colour = "#808080"),
            axis.text.x = element_text(colour = "#808080", angle = 45, hjust = 0),
            axis.text.y = element_text(colour = "#808080"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(colour = "#808080")
      )
    
    ggplotly(p)
  })
  
  # Plot of Quality Pass/Fails over time-----
  output$plot_quality_ts <- renderPlotly({
    dta <- data.frame(dt = seq.Date(from = as.Date("2020-12-01"),
                                    to = as.Date("2020-12-31"),
                                    by = "1 day"),
                      ct = sample(6:8, size = 31, replace = T)) %>% 
      mutate(err = 8-ct) %>% 
      gather(., key = "type", value = "val", -dt) %>% 
      mutate(type = factor(type, ordered = T, levels = c("err","ct")))
    
    p <- ggplot(dta, aes(x = dt, y = val))+
      geom_area(aes(color = type, fill = type))+
      scale_fill_manual(values = c("#f05656","#05990e90"))+
      scale_color_manual(values = c("#f05656","#05990e90"))+
      scale_x_date(breaks = sort(dta$dt[which(1:nrow(dta) %% 7 == 0)]), date_labels = "%b %d, %Y")+
      theme(plot.background = element_rect(fill = "#00000000"),
            panel.background = element_rect(fill="#00000000"),
            panel.grid.major = element_line(colour = "#808080"),
            axis.text.x = element_text(colour = "#808080", angle = 45, hjust = 0),
            axis.text.y = element_text(colour = "#808080"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(colour = "#808080"),
            legend.position="none"
      )
    
    ggplotly(p)
  })
  
  # Plot of Data Drift Pass/Fails over time -----
  output$plot_drift_ts <- renderPlotly({
    dta <- data.frame(dt = seq.Date(from = as.Date("2020-12-01"),
                                    to = as.Date("2020-12-31"),
                                    by = "1 day"),
                      ct = sample(6:8, size = 31, replace = T)) %>% 
      mutate(err = 8-ct) %>% 
      gather(., key = "type", value = "val", -dt) %>% 
      mutate(type = factor(type, ordered = T, levels = c("err","ct")))
    
    p <- ggplot(dta, aes(x = dt, y = val))+
      geom_area(aes(color = type, fill = type))+
      scale_fill_manual(values = c("#f05656","#05990e90"))+
      scale_color_manual(values = c("#f05656","#05990e90"))+
      theme(plot.background = element_rect(fill = "#00000000"),
            panel.background = element_rect(fill="#00000000"),
            panel.grid.major = element_line(colour = "#808080"),
            axis.text.x = element_text(colour = "#808080", angle = 45, hjust = 0),
            axis.text.y = element_text(colour = "#808080"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(colour = "#808080"),
            legend.position="none"
      )
    
    ggplotly(p)
  })
  
  # Data Table -----
  output$tbl_data_sample <- DT::renderDataTable({
    DT::datatable(data = taxi_data %>% 
                    slice(1:100) %>% 
                    select(-1,-2),
                  selection = list(mode = 'single'))
  })
  
  
  
  
  #********************Model Page Server:-----
  
  #Stars UI-----
  output$stars_ui <- renderUI({
    # to calculate our input %
    #n_fill <- (input$n_stars / 5) * 100
    
    # element will look like this: <div class="full-stars" style="width:n%"></div>
    # style_value <- sprintf("width:%s%%", n_fill)
    # style_value <- sprintf("width:%s%%", rating$curr)
    style_value <- sprintf("width:%s%%", (4.4/5) * 100) #value has to be out of 100 (e.g. 55 = 55%)
    tags$div(class = "full-stars", style = style_value)
  })
  
  #Gauge Chart (semi-circle) and status text-----
  observe({
    #print(paste0("newGauge",Sys.time()))
    #print(curr_run$mape)
  output$gauge = renderGauge({
    gauge(round(rv$curr_run$mape,0), 
          min = 0, 
          max = 100, 
          sectors = gaugeSectors(success = c(0, 35), 
                                 warning = c(35, 50),
                                 danger = c(50, 100)),
          symbol = "%"
          )
  })
  })
  
  observe({
    output$txt_model_health <- renderText({
      ifelse(round(rv$curr_run$mape,0) <= 35, "Model is still performing within healthy range.",
             ifelse(round(rv$curr_run$mape,0) <= 50, "Model has drifted slightly, but still appears to be performing moderately well.",
                    ifelse(round(rv$curr_run$mape,0) <= 100, "Model has drifted modestly, and retraining is recommended.", "Model has drifted a substantial amount. Deploy a new model immediately.")))
    })
  })
  
  
  
  #Time Series of Model Deployments-----
  #Observing for changes to deployment_date_range
  
  observe({
  output$plot_deployments <- renderPlotly({
    
    #Data Prep:
    dta <- data.frame(dt = as.Date(rv$mt$data_pushtime),
                      mape = rv$mt$mape, flg = rv$mt$flg)
    
    dta <- dta %>% mutate(grp = cumsum(flg))
    
    areas <- bind_rows(dta %>% mutate(type = "low", val = 35),
                       dta %>% mutate(type = "mid", val = 15),
                       dta %>% mutate(type = "high", val = 50))
    
     model_ranges <- dta %>% 
       group_by(grp) %>% 
       summarize(starts = first(dt), ends = last(dt), color = first(grp %% 2 + 1)) %>% 
       mutate(col = paste("v",as.character(grp),sep="")) %>% 
    #   mutate(col = ifelse(starts == "2020-12-21","Christmas Holiday Model",col)) %>% 
       mutate(col = factor(col, levels = unique(col), ordered = T))
    
    print(str(model_ranges$col))
    dta <- dta %>% mutate(alp = case_when(
      flg == 1 ~ 1.1 ,
      flg == 0 ~ 1.09
    ))
    #print('hello')
    #print(str(dta))
    #Plot:
    p <- ggplot(dta)+#, aes(x = dt, y = mae))+
      #geom_area(data = areas, aes(color = type, fill = type))+
      geom_rect(data = areas, aes(xmin = as.Date(input$input_deployment_date_range[1]), xmax = as.Date(input$input_deployment_date_range[2]), ymin = 0, ymax = 35),
                fill = "green", alpha = 0.1)+
      geom_rect(data = areas, aes(xmin = as.Date(input$input_deployment_date_range[1]), xmax = as.Date(input$input_deployment_date_range[2]), ymin = 35, ymax = 50),
                fill = "yellow", alpha = 0.1)+
      geom_rect(data = areas, aes(xmin = as.Date(input$input_deployment_date_range[1]), xmax = as.Date(input$input_deployment_date_range[2]), ymin = 50, ymax = 100),
                fill = "red", alpha = 0.1)
    
    for(i in unique(dta$grp)){
      p <- p + geom_line(data = dta %>% filter(grp==i), aes(x = dt, y = mape), color = "white")
    }
    
     p <- p + geom_rect(data = model_ranges, aes(xmin =starts, xmax = ends, ymin = -2, ymax =0, fill = col))+
       scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Set2"))(10)) #the 10 here is how many resulting colors
       scale_fill_manual(values = colorRampPalette(c("white", "#004d78"))(length(unique(model_ranges$col)))) #the 10 here is how many resulting colors
       scale_fill_brewer(palette = "Blues")
      
    p <- p + #geom_line(color = "white")+
      geom_point(data = dta, aes(x = dt, y = mape, alpha = alp), color = "white", size = 2.5)+
      #scale_x_date(breaks = sort(dta$dt[which(1:nrow(dta) %% 7 == 0)]), date_labels = "%b %d, %Y")+
      scale_x_date(breaks = dta$dt, date_labels = "%b %d, %Y")+
      theme(plot.background = element_rect(fill = "#00000000"),
            panel.background = element_rect(fill="#00000000"),
            panel.grid.major = element_line(colour = "#808080"),
            axis.text.x = element_text(colour = "#808080", angle = 45, hjust = 0),
            axis.text.y = element_text(colour = "#808080"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(colour = "#808080"),
            legend.position = "bottom"
      )
    
    #Final plotly rendering:
    ggplotly(p) %>%
      layout(legend = list(orientation = "h", x = 0.4, y = -.4, 
                           bgcolor = "#00000000",
                           font = list(
                            color = "#E2E2E2")))
    
    
  })
  })
  
  
  #Report Download------
  output$report_comps1 <- downloadHandler(
    filename = function() {
      #For now, keep exclusively as PDF report. 
      #if(input$comps1_pdf == "PDF"){
      if(TRUE){
        paste0("MLOps Taxis - ",Sys.Date(),".pdf")
      }else{
        paste0("[Excel Name if Functionality built out] - ",Sys.Date(),".xlsx")
      }
    },
    content = function(file) {
      if(TRUE){
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("rmd_report_templates/test.Rmd", tempReport, overwrite = TRUE)
        #file.copy("rmd_report_templates/comps_template.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(report_title = NA,
                       dt_start = as.Date(input$input_deployment_date_range[1]),
                       dt_end = as.Date(input$input_deployment_date_range[2])
        )
        #print(params)
        #print(str(params))
        
        # Knit the document, passing in the `params` list, and evaluate it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
        file.copy(paste0("rmd_report_templates/comps1.pdf"),file)
      }else{
        #[logic for Excel output]
      }
    }
  )
  
  
  #Radar Charts-----
  makeRadar <- function(rdf,dow){
    dta <- data.frame("Label" = c( paste0(c(12, 1:11)," AM"), paste0(c(12,1:11)," PM")), 
                      "MAE" = rdf)
    
    
    chartJSRadar(dta,
                 main = c("Mon","Tue","Wed","Thur","Fri","Sat","Sun")[dow],
                 showLegend = FALSE,
                 maxScale = round(max(dta$MAE),0)+1,
                 scaleStartValue = 0,
                 scaleStepWidth = 15,
                 showToolTipLabel= T,
                 colMatrix = matrix(c(255,255,255,
                                      100,100,0,
                                      0,0,100),ncol = 3),scaleLabel.fontColor="black"
    )
  }
  
  output$radar <- renderChartJSRadar({
    
    #Required changes to defaultFontColor, defaultFontSize, and backdropColor in package files:
    #Chart.js, Chart.bundle.js, Chart.min.js, and Chart.bundle.min.js (searching within the package folder for #666)
    # as these could not be configured as options in the chartJSRadar function below
  dta <- data.frame("Label" = c( paste0(c(12, 1:11)," AM"), paste0(c(12,1:11)," PM")), 
                    "MAPE" = (rv$radial_df %>% group_by(start_hr) %>% summarise(mape = 100*mean(ape)))$mape)
      
      
    chartJSRadar(dta,
                 showLegend = FALSE,
                 maxScale = max(dta$MAPE + 2),
                 scaleStartValue = 0,
                 scaleStepWidth = 10,
                 showToolTipLabel= T,
                 colMatrix = matrix(c(255,255,255,
                                      100,100,0,
                                      0,0,100),ncol = 3),scaleLabel.fontColor="black"
    )
    
    
  })
  
  output$radar_dow <- renderChartJSRadar({
    
    #Required changes to defaultFontColor, defaultFontSize, and backdropColor in package files:
    #Chart.js, Chart.bundle.js, Chart.min.js, and Chart.bundle.min.js (searching within the package folder for #666)
    # as these could not be configured as options in the chartJSRadar function below
  dta <- data.frame("Label" = c("M","T",'W',"Th","F","Sa","Su"), 
                    "MAPE" = (rv$radial_df %>% group_by(start_dow) %>% summarise(mape = mean(100*ape)))$mape)
      
      
    chartJSRadar(dta,
                 showLegend = FALSE,
                 maxScale = max(dta$MAPE + 2),
                 scaleStartValue = 0,
                 scaleStepWidth = 15,
                 showToolTipLabel= T,
                 colMatrix = matrix(c(255,255,255,
                                      100,100,0,
                                      0,0,100),ncol = 3),scaleLabel.fontColor="black"
    )
    
    
  })
  
  output$radar_mon <- renderChartJSRadar({
    makeRadar(round(100*rv$radial_df[rv$radial_df$start_dow == 'Monday',]$ape,2),1)
  })
  output$radar_tue <- renderChartJSRadar({
    makeRadar(round(100*rv$radial_df[rv$radial_df$start_dow == 'Tuesday',]$ape,2),2)
  })
  output$radar_wed <- renderChartJSRadar({
    makeRadar(round(100*rv$radial_df[rv$radial_df$start_dow == 'Wednesday',]$ape,2),3)
  })
  output$radar_thu <- renderChartJSRadar({
    makeRadar(round(100*rv$radial_df[rv$radial_df$start_dow == 'Thursday',]$ape,2),4)
  })
  output$radar_fri <- renderChartJSRadar({
    makeRadar(round(100*rv$radial_df[rv$radial_df$start_dow == 'Friday',]$ape,2),5)
  })
  output$radar_sat <- renderChartJSRadar({
    makeRadar(round(100*rv$radial_df[rv$radial_df$start_dow == 'Saturday',]$ape,2),6)
  })
  output$radar_sun <- renderChartJSRadar({
    makeRadar(round(100*rv$radial_df[rv$radial_df$start_dow == 'Sunday',]$ape,2),7)
  })
  
  
  
  
} 

shinyApp(ui, server)