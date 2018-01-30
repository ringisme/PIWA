rm(list=ls())


library(lubridate) # For better manipulate Date.
library(sqldf) # Allow to use SQL syntax in R.
library(scales) # For create percentage number.
library(gridExtra) # For combine two plots together.
library(plyr)
library(dplyr)
library(plotly) # For creaste pie chart
library(ggplot2)
library(purrr) # For using "map" functions in R.
library(ggpubr)# For better table displaying
library(shinydashboard)# For dashboard layout

D.data <- read.csv(paste0(DdataAddress,"/DRY_Weather.csv"),
                   sep = ",",
                   header = TRUE)

F.data <- read.csv(paste0(DdataAddress,"/DRY_Fires.csv"),
                   sep = ",",
                   header = TRUE)
# ----------------------------------------------------
# +++++ Pre-load datas for P1, "Real Time Trend" 
source("../Cores/P1_1.r")
source("../Cores/P1_2.r", local=TRUE)

# +++++ Pre-load datas for P2, "Today in history"
source("../Cores/P2_1.r", local=TRUE)
source("../Cores/P2_2.r", local=TRUE)

# +++++ Pre-load datas for P4, "Search Function"
source("../Cores/P4.r", local=TRUE)

# ============
# ui Section =
# ============
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "P.I.Trends BETA"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Real Time Trend",
               menuSubItem("Contrast Trend", tabName = "P1_CT"),
               menuSubItem("Individual Trend", tabName = "P1_IT")
      ),
      menuItem("Today in History", tabName = "Today_in_History"),
      menuItem("Search Function", tabName = "Search_Function")
    )
  ),
  dashboardBody(
   tabItems(
     # === Display Space fot P1 ===
     tabItem(tabName = "P1_CT",
             fluidRow(
                 box(width = 6, status = "primary",
                     plotlyOutput("p1_1_l1.plot", height = "350px")),
                 box(width = 6, status = "primary",
                     plotlyOutput("p1_1_r1.plot", height = "350px"))
              ),
             fluidRow(
                 box(width = 6, status = "primary",
                     plotlyOutput("p1_1_l2.plot", height = "200px"))
             ),
             fluidRow(
                 box(width = 3, status = "info", 
                     solidHeader = TRUE, 
                     dateInput("p1_1_date",
                               label = "What's the current date?",
                               value = "2012-07-15"),
                     selectInput("p1_1_var",
                                 label = "Choose a variale to display",
                                 choices = c("Temperature" = "TEMP",
                                             "BUI (Build-up Index)"  = "BUI",
                                             "DC (Drought Code)" = "DC",
                                             "DMC (Duff Moisture Code)" = "DMC",
                                             "DSR (Daily Severity Rating)" = "DSR",
                                             "FFMC (Fine Fuel Moisture Code)" = "FFMC",
                                             "FWI (Fire Weather Index)" = "FWI",
                                             "ISI (Initial Spread Index)" = "ISI",
                                             "Rainfall" = "RAIN",
                                             "Relative Humidity" = "REL_HUM",
                                             "Wind Direction" = "WIND_DIR",
                                             "Wind Speed" = "WIND_SPEED"
                                 ),
                                 selected = "Temperature")
                 ),
                 box(width = 3, status = "info",
                     solidHeader = TRUE,
                     numericInput("p1_1_rnum",
                                  label = "How many days you want to retrieve?",
                                  value = 7),
                     numericInput("p1_1_pnum",
                                  label = "How many days you want to preview?",
                                  value = 7)
                 )
            )
     ),
       
     tabItem(tabName = "P1_IT",
             fluidRow(
                 box(width = 12,
                     div(plotlyOutput("p1_2_1.plot"), style = "font-size: 100%; width: 99%")
                 )),
             fluidRow(
                 box(width = 12,
                     div(DT::dataTableOutput("p1_2_2.tb"), style = "width: 99%")
                 )
             ),
             fluidRow(
                 box(width = 3, status = "info",
                     collapsible = TRUE,
                     dateInput("p1_2_date", value = "2012-07-15", label = "Please input the current date:")
                 )
             )
     ),
       
     # === Display Space for P2 ===
     tabItem(tabName = "Today_in_History",
             fluidRow(
                 tabBox(width = 12, 
                     tabPanel("Weather Info", plotlyOutput("p2.plot"), hr()),
                     tabPanel("Fire Events Info", plotlyOutput("p2_2.plot"), hr())
                 )
             ),
             fluidRow(
                 box(width = 3, status = "info", # title = "Please select date:",
                     solidHeader = TRUE, 
                     fluidRow(
                         box(width = 6, solidHeader = TRUE,
                             selectInput("p2.month",
                                         label = "Month:", 
                                         choices = c(#"01 January" = 1,
                                             #"02 February"  = 2,
                                             #"03 March" = 3,
                                             "04 April" = 4,
                                             "05 May" = 5,
                                             "06 June" = 6,
                                             "07 July" = 7,
                                             "08 August" = 8,
                                             "09 September" = 9,
                                             "10 October" = 10,
                                             "11 November" = 11
                                             #"12 December" = 12
                                         ),
                                         selected = 7)),
                         box(width = 6, solidHeader = TRUE,
                             numericInput("p2.day",
                                          label = "Day:",
                                          min = 1,
                                          max = 31,
                                          value = 15))
                     ),
                     selectInput("p2.var",
                                 label = "Choose a variale to display",
                                 choices = c("Temperature" = "TEMP",
                                             "BUI (Build-up Index)"  = "BUI",
                                             "DC (Drought Code)" = "DC",
                                             "DMC (Duff Moisture Code)" = "DMC",
                                             "DSR (Daily Severity Rating)" = "DSR",
                                             "FFMC (Fine Fuel Moisture Code)" = "FFMC",
                                             "FWI (Fire Weather Index)" = "FWI",
                                             "ISI (Initial Spread Index)" = "ISI",
                                             "Rainfall" = "RAIN",
                                             "Relative Humidity" = "REL_HUM",
                                             "Wind Direction" = "WIND_DIR",
                                             "Wind Speed" = "WIND_SPEED"),
                                 selected = "Temperature")
                 ),
                 box(width = 3, status = "info", solidHeader = TRUE,
                     sliderInput(inputId = "p2.date.range",
                                 label = "Training Date Range:",
                                 min = 1963, max = 2015,
                                 value = c(1969, 2010)),
                     actionButton("p2_submit", "Submit")
                 )
            )
     ),
   # === Display Space for P4 ===
   tabItem(tabName = "Search_Function",
           fluidRow(
               div(DT::dataTableOutput("p4.tb"), style = "font-size: 100%; width: 99%")
           ),
           fluidRow(box(width=2, status = "info", solidHeader = TRUE, numericInput("TEMP_1", "TEMP", value = NA),
                        conditionalPanel(
                            condition = "input.set_all == false",
                            sliderInput("sens1", HTML('<p style="color:#3498DB; font-size:70%;">Sens of TEMP</p>'), 
                                        min=0, max=3, value=1, step=0.1)
                        ),
                        numericInput("FWI_1", "FWI", value = NA),
                        conditionalPanel(
                            condition = "input.set_all == false",
                            sliderInput("sens7", HTML('<p style="color:#3498DB; font-size:70%;">Sens of FWI</p>'), 
                                        min=0, max=3, value=1, step=0.1)
                        )
                    ),
                    box(width=2, status = "info", solidHeader = TRUE, numericInput("BUI_1", "BUI", value = 47),
                        conditionalPanel(
                            condition = "input.set_all == false",
                            sliderInput("sens2", HTML('<p style="color:#3498DB; font-size:70%;">Sens of BUI</p>'), 
                                        min=0, max=3, value=1, step=0.1)
                        ),
                        numericInput("ISI_1", "ISI", value = NA),
                        conditionalPanel(
                            condition = "input.set_all == false",
                            sliderInput("sens8", HTML('<p style="color:#3498DB; font-size:70%;">Sens of ISI</p>'), 
                                        min=0, max=3, value=1, step=0.1)
                        )
                    ),
                    box(width=2, status = "info", solidHeader = TRUE, numericInput("FFMC_1", "FFMC", value = NA),
                        conditionalPanel(
                            condition = "input.set_all == false",
                            sliderInput("sens3", HTML('<p style="color:#3498DB; font-size:70%;">Sens of FFMC</p>'), 
                                        min=0, max=3, value=1, step=0.1)
                        ),
                        numericInput("RAIN_1", "RAIN", value = NA),
                        conditionalPanel(
                            condition = "input.set_all == false",
                            sliderInput("sens9", HTML('<p style="color:#3498DB; font-size:70%;">Sens of RAIN</p>'), 
                                        min=0, max=3, value=1, step=0.1)
                        )
                    ),
                    box(width=2, status = "info", solidHeader = TRUE, numericInput("DC_1", "DC", value = 255),
                        conditionalPanel(
                            condition = "input.set_all == false",
                            sliderInput("sens4", HTML('<p style="color:#3498DB; font-size:70%;">Sens of DC</p>'), 
                                        min=0, max=3, value=1, step=0.1)
                        ),
                        numericInput("REL_HUM_1", "REL_HUM", value = NA),
                        conditionalPanel(
                            condition = "input.set_all == false",
                            sliderInput("sens10", HTML('<p style="color:#3498DB; font-size:70%;">Sens of REL_HUM</p>'), 
                                        min=0, max=3, value=1, step=0.1)
                        )
                    ),
                    box(width=2, status = "info", solidHeader = TRUE, numericInput("DMC_1", "DMC", value = NA),
                        conditionalPanel(
                            condition = "input.set_all == false",
                            sliderInput("sens5", HTML('<p style="color:#3498DB; font-size:70%;">Sens of DMC</p>'), 
                                        min=0, max=3, value=1, step=0.1)
                        ),
                        numericInput("WIND_DIR_1", "WIND_DIR", value = NA),
                        conditionalPanel(
                            condition = "input.set_all == false",
                            sliderInput("sens11", HTML('<p style="color:#3498DB; font-size:70%;">Sens of WIND_DIR</p>'), 
                                        min=0, max=3, value=1, step=0.1)
                        )
                    ),
                    box(width=2, status = "info", solidHeader = TRUE, numericInput("DSR_1", "DSR", value = NA),
                        conditionalPanel(
                            condition = "input.set_all == false",
                            sliderInput("sens6", HTML('<p style="color:#3498DB; font-size:70%;">Sens of DSR</p>'), 
                                        min=0, max=3, value=1, step=0.1)
                        ),
                        numericInput("WIND_SPEED_1", "WIND_SPEED", value = NA),
                        conditionalPanel(
                            condition = "input.set_all == false",
                            sliderInput("sens12", HTML('<p style="color:#3498DB; font-size:70%;">Sens of WIND_SPEED</p>'), 
                                        min=0, max=3, value=1, step=0.1)
                        )
                    )
           ),
           fluidRow(
               box(width=2,status = "info", solidHeader = TRUE, 
                   radioButtons("filetype", "File type:",
                                choices = c("csv", "tsv")),
                   downloadButton("downloadTable", "Download Table")
               ),
               box(width=2, status = "info", solidHeader = TRUE, 
                   dateRangeInput(inputId = "date.range.p4",
                                  label = "Please select the date range:",
                                  start = "1963-05-01", end = "2015-09-11",
                                  format = "yyyy-mm-dd",
                                  min = "1963-05-01", max = "2015-09-11",
                                  startview = "decade", width = "80%")
               ),
               box(width=2,status = "info", solidHeader = TRUE, 
                   checkboxInput("set_all", "Set all sensitivities as 1",
                                 value = TRUE), hr(),
                   actionButton("p4_submit", "Submit")
               )
           )
   )
  ) # +++ End TabItems Part +++
 )# --- End Body Part ---
)# ==== End UI part ====

# ==================
# = Server section =
# ==================

server <- function(input, output) {
  # == == == == == == == =
  #  Preload Functions  ==
  # == == == == == == == =
  # +++ P1 Real Time +++
  process.P1_1 <- reactive({
      tw.plot(input$p1_1_var, input$p1_1_date,
              input$p1_1_rnum, input$p1_1_pnum,
              D.data, F.data
      )
  })
    
  process.P1_2 <- reactive({
      create_table(input$p1_2_date, D.data)
  })
    
  # +++ P2 Today in History +++
  process.P2 <- eventReactive({input$p2_submit | input$p2_submit == 1},{
    daily.plot(input$p2.var, 
               as.numeric(input$p2.month), input$p2.day,
               D.data, input$p2.date.range[1], input$p2.date.range[2])
  })
  
  process.P2_2 <- eventReactive({input$p2_submit | input$p2_submit == 1},{
    daily.histo.plot(input$p2.month, input$p2.day,
                     F.data, input$p2.date.range[1], input$p2.date.range[2])
  })
  # +++ P4Search Function +++
  process.P4 <- eventReactive({input$p4_submit | input$p4_submit == 1},{
      sm.search(D.data, F.data, 
                c(input$sens1, input$sens2,
                  input$sens3, input$sens4,
                  input$sens5, input$sens6,
                  input$sens7, input$sens8,
                  input$sens9, input$sens10,
                  input$sens11, input$sens12),
                input$set_all,
                input$date.range.p4[1], input$date.range.p4[2],
                c(input$TEMP_1,
                  input$BUI_1,
                  input$FFMC_1,
                  input$DC_1,
                  input$DMC_1,
                  input$DSR_1,
                  input$FWI_1,
                  input$ISI_1,
                  input$RAIN_1,
                  input$REL_HUM_1,
                  input$WIND_DIR_1,
                  input$WIND_SPEED_1)
      )
  })
  # == == == == == == == == == == == == ==
  #            Output part:             ==  
  # == == == == == == == == == == == == ==
  # --- P1 Real Time ---
  output$p1_1_l1.plot <- renderPlotly({
      process.P1_1()
      hist.plot
  })
  
  output$p1_1_r1.plot <- renderPlotly({
      process.P1_1()
      future.plot
  })
  
  output$p1_1_l2.plot <- renderPlotly({
      process.P1_1()
      F1.plot
  })
  
  output$p1_2_1.plot <- renderPlotly({
      process.P1_2()
  })
  
  output$p1_2_2.tb <- DT::renderDataTable({
      process.P1_2()
      DT::datatable(D.tb, options = list(pageLength=8, scrollX='600px'))
  })
  
  # --- P2 Today in History ---
  output$p2.plot <- renderPlotly({
    process.P2()
  })
  
  
  output$p2_2.plot <- renderPlotly({
    process.P2_2()
  })
  # --- P4 Search Function ---
  output$p4.tb <- DT::renderDataTable({
    process.P4()
  })
    
  # allow user to download filtered table:
  output$downloadTable <- downloadHandler(
      filename = function() {
          paste("filtered_data", input$filetype, sep = ".")
      },
      content = function(file){
          seq1 <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
          write.table(process.P4(), file, sep = seq1, row.names = FALSE)
      }
  )
  
}

shinyApp(ui, server)
