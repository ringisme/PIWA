rm(list=ls())


library(lubridate) # For better manipulate Date.
library(sqldf) # Allow to use SQL syntax in R.
library(scales) # For create percentage number.
library(gridExtra) # For combine two plots together.
library(plyr)
library(dplyr)
library(shinythemes) # For more beautiful themes.
library(Cairo) # For beautiful plots.
library(plotly) # For creaste pie chart
library(ggplot2)
library(purrr) # For using "map" functions in R.
library(ggpubr)# For better table displaying


D.data <- read.csv("../../../../Ddata/DRY_Weather.csv",
                   sep = ",",
                   header = TRUE)

F.data <- read.csv("../../../../Ddata/DRY_Fires.csv",
                   sep = ",",
                   header = TRUE)
# ----------------------------------------------------

# +++++ Pre-load datas for P1, "Real Time Trends"
source("../Cores/P1.r", local=TRUE)

tw.plot("TEMP", 2012,
        07, 15,
        7,7,
        D.data, F.data)

source("../Cores/P1_2_1.r", local=TRUE)

create_table(2012, 07, 15, D.data)

# +++++ Pre-load datas for P2, "Today in history"
source("../Cores/P2.r", local=TRUE)
source("../Cores/P2_2.r", local=TRUE)

daily.plot("TEMP", 
        07, 15,
        D.data, 1969, 2010) 

# +++++ Pre-load datas for P3, "Monthly Trend"
source("../Cores/P3.r", local=TRUE)

m.plot("TEMP", 
       07, D.data)

# +++++ Pre-load datas for P4, "Search Function"
source("../Cores/P4.r", local=TRUE)

sm.search(D.data, F.data, sens = rep(1,12), FALSE,
          c(TEMP_1 = NA,
          BUI_1 = 47,
          FFMC_1 = NA,
          DC_1 = 255,
          DMC_1 = NA,
          DSR_1 = NA,
          FWI_1 = NA,
          ISI_1 = NA,
          RAIN_1 = NA,
          REL_HUM_1 = NA,
          WIND_DIR_1 = NA,
          WIND_SPEED_1 = NA)
          )

shinyApp(
  # ==========================
  # ui.R for Fire Management =
  # ==========================
  ui = navbarPage("P.I. Trends BETA",
                  theme = shinytheme("cerulean"),
                  # -- -- -- -- -
                  # Real Time --
                  # -- -- -- -- -
                  tabPanel("Real Time Trend",
                           tabsetPanel(type="tabs",
                                       # ----(1)
                                       tabPanel("Weather Info",
                                                fluidRow(
                                                    column(6,
                                                           plotlyOutput("P1_1.plot")),
                                                    column(6,
                                                           plotlyOutput("P1_2.plot"))
                                                ),
                                                fluidRow(
                                                    column(6,
                                                           plotlyOutput("P1_3.plot", height="200px")),
                                                    column(6,
                                                           #plotlyOutput("P1_4.plot", height="200px")
                                                           helpText(strong(h1("Reserve Zone")),
                                                                    p(" "),
                                                                    "waiting for prediction model"))
                                                )
                                        ),
                                       # ----(2)
                                       tabPanel("Details",
                                                fluidRow(
                                                    column(12,
                                                           plotOutput("P1_2_1.plot", height="600px")))
                                                #fluidRow(
                                                   # column(11, offset = 1,
                                                          # DT::dataTableOutput("P1_2_1.tb")))
                                        )          
                           ),
                           hr(),
                           fluidRow(
                               column(3, offset = 1,
                                      h4("Control Bar"),
                                      
                                      helpText("Show indicator's trend:",
                                               p(" "),
                                               strong("Left figure"),
                                               ": Show variable's historical information which is based on real data.",
                                               p(" "),
                                               strong("Right figure"),
                                               ": Show variable's future information which is based on average data.",
                                               p(" "),
                                               "The", strong("Red Point"),
                                               "shows the information of the current day"),
                                      
                                      submitButton("Apply Changes")
                               ),
                               column(4,
                                      dateInput("date1",
                                                label = "What's the current date?",
                                                value = "2012-07-15"),
                                      selectInput("var1",
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
                               column(4,
                                      numericInput("r.num",
                                                   label = "How many days you want to retrieve?",
                                                   value = 7),
                                      numericInput("p.num",
                                                   label = "How many days you want to preview?",
                                                   value = 7)
                               )
                           )
                   ),
                  
                  # -- -- -- -- -- -
                  # Today in hist --
                  # -- -- -- -- -- -
                  
                  tabPanel("Today in History",
                           tabsetPanel(type="tabs",
                                       tabPanel("Weather Info", plotlyOutput("p2.plot")),
                                       tabPanel("Fire Info", plotlyOutput("p2_2.plot"))
                           ), # <-- The Upper Plots
                           hr(),
                           fluidRow(
                             column(3, offset =1,
                                    h3("Control Bar"),
                                    h5("Show indicator's trend"),
                                    h5("The mean of the days is", mean2,"."),
                                    h5("The normal range is from", l2, "to", u2,"."),
                                    submitButton(text = "Apply Changes")
                             ),
                             column(3,
                                    selectInput("m2",
                                                label = "Please select the month:",
                                                choices = c("01 January" = 1,
                                                            "02 February"  = 2,
                                                            "03 March" = 3,
                                                            "04 April" = 4,
                                                            "05 May" = 5,
                                                            "06 June" = 6,
                                                            "07 July" = 7,
                                                            "08 August" = 8,
                                                            "09 September" = 9,
                                                            "10 October" = 10,
                                                            "11 November" = 11,
                                                            "12 December" = 12),
                                                selected = 7),
                                    numericInput("d2",
                                                 label = "Please select the day:",
                                                 min = 1,
                                                 max = 31,
                                                 value = 15)
                                    ),
                             column(3,
                                    selectInput("var2",
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
                                                selected = "Temperature"),
                                    dateRangeInput(inputId = "date.range",
                                                   label = "Training Date Range:",
                                                   start = "1963-05-01", end = "2010-09-11",
                                                   format = "yyyy",
                                                   min = "1963-05-01", max = "2015-09-11",
                                                   startview = "decade")
                                    )
                             )
                          ), # End of tab page 2.
                  # -- -- -- -- --
                  # Month Trend --
                  # -- -- -- -- --
                  tabPanel("Monthly Trend",
                           # Third Page here.
                           tabsetPanel(type="tabs",
                                       tabPanel("Weather Info", plotlyOutput("p3.plot")),
                                       tabPanel("Fire Info")
                           ), # <-- The Upper Plots
                           hr(),
                           fluidRow(
                               column(3, offset =1,
                                      h4("Control Bar"),
                                      
                                      helpText("Show indicator's monthly trend"),
                                      
                                      submitButton(text = "Apply Changes")
                               ),
                               column(3,
                                      selectInput("m3",
                                                  label = "Please select the month:",
                                                  choices = c("01 January" = 1,
                                                              "02 February"  = 2,
                                                              "03 March" = 3,
                                                              "04 April" = 4,
                                                              "05 May" = 5,
                                                              "06 June" = 6,
                                                              "07 July" = 7,
                                                              "08 August" = 8,
                                                              "09 September" = 9,
                                                              "10 October" = 10,
                                                              "11 November" = 11,
                                                              "12 December" = 12),
                                                  selected = 7),
                                      selectInput("var3",
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
                              )
                           )
                  ), # End of Part 3
                  # -- -- -- -- -- -- -
                  # Search Function --
                  # -- -- -- -- -- -- -
                  
                  tabPanel("Search Function",
                           fluidRow(
                             column(12, DT::dataTableOutput("p4.tb"))
                           ),
                           hr(),
                           fluidRow(column(2, 
                                           fluidRow(
                                             column(6, numericInput("TEMP_1", "TEMP", value = NA)),
                                             column(6, sliderInput("sens1", HTML('<p style="color:#3498DB; font-size:70%;">Sens of TEMP</p>'), min=0, max=3, value=1, step=0.1))),
                                           fluidRow(
                                             column(6, numericInput("BUI_1", "BUI", value = 47)),
                                             column(6, sliderInput("sens2", HTML('<p style="color:#3498DB; font-size:70%;">Sens of BUI</p>'), min=0, max=3, value=1, step=0.1))
                                           )),
                                    column(2, 
                                           fluidRow(
                                             column(6, numericInput("FFMC_1", "FFMC", value = NA)),
                                             column(6, sliderInput("sens3", HTML('<p style="color:#3498DB; font-size:70%;">Sens of FFMC</p>'), min=0, max=3, value=1, step=0.1))),
                                           fluidRow(
                                             column(6, numericInput("DC_1", "DC", value = 255)),
                                             column(6, sliderInput("sens4", HTML('<p style="color:#3498DB; font-size:70%;">Sens of DC</p>'), min=0, max=3, value=1, step=0.1))
                                           )),
                                    column(2, 
                                           fluidRow(
                                             column(6, numericInput("DMC_1", "DMC", value = NA)),
                                             column(6, sliderInput("sens5", HTML('<p style="color:#3498DB; font-size:70%;">Sens of DMC</p>'), min=0, max=3, value=1, step=0.1))),
                                           fluidRow(
                                             column(6, numericInput("DSR_1", "DSR", value = NA)),
                                             column(6, sliderInput("sens6", HTML('<p style="color:#3498DB; font-size:70%;">Sens of DSR</p>'), min=0, max=3, value=1, step=0.1))
                                           )),
                                    column(2, 
                                           fluidRow(
                                             column(6, numericInput("FWI_1", "FWI", value = NA)),
                                             column(6, sliderInput("sens7", HTML('<p style="color:#3498DB; font-size:70%;">Sens of FWI</p>'), min=0, max=3, value=1, step=0.1))),
                                           fluidRow(
                                             column(6, numericInput("ISI_1", "ISI", value = NA)),
                                             column(6, sliderInput("sens8", HTML('<p style="color:#3498DB; font-size:70%;">Sens of ISI</p>'), min=0, max=3, value=1, step=0.1))
                                           )),
                                    column(2, 
                                           fluidRow(
                                             column(6, numericInput("RAIN_1", "RAIN", value = NA)),
                                             column(6, sliderInput("sens9", HTML('<p style="color:#3498DB; font-size:70%;">Sens of RAIN</p>'), min=0, max=3, value=1, step=0.1))),
                                           fluidRow(
                                             column(6, numericInput("REL_HUM_1", "REL_HUM", value = NA)),
                                             column(6, sliderInput("sens10", HTML('<p style="color:#3498DB; font-size:70%;">Sens of REL_HUM</p>'), min=0, max=3, value=1, step=0.1))
                                           )),
                                    column(2, 
                                           fluidRow(
                                             column(6, numericInput("WIND_DIR_1", "WIND_DIR", value = NA)),
                                             column(6, sliderInput("sens11", HTML('<p style="color:#3498DB; font-size:70%;">Sens of WIND_DIR</p>'), min=0, max=3, value=1, step=0.1))),
                                           fluidRow(
                                             column(6, numericInput("WIND_SPEED_1", "WIND_SPEED", value = NA)),
                                             column(6, sliderInput("sens12", HTML('<p style="color:#3498DB; font-size:70%;">Sens of WIND_SPEED</p>'), min=0, max=3, value=1, step=0.1))
                                           ))
                           ),
                           fluidRow(column(2, offset = 4, checkboxInput("set_all", "Set all sensitivities as 1",
                                                             value = FALSE),
                                            submitButton()),
                                    column(2, 
                                           radioButtons("filetype", "File type:",
                                                        choices = c("csv", "tsv")),
                                           downloadButton("downloadTable", "Download Table")))
                                    
                                    
                  )
                  # =========
       ), # End of UI part.
  
  # ==================
  # = Server section =
  # ==================
  
  server = function(input, output){
    
    # == == == == == == == =
    #  Preload Functions  ==
    # == == == == == == == =
    
    # +++ P1 Real Time +++
     process.P1 <- reactive({
         tw.plot(
             input$var1, year(input$date1),
             month(input$date1), day(input$date1),
             input$r.num, input$p.num,
             D.data, F.data
         )
     })
     
     process.P1_2 <- reactive({
       create_table(
         year(input$date1), month(input$date1), day(input$date1), D.data
       )
     })
      
    # +++ P2 Today in History +++
    process.P2 <- reactive({
      daily.plot(input$var2, 
              as.numeric(input$m2), input$d2,
              D.data, year(input$date.range[1]), year(input$date.range[2]))
    })
    
    process.P2_2 <- reactive({
      daily.histo.plot(input$m2, input$d2,
                       F.data, year(input$date.range[1]), year(input$date.range[2]))
    })
    
    # +++ P3 Monthly Trend +++
    process.P3 <- reactive({
        m.plot(
            input$var3, as.numeric(input$m3), D.data
        )
    })
    
    # +++ P4Search Function +++
    process.P4 <- reactive({
      sm.search(D.data, F.data, 
                c(input$sens1, input$sens2,
                  input$sens3, input$sens4,
                  input$sens5, input$sens6,
                  input$sens7, input$sens8,
                  input$sens9, input$sens10,
                  input$sens11, input$sens12),
                input$set_all,
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
    
    output$P1_1.plot <- renderPlotly({
        process.P1()
        hist.plot
    })
    
    output$P1_2.plot <- renderPlotly({
        process.P1()
        ggplotly(future.plot)
    })
    
    output$P1_3.plot <- renderPlotly({
        process.P1()
        ggplotly(F1.plot)
    })
    
    output$P1_4.plot <- renderPlotly({
        process.P1()
        ggplotly(F2.plot)
    })
    
    #output$P1_2_1.tb <- DT::renderDataTable({
    #  process.P1_2()
    #  D.tb
    #})
    
    output$P1_2_1.plot <- renderPlot({
      process.P1_2()
      detailed.plot
    })

    # --- P2 Today in History ---
    output$p2.plot <- renderPlotly({
      process.P2()
      ggplotly(p2.plot,
               tooltip = c("x","text"))
      })
    
    
    output$p2_2.plot <- renderPlotly({
      process.P2_2()
      pie.chart
    })
    
    
    # --- P3 Monthly Trend ---
    output$p3.plot <- renderPlotly({
        process.P3()
    })
    
    # --- P4 Search Function ---
    output$p4.tb <- DT::renderDataTable({
      process.P4()
      DT::datatable(prepared.data)
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
    
    # END of Output Setting.
    }
  )