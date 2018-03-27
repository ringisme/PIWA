source("../Cores/global.r", local=FALSE)

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
                                #choices = c("Temperature" = "TEMP",
                                # "BUI (Build-up Index)"  = "BUI",
                                #"DC (Drought Code)" = "DC",
                                # "DMC (Duff Moisture Code)" = "DMC",
                                # "DSR (Daily Severity Rating)" = "DSR",
                                # "FFMC (Fine Fuel Moisture Code)" = "FFMC",
                                #"FWI (Fire Weather Index)" = "FWI",
                                # "ISI (Initial Spread Index)" = "ISI",
                                #"Rainfall" = "RAIN",
                                # "Relative Humidity" = "REL_HUM",
                                #"Wind Direction" = "WIND_DIR",
                                # "Wind Speed" = "WIND_SPEED"
                                #),
                                choices = names(D.data)[4:length(names(D.data))],
                                selected = names(D.data)[4]
                                
                                #selected = "Temperature"
                    )
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
                                choices = names(D.data)[4:length(names(D.data))],
                                selected = names(D.data)[4])
                ),
                box(width = 3, status = "info", solidHeader = TRUE,
                    sliderInput(inputId = "p2.date.range",
                                label = "Training Date Range:",
                                sep = "",
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