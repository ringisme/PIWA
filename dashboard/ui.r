if(DdataAddress == "samples"){
    source("../Cores/samples.r", local=FALSE)
}else{
    source("../Cores/global.r", local=FALSE)}


# ============
# ui Section =
# ============
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "P.I.Trends BETA"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Intorduction", tabName = "P0"),
      menuItem("Real Time Trend",
               menuSubItem("Contrast Trend", tabName = "P1_CT"),
               menuSubItem("Individual Trend", tabName = "P1_IT")
      ),
      menuItem("Today in History", tabName = "P2"),
      menuItem("Search Function", tabName = "P3")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "P0",
              fluidRow(
                box(width = 12, status = "primary",
                    includeMarkdown("../dashboard/markdown_text/P0.md"))
              )
              ),
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
                    plotlyOutput("p1_1_l2.plot", height = "200px"),
                    hr(),
                    fluidRow(
                      box(width = 6, status = "info", 
                          solidHeader = TRUE, 
                          dateInput("p1_1_date",
                                    label = "What's the current date?",
                                    value = weather.data$Date[(2*length(weather.data$Date)/3)-9]),
                          selectInput("p1_1_var",
                                      label = "Choose a variale to display",
                                      choices = names(weather.data)[2:length(names(weather.data))],
                                      selected = names(weather.data)[2]
                          )
                      ),
                      box(width = 6, status = "info",
                          solidHeader = TRUE,
                          numericInput("p1_1_rnum",
                                       label = "How many days you want to retrieve?",
                                       value = 7),
                          numericInput("p1_1_pnum",
                                       label = "How many days you want to preview?",
                                       value = 7)
                    ))),
                box(width = 6, status = "primary",
                    includeMarkdown("../dashboard/markdown_text/P1_1.md"))              
              )
      ), # P1_1 end.
      
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
                    dateInput("p1_2_date", 
                              value = weather.data$Date[(2*length(weather.data$Date)/3)-9], 
                              # This value just for display better,
                              # it Should be `sys.time` in the real applicaiton.
                              label = "Please input the current date:")
                ),
                box(width = 9, status = "primary",
                    includeMarkdown("../dashboard/markdown_text/P1_2.md")
                    )
              )
      ), # P1_2 end.
      
      tabItem(tabName = "P2",
              fluidRow(
                tabBox(width = 12, 
                       tabPanel("Weather Info", plotlyOutput("p2.plot"), hr()),
                       tabPanel("Fire Events Info", plotlyOutput("pie.chart"), hr())
                )
              ),
              fluidRow(
                box(width = 3, status = "info", # title = "Please select date:",
                    solidHeader = TRUE, 
                    fluidRow(
                      box(width = 6, solidHeader = TRUE,
                          selectInput("p2.month",
                                      label = "Month:", 
                                      choices = c("January" = 1,
                                        "February"  = 2,
                                        "March" = 3,
                                        "April" = 4,
                                        "May" = 5,
                                        "June" = 6,
                                        "July" = 7,
                                        "August" = 8,
                                        "September" = 9,
                                        "October" = 10,
                                        "November" = 11,
                                        "December" = 12
                                      ),
                                      selected = month(weather.data$Date[(2*length(weather.data$Date)/3)-9]) )),
                      box(width = 6, solidHeader = TRUE,
                          numericInput("p2.day",
                                       label = "Day:",
                                       min = 1,
                                       max = 31,
                                       value = day(weather.data$Date[(2*length(weather.data$Date)/3)-9]) ))
                    ),
                    selectInput("p2.var",
                                label = "Choose a variale to display",
                                choices = names(weather.data)[2:length(names(weather.data))],
                                selected = names(weather.data)[2]
                                )
                ),
                box(width = 3, status = "info", solidHeader = TRUE,
                    sliderInput(inputId = "p2.date.range",
                                label = "Date Range:",
                                sep = "",
                                min = min(year(weather.data$Date), na.rm=TRUE), 
                                max = max(year(weather.data$Date), na.rm=TRUE),
                                value = c(min, max)),
                    actionButton("p2_submit", "Submit")
                ),
                box(width = 6, status = "primary",
                    includeMarkdown("../dashboard/markdown_text/P2.md")
                    )
              )
      ),# P2 end
      
      tabItem(tabName = "P3",
              fluidRow(
                  box(width = 2, status = "info",
                      solidHeader = TRUE,
                      uiOutput("set_range")
                      ),
                  box(width = 10,
                      div(DT::dataTableOutput("p3.tb"), style = "font-size: 100%; width: 99%"),
                      fluidRow(
                        box(width = 3, status = "info", solidHeader = TRUE,
                            dateRangeInput(
                              inputId = "date.range.p3",
                              label = "Please select the date range:",
                              start = weather.data$Date[1], 
                              end = weather.data$Date[length(weather.data$Date)],
                              format = "yyyy-mm-dd",
                              min = weather.data$Date[1], 
                              max = weather.data$Date[length(weather.data$Date)],
                              startview = "decade", width = "80%"
                            ), hr(),
                            actionButton("p3_submit", "Submit")
                            ),
                        box(width = 3, status = "info", solidHeader = TRUE,
                            radioButtons("filetype", "File type:",
                                         choices = c("csv", "tsv")),
                            downloadButton("downloadTable", "Download Table")
                            ),
                        box(width = 6, status = "primary",
                            includeMarkdown("../dashboard/markdown_text/P3.md")
                            )
                      )
                  )
              )
      )
    ) # +++ End TabItems Part +++
  )# --- End Body Part ---
)# ==== End UI part ====





















