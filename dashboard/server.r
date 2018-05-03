server <- function(input, output) {

  # == == == == == == == =
  #  Dynamic UI render  ==
  # == == == == == == == =
  
  output$set_range <- renderUI({
      var_names <- names(weather.data)[-1]
      var_num <- length(var_names)
      
      lapply(1:var_num, function(i){
        checkbox <- checkboxInput(inputId = paste0("ext_", i),
                                  label = paste("Use the exact value of", var_names[i]),
                                  value = FALSE)
        
        slider <- conditionalPanel(
          condition = paste0("input.", paste0("ext_",i), " == false"),
          sliderInput(paste0("p3_", i), 
                      label = paste("Please set the range of",var_names[i]),
                      min = min(weather.data[, i+1], na.rm = TRUE),
                      max = max(weather.data[, i+1], na.rm = TRUE),
                      value = c(min, max),
                      round = -2, 
                      sep = ""))
        
        number <- conditionalPanel(
          condition = paste0("input.", paste0("ext_",i), " == true"),
          numericInput(inputId = paste0("p3_e", i), 
                       label = paste("Please input the value of",var_names[i]),
                       value = 0,
                       min = min(weather.data[, i+1], na.rm = TRUE),
                       max = max(weather.data[, i+1], na.rm = TRUE)
          ))
        
        space <- tags$hr()

        return(list(slider, number, checkbox, space))
      })
  }) 
    
    
  # == == == == == == == =
  #  Preload Functions  ==
  # == == == == == == == =
  # +++ P1 Real Time +++
  process.P1_1 <- reactive({
    P1_1_Process(input$p1_1_var, input$p1_1_date,
            input$p1_1_rnum, input$p1_1_pnum,
            weather.data, fire.data
    )
  })
  
  process.P1_2 <- reactive({
    P1_2_Process(weather.data, input$p1_2_date)
  })
  # +++ P2 Today in History +++
  process.P2 <- eventReactive({input$p2_submit | input$p2_submit == 1},{
    P2_Process(input$p2.month, input$p2.day, weather.data, fire.data,
               input$p2.date.range[1], input$p2.date.range[2])
  })
  
  process.P2_2 <- eventReactive({input$p2_submit | input$p2_submit == 1},{
    Draw_Plot_P2(input$p2.var, process.P2()[[1]])
  })
  # +++ P3 Search Function +++

  process.P3 <- eventReactive({input$p3_submit | input$p3_submit == 1},{
      P3_Process(
          {s.list <- list()
          if(input$p3_submit == 0){
              s.list <- rep(list(NA), var_num)
          }else{
              for(i in 1:var_num){
                  if(eval(parse(text = (paste0("input$ext_",i)))) == TRUE){
                      s.list = c(s.list, list(eval(parse(text = (paste0("input$p3_e",i))))))}
                  else{
                      s.list = c(s.list, list(eval(parse(text = (paste0("input$p3_",i))))))}
          }
              }
          s.list}, weather.data, fire.data, 
          input$date.range.p3[1], input$date.range.p3[2])
  })
  
  
  
  # == == == == == == == == == == == == ==
  #            Output part:             ==  
  # == == == == == == == == == == == == ==
  
  # --- P1 Real Time ---
  
  output$p1_1_l1.plot <- renderPlotly({
    datasets.cache <- process.P1_1()
    hist.bound <- as.data.frame(datasets.cache[1])
    hist.data <- as.data.frame(datasets.cache[2])
    Draw_Plot_W1(hist.bound, hist.data, input$p1_1_var)
  })
  
  output$p1_1_r1.plot <- renderPlotly({
    datasets.cache <- process.P1_1()
    future.bound <- as.data.frame(datasets.cache[3])
    future.data <- as.data.frame(datasets.cache[4])
    Draw_Plot_W2(future.bound, future.data, input$p1_1_var)
  })
  
  output$p1_1_l2.plot <- renderPlotly({
    datasets.cache <- process.P1_1()
    hist.data <- as.data.frame(datasets.cache[2])
    Draw_Plot_F1(hist.data, input$p1_1_rnum)
  })
  
  output$p1_2_1.plot <- renderPlotly({
    process.P1_2()[[2]]
  })
  
  output$p1_2_2.tb <- DT::renderDataTable({
    DT::datatable(process.P1_2()[[1]], 
                  options = list(pageLength=8, scrollX='600px'))
  })
  
  # --- P2 Today in History ---
  output$p2.plot <- renderPlotly({
    process.P2_2()
  })
  
  output$pie.chart <- renderPlotly({
    process.P2()[[2]]
  })
  
  # --- P3 Search Function ---
  output$p3.tb <- DT::renderDataTable({
    DT::datatable(process.P3(), 
                  options = list(pageLength=8, scrollX='600px'))
  })
  
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste("filtered_data", input$filetype, sep = ".")
    },
    content = function(file){
      seq1 <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
      write.table(process.P3(), file, sep = seq1, row.names = FALSE)
    }
  )
  
}

























