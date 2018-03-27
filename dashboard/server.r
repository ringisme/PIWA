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
    DT::datatable(rearranged.data, 
                  options = list(pageLength=8, scrollX='600px'))
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