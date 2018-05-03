P2_Process <- function(m1, d1, weather.data, fire.data, left.y, right.y){
  
  weather.data %>%
    filter(month(Date) == m1 & 
             day(Date) == d1 &
             year(Date) >= left.y &
             year(Date) <= right.y
             ) -> today.weather
    
  fire.data %>%
    filter(month(Date) == m1 & 
             day(Date) == d1 &
             year(Date) >= left.y &
             year(Date) <= right.y
    ) %>%
    group_by(Date) %>%
    summarise(FIRE_TIMES = n()) -> fire.details
    
  fire.details %>%
    group_by(FIRE_TIMES) %>%
    summarise(Percentage = n(), Dates = paste(Date, collapse = "<br>")) -> today.fire
  
  no_fires_days <- (right.y - left.y + 1) - sum(today.fire$Percentage)
  today.fire <- rbind(today.fire, c(0, no_fires_days, "(No need to display)"))
  
  today.fire$Percentage <- as.numeric(today.fire$Percentage)
  
  pie.chart <- plot_ly(today.fire, labels =~ FIRE_TIMES, values= ~Percentage, type='pie',
                       textinfo = 'label+percent',
                       insidetextfont = list(color = '#FFFFFF'),
                       hoverinfo = 'text',
                       text = ~paste(percent(Percentage/sum(today.fire$Percentage)),"(", Percentage,"of",sum(Percentage),")",
                                     "days have", FIRE_TIMES,"fire events in that day",
                                     "<br>",
                                     "<br>", "These days are:",
                                     "<br>",Dates
                                     ))
  pie.chart$elementId <- NULL
  
  return(list(today.weather, pie.chart))
}








