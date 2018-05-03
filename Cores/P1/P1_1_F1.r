Draw_Plot_F1 <- function(hist.data, r.num){
  
  F1.plot <- ggplot(data = hist.data, aes(x=Date, y=FIRE_TIMES,
                                           text = paste(
                                             "Date:", Date,
                                             "<br>Fire times:", FIRE_TIMES,
                                             "<br>( Human Caused:", HUMAN_CAUSED_TIMES,")",
                                             "<br>( Lightning Caused:", LIGHTNING_CAUSED_TIMES,")",
                                             "<br>Total Burned Size:", TOTAL_BURNED_SIZE
                                           )
  )) +
    scale_x_date(date_labels = "%b %d", date_breaks = "1 day") +
    ylab("Fire Times") +
    ylim(0, max(hist.data$FIRE_TIMES)) +
    geom_line(data = hist.data, mapping = aes(x=Date, y=FIRE_TIMES),
              size = 0.2,inherit.aes = FALSE) +
    geom_point() +
    # Highlight the current day:
    geom_point(aes(x = hist.data$Date[1+r.num], 
                   y = hist.data$FIRE_TIMES[1+r.num]), 
               colour = "red", size = 2)
  
  F1.plot <- ggplotly(F1.plot, tooltip = c("text"))
  F1.plot$elementId <- NULL
  return(F1.plot)
  
}