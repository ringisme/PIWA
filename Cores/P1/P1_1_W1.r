# Visual part W1:

Draw_Plot_W1 <- function(hist.bound, hist.data, v1){
  
  r.num <- length(hist.data$Date)
  
  W1.plot <- ggplot(data = hist.data, aes(Date, Value,
                                          text = paste( # Modify the hover info.
                                            "Date:", Date,
                                            "<br>", v1, ":", Value,
                                            "<br>level:", Level))) +
    geom_area(data = hist.bound,
              aes(x = Date, y = Value, fill = Level),
              alpha = 0.4, position = "identity", inherit.aes = FALSE)  +
    scale_fill_brewer(palette="Blues") +
    scale_x_date(date_labels = "%b %d", date_breaks = "1 day") +
    scale_y_continuous(name=paste(v1)) +
    guides(fill=FALSE) +
    # Add lines:
    geom_point(size=3) +
    geom_line(data = hist.data, aes(x = Date, y = Value), 
              size=0.3, inherit.aes = FALSE) +
    theme(axis.title.x =element_blank(),
          axis.ticks.x =element_blank(),
          axis.text.x=element_blank()) +
    #highlight the current day:
    geom_point(aes(x = hist.data[r.num, 1], y = hist.data[r.num, 2]), 
               colour = "red") +
    ggtitle("Historical Period")

  W1.plot <- hide_legend(ggplotly(W1.plot, tooltip = c("text")))
  W1.plot$elementId <- NULL
  return(W1.plot)
}