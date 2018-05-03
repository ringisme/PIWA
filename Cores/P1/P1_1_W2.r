# Visual part W2:

Draw_Plot_W2 <- function(future.bound, future.data, v1){

  W2.plot <- ggplot(data = future.data, aes(Date, Value,
                                          text = paste( # Modify the hover info.
                                            "Date:", Date,
                                            "<br>", v1, ":", Value,
                                            "<br>level:", Level))) +
    geom_area(data = future.bound,
              aes(x = Date, y = Value, fill = Level),
              alpha = 0.4, position = "identity", inherit.aes = FALSE)  +
    scale_fill_brewer(palette="Blues") +
    scale_x_date(date_labels = "%b %d", date_breaks = "1 day") +
    scale_y_continuous(name=paste(v1)) +
    guides(fill=FALSE) +
    # Add lines:
    geom_point(size=3) +
    geom_line(data = future.data, aes(x = Date, y = Value), 
              size=0.3, inherit.aes = FALSE) +
    theme(axis.title.y =element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y =element_blank(),
          axis.title.x =element_blank(),
          #axis.text.x=element_blank(),
          axis.ticks.x =element_blank()) +
    ggtitle("Future Period")
  
  W2.plot <- hide_legend(ggplotly(W2.plot, tooltip = c("text")))
  W2.plot$elementId <- NULL
  return(W2.plot)
}