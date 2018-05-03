Draw_Plot_P2 <- function(v1, today.weather){
  
  today.weather %>%
    select(1, eval(v1)) -> today.weather
  
  var_ratios <- quantile(today.weather[,2], 
                        probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1),
                        na.rm=TRUE)
  
  today.weather$level <- cut(today.weather[,2],
                             breaks = c(-Inf, var_ratios[1]+0.0001, var_ratios[2],
                                        var_ratios[3], var_ratios[5], var_ratios[6],
                                        var_ratios[7]-0.0001, Inf),
                             labels = c("Low","Moderately low","Slightly low",
                                        "Normal","Slightly high","Moderately high","High"))
  # Define the bounds:
  
  bounds.weather <- as.data.frame(rep(unique(today.weather$Date),7))
  names(bounds.weather) <- "Date"
  
  num_of_years <- length(unique(today.weather$Date))
  bounds.var <- c(var_ratios[7]*1.1, var_ratios[7], var_ratios[6],
                  var_ratios[5], var_ratios[3], var_ratios[2],
                  var_ratios[1])
  
  bounds.weather$value <- unlist(lapply(bounds.var, rep, num_of_years))
  
  levels.var <- c("High","Moderately high","Slightly high",
                  "Normal",
                  "Slightly low","Moderately low","Low")
  bounds.weather$level <- unlist(lapply(levels.var, rep, num_of_years))
  
  bounds.weather$level <- ordered(bounds.weather$level, 
                                  levels = c("High","Moderately high","Slightly high",
                                             "Normal",
                                             "Slightly low","Moderately low","Low"))
  
  #########################
  #     visualization     #
  #########################
  p2.plot <- ggplot(data = today.weather, aes(x = Date, y = today.weather[,2],
                                     text = paste("Date:", Date,
                                                  "<br>",v1,":", today.weather[,2],
                                                  "<br>level:", level
                                     ))) + 
    geom_area(data = bounds.weather, 
              aes(x=Date, y=value, fill = level),
              alpha = .5, position = "identity",
              inherit.aes = FALSE) +
    scale_fill_brewer(palette="Blues") +
    geom_point(fill="black", size = 2.8) +
    geom_line(data = today.weather, aes(x = Date, y = today.weather[,2]), 
              size = 0.5, inherit.aes = FALSE) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    ylab(paste(v1))
  
  p2.plot <- ggplotly(p2.plot, tooltip = c("text"))
  p2.plot$elementId <- NULL
  
  return(p2.plot)
}
