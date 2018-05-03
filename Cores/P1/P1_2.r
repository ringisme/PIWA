P1_2_Process <- function(weather.data, current.date){
  
  # Write a function to calculate the individual sum info.
  find_levels <- function(data){
    ave <- mean(as.numeric(data), na.rm=TRUE)
    std <- sd(data, na.rm=TRUE)
    max <- max(data, na.rm=TRUE)
    min <- min(data, na.rm=TRUE)
    return(round(c(ave, std, max, min), digits = 1))
  }
  
  as.data.frame(apply(weather.data[,-1], 2, find_levels)) %>%
    mutate(Type=c("Average", "SD","Maximum", "Minimum")) -> D.tb # Table for display
  # Add "today's" weather infos:
  weather.data %>%
    filter(Date == current.date) %>%
    select(-1) %>% round(, digits = 1) %>% mutate(Type = "Current") -> D.tb.cache
  D.tb <- rbind(D.tb, D.tb.cache)
  D.tb$Type <- factor(D.tb$Type, levels=c("Current","Average", "SD", "Minimum", "Maximum"))
  
  # ===== D.tb fot display in the bottom side =====
  
  # Write a function that can produce the "sigma-level" for the current value:
  sigma_level <- function(current_value, hist_col){
    if(is.na(current_value) == TRUE){
      return(NA)
    }else{
      slevel = (current_value - mean(hist_col, na.rm = TRUE))/sd(hist_col, na.rm = TRUE)
    }
    return(round(slevel, digits = 3))
  }
  
  bar.data <- as.data.frame(names(weather.data)[-1])
  names(bar.data) <- "Indicator"
  
  weather.data %>%
    filter(Date == current.date) -> current_value
  
  for(i in bar.data$Indicator){
    bar.data$level[bar.data$Indicator == i] <- 
      sigma_level(current_value[1,i], weather.data[,i])
  }
  
  positions <- unlist(bar.data$Indicator)
  
  bar.plot <- ggplot(bar.data, aes(x=Indicator, y=level)) +
    geom_bar(stat="identity", fill="#3498DB", width=0.5) +
    theme(legend.position = "none") +
    geom_hline(yintercept = 0) +
    scale_x_discrete(limits = positions)
  
  bar.plot <- ggplotly(bar.plot)
  bar.plot$elementId <- NULL
  
  return(list(D.tb, bar.plot))
}