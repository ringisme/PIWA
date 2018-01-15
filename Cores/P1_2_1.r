# This R file is to create the tables for P1_2 tab:
create_table <- function(y1, m1, d1, D.data){
  
  current.day <- as.Date(paste(y1,m1,d1), "%Y %m %d")
  
  # Date Wrangling:
  # CAREFUL!! This data based on the "Today-in-history" logic:
  
  D.data %>%
    select(3:15) %>%
    mutate(Date = as.Date(WX_DATE, format = "%m/%d/%Y")) %>%
    select(14, 2:13) %>%
    filter(format(Date, "%m-%d") == format(current.day, "%m-%d")) -> P.data
  
  # Write a function to calculate the individual sum info.
  find_levels <- function(x){
    ave <- mean(as.numeric(x), na.rm=TRUE)
    max <- max(x, na.rm=TRUE)
    min <- min(x, na.rm=TRUE)
    return(round(c(ave, max, min), digits = 1))
  }
  
  as.data.frame(apply(P.data[,-1],2, find_levels)) %>%
    mutate(Type=c("Average", "Maximum", "Minimum")) -> D.tb # table for display
  
  # Find the current day's situation:
  P.data %>%
      filter(Date == current.day) %>%
      select(-1) %>%
      mutate(Type = "Current") -> D.tb.2
  
  D.tb <- rbind(D.tb, D.tb.2)
  rm(D.tb.2)
  
  D.tb$Type <- factor(D.tb$Type, levels=c("Current", "Average", "Minimum", "Maximum"))
      
  # Reorder the columns:
  D.tb %>%
      select(Type,
             TEMP,
             BUI,
             FFMC,
             DC,
             DMC,
             DSR,
             FWI,
             ISI,
             RAIN,
             REL_HUM,
             WIND_DIR,
             WIND_SPEED
             ) %>%
      arrange(Type) -> D.tb
  
  # ===== PART END FOR HISTOGRAM CHART=====
  
  # Write a function that can produce the "sigma-level" for the current value:
  sigma_level <- function(current_value, hist_col){
    if(is.na(current_value) == TRUE){
      return(NA)
    }else{
    slevel = (current_value - mean(hist_col, na.rm = TRUE))/sd(hist_col, na.rm = TRUE)
    }
    return(round(slevel, digits = 3))
  }
  
  # rearrange P.data's order:
  P.data %>%
    select(TEMP,
           BUI,
           FFMC,
           DC,
           DMC,
           DSR,
           FWI,
           ISI,
           RAIN,
           REL_HUM,
           WIND_DIR,
           WIND_SPEED,
           Date) -> P.data
  
  bar1.data <- as.data.frame(c("TEMP","BUI",
                               "FFMC","DC","DMC",
                               "DSR","FWI","ISI",
                               "RAIN","REL_HUM",
                               "WIND_DIR",
                               "WIND_SPEED"))
  names(bar1.data) <- "Indicator"
  bar1.data$Indicator <- factor(bar1.data$Indicator, levels=c("TEMP","BUI",
                                                              "FFMC","DC","DMC",
                                                              "DSR","FWI","ISI",
                                                              "RAIN","REL_HUM",
                                                              "WIND_DIR",
                                                              "WIND_SPEED"))
  
  P.data %>%
    filter(Date == current.day) -> current.data
    
  for(i in bar1.data$Indicator){
    bar1.data$Sigma_level[bar1.data$Indicator == i] <-
      sigma_level(current.data[1,i], P.data[,i])
  }
  
  for(i in 1:length(bar1.data$Indicator)){
    if(bar1.data$Sigma_level[i] >= 0 | is.na(bar1.data$Sigma_level[i]) == TRUE){
      bar1.data$pos[i] = TRUE
    } else {bar1.data$pos[i] = FALSE}
  }
  
  rm(i)
  
  # Draw the bar graph:
  bar1 <- ggplot(bar1.data, aes(x=Indicator, y=Sigma_level, fill=pos)) +
      geom_bar(stat="identity", position="identity", width=0.5) +
      theme(legend.position = "none")
  
  # Combine the table and the bar plot together:
  D.tb.p <- ggtexttable(D.tb, theme=ttheme(base_style = "mCyan",
                                 base_size = 16, tbody.style = tbody_style(face="bold")
                              ))
  
  detailed.plot <<- ggarrange(bar1, D.tb.p,
            nrow = 2,heights=c(2,1))
  
}


