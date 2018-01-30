# This r file is to create the daily trend:

daily.histo.plot <- function(m1, d1, F.data, left.y, right.y){
  
  # Formating the report date of FIRE INFO:
  
  F.data$REPORT_DATE <- format(mdy_hm(F.data$REPORT_DATE), "%Y-%m-%d")
  F.data$REPORT_DATE <- as.Date(F.data$REPORT_DATE)
  
  # Step I:
  # Select years from "left.y" to "right.y":
  
  p2_2.data <- as.data.frame(c(left.y : right.y))
  names(p2_2.data)[1] <- "Year"
  
  # Step II:
  # Combine the fire information into this data frame.
  today_date <- as.Date(paste(m1,d1), format = "%m %d")
  today_date <- format(today_date, format = "%m-%d")
  
  F.data %>%
      filter(format(REPORT_DATE, format = "%m-%d") == today_date) -> clean.F.data
  
  for(i in c(1 : length(p2_2.data$Year) )){
    
    p2_2.data$FIRE_NUM[i] <- nrow(na.omit( # Summarize Fire Numbers.
      clean.F.data[clean.F.data$FIRE_YEAR == p2_2.data$Year[i],]
    ))
    
    p2_2.data$FIRE_SIZE[i] <- sum( # Sum Fire Burned Area.
        clean.F.data[clean.F.data$FIRE_YEAR == p2_2.data$Year[i],10],
      na.rm = TRUE
    )
    
    p2_2.data$HUMAN[i] <- nrow(na.omit(
      clean.F.data[clean.F.data$FIRE_YEAR == p2_2.data$Year[i] & clean.F.data$CAUSE == "Human",]
    ))
    
    p2_2.data$LIGHTNING[i] <- nrow(na.omit(
      clean.F.data[clean.F.data$FIRE_YEAR == p2_2.data$Year[i] & clean.F.data$CAUSE == "Lightning",]
    ))
    
  }
  
  # Create data fot Pir Chart & Histogram:
  
  pie.data <- as.data.frame(unique(p2_2.data$FIRE_NUM))
  names(pie.data)[1] <- "Fire_Number"
  
  for(i in 1:length(pie.data$Fire_Number)){
    pie.data$Percentage[i] <- nrow(na.omit(
      p2_2.data[p2_2.data$FIRE_NUM == pie.data$Fire_Number[i],]
    ))
  }
  
  
  pie.chart <- plot_ly(pie.data, labels =~ Fire_Number, values= ~Percentage, type='pie',
                       textinfo = 'label+percent',
                       insidetextfont = list(color = '#FFFFFF'),
                       hoverinfo = 'text',
                       text = ~paste("There are", Percentage, "days")
                       )
  # To ignore the useless warning (it caused by the incompatibility between the package
  # "ggplot2" and "plotly")
  pie.chart$elementId <- NULL
  pie.chart
  
}






