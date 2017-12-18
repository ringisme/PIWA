# This r file is to create the daily trend:

daily.plot <- function(v1, m1, d1, D.data, left.y, right.y){
  
    Vnum <- which(names(D.data) == v1)
    
    #########################
    #     Data Analysis     #
    #########################
    
    # ===== Daily Report =====
    # Data Selection for Daily Report:
    P2.data <- subset(x = D.data,
                         month(mdy(WX_DATE)) == m1 &
                             day(mdy(WX_DATE)) == d1 &
                             year(mdy(WX_DATE)) >= left.y &
                             year(mdy(WX_DATE)) <= right.y
    )
    
    D.V1.ratio <- quantile(P2.data[,Vnum], 
                           probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1),
                           na.rm=TRUE)
    
    Day.D.test <- subset(x = D.data,
                         month(mdy(WX_DATE)) == m1 &
                             day(mdy(WX_DATE)) == d1 &
                             year(mdy(WX_DATE)) > right.y
    )
    
    # Add a new factor "Var_level" indicating 
    # the level of indicator.
    
    P2.data$Type <- "train"
    Day.D.test$Type <- "test"
    
    # The average level of the trainning data.
    Day.Ave <- mean(P2.data[,Vnum], na.rm=TRUE)
    
    P2.data <- rbind(P2.data, Day.D.test)
    P2.data$Var_level <- cut(P2.data[,Vnum],
                                breaks = c(-1000, D.V1.ratio[1]+0.0001, D.V1.ratio[2],
                                           D.V1.ratio[3], D.V1.ratio[5], D.V1.ratio[6],
                                           D.V1.ratio[7]-0.00001, 1000),
                                labels = c("Low","Moderately low","Slightly low",
                                           "Normal","Slightly high","Moderately high","High")
    )
    
    mean2 <<- D.V1.ratio[4]
    l2 <<- D.V1.ratio[3]
    u2 <<- D.V1.ratio[5]
    P2.data <<- P2.data
    
    # Then define the bounds:
    
    Bound.Day <- as.data.frame(P2.data$WX_YEAR)
    Bound.Day <- rbind(Bound.Day,Bound.Day,Bound.Day,Bound.Day,Bound.Day,Bound.Day,Bound.Day)
    names(Bound.Day)[1] <- "Date"
    
    Cyc <- length(unique(Bound.Day$Date))
      
    for(i in c(1:Cyc)){
      
      Bound.Day$Value[i] <- D.V1.ratio[7] * 1.1
      Bound.Day$Level[i] <- "High"
      
      Bound.Day$Value[i+Cyc] <- D.V1.ratio[7]
      Bound.Day$Level[i+Cyc] <- "Moderately high"
      
      Bound.Day$Value[i+2*Cyc] <- D.V1.ratio[6]
      Bound.Day$Level[i+2*Cyc] <- "Slightly high"
      
      Bound.Day$Value[i+3*Cyc] <- D.V1.ratio[5]
      Bound.Day$Level[i+3*Cyc] <- "Normal"
      
      Bound.Day$Value[i+4*Cyc] <- D.V1.ratio[3]
      Bound.Day$Level[i+4*Cyc] <- "Slightly low"
      
      Bound.Day$Value[i+5*Cyc] <- D.V1.ratio[2]
      Bound.Day$Level[i+5*Cyc] <- "Moderately low"
      
      Bound.Day$Value[i+6*Cyc] <- D.V1.ratio[1]
      Bound.Day$Level[i+6*Cyc] <- "Low"
    }
    rm(i,Cyc)
    
    Bound.Day$Level <- ordered(Bound.Day$Level, 
                               levels = c("High","Moderately high","Slightly high",
                                          "Normal",
                                          "Slightly low","Moderately low","Low"))
    
    names(P2.data)[2] <- "Year"
    
    #########################
    #     visualization     #
    #########################
    
    # ===== For Daily Report ===================
    # ==========================================
    p.dr <<- ggplot(data = P2.data, aes(x = Year, y = P2.data[, Vnum],
                                        text = paste(v1,":", P2.data[, Vnum],
                                                     "<br>level:", Var_level
                                                     ))) + 
      geom_area(data = Bound.Day, 
                aes(x=Date, y=Value, fill = Level),
                alpha = .5, position = "identity",
                inherit.aes = FALSE) +
      scale_fill_brewer(palette="Blues") +
      geom_point(fill="black", size = 2.8) +
      geom_line(data = P2.data, aes(x = Year, y = P2.data[, Vnum]), 
                size = 0.5, inherit.aes = FALSE) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      ylab(paste(v1))
      #+
      #geom_hline(yintercept = Day.Ave, colour = "purple", size = 1, lty = 2) +
      #geom_vline(xintercept = left.y, colour = "red", size = 1, lty = 2) +
      #geom_vline(xintercept = right.y, colour = "red", size = 1, lty = 2)
    
    # remove the extra infos displayed on the plot:
    
    new.p.dr <- ggplotly(p.dr, tooltip = c("Year", "text"))
    for(i in c(1:7)){
      new.p.dr$x$data[[i]]$hoverinfo <- "none"
    }
    new.p.dr$x$data[[9]]$hoverinfo <- "none"
    
    p2.plot <<- new.p.dr

}






