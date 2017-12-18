 tw.plot <- function(v1, y1, m1, d1, r.num, p.num, D.data, F.data){
    
    Vnum <- which(names(D.data) == v1)
    
    # ========================================
    # Determine the historical period dataset:
    # ========================================
    
    current.date <- as.Date(paste(y1,m1,d1), "%Y %m %d")
    hist_period <- current.date
    hist_period <- as.data.frame(hist_period)
    
    for(i in c(1:r.num)){
        hist_period[(i+1),] <- current.date %m-% days(i)
    }
    
    
    # Selet the data pending to analysis:
    DDATA <- D.data # Because SQL can only perse ALL CAPITAL LETTERS dataset
    DDATA$WX_DATE <- mdy(DDATA$WX_DATE)
    hist.data <- sqldf(
        "SELECT * FROM DDATA where
        WX_DATE in hist_period"
    )
    rm(DDATA,i,hist_period)
    
    
    # Until this step, got the *ALL* historical data.   <---
    # Want add the new fire data into that
    # by add FOUR new vars:
    # FIRE_NUM, FIRE_SIZE, HUMAN, LIGHTNING
    
    F.data$REPORT_DATE <- format(mdy_hm(F.data$REPORT_DATE), "%Y-%m-%d")
    F.data$REPORT_DATE <- as.Date(F.data$REPORT_DATE)
    # Here use SQL to achieve data filtering:
    
    for(i in 1:length(hist.data$WX_DATE)){
        hist.data$FIRE_NUM[i] <- nrow(na.omit( # Summarize Fire Numbers.
            F.data[F.data$REPORT_DATE == hist.data$WX_DATE[i],]
        ))
        
        hist.data$FIRE_SIZE[i] <- sum( # Sum Fire Burned Area.
            F.data[F.data$REPORT_DATE == hist.data$WX_DATE[i], 10],
            na.rm = TRUE
        )
        
        hist.data$HUMAN[i] <- nrow(na.omit(
            F.data[F.data$REPORT_DATE == hist.data$WX_DATE[i] & F.data$CAUSE == "Human",]
        ))
        
        hist.data$LIGHTNING[i] <- nrow(na.omit(
            F.data[F.data$REPORT_DATE == hist.data$WX_DATE[i] & F.data$CAUSE == "Lightning",]
        ))
    }
    
    hist.data <- hist.data[, c(3,Vnum,16,17,18,19)] # <-- Simplify the historical data set:
    
    # Until here we got the combined data sheet for HISTORIY.
    # called "hist.data"  <---
    
    # now we want to build the data sheet for the "future period":
    # Unlike the previous method, here I decide to save the two periods data
    # into two "different" dataframe to achieve drawing two plots in one figure.
    
    # In order to reduce calculation cost, the "future data" will only contain
    # the information of selected variable:
    
    # The future data set will contain:
    # DATE, AVE, TYPE, FIRE_NUM, FIRE_SIZE, HUMAN, LIGHTNING.
    
    future.data <- current.date
    future.data <- as.data.frame(future.data)
    
    for(i in c(1:p.num)){
        future.data[(i+1),] <- current.date %m+% days(i)
    }
    rm(i)
    
    future.data <- future.data[-1,]
    future.data <- as.data.frame(future.data)
    names(future.data)[1] <- "Date"
    
    # Selet the data pending to analysis:
    j <- 1
    for(s.date in as.character(future.data$Date)){
        m2 <- month(s.date)
        d2 <- day(s.date)
        s.data <- subset(x=D.data,
                         month(mdy(WX_DATE)) == m2 &
                             day(mdy(WX_DATE)) == d2
        )
        future.data$Ave[j] <- round(mean(s.data[,Vnum], na.rm=TRUE),
                                    digits = 1)
        f.data <- subset(x=F.data,
                         month(REPORT_DATE) == m2 & day(REPORT_DATE) == d2
        )
        # Careful, here is the principle that how to present the future data:
        # Here, we use the AVERAGE num to represent its trend:
        sub.f.data <- as.data.frame(unique(f.data$REPORT_DATE))
        names(sub.f.data)[1] <- "date"
        for(i in c(1:nrow(sub.f.data))){
            sub.f.data$FIRE_NUM[i] <- nrow(f.data[f.data$REPORT_DATE == sub.f.data$date[i], ])
            sub.f.data$FIRE_SIZE[i] <- sum(f.data[f.data$REPORT_DATE == sub.f.data$date[i], 10])
            sub.f.data$HUMAN[i] <- nrow(f.data[f.data$REPORT_DATE == sub.f.data$date[i] & f.data$CAUSE == "Human", ])
            sub.f.data$LIGHTNING[i] <- nrow(f.data[f.data$REPORT_DATE == sub.f.data$date[i] & f.data$CAUSE == "Lightning", ])
        }
        # "sub.f.data" will summarize the data
        future.data$FIRE_NUM[j] <- round(mean(sub.f.data$FIRE_NUM), digits = 1)
        future.data$FIRE_SIZE[j] <- round(mean(sub.f.data$FIRE_SIZE), digits = 1)
        future.data$HUMAN[j] <- percent(round(mean(sub.f.data$HUMAN) / mean(sub.f.data$FIRE_NUM), digits = 2))
        future.data$LIGHTNING[j] <- percent(round(mean(sub.f.data$LIGHTNING) / mean(sub.f.data$FIRE_NUM), digits = 2))
        
        j <- j+1
    }
    rm(j, s.date, s.data, m2, d2, f.data, sub.f.data,i)
    
    names(hist.data)[c(1,2)] <- c("Date","Ave")
    
    
    # Draw the low & high bounds & levels:
    
    # (1) For hist.data:
    # Create a hist.bound for making a stacked area graph:
    
    hist.bound <- hist.data$Date
    hist.bound <- as.data.frame(hist.bound)
    hist.bound <- rbind(hist.bound, hist.bound, hist.bound, hist.bound, hist.bound, hist.bound)
    names(hist.bound)[1] <- "Date"
    
    
    j <- 1
    for(s.date in as.character(hist.data$Date)){ # s.data FOR Sample Date
        m2 <- month(s.date)
        d2 <- day(s.date)
        
        s.data <- subset(x=D.data,
                         month(mdy(WX_DATE)) == m2 &
                             day(mdy(WX_DATE)) == d2)
        s.ratio <- quantile(s.data[,Vnum],
                            probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1),
                            na.rm=TRUE)
        hist.data$low_bound[j] <- s.ratio[3]
        hist.data$high_bound[j] <- s.ratio[5]
        
        # Here is for creating bounds:
        
        hist.bound$Value[j] <- s.ratio[7]
        hist.bound$Level[j] <- "Moderately high"
        
        hist.bound$Value[j+r.num+1] <- s.ratio[6]
        hist.bound$Level[j+r.num+1] <- "Slightly high"
        
        hist.bound$Value[j+2*(r.num+1)] <- s.ratio[5]
        hist.bound$Level[j+2*(r.num+1)] <- "Normal"
        
        hist.bound$Value[j+3*(r.num+1)] <- s.ratio[3]
        hist.bound$Level[j+3*(r.num+1)] <- "Slightly low"
        
        hist.bound$Value[j+4*(r.num+1)] <- s.ratio[2]
        hist.bound$Level[j+4*(r.num+1)] <- "Moderately low"
        
        hist.bound$Value[j+5*(r.num+1)] <- s.ratio[1]
        hist.bound$Level[j+5*(r.num+1)] <- "Low"
        
        hist.data$level[j] <- cut(hist.data[j,2],
                                  breaks = c(-1000, s.ratio[1]+0.0001, s.ratio[2],
                                             s.ratio[3], s.ratio[5], s.ratio[6],
                                             s.ratio[7]-0.00001, 1000))
        j <- j+1
    }
    rm(j,m2,d2,s.ratio,s.data,s.date)
    
    
    # (2) For future data:
    
    future.bound <- future.data$Date
    future.bound <- as.data.frame(future.bound)
    future.bound <- rbind(future.bound, future.bound, future.bound, future.bound, future.bound, future.bound)
    names(future.bound)[1] <- "Date"
    
    j <- 1
    for(s.date in as.character(future.data$Date)){ # s.data FOR Sample Date
        m2 <- month(s.date)
        d2 <- day(s.date)
        s.data <- subset(x=D.data,
                         month(mdy(WX_DATE)) == m2 &
                             day(mdy(WX_DATE)) == d2)
        s.ratio <- quantile(s.data[,Vnum],
                            probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1),
                            na.rm=TRUE)
        future.data$low_bound[j] <- s.ratio[3]
        future.data$high_bound[j] <- s.ratio[5]
        
        # Here is for creating bounds:
        
        future.bound$Value[j] <- s.ratio[7]
        future.bound$Level[j] <- "Moderately high"
        
        future.bound$Value[j+p.num] <- s.ratio[6]
        future.bound$Level[j+p.num] <- "Slightly high"
        
        future.bound$Value[j+2*(p.num)] <- s.ratio[5]
        future.bound$Level[j+2*(p.num)] <- "Normal"
        
        future.bound$Value[j+3*(p.num)] <- s.ratio[3]
        future.bound$Level[j+3*(p.num)] <- "Slightly low"
        
        future.bound$Value[j+4*(p.num)] <- s.ratio[2]
        future.bound$Level[j+4*(p.num)] <- "Moderately low"
        
        future.bound$Value[j+5*(p.num)] <- s.ratio[1]
        future.bound$Level[j+5*(p.num)] <- "Low"
        
        future.data$level[j] <- cut(future.data[j,2],
                                    breaks = c(-1000, s.ratio[1]+0.0001, s.ratio[2],
                                               s.ratio[3], s.ratio[5], s.ratio[6],
                                               s.ratio[7]-0.00001, 1000)
        )
        j <- j+1
    }
    rm(j,m2,d2,s.ratio,s.data,s.date)
    
    # Create the MAX bound:
    
    hist.bound.cache <- unique(hist.bound$Date)
    hist.bound.cache <- as.data.frame(hist.bound.cache)
    names(hist.bound.cache)[1] <- "Date"
    hist.bound.cache$Value <- max(hist.bound$Value, future.bound$Value) * 1.1
    hist.bound.cache$Level <- "High"
    
    hist.bound <- rbind(hist.bound, hist.bound.cache)
    rm(hist.bound.cache)
    
    # For MAX future bound:
    
    future.bound.cache <- unique(future.bound$Date)
    future.bound.cache <- as.data.frame(future.bound.cache)
    names(future.bound.cache)[1] <- "Date"
    future.bound.cache$Value <- max(hist.bound$Value)
    future.bound.cache$Level <- "High"
    
    future.bound <- rbind(future.bound, future.bound.cache)
    rm(future.bound.cache)
    
    # Change level names:
    hist.data$level <- factor(hist.data$level,
                              levels = c(1,2,3,4,5,6,7),
                              labels = c("Low","Moderately low","Slightly low",
                                         "Normal","Slightly high","Moderately high","High")
    )
    hist.data$Type <- "Real"
    hist.bound$Type <- "Real"
    
    future.data$level <- factor(future.data$level,
                                levels = c(1,2,3,4,5,6,7),
                                labels = c("Low","Moderately low","Slightly low",
                                           "Normal","Slightly high","Moderately high","High")
    )
    future.data$Type <- "Ave"
    future.bound$Type <- "Ave"
    
    
    #########################
    #     visualization     #
    #########################
    
    # Make data in orders to achieve the right stacked layers:
    
    hist.bound$Level <- ordered(hist.bound$Level, 
                                levels = c("High","Moderately high","Slightly high",
                                           "Normal",
                                           "Slightly low","Moderately low","Low"))
    future.bound$Level <- ordered(future.bound$Level, 
                                levels = c("High","Moderately high","Slightly high",
                                           "Normal",
                                           "Slightly low","Moderately low","Low"))
    
    hist.data <<- hist.data
    hist.plot <- ggplot(data = hist.data, aes(x=Date, y=Ave, 
                                              text = paste( # Modify the hover info.
                                                "Date:", Date,
                                                "<br>", v1, ":", Ave,
                                                "<br>level:", level
                                              ) 
                                              )) +
        geom_area(data = hist.bound,
                  aes(x = Date, y=Value, fill = Level),
                  #colour="black", size=.2,
                  alpha=.4, position = "identity",inherit.aes = FALSE) +
        scale_fill_brewer(palette="Blues", breaks=rev(levels(hist.bound$Level))) +
        scale_x_date(date_labels = "%b %d", date_breaks = "1 day") +
        scale_y_continuous(name=paste(v1)) +
        guides(fill=FALSE) +
        # Add lines:
        geom_point(size=3) +
        geom_line(data = hist.data, aes(x = Date, y = Ave), 
                  size=0.3, inherit.aes = FALSE) +
        theme(axis.title.x =element_blank(),
              axis.ticks.x =element_blank(),
              axis.text.x=element_blank()) +
        #highlight the current day:
        geom_point(aes(x = hist.data$Date[1+r.num], y = hist.data$Ave[1+r.num]), colour = "red") +
        # Title
        ggtitle("Historical Period")
    
    # For transfer ggplot plot to interactive plot:
    
    hist.plot <- hide_legend(ggplotly(hist.plot, tooltip = c("text"))) # Hide the first plot legend
    for(i in c(1:7)){
      hist.plot$x$data[[i]]$hoverinfo <- "none"
    }
    hist.plot$x$data[[9]]$hoverinfo <- "none"
    
    hist.plot <<- hist.plot
    
    
    # --- p4_2 ---
    
    future.data <<- future.data
    future.plot <<- ggplot(data = future.data, aes(x=Date, y=Ave,
                                                   text = paste( # Modify the hover info.
                                                       "Date:", Date,
                                                       "<br>", v1, ":", Ave,
                                                       "<br>level:", level
                                                   )
                                                   )) +
        geom_area(data = future.bound,
                  aes(x = Date, y=Value, fill = Level),
                  #colour="black", size=.2,
                  alpha=.4, position = "identity",inherit.aes = FALSE) +
        scale_fill_brewer(palette="Blues", breaks=rev(levels(future.bound$Level))) +
        scale_x_date(date_labels = "%b %d", date_breaks = "1 day") +
        scale_y_continuous(name=paste(v1)) +
        guides(fill=FALSE) +
        # Add lines:
        geom_point(size=3) +
        geom_line(data = future.data, mapping = aes(x=Date, y=Ave),
                  size = 0.3,inherit.aes = FALSE) +
        theme(axis.title.y =element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y =element_blank(),
              axis.title.x =element_blank(),
              #axis.text.x=element_blank(),
              axis.ticks.x =element_blank()) +
        ggtitle("Future Period")
    
    # Transfer to interactive plot:
    future.plot <<- ggplotly(future.plot, tooltip = c("text"))
    
    # Here is the setting for FIRE plot:
    
    F1.plot <<- ggplot(data = hist.data, aes(x=Date, y=FIRE_NUM,
                                             text = paste(
                                                 "Date:", Date,
                                                 "<br>Number of Fires:", FIRE_NUM,
                                                 "<br>( Human Caused:", HUMAN,")",
                                                 "<br>( Lightning Caused:", LIGHTNING,")",
                                                 "<br>Total Burned Size:", FIRE_SIZE
                                             )
                                             )) +
        scale_x_date(date_labels = "%b %d", date_breaks = "1 day") +
        ylab("Number of Fires") +
        ylim(0, max(hist.data$FIRE_NUM, future.data$FIRE_NUM)) +
        geom_line(data = hist.data, mapping = aes(x=Date, y=FIRE_NUM),
                  size = 0.2,inherit.aes = FALSE) +
        geom_point() +
        # Highlight the current day:
        geom_point(aes(x = hist.data$Date[1+r.num], y = hist.data$FIRE_NUM[1+r.num]), colour = "red", size = 2)
        
    F1.plot <<- ggplotly(F1.plot, tooltip = c("text"))
    
    F2.plot <<- ggplot(data = future.data, aes(x=Date, y=FIRE_NUM)) + geom_point() +
        scale_x_date(date_labels = "%b %d", date_breaks = "1 day") +
        geom_line(data = future.data, mapping = aes(x=Date, y=FIRE_NUM),
                  size = 0.2,inherit.aes = FALSE) +
        theme(axis.title.y =element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y =element_blank()) +
        ylim(0, max(hist.data$FIRE_NUM, future.data$FIRE_NUM))
    
    #grid.arrange(hist.plot, future.plot, F1.plot, F2.plot,
                 #ncol=2, nrow=2, heights=c(3,1))
    
}















