 P1_1_Process <- function(v1, current.date, r.num, p.num, weather.data, fire.data){
    
    v_num <- which(names(weather.data) == v1)
   
    # ==============================================
    # Determine the basic historical period dataset:
    # ==============================================
    hist_period <- current.date
    
    for(i in c(1:r.num)){
      hist_period <- append(hist_period, current.date %m-% days(i))
    }
    
    # Find the hist bound:
    FindBound <- function(x, pos){
      quantile(x, probs = pos, na.rm = TRUE)
    }
    
    weather.data %>%
      filter(format(Date, "%m-%d") %in% format(hist_period, "%m-%d")) %>% 
      select(1, v_num) %>% mutate_at(1, format, "%m-%d") %>% 
      group_by(Date) %>% summarise_at(vars(1), 
                                      funs(MH = FindBound(., 1),
                                           SH = FindBound(., 0.9),
                                           N = FindBound(., 0.75),
                                           SL = FindBound(., 0.25),
                                           ML = FindBound(., 0.1),
                                           L = FindBound(., 0)
                                           )) -> hist.bound.raw
    # Rearrange its form for the convienience of layer painting.
    hist.bound <- data.frame()
    for(i in c(1:6)){
      hist.bound.raw %>% select(1, i+1) -> hist.bound.cache
      hist.bound.cache$Level <- names(hist.bound.raw)[i+1]
      names(hist.bound.cache)[2] <- v1
      hist.bound <- rbind(hist.bound, hist.bound.cache)
    }
    DateReform <- function(x, year){
      as.Date(
        paste(year,"-",x, sep="")
      )
    }
    
    hist.bound %>%
      mutate_at(1, DateReform, year = year(current.date)) -> hist.bound
    
    rm(hist.bound.cache, hist.bound.raw)
    
    # Hist Bound calculating part end.
    
    weather.data %>%
      filter(Date %in% hist_period) -> hist.data
    
    # Until this step, got the *ALL* historical data.
    # Want add the new fire data into that by add FOUR new vars:
    # FIRE_TIMES, TOTAL_BURNED_SIZE, HUMAN_CAUSED_TIMES, LIGHTNING_CAUSED_TIMES
    
    for(i in 1:length(hist.data$Date)){
        hist.data$FIRE_TIMES[i] <- nrow(na.omit( # Summarize Fire Times.
            fire.data[fire.data$Date == hist.data$Date[i],]
        ))
        hist.data$TOTAL_BURNED_SIZE[i] <- sum( # Sum Fire Total Burned Area.
            fire.data[fire.data$Date == hist.data$Date[i], 2],
            na.rm = TRUE
        )
        hist.data$HUMAN_CAUSED_TIMES[i] <- nrow(na.omit(
            fire.data[fire.data$Date == hist.data$Date[i] & fire.data[,3] == "Human",]
        ))
        hist.data$LIGHTNING_CAUSED_TIMES[i] <- nrow(na.omit(
            fire.data[fire.data$Date == hist.data$Date[i] & fire.data[,3] == "Lightning",]
        ))
    }
    
    # ===========================================
    # Determine the basic future period dataset:
    # ===========================================
    
    # now we want to build the data sheet for the "future period":
    # Unlike the previous method which used the real values,
    # the future data will use the AVERAGE values as its showing data.
    
    future_period <- current.date + 1
    
    for(i in c(1 : (p.num-1))){
      future_period <- append(future_period, current.date %m+% days(i+1))
    }
    
    # Find the bounds for the future period:
    weather.data %>%
      filter(format(Date, "%m-%d") %in% format(future_period, "%m-%d")) %>% 
      select(1, v_num) %>% mutate_at(1, format, "%m-%d") %>% 
      group_by(Date) %>% summarise_at(vars(1), 
                                      funs(MH = FindBound(., 1),
                                           SH = FindBound(., 0.9),
                                           N = FindBound(., 0.75),
                                           SL = FindBound(., 0.25),
                                           ML = FindBound(., 0.1),
                                           L = FindBound(., 0)
                                      )) -> future.bound.raw
    # Rearrange its form for the convienience of layer painting.
    future.bound <- data.frame()
    for(i in c(1:6)){
      future.bound.raw %>% select(1, i+1) -> future.bound.cache
      future.bound.cache$Level <- names(future.bound.raw)[i+1]
      names(future.bound.cache)[2] <- v1
      future.bound <- rbind(future.bound, future.bound.cache)
    }
    
    future.bound %>%
      mutate_at(1, DateReform, year = year(current.date)) -> future.bound
    
    rm(future.bound.cache, future.bound.raw)
    
    # Find the average value for each day:
    weather.data %>%
      filter(format(Date, "%m-%d") %in% format(future_period, "%m-%d")) %>%
      mutate(simplify_Date = format(Date, "%m-%d")) %>%
      group_by(simplify_Date) %>%
      select(simplify_Date,2:length(weather.data-1)) -> sub.weather.data
    
    # initialize the future.data:
    future_names <- names(weather.data)[-1]
    var_name <- quo(mean(
      eval(parse(text = future_names[1])),
      na.rm = TRUE
    ))
    sub.weather.data %>%
      summarise(!! var_name) -> future.data # So future.data is in "tbl" form.
    
    # add the rest parts in:
    for(i in c(2:length(future_names))){
      var_name <- quo(mean(
        eval(parse(text = future_names[i])), 
        na.rm = TRUE
      ))
      sub.weather.data %>%
        summarise(!! var_name) -> future.data.cache
      future.data <- left_join(
        future.data, future.data.cache, by = "simplify_Date"
      )
    }
    rm(sub.weather.data, future.data.cache, var_name, future_names)
    # rename its vars
    names(future.data) <- names(weather.data)
    
    future.data %>%
      mutate_at(1, DateReform, year = year(current.date)) -> future.data
    
    # Until here we finishd the prepare of the two period datasets.
    # Load them into Global Environment.
    
    hist.data %>% mutate_if(is.double, round, 1) -> hist.data
    future.data %>% mutate_if(is.double, round, 1) -> future.data
    
    # Define the max value to give the plots a upper horizontal bound:
    rbind(hist.bound, future.bound) %>%
      select(2) %>% max(na.rm = TRUE) -> max.cache
    max.cache <- max.cache * 1.1
    
    hist.max.cache <- data.frame(
      Date = hist_period, value = max.cache, Level = "H")
    names(hist.max.cache)[2] <- v1
    
    hist.bound <- rbind(hist.max.cache, hist.bound)
    names(hist.bound)[2] <- "Value"
    
    future.max.cache <- data.frame(
      Date = future_period, value = max.cache, Level = "H")
    names(future.max.cache)[2] <- v1
    
    future.bound <- rbind(future.max.cache, future.bound)
    names(future.bound)[2] <- "Value"
    
    rm(future.max.cache, hist.max.cache, future_period, 
       hist_period, max.cache)
    
    # ===================================
    # Determine the levels fot each day :
    # ===================================
    
    SetLevel <- function(date, value, bound){
      bound %>%
        filter(Date == date) %>%
        select(2) %>% unlist() -> stds.cache
      level <- cut(value, 
                   breaks = c(stds.cache[1], stds.cache[2], stds.cache[3],
                              stds.cache[4], stds.cache[5],
                              stds.cache[6], stds.cache[7], -Inf),
                   labels = FALSE
                   )
      return(level)
    }
    
    # For hist data:
    
    hist.data %>%
      select(1, Value = eval(v_num), (length(hist.data)-3): length(hist.data)) -> hist.data
    
    for(i in c(1:(r.num+1))){
      hist.data$Level[i] <- SetLevel(hist.data[i,1], hist.data[i,2], hist.bound)
    }
    
    hist.data$Level <- factor(hist.data$Level,
                                levels = c(1,2,3,4,5,6,7),
                                labels = c("Low","Moderately low","Slightly low",
                                           "Normal","Slightly high","Moderately high","High"))
    # For future data:
    
    future.data %>%
      select(1, Value = eval(v_num)) -> future.data
    
    future.data <- as.data.frame(future.data)
    
    for(i in c(1:p.num)){
      future.data$Level[i] <- SetLevel(future.data[i,1], future.data[i,2], future.bound)
    }
    
    future.data$Level <- factor(future.data$Level,
                              levels = c(1,2,3,4,5,6,7),
                              labels = c("Low","Moderately low","Slightly low",
                                         "Normal","Slightly high","Moderately high","High"))
    
    return(list(hist.bound, hist.data, future.bound, future.data))

    # hist.data <<- hist.data
    # future.data <<- future.data
    # hist.bound <<- hist.bound
    # future.bound <<- future.bound
    
    # v1 <<- v1
    
    # return(c(hist.bound, hist.data, future.bound, future.data))
}















