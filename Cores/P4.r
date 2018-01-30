sm.search <- function(D.data, F.data, sens, set_all,
                      date1, date2,
                      search.data
                      ){
  D.data %>%
    select(3:15) %>%
    mutate(Date = as.Date(WX_DATE, format = "%m/%d/%Y")) %>%
    select(14, 2:13) %>%
    select(Date,
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
    ) -> clean.D.data

  
  determine_bounds <- function(var, sens, train_col){
    if(is.na(var) == TRUE){
      upper = max(train_col, na.rm = TRUE)
      lower = min(train_col, na.rm = TRUE)
    }else{
      upper = var + sd(train_col, na.rm = TRUE)*sens
      lower = var - sd(train_col, na.rm = TRUE)*sens
    }
    return(c(upper, lower))
  }
  # ==========
  # Input here
  # ==========
  # The input "current day" values are:

  # ==== Input End ====
  
  # Form the conditions:
  prepared.data <- as.data.frame(names(clean.D.data[-1]))
  names(prepared.data) <- "Indicator"
  
  if(set_all == TRUE){
    sens = rep(1,12)
  }
  
  for(i in 1:length(prepared.data$Indicator)){
    prepared.data$value[i] <- search.data[i]
    prepared.data$lower[i] <- determine_bounds(search.data[i],
                                               sens[i],
                                               clean.D.data[,i+1])[2]
    prepared.data$upper[i] <- determine_bounds(search.data[i],
                                               sens[i],
                                               clean.D.data[,i+1])[1]
  }
  # Search!
  clean.D.data %>%
    filter(
      TEMP >= prepared.data$lower[1] & TEMP <= prepared.data$upper[1] &
        BUI >= prepared.data$lower[2] & BUI <= prepared.data$upper[2] &
        FFMC >= prepared.data$lower[3] & FFMC <= prepared.data$upper[3] &
        DC >= prepared.data$lower[4] & DC <= prepared.data$upper[4] &
        DMC >= prepared.data$lower[5] & DMC <= prepared.data$upper[5] &
        DSR >= prepared.data$lower[6] & DSR <= prepared.data$upper[6] &
        FWI >= prepared.data$lower[7] & FWI <= prepared.data$upper[7] &
        ISI >= prepared.data$lower[8] & ISI <= prepared.data$upper[8] &
        RAIN >= prepared.data$lower[9] & RAIN <= prepared.data$upper[9] &
        REL_HUM >= prepared.data$lower[10] & REL_HUM <= prepared.data$upper[10] &
        WIND_DIR >= prepared.data$lower[11] & WIND_DIR <= prepared.data$upper[11] &
        WIND_SPEED >= prepared.data$lower[12] & WIND_SPEED <= prepared.data$upper[12]
    ) -> prepared.data
  
  rm(search.data, i)
  
  # Now to present fire information:
  # Firstly, clean the fire data:
  F.data %>%
    select(REPORT_DATE,FINAL_SIZE,CAUSE) %>%
    mutate(Date = as.Date(REPORT_DATE, format = "%m/%d/%Y %H:%M")) %>%
    select(4,2,3) -> clean.F.data

  # Combine two datasets together:
  for(i in 1:length(prepared.data$Date)){
    prepared.data$FIRE_TIMES[i] <- nrow(na.omit( # Summarize Fire Numbers.
      clean.F.data[clean.F.data$Date == prepared.data$Date[i],]
    ))
    
    prepared.data$TOTAL_BURNED_SIZE[i] <- sum( # Sum Fire Burned Area.
      clean.F.data[clean.F.data$Date == prepared.data$Date[i], 2],
      na.rm = TRUE
    )
    
    prepared.data$HUMAN_CAUSED_TIMES[i] <- nrow(na.omit(
      clean.F.data[clean.F.data$Date == prepared.data$Date[i] & clean.F.data$CAUSE == "Human",]
    ))
    
    prepared.data$LIGHTNING_CAUSED_TIMES[i] <- nrow(na.omit(
      clean.F.data[clean.F.data$Date == prepared.data$Date[i] & clean.F.data$CAUSE == "Lightning",]
    ))
  }
  # Select the data in the selected date range:
  prepared.data %>%
    filter(Date >= date1 & Date <= date2) %>%
    select(Date, TOTAL_BURNED_SIZE, 2:14, 16, 17) -> rearranged.data
  DT::datatable(rearranged.data, 
                options = list(pageLength=8, scrollX='600px'))
}











