# input unit
P3_Process <- function(search.list, weather.data, fire.data, start.date, end.date){
    
  names(search.list) <- names(weather.data)[-1]
  var.num <- length(search.list)
  
  # Make the quote:
  condition <- paste()
  for(i in 1:var.num){
    if(anyNA(search.list[i]) == FALSE){
      if(length(search.list[[i]]) == 1){
        condition <- paste(condition, "&", names(search.list[i]), "==", search.list[i])
      }else if(length(search.list[[i]]) == 2){
        condition <- paste(condition, "&", names(search.list[i]), ">=", search.list[[i]][1],
                           "&", names(search.list[i]), "<=", search.list[[i]][2])
      }
    }
  }
  
  condition <- paste(unlist(strsplit(condition, " "))[c(-1,-2)], collapse = " ")
  
  if(condition == ""){
      weather.data -> search.data
  }else{
      weather.data %>%
          filter_(paste(condition)) -> search.data
  }
  
  # Joint the fire data:
  
  fire.data %>%
    filter(Date %in% search.data$Date) %>%
    group_by(Date) %>%
    summarise(FIRE_TIMES = n(), TOTAL_BURNED_SIZE = sum(FINAL_SIZE, na.rm = TRUE),
              HUMAN_CAUSED_TIMES = table(CAUSE)[1], 
              LIGHTNING_CAUSED_TIMES = table(CAUSE)[2]) -> fire.cache
  
  result.data <- left_join(search.data, fire.cache, "Date")
  col.num <- length(result.data)
  result.data[,((col.num-3):col.num)][is.na(result.data[,((col.num-3):col.num)])] <- 0
  
  result.data %>%
    filter(Date >= start.date & Date <= end.date) -> result.data
  
  return(result.data)
}



