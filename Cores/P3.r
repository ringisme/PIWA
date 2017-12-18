# This script is designed to create the monthly trend figure:

m.plot <- function(v1, m1, D.data){
  
  # determine var's name. Store it as Vnum
  
  Vnum <- which(names(D.data) == v1)
  
  #########################
  #     Data Analysis     #
  #########################
  
  # ===== Monthly Report =====
  # Data Selection fot Monthly Report:
  Mon.D.data <- subset(x = D.data,
                       month(mdy(WX_DATE)) == m1 &
                         year(mdy(WX_DATE)) < 2011
  )
  
  # Trying to build a new dataframe which only
  # contains Years and the mean of indicator for each day on this month.
  Mon.D.data %>%
    select(WX_DATE, c2 = Vnum) %>%
    group_by(day(mdy(Mon.D.data$WX_DATE))) %>%
    summarise(Average = mean(c2)) %>% as.data.frame() -> Mon.var
  
    names(Mon.var)[1] <- c("Day")
  
  M.V1.ratio <- quantile(Mon.var[,2], 
                         probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1),
                         na.rm=TRUE)
  
  # Use dplyr to define the boundaries:
  
  Mon.var %>%
      mutate(
          level = case_when(
              Average <= M.V1.ratio[1] ~ "Low",
              Average <= M.V1.ratio[2] & Average > M.V1.ratio[1] ~ "Moderately low",
              Average <= M.V1.ratio[3] & Average > M.V1.ratio[2] ~ "Slightly low",
              Average <= M.V1.ratio[5] & Average > M.V1.ratio[3] ~ "Normal",
              Average <= M.V1.ratio[6] & Average > M.V1.ratio[5] ~ "Slightly high",
              Average <= M.V1.ratio[7] & Average > M.V1.ratio[6] ~ "Moderately high",
              Average > M.V1.ratio[7] ~ "High"
              )
             ) -> Mon.var
  
  Mon.var$level <- ordered(Mon.var$level, 
                              levels = c("High","Moderately high","Slightly high",
                                         "Normal",
                                         "Slightly low","Moderately low","Low"))
  
  Mon.var <<- Mon.var # This is the set for drawing.
  
  #########################
  #     visualization     #
  #########################
  
  # ===== For Monthly Report =================
  # ==========================================
  
  p.mr <- ggplot(data = Mon.var, aes(x=Day, y=Average,
                                     text = paste(
                                         "Day:", Day,
                                         "<br>Average:", round(Average, digits = 2),
                                         "<br>level:", level
                                     )
                                     )) +
      scale_x_continuous(breaks=seq(1,31,1)) +
      geom_point(fill="black", size =3, colour = "blue") +
    # Line chart
      geom_line(data=Mon.var, mapping = aes(x=Day, y=Average),
                size = 0.7, inherit.aes = FALSE) +
    # Add Lesso lines
      geom_smooth(data=Mon.var, mapping = aes(x=Day, y=Average),
                  method = loess, se=FALSE, colour = "skyblue",
                  inherit.aes = FALSE) +
      geom_hline(yintercept = M.V1.ratio[-4], colour = "gray", size =1, lty=2)+
      geom_hline(yintercept = mean(Mon.var$Average, na.rm=TRUE), colour = "green", size = 1, lty = 2) +
    # Titles and axes
      labs(title = paste("Monthly", v1, "Report of", month(m1, label = TRUE, abbr = FALSE))) +
      theme(plot.title = element_text(hjust = 0.5)) +
      ylab(paste("Average", v1))
  
  p.mr <<- ggplotly(p.mr, tooltip = c("text"))
  
}






