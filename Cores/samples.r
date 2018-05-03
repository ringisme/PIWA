library(lubridate) # For better manipulate Date.
# library(sqldf) # Allow to use SQL syntax in R. 
#(the last version doesn't need SQL since I rewrite it in dplyr syntax.)
library(scales) # For create percentage number.
library(gridExtra) # For combine two plots together.
library(plyr)
library(dplyr)
library(plotly) # For creaste pie chart
library(ggplot2)
library(purrr) # For using "map" functions in R.
library(ggpubr)# For better table displaying
library(shinydashboard)# For dashboard layout
library(DT) # For display table objects in R.
# ----------------------------------------------------
# Create Weather Data:
Date <- as.data.frame(seq(as.Date("2046/1/1"), by = "day", length.out = 5000))
names(Date) <- "Date"

set.seed(233)

fire.date <- sample_n(Date, 2000)
names(fire.date) <- "Date"
fire.date$times <- sample(1:6, 2000, replace = TRUE)

dates <- rep(fire.date$Date[1], fire.date$times[1])
for(i in 2:length(fire.date$Date)){
    dates <- c(dates, rep(fire.date$Date[i], fire.date$times[i]))
}

fire.data <- as.data.frame(dates)
weather.data <- Date
rm(Date, dates, fire.date)

weather.data$PI_1 <- round(rnorm(5000, mean = 20, sd = 10), digits = 1)
weather.data$PI_2 <- round(rnorm(5000, mean = 100, sd = 50), digits = 1)
weather.data$PI_3 <- round(rnorm(5000, mean = 250, sd = 150), digits = 1)
# Create Fire Data:
names(fire.data) <- "Date"
fire.data$FINAL_SIZE <- round(rnorm(length(fire.data$Date), mean = 2500, sd = 1500), digits = 1)
fire.data$FINAL_SIZE[fire.data$FINAL_SIZE < 0] <- sample(1:100, 1)
fire.data$CAUSE <- sample(c("Lightning","Human"), size = length(fire.data$Date), 
                          replace = TRUE, prob = c(0.7, 0.3))
rm(i)


# ----------------------------------------------------
var_num <- length(weather.data)-1
# ----------------------------------------------------
# +++++ Pre-load datas for P1, "Real Time Trend" 
source("../Cores/P1/P1_1.r", local=TRUE)
source("../Cores/P1/P1_1_W1.r", local=TRUE)
source("../Cores/P1/P1_1_W2.r", local=TRUE)
source("../Cores/P1/P1_1_F1.r", local=TRUE)
source("../Cores/P1/P1_2.r")

# +++++ Pre-load datas for P2, "Today in history"
source("../Cores/P2/P2.r", local=TRUE)
source("../Cores/P2/P2_Plot.r", local=TRUE)

# +++++ Pre-load datas for P3, "Search Function"
source("../Cores/P3/P3_Table.r", local=TRUE)
