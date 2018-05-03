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
library(markdown)
# ----------------------------------------------------
# Load the two datasets:
 
D.data <- read.csv(paste0(DdataAddress,"/DRY_Weather.csv"),
                   sep = ",",
                   header = TRUE)

F.data <- read.csv(paste0(DdataAddress,"/DRY_Fires.csv"),
                   sep = ",",
                   header = TRUE)
# ----------------------------------------------------
# Clean the dataseats:

D.data %>%
  mutate(Date = as.Date(WX_DATE, format = "%m/%d/%Y"))%>%
  select(Date, 4:length(D.data))-> weather.data

F.data %>%
  select(REPORT_DATE,FINAL_SIZE,CAUSE) %>%
  mutate(Date = as.Date(REPORT_DATE, format = "%m/%d/%Y %H:%M")) %>%
  select(4,2,3) -> fire.data

rm(D.data, F.data)

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
