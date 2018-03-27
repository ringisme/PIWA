library(lubridate) # For better manipulate Date.
library(sqldf) # Allow to use SQL syntax in R.
library(scales) # For create percentage number.
library(gridExtra) # For combine two plots together.
library(plyr)
library(dplyr)
library(plotly) # For creaste pie chart
library(ggplot2)
library(purrr) # For using "map" functions in R.
library(ggpubr)# For better table displaying
library(shinydashboard)# For dashboard layout
library(DT, warn.conflicts = FALSE) # For display table objects in R.

D.data <- read.csv(paste0(DdataAddress,"/DRY_Weather.csv"),
                   sep = ",",
                   header = TRUE)

F.data <- read.csv(paste0(DdataAddress,"/DRY_Fires.csv"),
                   sep = ",",
                   header = TRUE)

# ----------------------------------------------------
# +++++ Pre-load datas for P1, "Real Time Trend" 
source("../Cores/P1_1.r")
source("../Cores/P1_2.r", local=TRUE)

# +++++ Pre-load datas for P2, "Today in history"
source("../Cores/P2_1.r", local=TRUE)
source("../Cores/P2_2.r", local=TRUE)

# +++++ Pre-load datas for P4, "Search Function"
source("../Cores/P4.r", local=TRUE)
