# This R script will evaluate the monthly and daily situation ...
# ====
# To clean your environment, please run:
rm(list=ls())

# install.packages("ggplot2")
# install.packages("lubridate")
# install.packages("knitr")


library(ggplot2)
library(lubridate)
library(sqldf) # Allow me to use SQL syntax.
library(scales) # For percentage number.
library(gridExtra) # For combine two plots together.
library(plyr)
library(dplyr) # For advanced data filtering.
library(shinythemes) # For more beautiful themes.
library(Cairo) # For beautiful plots.
library(plotly) # For creasting pie chart
library(purrr) # For using "map" functions.
library(DT)
library(ggpubr)# For better table displaying

### Set local path
setwd("./Cores")

D.data <- read.csv("../../../../Ddata/DRY_Weather.csv",
                   sep = ",",
                   header = TRUE)

F.data <- read.csv("../../../../Ddata/DRY_Fires.csv",
                   sep = ",",
                   header = TRUE)

# ======================================
##################                     =
#  Which year?  #                      = 
##################                     =
y1 <- 2012
##################                     =
#  Which month?  #                     =
##################                     =
m1 <- 7
##################                     =
#   Which day?   #                     =
##################                     =
d1 <- 15
##################                     =
#   Which var?   #                     =
##################                     =
v1 <- "TEMP"
# ======================================

# ======================================
####################################   =
#  How many days want to retrive?  #   =
####################################   =
r.num <- 7
####################################   =
#  How many days want to preview?  #   =
####################################   =
p.num <- 7
# ======================================

left.y <- 1965
right.y <- 2010

search.data = c(TEMP_1 = NA,
                BUI_1 = 47,
                FFMC_1 = NA,
                DC_1 = 255,
                DMC_1 = NA,
                DSR_1 = NA,
                FWI_1 = NA,
                ISI_1 = NA,
                RAIN_1 = NA,
                REL_HUM_1 = NA,
                WIND_DIR_1 = NA,
                WIND_SPEED_1 = NA)

sens = rep(1,12)


