# Pre-loaded:
rm(list=ls())
DdataAddress <- "E:/Ddata"

D.data <- read.csv(paste0(DdataAddress,"/DRY_Weather.csv"),
                   sep = ",",
                   header = TRUE)

F.data <- read.csv(paste0(DdataAddress,"/DRY_Fires.csv"),
                   sep = ",",
                   header = TRUE)
# ========

