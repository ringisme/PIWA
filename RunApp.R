# Before you run this R file, 
# please ensure that your current working directory
# has been set as same as the file path of this R file.
# (You can completely close RStudio at first, and then
# re-open it by clicking "RunApp.r" directly to set the path.)

rm(list=ls())

# === === === === === === === === ==
# Please change the folder address 
# as your own "Ddata" folder address at here:
# (if you are a vistor, please change it to:
# DdataAddress <- "samples")

DdataAddress <- "~/Ddata"

# Run tested dashboard:
shiny::runApp("dashboard")

