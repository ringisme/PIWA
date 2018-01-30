rm(list=ls())

# === === === === === === === === ==
# Please change folder address here:
DdataAddress <<- "E:/Ddata"


# Run tested dashboard:
shiny::runApp("dashboard")

# Run the old version:
# shiny::runApp("Web_App")
