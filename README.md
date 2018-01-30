# PIWA - Performance Indicator Web Application.

(**For department users only**)

This R Shiny application is built to demonstrate the performance indicator trends in wildland fire management.

---

To run the web application:

1. Put the data files `DRY_Weather.csv` and `DRY_Fires.csv` (these two files are not provided to public) in one folder. Rename this folder as "Ddata".

2. Open "RunApp.r", change the folder address (the address after `DdataAddress <<- `) to your own `Ddata` address.

   For example, Windows user may change the address to `"C:/Document/Ddata"`, and Mac OS user may change it to `"/Document/Ddata"`.

3. Save "RunApp.r".

4. Run "RunApp.r"

---

> The web application won't successfully run if the required R packages are missing.
>
> Please ensure you have installed the following R packages before you run.
>
> (Copy and paste the following R commands to your R console)

```r
install.packages("lubridate") # For better manipulate date objects.
install.packages("sqldf") # Allow to use SQL syntax in R.
install.packages("scales") # For create percentage number.
install.packages("gridExtra") # For combine two plots together.
install.packages("plyr")
install.packages("dplyr")
install.packages("plotly") # For creaste pie chart
install.packages("ggplot2") # For more powerful plot
install.packages("purrr") # For using "map" functions in R.
install.packages("ggpubr")# For better table displaying
install.packages("shinydashboard")# For dashboard layout
```

