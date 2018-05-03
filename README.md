# PIWA - Performance Indicator Web Application.

This R Shiny application is built to demonstrate the performance indicator trends in wildland fire management.

---

To run the web application:

1. Download the compressed file of this app, and then release them in your own computer. 

2. Please ensure that you have already installed `R` and `RStudio`.

   *(R: https://www.r-project.org/*

   *RStudio: https://www.rstudio.com/)*

3. Completely close RStudio if it is running. Re-open RStudio by double clicking "RunApp.r". 

   *(This step is for setting the current RStudio working directory as same as the file path of "RunApp.r". You can also manually change the working directory in RStudio.)*

   

   - if you have authority to access the data files `DRY_Weather.csv` and `DRY_Fires.csv` (these two files are not provided to public), please put them into one folder, and then rename this folder as "Ddata". Go to step 4.
   - if you are a vistor, please go to step 5.

   

4. *(For department stuff)* Open "RunApp.r", change the folder address (the address after `DdataAddress <- `) to your own `Ddata` address.

   For example, Windows user may change the address to `"C:/Document/Ddata"`, and Mac OS user may change it to `"/Document/Ddata"`.

5. *(For vistors)* Open "RunApp.r", change the folder address (the address after `DdataAddress <- `) to `"samples"` , so it will be `DdataAddress <- "samples"` at line 15.

6. Run "RunApp.r".

---

> The web application won't successfully run if there is any required R package missing.
>
> Please ensure you have installed the following R packages before you run it.
>
> (Copy and paste the following R commands to your R console, and run it **ONCE**)

```r
install.packages("lubridate") # For better manipulate date objects.
install.packages("scales") # For create percentage numbers.
install.packages("plyr")
install.packages("dplyr")
install.packages("plotly") # For creaste pie charts.
install.packages("ggplot2") # For more powerful plots.
install.packages("purrr") # For using "map" functions in R.
install.packages("ggpubr")# For better table displaying.
install.packages("shinydashboard")# For dashboard layout.
install.packages("DT") # For display table objects in R.
install.packages("markdown") # For display markdown documents in R.
```



# FAQs:

1. Why RStudio send me "**No Shiny application …**" when I am trying to run "RunApp.R"?

   > Please ensure you have already set your current working directory as same as the file path of "RunApp.R". 
   >
   > You can redo the step 3, or you can find the related setting located in the lower right side window of RStudio. There is a "gear icon" under the tab of `Files`.

2. Why RStudio shows "**unkown timezone …**" ?

   > The newest version of RStudio in Mac OS platform has a little bug that need you to manully set the system timezone whenever you need to load any files.
   >
   > Please add this command `Sys.setenv(TZ = "America/Toronto")`( replace "America/Toronto" to your own time zone) at the first line of "RunApp.R".

3. Why RStudio shows "Error in library(xxx): there is no package called 'xxx' " ?

   > That's because you haven't installed all the required packages of this app. Please see the warning message after the step 6.



# Contact info:

You can send me an Email if you have any question about this app.

Please contact me at: yfan95@uwo.ca