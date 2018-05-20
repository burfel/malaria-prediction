## About `Mapped reads`

This interactive application allows you to predict the percentage of reads that map to the host and to the pathogen based on some information of the patient, ie parasetemia percentage, number of white blood cells and/ or number of cells.
This allows the user to know the number of reads necessary for their study.
 
 ## Run this application on R Studio
 
 To run this application localy, simple paste the following code on `R` console: 
   ```{r} 
 # First install the following packages:
 library(shiny)
 library(shinydashboard)
 library(ggExtra)
 library(ggplot2)
 ### run the application:
 shiny::runGitHub("malaria-prediction/shinyapp2", "burfel")
 ```
 
 ## Author
 Felicia Burtscher | felicia.burtscher17@imperial.ac.uk
 
 ## Credit
 
 This application was developed with [shiny](http://shiny.rstudio.com/) in 
 [R studio](https://www.rstudio.com/).
 
 --->
 
 