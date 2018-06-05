# Prediction of read depth in whole blood Dual RNA-seq data from malaria patients

We developed a simple prediction tool that enables the user to estimate the likely proportion of reads that will map to parasite and host given basic information about the sample. 

The web tool can be found in the directory _shinyapp2_.
To run the app from ```R Studio```, copy and paste the following code in the folder:
   ```{r} 
 # First install the following packages:
 library(shiny)
 library(shinydashboard)
 library(ggExtra)
 library(ggplot2)
 ### run the application:
 shiny::runGitHub("malaria-prediction", "burfel", subdir="shinyapp2")
 ```

