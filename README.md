# Prediction of read depth in whole blood dual-RNA Seq data from malaria patients

We developed a simple prediction tool that enables the user to estimate the likely proportion of reads that will map to parasite and host given basic information about the sample. 

The web tool can be found in the directory ```{r} shinyapp2 ```.
To run the app from ```{r} R Studio```, copy and paste the following code:
   ```{r} 
 # First install the following packages:
 library(shiny)
 library(shinydashboard)
 library(ggExtra)
 library(ggplot2)
 ### run the application:
 shiny::runGitHub("malaria-prediction", "burfel", subdir="shinyapp2")
 ```

