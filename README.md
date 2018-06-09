# Prediction of read depth in whole blood Dual RNA-seq data from malaria patients

We developed a simple prediction tool that enables the user to estimate the likely proportion of reads that will map to parasite and host given basic information about the sample. 

The web tool can be found in the directory _shinyapp4_.
To run the app from ```R Studio```, copy and paste the following code from the folder:
 <!---
 # First install the following packages:
 library(shiny)
 library(shinythemes)
 libary(shinyalert)
 library(ggExtra)
 library(ggplot2)
 library(rmarkdown)
 library(plotly)
 library(knitr)
 --->
 ```{r}
 ### run the application:
 install.packages("shiny", 
                  "shinythemes", 
                  "shinyalert", 
                  "ggExtra",
                  "ggplot2",
                  "rmarkdown",
                  "plotly",
                  "knitr",
                  "lmtest")
 shiny::runGitHub("malaria-prediction", "burfel", subdir="shinyapp4")
 ```

