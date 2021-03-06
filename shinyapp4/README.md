# About `Mapped read prediction in Malaria`
<big>
This interactive application allows you to predict the percentage of reads that map to the host and to the pathogen based on some information of the patient, ie percentage of parasitemia, number of white blood cells or its subtypes (lymphocytes, monocytes and neutrophils).
This allows the user to know the number of reads necessary for their study.
 
## Run this application in R Studio
 
To run this application locally, simple paste the following code on a `R` console: 
   ```{r} 
 # First install the following packages:
 library(shiny)
 library(shinyalert)
 library(ggExtra)
 library(ggplot2)
 library(shinythemes)
 library(rmarkdown)
 library(plotly)
 library(knitr)
 library(lmtest)
 library(png)
 ### run the application:
 shiny::runGitHub("malaria-prediction", "burfel", subdir="shinyapp4")
 ```
 The code inluding all markdown files and figures shown on this website can be found on github (Link [here](https://github.com/burfel/malaria-prediction/tree/master/shinyapp4)). 
 
## Author
Felicia Burtscher | [felicia.burtscher17@imperial.ac.uk](mailto:felicia.burtscher17@imperial.ac.uk).
Do get in touch for complaints, suggestions and any feedback!
 
## Credit
This application was developed with [shiny](http://shiny.rstudio.com/) in [R studio](https://www.rstudio.com/). Special thanks go to Dr Aubrey Cunnington and Dr Clive Hoggart for support and advice and the rest of the Paediatrics group at St Mary's Hospital, Imperial College London.
</big>
<br>
</br>
<img src="img/logo.png" alt="logo" width="200px"/>
<!---
![Imperial Logo](img/logo.png =100x20)
--->
