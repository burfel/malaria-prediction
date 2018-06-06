# read map prediction
# Predicts the percentage of reads that map to host and pathogen
# Felicia Burtscher
# Date: June 6, 2018

# server.R

#===============================================================================
#                               SHINYSERVER                                    #
#===============================================================================

server <- function(input, output, session){
  
  #===============================================================================
  #                        DASHBOARD SERVER FUNCTIONS                            #
  #===============================================================================

  
  #-----------MODELS-----------------
  #----------------------------------
  # MODEL 0a: GLM SIMPLE | PERCENTAGE 
  ## MODEL.log: -1.964e+00 + 6.550e-02*dat.nona$Percentage.parasitemia
  glm_simple <- function(){
    I <- -1.964e+00
    P <- input$parasitemia_percentage
    #W <- input$white_blood
    logit <- I + 6.550e-02*P
    exp(logit)/(1+exp(logit))
  }
  
  
  # MODEL 0b: GLM SIMPLE | DENSITY
  ## MODEL.log: -2.012e+00 + 2.031e-06*dat.nona$Parasite.density...µl.
  glm_simple_dens <- function(){
    I <- 2.012e+00
    P <- input$parasitemia_density
    logit <- I + 2.031e-06*P
    exp(logit)/(1+exp(logit))
  }
  
  
  # MODEL 1a: GLM COMPLEX | PERCENTAGE | TOTAL WHITE BLOOD CELLS
  ## MODEL.log: -1.112e+00 + 5.324e-02*dat.nona$Percentage.parasitemia + (-7.415e-02)*dat.nona$Total.White.Cell.Count..x109.L.
  glm_complex <- function(){
    I <- -1.112e+00
    P <- input$parasitemia_percentage2
    W <- input$white_blood
    I + 5.324e-02*P + (-7.415e-02)*W
    exp(logit)/(1+exp(logit))
  }
  
  
  # MODEL 1b: GLM COMPLEX | DENSITY | TOTAL WHITE BLOOD CELLS
  # MODEL.log: -1.238e+00 + 1.676e-06*dat.nona$Parasite.density...µl. + (-6.638e-02)*dat.nona$Total.White.Cell.Count..x109.L.
  glm_complex_dens <- function(){
    I <- -1.238e+00
    P <- input$parasitemia_density2
    W <- input$white_blood
    I + 1.676e-06*P + (-6.638e-02)*W
    exp(logit)/(1+exp(logit))
  }
  
  # MODEL 1c: GLM COMPLEX | PERCENTAGE | DIFFERENT WHITE BLOOD CELL COUNTS
  ## MODEL.log: -1.068e+00 + 6.155e-02*dat.nona$Percentage.parasitemia + (-5.826e-01)*dat.nona$Lymphocyte.count...x109.L. + (2.898e+00)*dat.nona$Monocyte.count...x109.L. + (-1.634e-01)*dat.nona$Neutrophil.count...x109.L.
  glm_complex_counts <- function(){
    I <- -1.068e+00
    P <- input$parasitemia_percentage2
    L <- input$lympho
    M <- input$mono
    N <- input$neutro
    I + 6.155e-02*P + (-5.826e-01)*L + 2.898e+00*M + (-1.634e-01)*N
    exp(logit)/(1+exp(logit))
  }
  
  # MODEL 1D: GLM COMPLEX | DENSITY | DIFFERENT WHITE BLOOD CELL COUNTS
  ## MODEL.log: -1.129e+00 + 1.744e-06*dat.nona$Parasite.density...µl. + (-4.876e-01)*dat.nona$Lymphocyte.count...x109.L. + (2.639e+00)*dat.nona$Monocyte.count...x109.L. + (-1.666e-01)*dat.nona$Neutrophil.count...x109.L.
  glm_complex_counts_dens <- function(){
    I <- -1.129e+00
    P <- input$parasitemia_density2
    L <- input$lympho
    M <- input$mono
    N <- input$neutro
    I + 1.744e-06*P + (-4.876e-01)*L + 2.639e+00*M + (-1.666e-01)*N
    exp(logit)/(1+exp(logit))
  }
  
  # # ---old models
  # # MODEL 0b1b: glm total percentage lympho
  # # MODEL.log: -1.772e+00 + 7.545e-02*dat.nona$Percentage.parasitemia + (-1.056e-02)*dat.nona$Percentage.lymphocytes
  # 
  # # MODEL 0a1b: glm total density lympho
  # # MODEL.log: -1.939e+00 + 2.125e-06*dat.nona$Parasite.density...µl. + (-3.870e-03)*dat.nona$Percentage.lymphocytes
  # 
  # 
  # # MODEL 0a1c: glm total percentage mono
  # # MODEL.logL: -2.025e+00 + 6.461e-02*dat.nona$Percentage.parasitemia + 1.143e-02*dat.nona$Percentage.monocytes
  # 
  # # MODEL 0b1c: glm total density mono
  # # MODEL.logL: -2.226e+00 + 1.951e-06*dat.nona$Parasite.density...µl. + 3.953e-02*dat.nona$Percentage.monocytes
  # 
  # 
  # # MODEL 0a1d: glm total percentage neutro
  # # MODEL.log: -2.581e+00 + 7.341e-02*dat.nona$Percentage.parasitemia + 8.204e-03*dat.nona$Percentage.neutrophils
  # 
  # # MODEL 0b1d: glm total density neutro
  # # MODEL.log: -2.238e+00 + 2.106e-06*dat.nona$Parasite.density...µl. + 3.036e-03*dat.nona$Percentage.neutrophils
  
  
  #------- UPON CLICKING 'SIMPLE MODEL'... 
  # observeEvent(input$go_simple, {
  
  # COMPUTE PREDICTION SIMPLE MODEL ----
  output$comp_simple <- renderText({
    paste("Prediction:", br(), "Percentage of reads that will map to pathogen: ", 100*glm_simple(), br(), "Percentage of reads that will map to host: ", 100*(1 - glm_simple()))
  })
  
  output$comp_simple_dens <- renderText({
    paste("Prediction:", br(), "Percentage of reads that will map to pathogen: ", 100*glm_simple_dens(), br(), "Percentage of reads that will map to host: ", 100*(1 - glm_simple_dens()))
  })
  
  # PLOT SIMPLE MODEL SUMMARY ----
  output$plot_simple <- renderPlot({
    plot(fit.nona.paras)
  })
  output$plot_simple_dens <- renderPlot({
    plot(fit.nona.paras.dens)
  })
  
  # SUMMARY OF THE SIMPLE MODEL ----
  output$summary_simple <- renderUI({
    # dataset <- datasetInput()
    # summary(dataset)
    summary(fit.nona.paras)
  })
  output$summary_simple_dens <- renderUI({
    summary(fit.nona.paras.dens)
  })
  
  #   }
  # )
  
  
  #------- UPON CLICKING 'COMPLEX MODEL'...
  #observeEvent(input$go_complex, {
  
  # COMPUTE PREDICTION COMPLEX MODEL ----
  output$comp_complex <- renderText({
    paste("Prediction:", br(), "Percentage of reads that will map to pathogen: ", 100*glm_complex(), br(), "Percentage of reads that will map to host: ", 100*(1 - glm_complex()))
  })
  
  output$comp_complex_dens <- renderText({
    paste("Prediction:", br(), "Percentage of reads that will map to pathogen: ", 100*glm_complex_dens(), br(), "Percentage of reads that will map to host: ", 100*(1 - glm_complex_dens()))
  })
  
  output$comp_complex_counts <- renderText({
    paste("Prediction:", br(), "Percentage of reads that will map to pathogen: ", 100*glm_complex_counts(), br(), "Percentage of reads that will map to host: ", 100*(1 - glm_complex_counts()))
  })
  
  output$comp_complex_counts_dens <- renderText({
    paste("Prediction:", br(), "Percentage of reads that will map to pathogen: ", 100*glm_complex_counts_dens(), br(), "Percentage of reads that will map to host: ", 100*(1 - glm_complex_counts_dens()))
  })
  
  
  # output$comp_total <- renderUI({
  #   str1 <- paste("You have selected", input$parasitemia_percentage, " as percentage of parasitemia.")
  #   str2 <- paste("Prediction:")
  #   str3 <- paste("Percentage of reads that will map to pathogen: ", lm_simple_perc())
  #   str4 <- paste("Percentage of reads that will map to host: ", 1 - lm_simple_perc())
  #   HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
  # })
  
  ##############################
  # PLOT TOTAL MODEL ----
  output$plot_total <- renderPlot({
    
    # fit.nona.total <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$total.number.of.cells, data=dat.nona)
    # summary(fit.nona.total) # show results: R^2: 0.4622
    # summary(fit.nona.total)$sigma^2 # estimated variance of residuals around a fitted line: 0.02268394
    # 
    # # plot the statistics, OUTLIERS 35, 39 -- both in UM group? -- kept them -- BUT MIGHT BE WORTH TRYING WITHOUT THEM
    par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
    plot(fit.nona.total)  # Plot the model information
  })
  
  # fit.nona.total.L
  # fit.nona.total.M
  # fit.nona.total.N
  
  # SUMMARY OF THE COMPLEX MODEL ----
  output$summary_total <- renderPrint({
    # dataset <- datasetInput()
    # summary(dataset)
    summary(fit.nona.total)
  })
  
  #  }
  #)
  


})