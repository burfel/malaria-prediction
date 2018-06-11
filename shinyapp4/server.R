# read map prediction
# Predicts the percentage of reads that map to host and pathogen
# Felicia Burtscher
# Date: June 6, 2018

# server.R

#===============================================================================
#                               SHINYSERVER                                    #
#===============================================================================

# server = function(input, output) {
server = function(input, output, session) {
  
  # output$table <- renderTable({
  #   head(cars, 4)
  # })
  
  # filedata <- reactive({
  #   infile <- input$datafile
  #   if (is.null(infile)) {
  #     # User has not uploaded a file yet
  #     return(NULL)
  #   }
  #   read.csv(infile$datapath)
  # })
  
  
  
  #===============================================================================
  #                               LOAD MARKDOWN FILES                            #
  #===============================================================================   
  output$lay_summary <- renderUI({
    includeMarkdown("md/00_lay-summary.Rmd")
  })
  output$abstract <- renderUI({
    includeMarkdown("md/0_abstract.Rmd")
  })
  output$introduction <- renderUI({
    includeMarkdown("md/1_introduction.Rmd")
  })
  output$methods <- renderUI({
    includeMarkdown("md/2_methods.Rmd")
  })
  output$results <- renderUI({
    includeMarkdown("md/3_results.Rmd")
  })
  output$discussion <- renderUI({
    includeMarkdown("md/4_discussion.Rmd")
  })
  output$conclusion <- renderUI({
    includeMarkdown("md/5_conclusion.Rmd")
  })
  output$data <- renderUI({
    includeMarkdown("md/6_data.Rmd")
  })
  output$references <- renderUI({
    includeMarkdown("md/7_references.Rmd")
  })
  output$glossary <- renderUI({
    includeMarkdown("md/8_glossary.Rmd")
  })
  output$about <- renderUI({
    includeMarkdown("README.md")
  })
  
  #===============================================================================
  #                               MODELS                                         #
  #===============================================================================
  #===============================================================================
  #                               SIMPLE   MODEL                                 #
  #===============================================================================
  
  glm_both_simple<-function(ptype){
    if(input$tabset == "simple" && ptype == "ppercentage"){
      glm_simple()
    } 
    else if(input$tabset == "simple" && ptype == "pdensity"){
      glm_simple_dens()
    }
    #   ###########
    # else if(input$tabset == "complex" && ptype == "ppercentage"){
    #     glm_complex()
    #     # call_null()
    #   }
    # else if(input$tabset == "complex" && ptype == "pdensity"){
    #   glm_complex_dens()
    # }
    ###########-- attempts to include two regression plots for the complex model
    else if(input$tabset == "complex" && ptype == "ppercentage2" && input$wtype == "white_blood"){
      glm_complex()
    }
    else if(input$tabset == "complex" && ptype == "pdensity2" && input$wtype == "white_blood"){
      glm_complex_dens()
    }
    else if(input$tabset == "complex" && ptype == "ppercentage2" && input$wtype == "counts"){
      glm_complex_counts()
    }
    else if(input$tabset == "complex" && ptype == "pdensity2" && input$wtype == "counts"){
      glm_complex_counts_dens()
    }
  }
  
  # glm_both_complex<-function(ptype2, wtype){
  #   if(type2 == "ppercentage2" & wtype == "white_blood"){
  #     glm_complex()
  #   } 
  #   else if(type2 == "pdensity2" & wtype == "white_blood"){
  #     glm_complex_dens()
  #   }
  #   else if(type2 == "ppercentage2" & wtype == "counts"){
  #     glm_complex_counts()
  #   }
  #   else if(type2 == "pdensity2" & wtype == "counts"){
  #     glm_complex_counts_dens()
  #   }
  # }
  
  
  # MODEL 0a: GLM SIMPLE | PERCENTAGE
  ## MODEL.log: -1.964e+00 + 6.550e-02*dat.nona$Percentage.parasitemia
  glm_simple <- function(){
    # I <- -1.964e+00 # on small dat.nona (21 rows)
    I <- -1.613e+00
    P <- input$parasitemia_percentage
    # logit <- I + 6.550e-02*P 
    logit <- I + 4.321e-02*P
    exp(logit)/(1+exp(logit))
  }
   
  
  # MODEL 0b: GLM SIMPLE | DENSITY
  ## MODEL.log: -2.012e+00 + 2.031e-06*dat.nona$Parasite.density...µl.
  glm_simple_dens <- function(){
    # I <- -2.012e+00 # on small dat.nona (21 rows)
    I <- -1.833e+00
    P <- (input$parasitemia_density)/10000
    # logit <- I + 2.031e-02*P
    logit <- I + 1.809e-02*P
    exp(logit)/(1+exp(logit))
  }
  
  call_null <- function(){
    zero_outcome <- 0
    zero_outcome
  }
  
  #===============================================================================
  #                               COMPLEX MODEL                                 #
  #===============================================================================
  
  # MODEL 1a: GLM COMPLEX | PERCENTAGE | TOTAL WHITE BLOOD CELLS
  ## MODEL.log: -1.112e+00 + 5.324e-02*dat.nona$Percentage.parasitemia + (-7.415e-02)*dat.nona$Total.White.Cell.Count..x109.L.
  glm_complex <- function(){
    # I <- -1.112e+00 # on small dat.nona (21 rows)
    I <- -9.266e-01
    P <- input$parasitemia_percentage2
    W <- input$white_blood
    # logit <- I + 5.324e-02*P + (-7.415e-02)*W
    logit <- I + 4.200e-02*P + (-6.456e-02)*W
    exp(logit)/(1+exp(logit))
  }

  
  # MODEL 1b: GLM COMPLEX | DENSITY | TOTAL WHITE BLOOD CELLS
  # MODEL.log: -1.238e+00 + 1.676e-06*dat.nona$Parasite.density...µl. + (-6.638e-02)*dat.nona$Total.White.Cell.Count..x109.L.
  glm_complex_dens <- function(){
    # I <- -1.238e+00 # on small dat.nona (21 rows)
    I <- -1.358e+00
    P <- (input$parasitemia_density2)/10000
    W <- input$white_blood
    # logit <- I + 1.676e-02*P + (-6.638e-02)*W
    logit <- I + 1.701e-02*P + (-4.139e-02)*W
    exp(logit)/(1+exp(logit))
  }

  
  # # MODEL 1c: GLM COMPLEX | PERCENTAGE | DIFFERENT WHITE BLOOD CELL COUNTS #############---WRONG-----------
  # ## MODEL.log: -1.068e+00 + 6.155e-02*dat.nona$Percentage.parasitemia + (-5.826e-01)*dat.nona$Lymphocyte.count...x109.L. + (2.898e+00)*dat.nona$Monocyte.count...x109.L. + (-1.634e-01)*dat.nona$Neutrophil.count...x109.L.
  # glm_complex_counts <- function(){
  #   I <- -1.068e+00
  #   P <- input$parasitemia_percentage2
  #   L <- input$lympho
  #   M <- input$mono
  #   N <- input$neutro
  #   logit <- I + 6.155e-02*P + (-5.826e-01)*L + 2.898e+00*M + (-1.634e-01)*N
  #   exp(logit)/(1+exp(logit))
  # }
  # 
  # # MODEL 1D: GLM COMPLEX | DENSITY | DIFFERENT WHITE BLOOD CELL COUNTS #############---WRONG-----------
  # ## MODEL.log: -1.129e+00 + 1.744e-06*dat.nona$Parasite.density...µl. + (-4.876e-01)*dat.nona$Lymphocyte.count...x109.L. + (2.639e+00)*dat.nona$Monocyte.count...x109.L. + (-1.666e-01)*dat.nona$Neutrophil.count...x109.L.
  # glm_complex_counts_dens <- function(){
  #   I <- -1.129e+00
  #   P <- (input$parasitemia_density2)/10000
  #   L <- input$lympho
  #   M <- input$mono
  #   N <- input$neutro
  #   logit <- I + 0.01744*P + (-4.876e-01)*L + 2.639e+00*M + (-1.666e-01)*N
  #   exp(logit)/(1+exp(logit))
  # }
  
  # MODEL 1c: GLM COMPLEX | PERCENTAGE | DIFFERENT WHITE BLOOD CELL COUNTS #############---WRONG-----------
  ## MODEL.log: -1.068e+00 + 6.155e-02*dat.nona$Percentage.parasitemia + (-5.826e-01)*dat.nona$Lymphocyte.count...x109.L. + (2.898e+00)*dat.nona$Monocyte.count...x109.L. + (-1.634e-01)*dat.nona$Neutrophil.count...x109.L.
  glm_complex_counts <- function(){
    # I <- -1.112e+00 # on small dat.nona (21 rows)
    I <- -9.266e-01
    P <- input$parasitemia_percentage2
    W <- input$lympho + input$mono + input$neutro
    # logit <- I + 5.324e-02*P + (-7.415e-02)*W
    logit <- I + 4.200e-02*P + (-6.456e-02)*W
    exp(logit)/(1+exp(logit))
  }
  
  # MODEL 1D: GLM COMPLEX | DENSITY | DIFFERENT WHITE BLOOD CELL COUNTS #############---WRONG-----------
  ## MODEL.log: -1.129e+00 + 1.744e-06*dat.nona$Parasite.density...µl. + (-4.876e-01)*dat.nona$Lymphocyte.count...x109.L. + (2.639e+00)*dat.nona$Monocyte.count...x109.L. + (-1.666e-01)*dat.nona$Neutrophil.count...x109.L.
  glm_complex_counts_dens <- function(){
    # I <- -1.238e+00 # on small dat.nona (21 rows)
    I <- -1.358e+00
    P <- (input$parasitemia_density2)/10000
    W <- input$lympho + input$mono + input$neutro
    # logit <- I + 1.676e-02*P + (-6.638e-02)*W
    logit <- I + 1.701e-02*P + (-4.139e-02)*W
    exp(logit)/(1+exp(logit))
  }
  
  
  
  
  #===============================================================================
  #                               REACTIVE FUNCTIONS                             #
  #===============================================================================
  
  
  
  
  #===============================================================================
  #                               OUTPUT TEXT                                    #
  #===============================================================================
  
  # # File containing unique geo codes, state,city, zip
  # output$dummy <- read.csv("Rdata/dummy.csv", header = TRUE)
  
  ######## ONLY UPON CLICKING ACTION BUTTON....
  output$task <- renderText({
    paste(input$date, input$user_name, sep = ", \n")
  })
  
  output$dummy <- renderTable(dummy)
  output$summary_simple <- renderTable(summary_simple)
  output$summary_complex <- renderTable(summary_complex)
  
  # output$table.output <- renderTable({
  #   
  #   inFile <- input$file1
  #   
  #   if (is.null(inFile))
  #     return(NULL)
  #   
  #   tbl <- read.csv(inFile$datapath, header=input$header, sep=input$sep,  dec = input$dec)
  #   
  #   return(tbl)
  # })
  #===============================================================================
  #                               SIMPLE MODEL                                   #
  #===============================================================================
  #------- UPON CLICKING 'SIMPLE MODEL'...
  # observeEvent(input$go_simple, {
    
    # COMPUTE PREDICTION SIMPLE MODEL ----
    
    output$comp_text <- renderText({
      
      ptype <- paste(input$ptype)
      ptype2 <- paste(input$ptype2)
      wtype <- paste(input$wtype)
      word <- paste0(unlist(strsplit(ptype, ""))[2:length(unlist(strsplit(ptype, "")))], collapse = "")
      
      if(input$tabset == "simple"){
      paste("Simple model with", word, "of parasitemia: \n",
            "Reads that will map to pathogen: ", round(100*glm_both_simple(ptype), digits=2), "%", "\n",
            "Reads that will map to host:     ", round(100*(1 - glm_both_simple(ptype)), digits=2), "%")
      }
      else if(input$tabset == "complex"){
        if(ptype2 == "ppercentage2" & wtype == "white_blood"){
          paste("Complex model with percentage of parasitemia and total number of white cells: \n",
                "Reads that will map to pathogen: ", round(100*glm_complex(), digits=2), "%", "\n",
                "Reads that will map to host:     ", round(100*(1 - glm_complex()), digits=2), "%")
        }
        else if(ptype2 == "pdensity2" & wtype == "white_blood"){
          paste("Complex model with parasitemia density and total number of white cells: \n",
                "Reads that will map to pathogen: ", round(100*glm_complex_dens(), digits=2), "%", "\n",
                "Reads that will map to host:     ", round(100*(1 - glm_complex_dens()), digits=2), "%")
        }
        else if(ptype2 == "ppercentage2" & wtype == "counts"){
          paste("Complex model with percentage of parasitemia and white cell type counts: \n",
                "Reads that will map to pathogen: ", round(100*glm_complex_counts(), digits=2), "%", "\n",
                "Reads that will map to host:     ", round(100*(1 - glm_complex_counts()), digits=2), "%")
        }
        else if(ptype2 == "pdensity2" & wtype == "counts"){
          paste("Complex model with parasitemia density and white cell type counts: \n",
                "Reads that will map to pathogen: ", round(100*glm_complex_counts_dens(), digits=2), "%", "\n",
                "Reads that will map to host:     ", round(100*(1 - glm_complex_counts_dens()), digits=2), "%")
        }
      }
    })
    
    # output$comp_simple <- renderText({
    #   # paste(input$ptype)
    #   paste("Simple model with percentage of parasitemia: \n",
    #         "Reads that will map to pathogen: ", round(100*glm_simple(), digits=2), "%", "\n",
    #         "Reads that will map to host:     ", round(100*(1 - glm_simple()), digits=2), "%")
    # })
    # 
    # output$comp_simple_dens <- renderText({
    #   paste("Simple model with parasitemia density: \n",
    #         "Reads that will map to pathogen: ", round(100*glm_simple_dens(), digits=2), "%", "\n",
    #         "Reads that will map to host:      ", round(100*(1 - glm_simple_dens()), digits=2), "%")
    # })
  # }) # en actionbutton
  
  # PLOT SIMPLE MODEL SUMMARY ----
  # output$plot_simple <- renderPlot({
  #   plot(fit.nona.paras)
  # })
  # output$plot_simple_dens <- renderPlot({
  #   plot(fit.nona.paras.dens)
  # })
  
  # SUMMARY OF THE SIMPLE MODEL ----
  output$summary_simple <- renderPrint({
    # dataset <- datasetInput()
    # summary(dataset)
    summary(fit.nona.paras)
  })
  output$summary_simple_dens <- renderUI({
    summary(fit.nona.paras.dens)
  })
  
  #===============================================================================
  #                               COMPLEX MODEL                                  #
  #===============================================================================
  #------- UPON CLICKING 'COMPLEX MODEL'...
  # observeEvent(input$go_complex, {
    
    # COMPUTE PREDICTION COMPLEX MODEL ----
    output$comp_complex <- renderText({
      paste("Complex model with percentage of parasitemia and total number of white cells: \n",
            "Reads that will map to pathogen: ", round(100*glm_complex(), digits=2), "%", "\n",
            "Reads that will map to host:      ", round(100*(1 - glm_complex()), digits=2), "%")
    })
    output$comp_complex_dens <- renderText({
      paste("Complex model with parasitemia density and total number of white cells: \n",
            "Reads that will map to pathogen: ", round(100*glm_complex_dens(), digits=2), "%", "\n",
            "Reads that will map to host:      ", round(100*(1 - glm_complex_dens()), digits=2), "%")
    })
    output$comp_complex_counts <- renderText({
      paste("Complex model with percentage of parasitemia and white cell type counts: \n",
            "Reads that will map to pathogen: ", round(100*glm_complex_counts(), digits=2), "%", "\n",
            "Reads that will map to host:      ", round(100*(1 - glm_complex_counts()), digits=2), "%")
    })
    output$comp_complex_counts_dens <- renderText({
      paste("Complex model with parasitemia density and white cell type counts: \n",
            "Reads that will map to pathogen: ", round(100*glm_complex_counts_dens(), digits=2), "%", "\n",
            "Reads that will map to host:      ", round(100*(1 - glm_complex_counts_dens()), digits=2), "%")
    })
  # })
  
  #===============================================================================
  #                               OUTPUT GRAPHS                                  #
  #===============================================================================
  
  # output$residuals <- renderPlot({
  #   hist(rnorm(200))
  # })

  output$residuals <- renderPlotly({
    # if(input$tabset == "simple"){
    # ptype <- input$ptype
    ptype <- if(input$tabset == "simple") input$ptype else input$ptype2
    # plot_data <- if(input$ptype == "ppercentage") fit.nona.paras else if(input$ptype == "pdensity") fit.nona.paras.dens
    plot_data <- if(ptype == "ppercentage" || ptype == "ppercentage2") fit.nona.paras else fit.nona.paras.dens
    # limit <- if(input$ptype == "ppercentage"){limit=100} else if(input$ptype == "pdensity"){limit=3000000}
    limit <- if(ptype == "ppercentage" || ptype == "ppercentage2"){limit=100} else {limit=3000000}
    ggplotRegression(plot_data, glm_both_simple(ptype), limit, ptype)
    # }
    # else{
    #   renderUI({
    #     summary(fit.nona.paras.dens)
    #   })
    # }
  })
  
    # # attempts to make different plots relative to which tab is open
    # output$residuals <- renderPlotly({
    #   # if(input$tabset == "simple"){
    #   ptype <- input$ptype
    #   if(input$tabset == "simple" && input$ptype == "ppercentage"){
    #     plot_data <- fit.nona.paras
    #     limit <- 100
    #     ggplotRegression(plot_data, glm_both_simple(ptype), limit, ptype)
    #     }
    #     else if(input$tabset == "simple" && input$ptype == "pdensity"){
    #     plot_data <- fit.nona.paras.dens
    #     limit <- 3000000
    #     ggplotRegression(plot_data, glm_both_simple(ptype), limit, ptype)
    #     }
    #   else{
    #   }
    #   
    #   # # attempts to include 2 regression plots for the complex model
    #   # else if(input$tabset == "complex" && input$ptype == "ppercentage2"){
    #   #   wtype <- input$wtype
    #   #   plot_data <- fit.nona.total
    #   #   limit <- 100
    #   #   ggplotRegression2(plot_data, glm_both_simple(ptype), limit, ptype)
    #   # }
    #   # else if(input$tabset == "complex" && input$ptype == "pdensity2"){
    #   #   wtype <- input$wtype
    #   #   plot_data <- fit.nona.total.dens
    #   #   limit <- 3000000
    #   #   ggplotRegression2(plot_data, glm_both_simple(ptype), limit, ptype)
    #   # }
    #   # }
    #   # else{
    #   #   renderUI({
    #   #     summary(fit.nona.paras.dens)
    #   #   })
    #   # }
    # })
    # 
    
  # output$help <- helpPopup("How to use", "blabla",
  #                                                placement=c('right'),
  #                                                trigger=c('hover'))
  
  ###########################################
  #       # -------------------------------------------------------------------
  #       # Linked plots (middle and right)
  #       ranges2 <- reactiveValues(x = NULL, y = NULL)
  #       
  #       output$plot2 <- renderPlot({
  #         ggplot(mtcars, aes(wt, mpg)) +
  #           geom_point()
  #       })
  #       
  #       output$plot3 <- renderPlot({
  #         ggplot(mtcars, aes(wt, mpg)) +
  #           geom_point() +
  #           coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
  #       })
  #       
  #       # When a double-click happens, check if there's a brush on the plot.
  #       # If so, zoom to the brush bounds; if not, reset the zoom.
  #       observe({
  #         brush <- input$plot2_brush
  #         if (!is.null(brush)) {
  #           ranges2$x <- c(brush$xmin, brush$xmax)
  #           ranges2$y <- c(brush$ymin, brush$ymax)
  #           
  #         } else {
  #           ranges2$x <- NULL
  #           ranges2$y <- NULL
  #         }
  #       })
  # ###########################################
  
  ### Saving data:
  Rawdata <- reactive({
    input$refresh
    input$refresh2
    
    slope <- input$slope
    SD <- input$SD
    sample <- input$sample
    x <- round(1:sample + rnorm(n = sample, mean = 1, sd = 2), digits = 2)
    y <- round(slope * (x) + rnorm(n = sample, mean = 3, sd = SD ), digits = 2)
    mod <- lm(y ~ x, data.frame(y,x))
    ypred <- predict(mod)
    Rawdata <- data.frame(y, x, ypred)
  })
  
  SSdata <- reactive({
    dat <- Rawdata()
    Y <- mean(dat$y)
    mod <- lm(y ~ x, dat)
    ypred <- predict(mod)
    dat$ypred <- ypred
    SST <- sum((dat$y - Y)^2)
    SSE <- round(sum((dat$y - ypred)^2), digits = 5)
    SSA <- SST - SSE
    
    SSQ <- data.frame(SS = c("Total","Regression","Error"),
                      value = as.numeric(c(SST, SSA, SSE)/SST)*100)
    SSQ$SS <- factor(SSQ$SS, as.character(SSQ$SS))
    SSdata <- data.frame(SS = factor(SSQ$SS, as.character(SSQ$SS)),
                         value = as.numeric(c(SST, SSA, SSE)/SST)*100)
    
  })
  
  
  
  ### First output "graphs"
  output$total <- renderPlot({
    cols <- c("#619CFF", "#00BA38", "#F8766D")
    ggplot(Rawdata(), aes(x=x,y=y))+
      geom_point(size=3) +
      geom_segment(xend = Rawdata()[,2], yend = mean(Rawdata()[,1]),
                   colour = "#619CFF")+
      geom_hline(yintercept = mean(Rawdata()[,1]))+
      theme(axis.title = element_text(size = 20),
            axis.text  = element_text(size = 18),
            panel.background=element_rect(fill="white",colour="black"))+
      ggtitle("SS total")
  })
  
  ### First output "graphs"
  output$regression <- renderPlot({
    cols <- c("#619CFF", "#00BA38", "#F8766D")
    ggplot(Rawdata(), aes(x=x,y=y))+
      geom_point(alpha=0)+
      geom_smooth(method= "lm", se = F, colour = "black")+
      geom_hline(yintercept = mean(Rawdata()[,1]))+
      geom_segment(aes(x=x,y=ypred), xend = Rawdata()[,2],
                   yend = mean(Rawdata()[,1]),
                   colour = "#00BA38")+
      theme(axis.title = element_text(size = 20),
            axis.text  = element_text(size = 18),
            panel.background=element_rect(fill="white",colour="black"))+
      ggtitle("SS regression")
    
  })
  
  ### First output "graphs"
  output$error <- renderPlot({
    cols <- c("#619CFF", "#00BA38", "#F8766D")
    ggplot(Rawdata(), aes(x = x, y = y))+
      geom_point(size=3) + geom_smooth(method= "lm", se = F,
                                       colour = "black")+
      geom_segment(xend = Rawdata()[,2], yend = Rawdata()[,3],
                   colour = "#F8766D")+
      theme(axis.title = element_text(size = 20),
            axis.text  = element_text(size = 18),
            panel.background=element_rect(fill="white",colour="black"))+
      ggtitle("SS error")
    
  })
  
  output$variance <- renderPlot({
    cols <- c("#619CFF", "#00BA38", "#F8766D")
    ggplot(SSdata(), aes(y = value, x = SS, fill = SS))+
      geom_bar(stat = "identity")+
      scale_fill_manual(values = cols)+
      theme(axis.title = element_text(size = 20),
            axis.text.x  = element_text(size = 0),
            axis.text.y  = element_text(size = 16),
            panel.background=element_rect(fill="white",colour="black")) +
      ylab("% of variance")+
      xlab("Sums of Squares")
    
  })
  
  
  output$reg <- renderPlot({
    ggplot(Rawdata(), aes(y = y, x = x))+
      geom_point(size = 3, colour = "blue", alpha = .5)+
      geom_smooth(method = "lm")+
      theme(axis.title = element_text(size = 20),
            axis.text  = element_text(size = 16),
            panel.background=element_rect(fill="white",colour="black")) +
      ylab("Y")+
      xlab("X")
    
  })
  
  ### Second output "anova"
  output$anova <- renderTable({
    anova(lm(y ~ x, Rawdata()))
  })
  
  ### Second output "SS"
  output$summary <- renderTable({
    summary(lm(y ~ x, Rawdata()))
    
  })
  
  # output$data <- renderDataTable(
  #   Rawdata()[c(1,2)], options = list(
  #     searchable = FALSE, searching = FALSE, pageLength = 100))
  
  output$histogram <- renderPlot({
    d1 <- ggplot(Rawdata(), aes(y = y, x = x))+
      geom_point(size = 3, colour = "blue", alpha = .5)+
      theme(axis.title = element_text(size = 20),
            axis.text  = element_text(size = 16),
            panel.background=element_rect(fill="white",colour="black")) +
      ylab("Y")+
      xlab("X")
    ggMarginal(
      d1,
      type = 'histogram')
  })
  
  #===============================================================================
  #                               HOVERING OVER BUTTONS                          #
  #===============================================================================
  # addTooltip(session, id = "go_simple", title = "Click here to compute a prediction!",
  #            placement = "left", trigger = "hover")
  
  observeEvent(input$help_icon, {
    # Show a modal when the button is pressed
    shinyalert("How to use this web tool?", 
                "You can either choose between the simple or the complex model to make a prediction depending on the type of your data. 
                \n Then enter your data via the slider(s). 
                \n You can bookmark the state which automatically saves the values of all inputs.
                \n Optionally, you can enter the date and your name, so that this information will be bookmarked as well.
               ")
  })
  
  observeEvent(input$F_help, {
    shinyalert("What is a F-test?",
               "The F-test tests whether to reject the smaller reduced model (ie the intercept only model in case of the simple model) in favour of the larger full model.
               \n Therefore, we aim for a significant p-value (< 0.05) that indicates to reject the null (ie intercept model) in favour of the larger model.")
  })
  
  observeEvent(input$Rsq_help, {
    shinyalert("What is a R-squared value?",
               "The R-squared value measures how much variation in the output variable is explained 
                by the input variable(s). This means that (1- (R-squared)) of the variation is unaccounted for.
                The majority of the rest is due to noise and possibly measurement errors.
                \n The adjusted R-squared is a modified version of R-squared. It has been adjusted for the number of predictors in the model.
                \n The R-squared values in our simple model(s) do not seem very high; however, it does not assess the goodness-of-fit.")
  })
  
  observeEvent(input$help_anova, {
    shinyalert("What is ANOVA?",
               "Analysis of variance (ANOVA) is a collection of statistical models used to analyse 
               the differences among group means in a sample. Here we use it for model comparison. 
               You can choose between different tests: Chi-Squared, F and Rao."
               # In its simplest form, ANOVA provides a statistical test of whether the population means of several groups are equal, 
               # and therefore generalizes the t-test to more than two groups. 
               # ANOVA is useful for comparing (testing) three or more group means for statistical significance.
               )
  })
  
  observeEvent(input$help_likelihoodratio, {
    shinyalert("What is the likelihood ratio test?",
               "The likelihood ratio test compares two different models in terms of the goodness-of-fit: the null (here the intercept only model) model against an alternative model.
                \n It uses the likelihood ratio, which measures how many times more likely the data are under one model than the other. 
                \n From this likelihood ratio we compute a p-value to decide whether or not to reject the null model.")
  })
  
  
  
  #===============================================================================
  #                               OUTPUT TEXT                                    #
  #===============================================================================
  
  output$link_text <- renderText({
    if(input$tabset == "complex"){
      paste("For a 3D scatterplot including the regression plane we suggest to follow the link")
    }
    else{
      paste()
    }
   })
  
  
  #===============================================================================
  #                               LINKS                                          #
  #===============================================================================
  
  url <- a("Interactive 3D regression tool", href="http://miabellaai.net/demo.html")
  output$tab <- renderUI({
    if(input$tabset == "complex"){
      tagList("here:", url)
    }
    else{}
  })
    
  #===============================================================================
  #                               OUTPUT TABLES                                  #
  #===============================================================================
  
  output$likelihoodratio_summary <- renderPrint({
      if (input$test_select == "simple_paras_lr"){
        lrtest(glm.paras.logit)
      }
      else if (input$test_select == "simple_paras_dens_lr"){
        lrtest(glm.paras.dens.logit)
      }
      else if (input$test_select == "complex_paras_lr"){
        lrtest(glm.total.logit)
      }
      else{
        lrtest(glm.total.dens.logit)
      }
  })

  likelihoodratio <- function(){
    if (input$lrt_select == "simple_paras_lr"){
      lrtest(glm.paras.logit)
    }
    else if (input$lrt_select == "simple_paras_dens_lr"){
      lrtest(glm.paras.dens.logit)
    }
    else if (input$lrt_select == "complex_paras_lr"){
      lrtest(glm.total.logit)
    }
    else{
      lrtest(glm.total.dens.logit)
    }
  }
  
  
  output$likelihoodratio_summary <- renderPrint({
    likelihoodratio()
  })
  
  # mapping <- function(model){
  #   if (model == "simple_paras1" || model == "simple_paras2"){
  #     return glm.paras.logit
  #   }
  # }
  # 
  # translator <- function(model1, model2, model3){
  #   if (model1=="null"){
  #     renderText({ 
  #       anova(model2, model3)
  #     })
  #     else{
  #       renderTest({
  #         return anova(model1, model2, model3)
  #     })
  #   }
  # }
  # }
  
  # glm.paras.logit
  # glm.paras.dens.logit
  # glm.total.logit
  # glm.total.dens.logit
    
  # output$anova_summary <- function(){
  #   if(input$model1_select == "no_null"){
  #     anova(input$model2_select, input$model3_select)
  #     }
  #     else {
  #       anova(input$model1_select, input$model2_select, input$model3_select)
  #     }
  # }
  
  # output$anova_summary <- tanslator(input$model1_select, input$model2_select, input$model3_select)
  

  #===============================================================================
  #                               BOOKMARKS                                      #
  #===============================================================================
  
  # Need to exclude the buttons from themselves being bookmarked
  setBookmarkExclude(c("bookmark1", "bookmark2"))
  
  # Trigger bookmarking with either button
  observeEvent(input$bookmark1, {
    session$doBookmark()
  })
  observeEvent(input$bookmark2, {
    session$doBookmark()
  })
  
  output$power <- renderUI({
    withMathJax(helpText('$$10^9$$'))
  })
  
}



#===============================================================================
#                        MODELS                                                #
#===============================================================================




# 
#   ##############################
#   # PLOT TOTAL MODEL ----
#   output$plot_total <- renderPlot({
#     
#     # fit.nona.total <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$total.number.of.cells, data=dat.nona)
#     # summary(fit.nona.total) # show results: R^2: 0.4622
#     # summary(fit.nona.total)$sigma^2 # estimated variance of residuals around a fitted line: 0.02268394
#     # 
#     # # plot the statistics, OUTLIERS 35, 39 -- both in UM group? -- kept them -- BUT MIGHT BE WORTH TRYING WITHOUT THEM
#     par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
#     plot(fit.nona.total)  # Plot the model information
#   })
#   
#   # fit.nona.total.L
#   # fit.nona.total.M
#   # fit.nona.total.N
#   
#   # SUMMARY OF THE COMPLEX MODEL ----
#   output$summary_total <- renderPrint({
#     # dataset <- datasetInput()
#     # summary(dataset)
#     summary(fit.nona.total)
#   })
#   
#   #  }
#   #)
#   
# 
# 
# })