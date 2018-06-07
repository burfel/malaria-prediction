# read map prediction
# Predicts the percentage of reads that map to host and pathogen
# Felicia Burtscher
# Date: June 6, 2018

# server.R

#===============================================================================
#                               SHINYSERVER                                    #
#===============================================================================

server <- function(input, output) {
  
  # ### Saving data:
  # Rawdata <- reactive({
  #   input$refresh 
  #   input$refresh2 
  #   
  #   slope <- input$slope
  #   SD <- input$SD
  #   sample <- input$sample
  #   x <- round(1:sample + rnorm(n = sample, mean = 1, sd = 2), digits = 2)
  #   y <- round(slope * (x) + rnorm(n = sample, mean = 3, sd = SD ), digits = 2)
  #   mod <- lm(y ~ x, data.frame(y,x))
  #   ypred <- predict(mod)
  #   Rawdata <- data.frame(y, x, ypred)
  # })
  # 
  # SSdata <- reactive({
  #   dat <- Rawdata()
  #   Y <- mean(dat$y)
  #   mod <- lm(y ~ x, dat)
  #   ypred <- predict(mod)
  #   dat$ypred <- ypred
  #   SST <- sum((dat$y - Y)^2)
  #   SSE <- round(sum((dat$y - ypred)^2), digits = 5)
  #   SSA <- SST - SSE
  #   
  #   SSQ <- data.frame(SS = c("Total","Regression","Error"),
  #                     value = as.numeric(c(SST, SSA, SSE)/SST)*100)
  #   SSQ$SS <- factor(SSQ$SS, as.character(SSQ$SS))
  #   SSdata <- data.frame(SS = factor(SSQ$SS, as.character(SSQ$SS)),
  #                        value = as.numeric(c(SST, SSA, SSE)/SST)*100)
  #   
  # })
  # 
  # 
  # 
  # ### First output "graphs"
  # output$total <- renderPlot({
  #   cols <- c("#619CFF", "#00BA38", "#F8766D")
  #   ggplot(Rawdata(), aes(x=x,y=y))+
  #     geom_point(size=3) +
  #     geom_segment(xend = Rawdata()[,2], yend = mean(Rawdata()[,1]),
  #                  colour = "#619CFF")+
  #     geom_hline(yintercept = mean(Rawdata()[,1]))+
  #     theme(axis.title = element_text(size = 20),
  #           axis.text  = element_text(size = 18),
  #           panel.background=element_rect(fill="white",colour="black"))+
  #     ggtitle("SS total")
  # })
  # 
  # ### First output "graphs"
  # output$regression <- renderPlot({
  #   cols <- c("#619CFF", "#00BA38", "#F8766D")
  #   ggplot(Rawdata(), aes(x=x,y=y))+
  #     geom_point(alpha=0)+
  #     geom_smooth(method= "lm", se = F, colour = "black")+
  #     geom_hline(yintercept = mean(Rawdata()[,1]))+
  #     geom_segment(aes(x=x,y=ypred), xend = Rawdata()[,2],
  #                  yend = mean(Rawdata()[,1]),
  #                  colour = "#00BA38")+
  #     theme(axis.title = element_text(size = 20),
  #           axis.text  = element_text(size = 18),
  #           panel.background=element_rect(fill="white",colour="black"))+
  #     ggtitle("SS regression")
  #   
  # })
  # 
  # ### First output "graphs"
  # output$error <- renderPlot({
  #   cols <- c("#619CFF", "#00BA38", "#F8766D")
  #   ggplot(Rawdata(), aes(x = x, y = y))+
  #     geom_point(size=3) + geom_smooth(method= "lm", se = F,
  #                                      colour = "black")+
  #     geom_segment(xend = Rawdata()[,2], yend = Rawdata()[,3],
  #                  colour = "#F8766D")+
  #     theme(axis.title = element_text(size = 20),
  #           axis.text  = element_text(size = 18),
  #           panel.background=element_rect(fill="white",colour="black"))+
  #     ggtitle("SS error")
  #   
  # })
  # 
  # output$variance <- renderPlot({
  #   cols <- c("#619CFF", "#00BA38", "#F8766D")
  #   ggplot(SSdata(), aes(y = value, x = SS, fill = SS))+
  #     geom_bar(stat = "identity")+
  #     scale_fill_manual(values = cols)+
  #     theme(axis.title = element_text(size = 20),
  #           axis.text.x  = element_text(size = 0),
  #           axis.text.y  = element_text(size = 16),
  #           panel.background=element_rect(fill="white",colour="black")) +
  #     ylab("% of variance")+
  #     xlab("Sums of Squares")
  #   
  # })
  # 
  # 
  # output$reg <- renderPlot({
  #   ggplot(Rawdata(), aes(y = y, x = x))+
  #     geom_point(size = 3, colour = "blue", alpha = .5)+
  #     geom_smooth(method = "lm")+
  #     theme(axis.title = element_text(size = 20),
  #           axis.text  = element_text(size = 16),
  #           panel.background=element_rect(fill="white",colour="black")) +
  #     ylab("Y")+
  #     xlab("X")
  #   
  # })
  # 
  # ### Second output "anova"
  # output$anova <- renderTable({
  #   anova(lm(y ~ x, Rawdata()))
  # })
  # 
  # ### Second output "SS"
  # output$summary <- renderTable({
  #   summary(lm(y ~ x, Rawdata()))
  #   
  # })
  # 
  # output$data <- renderDataTable(
  #   Rawdata()[c(1,2)], options = list(
  #     searchable = FALSE, searching = FALSE, pageLength = 100))
  # 
  # output$histogram <- renderPlot({
  #   d1 <- ggplot(Rawdata(), aes(y = y, x = x))+
  #     geom_point(size = 3, colour = "blue", alpha = .5)+
  #     theme(axis.title = element_text(size = 20),
  #           axis.text  = element_text(size = 16),
  #           panel.background=element_rect(fill="white",colour="black")) +
  #     ylab("Y")+
  #     xlab("X")
  #   ggMarginal(
  #     d1,
  #     type = 'histogram')
  # })
  
  
}



  
  #===============================================================================
  #                        MODELS                                                #
  #===============================================================================

#   # MODEL 0a: GLM SIMPLE | PERCENTAGE 
#   ## MODEL.log: -1.964e+00 + 6.550e-02*dat.nona$Percentage.parasitemia
#   glm_simple <- function(){
#     I <- -1.964e+00
#     P <- input$parasitemia_percentage
#     #W <- input$white_blood
#     logit <- I + 6.550e-02*P
#     exp(logit)/(1+exp(logit))
#   }
#   
#   
#   # MODEL 0b: GLM SIMPLE | DENSITY
#   ## MODEL.log: -2.012e+00 + 2.031e-06*dat.nona$Parasite.density...µl.
#   glm_simple_dens <- function(){
#     I <- 2.012e+00
#     P <- input$parasitemia_density
#     logit <- I + 2.031e-06*P
#     exp(logit)/(1+exp(logit))
#   }
#   
#   
#   # MODEL 1a: GLM COMPLEX | PERCENTAGE | TOTAL WHITE BLOOD CELLS
#   ## MODEL.log: -1.112e+00 + 5.324e-02*dat.nona$Percentage.parasitemia + (-7.415e-02)*dat.nona$Total.White.Cell.Count..x109.L.
#   glm_complex <- function(){
#     I <- -1.112e+00
#     P <- input$parasitemia_percentage2
#     W <- input$white_blood
#     I + 5.324e-02*P + (-7.415e-02)*W
#     exp(logit)/(1+exp(logit))
#   }
#   
#   
#   # MODEL 1b: GLM COMPLEX | DENSITY | TOTAL WHITE BLOOD CELLS
#   # MODEL.log: -1.238e+00 + 1.676e-06*dat.nona$Parasite.density...µl. + (-6.638e-02)*dat.nona$Total.White.Cell.Count..x109.L.
#   glm_complex_dens <- function(){
#     I <- -1.238e+00
#     P <- input$parasitemia_density2
#     W <- input$white_blood
#     I + 1.676e-06*P + (-6.638e-02)*W
#     exp(logit)/(1+exp(logit))
#   }
#   
#   # MODEL 1c: GLM COMPLEX | PERCENTAGE | DIFFERENT WHITE BLOOD CELL COUNTS
#   ## MODEL.log: -1.068e+00 + 6.155e-02*dat.nona$Percentage.parasitemia + (-5.826e-01)*dat.nona$Lymphocyte.count...x109.L. + (2.898e+00)*dat.nona$Monocyte.count...x109.L. + (-1.634e-01)*dat.nona$Neutrophil.count...x109.L.
#   glm_complex_counts <- function(){
#     I <- -1.068e+00
#     P <- input$parasitemia_percentage2
#     L <- input$lympho
#     M <- input$mono
#     N <- input$neutro
#     I + 6.155e-02*P + (-5.826e-01)*L + 2.898e+00*M + (-1.634e-01)*N
#     exp(logit)/(1+exp(logit))
#   }
#   
#   # MODEL 1D: GLM COMPLEX | DENSITY | DIFFERENT WHITE BLOOD CELL COUNTS
#   ## MODEL.log: -1.129e+00 + 1.744e-06*dat.nona$Parasite.density...µl. + (-4.876e-01)*dat.nona$Lymphocyte.count...x109.L. + (2.639e+00)*dat.nona$Monocyte.count...x109.L. + (-1.666e-01)*dat.nona$Neutrophil.count...x109.L.
#   glm_complex_counts_dens <- function(){
#     I <- -1.129e+00
#     P <- input$parasitemia_density2
#     L <- input$lympho
#     M <- input$mono
#     N <- input$neutro
#     I + 1.744e-06*P + (-4.876e-01)*L + 2.639e+00*M + (-1.666e-01)*N
#     exp(logit)/(1+exp(logit))
#   }
#   
#   #------- UPON CLICKING 'SIMPLE MODEL'... 
#   # observeEvent(input$go_simple, {
#   
#   # COMPUTE PREDICTION SIMPLE MODEL ----
#   output$comp_simple <- renderText({
#     paste("Prediction:", br(), "Percentage of reads that will map to pathogen: ", 100*glm_simple(), br(), "Percentage of reads that will map to host: ", 100*(1 - glm_simple()))
#   })
#   
#   output$comp_simple_dens <- renderText({
#     paste("Prediction:", br(), "Percentage of reads that will map to pathogen: ", 100*glm_simple_dens(), br(), "Percentage of reads that will map to host: ", 100*(1 - glm_simple_dens()))
#   })
#   
#   # PLOT SIMPLE MODEL SUMMARY ----
#   output$plot_simple <- renderPlot({
#     plot(fit.nona.paras)
#   })
#   output$plot_simple_dens <- renderPlot({
#     plot(fit.nona.paras.dens)
#   })
#   
#   # SUMMARY OF THE SIMPLE MODEL ----
#   output$summary_simple <- renderUI({
#     # dataset <- datasetInput()
#     # summary(dataset)
#     summary(fit.nona.paras)
#   })
#   output$summary_simple_dens <- renderUI({
#     summary(fit.nona.paras.dens)
#   })
#   
#   #   }
#   # )
#   
#   
#   #------- UPON CLICKING 'COMPLEX MODEL'...
#   #observeEvent(input$go_complex, {
#   
#   # COMPUTE PREDICTION COMPLEX MODEL ----
#   output$comp_complex <- renderText({
#     paste("Prediction:", br(), "Percentage of reads that will map to pathogen: ", 100*glm_complex(), br(), "Percentage of reads that will map to host: ", 100*(1 - glm_complex()))
#   })
#   
#   output$comp_complex_dens <- renderText({
#     paste("Prediction:", br(), "Percentage of reads that will map to pathogen: ", 100*glm_complex_dens(), br(), "Percentage of reads that will map to host: ", 100*(1 - glm_complex_dens()))
#   })
#   
#   output$comp_complex_counts <- renderText({
#     paste("Prediction:", br(), "Percentage of reads that will map to pathogen: ", 100*glm_complex_counts(), br(), "Percentage of reads that will map to host: ", 100*(1 - glm_complex_counts()))
#   })
#   
#   output$comp_complex_counts_dens <- renderText({
#     paste("Prediction:", br(), "Percentage of reads that will map to pathogen: ", 100*glm_complex_counts_dens(), br(), "Percentage of reads that will map to host: ", 100*(1 - glm_complex_counts_dens()))
#   })
#   
#   
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