library(shiny)
library(shinydashboard)

### Title:

header <- dashboardHeader(title = "Mapped reads prediction")

### SideBar:
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Abstract", tabName = "abstract", icon = icon("fa fa-circle")),
    menuItem("Introduction", tabName = "introduction", icon = icon("fa fa-circle")),
    menuItem("Methods", tabName = "methods", icon = icon("fa fa-circle")),
    menuItem("Results", tabName = "results", icon = icon("fa fa-circle")),
    menuItem("Discussion", tabName = "discussion", icon = icon("fa fa-circle")),
    menuItem("Conclusion", tabName = "conclusion", icon = icon("fa fa-circle")),
    #menuItem("Introduction", tabName = "introduction", icon = icon("fa fa-circle")),
    #menuItem("Graphs", tabName = "graphs", icon = icon("fa fa-circle")),
    menuItem("Raw Data", tabName = "data", icon = icon("fa fa-circle")),
    menuItem("References", tabName = "references", icon = icon("fa fa-circle")),
    menuItem("Glossary", tabName = "glossary", icon = icon("fa fa-circle")),
    menuItem("About", tabName = "about", icon = icon("fa fa-info-circle"))
  )
)


### Dashboard:
body <- dashboardBody(
  
  
  
  ### Tabintes:
  
  tabItems(
    
    ### TAB 1 = dashboard:
    tabItem(tabName = "results",
            
            fluidRow(
              
              # Sample size slider
              box(width = 4, title = "Parameters",
                  solidHeader = TRUE, status = "primary",
                  
                  sliderInput(inputId = "sample",
                              label = "Sample size",
                              value = 50, min = 10, max = 100),
                  sliderInput(inputId = "slope",
                              label = "Regression slope",
                              value = .25, min = -2, max = 2,step = .25),
                  # Sd slider:
                  sliderInput(inputId = "SD",
                              label = "Standard deviation",
                              value = 3, min = 0, max = 50),
                  actionButton(inputId = "refresh", label = "Simulate New Data" , 
                               icon = icon("fa fa-refresh"))
              ),
              
              mainPanel(
                
                box(width = 6,
                    title = "Regression",
                    solidHeader = TRUE, status = "primary",
                    plotOutput(outputId = "reg")),
                
                
                box(width = 6,title = "Sums of Squares Graphs",
                    solidHeader = F, status = "primary",
                    tabsetPanel(type = "tabs",
                                tabPanel("Total", plotOutput("total")),
                                tabPanel("Regression", plotOutput("regression")),
                                tabPanel("Error", plotOutput("error")),
                                tabPanel("Variance Partition", plotOutput(("variance")))
                                
                    )
                )
              ),
              fluidRow(
                box(width = 6,title = "Anova Table",
                    solidHeader = FALSE, status = "warning",
                    tableOutput(outputId = "anova")),
                
                box(width = 6,title = "Summary",
                    solidHeader = FALSE, status = "warning",
                    tableOutput(outputId = "summary")))
            )),
    
    # TAB 2 = dashboard:
    
    tabItem(tabName = "data",
            fluidRow(
              box(width = 4, solidHeader = TRUE, status = "primary",
                  title = "Raw Data",
                  dataTableOutput(outputId = "data")),
              box(width = 6, solidHeader = TRUE, status = "primary",
                  title = "Data distribution",
                  plotOutput(outputId = "histogram"),
                  actionButton(inputId = "refresh2", label = "Simulate New Data" , 
                               icon = icon("fa fa-refresh")))
              
            )
    ),
    
    # TAB 3 = About
    tabItem(tabName = "about",
            fluidPage(
              box(width = 10,status = "success",
                  shiny::includeMarkdown("README.md"))
            )
    )
  )
)

ui <- dashboardPage(header, sidebar, body)



#########


library(shiny)
library(ggplot2)
library(shinydashboard)
library(grid)
library(markdown)
library(ggExtra)

server <- function(input, output) {
  
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
  
  output$data <- renderDataTable(
    Rawdata()[c(1,2)], options = list(
      searchable = FALSE, searching = FALSE, pageLength = 100))
  
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
  
  
}


shinyApp(ui = ui, server = server)
         
         