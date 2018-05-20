library(shiny)
library(ggplot2)
library(shinydashboard)
library(grid)
library(markdown)
library(ggExtra)


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
              box(width = 60, title = "Parameters",
                  solidHeader = TRUE, status = "primary",
                  
                  sidebarPanel(
                    helpText("Choose the parameters. You can choose between parasetemia percentage and parasitemia density."),
                    
                  sliderInput(inputId = "white-blood",
                              label = "Total number of white blood cells (x 10^9/ L)",
                              value = 9, min = 0, max = 20, step = .25),
                  sliderInput(inputId = "red-blood",
                              label = "Total number of red blood cells (x 10^12/ L)",
                              value = 4, min = 0, max = 10, step = .25),
                  sliderInput(inputId = "percentage-parasitemia",
                              label = "Percentage of parasitemia",
                              value = 8, min = 0, max = 100, step =1.),
                  sliderInput(inputId = "parasetemia-density",
                              label = "Parasitemia density (/ql)",
                              value = 800000, min = 0, max = 1500000, step = 1000)
                  )
              )
              
              # mainPanel(
              #   
              #   box(width = 6,
              #       title = "Regression",
              #       solidHeader = TRUE, status = "primary",
              #       plotOutput(outputId = "covariance"))
              #   
              # )
            )
      ),
  

              
              # mainPanel(
              #   img(src='img/mouseData_cov.png', align = "right"),
              #   ### the rest of your code
              # )
    
    # TAB 
    tabItem(tabName = "abstract",
            fluidPage(
              box(width = 10,status = "success",
                  shiny::includeMarkdown("md/abstract.md"))
            )
    ),
    
    # TAB 
    tabItem(tabName = "introduction",
            fluidPage(
              box(width = 10,status = "success",
                  shiny::includeMarkdown("md/introduction.md"))
            )
    ),
    
    # TAB 
    tabItem(tabName = "methods",
            fluidPage(
              box(width = 10,status = "success",
                  shiny::includeMarkdown("md/methods.md"))
            )
    ),
    
    # TAB 
    tabItem(tabName = "discussion",
            fluidPage(
              box(width = 10,status = "success",
                  shiny::includeMarkdown("md/discussion.md"))
            )
    ),
    
    # TAB
    tabItem(tabName = "conclusion",
            fluidPage(
              box(width = 10,status = "success",
                  shiny::includeMarkdown("md/conclusion.md"))
            )
    ),
    
    # TAB 
    tabItem(tabName = "data",
            fluidPage(
              box(width = 10,status = "success",
                  shiny::includeMarkdown("md/data.md"))
            )
    ),
    
    # TAB 
    tabItem(tabName = "references",
            fluidPage(
              box(width = 10,status = "success",
                  shiny::includeMarkdown("md/references.md"))
            )
    ),
    
    # TAB 
    tabItem(tabName = "glossary",
            fluidPage(
              box(width = 10,status = "success",
                  shiny::includeMarkdown("md/glossary.md"))
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

server <- function(input, output) {}

shinyApp(ui = ui, server = server)

