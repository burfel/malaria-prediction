library(shiny)
library(ggplot2)
library(shinydashboard)
library(grid)
library(markdown)
library(ggExtra)


####

#source('../src/databuilder.R', local = TRUE)
#source('../src/databuilder2.R', local = TRUE)

#fields <- c("name","age","height","weight")

### Title:

header <- dashboardHeader(title = "Mapped reads prediction")

### SideBar:
sidebar <- dashboardSidebar(
  sidebarMenu(
    # ##### ADD LAY-SUMMARY
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
                    helpText("You can choose between the simple and a more complex model."),
                  
                    
                    tabsetPanel(id="tabset",
                          
                      tabPanel("Simple model",
                        
                        helpText("Choose the parameters. You can choose between parasetemia percentage and parasitemia density."),
                        
                        # Input: Select the random distribution type ----
                        # radioButtons("ptype_visible", "Which data do you have?",
                        #              c("Percentage of parasetemia" = "ppercentage",
                        #                "Parasitemia density (/µl)" = "pdensity"
                        #                )),
                        # 
                        
                        selectInput("ptype", 
                                    label = "Which data do you have?",
                                    choices = c("Perentage of parasetemia", "Parasitemia density (/µl)"),
                                    selected = "Percentage of parasetemia"),
                        
                        # br() element to introduce extra vertical spacing ----
                        br(),
                     
                        # parasetemia input
                        sliderInput(inputId = "percentage-parasitemia",
                                label = "Percentage of parasitemia",
                                value = 8, min = 0, max = 100, step =1.),
                        # OR
                        sliderInput(inputId = "parasetemia-density",
                                label = "Parasitemia density (/ql)",
                                value = 800000, min = 0, max = 1500000, step = 1000), 
                        
                        actionButton("go-simple", "Plot")
                  
                        # sliderInput(inputId = "white-blood",
                        #       label = "Total number of white blood cells (x 10^9/ L)",
                        #       value = 9, min = 0, max = 20, step = .25)
              
                  #sliderInput(inputId = "red-blood",
                  #            label = "Total number of red blood cells (x 10^12/ L)",
                  #            value = 4, min = 0, max = 10, step = .25)
                  
                      ), # close tabpanel
                  
                      tabPanel("Complex model",
                               
                        helpText("Choose the parameters. You can choose between parasetemia percentage and parasitemia density and between total number of white blood cells and lymphoctye and monocyte percentage."),
                               
                        # parasetemia input
                        sliderInput(inputId = "percentage-parasitemia",
                                       label = "Percentage of parasitemia",
                                       value = 8, min = 0, max = 100, step =1.),
                        # OR
                        sliderInput(inputId = "parasetemia-density",
                                       label = "Parasitemia density (/ql)",
                                       value = 800000, min = 0, max = 1500000, step = 1000),  
                           
                        sliderInput(inputId = "white-blood",
                                       label = "Total number of white blood cells (x 10^9/ L)",
                                       value = 9, min = 0, max = 20, step = .25),
                        # OR
                        sliderInput(inputId = "lymphocyte-percentage",
                                    label = "Percentage of lymphoctyes (in white blood cells)",
                                    value = 30, min = 0, max = 100, step = .5),
                        sliderInput(inputId = "monocyte-percentage",
                                    label = "Percentage of monocytes (in white blood cells)",
                                    value = 10, min = 0, max = 100, step = .5),
                  
                        actionButton("go-complex", "Plot")
                ) # close tabpanel
              ) # close tabsetpanel
              
              # mainPanel(
              #   
              #   box(width = 6,
              #       title = "Regression",
              #       solidHeader = TRUE, status = "primary",
              #       plotOutput(outputId = "covariance"))
              #   
              # )
            ), # close sidebarpanel
            
            # Main panel for displaying outputs ----
            mainPanel(
              # Output: Tabset w/ plot, summary, and table ----
              tabsetPanel(type = "tabs",
                          tabPanel("Plot", plotOutput(outputId = "distPlot")),
                          tabPanel("Summary", verbatimTextOutput("summary")),
                          tabPanel("Table", tableOutput("table"))
              ) # close tabsetpanel           
            ) # close mainpanel
            
        ) # close box
        
      ) # close fluidrow
    ), # close tabitem
      
              
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
  ) # close tabitems
) # close body




ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {

  output$distPlot <- renderPlot({
    
      # fit.nona.total <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$total.number.of.cells, data=dat.nona)
      # summary(fit.nona.total) # show results: R^2: 0.4622
      # summary(fit.nona.total)$sigma^2 # estimated variance of residuals around a fitted line: 0.02268394
      # 
      # # plot the statistics, OUTLIERS 35, 39 -- both in UM group? -- kept them -- BUT MIGHT BE WORTH TRYING WITHOUT THEM
      # par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
    plot(fit.nona.total)  # Plot the model information/ 
      

    })
  
  # #create a data frame called responses
  # saveData <- function(data) {
  #   data <- as.data.frame(t(data))
  #   if (exists("responses")) {
  #     responses <<- rbind(responses, data)
  #   } else {
  #     responses <<- data
  #   }
  # }
  # 
  # loadData <- function() {
  #   if (exists("responses")) {
  #     responses
  #   }
  # }
  # 
  # 
  # # Whenever a field is filled, aggregate all form data
  # #formData is a reactive function
  # 
  # formData <- reactive({
  #   data <- sapply(fields, function(x) input[[x]])
  #   data
  # })
  # 
  # # When the Save button is clicked, save the form data
  # observeEvent(input$save, {
  #   saveData(formData())
  # })
  # 
  # # Show the previous responses
  # # (update with current response when save is clicked)
  # output$responses <- DT::renderDataTable({
  #   input$save
  #   loadData()
  # })  
  
}
    


shinyApp(ui = ui, server = server)

