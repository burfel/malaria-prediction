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

#if (interactive()) {

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
                    helpText("Choose between the simple and a more complex model."),
                  
                    
                    tabsetPanel(id="tabset",
                          
                      tabPanel("Simple model",
                        
                        helpText("Choose the parameters: You can choose between parasetemia percentage and parasitemia density."),
                        
                        # Input: Select the parasitemia type ----
                        # radioButtons("ptype", "Which data do you have?",
                        #              c("Percentage of parasitemia" = "ppercentage",
                        #                "Parasitemia density (/µl)" = "pdensity"
                        #                )),

                        selectInput("ptype",
                                    label = "Which data do you have?",
                                    choices = c("Percentage of parasitemia" = "ppercentage", "Parasitemia density (/µl)" = "pdensity"),
                                    #selected = "Percentage of parasitemia"
                                    ),
                       
                        # checkboxInput("ptype1", "Percentage of parasetemia"),
                        # conditionalPanel(
                        #   condition = "input.ptype1 == true",
                        #   selectInput("smoothMethod", "Method",
                        #               list("lm", "glm", "gam", "loess", "rlm"))
                        # ),
                        
                        #br(),
                        #checkboxInput("ptype", "Percentage of parasetemia"),
                        conditionalPanel(
                          condition = "input.ptype == 'ppercentage'",
                          sliderInput(inputId = "percentage-parasitemia",
                                      label = "Percentage of parasitemia",
                                      value = 8, min = 0, max = 100, step =1.)

                        ),
                       
                        conditionalPanel(
                          condition = "input.ptype == 'pdensity'",
                          sliderInput(inputId = "parasetemia-density",
                                    label = "Parasitemia density (/ql)",
                                    value = 800000, min = 0, max = 1500000, step = 1000)
                        ),
                        
                        # #########
                        # selectInput("dataset", "Dataset", c("diamonds", "rock", "pressure", "cars")),
                        # conditionalPanel( condition = "output.nrows",
                        #                   checkboxInput("headonly", "Only use first 1000 rows")),
                        # ########
                        
                        # br() element to introduce extra vertical spacing ----
                        # br(),
                     
                        # parasetemia input
                        # sliderInput(inputId = "percentage-parasitemia",
                        #         label = "Percentage of parasitemia",
                        #         value = 8, min = 0, max = 100, step =1.),
                        # OR
                        # sliderInput(inputId = "parasetemia-density",
                        #         label = "Parasitemia density (/ql)",
                        #         value = 800000, min = 0, max = 1500000, step = 1000), 
                        
                        actionButton("go-simple", "Compute"),
                        
                        br(),
                        renderText({ "text_calc"})
                      
                        #textOutput("text_calc") # prevents plot from being plotted for some reason
                  
                        # sliderInput(inputId = "white-blood",
                        #       label = "Total number of white blood cells (x 10^9/ L)",
                        #       value = 9, min = 0, max = 20, step = .25)
              
                  #sliderInput(inputId = "red-blood",
                  #            label = "Total number of red blood cells (x 10^12/ L)",
                  #            value = 4, min = 0, max = 10, step = .25)
                  
                      ), # close tabpanel
                  
                      tabPanel("Complex model",
                               
                        helpText("Choose the parameters: You can choose between parasetemia percentage and parasitemia density and between total number of white blood cells and lymphoctye and monocyte percentage."),
                        
                        # ###
                        # selectInput("dataset", "Dataset", c("diamonds", "rock", "pressure", "cars")),
                        # conditionalPanel( condition = "output.nrows",
                        #                   checkboxInput("headonly", "Only use first 1000 rows")),
                        # ####      
                        # parasetemia input
                        # sliderInput(inputId = "percentage-parasitemia",
                        #                label = "Percentage of parasitemia",
                        #                value = 8, min = 0, max = 100, step =1.),
                        # # OR
                        # sliderInput(inputId = "parasetemia-density",
                        #                label = "Parasitemia density (/ql)",
                        #                value = 800000, min = 0, max = 1500000, step = 1000),  
                        
                        # selectInput("ptype",
                        #             label = "Which data do you have?",
                        #             choices = c("Percentage of parasetemia", "Parasitemia density (/µl)"),
                        #             selected = "Percentage of parasetemia"),
                        
                        radioButtons("ptype", "Which data do you have?",
                                     c("Percentage of parasitemia" = "ppercentage",
                                       "Parasitemia density (/µl)" = "pdensity"
                                     )),
                        
                        
                        br(),
                        #checkboxInput("ptype1", "Percentage of parasetemia"),
                        conditionalPanel(
                          condition = "input.ptype == 'ppercentage'",
                          sliderInput(inputId = "percentage-parasitemia",
                                      label = "Percentage of parasitemia",
                                      value = 8, min = 0, max = 100, step =1.)
                          
                        ),
                        
                        conditionalPanel(
                          condition = "input.ptype == 'pdensity'",
                          sliderInput(inputId = "parasetemia-density",
                                      label = "Parasitemia density (/ql)",
                                      value = 800000, min = 0, max = 1500000, step = 1000)
                        ),
                           
                        selectInput("wtype",
                                    label = "Which data do you have?",
                                    choices = c("Total number of white blood cells (* 10^9/ L)", "Percentage of lymphoctyes and monocytes"),
                                    selected = "Total number of white blood cells (* 10^9/ L)"),
                        
                        conditionalPanel(
                          condition = "input.wtype == 'white-blood'",
                          sliderInput(inputId = "white-blood",
                                      label = "Total number of white blood cells (* 10^9/ L)",
                                      value = 9, min = 0, max = 20, step = .25)
                        ),
                        # OR
                        conditionalPanel(
                          condition = "input.wtype == lympho",
                          sliderInput(inputId = "lymphocyte-percentage",
                                    label = "Percentage of lymphoctyes (in white blood cells)",
                                    value = 30, min = 0, max = 100, step = .5),
                          sliderInput(inputId = "monocyte-percentage",
                                    label = "Percentage of monocytes (in white blood cells)",
                                    value = 10, min = 0, max = 100, step = .5)
                        ),
                  
                        actionButton("go-complex", "Compute"),
                        br(),
                        textOutput("text_calc")
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
                          tabPanel("Plot", 
                                   plotOutput(outputId = "distPlot")),
                          tabPanel("Summary", 
                                   verbatimTextOutput("summary")),
                          tabPanel("Table", 
                                   textOutput("table"))
           
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
            #source('abstract-app.R', local = TRUE)
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
            
            # ##########
            # 
            # titlePanel("Dynamically generated user interface components"),
            # fluidRow(
            #   
            #   column(3, wellPanel(
            #     selectInput("input_type", "Input type",
            #                 c("slider", "text", "numeric", "checkbox",
            #                   "checkboxGroup", "radioButtons", "selectInput",
            #                   "selectInput (multi)", "date", "daterange"
            #                 )
            #     )
            #   )),
            #   
            #   column(3, wellPanel(
            #     # This outputs the dynamic UI component
            #     uiOutput("ui")
            #   )),
            #   
            #   column(3,
            #          tags$p("Input type:"),
            #          verbatimTextOutput("input_type_text"),
            #          tags$p("Dynamic input value:"),
            #          verbatimTextOutput("dynamic_value")
            #   )
            # )
            # 
            # #########
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
  
  # output$plot <- renderPlot({
  #   if (input$plotType == "scatter") {
  #     plot(x, y)
  #   } else {
  #     breaks <- input$breaks
  #     if (breaks == "custom") {
  #       breaks <- input$breakCount
  #     }
  #     
  #     hist(x, breaks = breaks)
  #   }
  # })
  
  # MODEL O
  formula0 <- reactive({
    I <- 0.151312
    P <- input$percentage-parasitemia
    #W <- input$white-blood
    I + 0.010838 * P
  })
  
  output$text_calc <- reactiveUI(renderText({
    paste("Prediction:")
    paste("Percentage of reads that will map to pathogen: ", formula0())
    paste("Percentage of reads that will map to host: ", 1 - formula0())

  })
  )
  
  # Generate a summary of the dataset ----
  output$summary <- reactiveUI(renderPrint({
    # dataset <- datasetInput()
    # summary(dataset)
    summary(fit.paras)
  })
  )
  
  ######
  
  
  # output$ui <- renderUI({
  #   if (is.null(input$input_type))
  #     return()
  #   
  #   # Depending on input$input_type, we'll generate a different
  #   # UI component and send it to the client.
  #   switch(input$input_type,
  #          "slider" = sliderInput("dynamic", "Dynamic",
  #                                 min = 1, max = 20, value = 10),
  #          "text" = textInput("dynamic", "Dynamic",
  #                             value = "starting value"),
  #          "numeric" =  numericInput("dynamic", "Dynamic",
  #                                    value = 12),
  #          "checkbox" = checkboxInput("dynamic", "Dynamic",
  #                                     value = TRUE),
  #          "checkboxGroup" = checkboxGroupInput("dynamic", "Dynamic",
  #                                               choices = c("Option 1" = "option1",
  #                                                           "Option 2" = "option2"),
  #                                               selected = "option2"
  #          ),
  #          "radioButtons" = radioButtons("dynamic", "Dynamic",
  #                                        choices = c("Option 1" = "option1",
  #                                                    "Option 2" = "option2"),
  #                                        selected = "option2"
  #          ),
  #          "selectInput" = selectInput("dynamic", "Dynamic",
  #                                      choices = c("Option 1" = "option1",
  #                                                  "Option 2" = "option2"),
  #                                      selected = "option2"
  #          ),
  #          "selectInput (multi)" = selectInput("dynamic", "Dynamic",
  #                                              choices = c("Option 1" = "option1",
  #                                                          "Option 2" = "option2"),
  #                                              selected = c("option1", "option2"),
  #                                              multiple = TRUE
  #          ),
  #          "date" = dateInput("dynamic", "Dynamic"),
  #          "daterange" = dateRangeInput("dynamic", "Dynamic")
  #   )
  # })
  # 
  # output$input_type_text <- renderText({
  #   input$input_type
  # })
  # 
  # output$dynamic_value <- renderPrint({
  #   str(input$dynamic)
  # })
  # 
  # 
  #####
  
  # output$table <- renderTable({
  #   # head(datasetInput(), n = input$obs)
  # })
  
  
  # datasetInput <- reactive({
  #   switch(input$dataset,
  #          "rock" = rock,
  #          "pressure" = pressure,
  #          "cars" = cars)
  # })
  # 
  # output$nrows <- reactive({
  #   nrow(datasetInput())
  # })
  # 
  # outputOptions(output, "nrows", suspendWhenHidden = FALSE) 
  
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


#}

