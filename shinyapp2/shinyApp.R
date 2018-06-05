library(shiny)
library(ggplot2)
library(shinydashboard)
library(grid)
library(markdown)
library(ggExtra)
library(shinyBS)
library(qtl)
library(rmarkdown)

###########################################################################
source('../src/databuilder.R', local = TRUE)
#source('../src/databuilder2.R', local = TRUE)

setwd("~/Documents/IMPERIAL/PROJECTS/project2/GITHUB/shinyapp2")

#if (interactive()) {

### Title:
header <- dashboardHeader(title = "Read map prediction", titleWidth = 250,

  dropdownMenu(type = "messages",
             # messageItem(
             #   from = "Sales Dept",
             #   message = "Sales are steady this month."
             # ),
             messageItem(
               from = "New User",
               message = "How do I use the tool?",
               icon = icon("question")
               #time = "13:45"
             ),
             messageItem(
               from = "Support",
               message = "Get in touch.",
               icon = icon("life-ring"),
               time = "2018-06-12"
             )
)
) # end header

### SideBar:
sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search..."),
    #menuItem("Rmd", tabName = "Rmd", icon = icon("fa fa-circle"), badgeLabel = "for user", badgeColor = "green"),
    menuItem("Lay-summary", tabName = "lay-summary", icon = icon("fa fa-circle"), badgeLabel = "for user", badgeColor = "green"),
    menuItem("Abstract", tabName = "abstract", icon = icon("fa fa-circle")),
    menuItem("Introduction", tabName = "introduction", icon = icon("fa fa-circle")),
    menuItem("Methods", tabName = "methods", icon = icon("fa fa-circle")),
    menuItem("Results", tabName = "results", icon = icon("fa fa-chart-bar"), badgeLabel = "for user", badgeColor = "green"),
    menuItem("Discussion", tabName = "discussion", icon = icon("fa fa-circle")),
    menuItem("Conclusion", tabName = "conclusion", icon = icon("fa fa-circle")),
    menuItem("Raw Data", tabName = "data", icon = icon("fa fa-circle")),
    menuItem("References", tabName = "references", icon = icon("fa fa-circle")),
    menuItem("Glossary", tabName = "glossary", icon = icon("fa fa-circle")),
    menuItem("About", tabName = "about", icon = icon("file-code-o")#, href = "https://github.com/burfel/malaria-prediction"
             )
  )
)


### Dashboard:
body <- dashboardBody(
  ## From ui.R: Adds a tooltip to element with inputId = "someInput" 
  ## with text, "This is an input.", that appears to the left on hover.
  bsTooltip(id = "go-simple", title = "Make a prediction!", 
            placement = "left", trigger = "hover"),
  bsTooltip(id = "go-complex", title = "Make a prediction!", 
            placement = "left", trigger = "hover"),
  
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
                          
                      # SIMPLE MODEL          
                      tabPanel("Simple model",
                        
                        helpText("Choose the parameters: You can choose between parasitemia percentage and parasitemia density."),
                        
                        # Input: Select the parasitemia type ----
                        radioButtons("ptype", "Which type of data do you have?",
                                     c("Percentage of parasitemia" = "ppercentage",
                                       "Parasitemia density (/µl)" = "pdensity"
                                       )
                                     ),

                        # selectInput("ptype",
                        #             label = "Which type of data do you have?",
                        #             choices = c("Percentage of parasitemia" = "ppercentage", "Parasitemia density (/µl)" = "pdensity")
                        #             #selected = "Percentage of parasitemia"
                        #             ),
                       
                        
                        #br(),
                        #checkboxInput("ptype", "Percentage of parasitemia"),
                        conditionalPanel(
                          condition = "input.ptype == 'ppercentage'",
                          sliderInput(inputId = "parasitemia_percentage",
                                      label = "Percentage of parasitemia",
                                      value = 8, min = 1, max = 99, step =1.)

                        ),
                       
                        conditionalPanel(
                          condition = "input.ptype == 'pdensity'",
                          sliderInput(inputId = "parasitemia_density",
                                    label = "Parasitemia density (/ql)",
                                    value = 800000, min = 0, max = 1500000, step = 1000)
                        ),
                        
                        
                        # br() element to introduce extra vertical spacing ----
                     
                        # parasitemia input
                        # sliderInput(inputId = "parasitemia_percentage",
                        #         label = "Percentage of parasitemia",
                        #         value = 8, min = 0, max = 100, step =1.),
                        # OR
                        # sliderInput(inputId = "parasitemia-density",
                        #         label = "Parasitemia density (/ql)",
                        #         value = 800000, min = 0, max = 1500000, step = 1000), 
                        
                        actionButton("go_simple", "Compute")
                        
                        #br(),
                        #renderText({ "comp_paras"})
                      
                        #textOutput("text_calc") # prevents plot from being plotted for some reason
                  
                  
                      ), # close tabpanel
                  
                      # COMPLEX MODEL
                      tabPanel("Complex model",
                               
                        helpText("Choose the parameters: You can choose between parasitemia percentage and parasitemia density and between total number of white blood cells and lymphoctye, monocyte or neutrophil percentage."),
                        
                        # ###
                        # selectInput("dataset", "Dataset", c("diamonds", "rock", "pressure", "cars")),
                        # conditionalPanel( condition = "output.nrows",
                        #                   checkboxInput("headonly", "Only use first 1000 rows")),
                        # ####      
                        # parasetemia input
                        # sliderInput(inputId = "parasitemia_percentage",
                        #                label = "Percentage of parasitemia",
                        #                value = 8, min = 0, max = 100, step =1.),
                        # # OR
                        # sliderInput(inputId = "parasetemia_density",
                        #                label = "Parasitemia density (/ql)",
                        #                value = 800000, min = 0, max = 1500000, step = 1000),  
                        
                        # selectInput("ptype",
                        #             label = "Which data do you have?",
                        #             choices = c("Percentage of parasetemia", "Parasitemia density (/µl)"),
                        #             selected = "Percentage of parasetemia"),
                        
                        # Input: Select the parasitemia type ----
                        radioButtons("ptype2", "Which type of data do you have?",
                                     c("Percentage of parasitemia" = "ppercentage2",
                                       "Parasitemia density (/µl)" = "pdensity2"
                                     )),
      
                        conditionalPanel(
                          condition = "input.ptype2 == 'ppercentage2'",
                          sliderInput(inputId = "parasitemia_percentage2",
                                      label = "Percentage of parasitemia",
                                      value = 8, min = 1, max = 99, step =1.)
                          
                        ),
                        
                        conditionalPanel(
                          condition = "input.ptype2 == 'pdensity2'",
                          sliderInput(inputId = "parasitemia_density2",
                                      label = "Parasitemia density (/ql)",
                                      value = 800000, min = 0, max = 1500000, step = 1000)
                        ),   
                        
                           
                        # selectInput("wtype",
                        #             label = "Which data do you have?",
                        #             choices = c("Total number of white blood cells (* 10^9/ L)", "Percentage of lymphoctyes and monocytes"),
                        #             selected = "Total number of white blood cells (* 10^9/ L)"),
                        
                        radioButtons("wtype", "Which data do you have?",
                                     c("Total number of white blood cells (* 10^9/ L)" = "white_blood",
                                       "Percentage of lymphocytes" = "lympho",
                                       "Percentage of monocytes" = "mono",
                                       "Percentage of neutrophils" = "neutro"
                                     )
                        ),
                        
                        conditionalPanel(
                          condition = "input.wtype == 'white_blood'",
                          sliderInput(inputId = "white_blood",
                                      label = "Total number of white blood cells (* 10^9/ L)",
                                      value = 9, min = 0, max = 20, step = .25)
                        ),
                        # OR
                        conditionalPanel(
                          condition = "input.wtype == 'lympho'",
                          sliderInput(inputId = "lymphocyte_percentage",
                                    label = "Percentage of lymphoctyes (in white blood cells)",
                                    value = 30, min = 0, max = 100, step = .5)
                        ),
                        
                        conditionalPanel(
                          condition = "input.wtype == 'mono'",
                          sliderInput(inputId = "monocyte_percentage",
                                      label = "Percentage of monocytes (in white blood cells)",
                                      value = 10, min = 0, max = 100, step = .5)
                        ),
                        
                        conditionalPanel(
                          condition = "input.wtype == 'neutro'",
                          sliderInput(inputId = "neutrophil_percentage",
                                      label = "Percentage of neutrophils (in white blood cells)",
                                      value = 60, min = 0, max = 100, step = .5)
                        ),
                  
                        actionButton("go_complex", "Compute"),
                        br()
                        #textOutput("text_calc")
                ) # close tabpanel
                
              ), # close tabsetpanel
              
              br(),
              bookmarkButton()
              
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
                          # conditionalPanel(
                          #   condition = "input.ptype2 == 'ppercentage'",
                          #   tabPanel("Output",
                          #            textOutput("comp_simple")
                          #           )
                          #   ),
                          #   conditionalPanel(
                          #     condition = "input.ptype2 == 'pdensity'",
                          #     tabPanel("Output",
                          #              textOutput("comp_simple_dens")
                          #             )
                          #     
                          #   ),
                          tabPanel("Output",
                                   # verbatimTextOutput("comp_simple"),
                                   conditionalPanel(
                                      condition = "input.ptype2 == 'ppercentage'",
                                      textOutput("comp_simple")
                                   ),
                                   conditionalPanel(
                                      condition = "input.ptype2 == 'pdensity'",
                                      textOutput("comp_simple_dens")
                                   )
                          ),
                          tabPanel("Summary", 
                                   textOutput("summary_total")
                                   ),
                          tabPanel("Plot", 
                                   plotOutput(outputId = "plot_total")
                                   )
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
    tabItem(tabName = "Abstract",
            fluidPage(
              box(width = 12,status = "success",
                  includeMarkdown("md/abstract.Rmd")
                  )
              # actionButton(inputId='read-more1', label="Learn More",
              #              icon = icon("th"),
              #              onclick ="window.open('http://google.com', '_blank')")

              # uiOutput("doc_to_display")
            )
            #source('abstract-app.R', local = TRUE)
    ), # close tabitem

    # TAB
    tabItem(tabName = "Introduction",
            fluidPage(
              box(width = 12,status = "success",
                  shiny::includeMarkdown("md/introduction.Rmd"))
            )
    )
    # 
    # # TAB 
    # tabItem(tabName = "Methods",
    #         fluidPage(
    #           box(width = 12,status = "success",
    #               shiny::includeMarkdown("md/methods.Rmd"))
    #         )
    # ),
    # 
    # # TAB 
    # tabItem(tabName = "Discussion",
    #         fluidPage(
    #           box(width = 12,status = "success",
    #               shiny::includeMarkdown("md/discussion.Rmd"))
    #         )
    #         
    #         # ##########
    #         # 
    #         # titlePanel("Dynamically generated user interface components"),
    #         # fluidRow(
    #         #   
    #         #   column(3, wellPanel(
    #         #     selectInput("input_type", "Input type",
    #         #                 c("slider", "text", "numeric", "checkbox",
    #         #                   "checkboxGroup", "radioButtons", "selectInput",
    #         #                   "selectInput (multi)", "date", "daterange"
    #         #                 )
    #         #     )
    #         #   )),
    #         #   
    #         #   column(3, wellPanel(
    #         #     # This outputs the dynamic UI component
    #         #     uiOutput("ui")
    #         #   )),
    #         #   
    #         #   column(3,
    #         #          tags$p("Input type:"),
    #         #          verbatimTextOutput("input_type_text"),
    #         #          tags$p("Dynamic input value:"),
    #         #          verbatimTextOutput("dynamic_value")
    #         #   )
    #         # )
    #         # 
    #         # #########
    # ),
    # 
    # # TAB
    # tabItem(tabName = "Conclusion",
    #         fluidPage(
    #           box(width = 12,status = "success",
    #               shiny::includeMarkdown("md/conclusion.Rmd"))
    #         )
    # ),
    # 
    # # TAB 
    # tabItem(tabName = "Data preparation",
    #         fluidPage(
    #           box(width = 12,status = "success",
    #               shiny::includeMarkdown("md/data.Rmd"))
    #         )
    # ),
    # 
    # # TAB 
    # tabItem(tabName = "References",
    #         fluidPage(
    #           box(width = 12,status = "success",
    #               shiny::includeMarkdown("md/references.Rmd"))
    #         )
    # ),
    # 
    # # TAB 
    # tabItem(tabName = "Glossary",
    #         fluidPage(
    #           box(width = 12,status = "success",
    #               shiny::includeMarkdown("md/glossary.Rmd"))
    #         )
    # ),
    # 
    # # TAB 3 = About
    # tabItem(tabName = "About",
    #         fluidPage(
    #           box(width = 12,status = "success",
    #               shiny::includeMarkdown("README.Rmd"))
    #         )
    # ) 
  ) # close tabitems
) # close body




ui <- dashboardPage(header, sidebar, body)


#########################################################################################################################

server <- function(input, output, session) {

## MODELS
# MODEL 0a: glm paras percentage
glm_simple <- reactive({
  I <- -1.964e+00
  P <- input$parasitemia_percentage
    #W <- input$white_blood
  logit <- I + 6.550e-02*P
  exp(logit)/(1+exp(logit))
})


# MODEL 0b: glm paras density
glm_simple_dens <- reactive({
  I <- 2.012e+00
  P <- input$parasitemia_density
  logit <- I + 2.031e-06*P
  exp(logit)/(1+exp(logit))
})



# MODEL 0a1a: glm total percentage white blood cell
glm_total <- function(){
  I <- -1.112e+00
  P <- input$parasitemia_percentage2
  W <- input$white_blood
  I + 5.324e-02*P + (-7.415e-02)*W
  exp(logit)/(1+exp(logit))
}


# MODEL 0b1a: glm total density white blood cell
glm_total_dens <- function(){
  I <- -1.238e+00
  P <- input$parasitemia_percentage2
  W <- input$white_blood
  I + 1.676e-06*P + (-6.638e-02)*W
  exp(logit)/(1+exp(logit))
}


# MODEL 0b1b: glm total percentage lympho
# MODEL.log: -1.772e+00 + 7.545e-02*dat.nona$Percentage.parasitemia + (-1.056e-02)*dat.nona$Percentage.lymphocytes

# MODEL 0a1b: glm total density lympho
# MODEL.log: -1.939e+00 + 2.125e-06*dat.nona$Parasite.density...µl. + (-3.870e-03)*dat.nona$Percentage.lymphocytes


# MODEL 0a1c: glm total percentage mono
# MODEL.logL: -2.025e+00 + 6.461e-02*dat.nona$Percentage.parasitemia + 1.143e-02*dat.nona$Percentage.monocytes

# MODEL 0b1c: glm total density mono
# MODEL.logL: -2.226e+00 + 1.951e-06*dat.nona$Parasite.density...µl. + 3.953e-02*dat.nona$Percentage.monocytes


# MODEL 0a1d: glm total percentage neutro
# MODEL.log: -2.581e+00 + 7.341e-02*dat.nona$Percentage.parasitemia + 8.204e-03*dat.nona$Percentage.neutrophils

# MODEL 0b1d: glm total density neutro
# MODEL.log: -2.238e+00 + 2.106e-06*dat.nona$Parasite.density...µl. + 3.036e-03*dat.nona$Percentage.neutrophils


# UPON CLICKING 'SIMPLE MODEL'... 
observeEvent(input$go_simple, {

  # COMPUTE PREDICTION SIMPLE MODEL ----
  output$comp_paras <- renderText({
    "Prediction:"
  })
  
  # PLOT PARAS MODEL SUMMARY ----
  output$plot_paras <- renderPlot({
    plot(fit.nona.paras)
  })
  
  # SUMMARY OF THE SIMPLE MODEL ----
  output$summary_paras <- renderUI({
    # dataset <- datasetInput()
    # summary(dataset)
    summary(fit.nona.paras)
  })
  
  
  }
)
  

# UPON CLICKING 'COMPLEX MODELL'...
#observeEvent(input$go-complex, {
  
  # COMPUTE PREDICTION COMPLEX MODEL ----
  output$comp_simple <- renderText({
    paste("Prediction:", '<br/>', "Percentage of reads that will map to pathogen: ", glm_simple(), '<br/>', "Percentage of reads that will map to host: ", 1 - glm_simple())
  })

  output$comp_simple_dens <- renderText({
    paste("Prediction:", '<br/>', "Percentage of reads that will map to pathogen: ", glm_simple_dens(), '<br/>', "Percentage of reads that will map to host: ", 1 - glm_simple_dens())
  })

  # output$comp_total <- renderUI({
  #   str1 <- paste("You have selected", input$parasitemia_percentage, " as percentage of parasitemia.")
  #   str2 <- paste("Prediction:")
  #   str3 <- paste("Percentage of reads that will map to pathogen: ", lm_simple_perc())
  #   str4 <- paste("Percentage of reads that will map to host: ", 1 - lm_simple_perc())
  #   HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
  # })
  
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

  
########################################
  # output$dynamic_value <- renderPrint({
  #   str(input$dynamic)
  # }) 
  
  # output$view <- renderTable({
  #   head(datasetInput(), n = input$obs)
  # })
  
  ###
  
  # ## From server.R: Add the same tooltip as above
  # addTooltip(session, id = "go-simple", title = "Click here to compute a prediction!",
  #            placement = "left", trigger = "hover")
  
  ###
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
  
  
  # ####
  # output$doc_to_display <- renderUI({
  #   includeMarkdown("md/abstract.md")
  # })
  # ###
  
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
  
####################################
}
    
enableBookmarking(store = "url")

shinyApp(ui = ui, server = server)


#}

