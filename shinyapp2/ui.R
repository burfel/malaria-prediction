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
    menuItem("Data", tabName = "data", icon = icon("fa fa-circle")),
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
                                         
                                         radioButtons("wtype", "Which type of data do you have?",
                                                      c("Total number of white blood cells (* 10^9/ L)" = "white_blood",
                                                        "Counts of different white blood cell types (lymphocytes, monocytes, neutrophils)" = "counts"
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
                                           condition = "input.wtype == 'counts'",
                                           sliderInput(inputId = "lympho",
                                                       label = "Total number of lymphoctyes (* 10^9/ L)",
                                                       value = 2.7, min = 0, max = 20, step = .1),
                                           sliderInput(inputId = "mono",
                                                       label = "Total number of of monocytes (* 10^9/ L)",
                                                       value = 0.9, min = 0, max = 100, step = .1),
                                           sliderInput(inputId = "neutro",
                                                       label = "Total number of neutrophils (* 10^9/ L)",
                                                       value = 5.4, min = 0, max = 100, step = .1)
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
                                         #verbatimTextOutput("comp_simple"),
                                         textOutput("comp_simple_dens")
                                         # conditionalPanel(
                                         #    condition = "input.ptype == 'ppercentage'",
                                         #    textOutput("comp_simple")
                                         # ),
                                         # conditionalPanel(
                                         #    condition = "input.ptype == 'pdensity'",
                                         #    textOutput("comp_simple_dens")
                                         # )
                                         #verbatimTextOutput("comp_total")
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
                  includeMarkdown("md/0_abstract.Rmd")
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
                  shiny::includeMarkdown("md/1_introduction.Rmd"))
            )
    )
    # 
    # # TAB 
    # tabItem(tabName = "Methods",
    #         fluidPage(
    #           box(width = 12,status = "success",
    #               shiny::includeMarkdown("md/2_methods.Rmd"))
    #         )
    # ),
    # 
    # # TAB 
    # tabItem(tabName = "Discussion",
    #         fluidPage(
    #           box(width = 12,status = "success",
    #               shiny::includeMarkdown("md/3_discussion.Rmd"))
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
    #               shiny::includeMarkdown("md/5_conclusion.Rmd"))
    #         )
    # ),
    # 
    # # TAB 
    # tabItem(tabName = "Data preparation",
    #         fluidPage(
    #           box(width = 12,status = "success",
    #               shiny::includeMarkdown("md/6_data.Rmd"))
    #         )
    # ),
    # 
    # # TAB 
    # tabItem(tabName = "References",
    #         fluidPage(
    #           box(width = 12,status = "success",
    #               shiny::includeMarkdown("md/7_references.Rmd"))
    #         )
    # ),
    # 
    # # TAB 
    # tabItem(tabName = "Glossary",
    #         fluidPage(
    #           box(width = 12,status = "success",
    #               shiny::includeMarkdown("md/8_glossary.Rmd"))
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

