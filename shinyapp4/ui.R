# read map prediction
# Predicts the percentage of reads that map to host and pathogen
# Felicia Burtscher
# Date: June 6, 2018

# ui.R

#source('../src/databuilder.R', local = TRUE)
# setwd("shinyapp4")

#ui = bootstrapPage(
ui = tagList(
    #tags$style(type='text/css', ".selectize-input { font-size: 64px; line-height: 64px;} .selectize-dropdown { font-size: 28px; line-height: 28px; }"),
    tags$head(tags$style("#text1{color: blue;
                         font-size: 120px;
                         font-style: italic;
                         }")
              ),
    shinythemes::themeSelector(),
    navbarPage(
      #theme = "spacelab",  # <--- To use a theme, uncomment this
      "Read map prediction",
      
      #===============================================================================
      #                               TAB PANELS                                     #
      #===============================================================================
      tabPanel("Lay-summary", 
               uiOutput("lay_summary")
               ),
      tabPanel("Abstract", 
               uiOutput("abstract")
      ),
      tabPanel("Introduction", 
               uiOutput("introduction")
      ),
      navbarMenu("Methods", 
                  uiOutput("methods"),
        # tabPanel("Table"
        # #DT::dataTableOutput("table")
        #         ),
        tabPanel("OVERVIEW",
                 includeMarkdown("md/2_methods.Rmd")
        ),
        tabPanel("Data pre-processing",
                 includeMarkdown("md/2a_data_preprocessing.Rmd")
                  ),
          # fluidRow(
          # column(6,
          #   includeMarkdown("README.md"),
          #   column(6,
          #          img(class="img-polaroid",
          #              src=paste0("http://upload.wikimedia.org/",
          #                         "wikipedia/commons/9/92/",
          #                         "1919_Ford_Model_T_Highboy_Coupe.jpg")),
          #          tags$small(
          #            "Source: Photographed at the Bay State Antique ",
          #            "Automobile Club's July 10, 2005 show at the ",
          #            "Endicott Estate in Dedham, MA by ",
          #            a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
          #              "User:Sfoskett")
          #           )
          #         )  # end column
          #       ) # end column
          #   ) # end fluidrow
          # ) end tabPanel
        tabPanel("Variable selection",
                 includeMarkdown("md/2b_variable_selection.Rmd")
        ),
        tabPanel("Model selection",
                 includeMarkdown("md/2c_model_selection.Rmd")
        ),
        tabPanel("Model assumptions",
                 includeMarkdown("md/2d_model_assumptions.Rmd")
        ),
        tabPanel("Generalized Linear Model",
                 includeMarkdown("md/2e_glm.Rmd")
        ),
        tabPanel("Cross-validation",
                 includeMarkdown("md/2f_cross_validation.Rmd")
        )
      ), # end navbarMenu
        
      
      #===============================================================================
      #                               TAB PANEL: RESULTS                             #
      #===============================================================================
      tabPanel("Results",
               
         sidebarPanel(width=4,
                   
         #fileInput("file", "File input:"),
        
        # icon("question circle", "fa-3x"),
        useShinyalert(),
        actionButton("help_icon", "Info"), 
        br(), br(),
        icon("calendar", "fa-2x"),
        dateInput('date',
                  label = "Date input: yyyy-mm-dd",
                  value = Sys.Date()
        ),
        icon("user", "fa-2x"),
        textInput("user_name", 
                  "User name:", 
                  "Enter your name here!"),
                 
        # helpText("Choose between the simple and a more complex model."),
                 
        tabsetPanel(id = "tabset",

        #===============================================================================
        #                               SIMPLE MODEL                                   #
        #===============================================================================
            tabPanel("Simple model", value = "simple",
                 
                 br(),
                 # Input: Select the parasitemia type ----
                 radioButtons("ptype", "Which type of parasitemia data do you have?",
                              c("Percentage of parasitemia" = "ppercentage",
                                "Parasitemia density [1/µl]" = "pdensity"
                              )
                 ),
                 conditionalPanel(
                   condition = "input.ptype == 'ppercentage'",
                   sliderInput(inputId = "parasitemia_percentage",
                               label = "Percentage of parasitemia",
                               value = 8, min = 1, max = 99, step =1.)
                 ),
                 conditionalPanel(
                   condition = "input.ptype == 'pdensity'",
                   sliderInput(inputId = "parasitemia_density",
                               label = "Parasitemia density [1/µl]",
                               value = 800000, min = 0, max = 1500000, step = 1000)
                 ),
                 bookmarkButton(id = "bookmark1")
                 
                 # actionButton(inputId = "go_simple", label = "Submit"),
                 # p("Click the button only once to make a prediction displayed in the main panel. Then use the slider to adjust the input with a direct prediction output.")
                 
                 # tags$h5("actionButton with CSS class:"),
                 # actionButton("action2", "Action button", class = "btn-primary")
                 ), # end tabPanel
        
                
                #===============================================================================
                #                               COMPLEX MODEL                                 #
                #===============================================================================
                 tabPanel("Complex model", value = "complex",
                          # Input: Select the parasitemia type ----
                          br(),
                          radioButtons("ptype2", "Which type of parasitemia data do you have?",
                                       c("Percentage of parasitemia" = "ppercentage2",
                                         "Parasitemia density [1/µl]" = "pdensity2"
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
                                        label = "Parasitemia density [1/µl]",
                                        value = 800000, min = 0, max = 1500000, step = 1000)
                          ),   
                          
                          # selectInput("wtype",
                          #             label = "Which data do you have?",
                          #             choices = c("Total number of white blood cells (* 10^9/ L)", "Percentage of lymphoctyes and monocytes"),
                          #             selected = "Total number of white blood cells (* 10^9/ L)"),
                          
                          radioButtons("wtype", "Which type of white blood cell data do you have?",
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
                          # actionButton(inputId = "go_complex", label = "Submit")
                          bookmarkButton(id = "bookmark2")
                    ) # end tabPanel
                  ) # end tabsetPanel
               ), # end sidebarPanel
               
               #===============================================================================
               #                               RESULTS: MAIN PANEL                            #
               #===============================================================================
        
        
               #===============================================================================
               #                               MAIN PANEL FOR SIMPLE MODEL                    #
               #===============================================================================
               
               mainPanel(
                 tabsetPanel(
                    tabPanel("Output",
                            #h4("Task: "),
                            # tableOutput("table"), # output input values
                            # h2("Task: ", textOutput("task")),
                            h3("Prediction:"),
                            # uiOutput("comp_simple"),
                            # verbatimTextOutput("test0"),
                            # verbatimTextOutput("comp_simple"),
                            
                            # conditionalPanel(
                            #   condition = "input.tabset == 'complex'",
                            #   verbatimTextOutput("comp_text")
                            # ),
                            verbatimTextOutput("comp_text"),
                            
                            # verbatimTextOutput("comp_simple_dens"),
                            # verbatimTextOutput("comp_complex"),
                            # verbatimTextOutput("comp_complex_dens"),
                            # verbatimTextOutput("comp_complex_counts"),
                            # verbatimTextOutput("comp_complex_counts_dens"),
                            
                            # conditionalPanel(condition = "input.pytype!='pdensity'", verbatimTextOutput("comp_simple")),
                            # conditionalPanel(condition = "input.pytype!='ppercentage'", verbatimTextOutput("comp_simple_dens")),
                            
                            
                            
                            #### ADD RIGHT CONDITIONS HERE --- DOES NOT WORK!
                            # conditionalPanel(condition = "input.pytype == 'ppercentage'",
                            #                  verbatimTextOutput("comp_simple")
                            #                  ),
                            # conditionalPanel(condition = "input.pytype == 'pdensity'",
                            #                  verbatimTextOutput("comp_simple_dens")
                            #                 ),
                            h3("Regression line:"),
                            p("The horizontal line represents the perentage of pathogen reads (dependent variable). As the input (dependent variable) changes, the horizontal line changes."),
                            # plotOutput("residuals", width=600, height=500)
                            plotlyOutput("residuals", width=600, height=500)
                            # ###########################################################
                            # fluidRow(
                            #   column(width = 6,
                            #          plotOutput("plot2", height = 300,
                            #                     brush = brushOpts(
                            #                       id = "plot2_brush",
                            #                       resetOnNew = TRUE
                            #                     )
                            #          )
                            #   ),
                            #   column(width = 6,
                            #          plotOutput("plot3", height = 300)
                            #   )
                            # )
                            # ############################################################
                      ),
                   tabPanel("Summary",
                            # ######################################################
                            # fileInput('file1', 'Choose CSV File',
                            #           accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv'))
                            # ######################################################
                            tableOutput("dummy"),
                            verbatimTextOutput("summary_simple")
                            )
                   
                 ) # end tabsetPanel
               ) # end mainPanel
            ), # end tabpanel
      
              #===============================================================================
              #                               MAIN PANEL FOR COMPLEX MODEL                   #
              #===============================================================================
              
              # mainPanel()
      
      # ### TAB 1 = dashboard:
      # tabPanel(tabName = "graphs",
      #         
      #         sidebarPanel(
      #               solidHeader = TRUE, status = "primary",
      #               
      #               sliderInput(inputId = "sample",
      #                           label = "Sample size",
      #                           value = 50, min = 10, max = 100),
      #               sliderInput(inputId = "slope",
      #                           label = "Regression slope",
      #                           value = .25, min = -2, max = 2,step = .25),
      #               # Sd slider:
      #               sliderInput(inputId = "SD",
      #                           label = "Standard deviation",
      #                           value = 3, min = 0, max = 50),
      #               actionButton(inputId = "refresh", label = "Simulate New Data" , 
      #                            icon = icon("fa fa-refresh"))
      #           ),
      #           
      #           mainPanel(
      #             
      #             box(width = 6,
      #                 title = "Regression",
      #                 solidHeader = TRUE, status = "primary",
      #                 plotOutput(outputId = "reg")),
      #             
      #             
      #             box(width = 6,title = "Sums of Squares Graphs",
      #                 solidHeader = F, status = "primary",
      #                 tabsetPanel(type = "tabs",
      #                             tabPanel("Total", plotOutput("total")),
      #                             tabPanel("Regression", plotOutput("regression")),
      #                             tabPanel("Error", plotOutput("error")),
      #                             tabPanel("Variance Partition", plotOutput(("variance")))
      #                             
      #                 )
      #             )
      #           ),
      #           fluidRow(
      #             box(width = 6,title = "Anova Table",
      #                 solidHeader = FALSE, status = "warning",
      #                 tableOutput(outputId = "anova")),
      #             
      #             box(width = 6,title = "Summary",
      #                 solidHeader = FALSE, status = "warning",
      #                 tableOutput(outputId = "summary")))
      #         ),
      # 
      # # TAB 2 = dashboard:

      #===============================================================================
      #                               TAB PANELS CONT'D                              #
      #===============================================================================
      tabPanel("Discussion", 
               uiOutput("discussion")
      ),
      tabPanel("Conclusion", 
               uiOutput("conclusion")
      ),
      tabPanel("Data", 
               uiOutput("data")
      ),
      tabPanel("References", 
               uiOutput("references")
      ),
      tabPanel("Glossary", 
               uiOutput("glossary")
      ),
      tabPanel("About", 
               uiOutput("about")
      )
    )
  )

