# read map prediction
# Predicts the percentage of reads that map to host and pathogen
# Felicia Burtscher
# Date: June 6, 2018

# ui.R

#source('../src/databuilder.R', local = TRUE)


#ui = bootstrapPage(
ui = tagList(
    #tags$style(type='text/css', ".selectize-input { font-size: 64px; line-height: 64px;} .selectize-dropdown { font-size: 28px; line-height: 28px; }"),
    tags$head(tags$style("#text1{color: red;
                         font-size: 60px;
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
      tabPanel("Methods", 
               uiOutput("methods")
      ),
      
      #===============================================================================
      #                               TAB PANEL: RESULTS                             #
      #===============================================================================
      tabPanel("Results",
               sidebarPanel(
                   
                 #fileInput("file", "File input:"),
                 textInput("user_name", "User name:", "Enter your name here!"),
                 
                 # helpText("Choose between the simple and a more complex model."),
                 
                 tabsetPanel(id="tabset",

                #===============================================================================
                #                               SIMPLE MODEL                                   #
                #===============================================================================
                tabPanel("Simple model", id = "simple",
                 
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
                               label = "Parasitemia density [1/ql]",
                               value = 800000, min = 0, max = 1500000, step = 1000)
                 ),
                 
                 
                 # sliderInput("slider", "Slider input:", 1, 100, 30),
                 # tags$h5("Deafult actionButton:"),
                 actionButton("go_simple", "Compute")
                 
                 # tags$h5("actionButton with CSS class:"),
                 # actionButton("action2", "Action button", class = "btn-primary")
                             ), # end tabPanel
                 
                 br(),
                
                #===============================================================================
                #                               COMPLEX MODEL                                 #
                #===============================================================================
                 tabPanel("Complex model", id = "complex",
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
                          )
                    ) # wnd tabPanel
                  ) # end tabsetPanel
               ), # end sidebarPanel
               
               #===============================================================================
               #                               RESULTS: MAIN PANEL                            #
               #===============================================================================
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Output",
                            h4("Input values: "),
                            # tableOutput("table"), # output input values
                            h4("Prediction:"),
                            verbatimTextOutput("txtout"),
                            h1("Header 1"),
                            h2("Header 2"),
                            h3("Header 3"),
                            h4("Header 4"),
                            h5("Header 5")
                   ),
                   tabPanel("Summary"),
                   tabPanel("Plot")
                   
                 )
               )
      ),

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

