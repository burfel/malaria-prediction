# read map prediction
# Predicts the percentage of reads that map to host and pathogen
# Felicia Burtscher
# Date: June 6, 2018

# ui.R

#source('../src/databuilder.R', local = TRUE)
# setwd("shinyapp4")

#ui = bootstrapPage(
ui = tagList(
    # tags$style(type='text/css', ".selectize-input { font-size: 64px; line-height: 64px;} .selectize-dropdown { font-size: 28px; line-height: 28px; }"),
    # tags$head(tags$style("#text1{color: blue;
    #                      font-size: 120px;
    #                      font-style: italic;
    #                      }")
   # tags$style(type = "text/css",
   #           "label { font-size: 40px; }"
   #            ),
    shinythemes::themeSelector(),
    # tags$style(type = 'text/css', '.navbar { background-color: #262626;
    #                                            font-family: Arial;
    #                                            font-size: 13px;
    #                                            color: #FF0000; }'),
    navbarPage(
      #theme = "spacelab",  # <--- To use a theme, uncomment this
      # "Read map prediction",
      "Mapped read prediction in Malaria",
      
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
                  # uiOutput("methods"),
        # tabPanel("Table"
        # #DT::dataTableOutput("table")
        #         ),
        tabPanel("OVERVIEW",
                 includeMarkdown("md/2_methods.Rmd"
                 # tags$footer("My footer", align = "center", style = "
                 #                             position:absolute;
                 #                             bottom:0;
                 #                             width:100%;
                 #                             height:50px;   /* Height of the footer */
                 #                             color: white;
                 #                             padding: 10px;
                 #                             background-color: black;
                 #                             z-index: 1000;")
                 )
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

                 # ###########################
                 # ggplot(dat.nc.nona, aes(Percentage.parasitemia, outcome)) +
                 #   scale_x_continuous(name="Percentage of parasitemia", limits=c(0,50)) +  ## -- with it pdenstiy does not show up
                 #   scale_y_continuous(name="Percentage of reads mapping to pathogen", limits=c(0,1)) +
                 #   geom_text(label=rownames(dat.nc.nona)) +
                 #   geom_point(colour="dodgerblue",alpha=0.75) +
                 #   geom_abline(aes(colour="linear model on complete \n samples (21) \n", intercept=0.090267, slope=0.013339), alpha=1, size=1) +
                 #   geom_smooth(aes(colour="linear model on samples that complete \n after variable selection (40) \n"), method = "lm", linetype="dashed", se=FALSE) +
                 #   geom_smooth(data = dat.nc.nona[-c(7,10,11,19),], aes(colour="linear model without potential outliers 7,10,11,19"), method="lm", se=F, linetype = "dashed") +
                 #   scale_colour_manual(name="Linear regression lines \n", values=c("red", "blue", "green")) +
                 #   guides(colour = guide_legend(override.aes = list(alpha = 0))) +
                 #   theme(axis.text=element_text(size=14),
                 #         axis.title=element_text(size=14,face="bold")) +
                 #   theme(plot.title = element_text(size = 12, face = "bold"),
                 #         legend.title=element_text(size=15),
                 #         legend.text=element_text(size=13))
                 # ##########################
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
      tabPanel("RESULTS | web tool",
               
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
                               value = 8, min = 1, max = 99, step = 0.5)
                 ),
                 conditionalPanel(
                   condition = "input.ptype == 'pdensity'",
                   sliderInput(inputId = "parasitemia_density",
                               label = "Parasitemia density [1/µl]",
                               value = 800000, min = 0, max = 3000000, step = 1000)
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
                                        value = 8, min = 1, max = 99, step = 0.5)
                            
                          ),
                          conditionalPanel(
                            condition = "input.ptype2 == 'pdensity2'",
                            sliderInput(inputId = "parasitemia_density2",
                                        label = "Parasitemia density [1/µl]",
                                        value = 800000, min = 0, max = 3000000, step = 1000)
                          ),   
                          
                          # withMathJax(),
                          # uiOutput("power"),
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
                                        value = 10, min = 0, max = 100, step = .25)
                          ),
                          # OR
                          conditionalPanel(
                            condition = "input.wtype == 'counts'",
                            sliderInput(inputId = "lympho",
                                        label = "Total number of lymphoctyes (* 10^9/ L)",
                                        value = 3, min = 0, max = 30, step = .1),
                            sliderInput(inputId = "mono",
                                        label = "Total number of of monocytes (* 10^9/ L)",
                                        value = 1, min = 0, max = 10, step = .1),
                            sliderInput(inputId = "neutro",
                                        label = "Total number of neutrophils (* 10^9/ L)",
                                        value = 6, min = 0, max = 60, step = .1)
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
                            h3("Prediction of the generalized linear model (GLM) with logit link function:"),
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
                            br(),
                            h3("Linear regression model (ie GLM with identity as link function and normally distributed response variable):"),
                          
                            p("The horizontal line represents the percentage of pathogen reads (dependent variable). 
                              As the input (independent variable(s)) change(s), the horizontal line changes."),
                            p("The results of the linear model differ only slightly from the results above."),
                            # plotOutput("residuals", width=600, height=500)
                            textOutput("link_text"),
                            uiOutput("tab"),
              
                            br(),
                            
                            # conditionalPanel(condition = "tabset == 'simple'",
                            #                  plotlyOutput("residuals", width=600, height=500)
                            #                  ),
                           
                            useShinyalert(),
                            actionButton("F_help", "What is a F-test?"),
                            useShinyalert(),
                            actionButton("Rsq_help", "What is a R-squared value?"),
                            useShinyalert(),
                            actionButton("confidenceInterval_help", "What do the different colours mean?"),
                            br(), br(),
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
                   # tabPanel("Summary",
                   #          # ######################################################
                   #          # fileInput('file1', 'Choose CSV File',
                   #          #           accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv'))
                   #          # ######################################################
                   #          br(),
                   #          p("The following table shows the statistics of the linear model, the simple and complex version
                   #            based on the training data of 21 complete samples compared to the models for 46 samples."),
                   #          tableOutput("dummy")
                   # 
                   # ),
                   # tabPanel("ANOVA",
                   #          br(),
                   #          p("For the generalized linear models that are implemented behind this web tool you can calculate the p-value for yourself!"),
                   #        
                   #          # p("You can choose between comparing the fitted models with the anova function or with the likelihood ratio test."),
                   #          h3("Comparing different fitted models with ANOVA"),                         
                   #          p("Choose which models you would like to compare and which statistical test you would like to use to compare them!
                   #            \n Then choose for yourself, which model you would like to trust."),
                   #          useShinyalert(),
                   #          actionButton("help_anova", "What is ANOVA?"), 
                   #          br(), br(),
                   #          selectInput("model1_select",
                   #                       label = "Model 1",
                   #                       choices = c("Null" = "null", "No Null" = "no_null"),
                   #                       selected = "null"),
                   #          
                   #          selectInput("model2_select",
                   #                      label = "Model 2",
                   #                      choices = c("Simple model with percentage of parasitemia" = "simple_paras1", 
                   #                                  "Simple model with parasitemia density [1/µl]" = "simple_paras1_dens",
                   #                                  "Complex model with percentage of parasitemia and total white blood cell count" = "complex_paras1",
                   #                                  "Complex model with parasitemia density [1/µl] and total white blood cell count" = "complex_paras1_dens"
                   #                                  ),
                   #                      selected = "simple_paras1"),
                   #          
                   #          selectInput("model3_select",
                   #                      label = "Model 3",
                   #                      choices = c("Simple model with percentage of parasitemia" = "simple_paras2", 
                   #                                  "Simple model with parasitemia density [1/µl]" = "simple_paras1_dens",
                   #                                  "Complex model with percentage of parasitemia and total white blood cell count" = "complex_paras1",
                   #                                  "Complex model with parasitemia density [1/µl] and total white blood cell count" = "complex_paras1_dens"
                   #                                  ),
                   #                      selected = "complex_paras1"),
                   #          
                   #          selectInput("test_select",
                   #                      label = "Test",
                   #                      choices = c("Chisq" = "chiq", 
                   #                                  "F" = "f",
                   #                                  "Rao" = "rao"),
                   #                      selected = "chisq"),
                   #          # br(),
                   #          verbatimTextOutput("anova_summary")
                   #          #verbatimTextOutput("summary_simple")
                   #          ),
                   tabPanel("Likelihood ratio test",
                            h3("Likelihood ratio test"),  
                            
                            useShinyalert(),
                            actionButton("help_likelihoodratio", "What is the likelihood ratio test?"), 
                            br(), br(),
                            selectInput("lrt_select",
                                        label = "Which Model would you like to compare against the null model?",
                                        choices = c("Simple model with percentage of parasitemia" = "simple_paras_lr", 
                                                    "Simple model with parasitemia density [1/µl]" = "simple_paras_dens_lr",
                                                    "Complex model with percentage of parasitemia and total white blood cell count" = "complex_paras_lr",
                                                    "Complex model with parasitemia density [1/µl] and total white blood cell count" = "complex_paras_dens_lr"
                                        ),
                                        selected = "complex_paras_lr"),
                            
                            verbatimTextOutput("likelihoodratio_summary")
                   ) # end tabPanel
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
               # tableOutput("ref")
               uiOutput("references")
      ),
      tabPanel("Glossary", 
               tableOutput("ref")
               # uiOutput("glossary")
      ),
      tabPanel("About", 
               uiOutput("about")
      )
    )
  )

