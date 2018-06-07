# read map prediction
# Predicts the percentage of reads that map to host and pathogen
# Felicia Burtscher
# Date: June 6, 2018

# ui.R

#source('../src/databuilder.R', local = TRUE)

### Title:

header <- dashboardHeader(title = "Read map prediction", titleWidth = 250)

### SideBar:
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Lay-summary", tabName = "graphs", icon = icon("fa fa-circle"), badgeLabel = "for user", badgeColor = "green"),
    menuItem("Introduction", tabName = "data", icon = icon("fa fa-circle")),
    menuItem("Methods", tabName = "graphs", icon = icon("fa fa-circle")),
    menuItem("Results", tabName = "about", icon = icon("fa fa-info-circle"), badgeLabel = "for user", badgeColor = "green"),
    menuItem("Discussion", tabName = "graphs", icon = icon("fa fa-circle")),
    menuItem("Conclusion", tabName = "graphs", icon = icon("fa fa-circle")),
    menuItem("Data", tabName = "graphs", icon = icon("fa fa-circle")),
    menuItem("References", tabName = "graphs", icon = icon("fa fa-circle")),
    menuItem("Glossary", tabName = "graphs", icon = icon("fa fa-circle")),
    menuItem("About", tabName = "graphs", icon = icon("file-code-o"))
  ) # end sidebarMenue
) # end sidebar


### Dashboard:
body <- dashboardBody(
  
  
  
  ### Tabintems:
  
  tabItems(
    
    ### TAB 1 = dashboard:
    tabItem(tabName = "Results",
            
            fluidRow(
              
              # Sample size slider
              box(width = 60, title = "Parameters",
                  solidHeader = TRUE, status = "primary",
                  
                sidebarPanel(
                  
                  tabsetPanel(id = "tabset",
                      
                      # THE SIMPLE MODEL ----        
                      tabPanel("Simple model", id = "simple",
                              
                              # Input: Select the parasitemia type ----
                              radioButtons("ptype", "Which type of parasitemia data do you have?",
                                           choices = c("Percentage of parasitemia" = "ppercentage",
                                                      "Parasitemia density (/µl)" = "pdensity"),
                                           selected = character(0)
                                           )
                              # conditionalPanel(condition = "input.type == 'ppercentage'",
                              #                  sliderInput(inputId = "parasitemia_percentage",
                              #                              label = "Percentage of parasitemia",
                              #                              value = 8, min = 1, max = 99, step = 1.
                              #                              )
                              #                  ),
                              # conditionalPanel(condition = "input.type == 'pdensity'",
                              #                 sliderInput(inputId = "parasitemia_density",
                              #                 label = "Parasitemia density (/µl)"),
                              #                 value = 800000, min = 0, max = 1500000, step = 100
                              #                 )
                        ), # end tabpanel
                      
                        # THE COMPLEX MODEL ----
                        tabPanel("Complex model", id = "complex",
                                 
                                radioButtons("ptype2", "Which type of parasitemia data do you have?",
                                              choices = c("Percentage of parasitemia" = "ppercentage2",
                                                          "Parasitemia density (/µl)" = "pdensity2"),
                                              selected = character(0)
                                            )
                                # conditionalPanel(condition = "input.type2 == 'ppercentage2'",
                                #                   sliderInput(inputId = "parasitemia_percentage2",
                                #                               label = "Percentage of parasitemia",
                                #                               value = 8, min = 1, max = 99, step = 1.
                                #                               )
                                #                   ),
                                # conditionalPanel(condition = "input.type2 == 'pdensity2'",
                                #                  slideInput(inputId = "parasitemia_density2",
                                #                             label = "Parasitemia density (/µl)",
                                #                             value = 800000, min = 0, max = 1500000, step = 100
                                #                             )
                                #                  )

                        ) # end tabPanel
                    ) # end tabsetPanel
                ), # close sidebarPanel
              
                
                
              # Main panel for displaying outputs ----
              mainPanel(
                
                

              ) # end mainpanel

            ) # end box
          ) # end fluidrow
        ), # end tabitem
    
    
    # TAB 4 = Discussion
    tabItem(tabName = "Discussion",
            fluidPage(
              box(width = 10,status = "success",
                  shiny::includeMarkdown("md/4_discussion.Rmd"))
            )
    ), # end tabitem
    
    # TAB 3 = About
    tabItem(tabName = "About",
            fluidPage(
              box(width = 10,status = "success",
                  shiny::includeMarkdown("README.md"))
            )
    ) # end tabitem
  ) # end tabitems
 ) # end dashboardbody



ui <- dashboardPage(header, sidebar, body)

