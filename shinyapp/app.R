# MINIMAL POSSIBLE SHINY APP
# 26-04-2018
# see also https://shiny.rstudio.com/articles/build.html

library(shiny)

# ----Define the USER INTERFACE-----
# Define UI for miles per gallon app ----
ui <- pageWithSidebar(

  # App title ----
  headerPanel("Prediction of read depth in whole blood dual-RNA Seq data from malaria patients"),

  
  # Define UI for miles per gallon app ----
  ui <- fluidPage(
    
    # App title ----
    titlePanel("Miles Per Gallon"),
    
    sliderInput(inputId = "num", # input name (for internal use?s)
                label= "Choose a value of parasetemia", # label to display
                value=25, min=1, max=100)
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        
        # use the mtcars data from the R datasets package, 
        # allows users to see a box-plot that explores the relationship between 
        # miles-per-gallon (MPG) and three other variables (Cylinders, Transmission, and Gears).
        
        # Ad 1: 
        # provide a way to select which variable to plot MPG against as well as 
        # provide an option to include or exclude outliers from the plot. 
        # For that, we add two elements to the sidebar, a selectInput to specify the variable 
        # and a checkboxInput to control display of outliers. 
        
        # Input: Selector for variable to plot against mpg ----
        selectInput("variable", "Variable:",
                    c("Cylinders" = "cyl",
                      "Transmission" = "am",
                      "Gears" = "gear")),
        
        # Input: Checkbox for whether outliers should be included ----
        checkboxInput("outliers", "Show outliers", TRUE)
        
      ),
      
      # # Main panel for displaying outputs ----
      # mainPanel(
      #   
      #   # Ad 3:
      #   # The server function assigned two output values: output$caption and output$mpgPlot. 
      #   # To update our user interface to display the output we need to add some elements to the main UI panel.
      #   # In the updated user-interface definition below you can see that we’ve added the caption as an h3 element 
      #   # and filled in its value using the textOutput function, and also rendered the plot by calling the plotOutput function:
      # 
      #   # Output: Formatted text for caption ----
      #   h3(textOutput("caption")),
      #   
      #   # Output: Plot of the requested variable against mpg ----
      #   plotOutput("mpgPlot")
        
      #)
    )
  )
  


# ----Define skeletal SERVER implementation-----
# Define server logic to plot various variables against mpg ----

# TODO: DEFINE RELATIONSHIP BETWEEN OUR INPUTS AND OUTPUTS

# Ad 2:
# Data pre-processing ----
# Tweak the "am" variable to have nicer factor labels -- since this
# doesn't rely on any user inputs, we can do this once at startup
# and then use the value throughout the lifetime of the app

#mpgData <- completeDat
#mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste("mpg ~", input$variable)
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  output$mpgPlot <- renderPlot({
    boxplot(as.formula(formulaText()),
            data = mpgData,
            outline = input$outliers,
            col = "#75AADB", pch = 19)
  })
  
}

# ----app.R-----
# Apply shinyApp function that uses the ui object and the server function defined to build a Shiny app object.

shinyApp(ui, server)


#------Specifying the user interface object and implementing the server function
#------1. ADDING INPUTS TO THE SIDEBAR

#------2. CREATING THE SERVER FUNCTION
# now: define server-side of the application which will accept inputs and compute outputs. 
# Server function shown below, illustrates some important concepts:
# - Accessing input using slots on the input object and generating output by assigning to slots on the output object.
# - Initializing data at startup that can be accessed throughout the lifetime of the application.
# - Using a reactive expression to compute a value shared by more than one output.
# Basic task of a Shiny server function is to define the relationship between inputs and outputs. 
# Our function does this by accessing inputs to perform computations and by assigning reactive expressions to output slots.

# NOTE: The use of renderText and renderPlot to generate output (rather than just assigning values directly) 
# is what makes the application reactive. These reactive wrappers return special expressions that are only re-executed when 
# their dependencies change. This behavior is what enables Shiny to automatically update output whenever input changes.

#------3. DISPLAYING OUTPUTS
