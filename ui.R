#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(# Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "GHG",
        label = "Greenhouse Gas",
        choices = list(
          "All GHGs" = "ALL",
          "CO2" = "CO2",
          "CH4" = "CH4",
          "N2O" = "N2O"
        ),
        selected = "CO2"
      ),
      # Conditional input for selecting log base, only appears if "log" is selected
      conditionalPanel(
        condition = "input.GHG != 'CH4'",
        selectInput(
          inputId = "scale",
          label = "Scale",
          choices = list("Normal scale" = "normal" , "Log scale" = "log"),
          selected = "normal"
        )
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(plotOutput("distPlot"))
  ))
