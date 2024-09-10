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
  titlePanel("Pave the World?"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "time_scale",
        "Time scale",
        # inputID = "time_scale",
        # label = "Time scale",
        choices = list("Yearly" = 1, "Monthly" = 2),
        selected = 1
      ),
      # conditionalPanel(
        # condition = "input.time_scale == '1'",
        selectInput(
          inputId = "GHG",
          label = "Greenhouse Gas",
          choices = list(
            "All GHGs" = "All",
            "CO2" = "CO2",
            "CH4" = "CH4",
            "N2O" = "N2O"
          ),
          selected = "CO2"
        ),
        # Conditional input for selecting log base, only appears if "log" is selected
        conditionalPanel(
          condition = "input.GHG != 'CH4' && input.time_scale == '1'",
          selectInput(
            inputId = "scale",
            label = "Scale",
            choices = list("Normal scale" = "normal" , "Log scale" = "log"),
            selected = "normal"
          )
        )
      # )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(plotOutput("distPlot"))
  ))
