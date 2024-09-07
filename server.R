#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(scales)
theme_set(theme_bw())
 

# Define server logic required to draw a histogram
function(input, output, session) {
  # Observe changes in the `scale` input and modify the title string accordingly
  reactive_settings <- reactive({
    if (input$GHG == "CO2") {
      list(
        subtitle = expression(CO[2] ~ " (tonnes)"),
        landcover = c(
          "Barren",
          "Forestland",
          "Wetland",
          "Grassland",
          "Cropland"
        ),
        scaler = 1e-6,
        units = " Mt",
        legend_position = c(.1, 0.9)
      )
    } else if (input$GHG == "N2O") {
      return(list(
        subtitle =  expression(N[2] * O ~ " (tonnes)"),
        landcover = c(
          "Barren",
          "Forestland",
          "Wetland",
          "Grassland",
          "Cropland"
        ),
        scaler = 1e-3,
        units = " kt",
        legend_position = c(.1, 0.9)
      ))
    } else if (input$GHG == "CH4") {
      return(list(
        subtitle = expression(CH[4] ~ " (tonnes)"),
        landcover = c(
          "Barren",
          "Forestland",
          "Wetland",
          "Grassland",
          "Cropland"
        ),
        scaler = 1e-3,
        units = " kt",
        legend_position = c(.9, 0.25)
      ))
    }
  })
  output$distPlot <- renderPlot({
    # Calculate flux data -----------------------------------------------------
    plot_settings <- reactive_settings()
    
    full_data_ghg <- read_csv("data/no_corr_roads.csv") %>%
      bind_rows(read_csv("data/no_corr_no_roads.csv")) %>%
      filter(
        GHG == input$GHG &
          Landcover %in% plot_settings$landcover &
          Sealed_status == "fromsealing"
      ) %>%
      mutate(across(sd:upper, function(X) {
        (X / 1e6)
      }))
    
    roads_inc_ghg <- full_data_ghg %>% filter(source == "roads")
    #
    #
    # # Now calculate bar order -------------------------------------------------
    #
    #
    bar_order_ghg <- full_data_ghg %>% group_by(GHG, Landcover) %>%
      filter(source == "roads") %>%
      summarise(sum = sum(mean)) %>%
      arrange(sum) %>%
      rowid_to_column() %>%
      dplyr::select(-sum)
    
    p <- full_data_ghg %>%
      left_join(bar_order_ghg) %>%
      ggplot() +
      geom_col(aes(
        x = reorder(Landcover, rowid),
        y = mean,
        fill = source
      ), position = "identity") +
      geom_errorbar(
        data = roads_inc_ghg,
        aes(
          x = reorder(Landcover, mean, FUN = sum),
          ymin = lower,
          ymax = upper
        ),
        width = 0.4,
        colour = "orange",
        alpha = 0.9,
        size = 1.3
      ) +
      geom_label(
        data = roads_inc_ghg,
        aes(
          x = reorder(Landcover, mean, FUN = sum),
          y = mean,
          label = formatC(mean*plot_settings$scaler, digits = 2, format = "e", )
        ),
        vjust = -1,
        colour = "black"
      ) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 10),
        # labels = label_number(scale_cut = cut_si("g"))
        labels = label_number(scale = plot_settings$scaler, suffix = plot_settings$units)
      ) +
      scale_fill_manual(
        values = c("roads" = "grey30", "other" = "purple"),
        labels = c("Other sealing \nsurfaces", "Roads")
      ) +
      labs(
        x = "Land cover",
        y = expression("Flux not emitted due to sealing (" ~ log[10] ~ "scale)"),
        fill = "",
        subtitle =  plot_settings$subtitle
      ) +
      theme(
        legend.position = "inside",
        legend.position.inside = plot_settings$legend_position,
        legend.background = element_blank()
      )
    
    if (input$scale == "log") {
      p <- p +
        annotation_logticks(sides = "lr") +
        scale_y_log10(
          breaks = scales::trans_breaks("log10", function(x)
            10 ^ x, n = 10),
          labels = scales::trans_format("log10", scales::math_format(10 ^ .x))
        )
    }
    # generate bins based on input$bins from ui.R
    # x    <- faithful[, 2]
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #
    # # draw the histogram with the specified number of bins
    # hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #      xlab = 'Waiting time to next eruption (in mins)',
    #      main = 'Histogram of waiting times')
    
    print(p)
    
  })
  
}
