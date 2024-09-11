#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
rm(list = ls())
library(shiny)
library(tidyverse)
library(scales)
theme_set(theme_bw())


# Define server logic required to draw a histogram
function(input, output, session) {
  # Observe changes in the `scale` input and modify the title string accordingly
  reactive_settings <- reactive({
    # for yearly time_scale
    if (input$time_scale == 1) {
      if (input$GHG == "CO2") {
        list(
          subtitle = expression(CO[2] ~ " (megatonnes)"),
          landcover = c(
            "Barren",
            "Forestland",
            "Wetland",
            "Grassland",
            "Cropland"
          ),
          scaler = 1e-6,
          units = " Mt",
          legend_position = c(.3, 0.8),
          # caption = "Annual savings in soil carbon dioxide emissions in 2018.
          #          \nThe y-axis is in terms of megatonnes (1 million tonnes)"
          caption = ""
        )
      } else if (input$GHG == "N2O") {
        return(
          list(
            subtitle =  expression(N[2] * O ~ " (kilotonnes)"),
            landcover = c(
              "Barren",
              "Forestland",
              "Wetland",
              "Grassland",
              "Cropland"
            ),
            scaler = 1e-3,
            units = " kt",
            legend_position = c(.3, 0.8),
            # caption = "Annual savings in soil nitrous oxide emissions in 2018.
            #        \nThe y-axis is in terms of kilotonnes (1 thousand tonnes)"
            caption = ""
          )
        )
      } else if (input$GHG == "CH4") {
        return(
          list(
            subtitle = expression(CH[4] ~ " (kilotonnes)"),
            landcover = c(
              "Barren",
              "Forestland",
              "Wetland",
              "Grassland",
              "Cropland"
            ),
            scaler = 1e-3,
            units = " kt",
            legend_position = c(.8, 0.3),
            # caption = "Annual savings in soil methane emissions in 2018.\n
            #         Overall, paving over soil causes an increase in methane emissions \n
            #         because soil is a net producer of methane, hence the negative savings.
            #        \nThe y-axis is in terms of kilotonnes (1 thousand tonnes)"
            caption = ""
          )
        )
      }
      # now for seasonal time_scale
    } else if (input$time_scale == 2) {
      if (input$GHG == "CO2") {
        list(
          subtitle = expression(CO[2]~ " (megatonnes)"),
          landcover = c(
            "Barren",
            "Forestland",
            "Wetland",
            "Grassland",
            "Cropland"
          ),
          scaler = 1e-6,
          units = " Mt",
          legend_position = c(.2, 0.8)
          # limits = NULL
        )
      } else if (input$GHG == "N2O") {
        return(
          list(
            subtitle =  expression(N[2] * O~ " (kilotonnes)"),
            landcover = c(
              "Barren",
              "Forestland",
              "Wetland",
              "Grassland",
              "Cropland"
            ),
            scaler = 1e-3,
            units = " kt",
            legend_position = c(.2, 0.8)
            # limits = NULL
          )
        )
      } else if (input$GHG == "CH4") {
        return(
          list(
            subtitle = expression(CH[4]~ " (kilotonnes)"),
            landcover = c(
              "Barren",
              "Forestland",
              "Wetland",
              "Grassland",
              "Cropland"
            ),
            scaler = 1e-3,
            units = " kt",
            legend_position = c(.8, 0.3)
            # limits = c(0, 16 * 1e6)
          )
        )
      }
    }
  })
  output$distPlot <- renderPlot({
    # Calculate flux data -----------------------------------------------------
    plot_settings <- reactive_settings()
    
    if (input$scale == "log") {
      y_scale_label <- "\n(log scale)"
    } else{
      y_scale_label <- ""
    }
    
    if (input$time_scale == 1) {
      if (input$GHG == "All") {
        ghg_compare_annual <- read_csv("data/ghg_saving_comparison_df.csv")
        
        if(input$scale == "log"){
          ghg_compare_annual <- ghg_compare_annual %>% filter(GHG != "CH4")
        }
        
        p <- ghg_compare_annual %>%
          ggplot(aes(x = reorder(GHG, col_order), y = mean_co2e)) +
          geom_col() +
          geom_errorbar(
            aes(ymin = lower_co2e, ymax = upper_co2e),
            width = 0.4,
            colour = "orange",
            alpha = 0.9,
            size = 1.3
          )  +
          # geom_text(aes(label = round(1e-6 * mean_co2e, digits = 1)),
          #           vjust = -0.75,
          #           colour = "black") +
          scale_y_continuous(
            breaks = scales::pretty_breaks(n = 10),
            labels = label_number(scale = 1e-6, suffix = " Mt")
          ) +
          scale_x_discrete(labels = c(
            "All 3 GHGs",
            expression(CO[2]),
            expression(CH[4]),
            expression(N[2] *
                         O)
          )) +
          labs(
            x = "Greenhouse gas",
            y = "Flux not \nemitted due\n to sealing",
            subtitle =  expression(CO[2] * e~ "(tonnes)"),
            caption = plot_settings$caption
          ) +
          theme(text = element_text(size = 14),
                axis.title.y = element_text(angle = 0, vjust = 0.5))
      } else{
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
          # geom_label(
          #   data = roads_inc_ghg,
          #   aes(
          #     x = reorder(Landcover, mean, FUN = sum),
          #     y = mean,
          #     label = round(mean*plot_settings$scaler,1)
          #   ),
          #   vjust = -1,
          #   colour = "black"
          # ) +
          scale_y_continuous(
            breaks = scales::pretty_breaks(n = 10),
            # labels = label_number(scale_cut = cut_si("g"))
            labels = label_number(scale = plot_settings$scaler, suffix = plot_settings$units)
          ) +
          scale_fill_manual(
            values = c(
              "roads" = "grey30",
              "other" = "purple"
            ),
            labels = c("Other sealing \nsurfaces", "Roads")
          ) +
          labs(
            x = "Land cover",
            y = paste0("Flux not emitted \ndue to sealing", y_scale_label),
            fill = "",
            subtitle =  plot_settings$subtitle,
            caption = plot_settings$caption
          ) +
          theme(
            legend.position = "inside",
            legend.position.inside = plot_settings$legend_position,
            legend.background = element_blank(),
            axis.title.y = element_text(angle = 0, vjust = 0.5)
          )
      }
      if (input$scale == "log") {
        p <- p +
          annotation_logticks(sides = "lr") +
          scale_y_log10(
            breaks = scales::trans_breaks("log10", function(x)
              10 ^ x, n = 10),
            labels = scales::trans_format("log10", scales::math_format(10 ^ .x))
          )
        
        if(input$GHG == "All"){
          p <- p +
            scale_x_discrete(labels = c(
              "All 3 GHGs",
              expression(CO[2]),
              expression(N[2] *
                           O)
            ))+
            labs(caption = expression(CH[4]~" removed due to negative value"))
        }
      }
    } else if (input$time_scale == 2) {
      if (input$GHG != "All") {
        avg_monthly_fluxes <- read_csv(file = "data/avg_monthly_fluxes.csv")
        avg_monthly_fluxes$month <- factor(avg_monthly_fluxes$month, levels = month.abb)
        
        p <- avg_monthly_fluxes %>%
          filter(GHG == input$GHG) %>%
          mutate(
            fill_order = case_match(
              class,
              "Wetland" ~ 1,
              "Forestland" ~ 2,
              "Barren" ~ 3,
              "Cropland" ~ 4,
              "Grassland" ~ 5
            )
          ) %>%
          ggplot(aes(
            y = mean,
            x = month,
            fill = reorder(class, fill_order)
          )) +
          geom_col(position = "stack") +
          geom_hline(
            aes(yintercept = trend),
            linetype = "longdash",
            colour = "lightblue",
            alpha = 0.75
          ) +
          geom_hline(
            aes(yintercept = trend_low),
            # aes(yintercept = trend_low_q),
            linetype = "dashed",
            colour = "grey40",
            alpha = 0.5
          ) +
          geom_hline(
            aes(yintercept = trend_high),
            # aes(yintercept = trend_high_q),
            linetype = "dashed",
            colour = "grey40",
            alpha = 0.5
          ) +
          geom_errorbar(aes(ymin = sum_lower, ymax = sum_upper), width = 0.5) +
          # geom_errorbar(aes(ymin = sum_lower_q, ymax = sum_upper_q), width = 0.5) +
          scale_fill_manual(
            values = c(
              "Wetland" = "#440154FF",
              "Grassland" = "#5DC863FF",
              "Barren" = "#21908CFF",
              "Cropland" = "#FDE725FF",
              "Forestland" = "#3B528BFF"
            )
          ) +
          scale_y_continuous(
            labels = label_number(scale = plot_settings$scaler, suffix = plot_settings$units),
            # Converts to Megaton and adds "Mt"
            breaks = pretty_breaks(n = 10)
            # limits = plot_settings$limits
          ) +
          labs(
            fill = "",
            x = "Month",
            y = "Flux not\nemitted due\n to sealing",
            subtitle = plot_settings$subtitle
          ) +
          theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
                axis.text.x = element_text(angle = 90, vjust = 0.5))
      } else if (input$GHG == "All") {
        co2e_monthly <- read_csv("data/co2e_monthly.csv")
        co2e_monthly$month <- factor(co2e_monthly$month, levels = month.abb)

        p <- co2e_monthly %>%
          mutate(
            fill_order = case_match(
              class,
              "Wetland" ~ 1,
              "Forestland" ~ 2,
              "Barren" ~ 3,
              "Cropland" ~ 4,
              "Grassland" ~ 5
            )
          ) %>%
          # pivot_longer(cols = c(mean, lower, upper)) %>%
          # filter(GHG == "N2O") %>%
          ggplot(aes(
            y = mean,
            x = month,
            fill = reorder(class, fill_order)
          )) +
          geom_col(position = "stack") +
          geom_hline(
            aes(yintercept = trend),
            linetype = "longdash",
            colour = "lightblue",
            alpha = 0.75
          ) +
          geom_hline(
            aes(yintercept = trend_low_q),
            linetype = "dashed",
            colour = "grey40",
            alpha = 0.5
          ) +
          geom_hline(
            aes(yintercept = trend_high_q),
            linetype = "dashed",
            colour = "grey40",
            alpha = 0.5
          ) +
          geom_errorbar(
            aes(ymin = sum_lower_q, ymax = sum_upper_q),
            width = 0.5,
            colour = "black"
          ) +
          scale_fill_manual(
            values = c(
              "Wetland" = "#440154FF",
              "Grassland" = "#5DC863FF",
              "Barren" = "#21908CFF",
              "Cropland" = "#FDE725FF",
              "Forestland" = "#3B528BFF"
            )
          ) +
          scale_colour_viridis_d() +
          scale_y_continuous(
            labels = label_number(scale = 1e-6, suffix = " Mt"),
            # Converts to Megaton and adds "Mt"
            breaks = pretty_breaks(n = 10),
            limits = c(0, 16 * 1e6)
          ) +
          labs(
            fill = "",
            x = "Month",
            y = "Flux not\nemitted due\nto sealing",
            subtitle = expression(CO[2] * e)
          ) +
          theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
                axis.text.x = element_text(angle = 90, vjust = 0.5))
        
      }
      
    }
    print(p)
    
  })
  
}
