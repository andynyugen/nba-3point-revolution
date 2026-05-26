#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(hoopR)
library(tidyverse)

team_choices <- c("None", sort(unique(team_trend$team_display_name)))

# Define UI ----
ui <- page_sidebar(
  title = "The 3-Point Revolution",
  
  # Slide bar for season ranges
  sidebar = sidebar(
    sliderInput(
      inputId = "season_range",
      label   = "Season Range",
      min     = 2002,
      max     = 2026,
      value   = c(2002, 2026),
      sep     = ""
    ),
    # Drop-down filter for NBA teams
    selectInput(
      inputId  = "team",
      label    = "Highlight Team",
      choices  = team_choices,
      selected = "None"
    )
  ),
  navset_tab(
    nav_panel("League Trend",    plotOutput("league_trend")),
    nav_panel("Wins vs 3PA",     plotOutput("wins_vs_3pa")),
    nav_panel("Shot Breakdown",  plotOutput("shot_breakdown"))
  )
)

# Define server logic ----
server <- function(input, output) {
  
  # League Trend
  output$league_trend <- renderPlot({
    p <- three_point_trend |>
      filter(season >= input$season_range[1],
             season <= input$season_range[2]) |>
      ggplot(aes(x = season, y = three_point_attempt_pct)) +
      geom_line(color = "orange", linewidth = 1) +
      geom_point(color = "orange", size = 2)
    
    # Overlay selected team if not None
    if (input$team != "None") {
      team_data <- team_trend |>
        filter(team_display_name == input$team,
               season >= input$season_range[1],
               season <= input$season_range[2]) |>
        mutate(team_3pa_pct = avg_3pa / max(avg_3pa) * max(three_point_trend$three_point_attempt_pct))
      
      p <- p +
        geom_line(data  = team_data,
                  aes(x = season, y = team_3pa_pct),
                  color = "#D55E00", linewidth = 1) +
        geom_point(data = team_data,
                   aes(x = season, y = team_3pa_pct),
                   color = "#D55E00", size = 2)
    }
    
    p +
      scale_x_continuous(breaks = seq(2002, 2026, by = 2)) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      labs(
        title   = "The Rise of the Three-Point Shot",
        x       = "Season",
        y       = "3PT Attempt Rate (% of all shots)",
        caption = "Source: hoopR / ESPN"
      ) +
      theme_minimal() +
      theme(
        plot.title  = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # Wins vs 3PA
  output$wins_vs_3pa <- renderPlot({
    filtered <- wins_vs_3pa |>
      filter(season >= input$season_range[1],
             season <= input$season_range[2])
    
    p <- filtered |>
      ggplot(aes(x = avg_3pa, y = win_pct)) +
      geom_point(aes(color = season), alpha = 0.4, size = 2) +
      geom_smooth(method = "lm", color = "black", se = FALSE)
    
    # Highlight selected team
    if (input$team != "None") {
      team_data <- filtered |>
        filter(team_display_name == input$team)
      
      p <- p +
        geom_point(data  = team_data,
                   aes(x = avg_3pa, y = win_pct),
                   color = "#D55E00", size = 4) +
        geom_text(data  = team_data,
                  aes(x = avg_3pa, y = win_pct, label = season),
                  color = "#D55E00", vjust = -1, size = 3)
    }
    
    p +
      scale_color_viridis_c() +
      labs(
        title   = "Do More 3-Point Attempts Lead to More Wins?",
        x       = "Avg 3-Point Attempts per Game",
        y       = "Win Percentage (%)",
        color   = "Season",
        caption = "Source: hoopR / ESPN"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  # Shot Breakdown
  output$shot_breakdown <- renderPlot({
    
    # If no team selected, show league average by eras
    if (input$team == "None") {
      plot_data <- shot_breakdown |>
        mutate(
          era       = factor(era, levels = c("2002-2009", "2010-2015", "2016-2020", "2021-2026")),
          shot_type = factor(shot_type, levels = c("Mid-Range", "At Rim", "3-Point"))
        )
      title <- "How Shot Selection Has Changed by Era (League Average)"
    
    # Else, show selected team averages by eras
    } else {
      plot_data <- team_shot_breakdown |>
        filter(team_display_name == input$team) |>
        mutate(
          era       = factor(era, levels = c("2002-2009", "2010-2015", "2016-2020", "2021-2026")),
          shot_type = factor(shot_type, levels = c("Mid-Range", "At Rim", "3-Point"))
        )
      title <- paste("Shot Breakdown —", input$team)
    }
    
    plot_data |>
      ggplot(aes(x = era, y = pct, fill = shot_type)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c(
                                    "3-Point"   = "#0072B2",
                                    "At Rim"    = "#E69F00",
                                    "Mid-Range" = "#999999"
                                  )
      ) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      labs(
        title   = title,
        x       = NULL,
        y       = "Percentage of Shots",
        fill    = "Shot Type",
        caption = "Source: hoopR / ESPN"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
