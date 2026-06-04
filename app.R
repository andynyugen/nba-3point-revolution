library(shiny)
library(bslib)
library(shinythemes) # package for R shiny app themes
library(hoopR) # package for NBA data
library(tidyverse)
library(plotly) # package for interactive plots
library(rsconnect)

# Load datasets saved as .rds files for fast app startup
three_point_trend       <- readRDS("three_point_trend.rds")      # league-wide 3PT trend by season
team_three_point_trend  <- readRDS("team_three_point_trend.rds") # team-level 3PT trend by season
wins_vs_3pa             <- readRDS("wins_vs_3pa.rds")            # team win percentage vs avg 3PA by season
shot_breakdown          <- readRDS("shot_breakdown.rds")         # league shot type breakdown by era
team_shot_breakdown     <- readRDS("team_shot_breakdown.rds")    # team shot breakdown by era

# Get list of teams for dropdown choice filter
team_choices <- c("N/A", sort(unique(team_three_point_trend$team_display_name)))

# Define UI ----
ui <- page_sidebar(
  
  # Apply flatly Bootstrap theme for app styling
  theme = shinytheme("flatly"),
  
  title = "The 3-Point Revolution 🏀",
  
  # Slide bar for filtering by season ranges
  sidebar = sidebar(
    sliderInput(
      inputId = "season_range",
      label   = "Season Range",
      min     = 2002,
      max     = 2026,
      value   = c(2002, 2026),
      sep     = ""
    ),
    # Drop-down filter to highlight specific NBA teams across all tabs
    selectInput(
      inputId  = "team",
      label    = "Team Filter",
      choices  = team_choices,
      selected = "N/A" # default to no team selected (League Average)
    )
  ),
  
  navset_tab(
    
    # About tab - Background information, context, and instructions
    nav_panel("About",
              h2("The 3-Point Revolution: How the NBA Reinvented Itself"),
              p("Over the past two decades, the NBA has undergone one of the most dramatic 
                strategic shifts in professional sports history. Teams have systematically 
                abandoned the mid-range jumper in favor of longer-distance three-point shots, 
                fundamentally changing how basketball is played at the highest level."),
              p("This app explores that shift using play-by-play and team box score data from 
                2002 to 2026, sourced through the hoopR R package. The data covers over 14 
                million plays across 24 NBA seasons."),
              h4("How to use this app:"),
              tags$ul(
                tags$li("Use the ", tags$b("Season Range"), " slider to filter visualizations to a specific time period"),
                tags$li("Use the ", tags$b("Team"), " dropdown to highlight a specific franchise across all three tabs"),
                tags$li("Navigate between tabs to explore different aspects of the three-point revolution"),
                tags$li("Hover over any point on the League Trend chart to see the exact three-point rate for that season"),
                tags$li("Hover over highlighted team dots on the Wins vs 3PA tab to see exact season stats"),
                tags$li("Click legend items on the League Trend or Shot Breakdown chart to show or hide individual lines")
              ),
              p(tags$em("Data Source: hoopR R package / ESPN NBA Stats API"))
    ),
    
    # League Trend tab - time series of league-wide 3PT attempt rate
    nav_panel("League Trend",
              br(), # Add padding between navigation tabs and plot
              p("This chart tracks the percentage of all field goal attempts that were 
                three-pointers across NBA seasons from 2002 to 2026. The steady upward trend 
                reflects the league's growing reliance on the three-point shot. Use the season 
                slider to zoom into specific eras and the team dropdown to overlay a specific 
                franchise's trend against the league average. You can also click on the legend 
                to show or hide individual lines, and hover over any point to see the exact 
                values for that season."),
              plotlyOutput("league_trend")
    ),
    
    # Wins vs 3PA tab - scatter plot of 3PA vs win percentage
    nav_panel("Wins vs 3PA",
              br(), # Add padding between navigation tabs and plot
              p("This scatter plot shows the relationship between a team's average three-point attempts per game 
                and their win percentage for each season. Each dot represents one team in one season, colored by 
                year from oldest (dark purple) to most recent (yellow). A slight positive trend line is shown with 
                its correlation coefficient (r), which indicates the strength of the relationship. Select a team from
                the dropdown to highlight their seasons and hover over the highlighted dots to see exact stats."),
              plotlyOutput("wins_vs_3pa")
    ),
    
    # Shot Breakdown tab - bar chart of shot types by era
    nav_panel("Shot Breakdown",  
              br(), # Add padding between navigation tabs and plot
              p("This chart breaks down shot selection into three categories across four eras: three-pointers, 
                at-rim attempts (dunks and layups), and mid-range shots. The dramatic decline 
                of the mid-range jumper and the rise of the three-pointer tells the core story 
                of the three-point revolution. Select a team from the dropdown to see how their 
                shot selection compares to the league-wide trend. Hover over any bar segment to see the exact 
                percentage for that shot type and era."),
              plotlyOutput("shot_breakdown")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  # League Trend ----
  output$league_trend <- renderPlotly({
    
    # Build base plot with league-wide 3PT attempt rate filtered by season range
    p <- three_point_trend |>
      # Filter data by selected season range
      filter(season >= input$season_range[1],
             season <= input$season_range[2]) |>
      ggplot(aes(x = season, y = three_point_attempt_pct)) +
      # map color to legend label so it appears in legend
      geom_line(aes(color = "NBA League Average"), linewidth = 1) +
      geom_point(
        size  = 2,
        aes(color = "NBA League Average",
            # custom hover tooltip stat box
            text = paste0("Season: ", season,
                          "<br>3PT Rate: ", round(three_point_attempt_pct, 1), "%"))
      ) +
      # manual color scale which includes team color for when team is selected
      scale_color_manual(
        name   = "",
        values = c("NBA League Average" = "orange", setNames("#009E73", input$team))
      ) +
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
    
    # Overlay trend for selected team if a team is selected
    if (input$team != "N/A") {
      team_data <- team_three_point_trend |>
        # Filter data by selected season range
        filter(team_display_name == input$team,
               season >= input$season_range[1],
               season <= input$season_range[2]) |>
        # normalize team 3PA to same scale as league-wide attempt rate
        mutate(team_3pa_pct = avg_3pa / max(avg_3pa) * max(three_point_trend$three_point_attempt_pct))
      
      # Add separate trend line and season points for selected team to contrast with league average trend
      p <- p +
        geom_line(data = team_data,
                  aes(x = season, y = team_3pa_pct, color = input$team),
                  linewidth = 1) +
        geom_point(data = team_data,
                   aes(x     = season, 
                       y     = team_3pa_pct,
                       color = input$team,
                       # custom hover toolkit stat box
                       text  = paste0("Season: ", season,
                                      "<br>3PT Rate: ", round(avg_3pa, 1), "%")),
                   size = 2)
    }
    
    # convert ggplot to interactive plotly, show only custom text tooltips
    ggplotly(p, tooltip = "text")
  })
  
  # Wins vs 3PA ----
  output$wins_vs_3pa <- renderPlotly({
    # Filter data by selected season range
    filtered <- wins_vs_3pa |>
      filter(season >= input$season_range[1],
             season <= input$season_range[2])
    
    # calculate r coefficient between 3PA and win percentage
    r_val <- round(cor(filtered$avg_3pa, filtered$win_pct), 2)
    
    # Build base of scatter plot
    p <- filtered |>
      ggplot(aes(x = avg_3pa, y = win_pct)) +
      geom_point(aes(color = season), alpha = 0.4, size = 2) + # team dots colored by season
      geom_smooth(method = "lm", color = "black", se = FALSE) + # linear trend line
      # annotate trend line with r coefficient
      annotate("text",
               x     = max(filtered$avg_3pa) * 0.75,
               y     = min(filtered$win_pct) + 5,
               label = paste0("r = ", r_val),
               size  = 4,
               color = "black")
    
    # Highlight selected team
    if (input$team != "N/A") {
      team_data <- filtered |>
        filter(team_display_name == input$team)
      
      # Highlight points (red) for all seasons for selected team and add hovering interactivity
      p <- p +
        geom_point(data  = team_data,
                   aes(x = avg_3pa, y = win_pct,
                       # custom hover toolkit stat box
                       text = paste0(input$team,
                                     "<br>Season: ", season,
                                     "<br>Avg 3PA: ", round(avg_3pa, 1), " shots per game",
                                     "<br>Win Percentage: ", round(win_pct, 1), "%")),
                   color = "red", 
                   size  = 2
        )
    }
    
    # Add further aesthetics (scales, labels, theme, etc.)
    p <- p +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      # customize gradient scale
      scale_color_viridis_c(
        limits = c(input$season_range[1], input$season_range[2]),
        breaks = c(input$season_range[1], 
                   round(mean(c(input$season_range[1], input$season_range[2]))),  # midpoint
                   input$season_range[2]),
        guide  = guide_colorbar(title = "Season", title.position = "top")
      ) +
      labs(
        title   = "Do More 3-Point Attempts Lead to More Wins?",
        x       = "Average 3-Point Attempts per Game",
        y       = "Win Percentage",
        color   = "Season",
        caption = "Source: hoopR / ESPN"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), # bold pot title
            axis.text  = element_text(color = "black", size = 8), # darker clearer axis labels
            axis.title = element_text(color = "black", size = 13) # darker axis titles
      )
    
    # convert ggplot to interactive plotly, show only custom text tooltips
    ggplotly(p, tooltip = "text")
  })
  
  # Shot Breakdown ----
  output$shot_breakdown <- renderPlotly({
    
    # If no team selected, show league average by eras
    if (input$team == "N/A") {
      plot_data <- shot_breakdown |>
        mutate(
          era       = factor(era, levels = c("2002-2009", "2010-2015", "2016-2020", "2021-2026")),
          shot_type = factor(shot_type, levels = c("Mid-Range", "At Rim", "3-Point"))
        )
      title <- "How Shot Selection Has Changed by Era - League Average"
      
    # Else, show selected team averages by eras
    } else {
      # filter to selected team and apply same factor ordering
      plot_data <- team_shot_breakdown |>
        filter(team_display_name == input$team) |>
        mutate(
          era       = factor(era, levels = c("2002-2009", "2010-2015", "2016-2020", "2021-2026")),
          shot_type = factor(shot_type, levels = c("Mid-Range", "At Rim", "3-Point"))
        )
      # Display team name in plot title if selected
      title <- paste("How Shot Selection Has Changed by Era —", input$team) 
    }
    
    # Build stacked bar chart with custom hover tooltips
    p <- plot_data |>
      ggplot(aes(x    = era, 
                 y    = pct, 
                 fill = shot_type,
                 # custom hover toolkit stat box
                 text = paste0("Shot Type: ", shot_type,
                               "<br>Percentage: ", round(pct, 1), "%"))) +
      geom_col(position = "stack") +
      scale_fill_manual(values = c(
        "3-Point"   = "#009E73",
        "At Rim"    = "#E69F00",
        "Mid-Range" = "#56B4E9"
      )
      ) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      labs(
        title   = title,
        x       = NULL,
        y       = "Percentage of Shots",
        fill    = "Shot Type: ",
        caption = "Source: hoopR / ESPN"
      ) +
      theme_minimal() +
      theme(plot.title      = element_text(hjust = 0.5, face = "bold"), # bold title
            axis.text       = element_text(color = "black", size = 11), # darker clearer axis labels
            axis.title.y    = element_text(color = "black", size = 15), # darker y-axis title
            legend.position = "bottom"                                  # display legend below plot
      )
    
    # convert ggplot to interactive plotly, show only custom text tooltips
    ggplotly(p, tooltip = "text")
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
