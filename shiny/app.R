# load packages
library(shiny)
library(shinythemes)
library(ggthemes)

# load data
season <- read_rds("raw-data/data_season.rds")
finals <- read_rds("raw-data/finals.rds")
final_4 <- read_rds("raw-data/final_4.rds")
elite_8 <- read_rds("raw-data/elite_8.rds")
sweet_16 <- read_rds("raw-data/sweet_16.rds")
round_32 <- read_rds("raw-data/round_32.rds")

# assign dataset variables
statistics <- c("Adjusted Offensive Efficiency" = "adjoe", "Adjusted Defensive Efficiency" = "adjde", "Power Rating" = "barthag", "Effective Field Goal Percentage" = "efg_o", "Effective Field Goal Percentage Allowed" = "efg_d", "Turnover Rate" = "tor", "Steal Rate" = "tord", "Offensive Rebound Percentage" = "orb", "Defensive Rebound Percentage" = "drb", "Free Throw Rate" = "ftr", "Free Throw Rate Allowed" = "ftrd", "Two-Point Shooting Percentage" = "2p_o", "Two-Point Shooting Percentage Allowed" = "2p_d", "Three-Point Shooting Percentage" = "3p_o", "Three-Point Shooting Percentage Allowed" = "3p_d", "Adjusted Tempo" = "adj_t", "Wins Above Bubble" = "wab", "Win Percentage" = "win_pct")

# ui
ui <- navbarPage(
  theme = shinytheme("united"),
  "The Original March Madness",
  tabPanel(
    "Trends",
    column(
      6,
      selectInput(
        "comparison",
        "Comparison",
        c("Finalists" = "finals", "Final Four" = "final_4", "Elite Eight" = "elite_8", "Sweet Sixteen" = "sweet_16", "Round of 32" = "round_32")
      )
    ),
    column(
      6,
      selectInput(
        "stat",
        "Statistic",
        statistics
      )
    ),
    column(
      12,
      plotOutput("stat_plot")
    )
  )
)

server <- function(input, output) {
  output$stat_plot <- renderPlot(
    if (input$comparison == "finals") {
      finals %>%
        ggplot(aes(year, finals[[input$stat]])) +
        geom_line(aes(group = threshold, color = threshold)) +
        theme_fivethirtyeight() +
        scale_color_discrete(name = "Qualified?", labels = c("No", "Yes")) +
        labs(x = "Year", y = input$stat)
    }
    else if (input$comparison == "final_4") {
      final_4 %>%
        ggplot(aes(year, final_4[[input$stat]])) +
        geom_line(aes(group = threshold, color = threshold)) +
        theme_classic() +
        theme(legend.position = "top") +
        scale_color_discrete(name = "Qualified?", labels = c("No", "Yes")) +
        labs(x = "Year", y = input$stat)
    }
    else if (input$comparison == "elite_8") {
      elite_8 %>%
        ggplot(aes(year, elite_8[[input$stat]])) +
        geom_line(aes(group = threshold, color = threshold)) +
        theme_classic() +
        theme(legend.position = "top") +
        scale_color_discrete(name = "Qualified?", labels = c("No", "Yes")) +
        labs(x = "Year", y = input$stat)
    }
    else if (input$comparison == "sweet_16") {
      sweet_16 %>%
        ggplot(aes(year, sweet_16[[input$stat]])) +
        geom_line(aes(group = threshold, color = threshold)) +
        theme_classic() +
        theme(legend.position = "top") +
        scale_color_discrete(name = "Qualified?", labels = c("No", "Yes")) +
        labs(x = "Year", y = input$stat)
    }
    else if (input$comparison == "round_32") {
      round_32 %>%
        ggplot(aes(year, round_32[[input$stat]])) +
        geom_line(aes(group = threshold, color = threshold)) +
        theme_classic() +
        theme(legend.position = "top") +
        scale_color_discrete(name = "Qualified?", labels = c("No", "Yes")) +
        labs(x = "Year", y = input$stat)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
