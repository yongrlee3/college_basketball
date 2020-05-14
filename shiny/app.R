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
data <- read_rds("raw-data/data_final.rds")

# assign dataset variables
statistics <- c("Win %" = "win_pct", "Offensive Efficiency" = "adjoe", "Defensive Efficiency" = "adjde", "Power Rating" = "barthag", "Field Goal %" = "efg_o", "Field Goal % Allowed" = "efg_d", "Turnover Rate" = "tor", "Steal Rate" = "tord", "Offensive Rebound %" = "orb", "Defensive Rebound %" = "drb", "Free Throw Rate" = "ftr", "Free Throw Rate Allowed" = "ftrd", "2-Pt Shooting %" = "twop_o", "2-Pt Shooting % Allowed" = "twop_d", "3-Pt Shooting %" = "threep_o", "3-Pt Shooting % Allowed" = "threep_d", "Tempo" = "adj_t", "Wins Above Bubble" = "wab")

# ui
ui <- navbarPage(
  theme = shinytheme("united"),
  "The Original March Madness",
  tabPanel(
    "About",
    tabsetPanel(
      tabPanel(
        "Data",
        br(),
        p("This analysis aggregates publicly available college basketball season and tournament data and utilizes regularized regression and random forest techniques to develop an algorithm that successfully predicts tournament outcomes. Season statistics from 2015 to 2019 are used to index team performance, while tournament data provides NCAA March Madness matchups and outcomes. Given the variability in statistical trends year over year, I ranked the performance measures of qualifying teams for each year by percentile rank. Since the 64 selected teams were generally ranked higher compared to their ineligible peers, the percentile ranks were calculated amongst the qualifying teams to generate a wider spread of rankings. Performance rankings for tournament eligible teams in future seasons can be calculated prior to the tournament as predictors in determining matchup outcomes. The following tab outlines the performance metrics that were considered for this predictive analysis."),
      ),
      tabPanel(
        "Metrics",
        br(),
        p("Win Percentage: Percentage of games won."),
        p("Offensive Efficiency: Measure of offensive efficiency a team would have against the average Division I defense. Adjusted to points scored per 100 possessions."),
        p("Defensive Efficiency: Measure of defensive efficiency a team would have against the average Division I offense. Adjusted to points allowed per 100 possessions."),
        p("Power Rating: Chance of beating an average Division I team."),
        p("Field Goal Percentage: Percentage of field goals made"),
        p("Field Goal Percentage Allowed: Percentage of field goals allowed"),
        p("Turnover Rate: "),
        p("Adjusted Tempo: Measure of tempo (possessions per 40 minutes) a team would have against the team that wants to play at an average Division I tempo."),
        p("Wins Above Bubble: Wins above the cut off between making the NCAA March Madness Tournament and not making it.")
      )
    )
  ),
  tabPanel(
    "Trends",
    sidebarPanel(
      selectInput(
        "comparison",
        "Comparison",
        c("Finalists" = "finals", "Final Four" = "final_4", "Elite Eight" = "elite_8", "Sweet Sixteen" = "sweet_16", "Round of 32" = "round_32")
      ),
      selectInput(
        "stat",
        "Statistic",
        statistics
      )
    ),
    mainPanel(
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
        theme_fivethirtyeight() +
        scale_color_discrete(name = "Qualified?", labels = c("No", "Yes")) +
        labs(x = "Year", y = input$stat)
    }
    else if (input$comparison == "elite_8") {
      elite_8 %>%
        ggplot(aes(year, elite_8[[input$stat]])) +
        geom_line(aes(group = threshold, color = threshold)) +
        theme_fivethirtyeight() +
        scale_color_discrete(name = "Qualified?", labels = c("No", "Yes")) +
        labs(x = "Year", y = input$stat)
    }
    else if (input$comparison == "sweet_16") {
      sweet_16 %>%
        ggplot(aes(year, sweet_16[[input$stat]])) +
        geom_line(aes(group = threshold, color = threshold)) +
        theme_fivethirtyeight() +
        scale_color_discrete(name = "Qualified?", labels = c("No", "Yes")) +
        labs(x = "Year", y = input$stat)
    }
    else if (input$comparison == "round_32") {
      round_32 %>%
        ggplot(aes(year, round_32[[input$stat]])) +
        geom_line(aes(group = threshold, color = threshold)) +
        theme_fivethirtyeight() +
        scale_color_discrete(name = "Qualified?", labels = c("No", "Yes")) +
        labs(x = "Year", y = input$stat)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
