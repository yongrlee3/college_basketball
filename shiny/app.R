# load packages
library(shiny)
library(shinythemes)
library(ggthemes)
library(tidyverse)
library(gt)

# load data
finals <- read_rds("raw-data/finals.rds")
final_4 <- read_rds("raw-data/final_4.rds")
elite_8 <- read_rds("raw-data/elite_8.rds")
sweet_16 <- read_rds("raw-data/sweet_16.rds")
round_32 <- read_rds("raw-data/round_32.rds")
bracket <- read_rds("raw-data/bracket.rds")
final_four <- read_rds("raw-data/final_four.rds")
season_all <- read_rds("raw-data/season_all.rds")
win_composite <- read_rds("raw-data/win_composite.rds")
r32 <- read_rds("raw-data/r32.rds")
s16 <- read_rds("raw-data/s16.rds")
e8 <- read_rds("raw-data/e8.rds")
f4 <- read_rds("raw-data/f4.rds")
championship <- read_rds("raw-data/championship.rds") 

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
        "Background",
        h3("Data"),
        p("This analysis aggregates publicly available college basketball season and tournament data and utilizes regularized regression and random forest techniques to develop an algorithm that successfully predicts tournament outcomes. Season statistics from 2015 to 2019 are used to index team performance, while tournament data provides NCAA March Madness matchups and outcomes. Given the variability in statistical trends year over year, I ranked the performance measures of qualifying teams for each year by percentile rank. Since the 64 selected teams were generally ranked higher compared to their ineligible peers, the percentile ranks were calculated only within the qualifying teams to generate a wider spread of percentile rankings. Performance rankings for tournament eligible teams in future seasons can be calculated prior to the tournament as factors in predicting matchup outcomes. The following tab outlines some of the less intuitive performance metrics that were considered for this predictive analysis."),
        p("I use the more accurate predictive algorithm generated through random forests to generate a composite win total for each participating team. This score represents a team's total number of wins in a hypothetical scenario where each team competes against all 63 other teams. Using this composite win total, I provide interactive brackets for users to explore tournament history and examine matchups where disappointing favorites faltered and promising underdogs soared. I then apply this same methodology alongside 2020 season data to predict which teams were denied their shot at a national championship as a result of the COVID-19 crisis."),
        p(
          "You can find the data and code for this project on my ",
          a("Github",
            href = "https://github.com/yongrlee3/college_basketball"
          ),
          "."
        ),
        h3("About Me"),
        p("I am a former finance professional studying Education Policy and Management at the Harvard Graduate School of Education. I am passionate about leveraging data to inform strategic insights; namely in the fields of education, finance, and technology."),
        p(
          "You can reach me at ",
          a("yonglee@gse.harvard.edu",
            href = "mailto: yonglee@gse.harvard.edu",
          ),
          "or ",
          a("LinkedIn",
            href = "https://www.linkedin.com/in/yong-lee-034b39b8/"
          )
        )
      ),
      tabPanel(
        "Metrics",
        br(),
        p("Offensive Efficiency: Measure of offensive efficiency a team would have against the average Division I defense. Adjusted to points scored per 100 possessions."),
        p("Defensive Efficiency: Measure of defensive efficiency a team would have against the average Division I offense. Adjusted to points allowed per 100 possessions."),
        p("Power Rating: Chance of beating an average Division I team."),
        p("Adjusted Tempo: Measure of tempo a team would have against the team that wants to play at an average Division I tempo. Adjusted to possessions per 40 minutes"),
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
  ),
  tabPanel(
    "Analysis",
    h3("Methodology"),
    p("I used three regularized regressions (LASSO, Ridge, and Elastic Net) and a Random Forest to assess which predictors were most leveraged in determining game outcomes. All metrics with the exclusion of power ranking and wins above bubble were included in this analysis. The rationale for excluding these two metrics was two-fold: 1) I wanted to present an accessible and replicable predictive algorithm that uses basic statistical measures. Given that power ranking and wins above bubble are computed through third-party analyses, they may not be available in future iterations of the tournament. 2) Along those lines, I wanted to exclude composite measures that directly reflect the strength of a team. Win percentage was retained in spite of falling into this category given it's intuitive interpretation."),
    h3("Regularized Regression"),
    p("As stated above, this analysis examines three different regularized regression techniques to identify the most leveraged metrics in predicting game outcomes. The LASSO regression identified win percentage, offensive efficiency, defensive efficiency, and defensive rebounding as the most critical factors in determining wins. The ridge regression retained all metrics as significant in determining wins, with win percentage, offensive efficiency, and defensive efficiency as the most heavily weighted predictors. The elastic net regression represents a blended approach of LASSO and ridge regression techniques. I assessed different weights between LASSO and ridge regression and found a 50/50 split to be the most accurate in predicting game outcomes. After cross validating each technique to determine the optimal value of lambda, I decided to use a slightly higher lambda value which generated predictions that were only one standard error worse. Though less parsimonious in explaining the observed outcomes, the simplicity of this model compared to the optimal lambda model bodes well for predicting new data. Although LASSO regression generated the most accurate predictions by a tiny margin, all three models produced similar results in terms of accuracy."),
    h3("Random Forest"),
    p("The analysis first generates a classification tree using the same metrics from regularized regression. The classification tree found the splits which give the greatest improvement in predictions, which were win percentage and defensive efficiency. Perhaps defense does win championships? I then extend the analysis to generate a random forest, creating a large set of trees and combining the predictions from these trees to make the best possible predictions of the outcome. This model generates predictions based on an ensemble of 200 trees, with 4 trees at each split. As with ridge regression and elastic nets, random forests are good for prediction but not too much else. This contrasts with random trees and LASSO models, which can be used to make sense of data as well as make predictions. As expected, the random forest model was much more accurate, predicting game outcomes with 75% accuracy compared to the random tree model's 64% accuracy.")
  ),
  tabPanel(
    "Results",
    tabsetPanel(
      tabPanel(
        "Findings",
        br(),
        p("Overall, the random forest model provided the most accurate results, correctly predicting 75% of game outcomes. The regularized regression models were all much less accurate at 59%. Tournament data was then replicated to generate all potential matchups within the 64-team tournament. Using the predictors from the regression models, I predict outcomes for all games between a team and the other 63 competitors and sum the win total as a composite of team strength. Teams with a max value of 63 were predicted to win all games against tournament opponents, while team with a value of 0 were predicted to lose all games against tournamnet opponents. The regression models generated identical win counts given their similarities in accuracy, but deviated significantly from the random forest model. The following historic bracket tab maps out tournament outcomes on a predicted and actual basis, helping users visualize counterfactual scenarios where the better team as defined by the given measures did not win the game.")
      ),
      tabPanel(
        "Rankings",
        h3("Predicted Win Total (Out of 63 Games)"),
        selectInput(
          "year",
          "Year",
          2015:2019
        ),
        tableOutput(
          "rankings"
        )
      ),
      tabPanel(
        "Round of 64",
        sidebarPanel(
          selectInput(
            "result64",
            "Result",
            c("Actual", "Predicted")
          ),
          selectInput(
            "year64",
            "Year",
            2015:2019
          ),
          selectInput(
            "region64",
            "Region",
            c("East", "South", "West", "Midwest")
          )
        ),
        mainPanel(
          plotOutput(
            "R64"
          )
        )
      ),
      tabPanel(
        "Round of 32",
        sidebarPanel(
          selectInput(
            "result32",
            "Result",
            c("Actual", "Predicted")
          ),
          selectInput(
            "year32",
            "Year",
            2015:2019
          ),
          selectInput(
            "region32",
            "Region",
            c("East", "South", "West", "Midwest")
          )
        ),
        mainPanel(
          plotOutput(
            "R32"
          )
        )
      ),
      tabPanel(
        "Sweet 16",
        sidebarPanel(
          selectInput(
            "result16",
            "Result",
            c("Actual", "Predicted")
          ),
          selectInput(
            "year16",
            "Year",
            2015:2019
          ),
          selectInput(
            "region16",
            "Region",
            c("East", "South", "West", "Midwest")
          )
        ),
        mainPanel(
          plotOutput(
            "S16"
          )
        )
      ),
      tabPanel(
        "Elite 8",
        sidebarPanel(
          selectInput(
            "result8",
            "Result",
            c("Actual", "Predicted")
          ),
          selectInput(
            "year8",
            "Year",
            2015:2019
          ),
          selectInput(
            "region8",
            "Region",
            c("East", "South", "West", "Midwest")
          )
        ),
        mainPanel(
          plotOutput(
            "E8"
          )
        )
      ),
      tabPanel(
        "Final 4",
        sidebarPanel(
          selectInput(
            "result4",
            "Result",
            c("Actual", "Predicted")
          ),
          selectInput(
            "year4",
            "Year",
            2015:2019
          )
        ),
        mainPanel(
          plotOutput(
            "final4"
          )
        )
      ),
      tabPanel(
        "Championship",
        sidebarPanel(
          selectInput(
            "result2",
            "Result",
            c("Actual", "Predicted")
          ),
          selectInput(
            "year2",
            "Year",
            2015:2019
          )
        ),
        mainPanel(
          plotOutput(
            "championship"
          )
        )
      )
    )
  ),
  tabPanel(
    "2020 Tournament"
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

  output$rankings <- renderTable(
    win_composite %>%
      left_join(season_all, by = c("team", "year")) %>%
      mutate(postseason = factor(postseason, levels = c("R64", "R32", "S16", "E8", "F4", "2ND", "Champions"), labels = c("Round of 64", "Round of 32", "Sweet 16", "Elite 8", "Final 4", "Runner-Up", "Champion"))) %>%
      filter(year == input$year) %>%
      arrange(desc(forest)) %>% 
      select(Team = team, "Random Forest" = forest, "LASSO" = lasso, "Ridge" = ridge, "Elastic Net" = net, "Postseason Outcome" = postseason, "Seed" = seed) %>%
      gt()
  )

  output$R64 <- renderPlot(
    if (input$result64 == "Actual") {
      bracket %>%
        filter(year == input$year64, reg_name == input$region64, round == 1) %>%
        ggplot(aes(seed, forest)) +
        geom_col(aes(fill = win_actual)) +
        scale_fill_manual(name = "Outcome", labels = c("Loss", "Win"), values = c("coral1", "chartreuse1")) +
        labs(x = "Seed", y = "Projected Wins") +
        theme(axis.text.x = element_text(angle = 90)) +
        theme_fivethirtyeight() +
        geom_text(aes(label = team), size = 2) +
        coord_flip()
    }
    else if (input$result64 == "Predicted") {
      bracket %>%
        filter(year == input$year64, reg_name == input$region64, round == 1) %>%
        ggplot(aes(seed, forest)) +
        geom_col(aes(fill = win_pred)) +
        scale_fill_manual(name = "Outcome", labels = c("Loss", "Win"), values = c("coral1", "chartreuse1")) +
        labs(x = "Seed", y = "Projected Wins") +
        theme(axis.text.x = element_text(angle = 90)) +
        theme_fivethirtyeight() +
        geom_text(aes(label = team), size = 2) +
        coord_flip()
    }
  )

  output$R32 <- renderPlot(
    if (input$result32 == "Actual") {
      bracket %>%
        filter(year == input$year32, reg_name == input$region32, round == 2) %>%
        ggplot(aes(seed, forest)) +
        geom_col(aes(fill = win_actual)) +
        scale_fill_manual(name = "Outcome", labels = c("Loss", "Win"), values = c("coral1", "chartreuse1")) +
        labs(x = "Seed", y = "Projected Wins") +
        theme(axis.text.x = element_text(angle = 90)) +
        theme_fivethirtyeight() +
        geom_text(aes(label = team), size = 3, hjust = 2) +
        coord_flip()
    }
    else if (input$result32 == "Predicted") {
      r32 %>% 
        filter(year == input$year32, reg_name == input$region32) %>% 
        ggplot(aes(seed, forest)) +
        geom_col(aes(fill = win_pred)) +
        scale_fill_manual(name = "Outcome", labels = c("Loss", "Win"), values = c("coral1", "chartreuse1")) +
        labs(x = "Seed", y = "Projected Wins") +
        theme(axis.text.x = element_text(angle = 90)) +
        theme_fivethirtyeight() +
        geom_text(aes(label = team), size = 3, hjust = 2) +
        coord_flip()
    }
  )

  output$S16 <- renderPlot(
    if (input$result16 == "Actual") {
      bracket %>%
        filter(year == input$year16, reg_name == input$region16, round == 3) %>%
        ggplot(aes(seed, forest)) +
        geom_col(aes(fill = win_actual)) +
        scale_fill_manual(name = "Outcome", labels = c("Loss", "Win"), values = c("coral1", "chartreuse1")) +
        labs(x = "Seed", y = "Projected Wins") +
        theme(axis.text.x = element_text(angle = 90)) +
        theme_fivethirtyeight() +
        geom_text(aes(label = team), size = 4, hjust = 2) +
        coord_flip()
    }
    else if (input$result16 == "Predicted") {
      s16 %>% 
        filter(year == input$year16, reg_name == input$region16) %>% 
        ggplot(aes(seed, forest)) +
        geom_col(aes(fill = win_pred)) +
        scale_fill_manual(name = "Outcome", labels = c("Loss", "Win"), values = c("coral1", "chartreuse1")) +
        labs(x = "Seed", y = "Projected Wins") +
        theme(axis.text.x = element_text(angle = 90)) +
        theme_fivethirtyeight() +
        geom_text(aes(label = team), size = 4, hjust = 2) +
        coord_flip()
    }
  )

  output$E8 <- renderPlot(
    if (input$result8 == "Actual") {
      bracket %>%
        filter(year == input$year8, reg_name == input$region8, round == 4) %>%
        ggplot(aes(seed, forest)) +
        geom_col(aes(fill = win_actual)) +
        scale_fill_manual(name = "Outcome", labels = c("Loss", "Win"), values = c("coral1", "chartreuse1")) +
        labs(x = "Seed", y = "Projected Wins") +
        theme(axis.text.x = element_text(angle = 90)) +
        theme_fivethirtyeight() +
        geom_text(aes(label = team), size = 5, hjust = 2) +
        coord_flip()
    }
    else if (input$result8 == "Predicted") {
      e8 %>% 
        filter(year == input$year8, reg_name == input$region8) %>% 
        ggplot(aes(seed, forest)) +
        geom_col(aes(fill = win_pred)) +
        scale_fill_manual(name = "Outcome", labels = c("Loss", "Win"), values = c("coral1", "chartreuse1")) +
        labs(x = "Seed", y = "Projected Wins") +
        theme(axis.text.x = element_text(angle = 90)) +
        theme_fivethirtyeight() +
        geom_text(aes(label = team), size = 5, hjust = 2) +
        coord_flip()
    }
  )

  output$final4 <- renderPlot(
    if (input$result4 == "Actual") {
      final_four %>%
        filter(year == input$year4) %>%
        ggplot(aes(fct_reorder(team, region_number), forest)) +
        geom_col(aes(fill = as.factor(win_actual)), position = "dodge") +
        scale_fill_manual(name = "Outcome", labels = c("Loss", "Win"), values = c("coral1", "chartreuse1")) +
        labs(x = "Seed", y = "Projected Wins") +
        theme(axis.text.x = element_text(angle = 90)) +
        theme_fivethirtyeight() +
        geom_text(aes(label = region), size = 4, hjust = 2) +
        coord_flip()
    }
    else if (input$result4 == "Predicted"){
      f4 %>% 
        filter(year == input$year4) %>% 
        ggplot(aes(fct_reorder(team, reg_num), forest)) +
        geom_col(aes(fill = win_pred)) +
        scale_fill_manual(name = "Outcome", labels = c("Loss", "Win"), values = c("coral1", "chartreuse1")) +
        labs(x = "Seed", y = "Projected Wins") +
        theme(axis.text.x = element_text(angle = 90)) +
        theme_fivethirtyeight() +
        geom_text(aes(label = reg_name), size = 4, hjust = 2) +
        coord_flip()
    }
  )

  output$championship <- renderPlot(
    if (input$result2 == "Actual") {
      bracket %>%
        filter(year == input$year2, round == 6) %>%
        mutate(win_actual = factor(win_actual, levels = c(0,1), labels = c("Runner-Up", "Champion"))) %>% 
        ggplot(aes(team, forest)) +
        geom_col(aes(fill = win_actual)) +
        scale_fill_manual(name = "Outcome", values = c("coral1", "chartreuse1")) +
        labs(x = "Seed", y = "Projected Wins") +
        theme(axis.text.x = element_text(angle = 90)) +
        theme_fivethirtyeight() +
        geom_text(aes(label = win_actual), size = 5, hjust = 2) +
        coord_flip()
    }
    else if (input$result2 == "Predicted"){
      championship %>% 
        filter(year == input$year2) %>% 
        mutate(win_pred = factor(win_pred, levels = c(0,1), labels = c("Runner-Up", "Champion"))) %>% 
        ggplot(aes(team, forest)) +
        geom_col(aes(fill = win_pred)) +
        scale_fill_manual(name = "Outcome", values = c("coral1", "chartreuse1")) +
        labs(x = "Seed", y = "Projected Wins") +
        theme(axis.text.x = element_text(angle = 90)) +
        theme_fivethirtyeight() +
        geom_text(aes(label = win_pred), size = 5, hjust = 2) +
        coord_flip()
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
