#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(mlbgameday)
library(doParallel)
library(DBI)
library(RSQLite)
library(stringr)
library(plotly) 

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)  
registerDoParallel(cl)

player_id_list <- read_csv("playerid_list.csv")

ids18 <- search_gids(team = "twins", start = "2018-03-28", end = "2018-04-30")
ids19 <- make_gids(start = "2019-03-28", end = "2019-04-30") %>%
  str_subset("minmlb") %>%
  str_sub(66, 95)

batdat18 <- get_payload(game_ids = ids18, dataset = "inning_hit")
batdat19 <- get_payload(game_ids = ids19, dataset = "inning_hit")
gamedat18 <- get_payload(game_ids = ids18)
gamedat19 <- get_payload(game_ids = ids19)


pitches18 <- inner_join(gamedat18$pitch, gamedat18$atbat, by = c("num", "url")) %>%
  subset(pitcher_name == "Kyle Gibson" | pitcher_name == "Jake Odorizzi" | pitcher_name == "Jose Berrios") %>%
  mutate(year = "2018")
pitches19 <- inner_join(gamedat19$pitch, gamedat19$atbat, by = c("num", "url")) %>%
  subset(pitcher_name == "Kyle Gibson" | pitcher_name == "Jake Odorizzi" | pitcher_name == "Jose Berrios") %>%
  mutate(year = "2019")
pitches <- merge(pitches18, pitches19, all =TRUE)

pitchTypes <- pitches %>%
  group_by(pitcher_name, year) %>%
  count(pitch_type) %>%
  drop_na()
pitchResult <- pitches %>%
  group_by(pitcher_name, year) %>%
  count(type) %>%
  drop_na()


bat18 <- batdat18 %>%
  rename(MLBCODE = batter) %>%
  inner_join(player_id_list, by = "MLBCODE") %>%
  mutate(batter_name = paste0(FIRSTNAME, " ", LASTNAME)) %>%
  subset(batter_name == "Eddie Rosario" | batter_name == "Max Kepler") %>%
  mutate(year = "2018")
bat19 <- batdat19 %>%
  rename(MLBCODE = batter) %>%
  inner_join(player_id_list, by = "MLBCODE") %>%
  mutate(batter_name = paste0(FIRSTNAME, " ", LASTNAME)) %>%
  subset(batter_name == "Eddie Rosario" | batter_name == "Max Kepler") %>%
  mutate(year = "2019")
batting <- merge(bat18, bat19, all =TRUE)

batTypes <- batting %>%
  group_by(batter_name, year) %>%
  count(des) %>%
  drop_na()




ui <- fluidPage(
   
   # Application title
   titlePanel("Twins Batting and Pitching"),
   mainPanel(
      tabsetPanel(
        tabPanel("Pitching", plotlyOutput(outputId = "pitching1"),
                 p("This first plot is of pitches by Kyle Gibson, Jake Odorizzi, and Jose Berrios. It's hard to find
                   any statistically significant differences between 2018 and 2019 from this. However, there are
                   several interesting observations. One of these being that Jake Odorizzi stopped using a slider in
                   2019."),
                 plotlyOutput(outputId = "pitching2"),
                 p("This shows similar results to the previous graph, but in a different format. It's easier to see how
                   each pitcher is dividing up their pitches. Jake Odorizzi stopped using the slider in 2019 and started
                   throwing a lot more cutters."),
                 plotlyOutput(outputId = "pitching3"),
                 p("This last pitching chart shows each pitcher's divison of ball, strikes, other.")),
        tabPanel("Batting", plotlyOutput(outputId = "batting1"), 
                 p("This graph shows the location for each hit ball for Eddie Rosario and Max Kepler for the first
                   month of the 2018 and 2019 season. Triangles represent hits and squares are outs."),
                 plotlyOutput(outputId = "batting2"),
                 p("This bar chart is a reformating of the batting scatterplot. It shows how Eddie Rosario and Max Kepler
                   divide their hits. Rosario appears to have been much more effective in 2019. He has four times as many
                   home runs this year. This would have a strong impact of the Twins ability to win games.")),
        tabPanel("About", p("This Shiny app was created to explore how the Minnesota Twins baseball team improved from
                            the first month of the 2018 season to the first month of the 2019 season. Our main goal
                            was to determine what changed with respect to Minnesota's best batters and pitchers."),
                 p("The data was scraped from the MLBAM Gameday data and analyized using the following packages"),
                 p("- tidyverse"),
                 p("- mlbgameday"),
                 p("- doParallel"),
                 p("- stringr"),
                 p("- plotly"),
                 p("The source code for this Shiny app can be found at: ")))
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  
  output$pitching1 <- renderPlotly({
    p1 <- ggplot() +
      geom_point(data=pitches, aes(x=px, y=pz, shape=type, col=pitch_type)) +
      facet_grid(pitcher_name ~ year)+ coord_equal() + 
      geom_path(aes(x, y), data = mlbgameday::kzone)
    ggplotly(p1)
   })
   
   
   output$pitching2 <- renderPlotly({
     p2 <- ggplot() +
       geom_col(data=pitchTypes, aes(x = pitcher_name, y = n, fill = pitch_type)) +
       facet_grid(. ~ year)
     ggplotly(p2)
   })
   
   output$pitching3 <- renderPlotly({
     p3 <- ggplot() +
       geom_col(data=pitchResult, aes(x = pitcher_name, y = n, fill = type)) +
       facet_grid(. ~ year)
     ggplotly(p3)
   })
   
   output$batting1 <- renderPlotly({
     p4 <- ggplot() +
       geom_point(data = batting, aes(x = x, y = y, shape = type, color = des)) +
       facet_grid(batter_name ~ year)
     ggplotly(p4)
   })
   
   output$batting2 <- renderPlotly({
     p5 <- ggplot() +
       geom_col(data=batTypes, aes(x = batter_name, y = n, fill = des)) +
       facet_grid(. ~ year)
     ggplotly(p5)
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

