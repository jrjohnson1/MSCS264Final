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
library(stringr)
library(plotly)
library(fmsb)

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)  
registerDoParallel(cl)

player_id_list <- read_csv("playerid_list.csv")
pitches <- read_csv("pitchingOutput.csv")
batting <- read_csv("battingOutput.csv")
exit_velo2016 <- read_csv("exit_velocity2016.csv")
twins2018 <- read_csv("twins2018.csv")
twins2019 <- read_csv("twins2019.csv")

twins2018 <- twins2018 %>%
  mutate(player_name = paste0(player_name, " ", "(2018)"))
twins2019 <- twins2019 %>%
  mutate(player_name = paste0(player_name, " ", "(2019)"))
twinsbatting <- merge(twins2018, twins2019, all = TRUE) %>%
  select(hits, launch_speed, launch_angle, spin_rate, velocity) %>%
  mutate(hits_adj = hits/2, speed_adj = launch_speed/9, spin_adj = spin_rate/150, veloc_adj = velocity/5 ) %>%
  select(hits_adj, speed_adj, spin_adj, veloc_adj, launch_angle)

pitchTypes <- pitches %>%
  group_by(pitcher_name, year) %>%
  count(pitch_type) %>%
  drop_na()
pitchResult <- pitches %>%
  group_by(pitcher_name, year) %>%
  count(type) %>%
  drop_na()
batTypes <- batting %>%
  group_by(batter_name, year) %>%
  count(des) %>%
  drop_na()



lahman2016 <- Lahman::Batting %>%
  filter(yearID == "2016", AB > 24)
lahman_names <- Lahman::Master %>%
  select(playerID, nameFirst, nameLast)
lahman2016_w_names <- inner_join(lahman_names, lahman2016, by = "playerID")
exit_velo2016 <- unite_(exit_velo2016, "Name", c("first_name","last_name"), sep = " ")
batting2016 <- unite_(lahman2016_w_names, "Name", c("nameFirst","nameLast"), sep = " ")
bastats1 <- inner_join(exit_velo2016, batting2016, by = "Name") %>%
  select(Name, player_id, attempts, avg_hit_angle, anglesweetspotpercent, max_hit_speed, avg_hit_speed, max_distance, 
         avg_distance, avg_hr_distance, ev95plus, ev95percent, barrels, brl_percent, brl_pa, teamID, lgID, G, AB, R, H, 
         X2B, X3B, HR, RBI, SO, GIDP) %>%
  filter(attempts > 100) %>%
  arrange(desc(H)) %>%
  rename("Team" = teamID,
         "League" = lgID)
outlierHR <- bastats1 %>%
  filter(HR > 39)
outlierH <- bastats1 %>%
  filter(H > 190)



radar_setup_h <- bastats1 %>%
  select(Name, H, HR, avg_hit_angle, max_hit_speed, anglesweetspotpercent) %>%
  arrange(desc(H)) %>%
  slice(1:8) %>%
  mutate(h_adjusted = H/15, hr_adjusted = HR/2, max_velo_adj = max_hit_speed/9, assp_adj = anglesweetspotpercent/3 ) %>%
  select(h_adjusted, hr_adjusted, max_velo_adj, avg_hit_angle, assp_adj)
radar_setup_hr <- bastats1 %>%
  select(Name, H, HR, avg_hit_angle, max_hit_speed, anglesweetspotpercent) %>%
  arrange(desc(HR)) %>%
  slice(1:8) %>%
  mutate(h_adjusted = H/15, hr_adjusted = HR/2, max_velo_adj = max_hit_speed/9, assp_adj = anglesweetspotpercent/3 ) %>%
  select(h_adjusted, hr_adjusted, max_velo_adj, avg_hit_angle, assp_adj)


radarchart2 <- function(data,varlabs=NULL,grplabs=NULL,colors=1:nrow(data),axislim=NULL,fill=T,title="")
{
  makeTransparent<-function(someColor, alpha=50)
  {
    newColor<-col2rgb(someColor)
    apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                                blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
  }
  if(!is.null(varlabs)) { 
    if(length(varlabs) != ncol(data)) {stop("varlabs must have same length as ncol(data)")} 
    names(data) <- varlabs }
  if(!is.null(grplabs)) { 
    if(length(grplabs) != nrow(data)) {stop("grplabs must have same length as ncol(data)")} 
    rownames(data) <- grplabs }
  varlabs <- colnames(data); grplabs <- rownames(data)
  maxnum <- max(data); minnum <- min(data)
  if(!is.null(axislim)) {  
    maxnum <- axislim[2]; minnum <- axislim[1]
    if( axislim[2] <= axislim[1] ) { stop("Max value must be greater than min value") }
    if(!is.numeric(axislim)) { stop("Axis limits must be numeric")}
  }
  maxnum <- round(maxnum,1);minnum <- round(minnum,1)
  temp <- rbind(rep(maxnum,ncol(data)),rep(minnum,ncol(data)),data)
  colors_border <- colors
  colors_fill <- NA
  if(fill) { colors_fill <- makeTransparent(colors_border) }
  caxislabels <- c(minnum,rep("",3),maxnum)
  radarchart( temp, maxmin=TRUE, pcol=colors_border, pfcol=colors_fill,plwd=2 , plty=1,
              cglcol="grey",cglty=1,cglwd=0.8,vlcex=0.8,
              axistype=1,axislabcol="black",caxislabels=caxislabels,pch=20,title=title)
  legend("bottomleft", legend = grplabs, bty = "n", pch=20 , col=colors_border , text.col = "black", cex=0.8, pt.cex=1,lty=1,
         lwd=2)
}



ui <- fluidPage(
   
   # Application title
   titlePanel("Twins Batting and Pitching"),
   mainPanel(
      tabsetPanel(
        tabPanel("Dataset",
                 selectInput("y", strong("Hits or Runs"), choices = c("Hits" = "H", "Homeruns" = "HR")),
                 plotlyOutput(outputId = "hits_runs"),
                 p("These plots show boxplots from each team across the league with points from individual players. 
                   This allows us to see teams that were more successful with hits and homeruns, along with their median 
                   and outlier values. We want to look into the outliers, specifically the top 8 hitters and homerun 
                   players from the league in 2016."),
                 plotOutput(outputId = "radar1"),
                 plotOutput(outputId = "radar2"),
                 p("Here are two radar charts, one showing the top hitters in the league in 2016 and the other showing the 
                   top homerun hitters' statistics. The values are adjusted to be in a similar range numerically with each other. 
                   We can compare these stats and see that launch angle can turn a base hitter into a homerun hitter and angle 
                   sweet spot percentage can add more hits to a homerun hitter's resume.")),
        tabPanel("Pitching", 
                 selectInput("select", strong("Pitch Options"), choices = c("Pitch Type" = "pitch_type",
                    "Result" = "result", "Velocity" = "velocity")),
                 plotlyOutput(outputId = "pitching1"),
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
                   home runs this year. This would have a strong impact of the Twins ability to win games."),
                 plotOutput(outputId = "batting3"),
                 p("The average launch angle greatly increased for both players in 2019. This could account for the more home runs.")),
        tabPanel("About", 
                 p("This Shiny app was created by Joe Johnson, TJ Rogers, and Matt Muller to explore how the Minnesota Twins 
                    baseball team improved from the first month of the 2018 season to the first month of the 2019 season. ]
                    Our main goal was to determine what changed with respect to Minnesota's best batters and pitchers."),
                 p("The data was scraped from the MLBAM Gameday data and baseballsavant.mlb.com. It was
                   analyized using the following packages"),
                 p("- tidyverse"),
                 p("- mlbgameday"),
                 p("- fmsb"),
                 p("- doParallel"),
                 p("- stringr"),
                 p("- plotly"),
                 p("The source code for this Shiny app can be found at: https://github.com/jrjohnson1/MSCS264Final")))
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$hits_runs <- renderPlotly({
    if(input$y == "H"){
      pH <- ggplot(bastats1, aes(x = Team, y = HR, color = League)) +
        geom_boxplot() +
        geom_text(mapping = aes(label = Name), data = outlierHR, show.legend = FALSE)
    }
    else if(input$y == "HR") {
      pHR <- ggplot(bastats1, aes(x = Team, y = H, color = League)) +
        geom_boxplot() +
        geom_text(mapping = aes(label = Name), data = outlierH, show.legend = FALSE)
    }
  })
  
  output$pitching1 <- renderPlotly({
    if (input$select == "pitch_type") {
    p1 <- ggplot() +
      geom_point(data=pitches, aes(x=px, y=pz, shape=type, col=pitch_type), alpha=0.25) +
      facet_grid(pitcher_name ~ year)+ coord_equal() + 
      geom_path(aes(x, y), data = mlbgameday::kzone) +
      ylab("Horizontal Location (ft.)") + 
      xlab("Vertical Location (ft): Catcher's View")
    ggplotly(p1)
    }
    else if (input$select == "result") {
      p1 <- ggplot() +
        geom_point(data=pitches, aes(x=px, y=pz, shape=type, col=des), alpha=0.25) +
        facet_grid(pitcher_name ~ year)+ coord_equal() + 
        geom_path(aes(x, y), data = mlbgameday::kzone) +
        ylab("Horizontal Location (ft.)") + 
        xlab("Vertical Location (ft): Catcher's View")
      ggplotly(p1)
    }
    else if (input$select == "velocity") {
      p1 <- ggplot() +
        geom_point(data=pitches, aes(x=px, y=pz, shape=type, col=start_speed), alpha=0.25) +
        facet_grid(pitcher_name ~ year)+ coord_equal() + 
        geom_path(aes(x, y), data = mlbgameday::kzone) +
        ylab("Horizontal Location (ft.)") + 
        xlab("Vertical Location (ft): Catcher's View")
      ggplotly(p1)
    }
   })
   
   output$pitching2 <- renderPlotly({
     p2 <- ggplot() +
       geom_col(data=pitchTypes, aes(x = pitcher_name, y = n, fill = pitch_type)) +
       facet_grid(. ~ year) +
       ylab("Pitches") + 
       xlab("Pitcher")
     ggplotly(p2)
   })
   
   output$pitching3 <- renderPlotly({
     p3 <- ggplot() +
       geom_col(data=pitchResult, aes(x = pitcher_name, y = n, fill = type)) +
       facet_grid(. ~ year) +
       ylab("Pitches") + 
       xlab("Pitcher")
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
       facet_grid(. ~ year) +
       ylab("Hits") + 
       xlab("Batter")
     ggplotly(p5)
   })
   
   output$batting3 <- renderPlot({
     radarchart2(twinsbatting,fill=F,varlabs=c("Hits", "Speed", "Spin Rate", "Exit Velocity","Launch Angle"),
                 axislim=c(5, 25), title="Eddie Rosario and Max Kepler \n2018 vs 2019", grplabs=c("Eddie Rosario (2018)", 
                                   "Eddie Rosario (2019)", "Max Kepler (2019)", "Max Kepler (2018)"))
   })
   
   output$radar1 <- renderPlot({
     radarchart2(radar_setup_hr,fill=F,varlabs=c("Hits","HR","Max Exit Velocity","Launch Angle","Ang SS Perc."),
                 axislim=c(5, 25),title="Average numbers by the top 8 Homerun Hitters in 2016", grplabs=c("Mark Trumbo", 
                 "Nelson Cruz", "Brian Dozier", "Edwin Encarnacion", "Khris Davis", "Nolan Arenado", "Chris Carter", "Todd Frazier"))
   })
   
   output$radar2 <- renderPlot({
     radarchart2(radar_setup_h,fill=F,varlabs=c("Hits","HR","Max Exit Velocity","Launch Angle","Ang SS Perc."),
                 axislim=c(5, 25),title="Average numbers by the top 8 Hitters in 2016", grplabs=c("Jose Altuve", "Mookie Betts", 
                     "Jean Segura", "Dustin Pedroia", "Robinson Cano", "Corey Seager", "Xander Bogaerts", "DJ Lemahieu"))
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

