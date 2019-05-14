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

player_id_list <- read_csv("~/Mscs 264 S19/Inclass/TJ-Matt-Joe Final/playerid_list.csv")

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

write.csv(pitches, "pitchingOutput.csv")
write.csv(batting, "battingOutput.csv")
