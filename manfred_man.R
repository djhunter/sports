library(baseballr)
library(tidyverse)

infolist <- readRDS("data/mlb/mlbinfo1723.rds")
linescorelist <- readRDS("data/mlb/mlblinescore1723.rds")

infoDF <- bind_rows(infolist)
linescoreDF <- bind_rows(linescorelist)

################################
## top up data with most recent games of current season
current_season <- mlb_schedule(season = 2023) %>%
  filter(series_description == "Regular Season") 
current_season %>% 
  filter(status_detailed_state == "Final") %>%
  select(game_pk) %>%
  filter(!(game_pk %in% infoDF$game_pk)) %>%
  as_vector() %>%
  unique() -> new_game_pk
new_infolist <- map(new_game_pk, mlb_game_info)
infoDF <- infoDF %>%
  bind_rows(new_infolist)
new_linescorelist <- map(new_game_pk, mlb_game_linescore)
linescoreDF <- linescoreDF %>%
  bind_rows(new_linescorelist)
################################
years <- c(2017, 2018, 2019, 2021, 2022, 2023)
seasons <- map(years, mlb_schedule) %>%
  bind_rows() %>%
  filter(series_description == "Regular Season") 

game_innings <- linescoreDF %>%
  group_by(game_pk) %>%
  summarize(num_innings = n()) 

## This will give game times 
# mlb_game_pace(season = 2021, sport_ids = 1) %>% view()

game_data <- game_innings %>%
  left_join(seasons, by = "game_pk") %>%
  filter(scheduled_innings >= 9) %>%
  mutate(`Game Length` = if_else(num_innings == 9, "Regulation", "Extra Innings")) %>%
  mutate(`Automatic Runner` = (year(date) >= 2020)) %>%
  mutate(last_game_in_series = (series_game_number == games_in_series)) %>%
  left_join(infoDF, by = "game_pk") %>% 
  mutate(duration = hm(str_sub(elapsed_time, 1, 5)))

game_data %>%
  filter(`Game Length` == "Extra Innings") %>%
  ggplot(aes(x = duration, fill = `Automatic Runner`)) +
  geom_density(alpha = 0.5) +
  scale_x_time()

game_data %>%
  filter(`Game Length` == "Extra Innings") %>%
  ggplot(aes(x = num_innings, fill = `Automatic Runner`)) +
  geom_histogram(binwidth = 1, position = "dodge") 

game_data %>%
  filter(`Game Length` == "Extra Innings") %>%
  filter(!is.na(teams_home_is_winner)) %>%
  ggplot(aes(fill = teams_home_is_winner, x = `Automatic Runner`)) +
  geom_bar(position = "fill") 

# For extra inning games, the home team winning percentage has actually
# been lower since the automatic runner has been instituted.
game_data_extra <- game_data %>% 
  filter(`Game Length` == "Extra Innings") %>%
  filter(!is.na(teams_home_is_winner)) %>%
  select(`Automatic Runner`, teams_home_is_winner)
table(game_data_extra)
prop.table(table(game_data_extra), margin = 1)
prop.test(table(game_data_extra))
# However, the difference is not significant.
game_data_extra <- game_data %>% 
  filter(`Game Length` == "Extra Innings") %>%
  filter(!is.na(teams_home_is_winner)) %>%
  select(`Automatic Runner`, teams_home_is_winner)
table(game_data_extra)
prop.table(table(game_data_extra), margin = 1)
prop.test(table(game_data_extra))

# For all games, the home team is less likely to win if the game
# goes into extra innings. (50% vs 53%)
chisq.test(game_data$`Game Length`, game_data$teams_home_is_winner)
summary(glm(teams_home_is_winner ~ `Game Length` + `Automatic Runner`, 
            data = game_data, family = "binomial"))
winnerextra <- game_data %>%
  select(`Game Length`, teams_home_is_winner) 
prop.test(table(winnerextra))
table(winnerextra)
prop.table(table(winnerextra), margin = 1)
prop.test(table(winnerextra))

library(vcd)
game_data_simple <- game_data %>%
  transmute(`Winning Team` = ifelse(teams_home_is_winner, "Home", "Away"),
            `Automatic Runner` = ifelse(`Automatic Runner`, "Y", "N"),
            `Game Length` = `Game Length`)
   
xrtable <- structable(`Winning Team` ~ `Game Length` + `Automatic Runner`, 
                  data = game_data_simple)
mosaic(xrtable,
       shade = TRUE)

mosaic(xrtable,
       gp = shading_Friendly2,
       legend = FALSE)
