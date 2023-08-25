library(baseballr)
library(tidyverse)

years <- c(2017, 2018, 2019, 2021, 2022, 2023)
seasons <- map(years, mlb_schedule) %>%
  bind_rows() %>%
  filter(series_description == "Regular Season") 

seasons %>% 
  filter(status_detailed_state == "Final") %>%
  select(game_pk) %>%
  as_vector() %>%
  unique() -> game_pk_v


#mlb_game_info retrieves game time
infolist <- map(game_pk_v, mlb_game_info)
## mlb_game_changes will tell if it was the last game in a series
saveRDS(infolist, "data/mlbinfo1723.rds")

linescorelist <- map(game_pk_v, mlb_game_linescore)
saveRDS(linescorelist, "data/mlblinescore1723.rds")

game_innings <- linescorelist %>%
  bind_rows() %>%
  group_by(game_pk) %>%
  summarize(num_innings <- n()) 

## This will give game times 
# mlb_game_pace(season = 2021, sport_ids = 1) %>% view()
