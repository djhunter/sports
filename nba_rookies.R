library(tidyverse)

# The folder data/nba contains CSV files nbaYYYY.csv that were 
# downloaded from basketball-reference.com
years <- 1978:2023
season_stats <- list()

i <- 1
for(year in years) {
  season_stats[[i]] <- read_csv(paste0("data/nba/nba", year, ".csv")) %>%
    bind_cols(Year = year) %>%
    select(-Rk)
  i <- i + 1
}

all_seasons <- bind_rows(season_stats) %>%
  rename(PlayerID = `Player-additional`)

all_seasons %>% group_by(PlayerID) %>%
  mutate(numYears = length(unique(Year)))%>%
  ungroup()

vets <- unique(all_seasons[all_seasons$Year < 1980,]$PlayerID)
## Eliminate players who were rookies before 1979-80 season
## and after 2016-17 season
rookies <- all_seasons %>% 
  filter(!(PlayerID %in% vets)) %>%
  group_by(PlayerID) %>%
  mutate(numYears = length(unique(Year)),
         rookieYear = min(Year)) %>%
  ungroup() %>%
  filter(Year == rookieYear,
         Year < 2018) %>% 
  mutate(TARGET_5Yrs = (numYears > 4)) %>%
  select(-rookieYear, -numYears) %>%
  filter(Tm != "TOT") %>% # recompute totals for players with multiple teams 
  group_by(PlayerID) %>%
  summarize(Player = Player[1], Pos = Pos[1], Age = Age[1], 
            Tm = paste(Tm, collapse = "/"), Year = Year[1],
            G = sum(G), GS = sum(GS), MP = sum(MP), PTS = sum(PTS), 
            FG = sum(FG), FGA = sum(FGA), `FG%` = FG/FGA, 
            `3P` = sum(`3P`), `3PA` = sum(`3PA`), `3P%` = `3P`/`3PA`,
            `2P` = sum(`2P`), `2PA` = sum(`2PA`), `2P%` = `2P`/`2PA`, 
            `eFG%` = (FG + 0.5 * `3P`)/FGA, 
            FT = sum(FT), FTA = sum(FTA), `FT%` = FT/FTA, 
            ORB = sum(ORB), DRB = sum(DRB), TRB = sum(TRB), AST = sum(AST), 
            STL = sum(STL), BLK = sum(BLK), TOV = sum(TOV), PF = sum(PF),
            TARGET_5Yrs = TARGET_5Yrs[1]
  )
write_csv(rookies, "data/nba/nba_rookies.csv")

