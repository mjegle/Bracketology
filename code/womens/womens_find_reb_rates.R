####### Find rebounding rates for NCAA mens basketball ######

library(wehoop)
library(tidyverse)


# REBOUNDING RATES #
data <- data.frame()

for (i in 2014:2022)
{
  data <- data %>%
    bind_rows(load_wbb_team_box(i) %>% mutate(year = i))
}

data <- data %>%
  select(team = team_location, game_id, game_date, field_goal_pct:fouls, year)

data <- data %>%
  inner_join(data, suffix = c("", "_opp"), by = "game_id")

data <- data %>%
  filter(team != team_opp)

data %>%
  arrange(game_date) %>%
  mutate(offensive_rebounds = as.numeric(offensive_rebounds),
         defensive_rebounds = as.numeric(defensive_rebounds),
         offensive_rebounds_opp = as.numeric(offensive_rebounds_opp),
         defensive_rebounds_opp = as.numeric(defensive_rebounds_opp)) %>%
  group_by(team, year) %>%
  mutate(cumul_o_reb = cumsum(offensive_rebounds),
         cumul_d_reb = cumsum(defensive_rebounds),
         cumul_o_reb_opp = cumsum(offensive_rebounds_opp),
         cumul_d_reb_opp = cumsum(defensive_rebounds_opp),
         o_reb_rate = cumul_o_reb / (cumul_o_reb + cumul_d_reb_opp),
         d_reb_rate = cumul_d_reb / (cumul_d_reb + cumul_o_reb_opp),
         num_game = 1:n()) -> reb

reb <- reb %>%
  select(team, game_date, game_id, year, num_game, cumul_o_reb, cumul_d_reb, cumul_o_reb_opp, cumul_d_reb_opp, o_reb_rate, d_reb_rate)

write_csv(reb, "../../data/womens/womens_reb_rates.csv")


