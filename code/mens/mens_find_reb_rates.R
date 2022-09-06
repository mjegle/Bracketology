####### Find rebounding rates for NCAA mens basketball ######

library(hoopR)
library(tidyverse)

# REBOUNDING RATES #
data <- data.frame()

# Load in data
for (i in 2011:2022)
{
  data <- data %>%
    bind_rows(load_mbb_team_box(i) %>% mutate(year = i))
}

# Only use necessary columns
data <- data %>%
  select(team = team_location, game_id, game_date, field_goal_pct:fouls, year)

# join in the opponents data since there are two observations per game, one for each team
data <- data %>%
  inner_join(data, suffix = c("", "_opp"), by = "game_id")

# filter out observations where a team is joined with itself
data <- data %>%
  filter(team != team_opp)

# calculate rebounding rates
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

# save only useful data
reb <- reb %>%
  select(team, game_date, game_id, year, num_game, cumul_o_reb, cumul_d_reb, cumul_o_reb_opp, cumul_d_reb_opp, o_reb_rate, d_reb_rate)

# write as a csv
write_csv(reb, "../../data/mens/mens_reb_rates.csv")


