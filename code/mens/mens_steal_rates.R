##### STEAL RATES #######

library(hoopR)
library(tidyverse)

# Load in data
data <- data.frame()

for (i in 2011:2022)
{
  data <- data %>%
    bind_rows(load_mbb_team_box(i) %>% mutate(year = i))
}

# Only keep useful columns for steals
data <- data %>%
  select(team = team_location, game_id, game_date, turnovers, steals, field_goals_made_field_goals_attempted:fouls, year)

# Separate all field goal splits so that there are columns for makes and misses
data <- data %>%
  separate(field_goals_made_field_goals_attempted, into = c("fgm", "fga"), sep = "-") %>%
  separate(three_point_field_goals_made_three_point_field_goals_attempted, into = c("fgm3", "fga3"), sep = "-") %>%
  separate(free_throws_made_free_throws_attempted, into = c("ftm", "fta"), sep = "-") %>%
  mutate_at(.vars = vars(fgm:fta, turnovers, steals, offensive_rebounds),
            .funs = as.numeric)

# Going to use kenpom's approximation for possessions listed here: https://kenpom.com/blog/the-possession/#:~:text=The%20most%20common%20formula%20for,and%20FTA%20%3D%20free%20throw%20attempts.

# Find the number of field goal attempts
data <- data %>%
  mutate(fgm2 = fgm - fgm3,
         fga2 = fga - fga3,
         points = (ftm) + (2 * fgm2) + (3 * fgm3))

# Add opponents stats
data <- data %>%
  inner_join(data, suffix = c("", "_opp"), by = "game_id")

# filter out observations where the opponent is the team in question
data <- data %>%
  filter(team != team_opp)

# add number of possessions and calculate season steal rates by game
data <- data %>%
  mutate(possessions = round((fga - offensive_rebounds) + turnovers + (0.44 * fta)),
         possessions_opp = round((fga_opp - offensive_rebounds_opp) + turnovers_opp + (0.44 * fta_opp))) %>%
  group_by(team, year) %>%
  mutate(total_steals = cumsum(steals),
         total_steals_opp = cumsum(steals_opp),
         total_possessions = cumsum(possessions),
         total_possessions_opp = cumsum(possessions_opp),
         steal_rate = total_steals / total_possessions,
         opp_steal_rate = total_steals_opp / total_possessions_opp) %>%
  ungroup()


# Keep useful columns
data <- data %>%
  select(team, game_id, steal_rate, opp_steal_rate)

# Write to CSV
write_csv(data, "../../data/mens/mens_team_steal_rates.csv")