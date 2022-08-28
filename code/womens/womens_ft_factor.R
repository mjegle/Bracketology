####### FT Factor ########

library(tidyverse)
library(wehoop)

data <- data.frame()

for (i in 2014:2022)
{
  data <- data %>%
    bind_rows(load_wbb_team_box(i) %>% mutate(year = i))
}

data <- data %>%
  select(team = team_location, game_id, game_date, field_goals_made_field_goals_attempted:fouls, year)

data <- data %>%
  separate(field_goals_made_field_goals_attempted, into = c("fgm", "fga"), sep = "-") %>%
  separate(three_point_field_goals_made_three_point_field_goals_attempted, into = c("fgm3", "fga3"), sep = "-") %>%
  separate(free_throws_made_free_throws_attempted, into = c("ftm", "fta"), sep = "-") %>%
  mutate_at(.vars = vars(fgm:fta, turnovers, offensive_rebounds),
            .funs = as.numeric)

# Going to use kenpom's approximation for possessions listed here: https://kenpom.com/blog/the-possession/#:~:text=The%20most%20common%20formula%20for,and%20FTA%20%3D%20free%20throw%20attempts.



data <- data %>%
  mutate(fgm2 = fgm - fgm3,
         fga2 = fga - fga3,
         points = (ftm) + (2 * fgm2) + (3 * fgm3))

data <- data %>%
  inner_join(data, suffix = c("", "_opp"), by = "game_id")

data <- data %>%
  filter(team != team_opp)

data <- data %>%
  group_by(team, year) %>%
  mutate(ftf = cumsum(ftm) / cumsum(fga),
         ftf_opp = cumsum(ftm_opp) / cumsum(fga_opp)) %>%
  ungroup() %>%
  select(team, game_id, ftf, ftf_opp)

write_csv(data, "../../data/womens/womens_ft_factor.csv")

