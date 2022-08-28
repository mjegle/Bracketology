##### Possessions per game #######


library(hoopR)
library(tidyverse)

data <- data.frame()
schedule <- data.frame()

test <- load_mbb_schedule(2022)

for (i in 2011:2022)
{
  data <- data %>%
    bind_rows(load_mbb_team_box(i) %>% mutate(year = i))
  
  schedule <- schedule %>%
    bind_rows(load_mbb_schedule(i) %>%
                select(id, home.location, away.location, neutralSite, conferenceCompetition, notes_headline, tournamentId,
                       status.type.detail) %>%
                mutate(id = as.integer(id)))
}

home <- schedule %>%
  select(id, team = home.location, neutralSite, conferenceCompetition, game_result = status.type.detail) %>%
  mutate(home = T)

away <- schedule %>%
  select(id, team = away.location, neutralSite, conferenceCompetition, game_result = status.type.detail) %>%
  mutate(home = F)

schedule <- home %>%
  bind_rows(away)

data <- data %>%
  select(team = team_location, game_id, game_date, turnovers, steals, field_goals_made_field_goals_attempted:fouls, year)

data <- data %>%
  separate(field_goals_made_field_goals_attempted, into = c("fgm", "fga"), sep = "-") %>%
  separate(three_point_field_goals_made_three_point_field_goals_attempted, into = c("fgm3", "fga3"), sep = "-") %>%
  separate(free_throws_made_free_throws_attempted, into = c("ftm", "fta"), sep = "-") %>%
  mutate_at(.vars = vars(fgm:fta, turnovers, steals, offensive_rebounds),
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
  mutate(possessions = round((fga - offensive_rebounds) + turnovers + (0.44 * fta)),
         possessions_opp = round((fga_opp - offensive_rebounds_opp) + turnovers_opp + (0.44 * fta_opp))) %>%
  select(game_id, team, possessions, year)

# Want it to be possessions per 40 minutes

data <- data %>%
  inner_join(schedule, by = c("team" = "team", "game_id" = "id"))

data <- data %>%
  mutate(minutes = case_when(game_result == "Final" ~ 40,
                             game_result == "Final/OT" ~ 45,
                             game_result == "Final/2OT" ~ 50,
                             game_result == "Final/3OT" ~ 55,
                             game_result == "Final/4OT" ~ 60,
                             game_result == "Final/5OT" ~ 65,
                             game_result == "Final/6OT" ~ 70,
                             game_result == "Final/7OT" ~ 75,
                             game_result == "Final/8OT" ~ 80)) # Just in case

data <- data %>%
  group_by(team, year) %>%
  mutate(total_minutes = cumsum(minutes),
         total_possessions = cumsum(possessions),
         pace = total_possessions / total_minutes * 40)

data <- data %>%
  ungroup() %>%
  select(game_id, team, pace)

write_csv(data, "../../data/mens/mens_team_pace.csv")

