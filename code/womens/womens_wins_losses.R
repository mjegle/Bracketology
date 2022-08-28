##### Calculate Wins and Losses ######

library(tidyverse)
library(wehoop)

data <- data.frame()
schedule <- data.frame()
conf <- read_csv("../../data/womens/womens_team_conferences.csv")

# TODO - add variables for major and mid-major conference wins
for (i in 2014:2022)
{
  data <- data %>%
    bind_rows(load_wbb_team_box(i) %>% mutate(year = i))
  
  schedule <- schedule %>%
    bind_rows(load_wbb_schedule(i) %>%
                select(id, home_location, away_location, neutral_site, conference_competition, tournament_id) %>%
                mutate(id = as.integer(id)))
}

home <- schedule %>%
  select(id, team = home_location, neutral_site, conference_competition) %>%
  mutate(home = T)

away <- schedule %>%
  select(id, team = away_location, neutral_site, conference_competition) %>%
  mutate(home = F)

schedule <- home %>%
  bind_rows(away)

data <- data %>%
  select(team = team_location, game_id, game_date, field_goals_made_field_goals_attempted, three_point_field_goals_made_three_point_field_goals_attempted,
         free_throws_made_free_throws_attempted, year)

data <- data %>%
  separate(field_goals_made_field_goals_attempted, into = c("fgm", "fga"), sep = "-")

data <- data %>%
  separate(three_point_field_goals_made_three_point_field_goals_attempted, into = c("fgm3", "fga3"), sep = "-")

data <- data %>%
  separate(free_throws_made_free_throws_attempted, into = c("ftm", "fta"), sep = "-")

data <- data %>%
  mutate_at(.vars = vars(fgm:fta),
            .funs = as.numeric)

data <- data %>%
  mutate(fgm2 = fgm - fgm3,
         fga2 = fga - fga3,
         points = (ftm) + (2 * fgm2) + (3 * fgm3))

data <- data %>%
  inner_join(data, suffix = c("", "_opp"), by = "game_id")

data <- data %>%
  filter(team != team_opp)

data <- data %>%
  arrange(game_date)

data <- data %>%
  inner_join(schedule, by = c("team" = "team", "game_id" = "id"))

data <- data %>%
  inner_join(conf %>% select(team, game_id, conf_opp = conference), by = c("team_opp" = "team", "game_id" = "game_id"))

major_confs <- c("Big 12", "Big Ten", "Big East", "Pac-12", "SEC", "ACC")
mid_major_confs <- c("Mountain West", "American", "A 10", "WCC", "MAC", "MVC")

data <- data %>%
  mutate(win = ifelse(points > points_opp, 1, 0),
         loss = ifelse(points < points_opp, 1, 0),
         home = home & !neutral_site,
         on_road = !home & !neutral_site,
         major_opp = ifelse(conf_opp %in% major_confs, 1, 0),
         mid_major_opp = ifelse(conf_opp %in% mid_major_confs, 1, 0),
         major_win = ifelse(win == 1 & major_opp == 1, 1, 0),
         major_home_win = ifelse(win == 1 & major_opp == 1 & home, 1, 0),
         major_away_win = ifelse(win == 1 & major_opp == 1 & on_road, 1, 0),
         major_neutral_win = ifelse(win == 1 & major_opp == 1 & neutral_site, 1, 0),
         major_loss = ifelse(loss == 1 & major_opp == 1, 1, 0),
         major_home_loss = ifelse(loss == 1 & major_opp == 1 & home, 1, 0),
         major_away_loss = ifelse(loss == 1 & major_opp == 1 & on_road, 1, 0),
         major_neutral_loss = ifelse(loss == 1 & major_opp == 1 & neutral_site, 1, 0),
         mid_major_win = ifelse(win == 1 & mid_major_opp == 1, 1, 0),
         mid_major_home_win = ifelse(win == 1 & mid_major_opp == 1 & home, 1, 0),
         mid_major_away_win = ifelse(win == 1 & mid_major_opp == 1 & on_road, 1, 0),
         mid_major_neutral_win = ifelse(win == 1 & mid_major_opp == 1 & neutral_site, 1, 0),
         mid_major_loss = ifelse(loss == 1 & mid_major_opp == 1, 1, 0),
         mid_major_home_loss = ifelse(loss == 1 & mid_major_opp == 1 & home, 1, 0),
         mid_major_away_loss = ifelse(loss == 1 & mid_major_opp == 1 & on_road, 1, 0),
         mid_major_neutral_loss = ifelse(loss == 1 & mid_major_opp == 1 & neutral_site, 1, 0),
         home_win = ifelse(win == 1 & home, 1, 0),
         home_loss = ifelse(loss == 1 & home, 1, 0),
         away_win = ifelse(win == 1 & on_road, 1, 0),
         away_loss = ifelse(loss == 1 & on_road, 1, 0),
         neutral_win = ifelse(win == 1 & neutral_site, 1, 0),
         neutral_loss = ifelse(loss == 1 & neutral_site, 1, 0),
         conf_win = ifelse(win == 1 & conference_competition, 1, 0),
         conf_loss = ifelse(loss == 1 & conference_competition, 1, 0),
         nonconf_win = ifelse(win == 1 & !conference_competition, 1, 0),
         nonconf_loss = ifelse(loss == 1 & !conference_competition, 1, 0),
         conf_home_win = ifelse(win == 1 & conference_competition & home, 1, 0),
         conf_home_loss = ifelse(loss == 1 & conference_competition & home, 1, 0),
         conf_away_win = ifelse(win == 1 & conference_competition & on_road, 1, 0),
         conf_away_loss = ifelse(loss == 1 & conference_competition & on_road, 1, 0),
         nonconf_home_win = ifelse(win == 1 & !conference_competition & home, 1, 0),
         nonconf_home_loss = ifelse(loss == 1 & !conference_competition & home, 1, 0),
         nonconf_away_win = ifelse(win == 1 & !conference_competition & on_road, 1, 0),
         nonconf_away_loss = ifelse(loss == 1 & !conference_competition & on_road, 1, 0))

data %>%
  group_by(team, year) %>%
  mutate(total_wins = cumsum(win),
         total_losses = cumsum(loss),
         home_wins = cumsum(home_win),
         home_losses = cumsum(home_loss),
         away_wins = cumsum(away_win),
         away_losses = cumsum(away_loss),
         neutral_wins = cumsum(neutral_win),
         neutral_losses = cumsum(neutral_loss),
         major_wins = cumsum(major_win),
         major_home_wins = cumsum(major_home_win),
         major_away_wins = cumsum(major_away_win),
         major_neutral_wins = cumsum(major_neutral_win),
         major_losses = cumsum(major_loss),
         major_home_losses = cumsum(major_home_loss),
         major_away_losses = cumsum(major_away_loss),
         major_neutral_losses = cumsum(major_neutral_loss),
         mid_major_wins = cumsum(mid_major_win),
         mid_major_home_wins = cumsum(mid_major_home_win),
         mid_major_away_wins = cumsum(mid_major_away_win),
         mid_major_neutral_wins = cumsum(mid_major_neutral_win),
         mid_major_losses = cumsum(mid_major_loss),
         mid_major_home_losses = cumsum(mid_major_home_loss),
         mid_major_away_losses = cumsum(mid_major_away_loss),
         mid_major_neutral_losses = cumsum(mid_major_neutral_loss),
         conf_wins = cumsum(conf_win),
         conf_losses = cumsum(conf_loss),
         nonconf_wins = cumsum(nonconf_win),
         nonconf_losses = cumsum(nonconf_loss),
         conf_home_wins = cumsum(conf_home_win),
         conf_home_losses = cumsum(conf_home_loss),
         conf_away_wins = cumsum(conf_away_win),
         conf_away_losses = cumsum(conf_away_loss),
         nonconf_home_wins = cumsum(nonconf_home_win),
         nonconf_home_losses = cumsum(nonconf_home_loss),
         nonconf_away_wins = cumsum(nonconf_away_win),
         nonconf_away_losses = cumsum(nonconf_away_loss)) %>%
  ungroup() %>%
  select(team, game_id, total_wins:nonconf_away_losses) -> record


write_csv(record, "../../data/womens/womens_records.csv")
