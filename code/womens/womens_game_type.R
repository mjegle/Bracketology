####### Game type ########

# Find what event the game was played in: regular season, conf tournament, ncaa, etc
# important bc we can't have NCAA tournament games in our training data because it'll skew the training

library(wehoop)

data <- data.frame()

for (i in 2014:2022)
{
  data <- data %>%
    janitor::clean_names() %>%
    bind_rows(load_wbb_schedule(i) %>%
                select(id, date, neutral_site, conference_competition,
                       home_location, home_conference_id, away_location,
                       away_name, away_conference_id, season, tournament_id) %>%
                mutate(id = as.integer(id),
                       home_conference_id = as.integer(home_conference_id),
                       away_conference_id = as.integer(away_conference_id)))
}

# Get rid of the following
# 22: NCAA tournament
# 38: WBI
# 37: WNIT

test <- data %>%
  filter(!is.na(tournament_id))

data <- data %>%
  filter(is.na(tournament_id) | !(tournament_id %in% c(22, 37, 38))) %>%
  select(id, tournament_id)

write_csv(data, "../../data/womens/womens_eligible_game_ids.csv")
