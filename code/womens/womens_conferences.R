##### Find a team's conference #######

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

home <- data %>%
  select(team = home_location, conference_id = home_conference_id, id)

away <- data %>%
  select(team = away_location, conference_id = away_conference_id, id)

teams_conf <- home %>%
  bind_rows(away)

teams_conf <- teams_conf %>%
  mutate(conference_id = as.character(conference_id)) %>%
  inner_join(wehoop::espn_wbb_conferences(), by = c("conference_id" = "group_id"))

teams_conf <- teams_conf %>%
  select(team, conference = short_name, game_id = id)

write_csv(teams_conf, "../../data/womens/womens_team_conferences.csv")
