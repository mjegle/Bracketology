##### Find a team's conference #######

data <- data.frame()

# Load in all years of data
for (i in 2011:2022)
{
  data <- data %>%
    bind_rows(load_mbb_schedule(i) %>%
                select(id, date, neutralSite, conferenceCompetition, notes_headline,
                       home.location, home.conferenceId, home.score, away.location,
                       away.name, away.conferenceId, away.score, season, tournamentId) %>%
                mutate(id = as.integer(id),
                       home.conferenceId = as.integer(home.conferenceId),
                       away.conferenceId = as.integer(away.conferenceId)))
}

# Get home and away side data
home <- data %>%
  select(team = home.location, conference_id = home.conferenceId, id)

away <- data %>%
  select(team = away.location, conference_id = away.conferenceId, id)

# Combine the home and away data
teams_conf <- home %>%
  bind_rows(away)

# Add the conference names
teams_conf <- teams_conf %>%
  mutate(conference_id = as.character(conference_id)) %>%
  inner_join(hoopR::espn_mbb_conferences(), by = c("conference_id" = "group_id"))

# Keep important info
teams_conf <- teams_conf %>%
  select(team, conference = short_name, game_id = id)

# save as CSV
write_csv(teams_conf, "../../data/mens/mens_team_conferences.csv")
