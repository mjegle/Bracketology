####### Game type ########

# Find what event the game was played in: regular season, conf tournament, ncaa, etc
# important bc we can't have NCAA tournament games in our training data because it'll skew the training

library(hoopR)

# Load in all game data
data <- data.frame()

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

# Get rid of the following
# 22: NCAA tournament
# 11: CBI
# 21: NIT
# 35: CIT
# 42: basketball classic

# Only keep games that were not in the NCAA, CBI, NIT, CIT, or basketball classic
data <- data %>%
  filter(is.na(tournamentId) | !(tournamentId %in% c(22, 11, 21, 35, 42))) %>%
  select(id, tournamentId)

# Save as a csv
write_csv(data, "../../data/mens/mens_eligible_game_ids.csv")
