##### Find team logos ######

schedule <- data.frame()

# Load in game data
for (i in 2011:2022)
{
  schedule <- schedule %>%
    bind_rows(load_mbb_schedule(i) %>%
                select(id, home.location, away.location, neutralSite, conferenceCompetition, notes_headline, tournamentId,
                       home.logo, away.logo) %>%
                mutate(id = as.integer(id))) %>%
    mutate(year = i)
}

# Grab the home team's first logo since there are times where a team has two different logos
home_logos <- schedule %>%
  select(team = home.location, year, logo = home.logo) %>%
  group_by(team) %>%
  summarize(logo = first(logo))

# Same as above but for the away team
away_logos <- schedule %>%
  select(team = away.location, year, logo = away.logo) %>%
  group_by(team) %>%
  summarize(logo = first(logo))

# Combine the two
logos <- home_logos %>%
  bind_rows(away_logos) %>%
  distinct()

# save as a CSV
write_csv(logos, "../../data/mens/mens_logos.csv")

