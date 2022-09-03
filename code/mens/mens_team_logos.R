##### Find team logos ######

schedule <- data.frame()

# TODO - add variables for major and mid-major conference wins
for (i in 2011:2022)
{
  schedule <- schedule %>%
    bind_rows(load_mbb_schedule(i) %>%
                select(id, home.location, away.location, neutralSite, conferenceCompetition, notes_headline, tournamentId,
                       home.logo, away.logo) %>%
                mutate(id = as.integer(id))) %>%
    mutate(year = i)
}

home_logos <- schedule %>%
  select(team = home.location, year, logo = home.logo) %>%
  group_by(team, year) %>%
  summarize(logo = first(logo))

away_logos <- schedule %>%
  select(team = away.location, year, logo = away.logo) %>%
  group_by(team, year) %>%
  summarize(logo = first(logo))

logos <- home_logos %>%
  bind_rows(away_logos) %>%
  distinct()

write_csv(logos, "../../data/mens/mens_logos.csv")

