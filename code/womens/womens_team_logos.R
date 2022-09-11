##### Find team logos ######

schedule <- data.frame()

# TODO - add variables for major and mid-major conference wins
for (i in 2014:2022)
{
  schedule <- schedule %>%
    bind_rows(load_wbb_schedule(i) %>%
                select(id, home_location, away_location, neutral_site, conference_competition, tournament_id,
                       home_logo, away_logo) %>%
                mutate(id = as.integer(id))) %>%
    mutate(year = i)
}

home_logos <- schedule %>%
  select(team = home_location, year, logo = home_logo) %>%
  group_by(team) %>%
  summarize(logo = first(logo))

away_logos <- schedule %>%
  select(team = away_location, year, logo = away_logo) %>%
  group_by(team) %>%
  summarize(logo = first(logo))

logos <- home_logos %>%
  bind_rows(away_logos) %>%
  distinct()

write_csv(logos, "../../data/womens/womens_logos.csv")

