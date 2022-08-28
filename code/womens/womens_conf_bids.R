###### How many bids does each league get per year? #####
# Technically "cheating" since we're using the response as a predictor in a way
# But the number of tournament bids is roughly the same year to year for each conference?
# Might not need it if the conference itself is a variable in the model

seeds <- read_csv("../../data/womens/womens_tournament_seeds.csv")
reb <- read_csv("../../data/womens/reb_rates.csv") # Only need this for the year column
teams_conf <- read_csv("../../data/womens/team_conferences.csv")


seeds <- seeds %>%
  mutate(School = str_trim(School),
         School = case_when(School == "Arkansas–Little Rock" ~ "Little Rock",
                            School == "Connecticut" ~ "UConn",
                            School == "Loyola" ~ "Loyola (MD)",
                            School == "LIU-Brooklyn" ~ "Long Island University",
                            School == "LIU Brooklyn" ~ "Long Island University",
                            School == "Long Island" ~ "Long Island University",
                            School == "Saint Mary's (CA)" ~ "Saint Mary's",
                            School == "Louisiana-Lafayette" ~ "Louisiana",
                            School == "Massachusetts" ~ "UMass",
                            School == "Mississippi" ~ "Ole Miss",
                            School == "North Carolina State" ~ "NC State",
                            School == "Hawaii" ~ "Hawai'i",
                            School == "Cal State Bakersfield" ~ "CSU Bakersfield",
                            School == "Miami (FL)" ~ "Miami",
                            School == "Cal State Fullerton" ~ "CSU Fullerton",
                            School == "College of Charleston" ~ "Charleston",
                            School == "Penn" ~ "Pennsylvania",
                            School == "Gardner–Webb" ~ "Gardner-Webb", # Weird different kind of dash
                            School == "Texas A&M–Corpus Christi" ~ "Texas A&M-CC",
                            T ~ School))

reb <- reb %>%
  select(game_id, team, year)

teams_conf <- teams_conf %>%
  inner_join(reb, by = c("team" = "team", "game_id" = "game_id")) %>%
  select(team, year, conference)

seeds <- seeds %>%
  inner_join(teams_conf, by = c("School" = "team", "year" = "year")) %>%
  distinct()

conf_bids <- seeds %>%
  group_by(conference) %>%
  summarize(bids = n(),
            years = length(unique(year)),
            avg_bids = bids / years)

write_csv(conf_bids, "../../data/womens/womens_conf_bids.csv")
