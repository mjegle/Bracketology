###### COMBINE ALL STATS ######

library(tidyverse)

reb <- read_csv("../../data/womens/womens_reb_rates.csv")
record <- read_csv("../../data/womens/womens_records.csv")
net <- read_csv("../../data/womens/womens_net_ratings.csv")
sos <- read_csv("../../data/womens/womens_sos_ratings.csv")
teams_conf <- read_csv("../../data/womens/womens_team_conferences.csv")
to <- read_csv("../../data/womens/womens_team_to_rates.csv")
steal <- read_csv("../../data/womens/womens_team_steal_rates.csv")
pace <- read_csv("../../data/womens/womens_team_pace.csv")
event <- read_csv("../../data/womens/womens_eligible_game_ids.csv")
ftf <- read_csv("../../data/womens/womens_ft_factor.csv")
#conf_bids <- read_csv("../../data/womens/womens_conf_bids.csv")

stats <- reb %>%
  inner_join(record, by = c("team" = "team", "game_id" = "game_id")) %>%
  inner_join(net, by = c("team" = "team", "game_id" = "game_id")) %>%
  inner_join(sos, by = c("team" = "team", "game_id" = "game_id")) %>%
  inner_join(teams_conf, by = c("team" = "team", "game_id" = "game_id")) %>%
  inner_join(to, by = c("team" = "team", "game_id" = "game_id")) %>%
  inner_join(steal, by = c("team" = "team", "game_id" = "game_id")) %>%
  inner_join(pace, by = c("team" = "team", "game_id" = "game_id")) %>%
  inner_join(event, by = c("game_id" = "id")) %>%
  inner_join(ftf, by = c("team" = "team", "game_id" = "game_id"))

write_csv(stats, "../../data/womens/womens_stats.csv")

