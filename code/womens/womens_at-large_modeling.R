####### Modeling at-large bids #######

library(tidyverse)
library(randomForest)


stats <- read_csv("../../data/womens/womens_stats.csv")
seeds <- read_csv("../../data//womens/womens_tournament_seeds.csv")

stats <- stats %>%
  group_by(team, year) %>%
  summarize(conf_wins = last(conf_wins),
            conf_losses = last(conf_losses),
            wins = last(total_wins),
            losses = last(total_losses),
            major_wins = last(major_wins),
            major_losses = last(major_losses),
            mid_major_wins = last(mid_major_wins),
            mid_major_losses = last(mid_major_losses),
            o_reb_rate = last(o_reb_rate),
            d_reb_rate = last(d_reb_rate),
            off_rating = last(off_rating),
            def_rating = last(def_rating),
            net_rating = last(net_rating),
            sos_off = last(sos_off),
            sos_def = last(sos_def),
            sos_net = last(sos_net),
            to_rate_off = last(to_rate_off),
            to_rate_def = last(to_rate_def),
            steal_rate = last(steal_rate),
            opp_steal_rate = last(opp_steal_rate),
            pace = last(pace),
            ftf = last(ftf),
            ftf_opp = last(ftf_opp),
            conference = last(conference)) %>%
  ungroup() %>%
  group_by(conference, year) %>%
  mutate(conf_avg_net = mean(net_rating))

stats_seeds <- stats %>%
  left_join(seeds, by = c("team" = "School", "year" = "year"))

stats_seeds <- stats_seeds %>%
  mutate(Berth = ifelse(is.na(Berth), "DNQ", Berth),
         Seed = ifelse(is.na(Seed), "DNQ", Seed))

# Remove auto bids
stats_seeds <- stats_seeds %>%
  filter(Berth != "Automatic", Berth != "Auto")

stats_seeds <- stats_seeds %>%
  mutate(Berth = case_when(Berth == "At-large" ~ "At-Large",
                           Berth == "At - Large" ~ "At-Large",
                           T ~ Berth))
###### Create the model #####

stats_seeds$Berth <- as.factor(stats_seeds$Berth)

train <- stats_seeds %>%
  filter(year < 2022, year != 2020) %>% # Can't include 2020 since there was no tournament that season
  ungroup() %>%
  select(-c(team, year, Seed)) %>%
  na.omit()

test <- stats_seeds %>%
  filter(year == 2022) %>%
  ungroup() %>%
  select(-c(team, year, Seed)) %>%
  na.omit()

at_large_rf <- randomForest(Berth ~ conf_wins + conf_losses + wins + losses + major_wins + major_losses + mid_major_wins +
                              mid_major_losses + o_reb_rate + d_reb_rate + off_rating + off_rating + def_rating + net_rating +
                              sos_off + sos_def + sos_net + to_rate_off + to_rate_def + steal_rate + opp_steal_rate + pace +
                              ftf + ftf_opp + conference, data = train,
                            mtry = 10)
pred <- predict(at_large_rf, newdata = test, type = "prob")

saveRDS(at_large_rf, "../../data/womens/womens_at_large_rf.RDS")

pred <- pred %>%
  as.data.frame()

test <- test %>%
  bind_cols(pred)

