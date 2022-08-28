####### Create the seed model ######

library(tidyverse)
library(randomForest)

seeds <- read_csv("../../data/mens/mens_tournament_seeds.csv")
stats <- read_csv("../../data/mens/mens_stats.csv")

last_stats <- stats %>%
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
  ungroup()

# Will need to solve the american university issue later

stats_seeds <- seeds %>%
  inner_join(last_stats, by = c("School" = "team", "year" = "year"))

stats_seeds$Seed <- as.factor(stats_seeds$Seed)

train <- stats_seeds %>%
  filter(year <= 2021)

test <- stats_seeds %>%
  filter(year == 2022)

seed_rf <- randomForest(Seed ~ conf_wins + conf_losses + wins + losses + major_wins + major_losses + mid_major_wins +
                          mid_major_losses + o_reb_rate + d_reb_rate + off_rating + off_rating + def_rating + net_rating +
                          sos_off + sos_def + sos_net + to_rate_off + to_rate_def + steal_rate + opp_steal_rate + pace +
                          ftf + ftf_opp + conference,
                        data = train)

saveRDS(seed_rf, "../../data/mens/mens_seed_rf.RDS")

pred <- predict(seed_rf, newdata = test, type = "prob") %>%
  as.data.frame()

colnames(pred) <- paste0("seed_", colnames(pred))

pred_seed <- predict(seed_rf, newdata = test) %>%
  as.data.frame()

colnames(pred_seed) <- "pred_seed"

test <- test %>%
  bind_cols(pred)

test <- test %>%
  mutate(exp_seed = (1 * seed_1) + (2 * seed_2) + (3 * seed_3) + (4 * seed_4) + (5 * seed_5) + (6 * seed_6) + (7 * seed_7) +
           (8 * seed_8) + (9 * seed_9) + (10 * seed_10) + (11 * seed_11) + (12 * seed_12) + (13 * seed_13) + (14 * seed_14) + 
           (15 * seed_15) + (16 * seed_16))

test <- test %>%
  bind_cols(pred_seed)

test <- test %>%
  arrange(desc(seed_16)) %>%
  arrange(desc(seed_15)) %>%
  arrange(desc(seed_14)) %>%
  arrange(desc(seed_13)) %>%
  arrange(desc(seed_12)) %>%
  arrange(desc(seed_11)) %>%
  arrange(desc(seed_10)) %>%
  arrange(desc(seed_9)) %>%
  arrange(desc(seed_8)) %>%
  arrange(desc(seed_7)) %>%
  arrange(desc(seed_6)) %>%
  arrange(desc(seed_5)) %>%
  arrange(desc(seed_4)) %>%
  arrange(desc(seed_3)) %>%
  arrange(desc(seed_2)) %>%
  arrange(desc(seed_1)) %>%
  arrange(pred_seed)

test <- test %>%
  mutate(rank = 1:n())

