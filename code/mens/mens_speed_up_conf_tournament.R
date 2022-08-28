###### Predict Conference Tournament faster #######
library(randomForest)
matchup_rf <- readRDS("../../data/mens/mens_matchup_rf.RDS")
matchup_glm <- readRDS("../../data/mens/mens_matchup_glm.RDS")


predict_game_outcome <- function(team1, team2)
{
  if (team1 == "")
  {
    return(team2)
  }
  if (team2 == "")
  {
    return(team1)
  }
  team1_data <- conf_stats %>%
    filter(team == team1)
  
  team2_data <- conf_stats %>%
    filter(team == team2)
  
  colnames(team1_data) <- paste0("home_", colnames(team1_data))
  colnames(team2_data) <- paste0("away_", colnames(team2_data))
  
  input <- team1_data %>% bind_cols(team2_data) %>%
    mutate(neutralSite = T)
  
  
  team1_wp <- suppressWarnings(predict(matchup_glm, newdata = input, "response")[1])
  rand <- runif(1)
  
  if (team1_wp <= rand)
  {
    return(team2)
  }
  else
  {
    return(team1)
  }
}

stats <- read_csv("../data/mens_stats.csv")

conf_stats <- stats %>%
  filter(game_date < lubridate::ymd("2022-03-01"), conference == "Big 12", year == 2022) %>%
  group_by(team) %>%
  summarize(conf_wins = last(conf_wins),
            conf_losses = last(conf_losses),
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
            ftf_opp = last(ftf_opp)) %>%
  mutate(conference = "Big 12")

standings <- conf_stats %>%
  arrange(desc(conf_wins)) %>%
  mutate(conf_seed = 1:n())

num_teams <- max(standings$conf_seed)

seed10 <- standings %>% filter(conf_seed == 10) %>% pull(team)
seed9 <- standings %>% filter(conf_seed == 9) %>% pull(team)
seed8 <- standings %>% filter(conf_seed == 8) %>% pull(team)
seed7 <- standings %>% filter(conf_seed == 7) %>% pull(team)
seed6 <- standings %>% filter(conf_seed == 6) %>% pull(team)
seed5 <- standings %>% filter(conf_seed == 5) %>% pull(team)
seed4 <- standings %>% filter(conf_seed == 4) %>% pull(team)
seed3 <- standings %>% filter(conf_seed == 3) %>% pull(team)
seed2 <- standings %>% filter(conf_seed == 2) %>% pull(team)
seed1 <- standings %>% filter(conf_seed == 1) %>% pull(team)
#  Largest conference is 16
# Smallest conference is 6 

seed16 <- ifelse(is_empty(seed16), "", seed16)
seed15 <- ifelse(is_empty(seed16), "", seed15)
seed14 <- ifelse(is_empty(seed16), "", seed14)
seed13 <- ifelse(is_empty(seed16), "", seed13)
seed12 <- ifelse(is_empty(seed16), "", seed12)
seed11 <- ifelse(is_empty(seed16), "", seed11)
seed10 <- ifelse(is_empty(seed16), "", seed10)
seed9 <- ifelse(is_empty(seed16), "", seed9)
seed8 <- ifelse(is_empty(seed16), "", seed8)
seed7 <- ifelse(is_empty(seed16), "", seed7)

champs <- c()

# Looks like it works sequentially, not when parallelized?
tictoc::tic()
future::plan("sequential")
#future::plan("sequential")
# TODO - find a way to make this work with multisession mode, could speed up process
playin1_winner <- furrr::future_pmap(.l = list(rep(seed16, 100), rep(seed9, 100)),
                                     .f = predict_game_outcome,
                                     .options = furrr::furrr_options(seed = TRUE))
playin1_winner <- c(playin1_winner)


playin2_winner <- furrr::future_pmap(.l = list(rep(seed7, 100), rep(seed10, 100)),
                                     .f = predict_game_outcome,
                                     .options = furrr::furrr_options(seed = TRUE))


##### Quarterfinals #####
q1_winner <- furrr::future_pmap(.l = list(rep(seed1, 100), playin1_winner),
                                .f = predict_game_outcome,
                                .options = furrr::furrr_options(seed = TRUE))

q2_winner <- furrr::future_pmap(.l = list(rep(seed4, 100), rep(seed5, 100)),
                                .f = predict_game_outcome,
                                .options = furrr::furrr_options(seed = TRUE))

q3_winner <- furrr::future_pmap(.l = list(rep(seed3, 100), rep(seed6, 100)),
                                .f = predict_game_outcome,
                                .options = furrr::furrr_options(seed = TRUE))

q4_winner <- furrr::future_pmap(.l = list(rep(seed2, 100), playin2_winner),
                                .f = predict_game_outcome,
                                .options = furrr::furrr_options(seed = TRUE))

##### Semifinals #####

s1_winner <- furrr::future_pmap(.l = list(q1_winner, q2_winner),
                                .f = predict_game_outcome,
                                .options = furrr::furrr_options(seed = TRUE))

s2_winner <- furrr::future_pmap(.l = list(q3_winner, q4_winner),
                                .f = predict_game_outcome,
                                .options = furrr::furrr_options(seed = TRUE))

##### Championship #####

champs <- furrr::future_pmap(.l = list(s1_winner, s2_winner),
                                 .f = predict_game_outcome,
                                 .options = furrr::furrr_options(seed = TRUE))

champs <- champs %>%
  unlist() %>%
  as.data.frame()

colnames(champs)[1] <- "team"

champ_probs <- champs %>%
  group_by(team) %>%
  count() %>%
  mutate(prob = n / 100) %>%
  select(team, prob) %>%
  arrange(desc(prob))

tictoc::toc()
