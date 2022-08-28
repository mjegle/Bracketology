##### Predict Tournament Results ######
# This has some assumptions which do not hold in real life college basketball:
# 1. We assume that each team qualifies for their conference tournament
# 2. We assume that there are a minimum number of byes for each team. That is, if there are 12 teams, the top 4 teams receive
#    a first round bye
# 

library(tidyverse)
library(randomForest)

matchup_rf <- readRDS("../../data/womens/matchup_rf")

# 
# stats %>%
#   select(conference, team, year) %>%
#   distinct() %>%
#   group_by(conference, year) %>%
#   count() %>%
#   arrange(n) %>%
#   pull(n) %>%
#   unique()


#'
#'
#' @param stats - the dataframe of stats by game played for each team
#' @param conference - the conference we want to predict the outcome for
#' @param date - the last date we're trying to predict on (NOT inclusive)
#'
#' @return champs - the probabilities for all observed champions of the given conference's tournament
#'
predict_conf_tournament <- function(stat, this_conference, this_date, this_year)
{
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
  
  conf_stats <- stat %>%
    filter(game_date < this_date, conference == this_conference, year == this_year) %>%
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
    mutate(conference = this_conference)
  
  standings <- conf_stats %>%
    arrange(desc(conf_wins)) %>%
    mutate(conf_seed = 1:n())
  
  num_teams <- max(standings$conf_seed)
  
  seed16 <- standings %>% filter(conf_seed == 16) %>% pull(team)
  seed15 <- standings %>% filter(conf_seed == 15) %>% pull(team)
  seed14 <- standings %>% filter(conf_seed == 14) %>% pull(team)
  seed13 <- standings %>% filter(conf_seed == 13) %>% pull(team)
  seed12 <- standings %>% filter(conf_seed == 12) %>% pull(team)
  seed11 <- standings %>% filter(conf_seed == 11) %>% pull(team)
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
  
  # If there was no team at the given seed line, replace it with an empty string
  seed16 <- ifelse(is_empty(seed16), "", seed16)
  seed15 <- ifelse(is_empty(seed15), "", seed15)
  seed14 <- ifelse(is_empty(seed14), "", seed14)
  seed13 <- ifelse(is_empty(seed13), "", seed13)
  seed12 <- ifelse(is_empty(seed12), "", seed12)
  seed11 <- ifelse(is_empty(seed11), "", seed11)
  seed10 <- ifelse(is_empty(seed10), "", seed10)
  seed9 <- ifelse(is_empty(seed9), "", seed9)
  seed8 <- ifelse(is_empty(seed8), "", seed8)
  seed7 <- ifelse(is_empty(seed7), "", seed7)
  
  champs <- c()
  
  playin1_winner <- furrr::future_pmap(.l = list(rep(seed1, 100), rep(seed16, 100)),
                                       .f = predict_game_outcome,
                                       .options = furrr::furrr_options(seed = TRUE))
  
  playin2_winner <- furrr::future_pmap(.l = list(rep(seed8, 100), rep(seed9, 100)),
                                       .f = predict_game_outcome,
                                       .options = furrr::furrr_options(seed = TRUE))
  
  playin3_winner <- furrr::future_pmap(.l = list(rep(seed4, 100), rep(seed13, 100)),
                                       .f = predict_game_outcome,
                                       .options = furrr::furrr_options(seed = TRUE))
  
  playin4_winner <- furrr::future_pmap(.l = list(rep(seed5, 100), rep(seed12, 100)),
                                       .f = predict_game_outcome,
                                       .options = furrr::furrr_options(seed = TRUE))
  
  playin5_winner <- furrr::future_pmap(.l = list(rep(seed6, 100), rep(seed11, 100)),
                                       .f = predict_game_outcome,
                                       .options = furrr::furrr_options(seed = TRUE))
  
  playin6_winner <- furrr::future_pmap(.l = list(rep(seed3, 100), rep(seed14, 100)),
                                       .f = predict_game_outcome,
                                       .options = furrr::furrr_options(seed = TRUE))
  
  playin7_winner <- furrr::future_pmap(.l = list(rep(seed7, 100), rep(seed10, 100)),
                                       .f = predict_game_outcome,
                                       .options = furrr::furrr_options(seed = TRUE))
  
  playin8_winner <- furrr::future_pmap(.l = list(rep(seed2, 100), rep(seed15, 100)),
                                       .f = predict_game_outcome,
                                       .options = furrr::furrr_options(seed = TRUE))
  
  ##### Quarterfinals #####
  q1_winner <- furrr::future_pmap(.l = list(playin1_winner, playin2_winner),
                                  .f = predict_game_outcome,
                                  .options = furrr::furrr_options(seed = TRUE))
  
  q2_winner <- furrr::future_pmap(.l = list(playin3_winner, playin4_winner),
                                  .f = predict_game_outcome,
                                  .options = furrr::furrr_options(seed = TRUE))
  
  q3_winner <- furrr::future_pmap(.l = list(playin5_winner, playin6_winner),
                                  .f = predict_game_outcome,
                                  .options = furrr::furrr_options(seed = TRUE))
  
  q4_winner <- furrr::future_pmap(.l = list(playin7_winner, playin8_winner),
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
  
  return(champ_probs)
}

#TODO - check the code to ensure consistency in game predictions

# Testing purposes:
tictoc::tic()
test <- predict_conf_tournament(stats, "Big Ten", lubridate::ymd("2020-03-15"), 2020)
tictoc::toc()
