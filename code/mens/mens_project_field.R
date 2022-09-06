##### Project Field ######
source("mens_conf_tourney_predictions.R")
source("mens_create_at-large_bids.R")

matchup_glm <- readRDS("../../data/mens/mens_matchup_glm.RDS")
stats <- read_csv("../../data/mens/mens_stats.csv")
seed_rf <- readRDS("../../data/mens/mens_seed_rf.RDS")
at_large_rf <- readRDS("../../data/mens/mens_at_large_rf.RDS")

#'
#' FInds the projected field of 68 at a given date in the season
#' @param stat: the season's cumulative stats
#' @param this_date: the date on which to project the season
#' @param this_season: the season we are predicting in
#' 
#' @return top68: the field of 68 in order
#'
project_field <- function(stat, this_date, this_season)
{
  # Gather all of the conferences in the given season
  stat %>%
    filter(year == this_season) %>%
    pull(conference) %>%
    unique() -> all_conferences
  
  # Tried to get around this long run time by using mapping but couldn't figure it out
  all_champs <- data.frame()
  for (i in all_conferences)
  {
    print(i)
    conf_champs <- predict_conf_tournament(stat, i, this_date, this_season)
    conf_champs <- conf_champs
    all_champs <- all_champs %>%
      bind_rows(conf_champs)
  }
  
  # Find the at large probabilities for each team
  at_large_bids <- create_at_large_bids(stat, this_date, this_season)
  
  # combine the at large probabilities and conference champ probabilities
  probs <- at_large_bids %>%
    left_join(all_champs, by = c("team" = "team"))
  
  # Find the projected champion for each conference
  probs <- probs %>%
    mutate(prob = ifelse(is.na(prob), 0, prob)) %>%
    group_by(conference) %>%
    mutate(max_prob = max(prob)) %>%
    ungroup() %>%
    mutate(projected_champ = ifelse(prob == max_prob, 1, 0))
  
  # Arrange the data by the top 32 conference champs and the top 36 at large probabilities of the remaining teams
  probs <- probs %>%
    arrange(desc(`At-Large`)) %>%
    arrange(desc(projected_champ))
    
  # Add the teams overall rankings and a comment about their standing
  probs <- probs %>%
    mutate(index = 1:n(),
           desc = case_when(index < 61 ~ "In",
                            index >= 61 & index <= 64 ~ "Last Four Byes",
                            index >= 65 & index <= 68 ~ "Last Four In",
                            index >= 69 & index <= 72 ~ "First Four Out",
                            index >= 73 & index <= 76 ~ "Next Four Out",
                            `At-Large` > 0.1 ~ "In The Hunt",
                            prob > 0 ~ "Must Win Conference Tournament",
                            T ~ "No Shot"))
  # Take the top 68
  top68 <- probs %>%
    head(68)
  
  # find each team's most likely seed
  pred_seed <- predict(seed_rf, newdata = top68) %>%
    as.data.frame()
  
  # find each team's seed distribution
  pred <- predict(seed_rf, newdata = top68, type = "prob") %>%
    as.data.frame()
  
  # update column names for prediction data frames
  colnames(pred_seed) <- "pred_seed"
  colnames(pred) <- paste0("seed_", colnames(pred))
  
  # add the prediction data frames
  top68 <- top68 %>%
    bind_cols(pred)
  
  top68 <- top68 %>%
    bind_cols(pred_seed)
  
  # arrange and rank the teams one more time
  top68 <- top68 %>%
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
    arrange(pred_seed) %>%
    select(-index)
  
  top68 <- top68 %>%
    mutate(rank = 1:n())
  
  # Return the field of 68
  return(top68)
}

