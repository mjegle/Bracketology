##### Project Field ######
source("womens_conf_tourney_predictions.R")
source("womens_create_at-large_bids.R")

# Load in all data and functions
matchup_glm <- readRDS("../../data/womens/womens_matchup_glm.RDS")
stats <- read_csv("../../data/womens/womens_stats.csv")
seed_rf <- readRDS("../../data/womens/womens_seed_rf.RDS")
at_large_rf <- readRDS("../../data/womens/womens_at_large_rf.RDS")

project_field <- function(stat, this_date, this_season)
{
  # Gather all conferences from that season
  stat %>%
    filter(year == this_season) %>%
    pull(conference) %>%
    unique() -> all_conferences
  
  # Tried to get around this long run time by using mapping but couldn't figure it out
  all_champs <- data.frame()
  # simulate all conference tournaments
  for (i in all_conferences)
  {
    print(i)
    if (i != "Indep.")
    {
      conf_champs <- predict_conf_tournament(stat, i, this_date, this_season)
      conf_champs <- conf_champs
      all_champs <- all_champs %>%
        bind_rows(conf_champs)
    }
  }
  
  # Find each team's at-large probability
  at_large_bids <- create_at_large_bids(stat, this_date, this_season)
  
  # add conf championship and at-large probabilities
  probs <- at_large_bids %>%
    left_join(all_champs, by = c("team" = "team"))
  
  # Find each conference's projected champ
  probs <- probs %>%
    mutate(prob = ifelse(is.na(prob), 0, prob)) %>%
    group_by(conference) %>%
    mutate(max_prob = max(prob)) %>%
    ungroup() %>%
    mutate(projected_champ = ifelse(prob == max_prob, 1, 0))
  
  # order the teams by overall tournament bid probability
  probs <- probs %>%
    arrange(desc(`At-Large`)) %>%
    arrange(desc(projected_champ))
    
  # Add each team's ranking and a note about their standing
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
  
  # Take the top 68 teams
  top68 <- probs %>%
    head(68)
  
  # predict each team's most likely seed
  pred_seed <- predict(seed_rf, newdata = top68) %>%
    as.data.frame()
  
  # Get each team's seed distribution
  pred <- predict(seed_rf, newdata = top68, type = "prob") %>%
    as.data.frame()
  
  # Rename columns
  colnames(pred_seed) <- "pred_seed"
  colnames(pred) <- paste0("seed_", colnames(pred))
  
  # add the most likely seed and distribution to the team stats
  top68 <- top68 %>%
    bind_cols(pred)
  
  top68 <- top68 %>%
    bind_cols(pred_seed)
  
  # Order and rank the teams
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
  
  # Return the ordered top 68
  return(top68)
}

tictoc::tic()
test_field <- project_field(stats, lubridate::ymd("2014-03-01"), 2014)
tictoc::toc()

write_csv(test_field, "womens_final_projection_2014.csv")
