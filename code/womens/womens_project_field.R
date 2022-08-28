##### Project Field ######

project_field <- function(stat, this_date, this_season)
{
  stat %>%
    pull(conference) %>%
    unique() -> all_conferences
  
  # Tried to get around this long run time by using mapping but couldn't figure it out
  all_champs <- data.frame()
  for (i in all_conferences)
  {
    conf_champs <- predict_conf_tournament(stat, i, this_date, this_season)
    conf_champs <- conf_champs
    all_champs <- all_champs %>%
      bind_rows(conf_champs)
  }
  
  at_large_bids <- create_at_large_bids(stat, this_date, this_season)
  
  probs <- at_large_bids %>%
    left_join(all_champs, by = c("team" = "team"))
  
  probs <- probs %>%
    mutate(prob = ifelse(is.na(prob), 0, prob)) %>%
    group_by(conference) %>%
    mutate(max_prob = max(prob)) %>%
    ungroup() %>%
    mutate(projected_champ = ifelse(prob == max_prob, 1, 0))
  
  probs <- probs %>%
    arrange(desc(`At-Large`)) %>%
    arrange(desc(projected_champ))
    
  
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
  
  return(probs)
}

test_field <- project_field(stats, lubridate::ymd("2022-03-10"), 2022)
