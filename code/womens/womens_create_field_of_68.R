##### Create field of 68 #####

seed_rf <- readRDS("../../data/womens/womens_seed_rf.RDS")

create_field_of_68 <- function(teams)
{
  field <- teams %>%
    head(68)
  
  pred_seed <- predict(seed_rf, newdata = field) %>%
    as.data.frame()
  
  seed_probs <- predict(seed_rf, newdata = field, type = "prob") %>%
    as.data.frame()
  
  colnames(seed_probs) <- paste0("seed_", colnames(seed_probs))
  colnames(pred_seed) <- "pred_seed"
  
  field <- field %>%
    bind_cols(pred_seed) %>%
    bind_cols(seed_probs)
  
  field <- field %>%
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
    arrange(pred_seed) # Rank by the most likely seed but use probability of higher seeds as tie breakers
  
  field <- field %>%
    mutate(rank = 1:n())
  
  return(field)
}

test_68 <- create_field_of_68(test_field)
