#### Model for head to head #####

library(tidyverse)
library(hoopR)
library(randomForest)

# First we need to get all of the game data, only one observation per game
results <- data.frame()
team_stats <- read_csv("../../data/womens/womens_stats.csv")

# Only keep important stats for predicting a game
team_stats <- team_stats %>%
  select(team, year, game_id, o_reb_rate, d_reb_rate, off_rating:pace, ftf, ftf_opp)

# Need to look at stats after last game, since it doesn't include the game we're trying to predict.
team_stats <- team_stats %>%
  group_by(team, year) %>%
  mutate_at(.vars = vars(game_id:ftf_opp),
            .funs = lag)

home_team <- data.frame()

for (i in 2014:2022)
{
  results <- results %>%
    bind_rows(load_wbb_team_box(i) %>% mutate(year = i))
  
  home_team <- home_team %>%
    bind_rows(load_wbb_schedule(i) %>% mutate(year = i) %>% select(game_id = id, home_location, neutral_site))
}

results <- results %>%
  select(team = team_location, game_id, game_date, field_goals_made_field_goals_attempted, three_point_field_goals_made_three_point_field_goals_attempted,
         free_throws_made_free_throws_attempted, year)

results <- results %>%
  separate(field_goals_made_field_goals_attempted, into = c("fgm", "fga"), sep = "-")

results <- results %>%
  separate(three_point_field_goals_made_three_point_field_goals_attempted, into = c("fgm3", "fga3"), sep = "-")

results <- results %>%
  separate(free_throws_made_free_throws_attempted, into = c("ftm", "fta"), sep = "-")

# 
results <- results %>%
  mutate_at(.vars = vars(fgm:fta),
            .funs = as.numeric)

results <- results %>%
  mutate(fgm2 = fgm - fgm3,
         fga2 = fga - fga3,
         points = (ftm) + (2 * fgm2) + (3 * fgm3))

results <- results %>%
  inner_join(results, suffix = c("", "_opp"), by = "game_id")

results <- results %>%
  filter(team != team_opp)


home_stats <- team_stats
away_stats <- team_stats

colnames(home_stats) <- paste0("home_", colnames(home_stats))
colnames(away_stats) <- paste0("away_", colnames(away_stats))

home_team <- home_team %>%
  inner_join(results, by = c("game_id" = "game_id", "home_location" = "team"))

home_team <- home_team %>%
  select(game_id, home_team = home_location, game_date, neutral_site)

data <- home_team %>%
  inner_join(results, by = c("home_team" = "team", "game_id" = "game_id")) %>%
  inner_join(home_stats, by = c("home_team" = "home_team", "game_id" = "home_game_id")) %>%
  inner_join(away_stats, by = c("team_opp" = "away_team", "game_id" = "away_game_id"))

data <- data %>%
  mutate(home_win = ifelse(points > points_opp, 1, 0))


# Model #

train_index <- sample(nrow(data), nrow(data) * 0.8, replace = F)
train <- data[train_index,]
test <- data[-train_index,]



train <- train %>%
  select(home_win, neutral_site, home_o_reb_rate:home_ftf_opp, away_o_reb_rate:away_ftf_opp)
test <- test %>%
  select(home_win, neutral_site, home_o_reb_rate:home_ftf_opp, away_o_reb_rate:away_ftf_opp)

train$home_win <- as.factor(train$home_win)
test$home_win <- as.factor(test$home_win)

tictoc::tic()
matchup_glm <- glm(home_win ~ ., data = train, family = "binomial")
tictoc::toc()

summary(matchup_glm)

pred <- predict(matchup_glm, newdata = test, type = "response")
pred_outcome <- ifelse(pred > 0.5, 1, 0)
mean(pred_outcome == test$home_win, na.rm = T)

#saveRDS(matchup_rf, "../../data/womens/matchup_rf.RDS")
saveRDS(matchup_glm, "../../data/womens/womens_matchup_glm.RDS")

