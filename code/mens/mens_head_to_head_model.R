#### Model for head to head #####

library(tidyverse)
library(hoopR)
library(randomForest)

# First we need to get all of the game data, only one observation per game
results <- data.frame()
team_stats <- read_csv("../../data/mens/mens_stats.csv")

# Only keep important stats for predicting a game
team_stats <- team_stats %>%
  select(team, year, game_id, o_reb_rate, d_reb_rate, off_rating:pace, ftf, ftf_opp)

# Need to look at stats after last game, since it doesn't include the game we're trying to predict.
team_stats <- team_stats %>%
  group_by(team, year) %>%
  mutate_at(.vars = vars(game_id:ftf_opp),
            .funs = lag)

# Load all extra data
for (i in 2011:2022)
{
  results <- results %>%
    bind_rows(load_mbb_schedule(i) %>%
                select(id, date, neutralSite, conferenceCompetition, notes_headline,
                       home.location, home.conferenceId, home.score, away.location,
                       away.name, away.conferenceId, away.score, season, tournamentId) %>%
                mutate(id = as.integer(id),
                       home.conferenceId = as.integer(home.conferenceId),
                       away.conferenceId = as.integer(away.conferenceId)))
}

# Only keep regular season and conf. tournaments
results <- results %>%
  filter(is.na(tournamentId) | !(tournamentId %in% c(22, 11, 21, 35, 42)))

# Find if the home team won
results <- results %>%
  mutate(home_win = ifelse(home.score > away.score, 1, 0))

# create the home and away stats data
home_stats <- team_stats
away_stats <- team_stats

# Update the column names so we know which team won and what their stats were
colnames(home_stats) <- paste0("home_", colnames(home_stats))
colnames(away_stats) <- paste0("away_", colnames(away_stats))

# Join the stats data with the results
results <- results %>%
  inner_join(home_stats, by = c("home.location" = "home_team", "id" = "home_game_id")) %>%
  inner_join(away_stats, by = c("away.location" = "away_team", "id" = "away_game_id"))


# Model #

# Create train and test data
train_index <- sample(nrow(results), nrow(results) * 0.8, replace = F)
train <- results[train_index,]
test <- results[-train_index,]


# Only keep the variables we want to use
train <- train %>%
  select(home_win, neutralSite, home_o_reb_rate:home_ftf_opp, away_o_reb_rate:away_ftf_opp)
test <- test %>%
  select(home_win, neutralSite, home_o_reb_rate:home_ftf_opp, away_o_reb_rate:away_ftf_opp)

# Make the response a factor
train$home_win <- as.factor(train$home_win)
test$home_win <- as.factor(test$home_win)

# Train the model
matchup_glm <- glm(home_win ~ ., data = train, family = "binomial")

summary(matchup_glm)

saveRDS(matchup_glm, "../../data/mens/mens_matchup_glm.RDS")

