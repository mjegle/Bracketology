This is the repo for my CMSAC research competition paper. I look at an analytics based bracketology. Using a variety of predictor variables, I filtered down to about 16-17 that are most important in determining which seed a team will get. In here I'll provide a brief description of the files, what they do, and the order in which they should be run. Note that the file names are identical for men's and women's.

First it's important to know how the selection process works. Teams that win their conference tournament are automatically in the tournament. From there, there are 36 at-large bids that are to be determined. These 36 are picked by the selection committee and now we have the 68 team field. Note that this isn't *exactly* how the selection committee goes about this process. There's actually 7 pages of selection guidelines available online. Then the 68 team field is seeded. So for this project we'll need three models: one that predicts the outcome of each conference tournament, one that predicts which of the remaining teams is most likely to receive a tournament bid, and one to predict each selected team's seed.


# Files

All files are listed below in the order in which they are supposed to be run. All of the necessary intermediate data is in the data folder but I wanted to include all of this here so that this project is reproducible. Note that all files will follow the same structure, where mens files start with the "mens_" prefix and womens with the "womens_"

- [mens/womens]_reb.R: This file calculates the rebounding rate for each team at a certain point in the season.

- net_ratings_and_sos.R: Net ratings and strength of schedule calculations for each team.

- teams_conf.R: Finds each team's conference by season. Needs to be by season since conference realignment exists.

- ft_factor.R: Finds each team's free throw factor

- pace.R: Finds each team's possessions / 40 minutes.

- conferences.R: Finds each team's conference by season

- wins_losses.R: Finds win and loss splits for each team at a given point in the season. Total wins as well as wins against major conference teams, etc.

- find_reb_rates.R: Calculates team's offensive and defensive rebounding rates

- to_rates.R: Calculates each team's turnover rates on offense and defense

- steal_rates.R: Calculates offensive and defensive steal rates for each team

- game_type.R: Finds which game type a game was (regular season, conf. Tournament, NCAA tournament, etc)

- ft_factor.R: Calculates free throw factor for each team

- conf_bids.R: (Ended up not using this since it would essentially be cheating), calculates each conference's average number of NCAA tournament bids per year. Uses response variable as a predictor so it would make our model results overly optimistic

- combine_team_stats.R: Combines stats that were created from all of the above files

- combine_stats_and_seeds.R: Combines all team stats with their seed and berth type in the NCAA tournament that year

- head_to_head_model.R: Trains a head to head matchup model between two given teams

- conf_tourney_predictions.R: Simulates conference tournament 100 times and returns probability of each team winning

- mens_at_large_modeling.R: Trains at-large bid model

- create_at-large_bids.R: Generates probability for all given teams to receive an at-large bid

- seed_model.R: Trains model for seeding all 68 teams in the projected field

- project_field.R: Finalizes field of 68 teams in the projected tournament
