This is the repo for my CMSAC research competition paper. I look at an analytics based bracketology. Using a variety of predictor variables, I filtered down to about 16-17 that are most important in determining which seed a team will get. In here I'll provide a brief description of the files, what they do, and the order in which they should be run. Note that the file names are identical for men's and women's.

First it's important to know how the selection process works. Teams that win their conference tournament are automatically in the tournament. From there, there are 36 at-large bids that are to be determined. These 36 are picked by the selection committee and now we have the 68 team field. Note that this isn't *exactly* how the selection committee goes about this process. There's actually 7 pages of selection guidelines available online. Then the 68 team field is seeded. So for this project we'll need three models: one that predicts the outcome of each conference tournament, one that predicts which of the remaining teams is most likely to receive a tournament bid, and one to predict each selected team's seed.


# Files

All files are listed below in the order in which they are supposed to be run. All of the necessary intermediate data is in the data folder but I wanted to include all of this here so that this project is reproducible.

- reb.R: This file calculates the rebounding rate for each team at a certain point in the season.

- wins_losses.R: Finds win and loss splits for each team at a given point in the season. Total wins as well as wins against major conference teams, etc.

- net_ratings_and_sos.R: Net ratings and strength of schedule calculations for each team.

- teams_conf.R: Finds each team's conference by season. Needs to be by season since conference realignment exists.

