####### Create the At-Large Bids ######

at_large_rf <- readRDS("../../data/womens/womens_at_large_rf")

create_at_large_bids <- function(stat, this_date, this_season)
{
  stat <- stat %>%
    filter(game_date < this_date, year == this_season) %>%
    group_by(team, year) %>%
    summarize(conf_wins = last(conf_wins),
              conf_losses = last(conf_losses),
              wins = last(total_wins),
              losses = last(total_losses),
              major_wins = last(major_wins),
              major_losses = last(major_losses),
              mid_major_wins = last(mid_major_wins),
              mid_major_losses = last(mid_major_losses),
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
              ftf_opp = last(ftf_opp),
              conference = last(conference),
              avg_bids = last(avg_bids)) %>%
    ungroup() %>%
    group_by(conference, year) %>%
    mutate(conf_avg_net = mean(net_rating))
    
    pred <- predict(at_large_rf, newdata = stat, type = "prob")
    
    probs <- stat %>%
      bind_cols(pred %>% as.data.frame())
    
    return(probs)
}