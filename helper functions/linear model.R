library('dplyr')
library('tidyr')


# Get a few years worth of regular season data to build and validate the model on
season.avg.list = lapply(c(2021,2022),get_season_avgs)
do.call(rbind,season.avg.list) -> season.avg.df

# Run simulation on datasets
sim.post.list = lapply(c(2019,2018,2021,2022),FUN = function(x) replicate(100,simulate_post_season))
do.call(rbind,sim.post.list) -> sim.post.df



