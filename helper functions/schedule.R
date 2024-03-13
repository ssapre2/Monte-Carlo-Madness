library(hoopR)
library(dplyr)
library(tidyr)
library(ggplot2)

#mbb.schedule.2022 = load_mbb_schedule(2022)

#mbb.box.2022 %>% 
  # Filter on post-season games
#  filter(season_type == 3) -> post_season


# Function to obtain average ratings per season for each team 
# Get Season Averages ---------------------
get_season_avgs = function(year){
  
  
  message(paste0("Pulling Box Scores for "),year)
  box.df = load_mbb_team_box(year)
  
  message("Compiling Season Results for Each Team")
  box.df %>%
    # Only use regular season games
    filter(season_type == 2) %>%
    # Calculate the number of possessions and ratings
    group_by(game_id,team_winner) %>%
    summarise(
      team_id = first(team_id),
      name = first(team_display_name),
      team_poss = field_goals_attempted - offensive_rebounds + turnovers + 0.5*free_throws_attempted,
      offRtg = (team_score / team_poss) * 100
    ) %>%
    ungroup() -> team_results
  
  
  # Separate Winners and losers, join back
  
  team_results %>%
    filter(team_winner == TRUE) %>% 
    select(-team_winner)-> winners
  
  team_results %>%
    filter(team_winner == FALSE)%>% 
    select(-team_winner) -> losers
  
  winners %>%
    inner_join(y = losers,by = "game_id",suffix = c("",'.opp')) -> winner.opp
  
  losers %>%
    inner_join(y = winners,by = "game_id", suffix = c("",'.opp')) -> losers.opp
  
  bind_rows(winner.opp,losers.opp) -> game_results
  
  message("Calculating Averages")
  
  # Calculate Game Averages for team and Opponent
  game_results %>%
    group_by(team_id) %>%
    summarise(n = n(),
              team_name = first(name),
              offRtg.avg = mean(offRtg,na.rm = T),
              offRtg.sd = sd(offRtg,na.rm = T),
              defRtg.avg = mean(offRtg.opp,na.rm = T),
              defRtg.sd = sd(offRtg.opp,na.rm = T)) %>%
    ungroup() %>%
    # We want full seasons
    filter(n > 1) %>%
    # Add column to denote year
    mutate(year = year) -> avg.ratings
  
  message("------------------------")
  
  return(avg.ratings)
  
}





# SIMULATION ------------------

set.seed(1)



# Simulate the post_season of a given year
simulate_post_season = function(year){
  
  
  # Initialize ----
  
  load_mbb_team_box(year) %>% 
    # Filter on post-season games
  filter(season_type == 3) -> post_season
    
  # Load schedule from this season
  mbb.schedule = load_mbb_schedule(year)
  
  # Load average off and def ratings for the season
  avg.ratings = get_season_avgs(year)

    
  # Get teams for every game ID
  post_season %>%
    distinct(game_id,team_id) %>%
    # Pull in season averages
    inner_join(avg.ratings,by = 'team_id') %>% 
    # Apply sampling function rowwise
    rowwise() %>%
    mutate(
      # Simulate 100 replications for each game
      off.rtg = list(rnorm(n = 100, mean = offRtg.avg, sd = offRtg.sd)),
      def.rtg = list(rnorm(n = 100, mean = defRtg.avg, sd = defRtg.sd))
    ) %>%
    unnest_longer(col = c(off.rtg,def.rtg)) %>%
    select(game_id,team_id,off.rtg,def.rtg) -> matchups
  
  # Use schedule to set matchups
  mbb.schedule %>%
    select(id,home_id,away_id,home_name,away_name,notes_headline,home_winner) %>%
    # Join samples for home teams
    inner_join(y = select(matchups,game_id,team_id,home.off.rtg = off.rtg,home.def.rtg = def.rtg,def.rtg),
               by = c("home_id" = "team_id","id" = "game_id"),
               relationship = "one-to-many") %>%
    # Join samples for away teams
    inner_join(y = select(matchups,game_id,team_id,away.off.rtg = off.rtg,away.def.rtg = def.rtg,def.rtg),
               by = c("away_id" = "team_id","id" = "game_id"),
               relationship = "many-to-many")-> matchup.ready
  
  matchup.ready %>%
    # determine NET rating for home team... > 0 == "Winner" Else "Loser"
    mutate(
      home.net.rtg = (home.off.rtg + away.def.rtg) / 2,
      away.net.rtg = (away.off.rtg + home.def.rtg) / 2,
      simulated.winner.home = ifelse(home.net.rtg > away.net.rtg,T,F),
      simulation.res = ifelse(simulated.winner.home == home_winner,
                              yes = T,no = F)
    ) -> simulated_results
  
  # simulation results by game
  simulated_results %>%
    group_by(id) %>%
    summarise(game_acc = mean(simulation.res),n.games = n()) -> game.probs
  
  # Display Prediction Accuracy Distribution
  game.probs$game_acc %>%
    hist(main = paste0("Prediction Accuracy for ", year," March Madness"))
  
  # Sensitivity Analysis ---------------
  # Build Logit Model
  # Predict Winner selection based on the sample scores
  winner.logit = glm(data = simulated_results,formula = simulation.res ~ home.off.rtg + home.def.rtg + away.off.rtg + away.def.rtg,family = "binomial")
  print(summary(winner.logit))

  
 simulated_results %>%
    select(id,home_name,away_name,home.net.rtg,away.net.rtg,home_winner,
           simulated.winner.home,simulation.res) -> simulated.scores
  
  # Calculate accuracy
  message(sum(simulated.scores$simulation.res)/ nrow(simulated.scores))
  
  # Return matchups with predictor columns prepared
  #return(matchup.ready)
  
  
  
}




