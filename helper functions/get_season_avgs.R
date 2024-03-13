
# Function to obtain average ratings per season for each team 

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
