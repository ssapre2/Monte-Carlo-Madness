library(hoopR)
library(dplyr)
library(tidyr)

mbb.box.2022 = load_mbb_team_box(2022)
mbb.schedule.2022 = load_mbb_schedule(2022)



mbb.box.2022 %>%
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
  select(-team_winner)-> winners.22

team_results %>%
  filter(team_winner == FALSE)%>% 
  select(-team_winner) -> losers.22

winners.22 %>%
  inner_join(y = losers.22,by = "game_id",suffix = c("",'.opp')) -> winner.opp

losers.22 %>%
  inner_join(y = winners.22,by = "game_id", suffix = c("",'.opp')) -> losers.opp

bind_rows(winner.opp,losers.opp) -> game_results


# Calculate Game Averages for team and Opponent
game_results %>%
  group_by(team_id) %>%
  summarise(n = n(),
            team_name = first(name),
            offRtg.avg = mean(offRtg,na.rm = T),
            offRtg.sd = sd(offRtg,na.rm = T),
            defRtg.avg = mean(offRtg.opp,na.rm = T),
            defRtg.sd = sd(offRtg.opp,na.rm = T),
            netRtg.avg = offRtg.avg - defRtg.avg) %>%
  ungroup() %>%
  # We want full seasons
  filter(n > 1) -> avg.ratings


# Function to generate sample scores
generate_team_scores = function(x){
  
    # Sample from offensive rating centered around mean and sd of chosen team
    off.rtg = rnorm(n = 1000, mean = x[1], sd = x[2])
    
    # Sample from defensive rating centered around mean and sd of chosen team
    def.rtg = rnorm(n = 1000, mean = x[3], sd = x[4])
  
    return(list("Off. Rtg" = mean(off.rtg), "Def. Rtg" = mean(def.rtg)))
  
}




generate_team_scores(c(78.7,12.6,67,10))

apply(select(avg.ratings,4:7), 1,generate_team_scores)
# Export to csv
  #write.csv('2022-mbb-box.csv')
