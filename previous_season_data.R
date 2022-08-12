# fetch data on previous seasons 
source("./init.r")

players_18_R<-get_player_details(season=18)
players_17_R<-get_player_details(season=17)
players_19_R<-get_player_details(season=19)
players_20_R<-get_player_details(season=20)
players_21_R<-get_player_details(season=21)

# fpl data 
players_19_R$season<-2019
players_20_R$season<-2020
players_21_R$season<-2021

players_history_all<-rbind(players_19_R,players_20_R,players_21_R)

# understat data 

understat_epl_results_19 <- understat_league_match_results(league = "EPL", season_start_year = 2019)
understat_epl_results_20 <- understat_league_match_results(league = "EPL", season_start_year = 2020)
understat_epl_results_21 <- understat_league_match_results(league = "EPL", season_start_year = 2021)
understat_epl_results_all<-rbind(understat_epl_results_19,understat_epl_results_20,understat_epl_results_21)

epl_teams<-unique(understat_epl_results_all$home_team)

understat_team_urls <- understat_team_meta(team_name=epl_teams)

test <- understat_team_players_stats()

understat_team_stats_breakdown(team_url = c("https://understat.com/team/Liverpool/2020", "https://understat.com/team/Manchester_City/2020"))

understat_league_season_shots(league = "EPL", season_start_year = 2020)
test <- understat_team_season_shots(team_url = "https://understat.com/team/Manchester_City/2020")
