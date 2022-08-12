# fetch data on previous seasons 
source("./init.r")

players_18<-get_player_details(season=18)
players_17<-get_player_details(season=17)
players_19<-get_player_details(season=19)
players_20<-get_player_details(season=20)
players_21<-get_player_details(season=21)

# fpl data 
players_history_all<-rbind(players_19,players_20,players_21)

# understat data 
