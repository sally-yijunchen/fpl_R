source("./init.r")
# get general statistics for player 
# player ids 

player_ids<-get_player_id()
colnames(player_ids)[2]<-"element"


# external data for players info 

players_2022 <- read_csv("data/cleaned_players_2022.csv")
players_2022<-distinct(players_2022)
players_2022$playername<-paste(players_2022$first_name,players_2022$second_name,sep=" ")
players_2022<-relocate(players_2022,playername)
players_2022<-select(players_2022,-c("first_name","second_name"))
players_2022<-filter(players_2022,!playername=="Ben Davies")
player_idlist_2022 <- read_csv("data/player_idlist_2022.csv")
player_idlist_2022$playername<-paste(player_idlist_2022$first_name,player_idlist_2022$second_name,sep=" ")
player_idlist_2022<-select(player_idlist_2022,-c("first_name","second_name"))
player_idlist_2022<-filter(player_idlist_2022,!playername=="Ben Davies")
players_2022<-left_join(players_2022,player_idlist_2022,by="playername")
players_2022<-relocate(players_2022,id)
colnames(players_2022)[1]<-"element"
rm(player_idlist_2022)

players_2022 %>% filter(element_type=="DEF") %$% summary(.$now_cost)
players_2022 %>% filter(element_type=="GK") %$% summary(.$now_cost)
players_2022 %>% filter(element_type=="MID") %$% summary(.$now_cost)
players_2022 %>% filter(element_type=="FWD") %$% summary(.$now_cost)


players_2022$cost_level<-1
players_2022$cost_level[players_2022$element_type=="DEF"&players_2022$now_cost>=55]<-2
players_2022$cost_level[players_2022$element_type=="DEF"&players_2022$now_cost>=65]<-3

players_2022$cost_level[players_2022$element_type=="GK"&players_2022$now_cost>=45]<-2
players_2022$cost_level[players_2022$element_type=="GK"&players_2022$now_cost>=50]<-3


players_2022$cost_level[players_2022$element_type=="MID"&players_2022$now_cost>=65]<-2
players_2022$cost_level[players_2022$element_type=="MID"&players_2022$now_cost>=85]<-3

players_2022$cost_level[players_2022$element_type=="FWD"&players_2022$now_cost>=70]<-2
players_2022$cost_level[players_2022$element_type=="FWD"&players_2022$now_cost>=90]<-3

players_2022 %>% group_by(element_type,cost_level) %>% summarise(count=n(),mean_cost = mean(now_cost)) %>% View(.)

## year 2021
players_2021 <- read.csv("data/cleaned_players_2021.csv")
players_2021<-distinct(players_2021)
players_2021$playername<-paste(players_2021$first_name,players_2021$second_name,sep=" ")
players_2021<-relocate(players_2021,playername)
players_2021<-select(players_2021,-c("first_name","second_name"))
players_2021<-filter(players_2021,!playername=="Ben Davies")
player_idlist_2021 <- read.csv("data/player_idlist_2021.csv")
player_idlist_2021$playername<-paste(player_idlist_2021$first_name,player_idlist_2021$second_name,sep=" ")
player_idlist_2021<-select(player_idlist_2021,-c("first_name","second_name"))
player_idlist_2021<-filter(player_idlist_2021,!playername=="Ben Davies")
players_2021<-left_join(players_2021,player_idlist_2021,by="playername")
players_2021<-relocate(players_2021,id)
colnames(players_2021)[1]<-"element"
rm(player_idlist_2021)


players_2021$cost_level<-1
players_2021$cost_level[players_2021$element_type=="DEF"&players_2021$now_cost>=55]<-2
players_2021$cost_level[players_2021$element_type=="DEF"&players_2021$now_cost>=65]<-3

players_2021$cost_level[players_2021$element_type=="GK"&players_2021$now_cost>=45]<-2
players_2021$cost_level[players_2021$element_type=="GK"&players_2021$now_cost>=50]<-3


players_2021$cost_level[players_2021$element_type=="MID"&players_2021$now_cost>=65]<-2
players_2021$cost_level[players_2021$element_type=="MID"&players_2021$now_cost>=85]<-3

players_2021$cost_level[players_2021$element_type=="FWD"&players_2021$now_cost>=70]<-2
players_2021$cost_level[players_2021$element_type=="FWD"&players_2021$now_cost>=90]<-3

