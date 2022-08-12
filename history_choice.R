source("./init.r")
# anaysis on 2021/2022
# historic player analysis


# lag 
players_history_all <-arrange(players_history_all,element,round)
player_history_details<-player_history_details %>% group_by(element) %>% mutate(lag_point=lag(total_points))

# more players are transferred in because of good point in the last round
cor(player_history_details$total_points,player_history_details$lag_point,use="complete.obs")
cor(player_history_details$lag_point,player_history_details$transfers_in,use="complete.obs")
cor(player_history_details$lag_point,player_history_details$transfers_out,use="complete.obs")

# performance: 0.26 positive correlation between transfer in and point
cor(player_history_details$transfers_in,player_history_details$total_points,use="complete.obs")
cor(player_history_details$transfers_out,player_history_details$total_points,use="complete.obs")


# position analysis 
test<-select(players_2022,playername,element_type)
test<-inner_join(player_history_details,test,by="playername")

test %>% group_by(element_type) %>% summarise(mean_transfer_balance=mean(transfers_balance,na.rm=TRUE)) # most tranferred in FWD, least transferred in DEF 

test %>% group_by(element_type) %>% summarise(cor_point_lag_point = cor(total_points,lag_point,use="complete.obs")) # strongest correlation in point of this round vs last round for GK,similar for other positions 

test %>% group_by(element_type) %>% summarise(cor_in_point = cor(total_points,transfers_in,use="complete.obs")) # FWD and GK has strongest correlation in transfer in and next round point 

test %>% group_by(element_type) %>% summarise(cor_in_point = cor(lag_point,transfers_in,use="complete.obs")) # FWD and GK has strongest correlation in transfer in and next round point 
