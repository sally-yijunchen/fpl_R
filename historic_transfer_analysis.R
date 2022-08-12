source("./init.r")
# anaysis on 2021/2022
# historic transfer analysis 

# data wrangling ##################################################

player_history_details <- filter(players_history_all,season==2021)

# lead lag point 
player_history_details<-player_history_details %>% group_by(element) %>%
  mutate(lag_point_1=lag(total_points),lag_point_2 = lag(total_points,2),lag_point_3 = lag(total_points,3),
         lead_point_1 = lead(total_points,1),lead_point_2 = lead(total_points,2),lead_point_3 = lead(total_points,3))
player_history_details<-mutate(player_history_details,mean_lag_three_point=(lag_point_1+ lag_point_2 +lag_point_3)/3,
                               mean_lead_three_point=(lead_point_1+ lead_point_2 +lead_point_3)/3)
test<-select(players_2021,element,element_type,cost_level)
player_history_details<-inner_join(player_history_details,test,by="element")

# top transfer every game week 
player_history_details<-arrange(player_history_details,round,desc(transfers_in))
test <-player_history_details %>% group_by(round)  %>% slice(10)
test<-select(test,round,transfers_in)
colnames(test)[2]<-"top_10_transfer_in_boundary"
player_history_details<-left_join(player_history_details,test,by="round")

player_history_details<-arrange(player_history_details,round,desc(transfers_out))
test <-player_history_details %>% group_by(round)  %>% slice(10)
test<-select(test,round,transfers_out)
colnames(test)[2]<-"top_10_transfer_out_boundary"
player_history_details<-left_join(player_history_details,test,by="round")

player_history_details<-arrange(player_history_details,round,desc(transfers_in))
test <-player_history_details %>% group_by(round)  %>% slice(5)
test<-select(test,round,transfers_in)
colnames(test)[2]<-"top_5_transfer_in_boundary"
player_history_details<-left_join(player_history_details,test,by="round")

player_history_details<-arrange(player_history_details,round,desc(transfers_out))
test <-player_history_details %>% group_by(round)  %>% slice(5)
test<-select(test,round,transfers_out)
colnames(test)[2]<-"top_5_transfer_out_boundary"
player_history_details<-left_join(player_history_details,test,by="round")

# benckmark, average point per week per position
test<-player_history_details %>% group_by(element_type,round) %>% 
  summarise(mean_point_type_week = mean(total_points),mean_lead_three_point_type_week = mean(mean_lead_three_point))
player_history_details<-left_join(player_history_details,test,by=c("element_type","round"))

test<-player_history_details %>% group_by(element_type,round) %>% 
  summarise(mean_lag_three_point_type_week = mean(mean_lag_three_point,na.rm=TRUE))
player_history_details<-left_join(player_history_details,test,by=c("element_type","round"))

# demean: remove time trend and position effect 
player_history_details$demeaned_total_points = player_history_details$total_points-player_history_details$mean_point_type_week
player_history_details$demeaned_lead_three_point = player_history_details$mean_lead_three_point-player_history_details$mean_lead_three_point_type_week
player_history_details$demeaned_lag_three_point = player_history_details$mean_lag_three_point-player_history_details$mean_lag_three_point_type_week

# demean: remove cost effect 
test<- player_history_details %>% group_by(cost_level,element_type,round) %>% 
  summarise(demeaned_lag_three_point_type_week_cost=mean(demeaned_lag_three_point,na.rm=TRUE),demeaned_lead_three_point_type_week_cost =mean(demeaned_lead_three_point,na.rm=TRUE)) 
player_history_details<-left_join(player_history_details,test,by=c("cost_level","element_type","round"))

player_history_details$relative_lead_three_point<-player_history_details$demeaned_lead_three_point-player_history_details$demeaned_lead_three_point_type_week_cost
  

# correlational analysis for all players ##################################################################

test<-select(player_history_details,mean_lag_three_point,lag_point_1,total_points,lead_point_1,mean_lead_three_point,transfers_in,transfers_out)
test<-filter(test,is.na(mean_lag_three_point)==FALSE,is.na(mean_lead_three_point)==FALSE)
test<-test[,-1]
corrplot(cor(test),method = 'number')

# correlational analysis by position ##################################################################

par(mfrow = c(2, 2))
for(type in c("GK","DEF","MID","FWD")){
  test<-player_history_details %>% filter(element_type==type) %>%select(mean_lag_three_point,lag_point_1,total_points,lead_point_1,mean_lead_three_point,transfers_in,transfers_out)
  test<-filter(test,is.na(mean_lag_three_point)==FALSE,is.na(mean_lead_three_point)==FALSE)
  test<-test[,-1]
  recordPlot()
  corrplot(cor(test),method = 'number',title=type,mar=c(0,0,1,0))
}



# GK has strongest correlation in points, then is MID and FWD
# MID has the best cor between transfer in and future performance

# cost level analysis ##################################################################

par(mfrow = c(2, 2))
for(i in 1:3){
  test<-player_history_details %>% filter(cost_level==i) %>%select(mean_lag_three_point,lag_point_1,total_points,lead_point_1,mean_lead_three_point,transfers_in,transfers_out)
  test<-filter(test,is.na(mean_lag_three_point)==FALSE,is.na(mean_lead_three_point)==FALSE)
  test<-test[,-1]
  corrplot(cor(test),method = 'number',title=i,mar=c(0,0,1,0))
}

# most tranfer in analyis ##################################################################
par(mfrow = c(1, 2))

test<-player_history_details %>% filter(transfers_in>=top_10_transfer_in_boundary,transfers_in>0) %>%select(mean_lag_three_point,lag_point_1,total_points,lead_point_1,mean_lead_three_point,transfers_in,transfers_out)
test<-test[,-1]
test<-filter(test,is.na(mean_lag_three_point)==FALSE,is.na(mean_lead_three_point)==FALSE)
corrplot(cor(test),method = 'number',title="top 10 tranfer in each week",mar=c(0,0,1,0))

test<-player_history_details %>% filter(transfers_out>=top_10_transfer_out_boundary,transfer_out>0) %>%select(mean_lag_three_point,lag_point_1,total_points,lead_point_1,mean_lead_three_point,transfers_in,transfers_out)
test<-test[,-1]
test<-filter(test,is.na(mean_lag_three_point)==FALSE,is.na(mean_lead_three_point)==FALSE)
corrplot(cor(test),method = 'number',title="top 10 tranfer out each week",mar=c(0,0,1,0))

par(mfrow = c(1, 2))

test<-player_history_details %>% filter(transfers_in>=top_5_transfer_in_boundary) %>%select(mean_lag_three_point,lag_point_1,total_points,lead_point_1,mean_lead_three_point,transfers_in,transfers_out)
test<-test[,-1]
test<-filter(test,is.na(mean_lag_three_point)==FALSE,is.na(mean_lead_three_point)==FALSE)
corrplot(cor(test),method = 'number',title="top 5 tranfer in each week",mar=c(0,0,1,0))

test<-player_history_details %>% filter(transfers_out>=top_5_transfer_out_boundary) %>%select(mean_lag_three_point,lag_point_1,total_points,lead_point_1,mean_lead_three_point,transfers_in,transfers_out)
test<-test[,-1]
test<-filter(test,is.na(mean_lag_three_point)==FALSE,is.na(mean_lead_three_point)==FALSE)
corrplot(cor(test),method = 'number',title="top 5 tranfer out each week",mar=c(0,0,1,0))

# top transfers before and after transfer ##################################################################

player_history_details<-ungroup(player_history_details)
test_1<-player_history_details %>% filter(transfers_in>top_10_transfer_in_boundary)

test_2<-player_history_details %>% filter(transfers_in<top_10_transfer_in_boundary,transfers_in>0)
test_2<-sample_n(test_2,size=nrow(test_1))
test_3<-player_history_details %>% filter(transfers_in>top_5_transfer_in_boundary)

t.test(test_1$demeaned_lead_three_point,test_1$demeaned_lag_three_point)
t.test(test_2$demeaned_lead_three_point,test_2$demeaned_lag_three_point)
t.test(test_3$demeaned_lead_three_point,test_3$demeaned_lag_three_point)

# top transfer out 

test_1<-player_history_details %>% filter(transfers_out>top_10_transfer_out_boundary)

test_2<-player_history_details %>% filter(transfers_out<top_10_transfer_out_boundary,transfers_out>0)
test_2<-sample_n(test_2,size=nrow(test_1))
test_3<-player_history_details %>% filter(transfers_out>top_5_transfer_out_boundary)

t.test(test_1$demeaned_lead_three_point,test_1$demeaned_lag_three_point)
t.test(test_2$demeaned_lead_three_point,test_2$demeaned_lag_three_point)
t.test(test_3$demeaned_lead_three_point,test_3$demeaned_lag_three_point)

# top transfer in vs player of same cost level, use average demeaned point by cost level and position 

test_1<-player_history_details %>% filter(transfers_in>top_5_transfer_in_boundary)
t.test(test_1$demeaned_lag_three_point,test_1$demeaned_lag_three_point_type_week_cost)
t.test(test_1$demeaned_lead_three_point,test_1$demeaned_lead_three_point_type_week_cost)

# position comparison 

for(type in c("GK","DEF","MID","FWD")){
  test_1<-player_history_details %>% filter(transfers_in>top_10_transfer_in_boundary,element_type==type)
  print(type)
  print(t.test(test_1$demeaned_lag_three_point,test_1$demeaned_lag_three_point_type_week_cost))
  print(t.test(test_1$demeaned_lead_three_point,test_3$demeaned_lead_three_point_type_week_cost)
)
}

# cost level comparsion 

for(level in c(1,2,3)){
  test_1<-player_history_details %>% filter(transfers_in>top_10_transfer_in_boundary,cost_level==level)
  print(level)
  print(t.test(test_1$demeaned_lag_three_point,test_1$demeaned_lag_three_point_type_week_cost))
  print(t.test(test_1$demeaned_lead_three_point,test_3$demeaned_lead_three_point_type_week_cost)
  )
}

# histogram 
test_1<-player_history_details %>% filter(transfers_in>top_10_transfer_in_boundary)
ggplot(test_1,aes(x=relative_lead_three_point))+geom_density(aes(colour=as.factor(cost_level)))


ggplot(test_1,aes(x=relative_lead_three_point))+geom_density(aes(colour=as.factor(element_type)))



