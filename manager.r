#fetch manager history and manager choice 

library(fplscrapR)
library(dplyr)
library(doParallel)
library(data.table)
registerDoParallel(cores=8)


# fpl manager history fetch 
manager_history_list<-list()
manager_history_list<-foreach(i=1:90000) %dopar%{
  test<-get_entry_hist(i)
  test$manager_id<-i
  test
}

managers_history_all<-rbindlist(manager_history_list)


# fetch top managers in each season 

managers_history_all[manager_id==1,list(rank)]
managers_history_all$total_points<-as.numeric(managers_history_all$total_points)
managers_history_all$rank<-as.numeric(managers_history_all$rank)
managers_summary<-managers_history_all %>% group_by(manager_id) %>% summarise(mean_point=mean(total_points,na.rm=TRUE),mean_rank=mean(rank,na.rm=TRUE),
                                                                              n_season=n()) 

quantile(managers_summary$mean_rank,probs  =seq(0,1,0.05),na.rm=TRUE)


# top 5% of manager based on average rank, more than 5 seasons 

selected_manager_ids<-filter(managers_summary,mean_rank<125117,n_season>=5)$manager_id


# fetch week1 up choice of selected manager ids 
selected_manager_choice_list<-list()
selected_manager_choice_list<-foreach(i=1:length(selected_manager_ids)) %dopar%{
  id = selected_manager_ids[i]
  test = get_entry_picks(id,1)
  test$manager_id<-id
  test
}

# extract the player picks 
selected_manager_pick_list<-list()
selected_manager_pick_list<-foreach(i=1:length(selected_manager_ids))%dopar%{
  id = selected_manager_choice_list[[i]]$manager_id
  test = selected_manager_choice_list[[i]]$picks
  test$manager_id = id
  test
}
selected_manager_pick_all<-rbindlist(selected_manager_pick_list)
selected_manager_pick_all<-left_join(selected_manager_pick_all,player_ids,by="element")
selected_manager_pick_all<-arrange(selected_manager_pick_all,manager_id,position)
selected_manager_pick_list<-split(selected_manager_pick_all,by="manager_id")


# selected manager's performance
selected_manager_points<-list()

selected_manager_points<-list()
selected_manager_points<-foreach(i=1:length(selected_manager_ids))%dopar%{
  id = selected_manager_choice_list[[i]]$manager_id
  point = selected_manager_choice_list[[i]]$entry_history$points
  test<-data.frame(manager_id = id,point = point)
  test
}

selected_manager_points<-rbindlist(selected_manager_points)



