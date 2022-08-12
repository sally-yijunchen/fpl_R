#fetch manager history and manager choice 

source("./init.r")

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



# elite manager id 

elite_manager_ids<-filter(managers_summary,mean_rank<50000,n_season>=5)$manager_id


random_manager_ids<-sample(managers_summary$manager_id,length(selected_manager_ids),replace = FALSE)
