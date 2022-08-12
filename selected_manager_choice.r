source("./init.r")
# analysis of top mananger choice in 2022/2023

# ##########weekly manager choice fetch #########################

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

####################################################################################################


test<-select(players_2022,element,element_type,now_cost,cost_level)

selected_manager_pick_all <- left_join(selected_manager_pick_all,test,by="element")


na_player<- filter(selected_manager_pick_all,is.na(now_cost)==TRUE) %$% unique(.$element)


# top manager choice analysis 

## most selected 
test<-selected_manager_pick_all %>% group_by(element,element_type,playername) %>% summarise(count=n(),captain=sum(is_captain)) %>% arrange(desc(count))
test$percentage<-test$count/length(selected_manager_ids)

# most selected by each position 

test<-test %>% group_by(element_type) %>% arrange(desc(percentage))
test <-arrange(test,element_type)
test %>% filter(percentage>=0.02) %>% View(.)


## money_spent on each position 

test<-selected_manager_pick_all %>% filter(! element %in%na_player ) %>%group_by(manager_id,element_type) %>% summarise(total_spent=sum(now_cost,na.rm=TRUE))

test %>% filter(element_type=="MID") %$% hist(.$total_spent)
test %>% filter(element_type=="DEF") %$% hist(.$total_spent)
test %>% filter(element_type=="GK") %$% hist(.$total_spent)
test %>% filter(element_type=="FWD") %$% hist(.$total_spent)
test %>% filter(element_type=="FWD") %>% View(.)


# elite managers choice analyis
test<-selected_manager_pick_all %>% filter(manager_id %in% elite_manager_ids) %>% group_by(element,element_type,playername) %>% summarise(count=n(),captain=sum(is_captain)) 
test$percentage<-test$count/length(elite_manager_ids)
test1<-test %>% arrange(element_type,desc(percentage))

# point analysis 
summary(selected_manager_points$point)

selected_manager_points %>% filter(manager_id %in% elite_manager_ids) %$% summary(.$point)

# elite managers who do not choose robertson 
test<- selected_manager_pick_all %>% group_by(manager_id) %>% summarise(is_robertson = ifelse(284 %in% element,1,0))
test<-filter(test,manager_id %in% elite_manager_ids,is_robertson==0)

test<-selected_manager_pick_all %>% filter(manager_id %in% test$manager_id,position %in% c(1:11)) %>% group_by(element,element_type,playername) %>% summarise(count=n(),captain=sum(is_captain)) 
test$percentage<-test$count/69
test1<-test %>% arrange(element_type,desc(percentage))

# perisic 

test1<-selected_manager_pick_all %>% filter(element == 448, !position %in% c(1:11))
test1<-unique(test1$manager_id)

test<-selected_manager_pick_all%>% filter(manager_id %in% test1, !position %in% c(12:15)) %>% group_by(element,element_type,playername) %>% summarise(count=n(),captain=sum(is_captain)) 
test$percentage<-test$count/length(test1)
test<-test %>% arrange(element_type,desc(percentage))
