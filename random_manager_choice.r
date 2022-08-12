# benchmark random manager set 

source("./init.r")

# extract the player picks 
random_manager_pick_list<-list()
random_manager_pick_list<-foreach(i=1:length(random_manager_ids))%dopar%{
  id = random_manager_choice_list[[i]]$manager_id
  test = random_manager_choice_list[[i]]$picks
  test$manager_id = id
  test
}

random_manager_pick_all<-rbindlist(random_manager_pick_list)
random_manager_pick_all<-left_join(random_manager_pick_all,player_ids,by="element")
random_manager_pick_all<-arrange(random_manager_pick_all,manager_id,position)
random_manager_pick_list<-split(random_manager_pick_all,by="manager_id")


# random manager's performance
random_manager_points<-list()

random_manager_points<-list()
random_manager_points<-foreach(i=1:length(random_manager_ids))%dopar%{
  id = random_manager_choice_list[[i]]$manager_id
  point = random_manager_choice_list[[i]]$entry_history$points
  test<-data.frame(manager_id = id,point = point)
  test
}

random_manager_points<-rbindlist(random_manager_points)

####################################################################################################


test<-select(players_2022,element,element_type,now_cost,cost_level)

random_manager_pick_all <- left_join(random_manager_pick_all,test,by="element")


na_player<- filter(random_manager_pick_all,is.na(now_cost)==TRUE) %$% unique(.$element)


# top manager choice analysis 

## most random 
test<-random_manager_pick_all %>% group_by(element,element_type,playername) %>% summarise(count=n(),captain=sum(is_captain)) %>% arrange(desc(count))
test$percentage<-test$count/length(random_manager_ids)

# most random by each position 

test<-test %>% group_by(element_type) %>% arrange(desc(percentage))
test <-arrange(test,element_type)
test2<-test %>% filter(percentage>=0.02)


## money_spent on each position 

test<-random_manager_pick_all %>% filter(! element %in%na_player ) %>%group_by(manager_id,element_type) %>% summarise(total_spent=sum(now_cost,na.rm=TRUE))

test %>% filter(element_type=="MID") %$% hist(.$total_spent)
test %>% filter(element_type=="DEF") %$% hist(.$total_spent)
test %>% filter(element_type=="GK") %$% hist(.$total_spent)
test %>% filter(element_type=="FWD") %$% hist(.$total_spent)
test %>% filter(element_type=="FWD") %>% View(.)

# point analysis 

# point analysis 
summary(random_manager_points$point)

