library(fplscrapR)
library(dplyr)
library(doParallel)
library(data.table)
library(reshape2)
registerDoParallel(cores=8)


test<-select(players_2022,element,element_type,now_cost,cost_level)

selected_manager_pick_all <- left_join(selected_manager_pick_all,test,by="element")


na_player<- filter(selected_manager_pick_all,is.na(now_cost)==TRUE) %$% unique(.$element)


# top manager choice anaylis

## most selected 
test<-selected_manager_pick_all %>% group_by(element,element_type,playername) %>% summarise(count=n(),captain=sum(is_captain)) %>% arrange(desc(count))
test$percentage<-test$count/n_distinct(selected_manager_pick_all$manager_id)

# most selected by each position 

test<-test %>% group_by(element_type) %>% arrange(desc(percentage))
test <-arrange(test,element_type)
test %>% filter(percentage>=0.02) %>% View(.)


## money_spent on each position 

test<-selected_manager_pick_all %>% filter(! element %in%na_player ) %>%group_by(manager_id,element_type) %>% summarise(total_spent=sum(now_cost,na.rm=TRUE))

test %>% filter(element_type=="MID") %$% hist(.$total_spent)
test %>% filter(element_type=="DEF") %$% hist(.$total_spent)
#test %>% filter(element_type=="GK") %$% hist(.$total_spent)
test %>% filter(element_type=="FWD") %$% hist(.$total_spent)
test %>% filter(element_type=="FWD") %>% View(.)

test_1<-filter(test,element_type=="DEF")
test_2<-filter(test,element_type=="MID")
colnames(test_1)[3]<-"total_spent_def"
colnames(test_2)[3]<-"total_spent_mid"
test_1<-test_1[,-2]
test_2<-test_2[,-2]
test_1<-inner_join(test_1,test_2,by="manager_id")
plot(test_1$total_spent_def,test_1$total_spent_mid)


test<-selected_manager_pick_all %>% filter(element_type=="FWD") %>% group_by(manager_id) %>% summarise(sum_element = sum(element,na.rm=TRUE))


