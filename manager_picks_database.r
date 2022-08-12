# store a historical manager_picks database for future use 

source("./init.r")


# manager choice list for each game week 
selected_manager_choice_list_all<-list()
random_manager_choice_list_all<-list()

gw = 1

# fetch gw choice of selected manager ids 
selected_manager_choice_list<-list()
selected_manager_choice_list<-foreach(i=1:length(selected_manager_ids)) %dopar%{
  id = selected_manager_ids[i]
  test = get_entry_picks(id,gw)
  test$manager_id<-id
  test
}


# fetch gw up choice of random manager ids 
random_manager_choice_list<-list()
random_manager_choice_list<-foreach(i=1:length(random_manager_ids)) %dopar%{
  id = random_manager_ids[i]
  test = get_entry_picks(id,gw)
  test$manager_id<-id
  test
}


# run after point is calculated 
selected_manager_choice_list_all<-append(selected_manager_choice_list_all,selected_manager_choice_list)
random_manager_choice_list_all<-append(random_manager_choice_list_all,random_manager_choice_list)



