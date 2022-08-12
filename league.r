
source("./init.r")

# fetch league stats

league_id = 1014994
league_standing<-get_league(league_id)$standings$results
