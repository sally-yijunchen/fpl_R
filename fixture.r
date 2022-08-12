# fetch fixture of current season 
source("./init.r")


fdr <- get_fdr()
gamelist <- get_game_list()


fdrfixtures <- rbind(
  gamelist %>% mutate(team=home,oppo=away,homeaway="home"),
  gamelist %>% mutate(team=away,oppo=tolower(home),homeaway="away"))


for (i in 1:nrow(fdrfixtures)){
  ifelse(fdrfixtures$homeaway[i]=="home",
         fdrfixtures$fdr[i] <- fdr$strength_overall_away[which(fdr$short_name==toupper(fdrfixtures$oppo[i]))],
         fdrfixtures$fdr[i] <- fdr$strength_overall_home[which(fdr$short_name==toupper(fdrfixtures$oppo[i]))])
}

fdrfixtures %>%
  filter(GW %in% 1:15) %>%  # filtering for the gameweeks we are interested in
  ggplot() +
  geom_tile(aes(x=GW,y=team,fill=fdr),colour="lightgrey") +
  geom_text(aes(x=GW,y=team,label=oppo),size=2) +
  theme_void() +
  theme(axis.text = element_text(face = "bold")) +
  theme(axis.text.y = element_text(margin=margin(0,-20,0,0))) + # fixing the margins
  scale_x_continuous(position="top",breaks=1:15) +
  labs(caption=paste("Data from fplscrapR | ",Sys.Date(),sep="")) +
  scale_fill_gradient2(guide=F,low="#00FF87",mid="#D6DCD8",high="#7F002D",midpoint=median(fdrfixtures$fdr))