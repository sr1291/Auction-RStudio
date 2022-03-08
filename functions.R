setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library("plyr")

source('vars.R')

read_players_list <- function(path) {
  players_list <- read.csv(file = path)  
  
  return (players_list)
}

get_sold_players <- function(players) {
  return (players[players$Sold=="Yes",])
}

get_to_be_auctioned_players <- function(players, player_set=NULL) {
  df <- players[players$Auctioned=="No",]
  if (!is.null(player_set)){
    df <- df[df$Set==player_set,]
  }
  return (df)
}

get_unsold_players <- function(players) {
  return (players[(players$Auctioned=="Yes") & (players$Sold=="No"),])
}

get_next_player <- function(path, player_set=NULL){
  players_list <- read_players_list(path)
  to_be_auctioned <- get_to_be_auctioned_players(players_list, player_set)
  
  cols <- c("Name","Team","Country","Skillset","BasePrice")
  return (to_be_auctioned[sample(nrow(to_be_auctioned), 1), cols])
  
}

get_next_bid <- function(current_bid, starting_bid) {
  if (is.na(current_bid)) {
    return (starting_bid)
  # } else if (current_bid > 9.99999) {
  #   increment <- 0.5
  } else if (current_bid > 4.99999) {
    increment <- 0.25
  } else if (current_bid > 1.99999) {
    increment <- 0.2
  } else if (current_bid > 0.49999) {
    increment <- 0.1
  } else {
    increment <- 0.05
  }

  return (current_bid + increment)
}

get_team_composition <- function(players_list) {
  #skills <- unique(players_list[!is.na(players_list$Skillset),"Skillset"])
  skills <- c('BATSMAN','BOWLER','All-ROUNDER','WICKETKEEPER','Count')
  
  df <- data.frame(row.names = teams)
  df$Balance <- startingBalance
  df[,skills] <- 0
  
  return (df)
}

# update_team_composition <- function(players_list){
#   
#   if (nrow(subset(players_list, Sold=="Yes"))==0) {
#     return (get_team_composition(players_list))
#   }
#   
#   df <- count(players_list[!is.na(players_list$Skillset),], c("Owner","Skillset"))
#   df <- reshape(df, direction = "wide", idvar = "Owner", timevar = "Skillset")
#   names(df) <- sub('freq.', '', names(df))
#   
#   df[is.na(df)] <- 0
#   df$Count <- rowSums(df[,colnames(df)[-1]])
# 
#   # nrow(players_list[players_list$Sold=="Yes","Price"])
#   sold_players <- players_list[players_list$Sold=="Yes",]
#   spent <- aggregate(sold_players$Price, by=list(Category=sold_players$Owner), FUN=sum)
#   colnames(spent) <- c("Owner","Spent")
#   spent <- join(spent, startingBalanceDf, by="Owner", type="inner")
#   spent$Balance <- spent$startingBalance - spent$Spent
#   # spent$Balance <- startingBalance - spent$Spent
#   spent <- spent[,c("Owner","Balance")]
#   
#   df_1 <- join(spent, df, by="Owner", type="inner")
# 
#   team_comp <- data.frame(Owner = teams)
#   team_comp <- join(team_comp, df_1, by="Owner", type="left")
#   if (nrow(team_comp[is.na(team_comp$Spent),]) > 0){
#     team_comp$Spent[is.na(team_comp$Spent)] <- startingBalance  
#   }
#   
#   team_comp[is.na(team_comp)] <- 0
#   
#   return (team_comp)
# }

update_team_composition <- function(players_list){
  if (nrow(subset(players_list, Sold=="Yes"))==0) {
    return (get_team_composition(players_list))
  }
  
  df <- count(players_list[!is.na(players_list$Skillset),], c("Owner","Skillset"))
  df <- reshape(df, direction = "wide", idvar = "Owner", timevar = "Skillset")
  names(df) <- sub('freq.', '', names(df))
  
  df[is.na(df)] <- 0
  df$Count <- rowSums(df[,colnames(df)[-1]])
  
  sold_players <- players_list[players_list$Sold=="Yes",]
  spent <- aggregate(sold_players$Price, by=list(Category=sold_players$Owner), FUN=sum)
  colnames(spent) <- c("Owner","Spent")
  spent <- join(startingBalanceDf, spent, by="Owner", type="left")
  
  df_1 <- join(spent, df, by="Owner", type="left")
  df_1[is.na(df_1)] <- 0
  df_1$Balance <- df_1$startingBalance - df_1$Spent
  
  df_1 <- df_1[ , !(names(df_1) %in% c('startingBalance', 'Spent'))]
  
  return (df_1)
}

# path <- '/Users/sramasub/Documents/Personal/Reco-Auction/IPL/IPL-2022/Player-List/'
# orig_filename <- 'Players-List-With-Retentions.csv'
# 
# players_list <- read_players_list(paste0(path,orig_filename))
# update_team_composition(players_list)
# 
# df <- count(players_list[!is.na(players_list$Skillset),], c("Owner","Skillset"))
# df <- reshape(df, direction = "wide", idvar = "Owner", timevar = "Skillset")
# names(df) <- sub('freq.', '', names(df))
# 
# df[is.na(df)] <- 0
# df$Count <- rowSums(df[,colnames(df)[-1]])
# 
# sold_players <- players_list[players_list$Sold=="Yes",]
# spent <- aggregate(sold_players$Price, by=list(Category=sold_players$Owner), FUN=sum)
# colnames(spent) <- c("Owner","Spent")
# spent <- join(startingBalanceDf, spent, by="Owner", type="left")
# 
# df_1 <- join(spent, df, by="Owner", type="left")
# df_1[is.na(df_1)] <- 0
# df_1$Balance <- df_1$startingBalance - df_1$Spent
# 
# df_1 <- df_1[ , !(names(df_1) %in% c('startingBalance', 'Spent'))]
# 
# spent[is.na(spent$Spent),'Spent'] <- 0
# spent$Balance <- spent$startingBalance - spent$Spent
# spent <- spent[,c("Owner","Balance")]
# 
# 
# update_team_composition(players_list)
# 
# print (startingBalance)

# get_team_composition(players_list)
# df <- players_list[!is.na(players_list$Skillset),]

# get_team_composition <- function(){
#   skills <- c('Balance','Batsman','Bowler','All Rounder', 'WK', 'Count')
#   df <- data.frame(row.names = teams)
#   df[,skills] <- 0
#   # df[,'Spent'] <- 0
#   df[,'Balance'] <- startingBalance
# 
#   return (df)
# }

# update_team_composition <- function(team_comp, player, bidder, bidPrice){
#   
#   team_comp[bidder, player$Skillset] <- team_comp[bidder, player$Skillset] + 1
#   team_comp[bidder, 'Count'] <- team_comp[bidder, 'Count'] + 1
#   team_comp[bidder, 'Balance'] <- team_comp[bidder, 'Balance'] - bidPrice
# 
#   return (team_comp)
# }

append_prev_player <- function(curr_player, prev_players){
  cols <- c("Name","Team","Country","Skillset","Owner","Price")
  if (nrow(prev_players)==0){
    prev_players <- curr_player[,cols]
  } else {
    prev_players <- rbind(curr_player[,cols],prev_players)
  }
  prev_players <- prev_players[c(1:min(nrow(prev_players), 3)),]
  
  return (prev_players)
}

add_action_to_log <- function(auction_log, counter_log, playerNum, playerName, actionNum, 
                              action, bidder, time, soldPrice) {
  
  cols <- c('PlayerNum','Name','ActionNum','Action','Bidder','Time','SoldPrice')
  df <- data.frame(playerNum, playerName, actionNum, 
                   action, bidder, time, soldPrice)
  names(df) <- cols
  
  auction_log <- append_action_df(df, auction_log)  
  counter_log_text <- get_counter_log_text(playerName, action, bidder, soldPrice)
  counter_log <- append_action_df(counter_log_text, counter_log)
  
  return (list(auction_log, counter_log))
}

get_counter_log_text <- function(playerName, action, bidder, soldPrice) {
  
  if (action=="Revealed"){
    txt <- paste("Next player:",playerName)
  } else if (action=="Bid"){
    txt <- paste(bidder,"bids",soldPrice)
  } else if (action=="Sold"){
    txt <- paste(playerName, "sold to", bidder, "for", soldPrice)
  } else { #  if (action=="Unsold")
    txt <- paste(playerName, "unsold")
  }
  action_text <- data.frame(Log=txt)

  return (action_text)
}

append_action_df <- function(action, counter){
  if (nrow(counter) == 0){
    counter <- action
    
  } else {
    counter <- rbind(action,counter)
  }
  return (counter)
}


add_num <- function(a,b,c){
  return (a+c)
}

add_num(10,NA,20)
