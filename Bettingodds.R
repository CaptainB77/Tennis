library(remotes)
library(dplyr)
library(data.table)
library(tidyverse)
library(ggplot2)
library(reshape2)


#Create table of agencies score and season to get to know the data.
agencies = Data [,12:55]
rank_agencies <- select(agencies, -3:-15)
rank_agencies2 <- select(rank_agencies, -27:-30) 
rank_agencies2[3:20] <- apply(rank_agencies2[3:20], 2, round, 0)

rankagencies = rank_agencies2 [,1:27]


#Let's create columns with the wins and loses of each company

#CB Win and lose odds
rankagencies$CBW_Win_Odds <- ifelse(rankagencies$WRank == rankagencies$CBW, 1, 0)
rankagencies$CB_Lose_Odds <- ifelse(rankagencies$LRank == rankagencies$CBL, 1, 0)

#GB Win and lose odds
rankagencies$GB_Win_Odds <- ifelse(rankagencies$WRank == rankagencies$GBW, 1, 0)
rankagencies$GB_Lose_Odds <- ifelse(rankagencies$LRank == rankagencies$GBL, 1, 0)

#IW Win and lose odds
rankagencies$IW_Win_Odds <- ifelse(rankagencies$WRank == rankagencies$IWW, 1, 0)
rankagencies$IW_Lose_Odds <- ifelse(rankagencies$LRank == rankagencies$IWL, 1, 0)

#SB Win and lose odds
rankagencies$SB_Win_Odds <- ifelse(rankagencies$WRank == rankagencies$SBW, 1, 0)
rankagencies$SB_Lose_Odds <- ifelse(rankagencies$LRank == rankagencies$SBL, 1, 0)

#B365 Win and lose odds
rankagencies$B365_Win_Odds <- ifelse(rankagencies$WRank == rankagencies$B365W, 1, 0)
rankagencies$B365_Lose_Odds <- ifelse(rankagencies$LRank == rankagencies$B365L, 1, 0)

#EX Win and lose odds
rankagencies$EX_Win_Odds <- ifelse(rankagencies$WRank == rankagencies$EXW, 1, 0)
rankagencies$EX_Lose_Odds <- ifelse(rankagencies$LRank == rankagencies$EXL, 1, 0)

#PS Win and lose odds
rankagencies$PS_Win_Odds <- ifelse(rankagencies$WRank == rankagencies$PSW, 1, 0)
rankagencies$PS_Lose_Odds <- ifelse(rankagencies$LRank == rankagencies$PSL, 1, 0)

#UB Win and lose odds
rankagencies$UB_Win_Odds <- ifelse(rankagencies$WRank == rankagencies$UBW, 1, 0)
rankagencies$UB_Lose_Odds <- ifelse(rankagencies$LRank == rankagencies$UBL, 1, 0)

#LB Win and lose odds
rankagencies$LB_Win_Odds <- ifelse(rankagencies$WRank == rankagencies$LBW, 1, 0)
rankagencies$LB_Lose_Odds <- ifelse(rankagencies$LRank == rankagencies$LBL, 1, 0)

#SJ Win and lose odds
rankagencies$SJ_Win_Odds <- ifelse(rankagencies$WRank == rankagencies$SJW, 1, 0)
rankagencies$SJ_Lose_Odds <- ifelse(rankagencies$LRank == rankagencies$SJL, 1, 0)

#Let's see then how the agencies  score 

summary(rankagencies)
scores = rankagencies [,27:47]

scores %>% 
  group_by(Season) %>%
  summarize(count = n())
  

#Now let's create a table for each cateforia "win ods" and "loser ods"

Win_Ods = scores %>% 
  select(Season, ends_with("Win_Odds")) 

Loser_Ods = scores %>% 
  select(Season, ends_with("Lose_Odds")) 

summary(Win_Ods)

TotalWins = Win_Ods %>%
  group_by(Season) %>%
  summarize_all(funs(sum(., na.rm = TRUE)))

TotalLose = TotalLose = Loser_Ods %>%
  group_by(Season) %>%
  summarize_all(funs(sum(., na.rm = TRUE)))

summary(Loser_Ods)

TotL = TotalLose %>% 
  summarize_all(funs(sum(., na.rm = TRUE)))

#It's time to graphic that
#Wins

Win_Ods_long <- melt(Win_Ods, id.vars = "Season")

ggplot(Win_Ods_long, aes(x = Season, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  ggtitle("Win Odds by Year") +
  xlab("Year") +
  ylab("Win Odds") +
  theme(legend.title=element_blank())

#Lose

Lose_Ods_long <- melt(Loser_Ods, id.vars = "Season")

ggplot(Lose_Ods_long, aes(x = Season, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  ggtitle("Lose Odds by Year") +
  xlab("Year") +
  ylab("Lose Odds") +
  theme(legend.title=element_blank())


#Let's take a better look to our top3 agencies

#WinOdds
Top3W = TotalWins %>% 
  select(Season, B365_Win_Odds,EX_Win_Odds,PS_Win_Odds)

Top3W_long <- melt(Top3W, id.vars = "Season")

ggplot(Top3W_long, aes(x = Season, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  ggtitle("Win Odds by Year") +
  xlab("Year") +
  ylab("Win Odds") +
  theme(legend.title=element_blank())

Winss = Win_Ods %>%
  summarize_all(funs(sum(!is.na(.))))

Winss$Season<-NULL

Winss_long <- Winss %>% 
  gather(key = "columns", value = "value") 

ggplot(Winss_long, aes(x = columns, y = value)) + geom_col(fill = "blue")+
  theme(axis.text.x = element_text(angle = 45))


#LoseOdds

Top3L = TotalLose %>% 
  select(Season, B365_Lose_Odds,EX_Lose_Odds,PS_Lose_Odds)

Top3L_long <- melt(Top3L, id.vars = "Season")

ggplot(Top3L_long, aes(x = Season, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  ggtitle("Lose Odds by Year") +
  xlab("Year") +
  ylab("Lose Odds") +
  theme(legend.title=element_blank())


Loose = TotalLose %>%
  summarize_all(funs(sum(!is.na(.))))

Loose$Season<-NULL

Winss_long <- Winss %>% 
  gather(key = "columns", value = "value") 

ggplot(Winss_long, aes(x = columns, y = value)) + geom_col(fill = "blue")+
  theme(axis.text.x = element_text(angle = 45))

