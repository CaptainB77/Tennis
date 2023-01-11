library(dplyr)
library(RSQLite)
library(grid)  
library(gridExtra) 
library(ggExtra)
library(cowplot)
library(tidyverse)
library(ggvis)
library(shiny)
library(DT)
library(stringr)
library(knitr)
library(kableExtra)
library(readr)

library(readr)
Data <- read_csv("Data.csv")
View(Data)

str(Data)
summary(Data)

#Converting the Rank variables into numeric variables
Data$LRank=as.numeric(Data$LRank)
Data$LPts=as.numeric(Data$LPts)

#Formatting Date to extract Season Info
Data$Date=as.Date(Data$Date,format='%d/%m/%Y')
Data$Season=as.numeric(format(Data$Date,"%Y"))

Data %>% 
  mutate(Winner = gsub(" $", "", Winner), Loser = gsub(" $", "", Loser))
summary(Data)

#Top20 cities played

fillColor = "#FFA07A"
  fillColor2 = "#F1C40F"
    
  Data %>%
    group_by(Location) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    ungroup() %>%
    mutate(Location = reorder(Location,Count)) %>%
    head(20) %>%
    
    ggplot(aes(x = Location,y = Count)) +
    geom_bar(stat='identity',colour="white", fill = fillColor) +
    geom_text(aes(x = Location, y = 1, label = paste0("(",Count,")",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'Country', 
         y = 'Count', 
         title = 'Country and Count') +
    coord_flip() +
    theme_bw()
  


#First lets take a look at the players:

#Winners
winners = Data %>% 
  group_by(Surface, Winner) %>% 
  summarise(nWin = n()) 

#Losers
losers= Data %>% 
  group_by(Surface, Loser) %>% 
  summarise(nLose = n()) 

#Format to the new tables
colnames(winners)[2] = "Name"
colnames(losers)[2] = "Name"

#Let's take de percent of winners against losers
players_bySurface = merge(winners, losers, by = c("Surface", "Name"), all = T) %>% 
  mutate(nWin = ifelse(is.na(nWin), 0, nWin), nLose = ifelse(is.na(nLose), 0, nLose), winPerc = nWin/(nWin+nLose)) %>% 
  arrange(desc(winPerc))

#Taking some format of the percet of each player
players_overall = data.frame(players_bySurface %>% 
                               group_by(Name) %>% 
                               summarise(nWin = sum(nWin), nLose = sum(nLose)) %>% 
                               mutate(winPerc = paste(round(nWin/(nWin+nLose)*100,2),"%",sep="")) %>% 
                               filter(nWin+nLose>100) %>% 
                               arrange(desc(winPerc)))


#Let's see our table, to have a better knowledge of what are we doing.
players_overall %>% 
  select(Name, nWin, nLose, winPerc) %>% 
  datatable(., options = list(pageLength = 10))

colnames(players_overall)

#Let's see players who player more than 100 games
df100<- players_overall %>% 
  filter(nWin > 100) %>% 
  arrange(desc(nWin)) %>% 
  as.data.frame() 
  
#Let's take the top 20 of the players
top20 <- df100 %>% 
    arrange(desc(winPerc)) %>% 
    head(n = 20) %>%
    as.data.frame()

top20 %>% 
  ggplot(aes(y = Name , x = nWin , fill = nWin )) +
  geom_bar(stat="identity",position=position_dodge(), alpha = 0.8) + theme_minimal() + 
  scale_fill_gradient(low="#4f908c",high="#6e0ff9") +  theme(legend.position="none")+
  geom_text(aes(label= nWin), hjust= -0.2)


#Let's create a heat map of our top 20 of the players and the surfaces
ggplot(data.frame(players_bySurface %>% 
                    filter (Name %in% players_overall[1:20, "Name"] & Surface != "Carpet")), aes(x = Surface, y = Name)) +
  geom_raster(aes(fill = winPerc))+
  labs(x = "Surface", y = "Name")+
  scale_fill_gradientn(colours=c("#cc0000","#eeeeee","#0000ff"))

