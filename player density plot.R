# This script generates action density plots for a single player
library(StatsBombR)
library(tidyverse)
library(png)
library(grid)
library(ggpubr)

source('createPitchETM.R') # my modified pich plot function to allow faceting

#Get Data
Comp=FreeCompetitions() %>% 
  filter(competition_id==11)
Matches <- FreeMatches(Comp)

#uncomment this in order to pull the data from StatsBomb's API and save locally
# StatsBombData <- StatsBombFreeEvents(MatchesDF = (Matches), Parallel = T)
# StatsBombData = allclean(StatsBombData) 
#saveRDS(StatsBombData, "./sb_messi.rds")

#read if you have already saved the data, this saves time
StatsBombData= readRDS("./sb_messi.rds")

StatsBombData=left_join(StatsBombData, Matches %>% select(match_id, match_date, season.season_id, season.season_name)) %>% 
  mutate(key = paste(season.season_name, team.name, player.name))

#data frame to search for player names
players=data.frame(player=unique(StatsBombData$player.name))

#select the player you want to plot
player.select="Lionel Andrés Messi Cuccittini"  

#plot data
m=createPitch(data=StatsBombData %>% filter(player.name == player.select) %>% 
                mutate(in.possession= ifelse(possession_team.name==team.name, "In Possession", "Out of Possession")),
                xmax=120)+
  stat_density_2d(aes(x=location.x, y=location.y, color=in.possession, alpha = ..level..))+
  scale_color_manual(values=c("#004d98", "#A50044"))+
  coord_flip()+
  theme(text=element_text(size=14), legend.position = "none", aspect.ratio = 120/80,
        plot.title = element_text(hjust=.5))+
  labs(title=paste0(player.select, "\nDensity of Actions"))+
  facet_grid(in.possession~season.season_name)

#plot a footer to plot statsbomb logo and twitter handle to bottom  
footer=grobTree(textGrob("@etmckinley",
                                     x = 0.03, y=.3,vjust=0, hjust = 0, gp = gpar(fontsize=8)),
                      rasterGrob(readPNG("statsbomb logo.png"), x = 0.99, hjust=1))

#arrange
m.out=ggarrange(m,footer,
                ncol=1,
                nrow=2,
                heights=c(1, 0.05))

#save the plot, you may have to adjust the width for different numbers of seasons
ggsave(m.out, file= paste0(player.select, "-seasons-all.png"), width=15, height=5)
