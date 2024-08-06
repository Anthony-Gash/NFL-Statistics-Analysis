library(tidyverse)
library(ggrepel)
library(patchwork)
library(modelr)
library(stringr)
library(dplyr)

#setwd("./Desktop/Math with R/Project")

allStats <- read_csv("yearly_data_updated_08_23.csv")

#Part 1
#GRAPH 1
#filter only qbs set **minimum 8 games played**
qbStats <- allStats|>
  filter(position=='QB')|>
  select(id:season, completions:interceptions,games)|>
  filter(games>8)

#filter only rbs set
rbStats <- allStats|>
  filter(position=='RB')|>
  select(id:season, carries:rushing_fumbles)|>
  group_by(team,season)|>
  filter(row_number(desc(rushing_yards)) == 1)

#combining the rbs and qbs
rb_qb_stats<-left_join(qbStats,rbStats, by = c("team","season"))

#get the top 10 qbs by passing yards
top_10_qbStats <- allStats|>
  filter(position=='QB')|>
  select(id:season, completions:interceptions)|>
  filter(passing_yards >= 4000)|>
  filter(row_number(desc(passing_yards))<=10)

#joining the two, creating a list of 10. Top 10 qbs and their respective running back
best_qb_rb_stats <-left_join(top_10_qbStats,rbStats, by = c("team","season"))

#graph that was trying to see if there was a correlation between qb passing yards and 
#their respective best rb rushing yards. I thought that if a qb is throwing for a lot of yards the 
#rbs rushing yards would be at a low. The colored dots represnt the qbs that have had the highest passing
#yards season in the data set. As you can see during these great years the rushing yards were lower.
#This is what I thought we would see, a negative relationship betweeen the two (passing yards high
#rushing yards low and vice versa). However it is evident there is no little to no relation 
#between the two. What's also interesting is that despite playing 8 games, there are still QBs that 
#threw almost 0 yards.
rb_vs_qb_stats_graph<-ggplot()+
  geom_point(data=rb_qb_stats,mapping=aes(x=passing_yards,y=rushing_yards))+
  geom_point(data= best_qb_rb_stats, mapping=aes(x=passing_yards,y=rushing_yards, color=name.x))+
  scale_x_continuous(
    breaks=seq(0,5000,by=1000),
  )+
  labs(
    title = "QB Passing Yards vs RB Rushing Yards from 2012-2022",
    caption = "The RB rushing yards was limited to the best RB on the team that year\n 
    and QBs need a minimum 8 games played",
    x = "Passing Yards",
    y = "Rushing Yards",
    color = "QB Names",
    subtitle = "The color is the top 10 QBs by passing"
  )


rb_vs_qb_stats_graph

  







#GRAPH 2
#grabbing all wrs
wr_stats<- allStats|>
  filter(position== 'WR')|>
  select(id:season, receptions:receiving_yards)|>
  filter(receptions>40)|>
  mutate(yards_per_reception = receiving_yards/receptions)|>
  arrange(desc(yards_per_reception))

#getting the best wr teams based on average yards per reception on their recievers
top_5_teams_best_wr <- wr_stats|>
  group_by(team)|>
  summarise(avg_ypr = mean(yards_per_reception))|>
  arrange(desc(avg_ypr))|>
  filter(row_number(desc(avg_ypr))<=5)

#joining the wr to their team if they were in the top 10
wr_stats2<-left_join(top_5_teams_best_wr,wr_stats, by='team')

#This graph shows the relationship between receiving yards and receptions. As I envisioned,
#there is a positive relation between the two, meaning more receptions equals more receiving yards.
#The colored dots represent players that played on the teams that had the best average yards per 
#reception based on all their receivers. I will say I am little shocked on how low the colored dots are
#but it does make sense as every point represents a player and sometimes one player on a team can have
#a monster year while the wr do not. 
wr_stats_graph<-ggplot(data=wr_stats)+
  geom_point(mapping=aes(x=receptions, y=receiving_yards))+
  geom_point(data=wr_stats2, mapping=aes(x=receptions, y=receiving_yards, color =team))+
  scale_x_continuous(
    breaks=seq(40,150,by=20)
  )+
  labs(
    title = "Receiving Yards vs Receptions For Recievers from 2012-2022",
    subtitle = "The color represents teams with best average yards per reception",
    x = "Receptions",
    y = "Receiving Yards",
    color = "Team"
  )


wr_stats_graph









#GRAPH 3
#creating set of all the tes
te_stats<-allStats|>
  filter(position=="TE")|>
  filter(games>=8)|>
  select(id:season, receptions:receiving_tds)

#filtering out all te that don't have 70+ recpetions
te_target_stats <- te_stats|>
  filter(targets>70)

#filtering out all the te that dont have 60+ receptions
te_reception_stats<- te_target_stats|>
  filter(receptions>=60)

#filtering out all the te that dont have 800+ receiving yards
te_receiving_yards <- te_reception_stats|>
  filter(receiving_yards >=800)

#filtering out all the te that dont have 8+ receiving tds
te_td_stats<-te_receiving_yards|>
  filter(receiving_tds>=8)

#creating a bar graph to see how few te in the league are considered "elite." 
#The bottom of the graph to top of the gray represents all te in the league that have played 8
#or more games. The bottom to red represents all te that have 70+ targets and have played 8 or more games.
#The bottom to the blue represents all te that have 60+ receptions, 70+ targets, and have played
#8 or more games. The bottom to the green represents all the te that have 800+ receiving yards, 60+ 
#receptions, 70+ targets, and have played 8 or more games.The purple represents what are considered 
#"elite" te; with 8+ touchdowns, 800+ receiving yards, 60+ receptions, 70+ targets, and have played 
#8 or more games. What's interesting is that there were no "elite" te in 2016. I thought there might be
# a pattern of elite te increasing as time goes on, but there seems to be no pattern. 
ggplot()+
  geom_bar(data=te_stats, mapping=aes(x= season), color ="black")+
  geom_bar(data=te_target_stats, mapping=aes(x= season), fill = "red", color="black")+
  geom_bar(data=te_reception_stats, mapping=aes(x= season), fill = "blue", color="black",)+
  geom_bar(data=te_receiving_yards, mapping=aes(x= season), fill = "green", color="black")+
  geom_bar(data=te_td_stats, mapping=aes(x= season), fill = "purple", color="black")+
  scale_x_continuous(
    breaks=seq(2012,2022,by=1)
  )+
  labs(
    title = "Number of TE's Each Season With Increasing Filters",
    x="Season",
    y="Number of TEs",
    caption = "Red represents 70+ targets, Blue represents 60+ receptions, Green 
            represents 800+ receiving yards, Purple represents 8+ touchdowns"
  )








#GRAPH 4
#creating a set of all nfc teams
nfc_teams<-allStats|>
  filter(season==2022)|>
  filter(games>16)|>
  filter(team=="DAL" | team=="NYG" | team=="PHI" | team=="WAS" | team=="CHI" | team=="GB" | team=="MIN" | team=="DET"
         | team=="ATL" | team=="CAR" | team=="NO" | team=="TB" | team=="ARI" | team=="LA" | team=="SF" | team=="SEA")

#creating a set of all qbs in nfc
qb_nfc_teams<-nfc_teams|>
  filter(position=="QB")|>
  group_by(position)|>
  summarise(avg_pass=mean(passing_yards))

#creating a set of all rbs in nfc
rb_nfc_teams<-nfc_teams|>
  filter(position=="RB")|>
  group_by(position)|>
  summarise(avg_rush=mean(rushing_yards))

#creating a set of all wrs and tes in nfc
te_wr_nfc_teams<-nfc_teams|>
  filter(position=="WR"|position=="TE")|>
  group_by(position)|>
  summarise(avg_receiv=mean(receiving_yards))

#joining all of them, condensing to one column and adding a conference column
position_nfc<-full_join(full_join(qb_nfc_teams,rb_nfc_teams),te_wr_nfc_teams)
position_nfc2<-unite(position_nfc, avg_yards, avg_pass, avg_rush, avg_receiv, na.rm=TRUE)|>
  mutate(conference="NFC")

#setting the avg_yards to numeric and sorting by average yards
position_nfc2$avg_yards=as.numeric(position_nfc2$avg_yards)
position_nfc3<-position_nfc2|>
  arrange(avg_yards)




#creating a set of all afc teams
afc_teams <-allStats|>
  filter(season==2022)|>
  filter(games>16)|>
  filter(team=="BUF" | team=="MIA" | team=="NE" | team=="NYJ" | team=="BAL" | team=="CIN" | team=="CLE"
         | team=="PIT" | team=="HOU" | team=="IND" | team=="JAX" | team=="TEN" | team=="DEN" | team=="KC" | team=="LV" | team=="LAC")

#creating a set of all qbs in afc
qb_afc_teams<-afc_teams|>
  filter(position=="QB")|>
  group_by(position)|>
  summarise(avg_pass=mean(passing_yards))

#creating a set of all rbs in nfc
rb_afc_teams<-afc_teams|>
  filter(position=="RB")|>
  group_by(position)|>
  summarise(avg_rush=mean(rushing_yards))

#creating a set of all tes and wrs in nfc
te_wr_afc_teams<-afc_teams|>
  filter(position=="WR"|position=="TE")|>
  group_by(position)|>
  summarise(avg_receiv=mean(receiving_yards))

#joining all of them, condesing to one column and adding a conference column

position_afc<-full_join(full_join(qb_afc_teams,rb_afc_teams),te_wr_afc_teams)
position_afc2<-unite(position_afc, avg_yards, avg_pass, avg_rush, avg_receiv, na.rm=TRUE)|>
  mutate(conference="AFC")

#setting the avg_yards to numeric and sorting by average yards
position_afc2$avg_yards=as.numeric(position_afc2$avg_yards)
position_afc3<-position_afc2|>
  arrange(avg_yards)

#This graph shows the average number of yards per position in both the AFC and NFC. The red represents
#the AFC and blue represents the NFC. The goal of this graph was to see if any conference held a 
#total advantage over the yards by position. I was curious, since the NFC won the Super Bowl in 
#2022 if they also dominated the stats. This seems to not be the case as by average, the AFC 
#wins every position, except WR which it lost by a small margin. It is still important to note that 
#the difference between points is still relativley small. 
ggplot()+
  geom_point(data=position_afc3, mapping=aes(x=position, y=avg_yards, color=conference))+
  geom_line(data=position_afc3, mapping=aes(x=position, y=avg_yards,group=1,color=conference))+
  geom_point(data=position_nfc3, mapping=aes(x=position, y=avg_yards, color=conference))+
  geom_line(data=position_nfc3, mapping=aes(x=position, y=avg_yards,group=1,color=conference))+
  labs(
    title = "Average Yards Per Position in the AFC vs the NFC in 2022",
    caption="Players were selected by playing a minimum 16 games",
    x="Position",
    y="Average Yards",
    color="Conference"
  )









           
#Part 2
#Graph 1
#filtering qb stats to only include QBs who played all 16 games
qbstats2<-qbStats|>
  filter(games>=8)|>
  select(id:season, passing_yards:passing_tds)

#There was a couple outliers and the purpose of this was to see who it was, it was Taysom Hill
qbstats2|>
  arrange(passing_yards)

#Creating a model that predicts passing touchdowns based on passing yards
qbstats_mod<-lm(passing_tds~passing_yards, data=qbstats2)

#adds the residuals
qbstats3<-qbstats2|>
  add_residuals(qbstats_mod)

#creates a graph looking at passing yards vs passing touchdowns
qbstats_gg<- ggplot(data=qbstats2)+
  geom_point(mapping=aes(x=passing_yards, y=passing_tds))+
  geom_abline(intercept=qbstats_mod[[1]][[1]], 
              slope=qbstats_mod[[1]][[2]],
              color = "red")+
  labs(
    title = "Passing Yards vs Passing TDs from 2012-2022",
    subtitle = "Quarterbacks must have played a full 16 games",
    x = "Passing Yards",
    y= "Passing Touchdowns",
  )

#creates a graph looking at passing yards vs the residuals
qbstats_resid_gg <-ggplot(data=qbstats3, mapping=aes(x=passing_yards,y=resid))+
  geom_ref_line(h=0)+
  geom_point()+
  labs(
    title = "Passing Yards vs Residuals From 2012-2022",
    x = "Passing Yards",
    y= "Residuals"
  )

#displaying the graphs
#Residual is positive = QB over performed TDs based on passing yards
#I was looking to see how QBs performed on touchdowns based on predicted values using the passing 
#yards. The top graph shows the relationship between the passing yards and passing touchdowns from 2012 
#to 2022. You can see a gradual positive relationship between the two with the red line representing 
#the trend. The bottom graph represents the passing yard vs residuals in the same years. There is no
#predictability between the two. An interesting thing to note is the couple of outliers. One is 
#Brad Smith who is actually a WR but got credit as a QB for instances he played in the wildcat. Another one
#is Jacoby Brissett and the other two are Taysom Hill (who also plays multiple positions)
qbstats_gg /qbstats_resid_gg









#Graph 2
#filter only rbs that played 16 or more games and have had 100+ carries
rbstats<-allStats|>
  filter(position== "RB")|>
  filter(games>=16)|>
  filter(carries>=100)|>
  select(id:name, team:season, carries,rushing_yards:rushing_fumbles)

#creating a model that predicts rushing touchdowns based on number of carries
rb_mod <- lm(rushing_tds~carries, data=rbstats)

#filtering under performing RBs based on their residual values being less than -1
under_rbstats2<-rbstats|>
  add_residuals(rb_mod)|>
  arrange(desc(resid))|>
  filter(resid< -1)

##filtering under performing RBs based on their residual values being greater than 1
over_rbstats2 <-rbstats|>
  add_residuals(rb_mod)|>
  arrange(desc(resid))|>
  filter(resid> 1)

#This graph is divided up into 6 graphs based on fumbles in a given season. My 
#goal was to see if there was a correlation between fumbles and RBs under perfoming.
#I found under performing RBs by making a model of carries vs rushing TDs where TDs
#are predicted based on carries. If the running back had residual value of less than -1,
#I considered them to be under performing meaning their TD value was 1 or more less than 
#the predicted. I did the opposite for the over performing, filter the residual value
#if it was greater than 1. This graph shows that there isn't much of a correlation 
#between fumbles and performance. The only thing I notice is that having 5 fumbles
#puts you at more of a chance of under performing.
ggplot()+
  geom_point(data=under_rbstats2, mapping=aes(x=carries, y=rushing_tds))+
  geom_abline(intercept=rb_mod[[1]][[1]], 
              slope=rb_mod[[1]][[2]],
              color = "red")+
  geom_point(data=over_rbstats2, mapping=aes(x=carries, y=rushing_tds))+
  facet_wrap(~rushing_fumbles)+
  labs(
    title = "Carries vs Rushing TDs of Over and Under Performing Running Backs from 2012-2022",
    x = "Carries",
    y = "Rushing TDs",
    caption = "RBs needed a minimum of 16 games played and 100 carries"
  )









#Part 3
#Graph 1
ggplot()+
  geom_bar(data=te_stats, mapping=aes(x= season), color ="black")+
  geom_bar(data=te_target_stats, mapping=aes(x= season), fill = "red", color="black")+
  geom_bar(data=te_reception_stats, mapping=aes(x= season), fill = "blue", color="black",)+
  geom_bar(data=te_receiving_yards, mapping=aes(x= season), fill = "green", color="black")+
  geom_bar(data=te_td_stats, mapping=aes(x= season), fill = "purple", color="black")+
  scale_x_continuous(
    breaks=seq(2012,2022,by=1)
  )+
  labs(
    title = "Number of TE's Each Season With Increasing Filters",
    x="Season",
    y="Number of TEs",
    caption = "Red represents 70+ targets, Blue represents 60+ receptions, Green 
            represents 800+ receiving yards, Purple represents 8+ touchdowns"
  )+
  annotate("text", x = 2016, y = -2, label = "Zero Elite TEs", color = "purple", size = 2.7)



