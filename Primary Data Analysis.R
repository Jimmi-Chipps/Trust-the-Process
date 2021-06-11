#Import Data
library(readxl)
DraftData <- read_excel("DraftData.xlsx")
Records <- read_excel("Records.xlsx")
Pick_Values <- read_excel("Pick Values.xlsx")
  # Taken from:
    # http://www.tankathon.com/pick_odds#

#Libraries
library(tidyverse)
library(readxl)

####New Dataset for Chart####

#List of teams
teams <- DraftData$TEAM %>% sort() %>% unique()

#List of Years Observed#
Records <- Records[,1:(ncol(Records)-1)]
years <- Records[,2:ncol(Records)] %>% colnames()

#Finish The Matrix
m <- matrix(ncol = length(years) + 1,nrow = length(teams))
df <- as.data.frame(m)
colnames(df) <- c("Team", 2006:2020)
df$Team <- teams

#Clean Up
rm(m)

#Make a copy of the dataframe, one for team draft values, one for number of wins#
df_Value <- df
df_Wins <- df


####Function "draft score" for a year####
Year_Draft_Score<- function(year,team){
  
  #Establish Variables
  draft_value <- 0
  
#Filter DraftData by Team#
  temp_df <- filter(DraftData,TEAM == team)
  
#Filter DraftData by YEAR#
  temp_df <- filter(temp_df,YEAR == year)
  
#Take OVERALL PICK that are remaining#
  temp_picks <- temp_df[ ,7] %>% pull()
  
#Create Temp Value, Filter for Pick_Values$1,, add up pick values#
  temp_value_list <- filter(Pick_Values,`1` %in% temp_picks)
  draft_value <- temp_value_list$`100` %>% sum()
  return(draft_value)
#Drop that value into the correct df_Value col#
}

####Function that gets draft score for one team for each year####
builtDF = as.data.frame("")
Wins_Fill_Year <- function(team,yearlist=years%>%sort()){

#Initialize DF that will be built in function#
   builderDF <- df[1, ]
#Loop for one team for each year: Year_Draft_Score
   newlist=as.vector("")
   newlist[1] = team
   x=2
   for (year in yearlist) {
     temp_score <- Year_Draft_Score(year=year,team = team)
     newlist[x] <- temp_score
     x=x+1
  }
    builderDF <- rbind(builderDF,newlist)
    builderDF <- builderDF[2, ]
  if (ncol(builtDF) == 1){
    builtDF <<- builderDF
  }  else {
    builtDF <<- rbind(builderDF,builtDF)
  }
    rm(x,newlist)
    

}

####For Loop, Wins_Fill_Year for each team####
for (t in teams) {
  Wins_Fill_Year(team=t)
}
#Order Rows Correctly#
builtDF <- builtDF[order(builtDF$Team) ,]

#10 less games in 2020, out of 82, so 82/72 * 2020wins#
short_season_multiplier <- 82/72
Records$`2020Fixed` <- Records$`2020` * short_season_multiplier

#Reorder Records Cols to match order or builtDF
Records <- Records[ ,c(1,17:2)]
Records <- Records[, c(2:16,1)]

####Establish Draft Score Agg####
  #Draft Agg 2020 = .2*DS_2015 + .3*DS_2016 + .3*DS_2017 + .2*DS_2018#
scorer_agg <- function(team,year){
  
  #Calculate relevant years and convert to strings#
  rel_years <- ((year-5):(year-2)) %>% as.character()
  
  #Select row for team and filter col for year#
  df_temp <- filter(builtDF,Team == team)
  df_temp <- df_temp %>% select(rel_years)
  
  #Turn remaining values into list#
  df_list<-df_temp %>% as.numeric() %>% as.vector()
  
  #Apply weighting formula#
  df_list[1] <- df_list[1] * 0.2
  df_list[2] <- df_list[2] * 0.3
  df_list[3] <- df_list[3] * 0.3
  df_list[4] <- df_list[4] * 0.2
  
  #Sum of all years left#
  return_score <- df_list %>% as.numeric() %>% sum()
  
  #Return calculated Draft Score#
  return(return_score)
  
  }

#Check if it worked#
scorer_agg("Portland Trail Blazers", 2011)

####Loop scoerer_agg for each team for each year####

  for (team in teamlist) {
    for (year in 2011:2021) {
      df_Value [which(df_Value$Team==team) ,which(colnames(df_Value)==year %>% as.character) ] <- scorer_agg(team,year)
  }
  }

#Cor() between both Matrices#
cor_list = c("")
for (i in 7:16) {
corel <- cor(as.vector(df_Value[ ,i]),as.vector(Records[ ,i]))
cor_list<-c(cor_list,corel)
 
}

#Is there a corr?#
mean(cor_list[2:length(cor_list)] %>% as.numeric())

#Test cor for atlanta#
atlRecord <-Records[1,7:16] %>% as.numeric()
atlDraft <-df_Value[1,7:16] %>% as.numeric()
cor.test(atlRecord,atlDraft)

#Cor Test Loop#
cors <- ""
for (row in 1:30) {
  temp_cor<-cor(Records[row,7:16] %>% as.numeric(), df_Value[row,7:16] %>% as.numeric())
  cors<-c(cors,temp_cor)
}
cors<-cors[2:31]
cors%>%as.numeric() %>% mean()

#Create a wins col#
for (i in 1:nrow(df_Value)){
  df_Value$totalwins[i] <- sum(Records[i,3:ncol(Records)-1] %>% as.numeric)
}
Cor_Wins_List<-cbind(df$Team,cors,df_Value$totalwins) %>% as_tibble()
colnames(Cor_Wins_List)<- c("Team","Overall Correlation", "Wins from 2011 to 2021")

####Convert all data into one dataset: Team, Year, Wins, Draft Score####
#Establish new team list#
teamlist2 <- ""

#Loop for one entry of each team for each year
for (team in teamlist){
  teamlist2<-c(rep(team,times = 2020-2010),teamlist2)
}
#Sort Teamlist
teamlist2 <- sort(teamlist2[1:300])
teamlist2

#Add year column#
df_complete<-tibble(teamlist2,rep(2011:2020,30))
colnames(df_complete) <- c("Team","Year") 

#Book Keeping for Records#
Records2 <-Records
Records2$Season <- teams

#Establish Wins Column#
df_complete$Wins = ""
#Loop for Adding Wins#
for (num in 1:nrow(df_complete)){
  #x/y coords update values#
  ycoord <- which(Records2$Season == df_complete$Team[num])
  xcoord <- which(Records2 %>% colnames() %>% as.character() == df_complete$Year[num])
  
  #Write values to df_complete#
  df_complete$Wins[num] <- Records[ycoord,xcoord]

}

#Establish Draft Score Column#
df_complete$Draft_Score = ""
#Loop for Adding Wins#
for (num in 1:nrow(df_complete)){
  #x/y coords update values#
  ycoord <- which(df_Value$Team == df_complete$Team[num])
  xcoord <- which(df_Value %>% colnames() %>% as.character() == df_complete$Year[num])
  
  #Write values to df_complete#
  df_complete$Draft_Score[num] <- df_Value[ycoord,xcoord]

}

#Convert new cols to numeric#

df_complete$Draft_Score=df_complete$Draft_Score %>% as.numeric()
df_complete$Wins=df_complete$Wins %>% as.numeric()

cor.test(df_complete$Wins,df_complete$Draft_Score)

#Percentile Columns#
df_complete$Wins_Percentile <- df_complete

#write csvs#
write.csv(Cor_Wins_List,"Corel Wins.csv")
write.csv(Records, "Records.csv")
write.csv(df_Value,"Draft Score.csv")
write.csv(df_complete, "Complete Dataset.csv")

####Add Percentile Cols####

#Read in Finished Data#
library(readr)
Complete_Dataset <- read_csv("Complete Dataset.csv")
View(Complete_Dataset)

#Add Percentile for Wins#
Complete_Dataset$Wins_Percentile<-percent_rank(Complete_Dataset$Wins)

#Add Percentile for Draft_Score#
Complete_Dataset$Score_Percentile<-percent_rank(Complete_Dataset$Draft_Score)

#Write CSV#
write.csv(Complete_Dataset, "Complete Dataset2.csv")

cor.test(Complete_Dataset$Wins %>% as.numeric(),Complete_Dataset$Draft_Score %>% as.numeric())
