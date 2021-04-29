library(tidyverse)
library(httr)
library(jsonlite)
library(scales)


#######################################################################
# Get Players:
#https://www.nba.com/stats/players/shooting/?Season=2020-21&SeasonType=Regular%20Season&DistanceRange=By%20Zone

require(httr)

y <- list()

season <- c('2020-21')

count <- 1

for(i in 1:length(season)){
  
  headers = c(
    `Connection` = 'keep-alive',
    `Accept` = 'application/json, text/plain, */*',
    `x-nba-stats-token` = 'true',
    `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
    `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
    `x-nba-stats-origin` = 'stats',
    `Sec-Fetch-Site` = 'same-origin',
    `Sec-Fetch-Mode` = 'cors',
    `Referer` = 'https://stats.nba.com/',
    `Accept-Encoding` = 'gzip, deflate, br',
    `Accept-Language` = 'en-US,en;q=0.9'
  )
  
  params = list(
    `College` = '',
    `Conference` = '',
    `Country` = '',
    `DateFrom` = '',
    `DateTo` = '',
    `DistanceRange` = 'By Zone',
    `Division` = '',
    `DraftPick` = '',
    `DraftYear` = '',
    `GameScope` = '',
    `GameSegment` = '',
    `Height` = '',
    `LastNGames` = '0',
    `LeagueID` = '00',
    `Location` = '',
    `MeasureType` = 'Base',
    `Month` = '0',
    `OpponentTeamID` = '0',
    `Outcome` = '',
    `PORound` = '0',
    `PaceAdjust` = 'N',
    `PerMode` = 'Totals',
    `Period` = '0',
    `PlayerExperience` = '',
    `PlayerPosition` = '',
    `PlusMinus` = 'N',
    `Rank` = 'N',
    `Season` = season[i],
    `SeasonSegment` = '',
    `SeasonType` = 'Regular Season',
    `ShotClockRange` = '',
    `StarterBench` = '',
    `TeamID` = '0',
    `VsConference` = '',
    `VsDivision` = '',
    `Weight` = ''
  )
  
  res <- httr::GET(url = 'https://stats.nba.com/stats/leaguedashplayershotlocations', httr::add_headers(.headers=headers), query = params)
  #res <- httr::GET(url = url, httr::add_headers(.headers=headers))
  
  
  json_resp <- jsonlite::fromJSON(content(res, "text"))
  y[[count]] <- data.frame(json_resp$resultSets$rowSet, stringsAsFactors = F)
  y[[count]]$season = season[i]
  y[[count]]$yr = i + 2020
  
  count <- count + 1
  
}



z <- do.call('rbind', y)
colnames(z)[1:29] <- json_resp$resultSets$headers$columnNames[[2]]


colnames(z)[6:8] = paste(json_resp$resultSets$headers$columnNames[[1]][1], colnames(z)[6:8], 
                         sep = '_')
colnames(z)[9:11] = paste(json_resp$resultSets$headers$columnNames[[1]][2], colnames(z)[9:11], 
                          sep = '_')
colnames(z)[12:14] = paste(json_resp$resultSets$headers$columnNames[[1]][3], colnames(z)[12:14], 
                           sep = '_')
colnames(z)[15:17] = paste(json_resp$resultSets$headers$columnNames[[1]][4], colnames(z)[15:17], 
                           sep = '_')
colnames(z)[18:20] = paste(json_resp$resultSets$headers$columnNames[[1]][5], colnames(z)[18:20], 
                           sep = '_')
colnames(z)[21:23] = paste(json_resp$resultSets$headers$columnNames[[1]][6], colnames(z)[21:23], 
                           sep = '_')
colnames(z)[24:26] = paste(json_resp$resultSets$headers$columnNames[[1]][7], colnames(z)[24:26], 
                           sep = '_')

for(j in 6:26){
  z[,j] <- as.numeric(z[,j])
}

z$FGA <- z$`Restricted Area_FGA` + z$`In The Paint (Non-RA)_FGA` +
         z$`Mid-Range_FGA` + z$`Left Corner 3_FGA` + z$`Right Corner 3_FGA` +
         z$`Above the Break 3_FGA` + z$Backcourt_FGA 

#####################################################################


# START HERE IN CODE

# Loop through the players with the most shots:
z <- z[order(z$FGA, decreasing = T),]

df_store <- list()


# Get top 100 players shot chart:
for(i in 1:100){
  
id <- z$PLAYER_ID[i]

url <- paste('https://stats.nba.com/stats/shotchartdetail?AheadBehind=&CFID=33&CFPARAMS=2020-21&ClutchTime=&Conference=&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&Division=&EndPeriod=10&EndRange=28800&GROUP_ID=&GameEventID=&GameID=&GameSegment=&GroupID=&GroupMode=&GroupQuantity=5&LastNGames=0&LeagueID=00&Location=&Month=0&OnOff=&OpponentTeamID=0&Outcome=&PORound=0&Period=0&PlayerID=',
             id, 
             '&PlayerID1=&PlayerID2=&PlayerID3=&PlayerID4=&PlayerID5=&PlayerPosition=&PointDiff=&Position=&RangeType=0&RookieYear=&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StartPeriod=1&StartRange=0&StarterBench=&TeamID=0&VsConference=&VsDivision=&VsPlayerID1=&VsPlayerID2=&VsPlayerID3=&VsPlayerID4=&VsPlayerID5=&VsTeamID=',
             sep = '')

headers = c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/leaguedashplayerbiostats/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)


res <- httr::GET(url = url, httr::add_headers(.headers=headers))
json_resp <- jsonlite::fromJSON(content(res, "text"))
df <- data.frame(json_resp$resultSets$rowSet[1])
colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]]

df$LOC_X <- as.numeric(as.character(df$LOC_X))
df$LOC_Y <- as.numeric(as.character(df$LOC_Y))

df_store[[i]] = df
print(i)
}

# How many shots are left and how many are right?
h <- data.frame(player = rep('0', 100),
                right_shots = 0,
                left_shots = 0,
                middle_shots = 0,
                stringsAsFactors = F)
for(i in 1:100){
  temp = df_store[[i]]
  h$player[i] = as.character(temp$PLAYER_NAME[1])
  h$right_shots[i] = sum(temp$LOC_X > 0)
  h$left_shots[i] = sum(temp$LOC_X < 0)
  h$middle_shots[i] = sum(temp$LOC_X == 0)
}

h$prop_right_shots <- h$right_shots / (h$right_shots + h$left_shots)
h$fga <- (h$right_shots + h$left_shots + h$middle_shots)

p <- h[,c('player','fga','prop_right_shots')]
colnames(p) = c('Player', 'FGA', 'Proportion of FGA From Right Side')
p$`Proportion of FGA From Right Side` <- round(p$`Proportion of FGA From Right Side`,2)

# Display a nice table:
library(formattable)
library(data.table)
library(dplyr)

unit.scale = function(x) x

customGreen0 = "#DeF7E9"

customGreen = "#71CA97"

customRed = "#ff7f7f"

p <- p[order(p$`Proportion of FGA From Right Side`, decreasing = T),]
formattable(p, 
            align =c("l","c","r"), 
            list(`Player` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")),
              `Proportion of FGA From Right Side`= color_bar(customGreen0, fun = unit.scale))
            )

setwd("~/Documents")
write.csv(p, 'shots_from_right_and_left_side_4_29_21.csv',
          row.names = F)

