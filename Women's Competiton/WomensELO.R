library(tidyverse)
library(EloRating)

regularSeasonResults = read_csv("Women's Competiton/Data/WRegularSeasonCompactResults.csv")
tournamentResults = read_csv("Women's Competiton/Data/WNCAATourneyCompactResults.csv")
teams = read_csv("Women's Competiton/Data/WTeams.csv")
seeds = read_csv("Women's Competiton/Data/WNCAATourneySeeds.csv")

seedsNumeric = seeds %>%
  mutate(Seed = as.numeric(gsub("[^0-9.-]", "", Seed)))

## Naive Elo rating function

seasonEloCalculator = function(year){
  dateZero = as.Date(paste0(year,"-01-01"))
  
  seasonData = regularSeasonResults %>% 
    filter(Season == year) %>%
    mutate(dateIndex = dateZero + DayNum)
  
  seasonElo = elo.seq(winner = seasonData$WTeamID,
                      loser = seasonData$LTeamID,
                      Date = seasonData$dateIndex,
                      runcheck = F)
  
  optimizedSeasonElo = optimizek(seasonElo,krange = c(10,200),resolution = 191)
  
  bestK = optimizedSeasonElo[[1]]$k
  
  optimizedEloOutput = elo.seq(winner = seasonData$WTeamID,
                               loser = seasonData$LTeamID,
                               Date = seasonData$dateIndex,
                               runcheck = F,
                               k = bestK)
  
  eloByTeam = data.frame(elo = extract_elo(optimizedEloOutput))
  eloByTeam = eloByTeam %>%
    mutate(teamID = as.numeric(rownames(eloByTeam))) %>%
    left_join(teams,by = c("teamID" = "TeamID"))

  
  return(eloByTeam)
}

eloPredictor <- function(elorating1,elorating2) {
  team1.odds <- 1/(10 ** (-(elorating1 - elorating2) / 400) + 1)
  team1.odds
}

## Computing information on win probability based on seed difference
## the objective here: to figure out estimated win probability given a seed difference n
## and convert it into a modification that can be added to Elo ratings. 

seedWinProbCalculator = function(){
  seedsNumeric = seeds %>%
  mutate(Seed = as.numeric(gsub("[^0-9.-]", "", Seed)))

tournamentResultsWithSeeds = tournamentResults %>% 
  left_join(seedsNumeric,by = c("Season","WTeamID" = "TeamID")) %>%
  rename(WSeed = Seed) %>%
  left_join(seedsNumeric,by = c("Season","LTeamID" = "TeamID")) %>%
  rename(LSeed = Seed) %>%
  mutate(SeedDifference = WSeed - LSeed,
         HigherSeedWin = ifelse(WSeed < LSeed,1,0)) %>%
  mutate(absoluteSeedDifference = abs(SeedDifference))

tournamentResultsBySeedDifference = tournamentResultsWithSeeds %>% 
  group_by(absoluteSeedDifference) %>%
  summarise(NonUpsets = sum(HigherSeedWin),
            Games = n(),
            NonUpsetProb = NonUpsets/Games) %>%
  filter(Games >= 25)

tournamentResultsBySeedDifference[which(tournamentResultsBySeedDifference$absoluteSeedDifference == 0),4] = 0.5

loessSeedDifferenceFormula = loess(NonUpsetProb ~ absoluteSeedDifference,data = tournamentResultsBySeedDifference)
potentialSeedDifferences = data.frame(absoluteSeedDifference = seq(from = 0, to = 15)) %>%
  mutate(predictedValue = predict(loessSeedDifferenceFormula,absoluteSeedDifference))

potentialSeedDifferences[1,2] = 0.5
potentialSeedDifferences[14,2] = 1
potentialSeedDifferences[15,2] = 1
potentialSeedDifferences[16,2] = 1

potentialSeedDifferencesInverse = potentialSeedDifferences %>%
  mutate(absoluteSeedDifference = -absoluteSeedDifference,
         predictedValue = 1 - predictedValue)

potentialSeedFullTable = rbind(potentialSeedDifferences,potentialSeedDifferencesInverse) %>%
  distinct(absoluteSeedDifference, .keep_all = T)

return(potentialSeedFullTable)
}

seedWinProbTable = seedWinProbCalculator()

seasonTournamentPredictorSeed = function(year){

  seasonTournamentData = tournamentResults %>%
    filter(Season == year)
  
  seasonTournamentTeams = c(seasonTournamentData$WTeamID,seasonTournamentData$LTeamID) %>%
    unique()
  
  seasonTournamentMatchups = data.frame(t(combn(seasonTournamentTeams,2))) %>% 
    mutate(Year = year,
           TeamID1 = ifelse(X1 < X2,X1,X2),
           TeamID2 = ifelse(X1 < X2,X2,X1)) %>%
    select(-X1,-X2)
  
  seasonTournamentMatchupsWithSeed = seasonTournamentMatchups %>% 
    left_join(seedsNumeric,by = c("TeamID1" = "TeamID","Year" = "Season")) %>%
    rename(Seed1 = Seed) %>% 
    left_join(seedsNumeric,by = c("TeamID2" = "TeamID","Year" = "Season")) %>%
    rename(Seed2 = Seed) %>%
    mutate(SeedDifference = Seed2 - Seed1) %>%
    left_join(seedWinProbTable, by = c("SeedDifference" = "absoluteSeedDifference"))
  
  seasonTournamentMatchupsFormatted = seasonTournamentMatchupsWithSeed %>%
    mutate(matchupColumn = paste(Year,TeamID1,TeamID2,sep = "_")) %>%
    select(matchupColumn,predictedValue) %>%
    rename(ID = matchupColumn,Pred = predictedValue)
  return(seasonTournamentMatchupsFormatted)
    
}

seasonTournamentPredictorElo = function(year){
  seasonEloTable = seasonEloCalculator(year)
  
  seasonTournamentData = tournamentResults %>%
    filter(Season == year)
  
  seasonTournamentTeams = c(seasonTournamentData$WTeamID,seasonTournamentData$LTeamID) %>%
    unique()
  
  seasonTournamentMatchups = data.frame(t(combn(seasonTournamentTeams,2))) %>% 
    mutate(Year = year,
           TeamID1 = ifelse(X1 < X2,X1,X2),
           TeamID2 = ifelse(X1 < X2,X2,X1)) %>%
    select(-X1,-X2)
  
  seasonTournamentMatchupsWithElo = seasonTournamentMatchups %>% 
    left_join(seasonEloTable, by = c("TeamID1" = "teamID")) %>%
    left_join(seasonEloTable, by = c("TeamID2" = "teamID")) %>%
    rename(TeamElo1 = elo.x,TeamElo2 = elo.y) %>%
    select(-TeamName.x,-TeamName.y) %>%
    mutate(EloPrediction = eloPredictor(TeamElo1,TeamElo2)) %>%
    mutate(matchupColumn = paste(Year,TeamID1,TeamID2,sep = "_"))
  
  seasonPredictionOutput = seasonTournamentMatchupsWithElo %>%
    select(matchupColumn,EloPrediction) %>% 
    rename(ID = matchupColumn,Pred = EloPrediction)
  
  return(seasonPredictionOutput)
}

getWomensYearCSV = function(year,method){
  if (method == "elo") {
    seasonPicks = seasonTournamentPredictorElo(year)
    write.csv(seasonPicks,file = paste0("Women's Competiton/Predictions/elo-picks-",year,".csv"),row.names = F)
  }
  
  if (method == "seed"){
    seasonPicks = seasonTournamentPredictorSeed(year)
    write.csv(seasonPicks,file = paste0("Women's Competiton/Predictions/seed-picks-",year,".csv"),row.names = F)
  }
}


getWomensYearCSV(2016,"seed")
getWomensYearCSV(2017,"seed")
getWomensYearCSV(2018,"seed")
getWomensYearCSV(2019,"seed")
getWomensYearCSV(2021,"seed")

getWomensYearCSV(2016,"elo")
getWomensYearCSV(2017,"elo")
getWomensYearCSV(2018,"elo")
getWomensYearCSV(2019,"elo")
getWomensYearCSV(2021,"elo")

years = c(2016,2017,2018,2019,2021)

eloLastFivePicks = years %>%
  map_dfr(.f = ~seasonTournamentPredictorElo(.x))

seedLastFivePicks = years %>%
  map_dfr(.f = ~seasonTournamentPredictorSeed(.x))

combinedLastFivePicks = inner_join(eloLastFivePicks,seedLastFivePicks,by = "ID") %>%
  rename(EloPrediction = Pred.x,SeedPrediction = Pred.y)

combinedPicksWeighted = function(eloWeight){
  seedWeight = 1 - eloWeight
  
  weightedLastFivePicks = combinedLastFivePicks %>%
    mutate(weightedPred = eloWeight*EloPrediction + seedWeight*SeedPrediction) %>%
    select(ID,weightedPred) %>%
    rename(Pred = weightedPred)
  
  write.csv(weightedLastFivePicks,
            file = paste0("Women's Competiton/Predictions/weighted-picks-last-five-elo-",eloWeight*100,"-percent-weight.csv"),
            row.names = F)
}

combinedPicksWeighted(0.5)
combinedPicksWeighted(0.2)
combinedPicksWeighted(0.1)
combinedPicksWeighted(0.05)
combinedPicksWeighted(0.03)
combinedPicksWeighted(0.02)
combinedPicksWeighted(0.01)
combinedPicksWeighted(0.005)

write.csv(eloLastFivePicks,file = paste0("Women's Competiton/Predictions/elo-picks-last-five.csv"),row.names = F)
write.csv(seedLastFivePicks,file = paste0("Women's Competiton/Predictions/seed-picks-last-five.csv"),row.names = F)


