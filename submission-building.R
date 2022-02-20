library(tidyverse)
library(MLmetrics)
library(caret)
library(gtools)
library(combinat)

seeds = read.csv("Data/MNCAATourneySeeds.csv") %>% filter(Season >= 2016)
id_matches = read.csv("Data/MTeamSpellings.csv")

df_builder = function(year) {
  id_matches = read.csv("Data/MTeamSpellings.csv")
  csu = data.frame(TeamNameSpelling = c("cal st. bakersfield","arkansas little rock"),TeamID = c(1167,1114))
  
  id_matches = rbind(id_matches,csu)
  
  kp_16 = kenpom_output %>% 
    filter(Year == year) %>% 
    rename(Season = Year) %>% 
    left_join(id_matches,by = c("Team" = "TeamNameSpelling")) %>%
    select(-TeamID.x) %>%
    rename(TeamID = TeamID.y)
  
  seeds_16 = seeds %>% filter(Season == year) 
  
  bt_16 = torvik_output %>%
    filter(Year == year) %>%
    rename(Season = Year) %>% 
    left_join(id_matches,by = c("Team" = "TeamNameSpelling")) %>%
    select(-TeamID.x) %>%
    rename(TeamID = TeamID.y)
 
   teams_16 = seeds_16 %>% 
    left_join(kp_16,by = "TeamID") %>% 
    left_join(bt_16,by = "TeamID") %>%
    distinct(TeamID,.keep_all = T)
  teams_16
  
}

years = c(2016,2017,2018,2019,2021)

output = years %>%
  map_dfr(.f = ~df_builder(year = .x)) %>%
  select(-Season.x,-Season.y,-Team.y,-Conf.y) %>%
  rename(Team = Team.x,Conf = Conf.x)

output_cleaned = output %>% 
  select(-Seed,-Conf,-Record)

permutationGenerator = function(year){
  new_output = output_cleaned %>% 
    filter(Season == year) %>%
    select(-Season)
  new_output %>% arrange(TeamID)
  perm = data.frame(t(combn(new_output$TeamID,2)))
  formatted_perm = perm %>% 
    mutate(Year = year,
           TeamID1 = ifelse(X1 < X2,X1,X2),
           TeamID2 = ifelse(X1 < X2,X2,X1)) %>%
    select(-X1,-X2) %>%
    left_join(new_output,by = c("TeamID1" = "TeamID")) %>%
    left_join(new_output,by = c("TeamID2" = "TeamID"))
  
  formatted_perm = formatted_perm %>%
    mutate(OffDiffEFG = OffEFGPctBT.x - OffEFGPctBT.y,
           DefDiffEFG = DefEFGPctBT.x - DefEFGPctBT.y,
           OffDiffTOV = OffTOVPctBT.x - OffTOVPctBT.y,
           DefDiffORBPct = DefORBPctBT.x - DefORBPctBT.y,
           OffDiffKP = OffAdjEffKP.x - OffAdjEffKP.y,
           DefDiffKP = DefAdjEffKP.x - DefAdjEffKP.y,
           OffDiffBT = OffAdjEffBT.x - OffAdjEffBT.y,
           DefDiffBT = DefAdjEffBT.x - DefAdjEffBT.y) 
  
  model.4.predictions = predict(model.4,formatted_perm,type = "response")
  model.5.predictions = predict(model.5,formatted_perm,type = "response")
  
  
  formatted_perm = formatted_perm %>% mutate(Model4Odds = model.4.predictions,
                                             Model5Odds = model.5.predictions)
  
  formatted_perm
}

yr2016 = permutationGenerator(2016)
yr2017 = permutationGenerator(2017)
yr2018 = permutationGenerator(2018)
yr2019 = permutationGenerator(2019)
yr2021 = permutationGenerator(2021)

overallData = rbind(yr2016,yr2017,yr2018,yr2019,yr2021)

overallDataFormatted = overallData %>%
  mutate(ID = paste(Year,TeamID1,TeamID2,sep = "_"))

overallDataModel4 = overallDataFormatted %>%
  select(ID,Model4Odds) %>% 
  rename(Pred = Model4Odds) %>%
  arrange(ID)

overallDataModel5 = overallDataFormatted %>%
  select(ID,Model5Odds) %>% 
  rename(Pred = Model5Odds) %>%
  arrange(ID)

write.csv(overallDataModel4,file = "Predictions/model-4-picks.csv",
          row.names = F)
write.csv(overallDataModel5,file = "Predictions/model-5-picks.csv",
          row.names = F)
