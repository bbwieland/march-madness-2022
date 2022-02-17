## libraries ----
library(tidyverse)
library(rvest)

## CSV imports, straight from Kaggle ----

teams <- read.csv("Data/MTeams.csv") %>% select(TeamID,TeamName) %>%
  mutate(TeamName = sub(" St$"," St.",TeamName))
results <- read.csv("Data/MNCAATourneyDetailedResults.csv") %>%
  filter(Season >= 2008 & Season <= 2019) %>%
  mutate(list_index = Season - 2007) %>%
  mutate(gameid = seq(1:795)) %>%
  mutate(season_wteam = paste0(Season,WTeamID),
         season_lteam = paste0(Season,LTeamID))

## KenPom web scraping function ----

kenpom_scraper <- function(year){
  url <- read_html(paste0("https://kenpom.com/index.php?y=",year))
  kenpom <- html_table(url)[[1]]
  colnames(kenpom) = c("Rank","Team","Conf","Record","AdjEffMargin",
                       "OffAdjEff","OffAdjEffRk","DefAdjEff","DefAdjEffRk",
                       "AdjTempo","AdjTempoRank","Luck","LuckRank",
                       "SOSAdjEff","SOSAdjEffRk","SOSOffEff","SOSOffEffRk",
                       "SOSDefEff","SOSDefEffRk","SOSNonconAdjEff","SOSNonconAdjEffRk")
  
  kenpom_num <- kenpom %>% select(-Team,-Conf,-Record)
  kenpom_num <- as.data.frame(lapply(kenpom_num,as.numeric)) %>% filter(is.na(Rank) == F)
  
  kenpom_cat <- kenpom %>% select(Team,Conf,Record,Rank) %>% mutate(Rank = as.numeric(Rank))
  
  kenpom <- inner_join(kenpom_cat, kenpom_num, by = "Rank") %>%
    mutate(Team = gsub('[[:digit:]]+', '', Team)) %>%
    mutate(Team = gsub('\\*', '', Team)) %>%
    mutate(Team = sub(" $", "", Team))

  colnames(kenpom)[4:21] = paste0(colnames(kenpom)[4:21],"KP")
  
  dt = unlist(strsplit(kenpom$Record,"-"))
  wins = dt[seq(from = 1,to = (nrow(kenpom)*2-1),by = 2)]
  losses = dt[seq(from = 2,to = (nrow(kenpom)*2),by = 2)]
  
  kenpom = kenpom %>% mutate(Wins = as.numeric(wins),Losses = as.numeric(losses)) %>%
    mutate(WinPct = Wins/(Wins + Losses),
           Year = year) %>%
    left_join(teams,by = c("Team" = "TeamName"))
  
  
  kenpom
}

## BartTorvik web scraping function ----

torvik_scraper <- function(year){
  url <- read_html(paste0("https://barttorvik.com/teamstats.php?year=",year,"&sort=2"))
  torvik <- html_table(url)[[1]]
  colnames(torvik) <- c("Rank","Team","Conf","OffAdjEff","DefAdjEff",
                     "OffEFGPct","DefEFGPct","OffTOVPct","DefTOVPct",
                     "OffORBPct","DefORBPct","OffFTRate","DefFTRate",
                     "OffFTPct","DefFTPct","Off2PPct","Def2PPct",
                     "Off3PPct","Def3PPct")

  torvik_num <- torvik %>% select(-Team,-Conf)
  torvik_num <- as.data.frame(lapply(torvik_num,as.numeric)) %>% filter(is.na(Rank) == F)
  torvik_cat <- torvik %>% select(Team,Conf,Rank) %>% mutate(Rank = as.numeric(Rank))

  torvik <- inner_join(torvik_cat,torvik_num,by = "Rank") %>% 
    mutate(AdjEffMargin = OffAdjEff - DefAdjEff) %>%
    arrange(-AdjEffMargin) %>%
    select(-Rank) %>%
    mutate(Team = gsub('[[:digit:]]+', '', Team)) %>%
    mutate(Team = gsub('\\*', '', Team)) %>%
    mutate(Team = sub(" $", "", Team))

  colnames(torvik)[3:19] = paste0(colnames(torvik)[3:19],"BT")
  
  torvik = torvik %>% left_join(teams,by = c("Team" = "TeamName")) %>%
    mutate(Year = year)
  
  torvik
} 

## actually scraping data in list form
# for Kenpom, from 2008-2019
# for Torvik, from 2008-2019

# note: there are limitations on the scope of this data.
# Torvik data only goes back to 2008, while the Kaggle datasets end in 2019

kenpom_output = list(kp2008 = kenpom_scraper(2008),
                     kp2009 = kenpom_scraper(2009),
                     kp2010 = kenpom_scraper(2010),
                     kp2011 = kenpom_scraper(2011),
                     kp2012 = kenpom_scraper(2012),
                     kp2013 = kenpom_scraper(2013),
                     kp2014 = kenpom_scraper(2014),
                     kp2015 = kenpom_scraper(2015),
                     kp2016 = kenpom_scraper(2016),
                     kp2017 = kenpom_scraper(2017),
                     kp2018 = kenpom_scraper(2018),
                     kp2019 = kenpom_scraper(2019))

kenpom_output = rbind(kenpom_output[[1]],
                      kenpom_output[[2]],
                      kenpom_output[[3]],
                      kenpom_output[[4]],
                      kenpom_output[[5]],
                      kenpom_output[[6]],
                      kenpom_output[[7]],
                      kenpom_output[[8]],
                      kenpom_output[[9]],
                      kenpom_output[[10]],
                      kenpom_output[[11]],
                      kenpom_output[[12]])

torvik_output = list(bt2008 = torvik_scraper(2008),
                     bt2009 = torvik_scraper(2009),
                     bt2010 = torvik_scraper(2010),
                     bt2011 = torvik_scraper(2011),
                     bt2012 = torvik_scraper(2012),
                     bt2013 = torvik_scraper(2013),
                     bt2014 = torvik_scraper(2014),
                     bt2015 = torvik_scraper(2015),
                     bt2016 = torvik_scraper(2016),
                     bt2017 = torvik_scraper(2017),
                     bt2018 = torvik_scraper(2018),
                     bt2019 = torvik_scraper(2019))

torvik_output = rbind(torvik_output[[1]],
                      torvik_output[[2]],
                      torvik_output[[3]],
                      torvik_output[[4]],
                      torvik_output[[5]],
                      torvik_output[[6]],
                      torvik_output[[7]],
                      torvik_output[[8]],
                      torvik_output[[9]],
                      torvik_output[[10]],
                      torvik_output[[11]],
                      torvik_output[[12]])

### combining the big dataset: 
# note: our functions are basically doing the following:
# 1: selecting data for the winner and loser of each game, for each dataset (KP and BT)
# 2: applying that function to every game
# 3: combining the winner & loser data for each game for each dataset (KP and BT)

torvik_w_selector = function(input){
  year = input[1]
  id = input[2]
  df = torvik_output %>% filter(TeamID == id,Year == year)
  colnames(df) = paste0("W",colnames(df))
  df
}

torvik_l_selector = function(input){
  year = input[1]
  id = input[2]
  df = torvik_output %>% filter(TeamID == id,Year == year)
  colnames(df) = paste0("L",colnames(df))
  df
}

kenpom_w_selector = function(input){
  year = input[1]
  id = input[2]
  df = kenpom_output %>% filter(TeamID == id,Year == year)
  colnames(df) = paste0("W",colnames(df))
  df
}

kenpom_l_selector = function(input){
  year = input[1]
  id = input[2]
  df = kenpom_output %>% filter(TeamID == id,Year == year)
  colnames(df) = paste0("L",colnames(df))
  df
}

results_w_input = results %>% select(Season,WTeamID)
results_l_input = results %>% select(Season,LTeamID)

## 

apply_w_combiner = function(func,results){
test = apply(results,1,func)
test2 = do.call(rbind.data.frame, test) %>% 
  mutate(season_wteam = paste0(WYear,WTeamID))
test2
}

apply_l_combiner = function(func,results){
  test = apply(results,1,func)
  test2 = do.call(rbind.data.frame, test) %>% 
    mutate(season_lteam = paste0(LYear,LTeamID))
  test2
}

torvik_w_info = apply_w_combiner(torvik_w_selector,results_w_input)
torvik_l_info = apply_l_combiner(torvik_l_selector,results_l_input)
kenpom_w_info = apply_w_combiner(kenpom_w_selector,results_w_input)
kenpom_l_info = apply_l_combiner(kenpom_l_selector,results_l_input)

# ultimately, we have "fulldata", a dataset w/ info on every game
# as well as information from KenPom and BartTorvik on the winners & losers.

fulldata = inner_join(results,torvik_w_info,by = "season_wteam") %>%
  distinct() %>%
  inner_join(torvik_l_info,by = "season_lteam") %>%
  distinct() %>%
  inner_join(kenpom_w_info,by = "season_wteam") %>%
  distinct() %>%
  inner_join(kenpom_l_info,by = "season_lteam")

write.csv(fulldata,file = "Data/complete-model-data.csv")
