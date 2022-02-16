## I realized that I might've made a major error in selecting my training/test datasets
## in the model-building-1 script. This code is an attempt to remedy that. 


## libraries & global presets ----
library(tidyverse)
library(MLmetrics)
library(caret)

set.seed(4133)
data = read_csv("complete-model-data.csv") %>% mutate(GameID = seq(1:660))

## functions ----

LogLossTester = function(model,predict.set){
  
  prediction = predict(model,predict.set,type = "response")
  test.result = predict.set %>% mutate(WinProb = prediction) %>%
    mutate(PredictWin = ifelse(WinProb >= 0.5,1,0)) %>%
    mutate(Correct = ifelse(PredictWin == WinLoss,1,0))%>% 
    mutate(confidence = ifelse(
      WinProb < 0.05|WinProb > 0.95,"extreme",
      ifelse(WinProb < 0.20|WinProb > 0.80,"high",
             ifelse(WinProb < 0.40|WinProb > 0.60,"medium","low")))) %>%
    mutate(confidence = factor(confidence,levels = c("low","medium","high","extreme")))
  
  log.loss = LogLoss(test.result$WinProb,test.result$WinLoss)
  correct.summary = test.result %>% summarise(accuracy = sum(Correct)/n()) %>%
    mutate(logloss = log.loss) 
  
  failures = test.result %>% filter(Correct == 0)
  
  performance.by.confidence = test.result %>% 
    group_by(confidence) %>%
    summarise(correct = sum(Correct),total = n()) %>%
    mutate(percent = correct/total)
  
  output = list(Predictions = test.result, 
                Failures = failures, 
                Performance = correct.summary,
                ConfidencePerformance = performance.by.confidence)
  output
  
}

MadnessSimulator = function(model){
  test.2.madness = test.2 %>% slice_sample(n = 63)
  
  result = LogLossTester(model,test.2.madness)
  
  performance = result$Performance
  
  performance
}

MadnessReplicator = function(n,model){
  
  madness = unlist(replicate(n,madness_simulator(model),simplify = F))
  accuracy = madness[seq(1,length(madness),2)]
  logloss = madness[seq(2,length(madness),2)] 
  
  madness.results = data.frame(Accuracy = accuracy,LogLoss = logloss)
  madness.results
}

## creating training data & test data for model building 
## note: this is for the models using KenPom & Torvik as predictors!
## other logistic regression strategies might be in other files 

trial.w = data %>% select(WTeam.x,LTeam.x,WScore,LScore,WOffAdjEffBT,WDefAdjEffBT,
                          LOffAdjEffBT,LDefAdjEffBT,WOffAdjEffKP,WDefAdjEffKP,
                          LOffAdjEffKP,LDefAdjEffKP,GameID) %>%
  mutate(PtDiff = WScore - LScore,
         WinLoss = 1) %>%
  mutate(OffDiffBT = WOffAdjEffBT - LOffAdjEffBT,
         DefDiffBT = WDefAdjEffBT - LDefAdjEffKP,
         OffDiffKP = WOffAdjEffKP - LOffAdjEffKP,
         DefDiffKP = WDefAdjEffKP - LDefAdjEffKP) %>%
  select(WTeam.x,LTeam.x,WinLoss,OffDiffBT,DefDiffBT,OffDiffKP,DefDiffKP,GameID) %>%
  rename(WinTeam = WTeam.x,LostTeam = LTeam.x)

trial.l = trial.w %>% mutate(OffDiffBT = -OffDiffBT,
                             OffDiffKP = -OffDiffKP,
                             DefDiffBT = -DefDiffBT,
                             DefDiffKP = -DefDiffKP,
                             WinLoss = 0)

trials.1 = rbind(trial.w,trial.l) %>% 
  mutate(random.sort = runif(1320)) %>%
  arrange(random.sort)

trials.2 = rbind(trial.w,trial.l) %>% 
  mutate(random.sort = runif(1320)) %>%
  arrange(-random.sort)

## randomly sampling from trials
## this gives us EITHER the winner or loser for each of the 1660 games in the dataset
## note: test.2 will contain ALL observations not in trials

test.1 = trials.1[!duplicated(trials.1[,c('GameID')]),] %>%
  select(-random.sort)

test.2 = trials.2[!duplicated(trials.2[,c('GameID')]),] %>%
  select(-random.sort)

## What does each dataset contain?

# test.1 = all 660 games (GameID 1 to 660).
# test.2 = the same games, but from the opposite "perspective"
# for example, for GameID == 1: 
# test.1 contains the Duke "perspective," statistically, while test.2 contains Belmont's. 

## Selecting a training sample and a test sample from test.1: 
# note: 70% of data will be used to train; 30% will be used to test. 

training.index = sample(seq_len(nrow(test.1)), size = 0.7*nrow(test.1))

training.test.1 = test.1[training.index,]
validation.test.1 = test.1[-training.index,]

## now, we can train models on this training dataset + test them against the validation dataset
## this fixes a MAJOR error in the model-building-1.R code, which led it to return
## unrealistically low log-loss values & overfit the data. 

## let's try the model w/ interaction terms & the model without interaction terms. 

model.1 = glm(WinLoss~OffDiffKP+DefDiffKP+OffDiffBT+DefDiffBT,training.test.1,family = "binomial")
LogLossTester(model.1,validation.test.1)

model.1.interaction = glm(WinLoss~OffDiffKP+DefDiffKP+OffDiffBT+DefDiffBT+
                            OffDiffKP*OffDiffBT+DefDiffKP*DefDiffBT,training.test.1,family = "binomial")
model.1.interaction.logloss = LogLossTester(model.1.interaction,validation.test.1)

## uh... that's weird
## looking at model failures real quick just to analyze what's going on/why we're so accurate
## note: I think the model w/ the interaction term is *probably* more sound moving forward.

model.1.interaction.failures = model.1.interaction.logloss$Failures
model.1.interaction.results = model.1.interaction.logloss$Predictions

## let's make up a "confidence" variable with 4 levels: low, medium, high, extreme: 
# low for projected win prob 40%-60%
# medium for projected win prob 20-40% or 60-80%
# high for projected win prob 5-20% or 80-95%
# extreme for projected win prob 0-5% or 95-100% 

# a table displaying performance by confidence is now appended to the LogLossTester function.
# call it via (LogLossObject)$ConfidencePerformance



  