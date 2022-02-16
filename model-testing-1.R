library(tidyverse)
library(MLmetrics)

set.seed(4133)
data = read_csv("complete-model-data.csv") %>% mutate(GameID = seq(1:660))

LogLossTester = function(model,predict.set){
  
  prediction = predict(model,predict.set,type = "response")
  test.result = predict.set %>% mutate(WinProb = prediction) %>%
    mutate(PredictWin = ifelse(WinProb >= 0.5,1,0)) %>%
    mutate(Correct = ifelse(PredictWin == WinLoss,1,0))
  
  log.loss = LogLoss(test.result$WinProb,test.result$WinLoss)
  correct.summary = test.result %>% summarise(accuracy = sum(Correct)/n()) %>%
    mutate(logloss = log.loss)
  
  failures = test.result %>% filter(Correct == 0)
  
  output = list(Predictions = test.result, Failures = failures, Performance = correct.summary)
  output
  
}

## First possible approach: using differences in offensive & defensive efficiency

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


trials = rbind(trial.w,trial.l) %>% 
  mutate(random.sort = runif(1320)) %>%
  arrange(random.sort)

trials.2 = rbind(trial.w,trial.l) %>% 
  mutate(random.sort = runif(1320)) %>%
  arrange(-random.sort)

## randomly sampling from trials
## this gives us EITHER the winner or loser for each of the 1660 games in the dataset
## note: test.2 will contain ALL observations not in trials

test = trials[!duplicated(trials[,c('GameID')]),] %>%
  select(-GameID,-random.sort)

test.2 = trials.2[!duplicated(trials.2[,c('GameID')]),] %>%
  select(-GameID,-random.sort)

## building a baseline model, with p = 0.5 for all

LogLossBaseline = function(predict.set){
  
prediction = rep(0.5,660)

test.result = predict.set %>% mutate(WinProb = prediction)
log.loss = LogLoss(test.result$WinProb,test.result$WinLoss)

output = list(Predictions = test.result, LogLoss = log.loss)
output

}

LogLossBaseline(test.2)

## building the first model using all variables

model.1 = glm(WinLoss~OffDiffKP+DefDiffKP+OffDiffBT+DefDiffBT,test,family = "binomial")
LogLossTester(model.1,test.2)

model.1.interaction = glm(WinLoss~OffDiffKP+DefDiffKP+OffDiffBT+DefDiffBT+
                            OffDiffKP*OffDiffBT+DefDiffKP*DefDiffBT,test,family = "binomial")
LogLossTester(model.1.interaction,test.2)

## building model using just KenPom

model.2 = glm(WinLoss~OffDiffKP+DefDiffKP,test,family = "binomial")
LogLossTester(model.2,test.2)

## building model using just Torvik

model.3 = glm(WinLoss~OffDiffBT+DefDiffBT,test,family = "binomial")
LogLossTester(model.3,test.2)

## refining best model so far... using stepAIC

library(MASS)
model.4 <- stepAIC(model.1,direction = "both",trace = F)
detach("package:MASS")

wew = LogLossTester(model.1,test.2)$Failures

wew2 = LogLossTester(model.2,test.2)$Failures
## RESULTS: 

# best model is Model 1, with a log-loss of 0.370 and 84.5% accuracy

## simulating actual March Madness to see how the model performs... 

madness_simulator = function(model){
test.2.madness = test.2 %>% slice_sample(n = 63)

result = LogLossTester(model,test.2.madness)

performance = result$Performance

performance
}


madness_replicator = function(n,model){

madness = unlist(replicate(n,madness_simulator(model),simplify = F))
accuracy = madness[seq(1,length(madness),2)]
logloss = madness[seq(2,length(madness),2)] 

madness.results = data.frame(Accuracy = accuracy,LogLoss = logloss)
madness.results
}

model.1.outcomes = madness_replicator(500,model.1)
