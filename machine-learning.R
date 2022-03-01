library(recipes)
library(tidyverse)
library(MLmetrics)
library(caret)

set.seed(4133)
data = read_csv("Data/complete-model-data-2.csv") %>% mutate(GameID = seq(1:852))

## functions ----

theme_custom = function() {
  theme_minimal() +
    theme(
      plot.title = element_text(
        face = "bold",
        hjust = 0.5,
        size = 22
      ),
      plot.subtitle = element_text(hjust = 0.5, size = 14),
      axis.title = element_text(face = "bold", size = 10),
      axis.text = element_text(face = "bold")
    )
  
}

LogLossTester = function(model, predict.set) {
  prediction = predict(model, predict.set, type = "response")
  test.result = predict.set %>% mutate(WinProb = prediction) %>%
    mutate(PredictWin = ifelse(WinProb >= 0.5, 1, 0)) %>%
    mutate(Correct = ifelse(PredictWin == WinLoss, 1, 0)) %>%
    mutate(confidence = ifelse(
      WinProb < 0.05 | WinProb > 0.95,
      "extreme",
      ifelse(
        WinProb < 0.20 | WinProb > 0.80,
        "high",
        ifelse(WinProb < 0.40 |
                 WinProb > 0.60, "medium", "low")
      )
    )) %>%
    mutate(confidence = factor(confidence, levels = c("low", "medium", "high", "extreme")))
  
  log.loss = LogLoss(test.result$WinProb, test.result$WinLoss)
  correct.summary = test.result %>% summarise(accuracy = sum(Correct) /
                                                n()) %>%
    mutate(logloss = log.loss)
  
  failures = test.result %>% filter(Correct == 0)
  
  performance.by.confidence = test.result %>%
    group_by(confidence) %>%
    summarise(correct = sum(Correct), total = n()) %>%
    mutate(percent = correct / total)
  
  output = list(
    Predictions = test.result,
    Failures = failures,
    Performance = correct.summary,
    ConfidencePerformance = performance.by.confidence
  )
  output
  
}

MadnessSimulator = function(model) {
  test.2.madness = test.2 %>% slice_sample(n = 63)
  
  result = LogLossTester(model, test.2.madness)
  
  performance = result$Performance
  
  performance
}

MadnessReplicator = function(n, model) {
  madness = unlist(replicate(n, MadnessSimulator(model), simplify = F))
  accuracy = madness[seq(1, length(madness), 2)]
  logloss = madness[seq(2, length(madness), 2)]
  
  madness.results = data.frame(Accuracy = accuracy, LogLoss = logloss)
  madness.results
}

years = c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2021)

yearMadnessSimulator = function(model,year) {
  year.data = data.by.year[[(year - 2007)]]
  
  result = LogLossTester(model, year.data)
  
  performance = result$Performance
  
  performance
}

yearMadnessReplicator = function(model){
  years = c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
  years.real = c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2021)
  
  
  output = years %>%
    map_dfr(.f = ~yearMadnessSimulator(model = model,year = .x))
  
  output %>% mutate(Season = years.real)
}

yearMadnessReplicator(model.1)
yearMadnessReplicator(model.2)
yearMadnessReplicator(model.3)

## creating training data & test data for model building
## note: this is for the models using KenPom & Torvik as predictors!
## other logistic regression strategies might be in other files

## data creation ----

dataSelector = function(){
  trial.w = data %>% select(
    DayNum,
    WTeam.x,
    LTeam.x,
    WScore,
    LScore,
    WOffAdjEffBT,
    WDefAdjEffBT,
    LOffAdjEffBT,
    LDefAdjEffBT,
    WOffAdjEffKP,
    WDefAdjEffKP,
    LOffAdjEffKP,
    LDefAdjEffKP,
    GameID,
    WOffEFGPctBT,
    WDefEFGPctBT,
    LOffEFGPctBT,
    LDefEFGPctBT,
    WOffTOVPctBT,
    WDefTOVPctBT,
    LOffTOVPctBT,
    LDefTOVPctBT,
    WOffORBPctBT,
    WDefORBPctBT,
    LOffORBPctBT,
    LDefORBPctBT,
    WOffFTRateBT,
    WDefFTRateBT,
    LOffFTRateBT,
    LDefFTRateBT,
    Season
  ) %>%
    mutate(PtDiff = WScore - LScore,
           WinLoss = 1) %>%
    mutate(
      OffDiffBT = WOffAdjEffBT - LOffAdjEffBT,
      DefDiffBT = WDefAdjEffBT - LDefAdjEffKP,
      OffDiffKP = WOffAdjEffKP - LOffAdjEffKP,
      DefDiffKP = WDefAdjEffKP - LDefAdjEffKP,
      OffDiffEFG = WOffEFGPctBT - LOffEFGPctBT,
      DefDiffEFG = WDefEFGPctBT - LDefEFGPctBT,
      OffDiffTOV = WOffTOVPctBT - LOffTOVPctBT,
      DefDiffTOV = WDefTOVPctBT - LDefTOVPctBT,
      OffDiffORBPct = WOffORBPctBT - LOffORBPctBT,
      DefDiffORBPct = WDefORBPctBT - LDefORBPctBT,
      OffDiffFTRate = WOffFTRateBT - LOffFTRateBT,
      DefDiffFTRate = WDefFTRateBT - LDefFTRateBT
    ) %>%
    select(
      DayNum,
      WTeam.x,
      LTeam.x,
      WinLoss,
      OffDiffBT,
      DefDiffBT,
      OffDiffKP,
      DefDiffKP,
      GameID,
      OffDiffEFG,
      DefDiffEFG,
      OffDiffTOV,
      DefDiffTOV,
      OffDiffORBPct,
      DefDiffORBPct,
      OffDiffFTRate,
      DefDiffFTRate,
      Season
    ) %>%
    rename(WinTeam = WTeam.x, LostTeam = LTeam.x) %>%
    arrange(DayNum)
  
  trial.l = trial.w %>% mutate(
    OffDiffBT = -OffDiffBT,
    OffDiffKP = -OffDiffKP,
    DefDiffBT = -DefDiffBT,
    DefDiffKP = -DefDiffKP,
    OffDiffEFG = -OffDiffEFG,
    DefDiffEFG = -DefDiffEFG,
    OffDiffTOV = -OffDiffTOV,
    DefDiffTOV = -DefDiffTOV,
    OffDiffORBPct = -OffDiffORBPct,
    DefDiffORBPct = -DefDiffORBPct,
    OffDiffFTRate = -OffDiffFTRate,
    DefDiffFTRate = -DefDiffFTRate,
    WinLoss = 0
  )
  
  trials.1 = rbind(trial.w, trial.l) %>%
    mutate(random.sort = runif(1704)) %>%
    arrange(random.sort)
  
  trials.2 = rbind(trial.w, trial.l) %>%
    mutate(random.sort = runif(1704)) %>%
    arrange(-random.sort)
  
  ## randomly sampling from trials
  ## this gives us EITHER the winner or loser for each of the 1660 games in the dataset
  ## note: test.2 will contain ALL observations not in trials
  
  test.1 = trials.1[!duplicated(trials.1[, c('GameID')]), ] %>%
    select(-random.sort)
  
  test.2 = trials.2[!duplicated(trials.2[, c('GameID')]), ] %>%
    select(-random.sort)
  
  ## What does each dataset contain?
  
  # test.1 = all 660 games (GameID 1 to 660).
  # test.2 = the same games, but from the opposite "perspective"
  # for example, for GameID == 1:
  # test.1 contains the Duke "perspective," statistically, while test.2 contains Belmont's.
  
  ## Selecting a training sample and a test sample from test.1:
  # note: 70% of data will be used to train; 30% will be used to test.
  
  training.index = sample(seq_len(nrow(test.1)), size = 0.7 * nrow(test.1))
  
  training.test.1 = test.1[training.index, ]
  validation.test.1 = test.1[-training.index, ]
  
  output.data = list(training = training.test.1,validation = validation.test.1,all = test.2)
  output.data
}

yearSelector = function(year){
  trial.w.season = data %>% 
    filter(Season == year) %>% 
    select(
      DayNum,
      WTeam.x,
      LTeam.x,
      WScore,
      LScore,
      WOffAdjEffBT,
      WDefAdjEffBT,
      LOffAdjEffBT,
      LDefAdjEffBT,
      WOffAdjEffKP,
      WDefAdjEffKP,
      LOffAdjEffKP,
      LDefAdjEffKP,
      GameID,
      WOffEFGPctBT,
      WDefEFGPctBT,
      LOffEFGPctBT,
      LDefEFGPctBT,
      WOffTOVPctBT,
      WDefTOVPctBT,
      LOffTOVPctBT,
      LDefTOVPctBT,
      WOffORBPctBT,
      WDefORBPctBT,
      LOffORBPctBT,
      LDefORBPctBT,
      WOffFTRateBT,
      WDefFTRateBT,
      LOffFTRateBT,
      LDefFTRateBT
    ) %>%
    mutate(PtDiff = WScore - LScore,
           WinLoss = 1) %>%
    mutate(
      OffDiffBT = WOffAdjEffBT - LOffAdjEffBT,
      DefDiffBT = WDefAdjEffBT - LDefAdjEffKP,
      OffDiffKP = WOffAdjEffKP - LOffAdjEffKP,
      DefDiffKP = WDefAdjEffKP - LDefAdjEffKP,
      OffDiffEFG = WOffEFGPctBT - LOffEFGPctBT,
      DefDiffEFG = WDefEFGPctBT - LDefEFGPctBT,
      OffDiffTOV = WOffTOVPctBT - LOffTOVPctBT,
      DefDiffTOV = WDefTOVPctBT - LDefTOVPctBT,
      OffDiffORBPct = WOffORBPctBT - LOffORBPctBT,
      DefDiffORBPct = WDefORBPctBT - LDefORBPctBT,
      OffDiffFTRate = WOffFTRateBT - LOffFTRateBT,
      DefDiffFTRate = WDefFTRateBT - LDefFTRateBT
    ) %>%
    select(
      DayNum,
      WTeam.x,
      LTeam.x,
      WinLoss,
      OffDiffBT,
      DefDiffBT,
      OffDiffKP,
      DefDiffKP,
      GameID,
      OffDiffEFG,
      DefDiffEFG,
      OffDiffTOV,
      DefDiffTOV,
      OffDiffORBPct,
      DefDiffORBPct,
      OffDiffFTRate,
      DefDiffFTRate
    ) %>%
    rename(WinTeam = WTeam.x, LostTeam = LTeam.x) %>%
    arrange(DayNum)
  
  trial.l.season = trial.w.season %>% mutate(
    OffDiffBT = -OffDiffBT,
    OffDiffKP = -OffDiffKP,
    DefDiffBT = -DefDiffBT,
    DefDiffKP = -DefDiffKP,
    OffDiffEFG = -OffDiffEFG,
    DefDiffEFG = -DefDiffEFG,
    OffDiffTOV = -OffDiffTOV,
    DefDiffTOV = -DefDiffTOV,
    OffDiffORBPct = -OffDiffORBPct,
    DefDiffORBPct = -DefDiffORBPct,
    OffDiffFTRate = -OffDiffFTRate,
    DefDiffFTRate = -DefDiffFTRate,
    WinLoss = 0
  )
  
  trials.season = rbind(trial.w.season, trial.l.season)
  
  trials.season = trials.season %>%
    mutate(random.sort = runif(nrow(trials.season))) %>%
    arrange(random.sort)
  
  
  ## randomly sampling from trials
  ## this gives us EITHER the winner or loser for each of the 1660 games in the dataset
  ## note: test.2 will contain ALL observations not in trials
  
  output.season = trials.season[!duplicated(trials.season[, c('GameID')]), ] %>%
    select(-random.sort)
  
  output.season
}

allYearData = function() {  
  output = list(year08 = yearSelector(2008),
                year09 = yearSelector(2009),
                year10 = yearSelector(2010),
                year11 = yearSelector(2011),
                year12 = yearSelector(2012),
                year13 = yearSelector(2013),
                year14 = yearSelector(2014),
                year15 = yearSelector(2015),
                year16 = yearSelector(2016),
                year17 = yearSelector(2017),
                year18 = yearSelector(2018),
                year19 = yearSelector(2019),
                year21 = yearSelector(2021))
  output
}

LogLossTester = function(model, predict.set,typeIn = "response") {
  prediction = predict(model, predict.set, type = typeIn)
  test.result = predict.set %>% mutate(WinProb = prediction) %>%
    mutate(PredictWin = ifelse(WinProb >= 0.5, 1, 0)) %>%
    mutate(Correct = ifelse(PredictWin == WinLoss, 1, 0)) %>%
    mutate(confidence = ifelse(
      WinProb < 0.05 | WinProb > 0.95,
      "extreme",
      ifelse(
        WinProb < 0.20 | WinProb > 0.80,
        "high",
        ifelse(WinProb < 0.40 |
                 WinProb > 0.60, "medium", "low")
      )
    )) %>%
    mutate(confidence = factor(confidence, levels = c("low", "medium", "high", "extreme")))
  
  log.loss = LogLoss(test.result$WinProb, test.result$WinLoss)
  correct.summary = test.result %>% summarise(accuracy = sum(Correct) /
                                                n()) %>%
    mutate(logloss = log.loss)
  
  failures = test.result %>% filter(Correct == 0)
  
  performance.by.confidence = test.result %>%
    group_by(confidence) %>%
    summarise(correct = sum(Correct), total = n()) %>%
    mutate(percent = correct / total)
  
  output = list(
    Predictions = test.result,
    Failures = failures,
    Performance = correct.summary,
    ConfidencePerformance = performance.by.confidence
  )
  output
  
}

cleaned_data = dataSelector()

full_data_vector = cleaned_data$all %>% select(-DayNum,-WinTeam,-LostTeam,-GameID,-Season) %>%
  mutate(WinLoss = as.character(WinLoss)) %>%
  mutate(WinLoss = factor(WinLoss,levels = c("0","1")))

full_data = cleaned_data$all %>% select(-DayNum,-WinTeam,-LostTeam,-GameID,-Season)

sample = sample(nrow(full_data_vector),size = 0.7*nrow(full_data_vector))

## BUILDING KNN MODEL

knnModelTester = function(kval){
  
  train_data_vector = full_data_vector[sample,]
  test_data_vector = full_data_vector[-sample,]
  
  knn_model_result = class::knn(train_data_vector,test_data_vector,train_data_vector$WinLoss,k = kval)
  knn_table = table(knn_model_result,test_data_vector$WinLoss)
  
  knn_df = data.frame(predicted = knn_model_result,actual = test_data_vector$WinLoss) %>% 
    mutate(correct = ifelse(predicted == actual,1,0)) %>%
    summarise(pct_correct = sum(correct)/n()) %>%
    mutate(kvalue = kval)
  
  knn_df
}

kvals = seq(1:200)

kval_test = kvals %>%
  map_dfr(.f = ~knnModelTester(kval = .x))

plot(kval_test$kvalue,kval_test$pct_correct, type ="l")


train_data_vector = full_data_vector[sample,]
test_data_vector = full_data_vector[-sample,]

knn_model_result = class::knn(train_data_vector,test_data_vector,train_data_vector$WinLoss,k = 100,prob = T)
knn_table = table(knn_model_result,test_data_vector$WinLoss)

linearRegressionUsing10FoldCV = train(
  WinLoss ~ OffDiffEFG + DefDiffEFG +
    OffDiffTOV + DefDiffTOV +
    OffDiffORBPct + DefDiffORBPct +
    OffDiffKP + DefDiffKP +
    OffDiffBT + DefDiffBT,
  full_data,
  method = "glm",family = "binomial",
  trControl = trainControl(method = "cv",number = 10,verboseIter = F)
)


maxedModelPredictions = predict(linearRegressionUsing10FoldCV,full_data)


acc_check = full_data_vector %>% mutate(predicted = maxedModelPredictions) %>%
  mutate(guessed_outcome = ifelse(predicted >= 0.5,1,0)) %>%
  mutate(correct = ifelse(WinLoss == guessed_outcome,1,0))
