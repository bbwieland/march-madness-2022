
## libraries & global presets ----

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

cleaned_data = dataSelector()
training.test.1 = cleaned_data$training
validation.test.1 = cleaned_data$validation
test.2 = cleaned_data$all
data.by.year = allYearData()

last.five.data = test.2 %>% filter(Season >= 2016)

## now, we can train models on this training dataset + test them against the validation dataset
## this fixes a MAJOR error in the model-building-1.R code, which led it to return
## unrealistically low log-loss values & overfit the data.

## model building ----

# model.1 is designed to use JUST KenPom and Torvik efficiency inputs to predict!

model.1 = glm(WinLoss ~ OffDiffKP + DefDiffKP + OffDiffBT + DefDiffBT,
              test.2,
              family = "binomial")

model.1.interaction = glm(
  WinLoss ~ OffDiffKP + DefDiffKP + OffDiffBT + DefDiffBT +
    OffDiffKP * OffDiffBT + DefDiffKP * DefDiffBT,
  test.2,
  family = "binomial"
)

model.1.interaction.logloss = LogLossTester(model.1.interaction, test.2)
model.1.logloss = LogLossTester(model.1, test.2)

## note: I think the model w/ the interaction term is *probably* more sound moving forward.

## let's make up a "confidence" variable with 4 levels: low, medium, high, extreme:
# low for projected win prob 40%-60%
# medium for projected win prob 20-40% or 60-80%
# high for projected win prob 5-20% or 80-95%
# extreme for projected win prob 0-5% or 95-100%

# a table displaying performance by confidence is now appended to the LogLossTester function.
# call it via (LogLossObject)$ConfidencePerformance

# model.2 is designed to JUST use Dean Oliver's Four Factors of Winning!

model.2 = glm(
  WinLoss ~ OffDiffEFG + DefDiffEFG +
    OffDiffTOV + DefDiffTOV +
    OffDiffORBPct + DefDiffORBPct +
    OffDiffFTRate + DefDiffFTRate,
  test.2,
  family = "binomial"
)

model.2.logloss = LogLossTester(model.2, test.2)

# model.3 is designed to use *both* Four Factors and Efficiency data.

model.3 = glm(
  WinLoss ~ OffDiffEFG + DefDiffEFG +
    OffDiffTOV + DefDiffTOV +
    OffDiffORBPct + DefDiffORBPct +
    OffDiffFTRate + DefDiffFTRate +
    OffDiffKP + DefDiffKP +
    OffDiffBT + DefDiffBT,
  test.2,
  family = "binomial"
)

model.3.logloss = LogLossTester(model.3, test.2)

# model.4 is fit with the variables deemed significant in model 3.

library(MASS)

model.4 = stepAIC(model.3,direction = "both",trace = F)

detach("package:MASS")
model.4.logloss = LogLossTester(model.4,test.2)

# model.5 is fit with the last five years of data
model.5 = glm(
  WinLoss ~ OffDiffEFG + DefDiffEFG +
    OffDiffTOV + DefDiffTOV +
    OffDiffORBPct + DefDiffORBPct +
    OffDiffFTRate + DefDiffFTRate +
    OffDiffKP + DefDiffKP +
    OffDiffBT + DefDiffBT,
  last.five.data,
  family = "binomial"
)
library(MASS)
model.5 = stepAIC(model.5,direction = "both",trace = F)
detach("package:MASS")

model.5.logloss = LogLossTester(model.5,test.2)
## actual NCAA March Madness simulation ----

model.1.actual = yearMadnessReplicator(model.1)
model.2.actual = yearMadnessReplicator(model.2)
model.3.actual = yearMadnessReplicator(model.3)
model.4.actual = yearMadnessReplicator(model.4)
model.5.actual = yearMadnessReplicator(model.5)

actual.outcomes = rbind(model.1.actual,
                     model.2.actual,
                     model.3.actual) %>%
  mutate(model = c(rep(1, nrow(
    model.1.actual
  )), rep(2, nrow(
    model.1.actual
  )), rep(3, nrow(
    model.3.actual
  )))) %>%
  mutate(
    model_factor = case_when(
      model == 1 ~ "Efficiency",
      model == 2 ~ "Four Factors",
      model == 3 ~ "Combined",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(model_factor = factor(
    model_factor,
    levels = c("Combined", "Efficiency", "Four Factors")
  ))

plot.logloss.actual = ggplot(actual.outcomes, aes(x = model_factor, y = logloss)) +
  geom_boxplot(fill = "red", alpha = 0.3) +
  geom_jitter(width = 0.3, alpha = 0.3) +
  geom_hline(yintercept = 0.69, linetype = "dashed") +
  theme_custom() +
  labs(
    x = "Model",
    y = "Log-Loss",
    title = "Model Performance — Log-Loss",
    subtitle = "Each data point represents the outcome of a single season"
  )

plot.accuracy.actual = ggplot(actual.outcomes, aes(x = model_factor, y = accuracy)) +
  geom_boxplot(fill = "red", alpha = 0.3) +
  geom_jitter(width = 0.3,
              alpha = 0.3,
              color = "black") +
  theme_custom() +
  labs(
    x = "Model",
    y = "Accuracy",
    title = "Model Performance — Accuracy",
    subtitle = "Each data point represents the outcome of a single season"
  )
## random model simulation ----
# simulating NCAA tournaments for each model.

model.1.madness.sims = MadnessReplicator(500, model.1)
model.2.madness.sims = MadnessReplicator(500, model.2)
model.3.madness.sims = MadnessReplicator(500, model.3)

madness.sims = rbind(model.1.madness.sims,
                     model.2.madness.sims,
                     model.3.madness.sims) %>%
  mutate(model = c(rep(1, nrow(
    model.1.madness.sims
  )), rep(2, nrow(
    model.2.madness.sims
  )), rep(3, nrow(
    model.3.madness.sims
  )))) %>%
  mutate(
    model_factor = case_when(
      model == 1 ~ "Efficiency",
      model == 2 ~ "Four Factors",
      model == 3 ~ "Combined",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(model_factor = factor(
    model_factor,
    levels = c("Combined", "Efficiency", "Four Factors")
  ))

plot.logloss = ggplot(madness.sims, aes(x = model_factor, y = LogLoss)) +
  geom_boxplot(fill = "red", alpha = 0.3) +
  geom_jitter(width = 0.3, alpha = 0.3) +
  geom_hline(yintercept = 0.69, linetype = "dashed") +
  theme_custom() +
  labs(
    x = "Model",
    y = "Log-Loss",
    title = "Model Performance — Log-Loss",
    subtitle = "Each data point represents the outcome of a single March Madness simulation"
  )

ggsave(
  "logloss-performance.jpg",
  width = 8,
  height = 8,
  units = "in"
)

plot.accuracy = ggplot(madness.sims, aes(x = model_factor, y = Accuracy)) +
  geom_boxplot(fill = "red", alpha = 0.3) +
  geom_jitter(width = 0.3,
              alpha = 0.3,
              color = "black") +
  theme_custom() +
  labs(
    x = "Model",
    y = "Accuracy",
    title = "Model Performance — Accuracy",
    subtitle = "Each data point represents the outcome of a single March Madness simulation"
  )

ggsave(
  "accuracy-performance.jpg",
  width = 8,
  height = 8,
  units = "in"
)
