# NIR FGM: NIR Individual Dates Model Building - Honeycrisp, XGBoost
# author: Jimmy Larson
# created: 7.27.21
# last edited: 10.5.21

## load packages----
library(tidyverse)
library(xgboost)
## set seed----
set.seed(123)
## read in data----
meas1 <- read_csv("data/2021/honeycrisp/model_building/meas1_spectra.csv")
meas1Smoted <- read_csv("data/2021/honeycrisp/model_building/meas1Smoted.csv")
meas2 <- read_csv("data/2021/honeycrisp/model_building/meas2_spectra.csv")
meas2Smoted <- read_csv("data/2021/honeycrisp/model_building/meas2Smoted.csv")
meas3 <- read_csv("data/2021/honeycrisp/model_building/meas3_spectra.csv")
meas3Smoted <- read_csv("data/2021/honeycrisp/model_building/meas3Smoted.csv")
meas4 <- read_csv("data/2021/honeycrisp/model_building/meas4_spectra.csv")
meas4Smoted <- read_csv("data/2021/honeycrisp/model_building/meas4Smoted.csv")
meas5 <- read_csv("data/2021/honeycrisp/model_building/meas5_spectra.csv")
meas5Smoted <- read_csv("data/2021/honeycrisp/model_building/meas5Smoted.csv")
meas6 <- read_csv("data/2021/honeycrisp/model_building/meas6_spectra.csv")
meas6Smoted <- read_csv("data/2021/honeycrisp/model_building/meas6Smoted.csv")
meas7 <- read_csv("data/2021/honeycrisp/model_building/meas7_spectra.csv")
meas7Smoted <- read_csv("data/2021/honeycrisp/model_building/meas7Smoted.csv")
diff1 <- read_csv("data/2021/honeycrisp/model_building/diff1_spectra.csv")
diff1Smoted <- read_csv("data/2021/honeycrisp/model_building/diff1Smoted.csv")
diff2 <- read_csv("data/2021/honeycrisp/model_building/diff2_spectra.csv")
diff2Smoted <- read_csv("data/2021/honeycrisp/model_building/diff2Smoted.csv")
diff3 <- read_csv("data/2021/honeycrisp/model_building/diff3_spectra.csv")
diff3Smoted <- read_csv("data/2021/honeycrisp/model_building/diff3Smoted.csv")
diff4 <- read_csv("data/2021/honeycrisp/model_building/diff4_spectra.csv")
diff4Smoted <- read_csv("data/2021/honeycrisp/model_building/diff4Smoted.csv")
diff5 <- read_csv("data/2021/honeycrisp/model_building/diff5_spectra.csv")
diff5Smoted <- read_csv("data/2021/honeycrisp/model_building/diff5Smoted.csv")
diff6 <- read_csv("data/2021/honeycrisp/model_building/diff6_spectra.csv")
diff6Smoted <- read_csv("data/2021/honeycrisp/model_building/diff6Smoted.csv")
## build XGBoost model for ind. dates----
### meas1
meas1 %>%
  drop_na() %>%
  select(persistCode, 6:311) -> meas1
meas1Cases <- as.data.frame(table(meas1$persistCode))
meas1Per <- meas1Cases[1,2]
meas1Abs <- meas1Cases[2,2]
meas1 <- data.matrix(meas1)
xgbMeas1data <- xgb.DMatrix(meas1[,-1], label = meas1[,1])
xgbMeas1 <- xgb.cv(data = xgbMeas1data,
                   nround=3,
                   nfold = 5,
                   metrics = list("error","rmse","auc"),
                   scale_pos_weight = meas1Abs/meas1Per,
                   max_depth = 4, 
                   eta = .3,
                   prediction = TRUE,
                   objective = "binary:logistic")
### meas2
meas2 %>%
  drop_na() %>%
  select(persistCode, 6:311) -> meas2
meas2Cases <- as.data.frame(table(meas2$persistCode))
meas2Per <- meas2Cases[1,2]
meas2Abs <- meas2Cases[2,2]
meas2 <- data.matrix(meas2)
xgbMeas2data <- xgb.DMatrix(meas2[,-1], label = meas2[,1])
xgbMeas2 <- xgb.cv(data = xgbMeas2data,
                   nround=3,
                   nfold = 5,
                   metrics = list("error","rmse","auc"),
                   scale_pos_weight = meas2Abs/meas2Per,
                   max_depth = 4, 
                   eta = .3,
                   objective = "binary:logistic")
### meas3
meas3 %>%
  drop_na() %>%
  select(persistCode, 6:311) -> meas3
meas3Cases <- as.data.frame(table(meas3$persistCode))
meas3Per <- meas3Cases[1,2]
meas3Abs <- meas3Cases[2,2]
meas3 <- data.matrix(meas3)
xgbMeas3data <- xgb.DMatrix(meas3[,-1], label = meas3[,1])
xgbMeas3cv <- xgb.cv(data = xgbMeas3data,
                     nround=3,
                     nfold = 5,
                     metrics = list("error","rmse","auc"),
                     scale_pos_weight = meas3Abs/meas3Per,
                     max_depth = 4, 
                     eta = .3,
                     objective = "binary:logistic")
### meas4   
meas4 %>%
  drop_na() %>%
  select(persistCode, 6:311)-> meas4
meas4Cases <- as.data.frame(table(meas4$persistCode))
meas4Per <- meas4Cases[1,2]
meas4Abs <- meas4Cases[2,2]
meas4 <- data.matrix(meas4)
xgbMeas4data <- xgb.DMatrix(meas4[,-1], label = meas4[,1])
xgbMeas4 <- xgb.cv(data = xgbMeas4data,
                   nround=3,
                   nfold = 5,
                   metrics = list("error","rmse","auc"),
                   scale_pos_weight = meas4Abs/meas4Per,
                   max_depth = 4, 
                   eta = .3,
                   objective = "binary:logistic")
### meas5
meas5 %>%
  drop_na() %>%
  select(persistCode, 6:311)-> meas5
meas5Cases <- as.data.frame(table(meas5$persistCode))
meas5Per <- meas5Cases[1,2]
meas5Abs <- meas5Cases[2,2]
meas5 <- data.matrix(meas5)
xgbMeas5data <- xgb.DMatrix(meas5[,-1], label = meas5[,1])
xgbMeas5 <- xgb.cv(data = xgbMeas5data,
                   nround=3,
                   nfold = 5,
                   metrics = list("error","rmse","auc"),
                   scale_pos_weight = meas5Per/meas5Abs,
                   max_depth = 4, 
                   eta = .3,
                   objective = "binary:logistic")
### meas6
meas6 %>%
  drop_na() %>%
  select(persistCode, 6:311)-> meas6
meas6Cases <- as.data.frame(table(meas6$persistCode))
meas6Per <- meas6Cases[1,2]
meas6Abs <- meas6Cases[2,2]
meas6 <- data.matrix(meas6)
xgbMeas6data <- xgb.DMatrix(meas6[,-1], label = meas6[,1])
xgbMeas6 <- xgb.cv(data = xgbMeas6data,
                   nround=3,
                   nfold = 5,
                   metrics = list("error","rmse","auc"),
                   scale_pos_weight = meas6Abs/meas6Per,
                   max_depth = 4, 
                   eta = .3,
                   objective = "binary:logistic")
### meas7
meas7 %>%
  drop_na() %>%
  select(persistCode, 6:311)-> meas7
meas7Cases <- as.data.frame(table(meas7$persistCode))
meas7Per <- meas7Cases[1,2]
meas7Abs <- meas7Cases[2,2]
meas7 <- data.matrix(meas7)
xgbMeas7data <- xgb.DMatrix(meas7[,-1], label = meas7[,1])
xgbMeas7 <- xgb.cv(data = xgbMeas7data,
                   nround=3,
                   nfold = 5,
                   metrics = list("error","rmse","auc"),
                   scale_pos_weight = meas7Abs/meas7Per,
                   max_depth = 4, 
                   eta = .3,
                   objective = "binary:logistic")
## build XGBoost for ind. dates with SMOTE----
### meas1
meas1SmotedMatrix <- data.matrix(meas1Smoted)
xgbMeas1Smoted <- xgb.DMatrix(meas1SmotedMatrix[,-1], label = meas1SmotedMatrix[,1])
xgbMeas1SmotedCv <- xgb.cv(data = xgbMeas1Smoted,
                           nfold = 5,
                           nrounds = 3,
                           max_depth = 4,
                           eta = .3,
                           objective = "binary:logistic",
                           prediction = TRUE,
                           metrics = list("error","rmse","auc"))
### meas2
meas2SmotedMatrix <- data.matrix(meas2Smoted)
xgbMeas2Smoted <- xgb.DMatrix(meas2SmotedMatrix[,-1], label = meas2SmotedMatrix[,1])
xgbMeas2SmotedCv <- xgb.cv(data = xgbMeas2Smoted,
                           nfold = 5,
                           nrounds = 3,
                           max_depth = 4,
                           eta = .3,
                           objective = "binary:logistic",
                           prediction = TRUE,
                           metrics = list("error","rmse","auc"))
### meas3
meas3SmotedMatrix <- data.matrix(meas3Smoted)
xgbMeas3Smoted <- xgb.DMatrix(meas3SmotedMatrix[,-1], label = meas3SmotedMatrix[,1])
xgbMeas3SmotedCv <- xgb.cv(data = xgbMeas3Smoted,
                          nfold = 5,
                          nrounds = 3,
                          max_depth = 4,
                          eta = .3,
                          objective = "binary:logistic",
                          prediction = TRUE,
                          metrics = list("error","rmse","auc"))
### meas4
meas4SmotedMatrix <- data.matrix(meas4Smoted)
xgbmeas4Smoted <- xgb.DMatrix(meas4SmotedMatrix[,-1], label = meas4SmotedMatrix[,1])
xgbmeas4SmotedCv <- xgb.cv(data = xgbmeas4Smoted,
                           nfold = 5,
                           nrounds = 3,
                           max_depth = 4,
                           eta = .3,
                           objective = "binary:logistic",
                           prediction = TRUE,
                           metrics = list("error","rmse","auc"))
### meas5
meas5SmotedMatrix <- data.matrix(meas5Smoted)
xgbmeas5Smoted <- xgb.DMatrix(meas5SmotedMatrix[,-1], label = meas5SmotedMatrix[,1])
xgbmeas5SmotedCv <- xgb.cv(data = xgbmeas5Smoted,
                           nfold = 5,
                           nrounds = 3,
                           max_depth = 4,
                           eta = .3,
                           objective = "binary:logistic",
                           prediction = TRUE,
                           metrics = list("error","rmse","auc"))
### meas6
meas6SmotedMatrix <- data.matrix(meas6Smoted)
xgbmeas6Smoted <- xgb.DMatrix(meas6SmotedMatrix[,-1], label = meas6SmotedMatrix[,1])
xgbmeas6SmotedCv <- xgb.cv(data = xgbmeas6Smoted,
                           nfold = 5,
                           nrounds = 3,
                           max_depth = 4,
                           eta = .3,
                           objective = "binary:logistic",
                           prediction = TRUE,
                           metrics = list("error","rmse","auc"))
### meas7
meas7SmotedMatrix <- data.matrix(meas7Smoted)
xgbmeas7Smoted <- xgb.DMatrix(meas7SmotedMatrix[,-1], label = meas7SmotedMatrix[,1])
xgbmeas7SmotedCv <- xgb.cv(data = xgbmeas7Smoted,
                           nfold = 5,
                           nrounds = 3,
                           max_depth = 4,
                           eta = .3,
                           objective = "binary:logistic",
                           prediction = TRUE,
                           metrics = list("error","rmse","auc"))
## XGBoost for differences in dates----
### diff1
diff1 %>%
  drop_na() %>%
  select(persistCode, 6:311) -> diff1
diff1Cases <- as.data.frame(table(diff1$persistCode))
diff1Per <- diff1Cases[1,2]
diff1Abs <- diff1Cases[2,2]
diff1Matrix <- data.matrix(diff1)
xgbDiff1Data <- xgb.DMatrix(diff1Matrix[,-1], label= diff1Matrix[,1])
xgbDiff1Cv <- xgb.cv(data = xgbDiff1Data,
                     nfold = 5,
                     nrounds = 3,
                     scale_pos_weight = diff1Abs/diff1Per,
                     max_depth = 4,
                     eta = .3,
                     objective = "binary:logistic",
                     prediction = TRUE,
                     metrics = list("error", "rmse", "auc"))
### diff2
diff2 %>%
  drop_na() %>%
  select(persistCode, 6:311) -> diff2
diff2Cases <- as.data.frame(table(diff2$persistCode))
diff2Per <- diff2Cases[1,2]
diff2Abs <- diff2Cases[2,2]
diff2Matrix <- data.matrix(diff2)
xgbdiff2Data <- xgb.DMatrix(diff2Matrix[,-1], label= diff2Matrix[,1])
xgbdiff2Cv <- xgb.cv(data = xgbdiff2Data,
                     nfold = 5,
                     nrounds = 3,
                     scale_pos_weight = diff2Abs/diff2Per,
                     max_depth = 4,
                     eta = .3,
                     objective = "binary:logistic",
                     prediction = TRUE,
                     metrics = list("error", "rmse", "auc"))
### diff3
diff3 %>%
  drop_na() %>%
  select(persistCode, 6:311) -> diff3
diff3Cases <- as.data.frame(table(diff3$persistCode))
diff3Per <- diff3Cases[1,2]
diff3Abs <- diff3Cases[2,2]
diff3Matrix <- data.matrix(diff3)
xgbdiff3Data <- xgb.DMatrix(diff3Matrix[,-1], label= diff3Matrix[,1])
xgbdiff3Cv <- xgb.cv(data = xgbdiff3Data,
                     nfold = 5,
                     nrounds = 3,
                     scale_pos_weight = diff3Abs/diff3Per,
                     max_depth = 4,
                     eta = .3,
                     objective = "binary:logistic",
                     prediction = TRUE,
                     metrics = list("error", "rmse", "auc"))
### diff4
diff4 %>%
  drop_na() %>%
  select(persistCode, 6:311) -> diff4
diff4Cases <- as.data.frame(table(diff4$persistCode))
diff4Per <- diff4Cases[1,2]
diff4Abs <- diff4Cases[2,2]
diff4Matrix <- data.matrix(diff4)
xgbdiff4Data <- xgb.DMatrix(diff4Matrix[,-1], label= diff4Matrix[,1])
xgbdiff4Cv <- xgb.cv(data = xgbdiff4Data,
                     nfold = 5,
                     nrounds = 3,
                     scale_pos_weight = diff4Abs/diff4Per,
                     max_depth = 4,
                     eta = .3,
                     objective = "binary:logistic",
                     prediction = TRUE,
                     metrics = list("error", "rmse", "auc"))
### diff5
diff5 %>%
  drop_na() %>%
  select(persistCode, 6:311) -> diff5
diff5Cases <- as.data.frame(table(diff5$persistCode))
diff5Per <- diff5Cases[1,2]
diff5Abs <- diff5Cases[2,2]
diff5Matrix <- data.matrix(diff5)
xgbdiff5Data <- xgb.DMatrix(diff5Matrix[,-1], label= diff5Matrix[,1])
xgbdiff5Cv <- xgb.cv(data = xgbdiff5Data,
                     nfold = 5,
                     nrounds = 3,
                     scale_pos_weight = diff5Abs/diff5Per,
                     max_depth = 4,
                     eta = .3,
                     objective = "binary:logistic",
                     prediction = TRUE,
                     metrics = list("error", "rmse", "auc"))
### diff6
diff6 %>%
  drop_na() %>%
  select(persistCode, 6:311) -> diff6
diff6Cases <- as.data.frame(table(diff6$persistCode))
diff6Per <- diff6Cases[1,2]
diff6Abs <- diff6Cases[2,2]
diff6Matrix <- data.matrix(diff6)
xgbdiff6Data <- xgb.DMatrix(diff6Matrix[,-1], label= diff6Matrix[,1])
xgbdiff6Cv <- xgb.cv(data = xgbdiff6Data,
                     nfold = 5,
                     nrounds = 3,
                     scale_pos_weight = diff6Abs/diff6Per,
                     max_depth = 4,
                     eta = .3,
                     objective = "binary:logistic",
                     prediction = TRUE,
                     metrics = list("error", "rmse", "auc"))
## XGBoost for diff with SMOTE----
### diff1
diff1SmotedMatrix <- data.matrix(diff1Smoted)
xgbdiff1Smoted <- xgb.DMatrix(diff1SmotedMatrix[,-1], label = diff1SmotedMatrix[,1])
xgbdiff1SmotedCv <- xgb.cv(data = xgbdiff1Smoted,
                           nfold = 5,
                           nrounds = 3,
                           max_depth = 4,
                           eta = .3,
                           objective = "binary:logistic",
                           prediction = TRUE,
                           metrics = list("error","rmse","auc"))
### diff2
diff2SmotedMatrix <- data.matrix(diff2Smoted)
xgbdiff2Smoted <- xgb.DMatrix(diff2SmotedMatrix[,-1], label = diff2SmotedMatrix[,1])
xgbdiff2SmotedCv <- xgb.cv(data = xgbdiff2Smoted,
                           nfold = 5,
                           nrounds = 3,
                           max_depth = 4,
                           eta = .3,
                           objective = "binary:logistic",
                           prediction = TRUE,
                           metrics = list("error","rmse","auc"))
### diff3
diff3SmotedMatrix <- data.matrix(diff3Smoted)
xgbdiff3Smoted <- xgb.DMatrix(diff3SmotedMatrix[,-1], label = diff3SmotedMatrix[,1])
xgbdiff3SmotedCv <- xgb.cv(data = xgbdiff3Smoted,
                           nfold = 5,
                           nrounds = 3,
                           max_depth = 4,
                           eta = .3,
                           objective = "binary:logistic",
                           prediction = TRUE,
                           metrics = list("error","rmse","auc"))
### diff4
diff4SmotedMatrix <- data.matrix(diff4Smoted)
xgbdiff4Smoted <- xgb.DMatrix(diff4SmotedMatrix[,-1], label = diff4SmotedMatrix[,1])
xgbdiff4SmotedCv <- xgb.cv(data = xgbdiff4Smoted,
                           nfold = 5,
                           nrounds = 3,
                           max_depth = 4,
                           eta = .3,
                           objective = "binary:logistic",
                           prediction = TRUE,
                           metrics = list("error","rmse","auc"))
### diff5
diff5SmotedMatrix <- data.matrix(diff5Smoted)
xgbdiff5Smoted <- xgb.DMatrix(diff5SmotedMatrix[,-1], label = diff5SmotedMatrix[,1])
xgbdiff5SmotedCv <- xgb.cv(data = xgbdiff5Smoted,
                           nfold = 5,
                           nrounds = 3,
                           max_depth = 4,
                           eta = .3,
                           objective = "binary:logistic",
                           prediction = TRUE,
                           metrics = list("error","rmse","auc"))
### diff6
diff6SmotedMatrix <- data.matrix(diff6Smoted)
xgbdiff6Smoted <- xgb.DMatrix(diff6SmotedMatrix[,-1], label = diff6SmotedMatrix[,1])
xgbdiff6SmotedCv <- xgb.cv(data = xgbdiff6Smoted,
                           nfold = 5,
                           nrounds = 3,
                           max_depth = 4,
                           eta = .3,
                           objective = "binary:logistic",
                           prediction = TRUE,
                           metrics = list("error","rmse","auc"))
## XGBoost variable importance for ind. dates with SMOTE----
### meas1
xgbMeas1SmotedTrain <- xgb.train(data = xgbMeas1Smoted,
                           nfold = 5,
                           nrounds = 3,
                           max_depth = 4,
                           eta = .3,
                           objective = "binary:logistic")
xgbMeas1SmotedImp <- xgb.importance(feature_names = colnames(meas1Smoted[,-1]), model = xgbMeas1SmotedTrain)
xgb.plot.importance(importance_matrix = xgbMeas1SmotedImp[1:20])
xgbMeas1SmotedImp %>%
  filter(Gain >= 0.025) -> xgbMeas1SmotedImpFeat
xgbMeas1SmotedSelectedFeat <- xgbMeas1SmotedImpFeat$Feature
meas1Smoted %>%
  select(`meas1Smote$Y`, xgbMeas1SmotedSelectedFeat) %>%
  data.matrix() -> meas1SmotedSelectedMatrix
xgbMeas1SmotedSelected <- xgb.DMatrix(meas1SmotedSelectedMatrix[,-1], label = meas1SmotedSelectedMatrix[,1])
xgbMeas1SmotedSelectedCv <- xgb.cv(data = xgbMeas1SmotedSelected,
                           nfold = 5,
                           nrounds = 3,
                           max_depth = 4,
                           eta = .3,
                           objective = "binary:logistic",
                           prediction = TRUE,
                           metrics = list("error","rmse","auc"))
### meas2
xgbMeas2SmotedTrain <- xgb.train(data = xgbMeas2Smoted,
                                 nfold = 5,
                                 nrounds = 3,
                                 max_depth = 4,
                                 eta = .3,
                                 objective = "binary:logistic")
xgbMeas2SmotedImp <- xgb.importance(feature_names = colnames(meas2Smoted[,-1]), model = xgbMeas2SmotedTrain)
xgb.plot.importance(importance_matrix = xgbMeas2SmotedImp[1:20])
xgbMeas2SmotedImp %>%
  filter(Gain >= 0.025) -> xgbMeas2SmotedImpFeat
xgbMeas2SmotedSelectedFeat <- xgbMeas2SmotedImpFeat$Feature
meas2Smoted %>%
  select(`meas2Smote$Y`, xgbMeas2SmotedSelectedFeat) %>%
  data.matrix() -> meas2SmotedSelectedMatrix
xgbMeas2SmotedSelected <- xgb.DMatrix(meas2SmotedSelectedMatrix[,-1], label = meas2SmotedSelectedMatrix[,1])
xgbMeas2SmotedSelectedCv <- xgb.cv(data = xgbMeas2SmotedSelected,
                                   nfold = 5,
                                   nrounds = 3,
                                   max_depth = 4,
                                   eta = .3,
                                   objective = "binary:logistic",
                                   prediction = TRUE,
                                   metrics = list("error","rmse","auc"))
### meas3
xgbMeas3SmotedTrain <- xgb.train(data = xgbMeas3Smoted,
                                 nfold = 5,
                                 nrounds = 3,
                                 max_depth = 4,
                                 eta = .3,
                                 objective = "binary:logistic")
xgbMeas3SmotedImp <- xgb.importance(feature_names = colnames(meas3Smoted[,-1]), model = xgbMeas3SmotedTrain)
xgb.plot.importance(importance_matrix = xgbMeas3SmotedImp[1:20])
xgbMeas3SmotedImp %>%
  filter(Gain >= 0.025) -> xgbMeas3SmotedImpFeat
xgbMeas3SmotedSelectedFeat <- xgbMeas3SmotedImpFeat$Feature
meas3Smoted %>%
  select(`meas3Smote$Y`, xgbMeas3SmotedSelectedFeat) %>%
  data.matrix() -> meas3SmotedSelectedMatrix
xgbMeas3SmotedSelected <- xgb.DMatrix(meas3SmotedSelectedMatrix[,-1], label = meas3SmotedSelectedMatrix[,1])
xgbMeas3SmotedSelectedCv <- xgb.cv(data = xgbMeas3SmotedSelected,
                                   nfold = 5,
                                   nrounds = 3,
                                   max_depth = 4,
                                   eta = .3,
                                   objective = "binary:logistic",
                                   prediction = TRUE,
                                   metrics = list("error","rmse","auc"))
### meas4
xgbMeas4SmotedTrain <- xgb.train(data = xgbmeas4Smoted,
                                 nfold = 5,
                                 nrounds = 3,
                                 max_depth = 4,
                                 eta = .3,
                                 objective = "binary:logistic")
xgbMeas4SmotedImp <- xgb.importance(feature_names = colnames(meas4Smoted[,-1]), model = xgbMeas4SmotedTrain)
xgb.plot.importance(importance_matrix = xgbMeas4SmotedImp[1:20])
xgbMeas4SmotedImp %>%
  filter(Gain >= 0.025) -> xgbMeas4SmotedImpFeat
xgbMeas4SmotedSelectedFeat <- xgbMeas4SmotedImpFeat$Feature
meas4Smoted %>%
  select(`meas4Smote$Y`, xgbMeas4SmotedSelectedFeat) %>%
  data.matrix() -> meas4SmotedSelectedMatrix
xgbMeas4SmotedSelected <- xgb.DMatrix(meas4SmotedSelectedMatrix[,-1], label = meas4SmotedSelectedMatrix[,1])
xgbMeas4SmotedSelectedCv <- xgb.cv(data = xgbMeas4SmotedSelected,
                                   nfold = 5,
                                   nrounds = 3,
                                   max_depth = 4,
                                   eta = .3,
                                   objective = "binary:logistic",
                                   prediction = TRUE,
                                   metrics = list("error","rmse","auc"))
### meas5
xgbMeas5SmotedTrain <- xgb.train(data = xgbmeas5Smoted,
                                 nfold = 5,
                                 nrounds = 3,
                                 max_depth = 4,
                                 eta = .3,
                                 objective = "binary:logistic")
xgbMeas5SmotedImp <- xgb.importance(feature_names = colnames(meas5Smoted[,-1]), model = xgbMeas5SmotedTrain)
xgb.plot.importance(importance_matrix = xgbMeas5SmotedImp[1:20])
xgbMeas5SmotedImp %>%
  filter(Gain >= 0.025) -> xgbMeas5SmotedImpFeat
xgbMeas5SmotedSelectedFeat <- xgbMeas5SmotedImpFeat$Feature
meas5Smoted %>%
  select(`meas5Smote$Y`, xgbMeas5SmotedSelectedFeat) %>%
  data.matrix() -> meas5SmotedSelectedMatrix
xgbMeas5SmotedSelected <- xgb.DMatrix(meas5SmotedSelectedMatrix[,-1], label = meas5SmotedSelectedMatrix[,1])
xgbMeas5SmotedSelectedCv <- xgb.cv(data = xgbMeas5SmotedSelected,
                                   nfold = 5,
                                   nrounds = 3,
                                   max_depth = 4,
                                   eta = .3,
                                   objective = "binary:logistic",
                                   prediction = TRUE,
                                   metrics = list("error","rmse","auc"))
### meas6
xgbMeas6SmotedTrain <- xgb.train(data = xgbmeas6Smoted,
                                 nfold = 5,
                                 nrounds = 3,
                                 max_depth = 4,
                                 eta = .3,
                                 objective = "binary:logistic")
xgbMeas6SmotedImp <- xgb.importance(feature_names = colnames(meas6Smoted[,-1]), model = xgbMeas6SmotedTrain)
xgb.plot.importance(importance_matrix = xgbMeas6SmotedImp[1:20])
xgbMeas6SmotedImp %>%
  filter(Gain >= 0.025) -> xgbMeas6SmotedImpFeat
xgbMeas6SmotedSelectedFeat <- xgbMeas6SmotedImpFeat$Feature
meas6Smoted %>%
  select(`meas6Smote$Y`, xgbMeas6SmotedSelectedFeat) %>%
  data.matrix() -> meas6SmotedSelectedMatrix
xgbMeas6SmotedSelected <- xgb.DMatrix(meas6SmotedSelectedMatrix[,-1], label = meas6SmotedSelectedMatrix[,1])
xgbMeas6SmotedSelectedCv <- xgb.cv(data = xgbMeas6SmotedSelected,
                                   nfold = 5,
                                   nrounds = 3,
                                   max_depth = 4,
                                   eta = .3,
                                   objective = "binary:logistic",
                                   prediction = TRUE,
                                   metrics = list("error","rmse","auc"))
### meas7
xgbMeas7SmotedTrain <- xgb.train(data = xgbmeas7Smoted,
                                 nfold = 5,
                                 nrounds = 3,
                                 max_depth = 4,
                                 eta = .3,
                                 objective = "binary:logistic")
xgbMeas7SmotedImp <- xgb.importance(feature_names = colnames(meas7Smoted[,-1]), model = xgbMeas7SmotedTrain)
xgb.plot.importance(importance_matrix = xgbMeas7SmotedImp[1:20])
xgbMeas7SmotedImp %>%
  filter(Gain >= 0.025) -> xgbMeas7SmotedImpFeat
xgbMeas7SmotedSelectedFeat <- xgbMeas7SmotedImpFeat$Feature
meas7Smoted %>%
  select(`meas7Smote$Y`, xgbMeas7SmotedSelectedFeat) %>%
  data.matrix() -> meas7SmotedSelectedMatrix
xgbMeas7SmotedSelected <- xgb.DMatrix(meas7SmotedSelectedMatrix[,-1], label = meas7SmotedSelectedMatrix[,1])
xgbMeas7SmotedSelectedCv <- xgb.cv(data = xgbMeas7SmotedSelected,
                                   nfold = 5,
                                   nrounds = 3,
                                   max_depth = 4,
                                   eta = .3,
                                   objective = "binary:logistic",
                                   prediction = TRUE,
                                   metrics = list("error","rmse","auc"))

## XGBoost variable importance for differences in dates with SMOTE----
### diff1
xgbDiff1SmotedTrain <- xgb.train(data = xgbdiff1Smoted,
                                 nfold = 5,
                                 nrounds = 3,
                                 max_depth = 4,
                                 eta = .3,
                                 objective = "binary:logistic")
xgbDiff1SmotedImp <- xgb.importance(feature_names = colnames(diff1Smoted[,-1]), model = xgbDiff1SmotedTrain)
xgb.plot.importance(importance_matrix = xgbDiff1SmotedImp[1:20])
xgbDiff1SmotedImp %>%
  filter(Gain >= 0.025) -> xgbDiff1SmotedImpFeat
xgbDiff1SmotedSelectedFeat <- xgbDiff1SmotedImpFeat$Feature
diff1Smoted %>%
  select(`diff1Smote$Y`, xgbDiff1SmotedSelectedFeat) %>%
  data.matrix() -> diff1SmotedSelectedMatrix
xgbDiff1SmotedSelected <- xgb.DMatrix(diff1SmotedSelectedMatrix[,-1], label = diff1SmotedSelectedMatrix[,1])
xgbDiff1SmotedSelectedCv <- xgb.cv(data = xgbDiff1SmotedSelected,
                                   nfold = 5,
                                   nrounds = 3,
                                   max_depth = 4,
                                   eta = .3,
                                   objective = "binary:logistic",
                                   prediction = TRUE,
                                   metrics = list("error","rmse","auc"))
### diff2
xgbDiff2SmotedTrain <- xgb.train(data = xgbdiff2Smoted,
                                 nfold = 5,
                                 nrounds = 3,
                                 max_depth = 4,
                                 eta = .3,
                                 objective = "binary:logistic")
xgbDiff2SmotedImp <- xgb.importance(feature_names = colnames(diff2Smoted[,-1]), model = xgbDiff2SmotedTrain)
xgb.plot.importance(importance_matrix = xgbDiff2SmotedImp[1:20])
xgbDiff2SmotedImp %>%
  filter(Gain >= 0.025) -> xgbDiff2SmotedImpFeat
xgbDiff2SmotedSelectedFeat <- xgbDiff2SmotedImpFeat$Feature
diff2Smoted %>%
  select(`diff2Smote$Y`, xgbDiff2SmotedSelectedFeat) %>%
  data.matrix() -> diff2SmotedSelectedMatrix
xgbDiff2SmotedSelected <- xgb.DMatrix(diff2SmotedSelectedMatrix[,-1], label = diff2SmotedSelectedMatrix[,1])
xgbDiff2SmotedSelectedCv <- xgb.cv(data = xgbDiff2SmotedSelected,
                                   nfold = 5,
                                   nrounds = 3,
                                   max_depth = 4,
                                   eta = .3,
                                   objective = "binary:logistic",
                                   prediction = TRUE,
                                   metrics = list("error","rmse","auc"))
### diff3
xgbDiff3SmotedTrain <- xgb.train(data = xgbdiff3Smoted,
                                 nfold = 5,
                                 nrounds = 3,
                                 max_depth = 4,
                                 eta = .3,
                                 objective = "binary:logistic")
xgbDiff3SmotedImp <- xgb.importance(feature_names = colnames(diff3Smoted[,-1]), model = xgbDiff3SmotedTrain)
xgb.plot.importance(importance_matrix = xgbDiff3SmotedImp[1:20])
xgbDiff3SmotedImp %>%
  filter(Gain >= 0.025) -> xgbDiff3SmotedImpFeat
xgbDiff3SmotedSelectedFeat <- xgbDiff3SmotedImpFeat$Feature
diff3Smoted %>%
  select(`diff3Smote$Y`, xgbDiff3SmotedSelectedFeat) %>%
  data.matrix() -> diff3SmotedSelectedMatrix
xgbDiff3SmotedSelected <- xgb.DMatrix(diff3SmotedSelectedMatrix[,-1], label = diff3SmotedSelectedMatrix[,1])
xgbDiff3SmotedSelectedCv <- xgb.cv(data = xgbDiff3SmotedSelected,
                                   nfold = 5,
                                   nrounds = 3,
                                   max_depth = 4,
                                   eta = .3,
                                   objective = "binary:logistic",
                                   prediction = TRUE,
                                   metrics = list("error","rmse","auc"))
### diff4
xgbDiff4SmotedTrain <- xgb.train(data = xgbdiff4Smoted,
                                 nfold = 5,
                                 nrounds = 3,
                                 max_depth = 4,
                                 eta = .3,
                                 objective = "binary:logistic")
xgbDiff4SmotedImp <- xgb.importance(feature_names = colnames(diff4Smoted[,-1]), model = xgbDiff4SmotedTrain)
xgb.plot.importance(importance_matrix = xgbDiff4SmotedImp[1:20])
xgbDiff4SmotedImp %>%
  filter(Gain >= 0.025) -> xgbDiff4SmotedImpFeat
xgbDiff4SmotedSelectedFeat <- xgbDiff4SmotedImpFeat$Feature
diff4Smoted %>%
  select(`diff4Smote$Y`, xgbDiff4SmotedSelectedFeat) %>%
  data.matrix() -> diff4SmotedSelectedMatrix
xgbDiff4SmotedSelected <- xgb.DMatrix(diff4SmotedSelectedMatrix[,-1], label = diff4SmotedSelectedMatrix[,1])
xgbDiff4SmotedSelectedCv <- xgb.cv(data = xgbDiff4SmotedSelected,
                                   nfold = 5,
                                   nrounds = 3,
                                   max_depth = 4,
                                   eta = .3,
                                   objective = "binary:logistic",
                                   prediction = TRUE,
                                   metrics = list("error","rmse","auc"))
### diff5
xgbDiff5SmotedTrain <- xgb.train(data = xgbdiff5Smoted,
                                 nfold = 5,
                                 nrounds = 3,
                                 max_depth = 4,
                                 eta = .3,
                                 objective = "binary:logistic")
xgbDiff5SmotedImp <- xgb.importance(feature_names = colnames(diff5Smoted[,-1]), model = xgbDiff5SmotedTrain)
xgb.plot.importance(importance_matrix = xgbDiff5SmotedImp[1:20])
xgbDiff5SmotedImp %>%
  filter(Gain >= 0.025) -> xgbDiff5SmotedImpFeat
xgbDiff5SmotedSelectedFeat <- xgbDiff5SmotedImpFeat$Feature
diff5Smoted %>%
  select(`diff5Smote$Y`, xgbDiff5SmotedSelectedFeat) %>%
  data.matrix() -> diff5SmotedSelectedMatrix
xgbDiff5SmotedSelected <- xgb.DMatrix(diff5SmotedSelectedMatrix[,-1], label = diff5SmotedSelectedMatrix[,1])
xgbDiff5SmotedSelectedCv <- xgb.cv(data = xgbDiff5SmotedSelected,
                                   nfold = 5,
                                   nrounds = 3,
                                   max_depth = 4,
                                   eta = .3,
                                   objective = "binary:logistic",
                                   prediction = TRUE,
                                   metrics = list("error","rmse","auc"))
### diff6
xgbDiff6SmotedTrain <- xgb.train(data = xgbdiff6Smoted,
                                 nfold = 5,
                                 nrounds = 3,
                                 max_depth = 4,
                                 eta = .3,
                                 objective = "binary:logistic")
xgbDiff6SmotedImp <- xgb.importance(feature_names = colnames(diff6Smoted[,-1]), model = xgbDiff6SmotedTrain)
xgb.plot.importance(importance_matrix = xgbDiff6SmotedImp[1:20])
xgbDiff6SmotedImp %>%
  filter(Gain >= 0.025) -> xgbDiff6SmotedImpFeat
xgbDiff6SmotedSelectedFeat <- xgbDiff6SmotedImpFeat$Feature
diff6Smoted %>%
  select(`diff6Smote$Y`, xgbDiff6SmotedSelectedFeat) %>%
  data.matrix() -> diff6SmotedSelectedMatrix
xgbDiff6SmotedSelected <- xgb.DMatrix(diff6SmotedSelectedMatrix[,-1], label = diff6SmotedSelectedMatrix[,1])
xgbDiff6SmotedSelectedCv <- xgb.cv(data = xgbDiff6SmotedSelected,
                                   nfold = 5,
                                   nrounds = 3,
                                   max_depth = 4,
                                   eta = .3,
                                   objective = "binary:logistic",
                                   prediction = TRUE,
                                   metrics = list("error","rmse","auc"))
