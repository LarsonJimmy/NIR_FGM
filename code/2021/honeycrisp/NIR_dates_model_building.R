# NIR FGM: NIR Individual Dates Model Building - Honeycrisp, XGBoost
# author: Jimmy Larson
# created: 7.27.21
# last edited: 9.97.21

## load packages----
library(tidyverse)
library(lubridate)
library(caret)
#install.packages("repr")
library(repr)
library(xgboost)
#install.packages("prodlim")
library(prodlim)
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
## full model prediction metrics----
meas3Smoted %>%
  rownames_to_column() -> meas3rows
meas3rows$rowname <- as.numeric(meas3rows$rowname)
#### fold1
xgbMeas3Fold1 <- as.data.frame(c(xgbMeas3cv$folds[[1]]))
colnames(xgbMeas3Fold1) <- c("rowname")
Meas3Fold1 <- left_join(xgbMeas3Fold1, meas3rows)
#### 2
xgbMeas3Fold2 <- as.data.frame(c(xgbMeas3cv$folds[[2]]))
colnames(xgbMeas3Fold2) <- c("rowname")
Meas3Fold2 <- left_join(xgbMeas3Fold2, meas3rows)
#### fold3
xgbMeas3Fold3 <- as.data.frame(c(xgbMeas3cv$folds[[3]]))
colnames(xgbMeas3Fold3) <- c("rowname")
Meas3Fold3 <- left_join(xgbMeas3Fold3, meas3rows)
#### fold4
xgbMeas3Fold4 <- as.data.frame(c(xgbMeas3cv$folds[[4]]))
colnames(xgbMeas3Fold4) <- c("rowname")
Meas3Fold4 <- left_join(xgbMeas3Fold4, meas3rows)
#### fold5
xgbMeas3Fold5 <- as.data.frame(c(xgbMeas3cv$folds[[5]]))
colnames(xgbMeas3Fold5) <- c("rowname")
Meas3Fold5 <- left_join(xgbMeas3Fold5, meas3rows)


Meas3folds <- rbind(Meas3Fold1, Meas3Fold2, Meas3Fold3, Meas3Fold4, Meas3Fold5)
meas3pred <- xgbMeas3cv$pred
meas3prediction <- as.numeric(meas3pred > 0.5)
pred3Confusion <- table(meas3prediction, Meas3folds$`meas3Smote$Y`)
print(pred3Confusion)
## xgboost variable importance matrix----


xgbMeas3 <- xgb.train(params = params, data = xgbMeas3data,  nfold = 5, nrounds = 6)
Meas3imp <- xgb.importance(feature_names = colnames(meas3[,-1]), model = xgbMeas3)
xgb.plot.importance(importance_matrix = Meas3imp[1:20])
Meas3imp %>%
  filter(Gain >= 0.05) -> Meas3impFeat
Meas3selecedFeat <- Meas3impFeat$Feature
meas3 %>%
  select(persistCode, Meas3selecedFeat) -> meas3selected
meas3selected <- drop_na(meas3selected)
meas3selected <- data.matrix(meas3selected)
xgbMeas3selected <- xgb.DMatrix(meas3selected[,-1], label = meas3selected[,1])
xgbMeas3select <- xgb.cv(params = params, data = xgbMeas3selected,  nfold = 5, nrounds = 6, metrics = list("error", "auc"))

## manually select wavelengths for model building----
### meas3
NIRlong %>%
  select(rep, cluster, fruit, persist, wavelength, meas3_absorbance) %>%
  filter(475 < wavelength & wavelength < 700 | 950 < wavelength & wavelength < 1000) %>%
  mutate(persistCode = case_when(persist == "no" ~ 0,
                                 persist == "yes" ~ 1)) %>%
  spread(wavelength, meas3_absorbance) %>%
  select(persistCode, 6:97)-> meas3ManSelect
meas3ManSelect <- as.data.frame(meas3ManSelect)
colnames(meas3ManSelect)[2:93] <- paste0("nm3_", colnames(meas3ManSelect)[2:93])
## xboost for manually selected wavelengths----
meas3ManSelect <- drop_na(meas3ManSelect)
meas3ManSelectcases <- as.data.frame(table(meas3ManSelect$persistCode))
sum3ManSelectAbs <- meas3ManSelectcases[1,2]
sum3ManSelectPer <- meas3ManSelectcases[2,2]
meas3ManSelect <- data.matrix(meas3ManSelect)
xgbMeas3ManSelectdata <- xgb.DMatrix(meas3ManSelect[,-1], label = meas3ManSelect[,1])
params <- list(booster = "gbtree",
               objective = "binary:logistic",
               nrounds=6,
               metrics = list("error","rmse","auc"),
               scale_pos_weight = sum3ManSelectPer/sum3ManSelectAbs,
               max_depth = 20, 
               eta = .3)
xgbMeas3ManSelectcv <- xgb.cv(data = xgbMeas3ManSelectdata, params = params, nfold = 5, nrounds = 6)
