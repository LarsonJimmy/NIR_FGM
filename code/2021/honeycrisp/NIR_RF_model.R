# NIR FGM: NIR Individual Dates Model Building - Honeycrisp, Random Forest
# author: Jimmy Larson
# created: 9.29.21
# last edited: 10.11.21

## load packages----
library(tidyverse)
library(caret)
library(randomForest)

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
## set k-fold cross validation function----
train_control <- trainControl(method = "cv", number = 5, classProbs = TRUE, savePredictions = TRUE, summaryFunction = twoClassSummary)
### individual dates----
#### meas1
meas1 %>%
  drop_na() %>%
  select(persist, 6:311)-> meas1
meas1Rf <- train(persist ~ ., data = meas1, trControl = train_control, method = "rf", metric = "ROC")
print(meas1Rf)
meas1Confusion <- confusionMatrix(meas1Rf[["pred"]][["pred"]], meas1Rf[["pred"]][["obs"]])
#### meas2
meas2 %>%
  drop_na() %>%
  select(persist, 6:311)-> meas2
meas2Rf <- train(persist ~ ., data = meas2, trControl = train_control, method = "rf", metric = "ROC")
print(meas2Rf)
meas2Confusion <- confusionMatrix(meas2Rf[["pred"]][["pred"]], meas2Rf[["pred"]][["obs"]])
#### meas3
meas3 %>%
  drop_na() %>%
  select(persist, 6:311)-> meas3
meas3Rf <- train(persist ~ ., data = meas3, trControl = train_control, method = "rf", metric = "ROC")
print(meas3Rf)
meas3Confusion <- confusionMatrix(meas3Rf[["pred"]][["pred"]], meas3Rf[["pred"]][["obs"]])
#### meas4
meas4 %>%
  drop_na() %>%
  select(persist, 6:311)-> meas4
meas4Rf <- train(persist ~ ., data = meas4, trControl = train_control, method = "rf", metric = "ROC")
print(meas4Rf)
meas4Confusion <- confusionMatrix(meas4Rf[["pred"]][["pred"]], meas4Rf[["pred"]][["obs"]])
#### meas5
meas5 %>%
  drop_na() %>%
  select(persist, 6:311)-> meas5
meas5Rf <- train(persist ~ ., data = meas5, trControl = train_control, method = "rf", metric = "ROC")
print(meas5Rf)
meas5Confusion <- confusionMatrix(meas5Rf[["pred"]][["pred"]], meas5Rf[["pred"]][["obs"]])
#### meas6
meas6 %>%
  drop_na() %>%
  select(persist, 6:311)-> meas6
meas6Rf <- train(persist ~ ., data = meas6, trControl = train_control, method = "rf", metric = "ROC")
print(meas6Rf)
meas6Confusion <- confusionMatrix(meas6Rf[["pred"]][["pred"]], meas6Rf[["pred"]][["obs"]])
#### meas7
meas7 %>%
  drop_na() %>%
  select(persist, 6:311)-> meas7
meas7Rf <- train(persist ~ ., data = meas7, trControl = train_control, method = "rf", metric = "ROC")
print(meas7Rf)
meas7Confusion <- confusionMatrix(meas7Rf[["pred"]][["pred"]], meas7Rf[["pred"]][["obs"]])
### individual dates with SMOTE----
#### meas1 SMOTE
meas1Smoted %>%
  mutate(persist = case_when(meas1Smoted$`meas1Smote$Y` == 0 ~ "no",
                             meas1Smoted$`meas1Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> meas1Smoted
meas1SmotedRf <- train(persist ~ ., data = meas1Smoted, trControl = train_control, method = "rf", metric = "ROC")
print(meas1SmotedRf)
meas1SmotedConfusion <- confusionMatrix(meas1SmotedRf[["pred"]][["pred"]], meas1SmotedRf[["pred"]][["obs"]])
#### meas2 SMOTE
meas2Smoted %>%
  mutate(persist = case_when(meas2Smoted$`meas2Smote$Y` == 0 ~ "no",
                             meas2Smoted$`meas2Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> meas2Smoted
meas2SmotedRf <- train(persist ~ ., data = meas2Smoted, trControl = train_control, method = "rf", metric = "ROC")
print(meas2SmotedRf)
meas2SmotedConfusion <- confusionMatrix(meas2SmotedRf[["pred"]][["pred"]], meas2SmotedRf[["pred"]][["obs"]])
#### meas3 SMOTE
meas3Smoted %>%
  mutate(persist = case_when(meas3Smoted$`meas3Smote$Y` == 0 ~ "no",
                             meas3Smoted$`meas3Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> meas3Smoted
meas3SmotedRf <- train(persist ~ ., data = meas3Smoted, trControl = train_control, method = "rf", metric = "ROC")
print(meas3SmotedRf)
meas3SmotedConfusion <- confusionMatrix(meas3SmotedRf[["pred"]][["pred"]], meas3SmotedRf[["pred"]][["obs"]])
#### meas4 SMOTE
meas4Smoted %>%
  mutate(persist = case_when(meas4Smoted$`meas4Smote$Y` == 0 ~ "no",
                             meas4Smoted$`meas4Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> meas4Smoted
meas4SmotedRf <- train(persist ~ ., data = meas4Smoted, trControl = train_control, method = "rf", metric = "ROC")
print(meas4SmotedRf)
meas4SmotedConfusion <- confusionMatrix(meas4SmotedRf[["pred"]][["pred"]], meas4SmotedRf[["pred"]][["obs"]])
#### meas5 SMOTE
meas5Smoted %>%
  mutate(persist = case_when(meas5Smoted$`meas5Smote$Y` == 0 ~ "no",
                             meas5Smoted$`meas5Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> meas5Smoted
meas5SmotedRf <- train(persist ~ ., data = meas5Smoted, trControl = train_control, method = "rf", metric = "ROC")
print(meas5SmotedRf)
meas5SmotedConfusion <- confusionMatrix(meas5SmotedRf[["pred"]][["pred"]], meas5SmotedRf[["pred"]][["obs"]])
#### meas6 SMOTE
meas6Smoted %>%
  mutate(persist = case_when(meas6Smoted$`meas6Smote$Y` == 0 ~ "no",
                             meas6Smoted$`meas6Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> meas6Smoted
meas6SmotedRf <- train(persist ~ ., data = meas6Smoted, trControl = train_control, method = "rf", metric = "ROC")
print(meas6SmotedRf)
meas6SmotedConfusion <- confusionMatrix(meas6SmotedRf[["pred"]][["pred"]], meas6SmotedRf[["pred"]][["obs"]])
#### meas7 SMOTE
meas7Smoted %>%
  mutate(persist = case_when(meas7Smoted$`meas7Smote$Y` == 0 ~ "no",
                             meas7Smoted$`meas7Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> meas7Smoted
meas7SmotedRf <- train(persist ~ ., data = meas7Smoted, trControl = train_control, method = "rf", metric = "ROC")
print(meas7SmotedRf)
meas7SmotedConfusion <- confusionMatrix(meas7SmotedRf[["pred"]][["pred"]], meas7SmotedRf[["pred"]][["obs"]])
### differences in dates----
#### diff1
diff1 %>%
  drop_na() %>%
  select(persist, 6:311)-> diff1
diff1Rf <- train(persist ~ ., data = diff1, trControl = train_control, method = "rf", metric = "ROC")
print(diff1Rf)
diff1Confusion <- confusionMatrix(diff1Rf[["pred"]][["pred"]], diff1Rf[["pred"]][["obs"]])
#### diff2
diff2 %>%
  drop_na() %>%
  select(persist, 6:311)-> diff2
diff2Rf <- train(persist ~ ., data = diff2, trControl = train_control, method = "rf", metric = "ROC")
print(diff2Rf)
diff2Confusion <- confusionMatrix(diff2Rf[["pred"]][["pred"]], diff2Rf[["pred"]][["obs"]])
#### diff3
diff3 %>%
  drop_na() %>%
  select(persist, 6:311)-> diff3
diff3Rf <- train(persist ~ ., data = diff3, trControl = train_control, method = "rf", metric = "ROC")
print(diff3Rf)
diff3Confusion <- confusionMatrix(diff3Rf[["pred"]][["pred"]], diff3Rf[["pred"]][["obs"]])
#### diff4
diff4 %>%
  drop_na() %>%
  select(persist, 6:311)-> diff4
diff4Rf <- train(persist ~ ., data = diff4, trControl = train_control, method = "rf", metric = "ROC")
print(diff4Rf)
diff4Confusion <- confusionMatrix(diff4Rf[["pred"]][["pred"]], diff4Rf[["pred"]][["obs"]])
#### diff5
diff5 %>%
  drop_na() %>%
  select(persist, 6:311)-> diff5
diff5Rf <- train(persist ~ ., data = diff5, trControl = train_control, method = "rf", metric = "ROC")
print(diff5Rf)
diff5Confusion <- confusionMatrix(diff5Rf[["pred"]][["pred"]], diff5Rf[["pred"]][["obs"]])
#### diff6
diff6 %>%
  drop_na() %>%
  select(persist, 6:311)-> diff6
diff6Rf <- train(persist ~ ., data = diff6, trControl = train_control, method = "rf", metric = "ROC")
print(diff6Rf)
diff6Confusion <- confusionMatrix(diff6Rf[["pred"]][["pred"]], diff6Rf[["pred"]][["obs"]])
### differences in dates with SMOTE----
#### diff1 SMOTE
diff1Smoted %>%
  mutate(persist = case_when(diff1Smoted$`diff1Smote$Y` == 0 ~ "no",
                             diff1Smoted$`diff1Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> diff1Smoted
diff1SmotedRf <- train(persist ~ ., data = diff1Smoted, trControl = train_control, method = "rf", metric = "ROC")
print(diff1SmotedRf)
diff1SmotedConfusion <- confusionMatrix(diff1SmotedRf[["pred"]][["pred"]], diff1SmotedRf[["pred"]][["obs"]])
#### diff2 SMOTE
diff2Smoted %>%
  mutate(persist = case_when(diff2Smoted$`diff2Smote$Y` == 0 ~ "no",
                             diff2Smoted$`diff2Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> diff2Smoted
diff2SmotedRf <- train(persist ~ ., data = diff2Smoted, trControl = train_control, method = "rf", metric = "ROC")
print(diff2SmotedRf)
diff2SmotedConfusion <- confusionMatrix(diff2SmotedRf[["pred"]][["pred"]], diff2SmotedRf[["pred"]][["obs"]])
#### diff3 SMOTE
diff3Smoted %>%
  mutate(persist = case_when(diff3Smoted$`diff3Smote$Y` == 0 ~ "no",
                             diff3Smoted$`diff3Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> diff3Smoted
diff3SmotedRf <- train(persist ~ ., data = diff3Smoted, trControl = train_control, method = "rf", metric = "ROC")
print(diff3SmotedRf)
diff3SmotedConfusion <- confusionMatrix(diff3SmotedRf[["pred"]][["pred"]], diff3SmotedRf[["pred"]][["obs"]])
#### diff4 SMOTE
diff4Smoted %>%
  mutate(persist = case_when(diff4Smoted$`diff4Smote$Y` == 0 ~ "no",
                             diff4Smoted$`diff4Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> diff4Smoted
diff4SmotedRf <- train(persist ~ ., data = diff4Smoted, trControl = train_control, method = "rf", metric = "ROC")
print(diff4SmotedRf)
diff4SmotedConfusion <- confusionMatrix(diff4SmotedRf[["pred"]][["pred"]], diff4SmotedRf[["pred"]][["obs"]])
#### diff5 SMOTE
diff5Smoted %>%
  mutate(persist = case_when(diff5Smoted$`diff5Smote$Y` == 0 ~ "no",
                             diff5Smoted$`diff5Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> diff5Smoted
diff5SmotedRf <- train(persist ~ ., data = diff5Smoted, trControl = train_control, method = "rf", metric = "ROC")
print(diff5SmotedRf)
diff5SmotedConfusion <- confusionMatrix(diff5SmotedRf[["pred"]][["pred"]], diff5SmotedRf[["pred"]][["obs"]])
#### diff6 SMOTE
diff6Smoted %>%
  mutate(persist = case_when(diff6Smoted$`diff6Smote$Y` == 0 ~ "no",
                             diff6Smoted$`diff6Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> diff6Smoted
diff6SmotedRf <- train(persist ~ ., data = diff6Smoted, trControl = train_control, method = "rf", metric = "ROC")
print(diff6SmotedRf)
diff6SmotedConfusion <- confusionMatrix(diff6SmotedRf[["pred"]][["pred"]], diff6SmotedRf[["pred"]][["obs"]])
