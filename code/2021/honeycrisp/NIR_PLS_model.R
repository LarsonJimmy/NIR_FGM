# NIR FGM: NIR Individual Dates Model Building - Honeycrisp, Partial Least Squares Regression
# author: Jimmy Larson
# created: 9.29.21
# last edited: 10.12.21

## load packages----
library(tidyverse)
library(caret)
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
## individual dates----
#### meas1
meas1 %>%
  drop_na() %>%
  select(persist, 6:311)-> meas1
meas1Pls<- train(persist ~ ., data = meas1, trControl = train_control, method = "pls", metric = "ROC")
print(meas1Pls)
meas1Confusion <- confusionMatrix(meas1Pls[["pred"]][["pred"]], meas1Pls[["pred"]][["obs"]])
#### meas2
meas2 %>%
  drop_na() %>%
  select(persist, 6:311)-> meas2
meas2Pls<- train(persist ~ ., data = meas2, trControl = train_control, method = "pls", metric = "ROC")
print(meas2Pls)
meas2Confusion <- confusionMatrix(meas2Pls[["pred"]][["pred"]], meas2Pls[["pred"]][["obs"]])
#### meas3
meas3 %>%
  drop_na() %>%
  select(persist, 6:311)-> meas3
meas3Pls<- train(persist ~ ., data = meas3, trControl = train_control, method = "pls", metric = "ROC")
print(meas3Pls)
meas3Confusion <- confusionMatrix(meas3Pls[["pred"]][["pred"]], meas3Pls[["pred"]][["obs"]])
#### meas4
meas4 %>%
  drop_na() %>%
  select(persist, 6:311)-> meas4
meas4Pls<- train(persist ~ ., data = meas4, trControl = train_control, method = "pls", metric = "ROC")
print(meas4Pls)
meas4Confusion <- confusionMatrix(meas4Pls[["pred"]][["pred"]], meas4Pls[["pred"]][["obs"]])
#### meas5
meas5 %>%
  drop_na() %>%
  select(persist, 6:311)-> meas5
meas5Pls<- train(persist ~ ., data = meas5, trControl = train_control, method = "pls", metric = "ROC")
print(meas5Pls)
meas5Confusion <- confusionMatrix(meas5Pls[["pred"]][["pred"]], meas5Pls[["pred"]][["obs"]])
#### meas6
meas6 %>%
  drop_na() %>%
  select(persist, 6:311)-> meas6
meas6Pls<- train(persist ~ ., data = meas6, trControl = train_control, method = "pls", metric = "ROC")
print(meas6Pls)
meas6Confusion <- confusionMatrix(meas6Pls[["pred"]][["pred"]], meas6Pls[["pred"]][["obs"]])
#### meas7
meas7 %>%
  drop_na() %>%
  select(persist, 6:311)-> meas7
meas7Pls<- train(persist ~ ., data = meas7, trControl = train_control, method = "pls", metric = "ROC")
print(meas7Pls)
meas7Confusion <- confusionMatrix(meas7Pls[["pred"]][["pred"]], meas7Pls[["pred"]][["obs"]])

## individual dates with SMOTE----
### meas1
meas1Smoted %>%
  mutate(persist = case_when(meas1Smoted$`meas1Smote$Y` == 0 ~ "no",
                             meas1Smoted$`meas1Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> meas1Smoted
meas1SmotedPls <- train(persist ~ ., data = meas1Smoted, trControl = train_control, method = "pls", metric = "ROC")
print(meas1SmotedPls)
meas1SmotedConfusion <- confusionMatrix(meas1SmotedPls[["pred"]][["pred"]], meas1SmotedPls[["pred"]][["obs"]])
### meas2
meas2Smoted %>%
  mutate(persist = case_when(meas2Smoted$`meas2Smote$Y` == 0 ~ "no",
                             meas2Smoted$`meas2Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> meas2Smoted
meas2SmotedPls <- train(persist ~ ., data = meas2Smoted, trControl = train_control, method = "pls", metric = "ROC")
print(meas2SmotedPls)
meas2SmotedConfusion <- confusionMatrix(meas2SmotedPls[["pred"]][["pred"]], meas2SmotedPls[["pred"]][["obs"]])
### meas3
meas3Smoted %>%
  mutate(persist = case_when(meas3Smoted$`meas3Smote$Y` == 0 ~ "no",
                             meas3Smoted$`meas3Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> meas3Smoted
meas3SmotedPls <- train(persist ~ ., data = meas3Smoted, trControl = train_control, method = "pls", metric = "ROC")
print(meas3SmotedPls)
meas3SmotedConfusion <- confusionMatrix(meas3SmotedPls[["pred"]][["pred"]], meas3SmotedPls[["pred"]][["obs"]])
### meas4
meas4Smoted %>%
  mutate(persist = case_when(meas4Smoted$`meas4Smote$Y` == 0 ~ "no",
                             meas4Smoted$`meas4Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> meas4Smoted
meas4SmotedPls <- train(persist ~ ., data = meas4Smoted, trControl = train_control, method = "pls", metric = "ROC")
print(meas4SmotedPls)
meas4SmotedConfusion <- confusionMatrix(meas4SmotedPls[["pred"]][["pred"]], meas4SmotedPls[["pred"]][["obs"]])
### meas5
meas5Smoted %>%
  mutate(persist = case_when(meas5Smoted$`meas5Smote$Y` == 0 ~ "no",
                             meas5Smoted$`meas5Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> meas5Smoted
meas5SmotedPls <- train(persist ~ ., data = meas5Smoted, trControl = train_control, method = "pls", metric = "ROC")
print(meas5SmotedPls)
meas5SmotedConfusion <- confusionMatrix(meas5SmotedPls[["pred"]][["pred"]], meas5SmotedPls[["pred"]][["obs"]])
### meas6
meas6Smoted %>%
  mutate(persist = case_when(meas6Smoted$`meas6Smote$Y` == 0 ~ "no",
                             meas6Smoted$`meas6Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> meas6Smoted
meas6SmotedPls <- train(persist ~ ., data = meas6Smoted, trControl = train_control, method = "pls", metric = "ROC")
print(meas6SmotedPls)
meas6SmotedConfusion <- confusionMatrix(meas6SmotedPls[["pred"]][["pred"]], meas6SmotedPls[["pred"]][["obs"]])
### meas7
meas7Smoted %>%
  mutate(persist = case_when(meas7Smoted$`meas7Smote$Y` == 0 ~ "no",
                             meas7Smoted$`meas7Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> meas7Smoted
meas7SmotedPls <- train(persist ~ ., data = meas7Smoted, trControl = train_control, method = "pls", metric = "ROC")
print(meas7SmotedPls)
meas7SmotedConfusion <- confusionMatrix(meas7SmotedPls[["pred"]][["pred"]], meas7SmotedPls[["pred"]][["obs"]])
## differences in dates----
### diff1
diff1 %>%
  drop_na() %>%
  select(persist, 6:311)-> diff1
diff1Pls <- train(persist ~ ., data = diff1, trControl = train_control, method = "pls", metric = "ROC")
print(diff1Pls)
diff1Confusion <- confusionMatrix(diff1Pls[["pred"]][["pred"]], diff1Pls[["pred"]][["obs"]])
### diff2
diff2 %>%
  drop_na() %>%
  select(persist, 6:311)-> diff2
diff2Pls <- train(persist ~ ., data = diff2, trControl = train_control, method = "pls", metric = "ROC")
print(diff2Pls)
diff2Confusion <- confusionMatrix(diff2Pls[["pred"]][["pred"]], diff2Pls[["pred"]][["obs"]])
### diff3
diff3 %>%
  drop_na() %>%
  select(persist, 6:311)-> diff3
diff3Pls <- train(persist ~ ., data = diff3, trControl = train_control, method = "pls", metric = "ROC")
print(diff3Pls)
diff3Confusion <- confusionMatrix(diff3Pls[["pred"]][["pred"]], diff3Pls[["pred"]][["obs"]])
### diff4
diff4 %>%
  drop_na() %>%
  select(persist, 6:311)-> diff4
diff4Pls <- train(persist ~ ., data = diff4, trControl = train_control, method = "pls", metric = "ROC")
print(diff4Pls)
diff4Confusion <- confusionMatrix(diff4Pls[["pred"]][["pred"]], diff4Pls[["pred"]][["obs"]])
### diff5
diff5 %>%
  drop_na() %>%
  select(persist, 6:311)-> diff5
diff5Pls <- train(persist ~ ., data = diff5, trControl = train_control, method = "pls", metric = "ROC")
print(diff5Pls)
diff5Confusion <- confusionMatrix(diff5Pls[["pred"]][["pred"]], diff5Pls[["pred"]][["obs"]])
### diff6
diff6 %>%
  drop_na() %>%
  select(persist, 6:311)-> diff6
diff6Pls <- train(persist ~ ., data = diff6, trControl = train_control, method = "pls", metric = "ROC")
print(diff6Pls)
diff6Confusion <- confusionMatrix(diff6Pls[["pred"]][["pred"]], diff6Pls[["pred"]][["obs"]])
## differences in dates with SMOTE----
### diff1
diff1Smoted %>%
  mutate(persist = case_when(diff1Smoted$`diff1Smote$Y` == 0 ~ "no",
                             diff1Smoted$`diff1Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> diff1Smoted
diff1SmotedPls <- train(persist ~ ., data = diff1Smoted, trControl = train_control, method = "pls", metric = "ROC")
print(diff1SmotedPls)
diff1SmotedConfusion <- confusionMatrix(diff1SmotedPls[["pred"]][["pred"]], diff1SmotedPls[["pred"]][["obs"]])
### diff2
diff2Smoted %>%
  mutate(persist = case_when(diff2Smoted$`diff2Smote$Y` == 0 ~ "no",
                             diff2Smoted$`diff2Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> diff2Smoted
diff2SmotedPls <- train(persist ~ ., data = diff2Smoted, trControl = train_control, method = "pls", metric = "ROC")
print(diff2SmotedPls)
diff2SmotedConfusion <- confusionMatrix(diff2SmotedPls[["pred"]][["pred"]], diff2SmotedPls[["pred"]][["obs"]])
### diff3
diff3Smoted %>%
  mutate(persist = case_when(diff3Smoted$`diff3Smote$Y` == 0 ~ "no",
                             diff3Smoted$`diff3Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> diff3Smoted
diff3SmotedPls <- train(persist ~ ., data = diff3Smoted, trControl = train_control, method = "pls", metric = "ROC")
print(diff3SmotedPls)
diff3SmotedConfusion <- confusionMatrix(diff3SmotedPls[["pred"]][["pred"]], diff3SmotedPls[["pred"]][["obs"]])
### diff4
diff4Smoted %>%
  mutate(persist = case_when(diff4Smoted$`diff4Smote$Y` == 0 ~ "no",
                             diff4Smoted$`diff4Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> diff4Smoted
diff4SmotedPls <- train(persist ~ ., data = diff4Smoted, trControl = train_control, method = "pls", metric = "ROC")
print(diff4SmotedPls)
diff4SmotedConfusion <- confusionMatrix(diff4SmotedPls[["pred"]][["pred"]], diff4SmotedPls[["pred"]][["obs"]])
### diff5
diff5Smoted %>%
  mutate(persist = case_when(diff5Smoted$`diff5Smote$Y` == 0 ~ "no",
                             diff5Smoted$`diff5Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> diff5Smoted
diff5SmotedPls <- train(persist ~ ., data = diff5Smoted, trControl = train_control, method = "pls", metric = "ROC")
print(diff5SmotedPls)
diff5SmotedConfusion <- confusionMatrix(diff5SmotedPls[["pred"]][["pred"]], diff5SmotedPls[["pred"]][["obs"]])
### diff6
diff6Smoted %>%
  mutate(persist = case_when(diff6Smoted$`diff6Smote$Y` == 0 ~ "no",
                             diff6Smoted$`diff6Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> diff6Smoted
diff6SmotedPls <- train(persist ~ ., data = diff6Smoted, trControl = train_control, method = "pls", metric = "ROC")
print(diff6SmotedPls)
diff6SmotedConfusion <- confusionMatrix(diff6SmotedPls[["pred"]][["pred"]], diff6SmotedPls[["pred"]][["obs"]])