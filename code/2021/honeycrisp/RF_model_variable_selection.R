# NIR FGM: Variable Importance Selection - Honeycrisp, Random Forest
# author: Jimmy Larson
# created: 10.12.21
# last edited: 10.12.21

## load packages----
library(tidyverse)
library(caret)
library(randomForest)
library(cowplot)
## set seed----
set.seed(123)
## read in data-----
meas1Smoted <- read_csv("data/2021/honeycrisp/model_building/meas1Smoted.csv")
meas2Smoted <- read_csv("data/2021/honeycrisp/model_building/meas2Smoted.csv")
meas3Smoted <- read_csv("data/2021/honeycrisp/model_building/meas3Smoted.csv")
meas4Smoted <- read_csv("data/2021/honeycrisp/model_building/meas4Smoted.csv")
meas5Smoted <- read_csv("data/2021/honeycrisp/model_building/meas5Smoted.csv")
meas6Smoted <- read_csv("data/2021/honeycrisp/model_building/meas6Smoted.csv")
meas7Smoted <- read_csv("data/2021/honeycrisp/model_building/meas7Smoted.csv")
## set k-fold cross validation function----
train_control <- trainControl(method = "cv", number = 5, classProbs = TRUE, savePredictions = TRUE, summaryFunction = twoClassSummary)
## individual dates with SMOTE----
### meas1 SMOTE
meas1Smoted %>%
  mutate(persist = case_when(meas1Smoted$`meas1Smote$Y` == 0 ~ "no",
                             meas1Smoted$`meas1Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> meas1Smoted
meas1SmotedRf <- train(persist ~ ., data = meas1Smoted, trControl = train_control, method = "rf", metric = "ROC")
meas1SmotedConfusion <- confusionMatrix(meas1SmotedRf[["pred"]][["pred"]], meas1SmotedRf[["pred"]][["obs"]])
### meas2 SMOTE
meas2Smoted %>%
  mutate(persist = case_when(meas2Smoted$`meas2Smote$Y` == 0 ~ "no",
                             meas2Smoted$`meas2Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> meas2Smoted
meas2SmotedRf <- train(persist ~ ., data = meas2Smoted, trControl = train_control, method = "rf", metric = "ROC")
meas2SmotedConfusion <- confusionMatrix(meas2SmotedRf[["pred"]][["pred"]], meas2SmotedRf[["pred"]][["obs"]])
### meas3 SMOTE
meas3Smoted %>%
  mutate(persist = case_when(meas3Smoted$`meas3Smote$Y` == 0 ~ "no",
                             meas3Smoted$`meas3Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> meas3Smoted
meas3SmotedRf <- train(persist ~ ., data = meas3Smoted, trControl = train_control, method = "rf", metric = "ROC")
meas3SmotedConfusion <- confusionMatrix(meas3SmotedRf[["pred"]][["pred"]], meas3SmotedRf[["pred"]][["obs"]])
### meas4 SMOTE
meas4Smoted %>%
  mutate(persist = case_when(meas4Smoted$`meas4Smote$Y` == 0 ~ "no",
                             meas4Smoted$`meas4Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> meas4Smoted
meas4SmotedRf <- train(persist ~ ., data = meas4Smoted, trControl = train_control, method = "rf", metric = "ROC")
meas4SmotedConfusion <- confusionMatrix(meas4SmotedRf[["pred"]][["pred"]], meas4SmotedRf[["pred"]][["obs"]])
### meas5 SMOTE
meas5Smoted %>%
  mutate(persist = case_when(meas5Smoted$`meas5Smote$Y` == 0 ~ "no",
                             meas5Smoted$`meas5Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> meas5Smoted
meas5SmotedRf <- train(persist ~ ., data = meas5Smoted, trControl = train_control, method = "rf", metric = "ROC")
meas5SmotedConfusion <- confusionMatrix(meas5SmotedRf[["pred"]][["pred"]], meas5SmotedRf[["pred"]][["obs"]])
### meas6 SMOTE
meas6Smoted %>%
  mutate(persist = case_when(meas6Smoted$`meas6Smote$Y` == 0 ~ "no",
                             meas6Smoted$`meas6Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> meas6Smoted
meas6SmotedRf <- train(persist ~ ., data = meas6Smoted, trControl = train_control, method = "rf", metric = "ROC")
meas6SmotedConfusion <- confusionMatrix(meas6SmotedRf[["pred"]][["pred"]], meas6SmotedRf[["pred"]][["obs"]])
### meas7 SMOTE
meas7Smoted %>%
  mutate(persist = case_when(meas7Smoted$`meas7Smote$Y` == 0 ~ "no",
                             meas7Smoted$`meas7Smote$Y` == 1 ~ "yes")) %>%
  select(persist, 2:307) -> meas7Smoted
meas7SmotedRf <- train(persist ~ ., data = meas7Smoted, trControl = train_control, method = "rf", metric = "ROC")
meas7SmotedConfusion <- confusionMatrix(meas7SmotedRf[["pred"]][["pred"]], meas7SmotedRf[["pred"]][["obs"]])
## variable importance----
### meas1
meas1Imp <- varImp(meas1SmotedRf, scale = FALSE)
meas1Imp$importance %>%
  rownames_to_column(var = "predictor") %>%
  arrange(desc(Overall)) %>%
  rownames_to_column(var = "rank") %>%
  filter(rank <= 20)  -> meas1selected
meas1Smoted %>%
  select(persist, meas1selected$predictor) -> meas1SelectedWavelengths
meas1SelectedRf <- train(persist ~ ., data = meas1SelectedWavelengths, trControl = train_control, method = "rf", metric = "ROC")
meas1SelectedRfConfusion <- confusionMatrix(meas1SelectedRf[["pred"]][["pred"]], meas1SelectedRf[["pred"]][["obs"]])
### meas2
meas2Imp <- varImp(meas2SmotedRf, scale = FALSE)
meas2Imp$importance %>%
  rownames_to_column(var = "predictor") %>%
  arrange(desc(Overall)) %>%
  rownames_to_column(var = "rank") %>%
  filter(rank <= 20)  -> meas2selected
meas2Smoted %>%
  select(persist, meas2selected$predictor) -> meas2SelectedWavelengths
meas2SelectedRf <- train(persist ~ ., data = meas2SelectedWavelengths, trControl = train_control, method = "rf", metric = "ROC")
meas2SelectedRfConfusion <- confusionMatrix(meas2SelectedRf[["pred"]][["pred"]], meas2SelectedRf[["pred"]][["obs"]])
### meas3
meas3Imp <- varImp(meas3SmotedRf, scale = FALSE)
meas3Imp$importance %>%
  rownames_to_column(var = "predictor") %>%
  arrange(desc(Overall)) %>%
  rownames_to_column(var = "rank") %>%
  filter(rank <= 20) -> meas3selected
meas3Smoted %>%
  select(persist, meas3selected$predictor) -> meas3SelectedWavelengths
meas3SelectedRf <- train(persist ~ ., data = meas3SelectedWavelengths, trControl = train_control, method = "rf", metric = "ROC")
meas3SelectedRfConfusion <- confusionMatrix(meas3SelectedRf[["pred"]][["pred"]], meas3SelectedRf[["pred"]][["obs"]])
### meas4
meas4Imp <- varImp(meas4SmotedRf, scale = FALSE)
meas4Imp$importance %>%
  rownames_to_column(var = "predictor") %>%
  arrange(desc(Overall)) %>%
  rownames_to_column(var = "rank") %>%
  filter(rank <= 20)  -> meas4selected
meas4Smoted %>%
  select(persist, meas4selected$predictor) -> meas4SelectedWavelengths
meas4SelectedRf <- train(persist ~ ., data = meas4SelectedWavelengths, trControl = train_control, method = "rf", metric = "ROC")
meas4SelectedRfConfusion <- confusionMatrix(meas4SelectedRf[["pred"]][["pred"]], meas4SelectedRf[["pred"]][["obs"]])
### meas5
meas5Imp <- varImp(meas5SmotedRf, scale = FALSE)
meas5Imp$importance %>%
  rownames_to_column(var = "predictor") %>%
  arrange(desc(Overall)) %>%
  rownames_to_column(var = "rank") %>%
  filter(rank <= 20)  -> meas5selected
meas5Smoted %>%
  select(persist, meas5selected$predictor) -> meas5SelectedWavelengths
meas5SelectedRf <- train(persist ~ ., data = meas5SelectedWavelengths, trControl = train_control, method = "rf", metric = "ROC")
meas5SelectedRfConfusion <- confusionMatrix(meas5SelectedRf[["pred"]][["pred"]], meas5SelectedRf[["pred"]][["obs"]])
### meas6
meas6Imp <- varImp(meas6SmotedRf, scale = FALSE)
meas6Imp$importance %>%
  rownames_to_column(var = "predictor") %>%
  arrange(desc(Overall)) %>%
  rownames_to_column(var = "rank") %>%
  filter(rank <= 20)  -> meas6selected
meas6Smoted %>%
  select(persist, meas6selected$predictor) -> meas6SelectedWavelengths
meas6SelectedRf <- train(persist ~ ., data = meas6SelectedWavelengths, trControl = train_control, method = "rf", metric = "ROC")
meas6SelectedRfConfusion <- confusionMatrix(meas6SelectedRf[["pred"]][["pred"]], meas6SelectedRf[["pred"]][["obs"]])
### meas7
meas7Imp <- varImp(meas7SmotedRf, scale = FALSE)
meas7Imp$importance %>%
  rownames_to_column(var = "predictor") %>%
  arrange(desc(Overall)) %>%
  rownames_to_column(var = "rank") %>%
  filter(rank <= 20)  -> meas7selected
meas7Smoted %>%
  select(persist, meas7selected$predictor) -> meas7SelectedWavelengths
meas7SelectedRf <- train(persist ~ ., data = meas7SelectedWavelengths, trControl = train_control, method = "rf", metric = "ROC")
meas7SelectedRfConfusion <- confusionMatrix(meas7SelectedRf[["pred"]][["pred"]], meas7SelectedRf[["pred"]][["obs"]])
## graph spectra data----
### meas1
meas1Smoted %>%
  gather(2:307, key = "wavelength", value = "absorbance") %>%
  mutate(reflectance = 1 - absorbance) %>%
  group_by(wavelength, persist) %>%
  summarise(reflectance = mean(reflectance)) %>%
  separate(wavelength, sep = "_", into = c("nm", "wavelength")) -> meas1SmotedSpectra
meas1SmotedSpectra$wavelength <- as.numeric(meas1SmotedSpectra$wavelength)
ggplot(meas1SmotedSpectra, aes(x = wavelength, y = reflectance, color = persist)) +
  geom_point(alpha = 0.5, size = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Wavelength (nm)",
       y = "Reflectance",
       color = "Fruit Persistence")+
  theme_bw() -> meas1SpectraFig
### meas2
meas2Smoted %>%
  gather(2:307, key = "wavelength", value = "absorbance") %>%
  mutate(reflectance = 1 - absorbance) %>%
  group_by(wavelength, persist) %>%
  summarise(reflectance = mean(reflectance)) %>%
  separate(wavelength, sep = "_", into = c("nm", "wavelength")) -> meas2SmotedSpectra
meas2SmotedSpectra$wavelength <- as.numeric(meas2SmotedSpectra$wavelength)
ggplot(meas2SmotedSpectra, aes(x = wavelength, y = reflectance, color = persist)) +
  geom_point(alpha = 0.5, size = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Wavelength (nm)",
       y = "Reflectance",
       color = "Fruit Persistence")+
  theme_bw() -> meas2SpectraFig
### meas3
meas3Smoted %>%
  gather(2:307, key = "wavelength", value = "absorbance") %>%
  mutate(reflectance = 1 - absorbance) %>%
  group_by(wavelength, persist) %>%
  summarise(reflectance = mean(reflectance)) %>%
  separate(wavelength, sep = "_", into = c("nm", "wavelength")) -> meas3SmotedSpectra
meas3SmotedSpectra$wavelength <- as.numeric(meas3SmotedSpectra$wavelength)
ggplot(meas3SmotedSpectra, aes(x = wavelength, y = reflectance, color = persist)) +
  geom_point(alpha = 0.5, size = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Wavelength (nm)",
       y = "Reflectance",
       color = "Fruit Persistence")+
  theme_bw() -> meas3SpectraFig
### meas4
meas4Smoted %>%
  gather(2:307, key = "wavelength", value = "absorbance") %>%
  mutate(reflectance = 1 - absorbance) %>%
  group_by(wavelength, persist) %>%
  summarise(reflectance = mean(reflectance)) %>%
  separate(wavelength, sep = "_", into = c("nm", "wavelength")) -> meas4SmotedSpectra
meas4SmotedSpectra$wavelength <- as.numeric(meas4SmotedSpectra$wavelength)
ggplot(meas4SmotedSpectra, aes(x = wavelength, y = reflectance, color = persist)) +
  geom_point(alpha = 0.5, size = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Wavelength (nm)",
       y = "Reflectance",
       color = "Fruit Persistence")+
  theme_bw() -> meas4SpectraFig
### meas5
meas5Smoted %>%
  gather(2:307, key = "wavelength", value = "absorbance") %>%
  mutate(reflectance = 1 - absorbance) %>%
  group_by(wavelength, persist) %>%
  summarise(reflectance = mean(reflectance)) %>%
  separate(wavelength, sep = "_", into = c("nm", "wavelength")) -> meas5SmotedSpectra
meas5SmotedSpectra$wavelength <- as.numeric(meas5SmotedSpectra$wavelength)
ggplot(meas5SmotedSpectra, aes(x = wavelength, y = reflectance, color = persist)) +
  geom_point(alpha = 0.5, size = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Wavelength (nm)",
       y = "Reflectance",
       color = "Fruit Persistence")+
  theme_bw() -> meas5SpectraFig
### meas6
meas6Smoted %>%
  gather(2:307, key = "wavelength", value = "absorbance") %>%
  mutate(reflectance = 1 - absorbance) %>%
  group_by(wavelength, persist) %>%
  summarise(reflectance = mean(reflectance)) %>%
  separate(wavelength, sep = "_", into = c("nm", "wavelength")) -> meas6SmotedSpectra
meas6SmotedSpectra$wavelength <- as.numeric(meas6SmotedSpectra$wavelength)
ggplot(meas6SmotedSpectra, aes(x = wavelength, y = reflectance, color = persist)) +
  geom_point(alpha = 0.5, size = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Wavelength (nm)",
       y = "Reflectance",
       color = "Fruit Persistence")+
  theme_bw() -> meas6SpectraFig
### meas7
meas7Smoted %>%
  gather(2:307, key = "wavelength", value = "absorbance") %>%
  mutate(reflectance = 1 - absorbance) %>%
  group_by(wavelength, persist) %>%
  summarise(reflectance = mean(reflectance)) %>%
  separate(wavelength, sep = "_", into = c("nm", "wavelength")) -> meas7SmotedSpectra
meas7SmotedSpectra$wavelength <- as.numeric(meas7SmotedSpectra$wavelength)
ggplot(meas7SmotedSpectra, aes(x = wavelength, y = reflectance, color = persist)) +
  geom_point(alpha = 0.5, size = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Wavelength (nm)",
       y = "Reflectance",
       color = "Fruit Persistence")+
  theme_bw() -> meas7SpectraFig
## variable importance graph----
### meas1
meas1Imp$importance %>%
  rownames_to_column(var = "predictor") %>%
  separate(predictor, sep = "_", into = c("nm", "wavelength")) %>%
  mutate(wavelength.nm = as.numeric(wavelength)) %>%
  ggplot(aes(x = wavelength.nm, y = Overall)) +
  geom_bar(stat = "identity", fill = "forestgreen", alpha = 0.5) +
  labs(x = "Wavelength (nm)",
       y = "Variable Importance")+
  theme_bw() -> meas1ImpFig
meas1Plot <- plot_grid(meas1SpectraFig+ theme(axis.text.x = element_blank(),
                                 axis.ticks.x = element_blank(),
                                 axis.title.x = element_blank() ),
          meas1ImpFig, ncol = 1, nrow = 2, align = "v", axis = "r")
save_plot("plots/2021/honeycrisp/model_building/meas1RfVariableImpPlot.png", meas1Plot)
### meas2
meas2Imp$importance %>%
  rownames_to_column(var = "predictor") %>%
  separate(predictor, sep = "_", into = c("nm", "wavelength")) %>%
  mutate(wavelength.nm = as.numeric(wavelength)) %>%
  ggplot(aes(x = wavelength.nm, y = Overall)) +
  geom_bar(stat = "identity", fill = "forestgreen", alpha = 0.5) +
  labs(x = "Wavelength (nm)",
       y = "Variable Importance")+
  theme_bw() -> meas2ImpFig
meas2Plot <- plot_grid(meas2SpectraFig+ theme(axis.text.x = element_blank(),
                                 axis.ticks.x = element_blank(),
                                 axis.title.x = element_blank() ),
          meas2ImpFig, ncol = 1, nrow = 2, align = "v", axis = "r")
save_plot("plots/2021/honeycrisp/model_building/meas2RfVariableImpPlot.png", meas2Plot)
### meas3
meas3Imp$importance %>%
  rownames_to_column(var = "predictor") %>%
  separate(predictor, sep = "_", into = c("nm", "wavelength")) %>%
  mutate(wavelength.nm = as.numeric(wavelength)) %>%
  ggplot(aes(x = wavelength.nm, y = Overall)) +
  geom_bar(stat = "identity", fill = "forestgreen", alpha = 0.5) +
  labs(x = "Wavelength (nm)",
       y = "Variable Importance")+
  theme_bw() -> meas3ImpFig
meas3Plot <- plot_grid(meas3SpectraFig+ theme(axis.text.x = element_blank(),
                                 axis.ticks.x = element_blank(),
                                 axis.title.x = element_blank() ),
          meas3ImpFig, ncol = 1, nrow = 2, align = "v", axis = "r")
save_plot("plots/2021/honeycrisp/model_building/meas3RfVariableImpPlot.png", meas3Plot)
### meas4
meas4Imp$importance %>%
  rownames_to_column(var = "predictor") %>%
  separate(predictor, sep = "_", into = c("nm", "wavelength")) %>%
  mutate(wavelength.nm = as.numeric(wavelength)) %>%
  ggplot(aes(x = wavelength.nm, y = Overall)) +
  geom_bar(stat = "identity", fill = "forestgreen", alpha = 0.5) +
  labs(x = "Wavelength (nm)",
       y = "Variable Importance")+
  theme_bw() -> meas4ImpFig
meas4Plot <- plot_grid(meas4SpectraFig+ theme(axis.text.x = element_blank(),
                                 axis.ticks.x = element_blank(),
                                 axis.title.x = element_blank() ),
          meas4ImpFig, ncol = 1, nrow = 2, align = "v", axis = "r")
save_plot("plots/2021/honeycrisp/model_building/meas4RfVariableImpPlot.png", meas4Plot)
### meas5
meas5Imp$importance %>%
  rownames_to_column(var = "predictor") %>%
  separate(predictor, sep = "_", into = c("nm", "wavelength")) %>%
  mutate(wavelength.nm = as.numeric(wavelength)) %>%
  ggplot(aes(x = wavelength.nm, y = Overall)) +
  geom_bar(stat = "identity", fill = "forestgreen", alpha = 0.5) +
  labs(x = "Wavelength (nm)",
       y = "Variable Importance")+
  theme_bw() -> meas5ImpFig
meas5Plot <- plot_grid(meas5SpectraFig+ theme(axis.text.x = element_blank(),
                                 axis.ticks.x = element_blank(),
                                 axis.title.x = element_blank() ),
          meas5ImpFig, ncol = 1, nrow = 2, align = "v", axis = "r")
save_plot("plots/2021/honeycrisp/model_building/meas5RfVariableImpPlot.png", meas5Plot)
### meas6
meas6Imp$importance %>%
  rownames_to_column(var = "predictor") %>%
  separate(predictor, sep = "_", into = c("nm", "wavelength")) %>%
  mutate(wavelength.nm = as.numeric(wavelength)) %>%
  ggplot(aes(x = wavelength.nm, y = Overall)) +
  geom_bar(stat = "identity", fill = "forestgreen", alpha = 0.5) +
  labs(x = "Wavelength (nm)",
       y = "Variable Importance")+
  theme_bw() -> meas6ImpFig
meas6Plot <- plot_grid(meas6SpectraFig+ theme(axis.text.x = element_blank(),
                                 axis.ticks.x = element_blank(),
                                 axis.title.x = element_blank() ),
          meas6ImpFig, ncol = 1, nrow = 2, align = "v", axis = "r")
save_plot("plots/2021/honeycrisp/model_building/meas6RfVariableImpPlot.png", meas6Plot)
### meas7
meas7Imp$importance %>%
  rownames_to_column(var = "predictor") %>%
  separate(predictor, sep = "_", into = c("nm", "wavelength")) %>%
  mutate(wavelength.nm = as.numeric(wavelength)) %>%
  ggplot(aes(x = wavelength.nm, y = Overall)) +
  geom_bar(stat = "identity", fill = "forestgreen", alpha = 0.5) +
  labs(x = "Wavelength (nm)",
       y = "Variable Importance")+
  theme_bw() -> meas7ImpFig
meas7Plot <- plot_grid(meas7SpectraFig+ theme(axis.text.x = element_blank(),
                                 axis.ticks.x = element_blank(),
                                 axis.title.x = element_blank() ),
          meas7ImpFig, ncol = 1, nrow = 2, align = "v", axis = "r")
save_plot("plots/2021/honeycrisp/model_building/meas7RfVariableImpPlot.png", meas7Plot)
