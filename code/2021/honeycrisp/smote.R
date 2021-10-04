# NIR FGM: NIR Individual Dates Model Building - Honeycrisp, balance dataset
# author: Jimmy Larson
# created: 10.4.21
# last edited: 10.4.21

## load packages----
library(tidyverse)
#install.packages("unbalanced")
library(unbalanced)
## set seed----
set.seed(123)
## load data----
meas1 <- read_csv("data/2021/honeycrisp/model_building/meas1_spectra.csv")
meas2 <- read_csv("data/2021/honeycrisp/model_building/meas2_spectra.csv")
meas3 <- read_csv("data/2021/honeycrisp/model_building/meas3_spectra.csv")
meas4 <- read_csv("data/2021/honeycrisp/model_building/meas4_spectra.csv")
meas5 <- read_csv("data/2021/honeycrisp/model_building/meas5_spectra.csv")
meas6 <- read_csv("data/2021/honeycrisp/model_building/meas6_spectra.csv")
meas7 <- read_csv("data/2021/honeycrisp/model_building/meas7_spectra.csv")
diff1 <- read_csv("data/2021/honeycrisp/model_building/diff1_spectra.csv")
diff2 <- read_csv("data/2021/honeycrisp/model_building/diff2_spectra.csv")
diff3 <- read_csv("data/2021/honeycrisp/model_building/diff3_spectra.csv")
diff4 <- read_csv("data/2021/honeycrisp/model_building/diff4_spectra.csv")
diff5 <- read_csv("data/2021/honeycrisp/model_building/diff5_spectra.csv")
diff6 <- read_csv("data/2021/honeycrisp/model_building/diff6_spectra.csv")
## select predictor and response varaibles----
### measurement 1
meas1 %>%
  drop_na() %>%
  select(persistCode, 6:311) -> meas1
### measurement 3
meas2 %>%
  drop_na() %>%
  select(persistCode, 6:311) -> meas2
### measurement 3
meas3 %>%
  drop_na() %>%
  select(persistCode, 6:311) -> meas3
### measurement 4
meas4 %>%
  drop_na() %>%
  select(persistCode, 6:311) -> meas4
### measurement 5
meas5 %>%
  drop_na() %>%
  select(persistCode, 6:311) -> meas5
### measurement 6
meas6 %>%
  drop_na() %>%
  select(persistCode, 6:311) -> meas6
### measurement 7
meas7 %>%
  drop_na() %>%
  select(persistCode, 6:311) -> meas7
### diff 1
diff1 %>%
  drop_na() %>%
  select(persistCode, 6:311) -> diff1
### diff 2
diff2 %>%
  drop_na() %>%
  select(persistCode, 6:311) -> diff2
### diff 3
diff3 %>%
  drop_na() %>%
  select(persistCode, 6:311) -> diff3
### diff 4
diff4 %>%
  drop_na() %>%
  select(persistCode, 6:311) -> diff4
### diff 5
diff5 %>%
  drop_na() %>%
  select(persistCode, 6:311) -> diff5
### diff 6
diff6 %>%
  drop_na() %>%
  select(persistCode, 6:311) -> diff6
## SMOTE----
### measurement 1
meas1$persistCode <- as.factor(meas1$persistCode)
meas1Smote <- ubSMOTE(meas1[,2:307], meas1$persistCode, perc.over = 475, perc.under = 125)
table(meas1Smote$Y)
table(meas1$persistCode)
### measurement 2
meas2$persistCode <- as.factor(meas2$persistCode)
meas2Smote <- ubSMOTE(meas2[,2:307], meas2$persistCode, perc.over = 475, perc.under=125)
table(meas2Smote$Y)
### measurement 3
meas3$persistCode <- as.factor(meas3$persistCode)
meas3Smote <- ubSMOTE(meas3[,2:307], meas3$persistCode, perc.over = 475, perc.under=125)
table(meas3Smote$Y)
### measurement 4
meas4$persistCode <- as.factor(meas4$persistCode)
meas4Smote <- ubSMOTE(meas4[,2:307], meas4$persistCode, perc.over = 475, perc.under=125)
table(meas4Smote$Y)
### measurement 5
meas5$persistCode <- as.factor(meas5$persistCode)
meas5Smote <- ubSMOTE(meas5[,2:307], meas5$persistCode, perc.over = 475, perc.under=125)
table(meas5Smote$Y)
### measurement 6
meas6$persistCode <- as.factor(meas6$persistCode)
meas6Smote <- ubSMOTE(meas6[,2:307], meas6$persistCode, perc.over = 475, perc.under=125)
table(meas6Smote$Y)
### measurement 7
meas7$persistCode <- as.factor(meas7$persistCode)
meas7Smote <- ubSMOTE(meas7[,2:307], meas7$persistCode, perc.over = 475, perc.under=125)
table(meas7Smote$Y)
### diff 1
diff1$persistCode <- as.factor(diff1$persistCode)
diff1Smote <- ubSMOTE(diff1[,2:307], diff1$persistCode, perc.over = 475, perc.under = 125)
table(diff1Smote$Y)
### diff 2
diff2$persistCode <- as.factor(diff2$persistCode)
diff2Smote <- ubSMOTE(diff2[,2:307], diff2$persistCode, perc.over = 475, perc.under = 125)
table(diff2Smote$Y)
### diff 3
diff3$persistCode <- as.factor(diff3$persistCode)
diff3Smote <- ubSMOTE(diff3[,2:307], diff3$persistCode, perc.over = 475, perc.under = 125)
table(diff3Smote$Y)
### diff 4
diff4$persistCode <- as.factor(diff4$persistCode)
diff4Smote <- ubSMOTE(diff4[,2:307], diff4$persistCode, perc.over = 475, perc.under = 125)
table(diff4Smote$Y)
### diff 5
diff5$persistCode <- as.factor(diff5$persistCode)
diff5Smote <- ubSMOTE(diff5[,2:307], diff5$persistCode, perc.over = 475, perc.under = 125)
table(diff5Smote$Y)
### diff 6
diff6$persistCode <- as.factor(diff6$persistCode)
diff6Smote <- ubSMOTE(diff6[,2:307], diff6$persistCode, perc.over = 475, perc.under = 125)
table(diff6Smote$Y)
## create dataframe----
meas1Smoted <- as.data.frame(cbind(meas1Smote$Y, meas1Smote$X))
meas2Smoted <- as.data.frame(cbind(meas2Smote$Y, meas2Smote$X))
meas3Smoted <- as.data.frame(cbind(meas3Smote$Y, meas3Smote$X))
meas4Smoted <- as.data.frame(cbind(meas4Smote$Y, meas4Smote$X))
meas5Smoted <- as.data.frame(cbind(meas5Smote$Y, meas5Smote$X))
meas6Smoted <- as.data.frame(cbind(meas6Smote$Y, meas6Smote$X))
meas7Smoted <- as.data.frame(cbind(meas7Smote$Y, meas7Smote$X))
diff1Smoted <- as.data.frame(cbind(diff1Smote$Y, diff1Smote$X))
diff2Smoted <- as.data.frame(cbind(diff2Smote$Y, diff2Smote$X))
diff3Smoted <- as.data.frame(cbind(diff3Smote$Y, diff3Smote$X))
diff4Smoted <- as.data.frame(cbind(diff4Smote$Y, diff4Smote$X))
diff5Smoted <- as.data.frame(cbind(diff5Smote$Y, diff5Smote$X))
diff6Smoted <- as.data.frame(cbind(diff6Smote$Y, diff6Smote$X))
## save dataframes-----
write_csv(meas1Smoted, "data/2021/honeycrisp/model_building/meas1Smoted.csv")
write_csv(meas2Smoted, "data/2021/honeycrisp/model_building/meas2Smoted.csv")
write_csv(meas3Smoted, "data/2021/honeycrisp/model_building/meas3Smoted.csv")
write_csv(meas4Smoted, "data/2021/honeycrisp/model_building/meas4Smoted.csv")
write_csv(meas5Smoted, "data/2021/honeycrisp/model_building/meas5Smoted.csv")
write_csv(meas6Smoted, "data/2021/honeycrisp/model_building/meas6Smoted.csv")
write_csv(meas7Smoted, "data/2021/honeycrisp/model_building/meas7Smoted.csv")
write_csv(diff1Smoted, "data/2021/honeycrisp/model_building/diff1Smoted.csv")
write_csv(diff2Smoted, "data/2021/honeycrisp/model_building/diff2Smoted.csv")
write_csv(diff3Smoted, "data/2021/honeycrisp/model_building/diff3Smoted.csv")
write_csv(diff4Smoted, "data/2021/honeycrisp/model_building/diff4Smoted.csv")
write_csv(diff5Smoted, "data/2021/honeycrisp/model_building/diff5Smoted.csv")
write_csv(diff6Smoted, "data/2021/honeycrisp/model_building/diff6Smoted.csv")