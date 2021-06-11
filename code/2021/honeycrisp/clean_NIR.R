# NIR FGM: clean NIR and diameter data - Honeycrisp
# author: Jimmy Larson
# created: 6.8.21
# last edited: 6.9.21

## packages----
library(tidyverse)
library(lubridate)
## read in data ----
nir1 <- read_csv("data/2021/honeycrisp/felix/meas1_absorbance.csv")
nir2 <- read_csv("data/2021/honeycrisp/felix/meas2_absorbance.csv")
nir3 <- read_csv("data/2021/honeycrisp/felix/meas3_absorbance.csv")
nir4 <- read_csv("data/2021/honeycrisp/felix/meas4_absorbance.csv")
nir5 <- read_csv("data/2021/honeycrisp/felix/meas5_absorbance.csv")
nir6 <- read_csv("data/2021/honeycrisp/felix/meas6_absorbance.csv")
nir7 <- read_csv("data/2021/honeycrisp/felix/meas7_absorbance.csv")
diam <- read_csv("data/2021/honeycrisp/diams.csv")
## add date----
nir1 %>%
  mutate(measDate = ymd(20210428)) -> nir1
nir2 %>%
  mutate(measDate = ymd(20210501)) -> nir2
nir3 %>%
  mutate(measDate = ymd(20210505)) -> nir3
nir4 %>%
  mutate(measDate = ymd(20210507)) -> nir4
nir5 %>%
  mutate(measDate = ymd(20210509)) -> nir5
nir6 %>%
  mutate(measDate = ymd(20210511)) -> nir6
nir7 %>%
  mutate(measDate = ymd(20210514)) -> nir7
## extract rep and cluster info from file name----
nir1 %>%
  mutate(rep = str_extract(Filename, "R([1-5])"),
         cluster = str_extract(Filename, "S[:digit:]{1,}(?=_)")) -> nir1
nir2 %>%
  mutate(rep = str_extract(Filename, "R([1-5])"),
         cluster = str_extract(Filename, "S[:digit:]{1,}(?=_)")) -> nir2
nir3 %>%
  mutate(rep = str_extract(Filename, "R([1-5])"),
         cluster = str_extract(Filename, "S[:digit:]{1,}(?=_)")) -> nir3
nir4 %>%
  mutate(rep = str_extract(Filename, "R([1-5])"),
         cluster = str_extract(Filename, "S[:digit:]{1,}(?=_)")) -> nir4
nir5 %>%
  mutate(rep = str_extract(Filename, "R([1-5])"),
         cluster = str_extract(Filename, "S[:digit:]{1,}(?=_)")) -> nir5
nir6 %>%
  mutate(rep = str_extract(Filename, "R([1-5])"),
         cluster = str_extract(Filename, "S[:digit:]{1,}(?=_)")) -> nir6
nir7 %>%
  mutate(rep = str_extract(Filename, "R([1-5])"),
         cluster = str_extract(Filename, "S[:digit:]{1,}(?=_)")) -> nir7
## add fruit number in cluster----
nir1 %>%
  group_by(rep, cluster) %>%
  mutate(fruit = row_number()) %>%
  select(rep, cluster, fruit,1:313) -> nir1
nir2 %>%
  group_by(rep, cluster) %>%
  mutate(fruit = row_number()) %>%
  select(rep, cluster, fruit,1:313) -> nir2
nir3 %>%
  group_by(rep, cluster) %>%
  mutate(fruit = row_number()) %>%
  select(rep, cluster, fruit,1:313) -> nir3
nir4 %>%
  group_by(rep, cluster) %>%
  mutate(fruit = row_number()) %>%
  select(rep, cluster, fruit,1:313) -> nir4
nir5 %>%
  group_by(rep, cluster) %>%
  mutate(fruit = row_number()) %>%
  select(rep, cluster, fruit,1:313) -> nir5
nir6 %>%
  group_by(rep, cluster) %>%
  mutate(fruit = row_number()) %>%
  select(rep, cluster, fruit,1:313) -> nir6
nir7 %>%
  group_by(rep, cluster) %>%
  mutate(fruit = row_number()) %>%
  select(rep, cluster, fruit,1:313) -> nir7
## combine dates for NIR----
nir <- rbind(nir1, nir2, nir3, nir4, nir5, nir6, nir7)
##long form diam data----
diam %>%
  gather('meas1', 'meas2', 'meas3', 'meas4', 'meas5', 'meas6', 'meas7', key = "meas", value = "diam") %>%
  select(rep, cluster, fruit, meas, diam, persist) -> diam
## add date to diam----
diam %>%
  mutate(date = case_when(meas == "meas1" ~ ymd(20210428),
                          meas == "meas2" ~ ymd(20210501),
                          meas == "meas3" ~ ymd(20210505),
                          meas == "meas4" ~ ymd(20210507),
                          meas == "meas5" ~ ymd(20210509),
                          meas == "meas6" ~ ymd(20210511),
                          meas == "meas7" ~ ymd(20210514))) -> diam
## write csv files----
write_csv(nir, "nir_absorbance.csv")
write_csv(diam, "diam_long.csv")
