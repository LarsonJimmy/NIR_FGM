# NIR FGM: clean NIR data
# author: Jimmy Larson
# created: 6.8.21
# last edited: 6.8.21

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
  mutate(repCluster = str_extract(Filename, "R\\w.\\d")) %>%
  mutate(rep = str_extract(repCluster, "\\w\\d"),
         cluster = str_extract(repCluster, "\\w\\d$")) -> nir1
nir2 %>%
  mutate(repCluster = str_extract(Filename, "R\\w.\\d")) %>%
  mutate(rep = str_extract(repCluster, "\\w\\d"),
         cluster = str_extract(repCluster, "\\w\\d$")) -> nir2
nir3 %>%
  mutate(repCluster = str_extract(Filename, "R\\w.\\d")) %>%
  mutate(rep = str_extract(repCluster, "\\w\\d"),
         cluster = str_extract(repCluster, "\\w\\d$")) -> nir3
nir4 %>%
  mutate(repCluster = str_extract(Filename, "R\\w.\\d")) %>%
  mutate(rep = str_extract(repCluster, "\\w\\d"),
         cluster = str_extract(repCluster, "\\w\\d$")) -> nir4
nir5 %>%
  mutate(repCluster = str_extract(Filename, "R\\w.\\d")) %>%
  mutate(rep = str_extract(repCluster, "\\w\\d"),
         cluster = str_extract(repCluster, "\\w\\d$")) -> nir5
nir6 %>%
  mutate(repCluster = str_extract(Filename, "R\\w.\\d")) %>%
  mutate(rep = str_extract(repCluster, "\\w\\d"),
         cluster = str_extract(repCluster, "\\w\\d$")) -> nir6
nir7 %>%
  mutate(repCluster = str_extract(Filename, "R\\w.\\d")) %>%
  mutate(rep = str_extract(repCluster, "\\w\\d"),
         cluster = str_extract(repCluster, "\\w\\d$")) -> nir7
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
## write csv file
write_csv(nir1, "meas1_nir.csv")
write_csv(nir2, "meas2_nir.csv")
write_csv(nir3, "meas3_nir.csv")
write_csv(nir4, "meas4_nir.csv")
write_csv(nir5, "meas5_nir.csv")
write_csv(nir6, "meas6_nir.csv")
write_csv(nir7, "meas7_nir.csv")