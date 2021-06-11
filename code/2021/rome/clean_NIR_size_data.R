# NIR FGM: clean NIR and diameter data - Rome
# author: Jimmy Larson
# created: 6.10.21
# last edited: 6.10.21

## packages----
library(tidyverse)
library(lubridate)
## read in data ----
nir1 <- read_csv("data/2021/rome/felix/meas1/meas1_absorbance.csv")
nir2 <- read_csv("data/2021/rome/felix/meas2/meas2_absorbance.csv")
nir3 <- read_csv("data/2021/rome/felix/meas3/meas3_absorbance.csv")
nir4 <- read_csv("data/2021/rome/felix/meas4/meas4_absorbance.csv")
nir5 <- read_csv("data/2021/rome/felix/meas5/meas5_absorbance.csv")
size1 <- read_csv("data/2021/rome/sizeData/destructive_meas_5_18_21.csv")
size2 <- read_csv("data/2021/rome/sizeData/destructive_meas_5_20_21.csv")
size3 <- read_csv("data/2021/rome/sizeData/destructive_meas_5_24_21.csv")
size4 <- read_csv("data/2021/rome/sizeData/destructive_meas_5_27_21.csv")
size5 <- read_csv("data/2021/rome/sizeData/destructive_meas_6_1_21.csv")
## add date----
nir1 %>%
  mutate(measDate = ymd(20210518)) -> nir1
nir2 %>% 
  mutate(measDate = ymd(20210520)) -> nir2
nir3 %>% 
  mutate(measDate = ymd(20210524)) -> nir3
nir4 %>% 
  mutate(measDate = ymd(20210527)) -> nir4
nir5 %>% 
  mutate(measDate = ymd(20210601)) -> nir5
size1 %>%
  mutate(measDate = ymd(20210518)) -> size1
size2 %>% 
  mutate(measDate = ymd(20210520)) -> size2
size3 %>% 
  mutate(measDate = ymd(20210524)) -> size3
size4 %>% 
  mutate(measDate = ymd(20210527)) -> size4
size5 %>% 
  mutate(measDate = ymd(20210601)) -> size5
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
## combine dates into NIR and size dataframes----
nir <- rbind(nir1, nir2, nir3, nir4, nir5)
size <- rbind(size1, size2, size3, size4, size5)
size %>%
  drop_na() %>%
  select(measDate, rep, cluster, fruit, diam, weight)-> size
## write csv files----
write_csv(nir, "data/2021/rome/nir_absorbance.csv")
write_csv(size, "data/2021/rome/fruit_size.csv")
