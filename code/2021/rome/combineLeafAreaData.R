# NIR FGM: Rome Combine Leaf Area Data
# author: Jimmy Larson
# created: 10.12.21
# last edited: 10.12.21

## load packages----
library(tidyverse)
library(lubridate)
## read in data----
sample1Num <- read_csv("data/2021/rome/leafArea/sample1_leafArea.csv")
sample1Area <- read_csv("data/2021/rome/leafArea/sample1_area.csv")
sample2Num <- read_csv("data/2021/rome/leafArea/sample2_leafArea.csv")
sample2Area <- read_csv("data/2021/rome/leafArea/sample2_area.csv")
sample3Num <- read_csv("data/2021/rome/leafArea/sample3_leafArea.csv")
sample3Area <- read_csv("data/2021/rome/leafArea/sample3_area.csv")
sample4Num <- read_csv("data/2021/rome/leafArea/sample4_leafArea.csv")
sample4Area <- read_csv("data/2021/rome/leafArea/sample4_area.csv")
sample5Num <- read_csv("data/2021/rome/leafArea/sample5_leafArea.csv")
sample5Area <- read_csv("data/2021/rome/leafArea/sample5_area.csv")
sample6Area <- read_csv("data/2021/rome/leafArea/sample6_leafArea.csv")
sample6Num <- read_csv("data/2021/rome/leafArea/sample6_leafNum.csv")
harvestArea <- read_csv("data/2021/rome/leafArea/finalMeas_leafArea.csv")
harvestNum <- read_csv("data/2021/rome/leafArea/leaf_weights_num_oct6.csv")
## gather columns for leaf number samples 1-5----
### sample1
sample1Num %>%
  select(rep, cluster, bourse1.wt.g, bourse2.wt.g, spur.wt.g) %>%
  gather(3:5, key = "branch", value = "weight.g") %>%
  separate(branch, sep = ".w", into = c("branch", "measurement")) %>%
  select(rep, cluster, branch, weight.g)-> sample1Weights
sample1Num %>%
  select(rep, cluster, bourse1.num, bourse2.num, spur.num) %>%
  gather(3:5, key = "branch", value = "leafNumber") %>%
  separate(branch, sep = ".n", into = c("branch", "measurement")) %>%
  select(rep, cluster, branch, leafNumber)-> sample1Num
sample1Combined <- left_join(sample1Weights, sample1Num)
sample1Combined %>%
  mutate(date = ymd(20210518)) -> sample1Combined
### sample2
sample2Num %>%
  select(rep, cluster, bourse1.wt.g, bourse2.wt.g, spur.wt.g) %>%
  gather(3:5, key = "branch", value = "weight.g") %>%
  separate(branch, sep = ".w", into = c("branch", "measurement")) %>%
  select(rep, cluster, branch, weight.g)-> sample2Weights
sample2Num %>%
  select(rep, cluster, bourse1.num, bourse2.num, spur.num) %>%
  gather(3:5, key = "branch", value = "leafNumber") %>%
  separate(branch, sep = ".n", into = c("branch", "measurement")) %>%
  select(rep, cluster, branch, leafNumber)-> sample2Num
sample2Combined <- left_join(sample2Weights, sample2Num)
sample2Combined %>%
  mutate(date = ymd(20210520)) -> sample2Combined
### sample3
sample3Num %>%
  select(rep, cluster, bourse1.wt.g, bourse2.wt.g, spur.wt.g) %>%
  gather(3:5, key = "branch", value = "weight.g") %>%
  separate(branch, sep = ".w", into = c("branch", "measurement")) %>%
  select(rep, cluster, branch, weight.g)-> sample3Weights
sample3Num %>%
  select(rep, cluster, bourse1.num, bourse2.num, spur.num) %>%
  gather(3:5, key = "branch", value = "leafNumber") %>%
  separate(branch, sep = ".n", into = c("branch", "measurement")) %>%
  select(rep, cluster, branch, leafNumber)-> sample3Num
sample3Combined <- left_join(sample3Weights, sample3Num)
sample3Combined %>%
  mutate(date = ymd(20210524)) -> sample3Combined
### sample4
sample4Num %>%
  select(rep, cluster, bourse1.wt.g, bourse2.wt.g, spur.wt.g) %>%
  gather(3:5, key = "branch", value = "weight.g") %>%
  separate(branch, sep = ".w", into = c("branch", "measurement")) %>%
  select(rep, cluster, branch, weight.g)-> sample4Weights
sample4Num %>%
  select(rep, cluster, bourse1.num, bourse2.num, spur.num) %>%
  gather(3:5, key = "branch", value = "leafNumber") %>%
  separate(branch, sep = ".n", into = c("branch", "measurement")) %>%
  select(rep, cluster, branch, leafNumber)-> sample4Num
sample4Combined <- left_join(sample4Weights, sample4Num)
sample4Combined %>%
  mutate(date = ymd(20210527)) -> sample4Combined
### sample5
sample5Num %>%
  select(rep, cluster, bourse1.wt.g, bourse2.wt.g, spur.wt.g) %>%
  gather(3:5, key = "branch", value = "weight.g") %>%
  separate(branch, sep = ".w", into = c("branch", "measurement")) %>%
  select(rep, cluster, branch, weight.g)-> sample5Weights
sample5Num %>%
  select(rep, cluster, bourse1.num, bourse2.num, spur.num) %>%
  gather(3:5, key = "branch", value = "leafNumber") %>%
  separate(branch, sep = ".n", into = c("branch", "measurement")) %>%
  select(rep, cluster, branch, leafNumber)-> sample5Num
sample5Combined <- left_join(sample5Weights, sample5Num)
sample5Combined %>%
  mutate(date = ymd(20210601)) -> sample5Combined
## combine leaf number and area for samples 1-5----
### sample 1
sample1Area %>%
  rename(branch = `Leaf Type (Spur=S Bourse=B)`,
         rep = Rep,
         cluster = Spurs) -> sample1Area
sample1 <-left_join(sample1Area, sample1Combined)
sample1 %>%
  rename(leafArea.cm2 = Area) %>%
  select(date, rep, cluster, branch, leafArea.cm2, weight.g, leafNumber) -> sample1 
### sample 2
sample2Area %>%
  rename(branch = `Leaf Type (Spur=S Bourse=B)`,
         rep = Rep,
         cluster = Spurs) -> sample2Area
sample2 <-left_join(sample2Area, sample2Combined)
sample2 %>%
  rename(leafArea.cm2 = Area) %>%
  select(date, rep, cluster, branch, leafArea.cm2, weight.g, leafNumber) -> sample2 
### sample 3
sample3Area %>%
  rename(branch = `Leaf Type (Spur=S Bourse=B)`,
         rep = Rep,
         cluster = Spurs) -> sample3Area
sample3 <-left_join(sample3Area, sample3Combined)
sample3 %>%
  rename(leafArea.cm2 = Area) %>%
  select(date, rep, cluster, branch, leafArea.cm2, weight.g, leafNumber) -> sample3 
### sample 4
sample4Area %>%
  rename(branch = `Leaf Type (Spur=S Bourse=B)`,
         rep = Rep,
         cluster = Spurs) -> sample4Area
sample4 <-left_join(sample4Area, sample4Combined)
sample4 %>%
  rename(leafArea.cm2 = Area) %>%
  select(date, rep, cluster, branch, leafArea.cm2, weight.g, leafNumber) -> sample4 
### sample 5
sample5Area %>%
  rename(branch = `Leaf Type (Spur=S Bourse=B)`,
         rep = Rep,
         cluster = Spurs) -> sample5Area
sample5 <-left_join(sample5Area, sample5Combined)
sample5 %>%
  rename(leafArea.cm2 = Area) %>%
  select(date, rep, cluster, branch, leafArea.cm2, weight.g, leafNumber) -> sample5
## combine sample 6 weight and number----
sample6Area %>%
  rename(branch = `Leaf Type (Spur=S Bourse=B)`,
         rep = Rep,
         cluster = Spurs) -> sample6Area
sample6Combined <- left_join(sample6Area, sample6Num)
sample6Combined %>%
  rename(leafArea.cm2 = Area,
         leafNumber = leaf.num) %>%
  mutate(date = ymd(20210623)) %>%
  select(date, rep, cluster, branch, leafArea.cm2, weight.g, leafNumber) -> sample6Combined
## combine harvest weight and number----
harvestArea %>%
  rename(branch = `Leaf Type (Spur=S Bourse=B)`,
         rep = Rep,
         cluster = Spurs) -> harvestArea
harvestCombined <- left_join(harvestArea, harvestNum)
harvestCombined %>%
  rename(leafArea.cm2 = Area,
         leafNumber = leaf.num) %>%
  mutate(date = ymd(20211006)) %>%
  select(date, rep, cluster, branch, leafArea.cm2, weight.g, leafNumber) -> harvestCombined
## combine all dates----
leafArea <- rbind(sample1, sample2, sample3, sample4, sample5, sample6Combined, harvestCombined)
## write .csv----
write_csv(leafArea, "data/2021/rome/leaf_area_kon.csv")
