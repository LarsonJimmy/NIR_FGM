# NIR FGM: combine NIR and diameter data - Rome
# author: Jimmy Larson
# created: 6.10.21
# last edited: 6.10.21

## packages----
library(tidyverse)
## read in data----
matched <- read_csv("data/2021/rome/file_to_merge_size_NIR.csv")
nir <- read_csv("data/2021/rome/nir_absorbance.csv")
## join data----
dataSet <- left_join(matched, nir, by = "Filename")
## select columns----
dataSet %>%
  rename(measDate = measDate.x,
         rep = rep.x,
         cluster = cluster.x,
         fruit = fruit.x) %>%
  select(measDate, rep, cluster, fruit, diam, weight, 14:321) -> dataSet
## write .csv file----
write_csv(dataSet, "data/2021/rome/destructive_meas_full_dataset.csv")
