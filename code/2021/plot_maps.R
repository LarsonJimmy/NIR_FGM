# NIR FGM: Treatment Selection
# author: Jimmy Larson
# created: 4.12.21
# last edited: 4.12.21

## packages----
library(tidyverse)
## read in data ----
map <- read_csv("data/2021/maps_dataSheets/trt_assignment_map.csv")
## add trees----
map <- map[rep(seq_len(nrow(map)), each = 6),]
map %>%
  mutate(tree = rep(1:6, times = 10),
         sample = rep(c("dest1","dest2", "dest3", "non-dest", "dest4", "dest5"), times =10)) %>%
  group_by(trt) %>%
  mutate(rep = rep(1:5, each = 6)) %>% -> map
write_csv(map, "plot_map.csv")
## data sheet for non/destructive measures
sheet <- map[rep(seq_len(nrow(map)), each = 20),]
sheet %>%
  mutate(cluster = rep(1:20, times = 30)) -> sheet
sheet <- sheet[rep(seq_len(nrow(sheet)), each = 6),]
sheet%>%
  mutate(fruit = rep(1:6, times = 600)) -> sheet
sheet %>%
  filter(sample == "non-dest" & trt == "thin") -> non.sheet
write_csv(non.sheet,"repeated_meas_sheet.csv")
sheet %>%
  filter(!sample == "non-dest") -> dest.sheet
write_csv(dest.sheet,"destructive_meas_sheet.csv")
