# NIR FGM: Treatment Selection
# author: Jimmy Larson
# created: 4.12.21
# last edited: 4.12.21

## packages----
library(tidyverse)
library(randomizr)
## set seed----
set.seed(222)
## create treatments----
m <- as.data.frame(complete_ra(N= 10, num_arms = 2, conditions = c("ctrl", "thin")))
table(m)
print(m)
## add plot number and flag color----
m %>%
  rename(trt = `complete_ra(N = 10, num_arms = 2, conditions = c("ctrl", "thin"))`) %>%
  mutate(plot = seq(1,10),
         flag = case_when(trt == "ctrl" ~ "pink/black",
                          trt == "thin" ~ "green/black")) -> map
write_csv(map, "trt_assignment_map.csv")
