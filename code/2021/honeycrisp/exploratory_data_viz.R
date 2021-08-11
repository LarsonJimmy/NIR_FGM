# NIR FGM: exploratory data viz - Honeycrisp
# author: Jimmy Larson
# created: 6.18.21
# last edited: 6.18.21

## packages----
library(tidyverse)
## read in data---
sizeNIR <- read_csv("data/2021/honeycrisp/meas_full_dataset.csv")
## create long form for wavelength to be a column----
sizeNIR %>%
  gather(7:312, key = "wavelength", value = "absorbance") -> sizeNIRlong
## calculate reflectance----
sizeNIRlong %>%
  mutate(reflectance = 1 - absorbance) -> sizeNIRlong
## visualize spectra by date and persistence----
sizeNIRlong %>%
  drop_na() %>%
  group_by(measDate, wavelength, persist) %>%
  summarise(reflectance = mean(reflectance)) -> sizeNIRsum
sizeNIRsum$wavelength <- as.numeric(sizeNIRsum$wavelength)  
sizeNIRsum %>%
  filter(wavelength > 400) %>%
  ggplot(aes(x = wavelength, y = reflectance, color = persist)) +
  geom_point(alpha = 0.5) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~measDate)+
  labs(x = "Wavelength (nm)",
       y = "Reflectance",
       color = "Fruit Persistence")+
  theme_bw()
ggsave("persistence_spectra_honeycrisp.png", width = 12, height = 7)
## fruit diameter over time----
sizeNIR %>%
  mutate(size = as.numeric(diam),
         meas = case_when(measDate == "2021-04-28" ~ -5,
                          measDate == "2021-05-01" ~ -1,
                          measDate == "2021-05-05" ~ 3,
                          measDate == "2021-05-07" ~ 5,
                          measDate == "2021-05-09" ~ 7,
                          measDate == "2021-05-11" ~ 9,
                          measDate == "2021-05-14" ~ 12)) %>%
  select(meas, size, persist) %>%
  group_by(meas, persist) %>%
  drop_na() %>%
  summarise_each(funs(mean, sd, se = sd(.)/sqrt(n())))%>%
  ggplot(aes(x = meas, y = mean, color = persist)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(x = meas, ymin = mean - se, ymax = mean + se), width = 0.4)+
  scale_color_brewer(palette = "Set1") +
  labs(x = "Days Following Thinner Application",
       y = "Fruit Diameter (mm)",
       color = "Fruit Persistence") +
  theme_bw()
ggsave("persistence_diam_honeycrisp.png", width = 12, height = 7)
