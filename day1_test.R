install.packages(c("tidyverse", "hexbin", "patchwork", "RSQLite"))

library(tidyverse)
library(hexbin)

3 + 5
12 / 7

weight_kg <- 55

2.2 * weight_kg
# changing from kg to lb
weight_lb <- 2.2 * weight_kg

rounded <- round(3.142, 2)

download.file(url = "https://ndownloader.figshare.com/files/2292169",
              destfile = "portal_data_joined.csv")

surveys <- read_csv("portal_data_joined.csv")

head(surveys)
str(surveys)
summary(surveys)

month_surveys <- surveys$month

surveys$sex <- factor(surveys$sex)

surveys_small <- surveys %>%
  filter(weight < 5) %>%
  select(species_id, sex, weight)

surveys_2 <- surveys %>%
  filter(!is.na(weight)) %>%
  mutate(weight_kg = weight / 1000) %>%
  head()

surveys %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE)) %>%
  head()
