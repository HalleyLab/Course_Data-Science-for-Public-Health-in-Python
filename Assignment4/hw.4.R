library(tidyverse)

annual <- read_csv("county_annual.csv")
land <- read_csv("land_area.csv")
population <- read_csv("county_pop_arcos.csv")

land <- land %>%
  rename(countyfips = STCOU) %>%
  mutate(countyfips = str_pad(as.character(countyfips), width = 5, pad = "0", side = "left"))

population <- population %>%
  mutate(countyfips = str_pad(as.character(countyfips), width = 5, pad = "0", side = "left"))

annual <- annual %>%
  mutate(countyfips = str_pad(as.character(countyfips), width = 5, pad = "0", side = "left"))

df <- annual %>%
  inner_join(population, by = c("countyfips", "year")) %>%
  inner_join(land, by = "countyfips")

avg <- df %>%
  mutate(DOSAGE_UNIT = as.numeric(DOSAGE_UNIT)) %>%
  filter(!is.na(DOSAGE_UNIT)) %>%
  group_by(year) %>%
  summarise(avg_pills = mean(DOSAGE_UNIT) / 1e6)

ggplot(avg, aes(x = year, y = avg_pills)) +
  geom_point(size = 2) +
  scale_y_continuous(
    limits = c(2.5, 4.5)
  ) +
  scale_x_continuous(breaks = 2006:2014) +
  labs(
    title = "Average Number of Opioid Pills Shipped to a US County",
    y = "Number of pills in millions",
    x = "year"
  )
