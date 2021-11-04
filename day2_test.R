library(tidyverse)

surveys_complete <- read_csv("surveys_complete.csv")

surveys_plot <- ggplot(data = surveys_complete, aes(x = weight, y = hindfoot_length))

surveys_plot +
  geom_point(alpha = 0.1, aes(color = species_id)) +
  theme_classic()

ggplot(data = surveys_complete, aes(x = species_id, y = weight)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.05, color = "darkgreen") +
  theme_classic()

yearly_counts <- surveys_complete %>%
  count(year, genus)

plot_object <- ggplot(data = yearly_counts, aes(x = year, y = n)) +
  geom_line() +
  facet_wrap(facets = vars(genus)) +
  theme_bw()

ggsave("name_of_file.png", plot_object, width = 10, height = 15)