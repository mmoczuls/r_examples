

# Intro and notes -----
# Randy Swaty
# February 12, 2024
# Make grouped bar 
# compares amounts, but no information about replacement or conversion, e.g., we do not know which ecosystem(s) were converted to agriculture

# load dependencies -----

library(tidyverse)

data <- read_csv("grouped_bar/bps_evt_glaba.csv")


# wrangle data -----

data_long <- pivot_longer(data, 
                          cols = c('past_acres', 'current_acres'),
                          names_to = 'past_present',
                          values_to = 'acres')


# create initial grouped bar chart
ggplot(data_long, aes(x = type, y = acres, fill = past_present)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Grouped Bar Chart",
       x = "Ecosystem",
       y = "Acres") +
  theme_minimal() +
  coord_flip()
