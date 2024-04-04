install.packages("tidyverse")
library(tidyverse)

unicef_indicator_1 <- read_csv("unicef_indicator_1.csv")
unicef_indicator_2 <- read_csv("unicef_indicator_2.csv")
unicef_metadata_1_ <- read_csv("unicef_metadata.csv")
data_right_3 <- read_csv("data_right_3.csv")

data_join <- full_join(unicef_indicator_1, unicef_indicator_2, by = join_by(country, year))
data_join <- full_join(unicef_indicator_1, unicef_metadata_1_, by = join_by(country, year))
data_join <- full_join(unicef_indicator_1, data_right_3)

data_join <- unicef_indicator_1 %>%
  full_join(unicef_indicator_2) %>%
  full_join(unicef_metadata_1_) %>%
  full_join(data_right_3)

map_world <- map_data("world")

# Map 2005
map_data_join_2005 <- data_join %>%
  filter(year == 2005)

map_data_join_2005 <- full_join(data_join_2005, map_world, by = c("country" = "region"))

ggplot(map_data_join_2005)+
  aes( x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon() +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Low Birth Weight % by Country in 2005",
  fill = "%") + 
  theme(
    text = element_text(family = "calibri")
  )

# Map 2020
map_data_join_2020 <- data_join %>%
  filter(year == 2020)

map_data_join_2020 <- full_join(map_data_join_2020, map_world, by = c("country" = "region"))

ggplot(map_data_join_2020)+
  aes( x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon() +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Low Birth Weight % by Country in 2020",
       fill = "%") +
  theme(
    text = element_text(family = "calibri")
  )

# time series

install.packages("plotly")

library(plotly)

timeseries_plot_1 <- data_join %>%
  filter(year >= 2000 & year <= 2022) %>%
  ggplot() +
  aes(year, obs_value, group = country, color = continent) +
  geom_line() +
  labs(x = "Year",
       y = "Low Birth Weight %",
       title = "Low Birth Weight % per Continent") +
  theme(text = element_text(family = "Calibri")) 

ggplotly(timeseries_plot_1) %>%
  layout(plot_bgcolor = "rgba(0,0,0,0)",
         paper_bgcolor = "rgba(0,0,0,0)")

# scatter plot

scatter_plot <- ggplot(data_join) +
  aes(obs_value, Life_exp_at_birth, color = continent, size = obs_value) +
  geom_point(alpha = 0.5) +
  labs(x = "Low Birth Weight %",
       y = "Life Expectancy at Birth",
       title = "Relationship between Low Birth Weight and Life Expectancy at Birth", color = "continent") +
  theme(text = element_text(family = "Calibri"))

hover_info <- paste("Country: ", data_join$country, "<br>",
                    "Observation Value: ", data_join$obs_value, "<br>",
                    "Life Expectancy at Birth: ", data_join$Life_exp_at_birth)

ggplotly(scatter_plot)


# Bar chart

data_join %>%
  group_by(continent, year) %>%
  summarise(m_life_exp_at_birth = mean(Life_exp_at_birth, na.rm = TRUE)) %>%
  filter(year == 2008) %>%
  ggplot() +
  aes(continent, m_life_exp_at_birth, fill = continent) +
  geom_col() +
  labs(x = "Life Expectancy at Birth",
       y = "Continent",
       title = "Average Life Expectancy at Birth per Continent") +
  theme_classic() +
  theme(
    text = element_text(family = "cailibri"), 
    axis.text.x = element_blank()
  ) +
  scale_fill_manual(values = c("green", "red", "blue", "yellow","purple","grey"))
  