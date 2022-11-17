library(tidyverse)
library("sf") # Map time!
library("spData")

us_sf_data <- spData::us_states %>% as_tibble()

select <- dplyr::select
filter <- dplyr::filter
tuesdata <- tidytuesdayR::tt_load(x = 2022, week = 45)
state_stations <- tuesdata$state_stations
station_info   <- tuesdata$station_info

# Some aggregated stats -- ignore
format_by_state <- state_stations %>% count(state, format)
most_format_by_state <- format_by_state %>% group_by(state) %>% 
  filter(n == max(n)) %>% ungroup()

## 1. Categorical: most common radio station format per state
most_format_with_geometry <- full_join(
  most_format_by_state %>% select(state, format),
  us_sf_data %>% select(NAME, geometry) %>% rename(state = NAME),
  by = "state"
)
ggplot(most_format_with_geometry) +
  geom_sf(aes(geometry = geometry, fill = format))

## 2. Numeric: number of country stations per state
country_stations_by_state <- full_join(
  format_by_state %>% filter(format == 'Country'),
  us_sf_data %>% select(NAME, geometry) %>% rename(state = NAME),
  by = "state"
)
p <- ggplot(country_stations_by_state) +
  geom_sf(aes(geometry = geometry, fill = n)) + 
  geom_sf_text(aes(geometry = geometry, label = n), color = 'red') + 
  scale_fill_viridis_c(option = "D") + 
  labs(title = paste(emo::ji('cowboy'), " Howdy partner"),
       fill = "Number of Country \nradio stations") +
  theme_void() + 
  theme(
    legend.position = 'bottom',
    plot.title = element_text(hjust = 0.5, size = 16, family = "American Typewriter"),
    plot.background = element_rect(fill = '#ffffff', color = NA)
  )
ggsave(here::here("plots/2022-45-radio-stations.png"),
       plot = p, width = 16, height = 9, scale = 0.75)
