# Assignment 3: Data Visualization (Incarceration)

# For this assignment, you will create a report about incarceration in the U.S., which must include:
  # An introduction of the problem domain and a description of the variable(s) you are choosing to analyze (and why!)
  # A paragraph of summary information, citing at least 5 values calculated from the data
  # A chart that shows trends over time for a variable of your choice
  # A chart that compares two variables to one another
  # A map that shows how your measure of interest varies geographically


# Summary Information

# Load the *incarceration_trends* data into a variable. "incarceration_df'
incarceration_df <- read.csv("incarceration_trends.csv")

# Load the necessary packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(maps)
library(openintro)

# How many observations are in this dataset? 'obs_incarceration'
obs_incarceration <- nrow(incarceration_df)

# How many features are in this dataset? 'num_features_incarceration'
num_features_incarceration <- ncol(incarceration_df)

# What state has the highest population of people ages 15 to 64 
# by the most recent year in the dataset? `highest_total_pop_15to64`
highest_total_pop_15to64 <- incarceration_df %>%
  filter(year == max(year)) %>%
  filter(total_pop_15to64 == max(total_pop_15to64)) %>%
  pull(state)

# What state has the highest black population ages 15 to 64 
# by the most recent year in the dataset? 'highest_black_pop_15to64'
highest_black_pop_15to64 <- incarceration_df %>%
  filter(year == max(year)) %>%
  filter(black_pop_15to64 == max(black_pop_15to64)) %>%
  pull(state)

# Which state has the highest ratio of black jail population to black population ages 15 to 64
# (black_jail_pop / black_pop_15to64)? `state_highest_ratio_black`
# (may need to create a new column to do this)
ratio_black_jail_pop <- mutate(
  incarceration_df,
  ratio_black_jail_pop = black_jail_pop / black_pop_15to64
)

state_highest_ratio_black <- incarceration_df %>%            
  group_by(state) %>%                   
  summarize(ratio_black_jail_pop = n()) %>%
  filter(ratio_black_jail_pop == max(ratio_black_jail_pop)) %>% 
  pull(state) 

# Which state has the highest ratio of white jail population to white population ages 15 to 64
# (white_jail_pop / white_pop_15to64)? `state_highest_ratio_white`
# (may need to create a new column to do this)
ratio_white_jail_pop <- mutate(
  incarceration_df,
  ratio_white_jail_pop = white_jail_pop / white_pop_15to64
)

state_highest_ratio_white <- incarceration_df %>%            
  group_by(state) %>%                   
  summarize(ratio_white_jail_pop = n()) %>%          
  filter(ratio_white_jail_pop == max(ratio_white_jail_pop)) %>% 
  pull(state)

# What county has the highest black jail population by the most recent year 
# in this dataset? 'highest_black_jail_pop'
# (answer may return multiple counties)
highest_black_jail_pop <- incarceration_df %>%
  group_by(state) %>%
  filter(year == max(year)) %>%
  filter(black_jail_pop == max(black_jail_pop)) %>%
  pull(county_name)

# # What county has the highest white jail population by the most recent year 
# in this dataset? 'highest_white_jail_pop'
# (answer may return multiple counties)
highest_white_jail_pop <- incarceration_df %>%
  group_by(state) %>%
  filter(year == max(year)) %>%
  filter(white_jail_pop == max(white_jail_pop)) %>%
  pull(county_name)


# Add a new column to 'incarceration_df` called `location`
# that stores the county and state (separated by a comma and space).
# (make sure to keep the original columns)
counties <- incarceration_df %>%
  unite(location, county_name, state, sep = ", ", remove = FALSE)

# How has the white jail population in King County, WA changed 
# from 1990 to 2018? 'change_white_jail_pop_king'
white_jail_pop_king_1990 <- counties %>%
  filter(location == "King County, WA") %>%
  filter(year == "1990") %>%
  pull(white_jail_pop)

white_jail_pop_king_2018 <- counties %>%
  filter(location == "King County, WA") %>%
  filter(year == "2018") %>%
  pull(white_jail_pop)

change_white_jail_pop_king <- white_jail_pop_king_2018 - white_jail_pop_king_1990
  
# How has the black jail population in King County, WA changed 
# from 1990 to 2018? 'change_black_jail_pop_king'
black_jail_pop_king_1990 <- counties %>%
  filter(location == "King County, WA") %>%
  filter(year == "1990") %>%
  pull(black_jail_pop)

black_jail_pop_king_2018 <- counties %>%
  filter(location == "King County, WA") %>%
  filter(year == "2018") %>%
  pull(black_jail_pop)

change_black_jail_pop_king <- black_jail_pop_king_2018 - black_jail_pop_king_1990



# Trends Over Time Chart

# Trends in WA Jail Population Over Time
data_wa <- filter(incarceration_df, state == "WA")
trends_over_time <- ggplot(data = data_wa) + 
  geom_smooth(mapping = aes(x = year, y = aapi_jail_pop, color = "AAPI")) +
  geom_smooth(mapping = aes(x = year, y = black_jail_pop, color = "Black")) +
  geom_smooth(mapping = aes(x = year, y = latinx_jail_pop, color = "Latinx")) +
  geom_smooth(mapping = aes(x = year, y = native_jail_pop, color = "Native")) +
  geom_smooth(mapping = aes(x = year, y = white_jail_pop, color = "White")) +
  labs(title = "Trends In WA Jail Population Over Time", x = "Year", y = "Population") +
  scale_color_manual(name = "Race",
                   breaks = c("AAPI", "Black", "Latinx", "Native", "White"),
                   values = c("AAPI" = "red", "Black" = "black", "Latinx" = "blue", "Native" = "purple", "White" = "yellow"))


# Variable Comparison Chart

# Total Jail Population vs. Black Jail Population
variable_comp <- incarceration_df %>%
  filter(year == max(year)) %>%
  ggplot() +
  geom_line(aes(x = black_jail_pop, y = total_jail_pop), data = incarceration_df) +
  scale_x_continuous(breaks=seq(0, 15000, 3000)) +
  scale_y_continuous(breaks=seq(0, 24000, 3000)) +
  labs(title = "Total Jail Population vs. Black Jail Population", x = "Black Jail Population", y = "Total Jail Population")


# Map

# White Jail Population Across the United States

incarceration_mod <- incarceration_df %>%
  filter(year == max(year)) %>%
  group_by(state) %>%
  mutate(state = tolower(state.name[match(state, state.abb)])) %>%
  mutate(white_jail_pop_2018 = sum(white_jail_pop, na.rm = TRUE))

state_shapes <- map_data("state") %>%
  unite(polyname, region, subregion, sep = ",", na.rm = TRUE) %>%
  left_join(state.fips, by="polyname")

names(state_shapes)[names(state_shapes) == 'polyname'] <- 'state'

map_data <- left_join(incarceration_mod, state_shapes, by = "state") 

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

incarceration_map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x= long, y = lat, group = group, fill = white_jail_pop_2018), 
    color="gray", size = 0.3
  ) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(map_data$white_jail_pop_2018)), na.value = "white", low = "yellow", high = "red", name = "Population") +
  blank_theme +
  labs(title = "White Jail Population Across the U.S.") 


