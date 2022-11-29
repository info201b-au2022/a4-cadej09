library(tidyverse)
library(ggmap)
library(maps)
library(mapdata)

# The functions might be useful for A4
source("../source/a4-helpers.R")
incarceration_df <- get_data()

## Section 2  ----
#----------------------------------------------------------------------------#
# Your functions and variables might go here ...
# In this section, our task was to wrangle the given data set and find summarized information
# that can later be used to simply explain the idea of the data set. In this list,
# I tried to find the race that had the most prison population
summary_info <- list()

us_prison_pop_df <- incarceration_df %>%
  group_by(year) %>%
  select(
    year, aapi_prison_pop, black_prison_pop,
    native_prison_pop, latinx_prison_pop, white_prison_pop, total_prison_pop
  ) %>%
  summarise(across(c(
    aapi_prison_pop, black_prison_pop, native_prison_pop, latinx_prison_pop,
    white_prison_pop, total_prison_pop
  ), sum, na.rm = T))

us_prison_rate_df <- us_prison_pop_df %>%
  mutate(
    aapi_prison_rate = aapi_prison_pop / total_prison_pop,
    black_prison_rate = black_prison_pop / total_prison_pop,
    native_prison_rate = native_prison_pop / total_prison_pop,
    latinx_prison_rate = latinx_prison_pop / total_prison_pop,
    white_prison_rate = white_prison_pop / total_prison_pop
  ) %>%
  select(year, matches("rate"))

us_prison_rate_1970_df <- us_prison_rate_df %>%
  filter(year == 1970) %>%
  select(matches("rate"))

us_prison_rate_2016_df <- us_prison_rate_df %>%
  filter(year == 2016) %>%
  select(matches("rate"))

us_prison_rate_1970_df$most_prison_race_1970 <- colnames(us_prison_rate_1970_df)[apply(us_prison_rate_1970_df, 1, which.max)]
us_prison_rate_1970_df$least_prison_race_1970 <- colnames(us_prison_rate_1970_df)[apply(us_prison_rate_1970_df, 1, which.min)]

us_prison_rate_2016_df$most_prison_race_2016 <- colnames(us_prison_rate_2016_df)[apply(us_prison_rate_2016_df, 1, which.max)]
us_prison_rate_2016_df$least_prison_race_2016 <- colnames(us_prison_rate_2016_df)[apply(us_prison_rate_2016_df, 1, which.min)]

black_prison_pop_year <- incarceration_df %>%
  group_by(year) %>%
  summarise(black_prison_pop = sum(black_prison_pop, na.rm = T))

black_prison_pop_state <- incarceration_df %>%
  group_by(state) %>%
  summarise(black_prison_pop = sum(black_prison_pop, na.rm = T))

summary_info$wa_1970_race_most_prison <- us_prison_rate_1970_df %>%
  select(most_prison_race_1970)
summary_info$wa_1970_race_least_prison <- us_prison_rate_1970_df %>%
  select(least_prison_race_1970)
summary_info$wa_2016_race_most_prison <- us_prison_rate_2016_df %>%
  select(most_prison_race_2016)
summary_info$wa_2016_race_least_prison <- us_prison_rate_2016_df %>%
  select(least_prison_race_2016)
summary_info$year_most_black_prison <- black_prison_pop_year %>%
  filter(black_prison_pop == max(black_prison_pop, na.rm = T)) %>%
  select(year)
summary_info$black_prison_pop_2013 <- black_prison_pop_year %>%
  filter(year == 2013) %>%
  select(black_prison_pop)
summary_info$state_most_black_prison <- black_prison_pop_state %>%
  filter(black_prison_pop == max(black_prison_pop, na.rm = T)) %>%
  select(state)
summary_info$black_prison_pop_ca <- black_prison_pop_state %>%
  filter(state == "CA") %>%
  select(black_prison_pop)

#----------------------------------------------------------------------------#

## Section 3  ----
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# In this section, our task is to create two functions. One that will wrangle out data set into
# a subset that we can use in another functions to plot our data. The plotting will be done on
# prison population in the U.S. from 1970 to 2018 using a geom_col.

# This function ... breaks down the incarceration_df into subsets so that it only
# contains information about the prison population in the U.S. from 1970 to 2018.
get_year_jail_pop <- function() {
  prison_pop_1979_2018 <- incarceration_df %>%
    group_by(year) %>%
    summarise(total_jail_pop = sum(total_jail_pop, na.rm = T))
  return(prison_pop_1979_2018)
}

# This function ... calls the above get_year_jail_pop function to change data frame into
# a subset of data frame that would be used to create a bar visualizations that is used to
# show the growth of the U.S. prison population from 1970 to 2018 in the U.S.
plot_jail_pop_for_us <- function() {
  bar <- ggplot(get_year_jail_pop(), aes(year, total_jail_pop)) +
    geom_col() +
    labs(
      title = "Increase of Jail Population in the U.S. (1970 - 2018)",
      x = "Year", y = "Total Jail Population"
    ) +
    scale_y_continuous(labels = scales::comma)
  return(bar)
}
#----------------------------------------------------------------------------#

## Section 4  ----
#----------------------------------------------------------------------------#
# Growth of Prison Population by State
# In this section, our task is to create two functions where one function will take a
# vector of state names and use that to filter out the incarceration_df into a smaller
# subset df with specified states and will be used in another function to plot that data frame
# so that the plotting will be done on prison population of those states from 1970 to 2018
# using line charts.

# This functions takes in a manually specified vectors of state names (when called) and wrangles
# the incarceration_df into a subset that will only contain the states that are speified.
# Will contain it's sum of jail_pop by year and state.
get_jail_pop_by_states <- function(states) {
  state_pop <- incarceration_df %>%
    group_by(state, year) %>%
    filter(state %in% c(states)) %>%
    summarise(total_jail_pop = sum(total_jail_pop, na.rm = T))
  return(state_pop)
}

# This function calls the above function to use its wrangled subset to create a line chart
# containing increase of jail population from 1970 - 2018. Different states will differ in color.
plot_jail_pop_by_states <- function(states) {
  line <- ggplot(
    get_jail_pop_by_states(c(states)),
    aes(x = year, y = total_jail_pop, group = state)
  ) +
    geom_line(aes(color = state)) +
    labs(
      title = "Increase of Jail Population by States (1970 - 2018)",
      x = "Year", y = "Total Jail Population"
    ) +
    scale_y_continuous(labels = scales::comma)
  return(line)
}
#----------------------------------------------------------------------------#

## Section 5  ----
#----------------------------------------------------------------------------#
# Finding Inequalities in Incarceration Using Population of Total and Black in U.S. and Incarcerated Population.
# In this section, I will be creating functions that will be used to wrangle incarceration
# trends data set into subsets so that it can be used find the Black Population
# and the Incarceration of Black Population (jail + prison) grouped by the urbanicity to find the ratio.
# We will also look into the ratio of total population vs total incarcerated population to mention the inequality.
# Then another function will be made to call the get_black_incarceration function to plot our subset
# with interested variables and values to visualize our findings in inequality using charts.

# This function will wrangle and crate a subset of the incarceration_df into a
# variable called total_and_black_incarceration to group by urbanicity and mutate, summarise,
# to eventually create a data frame containing ratio of the incarceration for Black population.
get_black_incarceration_ratio <- function() {
  total_and_black_incarceration <- incarceration_df %>%
    group_by(urbanicity) %>%
    mutate(
      black_incarceration_pop = black_jail_pop + black_prison_pop,
      total_incarceration_pop = total_jail_pop + total_prison_pop
    ) %>%
    summarise(
      black_incarceration_pop = sum(black_incarceration_pop, na.rm = T),
      total_incarceration_pop = sum(total_incarceration_pop, na.rm = T),
      total_black_pop_15to64 = sum(black_pop_15to64, na.rm = T),
      total_pop_15to64 = sum(total_pop_15to64, na.rm = T)
    ) %>%
    mutate(black_rate = (black_incarceration_pop / total_black_pop_15to64)) %>%
    mutate(total_rate = (total_incarceration_pop / total_pop_15to64)) %>%
    mutate(black_total_rate = (black_incarceration_pop / total_pop_15to64)) %>%
    filter(urbanicity != "") %>%
    select(urbanicity, matches("rate")) %>%
    pivot_longer(cols = -urbanicity, names_to = "rate")
  return(total_and_black_incarceration)
}

# This function calls the above data wrangling function to grab the subset data frame
# and turn it into a barplot with x axis being urbanicity and y being the incarceration ratio.
# It shows three different values: Black incarceration compared to Black population,
# Total incarceration compared to Total population, and Black incarceration compared to Total population.
plot_black_incarceration_ratio <- function() {
  ggplot(data = get_black_incarceration_ratio()) +
    geom_col(
      mapping = aes(x = urbanicity, y = value, fill = rate),
      position = position_dodge2(reverse = TRUE)
    ) +
    labs(
      title = "Ratio of Incarceration for Black Population Based on Urbanicity",
      subtitle = "In comparison to the Total Incarceration Population and to Black Incarceration with Total",
      x = "Urbanicity", y = "Incarceration Ratio"
    ) +
    scale_fill_manual(
      values = c("black", "red", "blue"),
      labels = c(black_rate = "BlackToBlack", total_rate = "TotalToTotal", black_total_rate = "BlackToTotal")
    )
}
#----------------------------------------------------------------------------#

## Section 6  ----
#----------------------------------------------------------------------------#
# Black Incarceration to White Incarceration by Counties in WA
# In this section, we will create two functions in order to

states <- map_data("state")
wa_state <- subset(states, region == "washington")
counties <- map_data("county")
wa_county <- subset(counties, region == "washington")
wa_county$county <- toupper(wa_county$subregion)
minimalize <- theme(
  axis.line = element_blank(), # remove axis lines
  axis.text = element_blank(), # remove axis labels
  axis.ticks = element_blank(), # remove axis ticks
  axis.title = element_blank(), # remove axis titles
  plot.background = element_blank(), # remove gray background
  panel.grid.major = element_blank(), # remove major grid lines
  panel.grid.minor = element_blank(), # remove minor grid lines
  panel.border = element_blank(),
)
wa_base <- ggplot(wa_state, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "gray")
wa_base <- wa_base + minimalize +
  geom_polygon(data = wa_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)

#
get_county_inequality <- function() {
  incarceration_df$county_name <- gsub(" County", "", incarceration_df$county_name)
  wa_county_inequality <- incarceration_df %>%
    filter(state == "WA") %>%
    mutate(county = toupper(county_name)) %>%
    group_by(county) %>%
    mutate(
      black_incarceration_pop = black_jail_pop + black_prison_pop,
      white_incarceration_pop = white_jail_pop + white_prison_pop
    ) %>%
    summarise(
      black_incarceration_pop = sum(black_incarceration_pop, na.rm = T),
      white_incarceration_pop = sum(white_incarceration_pop, na.rm = T)
    ) %>%
    mutate(black_white_pop_ratio = black_incarceration_pop / white_incarceration_pop)
  enrol_map <- inner_join(wa_county, wa_county_inequality, by = "county")
  return(enrol_map)
}

# This function calls the above functions to join them together and use the given information to create
# a gradient map to show which county has more Black incarcerated population in compared to White incarcerated population.
plot_wa_incarceration <- function() {
  gg1 <- wa_base +
    geom_polygon(data = get_county_inequality(), aes(fill = black_white_pop_ratio), color = "white") +
    geom_polygon(color = "black", fill = NA) +
    theme_bw() +
    minimalize +
    ggtitle("Incarcerated Population Ratio of Black to White by Counties in WA (1970 - 2018)")

  county_black_to_white_incar_pop <- gg1 + scale_fill_gradient(
    low = "#000000", high = "#e74c3c",
    breaks = c(0.025, 0.05, 0.1, 0.2, 0.4, 0.8), trans = "log10",
    name = "# of Crimes"
  )
  return(county_black_to_white_incar_pop)
}

plot_wa_incarceration()
#----------------------------------------------------------------------------#

## Load data frame ----
