library(tidyverse)

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
  select(year, aapi_prison_pop, black_prison_pop,
         native_prison_pop, latinx_prison_pop, white_prison_pop, total_prison_pop) %>%
  summarise(across(c(aapi_prison_pop, black_prison_pop, native_prison_pop, latinx_prison_pop,
                     white_prison_pop, total_prison_pop), sum, na.rm=T)) 

us_prison_rate_df<- us_prison_pop_df %>%
  mutate(aapi_prison_rate = aapi_prison_pop/total_prison_pop,
         black_prison_rate = black_prison_pop/total_prison_pop,
         native_prison_rate = native_prison_pop/total_prison_pop,
         latinx_prison_rate = latinx_prison_pop/total_prison_pop,
         white_prison_rate = white_prison_pop/total_prison_pop) %>%
  select(year, matches("rate"))

us_prison_rate_1988_df <- us_prison_rate_df %>%
  filter(year == 1970) %>%
  select(matches("rate"))

us_prison_rate_2016_df <- us_prison_rate_df %>%
  filter(year == 2016) %>%
  select(matches("rate"))

aapi_trend <- lm(aapi_prison_pop ~ year, data = us_prison_pop_df)
black_trend <- lm(black_prison_pop ~ year, data = us_prison_pop_df)
latinx_trend <- lm(latinx_prison_pop ~ year, data = us_prison_pop_df)
white_trend <- lm(white_prison_pop ~ year, data = us_prison_pop_df)
native_trend <- lm(native_prison_pop ~ year, data = us_prison_pop_df)
  
us_prison_rate_1988_df$most_prison_race_1970 <- colnames(us_prison_rate_1988_df)[apply(us_prison_rate_1988_df,1,which.max)]
us_prison_rate_1988_df$least_prison_race_1970 <- colnames(us_prison_rate_1988_df)[apply(us_prison_rate_1988_df,1,which.min)]

us_prison_rate_2016_df$most_prison_race_2016 <- colnames(us_prison_rate_2016_df)[apply(us_prison_rate_2016_df,1,which.max)]
us_prison_rate_2016_df$least_prison_race_2016 <- colnames(us_prison_rate_2016_df)[apply(us_prison_rate_2016_df,1,which.min)]

summary_info$wa_1988_race_most_prison <- us_prison_rate_1988_df %>%
  select(most_prison_race_1970)
summary_info$wa_1988_race_least_prison <- us_prison_rate_1988_df %>%
  select(least_prison_race_1970)
summary_info$wa_2016_race_most_prison <- us_prison_rate_2016_df %>%
  select(most_prison_race_2016)
summary_info$wa_2016_race_least_prison <- us_prison_rate_2016_df %>%
  select(least_prison_race_2016)
summary_info$aapi_trend <- aapi_trend[1]
summary_info$black_trend <- black_trend[1]
summary_info$latinx_trend <- latinx_trend[1]
summary_info$white_trend <- white_trend[1]
summary_info$native_trend <- native_trend[1]

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
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
plot_jail_pop_for_us <- function()  {
  bar <- ggplot(get_year_jail_pop(), aes(year, total_jail_pop)) + 
    geom_col() +
    labs(title = "Increase of Jail Population in the U.S. (1970 - 2018)", 
         x = "Year", y = "Total Jail Population") + 
    scale_y_continuous(labels = scales::comma) 
  return(bar)   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


