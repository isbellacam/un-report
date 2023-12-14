library(tidyverse)

gapminder_data <- read_csv("gapminder_data.csv")

summarize(gapminder_data, averagelifeExp = mean(lifeExp))

gapminder_data %>% summarize(averagelifeExp = mean(lifeExp))

gapminder_data %>% 
  summarize(averagelifeExp = mean(lifeExp))

# %>% = "shift + Ctrl + m"

gapminder_data_summarized <- gapminder_data %>% summarise(averagelifeExp = mean(lifeExp))

gapminder_data <- gapminder_data_summarized %>% summarize(averageLifeExp = mean(lifeExp))

gapminder_data %>% 
  summarise(recent_year = max(year))

gapminder_data %>% 
  filter(year == 2007) %>% 
  summarise(average = mean(lifeExp))

# What is the average GDP per capita for the first year in the dataset?

gapminder_data %>% 
  filter(year == 1952) %>% 
  summarise(average = mean(gdpPercap))

#group_by command

gapminder_data %>% 
  group_by(year) %>% 
  summarise(average=mean(lifeExp))

# calculate the average life expectancy by continent

gapminder_data %>% 
  group_by(continent) %>% 
  summarise(average=mean(lifeExp))

gapminder_data %>% 
  group_by(continent) %>% 
  summarise(average=mean(lifeExp), min=min(lifeExp))

# mutate()

gapminder_data %>% 
  mutate(gdp = pop * gdpPercap)

gapminder_data %>% 
  mutate(gdp = pop * gdpPercap, popInMillions = pop/1000000)

# select()     to remove columns

gapminder_data %>% 
  select(pop, year)

gapminder_data %>% 
  select(-continent)

# get a data frame with only the columns country, continent, year, lifeExp

gapminder_data %>% 
  select(-pop, -gdpPercap)

gapminder_data %>% 
  select(year, starts_with("c"))

gapminder_data %>% 
  select(ends_with("p"))

gapminder_data %>% 
  select(country, continent, year, lifeExp) %>% 
  pivot_wider(names_from = year, values_from = lifeExp)

# new dataset

getwd()

gapminder_data_2007 <- read_csv("gapminder_data.csv") %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-year, -continent)

read_csv("co2-un-data.csv")

# to view the data, you can create an object to temporarily view data

temp <- read_csv("co2-un-data.csv")

read_csv('co2-un-data.csv', skip = 1)

# creating a "dirty" object to refer to how the data needs to be cleaned up

co2_emissions_dirty <- read_csv("co2-un-data.csv", skip = 2, col_names = c("region", 
                                                                           "country", 
                                                                           "year", 
                                                                           "series", 
                                                                           "value", 
                                                                           "footnotes",
                                                                           "source"))

co2_emissions_dirty

read_csv("co2_emissions_dirty", skip = 1) %>% 
  rename(country=...2)

co2_emissions_dirty %>% 
  select(country, year, series, value)

co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, 
    "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
    "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value)

co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, 
    "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
    "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  count(year)

# filter out data for just 2005, and drop out the year column

co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, 
    "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
    "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year == 2005) %>% 
  select(-year)

co2_emissions <- co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, 
    "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
    "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year == 2005) %>% 
  select(-year)

inner_join(gapminder_data, co2_emissions)

inner_join(gapminder_data, co2_emissions, by= "country")

gapminder_co2 <- inner_join(gapminder_data_2007, co2_emissions, by=c("country"))

# gapminder_co2 <- inner_join(gapminder_data_2007, co2_emissions, by=c("country", "year"))

ggplot(gapminder_co2, aes(x = gdpPercap, y = per_capita_emissions)) +
  geom_point() + 
  labs(x = "GDP (per capita)", y = "C02 emitted (per capita)", 
       title = "Strong association between a nation's GDP and C02 production")






