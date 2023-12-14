#Script to analyze relationship between GDP and Life Expectancy
library(tidyverse)
gapminder_1997 <- read_csv("data/gapminder_1997.csv")
View(gapminder_1997)

name <- "Ben"
name

age <- 26
age

name_character <- "Harry Potter"
name_character

test <- read_csv("data/gapminder_1997.csv")

Sys.Date() #outputs the current date, used for knowing when I last ran

getwd() #output current directory

sum(5,6)

round(3.1415)

round(3.1415, digits = 2)

read_csv(file = 'gapminder_1997.csv')

#Use code comments to provide context for the work that we are doing in our current script

ggplot(data=gapminder_1997) + 
  aes(x=gdpPercap) +
  labs(x = 'GDP Per Capita') +
  aes(y = lifeExp) +
  labs(y = "Life Expectancy") +
  geom_point()
labs(title = "Do people in wealthy countries live longer?")
aes(color = continent) +
  scale_color_brewer(palette = "Set1") +
  aes(size = pop/1000000) +
  labs(size = "Population (in millions)") +
  aes(shape = continent)

ggplot(data=gapminder_1997) + 
  aes(x=gdpPercap, y = lifeExp, color = continent, size = pop/1000000) +
  labs(x = "GDP Per Capita", y = "Life Expectancy",
       title = "Do people in wealthy countries live longer",
       size = "Population (in millions)") +
  geom_point() +
  scale_color_brewer(palette = "Set1")

gapminder_data <- read_csv("gapminder_data.csv")
dim(gapminder_data)
head(gapminder_data)

ggplot(gapminder_data, aes(x = year, y = lifeExp, color = continent)) +
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Year", y = "Life Expectancy")

ggsave("figures/gdpPercap_lifeExp.png")
