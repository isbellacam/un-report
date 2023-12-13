library(tidyverse)

gapminder_1997 <- read_csv("gapminder_1997.csv")

View(gapminder_1997)

name <- "Ben"
name

age <- 26
age

name_character <- "Harry Potter"
  
test <- read_csv("gapminder_1997.csv")

Sys.Date() #outputs the current date, used for knowing when I last ran

getwd() #output current directory

sum(5, 6) # adds numbers

round(3.1415, 2)

read_csv(file = 'gapminder_1997.csv')

ggplot(data=gapminder_1997) + 
  aes(x = gdpPercap) + 
  labs(x = "GDP Per Capita") + 
  aes(y = lifeExp)  +
  labs(y = "Life Expectancy") + 
  geom_point()  +
  labs(title = "Do people in wealthy countries live longer?") +
  aes(color = continent) + 
  scale_color_brewer(palette = "Set1") +
  aes(size = pop/1000000) +
  labs(size = "Population (in millions)") +
  aes(shape = continent)
  
ggplot(data = gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp, color = continent, size = pop/1000000, shape = continent) + 
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  labs(x = "GDP Per Capita", y = "Life Expectancy", 
       title = "Do people in wealthy countries live longer?",
       size = "Population (in millions)")

gapminder_data <- read_csv("gapminder_data.csv")

dim(gapminder_data)
head(gapminder_data)

read_csv(file = "gapminder_data.csv")

ggplot(data = gapminder_data) + 
  aes(x = year, y = lifeExp, color = continent) + 
  geom_point()






  