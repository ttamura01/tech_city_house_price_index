library(tidyverse)
library(ggtext)
library(patchwork)
library(glue)
library(ggtext)

setwd("/Users/takayukitamura/Documents/R_Computing/tech_city_house_price_index")

# upload house_px file
tech_house_index <- read.csv("/Users/takayukitamura/Documents/R_Computing/tech_city_house_price_index/tech_city_house_price_index.csv", sep = ",", 
                        header = TRUE, stringsAsFactors = FALSE ) 

tail(tech_house_index)

updates <- tribble(~date, ~KansasCity, ~Seattle, ~SanJose, ~SanFrancisco, ~Austin, ~Pittsburgh, ~Boston,
        "2024-04-01", 355.29, 540.76, 564.33, 505.18, 519.41, 312.96, 471.74)  

tech_house_index <- rbind(tech_house_index, updates)

# Convert 'date' column to Date format if it's not already
tech_house_index$date <- as.Date(tech_house_index$date)

sapply(tech_house_index, class)
head(tech_house_index)
tail(tech_house_index)

# Reshape the data frame from wide to long format
tech_house_index_long <- pivot_longer(tech_house_index, cols = -date, names_to = "city", values_to = "price")

# Get the latest price for each city
latest_prices <- aggregate(price ~ city, data = tech_house_index_long[tech_house_index_long$date == as.Date("2024-04-01"), ], max)

# Reorder the levels of 'city' based on the latest price 
tech_house_index_long$city <- factor(tech_house_index_long$city, levels = latest_prices[order(latest_prices$price, decreasing = TRUE), "city"])

# Plot the data with ggplot

a <- ggplot(data = tech_house_index_long, aes(x = date, y = price, color = city)) +
  geom_line() +
  labs(title = "Historical House Prices Index in US major technology cities",
       subtitle = "(house price of 1995-01-01 = 100)",
       caption = "Source = FRED(Federal Reserve Bank of St.Louis)", 
       x = NULL,
       y = "House Price Index") +
  theme(
    legend.title = element_blank(),
    legend.key = element_blank(),
    plot.caption = element_markdown(color = "grey", size = 7)
  ) 

ggsave("tech_city_house_index.png", width = 6.5, height = 5)


