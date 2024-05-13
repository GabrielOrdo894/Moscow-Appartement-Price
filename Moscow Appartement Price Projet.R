# EDA Analyse

## Setup Directory and keep file

# Setting the working directory and loading the necessary library
setwd("C:/Users/gabri/Downloads/Proyectos R/Moscow Appartement Price")
library(tidyverse)

# Reading the dataset
Moscow_appartement <- read.csv("data.csv")

## View of data

# Displaying the first few rows of the dataset
head(Moscow_appartement)

## View of data structure 

# Checking the structure of the dataset
str(Moscow_appartement)

## Summary of data

# Summarizing the dataset
summary(Moscow_appartement)

## Transforming data

# Transforming some variables to factor variables for better analysis
Moscow_appartement$Apartment.type <- factor(Moscow_appartement$Apartment.type, levels = c("Secondary", "New building"))
Moscow_appartement$Metro.station <- factor(Moscow_appartement$Metro.station)
Moscow_appartement$Region <- factor(Moscow_appartement$Region)
Moscow_appartement$Renovation <- factor(Moscow_appartement$Renovation)
Moscow_appartement$Number.of.rooms <- factor(Moscow_appartement$Number.of.rooms, ordered = TRUE, levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12))

# Displaying summary after transformation
summary(Moscow_appartement)

## Checking the null values

# Checking for null values in the dataset
colSums(is.na(Moscow_appartement))

## Cheking outliers values

# Verifying the presence of outliers in the dataset
Moscow_appartement %>%
  filter(Floor > Number.of.floors) %>%
  select(Floor, Number.of.floors)

# Checking area relation for outliers
Moscow_appartement %>%
  filter(Area < Living.area) %>%
  select(Area, Living.area)

Moscow_appartement %>%
  filter(Area < Kitchen.area) %>%
  select(Area, Kitchen.area)

## Transforming price Column for become more easy to read

# Converting prices into a new column denominated in millions
Moscow_appartement$Price <- round(Moscow_appartement$Price / 100000, 3)
colnames(Moscow_appartement)[1] <- "Price.(in.million)"
summary(Moscow_appartement)

## Boxplot of Appartment price distribution
ggplot(Moscow_appartement, aes(y = `Price.(in.million)`)) +
  geom_boxplot() +
  labs(title = "Boxplot of Distribution of Appartment price", x = "Distribution", y = "Price of Appartment (in million)")

# Data Analyse

## Appartment Type

# Bar plot of Appartment type
ggplot(Moscow_appartement, aes(Apartment.type, fill = Apartment.type)) +
  geom_bar(stat = "count", show.legend = FALSE) +
  geom_text(stat = "count", aes(label = ..count..), vjust = 2) +
  labs(title = "Count of Appartment type", x = "Appartment Type") +
  theme_classic()

# Histogram of Minutes to metro
ggplot(Moscow_appartement, aes(Minutes.to.metro)) +
  geom_histogram(binwidth = 2) +
  labs(title = "Histogram of appartments by Distance to Metro in minutes", x = "Minutes to metro")

## Preparing data for bar plot
appartment_by_minutes_to_metro <- Moscow_appartement %>%
  mutate(minutes_to_metro = case_when(Minutes.to.metro <= 10 ~ "0-10",
                                      Minutes.to.metro <= 20 ~ "11-20",
                                      Minutes.to.metro <= 30 ~ "21-30",
                                      Minutes.to.metro <= 40 ~ "31-40",
                                      Minutes.to.metro <= 50 ~ "41-50",
                                      Minutes.to.metro <= 60 ~ "51-60")) %>%
  group_by(minutes_to_metro) %>%
  summarize(mean_price = mean(`Price.(in.million)`))

# Bar plot of Average price by Distance to metro
ggplot(appartment_by_minutes_to_metro, aes(minutes_to_metro, mean_price)) +
  geom_bar(stat = "identity") +
  labs(title = "Average price by Distance to metro", x = "Mins to metro", y = "Average price (in million)")

##Correlation between Variable

# Calculating correlation between price and area
cor(Moscow_appartement$`Price.(in.million)`, Moscow_appartement$Area)

# Simple linear regression between price and area
lm(`Price.(in.million)` ~ Area, data = Moscow_appartement)

# Scatter plot between Area and Price of Appartment
ggplot(Moscow_appartement, aes(Area, `Price.(in.million)`)) +
  geom_point() +
  ylim(0, 1000) +
  geom_smooth(method = "lm") +
  labs(title = "Relation between Area and Price of Appartment", y = "Price (in million)")

# Bar plot of Average Area by Distance to metro
ggplot(area_appartment_by_minutes_to_metro, aes(minutes_to_metro, mean_Area)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Area by Distance to metro", x = "Mins to metro", y = "Average Area")

# Summary of Renovation column
Moscow_appartement %>%
  group_by(Renovation) %>%
  count() %>%
  ungroup() %>%
  mutate(percent = round((n / nrow(Moscow_appartement)) * 100, 2))
