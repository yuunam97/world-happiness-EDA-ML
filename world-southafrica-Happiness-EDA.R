"
# Name: Yuu Nam
# Title: World Happiness Analysis (South Africa vs World)
# Dataset Origin: Kaggle
# Created: 11/16/2023
# Email: yuunam97@gmail.com
"

#------------------------------------------------------------------------------#
### Initiation ###
#------------------------------------------------------------------------------#

# Clearing the workspace
rm(list = ls())

# Clearing the console space
cat("\014")

# Obtain and set the current working directory
getwd()
setwd("E:/3. R-course/Projects/3. World Happiness Ranking")

# Loading the libraries
library(tidyverse)
library(lubridate)
library(janitor)
library(dplyr)
library(ggplot2)
library(plotly)


#------------------------------------------------------------------------------#
### DATA VIEW && LOADING ###
#------------------------------------------------------------------------------#

# 1. Reading the data (Most updated one: 2023 data)
data <- read_csv("WHR2023.csv")

# 2. Checking for missing values:
data <- drop_na(data) # dropping missing/na values
colSums(is.na(data)) # missing values observed in some columns

# 3. Checking for duplicates:
sum(duplicated(data))
data <- unique(data)

# 4.1. Changing . to _
colnames(data) <- gsub(" ", "_", colnames(data), fixed = TRUE) 

# 4.2. Changing to lower case
colnames(data) <- tolower(colnames(data)) 

# 5. Renaming the columns:
data <- rename(data,
               country = country_name,
               happy_score = life_ladder,
               healthy_life_expectancy = healthy_life_expectancy_at_birth,
               freedom_life_choices = freedom_to_make_life_choices,
               perception_corruption = perceptions_of_corruption)

#------------------------------------------------------------------------------#
### WORLD VS SOUTH AFRICA HAPPINESS ANALYSIS ###
#------------------------------------------------------------------------------#

# 0. Top 3 countries, South Korea and South Africa data:
df_countries <- data %>% 
  filter(country == "Denmark"|
           country == "Finland"|
           country == "Switzerland"|
           country == "South Korea"|
           country == "South Africa")

# 0.1 Setting the fonts for legends:
t_legend <- list(family = "sans-serif",
                 size = 12)
t_labels <- list(family = "sans-serif",
                 size = 15)

# 1. Happiness Score:
plot_ly(df_countries, x = ~year, y = ~happy_score, color = ~country,
        type = "scatter",
        mode = "lines+markers",
        line = list(size = 4),
        marker = list(size = 4)) %>% 
  layout(title = list(text = "<b>Happiness Score: South Africa vs World</b>",
                      xanchor = "center", x = 0.5, y = 0.99),
         legend = list(title = list(text = "<b>Country Name:</b>"), 
                       font = t_legend),
         xaxis = list(title = list(text = "<b>Year</b>", font = t_labels)),
         yaxis = list(title = list(text = "<b>Happiness Score</b>", 
                                   font = t_labels)),
         plot_bgcolor = "#f9f9f9")

# 2. GDP per Capita (Economy Growth):
plot_ly(df_countries, x = ~year, y = ~log_gdp_per_capita, color = ~country,
        type = "scatter",
        mode = "lines+markers",
        line = list(size = 4),
        marker = list(size = 4)) %>% 
  layout(title = list(text = "<b>GDP per Capita: South Africa vs World</b>",
                      xanchor = "center", x = 0.5, y = 0.99),
         legend = list(title = list(text = "<b>Country Name:</b>"), 
                       font = t_legend),
         xaxis = list(title = list(text = "<b>Year</b>", font = t_labels)),
         yaxis = list(title = list(text = "<b>GDP per Capita</b>", 
                                   font = t_labels)),
         plot_bgcolor = "#f9f9f9")

# 3. Social Support:
plot_ly(df_countries, x = ~year, y = ~social_support, color = ~country,
        type = "scatter",
        mode = "lines+markers",
        line = list(size = 4),
        marker = list(size = 4)) %>% 
  layout(title = list(text = "<b>Social Support: South Africa vs World</b>",
                      xanchor = "center", x = 0.5, y = 0.99),
         legend = list(title = list(text = "<b>Country Name:</b>"), 
                       font = t_legend),
         xaxis = list(title = list(text = "<b>Year</b>", font = t_labels)),
         yaxis = list(title = list(text = "<b>Social Support</b>", 
                                   font = t_labels)),
         plot_bgcolor = "#f9f9f9")

# 4. Healthy Life Expectancy:
plot_ly(df_countries, x = ~year, y = ~healthy_life_expectancy, color = ~country,
        type = "scatter",
        mode = "lines+markers",
        line = list(size = 4),
        marker = list(size = 4)) %>% 
  layout(title = list(text = "<b>Healthy Life Expectancy: South Africa vs World</b>",
                      xanchor = "center", x = 0.5, y = 0.99),
         legend = list(title = list(text = "<b>Country Name:</b>"), 
                       font = t_legend),
         xaxis = list(title = list(text = "<b>Year</b>", font = t_labels)),
         yaxis = list(title = list(text = "<b>Healthy Life Expectancy</b>", 
                                   font = t_labels)),
         plot_bgcolor = "#f9f9f9")

# 5. Freedom to Make Life Choices:
plot_ly(df_countries, x = ~year, y = ~freedom_life_choices, color = ~country,
        type = "scatter",
        mode = "lines+markers",
        line = list(size = 4),
        marker = list(size = 4)) %>% 
  layout(title = list(text = "<b>Freedom to Make Life Choices: South Africa vs World</b>",
                      xanchor = "center", x = 0.5, y = 0.99),
         legend = list(title = list(text = "<b>Country Name:</b>"), 
                       font = t_legend),
         xaxis = list(title = list(text = "<b>Year</b>", font = t_labels)),
         yaxis = list(title = list(text = "<b>Freedom to Make Life Choices</b>", 
                                   font = t_labels)),
         plot_bgcolor = "#f9f9f9")

# 6. Generosity:
plot_ly(df_countries, x = ~year, y = ~generosity, color = ~country,
        type = "scatter",
        mode = "lines+markers",
        line = list(size = 4),
        marker = list(size = 4)) %>% 
  layout(title = list(text = "<b>Generosity: South Africa vs World</b>",
                      xanchor = "center", x = 0.5, y = 0.99),
         legend = list(title = list(text = "<b>Country Name:</b>"), 
                       font = t_legend),
         xaxis = list(title = list(text = "<b>Year</b>", font = t_labels)),
         yaxis = list(title = list(text = "<b>Generosity</b>", 
                                   font = t_labels)),
         plot_bgcolor = "#f9f9f9")

# 7. Perception of Corruption:
plot_ly(df_countries, x = ~year, y = ~perception_corruption, color = ~country,
        type = "scatter",
        mode = "lines+markers",
        line = list(size = 4),
        marker = list(size = 4)) %>% 
  layout(title = list(text = "<b>Perception of Corruption: South Africa vs World</b>",
                      xanchor = "center", x = 0.5, y = 0.99),
         legend = list(title = list(text = "<b>Country Name:</b>"), 
                       font = t_legend),
         xaxis = list(title = list(text = "<b>Year</b>", font = t_labels)),
         yaxis = list(title = list(text = "<b>Perception of Corruption</b>", 
                                   font = t_labels)),
         plot_bgcolor = "#f9f9f9")

# 8. Positive Affect:
plot_ly(df_countries, x = ~year, y = ~positive_affect, color = ~country,
        type = "scatter",
        mode = "lines+markers",
        line = list(size = 4),
        marker = list(size = 4)) %>% 
  layout(title = list(text = "<b>Positive Affect: South Africa vs World</b>",
                      xanchor = "center", x = 0.5, y = 0.99),
         legend = list(title = list(text = "<b>Country Name:</b>"), 
                       font = t_legend),
         xaxis = list(title = list(text = "<b>Year</b>", font = t_labels)),
         yaxis = list(title = list(text = "<b>Positive Affect</b>", 
                                   font = t_labels)),
         plot_bgcolor = "#f9f9f9")

# 9. Negative Affect:
plot_ly(df_countries, x = ~year, y = ~negative_affect, color = ~country,
        type = "scatter",
        mode = "lines+markers",
        line = list(size = 4),
        marker = list(size = 4)) %>% 
  layout(title = list(text = "<b>Negative Affect: South Africa vs World</b>",
                      xanchor = "center", x = 0.5, y = 0.99),
         legend = list(title = list(text = "<b>Country Name:</b>"), 
                       font = t_legend),
         xaxis = list(title = list(text = "<b>Year</b>", font = t_labels)),
         yaxis = list(title = list(text = "<b>Negative Affect</b>", 
                                   font = t_labels)),
         plot_bgcolor = "#f9f9f9")
