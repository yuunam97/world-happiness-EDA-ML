"
# Name: Yuu Nam
# Title: World Happiness Analysis (2021 and 2023)
# Dataset Origin: Kaggle
# Created: 11/16/2023
# Email: yuunam97@gmail.com
"

#------------------------------------------------------------------------------#
### PURPOSE: ###
#------------------------------------------------------------------------------#
"
1. Explore the World Happiness Data
2. Which country is the happiest? 
3. Identify which factors contributes to the most towards happiness score.
4. What are the factors' correlation towards regression?  
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
library(ggcorrplot) # correlation plot
library(plotly)
library(modelr)

#------------------------------------------------------------------------------#
### DATA VIEW && LOADING ###
#------------------------------------------------------------------------------#

# 1. Reading the data
data <- read_csv("WHR2021.csv")

# 2. Data Description:
View(data)
## 2.1 Data Dimensions
dim(data)       # 1949 rows + 11 columns
## 2.2 Data Columns
glimpse(data)   # Characters for categorical and Numericals (Double).
## 2.3 Summary of data
summary(data)
    # Missing values observed, the mean and median are close to each
    # other, indicating data not skewed/symmetrical.

# 3. Checking for missing values:
data <- drop_na(data) # dropping missing/na values
colSums(is.na(data)) # missing values observed in some columns

# 4. Checking for duplicates:
sum(duplicated(data))
data <- unique(data)

# 5.1. Changing . to _
colnames(data) <- gsub(" ", "_", colnames(data), fixed = TRUE) 

# 5.2. Changing to lower case
colnames(data) <- tolower(colnames(data)) 

# 6. Renaming the columns:
data <- rename(data,
               country = country_name,
               happy_score = life_ladder,
               healthy_life_expectancy = healthy_life_expectancy_at_birth,
               freedom_life_choices = freedom_to_make_life_choices,
               perception_corruption = perceptions_of_corruption)

View(data)

#------------------------------------------------------------------------------#
### Univariate Analysis ###
#------------------------------------------------------------------------------#

#-----------------------------
# Font settings for visuals:
#-----------------------------

t_title <- list(family = "sans-serif",
                size = 20)
t_labels <- list(family = "sans-serif",
                 size = 15)
t_legend <- list(family = "sans-serif",
                 size = 12)

#------------------------------------------------------
# 1. Countries with the highest and Lowest Happy Score:
#------------------------------------------------------

# 1.1 Obtaining the dataframes:
df_happiest <- data %>% 
  group_by(country) %>% 
  summarise(happy_score = mean(happy_score)) %>% 
  arrange(desc(happy_score)) %>% 
  mutate(happiness_rank = as.integer(rank(-happy_score))) %>% 
  slice(1:10)

df_saddest <- data %>% 
  group_by(country) %>%
  summarise(happy_score = mean(happy_score)) %>%
  arrange((happy_score)) %>% 
  mutate(happiness_rank = as.integer(rank(-happy_score))) %>% 
  slice(1:10)

# Plotting the graph:
plot_ly(df_happiest, x = ~happy_score, y = ~reorder(country, happy_score), 
        type = 'bar', 
        name = 'Happiness Score',
        orientation = "h",
        text = ~round(happy_score, 2), 
        textposition = 'auto',
        marker = list(color = '#ffa300',
                      line = list(color = "darkred", width = 1))) %>%
  layout(title = list(text = "<b>Top 10 Happiest Countries</b>",
                      xanchor = "center", x = 0.5, y = 0.95), 
         xaxis = list(title = list(text = "<b>Happiness Score</b>", 
                                   font = t_labels), 
                      showline = TRUE, 
                      showticklabels = TRUE, 
                      domain = c(0, 0.90),
                      font = t_labels), 
         yaxis = list(title = "", 
                      showline = TRUE, 
                      showticklabels = TRUE, 
                      domain = c(0, 0.90)),
         plot_bgcolor = "#f8f8ff") 

plot_ly(df_saddest, x = ~happy_score, y = ~reorder(country, -happy_score), 
        type = 'bar', 
        name = 'Happiness Score',
        orientation = "h",
        text = ~round(happy_score, 2), 
        textposition = 'auto',
        marker = list(color = 'darkred',
                      line = list(color = "#ffa300", width = 1))) %>%
  layout(title = list(text = "<b>Most Saddest Countries</b>",
                      xanchor = "center", x = 0.5, y = 0.95), 
         xaxis = list(title = list(text = "<b>Happiness Score</b>", 
                                   font = t_labels), 
                      showline = TRUE, 
                      showticklabels = TRUE, 
                      domain = c(0, 0.90),
                      font = t_labels), 
         yaxis = list(title = "", 
                      showline = TRUE, 
                      showticklabels = TRUE, 
                      domain = c(0, 0.90)),
         plot_bgcolor = "#f8f8ff") 


#------------------------------------------------------
# 2. Countries with the highest Healthy Life Expectancy
#------------------------------------------------------

# 2.1 Obtaining the dataframes:
df_highest_hle <- data %>% 
  group_by(country) %>% 
  summarise(healthy_life_expectancy = mean(healthy_life_expectancy)) %>% 
  arrange(desc(healthy_life_expectancy)) %>% 
  mutate(hle_rank = as.integer(rank(-healthy_life_expectancy))) %>% 
  slice(1:10)

# Plotting the graph:
plot_ly(df_highest_hle, x = ~healthy_life_expectancy, y = ~reorder(country, healthy_life_expectancy), 
        type = 'bar', 
        name = 'Healthy Life Expectancy',
        orientation = "h",
        text = ~round(healthy_life_expectancy, 2), 
        textposition = 'auto',
        marker = list(color = '#a6d75b',
                      line = list(color = "#76c68f", width = 1))) %>%
  layout(title = list(text = "<b>Top 10 Countries with Highest Healthy Life Expectancy</b>",
                      xanchor = "center", x = 0.5, y = 0.95), 
         xaxis = list(title = list(text = "<b>Healthy Life Expectancy</b>", 
                                   font = t_labels), 
                      showline = TRUE, 
                      showticklabels = TRUE, 
                      domain = c(0, 0.90),
                      font = t_labels), 
         yaxis = list(title = "", 
                      showline = TRUE, 
                      showticklabels = TRUE, 
                      domain = c(0, 0.90)),
         plot_bgcolor = "#f8f8ff")


#---------------------------------------------
# 3. Countries with the highest GDP per Capita
#---------------------------------------------

# 3.1 Obtaining the dataframes:
df_highest_gdp <- data %>% 
  group_by(country) %>% 
  summarise(gdp_per_capita = mean(log_gdp_per_capita)) %>% 
  arrange(desc(gdp_per_capita)) %>% 
  mutate(gdp_rank = as.integer(rank(-gdp_per_capita))) %>% 
  slice(1:10)

# Plotting the graph:
plot_ly(df_highest_gdp, x = ~gdp_per_capita, y = ~reorder(country, gdp_per_capita), 
        type = 'bar', 
        name = 'GDP per capita',
        orientation = "h",
        text = ~round(gdp_per_capita, 2), 
        textposition = 'auto',
        marker = list(color = '#22a7f0',
                      line = list(color = "#115f9a", width = 1))) %>%
  layout(title = list(text = "<b>Top 10 Countries with Highest GDP per capita</b>",
                      xanchor = "center", x = 0.5, y = 0.95), 
         xaxis = list(title = list(text = "<b>GDP per capita</b>", 
                                   font = t_labels), 
                      showline = TRUE, 
                      showticklabels = TRUE, 
                      domain = c(0, 0.90),
                      font = t_labels), 
         yaxis = list(title = "", 
                      showline = TRUE, 
                      showticklabels = TRUE, 
                      domain = c(0, 0.90)),
         plot_bgcolor = "#f8f8ff")


#---------------------------------------------
# 4. Countries with the highest Social Support
#---------------------------------------------

# 4.1 Obtaining the dataframes:
df_highest_ss <- data %>% 
  group_by(country) %>% 
  summarise(social_support = mean(social_support)) %>% 
  arrange(desc(social_support)) %>% 
  mutate(ss_rank = as.integer(rank(-social_support))) %>% 
  slice(1:10)

# Plotting the graph:
plot_ly(df_highest_ss, x = ~social_support, y = ~reorder(country, social_support), 
        type = 'bar', 
        name = 'Social Support',
        orientation = "h",
        text = ~round(social_support, 2), 
        textposition = 'auto',
        marker = list(color = '#f7ca18',
                      line = list(color = "#f4b350", width = 1))) %>%
  layout(title = list(text = "<b>Top 10 Countries with Highest Social Support</b>",
                      xanchor = "center", x = 0.5, y = 0.95), 
         xaxis = list(title = list(text = "<b>Social Support</b>", 
                                   font = t_labels), 
                      showline = TRUE, 
                      showticklabels = TRUE, 
                      domain = c(0, 0.90),
                      font = t_labels), 
         yaxis = list(title = "", 
                      showline = TRUE, 
                      showticklabels = TRUE, 
                      domain = c(0, 0.90)),
         plot_bgcolor = "#f8f8ff")

#-----------------------------------------------------------
# 5. Countries with the highest Freedom to make life choices
#-----------------------------------------------------------

# 5.1 Obtaining the dataframes:
df_highest_freedom <- data %>% 
  group_by(country) %>% 
  summarise(freedom = mean(freedom)) %>% 
  arrange(desc(freedom)) %>% 
  mutate(freedom_rank = as.integer(rank(-freedom))) %>% 
  slice(1:10)

# Plotting the graph:
plot_ly(df_highest_freedom, x = ~freedom, y = ~reorder(country, freedom), 
        type = 'bar', 
        name = 'Freedom',
        orientation = "h",
        text = ~round(freedom, 2), 
        textposition = 'auto',
        marker = list(color = '#f62459',
                      line = list(color = "#d1353f", width = 1))) %>%
  layout(title = list(text = "<b>Top 10 Countries with Highest Freedom</b>",
                      xanchor = "center", x = 0.5, y = 0.95), 
         xaxis = list(title = list(text = "<b>Freedom</b>", 
                                   font = t_labels), 
                      showline = TRUE, 
                      showticklabels = TRUE, 
                      domain = c(0, 0.90),
                      font = t_labels), 
         yaxis = list(title = "", 
                      showline = TRUE, 
                      showticklabels = TRUE, 
                      domain = c(0, 0.90)),
         plot_bgcolor = "#f8f8ff")

#-----------------------------------------
# 6. Countries with the highest Generosity
#-----------------------------------------

# 6.1 Obtaining the dataframes:
df_highest_generosity <- data %>% 
  group_by(country) %>% 
  summarise(generosity = mean(generosity)) %>% 
  arrange(desc(generosity)) %>% 
  mutate(generosity_rank = as.integer(rank(-generosity))) %>% 
  slice(1:10)

# Plotting the graph:
plot_ly(df_highest_generosity, x = ~generosity, y = ~reorder(country, generosity), 
        type = 'bar', 
        name = 'Generosity',
        orientation = "h",
        text = ~round(generosity, 2), 
        textposition = 'auto',
        marker = list(color = '#9b59b6',
                      line = list(color = "#8e44ad", width = 1))) %>%
  layout(title = list(text = "<b>Top 10 Countries with Highest Generosity</b>",
                      xanchor = "center", x = 0.5, y = 0.95), 
         xaxis = list(title = list(text = "<b>Generosity</b>", 
                                   font = t_labels), 
                      showline = TRUE, 
                      showticklabels = TRUE, 
                      domain = c(0, 0.90),
                      font = t_labels), 
         yaxis = list(title = "", 
                      showline = TRUE, 
                      showticklabels = TRUE, 
                      domain = c(0, 0.90)),
         plot_bgcolor = "#f8f8ff")

#-----------------------------------------
# 7. Countries with the highest Corruption
#-----------------------------------------

# 7.1 Obtaining the dataframes:
df_highest_corruption <- data %>% 
  group_by(country) %>% 
  summarise(corruption = mean(corruption)) %>% 
  arrange(desc(corruption)) %>% 
  mutate(corruption_rank = as.integer(rank(-corruption))) %>% 
  slice(1:10)

# Plotting the graph:
plot_ly(df_highest_corruption, x = ~corruption, y = ~reorder(country, corruption), 
        type = 'bar', 
        name = 'Corruption',
        orientation = "h",
        text = ~round(corruption, 2), 
        textposition = 'auto',
        marker = list(color = '#3498db',
                      line = list(color = "#2980b9", width = 1))) %>%
  layout(title = list(text = "<b>Top 10 Countries with Highest Corruption</b>",
                      xanchor = "center", x = 0.5, y = 0.95), 
         xaxis = list(title = list(text = "<b>Corruption</b>", 
                                   font = t_labels), 
                      showline = TRUE, 
                      showticklabels = TRUE, 
                      domain = c(0, 0.90),
                      font = t_labels), 
         yaxis = list(title = "", 
                      showline = TRUE, 
                      showticklabels = TRUE, 
                      domain = c(0, 0.90)),
         plot_bgcolor = "#f8f8ff")

#-------------------------------------------
# 8. Countries with the best Positive Affect
#-------------------------------------------

# 8.1 Obtaining the dataframes:
df_highest_positive_affect <- data %>% 
  group_by(country) %>% 
  summarise(positive_affect = mean(positive_affect)) %>% 
  arrange(desc(positive_affect)) %>% 
  mutate(positive_affect_rank = as.integer(rank(-positive_affect))) %>% 
  slice(1:10)

# Plotting the graph:
plot_ly(df_highest_positive_affect, x = ~positive_affect, y = ~reorder(country, positive_affect), 
        type = 'bar', 
        name = 'Positive Affect',
        orientation = "h",
        text = ~round(positive_affect, 2), 
        textposition = 'auto',
        marker = list(color = '#2ecc71',
                      line = list(color = "#27ae60", width = 1))) %>%
  layout(title = list(text = "<b>Top 10 Countries with Highest Positive Affect</b>",
                      xanchor = "center", x = 0.5, y = 0.95), 
         xaxis = list(title = list(text = "<b>Positive Affect</b>", 
                                   font = t_labels), 
                      showline = TRUE, 
                      showticklabels = TRUE, 
                      domain = c(0, 0.90),
                      font = t_labels), 
         yaxis = list(title = "", 
                      showline = TRUE, 
                      showticklabels = TRUE, 
                      domain = c(0, 0.90)),
         plot_bgcolor = "#f8f8ff")

#-------------------------------------------
# 9. Countries with the Worst Negative Affect
#-------------------------------------------

# 9.1 Obtaining the dataframes:
df_highest_negative_affect <- data %>% 
  group_by(country) %>% 
  summarise(negative_affect = mean(negative_affect)) %>% 
  arrange(desc(negative_affect)) %>% 
  mutate(negative_affect_rank = as.integer(rank(-negative_affect))) %>% 
  slice(1:10)

# Plotting the graph:
plot_ly(df_highest_negative_affect, x = ~negative_affect, y = ~reorder(country, negative_affect), 
        type = 'bar', 
        name = 'Negative Affect',
        orientation = "h",
        text = ~round(negative_affect, 2), 
        textposition = 'auto',
        marker = list(color = '#e74c3c',
                      line = list(color = "#c0392b", width = 1))) %>%
  layout(title = list(text = "<b>Top 10 Countries with Highest Negative Affect</b>",
                      xanchor = "center", x = 0.5, y = 0.95), 
         xaxis = list(title = list(text = "<b>Negative Affect</b>", 
                                   font = t_labels), 
                      showline = TRUE, 
                      showticklabels = TRUE, 
                      domain = c(0, 0.90),
                      font = t_labels), 
         yaxis = list(title = "", 
                      showline = TRUE, 
                      showticklabels = TRUE, 
                      domain = c(0, 0.90)),
         plot_bgcolor = "#f8f8ff")


#------------------------------------------------------------------------------#
### Bivariate Analysis (between Target vs FEATURES) ###
#------------------------------------------------------------------------------#

#-----------------------------
# Font settings for visuals:
#-----------------------------

t_title <- list(family = "sans-serif",
                 size = 20)
t_labels <- list(family = "sans-serif",
                 size = 15)
t_legend <- list(family = "sans-serif",
                 size = 12)

#-------------------------------
# 1. GDP per capita vs Happiness
#-------------------------------

# 1.1 Getting the regression formula
fit <- lm(happy_score ~ log_gdp_per_capita, data = data)

# 1.2 Plot + Regression line 
data %>% 
  plot_ly(x = ~log_gdp_per_capita) %>% 
  add_markers(y = ~happy_score, 
              color = ~happy_score,
              colors = c("#dedad2", "#54bebe"),
              name = "Data",
              text = ~paste("GDP/Capita: ", round(log_gdp_per_capita, 2),
                            '<br>Happy Score: ', round(happy_score, 2))) %>%
  add_lines(x = ~log_gdp_per_capita, y = ~fitted(fit), 
            color = I("#d7658b"), 
            name = "Reg. line",
            text = ~paste("GDP/Capita: ", round(log_gdp_per_capita, 2),
                          '<br>Happy Score: ', round(happy_score, 2))) %>%
  colorbar(title = list(text = "<b>Score Lvl:</b>", 
                        font = t_labels),
           tickvals = c(3, 5, 7),
           len = 0.5,
           y = 1,
           ypad = 20,
           thickness = 15) %>%
  layout(title = list(text = "<b>GDP per capita vs Happiness</b>", 
                      xanchor = "center", font = t_title, 
                      x = 0.5, y = 0.99),
         legend = list(title = list(text = "<b>Legend:</b>"), 
                       font = t_legend),
         xaxis = list(title = list(text = "<b>GDP per Capita</b>", 
                                   font = t_labels)),
         yaxis = list(title = list(text = "<b>Happiness Score</b>", 
                                   font = t_labels)), 
         plot_bgcolor = "#f9f9f9")


#-------------------------------
# 2. Social support vs Happiness
#-------------------------------

# 2.1 Getting the regression formula
fit <- lm(happy_score ~ social_support, data = data)

# 2.2 Plot + Regression line
data %>% 
  plot_ly(x = ~social_support) %>% 
  add_markers(y = ~happy_score, 
              color = ~happy_score,
              colors = c("#363445", "#ffb400"),
              name = "Data",
              text = ~paste("Social Support: ", round(social_support, 2),
                            '<br>Happy Score: ', round(happy_score, 2))) %>%
  add_lines(x = ~social_support, y = ~fitted(fit), 
            color = I("#e14b31"), 
            name = "Reg. line",
            text = ~paste("Social Support: ", round(social_support, 2),
                          '<br>Happy Score: ', round(happy_score, 2))) %>%
  colorbar(title = list(text = "<b>Score Lvl:</b>", 
                        font = t_labels),
           tickvals = c(3, 5, 7),
           len = 0.5,
           y = 1,
           ypad = 20,
           thickness = 15) %>%
  layout(title = list(text = "<b>Social Support vs Happiness</b>", 
                      xanchor = "center", font = t_title, 
                      x = 0.5, y = 0.99),
         legend = list(title = list(text = "<b>Legend:</b>"), 
                       font = t_legend),
         xaxis = list(title = list(text = "<b>Social Support</b>", 
                                   font = t_labels)),
         yaxis = list(title = list(text = "<b>Happiness Score</b>", 
                                   font = t_labels)), 
         plot_bgcolor = "#f9f9f9")


#-----------------------------------------
# 3. Healthy life expectancy vs Happiness
#-----------------------------------------

# 3.1 Getting the regression formula
fit <- lm(happy_score ~ healthy_life_expectancy, data = data)

# 3.2 Plot + Regression line
data %>% 
  plot_ly(x = ~healthy_life_expectancy) %>% 
  add_markers(y = ~happy_score, 
              color = ~happy_score,
              colors = c("#e2e2e2", "#1984c5"),
              name = "Data",
              text = ~paste("Healthy Life Expectancy: ", round(healthy_life_expectancy, 2),
                            '<br>Happy Score: ', round(happy_score, 2))) %>%
  add_lines(x = ~healthy_life_expectancy, y = ~fitted(fit), 
            color = I("#e14b31"), 
            name = "Reg. line",
            text = ~paste("Healthy Life Expectancy: ", round(healthy_life_expectancy, 2),
                          '<br>Happy Score: ', round(happy_score, 2))) %>%
  colorbar(title = list(text = "<b>Score Lvl:</b>", 
                        font = t_labels),
           tickvals = c(3, 5, 7),
           len = 0.5,
           y = 1,
           ypad = 20,
           thickness = 15) %>%
  layout(title = list(text = "<b>Healthy Life Expectancy vs Happiness</b>", 
                      xanchor = "center", font = t_title, 
                      x = 0.5, y = 0.99),
         legend = list(title = list(text = "<b>Legend:</b>"), 
                       font = t_legend),
         xaxis = list(title = list(text = "<b>Healthy Life Expectancy</b>", 
                                   font = t_labels)),
         yaxis = list(title = list(text = "<b>Happiness Score</b>", 
                                   font = t_labels)), 
         plot_bgcolor = "#f9f9f9")


#-----------------------------------------
# 4. Freedom to make life choices vs Happiness
#-----------------------------------------

# 4.1 Getting the regression formula
fit <- lm(happy_score ~ freedom_life_choices, data = data)

# 4.2 Plot + Regression line
data %>% 
  plot_ly(x = ~freedom_life_choices) %>% 
  add_markers(y = ~happy_score, 
              color = ~happy_score,
              colors = c("#dedad2", "#54bebe"),
              name = "Data",
              text = ~paste("Freedom to make life choices: ", round(freedom_life_choices, 2),
                            '<br>Happy Score: ', round(happy_score, 2))) %>%
  add_lines(x = ~freedom_life_choices, y = ~fitted(fit), 
            color = I("#e14b31"), 
            name = "Reg. line",
            text = ~paste("Freedom to make life choices: ", round(freedom_life_choices, 2),
                          '<br>Happy Score: ', round(happy_score, 2))) %>%
  colorbar(title = list(text = "<b>Score Lvl:</b>", 
                        font = t_labels),
           tickvals = c(3, 5, 7),
           len = 0.5,
           y = 1,
           ypad = 20,
           thickness = 15) %>%
  layout(title = list(text = "<b>Freedom to make life choices vs Happiness</b>", 
                      xanchor = "center", font = t_title, 
                      x = 0.5, y = 0.99),
         legend = list(title = list(text = "<b>Legend:</b>"), 
                       font = t_legend),
         xaxis = list(title = list(text = "<b>Freedom to make life choices</b>", 
                                   font = t_labels)),
         yaxis = list(title = list(text = "<b>Happiness Score</b>", 
                                   font = t_labels)), 
         plot_bgcolor = "#f9f9f9")


#-----------------------------------------
# 5. Generosity vs Happiness
#-----------------------------------------

# 5.1 Getting the regression formula
fit <- lm(happy_score ~ generosity, data = data)

# 5.2 Plot + Regression line
data %>% 
  plot_ly(x = ~generosity) %>% 
  add_markers(y = ~happy_score, 
              color = ~happy_score,
              colors = c("#a86464", "#333333"),
              name = "Data",
              text = ~paste("Generosity: ", round(generosity, 2),
                            '<br>Happy Score: ', round(happy_score, 2))) %>%
  add_lines(x = ~generosity, y = ~fitted(fit), 
            color = I("#e14b31"), 
            name = "Reg. line",
            text = ~paste("Generosity: ", round(generosity, 2),
                          '<br>Happy Score: ', round(happy_score, 2))) %>%
  colorbar(title = list(text = "<b>Score Lvl:</b>", 
                        font = t_labels),
           tickvals = c(3, 5, 7),
           len = 0.5,
           y = 1,
           ypad = 20,
           thickness = 15) %>%
  layout(title = list(text = "<b>Generosity vs Happiness</b>", 
                      xanchor = "center", font = t_title, 
                      x = 0.5, y = 0.99),
         legend = list(title = list(text = "<b>Legend:</b>"), 
                       font = t_legend),
         xaxis = list(title = list(text = "<b>Generosity</b>", 
                                   font = t_labels)),
         yaxis = list(title = list(text = "<b>Happiness Score</b>", 
                                   font = t_labels)), 
         plot_bgcolor = "#f9f9f9")


#-----------------------------------------
# 6. Perception of corruption vs Happiness
#-----------------------------------------

# 6.1 Getting the regression formula
fit <- lm(happy_score ~ perception_corruption, data = data)

# 6.2 Plot + Regression line
data %>% 
  plot_ly(x = ~perception_corruption) %>% 
  add_markers(y = ~happy_score, 
              color = ~happy_score,
              colors = c("#363445", "#ffb400"),
              name = "Data",
              text = ~paste("Perception of corruption: ", round(perception_corruption, 2),
                            '<br>Happy Score: ', round(happy_score, 2))) %>%
  add_lines(x = ~perception_corruption, y = ~fitted(fit), 
            color = I("#e14b31"), 
            name = "Reg. line",
            text = ~paste("Perception of corruption: ", round(perception_corruption, 2),
                          '<br>Happy Score: ', round(happy_score, 2))) %>%
  colorbar(title = list(text = "<b>Score Lvl:</b>", 
                        font = t_labels),
           tickvals = c(3, 5, 7),
           len = 0.5,
           y = 1,
           ypad = 20,
           thickness = 15) %>%
  layout(title = list(text = "<b>Perception of corruption vs Happiness</b>", 
                      xanchor = "center", font = t_title, 
                      x = 0.5, y = 0.99),
         legend = list(title = list(text = "<b>Legend:</b>"), 
                       font = t_legend),
         xaxis = list(title = list(text = "<b>Perception of corruption</b>", 
                                   font = t_labels)),
         yaxis = list(title = list(text = "<b>Happiness Score</b>", 
                                   font = t_labels)), 
         plot_bgcolor = "#f9f9f9")


#-----------------------------------------
# 7. Positive affect vs Happiness
#-----------------------------------------

# 7.1 Getting the regression formula
fit <- lm(happy_score ~ positive_affect, data = data)

# 7.2 Plot + Regression line
data %>% 
  plot_ly(x = ~positive_affect) %>% 
  add_markers(y = ~happy_score, 
              color = ~happy_score,
              colors = c("#00bfff", "#0000b3"),
              name = "Data",
              text = ~paste("Positive affect: ", round(positive_affect, 2),
                            '<br>Happy Score: ', round(happy_score, 2))) %>%
  add_lines(x = ~positive_affect, y = ~fitted(fit), 
            color = I("#e14b31"), 
            name = "Reg. line",
            text = ~paste("Positive affect: ", round(positive_affect, 2),
                          '<br>Happy Score: ', round(happy_score, 2))) %>%
  colorbar(title = list(text = "<b>Score Lvl:</b>", 
                        font = t_labels),
           tickvals = c(3, 5, 7),
           len = 0.5,
           y = 1,
           ypad = 20,
           thickness = 15) %>%
  layout(title = list(text = "<b>Positive affect vs Happiness</b>", 
                      xanchor = "center", font = t_title, 
                      x = 0.5, y = 0.99),
         legend = list(title = list(text = "<b>Legend:</b>"), 
                       font = t_legend),
         xaxis = list(title = list(text = "<b>Positive affect</b>", 
                                   font = t_labels)),
         yaxis = list(title = list(text = "<b>Happiness Score</b>", 
                                   font = t_labels)), 
         plot_bgcolor = "#f9f9f9")


#-----------------------------------------
# 8. Negative affect vs Happiness
#-----------------------------------------

# 8.1 Getting the regression formula
fit <- lm(happy_score ~ negative_affect, data = data)

# 8.2 Plot + Regression line
data %>% 
  plot_ly(x = ~negative_affect) %>% 
  add_markers(y = ~happy_score, 
              color = ~happy_score,
              colors = c("#d7e1ee", "#a4a2a8"),
              name = "Data",
              text = ~paste("Negative affect: ", round(negative_affect, 2),
                            '<br>Happy Score: ', round(happy_score, 2))) %>%
  add_lines(x = ~negative_affect, y = ~fitted(fit), 
            color = I("#e14b31"), 
            name = "Reg. line",
            text = ~paste("Negative affect: ", round(negative_affect, 2),
                          '<br>Happy Score: ', round(happy_score, 2))) %>%
  colorbar(title = list(text = "<b>Score Lvl:</b>", 
                        font = t_labels),
           tickvals = c(3, 5, 7),
           len = 0.5,
           y = 1,
           ypad = 20,
           thickness = 15) %>%
  layout(title = list(text = "<b>Negative affect vs Happiness</b>", 
                      xanchor = "center", font = t_title, 
                      x = 0.5, y = 0.99),
         legend = list(title = list(text = "<b>Legend:</b>"), 
                       font = t_legend),
         xaxis = list(title = list(text = "<b>Negative affect</b>", 
                                   font = t_labels)),
         yaxis = list(title = list(text = "<b>Happiness Score</b>", 
                                   font = t_labels)), 
         plot_bgcolor = "#f9f9f9")


#-----------------------------------------
# Conclusions from the analyses: 
#-----------------------------------------
"
Positive relationship are observed with Happiness between:
- Log GDP per capita
- Healthy life expectancy
- Social support
- Freedom to make life choices
- Generosity (However, the relationship is not strong)
- Positive affect

However, Negative relationship was observed between:
- Happiness & Negative Affect
"

#------------------------------------------------------------------------------#
### Regression Analysis ###
#------------------------------------------------------------------------------#

library(modelr)

# Filtering the dataset to include only the variables that we are interested in
data_happy <- subset(data, select = c(3:11))

# Splitting the data into train and test data
split_data <- resample_partition(data_happy, c(test = 0.2, train = 0.8))

# how many cases are in test & training set?
lapply(split_data, dim)

# Target Variable: `happy_score`; initializing the fit model:
happy_model <- lm(happy_score ~ log_gdp_per_capita + healthy_life_expectancy + 
                    social_support + freedom_life_choices + generosity + 
                    perception_corruption + positive_affect + negative_affect,
                  data = split_data$train)

# Summary of the model
summary(happy_model)

# Compute correlation at 3 decimal places
corr_matrix = round(cor(data_happy, method = "pearson"), 3)

# Pairplot:
ggpairs(data_happy)

# Plotting the correlation matrix
ggcorrplot(corr_matrix, hc.order = TRUE, type = "full",
           lab = TRUE, lab_size = 3, method = "circle")

## What we found:
"
# --> happy_score is highly correlated with:
#     - log_gdp_per_capita (0.793)
#     - healthy_life_expectancy (0.755)
#     - social_support (0.713)

# --> However, it is not correlated with:
#     - generosity (-0.017)
#     - negative_affect (-0.299)

## Here we see that `negative_affect` contributes a less in the model because
## (p-value < 0.05) and hence we will remove it from the model.

"

#------------------------------------------------------------------------------#
### Model Reliability Check ###
#------------------------------------------------------------------------------#

# Getting the list of residuals
residuals <- resid(happy_model)

## Checking our model's assumptions:

# 1. Distribution of the model residuals (Normality?):
hist(residuals, main = "Histogram of Residuals", xlab = "Residuals")

# 1.1. Q-Q plot of the residuals
qqnorm(residuals)
qqline(residuals) # plotting the QQ line

"
--> Histogram follows a normal distribution.
--> Q-Q plot shows that the residuals are normally distributed as most points
    are on the straight line.
"

# 2. Multicollinearity assumption check:
corr_matrix <- round(cor(data_happy), 3)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE)

"
happy_score is highly correlated with:
- log_gdp_per_capita (0.793)
- healthy_life_expectancy (0.755)
- social_support (0.713)

However, it is NOT correlated with:
- generosity (-0.017)
- negative_affect (-0.299)
"

# 3. Homoscedasticity assumption check:
plot(happy_model, which = 1) # Residuals vs Fitted

"
The residuals are not randomly spread around the 0 line, which indicates 
that the assumption of homoscedasticity is not met.

# Overall, the model is relatively reliable as it meets the majority
assumptions of linear regression.
"

#------------------------------------------------------------------------------#
### Analysis of variance (ANOVA) test of the two models ###
#------------------------------------------------------------------------------#
"
- It tests the null hypothesis (H0), where the variables that we removed 
previously have no significance, against the alternative hypothesis (H1) 
that those variables are significant. 

- If the new model is an improvement of the original model, 
then we fail to reject H0. 

- If that is not the case, it means that those variables were significant; 
hence we reject H0. 
"

happy_model <- lm(happy_score ~ log_gdp_per_capita + healthy_life_expectancy +
                    social_support + freedom_life_choices + generosity +
                    perception_corruption + positive_affect + negative_affect,
                  data = split_data$train)


happy_model_two <- lm(happy_score ~ log_gdp_per_capita +
                        healthy_life_expectancy + social_support +
                        freedom_life_choices + perception_corruption +
                        generosity + positive_affect, data = split_data$train)

summary(happy_model_two)

# ANOVA test on the two models:
anova(happy_model, happy_model_two)

"
The p-value is 0.5664 which is signficantly higher than 0.05. 
- Therefore, we do NOT reject the null hypothesis, conclude that the variables 
  we removed previously are NOT significant.
- The second model IS an improvement of the first one. 

- The model has R-squared of 0.77, which means that 77% of the variation in
  happy_score is explained by the model.

"

#------------------------------------------------------------------------------#
### Linear Regression Analysis using the New formula ###
#------------------------------------------------------------------------------#

# Model initiation:
happy_model <- lm(happy_score ~ log_gdp_per_capita +
                    healthy_life_expectancy + social_support +
                    freedom_life_choices + perception_corruption +
                    generosity + positive_affect, data = split_data$train)

# Summary of the model:
summary(happy_model)

# Testing on the test data:
print("Making predictions for the following five values:")
print(head(data_happy))

print("The predictions are:")
print(predict(happy_model, head(data_happy)))

print("Actual score:")
print(head(data_happy$happy_score))

# Get the MAE (Mean Absolute Error) for our model:
mae(model = happy_model, data = split_data$test)
mae(model = happy_model, data = split_data$train)


#------------------------------------------------------------------------------#
### Key Observations from the Analysis ###
#------------------------------------------------------------------------------#

"
1. Taking in all the factors into consideration, the best country to live in
   is Denmark, followed by Finland and Switzerland.
2. The worst country to live in is Congo, followed by Burundi and Togo. 
3. The most important factor that contributes to the happiness score is the
   log of GDP per capita, healthy life expectancy and social support.
4. Least correlated factor towards happiness score is negativite_affect 
   (negative emotions), and generosity.
5. The model is relatively reliable as it meets the majority assumptions of 
   linear regression.
6. The model has R-squared of 0.77, which means that 77% of the variation in
   happy_score is explained by the model.
7. The model has a MAE of 0.42, which means that on average, the model's 
   prediction is off by 0.42. 
8. The model is not perfect as it does not take into account other factors 
   such as political stability, crime rate, etc. Therefore future studies
   should explore on those factors more. 
"

#------------------------------------------------------------------------------#
### Looking at the most updated data to check the Trends ###
#------------------------------------------------------------------------------#

# Reading in the data:
df_2023 <- read_csv('WHR2023.csv') # This csv has the most updated version
View(df_2023)
glimpse(df_2023)
summary(df_2023)

#---------------------------#
# Data Cleaning/Engineering:
#---------------------------#

# Removing missing values:
df_2023 <- drop_na(df_2023) # dropping missing/na values
colSums(is.na(df_2023)) # missing values observed in some columns

# Removing duplicates:
sum(duplicated(df_2023))
df_2023 <- unique(df_2023)

# Changing . to _
colnames(df_2023) <- gsub(" ", "_", colnames(df_2023), fixed = TRUE) 

# Changing to lower case
colnames(df_2023) <- tolower(colnames(df_2023)) 

# Renaming the columns:
df_2023 <- rename(df_2023,
                  country = country_name,
                  happy_score = life_ladder,
                  healthy_life_expectancy = healthy_life_expectancy_at_birth,
                  freedom_life_choices = freedom_to_make_life_choices,
                  perception_corruption = perceptions_of_corruption)

View(df_2023)

#------------------------------------------------------------------------------#
### Top 10 Happiest and Saddest Countries (2023) Analysis ###
"
We will look at three most relevant feature towards `happy_score`: 
- log_gdp_per_capita,
- healthy_life_expectancy,
- social_support
"
#------------------------------------------------------------------------------#

# 0. Sorting by Top 10 Happiest and Saddest Countries

happy_country <- c("Denmark", "Finland", "Switzerland", "Norway", "Netherlands",
                   "Iceland", "Sweden", "Canada", "New Zealand", "Australia")

sad_country <- c("Afghanistan", "South Sudan", "Central African Republic", 
                 "Burundi", "Rwanda", "Togo", "Tanzania", "Zimbabwe", 
                 "Cosmoros", "Yemen")

df_happy <- df_2023[is.element(df_2023$country, happy_country), ]

df_sad <- df_2023[is.element(df_2023$country, sad_country), ]

#---------------------------------------------

# 0.1. Setting the fonts and color codes for graph visualization

happy_colors <- c("#54bebe", "#76c8c8", "#98d1d1", "#badbdb", "#3c4e4b",
                  "#e4bcad", "#df979e", "#d7658b", "#c80064", "#a80047")

sad_colors <- c("#e27c7c", "#a86464", "#6d4b4b", "#503f3f", "#333333",
                "#3c4e4b", "#466964", "#599e94", "#6cd4c5", "#7fffe8")

t <- list(family = "sans-serif", size = 16, color = "black")
t1 <- list(family = "sans-serif", size = 13, color = "black")
t2 <- list(family = "sans-serif", size = 15, color = "black")

#---------------------------------------------
# 1. Top/Lowest 10 Happy Score
#---------------------------------------------

# Happiness Score of Top 10 Happiest Countries

fig_happy <- plot_ly(df_happy, x = ~year, y = ~happy_score, color = ~country,
                     colors = happy_colors, type = 'scatter', mode = 'lines+markers',
                     marker = list(size = 10)) %>%
  layout(title = list(text = '<b>Happiness Score of Top 10 Happiest Countries</b>',
                      font = t, xanchor = 'center', yanchor =  'top', 
                      y = 0.99, x = 0.5), font = t1,
         legend = list(title = list(text = '<b>countries:</b>', font = t1)),
         xaxis = list(title = 'Year'),
         yaxis = list(title = 'Happiness Score'),
         plot_bgcolor = "#e2e2e2")


# Happiness Score of Top 10 Saddest Countries

fig_sad <- plot_ly(df_sad, x = ~year, y = ~happy_score, color = ~country, 
                   colors = sad_colors, type = 'scatter', mode = 'lines+markers',
                   marker = list(size = 10)) %>%
  layout(title = list(text = '<b>Saddest Score of Top 10 Happiest Countries</b>',
                      font = t, xanchor = 'center', yanchor =  'top', 
                      y = 0.99, x = 0.5), font = t1,
         legend = list(title = list(text = '<b>countries:</b>', font = t1)),
         xaxis = list(title = 'Year'),
         yaxis = list(title = 'Happiness Score'),
         plot_bgcolor = "#e2e2e2")

fig_happy
fig_sad

#---------------------------------------------
# 2. Top/Lowest 10 GDP/Capita (Economy Growth) 
#---------------------------------------------

# GDP/Capita of Top 10 Happiest Countries

fig_gdp_happy <- plot_ly(df_happy, x = ~year, y = ~log_gdp_per_capita, color = ~country,
                     colors = happy_colors, type = 'scatter', mode = 'lines+markers',
                     marker = list(size = 10)) %>%
  layout(title = list(text = '<b>GDP per Capita of Top 10 Happiest Countries</b>',
                      font = t, xanchor = 'center', yanchor =  'top', 
                      y = 0.99, x = 0.5), font = t1,
         legend = list(title = list(text = '<b>countries:</b>', font = t1)),
         xaxis = list(title = 'Year'),
         yaxis = list(title = 'Log of GDP/Capita'),
         plot_bgcolor = "#e2e2e2")


# GDP/Capita of Top 10 Saddest Countries

fig_gdp_sad <- plot_ly(df_sad, x = ~year, y = ~log_gdp_per_capita, color = ~country, 
                   colors = sad_colors, type = 'scatter', mode = 'lines+markers',
                   marker = list(size = 10)) %>%
  layout(title = list(text = '<b>GDP per Capita of Top 10 Saddest Countries</b>',
                      font = t, xanchor = 'center', yanchor =  'top', 
                      y = 0.99, x = 0.5), font = t1,
         legend = list(title = list(text = '<b>countries:</b>', font = t1)),
         xaxis = list(title = 'Year'),
         yaxis = list(title = 'Log of GDP/Capita'),
         plot_bgcolor = "#e2e2e2")

fig_gdp_happy
fig_gdp_sad

#---------------------------------------------
# 3. Top/Lowest 10 Life Expectancy
#---------------------------------------------

# Life Expectancy of Top 10 Happiest Countries

fig_life_happy <- plot_ly(df_happy, x = ~year, y = ~life_expectancy, color = ~country,
                     colors = happy_colors, type = 'scatter', mode = 'lines+markers',
                     marker = list(size = 10)) %>%
  layout(title = list(text = '<b>Life Expectancy of Top 10 Happiest Countries</b>',
                      font = t, xanchor = 'center', yanchor =  'top', 
                      y = 0.99, x = 0.5), font = t1,
         legend = list(title = list(text = '<b>countries:</b>', font = t1)),
         xaxis = list(title = 'Year'),
         yaxis = list(title = 'Life Expectancy'),
         plot_bgcolor = "#e2e2e2")


# Life Expectancy of Top 10 Saddest Countries

fig_life_sad <- plot_ly(df_sad, x = ~year, y = ~life_expectancy, color = ~country, 
                   colors = sad_colors, type = 'scatter', mode = 'lines+markers',
                   marker = list(size = 10)) %>%
  layout(title = list(text = '<b>Life Expectancy of Top 10 Saddest Countries</b>',
                      font = t, xanchor = 'center', yanchor =  'top', 
                      y = 0.99, x = 0.5), font = t1,
         legend = list(title = list(text = '<b>countries:</b>', font = t1)),
         xaxis = list(title = 'Year'),
         yaxis = list(title = 'Life Expectancy'),
         plot_bgcolor = "#e2e2e2")

fig_life_happy
fig_life_sad

#---------------------------------------------
# 4. Top/Lowest 10 Social Support
#---------------------------------------------

# Social Support of Top 10 Happiest Countries

fig_social_happy <- plot_ly(df_happy, x = ~year, y = ~social_support, color = ~country,
                     colors = happy_colors, type = 'scatter', mode = 'lines+markers',
                     marker = list(size = 10)) %>%
  layout(title = list(text = '<b>Social Support of Top 10 Happiest Countries</b>',
                      font = t, xanchor = 'center', yanchor =  'top', 
                      y = 0.99, x = 0.5), font = t1,
         legend = list(title = list(text = '<b>countries:</b>', font = t1)),
         xaxis = list(title = 'Year'),
         yaxis = list(title = 'Social Support'),
         plot_bgcolor = "#e2e2e2")


# Social Support of Top 10 Saddest Countries

fig_social_sad <- plot_ly(df_sad, x = ~year, y = ~social_support, color = ~country, 
                   colors = sad_colors, type = 'scatter', mode = 'lines+markers',
                   marker = list(size = 10)) %>%
  layout(title = list(text = '<b>Social Support of Top 10 Saddest Countries</b>',
                      font = t, xanchor = 'center', yanchor =  'top', 
                      y = 0.99, x = 0.5), font = t1,
         legend = list(title = list(text = '<b>countries:</b>', font = t1)),
         xaxis = list(title = 'Year'),
         yaxis = list(title = 'Social Support'),
         plot_bgcolor = "#e2e2e2")

fig_social_happy
fig_social_sad



#------------------------------------------------------------------------------#
### Appendix Code ###
#------------------------------------------------------------------------------#

## Converting Spreadsheet Files to CSV (using rio package):
"
library(rio) # converting xls -> csv
# xls <- dir(pattern = 'xls')
# created <- mapply(convert, xls, gsub('xls', 'csv', xls))
# unlink(xls) # Delete xls files
"

## Changing the column names:
"
# names(data) = gsub(' ', '_', names(data))
# head(data)
"

## ggpubr PACKAGE FOR DOING SCATTERPLOT WITH CORRELATION COEFFICIENT
"
# ggscatter(data, x = 'log_gdp_per_capita', y = 'life_ladder'
#           add = 'reg.line', conf.int = FALSE,
#           cor.coef = TRUE, cor.method = 'pearson',
#           xlab = 'Log GDP per capita',
#           ylab = 'Life ladder',
#           main = 'Log GDP per capita vs Life ladder')
"