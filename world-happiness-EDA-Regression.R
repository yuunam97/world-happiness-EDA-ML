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
library(viridis)
library(hrbrthemes)
library(rio) # converting xls -> csv
library(GGally) # correlation plot
library(ggpmisc)
library(ggpubr)
library(ggcorrplot)
library(plotly)

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

# 1. Countries with the highest Happy Score:
df_happiest <- data %>% 
  group_by(country) %>% 
  summarise(happy_score = mean(happy_score)) %>% 
  arrange(desc(happy_score)) %>% 
  mutate(happiness_rank = as.integer(rank(-happy_score)))

df_happiest_graph <- df_happiest %>%
  arrange(-happy_score) %>% 
  slice(1:10) %>% 
  ggplot(df_happiest_graph, mapping = aes(x=reorder(x = country, +happy_score),
                                          y = happy_score)) + 
  geom_col(fill = "#3db5ff") + coord_flip() +
  labs(title = "Top 10 Happiest Countries", 
       x = "Country", y = "Happiness Score")

df_happiest_graph
View(df_happiest)

# 1.1 Top 10 Saddest country:

df_saddest <- df_2023 %>% 
  group_by(country) %>%
  summarise(happy_score = mean(happy_score)) %>%
  arrange((happy_score)) %>% 
  mutate(happiness_rank = as.integer(rank(-happy_score)))

df_saddest_graph <- df_saddest %>%
  arrange(happy_score) %>% 
  slice(1:10) %>% 
  ggplot(df_saddest_graph, mapping = aes(x = reorder(x = country, -happy_score), 
                                         y = happy_score)) + 
  geom_col(fill = "#3db5ff") + coord_flip() +
  labs(title = "Top 10 Saddest Countries", 
       x = "Country", y = "Happiness Score")

df_saddest_graph
View(df_saddest)

#---------------------------------------------

# 2. Log GDP per capita per country:
log_gdp_per_capita <- data %>% 
  group_by(country) %>% 
  summarise(log_gdp_per_capita = mean(log_gdp_per_capita)) %>% 
  arrange(country)

log_gdp_per_capita_graph <- log_gdp_per_capita %>%
  arrange(-log_gdp_per_capita) %>% 
  slice(1:10) %>% 
  ggplot(log_gdp_per_capita_graph, mapping=aes(x = reorder(x = country, 
                                                           +log_gdp_per_capita),
                                               y = log_gdp_per_capita)) + 
  geom_col(fill = "orange") + coord_flip() +
  labs(title = "Top 10 Countries Log GDP per capita", 
       x = "Country", y = "Log GDP per capita value") +
  scale_fill_brewer(palette = "Spectral")

log_gdp_per_capita_graph
View(log_gdp_per_capita)

#---------------------------------------------

# 3. Social support per country:
social_support <- data %>% 
  group_by(country) %>% 
  summarise(social_support = mean(social_support)) %>% 
  arrange(country)

social_support_graph <- social_support %>%
  arrange(-social_support) %>% 
  slice(1:10) %>% 
  ggplot(social_support_graph, mapping = aes(x = reorder(x = country, 
                                                         +social_support), 
                                             y = social_support)) + 
  geom_col(fill = "green") + coord_flip() +
  labs(title = "Top 10 Countries Social Support", 
       x = "Country", y = "Social Support value")

social_support_graph
View(social_support)

#---------------------------------------------

# 4. Healthy life expectancy per country:
healthy_life_expectancy <- data %>% 
  group_by(country) %>% 
  summarise(healthy_life_expectancy = mean(healthy_life_expectancy)) %>% 
  arrange(country)

healthy_life_expectancy_graph <- healthy_life_expectancy %>%
  arrange(-healthy_life_expectancy) %>% 
  slice(1:10) %>% 
  ggplot(healthy_life_expectancy_graph, mapping = aes(x = reorder(x = country, +healthy_life_expectancy),
                                                      y = healthy_life_expectancy)) + 
  geom_col(fill = "red") + coord_flip() +
  labs(title = "Top 10 Countries Healthy Life Expectancy", 
       x = "Country", y = "Healthy Life Expectancy value")

healthy_life_expectancy_graph
View(healthy_life_expectancy)

#---------------------------------------------

# 5. Freedom to make life choices per country:
freedom_life_choices <- data %>% 
  group_by(country) %>% 
  summarise(freedom_life_choices = mean(freedom_life_choices)) %>% 
  arrange(country)

freedom_life_choices_graph <- freedom_life_choices %>%
  arrange(-freedom_life_choices) %>% 
  slice(1:10) %>% 
  ggplot(freedom_life_choices_graph, mapping = aes(x = reorder(x = country, +freedom_life_choices), y = freedom_life_choices)) + 
  geom_col(fill = "purple") + coord_flip() +
  labs(title = "Top 10 Countries Freedom to make life choices", 
       x = "Country", y = "Freedom to make life choices value")

freedom_life_choices_graph
View(freedom_life_choices)


#---------------------------------------------

# 6. Generosity per country:
generosity <- data %>% 
  group_by(country) %>% 
  summarise(generosity = mean(generosity)) %>% 
  arrange(country)

generosity_graph <- generosity %>%
  arrange(-generosity) %>% 
  slice(1:10) %>% 
  ggplot(generosity_graph, mapping = aes(x = reorder(x = country, +generosity), y = generosity)) + 
  geom_col(fill = "yellow") + coord_flip() +
  labs(title = "Top 10 Countries Generosity", 
       x = "Country", y = "Generosity value")

generosity_graph
View(generosity)

#---------------------------------------------

# 7. Perceptions of corruption per country:

perception_corruption <- data %>% 
  group_by(country) %>% 
  summarise(perception_corruption = mean(perception_corruption)) %>% 
  arrange(country)

perception_corruption_graph <- perception_corruption %>%
  arrange(-perception_corruption) %>% 
  slice(1:10) %>% 
  ggplot(perception_corruption_graph, mapping = aes(x = reorder(x = country, +perception_corruption), y = perception_corruption)) + 
  geom_col(fill = "brown") + coord_flip() +
  labs(title = "Top 10 Countries Perceptions of corruption", 
       x = "Country", y = "Perceptions of corruption value")

perception_corruption_graph
View(perception_corruption)

#---------------------------------------------

# 8. Positive affect per country:
positive_affect <- data %>% 
  group_by(country) %>% 
  summarise(positive_affect = mean(positive_affect)) %>% 
  arrange(country)

positive_affect_graph <- positive_affect %>%
  arrange(-positive_affect) %>% 
  slice(1:10) %>% 
  ggplot(positive_affect_graph, mapping = aes(x = reorder(x = country, +positive_affect), y = positive_affect)) + 
  geom_col(fill = "pink") + coord_flip() +
  labs(title = "Top 10 Countries Positivity", 
       x = "Country", y = "Positive affect value")

positive_affect_graph
View(positive_affect)

#---------------------------------------------

# 9. Negative affect per country:
negative_affect <- data %>% 
  group_by(country) %>% 
  summarise(negative_affect = mean(negative_affect)) %>% 
  arrange(country)

negative_affect_graph <- negative_affect %>%
  arrange(-negative_affect) %>% 
  slice(1:10) %>% 
  ggplot(negative_affect_graph, mapping = aes(x = reorder(x = country, +negative_affect), y = negative_affect)) + 
  geom_col(fill = "blue") + coord_flip() +
  labs(title = "Top 10 Countries Negativity", 
       x = "Country", y = "Negative affect value")

negative_affect_graph
View(negative_affect)

#------------------------------------------------------------------------------#
### Bivariate Analysis (between Target vs FEATURES) ###
#------------------------------------------------------------------------------#

cols <- c("log_gdp_per_capita", "social_support", "healthy_life_expectancy",
          "freedom_life_choices", "generosity",
          "perception_corruption", "positive_affect",
          "negative_affect")

for (i in cols){
    ggplot(data, aes(x = i, y = happy_score)) +
    geom_point(size = 2, shape = 18, color = "#0099f9") +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    stat_cor(p.accuracy = 0.001, r.accuracy = 0.001) +
    labs(title = "Happy Score vs Factors",
         subtitle = "",
         caption = "Source: World Happiness Report 2021",
         y = "Healthy life expectancy") +
    theme(plot.title = element_text(color = "#0099f9", face = "bold",
                                    size = 14, hjust = 0.5),
          legend.position = "none")
}

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