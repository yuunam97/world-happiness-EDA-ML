# :earth_africa::grin: WORLD HAPPINESS ANALYSIS :grin::earth_africa:

## :bar_chart::mag: EDA & Regression in R :mag::bar_chart:

![Banner](https://github.com/yuunam97/world-happiness-EDA-ML/blob/main/images/happiness-banner.png?raw=true)

### Description: 
This analysis looked at the number of factors affecting the happiness score in the world. EDA was conducted, followed by a regression analysis on predicting the target variable, `happy_score`. The dataset utilized for this analysis was from [World Happiness Report](https://worldhappiness.report/data/) dataset. I used the data from year 2021, and 2023.

### Tools used for EDA and Regression analysis:
- tidyverse   (1.3.0)
- lubridate   (1.9.3)
- janitor     (2.2.0)
- dplyr       (1.1.3)
- ggplot2     (3.4.4)
- ggcorrplot  (0.1.4.1)
- plotly      (4.10.3)
- modelr      (0.1.11)

### The goal of this project:
1. Explore the World Happiness Data
2. Which country is the happiest? 
3. Identify which factors contributes to the most towards happiness score.
4. What are the correlation between the features and the target variable `happy_score`?
5. Regression analysis on the predicting the happy score.  

### What I've learned:
1. EDA using R.
2. Data Visualization using ggplot2, and plotly to produce graphs like archarts, linecharts, etc.
3. Regression Analysis to make prediction on the continous variables based on other features. 
4. Statistical analysis using ANOVA to determine which models performs the best. 


5. Denmark, Finland, and Switzerland emerge as the top three countries for a high quality of life based on happiness scores.
![happiness](https://github.com/yuunam97/world-happiness-EDA-ML/blob/main/images/1-happiest-countries.png?raw=true)

6. The SADDEST countries are Central African Republic (Congo), Burundi, and Togo. 
![saddest](https://github.com/yuunam97/world-happiness-EDA-ML/blob/main/images/2-saddest-countries.png?raw=true)

6. The most influential factors contributing to happiness scores are the log of GDP per capita, healthy life expectancy, and social support.
| GDP per Capita  | Social Support | Healthy Life Expectancy |
| ------------- | ------------- | ------------- |
| ![gdp](https://github.com/yuunam97/world-happiness-EDA-ML/blob/main/images/3-gdp-capita-vs-happiness.png?raw=true) | ![social](https://github.com/yuunam97/world-happiness-EDA-ML/blob/main/images/4-social-support-vs-happiness.png?raw=true)  | ![healthy](https://github.com/yuunam97/world-happiness-EDA-ML/blob/main/images/5-healthy-vs-happiness.png?raw=true)  |

![correlation](https://github.com/yuunam97/world-happiness-EDA-ML/blob/main/images/6-correlation-plot.png?raw=true)

7. The linear regression model, though not perfect, demonstrates reliability with a 77% R-squared value, indicating that 77% of the variation in happiness scores is explained. The Mean Absolute Error (MAE) is 0.42, suggesting a reasonably accurate prediction on average.

8. The model lacks consideration for factors like political stability and crime rates, indicating the need for future studies to explore a more comprehensive set of variables for a holistic understanding of happiness determinants.

### South Africa vs World in Happiness:
Embarking on data analytics projects with the World Happiness Report dataset is a profound journey for me. It's a personal quest to understand global well-being factors. I am particularly drawn to comparing South Africa and South Korea, my home countries, using data analytics. This endeavor goes beyond academic interest; it's a heartfelt effort to uncover societal nuances and contribute meaningfully to the well-being of both nations.

- Happiness:
![world-happiness](https://github.com/yuunam97/world-happiness-EDA-ML/blob/main/images/7-sa-vs-world.png?raw=true)

- GDP-per-Capita (Economy)
![world-gdp](https://github.com/yuunam97/world-happiness-EDA-ML/blob/main/images/8-sa-vs-world-gdp.png?raw=true)

- Social Support
![world-social](https://github.com/yuunam97/world-happiness-EDA-ML/blob/main/images/9-sa-vs-world-social.png?raw=true)

- Healthy Life Expectancy
![world-healthy](https://github.com/yuunam97/world-happiness-EDA-ML/blob/main/images/10-sa-vs-world-health.png?raw=true)

### Credits
1. [World Happiness Report](https://worldhappiness.report/data/) dataset of 2021, and 2023. 
2. [Grace Leeswadtrakul's EDA](https://graceleeswadtrakul.medium.com/exploratory-data-analysis-eda-project-world-happiness-report-de37eaef6951) on World Happiness Report  
3. [Plotly guide for better visualization](https://www.kaggle.com/code/desalegngeb/plotly-guide-customize-for-better-visualizations)
