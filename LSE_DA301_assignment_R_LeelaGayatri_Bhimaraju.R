## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
## performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R--------

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 4. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data------

# Install and import Tidyverse.
library('tidyverse')


# Import the data set turtle_sales.csv
sales <- read.csv(file.choose(), header=T)


# Print the data frame.
head(sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales1 <- select(sales, -Ranking, -Year, -Genre, -Publisher)

# View the data frame.
head(sales1)

# View the descriptive statistics.
summary(sales1)

################################################################################

# 2. Review plots to determine insights into the data set.-----------

## 2a) Scatterplots
# Create scatterplots.
qplot(NA_Sales, EU_Sales, data=sales1, xlab = "NA Sales (Million £)",
      ylab = "Europe Sales (Million £) ", 
      main = "North America Sales vs Europe Sales")

# Save scatter plot
ggsave("NA_Europe_scatter.png")

qplot(NA_Sales, Global_Sales, data=sales1, xlab = "NA Sales (Million £)",
      ylab = "Global Sales (Million £) ", 
      main = "North America Sales vs Global Sales")

# Save scatter plot
ggsave("NA_Global_scatter.png")


qplot(EU_Sales, Global_Sales, data=sales1, xlab = "EU Sales (Million £)",
      ylab = "Global Sales (Million £) ", 
      main = "Europe Sales vs Global Sales")

# Save scatter plot
ggsave("Europe_Global_scatter.png")

## 2b) Histograms
# Create histograms.

qplot(NA_Sales, bins=20, data=sales1, main = "North America Sales")
ggsave("NA_sales.png")
qplot(EU_Sales, bins=20, data=sales1, main = "Europe Sales")
qplot(Global_Sales, bins=20, data=sales1, main = "Global Sales")

## 2c) Boxplots
# Create boxplots.
qplot(NA_Sales, data=sales1, geom = 'boxplot', main = "North America Sales")
qplot(EU_Sales, data=sales1, geom = 'boxplot', main = "Europe Sales")
qplot(Global_Sales, data=sales1, geom = 'boxplot', main = "Global Sales")


###############################################################################

# 3. Determine the impact on sales per product_id.----------

## 3a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.

sales1_agg <- sales1 %>% group_by(Product) %>% 
  summarise(NA_Sales_sum=sum(NA_Sales),
            EU_Sales_sum=sum(EU_Sales),
            Global_Sales_sum=sum(Global_Sales))

# View the data frame.
head(sales1_agg)

# Explore the data frame.
summary(sales1_agg)


## 3b) Determine which plot is the best to compare game sales.
# Create scatterplots.
qplot(NA_Sales_sum, EU_Sales_sum, data=sales1_agg, 
      xlab = "NA Sales (Million £)",
      ylab = "Europe Sales (Million £) ", 
      main = "North America Sales vs Europe Sales")

qplot(NA_Sales_sum, Global_Sales_sum, data=sales1_agg, 
      xlab = "NA Sales (Million £)",
      ylab = "Global Sales (Million £) ", 
      main = "North America Sales vs Global Sales")

qplot(EU_Sales_sum, Global_Sales_sum, data=sales1_agg, 
      xlab = "EU Sales (Million £)",
      ylab = "Global Sales (Million £) ", 
      main = "Europe Sales vs Global Sales")

# Create histograms.
qplot(NA_Sales_sum, bins=20, data=sales1_agg, main = "North America Sales", 
      xlab="NA Sales")
ggsave("NA_Sales_agg.png")
qplot(EU_Sales_sum, bins=20, data=sales1_agg, main = "Europe Sales")
qplot(Global_Sales_sum, bins=20, data=sales1_agg, main = "Global Sales")


# Create boxplots.
qplot(NA_Sales_sum, data=sales1_agg, geom = 'boxplot',
      main = "North America Sales")
qplot(EU_Sales_sum, data=sales1_agg, geom = 'boxplot', main = "Europe Sales")
qplot(Global_Sales_sum, data=sales1_agg, geom = 'boxplot',
      main = "Global Sales")


###############################################################################

# 4. Observations and insights---------------

## Your observations and insights here ......

## The scatter plots show there is a correlation between North America Sales 
## and Global Sales as well Europe Sales and Global Sales. The box plots and 
## scatter plots show there are outliers. Since this is sales this analysis is 
## made without removing outliers. However, it will be good to check the data
## in detail to see if the outliers can be removed for the analysis.
## The histograms before grouping show that the data is not normally distributed. 
## Viewing the data by grouping by product yielded histograms which showed 
## slightly better distribution of the data


###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R-----------

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 3. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 4. Include your insights and observations.

################################################################################

# 1. Load and explore the data------------

# View data frame created in Week 4.
head(sales1_agg)

# Check output: Determine the min, max, and mean values.
min(sales1_agg$NA_Sales_sum)
min(sales1_agg$EU_Sales_sum)
min(sales1_agg$Global_Sales_sum)

max(sales1_agg$NA_Sales_sum)
max(sales1_agg$EU_Sales_sum)
max(sales1_agg$Global_Sales_sum)

mean(sales1_agg$NA_Sales_sum)
mean(sales1_agg$EU_Sales_sum)
mean(sales1_agg$Global_Sales_sum)

# View the descriptive statistics.
summary(sales1_agg)

###############################################################################

# 2. Determine the normality of the data set.----------

## 2a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(sales1_agg$NA_Sales_sum)
qqline(sales1_agg$NA_Sales_sum)

qqnorm(sales1_agg$EU_Sales_sum)
qqline((sales1_agg$EU_Sales_sum))

qqnorm(sales1_agg$Global_Sales_sum)
qqline(sales1_agg$Global_Sales_sum)
ggsave("qq_global.png")


## 2b) Perform Shapiro-Wilk test
# Install and import Moments.
# install.packages("moments")
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test(sales1_agg$NA_Sales_sum)

shapiro.test(sales1_agg$EU_Sales_sum)

shapiro.test(sales1_agg$Global_Sales_sum)


## 2c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.

skewness(sales1_agg$NA_Sales_sum) 
skewness(sales1_agg$EU_Sales_sum) 
skewness(sales1_agg$Global_Sales_sum) 

kurtosis(sales1_agg$NA_Sales_sum)
kurtosis(sales1_agg$EU_Sales_sum)
kurtosis(sales1_agg$Global_Sales_sum)


## 2d) Determine correlation
# Determine correlation.
cor(sales1_agg$NA_Sales_sum, sales1_agg$EU_Sales_sum)
cor(sales1_agg$NA_Sales_sum, sales1_agg$Global_Sales_sum)
cor(sales1_agg$EU_Sales_sum, sales1_agg$Global_Sales_sum)


###############################################################################

# 3. Plot the data--------------
# Create plots to gain insights into data.
ggplot(sales1_agg, aes(x=NA_Sales_sum, y=Global_Sales_sum)) + 
  # Note: this object’s geom layer function.
  geom_point() +
  geom_smooth(se=FALSE, color='red', method=lm) +
  labs(title="Relationship between North America Sales and Global Sales ",
       y="Global Sales (Million £)",
       x="North America Sales (Million £)")
# Save graph
ggsave("NA_Global_lm.png")

ggplot(sales1_agg, aes(y= Global_Sales_sum, x= EU_Sales_sum)) + 
  # Note: this object’s geom layer function.
  geom_point() +
  geom_smooth(se=FALSE, color='red', method=lm) +
  labs(title="Relationship between Europe Sales and Global Sales",
       y="Global Sales (Million £)",
       x="Europe Sales (Million £)")

# Save graph
ggsave("EU_Global_lm.png")

ggplot(sales1_agg, aes(x=NA_Sales_sum, y=EU_Sales_sum )) + 
  # Note: this object’s geom layer function.
  geom_point() +
  geom_smooth(se=FALSE, color='red',method=lm) +
  labs(title="Relationship between North America Sales and Europe  Sales",
       x="North America Sales (Million £)",
       y="Europe Sales (Million £)")

# Save graph
ggsave("NA_EU_lm.png")

###############################################################################

# 4. Observations and insights---------------
# Your observations and insights here...

## The Q-Q plots show that the data points are not all close to the line.
## The Shapiro wilk test shows value lesser than 0.05
## Skewness and kurtosis values are greater than 1 and 3
## Considering the above values and plots we can say that the data is not 
## normally distributed and highly skewed
## Checking the correlation values , we can see that there is a strong 
## correlation between both North America sales and Europe sales to the 
## Global sales

###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R-----

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data------
# View data frame created in Week 5.
as_tibble(sales1_agg)

# Determine a summary of the data frame.
summary(sales1_agg)

###############################################################################

# 2. Create a simple linear regression model-----------

## 2a) Determine the correlation between columns

sales1_cor1 <- select(sales1_agg, NA_Sales_sum, EU_Sales_sum)
cor(sales1_cor1)

sales1_cor2 <- select(sales1_agg, Global_Sales_sum, NA_Sales_sum)
cor(sales1_cor2)

sales1_cor3 <- select(sales1_agg, Global_Sales_sum, EU_Sales_sum)
cor(sales1_cor3)

# Basic visualisation.
ggplot(sales1_cor1, aes(NA_Sales_sum, EU_Sales_sum)) +
  geom_point()

ggplot(sales1_cor2, aes(Global_Sales_sum, NA_Sales_sum)) +
  geom_point()

ggplot(sales1_cor3, aes(Global_Sales_sum, EU_Sales_sum)) +
  geom_point()

## 2b) Create a plot (simple linear regression)----

## Model with North America Sales-----

# Create a linear regression model on the original data.
slr_model1 <- lm(Global_Sales_sum~NA_Sales_sum, data=sales1_agg)

# View full regression table.
summary(slr_model1)

# Plor the relationship between NA sales and Global sales
plot(sales1_agg$NA_Sales_sum, sales1_agg$Global_Sales_sum)

# Add a line-of-best fit to existing plot.
abline(coefficients(slr_model1))

## Model with Europe  Sales-----

# Create a linear regression model on the original data.
slr_model2 <- lm(Global_Sales_sum~EU_Sales_sum, data=sales1_agg)

# View full regression table.
summary(slr_model2)

# Plor the relationship between Europe sales and Global sales
plot(sales1_agg$EU_Sales_sum, sales1_agg$Global_Sales_sum)

# Add a line-of-best fit to existing plot.
abline(coefficients(slr_model2))


###############################################################################

# 3. Create a multiple linear regression model on 
##   original data before aggregation-------

# View original data object
head(sales1)

# Create Multiple linear regression model
mlr_model <- lm(Global_Sales ~ NA_Sales + EU_Sales, data = sales1)

# View regression table
summary(mlr_model)

###############################################################################

# 4. Predictions based on given values----------
# Compare with observed values for a number of records.

# Assign the values to be predicted 
NA_Sales <- c(34.02, 3.93, 2.73, 2.26, 22.08)
EU_Sales <- c(23.80, 1.56, 0.65, 0.97, 0.52)

# Add the values for independent variable to new data object
sales_predict <- data.frame(NA_Sales, EU_Sales)

# Predict values for Global sales with given values for 
## independent variable (NA_Sales and EU_Sales)
sales_predict$Global_Sales <- predict(mlr_model, newdata = sales_predict)

# View object
sales_predict

# Compare the predicted values of Global_Sales with original 
## values of Global_Sales
# Filter the original data frame with the given values
filter(sales1,
       NA_Sales == 34.02 | NA_Sales== 3.93 | NA_Sales == 2.73 
       | NA_Sales == 2.26 | NA_Sales == 22.08)


###############################################################################

# 5. Observations and insights-----------
# Your observations and insights here...

## Simple linear regression model is fit to the aggregated data set and the line
## of best fit is viewed with each of the independent variables, i.e, one with
## North America Sales vs Global sales and other with Europe sales vs
## Global sales.
## Multiple linear regression model is fit with both the independent variables 
## North America sales and Europe sales and dependent variable being
## Global sales. The model is fit on the original data set before it was 
## aggregated to be able to predict values and compare them to the original
## values.
## NA_Sales EU_Sales Global_Sales(pred) Global_sales(actual)
## 1    34.02    23.80    71.468572      67.85
## 2     3.93     1.56     6.856083      6.04
## 3     2.73     0.65     4.248367      4.32
## 4     2.26     0.97     4.134744      3.53
## 5    22.08     0.52    26.431567      23.21

## Looking at the predicted and actual values of global sales , it can be seen
## that the values are slightly closer and the model is slightly good  fit. 

###############################################################################
###############################################################################




