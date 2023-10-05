# Install the packages
install.packages('readr')
install.packages('tidyr')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('moments')

# Load the Libraries
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(moments)

# Load the Sales data
raw_sales<- read.csv(file.choose(), header = TRUE)

# Preview the sales data
head(raw_sales)

# Check for any null values in the dataframe
null_count <- colSums(is.na(raw_sales))
null_count

# Filter out any null values
sales <- na.omit(raw_sales)
head(sales)

# Check Data Types
sales_types <- sapply(sales, class)
sales_types

# List the number of unique platforms
number_platforms <- unique(sales$Platform)
number_platforms

# Number of products per platform
platform_freq <- table(sales$Platform)
platform_freq

# List the number of unique genres
number_genres <- unique(sales$Genre)
number_genres

# Number of products per genre
genre_freq <- table(sales$Genre)
genre_freq

# List the number of Publishers
number_publishers <- unique(sales$Publisher)
number_publishers

# Number of products per publisher
publisher_freq <- table(sales$Publisher)
publisher_freq

# Create New DataFrame with just Sales Data
sales_data <- data.frame(sales$Ranking, sales$Product, sales$NA_Sales, sales$EU_Sales, sales$Global_Sales)
head(sales_data)

# Summary of the Sales Data
summary(sales_data)

# Histogram showing Distribution of North America Sales
qplot(x = sales.NA_Sales, data = sales_data, geom = 'histogram', bins = 10, 
      xlab = 'North America Sales (£, millions)', ylab = 'Frequency', main = 'North America Sales Distribution'
)

# Histogram showing Distribution of European Sales
qplot(x = sales.EU_Sales, data = sales_data, geom = 'histogram', bins = 10, 
      xlab = 'European Sales (£, millions)', ylab = 'Frequency', main = 'European Sales Distribution'
)

# Histogram showing Distribution of Global Sales
qplot(x = sales.Global_Sales, data = sales_data, geom = 'histogram', bins = 20, 
      xlab = 'Global Sales (£, millions)', ylab = 'Frequency', main = 'Global Sales Distribution'
)

# Q-Q Plot of North American Sales
na_sales <- sales_data$'sales.NA_Sales'
qqnorm(na_sales, main = "Q-Q Plot of North American Sales")
qqline(na_sales, col = 2)

# Shapiro Tests for Normality of North American Sales
na_shapiro_test <- shapiro.test(na_sales)
na_shapiro_test

# Kurtosis and Skewness of North American Sales
na_kurtosis_score <- moments::kurtosis(na_sales)
na_skewness_score <- moments::skewness(na_sales)
cat("Kurtosis:", na_kurtosis_score, "\n")
cat("Skewness:", na_skewness_score, "\n")

# Q-Q Plot of European Sales
eu_sales <- sales_data$'sales.EU_Sales'
qqnorm(eu_sales, main = "Q-Q Plot of European Sales")
qqline(eu_sales, col = 2)

# Shapiro Tests for Normality of European Sales
eu_shapiro_test <- shapiro.test(eu_sales)
eu_shapiro_test

# Kurtosis and Skewness of European Sales
eu_kurtosis_score <- moments::kurtosis(eu_sales)
eu_skewness_score <- moments::skewness(eu_sales)
cat("Kurtosis:", eu_kurtosis_score, "\n")
cat("Skewness:", eu_skewness_score, "\n")

# Q-Q Plot of Global Sales
global_sales <- sales_data$'sales.Global_Sales'
qqnorm(global_sales, main = "Q-Q Plot of Global Sales")
qqline(global_sales, col = 2)

# Shapiro Tests for Normality of Global Sales
global_shapiro_test <- shapiro.test(global_sales)
global_shapiro_test
 
# Kurtosis and Skewness of Global Sales
global_kurtosis_score <- moments::kurtosis(global_sales)
global_skewness_score <- moments::skewness(global_sales)
cat("Kurtosis:", global_kurtosis_score, "\n")
cat("Skewness:", global_skewness_score, "\n")

# Plotting the relationship between North American, European and Global Sales
ggplot(sales_data, aes(x = sales.NA_Sales, y = sales.Global_Sales)) + geom_point() + ggtitle("North American vs Global Sales")
ggplot(sales_data, aes(x = sales.EU_Sales, y = sales.Global_Sales)) + geom_point() + ggtitle("European vs Global Sales")
ggplot(sales_data, aes(x = sales.NA_Sales, y = sales.EU_Sales)) + geom_point() + ggtitle("North American vs European Sales")


# Linear Regression Model
lm_model <- lm(formula = sales.Global_Sales ~ sales.NA_Sales + sales.EU_Sales, data = sales_data)
summary(lm_model)

# Future Sales Predictions
prediction_data <- data.frame(sales.NA_Sales = c(34.02, 3.93, 2.73, 2.26, 22.08), sales.EU_Sales = c(23.8, 1.56, 0.65, 0.97, 0.52))
predictions <- predict(lm_model, newdata = prediction_data)
prediction_data$predicted_sales <- predictions
prediction_data