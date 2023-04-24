# Install packages.
install.packages("corrplot")
install.packages("ggpubr")
install.packages("forecast")
install.packages("tseries")
install.packages('psych')


# Import libraries.
library(tidyverse)
library(skimr)
library(readr)
library(ggplot2)
library(corrplot)
library(ggpubr)
library(moments)
library(forecast)
library(tseries)
library(psych)
library(stats)



# Set up library and directory.
getwd()


# Load the data.
turtle <- read.csv("turtle_sales.csv")

View(turtle)

glimpse(turtle)


# How many records are in this data set?
dim(turtle)

# Notes:
# There are 352 records in this data set. There are 9 attributes.

# How many platforms?
turtle %>%
  group_by(Platform) %>%
  summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>%
  arrange(desc(Count)) %>%
  print(n=30)

# Notes:
# There are 22 unique platforms.
# X360 is the platform with the the most number of products at 13.4%.

# How many genres?
turtle %>%
  group_by(Genre) %>%
  summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>%
  arrange(desc(Count)) %>%
  print(n=30)

# Notes:
# There are 12 different genres, Genre "Shooter" accounts for most number
# of products, followed closely by "Action".

# How many publishers?
turtle %>%
  group_by(Publisher) %>%
  summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>%
  arrange(desc(Count)) %>%
  print(n=40)

# Notes:
# There are 24 publishers, "Nintendo" released the most number of products
# at 25% or 88 products.

# Find out if there are any null values for the North America Sales data.
turtle %>% summarise(is_NULL=sum(is.na(NA_Sales)==1),
                      is_NOT_NULL=sum(!is.na(NA_Sales)==1))
summary(turtle$NA_Sales)

# Notes: 
# No null values found however the minimum value is 0.0000. 

# Find out if there are any null values for the EU Sales data.
turtle %>% summarise(is_NULL=sum(is.na(EU_Sales)==1),
                      is_NOT_NULL=sum(!is.na(EU_Sales)==1))
summary(turtle$EU_Sales)

# Notes: 
# No null values found however the minimum value is also 0.0000. 

# Find out if there are any null values for the Global Sales data.
turtle %>% summarise(is_NULL=sum(is.na(Global_Sales)==1),
                      is_NOT_NULL=sum(!is.na(Global_Sales)==1))
summary(turtle$Global_Sales)

# Notes: 
# No null values found however the minimum value is 0.0000. 


##############################################################

# Top 10 Platforms by Global Sales


platform <- turtle %>%
  group_by(Platform) %>%
  summarise(Global_Sum = sum(Global_Sales)) %>%
  top_n(n = 10, wt = Global_Sum)

platform_chart <- ggplot(platform,
                         aes(x = Platform, y = Global_Sum, fill=Global_Sum)) + 
  geom_col(fill="darkgreen")+
  labs(title = "Top 10 Platforms by Global Sales", x="Platform",y="Total")+ coord_flip()

platform_chart


# Top 10 Publishers by Global Sales
publisher <- turtle %>%
  group_by(Publisher) %>%
  summarise(Global_Sum = sum(Global_Sales)) %>%
  top_n(n = 10, wt = Global_Sum)

publisher_chart <- ggplot(publisher,
                         aes(x = Publisher, y = Global_Sum, fill=Global_Sum)) + 
  geom_col(fill="blue")+
  labs(title = "Top 10 Publishers by Global Sales", x="Publisher",y="Total")+ coord_flip()

publisher_chart


# Top 5 Genres by Global Sales
genre <- turtle %>%
  group_by(Genre) %>%
  summarise(Global_Sum = sum(Global_Sales)) %>%
  top_n(n = 10, wt = Global_Sum)

genre_chart <- ggplot(genre,
                          aes(x = Genre, y = Global_Sum, fill=Global_Sum)) + 
  geom_col(fill="red")+
  labs(title = "Top 10 Genre by Global Sales", x="Genre",y="Total")+ coord_flip()

genre_chart


# Explore to find the Top Products.
turtle %>%
  group_by(Product) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  print(n=200)

# Notes:
# Upon investigation, the product number is not unique at all. 
# Therefore we cannot use the Product ID as an identifier.
# Maybe we can use a different identifier instead such as
# the "Ranking" as this seems to be unique.


# Explore the data on the product release year and the sum of global sales
# per each release year.

# Top Release Year by Global Sales

year <- turtle %>%
  group_by(Year) %>%
  summarise(Global_Sum = sum(Global_Sales)) %>%
  arrange(desc(Global_Sum)) %>%
  print(n=40)

# Notes:
  # It seems that games that were released in year 2006 accounts for the highest
  # sum of Global_Sales, followed by year 2009 and year 2010.

# Create bar plot to visualise.
ggplot(year, aes(x = Year, y = Global_Sum)) +
  geom_bar(stat = "identity") +
  labs(title = "Sum of Global Sales by Product Release Year",
       x = "Release Year",
       y = "Sum of Global Sales") + 
  scale_x_discrete(limits = turtle$Year) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Find out which games were released in the year 2006 and
# arrange by Global_Sales.

games_2006 <- turtle %>%
  filter(Year == '2006') %>%
  arrange(desc(Global_Sales))

print(games_2006)

# Notes:
  # The top 5 best-selling products from 2006 are all published by Nintendo.


###############################################################################

# EDA

# Determine the min, max and mean of all the sales data.

# Remove redundant columns (Ranking, Year, Genre, Publisher) by creating a subset of the data frame
sales <- select(turtle, Ranking, Platform, NA_Sales,
                EU_Sales, Global_Sales)

# (1) Create a columns filter.
cols <- c("NA_Sales", "EU_Sales", "Global_Sales")

# Calculate the mean for all three columns.
sales_mean <- apply(sales[,cols], 2, mean)
sales_mean

# Calculate the min for all three columns.
sales_min <- apply(sales[,cols], 2, min)
sales_min

# Calculate the max for all three columns.
sales_max <- apply(sales[,cols], 2, max)
sales_max


# Print the results.
summary(sales)
View(sales)

## can be deleted
# Boxplots of the sales variables.
# boxplot(sales$Global_Sales) 
# boxplot(sales$NA_Sales) 
# boxplot(sales$EU_Sales) 




# Determine skewness and distribution of data.

# total_global_sales Histogram:
ggplot(data = sales,
       mapping = aes(Global_Sales)) +
  geom_histogram(fill = "red",
                 color = "black",
                 bins = 20) +
  labs(title = 'Global Sales Distribution',
       y = 'Frequency',
       x = 'Global Sales') +
  theme_classic()


# total_na_sales Histogram:
ggplot(data = sales,
       mapping = aes(NA_Sales)) +
  geom_histogram(fill = "red",
                 color = "black",
                 bins = 20) +
  labs(title = 'North America Sales Distribution',
       y = 'Frequency',
       x = 'North America Sales') +
  theme_classic()

# total_eu_sales Histogram:
ggplot(data = sales,
       mapping = aes(EU_Sales)) +
  geom_histogram(fill = "red",
                 color = "black",
                 bins = 20) +
  labs(title = 'EU Sales Distribution',
       y = 'Frequency',
       x = 'EU Sales') +
  theme_classic()


###############################################################################

# Determine the normality for all the sales data.

# NA Sales

# Create a Q-Q plot.
qqnorm(sales$NA_Sales)
qqline(sales$NA_Sales,
       col ='red')

# Perform the Shapiro-Wilk test.
shapiro.test(sales$NA_Sales)

# Calculate the skewness and kurtosis.
skewness(sales$NA_Sales)
kurtosis(sales$NA_Sales)


# Create a histogram and a density plot.
hist(sales$NA_Sales)

# Create a density plot of the data.
na_density <- densityplot(sales$NA_Sales)

# Add a normal distribution curve as a trend line.
mean_x <- mean(sales$NA_Sales)
sd_x <- sd(sales$NA_Sales)
curve(dnorm(x, mean_x, sd_x), add = TRUE, col = "red")

na_density




# Notes:
  # NA Sales has p-value < 2.2e-16  skewness 4.31  kurtosis 31.368
  # P Value - very strong statistical evidence against the null hypothesis
  # Skewness - indicates a highly skewed distribution with a long tail
  # to the right.
  # Kurtosis - indicates a distribution that is highly peaked with heavy
  # tails compared to a normal distribution.
  # Overall, these results suggest that the data may have a highly skewed to
  # the right and peaked distribution with a few extreme values in the tails.


# EU Sales

# Create a Q-Q plot.
qqnorm(sales$EU_Sales)
qqline(sales$EU_Sales,
       col ='red')

# Perform the Shapiro-Wilk test.
shapiro.test(sales$EU_Sales)

# Calculate the skewness and kurtosis.
skewness(sales$EU_Sales)
kurtosis(sales$EU_Sales)


# Create a histogram and a density plot.
hist(sales$EU_Sales)

# Create a density plot of the data.
eu_density <- densityplot(sales$EU_Sales)

# Add a normal distribution curve as a trend line.
mean_x <- mean(sales$EU_Sales)
sd_x <- sd(sales$EU_Sales)
curve(dnorm(x, mean_x, sd_x), add = TRUE, col = "red")

eu_density


# Notes:
# EU Sales has p-value < 2.2e-16 skew 4.82 kurtosis 44.67
# P Value -  shows very strong statistical evidence against the null hypothesis
# Skewness - indicates a highly skewed distribution with a long tail to the right
# Kurtosis - indicates a distribution that is highly peaked with heavy tails
# compared to a normal distribution. 
# Overall, these results suggest that the data may have a highly skewed and
# peaked distribution with a few extreme values in the tails.



# Global Sales

# Create a Q-Q plot.
qqnorm(sales$Global_Sales)
qqline(sales$Global_Sales,
       col ='red')

# Perform the Shapiro-Wilk test.
shapiro.test(sales$Global_Sales)

# Calculate the skewness and kurtosis.
skewness(sales$Global_Sales)
kurtosis(sales$Global_Sales)


# Create a histogram and a density plot.
hist(sales$Global_Sales)

# Create a density plot of the data.
global_density <- densityplot(sales$Global_Sales)

# Add a normal distribution curve as a trend line.
mean_x <- mean(sales$Global_Sales)
sd_x <- sd(sales$Global_Sales)
curve(dnorm(x, mean_x, sd_x), add = TRUE, col = "red")

global_density




# Notes:
# Global Sales has p-value < 2.2e-16 skewness 4.046 kurtosis 32.64
# P Value -  shows very strong statistical evidence against the null hypothesis
# Skewness - indicates a highly skewed distribution with a long tail to the right
# Kurtosis - indicates a distribution that is highly peaked with heavy tails
# compared to a normal distribution. 
# Overall, these results suggest that the data may have a highly skewed and
# peaked distribution with a few extreme values in the tails.






















###############################################################################
# Calculate and determine correlation if any.
correlation <- select(turtle, Ranking, NA_Sales,
                               EU_Sales, Global_Sales)

# Use the corPlot() function.
# character size (cex=2).
corPlot(correlation, cex=2)


# Notes:
# Correlation between NA_Sales and EU_Sales to Global_Sales are strong.
# 0.93 for NA Sales; 0.88 for EU Sales.

###############################################################################

# Create visualisations.

# NA Sales - Global Sales plot.
plot1 <- ggplot(sales,aes(x = Global_Sales,
                    y = NA_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "North America Sales - Global Sales",
       x = "Global Sales", y = "North America Sales")

# EU Sales - Global Sales plot.
plot2 <- ggplot(sales, aes(x = Global_Sales, y = EU_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "EU Sales - Global Sales",
       x = "Global Sales", y = "EU Sales")

# NA Sales - EU Sales plot.
plot3 <- ggplot(sales, aes(x = NA_Sales, y = EU_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "NA Sales - EU Sales", 
       x = "North America Sales", y = "EU Sales")

# view the plots.
plot1
plot2
plot3

# Arrange the scatterplots in a grid
grid.arrange(plot1, plot2, ncol = 2 )

# Notes:
#EU Sales + NA_Sales can be a strong predictor for Global Sales therefore we can use this to build MLR to predict Global Sales.


# Subset the data to show the variables needed to build MLR.
sales_mlr <- select(sales, c(NA_Sales, EU_Sales, Global_Sales))

# View the data frame.
head(sales_mlr)

# Build the Multiple Linear Regression model.
model_mlr =lm(Global_Sales ~ EU_Sales + NA_Sales, data = sales_mlr)

# View summary of the MLR.
summary(model_mlr)

# Notes:
# Adjusted R-Squared shows that 96.85% of the observed values can be explained by the model.
# This suggests that the MLR model is considered highly accurate.
# The coefficients table suggests that the significance of the explanatory variables are both very high.
# Therefore, we can conclude that this MLR model is very strong. 

# Histogram plot for the residual standard error.
ggplot(data = model_mlr, aes(x = model_mlr$residuals)) +
  geom_histogram(fill = 'blue', color = 'black', bins = 20) +
  labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')

# Plot 4 charts: Residuals vs Leverage, Residuals vs Fitted,Normal Q-Q, Scale-Location.
plot(model_mlr)

# Visualise the MLR model.

# Generate predicted values
predicted <- predict(model_mlr)
head(predicted)
predicted

# Round predicted values to 2 decimal points.
predicted_values <-round(predicted, 2)
predicted_values

# Create a new data frame of observed and predicted values for Global Sales.
results <- data.frame(Global_Sales = sales_mlr$Global_Sales, Predicted_Sales = predicted_values)

# view the output.
View(results)

# Save the results to a CSV file.
write.csv(results, file = "turtle_sales_prediction.csv", row.names = TRUE)


# Create a scatter plot of observed versus predicted values with a regression line
ggplot(results, aes(x = Global_Sales, y = Predicted_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Global_Sales") +
  ylab("Predicted_Sales") +
  ggtitle("Global_Sales vs. Predicted_Sales values")

# Define the given five (5) predictor pairs (NA_Sales and EU_Sales)
# for prediction.
input_pairs <- data.frame(NA_Sales = c(34.02, 3.93, 2.73, 2.26, 22.08),
                          EU_Sales = c(23.80, 1.56, 0.65, 0.97, 0.52))

# Generate the predicted y values.
predicted_y <- predict(model_mlr, newdata = input_pairs)

# Print the predicted y values.
print(paste("Predicted y values:", round(predicted_y, 2)))

View(predicted_y)