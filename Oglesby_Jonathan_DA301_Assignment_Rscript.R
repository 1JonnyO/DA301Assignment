## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact
# Jonny Oglesby
###############################################################################

##  Turtle Games wants to understand:
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

# Load and explore the data

# Importing the data and data validation 

# Install the libraries.
install.packages('tidyverse')
install.packages("ggplot2")
install.packages("cowplot")
install.packages("reshape2")


# Import the libraries.
library(tidyverse)
library(tibble)
library(ggplot2)
library(tidyr)
library(cowplot)
library(conflicted)
library(dplyr)
library(reshape2)


# Determine your working directory
getwd()
# Change your current directory, if needed. Using forward slashes.
# setwd(dir='C:/')
file_path <- 
  "C:/Users/j_ogl/Documents/LSE_DA301_assignment_files/turtle_sales.csv"

# Import the CSV file using the specified file path
data <- read.csv(file_path, header = TRUE)

# Print the data frame.
print(data)
View(data)
# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
data <- subset(data, select = -c(Ranking, Year, Genre, Publisher))
# View the data frame.
View(data)

## View the descriptive statistics.
# Sense-check the data set
# Return the structure of the data frame.
str(data)
# Check the type of the data frame.
typeof(data)
# Check the class of the data frame.
class(data)
# Check the dimensions of the data frame
dim(data)

# View as a tibble.
as_tibble(data)
# View summary of data
summary(data)
# Check missing data
missing_values_count <- colSums(is.na(data))
# Print the count of missing values for each column
print(missing_values_count)

# Using names function see column titles.
column_names <- names(data)
print(column_names)

# Check the unique values in the platform column 
unique_values<- unique(data$Platform)
print(unique_values)

# Count non-missing entries in each column
entry_counts <- colSums(!is.na(data))
# Print the entry counts
print(entry_counts)

# Count the numnber of sales below # Define the threshold value
threshold <- 20
# Calculate the proportions below the threshold for each column
proportion_below_threshold <- c(
  sum(data$NA_Sales < threshold) / nrow(data),
  sum(data$EU_Sales < threshold) / nrow(data),
  sum(data$Global_Sales < threshold) / nrow(data))
# Print the proportions
proportion_below_threshold
# Calculate the mean of the proportions
mean_proportion <- mean(proportion_below_threshold)
# Print the mean
mean_proportion

## This shows that within each geographical region 99% of sales were below 
## 20 Million.


################################################################################

# Exploratory analysis and data visualisation 

# Review plots to determine insights into the data set.
# Create Scatter plots


# Assess products vs Global and EU sales
plot(data$Product, 
     data$Global_Sales, 
     main = "Product vs Global_Sales",  
     xlab = "Product",            
     ylab = "Global_Sales",         
     pch = 18,                       
     col = "blue")
# Add gridlines
grid()
plot(data$Product, 
     data$EU_Sales, 
     main = "Product vs EU_Sales",  
     xlab = "Product",            
     ylab = "EU_Sales",         
     pch = 18,                       
     col = "blue")

## Exploratory scatter plots to get a feel for the distribution of the data. 



# Create a new column 'Total_Sales' by summing the individual sales columns
data_combined <- data %>%
  dplyr::mutate(Total_Sales = NA_Sales + EU_Sales + Global_Sales)
# Group the data by 'Product' and calculate the sum of 'Total_Sales' for 
# each product
grouped_data <- data_combined %>%
  group_by(Product) %>%
  summarise(Total_Sales = sum(Total_Sales))
# Plot 'Product' against 'Total_Sales'
plot(grouped_data$Product, 
     grouped_data$Total_Sales,  
     main = "Product vs Total_Sales",    
     xlab = "Product",            
     ylab = "Total_Sales",  
     pch = 18,                       
     col = "blue")

## Determine this plot is very good for comparing game sales.



# Create a scatterplot with a legend, on  platform, product vs NA sales 
ggplot(data, aes(x = Product, y = NA_Sales, color = Platform)) +
  geom_point(shape = 18) +
  labs(title = "Product vs NA_Sales",
       x = "Product",
       y = "NA_Sales") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.position = "right")

## Difficult to assess by platform due to the number of platforms and seen in 
## this plot. 



# Filter the data to include sales above 15 million
## These will indicate if there is any possible relationship in sales 
## between North America, Europe, and global sales.
library(ggplot2)
filtered_data <- data %>%
  dplyr::filter(NA_Sales > 15 | EU_Sales > 15 | Global_Sales > 15)
# Create a scatterplot
ggplot(filtered_data, aes(x = NA_Sales, y = Global_Sales, color = EU_Sales, 
                          label = Product)) +
  geom_point() +
  labs(
    x = "NA Sales (in millions of £)",
    y = "Global Sales (in millions of £)",
    title = "Scatterplot of Sales (Above 10 Million) by Product",
    color = "EU Sales (in millions of £)") +
  geom_text(nudge_x = 0.5, nudge_y = 0.5) + 
  scale_color_gradient(low = "lightgreen", high = "darkgreen")  

# Create a scatterplot
ggplot(filtered_data, aes(x = NA_Sales, y = EU_Sales, color = Global_Sales, 
                          label = Product)) + geom_point() + labs(
    x = "NA Sales (in millions of £)",
    y = "EU Sales (in millions of £)",
    title = "Scatterplot of Sales (Above 10 Million) by Product",
    color = "Global Sales (in millions of £)") +
  geom_text(nudge_x = 0.5, nudge_y = 0.5) +  
  scale_color_gradient(low = "lightblue", high = "navy")  

## There is less correlation in this plot between EU and NA sales.



# Create a new variable for combined EU and NA sales.
filtered_data <- filtered_data %>%
  mutate(Combined_Sales = EU_Sales + NA_Sales)
# Set a dark theme for the plot, to aid readability.
ggplot() +
  geom_point(data = filtered_data, aes(x = Combined_Sales, y = Global_Sales, 
                                       color = Global_Sales, label = Product)) +
  labs(
    x = "Combined Sales (in millions of £)",
    y = "Global Sales (in millions of £)",
    title = "Scatterplot of Combined Sales vs Global Sales (Above 15 Million) 
    by Product",
    color = "Global Sales (in millions of £)") +
  geom_text(nudge_x = 0.5, nudge_y = 0.5) +  
  scale_color_gradient(low = "yellow", high = "orange") +  
  theme_dark()  

## In the Scatter plot of Combined Sales vs Global Sales (above 15 Million) 
## by Product, it is clear there is a strong positive correlation between 
## global sales and North America and Europe combined. 



################################################################################

# Histograms and barplots


## Plots to analyse if there is any possible relationship in sales between 
## North America, Europe, and global sales.

# Set the axis limits to have a common plot frame.
y_limit <- 350
x_limit <- range(c(data$NA_Sales, data$EU_Sales, data$Global_Sales))
# Define the bin width
bin_width <- 10
# Calculate the breaks based on the bin width and the data range
breaks <- seq(floor(min(x_limit) / bin_width) * bin_width, 
              ceiling(max(x_limit) / bin_width) * bin_width, by = bin_width)
# Create a layout for multiple plots
par(mfrow = c(1, 3))
# Create histograms for NA_Sales, EU_Sales, and Global_Sales
hist(data$NA_Sales, col = "skyblue", main = "NA_Sales", 
     xlab = "Sales (in millions of £)", ylim = c(0, y_limit), xlim = x_limit, 
     breaks = breaks)
hist(data$EU_Sales, col = "skyblue", main = "EU_Sales", 
     xlab = "Sales (in millions of £)", ylim = c(0, y_limit), xlim = x_limit, 
     breaks = breaks)
hist(data$Global_Sales, col = "skyblue", main = "Global_Sales", 
     xlab = "Sales (in millions of £)", ylim = c(0, y_limit), xlim = x_limit, 
     breaks = breaks)

## These plots show a significantly lower frequency from NA to EU to global 
## sales. It also shows significantly higher sales moving from EU to NA to 
## global sales.

## These plots have bins with the same width and the same y-axis scale, ensuring
## consistent comparison of sales distribution across NA, EU, and global regions.




## Repeat the plot to see the sales in more detail up to 20m. # Set the axis 
##limits to have a common plot frame.


# Set the axis limits to have a common plot frame.
y_limit <- 350
x_limit <- c(0, 20) 
# Calculate the breaks based on the data range for Global_Sales
breaks <- seq(floor(min(data$Global_Sales)), ceiling(max(data$Global_Sales)), 
              by = 2)  
# Create a layout for multiple plots
par(mfrow = c(1, 3))
# Create histograms for NA_Sales, EU_Sales, and Global_Sales
hist(data$NA_Sales, col = "skyblue", main = "NA_Sales", 
     xlab = "Sales (in millions of £)", ylim = c(0, y_limit), xlim = x_limit, 
     breaks = breaks)
hist(data$EU_Sales, col = "skyblue", main = "EU_Sales", 
     xlab = "Sales (in millions of £)", ylim = c(0, y_limit), xlim = x_limit, 
     breaks = breaks)
hist(data$Global_Sales, col = "skyblue", main = "Global_Sales", 
     xlab = "Sales (in millions of £)", ylim = c(0, y_limit), xlim = x_limit, 
     breaks = breaks)






# Reset the plot layout to a single plot area
par(mfrow = c(1, 1))




# Determine the impact on sales per product_id.
# Combine the sales columns to plot total sales by product.
data_combined <- data %>%
  mutate(Total_Sales = NA_Sales + EU_Sales + Global_Sales)
# Group the data by 'Product' and calculate the total sales for each product
sales_by_product <- data_combined %>%
  group_by(Product) %>%
  summarize(Total_Sales = sum(Total_Sales))
# Calculate the median Total_Sales
median_total_sales <- median(sales_by_product$Total_Sales)
# Create a bar plot
ggplot(sales_by_product, aes(x = reorder(Product, Total_Sales), y = 
                               Total_Sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  # Add a line for the median Total_Sales
  geom_hline(yintercept = 27, color = "red", linetype = "dashed") +
  labs(
    x = "Product",
    y = "Total Sales (in millions of £)",
    title = "Total Sales by Product" ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## Too many products to look at in detail but it gives a good braod overview of
## the impact on sales per product. 
## Can see in the Total Sales by product that there are a couple of very 
## successful games.



## To understand the impact on sales per product, I analysed the majority
## of sales by assessing those with total sales below 27m.



# Count the number of sales below the threshold
# Function to calculate summary statistics and create the plot
calculate_and_plot_sales_summary <- function(threshold) {
  # Filter data based on the threshold
  filtered_data <- data_combined[data_combined$Total_Sales < threshold, ]
  # Calculate the number of sales above the threshold
  sales_above_threshold <- nrow(data_combined) - nrow(filtered_data)
  # Calculate total sales values both above and below the threshold
  total_sales_below_threshold <- sum(filtered_data$Total_Sales)
  total_sales_above_threshold <- sum(data_combined$Total_Sales) - 
    total_sales_below_threshold
  # Calculate the mean sale value per sale for both above and below the 
  # threshold
  mean_sale_below_threshold <- total_sales_below_threshold / 
    nrow(filtered_data)
  mean_sale_above_threshold <- total_sales_above_threshold / 
    sales_above_threshold
  # Calculate the total sales percentage both above and below the threshold
  total_sales <- sum(data_combined$Total_Sales)
  percentage_below_threshold <- (total_sales_below_threshold / total_sales) * 
    100
  percentage_above_threshold <- (total_sales_above_threshold / total_sales) * 
    100
  # Print the results
  cat("Number of Total_Sales below the threshold:", nrow(filtered_data),
      " (£", total_sales_below_threshold, ", ", percentage_below_threshold, "%),
      Mean product impact (£", mean_sale_below_threshold, ")\n")
  cat("Number of Total_Sales above the threshold:", sales_above_threshold,
      " (£", total_sales_above_threshold, ", ", percentage_above_threshold, "%), 
      Mean product impact (£", mean_sale_above_threshold, ")\n")
  # Create a data frame for the results
  results <- data.frame(
    Category = c("Below Threshold", "Above Threshold"),
    Sales = c(nrow(filtered_data), sales_above_threshold),
    Total_Sales = c(total_sales_below_threshold, total_sales_above_threshold),
    Percentage = c(percentage_below_threshold, percentage_above_threshold),
    Mean_Impact = c(mean_sale_below_threshold, mean_sale_above_threshold))
  # Create a bar plot
  ggplot(results, aes(x = Category, y = Sales, fill = Category)) +
    geom_bar(stat = "identity") +
    labs(
      x = "Category",
      y = "Number of Sales",
      title = "Sales Distribution Above and Below Threshold" )}

# Set the threshold value
threshold <- 20
# Call the function to calculate and plot the summary
calculate_and_plot_sales_summary(threshold)





# Create a bar plot for Platform vs. Total_Sales

# Group the data by 'Platform' and calculate the total sales for each platform
sales_by_platform <- data_combined %>%
  group_by(Platform) %>%
  summarize(Total_Sales = sum(Total_Sales))

ggplot(sales_by_platform, aes(x = reorder(Platform, Total_Sales), 
                              y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    x = "Platform",
    y = "Total Sales (in millions of £)",
    title = "Total Sales by Platform") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## This shows the sales by platform with a broad range of total sales.



# Group the data by 'Platform' and calculate count of products for each 
platform_counts <- data %>%
  group_by(Platform) %>%
  summarize(Count = n())
# Create a bar plot for 'Platform'
ggplot(platform_counts, aes(x = reorder(Platform, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    x = "Platform",
    y = "Count",
    title = "Count of Products by Platform") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## After grouping this shows the number of products by platform. 



sales_by_platform <- data_combined %>%
  group_by(Platform) %>%
  summarize(Total_Sales = sum(Total_Sales))
plot1 <- ggplot(sales_by_platform, aes(x = reorder(Platform, Total_Sales), 
                                       y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    x = "Platform",
    y = "Total Sales (in millions of £)",
    title = "Total Sales by Platform"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Group the data by 'Platform' and calculate count of products for each platform
platform_counts <- data %>%
  group_by(Platform) %>%
  summarize(Count = n())
plot2 <- ggplot(platform_counts, aes(x = reorder(Platform, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    x = "Platform",
    y = "Count",
    title = "Count of Products by Platform") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Combine the plots using cowplot
combined_plot <- plot_grid(plot1, plot2, ncol = 2, labels = c("A", "B"))
# Print the combined plot
print(combined_plot)

## Created these histograms alongside each other for presentation purposes.



################################################################################

# Boxplots
# Create boxplots.
ggplot(data, aes(x = Platform, y = Global_Sales, fill = Platform)) +
  geom_boxplot() +
  labs(
    x = "Platform",
    y = "Global Sales",
    title = "Boxplot of Global Sales by Platform")

## Exploratory box plot.



# Create a boxplot of sales data
# Reshape the data into long format
data_long <- data %>%
  gather(Sales_Type, Sales, Global_Sales, EU_Sales, NA_Sales)
ggplot(data_long, aes(x = Sales_Type, y = Sales, fill = Sales_Type)) +
  geom_boxplot() +
  labs(
    x = "Sales Type",
    y = "Sales (in millions of £)",
    title = "Boxplot of Sales Data by Sales Type")


## This box plot shows the sales by each given geographic parameter. 



################################################################################

#Barplots

# Create a grouped barplot
ggplot(data, aes(x = Platform, y = Global_Sales, fill = Platform)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Platform",           
    y = "Global_Sales",              
    title = "Distribution of Global_Sales by Platform"  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  

## This plot shows the most productive platform by global sales.




################################################################################

# Load and explore the data
# View data frame created in Week 4.
data

# Check output: Determine the min, max, and mean values.
# Calculate minimum, maximum, and mean values for specific columns
min_values <- sapply(data, function(x) 
  if(is.numeric(x)) min(x, na.rm = TRUE) else NA)
max_values <- sapply(data, function(x) 
  if(is.numeric(x)) max(x, na.rm = TRUE) else NA)
mean_values <- sapply(data, function(x) 
  if(is.numeric(x)) mean(x, na.rm = TRUE) else NA)
# Create a summary dataframe
summary_df <- data.frame(
  Minimum = min_values,
  Maximum = max_values,
  Mean = mean_values)
# Print the summary dataframe
print(summary_df)

# View the descriptive statistics.
summary(data)



###############################################################################

# Determine the normality of the data set.
# Create Q-Q Plots.



# Q-Q Plot to Determine Normality and add a reference line
# Create a Q-Q plot
# Extract the Global_Sales
Global_Sales <- data$`Global_Sales`
qqnorm(Global_Sales, main = "Q-Q Plot of Global_Sales")
qqline(Global_Sales, col = 2)  

# Create a Q-Q plot for EU_Sales
EU_Sales <- data$`EU_Sales`
qqnorm(EU_Sales, main = "Q-Q Plot of EU_Sales")
qqline(EU_Sales, col = 4)  

# Create a Q-Q plot for NA_Sales
NA_Sales <- data$`NA_Sales`
qqnorm(NA_Sales, main = "Q-Q Plot of NA_Sales")
qqline(NA_Sales, col = 4)  

## There is a large proportion of points consistently above the line,
## This suggests that the data has heavier tails than the theoretical 
## distribution.



###############################################################################

## Perform Shapiro-Wilk test
# Install and import Moments.
install.packages("moments")
library(moments)

perform_shapiro_wilk_test <- function(data, column_name) {
  # Extract the data vector from the specified column
  data_vector <- data[[column_name]]
  # Check if the data_vector is numeric
  if (is.numeric(data_vector)) {
    # Perform the Shapiro-Wilk test
    shapiro_result <- shapiro.test(data_vector)
    
    # Print the test result
    print(shapiro_result)
  } else {
    print(paste("The selected column '", 
                column_name, "' does not contain numeric data.", sep = ""))}}
# Perform Shapiro-Wilk test.
perform_shapiro_wilk_test(data, 'NA_Sales')
perform_shapiro_wilk_test(data, 'EU_Sales')
perform_shapiro_wilk_test(data, 'Global_Sales')

## As the p-value is greater than 0.05 it suggests that there is not the 
## required evidence to reject the null hypothesis. So it is assumed that the 
## data follows a normal distribution.




###############################################################################

## Determine Skewness and Kurtosis
# Skewness and Kurtosis

# Install and load the e1071 package 
install.packages("e1071")
library(e1071)
install.packages("moments")
library(moments)


# Function to calculate skewness
calculate_skewness <- function(dataframe, column_name) {
  # Extract the data vector from the specified column
  data_vector <- dataframe[[column_name]]
  # Check if the data_vector is numeric
  if (is.numeric(data_vector)) {
    # Calculate skewness using the skewness() function from the e1071 package
    skewness_value <- e1071::skewness(data_vector)
    # Return the skewness value
    return(skewness_value)
  } else {
    # If the data is not numeric, return an error message
    return("Error: The selected column does not contain numeric data.")}}

# Usage:
skewness_result <- calculate_skewness(data, 'NA_Sales')
print(skewness_result)
skewness_result <- calculate_skewness(data, 'EU_Sales')
print(skewness_result)
skewness_result <- calculate_skewness(data, 'Global_Sales')
print(skewness_result)

## A skewness value of 4.0 indicates that the data is positively skewed, 
## as it has a long tail on the right side of the distribution. 
## This may need to be addressed.



###############################################################################



# Calculate kurtosis for specific columns
kurtosis_values <- moments::kurtosis(data[, c("NA_Sales", "EU_Sales", 
                                              "Global_Sales")])
# Print the kurtosis values
print(kurtosis_values)


## The kurtosis values are significantly higher than the kurtosis of a normal 
## distribution(3). Indicating heavy tails, with more extreme values (outliers) 
## compared to normal distribution.
## The distribution is leptokurtic, which means it is more peaked. 



###############################################################################



# Create the results in a format to share
# Create an empty data frame to store the results
results_df <- data.frame(
  Variable = character(),
  Shapiro_Wilk_p_value = numeric(),
  Skewness = numeric(),
  Kurtosis = numeric()
)

# Function to perform Shapiro-Wilk test and calculate skewness
perform_shapiro_wilk_and_skewness <- function(dataframe, column_name) {
  # Perform Shapiro-Wilk test
  shapiro_test_result <- shapiro.test(dataframe[[column_name]])
  
  # Calculate skewness using moments package
  skewness_value <- moments::skewness(dataframe[[column_name]])
  
  # Calculate kurtosis using moments package
  kurtosis_value <- moments::kurtosis(dataframe[[column_name]])
  
  # Create a data frame with the results
  result_row <- data.frame(
    Variable = column_name,
    Shapiro_Wilk_p_value = shapiro_test_result$p.value,
    Skewness = skewness_value,
    Kurtosis = kurtosis_value
  )
  
  return(result_row)
}

# Perform Shapiro-Wilk test and calculate skewness for each column
sales_columns <- c('NA_Sales', 'EU_Sales', 'Global_Sales')

for (col in sales_columns) {
  result_row <- perform_shapiro_wilk_and_skewness(data, col)
  results_df <- rbind(results_df, result_row)
}

# Print the results data frame
print(results_df)
view(results_df)



###############################################################################

# Determine correlation between the sales columns.

# Function to calculate correlation
calculate_correlation <- function(dataframe, var1, var2) {
  # Check if the specified variables exist in the dataframe
  if (var1 %in% colnames(dataframe) && var2 %in% colnames(dataframe)) {
    # Calculate the correlation between the two variables
    correlation_value <- cor(dataframe[[var1]], dataframe[[var2]])
    
    # Return the correlation coefficient
    return(correlation_value)
  } else {
    # If one or both variables do not exist, return an error message
    return("Error: One or both specified variables do 
           not exist in the dataframe.") }}

correlation_result <- calculate_correlation(data, 'NA_Sales', 'EU_Sales')
print(correlation_result)
correlation_result <- calculate_correlation(data, 'NA_Sales', 'Global_Sales')
print(correlation_result)
correlation_result <- calculate_correlation(data, 'EU_Sales', 'Global_Sales')
print(correlation_result)

## A correlation coefficient ranges from -1 (perfect negative correlation) to 1 
## (perfect positive correlation).
## These scores indicates a strong positive linear relationship between 
## the two variables.
## This suggests that the relationship between the two variables is strong. 
## Meaning that changes in one variable are associated with predictable changes 
## in the other variable.




###############################################################################

# Create a simple linear regression model
# Determine the correlation between columns
# Create a linear regression model function.
create_simple_linear_regression <- function(data, predictor_variable, 
                                            response_variable) {
  # Create the linear model
  model <- lm(data[[response_variable]] ~ data[[predictor_variable]])
    # View model summary and return the fitted model
  summary(model)
  return(model)}

# Call the function to create the linear regression model
regression_model <- create_simple_linear_regression(data, "EU_Sales", 
                                                    "NA_Sales")
# View the summary of the regression model and visualise the regression line 
summary(regression_model)
plot(data$EU_Sales, data$NA_Sales)
abline(regression_model, col = "red")

# Call the function to create the linear regression model on EU sales
regression_model <- create_simple_linear_regression(data, "EU_Sales", 
                                                    "Global_Sales")
# View the summary of the regression model and visualise the regression line 
summary(regression_model)
plot(data$EU_Sales, data$Global_Sales)
abline(regression_model, col = "yellow")

# Call the function to create the linear regression model on NA Sales
regression_model <- create_simple_linear_regression(data, "NA_Sales", 
                                                    "Global_Sales")
# View the summary of the regression model and visualise the regression line 
summary(regression_model)
plot(data$NA_Sales, data$Global_Sales)
abline(regression_model, col = "green")

## The linear regression model appears to be statistically significant, 
## as indicated by the low p-values for both the intercept and predictor 
## variable.
## The F-statistic also suggests that the model is significant overall.
## It has a very low p-value (< 2.2e-16), indicating that the overall model 
## is highly significant. This is because the p-value is essentially zero for 
## practical purposes and indicates an strong level of evidence against 
## the null hypothesis. 



###############################################################################

# Create a multiple linear regression model
# Select only numeric columns from the original data frame.

#  Create a Function and visualize a multiple linear regression model
create_multiple_linear_regression_model <- function(data, dependent_variable, 
                                                    independent_variables) {
  # Create the multiple linear regression model
  regression_model <- lm(data = data, formula = 
                           paste(dependent_variable, "~",
                                 paste(independent_variables, 
                                       collapse = " + ")))
    # Generate the Residuals vs. Fitted plot
  residuals_plot <- ggplot(data = data, aes(x = fitted(regression_model), 
                                            y = residuals(regression_model))) +
    geom_point() +
    geom_smooth(method = "loess", color = "red") +
    labs(
      x = "Fitted Values",
      y = "Residuals",
      title = "Residuals vs. Fitted Plot") +
    theme_minimal()
    # Print the summary of the regression model
  summary(regression_model)
    # Return the regression model and the Residuals vs. Fitted plot
  return(list(model = regression_model, residuals_plot = residuals_plot))}

result <- create_multiple_linear_regression_model(data = data, 
                                                  dependent_variable = 
                                                    "Global_Sales", 
                                                  independent_variables = 
                                                    c("EU_Sales", "NA_Sales"))
# View the Residuals vs. Fitted plot
print(result$residuals_plot)
summary(regression_model)

## The results suggest that the linear regression model is a good fit. 




###############################################################################

# Predictions based on given values

# Compare with observed values for a number of records.

# Create a function to create a multiple linear regression model
create_multiple_linear_regression_model <- function(data, dependent_variable, 
                                                    independent_variables) {
  # Combine the variables into a formula
  formula_str <- paste(dependent_variable, "~", 
                       paste(independent_variables, collapse = " + "))
  formula <- as.formula(formula_str)
  # Create the regression model
  regression_model <- lm(formula, data = data)
  # Return the trained regression model
  return(regression_model)
}
# Create the multiple linear regression model
model <- create_multiple_linear_regression_model(data, "Global_Sales", 
                                                 c("EU_Sales", "NA_Sales"))

# Create an empty data frame to store the results
result_df <- data.frame(
  Scenario = character(0),
  EU_Sales = numeric(0),
  NA_Sales = numeric(0),
  Predicted_Global_Sales = numeric(0))

# Scenario 4a
new_data <- data.frame(EU_Sales = 23.80, NA_Sales = 34.02)
predicted_global_sales <- predict(model, new_data)
result_df <- rbind(result_df, 
                   data.frame(Scenario = "4a", EU_Sales = new_data$EU_Sales, 
                              NA_Sales = new_data$NA_Sales, 
                              Predicted_Global_Sales = predicted_global_sales))
# Scenario 4b
new_data <- data.frame(EU_Sales = 1.56, NA_Sales = 3.93)
predicted_global_sales <- predict(model, new_data)
result_df <- rbind(result_df, 
                   data.frame(Scenario = "4b", EU_Sales = new_data$EU_Sales, 
                              NA_Sales = new_data$NA_Sales, 
                              Predicted_Global_Sales = predicted_global_sales))
# Scenario 4c
new_data <- data.frame(EU_Sales = 0.65, NA_Sales = 2.73)
predicted_global_sales <- predict(model, new_data)
result_df <- rbind(result_df, 
                   data.frame(Scenario = "4c", EU_Sales = new_data$EU_Sales, 
                              NA_Sales = new_data$NA_Sales, 
                              Predicted_Global_Sales = predicted_global_sales))
# Scenario 4d
new_data <- data.frame(EU_Sales = 0.97, NA_Sales = 2.26)
predicted_global_sales <- predict(model, new_data)
result_df <- rbind(result_df, 
                   data.frame(Scenario = "4d", EU_Sales = new_data$EU_Sales, 
                              NA_Sales = new_data$NA_Sales, 
                              Predicted_Global_Sales = predicted_global_sales))
# Scenario 4e
new_data <- data.frame(EU_Sales = 0.52, NA_Sales = 22.08)
predicted_global_sales <- predict(model, new_data)
result_df <- rbind(result_df, 
                   data.frame(Scenario = "4e", EU_Sales = new_data$EU_Sales, 
                              NA_Sales = new_data$NA_Sales, 
                              Predicted_Global_Sales = predicted_global_sales))

# Print the result data frame
print(result_df)



## I have confidence in the models based on the goodness of fit that the 
## predictions are accurate.


###############################################################################

# Observations and insights

## Heavy tails could be indicative of products or factors that 
## significantly influence sales in their respective regions. 

## Strong positive correlations between sales suggests changes in regional sales 
## closely relate to changes in global sales, which may imply that that the 
## regions significantly contribute to the overall global sales performance.

## To improve the models I would Collect more data if possible. A larger 
## data set can help improve the performance and reduce over fitting.


###############################################################################
###############################################################################



# Appendix 


# Analysis to see the impact of the top products. 

# Calculate market share for each product in the 'NA_Sales' region
data <- data %>%
  mutate(NA_Market_Share = NA_Sales / sum(NA_Sales))

# Set a threshold to filter out less significant products 
threshold <- 0.01

# Create a new data frame with only the top products and an "Others" category
top_products <- data %>%
  dplyr::filter(NA_Market_Share >= threshold) %>%
  arrange(desc(NA_Market_Share))

# Convert the "Product" column to character
top_products$Product <- as.character(top_products$Product)

# Combine the market shares of less significant products into "Others" category
others_market_share <- sum(data$NA_Market_Share[data$NA_Market_Share < 
                                                  threshold])

# Create a data frame for plotting
plot_data <- bind_rows(top_products, data.frame
                       (Product = "Others",NA_Market_Share = 
                           others_market_share))

# Create a horizontal bar chart
ggplot(plot_data, aes(x = reorder(Product, NA_Market_Share), 
                      y = NA_Market_Share)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Top Products' Market Share in NA Sales",
    x = "Product",
    y = "Market Share"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  



## A boxplot of sales data by sales type (Global Sales, EU Sales, NA Sales) 
## and color the boxes based on the Platform variable.
data_long <- data %>%
  gather(Sales_Type, Sales, Global_Sales, EU_Sales, NA_Sales)
ggplot(data_long, aes(x = Sales_Type, y = Sales, fill = Platform)) +
  geom_boxplot() +
  labs(
    x = "Sales Type",
    y = "Sales (in millions of £)",
    title = "Boxplot of Sales Data by Platform")




## Building a multiple linear regression model
regression_model <- lm(Global_Sales ~ EU_Sales + NA_Sales, data = data)
# Prepare new data for predictions
new_data <- data.frame(
  EU_Sales = c(10, 15, 20),  
  NA_Sales = c(5, 8, 12)    
)
# Make predictions
predictions <- predict(regression_model, newdata = data)
# View the predicted values
print(predictions)
# Create a scatterplot of the observed Global_Sales vs. the predicted 
# Global_Sales
plot(data$Global_Sales, predictions, 
     main = "Observed vs. Predicted Global Sales",
     xlab = "Observed Global Sales",
     ylab = "Predicted Global Sales")
# Add a reference line with a 1:1 relationship (ideal predictions)
abline(0, 1, col = "red")
# Add a legend
legend("topleft", legend = "Ideal Predictions", col = "red", lty = 1)





## I tried to add data labels however could not get them to show correctly.

# Set the axis limits to have a common plot frame.
y_limit <- 350
x_limit <- range(c(data$NA_Sales, data$EU_Sales, data$Global_Sales))
# Define the bin width
bin_width <- 10
# Calculate the breaks based on the bin width and the data range
breaks <- seq(floor(min(x_limit) / bin_width) * bin_width, 
              ceiling(max(x_limit)  / bin_width) * bin_width, by = bin_width)
# Create a layout for multiple plots
par(mfrow = c(1, 3))
# Create histograms for NA_Sales, EU_Sales, and Global_Sales
hist(data$NA_Sales, col = "skyblue", main = "NA_Sales", 
     xlab = "Sales (in millions of £)", ylim = c(0, y_limit), xlim = x_limit, 
     breaks = breaks)
hist(data$EU_Sales, col = "skyblue", main = "EU_Sales", 
     xlab = "Sales (in millions of £)", ylim = c(0, y_limit), xlim = x_limit, 
     breaks = breaks)
hist(data$Global_Sales, col = "skyblue", main = "Global_Sales", 
     xlab = "Sales (in millions of £)", ylim = c(0, y_limit), xlim = x_limit, 
     breaks = breaks)

# Add data labels (counts) to the columns
counts <- as.numeric(table(cut(data$NA_Sales, breaks = breaks)))
text(x = as.numeric(names(counts)), y = counts, labels = counts, pos = 3, 
     col = "red", cex = 0.8)
counts <- as.numeric(table(cut(data$EU_Sales, breaks = breaks)))
text(x = as.numeric(names(counts)), y = counts, labels = counts, pos = 3, 
     col = "red", cex = 0.8)
counts <- as.numeric(table(cut(data$Global_Sales, breaks = breaks)))
text(x = as.numeric(names(counts)), y = counts, labels = counts, pos = 3, 
     col = "red", cex = 0.8)



