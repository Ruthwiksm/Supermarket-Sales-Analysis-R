# install.packages("lubridate")
# install.packages("lme4")
# install.packages("lmerTest")
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("magrittr")
# install.packages("sjPlot")
# install.packages("sjmisc")
# install.packages("sjstats")
# install.packages("arm")
# install.packages("ggthemes")
# install.packages("ggplot2")
# install.packages("broom")
# install.packages("knitr")
# install.packages("kableExtra")
# install.packages("gridExtra")
#install.packages("mice")
library(mice)
library(lubridate)
library(lmerTest)
library(tidyverse)
library(readxl)
library(magrittr)
library(sjPlot)
library(sjmisc)
library(sjstats)
library(arm)
library(ggthemes)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(broom)
library(knitr)
library(lme4)
library(kableExtra)

rdata <- read_csv("C:/Users/homehp/Desktop/Project/SuperMarket_Analysis_Missing.csv")

str(rdata)
#correcting the date and time into one set
rdata$fulltime <- paste(rdata$Date, rdata$Time)
rdata$fulltime
rdata$fulltime <- strptime(rdata$fulltime, format = "%m/%d/%Y %H:%M:%S")
sort(rdata$fulltime)
rdata$newdate <- as.Date(rdata$fulltime)
str(rdata$newdate)
rdata$Month <- format(rdata$newdate, "%m")
#changing column names
str(rdata)
colnames(rdata, do.NULL = TRUE, prefix = "col")
colnames(rdata) <- c("invoice_ID", "Branch", "City", "customer_kind", "gender"
                     , "item", "unit_price", "quantity", "tax(5%)", "total_sales", 
                     "Date", "time", "payment_method", "cost_of_goods_sold", 
                     "gross_margin_percentage", "gross_income", "rating", "exact_time", 
                     "new_date", "month")
rdata$Date <- NULL
rdata$exact_time <- NULL
rdata$time <- NULL
rdata$invoice_ID <- NULL
rdata$gross_margin_percentage <- NULL
summary(rdata)

# Calculate the amount of missing data in the dataframe
missing_data_summary <- sapply(rdata, function(x) sum(is.na(x)))
print("Missing data summary:")
print(missing_data_summary)

# Impute missing values using mice with m=10
imputed_data <- mice(rdata, m=10, method='pmm', maxit=50, seed=500)

# Use Rubin's rule to pool the results
pooled_results <- complete(imputed_data, "long", include = TRUE)

# Display the summary of the pooled results
summary(pooled_results)

# Example of using the pooled data for analysis
# Calculate the mean of total_sales from the pooled data
mean_total_sales <- with(pooled_results, mean(total_sales, na.rm = TRUE))
print(mean_total_sales)

#summary table
summary_specific <- rdata %>%
  summarise(
    Statistic = c("Mean", "Standard Deviation", "Min", "Max"),
    `Cost of Goods Sold` = c(mean(cost_of_goods_sold, na.rm = TRUE), sd(cost_of_goods_sold, na.rm = TRUE), min(cost_of_goods_sold, na.rm = TRUE), max(cost_of_goods_sold, na.rm = TRUE)),
    `Total Sales` = c(mean(total_sales, na.rm = TRUE), sd(total_sales, na.rm = TRUE), min(total_sales, na.rm = TRUE), max(total_sales, na.rm = TRUE)),
    `Gross Income` = c(mean(gross_income, na.rm = TRUE), sd(gross_income, na.rm = TRUE), min(gross_income, na.rm = TRUE), max(gross_income, na.rm = TRUE)),
    Rating = c(mean(rating, na.rm = TRUE), sd(rating, na.rm = TRUE), min(rating, na.rm = TRUE), max(rating, na.rm = TRUE))
  )
# Display table
kable(summary_specific, caption = "Summary of Numerics in supermarket") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Function to calculate outliers
calculate_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  outliers <- x[x < lower_bound | x > upper_bound]
  return(outliers)
}

# Calculate outliers for total_sales
total_sales_outliers <- calculate_outliers(rdata$total_sales)
print("Outliers in Total Sales:")
print(total_sales_outliers)

# Calculate outliers for gross_income
gross_income_outliers <- calculate_outliers(rdata$gross_income)
print("Outliers in Gross Income:")
print(gross_income_outliers)

# Calculate outliers for total_sales by item
item_sales_outliers <- rdata %>%
  group_by(item) %>%
  summarise(outliers = list(calculate_outliers(total_sales))) %>%
  unnest(outliers)

print("Outliers in Total Sales by Item:")
print(item_sales_outliers)

# Create a pie chart showing the mean of total sales for each branch
branch_mean_sales <- rdata %>%
  group_by(Branch) %>%
  summarise(mean_total_sales = mean(total_sales, na.rm = TRUE)) %>%
  mutate(percentage = mean_total_sales / sum(mean_total_sales) * 100)

ggplot(branch_mean_sales, aes(x = "", y = percentage, fill = Branch)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Mean Total Sales by Branch", x = "", y = "Percentage") +
  theme_void()

#bar chart
ggplot(rdata, aes(x = factor(Branch))) +
  geom_bar(fill='blue', colour='black') +
  labs(x = "Branch", y = "frequency")

#total_sales by gender
ggplot(rdata, aes(x = gender, y = total_sales, fill = gender)) +
  geom_boxplot() +
  labs(title = "Total Sales by Gender", x = "Gender", y = "Total Sales")

#scatter plot
ggplot(data = rdata, aes(x= `rating`,y= `cost_of_goods_sold`))+
  geom_point(colour='red')+
  labs(x="rating", y="COGS")

#Sales of item per month
x <- rdata %>% filter(month %in% c("01", "02", "03"))
ggplot(x, aes(x = item, y = total_sales, fill = month)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Item sale per month", x = "Item", y = "Total Sales")

# Generate line charts for each selected item with average sales
selected_items <- rdata %>%
  group_by(item) %>%
  summarise(total_sales = sum(total_sales)) %>%
  top_n(6, total_sales) %>%
  pull(item)

# Filter data for the selected items
rdata_selected_items <- rdata %>% filter(item %in% selected_items)
plots <- list()
for (item in selected_items) {
  rdata_item <- rdata_selected_items %>% filter(item == !!item) %>%
    group_by(new_date) %>%
    summarise(avg_total_sales = mean(total_sales, na.rm = TRUE))
  
  plot <- ggplot(rdata_item, aes(x = new_date, y = avg_total_sales, group = item)) +
    geom_line(color = "blue") +
    labs(title = paste("Average Sales:", item), x = "Date", y = "Average Total Sales") +
    theme(legend.position = "none")
  
  plots[[length(plots) + 1]] <- plot
}

# Arrange all plots in one frame
do.call(grid.arrange, c(plots, ncol = 2))

#Sales for each gender per month
z<- rdata %>% filter(month %in% c("01", "02", "03"))
ggplot(z, aes(x = month, y = total_sales, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sales by Gender and month", x = "Month", y = "Total Sales")

#Total Sales by Payment Method and Months
ggplot(rdata, aes(x = month, y = total_sales, fill = payment_method)) +
  geom_histogram(stat = "identity", position = "dodge") +
  labs(title = "Total Sales by Payment Method and Months", x = "Month", y = "Total Sales")

