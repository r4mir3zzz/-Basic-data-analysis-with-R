library(tidyverse)
library(lubridate)
library(skimr)
library(plotrix)
library(lessR)

salesData <- read.csv(file.choose(), sep = ',')

# Exploring the data
glimpse(salesData)
skim(salesData)

# Cleaning missing values
salesData <- drop_na(salesData)
View(salesData)

salesData <- salesData %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))


# Analysis Of The Data

# What is the product category with the highest sales?

ggplot(salesData, aes(x=productCategory, y=unitsSold)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Units Sold for Categories", 
       x="Product Category", y="Units Sold") +
  theme_minimal()

# What is the individual product with the highest units sold?

highestUnitsSold <- salesData %>% 
  group_by(productName) %>% 
  summarize(totalUnitSold = sum(unitsSold)) %>% 
  arrange(desc(totalUnitSold)) %>% 
  slice(1)

highestUnitsSold

# What is the region with the highest total sales?
ggplot(salesData, aes(x=region,y=unitsSold))+
  geom_bar(stat = "identity")

# What payment method is the most used by customers?
ggplot(salesData, aes(x=paymentMethod))+
  geom_bar(stat = "count")

# Which products have the highest profit margin?
highesProfitMargin <- salesData %>% 
  group_by(productName) %>% 
  summarize(highestProfit = sum(totalRevenue)) %>% 
  arrange(desc(highestProfit)) %>% 
  slice(1)

highesProfitMargin

# Region with the most total revenue
salesData %>% 
  group_by(region) %>% 
  summarize(mostRevenue = sum(totalRevenue)) %>% 
  arrange(desc(mostRevenue))

# Months with the most units sold
monthlySales <- salesData %>% 
  group_by(month = format(date,"%m")) %>% 
  summarize(totalUnits = sum(unitsSold))

monthly_sales <- monthlySales[order(monthlySales$month),]
monthly_sales

ggplot(monthly_sales, aes(x= month, y=totalUnits))+
  geom_bar(stat = "identity")


# Distribution of units sold by region
totalSoldPerRegion <- salesData %>% 
  group_by(region) %>% 
  summarize(totalRegionSold = sum(unitsSold))

totalSoldPerRegion

pie(totalSoldPerRegion$totalRegionSold)



# Checking which algorithm is good for this case

# Histograms
hist(salesData$unitsSold,main = "Distribution of Units Sold",
     xlab = "Units Solds",col="darkmagenta")

hist(salesData$totalRevenue)
hist(salesData$unitPrice)

ggplot(salesData, mapping = aes(x=totalRevenue, y=unitPrice))+
  geom_point(aes(color = productCategory))+
  geom_smooth()

zoomInPlot(x=salesData$unitPrice, y=salesData$totalRevenue,
           zoomtitle = "Plot whith zoom", rxlim = c(0, 800), rylim = c(0, 800), 
           col = 3)