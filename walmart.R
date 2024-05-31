library(readxl)
excel_file <- "C:/Users/utkar/Downloads/Marketing Analytics/walmart.xlsx"
Df <- read_excel(excel_file)
View(Df)
install.packages("readxl")
install.library("corrplot")
library(readxl)
library(scales)
library(corrplot)
library(dplyr)
library(ggplot2)
library(lubridate)
Store_Sales <- aggregate(Weekly_Sales ~ Store,data=Df,sum)
View(Store_Sales)
Store_Sales$Store <- as.character(Store_Sales$Store)
Store_Sales$Store <- factor(Store_Sales$Store, levels=unique(Store_Sales$Store))
ggplot(data = Store_Sales, aes(x = Store, y = Weekly_Sales)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5)) +
  scale_x_discrete(breaks = Df$Store) +
  scale_y_continuous(labels = function(x) paste0(x / 1e6, " M")) +
  ggtitle('Store  vs Sales') +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Stores") + ylab("Total Sales")

Df$Month <- substr(Df$Date, 4,5)
Df$Month <- as.numeric(Df$Month)
Df$Season <- cut(Df$Month, breaks = c(0, 3, 6, 9, 12), labels = c("Winter", "Spring", "Summer", "Fall"), include.lowest = TRUE)

# Calculate total sales for each season
total_sales_season <- aggregate(Weekly_Sales ~ Season, data = Df, sum)
View(total_sales_season)
ggplot(total_sales_season, aes(x = Season, y = Weekly_Sales, fill = Season)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::dollar_format(suffix = " M", scale = 1e-6)(Weekly_Sales),
                y = Weekly_Sales), vjust = -0.5) +
  labs(title = "Total Sales Comparison by Season",
       x = "Season",
       y = "Total Sales (Millions)",
       fill = "Season") +
  scale_y_continuous(labels = scales::dollar_format(suffix = " M", scale = 1e-6)) +
  theme_minimal()


View(sales_2010)
# Display Super Bowl data
print(super_bowl_data)
non_holiday_data <- Df[Df$Holiday_Flag == 0, ]
View(non_holiday_data )
View(Df$Date %in% c("2010-02-12", "2011-02-11", "2012-02-10"))
View(mean_sales_super_bowl)
mean_sales_super_bowl <- mean(super_bowl_data$Weekly_Sales)
mean_sales_labour_day <- mean(labour_day_data$Weekly_Sales)
mean_sales_thanksgiving <- mean(thanksgiving_data$Weekly_Sales)
mean_sales_christmas <- mean(christmas_data$Weekly_Sales)
mean_sales_non_holiday <- mean(non_holiday_data$Weekly_Sales)


selected_data <- Df %>%
  select(Weekly_Sales, Holiday_Flag, Temperature, Fuel_Price, CPI, Unemployment)

ggpairs(selected_data)
View(selected_data)
Df$Date <- as.Date(Df$Date, format = "%Y-%m-%d")
# Example: Creating a variable for the month
Df$Month <- as.numeric(format(Df$Date, "%m"))
correlation_matrix <- cor(Df[,c('Weekly_Sales','Holiday_Flag','Temperature',
                              'Fuel_Price','CPI',
                              'Unemployment')])
corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
hist(Df$Weekly_Sales)


model1 <- lm(Weekly_Sales ~ Holiday_Flag + Temperature +
               Fuel_Price + CPI + Unemployment, data = Df)
summary(model1)

model2 <- lm(Weekly_Sales ~ Holiday_Flag + Temperature +
                CPI + Unemployment + Holiday_Flag*Temperature, data = selected_data)
summary(model2)
Df$Year <- substr(Df$Date, 7, 10)

Df$ <- log(Df$Weekly_Sales+1)
View(Df)
table(Df$Store)

table(Df$Holiday_Flag)
model3 <- lm(log_transformed_weeklysales ~ Holiday_Flag + Temperature +
               Fuel_Price + CPI + Unemployment, data = selected_data)
summary(model3)
str(Df)
# Assuming 'Date' is in a character format, convert it to a Date object
Df$Date <- as.Date(Df$Date, format = "%YYYY-%mm-%dd")

Df$Date <- as.Date(Df$Date, format = "%Y-%m-%d")

Df$Holiday_Temperature <- Df$Holiday_Flag*Df$Temperature
Df$CPI_Temp <- Df$CPI*Df$Temperature
Df$CPI_Un <- Df$CPI*Df$Unemployment

Df$Month <- format(Df$Date, "%m")
your_data$Quarter <- quarters(your_data$Date)
# Example: Creating a variable for the month
Df$Month <- as.numeric(format(Df$Date, "%m"))

str(Df)
model3 <- lm(log_transformed_weeklysales ~  Holiday_Flag + Temperature +
               Fuel_Price + CPI + Unemployment + Temperature_Fuel
             +, data = Df)
summary(model3)
model4 <- lm(Weekly_Sales ~  Unemployment + CPI+
               Fuel_Price + Temperature+ Holiday_Flag+ CPI_Fuel+
               CPI_Un, data = Df)
summary(model4)
str(Df$Temperature)
ggplot(Df, aes(x = as.factor(Holiday_Flag), y = Weekly_Sales, fill = as.factor(Holiday_Flag))) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Effect of Holiday_Flag on Average Weekly Sales",
       x = "Holiday_Flag",
       y = "Average Weekly Sales") +
  theme_minimal()

Df$Temperature_Category <- cut(Df$Temperature, breaks = c(-Inf, 50, 75, Inf), labels = c("Low", "Medium", "High"))
ggplot(Df, aes(x = Temperature_Category, y = Weekly_Sales, fill = Temperature_Category)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Effect of Temperature on Average Weekly Sales",
       x = "Temperature Category",
       y = "Average Weekly Sales") +
  theme_minimal()

ggplot(Df, aes(x = Temperature_Category, y = Weekly_Sales, fill = Temperature_Category)) +
  geom_bar(stat = "sum", position = "dodge") +
  labs(title = "Effect of Temperature on Total Weekly Sales",
       x = "Temperature Category",
       y = "Total Weekly Sales") +
  theme_minimal()
ggplot(Df, aes(x = Holiday_Flag, y = Weekly_Sales)) +
  geom_point() +
  labs(title = "Scatter Plot of Weekly Sales vs Holiday_Flag",
       x = "Holiday_Flag",
       y = "Weekly Sales") +
  theme_minimal()
total_sales_by_category <- aggregate(Weekly_Sales ~ Temperature_Category, data = Df, sum)

# Calculate Percentage
total_sales_by_category$Percentage <- total_sales_by_category$Weekly_Sales / sum(total_sales_by_category$Weekly_Sales) * 100

# Create Pie Chart with Percentage Labels
ggplot(total_sales_by_category, aes(x = "", y = Percentage, fill = Temperature_Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Distribution of Total Weekly Sales by Temperature Category",
       fill = "Temperature Category") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())

ggplot(Df, aes(x = CPI, y = Weekly_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue", formula = y ~ x) +
  labs(title = "Scatter Plot with Regression Line",
       x = "CPI",
       y = "Weekly Sales") +
  theme_minimal()
model <- lm(Weekly_Sales ~ CPI, data = Df)
summary(model)
intercept <- coef(model)[1]
slope <- coef(model)[2]
ggplot(Df, aes(x = CPI, y = Weekly_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue", formula = y ~ x) +
  geom_text(x = max(Df$CPI), y = max(Df$Weekly_Sales), 
            label = paste("Equation: y =", round(intercept, 2), "+", round(slope, 2), "* x"),
            hjust = 1, vjust = 1, parse = TRUE) +
  labs(title = "Scatter Plot with Regression Line and Equation",
       x = "CPI",
       y = "Weekly Sales") +
  theme_minimal()
model <- lm(Weekly_Sales ~ Fuel_Price, data = Df)
summary(model)
# Extract coefficients
intercept <- coef(model)[1]
slope <- coef(model)[2]

# Create Line Plot with Regression Line
ggplot(Df, aes(x = Fuel_Price, y = Weekly_Sales)) +
  geom_line(color = "blue", size = 1) +
  geom_abline(intercept = intercept, slope = slope, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Line Plot with Regression Line",
       x = "Fuel Price",
       y = "Weekly Sales") +
  theme_minimal()

ggplot(Df, aes(x = Unemployment, y = Weekly_Sales)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "loess", se = FALSE, color = "red", linetype = "dashed") +
  labs(title = "Line Plot with Smoothed Trend",
       x = "Unemployment",
       y = "Weekly Sales") +
  theme_minimal()

Df$Year <- substr(Df$Date, 7, 10)
ggplot(walmart_data, aes(x = Date, y = Weekly_Sales, color = factor(Holiday_Flag))) +
  geom_line() +
  labs(title = "Weekly Sales Trends Over Time",
       x = "Date",
       y = "Weekly Sales",
       color = "Holiday Flag") +
  scale_color_manual(values = c("#2d388a", "#00aeef"), labels = c("Non-holiday", "Holiday"))