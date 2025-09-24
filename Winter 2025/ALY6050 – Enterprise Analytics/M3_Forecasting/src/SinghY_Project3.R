#Author: Yash S
#Created: 2025-03-05
#Last Edited: 2025-03-11
#Class: ALY6050

cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100) # disables scientific notation for entire R session

library(pacman)
p_load(tidyverse, readxl, forecast, car, nortest)

# Load data
df <- read_excel("ALY6050_Module3Project_Data.xlsx", sheet = "6050_Module3Project_Data")
df <- df %>% drop_na()

# Rename columns for easier reference
colnames(df) <- c("Date", "Period", "AAPL", "AAPL_Volume", "HON", "HON_Volume")

# Convert Date to Date format
df$Date <- as.Date(df$Date)

# Plot AAPL and HON Prices
ggplot(df, aes(x = Date)) +
  geom_line(aes(y = AAPL, color = "Apple Inc")) +
  geom_line(aes(y = HON, color = "Honeywell Inc")) +
  labs(title = "Stock Prices of Apple Inc and Honeywell Inc", x = "Date", y = "Price ($)") +
  theme_minimal()

# Exponential Smoothing & Forecasting
alpha_values <- c(0.15, 0.35, 0.55, 0.75)
mape_results <- data.frame(alpha = numeric(), MAPE_AAPL = numeric(), MAPE_HON = numeric())

for (alpha in alpha_values) {
  model_aapl <- HoltWinters(df$AAPL, alpha = alpha, beta = FALSE, gamma = FALSE)
  model_hon <- HoltWinters(df$HON, alpha = alpha, beta = FALSE, gamma = FALSE)
  
  forecast_aapl <- forecast(model_aapl, h = 1)
  forecast_hon <- forecast(model_hon, h = 1)
  
  # Calculate MAPE
  mape_aapl <- mean(abs((tail(df$AAPL, 1) - forecast_aapl$mean) / tail(df$AAPL, 1))) * 100
  mape_hon <- mean(abs((tail(df$HON, 1) - forecast_hon$mean) / tail(df$HON, 1))) * 100
  
  mape_results <- rbind(mape_results, data.frame(alpha, MAPE_AAPL = mape_aapl, MAPE_HON = mape_hon))
}

# Find the best alpha
best_alpha_aapl <- mape_results$alpha[which.min(mape_results$MAPE_AAPL)]
best_alpha_hon <- mape_results$alpha[which.min(mape_results$MAPE_HON)]

# Adjusted Exponential Smoothing
beta_values <- c(0.15, 0.25, 0.45, 0.85)
mape_trend_results <- data.frame(beta = numeric(), MAPE_AAPL = numeric(), MAPE_HON = numeric())

for (beta in beta_values) {
  model_aapl_trend <- HoltWinters(df$AAPL, alpha = best_alpha_aapl, beta = beta, gamma = FALSE)
  model_hon_trend <- HoltWinters(df$HON, alpha = best_alpha_hon, beta = beta, gamma = FALSE)
  
  forecast_aapl_trend <- forecast(model_aapl_trend, h = 1)
  forecast_hon_trend <- forecast(model_hon_trend, h = 1)
  
  # Calculate MAPE
  mape_aapl_trend <- mean(abs((tail(df$AAPL, 1) - forecast_aapl_trend$mean) / tail(df$AAPL, 1))) * 100
  mape_hon_trend <- mean(abs((tail(df$HON, 1) - forecast_hon_trend$mean) / tail(df$HON, 1))) * 100
  
  mape_trend_results <- rbind(mape_trend_results, data.frame(beta, MAPE_AAPL = mape_aapl_trend, MAPE_HON = mape_hon_trend))
}

# Find the best beta
best_beta_aapl <- mape_trend_results$beta[which.min(mape_trend_results$MAPE_AAPL)]
best_beta_hon <- mape_trend_results$beta[which.min(mape_trend_results$MAPE_HON)]

# Print results
print(mape_results)
print(mape_trend_results)
cat("Best alpha for AAPL:", best_alpha_aapl, "\n")
cat("Best alpha for HON:", best_alpha_hon, "\n")
cat("Best beta for AAPL:", best_beta_aapl, "\n")
cat("Best beta for HON:", best_beta_hon, "\n")

#Part 2: 
# Extract AAPL and HON stock prices
dates <- df$Date
aapl_prices <- df$AAPL
hon_prices <- df$HON

# Function to calculate weighted moving average
weighted_moving_avg <- function(prices, weights) {
  n <- length(prices)
  wma <- numeric(n - 2)
  for (i in 3:n) {
    wma[i - 2] <- sum(prices[(i-2):i] * weights)
  }
  return(wma)
}

# Define weights
weights <- c(0.2, 0.3, 0.5)

# Compute 3-period weighted moving average for first 100 periods
wma_aapl <- weighted_moving_avg(aapl_prices[1:100], weights)
wma_hon <- weighted_moving_avg(hon_prices[1:100], weights)

# Extract actual prices for comparison
actual_aapl <- aapl_prices[3:100]
actual_hon <- hon_prices[3:100]

# Compute MAPE for weighted moving average
mape_wma_aapl <- mean(abs((actual_aapl - wma_aapl) / actual_aapl)) * 100
mape_wma_hon <- mean(abs((actual_hon - wma_hon) / actual_hon)) * 100

# Linear Trend Forecast for Periods 101-257
linear_trend_forecast <- function(prices, start_period, end_period) {  
  time_index <- seq_along(prices)  
  model <- lm(prices ~ time_index)  
  future_periods <- seq(start_period, end_period)  
  future_data <- data.frame(time_index = future_periods)  
  forecast <- predict(model, newdata = future_data)  
  return(forecast)  
}

# Generate forecasts for periods 101-257
forecast_aapl <- linear_trend_forecast(aapl_prices[1:101], 101, 257)
forecast_hon <- linear_trend_forecast(hon_prices[1:101], 101, 257)

# Extract actual prices for comparison (if available)
actual_aapl_future <- aapl_prices[101:257]
actual_hon_future <- hon_prices[101:257]

# Compute MAPE for linear trend
mape_trend_aapl <- mean(abs((actual_aapl_future - forecast_aapl) / actual_aapl_future)) * 100
mape_trend_hon <- mean(abs((actual_hon_future - forecast_hon) / actual_hon_future)) * 100

# Compare MAPE values
mape_comparison <- data.frame(
  Method = c("Exponential Smoothing", "Weighted Moving Average", "Linear Trend"),
  MAPE_AAPL = c(0.4596, mape_wma_aapl, mape_trend_aapl),
  MAPE_HON = c(0.8683, mape_wma_hon, mape_trend_hon)
)

print(mape_comparison)

#Part3
# Create a time variable
df$Time <- 1:nrow(df)

# Simple Regression for AAPL
lm_aapl <- lm(AAPL ~ Time, data = df)
summary(lm_aapl)  # Summary of regression model

# Simple Regression for HON
lm_hon <- lm(HON ~ Time, data = df)
summary(lm_hon)  # Summary of regression model

# Forecasting future periods (101 to 257)
future_periods <- data.frame(Time = 101:257)
forecast_aapl <- predict(lm_aapl, newdata = future_periods)
forecast_hon <- predict(lm_hon, newdata = future_periods)

# Residual Analysis

# Residuals
residuals_aapl <- residuals(lm_aapl)
residuals_hon <- residuals(lm_hon)

# Independence Check - Plot residuals vs time
ggplot(df, aes(x = Time, y = residuals_aapl)) +
  geom_point() + geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Residuals vs Time (Apple)", y= "Residuals")

ggplot(df, aes(x = Time, y = residuals_hon)) +
  geom_point() + geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Residuals vs Time (Honeywell)", y= "Residuals")

# Homoscedasticity Check - Residuals vs Fitted values
ggplot(df, aes(x = fitted(lm_aapl), y = residuals_aapl)) +
  geom_point() + geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Residuals vs Fitted (Apple)", x = "Fitted Values", y = "Residuals")

ggplot(df, aes(x = fitted(lm_hon), y = residuals_hon)) +
  geom_point() + geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Residuals vs Fitted (Honeywell)", x = "Fitted Values", y = "Residuals")

# Normality Check - QQ Plot
qqnorm(residuals_aapl, main = "Normal Q-Q Plot of Apple"); qqline(residuals_aapl, col = "red")
qqnorm(residuals_hon, main = "Normal Q-Q Plot of Honeywell"); qqline(residuals_hon, col = "red")

# Normality Test - Chi-Squared Test
shapiro.test(residuals_aapl)  # Shapiro-Wilk test for normality
shapiro.test(residuals_hon)

# Print results
print(data.frame(Period = 101:257, AAPL_Forecast = forecast_aapl, HON_Forecast = forecast_hon))

