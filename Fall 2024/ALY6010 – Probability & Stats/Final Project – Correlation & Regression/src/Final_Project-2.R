#This is script for Final Project — Milestone 1 
#Author: Yash S
#Created on: 2024-11-05
#Last Edited: 2024-12-12
#Class: ALY6010

cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100) # disables scientific notation for entire R session

# Load "pacman" to load/install packages easily 
library(pacman)
p_load(tidyverse, readxl, caret)

# Loading the data set
casino <- read_excel("toronto-casino-survey-results.xlsx")

# Getting a sense of the data

str(casino) # Viewing the structure of the data 
head(casino) # Viewing the first 6 rows of the data
colnames(casino) # Viewing the 94 Column names in the data set
colSums(is.na(casino)) # This counts the number of NA in each column
summary(casino) # Checking the data set

# Cleaning data for further analysis
clean_casino <- casino %>%
  select(
    Sentiment_Analysis = Q1_A,
    Image_Fit = Q2_A,
    Revenue_Influence = Q4_A,
    Preferred_Location = Q6,
    Considered_Location = ,
    Gender = Gender
  ) %>%
  filter(
    Gender %in% c("Male", "Female"),
    Preferred_Location %in% c("City of Toronto", "Adjacent Municipality")
    ) %>%
  drop_na()  # Drop rows with any NA values

# Converting categorical fields to factors for better analysis
clean_casino <- clean_casino %>%
  mutate(
    Sentiment_Analysis = as.factor(Sentiment_Analysis),
    Image_Fit = as.factor(Image_Fit),
    Revenue_Influence = as.factor(Revenue_Influence),
    Preferred_Location = as.factor(Preferred_Location),
    Gender = as.factor(Gender)
  )

str(clean_casino) # Viewing the structure of the cleaned data 
head(clean_casino) # Viewing the first 6 rows of the cleaned data
colnames(clean_casino) # Viewing the column names in the cleaned data set
colSums(is.na(clean_casino)) # This counts the number of NA in each column
summary(clean_casino) # Checking the cleaned data set

# Descriptive Statistics
# Summary statistics for each field
summary_stats <- clean_casino %>%
  summarise(
    Sentiment_Count = n_distinct(Sentiment_Analysis),
    Image_Fit_Count = n_distinct(Image_Fit),
    Revenue_Influence_Count = n_distinct(Revenue_Influence),
    Preferred_Location_Count = n_distinct(Preferred_Location),
    Gender_Count = n_distinct(Gender)
  )
print(summary_stats)

# Frequency Tables
sentiment_table <- table(clean_casino$Sentiment_Analysis)
print("Frequency Table for Sentiment Analysis:")
print(sentiment_table)
# Image Fit
image_fit_table <- table(clean_casino$Image_Fit)
print("Frequency Table for Image Fit:")
print(image_fit_table)
# Revenue Influence
revenue_influence_table <- table(clean_casino$Revenue_Influence)
print("Frequency Table for Revenue Influence:")
print(revenue_influence_table)
# Preferred Location
preferred_location_table <- table(clean_casino$Preferred_Location)
print("Frequency Table for Preferred Location:")
print(preferred_location_table)
# Gender Distribution
gender_table <- table(clean_casino$Gender)
print("Frequency Table for Gender:")
print(gender_table)

# Data Visualization 

# 1. Sentiment Analysis Distribution
ggplot(clean_casino, aes(x = Sentiment_Analysis)) +
  geom_bar(fill = "blue", colour = "black") +
  labs(title = "Sentiment Analysis Distribution", x = "Sentiment", y = "Count",) +
  theme_minimal() +
  coord_flip()

# 2. Gender Distribution
ggplot(clean_casino, aes(x = Gender)) +
  geom_bar(fill = "green", colour = "black") +
  labs(title = "Gender Distribution", x = "Gender", y = "Count") +
  theme_minimal()

# 3. Preferred Location Distribution
ggplot(clean_casino, aes(x = Preferred_Location)) +
  geom_bar(fill = "orange") +
  labs(title = "Preferred Location Distribution", x = "Location", y = "Count") +
  theme_minimal()

# Subset Analysis
# Analyze sentiments by gender
sentimentXgender <- clean_casino %>%
  group_by(Gender, Sentiment_Analysis) %>%
  summarise(Count = n(), .groups = "drop")

# Cross-tabulation: Gender vs Sentiment Analysis
gender_sentiment_crosstab <- table(clean_casino$Gender, clean_casino$Sentiment_Analysis)
print("Cross-tabulation of Gender and Sentiment Analysis:")
print(gender_sentiment_crosstab)

# Count: Gender and Sentiment Analysis
gender_sentiment_count <- clean_casino %>%
  count(Gender, Sentiment_Analysis)
print("Count of Gender and Sentiment Analysis:")
print(gender_sentiment_count)

# Visualization: Sentiments by Gender
ggplot(sentimentXgender, aes(x = Sentiment_Analysis, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Sentiment by Gender", x = "Sentiment", y = "Count") +
  coord_flip()

#-------------------------------------------------------------------------------
# This is script for Final Project — Milestone 2 
#-------------------------------------------------------------------------------

# Question 1: Is the overall public sentiment towards establishing a new casino in Toronto neutral, or is it not neutral (positive/negative)?
# Map sentiment levels to numerical values
sentiment_mapping <- c(
  "Neutral or Mixed Feelings" = 0,
  "Somewhat in Favour" = 1, 
  "Somewhat Opposed" = -1, 
  "Strongly in Favour" = 2, 
  "Strongly Opposed" = -2)
clean_casino$Sentiment_Numeric <- as.numeric(sapply(clean_casino$Sentiment_Analysis, function(x) sentiment_mapping[x]))
# Null Hypothesis (H0): The mean sentiment score is exactly neutral (0).
# Alternative Hypothesis (H1): The mean sentiment score is either positive(>0) or negative(<0).
# significance_level <- 0.05
# Compute test statistic
sentiment_t_test <- t.test(clean_casino$Sentiment_Numeric, mu = 0, alternative = "two.sided")
print(sentiment_t_test)
# p-value < 0.00000000000000022
# Final Conclusion: 
# Since p-value is less then significance level(0.05), we reject the Null Hypothesis (H0)

# Question 2: Is the overall public sentiment towards establishing a new casino in Toronto positive or is it negative?
# Null Hypothesis (H0): The mean sentiment score is positive  (> 0).
# Alternative Hypothesis (H1): The mean sentiment score is negative (< 0).
# significance_level <- 0.05
# Compute test statistic
sentiment_negative_test <- t.test(clean_casino$Sentiment_Numeric, mu = 0, alternative = "less")
print(sentiment_negative_test)
# p-value < 0.00000000000000022
# Final Conclusion: 
# Since p-value is less then significance level(0.05), we reject the Null Hypothesis (H0)

# Question 3: Is the casino more preferred in the City of Toronto than in adjacent municipalities or "neither"?
# Further cleaning the data by removing rows where Preferred_Location is "Neither"
location_filtered <- clean_casino %>%
  filter(Preferred_Location %in% c("Adjacent Municipality", "City of Toronto"))
# Frequency table for "City of Toronto" and "Adjacent Municipality"
location_preference_filtered_table <- table(location_filtered$Preferred_Location)
city_count <- location_preference_filtered_table["City of Toronto"]
adj_muni_count <- location_preference_filtered_table["Adjacent Municipality"]
total_count <- sum(location_preference_filtered_table)
# Null Hypothesis (H0): Adjacent Municipality is significantly more preferred than City of Toronto.
# Alternative Hypothesis (H1): City of Toronto is significantly more preferred than Adjacent Municipality.
# significance_level <- 0.05
# Compute test statistic
city_vs_adj_muni_test <- prop.test(c(city_count, adj_muni_count), n = c(total_count, total_count), p = c(0.5, 0.5))
print(city_vs_adj_muni_test)
# p-value < 0.00000000000000022
# Final Conclusion: 
# Since p-value is less then significance level(0.05), we reject the Null Hypothesis (H0)

#-------------------------------------------------------------------------------
# This is script for Final Project 
#-------------------------------------------------------------------------------

# Dummy Encoding 
clean_casino <- clean_casino %>%
  mutate(
    Revenue_Influence_Numeric = ifelse(Revenue_Influence == "Yes", 1, 0),
    Fits_Perfectly = ifelse(Image_Fit == "Fits Image Perfectly", 1, 0),
    Fits_Somewhat = ifelse(Image_Fit == "Fits Image Somewhat", 1, 0),
    Neutral_Not_Sure = ifelse(Image_Fit == "Neutral / I am Not Sure", 1, 0),
    City_of_Toronto = ifelse(Preferred_Location == "City of Toronto", 1, 0),
    Adjacent_Municipality = ifelse(Preferred_Location == "Adjacent Municipality", 1, 0)
    ) %>%
  select(-c(Revenue_Influence, Image_Fit, Preferred_Location)) # Dropping Columns to avoid multi collinearity 

# Question 1: Does the revenue influence predict public sentiment towards the casino?
# Null Hypothesis (H0): Revenue influence does not predict public sentiment (no significant relationship).
# Alternative Hypothesis (H1): Revenue influence predicts public sentiment (significant relationship).
# significance_level <- 0.05
# Linear Regression
model_location_sentiment <- lm(
  formula = City_of_Toronto ~ Sentiment_Numeric, 
  data = clean_casino
  )
summary(model_location_sentiment)
# p-value < 0.00000000000000022
# Final Conclusion: 
# Since p-value is less then significance level(0.05), we reject the Null Hypothesis (H0)
# Scatter plot with regression line
ggplot(clean_casino, aes(x = Revenue_Influence_Numeric, y = Sentiment_Numeric)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Public Sentiment vs. Revenue Influence",
       x = "Revenue Influence (Dummy: Yes = 1, No = 0)",
       y = "Public Sentiment") +
  theme_minimal()

# Question 2: Does the perception of image fit predict public sentiment toward the casino?
# Null Hypothesis (H0):  Perception of image fit does not predict public sentiment (no significant relationship).
# Alternative Hypothesis (H1): Perception of image fit predicts public sentiment (significant relationship).
# significance_level <- 0.05
# Linear Regression
model_image_sentiment <- lm(
  formula = Sentiment_Numeric ~ Fits_Perfectly + Fits_Somewhat + Neutral_Not_Sure, 
  data = clean_casino
  )
summary(model_image_sentiment)
# p-value < 0.00000000000000022
# Final Conclusion: 
# Since p-value is less then significance level(0.05), we reject the Null Hypothesis (H0)
# Scatter plot with regression line for one level ("Fits Image Perfectly")
ggplot(clean_casino, aes(x = Fits_Perfectly, y = Sentiment_Numeric)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Public Sentiment vs. Image Fit (Fits Perfectly)",
       x = "Fits Image Perfectly (Dummy: Yes = 1, No = 0)",
       y = "Public Sentiment") +
  theme_minimal()

# Question 3: Does Sentiment Predict Preference for the City of Toronto?
# Null Hypothesis (H0):  Sentiment does not predict preference for the City of Toronto.
# Alternative Hypothesis (H1): Sentiment predicts preference for the City of Toronto.
# significance_level <- 0.05
# Linear Regression
model_toronto_sentiment <- lm(
  formula = City_of_Toronto ~ Sentiment_Numeric, 
  data = clean_casino
)
summary(model_toronto_sentiment)
# p-value < 0.00000000000000022
# Final Conclusion: 
# Since p-value is less then significance level(0.05), we reject the Null Hypothesis (H0)
# Scatter plot with regression line for one level ("Fits Image Perfectly")
ggplot(clean_casino, aes(x = City_of_Toronto, y = Sentiment_Numeric)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(title = "City of Toronto Preference vs. Public Sentiment",
       x = "City of Toronto Preference (Dummy: Yes = 1, No = 0)",
       y = "Public Sentiment"
  ) +
  theme_minimal()
