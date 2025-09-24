#Authors: Yash S, Trusha S, Neer B
#Created: 2025-01-30
#Edited: 2025-02-08
#Course: ALY6015
#Final Project

cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notion for entire R session

library(pacman)
p_load(tidyverse, corrplot, RColorBrewer, caret, car, rcompanion, vcd, leaps, glmnet)

drugs <- read_csv("drugs_side_effects_drugs_com.csv")

# EDA 
summary(drugs) # 1345 NA values found, others exist but is not shown

# Key columns identified:
# Categorical Variables: drug_name, medical_condition, side_effects, drug_classes, rx_otc, pregnancy_category, csa, alcohol
# Numerical Variables: rating, no_of_reviews
# Missing values manually identified:
# side_effects, drug_classes, brand_names, rx_otc, pregnancy_category, alcohol, rating, and no_of_reviews.

# Imputing NA values
drugs_cleaned <- drugs %>%
  mutate(
    side_effects = ifelse(is.na(side_effects), "Unknown", side_effects),
    drug_classes = ifelse(is.na(drug_classes), "Unknown", drug_classes),
    rx_otc = ifelse(is.na(rx_otc), "Unknown", rx_otc),
    pregnancy_category = ifelse(is.na(pregnancy_category), "Unknown", pregnancy_category),
    alcohol = ifelse(is.na(alcohol), "Unkonwn", alcohol),
    )

# Selecting the key columns for analysis
drugs_cleaned <- drugs %>%
  select(
    drug_name, medical_condition, side_effects, drug_classes, rx_otc, pregnancy_category, csa, rating, no_of_reviews, activity
  ) %>% drop_na
summary(drugs_cleaned) # Checking if the missing values were dropped

# Visualizations
# Distribution of Ratings
ggplot(drugs_cleaned, aes(x = rating)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Drug Ratings", x = "Rating", y = "Frequency") +
  theme_minimal()

# Top 10 most common medical conditions
drugs_cleaned %>%
  count(medical_condition, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(medical_condition, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  coord_flip() +
  labs(title = "Top 10 Most Common Medical Conditions", x = "Medical Condition", y = "Count") +
  theme_minimal()

# Top 10 Drug Classes by average number of reviews
drugs_cleaned %>%
  group_by(drug_classes) %>%
  summarise(avg_reviews = mean(rating, na.rm = TRUE)) %>%
  top_n(10, avg_reviews) %>%
  ggplot(aes(x = reorder(drug_classes, avg_reviews), y = avg_reviews)) +
  geom_bar(stat = "identity", fill = "darkgreen", color = "black") +
  coord_flip() +
  labs(title = "Top 10 Drug Classes by Avg No of Reviews", 
       x = "Drug Class", y = "Average Number of Reviews") +
  theme_minimal()

# Boxplot of ratings by medical condition
ggplot(drugs_cleaned, aes(x = medical_condition, y = rating)) +
  geom_boxplot(fill = "salmon", color = "black") +
  labs(title = "Ratings by Medical Condition", x = "Medical Condition", y = "Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

# # A# Count the occurrences of each drug class
# top_10_counts <- head(sort(table(drugs_cleaned$drug_classes), decreasing = TRUE), 10)
# 
# # Create the bar plot
# ggplot(data.frame(Drug_Class = names(top_10_counts), Count = as.numeric(top_10_counts)),
#        aes(x = reorder(Drug_Class, Count), y = Count)) +
#   geom_bar(stat = "identity", fill = "skyblue") +
#   labs(title = "Top 10 Most Frequent Drug Classes",
#        x = "Drug Class",
#        y = "Count")+
#   theme_minimal() +
#   coord_flip()
################################################################################
# Questions to Explore:
# 1. Are there significant correlations between variables (Numerical variables)?
# Selecting the numeric columns for correlation analysis
numeric_data <- drugs_cleaned %>% select(rating, no_of_reviews)
# Calculating the correlation matrix
correlation_matrix <- cor(numeric_data, use = "complete.obs")
print(correlation_matrix)
# Plot of correlation matrix
corrplot(correlation_matrix, method = "color", 
         tl.cex = 0.7, 
         number.cex = 0.7, 
         col = brewer.pal(n = 8, name = "RdYlBu"))

# Scatter plot showing correlation between the Drug ratings and Number of Reviews
scatterplot(rating ~ no_of_reviews, data = drugs_cleaned, xlab = "Ratings (1-10)", ylab = "Number of Reviews (Count)", 
            main = "Scatter Plot of relationship between Ratings and Number of Reviews")

################################################################################
# 2. Is there a significant association between specific drug classes and medical conditions?
# Is there a significant association between specific drug classes and certain side effects?
# Correlation Analysis for categorical (nominal) variables using Cramér's V
# Converting the variables as factors
drugs_cleaned <- drugs_cleaned %>%
  mutate(
    medical_condition = as.factor(medical_condition),
    side_effects = as.factor(side_effects),
    drug_classes = as.factor(drug_classes)
  )

# Drug Classes vs. Medical Condition
# Chi-square Test
# a)
# Null Hypothesis (H0): There is no significant association between drug classes and medical condition.
# Alternative Hypothesis (H1): There is a significant association between drug classes and medical condition
# b)
# Degrees of Freedom = 10170
# Critical Value = 10353.2
# c)
# Creating a contingency table

# Chi-Square Value (X-squared) = 55067
# p-value < 0.00000000000000022
# d)
# Chi-Square Value (55067) > Critical Value (10769.83)
# p-value (< 0.00000000000000022) < α (0.10)
# e)
# Conclusion: 
# Since p-value (< 0.00000000000000022) is less than α = 0.10.
# We reject the Null Hypothesis. 
# There is evidence to claim that there is a significant association 
# between drug classes and medical conditions.

# Since Chi-squared approximation may be incorrect, we can further confirm this by 
# calculating Cramér's V for drug classes vs. medical condition 
v_test_drug_condition <- cramerV(chi_drug_condition_table)
print(paste("Cramér's V for Drug Classes vs. Medical Condition:", round(v_test_drug_condition, 3)))

# Drug Classes vs. Side Effect
# Chi-square Test
# a)
# Null Hypothesis (H0): There is no significant association between drug classes and side effects.
# Alternative Hypothesis (H1): There is a significant association between drug classes and side effects.
# b)
# Degrees of Freedom = 361998
# Critical Value = 363398.7 
# c)
# Creating a contingency table
chi_drug_effects_table <- table(drugs_cleaned$drug_classes, drugs_cleaned$side_effects)
# Chi-Square Independence test
chi_test_drug_effect <- chisq.test(chi_drug_effects_table)
chi_test_drug_effect
# Chi-Square Value (X-squared) = 368108
# p-value = 0.0000000000004621
# d)
# Chi-Square Value (368108) > Critical Value (363398.7 )
# p-value (0.0000000000004621) < α (0.05)
# e)
# Conclusion: 
# Since p-value (0.0000000000004621) is less than α = 0.05.
# We reject the Null Hypothesis due to lack of evidence. 
# There is a significant association between drug classes and side effects.

# Since Chi-squared approximation may be incorrect, we can further confirm this by 
# calculating Cramér's V for drug classes vs. side effects 
v_test_drug_effects <- cramerV(chi_drug_effects_table)
print(paste("Cramér's V for Drug Classes vs. Side Effects:", round(v_test_drug_effects, 3)))

################################################################################
# 3. Compare mean drug efficacy ratings across different drug categories to identify
# any significant variations.
# ANOVA
# a)
# H0: There is no difference in mean ratings among different drug classes.
# H1: There is a significant difference in mean ratings among different drug classes.
# b)
# Degree of Freedom Numerator = 226
# Degree of Freedom Denominator = 1193
# Critical Value = 1.13577
# c)
# One-way Anova
anova_drug_class <- aov(rating ~ drug_classes, data = drugs_cleaned)
summary(anova_drug_class)
# F-value = 2.505
# p-value <0.0000000000000002
# d)
# F-value(2.565) > Critical value(1.255)
# p-value (0.0000000000000002) < Significance level(0.05)
# e)
# Conclusion:
# Since F-value(2.565) is more than Critical value(1.255).
# We reject the Null Hypothesis 

################################################################################
# End of Preliminary Analysis
################################################################################
# Future Exploration:
# Predicting Drug Effectiveness (Rating)
# Model: Linear Regression 
# Regularization: LASSO (Least Absolute Shrinkage and Selection Operator)
# Response Variable: rating (continuous)
# Predictors: pregnancy_category, rx_otc 
# Use Case: Determine how different drug features impact their rating.
################################################################################
# Splitting the data into train and test set
# Maintaining a % of event rate 70/30 split
set.seed(123)
trainIndex <- createDataPartition(drugs_cleaned$rating, p = 0.65, list = FALSE, times = 1)
train_set <- drugs_cleaned[trainIndex, ]
test_set <- drugs_cleaned[-trainIndex, ]

# Selecting the key columns for analysis (these columns ensure no noise is introduced to the model)
drugs_cleaned <- drugs %>%
  select(
    rx_otc, pregnancy_category, csa, rating, 
  )

# Step-wise Feature Selection
model_step <- step(lm(rating ~ ., data = drugs_cleaned), direction = "both")
step_summary <- summary(model_step)
step_summary
step_summary$r.squared

# # All subset regression 
# best_subset <- regsubsets(rating ~., data = drugs_cleaned, nvmax = 3)
# reg_summary <- summary(best_subset)
# reg_summary
# # Best model by Mallow's Cp and BIC
# which.min(reg_summary$cp)
# which.max(reg_summary$adjr2)

# Plot diagnostic graphs for the regression model
par(mfrow = c(2, 2))
plot(model_step)
dev.off()

# # Check for multicollinearity using VIF
# # Now, running VIF function
# vif(model_step)

################################################################################
# LASSO Regression
x_train <- model.matrix(rating ~ ., data = train_set)[, -1]
y_train <- train_set$rating
x_test <- model.matrix(rating ~ ., data = test_set)[, -1]
y_test <- test_set$rating

# Finding best values for lambda
set.seed(123)
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1)  # Alpha = 1 for LASSO
# Comparing Lambda values
lambda_min_lasso <- lasso_model$lambda.min
lambda_1se_lasso <- lasso_model$lambda.1se
lambda_min_lasso; lambda_1se_lasso

# Plotting the LASSO regression model
plot(lasso_model)

# Fitting a LASSO regression model with minimum lambda value
lasso_fit <- glmnet(x_train, y_train, alpha = 1, lambda = lambda_min_lasso)
# Checking Coefficients
coef(lasso_fit)

# Determining RMSE for training set
pred_train_lasso <- predict(lasso_fit, s = lambda_min_lasso, newx = x_train)
rmse_train_lasso <- sqrt(mean((y_train - pred_train_lasso)^2))

# Determining RMSE for testing set
pred_test_lasso <- predict(lasso_fit, s = lambda_min_lasso, newx = x_test)
rmse_test_lasso <- sqrt(mean((y_test - pred_test_lasso)^2))

rmse_train_lasso; rmse_test_lasso

# Determining R-squared for training data
# Computing R-squared for LASSO Regression
ss_res_lasso_train <- sum((y_train - pred_train_lasso)^2)
ss_tot_lasso_train <- sum((y_train - mean(y_train))^2)
r2_train_lasso <- 1 - (ss_res_lasso_train / ss_tot_lasso_train)

# Determining R-squared for testing data
ss_res_lasso_test <- sum((y_test - pred_test_lasso)^2)
ss_tot_lasso_test <- sum((y_test - mean(y_test))^2)
r2_test_lasso <- 1 - (ss_res_lasso_test / ss_tot_lasso_test)

r2_train_lasso; r2_test_lasso

################################################################################
################################################################################
# End of Final Project