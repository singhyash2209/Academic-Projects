#Author: Yash s
#Created: 2025-01-25
#Edited: 2025-02-02
#Course: ALY6015

cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notion for entire R session

library(pacman)
p_load(tidyverse, ISLR, caret, pROC, corrplot, RColorBrewer, car, leaps)

# Loading the college dataset and saving it as dataframe
data("College")
college <- as.data.frame(College)
# write.csv(college, "college_data.csv", row.names = TRUE)

# Converting the Private column to a binary column depicting as 1 as private and 0 as public
# This is done for applying logistic regression
college <- college %>%
  mutate(
    Private = if_else(Private == "Yes", 1, 0),
    Private = as.factor(Private)
  )

# EDA on the college dataset
summary(college) # 0 NA
# Calculating the standard deviation for 'Outstate' and 'Expend'
sd_outstate <- sd(college$Outstate, na.rm = TRUE)
sd_expend <- sd(college$Expend, na.rm = TRUE)

cat("Standard Deviation for Outstate:", sd_outstate, "\n")
cat("Standard Deviation for Expend:", sd_expend, "\n")

# Visualizations
# Distribution of Out-of-state Tuition by private and public college
ggplot(college, aes(x = Outstate, fill = Private)) + 
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") + 
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal() +
  labs(
    title = "Distribution of Out-of-state Tuition by Private and Public Colleges",
    x = "Out-of-State Tuition (USD)",
    y = "Private(Red), Public(Blue)",
    fill = "Private(1), Public(0)"
  )
# Box plot of Out-of-state Tuition by private and public college
ggplot(data = college, aes(x = Private, y = Outstate, fill = Private)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("blue", "red"))+
  theme_minimal() +
  labs(
    title = "Out-of-state Tuition by Private and Public Colleges",
    x = "Private(1), Public(0)",
    y = "Out-of-State Tuition (USD)",
    fill = "Private(1), Public(0)"
  )
# Box plot of Full-time Undergraduates by private and public college
ggplot(college, aes(x = Private, y = F.Undergrad, fill = Private)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal() +
  labs(title = "Full-time Undergraduates by Private and Public Colleges",
    x = "Private(1), Public(0)",
    y = "Full-time Undergraduates (Count)",
    fill = "Private(1), Public(0)")

# Correlation matrix
# Selecting only numeric columns
num_cols <- college %>% select_if(is.numeric)
# Computing and plotting the correlation matrix
corr_matrix <- cor(num_cols)
corrplot(corr_matrix, method = "color", col = brewer.pal(n = 8, name = "RdBu"), 
         tl.cex = 0.8, cl.cex = 0.8, number.cex = 0.6, addCoef.col = "black")

# Checking correlation for all key predictors
# Apps
apps_corr  <- corr_matrix["Apps", ]
apps_corr <-sort(apps_corr, decreasing = TRUE)
apps_corr
high_corr_apps <- names(apps_corr[2])
high_corr_apps #Accept
# Scatter plot for highest correlated variable showing relationship between Accepted applications and Applications received
ggplot(college, aes(x = Accept, y = Apps, color = Private)) + 
  geom_point(alpha = 0.6) + 
  theme_minimal()  +
  labs(title = "Scatterplot of Applications Recieved vs. Accepted Applications",
       x = "No. of Accepted Applications (Count)",
       y = "No. of Applications recieved (Count)",
       color = "Private(1), Public(0)")
# F.Undergad
fundergrad_corr  <- corr_matrix["F.Undergrad", ]
fundergrad_corr <- sort(fundergrad_corr, decreasing = TRUE)
fundergrad_corr
high_corr_fundg <- names(fundergrad_corr[2])
high_corr_fundg #Enroll
# Scatter plot for highest correlated variable showing relationship between Expenditure (per student) and Out-of-state tuition
ggplot(college, aes(x = Enroll, y = F.Undergrad, color = Private)) + 
  geom_point(alpha = 0.6) + 
  theme_minimal()  +
  labs(title = "Scatterplot of Full-time Undergraduates vs. Students Enrolled",
       x = "No. of Students Enrolled (Count)",
       y = "No. of Full-time Undergraduates (Count)",
       color = "Private(1), Public(0)")
# Outstate
outstate_corr  <- corr_matrix["Outstate", ]
outstate_corr <- sort(outstate_corr, decreasing = TRUE)
outstate_corr
high_corr_outstate <- names(outstate_corr[2])
high_corr_outstate #Expend
# Scatter plot for highest correlated variable showing relationship between Expenditure (per student) and Out-of-state tuition
ggplot(college, aes(x = Expend, y = Outstate, color = Private)) + 
  geom_point(alpha = 0.6) + 
  theme_minimal()  +
  labs(title = "Scatterplot of Out-of-State Tuition vs. Expenditure (per student)",
       x = "Expenditure (per student)",
       y = "Out-of-State Tuition (USD)",
       color = "Private(1), Public(0)")
# PhD
phd_corr  <- corr_matrix["PhD", ]
phd_corr <-sort(phd_corr, decreasing = TRUE)
phd_corr
high_corr_phd <- names(phd_corr[2])
high_corr_phd # Terminal
# Scatter plot for highest correlated variable showing relationship between Terminal Degree Faculty and PhD Faculty
ggplot(college, aes(x = Terminal, y = PhD, color = Private)) + 
  geom_point(alpha = 0.6) + 
  theme_minimal()  +
  labs(title = "Scatterplot of PhD Faculty vs. Terminal Degree Faculty",
       x = "Faculty with Terminal Degrees (%)",
       y = "Faculty with a PhD (%)",
       color = "Private(1), Public(0)")


# Creating the Training and Testing dataset - maintaining a % of event rate 70/30 split
set.seed(123)
trainIndex <- createDataPartition(college$Private, p = 0.7, list = FALSE, times = 1)
train_set <- college[trainIndex, ]
test_set <- college[-trainIndex, ]

# Feature Selection - Step-wise Selection
model_step <- step(glm(Private ~ ., data = train_set, family = binomial), direction = "both")
summary(model_step)

# Creating an initial classifier model from the above step with the 5 predictors
initial_model <- glm(formula = Private ~ Apps + F.Undergrad + Outstate + 
      PhD + Expend, family = binomial, 
    data = train_set)
summary(initial_model)

# Plot diagnostic graphs for the regression model
par(mfrow = c(2, 2))
plot(initial_model)
dev.off()

# Check for multi-collinearity using VIF
vif(initial_model)

# Handling unusual observations
# Identifying Outliers
standardized_residuals <- rstandard(initial_model)
outlier_threshold <- 3
outliers <- which(abs(standardized_residuals) > outlier_threshold)
print(outliers)
# Visualizing outliers
plot(standardized_residuals, main = "Standardized Residuals", ylab = "Residuals", xlab = "Index")
abline(h = c(-outlier_threshold, outlier_threshold), col = "red", lty = 2)
text(outliers, standardized_residuals[outliers], labels = outliers, col = "blue", pos = 4)

# Identifying high-Leverage points
leverage <- hatvalues(initial_model)
leverage_threshold <- 2 * mean(leverage)
high_leverage <- which(leverage > leverage_threshold)
print(high_leverage)
# Visualizing high leverage points
plot(leverage, main = "Leverage Points", ylab = "Leverage", xlab = "Index")
abline(h = leverage_threshold, col = "red", lty = 2)
text(high_leverage, leverage[high_leverage], labels = high_leverage, col = "blue", pos = 4)

# Identifying influential observations
cooks <- cooks.distance(initial_model)
influential_threshold <- 4 / nrow(train_set)
influential_points <- which(cooks > influential_threshold)
print(influential_points)
# Visualizing influential points
plot(cooks, main = "Cook's Distance", ylab = "Cook's Distance", xlab = "Index")
abline(h = influential_threshold, col = "red", lty = 2)
text(influential_points, cooks[influential_points], labels = influential_points, col = "blue", pos = 4)

# Combining all unusual observations into a single vector
unusual_points <- sort(unique(c(high_leverage, outliers, influential_points)))
print(unusual_points)

# Removing the unusual observations
train_set_cleaned <- train_set[-unusual_points, ]

second_model <- glm(formula = Private ~ Apps + F.Undergrad + Outstate + 
                      PhD + Expend, family = binomial, 
                    data = train_set_cleaned)
summary(second_model)

# All subset regression 
best_subset <- regsubsets(Private ~., data = train_set_cleaned, nvmax = 5)
reg_summary <- summary(best_subset)
reg_summary
# Best model by Mallow's Cp and BIC
which.min(reg_summary$cp)
which.min(reg_summary$bic)

# Let us make the best model using 5 predictors
best_model <- glm(formula = Private ~ F.Undergrad + Outstate + 
      PhD + S.F.Ratio + Expend, family = binomial, 
    data = train_set_cleaned)
summary(best_model)

# Comparing all the models except the initial model since it is not of the same dataset
anova(second_model, best_model)
AIC(second_model, best_model)
BIC(second_model, best_model)

# Prediction on training set
train_pred <- predict(best_model, newdata = train_set_cleaned, type = "response")

# Converting probabilities to binary classification (0.5 threshold)
train_pred_class <- ifelse(train_pred > 0.5, 1, 0)

# Confusion Matrix for Training Set
conf_matrix_train <- table(Predicted = train_pred_class, Actual = train_set_cleaned$Private)
print(conf_matrix_train)

# Computing performance metrics
train_accuracy <- sum(diag(conf_matrix_train)) / sum(conf_matrix_train)
train_precision <- conf_matrix_train[2, 2] / sum(conf_matrix_train[, 2])
train_recall <- conf_matrix_train[2, 2] / sum(conf_matrix_train[2, ])
train_specificity <- conf_matrix_train[1, 1] / sum(conf_matrix_train[1, ])

# Prediction on testing set
test_pred <- predict(best_model, newdata = test_set, type = "response")

# Converting probabilities to binary classification
test_pred_class <- ifelse(test_pred > 0.5, 1, 0)

# Confusion Matrix for Testing Set
conf_matrix_test <- table(Predicted = test_pred_class, Actual = test_set$Private)
print(conf_matrix_test)

# Computing performance metrics for testing set set
test_accuracy <- sum(diag(conf_matrix_test)) / sum(conf_matrix_test)
test_precision <- conf_matrix_test[2, 2] / sum(conf_matrix_test[, 2])
test_recall <- conf_matrix_test[2, 2] / sum(conf_matrix_test[2, ])
test_specificity <- conf_matrix_test[1, 1] / sum(conf_matrix_test[1, ])

# Computing ROC curve
roc_curve <- roc(test_set$Private, test_pred)

# Plotting ROC Curve
plot(roc_curve, col = "blue", main = "ROC Curve for Logistic Regression Model")

# Computing AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))

################################################################################
# End of Assignment 3