#Authors: Yash S
#Created: 2025-02-01
#Edited: 2025-02-09
#Course: ALY6015
#Assignment 4

cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notion for entire R session

library(pacman)
p_load(tidyverse, caret, glmnet, ISLR, car, leaps)

# Loading the college dataset and saving it as dataframe
data("College")
college <- as.data.frame(College)
# To build regularization models by using Ridge and Lasso (least absolute shrinkage and selection operator).
# Predict Grad.Rate for all models.

# EDA on the college dataset
summary(college) # 0 NA

# 1. Splitting the data into train and test set
# Maintaining a % of event rate 70/30 split
set.seed(123)
trainIndex <- createDataPartition(college$Grad.Rate, p = 0.7, list = FALSE, times = 1)
train_set <- college[trainIndex, ]
test_set <- college[-trainIndex, ]

x_train <- model.matrix(Grad.Rate ~ ., data = train_set)[, -1]
y_train <- train_set$Grad.Rate
x_test <- model.matrix(Grad.Rate ~ ., data = test_set)[, -1]
y_test <- test_set$Grad.Rate

################################################################################
# Ridge Regression
# 2. Finding best values for lambda
set.seed(123)
ridge_model <- cv.glmnet(x_train, y_train, alpha = 0) # Alpha = 0 for Ridge
# Comparing Lambda values
lambda_min_ridge <- ridge_model$lambda.min
lambda_1se_ridge <- ridge_model$lambda.1se
lambda_min_ridge; lambda_1se_ridge

# 3. Plotting the Ridge regression model
plot(ridge_model)

# 4. Fitting a Ridge regression model with minimum lambda value
ridge_fit <- glmnet(x_train, y_train,alpha = 0, lambda = lambda_min_ridge) 
# Checking Coefficients
coef(ridge_fit)

# 5. Determining RMSE for training set
pred_train_ridge <- predict(ridge_fit, s = lambda_min_ridge, newx = x_train)
rmse_train_ridge <- sqrt(mean((y_train - pred_train_ridge)^2))

# 6. Determining RMSE for testing set
pred_test_ridge <- predict(ridge_fit, s = lambda_min_ridge, newx = x_test)
rmse_test_ridge <- sqrt(mean((y_test - pred_test_ridge)^2))

rmse_train_ridge; rmse_test_ridge

# Determining R-squared for training and testing data
# Computing R-squared for Ridge Regression
# Training Data
ss_res_ridge_train <- sum((y_train - pred_train_ridge)^2)
ss_tot_ridge_train <- sum((y_train - mean(y_train))^2)
r2_train_ridge <- 1 - (ss_res_ridge_train / ss_tot_ridge_train)
# Testing Data
ss_res_ridge_test <- sum((y_test - pred_test_ridge)^2)
ss_tot_ridge_test <- sum((y_test - mean(y_test))^2)
r2_test_ridge <- 1 - (ss_res_ridge_test / ss_tot_ridge_test)

r2_train_ridge; r2_test_ridge
################################################################################
# LASSO Regression
# 7. Finding best values for lambda
set.seed(123)
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1)  # Alpha = 1 for LASSO
# Comparing Lambda values
lambda_min_lasso <- lasso_model$lambda.min
lambda_1se_lasso <- lasso_model$lambda.1se
lambda_min_lasso; lambda_1se_lasso

# 8. Plotting the LASSO regression model
plot(lasso_model)

# 9. Fitting a LASSO regression model with minimum lambda value
lasso_fit <- glmnet(x_train, y_train, alpha = 1, lambda = lambda_min_lasso)
# Checking Coefficients
coef(lasso_fit)

# 10. Determining RMSE for training set
pred_train_lasso <- predict(lasso_fit, s = lambda_min_lasso, newx = x_train)
rmse_train_lasso <- sqrt(mean((y_train - pred_train_lasso)^2))

# 11. Determining RMSE for testing set
pred_test_lasso <- predict(lasso_fit, s = lambda_min_lasso, newx = x_test)
rmse_test_lasso <- sqrt(mean((y_test - pred_test_lasso)^2))

rmse_train_lasso; rmse_test_lasso

# Determining R-squared for training and testing data
# Computing R-squared for LASSO Regression
# Compute R² for Lasso Regression
ss_res_lasso_train <- sum((y_train - pred_train_lasso)^2)
ss_tot_lasso_train <- sum((y_train - mean(y_train))^2)
r2_train_lasso <- 1 - (ss_res_lasso_train / ss_tot_lasso_train)

ss_res_lasso_test <- sum((y_test - pred_test_lasso)^2)
ss_tot_lasso_test <- sum((y_test - mean(y_test))^2)
r2_test_lasso <- 1 - (ss_res_lasso_test / ss_tot_lasso_test)

# Output R² values
r2_train_lasso; r2_test_lasso
################################################################################
# Comparison
# 13. Step-wise Feature Selection
model_step <- step(lm(Grad.Rate ~ ., data = train_set), direction = "both")
step_summary <- summary(model_step)
step_summary
step_summary$r.squared

# Plot diagnostic graphs for the regression model
par(mfrow = c(2, 2))
plot(model_step)
dev.off()

# Check for multicollinearity using VIF
# Now, running VIF function
vif(model_step)

# Handling unusual observations
# Identifying Outliers
standardized_residuals <- rstandard(model_step)
outlier_threshold <- 3
outliers <- which(abs(standardized_residuals) > outlier_threshold)
print(outliers)
# Visualizing outliers
plot(standardized_residuals, main = "Standardized Residuals", ylab = "Residuals", xlab = "Index")
abline(h = c(-outlier_threshold, outlier_threshold), col = "red", lty = 2)
text(outliers, standardized_residuals[outliers], labels = outliers, col = "blue", pos = 4)

# Identifying high-Leverage points
leverage <- hatvalues(model_step)
leverage_threshold <- 2 * mean(leverage)
high_leverage <- which(leverage > leverage_threshold)
print(high_leverage)
# Visualizing high leverage points
plot(leverage, main = "Leverage Points", ylab = "Leverage", xlab = "Index")
abline(h = leverage_threshold, col = "red", lty = 2)
text(high_leverage, leverage[high_leverage], labels = high_leverage, col = "blue", pos = 4)

# Identifying influential observations
cooks <- cooks.distance(model_step)
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

cleaned_model_step <- lm(Grad.Rate ~  Private + Apps + Top25perc + P.Undergrad +
                    Outstate + Room.Board + Personal + perc.alumni + Expend,
                    data = train_set_cleaned)
cleaned_summary <- summary(cleaned_model_step)
cleaned_summary

step_summary$adj.r.squared; cleaned_summary$adj.r.squared


# All subset regression 
best_subset <- regsubsets(Grad.Rate ~., data = train_set_cleaned, nvmax = 9)
reg_summary <- summary(best_subset)
reg_summary
# Best model by Mallow's Cp and BIC
which.min(reg_summary$cp)
which.max(reg_summary$adjr2)

# Since all subset regression confirms that step-wise feature selected model is the best model
# We evaluate the cleaned best model
# Predictions on Training Set
train_pred <- predict(cleaned_model_step, newdata = train_set_cleaned)

# Computing performance metrics for Training Set
train_mse <- mean((train_set_cleaned$Grad.Rate - train_pred)^2)
train_rmse <- sqrt(train_mse)
train_r2 <- 1 - (sum((train_set_cleaned$Grad.Rate - train_pred)^2) / sum((train_set_cleaned$Grad.Rate - mean(train_set_cleaned$Grad.Rate))^2))

# Predictions on Testing Set
test_pred <- predict(cleaned_model_step, newdata = test_set)

# Computing performance metrics for Testing Set
test_mse <- mean((test_set$Grad.Rate - test_pred)^2)
test_rmse <- sqrt(test_mse)
test_r2 <- 1 - (sum((test_set$Grad.Rate - test_pred)^2) / sum((test_set$Grad.Rate - mean(test_set$Grad.Rate))^2))

# Training Set & Testing Set performance metrics
train_mse; test_mse
train_rmse; test_rmse
train_r2; test_r2

# Plotting Residuals for Training Set
plot(train_set_cleaned$Grad.Rate, train_pred, xlab = "Actual Grad Rate", ylab = "Predicted Grad Rate", main = "Training Set: Actual vs Predicted Grad Rate")
abline(0, 1, col = "red")

# Plotting Residuals for Testing Set
plot(test_set$Grad.Rate, test_pred, xlab = "Actual Grad Rate", ylab = "Predicted Grad Rate", main = "Testing Set: Actual vs Predicted Grad Rate")
abline(0, 1, col = "red")

################################################################################
AIC(model_step); AIC(cleaned_model_step)
BIC(model_step); BIC(cleaned_model_step)

