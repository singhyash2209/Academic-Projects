# ----------------------------------------------
# International Education Dataset - SVM Classification
# ----------------------------------------------
# Author: Yash Singh
# Date: 2025-05-07
# Assignment 4 
# ALY6040

# Clear environment and console
cat("\014")
rm(list = ls())
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE)
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE)
options(scipen = 100)

# Load required libraries
library(pacman)
p_load(tidyverse, e1071, caret, pROC)

# ----------------------------------------------
# Step 1: Load and Prepare the Dataset
# ----------------------------------------------

# Load the dataset 
data <- read.csv("International_Education_Costs.csv")

# Feature engineering: calculate total cost
data$Total_Cost <- data$Tuition_USD + data$Rent_USD + data$Visa_Fee_USD + data$Insurance_USD

# Create binary target variable
median_cost <- median(data$Total_Cost, na.rm = TRUE)
data$High_Cost <- ifelse(data$Total_Cost > median_cost, 1, 0)
data$High_Cost <- as.factor(data$High_Cost)

# Drop unnecessary columns
data <- data %>%
  select(Country, Duration_Years, Tuition_USD, Rent_USD, Visa_Fee_USD,
         Insurance_USD, Exchange_Rate, High_Cost)

# View the summary of the dataset
summary(data)

# ----------------------------------------------
# Step 2: Data Preprocessing
# ----------------------------------------------

# Separate predictors and target
target <- data$High_Cost
data_no_target <- data %>% select(-High_Cost)

# One-hot encode categorical predictors
dummy_model <- dummyVars(~ ., data = data_no_target)
data_transformed <- predict(dummy_model, newdata = data_no_target)

# Combine encoded predictors with target variable
data_encoded <- data.frame(data_transformed, High_Cost = target)

# Normalize numeric columns
numeric_cols <- sapply(data_encoded, is.numeric)
data_encoded[numeric_cols] <- scale(data_encoded[numeric_cols])

# ----------------------------------------------
# Step 3: Train-Test Split
# ----------------------------------------------

set.seed(123)
train_index <- createDataPartition(data_encoded$High_Cost, p = 0.7, list = FALSE)
train_data <- data_encoded[train_index, ]
test_data <- data_encoded[-train_index, ]

# ----------------------------------------------
# Step 4: Train the SVM Model
# ----------------------------------------------

# Train with radial basis function kernel
svm_model <- svm(High_Cost ~ ., data = train_data, kernel = "radial", cost = 1, gamma = 0.1, probability = TRUE)

# ----------------------------------------------
# Step 5: Model Prediction and Evaluation
# ----------------------------------------------

# Predict on test data
pred <- predict(svm_model, test_data, probability = TRUE)
prob <- attr(pred, "probabilities")[,2]  # Get probability of class "1"

# Confusion Matrix
conf_matrix <- confusionMatrix(pred, test_data$High_Cost, positive = "1")
print(conf_matrix)

# ROC Curve and AUC
roc_obj <- roc(response = test_data$High_Cost, predictor = prob, levels = rev(levels(test_data$High_Cost)))
auc_value <- auc(roc_obj)
print(paste("AUC:", round(auc_value, 3)))

# Plot ROC Curve
plot(roc_obj, col = "blue", lwd = 2, main = "SVM ROC Curve - High Cost Classification")

# ----------------------------------------------
# Step 6: Interpretation and Recommendations
# ----------------------------------------------

# Based on confusion matrix and ROC:
# - Evaluate sensitivity vs specificity balance
# - Apply SMOTE or rebalancing to improve minority class recall
# - Tune cost and gamma hyperparameters via grid search
# - Recommend financial planning for countries with high predicted cost likelihood
