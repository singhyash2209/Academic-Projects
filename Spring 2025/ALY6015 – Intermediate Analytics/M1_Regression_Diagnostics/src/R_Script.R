#Author: Yash S
#Created On: 2025-01-11
#Last Edited: 2025-01-18
#Course: ALY6015

cat("\014") # Clears console
rm(list = ls()) # Clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # Clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #Clears packages
options(scipen = 100) # Disables scientific notion for entire R session

# Loading necessary packages
library(pacman)
p_load(tidyverse, corrplot, RColorBrewer, car, leaps, janitor)

################################################################################

# Loading the dataset
ameshousing <- read_csv("AmesHousing.csv")

################################################################################

# Getting a sense of the dataset
str(ameshousing)
head(ameshousing)
glimpse(ameshousing)

# Checking for missing values
# colSums(is.na(ameshousing))
ameshousing %>% is.na() %>% colSums()
summary(ameshousing)

# Handling missing values 
ameshousing_cleaned <- ameshousing %>%
  mutate(
    # Imputing "Lot Frontage" with the median of the neighborhood, since the missing values indicate no street access
    `Lot Frontage` = ifelse(is.na(`Lot Frontage`), median(`Lot Frontage`, na.rm = TRUE), `Lot Frontage`), 
    
    # Replacing missing 'Categorical' data, since the missing indicates no garage/fireplace; impute as "None".
    Alley = replace_na(Alley, "None"),
    `Mas Vnr Type` = replace_na(`Mas Vnr Type`, "None"),
    `Fireplace Qu` = replace_na(`Fireplace Qu`, "None"),
    `Garage Qual` = replace_na(`Garage Qual`, "None"),
    `Garage Cond` = replace_na(`Garage Cond`, "None"),
    `Garage Type` = replace_na(`Garage Type`, "None"),
    `Garage Finish` = replace_na(`Garage Finish`, "None"),
    `Bsmt Qual` = replace_na(`Bsmt Qual`, "None"),
    `Bsmt Cond` = replace_na(`Bsmt Cond`, "None"),
    `Bsmt Exposure` = replace_na(`Bsmt Exposure`, "None"),
    `BsmtFin Type 1` = replace_na(`BsmtFin Type 1`, "None"),
    `BsmtFin Type 2` = replace_na(`BsmtFin Type 2`, "None"),
    Electrical = replace_na(Electrical, "None"),
    `Pool QC` = replace_na(`Pool QC`, "None"),
    Fence = replace_na(Fence, "None"),
    `Misc Feature`= replace_na(`Misc Feature`, "None"),
    
    # Replacing missing 'Numerical' values
    `Mas Vnr Area` = ifelse(is.na(`Mas Vnr Area`), 0, `Mas Vnr Area`),
    `BsmtFin SF 1` = ifelse(is.na(`BsmtFin SF 1`), 0, `BsmtFin SF 1`),
    `BsmtFin SF 2` = ifelse(is.na(`BsmtFin SF 2`), 0, `BsmtFin SF 2`),
    `Bsmt Unf SF` = ifelse(is.na(`Bsmt Unf SF`), 0, `Bsmt Unf SF`),
    `Total Bsmt SF` = ifelse(is.na(`Total Bsmt SF`), 0, `Total Bsmt SF`), 
    `Garage Cars` = ifelse(is.na(`Garage Cars`), 0, `Garage Cars`),
    `Garage Area` = ifelse(is.na(`Garage Area`), 0, `Garage Area`),
    `Garage Yr Blt`= ifelse(is.na(`Garage Yr Blt`), 0, `Garage Yr Blt`),
    `Bsmt Full Bath` = ifelse(is.na(`Bsmt Full Bath`), 0, `Bsmt Full Bath`),
    `Bsmt Half Bath` = ifelse(is.na(`Bsmt Half Bath`), 0, `Bsmt Half Bath`)
  ) 

# Checking for missing values in cleaned data
colSums(is.na(ameshousing_cleaned))
summary(ameshousing_cleaned)

# Cleaning column names using "Janitor" package
ameshousing_cleaned <- clean_names(ameshousing_cleaned)

# Visualizing variables
# Histogram for SalePrice
ggplot(ameshousing_cleaned, aes(x = sale_price)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of SalePrice",
       x = "Sale Price", y = "Frequency") +
  theme_minimal()

# Histogram for Gr Liv Area
ggplot(ameshousing_cleaned, aes(x = gr_liv_area)) +
  geom_histogram( fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Above-Ground Living Area",
       x = "Above-Ground Living Area", y = "Frequency") +
  theme_minimal()

# Histogram for Overall Qual
ggplot(ameshousing_cleaned, aes(x = overall_qual)) +
  geom_histogram(bins = 10, fill = "pink", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Overall Quality of the House",
       x = "Overall Quality", y = "Frequency") +
  theme_minimal()

# Boxplot & Scatterplot for Sale Price vs Gr Liv Area
scatterplot(gr_liv_area ~ sale_price,
            data = ameshousing_cleaned, 
            xlab = "Above-Ground Living Area", 
            ylab = "Sale Price", 
            main = "Scatter Plot of Sale Price vs Above-Ground Living Area")

# Scatterplot for Sale Price vs Overall Qual
scatterplot(overall_qual ~ sale_price, 
            data = ameshousing_cleaned, 
            xlab = "Overall Quality", 
            ylab = "Sale Price", 
            main = "Scatter Plot of Sale Price vs Overall Quality") 

################################################################################

# Correlation Matrix 
numeric_values <- ameshousing_cleaned %>%
  select(where(is.numeric))

cor_matrix <- cor(numeric_values, use = "complete.obs")
corrplot(cor_matrix, 
         method = "color",
         col = brewer.pal(n = 8, name = "RdYlBu"), # Choose a diverging color palette
         type = "lower",
         order = "hclust", # Order variables hierarchically
         addCoef.col = "black", # Add correlation coefficients
         tl.col = "black", # Color for labels
         tl.srt = 45, # Rotate labels
         title = "Correlation Plot with Color Brewer",
         mar = c(0, 0, 1, 0), # Adjust margins
         number.cex = 1.3,
         tl.cex = 2 #2.5
         )

# Identify variables for scatter plots
cor_with_price <- cor_matrix["sale_price", ]
highest_corr <- names(which.max(cor_with_price[-length(cor_with_price)]))  # Exclude sale_price itself
moderate_corr <- names(which.min(abs(cor_with_price - 0.5)))
lowest_corr <- names(which.min(abs(cor_with_price[cor_with_price != 0])))

# Highest correlation plot
ggplot(ameshousing_cleaned, aes_string(x = highest_corr, y = "sale_price")) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = paste("Scatter Plot: Overall Qual vs SalePrice"),
       x = highest_corr, y = "SalePrice")

# Moderate correlation plot
ggplot(ameshousing_cleaned, aes_string(x = moderate_corr, y = "sale_price")) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = paste("Scatter Plot: Masonry Veneer Type vs SalePrice"),
       x = moderate_corr, y = "SalePrice")

# Lowest correlation plot
ggplot(ameshousing_cleaned, aes_string(x = lowest_corr, y = "sale_price")) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "green", se = FALSE) +
  labs(title = paste("Scatter Plot: Basement Type 2 finished sq ft vs SalePrice"),
       x = lowest_corr, y = "SalePrice")

################################################################################

# Sorting the correlation matrix to choose strongest explanatory variables of Sale Price
cor_with_price <- sort(cor_with_price, decreasing = TRUE)
print(cor_with_price[2:4])

# Fitting a regression model with highest correlation variables
model_fit <- lm(formula = sale_price ~ overall_qual + gr_liv_area + garage_cars, data = ameshousing_cleaned)
summary(model_fit)

################################################################################

# Reviewing diagnostic plots 
# Residual vs Fitted - LINEARITY
# Q-Q Residual Plot - NORMALITY 
# Scale-Location - HOMOSCEDASTICITY
# Residual vs Leverage - UNUSUAL OBSERVATIONS 
par(mfrow = c(2,2))
plot(model_fit)
dev.off()

################################################################################

# Checking VIF values to check if multicollinearity exists
vif(model_fit)

################################################################################

# Handling unusual observations
# Identifying Outliers
standardized_residuals <- rstandard(model_fit)
outlier_threshold <- 3
outliers <- which(abs(standardized_residuals) > outlier_threshold)
print(outliers)
# Visualizing outliers
plot(standardized_residuals, main = "Standardized Residuals", ylab = "Residuals", xlab = "Index")
abline(h = c(-outlier_threshold, outlier_threshold), col = "red", lty = 2)
text(outliers, standardized_residuals[outliers], labels = outliers, col = "blue", pos = 4)

# Identifying high-Leverage points
leverage <- hatvalues(model_fit)
leverage_threshold <- 2 * mean(leverage)
high_leverage <- which(leverage > leverage_threshold)
print(high_leverage)
# Visualizing high leverage points
plot(leverage, main = "Leverage Points", ylab = "Leverage", xlab = "Index")
abline(h = leverage_threshold, col = "red", lty = 2)
text(high_leverage, leverage[high_leverage], labels = high_leverage, col = "blue", pos = 4)

# Identifying influential observations
cooks <- cooks.distance(model_fit)
influential_threshold <- 4 / nrow(ameshousing_cleaned)
influential_points <- which(cooks > influential_threshold)
print(influential_points)
# Visualizing influential points
plot(cooks, main = "Cook's Distance", ylab = "Cook's Distance", xlab = "Index")
abline(h = influential_threshold, col = "red", lty = 2)
text(influential_points, cooks[influential_points], labels = influential_points, col = "blue", pos = 4)

################################################################################

# Combining all unusual observations into a single vector
unusual_points <- unique(c(high_leverage, outliers, influential_points))

# Viewing unusual observations
unusual_data <- ameshousing_cleaned[unusual_points, ]
print(unusual_data)

# Removing the unsual observations
ameshousing_cleaned_updated <- ameshousing_cleaned[-unusual_points, ]

# Checking if model is improved
model_fit_updated <- lm(sale_price ~ overall_qual + gr_liv_area + garage_cars, data = ameshousing_cleaned_updated)
summary(model_fit_updated)

################################################################################

# All subsets regression method to identify the best model after handling outliers 
all_subsets <- regsubsets(sale_price ~ overall_qual + gr_liv_area + garage_cars
                                  + garage_area + total_bsmt_sf + x1st_flr_sf + year_built 
                                  + full_bath + year_remod_add + mas_vnr_area + tot_rms_abv_grd 
                                  + fireplaces + bsmt_fin_sf_1 + lot_frontage + wood_deck_sf 
                                  + open_porch_sf + half_bath + bsmt_full_bath + x2nd_flr_sf 
                                  + lot_area + garage_yr_blt + bsmt_unf_sf + bedroom_abv_gr 
                                  + screen_porch + pool_area + mo_sold + x3ssn_porch
                                  + misc_val + yr_sold + order + bsmt_half_bath 
                                  + overall_cond + kitchen_abv_gr + enclosed_porch, data = ameshousing_cleaned_updated,
                                  nvmax = 10, nbest = 3)
plot(all_subsets, scale = "adjr2")
summary(all_subsets)

################################################################################

ames_final_data <- ameshousing_cleaned_updated %>%
  select(sale_price, overall_qual, gr_liv_area, bsmt_fin_sf_1)

# Final model selection based on all subset regression
final_model <- lm(formula = sale_price ~ overall_qual + gr_liv_area 
                  +  bsmt_fin_sf_1  , data = ames_final_data)
summary(final_model)

# Comparing all models to find best model

# Adjusted R^2
adj_r2_initial <- summary(model_fit)$adj.r.squared
adj_r2_updated <- summary(model_fit_updated)$adj.r.squared
adj_r2_final <- summary(final_model)$adj.r.squared

# AIC and BIC
aic_initial <- AIC(model_fit)
bic_initial <- BIC(model_fit)

aic_updated <- AIC(model_fit_updated)
bic_updated <- BIC(model_fit_updated)

aic_final <- AIC(final_model)
bic_final <- BIC(final_model)

# Output comparison
comparison <- tibble(
  Model = c("Initial Model", "Updated Model", "Final Model"),
  Adj_R2 = c(adj_r2_initial, adj_r2_updated, adj_r2_final),
  AIC = c(aic_initial, aic_updated, aic_final),
  BIC = c(bic_initial, bic_updated, bic_final)
)
print(comparison)
################################################################################
#End of Assignment 1

