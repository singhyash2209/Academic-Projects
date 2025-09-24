#Author: Yash S
#Created: 2025-02-26
#Last Edited: 2025-03-04
#Class: ALY6050

#-------------------------------------------------------------------------------
cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100) # disables scientific notation for entire R session

library(pacman)
p_load(tidyverse, triangle, moments)
#-------------------------------------------------------------------------------

# Setting seed for reproducibility
set.seed(345)

#-------------------------------------------------------------------------------
# Part 1 Creation and Analysis of a Monte Carlo Simulation
# (i) Simulations
# Number of simulations
n <- 10000
# BENEFIT & COST RANGES FOR DAM #1 (Triangular Distribution)
# Benefits
benefits_dam1 <- rtriangle(n, 
                           a = (1.1 + 8 + 1.4 + 6.5 + 1.7 + 0),  # Min
                           b = (2.8 + 14.9 + 2.2 + 14.6 + 3.6 + 2.4), # Max
                           c = (2.0 + 12.0 + 1.4 + 9.8 + 2.4 + 1.6))  # Mode

# Costs 
costs_dam1 <- rtriangle(n, 
                        a = (13.2 + 3.5),  # Min
                        b = (19.1 + 7.4),  # Max
                        c = (14.2 + 4.9))  # Mode

# BENEFIT & COST RANGES FOR DAM #2 
# Benefits
benefits_dam2 <- rtriangle(n, 
                           a = (2.1 + 8.7 + 2.3 + 5.9 + 0 + 0),  # Min
                           b = (4.8 + 13.6 + 3.0 + 15.0 + 3.4 + 1.8), # Max
                           c = (3.0 + 12.2 + 3.0 + 8.7 + 3.4 + 1.2))  # Mode

# Costs
costs_dam2 <- rtriangle(n, 
                        a = (12.8 + 3.8),  # Min
                        b = (20.1 + 8.0),  # Max
                        c = (15.8 + 5.7))  # Mode

# BENIFIT-COST RATIOS
# Computing the benefit-cost ratios for Dam #1 & #2
alpha1 <- benefits_dam1 / costs_dam1
alpha2 <- benefits_dam2 / costs_dam2
#-------------------------------------------------------------------------------
# (ii) Frequency Distribution & Visualizations
# Create frequency distribution (binning into intervals)
alpha1_bins <- cut(alpha1, breaks = seq(min(alpha1), max(alpha1), length.out = 11), include.lowest = TRUE)
alpha1_freq <- table(alpha1_bins)

alpha2_bins <- cut(alpha2, breaks = seq(min(alpha2), max(alpha2), length.out = 11), include.lowest = TRUE)
alpha2_freq <- table(alpha2_bins)

# Convert to data frame for easy reporting
alpha1_freq_df <- tibble(Interval = names(alpha1_freq), Frequency = as.numeric(alpha1_freq))
alpha2_freq_df <- tibble(Interval = names(alpha2_freq), Frequency = as.numeric(alpha2_freq))

# Print frequency tables
# Frequency Distribution for α1
print(alpha1_freq_df)

# Frequency Distribution for α2
print(alpha2_freq_df)

# Plotting histograms for Dam 1 and Dam 2
ggplot(tibble(alpha1), aes(x = alpha1)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  labs(title = "Histogram of Benefit-Cost Ratio (Dam #1)",
       x = "Benefit-Cost Ratio",
       y = "Frequency") +
  theme_minimal()

ggplot(tibble(alpha2), aes(x = alpha2)) +
  geom_histogram(binwidth = 0.1, fill = "pink", color = "black") +
  labs(title = "Histogram of Benefit-Cost Ratio (Dam #2)",
       x = "Benefit-Cost Ratio",
       y = "Frequency") +
  theme_minimal()

# (iii) Descriptive Statistics of Dam #1 & #2
# Calculating the theoretical benefit and cost value based on most the likely values for Dam #1 & #2
theoretical_benefits_dam1 <- c(2, 12, 1.4, 9.8, 2.4, 1.6)
theoretical_costs_dam1 <- c(14.2, 4.9)
theoretical_benefits_dam2 <- c(3, 12.2, 3, 8.7, 3.4, 1.2)
theoretical_costs_dam2 <- c(15.8, 5.7)

# Computing the descriptive statistics for Dam #1 
descriptive_stats_1 <- tibble(
  Dam1_Project = c("Benefits(Mean)", "Benefits(SD)", "Costs(Mean)", "Costs(SD)",
                   "Benefit-Cost Ratio(Mean)", "Benefit-Cost Ratio(SD)"),
  Observed = c(mean(benefits_dam1), sd(benefits_dam1), mean(costs_dam1), sd(costs_dam1), 
           mean(alpha1), sd(alpha1)),
  Theoretical = c(mean(theoretical_benefits_dam1), sd(theoretical_benefits_dam1),
                  mean(theoretical_costs_dam1), sd(theoretical_costs_dam1), NA, NA)
)
print(descriptive_stats_1)

# Computing the descriptive statistics for Dam #1 
descriptive_stats_2 <- tibble(
  Dam2_Project = c("Benefits(Mean)", "Benefits(SD)", "Costs(Mean)", "Costs(SD)",
                   "Benefit-Cost Ratio(Mean)", "Benefit-Cost Ratio(SD)"),
  Observed = c(mean(benefits_dam2), sd(benefits_dam2), mean(costs_dam2), sd(costs_dam2), 
               mean(alpha2), sd(alpha2)),
  Theoretical = c(mean(theoretical_benefits_dam2), sd(theoretical_benefits_dam2),
                  mean(theoretical_costs_dam2), sd(theoretical_costs_dam2), NA, NA)
)
print(descriptive_stats_2)

#-------------------------------------------------------------------------------
# Part 2: Analysis of probability distribution
# Using bins from part 1
bins <- seq(min(alpha1), max(alpha1), length.out = 11)  
# Computing observed frequencies from simulated α1
freq_alpha1 <- table(cut(alpha1, breaks = bins, include.lowest = TRUE))

# Estimating log-normal parameters manually
log_alpha1 <- log(alpha1)
log_mu <- mean(log_alpha1)  # Estimated mean of log(alpha1)
log_sd <- sd(log_alpha1)    # Estimated standard deviation of log(alpha1)

# Generating theoretical α1 values using manually estimated log-normal parameters
alpha1_theoretical <- rlnorm(n, meanlog = log_mu, sdlog = log_sd)

# Computing expected frequencies using the same bins
freq_theoretical <- table(cut(alpha1_theoretical, breaks = bins, include.lowest = TRUE))

# Converting the tables to numeric vectors for chi-square test
observed_freq <- as.numeric(freq_alpha1)
expected_freq <- as.numeric(freq_theoretical)

# Normalizing expected frequencies to sum to 1 (as required for chisq.test())
expected_freq <- expected_freq / sum(expected_freq)

# Chi-Square Goodness-of-Fit test
# a)Null & Alternative Hypothesis
# Null Hypothesis (H0): The simulated frequency distribution does not differ from the theoretical frequency distribution.
# Alternative Hypothesis (H1): The simulated frequency distribution differs from the theoretical frequency distribution.
# b) Critical Value
# α = 0.05
# Degree of freedoms: 9
# Critical Value: 16.918
# c) Test Statistic
# Chi-Square Goodness of fit test
chisq_test <- chisq.test(x = observed_freq, p = expected_freq, rescale.p = TRUE)
# Displaying the results
chisq_test
list(Chi_Square_Statistic = chisq_test$statistic, P_Value = chisq_test$p.value)
# X-squared = 84.98564 
# p-value < 0.00000000000001642
# d) Interpretation
# Chi-Square Value (84.98564 ) > Critical Value (16.918)
# p-value (0.00000000000001642) < α (0.05)
# e) Conclusion:
# Since p-value (0.00000000000001642) is less than α = 0.05 &
# Chi-squared value (84.98564 ) is greater than the critical value (16.918);
# we reject the Null Hypothesis, due to strong of evidence. 
#-------------------------------------------------------------------------------
# Part 3: Comparison of the results.
# Summary statistics for alpha1 and alpha2
comparison_table <- tibble(
  Statistic = c("Minimum", "Maximum", "Mean", "Median", "Variance",
                "Standard Deviation", "Skewness"),
  Alpha1 = c(min(alpha1), max(alpha1), mean(alpha1), median(alpha1), var(alpha1), 
             sd(alpha1), moments::skewness(alpha1)),
  Alpha2 = c(min(alpha2), max(alpha2), mean(alpha2), median(alpha2), var(alpha2), 
             sd(alpha2), moments::skewness(alpha2))
)
print(comparison_table)

# Probability calculations
p_values <- c(2.0, 1.8, 1.5, 1.2, 1.0)
p_alpha1 <- sapply(p_values, function(x) mean(alpha1 > x))
p_alpha2 <- sapply(p_values, function(x) mean(alpha2 > x))

# Probability table
threshold <- tibble(Threshold = p_values, P_Alpha1 = p_alpha1, P_Alpha2 = p_alpha2) 

# Compute probability that alpha1 is greater than alpha2
p_alpha1_greater <- mean(alpha1 > alpha2)
print(paste("P(α1 > α2) =", round(p_alpha1_greater, 4)))
#-------------------------------------------------------------------------------