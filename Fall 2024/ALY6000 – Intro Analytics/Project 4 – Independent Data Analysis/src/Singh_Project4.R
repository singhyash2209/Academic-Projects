#This is the script for Project 4
#Author: Yash S
#Created On: 2024-10-07
#Last Edited: 2024-10-12
#Class: ALY6000

cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100) # disables scientific notation for entire R session

# Loading necessary libraries
library(pacman)
p_load(tidyverse)

# Part I – Exploring

# Loading the  dataset
data <- read_csv("leafly_strain_data.csv")

#1 Reviewing the dataset (Data dictionary)

str(data)
head(data)

#2 Cleaning the data

# a. Renaming columns (if needed)
# No specific column renaming required.

# b. Managing NAs
cleaned_data <- data %>%
  filter(!is.na(type))  # Remove rows where 'type' is NA

# c. Correcting data types
# Convert percentage columns from strings to numeric after removing "%"
percentage_columns <- c("thc_level", "relaxed", "happy", "euphoric", "uplifted", "sleepy", 
                        "dry_mouth", "dry_eyes", "dizzy", "paranoid", "anxious", "stress", 
                        "pain", "depression", "anxiety", "insomnia", "hungry", "talkative", 
                        "headache", "ptsd", "creative", "energetic", "fatigue", "focused", 
                        "giggly", "lack_of_appetite", "nausea", "headaches")

# Remove '%' and convert to numeric
cleaned_data[percentage_columns] <- lapply(cleaned_data[percentage_columns], 
                                           function(x) as.numeric(gsub("%", "", x)))

# d. Removing columns or rows 
cleaned_data <- select(cleaned_data, -img_url)

#3: Descriptive statistics
summary(cleaned_data$thc_level)  # Summary of THC levels
summary(cleaned_data$type)       # Count for each strain type

#4: Visualizations
# Bar plot for strain type distribution
ggplot(cleaned_data, aes(x = type)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Strain Type Distribution", x = "Strain Type", y = "Count") +
  theme_minimal()


#Part II – Expanding

#1 Create new variables
terpene_count <- cleaned_data %>%
  filter(!is.na(most_common_terpene)) %>%
  count(most_common_terpene) %>%
  arrange(desc(n))

#2 Group, summarize, rank
# Summarize average THC level by strain type
thc_by_type <- cleaned_data %>%
  group_by(type) %>%
  summarise(avg_thc = mean(thc_level, na.rm = TRUE))

#3 Visualizations of new insights

# Terpene Analysis:
# The bar chart will show the most common terpenes overall, 
# while the stacked bar chart will reveal how different terpenes are distributed across strain types.

# Bar chart for the most common terpenes across all strains
ggplot(terpene_count, aes(x = reorder(most_common_terpene, -n), y = n)) +
  geom_bar(stat = "identity", fill = "limegreen") +
  labs(title = "Most Common Terpenes Across Strains", x = "Terpene", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Stacked bar chart to see terpene distribution across strain types
ggplot(cleaned_data, aes(x = type, fill = most_common_terpene)) +
  geom_bar(position = "fill") +
  labs(title = "Terpene Distribution by Strain Type", x = "Strain Type", y = "Proportion") +
  theme_minimal() 

# THC Levels by Strain Type: 
# The following two visualizations (bar chart and boxplot) will help you compare
# THC levels across Indica, Sativa, and Hybrid strains.

# Bar chart to show average THC levels by strain type
ggplot(thc_by_type, aes(x = type, y = avg_thc, fill = type)) +
  geom_bar(stat = "identity") +
  labs(title = "Average THC Levels by Strain Type", x = "Strain Type", y = "Average THC Level (%)") +
  theme_minimal()

# Boxplot for THC levels by strain type
ggplot(cleaned_data, aes(x = type, y = thc_level, fill = type)) +
  geom_boxplot() +
  labs(title = "THC Levels Across Strain Types", x = "Strain Type", y = "THC Level (%)") +
  theme_minimal()

