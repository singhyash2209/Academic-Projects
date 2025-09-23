#This is the script for Project 3
#Author: Yash S
#Created On: 2024-09-30
#Last Edited: 2024-10-05
#Class: ALY6000

cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100) # disables scientific notation for entire R session

library(pacman)
p_load(tidyverse)
p_load(janitor)
p_load(lubridate)
p_load(ggthemes)
p_load(ggeasy)
p_load(tibble)
p_load(testthat)

#1
#Loads the data set 
books <- read_csv("books.csv")

#Cleaning the data set 

#1
#Cleans the name of the columns
books <-clean_names(books)

#2
books <- books %>%
  mutate(first_publish_date = mdy(first_publish_date))

#3
books <- books %>%
  mutate(year = year(first_publish_date))

#4
books <- books %>% 
  filter(year >= 1990 & year <= 2020)

#5
books <- books %>%
  select(-publish_date, -edition, -characters, -price, -genres, -setting, -isbn)


#6
books <- books %>%
  filter(pages<1200)

#Data Analysis

#1
glimpse(books)

#2 
summary(books)

#3
ggplot(books, aes(x = rating)) +
  geom_histogram(binwidth = 0.25, fill = "Red") +
  labs(x= "Rating", y="Number of Books", title = "Histogram of Book Ratings") +
  theme_bw()

#4
ggplot(books, aes(x = pages)) +
  geom_boxplot(fill = "Magenta") +
  labs(x = "Pages", title = "Box Plot of Page Counts") +
  theme_economist()
 
#5
book_publishers <- books %>%
  group_by(publisher) %>%
  summarize(total_books = n())%>%
  filter(!is.na(publisher) & total_books>250) %>%
  arrange(desc(total_books)) %>%
  mutate(
    publisher = factor(publisher, levels = publisher), 
    cum_count = cumsum(total_books), 
    rel_freq = total_books/ sum(total_books),
    cum_freq = cumsum(rel_freq)
    )

#6
ggplot(book_publishers, aes(x = publisher, y = total_books)) +
  geom_bar(fill = "Cyan", stat = "identity") +
  geom_line(aes(y=cum_count), color = "Black",group=1,  size = 1) +
  geom_point(aes(y=cum_count), color = "Black", size = 2)+
  labs(x="Publisher", y="Number of Books", title = "Pareto and Ogive of Publisher Book Counts (1990 - 2020)")+
  theme_clean()+
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) 

#7
ggplot(books, aes(x=pages, y=rating, colour = year))+
  geom_point(alpha = 0.6)+
  labs(x="Pages", y="Rating", title = "Scatter Plot of Pages vs. Rating")+
  theme_tufte()

#8
by_year <- books %>%
  group_by(year) %>%
  summarise(total_books = n(), avg_rating = mean(rating, na.rm = TRUE))
  
# #9
ggplot(by_year, aes(x = year, y = total_books)) +
  geom_line() +
  geom_point(aes(color = avg_rating)) +
  labs(title = "Total Number of Books Rated Per Year", x = "Year", y = "Number of Books") +
  theme_excel_new()

#10
average <- function(x){
  n <- length(x[!is.na(x)])
  return(sum(x, na.rm = TRUE)/n)
}

pop_var <- function(x){
  avg <- average(x)
  n <- length(x[!is.na(x)])
  return(sum((x[!is.na(x)] - avg)^2) / n)
}

sd_var <- function(x){
  return(sqrt(pop_var(x)))
}

#11
n <- length(books$rating)
books_rating <- tibble(
  avg_rating = average(books$rating),
  variance = (pop_var(books$rating) * (n - 1) / n),
  sd = sd_var(books$rating)
)

#12
#Population statistics from the actual data
population_mean <- mean(books$rating, na.rm = TRUE)
population_sd <- sd(books$rating, na.rm = TRUE)

#Creates three samples of size 100 using rnorm with population statistics
set.seed(123)  # For reproducibility
sample1 <- rnorm(100, mean = population_mean, sd = population_sd)
sample2 <- rnorm(100, mean = population_mean, sd = population_sd)
sample3 <- rnorm(100, mean = population_mean, sd = population_sd)

#Creates a tibble to store the sample data
samples <- tibble(
  sample1 = sample1,
  sample2 = sample2,
  sample3 = sample3
)

#Function to compute sample statistics (mean, variance, sd)
sample_statistics <- function(sample) {
  avg <- round(mean(sample), 2)
  var <- round(var(sample), 2)
  sd <- round(sd(sample), 2)
  return(data.frame(mean = avg, variance = var, sd = sd))
}

#Computes statistics for each sample
stats_sample1 <- sample_statistics(sample1)
stats_sample2 <- sample_statistics(sample2)
stats_sample3 <- sample_statistics(sample3)
og_sample <- sample_statistics(books$rating)

#Combines the statistics into a single data frame
sample_stats <- bind_rows(
  og_sample %>% mutate(sample = "Books Ratings"),
  stats_sample1 %>% mutate(sample = "Sample 1"),
  stats_sample2 %>% mutate(sample = "Sample 2"),
  stats_sample3 %>% mutate(sample = "Sample 3")
)
#Prints sample statistics
sample_stats

#13
#Converts samples to long format for ggplot
samples_long <- samples %>%
  pivot_longer(cols = everything(), names_to = "sample", values_to = "rating")

#Creates histogram for each sample
ggplot(samples_long, aes(x = rating, fill = sample)) +
  geom_histogram(binwidth = 0.25, alpha = 0.6, position = "identity") +
  labs(title = "Histogram of Randomly Generated Samples", x = "Rating", y = "Frequency") +
  theme_bw()

testthat::test_file("project3_tests.R")
