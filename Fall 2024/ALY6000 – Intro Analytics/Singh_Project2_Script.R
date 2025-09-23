#Author: Yash S
#Created On: 2024-09-23
#Edited: 2024-09-
#Course: ALY6000

cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100) # disables scientific notion for entire R session

library(pacman)
p_load(tidyverse)

#Assignment Part 1
#1
data_2015 <- read_csv("2015.csv")
head(data_2015)

#2
names(data_2015)

#3
#view(data_2015) 

#4
glimpse(data_2015)

#5
p_load(janitor)
data_2015 <- clean_names(data_2015)
data_2015

#6
happy_df <- select(data_2015,  country, region, happiness_score, freedom)
happy_df

#7
top_ten_df <- slice(happy_df, 1:10)
top_ten_df

#8
no_freedom_df <- filter(happy_df, freedom < 0.20)
no_freedom_df

#9
best_freedom_df <- arrange(happy_df, desc(freedom))
best_freedom_df

#10
data_2015 <- mutate(data_2015, gff_stat = family + freedom + generosity)
data_2015$gff_stat

#11
happy_summary <- happy_df %>% 
  summarise(
    mean_happiness = mean(happiness_score, na.rm = TRUE),
    max_happiness = max(happiness_score),
    mean_freedom = mean(freedom),
    max_freedom = max(freedom)
  )
happy_summary

#12
regional_stats_df <- happy_df %>%
  group_by(region) %>%
  summarise(
    country_count = n(),
    mean_happiness = mean(happiness_score),
    mean_freedom = mean(freedom)
  )
regional_stats_df

#13
least_happy_WEC <- data_2015 %>%
  filter(region == "Western Europe") %>%
  arrange(happiness_score) %>%
  slice_head(n=10) %>%
  summarise(
    europe_gdp = mean(economy_gdp_per_capita)
  )

most_happy_SSAC <- data_2015 %>%
  filter(region == "Sub-Saharan Africa") %>%
  arrange(happiness_score) %>%
  slice_tail(n=10) %>%
  summarise(
    africa_gdp = mean(economy_gdp_per_capita)
  )

gdp_df <- bind_cols(least_happy_WEC, most_happy_SSAC)
gdp_df


#14
#ggplot(regional_stats_df, 
#       aes(x=mean_happiness, y = mean_freedom, colour = region)) +
#  geom_point(size = 3)+
#  geom_segment(
#    aes(
#      x= min(mean_happiness), 
#      y = min(mean_freedom),
#      xend = max(mean_happiness),
#      yend = max(mean_freedom)
#      ),
#    linetype = "solid",
#    color = "Black",
#    linewidth = 1
#    )
ggplot(regional_stats_df, 
       aes(x=mean_happiness, y = mean_freedom, colour = region)) +
  geom_point(size = 3)+
  annotate( "segment", 
            x= min(regional_stats_df$mean_happiness), 
            y = min(regional_stats_df$mean_freedom),
            xend = max(regional_stats_df$mean_happiness),
            yend = max(regional_stats_df$mean_freedom),
            linetype = "solid",
            color = "Black",
            linewidth = 1
  )

#Assignment Part 2 
#1
baseball <- read_csv("baseball.csv")
baseball

#2
head(baseball)
#This line of code displays the first 6 rows
names(baseball)
#This line of code lists the column names
glimpse(baseball)
#This line of code gives the summary of the data, showing types of each variable

#3
class(baseball)

#4
age_stats_df <- baseball %>%
  group_by(Age) %>%
  summarise(
    Count = n(),
    HR = mean(HR, na.rm = TRUE), 
    H = mean(H, na.rm = TRUE), 
    R = mean(R, na.rm = TRUE)
  )
age_stats_df

#5
baseball <- baseball %>%
  filter(AB > 0)
baseball

#6
baseball <- baseball %>%
  mutate(BA = H/AB)
baseball

#7
baseball <- baseball %>%
  mutate(BA = round(BA,3))
baseball

#8
baseball <- baseball %>%
  mutate(OBP =  (H + BB) / (AB + BB))
baseball

#9
baseball <- baseball %>%
  mutate(OBP =  round(OBP,3))
baseball

#10 
strikeout_artist <- baseball %>%
  arrange(desc(SO)) %>%
  head(10)
strikeout_artist

#11
ggplot(baseball, aes(x = HR, y = RBI))+
  geom_point()

#12
eligible_df <- baseball %>%
  filter( AB >= 300 | G >= 100)
eligible_df

#13
ggplot(eligible_df, aes(x = BA))+
  geom_histogram(binwidth = 0.025, color = "Blue", fill = "Green")

#14
eligible_df <- eligible_df |>
  mutate(RankHR =rank(-1 * HR, ties.method = "min"))
eligible_df

#15
eligible_df <- eligible_df |>
  mutate(RankRBI =rank(-1 * RBI, ties.method = "min"))
eligible_df <- eligible_df |>
  mutate(RankOBP =rank(-1 * OBP, ties.method = "min"))
eligible_df

#16
eligible_df <- eligible_df%>%
  mutate(TotalRank = RankHR + RankRBI + RankOBP)
eligible_df

#17
mvp_candidates <- eligible_df |>
  arrange(TotalRank) |>
  head(20)
mvp_candidates

#18
mvp_candidates_abbreviated <- mvp_candidates |>
  select(First, Last, RankHR, RankRBI, RankOBP, TotalRank)
mvp_candidates_abbreviated

#p_load(testthat)
#testthat::test_file("project2_tests.R")

