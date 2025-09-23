# Project 3 – Goodreads Visualization

**Course:** ALY6000 – Introduction to Analytics  
**Student:** Yash Singh  
**Date:** Fall 2024  

---

## Problem Statement
Analyze Goodreads (Kaggle) dataset of ~52,000 books (1990–2020) to explore:
- Statistical properties: mean, variance, std deviation of ratings.
- Visual patterns of ratings, page counts, and publisher dominance.
- Population vs. sample statistics using random samples.
- Trends in books published per year.

---

## Project Structure
- `src/` – R script (`Project3_Script.R`)
- `data/` – Dataset (`books.csv`)
- `docs/` – Report PDF + assignment instructions

---

## Key Visualizations
- Histogram of book ratings (binwidth 0.25, red).
- Box plot of page counts (magenta, horizontal).
- Pareto chart of publisher dominance.
- Scatter plot of pages vs. rating (colored by year).
- Line plot of books rated per year (avg rating as color).
- Heatmap of pages vs. ratings (extra).

---

## Insights
- Ratings skew positive (~4.0 avg).
- Most books under 500 pages; few outliers near 1200.
- Random House & Harper Collins dominate ~70% of books.
- Page count does **not** correlate strongly with ratings.
- Publishing activity has grown steadily since 2000.

---

## How to Run
1. Open `src/Project3_Script.R` in RStudio.  
2. Ensure required packages: `tidyverse, janitor, lubridate, ggthemes, ggeasy, tibble`.  
3. Run the script. It generates all visualizations used in the report.

---

## References
- Dataset: Goodreads via Kaggle  
- Course material (ALY6000 – Northeastern University)  
