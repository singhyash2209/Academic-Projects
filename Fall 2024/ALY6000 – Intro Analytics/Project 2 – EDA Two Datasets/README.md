# ALY6000 — Project 2: EDA of Two Datasets
**Term:** Fall 2024  
**Tech:** R (tidyverse, janitor, ggplot2)

## Objective
Practice wrangling, summaries, feature engineering, and basic visuals on two small datasets:
1) **World Happiness (2015)** — regional happiness & freedom  
2) **MLB 1986 Batting** — HR, RBI, BA, OBP, MVP rank

## Methods
- `janitor::clean_names()`; `dplyr::mutate/select/group_by/summarise`
- Feature engineering: **BA = H/AB**, **OBP = (H+BB)/(AB+BB)**
- Simple ranking: combined ranks of HR, RBI, OBP
- `ggplot2` histograms/scatter; quick outlier checks

## Key Results
- Strong HR–RBI relationship; transparent MVP shortlist via total rank  
- Regional patterns in happiness vs. freedom; visible clustering

## Repo Structure
Project 2 – EDA Two Datasets/  
├─ **src/** Project2_Script.R  
├─ **docs/** Project2_Report.pdf  
└─ **data/** 2015.csv, baseball.csv

## How to Run
1. Open `src/Project2_Script.R` in RStudio.  
2. Ensure CSVs exist in `data/`.  
3. Install: `install.packages(c("tidyverse","janitor","ggplot2"))`  
4. Source the script; visuals render inline.

**Notes:** Instructor PDFs/tests omitted; data kept small for reproducibility.
