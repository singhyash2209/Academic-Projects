# ALY6000 — Project 2: EDA of Two Datasets
**Term:** Fall 2024  
**Tech:** R (tidyverse, janitor, ggplot2)

## Objective
Perform exploratory data analysis on two datasets to practice wrangling, summaries, feature engineering and basic visualization:
1) **World Happiness (2015)** — regional happiness & freedom  
2) **MLB 1986 Batting** — player stats (HR, RBI, BA, OBP) and MVP ranking

## Methods
- `janitor::clean_names()` to standardize fields
- Grouped summaries (`dplyr::group_by/summarise`)
- Feature engineering: **BA = H/AB**, **OBP = (H+BB)/(AB+BB)**
- Simple ranking by combined ranks of HR, RBI, OBP
- `ggplot2` scatter/histogram visuals

## Key Results
- Clear HR–RBI relationship; a transparent MVP shortlist via total rank
- Regional differences in happiness vs. freedom; visible cluster patterns

## Repo Structure
Project 2 – EDA Two Datasets/
├─ src/
│ └─ Project2_Script.R
├─ docs/
│ └─ Project2_Report.pdf
└─ data/
├─ 2015.csv
└─ baseball.csv

## How to Run
1. Open `src/Project2_Script.R` in **RStudio**.  
2. Ensure `data/2015.csv` and `data/baseball.csv` exist (already included here).  
3. Install required packages if missing:
   install.packages(c("tidyverse","janitor","ggplot2"))
4. Source the script; outputs are printed/visualized inline.
Notes
- Course instruction PDFs/tests are not included to keep the repo focused on my deliverables.
- Datasets are small and included for reproducibility; if you prefer, swap with your own copies and keep the file names.

Author

Yash Amit Singh
