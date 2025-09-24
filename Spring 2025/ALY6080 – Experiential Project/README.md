\# ALY6080 – Experiential Project (Spring 2025)



\## Project: Federal Opportunity Mapping Through NAICS Codes and API Integration

\*\*Sponsor:\*\* ImEx Cargo  

\*\*Team:\*\* Group 09 (Trusha Sonawane, Yash Singh, Hunaynah Umerjee, Shivam Joshi)



---



\## Project Structure



Group09\_Federal\_Opportunity\_Mapping/

├── data/ # Cleaned datasets and NAICS reference

├── src/ # R scripts (data cleaning, NLP, API integration)

├── dashboards/ # Power BI dashboard (.pbix)

└── docs/ # Final report (PDF)





---



\## Methods



\- \*\*Data Cleaning \& Transformation\*\*: R (`dplyr`, `tidyr`, `stringr`), handling nulls, normalizing agency names  

\- \*\*Keyword → NAICS Mapping\*\*: NLP preprocessing (`nltk`, `spaCy`), TF-IDF filtering  

\- \*\*API Integration\*\*: Grants.gov API via `httr` + `jsonlite` in R  

\- \*\*Visualization\*\*: Power BI interactive dashboard (`.pbix`)  



---



\## Deliverables



\- \*\*docs/group\_09\_report.pdf\*\* – Final APA-style report  

\- \*\*dashboards/ALY6080\_Grp9\_DB.pbix\*\* – Power BI dashboard  

\- \*\*src/group9.R\*\* – R code for cleaning, mapping, API integration  

\- \*\*data/\*\* – Cleaned CSVs and NAICS reference  



---



\## How to Reproduce



1\. Clone this repo.  

2\. Install R packages:  

install.packages(c("tidyverse","stringr","jsonlite","httr"))

3\. Open src/group9.R in RStudio.

4\. Place CSV/XLSX from data/ in working directory.

5\. Run script → cleans data, maps NAICS, integrates Grants.gov API.

6\. Open dashboards/ALY6080\_Grp9\_DB.pbix in Power BI Desktop to explore dashboards.



\## Outcomes



* 45+ NAICS codes mapped
* Grants.gov API successfully integrated
* Clean datasets for reproducibility
* Final PDF report for stakeholders
