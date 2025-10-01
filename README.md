# Formula 1 Statistics Project

This project analyzes Formula 1 data using **inferential statistics** in R.  
It includes **confidence intervals, hypothesis testing, ANOVA, and regression models** applied to race and driver data.

---

## 📑 Table of Contents
1. [Project Overview](#project-overview)  
2. [Files](#files)  
3. [Statistical Analyses](#statistical-analyses)  
   - [Confidence Intervals](#confidence-intervals)  
   - [Hypothesis Testing](#hypothesis-testing)  
   - [ANOVA](#anova)  
   - [Regression Analysis](#regression-analysis)
4. [How to Run](#how-to-run)  
5. [Dependencies](#dependencies)  

---

## 🔎 Project Overview
The goal of this project is to explore whether Formula 1 performance is driven more by **drivers** or **teams**, using data from multiple seasons.  
We apply various statistical methods to investigate:
- Driver ages and performance  
- Circuit types (street vs permanent)  
- Influence of qualifying position on finishing position  
- Team tier effects  

---

## 📂 Files
- `f1_statistics_analysis.R` – Main R script containing all analyses   
- `README.md` – Project documentation  

---

## 📊 Statistical Analyses

### Confidence Intervals
- Estimated **average age** of Formula 1 drivers.  
- Estimated the **proportion of street circuits** on the calendar.  

### Hypothesis Testing
1. Do drivers improve their position during races?  
2. Is the **average driver age > 30**?  
3. Do drivers under 25 achieve **better than 10th place** on average?  

### ANOVA
- Differences in mean **driver age** across constructor tiers.  
- Differences in **qualifying position** across age categories.  
- Differences in **finishing position** across constructor tiers.  

### Regression Analysis
- **Simple Linear Regression**  
  - Finishing position ~ Constructor’s championship standing  
  - Finishing position ~ Driver age  
  - Finishing position ~ Circuit type  
  - Finishing position ~ Qualifying position  

- **Multiple Linear Regression**  
  - Combined effect of qualifying position, constructor performance, race type, and interactions.  

---

## ▶️ How to Run
1. Clone this repository:
   ```bash
   git clone https://github.com/YOUR_USERNAME/f1-statistics-project.git
2. Open the R script
   ```R
   source("f1_statistics_analysis.R")
## Dependencies

This project uses the following R libraries:

readxl

dplyr

ggplot2

Install them if missing:

```R
install.packages(c("readxl", "dplyr", "ggplot2"))

