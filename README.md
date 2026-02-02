# Loan Default Scoring

This project analyzes Lending Club loan data to predict loan default and build a normalized scoring system (0–100) for borrowers.

## Project Overview
The workflow includes:

1. **Data Preparation**  
   - Reading and cleaning the dataset  
   - Creating a binary `default` variable  
   - Converting categorical variables into factors  
   - Grouping variables (e.g., employment length, loan purpose, DTI quartiles)

2. **Exploratory Data Analysis (EDA)**  
   - Distribution of loan amount, annual income, and DTI  
   - Default rates by employment length, home ownership, region, and loan purpose  
   - Statistical tests (Wilcoxon, Chi-square) to assess associations

3. **Variable Discretization**  
   - Converting numerical variables into bins or categories  
   - Creating factors for historical credit variables

4. **Modeling**  
   - Logistic regression with weighted classes  
   - Stepwise selection to identify significant predictors  
   - Adding interaction terms to improve model performance  
   - Comparing models using AIC, BIC, and pseudo-R² metrics

5. **Interpretation & Scoring**  
   - Odds ratios with confidence intervals  
   - Scoring each borrower based on model coefficients  
   - Normalized score (0–100) and grade prediction (A–G)  
   - Comparison with existing `grade` (if available)

6. **Visualization**  
   - Histograms, boxplots, barplots for EDA  
   - Heatmaps of correlations  
   - Score distribution by predicted grade

## Data
- The dataset `Loan_club_group3.csv` contains Lending Club loans with financial, demographic, and credit history information.
- 
## Requirements
R packages used in the project:

```r
dplyr, ggplot2, patchwork, caret, MASS, stargazer, pscl, purrr, tidyr
