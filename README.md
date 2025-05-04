# BD6 Biodiversity Analysis Project

## Overview

This project investigates ecological trends, variability, and interrelationships within the BD6 taxonomic group—**Bryophytes**, **Isopods**, **Birds**, **Hoverflies**, **Bees**, and **Grasshoppers and Crickets**—in comparison to overall biodiversity (BD11). Using **RStudio**, a series of statistical techniques including univariate analysis, hypothesis testing, correlation analysis, contingency tables, and regression modeling were employed to extract ecological insights.

## Objectives

- Examine the distribution and variability within BD6 taxa.
- Compare BD6 with BD11 using statistical tests.
- Explore interrelationships through correlation and regression.
- Assess the predictive power of BD6 values on Ladybird counts.
- Apply feature selection and model optimization for ecological interpretation.

---

## Methods

### 1. **Univariate Analysis**
- **Summary Statistics & 25% Winsorized Mean** calculated for all BD6 variables.
- Key observations:
  - *Bryophytes* and *Birds* showed high consistency and centrality.
  - *Isopods* and *Bees* showed high variability and outliers.

### 2. **Correlation Matrix**
- Examined pairwise correlations within BD6.
- Most relationships were weak (e.g., Bryophytes vs. Isopods: -0.07).
- Moderate positives: Birds vs. Hoverflies (0.48), suggesting ecological overlap.

### 3. **Boxplot Analysis**
- Boxplot of *Birds* across two periods (Y00 & Y70) revealed:
  - Stable median values.
  - Presence of outliers potentially due to external variability.

---

## Hypothesis Testing

### 1. **Kolmogorov-Smirnov Test**
- Compared BD6 and BD11 distributions.
- Statistically significant differences detected (p < 0.00001), confirming BD6 differs from BD11.

### 2. **T-Test (92% CI)**
- One-sample t-test on BD6 ecology change.
- No significant difference from the mean of -0.02278 (p = 0.1618).

---

## Categorical Analysis

### 1. **Contingency Tables**
- Observed strong concordance in BD6/BD11 direction of change.
- Deviations from independence model suggest interdependence.

### 2. **Likelihood Ratio Test**
- BD6 has a higher rate of increase (35.7%) vs. BD11 (31.0%).
- Statistically significant relationship (p = 0.038).

### 3. **Odds Ratio & Model Evaluation**
- Odds Ratio: **13.19** — strong association.
- Sensitivity: **0.746**, Specificity: **0.818**, Youden’s Index: **0.564**.

---

## Regression Models

### 1. **Simple Linear Regression**
- Predictor: BD6 Eco Values  
- Response: Ladybird Counts  
- Slope: **0.05466** (p < 2e-16)
- Adjusted R²: **0.217**
- Diagnostic plots showed some violation of assumptions (e.g., heteroscedasticity).

### 2. **Multiple Linear Regression**
- Predictors: All BD6 taxa proportions.
- Response: Ladybird BD1 values.
- Adjusted R²: **0.426**
- Feature selection showed Grasshoppers/Crickets had weak contribution.
- Final model retained all variables; interaction terms did not improve performance.
- AIC: **-127.79**, Correlation: **0.654**

---

## Conclusion

This study highlights how BD6 taxa, especially Bryophytes and Birds, provide valuable ecological insight through distinct distributions and patterns. Statistical testing revealed BD6 differs significantly from BD11. Regression modeling confirmed the predictive strength of BD6 values for Ladybird counts. Overall, the project underscores BD6’s utility in biodiversity monitoring and ecological modeling.

---

## Technologies Used

- **R**
- **RStudio**
- **ggplot2**, **dplyr**, **base R functions**
