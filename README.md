# Exploring Obesity Risk

## Overview

This project investigates the relationship between demographic,
dietary, lifestyle, and physical characteristics and obesity status.

## Data Source
- The OBESITY PREDICTION dataset is collected from the website Kaggle.com. The link to the original data is attached below.
- https://www.kaggle.com/datasets/ruchikakumbhar/obesity-prediction


## Data Description
- The OBESITY PREDICTION dataset include data for the estimation of obesity levels in individuals from the countries of Mexico, Peru, and Colombia, based on their physical conditions and daily habits.
- The data contains 17 columns and 2111 rows.The 17 columns can work as the response and the predictors in our analysis.


## Project Objectives

- Explore the relationship between obesity and its potential predictors.
- Apply statistical tests to examine associations between categorical variables.
- Investigate multicollinearity using Variance Inflation Factors.
- Fit a logistic regression model to predict obesity status.
- Evaluate classification performance using a confusion matrix and ROC-AUC.
- Extend the original analysis with additional machine learning methods.

## Repository Structure

- `data/`: Raw and cleaned datasets.
- `report/`: Original report.
- `R/`: Original R-based statistical analysis.
- `Python/`: Python-based analysis and machine learning extensions.
- `results/`: Visualizations and model evaluation outputs.

## Methodology

### Exploratory Data Analysis
- Dataset inspection and missing-value checks
- Boxplots of height and weight
- Stacked bar charts
- Contingency tables

### Statistical Analysis
- Yule's Q
- Chi-square tests of independence
- Symmetry tests
- Tests comparing obesity proportions across age groups

### Logistic Regression
- Binary logistic regression
- Multicollinearity assessment using VIF
- Odds ratios and model interpretation
- Confusion matrix
- ROC curve and AUC

## Implementations

### R
Reproduces the statistical analysis associated with the original dissertation.

### Python
A separate implementation intended to extend the analysis with
additional modelling approaches and more rigorous model evaluation.

## Report

See `dissertation/Obesity Prediction Analysis.pdf` for the original
dissertation.

## Tools

- R
- Python
- pandas
- NumPy
- scikit-learn
- statsmodels
- Matplotlib
