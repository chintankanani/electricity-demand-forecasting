# Electricity Demand Forecasting Using Machine Learning

## Project Overview

This project investigates short-term electricity demand forecasting using historical electricity consumption and weather data. The objective was to identify key drivers of electricity demand and develop predictive models that support operational planning, resource allocation, and decision-making.

The project applies statistical analysis, feature engineering, Multiple Linear Regression (MLR), and Random Forest algorithms to forecast electricity demand and compare model performance.

---

## Business Problem

Electricity providers require accurate demand forecasts to efficiently manage energy generation, distribution, and resource allocation.

Inaccurate forecasts can result in:

* Overproduction and unnecessary operational costs
* Underestimation leading to supply shortages
* Reduced operational efficiency
* Poor resource planning

This project aims to develop reliable forecasting models using weather conditions and historical demand patterns.

---

## Dataset

The dataset consists of:

* Historical electricity demand records
* Temperature measurements
* Humidity data
* Wind speed information
* Atmospheric pressure
* Moving averages
* Time-based variables

Additional lagged variables were created to capture delayed weather effects and temporal dependencies in electricity demand.

---

## Project Objectives

* Analyse historical electricity demand patterns
* Investigate relationships between weather variables and demand
* Engineer lagged weather and demand features
* Build predictive forecasting models
* Compare Multiple Linear Regression and Random Forest performance
* Generate actionable business insights for planning and decision-making

---

## Tools & Technologies

### Programming & Analytics

* R
* RStudio

### Data Processing & Transformation

* dplyr
* readr
* lubridate

### Statistical Analysis

* Descriptive Statistics
* Skewness Analysis
* Correlation Analysis
* Multicollinearity Testing (VIF)
* Durbin-Watson Test
* Residual Diagnostics

### Machine Learning & Forecasting

* Multiple Linear Regression (MLR)
* Random Forest Regression
* Lag Feature Engineering
* Predictive Modelling

### Data Visualisation

* ggplot2
* corrplot
* ggcorrplot

### Model Evaluation

* RMSE (Root Mean Squared Error)
* MAE (Mean Absolute Error)
* MAPE (Mean Absolute Percentage Error)
* R² (Coefficient of Determination)
* AIC Model Comparison

---

## Methodology

### 1. Data Preparation

* Imported and structured electricity demand and weather datasets
* Renamed variables for consistency
* Handled missing values
* Removed incomplete observations
* Performed data validation and quality checks

### 2. Feature Engineering

Created lagged variables to capture delayed impacts of weather conditions:

* Temperature Lag (30 minutes)
* Temperature Lag (1 hour)
* Humidity Lag (30 minutes)
* Humidity Lag (1 hour)
* Wind Speed Lag (30 minutes)
* Wind Speed Lag (1 hour)
* Power Demand Lag Variables

### 3. Exploratory Data Analysis

Conducted:

* Descriptive statistical analysis
* Skewness assessment
* Boxplot analysis
* Correlation analysis
* Weather-demand relationship investigation

### 4. Statistical Diagnostics

Validated model assumptions through:

* Residual analysis
* Durbin-Watson autocorrelation testing
* Variance Inflation Factor (VIF) analysis
* Normality assessment
* Homoscedasticity checks

### 5. Forecasting Models

#### Multiple Linear Regression (MLR)

Developed several regression models using combinations of:

* Current weather conditions
* Lagged weather variables
* Historical power demand

Models were evaluated and compared using:

* RMSE
* R²
* AIC

#### Random Forest Regression

Developed Random Forest models to:

* Capture non-linear relationships
* Improve predictive performance
* Identify important forecasting variables

Variable importance analysis was conducted to determine the strongest demand drivers.

---

## Model Evaluation Metrics

The forecasting models were evaluated using:

### Mean Absolute Error (MAE)

Measures average prediction error magnitude.

### Root Mean Squared Error (RMSE)

Measures forecast accuracy while penalising larger errors.

### Mean Absolute Percentage Error (MAPE)

Measures forecasting error as a percentage.

### R-Squared (R²)

Measures the proportion of variation explained by the model.

---

## Key Findings

* Historical power demand was one of the strongest predictors of future demand.
* Weather variables significantly influenced electricity consumption.
* Lagged weather variables improved forecasting performance.
* Random Forest captured complex non-linear relationships effectively.
* Feature engineering substantially improved predictive accuracy.

---

## Business Insights

The analysis demonstrates how predictive analytics can support:

* Electricity demand forecasting
* Resource allocation planning
* Capacity management
* Operational efficiency improvements
* Data-driven decision-making

By incorporating weather variables and historical demand behaviour, organisations can improve forecast accuracy and make more informed operational decisions.

---

## Repository Structure

```text
electricity-demand-forecasting/
│
├── README.md
├── demand_forecasting.R
├── Final_Report.pdf
├── Dataset/
│   └── electricity_data.csv
│
└── Images/
    ├── Correlation_Matrix.png
    ├── Variable_Importance.png
    ├── Actual_vs_Predicted_MLR.png
    └── Actual_vs_Predicted_RF.png
```

## Future Improvements

* Explore advanced time-series models such as ARIMA and Prophet
* Implement XGBoost and Gradient Boosting models
* Automate forecasting workflows
* Develop interactive dashboards using Power BI or Shiny
* Deploy forecasting models for real-time prediction

---

## Author

**Chintankumar Kanani**

Master of Applied Business (Business Analytics)

Auckland, New Zealand

### Skills Demonstrated

* Data Cleaning & Transformation
* Feature Engineering
* Forecasting
* Machine Learning
* Statistical Analysis
* Data Visualisation
* Business Analytics
* Predictive Modelling
* Insight Generation
