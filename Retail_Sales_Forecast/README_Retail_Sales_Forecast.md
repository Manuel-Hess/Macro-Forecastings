# Forecasting U.S. Retail and Food Services Sales Using SARIMA

## Overview
This project compares three time-series forecasting models for monthly U.S. Retail and Food Services Sales (FRED series RSAFSNA), with a particular focus on how each model handles the structural break caused by the COVID-19 pandemic.

## Models Compared
- **Seasonal ARIMA (SARIMA)** – univariate benchmark capturing trend and seasonality
- **Dynamic Regression with COVID intervention dummy** – SARIMA errors plus an explicit pandemic shock variable
- **ETS (Error-Trend-Seasonal)** – state-space model with adaptive level/trend/seasonal components

## Result
Based on out-of-sample forecast accuracy (RMSE, MAE, MAPE) over the post-pandemic test period, the **ETS model** delivered the most accurate and stable forecasts. It adapted more flexibly to the persistent post-COVID level shift, while the SARIMA and dynamic regression models struggled to track the new elevated sales level.

| Model | RMSE | MAE | MAPE |
|---|---|---|---|
| ETS | 78.8 | 76.6 | 11.3% |
| SARIMA | 116 | 111 | 16.4% |
| Dynamic Regression (COVID) | 141 | 125 | 18.4% |

## Data
Source: [FRED – Retail and Food Services Sales (RSAFSNA)](https://fred.stlouisfed.org/series/RSAFSNA), monthly, not seasonally adjusted. Sample period: September 2010 – September 2025.

## Files
- `Retail_Sales_Forecast_USA.R` – full analysis script (data cleaning, EDA, model fitting, forecasting, diagnostics)
- `SALES_DATA.csv` – raw input data from FRED
- `output/figures/` – all generated plots
- `output/tables/` – model comparison and diagnostic tables

## Origin
Originally developed as a group project for the course *Principles and Practice of Forecasting* (Prof. Dr. Matthias Fengler, University of St. Gallen) with Cecilia Henriette Eek Thorstensen and Xavier Wanner.

## Possible Extensions
- Generate genuine out-of-sample forecasts beyond September 2025 (e.g. a 5-year horizon) using the selected ETS model
- Re-estimate with more recent data as it becomes available
- Compare forecast stability across different COVID dummy specifications (already partially explored in the robustness checks)
