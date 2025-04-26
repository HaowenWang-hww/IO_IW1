# Forecasting EQIX Stock Reactions to M&A Activity Using ARIMA

## Author
Haowen Wang

## Overview
This project analyzes how Equinix’s (EQIX) stock reacts to M&A announcements and develops a forecasting model for post-acquisition stock behavior.  
Using **regression analysis** and **ARIMA (AutoRegressive Integrated Moving Average)** modeling, the study quantifies short-run price movements following major acquisitions and categorizes event outcomes by slope direction.

## Methodology
- **Data Sources:**  
  - M&A Event Data: Speeda  
  - Stock Price Data: Yahoo Finance (adjusted close prices)
- **Normalization:**  
  - Stock prices normalized to 1.0 on announcement date to ensure comparability.
- **Regression Analysis:**  
  - Fitted a linear model:  
    ```
    Adjusted Price_it = β0 + β1 × Day_it + ε_it
    ```
  - Events classified into **Positive** or **Negative** slope groups based on the value of β₁.
- **ARIMA Forecasting:**  
  - Separate ARIMA models trained for Positive and Negative groups.
  - Forecast horizon: 5–10 trading days beyond the initial 20–60 day observation window.
- **Validation:**  
  - Forecasts compared against actual price paths.
  - Visualizations include regression lines, group-averaged trajectories, and boxplots.

## Key Results
- Positive-slope deals (core markets, large acquisitions) showed continued price appreciation.
- Negative-slope deals (non-core regions) showed flat or declining trends.
- ARIMA forecasts captured meaningful divergence between Positive and Negative event groups.

## Research Motivation
The project is motivated by real-world trends in data center M&A and aims to provide a structured framework for predicting post-M&A stock behavior, especially in digital infrastructure sectors.

## Contribution
- Combines regression-based classification and ARIMA forecasting.
- Offers a methodology that can be extended to broader M&A event studies.
- Highlights strategic factors (deal size, region) influencing stock price reactions.
