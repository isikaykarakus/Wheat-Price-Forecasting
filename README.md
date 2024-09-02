# Wheat Price Prediction

This repository contains the code, data, and documentation for the project **Wheat Price Prediction**. The aim of this project is to develop a reliable model for predicting and forecasting wheat prices over the next 5 years. The prediction is based on various factors that influence wheat prices, including weather conditions, global supply and demand, political and economic stability, consumption patterns, government policies, technological advances, energy costs, and global events.

## Table of Contents

1. [Introduction](#introduction)
2. [Project Specifications](#project-specifications)
3. [Data Collection and Preparation](#data-collection-and-preparation)
4. [Exploratory Data Analysis (EDA)](#exploratory-data-analysis-eda)
5. [Data Modeling](#data-modeling)
6. [Model Comparison and Forecasting](#model-comparison-and-forecasting)
7. [Results and Conclusions](#results-and-conclusions)
8. [Future Work](#future-work)
9. [Acknowledgements](#acknowledgements)

## Introduction

Wheat is a vital food source, often referred to as "edible gold." This project focuses on developing models to predict wheat prices by analyzing various influencing factors and using historical data. The models help understand price trends and forecast future prices, which is crucial for stakeholders in agriculture, economics, and policy-making.

## Project Specifications

The goal of this project is to create robust predictive models for wheat prices over the next five years. Key factors influencing wheat prices include:

- Weather Conditions
- Global Supply and Demand
- Political and Economic Stability
- Consumption Patterns
- Government Policies and Subsidies
- Market Speculation
- Technological Advances
- Energy Costs
- Global Events
- Currency Fluctuations

## Data Collection and Preparation

Data used in this project includes:

- Wheat price data (1990-2022)
- Pasta price data (1984-2023)
- Flour price data (1980-2023)
- Crop production data (1990-2023)
- Global land and ocean temperature anomalies (1990-2023)
- Supply and disappearance data (2003-2023)
- International trade data (1990-2022)

The dataset combines several sources to provide a comprehensive view of the factors affecting wheat prices.

## Exploratory Data Analysis (EDA)

The exploratory data analysis phase involved:

- Examining historical trends and patterns in wheat prices.
- Identifying key events that affected wheat prices, such as droughts, floods, and geopolitical events.
- Analyzing the top wheat-producing and exporting countries.

## Data Modeling

Various statistical and machine learning models were employed for predicting wheat prices:

- **Linear Regression Model**
- **Bass Model and Generalized Bass Model**
- **Local Regression and Splines**
- **LASSO Regression**
- **ARIMA and ARMAX Models**
- **Exponential Smoothing**
- **Generalized Additive Model (GAM)**

Each model was evaluated using error metrics such as Mean Squared Error (MSE), Root Mean Squared Error (RMSE), Mean Absolute Error (MAE), and Mean Absolute Percentage Error (MAPE).

## Model Comparison and Forecasting

The models were compared based on their forecasting accuracy:

- The **Generalized Additive Model (GAM)** demonstrated the best forecasting performance with the lowest error metrics.
- The **LASSO Regression** was identified as the best model for prediction.

## Results and Conclusions

- Forecasting wheat prices using the **GAM model** resulted in the most accurate predictions with an error margin of approximately $0.019 per kilogram.
- Key factors influencing wheat prices were identified as production and total import quantities.
- The project highlighted the impact of global events and economic policies on wheat prices.

## Future Work

Future developments could include:

- Incorporating more diverse data, such as satellite imagery, for better crop assessment.
- Developing more advanced models like regression trees.
- Including socio-economic factors, such as poverty levels, to assess potential food crises.

## Acknowledgements

This project was conducted under the supervision of Prof. Mariangela Guidolin, with contributions from Isikay Karakus and Auriane Mahfouz.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.


