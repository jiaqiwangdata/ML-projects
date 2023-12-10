# Lyft Price Regression Analysis

## Introduction
With the rapid growth of modernization and urbanization in the late 20th and early 21st centuries, limited space and resources in urban areas led to the emergence of the concept of the "sharing economy." This concept has had a significant impact on various industries, including online car platforms like Lyft. Lyft, along with Uber, provides an online car platform that allows car owners to register as drivers, pick up passengers, and earn money.

This repository focuses on analyzing Lyft's pricing model in the city of Boston. The analysis aims to understand the factors that influence Lyft's pricing, including order time, pickup and drop-off locations, and weather conditions.

## Data Preparation
### Data Combination
To perform the analysis, we merge two datasets: one containing Lyft price information and the other containing weather data. Since the timestamps of these datasets are not evenly distributed, we use a Cartesian Product approach to merge them and select the records with the minimum time difference for each specific ride's observation.

### Variables Transformation
We transform some variables, such as rain, to better suit our analysis. For example, we convert the rain variable into a binary factor (0 for no precipitation, 1 for rainy days). We also transform timestamps and add a time_range variable for future analysis.

## Data Pre-analysis
In the pre-analysis phase, we examine the data and build a raw model to understand the relationships between variables. We explore attributes such as distance, source, destination, order type, surge multiplier, weather conditions, and more.

## Model Selection
We explore different models and select the one that best fits our analysis. We consider stepwise analysis and interaction terms to refine the model.

## Data Link
You can access the dataset used for this analysis [here](https://drive.google.com/file/d/1QFrortyQC9qnpo92yV4o23Bex1c439nS/view?usp=share_link).


