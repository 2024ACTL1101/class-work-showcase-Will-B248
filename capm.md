---
title: "ACTL1101 Assignment Part B"
author: "William Brett"
date: "2024 T2"
output:
  pdf_document: default
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(quantmod)
library(ggplot2)
library(tidyverse)
```

# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```{r load-data}
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```{r data}
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

\[ E(R_i) = R_f + \beta_i (E(R_m) - R_f) \]

Where:

- \( E(R_i) \) is the expected return on the capital asset,
- \( R_f \) is the risk-free rate,
- \( \beta_i \) is the beta of the security, which represents the systematic risk of the security,
- \( E(R_m) \) is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```{r return}
df$dr_AMD <- NA
df$dr_SP <- NA
for (i in 2:nrow(df)) {
  df$dr_AMD[i] <- (df$AMD[i] - df$AMD[i-1])/df$AMD[i-1]
  df$dr_SP[i] <- (df$GSPC[i] - df$GSPC[i-1])/df$GSPC[i-1]
}
```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```{r riskfree}
df$risk_free <- NA
for (i in 1:nrow(df)) {
  df$risk_free[i] <- (1 + df$RF[i]/100) ^ (1/360) - 1
}
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```{r excess return}
df$excess_AMD <- NA
df$excess_SP <- NA
for (i in 2:nrow(df)) {
  df$excess_AMD[i] <- df$dr_AMD[i] - df$risk_free[i]
  df$excess_SP[i] <- df$dr_SP[i] - df$risk_free[i]
}
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```{r lm}
model <- lm(excess_AMD ~ excess_SP, data = df)
beta <- summary(model)$coefficients[2,1]
cat("The beta is", beta, "\n")
```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:**
The calculated beta is 1.569. This means that AMD is more volatile than the market as measured by the S&P500. From the CAPM formula \[ E(R_i) = R_f + \beta_i (E(R_m) - R_f) \] it is clear that a beta of 1.569 will mean that the expected fluctuations in the price will be 57% greater than those of the S&P500. This means that volatility is much greater than the wider market. Ultimately, investors stand to gain or lose more from their investment.

#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```{r plot}
ggplot(df, aes(x = excess_SP, y = excess_AMD)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "purple") +
  labs(title = "Scatter plot of AMD vs S&P 500 excess returns",
       x = "Market excess return", y = "AMD excess   return")
```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.

*Hint: Calculate the daily standard error of the forecast ($s_f$), and assume that the annual standard error for prediction is $s_f \times \sqrt{252}$. Use the simple return average method to convert daily stock returns to annual returns if needed.*


**Answer:**

```{r pi}
risk_free <- 5/100
exp_market <- 13.3/100
exp_return <- risk_free + beta*(exp_market-risk_free)


#calculating the interval
n <- length(na.omit(df$excess_SP))
mean_SP <- mean(na.omit(df$dr_SP))
se <- sqrt(sum(residuals(model)^2)/(n-2))
SSX <- sum((na.omit(df$GSPC)-mean_SP)^2)

sf <- se * sqrt(1+1/n + (exp_market - mean_SP - risk_free)^2 / SSX)

alpha <- 0.9
t_value <- qt((1 + alpha) / 2, df = n - 2)

lower_bound <- exp_return - (t_value * sf)*sqrt(252)
upper_bound <- exp_return + (t_value * sf)*sqrt(252)
cat("90% prediction interval is: [", lower_bound, ",", upper_bound, "]")
```

This means that with 90% confidence one can assume that the annual change the AMD price will be between -49% and 85%. So, while investors stand to lose a lot of invested capital they are more likely to have their investment appreciate.
