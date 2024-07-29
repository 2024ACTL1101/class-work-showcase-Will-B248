## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```{r load-data}

# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

amd_df <- amd_df[, c("date", "close")]
```


## Plotting the Data
Plot the closing prices over time to visualize the price movement.
```{r plot}
plot(amd_df$date, amd_df$close,'l')
```


## Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```{r trading}
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
  if (i == 1) {     # for the initial buy there is no need to carry the previous
                    #accumulated shares over so that line is omitted
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -1*amd_df$close[i]*share_size
    amd_df$accumulated_shares[i] <- share_size
    accumulated_shares <- accumulated_shares + share_size
  }
  if (previous_price > amd_df$close[i] && i != 1) {   # buy condition
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -1*amd_df$close[i]*share_size     #cost of the buy
    accumulated_shares <- accumulated_shares + share_size
  } 
  amd_df$accumulated_shares[i] <- accumulated_shares    # insert a value in the
                                                        #accumulated shares column
  previous_price <- amd_df$close[i]         # update the previous close variable
}
# sell all on last day
i <- nrow(amd_df)
amd_df$trade_type[i] <- "sell"
amd_df$costs_proceeds[i] <- accumulated_shares * amd_df$close[i]  #value of the accumulated shares
amd_df$accumulated_shares[i] <- 0       #once you sell the shares there are none accumulated
accumulated_shares <- 0
```

This basic code chunk starts by initialising the required rows in the data frame and the variables needed for the trading logic. The 'for' loop iterates through every row (representing a trading day) in the data set and applies logical tests with 'if' statements to determine if the market conditions meet the requirements to buy. The first if statement ensures that the algorithm buys on the first day and appropriately updates the trade type, cost proceeds and accumulated shares. The second if statement contains the same directions as the first but includes an additional logical test. This ensures that a second buy is not triggered on the first day and avoids using the default intialised value in previous_price. After the second if statement the program updates the accumulated_shares column in the data frame and iterates the previous_price variable to the next day. The last section of the code is outside the loop and so runs at the end of the trading period to sell all of the accumulated shares. Similar to the other two if statements this block sets the trade type, cost proceeds and accumulated shares to their appropriate values.

## Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```{r period}
#defining dates
start_date <- as.Date('2024-01-03')
end_date <- as.Date ('2024-04-01')

#trimming data to specified period
amd_df <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date,]
```


## Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```{r}
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 138.58 #close price for the day before the customised trading period
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
  if (i == 1) {     # for the initial buy there is no need to carry the previous
                    #accumulated shares over so that line is omitted
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -1*amd_df$close[i]*share_size
    amd_df$accumulated_shares[i] <- share_size
    accumulated_shares <- accumulated_shares + share_size
  }
  if (previous_price > amd_df$close[i] && i != 1) {   # buy condition
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -1*amd_df$close[i]*share_size     #cost of the buy
    accumulated_shares <- accumulated_shares + share_size
  } 
  amd_df$accumulated_shares[i] <- accumulated_shares    # insert a value in the accumulated
                                                        #shares column
  previous_price <- amd_df$close[i]         # update the previous close variable
}
# sell all on last day
i <- nrow(amd_df)
amd_df$trade_type[i] <- "sell"
amd_df$costs_proceeds[i] <- accumulated_shares * amd_df$close[i]    #value of the accumulated shares
amd_df$accumulated_shares[i] <- 0       #once you sell the shares there are none accumulated
accumulated_shares <- 0

#calculating total expenditure
total_cost4 <- 0
for (i in 1:nrow(amd_df)) {
  if (!is.na(amd_df$costs_proceeds[i]) && amd_df$costs_proceeds[i] < 0) {
    total_cost4 <- total_cost4 + amd_df$costs_proceeds[i]
  }
}
total_cost4 <- total_cost4 * -1       #reflecting total cost as a positive value

#calculating total profit
total_profit_loss4 <- 0
for (i in 1:nrow(amd_df)) {
  if (!is.na(amd_df$costs_proceeds[i])) {
    total_profit_loss4 <- total_profit_loss4 + amd_df$costs_proceeds[i]
  }
}
cat("Total capital invested $", total_cost4, "\n")
cat("Profit is $", total_profit_loss4, "\n")
roi4 <- total_profit_loss4/total_cost4 *100
cat("Return on investment(ROI) is", roi4, "%")
```

This variation of the trading algorithm is very similar to the last but now uses a trimmed data set and includes calculations to assess the performance of the algorithm. Total expenditure is calculated by iterating through the data frame and summing all the negative transactions which represents buy transactions. The profit calculation is the same except it includes all transactions which equates to the profit made during the period. The return on investment calculation uses the calculated variables to substitute into the formula. The final section prints out these financial performance metrics.

## Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.

```{r}
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking
amd_df$profit_take <- "no"

# Initialize variables for trading logic
previous_price <- 138.58 #close price for the day before the customised trading period
share_size <- 100
accumulated_shares <- 0
acc_purchase_cost <- 0
purchase_avg <- 0

for (i in 1:nrow(amd_df)) {
  if (i == 1) {     # for the initial buy there is no need to carry the previous
                    #accumulated shares over so that line is omitted
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -1*amd_df$close[i]*share_size
    amd_df$accumulated_shares[i] <- share_size
    accumulated_shares <- accumulated_shares + share_size
    acc_purchase_cost <- share_size * amd_df$close[i]
  }
  #calculating average purchase price
  if (i != 1) {
    purchase_avg <- acc_purchase_cost/accumulated_shares
  }
  
  #profit taking when 20% greater than average purchase price
  if (amd_df$close[i] > purchase_avg * 1.20 && accumulated_shares > 0 && i != 1) {       
    sell_shares <- accumulated_shares / 2       # Sell half of the holdings
    amd_df$trade_type[i] <- "sell"
    amd_df$profit_take[i] <- "yes"
    amd_df$costs_proceeds[i] <- sell_shares * amd_df$close[i]
    accumulated_shares <- accumulated_shares - sell_shares      #updating accumulated shares
    acc_purchase_cost <- acc_purchase_cost / 2
  } else if (previous_price > amd_df$close[i] && i != 1) {   # buy condition
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -1*amd_df$close[i]*share_size     #cost of the buy
    accumulated_shares <- accumulated_shares + share_size
    acc_purchase_cost <- acc_purchase_cost + share_size * amd_df$close[i]
  } 
  amd_df$accumulated_shares[i] <- accumulated_shares    # insert a value in the accumulated
                                                        #shares column
  previous_price <- amd_df$close[i]         # update the previous close variable
}
# sell all on last day
i <- nrow(amd_df)
amd_df$trade_type[i] <- "sell"
amd_df$costs_proceeds[i] <- accumulated_shares * amd_df$close[i]    #value of the accumulated shares
amd_df$accumulated_shares[i] <- 0       #once you sell the shares there are none accumulated
accumulated_shares <- 0

#calculating total expenditure
total_cost <- 0
for (i in 1:nrow(amd_df)) {
  if (!is.na(amd_df$costs_proceeds[i]) && amd_df$costs_proceeds[i] < 0) {
    total_cost <- total_cost + amd_df$costs_proceeds[i]
  }
}
total_cost <- total_cost * -1       #reflecting total cost as a positive value

total_profit_loss <- 0
for (i in 1:nrow(amd_df)) {
  if (!is.na(amd_df$costs_proceeds[i])) {
    total_profit_loss <- total_profit_loss + amd_df$costs_proceeds[i]
  }
}
cat("Total capital invested $", total_cost, "\n")
cat("Profit is $", total_profit_loss, "\n")
roi <- total_profit_loss/total_cost *100
cat("Return on investment(ROI) is", roi, "%")
```

This code is largely the same to the other two so far but now calculates the average purchase price by dividing the accumulated purchase cost by the number of accumulated shares. Then there is an additional if statement that precedes the typical buy algorithm with an 'if-else' structure to override any buy signals with a sell if the close price is 20% greater than the average purchase price. In this case it will sell half the shares and set the trade type and cost proceeds to their new values.

## Step 5.5: Adjustments to the trading strategy

I believe for a long term trading algorithm a better metric to use to decide if a profit-taking sell should be made is using a rolling average window with the previous closing prices. This means that the algorithm adjusts for the slow growth of a share price over long periods of time without selling. I used a window size of 20 trading days which seemed optimal from some brief testing.

```{r}
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking
amd_df$profit_take <- "No"

# Initialize variables for trading logic
previous_price <- 138.58 #close price for the day before the customised trading period
share_size <- 100
accumulated_shares <- 0
window_size <- 20       #size of the averaging window
running_average <- previous_price     #initialsing the average

for (i in 1:nrow(amd_df)) {
  if (i == 1) {     # for the initial buy there is no need to carry the previous
                    #accumulated shares over so that line is omitted
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -1*amd_df$close[i]*share_size
    amd_df$accumulated_shares[i] <- share_size
    accumulated_shares <- accumulated_shares + share_size
  }
  if (i > window_size) {
    running_average <- mean(amd_df$close[(i-window_size):(i-1)])    #updating the average window
  }
  
  #profit taking when 20% greater than rolling average
  if (amd_df$close[i] > running_average * 1.20 && accumulated_shares > 0) {       
    sell_shares <- accumulated_shares / 2       # Sell half of the holdings
    amd_df$trade_type[i] <- "sell"
    amd_df$profit_take[i] <- "Yes"
    amd_df$costs_proceeds[i] <- sell_shares * amd_df$close[i]
    accumulated_shares <- accumulated_shares - sell_shares      #updating accumulated shares
  } else if (previous_price > amd_df$close[i] && i != 1) {   # buy condition
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -1*amd_df$close[i]*share_size     #cost of the buy
    accumulated_shares <- accumulated_shares + share_size
  } 
  
  amd_df$accumulated_shares[i] <- accumulated_shares    # insert a value in the accumulated
                                                        #shares column
  previous_price <- amd_df$close[i]         # update the previous close variable
}
# sell all on last day
i <- nrow(amd_df)
amd_df$trade_type[i] <- "sell"
amd_df$costs_proceeds[i] <- accumulated_shares * amd_df$close[i]     #value of the accumulated shares
amd_df$accumulated_shares[i] <- 0       #once you sell the shares there are none accumulated
accumulated_shares <- 0

#calculating total expenditure
total_cost1 <- 0
for (i in 1:nrow(amd_df)) {
  if (!is.na(amd_df$costs_proceeds[i]) && amd_df$costs_proceeds[i] < 0) {
    total_cost1 <- total_cost1 + amd_df$costs_proceeds[i]
  }
}
total_cost1 <- total_cost1 * -1       #reflecting total cost as a positive value

total_profit_loss1 <- 0
for (i in 1:nrow(amd_df)) {
  if (!is.na(amd_df$costs_proceeds[i])) {
    total_profit_loss1 <- total_profit_loss1 + amd_df$costs_proceeds[i]
  }
}
cat("Total capital invested $", total_cost1, "\n")
cat("Profit is $", total_profit_loss1, "\n")
roi1 <- total_profit_loss1/total_cost1 *100
cat("Return on investment(ROI) is", roi1, "%\n")
```

The algorithm above differs from the previous by assessing the average differently. This one progressively fills a buffer of the last 20 closing prices and then takes the mean. The average price is used in exactly the same way as in the last algorithm.

To improve on the strategy above an adjustment can be made. Rather than ignoring the buy signal if the profit taking is triggered in this period it is better to buy and then sell half so there is still a half buy when there is a sharp increase in closing price
```{r option}
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking
amd_df$profit_take <- "No"

# Initialize variables for trading logic
previous_price <- 138.58 #close price for the day before the customised trading period
share_size <- 100
accumulated_shares <- 0
window_size <- 20       #size of the averaging window
running_average <- previous_price     #initialsing the average

for (i in 1:nrow(amd_df)) {
  if (i == 1) {     # for the initial buy there is no need to carry the previous
                    #accumulated shares over so that line is omitted
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -1*amd_df$close[i]*share_size
    amd_df$accumulated_shares[i] <- share_size
    accumulated_shares <- accumulated_shares + share_size
  }
  if (previous_price > amd_df$close[i] && i != 1) {   # buy condition
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -1*amd_df$close[i]*share_size     #cost of the buy
    accumulated_shares <- accumulated_shares + share_size
  } 
  if (i > window_size) {
    running_average <- mean(amd_df$close[(i-window_size):(i-1)])    #updating the average window
  }
  
  #profit taking when 20% greater than rolling average
  if (amd_df$close[i] > running_average * 1.20 && accumulated_shares > 0) {       
    sell_shares <- accumulated_shares / 2       # Sell half of the holdings
    amd_df$trade_type[i] <- "sell"
    amd_df$profit_take[i] <- "Yes"
    amd_df$costs_proceeds[i] <- sell_shares * amd_df$close[i]
    accumulated_shares <- accumulated_shares - sell_shares      #updating accumulated shares
  }
  
  amd_df$accumulated_shares[i] <- accumulated_shares    # insert a value in the accumulated
                                                        #shares column
  previous_price <- amd_df$close[i]         # update the previous close variable
}
# sell all on last day
i <- nrow(amd_df)
amd_df$trade_type[i] <- "sell"
amd_df$costs_proceeds[i] <- accumulated_shares * amd_df$close[i]     #value of the accumulated shares
amd_df$accumulated_shares[i] <- 0       #once you sell the shares there are none accumulated
accumulated_shares <- 0

#calculating total expenditure
total_cost3 <- 0
for (i in 1:nrow(amd_df)) {
  if (!is.na(amd_df$costs_proceeds[i]) && amd_df$costs_proceeds[i] < 0) {
    total_cost3 <- total_cost3 + amd_df$costs_proceeds[i]
  }
}
total_cost3 <- total_cost3 * -1       #reflecting total cost as a positive value

total_profit_loss3 <- 0
for (i in 1:nrow(amd_df)) {
  if (!is.na(amd_df$costs_proceeds[i])) {
    total_profit_loss3 <- total_profit_loss3 + amd_df$costs_proceeds[i]
  }
}
cat("Total capital invested $", total_cost3, "\n")
cat("Profit is $", total_profit_loss3, "\n")
roi3 <- total_profit_loss3/total_cost3 *100
cat("Return on investment(ROI) is", roi3, "%\n")
```

This code is exactly the same as the last chunk except there is no else statement which allows a buy and sell on the same day. This has resulted in greatly improved performance.

## Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```{r}
d_profit <- total_profit_loss3 - total_profit_loss4
d_total_cost <- total_cost3 - total_cost4
d_roi <- roi3 - roi4
cat("The introduction of the profit taking strategy has resulted in the following changes:\n")
cat("Invested capital has increased by $", d_total_cost, "\n")
cat("Profit has increased by $", d_profit, "\n")
cat("Return on investment has increased by", d_roi, "%\n")
```
It is clear that the final profit taking strategy has yielded significant gains over the original algorithm. The new strategy has almost \$70,000 less invested in the stock market over the period which reduces the total risk of the investment. Even with less invested the return for the period was \$60,510 higher. Overall, the return on investment increased by 13.5% over the four month period. However, the stock price rose 35% from the start date to the end date. So, it would have been more effective to fully buy in early and sell it all at the end of the four months. A lot of this loss was due to the algorithm buying the stock throughout the continued decline in its value from its peak on the 7th of March.

```{r, echo=FALSE}
plot(amd_df$date, amd_df$close,'l')
```
As shown by the plot above of the close price there is considerable variance throughout the period from early January to the start of April. There is a clear upwards trend in this four month period. The share price rose roughly 35% from the start to the end of the period with a peak increase of 56% at the height of the price. The general trend is upwards with sharper rises in mid to late January and late February to early March. There is a turning point on the 7th of March where the price declines considerably over the next twenty days before stabilising and resuming the upwards trend.

The key event in this period was AMD reaching a new all-time high after strong speculation from investors about their next generation of AI accelerating processors. However, sentiment from investors quickly shifted after a rival chip manufacturer NVIDIA prepared to unveil their latest AI processors. In this volatile period both of the algorithms took the same actions of buying on the downward slope. The addition of the profit taking strategy did capitalise on on the higher price however which yielded an increase in return and an overall better ROI.




