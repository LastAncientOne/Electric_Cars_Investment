library(quantmod)

symbols <- c('RIVN', 'LCID', 'GM', 'F', 'HMC', 'TSLA')
start_date <- as.Date('2021-12-01')
end_date <- as.Date('2022-09-02')

# Download stock prices
getSymbols(symbols, from = start_date, to = end_date, adjust = TRUE)

# Calculate daily returns for each symbol
returns <- list()
for (symbol in symbols) {
  symbol_data <- get(symbol)
  symbol_returns <- dailyReturn(Cl(symbol_data))
  returns[[symbol]] <- symbol_returns
}

# Calculate Sharpe ratio for each symbol
sharpe_ratios <- vector("numeric", length = length(symbols))
for (i in 1:length(symbols)) {
  symbol <- symbols[i]
  symbol_returns <- returns[[symbol]]
  
  # Calculate annualized mean return
  mean_return <- mean(symbol_returns) * 252
  
  # Calculate annualized standard deviation
  std_dev <- sd(symbol_returns) * sqrt(252)
  
  # Calculate risk-free rate (assumed to be 0 in this example)
  risk_free_rate <- 0.001
  
  # Calculate Sharpe ratio
  sharpe_ratio <- (mean_return - risk_free_rate) / std_dev
  
  sharpe_ratios[i] <- sharpe_ratio
}

# Create a data frame with symbol and corresponding Sharpe ratio
result <- data.frame(Symbol = symbols, SharpeRatio = sharpe_ratios)
result
