library(quantmod)
library(TTR)
library(PortfolioAnalytics)
# library(fPortfolio)
# library(PerformanceAnalytics)

# create a vector of stocks
stocks <- c('RIVN', 'LCID', 'GM', 'F', 'HMC', 'TSLA')

# download stock prices and create returns from Adjusted Prices
data = lapply(stocks, FUN = function(x) {
  ROC(Ad(getSymbols(x, from = "2021-12-01", to = "2022-09-02", auto.assign = FALSE)),
      type = "discrete") * 100
})  #%returns

# convert to data frame
rets = as.data.frame(do.call(merge, data))

# change columns names
colnames(rets) = gsub(".Adjusted", "", colnames(rets))
# remove the first row of missing values
rets = rets[-1, ]
# add dates column
rets = data.frame(Date = as.Date(row.names(rets)), rets)
row.names(rets) = NULL

data_p2 = zoo(rets[, -1], order.by = as.Date(rets$Date))

# create specification
# specification
# Create the portfolio specification
port_spec <- portfolio.spec(colnames(data_p2 ))

# Add a full investment constraint such that the weights sum to 1
port_spec <- add.constraint(portfolio =port_spec, type = "full_investment")

# Add a long only constraint such that the weight of an asset is between 0 and 1
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")

# Add an objective to minimize portfolio standard deviation
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")

# Solve the optimization problem
opt <- optimize.portfolio(data_p2 , portfolio = port_spec, optimize_method = "ROI")

# Print the results of the optimization
print(opt)

# Extract the optimal weights
extractWeights(opt)

# Chart the optimal weights
chart.Weights(opt)
