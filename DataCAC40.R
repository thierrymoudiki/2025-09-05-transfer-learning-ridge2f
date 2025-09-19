# Install and load necessary packages
library(quantmod)
library(zoo)

# List of ticker symbols for the CAC40 (example)
# You can replace this with the real tickers from Wikipedia or other sources
cac40_tickers <- c("AC.PA", "AI.PA", "AIR.PA", "ATO.PA", 
                   "BNP.PA", "CAP.PA", "CS.PA", "ENGI.PA", 
                   "GLE.PA", "KER.PA")

# Fetch the stock data for each ticker and calculate log-returns
log_returns_list <- list()
date_ <- as.Date("2025-09-05")

for (ticker in cac40_tickers) {
  # Get the stock data for the ticker
  getSymbols(ticker, src = "yahoo", from = date_ - 500, to = date_, auto.assign = TRUE)
  
  # Retrieve the Adjusted Close Prices
  stock_prices <- Cl(get(ticker))
  
  # Calculate log-returns
  log_returns <- diff(log(stock_prices))
  
  # Linear interpolation for missing data (weekends)
  log_returns_interpolated <- na.approx(log_returns)
  
  # Store the log-returns in the list
  log_returns_list[[ticker]] <- log_returns_interpolated
}

# Convert the list of log-returns into a data frame or matrix
log_returns_matrix <- do.call(cbind, log_returns_list)

# Convert the matrix to a ts object (time series)
log_returns_ts <- ts(log_returns_matrix, start = c(2023, 1), frequency = 252)  # Assuming 252 trading days per year

write.csv(log_returns_matrix, file = "log_returns.csv")
