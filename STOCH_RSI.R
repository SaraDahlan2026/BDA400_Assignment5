# Stochastic RSI (StochRSI)
stoch_rsi <- function(data, period, k_period, d_period) {
  # Calculate RSI
  rsi_values <- rsi(data, period)
  
  # Calculate StochRSI using all available RSI values
  min_rsi <- min(rsi_values, na.rm = TRUE)
  max_rsi <- max(rsi_values, na.rm = TRUE)
  
  k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
  
  # Calculate %K and %D lines
  k_line <- sma(k_values, k_period)
  d_line <- sma(k_line, d_period)
  
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  
  return(result)
}
# Sample Data
data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62,
          64, 67, 63, 69, 72, 70, 74, 76, 73, 78,
          80, 77, 82, 85, 83, 87, 84, 89, 91, 88)
# Calculate StochRSI
stoch_rsi_result <- stoch_rsi(
  data, period = 14, k_period = 3, d_period = 3
)
print(stoch_rsi_result)
