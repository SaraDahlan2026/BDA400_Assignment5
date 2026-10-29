sma <- function(data, period) {
  # Check that enough data points are available
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }
  
  # Initialize the vector for SMA values
  sma_values <- numeric(0)
  
  # Calculate the average for each window
  for (i in 1:(length(data) - period + 1)) {
    current_window <- data[i:(i + period - 1)]
    mean_value <- sum(current_window) / period
    sma_values[i] <- mean_value
  }
  
  return(sma_values)
}