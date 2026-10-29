stdev <- function(data) {
  # Calculate the mean
  mean_value <- sum(data) / length(data)
  
  # Calculate differences from the mean
  diff_values <- numeric(0)
  for (i in seq_along(data)) {
    diff_values[i] <- data[i] - mean_value
  }
  
  # Calculate squared differences
  squared_diff <- numeric(0)
  for (i in seq_along(diff_values)) {
    squared_diff[i] <- diff_values[i] * diff_values[i]
  }
  
  # Calculate the population variance
  variance <- sum(squared_diff) / length(squared_diff)
  
  # Calculate the standard deviation
  standard_deviation <- sqrt(variance)
  
  return(standard_deviation)
}
