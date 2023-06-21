#Caz discret
# Defining the function to calculate covariance and correlation
calculate_cov_cor <- function(x, y) {
  # Calculating the means
  mean_x <- mean(x)
  mean_y <- mean(y)
  
  # Calculating the covariance
  covariance <- cov(x, y)
  
  # Calculating the standard deviations
  std_dev_x <- sd(x)
  std_dev_y <- sd(y)
  
  # Calculating the correlation coefficient
  correlation_coefficient <- cor(x, y)
  
  # Returning the results
  results <- list(covariance = covariance, correlation_coefficient = correlation_coefficient)
  return(results)
}

# Example variables
x <- c(1, 2, 3, 4)
y <- c(2, 3, 4, 5)

# Calculating the covariance and correlation coefficient
results <- calculate_cov_cor(x, y)

# Displaying the results
print(results$covariance)
print(results$correlation_coefficient)