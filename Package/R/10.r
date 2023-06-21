analyze_values <- function(values) {
  # Calcularea mediei, medianei si deviatiei standard
  mean_value <- mean(values)
  median_value <- median(values)
  sd_value <- sd(values)

  # Generarea histograma
  hist(values, main = "Histograma")

  # Testul Shapiro-Wilk
  shapiro_result <- shapiro.test(values)

  # Estimarea parametrilor prin metoda verosimilitatii maxime
  parameters_mle <- estimate_parameters_mle(values)

  # Estimarea parametrilor prin metoda momentelor
  parameters_moments <- estimate_parameters_moments(values)

  # Returnarea rezultatelor
  results <- list(
    mean = mean_value,
    median = median_value,
    sd = sd_value,
    shapiro_test = shapiro_result,
    mle_parameters = parameters_mle,
    moments_parameters = parameters_moments
  )

  return(results)
}

# Functia pentru estimarea parametrilor prin metoda verosimilitatii maxime
estimate_parameters_mle <- function(data) {
  mean <- mean(data)
  variance <- var(data)
  list(mean = mean, variance = variance)
}

# Functia pentru estimarea parametrilor prin metoda momentelor
estimate_parameters_moments <- function(data) {
  mean <- mean(data)
  variance <- var(data)
  list(mean = mean, variance = variance)
}

# # Citirea setului de valori de la consola
# cat("Introduceti setul de valori (separate prin spatiu): ")
# values <- scan(text = readLines(n = 1), what = numeric(), quiet = TRUE)
values <- c(10, 20, 30, 40, 50)

# Apelarea functiei analyze_values pentru setul de valori introdus de la consola
result <- analyze_values(values)

# Afisarea rezultatelor
cat("Media:", result$mean, "\n")
cat("Mediana:", result$median, "\n")
cat("Deviatia standard:", result$sd, "\n")
print(result$shapiro_test)
print(result$mle_parameters)
print(result$moments_parameters)
