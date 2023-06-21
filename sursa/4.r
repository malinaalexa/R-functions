# Funcția pentru calculul mediei
calculate_mean <- function(data) {
  mean(data)  # Calculează media utilizând funcția mean()
}

# Funcția pentru calculul dispersiei
calculate_variance <- function(data) {
  var(data)  # Calculează dispersia utilizând funcția var()
}

# Funcția pentru calculul momentelor
calculate_moments <- function(data, order) {
  if (order < 1 || order > 4) {
    stop("Ordinul trebuie să fie între 1 și 4.")  # Verifică dacă ordinul se află în intervalul valid
  }
  
  mean_value <- calculate_mean(data)  # Calculează media datelor utilizând funcția calculate_mean()
  moments <- sum((data - mean_value)^order) / length(data)  # Calculează momentele prin scăderea mediei și ridicarea la ordinul specificat
  
  moments  # Returnează momentele calculate
}

# Funcția pentru calculul momentelor centrate
calculate_centered_moments <- function(data, order) {
  if (order < 1 || order > 4) {
    stop("Ordinul trebuie să fie între 1 și 4.")  # Verifică dacă ordinul se află în intervalul valid
  }
  
  mean_value <- calculate_mean(data)  # Calculează media datelor utilizând funcția calculate_mean()
  centered_data <- data - mean_value  # Centrează datele prin scăderea mediei
  
  centered_moments <- sum(centered_data^order) / length(data)  # Calculează momentele centrate prin ridicarea datelor centrate la ordinul specificat
  
  if (order > 1) {
    centered_moments <- centered_moments - calculate_moments(centered_data, order)  # Scade momentele datelor centrate dacă ordinul > 1
  }
  
  centered_moments  # Returnează momentele centrate calculate
}

# Exemplu de utilizare
dataset <- c(1, 2, 3, 4, 5, 6)  # Set de date de exemplu

mean_value <- calculate_mean(dataset)  # Calculează media setului de date utilizând funcția calculate_mean()
cat("Medie: ", mean_value, "\n")  # Afișează media calculată

variance_value <- calculate_variance(dataset)  # Calculează dispersia setului de date utilizând funcția calculate_variance()
cat("Dispersie: ", variance_value, "\n")  # Afișează dispersia calculată

order <- 4  # Ordinul pentru calculul momentelor

# Calculează momentele setului de date până la ordinul specificat utilizând funcția calculate_moments()
moments <- calculate_moments(dataset, order)
cat("Moment (până la ordinul ", order, "): ", moments, "\n")  # Afișează momentele calculate

# Calculează momentele centrate ale setului de date până la ordinul specificat utilizând funcția calculate_centered_moments()
centered_moments <- calculate_centered_moments(dataset, order)
cat("Momente centrate (până la ordinul ", order, "): ", centered_moments, "\n")  # Afișează momentele centrate calculate
