# Funcția pentru construirea funcțiilor de masă/densităților marginale și condiționate
construct_distributions <- function(joint_function) {
  # Calcularea funcției de masă/densității marginale pentru X
  marginal_fx <- aggregate(probabilities ~ x, joint_function, sum)
  
  # Calcularea funcției de masă/densității marginale pentru Y
  marginal_fy <- aggregate(probabilities ~ y, joint_function, sum)
  
  # Calcularea funcției de masă/densității condiționate f(x|y)
  conditional_fx_given_y <- merge(joint_function, marginal_fy, by = "y")
  conditional_fx_given_y$conditional_prob <- conditional_fx_given_y$probabilities.x / conditional_fx_given_y$probabilities.y
  
  # Calcularea funcției de masă/densității condiționate f(y|x)
  conditional_fy_given_x <- merge(joint_function, marginal_fx, by = "x")
  conditional_fy_given_x$conditional_prob <- conditional_fy_given_x$probabilities.x / conditional_fy_given_x$probabilities.y
  
  # Returnarea rezultatelor
  results <- list(marginal_fx = marginal_fx,
                  marginal_fy = marginal_fy,
                  conditional_fx_given_y = conditional_fx_given_y,
                  conditional_fy_given_x = conditional_fy_given_x)
  return(results)
}

# Exemplu de funcție de masă/densitate comună pentru X și Y
joint_function <- data.frame(
  x = c(1, 2, 3),
  y = c(4, 5, 6),
  probabilities = c(0.2, 0.3, 0.5)
)

# Construirea funcțiilor de masă/densităților marginale și condiționate
results <- construct_distributions(joint_function)

# Afișarea funcțiilor de masă/densităților marginale
print("Funcția de masă/densitate marginală pentru X:")
print(results$marginal_fx)

print("Funcția de masă/densitate marginală pentru Y:")
print(results$marginal_fy)

# Afisarea funcțiilor de masă/densităților condiționate
print("Funcția de masă/densitate condiționată f(x|y):")
print(results$conditional_fx_given_y)

print("Funcția de masă/densitate condiționată f(y|x):")
print(results$conditional_fy_given_x)
