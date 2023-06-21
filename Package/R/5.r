#----MEDIE----
# Calculează media unei variabile aleatoare g(X) folosind funcția de densitate și funcția g specificate
medieVariabileAleatoare <- function(densitate, g) {
  medie <- integrate(function(x) g(x) * densitate(x), lower = -Inf, upper = Inf)$value
  return(medie)
}

#----DISPERSIE----
# Calculează dispersia unei variabile aleatoare g(X) folosind funcția de densitate și funcția g specificate
dispersieVariabileAleatoare <- function(densitate, g) {
  medie <- medieVariabileAleatoare(densitate, g)
  dispersie <- integrate(function(x) (g(x) - medie)^2 * densitate(x), lower = -Inf, upper = Inf)$value
  return(dispersie)
}

#----FUNCTIE----
# Definește funcția de densitate a variabilei aleatoare X
f <- function(x) {
  density <- ifelse(x >= 1 & x <= 2, 2 * (x - 1), 0)  # Utilizează o structură condițională ifelse pentru a specifica densitatea
  return(density)
}

#----FUNCTIA UTILIZATORULUI----
# Definește funcția g(X) specificată de utilizator
g <- function(x) {
  return(x^2 + 1)
}

#----CALCULUL MEDIEI----
# Calculează media lui g(X)
medie <- medieVariabileAleatoare(f, g)
print("Medie:")
print(medie)

#----CALCULUL DISPERSIEI----
# Calculează dispersia lui g(X)
dispersie <- dispersieVariabileAleatoare(f, g)
print("Dispersie:")
print(dispersie)
