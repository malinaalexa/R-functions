# Înc?rcarea pachetului "ggplot2"
library(ggplot2)

# Definirea func?iei "verif3" care verific? tipul de func?ie ?i genereaz? grafice corespunz?toare
verif3 <- function(tipulfunctiei, f) {
  if (tipulfunctiei == 0) {
    if (any(f < 0)) {
      print("f nu este functie de masa")
    } else if (abs(sum(f) - 1) > 1e-10) {
      print("f nu este functie de masa")
    } else {
      print("f este functie de masa")
    }
    
    # Reprezentarea grafic? a func?iei de mas?
    df <- data.frame(x = 1:length(f), y = f)
    ggplot(df, aes(x, y)) +
      geom_bar(stat = "identity") +
      labs(title = "Reprezentarea grafica a functiei de masa", x = "x", y = "Probabilitate") +
      theme_bw()
    
  } else if (tipulfunctiei == 1) {
    xs <- seq(-100, 100, length.out = 1000)
    
    if (any(f(xs) < 0)) {
      print("f nu este densitate de probabilitate")
    } else {
      tryCatch(
        {
          integrala <- integrate(f, -Inf, Inf)
          if (abs(integrala$value - 1) > 1e-4) {
            print("f nu este densitate de probabilitate")
          } else {
            print("f este densitate de probabilitate")
          }
          
          # Reprezentarea grafica a densitatii de probabilitate
          df <- data.frame(x = xs, y = f(xs))
          ggplot(df, aes(x, y)) +
            geom_line() +
            labs(title = "Reprezentarea grafica a densit?tii de probabilitate", x = "x", y = "Densitate de probabilitate") +
            theme_bw()
          
          # Reprezentarea grafica a functiei de repartitie
          cumulative <- function(x) {
            sapply(x, function(x_val) {
              integrate(f, -Inf, x_val)$value
            })
          }
          df_cumulative <- data.frame(x = xs, y = cumulative(xs))
          ggplot(df_cumulative, aes(x, y)) +
            geom_line() +
            labs(title = "Reprezentarea grafica a functiei de repartitie", x = "x", y = "Probabilitate") +
            theme_bw()
        },
        error = function(e) {
          print("f nu este densitate de probabilitate")
        }
      )
    }
  }
}

# Exemplu PMF: Func?ie de mas? cu 10 parametrii
pmf <- c(0.1, 0.05, 0.15, 0.2, 0.1, 0.05, 0.1, 0.1, 0.05, 0.1)
verif3(0, pmf)

# Exemplu PMF: Aruncarea unui zar echilibrat cu sase fete
pmf <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
verif3(0, pmf)

# Exemplu PMF invalid: Probabilitati inegale
pmf_invalid <- c(0.2, 0.3, 0.4)
verif3(0, pmf_invalid)

# Exemplu PMF invalid: Probabilitate negativa
pmf_invalid <- c(0.2, 0.3, -0.1, 0.6)
verif3(0, pmf_invalid)

# Exemplu PDF: Distributia normala standard
pdf <- function(x) dnorm(x, mean = 0, sd = 1)
verif3(1, pdf)

# Exemplu PDF invalid: Densitate negativa
pdf_invalid <- function(x) -dnorm(x, mean = 0, sd = 1)
verif3(1, pdf_invalid)

# Exemplu PDF invalid: Integral? incorecta
pdf_invalid <- function(x) dnorm(x, mean = 0, sd = 1) * 2
verif3(1, pdf_invalid)

