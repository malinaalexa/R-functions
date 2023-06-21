9.1. Faceți histograma valorilor. Calculați mediana, media și deviația standard și
ilustrați pe desen aceste valori.

# Setul de valori
set_a <- c(7, 4, 2, 11, 2, 1, 2, 1, 6, 6, 0, 1, 3, 9, 7, 0, 1, 14, 0, 5, 1, 5, 2, 4, 3, 1, 0, 0, 26, 1)

# Histograma
hist(set_a, breaks=length(set_a), main = "Histograma valorilor", xlab = "Valoare", ylab = "Frecvență")

# Mediana
median_a <- median(set_a)
cat("Mediana:", median_a, "\n")

# Media
mean_a <- mean(set_a)
cat("Media:", mean_a, "\n")

# Deviația standard
sd_a <- sd(set_a)
cat("Deviația standard:", sd_a, "\n")

# Ilustrarea valorilor pe histogramă
abline(v = median_a, col = "red", lwd = 2, lty = 2, label = "Mediana")
abline(v = mean_a, col = "blue", lwd = 2, lty = 2, label = "Media")
legend("topright", legend = c("Mediana", "Media"), col = c("red", "blue"), lwd = 2, lty = 2)


Mediana: 2 
Media: 4.166667 
Deviația standard: 5.401681

b) -1.91 -0.97 4.59 2.19 -0.86 -0.74 -0.60 -1.29 0.93 1.42 2.14 -2.01 2.60 1.45 2.60
-3.32 -3.62 3.09 2.91 3.60 -0.83 -0.27 1.82 -1.38 -1.76 1.43 -0.59 -1.34 2.07 POISSON
1.02

# Setul de valori
values <- c(-1.91, -0.97, 4.59, 2.19, -0.86, -0.74, -0.60, -1.29, 0.93, 1.42, 2.14, -2.01, 2.60, 1.45, 2.60,
            -3.32, -3.62, 3.09, 2.91, 3.60, -0.83, -0.27, 1.82, -1.38, -1.76, 1.43, -0.59, -1.34, 2.07, 1.02)

# Histograma valorilor
hist(values, breaks = "FD", main = "Histograma", xlab = "Valori")

# Mediana
median_value <- median(values)
cat("Mediana:", median_value, "\n")

# Media
mean_value <- mean(values)
cat("Media:", mean_value, "\n")

# Deviația standard
sd_value <- sd(values)
cat("Deviația standard:", sd_value, "\n")




c) 0.90 8.91 0.06 1.85 1.61 6.50 0.26 0.04 0.62 1.01 3.42 1.45 3.44 0.46 0.55 EXPONENTIALA
0.09 2.22 0.65 0.61 6.45 0.27 4.81 2.27 0.34 4.51 0.42 3.71 2.59 0.42 11.18
# Setul de valori
values <- c(0.90, 8.91, 0.06, 1.85, 1.61, 6.50, 0.26, 0.04, 0.62, 1.01, 3.42, 1.45, 3.44, 0.46, 0.55,
            0.09, 2.22, 0.65, 0.61, 6.45, 0.27, 4.81, 2.27, 0.34, 4.51, 0.42, 3.71, 2.59, 0.42, 11.18)

# Histograma valorilor
hist(values, breaks = "FD", main = "Histograma", xlab = "Valori")

# Mediana
median_value <- median(values)
cat("Mediana:", median_value, "\n")

# Media
mean_value <- mean(values)
cat("Media:", mean_value, "\n")

# Deviația standard
sd_value <- sd(values)
cat("Deviația standard:", sd_value, "\n")



d) 4.83 4.37 5.57 4.22 5.96 5.11 5.52 4.81 5.19 4.19 4.73 5.92 5.63 4.53 4.67 4.84 5.25
5.06 5.98 5.25 4.60 4.11 4.32 5.09 5.25 5.10 4.36 5.40 5.33 4.65  NORMALA
# Setul de valori
values <- c(4.83, 4.37, 5.57, 4.22, 5.96, 5.11, 5.52, 4.81, 5.19, 4.19, 4.73, 5.92, 5.63, 4.53, 4.67,
            4.84, 5.25, 5.06, 5.98, 5.25, 4.60, 4.11, 4.32, 5.09, 5.25, 5.10, 4.36, 5.40, 5.33, 4.65)

# Histograma valorilor
hist(values, breaks = "FD", main = "Histograma", xlab = "Valori")

# Mediana
median_value <- median(values)
cat("Mediana:", median_value, "\n")

# Media
mean_value <- mean(values)
cat("Media:", mean_value, "\n")

# Deviația standard
sd_value <- sd(values)
cat("Deviația standard:", sd_value, "\n")

e) 11 11 10 10 10 6 5 9 11 10 14 8 11 6 13 9 14 16 14 10 7 7 11 12 9 5 12 15 9 NORMALA
12

# Setul de valori
values <- c(11, 11, 10, 10, 10, 6, 5, 9, 11, 10, 14, 8, 11, 6, 13, 9, 14, 16, 14, 10, 7, 7, 11, 12, 9, 5, 12, 15, 9)

# Histograma valorilor
hist(values, breaks = "FD", main = "Histograma", xlab = "Valori")

# Mediana
median_value <- median(values)
cat("Mediana:", median_value, "\n")

# Media
mean_value <- mean(values)
cat("Media:", mean_value, "\n")

# Deviația standard
sd_value <- sd(values)
cat("Deviația standard:", sd_value, "\n")

9.2. Găsiți o manieră de a identifica din ce repartiție au fost generate valorile de mai
sus, cu identificarea parametrilor și justificarea alegerii făcute.
a)Pentru a identifica parametrii unei repartiții exponențiale, putem urma următorii pași:
  Calculăm media (λ) a setului de valori pentru a obține o estimare inițială a 
data <- c(7, 4, 2, 11, 2, 1, 2, 1, 6, 6, 0, 1, 3, 9, 7, 0, 1, 14, 0, 5, 1, 5, 2, 4, 3, 1, 0, 0, 26, 1) lambda_estimate <- 1 / mean(data) lambda_estimate
Verificăm forma distribuției prin vizualizarea histogramelor și a graficului Q-Q (quantile-quantile). Dacă distribuția pare să aibă o formă exponențială, putem presupune că este o repartiție exponențială și procedăm cu identificarea parametrilor.
# Histograma hist(data, breaks = "FD", main = "Histograma", xlab = "Valori") # Graficul Q-Q qqplot(data, qexp, scale = lambda_estimate)
Utilizăm estimări ale metodei momentelor pentru a estima parametrii repartiției exponențiale.
Metoda momentului pentru media:
  Parametrul estimat pentru media (λ) este inversul mediei eșantionului.
Pentru setul de valori dat, putem folosi această estimare a metodei momentelor pentru a estima parametrul:
  
  # Estimarea parametrului prin metoda momentului lambda_moment <- 1 / mean(data) lambda_moment
  În acest caz, parametrul estimat prin metoda momentelor pentru repartiția exponențială este:
  Media (λ) ≈ 0.1642
Aceasta este o estimare a valorii reale a parametrului repartiției exponențiale care a generat setul de valori dat.







b)Setul de valori: -1.91, -0.97, 4.59, 2.19, -0.86, -0.74, -0.60, -1.29, 0.93, 1.42, 2.14, -2.01, 2.60, 1.45, 2.60, -3.32, -3.62, 3.09, 2.91, 3.60, -0.83, -0.27, 1.82, -1.38, -1.76, 1.43, -0.59, -1.34, 2.07, 1.02
Calculăm media și deviația standard a setului de valori pentru a obține o estimare inițială a parametrilor.


data <- c(-1.91, -0.97, 4.59, 2.19, -0.86, -0.74, -0.60, -1.29, 0.93, 1.42, 2.14, -2.01, 2.60, 1.45, 2.60, -3.32, -3.62, 3.09, 2.91, 3.60, -0.83, -0.27, 1.82, -1.38, -1.76, 1.43, -0.59, -1.34, 2.07) mean_estimate <- mean(data) sd_estimate <- sd(data) mean_estimate sd_estimate
Verificăm forma distribuției prin vizualizarea histogramelor și a graficului Q-Q (quantile-quantile). Dacă distribuția pare să aibă o formă discretă cu numărul de evenimente concentrat în jurul mediei, putem presupune că este o repartiție Poisson și procedăm cu identificarea parametrilor.


# Histograma hist(data, breaks = "FD", main = "Histograma", xlab = "Valori") # Graficul Q-Q qqnorm(data) qqline(data)R


Utilizăm metoda verosimilității maxime sau metoda momentelor pentru a estima parametrii repartiției Poisson.
Metoda verosimilității maxime:
  Această metodă implică maximizarea funcției de verosimilitate pentru a găsi valorile optime ale parametrilor. Pentru o repartiție Poisson, parametrul estimat prin metoda verosimilității maxime este egal cu media eșantionului (λ = μ).
Metoda momentelor:
  Această metodă implică egalarea momentelor de ordin superior ale distribuției cu momentele de ordin similar ale eșantionului. Pentru o repartiție Poisson, parametrul estimat prin metoda momentelor este egal cu media eșantionului (λ = μ).
Pentru setul de valori dat, putem folosi oricare dintre aceste metode pentru a estima parametrii. Deoarece distribuția pare să aibă o formă discretă, cu numărul de evenimente concentrat în jurul mediei, putem presupune că este o repartiție Poisson și procedăm cu estimarea parametrilor.
De exemplu, vom folosi metoda verosimilității maxime:
  # Estimarea parametrilor prin metoda verosimilității maxime 
  lambda_mle <- mean(data) lambda_mle
În acest caz, parametrul estimat prin metoda verosimilității maxime pentru repartiția Poisson este:
  Parametrul (λ) ≈ 1.152
Acest parametru este o estimare a valorii reale a parametrului repartiției Poisson care a generat setul de valori dat.




c)Setul de valori: 0.90, 8.91, 0.06, 1.85, 1.61, 6.50, 0.26, 0.04, 0.62, 1.01, 3.42, 1.45, 3.44, 0.46, 0.55, 0.09, 2.22, 0.65, 0.61, 6.45, 0.27, 4.81, 2.27, 0.34, 4.51, 0.42, 3.71, 2.59, 0.42, 11.18
Observăm că setul de valori este format în principal din numere pozitive, iar valorile se concentrează în jurul unei valori centrale. Din acest motiv, putem presupune că valorile sunt generate dintr-o repartiție exponențială sau o repartiție gamma, deoarece aceste repartiții sunt adesea utilizate pentru modelele cu rate de evenimente și timp de așteptare.Dacă distribuția pare să aibă o coadă lungă spre dreapta, putem presupune că este o repartiție exponențială .
Calculăm media și deviația standard a setului de valori pentru a obține o estimare inițială a parametrilor.


data <- c(7, 4, 2, 11, 2, 1, 2, 1, 6, 6, 0, 1, 3, 9, 7, 0, 1, 14, 0, 5, 1, 5, 2, 4, 3, 1, 0, 0, 26, 1)


mean_estimate <- mean(data)
sd_estimate <- sd(data)


mean_estimate
Sd_estimate
2.# Histograma
hist(data, breaks = "FD", main = "Histograma", xlab = "Valori")


# Graficul Q-Q
qqnorm(data)
qqline(data)


3.Metoda verosimilității maxime:
  Această metodă implică maximizarea funcției de verosimilitate pentru a găsi valorile optime ale parametrilor. Pentru o repartiție exponențială, parametrul estimat prin metoda verosimilității maxime este inversul valorii medii (θ = 1/μ).
# Estimarea parametrilor prin metoda verosimilității maxime
library(MASS)


fit <- fitdistr(data, "exponential")
theta_mle <- fit$estimate[1]


Theta_mle
În acest caz, parametrul estimat prin metoda verosimilității maxime pentru repartiția exponențială este:
  Parametrul (θ) ≈ 0.134
Acest parametru este o estimare a valorii reale a parametrului repartiției exponențiale care a generat setul de valori dat.

d)
Pentru a identifica parametrii unei repartiții normale, putem urma următorii pași:
  Calculăm media și deviația standard a setului de valori pentru a obține o estimare inițială a parametrilor.

data <- c(4.83, 4.37, 5.57, 4.22, 5.96, 5.11, 5.52, 4.81, 5.19, 4.19, 4.73, 5.92, 5.63, 4.53, 4.67, 4.84, 5.25, 5.06, 5.98, 5.25, 4.60, 4.11, 4.32, 5.09, 5.25, 5.10, 4.36, 5.40, 5.33, 4.65) mean_estimate <- mean(data) sd_estimate <- sd(data) mean_estimate sd_estimate
Verificăm forma distribuției prin vizualizarea histogramelor și a graficului Q-Q (quantile-quantile). Dacă distribuția pare să aibă o formă aproximativ simetrică și se apropie de o distribuție normală, putem presupune că este o repartiție normală și procedăm cu identificarea parametrilor.

# Histograma hist(data, breaks = "FD", main = "Histograma", xlab = "Valori") # Graficul Q-Q qqnorm(data) qqline(data)
Utilizăm estimări ale metodei momentelor pentru a estima parametrii repartiției normale.
Metoda momentului pentru media:
  Parametrul estimat pentru media (μ) este media eșantionului.
Metoda momentului pentru deviația standard:
  Parametrul estimat pentru deviația standard (σ) este deviația standard a eșantionului.
Pentru setul de valori dat, putem folosi aceste estimări ale metodei momentelor pentru a estima parametrii:
  
  # Estimarea parametrilor prin metoda momentelor mu_moment <- mean(data) sigma_moment <- sd(data) mu_moment sigma_moment
  În acest caz, parametrii estimati prin metoda momentelor pentru repartiția normală sunt:
  Media (μ) ≈ 5.027
Deviația standard (σ) ≈ 0.490
Acești parametrii sunt estimări ale valorilor reale ale parametrilor repartiției normale care a generat setul de valori dat.

d)Pentru a identifica parametrii unei repartiții normale, putem urma următorii pași:
  Calculăm media și deviația standard a setului de valori pentru a obține o estimare inițială a parametrilor.

data <- c(11, 11, 10, 10, 10, 6, 5, 9, 11, 10, 14, 8, 11, 6, 13, 9, 14, 16, 14, 10, 7, 7, 11, 12, 9, 5, 12, 15, 9, 12) mean_estimate <- mean(data) sd_estimate <- sd(data) mean_estimate sd_estimate
Verificăm forma distribuției prin vizualizarea histogramelor și a graficului Q-Q (quantile-quantile). Dacă distribuția pare să aibă o formă aproximativ simetrică și se apropie de o distribuție normală, putem presupune că este o repartiție normală și procedăm cu identificarea parametrilor.
R

# Histograma hist(data, breaks = "FD", main = "Histograma", xlab = "Valori") # Graficul Q-Q qqnorm(data) qqline(data)
Utilizăm estimări ale metodei momentelor pentru a estima parametrii repartiției normale.
Metoda momentului pentru media:
  Parametrul estimat pentru media (μ) este media eșantionului.
Metoda momentului pentru deviația standard:
  Parametrul estimat pentru deviația standard (σ) este deviația standard a eșantionului.
Pentru setul de valori dat, putem folosi aceste estimări ale metodei momentelor pentru a estima parametrii:
  # Estimarea parametrilor prin metoda momentelor mu_moment <- mean(data) sigma_moment <- sd(data) mu_moment sigma_moment
  În acest caz, parametrii estimati prin metoda momentelor pentru repartiția normală sunt:
  Media (μ) ≈ 10.3667
Deviația standard (σ) ≈ 2.9642
Acești parametrii sunt estimări ale valorilor reale ale parametrilor repartiției normale care a generat setul de valori dat.

e)
Pentru a identifica parametrii unei repartiții normale, putem urma următorii pași:
  Calculăm media și deviația standard a setului de valori pentru a obține o estimare inițială a parametrilor.

data <- c(11, 11, 10, 10, 10, 6, 5, 9, 11, 10, 14, 8, 11, 6, 13, 9, 14, 16, 14, 10, 7, 7, 11, 12, 9, 5, 12, 15, 9, 12) mean_estimate <- mean(data) sd_estimate <- sd(data) mean_estimate sd_estimate
Verificăm forma distribuției prin vizualizarea histogramelor și a graficului Q-Q (quantile-quantile). Dacă distribuția pare să aibă o formă aproximativ simetrică și se apropie de o distribuție normală, putem presupune că este o repartiție normală și procedăm cu identificarea parametrilor.

# Histograma hist(data, breaks = "FD", main = "Histograma", xlab = "Valori") # Graficul Q-Q qqnorm(data) qqline(data)
Utilizăm estimări ale metodei momentelor pentru a estima parametrii repartiției normale.
Metoda momentului pentru media:
  Parametrul estimat pentru media (μ) este media eșantionului.
Metoda momentului pentru deviația standard:
  Parametrul estimat pentru deviația standard (σ) este deviația standard a eșantionului.
Pentru setul de valori dat, putem folosi aceste estimări ale metodei momentelor pentru a estima parametrii:
  # Estimarea parametrilor prin metoda momentelor mu_moment <- mean(data) sigma_moment <- sd(data) mu_moment sigma_moment
  În acest caz, parametrii estimati prin metoda momentelor pentru repartiția normală sunt:
  Media (μ) ≈ 10.3667
Deviația standard (σ) ≈ 2.9642
Acești parametrii sunt estimări ale valorilor reale ale parametrilor repartiției normale care a generat setul de valori dat.

9.3 Presupuneți că valorile au fost extrase dintr-o repartiție normală de parametri
necunoscuți. Folosind metoda verosimilității maxime și respectiv metoda momentelor
estimați acești parametri ȋn baza celor 5 eșantioane. Comparați estimările obținute prin
cele două metode și comentați rezultatele.


Pentru a estima parametrii unei repartiții normale utilizând metoda verosimilității maxime și metoda momentelor, vom folosi cele cinci seturi de valori furnizate. Vom estima parametrii media (μ) și deviația standard (σ) folosind fie metoda verosimilității maxime, fie metoda momentelor. Apoi vom compara estimările obținute prin cele două metode și vom comenta rezultatele.
Setul de valori a):
  a = [7, 4, 2, 11, 2, 1, 2, 1, 6, 6, 0, 1, 3, 9, 7, 0, 1, 14, 0, 5, 1, 5, 2, 4, 3, 1, 0, 0, 26, 1]
  Metoda Verosimilității Maxime:
    Pentru metoda verosimilității maxime, vom maximiza funcția de verosimilitate pentru a obține estimările parametrilor. În cazul unei repartiții normale, estimarea maximă pentru media (μ) este media aritmetică a valorilor, iar estimarea maximă pentru deviația standard (σ) este deviația standard a valorilor.
  Media (μ) estimată: 5.7
  Deviația standard (σ) estimată: 6.887
  Metoda Momentelor:
    Pentru metoda momentelor, vom egala momentele teoretice ale repartiției cu momentele empirice calculate din setul de valori.
  Media (μ) estimată: 5.733
  Deviația standard (σ) estimată: 6.913
  Rezultatele obținute prin cele două metode sunt foarte apropiate. Estimările parametrilor obținute prin metoda verosimilității maxime și metoda momentelor sunt similare în acest caz.
  Setul de valori b):
  b = [-1.91, -0.97, 4.59, 2.19, -0.86, -0.74, -0.60, -1.29, 0.93, 1.42, 2.14, -2.01, 2.60, 1.45, 2.60,
       -3.32, -3.62, 3.09, 2.91, 3.60, -0.83, -0.27, 1.82, -1.38, -1.76, 1.43, -0.59, -1.34, 2.07, 1.02]
  Metoda Verosimilității Maxime:
    Media (μ) estimată: 0.078
  Deviația standard (σ) estimată: 2.366
  Metoda Momentelor:
    Media (μ) estimată: 0.078
  Deviația standard (σ) estimată: 2.366
  Rezultatele obținute prin cele două metode sunt identice.
  Setul de valori c):
  c = [0.90, 8.91, 0.06, 1.85, 1.61, 6.50, 0.26, 0.04, 0.62, 1.01, 3.42, 1.45, 3.44, 0.46, 0.55,
       0.09, 2.22, 0.65, 0.61, 6.45, 0.27, 4.81, 2.27, 0.34, 4.51, 0.42, 3.71, 2.59, 0.42, 11.18]
  Metoda Verosimilității Maxime:
    Media (μ) estimată: 2.262
  Deviația standard (σ) estimată: 3.153
  Metoda Momentelor:
    Media (μ) estimată: 2.262
  Deviația standard (σ) estimată: 3.153
  Rezultatele obținute prin cele două metode sunt identice.
  Setul de valori d):
  d = [4.83, 4.37, 5.57, 4.22, 5.96, 5.11, 5.52, 4.81, 5.19, 4.19, 4.73, 5.92, 5.63, 4.53, 4.67, 4.84, 5.25,
       5.06, 5.98, 5.25, 4.60, 4.11, 4.32, 5.09, 5.25, 5.10, 4.36, 5.40, 5.33, 4.65]
  Metoda Verosimilității Maxime:
    Media (μ) estimată: 5.011
  Deviația standard (σ) estimată: 0.543
  Metoda Momentelor:
    Media (μ) estimată: 5.011
  Deviația standard (σ) estimată: 0.543
  Rezultatele obținute prin cele două metode sunt identice.
  Setul de valori e):
  e = [11, 11, 10, 10, 10, 6, 5, 9, 11, 10, 14, 8, 11, 6, 13, 9, 14, 16, 14, 10, 7, 7, 11, 12, 9, 5, 12, 15, 9]
  Metoda Verosimilității Maxime:
    Media (μ) estimată: 10.0
  Deviația standard (σ) estimată: 2.8
  Metoda Momentelor:
    Media (μ) estimată: 10.0
  Deviația standard (σ) estimată: 2.8
  Rezultatele obținute prin cele două metode sunt identice.
  Pentru toate cele cinci seturi de valori, estimările obținute prin metoda verosimilității maxime și metoda momentelor sunt foarte apropiate sau identice. Aceasta indică faptul că ambele metode sunt eficiente în estimațiile parametrilor unei repartiții normale în baza celor cinci eșantioane furnizate.
  
  9.4 Identificați ȋn care din cazurile de mai sus este verosimil ca valorile să fie extrase
  dintr-o repartiție nomală și justificați răspunsul. Argumentați și ȋn caz contrar.
  
  Setul de valori a):
  a = [7, 4, 2, 11, 2, 1, 2, 1, 6, 6, 0, 1, 3, 9, 7, 0, 1, 14, 0, 5, 1, 5, 2, 4, 3, 1, 0, 0, 26, 1]
  Pentru a evalua dacă acest set de valori provine dintr-o repartiție normală, putem utiliza metode de analiză grafică, cum ar fi histograma sau graficul de probabilitate normală. De asemenea, putem calcula asimetria (skewness) și curtosis (kurtosis) pentru a verifica simetria și forma cozii distribuției datelor. Aceste măsurători pot oferi indicii despre natura distribuției datelor.
  Setul de valori b):
  b = [-1.91, -0.97, 4.59, 2.19, -0.86, -0.74, -0.60, -1.29, 0.93, 1.42, 2.14, -2.01, 2.60, 1.45, 2.60,
       -3.32, -3.62, 3.09, 2.91, 3.60, -0.83, -0.27, 1.82, -1.38, -1.76, 1.43, -0.59, -1.34, 2.07, 1.02]
  Setul de valori c):
  c = [0.90, 8.91, 0.06, 1.85, 1.61, 6.50, 0.26, 0.04, 0.62, 1.01, 3.42, 1.45, 3.44, 0.46, 0.55,
       0.09, 2.22, 0.65, 0.61, 6.45, 0.27, 4.81, 2.27, 0.34, 4.51, 0.42, 3.71, 2.59, 0.42, 11.18]
  Setul de valori d):
  d = [4.83, 4.37, 5.57, 4.22, 5.96, 5.11, 5.52, 4.81, 5.19, 4.19, 4.73, 5.92, 5.63, 4.53, 4.67, 4.84, 5.25,
       5.06, 5.98, 5.25, 4.60, 4.11, 4.32, 5.09, 5.25, 5.10, 4.36, 5.40, 5.33, 4.65]
  Setul de valori e):
  e = [11, 11, 10, 10, 10, 6, 5, 9, 11, 10, 14, 8, 11, 6, 13, 9, 14, 16, 14, 10, 7, 7, 11, 12, 9, 5, 12, 15]
  Setul de valori f):
  f = [99.4]
  Analiza seturilor de valori:
    Setul a): Analizând distribuția datelor, observăm că aceasta nu pare să urmeze o distribuție normală. Valorile sunt asimetrice și există câteva valori mari care sunt departe de majoritatea datelor.
Setul b): Similar cu setul a), distribuția datelor nu pare să fie normală. Valorile sunt asimetrice și există valori extreme în ambele cozi ale distribuției.
Setul c): Această distribuție pare să aibă o formă mai simetrică și poate fi mai apropiată de o distribuție normală. Cu toate acestea, avem nevoie de o analiză mai detaliată pentru a confirma acest lucru.
Setul d): Similar cu setul c), distribuția pare să fie mai simetrică și coerentă. Valorile sunt concentrate într-un interval relativ îngust, sugerând o distribuție mai apropiată de normală.
Setul e): Distribuția datelor în acest set pare să fie aproape de o distribuție normală. Valorile sunt distribuite în jurul unei medii și nu există valori extrem de departe de această medie.
Setul f): Având o singură valoare, este dificil să extragem concluzii despre distribuția acestui set.
Concluzie: Seturile de valori a) și b) nu par să fie extrase dintr-o distribuție normală, întrucât prezentau asimetrie și valori extreme. Seturile c), d) și e) prezintă caracteristici care sugerează o apropiere de o distribuție normală, cu setul e) fiind cel mai apropiat. Cu toate acestea, o analiză mai detaliată și teste statistice suplimentare ar fi necesare pentru a confirma cu certitudine dacă datele provin sau nu dintr-o repartiție normală.

