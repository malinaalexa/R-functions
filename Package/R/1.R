verif1 <- function(tipulfunctiei, f){
  #tipulfunctiei = 0 pt functie discreta, = 1 pentru functie continua
  if (tipulfunctiei == 0){
    if (any(f < 0)) { #cerem ca f sa ia doar valori pozitive sau zero
      return(0) #returnam 0 daca nu este functie de masa
      
    }
    
    
    else if (abs(sum(f) - 1) > 1e-10) {# Verificăm dacă suma valorilor funcției este egală cu 1
      
      return(0)
    }
    
    # se indeplinesc conditiile ca f sa fie functie de masa
    else
    { return(1) #returnam 1 daca este functie de masa
    }
    
  }
  else if (tipulfunctiei == 1){
    xs <- seq(-10000, 10000, length.out = 1000000)
    
    if (any(f(xs) < 0))
      return(0)
    else{    
      # Verificăm dacă integrala funcției este egală cu 1
      tryCatch(
        {
          integrala <-integrate(f, -Inf, Inf)
          if (abs(integrate(f, -Inf, Inf)$value - 1) > 1e-4) { 
            return(0)
          }
          else 
            return(2) #returnam 2 daca este densitate de probabilitate
        },
        error = function(e){
          return(0)
        } 
      )
    }
    
    # se indeplinesc conditiile ca f sa fie densitate de probabilitate
  }
  else return (-1) #returnam -1 pentru parametru nevalid
  
}

#sa verificam pentru functii discrete
f1 <- c(0.2, 0.3, 0.5) #este functie de masa

verif1(0, f1)

f2 <- c(0.7, 0.4, 0.2) #nu este functie de masa pt ca nu are suma valorilor 1

verif1(0, f2)

f3 <- c(-0.2, 0.7, 0.5) #nu este functie de masa pt ca are si valori negative

verif1(0, f3)

#verificam pentru functii continue

f4 <- function(x) #nu este densitate de probabilitate pt ca are valori negative
{return (x + 1)}

verif1(1, f4)

f5 <- function(x) #este densitate de probabilitate
{
  dnorm(x, mean = 0, sd = 1)
}

verif1(1,f5)

f6 <- function(x) #nu este densitate de probabilitate pt ca nu are integrala egala cu 1
{
  return(x^2)
}

verif1(1,f6)
