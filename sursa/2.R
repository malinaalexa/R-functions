verif2<-function(tipulfunctiei,f){
  #tipulfunctiei = 0 pt functie discreta, = 1 pentru functie continua
  if(tipulfunctiei == 0){
    if(any(f < 0))
      return(0) #returnam 0 daca nu indeplineste conditia, deci daca nu are constanta de normalizare
    else 
      {k <- 1/sum(f)
      if(k == 1) return(1) #returnam 1 fiindca functia nu are nev de constanta de normalizare
      else return(k)
      }
  }
  else if(tipulfunctiei == 1){
    xs <- seq(-10000, 10000, length.out = 1000000)
    if (any(f(xs) < 0))
      return(0)
     else {
      tryCatch(
        {
          integrala <-integrate(f, -Inf, Inf)$value
            k <- 1/integrala
            if ( k - 1 < 1e-4 ) return(1)
            else return(k)
        },
        error = function(e){
          return(0)
        } 
      )
    }
   
  }
  else return (-1) #returnăm -1 pentru parametru nevalid.
}

#sa verificam pentru functii discrete

f1 <- c(0.2, 0.3, 0.5) #este functie de masa

verif2(0, f1)

f2 <- c(0.7, 0.4, 0.2) #nu este functie de masa pt ca nu are suma valorilor 1

verif2(0, f2)

f3 <- c(-0.2, 0.7, 0.5) #nu este functie de masa pt ca are si valori negative

verif2(0, f3)

#verificam pentru functii continue

f4 <- function(x) #nu este densitate de probabilitate pt ca are valori negative
{return (x + 1)}

verif2(1, f4)

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
