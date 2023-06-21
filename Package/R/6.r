P <- function(va, v, situatie, f = NULL) {
  if (situatie == 1) {
    # Calculam probabilitatea aparitiei valorii date
    p <- sum(va == v) / length(va)
    return(p)
  }
  else if (situatie == 2){
    #calculam probabilitatea frecventista
    s <- sample(va,f,replace=TRUE) #simulam f extrageri din variabila aleatoare
    p <- sum(s == v) / f
    return(p)
  }
  else if (situatie == 3){
    #calculam probabilitatea aparitiei unei valori mai mari sau egale v
    p <- sum(va >= v) / length(va)
    return(p)
  }
  else if (situatie == 4){
    #calculam probabilitatea frecventista a aparitiei unei valori mai mari sau egale v
    s <- sample(va,f,replace=TRUE) #simulam 500 de extrageri din variabila aleatoare
    p <- sum(s >= v) / f
    return(p)
  }
  else if (situatie == 5){
    #calculam probabilitatea conditionata de aparitia evenimentelor redate in f
    date <- table(va) #transformam setul de date intr-un tabel pentru a manipula datele mai usor
    p <- date[v,f]/sum(date[, f])
    return(p)
  }
  else if (situatie == 6){
    #calculam probabilitatea conditionata de aparitia evenimentelor redate in v
    date <- table(va) #transformam setul de date intr-un tabel pentru a manipula datele mai usor
    p <- date[v,f]/sum(date[v, ])
    return(p)
  }
  
}

va1 <- c(1, 2, 3, 4, 5, 6)
P(va1, 2, 1)

P(va1, 2, 2, 1000)

P(va1, 4, 3)
P(va1, 4, 4, 1000)

va2<- data.frame(clasa=rep(c('12A', '12B', '12C'), each=30),
                 optional=rep(c('Engleza', 'Informatica', 'Matematica', 'Romana',
                                'Engleza', 'Informatica', 'Matematica', 'Romana',
                                'Engleza', 'Informatica', 'Matematica', 'Romana'),
                              times=c(12, 7, 3, 8, 10, 10, 4, 6, 14, 5, 3, 8)))

P(va2, 1, 5, 1) #probabilitatea ca un elev sa fie din clasa 12A, stiind ca a ales ca optional engleza
P(va2, 1, 6, 1) #probabilitatea ca un elev sa aleaga engleza, stiind ca e din 12A