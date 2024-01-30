# RECHERCHE DE ZEROS PAR LA METHODE DE DICHOTOMIE

zero_dichotomie <- function(f, a, b, e){
  if(a > b) return(cat("Les bornes entrees ne definissent pas un intervalle"))
  
  if((f(a) < 0 & f(b) < 0) | ((f(a) > 0 & f(b) > 0)))
    return(paste("Les parametres ne verifient pas certaines conditions (monotonie de la",
                  "fonction et/ou changement de signe sur l'intervalle"))
  
  if(f(a) == 0) return(list(it = 0, c = a, `f(c)` = 0))
  if(f(b) == 0) return(list(it = 0, c = b, `f(c)` = 0))
  
  stop <- log(((b-a)/e)) / log(2)
  m <- 0;

  repeat{
    m <- m + 1
    
    c <- (a + b) / 2
    
    if(f(c) > 0){
      if(f(a) > 0) a <- c 
      else b <- c
    }else if(f(c) < 0){
      if(f(a) > 0) b <- c
      else a <- c
    } else return(list(it = m, c = c, `f(c)` = 0))
    
    if(m > stop + 1) return(list(it = (m-1), c = c, `f(c)` = f(c)))
  }
}



# RECHERCHE DE ZEROS PAR LA METHODE DE LAGRANGE

zero_lagrange <- function(f, a, b, e, control = "residus"){
  if(a > b) return(cat("Les bornes entrees ne definissent pas un intervalle"))
  
  if((f(a) < 0 & f(b) < 0) | ((f(a) > 0 & f(b) > 0)))
    return(paste("Les parametres ne verifient pas certaines conditions (monotonie de la",
                 "fonction et/ou changement de signe sur l'intervalle"))
  
  if(f(a) == 0) return(list(it = 0, c = a, `f(c)` = 0))
  if(f(b) == 0) return(list(it = 0, c = b, `f(c)` = 0))
  
  m <- c_0 <- c <- 0
  
  repeat{
    m <- m + 1
    
    c_0 <- c
    c <- (a * f(b) - b * f(a)) / (f(b) - f(a))
    
    if(f(c) > 0){
      if(f(a) > 0) a <- c 
      else b <- c
    }else if(f(c) < 0){
      if(f(a) > 0) b <- c
      else a <- c
    } else return(list(it = m, c = c, `f(c)` = 0))
    
    if(control == "residus")
      if(abs(f(c)) < e) return(list(it = (m-1), c = c, `f(c)` = f(c)))
    
    if(control == "increments")
      if(abs(c-c_0) < e) return(list(it = (m-1), c = c, `f(c)` = f(c)))
  }
}



# TESTS
f <- function(x) return(2*x^3-x-5)

dichotomie <- unlist(zero_dichotomie(f, 1, 2, 0.01))
lagrange_residus <- unlist(zero_lagrange(f, 1, 2, 0.01, control = "residus"))
lagrange_increments <- unlist(zero_lagrange(f, 1, 2, 0.01, control = "increments"))

data.frame(dichotomie, lagrange_residus, lagrange_increments)


g <- function(x) return(x^3-13*x^2+40*x-25)

dichotomie <- unlist(zero_dichotomie(g, 0, 1, 0.001))
lagrange_residus <- unlist(zero_lagrange(g, 0, 1, 0.001, control = "residus"))
lagrange_increments <- unlist(zero_lagrange(g, 0, 1, 0.001, control = "increments"))

data.frame(dichotomie, lagrange_residus, lagrange_increments)


h <- function(x) return(x^3-x-3)

dichotomie <- unlist(zero_dichotomie(h, 1, 2, 0.01))
lagrange_residus <- unlist(zero_lagrange(h, 1, 2, 0.01, control = "residus"))
lagrange_increments <- unlist(zero_lagrange(h, 1, 2, 0.01, control = "increments"))

data.frame(dichotomie, lagrange_residus, lagrange_increments)
