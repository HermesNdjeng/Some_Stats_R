##Un peu d'approximation polynomiale

# INTERPOLATION DE LAGRANGE
polynome_lagrange <- function(X, Y, x){
  if(length(X) != length(Y)) return("Valeurs  invalides")
  
  P <- 0
  n <-length(X)
  
  for(i in 1:n){
    L <- 1
    for(j in 1:n){
      if(i == j) next
      L <- L * ((x-X[j]) / (X[i]-X[j]))
    }
    P <- P + Y[i] * L
  }
  
  return(list(x = x, P = P))
}


## Exemple
X = 0:6
X <- X[-6]
Y <- c(12.19, 14.07, 16.21, 18.28, 20, 23.89)
polynome_lagrange(X, Y, 5)



# METHODE DE NEWTON
D <- function(n, X, Y){
  if(length(X) != length(Y) | length(X) != (n+1))
    return("Entrees invaldes")
  
  if(n == 0)
    return(Y[1])
  
  else
    return((D(n-1, X[-1], Y[-1]) - D(n-1, X[-length(X)], Y[-length(Y)])) / (X[length(X)] - X[1]))
}

polynome_newton <- function(X, Y, x){
  if(length(X) != length(Y)) return("Valeurs invalides")
  
  P <- 0
  n <- length(X)
  
  for(i in 0:n){
    PI <- D(i, X[1:(i+1)], Y[1:(i+1)])
    for(k in 0:i)
      PI <- PI * (x - X[k+1])
    P <- P + PI
  }
  
  return(list(x = x, P = P))
}

polynome_newton(c(-1, 1, 2, 3), c(4, -1, 4, 6), -1)
