add2 <- function(x, y) {
  x + y
}

above10 <- funciton(x) {
  # find all numbers above 10
  use <- x > 10
  # subsetting...
  x[use]
}

# function with a default value.
above <- function(x, n = 10) {
  use <- x > n
  x[use]
}

# Data frame or matrix as input
column_mean <- function(y, removeNa = TRUE) {
  nCol <- ncol(y)
  means < numeric(nCol)
  for(i in 1:nCol) {
    mean[i] <- mean(y[,i], na.rm = removeNA)
  }
  means
}

# Lexical Scoping
make.power <- function(n) {
  pow <- function(x) {
    x^n
  }
}

y <- 10

f <- function(x) {
  y <- 2
  y^2 + g(x)
}

g <- function(x) {
  x*y
}

make.NegLogLik <- function(data, fixed=c(F, F)){
  params <- fixed
  # constructor
  function(p) {
    params[!fixed] <- p
    mu <- params[1]
    sigma <- params[2]
    a <- -0.5*length(data)*log(2*pi*sigma)
    b <- -0.5*sum((data-mu)^2)/(sigma^2)
    -(a+b)
  }
}













