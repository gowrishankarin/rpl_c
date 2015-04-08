getwd()
myfunction <- function(x) {
  y <- rnorm(100)
  mean(y)
}

second <- function(x) {
  x + rnorm(length(x))
}

naCount <- function(x, y) {
  count <- 0
  sum <- 0
  index <- 1
  for(k in x) {
    if(k == TRUE)
      count <- count + 1
    else 
      sum = sum + y[index]
    index <- index + 1
  }
  print(count)
  print(sum/leng)
}

subset<- function(x) {
  oz = x["Ozone"]
  tm = x["Temp"]
  sr = x["Solar.R"]
  
  index <- 1
  count <- 0
  sum <- 0
  
  for(k in oz) {
    print(k)
    
    if(k > 31 && tm[index] > 90) {
      
      a <- sr[index]
      if(is.na(a) == FALSE) {
        print(a)
        count <- count + 1
        sum <- sum + a
      }
    }
    
    index <- index +1
  }
  print(sum/count)
}

rm_bad <- function(x) {
  count <- 0
  sum <- 0
  for(i in 1:dim(x)[[1]]) {
    y <- x[i,]
    print(y)
    if(x[i,1] > 31 && x[i,4] > 90) {
      sum <- sum + x[i,2]
      count <- count + 1
    }
  }
  
  print(sum/count)

}

month_6 <- function(y) {
  count <- 0
  sum <- 0
  for(i in 1:dim(x)[[1]]) {
    if(x[i,5] == 6) {
      sum <- sum + x[i,4]
      count <- count + 1
    }
  }
  
  print(sum/count)
}

