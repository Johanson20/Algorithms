minimum <- -5.12
precision <- 1/1023


evaluateOneMax <- function(chromosome){
  return(sum(chromosome))
}


evaluateX2 <- function(chromosome, mini = minimum, prec = precision){
  chromLen <- length(chromosome)/3
  x <- chromosome * 2^(c(chromLen:1) - 1)
  value <- mini + prec*sum(x, na.rm = T)
  return(value^2)
}


evaluateDeJong1 <- function(chromosome, mini = -5.12, prec = 10.24/2047){
  score <- 0
  param <- length(chromosome)/3
  for (i in 1:3){
    startat <- 1 + param*(i-1)
    endat <- i*param
    x <- chromosome[startat:endat] * 2^(c(param:1) - 1)
    value <- mini + prec*sum(x, na.rm = T)
    score <- score + value^2
  }
  return(score)
}


evaluateDeJong2 <- function(chromosome, mini = -2.048, prec = 4.096/8191){
  chromLen <- length(chromosome)
  param <- chromLen/2
  x1 <- chromosome[1:param] * 2^(c(param:1) - 1)
  x2 <- chromosome[(param+1):chromLen] * 2^(c(param:1) - 1)
  value1 <- mini + prec*sum(x1, na.rm = T)
  value2 <- mini + prec*sum(x2, na.rm = T)
  score <- 100*(value1^2 - value2)^2 + (1 - value1)^2
  return(score)
}


evaluateDeJong3 <- function(chromosome, mini = -5.12, prec = 10.24/2047){
  score <- 0
  param <- length(chromosome)/5
  for (i in 1:5){
    startat <- 1 + param*(i-1)
    endat <- i*param
    x <- chromosome[startat:endat] * 2^(c(param:1) - 1)
    value <- mini + prec*sum(x, na.rm = T)
    score <- score + value
  }
  return(score)
}


evaluateDeJong4 <- function(chromosome, mini = -1.28, prec = 2.56/511){
  score <- 0
  param <- length(chromosome)/30
  for (i in 1:30){
    startat <- 1 + param*(i-1)
    endat <- i*param
    x <- chromosome[startat:endat] * 2^(c(param:1) - 1)
    value <- mini + prec*sum(x, na.rm = T)
    score <- score + i*value^4
  }
  return(score + rnorm(1))
}


evaluateDeJong5 <- function(chromosome, mini = -65.536, prec = 131.072/262143){
  score <- 0
  chromLen <- length(chromosome)
  param <- chromLen/2
  a <- matrix(ncol = 25, nrow = 2)
  a[1,] <- rep(c(-32,-16,0,16,32), 5)
  a[2,] <- rep(c(-32,-16,0,16,32), each = 5)
  x1 <- chromosome[1:param] * 2^(c(param:1) - 1)
  x2 <- chromosome[(param+1):chromLen] * 2^(c(param:1) - 1)
  value1 <- mini + prec*sum(x1, na.rm = T)
  value2 <- mini + prec*sum(x2, na.rm = T)
  for (j in 1:25){
    score <- score + 1/(j + (value1 - a[1,j])^6 + (value2 - a[2,j])^6)
  }
  return(score + 0.002)
}