source("C:/Users/Johanson Onyegbula/Documents/Masters in NRES/Fall 2021/CS 790K/Assignments/Options.R")

chromosome <- integer(length = chromosomeLength)

flip <- function(prob){
  flipBit <- runif(1) < prob
  return(flipBit)
}


randInt <- function(low, high){
  generatedInt <- floor(runif(1, low, high))
  return(generatedInt)
}


mutate <- function(chromosome, pMut = probMutation){
  for (i in 1:length(chromosome)){
    if (flip(pMut) == TRUE){
      chromosome[i] <- 1 - chromosome[i]
    }
  }
  return(chromosome)
}


initializeIndividual <- function(chromLen = chromosomeLength){
  chromosome <<- sample(c(0,1), chromLen, replace = T)
  return(chromosome)
}