source("C:/Users/Johanson Onyegbula/Documents/Masters in NRES/Fall 2021/CS 790K/Assignments/TSP/Options.R")

chromosome <- integer(length = chromosomeLength)

flip <- function(prob){
  flipBit <- runif(1) < prob
  return(flipBit)
}


swapMutate <- function(chromosome, pMut = probMutation){
  if (flip(pMut) == TRUE){
    pts <- sample(1:chromosomeLength, 2, replace = F)
    tempBit <- chromosome[pts[1]]
    chromosome[pts[1]] <- chromosome[pts[2]]
    chromosome[pts[2]] <- tempBit
  }
  return(chromosome)
}


invertMutate <- function(chromosome, pMut = probMutation){
  if (flip(pMut) == TRUE){
    pts <- sample(1:chromosomeLength, 2, replace = F)
    tempBits <- rev(chromosome[pts[1]:pts[2]])
    chromosome[pts[1]:pts[2]] <- tempBits
  }
  return(chromosome)
}


initializeIndividual <- function(chromLen = chromosomeLength){
  chromosome <<- sample(1:chromLen, chromLen, replace = F)
  return(chromosome)
}
