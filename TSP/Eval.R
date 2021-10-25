source("C:/Users/Johanson Onyegbula/Documents/Masters in NRES/Fall 2021/CS 790K/Assignments/TSP/Options.R")


evaluateTSP <- function(chromosome, chromLen = chromosomeLength){
  sumDist <- 0
  for (i in 1:chromLen){
    if (i < chromLen){
      pairBits <- c(chromosome[i], chromosome[i+1])
    } else{
      pairBits <- c(chromosome[i], chromosome[1])
    }
    sumDist <- sumDist + dist.mat[pairBits[1], pairBits[2]]
  }
  #fitness <- 1/sumDist
  fitness <- maxImposFitness - sumDist
  return(fitness)
}
