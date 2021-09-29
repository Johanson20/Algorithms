source("C:/Users/Johanson Onyegbula/Documents/Masters in NRES/Fall 2021/CS 790K/Assignments/Individual.R")
source("C:/Users/Johanson Onyegbula/Documents/Masters in NRES/Fall 2021/CS 790K/Assignments/Eval.R")


population <- matrix(0, ncol = chromosomeLength, nrow = populationSize)


initializePopulation <- function(popSize = populationSize, chromLen = chromosomeLength){
  population <- matrix(0, ncol = chromLen, nrow = popSize)
  for (i in 1:populationSize){
    population[i,] <- initializeIndividual(chromLen)
  }
  startGA <- sprintf("%s %11s %11s %11s %11s", "Generation", "Minimum", "Mean", "Maximum", "Sum")
  #print(startGA)
  return(population)
}


evaluatePop <- function(population, funct){
  popSize <- dim(population)[1]
  popFitness <- matrix(0, nrow = popSize)
  for (i in 1:popSize){
    #popFitness[i,] <- evaluateX2(population[i,])
    popFitness[i,] <- evaluateDeJong1(population[i,])
  }
  return(popFitness)
}


xover1Pt <- function(population, pXover = probCrossover, popFitness, popStats){
  popSize <- dim(population)
  newParents <- matrix(ncol = popSize[2], nrow = popSize[1])
  
  for (i in seq(1, popSize[1], 2)){
    parents <- nextParents(population, popFitness, popStats)
    parent1 <- parents[1,]
    parent2 <- parents[2,]
    child1 <- parent1
    child2 <- parent2
    parentSize <- length(parent1)
    if (flip(pXover)){
      xp <- randInt(1, parentSize)
      subBits <- seq(xp, parentSize)
      child1[subBits] <- parent2[subBits]
      child2[subBits] <- parent1[subBits]
    }
    newParents[i, ] <- child1
    newParents[i+1, ] <- child2
  }
  return(newParents)
}


xover2Pt <- function(population, pXover, popFitness, popStats){
  popSize <- dim(population)[1]
  newParents <- matrix(0, nrow = popSize)
  
  for (i in seq(1, popSize, 2)){
    parents <- nextParents(population, popFitness, popStats)
    parent1 <- parents[1,]
    parent2 <- parents[2,]
    child1 <- parent1
    child2 <- parent2
    parentSize <- length(parent1)
    if (flip(pXover)){
      xp1 <- randInt(1, parentSize)
      xp2 <- randInt(xp1, parentSize)
      subBits <- seq(xp1, xp2)
      child1[subBits] <- parent2[subBits]
      child2[subBits] <- parent1[subBits]
    }
    newParents[i, ] <- child1
    newParents[i+1, ] <- child2
  }
  return(newParents)
}


populationStats <- function(population, gen){
  PopFitness <<- evaluatePop(population)
  PopMean <- mean(PopFitness)
  PopMin <- min(PopFitness)
  PopMax <- max(PopFitness)
  PopSum <- sum(PopFitness)
  PopStats <<- data.frame(Gen = gen, Min = PopMin, Mean = PopMean, Max = PopMax, Sum = PopSum,
                          Population = populationSize, Crossover = probCrossover, Mutation = probMutation)
  return(PopStats)
}


populationReport <- function(popStats){
  report[gen+1, ] <<- popStats[1,]
  output <- sprintf("%6d %15.4f %11.4f %11.4f %11.4f", popStats$Gen, popStats$Min, popStats$Mean, popStats$Max, popStats$Sum)
  #print(output)
}


writeReport <- function(report = report, file = outfile){
  write.csv(report, file = outfile, row.names = FALSE)
}


proportionalSelector <- function(popFitness, popStats){
  parentSize <- length(popFitness)
  selectionProb <- popFitness/popStats$Sum
  tempFitness <- popFitness
  if (max(tempFitness) >= 0){
    tempFitness[tempFitness < 0] <- 0
    selectionProb <- tempFitness/sum(tempFitness)
  }
  tempFitness[tempFitness < 0] <- 0
  chosenIndex <- sample(c(1:parentSize), 1, prob = selectionProb)
  return(chosenIndex)
}


nextParents <- function(population, popFitness, popStats){
  index1 <- proportionalSelector(popFitness, popStats)
  index2 <- proportionalSelector(popFitness, popStats)
  return( matrix(c(population[index1, ], population[index2, ]), nrow = 2) )
}